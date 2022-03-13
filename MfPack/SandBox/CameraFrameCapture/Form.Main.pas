unit Form.Main;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  System.TimeSpan,
  System.IOUtils,
  System.Services.Dbt,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfIdl,
  {Application}
  MfDeviceCaptureClass,
  CameraCapture.Asynchronous,
  Support;

type
  TDeviceDetails = record
    oExtendedDetails : TDeviceProperties;
    sOriginalName : string;
    sUniqueName : string;
    iCount : Integer;
  end;

  TFrmMain = class(TForm)
    pnlTop: TPanel;
    cbxCaptureDevices: TComboBox;
    lblSelectDevice: TLabel;
    lblResoltution: TLabel;
    cbxResolution: TComboBox;
    pnlVideo: TPanel;
    btnRefreshDevices: TButton;
    memLog: TMemo;
    lblLog: TLabel;
    btnClearLog: TButton;
    btnCaptureFrame: TButton;
    pnlFrameCapture: TPanel;
    picFrame: TImage;
    pnlBottom: TPanel;
    grpVideoPreview: TGroupBox;
    grpFrameCapture: TGroupBox;
    btnSaveImage: TButton;
    sdSaveFrame: TSaveDialog;
    btnStartBurst: TButton;
    btnStopBurst: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDevicesClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure HandleSelectedDeviceChange(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnCaptureFrameClick(Sender: TObject);
    procedure HandleSaveImageClick(Sender: TObject);
    procedure cbxResolutionChange(Sender: TObject);
    procedure HandlFormShow(Sender: TObject);
    procedure HandleStartBurstCapture(Sender: TObject);
    procedure HandleStopBurstCapture(Sender: TObject);
  private
    FLogLevel: TLogType;
    FFormatSettings: TFormatSettings;
    FCapture: TCameraCaptureAsync;
    FFrequency: int64;
    FCaptureStart: int64;
    FDefaultDeviceName : string;
    FLastFrameTime : TTimeSpan;
    FDevices : TArray<TDeviceDetails>;
    FBurstCaptureEnabled : Boolean;
    FBurstStartTime : TDateTime;

    function StartCapturePreview : Boolean;
    function GetDefaultSaveName: string;
    function DeviceExists(ADevices : TArray<TDeviceDetails>; const AName: string; out AIndex : Integer): Boolean;

    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;

    procedure PopulateDeviceList;
    procedure StopCapturePreview;
    procedure Log(const AText: string; ALogType: TLogType);
    procedure SetDefaults;
    procedure BeginBusy;
    procedure EndBusy;
    procedure SetDevice(const ADevice : TDeviceDetails);
    procedure HandleFrameFound(ABitmap: TBitmap; ATimeStamp: TTimeSpan);
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure PopulateResolutions;
    procedure HandleResolutionChanged;
    procedure RestoreDefaults;
    procedure UpdateSelectedDevice;
    procedure ClearValues;
    procedure RequestFrame;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  System.DateUtils,
  VCL.Imaging.pngimage,
  VCL.Imaging.jpeg;

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  CoInitializeEx(Nil,
                 COINIT_APARTMENTTHREADED);

  // Startup Media Foundation
  if FAILED(MFStartup(MF_VERSION, 0)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                   MB_ICONSTOP);
        Application.Terminate;
   end;

   ClearValues;

   // Create capture class
   FCapture := TCameraCaptureAsync.Create;
   FCapture.OnFrameFound := HandleFrameFound;
   FCapture.OnLog := Log;

   FFormatSettings := TFormatSettings.Create;

   SetDefaults;
   PopulateDeviceList;
   UpdateEnabledStates;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  StopCapturePreview;
  FCapture.Free;
  MFShutdown;
  CoUnInitialize;
end;

procedure TFrmMain.ClearValues;
begin
  FFrequency := 0;
  FCaptureStart := 0;
  FLastFrameTime := TTimeSpan.Zero;
  SetLength(FDevices, 0);
  FBurstCaptureEnabled := False;
end;

procedure TFrmMain.SetDefaults;
begin
  FLogLevel := ltInfo;

  // Set a default video device - for debugging
  FDefaultDeviceName := '';
end;

procedure TFrmMain.HandlFormShow(Sender: TObject);
begin
  RestoreDefaults;
end;

procedure TFrmMain.RestoreDefaults;
var
  iDefaultDeviceIndex : Integer;
begin
  // Set the default startup device if it exists
  iDefaultDeviceIndex := cbxCaptureDevices.Items.IndexOf(FDefaultDeviceName);

  if iDefaultDeviceIndex > -1 then
  begin
   cbxCaptureDevices.ItemIndex := iDefaultDeviceIndex;
   UpdateSelectedDevice;
  end;
end;

procedure TFrmMain.btnCaptureFrameClick(Sender: TObject);
begin
  RequestFrame;
end;

procedure TFrmMain.RequestFrame;
begin
  // Only clear the image on request if we are not in burst capture mode
  if not FBurstCaptureEnabled then
    ClearImage;

  QueryPerformanceFrequency(FFrequency);
  QueryPerformanceCounter(FCaptureStart);
  Log('Requesting frame.', ltInfo);
  FCapture.RequestFrame;
end;

procedure TFrmMain.btnClearLogClick(Sender: TObject);
begin
  memLog.Lines.Clear;
end;

procedure TFrmMain.btnRefreshDevicesClick(Sender: TObject);
begin
  PopulateDeviceList;
end;

procedure TFrmMain.cbxResolutionChange(Sender: TObject);
begin
  HandleResolutionChanged;
end;

function TFrmMain.StartCapturePreview: Boolean;
begin
  Result:= Assigned(MfDeviceCapture);
  if not Result then
  begin
     MfDeviceCapture:= Nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      Result := SUCCEEDED(TMfCaptureEngine.CreateInstance(pnlVideo.Handle,       // The clipping window / control
                                      Handle,
                                      MfDeviceCapture));  // Must be main form or parent window !!!
  end;
end;

procedure TFrmMain.HandleResolutionChanged;
var
  oFormat : TVideoFormat;
begin
  if FCapture.SourceOpen then
  begin
    Log('Updating capture format', ltInfo);
    if FCapture.SetVideoFormat(cbxResolution.ItemIndex) and FCapture.GetCurrentFormat(oFormat) then
      Log(Format('Capture format change to %d x %d', [oFormat.iFrameWidth, oFormat.iFrameHeigth]), ltInfo)
    else
      Log('Failed to set capture format', ltError);

    UpdateEnabledStates;
  end;
end;

procedure TFrmMain.HandleFrameFound(ABitmap: TBitmap;
                                    ATimeStamp: TTimeSpan);
var
  iCaptureEnd: int64;
begin
  QueryPerformanceCounter(iCaptureEnd);
  Log(Format('Image found in %f milliseconds. Frames Skipped: %d',
             [(iCaptureEnd - FCaptureStart) / FFrequency * 1000,
                               FCapture.FramesSkipped]),
                               ltInfo);

  try
    pnlFrameCapture.Caption := '';
    picFrame.Picture.Bitmap.Assign(ABitmap);
    FLastFrameTime := ATimeStamp;
  finally
    FreeAndNil(ABitmap);
  end;

  if FBurstCaptureEnabled then
  begin
    FBurstCaptureEnabled := (SecondsBetween(Now, FBurstStartTime) <= 10);
    if FBurstCaptureEnabled then
      RequestFrame;
  end;

  UpdateEnabledStates;
end;


function TFrmMain.GetDefaultSaveName : string;
begin
  Result := 'Capture_' + TimeSpanToDisplay(FLastFrameTime, True).Replace(':', '.');
end;

procedure TFrmMain.ClearImage;
begin
  pnlFrameCapture.Caption := 'No Image. Waiting frame capture...';
  picFrame.Picture := Nil;
end;

procedure TFrmMain.WMDeviceChange(var Msg: TMessage);
var
  oResult : HResult;
  bDeviceLost : Boolean;
begin
  oResult := S_OK;

  if Assigned(MfDeviceCapture) then
  begin
    if (Msg.lParam <> 0) then
      oResult := MfDeviceCapture.CheckCaptureDeviceLost(PDEV_BROADCAST_HDR(Msg.LParam), bDeviceLost);

    if (Failed(oResult) or bDeviceLost) then
      begin
        MfDeviceCapture.ShutDownEngine;
        Log('Lost the capture device.', ltError);
      end;
  end;
end;

procedure TFrmMain.StopCapturePreview;
begin
   if Assigned(MfDeviceCapture) then
   begin
      BeginBusy;
      try
        if SUCCEEDED(MfDeviceCapture.ShutDownEngine) then
        begin
          MfDeviceCapture.Free;
          MfDeviceCapture := Nil;
        end;
      finally
       EndBusy;
      end;
   end;
end;

procedure TFrmMain.FormPaint(Sender: TObject);
var
  ps: PAINTSTRUCT;
  vHdc: HDC;
begin
  vHdc := 0;
  BeginPaint(Self.Handle, ps);

  try
    if Assigned(MfDeviceCapture) and (MfDeviceCapture.VideoDetected) then
      MfDeviceCapture.UpdateVideo
    else
      FillRect(vHdc, ps.rcPaint, HBRUSH(COLOR_APPWORKSPACE + 1));
  finally
    EndPaint(Self.Handle, ps);
  end;
end;


procedure TFrmMain.HandleSaveImageClick(Sender: TObject);
var
  pPng: TPngImage;
  pJpg: TJPEGImage;
  iStart: int64;
  iEnd : int64;
  iFrequency: int64;
begin
  if not picFrame.Picture.Bitmap.Empty then
  begin
    sdSaveFrame.FileName := GetDefaultSaveName;

    if sdSaveFrame.Execute then
      begin
        QueryPerformanceFrequency(iFrequency);
        QueryPerformanceCounter(iStart);

        case sdSaveFrame.FilterIndex of
          {BMP}
          1: begin
               picFrame.Picture.SaveToFile(sdSaveFrame.FileName);
             end;
          {PNG}
          2: begin
               pPng := TPngImage.Create;
               pPng.Assign(picFrame.Picture.Bitmap);
               pPng.SaveToFile(sdSaveFrame.FileName);

               if Assigned(pPng) then
                 pPng.Free;
             end;
          {JPG}
          3: begin
               pJpg := TJPEGImage.Create;
               // Adjust performance, compression etc.
               pJpg.Performance := jpBestQuality;
               pJpg.ProgressiveEncoding := True;
               pJpg.ProgressiveDisplay := True;
               //pJpg.CompressionQuality := 30;
               pJpg.Compress;
               pJpg.Assign(picFrame.Picture.Bitmap);
               pjpg.SaveToFile(sdSaveFrame.FileName);

               if Assigned(pJpg) then
                 pJpg.Free;
             end;
        end;

      QueryPerformanceCounter(iEnd);

      Log(Format('Image saved to disk in %f milliseconds. Location: %s',
             [(iEnd - iStart) / iFrequency * 1000, sdSaveFrame.FileName]),
                               ltInfo);
      end;
  end;
end;

procedure TFrmMain.HandleSelectedDeviceChange(Sender: TObject);
begin
  UpdateSelectedDevice;
end;

procedure TFrmMain.HandleStartBurstCapture(Sender: TObject);
begin
  UpdateEnabledStates;
  FBurstStartTime := Now;
  FBurstCaptureEnabled := True;
  RequestFrame;
end;

procedure TFrmMain.HandleStopBurstCapture(Sender: TObject);
begin
  FBurstCaptureEnabled := False;
  UpdateEnabledStates;
end;

procedure TFrmMain.UpdateSelectedDevice;
begin
  FCapture.CloseSource;

  if cbxCaptureDevices.ItemIndex = 0 then
    StopCapturePreview
  else if cbxCaptureDevices.ItemIndex > 0 then
  begin
    StartCapturePreview;
    SetDevice(FDevices[cbxCaptureDevices.ItemIndex - 1]);
  end;

  PopulateResolutions;
  UpdateEnabledStates;
end;

procedure TFrmMain.SetDevice(const ADevice : TDeviceDetails);
begin
  BeginBusy;
  try
    Log('Setting selected device to: ' + ADevice.sUniqueName, ltInfo);

    if SUCCEEDED(MfDeviceCapture.SetDevice(ADevice.oExtendedDetails)) then
    begin
      Log('Device selected', ltInfo);

      // Prepare the frame capture for the device
      FCapture.OpenDeviceSource(ADevice.oExtendedDetails.lpSymbolicLink);
    end
    else
      Log('Failed to set video device', ltError);

    UpdateEnabledStates;
  finally
    EndBusy;
  end;
end;

procedure TFrmMain.PopulateResolutions;
var
  sFormatDescription : string;
  iSelectedIndex : Integer;
  oFormat : TVideoFormat;
  i : Integer;
begin
  cbxResolution.Clear;

  if Assigned(FCapture) and FCapture.SourceOpen then
  begin
    Log('Populating device resolutions', ltInfo);
    iSelectedIndex := -1;

    for oFormat in FCapture.VideoFormats  do
    begin
      sFormatDescription := Format('%d x %d (%d fps. %s)', [oFormat.iFrameWidth, oFormat.iFrameHeigth, oFormat.iFrameRateNumerator, GetGUIDNameConst(oFormat.oSubType)]);
//      if oFormats[i].bSelected then
//        iSelectedIndex := i;
      cbxResolution.Items.Add(sFormatDescription);
    end;

    if iSelectedIndex > - 1 then
      cbxResolution.ItemIndex := iSelectedIndex;

    Log(Format('Found (%d) device resolutions', [cbxResolution.Items.Count]), ltInfo);
  end;
end;

procedure TFrmMain.pnlVideoResize(Sender: TObject);
var
  crD: TRECT;
  pcrD: LPRECT;
begin
  // Set video size
  if Assigned(MfDeviceCapture) then
  begin
    crD.left := 0;
    crD.top := 0;
    crD.right := pnlVideo.ClientWidth;
    crD.bottom := pnlVideo.ClientHeight;
    pcrD := @crD;
    MfDeviceCapture.ResizeVideo(pcrD);
  end;
end;

procedure TFrmMain.PopulateDeviceList;
var
  oResult : HRESULT;
  i : Integer;
  iCount : Integer;
  iIndex : Integer;
  oDeviceProperties : TDevicePropertiesArray;
begin
  BeginBusy;
  try
    SetLength(oDeviceProperties, 0);
    cbxCaptureDevices.Clear;

    oResult := EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                                   oDeviceProperties);
    cbxCaptureDevices.Items.Add('None');

    SetLength(FDevices, Length(oDeviceProperties));

    // Update display name for devices with the same name.
    for i:= Low(oDeviceProperties) to High(oDeviceProperties) do
    begin
      if DeviceExists(FDevices, oDeviceProperties[i].sFriendlyName, iIndex) then
      begin
        iCount := FDevices[iIndex].iCount + 1;
        if iCount = 2 then
          // Update the first device to include '(1)'
          FDevices[iIndex].sUniqueName := Format('%s (%d)', [FDevices[iIndex].sOriginalName, FDevices[iIndex].iCount]);
      end
      else
        iCount := 1;

      // Keep a reference to the full device details
      FDevices[i].oExtendedDetails := oDeviceProperties[i];
      FDevices[i].sOriginalName := oDeviceProperties[i].sFriendlyName;

      if iCount > 1 then
        FDevices[i].sUniqueName := Format('%s (%d)', [oDeviceProperties[i].sFriendlyName, iCount])
      else
        FDevices[i].sUniqueName := FDevices[i].sOriginalName;
      FDevices[i].iCount := iCount;
    end;


    if SUCCEEDED(oResult) then
    begin
      for i:= Low(FDevices) to High(FDevices) do
        cbxCaptureDevices.Items.Add(FDevices[i].sUniqueName);

      cbxCaptureDevices.ItemIndex := 0;
    end;
  finally
    EndBusy;
  end;
end;

function TFrmMain.DeviceExists(ADevices : TArray<TDeviceDetails>; const AName: string; out AIndex : Integer): Boolean;
var
  i : Integer;
begin
  AIndex := -1;
  i := Length(ADevices) - 1;

  // Find the last item in the array, with the same name
  while (AIndex = -1) and (i > -1) do
  begin
    Result := SameText(AName, ADevices[i].sOriginalName);
    if Result then
      AIndex := i;
    dec(i);
  end;

  Result := AIndex >= 0;
end;

procedure TFrmMain.Log(const AText: string; ALogType: TLogType);
begin
  if ALogType >= FLogLevel then
    memLog.Lines.Add(FormatDateTime('yyyy/mm/dd HH:mm:ss.zzz',
                                    Now,
                                    FFormatSettings) + cTab + ALogType.AsDisplay + cTab + 'Memory Used: ' + GetMemoryUsed + cTab + AText);
end;

procedure TFrmMain.UpdateEnabledStates;
var
  oCurrentFormat : TVideoFormat;
begin
  btnCaptureFrame.Enabled := FCapture.SourceOpen;
  cbxResolution.Enabled := FCapture.SourceOpen and (cbxCaptureDevices.ItemIndex > 0);
  btnSaveImage.Enabled := Assigned(picFrame.Picture.Bitmap) and not picFrame.Picture.Bitmap.Empty;
  btnStartBurst.Enabled := FCapture.SourceOpen and not FBurstCaptureEnabled;
  btnStopBurst.Enabled := FBurstCaptureEnabled;

  grpFrameCapture.Caption := 'Frame Capture';

  if FCapture.SourceOpen and FCapture.GetCurrentFormat(oCurrentFormat) then
    grpFrameCapture.Caption := Format(grpFrameCapture.Caption + ' (%d x %d ) ', [oCurrentFormat.iFrameWidth, oCurrentFormat.iFrameHeigth]);
end;

procedure TFrmMain.BeginBusy;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TFrmMain.EndBusy;
begin
  Screen.Cursor := crDefault;
end;


end.
