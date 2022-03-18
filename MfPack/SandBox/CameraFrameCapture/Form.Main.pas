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
  System.Math,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Clipbrd,
  Vcl.ComCtrls,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfIdl,
  {Application}
  Support,
  SampleConverter,
  CameraCapture,
  CameraCapture.Asynchronous,
  CameraCapture.Synchronous;


type
  TCapturePreviewType = (ptNone, ptNormal);

  TDeviceDetails = record
    oExtendedDetails : TDeviceProperties;
    sOriginalName : string;
    sUniqueName : string;
    iCount : Integer;
  end;

  TFrmMain = class(TForm)
    cbxCaptureDevices: TComboBox;
    lblSelectDevice: TLabel;
    lblResoltution: TLabel;
    cbxResolution: TComboBox;
    btnRefreshDevices: TButton;
    memLog: TMemo;
    lblLog: TLabel;
    btnClearLog: TButton;
    btnCaptureFrame: TButton;
    btnSaveImage: TButton;
    sdSaveFrame: TSaveDialog;
    btnStartBurst: TButton;
    btnStopBurst: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnEdit: TMenuItem;
    mnLogLevel: TMenuItem;
    mnDebugLevel: TMenuItem;
    mnInfoLevel: TMenuItem;
    mnWarningLevel: TMenuItem;
    mnErrorLevel: TMenuItem;
    btnCopyLog: TButton;
    lblSupported: TLabel;
    lblMethod: TLabel;
    cboMethod: TComboBox;
    tcCapture: TPageControl;
    tsFrame: TTabSheet;
    pnlFrameCapture: TPanel;
    pbCapture: TPaintBox;
    pcSetup: TPageControl;
    tsSetup: TTabSheet;
    tsAdvanced: TTabSheet;
    cbxPreviewType: TComboBox;
    cbxRenderMode: TComboBox;
    lblPreviewType: TLabel;
    Label1: TLabel;
    cbxDuration: TComboBox;
    lblSeconds: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDevicesClick(Sender: TObject);
    procedure HandleSelectedDeviceChange(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnCaptureFrameClick(Sender: TObject);
    procedure HandleSaveImageClick(Sender: TObject);
    procedure cbxResolutionChange(Sender: TObject);
    procedure HandlFormShow(Sender: TObject);
    procedure HandleStartBurstCapture(Sender: TObject);
    procedure HandleStopBurstCapture(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure HandleLogLevelChange(Sender: TObject);
    procedure HandleCopyLog(Sender: TObject);
    procedure HandleMethodChanged(Sender: TObject);
    procedure HandleCapturePaint(Sender: TObject);
    procedure HandleChangeEnablePreview(Sender: TObject);
    procedure HandlePreviewTypeChange(Sender: TObject);
    procedure HandleRenderModeChange(Sender: TObject);
  private
    FLogLevel: TLogType;
    FFormatSettings: TFormatSettings;
    FCapture: TCameraCapture;
    FCaptureMethod: TCaptureMethod;
    FBurstCaptureCount : Integer;
    FLastCapturedFrame : TBitmap;
    FPreviewType : TCapturePreviewType;

    FDefaultDeviceName : string;
    FDefaultResolution : string;
    FDevices : TArray<TDeviceDetails>;
    FBurstStartTime : TDateTime;
    FBurstDurationSeconds : Integer;
    FBurstStatisticsUpdate : TDateTime;
    FCurrentDevice : TDeviceDetails;
    FCurrentCaptureFormat : TVideoFormat;

    function UpdateCaptureFormat : Boolean;
    function DeviceExists(ADevices : TArray<TDeviceDetails>; const AName: string; out AIndex : Integer): Boolean;

    procedure PopulateDeviceList;
    procedure Log(const AText: string; ALogType: TLogType);
    procedure SetDefaults;
    procedure BeginBusy;
    procedure EndBusy;
    procedure SetDevice(const ADevice : TDeviceDetails);
    procedure HandleFrameFound(ABitmap: TBitmap);
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure PopulateResolutions;
    procedure RestoreDefaults;
    procedure UpdateSelectedDevice;
    procedure ClearValues;
    procedure RequestFrame;
    procedure UpdateLogLevelMenu;
    procedure HandleResolutionChanged;
    procedure StopBurstCapture;
    procedure HandleBurstMode;
    procedure UpdateCapturedFrame(const ABitmap: TBitmap);
    procedure UpdateCaptureButtons;
    procedure SetCaptureMethod(const AValue: TCaptureMethod);
    procedure DestroyCapture;
    procedure PaintLastCapture;
    procedure GetPaintArea(var AWidth : Integer; var AHeight : Integer; var ATop : Integer; var ALeft : Integer);
    procedure GetBurstDetails(var ADurationSec, AFramesPerSecond: Integer);
  public
    property CaptureMethod: TCaptureMethod read FCaptureMethod write SetCaptureMethod;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  System.DateUtils;

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

   FLastCapturedFrame := TBitmap.Create;

   FFormatSettings := TFormatSettings.Create;

   pcSetup.ActivePageIndex := 0;

   SetDefaults;

   cboMethod.ItemIndex := Ord(FCaptureMethod);

   PopulateDeviceList;
   UpdateEnabledStates;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  DestroyCapture;
  FLastCapturedFrame.Free;
  MFShutdown;
  CoUnInitialize;
end;

procedure TFrmMain.ClearValues;
begin
  FBurstStatisticsUpdate := 0;
  FBurstDurationSeconds := 0;
  SetLength(FDevices, 0);
end;

procedure TFrmMain.SetCaptureMethod(const AValue: TCaptureMethod);
var
  iCurrentResolutionIndex : Integer;
begin
  iCurrentResolutionIndex := cbxResolution.ItemIndex;

  FCaptureMethod := AValue;

  DestroyCapture;

  if FCaptureMethod = cmSync then
    FCapture := TCameraCaptureSync.Create
  else
    FCapture := TCameraCaptureAsync.Create;

  FCapture.OnFrameFound := HandleFrameFound;
  FCapture.OnLog := Log;

  UpdateSelectedDevice;
  cbxResolution.ItemIndex := iCurrentResolutionIndex;
  HandleResolutionChanged;

  FCapture.GetCurrentFormat(FCurrentCaptureFormat);
  UpdateEnabledStates;
end;

procedure TFrmMain.DestroyCapture;
begin
  if Assigned(FCapture) then
    FreeAndNil(FCapture);
end;

procedure TFrmMain.SetDefaults;
begin
  FLogLevel := ltInfo;
  FPreviewType := ptNormal;

  // Set a default values for selection on load.
  // Examples below
  //FDefaultDeviceName := 'HD Webcam C615';
  //FDefaultResolution := '1920 x 1080   (30 fps)    MFVideoFormat_NV12';

  UpdateLogLevelMenu;

  CaptureMethod := cmAsync;
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
  if not FCapture.BurstEnabled then
    ClearImage;

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

procedure TFrmMain.HandleRenderModeChange(Sender: TObject);
begin
  FCapture.SampleConverter.RenderType := TRenderType(Ord(cbxRenderMode.ItemIndex));
end;

procedure TFrmMain.HandleResolutionChanged;
begin
  if cbxResolution.ItemIndex > -1 then
  begin
    ClearImage;
    BeginBusy;
    try
      UpdateCaptureFormat;
    finally
      EndBusy;
    end;
  end;
end;

function TFrmMain.UpdateCaptureFormat: Boolean;
begin
  Result := FCapture.SourceOpen;

  if Result then
  begin
    if FCapture.FormatSupported(cbxResolution.ItemIndex) then
    begin
      lblSupported.Caption := 'Format is supported for capture';
      lblSupported.Font.Color := clGreen;

      Log('Updating capture format', ltInfo);

      Result := FCapture.SetVideoFormat(cbxResolution.ItemIndex) and FCapture.GetCurrentFormat(FCurrentCaptureFormat);

      if Result then
        Log(Format('Capture format change to %d x %d', [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]), ltInfo)
      else
        Log('Failed to set capture format', ltError);

      UpdateEnabledStates;
    end
    else
    begin
      lblSupported.Caption := 'Format is NOT supported for capture';
      lblSupported.Font.Color := clRed;
    end;
  end;
end;

procedure TFrmMain.HandleCapturePaint(Sender: TObject);
begin
  PaintLastCapture;
end;

procedure TFrmMain.HandleChangeEnablePreview(Sender: TObject);
begin
  if FPreviewType <> ptNone then
    pnlFrameCapture.Caption := 'No Image. Waiting frame capture...'
  else
  begin
    ClearImage;
    pnlFrameCapture.Caption := 'Frame capture preview is disabled.';
    PaintLastCapture;
  end;
end;

procedure TFrmMain.HandleCopyLog(Sender: TObject);
begin
  Clipboard.AsText := memLog.Lines.Text;
end;

procedure TFrmMain.HandleFrameFound(ABitmap: TBitmap);
var
  iDuration : Integer;
  iFrameRate : Integer;
begin
  // Do as little as possible here, otherwise burst captured will be slowed.
  FCapture.StopTimer;

  if FCapture.BurstEnabled then
  begin
    if MillisecondsBetween(Now, FBurstStatisticsUpdate) > 1000 then
    begin
      GetBurstDetails(iDuration, iFrameRate);
      Log(Format('Burst in progress. Current frame rate %d in %d seconds', [iFrameRate, iDuration]), ltInfo);
      FBurstStatisticsUpdate := Now;
    end;
  end
  else
    Log(Format('Image returned in %f milliseconds. %d x %d.',
               [FCapture.GetTimerMs, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]),
                                 ltInfo);

  try
    // Display captured frame
      UpdateCapturedFrame(ABitmap);
  finally
    FreeAndNil(ABitmap);
  end;

  HandleBurstMode;

  UpdateCaptureButtons;
end;

procedure TFrmMain.UpdateCapturedFrame(const ABitmap : TBitmap);
begin
  pnlFrameCapture.Caption := '';
  FLastCapturedFrame.Assign(ABitmap);
  PaintLastCapture;
end;

procedure TFrmMain.PaintLastCapture;
var
  iWidth : Integer;
  iHeight : Integer;
  iTop : Integer;
  iLeft : Integer;
begin
  if (FPreviewType <> ptNone) and Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty then
  begin
    // Scale and center the image
    GetPaintArea(iWidth, iHeight, iTop, iLeft);

    SetStretchBltMode(pbCapture.Canvas.Handle,HALFTONE);
    SetBrushOrgEx(pbCapture.Canvas.Handle, 0, 0, nil);

    // Stretch draw
    StretchBlt(pbCapture.Canvas.Handle, iLeft, iTop, iWidth,
      iHeight, FLastCapturedFrame.Canvas.Handle, 0, 0, FLastCapturedFrame.Width,
      FLastCapturedFrame.Height, SRCCOPY);
  end;
end;

procedure TFrmMain.GetPaintArea(var AWidth : Integer; var AHeight : Integer; var ATop : Integer; var ALeft : Integer);
var
  iRatio : Double;
  iHeightRatio : Double;
  iWidthRatio : Double;
begin
  iHeightRatio := pbCapture.Height / FLastCapturedFrame.Height;
  iWidthRatio := pbCapture.Width / FLastCapturedFrame.Width;

  if iHeightRatio > iWidthRatio then
    iRatio := Min(1, iWidthRatio)
  else
    iRatio := Min(1, iHeightRatio);

  AWidth := Round(FLastCapturedFrame.Width * iRatio);
  AHeight := Round(FLastCapturedFrame.Height * iRatio);
  ATop := (pbCapture.Height - AHeight) div 2;
  ALeft := (pbCapture.Width - AWidth) div 2;
end;

procedure TFrmMain.HandleBurstMode;
begin
  if FCapture.BurstEnabled then
  begin
    inc(FBurstCaptureCount);
    if (SecondsBetween(Now, FBurstStartTime) >= FBurstDurationSeconds) then
      StopBurstCapture;
  end;
end;


procedure TFrmMain.ClearImage;
begin
  pnlFrameCapture.Caption := 'No Image. Waiting frame capture...';
end;

procedure TFrmMain.HandleSaveImageClick(Sender: TObject);
begin
  if not FLastCapturedFrame.Empty then
  begin
    sdSaveFrame.FileName := 'Capture';

    if sdSaveFrame.Execute then
      SaveImage(FLastCapturedFrame, sdSaveFrame.FileName, TImageType(Ord(sdSaveFrame.FilterIndex)));
  end;
end;

procedure TFrmMain.HandleSelectedDeviceChange(Sender: TObject);
begin
  UpdateSelectedDevice;
end;

procedure TFrmMain.HandleStartBurstCapture(Sender: TObject);
begin
  UpdateEnabledStates;
  FBurstCaptureCount := 0;
  FBurstStartTime := Now;
  FBurstStatisticsUpdate := Now;
  FBurstDurationSeconds := StrToInt(cbxDuration.Text);
  FCapture.StartBurst;
end;

procedure TFrmMain.HandleStopBurstCapture(Sender: TObject);
begin
  StopBurstCapture;
end;

procedure TFrmMain.StopBurstCapture;
var
  iDuration : Integer;
  iFrameRate : Integer;
begin
  FCapture.StopBurst;

  UpdateEnabledStates;

  GetBurstDetails(iDuration, iFrameRate);

  Log(Format('Burst capture stopped. Source: %d x %d (%d fps). Captured %d frames in %d seconds (%d fps).',
   [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth,
   FCurrentCaptureFormat.iFramesPerSecond, FBurstCaptureCount, iDuration, iFrameRate]), ltInfo);
end;

procedure TFrmMain.GetBurstDetails(var ADurationSec : Integer; var AFramesPerSecond : Integer);
begin
  ADurationSec := SecondsBetween(FBurstStartTime, Now);
  AFramesPerSecond := Round(FBurstCaptureCount / ADurationSec);
end;

procedure TFrmMain.UpdateSelectedDevice;
begin
  FCapture.CloseSource;

  if cbxCaptureDevices.ItemIndex > 0 then
    SetDevice(FDevices[cbxCaptureDevices.ItemIndex - 1]);

  PopulateResolutions;
  UpdateEnabledStates;
end;

procedure TFrmMain.SetDevice(const ADevice : TDeviceDetails);
begin
  BeginBusy;
  try
    ClearImage;
    Log('Setting selected device to: ' + ADevice.sUniqueName, ltInfo);

    FCurrentDevice := ADevice;

    // Prepare the frame capture for the device
    if not FCapture.OpenDeviceSource(ADevice.oExtendedDetails.lpSymbolicLink) then
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
  iDefaultIndex : Integer;
begin
  cbxResolution.Clear;

  if Assigned(FCapture) and FCapture.SourceOpen then
  begin
    Log('Populating device resolutions', ltInfo);
    iSelectedIndex := -1;

    for oFormat in FCapture.VideoFormats  do
    begin
      sFormatDescription := Format('%d x %d   (%d fps)    %s',
      [oFormat.iFrameWidth, oFormat.iFrameHeigth, oFormat.iFramesPerSecond, GetGUIDNameConst(oFormat.oSubType)]);
      // TODO - Determine what format is currently selected
      cbxResolution.Items.Add(sFormatDescription);
    end;

    if iSelectedIndex > - 1 then
      cbxResolution.ItemIndex := iSelectedIndex;

    if FDefaultResolution <> '' then
    begin
      iDefaultIndex := cbxResolution.Items.IndexOf(FDefaultResolution);
      if iDefaultIndex > -1 then
      begin
        cbxResolution.ItemIndex := iDefaultIndex;
        FDefaultResolution := '';
        HandleResolutionChanged;
      end;
    end;

    Log(Format('Found (%d) supported resolutions', [cbxResolution.Items.Count]), ltInfo);
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
var
  sThread : string;
begin
  if ALogType >= FLogLevel then
  begin
    if GetCurrentThreadId = MainThreadId then
      sThread := 'Main'
    else
      sThread := IntToStr(GetCurrentThreadId);

    memLog.Lines.Add(FormatDateTime('yyyy/mm/dd HH:mm:ss.zzz',
                                    Now,
                                    FFormatSettings) + cTab + ALogType.AsDisplay + cTab + 'Thread:  ' + sThread +
                                    cTab + 'Memory Used: ' + GetMemoryUsed + cTab + AText);
  end;
end;

procedure TFrmMain.OnExit(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TFrmMain.UpdateEnabledStates;
begin
  UpdateCaptureButtons;

  tsFrame.Caption := 'Frame Capture';

  if FCapture.SourceOpen then
    tsFrame.Caption := Format('Frame Capture (%d x %d ) ', [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]);
end;

procedure TFrmMain.UpdateCaptureButtons;
begin
  btnCaptureFrame.Enabled := FCapture.SourceOpen;
  cbxResolution.Enabled := FCapture.SourceOpen and (cbxCaptureDevices.ItemIndex > 0);
  btnSaveImage.Enabled := Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty;
  btnStartBurst.Enabled := FCapture.SourceOpen and not FCapture.BurstEnabled;
  btnStopBurst.Enabled := FCapture.SourceOpen and FCapture.BurstEnabled;
end;

procedure TFrmMain.BeginBusy;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TFrmMain.EndBusy;
begin
  Screen.Cursor := crDefault;
end;

procedure TFrmMain.HandleLogLevelChange(Sender: TObject);
begin
  if Sender is TMenuItem then
    begin
      FLogLevel := TLogType(TMenuItem(Sender).Tag);
      UpdateLogLevelMenu;
    end;
end;

procedure TFrmMain.HandleMethodChanged(Sender: TObject);
begin
  if Sender is TComboBox then
  begin
    ClearImage;
    CaptureMethod := TCaptureMethod(TComboBox(Sender).ItemIndex);
  end;
end;

procedure TFrmMain.HandlePreviewTypeChange(Sender: TObject);
begin
  FPreviewType := TCapturePreviewType(ord(cbxPreviewType.ItemIndex));
end;

procedure TFrmMain.UpdateLogLevelMenu;
var
  i: Integer;

begin
  for i := 0 to mnLogLevel.Count - 1 do
    mnLogLevel.Items[i].Checked := Ord(FLogLevel) = mnLogLevel.Items[i].Tag;
end;


end.
