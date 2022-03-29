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
    sdSaveFrame: TSaveDialog;
    btnCopyLog: TButton;
    tcCapture: TPageControl;
    tsFrame: TTabSheet;
    pcSetup: TPageControl;
    tsSetup: TTabSheet;
    tsOptions: TTabSheet;
    cbxRenderMode: TComboBox;
    lblRenderTypeTitle: TLabel;
    cbxDuration: TComboBox;
    lblSeconds: TLabel;
    Label2: TLabel;
    lblResolution: TLabel;
    cbxFrameRateMin: TComboBox;
    lblFPSDesc: TLabel;
    tsDiagnostics: TTabSheet;
    btnCalculateMax: TButton;
    lblCurrentMethod: TLabel;
    lblMaxDesc: TLabel;
    lblMaxDesc1: TLabel;
    lblMaxDesc2: TLabel;
    lblLogLevel: TLabel;
    cboLogLevel: TComboBox;
    pbCapture: TPaintBox;
    lblMaxTitle: TLabel;
    cboMethod: TComboBox;
    btnToggleBurst: TButton;
    btnCaptureFrame: TButton;
    lblMethod: TLabel;
    btnSaveImage: TButton;
    chkDisplayPreview: TCheckBox;
    cbxRenderType: TComboBox;
    lblPreviewType: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDevicesClick(Sender: TObject);
    procedure HandleSelectedDeviceChange(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure HandleCaptureFrame(Sender: TObject);
    procedure HandleSaveImageClick(Sender: TObject);
    procedure cbxResolutionChange(Sender: TObject);
    procedure HandlFormShow(Sender: TObject);
    procedure HandleToggleBurst(Sender: TObject);
    procedure HandleStopBurstCapture(Sender: TObject);
    procedure HandleLogLevelChange(Sender: TObject);
    procedure HandleCopyLog(Sender: TObject);
    procedure HandleMethodChanged(Sender: TObject);
    procedure HandleCapturePaint(Sender: TObject);
    procedure HandlePreviewTypeChange(Sender: TObject);
    procedure HandleRenderModeChange(Sender: TObject);
    procedure HandleMinimumFrameRateChange(Sender: TObject);
    procedure HandleCalculateMax(Sender: TObject);
    procedure HandleDisplayImage(Sender: TObject);
    procedure HandlePreviewChange(Sender: TObject);
  private
    FLogLevel: TLogType;
    FFormatSettings: TFormatSettings;
    FCapture: TCameraCapture;
    FCaptureMethod: TCaptureMethod;
    FBurstCaptureCount : Integer;
    FLastCapturedFrame : TBitmap;

    FDefaultDeviceName : string;
    FDefaultResolution : string;
    FDevices : TArray<TDeviceDetails>;
    FBurstStartTime : TDateTime;
    FBurstDurationSeconds : Integer;
    FBurstStatisticsUpdate : TDateTime;
    FCurrentDevice : TDeviceDetails;
    FCurrentCaptureFormat : TVideoFormat;
    FLastMemoryStream : TMemoryStream;

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
    procedure HandleCalculateMaxComplete(const AFramesPerSecond: Integer);
    procedure StartBurstCapture;
    procedure HandleFrameDataFound(AMemoryStream : TMemoryStream);
    procedure UpdateReturnTimer;
    procedure LoadImageFromStream;
    procedure PaintMessage(const AText: string);
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
   FCapture.OnFrameDataFound := HandleFrameDataFound;
   FCapture.OnLog := Log;

  {$IFDEF DEBUG}
   Caption := Caption + ' (DEBUG BUILD)';
  {$ENDIF}

  {$IFDEF WIN32}
     Caption := Caption + ' 32-bit';
  {$ELSE}
     Caption := Caption + ' 64-bit';
  {$ENDIF}

   FLastCapturedFrame := TBitmap.Create;
   FFormatSettings := TFormatSettings.Create;
   pcSetup.ActivePageIndex := 0;

   FLastMemoryStream := TMemoryStream.Create;

   SetDefaults;

   cboMethod.ItemIndex := Ord(FCaptureMethod);

   PopulateDeviceList;
   UpdateEnabledStates;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  DestroyCapture;
  FLastMemoryStream.Free;
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

  lblCurrentMethod.Caption := 'Capture method: ' + FCaptureMethod.AsDisplay;

  FCapture.OnFrameFound := HandleFrameFound;
  FCapture.OnFrameDataFound := HandleFrameDataFound;
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

  // Set a default values for selection on load.
  // Examples below
  //FDefaultDeviceName := 'HD Webcam C615';
  //FDefaultResolution := '1920 x 1080   (30 fps)    MFVideoFormat_NV12';

  CaptureMethod := cmASync;

  FCapture.CaptureReturnType := rtMemoryStream;

  FCapture.MinimumFrameRate := StrToInt(cbxFrameRateMin.Text);
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

procedure TFrmMain.HandleCaptureFrame(Sender: TObject);
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
    Result := FCapture.SetVideoFormat(cbxResolution.ItemIndex) and FCapture.GetCurrentFormat(FCurrentCaptureFormat);

    if Result then
      Log(Format('Capture format change to %d x %d', [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]), ltInfo)
    else
      Log('Failed to set capture format', ltError);

    UpdateEnabledStates;
  end;
end;

procedure TFrmMain.HandleCalculateMax(Sender: TObject);
begin
  btnCalculateMax.Caption := 'Calculating...';
  BeginBusy;
  try
    FCapture.CalculateMaxFrameRate(HandleCalculateMaxComplete);
  finally
    EndBusy;
  end;
end;

procedure TFrmMain.HandleCalculateMaxComplete(const AFramesPerSecond : Integer);
begin
  btnCalculateMax.Caption := 'Calculate Max';
  MessageDlg(Format('Estimated readable frame rate: %d (fps).' + #13#10 +
          'Current Format: %d x %d (%d fps)', [AFramesPerSecond,
          FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth, FCurrentCaptureFormat.iFramesPerSecond]), mtInformation,
          [mbOk], 0, mbOk);
end;

procedure TFrmMain.HandleCapturePaint(Sender: TObject);
begin
  PaintLastCapture;
end;

procedure TFrmMain.HandleCopyLog(Sender: TObject);
begin
  Clipboard.AsText := memLog.Lines.Text;
end;

procedure TFrmMain.HandleDisplayImage(Sender: TObject);
begin
  LoadImageFromStream;
end;

procedure TFrmMain.LoadImageFromStream;
begin
  if Assigned(FLastMemoryStream) then
  begin
    try
      FLastCapturedFrame.LoadFromStream(FLastMemoryStream);
      FLastCapturedFrame.PixelFormat := pf24bit;
      PaintLastCapture;
    except
      on E : Exception do
        Log('Failed to load BMP from memory stream. Error: %s' + E.Message, ltError);
    end;
  end;
end;

procedure TFrmMain.HandleFrameDataFound(AMemoryStream : TMemoryStream);
begin
  try
    FLastMemoryStream.Clear;
    FLastMemoryStream.LoadFromStream(AMemoryStream);

    UpdateReturnTimer;
    HandleBurstMode;
    UpdateCaptureButtons;
  finally
    AMemoryStream.Free;
  end;

  if chkDisplayPreview.Checked then
    LoadImageFromStream;
end;

procedure TFrmMain.HandleFrameFound(ABitmap: TBitmap);
begin
  // Do as little as possible here, otherwise burst captured will be slowed.
  UpdateReturnTimer;

  try
    // Display captured frame
    UpdateCapturedFrame(ABitmap);
  finally
    FreeAndNil(ABitmap);
  end;

  HandleBurstMode;

  UpdateCaptureButtons;
end;

procedure TFrmMain.UpdateReturnTimer;
var
  iDuration : Integer;
  iFrameRate : Integer;
begin
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
    Log(Format('Returned image in %f milliseconds. %d x %d. Method: %s',
               [FCapture.GetTimerMs, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth, FCapture.CaptureReturnType.AsDisplay]),
                                 ltInfo);
end;

procedure TFrmMain.UpdateCapturedFrame(const ABitmap : TBitmap);
begin
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
  if not chkDisplayPreview.Checked then
    PaintMessage('Image Preview is disabled')
  else
  if Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty then
  begin
    // Scale and center the image
    GetPaintArea(iWidth, iHeight, iTop, iLeft);

    SetStretchBltMode(pbCapture.Canvas.Handle,HALFTONE);
    SetBrushOrgEx(pbCapture.Canvas.Handle, 0, 0, nil);

    // Stretch draw
    StretchBlt(pbCapture.Canvas.Handle, iLeft, iTop, iWidth,
      iHeight, FLastCapturedFrame.Canvas.Handle, 0, 0, FLastCapturedFrame.Width,
      FLastCapturedFrame.Height, SRCCOPY);
  end
  else
    PaintMessage('Waiting for image capture... ')
end;

procedure TFrmMain.PaintMessage(const AText : string);
var
  iWidth : Integer;
begin
  pbCapture.Canvas.Brush.Style := bsClear;
  pbCapture.Canvas.Font.Size := 12;
  iWidth := pbCapture.Canvas.TextWidth(AText);
  pbCapture.Canvas.Font.Name := 'Segoe UI';
  pbCapture.Canvas.TextOut(Round((pbCapture.Width - iWidth) / 2), 20, AText);
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
  // To DO
end;

procedure TFrmMain.HandleSaveImageClick(Sender: TObject);
begin
  if not FLastCapturedFrame.Empty then
  begin
    sdSaveFrame.FileName := 'Capture';

    if sdSaveFrame.Execute then
      SaveImage(FLastCapturedFrame, sdSaveFrame.FileName, TImageType(Ord(sdSaveFrame.FilterIndex - 1)));
  end;
end;

procedure TFrmMain.HandleSelectedDeviceChange(Sender: TObject);
begin
  UpdateSelectedDevice;
end;

procedure TFrmMain.HandleToggleBurst(Sender: TObject);
begin
  if FCapture.BurstEnabled then
    StopBurstCapture
  else
    StartBurstCapture;
end;

procedure TFrmMain.StartBurstCapture;
begin
  FBurstCaptureCount := 0;
  FBurstStartTime := Now;
  FBurstStatisticsUpdate := Now;
  FBurstDurationSeconds := StrToInt(cbxDuration.Text);
  FCapture.StartBurst;
  btnToggleBurst.Caption := 'Stop Burst Capture';
  UpdateEnabledStates;
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

  btnToggleBurst.Caption := 'Start Burst Capture';

  UpdateEnabledStates;

  GetBurstDetails(iDuration, iFrameRate);

  Log(Format('Burst capture stopped. Method: %s. Source: %d x %d (%d fps). Captured %d frames in %d seconds (%d fps).',
   [FCapture.CaptureReturnType.AsDisplay, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth,
   FCurrentCaptureFormat.iFramesPerSecond, FBurstCaptureCount, iDuration, iFrameRate]), ltInfo);
end;

procedure TFrmMain.GetBurstDetails(var ADurationSec : Integer; var AFramesPerSecond : Integer);
begin
  ADurationSec := SecondsBetween(FBurstStartTime, Now);

  if ADurationSec > 0 then
    AFramesPerSecond := Round(Max(1, FBurstCaptureCount) / ADurationSec)
  else
    AFramesPerSecond := 0;
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
    SetLength(FDevices, 0);

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

procedure TFrmMain.UpdateEnabledStates;
begin
  UpdateCaptureButtons;

  tsFrame.Caption := 'Frame Capture';

  if FCapture.SourceOpen then
    tsFrame.Caption := Format('Frame Capture (%d x %d ) ', [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]);
end;

procedure TFrmMain.UpdateCaptureButtons;
begin
  btnCaptureFrame.Enabled := FCapture.SourceOpen and not FCapture.BurstEnabled;
  cbxResolution.Enabled := FCapture.SourceOpen and (cbxCaptureDevices.ItemIndex > 0);
  btnSaveImage.Enabled := Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty;
  btnToggleBurst.Enabled := FCapture.SourceOpen;
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
  FLogLevel := TLogType(cboLogLevel.ItemIndex);
end;

procedure TFrmMain.HandleMethodChanged(Sender: TObject);
begin
  if Sender is TComboBox then
  begin
    ClearImage;
    CaptureMethod := TCaptureMethod(TComboBox(Sender).ItemIndex);
  end;
end;

procedure TFrmMain.HandleMinimumFrameRateChange(Sender: TObject);
begin
  FCapture.MinimumFrameRate := StrToInt(cbxFrameRateMin.Text);
  UpdateSelectedDevice;
end;

procedure TFrmMain.HandlePreviewChange(Sender: TObject);
begin
  pbCapture.Repaint;
end;

procedure TFrmMain.HandlePreviewTypeChange(Sender: TObject);
begin
  FCapture.CaptureReturnType := TCaptureReturnType(ord(cbxRenderType.ItemIndex));
end;


end.
