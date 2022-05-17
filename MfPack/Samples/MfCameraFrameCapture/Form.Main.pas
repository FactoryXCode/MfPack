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
  CameraCapture.Asynchronous, Vcl.NumberBox;

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
    tsDiagnostics: TTabSheet;
    btnCalculateMax: TButton;
    lblMaxDesc2: TLabel;
    pbCapture: TPaintBox;
    lblMaxTitle: TLabel;
    btnStartBurstCapture: TButton;
    btnCaptureFrame: TButton;
    btnSaveImage: TButton;
    chkDisplayPreview: TCheckBox;
    cbxDuration: TComboBox;
    lblSeconds: TLabel;
    cboLogLevel: TComboBox;
    lblLogLevel: TLabel;
    cbxFrameRateMin: TComboBox;
    lblFPSDesc: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    btnStopBurstCapture: TButton;
    tsAdvanced: TTabSheet;
    chkDirect2D: TCheckBox;
    tbBrightness: TTrackBar;
    lblBrightness: TLabel;
    lblBrightnessValue: TLabel;
    btnResetBrightness: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDevicesClick(Sender: TObject);
    procedure HandleSelectedDeviceChange(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure HandleCaptureFrame(Sender: TObject);
    procedure HandleSaveImageClick(Sender: TObject);
    procedure cbxResolutionChange(Sender: TObject);
    procedure HandlFormShow(Sender: TObject);
    procedure HandleStartBurstCapture(Sender: TObject);
    procedure HandleStopBurstCapture(Sender: TObject);
    procedure HandleLogLevelChange(Sender: TObject);
    procedure HandleCopyLog(Sender: TObject);
    procedure HandleCapturePaint(Sender: TObject);
    procedure HandleMinimumFrameRateChange(Sender: TObject);
    procedure HandleCalculateMax(Sender: TObject);
    procedure HandleDisplayImage(Sender: TObject);
    procedure HandlePreviewChange(Sender: TObject);
    procedure ToggleEnabledDirectX(Sender: TObject);
    procedure HandleBrightnessChanged(Sender: TObject);
    procedure HandleResetBrightness(Sender: TObject);
  private
    FLogLevel: TLogType;
    FUpdating : Boolean;
    FFormatSettings: TFormatSettings;
    FCapture: TCameraCaptureAsync;
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
    FImageCleared : Boolean;
    function UpdateCaptureFormat : Boolean;
    function DeviceExists(ADevices : TArray<TDeviceDetails>; const AName: string; out AIndex : Integer): Boolean;
    procedure PopulateDeviceList;
    procedure Log(const AText: string; ALogType: TLogType);
    procedure SetDefaults;
    procedure BeginBusy;
    procedure EndBusy;
    procedure SetDevice(const ADevice : TDeviceDetails);
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure PopulateResolutions;
    procedure RestoreDefaults;
    procedure UpdateSelectedDevice;
    procedure ClearValues;
    procedure HandleResolutionChanged;
    procedure StopBurstCapture;
    procedure HandleBurstMode;
    procedure DestroyCapture;
    procedure PaintLastCapture;
    procedure GetPaintArea(AImage : TBitmap; var AWidth : Integer; var AHeight : Integer; var ATop : Integer; var ALeft : Integer);
    procedure GetBurstDetails(var ADurationSec, AFramesPerSecond: Integer);
    procedure HandleCalculateMaxComplete(const AFramesPerSecond: Integer);
    procedure StartBurstCapture;
    procedure HandleFrameDataFound(AMemoryStream : TMemoryStream);
    procedure UpdateReturnTimer;
    procedure LoadImageFromStream;
    procedure PaintMessage(const AText: string);
    procedure UpdateLogLevel;
    procedure GetCurrentBrightness;
    procedure SetBrightness(AValue : Integer);
    procedure CopyStream(AMemoryStream: TMemoryStream);
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
   FCapture.OnFrameDataFound := HandleFrameDataFound;
   FCapture.OnLog := Log;
  {$IFDEF DEBUG}
   Caption := Caption + ' (DEBUG BUILD)';
   cboLogLevel.ItemIndex := 1;
  {$ELSE}
   cboLogLevel.ItemIndex := 2;
  {$ENDIF}
  {$IFDEF WIN32}
     Caption := Caption + ' 32-bit';
  {$ELSE}
     Caption := Caption + ' 64-bit';
  {$ENDIF}
   FUpdating := False;

   lblBrightnessValue.Caption := '';
   FLastCapturedFrame := TBitmap.Create;
   FFormatSettings := TFormatSettings.Create;
   pcSetup.ActivePageIndex := 0;
   FLastMemoryStream := TMemoryStream.Create;
   SetDefaults;
   PopulateDeviceList;
   UpdateEnabledStates;
   UpdateLogLevel;
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
  FImageCleared := False;
  FBurstStatisticsUpdate := 0;
  FBurstDurationSeconds := 0;
  SetLength(FDevices, 0);
end;

procedure TFrmMain.DestroyCapture;
begin
  if Assigned(FCapture) then
    FreeAndNil(FCapture);
end;

procedure TFrmMain.SetDefaults;
begin
  // Set a default values for selection on load.
  // Examples below
  //FDefaultDeviceName := 'HD USB CAMERA';
  //FDefaultResolution := '3840 x 2160   (30 fps)    MFVideoFormat_NV12';
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

procedure TFrmMain.HandleResetBrightness(Sender: TObject);
begin
  tbBrightness.Position := FCapture.BrightnessControl.FDefault;
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

    GetCurrentBrightness;
    UpdateEnabledStates;
  end;
end;

procedure TFrmMain.GetCurrentBrightness;
begin
  FUpdating := True;
  try
    btnResetBrightness.Enabled := FCapture.BrightnessControl.FManualControl;

    tbBrightness.Enabled := FCapture.BrightnessControl.FManualControl;
    tbBrightness.Min := FCapture.BrightnessControl.FMin;
    tbBrightness.Max := FCapture.BrightnessControl.FMax;

    tbBrightness.Position := FCapture.Brightness;
    lblBrightnessValue.Caption := tbBrightness.Position.ToString;
  finally
    FUpdating := False;
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
  PaintLastCapture;
end;

procedure TFrmMain.LoadImageFromStream;
var
  iTimerStart : Int64;
  iTimerEnd : Int64;
begin
  if Assigned(FLastMemoryStream) then
  begin
    QueryPerformanceCounter(iTimerStart);
    try
      FLastCapturedFrame.PixelFormat := pf24bit;
      FLastCapturedFrame.LoadFromStream(FLastMemoryStream);


      QueryPerformanceCounter(iTimerEnd);
        Log(Format('LoadImageFromStream took %f milliseconds. %d x %d.',
               [(iTimerEnd - iTimerStart) / TimerFrequency * 1000, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]),
                                 ltDebug1);
    except
      on E : Exception do
        Log('Failed to load BMP from memory stream. Error: %s' + E.Message, ltError);
    end;
  end;
end;

procedure TFrmMain.HandleFrameDataFound(AMemoryStream : TMemoryStream);
begin
  FImageCleared := False;
  UpdateReturnTimer;

  try
    CopyStream(AMemoryStream);
  finally
    AMemoryStream.Free;
  end;

  if chkDisplayPreview.Checked then
  begin
    LoadImageFromStream;
    PaintLastCapture;
  end;

  HandleBurstMode;
end;

procedure TFrmMain.CopyStream(AMemoryStream : TMemoryStream);
begin
  FLastMemoryStream.Clear;
  FLastMemoryStream.LoadFromStream(AMemoryStream);
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
    Log(Format('Returned image in %f milliseconds. %d x %d.',
               [FCapture.GetTimerMs, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]),
                                 ltInfo);
end;

procedure TFrmMain.PaintLastCapture;
var
  iWidth : Integer;
  iHeight : Integer;
  iTop : Integer;
  iLeft : Integer;
  iTimerStart: int64;
  iTimerEnd : int64;
begin
  if not chkDisplayPreview.Checked then
    PaintMessage('Image Preview is disabled')
  else
  if not FImageCleared and Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty then
  begin
    QueryPerformanceCounter(iTimerStart);
    SetStretchBltMode(pbCapture.Canvas.Handle,HALFTONE);
    SetBrushOrgEx(pbCapture.Canvas.Handle, 0, 0, nil);

    // Scale and center the image
    GetPaintArea(FLastCapturedFrame, iWidth, iHeight, iTop, iLeft);

    // Stretch draw the whole image
    StretchBlt(pbCapture.Canvas.Handle, iLeft, iTop, iWidth,
      iHeight, FLastCapturedFrame.Canvas.Handle, 0, 0, FLastCapturedFrame.Width,
      FLastCapturedFrame.Height, SRCCOPY);

    QueryPerformanceCounter(iTimerEnd);
      Log(Format('Paint image took %f milliseconds. %d x %d.',
             [(iTimerEnd - iTimerStart) / TimerFrequency * 1000, FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth]),
                               ltDebug1);
  end
  else
    PaintMessage('Waiting for image capture... ')
end;

procedure TFrmMain.PaintMessage(const AText : string);
var
  iWidth : Integer;
begin
  pbCapture.Canvas.Brush.Color := clBtnFace;
  pbCapture.Canvas.FillRect(pbCapture.BoundsRect);
  pbCapture.Canvas.Brush.Style := bsClear;
  pbCapture.Canvas.Font.Size := 12;
  iWidth := pbCapture.Canvas.TextWidth(AText);
  pbCapture.Canvas.Font.Name := 'Segoe UI';
  pbCapture.Canvas.TextOut(Round((pbCapture.Width - iWidth) / 2), 20, AText);
end;

procedure TFrmMain.GetPaintArea(AImage : TBitmap; var AWidth : Integer; var AHeight : Integer; var ATop : Integer; var ALeft : Integer);
var
  iRatio : Double;
  iHeightRatio : Double;
  iWidthRatio : Double;
begin
  iHeightRatio := pbCapture.Height / AImage.Height;
  iWidthRatio := pbCapture.Width / AImage.Width;
  if iHeightRatio > iWidthRatio then
    iRatio := Min(1, iWidthRatio)
  else
    iRatio := Min(1, iHeightRatio);

  AWidth := Round(AImage.Width * iRatio);
  AHeight := Round(AImage.Height * iRatio);
  ATop := (pbCapture.Height - AHeight) div 2;
  ALeft := (pbCapture.Width - AWidth) div 2;
end;

procedure TFrmMain.HandleBrightnessChanged(Sender: TObject);
begin
  SetBrightness(tbBrightness.Position);
end;

procedure TFrmMain.SetBrightness(AValue : Integer);
begin
  FCapture.Brightness := AValue;
  lblBrightnessValue.Caption := AValue.ToString;

  if not FUpdating and FCapture.SourceOpen then
    FCapture.RequestFrame;
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
  FImageCleared := True;
  PaintLastCapture;
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

procedure TFrmMain.HandleStartBurstCapture(Sender: TObject);
begin
  if not FCapture.BurstEnabled then
    StartBurstCapture;
end;

procedure TFrmMain.StartBurstCapture;
begin
  FBurstCaptureCount := 0;
  FBurstStartTime := Now;
  FBurstStatisticsUpdate := Now;
  FBurstDurationSeconds := StrToInt(cbxDuration.Text);
  FCapture.StartBurst;
  UpdateEnabledStates;
end;

procedure TFrmMain.HandleStopBurstCapture(Sender: TObject);
begin
  if FCapture.BurstEnabled then
    StopBurstCapture;
end;

procedure TFrmMain.StopBurstCapture;
var
  iDuration : Integer;
  iFrameRate : Integer;
begin
  GetBurstDetails(iDuration, iFrameRate);
  FCapture.StopBurst;
  UpdateEnabledStates;
  Log(Format('Burst capture stopped. Source: %d x %d (%d fps). Captured %d frames in %d seconds (%d fps).',
   [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth,
   FCurrentCaptureFormat.iFramesPerSecond, FBurstCaptureCount, iDuration, iFrameRate]), ltInfo);
end;

procedure TFrmMain.ToggleEnabledDirectX(Sender: TObject);
begin
  FCapture.EnabledDirectX := chkDirect2D.Checked;
  UpdateSelectedDevice;
  GetCurrentBrightness;
  FCapture.RequestFrame;
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
  oFormat : TVideoFormat;
  iDefaultIndex : Integer;
  sCurrentResolution : string;
begin
  if FDefaultResolution <> '' then
    sCurrentResolution := FDefaultResolution
  else
    sCurrentResolution := cbxResolution.Text;

  cbxResolution.Clear;
  if Assigned(FCapture) and FCapture.SourceOpen then
  begin
    Log('Populating device resolutions', ltInfo);
    for oFormat in FCapture.VideoFormats  do
    begin
      sFormatDescription := Format('%d x %d   (%d fps)    %s',
      [oFormat.iFrameWidth, oFormat.iFrameHeigth, oFormat.iFramesPerSecond, GetGUIDNameConst(oFormat.oSubType)]);
      cbxResolution.Items.Add(sFormatDescription);
    end;

    if sCurrentResolution <> '' then
    begin
      iDefaultIndex := cbxResolution.Items.IndexOf(sCurrentResolution);
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
  if FCapture.SourceOpen then
    tsFrame.Caption := Format('Frame Capture (%d x %d ) ', [FCurrentCaptureFormat.iFrameWidth, FCurrentCaptureFormat.iFrameHeigth])
  else
    tsFrame.Caption := 'Frame Capture';

  btnCaptureFrame.Enabled := FCapture.SourceOpen and not FCapture.BurstEnabled;
  cbxResolution.Enabled := FCapture.SourceOpen and (cbxCaptureDevices.ItemIndex > 0);
  btnSaveImage.Enabled := Assigned(FLastCapturedFrame) and not FLastCapturedFrame.Empty;
  btnStartBurstCapture.Enabled := FCapture.SourceOpen and not FCapture.BurstEnabled;
  btnStopBurstCapture.Enabled :=  FCapture.SourceOpen and FCapture.BurstEnabled;
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
  UpdateLogLevel;
end;

procedure TFrmMain.UpdateLogLevel;
begin
  FLogLevel := TLogType(cboLogLevel.ItemIndex);
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
end.
