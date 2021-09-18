// Version 2.0
unit Form.Main;

interface

uses
  {Winapi}
  Winapi.ComBaseApi,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.MediaFoundationApi.MfObjects,
  Winapi.MediaFoundationApi.MfReadWrite,
  Winapi.ActiveX.ObjBase,
  Winapi.MediaFoundationApi.MfApi,
  Winapi.MediaFoundationApi.MfUtils,
  Winapi.D2D1,
  {System}
  System.TimeSpan,
  System.SysUtils,
  System.Variants,
  System.Classes,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Samples.Spin,
  Vcl.Menus,
  {Application}
  Support;

type
  TInternalTrackBar = class(TTrackbar)
  published
    property OnMouseDown;
    property OnMouseUp;
  end;

  TVideoFormatInfo = record
  public
    iVideoWidth : Integer;
    iVideoHeight : Integer;
    iBufferWidth : Integer;
    iBufferHeight : Integer;
    iStride : Integer;
    procedure Reset;
  end;

  TFrmMain = class(TForm)
    picFrame : TImage;
    pnlTop : TPanel;
    edtVideoFile : TEdit;
    btnBrowse : TButton;
    lblVideo : TLabel;
    memLog : TMemo;
    lblLog : TLabel;
    fdOpenVideo : TOpenDialog;
    btnClearLog : TButton;
    tbVideoPosition : TTrackbar;
    lblCurrentPosition : TLabel;
    btnClose : TButton;
    btnOpen : TButton;
    spAccuracy : TSpinEdit;
    lblMs : TLabel;
    lblAccuracy : TLabel;
    lblFramesToSkip : TLabel;
    spMaxSkipFrames : TSpinEdit;
    pnlFrameCapture : TPanel;
    chkReopen : TCheckBox;
    lblPosition : TLabel;
    cboMethod : TComboBox;
    lblMethod : TLabel;
    MainMenu1 : TMainMenu;
    File1 : TMenuItem;
    mnEdit : TMenuItem;
    mnLogLevel : TMenuItem;
    mnDebugLevel : TMenuItem;
    mnInfoLevel : TMenuItem;
    mnWarningLevel : TMenuItem;
    mnErrorLevel : TMenuItem;

    procedure HandleBrowseClick(Sender : TObject);
    procedure HandleClearLogClick(Sender : TObject);
    procedure HandleFormCreate(Sender : TObject);
    procedure HandleFormDestroy(Sender : TObject);
    procedure HandleTrackbarChange(Sender : TObject);
    procedure HandleCloseVideoClick(Sender : TObject);
    procedure HandleOpenClick(Sender : TObject);
    procedure HandleExitClick(Sender : TObject);
    procedure HandleLogLevelChange(Sender : TObject);

  private
    FSourceURL : string;
    FSourceReader : IMFSourceReader;
    FSupportsSeek : Boolean;
    FDuration : TTimeSpan;
    FFormatSettings : TFormatSettings;
    FVideoInfo : TVideoFormatInfo;
    FRenderTarget : ID2D1DCRenderTarget;
    F2DBitmapProperties : TD2D1BitmapProperties;
    FDPI : Integer;
    FLogLevel : TLogType;

    FFrequency : int64;
    FCaptureStart : int64;
    FRequestedFramePosition : Double;

    procedure OpenVideo(const AFilePath : string);
    procedure Log(const AText : string; ALogType : TLogType);
    procedure GetVideoFrame;
    procedure UpdateCapturePositionDisplay;

    procedure CloseSource;
    procedure FlushSource;
    procedure ResetVariables;
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure CreateDirect2DBitmapProperties;
    procedure HandleTrackbarMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure BeginBusy;
    procedure EndBusy;
    procedure UpdateLogLevelMenu;

    function CreateSourceReader(const AURL : string) : Boolean;
    function SelectVideoStream : Boolean;
    function UpdateCapabilities : Boolean;
    function CaptureFrame(APosition : TTimeSpan) : Boolean;
    function OpenSource(const AURL : string) : Boolean;
    function GetSourceOpen : Boolean;
    function GetDuration : TTimeSpan;
    function SetPosition(APosition : TTimeSpan) : Boolean;
    function GetMemoryUsed : string;

    function BitmapFromSample(const ASample : IMFSample; var AImage : TBitmap) : Boolean;
    function GetVideoFormat(AMediaTypeChanged : Boolean) : Boolean;

    function CreateRenderTarget : Boolean;
    procedure UpdateImage(const ASample : IMFSample);
    function SampleWithTolerance(ARequestedTime, AActualTime : TTimeSpan) : Boolean;

  public
    property SourceOpen : Boolean read GetSourceOpen;

  end;

var
  FrmMain : TFrmMain;

implementation

uses
  Winapi.ActiveX.PropIdl, Winapi.MediaFoundationApi.MfIdl, Winapi.ActiveX.PropVarUtil, Winapi.WinApiTypes, Winapi.DxgiFormat,
  System.IOUtils, System.Types, System.Math;

{$r *.dfm}

procedure TFrmMain.HandleFormCreate(Sender : TObject);
begin
  ResetVariables;

  FLogLevel := ltInfo;

  FFormatSettings := TFormatSettings.Create;

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION, 0)) then
  begin
    MessageBox(0, lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
      lpcwstr('MFStartup Failure!'), MB_ICONSTOP);
    Application.Terminate;
  end;

  FDPI := DevicePixelPerInch;

  CreateDirect2DBitmapProperties;

  if not CreateRenderTarget then
    Log('Failed to create render target', ltError);

  TInternalTrackBar(tbVideoPosition).OnMouseUp := HandleTrackbarMouseUp;

  UpdateLogLevelMenu;

  OpenVideo('C:\Temp\the-matrix-resurrections-trailer-1_h1080p.mov');
end;

procedure TFrmMain.HandleFormDestroy(Sender : TObject);
begin
  CloseSource;

  FRenderTarget.Flush();
  SafeRelease(FRenderTarget);

  MFShutdown();
  CoUnInitialize();
end;

procedure TFrmMain.OpenVideo(const AFilePath : string);
begin
  ClearImage;
  OpenSource(AFilePath);
end;

function TFrmMain.OpenSource(const AURL : string) : Boolean;
var
  oPreviousRounding : TRoundingMode;
begin
  Result := TFile.Exists(AURL);

  if Result then
  begin
    Log(Format('Opening video: "%s"', [AURL]), ltInfo);
    try
      CloseSource;

      Result := CreateSourceReader(AURL);

      if Result then
      begin
        Result := SelectVideoStream and GetVideoFormat(False);

        if Result then
        begin
          UpdateCapabilities;
          FDuration := GetDuration;
          FSourceURL := AURL;

          edtVideoFile.Text := FSourceURL;
          tbVideoPosition.Position := 0;

          oPreviousRounding := GetRoundMode;
          try
            SetRoundMode(rmDown);
            tbVideoPosition.Max := Round(FDuration.TotalSeconds);
          finally
            SetRoundMode(oPreviousRounding);
          end;

          UpdateCapturePositionDisplay;
        end;
      end;
    finally
      Log(Format('Opened video. Width: %d. Height: %d. Duration: %s. Supports Seek: %s', [FVideoInfo.iVideoWidth, FVideoInfo.iVideoHeight,
        TimeSpanToDisplay(FDuration), BoolToStr(FSupportsSeek, True)]), ltInfo);
    end;
  end
  else
    Log(Format('File does not exist: "%s"', [AURL]), ltError);

  UpdateEnabledStates;
end;

procedure TFrmMain.HandleLogLevelChange(Sender : TObject);
begin
  if Sender is TMenuItem then
  begin
    FLogLevel := TLogType(TMenuItem(Sender).Tag);
    UpdateLogLevelMenu;
  end;
end;

procedure TFrmMain.UpdateLogLevelMenu;
var
  i : Integer;
begin
  for i := 0 to mnLogLevel.Count - 1 do
    mnLogLevel.Items[i].Checked := Ord(FLogLevel) = mnLogLevel.Items[i].Tag;
end;

procedure TFrmMain.HandleTrackbarMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  GetVideoFrame;
end;

function TFrmMain.CreateRenderTarget : Boolean;
var
  pFactory : ID2D1Factory;
  oProperties : TD2D1RenderTargetProperties;
begin
  Result := SUCCEEDED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, IID_ID2D1Factory, nil, pFactory));
  try
    if Result then
    begin
      oProperties.&type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
      oProperties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties.PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      oProperties.dpiX := 0;
      oProperties.dpiY := 0;
      oProperties.usage := D2D1_RENDER_TARGET_USAGE_NONE;
      oProperties.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
      Result := SUCCEEDED(pFactory.CreateDCRenderTarget(oProperties, FRenderTarget));
    end;
  finally
    pFactory := nil;
  end;
end;

procedure TFrmMain.CreateDirect2DBitmapProperties;
var
  oPixelFormat : D2D1_PIXEL_FORMAT;
begin
  oPixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  oPixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
  F2DBitmapProperties.PixelFormat := oPixelFormat;
  F2DBitmapProperties.dpiX := FDPI;
  F2DBitmapProperties.dpiY := FDPI;
end;

procedure TFrmMain.HandleOpenClick(Sender : TObject);
begin
  ClearImage;
  CloseSource;
  OpenVideo(edtVideoFile.Text);
end;

procedure TFrmMain.HandleBrowseClick(Sender : TObject);
begin
  if fdOpenVideo.Execute then
    OpenVideo(fdOpenVideo.Filename);
end;

procedure TFrmMain.GetVideoFrame;
var
  oRequestedFramePosition : TTimeSpan;
  iPreviousPosition : Integer;
begin
  if SourceOpen then
  begin
    ClearImage;

    oRequestedFramePosition := TTimeSpan.Create(0, 0, tbVideoPosition.Position);
    FRequestedFramePosition := oRequestedFramePosition.TotalMilliseconds;

    SetPosition(oRequestedFramePosition);

    Log('Requesting image...', ltInfo);

    BeginBusy;
    try
      if CaptureFrame(oRequestedFramePosition) then
      begin
        if chkReopen.Checked then
        begin
          iPreviousPosition := tbVideoPosition.Position;
          if OpenSource(FSourceURL) then
            tbVideoPosition.Position := iPreviousPosition;
        end;
      end;
    finally
      EndBusy;
    end;
  end;
end;

function TFrmMain.GetVideoFormat(AMediaTypeChanged : Boolean) : Boolean;
var
  oSubType : TGUID;
  uHeight : UINT32;
  uWidth : UINT32;
  pInputType : IMFMediaType;
begin
  Result := SUCCEEDED(FSourceReader.GetCurrentMediaType(DWord(MF_SOURCE_READER_FIRST_VIDEO_STREAM), pInputType));
  try
    if Result and SUCCEEDED(pInputType.GetGUID(MF_MT_SUBTYPE, oSubType)) then
    begin
      // Make sure it is RGB 32
      if (oSubType = MFVideoFormat_RGB32) then
      begin
        Result := SUCCEEDED(MFGetAttributeSize(pInputType, MF_MT_FRAME_SIZE, uWidth, uHeight));
        if Result then
        begin
          FVideoInfo.iBufferWidth := uWidth;
          FVideoInfo.iBufferHeight := uHeight;

          // If the source type has changed the video buffer dimensions have changed.
          // We still want to use the original video dimensions for the full frame capture, not the buffer dimensions.
          if not AMediaTypeChanged then
          begin
            FVideoInfo.iVideoWidth := uWidth;
            FVideoInfo.iVideoHeight := uHeight;
          end;
        end;

        FVideoInfo.iStride := MFGetAttributeUINT32(pInputType, MF_MT_DEFAULT_STRIDE, 1);
      end
      else
        Log('GetVideoFormat. Video is not RGB 32 format', ltError);
    end;
  finally
    pInputType := nil;
  end;
end;

function TFrmMain.CreateSourceReader(const AURL : string) : Boolean;
var
  oAttributes : IMFAttributes;
begin
  // Configure the source reader to perform video processing.
  Result := SUCCEEDED(MFCreateAttributes(oAttributes, 1));
  try
    if Result then
    begin
      Result := SUCCEEDED(oAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING, 1));

      if Result then
        Result := SUCCEEDED(MFCreateSourceReaderFromURL(PWideChar(AURL), oAttributes, FSourceReader));
    end;
  finally
    oAttributes := nil;
  end;

  if not Result then
    Log('Failed to create source reader for frame capture', ltError);
end;

function TFrmMain.CaptureFrame(APosition : TTimeSpan) : Boolean;
var
  dwFlags : DWord;
  pSample : IMFSample;
  dSampleTimeStamp : LONGLONG;
  iSkippedFrames : Integer;
  tsSampleTime : TTimeSpan;
  bEndOfStream : Boolean;
  iCaptureEnd : int64;
  bReachedMaxFrames : Boolean;

begin
  iSkippedFrames := 0;
  dSampleTimeStamp := 0;
  Result := False;
  bEndOfStream := False;
  bReachedMaxFrames := False;

  dwFlags := 0;

  QueryPerformanceFrequency(FFrequency);
  QueryPerformanceCounter(FCaptureStart);

  FSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                   True);

  while not Result and not bEndOfStream and (iSkippedFrames < spMaxSkipFrames.Value) and
    SUCCEEDED(FSourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0, nil, @dwFlags, nil, @pSample)) do
  begin
    bEndOfStream := (dwFlags = MF_SOURCE_READERF_ENDOFSTREAM);

    if (dwFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
    begin
      // Type change. Get the new format.
      Log('Media Format has changed, getting new format', ltInfo);
      GetVideoFormat(True);
    end
    else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
    begin
      tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
      Result := bEndOfStream or SampleWithTolerance(APosition, tsSampleTime);

      if Result then
      begin
        QueryPerformanceCounter(iCaptureEnd);
        Log(Format('Image found in %f milliseconds. Time Stamp: %s. Frames Skipped: %d', [(iCaptureEnd - FCaptureStart) / FFrequency * 1000,
          TimeSpanToDisplay(tsSampleTime, True), iSkippedFrames]), ltInfo);

        UpdateImage(pSample);
      end
      else
      begin
        inc(iSkippedFrames);
        Log(Format('Skipped frame. Not within requested accuracy. Sample time stamp: %s. Frames Skipped: %d',
          [TimeSpanToDisplay(tsSampleTime, True), iSkippedFrames]), ltDebug);
      end;
    end;

    bReachedMaxFrames := (iSkippedFrames = spMaxSkipFrames.Value);

    if not bReachedMaxFrames then
      SafeRelease(pSample);

  end;

  if not Result then
  begin
    if bReachedMaxFrames and Assigned(pSample) then
    begin
      Log(Format('Reached maximum frames to skip %d. Using last frame returned at: %s',
        [iSkippedFrames, TimeSpanToDisplay(tsSampleTime, True)]), ltWarning);
      UpdateImage(pSample);
    end
    else
      Log(Format('Frame not found. Frames Skipped: %d', [iSkippedFrames]), ltWarning);
  end;

  SafeRelease(pSample);
end;

function TFrmMain.SampleWithTolerance(ARequestedTime : TTimeSpan; AActualTime : TTimeSpan) : Boolean;
begin
  Result := CompareValue(ARequestedTime.TotalMilliseconds, AActualTime.TotalMilliseconds, spAccuracy.Value) = EqualsValue;
end;

procedure TFrmMain.UpdateImage(const ASample : IMFSample);
var
  oBitmap : TBitmap;
begin
  oBitmap := TBitmap.Create({FVideoInfo.iVideoWidth, FVideoInfo.iVideoHeight});
  // Compatible with Delphi versions <= 10.3.3
  oBitmap.Width := FVideoInfo.iVideoWidth;
  oBitmap.Height := FVideoInfo.iVideoHeight;
  try
    if BitmapFromSample(ASample, oBitmap) then
    begin
      pnlFrameCapture.Caption := '';
      picFrame.Picture.Bitmap.Assign(oBitmap);
    end
    else
      Log('Failed to create BMP from frame sample', ltError);
  finally
    FreeAndNil(oBitmap);
  end;
end;

function TFrmMain.BitmapFromSample(const ASample : IMFSample; var AImage : TBitmap) : Boolean;
var
  pBuffer : IMFMediaBuffer;
  pBitmapData : PByte;
  cbBitmapData : DWord;
  o2DBitmap : ID2D1Bitmap;
  oSize : D2D1_SIZE_U;
  iPitch : Integer;
  iActualBMPDataSize : Integer;
  iExpectedBMPDataSize : Integer;

begin

  Result := SUCCEEDED(ASample.ConvertToContiguousBuffer(pBuffer));

  try
    if Result then
    begin
      Result := SUCCEEDED(pBuffer.Lock(pBitmapData, nil, @cbBitmapData));
      try
        iPitch := 4 * FVideoInfo.iVideoWidth;

        // For full frame capture, use the buffer dimensions for the data size check
        iExpectedBMPDataSize := (FVideoInfo.iBufferWidth * 4) * FVideoInfo.iBufferHeight;

        iActualBMPDataSize := Integer(cbBitmapData);

        if iActualBMPDataSize <> iExpectedBMPDataSize then
          Log(Format('Sample size does not match expected size. Current: %d. Expected: %d', [iActualBMPDataSize, iExpectedBMPDataSize]
            ), ltError);

        if Result then
        begin
          // Bind the render target to the bitmap
          Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle, AImage.Canvas.ClipRect));

          if Result then
          begin
            // Create the 2D bitmap interface
            oSize.Width := FVideoInfo.iVideoWidth;
            oSize.Height := FVideoInfo.iVideoHeight;
            Result := SUCCEEDED(FRenderTarget.CreateBitmap(oSize, pBitmapData, iPitch, F2DBitmapProperties, o2DBitmap));
          end;

          // Draw the 2D bitmap to the render target
          if Result then
          begin
            try
              FRenderTarget.BeginDraw;
              try
                FRenderTarget.DrawBitmap(o2DBitmap);
              finally
                FRenderTarget.EndDraw();
              end;
            finally
              SafeRelease(o2DBitmap);
            end;
          end;
        end;
      finally
        pBuffer.Unlock;
        SafeRelease(pBuffer);
      end;
    end;
  finally
    pBitmapData := Nil;
    SafeRelease(pBuffer);
  end;
end;

function TFrmMain.SelectVideoStream : Boolean;
var
  pMediaType : IMFMediaType;

begin

  // Configure the source reader to give us progressive RGB32 frames.
  Result := SUCCEEDED(MFCreateMediaType(pMediaType));

  if Result then
    Result := SUCCEEDED(pMediaType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Video));

  if Result then
    Result := SUCCEEDED(pMediaType.SetGUID(MF_MT_SUBTYPE, MFVideoFormat_RGB32));

  if Result then
    Result := SUCCEEDED(FSourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0, pMediaType));

  // Select the first video stream
  if Result then
    Result := SUCCEEDED(FSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM, True));

  if not Result then
    Log('SelectVideoStream failed', ltError);
end;

function TFrmMain.UpdateCapabilities : Boolean;
var
  oFlags : DWord;
  oPropVar : PROPVARIANT;
begin
  FSupportsSeek := False;
  Result := SourceOpen;

  if Result then
  begin
    PropVariantInit(oPropVar);
    try
      Result := SUCCEEDED(FSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
        MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS, oPropVar));

      if Result then
      begin
        Result := SUCCEEDED(PropVariantToUInt32(oPropVar, oFlags));

        if Result then
          FSupportsSeek := (oFlags and MFMEDIASOURCE_CAN_SEEK) = MFMEDIASOURCE_CAN_SEEK;
      end
      else
        Log('GetPresentationAttribute failed', ltError);
    finally
      PropVariantClear(oPropVar);
    end;
  end;
end;

function TFrmMain.GetDuration : TTimeSpan;
var
  oPropVar : PROPVARIANT;
begin
  Result := TTimeSpan.Zero;

  if SourceOpen then
  begin
    PropVariantInit(oPropVar);
    try
      if SUCCEEDED(FSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE, MF_PD_DURATION, oPropVar)) and
        (oPropVar.vt = VT_UI8) then
        Result := TTimeSpan.Create(oPropVar.hVal.QuadPart)
      else
        Log('GetDuration failed', ltError);
    finally
      PropVariantClear(oPropVar);
    end;
  end;
end;

procedure TFrmMain.FlushSource;
var
  hr: HResult;

begin
  if SourceOpen then
  begin
    Log('Flush - Begin', ltInfo);

    hr := FSourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);

    if SUCCEEDED(hr) then
      Log('Flush - End', ltInfo)
    else
      Log('Failed to flush source', ltError)
  end;
end;

function TFrmMain.SetPosition(APosition : TTimeSpan) : Boolean;
var
  oStartPropVar : PROPVARIANT;
begin
  Result := SourceOpen;

  if Result then
  begin
    Log(Format('Setting source position: %s', [TimeSpanToDisplay(APosition, True)]), ltInfo);

    PropVariantInit(oStartPropVar);
    try
      oStartPropVar.vt := VT_I8;
      oStartPropVar.hVal.QuadPart := APosition.Ticks;
      Result := SUCCEEDED(FSourceReader.SetCurrentPosition(GUID_NULL, oStartPropVar));
    finally
      PropVariantClear(oStartPropVar);
    end;

    if not Result then
      Log('Failed to set position', ltError);
  end;
end;

procedure TFrmMain.BeginBusy;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TFrmMain.EndBusy;
begin
  Screen.Cursor := crDefault;
end;

procedure TFrmMain.HandleClearLogClick(Sender : TObject);
begin
  memLog.Lines.Clear;
end;

procedure TFrmMain.HandleCloseVideoClick(Sender : TObject);
begin
  CloseSource;
  ClearImage;
end;

procedure TFrmMain.HandleExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TFrmMain.ResetVariables;
begin
  SafeRelease(FSourceReader);
  FSupportsSeek := False;
  FFrequency := 0;
  FCaptureStart := 0;
  FRequestedFramePosition := 0;
  FVideoInfo.Reset;
end;

procedure TFrmMain.HandleTrackbarChange(Sender : TObject);
begin
  UpdateCapturePositionDisplay;
end;

procedure TFrmMain.UpdateCapturePositionDisplay;
begin
  lblCurrentPosition.Caption := TimeSpanToDisplay(TTimeSpan.Create(0, 0, tbVideoPosition.Position)) + ' / ' + TimeSpanToDisplay(FDuration);
end;

procedure TFrmMain.Log(const AText : string; ALogType : TLogType);
begin
  if ALogType >= FLogLevel then
    memLog.Lines.Add(FormatDateTime('yyyy/mm/dd HH:mm:ss.zzz', Now, FFormatSettings) + cTab + ALogType.AsDisplay + cTab + 'Memory Used: ' +
      GetMemoryUsed + cTab + AText);
end;

function TFrmMain.GetMemoryUsed : string;
begin
  Result := ToSize(Round(ProcessMemoryUsage / 1024));
end;

function TFrmMain.GetSourceOpen : Boolean;
begin
  Result := Assigned(FSourceReader);
end;

procedure TFrmMain.CloseSource;
begin
  FlushSource;
  if Assigned(FSourceReader) then
  begin
    Log('Destroy source reader - Begin', ltInfo);
    SafeRelease(FSourceReader);
    Log('Destroy source reader - End', ltInfo);
  end;

  UpdateEnabledStates;
end;

procedure TFrmMain.UpdateEnabledStates;
begin
  tbVideoPosition.Enabled := SourceOpen and FSupportsSeek;

  if not SourceOpen then
  begin
    lblCurrentPosition.Caption := 'N/A';
    lblCurrentPosition.Invalidate;
    tbVideoPosition.Min := 1;
    tbVideoPosition.Max := 1;
    tbVideoPosition.Position := 1;
  end;

  btnClose.Enabled := SourceOpen;
  btnOpen.Enabled := not SourceOpen;
end;

procedure TFrmMain.ClearImage;
begin
  pnlFrameCapture.Caption := 'No Image. Waiting frame capture...';
  picFrame.Picture := nil;
end;

{ TVideoFormatInfo }

procedure TVideoFormatInfo.Reset;
begin
  iVideoWidth := 0;
  iVideoHeight := 0;
  iBufferWidth := 0;
  iBufferHeight := 0;
  iStride := 0;
end;

end.
