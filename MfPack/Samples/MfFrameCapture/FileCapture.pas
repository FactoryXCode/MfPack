unit FileCapture;

interface

uses
  {Winapi}
  WinAPI.Messages,
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.Windows,
  WinAPI.MediaFoundationApi.MfApi,
  WinAPI.MediaFoundationApi.MfObjects,
  WinAPI.MediaFoundationApi.MfUtils,
  {System}
  System.Classes,
  System.TimeSpan,Support,
  {Application}
  SampleConverter;

type
  TFileCapture = class(TInterfacedPersistent)
  private
    FAccuracy : Double;
    FMaxFramesToSkip : Integer;
    FSourceReader : IMFSourceReader;
    FOnFlushComplete : TNotifyEvent;
    FOnLog : TLogEvent;
    FAwaitingFlush : Boolean;
    FRequestedTime : TTimeSpan;
    FFramesSkipped : Integer;
    FDuration : TTimeSpan;
    FURL : string;
    FVideoInfo : TVideoFormatInfo;
    FSupportsSeek : Boolean;
    FOnFrameFound : TFrameEvent;
    FSampleConverter : TSampleConverter;

    procedure SetAccuracy(const AValue : Double);
    procedure SetMaxFramesToSkip(const AValue : Integer);
    procedure ResetVariables;

    function GetAccuracy : Double;
    function GetMaxFramesToSkip : Integer;
    function GetSourceOpen : Boolean;
    function CreateSourceReader(const AURL : string) : Boolean;
    function SelectVideoStream : Boolean;
    function GetVideoFormat(AMediaTypeChanged : Boolean) : Boolean;
    function UpdateCapabilities : Boolean;
    function GetDuration : TTimeSpan;
  protected
    procedure HandleFlushComplete;
    procedure HandleFrameSkipped;
    procedure ResetFramesSkipped;

    procedure Flush; virtual;
    procedure HandleMediaFormatChanged; virtual;

    procedure ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan); virtual; abstract;

    procedure Log(const AMessage : string; const AType : TLogType);
    procedure ConfigureSourceReader(const AAttributes : IMFAttributes); virtual;

    property SourceReader : IMFSourceReader read FSourceReader;
    property SampleConverter : TSampleConverter read FSampleConverter;

    function SampleWithinTolerance(ARequestedTime, AActualTime : TTimeSpan) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Accuracy : Double read GetAccuracy write SetAccuracy;
    property MaxFramesToSkip : Integer read GetMaxFramesToSkip write SetMaxFramesToSkip;

    procedure RequestFrame(APosition : TTimeSpan); virtual;

    function OpenSource(const AURL : string) : Boolean;
    function SetPosition(APosition : TTimeSpan) : Boolean;

    procedure CloseSource;

    // Event hooks
    property OnLog : TLogEvent read FOnLog write FOnLog;
    property OnFlushComplete : TNotifyEvent read FOnFlushComplete write FOnFlushComplete;
    property OnFrameFound : TFrameEvent read FOnFrameFound write FOnFrameFound;

    property RequestedTime : TTimeSpan read FRequestedTime;
    property FramesSkipped : Integer read FFramesSkipped;

    property Duration : TTimeSpan read FDuration;
    property SourceOpen : Boolean read GetSourceOpen;
    property URL : string read FURL;
    property VideoInfo : TVideoFormatInfo read FVideoInfo;
    property SupportsSeek : Boolean read FSupportsSeek;
  end;

implementation

uses
  WinAPI.ActiveX.PropIdl, WinAPI.WinApiTypes, WinAPI.MediaFoundationApi.MfIdl, WinAPI.ActiveX.PropVarUtil, System.Math, System.SysUtils,
  System.IOUtils, System.Types;

{ TFileCapture }

constructor TFileCapture.Create;
begin
  ResetVariables;

  FSampleConverter := TSampleConverter.Create;
end;

destructor TFileCapture.Destroy;
begin
  FSampleConverter.Free;
  FSampleConverter := nil;
  inherited;
end;

procedure TFileCapture.CloseSource;
begin
  if SourceOpen then
  begin
    Flush;
    Log('Destroy source reader - Begin', ltInfo);
    SafeRelease(FSourceReader);
    Log('Destroy source reader - End', ltInfo);
  end;

  ResetVariables;
end;

procedure TFileCapture.ResetVariables;
begin
  FVideoInfo.Reset;
  FAwaitingFlush := False;
  FRequestedTime := TTimeSpan.Zero;
  FFramesSkipped := 0;
  FDuration := TTimeSpan.Zero;
  FURL := '';
  FSupportsSeek := False;
end;

procedure TFileCapture.Flush;
begin
  FAwaitingFlush := True;
end;

function TFileCapture.GetAccuracy : Double;
begin
  Result := FAccuracy;
end;

procedure TFileCapture.RequestFrame(APosition : TTimeSpan);
begin
  FFramesSkipped := 0;
  FRequestedTime := APosition;
end;

procedure TFileCapture.SetAccuracy(const AValue : Double);
begin
  FAccuracy := AValue;
end;

function TFileCapture.GetMaxFramesToSkip : Integer;
begin
  Result := FMaxFramesToSkip;
end;

function TFileCapture.GetSourceOpen : Boolean;
begin
  Result := Assigned(FSourceReader);
end;

procedure TFileCapture.HandleFlushComplete;
begin
  FAwaitingFlush := False;

  if Assigned(FOnFlushComplete) then
    FOnFlushComplete(Self);
end;

procedure TFileCapture.HandleFrameSkipped;
begin
  inc(FFramesSkipped);
end;

procedure TFileCapture.HandleMediaFormatChanged;
begin
  GetVideoFormat(True);
  FSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM, True);
end;

procedure TFileCapture.Log(const AMessage : string; const AType : TLogType);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage, AType);
end;

procedure TFileCapture.ConfigureSourceReader(const AAttributes : IMFAttributes);
begin
  //
end;

function TFileCapture.OpenSource(const AURL : string) : Boolean;
begin
  CloseSource;
  FURL := '';

  Result := IsURL(AURL) or TFile.Exists(AURL);

  if Result then
  begin
    Result := CreateSourceReader(AURL);

    if Result then
    begin
      Result := SelectVideoStream and GetVideoFormat(False);

      if Result then
      begin
        UpdateCapabilities;
        FDuration := GetDuration;
        FURL := AURL;
      end;
    end;
  end
  else
    Log(Format('File does not exist: "%s"', [AURL]), ltError);
end;

function TFileCapture.GetVideoFormat(AMediaTypeChanged : Boolean) : Boolean;
var
  oSubType : TGUID;
  uHeight : UINT32;
  uWidth : UINT32;
  pInputType : IMFMediaType;
begin
  Result := SUCCEEDED(FSourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM, pInputType));
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

function TFileCapture.SelectVideoStream : Boolean;
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

function TFileCapture.CreateSourceReader(const AURL : string) : Boolean;
var
  oAttributes : IMFAttributes;
begin
  // Configure the source reader to perform video processing.
  Result := SUCCEEDED(MFCreateAttributes(oAttributes, 1));
  try
    if Result then
    begin
      Result := SUCCEEDED(oAttributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING, 1));

      ConfigureSourceReader(oAttributes);

      if Result then
        Result := SUCCEEDED(MFCreateSourceReaderFromURL(PWideChar(AURL), oAttributes, FSourceReader));
    end;
  finally
    oAttributes := nil;
  end;
  if not Result then
    Log('Failed to create source reader for frame capture', ltError);
end;

function TFileCapture.UpdateCapabilities : Boolean;
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
      Result := SUCCEEDED(FSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE, MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS,
        oPropVar));

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

function TFileCapture.GetDuration : TTimeSpan;
var
  oPropVar : PROPVARIANT;
begin
  Result := TTimeSpan.Zero;

  if SourceOpen then
  begin
    PropVariantInit(oPropVar);
    try
      if SUCCEEDED(FSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE, MF_PD_DURATION, oPropVar)) and (oPropVar.vt = VT_UI8)
      then
        Result := TTimeSpan.Create(oPropVar.hVal.QuadPart)
      else
        Log('GetDuration failed', ltError);
    finally
      PropVariantClear(oPropVar);
    end;
  end;
end;

function TFileCapture.SetPosition(APosition : TTimeSpan) : Boolean;
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

function TFileCapture.SampleWithinTolerance(ARequestedTime : TTimeSpan; AActualTime : TTimeSpan) : Boolean;
begin
  Result := CompareValue(ARequestedTime.TotalMilliseconds, AActualTime.TotalMilliseconds, Accuracy) = EqualsValue;
end;

procedure TFileCapture.ResetFramesSkipped;
begin
  FMaxFramesToSkip := 0;
end;

procedure TFileCapture.SetMaxFramesToSkip(const AValue : Integer);
begin
  FMaxFramesToSkip := AValue;
end;

end.
