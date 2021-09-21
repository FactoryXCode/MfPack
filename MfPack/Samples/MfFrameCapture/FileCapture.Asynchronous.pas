unit FileCapture.Asynchronous;

interface

uses
  {Winapi}
  WinAPI.Messages,
  WinAPI.Windows,
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfObjects,
  {System}
  System.TimeSpan,
  {Application}
  FileCapture,
  MessageHandler,
  Support;

const
  WM_FLUSH_COMPLETE = WM_USER + 1001;
  WM_MEDIA_FORMAT_CHANGED = WM_USER + 1002;

type
  TFileCaptureAsync = class(TFileCapture, IMFSourceReaderCallback)
  private
    FMessageHandler : TMessageHandler;
    FCritSec : TMFCritSec;

    procedure HandleMessages(var AMessage : TMessage; var AHandled : Boolean);
    procedure NotifyMediaFormatChanged;

    {$region 'IMFSourceReaderCallback methods'}
    function OnReadSample(hrStatus : HRESULT; dwStreamIndex : DWord; dwStreamFlags : DWord; llTimestamp : LONGLONG; pSample : IMFSample)
      : HRESULT; stdcall;
    function OnFlush(dwStreamIndex : DWord) : HRESULT; stdcall;
    function OnEvent(dwStreamIndex : DWord; pEvent : IMFMediaEvent) : HRESULT; stdcall;
    {$endregion}
  protected
    procedure ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan); override;
    procedure ConfigureSourceReader(const AAttributes : IMFAttributes); override;
    procedure HandleMediaFormatChanged; override;
    procedure Flush; override;
    function ReadNextSample : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RequestFrame(APosition : TTimeSpan); override;
  end;

implementation

uses
  WinAPI.MediaFoundationApi.MfError, WinAPI.MediaFoundationApi.MfUtils;

{ TFileCaptureAsync }

constructor TFileCaptureAsync.Create;
begin
  FCritSec := TMFCritSec.Create;

  FMessageHandler := TMessageHandler.Create;
  FMessageHandler.OnMessage := HandleMessages;
end;

destructor TFileCaptureAsync.Destroy;
begin
  FMessageHandler.RemoveHandle;
  FMessageHandler.Free;
  FMessageHandler := nil;

  FCritSec.Free;
  FCritSec := nil;
  inherited;
end;

procedure TFileCaptureAsync.HandleMediaFormatChanged;
begin
  inherited;
  ReadNextSample;
end;

procedure TFileCaptureAsync.HandleMessages(var AMessage : TMessage; var AHandled : Boolean);
begin
  if AMessage.Msg = WM_FLUSH_COMPLETE then
  begin
    AHandled := True;
    HandleFlushComplete;
  end
  else if AMessage.Msg = WM_MEDIA_FORMAT_CHANGED then
  begin
    AHandled := True;
    HandleMediaFormatChanged;
  end
  else
    AHandled := False;
end;

procedure TFileCaptureAsync.Flush;
begin
  inherited;
  Log('Flush - Begin', ltInfo);
  SourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
end;

procedure TFileCaptureAsync.RequestFrame(APosition : TTimeSpan);
begin
  inherited;
  ReadNextSample;
end;

function TFileCaptureAsync.ReadNextSample : Boolean;
var
  oResult : HRESULT;
  sMessage : string;
begin
  Result := Assigned(SourceReader);

  if Result then
  begin
    oResult := SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0);
    Result := SUCCEEDED(oResult);
    if not Result then
    begin
      case oResult of
        MF_E_INVALIDREQUEST :
          sMessage := 'Invalid request';
        MF_E_INVALIDSTREAMNUMBER :
          sMessage := 'The dwStreamIndex parameter is invalid.';
        MF_E_NOTACCEPTING :
          sMessage := 'A flush operation is pending';
        E_INVALIDARG :
          sMessage := 'Invalid argument.';
      else
        sMessage := 'Unknown error';
      end;

      Log('ReadSample call failed: ' + sMessage, ltError);
    end;
  end;
end;

procedure TFileCaptureAsync.ConfigureSourceReader(const AAttributes : IMFAttributes);
var
  oResult : HRESULT;
begin
  oResult := AAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK, Self);
  if not SUCCEEDED(oResult) then
    Log('Failed to configure source reader callback', ltError);
end;

function TFileCaptureAsync.OnEvent(dwStreamIndex : DWord; pEvent : IMFMediaEvent) : HRESULT;
begin
  Result := S_OK;
end;

function TFileCaptureAsync.OnFlush(dwStreamIndex : DWord) : HRESULT;
begin
  Result := S_OK;
  PostMessage(FMessageHandler.Handle, WM_FLUSH_COMPLETE, 0, 0);
  HandleThreadMessages(GetCurrentThread());
end;

procedure TFileCaptureAsync.NotifyMediaFormatChanged;
begin
  PostMessage(FMessageHandler.Handle, WM_MEDIA_FORMAT_CHANGED, 0, 0);
  HandleThreadMessages(GetCurrentThread());
end;

procedure TFileCaptureAsync.ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan);
begin
  {$message 'TODO: Handle sample conversion to BMP, or provide sample to main thread for conversion'}
end;

function TFileCaptureAsync.OnReadSample(hrStatus : HRESULT; dwStreamIndex, dwStreamFlags : DWord; llTimestamp : LONGLONG;
  pSample : IMFSample) : HRESULT;
var
  bEndOfStream : Boolean;
  dSampleTimeStamp : LONGLONG;
  tsSampleTime : TTimeSpan;
begin
  // Note: This will be called in a worker thread.
  // Be careful of accessing anything outside of this method.
  Result := S_OK;

  try
    FCritSec.Lock;
    try
      if SUCCEEDED(hrStatus) then
      begin
        bEndOfStream := (dwStreamFlags = MF_SOURCE_READERF_ENDOFSTREAM);

        if (dwStreamFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
          NotifyMediaFormatChanged
        else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
        begin
          tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
          if bEndOfStream or SampleWithinTolerance(RequestedTime, tsSampleTime) then
            ProcessSample(pSample, tsSampleTime)
          else
          begin
            HandleFrameSkipped;
            // Read the next sample
            if Assigned(SourceReader) and (FramesSkipped < MaxFramesToSkip) then
              SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0);
          end;
        end;
      end;
    finally
      FCritSec.Unlock;
    end;
  finally
    SafeRelease(pSample);
  end;
end;

end.
