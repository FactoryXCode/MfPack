unit FileCapture.Asynchronous;

interface

uses
  {Winapi}
  WinAPI.Messages, //
  WinAPI.Windows, //
  WinAPI.MediaFoundationApi.MfReadWrite, //
  WinAPI.MediaFoundationApi.MfObjects, //
  {System}
  System.TimeSpan, //
  {Application}
  FileCapture, //
  MessageHandler, //
  Support;

const
  WM_FLUSH_COMPLETE = WM_USER + 1001;
  WM_MEDIA_FORMAT_CHANGED = WM_USER + 1002;
  WM_SAMPLE_FOUND = WM_USER + 1003;

type
  TPositionRequest = record
    bPending : Boolean;
    bRequestNextSample : Boolean;
    oPosition : TTimeSpan;

    procedure Reset;
    class function New(APosition : TTimeSpan; ARequestSample : Boolean) : TPositionRequest; static;
  end;

  PSampleReply = ^TSampleReply;

  TSampleReply = record
    oSampleTimeStamp : TTimeSpan;
    oSample : IMFSample;
  end;

  TFileCaptureAsync = class(TFileCapture, IMFSourceReaderCallback)
  private
    FMessageHandler : TMessageHandler;
    FPositionRequest : TPositionRequest;
    FCritSec : TMFCritSec;
    FFindingSample : Boolean;

    procedure HandleMessages(var AMessage : TMessage; var AHandled : Boolean);
    procedure NotifyMediaFormatChanged;
    procedure HandleSampleFoundMessage(var AMessage : TMessage);

    function ReadNextSample : Boolean;

    {$region 'IMFSourceReaderCallback methods'}
    function OnReadSample(hrStatus : HRESULT; dwStreamIndex : DWord; dwStreamFlags : DWord; llTimestamp : LONGLONG; pSample : IMFSample)
      : HRESULT; stdcall;
    function OnFlush(dwStreamIndex : DWord) : HRESULT; stdcall;
    function OnEvent(dwStreamIndex : DWord; pEvent : IMFMediaEvent) : HRESULT; stdcall;
    procedure ProcessPendingRequest;
    {$endregion}
  protected
    procedure ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan); override;
    procedure ConfigureSourceReader(const AAttributes : IMFAttributes); override;
    procedure HandleMediaFormatChanged; override;
    procedure ResetVariables; override;
    procedure HandleFlushComplete; override;
    procedure Flush; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RequestFrame(APosition : TTimeSpan); override;

    property FindingSample : Boolean read FFindingSample;
  end;

implementation

uses
  {WinApi}
  WinAPI.MediaFoundationApi.MfUtils, //
  {System}
  System.SysUtils;

{ TFileCaptureAsync }

constructor TFileCaptureAsync.Create;
begin
  inherited;
  FCritSec := TMFCritSec.Create;

  FMessageHandler := TMessageHandler.Create;
  FMessageHandler.OnMessage := HandleMessages;

  FPositionRequest.Reset;
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

procedure TFileCaptureAsync.HandleFlushComplete;
begin
  inherited;
  FFindingSample := False;

  if SourceOpen and FPositionRequest.bPending then
    ProcessPendingRequest;
end;

procedure TFileCaptureAsync.ProcessPendingRequest;
begin
  RequestFrame(FPositionRequest.oPosition);
  FPositionRequest.bPending := False;
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
  else if AMessage.Msg = WM_SAMPLE_FOUND then
  begin
    AHandled := True;
    HandleSampleFoundMessage(AMessage);
  end
  else
    AHandled := False;
end;

procedure TFileCaptureAsync.HandleSampleFoundMessage(var AMessage : TMessage);
var
  oSampleReply : PSampleReply;
begin
  oSampleReply := PSampleReply(AMessage.LPARAM);
  try
    try
      ReturnSample(oSampleReply.oSample, oSampleReply.oSampleTimeStamp);
    finally
      SafeRelease(oSampleReply.oSample);
    end;
  finally
    Dispose(oSampleReply);
  end;
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

  if FindingSample then
  begin
    // We need to flush before changing position if a sample request is in progress.
    FPositionRequest := TPositionRequest.New(APosition, True);

    if not AwaitingFlush then
      Flush;
  end
  else
    ReadNextSample;
end;

procedure TFileCaptureAsync.ResetVariables;
begin
  inherited;
  FFindingSample := False;
  FPositionRequest.Reset;
end;

function TFileCaptureAsync.ReadNextSample : Boolean;
var
  oResult : HRESULT;
begin
  Result := Assigned(SourceReader);

  if Result then
  begin
    FFindingSample := True;
    oResult := SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0);
    Result := SUCCEEDED(oResult);
    if not Result then
      HandleSampleReadError(oResult);
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
  // Note: This will be called in a worker thread.
  Result := S_OK;
end;

function TFileCaptureAsync.OnFlush(dwStreamIndex : DWord) : HRESULT;
begin
  // Note: This will be called in a worker thread.
  Result := S_OK;
  PostMessage(FMessageHandler.Handle, WM_FLUSH_COMPLETE, 0, 0);
  HandleThreadMessages(GetCurrentThread());
end;

procedure TFileCaptureAsync.NotifyMediaFormatChanged;
begin
  // Note: This will be called in a worker thread.
  PostMessage(FMessageHandler.Handle, WM_MEDIA_FORMAT_CHANGED, 0, 0);
  HandleThreadMessages(GetCurrentThread());
end;

procedure TFileCaptureAsync.ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan);
var
  oSampleReply : PSampleReply;
begin
  // Note: This will be called in a worker thread.

  // _AddRef will be called, so the sample will not be released until the message is handled.
  New(oSampleReply);
  oSampleReply.oSample := ASample;
  oSampleReply.oSampleTimeStamp := ATimeStamp;

  if not PostMessage(FMessageHandler.Handle, WM_SAMPLE_FOUND, 0, LPARAM(oSampleReply)) then
  begin
    SafeRelease(oSampleReply.oSample);
    Dispose(oSampleReply);
  end;

  HandleThreadMessages(GetCurrentThread());
end;

function TFileCaptureAsync.OnReadSample(hrStatus : HRESULT; dwStreamIndex, dwStreamFlags : DWord; llTimestamp : LONGLONG;
  pSample : IMFSample) : HRESULT;
var
  bEndOfStream : Boolean;
  dSampleTimeStamp : LONGLONG;
  tsSampleTime : TTimeSpan;
  bReachedMaxFrames : Boolean;
begin
  // Note: This will be called in a worker thread. Be careful of accessing anything outside of this method.
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
          bReachedMaxFrames := (FramesSkipped >= MaxFramesToSkip);

          tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
          if bEndOfStream or SampleWithinTolerance(RequestedTime, tsSampleTime) or bReachedMaxFrames then
          begin
            if bReachedMaxFrames then
              Log(Format('Reached maximum frames to skip %d. Using last frame returned at: %s',
                [FramesSkipped, TimeSpanToDisplay(tsSampleTime, True)]), ltWarning);

            ProcessSample(pSample, tsSampleTime);

            FFindingSample := False;
          end
          else
          begin
            // Read the next sample
            if (FramesSkipped < MaxFramesToSkip) then
            begin
              ReadNextSample;
              HandleFrameSkipped;
            end;
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

{ TPositionRequest }

class function TPositionRequest.New(APosition : TTimeSpan; ARequestSample : Boolean) : TPositionRequest;
begin
  Result.oPosition := APosition;
  Result.bRequestNextSample := ARequestSample;
  Result.bPending := True;
end;

procedure TPositionRequest.Reset;
begin
  oPosition := TTimeSpan.Zero;
  bRequestNextSample := False;
  bPending := False;
end;

end.
