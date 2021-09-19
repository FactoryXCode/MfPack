unit SourceReaderCallback;

interface

uses
  {Winapi}
  WinAPI.Messages,
  WinAPI.Windows,
  WinAPI.MediaFoundationApi.MfReadWrite,
  WinAPI.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {System}
  System.Classes,
  System.TimeSpan,
  {Application}
  Support;

const
  WM_FLUSH_COMPLETE = WM_USER + 1001;
  WM_MEDIA_FORMAT_CHANGED = WM_USER + 1002;

type

  TSourceReaderCallback = class(TInterfacedPersistent, IMFSourceReaderCallback)
  private
    FRequestedTime : TTimeSpan;
    FFramesSkipped : Integer;
    FAllowableOffset : Double;
    FMaxFramesToSkip : Integer;
    FSourceReader : IMFSourceReader;
    FCaller : HWnd;
    FCritSec : TMFCritSec;
    procedure NotifyMediaFormatChanged;
    function SampleWithTolerance(ARequestedTime, AActualTime : TTimeSpan) : Boolean;
    procedure ProcessSample(const ASample : IMFSample);
  protected
    {$region 'IMFSourceReaderCallback methods'}
    function OnReadSample(hrStatus : HRESULT; dwStreamIndex : DWord; dwStreamFlags : DWord; llTimestamp : LONGLONG; pSample : IMFSample)
      : HRESULT; stdcall;
    function OnFlush(dwStreamIndex : DWord) : HRESULT; stdcall;
    function OnEvent(dwStreamIndex : DWord; pEvent : IMFMediaEvent) : HRESULT; stdcall;
    {$endregion}
  public
    constructor Create(); overload;
    constructor Create(const ACaller : HWnd); overload;
    destructor Destroy; override;

    property RequestedTime : TTimeSpan read FRequestedTime write FRequestedTime;
    property AllowableOffset : Double read FAllowableOffset write FAllowableOffset;
    property MaxFramesToSkip : Integer read FMaxFramesToSkip write FMaxFramesToSkip;

    property SourceReader : IMFSourceReader read FSourceReader write FSourceReader;

    procedure ResetFramesSkipped;
  end;

implementation

uses
  System.Math, System.SysUtils, System.Types;

{ TSourceReaderCallback }

constructor TSourceReaderCallback.Create();
begin
  FCritSec := TMFCritSec.Create;
  FRequestedTime := TTimeSpan.Zero;
  FAllowableOffset := 2000;
  FMaxFramesToSkip := 120;

  ResetFramesSkipped;
end;

constructor TSourceReaderCallback.Create(const ACaller : HWnd);
begin
  Create;
  FCaller := ACaller;
end;

destructor TSourceReaderCallback.Destroy;
begin
  FCritSec.Free;
  FCritSec := nil;
  inherited;
end;

function TSourceReaderCallback.OnEvent(dwStreamIndex : DWord; pEvent : IMFMediaEvent) : HRESULT;
begin
  Result := S_OK;
end;

function TSourceReaderCallback.OnFlush(dwStreamIndex : DWord) : HRESULT;
begin
  Result := S_OK;
  PostMessage(FCaller, WM_FLUSH_COMPLETE, 0, 0);
  HandleMessages(GetCurrentThread());
end;

procedure TSourceReaderCallback.NotifyMediaFormatChanged;
begin
  PostMessage(FCaller, WM_MEDIA_FORMAT_CHANGED, 0, 0);
  HandleMessages(GetCurrentThread());
end;

procedure TSourceReaderCallback.ResetFramesSkipped;
begin
  FFramesSkipped := 0;
end;

procedure TSourceReaderCallback.ProcessSample(const ASample : IMFSample);
begin
  {$message 'TODO: Handle sample conversion to BMP, or provide sample to main thread for conversion'}
end;

function TSourceReaderCallback.OnReadSample(hrStatus : HRESULT; dwStreamIndex, dwStreamFlags : DWord; llTimestamp : LONGLONG;
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
        bEndOfStream := (dwStreamFlags = DWord(MF_SOURCE_READERF_ENDOFSTREAM));

        if (dwStreamFlags = DWord(MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED)) then
          NotifyMediaFormatChanged
        else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
        begin
          tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
          if bEndOfStream or SampleWithTolerance(RequestedTime, tsSampleTime) then
            ProcessSample(pSample)
          else
          begin
            inc(FFramesSkipped);
            // Read the next sample
            if Assigned(FSourceReader) and (FFramesSkipped < MaxFramesToSkip) then
              FSourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0);
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

function TSourceReaderCallback.SampleWithTolerance(ARequestedTime : TTimeSpan; AActualTime : TTimeSpan) : Boolean;
begin
  Result := CompareValue(ARequestedTime.TotalMilliseconds, AActualTime.TotalMilliseconds, FAllowableOffset) = EqualsValue;
end;

end.
