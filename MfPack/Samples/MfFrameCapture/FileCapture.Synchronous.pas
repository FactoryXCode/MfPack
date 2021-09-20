unit FileCapture.Synchronous;

interface

uses
  System.TimeSpan, WinApi.Windows, WinApi.MediaFoundationApi.MfReadWrite, WinApi.MediaFoundationApi.MfObjects, FileCapture;

type
  TFileCaptureSync = class(TFileCapture)
  protected
    procedure ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan); override;
  public
    procedure RequestFrame(APosition : TTimeSpan); override;
    procedure Flush; override;
  end;

implementation

uses
  {Winapi}
  WinApi.WinApiTypes,
  WinApi.MediaFoundationApi.MfUtils,
  {System}
  System.SysUtils,
  System.Types,
  {Application}
  Support,
  VCL.Graphics;

{ TFileCaptureSync }

procedure TFileCaptureSync.RequestFrame(APosition : TTimeSpan);
var
  dwFlags : DWord;
  pSample : IMFSample;
  dSampleTimeStamp : LONGLONG;
  tsSampleTime : TTimeSpan;
  bEndOfStream : Boolean;
  bReachedMaxFrames : Boolean;
  bFound : Boolean;
begin
  inherited;
  dSampleTimeStamp := 0;

  bFound := False;

  bEndOfStream := False;
  bReachedMaxFrames := False;
  dwFlags := 0;

  while not bFound and not bEndOfStream and (FramesSkipped < MaxFramesToSkip) and
    SUCCEEDED(SourceReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM, 0, nil, @dwFlags, nil, @pSample)) do
  begin
    bEndOfStream := (dwFlags = MF_SOURCE_READERF_ENDOFSTREAM);

    if (dwFlags = MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) then
    begin
      // Type change. Get the new format.
      Log('Media Format has changed, getting new format', ltInfo);
      HandleMediaFormatChanged;
    end
    else if Assigned(pSample) and SUCCEEDED(pSample.GetSampleTime(dSampleTimeStamp)) then
    begin
      tsSampleTime := TTimeSpan.FromTicks(dSampleTimeStamp);
      bFound := bEndOfStream or SampleWithinTolerance(APosition, tsSampleTime);
      if bFound then
        ProcessSample(pSample, tsSampleTime)
      else
      begin
        HandleFrameSkipped;
        Log(Format('Skipped frame. Not within requested accuracy. Sample time stamp: %s. Frames Skipped: %d',
          [TimeSpanToDisplay(tsSampleTime, True), FramesSkipped]), ltDebug);
      end;
    end;

    bReachedMaxFrames := (FramesSkipped = MaxFramesToSkip);

    if not bReachedMaxFrames then
      SafeRelease(pSample);
  end;

  if not bFound then
  begin
    if bReachedMaxFrames and Assigned(pSample) then
    begin
      Log(Format('Reached maximum frames to skip %d. Using last frame returned at: %s',
        [FramesSkipped, TimeSpanToDisplay(tsSampleTime, True)]), ltWarning);
      ProcessSample(pSample, tsSampleTime);
    end
    else
      Log(Format('Frame not found. Frames Skipped: %d', [FramesSkipped]), ltWarning);
  end;

  SafeRelease(pSample);
end;

procedure TFileCaptureSync.ProcessSample(const ASample : IMFSample; ATimeStamp : TTimeSpan);
var
  oBitmap : TBitmap;
  sError : string;
begin
  inherited;
  oBitmap := TBitmap.Create( { FVideoInfo.iVideoWidth, FVideoInfo.iVideoHeight } );
  // Compatible with Delphi versions <= 10.3.3
  oBitmap.Width := VideoInfo.iVideoWidth;
  oBitmap.Height := VideoInfo.iVideoHeight;

  if SampleConverter.BitmapFromSample(ASample, VideoInfo, sError, oBitmap) then
  begin
    if Assigned(OnFrameFound) then
      OnFrameFound(oBitmap, ATimeStamp);
  end
  else
    Log('Failed to create BMP from frame sample: ' + sError, ltError);
end;

procedure TFileCaptureSync.Flush;
var
  hr : HRESULT;
begin
  inherited;
  Log('Flush - Begin', ltInfo);

  hr := SourceReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
  if SUCCEEDED(hr) then
    Log('Flush - End', ltInfo)
  else
    Log('Failed to flush source', ltError);

  HandleFlushComplete;
end;

end.
