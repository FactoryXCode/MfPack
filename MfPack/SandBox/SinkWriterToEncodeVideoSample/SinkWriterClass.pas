unit SinkWriterClass;

interface

uses
  Winapi.Windows,
  System.Classes,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.Mfobjects;


const
    // Format constants
    VIDEO_WIDTH  = 640;
    VIDEO_HEIGHT = 480;
    VIDEO_FPS = 30;
    VIDEO_BIT_RATE = 800000;

type

  TSampleSinkWriter = class
  private
    VIDEO_FRAME_DURATION: UINT64;
    VIDEO_ENCODING_FORMAT: TGUID;
    VIDEO_INPUT_FORMAT: TGUID;
    VIDEO_PELS: UINT32;
    VIDEO_FRAME_COUNT: UINT32;
    videoFrameBuffer: array of DWORD;


    function InitializeSinkWriter(out ppWriter: IMFSinkWriter;
                                  out pStreamIndex: DWORD): HResult;
    function WriteFrame(pWriter: IMFSinkWriter;
                    streamIndex: DWORD;
                    const rtStart: LONGLONG {Time stamp.}): HResult;

  public

    constructor Create();
    destructor Destroy(); override;

    function RunSinkWriter(): HResult;

  end;

  var
    FSampleSinkWriter: TSampleSinkWriter;

implementation


constructor TSampleSinkWriter.Create();
begin
  inherited Create();

end;


destructor TSampleSinkWriter.Destroy();
begin

  inherited Destroy();
end;


function TSampleSinkWriter.RunSinkWriter(): HResult;
var
  hr: HResult;
  i: DWord;
  stream: DWORD;
  pSinkWriter: IMFSinkWriter;
  rtStart: LONGLONG;

begin
  VIDEO_FRAME_DURATION := 10 * 1000 * 1000 div VIDEO_FPS;
  VIDEO_ENCODING_FORMAT := MFVideoFormat_WMV3;
  VIDEO_INPUT_FORMAT := MFVideoFormat_RGB32;
  VIDEO_PELS := VIDEO_WIDTH * VIDEO_HEIGHT;
  VIDEO_FRAME_COUNT := 20 * VIDEO_FPS;

  // Buffer to hold the video frame data.
  SetLength(videoFrameBuffer,
            VIDEO_PELS);

  rtStart := 0;

  // Set all pixels to green
  for i := 0 to VIDEO_PELS -1 do
    videoFrameBuffer[i] := $0000FF00;



  hr := CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED);

  if SUCCEEDED(hr) then
    begin
      hr := MFStartup(MF_VERSION);
      if SUCCEEDED(hr) then
        begin
          hr := InitializeSinkWriter(pSinkWriter,
                                     stream);
          if SUCCEEDED(hr) then
            begin
                // Send frames to the sink writer.
                for i := 0 to VIDEO_FRAME_COUNT -1 do
                  begin
                    hr := WriteFrame(pSinkWriter,
                                     stream,
                                     rtStart);
                    if FAILED(hr) then
                      Break;

                    inc(rtStart,
                        VIDEO_FRAME_DURATION);
                  end;
            end;
          if SUCCEEDED(hr) then
            hr := pSinkWriter.Finalize();
        end;
      MFShutdown();
      CoUninitialize();
    end;
  Result := hr;
end;


function TSampleSinkWriter.InitializeSinkWriter(out ppWriter: IMFSinkWriter;
                                                out pStreamIndex: DWORD): HResult;
var
  hr: HResult;
  pSinkWriter: IMFSinkWriter;
  pMediaTypeOut: IMFMediaType;
  pMediaTypeIn: IMFMediaType;
  streamIndex: DWORD;

begin
  hr := MFCreateSinkWriterFromURL(PWideChar('output.wmv'),
                                  nil,
                                  nil,
                                  pSinkWriter);

  // Set the output media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeOut);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_SUBTYPE,
                                VIDEO_ENCODING_FORMAT);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                                  VIDEO_BIT_RATE);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                                  MFVideoInterlace_Progressive);
  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeOut,
                             MF_MT_FRAME_SIZE,
                             VIDEO_WIDTH,
                             VIDEO_HEIGHT);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_FRAME_RATE,
                              VIDEO_FPS,
                              1);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.AddStream(pMediaTypeOut,
                                streamIndex);

  // Set the input media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeIn);


  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_SUBTYPE,
                               VIDEO_INPUT_FORMAT);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             VIDEO_WIDTH,
                             VIDEO_HEIGHT);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_FRAME_RATE,
                              VIDEO_FPS,
                              1);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.SetInputMediaType(streamIndex,
                                        pMediaTypeIn,
                                        nil);


  // Tell the sink writer to start accepting data.
  if SUCCEEDED(hr) then
    hr := pSinkWriter.BeginWriting();

  // Return the pointer to the caller.
  if SUCCEEDED(hr) then
    begin
       ppWriter := pSinkWriter;
       pStreamIndex := streamIndex;
    end;

  Result := hr;
end;



function TSampleSinkWriter.WriteFrame(pWriter: IMFSinkWriter;
                                      streamIndex: DWORD;
                                      const rtStart: LONGLONG {Time stamp.}): HResult;

var
  hr: HResult;
  pSample: IMFSample;
  pBuffer: IMFMediaBuffer;
  pData: PByte;
  cbWidth: LONG;
  cbBuffer: DWORD;

begin
  cbWidth := 4 * VIDEO_WIDTH;
  cbBuffer := DWord(cbWidth * VIDEO_HEIGHT);

  // Create a new memory buffer.
  hr := MFCreateMemoryBuffer(cbBuffer,
                             pBuffer);

  // Lock the buffer and copy the video frame to the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.Lock(pData,
                       nil,
                       nil);

  if SUCCEEDED(hr) then
    begin
        hr := MFCopyImage(pData,                      // Destination buffer.
                          cbWidth,                    // Destination stride.
                          PByte(videoFrameBuffer),    // First row in source image.
                          cbWidth,                    // Source stride.
                          cbWidth,                    // Image width in bytes.
                          VIDEO_HEIGHT                // Image height in pixels.
                         );
     end;

  if Assigned(pBuffer) then
    pBuffer.Unlock();


  // Set the data length of the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.SetCurrentLength(cbBuffer);


  // Create a media sample and add the buffer to the sample.
  if SUCCEEDED(hr) then
    hr := MFCreateSample(pSample);

  if SUCCEEDED(hr) then
    hr := pSample.AddBuffer(pBuffer);

  // Set the time stamp and the duration.
  if SUCCEEDED(hr) then
    hr := pSample.SetSampleTime(rtStart);

  if SUCCEEDED(hr) then
    hr := pSample.SetSampleDuration(VIDEO_FRAME_DURATION);

  // Send the sample to the Sink Writer.
  if SUCCEEDED(hr) then
    hr := pWriter.WriteSample(streamIndex,
                              pSample);

  Result := hr;
end;

end.
