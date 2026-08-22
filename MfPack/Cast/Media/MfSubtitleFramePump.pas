// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfSubtitleFramePump.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: MfPlayer X2 frame pump. Reads decoded RGB32 video frames, burns subtitles into
//              the frames, and writes the result to a new video stream.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: -
//
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit MfSubtitleFramePump;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.WmCodecDsp,
  {Windows Imaging Component}
  WinApi.WIC.WinCodec,
  {Cast/Media}
  MfSubtitleCompositor;

type
  TMfSubtitleFramePumpProgress = procedure(Sender: TObject;
                                           FramesWritten: Int64;
                                           SampleTime: MFTIME;
                                           var Cancel: Boolean) of object;

  TMfSubtitleFramePumpVideoSample = procedure(Sender: TObject;
                                              const Sample: IMFSample;
                                              Width: UINT32;
                                              Height: UINT32;
                                              SampleTime: MFTIME;
                                              SampleDuration: MFTIME) of object;

  TMfSubtitleFramePumpAudioSample = procedure(Sender: TObject;
                                              const Sample: IMFSample;
                                              SampleTime: MFTIME;
                                              SampleDuration: MFTIME) of object;

  TMfSubtitleAudioState = record
    PendingSample: IMFSample;
    PendingTime: LONGLONG;
    OutputTime: LONGLONG;
    BytesPerSecond: UINT32;
    Pending: Boolean;
    Done: Boolean;
  end;

  TMfSubtitleFramePump = class(TObject)
  private
    FCompositor: TMfSubtitleCompositor;
    FOnProgress: TMfSubtitleFramePumpProgress;
    FOnVideoSample: TMfSubtitleFramePumpVideoSample;
    FOnAudioSample: TMfSubtitleFramePumpAudioSample;
    FFramesWritten: Int64;
    FCancelRequested: Boolean;
    FPauseRequested: Boolean;
    FUseSoftwareVideoDecoder: Boolean;
    FRealTimePacing: Boolean;
    FLoggedConverterGeometry: Boolean;
    FReader: IMFSourceReader;
    FWriter: IMFSinkWriter;
    FStreamIndex: DWORD;
    FAudioVolumePermille: Integer;
    FAudioMuted: Integer;
    FAudioInputStreamIndex: DWORD;

    function CreateReader(const InputFileName: WideString;
                          out Reader: IMFSourceReader): HRESULT;
    function ConfigureReader(Reader: IMFSourceReader;
                             UseNativeNv12: Boolean;
                             out MediaType: IMFMediaType;
                             out Width: UINT32;
                             out Height: UINT32;
                             out FrameRateNum: UINT32;
                             out FrameRateDen: UINT32): HRESULT;

    function CreateNv12Converter(InputType: IMFMediaType;
                                 out Converter: IMFTransform): HRESULT;

    function ConvertNv12Sample(Converter: IMFTransform;
                               InputSample: IMFSample;
                               Width: UINT32;
                               Height: UINT32;
                               out OutputSample: IMFSample): HRESULT;

    function CreateWriter(const OutputFileName: WideString;
                          Width: UINT32;
                          Height: UINT32;
                          FrameRateNum: UINT32;
                          FrameRateDen: UINT32;
                          Bitrate: UINT32;
                          out Writer: IMFSinkWriter;
                          out StreamIndex: DWORD;
                          const OutputByteStream: IMFByteStream = nil;
                          UseFragmentedMp4: Boolean = False): HRESULT;

    function ConfigureAudioReader(const InputFileName: WideString;
                                  out AudioReader: IMFSourceReader;
                                  out AudioMediaType: IMFMediaType;
                                  out HasAudio: Boolean): HRESULT;

    function AddAudioStream(Writer: IMFSinkWriter;
                            AudioInputType: IMFMediaType;
                            out AudioStreamIndex: DWORD): HRESULT;

    function GetAudioSampleDuration(Sample: IMFSample;
                                    const AudioState: TMfSubtitleAudioState;
                                    out SampleDuration: LONGLONG): HRESULT;

    function ApplyAudioGain(Sample: IMFSample): HRESULT;

    function WriteAudioSamples(AudioReader: IMFSourceReader;
                               Writer: IMFSinkWriter;
                               AudioStreamIndex: DWORD;
                               var AudioState: TMfSubtitleAudioState;
                               StopTime: LONGLONG): HRESULT;

    function CompositeSample(Sample: IMFSample;
                             Width: UINT32;
                             Height: UINT32;
                             SampleTime: MFTIME): HRESULT;

    function FlipRgb32InPlace(VideoBuffer: PByte;
                              BufferSize: DWORD;
                              Width: UINT32;
                              Height: UINT32;
                              Stride: Integer): HRESULT;

    function CancelRequested(SampleTime: MFTIME): Boolean;
    function WaitIfPaused(var AExportStartTick: DWORD;
                          SampleTime: MFTIME): Boolean;

    function SetReaderPosition(const Reader: IMFSourceReader;
                               const Position100ns: MFTIME): HRESULT;

    function LoadArtworkRgb32(const FileName: WideString;
                              Width: UINT32;
                              Height: UINT32;
                              out Pixels: TBytes): HRESULT;

    function CreateRgb32Sample(const Pixels: TBytes;
                               SampleTime: MFTIME;
                               SampleDuration: MFTIME;
                               out Sample: IMFSample): HRESULT;
  public

    constructor Create(Compositor: TMfSubtitleCompositor);

    procedure Cancel();
    procedure Pause();
    procedure Resume();
    procedure SetAudioVolume(const AVolume: Single);
    procedure SetAudioMuted(const AMuted: Boolean);
    procedure SelectAudioStream(const AStreamIndex: DWORD);

    function BurnSubtitlesToFile(const InputFileName: WideString;
                                 const OutputFileName: WideString;
                                 Bitrate: UINT32 = 8000000;
                                 const OutputByteStream: IMFByteStream = nil;
                                 UseFragmentedMp4: Boolean = False;
                                 StartTime100ns: MFTIME = 0): HRESULT;

    function AudioWithArtworkToFile(const AudioFileName: WideString;
                                    const ArtworkFileName: WideString;
                                    const OutputFileName: WideString;
                                    Bitrate: UINT32 = 1000000;
                                    const OutputByteStream: IMFByteStream = nil;
                                    UseFragmentedMp4: Boolean = False;
                                    StartTime100ns: MFTIME = 0;
                                    ArtworkFrameRate: UINT32 = 25): HRESULT;

    property FramesWritten: Int64 read FFramesWritten;
    property UseSoftwareVideoDecoder: Boolean read FUseSoftwareVideoDecoder write FUseSoftwareVideoDecoder;
    property RealTimePacing: Boolean read FRealTimePacing write FRealTimePacing;
    property OnProgress: TMfSubtitleFramePumpProgress read FOnProgress write FOnProgress;
    property OnVideoSample: TMfSubtitleFramePumpVideoSample read FOnVideoSample write FOnVideoSample;
    property OnAudioSample: TMfSubtitleFramePumpAudioSample read FOnAudioSample write FOnAudioSample;
  end;


implementation

type

  TMfSubtitleFramePumpCancelThread = class(TThread)
  private
    FReader: IMFSourceReader;
    FWriter: IMFSinkWriter;
    FStreamIndex: DWORD;

  protected
    procedure Execute(); override;

  public

    constructor Create(const Reader: IMFSourceReader;
                       const Writer: IMFSinkWriter;
                       StreamIndex: DWORD);
  end;

constructor TMfSubtitleFramePumpCancelThread.Create(const Reader: IMFSourceReader;
                                                    const Writer: IMFSinkWriter;
                                                    StreamIndex: DWORD);
begin

  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpHigher;
  FReader := Reader;
  FWriter := Writer;
  FStreamIndex := StreamIndex;
end;


procedure TMfSubtitleFramePumpCancelThread.Execute();
var
  hr: HRESULT;
  hrCom: HRESULT;
  comInitialized: Boolean;

begin

  comInitialized := False;
  OutputDebugString(PChar('Export: cancel helper started'));

  hrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);
  if SUCCEEDED(hrCom) then
    comInitialized := True;

  try

    if Assigned(FReader) then
      begin

        OutputDebugString(PChar('Export: cancel helper before reader Flush'));

        hr := FReader.Flush(MF_SOURCE_READER_FIRST_VIDEO_STREAM);

        OutputDebugString(PChar(Format('Export: cancel helper after reader Flush hr=%.8x',
                                       [DWORD(hr)])));
      end;

    // Do not flush the sink writer during a graceful stop. We want samples that
    // were already accepted by WriteSample to remain available for Finalize().
  finally

    FWriter := nil;
    FReader := nil;
    if comInitialized then
      CoUninitialize();
    OutputDebugString(PChar('Export: cancel helper done'));
  end;
end;


constructor TMfSubtitleFramePump.Create(Compositor: TMfSubtitleCompositor);
begin

  inherited Create();

  FCompositor := Compositor;
  FFramesWritten := 0;
  FCancelRequested := False;
  FPauseRequested := False;
  FUseSoftwareVideoDecoder := False;
  FRealTimePacing := False;
  FLoggedConverterGeometry := False;
  FReader := nil;
  FWriter := nil;
  FStreamIndex := 0;
  FAudioVolumePermille := 1000;
  FAudioMuted := 0;
  FAudioInputStreamIndex := MF_SOURCE_READER_FIRST_AUDIO_STREAM;
end;


function TMfSubtitleFramePump.CreateReader(const InputFileName: WideString;
                                           out Reader: IMFSourceReader): HRESULT;
var
  attribs: IMFAttributes;
  useNativeMp4Video: Boolean;

begin

  Reader := nil;
  attribs := nil;
  useNativeMp4Video := SameText(ExtractFileExt(InputFileName), '.mp4');

  Result := MFCreateAttributes(attribs,
                               3);
  if FAILED(Result) then
    Exit;

  if FUseSoftwareVideoDecoder then
    begin
      Result := attribs.SetUINT32(MF_SOURCE_READER_DISABLE_DXVA,
                                  UINT32(True));
      if FAILED(Result) then
        Exit;
    end;

  if not useNativeMp4Video then
    begin
      Result := attribs.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                                  UINT32(True));
      if FAILED(Result) then
        Exit;
    end;

  OutputDebugString(PChar(Format('Export: source reader native MP4 video=%s',
                                 [BoolToStr(useNativeMp4Video)])));

  Result := MFCreateSourceReaderFromURL(PWideChar(InputFileName),
                                        attribs,
                                        Reader);
end;


function TMfSubtitleFramePump.ConfigureReader(Reader: IMFSourceReader;
                                              UseNativeNv12: Boolean;
                                              out MediaType: IMFMediaType;
                                              out Width: UINT32;
                                              out Height: UINT32;
                                              out FrameRateNum: UINT32;
                                              out FrameRateDen: UINT32): HRESULT;
var
  partialType: IMFMediaType;

begin

  MediaType := nil;
  Width := 0;
  Height := 0;
  FrameRateNum := 25;
  FrameRateDen := 1;
  partialType := nil;

  if not Assigned(Reader) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := Reader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                      False);
  if FAILED(Result) then
    Exit;

  Result := Reader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                      True);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(partialType);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  if UseNativeNv12 then
    Result := partialType.SetGUID(MF_MT_SUBTYPE,
                                  MFVideoFormat_NV12)
  else
    Result := partialType.SetGUID(MF_MT_SUBTYPE,
                                  MFVideoFormat_RGB32);
  if FAILED(Result) then
    Exit;

  Result := Reader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                       0,
                                       partialType);
  if FAILED(Result) then
    Exit;

  Result := Reader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                       @MediaType);
  if FAILED(Result) then
    Exit;

  Result := MFGetAttributeSize(MediaType,
                               MF_MT_FRAME_SIZE,
                               Width,
                               Height);
  if FAILED(Result) then
    Exit;

  Result := MFGetAttributeRatio(MediaType,
                                MF_MT_FRAME_RATE,
                                FrameRateNum,
                                FrameRateDen);
  if FAILED(Result) then
    begin
      FrameRateNum := 25;
      FrameRateDen := 1;
      Result := S_OK;
    end;
end;


function TMfSubtitleFramePump.CreateNv12Converter(InputType: IMFMediaType;
                                                  out Converter: IMFTransform): HRESULT;
var
  outputType: IMFMediaType;

begin

  Converter := nil;
  outputType := nil;

  Result := CoCreateInstance(CLSID_CColorConvertDMO,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IID_IMFTransform,
                             Converter);
  if FAILED(Result) then
    Exit;

  Result := Converter.SetInputType(0,
                                   InputType,
                                   0);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(outputType);
  if FAILED(Result) then
    Exit;

  Result := InputType.CopyAllItems(outputType);
  if FAILED(Result) then
    Exit;

  Result := outputType.SetGUID(MF_MT_SUBTYPE,
                               MFVideoFormat_RGB32);
  if FAILED(Result) then
    Exit;

  Result := Converter.SetOutputType(0,
                                    outputType,
                                    0);
  if FAILED(Result) then
    Exit;

  Result := Converter.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                     0);
  if SUCCEEDED(Result) then
    Result := Converter.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                       0);
end;


function TMfSubtitleFramePump.ConvertNv12Sample(Converter: IMFTransform;
                                                InputSample: IMFSample;
                                                Width: UINT32;
                                                Height: UINT32;
                                                out OutputSample: IMFSample): HRESULT;
var
  streamInfo: MFT_OUTPUT_STREAM_INFO;
  outputBuffer: IMFMediaBuffer;
  outputData: MFT_OUTPUT_DATA_BUFFER;
  processStatus: DWORD;
  bufferSize: UINT32;
  currentLength: DWORD;
  maxLength: DWORD;
  tightSample: IMFSample;
  tightBuffer: IMFMediaBuffer;
  sourceData: PByte;
  targetData: PByte;
  sourceMaxLength: DWORD;
  targetMaxLength: DWORD;
  targetLength: DWORD;
  sourceStride: UINT32;
  codedHeight: UINT32;
  row: UINT32;

begin

  OutputSample := nil;
  outputBuffer := nil;
  ZeroMemory(@streamInfo, SizeOf(streamInfo));
  ZeroMemory(@outputData, SizeOf(outputData));
  processStatus := 0;
  currentLength := 0;
  maxLength := 0;
  tightSample := nil;
  tightBuffer := nil;
  sourceData := nil;
  targetData := nil;

  if (not Assigned(Converter)) or (not Assigned(InputSample)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := Converter.ProcessInput(0,
                                   InputSample,
                                   0);
  if FAILED(Result) then
    Exit;

  Result := Converter.GetOutputStreamInfo(0,
                                          streamInfo);
  if FAILED(Result) then
    Exit;

  bufferSize := Width * Height * 4;
  if streamInfo.cbSize > bufferSize then
    bufferSize := streamInfo.cbSize;

  Result := MFCreateSample(OutputSample);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMemoryBuffer(bufferSize,
                                 outputBuffer);
  if FAILED(Result) then
    Exit;

  Result := OutputSample.AddBuffer(outputBuffer);
  if FAILED(Result) then
    Exit;

  outputData.dwStreamID := 0;
  outputData.pSample := OutputSample;
  Result := Converter.ProcessOutput(0,
                                    1,
                                    @outputData,
                                    processStatus);
  if FAILED(Result) then
    begin
      OutputSample := nil;
      Exit;
    end;

  Result := outputBuffer.GetCurrentLength(currentLength);
  if FAILED(Result) then
    Exit;
  outputBuffer.GetMaxLength(maxLength);

  if not FLoggedConverterGeometry then
    begin
      FLoggedConverterGeometry := True;
      OutputDebugString(PChar(Format(
        'Export: converter output width=%d height=%d cbSize=%d current=%d max=%d',
        [Width, Height, streamInfo.cbSize, currentLength, maxLength])));
    end;

  targetLength := Width * Height * 4;
  if currentLength = targetLength then
    Exit;

  codedHeight := (Height + 15) and not UINT32(15);
  if (codedHeight = 0) or ((currentLength mod codedHeight) <> 0) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  sourceStride := currentLength div codedHeight;
  if sourceStride < (Width * 4) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  Result := MFCreateSample(tightSample);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMemoryBuffer(targetLength,
                                 tightBuffer);
  if FAILED(Result) then
    Exit;

  Result := tightSample.AddBuffer(tightBuffer);
  if FAILED(Result) then
    Exit;

  sourceMaxLength := 0;
  targetMaxLength := 0;
  Result := outputBuffer.Lock(sourceData,
                              @sourceMaxLength,
                              @currentLength);
  if FAILED(Result) then
    Exit;
  try
    Result := tightBuffer.Lock(targetData,
                               @targetMaxLength,
                               nil);
    if FAILED(Result) then
      Exit;
    try
      for row := 0 to Height - 1 do
        CopyMemory(PByte(NativeInt(targetData) + NativeInt(row * Width * 4)),
                   PByte(NativeInt(sourceData) + NativeInt(row * sourceStride)),
                   Width * 4);
      Result := tightBuffer.SetCurrentLength(targetLength);
    finally
      tightBuffer.Unlock();
    end;
  finally
    outputBuffer.Unlock();
  end;

  if SUCCEEDED(Result) then
    OutputSample := tightSample;
end;


function TMfSubtitleFramePump.CreateWriter(const OutputFileName: WideString;
                                           Width: UINT32;
                                           Height: UINT32;
                                           FrameRateNum: UINT32;
                                           FrameRateDen: UINT32;
                                           Bitrate: UINT32;
                                           out Writer: IMFSinkWriter;
                                           out StreamIndex: DWORD;
                                           const OutputByteStream: IMFByteStream;
                                           UseFragmentedMp4: Boolean): HRESULT;
var
  attribs: IMFAttributes;
  outputType: IMFMediaType;
  inputType: IMFMediaType;
  KeyFrameSpacing: UINT32;

begin

  Writer := nil;
  StreamIndex := 0;
  attribs := nil;
  outputType := nil;
  inputType := nil;

  Result := MFCreateAttributes(attribs,
                               2);
  if FAILED(Result) then
    Exit;

  Result := attribs.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
                              UINT32(False));
  if FAILED(Result) then
    Exit;

  Result := attribs.SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING,
                              UINT32(True));
  if FAILED(Result) then
    Exit;

  if UseFragmentedMp4 then
    begin
      Result := attribs.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                                MFTranscodeContainerType_FMPEG4);
      if FAILED(Result) then
        Exit;

      // The default fragmented-MP4 cadence follows the encoder GOP and can
      // release several seconds of media in one burst. Chromecast then drains
      // that fragment and repeatedly starves at the next boundary. Request
      // one-second fragments for a steady live HTTP stream.
      Result := attribs.SetUINT64(MF_MPEG4SINK_MIN_FRAGMENT_DURATION,
                                  UInt64(10000000));
      if FAILED(Result) then
        Exit;
    end;

  Result := MFCreateSinkWriterFromURL(PWideChar(OutputFileName),
                                      OutputByteStream,
                                      attribs,
                                      Writer);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(outputType);
  if FAILED(Result) then
    Exit;

  Result := outputType.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := outputType.SetGUID(MF_MT_SUBTYPE,
                               MFVideoFormat_H264);
  if FAILED(Result) then
    Exit;

  Result := outputType.SetUINT32(MF_MT_AVG_BITRATE,
                                 Bitrate);
  if FAILED(Result) then
    Exit;

  Result := outputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeSize(outputType,
                               MF_MT_FRAME_SIZE,
                               Width,
                               Height);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(outputType,
                                MF_MT_FRAME_RATE,
                                FrameRateNum,
                                FrameRateDen);
  if FAILED(Result) then
    Exit;

  if UseFragmentedMp4 then
    begin
      KeyFrameSpacing := FrameRateNum div FrameRateDen;
      if (FrameRateNum mod FrameRateDen) <> 0 then
        Inc(KeyFrameSpacing);
      if (KeyFrameSpacing = 0) then
        KeyFrameSpacing := 1;

      Result := outputType.SetUINT32(MF_MT_MAX_KEYFRAME_SPACING,
                                     KeyFrameSpacing);
      if FAILED(Result) then
        Exit;
    end;

  Result := MFSetAttributeRatio(outputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
  if FAILED(Result) then
    Exit;


  Result := Writer.AddStream(outputType,
                             StreamIndex);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(inputType);
  if FAILED(Result) then
    Exit;

  Result := inputType.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := inputType.SetGUID(MF_MT_SUBTYPE,
                              MFVideoFormat_RGB32);
  if FAILED(Result) then
    Exit;

  Result := inputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeSize(inputType,
                               MF_MT_FRAME_SIZE,
                               Width,
                               Height);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(inputType,
                                MF_MT_FRAME_RATE,
                                FrameRateNum,
                                FrameRateDen);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(inputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
  if FAILED(Result) then
    Exit;

  Result := Writer.SetInputMediaType(StreamIndex,
                                     inputType,
                                     nil);
end;


function TMfSubtitleFramePump.ConfigureAudioReader(const InputFileName: WideString;
                                                   out AudioReader: IMFSourceReader;
                                                   out AudioMediaType: IMFMediaType;
                                                   out HasAudio: Boolean): HRESULT;
var
  partialType: IMFMediaType;

begin

  AudioReader := nil;
  AudioMediaType := nil;
  HasAudio := False;
  partialType := nil;

  Result := MFCreateSourceReaderFromURL(PWideChar(InputFileName),
                                        nil,
                                        AudioReader);
  if FAILED(Result) then
    Exit;

  Result := AudioReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                           False);
  if FAILED(Result) then
    Exit;

  Result := AudioReader.SetStreamSelection(FAudioInputStreamIndex,
                                           True);
  if FAILED(Result) then
    begin
      Result := S_OK;
      AudioReader := nil;
      Exit;
    end;

  Result := MFCreateMediaType(partialType);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetGUID(MF_MT_SUBTYPE,
                                MFAudioFormat_PCM);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                  16);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                  48000);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                  2);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                  4);
  if FAILED(Result) then
    Exit;

  Result := partialType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                  192000);
  if FAILED(Result) then
    Exit;

  Result := AudioReader.SetCurrentMediaType(FAudioInputStreamIndex,
                                            0,
                                            partialType);
  if FAILED(Result) then
    begin
      Result := S_OK;
      AudioReader := nil;
      Exit;
    end;

  Result := AudioReader.GetCurrentMediaType(FAudioInputStreamIndex,
                                            @AudioMediaType);
  if SUCCEEDED(Result) then
    HasAudio := True;
end;


function TMfSubtitleFramePump.AddAudioStream(Writer: IMFSinkWriter;
                                             AudioInputType: IMFMediaType;
                                             out AudioStreamIndex: DWORD): HRESULT;
var
  audioOutputType: IMFMediaType;
  channels: UINT32;
  samplesPerSec: UINT32;
  bitsPerSample: UINT32;
  avgBytesPerSec: UINT32;

begin

  AudioStreamIndex := 0;
  audioOutputType := nil;
  channels := 2;
  samplesPerSec := 48000;
  bitsPerSample := 16;

  if (not Assigned(Writer)) or (not Assigned(AudioInputType)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if FAILED(AudioInputType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                     channels)) or (channels = 0) then
    channels := 2;
  if FAILED(AudioInputType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                     samplesPerSec)) or (samplesPerSec = 0) then
    samplesPerSec := 48000;
  if FAILED(AudioInputType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                     bitsPerSample)) or (bitsPerSample = 0) then
    bitsPerSample := 16;

  if (channels <= 1) then
    avgBytesPerSec := 12000
  else
    if (channels <= 2) then
      avgBytesPerSec := 24000
    else
      avgBytesPerSec := 48000;

  OutputDebugString(PChar(Format('Export: audio input channels=%d rate=%d bits=%d aacBytesPerSec=%d',
                                 [channels, samplesPerSec, bitsPerSample, avgBytesPerSec])));

  Result := MFCreateMediaType(audioOutputType);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetGUID(MF_MT_MAJOR_TYPE,
                                    MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetGUID(MF_MT_SUBTYPE,
                                    MFAudioFormat_AAC);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                      bitsPerSample);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                      samplesPerSec);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                      channels);
  if FAILED(Result) then
    Exit;


  Result := audioOutputType.SetUINT32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                      $29);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_AAC_PAYLOAD_TYPE,
                                      0);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                      avgBytesPerSec);
  if FAILED(Result) then
    Exit;

  Result := audioOutputType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                                      UINT32(True));
  if FAILED(Result) then
    Exit;

  Result := Writer.AddStream(audioOutputType,
                             AudioStreamIndex);
  if FAILED(Result) then
    begin

      OutputDebugString(PChar(Format('Export: audio AddStream failed hr=%.8x',
                                     [DWORD(Result)])));
      Exit;
    end;

  Result := Writer.SetInputMediaType(AudioStreamIndex,
                                     AudioInputType,
                                     nil);
  if FAILED(Result) then
    OutputDebugString(PChar(Format('Export: audio SetInputMediaType failed hr=%.8x', [DWORD(Result)])));
end;


function TMfSubtitleFramePump.GetAudioSampleDuration(Sample: IMFSample;
                                                    const AudioState: TMfSubtitleAudioState;
                                                    out SampleDuration: LONGLONG): HRESULT;
var
  totalLength: DWORD;

begin

  SampleDuration := 0;
  Result := S_OK;

  if not Assigned(Sample) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  totalLength := 0;
  if SUCCEEDED(Sample.GetTotalLength(@totalLength)) and
     (totalLength > 0) and (AudioState.BytesPerSecond > 0) then
    SampleDuration := (Int64(totalLength) * 10000000) div Int64(AudioState.BytesPerSecond);

  if (SampleDuration <= 0) then
    begin
      Sample.GetSampleDuration(@SampleDuration);

      if (SampleDuration <= 0) then
        SampleDuration := 100000;
    end;
end;


function TMfSubtitleFramePump.ApplyAudioGain(Sample: IMFSample): HRESULT;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  SampleData: PSmallInt;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  SampleCount: DWORD;
  I: DWORD;
  VolumePermille: Integer;
  SampleValue: Integer;

begin

  if not Assigned(Sample) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  VolumePermille := InterlockedCompareExchange(FAudioVolumePermille,
                                               0,
                                               0);

  if (InterlockedCompareExchange(FAudioMuted,
                                 0,
                                 0) <> 0) then
    VolumePermille := 0;

  if (VolumePermille >= 1000) then
    begin
      Result := S_OK;
      Exit;
    end;

  Buffer := nil;
  Result := Sample.ConvertToContiguousBuffer(@Buffer);
  if FAILED(Result) then
    Exit;

  Data := nil;
  MaxLength := 0;
  CurrentLength := 0;
  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    if (VolumePermille = 0) then
      begin
        if (CurrentLength > 0) then
          FillChar(Data^,
                   CurrentLength,
                   0);
      end
    else
      begin
        SampleData := PSmallInt(Data);
        SampleCount := CurrentLength div SizeOf(SmallInt);

        if (SampleCount > 0) then
          for I := 0 to SampleCount - 1 do
            begin
              SampleValue := (Integer(SampleData^) * VolumePermille) div 1000;
              SampleData^ := SmallInt(SampleValue);
              Inc(SampleData);
            end;
      end;
  finally
    Buffer.Unlock();
  end;

  Result := S_OK;
end;


procedure TMfSubtitleFramePump.Cancel();
begin

  if FCancelRequested then
    Exit;

  OutputDebugString(PChar('Export: cancel requested'));
  FCancelRequested := True;
  FPauseRequested := False;

  if Assigned(FReader) or Assigned(FWriter) then
    TMfSubtitleFramePumpCancelThread.Create(FReader,
                                            FWriter,
                                            FStreamIndex).Start();
end;


procedure TMfSubtitleFramePump.Pause();
begin

  if FCancelRequested or FPauseRequested then
    Exit;

  OutputDebugString(PChar('Export: pause requested'));
  FPauseRequested := True;
end;


procedure TMfSubtitleFramePump.Resume();
begin

  if not FPauseRequested then
    Exit;

  OutputDebugString(PChar('Export: resume requested'));
  FPauseRequested := False;
end;


procedure TMfSubtitleFramePump.SetAudioVolume(const AVolume: Single);
var
  Volume: Single;

begin

  Volume := AVolume;
  if (Volume < 0.0) then
    Volume := 0.0
  else
    if (Volume > 1.0) then
      Volume := 1.0;

  InterlockedExchange(FAudioVolumePermille,
                      Round(Volume * 1000.0));
end;


procedure TMfSubtitleFramePump.SelectAudioStream(const AStreamIndex: DWORD);
begin

  FAudioInputStreamIndex := AStreamIndex;
end;


procedure TMfSubtitleFramePump.SetAudioMuted(const AMuted: Boolean);
begin

  if AMuted then
    InterlockedExchange(FAudioMuted,
                        1)
  else
    InterlockedExchange(FAudioMuted,
                        0);
end;


function TMfSubtitleFramePump.WaitIfPaused(var AExportStartTick: DWORD;
                                           SampleTime: MFTIME): Boolean;
var
  pauseStartTick: DWORD;
  pauseElapsedMs: DWORD;

begin

  Result := FCancelRequested;
  if Result or (not FPauseRequested) then
    Exit;

  pauseStartTick := GetTickCount();
  OutputDebugString(PChar(Format('Export: paused at frames=%d time=%d',
                                 [FFramesWritten, SampleTime])));
  while FPauseRequested and (not FCancelRequested) do
    Sleep(25);

  pauseElapsedMs := DWORD(GetTickCount() - pauseStartTick);
  AExportStartTick := DWORD(AExportStartTick + pauseElapsedMs);
  OutputDebugString(PChar(Format('Export: resumed after %d ms',
                                 [pauseElapsedMs])));
  Result := FCancelRequested;
end;


function TMfSubtitleFramePump.CancelRequested(SampleTime: MFTIME): Boolean;
var
  cancel: Boolean;

begin

  Result := FCancelRequested;

  if Assigned(FOnProgress) then
    begin
      cancel := False;
      FOnProgress(Self,
                  FFramesWritten,
                  SampleTime,
                  cancel);
      Result := Result or cancel;
    end;
end;


function TMfSubtitleFramePump.WriteAudioSamples(AudioReader: IMFSourceReader;
                                                Writer: IMFSinkWriter;
                                                AudioStreamIndex: DWORD;
                                                var AudioState: TMfSubtitleAudioState;
                                                StopTime: LONGLONG): HRESULT;
var
  sample: IMFSample;
  actualStreamIndex: DWORD;
  flags: DWORD;
  sampleTime: LONGLONG;
  sampleDuration: LONGLONG;
  writeRetryCount: Integer;

begin

  Result := S_OK;

  if (not Assigned(AudioReader)) or (not Assigned(Writer)) or AudioState.Done then
    Exit;

  if AudioState.Pending then
    begin
      if (StopTime > 0) and (AudioState.PendingTime > StopTime) then
        Exit;

      sample := AudioState.PendingSample;
      AudioState.PendingSample := nil;
      AudioState.Pending := False;

      if Assigned(sample) then
        begin
          Result := GetAudioSampleDuration(sample,
                                           AudioState,
                                           sampleDuration);
          if FAILED(Result) then
            Exit;
          Result := sample.SetSampleTime(AudioState.PendingTime);

          if SUCCEEDED(Result) then
            Result := sample.SetSampleDuration(sampleDuration);

          if SUCCEEDED(Result) then
            Result := ApplyAudioGain(sample);

          if SUCCEEDED(Result) and Assigned(FOnAudioSample) then
            FOnAudioSample(Self,
                           sample,
                           AudioState.PendingTime,
                           sampleDuration);

          if SUCCEEDED(Result) then
            begin
              writeRetryCount := 0;
              repeat
                Result := Writer.WriteSample(AudioStreamIndex,
                                             sample);
                if (Result <> E_OUTOFMEMORY) then
                  Break;

                Inc(writeRetryCount);
                if (writeRetryCount = 1) or
                   ((writeRetryCount mod 40) = 0) then
                  OutputDebugString(PChar(Format('Export: pending audio WriteSample waiting for encoder retries=%d',
                                                 [writeRetryCount])));
                Sleep(25);
              until (writeRetryCount >= 1200) or FCancelRequested;
            end;
          if FAILED(Result) then
            Exit;
          AudioState.OutputTime := AudioState.PendingTime + sampleDuration;
        end;
    end;

  while True do
    begin

      sample := nil;
      actualStreamIndex := 0;
      flags := 0;
      sampleTime := 0;

      Result := AudioReader.ReadSample(FAudioInputStreamIndex,
                                       0,
                                       @actualStreamIndex,
                                       @flags,
                                       @sampleTime,
                                       @sample);
      if FAILED(Result) then
        Break;

      if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
        begin
          AudioState.Done := True;
          Break;
        end;

      if ((flags and MF_SOURCE_READERF_ERROR) <> 0) or
         ((flags and MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) <> 0) or
         ((flags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0) then
        begin

          Result := E_FAIL;
          Break;
        end;

      if not Assigned(sample) then
        Continue;

      if (StopTime > 0) and (AudioState.OutputTime > StopTime) then
        begin
          AudioState.PendingSample := sample;
          AudioState.PendingTime := AudioState.OutputTime;
          AudioState.Pending := True;
          Break;
        end;

      Result := GetAudioSampleDuration(sample,
                                       AudioState,
                                       sampleDuration);
      if FAILED(Result) then
        Break;

      Result := sample.SetSampleTime(AudioState.OutputTime);
      if SUCCEEDED(Result) then
        Result := sample.SetSampleDuration(sampleDuration);

      if SUCCEEDED(Result) then
        Result := ApplyAudioGain(sample);

      if SUCCEEDED(Result) and Assigned(FOnAudioSample) then
        FOnAudioSample(Self,
                       sample,
                       AudioState.OutputTime,
                       sampleDuration);

      if SUCCEEDED(Result) then
        begin
          writeRetryCount := 0;
          repeat
            Result := Writer.WriteSample(AudioStreamIndex,
                                         sample);
            if (Result <> E_OUTOFMEMORY) then
              Break;

            Inc(writeRetryCount);
            if (writeRetryCount = 1) or
               ((writeRetryCount mod 40) = 0) then
              OutputDebugString(PChar(Format('Export: audio WriteSample waiting for encoder retries=%d time=%d',
                                             [writeRetryCount, AudioState.OutputTime])));
            Sleep(25);
          until (writeRetryCount >= 1200) or FCancelRequested;
        end;
      if FAILED(Result) then
        Break;

      Inc(AudioState.OutputTime,
          sampleDuration);
    end;
end;


function TMfSubtitleFramePump.FlipRgb32InPlace(VideoBuffer: PByte;
                                              BufferSize: DWORD;
                                              Width: UINT32;
                                              Height: UINT32;
                                              Stride: Integer): HRESULT;
var
  rowBytes: Integer;
  row: Integer;
  halfRows: Integer;
  requiredBytes: UInt64;
  topRow: PByte;
  bottomRow: PByte;
  tempRow: Pointer;

begin

  Result := S_OK;

  if (VideoBuffer = nil) or
     (Width = 0) or
     (Height = 0) or
     (Stride <= 0) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  rowBytes := Integer(Width) * 4;
  if (Stride < rowBytes) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  requiredBytes := UInt64(Stride) * UInt64(Height);
  if (requiredBytes > UInt64(BufferSize)) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  halfRows := Integer(Height div 2);
  if (halfRows <= 0) then
    Exit;

  GetMem(tempRow,
         rowBytes);

  if (tempRow = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  try
    for row := 0 to halfRows - 1 do
      begin
        topRow := PByte(NativeInt(VideoBuffer) + (NativeInt(row) * NativeInt(Stride)));
        bottomRow := PByte(NativeInt(VideoBuffer) + (NativeInt(Integer(Height) - 1 - row) * NativeInt(Stride)));

        CopyMemory(tempRow,
                   topRow,
                   rowBytes);

        CopyMemory(topRow,
                   bottomRow,
                   rowBytes);

        CopyMemory(bottomRow,
                   tempRow,
                   rowBytes);
      end;
  finally
    FreeMem(tempRow);
  end;
end;


function TMfSubtitleFramePump.CompositeSample(Sample: IMFSample;
                                              Width: UINT32;
                                              Height: UINT32;
                                              SampleTime: MFTIME): HRESULT;
var
  buffer: IMFMediaBuffer;
  data: PByte;
  maxLength: DWORD;
  currentLength: DWORD;
  stride: Integer;

begin

  Result := S_OK;

  if (not Assigned(Sample)) or (not Assigned(FCompositor)) then
    Exit;

  buffer := nil;
  data := nil;
  maxLength := 0;
  currentLength := 0;
  stride := Integer(Width) * 4;

  Result := Sample.ConvertToContiguousBuffer(@buffer);
  if FAILED(Result) then
    Exit;

  Result := buffer.Lock(data,
                        @maxLength,
                        @currentLength);
  if FAILED(Result) then
    Exit;

  try
    Result := FCompositor.CompositeRgb32(data,
                                         currentLength,
                                         Integer(Width),
                                         Integer(Height),
                                         stride,
                                         SampleTime div 10000);
    if SUCCEEDED(Result) then
      Result := FlipRgb32InPlace(data,
                                 currentLength,
                                 Width,
                                 Height,
                                 stride);
  finally
    buffer.Unlock();
  end;
end;


function TMfSubtitleFramePump.SetReaderPosition(
  const Reader: IMFSourceReader;
  const Position100ns: MFTIME): HRESULT;
var
  Position: PROPVARIANT;

begin

  if not Assigned(Reader) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if (Position100ns <= 0) then
    begin
      Result := S_OK;
      Exit;
    end;

  PropVariantInit(Position);

  try
    Position.vt := VT_I8;
    Position.hVal.QuadPart := Position100ns;
    Result := Reader.SetCurrentPosition(GUID_NULL,
                                        Position);
  finally
    PropVariantClear(Position);
  end;
end;


function TMfSubtitleFramePump.LoadArtworkRgb32(const FileName: WideString;
                                               Width: UINT32;
                                               Height: UINT32;
                                               out Pixels: TBytes): HRESULT;
var
  factory: IWICImagingFactory;
  decoder: IWICBitmapDecoder;
  frame: IWICBitmapFrameDecode;
  scaler: IWICBitmapScaler;
  converter: IWICFormatConverter;
  sourceWidth: UINT;
  sourceHeight: UINT;
  scaledWidth: UINT;
  scaledHeight: UINT;
  left: UINT;
  top: UINT;
  sourcePixels: TBytes;
  sourceRow: UINT;
  targetRow: UINT;
  sourceOffset: NativeInt;
  targetOffset: NativeInt;

begin

  SetLength(Pixels,
            0);
  factory := nil;
  decoder := nil;
  frame := nil;
  scaler := nil;
  converter := nil;

  if (FileName = '') or (Width = 0) or (Height = 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Result := CoCreateInstance(CLSID_WICImagingFactory1,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IWICImagingFactory,
                             factory);
  if FAILED(Result) then
    Exit;

  Result := factory.CreateDecoderFromFilename(PWideChar(FileName),
                                               GUID_NULL,
                                               GENERIC_READ,
                                               WICDecodeMetadataCacheOnLoad,
                                               decoder);
  if FAILED(Result) then
    Exit;

  // A GIF is deliberately treated as static artwork: use its first frame.
  Result := decoder.GetFrame(0,
                             frame);
  if FAILED(Result) then
    Exit;

  Result := frame.GetSize(sourceWidth,
                          sourceHeight);
  if FAILED(Result) or (sourceWidth = 0) or (sourceHeight = 0) then
    begin
      if SUCCEEDED(Result) then
        Result := E_INVALIDARG;
      Exit;
    end;

  if (UInt64(Width) * UInt64(sourceHeight)) <=
     (UInt64(Height) * UInt64(sourceWidth)) then
    begin
      scaledWidth := Width;
      scaledHeight := UINT((UInt64(sourceHeight) * UInt64(Width)) div sourceWidth);
    end
  else
    begin
      scaledHeight := Height;
      scaledWidth := UINT((UInt64(sourceWidth) * UInt64(Height)) div sourceHeight);
    end;

  if scaledWidth = 0 then
    scaledWidth := 1;
  if scaledHeight = 0 then
    scaledHeight := 1;

  Result := factory.CreateBitmapScaler(scaler);
  if FAILED(Result) then
    Exit;

  Result := scaler.Initialize(frame,
                              scaledWidth,
                              scaledHeight,
                              WICBitmapInterpolationModeHighQualityCubic);
  if FAILED(Result) then
    Exit;

  Result := factory.CreateFormatConverter(converter);
  if FAILED(Result) then
    Exit;

  Result := converter.Initialize(scaler,
                                 GUID_WICPixelFormat32bppBGRA,
                                 WICBitmapDitherTypeNone,
                                 nil,
                                 0.0,
                                 WICBitmapPaletteTypeCustom);
  if FAILED(Result) then
    Exit;

  SetLength(sourcePixels,
            NativeInt(scaledWidth) * NativeInt(scaledHeight) * 4);
  Result := converter.CopyPixels(nil,
                                 scaledWidth * 4,
                                 Length(sourcePixels),
                                 PByte(sourcePixels));
  if FAILED(Result) then
    Exit;

  // The sink writer's RGB32 input is bottom-up. Centre the decoded picture on
  // a black 16:9 canvas and reverse the WIC top-down scan-line order.
  SetLength(Pixels,
            NativeInt(Width) * NativeInt(Height) * 4);

  FillChar(Pixels[0],
           Length(Pixels),
           0);

  left := (Width - scaledWidth) div 2;
  top := (Height - scaledHeight) div 2;

  for sourceRow := 0 to scaledHeight - 1 do
    begin
      targetRow := Height - 1 - (top + sourceRow);
      sourceOffset := NativeInt(sourceRow) * NativeInt(scaledWidth) * 4;
      targetOffset := (NativeInt(targetRow) * NativeInt(Width) + NativeInt(left)) * 4;
      CopyMemory(@Pixels[targetOffset],
                 @sourcePixels[sourceOffset],
                 scaledWidth * 4);
    end;

  Result := S_OK;
end;


function TMfSubtitleFramePump.CreateRgb32Sample(const Pixels: TBytes;
                                                SampleTime: MFTIME;
                                                SampleDuration: MFTIME;
                                                out Sample: IMFSample): HRESULT;
var
  buffer: IMFMediaBuffer;
  data: PByte;
  maxLength: DWORD;
  currentLength: DWORD;

begin

  Sample := nil;
  buffer := nil;
  data := nil;

  if Length(Pixels) = 0 then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Result := MFCreateSample(Sample);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMemoryBuffer(Length(Pixels),
                                 buffer);
  if FAILED(Result) then
    Exit;

  Result := buffer.Lock(data,
                        @maxLength,
                        @currentLength);
  if FAILED(Result) then
    Exit;

  try
    CopyMemory(data,
               @Pixels[0],
               Length(Pixels));
  finally
    buffer.Unlock();
  end;

  Result := buffer.SetCurrentLength(Length(Pixels));
  if SUCCEEDED(Result) then
    Result := Sample.AddBuffer(buffer);
  if SUCCEEDED(Result) then
    Result := Sample.SetSampleTime(SampleTime);
  if SUCCEEDED(Result) then
    Result := Sample.SetSampleDuration(SampleDuration);
end;


function TMfSubtitleFramePump.AudioWithArtworkToFile(const AudioFileName: WideString;
                                                     const ArtworkFileName: WideString;
                                                     const OutputFileName: WideString;
                                                     Bitrate: UINT32;
                                                     const OutputByteStream: IMFByteStream;
                                                     UseFragmentedMp4: Boolean;
                                                     StartTime100ns: MFTIME;
                                                     ArtworkFrameRate: UINT32): HRESULT;
const
  OutputWidth = 1280;
  OutputHeight = 720;
  StartupLeadMs = 1000;

var
  writer: IMFSinkWriter;
  audioReader: IMFSourceReader;
  audioType: IMFMediaType;
  sample: IMFSample;
  videoStreamIndex: DWORD;
  audioStreamIndex: DWORD;
  hasAudio: Boolean;
  audioState: TMfSubtitleAudioState;
  pixels: TBytes;
  frameDuration: MFTIME;
  outputTime: MFTIME;
  exportStartTick: DWORD;
  paceTargetMs: Int64;
  paceElapsedMs: Int64;
  paceSleepMs: Int64;

begin

  FFramesWritten := 0;
  FCancelRequested := False;
  FPauseRequested := False;
  FReader := nil;
  FWriter := nil;
  FStreamIndex := 0;
  writer := nil;
  audioReader := nil;
  audioType := nil;
  sample := nil;
  videoStreamIndex := 0;
  audioStreamIndex := 0;
  hasAudio := False;
  audioState.PendingSample := nil;
  audioState.PendingTime := 0;
  audioState.OutputTime := 0;
  audioState.BytesPerSecond := 0;
  audioState.Pending := False;
  audioState.Done := False;

  if (AudioFileName = '') or (ArtworkFileName = '') or
     ((OutputFileName = '') and (not Assigned(OutputByteStream))) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if (ArtworkFrameRate = 0) then
    ArtworkFrameRate := 25;

  frameDuration := 10000000 div ArtworkFrameRate;

  if (frameDuration <= 0) then
    frameDuration := 400000;

  if (StartTime100ns < 0) then
    StartTime100ns := 0;

  OutputDebugString(PChar(Format('Export: loading audio artwork "%s"',
                                 [ArtworkFileName])));

  Result := LoadArtworkRgb32(ArtworkFileName,
                             OutputWidth,
                             OutputHeight,
                             pixels);
  if FAILED(Result) then
    Exit;

  Result := ConfigureAudioReader(AudioFileName,
                                 audioReader,
                                 audioType,
                                 hasAudio);
  if FAILED(Result) then
    Exit;

  if not hasAudio then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  if (StartTime100ns > 0) then
    begin
      Result := SetReaderPosition(audioReader,
                                  StartTime100ns);
      if FAILED(Result) then
        Exit;
    end;

  FReader := audioReader;
  Result := CreateWriter(OutputFileName,
                         OutputWidth,
                         OutputHeight,
                         ArtworkFrameRate,
                         1,
                         Bitrate,
                         writer,
                         videoStreamIndex,
                         OutputByteStream,
                         UseFragmentedMp4);
  if FAILED(Result) then
    Exit;

  FWriter := writer;
  FStreamIndex := videoStreamIndex;
  Result := AddAudioStream(writer,
                           audioType,
                           audioStreamIndex);
  if FAILED(Result) then
    Exit;

  Result := audioType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                audioState.BytesPerSecond);
  if FAILED(Result) then
    Exit;

  Result := writer.BeginWriting();
  if FAILED(Result) then
    Exit;

  outputTime := 0;
  exportStartTick := GetTickCount();

  while SUCCEEDED(Result) and (not audioState.Done) do
    begin
      if WaitIfPaused(exportStartTick,
                      outputTime) or CancelRequested(outputTime) then
        begin
          Result := E_ABORT;
          Break;
        end;

      if FRealTimePacing then
        begin

          // Feed the encoder one second of identical pictures immediately so
          // its first H.264 keyframe/fragment is available without waiting for
          // wall-clock samples. Thereafter keep the presentation one second
          // ahead of real time as a modest receiver startup buffer.
          paceTargetMs := (outputTime div 10000) - StartupLeadMs;

          if (paceTargetMs < 0) then
            paceTargetMs := 0;

          paceElapsedMs := DWORD(GetTickCount() - exportStartTick);

          while (paceElapsedMs < paceTargetMs) and (not FCancelRequested) do
            begin
              paceSleepMs := paceTargetMs - paceElapsedMs;
              if paceSleepMs > 25 then
                paceSleepMs := 25;
              Sleep(DWORD(paceSleepMs));
              paceElapsedMs := DWORD(GetTickCount() - exportStartTick);
            end;

          if FCancelRequested then
            begin
              Result := E_ABORT;
              Break;
            end;
        end;

      Result := CreateRgb32Sample(pixels,
                                  outputTime,
                                  frameDuration,
                                  sample);
      if FAILED(Result) then
        Break;

      if Assigned(FOnVideoSample) then
        FOnVideoSample(Self,
                       sample,
                       OutputWidth,
                       OutputHeight,
                       outputTime,
                       frameDuration);

      Result := writer.WriteSample(videoStreamIndex,
                                   sample);
      sample := nil;
      if FAILED(Result) then
        Break;

      Result := WriteAudioSamples(audioReader,
                                  writer,
                                  audioStreamIndex,
                                  audioState,
                                  outputTime + frameDuration);
      if FAILED(Result) then
        Break;

      Inc(FFramesWritten);
      Inc(outputTime,
          frameDuration);
    end;

  if Assigned(writer) and SUCCEEDED(Result) and (FFramesWritten > 0) then
    Result := writer.Finalize();

  sample := nil;
  audioState.PendingSample := nil;
  audioType := nil;
  audioReader := nil;
  writer := nil;
  FWriter := nil;
  FReader := nil;

  OutputDebugString(PChar(Format('Export: audio artwork done hr=%.8x frames=%d videoSec=%.3f audioSec=%.3f',
                                 [DWORD(Result), FFramesWritten, outputTime / 10000000.0, audioState.OutputTime / 10000000.0])));
end;


function TMfSubtitleFramePump.BurnSubtitlesToFile(const InputFileName: WideString;
                                                  const OutputFileName: WideString;
                                                  Bitrate: UINT32;
                                                  const OutputByteStream: IMFByteStream;
                                                  UseFragmentedMp4: Boolean;
                                                  StartTime100ns: MFTIME): HRESULT;
var
  reader: IMFSourceReader;
  writer: IMFSinkWriter;
  mediaType: IMFMediaType;
  converter: IMFTransform;
  audioType: IMFMediaType;
  sample: IMFSample;
  rgbSample: IMFSample;
  audioReader: IMFSourceReader;
  streamIndex: DWORD;
  audioStreamIndex: DWORD;
  actualStreamIndex: DWORD;
  flags: DWORD;
  width: UINT32;
  height: UINT32;
  frameRateNum: UINT32;
  frameRateDen: UINT32;
  sampleTime: LONGLONG;
  sampleDuration: LONGLONG;
  frameDuration: LONGLONG;
  outputTime: LONGLONG;
  lastOutputTime: LONGLONG;
  emptyReads: Integer;
  hasAudio: Boolean;
  audioHr: HRESULT;
  audioState: TMfSubtitleAudioState;
  lastReadDebugFrame: Int64;
  lastWriteDebugFrame: Int64;
  writeRetryCount: Integer;
  exportStartTick: DWORD;
  paceTargetMs: Int64;
  paceElapsedMs: Int64;
  paceSleepMs: Int64;
  subtitleTime: MFTIME;
  useSourceSubtitleTime: Boolean;
  useNativeNv12: Boolean;

begin

  FFramesWritten := 0;
  FCancelRequested := False;
  FPauseRequested := False;
  FReader := nil;
  FWriter := nil;
  FStreamIndex := 0;
  reader := nil;
  writer := nil;
  mediaType := nil;
  converter := nil;
  audioType := nil;
  sample := nil;
  rgbSample := nil;
  audioReader := nil;
  streamIndex := 0;
  audioStreamIndex := 0;
  actualStreamIndex := 0;
  flags := 0;
  sampleTime := 0;
  frameDuration := 400000;
  outputTime := 0;
  lastOutputTime := -1;
  emptyReads := 0;
  hasAudio := False;
  audioState.PendingSample := nil;
  audioState.PendingTime := 0;
  audioState.OutputTime := 0;
  audioState.BytesPerSecond := 0;
  audioState.Pending := False;
  audioState.Done := False;
  lastReadDebugFrame := -1;
  lastWriteDebugFrame := -1;

  if (InputFileName = '') or
     ((OutputFileName = '') and (not Assigned(OutputByteStream))) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  OutputDebugString(PChar('Export: creating source reader'));
  Result := CreateReader(InputFileName,
                         reader);
  if FAILED(Result) then
    Exit;
  FReader := reader;

  OutputDebugString(PChar('Export: configuring source reader'));
  useNativeNv12 := SameText(ExtractFileExt(InputFileName),
                            '.mp4');

  Result := ConfigureReader(reader,
                            useNativeNv12,
                            mediaType,
                            width,
                            height,
                            frameRateNum,
                            frameRateDen);
  if FAILED(Result) then
    Exit;

  if useNativeNv12 then
    begin
      OutputDebugString('Export: creating explicit NV12 to RGB32 converter');

      Result := CreateNv12Converter(mediaType,
                                    converter);
      if FAILED(Result) then
        Exit;
    end;

  if (frameRateNum > 0) then
    frameDuration := (Int64(frameRateDen) * 10000000) div Int64(frameRateNum);

  if (frameDuration <= 0) then
    frameDuration := 400000;

  if (StartTime100ns < 0) then
    StartTime100ns := 0;

  // AVI timestamps are commonly unreliable, so retain the stable CFR clock
  // used by the existing export path. Other containers provide the original
  // timestamp of the decoded picture, which is the correct subtitle lookup
  // time even though the cast MP4 itself is rebased to zero.
  useSourceSubtitleTime := not SameText(ExtractFileExt(InputFileName),
                                        '.avi');

  // Do not flush a newly configured H.264 decoder merely to seek to zero.
  // Some MP4 Source Reader decoder chains accept that seek but then fail the
  // first ReadSample call. A non-zero cast restart still requires a real seek.
  if StartTime100ns > 0 then
    begin
      Result := SetReaderPosition(reader,
                                  StartTime100ns);
      if FAILED(Result) then
        Exit;
    end;

  OutputDebugString(PChar('Export: creating sink writer'));

  Result := CreateWriter(OutputFileName,
                         width,
                         height,
                         frameRateNum,
                         frameRateDen,
                         Bitrate,
                         writer,
                         streamIndex,
                         OutputByteStream,
                         UseFragmentedMp4);
  if FAILED(Result) then
    Exit;

  FWriter := writer;
  FStreamIndex := streamIndex;

  audioHr := ConfigureAudioReader(InputFileName,
                                  audioReader,
                                  audioType,
                                  hasAudio);
  if FAILED(audioHr) then
    begin
      OutputDebugString(PChar(Format('Export: audio reader disabled hr=%.8x',
                                     [DWORD(audioHr)])));
      hasAudio := False;
      audioReader := nil;
      audioType := nil;
    end;

  if hasAudio then
    begin
      audioHr := SetReaderPosition(audioReader,
                                   StartTime100ns);
      if FAILED(audioHr) then
        begin
          OutputDebugString(PChar(Format('Export: audio seek disabled hr=%.8x',
                                         [DWORD(audioHr)])));
          hasAudio := False;
          audioReader := nil;
          audioType := nil;
        end;
    end;

  if hasAudio then
    begin
      audioHr := AddAudioStream(writer,
                                audioType,
                                audioStreamIndex);
      if FAILED(audioHr) then
        begin

          OutputDebugString(PChar(Format('Export: audio stream disabled hr=%.8x',
                                         [DWORD(audioHr)])));
          hasAudio := False;
          audioReader := nil;
          audioType := nil;
        end
      else
        begin
          if FAILED(audioType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                        audioState.BytesPerSecond)) or
             (audioState.BytesPerSecond = 0) then
            audioState.BytesPerSecond := 48000 * 2 * 16 div 8;

          OutputDebugString(PChar(Format('Export: audio stream enabled index=%d bytesPerSec=%d',
                                         [audioStreamIndex, audioState.BytesPerSecond])));
        end;
    end;

  Result := writer.BeginWriting();
  if FAILED(Result) then
    Exit;

  exportStartTick := GetTickCount();

  while True do
    begin
      sample := nil;
      flags := 0;
      actualStreamIndex := 0;
      sampleTime := 0;

      if CancelRequested(outputTime) then
        begin
          if (FFramesWritten > 0) then
            Result := S_OK
          else
            Result := E_ABORT;
          Break;
        end;

      if WaitIfPaused(exportStartTick,
                      outputTime) then
        begin
          if (FFramesWritten > 0) then
            Result := S_OK
          else
            Result := E_ABORT;
          Break;
        end;

      if (FFramesWritten <> lastReadDebugFrame) and ((FFramesWritten mod 250) = 0) then
        begin
          lastReadDebugFrame := FFramesWritten;
          OutputDebugString(PChar(Format('Export: before ReadSample frames=%d time=%d', [FFramesWritten, outputTime])));
        end;

      Result := reader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                  0,
                                  @actualStreamIndex,
                                  @flags,
                                  @sampleTime,
                                  @sample);

      if (FFramesWritten = 0) then
        OutputDebugString(PChar(Format('Export: first ReadSample hr=%.8x flags=%.8x sample=%d time=%d',
                                       [DWORD(Result), flags, Ord(Assigned(sample)), sampleTime])));
      if FAILED(Result) then
        Break;

      if CancelRequested(outputTime) then
        begin
          if (FFramesWritten > 0) then
            Result := S_OK
          else
            Result := E_ABORT;
          Break;
        end;

      if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
        Break;

      if ((flags and MF_SOURCE_READERF_ERROR) <> 0) or
         ((flags and MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) <> 0) then
        begin
          Result := E_FAIL;
          Break;
        end;

      if ((flags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0) then
        begin
          mediaType := nil;
          Result := reader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                               @mediaType);
          if FAILED(Result) then
            Break;

          if useNativeNv12 then
            begin
              converter := nil;

              Result := CreateNv12Converter(mediaType,
                                            converter);

              OutputDebugString(PChar(Format('Export: refreshed NV12 converter hr=%.8x',
                                             [DWORD(Result)])));

              if FAILED(Result) then
                Break;

            end;
        end;

      if ((flags and MF_SOURCE_READERF_STREAMTICK) <> 0) or
         (not Assigned(sample)) then
        begin
          Inc(emptyReads);

          if (emptyReads > 1000) then
            begin
              Result := E_FAIL;
              Break;
            end;

          Sleep(1);
          Continue;
        end;

      emptyReads := 0;

      if useNativeNv12 then
        begin
          rgbSample := nil;
          Result := ConvertNv12Sample(converter,
                                      sample,
                                      width,
                                      height,
                                      rgbSample);
          if (FFramesWritten = 0) then
            OutputDebugString(PChar(Format('Export: first NV12 conversion hr=%.8x sample=%d',
                                           [DWORD(Result), Ord(Assigned(rgbSample))])));
          if FAILED(Result) then
            Break;

          sample := rgbSample;
        end;

      // AVI files often have sparse, repeated, or otherwise unreliable frame
      // timestamps. Generate a stable CFR export timeline from the configured
      // frame rate so audio/video muxing and subtitle timing stay coherent.
      outputTime := FFramesWritten * frameDuration;
      lastOutputTime := outputTime;

      if WaitIfPaused(exportStartTick,
                      outputTime) then
        Break;

      if FRealTimePacing then
        while not CancelRequested(outputTime) do
          begin
            // Keep the decoded preview video on the same wall clock as its
            // local PCM monitor. Cast buffering is handled by the fragmented
            // MP4 cadence rather than by running the preview ahead of audio.
            paceTargetMs := outputTime div 10000;
            paceElapsedMs := Int64(DWORD(GetTickCount() - exportStartTick));

            if (paceElapsedMs + 5 >= paceTargetMs) then
              Break;

            paceSleepMs := paceTargetMs - paceElapsedMs;

            if (paceSleepMs > 25) then
              paceSleepMs := 25;

            Sleep(DWORD(paceSleepMs));
          end;

      if CancelRequested(outputTime) then
        begin
          if (FFramesWritten > 0) then
            Result := S_OK
          else
            Result := E_ABORT;
          Break;
        end;

      Result := sample.SetSampleTime(outputTime);
      if FAILED(Result) then
        Break;

      if useSourceSubtitleTime and (sampleTime >= 0) then
        subtitleTime := sampleTime
      else
        subtitleTime := StartTime100ns + outputTime;

      if (FFramesWritten = 0) then
        OutputDebugString(PChar(Format(
          'Export: first frame source=%d output=%d subtitle=%d requested=%d',
          [sampleTime, outputTime, subtitleTime, StartTime100ns])));

      Result := CompositeSample(sample,
                                width,
                                height,
                                subtitleTime);
      if FAILED(Result) then
        Break;

      sampleDuration := frameDuration;
      Result := sample.SetSampleDuration(sampleDuration);
      if FAILED(Result) then
        Break;

      if Assigned(FOnVideoSample) then
        FOnVideoSample(Self,
                       sample,
                       width,
                       height,
                       outputTime,
                       sampleDuration);

      if (FFramesWritten <> lastWriteDebugFrame) and ((FFramesWritten mod 250) = 0) then
        begin
          lastWriteDebugFrame := FFramesWritten;
          OutputDebugString(PChar(Format('Export: before WriteSample frames=%d time=%d',
                                          [FFramesWritten, outputTime])));
        end;

      writeRetryCount := 0;

      repeat
        Result := writer.WriteSample(streamIndex,
                                     sample);
        if (Result <> E_OUTOFMEMORY) then
          Break;

        Inc(writeRetryCount);

        if (writeRetryCount = 1) or ((writeRetryCount mod 20) = 0) then
          OutputDebugString(PChar(Format('Export: video WriteSample waiting for encoder frames=%d retries=%d',
                                         [FFramesWritten, writeRetryCount])));
        Sleep(25);
      until (writeRetryCount >= 200) or CancelRequested(outputTime);

      if FAILED(Result) then
        Break;

      if hasAudio and Assigned(audioReader) then
        begin
          audioHr := WriteAudioSamples(audioReader,
                                       writer,
                                       audioStreamIndex,
                                       audioState,
                                       outputTime + sampleDuration);
          if FAILED(audioHr) then
            begin
              Result := audioHr;
              Break;
            end;
        end;

      Inc(FFramesWritten);
      if CancelRequested(outputTime) then
        begin
          if (FFramesWritten > 0) then
            Result := S_OK
          else
            Result := E_ABORT;
          Break;
        end;
    end;

  if Assigned(writer) and
     SUCCEEDED(Result) and
     hasAudio and
     Assigned(audioReader) and
     (FFramesWritten > 0) then
    begin
      OutputDebugString(PChar(Format('Export: before audio copy stop=%d',
                                     [lastOutputTime + frameDuration])));

      audioHr := WriteAudioSamples(audioReader,
                                   writer,
                                   audioStreamIndex,
                                   audioState,
                                   lastOutputTime + frameDuration);

      OutputDebugString(PChar(Format('Export: after audio copy hr=%.8x',
                                     [DWORD(audioHr)])));
    end;

  if Assigned(writer) and SUCCEEDED(Result) and (FFramesWritten > 0) then
    begin

      OutputDebugString(PChar(Format('Export: before Finalize cancel=%d',
                                     [Integer(FCancelRequested)])));
      Result := writer.Finalize();
      OutputDebugString(PChar(Format('Export: after Finalize hr=%.8x',
                                     [DWORD(Result)])));
    end
  else
    OutputDebugString(PChar(Format('Export: skipping Finalize hr=%.8x cancel=%d frames=%d',
                                   [DWORD(Result), Integer(FCancelRequested), FFramesWritten])));

  sample := nil;
  audioState.PendingSample := nil;
  audioType := nil;
  mediaType := nil;
  audioReader := nil;
  writer := nil;
  reader := nil;
  FWriter := nil;
  FReader := nil;

  OutputDebugString(PChar(Format('Export: done hr=%.8x frames=%d videoSec=%.3f audioSec=%.3f audioBps=%d',
                                 [DWORD(Result), FFramesWritten, (lastOutputTime + frameDuration) / 10000000.0, audioState.OutputTime / 10000000.0, audioState.BytesPerSecond])));
end;

end.
