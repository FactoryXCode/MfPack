// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastDesktopCapture.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: MfPack Cast V2.1 DXGI desktop capture and fixed-codec fMP4 encoder.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release SDK 10.0.28000.2705 (Windows 11)
// 12/09/2026 All                 Use DXGI 1.6 output discovery and DXGI 1.5
//                                DuplicateOutput1 desktop capture.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 11 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfCastDesktopCapture;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SyncObjs,
  {Cast}
  MfCastTypes,
  MfCastInterfaces,
  MfCastMediaInterfaces;

type
  TMfCastDesktopCapturePipeline = class(TInterfacedObject,
                                        IMfCastCapturePipeline)
  private
    FSettings: TMfCastCaptureSettings;
    FVideoSubtype: TGUID;
    FPublisher: IMfCastSegmentPublisher;
    FPreviewSink: IMfCastPreviewSink;
    FLogger: IMfCastLogger;
    FWorker: TThread;
    FState: TMfCastState;
    FWorkerResult: HRESULT;
    FStartupEvent: TEvent;
    FStartupResult: HRESULT;
    FStartupReady: Boolean;

    procedure Log(const ALevel: TMfCastLogLevel;
                  const AMessage: string);
    procedure SetState(const AState: TMfCastState);
    procedure SignalStartup(const AResult: HRESULT);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetLogger(const ALogger: IMfCastLogger);
    function Start(const ASettings: TMfCastCaptureSettings;
                   const AVideoSubtype: TGUID;
                   const APublisher: IMfCastSegmentPublisher;
                   const APreviewSink: IMfCastPreviewSink): HRESULT;
    function Stop(): HRESULT;
    function GetState(): TMfCastState;
  end;


implementation

uses

  {WinApi}
  WinApi.ActiveX,
  WinApi.WinError,
  {System}
  System.SysUtils,
  {DirectX}
  WinApi.DirectX.D3DCommon,
  WinApi.DirectX.D3D11,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGI1_5,
  WinApi.DirectX.DXGI1_6,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.WinMM.MMeApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform;

const
  // MF_SINK_WRITER_D3D_MANAGER, absent from some Delphi SDK translations.
  MFCAST_SINK_WRITER_D3D_MANAGER: TGUID = (D1: $EC822DA2;
    D2: $E1E9; D3: $4B29; D4: ($A0, $D8, $56, $3C, $71, $9F, $52, $69));

type
  TMfCastDesktopCaptureWorker = class(TThread)
  private
    FOwner: TMfCastDesktopCapturePipeline;

    function CreateWriter(const AByteStream: IMFByteStream;
                          const AWidth: UINT32;
                          const AHeight: UINT32;
                          const AWaveFormat: PWaveFormatEx;
                          const AVideoInputSubtype: TGUID;
                          const ADxgiDeviceManager: IMFDXGIDeviceManager;
                          out AWriter: IMFSinkWriter;
                          out AVideoStreamIndex: DWORD;
                          out AAudioStreamIndex: DWORD): HRESULT;

    function WriteFrame(const AWriter: IMFSinkWriter;
                        const AStreamIndex: DWORD;
                        const AData: TBytes;
                        const ATime: Int64;
                        const ADuration: Int64): HRESULT;

    function PresentPreviewFrame(const AData: TBytes;
                                 const AWidth: UINT32;
                                 const AHeight: UINT32;
                                 const ATime: Int64;
                                 const ADuration: Int64): HRESULT;

    function WriteAudioPacket(const AWriter: IMFSinkWriter;
                              const AStreamIndex: DWORD;
                              const AData: PByte;
                              const AByteCount: DWORD;
                              const ASilent: Boolean;
                              const ATime: Int64;
                              const ADuration: Int64): HRESULT;

    function RunCapture(): HRESULT;

  protected
    procedure Execute(); override;

  public

    constructor Create(AOwner: TMfCastDesktopCapturePipeline);
  end;


function MfCastPerformanceDelta100ns(const ACounterDelta: Int64;
                                     const AFrequency: Int64): Int64;
begin
  if (ACounterDelta <= 0) or (AFrequency <= 0) then
    Exit(0);

  // Split the calculation to avoid overflowing Int64 on long-running casts.
  Result := (ACounterDelta div AFrequency) * 10000000 +
            ((ACounterDelta mod AFrequency) * 10000000) div AFrequency;
end;


constructor TMfCastDesktopCaptureWorker.Create(AOwner: TMfCastDesktopCapturePipeline);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  Priority := tpHigher;
  FOwner := AOwner;
end;


function TMfCastDesktopCaptureWorker.CreateWriter(const AByteStream: IMFByteStream;
                                                   const AWidth: UINT32;
                                                   const AHeight: UINT32;
                                                   const AWaveFormat: PWaveFormatEx;
                                                   const AVideoInputSubtype: TGUID;
                                                   const ADxgiDeviceManager: IMFDXGIDeviceManager;
                                                  out AWriter: IMFSinkWriter;
                                                  out AVideoStreamIndex: DWORD;
                                                  out AAudioStreamIndex: DWORD): HRESULT;
var
  Attributes: IMFAttributes;
  OutputType: IMFMediaType;
  InputType: IMFMediaType;
  FrameRateNumerator: UINT32;
  FrameRateDenominator: UINT32;
  KeyFrameSpacing: UINT32;
  Bitrate: UINT32;
  AudioBitrate: UINT32;

begin

  AWriter := nil;
  AVideoStreamIndex := DWORD(-1);
  AAudioStreamIndex := DWORD(-1);
  Attributes := nil;
  OutputType := nil;
  InputType := nil;

  Result := MFCreateAttributes(Attributes,
                                5);
  if FAILED(Result) then
    Exit;

  Result := Attributes.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                                MFTranscodeContainerType_FMPEG4);
  if FAILED(Result) then
    Exit;

  // Do not let the fragmented-MP4 sink follow a multi-second encoder GOP.
  // Short, regular fragments keep audio and video arriving at the receiver
  // together instead of allowing one track to be presented a fragment behind.
  Result := Attributes.SetUINT64(MF_MPEG4SINK_MIN_FRAGMENT_DURATION,
                                  UInt64(10000000));
  if FAILED(Result) then
    Exit;

  // Keep Sink Writer backpressure enabled. The hardware encoder is
  // asynchronous; disabling throttling lets unbounded GPU samples accumulate.
  Result := Attributes.SetUINT32(MF_LOW_LATENCY,
                                  1);
  if FAILED(Result) then Exit;

  Result := Attributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
                                  1);
  if FAILED(Result) then Exit;

  if Assigned(ADxgiDeviceManager) then
    begin
      // Not declared by older Delphi SDK headers.
      Result := Attributes.SetUnknown(MFCAST_SINK_WRITER_D3D_MANAGER,
                                      ADxgiDeviceManager);
      if FAILED(Result) then Exit;
    end;

  Result := MFCreateSinkWriterFromURL(PWideChar(WideString('mfcast-capture.mp4')),
                                      AByteStream,
                                      Attributes,
                                      AWriter);
  if FAILED(Result) then
    Exit;

  FrameRateNumerator := FOwner.FSettings.FrameRateNumerator;
  FrameRateDenominator := FOwner.FSettings.FrameRateDenominator;

  if FrameRateNumerator = 0 then
    FrameRateNumerator := 30;

  if FrameRateDenominator = 0 then
    FrameRateDenominator := 1;

  Bitrate := FOwner.FSettings.VideoBitrate;

  if (Bitrate = 0) then
    Bitrate := 4000000;

  FOwner.Log(cllInfo,
             Format('Desktop H.264/HEVC encoder target bitrate: %d bps.',
                    [Bitrate]));

  AudioBitrate := FOwner.FSettings.AudioBitrate;
  if AudioBitrate = 0 then
    AudioBitrate := 192000;

  Result := MFCreateMediaType(OutputType);
  if FAILED(Result) then
    Exit;

  Result := OutputType.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := OutputType.SetGUID(MF_MT_SUBTYPE,
                               FOwner.FVideoSubtype);
  if FAILED(Result) then
    Exit;

  Result := OutputType.SetUINT32(MF_MT_AVG_BITRATE,
                                 Bitrate);
  if FAILED(Result) then
    Exit;

  Result := OutputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeSize(OutputType,
                               MF_MT_FRAME_SIZE,
                               AWidth,
                               AHeight);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(OutputType,
                                MF_MT_FRAME_RATE,
                                FrameRateNumerator,
                                FrameRateDenominator);
  if FAILED(Result) then
    Exit;

  KeyFrameSpacing := FrameRateNumerator div FrameRateDenominator;
  if (FrameRateNumerator mod FrameRateDenominator) <> 0 then
    Inc(KeyFrameSpacing);
  if KeyFrameSpacing = 0 then
    KeyFrameSpacing := 1;

  // A fragmented MP4 segment can only begin independently at a keyframe.
  // Match the GOP to the one-second fragment cadence used for live casting.
  Result := OutputType.SetUINT32(MF_MT_MAX_KEYFRAME_SPACING,
                                 KeyFrameSpacing);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(OutputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
  if FAILED(Result) then
    Exit;

  Result := AWriter.AddStream(OutputType,
                              AVideoStreamIndex);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(InputType);
  if FAILED(Result) then
    Exit;

  Result := InputType.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := InputType.SetGUID(MF_MT_SUBTYPE,
                               AVideoInputSubtype);
  if FAILED(Result) then
    Exit;

  Result := InputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeSize(InputType,
                               MF_MT_FRAME_SIZE,
                               AWidth,
                               AHeight);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(InputType,
                                MF_MT_FRAME_RATE,
                                FrameRateNumerator,
                                FrameRateDenominator);
  if FAILED(Result) then
    Exit;

  Result := MFSetAttributeRatio(InputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
  if FAILED(Result) then
    Exit;

  if IsEqualGUID(AVideoInputSubtype, MFVideoFormat_NV12) then
    begin
      Result := InputType.SetUINT32(MF_MT_DEFAULT_STRIDE, AWidth);
      if FAILED(Result) then Exit;
      Result := InputType.SetUINT32(MF_MT_FIXED_SIZE_SAMPLES, 1);
      if FAILED(Result) then Exit;
      Result := InputType.SetUINT32(MF_MT_SAMPLE_SIZE,
                                    (AWidth * AHeight * 3) div 2);
      if FAILED(Result) then Exit;
    end;

  Result := AWriter.SetInputMediaType(AVideoStreamIndex,
                                      InputType,
                                      nil);
  if FAILED(Result) then
    Exit;

  if Assigned(AWaveFormat) then
    begin
      OutputType := nil;
      Result := MFCreateMediaType(OutputType);
      if FAILED(Result) then Exit;
      Result := OutputType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
      if FAILED(Result) then Exit;
      Result := OutputType.SetGUID(MF_MT_SUBTYPE, MFAudioFormat_AAC);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, 2);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, 48000);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                     AudioBitrate div 8);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AAC_PAYLOAD_TYPE, 0);
      if FAILED(Result) then Exit;
      Result := OutputType.SetUINT32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                     $29);
      if FAILED(Result) then Exit;
      Result := AWriter.AddStream(OutputType, AAudioStreamIndex);
      if FAILED(Result) then Exit;

      InputType := nil;
      Result := MFCreateMediaType(InputType);
      if FAILED(Result) then Exit;
      Result := MFInitMediaTypeFromWaveFormatEx(InputType,
                                                AWaveFormat,
                                                SizeOf(WAVEFORMATEX) +
                                                AWaveFormat^.cbSize);
      if FAILED(Result) then Exit;
      Result := AWriter.SetInputMediaType(AAudioStreamIndex,
                                          InputType,
                                          nil);
      if FAILED(Result) then Exit;
    end;

  Result := AWriter.BeginWriting();
end;


function TMfCastDesktopCaptureWorker.WriteFrame(const AWriter: IMFSinkWriter;
                                                const AStreamIndex: DWORD;
                                                const AData: TBytes;
                                                const ATime: Int64;
                                                const ADuration: Int64): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaximumLength: DWORD;
  CurrentLength: DWORD;

begin

  Result := MFCreateSample(Sample);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMemoryBuffer(Length(AData),
                                 Buffer);
  if FAILED(Result) then
    Exit;

  Result := Buffer.Lock(Data,
                        @MaximumLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    if (Length(AData) > 0) then
      Move(AData[0],
           Data^,
           Length(AData));

  finally
    Buffer.Unlock();
  end;

  Result := Buffer.SetCurrentLength(Length(AData));
  if FAILED(Result) then
    Exit;

  Result := Sample.AddBuffer(Buffer);
  if FAILED(Result) then
    Exit;

  Result := Sample.SetSampleTime(ATime);
  if FAILED(Result) then
    Exit;

  Result := Sample.SetSampleDuration(ADuration);
  if FAILED(Result) then
    Exit;

  Result := AWriter.WriteSample(AStreamIndex,
                                Sample);
end;


function TMfCastDesktopCaptureWorker.WriteAudioPacket(
  const AWriter: IMFSinkWriter;
  const AStreamIndex: DWORD;
  const AData: PByte;
  const AByteCount: DWORD;
  const ASilent: Boolean;
  const ATime: Int64;
  const ADuration: Int64): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  Destination: PByte;
  MaximumLength: DWORD;
  CurrentLength: DWORD;

begin
  if AByteCount = 0 then
    Exit(S_FALSE);

  Result := MFCreateSample(Sample);
  if FAILED(Result) then Exit;
  Result := MFCreateMemoryBuffer(AByteCount, Buffer);
  if FAILED(Result) then Exit;
  Result := Buffer.Lock(Destination, @MaximumLength, @CurrentLength);
  if FAILED(Result) then Exit;
  try
    if ASilent then
      FillChar(Destination^, AByteCount, 0)
    else if Assigned(AData) then
      Move(AData^, Destination^, AByteCount);
  finally
    Buffer.Unlock();
  end;
  Result := Buffer.SetCurrentLength(AByteCount);
  if FAILED(Result) then Exit;
  Result := Sample.AddBuffer(Buffer);
  if FAILED(Result) then Exit;
  Result := Sample.SetSampleTime(ATime);
  if FAILED(Result) then Exit;
  Result := Sample.SetSampleDuration(ADuration);
  if FAILED(Result) then Exit;
  Result := AWriter.WriteSample(AStreamIndex, Sample);
end;


function TMfCastDesktopCaptureWorker.PresentPreviewFrame(const AData: TBytes;
                                                         const AWidth: UINT32;
                                                         const AHeight: UINT32;
                                                         const ATime: Int64;
                                                         const ADuration: Int64): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaximumLength: DWORD;
  CurrentLength: DWORD;

begin

  if not Assigned(FOwner.FPreviewSink) then
    Exit(S_FALSE);

  Result := MFCreateSample(Sample);
  if FAILED(Result) then Exit;
  Result := MFCreateMemoryBuffer(Length(AData), Buffer);
  if FAILED(Result) then Exit;
  Result := Buffer.Lock(Data, @MaximumLength, @CurrentLength);
  if FAILED(Result) then Exit;
  try
    if Length(AData) > 0 then
      Move(AData[0], Data^, Length(AData));
  finally
    Buffer.Unlock();
  end;

  Result := Buffer.SetCurrentLength(Length(AData));
  if FAILED(Result) then Exit;
  Result := Sample.AddBuffer(Buffer);
  if FAILED(Result) then Exit;
  Result := Sample.SetSampleTime(ATime);
  if FAILED(Result) then Exit;
  Result := Sample.SetSampleDuration(ADuration);
  if FAILED(Result) then Exit;

  Result := FOwner.FPreviewSink.PresentSample(Sample,
                                              ATime,
                                              ADuration);
end;


function TMfCastDesktopCaptureWorker.RunCapture(): HRESULT;
const
  // Keep a sparse video timeline alive when the desktop is completely static,
  // without converting static content into an artificial 30-fps sequence.
  MFCAST_VIDEO_KEEPALIVE_100NS = Int64(1000000); // 100 ms
  MFCAST_VIDEO_SCHEDULER_TOLERANCE_100NS = Int64(20000); // 2 ms
  MFCAST_PREVIEW_INTERVAL_100NS = Int64(1000000); // 10 fps
var
  ByteStream: IMFByteStream;
  Device: ID3D11Device;
  Context: ID3D11DeviceContext;
  VideoDevice: ID3D11VideoDevice;
  VideoContext: ID3D11VideoContext;
  VideoEnumerator: ID3D11VideoProcessorEnumerator;
  VideoProcessor: ID3D11VideoProcessor;
  VideoInputView: ID3D11VideoProcessorInputView;
  VideoOutputView: ID3D11VideoProcessorOutputView;
  VideoSampleAllocator: IMFVideoSampleAllocatorEx;
  AllocatorAttributes: IMFAttributes;
  AllocatorMediaType: IMFMediaType;
  GpuSample: IMFSample;
  GpuBuffer: IMFMediaBuffer;
  GpuDxgiBuffer: IMFDXGIBuffer;
  GpuBuffer2D: IMF2DBuffer;
  GpuBufferLength: DWORD;
  DxgiDeviceManager: IMFDXGIDeviceManager;
  DxgiResetToken: UINT;
  DxgiDevice: IDXGIDevice;
  Adapter: IDXGIAdapter;
  Output: IDXGIOutput;
  Output1: IDXGIOutput1;
  Output6: IDXGIOutput6;
  OutputDescription1: DXGI_OUTPUT_DESC1;
  SupportedCaptureFormats: array[0..0] of DXGI_FORMAT;
  Duplication: IDXGIOutputDuplication;
  Resource: IDXGIResource;
  DesktopTexture: ID3D11Texture2D;
  StagingTexture: ID3D11Texture2D;
  Nv12StagingTexture: ID3D11Texture2D;
  GpuFrameTexture: ID3D11Texture2D;
  Description: D3D11_TEXTURE2D_DESC;
  StagingDescription: D3D11_TEXTURE2D_DESC;
  VideoContentDescription: D3D11_VIDEO_PROCESSOR_CONTENT_DESC;
  VideoInputViewDescription: D3D11_VIDEO_PROCESSOR_INPUT_VIEW_DESC;
  VideoOutputViewDescription: D3D11_VIDEO_PROCESSOR_OUTPUT_VIEW_DESC;
  VideoStream: D3D11_VIDEO_PROCESSOR_STREAM;
  VideoFormatFlags: UINT;
  VideoProcessorFrame: UINT;
  SourceBox: D3D11_BOX;
  Mapped: D3D11_MAPPED_SUBRESOURCE;
  FrameInfo: DXGI_OUTDUPL_FRAME_INFO;
  FeatureLevels: array[0..1] of D3D_FEATURE_LEVEL;
  FeatureLevel: D3D_FEATURE_LEVEL;
  Writer: IMFSinkWriter;
  StreamIndex: DWORD;
  AudioStreamIndex: DWORD;
  DeviceEnumerator: IMMDeviceEnumerator;
  AudioDevice: IMMDevice;
  AudioClient: IAudioClient;
  CaptureClient: IAudioCaptureClient;
  WaveFormat: PWaveFormatEx;
  AudioData: PByte;
  PacketFrames: UINT32;
  AudioFrames: UINT32;
  AudioFlags: AUDCLNT_BUFFERFLAGS;
  AudioBytes: DWORD;
  AudioTime: Int64;
  AudioDuration: Int64;
  AudioPacketTime: Int64;
  AudioDevicePosition: UINT64;
  AudioQpcPosition: UINT64;
  AudioStarted: Boolean;
  AudioTimelineStarted: Boolean;
  Frame: TBytes;
  PreviewFrame: TBytes;
  Width: UINT32;
  Height: UINT32;
  Row: UINT32;
  FrameRateNumerator: UINT32;
  FrameRateDenominator: UINT32;
  FrameDuration: Int64;
  FramePresentationTime: Int64;
  LastVideoTime: Int64;
  LastPreviewTime: Int64;
  FirstVideoTime: Int64;
  FirstAudioTime: Int64;
  TimelineLogTick: Cardinal;
  FramePresentCounter: Int64;
  ElapsedTime: Int64;
  PerformanceFrequency: Int64;
  CaptureStartCounter: Int64;
  CurrentCounter: Int64;
  CaptureStartQpc100ns: Int64;
  MissedDesktopFrames: Int64;
  FrameHeld: Boolean;
  FrameUpdated: Boolean;
  ShouldCaptureFrame: Boolean;
  ShouldPresentPreview: Boolean;
  PreviewFrameUpdated: Boolean;
  UseGpuSurfaces: Boolean;
  WriterUsesD3DManager: Boolean;
  MappedTexture: Boolean;
  Hr: HRESULT;

begin
  WaveFormat := nil;
  AudioStarted := False;
  AudioTimelineStarted := False;
  FOwner.Log(cllDebug, 'Capture worker entered RunCapture.');
  try
  Result := FOwner.FPublisher.GetByteStream(ByteStream);
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllDebug, 'Live MP4 byte stream acquired.');

  FeatureLevels[0] := D3D_FEATURE_LEVEL_11_0;
  FeatureLevels[1] := D3D_FEATURE_LEVEL_10_0;

  Result := D3D11CreateDevice(nil,
                              D3D_DRIVER_TYPE_HARDWARE,
                              0,
                              D3D11_CREATE_DEVICE_BGRA_SUPPORT,
                              @FeatureLevels[0],
                              Length(FeatureLevels),
                              D3D11_SDK_VERSION,
                              @Device,
                              @FeatureLevel,
                              @Context);
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllDebug, 'D3D11 hardware device created.');

  // MfCaptureVideoFromGPU II keeps DXGI capture/conversion private and feeds
  // contiguous NV12 to Media Foundation. No MF DXGI device manager is shared
  // with the encoder or with any file-casting pipeline.
  DxgiDeviceManager := nil;
  UseGpuSurfaces := True;

  FOwner.Log(cllDebug, 'Querying IDXGIDevice.');
  Result := Device.QueryInterface(IID_IDXGIDevice,
                                  DxgiDevice);
  FOwner.Log(cllDebug,
    Format('IDXGIDevice query returned HRESULT $%.8x.', [Cardinal(Result)]));
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllDebug, 'Resolving the D3D11 adapter.');
  Result := DxgiDevice.GetAdapter(Adapter);
  FOwner.Log(cllDebug,
    Format('IDXGIDevice.GetAdapter returned HRESULT $%.8x.', [Cardinal(Result)]));
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllDebug,
    Format('Resolving DXGI output index %d.', [FOwner.FSettings.OutputIndex]));
  Result := Adapter.EnumOutputs(FOwner.FSettings.OutputIndex,
                                Output);
  FOwner.Log(cllDebug,
    Format('IDXGIAdapter.EnumOutputs returned HRESULT $%.8x.', [Cardinal(Result)]));
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllDebug, 'Querying IDXGIOutput6.');
  Result := Output.QueryInterface(IID_IDXGIOutput6,
                                  Output6);
  FOwner.Log(cllDebug,
    Format('IDXGIOutput6 query returned HRESULT $%.8x.', [Cardinal(Result)]));
  if FAILED(Result) then
    Exit;

  ZeroMemory(@OutputDescription1,
             SizeOf(OutputDescription1));
  FOwner.Log(cllDebug, 'Reading DXGI output description.');
  Result := Output6.GetDesc1(OutputDescription1);
  FOwner.Log(cllDebug,
    Format('IDXGIOutput6.GetDesc1 returned HRESULT $%.8x.', [Cardinal(Result)]));
  if FAILED(Result) then
    Exit;

  // The current preview, CPU fallback and Media Foundation input path all
  // consume 32-bit BGRA. DuplicateOutput1 can therefore avoid an unnecessary
  // fullscreen conversion when BGRA is already available, while guaranteeing
  // the same texture layout for SDR and HDR desktops. Native 10/16-bit capture
  // can be enabled later together with explicit HDR-to-SDR tone mapping.
  SupportedCaptureFormats[0] := DXGI_FORMAT_B8G8R8A8_UNORM;
  FOwner.Log(cllDebug, 'Creating DuplicateOutput1 desktop duplication.');
  Result := Output6.DuplicateOutput1(Device,
                                     0,
                                     Length(SupportedCaptureFormats),
                                     @SupportedCaptureFormats[0],
                                     Duplication);
  FOwner.Log(cllDebug,
    Format('IDXGIOutput5.DuplicateOutput1 returned HRESULT $%.8x.',
           [Cardinal(Result)]));

  if Result = DXGI_ERROR_UNSUPPORTED then
    begin
      // DuplicateOutput1 is optional for some active desktop modes even when
      // IDXGIOutput5/6 is exposed. The original API converts the duplicated
      // surface to BGRA8, which is precisely the format consumed below.
      FOwner.Log(cllWarning,
        'DuplicateOutput1 does not support this desktop mode; falling back to IDXGIOutput1.DuplicateOutput (BGRA8).');
      Duplication := nil;
      Result := Output.QueryInterface(IID_IDXGIOutput1,
                                      Output1);
      if SUCCEEDED(Result) then
        Result := Output1.DuplicateOutput(Device,
                                          Duplication);
      FOwner.Log(cllDebug,
        Format('IDXGIOutput1.DuplicateOutput fallback returned HRESULT $%.8x.',
               [Cardinal(Result)]));
    end;
  if FAILED(Result) then
    Exit;

  FOwner.Log(cllInfo,
    Format('DXGI 1.6 output selected: %d bits per colour, colour space %d; desktop duplication uses DXGI 1.5 DuplicateOutput1 (BGRA8).',
           [OutputDescription1.BitsPerColor,
            Ord(OutputDescription1.ColorSpace)]));

  if FOwner.FSettings.CaptureSystemAudio then
    begin
      FOwner.Log(cllDebug, 'Initializing the private WASAPI loopback client.');
      Result := CoCreateInstance(CLSID_MMDeviceEnumerator,
                                 nil,
                                 CLSCTX_INPROC_SERVER,
                                 IID_IMMDeviceEnumerator,
                                 DeviceEnumerator);
      if FAILED(Result) then Exit;

      Result := DeviceEnumerator.GetDefaultAudioEndpoint(eRender,
                                                          eMultimedia,
                                                          AudioDevice);
      if FAILED(Result) then Exit;

      Result := AudioDevice.Activate(IID_IAudioClient,
                                     CLSCTX_INPROC_SERVER,
                                     nil,
                                     Pointer(AudioClient));
      if FAILED(Result) then Exit;

      Result := AudioClient.GetMixFormat(WaveFormat);
      if FAILED(Result) then Exit;

      Result := AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                       AUDCLNT_STREAMFLAGS_LOOPBACK,
                                       1000000,
                                       0,
                                       WaveFormat,
                                       nil);
      if FAILED(Result) then Exit;

      Result := AudioClient.GetService(IID_IAudioCaptureClient,
                                       CaptureClient);
      if FAILED(Result) then Exit;

      FOwner.Log(cllDebug, 'Private WASAPI loopback client initialized.');

    end;

  // Follow MfCaptureVideoFromGPU II and take the dimensions from the selected
  // output rectangle. Querying the duplication descriptor here proved to be a
  // driver-dependent blocking point on this system.
  Width := UINT32(OutputDescription1.DesktopCoordinates.Right -
                  OutputDescription1.DesktopCoordinates.Left);
  Height := UINT32(OutputDescription1.DesktopCoordinates.Bottom -
                   OutputDescription1.DesktopCoordinates.Top);
  FOwner.Log(cllDebug,
             Format('Selected desktop dimensions resolved: %dx%d.',
                    [Width, Height]));

  ZeroMemory(@Description,
             SizeOf(Description));
  Description.Width := Width;
  Description.Height := Height;
  Description.MipLevels := 1;
  Description.ArraySize := 1;
  Description.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Description.SampleDesc.Count := 1;
  Description.SampleDesc.Quality := 0;
  Description.Usage := D3D11_USAGE_DEFAULT;

  if (FOwner.FSettings.Width > 0) and (FOwner.FSettings.Width < Width) then
    Width := FOwner.FSettings.Width;

  if (FOwner.FSettings.Height > 0) and (FOwner.FSettings.Height < Height) then
    Height := FOwner.FSettings.Height;

  Width := Width and not UINT32(1);
  Height := Height and not UINT32(1);
  if (Width = 0) or (Height = 0) then
    Exit(E_INVALIDARG);

  FrameRateNumerator := FOwner.FSettings.FrameRateNumerator;
  FrameRateDenominator := FOwner.FSettings.FrameRateDenominator;
  if FrameRateNumerator = 0 then FrameRateNumerator := 30;
  if FrameRateDenominator = 0 then FrameRateDenominator := 1;

  // Hardware H.264 encoders generally reject RGB32 DXGI surfaces. Convert the
  // desktop BGRA texture to an NV12 texture with the D3D11 video processor so
  // the frame remains on the GPU from duplication through encoding.
  Hr := S_OK;
  if UseGpuSurfaces then
    begin
      FOwner.Log(cllDebug,
                 'Creating the private D3D11 BGRA-to-NV12 staging converter.');
      Hr := Device.QueryInterface(IID_ID3D11VideoDevice,
                                  VideoDevice);
      if SUCCEEDED(Hr) then
        Hr := Context.QueryInterface(IID_ID3D11VideoContext,
                                     VideoContext);

      if SUCCEEDED(Hr) then
        begin
          ZeroMemory(@VideoContentDescription,
                     SizeOf(VideoContentDescription));
          VideoContentDescription.InputFrameFormat :=
            D3D11_VIDEO_FRAME_FORMAT_PROGRESSIVE;
          VideoContentDescription.InputFrameRate.Numerator := FrameRateNumerator;
          VideoContentDescription.InputFrameRate.Denominator := FrameRateDenominator;
          VideoContentDescription.InputWidth := Width;
          VideoContentDescription.InputHeight := Height;
          VideoContentDescription.OutputFrameRate :=
            VideoContentDescription.InputFrameRate;
          VideoContentDescription.OutputWidth := Width;
          VideoContentDescription.OutputHeight := Height;
          VideoContentDescription.Usage := D3D11_VIDEO_USAGE_OPTIMAL_SPEED;
          Hr := VideoDevice.CreateVideoProcessorEnumerator(
            @VideoContentDescription,
            VideoEnumerator);
        end;

      if SUCCEEDED(Hr) then
        begin
          Hr := VideoEnumerator.CheckVideoProcessorFormat(Description.Format,
                                                           VideoFormatFlags);
          if SUCCEEDED(Hr) and
             ((VideoFormatFlags and
               UINT(D3D11_VIDEO_PROCESSOR_FORMAT_SUPPORT_INPUT)) = 0) then
            Hr := E_NOTIMPL;
        end;
      if SUCCEEDED(Hr) then
        begin
          Hr := VideoEnumerator.CheckVideoProcessorFormat(DXGI_FORMAT_NV12,
                                                           VideoFormatFlags);
          if SUCCEEDED(Hr) and
             ((VideoFormatFlags and
               UINT(D3D11_VIDEO_PROCESSOR_FORMAT_SUPPORT_OUTPUT)) = 0) then
            Hr := E_NOTIMPL;
        end;
      if SUCCEEDED(Hr) then
        Hr := VideoDevice.CreateVideoProcessor(VideoEnumerator,
                                               0,
                                               VideoProcessor);

      ZeroMemory(@VideoOutputViewDescription,
                 SizeOf(VideoOutputViewDescription));
      VideoOutputViewDescription.ViewDimension :=
        D3D11_VPOV_DIMENSION_TEXTURE2D;
      VideoOutputViewDescription.Texture2D.MipSlice := 0;

      // Follow MfCaptureVideoFromGPU II: conversion owns its textures and the
      // Sink Writer receives ordinary contiguous NV12 samples. Do not give the
      // file/capture writer an MF DXGI sample allocator or shared GPU surfaces.
      ZeroMemory(@StagingDescription, SizeOf(StagingDescription));
      StagingDescription.Width := Width;
      StagingDescription.Height := Height;
      StagingDescription.MipLevels := 1;
      StagingDescription.ArraySize := 1;
      StagingDescription.Format := DXGI_FORMAT_NV12;
      StagingDescription.SampleDesc.Count := 1;
      StagingDescription.Usage := D3D11_USAGE_DEFAULT;
      StagingDescription.BindFlags := D3D11_BIND_RENDER_TARGET;
      if SUCCEEDED(Hr) then
        Hr := Device.CreateTexture2D(StagingDescription,
                                     nil,
                                     @GpuFrameTexture);
      if SUCCEEDED(Hr) then
        Hr := VideoDevice.CreateVideoProcessorOutputView(
          GpuFrameTexture,
          VideoEnumerator,
          @VideoOutputViewDescription,
          @VideoOutputView);

      StagingDescription.BindFlags := 0;
      StagingDescription.Usage := D3D11_USAGE_STAGING;
      StagingDescription.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
      if SUCCEEDED(Hr) then
        Hr := Device.CreateTexture2D(StagingDescription,
                                     nil,
                                     @Nv12StagingTexture);

      UseGpuSurfaces := SUCCEEDED(Hr);
      if not UseGpuSurfaces then
        begin
          DxgiDeviceManager := nil;
          FOwner.Log(cllWarning,
            Format('D3D11 BGRA-to-NV12 video processor unavailable (0x%.8x); using CPU RGB32 buffers.',
                   [Cardinal(Hr)]));
        end;
    end;

  FOwner.Log(cllDebug,
    Format('Video input preparation completed: gpuSurfaces=%s hr=$%.8x size=%dx%d.',
           [BoolToStr(UseGpuSurfaces, True), Cardinal(Hr), Width, Height]));

  WriterUsesD3DManager := False;
  if UseGpuSurfaces then
    Result := CreateWriter(ByteStream,
                           Width,
                           Height,
                           WaveFormat,
                           MFVideoFormat_NV12,
                           nil,
                           Writer,
                           StreamIndex,
                           AudioStreamIndex)
  else
    Result := CreateWriter(ByteStream,
                           Width,
                           Height,
                           WaveFormat,
                           MFVideoFormat_RGB32,
                           nil,
                           Writer,
                           StreamIndex,
                           AudioStreamIndex);
  if FAILED(Result) then Exit;

  FOwner.Log(cllDebug,
    Format('Fragmented MP4 Sink Writer created: videoStream=%d audioStream=%d.',
           [StreamIndex, AudioStreamIndex]));

  if UseGpuSurfaces then
    FOwner.Log(cllInfo,
      'Desktop conversion uses a private D3D11 NV12 staging path; Media Foundation receives contiguous NV12 samples.');

  StagingDescription := Description;
  StagingDescription.Width := Width;
  StagingDescription.Height := Height;
  StagingDescription.BindFlags := 0;
  StagingDescription.Usage := D3D11_USAGE_STAGING;
  StagingDescription.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
  StagingDescription.MiscFlags := 0;

  Result := Device.CreateTexture2D(StagingDescription,
                                   nil,
                                   @StagingTexture);
  if FAILED(Result) then
    Exit;

  SourceBox.left := 0;
  SourceBox.top := 0;
  SourceBox.front := 0;
  SourceBox.right := Width;
  SourceBox.bottom := Height;
  SourceBox.back := 1;

  if UseGpuSurfaces then
    SetLength(Frame, (Width * Height * 3) div 2)
  else
    SetLength(Frame, Width * Height * 4);
  if Assigned(FOwner.FPreviewSink) then
    begin
      SetLength(PreviewFrame,
                Width * Height * 4);
      Hr := FOwner.FPreviewSink.ConfigureVideo(Width,
                                               Height);
      if FAILED(Hr) then
        FOwner.Log(cllWarning,
                   Format('Desktop preview could not be configured (0x%.8x).',
                          [Cardinal(Hr)]));
    end;
  FrameDuration := (Int64(10000000) * FrameRateDenominator) div FrameRateNumerator;
  if not QueryPerformanceFrequency(PerformanceFrequency) then
    Exit(HRESULT_FROM_WIN32(GetLastError()));

  if Assigned(AudioClient) then
    begin
      Result := AudioClient.Start();
      if FAILED(Result) then Exit;
      AudioStarted := True;

      FOwner.Log(cllInfo,
                 Format('WASAPI loopback started: %d Hz, %d channels, %d-bit, formatTag=%d.',
                        [WaveFormat^.nSamplesPerSec,
                         WaveFormat^.nChannels,
                         WaveFormat^.wBitsPerSample,
                         WaveFormat^.wFormatTag]));
    end;

  if not QueryPerformanceCounter(CaptureStartCounter) then
    Exit(HRESULT_FROM_WIN32(GetLastError()));

  CaptureStartQpc100ns :=
    MfCastPerformanceDelta100ns(CaptureStartCounter, PerformanceFrequency);
  LastVideoTime := -1;
  LastPreviewTime := -1;
  FramePresentationTime := 0;
  AudioTime := 0;
  FirstVideoTime := -1;
  FirstAudioTime := -1;
  TimelineLogTick := 0;
  MissedDesktopFrames := 0;
  FrameHeld := False;
  VideoProcessorFrame := 0;

  FOwner.SetState(csPlaying);
  // The desktop pipeline is now completely initialized: duplication, WASAPI,
  // converter and Sink Writer are all live. The controller may safely expose
  // the dedicated desktop URL to the receiver.
  FOwner.SignalStartup(S_OK);
  FOwner.Log(cllInfo,
              Format('DXGI capture started: %dx%d, codec=%s.',
                     [Width, Height, GUIDToString(FOwner.FVideoSubtype)]));

  while not Terminated do
    begin
      Resource := nil;
      DesktopTexture := nil;
      MappedTexture := False;
      FrameUpdated := False;
      PreviewFrameUpdated := False;

      Hr := Duplication.AcquireNextFrame(5,
                                         FrameInfo,
                                         Resource);
      if SUCCEEDED(Hr) then
        begin
          FramePresentCounter := FrameInfo.LastPresentTime.QuadPart;
          if FrameInfo.AccumulatedFrames > 1 then
            Inc(MissedDesktopFrames,
                FrameInfo.AccumulatedFrames - 1);

          if not QueryPerformanceCounter(CurrentCounter) then
            begin
              Result := HRESULT_FROM_WIN32(GetLastError());
              Duplication.ReleaseFrame();
              Exit;
            end;

          ElapsedTime := MfCastPerformanceDelta100ns(
            CurrentCounter - CaptureStartCounter,
            PerformanceFrequency);

          if FramePresentCounter > 0 then
            FramePresentationTime := MfCastPerformanceDelta100ns(
              FramePresentCounter - CaptureStartCounter,
              PerformanceFrequency)
          else
            FramePresentationTime := ElapsedTime;

          if FramePresentationTime < 0 then
            FramePresentationTime := 0;
          if FramePresentationTime > ElapsedTime then
            FramePresentationTime := ElapsedTime;

          // Encode a constant-rate 30-fps timeline. Raw desktop-present QPC
          // values include small scheduling variations that otherwise become
          // visible as uneven motion on Chromecast receivers.
          FramePresentationTime :=
            ((FramePresentationTime + (FrameDuration div 2)) div
             FrameDuration) * FrameDuration;

          ShouldCaptureFrame := (LastVideoTime < 0) or
            (FramePresentationTime +
             MFCAST_VIDEO_SCHEDULER_TOLERANCE_100NS >=
              LastVideoTime + FrameDuration);
          ShouldPresentPreview := Assigned(FOwner.FPreviewSink) and
            ((LastPreviewTime < 0) or
             (FramePresentationTime >=
              LastPreviewTime + MFCAST_PREVIEW_INTERVAL_100NS));

          try
            // Desktop duplication commonly reports at 60 Hz while a first-
            // generation Chromecast accepts 1080p at 30 fps. Discard the
            // over-rate frame before copying it into the encoder surface.
            if ShouldCaptureFrame then
              begin
                Hr := Resource.QueryInterface(IID_ID3D11Texture2D,
                                               DesktopTexture);
                if FAILED(Hr) then
                  begin
                    Result := Hr;
                    Exit;
                  end;

                if UseGpuSurfaces then
                  begin
                    ZeroMemory(@VideoInputViewDescription,
                               SizeOf(VideoInputViewDescription));
                    VideoInputViewDescription.ViewDimension :=
                      D3D11_VPIV_DIMENSION_TEXTURE2D;
                    VideoInputViewDescription.Texture2D.MipSlice := 0;
                    VideoInputViewDescription.Texture2D.ArraySlice := 0;
                    VideoInputView := nil;
                    Hr := VideoDevice.CreateVideoProcessorInputView(
                      DesktopTexture,
                      VideoEnumerator,
                      @VideoInputViewDescription,
                      @VideoInputView);
                    if FAILED(Hr) then
                      begin
                        Result := Hr;
                        Exit;
                      end;

                    ZeroMemory(@VideoStream, SizeOf(VideoStream));
                    VideoStream.Enable := True;
                    VideoStream.pInputSurface := VideoInputView;
                    Hr := VideoContext.VideoProcessorBlt(
                      VideoProcessor,
                      VideoOutputView,
                      VideoProcessorFrame,
                      1,
                      @VideoStream);
                    if FAILED(Hr) then
                      begin
                        Result := Hr;
                        Exit;
                      end;
                    Inc(VideoProcessorFrame);

                    // The reference sample explicitly stages NV12 and copies
                    // it into a normal MF memory buffer. This avoids the driver
                    // dependent IMFVideoSampleAllocatorEx initialization that
                    // blocked this machine for more than fifteen seconds.
                    Context.CopyResource(Nv12StagingTexture,
                                         GpuFrameTexture);
                    ZeroMemory(@Mapped, SizeOf(Mapped));
                    Hr := Context.Map(Nv12StagingTexture,
                                      0,
                                      D3D11_MAP_READ,
                                      0,
                                      Mapped);
                    if FAILED(Hr) then
                      begin
                        Result := Hr;
                        Exit;
                      end;
                    try
                      for Row := 0 to Height - 1 do
                        Move(PByte(NativeUInt(Mapped.pData) +
                                   NativeUInt(Row) * Mapped.RowPitch)^,
                             Frame[Row * Width],
                             Width);
                      for Row := 0 to (Height div 2) - 1 do
                        Move(PByte(NativeUInt(Mapped.pData) +
                                   NativeUInt(Height + Row) * Mapped.RowPitch)^,
                             Frame[Width * Height + Row * Width],
                             Width);
                    finally
                      Context.Unmap(Nv12StagingTexture, 0);
                    end;
                  end;

                // The encoder normally stays entirely on the GPU. At a
                // deliberately lower cadence, copy one desktop frame to a
                // staging texture for the local preview window. This avoids
                // putting a synchronous GPU readback in every 30-fps frame.
                if (not UseGpuSurfaces) or ShouldPresentPreview then
                  begin
                    Context.CopySubresourceRegion(StagingTexture,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  DesktopTexture,
                                                  0,
                                                  @SourceBox);

                    Hr := Context.Map(StagingTexture,
                                      0,
                                      D3D11_MAP_READ,
                                      0,
                                      Mapped);
                    if FAILED(Hr) then
                      begin
                        Result := Hr;
                        Exit;
                      end;

                    MappedTexture := True;

                    for Row := 0 to Height - 1 do
                      begin
                        if not UseGpuSurfaces then
                          if WriterUsesD3DManager then
                            Move(PByte(NativeUInt(Mapped.pData) + NativeUInt(Row) * Mapped.RowPitch)^,
                                 Frame[Row * Width * 4],
                                 Width * 4)
                          else
                            Move(PByte(NativeUInt(Mapped.pData) + NativeUInt(Row) * Mapped.RowPitch)^,
                                 Frame[(Height - 1 - Row) * Width * 4],
                                 Width * 4);

                        if ShouldPresentPreview then
                          Move(PByte(NativeUInt(Mapped.pData) + NativeUInt(Row) * Mapped.RowPitch)^,
                               PreviewFrame[(Height - 1 - Row) * Width * 4],
                               Width * 4);
                      end;

                    PreviewFrameUpdated := ShouldPresentPreview;
                  end;

                FrameHeld := True;
                FrameUpdated := True;
              end;

          finally
            if MappedTexture then
              Context.Unmap(StagingTexture,
                            0);
            Duplication.ReleaseFrame();
          end;

          if PreviewFrameUpdated then
            begin
              Hr := PresentPreviewFrame(PreviewFrame,
                                        Width,
                                        Height,
                                        FramePresentationTime,
                                        MFCAST_PREVIEW_INTERVAL_100NS);
              if SUCCEEDED(Hr) then
                LastPreviewTime := FramePresentationTime;
            end;

        end
      else
        if (Hr <> DXGI_ERROR_WAIT_TIMEOUT) then
          begin
            Result := Hr;
              Exit;
            end;

      if not QueryPerformanceCounter(CurrentCounter) then
        begin
          Result := HRESULT_FROM_WIN32(GetLastError());
          Exit;
        end;

      ElapsedTime := MfCastPerformanceDelta100ns(
        CurrentCounter - CaptureStartCounter,
        PerformanceFrequency);

      if Assigned(CaptureClient) then
        begin
          Hr := CaptureClient.GetNextPacketSize(PacketFrames);
          if FAILED(Hr) then
            begin
              Result := Hr;
              Exit;
            end;

          while PacketFrames > 0 do
            begin
              AudioData := nil;
              AudioFrames := 0;
              AudioFlags := 0;
              AudioDevicePosition := 0;
              AudioQpcPosition := 0;
              Hr := CaptureClient.GetBuffer(AudioData,
                                             AudioFrames,
                                             AudioFlags,
                                             @AudioDevicePosition,
                                             @AudioQpcPosition);
              if FAILED(Hr) then
                begin
                  Result := Hr;
                  Exit;
                end;

              try
                if AudioFrames > 0 then
                  begin
                    AudioBytes := AudioFrames * WaveFormat^.nBlockAlign;
                    AudioDuration := (Int64(AudioFrames) * 10000000) div
                                     WaveFormat^.nSamplesPerSec;

                    if (AudioFlags and
                        AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR) <> 0 then
                      begin
                        if AudioTimelineStarted then
                          AudioPacketTime := AudioTime
                        else
                          AudioPacketTime := ElapsedTime;
                      end
                    else
                      begin
                        AudioPacketTime := Int64(AudioQpcPosition) -
                                           CaptureStartQpc100ns;
                        if AudioPacketTime < 0 then
                          AudioPacketTime := 0;
                      end;

                    // WASAPI supplies the QPC position of the first frame in
                    // each packet. Use it as the authoritative shared clock;
                    // only clamp backwards jitter to keep MF timestamps valid.
                    if AudioTimelineStarted and
                       (AudioPacketTime < AudioTime) then
                      AudioPacketTime := AudioTime;

                    AudioTime := AudioPacketTime;
                    AudioTimelineStarted := True;

                    if FirstAudioTime < 0 then
                      FirstAudioTime := AudioTime;
                    Result := WriteAudioPacket(Writer,
                                               AudioStreamIndex,
                                               AudioData,
                                               AudioBytes,
                                               (AudioFlags and
                                                AUDCLNT_BUFFERFLAGS_SILENT) <> 0,
                                               AudioTime,
                                               AudioDuration);
                    if FAILED(Result) then Exit;
                    Inc(AudioTime, AudioDuration);
                  end;
              finally
                CaptureClient.ReleaseBuffer(AudioFrames);
              end;

              Hr := CaptureClient.GetNextPacketSize(PacketFrames);
              if FAILED(Hr) then
                begin
                  Result := Hr;
                  Exit;
                end;
            end;
        end;

      if FrameHeld and
         ((FrameUpdated and
           ((LastVideoTime < 0) or
            (FramePresentationTime + MFCAST_VIDEO_SCHEDULER_TOLERANCE_100NS >=
             LastVideoTime + FrameDuration))) or
          ((not FrameUpdated) and
           (LastVideoTime >= 0) and
           (ElapsedTime >= LastVideoTime + MFCAST_VIDEO_KEEPALIVE_100NS))) then
        begin
          if FrameUpdated then
            LastVideoTime := FramePresentationTime
          else
            LastVideoTime := ElapsedTime;

          if FirstVideoTime < 0 then
            FirstVideoTime := LastVideoTime;

          Result := WriteFrame(Writer,
                               StreamIndex,
                               Frame,
                               LastVideoTime,
                               FrameDuration);

          if FAILED(Result) and UseGpuSurfaces then
            FOwner.Log(cllError,
              Format('Encoder rejected a GPU NV12 surface (0x%.8x).',
                     [Cardinal(Result)]));
          if FAILED(Result) then Exit;

          // This diagnostic is intentionally sparse. It shows whether the
          // samples entering Media Foundation remain aligned, without adding
          // enough debugger traffic to disturb real-time capture.
          if (FirstAudioTime >= 0) and
             ((TimelineLogTick = 0) or
              ((GetTickCount() - TimelineLogTick) >= 30000)) then
            begin
              TimelineLogTick := GetTickCount();
              FOwner.Log(cllDebug,
                Format('A/V input timeline: video=%d ms audio=%d ms delta(audio-video)=%d ms firstDelta=%d ms.',
                       [LastVideoTime div 10000,
                        AudioTime div 10000,
                        (AudioTime - LastVideoTime) div 10000,
                        (FirstAudioTime - FirstVideoTime) div 10000]));
            end;
        end;
    end;

  if MissedDesktopFrames > 0 then
    FOwner.Log(cllWarning,
               Format('DXGI reported %d accumulated desktop frame(s) that could not be read individually.',
                      [MissedDesktopFrames]));

  if Terminated then
    begin
      // Stop is cancellation, not end-of-file authoring. Discard queued encoder
      // work so the UI does not wait for Finalize to drain a dead live stream.
      Writer.Flush(StreamIndex);
      if AudioStreamIndex <> DWORD(-1) then
        Writer.Flush(AudioStreamIndex);
      Result := S_OK;
    end
  else
    Result := Writer.Finalize();
  finally
    if AudioStarted and Assigned(AudioClient) then
      AudioClient.Stop();
    VideoOutputView := nil;
    Nv12StagingTexture := nil;
    GpuFrameTexture := nil;
    GpuBuffer2D := nil;
    GpuDxgiBuffer := nil;
    GpuBuffer := nil;
    GpuSample := nil;
    if Assigned(VideoSampleAllocator) then
      VideoSampleAllocator.UninitializeSampleAllocator();
    VideoSampleAllocator := nil;
    CaptureClient := nil;
    AudioClient := nil;
    AudioDevice := nil;
    DeviceEnumerator := nil;
    if Assigned(WaveFormat) then
      CoTaskMemFree(WaveFormat);
  end;
end;


procedure TMfCastDesktopCaptureWorker.Execute();
var
  ComResult: HRESULT;
  ComInitialized: Boolean;

begin

  ComInitialized := False;
  FOwner.Log(cllDebug, 'DXGI/WASAPI capture worker thread started.');
  ComResult := CoInitializeEx(nil,
                              COINIT_MULTITHREADED);
  if SUCCEEDED(ComResult) then
    ComInitialized := True

  else
    if (ComResult <> RPC_E_CHANGED_MODE) then
      begin
        FOwner.FWorkerResult := ComResult;
        FOwner.SetState(csError);
        Exit;
      end;
  try
    FOwner.Log(cllDebug,
      Format('Capture worker COM initialization returned HRESULT $%.8x.',
             [Cardinal(ComResult)]));
    FOwner.FWorkerResult := RunCapture();

    // Every initialization failure must wake Start; otherwise the caller
    // would wait for a stream that can never produce an MP4 header.
    FOwner.SignalStartup(FOwner.FWorkerResult);

    if FAILED(FOwner.FWorkerResult) and (not Terminated) then
      FOwner.Log(cllError,
        Format('DXGI/WASAPI capture worker stopped with HRESULT $%.8x.',
               [Cardinal(FOwner.FWorkerResult)]));

    if Terminated then
      FOwner.SetState(csStopped)
    else
      if FAILED(FOwner.FWorkerResult) then
        FOwner.SetState(csError)
      else
        FOwner.SetState(csStopped);

  finally
    if Assigned(FOwner.FPublisher) then
      if Terminated then
        FOwner.FPublisher.AbortPresentation(E_ABORT)
      else
        if SUCCEEDED(FOwner.FWorkerResult) then
          FOwner.FPublisher.CompletePresentation()
        else FOwner.FPublisher.AbortPresentation(FOwner.FWorkerResult);

    if ComInitialized then CoUninitialize();
  end;
end;


constructor TMfCastDesktopCapturePipeline.Create();
begin

  inherited Create();

  FState := csIdle;
  FWorker := nil;
  FWorkerResult := S_OK;
  FStartupResult := S_OK;
  FStartupReady := False;
  FStartupEvent := TEvent.Create(nil, True, False, '');
end;


destructor TMfCastDesktopCapturePipeline.Destroy();
begin

  Stop();
  FreeAndNil(FStartupEvent);

  inherited Destroy();
end;


procedure TMfCastDesktopCapturePipeline.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastDesktopCapturePipeline.Start(const ASettings: TMfCastCaptureSettings;
                                             const AVideoSubtype: TGUID;
                                             const APublisher: IMfCastSegmentPublisher;
                                             const APreviewSink: IMfCastPreviewSink): HRESULT;
begin

  if not Assigned(APublisher) then
    Exit(E_POINTER);

  if (ASettings.SourceKind <> ccskDesktop) then
    Exit(E_NOTIMPL);

  if not IsEqualGUID(AVideoSubtype,
                     MFVideoFormat_H264) and not
         IsEqualGUID(AVideoSubtype,
                     MFVideoFormat_HEVC) then
    Exit(E_INVALIDARG);

  if Assigned(FWorker) then
    Exit(HRESULT_FROM_WIN32(ERROR_BUSY));

  FSettings := ASettings;
  FVideoSubtype := AVideoSubtype;
  FPublisher := APublisher;
  FPreviewSink := APreviewSink;
  FWorkerResult := S_OK;
  FStartupResult := S_OK;
  FStartupReady := False;
  FStartupEvent.ResetEvent();
  SetState(csPreparingMedia);
  FWorker := TMfCastDesktopCaptureWorker.Create(Self);
  FWorker.Start();

  // Unlike file casting, a desktop LOAD must not be sent while DXGI and the
  // live Sink Writer are still being initialized. Older receivers reject an
  // empty live resource quickly and report LOAD_FAILED.
  if FStartupEvent.WaitFor(60000) <> wrSignaled then
    begin
      Log(cllError, 'Desktop capture initialization timed out.');
      Stop();
      Exit(HRESULT_FROM_WIN32(ERROR_TIMEOUT));
    end;

  Result := FStartupResult;
  if FAILED(Result) then
    begin
      Log(cllError,
          Format('Desktop capture initialization failed with HRESULT $%.8x.',
                 [Cardinal(Result)]));
      Stop();
    end;
end;


function TMfCastDesktopCapturePipeline.Stop(): HRESULT;
begin

  if Assigned(FWorker) then
    begin
      FWorker.Terminate();

      if Assigned(FPublisher) then
        FPublisher.AbortPresentation(E_ABORT);

      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  FPublisher := nil;
  FPreviewSink := nil;
  SetState(csStopped);
  Result := S_OK;
end;


function TMfCastDesktopCapturePipeline.GetState(): TMfCastState;
begin

  Result := FState;
end;


procedure TMfCastDesktopCapturePipeline.Log(const ALevel: TMfCastLogLevel;
                                            const AMessage: string);
begin

  if Assigned(FLogger) then
    FLogger.Log(ALevel,
                'DesktopCapture',
                AMessage)
  else
    OutputDebugString(PChar('[MfCast][DesktopCapture] ' + AMessage));
end;


procedure TMfCastDesktopCapturePipeline.SetState(const AState: TMfCastState);
begin

  FState := AState;
end;


procedure TMfCastDesktopCapturePipeline.SignalStartup(const AResult: HRESULT);
begin

  if FStartupReady then
    Exit;

  FStartupResult := AResult;
  FStartupReady := True;
  FStartupEvent.SetEvent();
end;

end.
