// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SimpleAvCapture.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: MfSimpleWebCamStreamer - simple webcam + microphone A/V sample.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
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
unit SimpleAvCapture;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.ComBaseApi,
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  {Media Foundation}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  SimpleFmp4ByteStream;

type
  TSimpleCaptureKind = (ckVideo, ckAudio);

  TSimpleDeviceList = class
  private
    FCount: UINT32;
    FDevices: PIMFActivate;
    FSourceType: TGUID;

  public
    constructor Create(const ASourceType: TGUID);
    destructor Destroy(); override;

    procedure Clear;
    function Enumerate: HRESULT;
    function GetActivate(const AIndex: UINT32;
                         out AActivate: IMFActivate): HRESULT;
    function GetFriendlyName(const AIndex: UINT32;
                             out AName: string): HRESULT;
    property Count: UINT32 read FCount;
  end;

  TSimpleAvCapture = class;

  TSimpleSourceReaderCallback = class(TInterfacedObject,
                                      IMFSourceReaderCallback)
  private
    FOwner: TSimpleAvCapture;
    FKind: TSimpleCaptureKind;

  public
    constructor Create(AOwner: TSimpleAvCapture;
                       const AKind: TSimpleCaptureKind);

    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HRESULT; stdcall;

    function OnFlush(dwStreamIndex: DWORD): HRESULT; stdcall;
    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HRESULT; stdcall;
  end;

  TSimpleCaptureState = (csStopped,
                         csStarting,
                         csCapturing,
                         csStopping,
                         csError);

  TSimpleAvCapture = class
  private
    FLock: TCriticalSection;

    FVideoReader: IMFSourceReader;
    FAudioReader: IMFSourceReader;

    FVideoActivate: IMFActivate;
    FAudioActivate: IMFActivate;
    FWriter: IMFSinkWriter;
    FMediaSink: IMFMediaSink;
    FByteStream: IMFByteStream;
    FObserver: TSimpleFmp4ByteStream;

    FVideoCallback: IMFSourceReaderCallback;
    FAudioCallback: IMFSourceReaderCallback;

    FVideoStreamIndex: DWORD;
    FAudioStreamIndex: DWORD;

    FVideoFirstTimeSet: Boolean;
    FAudioFirstTimeSet: Boolean;
    FVideoFirstTime: LONGLONG;
    FAudioFirstTime: LONGLONG;

    FVideoSamples: UInt64;
    FAudioSamples: UInt64;

    FState: TSimpleCaptureState;
    FLastError: HRESULT;

    function CreateReader(AActivate: IMFActivate;
                          ACallback: IMFSourceReaderCallback;
                          out AReader: IMFSourceReader): HRESULT;

    function ConfigureVideoReader(out AInputType: IMFMediaType): HRESULT;
    function ConfigureAudioReader(out AInputType: IMFMediaType): HRESULT;

    function ConfigureWriter(const AFileName: string;
                             AVideoInputType: IMFMediaType;
                             AAudioInputType: IMFMediaType): HRESULT;


    procedure SetError(const AError: HRESULT);
    procedure RequestNextSample(const AKind: TSimpleCaptureKind);
    procedure HandleSample(const AKind: TSimpleCaptureKind;
                           const AStatus: HRESULT;
                           const AFlags: DWORD;
                           const ATimestamp: LONGLONG;
                           ASample: IMFSample);

  public

    constructor Create();
    destructor Destroy(); override;

    function Start(AVideoDevice: IMFActivate;
                   AAudioDevice: IMFActivate;
                   const AFileName: string): HRESULT;
    function Stop(): HRESULT;

    function GetFmp4Diagnostics(out AInitBytes: Integer;
                                out ALastFragmentBytes: Integer;
                                out AFragmentCount: UInt64;
                                out ATotalBytesObserved: UInt64): Boolean;

    function GetInitSegment(out ASegment: TBytes): Boolean;

    function GetFragment(const ASequence: UInt64;
                         out AFragment: TBytes): Boolean;

    function GetFragmentWindow(out AFirstSequence: UInt64;
                               out ALastSequence: UInt64;
                               out ACount: Integer): Boolean;

    function TryPopArchiveFragment(out AFragment: TBytes): Boolean;
    procedure ResetArchiveTimeline();

    property State: TSimpleCaptureState read FState;
    property LastError: HRESULT read FLastError;
    property VideoSamples: UInt64 read FVideoSamples;
    property AudioSamples: UInt64 read FAudioSamples;
  end;

implementation

const
  SIMPLE_VIDEO_BITRATE = 2500000;
  SIMPLE_AUDIO_AVG_BYTES_PER_SEC = 16000; // 128 kbit/s
  SIMPLE_AUDIO_SAMPLES_PER_SEC = 48000;
  SIMPLE_AUDIO_CHANNELS = 2;
  SIMPLE_AUDIO_BITS_PER_SAMPLE = 16;

  // Ask the fragmented MPEG-4 sink to emit a movie fragment roughly once
  // per second. The value is expressed in Media Foundation 100-ns units.
  SIMPLE_FRAGMENT_DURATION_MS = 1000;
  SIMPLE_FRAGMENT_DURATION_100NS = SIMPLE_FRAGMENT_DURATION_MS * 10000;

{ TSimpleDeviceList }

constructor TSimpleDeviceList.Create(const ASourceType: TGUID);
begin

  inherited Create();

  FDevices := nil;
  FCount := 0;
  FSourceType := ASourceType;
end;


destructor TSimpleDeviceList.Destroy();
begin

  Clear();

  inherited Destroy;
end;


{$POINTERMATH ON}

procedure TSimpleDeviceList.Clear;
var
  I: Integer;

begin

  if Assigned(FDevices) then
    begin
      for I := 0 to Integer(FCount) - 1 do
        FDevices[I] := nil;

      CoTaskMemFree(FDevices);
      FDevices := nil;
    end;

  FCount := 0;
end;


function TSimpleDeviceList.Enumerate: HRESULT;
var
  Attributes: IMFAttributes;

begin

  Clear();

  Result := MFCreateAttributes(Attributes,
                               1);
  if SUCCEEDED(Result) then
    Result := Attributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                 FSourceType);

  if SUCCEEDED(Result) then
    Result := MFEnumDeviceSources(Attributes,
                                  FDevices,
                                  FCount);
end;


{$POINTERMATH ON}

function TSimpleDeviceList.GetActivate(const AIndex: UINT32;
                                       out AActivate: IMFActivate): HRESULT;
begin
  AActivate := nil;

  if AIndex >= FCount then
    Exit(E_INVALIDARG);

  AActivate := FDevices[AIndex];

  if Assigned(AActivate) then
    Result := S_OK
  else
    Result := E_POINTER;
end;


{$POINTERMATH ON}

function TSimpleDeviceList.GetFriendlyName(const AIndex: UINT32;
                                           out AName: string): HRESULT;
var
  Name: LPWSTR;
  Length: UINT32;

begin

  AName := '';
  Name := nil;
  Length := 0;

  if (AIndex >= FCount) then
    Exit(E_INVALIDARG);

  Result := FDevices[AIndex].GetAllocatedString(
              MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
              Name,
              Length);

  if SUCCEEDED(Result) then
  begin
    AName := Name;
    CoTaskMemFree(Name);
  end;
end;


{ TSimpleSourceReaderCallback }

constructor TSimpleSourceReaderCallback.Create(AOwner: TSimpleAvCapture;
                                                const AKind: TSimpleCaptureKind);
begin

  inherited Create;

 FOwner := AOwner;
  FKind := AKind;
end;


function TSimpleSourceReaderCallback.OnEvent(dwStreamIndex: DWORD;
                                             pEvent: IMFMediaEvent): HRESULT;
begin

  Result := S_OK;
end;


function TSimpleSourceReaderCallback.OnFlush(dwStreamIndex: DWORD): HRESULT;
begin

  Result := S_OK;
end;


function TSimpleSourceReaderCallback.OnReadSample(hrStatus: HRESULT;
                                                  dwStreamIndex: DWORD;
                                                  dwStreamFlags: DWORD;
                                                  llTimestamp: LONGLONG;
                                                  pSample: IMFSample): HRESULT;
begin

  // Media Foundation calls this on a worker thread.
  if Assigned(FOwner) then
    FOwner.HandleSample(FKind,
                        hrStatus,
                        dwStreamFlags,
                        llTimestamp,
                        pSample);

  Result := S_OK;
end;


{ TSimpleAvCapture }

constructor TSimpleAvCapture.Create;
begin

  inherited Create;

  FVideoActivate := nil;
  FAudioActivate := nil;


  FLock := TCriticalSection.Create();

  FVideoStreamIndex := DWORD(-1);
  FAudioStreamIndex := DWORD(-1);

  FVideoCallback := TSimpleSourceReaderCallback.Create(Self, ckVideo);
  FAudioCallback := TSimpleSourceReaderCallback.Create(Self, ckAudio);

  FState := csStopped;
  FLastError := S_OK;
end;


destructor TSimpleAvCapture.Destroy;
begin

  Stop();

  FVideoCallback := nil;
  FAudioCallback := nil;

  FreeAndNil(FLock);

  inherited Destroy();
end;


procedure TSimpleAvCapture.SetError(const AError: HRESULT);
begin

  FLastError := AError;
  FState := csError;
end;


function TSimpleAvCapture.CreateReader(AActivate: IMFActivate;
                                       ACallback: IMFSourceReaderCallback;
                                       out AReader: IMFSourceReader): HRESULT;
var
  Source: IMFMediaSource;
  Attributes: IMFAttributes;

begin

  AReader := nil;

  Result := AActivate.ActivateObject(IID_IMFMediaSource,
                                     Pointer(Source));

  if SUCCEEDED(Result) then
    Result := MFCreateAttributes(Attributes,
                                 2);

  if SUCCEEDED(Result) then
    Result := Attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                                    ACallback);

  // Let SourceReader insert video/audio processors where required.
  if SUCCEEDED(Result) then
    Result := Attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                                   1);

  if SUCCEEDED(Result) then
    Result := MFCreateSourceReaderFromMediaSource(Source,
                                                   Attributes,
                                                   AReader);
end;


function TSimpleAvCapture.ConfigureVideoReader(out AInputType: IMFMediaType): HRESULT;
var
  NativeType: IMFMediaType;
  TypeToSet: IMFMediaType;
  WidthHeight: UINT64;
  FrameRate: UINT64;
  PixelAspect: UINT64;
  Interlace: UINT32;

begin

  AInputType := nil;
  NativeType := nil;
  TypeToSet := nil;

  Result := FVideoReader.GetNativeMediaType(
              MF_SOURCE_READER_FIRST_VIDEO_STREAM,
              0,
              @NativeType);

  if FAILED(Result) then
    Exit;

  // Keep the native geometry/rate, request NV12 for the SinkWriter.
  Result := NativeType.GetUINT64(MF_MT_FRAME_SIZE,
                                 WidthHeight);
  if FAILED(Result) then
    Exit;

  Result := NativeType.GetUINT64(MF_MT_FRAME_RATE,
                                 FrameRate);
  if FAILED(Result) then
    Exit;

  if FAILED(NativeType.GetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                 PixelAspect)) then
    PixelAspect := (UINT64(1) shl 32) or 1;

  if FAILED(NativeType.GetUINT32(MF_MT_INTERLACE_MODE,
                                 Interlace)) then
    Interlace := MFVideoInterlace_Progressive;

  Result := MFCreateMediaType(TypeToSet);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetGUID(MF_MT_SUBTYPE,
                                MFVideoFormat_NV12);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetUINT64(MF_MT_FRAME_SIZE,
                                  WidthHeight);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetUINT64(MF_MT_FRAME_RATE,
                                  FrameRate);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                  PixelAspect);
  if SUCCEEDED(Result) then
    Result := TypeToSet.SetUINT32(MF_MT_INTERLACE_MODE,
                                  Interlace);

  if SUCCEEDED(Result) then
    Result := FVideoReader.SetCurrentMediaType(
                MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                0,
                TypeToSet);

  if SUCCEEDED(Result) then
    Result := FVideoReader.GetCurrentMediaType(
                MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                @AInputType);
end;


function TSimpleAvCapture.ConfigureAudioReader(out AInputType: IMFMediaType): HRESULT;
var
  AudioType: IMFMediaType;

begin

  AInputType := nil;
  AudioType := nil;

  Result := MFCreateMediaType(AudioType);

  if SUCCEEDED(Result) then
    Result := AudioType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Audio);
  if SUCCEEDED(Result) then
    Result := AudioType.SetGUID(MF_MT_SUBTYPE,
                                MFAudioFormat_PCM);
  if SUCCEEDED(Result) then
    Result := AudioType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                  SIMPLE_AUDIO_CHANNELS);
  if SUCCEEDED(Result) then
    Result := AudioType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                  SIMPLE_AUDIO_SAMPLES_PER_SEC);
  if SUCCEEDED(Result) then
    Result := AudioType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                  SIMPLE_AUDIO_BITS_PER_SAMPLE);
  if SUCCEEDED(Result) then
    Result := AudioType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                  SIMPLE_AUDIO_CHANNELS *
                                  (SIMPLE_AUDIO_BITS_PER_SAMPLE div 8));
  if SUCCEEDED(Result) then
    Result := AudioType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                  SIMPLE_AUDIO_SAMPLES_PER_SEC *
                                  SIMPLE_AUDIO_CHANNELS *
                                  (SIMPLE_AUDIO_BITS_PER_SAMPLE div 8));

  if SUCCEEDED(Result) then
    Result := FAudioReader.SetCurrentMediaType(
                MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                0,
                AudioType);

  if SUCCEEDED(Result) then
    Result := FAudioReader.GetCurrentMediaType(
                MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                @AInputType);
end;


function TSimpleAvCapture.ConfigureWriter(const AFileName: string;
                                          AVideoInputType: IMFMediaType;
                                          AAudioInputType: IMFMediaType): HRESULT;
var
  VideoOutputType: IMFMediaType;
  AudioOutputType: IMFMediaType;
  MediaSinkAttributes: IMFAttributes;
  WriterAttributes: IMFAttributes;
  Value64: UINT64;
  Value32: UINT32;

begin

  FWriter := nil;
  FMediaSink := nil;
  FByteStream := nil;
  FObserver := nil;

  VideoOutputType := nil;
  AudioOutputType := nil;
  MediaSinkAttributes := nil;
  WriterAttributes := nil;

  // The fragmented MPEG-4 sink is created with its final encoded media types.
  Result := MFCreateMediaType(VideoOutputType);

  if SUCCEEDED(Result) then
    Result := VideoOutputType.SetGUID(MF_MT_MAJOR_TYPE,
                                      MFMediaType_Video);
  if SUCCEEDED(Result) then
    Result := VideoOutputType.SetGUID(MF_MT_SUBTYPE,
                                      MFVideoFormat_H264);
  if SUCCEEDED(Result) then
    Result := VideoOutputType.SetUINT32(MF_MT_AVG_BITRATE,
                                        SIMPLE_VIDEO_BITRATE);

  if SUCCEEDED(Result) then
    begin
      Result := AVideoInputType.GetUINT64(MF_MT_FRAME_SIZE,
                                          Value64);
      if SUCCEEDED(Result) then
        Result := VideoOutputType.SetUINT64(MF_MT_FRAME_SIZE,
                                            Value64);
    end;

  if SUCCEEDED(Result) then
    begin
      Result := AVideoInputType.GetUINT64(MF_MT_FRAME_RATE,
                                          Value64);
      if SUCCEEDED(Result) then
        Result := VideoOutputType.SetUINT64(MF_MT_FRAME_RATE,
                                            Value64);
    end;

  if SUCCEEDED(Result) then
    begin
      if FAILED(AVideoInputType.GetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                          Value64)) then
        Value64 := (UINT64(1) shl 32) or 1;

      Result := VideoOutputType.SetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                        Value64);
    end;

  if SUCCEEDED(Result) then
    begin
      if FAILED(AVideoInputType.GetUINT32(MF_MT_INTERLACE_MODE,
                                          Value32)) then
        Value32 := MFVideoInterlace_Progressive;

      Result := VideoOutputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                          Value32);
    end;

  if SUCCEEDED(Result) then
    Result := MFCreateMediaType(AudioOutputType);

  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetGUID(MF_MT_MAJOR_TYPE,
                                      MFMediaType_Audio);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetGUID(MF_MT_SUBTYPE,
                                      MFAudioFormat_AAC);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                        SIMPLE_AUDIO_CHANNELS);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                        SIMPLE_AUDIO_SAMPLES_PER_SEC);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                        SIMPLE_AUDIO_AVG_BYTES_PER_SEC);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                        SIMPLE_AUDIO_BITS_PER_SAMPLE);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                                        $29);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_AAC_PAYLOAD_TYPE,
                                        0);
  if SUCCEEDED(Result) then
    Result := AudioOutputType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                                        UINT32(True));

  // Milestone 14 Fix 2: the single Media Foundation writer is the permanent
  // live encoder. Its custom IMFByteStream is writable/seekable but has no
  // backing file. It extracts init/raw/patched fMP4 data in memory.
  //
  // The rolling archive is assembled later from the already-encoded raw
  // fragments, so there is no second H.264/AAC encoder and no hourly MF
  // Finalize/Create cycle to disturb live playback.
  if SUCCEEDED(Result) then
    begin
      FObserver := TSimpleFmp4ByteStream.Create(nil);
      FByteStream := FObserver as IMFByteStream;
    end;

  if SUCCEEDED(Result) then
    Result := MFCreateFMPEG4MediaSink(FByteStream,
                                      VideoOutputType,
                                      AudioOutputType,
                                      @FMediaSink);

  // Explicitly enable periodic fragmentation. Without this setting the sink
  // is allowed to keep encoded media internally until Finalize.
  if SUCCEEDED(Result) then
    begin
      Result := FMediaSink.QueryInterface(IID_IMFAttributes,
                                          MediaSinkAttributes);

      if SUCCEEDED(Result) then
        Result := MediaSinkAttributes.SetUINT32(MF_MPEG4SINK_MIN_FRAGMENT_DURATION,
                                                SIMPLE_FRAGMENT_DURATION_100NS);
    end;

  if SUCCEEDED(Result) then
    Result := MFCreateAttributes(WriterAttributes,
                                 2);

  if SUCCEEDED(Result) then
    begin
      WriterAttributes.SetUINT32(MF_LOW_LATENCY,
                                 1);
      WriterAttributes.SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING,
                                 1);

      Result := MFCreateSinkWriterFromMediaSink(FMediaSink,
                                                WriterAttributes,
                                                FWriter);
    end;

  // MFCreateFMPEG4MediaSink creates fixed streams. For one video and one audio
  // stream the SinkWriter exposes stream 0 as video and stream 1 as audio.
  if SUCCEEDED(Result) then
    begin
      FVideoStreamIndex := 0;
      FAudioStreamIndex := 1;

      Result := FWriter.SetInputMediaType(FVideoStreamIndex,
                                          AVideoInputType,
                                          nil);
    end;

  if SUCCEEDED(Result) then
    Result := FWriter.SetInputMediaType(FAudioStreamIndex,
                                        AAudioInputType,
                                        nil);

  if SUCCEEDED(Result) then
    Result := FWriter.BeginWriting;
end;


procedure TSimpleAvCapture.RequestNextSample(const AKind: TSimpleCaptureKind);
var
  hr: HRESULT;

begin

  if (FState <> csCapturing) then
    Exit;

  case AKind of
    ckVideo:
      hr := FVideoReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                    0,
                                    nil,
                                    nil,
                                    nil,
                                    nil);
  else
    hr := FAudioReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                  0,
                                  nil,
                                  nil,
                                  nil,
                                  nil);
  end;

  if FAILED(hr) then
    SetError(hr);
end;


procedure TSimpleAvCapture.HandleSample(const AKind: TSimpleCaptureKind;
                                        const AStatus: HRESULT;
                                        const AFlags: DWORD;
                                        const ATimestamp: LONGLONG;
                                        ASample: IMFSample);
var
  hr: HRESULT;
  SampleTime: LONGLONG;

begin

  if (FState <> csCapturing) then
    Exit;

  if FAILED(AStatus) then
    begin
      SetError(AStatus);
      Exit;
    end;

  if ((AFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
    Exit;

  if ((AFlags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0) then
  begin
    SetError(MF_E_TRANSFORM_STREAM_CHANGE);
    Exit;
  end;

  HR := S_OK;

  if Assigned(ASample) then
    begin
      FLock.Enter;
      try
        case AKind of
          ckVideo:
            begin
              if not FVideoFirstTimeSet then
                begin
                  FVideoFirstTime := ATimestamp;
                  FVideoFirstTimeSet := True;
                end;

              SampleTime := ATimestamp - FVideoFirstTime;
              hr := ASample.SetSampleTime(SampleTime);

              if SUCCEEDED(hr) then
                hr := FWriter.WriteSample(FVideoStreamIndex,
                                          ASample);

              if SUCCEEDED(hr) then
                Inc(FVideoSamples);
            end;

        ckAudio:
          begin
            if not FAudioFirstTimeSet then
              begin
                FAudioFirstTime := ATimestamp;
                FAudioFirstTimeSet := True;
              end;

              SampleTime := ATimestamp - FAudioFirstTime;
              hr := ASample.SetSampleTime(SampleTime);

              if SUCCEEDED(hr) then
                HR := FWriter.WriteSample(FAudioStreamIndex,
                                          ASample);

              if SUCCEEDED(hr) then
                Inc(FAudioSamples);
          end;
      end;

    finally
      FLock.Leave;
    end;
  end;

  if FAILED(hr) then
    begin
      SetError(hr);
      Exit;
    end;

  RequestNextSample(AKind);
end;


function TSimpleAvCapture.Start(AVideoDevice: IMFActivate;
                                AAudioDevice: IMFActivate;
                                const AFileName: string): HRESULT;
var
  VideoType: IMFMediaType;
  AudioType: IMFMediaType;

begin

  // Keep the IMFActivate objects for the lifetime of this capture session.
  // After Stop releases the SourceReaders, ShutdownObject() is called on
  // these activations so the next Start receives fresh media sources instead
  // of an already-shutdown MF device object.
  FVideoActivate := AVideoDevice;
  FAudioActivate := AAudioDevice;

  if (FState <> csStopped) then
    Exit(MF_E_INVALIDREQUEST);

  FState := csStarting;
  FLastError := S_OK;
  FVideoSamples := 0;
  FAudioSamples := 0;

  FVideoFirstTimeSet := False;
  FAudioFirstTimeSet := False;
  FVideoFirstTime := 0;
  FAudioFirstTime := 0;

  Result := CreateReader(AVideoDevice,
                         FVideoCallback,
                         FVideoReader);

  if SUCCEEDED(Result) then
    Result := CreateReader(AAudioDevice,
                           FAudioCallback,
                           FAudioReader);

  if SUCCEEDED(Result) then
    Result := ConfigureVideoReader(VideoType);

  if SUCCEEDED(Result) then
    Result := ConfigureAudioReader(AudioType);

  if SUCCEEDED(Result) then
    Result := ConfigureWriter(AFileName,
                              VideoType,
                              AudioType);

  if SUCCEEDED(Result) then
    begin
      FState := csCapturing;

      RequestNextSample(ckVideo);
      RequestNextSample(ckAudio);
    end
  else
    begin
      SetError(Result);
      Stop;
    end;
end;


function TSimpleAvCapture.GetInitSegment(out ASegment: TBytes): Boolean;
begin

  ASegment := nil;
  Result := Assigned(FObserver);

  if Result then
    Result := FObserver.GetInitSegment(ASegment);
end;


function TSimpleAvCapture.GetFragment(const ASequence: UInt64;
                                      out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  Result := Assigned(FObserver);

  if Result then
    Result := FObserver.GetFragment(ASequence,
                                    AFragment);
end;


function TSimpleAvCapture.GetFragmentWindow(out AFirstSequence: UInt64;
                                             out ALastSequence: UInt64;
                                             out ACount: Integer): Boolean;
begin

  AFirstSequence := 0;
  ALastSequence := 0;
  ACount := 0;

  Result := Assigned(FObserver);

  if Result then
    Result := FObserver.GetFragmentWindow(AFirstSequence,
                                          ALastSequence,
                                          ACount);
end;


function TSimpleAvCapture.GetFmp4Diagnostics(out AInitBytes: Integer;
                                                  out ALastFragmentBytes: Integer;
                                                  out AFragmentCount: UInt64;
                                                  out ATotalBytesObserved: UInt64): Boolean;
begin

  AInitBytes := 0;
  ALastFragmentBytes := 0;
  AFragmentCount := 0;
  ATotalBytesObserved := 0;

  Result := Assigned(FObserver);

  if Result then
    Result := FObserver.GetDiagnostics(AInitBytes,
                                       ALastFragmentBytes,
                                       AFragmentCount,
                                       ATotalBytesObserved);
end;


function TSimpleAvCapture.TryPopArchiveFragment(out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  FLock.Enter;

  try
    Result := Assigned(FObserver);

    if Result then
      Result := FObserver.TryPopArchiveFragment(AFragment);
  finally
    FLock.Leave;
  end;
end;


procedure TSimpleAvCapture.ResetArchiveTimeline;
begin

  FLock.Enter;

  try
    if Assigned(FObserver) then
      FObserver.ResetArchiveTimeline;
  finally
    FLock.Leave;
  end;
end;


function TSimpleAvCapture.Stop: HRESULT;
var
  hr: HRESULT;

begin
  Result := S_OK;

  if (FState = csStopped) then
    Exit;

  FState := csStopping;

  if Assigned(FVideoReader) then
    FVideoReader.Flush(MF_SOURCE_READER_ALL_STREAMS);

  if Assigned(FAudioReader) then
    FAudioReader.Flush(MF_SOURCE_READER_ALL_STREAMS);

  hr := S_OK;

  FLock.Enter;

  try
    if Assigned(FWriter) then
      hr := FWriter.Finalize;
  finally
    FLock.Leave;
  end;

  FWriter := nil;

  if Assigned(FMediaSink) then
    FMediaSink.Shutdown();

  FMediaSink := nil;

  if Assigned(FByteStream) then
    begin
      FByteStream.Flush();
      FByteStream.Close();
    end;

  FByteStream := nil;
  FObserver := nil;
  FVideoReader := nil;
  FAudioReader := nil;


  // IMFActivate caches its activated device object. Once the SourceReader
  // has been released, explicitly shut down that object so a later Start
  // creates a fresh IMFMediaSource. Reusing the old activated source causes
  // MF_E_SHUTDOWN (0xC00D3E85) on Start -> Stop -> Start.
  if Assigned(FVideoActivate) then
    FVideoActivate.ShutdownObject();

  if Assigned(FAudioActivate) then
    FAudioActivate.ShutdownObject();

  FVideoActivate := nil;
  FAudioActivate := nil;

  FVideoStreamIndex := DWORD(-1);
  FAudioStreamIndex := DWORD(-1);

  FState := csStopped;
  Result := hr;
end;

end.
