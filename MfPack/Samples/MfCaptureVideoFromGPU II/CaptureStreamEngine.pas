// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  CaptureStreamEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description: The capture engine that takes care of writing to a
//              preview object (TPanel) and output file (MP4).
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Carmen (carmenh), Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit CaptureStreamEngine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  WinApi.ksmedia,
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Math,
  System.TimeSpan,
  System.SyncObjs,
  System.Generics.Collections,
  {Vcl}
  Vcl.Dialogs,
  {D3D11}
  WinApi.DirectX.D3D11,
  WinApi.DirectX.D3D11_1,
  WinApi.DirectX.D3DCommon,
  {DXGI}
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGIFormat,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.CodecApi,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.AudioSessionTypes,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  PreviewRenderer,
  GpuNV12Converter,
  WasapiLoopbackCapture,
  Helpers;

type
  TCaptureProgressEvent = procedure(Sender: TObject;
                                    FrameIndex: Int64;
                                    MilliSeconds: Double) of object;

  TCaptureErrorEvent = procedure(Sender: TObject;
                                 const Msg: string) of object;

  TDXGIOutputInfo = record
    OutputIndex: UINT;
    DeviceName: string;
  end;

  TQueuedSample = record
    StreamIndex: DWORD;
    Sample: IMFSample;
    Time100ns: Int64;
  end;

  TAudioChunk = record
    Time100ns: Int64;
    Duration100ns: Int64;
    Data: TBytes;
  end;

  TAudioCodec = (acNone,
                 acAac,
                 acFlac);

  TCaptureMode = (cmVideoOnly,
                  cmAudioOnly,
                  cmAudioVideo);

  TCaptureStreamEngine = class
    private

    // D3D / DXGI
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FPreview: TPreviewRenderer;

    FDXGIDevice: IDXGIDevice;
    FDXGIOutput1: IDXGIOutput1;
    FDeskDupl: IDXGIOutputDuplication;

    // NV12 staging
    FNV12StagingTexture: ID3D11Texture2D;

    // GPU converter
    FConverter: TGpuNV12Converter;

    // MF sink writer
    FSinkWriter: IMFSinkWriter;
    FVideoStreamIndex: DWORD;
    FAudioStreamIndex: DWORD;

    // cached lowercase extension of output file
    FOutputExt: string;

    // Single writer thread (mux-style)
    FWriterRunning: Boolean;
    FWriterThread: TThread;

    // Queues (written only by writer thread)
    FQueueCS: TCriticalSection;
    FQueueEvent: TEvent;
    FVideoQueue: TQueue<TQueuedSample>;
    FAudioQueue: TQueue<TQueuedSample>;

    // Audio capture + gating
    FAudioEnabled: Boolean;
    FAudioActive: Boolean;
    FAudioEverActive: Boolean;
    FAudioThresholdOnDb: Single;
    FAudioThresholdOffDb: Single;
    FAudioHoldOnMs: Integer;
    FAudioHoldOffMs: Integer;
    FAudioPreRollMs: Integer;

    // Pre-roll buffer (PCM chunks). Flushed when sound becomes active.
    FAudioPreRoll: TList<TAudioChunk>;
    FAudioAboveMs: Integer;
    FAudioBelowMs: Integer;
    FAudioNextTime100ns: Int64;

    // WASAPI mix-format characteristics (captured buffers). If True, we convert float->s16.
    FAudioSourceIsFloat: Boolean;

    // Audio chunking to reduce SinkWriter pressure (especially when encoding)
    FAudioBytesPerFrameOut: Integer; // bytes per audio frame written to sink (s16 interleaved)
    FAudioChunkFramesTarget: Cardinal;
    FAudioAccum: TBytes;
    FAudioAccumOffset: Integer;
    FAudioAccumFrames: Cardinal;

    FAudioSampleRate: Cardinal;
    FAudioChannels: Cardinal;
    FAudioBitsPerSample: Cardinal;
    FAudioBlockAlign: Cardinal;
    FAudioAvgBytesPerSec: Cardinal;
    FAudioChannelMask: Cardinal;

    // For MP4+FLAC authoring (must be provided for MP4 sink)
    FAudioMp4SampleDescription: TBytes;
    // AAC/FLAC selection for MP4
    FAudioCodec: TAudioCodec;

    // Output encoder target (bytes/sec). For PCM input see FAudioAvgBytesPerSec.
    FAudioOutAvgBytesPerSec: Cardinal;

    // WASAPI
    FWasapiCapture: TWasapiLoopbackCapture;
    FAudioDeviceId : string;

    // Geometry / preview
    FWidth,
    FHeight: UINT;
    FFrameRate: UINT32;
    FPreviewHandle: HWND;

    // State
    FRunning: Boolean;  // “engine is active”
    FVideoRunning: Boolean;  // “worker loop should keep running”
    FVideoThread: TThread;  // dedicated worker thread

    // Timing
    FFrameDuration100ns: Int64;
    FFrameIndex: Int64;
    FMilliSeconds: Double;
    FQpcStart: Int64;
    FPerfFreq: Int64;

    // Monitor selection
    FSelectedDisplayName: string; // "\\.\DISPLAYx"

    FDisplayOutputIndex: UINT;  // Selected monitor

    // Capture mode flags
    FCaptureMode : TCaptureMode;
    FRecordVideo : Boolean;

    FRecordAudio : Boolean;

    // Events
    FOnProgress: TCaptureProgressEvent;
    FOnError: TCaptureErrorEvent;

  protected

    procedure CreateD3DDevice();
    procedure InitDesktopDuplication();
    procedure CreateNV12Staging();
    procedure CreateSinkWriter(const OutputFile: string);
    procedure PrepareAudioFormatFromLoopbackMix;
    procedure AddAudioStreamToSinkWriter;
    procedure EnqueueSample(const StreamIndex: DWORD;
                            const Sample: IMFSample;
                            const Time100ns: Int64);
    function  PopNextSample(out Item: TQueuedSample): Boolean;
    procedure SinkWriterLoop;
    procedure GenerateSilenceToTime(const TargetTime100ns: Int64);
    procedure ResetAudioGating;

    procedure OnWasapiData(Sender: TObject;
                           const Buffer: Pointer;
                           NumFrames: Cardinal;
                           const WaveFormat: PWAVEFORMATEX);

    procedure SubmitAudioChunk(const Chunk: TAudioChunk);
    procedure SubmitVideoSampleToSinkWriter(const mappedNV12: D3D11_MAPPED_SUBRESOURCE);
    procedure VideoLoop();

    procedure SetCaptureMode(AMode: TCaptureMode);

  public

    constructor Create(aHwnd: HWND;
                       aWidth,
                       aHeight: UINT;
                       aFrameRate: UINT32);
    destructor Destroy(); override;

    procedure StartCapture(const OutputFile: string); overload;
    procedure StartCapture(const OutputFile: string;
                           aDisplayOutput: TDXGIOutputInfo); overload;
    procedure StopCapture();

    // Audio: Create audio stream at start, but only write samples when sound is detected.
    procedure EnableLoopbackAudioFlacMp4(const Mp4SampleDescription: TBytes;
                                         const ChannelMask: Cardinal);
    procedure EnableLoopbackAudioAacMp4(const ChannelMask: Cardinal;
                                        const TargetAvgBytesPerSec: Cardinal = 20000);

    procedure DisableAudio();
    procedure SetAudioDeviceID(const Value: string);


    function EnumerateOutputs(): TArray<TDXGIOutputInfo>;
    function GetSelectedOutputRect(): TRect;

    property AudioCodec: TAudioCodec read FAudioCodec write FAudioCodec;
    property AudioThresholdOnDb: Single read FAudioThresholdOnDb write FAudioThresholdOnDb;
    property AudioThresholdOffDb: Single read FAudioThresholdOffDb write FAudioThresholdOffDb;
    property AudioHoldOnMs: Integer read FAudioHoldOnMs write FAudioHoldOnMs;
    property AudioHoldOffMs: Integer read FAudioHoldOffMs write FAudioHoldOffMs;
    property AudioPreRollMs: Integer read FAudioPreRollMs write FAudioPreRollMs;

    property VideoOutputIndex: UINT read FDisplayOutputIndex write FDisplayOutputIndex;
    property AudioDeviceID: string read FAudioDeviceId write SetAudioDeviceID;

    property CaptureMode: TCaptureMode read FCaptureMode write SetCaptureMode;

    property SetFrameWidth: UINT write FWidth;
    property SetFrameHeight: UINT write FHeight;
    property SetFrameRate: UINT32 write FFRameRate;


    property OnProgress: TCaptureProgressEvent read FOnProgress write FOnProgress;
    property OnError: TCaptureErrorEvent   read FOnError    write FOnError;
  end;


implementation


{ TCaptureStreamEngine }

procedure TCaptureStreamEngine.CreateD3DDevice;
const
  FeatureLevels: array[0..2] of D3D_FEATURE_LEVEL = (D3D_FEATURE_LEVEL_11_1,
                                                     D3D_FEATURE_LEVEL_11_0,
                                                     D3D_FEATURE_LEVEL_10_0);
var
  hr: HResult;

begin
  hr := D3D11CreateDevice(nil,
                          D3D_DRIVER_TYPE_HARDWARE,
                          0,
                          D3D11_CREATE_DEVICE_BGRA_SUPPORT or D3D11_CREATE_DEVICE_DEBUG,  // For the best performance, shut the D3D11 debugger down on your final release! Remove "or D3D11_CREATE_DEVICE_DEBUG"
                          @FeatureLevels,
                          Length(FeatureLevels),
                          D3D11_SDK_VERSION,
                          @FDevice,
                          nil,  // Feature level the application wil use, can be nil when not needed.
                          @FContext);
  CheckHR(hr, 'D3D11CreateDevice');

  FPreview := TPreviewRenderer.Create(FDevice,
                                      FContext);
end;


procedure TCaptureStreamEngine.InitDesktopDuplication;
var
  hr: HResult;
  dxgiDev: IDXGIDevice;
  adapter: IDXGIAdapter;
  output: IDXGIOutput;
  output1: IDXGIOutput1;
  outDesc: DXGI_OUTPUT_DESC;
  deskW,
  deskH: UINT;

begin

  FDXGIDevice := nil;
  FDXGIOutput1 := nil;
  FDeskDupl := nil;

  hr := FDevice.QueryInterface(IDXGIDevice,
                               dxgiDev);
  CheckHR(hr, 'QueryInterface(IDXGIDevice)');

  hr := dxgiDev.GetAdapter(adapter);
  CheckHR(hr, 'IDXGIDevice.GetAdapter');

  // Use currently selected output index
  hr := adapter.EnumOutputs(FDisplayOutputIndex,
                            output);
  CheckHR(hr, 'IDXGIAdapter.EnumOutputs(FOutputIndex)');

  hr := output.QueryInterface(IDXGIOutput1,
                              output1);
  CheckHR(hr, 'QueryInterface(IDXGIOutput1)');

  FDXGIDevice  := dxgiDev;
  FDXGIOutput1 := output1;

  ZeroMemory(@outDesc,
             SizeOf(outDesc));

  hr := output.GetDesc(outDesc);
  CheckHR(hr, 'IDXGIOutput.GetDesc');

  deskW := outDesc.DesktopCoordinates.Right  - outDesc.DesktopCoordinates.Left;
  deskH := outDesc.DesktopCoordinates.Bottom - outDesc.DesktopCoordinates.Top;

  FWidth  := deskW;
  FHeight := deskH;

  if Assigned(FPreview) then
    FPreview.CreateSwapChainForHWND(FPreviewHandle,
                                    FWidth,
                                    FHeight,
                                    FFrameRate);

  hr := FDXGIOutput1.DuplicateOutput(FDevice,
                                     FDeskDupl);
  if (hr = DXGI_ERROR_NOT_CURRENTLY_AVAILABLE) then
    raise Exception.Create('Desktop duplication: too many clients are active.');
  CheckHR(hr, 'IDXGIOutput1.DuplicateOutput');
end;


function TCaptureStreamEngine.EnumerateOutputs(): TArray<TDXGIOutputInfo>;
var
  hr: HResult;
  dxgiDev: IDXGIDevice;
  adapter: IDXGIAdapter;
  output: IDXGIOutput;
  outDesc: DXGI_OUTPUT_DESC;
  outIdx: UINT;
  list: TArray<TDXGIOutputInfo>;

begin

  SetLength(list,
            0);

  if (FDevice = nil) then
    Exit(list);

  hr := FDevice.QueryInterface(IDXGIDevice,
                               dxgiDev);
  CheckHR(hr, 'EnumerateOutputs: QueryInterface(IDXGIDevice)');

  hr := dxgiDev.GetAdapter(adapter);
  CheckHR(hr, 'EnumerateOutputs: GetAdapter');

  outIdx := 0;

  while adapter.EnumOutputs(outIdx,
                            output) <> DXGI_ERROR_NOT_FOUND do
    begin
      ZeroMemory(@outDesc,
                 SizeOf(outDesc));

      hr := output.GetDesc(outDesc);
      CheckHR(hr, 'EnumerateOutputs: GetDesc');

      SetLength(list,
                Length(list) + 1);

      list[High(list)].OutputIndex := outIdx;
      list[High(list)].DeviceName := outDesc.DeviceName;

      output := nil;
      Inc(outIdx);
    end;

  Result := list;
end;


function TCaptureStreamEngine.GetSelectedOutputRect(): TRect;
var

  hr: HResult;
  dxgiDev: IDXGIDevice;
  adapter: IDXGIAdapter;
  output: IDXGIOutput;
  desc: DXGI_OUTPUT_DESC;
  idx: UINT;
  wanted : string;

begin

  Result := Rect(0,
                 0,
                 0,
                 0);

  if (FDevice = nil) then
    Exit;

  wanted := FSelectedDisplayName;

  if (wanted = '') then
    wanted := '\\.\DISPLAY1';

  hr := FDevice.QueryInterface(IDXGIDevice,
                               dxgiDev);
  CheckHR(hr, 'GetSelectedOutputRect: QI IDXGIDevice');

  hr := dxgiDev.GetAdapter(adapter);
  CheckHR(hr, 'GetSelectedOutputRect: GetAdapter');

  idx := 0;

  while (adapter.EnumOutputs(idx, output) <> DXGI_ERROR_NOT_FOUND) do
    begin

      ZeroMemory(@desc,
                 SizeOf(desc));

       hr := output.GetDesc(desc);

       if Succeeded(hr) then
         begin

          if SameText(desc.DeviceName, wanted) or (idx = FDisplayOutputIndex) then
            begin

              Result.Left := desc.DesktopCoordinates.Left;
              Result.Top := desc.DesktopCoordinates.Top;
              Result.Right := desc.DesktopCoordinates.Right;
              Result.Bottom := desc.DesktopCoordinates.Bottom;
              Exit;
            end;

         end;

    output := nil;
    Inc(idx);
  end;
end;


procedure TCaptureStreamEngine.CreateNV12Staging();
var
  hr: HResult;
  desc: D3D11_TEXTURE2D_DESC;

begin

  FNV12StagingTexture := nil;

  ZeroMemory(@desc,
             SizeOf(desc));

  desc.Width := FWidth;
  desc.Height := FHeight;
  desc.MipLevels := 1;
  desc.ArraySize := 1;
  desc.Format := DXGI_FORMAT_NV12;
  desc.SampleDesc.Count := 1;
  desc.SampleDesc.Quality := 0;
  desc.Usage := D3D11_USAGE_STAGING;
  desc.BindFlags := 0;
  desc.CPUAccessFlags := D3D11_CPU_ACCESS_READ or D3D11_CPU_ACCESS_WRITE;
  desc.MiscFlags := 0;

  hr := FDevice.CreateTexture2D(desc,
                                nil,
                                @FNV12StagingTexture);
  CheckHR(hr, 'CreateTexture2D(NV12 staging)');
end;


procedure TCaptureStreamEngine.CreateSinkWriter(const OutputFile: string);
var
  hr: HResult;
  mediaTypeOut,
  mediaTypeIn: IMFMediaType;

begin

  FSinkWriter := nil;
  FVideoStreamIndex := 0;
  FAudioStreamIndex := 0;


  FOutputExt := LowerCase(ExtractFileExt(OutputFile));
  // Simple MP4 sink writer setup (as you had it)
  hr := MFCreateSinkWriterFromURL(PWideChar(WideString(OutputFile)),
                                  nil,
                                  nil,
                                  FSinkWriter);
  CheckHR(hr, 'MFCreateSinkWriterFromURL');

  // Output type: H.264
  hr := MFCreateMediaType(mediaTypeOut);
  CheckHR(hr, 'MFCreateMediaType(out)');

  hr := mediaTypeOut.SetGUID(MF_MT_MAJOR_TYPE,
                             MFMediaType_Video);
  CheckHR(hr, 'SetGUID(MF_MT_MAJOR_TYPE, Video)');

  hr := mediaTypeOut.SetGUID(MF_MT_SUBTYPE,
                             MFVideoFormat_H264);
  CheckHR(hr, 'SetGUID(MF_MT_SUBTYPE, H264)');

  hr := mediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                               8000000);
  CheckHR(hr, 'SetUINT32(MF_MT_AVG_BITRATE)');

  hr := mediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                               MFVideoInterlace_Progressive);
  CheckHR(hr, 'SetUINT32(MF_MT_INTERLACE_MODE)');

  hr := MFSetAttributeSize(mediaTypeOut,
                           MF_MT_FRAME_SIZE,
                           FWidth,
                           FHeight);
  CheckHR(hr, 'MFSetAttributeSize(FRAME_SIZE out)');

  hr := MFSetAttributeRatio(mediaTypeOut,
                            MF_MT_FRAME_RATE,
                            FFrameRate,
                            1);
  CheckHR(hr, 'MFSetAttributeRatio(FRAME_RATE out)');

  hr := MFSetAttributeRatio(mediaTypeOut,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            1,
                            1);
  CheckHR(hr, 'MFSetAttributeRatio(PAR out)');

  hr := FSinkWriter.AddStream(mediaTypeOut,
                              FVideoStreamIndex);
  CheckHR(hr, 'AddStream');

  // Input type: NV12
  hr := MFCreateMediaType(mediaTypeIn);
  CheckHR(hr, 'MFCreateMediaType(in)');

  hr := mediaTypeIn.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Video);
  CheckHR(hr, 'SetGUID(MF_MT_MAJOR_TYPE, Video in)');

  hr := mediaTypeIn.SetGUID(MF_MT_SUBTYPE,
                            MFVideoFormat_NV12);
  CheckHR(hr, 'SetGUID(MF_MT_SUBTYPE, NV12)');

  hr := mediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                              MFVideoInterlace_Progressive);
  CheckHR(hr, 'SetUINT32(MF_MT_INTERLACE_MODE in)');

  hr := MFSetAttributeSize(mediaTypeIn,
                           MF_MT_FRAME_SIZE,
                           FWidth,
                           FHeight);
  CheckHR(hr, 'MFSetAttributeSize(FRAME_SIZE in)');

  hr := MFSetAttributeRatio(mediaTypeIn,
                            MF_MT_FRAME_RATE,
                            FFrameRate,
                            1);
  CheckHR(hr, 'MFSetAttributeRatio(FRAME_RATE in)');

  hr := MFSetAttributeRatio(mediaTypeIn,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            1,
                            1);
  CheckHR(hr, 'MFSetAttributeRatio(PAR in)');

  hr := FSinkWriter.SetInputMediaType(FVideoStreamIndex,
                                      mediaTypeIn,
                                      nil);
  CheckHR(hr, 'SetInputMediaType');

  // Optional audio stream (created upfront; samples may start later)
  if FAudioEnabled then
    AddAudioStreamToSinkWriter;

  hr := FSinkWriter.BeginWriting;
  CheckHR(hr, 'BeginWriting');

  // Initialize mux-style writer infrastructure
  FreeAndNil(FQueueCS);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FAudioPreRoll);

  FQueueCS := TCriticalSection.Create;
  FQueueEvent := TEvent.Create(nil, False, False, '');
  FVideoQueue := TQueue<TQueuedSample>.Create;
  FAudioQueue := TQueue<TQueuedSample>.Create;
  FWriterRunning := True;
  FWriterThread := TThread.CreateAnonymousThread(
    procedure
    begin
      SinkWriterLoop;
    end);
  FWriterThread.FreeOnTerminate := False;
  FWriterThread.Start;

end;


procedure TCaptureStreamEngine.PrepareAudioFormatFromLoopbackMix;
var
  hr: HRESULT;
  enum: IMMDeviceEnumerator;
  dev: IMMDevice;
  ac: IAudioClient;
  wf: PWAVEFORMATEX;
  pExt: PWAVEFORMATEXTENSIBLE;
  chMask: Cardinal;
  isFloat: Boolean;
  chunkMs: Cardinal;

begin

  if not FAudioEnabled then
    Exit;

  wf := nil;
  enum := nil;
  dev := nil;
  ac := nil;

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         enum);
  CheckHR(hr, 'CoCreateInstance(IMMDeviceEnumerator)');

  hr := enum.GetDefaultAudioEndpoint(eRender,
                                     eConsole,
                                     dev);
  CheckHR(hr, 'GetDefaultAudioEndpoint');

  hr := dev.Activate(IID_IAudioClient,
                     CLSCTX_INPROC_SERVER,
                     nil,
                     Pointer(ac));
  CheckHR(hr, 'IMMDevice.Activate(IAudioClient)');

  hr := ac.GetMixFormat(wf);
  CheckHR(hr, 'IAudioClient.GetMixFormat');

  try
    if (wf = nil) or
       (wf.nSamplesPerSec = 0) or
       (wf.nChannels = 0) then
      raise Exception.Create('Invalid WASAPI mix format');

    // Determine source format
    isFloat := (wf.wFormatTag = WAVE_FORMAT_IEEE_FLOAT);
    chMask := 0;

    if wf.wFormatTag = WAVE_FORMAT_EXTENSIBLE then
      begin

        pExt := PWAVEFORMATEXTENSIBLE(wf);
        chMask := pExt^.dwChannelMask;
        isFloat := IsEqualGUID(pExt^.SubFormat,
                               KSDATAFORMAT_SUBTYPE_IEEE_FLOAT);
      end;

    // Fall back to typical channel masks if not provided
    if (chMask = 0) then
      begin

        if (wf.nChannels = 1) then
          chMask := $4
        else
          if (wf.nChannels = 2) then
          chMask := $3;
      end;

    // We write s16 PCM into the sink writer input (convert from float if needed).
    FAudioSampleRate := wf.nSamplesPerSec;
    FAudioChannels := wf.nChannels;
    FAudioBitsPerSample := 16;
    FAudioBytesPerFrameOut := Integer(FAudioChannels) * 2; // s16 interleaved
    FAudioBlockAlign := FAudioBytesPerFrameOut;
    FAudioAvgBytesPerSec := FAudioSampleRate * FAudioBlockAlign;
    
    // Output target: for FLAC we keep it equal to PCM; for AAC we use the configured bitrate.
    if (FAudioCodec = acAac) then
      begin

        if (FAudioOutAvgBytesPerSec = 0) then
          FAudioOutAvgBytesPerSec := 20000;
      end
    else
      begin

        if (FAudioOutAvgBytesPerSec = 0) then
          FAudioOutAvgBytesPerSec := FAudioAvgBytesPerSec;
      end;

    FAudioChannelMask := chMask;
    FAudioSourceIsFloat := isFloat;

    // Chunk size: ~40ms to reduce WriteSample overhead (esp. if encoding)
    chunkMs := 40;
    FAudioChunkFramesTarget := (FAudioSampleRate * chunkMs + 999) div 1000;
    if (FAudioChunkFramesTarget < 256) then
      FAudioChunkFramesTarget := 256;

    // Reset accumulator
    SetLength(FAudioAccum, 0);
    FAudioAccumOffset := 0;
    FAudioAccumFrames := 0;

  finally

    if Assigned(wf) then
      CoTaskMemFree(wf);
  end;
end;


procedure TCaptureStreamEngine.AddAudioStreamToSinkWriter();
var
  hr: HRESULT;
  outType,
  inType: IMFMediaType;

begin

  if (FSinkWriter = nil) then
    Exit;

  // Infer audio format from loopback later; for now we set a safe default.
  // If loopback provides different format, SetInputMediaType will accept as long as MFT can convert.
  if FAudioSampleRate = 0 then
    begin

      FAudioSampleRate := 48000;
      FAudioChannels := 2;
      FAudioBitsPerSample := 16;
      FAudioBlockAlign := FAudioChannels * (FAudioBitsPerSample div 8);
      FAudioAvgBytesPerSec := FAudioSampleRate * FAudioBlockAlign;

      if (FAudioOutAvgBytesPerSec = 0) then
        FAudioOutAvgBytesPerSec := FAudioAvgBytesPerSec;

      if (FAudioChannelMask = 0) then
        FAudioChannelMask := $3; // Stereo.
    end;

  hr := MFCreateMediaType(outType);
  CheckHR(hr, 'MFCreateMediaType(audio out)');

  hr := outType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
  CheckHR(hr, 'SetGUID(MF_MT_MAJOR_TYPE audio out)');

  // Output subtype: FLAC (requires MP4 sample description) or standard AAC.
  if (FAudioCodec = acAac) then
    begin
      hr := outType.SetGUID(MF_MT_SUBTYPE, MFAudioFormat_AAC);
      CheckHR(hr, 'SetGUID(MF_MT_SUBTYPE AAC out)');
    end
  else // FLAC
    begin

      hr := outType.SetGUID(MF_MT_SUBTYPE,
                            MFAudioFormat_FLAC);
      CheckHR(hr, 'SetGUID(MF_MT_SUBTYPE FLAC out)');

      // For MP4+FLAC, Media Foundation may require MF_MT_MPEG4_SAMPLE_DESCRIPTION (track sample entry).
      // If the caller provided one, attach it; otherwise we try without it.
      if (FOutputExt = '.mp4') and (Length(FAudioMp4SampleDescription) > 0) then
        begin
          hr := outType.SetBlob(MF_MT_MPEG4_SAMPLE_DESCRIPTION,
                                @FAudioMp4SampleDescription[0],
                                Length(FAudioMp4SampleDescription));
          CheckHR(hr, 'SetBlob(MF_MT_MPEG4_SAMPLE_DESCRIPTION)');
        end;
    end;

  hr := outType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                          FAudioChannels);
  CheckHR(hr, 'SetUINT32(NUM_CHANNELS out)');

  hr := outType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                          FAudioSampleRate);
  CheckHR(hr, 'SetUINT32(SAMPLES_PER_SECOND out)');

  hr := outType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                          FAudioBitsPerSample);
  CheckHR(hr, 'SetUINT32(BITS_PER_SAMPLE out)');

  hr := outType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                          FAudioBlockAlign);
  CheckHR(hr, 'SetUINT32(BLOCK_ALIGNMENT out)');

  hr := outType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                          FAudioOutAvgBytesPerSec);
  CheckHR(hr, 'SetUINT32(AVG_BYTES_PER_SECOND out)');

  hr := FSinkWriter.AddStream(outType,
                              FAudioStreamIndex);
  CheckHR(hr, 'FSinkWriter.AddStream(audio)');

  // Input: PCM (what WASAPI typically delivers). If your loopback is float, update this later.
  hr := MFCreateMediaType(inType);
  CheckHR(hr, 'MFCreateMediaType(audio in)');

  hr := inType.SetGUID(MF_MT_MAJOR_TYPE,
                       MFMediaType_Audio);
  CheckHR(hr, 'SetGUID(MF_MT_MAJOR_TYPE audio in)');

  hr := inType.SetGUID(MF_MT_SUBTYPE,
                       MFAudioFormat_PCM);
  CheckHR(hr, 'SetGUID(MF_MT_SUBTYPE PCM in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                         FAudioChannels);
  CheckHR(hr, 'SetUINT32(NUM_CHANNELS in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                         FAudioSampleRate);
  CheckHR(hr, 'SetUINT32(SAMPLES_PER_SECOND in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                         FAudioBitsPerSample);
  CheckHR(hr, 'SetUINT32(BITS_PER_SAMPLE in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                         FAudioBlockAlign);
  CheckHR(hr, 'SetUINT32(BLOCK_ALIGNMENT in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                         FAudioAvgBytesPerSec);
  CheckHR(hr, 'SetUINT32(AVG_BYTES_PER_SECOND in)');

  hr := inType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK,
                         FAudioChannelMask);
  CheckHR(hr, 'SetUINT32(CHANNEL_MASK in)');

  hr := FSinkWriter.SetInputMediaType(FAudioStreamIndex,
                                      inType,
                                      nil);
  CheckHR(hr, 'FSinkWriter.SetInputMediaType(audio)');
end;



procedure TCaptureStreamEngine.SubmitVideoSampleToSinkWriter(const mappedNV12: D3D11_MAPPED_SUBRESOURCE);
var
  hr: HResult;
  sample: IMFSample;
  buffer: IMFMediaBuffer;
  pData: PByte;
  maxLen,
  curLen: DWORD;
  frameSizeY,
  frameSizeUV: UINT32;
  qpcNow: Int64;
  sampleTime: Int64;

begin

  // NV12 layout: Y (W*H), then UV (W*H/2)
  frameSizeY  := FWidth * FHeight;
  frameSizeUV := FWidth * (FHeight div 2);

  hr := MFCreateMemoryBuffer(frameSizeY + frameSizeUV,
                             buffer);
  CheckHR(hr, 'MFCreateMemoryBuffer');

  hr := buffer.Lock(pData,
                    @maxLen,
                    @curLen);
  CheckHR(hr, 'IMFMediaBuffer.Lock');

  try
    // Y plane
    Move(mappedNV12.pData^,
         pData^,
         frameSizeY);

    // UV plane
    Move(PByte(NativeUInt(mappedNV12.pData) + mappedNV12.RowPitch * FHeight)^,
         PByte(NativeUInt(pData) + frameSizeY)^,
         frameSizeUV);

    hr := buffer.SetCurrentLength(frameSizeY + frameSizeUV);
    CheckHR(hr, 'IMFMediaBuffer.SetCurrentLength');
  finally
    buffer.Unlock;
  end;

  hr := MFCreateSample(sample);
  CheckHR(hr, 'MFCreateSample');

  hr := sample.AddBuffer(buffer);
  CheckHR(hr, 'IMFSample.AddBuffer');

  // timestamps
  QueryPerformanceCounter(qpcNow);
  // Don't use MulDiv for Int64 types!
  // sampleTime := MulDiv(qpcNow - FQpcStart,
  //                        10000000,
  //                        FPerfFreq);

  // MulDiv replacement for Int64 types.
  sampleTime := _MulDiv64(qpcNow - FQpcStart,
                          10000000,
                          FPerfFreq);

  hr := sample.SetSampleTime(sampleTime);
  CheckHR(hr, 'IMFSample.SetSampleTime');

  hr := sample.SetSampleDuration(FFrameDuration100ns);
  CheckHR(hr, 'IMFSample.SetSampleDuration');

  // Enqueue for mux-style single-writer thread (prevents SinkWriter stress / buffering issues)
  EnqueueSample(FVideoStreamIndex,
                sample,
                sampleTime);
end;


procedure TCaptureStreamEngine.VideoLoop();
var
  hr: HResult;
  frameInfo: DXGI_OUTDUPL_FRAME_INFO;
  desktopRes: IDXGIResource;
  desktopTex: ID3D11Texture2D;
  mappedNV12: D3D11_MAPPED_SUBRESOURCE;
  ctx: ID3D11DeviceContext;
  t0,
  t1: Int64;

begin

  FDevice.GetImmediateContext(ctx);

  while FVideoRunning do
    begin
      QueryPerformanceCounter(t0);

       ZeroMemory(@frameInfo,
                  SizeOf(frameInfo));
       hr := FDeskDupl.AcquireNextFrame(16, // Must be between 2 and 16!
                                        frameInfo,
                                        desktopRes);

      if not FVideoRunning then
        Break;

      if (hr = DXGI_ERROR_WAIT_TIMEOUT) then
        Continue;

      if (hr = DXGI_ERROR_ACCESS_LOST) then
        begin
          try
            InitDesktopDuplication();
          except
            on E: Exception do
              begin
                if Assigned(FOnError) then
                  FOnError(Self, 'Access lost: ' + E.Message);
                Exit;
              end;
          end;
          Continue;
        end;

      CheckHR(hr, 'AcquireNextFrame');

      try
        CheckHR(desktopRes.QueryInterface(ID3D11Texture2D, desktopTex),
              'QueryInterface(ID3D11Texture2D)');

        // Preview
        if Assigned(FPreview) then
          FPreview.RenderFrame(desktopTex);

        // NV12 staging map
        ZeroMemory(@mappedNV12,
                   SizeOf(mappedNV12));

        hr := ctx.Map(FNV12StagingTexture,
                      0,
                      D3D11_MAP_WRITE,
                      0,
                      mappedNV12);
        CheckHR(hr, 'Map(NV12 staging)');

        try

          FConverter.Convert(desktopTex,
                             mappedNV12);
        finally

          ctx.Unmap(FNV12StagingTexture, 0);
        end;

        // push sample
        SubmitVideoSampleToSinkWriter(mappedNV12);

        QueryPerformanceCounter(t1);
        FMilliSeconds := (t1 - t0) / FPerfFreq;

        Inc(FFrameIndex);

        if Assigned(FOnProgress) then
          FOnProgress(Self,
                      FFrameIndex,
                      FMilliSeconds);

      finally
        FDeskDupl.ReleaseFrame;
      end;
  end;
end;


constructor TCaptureStreamEngine.Create(aHwnd: HWND;
                                        aWidth,
                                        aHeight: UINT;
                                        aFrameRate: UINT32);
var
  hr: HResult;

begin

  inherited Create;

  FPreviewHandle := aHwnd;
  FWidth := aWidth;
  FHeight := aHeight;
  FFrameRate := aFrameRate;
  FDisplayOutputIndex := 0;

  FRunning := False;
  FVideoRunning := False;
  FVideoThread := nil;

  CreateD3DDevice();

  hr := MFStartup(MF_VERSION,
                  MFSTARTUP_FULL);
  CheckHR(hr, 'MFStartup');

  QueryPerformanceFrequency(FPerfFreq);

  // Writer infra
  FQueueCS := nil;
  FQueueEvent := nil;
  FVideoQueue := nil;
  FAudioQueue := nil;
  FWriterThread := nil;
  FWriterRunning := False;

  // Audio defaults
  FAudioEnabled := True;
  FAudioCodec := acAac;
  ResetAudioGating();

  FAudioThresholdOnDb := -45.0;
  FAudioThresholdOffDb := -55.0;
  FAudioHoldOnMs := 100;
  FAudioHoldOffMs := 300;
  FAudioPreRollMs := 400;
  FAudioSampleRate := 0;
  FAudioChannels := 0;
  FAudioBitsPerSample := 0;
  FAudioBlockAlign := 0;
  FAudioAvgBytesPerSec := 0;
  FAudioChannelMask := 0;
  FAudioSourceIsFloat := False;
  FAudioBytesPerFrameOut := 0;
  FAudioChunkFramesTarget := 0;
  FAudioAccum := nil;
  FAudioAccumOffset := 0;
  FAudioAccumFrames := 0;
  FAudioMp4SampleDescription := nil;
  FWasapiCapture := nil;

  FFrameDuration100ns := 10000000 div FFrameRate;

  FCaptureMode := cmAudioVideo;
  FRecordVideo := True;
  FRecordAudio := True;
end;


destructor TCaptureStreamEngine.Destroy;
begin

  // Ensure capture is fully stopped and thread is gone
  StopCapture();

  FreeAndNil(FPreview);
  FreeAndNil(FConverter);

  FSinkWriter := nil;
  FDeskDupl := nil;
  FDXGIOutput1 := nil;
  FDXGIDevice := nil;
  FNV12StagingTexture := nil;

  MFShutdown();

  // Safety
  FreeAndNil(FWasapiCapture);
  FreeAndNil(FAudioPreRoll);
  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FQueueCS);

  inherited;
end;


procedure TCaptureStreamEngine.StartCapture(const OutputFile: string);
begin

  if FRunning then
    Exit;

  SetFrameWidth := FWidth;
  SetFrameHeight := FHeight;
  SetFrameRate := FFRameRate;

  // (Re)initialize everything for the current output.
  InitDesktopDuplication();
  CreateNV12Staging();

  FreeAndNil(FConverter); // safety if re-used
  FConverter := TGpuNV12Converter.Create(FDevice,
                                         FContext,
                                         FWidth,
                                         FHeight);
  // If audio is enabled, query the loopback mix format BEFORE creating the sink writer.
  // SinkWriter audio input type must be set before BeginWriting.
  if FAudioEnabled then
    PrepareAudioFormatFromLoopbackMix;

  ResetAudioGating;
  CreateSinkWriter(OutputFile);

  QueryPerformanceCounter(FQpcStart);
  FFrameIndex := 0;

  FRunning := True;
  FVideoRunning := True;

  // Start WASAPI loopback (captures always; we only write samples when audio triggers)
  if FAudioEnabled then
  begin
    FreeAndNil(FWasapiCapture);
    FreeAndNil(FAudioPreRoll);
    FWasapiCapture := TWasapiLoopbackCapture.Create;
    FWasapiCapture.OnData := OnWasapiData;
    FWasapiCapture.Start;
  end;

  // Dedicated worker thread
  FVideoThread := TThread.CreateAnonymousThread(procedure
                                                begin
                                                  try
                                                    VideoLoop();
                                                  except
                                                    on E: Exception do
                                                      if Assigned(FOnError) then
                                                        FOnError(Self, 'VideoLoop error: ' + E.Message);
                                                  end;
                                                end);
  FVideoThread.FreeOnTerminate := False;
  FVideoThread.Start;
end;


procedure TCaptureStreamEngine.StartCapture(const OutputFile: string;
                                            aDisplayOutput: TDXGIOutputInfo);
begin

  FDisplayOutputIndex := aDisplayOutput.OutputIndex;
  FSelectedDisplayName := aDisplayOutput.DeviceName;
  StartCapture(OutputFile);
end;


procedure TCaptureStreamEngine.StopCapture();
begin

  if not FRunning then
    Exit;

  // Stop producers first
  FVideoRunning := False;

  if Assigned(FWasapiCapture) then
    begin
      FWasapiCapture.Stop;
      FreeAndNil(FWasapiCapture);
      FreeAndNil(FAudioPreRoll);
    end;

  // Wait for worker thread to finish and free it
  if Assigned(FVideoThread) then
    begin
      FVideoThread.WaitFor;
      FreeAndNil(FVideoThread);
    end;

  // Stop writer thread (it finalizes the sink)
  FWriterRunning := False;
  if Assigned(FQueueEvent) then
    FQueueEvent.SetEvent;

  if Assigned(FWriterThread) then
    begin
      FWriterThread.WaitFor;
      FreeAndNil(FWriterThread);
    end;

  // Release sink + queues
  FSinkWriter := nil;
  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FAudioPreRoll);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FQueueCS);

  // Release GPU resources tied to duplication and staging
  FDeskDupl := nil;
  FDXGIOutput1 := nil;
  FDXGIDevice := nil;
  FNV12StagingTexture := nil;

  FreeAndNil(FConverter);

  FRunning := False;
end;

{ ==== Audio enable/disable (Option A) =================================== }

procedure TCaptureStreamEngine.EnableLoopbackAudioFlacMp4(const Mp4SampleDescription: TBytes;
                                                          const ChannelMask: Cardinal);
begin

  // Must be called before StartCapture. We create the audio stream at CreateSinkWriter.
  FAudioEnabled := True;
  FAudioMp4SampleDescription := Copy(Mp4SampleDescription);
  FAudioChannelMask := ChannelMask;

  FAudioCodec := acFlac;
  FAudioOutAvgBytesPerSec := 0; // use PCM rate unless explicitly set elsewhere

  // Sensible defaults (you can tune via properties)
  if FAudioThresholdOnDb = 0 then
    FAudioThresholdOnDb := -45.0;
  if FAudioThresholdOffDb = 0 then
    FAudioThresholdOffDb := -55.0;
  if FAudioHoldOnMs = 0 then
    FAudioHoldOnMs := 100;
  if FAudioHoldOffMs = 0 then
    FAudioHoldOffMs := 300;
  if FAudioPreRollMs = 0 then
    FAudioPreRollMs := 400;
end;


procedure TCaptureStreamEngine.EnableLoopbackAudioAacMp4(const ChannelMask: Cardinal;
                                                         const TargetAvgBytesPerSec: Cardinal);
begin

  // Must be called before StartCapture. We create the audio stream at CreateSinkWriter.
  FAudioEnabled := True;
  FAudioMp4SampleDescription := nil; // not used for standard MP4+AAC
  FAudioChannelMask := ChannelMask;

  FAudioCodec := acAac;

  // Target bitrate (bytes/sec). 20000 ~= 160 kbps.
  if TargetAvgBytesPerSec <> 0 then
    FAudioOutAvgBytesPerSec := TargetAvgBytesPerSec
  else
    FAudioOutAvgBytesPerSec := 20000;

  // Sensible defaults (you can tune via properties)
  if FAudioThresholdOnDb = 0 then
    FAudioThresholdOnDb := -45.0;
  if FAudioThresholdOffDb = 0 then
    FAudioThresholdOffDb := -55.0;
  if FAudioHoldOnMs = 0 then
    FAudioHoldOnMs := 150;
  if FAudioHoldOffMs = 0 then
    FAudioHoldOffMs := 250;
  if FAudioPreRollMs = 0 then
    FAudioPreRollMs := 350;
end;


procedure TCaptureStreamEngine.DisableAudio;
begin

  FAudioEnabled := False;
  FAudioMp4SampleDescription := nil;
  FAudioCodec := acNone;
  FAudioOutAvgBytesPerSec := 0;
end;


procedure TCaptureStreamEngine.SetCaptureMode(AMode: TCaptureMode);
begin
  FCaptureMode := AMode;
  FRecordVideo := False;
  FRecordAudio := False;

  // Destroy / recreate WASAPI capture when needed

  if Assigned(FWasapiCapture) then
    FreeAndNil(FWasapiCapture);

  case FCaptureMode of
    cmVideoOnly: begin

                   FRecordVideo := True;
                   FRecordAudio := False;
                 end;



    cmAudioVideo: begin

                    FRecordVideo := True;
                    FRecordAudio := True;
                    FWasapiCapture := TWasapiLoopbackCapture.Create(FAudioDeviceId);
                    FWasapiCapture.OnData := OnWasapiData;
                  end;
  end; // Case
end;


procedure TCaptureStreamEngine.SetAudioDeviceID(const Value: string);
begin

  if (FAudioDeviceId = Value) then
    Exit;

  FAudioDeviceId := Value;

  if Assigned(FWasapiCapture) then
    FWasapiCapture.AudioDeviceId := FAudioDeviceId;
end;


procedure TCaptureStreamEngine.ResetAudioGating();
begin

  if Assigned(FAudioPreRoll) then
    FAudioPreRoll.Clear;

  SetLength(FAudioAccum, 0);
  FAudioAccumOffset := 0;
  FAudioAccumFrames := 0;

  FAudioActive := False;
  FAudioEverActive := False;
  FAudioAboveMs := 0;
  FAudioBelowMs := 0;
  FAudioNextTime100ns := -1;
end;

{ ==== Queue / writer thread ================================================= }

procedure TCaptureStreamEngine.EnqueueSample(const StreamIndex: DWORD;
                                             const Sample: IMFSample;
                                             const Time100ns: Int64);
var
  item: TQueuedSample;

begin

  if (Sample = nil) then
    Exit;

  item.StreamIndex := StreamIndex;
  item.Sample := Sample;
  item.Time100ns := Time100ns;

  FQueueCS.Enter();

  try

    if StreamIndex = FVideoStreamIndex then
      FVideoQueue.Enqueue(item)
    else if StreamIndex = FAudioStreamIndex then
      FAudioQueue.Enqueue(item)
    else
      FVideoQueue.Enqueue(item); // fallback
  finally

    FQueueCS.Leave;
  end;

  FQueueEvent.SetEvent;
end;


function TCaptureStreamEngine.PopNextSample(out Item: TQueuedSample): Boolean;
var
  hasV,
  hasA: Boolean;
  vItem,
  aItem: TQueuedSample;

begin

  FillChar(Item,
           SizeOf(Item),
           0);

  FQueueCS.Enter();

  try

    hasV := (FVideoQueue.Count > 0);
    hasA := (FAudioQueue.Count > 0);

    if not hasV and not hasA then
      Exit(False);

    if hasV then
      vItem := FVideoQueue.Peek;
    if hasA then
      aItem := FAudioQueue.Peek;

    if hasV and (not hasA or (vItem.Time100ns <= aItem.Time100ns)) then
      begin
        Item := FVideoQueue.Dequeue;
        Exit(True);
      end
    else
      begin
        Item := FAudioQueue.Dequeue;
        Exit(True);
      end;
  finally
    FQueueCS.Leave;
  end;
end;


procedure TCaptureStreamEngine.SinkWriterLoop;
var
  hr: HRESULT;
  item: TQueuedSample;
  targetTime: Int64;
  needInject: Boolean;

begin

  // Single place where WriteSample is called (mux-style)
  try

    while FWriterRunning or
          (FVideoQueue.Count > 0) or
          (FAudioQueue.Count > 0) do
      begin

        // Wait for work or stop.
        FQueueEvent.WaitFor(50);

        // --- Audio watchdog ---
        // Some MP4 sinks behave poorly if an audio stream exists but no audio samples arrive for a while.
        // To keep the container happy and ensure stable interleaving, we inject SILENCE samples up to the
        // time of the next pending video sample when:
        //   - Audio is enabled AND stream exists
        //   - Audio queue is empty
        // This preserves "silence before first sound" while avoiding buffering/oom-like behavior.

        needInject := False;
        targetTime := -1;

        if FAudioEnabled and
           (FAudioStreamIndex <> FVideoStreamIndex) then
          begin

            FQueueCS.Enter();

            try

              if (FAudioQueue.Count = 0) and
                 (FVideoQueue.Count > 0) then
                begin
                  targetTime := FVideoQueue.Peek.Time100ns;
                  needInject := (targetTime >= 0);
                end;
            finally
              FQueueCS.Leave();
            end;

            if needInject then
              GenerateSilenceToTime(targetTime);
          end;

      while PopNextSample(item) do
        begin

          if (FSinkWriter <> nil) then
            begin
              hr := FSinkWriter.WriteSample(item.StreamIndex,
                                            item.Sample);
              CheckHR(hr, 'FSinkWriter.WriteSample (writer thread)');
            end;
        end;
    end;

    if (FSinkWriter <> nil) then
      begin

        hr := FSinkWriter.Finalize;
        CheckHR(hr, 'FSinkWriter.Finalize');
      end;
  except

    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, 'SinkWriterLoop error: ' + E.Message);
  end;
end;


procedure TCaptureStreamEngine.GenerateSilenceToTime(const TargetTime100ns: Int64);
var
  chunk: TAudioChunk;
  chunkFrames: Cardinal;
  chunkBytes: Integer;
  dur100nsChunk: Int64;
  i, maxChunks: Integer;

begin

  if (not FAudioEnabled) then
    Exit;

  if (FAudioStreamIndex = FVideoStreamIndex) then
    Exit;

  if (FAudioSampleRate = 0) or
     (FAudioChannels = 0) then
    Exit;

  if (TargetTime100ns < 0) then
    Exit;

  // Ensure output framing parameters.
  if (FAudioBytesPerFrameOut <= 0) then
    FAudioBytesPerFrameOut := Integer(FAudioChannels) * 2; // s16 interleaved

  if (FAudioChunkFramesTarget = 0) then
    begin

      // ~40ms chunks
      FAudioChunkFramesTarget := Cardinal(FAudioSampleRate div 25);
      if (FAudioChunkFramesTarget < 256) then
        FAudioChunkFramesTarget := 256;
    end;

  chunkFrames := FAudioChunkFramesTarget;
  chunkBytes := Integer(chunkFrames) * FAudioBytesPerFrameOut;
  dur100nsChunk := (Int64(chunkFrames) * 10000000) div Int64(FAudioSampleRate);

  if (dur100nsChunk <= 0) then
  Exit;

  // Initialize timeline if we never received an audio callback yet.
  if (FAudioNextTime100ns < 0) then
    FAudioNextTime100ns := 0;

  // Inject at most a small burst per wake-up (avoid stalling the writer thread).
  maxChunks := 6;
  i := 0;

  while (i < maxChunks) and
        ((FAudioNextTime100ns + dur100nsChunk) <= TargetTime100ns) do

    begin

      SetLength(chunk.Data,
                chunkBytes);
      if (chunkBytes > 0) then
        FillChar(chunk.Data[0],
                 chunkBytes,
                 0);

    chunk.Time100ns := FAudioNextTime100ns;
    chunk.Duration100ns := dur100nsChunk;
    Inc(FAudioNextTime100ns, dur100nsChunk);
    SubmitAudioChunk(chunk);
    Inc(i);
  end;
end;

{ ==== Audio sample creation + gating ==================================== }

procedure TCaptureStreamEngine.SubmitAudioChunk(const Chunk: TAudioChunk);
var
  hr: HRESULT;
  sample: IMFSample;
  buffer: IMFMediaBuffer;
  pData: PByte;
  maxLen,
  curLen: DWORD;

begin

  if (Chunk.Data = nil) or
     (Length(Chunk.Data) = 0) then
    Exit;

  hr := MFCreateSample(sample);
  CheckHR(hr, 'MFCreateSample(audio)');

  hr := MFCreateMemoryBuffer(Length(Chunk.Data), buffer);
  CheckHR(hr, 'MFCreateMemoryBuffer(audio)');

  hr := buffer.Lock(pData, @maxLen, @curLen);
  CheckHR(hr, 'IMFMediaBuffer.Lock(audio)');

  try

    Move(Chunk.Data[0],
         pData^,
         Length(Chunk.Data));

    hr := buffer.SetCurrentLength(Length(Chunk.Data));
    CheckHR(hr, 'IMFMediaBuffer.SetCurrentLength(audio)');
  finally

    buffer.Unlock();
  end;

  hr := sample.AddBuffer(buffer);
  CheckHR(hr, 'IMFSample.AddBuffer(audio)');

  hr := sample.SetSampleTime(Chunk.Time100ns);
  CheckHR(hr, 'IMFSample.SetSampleTime(audio)');

  hr := sample.SetSampleDuration(Chunk.Duration100ns);
  CheckHR(hr, 'IMFSample.SetSampleDuration(audio)');

  EnqueueSample(FAudioStreamIndex,
                sample,
                Chunk.Time100ns);
end;


function _IsFloatFormat(const wf: PWAVEFORMATEX): Boolean;
begin

  Result := False;

  if (wf = nil) then
    Exit(False);

  if (wf.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) then
    Exit(True);

  if (wf.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
    begin

      // WAVEFORMATEXTENSIBLE: SubFormat GUID
      // Avoid GUID compare complexity here; treat 32-bit as float if so tagged.
      // Many loopback devices deliver IEEE float via extensible.
      Result := (wf.wBitsPerSample = 32);
    end;
end;


function _CalcRmsDb(const Buf: Pointer;
                    const NumFrames: Cardinal;
                    const wf: PWAVEFORMATEX): Single;
var
  i,
  n: Integer;
  sumSq: Double;
  s16: PSmallInt;
  sf: PSingle;
  v: Double;
  ch: Integer;

begin

  Result := -120.0;
  if (Buf = nil) or
     (wf = nil) or
     (NumFrames = 0) then
    Exit;

  ch := wf.nChannels;
  if ch <= 0 then ch := 1;

  sumSq := 0.0;
  n := Integer(NumFrames) * ch;

  if _IsFloatFormat(wf) then
    begin

      sf := PSingle(Buf);

      for i := 0 to n-1 do
        begin
          v := sf^;
          sumSq := sumSq + (v*v);
          Inc(sf);
        end;
    v := Sqrt(sumSq / n);
    end
  else
    begin

      // Assume 16-bit PCM for RMS detection (common + good enough)
      if (wf.wBitsPerSample <> 16) then
        Exit(-120.0);

      s16 := PSmallInt(Buf);

      for i := 0 to n-1 do
        begin
          v := s16^ / 32768.0;
          sumSq := sumSq + (v*v);
          Inc(s16);
        end;
    v := Sqrt(sumSq / n);
  end;

  if (v < 1e-9) then
    Exit(-120.0);

  Result := 20.0 * Log10(v);
end;


procedure TCaptureStreamEngine.OnWasapiData(Sender: TObject;
                                            const Buffer: Pointer;
                                            NumFrames: Cardinal;
                                            const WaveFormat: PWAVEFORMATEX);

var
  dur100nsIn: Int64;
  dur100nsChunk: Int64;
  durMs: Integer;
  db: Single;
  bytesIn: Integer;
  bytesOut: Integer;
  qpcNow: Int64;
  now100ns: Int64;
  isFloatNow: Boolean;
  tmp: TBytes;
  oldLen: Integer;
  chunk: TAudioChunk;
  writeReal: Boolean;
  chunkBytes: Integer;
  chunkFrames: Cardinal;
  remainBytes: Integer;
  srcF: PSingle;
  dstS16: PSmallInt;
  i,
  n: Integer;
  v: Single;

begin

  if not FRunning then
    Exit;

  if (not FAudioEnabled) then
    Exit;

  // If the audio stream was not actually created, don't enqueue anything.
  // (Prevents accidentally enqueueing "audio" into the video queue.)
  if (FAudioStreamIndex = FVideoStreamIndex) then
    Exit;

  if (WaveFormat = nil) or
     (WaveFormat.nSamplesPerSec = 0) or
     (WaveFormat.nBlockAlign = 0) then
    Exit;

  if (Buffer = nil) or
     (NumFrames = 0) then
    Exit;

  // Duration in 100-ns units (input buffer)
  dur100nsIn := (Int64(NumFrames) * 10000000) div Int64(WaveFormat.nSamplesPerSec);

  if (dur100nsIn <= 0) then
    Exit;

  durMs := Integer(dur100nsIn div 10000);

  if (durMs <= 0) then
    durMs := 1;

  // Compute loudness from the *real* loopback buffer (even if we decide to write silence)
  db := _CalcRmsDb(Buffer,
                   NumFrames,
                   WaveFormat);

  // --- Gate logic with hysteresis + hold times ---
  if not FAudioActive then
    begin

       // Track "above" time to arm
       if (db >= FAudioThresholdOnDb) then
         Inc(FAudioAboveMs,
             durMs)
       else
         FAudioAboveMs := 0;

       if (FAudioAboveMs >= FAudioHoldOnMs) then
         begin

           FAudioActive := True;
           FAudioEverActive := True;
           FAudioBelowMs := 0;
         end;
    end
  else
    begin

    // Track "below" time to disarm
    if (db <= FAudioThresholdOffDb) then
      Inc(FAudioBelowMs,
          durMs)
    else
      FAudioBelowMs := 0;

    if (FAudioBelowMs >= FAudioHoldOffMs) then
      begin

        FAudioActive := False;
        FAudioAboveMs := 0;
        // (We keep writing samples, but as silence while inactive)
      end;
  end;

  // We chunk audio to reduce WriteSample call rate (important when encoding).
  // We always enqueue audio samples (silence while inactive) to keep MP4 sinks happy.

  // Compute 'now' based on the same master QPC start as video
  QueryPerformanceCounter(qpcNow);
  //This will ends in Range Check Error!
  //now100ns := MulDiv(qpcNow - FQpcStart,
  //                   10000000,
  //                   FPerfFreq);

  // MulDiv replacement for Int64 types.
  // Note: In later MfPack versions (>= 3.18), this method will be declared in WinApi.MediaFoundationApi.MfUtils.
  now100ns := _MulDiv64(qpcNow - FQpcStart,
                        10000000,
                        FPerfFreq);

  if (FAudioBytesPerFrameOut <= 0) then
    FAudioBytesPerFrameOut := Integer(WaveFormat.nChannels) * 2;

  if (FAudioChunkFramesTarget = 0) then
    begin

      // ~40ms target chunk (reduce WriteSample overhead)
      FAudioChunkFramesTarget := Cardinal(WaveFormat.nSamplesPerSec div 25);
      if (FAudioChunkFramesTarget < 256) then
        FAudioChunkFramesTarget := 256;
    end;

  // Initialise the audio timeline on first packet (align start to the beginning of this packet)
  if (FAudioNextTime100ns < 0) then
    begin

      FAudioNextTime100ns := now100ns - dur100nsIn;
      if (FAudioNextTime100ns < 0) then
        FAudioNextTime100ns := 0;
    end;

  // Decide whether to write real audio or silence for this incoming buffer
  writeReal := FAudioActive;

  bytesIn := Integer(NumFrames) * Integer(WaveFormat.nBlockAlign);
  if (bytesIn <= 0) then
    Exit;

  bytesOut := Integer(NumFrames) * FAudioBytesPerFrameOut;
  SetLength(tmp,
            bytesOut);

  if not writeReal then
    begin

      FillChar(tmp[0],
      bytesOut, 0);
    end
  else
    begin

      isFloatNow := _IsFloatFormat(WaveFormat) or FAudioSourceIsFloat;

      if isFloatNow then
        begin

          // Convert interleaved float32 [-1..1] -> interleaved s16
          srcF := PSingle(Buffer);
          dstS16 := PSmallInt(@tmp[0]);
          n := Integer(NumFrames) * Integer(WaveFormat.nChannels);

          for i := 0 to n-1 do
            begin

              v := srcF^;
              if (v > 1.0) then
                v := 1.0
              else
                if (v < -1.0) then
                  v := -1.0;

              dstS16^ := SmallInt(Round(v * 32767.0));
              Inc(srcF);
              Inc(dstS16);
            end;
        end
      else
        if (WaveFormat.wBitsPerSample = 16) and
           (WaveFormat.nBlockAlign = FAudioBytesPerFrameOut) then
          begin

            Move(Buffer^,
                 tmp[0],
                 bytesOut);
          end
    else
      begin

        // Unknown source format; write silence to avoid noise
        FillChar(tmp[0],
                 bytesOut, 0);
      end;
    end;

  // Append to accumulator
  oldLen := Length(FAudioAccum);
  SetLength(FAudioAccum,
            oldLen + bytesOut);
  Move(tmp[0],
       FAudioAccum[oldLen],
       bytesOut);
  Inc(FAudioAccumFrames,
      NumFrames);

  // Emit fixed-size chunks
  chunkFrames := FAudioChunkFramesTarget;
  chunkBytes := Integer(chunkFrames) * FAudioBytesPerFrameOut;
  dur100nsChunk := (Int64(chunkFrames) * 10000000) div Int64(WaveFormat.nSamplesPerSec);

  while (FAudioAccumFrames >= chunkFrames) do
    begin

      // Ensure contiguous data
      if (FAudioAccumOffset + chunkBytes) > Length(FAudioAccum) then
        begin

          remainBytes := Length(FAudioAccum) - FAudioAccumOffset;

          if (remainBytes > 0) then
            Move(FAudioAccum[FAudioAccumOffset],
                             FAudioAccum[0],
                             remainBytes);

          SetLength(FAudioAccum,
                    remainBytes);
          FAudioAccumOffset := 0;
        end;

      SetLength(chunk.Data,
                chunkBytes);

      Move(FAudioAccum[FAudioAccumOffset],
           chunk.Data[0],
           chunkBytes);

      Inc(FAudioAccumOffset,
          chunkBytes);
      Dec(FAudioAccumFrames,
          chunkFrames);

      // Periodic compaction to prevent unbounded growth
      if (FAudioAccumOffset > 65536) and
         (FAudioAccumOffset < Length(FAudioAccum)) then
        begin

          remainBytes := Length(FAudioAccum) - FAudioAccumOffset;

          if (remainBytes > 0) then
            Move(FAudioAccum[FAudioAccumOffset],
                 FAudioAccum[0],
                 remainBytes);

           SetLength(FAudioAccum,
                     remainBytes);
           FAudioAccumOffset := 0;
        end
      else
        if (FAudioAccumOffset >= Length(FAudioAccum)) then
          begin

            SetLength(FAudioAccum,
                      0);
            FAudioAccumOffset := 0;
          end;

      // Timestamp + enqueue
      chunk.Time100ns := FAudioNextTime100ns;
      chunk.Duration100ns := dur100nsChunk;

      Inc(FAudioNextTime100ns,
          dur100nsChunk);
      SubmitAudioChunk(chunk);
    end;
end;

end.
