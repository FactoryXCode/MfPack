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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
  WinApi.Messages,
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
  System.TimeSpan,
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
  {Application}
  PreviewRenderer,
  GpuNV12Converter,
  Helpers;

type
  PGpuCaptureNotification = ^TGpuCaptureNotification;
  TGpuCaptureNotification = record
    FrameIndex: Int64;
    Seconds: Double;
    ErrorMessage: UnicodeString;
    IsError: Boolean;
  end;

  TCaptureProgressEvent = procedure(Sender: TObject;
                                    FrameIndex: Int64;
                                    MilliSeconds: Double) of object;

  TCaptureErrorEvent = procedure(Sender: TObject;
                                 const Msg: string) of object;

  TDXGIOutputInfo = record
    OutputIndex: UINT;
    DeviceName: string;
  end;

  TCaptureStreamEngine = class
  private
    FNotifyWindow: HWND;
    FNotifyClosing: Boolean;
    FMFStarted: Boolean;
    procedure NotifyWindowProc(var Msg: TMessage);
    procedure DiscardNotifications();
    procedure QueueProgress(Sender: TObject;FrameIndex: Int64;Seconds: Double);
    procedure QueueError(Sender: TObject;const Text: string);
  private
    procedure VideoThreadProc();
  private

    // D3D / DXGI
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FPreview: TPreviewRenderer;

    FDXGIDevice: IDXGIDevice;
    FDXGIOutput1: IDXGIOutput1;
    FDeskDupl: IDXGIOutputDuplication;
    FOutputIndex: UINT; // selected monitor

    // NV12 staging
    FNV12StagingTexture: ID3D11Texture2D;

    // GPU converter
    FConverter: TGpuNV12Converter;

    // MF sink writer
    FSinkWriter: IMFSinkWriter;
    FVideoStreamIndex: DWORD;

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

    // Events
    FOnProgress: TCaptureProgressEvent;
    FOnError: TCaptureErrorEvent;

  protected

    procedure CreateD3DDevice();
    procedure InitDesktopDuplication();
    procedure CreateNV12Staging();
    procedure CreateSinkWriter(const OutputFile: string);
    procedure SubmitVideoSampleToSinkWriter(const mappedNV12: D3D11_MAPPED_SUBRESOURCE);
    procedure VideoLoop();

  public

    constructor Create(aHwnd: HWND;
                       aWidth,
                       aHeight: UINT;
                       aFrameRate: UINT32);
    destructor Destroy(); override;

    procedure StartCapture(const OutputFile: string); overload;
    procedure StartCapture(const OutputFile: string; aOutputIndex: UINT); overload;
    procedure StopCapture();

    function EnumerateOutputs: TArray<TDXGIOutputInfo>;

    property OutputIndex: UINT read FOutputIndex write FOutputIndex;

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
  FeatureLevels: array[0..6] of D3D_FEATURE_LEVEL = (D3D_FEATURE_LEVEL_11_1,
                                                     D3D_FEATURE_LEVEL_11_0,
                                                     D3D_FEATURE_LEVEL_10_1,
                                                     D3D_FEATURE_LEVEL_10_0,
                                                     D3D_FEATURE_LEVEL_9_3,
                                                     D3D_FEATURE_LEVEL_9_2,
                                                     D3D_FEATURE_LEVEL_9_1);
var
  hr: HResult;
  rUsedFeatureLevel: D3D_FEATURE_LEVEL;

begin
  hr := D3D11CreateDevice(nil,
                          D3D_DRIVER_TYPE_HARDWARE,
                          0,
                          D3D11_CREATE_DEVICE_BGRA_SUPPORT or D3D11_CREATE_DEVICE_DEBUG,  // For the best performance, shut the D3D11 debugger down on your final release! Remove "or D3D11_CREATE_DEVICE_DEBUG"
                          @FeatureLevels,
                          Length(FeatureLevels),
                          D3D11_SDK_VERSION,
                          @FDevice,
                          @rUsedFeatureLevel,  // Returns the feature level the device is using depending on your system's Direct3D runtime version.
                          @FContext);
  if hr = DXGI_ERROR_SDK_COMPONENT_MISSING then
    begin
      hr := D3D11CreateDevice(nil,
                              D3D_DRIVER_TYPE_HARDWARE,
                              0,
                              D3D11_CREATE_DEVICE_BGRA_SUPPORT,  // For the best performance, shut the D3D11 debugger down on your final release! Remove "or D3D11_CREATE_DEVICE_DEBUG"
                              @FeatureLevels,
                              Length(FeatureLevels),
                              D3D11_SDK_VERSION,
                              @FDevice,
                              @rUsedFeatureLevel,  // Returns the feature level the device is using depending on your system's Direct3D runtime version.
                              @FContext);
    end;
  CheckHR(hr, 'D3D11CreateDevice');

  FPreview := TPreviewRenderer.Create(FDevice, FContext);
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
  hr := adapter.EnumOutputs(FOutputIndex,
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


function TCaptureStreamEngine.EnumerateOutputs: TArray<TDXGIOutputInfo>;
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

  while adapter.EnumOutputs(outIdx, output) <> DXGI_ERROR_NOT_FOUND do
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


procedure TCaptureStreamEngine.CreateNV12Staging;
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

  hr := FSinkWriter.BeginWriting;
  CheckHR(hr, 'BeginWriting');
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
    PackGpuNV12(mappedNV12,FWidth,FHeight,pData);

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
  sampleTime := GpuCounterTime(qpcNow - FQpcStart,FPerfFreq);

  hr := sample.SetSampleTime(sampleTime);
  CheckHR(hr, 'IMFSample.SetSampleTime');

  hr := sample.SetSampleDuration(FFrameDuration100ns);
  CheckHR(hr, 'IMFSample.SetSampleDuration');

  hr := FSinkWriter.WriteSample(FVideoStreamIndex,
                                sample);
  CheckHR(hr, 'FSinkWriter.WriteSample');
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
        begin
          if hr >= 0 then FDeskDupl.ReleaseFrame();
          Break;
        end;

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
                  QueueError(Self, 'Access lost: ' + E.Message);
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
          SubmitVideoSampleToSinkWriter(mappedNV12);
        finally
          ctx.Unmap(FNV12StagingTexture, 0);
        end;

        // The mapped bytes are consumed before Unmap above.

        QueryPerformanceCounter(t1);
        FMilliSeconds := (t1 - t0) / FPerfFreq;

        Inc(FFrameIndex);

        if Assigned(FOnProgress) then
          QueueProgress(Self,
                      FFrameIndex,
                      FMilliSeconds);

      finally
        FDeskDupl.ReleaseFrame;
      end;
  end;
end;


procedure TCaptureStreamEngine.VideoThreadProc();
begin
  try
    VideoLoop();
  except
    on E: Exception do
      if Assigned(FOnError) then
        QueueError(Self,'VideoLoop error: ' + E.Message);
  end;
end;

procedure TCaptureStreamEngine.QueueProgress(Sender: TObject;FrameIndex: Int64;Seconds: Double);
var
  Notice: PGpuCaptureNotification;
begin
  if FNotifyClosing then Exit;
  New(Notice);
  Notice.FrameIndex := FrameIndex;
  Notice.Seconds := Seconds;
  Notice.IsError := False;
  if not PostMessage(FNotifyWindow,WM_APP + $510,0,LPARAM(Notice)) then Dispose(Notice);
end;

procedure TCaptureStreamEngine.QueueError(Sender: TObject;const Text: string);
var
  Notice: PGpuCaptureNotification;
begin
  if FNotifyClosing then Exit;
  New(Notice);
  Notice.ErrorMessage := Text;
  Notice.IsError := True;
  if not PostMessage(FNotifyWindow,WM_APP + $510,0,LPARAM(Notice)) then Dispose(Notice);
end;

procedure TCaptureStreamEngine.NotifyWindowProc(var Msg: TMessage);
var
  Notice: PGpuCaptureNotification;
begin
  if Msg.Msg = WM_APP + $510 then
    begin
      Notice := PGpuCaptureNotification(Msg.LParam);
      try
        if not FNotifyClosing then
          if Notice.IsError then
            begin
              if Assigned(FOnError) then FOnError(Self,Notice.ErrorMessage);
            end
          else if Assigned(FOnProgress) then
            FOnProgress(Self,Notice.FrameIndex,Notice.Seconds);
      finally Dispose(Notice);end;
      Msg.Result := 0;
    end
  else Msg.Result := DefWindowProc(FNotifyWindow,Msg.Msg,Msg.WParam,Msg.LParam);
end;

procedure TCaptureStreamEngine.DiscardNotifications();
var
  Msg: TMsg;
begin
  while PeekMessage(Msg,FNotifyWindow,WM_APP + $510,WM_APP + $510,PM_REMOVE) do
    Dispose(PGpuCaptureNotification(Msg.LParam));
end;

constructor TCaptureStreamEngine.Create(aHwnd: HWND;
                                        aWidth,
                                        aHeight: UINT;
                                        aFrameRate: UINT32);
var
  hr: HResult;

begin

  inherited Create;
  FNotifyWindow := AllocateHWnd(NotifyWindowProc);
  if (aWidth = 0) or (aHeight = 0) or (aFrameRate = 0) then
    raise EArgumentException.Create('Capture size and frame rate must be positive');

  FPreviewHandle := aHwnd;
  FWidth := aWidth;
  FHeight := aHeight;
  FFrameRate := aFrameRate;
  FOutputIndex := 0;

  FRunning := False;
  FVideoRunning := False;
  FVideoThread := nil;

  CreateD3DDevice();

  hr := MFStartup(MF_VERSION,
                  MFSTARTUP_FULL);
  CheckHR(hr, 'MFStartup');
  FMFStarted := True;

  QueryPerformanceFrequency(FPerfFreq);
  FFrameDuration100ns := 10000000 div FFrameRate;
end;


destructor TCaptureStreamEngine.Destroy;
begin
  FNotifyClosing := True;

  // Ensure capture is fully stopped and thread is gone
  StopCapture();

  FreeAndNil(FPreview);
  FreeAndNil(FConverter);

  FSinkWriter := nil;
  FDeskDupl := nil;
  FDXGIOutput1 := nil;
  FDXGIDevice := nil;
  FNV12StagingTexture := nil;

  if FMFStarted then MFShutdown();

  DiscardNotifications();
  if FNotifyWindow <> 0 then DeallocateHWnd(FNotifyWindow);
  inherited;
end;


procedure TCaptureStreamEngine.StartCapture(const OutputFile: string);
begin

  if FRunning then
    Exit;

  if (FWidth = 0) or (FHeight = 0) or (FFrameRate = 0) or
     ((FWidth and 1) <> 0) or ((FHeight and 1) <> 0) then
    raise EArgumentException.Create('NV12 dimensions must be positive and even; frame rate must be positive');
  FFrameDuration100ns := 10000000 div FFrameRate;
  DiscardNotifications();
  FRunning := True;
  try
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

  CreateSinkWriter(OutputFile);

  QueryPerformanceCounter(FQpcStart);
  FFrameIndex := 0;

  FRunning := True;
  FVideoRunning := True;

  // Dedicated worker thread
  FVideoThread := TGpuCaptureWorker.Create(VideoThreadProc);
  FVideoThread.FreeOnTerminate := False;
  FVideoThread.Start;
  except
    StopCapture();
    raise;
  end;
end;


procedure TCaptureStreamEngine.StartCapture(const OutputFile: string;
                                            aOutputIndex: UINT);
begin
  FOutputIndex := aOutputIndex;
  StartCapture(OutputFile);
end;


procedure TCaptureStreamEngine.StopCapture();
begin

  if not FRunning then
    Exit;

  // Signal loop to exit
  FVideoRunning := False;

  // Wait for worker thread to finish and free it
  if Assigned(FVideoThread) then
    begin
      FVideoThread.WaitFor;
      FreeAndNil(FVideoThread);
    end;

  // Stop MF sink
  if Assigned(FSinkWriter) then
    begin
      FSinkWriter.Finalize;
      FSinkWriter := nil;
    end;

  // Release GPU resources tied to duplication and staging
  FDeskDupl := nil;
  FDXGIOutput1 := nil;
  FDXGIDevice := nil;
  FNV12StagingTexture := nil;

  FreeAndNil(FConverter);

  FRunning := False;
end;

end.

