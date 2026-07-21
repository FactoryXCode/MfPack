// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LoopBackCapture.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: LoopBack Capture Engine (loopback deck and recorder).
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
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
//          Please, read documentation carefully!
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
// Source: FactoryX.Code.
// =============================================================================
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
unit LoopBackCapture;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.ObjIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioClientActivationParams,
  WinApi.CoreAudioApi.AudioSessionTypes,
  {WinMM}
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg;

type

  TDeviceState = (Uninitialized,
                  Error,
                  Initialized,
                  Starting,
                  Capturing,
                  Stopping,
                  Stopped);

  TAsyncCmd = (
    StartCapture,
    StopCapture,
    FinishCapture
  );

  TCapturedPcmEvent = procedure(Sender: TObject;
                                pData: PByte;
                                ByteCount: DWORD;
                                pwfx: PWAVEFORMATEX;
                                const CaptureFlags: DWORD) of object;

type

  TCallbackAsync = class;
  TRenderThread = class;

  TLoopbackCapture = class(TInterfacedPersistent, IActivateAudioInterfaceCompletionHandler, IAgileObject)
  protected

    pvRenderThread: TRenderThread;

  private

    pvAudioClient: IAudioClient;
    pvAudioCaptureClient: IAudioCaptureClient;

    pvRenderThreadClosedEvent: THandle;
    pvShutdownEvent: THandle;

    pvxStartCapture: TCallbackAsync;
    pvxStopCapture: TCallbackAsync;
    pvxFinishCapture: TCallbackAsync;

    FOnCapturingStart: TNotifyEvent;
    FOnCapturingStopped: TNotifyEvent;
    FOnCapturedPcm: TCapturedPcmEvent;

    gs_SampleReadyEvent: TEvent;
    gs_hActivateCompleted: TEvent;
    gs_hCaptureStopped: TEvent;

    pvMixFormat: WAVEFORMATEX;
    pvBufferFrames: UINT32;
    pvBufferDuration: REFERENCE_TIME;
    pvactivateResult: HResult;
    pvDeviceState: TDeviceState;

    function OnStartCapture(pResult: IMFAsyncResult): HResult;
    function OnStopCapture(pResult: IMFAsyncResult): HResult;
    function OnFinishCapture(pResult: IMFAsyncResult): HResult;

    function InitializeLoopbackCapture(): HResult;
    procedure GetMixFormat(out pMixFmt: WAVEFORMATEX);

    function ActivateAudioInterface(const processId: DWord;
                                    includeProcessTree: Boolean): HResult;
    function FinishCaptureAsync(): HResult;
    procedure Reset();
    procedure SetDeviceStateErrorIfFailed(hr: HResult);

    function CaptureThreadFunc(): HRESULT;
    procedure CreatedRenderThread();
    procedure TerminateRenderThread();
    function EventWait(EventObj: TEvent;
                       Period: Integer = 100): HResult;

  public

    constructor Create();
    destructor Destroy(); override;

    function ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HResult; stdcall;

    function StartCaptureAsync(const processId: DWord;
                               includeProcessTree: Boolean;
                               initialBufferSize: REFERENCE_TIME = 0): HResult;

    function StopCaptureAsync(): HResult;

    property CurrentMixFormat: TWAVEFORMATEX
      read pvMixFormat;

    property CaptureBufferLength: UINT32
      read pvBufferFrames;

    property OnStartCapturing: TNotifyEvent
      read FOnCapturingStart
      write FOnCapturingStart;

    property OnStoppedCapturing: TNotifyEvent
      read FOnCapturingStopped
      write FOnCapturingStopped;

    property OnCapturedPcm: TCapturedPcmEvent
      read FOnCapturedPcm
      write FOnCapturedPcm;
  end;

  TCallbackEvent = procedure(Sender: TObject;
                             hr: HRESULT) of object;

  TRenderThread = class(TThread)
  private

    FEngine: TLoopbackCapture;
    FSuccess: HResult;
    FOnEvent: TCallbackEvent;

  protected

    procedure Execute; override;
    procedure SetEvent;

  public

    constructor Create(AEngine: TLoopbackCapture);
    destructor Destroy; override;

    property OnEvent: TCallbackEvent read FOnEvent write FOnEvent;
  end;

  TCallbackAsync = class(TInterfacedPersistent, IMFAsyncCallback)

  private

    _parent: TLoopbackCapture;
    _AsyncCmd: TAsyncCmd;

  public

    constructor Create(AParent: TLoopbackCapture;
                       ASyncCmd: TAsyncCmd);
    destructor Destroy(); override;

    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;
    function Invoke(pResult: IMFAsyncResult): HResult; stdcall;
  end;


implementation


uses
  System.Services.Avrt;


{ TRenderThread }

constructor TRenderThread.Create(AEngine: TLoopbackCapture);
begin

  inherited Create(True);

  FEngine := AEngine;
  FreeOnTerminate := False;
end;


destructor TRenderThread.Destroy();
begin

  inherited;
end;


procedure TRenderThread.Execute();
begin

  FSuccess := FEngine.CaptureThreadFunc;
end;


procedure TRenderThread.SetEvent();
begin

  if Assigned(FOnEvent) then
    FOnEvent(Self,
             FSuccess);
end;


{ TLoopbackCapture }

constructor TLoopbackCapture.Create();
begin

  inherited Create();

  pvactivateResult := E_UNEXPECTED;
  pvDeviceState := Uninitialized;

  pvxStartCapture := TCallbackAsync.Create(Self,
                                           StartCapture);
  pvxStopCapture := TCallbackAsync.Create(Self,
                                          StopCapture);
  pvxFinishCapture := TCallbackAsync.Create(Self,
                                            FinishCapture);
end;


destructor TLoopbackCapture.Destroy();
begin

  Reset();

  FreeAndNil(pvxStartCapture);
  FreeAndNil(pvxStopCapture);
  FreeAndNil(pvxFinishCapture);

  inherited Destroy();
end;


function TLoopbackCapture.EventWait(EventObj: TEvent;
                                    Period: Integer = 100): HResult;
var
  wrWaitResult: TWaitResult;

begin

  wrWaitResult := EventObj.WaitFor(Period);

  case wrWaitResult of
    wrSignaled:     Result := S_OK;
    wrTimeout:      Result := HRESULT_FROM_WIN32(ERROR_TIMEOUT);
    wrAbandoned:    Result := HRESULT_FROM_WIN32(ERROR_TIMEOUT);
    wrError:        Result := HRESULT_FROM_WIN32(EventObj.LastError);
    wrIOCompletion: Result := S_OK;
  else
    Result := E_FAIL;
  end;
end;


procedure TLoopbackCapture.CreatedRenderThread;
begin

  if not Assigned(pvRenderThread) then
    begin
      pvRenderThread := TRenderThread.Create(Self);
      pvRenderThreadClosedEvent := CreateEventEx(nil,
                                                 nil,
                                                 0,
                                                 EVENT_MODIFY_STATE or SYNCHRONIZE);
      pvRenderThread.Start;
    end;
end;


procedure TLoopbackCapture.TerminateRenderThread();
begin

  if Assigned(pvRenderThread) then
    begin

      pvRenderThread.Terminate;
      pvRenderThread.WaitFor;
      FreeAndNil(pvRenderThread);
    end;

  if (pvRenderThreadClosedEvent <> 0) then
    begin

      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;
end;


function TLoopbackCapture.ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HRESULT;
var
  hr: HResult;
  hrActivateResult: HResult;

begin

  hrActivateResult := E_UNEXPECTED;

  hr := activateOperation.GetActivateResult(hrActivateResult,
                                            IUnknown(pvAudioClient));
  if FAILED(hrActivateResult) or FAILED(hr) then
    begin
      hr := hrActivateResult;
      Exit(hr);
    end;

  GetMixFormat(pvMixFormat);

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_LOOPBACK or AUDCLNT_STREAMFLAGS_EVENTCALLBACK,
                                 pvBufferDuration,
                                 0,
                                 @pvMixFormat,
                                 @GUID_NULL);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetBufferSize(pvBufferFrames);
  if FAILED(hr) then
    Exit(hr);

  pvAudioCaptureClient := nil;

  hr := pvAudioClient.SetEventHandle(gs_SampleReadyEvent.Handle);
  if FAILED(hr) then
    Exit(hr);

  pvDeviceState := Initialized;
  gs_hActivateCompleted.SetEvent();

  Result := hr;
end;


function TLoopbackCapture.OnStartCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  if not Assigned(pvAudioClient) then
    hr := E_POINTER
  else
    begin

      if (pvShutdownEvent <> 0) then
        ResetEvent(pvShutdownEvent);

      hr := pvAudioClient.Start();

      if SUCCEEDED(hr) then
        begin

          pvDeviceState := Capturing;
          CreatedRenderThread();
        end;
    end;

  SetDeviceStateErrorIfFailed(hr);
  Result := hr;
end;


function TLoopbackCapture.OnStopCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  pvDeviceState := Stopping;

  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  if Assigned(pvAudioClient) then
    hr := pvAudioClient.Stop()
  else
    hr := E_POINTER;

  TerminateRenderThread();
  pvAudioCaptureClient := nil;

  if SUCCEEDED(hr) then
    hr := FinishCaptureAsync()
  else
    pvDeviceState := Error;

  Result := hr;
end;


function TLoopbackCapture.OnFinishCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  hr := S_OK;

  gs_hCaptureStopped.SetEvent();
  if Assigned(FOnCapturingStopped) then
    FOnCapturingStopped(Self);

  pvDeviceState := Stopped;

  Result := hr;
end;


function TLoopbackCapture.InitializeLoopbackCapture(): HResult;
begin

  Reset();

  gs_SampleReadyEvent := TEvent.Create(nil,
                                      False,
                                       False,
                                       '',
                                       True);

  gs_hActivateCompleted := TEvent.Create(nil,
                                         False,
                                         False,
                                         '',
                                         True);

  gs_hCaptureStopped := TEvent.Create(nil,
                                      False,
                                      False,
                                      '',
                                      True);

  if (pvShutdownEvent = 0) then
    pvShutdownEvent := CreateEventEx(nil,
                                     nil,
                                     0,
                                     EVENT_MODIFY_STATE or SYNCHRONIZE)
  else
    ResetEvent(pvShutdownEvent);

  Result := S_OK;
end;


procedure TLoopbackCapture.GetMixFormat(out pMixFmt: WAVEFORMATEX);
begin

  pMixFmt := Default(WAVEFORMATEX);
  pMixFmt.wFormatTag := WAVE_FORMAT_PCM;
  pMixFmt.nChannels := 2;
  pMixFmt.nSamplesPerSec := 44100;
  pMixFmt.wBitsPerSample := 16;
  pMixFmt.nBlockAlign := (pMixFmt.nChannels * pMixFmt.wBitsPerSample) div BITS_PER_BYTE;
  pMixFmt.nAvgBytesPerSec := pMixFmt.nSamplesPerSec * pMixFmt.nBlockAlign;
end;


function TLoopbackCapture.CaptureThreadFunc(): HRESULT;
var
  hr: HRESULT;
  coHr: HRESULT;
  waitArray: array[0..1] of THandle;
  waitResult: DWORD;
  mmcssHandle: THandle;
  mmcssTaskIndex: DWORD;
  packetSize: UINT32;
  framesAvailable: UINT32;
  pData: PByte;
  dwCaptureFlags: DWORD;
  packetByteCount: DWORD;

begin

  pData := nil;
  packetSize := 0;
  framesAvailable := 0;
  dwCaptureFlags := 0;
  //packetByteCount := 0;
  mmcssHandle := 0;
  mmcssTaskIndex := 0;

  coHr := CoInitializeEx(nil,
                         COINIT_MULTITHREADED);
  try
    if (pvAudioClient = nil) then
      begin
        pvDeviceState := Error;
        Exit(E_POINTER);
      end;

    pvAudioCaptureClient := nil;

    hr := pvAudioClient.GetService(IID_IAudioCaptureClient,
                                   pvAudioCaptureClient);
    if FAILED(hr) or (pvAudioCaptureClient = nil) then
      begin
        pvDeviceState := Error;
        Exit(hr);
      end;

    mmcssHandle := AvSetMmThreadCharacteristics('Audio',
                                                @mmcssTaskIndex);

    waitArray[0] := pvShutdownEvent;
    waitArray[1] := gs_SampleReadyEvent.Handle;

    while (pvDeviceState = Capturing) do
      begin

        waitResult := WaitForMultipleObjects(2,
                                             @waitArray[0],
                                             False,
                                             INFINITE);

        if (waitResult = WAIT_OBJECT_0) then
          Break;

        if (waitResult <> (WAIT_OBJECT_0 + 1)) then
          begin

            pvDeviceState := Error;
            hr := HRESULT_FROM_WIN32(GetLastError());
            Break;
          end;

        while (pvDeviceState = Capturing) do
          begin

            hr := pvAudioCaptureClient.GetNextPacketSize(packetSize);
            if FAILED(hr) then
              begin

                pvDeviceState := Error;
                Break;
              end;

            if (packetSize = 0) then
              Break;

            hr := pvAudioCaptureClient.GetBuffer(pData,
                                                 framesAvailable,
                                                 dwCaptureFlags,
                                                 nil,
                                                 nil);
            if FAILED(hr) then
              begin
                pvDeviceState := Error;
                Break;
              end;

            try
              packetByteCount := framesAvailable * pvMixFormat.nBlockAlign;

              if ((dwCaptureFlags and AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) = 0) then
                begin

                  if Assigned(FOnCapturedPcm) then
                    begin

                      if ((dwCaptureFlags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                        FOnCapturedPcm(Self,
                                       nil,
                                       packetByteCount,
                                       @pvMixFormat,
                                       dwCaptureFlags)
                      else
                        FOnCapturedPcm(Self,
                                       pData,
                                       packetByteCount,
                                       @pvMixFormat,
                                       dwCaptureFlags);
                    end;
                end;
            finally

              pvAudioCaptureClient.ReleaseBuffer(framesAvailable);
              pData := nil;
            end;

            if (pvDeviceState <> Capturing) then
              Break;
          end;
      end;
  finally

    if (mmcssHandle <> 0) then
      AvRevertMmThreadCharacteristics(mmcssHandle);

    if SUCCEEDED(coHr) then
      CoUninitialize();

    if (pvRenderThreadClosedEvent <> 0) then
      SetEvent(pvRenderThreadClosedEvent);
  end;

  Result := hr;
end;


function TLoopbackCapture.ActivateAudioInterface(const processId: DWord;
                                                 includeProcessTree: Boolean): HResult;
var
  hr: HResult;
  audioclientActivationParams: AUDIOCLIENT_ACTIVATION_PARAMS;
  activateParams: PROPVARIANT;
  asyncOp: IActivateAudioInterfaceAsyncOperation;

label
  leave;

begin

  audioclientActivationParams := Default(AUDIOCLIENT_ACTIVATION_PARAMS);
  audioclientActivationParams.ActivationType := AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK;

  if includeProcessTree then
    audioclientActivationParams.ProcessLoopbackParams.ProcessLoopbackMode := PROCESS_LOOPBACK_MODE_INCLUDE_TARGET_PROCESS_TREE
  else
    audioclientActivationParams.ProcessLoopbackParams.ProcessLoopbackMode := PROCESS_LOOPBACK_MODE_EXCLUDE_TARGET_PROCESS_TREE;

  audioclientActivationParams.ProcessLoopbackParams.TargetProcessId := processId;

  PropVariantInit(activateParams);

  activateParams.vt := VT_BLOB;
  activateParams.blob.cbSize := SizeOf(audioclientActivationParams);
  activateParams.blob.pBlobData := PByte(@audioclientActivationParams);

  hr := ActivateAudioInterfaceAsync(LPCWSTR(VIRTUAL_AUDIO_DEVICE_PROCESS_LOOPBACK),
                                    IID_IAudioClient,
                                    activateParams,
                                    Self,
                                    asyncOp);
  if FAILED(hr) then
    goto leave;

  hr := EventWait(gs_hActivateCompleted);
  if SUCCEEDED(hr) then
    pvDeviceState := Initialized;

leave:

  PropVariantClearSafe(activateParams);
  SetDeviceStateErrorIfFailed(hr);
  Result := hr;
end;


function TLoopbackCapture.FinishCaptureAsync(): HResult;
var
  hr: HResult;

begin

  hr := MFPutWorkItem2(MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                       0,
                       pvxFinishCapture,
                       nil);
  if FAILED(hr) then
    pvDeviceState := Error;

  Result := hr;
end;


procedure TLoopbackCapture.Reset();
begin

  try

    if (pvShutdownEvent <> 0) then
      SetEvent(pvShutdownEvent);

    TerminateRenderThread();

    pvAudioCaptureClient := nil;
    pvAudioClient := nil;

    if Assigned(gs_SampleReadyEvent) then
      FreeAndNil(gs_SampleReadyEvent);

    if Assigned(gs_hActivateCompleted) then
      FreeAndNil(gs_hActivateCompleted);

    if Assigned(gs_hCaptureStopped) then
      FreeAndNil(gs_hCaptureStopped);

    if (pvShutdownEvent <> 0) then
      begin
        CloseHandle(pvShutdownEvent);
        pvShutdownEvent := 0;
      end;

    pvactivateResult := E_UNEXPECTED;
    pvDeviceState := Uninitialized;
  except
    Abort;
  end;
end;


procedure TLoopbackCapture.SetDeviceStateErrorIfFailed(hr: HResult);
begin

  if FAILED(hr) then
    pvDeviceState := Error;
end;


function TLoopbackCapture.StartCaptureAsync(const processId: DWord;
                                            includeProcessTree: Boolean;
                                            initialBufferSize: REFERENCE_TIME = 0): HResult;
var
  hr: HResult;

label
  leave;

begin

  pvBufferDuration := initialBufferSize;

  hr := InitializeLoopbackCapture();
  if FAILED(hr) then
    goto leave;

  hr := ActivateAudioInterface(processId,
                               includeProcessTree);
  if FAILED(hr) then
    goto leave;

  if (pvDeviceState = Initialized) then
    begin
      pvDeviceState := Starting;
      hr := MFPutWorkItem2(MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                           0,
                           pvxStartCapture,
                           nil);
      if FAILED(hr) then
        goto leave;
    end;

leave:
  Result := hr;
end;


function TLoopbackCapture.StopCaptureAsync(): HResult;
var
  hr: HResult;

begin

  hr := S_OK;

  if (pvDeviceState <> Capturing) and
     (pvDeviceState <> Error) then
    hr := E_NOT_VALID_STATE;

  if SUCCEEDED(hr) then
    begin
      pvDeviceState := Stopping;

      hr := MFPutWorkItem2(MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                           0,
                           pvxStopCapture,
                           nil);
      if SUCCEEDED(hr) then
        hr := EventWait(gs_hCaptureStopped);
    end;

  Result := hr;
end;

{ TCallbackAsync }

constructor TCallbackAsync.Create(AParent: TLoopbackCapture;
                                  ASyncCmd: TAsyncCmd);
begin
  inherited Create();
  _parent := AParent;
  _AsyncCmd := ASyncCmd;
end;


destructor TCallbackAsync.Destroy();
begin

  inherited Destroy();
end;


function TCallbackAsync.GetParameters(out pdwFlags: DWord;
                                      out pdwQueue: DWord): HResult;
begin

  pdwFlags := 0;
  pdwQueue := MFASYNC_CALLBACK_QUEUE_MULTITHREADED;
  Result := S_OK;
end;


function TCallbackAsync.Invoke(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  case _AsyncCmd of
    StartCapture:
      begin
        hr := _parent.OnStartCapture(pResult);
        if SUCCEEDED(hr) and Assigned(_parent.FOnCapturingStart) then
          _parent.FOnCapturingStart(Self);
      end;

    StopCapture:
      hr := _parent.OnStopCapture(pResult);

    FinishCapture:
      hr := _parent.OnFinishCapture(pResult);
  else
    hr := S_FALSE;
  end;

  Result := hr;
end;

end.

