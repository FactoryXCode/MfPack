// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LoopBackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.8
// Description: The audio loopback capture engine.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Jacob C.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/07/2025 All                 Ozzy Osbourne release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX318
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: LoopBackAudio Capture example.
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
unit LoopBackCapture;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Messages,
  {WinMM}
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {activeX}
  //WinApi.ActiveX,
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
  {Application}
  MfAudioWriter,
  Common;

var
  // Events should be declared in a global scope.
  gs_SampleReadyEvent: TEvent;
  gs_hActivateCompleted: TEvent;
  gs_hCaptureStopped: TEvent;

type

  TDeviceState = (Uninitialized,
                  Error,       // Implemented to prevent calls to IAudioCaptureClient.GetNextPacketSize.
                               // See: OnAudioSampleRequested() and error handling.
                  MaxFileSizeReached,
                  // All states >= Initialized will allow some methods
                  // to be called successfully on the Audio Client.
                  Initialized, // < from this one..
                  Starting,
                  Capturing,
                  Stopping,
                  Stopped); // < ..until here

  TAsyncCmd = (StartCapture,
               StopCapture,
               SampleReady,
               FinishCapture);

  TWavFormat = (fmt44100b16,
                fmt48000b24,
                fmt48000b32,
                fmt96000b24,
                fmt96000b32);

type

  // Forwarded classes.
  TCallbackAsync = class;
  TRenderThread = class;

  TLoopbackCapture = class(TInterfacedPersistent, IActivateAudioInterfaceCompletionHandler, IAgileObject)
  protected

    pvRenderThread: TRenderThread;

  private

    pvAudioClient: IAudioClient;
    pvAudioCaptureClient: IAudioCaptureClient;
    pvSampleReadyAsyncResult: IMFAsyncResult;

    pvRenderThreadClosedEvent: THandle;
    pvShutdownEvent: THandle;

    pvxStartCapture: TCallbackAsync;
    pvxStopCapture: TCallbackAsync;
    pvxSampleReady: TCallbackAsync;
    pvxFinishCapture: TCallbackAsync;

    FOnCapturingStart: TNotifyEvent;
    FOnCapturingStopped: TNotifyEvent;

    pvSampleReadyKey: MFWORKITEM_KEY;
    pvMixFormat: WAVEFORMATEX;
    pvWavFormat: TWavFormat;

    pvBufferFrames: UINT32;
    pvBufferDuration: REFERENCE_TIME;
    pvBytesWritten: Int64;
    pvdwTaskID: DWord;
    pvdwQueueID: DWord;
    // These two members are used to communicate between the main thread
    // and the ActivateCompleted callback.
    pvoutputFileName: LPCWSTR;
    pvactivateResult: HResult;

    pvDeviceState: TDeviceState;

    function OnStartCapture(pResult: IMFAsyncResult): HResult;
    function OnStopCapture(pResult: IMFAsyncResult): HResult;
    function OnFinishCapture(pResult: IMFAsyncResult): HResult;
    function OnSampleReady(pResult: IMFAsyncResult): HResult;

    function InitializeLoopbackCapture(): HResult;
    procedure GetMixFormat(out pMixFmt: WAVEFORMATEX;
                           WavFormat: TWavFormat = fmt44100b16);
    function CreateAudioFile(): HResult;

    function ActivateAudioInterface(const processId: DWord;
                                    includeProcessTree: Boolean): HResult;
    function FinishCaptureAsync(): HResult;
    procedure Reset();
    procedure SetDeviceStateErrorIfFailed(hr: HResult);

    //
    // Thread functions.
    //
    // Here the rendering takes place in a thread.
    function CaptureThreadFunc(): HRESULT;
    procedure CreatedRenderThread();
    procedure TerminateRenderThread();

  public

    constructor Create();
    destructor Destroy(); override;

  {$region 'IActivateAudioInterfaceCompletionHandler implementation'}
    function ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HResult; stdcall;
  {$endregion}

    function StartCaptureAsync(const hWindow: HWND;
                               const processId: DWord;
                               includeProcessTree: Boolean;
                               const outputFileName: LPCWSTR;
                               WavFormat: TWavFormat = fmt44100b16;
                               initialBufferSize: REFERENCE_TIME = 0): HResult;

    function StopCaptureAsync(): HResult;

    property CurrentWavFormat: TWAVEFORMATEX read pvMixFormat;
    property BytesWritten: Int64 read pvBytesWritten;
    property CaptureBufferLength: UINT32 read pvBufferFrames;

    // Notify events.
    property OnStartCapturing: TNotifyEvent read FOnCapturingStart write FOnCapturingStart;
    property OnStoppedCapturing: TNotifyEvent read FOnCapturingStopped write FOnCapturingStopped;
  end;

  //   ////////////////////////////////////////////////////////////////////////

  TCallbackAsync = class(TInterfacedPersistent, IMFAsyncCallback)
  private
    _parent: TLoopbackCapture;
    _dwQueueID: DWord;
    _AsyncCmd: TAsyncCmd;

  public

    constructor Create(AParent: TLoopbackCapture;
                       ASyncCmd: TAsyncCmd;
                       AQueueID: DWord = MFASYNC_CALLBACK_QUEUE_MULTITHREADED);
    destructor Destroy(); override;

  {$region 'IActivateAudioInterfaceCompletionHandler implementation'}
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;
    function Invoke(pResult: IMFAsyncResult): HResult; stdcall;
  {$endregion}

    procedure SetQueueID(dwQueueID: DWord);

  end;


  // This event type is used to pass back a HResult.
  TCallbackEvent = procedure(Sender: TObject;
                             const Hres: HRESULT) of object;

  // The thread we render after Start.
  TRenderThread = class(TThread)
  protected

    procedure Execute; override;
    procedure SetEvent;

  private

    FEngine: TLoopbackCapture;
    FSuccess: HResult; // Used internally when synchronizing the HRESULT for handling.
    FOnEvent: TCallbackEvent;

  public

    constructor Create(AEngine: TLoopbackCapture);
    destructor Destroy; override;
    property OnEvent: TCallbackEvent read FOnEvent write FOnEvent; // Triggered when a status changed.
  end;



implementation

uses
  System.Services.Avrt;


// Thread ======================================================================
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


procedure TRenderThread.Execute;
begin

  // The function where we render the audio data and
  // (de)activate the MMCSS feature.
  // See: https://learn.microsoft.com/en-us/windows/win32/procthread/multimedia-class-scheduler-service
  // To get the best performance, it's recommended to set "Best Performance" in Windows energy settings.
  FSuccess := FEngine.CaptureThreadFunc;
  Synchronize(SetEvent);
end;


procedure TRenderThread.SetEvent;
begin

  // Called from 'Synchronize'.
  // All code run from "Synchronize()"
  //   runs in the context of the Main VCL UI Thread, NOT from this thread.
  //   This simply triggers the event which was assigned by the calling thread
  //   to inform it that the download has completed.

  if Assigned(FOnEvent) then
    FOnEvent(Self,
             FSuccess);
end;

// =============================================================================
// Thread control methods ======================================================

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
      pvRenderThread.WaitFor; // Ensure capture thread finishes (and closes writer) before cleanup.
      FreeAndNil(pvRenderThread);
    end;

  if (pvRenderThreadClosedEvent <> 0) then
    begin

      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;
end;

// =============================================================================

constructor TLoopbackCapture.Create();
begin

  pvactivateResult := E_UNEXPECTED;
  pvDeviceState := Uninitialized;
  pvdwQueueID := 0;

  // Create the callback interfaces
  {StartCapture, StopCapture, SampleReady, FinishCapture}
  pvxStartCapture := TCallbackAsync.Create(Self,
                                           StartCapture);

  pvxStopCapture := TCallbackAsync.Create(Self,
                                          StopCapture);

  pvxSampleReady := TCallbackAsync.Create(Self,
                                          SampleReady);

  pvxFinishCapture := TCallbackAsync.Create(Self,
                                            FinishCapture);
  // Create the audio writer (created/opened inside capture thread).
end;


destructor TLoopbackCapture.Destroy();
begin

  Reset();
  FreeAndNil(pvxStartCapture);
  FreeAndNil(pvxStopCapture);
  FreeAndNil(pvxSampleReady);

  FreeAndNil(pvxFinishCapture);
  inherited Destroy();
end;


// IActivateAudioInterfaceCompletionHandler ////////////////////////////////////
//
//  ActivateCompleted()
//
//  Callback implementation of ActivateAudioInterfaceAsync function. This will be called on MTA thread
//  when results of the activation are available.
//
function TLoopbackCapture.ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HRESULT;
var
  hr: HResult;
  hrActivateResult: HResult;

begin

  // Check for a successful activation result
  hrActivateResult := E_UNEXPECTED;

  hr := activateOperation.GetActivateResult(hrActivateResult,
                                            IUnknown(pvAudioClient));
  if FAILED(hrActivateResult) or FAILED(hr) then
    begin
      hr := hrActivateResult;
      ErrMsg(Format('activateOperation.GetActivateResult failed. LastError = %d',[GetLastError()]), hrActivateResult);
      Exit(hr);
    end;

  // Set the WAV format to use.
  GetMixFormat(pvMixFormat,
               pvWavFormat);

  //
  // Initialize the AudioClient in Shared Mode with the user specified buffer.
  //
  // Note: - Shared Mode is needed when rendering from an audio application or process.
  //       - Exclusive Mode is used when rendering from a hardware endpoint.
  //       - Interface methods that are reffering to audioendpoints, will not work and returns E_NOTIMPL,
  //         for example: GetBufferSize(), IsFormatSupported(), GetDevicePeriod(), GetStreamLatency() and GetMixFormat() methods.
  //         See: https://learn.microsoft.com/en-us/answers/questions/1125409/loopbackcapture-(-activateaudiointerfaceasync-with?page=1&orderby=Helpful#answers
  //

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_LOOPBACK or
                                 AUDCLNT_STREAMFLAGS_EVENTCALLBACK,
                                 pvBufferDuration, // Note: When bufferDuration = 0, the audioclient will automaticly decise the bufferduration.
                                 0, // Must be zero in shared mode!
                                 @pvMixFormat,
                                 @GUID_NULL);
  if FAILED(hr) then
    Exit(hr);

  // Get the maximum size of the AudioClient Buffer
  hr := pvAudioClient.GetBufferSize(pvBufferFrames);
  if FAILED(hr) then
     Exit(hr);

  // v3-style threaded capture:
  // Acquire IAudioCaptureClient in the capture thread (CaptureThreadFunc) to avoid cross-thread COM issues.
  pvAudioCaptureClient := nil;

  // v3-style threaded capture:
  // No MF waiting work item chain is used for sample-ready notifications.
  pvSampleReadyAsyncResult := nil;

  // Tell the system which event handle it should signal when an audio buffer is ready to be processed by the client.
  hr := pvAudioClient.SetEventHandle(gs_SampleReadyEvent.Handle);
  if FAILED(hr) then
     Exit(hr);

  // Creates the WAV file.
  hr := CreateAudioFile();
  if FAILED(hr) then
     Exit(hr);

  // Everything is ready.
  pvDeviceState := Initialized;

  // Let ActivateAudioInterface know that pvactivateResult has the result of the activation attempt.
  gs_hActivateCompleted.SetEvent();

  Result := hr;
end;


////////////////////////////////////////////////////////////////////////////////

// PRIVATE

//
//  OnStartCapture()
//
//  Callback method to start the capture process.
//
function TLoopbackCapture.OnStartCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  if not Assigned(pvAudioClient) then
    hr := E_POINTER
  else
    begin

      // Reset shutdown signal (if reusing the object for multiple runs).
      if (pvShutdownEvent <> 0) then
        ResetEvent(pvShutdownEvent);

      // Start the capture (owned by v2 lifecycle: StartCapture work item).
      hr := pvAudioClient.Start();

      if SUCCEEDED(hr) then
        begin

          pvDeviceState := Capturing;

         // The render thread waits directly on gs_SampleReadyEvent.
         CreatedRenderThread();
        end;
    end;

  SetDeviceStateErrorIfFailed(hr);
  Result := hr;
end;


//
//  OnStopCapture()
//
//  Callback method to stop capture
//
function TLoopbackCapture.OnStopCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  pvDeviceState := Stopping;

  // Signal the capture thread to exit.
  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  // Stop the audio engine (best effort).
  if Assigned(pvAudioClient) then
    hr := pvAudioClient.Stop()
  else
    hr := E_POINTER;

  // Ensure capture thread exits (writer is finalized in-thread).
  TerminateRenderThread();

  // Sample-ready MF work items are not used in the v3-style capture loop.
  pvSampleReadyKey := 0;
  SafeRelease(pvSampleReadyAsyncResult);
  pvAudioCaptureClient := nil;

  if SUCCEEDED(hr) then
    begin
      hr := FinishCaptureAsync();
      if SUCCEEDED(hr) then
        pvDeviceState := Stopped
      else
        begin
          pvDeviceState := Error;
          if FAILED(hr) then
            ErrMsg(Format('FinishCaptureAsync failed. LastError = %d',[GetLastError()]), hr);
        end;
    end;

  Result := hr;
end;


//
//  OnFinishCapture()
//
//  Because of the asynchronous nature of the MF Work Queues and the DataWriter, there could still be
//  a sample processing. So this will get called to finalize the audio file.
//
function TLoopbackCapture.OnFinishCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  // Writer finalization happens inside the capture thread.
  hr := S_OK;

  gs_hCaptureStopped.SetEvent();

  pvDeviceState := Stopped;
  if Assigned(FOnCapturingStopped) then
    FOnCapturingStopped(Self);

  Result := hr;
end;


//
//  OnSampleReady()
//
//  Callback method when ready to fill sample buffer
//
function TLoopbackCapture.OnSampleReady(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin
  // v3 model: no MF waiting work items.
  // Sample processing happens in CaptureThreadFunc(), which waits on gs_SampleReadyEvent.
  hr := S_OK;
  Result := hr;
end;


function TLoopbackCapture.InitializeLoopbackCapture(): HResult;
var
  hr: HResult;

begin

  pvdwTaskID := 0;
  Reset();

  // Create events for sample ready or user stop.
  gs_SampleReadyEvent := TEvent.Create(nil,
                                       False,
                                       False,
                                       '',
                                       True);

  // Register MMCSS work queue.
  hr := MFLockSharedWorkQueue(PWideChar('Capture'),
                              MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                              pvdwTaskID,
                              pvdwQueueID);
  if FAILED(hr) then
    begin
      ErrMsg(Format('MFLockSharedWorkQueue failed. LastError = %d',[GetLastError()]), hr);
      Exit(hr);
    end;

  // Set the capture event work queue to use the MMCSS queue.
  pvxSampleReady.SetQueueID(pvdwQueueID);

  // Create the completion event as auto-reset.
  gs_hActivateCompleted := TEvent.Create(nil,
                                         False,
                                         False,
                                         '',
                                         True);

  // Create the capture-stopped event as auto-reset.
  gs_hCaptureStopped := TEvent.Create(nil,
                                      False,
                                      False,
                                      '',
                                      True);

  // Shutdown event for the capture thread (manual reset not required here; we reset on start).
  if (pvShutdownEvent = 0) then
    pvShutdownEvent := CreateEventEx(nil, nil, 0, EVENT_MODIFY_STATE or SYNCHRONIZE)
  else
    ResetEvent(pvShutdownEvent);

  Result := hr;
end;


//  Set the format we'll use to capture samples.
//  This can be PCM 44.1 16 bit or other user defined.
//
procedure TLoopbackCapture.GetMixFormat(out pMixFmt: WAVEFORMATEX;
                                        WavFormat: TWavFormat = fmt44100b16);
begin

  pMixFmt := Default(WAVEFORMATEX);

  // We only support PCM formats in stereo.
  pMixFmt.wFormatTag := WAVE_FORMAT_PCM;
  pMixFmt.nChannels := 2;

  // set the formats: fmt44100b16, fmt48000b24, fmt48000b32, fmt96000b24, fmt96000b32
  if (WavFormat = fmt44100b16) then
    begin
      pMixFmt.nSamplesPerSec := 44100;
      pMixFmt.wBitsPerSample := 16;
    end
  else if (WavFormat = fmt48000b24) then
    begin
      pMixFmt.nSamplesPerSec := 48000;
      pMixFmt.wBitsPerSample := 24;
    end
  else if (WavFormat = fmt48000b32) then
    begin
      pMixFmt.nSamplesPerSec := 48000;
      pMixFmt.wBitsPerSample := 32;
    end
  else if (WavFormat = fmt96000b24) then
    begin
      pMixFmt.nSamplesPerSec := 96000;
      pMixFmt.wBitsPerSample := 24;
    end
  else if (WavFormat = fmt96000b32) then
    begin
      pMixFmt.nSamplesPerSec := 96000;
      pMixFmt.wBitsPerSample := 32;
    end
  else // Default
    begin
      pMixFmt.nSamplesPerSec := 44100;
      pMixFmt.wBitsPerSample := 16;
    end;

  pMixFmt.nBlockAlign := (pMixFmt.nChannels * pMixFmt.wBitsPerSample) div BITS_PER_BYTE;
  pMixFmt.nAvgBytesPerSec := (pMixFmt.nSamplesPerSec * pMixFmt.nBlockAlign);
end;


//
//  CreateAudioFile()
//
//  Creates a WAV file in music folder.
//
function TLoopbackCapture.CreateAudioFile(): HResult;
var
  hr: HResult;
  fn: string;
  w: IAudioWriter;

begin

  hr := S_OK;

  pvBytesWritten := 0;
  // Validate output format early (writer is created/opened in the capture thread).
  fn := WideCharToString(pvoutputFileName);
  w := CreateAudioWriterFromFileName(fn);
  if (w = nil) then
    Exit(E_FAIL);

  Result := hr;
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
  dwBytesWritten: UINT32;

  fn: string;
  audioWriter: IAudioWriter;
  audioTime100ns: Int64;

begin

  pData := nil;
  packetSize := 0;
  framesAvailable := 0;
  dwCaptureFlags := 0;
  dwBytesWritten := 0;

  audioWriter := nil;
  audioTime100ns := 0;

  mmcssHandle := 0;
  mmcssTaskIndex := 0;

  // Ensure COM is initialized in this thread. This is important because we acquire
  // IAudioCaptureClient and call WASAPI methods from this thread (v3 model).
  coHr := CoInitializeEx(nil,
                         COINIT_MULTITHREADED);

  try

    // Thread-local writer (v3 rule).
    fn := WideCharToString(pvoutputFileName);
    audioWriter := CreateAudioWriterFromFileName(fn);

    if (audioWriter = nil) then
      begin

        pvDeviceState := Error;
        Exit(E_FAIL);
      end;

    hr := audioWriter.Open(fn,
                           pvMixFormat);
    if FAILED(hr) then
      begin

        pvDeviceState := Error;
        Exit(hr);
      end;

    // Acquire IAudioCaptureClient in THIS thread (v3 rule).
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

    // MMCSS (best effort).
    mmcssHandle := AvSetMmThreadCharacteristics('Audio',
                                                @mmcssTaskIndex);

    // Wait for shutdown or sample-ready.
    waitArray[0] := pvShutdownEvent;
    waitArray[1] := gs_SampleReadyEvent.Handle;

    while (pvDeviceState = Capturing) do
      begin

        waitResult := WaitForMultipleObjects(2,
                                             @waitArray[0],
                                             False,
                                             INFINITE);

        if (waitResult = WAIT_OBJECT_0) then
          Break; // shutdown

        if (waitResult <> (WAIT_OBJECT_0 + 1)) then
          begin

            pvDeviceState := Error;
            hr := HRESULT_FROM_WIN32(GetLastError());
            Break;
          end;

        // Drain ALL pending packets (v3 rule).
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

              // DATA_DISCONTINUITY: skip packet but still release the buffer.
              if ((dwCaptureFlags and AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) <> 0) then
                begin

                 // skip
                end
              else
                begin

                  // SILENT: write zeros.
                  if ((dwCaptureFlags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                    pData := nil;

                  hr := audioWriter.WriteFrames(pData,
                                                framesAvailable,
                                                pvMixFormat,
                                                audioTime100ns,
                                                dwBytesWritten);
                  if FAILED(hr) then
                    begin

                      pvDeviceState := Error;
                      Break;
                    end;

                  Inc(pvBytesWritten,
                      dwBytesWritten);
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

    // Finalize writer in this thread (v3 rule).
    if (audioWriter <> nil) then
      try
        audioWriter.Close();
      except
        // ignore shutdown errors.
      end;
    audioWriter := nil;

    if (mmcssHandle <> 0) then
      AvRevertMmThreadCharacteristics(mmcssHandle);

    if SUCCEEDED(coHr) then
      CoUninitialize();

    // Signal thread exit (legacy event used by Reset/Stop paths).
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
    begin
      ErrMsg(Format('ActivateAudioInterfaceAsync failed. LastError = %d',[GetLastError()]), hr);
      goto leave;
    end;

  hr := EventWait(gs_hActivateCompleted);
    if SUCCEEDED(hr) then
      pvDeviceState := Initialized;

leave:

  //PropVariantClear(activateParams);  // Works in Delphi XE7, but not in 10.3, where raises an exception.
  PropVariantClearSafe(activateParams);
  SetDeviceStateErrorIfFailed(hr);

  Result := hr;
end;


//
//  FinishCaptureAsync()
//
//  Finalizes WAV file on a separate thread via MF Work Item.
//
function TLoopbackCapture.FinishCaptureAsync(): HResult;
var
  hr: HResult;

begin

  // We should be flushing when this is called.
  hr := MFPutWorkItem2(MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                       0,
                       pvxFinishCapture,
                       nil);
  if FAILED(hr) then
    begin
      pvDeviceState := Error;
      ErrMsg(Format('MFPutWorkItem2 failed. LastError = %d',[GetLastError()]), hr);
    end;

  Result := hr;
end;


procedure TLoopbackCapture.Reset();
var
  hr: HResult;

begin
  hr := S_OK;

try

  // Stop and destroy the capture thread (if still running).
  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  TerminateRenderThread();

  // Shutdown event is created per capture session.
  if (pvShutdownEvent <> 0) then
    begin

      CloseHandle(pvShutdownEvent);
      pvShutdownEvent := 0;
    end;

  pvactivateResult := E_UNEXPECTED;
  pvDeviceState := Uninitialized;
  if (pvdwQueueID <> 0) then
    MFUnlockWorkQueue(pvdwQueueID);

  // Free the events but reset first and then wait until all events are processed.
  if Assigned(gs_SampleReadyEvent) then
    begin
      gs_SampleReadyEvent.ResetEvent;
      hr := EventWait(gs_SampleReadyEvent);
      FreeAndNil(gs_SampleReadyEvent);
    end;

  if Assigned(gs_hActivateCompleted) then
    begin
      gs_hActivateCompleted.ResetEvent;
      hr := EventWait(gs_hActivateCompleted);
      FreeAndNil(gs_hActivateCompleted);
    end;

  if Assigned(gs_hCaptureStopped) then
    begin
      gs_hCaptureStopped.ResetEvent;
      hr := EventWait(gs_hCaptureStopped);
      FreeAndNil(gs_hCaptureStopped);
    end;

  if FAILED(hr) then
    ErrMsg(Format('A ResetEvent failed. LastError = %d',[GetLastError()]), hr);

except
  Abort;
end;
end;


procedure TLoopbackCapture.SetDeviceStateErrorIfFailed(hr: HResult);
begin

  if FAILED(hr) then
    pvDeviceState := Error;
end;


// PUBLIC

function TLoopbackCapture.StartCaptureAsync(const hWindow: HWND;
                                            const processId: DWord;
                                            includeProcessTree: Boolean;
                                            const outputFileName: LPCWSTR;
                                            WavFormat: TWavFormat = fmt44100b16;
                                            initialBufferSize: REFERENCE_TIME = 0): HResult;
var
  hr: HResult;

label
  leave;

begin

  if (outputFileName = nil) then
    begin
      hr := E_POINTER;
      goto leave;
    end;

  if (hWindow = 0) then
    begin
      hr := E_POINTER;
      goto leave;
    end;

  pvoutputFileName := outputFileName;
  pvWavFormat := WavFormat;

  hr := InitializeLoopbackCapture();
  if FAILED(hr) then
    begin
      ErrMsg(Format('InitializeLoopbackCapture failed. LastError = %d',[GetLastError()]), hr);
      goto leave;
    end;

  // Activate the audio interface.
  hr := ActivateAudioInterface(processId,
                               includeProcessTree);
  if FAILED(hr) then
    begin
      ErrMsg(Format('ActivateAudioInterface failed. LastError = %d',[GetLastError()]), hr);
      goto leave;
    end;

  // We should be in the initialzied state if this is the first time through getting ready to capture.
  if (pvDeviceState = Initialized) then
    begin
      pvDeviceState := Starting;
      hr := MFPutWorkItem2(MFASYNC_CALLBACK_QUEUE_MULTITHREADED,
                           0,
                           pvxStartCapture,  // The callback interface.
                           nil);
      if FAILED(hr) then
        begin
          ErrMsg(Format('MFPutWorkItem2 failed. LastError = %d',[GetLastError()]), hr);
          goto leave;
        end;
    end;

leave:
  Result := hr;
end;


//
//  StopCaptureAsync()
//
//  Stop capture asynchronously via MF Work Item.
//
function TLoopbackCapture.StopCaptureAsync(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  if (pvDeviceState <> Capturing) and
     (pvDeviceState <> Error) then
    begin
      hr := E_NOT_VALID_STATE;
    end;

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


// TAsyncCallback //////////////////////////////////////////////////////////////

constructor TCallbackAsync.Create(AParent: TLoopbackCapture;
                                  ASyncCmd: TAsyncCmd;
                                  AQueueID: DWord);
begin

  inherited Create();

  _parent := AParent;
  _AsyncCmd := ASyncCmd;
  _dwQueueID := AQueueID;
end;


destructor TCallbackAsync.Destroy();
begin

  //
  inherited Destroy();
end;


function TCallbackAsync.GetParameters(out pdwFlags: DWord;
                                      out pdwQueue: DWord): HResult;
begin

  pdwFlags := 0;
  pdwQueue := _dwQueueID;
  Result := S_OK;
end;


// All callbacks are derived from this and will process this invoke.
function TCallbackAsync.Invoke(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  case _AsyncCmd of
    StartCapture:  begin
                     hr := _parent.OnStartCapture(pResult);
                     if SUCCEEDED(hr) then
                       _parent.FOnCapturingStart(Self);
                   end;
    StopCapture:   hr := _parent.OnStopCapture(pResult);
    SampleReady:   hr := _parent.OnSampleReady(pResult);
    FinishCapture: hr := _parent.OnFinishCapture(pResult);
    else
      hr := S_FALSE;  // No error, but wrong command.
  end;
  Result := hr;
end;


procedure TCallbackAsync.SetQueueID(dwQueueID: DWord);
begin

  _dwQueueID := dwQueueID;
end;

end.
