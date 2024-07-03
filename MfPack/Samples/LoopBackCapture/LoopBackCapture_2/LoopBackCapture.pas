// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: ThreadedLoopBackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 16-06-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: The threaded audio loopbackcapture engine.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Jacob C.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
  Writer,
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
    pvcbHeaderSize: DWord;
    pvWAVWriter: TWavWriter;
    ckRIFF: MMCKINFO;
    ckData: MMCKINFO;

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
    function CreateWAVFile(): HResult;

    function OnAudioSampleRequested(): HResult;

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
  // Initialize COM concurrency model multithreaded.
  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);
end;


destructor TRenderThread.Destroy();
begin
  CoUnInitialize;
  inherited;
end;


procedure TRenderThread.Execute;
begin

  // The function where we render the audio data and
  // (de)activate the MMCSS feature.
  // See: https://learn.microsoft.com/en-us/windows/win32/procthread/multimedia-class-scheduler-service
  // To get the best performance, it's recomended to set "Best Performance" in Windows energy settings.
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
      pvRenderThread.SetFreeOnTerminate(True);
      pvRenderThread.Terminate;
      FreeAndNil(pvRenderThread);
      // Signal the thread is terminated.
      if (pvRenderThreadClosedEvent <> 0) then
        begin
          SetEvent(pvRenderThreadClosedEvent);
          WaitForSingleObject(pvRenderThreadClosedEvent,
                              INFINITE);
          CloseHandle(pvRenderThreadClosedEvent);
          pvRenderThreadClosedEvent := 0;
        end;
    end;
end;

// =============================================================================

constructor TLoopbackCapture.Create();
begin

  pvactivateResult := E_UNEXPECTED;
  pvDeviceState := Uninitialized;
  pvdwQueueID := 0;
  pvcbHeaderSize := 0;

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
  // Create the WAV-filewriter.
  pvWAVWriter := TWavWriter.Create;
end;


destructor TLoopbackCapture.Destroy();
begin

  Reset();
  FreeAndNil(pvxStartCapture);
  FreeAndNil(pvxStopCapture);
  FreeAndNil(pvxSampleReady);
  FreeAndNil(pvxFinishCapture);
  if Assigned(pvWavWriter) then
    FreeAndNil(pvWavWriter);
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
  // See: https://learn.microsoft.com/en-us/answers/questions/1125409/loopbackcapture-(-activateaudiointerfaceasync-with?page=1&orderby=Helpful#answers
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

  // Get the capture client
  hr := pvAudioClient.GetService(IID_IAudioCaptureClient,
                                 pvAudioCaptureClient);
  if FAILED(hr) then
     Exit(hr);

  // Create Async callback for sample events.
  hr := MFCreateAsyncResult(nil,
                            pvxSampleReady,
                            nil,
                            pvSampleReadyAsyncResult);
  if FAILED(hr) then
     Exit(hr);

  // Tell the system which event handle it should signal when an audio buffer is ready to be processed by the client.
  hr := pvAudioClient.SetEventHandle(gs_SampleReadyEvent.Handle);
  if FAILED(hr) then
     Exit(hr);

  // Creates the WAV file.
  hr := CreateWAVFile();
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

  if Assigned(pvAudioClient) then
    begin
      // Start the capture
      hr := pvAudioClient.Start();

      if SUCCEEDED(hr) then
        begin
          pvDeviceState := Capturing;

          // We have an issue here. hr  = -2147467261
          hr := MFPutWaitingWorkItem(gs_SampleReadyEvent.Handle,
                                     0,
                                     pvSampleReadyAsyncResult,
                                     pvSampleReadyKey);
        end;
    end
  else
    hr := E_POINTER;

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
  // Stop capture by cancelling Work Item
  // Cancel the queued work item (if any)
  if (pvSampleReadyKey <> 0) then
    begin
        hr := MFCancelWorkItem(pvSampleReadyKey);
        if FAILED(hr) then
          ErrMsg(Format('MFCancelWorkItem failed. LastError = %d',[GetLastError()]), hr);
        pvSampleReadyKey := 0;
    end;

  hr := pvAudioClient.Stop();

  // Tell the capture thread to shut down, wait for the thread to complete then clean up all the stuff we
  // allocated in OnAudioSampleRequested().
  TerminateRenderThread();

  SafeRelease(pvSampleReadyAsyncResult);

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
//  a sample processing. So this will get called to finalize the WAV header.
//
function TLoopbackCapture.OnFinishCapture(pResult: IMFAsyncResult): HResult;
var
  hr: HResult;
  mmRes: MMRESULT;

begin
  pvDeviceState := Stopping;

  // Close the WAV-File.
  mmRes := pvWavWriter.CloseFile(ckRIFF,
                                 ckData);

  if (mmRes = MMSYSERR_NOERROR) then
    begin
      gs_hCaptureStopped.SetEvent();
      hr := EventWait(gs_hCaptureStopped);
      if SUCCEEDED(hr) then
        begin
          pvDeviceState := Stopped;
          FOnCapturingStopped(Self);
        end;
    end
  else
    begin
      pvDeviceState := Error;
      gs_hCaptureStopped.SetEvent();
      hr := EventWait(gs_hCaptureStopped);
    end;

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
  hr := OnAudioSampleRequested();
  if SUCCEEDED(hr) then
    begin
      // Re-queue work item for next sample.
      if (pvDeviceState = Capturing) then
        begin
          // Re-queue work item for next sample.
          hr := MFPutWaitingWorkItem(gs_SampleReadyEvent.Handle,
                                     0,
                                     pvSampleReadyAsyncResult,
                                     pvSampleReadyKey);
        end;
    end
  else
    pvDeviceState := Error;

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
  else if (WavFormat = fmt48000b24) then
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
//  CreateWAVFile()
//
//  Creates a WAV file in music folder.
//
function TLoopbackCapture.CreateWAVFile(): HResult;
var
  mmRes: MMRESULT;
  hr: HResult;

begin
  hr := S_OK;
  pvBytesWritten := 0;

  mmRes := pvWavWriter.CreateFile(pvoutputFileName);
  if (mmRes <> MMSYSERR_NOERROR) then
    begin
      hr := E_FAIL;
      ErrMsg(Format('CreateFile(%s) failed. LastError = %d',
                    [WideCharToString(pvoutputFileName),
                    GetLastError()]), hr);
      Exit(hr);
    end;

  // Write the wavfile header.
  mmRes := pvWavWriter.WriteWaveHeader(@pvMixFormat,
                                       ckRIFF,
                                       ckData);
  if (mmRes <> MMSYSERR_NOERROR) then
    begin
      ErrMsg(Format('Unable to write the wavfile header: %d',[GetLastError()]), E_FAIL);
      Exit(E_FAIL);
    end;

  Inc(pvBytesWritten,
      pvWavWriter.TotalBytesWritten);
  Result := hr;
end;


function TLoopbackCapture.CaptureThreadFunc(): HRESULT;
var
  hr: HResult;
  mmRes: MMRESULT;
  packetSize: UINT32;
  framesAvailable: UINT32;
  pData: PByte;
  dwCaptureFlags: DWord;
  dwBytesWritten: LongInt;

begin

  hr := S_OK;
  pData := nil;
  pvBufferFrames := 0;

  // A word on why we have a loop here;
  // Suppose it has been 10 milliseconds or so since the last time
  // this routine was invoked, and that we're capturing 48000 samples per second.
  //
  // The audio engine can be reasonably expected to have accumulated about that much
  // audio data - that is, about 480 samples.
  //
  // However, the audio engine is free to accumulate this in various ways:
  // a. as a single packet of 480 samples, OR
  // b. as a packet of 80 samples plus a packet of 400 samples, OR
  // c. as 48 packets of 10 samples each.
  //
  // In particular, there is no guarantee that this routine will be
  // run once for each packet.
  //
  // So every time this routine runs, we need to read ALL the packets
  // that are now available;
  //
  // We do this by calling IAudioCaptureClient.GetNextPacketSize
  // over and over again until it indicates there are no more packets remaining.

  // The original code: while SUCCEEDED(pvAudioCaptureClient.GetNextPacketSize(FramesAvailable)) and (FramesAvailable > 0) do

  // We check the device state first and then call IAudioCaptureClient.GetNextPacketSize.
  // This way we handle the internal async calls that could interfere first.

try

  while SUCCEEDED(pvAudioCaptureClient.GetNextPacketSize(packetSize)) and (packetSize > 0) do
    begin

      hr := pvAudioCaptureClient.GetBuffer(pData,
                                           FramesAvailable,
                                           dwCaptureFlags,
                                           nil,
                                           nil);

      if SUCCEEDED(hr) then
        begin

          if (dwCaptureFlags = AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) then
            begin
              pData := nil;
              Continue;
            end;

          // Tell WriteData to write silence.
          // When a sound is detected, the app will act and process data.
          if (dwCaptureFlags = AUDCLNT_BUFFERFLAGS_SILENT) then
            pData := nil;

          // Write the available capture data to the audio sink.
          mmRes := pvWavWriter.WriteData(pData,
                                         FramesAvailable,
                                         pvMixFormat.nBlockAlign,
                                         dwBytesWritten);

          // Note: The writer will automaticly closes the file when a HResult <> S_OK.
          if (mmRes <> MMSYSERR_NOERROR) then
            if (mmRes = MMIOERR_CANNOTEXPAND) then
              begin
                pvDeviceState := MaxFileSizeReached;
                hr := ERROR_FILE_TOO_LARGE;
                Break;
              end
            else
              begin
                pvDeviceState := Error;
                hr := E_FAIL;
                Break;
              end;
        end
      else
        begin
          StopCaptureAsync();
          ErrMsg(Format('pvAudioCaptureClient.GetBuffer failed. LastError = %d',[GetLastError()]), hr);
          pvDeviceState := Error;
          Break;
        end;

      // Release buffer.
      hr := pvAudioCaptureClient.ReleaseBuffer(FramesAvailable);
      if FAILED(hr) then
        begin
          StopCaptureAsync();
          pvDeviceState := Error;
          Break;
        end;

      // Keep score. Store to public.
      Inc(pvBytesWritten,
          dwBytesWritten);

      pData := nil;
    end;

finally
  pData := nil;
  Result := hr;
end;
end;

//
//  OnAudioSampleRequested()
//
//  Called when audio device fires pvSampleReadyEvent.
//
function TLoopbackCapture.OnAudioSampleRequested(): HResult;
begin

  // If this flag is set, we have already queued up the async call to finialize the WAV header.
  // So we don't want to grab or write any more data that would possibly give us an invalid size.
  if (pvDeviceState <> Capturing) or not Assigned(pvAudioCaptureClient) then
    Exit(E_POINTER);

  // Now the stream will be rendered in another thread.
  // So, we need to create another thread to keep control.
  //
  // Note that, when this audiostream is over,
  // the end of buffer will be called first, thus before signal endofstream (when all buffers have been processed).
  if not Assigned(pvRenderThread) then
    CreatedRenderThread()
  else
    pvRenderThread.Execute; // Continue when we are rendering a new buffer.

  Result := S_OK;
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
  //PropVariantClear(activateParams);
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

  if (pvRenderThreadClosedEvent <> 0) then
    begin
      SetEvent(pvShutdownEvent);
      WaitForSingleObject(pvRenderThreadClosedEvent,
                          INFINITE);
      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;

  pvactivateResult := E_UNEXPECTED;
  pvDeviceState := Uninitialized;
  pvcbHeaderSize := 0;

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


initialization
  //InitializeCriticalSection(oCriticalSection);

finalization
  //DeleteCriticalSection(oCriticalSection);

end.
