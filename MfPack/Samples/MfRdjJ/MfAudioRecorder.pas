// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LoopBackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.2.0
// Description: audio recorder based on WAS Loopback Capture Engine using TThread to render data.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
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
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: Microsoft.
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
unit MfAudioRecorder;

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
  System.Generics.Collections,
  {activeX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.ObjIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfReadWrite,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils,
  WinApi.CoreAudioApi.DeviceTopology,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioClientActivationParams,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  {WinMM}
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  RDJ_Common,
  MfAudioFileWriter;

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
  // BPM
  TDeckTickEvent = procedure(Sender: TObject;
                             const Position100ns: Int64;
                             const CurrentBpm: Double;
                             const BeatPhase: Double) of object;

  TDeckBeatEvent = procedure(Sender: TObject;
                             const Position100ns: Int64;
                             const BeatNumber: Int64;
                             const CurrentBpm: Double) of object;

  TDeckBpmAnalyzedEvent = procedure(Sender: TObject;
                                    const Bpm: Double) of object;

  TMfRecordedBlock = record
    Frames: Integer;
    ByteCount: Integer;
    Data: TBytes;
  end;

  TMfInternalMixerRecorder = class;

  TInternalMixerWriterThread = class(TThread)
  private
    FRecorder: TMfInternalMixerRecorder;
  protected
    procedure Execute; override;
  public
    constructor Create(ARecorder: TMfInternalMixerRecorder);
  end;

  TMfInternalMixerRecorder = class(TObject)
  private

    FWriter: IAudioWriter;
    FMixFormat: WAVEFORMATEX;
    FFileName: TFileName;
    FRecording: Boolean;
    FTime100ns: Int64;
    FBytesCaptured: Int64;
    FCritSec: TRTLCriticalSection;
    FQueueEvent: THandle;
    FShutdownEvent: THandle;
    FBlocks: TList<TMfRecordedBlock>;
    FWorkerThread: TInternalMixerWriterThread;

    function PopQueuedBlock(out ABlock: TMfRecordedBlock): Boolean;
    procedure ClearQueue();
    function WriteQueuedBlock(const ABlock: TMfRecordedBlock): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;

    function Start(const AFileName: TFileName;
                   const AWfx: WAVEFORMATEX): Boolean;

    function PushFloat32(const pData: PSingle;
                         const Frames: Integer): HRESULT;

    procedure Stop();

    property Recording: Boolean read FRecording;
    property BytesCaptured: Int64 read FBytesCaptured;
    property FileName: TFileName read FFileName;
  end;

  TRenderThread = class;

  //
  //  WASAPI Capture class.
  //
  TAudioCapture = class(TInterfacedPersistent,
                        IAudioSessionEvents,
                        IMMNotificationClient)
  protected

    pvRenderThread: TRenderThread;

  private

    pvRenderThreadClosedEvent: THandle;
    pvShutdownEvent: THandle;
    pvAudioSamplesReadyEvent: THandle;

    pvOnCapturingStart: TNotifyEvent;
    pvOnCapturingStopped: TNotifyEvent;

    //
    //  Core Audio Capture member variables.
    //
    pvEndpoint: IMMDevice;
    pvAudioClient: IAudioClient;
    pvCaptureClient: IAudioCaptureClient;

    // WAV-Filewriter.
    pvUseDefaultAudioFmt: Boolean;
    pvMixFormat: WAVEFORMATEXTENSIBLE;


    pvFrameSize: NativeUint;
    pvBufferSize: UINT32;

    pvFileName: TFileName;

    //
    //  Capture buffer management.
    //
    pvCaptureBuffer: PByte;
    pvCaptureBufferSize: NativeUint;
    pvBytesCaptured: Int64;
    pvDisableMMCSS: Boolean;

    //
    //  Stream switch related members and methods.
    //

    pvEndpointRole: ERole;
    pvStreamSwitchEvent: THandle;           // Set when the current session is disconnected or the default device changes.
    pvStreamSwitchCompleteEvent: THandle;   // Set when the default device changed.
    pvAudioSessionControl: IAudioSessionControl;
    pvDeviceEnumerator: IMMDeviceEnumerator;
    pvEngineLatency: REFERENCE_TIME;
    pvBufferDuration: REFERENCE_TIME;
    pvEnableStreamSwitch: Boolean;

    // BPM
    FTrackBpm: Double; // original analyzed BPM
    FBeatOffset100ns: Int64; // Where beat 1 starts
    FOnDeckTick: TDeckTickEvent;
    FOnBeat: TDeckBeatEvent;
    FOnBpmAnalyzed: TDeckBpmAnalyzedEvent;
    FLastTick100ns: Int64;
    FLastBeatIndex: Int64;
    FCurrentBpm: Double; // BPM after tempo change
    FTempoPercent: Double;
    FPosition100ns: Int64;
    //
    pvDeviceState: TDeviceState;

    function InitializeStreamSwitch(): Boolean;
    procedure TerminateStreamSwitch();
    function HandleStreamSwitchEvent(): Boolean;

    //
    // Thread functions.
    //
    function CaptureThreadFunc(): HRESULT;
    procedure CreateRenderThread();
    procedure TerminateRenderThread();

    //
    // Utility functions.
    //
    function InitializeAudioEngine(): Boolean;
    function GetMixFormat(out pMixFmt: PWAVEFORMATEX;
                          pGetDefault: Boolean = False): HResult;

    function GetCannelCount(): Word;
    function GetSamplesPerSecond(): DWord;
    function GetBytesPerSample(): Word;

    // BPM
    function GetBeatIndex(): Int64;
    procedure SetTrackBpm(const Value: Double);
    procedure UpdateCurrentBpm();
    function GetTempoFactor(): Double;
    function GetCurrentBpm(): Double;
    function GetBeatLength100ns(): Double;
    function GetBeatPhase(): Double;
    procedure DoBpmTracking(const APosition100ns: Int64);
    function GetPosition100ns(): Int64;
    procedure SetTempoPercent(const Value: Double);

{$Region 'Implementation of IAudioSessionEvents'}
    function OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                  const EventContext: TGUID): HResult; stdcall;
    function OnIconPathChanged(NewIconPath: LPCWSTR;
                               const EventContext: TGUID): HResult; stdcall;
    function OnSimpleVolumeChanged(NewSimpleVolume: Single;
                                   NewMute: BOOL;
                                   const EventContext: TGUID): HResult; stdcall;
    function OnChannelVolumeChanged(ChannelCount: UINT;
                                    NewChannelVolumeArray: PSINGLE;
                                    ChangedChannel: UINT;
                                    const EventContext: TGUID): HResult; stdcall;
    function OnGroupingParamChanged(const NewGroupingParam: TGUID;
                                    const EventContext: TGUID): HResult; stdcall;
    function OnStateChanged(NewState: AudioSessionState): HResult; stdcall;
    function OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult; stdcall;
{$EndRegion}

{$Region 'Implementation of IMMNotificationClient'}
    function OnDeviceStateChanged(DeviceId: LPCWSTR;
                                  NewState: DWord): HResult; stdcall;
    function OnDeviceAdded(DeviceId: LPCWSTR): HResult; stdcall;
    function OnDeviceRemoved(DeviceId: LPCWSTR): HResult; stdcall;
    function OnDefaultDeviceChanged(Flow: EDataFlow;
                                    Role: ERole;
                                    NewDefaultDeviceId: LPCWSTR): HResult; stdcall;
    function OnPropertyValueChanged(DeviceId: LPCWSTR;
                                    const Key: PROPERTYKEY): HResult; stdcall;
{$EndRegion}

  public

    // Public interface to TAudioCapture.
    constructor Create(pEndpoint: IMMDevice;
                       pEnableStreamSwitch: Boolean;
                       pDisableMmcss: Boolean;
                       pEndpointRole: ERole); reintroduce;

    destructor Destroy; override;

    //
    function Initialize(pBufferDuration: REFERENCE_TIME;
                        pEngineLatency: UINT32;
                        pUseDefaultAudioFmt: Boolean = True): Boolean;

    procedure Shutdown();

    function Start(const pFileName: TFileName): Boolean;
    procedure Stop();

    //
    // Properties.
    //
    property EndPoint: IMMDevice read pvEndpoint write pvEndpoint;
    //property MixFormat: PWAVEFORMATEX read pvMixFormat;
    property MixFormat: WAVEFORMATEXTENSIBLE read pvMixFormat;
    property ChannelCount: Word read GetCannelCount;
    property SamplesPerSecond: DWord read GetSamplesPerSecond;
    property BytesPerSample: Word read GetBytesPerSample;

    property FrameSize: SIZE_T read pvFrameSize;
    property BytesCaptured: Int64 read pvBytesCaptured;

    property DeviceState: TDeviceState read pvDeviceState;

    // BPM.
    property TrackBpm: Double read FTrackBpm write SetTrackBpm;
    property CurrentBpm: Double read GetCurrentBpm;
    property TempoPercent: Double read FTempoPercent write SetTempoPercent;
    property BeatOffset100ns: Int64 read FBeatOffset100ns write FBeatOffset100ns;
    property Position100ns: Int64 read GetPosition100ns;
    property OnDeckTick: TDeckTickEvent read FOnDeckTick write FOnDeckTick;
    property OnBeat: TDeckBeatEvent read FOnBeat write FOnBeat;
    property OnBpmAnalyzed: TDeckBpmAnalyzedEvent read FOnBpmAnalyzed write FOnBpmAnalyzed;

    // Notify events.
    property OnStartCapturing: TNotifyEvent read pvOnCapturingStart write pvOnCapturingStart;
    property OnStoppedCapturing: TNotifyEvent read pvOnCapturingStopped write pvOnCapturingStopped;
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

    FEngine: TAudioCapture;
    FSuccess: HResult; // Used internally when synchronizing the HRESULT for handling.
    FOnEvent: TCallbackEvent;

  public

    constructor Create(AEngine: TAudioCapture);
    property OnEvent: TCallbackEvent read FOnEvent write FOnEvent; // Triggered when a status changed.
  end;


implementation


uses
  System.Math,
  System.Services.Avrt;


{ TInternalMixerWriterThread }

constructor TInternalMixerWriterThread.Create(ARecorder: TMfInternalMixerRecorder);
begin

  inherited Create(False);

  FRecorder := ARecorder;
  FreeOnTerminate := False;
end;


procedure TInternalMixerWriterThread.Execute;
var
  WaitRes: DWORD;
  Block: TMfRecordedBlock;
  ShutdownRequested: Boolean;
  WaitHandles: array [0..1] of THandle;

begin

  ShutdownRequested := False;
  ZeroMemory(@Block,
             SizeOf(Block));
  WaitHandles[0] := FRecorder.FShutdownEvent;
  WaitHandles[1] := FRecorder.FQueueEvent;

  while (not Terminated) do
    begin

      WaitRes := WaitForMultipleObjects(2,
                                        @WaitHandles[0],
                                        False,
                                        INFINITE);

      case WaitRes of
        WAIT_OBJECT_0:
          ShutdownRequested := True;

        WAIT_OBJECT_0 + 1:
          ;
      else

        if ShutdownRequested then
          Break;

        Continue;
      end;

      while FRecorder.PopQueuedBlock(Block) do
        begin

          FRecorder.WriteQueuedBlock(Block);
          Block.Data := nil;
          Block.Frames := 0;
          Block.ByteCount := 0;
        end;

      if ShutdownRequested then
        Break;
    end;

  while FRecorder.PopQueuedBlock(Block) do
    begin

      FRecorder.WriteQueuedBlock(Block);
      Block.Data := nil;
      Block.Frames := 0;
      Block.ByteCount := 0;
    end;
end;


{ TMfInternalMixerRecorder }

constructor TMfInternalMixerRecorder.Create();
begin

  inherited Create();

  FWriter := nil;
  FRecording := False;
  FTime100ns := 0;
  FBytesCaptured := 0;
  FFileName := '';
  ZeroMemory(@FMixFormat,
             SizeOf(FMixFormat));

  InitializeCriticalSection(FCritSec);

  FBlocks := TList<TMfRecordedBlock>.Create();
  FQueueEvent := CreateEvent(nil,
                             False,
                             False,
                             nil);
  FShutdownEvent := CreateEvent(nil,
                                True,
                                False,
                                nil);
  FWorkerThread := nil;
end;


destructor TMfInternalMixerRecorder.Destroy();
begin

  Stop();

  if (FShutdownEvent <> 0) then
    CloseHandle(FShutdownEvent);

  if (FQueueEvent <> 0) then
    CloseHandle(FQueueEvent);

  FBlocks.Free();
  DeleteCriticalSection(FCritSec);

  inherited Destroy();
end;


procedure TMfInternalMixerRecorder.ClearQueue();
begin

  EnterCriticalSection(FCritSec);
  try

    FBlocks.Clear();
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TMfInternalMixerRecorder.PopQueuedBlock(out ABlock: TMfRecordedBlock): Boolean;
begin

  Result := False;
  ZeroMemory(@ABlock,
             SizeOf(ABlock));

  EnterCriticalSection(FCritSec);
  try

    if (FBlocks.Count <= 0) then
      Exit;

    ABlock := FBlocks[0];
    FBlocks.Delete(0);
    Result := True;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TMfInternalMixerRecorder.WriteQueuedBlock(const ABlock: TMfRecordedBlock): HRESULT;
var
  BytesWritten: UINT32;

begin

  if (ABlock.Frames <= 0) or (ABlock.ByteCount <= 0) then
    Exit(E_INVALIDARG);

  EnterCriticalSection(FCritSec);

  try

    if (not Assigned(FWriter)) then
      Exit(S_FALSE);

    BytesWritten := 0;

    Result := FWriter.WriteFrames(@ABlock.Data[0],
                                  UINT32(ABlock.Frames),
                                  FMixFormat,
                                  FTime100ns,
                                  BytesWritten);
    if SUCCEEDED(Result) then
      Inc(FBytesCaptured,
          BytesWritten);
  finally

    LeaveCriticalSection(FCritSec);
  end;
end;


function TMfInternalMixerRecorder.Start(const AFileName: TFileName;
                                        const AWfx: WAVEFORMATEX): Boolean;
var
  hr: HRESULT;

begin

  Result := False;

  Stop();

  if (Trim(AFileName) = '') then
    Exit;

  EnterCriticalSection(FCritSec);

  try

    FWriter := CreateAudioWriterFromFileName(AFileName);
    if not Assigned(FWriter) then
      Exit;

    FMixFormat := AWfx;
    FFileName := AFileName;
    FTime100ns := 0;
    FBytesCaptured := 0;

    hr := FWriter.Open(AFileName,
                       FMixFormat);
    if FAILED(hr) then
      begin

        FWriter := nil;
        Exit;
      end;

    FRecording := True;
  finally

    LeaveCriticalSection(FCritSec);
  end;

  ClearQueue();
  ResetEvent(FShutdownEvent);

  FWorkerThread := TInternalMixerWriterThread.Create(Self);
  Result := Assigned(FWorkerThread);

  if not Result then
    Stop();
end;


function TMfInternalMixerRecorder.PushFloat32(const pData: PSingle;
                                              const Frames: Integer): HRESULT;
var
  Block: TMfRecordedBlock;
  ByteCount: Integer;

begin

  if (pData = nil) or (Frames <= 0) then
    Exit(E_INVALIDARG);

  ByteCount := Frames * Integer(FMixFormat.nBlockAlign);
  if (ByteCount <= 0) then
    Exit(E_UNEXPECTED);

  EnterCriticalSection(FCritSec);

  try

    if (not FRecording) or (not Assigned(FWriter)) or (FWorkerThread = nil) then
      Exit(S_FALSE);

    Block.Frames := Frames;
    Block.ByteCount := ByteCount;
    SetLength(Block.Data,
              ByteCount);
    Move(pData^,
         Block.Data[0],
         ByteCount);

    FBlocks.Add(Block);
  finally

    LeaveCriticalSection(FCritSec);
  end;

  SetEvent(FQueueEvent);
  Result := S_OK;
end;


procedure TMfInternalMixerRecorder.Stop();
begin

  EnterCriticalSection(FCritSec);

  try

    FRecording := False;
  finally

    LeaveCriticalSection(FCritSec);
  end;

  if Assigned(FWorkerThread) then
    begin

      SetEvent(FShutdownEvent);
      FWorkerThread.WaitFor();
      FreeAndNil(FWorkerThread);
    end;

  EnterCriticalSection(FCritSec);

  try

    if Assigned(FWriter) then
      begin
        FWriter.Close();
        FWriter := nil;
      end;
  finally

    LeaveCriticalSection(FCritSec);
  end;

  ClearQueue();
end;


constructor TRenderThread.Create(AEngine: TAudioCapture);
begin

  inherited Create(True);

  FEngine := AEngine;
  FreeOnTerminate := False;
end;


procedure TRenderThread.Execute();
begin

  // The function where we render the audio data and
  // (de)activate the MMCSS feature.
  // See: https://learn.microsoft.com/en-us/windows/win32/procthread/multimedia-class-scheduler-service
  // To get the best performance, it's recomended to set "Best Performance" in Windows energy settings.
  FSuccess := FEngine.CaptureThreadFunc();
  Synchronize(SetEvent);
end;


procedure TRenderThread.SetEvent();
begin

  // Called from 'Synchronize'.
  // All code run from "Synchronize()" runs in the context of the
  // Main VCL UI Thread, NOT from this thread.
  // This triggers the event which was assigned by the calling thread
  // to inform it that rendering has been completed.

  if Assigned(FOnEvent) then
    FOnEvent(Self,
             FSuccess);
end;

// =============================================================================

procedure TAudioCapture.CreateRenderThread;
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


procedure TAudioCapture.TerminateRenderThread();
begin

  if Assigned(pvRenderThread) then
    begin

      pvRenderThread.SetFreeOnTerminate(True);
      pvRenderThread.Terminate;
      // Give thread time to terminate itself.
      Sleep(200);
      pvRenderThread := nil;
      // Signal the thread is closed.
      if (pvRenderThreadClosedEvent <> 0) then
        SetEvent(pvRenderThreadClosedEvent);
    end;
end;

// =============================================================================

constructor TAudioCapture.Create(pEndpoint: IMMDevice;
                                 pEnableStreamSwitch: Boolean;
                                 pDisableMmcss: Boolean;
                                 pEndpointRole: ERole);
var
  hr: HResult;
  pEnumerator: IMMDeviceEnumerator;

begin

  inherited Create;

  if (pEndpoint <> nil) then
    pvEndpoint := pEndpoint
  else
    begin

      // Enumerate on capture and render devices
      hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                             nil,
                             CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,
                             pEnumerator);

      if SUCCEEDED(hr) then
        // Get the default endpoint. See MMDeviceApi line 278 for explanation.
        hr := pEnumerator.GetDefaultAudioEndpoint(ERender, // eRender or eCapture,
                                                  eMultimedia,     // eMultimedia, eConsole or eCommunications
                                                  pvEndpoint);
      if FAILED(hr) then
        Abort;
    end;

  pvEnableStreamSwitch := pEnableStreamSwitch;
  pvDisableMMCSS := pDisableMmcss;
  pvEndpointRole := pEndpointRole;

  pvShutdownEvent := 0;
  pvRenderThreadClosedEvent := 0;
  pvAudioSamplesReadyEvent := 0;
  pvStreamSwitchEvent := 0;
  pvStreamSwitchCompleteEvent := 0;

  pvFrameSize := 0;
  pvBufferSize := 0;
  pvCaptureBuffer := nil;
  pvCaptureBufferSize := 0;
  pvEnableStreamSwitch := False;

  FTrackBpm := 0.0;
  FBeatOffset100ns := 0;
  FLastTick100ns := 0;
  FLastBeatIndex := -1;
  FCurrentBpm := 0.0;
  FTempoPercent := 0.0;
  FPosition100ns := 0;

end;


destructor TAudioCapture.Destroy();
begin

  Shutdown();
  inherited Destroy;
end;


//
//  Initialize the stream switch logic.
//
function TAudioCapture.InitializeStreamSwitch(): Boolean;
var
  hr: HResult;

begin

  hr := pvAudioClient.GetService(IID_IAudioSessionControl,
                                 pvAudioSessionControl);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to retrieve session control: %d.',
               [GetLastError()]), hr);
      Exit(False);
    end;

  //  Create the stream switch complete event- we want a manual reset event that starts in the not-signaled state.
  pvStreamSwitchCompleteEvent := CreateEventEx(nil,
                                               nil,
                                               CREATE_EVENT_INITIAL_SET or CREATE_EVENT_MANUAL_RESET,
                                               EVENT_MODIFY_STATE or SYNCHRONIZE);
  if (pvStreamSwitchCompleteEvent = 0) then
    begin

      InfoMsg(optIDE, Format('Unable to create stream switch event: %d.',[GetLastError()]), hr);
      Exit(False);
    end;

  // Register for session and endpoint change notifications.

  // A stream switch is initiated when we receive a session disconnect notification or
  // we receive a default device changed notification.

  hr := pvAudioSessionControl.RegisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to register for stream switch notifications: %d.',[GetLastError()]), hr);
      Exit(False);
    end;

  hr := pvDeviceEnumerator.RegisterEndpointNotificationCallback(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to register for stream switch notifications: %d.',[GetLastError()]), hr);
      Exit(False);
    end;

  Result := True;
end;


procedure TAudioCapture.TerminateStreamSwitch();
var
  hr: HResult;

begin

  hr := pvAudioSessionControl.UnregisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to unregister for session notifications: %d.',[GetLastError()]), hr);
    end;

  pvDeviceEnumerator.UnregisterEndpointNotificationCallback(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to unregister for endpoint notifications: %d.',[GetLastError()]), hr);
    end;

  if (pvStreamSwitchCompleteEvent <> 0) then
    begin

      CloseHandle(pvStreamSwitchCompleteEvent);
      pvStreamSwitchCompleteEvent := 0;
    end;

  SafeRelease(pvAudioSessionControl);
  SafeRelease(pvDeviceEnumerator);
end;


//
//  Handle the stream switch.
//
//  When a stream switch happens, we want to do several things in turn:
//
//  1) Stop the current capturer.
//  2) Release any resources we have allocated (the pvAudioClient,
//     pvAudioSessionControl (after unregistering for notifications) and  pvCaptureClient).
//  3) Wait until the default device has changed (or 500ms has elapsed).
//     If we time out, we need to abort because the stream switch can't happen.
//  4) Retrieve the new default endpoint for our role.
//  5) Re-instantiate the audio client on that new endpoint.
//  6) Retrieve the mix format for the new endpoint.
//     If the mix format doesn't match the old endpoint's mix format,
//     we need to abort because the stream switch can't happen.
//  7) Re-initialize the pvAudioClient.
//  8) Re-register for session disconnect notifications and reset the stream switch complete event.
//
function TAudioCapture.HandleStreamSwitchEvent(): Boolean;
var
  hr: HResult;
  bRes: Boolean;
  waitResult: DWord;
  wfxNew: PWAVEFORMATEX;

label
  ErrorExit;

begin

  {$IFDEF DEBUG}
    Assert(pvEnableStreamSwitch);
  {$ENDIF}

  bRes := False;

  // Step 1.  Stop capturing.

  hr := pvAudioClient.Stop();
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to stop audio client during stream switch: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  // Step 2.  Release our resources.  Note that we don't release the mix format, we need it for step 6.

  hr := pvAudioSessionControl.UnregisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to stop audio client during stream switch: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  SafeRelease(pvAudioSessionControl);
  SafeRelease(pvCaptureClient);
  SafeRelease(pvAudioClient);
  SafeRelease(pvEndpoint);

  // Step 3.  Wait for the default device to change.

  // There is a race between the session disconnect arriving and the new default device
  // arriving (if applicable).
  // Wait the shorter of 500 milliseconds or the arrival of the
  // new default device, then attempt to switch to the default device.
  // In the case of a format change (i.e. the default device does not change),
  // we artificially generate a new default device notification so the code will
  // not needlessly wait 500ms before re-opening on the new format.
  // (However, note below in step 6 that in this SDK sample,
  // we are unlikely to actually successfully absorb a format change,
  // but a real audio application implementing stream switching would re-format their
  // pipeline to deliver the new format).

  waitResult := WaitForSingleObject(pvStreamSwitchCompleteEvent,
                                    500);
  if (waitResult = WAIT_TIMEOUT) then
    begin

      InfoMsg(optIDE,
              Format('Stream switch timeout - aborting...: %d.',
                     [waitResult]),
                     E_FAIL);
      goto ErrorExit;
    end;


  // Step 4.  If we can't get the new endpoint, we need to abort the stream switch.
  // If there is a new device, we should be able to retrieve it.
  if not Assigned(pvEndpoint) then
    hr := pvDeviceEnumerator.GetDefaultAudioEndpoint(EDataFlow(eRender),
                                                     pvEndpointRole,
                                                     pvEndpoint);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to retrieve new default device during stream switch: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  // Step 5 - Re-instantiate the audio client on the new endpoint.

  hr := pvEndpoint.Activate(IID_IAudioClient,
                            CLSCTX_INPROC_SERVER,
                            nil,
                            Pointer(pvAudioClient));
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to activate audio client on the new endpoint: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  // Step 6 - Retrieve the new mix format.

  // pvUseDeviceAudioFmt = True  => use device mix format (GetMixFormat)
  // pvUseDeviceAudioFmt = False => use plain default PCM 44.1/16 (GetDefaultWaveFmtEx)
  hr := GetMixFormat(wfxNew,
                     pvUseDefaultAudioFmt);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to retrieve mix format for new audio client: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  try

    // Copy only the base WAVEFORMATEX header into our stored record.
    // (Most devices return WAVEFORMATEXTENSIBLE; we ignore extra fields here.)
    Move(wfxNew^,
         pvMixFormat,
         SizeOf(WAVEFORMATEX));

    // Prefer correct frame size based on nBlockAlign
    pvFrameSize := pvMixFormat.Format.nBlockAlign;
  finally

    CoTaskMemFree(wfxNew);
  end;

  InfoMsg(optIDE,
          Format('MixFmt: tag=%d ch=%d sr=%d bps=%d align=%d avg=%d cb=%d',
                 [pvMixFormat.Format.wFormatTag, pvMixFormat.Format.nChannels, pvMixFormat.Format.nSamplesPerSec,
                  pvMixFormat.Format.wBitsPerSample, pvMixFormat.Format.nBlockAlign, pvMixFormat.Format.nAvgBytesPerSec, pvMixFormat.Format.cbSize]),
          S_OK);

  // Note that this is an intentionally naive comparison.
  // A more sophisticated comparison would compare the sample rate,
  // channel count and format and apply the appropriate conversions into the capture pipeline.

  if not CompareMem(@pvMixFormat,
                    @wfxNew,
                    SizeOf(WAVEFORMATEX) + wfxNew.cbSize) then
    begin

      InfoMsg(optIDE,
              Format('New mix format doesn''t match old mix format.  Aborting... : %d.',
                     [E_FAIL]),
              E_FAIL);
      // CoTaskMemFree(wfxNew);
      goto ErrorExit;
    end;

  // CoTaskMemFree(wfxNew);

  // Step 7:  Re-initialize the audio client.

  if not InitializeAudioEngine() then
   begin
      goto ErrorExit;
   end;

  // Step 8: Re-register for session disconnect notifications.

  hr := pvAudioClient.GetService(IID_IAudioSessionControl,
                                 pvAudioSessionControl);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to retrieve session control on new audio client: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  hr := pvAudioSessionControl.RegisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to retrieve session control on new audio client: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  // Reset the stream switch complete event because it's a manual reset event.
  ResetEvent(pvStreamSwitchCompleteEvent);

  // And we're done.  Start capturing again.

  hr := pvAudioClient.Start();
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to start the new audio client: %d.',
                     [GetLastError()]),
              hr);
      goto ErrorExit;
    end;

  pvEnableStreamSwitch := True;
  bRes := True;

ErrorExit:
  pvEnableStreamSwitch := False;
  Result := bRes;
end;

//
// Utility functions.
//

//
// Initialize WASAPI in event driven mode, associate the audio client with our samples ready event handle,
// retrieve a capture client for the transport, create the capture thread and start the audio engine.
//
function TAudioCapture.InitializeAudioEngine(): Boolean;
var
  hr: HResult;
  hnsDefaultDevicePeriod: REFERENCE_TIME;
  hnsMinimumDevicePeriod: REFERENCE_TIME;
  hnsLatency: REFERENCE_TIME;
  pWfx: PWAVEFORMATEX;

begin

  pWfx := nil;

  // Let the endpoint we selected - when not the default - creates the audioclient.
  hr := pvEndpoint.Activate(IID_IAudioClient,
                            CLSCTX_ALL,
                            nil,
                            Pointer(pvAudioClient));

  if FAILED(hr) then
    Exit(SUCCEEDED(hr));

  hr := GetMixFormat(pWfx,
                     pvUseDefaultAudioFmt);
  if FAILED(hr) then
    Exit(False);

  try

    ZeroMemory(@pvMixFormat,
               SizeOf(pvMixFormat));

        // Always copy the WAVEFORMATEX header first
    Move(pWfx^,
         pvMixFormat.Format,
         SizeOf(WAVEFORMATEX));

    // If the returned format is extensible, copy only the extension fields safely
    if (pWfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and
       (pWfx.cbSize >= (SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX))) then
      begin

        pvMixFormat.Samples.wValidBitsPerSample := PWAVEFORMATEXTENSIBLE(pWfx).Samples.wValidBitsPerSample;
        pvMixFormat.dwChannelMask := PWAVEFORMATEXTENSIBLE(pWfx).dwChannelMask;
        pvMixFormat.SubFormat := PWAVEFORMATEXTENSIBLE(pWfx).SubFormat;

        pvMixFormat.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
        pvMixFormat.Format.cbSize := SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX);
      end
    else
      begin
        // Not extensible -> synthesize extensible wrapper fields
        pvMixFormat.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
        pvMixFormat.Format.cbSize := SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX);

        pvMixFormat.Samples.wValidBitsPerSample := pvMixFormat.Format.wBitsPerSample;

        case pvMixFormat.Format.nChannels of
          1: pvMixFormat.dwChannelMask := $00000004; // FRONT_CENTER
          2: pvMixFormat.dwChannelMask := $00000003; // FRONT_LEFT|FRONT_RIGHT
        else
          pvMixFormat.dwChannelMask := 0;
        end;

        if pWfx.wFormatTag = WAVE_FORMAT_IEEE_FLOAT then
          pvMixFormat.SubFormat := KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
        else
          pvMixFormat.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
      end;


    // Validate before using
    if (pvMixFormat.Format.nChannels = 0) or
       (pvMixFormat.Format.nSamplesPerSec = 0) or
       (pvMixFormat.Format.nBlockAlign = 0) then
      Exit(False);

    // Correct frame size (safe now)
    pvFrameSize := pvMixFormat.Format.nBlockAlign;

  finally

    CoTaskMemFree(pWfx);
  end;


  // The original sample creates a bufferDuration of 2 seconds,
  // that will cause sound disturbtion when capture sound from a streameservice like
  // YouTube or other high latency services.
  // To prevent this, we use as a minimum the value of hnsDefaultDevicePeriod.
  hr := pvAudioClient.GetDevicePeriod(hnsDefaultDevicePeriod,
                                      hnsMinimumDevicePeriod);
  if FAILED(hr) then
    Exit(False);

  // Pick the correct device period for the bufferduration.
  if (pvBufferDuration > 0) then
    begin

      // Set bufferduration including latency.
      Inc(pvBufferDuration,
          pvEngineLatency);
      if (pvBufferDuration < hnsMinimumDevicePeriod) then
        pvBufferDuration :=  hnsMinimumDevicePeriod;
    end;

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_LOOPBACK or
                                 AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                                 AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
                                 pvBufferDuration, // Note: When bufferDuration = 0, the audioclient will automaticly decise the bufferduration.
                                 0, // Must be zero in shared mode!
                                 @pvMixFormat.Format,
                                 @GUID_NULL);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE, Format('Unable to initialize the audio client: %d.',[GetLastError()]), hr);
      Exit(SUCCEEDED(hr));
    end;

  // Retrieve the maximum size of the shared buffer
  hr := pvAudioClient.GetBufferSize(pvBufferSize);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to get audio client buffer: %d.',
                     [GetLastError()]),
              hr);
      Exit(SUCCEEDED(hr));
    end;

 // Get the stream latency (normally this should be inbetween 0 and 15 ms on Windows 11)
 // See: https://learn.microsoft.com/en-us/windows-hardware/drivers/audio/low-latency-audio
  hr := pvAudioClient.GetStreamLatency(hnsLatency);
  if FAILED(hr) then
    Exit(SUCCEEDED(hr));

  // We don't want to get below the stream latency.
  if (pvEngineLatency < hnsLatency) then
    pvEngineLatency := hnsLatency;

  // Set the eventhandle for AudioSamplesReadyEvent.
  hr := pvAudioClient.SetEventHandle(pvAudioSamplesReadyEvent);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to set ready event: %d.',
                     [GetLastError()]),
              hr);
      Exit(SUCCEEDED(hr));
    end;

  // Get the capture client.
  hr := pvAudioClient.GetService(IID_IAudioCaptureClient,
                                 IUnknown(pvCaptureClient));
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to get new capture client: %d.',
                     [GetLastError()]),
              hr);
      Exit(SUCCEEDED(hr));
    end;

  Result := SUCCEEDED(hr);
end;

//
//  Retrieve the format we'll use to capture samples.
//
//  We use the Mix format since we're capturing in shared mode.
//
function TAudioCapture.GetMixFormat(out pMixFmt: PWAVEFORMATEX;
                                  pGetDefault: Boolean = False): HResult;
begin

  pMixFmt := nil;
  pvFrameSize := 0;

  if pGetDefault then
    begin

      pMixFmt := GetDefaultWaveFmtEx();
      if (pMixFmt = nil) then
        Exit(E_OUTOFMEMORY);

      pvFrameSize := pMixFmt.nBlockAlign;
      Exit(S_OK);
    end;

  Result := pvAudioClient.GetMixFormat(pMixFmt);

  if FAILED(Result) then
    begin

      InfoMsg(optIDE,
              Format('Unable to get mix format on audio client: %d.',
                     [GetLastError()]),
              Result);
      pvFrameSize := 0;
      Exit(Result);
    end;

  pvFrameSize := pMixFmt.nBlockAlign;
end;



// Property read function.

function TAudioCapture.GetCannelCount(): Word;
begin

  Result := pvMixFormat.Format.nChannels;
end;


function TAudioCapture.GetSamplesPerSecond(): DWord;
begin

  Result := pvMixFormat.Format.nSamplesPerSec;
end;


function TAudioCapture.GetBytesPerSample(): Word;
begin

  Result := pvMixFormat.Format.wBitsPerSample div 8;
end;


/// IAudioSessionEvents methods ////////////////////////////////////////////////

function TAudioCapture.OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                          const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnIconPathChanged(NewIconPath: LPCWSTR;
                                       const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnSimpleVolumeChanged(NewSimpleVolume: Single;
                                                   NewMute: BOOL;
                                                   const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnChannelVolumeChanged(ChannelCount: UINT;
                                                    NewChannelVolumeArray: PSINGLE;
                                                    ChangedChannel: UINT;
                                                    const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnGroupingParamChanged(const NewGroupingParam: TGUID;
                                            const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnStateChanged(NewState: AudioSessionState): HResult;
begin

  Result := S_OK;
end;

//
// Called when an audio session is disconnected.
//
// When a session is disconnected because of a device removal or format change event,
// we just want to let the capture thread know that the session's gone away.
//
function TAudioCapture.OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult;
begin

  if (DisconnectReason = DisconnectReasonDeviceRemoval) then
    begin

      // The stream was disconnected because the device we're capturing to was removed.
      //
      // We want to reset the stream switch complete event (so we'll block when the HandleStreamSwitchEvent function
      // waits until the default device changed event occurs).
      //
      // Note that we don't set the pvStreamSwitchCompleteEvent - that will be set when the OnDefaultDeviceChanged event occurs.

      pvEnableStreamSwitch := True;
      SetEvent(pvStreamSwitchEvent);
    end;

  if (DisconnectReason = DisconnectReasonFormatChanged) then
    begin

      // The stream was disconnected because the format changed on our capture device.
      //
      // We want to flag that we're in a stream switch and then set the stream switch event (which breaks out of the capturer).
      // We also want to set the pvStreamSwitchCompleteEvent because we're not going to see a default device changed event after this.

      pvEnableStreamSwitch := True;
      SetEvent(pvStreamSwitchEvent);
      SetEvent(pvStreamSwitchCompleteEvent);
    end;

  Result := S_OK;
end;


/// IMMNotificationClient methods //////////////////////////////////////////////

function TAudioCapture.OnDeviceStateChanged(DeviceId: LPCWSTR;
                                          NewState: DWord): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnDeviceAdded(DeviceId: LPCWSTR): HResult;
begin

  Result := S_OK;
end;


function TAudioCapture.OnDeviceRemoved(DeviceId: LPCWSTR): HResult;
begin

  Result := S_OK;
end;

//
// Called when the default capture device changed.
// We just want to set an event which lets the stream switch logic know that it's Ok to
// continue with the stream switch.
//
function TAudioCapture.OnDefaultDeviceChanged(Flow: EDataFlow;
                                            Role: ERole;
                                            NewDefaultDeviceId: LPCWSTR): HResult;
begin

  if (Flow = EDataFlow(eRender)) and (Role = pvEndpointRole) then
    begin

      //  The default capture device for the configured role was changed.
      //
      //  If we're not in a stream switch already, we want to initiate a stream switch event.
      //  We also we want to set the stream switch complete event.  That will signal the capture thread that it's ok to re-initialize the
      //  audio capturer.
      //
      if not pvEnableStreamSwitch then
        begin
            pvEnableStreamSwitch := True;
            SetEvent(pvStreamSwitchEvent);
        end;
      SetEvent(pvStreamSwitchCompleteEvent);
    end;
  Result := S_OK;
end;


function TAudioCapture.OnPropertyValueChanged(DeviceId: LPCWSTR;
                                            const Key: PROPERTYKEY): HResult;
begin

  Result := S_OK;
end;


/// Public methods /////////////////////////////////////////////////////////////

//
//  Initialize the capturer.
//
function TAudioCapture.Initialize(pBufferDuration: REFERENCE_TIME;
                                  pEngineLatency: UINT32;
                                  pUseDefaultAudioFmt: Boolean = True): Boolean;
begin

  //  Create our shutdown and samples ready events- we want auto reset events that
  //  start in the not-signaled state.

  pvShutdownEvent := CreateEventEx(nil,
                                   nil,
                                   0,
                                   EVENT_MODIFY_STATE or SYNCHRONIZE);

  if (pvShutdownEvent = 0) then
    begin

      InfoMsg(optIDE,
              Format('Unable to create shutdown event: %d.',
                     [E_FAIL]),
              E_FAIL);
      Exit(False);
    end;

  pvAudioSamplesReadyEvent := CreateEventEx(nil,
                                            nil,
                                            0,
                                            EVENT_MODIFY_STATE or SYNCHRONIZE);

  if (pvAudioSamplesReadyEvent = 0) then
    begin

      InfoMsg(optIDE,
              Format('Unable to create samples ready event: %d.',
                     [E_FAIL]),
                     E_FAIL);
      Exit(False);
    end;

  pvStreamSwitchEvent := CreateEventEx(nil,
                                       nil,
                                       0,
                                       EVENT_MODIFY_STATE or SYNCHRONIZE);

  // Create our stream switch event- we want auto reset events that start in the not-signaled state.
  // Note that we create this event even if we're not going to stream switch - that's because the event is used
  // in the main loop of the capturer and thus it has to be set.
  if (pvStreamSwitchEvent = 0) then
    begin

      InfoMsg(optIDE,
              Format('Unable to create stream switch event: %d.',[E_FAIL]),
              E_FAIL);
      Exit(False);
    end;

  // Remember our configured latency in case we'll need it for a stream switch later.
  pvEngineLatency := pEngineLatency;
  pvBufferDuration := pBufferDuration;
  pvUseDefaultAudioFmt := pUseDefaultAudioFmt;

  // After setting the events, we initialize the audioclient aand captureclient.
  if not InitializeAudioEngine() then
    Exit(False);

  if pvEnableStreamSwitch then
    begin
      if not InitializeStreamSwitch() then
        Exit(False);
    end;

  Result := True;
end;

//
// Shut down the capture code and free all the resources.
//
procedure TAudioCapture.Shutdown();
begin

  if (pvRenderThreadClosedEvent <> 0) then
    begin

      SetEvent(pvShutdownEvent);
      WaitForSingleObject(pvRenderThreadClosedEvent,
                          INFINITE);
      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;

    if (pvShutdownEvent <> 0) then
      begin

        CloseHandle(pvShutdownEvent);
        pvShutdownEvent := 0;
      end;

    if (pvAudioSamplesReadyEvent <> 0) then
      begin

        CloseHandle(pvAudioSamplesReadyEvent);
        pvAudioSamplesReadyEvent := 0;
      end;

    if (pvStreamSwitchEvent <> 0) then
      begin

        CloseHandle(pvStreamSwitchEvent);
        pvStreamSwitchEvent := 0;
      end;

    SafeRelease(pvEndpoint);
    SafeRelease(pvAudioClient);
    SafeRelease(pvCaptureClient);

    //pvMixFormat := nil;

    if (pvEnableStreamSwitch = True) then
      TerminateStreamSwitch();
end;


//
//  Start capturing...
//
function TAudioCapture.Start(const pFileName: TFileName): Boolean;
var
  hr: HResult;

begin

  pvFileName := pFileName;

  // Now the stream will be rendered in another thread.
  // So, we need to create another thread to keep control.
  //
  // Note that, when this audiostream is over,
  // the end of buffer will be signaled first, before signal endofstream.
  if Assigned(pvRenderThread) then
    begin

      TerminateRenderThread();

      if (pvRenderThreadClosedEvent <> 0) then
        begin

          WaitForSingleObject(pvRenderThreadClosedEvent,
                              INFINITE);

          CloseHandle(pvRenderThreadClosedEvent);
          pvRenderThreadClosedEvent := 0;
        end;
    end
  else
    begin
      // Start the rendering loop in the separate thread.
      CreateRenderThread();
    end;

  // We're ready to go, start capturing!
  hr := pvAudioClient.Start();
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to start capture client: %d.',
                     [GetLastError()]),
              hr);
      Exit(False);
    end;

  // Notify the mainform to start it's timer.
  pvOnCapturingStart(Self);
  Result := True;
end;

//
//  Stop the capturer.
//
procedure TAudioCapture.Stop();
var
  hr: HResult;

begin
  hr := S_OK;

  pvDeviceState := Stopping;
  // Tell the capture thread to shut down, wait for the thread to complete then clean up all the stuff we
  // allocated in Start().

  TerminateRenderThread();

  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  if Assigned(pvAudioClient) then
    begin

      hr := pvAudioClient.Stop();
      if FAILED(hr) then
        InfoMsg(optIDE,
                Format('Unable to stop audio client: %d',
                       [GetLastError()]),
                hr);
    end;

  if (pvRenderThreadClosedEvent <> 0) then
    begin

      WaitForSingleObject(pvRenderThreadClosedEvent,
                          INFINITE);

      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;

  if SUCCEEDED(hr) and (pvRenderThreadClosedEvent = 0) then
    begin

      // Signal the mainform capturing has been stopped.
      pvOnCapturingStopped(Self);
      Sleep(1); // Sleep to prevent a mainform timer would stop before the notify event has been processed.
      pvDeviceState := Stopped;
    end;
end;


function TAudioCapture.CaptureThreadFunc(): HRESULT;
var
  hr: HResult;
  waitArray: array[0..2] of THandle;
  mmcssHandle: THandle;
  mmcssTaskIndex: DWord;
  waitResult: DWord;

  packetSize: UINT32;
  pData: PByte;
  NumFramesToRead: UINT32;
  flags: DWord;
  bytesToWrite: Integer;

  // Writer stuff.
  audioWriter: IAudioWriter;
  time100ns: Int64;
  bytesWrittenMF: UINT32;

  // For SILENT packets (never pass nil to mmioWrite)
  silenceBuf: TBytes;

begin

  mmcssHandle := 0;
  mmcssTaskIndex := 0;

  SetLength(silenceBuf, 0);

  // Create and open file.
  audioWriter := CreateAudioWriterFromFileName(pvFileName);
  time100ns := 0;

  hr := audioWriter.Open(pvFileName,
                         pvMixFormat.Format);
  if FAILED(hr) then
    begin

      InfoMsg(optIDE,
              Format('Unable to open MF sink writer ''%s'': %d.',
                     [pvFileName, GetLastError()]),
              hr);
      Exit(hr);
    end;


  // Enable MMCSS.
  if not pvDisableMMCSS then
    begin

      mmcssHandle := AvSetMmThreadCharacteristics('Audio',
                                                  @mmcssTaskIndex);
      // If it fails, we continue anyway.
    end;

  waitArray[0] := pvShutdownEvent;
  waitArray[1] := pvStreamSwitchEvent;
  waitArray[2] := pvAudioSamplesReadyEvent;

  pvDeviceState := Capturing;

  try
    while (pvDeviceState = Capturing) do
      begin

        waitResult := WaitForMultipleObjects(3,
                                             @waitArray,
                                             False,
                                             INFINITE);

        case waitResult of
          // pvShutdownEvent
          WAIT_OBJECT_0 + 0: pvDeviceState := Stopping;

        // pvStreamSwitchEvent
        WAIT_OBJECT_0 + 1: begin

                             if not HandleStreamSwitchEvent() then
                             pvDeviceState := Error;
                           end;

        // pvAudioSamplesReadyEvent
        WAIT_OBJECT_0 + 2: begin

                             hr := pvCaptureClient.GetNextPacketSize(packetSize);
                             if FAILED(hr) then
                               begin

                                 pvDeviceState := Error;
                                 Continue;
                                end;

                             // Drain all packets for this event.
                             while (packetSize > 0) and (pvDeviceState = Capturing) do
                               begin

                                 hr := pvCaptureClient.GetBuffer(pData,
                                                                 NumFramesToRead,
                                                                 flags,
                                                                 nil,
                                                                 nil);
                                 if FAILED(hr) then
                                   begin

                                     pvDeviceState := Error;
                                     Break;
                                    end;

                                 try

                                   // It is valid (and happens) that NumFramesToRead can be 0.
                                   // If so: just release and continue.
                                   if (NumFramesToRead = 0) then
                                     begin

                                       // no write, dwBytesWritten stays 0
                                     end
                                   else
                                     begin

                                       bytesToWrite := Integer(NumFramesToRead) * Integer(pvMixFormat.Format.nBlockAlign);

                                       // SILENT is a bitmask, the writer cannot accept nil pointers.
                                       if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                                         begin

                                           if (Length(silenceBuf) < bytesToWrite) then
                                             SetLength(silenceBuf,
                                                       bytesToWrite);

                                           FillChar(silenceBuf[0],
                                                    bytesToWrite,
                                                    0);
                                           pData := @silenceBuf[0];
                                         end;

                                       // DO NOT "continue" on DATA_DISCONTINUITY; just optionally log it.
                                       // if ((flags and AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) <>) 0 then
                                       //   InfoMsg(optIDE, 'DATA_DISCONTINUITY', S_OK);

                                       hr := audioWriter.WriteFrames(pData,
                                                                     NumFramesToRead,
                                                                     pvMixFormat.Format,
                                                                     time100ns,
                                                                     bytesWrittenMF);
                                       if FAILED(hr) then
                                         begin

                                           pvDeviceState := Error;
                                           Break;
                                         end;

                                       Inc(pvBytesCaptured,
                                           bytesWrittenMF);

                                       Inc(time100ns,
                                           Round((Double(NumFramesToRead) * REFTIMES_PER_SEC) /
                                                 pvMixFormat.Format.nSamplesPerSec));
                                       DoBpmTracking(time100ns);
                                     end;

                                 finally

                                   hr := pvCaptureClient.ReleaseBuffer(NumFramesToRead);
                                   if FAILED(hr) then
                                     pvDeviceState := Error;
                                 end;

                                 hr := pvCaptureClient.GetNextPacketSize(packetSize);
                                 if FAILED(hr) then
                                   begin

                                     pvDeviceState := Error;
                                     Break;
                                   end;
                               end;
                           end;
        end; // case
    end; // while

    if (pvDeviceState = Error) then
      Result := hr
    else
      Result := S_OK;

  finally

    if not pvDisableMMCSS then
      AvRevertMmThreadCharacteristics(mmcssHandle);

    pvDeviceState := Stopped;
    if (audioWriter <> nil) then
      audioWriter.Close();
  end;
end;


// BPM -------------------------------------------------------------------------
procedure TAudioCapture.DoBpmTracking(const APosition100ns: Int64);
begin

  FPosition100ns := APosition100ns;

  if (FCurrentBpm <= 0.0) then
    Exit;

  if (FLastTick100ns = 0) or
     ((APosition100ns - FLastTick100ns) >= 250000) then
    begin
      FLastTick100ns := APosition100ns;

      if Assigned(FOnDeckTick) then
        FOnDeckTick(Self,
                    APosition100ns,
                    FCurrentBpm,
                    GetBeatPhase());
    end;

  if (APosition100ns >= FBeatOffset100ns) then
    begin
      if (GetBeatIndex() <> FLastBeatIndex) then
        begin
          FLastBeatIndex := GetBeatIndex();

          if Assigned(FOnBeat) then
            FOnBeat(Self,
                    APosition100ns,
                    FLastBeatIndex,
                    FCurrentBpm);
        end;
    end;
end;


procedure TAudioCapture.SetTempoPercent(const Value: Double);
var
  NewValue: Double;
begin

  NewValue := Value;

  if (NewValue < -16.0) then
    NewValue := -16.0
  else
    if (NewValue > 16.0) then
      NewValue := 16.0;

  if SameValue(FTempoPercent,
               NewValue,
               0.0001) then
    Exit;

  FTempoPercent := NewValue;
  UpdateCurrentBpm();
end;


function TAudioCapture.GetCurrentBpm(): Double;
begin

  Result := FCurrentBpm;
end;


function TAudioCapture.GetBeatLength100ns: Double;
begin

  if (FCurrentBpm <= 0.0) then
    Exit(0.0);

  Result := (60.0 * REFTIMES_PER_SEC) / FCurrentBpm;
end;


function TAudioCapture.GetBeatPhase(): Double;
var
  BeatLen: Double;
  PosAdj: Double;

begin

  Result := 0.0;

  BeatLen := GetBeatLength100ns;
  if (BeatLen <= 0.0) then
    Exit;

  PosAdj := FPosition100ns - FBeatOffset100ns;
  if (PosAdj < 0.0) then
    PosAdj := 0.0;

  Result := Frac(PosAdj / BeatLen);
end;


function TAudioCapture.GetBeatIndex(): Int64;
var
  BeatLen: Double;
  PosRel: Double;

begin

  Result := 0;

  BeatLen := GetBeatLength100ns();
  if (BeatLen <= 0.0) then
    Exit;

  PosRel := FPosition100ns - FBeatOffset100ns;
  if (PosRel < 0.0) then
    PosRel := 0.0;

  Result := Trunc(PosRel / BeatLen);
end;


procedure TAudioCapture.SetTrackBpm(const Value: Double);
begin

  if SameValue(FTrackBpm,
               Value,
               0.0001) then
    Exit;

  FTrackBpm := Value;
  UpdateCurrentBpm();
  FLastTick100ns := 0;
  FLastBeatIndex := -1;

  if Assigned(FOnBpmAnalyzed) and (FTrackBpm > 0.0) then
    FOnBpmAnalyzed(Self,
                   FTrackBpm);
end;


procedure TAudioCapture.UpdateCurrentBpm();
begin

  if (FTrackBpm <= 0.0) then
    FCurrentBpm := 0.0
  else
    FCurrentBpm := FTrackBpm * GetTempoFactor();
end;


function TAudioCapture.GetTempoFactor(): Double;
begin

  Result := 1.0 + (FTempoPercent / 100.0);

  if (Result < 0.10) then
    Result := 0.10;
end;


function TAudioCapture.GetPosition100ns(): Int64;
begin

  Result := FPosition100ns;
end;
// BPM end ---------------------------------------------------------------------

end.
