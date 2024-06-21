// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WASCapture_v3.pas
// Kind: Pascal / Delphi unit
// Release date: 15-06-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: WAS Loopback Capture Engine using TThread to render data.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 Rammstein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
unit ThreadedWASLoopbackCapture;

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
  System.Threading,
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
  Common,
  Writer;

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

  //
  //  WASAPI Capture class.
  //
  TWASCapture = class(TInterfacedPersistent,
                      IAudioSessionEvents,
                      IMMNotificationClient)
  private
    pvShutdownEvent: THandle;
    pvAudioSamplesReadyEvent: THandle;
    // Events for the mainform.
    pvOnCapturingStopped: TNotifyEvent;
    pvOnCapturingStart: TNotifyEvent;

    //
    //  Core Audio Capture member variables.
    //
    pvEndpoint: IMMDevice;
    pvAudioClient: IAudioClient;
    pvCaptureClient: IAudioCaptureClient;

    // WAV-Filewriter.
    pvWavWriter: TWavWriter;
    pvpUsePCMAudioFmt: Boolean;
    pvMixFormat: WAVEFORMATEX;

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

    //
    pvDeviceState: TDeviceState;

    function InitializeStreamSwitch(): Boolean;
    procedure TerminateStreamSwitch();
    function HandleStreamSwitchEvent(): Boolean;

    //
    // Thread functions.
    //
    procedure StartCapture();
    function RenderData(): HResult;


    //
    // Utility functions.
    //
    function InitializeAudioEngine(): Boolean;
    function GetMixFormat(out pMixFmt: WAVEFORMATEX;
                          pGetDefault: Boolean = False): HResult;

    function GetCannelCount(): Word;
    function GetSamplesPerSecond(): DWord;
    function GetBytesPerSample(): Word;

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

    // Public interface to TWASCapture.
    constructor Create(phObj: HWND;
                       pEndpoint: IMMDevice;
                       pEnableStreamSwitch: Boolean;
                       pDisableMmcss: Boolean;
                       pEndpointRole: ERole); reintroduce;

    destructor Destroy; override;

    //
    function Initialize(pBufferDuration: REFERENCE_TIME;
                        pEngineLatency: UINT32;
                        pUsePCMAudioFmt: Boolean = True): Boolean;

    procedure Shutdown();

    function Start(const pFileName: TFileName): Boolean;

    procedure Stop();

    //
    // Properties.
    //
    property EndPoint: IMMDevice read pvEndpoint write pvEndpoint;
    property MixFormat: WAVEFORMATEX read pvMixFormat;
    property ChannelCount: Word read GetCannelCount;
    property SamplesPerSecond: DWord read GetSamplesPerSecond;
    property BytesPerSample: Word read GetBytesPerSample;

    property FrameSize: SIZE_T read pvFrameSize;
    property BytesCaptured: Int64 read pvBytesCaptured;

    property DeviceState: TDeviceState read pvDeviceState;

    // Notify event.
    property OnStoppedCapturing: TNotifyEvent read pvOnCapturingStopped write pvOnCapturingStopped;
    property OnStartCapturing: TNotifyEvent read pvOnCapturingStart write pvOnCapturingStart;
  end;

  // This event type is used to pass back a HResult.
  TCallbackEvent = procedure(Sender: TObject;
                             const Hres: HRESULT) of object;

implementation


uses
  System.Services.Avrt;


constructor TWASCapture.Create(phObj: HWND;
                               pEndpoint: IMMDevice;
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

  // Set event ID's.
  pvShutdownEvent := 0;
  pvAudioSamplesReadyEvent := 0;
  pvStreamSwitchEvent := 0;
  pvStreamSwitchCompleteEvent := 0;

  pvFrameSize := 0;
  pvBufferSize := 0;
  pvCaptureBuffer := nil;
  pvCaptureBufferSize := 0;
  pvEnableStreamSwitch := False;
  // Create the WAV-filewriter.
  pvWavWriter := TWavWriter.Create();
end;


destructor TWASCapture.Destroy();
begin

  Shutdown();
  inherited Destroy;
end;


//
//  Initialize the stream switch logic.
//
function TWASCapture.InitializeStreamSwitch(): Boolean;
var
  hr: HResult;

begin

  hr := pvAudioClient.GetService(IID_IAudioSessionControl,
                                 pvAudioSessionControl);
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to retrieve session control: %d.',[GetLastError()]), hr);
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


procedure TWASCapture.TerminateStreamSwitch();
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
function TWASCapture.HandleStreamSwitchEvent(): Boolean;
var
  hr: HResult;
  bRes: Boolean;
  waitResult: DWord;
  wfxNew: WAVEFORMATEX;

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
      InfoMsg(optIDE, Format('Unable to stop audio client during stream switch: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  // Step 2.  Release our resources.  Note that we don't release the mix format, we need it for step 6.

  hr := pvAudioSessionControl.UnregisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to stop audio client during stream switch: %d.',[GetLastError()]), hr);
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
      InfoMsg(optIDE, Format('Stream switch timeout - aborting...: %d.',[waitResult]), E_FAIL);
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
      InfoMsg(optIDE, Format('Unable to retrieve new default device during stream switch: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  // Step 5 - Re-instantiate the audio client on the new endpoint.

  hr := pvEndpoint.Activate(IID_IAudioClient,
                            CLSCTX_INPROC_SERVER,
                            nil,
                            Pointer(pvAudioClient));
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to activate audio client on the new endpoint: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  // Step 6 - Retrieve the new mix format.

  hr := GetMixFormat(wfxNew,
                     pvpUsePCMAudioFmt);
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to retrieve mix format for new audio client: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  // Note that this is an intentionally naive comparison.
  // A more sophisticated comparison would compare the sample rate,
  // channel count and format and apply the appropriate conversions into the capture pipeline.

  if not CompareMem(@pvMixFormat,
                    @wfxNew,
                    SizeOf(WAVEFORMATEX) + wfxNew.cbSize) then
    begin
      InfoMsg(optIDE, Format('New mix format doesn''t match old mix format.  Aborting... : %d.',[E_FAIL]), E_FAIL);
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
      InfoMsg(optIDE, Format('Unable to retrieve session control on new audio client: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  hr := pvAudioSessionControl.RegisterAudioSessionNotification(Self);
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to retrieve session control on new audio client: %d.',[GetLastError()]), hr);
      goto ErrorExit;
    end;

  // Reset the stream switch complete event because it's a manual reset event.
  ResetEvent(pvStreamSwitchCompleteEvent);

  // And we're done.  Start capturing again.

  hr := pvAudioClient.Start();
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to start the new audio client: %d.',[GetLastError()]), hr);
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
function TWASCapture.InitializeAudioEngine(): Boolean;
var
  hr: HResult;
  hnsDefaultDevicePeriod: REFERENCE_TIME;
  hnsMinimumDevicePeriod: REFERENCE_TIME;
  hnsLatency: REFERENCE_TIME;

begin
  // Let the endpoint we selected - when not the default - creates the audioclient.
  hr := pvEndpoint.Activate(IID_IAudioClient,
                            CLSCTX_ALL,
                            nil,
                            Pointer(pvAudioClient));

  if FAILED(hr) then
    Exit(SUCCEEDED(hr));

  // Retrieve the correct wav format to write to files.
  hr := GetMixFormat(pvMixFormat,
                     pvpUsePCMAudioFmt);
  if FAILED(hr) then
    Exit(False);

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
                                 @pvMixFormat,
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
      InfoMsg(optIDE, Format('Unable to get audio client buffer: %d.',[GetLastError()]), hr);
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
      InfoMsg(optIDE, Format('Unable to set ready event: %d.',[GetLastError()]), hr);
      Exit(SUCCEEDED(hr));
    end;

  // Get the capture client.
  hr := pvAudioClient.GetService(IID_IAudioCaptureClient,
                                 IUnknown(pvCaptureClient));
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to get new capture client: %d.',[GetLastError()]), hr);
      Exit(SUCCEEDED(hr));
    end;

  Result := SUCCEEDED(hr);
end;

//
//  Retrieve the format we'll use to capture samples.
//
//  We use the Mix format since we're capturing in shared mode.
//
function TWASCapture.GetMixFormat(out pMixFmt: WAVEFORMATEX;
                                  pGetDefault: Boolean = False): HResult;
var
  hr: HResult;

begin

  if pGetDefault then
    begin
      pMixFmt := GetDefaultWaveFmtEx();
      hr := S_OK;
    end
  else
    begin
      hr := pvAudioClient.GetMixFormat(@pMixFmt);
      if FAILED(hr) then
        begin
          InfoMsg(optIDE, Format('Unable to get mix format on audio client: %d.',[GetLastError()]), hr);
          pvFrameSize := 0;
          Exit(hr);
        end;
    end;

  // Calculate the framesize.
  pvFrameSize := (pMixFmt.wBitsPerSample div 8) * pMixFmt.nChannels;
  Result := hr;
end;


// Property read function.

function TWASCapture.GetCannelCount(): Word;
begin

  Result := pvMixFormat.nChannels;
end;


function TWASCapture.GetSamplesPerSecond(): DWord;
begin

  Result := pvMixFormat.nSamplesPerSec;
end;


function TWASCapture.GetBytesPerSample(): Word;
begin

  Result := pvMixFormat.wBitsPerSample div 8;
end;


/// IAudioSessionEvents methods ////////////////////////////////////////////////

function TWASCapture.OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                          const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnIconPathChanged(NewIconPath: LPCWSTR;
                                       const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnSimpleVolumeChanged(NewSimpleVolume: Single;
                                                   NewMute: BOOL;
                                                   const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnChannelVolumeChanged(ChannelCount: UINT;
                                                    NewChannelVolumeArray: PSINGLE;
                                                    ChangedChannel: UINT;
                                                    const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnGroupingParamChanged(const NewGroupingParam: TGUID;
                                            const EventContext: TGUID): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnStateChanged(NewState: AudioSessionState): HResult;
begin

  Result := S_OK;
end;

//
// Called when an audio session is disconnected.
//
// When a session is disconnected because of a device removal or format change event,
// we just want to let the capture thread know that the session's gone away.
//
function TWASCapture.OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult;
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

function TWASCapture.OnDeviceStateChanged(DeviceId: LPCWSTR;
                                          NewState: DWord): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnDeviceAdded(DeviceId: LPCWSTR): HResult;
begin

  Result := S_OK;
end;


function TWASCapture.OnDeviceRemoved(DeviceId: LPCWSTR): HResult;
begin

  Result := S_OK;
end;

//
// Called when the default capture device changed.
// We just want to set an event which lets the stream switch logic know that it's Ok to
// continue with the stream switch.
//
function TWASCapture.OnDefaultDeviceChanged(Flow: EDataFlow;
                                            Role: ERole;
                                            NewDefaultDeviceId: LPCWSTR): HResult;
begin

  if (Flow = EDataFlow(eCapture)) and (Role = pvEndpointRole) then
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


function TWASCapture.OnPropertyValueChanged(DeviceId: LPCWSTR;
                                            const Key: PROPERTYKEY): HResult;
begin

  Result := S_OK;
end;


/// Public methods /////////////////////////////////////////////////////////////

//
//  Initialize the capturer.
//
function TWASCapture.Initialize(pBufferDuration: REFERENCE_TIME;
                                pEngineLatency: UINT32;
                                pUsePCMAudioFmt: Boolean = True): Boolean;
begin

  //  Create our shutdown and samples ready events- we want auto reset events that
  //  start in the not-signaled state.

  pvShutdownEvent := CreateEventEx(nil,
                                   nil,
                                   0,
                                   EVENT_MODIFY_STATE or SYNCHRONIZE);

  if (pvShutdownEvent = 0) then
    begin
      InfoMsg(optIDE, Format('Unable to create shutdown event: %d.',[E_FAIL]), E_FAIL);
      Exit(False);
    end;

  pvAudioSamplesReadyEvent := CreateEventEx(nil,
                                            nil,
                                            0,
                                            EVENT_MODIFY_STATE or SYNCHRONIZE);

  if (pvAudioSamplesReadyEvent = 0) then
    begin
      InfoMsg(optIDE, Format('Unable to create samples ready event: %d.',[E_FAIL]), E_FAIL);
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
      InfoMsg(optIDE, Format('Unable to create stream switch event: %d.',[E_FAIL]), E_FAIL);
      Exit(False);
    end;

  // Remember our configured latency in case we'll need it for a stream switch later.
  pvEngineLatency := pEngineLatency;
  pvBufferDuration := pBufferDuration;
  pvpUsePCMAudioFmt := pUsePCMAudioFmt;

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
procedure TWASCapture.Shutdown();
begin

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

    if Assigned(pvWavWriter) then
      FreeAndNil(pvWavWriter);
end;


//
//  Start capturing...
//
function TWASCapture.Start(const pFileName: TFileName): Boolean;
var
  hr: HResult;

begin

  pvFileName := pFileName;

  // Now the stream will be rendered in another thread.
  // So, we need to create another thread to keep control.
  //
  // Note that, when this audiostream is over,
  // the end of buffer will be signaled first, before signal endofstream.

  // Start. The rendering loop will be done in a separate task.
  StartCapture();

  // We're ready to go, start capturing!
  hr := pvAudioClient.Start();
  if FAILED(hr) then
    begin
      InfoMsg(optIDE, Format('Unable to start capture client: %d.',[GetLastError()]), hr);
      Exit(False);
    end;

  Result := True;
end;

//
//  Stop the capturer.
//
procedure TWASCapture.Stop();
var
  hr: HResult;

begin
  hr := S_OK;

  pvDeviceState := Stopping;
  // Tell the capture thread to shut down, wait for the thread to complete then clean up all the stuff we
  // allocated in Start().

  if (pvShutdownEvent <> 0) then
    SetEvent(pvShutdownEvent);

  if Assigned(pvAudioClient) then
    begin
      hr := pvAudioClient.Stop();
      if FAILED(hr) then
        InfoMsg(optIDE, Format('Unable to stop audio client: %d',[GetLastError()]), hr);
    end;

  if SUCCEEDED(hr) then
    begin
      // Signal the mainform capturing has been stopped.
      pvOnCapturingStopped(Self);
      Sleep(1); // Sleep to prevent a mainform timer would stop before the notify event has been processed.
      pvDeviceState := Stopped;
    end;
end;


function TWASCapture.RenderData(): HResult;
var
  hr: HResult;
  mr: MMRESULT;
  waitResult: DWord;
  packetSize: UINT32;
  pData: PByte;
  NumFramesToRead: UINT32;
  flags: DWord;
  dwBytesWritten: LongInt;
  MmcssHandle: THandle;
  MmcssTaskIndex: DWord;
  WaitArray: array[0..2] of THandle;
  CkRIFF: MMCKINFO;
  CkData: MMCKINFO;

begin
  hr := S_OK;
  mr := MMSYSERR_NOERROR;
  MmcssHandle := 0;
  MmcssTaskIndex := 0;
  WaitArray[0] := pvShutdownEvent;
  WaitArray[1] := pvStreamSwitchEvent;
  WaitArray[2] := pvAudioSamplesReadyEvent;

try
  // Initialize COM concurrency model multithreaded.
  hr := CoInitializeEx(nil,
                       COINIT_MULTITHREADED);
  if FAILED(hr) then
    begin
      pvDeviceState := Error;
      InfoMsg(optIDE, Format('Unable to initialize COM: %d',[GetLastError()]), E_FAIL);
    end;

  mr := pvWavWriter.CreateFile(StrToPWidechar(pvFileName));
  // If a previous method report a failure, we handle that here.
  if (mr <> MMSYSERR_NOERROR) then
    begin
      InfoMsg(optIDE, Format('Unable to create wav-file ''%s'': %d.',[pvFileName, GetLastError()]), E_FAIL);
      Exit(E_FAIL);
    end;

  // Write the wavfile header
  mr := pvWavWriter.WriteWaveHeader(@pvMixFormat,
                                    CkRIFF,
                                    CkData);
  if (mr <> MMSYSERR_NOERROR) then
    begin
      InfoMsg(optIDE, Format('Unable to write the wavfile header: %d',[GetLastError()]), E_FAIL);
      Exit(E_FAIL);
    end;

  if not (pvDisableMMCSS) then
    begin
      MmcssHandle := AvSetMmThreadCharacteristics('Audio',
                                                  @MmcssTaskIndex);
      if (MmcssHandle = 0) then
        InfoMsg(optIDE, Format('Unable to enable MMCSS on capture thread: %d',[GetLastError()]), E_FAIL);
    end;

  hr := pvCaptureClient.GetNextPacketSize(packetSize);
  if FAILED(hr) then
    begin
      pvDeviceState := Error;
      InfoMsg(optIDE, Format('Unable to get first packetsize: %d',[GetLastError()]), E_FAIL);
    end;


  // When successfully reached to here, set the status to 'Capturing'.
  pvDeviceState := Capturing;

  while (pvDeviceState = Capturing) do
    begin
      waitResult := WaitForMultipleObjects(3,
                                           @WaitArray,
                                           False,
                                           INFINITE);

          case waitResult of
            // pvShutdownEvent
            WAIT_OBJECT_0 + 0:
              // We're done, exit the loop.
              pvDeviceState := Stopping;

            // pvStreamSwitchEvent
            WAIT_OBJECT_0 + 1:

              // We need to stop the capturer,
              // tear down the pvAudioClient and pvCaptureClient objects and
              // re-create them on the new endpoint if possible.
              // If this fails, abort the thread.
              if not HandleStreamSwitchEvent() then
                pvDeviceState := Error;

            // pvAudioSamplesReadyEvent
            WAIT_OBJECT_0 + 2:
              begin
                //
                //  Find out how much capture data is available.  We need to make sure we don't run over the length
                //  of our capture buffer.  We'll discard any samples that don't fit in the buffer.
                //
                hr := pvCaptureClient.GetBuffer(pData,
                                                NumFramesToRead,
                                                flags,
                                                nil,
                                                nil);
                if SUCCEEDED(hr) then
                  begin

                    if (flags = AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) then
                      begin
                        pData := nil;
                        Continue;
                      end;

                    // Tell WriteData to write silence.
                    // When a sound is detected, the app will act and process data.
                    if (flags = AUDCLNT_BUFFERFLAGS_SILENT) then
                      begin
                        pData := nil;
                        Continue;
                      end;

                    // Write the available capture data to the audio sink.
                    mr := pvWavWriter.WriteData(pData,
                                                NumFramesToRead,
                                                pvMixFormat.nBlockAlign,
                                                dwBytesWritten);
                    // Note: The writer will automaticly closes the file when a HResult <> S_OK.
                    if (mr <> MMSYSERR_NOERROR) then
                      if (mr = MMIOERR_CANNOTEXPAND) then
                        pvDeviceState := MaxFileSizeReached
                      else
                        pvDeviceState := Error;
                  end;

                hr := pvCaptureClient.ReleaseBuffer(NumFramesToRead);
                if FAILED(hr) then
                  pvDeviceState := Error;

                // `Keep score.
                Inc(pvBytesCaptured,
                    dwBytesWritten);

                hr := pvCaptureClient.GetNextPacketSize(packetSize);
                if FAILED(hr) then
                  pvDeviceState := Error;
              end;
          end; // case
        end; // while

  pvDeviceState := Stopped;

finally

  pData := nil;
  if (MmcssHandle > 0) then
    AvRevertMmThreadCharacteristics(MmcssHandle);

  // Close the file when succeeded.
  // Note that when an error or other HResult value occurs, the writefile method will automatcly closes the file.
  if SUCCEEDED(mr) then
    pvWavWriter.CloseFile(CkRIFF,
                          CkData);

  CoUnInitialize;
  Result := hr;
end;
end;


procedure TWASCapture.StartCapture();
var
  Task: ITask;

begin

  Task := TTask.Create(procedure
                       var
                         hr: HResult;

                       begin

                         // The function where we render the audio data and
                         // (de)activate the MMCSS feature.
                         // See: https://learn.microsoft.com/en-us/windows/win32/procthread/multimedia-class-scheduler-service
                         // To get the best performance, it's recomended to set "Best Performance" in Windows energy settings.
                         hr := RenderData();
                         if FAILED(hr) then
                           begin
                             TThread.Queue(nil,
                                           procedure
                                           begin
                                             pvDeviceState := Error; // Update the status in the main thread if the function fails.
                                           end);
                           end;
                       end);
  //
  // Send message to mainform the capturing started, enable the timer.
  //
  pvOnCapturingStart(Self);

  // Let's go!
  Task.Start;
end;

end.
