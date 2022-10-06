// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MediaPlayer.pas
// Kind: Pascal Unit
// Release date: 05-07-2020
// Language: ENU
//
// Version: 3.1.1
// Description: This sample implements a simple media player that responds to the "ducking"
//              feature in Windows 7 and higher.
//              It also implements a volume control which tracks
//              to the volume control in the volume mixer.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/06/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX312
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Ducking Media Player Sample Project
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit MediaPlayer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.DirectShow9, {or DSPack DirectShow}
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Dialogs,
  {CoreAudioApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.MMDeviceApi;

 {$I 'WinApiTypes.inc'}

const
  WM_APP_SESSION_DUCKED               = WM_APP;
  WM_APP_SESSION_UNDUCKED             = WM_APP + 1;
  WM_APP_GRAPHNOTIFY                  = WM_APP + 2;
  WM_APP_SESSION_VOLUME_CHANGED       = WM_APP + 3;  // wParam = Mute State, lParam = (float) new volume 0.0 - 1.0

  // NOTE: The TCMediaPlayer class is posting messages.
  //       Be sure, your application has an "OnMessage" method ( like the TMessageEvent ) as described at
  //       http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Forms_TApplication_OnMessage.html
  //       to process these messages.

type

  TCMediaPlayer = class(TInterfacedPersistent, IAudioVolumeDuckNotification, IAudioSessionEvents)
  private
    _FileName: PWideChar;
    _GraphBuilder: IGraphBuilder;
    _MediaSeeking: IMediaSeeking;
    _MediaEvent: IMediaEventEx;
    _DuckingRegistered: Boolean;
    _SessionNotificationRegistered: Boolean;
    _AppWindow: HWND;

    //
    //  Event context used for media player volume changes.  This allows us to determine if a volume change request was initiated by
    //  this media player application or some other application.
    //
    _MediaPlayerEventContext: TGUID;
    _MediaPlayerTime: REFERENCE_TIME;
    _SessionManager2: IAudioSessionManager2;
    _SimpleVolume: ISimpleAudioVolume;
    _SessionControl2: IAudioSessionControl2;


   // IAudioVolumeDuckNotification implementation
    function OnVolumeDuckNotification(sessionID: LPCWSTR;
                                      countCommunicationSessions: UINT32): HResult; stdcall;

    function OnVolumeUnduckNotification(sessionID: LPCWSTR): HResult; stdcall;


   // IAudioSessionEvents implementation
    function OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                  const EventContext: TGUID): HResult; stdcall;

    function OnIconPathChanged(NewIconPath: LPCWSTR;
                               const EventContext: TGUID): HResult; stdcall; { return S_OK; }

    function OnSimpleVolumeChanged(NewVolume: FLOAT;
                                   NewMute: BOOL;
                                   const EvntContext: TGUID): HResult; stdcall;

    function OnChannelVolumeChanged(ChannelCount: UINT;
                                    NewChannelArray: PFloat;
                                    ChangedChannel: UINT;
                                    const EventContext: TGUID): HResult; stdcall; { return S_OK; }

    function OnGroupingParamChanged(NewGroupingParam: TGUID;
                                    const EventContext: TGUID): HResult; stdcall; { return S_OK; }

    function OnStateChanged(NewState: AudioSessionState): HResult; stdcall; { return S_OK; }

    function OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult; stdcall; { return S_OK; }


   // Other

    function GetSessionManager2(): HResult; stdcall;
    function GetSessionControl2(): HResult; stdcall;
    function GetSimpleVolume(): HResult; stdcall;
    function GetCurrentSessionId(var SessionId: PWideChar): HResult; stdcall;

  public

    //
    //  Constuctor for the "Media Player" application.
    //
    //  Initialize a bunch of stuff.
    //
    constructor Create(AppWindow: HWND);
    destructor Destroy(); override;

    // Initialize the media player.
    function Initialize(): HResult;
    // Shuts down the media player if it's currently active.
    procedure Shutdown();
    procedure SyncPauseOnDuck(PauseOnDuckChecked: Boolean);
    procedure SyncDuckingOptOut(DuckingOptOutChecked: Boolean);
    procedure Play();
    procedure Stop();
    function Pause(): BOOL;
    procedure SetVolume(Volume: Single);
    function GetVolume(): Single;
    procedure SetMute(Mute: BOOL);
    function GetMute(): BOOL;
    function Continue(): BOOL;
    // Toggles the "Pause" state for the player.
    function TogglePauseState(): Boolean;
    function HandleGraphEvent(): Boolean;
    procedure RemoveAllFilters();
    function SetFileName(FileName: PWideChar): Boolean;
    // Returns the current position in the file.
    function GetPosition(): LongInt;

    // Not implemented
    //procedure OnSessionDucked();
    //procedure OnSessionUnducked();

    property Duration: REFERENCE_TIME read _MediaPlayerTime;

end;


implementation

//
//  Constuctor for the "Media Player" application.
//
//  Initialize a bunch of stuff.
//
constructor TCMediaPlayer.Create(AppWindow: HWND);
begin
  inherited Create();
  _FileName := Nil;
  _GraphBuilder := Nil;
  _MediaSeeking := Nil;
  _MediaPlayerTime := 0;
  _MediaEvent := Nil;
  _AppWindow := AppWindow;
  _DuckingRegistered := False;
  _SimpleVolume := Nil;
  _SessionNotificationRegistered := False;
  _SessionControl2 := Nil;
  _SessionManager2 := Nil;

  // UuidCreate(_MediaPlayerEventContext);
  {void} CreateGuid(_MediaPlayerEventContext);

end;

//
//  Destructor for the "Media Player".
//
//  Does nothing because all the cleanup happened in the Shutdown() method.
//
destructor TCMediaPlayer.Destroy();
begin
  inherited Destroy();
end;

//
//  Initialize the media player.  Instantiates DShow, retrieves the session control for
//  the current audio session and registers for notifications on that session control.
//
function TCMediaPlayer.Initialize(): HResult;
var
  hr: HResult;

begin

  hr := CoCreateInstance(CLSID_FilterGraph,
                         Nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IGraphBuilder,
                         Pointer(_GraphBuilder));
  if Succeeded(hr) then
    begin
      hr := _GraphBuilder.QueryInterface(IID_IMediaEvent,
                                         _MediaEvent);
      if Succeeded(hr) then
        hr := _MediaEvent.SetNotifyWindow(_AppWindow,
                                          WM_APP_GRAPHNOTIFY,
                                          0);
      if Succeeded(hr) then
        hr := _GraphBuilder.QueryInterface(IID_IMediaSeeking,
                                           _MediaSeeking);
    end;

  if Succeeded(hr) then
    begin
      hr := GetSessionControl2();
      if SUCCEEDED(hr) then
        begin
          hr := _SessionControl2.RegisterAudioSessionNotification(Self);
          if Succeeded(hr) then
            _SessionNotificationRegistered := True;
        end;
    end;
  Result := hr;
end;

//
//  Shut down the media player - releases all the resources associated with the media player.
//
procedure TCMediaPlayer.Shutdown();
var
 hr: HResult;

begin
  SafeRelease(_MediaEvent);
  SafeRelease(_MediaSeeking);
  SafeRelease(_GraphBuilder);

  if Assigned(_SessionManager2) then
    begin
      if _DuckingRegistered then
        begin
          hr := _SessionManager2.UnregisterDuckNotification(Self);
          if Failed(hr) then
				    // Failures here are highly unlikely and could indicate an application defect
            MessageBox(_AppWindow,
                       lpcwstr('Unable to unregister for ducking notifications'),
                       lpcwstr('Stop Player Error'),
                       MB_OK);
            _DuckingRegistered := False;
        end;
      SafeRelease(_SessionManager2);
    end;

  SafeRelease(_SimpleVolume);

  if Assigned(_SessionControl2) then
    begin
      if _SessionNotificationRegistered then
        begin
          hr := _SessionControl2.UnregisterAudioSessionNotification(Self);
          if Failed(hr) then
            // Failures here are highly unlikely and could indicate an application defect
            MessageBox(_AppWindow,
                       lpcwstr('Unable to unregister for session notifications'),
                       lpcwstr('Stop PlayerError'),
                       MB_OK);

          _SessionNotificationRegistered := False;
        end;
      SafeRelease(_SessionControl2);
    end;
end;

//
//  Removes any filters in the audio graph - called before rebuilding the audio graph.
//
procedure TCMediaPlayer.RemoveAllFilters();
var
  hr: HResult;
  enumFilters: IEnumFilters;
  filter: IBaseFilter;

begin
  hr := _GraphBuilder.EnumFilters(enumFilters);

  if Succeeded(hr) then
    begin
      hr := enumFilters.Next(1,
                             filter,
                             Nil);
      while (hr = S_OK) do
        begin
          //
          //  Remove the filter from the graph.
          //
          _GraphBuilder.RemoveFilter(filter);
          SafeRelease(filter);
          //
          //  Reset the enumeration since we removed the filter (which invalidates the enumeration).
          //
          enumFilters.Reset();

          hr := enumFilters.Next(1,
                            filter,
                            Nil);
        end;
      SafeRelease(enumFilters);
    end;
end;

//
//  Sets the file we're going to play.
//
//  Returns true if the file can be played, false otherwise.
//
function TCMediaPlayer.SetFileName(FileName: PWideChar): Boolean;
var
  hr: HResult;
  caps: DWord;
  canSeek: Boolean;

begin
  RemoveAllFilters();

  if Not Assigned(FileName) then
    begin
      Result := False;
      Exit;
    end;

  _FileName := FileName;

  //
  //  Ask DirectShow to build a render graph for this file.
  //
  hr := _GraphBuilder.RenderFile(_FileName,
                                 Nil);
  if Failed(hr) then
    begin
      MessageBox(_AppWindow,
                 lpcwstr('Unable to build graph for media file'),
                 lpcwstr('Set Filename Error'),
                 MB_OK);
      Result := False;
      Exit;
    end;

  //
  //  If we can figure out the length of this track retrieving it.
  //
  caps := AM_SEEKING_CanGetDuration;
  canSeek := (S_OK = _MediaSeeking.CheckCapabilities(caps));

  if canSeek then
    _MediaSeeking.GetDuration(_MediaPlayerTime);

  Result := True;

end;

//
//  Starts the media player.
//
procedure TCMediaPlayer.Play();
var
  mediaControl: IMediaControl;
  timeBegin, timeStop: REFERENCE_TIME;

begin
  timeBegin := 0;
  timeStop  := 0;

  _GraphBuilder.QueryInterface(IID_IMediaControl,
                               mediaControl);

  if Assigned(mediaControl) then
    begin
      _MediaSeeking.SetPositions(timeBegin,
                                 AM_SEEKING_AbsolutePositioning,
                                 timeStop,
                                 AM_SEEKING_NoPositioning);

      mediaControl.Run();
    end
end;


//
//  Pauses media playback if it is currently running.
//
function TCMediaPlayer.Pause(): BOOL;
var
  hr: HResult;
  isPaused: BOOL;
  mediaControl: IMediaControl;

begin
  isPaused := False;
  hr := _GraphBuilder.QueryInterface(IID_IMediaControl,
                                     mediaControl);

  if Succeeded(hr) then
    begin
      mediaControl.Pause();
      isPaused := True;
    end;

  Result := isPaused;
end;


//
//  Continues media playback if it is currently paused.
//
function TCMediaPlayer.Continue(): BOOL;
var
  hr: HRESULT;
  isContinued: BOOL;
  mediaControl: IMediaControl;

begin
  isContinued := False;
  hr := _GraphBuilder.QueryInterface(IID_IMediaControl,
                                     mediaControl);
  if Succeeded(hr) then
    begin
      mediaControl.Run();
      isContinued := True;
    end;

  Result := isContinued;
end;


//
//  Toggle the pause state for the media player.  Returns true if the media player pauses, false if it runs.
//
function TCMediaPlayer.TogglePauseState(): Boolean;
var
  hr: HRESULT;
  isPaused: BOOL;
  mediaControl: IMediaControl;
  filterState: TFilterState;

begin
  isPaused := False;
  hr := _GraphBuilder.QueryInterface(IID_IMediaControl,
                                     mediaControl);
  if Succeeded(hr) then
    begin
      hr := mediaControl.GetState(INFINITE,
                                  filterState);
      if Succeeded(hr) then
        begin
          if (filterState = State_Running) then
            begin
              mediaControl.Pause();
              isPaused := True;
            end
          else if (filterState = State_Paused) then
            begin
              mediaControl.Run();
              isPaused := False;
            end;
        end;
    end;
  Result := isPaused;
end;


//
//  Stops media playback.
//
procedure TCMediaPlayer.Stop();
var
  hr: HRESULT;
  mediaControl: IMediaControl;

begin
  hr := _GraphBuilder.QueryInterface(IID_IMediaControl,
                                     mediaControl);
  if Succeeded(hr) then
     mediaControl.Stop();
end;


//
//  Handles DirectShow graph events.
//
//  Returns true if the player should be stopped.
//
function TCMediaPlayer.HandleGraphEvent(): Boolean;
var
  stopped: Boolean;
  evCode: LongInt;
  param1, param2: LONG_PTR;
  timeBegin, timeStop: REFERENCE_TIME;

begin
  stopped := False;
  timeBegin := 0;
  timeStop := 0;
  // Disregard if we don't have an IMediaEventEx pointer.
  if not Assigned(_MediaEvent) then
    begin
      Result := stopped;
      Exit;
    end;


  // Get all the events
  while Succeeded(_MediaEvent.GetEvent(evCode,
                                       param1,
                                       param2,
                                       0)) do
    begin
      _MediaEvent.FreeEventParams(evCode,
                                  param1,
                                  param2);
      case evCode of
        EC_COMPLETE:       begin
                             // Stop playback, we're done.
                             Stop();
                             _MediaSeeking.SetPositions(timeBegin,
                                                        AM_SEEKING_AbsolutePositioning,
                                                        timeStop,
                                                        AM_SEEKING_NoPositioning);
                             stopped := True;
                             Break;
                           end;

        EC_USERABORT:      begin { Fall through. } end;
        EC_ERRORABORT:     begin
                             stopped := false;
                           end;

      end;
    end;
  Result := stopped;
end;

//
//  Returns the position in the song being played in units 0..1000
//
function TCMediaPlayer.GetPosition(): LongInt;
var
  sliderTick: LongInt;
  position: REFERENCE_TIME;

begin
  if Assigned(_MediaSeeking) and (_MediaPlayerTime > 0) then
    begin
      if SUCCEEDED(_MediaSeeking.GetCurrentPosition(position)) then
        begin
          sliderTick := LongInt((position * 1000) div _MediaPlayerTime);
          Result := sliderTick;
          Exit;
        end;
    end;
  Result := 0;
end;




//
//  Sync's the "Pause On Duck" state for the media player.
//
//  Either registers or unregisters for ducking notification.
//
procedure TCMediaPlayer.SyncPauseOnDuck(PauseOnDuckChecked: Boolean);
var
  hr: HResult;
  sessionId: PWideChar;

begin
  hr := GetSessionManager2();

  //
  //  Retrieve the current session ID.  We'll use that to request that the ducking manager
  //  filter our notifications (so we only see ducking notifications for our session).
  //
  if Succeeded(hr) then
    begin
      hr := GetCurrentSessionId(sessionId);

      //
      //  And either register or unregister for ducking notifications based on whether or not the Pause On Duck state is checked.
      //
      if Succeeded(hr) then
        begin
          if PauseOnDuckChecked then
            begin
              if _DuckingRegistered then
                begin
                  hr := _SessionManager2.RegisterDuckNotification(sessionId,
                                                                  Self);
                  if Succeeded(hr) then
                    _DuckingRegistered := True;
                end;
            end
          else
            begin
              if _DuckingRegistered then
                begin
                  hr := _SessionManager2.UnregisterDuckNotification(Self);
                  if Succeeded(hr) then
                    _DuckingRegistered := False;
                end;
            end;

          if Failed(hr) then
            MessageBox(_AppWindow,
                       lpcwstr('Unable to register or unregister for ducking notifications'),
                       lpcwstr('Sync Ducking Pause Error'),
                       MB_OK);

          CoTaskMemFree(sessionId);
        end;
    end;
end;


//
//  When we receive a duck notification, post a "Session Ducked" message to the application window.
//
function TCMediaPlayer.OnVolumeDuckNotification(sessionID: LPCWSTR;
                                                countCommunicationSessions: UINT32): HResult;
begin
  PostMessage(_AppWindow,
              WM_APP_SESSION_DUCKED,
              0,
              0);
  Result := S_OK;
end;


//
//  When we receive an unduck notification, post a "Session Unducked" message to the application window.
//
function TCMediaPlayer.OnVolumeUnduckNotification(SessionID: PWideChar): HResult;
begin
  PostMessage(_AppWindow,
              WM_APP_SESSION_UNDUCKED,
              0,
              0);
  Result := S_OK;
end;


//
//  Sync the "Ducking Opt Out" state with the UI - either enable or disable ducking for this session.
//
procedure TCMediaPlayer.SyncDuckingOptOut(DuckingOptOutChecked: Boolean);
var
  hr: HResult;

begin
  hr := GetSessionControl2();

  //
  //  Sync our ducking state to the UI.
  //
  if Succeeded(hr) then
    begin
      if DuckingOptOutChecked then
        hr := _SessionControl2.SetDuckingPreference(True)
      else
        hr := _SessionControl2.SetDuckingPreference(False);

      if Failed(hr) then
        MessageBox(_AppWindow,
                   lpcwstr('Unable to update the ducking preference'),
                   lpcwstr('Sync Ducking State Error'),
                   MB_OK);

    end;
end;


//
//  Get the volume on the current audio session.
//
function TCMediaPlayer.GetVolume(): Single;
var
  hr: HResult;
  volume: Single;

begin
  hr := GetSimpleVolume();
  if Succeeded(hr) then
    begin
      hr := _SimpleVolume.GetMasterVolume(volume);
      if Succeeded(hr) then
        begin
          Result := volume;
          Exit;
        end
      else
        MessageBox(_AppWindow,
                   lpcwstr('Unable to retrieve volume for current session'),
                   lpcwstr('Get Volume Error'),
                   MB_OK);
    end
  else
    MessageBox(_AppWindow,
               lpcwstr('Unable to retrieve simple volume control for current session'),
               lpcwstr('Get Volume Error'),
               MB_OK);

  Result := 0.0;
end;


//
//  Set the volume on the current audio session.
//
//  We set a specific event context on the SetMasterVolume call - when we receive the simple volume changed
//  notification we can use this event context to determine if the volume change call came from our application or another
//  application.
//
procedure TCMediaPlayer.SetVolume(Volume: Single);
var
  hr: HResult;

begin
  hr := GetSimpleVolume();
  if Succeeded(hr) then
    begin
      hr := _SimpleVolume.SetMasterVolume(Volume,
                                          @_MediaPlayerEventContext);
      if Failed(hr) then
        MessageBox(_AppWindow,
                   lpcwstr('Unable to retrieve volume for current session'),
                   lpcwstr('Set Volume Error'),
                   MB_OK);
    end
  else
    MessageBox(_AppWindow,
               lpcwstr('Unable to retrieve simple volume control for current session'),
               lpcwstr('Set Volume Error'),
               MB_OK);

end;


//
//  Get the mute state for the current audio session.
//
function TCMediaPlayer.GetMute(): BOOL;
var
  hr: HResult;
  mute: BOOL;

begin
  hr := GetSimpleVolume();
  if Succeeded(hr) then
    begin
      hr := _SimpleVolume.GetMute(mute);
      if Succeeded(hr) then
        begin
          Result := mute;
          Exit;
        end
      else
        MessageBox(_AppWindow,
                   lpcwstr('Unable to retrieve mute for current session'),
                   lpcwstr('Get Mute Error'),
                   MB_OK);
    end
  else
    MessageBox(_AppWindow,
               lpcwstr('Unable to retrieve simple volume control for current session'),
               lpcwstr('Get Mute Error'),
               MB_OK);

  Result := False;
end;


//
//  Set the mute state on the current audio session.
//
//  We set a specific event context on the SetMasterVolume call - when we receive the simple volume changed
//  notification we can use this event context to determine if the volume change call came from our application or another
//  application.
//
procedure TCMediaPlayer.SetMute(Mute: BOOL);
var
  hr: HResult;

begin
  hr := GetSimpleVolume();
  if Succeeded(hr) then
    begin
      hr := _SimpleVolume.SetMute(Mute,
                                  @_MediaPlayerEventContext);
      if Failed(hr) then
        MessageBox(_AppWindow,
                   lpcwstr('Unable to set mute for current session'),
                   lpcwstr('Set Mute Error'),
                   MB_OK);
    end
  else
    MessageBox(_AppWindow,
               lpcwstr('Unable to retrieve simple volume control for current session'),
               lpcwstr('Set Mute Error'),
               MB_OK);
end;






// IAudioSessionEvents implementations
function TCMediaPlayer.OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                            const EventContext: TGUID): HResult;
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;


function TCMediaPlayer.OnIconPathChanged(NewIconPath: LPCWSTR;
                                         const EventContext: TGUID): HResult;
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;


//
//  Someone changed the volume on the media player session.
//
//  If the person making the change wasn't the current player, let our UI know that the volume changed (and what the new volume is)
//  so it can update the UI to reflect the new state.
//
function TCMediaPlayer.OnSimpleVolumeChanged(NewVolume: FLOAT;
                                             NewMute: BOOL;
                                             const EvntContext: TGUID): HResult;
begin
  if IsEqualGuid(EvntContext, _MediaPlayerEventContext) then
    PostMessage(_AppWindow,
                WM_APP_SESSION_VOLUME_CHANGED,
                WParam(NewMute),
                LParam(@NewVolume));

  Result := S_OK;
end;


function TCMediaPlayer.OnChannelVolumeChanged(ChannelCount: UINT;
                                              NewChannelArray: PFloat;
                                              ChangedChannel: UINT;
                                              const EventContext: TGUID): HResult; stdcall; { return S_OK; }
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;


function TCMediaPlayer.OnGroupingParamChanged(NewGroupingParam: TGUID;
                                              const EventContext: TGUID): HResult; stdcall; { return S_OK; }
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;


function TCMediaPlayer.OnStateChanged(NewState: AudioSessionState): HResult; stdcall; { return S_OK; }
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;


function TCMediaPlayer.OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult; stdcall; { return S_OK; }
begin
  // to prevent "Return value might be undefined" compilerwarning
  Result := S_OK;
end;

// End implementations of IAudioSessionEvents


//
//  Utility function to retrieve the session manager for the default audio endpoint.
//
function TCMediaPlayer.GetSessionManager2(): HResult;
var
  hr: HResult;
  deviceEnumerator: IMMDeviceEnumerator;
  endpoint: IMMDevice;

begin
  hr := S_OK;
  if not Assigned(_SessionManager2) then
    begin
      //
      //  Start with the default endpoint.
      //
      hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                             Nil,
                             CLSCTX_INPROC_SERVER,
                             IID_IMMDeviceEnumerator,
                             deviceEnumerator);

      if Succeeded(hr) then
        hr := deviceEnumerator.GetDefaultAudioEndpoint(eRender,
                                                       eConsole,
                                                       endpoint)
      else
        MessageBox(_AppWindow,
                   lpcwstr('Unable to instantiate MMDeviceEnumerator'),
                   lpcwstr('Get SessionManager Error'),
                   MB_OK);

      if Succeeded(hr) then
        begin
          hr := endpoint.Activate(IID_IAudioSessionManager2,
                                  CLSCTX_INPROC_SERVER,
                                  Nil,
                                  Pointer(_SessionManager2));
          if Failed(hr) then
            MessageBox(_AppWindow,
                       lpcwstr('Unable to Activate session manager'),
                       lpcwstr('Get SessionManager Error'),
                       MB_OK);
        end
      else
        MessageBox(_AppWindow,
                   lpcwstr('Unable to get default endpoint'),
                   lpcwstr('Get SessionManager Error'),
                   MB_OK);
    end;
  Result := hr;
end;


//
//  Utility function to retrieve the session control interface for the current audio session.
//
//  We assume that DirectShow uses the Nil session GUID and doesn't specify any session specific flags.
//
function TCMediaPlayer.GetSessionControl2(): HResult;
var
  hr: HResult;
  sessionControl: IAudioSessionControl;

begin
  hr := S_OK;
  if not Assigned(_SessionControl2) then
    begin
      hr := GetSessionManager2();
      if Succeeded(hr) then
        begin
          hr := _SessionManager2.GetAudioSessionControl(@GUID_NULL, // or Nil
                                                        0,
                                                        sessionControl);
          if Succeeded(hr) then
            begin
              hr := sessionControl.QueryInterface(IID_IAudioSessionControl2,
                                                  _SessionControl2);
              if Failed(hr) then
                MessageBox(_AppWindow,
                           lpcwstr('Unable to QI for SessionControl2'),
                           lpcwstr('Get SessionControl Error'),
                           MB_OK);
            end
          else
            MessageBox(_AppWindow,
                       lpcwstr('Unable to get Session Control'),
                       lpcwstr('Get SessionControl Error'),
                       MB_OK);
        end;
    end;
  Result := hr;
end;


//
//  Utility function to retrieve the simple volume control interface for the current audio session.
//
//  We assume that DirectShow uses the NULL session GUID and doesn't specify any session specific flags.
//
function TCMediaPlayer.GetSimpleVolume(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  if not Assigned(_SimpleVolume) then
    begin
      hr := GetSessionManager2();
      if Succeeded(hr) then
        begin
          hr := _SessionManager2.GetSimpleAudioVolume(Nil,
                                                      0,
                                                      _SimpleVolume);
          if Failed(hr) then
            MessageBox(_AppWindow,
                       lpcwstr('Unable to get Simple Volume'),
                       lpcwstr('Get Simple Volume Error'),
                       MB_OK);
        end;
    end;
  Result := hr;
end;


//
//  Utility function to retrieve the Session ID for the current audio session.
//
function TCMediaPlayer.GetCurrentSessionId(var SessionId: PWideChar): HResult;
var
  hr: HResult;

begin
  hr := GetSessionControl2();
  if Succeeded(hr) then
    begin
      hr := _SessionControl2.GetSessionInstanceIdentifier(SessionId);
      if Failed(hr) then
        MessageBox(_AppWindow,
                   lpcwstr('Unable to get the session instance ID'),
                   lpcwstr('Get session instance ID Error'),
                   MB_OK);
    end;
  Result := hr;
end;


end.
