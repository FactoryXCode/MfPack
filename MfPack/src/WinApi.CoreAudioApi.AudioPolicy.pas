// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioPolicy.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
//          The interface and type definitions for base APO functionality.
//          Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: audiopolicy.h
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit WinApi.CoreAudioApi.AudioPolicy;

  {$HPPEMIT '#include "audiopolicy.h"'}

interface

uses
  {WinApi}
  WinApi.PropSys,
  WinApi.WinApiTypes,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


type
  // local
  _tagAudioSessionDisconnectReason        = (
    DisconnectReasonDeviceRemoval         = 0,
    DisconnectReasonServerShutdown        = (DisconnectReasonDeviceRemoval + 1),
    DisconnectReasonFormatChanged         = (DisconnectReasonServerShutdown + 1),
    DisconnectReasonSessionLogoff         = (DisconnectReasonFormatChanged + 1),
    DisconnectReasonSessionDisconnected   = (DisconnectReasonSessionLogoff + 1),
    DisconnectReasonExclusiveModeOverride = (DisconnectReasonSessionDisconnected + 1)
  );
  {$EXTERNALSYM _tagAudioSessionDisconnectReason}
  AudioSessionDisconnectReason = _tagAudioSessionDisconnectReason;


  // Interface IAudioSessionEvents
  // =============================
  //  Application initiated events.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionEvents);'}
  {$EXTERNALSYM IAudioSessionEvents}
  IAudioSessionEvents = interface(IUnknown)
  ['{24918ACC-64B3-37C1-8CA9-74A66E9957A8}']
    function OnDisplayNameChanged(NewDisplayName: LPCWSTR;
                                  const EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Called when the display name of an AudioSession changes.
    //
    // Parameters:
    //
    // newDisplayName - [in]
    //    The new display name for the Audio Session.
    // EventContext - [in]
    //    Context passed to SetDisplayName routine.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    function OnIconPathChanged(NewIconPath: LPCWSTR;
                               const EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Called when the icon path of an AudioSession changes.
    //
    // Parameters:
    //
    // NewIconPath - [in]
    //    The new icon path for the Audio Session.
    // EventContext - [in]
    //    Context passed to SetIconPath routine.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    function OnSimpleVolumeChanged(NewVolume: FLOAT;
                                   NewMute: BOOL;
                                   const EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Called when the simple volume of an AudioSession changes.
    //
    // Parameters:
    //
    // newVolume - [in]
    //    The new volume for the AudioSession.
    // newMute - [in]
    //    The new mute state for the AudioSession.
    // EventContext - [in]
    //    Context passed to SetVolume routine.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    function OnChannelVolumeChanged(ChannelCount: UINT;
                                    NewChannelArray: PFloat;
                                    ChangedChannel: UINT;
                                    const EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Called when the channel volume of an AudioSession changes.
    //
    // Parameters:
    //
    // ChannelCount - [in]
    //    The number of channels in the channel array.
    // NewChannelVolumeArray - [in]
    //    An array containing the new channel volumes.
    // ChangedChannel - [in]
    //    -1 if all channnels were changed, otherwise the channel volume which changed,
    //    0..ChannelCount-1
    // EventContext - [in]
    //    Context passed to SetVolume routine.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure (ignored)
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    function OnGroupingParamChanged(NewGroupingParam: TGUID;
                                    const EventContext: TGUID): HResult; stdcall;
    // Description:
    //      Called when the grouping param of an Audio Session changes.
    //
    // Parameters:
    // NewGroupingParam - [in]
    //    The new gropuing param for the Audio Session.
    // EventContext - [in]
    //    Context passed to SetGroupingParam routine.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    //  System initiated events.
    //==========================

    function OnStateChanged(NewState: AudioSessionState): HResult; stdcall;
    // Description:
    //
    //  Called when the state of an AudioSession changes.
    //
    // Parameters:
    //
    // newState - [in]
    //    The new state for the AudioSession.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //

    function OnSessionDisconnected(DisconnectReason: AudioSessionDisconnectReason): HResult; stdcall;
    // Description:
    //      Called when the audio session has been disconnected.
    //
    // Parameters:
    // DisconnectReason - [in]
    //    The reason for the disconnection.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    //  Please note: The caller of this function ignores all return
    //  codes from this method.
    //
  end;
  // IAudioSessionEvents
  IID_IAudioSessionEvents = IAudioSessionEvents;
  {$EXTERNALSYM IID_IAudioSessionEvents}


  // Interface IAudioSessionControl >= Vista
  // ==============================
  // Client interface that allows control over a AudioSession.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionControl);'}
  {$EXTERNALSYM IAudioSessionControl}
  IAudioSessionControl = interface(IUnknown)
  ['{F4B1A599-7266-4319-A8CA-E70ACB11E8CD}']
    function GetState(out pRetVal: UINT): HResult; stdcall;
    // Description:
    //
    //  Retrieves the current AudioSession state.
    //
    // Parameters:
    //
    //  pRetVal - [out]
    //    The current AudioSession state, either
    //    AudioSessionStateActive or AudioSessionStateInactive
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    // Remarks:
    //
    //  If an AudioSession has audio streams currently opened on the AudioSession,
    //  the AudioSession is considered active, otherwise it is inactive.
    //

    function GetDisplayName(out pRetVal: LPWSTR): HResult; stdcall; // pRetVal must be freed by CoTaskMemFree

    function SetDisplayName(Val: LPCWSTR;
                            EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Sets or retrieves the current display name of the AudioSession.
    //
    // Parameters:
    //
    //  Value - [in]
    //    A string containing the current display name for the AudioSession.
    //
    //  pRetVal - [out]
    //    A string containing the current display name for the
    //    AudioSession. Caller should free this with CoTaskMemFree.
    //
    //    The DisplayName may be in the form of a shell resource spcification, in which case
    //    the volume UI will extract the display name for the current language from
    //    the specified path.
    //
    //    The shell resource specification is of the form:
    //     <path(including %environmentvariable%)>\<dll name>,<resource ID>
    //    So:"%windir%\system32\shell32.dll,-240" is an example.
    // EventContext - [in]
    //    Context passed to OnDisplayNameChanged routine, GUID_NULL if NULL.
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    // Remarks:
    //      The application hosting the session controls the display name, if the application has NOT set the display name,
    //      then this will return an empty string ("").
    //

    function GetIconPath(out pRetVal: LPWSTR): HResult; stdcall;    // pRetVal must be freed by CoTaskMemFree

    function SetIconPath(Val: LPCWSTR;
                         EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Sets or retrieves an icon resource associated with the session.
    //
    // Parameters:
    //
    // Value - [in]
    //    A string containing a shell resource specification used to retrieve
    //    the icon for the Audio Session.  The volume UI will pass this string
    //    to the ExtractIcon(Ex) API to extract the icon that is displayed
    //
    //    The shell resource specification is of the form:
    //    <path(including %environmentvariable%)>\<dll name>,<resource ID>
    //    So:"%windir%\system32\shell32.dll,-240" is an example.
    //
    // pRetVal - [out]
    //    A string containing the shell resource specification for the
    //    Audio Session. Caller should free this with CoTaskMemFree.
    //
    // EventContext - [in]
    //    Context passed to OnIconPathChanged routine.
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    // Remarks:
    //      The application hosting the session controls the icon path, if the application has NOT set the icon path,
    //      then this will return an empty string ("").
    //

    function GetGroupingParam(out pRetVal: TGUID): HResult; stdcall;

    function SetGroupingParam(const OverrideValue: TGUID;
                              const EventContext: TGUID): HResult; stdcall;
    // Description:
    //
    //  Gets or sets the current grouping param of the Audio Session.
    //
    // Parameters:
    //
    // GroupingParam - [in]
    //    The GUID grouping param for the current Audio Session.
    // pRetVal - [out]
    //    The GUID grouping param for the current Audio Session.
    // EventContext - [in]
    //    Context passed to OnGroupingParamChanged routine.
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    // Remarks:
    //  Normally the volume control application (sndvol.exe) will launch a separate slider
    //  for each audio session.  If an application wishes to override this behavior, it can
    //  set the session grouping param to an application defined GUID.  When the volume
    //  control application sees two sessions with the same session control, it will only
    //  display a single slider for those applications.
    //
    //  As an example, normally, if you launch two copies of sndrec32.exe, then you will see
    //  two volume control sliders in the windows volume control application. If sndrec32.exe
    //  sets the grouping param, then the volume control will only show one slider, even though
    //  there are two sessions.
    //
    //  Please note that there are still two sessions, each with its own volume control, and those
    //  volume controls may not have the same value. If this is the case, then it is the responsibility
    //  of the application to ensure that the volume control on each session has the same value.
    //

    function RegisterAudioSessionNotification(NewNotifications: IAudioSessionEvents): HResult; stdcall;
    // Description:
    //
    //  Add a notification callback to the list of AudioSession notification
    //  callbacks.
    //
    // Parameters:
    //
    // NewNotifications - [in]
    //    An object implementing the IAudioSessionEvents
    //                  interface.
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function UnregisterAudioSessionNotification(NewNotifications: IAudioSessionEvents): HResult; stdcall;
    // Description:
    //
    // Remove a notification callback to the list of AudioSession notification
    // callbacks.
    //
    // Parameters:
    //
    // NewNotifications - [in]
    //    An object implementing the IAudioSessionEvents
    //                  interface.
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    // Remarks:
    //      Please note: This function is a "finalizer". As such,
    //      assuming that the NewNotification parameter has been
    //      previously registered for notification, this function has
    //      no valid failure modes.
    //
  end;
  IID_IAudioSessionControl = IAudioSessionControl;
  {$EXTERNALSYM IID_IAudioSessionControl}



  // Interface IAudioSessionControl2 >= Windows 7
  // ===============================
  // AudioSession Control Extended Interface
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionControl2);'}
  {$EXTERNALSYM IAudioSessionControl2}
  IAudioSessionControl2 = interface(IAudioSessionControl)
  ['{bfb7ff88-7239-4fc9-8fa2-07c950be9c6d}']
    function GetSessionIdentifier(out pRetVal: LPWSTR): Hresult; stdcall;
    // Description:
    //
    // Retrieves the AudioSession ID.
    //
    // Parameters:
    //
    // pRetVal - [out]
    //    A string containing the ID of the AudioSession.
    //                  Freed with CoTaskMemFree
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    // Remarks:
    //  Each AudioSession has a unique identifier string associated with it.
    //  This ID string represents the identifier of the AudioSession. It is NOT unique across all instances - if there are two
    //  instances of the application playing, they will both have the same session identifier.
    //

    function GetSessionInstanceIdentifier(out pRetVal: LPWSTR): Hresult; stdcall;
    // Description:
    //
    // Retrieves the AudioSession instance ID.
    //
    // Parameters:
    //
    // pRetVal - [out]
    //    A string containing the instance ID of the AudioSession.
    //                  Freed with CoTaskMemFree
    //
    // Return values:
    //
    //      S_OK        Success
    //      FAILURECODE Failure
    //
    // Remarks:
    //
    //  Each AudioSession has a unique identifier string associated with it.
    //  This ID string represents the particular instance of the AudioSession.
    //
    //  The session instance identifier is unique across all instances, if there are two instances of the application playing,
    //  they will have different instance identifiers.
    //

    function GetProcessId(out pRetVal: DWord): Hresult; stdcall;
    // Description:
    //
    //  Retrieves the AudioSession Process ID.
    //
    // Parameters:
    //
    //  pRetVal - [out]
    //    A string containing the ID of the AudioSession.
    //    Freed with CoTaskMemFree
    //
    // Return values:
    //
    //      S_OK: Success
    //      AUDCLNT_E_NO_SINGLE_PROCESS: The session spans more than one process.
    //                                   In this case, pRetVal receives the initial identifier of the process that created the
    //                                   session. To use this value, include the following definition:
    //                                   const AUDCLNT_S_NO_SINGLE_PROCESS = $8890000D;
    //                                   NOTE: This constant is defined in WinApi.CoreAudioApi.AudioClient.pas and is NOT present in any headerfile.
    //                                   SEE: https://docs.microsoft.com/en-us/windows/win32/api/audiopolicy/nf-audiopolicy-iaudiosessioncontrol2-getprocessid
    //      FAILURECODE: Failure
    //
    // Remarks:
    //
    //

    function IsSystemSoundsSession(): Hresult; stdcall;
    // Description:
    //
    //  Determines if the specified session is the system sounds session
    //
    // Parameters:
    //
    //  None
    //
    // Return values:
    //
    //      S_OK        Success - The session is the system sounds session.
    //      S_FALSE     Success - The session is NOT the system sounds session.
    //      FAILURECODE Failure
    //

    function SetDuckingPreference(const optOut: BOOL): Hresult; stdcall; //A BOOL variable that enables or disables system auto-ducking.
    // Description:
    //
    // Allows client to set its ducking preference
    //
    // Parameters:
    //      optOut - [in]
    //    Indicates whether caller wants to opt out of
    //    system auto-ducking
    //
    // Remarks
    //
    //     An application should call this method in advance of receiving
    //     a ducking notification (generally at stream create time).  This
    //     method can be called dynamically (i.e. over and over) as its
    //     desire to opt in or opt out of ducking changes.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
  end;
  // IAudioSessionControl2
  IID_IAudioSessionControl2 = IAudioSessionControl2;
  {$EXTERNALSYM IID_IAudioSessionControl2}


  // Interface IAudioSessionManager
  // ==============================
  // Notification on session changes.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionManager);'}
  {$EXTERNALSYM IAudioSessionManager}
  IAudioSessionManager = interface(IUnknown)
  ['{BFA971F1-4D5E-40BB-935E-967039BFBEE4}']
    function GetAudioSessionControl(AudioSessionGuid: LPCGUID;  // may be @GUID_NULL or Nil when nothing is specified.
                                    StreamFlag: UINT;
                                    out SessionControl: IAudioSessionControl): HResult; stdcall;
    // Description:
    //
    // Return an audio session control for the current process.
    //
    // Parameters:
    // AudioSessionGuid - [in]
    //    Session ID for the session.
    // StreamFlags    - [in]
    //    Combination of AUDCLNT_STREAMFLAGS_XYZ flags, defined MfPack.AudioSessionTypes
    // SessionControl   - [out]
    //    Returns a pointer to an audio session control for the current process.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function GetSimpleAudioVolume(AudioSessionGuid: LPCGUID;  // may be @GUID_NULL or Nil when nothing is specified.
                                  StreamFlag: UINT;
                                  out AudioVolume: ISimpleAudioVolume): HResult; stdcall;
    // Description:
    //
    // Return an audio volume control for the current process.
    //
    // Parameters:
    // AudioSessionGuid - [in]
    //    Session ID for the session.
    // StreamFlags - [in]
    //    Combination of AUDCLNT_STREAMFLAGS_XYZ flags, defined MfPack.AudioSessionTypes
    // AudioVolume - [out]
    //    Returns a pointer to an audio volume control for a session in the current process.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

  end;
  IID_IAudioSessionManager = IAudioSessionManager;
  {$EXTERNALSYM IID_IAudioSessionManager}



  // Interface IAudioVolumeDuckNotification >= Windows 7
  // ======================================
  // Notification on session changes.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioVolumeDuckNotification);'}
  {$EXTERNALSYM IAudioVolumeDuckNotification}
  IAudioVolumeDuckNotification = interface(IUnknown)
  ['{C3B284D4-6D39-4359-B3CF-B56DDB3BB39C}']
    function OnVolumeDuckNotification(sessionID: LPCWSTR;
                                      countCommunicationSessions: UINT32): HResult; stdcall;
    // Description:
    //
    // Notification of a pending system auto-duck
    //
    // Parameters:
    // sessionID - [in]
    //    Session instance ID of the communications session
    //                        creating the auto-ducking event.
    // countCommunicationsSessions - [in]
    //    the number of active communications sessions (first
    //    session is 1).
    //
    // Remarks
    //      If applications wish to opt out of ducking, they must call
    //      IAudioVolumeDuck.SetDuckingPreference()
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function OnVolumeUnduckNotification(sessionID: LPCWSTR): HResult; stdcall;
    // Description:
    //
    // Notification of a pending system auto-unduck
    //
    // Parameters:
    // sessionID - [in]
    //    Session instance ID of the communications session
    //    that is terminating.
    // countCommunicationsSessions - [in]
    //    the number of active communications sessions (last
    //    session is 1).
    //
    // Remarks
    //      This is simply a notification that
    //      the communications stream that initiated the ducking is terminating.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //
  end;
  IID_IAudioVolumeDuckNotification = IAudioVolumeDuckNotification;
  {$EXTERNALSYM IID_IAudioVolumeDuckNotification}


  // Interface IAudioSessionNotification
  // ===================================
  // Audio Session Notification Interface
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionNotification);'}
  {$EXTERNALSYM IAudioSessionNotification}
  IAudioSessionNotification = interface(IUnknown)
  ['{641DD20B-4D41-49CC-ABA3-174B9477BB08}']

    function OnSessionCreated(NewSession: IAudioSessionControl): HResult; stdcall;

  end;
  IID_IAudioSessionNotification = IAudioSessionNotification;
  {$EXTERNALSYM IID_IAudioSessionNotification}


  // Interface IAudioSessionEnumerator
  // =================================
  // Audio Session Enumerator Interface
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionEnumerator);'}
  {$EXTERNALSYM IAudioSessionEnumerator}
  IAudioSessionEnumerator = interface(IUnknown)
  ['{E2F5BB11-0570-40CA-ACDD-3AA01277DEE8}']

    function GetCount(out SessionCount: integer): HResult; stdcall;

    function GetSession(SessionCount: integer;
                        out Session: IAudioSessionControl): HResult; stdcall;

  end;
  IID_IAudioSessionEnumerator = IAudioSessionEnumerator;
  {$EXTERNALSYM IID_IAudioSessionEnumerator}


  // Interface IAudioSessionManager2 >= Windows 7
  // ===============================
  // Manage interface for all submixes - Supports
  // enumeration and notification of submixes.
  // Also provides support for ducking notifications.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSessionManager2);'}
  {$EXTERNALSYM IAudioSessionManager2}
  IAudioSessionManager2 = interface(IAudioSessionManager)
  ['{77AA99A0-1BD6-484F-8BC7-2C654C9A9B6F}']
    function GetSessionEnumerator(out SessionEnum: IAudioSessionEnumerator): HResult; stdcall;
    // Description:
    //
    // Return the audio session enumerator.
    //
    // Parameters:
    // SessionList - [out]
    //    An IAudioSessionEnumerator interface that
    //                          can enumerate the audio sessions.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function RegisterSessionNotification(SessionNotification: IAudioSessionNotification): HResult; stdcall;
    // Description:
    //
    // Add the specified process ID as a target for session
    // notifications.
    //
    // Parameters:
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function UnregisterSessionNotification(SessionNotification: IAudioSessionNotification): HResult; stdcall;
    // Description:
    //
    // Remove the specified process ID as a target for session
    // notifications.
    //
    // Parameters:
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function RegisterDuckNotification(sessionID: LPCWSTR;
                                      duckNotification: IAudioVolumeDuckNotification): HResult; stdcall;
    // Description:
    //
    // Register for notification of a pending system auto-duck
    //
    // Parameters:
    // sessionID - [in]
    //     A filter.  Applications like media players
    //                that are interested in their sessions will
    //                pass their own session instance ID.  Other applications
    //                that want to see all the ducking notifications
    //                can pass NULL.
    // duckNotification - [in]
    //    Object which implements the
    //    IAudioVolumeDuckNotification interface
    //    which will receive new notifications.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
    //

    function UnregisterDuckNotification(duckNotification: IAudioVolumeDuckNotification): HResult; stdcall;
    // Description:
    //
    // Unregisters for notification of a pending system auto-duck
    //
    // Parameters:
    // duckNotification - [in]
    //    Object which implements the
    //    IAudioVolumeDuckNotification interface
    //    previously registered, which will be
    //    no longer receive notifications.  Please
    //    note that after this routine returns, no
    //    all pending notifications will have been
    //    processed.
    //
    // Return values:
    //      S_OK        Success
    //      FAILURECODE Failure
  end;
  IID_IAudioSessionManager2 = IAudioSessionManager2;
  {$EXTERNALSYM IID_IAudioSessionManager2}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
