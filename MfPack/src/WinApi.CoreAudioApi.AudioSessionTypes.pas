// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Audio Session Types
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioSessionTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Type definitions used by the audio session manager RPC/COM interfaces.
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
// Source: audiosessiontypes.h
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
unit WinApi.CoreAudioApi.AudioSessionTypes;

  {$HPPEMIT '#include "audiosessiontypes.h"'}

interface

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

  //-------------------------------------------------------------------------
  // Description: AudioClient share mode
  //
  //     AUDCLNT_SHAREMODE_SHARED -    The device will be opened in shared mode and use the
  //                                   WAS format.
  //     AUDCLNT_SHAREMODE_EXCLUSIVE - The device will be opened in exclusive mode and use the
  //                                   application specified format.
  //

type
  PAUDCLNT_SHAREMODE = ^AUDCLNT_SHAREMODE;
  _AUDCLNT_SHAREMODE = (
    AUDCLNT_SHAREMODE_SHARED,
    AUDCLNT_SHAREMODE_EXCLUSIVE
  );
  {$EXTERNALSYM _AUDCLNT_SHAREMODE}
  AUDCLNT_SHAREMODE = _AUDCLNT_SHAREMODE;
  {$EXTERNALSYM AUDCLNT_SHAREMODE}

  // Description: Audio stream categories
  //=====================================
  // ForegroundOnlyMedia     - (deprecated for Win10) Music, Streaming audio
  // BackgroundCapableMedia  - (deprecated for Win10) Video with audio
  // Communications          - VOIP, chat, phone call
  // Alerts                  - Alarm, Ring tones
  // SoundEffects            - Sound effects, clicks, dings
  // GameEffects             - Game sound effects
  // GameMedia               - Background audio for games
  // GameChat                - In game player chat
  // Speech                  - Speech recognition
  // Media                   - Music, Streaming audio
  // Movie                   - Video with audio
  // Other                   - All other streams (default)
  //
  // IMPORTANT NOTES:
  // The values AudioCategory_ForegroundOnlyMedia and AudioCategory_BackgroundCapableMedia are deprecated.
  // For Windows Store apps, these values will continue to function the same when running on
  // Windows 10 as they did on Windows 8.1.
  // Attempting to use these values in a Universal Windows Platform (UWP) app,
  // will result in compilation errors and an exception at runtime.
  // Using these values in a Windows desktop application built with the Windows 10 SDK the will result in a compilation error.

  PAUDIO_STREAM_CATEGORY = ^AUDIO_STREAM_CATEGORY;
  _AUDIO_STREAM_CATEGORY = (
    AudioCategory_Other                   = 0,
    AudioCategory_ForegroundOnlyMedia     = 1,
//#if NTDDI_VERSION < NTDDI_WINTHRESHOLD
    AudioCategory_BackgroundCapableMedia  = 2,
//#endif
    AudioCategory_Communications          = 3,
    AudioCategory_Alerts                  = 4,
    AudioCategory_SoundEffects            = 5,
    AudioCategory_GameEffects             = 6,
    AudioCategory_GameMedia               = 7,
    AudioCategory_GameChat                = 8,
    AudioCategory_Speech                  = 9,
    AudioCategory_Movie                   = 10,
    AudioCategory_Media                   = 11
  );
  {$EXTERNALSYM _AUDIO_STREAM_CATEGORY}
  AUDIO_STREAM_CATEGORY = _AUDIO_STREAM_CATEGORY;
  {$EXTERNALSYM AUDIO_STREAM_CATEGORY}


  // Description: AudioClient stream flags
  //======================================
  // Can be a combination of AUDCLNT_STREAMFLAGS and AUDCLNT_SYSFXFLAGS:
  //
  // AUDCLNT_STREAMFLAGS (this group of flags uses the high word,
  // w/exception of high-bit which is reserved, 0x7FFF0000):
  //
  //
  //     AUDCLNT_STREAMFLAGS_CROSSPROCESS -             Audio policy control for this stream will be shared with
  //                                                    with other process sessions that use the same audio session
  //                                                    GUID.
  //
  //     AUDCLNT_STREAMFLAGS_LOOPBACK -                 Initializes a renderer endpoint for a loopback audio application.
  //                                                    In this mode, a capture stream will be opened on the specified
  //                                                    renderer endpoint. Shared mode and a renderer endpoint is required.
  //                                                    Otherwise the IAudioClient.Initialize call will fail. If the
  //                                                    initialize is successful, a capture stream will be available
  //                                                    from the IAudioClient object.
  //
  //     AUDCLNT_STREAMFLAGS_EVENTCALLBACK -            An exclusive mode client will supply an event handle that will be
  //                                                    signaled when an IRP completes (or a waveRT buffer completes) telling
  //                                                    it to fill the next buffer
  //
  //     AUDCLNT_STREAMFLAGS_NOPERSIST -                Session state will not be persisted
  //
  //     AUDCLNT_STREAMFLAGS_RATEADJUST -               The sample rate of the stream is adjusted to a rate specified by an application.
  //
  //     AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY -      When used with AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM, a sample rate
  //                                                    converter with better quality than the default conversion but with a
  //                                                    higher performance cost is used. This should be used if the audio is
  //                                                    ultimately intended to be heard by humans as opposed to other
  //                                                    scenarios such as pumping silence or populating a meter.
  //
  //     AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM -           A channel matrixer and a sample rate converter are inserted as necessary
  //                                                    to convert between the uncompressed format supplied to
  //                                                    IAudioClient.Initialize and the audio engine mix format.
  //
  //     AUDCLNT_STREAMFLAGS_KEYWORDDETECTOR -          Initializes a capture endpoint for keyword detector streaming.
  //
  //
  //     AUDCLNT_SESSIONFLAGS_EXPIREWHENUNOWNED -       Session expires when there are no streams and no owning
  //                                                    session controls.
  //
  //     AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE -            Don't show volume control in the Volume Mixer.
  //
  //     AUDCLNT_SESSIONFLAGS_DISPLAY_HIDEWHENEXPIRED - Don't show volume control in the Volume Mixer after the
  //                                                    session expires.
  //
  // AUDCLNT_SYSFXFLAGS (these flags use low word $0000FFFF):
  //
  //     none defined currently
  //



const

  AUDCLNT_STREAMFLAGS_CROSSPROCESS              = $00010000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_CROSSPROCESS}
  AUDCLNT_STREAMFLAGS_LOOPBACK                  = $00020000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_LOOPBACK}
  AUDCLNT_STREAMFLAGS_EVENTCALLBACK             = $00040000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_EVENTCALLBACK}
  AUDCLNT_STREAMFLAGS_NOPERSIST                 = $00080000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_NOPERSIST}
  AUDCLNT_STREAMFLAGS_RATEADJUST                = $00100000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_RATEADJUST}
  AUDCLNT_STREAMFLAGS_PREVENT_LOOPBACK_CAPTURE  = $01000000; // deprecated: Removed since version 1903
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_PREVENT_LOOPBACK_CAPTURE}
  AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY       = $08000000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY}
  AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM            = $80000000;
  {$EXTERNALSYM AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM}
  AUDCLNT_SESSIONFLAGS_EXPIREWHENUNOWNED        = $10000000;
  {$EXTERNALSYM AUDCLNT_SESSIONFLAGS_EXPIREWHENUNOWNED}
  AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE             = $20000000;
  {$EXTERNALSYM AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE}
  AUDCLNT_SESSIONFLAGS_DISPLAY_HIDEWHENEXPIRED  = $40000000;
  {$EXTERNALSYM AUDCLNT_SESSIONFLAGS_DISPLAY_HIDEWHENEXPIRED}

  //-------------------------------------------------------------------------
  // Description: AudioSession State.
  //
  //      AudioSessionStateInactive - The session has no active audio streams.
  //      AudioSessionStateActive   - The session has active audio streams.
  //      AudioSessionStateExpired  - The session is dormant.
type

  PAudioSessionState = ^AudioSessionState;
  _AudioSessionState          = (
    AudioSessionStateInactive = 0,
    AudioSessionStateActive   = 1,
    AudioSessionStateExpired  = 2
  );
  {$EXTERNALSYM _AudioSessionState}
  AudioSessionState = _AudioSessionState;
  {$EXTERNALSYM AudioSessionState}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
