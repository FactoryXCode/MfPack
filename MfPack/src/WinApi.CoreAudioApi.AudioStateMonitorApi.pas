// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Audio Session Types
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioStateMonitorApi.pas
// Kind: Pascal / Delphi unit
// Release date: 28-09-2021
// Language: ENU
//
// Revision Version: 3.1.3
// Description: Provides methods that allow desktop apps and games to determine
//              the time their audio streams' sound levels are modified by the system.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: audiostatemonitorapi.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
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
unit WinApi.CoreAudioApi.AudioStateMonitorApi;

  {$HPPEMIT '#include "audiostatemonitorapi.h"'}

interface

uses
  WinApi.WinApiTypes,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.MMDeviceApi;


// Desktop and Games Family


type

  IAudioStateMonitor = interface;

  PAudioStateMonitorCallback = ^AudioStateMonitorCallback;
  AudioStateMonitorCallback = Reference to procedure({In} audioStateMonitor: IAudioStateMonitor;
                                                     {In_opt} context: Pointer);
  {$EXTERNALSYM AudioStateMonitorCallback}


  AudioStateMonitorRegistrationHandle = Int64;
  {$EXTERNALSYM AudioStateMonitorRegistrationHandle}


  AudioStateMonitorSoundLevel = (
                                 Muted = 0, // Audio is muted
                                 Low,       // Audio is ducked
                                 Full       // Audio is unchanged
                                );
  {$EXTERNALSYM AudioStateMonitorSoundLevel}


  // Interface IAudioStateMonitor
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUnknown);'}
  PIAudioStateMonitor = ^IAudioStateMonitor;
  {$EXTERNALSYM IAudioStateMonitor}
  IAudioStateMonitor = interface(IUnknown)
  ['{63BD8738-E30D-4C77-BF5C-834E87C657E2}']
    function RegisterCallback({In} callback: PAudioStateMonitorCallback;
                              {In_opt} context: Pointer;
                              out registration: AudioStateMonitorRegistrationHandle): HResult; stdcall;
     // Registers a new callback with the AudioStateMonitor.

    procedure UnregisterCallback({In} registration: AudioStateMonitorRegistrationHandle); stdcall;
    // Unregisters an existing callback with the AudioStateMonitor
    // This method will block if any callbacks are in progress, until the callbacks have completed.
    // This method may be called from within the callback (it will not block in this case)

    function GetSoundLevel(): AudioStateMonitorSoundLevel; stdcall;
    // Retrieves the current sound level for the AudioStateMonitor

  end;
  IID_IAudioStateMonitor = IAudioStateMonitor;
  {$EXTERNALSYM IID_IAudioStateMonitor}


  //**************************************************************************
  //*
  //* Functions that create and return an AudioStateMonitor instance.
  //*
  //* There are four variants each for capture and render streams, to determine
  //* whether a process can
  //*  - capture or render any audio of any category
  //*  - capture or render audio of a specific category
  //*  - capture or render audio of a specific category, to a specific endpoint.
  //*    The endpoint may be specified using the MMDevice id (obtained using IMMDevice::GetId()), or
  //*    by using its SWD id (obtained using Windows.Devices.Enumeration or Windows.Media.Devices.MediaDevice)
  //*  - capture or render audio of a specific category to the default endpoint
  //*    for a given role
  //**************************************************************************

  function CreateRenderAudioStateMonitor({Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateRenderAudioStateMonitorForCategory({In} category: AUDIO_STREAM_CATEGORY;
                                                    {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateRenderAudioStateMonitorForCategoryAndDeviceRole({In} category: AUDIO_STREAM_CATEGORY;
                                                                 {In} role: ERole;
                                                                 {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateRenderAudioStateMonitorForCategoryAndDeviceId({In} category: AUDIO_STREAM_CATEGORY;
                                                               {In} deviceId: PCWSTR;
                                                               {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateCaptureAudioStateMonitor(audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateCaptureAudioStateMonitorForCategory({In} category: AUDIO_STREAM_CATEGORY;
                                                     {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateCaptureAudioStateMonitorForCategoryAndDeviceRole({In} category: AUDIO_STREAM_CATEGORY;
                                                                  {In} role: ERole;
                                                                  {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;

  function CreateCaptureAudioStateMonitorForCategoryAndDeviceId({In} category: AUDIO_STREAM_CATEGORY;
                                                                {In} deviceId: PCWSTR;
                                                                {Outptr} audioStateMonitor: PIAudioStateMonitor): HResult; stdcall;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

{$WARN SYMBOL_PLATFORM OFF}
const
  AudioStateMonitorApiLib = '';

  function CreateRenderAudioStateMonitor; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateRenderAudioStateMonitorForCategory; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateRenderAudioStateMonitorForCategoryAndDeviceRole; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateRenderAudioStateMonitorForCategoryAndDeviceId; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateCaptureAudioStateMonitor; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateCaptureAudioStateMonitorForCategory; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateCaptureAudioStateMonitorForCategoryAndDeviceRole; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function CreateCaptureAudioStateMonitorForCategoryAndDeviceId; external AudioStateMonitorApiLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}
  // Implement Additional functions here.

end.
