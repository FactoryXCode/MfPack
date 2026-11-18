// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Audio Session Types
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioStateMonitorApi.pas
// Kind: Pascal / Delphi unit
// Release date: 28-09-2021
// Language: ENU
//
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
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
// Source: audiostatemonitorapi.h
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.CoreAudioApi.AudioStateMonitorApi;

  {$HPPEMIT '#include "audiostatemonitorapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.MMDeviceApi;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

// Desktop and Games Family


type

  IAudioStateMonitor = interface;

  // A procedural type already stores the native function pointer.
  AudioStateMonitorCallback = procedure(audioStateMonitor: IAudioStateMonitor;
                                        context: Pointer); stdcall;
  PAudioStateMonitorCallback = AudioStateMonitorCallback;
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

const AudioStateMonitorApiLib = 'Windows.Media.MediaControl.dll';
type
  TCreateMonitor = function(Monitor: PIAudioStateMonitor): HRESULT; stdcall;
  TCreateMonitorForCategory = function(Category: AUDIO_STREAM_CATEGORY; Monitor: PIAudioStateMonitor): HRESULT; stdcall;
  TCreateMonitorForCategoryAndDeviceRole = function(Category: AUDIO_STREAM_CATEGORY; Role: ERole; Monitor: PIAudioStateMonitor): HRESULT; stdcall;
  TCreateMonitorForCategoryAndDeviceId = function(Category: AUDIO_STREAM_CATEGORY; DeviceId: PCWSTR; Monitor: PIAudioStateMonitor): HRESULT; stdcall;
var
  AudioStateMonitorModule: HMODULE;
  AudioStateMonitorLock: TRTLCriticalSection;

function ResolveAudioStateMonitorProc(const Name: PAnsiChar): Pointer;
var Buffer: array[0..MAX_PATH] of WideChar; Count: UINT; FullPath: UnicodeString;
begin
  Result := nil;
  EnterCriticalSection(AudioStateMonitorLock);
  try
    if AudioStateMonitorModule = 0 then
    begin
      Count := GetSystemDirectoryW(@Buffer[0], Length(Buffer));
      if (Count = 0) or (Count >= UINT(Length(Buffer))) then Exit;
      FullPath := UnicodeString(PWideChar(@Buffer[0])) + '\' + AudioStateMonitorApiLib;
      AudioStateMonitorModule := LoadLibraryW(PWideChar(FullPath));
    end;
    if AudioStateMonitorModule <> 0 then
      Result := GetProcAddress(AudioStateMonitorModule, Name);
  finally LeaveCriticalSection(AudioStateMonitorLock); end;
end;

function CreateRenderAudioStateMonitor(audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateRenderAudioStateMonitor');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitor(Proc)(audioStateMonitor);
end;

function CreateRenderAudioStateMonitorForCategory(category: AUDIO_STREAM_CATEGORY; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateRenderAudioStateMonitorForCategory');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategory(Proc)(category, audioStateMonitor);
end;

function CreateRenderAudioStateMonitorForCategoryAndDeviceRole(category: AUDIO_STREAM_CATEGORY; role: ERole; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateRenderAudioStateMonitorForCategoryAndDeviceRole');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategoryAndDeviceRole(Proc)(category, role, audioStateMonitor);
end;

function CreateRenderAudioStateMonitorForCategoryAndDeviceId(category: AUDIO_STREAM_CATEGORY; deviceId: PCWSTR; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateRenderAudioStateMonitorForCategoryAndDeviceId');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategoryAndDeviceId(Proc)(category, deviceId, audioStateMonitor);
end;

function CreateCaptureAudioStateMonitor(audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateCaptureAudioStateMonitor');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitor(Proc)(audioStateMonitor);
end;

function CreateCaptureAudioStateMonitorForCategory(category: AUDIO_STREAM_CATEGORY; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateCaptureAudioStateMonitorForCategory');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategory(Proc)(category, audioStateMonitor);
end;

function CreateCaptureAudioStateMonitorForCategoryAndDeviceRole(category: AUDIO_STREAM_CATEGORY; role: ERole; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateCaptureAudioStateMonitorForCategoryAndDeviceRole');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategoryAndDeviceRole(Proc)(category, role, audioStateMonitor);
end;

function CreateCaptureAudioStateMonitorForCategoryAndDeviceId(category: AUDIO_STREAM_CATEGORY; deviceId: PCWSTR; audioStateMonitor: PIAudioStateMonitor): HRESULT; stdcall;
var Proc: Pointer;
begin
  if audioStateMonitor = nil then begin Result := HRESULT($80004003); Exit; end;
  Pointer(audioStateMonitor^) := nil;
  Proc := ResolveAudioStateMonitorProc('CreateCaptureAudioStateMonitorForCategoryAndDeviceId');
  if Proc = nil then begin Result := HRESULT($80004001); Exit; end;
  Result := TCreateMonitorForCategoryAndDeviceId(Proc)(category, deviceId, audioStateMonitor);
end;

initialization
  InitializeCriticalSection(AudioStateMonitorLock);
finalization
  // Retain the module until process exit: native monitor objects/callbacks may outlive this unit.
  DeleteCriticalSection(AudioStateMonitorLock);
end.
