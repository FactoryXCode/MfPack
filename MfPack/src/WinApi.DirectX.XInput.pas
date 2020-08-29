// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.XInput.pas
// Kind: Pascal / Delphi Unit
// Release date: 14-01-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This module defines Xbox 360 Common Controller APIs
//              and constants for the Windows platform.
//              Windows Vista or later.
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
// Remarks: Requires Windows Vista or later.
// 
//          Using packed records is not a recommended practice,
//          because it can prevent compatibility with other languages or
//          platforms, it slows data access, and in the case of a character array,
//          it affects type compatibility.
//          For more information, see Memory management and Implicit Packing of
//          Fields with a Common Type Specification.
//
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
// Source: XInput.h
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
unit WinApi.DirectX.XInput;

  {$HPPEMIT '#include "XInput.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  // Current name of the DLL shipped in the same SDK as this header.
  // The name reflects the current version

{>= WIN8}
  XINPUT_DLL_A                        = 'xinput1_4.dll';
  {$EXTERNALSYM XINPUT_DLL_A}
  XINPUT_DLL_W                        = 'xinput1_4.dll';
  {$EXTERNALSYM XINPUT_DLL_W}
{ELSE}
  //XINPUT_DLL_A                        = 'xinput9_1_0.dll';
  //XINPUT_DLL_W                        = 'xinput9_1_0.dll';
{ENDIF}

{$IFDEF UNICODE}
  XINPUT_DLL                          = XINPUT_DLL_W;
{$ELSE}
  XINPUT_DLL                          = XINPUT_DLL_A;
{$ENDIF}

  //
  // Device types available in XINPUT_CAPABILITIES
  //
  XINPUT_DEVTYPE_GAMEPAD              = $01;
  {$EXTERNALSYM XINPUT_DEVTYPE_GAMEPAD}

  //
  // Device subtypes available in XINPUT_CAPABILITIES
  //

  XINPUT_DEVSUBTYPE_GAMEPAD           = $01;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_GAMEPAD}

{>=WIN8}

  XINPUT_DEVSUBTYPE_UNKNOWN           = $00;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_UNKNOWN}
  XINPUT_DEVSUBTYPE_WHEEL             = $02;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_WHEEL}
  XINPUT_DEVSUBTYPE_ARCADE_STICK      = $03;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_ARCADE_STICK}
  XINPUT_DEVSUBTYPE_FLIGHT_STICK      = $04;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_FLIGHT_STICK}
  XINPUT_DEVSUBTYPE_DANCE_PAD         = $05;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_DANCE_PAD}
  XINPUT_DEVSUBTYPE_GUITAR            = $06;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_GUITAR}
  XINPUT_DEVSUBTYPE_GUITAR_ALTERNATE  = $07;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_GUITAR_ALTERNATE}
  XINPUT_DEVSUBTYPE_DRUM_KIT          = $08;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_DRUM_KIT}
  XINPUT_DEVSUBTYPE_GUITAR_BASS       = $0B;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_GUITAR_BASS}
  XINPUT_DEVSUBTYPE_ARCADE_PAD        = $13;
  {$EXTERNALSYM XINPUT_DEVSUBTYPE_ARCADE_PAD}

{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)

  //
  // Flags for XINPUT_CAPABILITIES
  //

  XINPUT_CAPS_VOICE_SUPPORTED         = $0004;
  {$EXTERNALSYM XINPUT_CAPS_VOICE_SUPPORTED}

{>= _WIN32_WINNT_WIN8}
  XINPUT_CAPS_FFB_SUPPORTED           = $0001;
  {$EXTERNALSYM XINPUT_CAPS_FFB_SUPPORTED}
  XINPUT_CAPS_WIRELESS                = $0002;
  {$EXTERNALSYM XINPUT_CAPS_WIRELESS}
  XINPUT_CAPS_PMD_SUPPORTED           = $0008;
  {$EXTERNALSYM XINPUT_CAPS_PMD_SUPPORTED}
  XINPUT_CAPS_NO_NAVIGATION           = $0010;
  {$EXTERNALSYM XINPUT_CAPS_NO_NAVIGATION}
{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)

  //
  // Constants for gamepad buttons
  //
  XINPUT_GAMEPAD_DPAD_UP              = $0001;
  {$EXTERNALSYM XINPUT_GAMEPAD_DPAD_UP}
  XINPUT_GAMEPAD_DPAD_DOWN            = $0002;
  {$EXTERNALSYM XINPUT_GAMEPAD_DPAD_DOWN}
  XINPUT_GAMEPAD_DPAD_LEFT            = $0004;
  {$EXTERNALSYM XINPUT_GAMEPAD_DPAD_LEFT}
  XINPUT_GAMEPAD_DPAD_RIGHT           = $0008;
  {$EXTERNALSYM XINPUT_GAMEPAD_DPAD_RIGHT}
  XINPUT_GAMEPAD_START                = $0010;
  {$EXTERNALSYM XINPUT_GAMEPAD_START}
  XINPUT_GAMEPAD_BACK                 = $0020;
  {$EXTERNALSYM XINPUT_GAMEPAD_BACK}
  XINPUT_GAMEPAD_LEFT_THUMB           = $0040;
  {$EXTERNALSYM XINPUT_GAMEPAD_LEFT_THUMB}
  XINPUT_GAMEPAD_RIGHT_THUMB          = $0080;
  {$EXTERNALSYM XINPUT_GAMEPAD_RIGHT_THUMB}
  XINPUT_GAMEPAD_LEFT_SHOULDER        = $0100;
  {$EXTERNALSYM XINPUT_GAMEPAD_LEFT_SHOULDER}
  XINPUT_GAMEPAD_RIGHT_SHOULDER       = $0200;
  {$EXTERNALSYM XINPUT_GAMEPAD_RIGHT_SHOULDER}
  XINPUT_GAMEPAD_A                    = $1000;
  {$EXTERNALSYM XINPUT_GAMEPAD_A}
  XINPUT_GAMEPAD_B                    = $2000;
  {$EXTERNALSYM XINPUT_GAMEPAD_B}
  XINPUT_GAMEPAD_X                    = $4000;
  {$EXTERNALSYM XINPUT_GAMEPAD_X}
  XINPUT_GAMEPAD_Y                    = $8000;
  {$EXTERNALSYM XINPUT_GAMEPAD_Y}


  //
  // Gamepad thresholds
  //
  XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE  = 7849;
  {$EXTERNALSYM XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE}
  XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE = 8689;
  {$EXTERNALSYM XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE}
  XINPUT_GAMEPAD_TRIGGER_THRESHOLD    = 30;
  {$EXTERNALSYM XINPUT_GAMEPAD_TRIGGER_THRESHOLD}

  //
  // Flags to pass to XInputGetCapabilities
  //
  XINPUT_FLAG_GAMEPAD                 = $00000001;
  {$EXTERNALSYM XINPUT_FLAG_GAMEPAD}

{>= WIN8}

  //
  // Devices that support batteries
  //
  BATTERY_DEVTYPE_GAMEPAD             = $00;
  {$EXTERNALSYM BATTERY_DEVTYPE_GAMEPAD}
  BATTERY_DEVTYPE_HEADSET             = $01;
  {$EXTERNALSYM BATTERY_DEVTYPE_HEADSET}

  //
  // Flags for battery status level
  //
  BATTERY_TYPE_DISCONNECTED           = $00;  // This device is not connected
  {$EXTERNALSYM BATTERY_TYPE_DISCONNECTED}
  BATTERY_TYPE_WIRED                  = $01;  // Wired device, no battery
  {$EXTERNALSYM BATTERY_TYPE_WIRED}
  BATTERY_TYPE_ALKALINE               = $02;  // Alkaline battery source
  {$EXTERNALSYM BATTERY_TYPE_ALKALINE}
  BATTERY_TYPE_NIMH                   = $03;  // Nickel Metal Hydride battery source
  {$EXTERNALSYM BATTERY_TYPE_NIMH}
  BATTERY_TYPE_UNKNOWN                = $FF;  // Cannot determine the battery type
  {$EXTERNALSYM BATTERY_TYPE_UNKNOWN}

  // These are only valid for wireless, connected devices, with known battery types
  // The amount of use time remaining depends on the type of device.
  BATTERY_LEVEL_EMPTY                 = $00;
  {$EXTERNALSYM BATTERY_LEVEL_EMPTY}
  BATTERY_LEVEL_LOW                   = $01;
  {$EXTERNALSYM BATTERY_LEVEL_LOW}
  BATTERY_LEVEL_MEDIUM                = $02;
  {$EXTERNALSYM BATTERY_LEVEL_MEDIUM}
  BATTERY_LEVEL_FULL                  = $03;
  {$EXTERNALSYM BATTERY_LEVEL_FULL}

{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)

  // User index definitions
  XUSER_MAX_COUNT                     = 4;
  {$EXTERNALSYM XUSER_MAX_COUNT}

  XUSER_INDEX_ANY                     = $000000FF;
  {$EXTERNALSYM XUSER_INDEX_ANY}

{>= WIN8}

  //
  // Codes returned for the gamepad keystroke
  //

  VK_PAD_A                            = $5800;
  {$EXTERNALSYM VK_PAD_A}
  VK_PAD_B                            = $5801;
  {$EXTERNALSYM VK_PAD_B}
  VK_PAD_X                            = $5802;
  {$EXTERNALSYM VK_PAD_X}
  VK_PAD_Y                            = $5803;
  {$EXTERNALSYM VK_PAD_Y}
  VK_PAD_RSHOULDER                    = $5804;
  {$EXTERNALSYM VK_PAD_RSHOULDER}
  VK_PAD_LSHOULDER                    = $5805;
  {$EXTERNALSYM VK_PAD_LSHOULDER}
  VK_PAD_LTRIGGER                     = $5806;
  {$EXTERNALSYM VK_PAD_LTRIGGER}
  VK_PAD_RTRIGGER                     = $5807;
  {$EXTERNALSYM VK_PAD_RTRIGGER}

  VK_PAD_DPAD_UP                      = $5810;
  {$EXTERNALSYM VK_PAD_DPAD_UP}
  VK_PAD_DPAD_DOWN                    = $5811;
  {$EXTERNALSYM VK_PAD_DPAD_DOWN}
  VK_PAD_DPAD_LEFT                    = $5812;
  {$EXTERNALSYM VK_PAD_DPAD_LEFT}
  VK_PAD_DPAD_RIGHT                   = $5813;
  {$EXTERNALSYM VK_PAD_DPAD_RIGHT}
  VK_PAD_START                        = $5814;
  {$EXTERNALSYM VK_PAD_START}
  VK_PAD_BACK                         = $5815;
  {$EXTERNALSYM VK_PAD_BACK}
  VK_PAD_LTHUMB_PRESS                 = $5816;
  {$EXTERNALSYM VK_PAD_LTHUMB_PRESS}
  VK_PAD_RTHUMB_PRESS                 = $5817;
  {$EXTERNALSYM VK_PAD_RTHUMB_PRESS}

  VK_PAD_LTHUMB_UP                    = $5820;
  {$EXTERNALSYM VK_PAD_LTHUMB_UP}
  VK_PAD_LTHUMB_DOWN                  = $5821;
  {$EXTERNALSYM VK_PAD_LTHUMB_DOWN}
  VK_PAD_LTHUMB_RIGHT                 = $5822;
  {$EXTERNALSYM VK_PAD_LTHUMB_RIGHT}
  VK_PAD_LTHUMB_LEFT                  = $5823;
  {$EXTERNALSYM VK_PAD_LTHUMB_LEFT}
  VK_PAD_LTHUMB_UPLEFT                = $5824;
  {$EXTERNALSYM VK_PAD_LTHUMB_UPLEFT}
  VK_PAD_LTHUMB_UPRIGHT               = $5825;
  {$EXTERNALSYM VK_PAD_LTHUMB_UPRIGHT}
  VK_PAD_LTHUMB_DOWNRIGHT             = $5826;
  {$EXTERNALSYM VK_PAD_LTHUMB_DOWNRIGHT}
  VK_PAD_LTHUMB_DOWNLEFT              = $5827;
  {$EXTERNALSYM VK_PAD_LTHUMB_DOWNLEFT}

  VK_PAD_RTHUMB_UP                    = $5830;
  {$EXTERNALSYM VK_PAD_RTHUMB_UP}
  VK_PAD_RTHUMB_DOWN                  = $5831;
  {$EXTERNALSYM VK_PAD_RTHUMB_DOWN}
  VK_PAD_RTHUMB_RIGHT                 = $5832;
  {$EXTERNALSYM VK_PAD_RTHUMB_RIGHT}
  VK_PAD_RTHUMB_LEFT                  = $5833;
  {$EXTERNALSYM VK_PAD_RTHUMB_LEFT}
  VK_PAD_RTHUMB_UPLEFT                = $5834;
  {$EXTERNALSYM VK_PAD_RTHUMB_UPLEFT}
  VK_PAD_RTHUMB_UPRIGHT               = $5835;
  {$EXTERNALSYM VK_PAD_RTHUMB_UPRIGHT}
  VK_PAD_RTHUMB_DOWNRIGHT             = $5836;
  {$EXTERNALSYM VK_PAD_RTHUMB_DOWNRIGHT}
  VK_PAD_RTHUMB_DOWNLEFT              = $5837;
  {$EXTERNALSYM VK_PAD_RTHUMB_DOWNLEFT}

  //
  // Flags used in XINPUT_KEYSTROKE
  //
  XINPUT_KEYSTROKE_KEYDOWN            = $0001;
  {$EXTERNALSYM XINPUT_KEYSTROKE_KEYDOWN}
  XINPUT_KEYSTROKE_KEYUP              = $0002;
  {$EXTERNALSYM XINPUT_KEYSTROKE_KEYUP}
  XINPUT_KEYSTROKE_REPEAT             = $0004;
  {$EXTERNALSYM XINPUT_KEYSTROKE_REPEAT}

{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)



type

  //
  // Structures used by XInput APIs
  //

  PXINPUT_GAMEPAD = ^_XINPUT_GAMEPAD;
   _XINPUT_GAMEPAD = record
    wButtons: WORD;
    bLeftTrigger: Byte;
    bRightTrigger: Byte;
    sThumbLX: SHORT;
    sThumbLY: SHORT;
    sThumbRX: SHORT;
    sThumbRY: SHORT;
  end;
   {$EXTERNALSYM _XINPUT_GAMEPAD}
  XINPUT_GAMEPAD = _XINPUT_GAMEPAD;
  {$EXTERNALSYM XINPUT_GAMEPAD}


  PXINPUT_STATE = ^_XINPUT_STATE;
  _XINPUT_STATE = record
    dwPacketNumber: DWORD;
    Gamepad: XINPUT_GAMEPAD;
  end;
  {$EXTERNALSYM _XINPUT_STATE}
  XINPUT_STATE = _XINPUT_STATE;
  {$EXTERNALSYM XINPUT_STATE}


  PXINPUT_VIBRATION = ^_XINPUT_VIBRATION;
  _XINPUT_VIBRATION = record
    wLeftMotorSpeed: WORD;
    wRightMotorSpeed: WORD;
  end;
  {$EXTERNALSYM _XINPUT_VIBRATION}
  XINPUT_VIBRATION = _XINPUT_VIBRATION;
  {$EXTERNALSYM XINPUT_VIBRATION}


  PXINPUT_CAPABILITIES = ^_XINPUT_CAPABILITIES;
  _XINPUT_CAPABILITIES = record
    _Type: Byte;
    SubType: Byte;
    Flags: WORD;
    Gamepad: XINPUT_GAMEPAD;
    Vibration: XINPUT_VIBRATION;
  end;
  {$EXTERNALSYM _XINPUT_CAPABILITIES}
  XINPUT_CAPABILITIES = _XINPUT_CAPABILITIES;
  {$EXTERNALSYM XINPUT_CAPABILITIES}


{>= WIN8}

  PXINPUT_BATTERY_INFORMATION = ^_XINPUT_BATTERY_INFORMATION;
  _XINPUT_BATTERY_INFORMATION = record
    BatteryType: Byte;
    BatteryLevel: Byte;
  end;
  {$EXTERNALSYM _XINPUT_BATTERY_INFORMATION}
  XINPUT_BATTERY_INFORMATION = _XINPUT_BATTERY_INFORMATION;
  {$EXTERNALSYM XINPUT_BATTERY_INFORMATION}


  PXINPUT_KEYSTROKE = ^_XINPUT_KEYSTROKE;
  _XINPUT_KEYSTROKE = record
    VirtualKey: WORD;
    Unicode: WideChar;
    Flags: WORD;
    UserIndex: Byte;
    HidCode: Byte;
  end;
  {$EXTERNALSYM _XINPUT_KEYSTROKE}
  XINPUT_KEYSTROKE = _XINPUT_KEYSTROKE;
  {$EXTERNALSYM XINPUT_KEYSTROKE}

{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)


  //
  // XInput APIs
  //


  function XInputGetState(dwUserIndex: DWORD;  // Index of the gamer associated with the device
                          pState: XINPUT_STATE): DWORD; stdcall;
  {$EXTERNALSYM XInputGetState}

  function XInputSetState(dwUserIndex: DWORD;  // Index of the gamer associated with the device
                          pVibration: XINPUT_VIBRATION): DWORD; stdcall; // The vibration information to send to the controller
  {$EXTERNALSYM XInputSetState}

  function XInputGetCapabilities(dwUserIndex: DWORD;  // Index of the gamer associated with the device
                                 dwFlags: DWORD;      // Input flags that identify the device type
                                 out pCapabilities: XINPUT_CAPABILITIES): DWORD; stdcall; // Receives the capabilities
  {$EXTERNALSYM XInputGetCapabilities}


  // Sets the reporting state of XInput.
// #if(_WIN32_WINNT >= _WIN32_WINNT_WIN8)
  //procedure XInputEnable({in} enable: BOOL); stdcall;
//#if(_WIN32_WINNT >= _WIN32_WINNT_WIN10)
  procedure XInputEnable({in} enable: BOOL); stdcall; deprecated 'Only valid for Win 8 and 8.1';
  {$EXTERNALSYM XInputEnable}
//#endif

{ENDIF}

  function XInputGetAudioDeviceIds(dwUserIndex: DWORD;               // Index of the gamer associated with the device
                                   out pRenderDeviceId: PWideChar;   // Windows Core Audio device ID string for render (speakers)
                                   var pRenderCount: UINT;           // Size of render device ID string buffer (in wide-chars)
                                   out pCaptureDeviceId: PWideChar;  // Windows Core Audio device ID string for capture (microphone)
                                   var pCaptureCount: UINT): DWORD; stdcall;  // Size of capture device ID string buffer (in wide-chars)
  {$EXTERNALSYM XInputGetAudioDeviceIds}



  function XInputGetBatteryInformation(dwUserIndex: DWORD;           // Index of the gamer associated with the device
                                       devType: Byte;                // Which device on this user index
                                       out pBatteryInformation: XINPUT_BATTERY_INFORMATION): DWORD; stdcall; // Contains the level and types of batteries
  {$EXTERNALSYM XInputGetBatteryInformation}


  function XInputGetKeystroke(dwUserIndex: DWORD;           // Index of the gamer associated with the device
                              dwReserved: DWORD;            // Reserved for future use
                              out pKeystroke: PXINPUT_KEYSTROKE): DWORD; stdcall; // Pointer to an XINPUT_KEYSTROKE structure that receives an input event.
  {$EXTERNALSYM XInputGetKeystroke}



{ENDIF} //(_WIN32_WINNT >= _WIN32_WINNT_WIN8)


{>= WIN8}

  function XInputGetDSoundAudioDeviceGuids(dwUserIndex: DWORD;            // Index of the gamer associated with the device
                                           out pDSoundRenderGuid: TGUID;  // DSound device ID for render (speakers)
                                           out pDSoundCaptureGuid: TGUID): DWORD; stdcall;  // DSound device ID for capture (microphone)
  {$EXTERNALSYM XInputGetDSoundAudioDeviceGuids}

{ENDIF} //(_WIN32_WINNT < _WIN32_WINNT_WIN8)


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


{$WARN SYMBOL_PLATFORM OFF}

function XInputGetState;                  external XINPUT_DLL name 'XInputGetState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputSetState;                  external XINPUT_DLL name 'XInputSetState' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputGetCapabilities;           external XINPUT_DLL name 'XInputGetCapabilities' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure XInputEnable;                   external XINPUT_DLL name 'XInputEnable' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputGetAudioDeviceIds;         external XINPUT_DLL name 'XInputGetAudioDeviceIds' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputGetBatteryInformation;     external XINPUT_DLL name 'XInputGetBatteryInformation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputGetKeystroke;              external XINPUT_DLL name 'XInputGetKeystroke' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function XInputGetDSoundAudioDeviceGuids; external XINPUT_DLL name 'XInputGetDSoundAudioDeviceGuids' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
