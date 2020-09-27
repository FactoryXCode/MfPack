// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.JoyStickApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-joystick-l1-1-0
//              Part of Windows Multimedia
//              See: https://docs.microsoft.com/en-us/windows/win32/api/_multimedia/
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
// Remarks: -
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
// Source: joystickapi.h
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
//==============================================================================
unit WinApi.WinMM.JoyStickApi;

interface

uses
  WinApi.Windows,
  WinApi.WinMM.MMSysCom;

  //****************************************************************************
  //
  //                          Joystick support
  //
  //****************************************************************************

  { joystick error return values }

const

  {$EXTERNALSYM JOYERR_NOERROR}
  JOYERR_NOERROR                      = (0);  { no error }
  {$EXTERNALSYM JOYERR_PARMS}
  JOYERR_PARMS                        = (JOYERR_BASE + 5);  { bad parameters }
  {$EXTERNALSYM JOYERR_NOCANDO}
  JOYERR_NOCANDO                      = (JOYERR_BASE + 6);  { request not completed }
  {$EXTERNALSYM JOYERR_UNPLUGGED}
  JOYERR_UNPLUGGED                    = (JOYERR_BASE + 7);  { joystick is unplugged }

  { constants used with JOYINFO and JOYINFOEX structures and MM_JOY* messages }

  {$EXTERNALSYM JOY_BUTTON1}
  JOY_BUTTON1                         = $0001;
  {$EXTERNALSYM JOY_BUTTON2}
  JOY_BUTTON2                         = $0002;
  {$EXTERNALSYM JOY_BUTTON3}
  JOY_BUTTON3                         = $0004;
  {$EXTERNALSYM JOY_BUTTON4}
  JOY_BUTTON4                         = $0008;
  {$EXTERNALSYM JOY_BUTTON1CHG}
  JOY_BUTTON1CHG                      = $0100;
  {$EXTERNALSYM JOY_BUTTON2CHG}
  JOY_BUTTON2CHG                      = $0200;
  {$EXTERNALSYM JOY_BUTTON3CHG}
  JOY_BUTTON3CHG                      = $0400;
  {$EXTERNALSYM JOY_BUTTON4CHG}
  JOY_BUTTON4CHG                      = $0800;

  { constants used with JOYINFOEX }

  {$EXTERNALSYM JOY_BUTTON5}
  JOY_BUTTON5                         = $00000010;
  {$EXTERNALSYM JOY_BUTTON6}
  JOY_BUTTON6                         = $00000020;
  {$EXTERNALSYM JOY_BUTTON7}
  JOY_BUTTON7                         = $00000040;
  {$EXTERNALSYM JOY_BUTTON8}
  JOY_BUTTON8                         = $00000080;
  {$EXTERNALSYM JOY_BUTTON9}
  JOY_BUTTON9                         = $00000100;
  {$EXTERNALSYM JOY_BUTTON10}
  JOY_BUTTON10                        = $00000200;
  {$EXTERNALSYM JOY_BUTTON11}
  JOY_BUTTON11                        = $00000400;
  {$EXTERNALSYM JOY_BUTTON12}
  JOY_BUTTON12                        = $00000800;
  {$EXTERNALSYM JOY_BUTTON13}
  JOY_BUTTON13                        = $00001000;
  {$EXTERNALSYM JOY_BUTTON14}
  JOY_BUTTON14                        = $00002000;
  {$EXTERNALSYM JOY_BUTTON15}
  JOY_BUTTON15                        = $00004000;
  {$EXTERNALSYM JOY_BUTTON16}
  JOY_BUTTON16                        = $00008000;
  {$EXTERNALSYM JOY_BUTTON17}
  JOY_BUTTON17                        = $00010000;
  {$EXTERNALSYM JOY_BUTTON18}
  JOY_BUTTON18                        = $00020000;
  {$EXTERNALSYM JOY_BUTTON19}
  JOY_BUTTON19                        = $00040000;
  {$EXTERNALSYM JOY_BUTTON20}
  JOY_BUTTON20                        = $00080000;
  {$EXTERNALSYM JOY_BUTTON21}
  JOY_BUTTON21                        = $00100000;
  {$EXTERNALSYM JOY_BUTTON22}
  JOY_BUTTON22                        = $00200000;
  {$EXTERNALSYM JOY_BUTTON23}
  JOY_BUTTON23                        = $00400000;
  {$EXTERNALSYM JOY_BUTTON24}
  JOY_BUTTON24                        = $00800000;
  {$EXTERNALSYM JOY_BUTTON25}
  JOY_BUTTON25                        = $01000000;
  {$EXTERNALSYM JOY_BUTTON26}
  JOY_BUTTON26                        = $02000000;
  {$EXTERNALSYM JOY_BUTTON27}
  JOY_BUTTON27                        = $04000000;
  {$EXTERNALSYM JOY_BUTTON28}
  JOY_BUTTON28                        = $08000000;
  {$EXTERNALSYM JOY_BUTTON29}
  JOY_BUTTON29                        = $10000000;
  {$EXTERNALSYM JOY_BUTTON30}
  JOY_BUTTON30                        = $20000000;
  {$EXTERNALSYM JOY_BUTTON31}
  JOY_BUTTON31                        = $40000000;
  {$EXTERNALSYM JOY_BUTTON32}
  JOY_BUTTON32                        = $80000000;

  { constants used with JOYINFOEX structure }

  {$EXTERNALSYM JOY_POVCENTERED}
  JOY_POVCENTERED                     = WORD (- 1);
  {$EXTERNALSYM JOY_POVFORWARD}
  JOY_POVFORWARD                      = 0;
  {$EXTERNALSYM JOY_POVRIGHT}
  JOY_POVRIGHT                        = 9000;
  {$EXTERNALSYM JOY_POVBACKWARD}
  JOY_POVBACKWARD                     = 18000;
  {$EXTERNALSYM JOY_POVLEFT}
  JOY_POVLEFT                         = 27000;

  {$EXTERNALSYM JOY_RETURNX}
  JOY_RETURNX                         = $00000001;
  {$EXTERNALSYM JOY_RETURNY}
  JOY_RETURNY                         = $00000002;
  {$EXTERNALSYM JOY_RETURNZ}
  JOY_RETURNZ                         = $00000004;
  {$EXTERNALSYM JOY_RETURNR}
  JOY_RETURNR                         = $00000008;
  {$EXTERNALSYM JOY_RETURNU}
  JOY_RETURNU                         = $00000010;  { axis 5 }
  {$EXTERNALSYM JOY_RETURNV}
  JOY_RETURNV                         = $00000020;  { axis 6 }
  {$EXTERNALSYM JOY_RETURNPOV}
  JOY_RETURNPOV                       = $00000040;
  {$EXTERNALSYM JOY_RETURNBUTTONS}
  JOY_RETURNBUTTONS                   = $00000080;
  {$EXTERNALSYM JOY_RETURNRAWDATA}
  JOY_RETURNRAWDATA                   = $00000100;
  {$EXTERNALSYM JOY_RETURNPOVCTS}
  JOY_RETURNPOVCTS                    = $00000200;
  {$EXTERNALSYM JOY_RETURNCENTERED}
  JOY_RETURNCENTERED                  = $00000400;
  {$EXTERNALSYM JOY_USEDEADZONE}
  JOY_USEDEADZONE                     = $00000800;
  {$EXTERNALSYM JOY_RETURNALL}
  JOY_RETURNALL                       = (JOY_RETURNX OR JOY_RETURNY OR JOY_RETURNZ OR
                                         JOY_RETURNR OR JOY_RETURNU OR JOY_RETURNV OR
                                         JOY_RETURNPOV OR JOY_RETURNBUTTONS);

  {$EXTERNALSYM JOY_CAL_READALWAYS}
  JOY_CAL_READALWAYS                  = $00010000;
  {$EXTERNALSYM JOY_CAL_READXYONLY}
  JOY_CAL_READXYONLY                  = $00020000;
  {$EXTERNALSYM JOY_CAL_READ3}
  JOY_CAL_READ3                       = $00040000;
  {$EXTERNALSYM JOY_CAL_READ4}
  JOY_CAL_READ4                       = $00080000;
  {$EXTERNALSYM JOY_CAL_READXONLY}
  JOY_CAL_READXONLY                   = $00100000;
  {$EXTERNALSYM JOY_CAL_READYONLY}
  JOY_CAL_READYONLY                   = $00200000;
  {$EXTERNALSYM JOY_CAL_READ5}
  JOY_CAL_READ5                       = $00400000;
  {$EXTERNALSYM JOY_CAL_READ6}
  JOY_CAL_READ6                       = $00800000;
  {$EXTERNALSYM JOY_CAL_READZONLY}
  JOY_CAL_READZONLY                   = $01000000;
  {$EXTERNALSYM JOY_CAL_READRONLY}
  JOY_CAL_READRONLY                   = $02000000;
  {$EXTERNALSYM JOY_CAL_READUONLY}
  JOY_CAL_READUONLY                   = $04000000;
  {$EXTERNALSYM JOY_CAL_READVONLY}
  JOY_CAL_READVONLY                   = $08000000;

  { joystick ID constants }

  {$EXTERNALSYM JOYSTICKID1}
  JOYSTICKID1                         = 0;
  {$EXTERNALSYM JOYSTICKID2}
  JOYSTICKID2                         = 1;

  { joystick driver capabilites }

  {$EXTERNALSYM JOYCAPS_HASZ}
  JOYCAPS_HASZ                        = $0001;
  {$EXTERNALSYM JOYCAPS_HASR}
  JOYCAPS_HASR                        = $0002;
  {$EXTERNALSYM JOYCAPS_HASU}
  JOYCAPS_HASU                        = $0004;
  {$EXTERNALSYM JOYCAPS_HASV}
  JOYCAPS_HASV                        = $0008;
  {$EXTERNALSYM JOYCAPS_HASPOV}
  JOYCAPS_HASPOV                      = $0010;
  {$EXTERNALSYM JOYCAPS_POV4DIR}
  JOYCAPS_POV4DIR                     = $0020;
  {$EXTERNALSYM JOYCAPS_POVCTS}
  JOYCAPS_POVCTS                      = $0040;



  { joystick device capabilities data structure }

type

  PJOYCAPSA = ^tagJOYCAPSA;
  {$EXTERNALSYM PJOYCAPSA}
  tagJOYCAPSA = record
    wMid: WORD;                                                   { manufacturer ID }
    wPid: WORD;                                                   { product ID }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;               { product name (NULL terminated string) }
    wXmin: UINT;                                                  { minimum x position value }
    wXmax: UINT;                                                  { maximum x position value }
    wYmin: UINT;                                                  { minimum y position value }
    wYmax: UINT;                                                  { maximum y position value }
    wZmin: UINT;                                                  { minimum z position value }
    wZmax: UINT;                                                  { maximum z position value }
    wNumButtons: UINT;                                            { number of buttons }
    wPeriodMin: UINT;                                             { minimum message period when captured }
    wPeriodMax: UINT;                                             { maximum message period when captured }
    wRmin: UINT;                                                  { minimum r position value }
    wRmax: UINT;                                                  { maximum r position value }
    wUmin: UINT;                                                  { minimum u (5th axis) position value }
    wUmax: UINT;                                                  { maximum u (5th axis) position value }
    wVmin: UINT;                                                  { minimum v (6th axis) position value }
    wVmax: UINT;                                                  { maximum v (6th axis) position value }
    wCaps: UINT;                                                  { joystick capabilites }
    wMaxAxes: UINT;                                               { maximum number of axes supported }
    wNumAxes: UINT;                                               { number of axes in use }
    wMaxButtons: UINT;                                            { maximum number of buttons supported }
    szRegKey: array[0..MAXPNAMELEN - 1] of AnsiChar;              { registry key }
    szOEMVxD: array[0..MAX_JOYSTICKOEMVXDNAME - 1] of AnsiChar;  { OEM VxD in use }
  end;
  {$EXTERNALSYM tagJOYCAPSA}
  JOYCAPSA = tagJOYCAPSA;
  {$EXTERNALSYM JOYCAPSA}
  NPJOYCAPSA = ^tagJOYCAPSA;
  {$EXTERNALSYM NPJOYCAPSA}
  LPJOYCAPSA = ^tagJOYCAPSA;
  {$EXTERNALSYM LPJOYCAPSA}

  PJOYCAPSW = ^tagJOYCAPSW;
  {$EXTERNALSYM PJOYCAPSW}
  tagJOYCAPSW = record
    wMid: WORD;                                                   { manufacturer ID }
    wPid: WORD;                                                   { product ID }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;               { product name (NULL terminated string) }
    wXmin: UINT;                                                  { minimum x position value }
    wXmax: UINT;                                                  { maximum x position value }
    wYmin: UINT;                                                  { minimum y position value }
    wYmax: UINT;                                                  { maximum y position value }
    wZmin: UINT;                                                  { minimum z position value }
    wZmax: UINT;                                                  { maximum z position value }
    wNumButtons: UINT;                                            { number of buttons }
    wPeriodMin: UINT;                                             { minimum message period when captured }
    wPeriodMax: UINT;                                             { maximum message period when captured }
    wRmin: UINT;                                                  { minimum r position value }
    wRmax: UINT;                                                  { maximum r position value }
    wUmin: UINT;                                                  { minimum u (5th axis) position value }
    wUmax: UINT;                                                  { maximum u (5th axis) position value }
    wVmin: UINT;                                                  { minimum v (6th axis) position value }
    wVmax: UINT;                                                  { maximum v (6th axis) position value }
    wCaps: UINT;                                                  { joystick capabilites }
    wMaxAxes: UINT;                                               { maximum number of axes supported }
    wNumAxes: UINT;                                               { number of axes in use }
    wMaxButtons: UINT;                                            { maximum number of buttons supported }
    szRegKey: array[0..MAXPNAMELEN - 1] of WideChar;              { registry key }
    szOEMVxD: array[0..MAX_JOYSTICKOEMVXDNAME - 1] of WideChar;  { OEM VxD in use }
  end;
  {$EXTERNALSYM tagJOYCAPSW}
  JOYCAPSW = tagJOYCAPSW;
  {$EXTERNALSYM JOYCAPSW}
  NPJOYCAPSW = ^tagJOYCAPSW;
  {$EXTERNALSYM NPJOYCAPSW}
  LPJOYCAPSW = ^tagJOYCAPSW;
  {$EXTERNALSYM LPJOYCAPSW}

  { Delphi }
  JOYCAPS = tagJOYCAPSW;
  PJOYCAPS = ^tagJOYCAPSW;
  NPJOYCAPS = ^tagJOYCAPSW;
  LPJOYCAPS = ^tagJOYCAPSW;


  PJOYCAPS2A = ^tagJOYCAPS2A;
  {$EXTERNALSYM PJOYCAPS2A}
  tagJOYCAPS2A = record
    wMid: WORD;                                                   { manufacturer ID }
    wPid: WORD;                                                   { product ID }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;               { product name (NULL terminated string) }
    wXmin: UINT;                                                  { minimum x position value }
    wXmax: UINT;                                                  { maximum x position value }
    wYmin: UINT;                                                  { minimum y position value }
    wYmax: UINT;                                                  { maximum y position value }
    wZmin: UINT;                                                  { minimum z position value }
    wZmax: UINT;                                                  { maximum z position value }
    wNumButtons: UINT;                                            { number of buttons }
    wPeriodMin: UINT;                                             { minimum message period when captured }
    wPeriodMax: UINT;                                             { maximum message period when captured }
    wRmin: UINT;                                                  { minimum r position value }
    wRmax: UINT;                                                  { maximum r position value }
    wUmin: UINT;                                                  { minimum u (5th axis) position value }
    wUmax: UINT;                                                  { maximum u (5th axis) position value }
    wVmin: UINT;                                                  { minimum v (6th axis) position value }
    wVmax: UINT;                                                  { maximum v (6th axis) position value }
    wCaps: UINT;                                                  { joystick capabilites }
    wMaxAxes: UINT;                                               { maximum number of axes supported }
    wNumAxes: UINT;                                               { number of axes in use }
    wMaxButtons: UINT;                                            { maximum number of buttons supported }
    szRegKey: array[0..MAXPNAMELEN - 1] of AnsiChar;              { registry key }
    szOEMVxD: array[0..MAX_JOYSTICKOEMVXDNAME - 1] of AnsiChar;   { OEM VxD in use }
    ManufacturerGuid: TGUID;                                      { for extensible MID mapping }
    ProductGuid: TGUID;                                           { for extensible PID mapping }
    NameGuid: TGUID;                                             { for name lookup in registry }
  end;
  {$EXTERNALSYM tagJOYCAPS2A}
  JOYCAPS2A = tagJOYCAPS2A;
  {$EXTERNALSYM JOYCAPS2A}
  NPJOYCAPS2A = ^tagJOYCAPS2A;
  {$EXTERNALSYM NPJOYCAPS2A}
  LPJOYCAPS2A = ^tagJOYCAPS2A;
  {$EXTERNALSYM LPJOYCAPS2A}

  PJOYCAPS2W = ^tagJOYCAPS2W;
  {$EXTERNALSYM PJOYCAPS2W}
  tagJOYCAPS2W = record
    wMid: WORD;                                                   { manufacturer ID }
    wPid: WORD;                                                   { product ID }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;               { product name (NULL terminated string) }
    wXmin: UINT;                                                  { minimum x position value }
    wXmax: UINT;                                                  { maximum x position value }
    wYmin: UINT;                                                  { minimum y position value }
    wYmax: UINT;                                                  { maximum y position value }
    wZmin: UINT;                                                  { minimum z position value }
    wZmax: UINT;                                                  { maximum z position value }
    wNumButtons: UINT;                                            { number of buttons }
    wPeriodMin: UINT;                                             { minimum message period when captured }
    wPeriodMax: UINT;                                             { maximum message period when captured }
    wRmin: UINT;                                                  { minimum r position value }
    wRmax: UINT;                                                  { maximum r position value }
    wUmin: UINT;                                                  { minimum u (5th axis) position value }
    wUmax: UINT;                                                  { maximum u (5th axis) position value }
    wVmin: UINT;                                                  { minimum v (6th axis) position value }
    wVmax: UINT;                                                  { maximum v (6th axis) position value }
    wCaps: UINT;                                                  { joystick capabilites }
    wMaxAxes: UINT;                                               { maximum number of axes supported }
    wNumAxes: UINT;                                               { number of axes in use }
    wMaxButtons: UINT;                                            { maximum number of buttons supported }
    szRegKey: array[0..MAXPNAMELEN - 1] of WideChar;              { registry key }
    szOEMVxD: array[0..MAX_JOYSTICKOEMVXDNAME - 1] of WideChar;   { OEM VxD in use }
    ManufacturerGuid: TGUID;                                      { for extensible MID mapping }
    ProductGuid: TGUID;                                           { for extensible PID mapping }
    NameGuid: TGUID;                                             { for name lookup in registry }
  end;
  {$EXTERNALSYM tagJOYCAPS2W}
  JOYCAPS2W = tagJOYCAPS2W;
  {$EXTERNALSYM JOYCAPS2W}
  NPJOYCAPS2W = ^tagJOYCAPS2W;
  {$EXTERNALSYM NPJOYCAPS2W}
  LPJOYCAPS2W = ^tagJOYCAPS2W;
  {$EXTERNALSYM LPJOYCAPS2W}

  { Delphi }
  JOYCAPS2 = tagJOYCAPS2W;
  PJOYCAPS2 = ^tagJOYCAPS2W;
  NPJOYCAPS2 = ^tagJOYCAPS2W;
  LPJOYCAPS2 = ^tagJOYCAPS2W;


  { joystick information data structure }

  PJOYINFO = ^joyinfo_tag;
  {$EXTERNALSYM PJOYINFO}
  joyinfo_tag = record
    wXpos: UINT;                     { x position }
    wYpos: UINT;                     { y position }
    wZpos: UINT;                     { z position }
    wButtons: UINT;                 { button states }
  end;
  {$EXTERNALSYM joyinfo_tag}
  JOYINFO = joyinfo_tag;
  {$EXTERNALSYM JOYINFO}
  NPJOYINFO = ^joyinfo_tag;
  {$EXTERNALSYM NPJOYINFO}
  LPJOYINFO = ^joyinfo_tag;
  {$EXTERNALSYM LPJOYINFO}


  PJOYINFOEX = ^joyinfoex_tag;
  {$EXTERNALSYM PJOYINFOEX}
  joyinfoex_tag = record
    dwSize: DWORD;                   { size of structure }
    dwFlags: DWORD;                  { flags to indicate what to return }
    dwXpos: DWORD;                   { x position }
    dwYpos: DWORD;                   { y position }
    dwZpos: DWORD;                   { z position }
    dwRpos: DWORD;                   { rudder/4th axis position }
    dwUpos: DWORD;                   { 5th axis position }
    dwVpos: DWORD;                   { 6th axis position }
    dwButtons: DWORD;                { button states }
    dwButtonNumber: DWORD;           { current button number pressed }
    dwPOV: DWORD;                    { point of view state }
    dwReserved1: DWORD;              { reserved for communication between winmm  driver }
    dwReserved2: DWORD;             { reserved for future expansion }
  end;
  {$EXTERNALSYM joyinfoex_tag}
  JOYINFOEX = joyinfoex_tag;
  {$EXTERNALSYM JOYINFOEX}
  NPJOYINFOEX = ^joyinfoex_tag;
  {$EXTERNALSYM NPJOYINFOEX}
  LPJOYINFOEX = ^joyinfoex_tag;
  {$EXTERNALSYM LPJOYINFOEX}


  { joystick function prototypes }

  function joyGetPosEx(uJoyID: UINT;
                      {_Out_} pji: LPJOYINFOEX): MMRESULT; stdcall;

  function joyGetNumDevs(): MMRESULT; stdcall;

  function joyGetDevCapsA(uJoyID: UINT_PTR;
                          {_Out_} pjc: LPJOYCAPSA;
                          cbjc: UINT): MMRESULT; stdcall;

  function joyGetDevCapsW(uJoyID: UINT_PTR;
                          {_Out_} pjc: LPJOYCAPSW;
                          cbjc: UINT): MMRESULT; stdcall;
  { Delphi }
  function joyGetDevCaps(uJoyID: UINT_PTR;
                         {_Out_} pjc: LPJOYCAPSW;
                         cbjc: UINT): MMRESULT; stdcall;

  function joyGetPos(uJoyID: UINT;
                     {_Out_} pji: LPJOYINFO): MMRESULT; stdcall;

  function joyGetThreshold(uJoyID: UINT;
                           {_Out_} puThreshold: PUINT): MMRESULT; stdcall;

  function joyReleaseCapture(uJoyID: UINT): MMRESULT; stdcall;

  function joySetCapture(hwnd: HWND;
                         uJoyID: UINT;
                         uPeriod: UINT;
                         fChanged: BOOL): MMRESULT; stdcall;

  function joySetThreshold(uJoyID: UINT;
                           uThreshold: UINT): MMRESULT; stdcall;

  function joyConfigChanged(dwFlags: DWORD): MMRESULT; stdcall;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  JoyStickApiLib = 'winmm.dll';


function joyGetPosEx;    external JoyStickApiLib name 'joyGetPosEx';
function joyGetNumDevs;  external JoyStickApiLib name 'joyGetNumDevs';

function joyGetDevCapsA;  external JoyStickApiLib name 'joyGetDevCapsA';
function joyGetDevCapsW;  external JoyStickApiLib name 'joyGetDevCapsW';
function joyGetDevCaps;   external JoyStickApiLib name 'joyGetDevCapsW';

function joyGetPos;         external JoyStickApiLib name 'joyGetPos';
function joyGetThreshold;   external JoyStickApiLib name 'joyGetThreshold';
function joyReleaseCapture; external JoyStickApiLib name 'joyReleaseCapture';
function joySetCapture;     external JoyStickApiLib name 'joySetCapture';
function joySetThreshold;   external JoyStickApiLib name 'joySetThreshold';
function joyConfigChanged;  external JoyStickApiLib name 'joyConfigChanged';

  // Implement Additional Prototypes here.

end.
