// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.TimeApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-time-l1-1-0
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
// Source: timeapi.h
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

unit WinApi.WinMM.TimeApi;

interface

 {$HPPEMIT '#include "timeapi.h"'}

uses
  WinApi.Windows,
  WinApi.WinMM.MMSysCom;

  //****************************************************************************
  //
  //                          Timer support
  //
  //****************************************************************************

  { timer error return values }

const

  TIMERR_NOERROR                      = (0);  { no error }
  {$EXTERNALSYM TIMERR_NOERROR}
  TIMERR_NOCANDO                      = (TIMERR_BASE + 1);  { request not completed }
  {$EXTERNALSYM TIMERR_NOCANDO}
  TIMERR_STRUCT                       = (TIMERR_BASE + 33);  { time struct size }
  {$EXTERNALSYM TIMERR_STRUCT}

  { timer device capabilities data structure }

type

  PTIMECAPS = ^timecaps_tag;
  {$EXTERNALSYM PTIMECAPS}
  timecaps_tag = record
    wPeriodMin: UINT;               { minimum period supported  }
    wPeriodMax: UINT;               { maximum period supported  }
  end;
  {$EXTERNALSYM timecaps_tag}
  TIMECAPS = timecaps_tag;
  {$EXTERNALSYM TIMECAPS}
  NPTIMECAPS = ^timecaps_tag;
  {$EXTERNALSYM NPTIMECAPS}
  LPTIMECAPS = ^timecaps_tag;
  {$EXTERNALSYM LPTIMECAPS}

  { timer function prototypes }

  function timeGetSystemTime({_Out_} pmmt: LPMMTIME;
                             cbmmt: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM timeGetSystemTime}

  function timeGetTime(): MMRESULT; stdcall;
  {$EXTERNALSYM timeGetTime}

  function timeGetDevCaps({_Out_} ptc: LPTIMECAPS;
                          cbtc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM timeGetDevCaps}

  function timeBeginPeriod(uPeriod: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM timeBeginPeriod}

  function timeEndPeriod(uPeriod: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM timeEndPeriod}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation

const
  TimerApiLib = 'winmm.dll';

function timeGetSystemTime; external TimerApiLib name 'timeGetSystemTime';
function timeGetTime;       external TimerApiLib name 'timeGetTime';
function timeGetDevCaps;    external TimerApiLib name 'timeGetDevCaps';
function timeBeginPeriod;   external TimerApiLib name 'timeBeginPeriod';
function timeEndPeriod;     external TimerApiLib name 'timeEndPeriod';

  // Implement Additional Prototypes here.

end.
