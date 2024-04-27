// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - WinApi
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: System.Services.Avrt.pas
// Kind: Pascal / Delphi unit
// Release date: 24-10-2020
// Language: ENU
//
// Revision Version: 3.1.6
// Description: This module contains the multimedia class scheduler APIs and any public data
//              structures needed to call these APIs.
//              This header is part of the System Services API.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Please take notice of ANSI or Unicode versions of the included function.
//          Mixing usage of the encoding-neutral alias with code that not encoding-neutral
//          can lead to mismatches that result in compilation or runtime errors.
//          For more information, see https://docs.microsoft.com/en-us/windows/win32/intl/conventions-for-function-prototypes.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: avrt.h
// *++ BUILD Version: 0001    // Increment this if a change has global effects
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
unit System.Services.Avrt;

  {$HPPEMIT '#include "avrt.h"'}

interface

uses
  WinApi.Windows;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type

  //
  // AvRt Priorities
  //

  _AVRT_PRIORITY = (
    AVRT_PRIORITY_VERYLOW = -2,
    AVRT_PRIORITY_LOW,
    AVRT_PRIORITY_NORMAL,
    AVRT_PRIORITY_HIGH,
    AVRT_PRIORITY_CRITICAL);
  {$EXTERNALSYM _AVRT_PRIORITY}
  AVRT_PRIORITY = _AVRT_PRIORITY;
  {$EXTERNALSYM AVRT_PRIORITY}
  PAVRT_PRIORITY = ^_AVRT_PRIORITY;
  {$EXTERNALSYM PAVRT_PRIORITY}

const

  //
  //  Infinite timeout for a thread order group.
  //

  THREAD_ORDER_GROUP_INFINITE_TIMEOUT = Int64(-1);
  {$EXTERNALSYM THREAD_ORDER_GROUP_INFINITE_TIMEOUT}



  // The following functions returns a handle if succeeded and 0 if failed

  function AvSetMmThreadCharacteristicsA({_In_} TaskName: PAnsiChar;
                                         {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmThreadCharacteristicsA}


  function AvSetMmThreadCharacteristicsW({_In_} TaskName: PWideChar;
                                         {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmThreadCharacteristicsW}

  function AvSetMmThreadCharacteristics({_In_} TaskName: PWideChar;
                                        {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmThreadCharacteristics}


  function AvSetMmMaxThreadCharacteristicsA({_In_} FirstTask: PAnsiChar;
                                            {_In_} SecondTask: PAnsiChar;
                                            {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristicsA}

  function AvSetMmMaxThreadCharacteristicsW({_In_} FirstTask: PWideChar;
                                            {_In_} SecondTask: PWideChar;
                                            {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristicsW}

  function AvSetMmMaxThreadCharacteristics({_In_} FirstTask: PWideChar;
                                           {_In_} SecondTask: PWideChar;
                                           {_Inout_} var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristics}


  // Returns False if failed
  function AvRevertMmThreadCharacteristics({_In_} AvrtHandle: THandle): Bool; stdcall;
  {$EXTERNALSYM AvRevertMmThreadCharacteristics}

  // Returns False if failed
  function AvSetMmThreadPriority({_In_} AvrtHandle: THandle;
                                 {_In_} Priority: AVRT_PRIORITY): Bool; stdcall;
  {$EXTERNALSYM AvSetMmThreadPriority}

  // Returns False if failed
  function AvRtCreateThreadOrderingGroup({_Out_} out Context: PHandle;
                                         {_In_} Period: LARGE_INTEGER;
                                         {_Inout_} var ThreadOrderingGuid: TGUID;
                                         {_In_opt_} Timeout: LARGE_INTEGER): Bool; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroup}

  // Returns False if failed
  function AvRtCreateThreadOrderingGroupExA({_Out_} out Context: PHandle;
                                            {_In_} Period: LARGE_INTEGER;
                                            {_Inout_} var ThreadOrderingGuid: TGUID;
                                            {_In_opt_} Timeout: LARGE_INTEGER;
                                            {_In_} TaskName: PAnsiChar): Bool; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroupExA}

  // Returns False if failed
  function AvRtCreateThreadOrderingGroupExW({_Out_} out Context: PHandle;
                                            {_In_} Period: LARGE_INTEGER;
                                            {_Inout_} var ThreadOrderingGuid: TGUID;
                                            {_In_opt_} Timeout: LARGE_INTEGER;
                                            {_In_} TaskName: PWideChar): Bool; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroupExW}

  // Returns False if failed
  function AvRtCreateThreadOrderingGroupEx({_Out_} out Context: PHandle;
                                           {_In_} Period: LARGE_INTEGER;
                                           {_Inout_} var ThreadOrderingGuid: TGUID;
                                           {_In_opt_} Timeout: LARGE_INTEGER;
                                           {_In_} TaskName: PWideChar): Bool; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroupEx}

  // Returns False if failed
  function AvRtJoinThreadOrderingGroup({_Out_} out Context: PHandle;
                                       {_In_} const ThreadOrderingGuid: TGUID;
                                       {_In_} Before: BOOL): Bool; stdcall;
  {$EXTERNALSYM AvRtJoinThreadOrderingGroup}

  // Returns False if failed
  function AvRtWaitOnThreadOrderingGroup({_In_} Context: THandle): Bool; stdcall;
  {$EXTERNALSYM AvRtWaitOnThreadOrderingGroup}

  // Returns False if failed
  function AvRtLeaveThreadOrderingGroup({_In_} Context: THandle): Bool; stdcall;
  {$EXTERNALSYM AvRtLeaveThreadOrderingGroup}

  // Returns False if failed
  function AvRtDeleteThreadOrderingGroup({_In_} Context: THandle): Bool; stdcall;
   {$EXTERNALSYM AvRtDeleteThreadOrderingGroup}

  // Returns False if failed
  function AvQuerySystemResponsiveness({_In_} Context: THandle;
                                       {_Out_} SystemResponsivenessValue: ULONG): Bool; stdcall;
  {$EXTERNALSYM AvQuerySystemResponsiveness}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  AvrtLib = 'avrt.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function AvSetMmThreadCharacteristicsA; external AvrtLib name 'AvSetMmThreadCharacteristicsA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmThreadCharacteristicsW; external AvrtLib name 'AvSetMmThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmThreadCharacteristics; external AvrtLib name 'AvSetMmThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmMaxThreadCharacteristicsA; external AvrtLib name 'AvSetMmMaxThreadCharacteristicsA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmMaxThreadCharacteristicsW; external AvrtLib name 'AvSetMmMaxThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmMaxThreadCharacteristics; external AvrtLib name 'AvSetMmMaxThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRevertMmThreadCharacteristics; external AvrtLib name 'AvRevertMmThreadCharacteristics' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvSetMmThreadPriority; external AvrtLib name 'AvSetMmThreadPriority' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtCreateThreadOrderingGroup; external AvrtLib name 'AvRtCreateThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtCreateThreadOrderingGroupExA; external AvrtLib name 'AvRtCreateThreadOrderingGroupExA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtCreateThreadOrderingGroupExW; external AvrtLib name 'AvRtCreateThreadOrderingGroupExW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtCreateThreadOrderingGroupEx; external AvrtLib name 'AvRtCreateThreadOrderingGroupExW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtJoinThreadOrderingGroup; external AvrtLib name 'AvRtJoinThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtLeaveThreadOrderingGroup; external AvrtLib name 'AvRtLeaveThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtWaitOnThreadOrderingGroup; external AvrtLib name 'AvRtWaitOnThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvRtDeleteThreadOrderingGroup; external AvrtLib name 'AvRtDeleteThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function AvQuerySystemResponsiveness; external AvrtLib name 'AvQuerySystemResponsiveness' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
