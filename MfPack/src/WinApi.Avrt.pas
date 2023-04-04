// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AVRT.pas
// Kind: Pascal / Delphi unit
// Release date: 04-03-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description:  This module contains the multimedia class scheduler APIs and any public data
//               structures needed to call these APIs.
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
// Remarks: Requires Windows Vista or higher.
//
//         Using packed records is not a recommended practice,
//         because it can prevent compatibility with other languages or
//         platforms, it slows data access, and, in the case of a character array,
//         it affects type compatibility.
//         For more information, see Memory management and Implicit Packing of
//         Fields with a Common Type Specification.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: avrt.h
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
unit WinApi.Avrt;

  {$HPPEMIT '#include "avrt.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes;

type

  //
  // AvRt Priorities
  //
  PAVRT_PRIORITY = ^AVRT_PRIORITY;
  _AVRT_PRIORITY = (AVRT_PRIORITY_VERYLOW = -2,
                    AVRT_PRIORITY_LOW,
                    AVRT_PRIORITY_NORMAL,
                    AVRT_PRIORITY_HIGH,
                    AVRT_PRIORITY_CRITICAL);
  {$EXTERNALSYM _AVRT_PRIORITY}
  AVRT_PRIORITY = _AVRT_PRIORITY;
  {$EXTERNALSYM AVRT_PRIORITY}

const
  //
  // Infinite timeout for a thread order group.
  //
  THREAD_ORDER_GROUP_INFINITE_TIMEOUT = Int64(-1);
  {$EXTERNALSYM THREAD_ORDER_GROUP_INFINITE_TIMEOUT}

  //
  // Define API decoration for direct importing of DLL references.
  //

  function AvSetMmThreadCharacteristics({$IFDEF UNICODE}TaskName: LPCWSTR;{$ELSE}TaskName: LPCSTR;{$ENDIF}
                                        var TaskIndex: DWORD): THandle;
  {$EXTERNALSYM AvSetMmThreadCharacteristics}

  function AvSetMmThreadCharacteristicsA(TaskName: LPCSTR;
                                         var TaskIndex: DWORD): THandle;
  {$EXTERNALSYM AvSetMmThreadCharacteristicsA}

  function AvSetMmThreadCharacteristicsW(TaskName: LPCWSTR;
                                         var TaskIndex: DWORD): THandle;
  {$EXTERNALSYM AvSetMmThreadCharacteristicsW}


  function AvSetMmMaxThreadCharacteristics({$IFDEF UNICODE}FirstTask: LPCWSTR;{$ELSE}FirstTask: LPCSTR;{$ENDIF}
                                           {$IFDEF UNICODE}SecondTask: LPCWSTR;{$ELSE}SecondTask: LPCSTR;{$ENDIF}
                                            var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristics}


  function AvSetMmMaxThreadCharacteristicsA(FirstTask: LPCSTR;
                                            SecondTask: LPCSTR;
                                            var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristicsA}

  function AvSetMmMaxThreadCharacteristicsW(FirstTask: LPCWSTR;
                                            SecondTask: LPCWSTR;
                                            var TaskIndex: DWORD): THandle; stdcall;
  {$EXTERNALSYM AvSetMmMaxThreadCharacteristicsW}

  function AvRevertMmThreadCharacteristics(AvrtHandle: THandle): BOOL; stdcall;
  {$EXTERNALSYM AvRevertMmThreadCharacteristics}

  function AvSetMmThreadPriority(AvrtHandle: THandle;
                                 Priority: AVRT_PRIORITY): BOOL; stdcall;
  {$EXTERNALSYM AvSetMmThreadPriority}

  function AvRtCreateThreadOrderingGroup(out Context: THandle;
                                         Period: PLARGE_INTEGER;
                                         var ThreadOrderingGuid: TGUID;
                                         Timeout: PLARGE_INTEGER): BOOL; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroup}


  function AvRtCreateThreadOrderingGroupEx(out Context: THandle;
                                           Period: PLARGE_INTEGER;
                                           var ThreadOrderingGuid: TGUID;
                                           Timeout: PLARGE_INTEGER;
                                           {$IFDEF UNICODE}TaskName: LPCWSTR{$ELSE}TaskName: LPCSTR{$ENDIF}): BOOL; stdcall;

  function AvRtCreateThreadOrderingGroupExA(out Context: THandle;
                                            Period: PLARGE_INTEGER;
                                            var ThreadOrderingGuid: TGUID;
                                            Timeout: PLARGE_INTEGER;
                                            TaskName: LPCSTR): BOOL; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroupExA}

  function AvRtCreateThreadOrderingGroupExW(out Context: THandle;
                                            Period: PLARGE_INTEGER;
                                            var ThreadOrderingGuid: TGUID;
                                            Timeout: PLARGE_INTEGER;
                                            TaskName: LPCWSTR): BOOL; stdcall;
  {$EXTERNALSYM AvRtCreateThreadOrderingGroupExW}

  function AvRtJoinThreadOrderingGroup(out Context: THandle;
                                       const ThreadOrderingGuid: TGUID;
                                       Before: boolean): BOOL; stdcall;
  {$EXTERNALSYM AvRtJoinThreadOrderingGroup}

  function AvRtWaitOnThreadOrderingGroup(Context: THandle): BOOL; stdcall;
  {$EXTERNALSYM AvRtWaitOnThreadOrderingGroup}

  function AvRtLeaveThreadOrderingGroup(Context: THandle): BOOL; stdcall;
  {$EXTERNALSYM AvRtLeaveThreadOrderingGroup}

  function AvRtDeleteThreadOrderingGroup(Context: THandle): BOOL; stdcall;
  {$EXTERNALSYM AvRtDeleteThreadOrderingGroup}

  function AvQuerySystemResponsiveness(AvrtHandle: THandle;
                                       out SystemResponsivenessValue: ULONG): BOOL; stdcall;
  {$EXTERNALSYM AvQuerySystemResponsiveness}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  AvrtLib = 'avrt.dll';

{$WARN SYMBOL_PLATFORM OFF}

function AvSetMmThreadCharacteristics; external AvrtLib name 'AvSetMmThreadCharacteristics' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvSetMmThreadCharacteristicsA; external AvrtLib name 'AvSetMmThreadCharacteristicsA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvSetMmThreadCharacteristicsW; external AvrtLib name 'AvSetMmThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function AvSetMmMaxThreadCharacteristics; external AvrtLib name 'AvSetMmMaxThreadCharacteristics' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvSetMmMaxThreadCharacteristicsA; external AvrtLib name 'AvSetMmMaxThreadCharacteristicsA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvSetMmMaxThreadCharacteristicsW; external AvrtLib name 'AvSetMmMaxThreadCharacteristicsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function AvRevertMmThreadCharacteristics; external AvrtLib name 'AvRevertMmThreadCharacteristics' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvSetMmThreadPriority; external AvrtLib name 'AvSetMmThreadPriority' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtCreateThreadOrderingGroup; external AvrtLib name 'AvRtCreateThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function AvRtCreateThreadOrderingGroupEx; external AvrtLib name 'AvRtCreateThreadOrderingGroupEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtCreateThreadOrderingGroupExA; external AvrtLib name 'AvRtCreateThreadOrderingGroupExA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtCreateThreadOrderingGroupExW; external AvrtLib name 'AvRtCreateThreadOrderingGroupExW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function AvRtJoinThreadOrderingGroup; external AvrtLib name 'AvRtJoinThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtWaitOnThreadOrderingGroup; external AvrtLib name 'AvRtWaitOnThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtLeaveThreadOrderingGroup; external AvrtLib name 'AvRtLeaveThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvRtDeleteThreadOrderingGroup; external AvrtLib name 'AvRtDeleteThreadOrderingGroup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function AvQuerySystemResponsiveness; external AvrtLib name 'AvQuerySystemResponsiveness' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

// Implement Additional Prototypes here.

end.

