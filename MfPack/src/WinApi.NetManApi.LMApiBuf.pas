// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.NetManApi.LMApiBuf.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description: This file contains information about NetApiBuffer APIs.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: LMApiBuf.h
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
unit WinApi.NetManApi.LMApiBuf;

interface

uses

  {WinApi}
  WinApi.Windows,
  {LanManApi}
  WinApi.NetManApi.LMCons;


  //
  // Function Prototypes
  //

function NetApiBufferAllocate(ByteCount: DWORD;
                              var Buffer: LPVOID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferAllocate}

function NetApiBufferFree(Buffer: LPVOID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferFree}

function NetApiBufferReallocate(OldBuffer: LPVOID;
                                NewByteCount: DWORD;
                                var NewBuffer: LPVOID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferReallocate}

function NetApiBufferSize(Buffer: LPVOID;
                          ByteCount: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferSize}


  //
  // The following private function will go away eventually.
  // Call NetApiBufferAllocate instead.
  //
function NetapipBufferAllocate(ByteCount: DWORD;     // Internal Function
                               var Buffer: LPVOID): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetapipBufferAllocate}



  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

const
  netapi32Lib = 'netapi32.dll';

{$WARN SYMBOL_PLATFORM OFF}
function NetApiBufferAllocate; external netapi32Lib name 'NetApiBufferAllocate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetApiBufferFree; external netapi32Lib name 'NetApiBufferFree' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetApiBufferReallocate; external netapi32Lib name 'NetApiBufferReallocate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetApiBufferSize; external netapi32Lib name 'NetApiBufferSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetapipBufferAllocate; external netapi32Lib name 'NetapipBufferAllocate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
