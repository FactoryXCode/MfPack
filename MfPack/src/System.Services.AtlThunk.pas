// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: System.Services.AtlThunk.pas
// Kind: Pascal / Delphi unit
// Release date: 15/01/2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description:  This header is part of the System Services API.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: atlthunk.h
// ATL thunks without writable executable memory.
//
// Author: Jay Krell (jaykrell) 26-Apr-2013
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
unit System.Services.AtlThunk;

  {$HPPEMIT '#include "atlthunk.h"'}

interface

uses

  {Winapi}
  Winapi.Windows,
  WinApi.WinApiTypes;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type

  PAtlThunkData_t = ^AtlThunkData_t;
  AtlThunkData_t = record
  end;
  {$NODEFINE AtlThunkData_t}


  // Allocates space in memory for an ATL thunk.
  procedure AtlThunk_AllocateData(); stdcall;
  {$EXTERNALSYM AtlThunk_AllocateData}

  // Initializes an ATL thunk.
  procedure AtlThunk_InitData(Thunk: PAtlThunkData_t;
                              Proc: Pointer;
                              FirstParameter: SIZE_T); stdcall;
  {$EXTERNALSYM AtlThunk_InitData}

  // Returns an executable function corresponding to the AtlThunkData_t parameter.
  procedure AtlThunk_DataToCode(unnamedParam1: PAtlThunkData_t); stdcall;
  {$EXTERNALSYM AtlThunk_DataToCode}

  // Frees memory associated with an ATL thunk.
  procedure AtlThunk_FreeData(Thunk: PAtlThunkData_t); stdcall;
  {$EXTERNALSYM AtlThunk_FreeData}


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes


implementation

const
  AtlthunkLib = 'Atlthunk.dll';


{$WARN SYMBOL_PLATFORM OFF}
  procedure AtlThunk_AllocateData; external AtlthunkLib name 'AtlThunk_AllocateData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure AtlThunk_InitData; external AtlthunkLib name 'AtlThunk_InitData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure AtlThunk_DataToCode; external AtlthunkLib name 'AtlThunk_DataToCode' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure AtlThunk_FreeData; external AtlthunkLib name 'AtlThunk_FreeData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

   // Implement Additional functions here.

end.
