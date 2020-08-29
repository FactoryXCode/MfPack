// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.InitGuid.pas
// Kind: Pascal / Delphi unit
// Release date: 20-12-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Definitions for controlling GUID initialization.
//              OLE Version 2.0
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
// Source: initguid.h
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
unit WinApi.InitGuid;

  {$HPPEMIT '#include "initguid.h"'}
  {$HPPEMIT '#include "guiddef.h"'}

interface


////////////////////////////////////////////////////////////////////////////////
// In Delphi you should implement a GUID that needs to be defined this way:
//
//  Guidname: TGUID = '{B502D1BC-9A57-11d0-8FDE-00C04FD9189D}';
//
// Or like this (both ways are permitted):
//
//  Guidname: TGUID = (D1: $B502D1BC;
//                     D2: $9A57;
//                     D3: $11d0;
//                     D4: ($8F, $DE, $00, $C0, $4F, $D9, $18, $9D));
//
// This guidtype, however, is preferred when a GUID can be altered or used in routines.
//
// A "pid" is added, in case of propertykeys format IDs.
//
//  PKEY_NAME: PROPERTYKEY = (fmtid: (D1: $B725F130;
//                                    D2: $47EF;
//                                    D3: $101A;
//                                    D4: ($A5, $F1, $02, $60, $8C, $9E, $EB, $AC));
//                                    pid: 10);
//
////////////////////////////////////////////////////////////////////////////////

// #include guiddef.h   >>  Definitions are present in Delphi, skip this.

// INITGUID;

implementation

end.
