// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.PropKeyDef.pas
// Kind: Pascal / Delphi unit
// Release date: 06-10-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
// 10/08/2010 All                 #2 => #2b The Model
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 2000 Pro or later.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: propkeydef.h
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
unit MfPack.PropKeyDef;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "propkeydef.h"'}
  {$HPPEMIT ''}

interface

uses

  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfpUtils;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}

const

{$IFNDEF PID_FIRST_USABLE}
  PID_FIRST_USABLE                    = 2;
{$DEFINE PID_FIRST_USABLE}
{$ENDIF }

type

  REFPROPERTYKEY                      =  PROPERTYKEY;
  {$EXTERNALSYM REFPROPERTYKEY}


// Info
//DEFINE_PROPERTYKEY(name;
//                   l: DWORD;
//                   w1: WORD;
//                   w2: WORD;
//                   b1: Byte;
//                   b2: Byte;
//                   b3: Byte;
//                   b4: Byte;
//                   b5: Byte;
//                   b6: Byte;
//                   b7: Byte;
//                   b8: Byte;
//                   pid: DWORD);
// Used to pack a format identifier (FMTID) and property identifier (PID) into a
// PROPERTYKEY structure that represents a property key.
// Parameters:
// name The name of a PROPERTYKEY structure that represents a property key.
// l    The value of the Data1 member of the fmtid member of the PROPERTYKEY structure.
// w1   The value of the Data2 member of the fmtid member of the PROPERTYKEY structure.
// w2   The value of the Data3 member of the fmtid member of the PROPERTYKEY structure.
// b1   The value of the Data4[0] member of the fmtid member of the PROPERTYKEY structure.
// b2   The value of the Data4[1] member of the fmtid member of the PROPERTYKEY structure.
// b3   The value of the Data4[2] member of the fmtid member of the PROPERTYKEY structure.
// b4   The value of the Data4[3] member of the fmtid member of the PROPERTYKEY structure.
// b5   The value of the Data4[4] member of the fmtid member of the PROPERTYKEY structure.
// b6   The value of the Data4[5] member of the fmtid member of the PROPERTYKEY structure.
// b7   The value of the Data4[6] member of the fmtid member of the PROPERTYKEY structure.
// b8   The value of the Data4[7] member of the fmtid member of the PROPERTYKEY structure.
// pid  A property identifier (PID). It is recommended that you set this value to PID_FIRST_USABLE.
//      Any value greater than or equal to 2 is acceptable.
//      Note:  Values of 0 and 1 are reserved and should not be used.
//
// Return value: -
// This macro does not return a value.
// Remarks:
// In Delphi you should implement a PROPERTYKEY that needs to be defined this way:
// pkname: PROPERTYKEY = (fmtid: (D1: $xxxxxxxx;
//                                D2: $xxxx;
//                                D3: $xxxx;
//                                D4: ($xx, $xx, $xx, $xx, $xx, $xx, $xx, $xx));
//								                pid: 0);
//
// Example:
// PKEY_AudioEndpoint_FormFactor: PROPERTYKEY = (fmtid: (D1: $1da5d803;
//                                                       D2: $d492;
//                                                       D3: $4edd;
//                                                       D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
//								                                       pid: 0);
//


  // Translated MACRO IsEqualPropertyKey
  function IsEqualPropertyKey(pkeyOne: PROPERTYKEY;
                              pkeyOther: PROPERTYKEY ): BOOL;
  {$EXTERNALSYM IsEqualPropertyKey}
  // Compares the members of two PROPERTYKEY structures and returns whether they are equal.
  // Parameters:
  //   pkeyOne    The first PROPERTYKEY.
  //   pkeyOther  The second PROPERTYKEY.
  // Return value:
  //   Returns TRUE if the PROPERTYKEY structure members are equal.
  // Remarks -


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes


implementation

function IsEqualPropertyKey(pkeyOne: PROPERTYKEY;
                            pkeyOther: PROPERTYKEY): BOOL;
begin
  Result:= (pkeyOne.pid = pkeyOther.pid) and
            IsEqualGUID(pkeyOne.fmtid,
                        pkeyOther.fmtid);
end;

  // Implement Additional Prototypes here.

end.
