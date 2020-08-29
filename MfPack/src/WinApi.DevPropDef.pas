// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DevPropDef.pas
// Kind: Pascal / Delphi unit
// Release date: 09-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Defines property types and keys for the Plug and Play Device Property API.
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
// Source: devpropdef.h
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
unit WinApi.DevPropDef;

  {$HPPEMIT '#include "devpropdef.h"'}

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


  //
  // Type definition for property data types.  Valid DEVPROPTYPE values are
  // constructed from base DEVPROP_TYPE_ values, which may be modified by a
  // logical OR with DEVPROP_TYPEMOD_ values, as appropriate.
  //

type

{$IFNDEF _DEVPROPTYPE_DEFINED}
  PDEVPROPTYPE = ^ULONG;
  DEVPROPTYPE = ULONG;
  {$EXTERNALSYM DEVPROPTYPE}
{$DEFINE _DEVPROPTYPE_DEFINED}
{$ENDIF}


  //
  // Property type modifiers.  Used to modify base DEVPROP_TYPE_ values, as
  // appropriate.  Not valid as standalone DEVPROPTYPE values.
  //

const

    DEVPROP_TYPEMOD_ARRAY               = $00001000;  // array of fixed-sized data elements
    {$EXTERNALSYM DEVPROP_TYPEMOD_ARRAY}
    DEVPROP_TYPEMOD_LIST                = $00002000;  // list of variable-sized data elements
    {$EXTERNALSYM DEVPROP_TYPEMOD_LIST}

  //
  // Property data types.
  //
    DEVPROP_TYPE_EMPTY                  = $00000000;  // nothing, no property data
    {$EXTERNALSYM DEVPROP_TYPE_EMPTY}
    DEVPROP_TYPE_NULL                   = $00000001;  // null property data
    {$EXTERNALSYM DEVPROP_TYPE_NULL}
    DEVPROP_TYPE_SBYTE                  = $00000002;  // 8-bit signed int (SBYTE)
    {$EXTERNALSYM DEVPROP_TYPE_SBYTE}
    DEVPROP_TYPE_BYTE                   = $00000003;  // 8-bit unsigned int (BYTE)
    {$EXTERNALSYM DEVPROP_TYPE_BYTE}
    DEVPROP_TYPE_INT16                  = $00000004;  // 16-bit signed int (SHORT)
    {$EXTERNALSYM DEVPROP_TYPE_INT16}
    DEVPROP_TYPE_UINT16                 = $00000005;  // 16-bit unsigned int (USHORT)
    {$EXTERNALSYM DEVPROP_TYPE_UINT16}
    DEVPROP_TYPE_INT32                  = $00000006;  // 32-bit signed int (LONG)
    {$EXTERNALSYM DEVPROP_TYPE_INT32}
    DEVPROP_TYPE_UINT32                 = $00000007;  // 32-bit unsigned int (ULONG)
    {$EXTERNALSYM DEVPROP_TYPE_UINT32}
    DEVPROP_TYPE_INT64                  = $00000008;  // 64-bit signed int (LONG64)
    {$EXTERNALSYM DEVPROP_TYPE_INT64}
    DEVPROP_TYPE_UINT64                 = $00000009;  // 64-bit unsigned int (ULONG64)
    {$EXTERNALSYM DEVPROP_TYPE_UINT64}
    DEVPROP_TYPE_FLOAT                  = $0000000A;  // 32-bit floating-point (FLOAT)
    {$EXTERNALSYM DEVPROP_TYPE_FLOAT}
    DEVPROP_TYPE_DOUBLE                 = $0000000B;  // 64-bit floating-point (DOUBLE)
    {$EXTERNALSYM DEVPROP_TYPE_DOUBLE}
    DEVPROP_TYPE_DECIMAL                = $0000000C;  // 128-bit data (DECIMAL)
    {$EXTERNALSYM DEVPROP_TYPE_DECIMAL}
    DEVPROP_TYPE_GUID                   = $0000000D;  // 128-bit unique identifier (GUID)
    {$EXTERNALSYM DEVPROP_TYPE_GUID}
    DEVPROP_TYPE_CURRENCY               = $0000000E;  // 64 bit signed int currency value (CURRENCY)
    {$EXTERNALSYM DEVPROP_TYPE_CURRENCY}
    DEVPROP_TYPE_DATE                   = $0000000F;  // date (DATE)
    {$EXTERNALSYM DEVPROP_TYPE_DATE}
    DEVPROP_TYPE_FILETIME               = $00000010;  // file time (FILETIME)
    {$EXTERNALSYM DEVPROP_TYPE_FILETIME}
    DEVPROP_TYPE_BOOLEAN                = $00000011;  // 8-bit boolean (DEVPROP_BOOLEAN)
    {$EXTERNALSYM DEVPROP_TYPE_BOOLEAN}
    DEVPROP_TYPE_STRING                 = $00000012;  // null-terminated string
    {$EXTERNALSYM DEVPROP_TYPE_STRING}
    DEVPROP_TYPE_STRING_LIST            = (DEVPROP_TYPE_STRING or DEVPROP_TYPEMOD_LIST);  // multi-sz string list
    {$EXTERNALSYM DEVPROP_TYPE_STRING_LIST}
    DEVPROP_TYPE_SECURITY_DESCRIPTOR    = $00000013;  // self-relative binary SECURITY_DESCRIPTOR
    {$EXTERNALSYM DEVPROP_TYPE_SECURITY_DESCRIPTOR}
    DEVPROP_TYPE_SECURITY_DESCRIPTOR_STRING= $00000014;  // security descriptor string (SDDL format)
    {$EXTERNALSYM DEVPROP_TYPE_SECURITY_DESCRIPTOR_STRING}
    DEVPROP_TYPE_DEVPROPKEY             = $00000015;  // device property key (DEVPROPKEY)
    {$EXTERNALSYM DEVPROP_TYPE_DEVPROPKEY}
    DEVPROP_TYPE_DEVPROPTYPE            = $00000016;  // device property type (DEVPROPTYPE)
    {$EXTERNALSYM DEVPROP_TYPE_DEVPROPTYPE}
    DEVPROP_TYPE_BINARY                 = (DEVPROP_TYPE_BYTE or DEVPROP_TYPEMOD_ARRAY);  // custom binary data
    {$EXTERNALSYM DEVPROP_TYPE_BINARY}
    DEVPROP_TYPE_ERROR                  = $00000017;  // 32-bit Win32 system error code
    {$EXTERNALSYM DEVPROP_TYPE_ERROR}
    DEVPROP_TYPE_NTSTATUS               = $00000018;  // 32-bit NTSTATUS code
    {$EXTERNALSYM DEVPROP_TYPE_NTSTATUS}
    DEVPROP_TYPE_STRING_INDIRECT        = $00000019;  // string resource (@[path\]<dllname>,-<strId>)
    {$EXTERNALSYM DEVPROP_TYPE_STRING_INDIRECT}

  //
  // Max base DEVPROP_TYPE_ and DEVPROP_TYPEMOD_ values.
  //

    MAX_DEVPROP_TYPE                    = $00000019;  // max valid DEVPROP_TYPE_ value
    {$EXTERNALSYM MAX_DEVPROP_TYPE}
    MAX_DEVPROP_TYPEMOD                 = $00002000;  // max valid DEVPROP_TYPEMOD_ value
    {$EXTERNALSYM MAX_DEVPROP_TYPEMOD}

  //
  // Bitmasks for extracting DEVPROP_TYPE_ and DEVPROP_TYPEMOD_ values.
  //

    DEVPROP_MASK_TYPE                   = $00000FFF;  // range for base DEVPROP_TYPE_ values
    {$EXTERNALSYM DEVPROP_MASK_TYPE}
    DEVPROP_MASK_TYPEMOD                = $0000F000;  // mask for DEVPROP_TYPEMOD_ type modifiers
    {$EXTERNALSYM DEVPROP_MASK_TYPEMOD}


  //
  // Property type specific data types.
  //

  // 8-bit boolean type definition for DEVPROP_TYPE_BOOLEAN (True = -1, False = 0)
type

  PDevpropBoolean = ^DEVPROP_BOOLEAN;
  PDEVPROP_BOOLEAN = ^ByteBool;
  DEVPROP_BOOLEAN = ByteBool;
  {$EXTERNALSYM DEVPROP_BOOLEAN}

const

  DEVPROP_TRUE                        = -1;
  {$EXTERNALSYM DEVPROP_TRUE}
  DEVPROP_FALSE                       = 0;
  {$EXTERNALSYM DEVPROP_FALSE}



type

  //
  // DEVPROPKEY structure
  // In Windows Vista and later versions of Windows, the DEVPROPKEY structure
  // represents a device property key for a device property in the unified device property model.
  //
{$IFNDEF _DEVPROPKEY_DEFINED}

    PDEVPROPGUID = ^TGUID;
    DEVPROPGUID = TGUID;
    {$EXTERNALSYM DEVPROPGUID}

    PDEVPROPID = ^ULONG;
    DEVPROPID = ULONG;
    {$EXTERNALSYM DEVPROPID}

  PDEVPROPKEY = ^DEVPROPKEY;
  _DEVPROPKEY = record
    fmtid: DEVPROPGUID;
    pid: DEVPROPID;
  end;
  {$EXTERNALSYM _DEVPROPKEY}
  DEVPROPKEY = _DEVPROPKEY;
  {$EXTERNALSYM DEVPROPKEY}
{$DEFINE _DEVPROPKEY_DEFINED}
{$ENDIF} // DEVPROPKEY_DEFINED


  // IsEqualDevPropKey
  // This function is moved to PropKeyDef.pas

  //
  // DEVPROPSTORE Enumeration
  //
  // This enumeration describes where a property is stored.
  //
  PDEVPROPSTORE = ^_DEVPROPSTORE;
  _DEVPROPSTORE = (
    DEVPROP_STORE_SYSTEM = 0,
    DEVPROP_STORE_USER   = 1
  );
  {$EXTERNALSYM _DEVPROPSTORE}
  DEVPROPSTORE = _DEVPROPSTORE;
  {$EXTERNALSYM DEVPROPSTORE}


  //
  // DEVPROPCOMPKEY structure
  //
  // This structure represents a compound key for a property.
  //
  PDEVPROPCOMPKEY = ^DEVPROPCOMPKEY;
  _DEVPROPCOMPKEY = record
    Key: DEVPROPKEY;
    Store: DEVPROPSTORE;
    LocaleName: PWideChar;
  end;
  {$EXTERNALSYM _DEVPROPCOMPKEY}
  DEVPROPCOMPKEY = _DEVPROPCOMPKEY;
  {$EXTERNALSYM DEVPROPCOMPKEY}


  //
  // DEVPROPERTY structure
  //
  PDEVPROPERTY = ^_DEVPROPERTY;
  _DEVPROPERTY = record
    CompKey: DEVPROPCOMPKEY;
    dvType: DEVPROPTYPE;
    BufferSize: ULONG;
    Buffer: PByte;
  end;
  {$EXTERNALSYM _DEVPROPERTY}
  DEVPROPERTY = _DEVPROPERTY;
  {$EXTERNALSYM DEVPROPERTY}


  //
  // All valid DEVPROPKEY definitions must use a PROPID that is equal to or greater
  // than DEVPROPID_FIRST_USABLE.
  //

const

  DEVPROPID_FIRST_USABLE              = 2;
  {$EXTERNALSYM DEVPROPID_FIRST_USABLE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.


end.
