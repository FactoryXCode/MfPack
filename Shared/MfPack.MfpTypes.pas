// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.MfpTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 29-07-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Generic converted Windows (c/cpp) types for Win32 / Win64 compatibility
//              used by DirectX, Media Foundation and Core Audio.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: wtypes.h, wTypesBase.h
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit MfPack.MfpTypes;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "wtypes.h"'}
  {$HPPEMIT '#include "guiddef.h"'}
  {$HPPEMIT '#include "WTypesbase.h"'}
  {$HPPEMIT ''}


interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Types,
  System.UITypes,
  System.Classes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}


// =============================================================================
// Source: guiddef.h
//
// Copyright (c) 1997-2016 Microsoft Corporation. All rights reserved
//==============================================================================

//
// Don't use this C++ Guid (which is the original aliased _Guid)
// This guid is redefined in the system unit (TGUID).
//{$IFNDEF __GUID_DEFINED}
//  _GUID = record
//    Data1: LongWord;
//    Data2: Word;
//    Data3: Word;
//    Data4: array[0..7] of Byte;
//  end;
//  GUID = _GUID;
//{$DEFINE __GUID_DEFINED}
//

const

// To tackle some issues about where to find the GUID_NULL declaration, here it is.
{$IFDEF MFP_GUID_NULL}
  GUID_NULL  : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM GUID_NULL}
  IID_NULL   : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM IID_NULL}
  CLSID_NULL : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM CLSID_NULL}
  FMTID_NULL : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM FMTID_NULL}
{$ENDIF}


{$ifdef MFP_GUID}
{$undef MFP_GUID} // Sorry ;-)
{$endif}

// In Delphi you should implement a GUID that needs to be defined this way:
//
//  Guidname: TGUID = '{B502D1BC-9A57-11d0-8FDE-00C04FD9189D}';
//
// Or like this (both ways are permitted, how ever, the last one should be used if the guid is an item of change):
//
//  Guidname: TGUID = (D1: $5cefee10;
//                     D2: $e210;
//                     D3: $45c6;
//                     D4: ($9e, $28, $f5, $a8, $73, $1c, $96, $c7));
//
// To keep things simple: don't use the commented out _GUID in this unit,
//                        but the one declared in System
//
// See comments inPropKeyDef.pas
//***********************************************************************************************
//#ifdef INITGUID
//#define MFP_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
//        EXTERN_C const GUID DECLSPEC_SELECTANY name \
//                = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }
//#else
//#define MFP_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
//    EXTERN_C const GUID FAR name
//#endif // INITGUID
//
//#define MFP_OLEGUID(name, l, w1, w2) MFP_GUID(name, l, w1, w2, 0xC0,0,0,0,0,0,0,0x46)
//***********************************************************************************************

type

{$IFDEF MFP_DWORD}
   DWORD = System.Types.DWORD;
   {$IF COMPILERVERSION < 30.0}
     PDWORD = ^LongWord; // Override wrong implementation in pre-delphi 10 versions
   {$ELSE}
     PDWORD = WinApi.Windows.PDWORD;
   {$ENDIF}
{$ENDIF}


{$IFDEF MFP_LPGUID}
  LPGUID = ^PGUID;
  {$EXTERNALSYM LPGUID}
  LPCGUID = ^PGUID;
  {$EXTERNALSYM LPCGUID}
{$ENDIF}


{$IFDEF MFP_IID}
  LPIID = ^PIID;
  {$EXTERNALSYM LPIID}
  PIID = ^IID;
  {$EXTERNALSYM PIID}
  IID = TGUID;
  {$EXTERNALSYM IID}
{$ENDIF}


{$IFDEF MFP_CLSID}
  LPCLSID = ^PCLSID;
  {$EXTERNALSYM LPCLSID}
  PCLSID = ^CLSID;
  {$EXTERNALSYM PCLSID}
  CLSID = IID;
  {$EXTERNALSYM CLSID}
  TClsidArray = array of CLSID;
{$ENDIF}


{$IFDEF MFP_FMTID}
  LPFMTID = ^PFMTID;
  PFMTID = ^FMTID;
  FMTID = IID;
{$ENDIF}


{$IFDEF MFP_REFGUID}
  REFGUID = TGUID;
  {$EXTERNALSYM REFGUID}
{$ENDIF}


{$IFDEF MFP_REFIID}
  REFIID = IID;
  {$EXTERNALSYM REFIID}
{$ENDIF}


{$IFDEF MFP_REFCLSID}
  REFCLSID = IID;
  {$EXTERNALSYM REFCLSID}
{$ENDIF}


{$IFDEF MFP_REFFMTID}
  REFFMTID = IID;
  {$EXTERNALSYM REFFMTID}
{$ENDIF}


{$IFDEF MFP_UUID}
  UUID = IID;
{$ENDIF}

// =============================================================================


// =============================================================================
// Source: WTypesbase.h
// Microsoft Windows
// Copyright (c) Microsoft Corporation. All rights reserved
//==============================================================================

type

  //
  // undefine those in MfPack.inc, if you have compiler trouble
  //

{$IFDEF MFP_Hyper}
  PHyper = ^Hyper;
  {$IF COMPILERVERSION >= 11.0}
    Hyper = System.Int64;  // Signed or...
    {$EXTERNALSYM Hyper}
  {$ELSE}
    Hyper = System.Uint64; // unsigned 64 bit
    {$EXTERNALSYM Hyper}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_OLECHAR}
  POLECHAR = ^OLECHAR;
  {$IFDEF UNICODE}
  OLECHAR = WideChar;
  {$EXTERNALSYM OLECHAR}
  {$ELSE}
  OLECHAR = AnsiChar;
  {$EXTERNALSYM OLECHAR}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_POLESTR}
 PPOLESTR = ^POLESTR;
 POLESTR = PWideChar;
{$ENDIF}


{$IFDEF MFP_LPOLESTR}
  {$IFDEF UNICODE}
    LPOLESTR = ^PWideChar;
    {$EXTERNALSYM LPOLESTR}
  {$ELSE}
    LPOLESTR = ^PAnsiChar;
    {$EXTERNALSYM LPOLESTR}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_UCHAR}
  PUchar = ^UCHAR;
  UCHAR = Byte;
  {$EXTERNALSYM UCHAR}
{$ENDIF}


{$IFDEF MFP_USHORT}
  USHORT = Word;
  {$EXTERNALSYM USHORT}
  PUSHORT = ^TUshort;
  TUshort = Word;
{$ENDIF}


{$IFDEF MFP_ULONG}
  PULONG = ^ULONG;
  ULONG = DWORD;
{$ENDIF}


{$IFDEF MFP_DWORDLONG}
  PDWORDLONG = ^DWORDLONG;
  {$IF COMPILERVERSION >= 11.0}
    {$IFDEF WIN64}
      DWORDLONG = UInt64;
    {$ENDIF}
    {$IFDEF WIN32}
      DWORDLONG = Int64;
    {$ENDIF}
  {$ELSE}
    {$IFDEF WIN64}
      DWORDLONG = Int64;
    {$ENDIF}
    {$IFDEF WIN32}
      DWORDLONG = Int64;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_LONGLONG}
  PULONGLONG = ^ULONGLONG;
  PLONGLONG = ^LONGLONG;
  LONGLONG = Int64;
  {$IF COMPILERVERSION >= 11.0}
    {$IFDEF WIN64}
      ULONGLONG = UInt64;
    {$ENDIF}
    {$IFDEF WIN32}
      ULONGLONG = Int64;
    {$ENDIF}
  {$ELSE}
    {$IFDEF WIN64}
      ULONGLONG = Int64;
    {$ENDIF}
    {$IFDEF WIN32}
      ULONGLONG = Int64;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_LARGE_INTEGER}
  PLARGE_INTEGER = ^LARGE_INTEGER;
  _LARGE_INTEGER = record
  case integer of
    0: (LowPart: DWORD;
        HighPart: DWORD);
    1: (QuadPart: LONGLONG;);
  end;
  {$EXTERNALSYM _LARGE_INTEGER}
  LARGE_INTEGER = _LARGE_INTEGER;
{$ENDIF}


{$IFDEF MFP_ULARGE_INTEGER}
  PULARGE_INTEGER = ^ULARGE_INTEGER;
  ULARGE_INTEGER = record
  case integer of
    0: (LowPart: DWORD;
        HighPart: DWORD);
    1: (QuadPart: ULONGLONG);
  end;
{$ENDIF}


{$IFDEF MFP_FILETIME}
  PFILETIME = ^_FILETIME;
  LPFILETIME = ^_FILETIME;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
  FILETIME = _FILETIME;
{$ENDIF}


{$IFDEF MFP_SYSTEMTIME}
  LPSYSTEMTIME = ^PSYSTEMTIME;
  PSYSTEMTIME = ^_SYSTEMTIME;
  _SYSTEMTIME = record
    wYear: WORD;
    wMonth: WORD;
    wDayOfWeek: WORD;
    wDay: WORD;
    wHour: WORD;
    wMinute: WORD;
    wSecond: WORD;
    wMilliseconds: WORD;
  end;
  SYSTEMTIME = _SYSTEMTIME;
{$ENDIF}


{$IFDEF MFP_SECURITY_ATTRIBUTES_}
  LPSECURITY_ATTRIBUTES = ^PSECURITY_ATTRIBUTES;
  PSECURITY_ATTRIBUTES = ^_SECURITY_ATTRIBUTES;
  _SECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: Pointer;
    bInheritHandle: LongBool;
  end;
  SECURITY_ATTRIBUTES = _SECURITY_ATTRIBUTES;
{$ENDIF}


{$IFDEF MFP_SECURITY_DESCRIPTOR_REVISION}
  PSECURITY_DESCRIPTOR_REVISION = ^USHORT;
  SECURITY_DESCRIPTOR_REVISION = USHORT;
{$ENDIF}


{$IFDEF MFP_SECURITY_DESCRIPTOR_CONTROL}
  PSECURITY_DESCRIPTOR_CONTROL = ^USHORT;
  SECURITY_DESCRIPTOR_CONTROL = USHORT;
{$ENDIF}


{$IFDEF MFP_SID}
  // forward declaration for SID (see below)
  PSID_IDENTIFIER_AUTHORITY = ^_SID_IDENTIFIER_AUTHORITY;
  _SID_IDENTIFIER_AUTHORITY = record
    Value: array[0..5] of UCHAR;
  end;
  SID_IDENTIFIER_AUTHORITY = _SID_IDENTIFIER_AUTHORITY;


  PSID = ^SID; // original:  already declared as a Pointer (see above PSID)
  PISID = ^_SID;
  _SID = record
    Revision: Byte;
    SubAuthorityCount: Byte;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority: array [0..0] of ULONG;
  end;
  SID = _SID;
{$ENDIF}


{$IFDEF MFP_SID_AND_ATTRIBUTES}
  PSID_AND_ATTRIBUTES = ^_SID_AND_ATTRIBUTES;
  _SID_AND_ATTRIBUTES = record
    Sid: PSID;
    Attributes: DWORD;
  end;
  SID_AND_ATTRIBUTES = _SID_AND_ATTRIBUTES;
{$ENDIF}


{$IFDEF MFP_ACL}
  PACL = ^ACL;
  _ACL = record
    AclRevision: UCHAR;
    Sbz1: UCHAR;
    AclSize: USHORT;
    AceCount: USHORT;
    Sbz2: USHORT;
  end;
  ACL = _ACL;
{$ENDIF}


{$IFDEF MFP_SECURITY_DESCRIPTOR}
  PSECURITY_DESCRIPTOR = ^_SECURITY_DESCRIPTOR;
  _SECURITY_DESCRIPTOR = record
    Revision: UCHAR;
    Sbz1: UCHAR;
    Control: SECURITY_DESCRIPTOR_CONTROL;
    Owner: PSID;
    Group: PSID;
    Sacl: PACL;
    Dacl: PACL;
  end;
  SECURITY_DESCRIPTOR = _SECURITY_DESCRIPTOR;
{$ENDIF}


{$IFDEF MFP_COAUTHIDENTITY}
  PCOAUTHIDENTITY = ^COAUTHIDENTITY;
  _COAUTHIDENTITY = record
    User: PUSHORT;
    UserLength: ULONG;
    Domain: PUSHORT;
    DomainLength: ULONG;
    Password: PUSHORT;
    PasswordLength: ULONG;
    Flags: ULONG;
  end;
  COAUTHIDENTITY = _COAUTHIDENTITY;
{$ENDIF}


{$IFDEF MFP_COAUTHINFO}
  PCOAUTHINFO = ^COAUTHINFO;
  _COAUTHINFO = record
    dwAuthnSvc: DWORD;
    dwAuthzSvc: DWORD;
    pwszServerPrincName: PWideChar;
    dwAuthnLevel: DWORD;
    dwImpersonationLevel: DWORD;
    pAuthIdentityData: PCOAUTHIDENTITY;
    dwCapabilities: DWORD;
  end;
  COAUTHINFO = _COAUTHINFO;
{$ENDIF}


{$IFDEF MFP_SCODE}
  PSCODE = ^SCODE;
  SCODE = LongInt;
{$ENDIF}


{$IFDEF MFP_HRESULT}
  PHResult = ^HResult;
  HResult = LongInt;
{$ENDIF}


{$IFDEF MFP_MFP__OBJECTID}
  POBJECTID = ^OBJECTID;
  _OBJECTID = record
    Lineage: TGUID;
    Uniquifier: ULONG;
  end;
  OBJECTID = _OBJECTID;
{$ENDIF}


{$IFDEF MFP_MEMCTX}
  PMEMCTX = ^MEMCTX;
  tagMEMCTX          = (
    MEMCTX_TASK      = 1,
    MEMCTX_SHARED    = 2,
    MEMCTX_MACSYSTEM = 3,
    MEMCTX_UNKNOWN   = -1,
    MEMCTX_SAME      = -2);
  {$EXTERNALSYM tagMEMCTX}
  MEMCTX = tagMEMCTX;
{$ENDIF}


{$IFDEF MFP_ROTREGFLAGS}
const
  ROTREGFLAGS_ALLOWANYCLIENT          = $1;
{$ENDIF}


{$IFDEF MFP_APPIDREGFLAGS}
const
  APPIDREGFLAGS_ACTIVATE_IUSERVER_INDESKTOP              = $1;
  APPIDREGFLAGS_SECURE_SERVER_PROCESS_SD_AND_BIND        = $2;
  APPIDREGFLAGS_ISSUE_ACTIVATION_RPC_AT_IDENTIFY         = $4;
  APPIDREGFLAGS_IUSERVER_UNMODIFIED_LOGON_TOKEN          = $8;
  APPIDREGFLAGS_IUSERVER_SELF_SID_IN_LAUNCH_PERMISSION   = $10;
  APPIDREGFLAGS_IUSERVER_ACTIVATE_IN_CLIENT_SESSION_ONLY = $20;
  APPIDREGFLAGS_RESERVED1                                = $40;
  APPIDREGFLAGS_RESERVED2                                = $80;
  APPIDREGFLAGS_RESERVED3                                = $100;
  APPIDREGFLAGS_RESERVED4                                = $200;
  APPIDREGFLAGS_RESERVED5                                = $400;
  APPIDREGFLAGS_RESERVED6                                = $800;
  APPIDREGFLAGS_RESERVED7                                = $1000;
  APPIDREGFLAGS_RESERVED8                                = $2000;
  APPIDREGFLAGS_RESERVED9                                = $4000;
{$ENDIF}


{$IFDEF MFP_DCOMSCM}
const
  DCOMSCM_ACTIVATION_USE_ALL_AUTHNSERVICES   = $1;
  DCOMSCM_ACTIVATION_DISALLOW_UNSECURE_CALL  = $2;
  DCOMSCM_RESOLVE_USE_ALL_AUTHNSERVICES      = $4;
  DCOMSCM_RESOLVE_DISALLOW_UNSECURE_CALL     = $8;
  DCOMSCM_PING_USE_MID_AUTHNSERVICE          = $10;
  DCOMSCM_PING_DISALLOW_UNSECURE_CALL        = $20;
{$ENDIF}


{$IFDEF MFP_CLSCTX}
type
  PCLSCTX = ^CLSCTX;
  tagCLSCTX                       =  integer;
  CLSCTX = tagCLSCTX;

const
    CLSCTX_INPROC_SERVER          = $1;
    CLSCTX_INPROC_HANDLER         = $2;
    CLSCTX_LOCAL_SERVER           = $4;
    CLSCTX_INPROC_SERVER16        = $8;
    CLSCTX_REMOTE_SERVER          = $10;
    CLSCTX_INPROC_HANDLER16       = $20;
    CLSCTX_RESERVED1              = $40;
    CLSCTX_RESERVED2              = $80;
    CLSCTX_RESERVED3              = $100;
    CLSCTX_RESERVED4              = $200;
    CLSCTX_NO_CODE_DOWNLOAD       = $400;
    CLSCTX_RESERVED5              = $800;
    CLSCTX_NO_CUSTOM_MARSHAL      = $1000;
    CLSCTX_ENABLE_CODE_DOWNLOAD   = $2000;
    CLSCTX_NO_FAILURE_LOG         = $4000;
    CLSCTX_DISABLE_AAA            = $8000;
    CLSCTX_ENABLE_AAA             = $10000;
    CLSCTX_FROM_DEFAULT_CONTEXT   = $20000;
    CLSCTX_ACTIVATE_32_BIT_SERVER = $40000;
    CLSCTX_ACTIVATE_64_BIT_SERVER = $80000;
    CLSCTX_ENABLE_CLOAKING        = $100000;
    CLSCTX_APPCONTAINER           = $400000;
    CLSCTX_ACTIVATE_AAA_AS_IU     = $800000;
    CLSCTX_PS_DLL                 = integer($80000000);
{$ENDIF}


{$IFDEF MFP_X}
const
    CLSCTX_VALID_MASK  = (Ord(CLSCTX_INPROC_SERVER) OR
                          Ord(CLSCTX_INPROC_HANDLER) OR
                          Ord(CLSCTX_LOCAL_SERVER) OR
                          Ord(CLSCTX_INPROC_SERVER16) OR
                          Ord(CLSCTX_REMOTE_SERVER) OR
                          Ord(CLSCTX_NO_CODE_DOWNLOAD) OR
                          Ord(CLSCTX_NO_CUSTOM_MARSHAL) OR
                          Ord(CLSCTX_ENABLE_CODE_DOWNLOAD) OR
                          Ord(CLSCTX_NO_FAILURE_LOG) OR
                          Ord(CLSCTX_DISABLE_AAA) OR
                          Ord(CLSCTX_ENABLE_AAA) OR
                          Ord(CLSCTX_FROM_DEFAULT_CONTEXT) OR
                          Ord(CLSCTX_ACTIVATE_32_BIT_SERVER) OR
                          Ord(CLSCTX_ACTIVATE_64_BIT_SERVER) OR
                          Ord(CLSCTX_ENABLE_CLOAKING) OR
                          Ord(CLSCTX_APPCONTAINER) OR
                          Ord(CLSCTX_ACTIVATE_AAA_AS_IU) OR
                          Ord(CLSCTX_PS_DLL));
{$ENDIF}




{$IFDEF MFP_MSHLFLAGS}
type
  PMSHLFLAGS = ^MSHLFLAGS;
  tagMSHLFLAGS            = Byte;
  MSHLFLAGS = tagMSHLFLAGS;
const
    MSHLFLAGS_NORMAL      = 0;
    MSHLFLAGS_TABLESTRONG = 1;
    MSHLFLAGS_TABLEWEAK   = 2;
    MSHLFLAGS_NOPING      = 4;
    MSHLFLAGS_RESERVED1   = 8;
    MSHLFLAGS_RESERVED2   = 16;
    MSHLFLAGS_RESERVED3   = 32;
    MSHLFLAGS_RESERVED4   = 64;
{$ENDIF}


{$IFDEF MFP_MSHCTX}
type
  PMSHCTX = ^MSHCTX;
  tagMSHCTX                 = DWORD;
  MSHCTX = tagMSHCTX;
const
    MSHCTX_LOCAL            = 0;
    MSHCTX_NOSHAREDMEM      = 1;
    MSHCTX_DIFFERENTMACHINE = 2;
    MSHCTX_INPROC           = 3;
    MSHCTX_CROSSCTX         = 4;
{$ENDIF}

type

{$IFDEF MFP_BYTE_BLOB}
  PBYTE_BLOB = ^BYTE_BLOB;
  UP_BYTE_BLOB = ^BYTE_BLOB;
  _BYTE_BLOB = record
    clSize: ULONG;
    abData: array [0..0] of byte;
  end;
  BYTE_BLOB = _BYTE_BLOB;
{$ENDIF}


{$IFDEF MFP_WORD_BLOB}
  PWORD_BLOB = ^WORD_BLOB;
  UP_WORD_BLOB = ^WORD_BLOB;
  _WORD_BLOB = record
    clSize: ULONG;
    asData: array [0..0] of Word;
  end;
  WORD_BLOB = _WORD_BLOB;
{$ENDIF}


{$IFDEF MFP_DWORD_BLOB}
  PDWORD_BLOB = ^DWORD_BLOB;
  UP_DWORD_BLOB = ^DWORD_BLOB;
  _DWORD_BLOB = record
    clSize: ULONG;
    alData: array [0..0] of ULONG;
  end;
  DWORD_BLOB = _DWORD_BLOB;
{$ENDIF}


{$IFDEF MFP_FLAGGED_BYTE_BLOB}
  PFLAGGED_BYTE_BLOB = ^FLAGGED_BYTE_BLOB;
  UP_FLAGGED_BYTE_BLOB = ^FLAGGED_BYTE_BLOB;
  _FLAGGED_BYTE_BLOB = record
    fFlags: ULONG;
    clSize: ULONG;
    abData: array [0..0] of byte;
  end;
  FLAGGED_BYTE_BLOB = _FLAGGED_BYTE_BLOB;
{$ENDIF}


{$IFDEF MFP_FLAGGED_WORD_BLOB}
  PFLAGGED_WORD_BLOB = ^FLAGGED_WORD_BLOB;
  UP_FLAGGED_WORD_BLOB = ^FLAGGED_WORD_BLOB;
  _FLAGGED_WORD_BLOB = record
    fFlags: ULONG;
    clSize: ULONG;
    asData: array [0..0] of Word;
  end;
  FLAGGED_WORD_BLOB = _FLAGGED_WORD_BLOB;
{$ENDIF}


{$IFDEF MFP_BYTE_SIZEDARR}
  PBYTE_SIZEDARR = ^BYTE_SIZEDARR;
  _BYTE_SIZEDARR = record
    clSize: ULONG;
    pData: Pbyte;
  end;
  BYTE_SIZEDARR = _BYTE_SIZEDARR;
{$ENDIF}


{$IFDEF MFP_WORD_SIZEDARR}
  PWORD_SIZEDARR = ^WORD_SIZEDARR;
  _SHORT_SIZEDARR = record
    clSize: ULONG;
    pData: PWord;
  end;
  WORD_SIZEDARR = _SHORT_SIZEDARR;
{$ENDIF}


{$IFDEF MFP_DWORD_SIZEDARR}
  PDWORD_SIZEDARR = ^DWORD_SIZEDARR;
  _LONG_SIZEDARR = record
    clSize: ULONG;
    pData: PULONG;
  end;
  DWORD_SIZEDARR = _LONG_SIZEDARR;
{$ENDIF}


{$IFDEF MFP_HYPER_SIZEDARR}
  PHYPER_SIZEDARR = ^HYPER_SIZEDARR;
  _HYPER_SIZEDARR = record
    clSize: ULONG;
    pData: PHyper;
  end;
  HYPER_SIZEDARR = _HYPER_SIZEDARR;
{$ENDIF}


{$IFDEF MFP_BLOB}
  PBLOB = ^BLOB;
  LPBLOB = ^tagBLOB;
  tagBLOB = record
    cbSize: ULONG;
    pBlobData: PByte;
  end;
  BLOB = tagBLOB;
{$ENDIF}


// =============================================================================
// Source: wtypes.h
// Microsoft Windows
// Copyright (c) Microsoft Corporation. All rights reserved
//==============================================================================

// Forward Declarations

// header files for imported files
//#include "basetsd.h"
//#include "guiddef.h"

// interface IWinTypes

type

{$IFDEF MFP_RemHGLOBAL}
  PRemHGLOBAL = ^RemHGLOBAL;
  tagRemHGLOBAL = record
    fNullHGlobal: Longint;
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemHGLOBAL}
  RemHGLOBAL = tagRemHGLOBAL;
{$ENDIF}


{$IFDEF MFP_RemHMETAFILEPICT}
  PRemHMETAFILEPICT = ^RemHMETAFILEPICT;
  tagRemHMETAFILEPICT = record
    mm: Longint;
    xExt: Longint;
    yExt: Longint;
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemHMETAFILEPICT}
  RemHMETAFILEPICT = tagRemHMETAFILEPICT;
{$ENDIF}


{$IFDEF MFP_RemHENHMETAFILE}
  PRemHENHMETAFILE = ^RemHENHMETAFILE;
  tagRemHENHMETAFILE = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemHENHMETAFILE}
  RemHENHMETAFILE = tagRemHENHMETAFILE;
{$ENDIF}


{$IFDEF MFP_RemHBITMAP}
  PRemHBITMAP = ^RemHBITMAP;
  tagRemHBITMAP = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemHBITMAP}
  RemHBITMAP = tagRemHBITMAP;
{$ENDIF}


{$IFDEF MFP_RemHPALETTE}
  PRemHPALETTE = ^RemHPALETTE;
  tagRemHPALETTE = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemHPALETTE}
  RemHPALETTE = tagRemHPALETTE;
{$ENDIF}


{$IFDEF MFP_RemHBRUSH}
  PRemHBRUSH = ^RemHBRUSH;
  tagRemBRUSH = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  {$EXTERNALSYM tagRemBRUSH}
  RemHBRUSH = tagRemBRUSH;
{$ENDIF}


{$IFDEF MFP_PWORD}
  PWORD = ^WORD;
{$ENDIF}


{$IFDEF MFP_UINT}
  PUINT = ^UINT;
  UINT = LongWord;
  {$EXTERNALSYM UINT}
{$ENDIF}


{$IFDEF MFP_INT}
  PINT = ^INT;
  INT = Integer;
  {$EXTERNALSYM INT}
{$ENDIF}


{$IFDEF MFP_BOOL}
  //
  // According to MS, a bool (not in capitals) has the size of 1 byte.
  // See: https://msdn.microsoft.com/en-us/library/cc953fe1
  // A BOOL (in capitals) has a size of 4 bytes and is based on the INT (Integer or LongInt)
  // However the Delphi LongBool size is 4 bytes, and not compatible with
  // the C++ BOOL (INT). The reason is that Delphi True always represents a -1 for True,
  // (see: https://www.safaribooksonline.com/library/view/delphi-in-a/1565926595/re168.html)
  // while a C-BOOL represents 1: This will generate a "Wrong parameter" result when passing
  // a Delphi BOOL (LongBool) to a C-BOOL property or value.
  //
  // This is actually what a BOOL in Delphi should be:
  // type BOOL = (True  = INT(1),
  //              False = INT(0));
  // Unhappily Delphi don't eat this at runtime, to be more implicit: Not when you assign a value like
  // that on a property or parameter.
  //
  // To deal with earlier implementations, you could occasionally translate a BOOL (LongBool)
  // to "Abs(INT(value))" or "Abs(BOOL)" if possible.
  // Another solution is to create a new definition (must be 4 bytes!):
  // Use this type if some interface method properties or parameters, defined as BOOL behaving strange or
  // trigger the "Wrong parameter" result.
  //
  PBOOL = ^BOOL;        // Winapi.Windows
  BOOL = LongBool;      // System
  {$EXTERNALSYM BOOL}
{$ENDIF}


{$IFDEF MFP_LONG}
  PLONG = ^LONG;
  LONG = LongInt;
  {$EXTERNALSYM LONG}
{$ENDIF}


{$IFDEF MFP_WPARAM}
  PWPARAM = ^WPARAM;
  WPARAM = Cardinal;
  {$EXTERNALSYM WPARAM}
{$ENDIF}


{$IFDEF MFP_LPARAM}
  PLPARAM = ^LPARAM;
  LPARAM = LONG_PTR;
  {$EXTERNALSYM LPARAM}
{$ENDIF}


{$IFDEF MFP_LRESULT}
  PLRESULT = ^LRESULT;
  LRESULT = LONG_PTR;
  {$EXTERNALSYM LRESULT}
{$ENDIF}


{$IFDEF MFP_HSTRING}
  HSTRING = THandle;
  {$EXTERNALSYM HSTRING}
{$ENDIF}


{$IFDEF MFP_HMODULE}
  PHMODULE = ^HMODULE;
  HMODULE = THandle;
  {$EXTERNALSYM HMODULE}
{$ENDIF}


{$IFDEF MFP_HINSTANCE}
  PHINSTANCE = ^HINSTANCE;
  HINSTANCE = THandle;
  {$EXTERNALSYM HINSTANCE}
{$ENDIF}


{$IFDEF MFP_HTASK}
  PHTASK = ^HTASK;
  HTASK = THandle;
  {$EXTERNALSYM HTASK}
{$ENDIF}


{$IFDEF MFP_HKEY}
  PHKEY = ^HKEY;
  HKEY = THandle;
  {$EXTERNALSYM HKEY}
{$ENDIF}


{$IFDEF MFP_HDESK}
  PHDESK = ^HDESK;
  HDESK = THandle;
  {$EXTERNALSYM HDESK}
{$ENDIF}


{$IFDEF MFP_HMF}
  PHMF = ^HMF;
  HMF = THandle;
  {$EXTERNALSYM HMF}
{$ENDIF}


{$IFDEF MFP_HEMF}
  PHEMF = ^HEMF;
  HEMF = THandle;
  {$EXTERNALSYM HEMF}
{$ENDIF}


{$IFDEF MFP_HPEN}
  PHPEN = ^HPEN;
  HPEN = THandle;
  {$EXTERNALSYM HPEN}
{$ENDIF}


{$IFDEF MFP_HRSRC}
  PHRSRC = ^HRSRC;
  HRSRC = THandle;
  {$EXTERNALSYM HRSRC}
{$ENDIF}


{$IFDEF MFP_HSTR}
  PHSTR = ^HSTR;
  HSTR = THandle;
  {$EXTERNALSYM HSTR}
{$ENDIF}


{$IFDEF MFP_HWINSTA}
  PHWINSTA = ^HWINSTA;
  HWINSTA = THandle;
  {$EXTERNALSYM HWINSTA}
{$ENDIF}


{$IFDEF MFP_HKL}
  PHkl = ^Hkl;
  HKL = THandle;
  {$EXTERNALSYM HKL}
{$ENDIF}


{$IFDEF MFP_HGDIOBJ}
  PHGDIOBJ = ^HGDIOBJ;
  HGDIOBJ = THandle;
  {$EXTERNALSYM HGDIOBJ}
{$ENDIF}


{$IFDEF MFP_HDWP}
  PHDWP = ^HDWP;
  HDWP = THandle;
  {$EXTERNALSYM HDWP}
{$ENDIF}


{$IFDEF MFP_HFILE}
  PHfile = ^THfile;
  HFILE = INT;
  {$EXTERNALSYM HFILE}
  THfile = INT;
{$ENDIF}


{$IFDEF MFP_LPWORD}
  LPWORD = ^WORD;
  {$EXTERNALSYM LPWORD}
{$ENDIF}


{$IFDEF MFP_LPDWORD}
  LPDWORD = PDWORD;
  {$EXTERNALSYM LPDWORD}
{$ENDIF}


{$IFDEF MFP_PCHAR}
  PCHAR = PWideChar;
  {$EXTERNALSYM PCHAR}
{$ENDIF}


{$IFDEF MFP_LPSTR}
  LPSTR = PAnsiChar;
  {$EXTERNALSYM LPSTR}
{$ENDIF}


{$IFDEF MFP_WCHAR}
  {$IFDEF UNICODE}
    WCHAR = WideChar;
    {$EXTERNALSYM WCHAR}
    PWCHAR = PWideChar;
    {$EXTERNALSYM PWCHAR}
  {$ELSE}
    WCHAR = AnsiChar;
    {$EXTERNALSYM WCHAR}
    PWCHAR = PAnsiChar;
    {$EXTERNALSYM PWCHAR}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_LPCSTR}
{$IFDEF UNICODE}
  LPCSTR = PWideChar;
  {$EXTERNALSYM LPCSTR}
{$ELSE}
  LPCSTR = PAnsiChar;
  {$EXTERNALSYM LPCSTR}
{$ENDIF}
{$ENDIF}


{$IFDEF MFP_LPTSTR}
{$IFDEF UNICODE}
    LPTSTR = PWideChar;  // should be PAnsiChar if NOT UNICODE
    {$EXTERNALSYM LPTSTR}
{$ELSE}
    LPTSTR = PAnsiChar;  // should be PWideChar if UNICODE
    {$EXTERNALSYM LPTSTR}
{$ENDIF}
{$ENDIF}


{$IFDEF MFP_wchar_t}
  Pwchar_t = ^wchar_t;
  PTCHAR = ^TTCHAR;
  {$IF COMPILERVERSION >= 20.0}
    wchar_t = WideChar;
    {$EXTERNALSYM wchar_t}
    TTCHAR = WideChar;
    {$EXTERNALSYM TTCHAR}
  {$ELSE}
    wchar_t = AnsiChar;
    {$EXTERNALSYM wchar_t}
    TTCHAR = AnsiChar;
    {$EXTERNALSYM TTCHAR}
  {$ENDIF}
{$ENDIF}


  // WideChar
  // Those are defined in winnt.h, but for our convenience we do it here to keep things simple.


{$IFDEF MFP_LPWSTR}
  PWSTR = PWideChar;
  {$EXTERNALSYM PWSTR}
  PLPWSTR = ^LPWSTR;
  LPWSTR = PWSTR;
  {$EXTERNALSYM LPWSTR}
{$ENDIF}


{$IFDEF MFP_LPCWSTR}
  PCWSTR = WideChar;
  {$EXTERNALSYM PCWSTR}
  LPCWSTR = PWideChar;
  {$EXTERNALSYM LPCWSTR}
{$ENDIF}


{$IFDEF MFP_LPCTSTR}
  {$IFDEF UNICODE}
    LPCTSTR = PWideChar; // should be PAnsiChar if NOT UNICODE
    {$EXTERNALSYM LPCTSTR}
  {$ELSE}
    LPCTSTR = PAnsiChar; // should be PWideChar if UNICODE
    {$EXTERNALSYM LPCTSTR}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_COLORREF}
  PCOLORREF = ^COLORREF;
  LPCOLORREF = ^COLORREF;
  COLORREF = DWORD;
  {$EXTERNALSYM COLORREF}
{$ENDIF}


{$IFDEF MFP_LPHANDLE}
  LPHANDLE = ^THandle;
{$ENDIF}


{$IFDEF MFP_POINT}
  PPOINT = ^tagPOINT;
  LPPOINT = ^tagPOINT;
  tagPOINT = record
    x: LongInt;
    y: LongInt;
  end;
  {$EXTERNALSYM tagPOINT}
  POINT = tagPOINT;
{$ENDIF}


{$IFDEF MFP_POINTL}
  PPOINTL = ^_POINTL;
  _POINTL = record
    x: LongInt;
    y: LongInt;
  end;
  {$EXTERNALSYM _POINTL}
  POINTL = _POINTL;
{$ENDIF}


// = System.Types.TSize !
{$IFDEF MFP_SIZE}
  LPSIZE = ^tagSIZE;
  PSIZE = ^tagSIZE;
  tagSIZE = record
    cx: Longint;
    cy: Longint;
  end;
  {$EXTERNALSYM tagSIZE}
  SIZE = tagSIZE;
{$ENDIF}


{$IFDEF MFP_SIZEL}
  PSIZEL = ^tagSIZEL;
  LPSIZEL = ^tagSIZEL;
  tagSIZEL = record
    cx: LongInt;
    cy: LongInt;
  end;
  {$EXTERNALSYM tagSIZEL}
  SIZEL = tagSIZEL;
  TSizeL = tagSIZEL;
{$ENDIF}


{$IFDEF MFP_PALETTEENTRY}
  PPALETTEENTRY = ^tagPALETTEENTRY;
  LPPALETTEENTRY = ^tagPALETTEENTRY;
  tagPALETTEENTRY = record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
  {$EXTERNALSYM tagPALETTEENTRY}
  PALETTEENTRY = tagPALETTEENTRY;
{$ENDIF}


{$IFDEF MFP_LOGPALETTE}
  PLOGPALETTE = ^tagLOGPALETTE;
  LPLOGPALETTE = ^tagLOGPALETTE;
  tagLOGPALETTE = record
    palVersion: WORD;
    palNumEntries: WORD;
    palPalEntry: array [0..0] of PALETTEENTRY;
  end;
  {$EXTERNALSYM tagLOGPALETTE}
  LOGPALETTE = tagLOGPALETTE;
{$ENDIF}


{$IFNDEF MFVideoNormalizedRect}
  PMFVideoNormalizedRect = ^TRectF;
  MFVideoNormalizedRect = TRectF;
  {$EXTERNALSYM MFVideoNormalizedRect}
{$DEFINE MFVideoNormalizedRect}
{$ENDIF}


// Identical to TRect
{$IFDEF MFP_RECTL}
  PRECTL = ^TRect;
  LPRECTL = ^TRect;
  RECTL = TRect;
  {$EXTERNALSYM RECTL}
{$ENDIF}


{$IFDEF MFP_LPCRECT}
  LPCRECT = TRect;
  {$EXTERNALSYM LPCRECT}
  LPCRECTL = TRect;
  {$EXTERNALSYM LPCRECTL}
{$ENDIF}


{$IFDEF MFP_SHORT}
  PSHORT = PSmallInt;
  SHORT = SmallInt;
  {$EXTERNALSYM SHORT}
{$ENDIF}


{$IFDEF MFP_CSHORT}
  CShort = SmallInt;
  {$EXTERNALSYM CShort}
{$ENDIF}

// This structure is used by console functions to specify rectangular areas of console screen buffers,
// where the coordinates specify the rows and columns of screen-buffer character cells.
{$IFDEF MFP_SMALL_RECT}
  PSMALL_RECT = ^_SMALL_RECT;
  _SMALL_RECT = record
    Left: Smallint;
    Top: Smallint;
    Right: Smallint;
    Bottom: Smallint;
  end;
  {$EXTERNALSYM _SMALL_RECT}
  SMALL_RECT = _SMALL_RECT;
{$ENDIF}


{$IFDEF MFP_ROTFLAGS}
const
  ROTFLAGS_REGISTRATIONKEEPSALIVE     = $1;
  {$EXTERNALSYM ROTFLAGS_REGISTRATIONKEEPSALIVE}
  ROTFLAGS_ALLOWANYCLIENT             = $2;
  {$EXTERNALSYM ROTFLAGS_ALLOWANYCLIENT}
{$ENDIF}


{$IFDEF MFP_X}
const
  ROT_COMPARE_MAX                     = 2048;
  {$EXTERNALSYM ROT_COMPARE_MAX}
{$ENDIF}

type

{$IFDEF MFP_DVASPECT}
  PDVASPECT = ^DVASPECT;
  tagDVASPECT          = (
    DVASPECT_CONTENT   = 1,
    DVASPECT_THUMBNAIL = 2,
    DVASPECT_ICON      = 4,
    DVASPECT_DOCPRINT  = 8);
  {$EXTERNALSYM tagDVASPECT}
  DVASPECT = tagDVASPECT;
{$ENDIF}


{$IFDEF MFP_STGC}
  PSTGC = ^STGC;
  tagSTGC                                   = (
    STGC_DEFAULT                            = 0,
    STGC_OVERWRITE                          = 1,
    STGC_ONLYIFCURRENT                      = 2,
    STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4,
    STGC_CONSOLIDATE                        = 8);
  {$EXTERNALSYM tagSTGC}
  STGC = tagSTGC;
{$ENDIF}


{$IFDEF MFP_STGMOVE}
  PSTGMOVE = ^STGMOVE;
  tagSTGMOVE            = (
    STGMOVE_MOVE        = 0,
    STGMOVE_COPY        = 1,
    STGMOVE_SHALLOWCOPY = 2);
  {$EXTERNALSYM tagSTGMOVE}
  STGMOVE = tagSTGMOVE;
{$ENDIF}


{$IFDEF MFP_STATFLAG}
  PSTATFLAG = ^STATFLAG;
  tagSTATFLAG        = (
    STATFLAG_DEFAULT = 0,
    STATFLAG_NONAME  = 1,
    STATFLAG_NOOPEN  = 2);
  {$EXTERNALSYM tagSTATFLAG}
  STATFLAG = tagSTATFLAG;
  {$EXTERNALSYM STATFLAG}
{$ENDIF}

  // context_handle

{$IFDEF MFP_HCONTEXT}
  PHCONTEXT = ^HCONTEXT;
  HCONTEXT = Pointer;
  {$EXTERNALSYM HCONTEXT}
{$ENDIF}


{$IFDEF MFP_LCID}
  PLCIDd = ^LCID;
  LCID = DWORD;
  {$EXTERNALSYM LCID}
{$ENDIF}


{$IFDEF MFP_LANGID}
  PLANGID = ^LANGID;
  LANGID = USHORT;
  {$EXTERNALSYM LANGID}
{$ENDIF}


{$IFDEF MFP_WDT}
const
  WDT_INPROC_CALL                     = ($48746457);
  {$EXTERNALSYM WDT_INPROC_CALL}
  WDT_REMOTE_CALL                     = ($52746457);
  {$EXTERNALSYM WDT_REMOTE_CALL}
  WDT_INPROC64_CALL                   = ($50746457);
  {$EXTERNALSYM WDT_INPROC64_CALL}
{$ENDIF}

type

{$IFDEF MFP_userCLIPFORMAT}
  wireCLIPFORMAT = ^userCLIPFORMAT;
  _userCLIPFORMAT = record
  fContext: LongInt;
    case integer of
      0: (dwValue: DWORD;);
      1: (pwszName: PWideChar;);
    end;
  {$EXTERNALSYM _userCLIPFORMAT}
  userCLIPFORMAT = _userCLIPFORMAT;
  {$EXTERNALSYM userCLIPFORMAT}
{$ENDIF}


{$IFDEF MFP_CLIPFORMAT}
  PCLIPFORMAT = ^CLIPFORMAT;
  CLIPFORMAT = WORD;
  {$EXTERNALSYM CLIPFORMAT}
{$ENDIF}


{$IFDEF MFP_GDI_NONREMOTE}
  _GDI_NONREMOTE = record
    fContext: LongInt;
    case integer of
      0: (hInproc: LongInt;);
      1: (hRemote: PDWORD_BLOB;);
    end;
  {$EXTERNALSYM _GDI_NONREMOTE}
  GDI_NONREMOTE = _GDI_NONREMOTE;
  {$EXTERNALSYM GDI_NONREMOTE}
{$ENDIF}


{$IFDEF MFP_userHGLOBAL}
  wireHGLOBAL = ^userHGLOBAL;
  _userHGLOBAL = record
    fContext: LongInt;
    case Integer of
      0: (hInproc: LongInt;);
      1: (hRemote: PFLAGGED_BYTE_BLOB;);
      2: (hInproc64: Int64;);
    end;
  {$EXTERNALSYM _userHGLOBAL}
  userHGLOBAL = _userHGLOBAL;
  {$EXTERNALSYM userHGLOBAL}
{$ENDIF}


{$IFDEF MFP_userHMETAFILE}
  PuserHMETAFILE = ^userHMETAFILE;
  _userHMETAFILE = record
    fContext: LongInt;
    case integer of
      0: (hInproc: LongInt;);
      1: (hRemote: PBYTE_BLOB;);
      2: (hInproc64: Int64;);
    end;
  {$EXTERNALSYM _userHMETAFILE}
  userHMETAFILE = _userHMETAFILE;
  {$EXTERNALSYM userHMETAFILE}
{$ENDIF}


{$IFDEF MFP_remoteMETAFILEPICT}
  PRemoteMETAFILEPICT = ^remoteMETAFILEPICT;
  _remoteMETAFILEPICT = record
    mm: LongInt;
    xExt: LongInt;
    yExt: LongInt;
    hMF: PuserHMETAFILE;
  end;
  {$EXTERNALSYM _remoteMETAFILEPICT}
  remoteMETAFILEPICT = _remoteMETAFILEPICT;
  {$EXTERNALSYM remoteMETAFILEPICT}
{$ENDIF}


{$IFDEF MFP_userHMETAFILEPICT}
  _userHMETAFILEPICT = record
    fContext: LongInt;
    case Integer of
      0: (hInproc: LongInt;);
      1: (hRemote: PremoteMETAFILEPICT;);
      2: (hInproc64: Int64;);
    end;
  {$EXTERNALSYM _userHMETAFILEPICT}
  userHMETAFILEPICT = _userHMETAFILEPICT;
  {$EXTERNALSYM userHMETAFILEPICT}
{$ENDIF}


{$IFDEF MFP_userHENHMETAFILE}
  _userHENHMETAFILE = record
    fContext: LongInt;
    case integer of
      0: (hInproc: LongInt; );
      1: (hRemote: PBYTE_BLOB; );
      2: (hInproc64: Int64; );
    end;
  {$EXTERNALSYM _userHENHMETAFILE}
  userHENHMETAFILE = _userHENHMETAFILE;
  {$EXTERNALSYM userHENHMETAFILE}
{$ENDIF}


{$IFDEF MFP_userBITMAP}
  PuserBITMAP = ^userBITMAP;
  _userBITMAP = record
    bmType: LongInt;
    bmWidth: LongInt;
    bmHeight: LongInt;
    bmWidthBytes: LongInt;
    bmPlanes: LongInt;
    bmBitsPixel: WORD;
    cbSize: ULONG;
    pBuffer: array [0..0] of Byte;
  end;
  {$EXTERNALSYM _userBITMAP}
  userBITMAP = _userBITMAP;
  {$EXTERNALSYM userBITMAP}
{$ENDIF}


{$IFDEF MFP_userHBITMAP}
  PuserHBITMAP = ^userHBITMAP;
  _userHBITMAP = record
  fContext: LongInt;
    case integer of
      0: (hInproc: LongInt;);
      1: (hRemote: PuserBITMAP;);
      2: (hInproc64: Int64;);
    end;
  {$EXTERNALSYM _userHBITMAP}
  userHBITMAP = _userHBITMAP;
  {$EXTERNALSYM userHBITMAP}
{$ENDIF}


{$IFDEF MFP_userHPALETTE}
  PuserHPALETTE = ^userHPALETTE;
  _userHPALETTE = record
  fContext: LongInt;
    case integer of
      0: (hInproc: LongInt; );
      1: (hRemote: PLOGPALETTE; );
      2: (hInproc64: Int64; );
    end;
  {$EXTERNALSYM _userHPALETTE}
  userHPALETTE = _userHPALETTE;
  {$EXTERNALSYM userHPALETTE}
{$ENDIF}


{$IFDEF MFP_RemotableHandle}
  wireHWND = ^RemotableHandle;
  wireHMENU = ^RemotableHandle;
  wireHACCEL = ^RemotableHandle;
  wireHBRUSH = ^RemotableHandle;
  wireHFONT = ^RemotableHandle;
  wireHDC = ^RemotableHandle;
  wireHICON = ^RemotableHandle;
  wireHRGN = ^RemotableHandle;
  wireHMONITOR = ^RemotableHandle;

  PRemotableHandle = ^RemotableHandle;
  _RemotableHandle = record
  fContext: LongInt;
    case integer of
      0: (hInproc: LongInt; );
      1: (hRemote: LongInt; );
    end;
  {$EXTERNALSYM _RemotableHandle}
  RemotableHandle = _RemotableHandle;
  {$EXTERNALSYM RemotableHandle}
{$ENDIF}


{$IFDEF MFP_PHWND}
  PHWND = ^HWND;
{$ENDIF}

// no re-definitions for those needed
/////////////////////////////////////
//
//  HWND =
//
//  HMENU = Pointer;
//  PHMENU = ^HMENU;
//
//  HACCEL = Pointer;
//  PHACCEL = ^HACCEL;
//
//  HBRUSH = Pointer;
//  PHBRUSH = ^HBRUSH;
//
//  HFONT = Pointer;
//  PHFONT = ^HFONT;
//
//  HDC = Pointer;
//  PHDC = ^HDC;
//
//  HICON = Pointer;
//  PHICON = ^HICON;
//
//  HRGN = Pointer;
//  PHRGN = ^HRGN;
//
//  HMONITOR = Pointer;
//  PHMONITORr = ^HMONITOR;
//
//{$ifndef _HCURSOR_DEFINED}
//{$define _HCURSOR_DEFINED}
//  HCURSOR = HICON;
//  PHCURSOR = ^HCURSOR;
//{$endif} // !_HCURSOR_DEFINED
//
/////////////////////////////////////


{$IFDEF MFP_TEXTMETRICW}
  PTEXTMETRICW = ^TEXTMETRICW;
  LPTEXTMETRICW = ^tagTEXTMETRICW;
  tagTEXTMETRICW = record
    tmHeight: LongInt;
    tmAscent: LongInt;
    tmDescent: LongInt;
    tmInternalLeading: LongInt;
    tmExternalLeading: LongInt;
    tmAveCharWidth: LongInt;
    tmMaxCharWidth: LongInt;
    tmWeight: LongInt;
    tmOverhang: LongInt;
    tmDigitizedAspectX: LongInt;
    tmDigitizedAspectY: LongInt;
    tmFirstChar: WideChar;
    tmLastChar: WideChar;
    tmDefaultChar: WideChar;
    tmBreakChar: WideChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;
  {$EXTERNALSYM tagTEXTMETRICW}
  TEXTMETRICW = tagTEXTMETRICW;
  {$EXTERNALSYM TEXTMETRICW}
{$ENDIF}


{$IFDEF MFP_wireHBITMAP}
  wireHBITMAP = ^userHBITMAP;
{$ENDIF}


{$IFDEF MFP_wireHPALETTE}
  wireHPALETTE = ^userHPALETTE;
{$ENDIF}


{$IFDEF MFP_wireHENHMETAFILE}
  wireHENHMETAFILE = ^userHENHMETAFILE;
{$ENDIF}


{$IFDEF MFP_wireHMETAFILE}
  wireHMETAFILE = ^userHMETAFILE;
{$ENDIF}


{$IFDEF MFP_wireHMETAFILEPICT}
  wireHMETAFILEPICT = ^userHMETAFILEPICT;
{$ENDIF}
// no need for those re-definitions
///////////////////////////////////
//
//  HGLOBAL = Pointer;
//  PHGLOBAL = ^HGLOBAL;
//
//  HLOCAL = HGLOBAL;
//  PHLOCAL = ^HLOCAL;
//
//  HBITMAP = Pointer;
//  PHBITMAP = ^HBITMAP;
//
//  HPALETTE = Pointer;
//  PHPALETTE = ^HPALETTE;
//
//  HENHMETAFILE = Pointer;
//  PHENHMETAFILE = ^HENHMETAFILE;
//
//  HMETAFILE = Pointer;
//  PHMETAFILE = ^HMETAFILE;
//////////////////////////////////

{$IFDEF MFP_HMETAFILEPICT}
  PHMETAFILEPICT = ^HMETAFILEPICT;
  HMETAFILEPICT = Pointer;
  {$EXTERNALSYM HMETAFILEPICT}
{$ENDIF}

//  PDATE = ^DATE;
//  DATE = Double;


{$IFDEF MFP_CY}
  PCy = ^CY;
  LPCY = ^CY;
  tagCY = record
  case integer of
    0: (Lo: ULONG; Hi: LongInt);
    1: (iInt64: LONGLONG);
  end;
  {$EXTERNALSYM tagCY}
  CY = tagCY;
  {$EXTERNALSYM CY}
{$ENDIF}


{$IFDEF MFP_DECIMAL}
  PDECIMAL = ^DECIMAL;
  tagDEC = record
    wReserved: WORD;
    case integer of
      0: (scale: Byte;
          sign: Byte;
          Hi32: DWORD;
      case integer of
        0: (Lo32: DWORD;
            Mid32: DWORD);
        1: (Lo64: LONGLONG));
      1: (signscale: WORD);
  end;
  {$EXTERNALSYM tagDEC}
  DECIMAL = tagDEC;
  {$EXTERNALSYM DECIMAL}
{$ENDIF}


{$IFDEF MFP_DECIMAL_NEG}
const
  DECIMAL_NEG = Byte($80);
  {$EXTERNALSYM DECIMAL_NEG}
{$ENDIF}

  //procedure DECIMAL_SETZERO(var dec: DECIMAL); Moved to MfpUtils.


type
{$IFDEF MFP_LPDECIMAL}
  LPDECIMAL = ^DECIMAL;
  {$EXTERNALSYM LPDECIMAL}
{$ENDIF}


{$IFDEF MFP_wireBSTR}
  PwireBSTR = ^wireBSTR;
  wireBSTR = ^FLAGGED_WORD_BLOB;
  {$EXTERNALSYM wireBSTR}
{$ENDIF}


{$IFDEF MFP_BSTR}
  LPBSTR = ^BSTR;
  {$EXTERNALSYM LPBSTR}
  BSTR = ^OLECHAR;
  {$EXTERNALSYM BSTR}
  PBSTR = ^BSTR;
  {$EXTERNALSYM PBSTR}
{$ENDIF}


{$IFDEF MFP_VARIANT_BOOL}
  PVARIANT_BOOL = ^VARIANT_BOOL;
  VARIANT_BOOL = WordBool;  // 0 = FALSE, -1 = TRUE
  {$EXTERNALSYM VARIANT_BOOL}
  _VARIANT_BOOL = VARIANT_BOOL;
  {$EXTERNALSYM _VARIANT_BOOL}
{$ENDIF}


// The BSTRBLOB structure is used by some implementations
// of the IPropertyStorage interface when marshaling BSTRs
// on systems which don't support BSTR marshaling.
{$IFDEF MFP_BSTRBLOB}
  PBSTRBLOB = ^BSTRBLOB;
  LPBSTRBLOB = ^tagBSTRBLOB;
  {$EXTERNALSYM LPBSTRBLOB}
  tagBSTRBLOB = record
    cbSize: ULONG;
    pData: PByte;
  end;
  {$EXTERNALSYM tagBSTRBLOB}
  BSTRBLOB = tagBSTRBLOB;
  {$EXTERNALSYM BSTRBLOB}
{$ENDIF}


{$IFDEF MFP_VARIANT_TRUE_FALSE}
const
  VARIANT_TRUE  = VARIANT_BOOL(-1);
  {$EXTERNALSYM VARIANT_TRUE}
  VARIANT_FALSE = VARIANT_BOOL(0);
  {$EXTERNALSYM VARIANT_FALSE}
{$ENDIF}


{$IFDEF MFP_CLIPDATA}
type
  pClipData = ^CLIPDATA;
  tagCLIPDATA = record
    cbSize: ULONG;
    ulClipFmt: LongInt;
    pbClipData: PByte;
  end;
  {$EXTERNALSYM tagCLIPDATA}
  CLIPDATA = tagCLIPDATA;
  {$EXTERNALSYM CLIPDATA}
{$ENDIF}


{$IFDEF MFP_VARTYPE}
type
  PVARTYPE = ^VARTYPE;
  VARTYPE = WORD;
  {$EXTERNALSYM VARTYPE}
{$ENDIF}
//
// VARENUM usage key,
//
// [V] - may appear in a VARIANT
// [T] - may appear in a TYPEDESC
// [P] - may appear in an OLE property set
// [S] - may appear in a Safe Array
//
//
//   VT_EMPTY            [V]   [P]     nothing
//   VT_NULL             [V]   [P]     SQL style Null
//   VT_I2               [V][T][P][S]  2 byte signed int
//   VT_I4               [V][T][P][S]  4 byte signed int
//   VT_R4               [V][T][P][S]  4 byte real
//   VT_R8               [V][T][P][S]  8 byte real
//   VT_CY               [V][T][P][S]  currency
//   VT_DATE             [V][T][P][S]  date
//   VT_BSTR             [V][T][P][S]  OLE Automation string
//   VT_DISPATCH         [V][T]   [S]  IDispatch *
//   VT_ERROR            [V][T][P][S]  SCODE
//   VT_BOOL             [V][T][P][S]  True=-1, False=0
//   VT_VARIANT          [V][T][P][S]  VARIANT *
//   VT_UNKNOWN          [V][T]   [S]  IUnknown *
//   VT_DECIMAL          [V][T]   [S]  16 byte fixed point
//   VT_RECORD           [V]   [P][S]  user defined type
//   VT_I1               [V][T][P][s]  signed char
//   VT_UI1              [V][T][P][S]  unsigned char
//   VT_UI2              [V][T][P][S]  unsigned short
//   VT_UI4              [V][T][P][S]  unsigned long
//   VT_I8                  [T][P]     signed 64-bit int
//   VT_UI8                 [T][P]     unsigned 64-bit int
//   VT_INT              [V][T][P][S]  signed machine int
//   VT_UINT             [V][T]   [S]  unsigned machine int
//   VT_INT_PTR             [T]        signed machine register size width
//   VT_UINT_PTR            [T]        unsigned machine register size width
//   VT_VOID                [T]        C style void
//   VT_HRESULT             [T]        Standard return type
//   VT_PTR                 [T]        pointer type
//   VT_SAFEARRAY           [T]        (use VT_ARRAY in VARIANT)
//   VT_CARRAY              [T]        C style array
//   VT_USERDEFINED         [T]        user defined type
//   VT_LPSTR               [T][P]     null terminated string
//   VT_LPWSTR              [T][P]     wide null terminated string
//   VT_FILETIME               [P]     FILETIME
//   VT_BLOB                   [P]     Length prefixed bytes
//   VT_STREAM                 [P]     Name of the stream follows
//   VT_STORAGE                [P]     Name of the storage follows
//   VT_STREAMED_OBJECT        [P]     Stream contains an object
//   VT_STORED_OBJECT          [P]     Storage contains an object
//   VT_VERSIONED_STREAM       [P]     Stream with a GUID version
//   VT_BLOB_OBJECT            [P]     Blob contains an object
//   VT_CF                     [P]     Clipboard format
//   VT_CLSID                  [P]     A Class ID
//   VT_VECTOR                 [P]     simple counted array
//   VT_ARRAY            [V]           SAFEARRAY*
//   VT_BYREF            [V]           void* for local use
//   VT_BSTR_BLOB                      Reserved for system use
///////////////////////////////////////////////////////////////////////////////

{$IFDEF MFP_VARENUM}
const
// VARENUM      See: MfPack.Ks.pas for the enumeration
//////////////////////////////////////////////////////

    VT_EMPTY            = $0000;  // Not specified. 0
    {$EXTERNALSYM VT_EMPTY}
    VT_NULL             = $0001;  // Nil. 1
    {$EXTERNALSYM VT_NULL}
    VT_I2               = $0002;  // A 2-byte integer. 2
    {$EXTERNALSYM VT_I2}
    VT_I4               = $0003;  // A 4-byte integer. 3
    {$EXTERNALSYM VT_I4}
    VT_R4               = $0004;  // A 4-byte real. 4
    {$EXTERNALSYM VT_R4}
    VT_R8               = $0005;  // An 8-byte real. 5
    {$EXTERNALSYM VT_R8}
    VT_CY               = $0006;  // Currency. 6
    {$EXTERNALSYM VT_CY}
    VT_DATE             = $0007;  // A date. 7
    {$EXTERNALSYM VT_DATE}
    VT_BSTR             = $0008;  // A string. 8
    {$EXTERNALSYM VT_BSTR}
    VT_DISPATCH         = $0009;  // An IDispatch pointer. 9
    {$EXTERNALSYM VT_DISPATCH}
    VT_ERROR            = $000A;  // An SCODE value. 10
    {$EXTERNALSYM VT_ERROR}
    VT_BOOL             = $000B;  // A Boolean value. True is -1 and false is 0. 11
    {$EXTERNALSYM VT_BOOL}
    VT_VARIANT          = $000C;  // A variant pointer. 12
    {$EXTERNALSYM VT_VARIANT}
    VT_UNKNOWN          = $000D;  // An IUnknown pointer. 13
    {$EXTERNALSYM VT_UNKNOWN}
    VT_DECIMAL          = $000E;  // A 16-byte fixed-pointer value. 14
    {$EXTERNALSYM VT_DECIMAL}
    //
    VT_I1               = $0010;  // A character. 16
    {$EXTERNALSYM VT_I1}
    VT_UI1              = $0011;  // An unsigned character. 17
    {$EXTERNALSYM VT_UI1}
    VT_UI2              = $0012;  // An unsigned short. 18
    {$EXTERNALSYM VT_UI2}
    VT_UI4              = $0013;  // An unsigned long. 19
    {$EXTERNALSYM VT_UI4}
    VT_I8               = $0014;  // A 64-bit integer. 20
    {$EXTERNALSYM VT_I8}
    VT_UI8              = $0015;  // A 64-bit unsigned integer. 21
    {$EXTERNALSYM VT_UI8}
    VT_INT              = $0016;  // An integer. 22
    {$EXTERNALSYM VT_INT}
    VT_UINT             = $0017;  // An unsigned integer.23
    {$EXTERNALSYM VT_UINT}
    VT_VOID             = $0018;  // A C-style void. 24
    {$EXTERNALSYM VT_VOID}
    VT_HRESULT          = $0019;  // An HRESULT value. 25
    {$EXTERNALSYM VT_HRESULT}
    VT_PTR              = $001A;  // A pointer type. 26
    {$EXTERNALSYM VT_PTR}
    VT_SAFEARRAY        = $001B;  // A safe array. Use VT_ARRAY in VARIANT. 27
    {$EXTERNALSYM VT_SAFEARRAY}
    VT_CARRAY           = $001C;  // A C-style array. 28
    {$EXTERNALSYM VT_CARRAY}
    VT_USERDEFINED      = $001D;  // A user-defined type. 29
    {$EXTERNALSYM VT_USERDEFINED}
    VT_LPSTR            = $001E;  // A null-terminated string. 30
    {$EXTERNALSYM VT_LPSTR}
    VT_LPWSTR           = $001F;  // A wide null-terminated string. 31
    {$EXTERNALSYM VT_LPWSTR}
    VT_RECORD           = $0024;  // A user-defined type. 36
    {$EXTERNALSYM VT_RECORD}
    VT_INT_PTR          = $0025;  // A signed machine register size width. 37
    {$EXTERNALSYM VT_INT_PTR}
    VT_UINT_PTR         = $0026;  // An unsigned machine register size width. 38
    {$EXTERNALSYM VT_UINT_PTR}
    //
    VT_FILETIME         = $0040;  // A FILETIME value. 64
    {$EXTERNALSYM VT_FILETIME}
    VT_BLOB             = $0041;  // Length-prefixed bytes. 65
    {$EXTERNALSYM VT_BLOB}
    VT_STREAM           = $0042;  // The name of the stream follows. 66
    {$EXTERNALSYM VT_STREAM}
    VT_STORAGE          = $0043;  // The name of the storage follows. 67
    {$EXTERNALSYM VT_STORAGE}
    VT_STREAMED_OBJECT  = $0044;  // The stream contains an object. 68
    {$EXTERNALSYM VT_STREAMED_OBJECT}
    VT_STORED_OBJECT    = $0045;  // The storage contains an object. 69
    {$EXTERNALSYM VT_STORED_OBJECT}
    VT_BLOBOBJECT       = $0046;  // The blob contains an object. 70
    {$EXTERNALSYM VT_BLOBOBJECT}
    VT_CF               = $0047;  // A clipboard format. 71
    {$EXTERNALSYM VT_CF}
    VT_CLSID            = $0048;  // A class ID. 72
    {$EXTERNALSYM VT_CLSID}
    VT_VERSIONED_STREAM = $0049;  // A stream with a GUID version. 73
    {$EXTERNALSYM VT_VERSIONED_STREAM}
    //
    VT_VECTOR           = $1000;  // A simple counted array. $1000
    {$EXTERNALSYM VT_VECTOR}
    VT_ARRAY            = $2000;  // A SAFEARRAY pointer. $2000
    {$EXTERNALSYM VT_ARRAY}
    VT_BYREF            = $4000;  // A void pointer for local use. $4000
    {$EXTERNALSYM VT_BYREF}
    VT_RESERVED         = $8000;  // Reserved. $8000
    {$EXTERNALSYM VT_RESERVED}
    VT_BSTR_BLOB        = $0fff;  // Reserved (is reserved for system use.)
    {$EXTERNALSYM VT_BSTR_BLOB}
    VT_ILLEGAL          = $ffff;  //
    {$EXTERNALSYM VT_ILLEGAL}
    VT_ILLEGALMASKED    = $0fff;  //
    {$EXTERNALSYM VT_ILLEGALMASKED}
    VT_TYPEMASK         = $0fff;  //
    {$EXTERNALSYM VT_TYPEMASK}

    // Delphi specific ( Not OLE compatible )
    varString           = $0100; // Pascal string 256
    {$EXTERNALSYM varString}
    varAny              = $0101; // Corba any 257
    {$EXTERNALSYM varAny}
    varUString          = $0102; // Unicode string 258
    {$EXTERNALSYM varUString}

{$ENDIF}

type

  // Generic types
  // PIUnknown = ^IUnknown; > defined in MfPack.Unknwn.pas
  PHDC = ^HDC;
  {$EXTERNALSYM PHDC}

{$IFDEF MFP_FOURCC}
   FOURCC = DWORD;
   {$DEFINE FOURCC}
{$ENDIF}


{$IFDEF MFP_PROPID}
  PPROPID = ^PROPID;
  PROPID = ULONG;
  {$EXTERNALSYM PROPID}
{$ENDIF}


{$IFDEF MFP_PROPERTYKEY}
  PPROPERTYKEY = ^PROPERTYKEY;
  _tagpropertykey = record
    fmtid: TGUID;
    pid: DWORD;
  end;
  {$EXTERNALSYM _tagpropertykey}
  PROPERTYKEY = _tagpropertykey;
  {$EXTERNALSYM PROPERTYKEY}
{$ENDIF}


{$IFDEF MFP_CSPLATFORM}
  PCSPLATFORM = ^CSPLATFORM;
  tagCSPLATFORM = record
    dwPlatformId: DWORD;
    dwVersionHi: DWORD;
    dwVersionLo: DWORD;
    dwProcessorArch: DWORD;
  end;
  {$EXTERNALSYM tagCSPLATFORM}
  CSPLATFORM = tagCSPLATFORM;
  {$EXTERNALSYM CSPLATFORM}
{$ENDIF}


{$IFDEF MFP_QUERYCONTEXT}
  PQUERYCONTEXT = ^QUERYCONTEXT;
  tagQUERYCONTEXT = record
    dwContext: DWORD;
    _Platform: CSPLATFORM;
    Locale: LCID;
    dwVersionHi: DWORD;
    dwVersionLo: DWORD;
  end;
  {$EXTERNALSYM tagQUERYCONTEXT}
  QUERYCONTEXT = tagQUERYCONTEXT;
  {$EXTERNALSYM QUERYCONTEXT}
{$ENDIF}


{$IFDEF MFP_TYSPEC}
  tagTYSPEC            = (
    TYSPEC_CLSID       = 0,
    TYSPEC_FILEEXT     = (TYSPEC_CLSID + 1),
    TYSPEC_MIMETYPE    = (TYSPEC_FILEEXT + 1),
    TYSPEC_FILENAME    = (TYSPEC_MIMETYPE + 1),
    TYSPEC_PROGID      = (TYSPEC_FILENAME + 1),
    TYSPEC_PACKAGENAME = (TYSPEC_PROGID + 1),
    TYSPEC_OBJECTID    = (TYSPEC_PACKAGENAME + 1));
  {$EXTERNALSYM tagTYSPEC}
  TYSPEC = tagTYSPEC;
  {$EXTERNALSYM TYSPEC}
{$ENDIF}


{$IFDEF MFP_uCLSSPEC}
  // predefined records for uCLSSPEC
  PByObjectId = ^_ByObjectId;
  _ByObjectId = record
    ObjectId: TGUID;
    PolicyId: TGUID;
  end;
  {$EXTERNALSYM _ByObjectId}
  ByObjectId = _ByObjectId;
  {$EXTERNALSYM ByObjectId}


  PByName = ^_ByName;
  _ByName = record
    pPackageName: PWideChar;
    PolicyId: TGUID;
  end;
  {$EXTERNALSYM _ByName}
  ByName = _ByName;
  {$EXTERNALSYM ByName}

  PuCLSSPEC = ^uCLSSPEC;
  uCLSSPEC = record
    tyspec: DWORD;
    case integer of
      0: (clsid: CLSID);
      1: (pFileExt: PWideChar);
      2: (pMimeType: PWideChar);
      3: (pProgId: PWideChar);
      4: (pFileName: PWideChar);
      5: (ByObjectId: _ByObjectId);
      6: (ByName: _ByName);
    end;
  {$EXTERNALSYM uCLSSPEC}
{$ENDIF}

//==============================================================================

type

// DirectX ///////////////////////////////////////////////////////////////////


{$IFDEF MFP_X_PTRS}

  PINT_PTR   = ^INT_PTR;
  PUINT_PTR  = ^UINT_PTR;
  PLONG_PTR  = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;
  PDWORD_PTR = ^DWORD_PTR;

  {$IFDEF WIN64}
    INT_PTR    = System.Int64;
    {$EXTERNALSYM INT_PTR}
    UINT_PTR   = System.PUInt64;
    {$EXTERNALSYM UINT_PTR}
    LONG_PTR   = System.Int64;
    {$EXTERNALSYM LONG_PTR}
    ULONG_PTR  = System.UInt64;
    {$EXTERNALSYM ULONG_PTR}
    DWORD_PTR  = System.UInt64;
    {$EXTERNALSYM DWORD_PTR}
  {$ENDIF}

  {$IFDEF WIN32}
    INT_PTR    = LongInt;
    {$EXTERNALSYM INT_PTR}
    UINT_PTR   = LongWord;
    {$EXTERNALSYM UINT_PTR}
    LONG_PTR   = LongInt;
    {$EXTERNALSYM LONG_PTR}
    ULONG_PTR  = LongInt;
    {$EXTERNALSYM ULONG_PTR}
    DWORD_PTR  = LongWord;
    {$EXTERNALSYM DWORD_PTR}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_PtrInt}
  PPtrInt    = ^PtrInt;
  PPtrUInt   = ^PtrUInt;
  {$IFDEF WIN64}
    PtrInt     = System.Int64;
    {$EXTERNALSYM PtrUInt}
    PtrUInt    = System.UInt64;
    {$EXTERNALSYM PtrInt}
  {$ENDIF}

  {$IFDEF WIN32}
    PtrInt     = Longint;
    {$EXTERNALSYM PtrInt}
    PtrUInt    = Longword;
    {$EXTERNALSYM PtrUInt}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_LPCOLESTR}
  {$IF UNICODE}
    LPCOLESTR = PWideChar;
  {$ELSE}
    LPCOLESTR = PAnsiChar;
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_OLE_ENUM}
  OLE_ENUM = type LongWord;
  {$EXTERNALSYM OLE_ENUM}
{$ENDIF}


{$IFDEF MFP_OLEDATE}
  POLE_DATE = ^OLE_DATE;
  OLE_DATE = Double;
  {$EXTERNALSYM OLE_DATE}
{$ENDIF}


{$IFDEF MFP_OLEBOOL}
  POLE_BOOL = ^OLE_BOOL;
  OLE_BOOL = WordBool;
  {$EXTERNALSYM OLE_BOOL}
{$ENDIF}


  //////////////////////////////////////////////////////////////////////////////
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //////////////////////////////////////////////////////////////////////////////

{$IFDEF MFP_SIZE_T}
  PSIZE_T    = ^SIZE_T;
  SIZE_T     = ULONG_PTR;
  {$EXTERNALSYM SIZE_T}
  PSSIZE_T   = ^SSIZE_T;
  SSIZE_T    = LONG_PTR;
  {$EXTERNALSYM SSIZE_T}

  PSizeInt   = PSSIZE_T;
  SizeInt    = SSIZE_T;
  {$EXTERNALSYM SizeInt}
  PSizeUInt  = PSIZE_T;
  SizeUInt   = SIZE_T;
  {$EXTERNALSYM SizeUInt}
{$ENDIF}
// END DX


// MFPACK - shared
//----------------



{$IFDEF MFP_UINT8}
  PUINT8 = ^UINT8;
  UINT8 = Byte;
  {$EXTERNALSYM UINT8}
{$ENDIF}


{$IFDEF MFP_UINT32}
  PUINT32 = ^UINT32;
  {$IF COMPILERVERSION >= 18.0}
    UINT32 = Cardinal;
    {$EXTERNALSYM UINT32}
  {$ELSE}
    UINT32 = Integer;
    {$EXTERNALSYM UINT32}
  {$IFEND}
{$ENDIF}


{$IFDEF MFP_ULONG64}
  PULONG64 = ^ULONG64;
  {$IF COMPILERVERSION >= 11.0}
    ULONG64 = System.Uint64;
    {$EXTERNALSYM ULONG64}
  {$ELSE}
    ULONG64 = System.int64;
    {$EXTERNALSYM ULONG64}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_FLOAT}
  PFLOAT = PSingle;
  FLOAT = Single;
  {$EXTERNALSYM FLOAT}
  TFloatArray = array of FLOAT;
{$ENDIF}


{$IFDEF MFP_FLOAT32}
  PFLOAT32 = PSingle;
  {$EXTERNALSYM FLOAT32}
  TFloat32Array = array of FLOAT32;
  FLOAT32 = Single;
{$ENDIF}


{$IFDEF MFP_FLOAT64}
  PFLOAT64 = PDouble;
  FLOAT64 = Double;
  {$EXTERNALSYM FLOAT64}
  TFloat64Array = array of FLOAT64;
{$ENDIF}


{$IFNDEF MFVideoNormalizedRect}
  PMFVideoNormalizedRect = ^MFVideoNormalizedRect;
  MFVideoNormalizedRect = record
    left: FLOAT;
    top: FLOAT;
    right: FLOAT;
    bottom: FLOAT;
  end;
  {$EXTERNALSYM MFVideoNormalizedRect}
  TMFVideoNormalizedRect = MFVideoNormalizedRect;
  {$DEFINE MFVideoNormalizedRect}
{$ENDIF}


{$IFDEF MFP_LPRECT}
  LPRECT = ^TRect;
{$ENDIF}


{$IFDEF MFP_PPByte}
  PPByte = ^Byte;
{$ENDIF}


{$IFDEF MFP_SYSINT}
  SYSINT = Integer;
  {$EXTERNALSYM SYSINT}
{$ENDIF}


{$IFDEF MFP_TOPOID}
  {$IF COMPILERVERSION >= 11.0}
    TOPOID = System.Uint64;
    {$EXTERNALSYM TOPOID}
  {$ELSE}
    TOPOID = System.Int64;
    {$EXTERNALSYM TOPOID}
  {$ENDIF}
  PTOPOID = ^TOPOID;
{$ENDIF}


{$IFDEF MFP_TBool}
// BOOL is defined in WTypes
  TBool = BOOL;
  {$EXTERNALSYM TBool}
{$ENDIF}

type
{$IFDEF MFP_bstrVal}
  bstrVal = WideString;
  {$EXTERNALSYM bstrVal}
{$ENDIF}

{$IFDEF MFP_DATE}
  DATE = TDate;
  {$EXTERNALSYM DATE}
{$ENDIF}

{$IFDEF MFP_MFTIME}
  {$IF COMPILERVERSION >= 11.0}
    {$IFDEF WIN64}
      MFTIME = System.UInt64; // Time in 100 nanosecond slices
      {$EXTERNALSYM MFTIME}
    {$ENDIF}

    {$IFDEF WIN32}
      MFTIME = System.UInt64;
      {$EXTERNALSYM MFTIME}
    {$ENDIF}
  {$ELSE}
    {$IFDEF WIN64}
      MFTIME = System.UInt64;
      {$EXTERNALSYM MFTIME}
    {$ENDIF}

    {$IFDEF WIN32}
      MFTIME = System.Int64;
      {$EXTERNALSYM MFTIME}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$IFDEF MFP_HNSTIME}
  PHnstime = ^HNSTIME;
  HNSTIME = LONGLONG;
  {$EXTERNALSYM HNSTIME}
  THnstime = LONGLONG;
{$ENDIF}


{$IFDEF MFP_SHCOLSTATEF}
  PSHCOLSTATEF = ^SHCOLSTATEF;
  SHCOLSTATEF = DWORD;
  {$EXTERNALSYM SHCOLSTATEF}
{$ENDIF}


{$IFDEF MFP_POINTS}
  PPOINTS = ^POINTS;
  tagPOINTS = record
    x: SHORT;
    y: SHORT;
  end;
  {$EXTERNALSYM tagPOINTS}
  POINTS = tagPOINTS;
{$ENDIF}


  // GUID related
  // ============


{$IFDEF MFP_GUID}
  PGUID = ^GUID;
  GUID = TGUID;
  {$EXTERNALSYM GUID}
{$ENDIF}


{$IFDEF MFP_PPTGUID}
  PPTGUID = ^TGUID;
  {$EXTERNALSYM PPTGUID}
{$ENDIF}


// PROPERTYKEY related
//--------------------


{$IFDEF MFP_PIDispatch}
  PIDispatch = ^IDispatch;
{$ENDIF}


{$IFDEF MFP_MEDIATYPE_NULL}
const
  MEDIATYPE_NULL : TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  {$EXTERNALSYM MEDIATYPE_NULL}
{$ENDIF}


{$IFDEF MFP_MEDIASUBTYPE_NULL}
const
  MEDIASUBTYPE_NULL : TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  {$EXTERNALSYM MEDIASUBTYPE_NULL}
{$ENDIF}


{$IFDEF MFP_REFVARIANT}
type
  PREFVARIANT = POleVariant;
  REFVARIANT = OleVariant;  // Used in external unit PropVarUtil.pas
  {$EXTERNALSYM REFVARIANT}
{$ENDIF}


{$IFDEF MFP_LUID}
// Locally Unique Identifier
  PLUID = ^LUID;
  _LUID = record
    LowPart: DWORD;
    HighPart: INT32;
  end;
  {$EXTERNALSYM _LUID}
  LUID = _LUID;
  {$EXTERNALSYM LUID}
{$ENDIF}


  //=========== SPECIFIC PLATFORM TYPES ========================


  //Core Audio & DirectShow
  //=======================

{$IFDEF MFP_REFERENCE_TIME}
  PREFERENCE_TIME = ^REFERENCE_TIME;
  REFERENCE_TIME = LONGLONG;
  {$EXTERNALSYM REFERENCE_TIME}
  PReferenceTime = ^ReferenceTime;
  ReferenceTime = REFERENCE_TIME;
{$ENDIF}


  // DirectShow & some includefiles
  //===============================

{$IFDEF MFP_REFTIME}
  REFTIME = Double;
  {$EXTERNALSYM REFTIME}
{$ENDIF}


{$IFDEF MFP_HSEMAPHORE}
  HSEMAPHORE = Longint;
  {$EXTERNALSYM HSEMAPHORE}
{$ENDIF}


{$IFDEF MFP_HMONITOR}
  HMONITOR = THANDLE;
  {$EXTERNALSYM HMONITOR}
{$ENDIF}


{$IFDEF MFP_HEVENT}
  PHEVENT = ^HEVENT;
  HEVENT = DWORD_PTR;
  {$EXTERNALSYM HEVENT}
{$ENDIF}

{$IFDEF MFP_ANYSIZE_ARRAY}
  // Used in KsMedia
const
  ANYSIZE_ARRAY = 1;
  {$EXTERNALSYM ANYSIZE_ARRAY}
{$ENDIF}

{$IFDEF MFP_MAX_INPUT_OUTPUT}
  // used by Endpoint Volume
  //========================
  MAX_INPUT_VALUE  = High(WORD);
  {$EXTERNALSYM MAX_INPUT_VALUE}
  MIN_INPUT_VALUE  = 0;
  {$EXTERNALSYM MIN_INPUT_VALUE}
  MAX_OUTPUT_VALUE = High(WORD);
  {$EXTERNALSYM MAX_OUTPUT_VALUE}
  MIN_OUTPUT_VALUE = 0;
  {$EXTERNALSYM MIN_OUTPUT_VALUE}
{$ENDIF}




  //Media Foundation
  //----------------


  //Special types
  //-------------
{$IFDEF MFP_MAXIMUM_VOLUME_LABEL_LENGTH}
  MAXIMUM_VOLUME_LABEL_LENGTH = 32;
  {$EXTERNALSYM MAXIMUM_VOLUME_LABEL_LENGTH}
{$ENDIF}


// MfApi
{$IFDEF MFP_WORDARRAY}
type
  _WordArray = array [0..0] of Word;
  {$EXTERNALSYM _WordArray}
  TWordArray = _WordArray;
{$ENDIF}



const
  // From limits.h
  //==============
  {$IFDEF MFP_ARG_MAX}
  ARG_MAX     = (256 * 1024); // max bytes for an exec function
  {$EXTERNALSYM ARG_MAX}
  {$ENDIF}
  {$IFDEF MFP_CHILD_MAX}
  CHILD_MAX   = 266; // max simultaneous processes
  {$EXTERNALSYM CHILD_MAX}
  {$ENDIF}
  {$IFDEF MFP_LINK_MAX}
  LINK_MAX    = 32767; // max file link count
  {$EXTERNALSYM LINK_MAX}
  {$ENDIF}
  {$IFDEF MFP_MAX_CANON}
  MAX_CANON   =  1024; // max bytes in term canon input line
  {$EXTERNALSYM MAX_CANON}
  {$ENDIF}
  {$IFDEF MFP_MAX_INPUT}
  MAX_INPUT   =  1024; // max bytes in terminal input
  {$EXTERNALSYM MAX_INPUT}
  {$ENDIF}
  {$IFDEF MFP_NAME_MAX}
  NAME_MAX    =   255; // max bytes in a file name
  {$EXTERNALSYM NAME_MAX}
  {$ENDIF}
  {$IFDEF MFP_NGROUPS_MAX}
  NGROUPS_MAX =    16; // max supplemental group id's
  {$EXTERNALSYM NGROUPS_MAX}
  {$ENDIF}
  {$IFDEF MFP_GID_MAX}
  GID_MAX     = 2147483647; // max value for a gid_t (2^31-2)
  {$EXTERNALSYM GID_MAX}
  {$ENDIF}
  {$IFDEF MFP_UID_MAX}
  UID_MAX     = 2147483647; // max value for a uid_t (2^31-2)
  {$EXTERNALSYM UID_MAX}
  {$ENDIF}

  {$IFDEF MFP_OPEN_MAX}
  OPEN_MAX    = 10240; // max open files per process
  {$EXTERNALSYM OPEN_MAX}
  {$ENDIF}
  {$IFDEF MFP_PATH_MAX}
  PATH_MAX    =  1024; // max bytes in pathname
  {$EXTERNALSYM PATH_MAX}
  {$ENDIF}
  {$IFDEF MFP_PIPE_BUF}
  PIPE_BUF    =   512; // max bytes for atomic pipe writes
  {$EXTERNALSYM PIPE_BUF}
  {$ENDIF}
  {$IFDEF MFP_BC_BASE_MAX}
  BC_BASE_MAX        =   99; // max ibase/obase values in bc(1)
  {$EXTERNALSYM BC_BASE_MAX}
  {$ENDIF}
  {$IFDEF MFP_BC_DIM_MAX}
  BC_DIM_MAX         = 2048; // max array elements in bc(1)
  {$EXTERNALSYM BC_DIM_MAX}
  {$ENDIF}
  {$IFDEF MFP_BC_SCALE_MAX}
  BC_SCALE_MAX       =   99; // max scale value in bc(1)
  {$EXTERNALSYM BC_SCALE_MAX}
  {$ENDIF}
  {$IFDEF MFP_BC_STRING_MAX}
  BC_STRING_MAX      = 1000; // max const string length in bc(1)
  {$EXTERNALSYM BC_STRING_MAX}
  {$ENDIF}
  {$IFDEF MFP_CHARCLASS_NAME_MAX}
  CHARCLASS_NAME_MAX =   14; // max character class name size
  {$EXTERNALSYM CHARCLASS_NAME_MAX}
  {$ENDIF}
  {$IFDEF MFP_COLL_WEIGHTS_MAX}
  COLL_WEIGHTS_MAX   =    2; // max weights for order keyword
  {$EXTERNALSYM COLL_WEIGHTS_MAX}
  {$ENDIF}
  {$IFDEF MFP_EQUIV_CLASS_MAX}
  EQUIV_CLASS_MAX    =    2;
  {$EXTERNALSYM EQUIV_CLASS_MAX}
  {$ENDIF}
  {$IFDEF MFP_EXPR_NEST_MAX}
  EXPR_NEST_MAX      =   32; // max expressions nested in expr(1)
  {$EXTERNALSYM EXPR_NEST_MAX}
  {$ENDIF}
  {$IFDEF MFP_LINE_MAX}
  LINE_MAX           = 2048; // max bytes in an input line
  {$EXTERNALSYM LINE_MAX}
  {$ENDIF}
  {$IFDEF MFP_RE_DUP_MAX}
  RE_DUP_MAX         =  255; // max RE's in interval notation
  {$EXTERNALSYM RE_DUP_MAX}
  {$ENDIF}
  {$IFDEF MFP_NZERO}
  NZERO = 20; // default priority [XSI]
  {$EXTERNALSYM NZERO}
  {$ENDIF}
  {$IFDEF MFP_SCHAR_MAX}
  SCHAR_MAX      = 127; // min value for a signed char
  {$EXTERNALSYM SCHAR_MAX}
  {$ENDIF}
  {$IFDEF MFP_SCHAR_MIN}
  SCHAR_MIN      = -128; // max value for a signed char
  {$EXTERNALSYM SCHAR_MIN}
  {$ENDIF}
  {$IFDEF MFP_UCHAR_MAX}
  UCHAR_MAX      = 255; // max value for an unsigned char
  {$EXTERNALSYM UCHAR_MAX}
  {$ENDIF}
  {$IFDEF MFP_CHAR_MAX}
  CHAR_MAX       = 127; // max value for a char
  {$EXTERNALSYM CHAR_MAX}
  {$ENDIF}
  {$IFDEF MFP_CHAR_MIN}
  CHAR_MIN       = -128; // min value for a char
  {$EXTERNALSYM CHAR_MIN}
  {$ENDIF}
  {$IFDEF MFP_USHRT_MAX}
  USHRT_MAX      = 65535; // max value for an unsigned short
  {$EXTERNALSYM USHRT_MAX}
  {$ENDIF}
  {$IFDEF MFP_SHRT_MAX}
  SHRT_MAX       = 32767; // max value for a short
  {$EXTERNALSYM SHRT_MAX}
  {$ENDIF}
  {$IFDEF MFP_SHRT_MIN}
  SHRT_MIN       = -32768; // min value for a short
  {$EXTERNALSYM SHRT_MIN}
  {$ENDIF}
  {$IFDEF MFP_UINT_MAX}
  UINT_MAX       = MAXDWORD; // max value for an unsigned int
  {$EXTERNALSYM UINT_MAX}
  {$ENDIF}

  {$IFDEF MFP_FORCEDWORD}   // Force value to DWord
  FORCEDWORD = MAXDWORD;
  {$ENDIF}

  MAXDW = MAXDWORD;
  MAXUINT32 = High(UINT32);

  {$IFDEF MFP_INT_MAX}
  INT_MAX        = 2147483647; // max value for an int
  {$EXTERNALSYM INT_MAX}
  {$ENDIF}
  {$IFDEF MFP_INT_MIN}
  INT_MIN        = -2147483647-1; // min value for an int
  {$EXTERNALSYM INT_MIN}
  {$ENDIF}
  {$IFDEF MFP_ULONG_MAX}
  ULONG_MAX      = $ffffffffffffffff; // max unsigned long
  {$EXTERNALSYM ULONG_MAX}
  {$ENDIF}
  {$IFDEF MFP_LONG_MAX}
  LONG_MAX       = $7fffffffffffffff; // max signed long
  {$EXTERNALSYM LONG_MAX}
  {$ENDIF}
  {$IFDEF MFP_LONG_MIN}
  LONG_MIN       = -$7fffffffffffffff-1; // min signed long
  {$EXTERNALSYM LONG_MIN}
  {$ENDIF}
  {$IFDEF MFP_ULLONG_MAX}
  ULLONG_MAX     = $ffffffffffffffff; // max unsigned long long
  {$EXTERNALSYM ULLONG_MAX}
  {$ENDIF}
  {$IFDEF MFP_ULONGLONG_MAX}
  ULONGLONG_MAX  = ULLONG_MAX; // Unsigned long long
  {$EXTERNALSYM ULONGLONG_MAX}
  {$ENDIF}
  {$IFDEF MFP_LLONG_MAX}
  LLONG_MAX      = $7fffffffffffffff; // max signed long long
  {$EXTERNALSYM LLONG_MAX}
  {$ENDIF}
  {$IFDEF MFP_LLONG_MIN}
  LLONG_MIN      = -$7fffffffffffffff-1; // min signed long long
  {$EXTERNALSYM LLONG_MIN}
  {$ENDIF}
  {$IFDEF MFP_LONG_BIT}
  LONG_BIT       = 64;
  {$EXTERNALSYM LONG_BIT}
  {$ENDIF}

  {$IFDEF MFP_SSIZE_MAX}
  SSIZE_MAX      = LONG_MAX; // max value for a ssize_t
  {$EXTERNALSYM SSIZE_MAX}
  {$ENDIF}
  {$IFDEF MFP_WORD_BIT}
  WORD_BIT       = 32;
  {$EXTERNALSYM WORD_BIT}
  {$ENDIF}
  {$IFDEF MFP_SIZE_T_MAX}
  SIZE_T_MAX     = ULONG_MAX; // max value for a size_t
  {$EXTERNALSYM SIZE_T_MAX}
  {$ENDIF}
  {$IFDEF MFP_UQUAD_MAX}
  UQUAD_MAX      = ULLONG_MAX;
  {$EXTERNALSYM UQUAD_MAX}
  {$ENDIF}
  {$IFDEF MFP_QUAD_MAX}
  QUAD_MAX       = LLONG_MAX;
  {$EXTERNALSYM QUAD_MAX}
  {$ENDIF}
  {$IFDEF MFP_QUAD_MIN}
  QUAD_MIN       = LLONG_MIN;
  {$EXTERNALSYM QUAD_MIN}
  {$ENDIF}

  // Delphi
  MIN_INT64 = $8000000000000000;
  {$EXTERNALSYM MIN_INT64}
  MAX_INT64 = $7FFFFFFFFFFFFFFF;
  {$EXTERNALSYM MAX_INT64}

  // From WinGdi.h
  //--------------


{$IFDEF MFP_RGBQUAD}

type
  // Also defined in Winapi.Windows
  // The RGBQUAD structure describes a color consisting of relative intensities of red, green, and blue.
  PRGBQUAD = ^tagRGBQUAD;
  tagRGBQUAD = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;  // MUST BE 0!
  end;
  {$EXTERNALSYM tagRGBQUAD}
  RGBQUAD = tagRGBQUAD;
  {$EXTERNALSYM RGBQUAD}

//  Members
//  rgbBlue
//    The intensity of blue in the color.
//
//  rgbGreen
//    The intensity of green in the color.
//
//  rgbRed
//    The intensity of red in the color.
//
//  rgbReserved
//    This member is reserved and must be zero.
//
//  Remarks
//    The bmiColors member of the BITMAPINFO structure consists of an array of RGBQUAD structures.

{$ENDIF}


{$IFDEF MFP_RGB}
type
  PRGB = ^TRGB;
  _RGB = record
    rgbBlue: Byte;
	  rgbGreen: Byte;
	  rgbRed: Byte;
  end;
  {$EXTERNALSYM _RGB}
  TRGB = _RGB;
{$ENDIF}


{$IFDEF MFP_MemoryAllocator}
type
  PMemoryAllocator = ^MemoryAllocator;
  MemoryAllocator = function(Size: DWORD): Pointer; stdcall;
  {$EXTERNALSYM MemoryAllocator}
{$ENDIF}


// MfPack.WinError
{$IFDEF MFP_WinError}
const
  APPLICATION_ERROR_MASK       = $20000000;
  {$EXTERNALSYM APPLICATION_ERROR_MASK}
  ERROR_SEVERITY_SUCCESS       = $00000000;
  {$EXTERNALSYM ERROR_SEVERITY_SUCCESS}
  ERROR_SEVERITY_INFORMATIONAL = $40000000;
  {$EXTERNALSYM ERROR_SEVERITY_INFORMATIONAL}
  ERROR_SEVERITY_WARNING       = DWORD($80000000);
  {$EXTERNALSYM ERROR_SEVERITY_WARNING}
  ERROR_SEVERITY_ERROR         = DWORD($C0000000);
  {$EXTERNALSYM ERROR_SEVERITY_ERROR}
{$ENDIF}



// wingdi.h

{$IFDEF MFP_FONTSIGNATURE}
type
  PFONTSIGNATURE = ^tagFONTSIGNATURE;
  LPFONTSIGNATURE = ^tagFONTSIGNATURE;
  tagFONTSIGNATURE = record
    fsUsb: array[0..3] of DWORD;
    fsCsb: array[0..1] of DWORD;
  end;
  {$EXTERNALSYM tagFONTSIGNATURE}
  FONTSIGNATURE = tagFONTSIGNATURE;
  {$EXTERNALSYM FONTSIGNATURE}
{$ENDIF}


{$IFDEF MFP_LOGFONT}
//* Logical Font */

const LF_FACESIZE = 32;
{$EXTERNALSYM LF_FACESIZE}

type
  PLOGFONTA = ^tagLOGFONTA;
  NPLOGFONTA = ^tagLOGFONTA;
  LPLOGFONTA = PLOGFONTA;
  tagLOGFONTA = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tagLOGFONTA}
  LOGFONTA = tagLOGFONTA;
  {$EXTERNALSYM LOGFONTA}


  PLOGFONTW = ^tagLOGFONTW;
  NPLOGFONTW = ^tagLOGFONTW;
  {$EXTERNALSYM NPLOGFONTW}
  LPLOGFONTW = PLOGFONTW;
  {$EXTERNALSYM LPLOGFONTW}
  tagLOGFONTW = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of WideChar;
  end;
  {$EXTERNALSYM tagLOGFONTW}
  LOGFONTW = tagLOGFONTW;
  {$EXTERNALSYM LOGFONTW}


{$IFDEF UNICODE}
  LOGFONT = LOGFONTW;
  {$EXTERNALSYM LOGFONT}
  PLOGFONT = PLOGFONTW;
  {$EXTERNALSYM PLOGFONT}
  NPLOGFONT = NPLOGFONTW;
  {$EXTERNALSYM NPLOGFONT}
  LPLOGFONT = LPLOGFONTW;
  {$EXTERNALSYM LPLOGFONT}
{$ELSE}
  LOGFONT = LOGFONTA;
  {$EXTERNALSYM LOGFONT}
  PLOGFONT = PLOGFONTA;
  {$EXTERNALSYM PLOGFONT}
  NPLOGFONT = NPLOGFONTA;
  {$EXTERNALSYM NPLOGFONT}
  LPLOGFONT = LPLOGFONTA;
  {$EXTERNALSYM LPLOGFONT}
{$ENDIF} // UNICODE

{$ENDIF} // LOGFONT


{$IFDEF MFP_DISPID}
  TDISPID = Longint;
  {$EXTERNALSYM TDISPID}
{$ENDIF}


  // Additional Prototypes for ALL interfaces

const ONE_HNS_SECOND  = 10000000; // One sec = 100,000 hns
{$EXTERNALSYM ONE_HNS_SECOND}
const ONE_HNS_MSEC    = 10000;    // One ms  = 10,000 hns
{$EXTERNALSYM ONE_HNS_MSEC}
const ONE_MSEC_SECOND = 1000;     // One second = 1,000 ms
{$EXTERNALSYM ONE_MSEC_SECOND}

const ONE_MHZ = 1000000;          // One MegaHertz = 1,000,000 Hertz
{$EXTERNALSYM ONE_MHZ}
const ONE_GHZ = ONE_MHZ * 1000;   // One GigaHertz = 1,000 MegaHertz
{$EXTERNALSYM ONE_GHZ}


type
  _HTMLColors = record
    ClrName: string;
    DelphiClr: TColor;
  end;
  {$EXTERNALSYM _HTMLColors}
  THTMLColors = _HTMLColors;

const
  // We don't use the delphi colornames, because of version compatibility
  HTMLColorNames: array [0..142] of _HTMLColors = (
    (ClrName: 'ALICEBLUE'; DelphiClr: TColor($FFF8F0)),
    (ClrName: 'ANTIQUEWHITE'; DelphiClr: TColor($D7EBFA)),
    (ClrName: 'AQUA'; DelphiClr: TColor($FFFF00)),
    (ClrName: 'AQUAMARINE'; DelphiClr: TColor($D4FF7F)),
    (ClrName: 'AZURE'; DelphiClr: TColor($FFFFF0)),
    (ClrName: 'BEIGE'; DelphiClr: TColor($DCF5F5)),
    (ClrName: 'BISQUE'; DelphiClr: TColor($C4E4FF)),
    (ClrName: 'BLACK'; DelphiClr: TColor($000000)),
    (ClrName: 'BLANCHEDALMOND'; DelphiClr: TColor($CDEBFF)),
    (ClrName: 'BLUE'; DelphiClr: TColor($FF0000)),
    (ClrName: 'BLUEVIOLET'; DelphiClr: TColor($8A2BE2)),
    (ClrName: 'BROWN'; DelphiClr: TColor($A52A2A)),
    (ClrName: 'BURLYWOOD'; DelphiClr: TColor($DEB887)),
    (ClrName: 'CADETBLUE'; DelphiClr: TColor($5F9EA0)),
    (ClrName: 'CHARTREUSE'; DelphiClr: TColor($7FFF00)),
    (ClrName: 'CHOCOLATE'; DelphiClr: TColor($D2691E)),
    (ClrName: 'CORAL'; DelphiClr: TColor($FF7F50)),
    (ClrName: 'CORNFLOWERBLUE'; DelphiClr: TColor($6495ED)),
    (ClrName: 'CORNSILK'; DelphiClr: TColor($FFF8DC)),
    (ClrName: 'CRIMSON'; DelphiClr: TColor($DC143C)),
    (ClrName: 'CYAN'; DelphiClr: TColor($00FFFF)),
    (ClrName: 'DARKBLUE'; DelphiClr: TColor($00008B)),
    (ClrName: 'DARKCYAN'; DelphiClr: TColor($008B8B)),
    (ClrName: 'DARKGOLDENROD'; DelphiClr: TColor($B8860B)),
    (ClrName: 'DARKGRAY'; DelphiClr: TColor($A9A9A9)),
    (ClrName: 'DARKGREEN'; DelphiClr: TColor($006400)),
    (ClrName: 'DARKKHAKI'; DelphiClr: TColor($BDB76B)),
    (ClrName: 'DARKMAGENTA'; DelphiClr: TColor($8B008B)),
    (ClrName: 'DARKOLIVEGREEN'; DelphiClr: TColor($556B2F)),
    (ClrName: 'DARKORANGE'; DelphiClr: TColor($FF8C00)),
    (ClrName: 'DARKORCHID'; DelphiClr: TColor($9932CC)),
    (ClrName: 'DARKRED'; DelphiClr: TColor($8B0000)),
    (ClrName: 'DARKSALMON'; DelphiClr: TColor($E9967A)),
    (ClrName: 'DARKSEAGREEN'; DelphiClr: TColor($8FBC8B)),
    (ClrName: 'DARKSLATEBLUE'; DelphiClr: TColor($483D8B)),
    (ClrName: 'DARKSLATEGRAY'; DelphiClr: TColor($2F4F4F)),
    (ClrName: 'DARKTURQUOISE'; DelphiClr: TColor($00CED1)),
    (ClrName: 'DARKVIOLET'; DelphiClr: TColor($9400D3)),
    (ClrName: 'DEEPPINK'; DelphiClr: TColor($FF1493)),
    (ClrName: 'DEEPSKYBLUE'; DelphiClr: TColor($00BFFF)),
    (ClrName: 'DIMGRAY'; DelphiClr: TColor($696969)),
    (ClrName: 'DODGERBLUE'; DelphiClr: TColor($1E90FF)),
    (ClrName: 'FIREBRICK'; DelphiClr: TColor($B22222)),
    (ClrName: 'FLORALWHITE'; DelphiClr: TColor($FFFAF0)),
    (ClrName: 'FORESTGREEN'; DelphiClr: TColor($228B22)),
    (ClrName: 'FUCHSIA'; DelphiClr: TColor($FF00FF)),
    (ClrName: 'GAINSBORO'; DelphiClr: TColor($DCDCDC)),
    (ClrName: 'GHOSTWHITE'; DelphiClr: TColor($F8F8FF)),
    (ClrName: 'GOLD'; DelphiClr: TColor($FFD700)),
    (ClrName: 'GOLDENROD'; DelphiClr: TColor($DAA520)),
    (ClrName: 'GRAY'; DelphiClr: TColor($808080)),
    (ClrName: 'GREEN'; DelphiClr: TColor($008000)),
    (ClrName: 'GREENYELLOW'; DelphiClr: TColor($ADFF2F)),
    (ClrName: 'HONEYDEW'; DelphiClr: TColor($F0FFF0)),
    (ClrName: 'HOTPINK'; DelphiClr: TColor($FF69B4)),
    (ClrName: 'INDIANRED'; DelphiClr: TColor($CD5C5C)),
    (ClrName: 'INDIGO'; DelphiClr: TColor($4B0082)),
    (ClrName: 'IVORY'; DelphiClr: TColor($FFFFF0)),
    (ClrName: 'KHAKI'; DelphiClr: TColor($F0E68C)),
    (ClrName: 'LAVENDER'; DelphiClr: TColor($E6E6FA)),
    (ClrName: 'LAVENDERBLUSH'; DelphiClr: TColor($FFF0F5)),
    (ClrName: 'LAWNGREEN'; DelphiClr: TColor($7CFC00)),
    (ClrName: 'LEMONCHIFFON'; DelphiClr: TColor($FFFACD)),
    (ClrName: 'LIGHTBLUE'; DelphiClr: TColor($ADD8E6)),
    (ClrName: 'LIGHTCORAL'; DelphiClr: TColor($F08080)),
    (ClrName: 'LIGHTCYAN'; DelphiClr: TColor($E0FFFF)),
    (ClrName: 'LIGHTGOLDENRODYELLOW'; DelphiClr: TColor($FAFAD2)),
    (ClrName: 'LIGHTGRAY'; DelphiClr: TColor($D3D3D3)),
    (ClrName: 'LIGHTGREEN'; DelphiClr: TColor($90EE90)),
    (ClrName: 'LIGHTPINK'; DelphiClr: TColor($FFB6C1)),
    (ClrName: 'LIGHTSALMON'; DelphiClr: TColor($FFA07A)),
    (ClrName: 'LIGHTSALMON'; DelphiClr: TColor($FFA07A)),
    (ClrName: 'LIGHTSEAGREEN'; DelphiClr: TColor($20B2AA)),
    (ClrName: 'LIGHTSKYBLUE'; DelphiClr: TColor($87CEFA)),
    (ClrName: 'LIGHTSLATEGRAY'; DelphiClr: TColor($778899)),
    (ClrName: 'LIGHTSTEELBLUE'; DelphiClr: TColor($B0C4DE)),
    (ClrName: 'LIGHTYELLOW'; DelphiClr: TColor($FFFFE0)),
    (ClrName: 'LIME'; DelphiClr: TColor($00FF00)),
    (ClrName: 'LIMEGREEN'; DelphiClr: TColor($32CD32)),
    (ClrName: 'LINEN'; DelphiClr: TColor($FAF0E6)),
    (ClrName: 'MAGENTA'; DelphiClr: TColor($FF00FF)),
    (ClrName: 'MAROON'; DelphiClr: TColor($800000)),
    (ClrName: 'MEDIUMAQUAMARINE'; DelphiClr: TColor($66CDAA)),
    (ClrName: 'MEDIUMBLUE'; DelphiClr: TColor($0000CD)),
    (ClrName: 'MEDIUMORCHID'; DelphiClr: TColor($BA55D3)),
    (ClrName: 'MEDIUMPURPLE'; DelphiClr: TColor($9370DB)),
    (ClrName: 'MEDIUMSEAGREEN'; DelphiClr: TColor($3CB371)),
    (ClrName: 'MEDIUMSLATEBLUE'; DelphiClr: TColor($7B68EE)),
    (ClrName: 'MEDIUMSLATEBLUE'; DelphiClr: TColor($7B68EE)),
    (ClrName: 'MEDIUMSPRINGGREEN'; DelphiClr: TColor($00FA9A)),
    (ClrName: 'MEDIUMTURQUOISE'; DelphiClr: TColor($48D1CC)),
    (ClrName: 'MEDIUMVIOLETRED'; DelphiClr: TColor($C71585)),
    (ClrName: 'MIDNIGHTBLUE'; DelphiClr: TColor($191970)),
    (ClrName: 'MINTCREAM'; DelphiClr: TColor($F5FFFA)),
    (ClrName: 'MISTYROSE'; DelphiClr: TColor($FFE4E1)),
    (ClrName: 'MOCCASIN'; DelphiClr: TColor($FFE4B5)),
    (ClrName: 'NAVAJOWHITE'; DelphiClr: TColor($FFDEAD)),
    (ClrName: 'NAVY'; DelphiClr: TColor($000080)),
    (ClrName: 'OLDLACE'; DelphiClr: TColor($FDF5E6)),
    (ClrName: 'OLIVE'; DelphiClr: TColor($808000)),
    (ClrName: 'OLIVEDRAB'; DelphiClr: TColor($6B8E23)),
    (ClrName: 'ORANGE'; DelphiClr: TColor($FFA500)),
    (ClrName: 'ORANGERED'; DelphiClr: TColor($FF4500)),
    (ClrName: 'ORCHID'; DelphiClr: TColor($DA70D6)),
    (ClrName: 'PALEGOLDENROD'; DelphiClr: TColor($EEE8AA)),
    (ClrName: 'PALEGREEN'; DelphiClr: TColor($98FB98)),
    (ClrName: 'PALETURQUOISE'; DelphiClr: TColor($AFEEEE)),
    (ClrName: 'PALEVIOLETRED'; DelphiClr: TColor($DB7093)),
    (ClrName: 'PAPAYAWHIP'; DelphiClr: TColor($FFEFD5)),
    (ClrName: 'PEACHPUFF'; DelphiClr: TColor($FFDAB9)),
    (ClrName: 'PERU'; DelphiClr: TColor($CD853F)),
    (ClrName: 'PINK'; DelphiClr: TColor($FFC0CB)),
    (ClrName: 'PLUM'; DelphiClr: TColor($DDA0DD)),
    (ClrName: 'POWDERBLUE'; DelphiClr: TColor($B0E0E6)),
    (ClrName: 'PURPLE'; DelphiClr: TColor($800080)),
    (ClrName: 'REBECCAPURPLE'; DelphiClr: TColor($663399)),
    (ClrName: 'RED'; DelphiClr: TColor($FF0000)),
    (ClrName: 'ROSYBROWN'; DelphiClr: TColor($BC8F8F)),
    (ClrName: 'ROYALBLUE'; DelphiClr: TColor($4169E1)),
    (ClrName: 'SADDLEBROWN'; DelphiClr: TColor($8B4513)),
    (ClrName: 'SALMON'; DelphiClr: TColor($FA8072)),
    (ClrName: 'SANDYBROWN'; DelphiClr: TColor($F4A460)),
    (ClrName: 'SEAGREEN'; DelphiClr: TColor($2E8B57)),
    (ClrName: 'SEASHELL'; DelphiClr: TColor($FFF5EE)),
    (ClrName: 'SIENNA'; DelphiClr: TColor($A0522D)),
    (ClrName: 'SILVER'; DelphiClr: TColor($C0C0C0)),
    (ClrName: 'SKYBLUE'; DelphiClr: TColor($87CEEB)),
    (ClrName: 'SLATEBLUE'; DelphiClr: TColor($6A5ACD)),
    (ClrName: 'SLATEGRAY'; DelphiClr: TColor($708090)),
    (ClrName: 'SNOW'; DelphiClr: TColor($FFFAFA)),
    (ClrName: 'SPRINGGREEN'; DelphiClr: TColor($00FF7F)),
    (ClrName: 'STEELBLUE'; DelphiClr: TColor($4682B4)),
    (ClrName: 'TAN'; DelphiClr: TColor($D2B48C)),
    (ClrName: 'TEAL'; DelphiClr: TColor($008080)),
    (ClrName: 'THISTLE'; DelphiClr: TColor($D8BFD8)),
    (ClrName: 'TOMATO'; DelphiClr: TColor($FF6347)),
    (ClrName: 'TURQUOISE'; DelphiClr: TColor($40E0D0)),
    (ClrName: 'VIOLET'; DelphiClr: TColor($EE82EE)),
    (ClrName: 'WHEAT'; DelphiClr: TColor($F5DEB3)),
    (ClrName: 'WHITE'; DelphiClr: TColor($FFFFFF)),
    (ClrName: 'WHITESMOKE'; DelphiClr: TColor($F5F5F5)),
    (ClrName: 'YELLOW'; DelphiClr: TColor($FFFF00)),
    (ClrName: 'YELLOWGREEN'; DelphiClr: TColor($9ACD32))
  );

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.

