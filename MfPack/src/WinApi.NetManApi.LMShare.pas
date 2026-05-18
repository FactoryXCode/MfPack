// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.NetManApi.LMShare.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: This module defines the API function prototypes and data structures
//              for the following groups of NT API functions:
//                NetShare
//                NetSession
//                NetFile
//                NetConnection
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: LMShare.h
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
unit WinApi.NetManApi.LMShare;

  {$HPPEMIT '#include "windef.h"'}
  {$HPPEMIT '#include "lmcons.h"'}
  {$HPPEMIT '#include "LMShare.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type

  NET_API_STATUS = DWORD;
  {$EXTERNALSYM NET_API_STATUS}
  LMSTR = PWideChar;
  {$EXTERNALSYM LMSTR}
  PLMSTR = ^LMSTR;
  {$EXTERNALSYM PLMSTR}
  LPDWORD = PDWORD;
  {$EXTERNALSYM LPDWORD}
  PDWORD_PTR = ^DWORD_PTR;
  {$EXTERNALSYM PDWORD_PTR}

  //
  // SHARE API
  //

  //
  // Function Prototypes - Share
  //

function NetShareAdd(servername: LMSTR;
                     level: DWORD;
                     buf: LPBYTE;
                     parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareAdd}

function NetShareEnum(servername: LMSTR;
                      level: DWORD;
                      var bufptr: LPBYTE;
                      prefmaxlen: DWORD;
                      entriesread: LPDWORD;
                      totalentries: LPDWORD;
                      resume_handle: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareEnum}

function NetShareEnumSticky(servername: LMSTR;
                            level: DWORD;
                            var bufptr: LPBYTE;
                            prefmaxlen: DWORD;
                            entriesread: LPDWORD;
                            totalentries: LPDWORD;
                            resume_handle: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareEnumSticky}

function NetShareGetInfo(servername: LMSTR;
                         netname: LMSTR;
                         level: DWORD;
                         var bufptr: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareGetInfo}

function NetShareSetInfo(servername: LMSTR;
                         netname: LMSTR;
                         level: DWORD;
                         buf: LPBYTE;
                         parm_err: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareSetInfo}

function NetShareDel(servername: LMSTR;
                     netname: LMSTR;
                     reserved: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDel}

function NetShareDelSticky(servername: LMSTR;
                           netname: LMSTR;
                           reserved: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDelSticky}

function NetShareCheck(servername: LMSTR;
                       device: LMSTR;
                       type_: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareCheck}

function NetShareDelEx(servername: LMSTR;
                       level: DWORD;
                       buf: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDelEx}

  //
  // Data Structures - Share
  //

type

  _SHARE_INFO_0 = record
    shi0_netname: LMSTR;
  end;
  {$EXTERNALSYM _SHARE_INFO_0}
  SHARE_INFO_0 = _SHARE_INFO_0;
  {$EXTERNALSYM SHARE_INFO_0}
  PSHARE_INFO_0 = ^SHARE_INFO_0;
  {$EXTERNALSYM PSHARE_INFO_0}
  LPSHARE_INFO_0 = ^SHARE_INFO_0;
  {$EXTERNALSYM LPSHARE_INFO_0}

  _SHARE_INFO_1 = record
    shi1_netname: LMSTR;
    shi1_type: DWORD;
    shi1_remark: LMSTR;
  end;
  {$EXTERNALSYM _SHARE_INFO_1}
  SHARE_INFO_1 = _SHARE_INFO_1;
  {$EXTERNALSYM SHARE_INFO_1}
  PSHARE_INFO_1 = ^SHARE_INFO_1;
  {$EXTERNALSYM PSHARE_INFO_1}
  LPSHARE_INFO_1 = ^SHARE_INFO_1;
  {$EXTERNALSYM LPSHARE_INFO_1}

  _SHARE_INFO_2 = record
    shi2_netname: LMSTR;
    shi2_type: DWORD;
    shi2_remark: LMSTR;
    shi2_permissions: DWORD;
    shi2_max_uses: DWORD;
    shi2_current_uses: DWORD;
    shi2_path: LMSTR;
    shi2_passwd: LMSTR;
  end;
  {$EXTERNALSYM _SHARE_INFO_2}
  SHARE_INFO_2 = _SHARE_INFO_2;
  {$EXTERNALSYM SHARE_INFO_2}
  PSHARE_INFO_2 = ^SHARE_INFO_2;
  {$EXTERNALSYM PSHARE_INFO_2}
  LPSHARE_INFO_2 = ^SHARE_INFO_2;
  {$EXTERNALSYM LPSHARE_INFO_2}

  _SHARE_INFO_501 = record
    shi501_netname: LMSTR;
    shi501_type: DWORD;
    shi501_remark: LMSTR;
    shi501_flags: DWORD;
  end;
  {$EXTERNALSYM _SHARE_INFO_501}
  SHARE_INFO_501 = _SHARE_INFO_501;
  {$EXTERNALSYM SHARE_INFO_501}
  PSHARE_INFO_501 = ^SHARE_INFO_501;
  {$EXTERNALSYM PSHARE_INFO_501}
  LPSHARE_INFO_501 = ^SHARE_INFO_501;
  {$EXTERNALSYM LPSHARE_INFO_501}

  _SHARE_INFO_502 = record
    shi502_netname: LMSTR;
    shi502_type: DWORD;
    shi502_remark: LMSTR;
    shi502_permissions: DWORD;
    shi502_max_uses: DWORD;
    shi502_current_uses: DWORD;
    shi502_path: LMSTR;
    shi502_passwd: LMSTR;
    shi502_reserved: DWORD;
    shi502_security_descriptor: PSECURITY_DESCRIPTOR;
  end;
  {$EXTERNALSYM _SHARE_INFO_502}
  SHARE_INFO_502 = _SHARE_INFO_502;
  {$EXTERNALSYM SHARE_INFO_502}
  PSHARE_INFO_502 = ^SHARE_INFO_502;
  {$EXTERNALSYM PSHARE_INFO_502}
  LPSHARE_INFO_502 = ^SHARE_INFO_502;
  {$EXTERNALSYM LPSHARE_INFO_502}

  _SHARE_INFO_503 = record
    shi503_netname: LMSTR;
    shi503_type: DWORD;
    shi503_remark: LMSTR;
    shi503_permissions: DWORD;
    shi503_max_uses: DWORD;
    shi503_current_uses: DWORD;
    shi503_path: LMSTR;
    shi503_passwd: LMSTR;
    shi503_servername: LMSTR;
    shi503_reserved: DWORD;
    shi503_security_descriptor: PSECURITY_DESCRIPTOR;
  end;
  {$EXTERNALSYM _SHARE_INFO_503}
  SHARE_INFO_503 = _SHARE_INFO_503;
  {$EXTERNALSYM SHARE_INFO_503}
  PSHARE_INFO_503 = ^SHARE_INFO_503;
  {$EXTERNALSYM PSHARE_INFO_503}
  LPSHARE_INFO_503 = ^SHARE_INFO_503;
  {$EXTERNALSYM LPSHARE_INFO_503}

  _SHARE_INFO_1004 = record
    shi1004_remark: LMSTR;
  end;
  {$EXTERNALSYM _SHARE_INFO_1004}
  SHARE_INFO_1004 = _SHARE_INFO_1004;
  {$EXTERNALSYM SHARE_INFO_1004}
  PSHARE_INFO_1004 = ^SHARE_INFO_1004;
  {$EXTERNALSYM PSHARE_INFO_1004}
  LPSHARE_INFO_1004 = ^SHARE_INFO_1004;
  {$EXTERNALSYM LPSHARE_INFO_1004}

  _SHARE_INFO_1005 = record
    shi1005_flags: DWORD;
  end;
  {$EXTERNALSYM _SHARE_INFO_1005}
  SHARE_INFO_1005 = _SHARE_INFO_1005;
  {$EXTERNALSYM SHARE_INFO_1005}
  PSHARE_INFO_1005 = ^SHARE_INFO_1005;
  {$EXTERNALSYM PSHARE_INFO_1005}
  LPSHARE_INFO_1005 = ^SHARE_INFO_1005;
  {$EXTERNALSYM LPSHARE_INFO_1005}

  _SHARE_INFO_1006 = record
    shi1006_max_uses: DWORD;
  end;
  {$EXTERNALSYM _SHARE_INFO_1006}
  SHARE_INFO_1006 = _SHARE_INFO_1006;
  {$EXTERNALSYM SHARE_INFO_1006}
  PSHARE_INFO_1006 = ^SHARE_INFO_1006;
  {$EXTERNALSYM PSHARE_INFO_1006}
  LPSHARE_INFO_1006 = ^SHARE_INFO_1006;
  {$EXTERNALSYM LPSHARE_INFO_1006}

  _SHARE_INFO_1501 = record
    shi1501_reserved: DWORD;
    shi1501_security_descriptor: PSECURITY_DESCRIPTOR;
  end;
  {$EXTERNALSYM _SHARE_INFO_1501}
  SHARE_INFO_1501 = _SHARE_INFO_1501;
  {$EXTERNALSYM SHARE_INFO_1501}
  PSHARE_INFO_1501 = ^SHARE_INFO_1501;
  {$EXTERNALSYM PSHARE_INFO_1501}
  LPSHARE_INFO_1501 = ^SHARE_INFO_1501;
  {$EXTERNALSYM LPSHARE_INFO_1501}

  _SHARE_INFO_1503 = record
    shi1503_sharefilter: TGUID;
  end;
  {$EXTERNALSYM _SHARE_INFO_1503}
  SHARE_INFO_1503 = _SHARE_INFO_1503;
  {$EXTERNALSYM SHARE_INFO_1503}
  PSHARE_INFO_1503 = ^SHARE_INFO_1503;
  {$EXTERNALSYM PSHARE_INFO_1503}
  LPSHARE_INFO_1503 = ^SHARE_INFO_1503;
  {$EXTERNALSYM LPSHARE_INFO_1503}

  //
  // NetShareAlias functions
  //
function NetServerAliasAdd(servername: LMSTR;
                           level: DWORD;
                           buf: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerAliasAdd}

function NetServerAliasDel(servername: LMSTR;
                           level: DWORD;
                           buf: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerAliasDel}

function NetServerAliasEnum(servername: LMSTR;
                            level: DWORD;
                            var bufptr: LPBYTE;
                            prefmaxlen: DWORD;
                            entriesread: LPDWORD;
                            totalentries: LPDWORD;
                            resumehandle: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerAliasEnum}

type

  _SERVER_ALIAS_INFO_0 = record
    srvai0_alias: LMSTR;
    srvai0_target: LMSTR;
    srvai0_default: BOOLEAN;
    srvai0_reserved: ULONG;
  end;
  {$EXTERNALSYM _SERVER_ALIAS_INFO_0}
  SERVER_ALIAS_INFO_0 = _SERVER_ALIAS_INFO_0;
  {$EXTERNALSYM SERVER_ALIAS_INFO_0}
  PSERVER_ALIAS_INFO_0 = ^SERVER_ALIAS_INFO_0;
  {$EXTERNALSYM PSERVER_ALIAS_INFO_0}
  LPSERVER_ALIAS_INFO_0 = ^SERVER_ALIAS_INFO_0;
  {$EXTERNALSYM LPSERVER_ALIAS_INFO_0}

  //
  // Special Values and Constants - Share
  //

const
  PARMNUM_BASE_INFOLEVEL         = 1000;
  {$EXTERNALSYM PARMNUM_BASE_INFOLEVEL}

  SHARE_NETNAME_PARMNUM         = 1;
  {$EXTERNALSYM SHARE_NETNAME_PARMNUM}
  SHARE_TYPE_PARMNUM            = 3;
  {$EXTERNALSYM SHARE_TYPE_PARMNUM}
  SHARE_REMARK_PARMNUM          = 4;
  {$EXTERNALSYM SHARE_REMARK_PARMNUM}
  SHARE_PERMISSIONS_PARMNUM     = 5;
  {$EXTERNALSYM SHARE_PERMISSIONS_PARMNUM}
  SHARE_MAX_USES_PARMNUM        = 6;
  {$EXTERNALSYM SHARE_MAX_USES_PARMNUM}
  SHARE_CURRENT_USES_PARMNUM    = 7;
  {$EXTERNALSYM SHARE_CURRENT_USES_PARMNUM}
  SHARE_PATH_PARMNUM            = 8;
  {$EXTERNALSYM SHARE_PATH_PARMNUM}
  SHARE_PASSWD_PARMNUM          = 9;
  {$EXTERNALSYM SHARE_PASSWD_PARMNUM}
  SHARE_FILE_SD_PARMNUM         = 501;
  {$EXTERNALSYM SHARE_FILE_SD_PARMNUM}
  SHARE_SERVER_PARMNUM          = 503;
  {$EXTERNALSYM SHARE_SERVER_PARMNUM}
  SHARE_QOS_POLICY_PARMNUM      = 504;
  {$EXTERNALSYM SHARE_QOS_POLICY_PARMNUM}

  SHARE_REMARK_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + SHARE_REMARK_PARMNUM;
  {$EXTERNALSYM SHARE_REMARK_INFOLEVEL}
  SHARE_MAX_USES_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + SHARE_MAX_USES_PARMNUM;
  {$EXTERNALSYM SHARE_MAX_USES_INFOLEVEL}
  SHARE_FILE_SD_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + SHARE_FILE_SD_PARMNUM;
  {$EXTERNALSYM SHARE_FILE_SD_INFOLEVEL}

  SHI1_NUM_ELEMENTS             = 4;
  {$EXTERNALSYM SHI1_NUM_ELEMENTS}
  SHI2_NUM_ELEMENTS             = 10;
  {$EXTERNALSYM SHI2_NUM_ELEMENTS}

  STYPE_DISKTREE                = 0;
  {$EXTERNALSYM STYPE_DISKTREE}
  STYPE_PRINTQ                  = 1;
  {$EXTERNALSYM STYPE_PRINTQ}
  STYPE_DEVICE                  = 2;
  {$EXTERNALSYM STYPE_DEVICE}
  STYPE_IPC                     = 3;
  {$EXTERNALSYM STYPE_IPC}

  STYPE_MASK                    = $000000FF;
  {$EXTERNALSYM STYPE_MASK}

  STYPE_RESERVED1               = $01000000;
  {$EXTERNALSYM STYPE_RESERVED1}
  STYPE_RESERVED2               = $02000000;
  {$EXTERNALSYM STYPE_RESERVED2}
  STYPE_RESERVED3               = $04000000;
  {$EXTERNALSYM STYPE_RESERVED3}
  STYPE_RESERVED4               = $08000000;
  {$EXTERNALSYM STYPE_RESERVED4}
  STYPE_RESERVED5               = $00100000;
  {$EXTERNALSYM STYPE_RESERVED5}
  STYPE_RESERVED_ALL            = $3FFFFF00;
  {$EXTERNALSYM STYPE_RESERVED_ALL}

  STYPE_TEMPORARY               = $40000000;
  {$EXTERNALSYM STYPE_TEMPORARY}
  STYPE_SPECIAL                 = DWORD($80000000);
  {$EXTERNALSYM STYPE_SPECIAL}

  SHI_USES_UNLIMITED            = DWORD($FFFFFFFF);
  {$EXTERNALSYM SHI_USES_UNLIMITED}

  SHI1005_FLAGS_DFS             = $0001;
  {$EXTERNALSYM SHI1005_FLAGS_DFS}
  SHI1005_FLAGS_DFS_ROOT        = $0002;
  {$EXTERNALSYM SHI1005_FLAGS_DFS_ROOT}

  CSC_MASK_EXT                  = $2030;
  {$EXTERNALSYM CSC_MASK_EXT}
  CSC_MASK                      = $0030;
  {$EXTERNALSYM CSC_MASK}

  CSC_CACHE_MANUAL_REINT        = $0000;
  {$EXTERNALSYM CSC_CACHE_MANUAL_REINT}
  CSC_CACHE_AUTO_REINT          = $0010;
  {$EXTERNALSYM CSC_CACHE_AUTO_REINT}
  CSC_CACHE_VDO                 = $0020;
  {$EXTERNALSYM CSC_CACHE_VDO}
  CSC_CACHE_NONE                = $0030;
  {$EXTERNALSYM CSC_CACHE_NONE}

  SHI1005_FLAGS_RESTRICT_EXCLUSIVE_OPENS         = $000100;
  {$EXTERNALSYM SHI1005_FLAGS_RESTRICT_EXCLUSIVE_OPENS}
  SHI1005_FLAGS_FORCE_SHARED_DELETE              = $000200;
  {$EXTERNALSYM SHI1005_FLAGS_FORCE_SHARED_DELETE}
  SHI1005_FLAGS_ALLOW_NAMESPACE_CACHING          = $000400;
  {$EXTERNALSYM SHI1005_FLAGS_ALLOW_NAMESPACE_CACHING}
  SHI1005_FLAGS_ACCESS_BASED_DIRECTORY_ENUM      = $000800;
  {$EXTERNALSYM SHI1005_FLAGS_ACCESS_BASED_DIRECTORY_ENUM}
  SHI1005_FLAGS_FORCE_LEVELII_OPLOCK             = $001000;
  {$EXTERNALSYM SHI1005_FLAGS_FORCE_LEVELII_OPLOCK}
  SHI1005_FLAGS_ENABLE_HASH                      = $002000;
  {$EXTERNALSYM SHI1005_FLAGS_ENABLE_HASH}
  SHI1005_FLAGS_ENABLE_CA                        = $004000;
  {$EXTERNALSYM SHI1005_FLAGS_ENABLE_CA}
  SHI1005_FLAGS_ENCRYPT_DATA                     = $008000;
  {$EXTERNALSYM SHI1005_FLAGS_ENCRYPT_DATA}
  SHI1005_FLAGS_RESERVED                         = $010000;
  {$EXTERNALSYM SHI1005_FLAGS_RESERVED}
  SHI1005_FLAGS_DISABLE_CLIENT_BUFFERING         = $020000;
  {$EXTERNALSYM SHI1005_FLAGS_DISABLE_CLIENT_BUFFERING}
  SHI1005_FLAGS_IDENTITY_REMOTING                = $040000;
  {$EXTERNALSYM SHI1005_FLAGS_IDENTITY_REMOTING}
  SHI1005_FLAGS_CLUSTER_MANAGED                  = $080000;
  {$EXTERNALSYM SHI1005_FLAGS_CLUSTER_MANAGED}
  SHI1005_FLAGS_COMPRESS_DATA                    = $100000;
  {$EXTERNALSYM SHI1005_FLAGS_COMPRESS_DATA}
  SHI1005_FLAGS_ISOLATED_TRANSPORT               = $200000;
  {$EXTERNALSYM SHI1005_FLAGS_ISOLATED_TRANSPORT}
  SHI1005_FLAGS_DISABLE_DIRECTORY_HANDLE_LEASING = $400000;
  {$EXTERNALSYM SHI1005_FLAGS_DISABLE_DIRECTORY_HANDLE_LEASING}

  SHI1005_VALID_FLAGS_SET    = CSC_MASK or
                               SHI1005_FLAGS_RESTRICT_EXCLUSIVE_OPENS or
                               SHI1005_FLAGS_FORCE_SHARED_DELETE or
                               SHI1005_FLAGS_ALLOW_NAMESPACE_CACHING or
                               SHI1005_FLAGS_ACCESS_BASED_DIRECTORY_ENUM or
                               SHI1005_FLAGS_FORCE_LEVELII_OPLOCK or
                               SHI1005_FLAGS_ENABLE_HASH or
                               SHI1005_FLAGS_ENABLE_CA or
                               SHI1005_FLAGS_ENCRYPT_DATA or
                               SHI1005_FLAGS_DISABLE_CLIENT_BUFFERING or
                               SHI1005_FLAGS_IDENTITY_REMOTING or
                               SHI1005_FLAGS_CLUSTER_MANAGED or
                               SHI1005_FLAGS_RESERVED or
                               SHI1005_FLAGS_COMPRESS_DATA or
                               SHI1005_FLAGS_ISOLATED_TRANSPORT or
                               SHI1005_FLAGS_DISABLE_DIRECTORY_HANDLE_LEASING;
  {$EXTERNALSYM SHI1005_VALID_FLAGS_SET}

  //
  // SESSION API
  //

  //
  // Function Prototypes Session
  //

function NetSessionEnum(servername: LMSTR;
                        UncClientName: LMSTR;
                        username: LMSTR;
                        level: DWORD;
                        var bufptr: LPBYTE;
                        prefmaxlen: DWORD;
                        entriesread: LPDWORD;
                        totalentries: LPDWORD;
                        resume_handle: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionEnum}

function NetSessionDel(servername: LMSTR;
                       UncClientName: LMSTR;
                       username: LMSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionDel}

function NetSessionGetInfo(servername: LMSTR;
                           UncClientName: LMSTR;
                           username: LMSTR;
                           level: DWORD;
                           var bufptr: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionGetInfo}

  //
  // Data Structures - Session
  //

type

  _SESSION_INFO_0 = record
    sesi0_cname: LMSTR;              // client name (no backslashes)
  end;
  {$EXTERNALSYM _SESSION_INFO_0}
  SESSION_INFO_0 = _SESSION_INFO_0;
  {$EXTERNALSYM SESSION_INFO_0}
  PSESSION_INFO_0 = ^SESSION_INFO_0;
  {$EXTERNALSYM PSESSION_INFO_0}
  LPSESSION_INFO_0 = ^SESSION_INFO_0;
  {$EXTERNALSYM LPSESSION_INFO_0}

  _SESSION_INFO_1 = record
    sesi1_cname: LMSTR;              // client name (no backslashes)
    sesi1_username: LMSTR;
    sesi1_num_opens: DWORD;
    sesi1_time: DWORD;
    sesi1_idle_time: DWORD;
    sesi1_user_flags: DWORD;
  end;
  {$EXTERNALSYM _SESSION_INFO_1}
  SESSION_INFO_1 = _SESSION_INFO_1;
  {$EXTERNALSYM SESSION_INFO_1}
  PSESSION_INFO_1 = ^SESSION_INFO_1;
  {$EXTERNALSYM PSESSION_INFO_1}
  LPSESSION_INFO_1 = ^SESSION_INFO_1;
  {$EXTERNALSYM LPSESSION_INFO_1}

  _SESSION_INFO_2 = record
    sesi2_cname: LMSTR;              // client name (no backslashes)
    sesi2_username: LMSTR;
    sesi2_num_opens: DWORD;
    sesi2_time: DWORD;
    sesi2_idle_time: DWORD;
    sesi2_user_flags: DWORD;
    sesi2_cltype_name: LMSTR;
  end;
  {$EXTERNALSYM _SESSION_INFO_2}
  SESSION_INFO_2 = _SESSION_INFO_2;
  {$EXTERNALSYM SESSION_INFO_2}
  PSESSION_INFO_2 = ^SESSION_INFO_2;
  {$EXTERNALSYM PSESSION_INFO_2}
  LPSESSION_INFO_2 = ^SESSION_INFO_2;
  {$EXTERNALSYM LPSESSION_INFO_2}

  _SESSION_INFO_10 = record
    sesi10_cname: LMSTR;             // client name (no backslashes)
    sesi10_username: LMSTR;
    sesi10_time: DWORD;
    sesi10_idle_time: DWORD;
  end;
  {$EXTERNALSYM _SESSION_INFO_10}
  SESSION_INFO_10 = _SESSION_INFO_10;
  {$EXTERNALSYM SESSION_INFO_10}
  PSESSION_INFO_10 = ^SESSION_INFO_10;
  {$EXTERNALSYM PSESSION_INFO_10}
  LPSESSION_INFO_10 = ^SESSION_INFO_10;
  {$EXTERNALSYM LPSESSION_INFO_10}

  _SESSION_INFO_502 = record
    sesi502_cname: LMSTR;             // client name (no backslashes)
    sesi502_username: LMSTR;
    sesi502_num_opens: DWORD;
    sesi502_time: DWORD;
    sesi502_idle_time: DWORD;
    sesi502_user_flags: DWORD;
    sesi502_cltype_name: LMSTR;
    sesi502_transport: LMSTR;
  end;
  {$EXTERNALSYM _SESSION_INFO_502}
  SESSION_INFO_502 = _SESSION_INFO_502;
  {$EXTERNALSYM SESSION_INFO_502}
  PSESSION_INFO_502 = ^SESSION_INFO_502;
  {$EXTERNALSYM PSESSION_INFO_502}
  LPSESSION_INFO_502 = ^SESSION_INFO_502;
  {$EXTERNALSYM LPSESSION_INFO_502}

//
// Special Values and Constants - Session
//

const

  SESS_GUEST          = $00000001;
  {$EXTERNALSYM SESS_GUEST}
  SESS_NOENCRYPTION   = $00000002;
  {$EXTERNALSYM SESS_NOENCRYPTION}

  SESI1_NUM_ELEMENTS  = 8;
  {$EXTERNALSYM SESI1_NUM_ELEMENTS}
  SESI2_NUM_ELEMENTS  = 9;
  {$EXTERNALSYM SESI2_NUM_ELEMENTS}

  //
  // CONNECTION API
  //

  //
  // Function Prototypes - CONNECTION
  //

function NetConnectionEnum(
    servername: LMSTR;
    qualifier: LMSTR;
    level: DWORD;
    var bufptr: LPBYTE;
    prefmaxlen: DWORD;
    entriesread: LPDWORD;
    totalentries: LPDWORD;
    resume_handle: LPDWORD
    ): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConnectionEnum}

  //
  // Data Structures - CONNECTION
  //

type

  _CONNECTION_INFO_0 = record
    coni0_id: DWORD;
  end;
  {$EXTERNALSYM _CONNECTION_INFO_0}
  CONNECTION_INFO_0 = _CONNECTION_INFO_0;
  {$EXTERNALSYM CONNECTION_INFO_0}
  PCONNECTION_INFO_0 = ^CONNECTION_INFO_0;
  {$EXTERNALSYM PCONNECTION_INFO_0}
  LPCONNECTION_INFO_0 = ^CONNECTION_INFO_0;
  {$EXTERNALSYM LPCONNECTION_INFO_0}

  _CONNECTION_INFO_1 = record
    coni1_id: DWORD;
    coni1_type: DWORD;
    coni1_num_opens: DWORD;
    coni1_num_users: DWORD;
    coni1_time: DWORD;
    coni1_username: LMSTR;
    coni1_netname: LMSTR;
  end;
  {$EXTERNALSYM _CONNECTION_INFO_1}
  CONNECTION_INFO_1 = _CONNECTION_INFO_1;
  {$EXTERNALSYM CONNECTION_INFO_1}
  PCONNECTION_INFO_1 = ^CONNECTION_INFO_1;
  {$EXTERNALSYM PCONNECTION_INFO_1}
  LPCONNECTION_INFO_1 = ^CONNECTION_INFO_1;
  {$EXTERNALSYM LPCONNECTION_INFO_1}

  //
  // FILE API
  //

  //
  // Function Prototypes - FILE
  //

function NetFileClose(
    servername: LMSTR;
    fileid: DWORD
    ): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileClose}

function NetFileEnum(
    servername: LMSTR;
    basepath: LMSTR;
    username: LMSTR;
    level: DWORD;
    var bufptr: LPBYTE;
    prefmaxlen: DWORD;
    entriesread: LPDWORD;
    totalentries: LPDWORD;
    resume_handle: PDWORD_PTR
    ): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileEnum}

function NetFileGetInfo(
    servername: LMSTR;
    fileid: DWORD;
    level: DWORD;
    var bufptr: LPBYTE
    ): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileGetInfo}

  //
  // Data Structures - File
  //

  //  File APIs are available at information levels 2 & 3 only. Levels 0 &
  //  1 are not supported.
  //

type
  _FILE_INFO_2 = record
    fi2_id: DWORD;
  end;
  {$EXTERNALSYM _FILE_INFO_2}
  FILE_INFO_2 = _FILE_INFO_2;
  {$EXTERNALSYM FILE_INFO_2}
  PFILE_INFO_2 = ^FILE_INFO_2;
  {$EXTERNALSYM PFILE_INFO_2}
  LPFILE_INFO_2 = ^FILE_INFO_2;
  {$EXTERNALSYM LPFILE_INFO_2}

  _FILE_INFO_3 = record
    fi3_id: DWORD;
    fi3_permissions: DWORD;
    fi3_num_locks: DWORD;
    fi3_pathname: LMSTR;
    fi3_username: LMSTR;
  end;
  {$EXTERNALSYM _FILE_INFO_3}
  FILE_INFO_3 = _FILE_INFO_3;
  {$EXTERNALSYM FILE_INFO_3}
  PFILE_INFO_3 = ^FILE_INFO_3;
  {$EXTERNALSYM PFILE_INFO_3}
  LPFILE_INFO_3 = ^FILE_INFO_3;
  {$EXTERNALSYM LPFILE_INFO_3}

  //
  // Special Values and Constants - File
  //

const
  PERM_FILE_READ      = $1;
  {$EXTERNALSYM PERM_FILE_READ}
  PERM_FILE_WRITE     = $2;
  {$EXTERNALSYM PERM_FILE_WRITE}
  PERM_FILE_CREATE    = $4;
  {$EXTERNALSYM PERM_FILE_CREATE}


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

const
  netapi32 = 'netapi32.dll';

{$WARN SYMBOL_PLATFORM OFF}
function NetShareAdd; external netapi32 name 'NetShareAdd' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareEnum; external netapi32 name 'NetShareEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareEnumSticky; external netapi32 name 'NetShareEnumSticky' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareGetInfo; external netapi32 name 'NetShareGetInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareSetInfo; external netapi32 name 'NetShareSetInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareDel; external netapi32 name 'NetShareDel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareDelSticky; external netapi32 name 'NetShareDelSticky' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareCheck; external netapi32 name 'NetShareCheck' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetShareDelEx; external netapi32 name 'NetShareDelEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetServerAliasAdd; external netapi32 name 'NetServerAliasAdd' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetServerAliasDel; external netapi32 name 'NetServerAliasDel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetServerAliasEnum; external netapi32 name 'NetServerAliasEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetSessionEnum; external netapi32 name 'NetSessionEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetSessionDel; external netapi32 name 'NetSessionDel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetSessionGetInfo; external netapi32 name 'NetSessionGetInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetConnectionEnum; external netapi32 name 'NetConnectionEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetFileClose; external netapi32 name 'NetFileClose' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetFileEnum; external netapi32 name 'NetFileEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function NetFileGetInfo; external netapi32 name 'NetFileGetInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}
end.
