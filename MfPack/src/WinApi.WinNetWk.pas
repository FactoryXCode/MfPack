// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinNetWk.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Standard WINNET Header File for WIN32.
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
// Remarks: - This unit intentionally declares both ANSI and Unicode entry points.
//          - Delphi applications should normally use the explicit W versions.
//          - The unsuffixed helper aliases at the bottom map to the Unicode versions.
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
// Source: winnetwk.h
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
unit WinApi.WinNetWk;

  {$HPPEMIT '#include "WinNetWk.h"'}

interface

uses
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes;

  {$ALIGN ON}
  {$WEAKPACKAGEUNIT}

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

 { Network Resources }
  RESOURCE_CONNECTED = $00000001;
  {$EXTERNALSYM RESOURCE_CONNECTED}
  RESOURCE_GLOBALNET = $00000002;
  {$EXTERNALSYM RESOURCE_GLOBALNET}
  RESOURCE_REMEMBERED = $00000003;
  {$EXTERNALSYM RESOURCE_REMEMBERED}
  RESOURCE_RECENT = $00000004;
  {$EXTERNALSYM RESOURCE_RECENT}
  RESOURCE_CONTEXT = $00000005;
  {$EXTERNALSYM RESOURCE_CONTEXT}

  RESOURCETYPE_ANY = $00000000;
  {$EXTERNALSYM RESOURCETYPE_ANY}
  RESOURCETYPE_DISK = $00000001;
  {$EXTERNALSYM RESOURCETYPE_DISK}
  RESOURCETYPE_PRINT = $00000002;
  {$EXTERNALSYM RESOURCETYPE_PRINT}
  RESOURCETYPE_RESERVED = $00000008;
  {$EXTERNALSYM RESOURCETYPE_RESERVED}
  RESOURCETYPE_UNKNOWN = DWORD($FFFFFFFF);
  {$EXTERNALSYM RESOURCETYPE_UNKNOWN}

  RESOURCEUSAGE_CONNECTABLE = $00000001;
  {$EXTERNALSYM RESOURCEUSAGE_CONNECTABLE}
  RESOURCEUSAGE_CONTAINER = $00000002;
  {$EXTERNALSYM RESOURCEUSAGE_CONTAINER}
  RESOURCEUSAGE_NOLOCALDEVICE = $00000004;
  {$EXTERNALSYM RESOURCEUSAGE_NOLOCALDEVICE}
  RESOURCEUSAGE_SIBLING = $00000008;
  {$EXTERNALSYM RESOURCEUSAGE_SIBLING}
  RESOURCEUSAGE_ATTACHED = $00000010;
  {$EXTERNALSYM RESOURCEUSAGE_ATTACHED}
  RESOURCEUSAGE_ALL = RESOURCEUSAGE_CONNECTABLE or RESOURCEUSAGE_CONTAINER or RESOURCEUSAGE_ATTACHED;
  {$EXTERNALSYM RESOURCEUSAGE_ALL}
  RESOURCEUSAGE_RESERVED = DWORD($80000000);
  {$EXTERNALSYM RESOURCEUSAGE_RESERVED}

  RESOURCEDISPLAYTYPE_GENERIC = $00000000;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_GENERIC}
  RESOURCEDISPLAYTYPE_DOMAIN = $00000001;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_DOMAIN}
  RESOURCEDISPLAYTYPE_SERVER = $00000002;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SERVER}
  RESOURCEDISPLAYTYPE_SHARE = $00000003;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SHARE}
  RESOURCEDISPLAYTYPE_FILE = $00000004;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_FILE}
  RESOURCEDISPLAYTYPE_GROUP = $00000005;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_GROUP}
  RESOURCEDISPLAYTYPE_NETWORK = $00000006;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_NETWORK}
  RESOURCEDISPLAYTYPE_ROOT = $00000007;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_ROOT}
  RESOURCEDISPLAYTYPE_SHAREADMIN = $00000008;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SHAREADMIN}
  RESOURCEDISPLAYTYPE_DIRECTORY = $00000009;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_DIRECTORY}
  RESOURCEDISPLAYTYPE_TREE = $0000000A;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_TREE}
  RESOURCEDISPLAYTYPE_NDSCONTAINER = $0000000B;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_NDSCONTAINER}

  { Network Connections }
  NETPROPERTY_PERSISTENT = 1;
  {$EXTERNALSYM NETPROPERTY_PERSISTENT}

  CONNECT_UPDATE_PROFILE = $00000001;
  {$EXTERNALSYM CONNECT_UPDATE_PROFILE}
  CONNECT_UPDATE_RECENT = $00000002;
  {$EXTERNALSYM CONNECT_UPDATE_RECENT}
  CONNECT_TEMPORARY = $00000004;
  {$EXTERNALSYM CONNECT_TEMPORARY}
  CONNECT_INTERACTIVE = $00000008;
  {$EXTERNALSYM CONNECT_INTERACTIVE}
  CONNECT_PROMPT = $00000010;
  {$EXTERNALSYM CONNECT_PROMPT}
  CONNECT_NEED_DRIVE = $00000020;
  {$EXTERNALSYM CONNECT_NEED_DRIVE}
  CONNECT_REFCOUNT = $00000040;
  {$EXTERNALSYM CONNECT_REFCOUNT}
  CONNECT_REDIRECT = $00000080;
  {$EXTERNALSYM CONNECT_REDIRECT}
  CONNECT_LOCALDRIVE = $00000100;
  {$EXTERNALSYM CONNECT_LOCALDRIVE}
  CONNECT_CURRENT_MEDIA = $00000200;
  {$EXTERNALSYM CONNECT_CURRENT_MEDIA}
  CONNECT_DEFERRED = $00000400;
  {$EXTERNALSYM CONNECT_DEFERRED}
  CONNECT_COMMANDLINE = $00000800;
  {$EXTERNALSYM CONNECT_COMMANDLINE}
  CONNECT_CMD_SAVECRED = $00001000;
  {$EXTERNALSYM CONNECT_CMD_SAVECRED}
  CONNECT_CRED_RESET = $00002000;
  {$EXTERNALSYM CONNECT_CRED_RESET}
  CONNECT_REQUIRE_INTEGRITY = $00004000;
  {$EXTERNALSYM CONNECT_REQUIRE_INTEGRITY}
  CONNECT_REQUIRE_PRIVACY = $00008000;
  {$EXTERNALSYM CONNECT_REQUIRE_PRIVACY}
  CONNECT_WRITE_THROUGH_SEMANTICS = $00010000;
  {$EXTERNALSYM CONNECT_WRITE_THROUGH_SEMANTICS}
  CONNECT_GLOBAL_MAPPING = $00040000;
  {$EXTERNALSYM CONNECT_GLOBAL_MAPPING}
  CONNECT_RESERVED = DWORD($FF000000);
  {$EXTERNALSYM CONNECT_RESERVED}

  { Connection Dialogs }
  CONNDLG_RO_PATH = $00000001;
  {$EXTERNALSYM CONNDLG_RO_PATH}
  CONNDLG_CONN_POINT = $00000002;
  {$EXTERNALSYM CONNDLG_CONN_POINT}
  CONNDLG_USE_MRU = $00000004;
  {$EXTERNALSYM CONNDLG_USE_MRU}
  CONNDLG_HIDE_BOX = $00000008;
  {$EXTERNALSYM CONNDLG_HIDE_BOX}
  CONNDLG_PERSIST = $00000010;
  {$EXTERNALSYM CONNDLG_PERSIST}
  CONNDLG_NOT_PERSIST = $00000020;
  {$EXTERNALSYM CONNDLG_NOT_PERSIST}

  DISC_UPDATE_PROFILE = $00000001;
  {$EXTERNALSYM DISC_UPDATE_PROFILE}
  DISC_NO_FORCE = $00000040;
  {$EXTERNALSYM DISC_NO_FORCE}

  { Universal Naming }
  UNIVERSAL_NAME_INFO_LEVEL = $00000001;
  {$EXTERNALSYM UNIVERSAL_NAME_INFO_LEVEL}
  REMOTE_NAME_INFO_LEVEL = $00000002;
  {$EXTERNALSYM REMOTE_NAME_INFO_LEVEL}

  { Other }
  WNFMT_MULTILINE = $01;
  {$EXTERNALSYM WNFMT_MULTILINE}
  WNFMT_ABBREVIATED = $02;
  {$EXTERNALSYM WNFMT_ABBREVIATED}
  WNFMT_INENUM = $10;
  {$EXTERNALSYM WNFMT_INENUM}
  WNFMT_CONNECTION = $20;
  {$EXTERNALSYM WNFMT_CONNECTION}

  NETINFO_DLL16 = $00000001;
  {$EXTERNALSYM NETINFO_DLL16}
  NETINFO_DISKRED = $00000004;
  {$EXTERNALSYM NETINFO_DISKRED}
  NETINFO_PRINTERRED = $00000008;
  {$EXTERNALSYM NETINFO_PRINTERRED}

  { Status Codes }
  WN_SUCCESS = NO_ERROR;
  {$EXTERNALSYM WN_SUCCESS}
  WN_NO_ERROR = NO_ERROR;
  {$EXTERNALSYM WN_NO_ERROR}
  WN_NOT_SUPPORTED = ERROR_NOT_SUPPORTED;
  {$EXTERNALSYM WN_NOT_SUPPORTED}
  WN_CANCEL = ERROR_CANCELLED;
  {$EXTERNALSYM WN_CANCEL}
  WN_RETRY = ERROR_RETRY;
  {$EXTERNALSYM WN_RETRY}
  WN_NET_ERROR = ERROR_UNEXP_NET_ERR;
  {$EXTERNALSYM WN_NET_ERROR}
  WN_MORE_DATA = ERROR_MORE_DATA;
  {$EXTERNALSYM WN_MORE_DATA}
  WN_BAD_POINTER = ERROR_INVALID_ADDRESS;
  {$EXTERNALSYM WN_BAD_POINTER}
  WN_BAD_VALUE = ERROR_INVALID_PARAMETER;
  {$EXTERNALSYM WN_BAD_VALUE}
  WN_BAD_USER = ERROR_BAD_USERNAME;
  {$EXTERNALSYM WN_BAD_USER}
  WN_BAD_PASSWORD = ERROR_INVALID_PASSWORD;
  {$EXTERNALSYM WN_BAD_PASSWORD}
  WN_ACCESS_DENIED = ERROR_ACCESS_DENIED;
  {$EXTERNALSYM WN_ACCESS_DENIED}
  WN_FUNCTION_BUSY = ERROR_BUSY;
  {$EXTERNALSYM WN_FUNCTION_BUSY}
  WN_WINDOWS_ERROR = ERROR_UNEXP_NET_ERR;
  {$EXTERNALSYM WN_WINDOWS_ERROR}
  WN_OUT_OF_MEMORY = ERROR_NOT_ENOUGH_MEMORY;
  {$EXTERNALSYM WN_OUT_OF_MEMORY}
  WN_NO_NETWORK = ERROR_NO_NETWORK;
  {$EXTERNALSYM WN_NO_NETWORK}
  WN_EXTENDED_ERROR = ERROR_EXTENDED_ERROR;
  {$EXTERNALSYM WN_EXTENDED_ERROR}
  WN_BAD_LEVEL = ERROR_INVALID_LEVEL;
  {$EXTERNALSYM WN_BAD_LEVEL}
  WN_BAD_HANDLE = ERROR_INVALID_HANDLE;
  {$EXTERNALSYM WN_BAD_HANDLE}
  WN_NOT_INITIALIZING = ERROR_ALREADY_INITIALIZED;
  {$EXTERNALSYM WN_NOT_INITIALIZING}
  WN_NO_MORE_DEVICES = ERROR_NO_MORE_DEVICES;
  {$EXTERNALSYM WN_NO_MORE_DEVICES}

  WN_NOT_CONNECTED = ERROR_NOT_CONNECTED;
  {$EXTERNALSYM WN_NOT_CONNECTED}
  WN_OPEN_FILES = ERROR_OPEN_FILES;
  {$EXTERNALSYM WN_OPEN_FILES}
  WN_DEVICE_IN_USE = ERROR_DEVICE_IN_USE;
  {$EXTERNALSYM WN_DEVICE_IN_USE}
  WN_BAD_NETNAME = ERROR_BAD_NET_NAME;
  {$EXTERNALSYM WN_BAD_NETNAME}
  WN_BAD_LOCALNAME = ERROR_BAD_DEVICE;
  {$EXTERNALSYM WN_BAD_LOCALNAME}
  WN_ALREADY_CONNECTED = ERROR_ALREADY_ASSIGNED;
  {$EXTERNALSYM WN_ALREADY_CONNECTED}
  WN_DEVICE_ERROR = ERROR_GEN_FAILURE;
  {$EXTERNALSYM WN_DEVICE_ERROR}
  WN_CONNECTION_CLOSED = ERROR_CONNECTION_UNAVAIL;
  {$EXTERNALSYM WN_CONNECTION_CLOSED}
  WN_NO_NET_OR_BAD_PATH = ERROR_NO_NET_OR_BAD_PATH;
  {$EXTERNALSYM WN_NO_NET_OR_BAD_PATH}
  WN_BAD_PROVIDER = ERROR_BAD_PROVIDER;
  {$EXTERNALSYM WN_BAD_PROVIDER}
  WN_CANNOT_OPEN_PROFILE = ERROR_CANNOT_OPEN_PROFILE;
  {$EXTERNALSYM WN_CANNOT_OPEN_PROFILE}
  WN_BAD_PROFILE = ERROR_BAD_PROFILE;
  {$EXTERNALSYM WN_BAD_PROFILE}
  WN_BAD_DEV_TYPE = ERROR_BAD_DEV_TYPE;
  {$EXTERNALSYM WN_BAD_DEV_TYPE}
  WN_DEVICE_ALREADY_REMEMBERED = ERROR_DEVICE_ALREADY_REMEMBERED;
  {$EXTERNALSYM WN_DEVICE_ALREADY_REMEMBERED}
  WN_CONNECTED_OTHER_PASSWORD = ERROR_CONNECTED_OTHER_PASSWORD;
  {$EXTERNALSYM WN_CONNECTED_OTHER_PASSWORD}
  WN_CONNECTED_OTHER_PASSWORD_DEFAULT = ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT;
  {$EXTERNALSYM WN_CONNECTED_OTHER_PASSWORD_DEFAULT}

  WN_NO_MORE_ENTRIES = ERROR_NO_MORE_ITEMS;
  {$EXTERNALSYM WN_NO_MORE_ENTRIES}
  WN_NOT_CONTAINER = ERROR_NOT_CONTAINER;
  {$EXTERNALSYM WN_NOT_CONTAINER}

  WN_NOT_AUTHENTICATED = ERROR_NOT_AUTHENTICATED;
  {$EXTERNALSYM WN_NOT_AUTHENTICATED}
  WN_NOT_LOGGED_ON = ERROR_NOT_LOGGED_ON;
  {$EXTERNALSYM WN_NOT_LOGGED_ON}
  WN_NOT_VALIDATED = ERROR_NO_LOGON_SERVERS;
  {$EXTERNALSYM WN_NOT_VALIDATED}

  { For Shell }
  WNCON_FORNETCARD = $00000001;
  {$EXTERNALSYM WNCON_FORNETCARD}
  WNCON_NOTROUTED = $00000002;
  {$EXTERNALSYM WNCON_NOTROUTED}
  WNCON_SLOWLINK = $00000004;
  {$EXTERNALSYM WNCON_SLOWLINK}
  WNCON_DYNAMIC = $00000008;
  {$EXTERNALSYM WNCON_DYNAMIC}

type

  PNETRESOURCEA = ^NETRESOURCEA;
  {$EXTERNALSYM PNETRESOURCEA}
  LPNETRESOURCEA = PNETRESOURCEA;
  {$EXTERNALSYM LPNETRESOURCEA}

  _NETRESOURCEA = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: PAnsiChar;
    lpRemoteName: PAnsiChar;
    lpComment: PAnsiChar;
    lpProvider: PAnsiChar;
  end;
  {$EXTERNALSYM _NETRESOURCEA}
  NETRESOURCEA = _NETRESOURCEA;
  {$EXTERNALSYM NETRESOURCEA}
  PNETRESOURCEW = ^NETRESOURCEW;
  {$EXTERNALSYM PNETRESOURCEW}
  LPNETRESOURCEW = PNETRESOURCEW;
  {$EXTERNALSYM LPNETRESOURCEW}

  _NETRESOURCEW = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: LPWSTR;
    lpRemoteName: LPWSTR;
    lpComment: LPWSTR;
    lpProvider: LPWSTR;
  end;
  {$EXTERNALSYM _NETRESOURCEW}
  NETRESOURCEW = _NETRESOURCEW;
  {$EXTERNALSYM NETRESOURCEW}
  NETRESOURCE = NETRESOURCEW;
  {$EXTERNALSYM NETRESOURCE}
  PNETRESOURCE = PNETRESOURCEW;
  {$EXTERNALSYM PNETRESOURCE}
  LPNETRESOURCE = LPNETRESOURCEW;
  {$EXTERNALSYM LPNETRESOURCE}


  LPCONNECTDLGSTRUCTA = ^CONNECTDLGSTRUCTA;
  {$EXTERNALSYM LPCONNECTDLGSTRUCTA}
  _CONNECTDLGSTRUCTA = record
    cbStructure: DWORD;
    hwndOwner: HWND;
    lpConnRes: LPNETRESOURCEA;
    dwFlags: DWORD;
    dwDevNum: DWORD;
  end;
  {$EXTERNALSYM _CONNECTDLGSTRUCTA}
  CONNECTDLGSTRUCTA = _CONNECTDLGSTRUCTA;
  {$EXTERNALSYM CONNECTDLGSTRUCTA}


  LPCONNECTDLGSTRUCTW = ^CONNECTDLGSTRUCTW;
  {$EXTERNALSYM LPCONNECTDLGSTRUCTW}
  _CONNECTDLGSTRUCTW = record
    cbStructure: DWORD;
    hwndOwner: HWND;
    lpConnRes: LPNETRESOURCEW;
    dwFlags: DWORD;
    dwDevNum: DWORD;
  end;
  {$EXTERNALSYM _CONNECTDLGSTRUCTW}
  CONNECTDLGSTRUCTW = _CONNECTDLGSTRUCTW;
  {$EXTERNALSYM CONNECTDLGSTRUCTW}
  CONNECTDLGSTRUCT = CONNECTDLGSTRUCTW;
  {$EXTERNALSYM CONNECTDLGSTRUCT}
  LPCONNECTDLGSTRUCT = LPCONNECTDLGSTRUCTW;
  {$EXTERNALSYM LPCONNECTDLGSTRUCT}


  LPDISCDLGSTRUCTA = ^DISCDLGSTRUCTA;
  {$EXTERNALSYM LPDISCDLGSTRUCTA}
  _DISCDLGSTRUCTA = record
    cbStructure: DWORD;
    hwndOwner: HWND;
    lpLocalName: PAnsiChar;
    lpRemoteName: PAnsiChar;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM _DISCDLGSTRUCTA}
  DISCDLGSTRUCTA = _DISCDLGSTRUCTA;
  {$EXTERNALSYM DISCDLGSTRUCTA}

  LPDISCDLGSTRUCTW = ^DISCDLGSTRUCTW;
  {$EXTERNALSYM LPDISCDLGSTRUCTW}
  _DISCDLGSTRUCTW = record
    cbStructure: DWORD;
    hwndOwner: HWND;
    lpLocalName: LPWSTR;
    lpRemoteName: LPWSTR;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM _DISCDLGSTRUCTW}
  DISCDLGSTRUCTW = _DISCDLGSTRUCTW;
  {$EXTERNALSYM DISCDLGSTRUCTW}
  DISCDLGSTRUCT = DISCDLGSTRUCTW;
  {$EXTERNALSYM DISCDLGSTRUCT}
  LPDISCDLGSTRUCT = LPDISCDLGSTRUCTW;
  {$EXTERNALSYM LPDISCDLGSTRUCT}


  LPUNIVERSAL_NAME_INFOA = ^UNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFOA}
  _UNIVERSAL_NAME_INFOA = record
    lpUniversalName: PAnsiChar;
  end;
  {$EXTERNALSYM _UNIVERSAL_NAME_INFOA}
  UNIVERSAL_NAME_INFOA = _UNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM UNIVERSAL_NAME_INFOA}

  LPUNIVERSAL_NAME_INFOW = ^UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFOW}
  _UNIVERSAL_NAME_INFOW = record
    lpUniversalName: LPWSTR;
  end;
  {$EXTERNALSYM _UNIVERSAL_NAME_INFOW}
  UNIVERSAL_NAME_INFOW = _UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM UNIVERSAL_NAME_INFOW}

  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM UNIVERSAL_NAME_INFO}
  LPUNIVERSAL_NAME_INFO = LPUNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFO}

  LPREMOTE_NAME_INFOA = ^REMOTE_NAME_INFOA;
  {$EXTERNALSYM LPREMOTE_NAME_INFOA}
  _REMOTE_NAME_INFOA = record
    lpUniversalName: PAnsiChar;
    lpConnectionName: PAnsiChar;
    lpRemainingPath: PAnsiChar;
  end;
  {$EXTERNALSYM _REMOTE_NAME_INFOA}
  REMOTE_NAME_INFOA = _REMOTE_NAME_INFOA;
  {$EXTERNALSYM REMOTE_NAME_INFOA}

  LPREMOTE_NAME_INFOW = ^REMOTE_NAME_INFOW;
  {$EXTERNALSYM LPREMOTE_NAME_INFOW}
  _REMOTE_NAME_INFOW = record
    lpUniversalName: LPWSTR;
    lpConnectionName: LPWSTR;
    lpRemainingPath: LPWSTR;
  end;
  {$EXTERNALSYM _REMOTE_NAME_INFOW}
  REMOTE_NAME_INFOW = _REMOTE_NAME_INFOW;
  {$EXTERNALSYM REMOTE_NAME_INFOW}

  REMOTE_NAME_INFO = REMOTE_NAME_INFOW;
  {$EXTERNALSYM REMOTE_NAME_INFO}
  LPREMOTE_NAME_INFO = LPREMOTE_NAME_INFOW;
  {$EXTERNALSYM LPREMOTE_NAME_INFO}
  TREMOTE_NAME_INFO = REMOTE_NAME_INFOW;

  LPNETINFOSTRUCT = ^NETINFOSTRUCT;
  {$EXTERNALSYM LPNETINFOSTRUCT}
  _NETINFOSTRUCT = record
    cbStructure: DWORD;
    dwProviderVersion: DWORD;
    dwStatus: DWORD;
    dwCharacteristics: DWORD;
    dwHandle: ULONG_PTR;
    wNetType: WORD;
    dwPrinters: DWORD;
    dwDrives: DWORD;
  end;
  {$EXTERNALSYM _NETINFOSTRUCT}
  NETINFOSTRUCT = _NETINFOSTRUCT;
  {$EXTERNALSYM NETINFOSTRUCT}


  LPNETCONNECTINFOSTRUCT = ^NETCONNECTINFOSTRUCT;
  {$EXTERNALSYM LPNETCONNECTINFOSTRUCT}
  _NETCONNECTINFOSTRUCT = record
    cbStructure: DWORD;
    dwFlags: DWORD;
    dwSpeed: DWORD;
    dwDelay: DWORD;
    dwOptDataSize: DWORD;
  end;
  {$EXTERNALSYM _NETCONNECTINFOSTRUCT}
  NETCONNECTINFOSTRUCT = _NETCONNECTINFOSTRUCT;
  {$EXTERNALSYM NETCONNECTINFOSTRUCT}


{ Network Connections }
function WNetAddConnectionA(lpRemoteName: PAnsiChar;
                            lpPassword: PAnsiChar;
                            lpLocalName: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnectionA}

function WNetAddConnectionW(lpRemoteName: LPWSTR;
                            lpPassword: LPWSTR;
                            lpLocalName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnectionW}

function WNetAddConnection2A(lpNetResource: LPNETRESOURCEA;
                             lpPassword: PAnsiChar;
                             lpUserName: PAnsiChar;
                             dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection2A}

function WNetAddConnection2W(lpNetResource: LPNETRESOURCEW;
                             lpPassword: LPWSTR;
                             lpUserName: LPWSTR;
                             dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection2W}

function WNetAddConnection3A(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEA;
                             lpPassword: PAnsiChar;
                             lpUserName: PAnsiChar;
                             dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection3A}

function WNetAddConnection3W(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEW;
                             lpPassword: LPWSTR;
                             lpUserName: LPWSTR;
                             dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection3W}

function WNetAddConnection4A(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEA;
                             pAuthBuffer: Pointer;
                             cbAuthBuffer: DWORD;
                             dwFlags: DWORD;
                             lpUseOptions: PByte;
                             cbUseOptions: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection4A}

function WNetAddConnection4W(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEW;
                             pAuthBuffer: Pointer;
                             cbAuthBuffer: DWORD;
                             dwFlags: DWORD;
                             lpUseOptions: PByte;
                             cbUseOptions: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection4W}


function WNetCancelConnectionA(lpName: PAnsiChar;
                               fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnectionA}

function WNetCancelConnectionW(lpName: LPWSTR;
                               fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnectionW}

function WNetCancelConnection2A(lpName: PAnsiChar;
                                dwFlags: DWORD;
                                fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection2A}

function WNetCancelConnection2W(lpName: LPWSTR;
                                dwFlags: DWORD;
                                fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection2W}

function WNetGetConnectionA(lpLocalName: PAnsiChar;
                            lpRemoteName: PAnsiChar;
                            var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetConnectionA}

function WNetGetConnectionW(lpLocalName: LPWSTR;
                            lpRemoteName: LPWSTR;
                            var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetConnectionW}

function WNetRestoreSingleConnectionW(hwndParent: HWND;
                                      lpDevice: LPWSTR;
                                      fUseUI: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetRestoreSingleConnectionW}

function WNetRestoreConnectionW(hWnd: HWND;
                                lpDevice: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetRestoreConnectionW}

function WNetUseConnectionA(hwndOwner: HWND;
                            lpNetResource: LPNETRESOURCEA;
                            lpPassword: PAnsiChar;
                            lpUserId: PAnsiChar;
                            dwFlags: DWORD;
                            lpAccessName: PAnsiChar;
                            var lpBufferSize: DWORD;
                            lpResult: LPDWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnectionA}

function WNetUseConnectionW(hwndOwner: HWND;
                            lpNetResource: LPNETRESOURCEW;
                            lpPassword: LPWSTR;
                            lpUserId: LPWSTR;
                            dwFlags: DWORD;
                            lpAccessName: LPWSTR;
                            var lpBufferSize: DWORD;
                            lpResult: LPDWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnectionW}

function WNetUseConnection4A(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEA;
                             pAuthBuffer: Pointer;
                             cbAuthBuffer: DWORD;
                             dwFlags: DWORD;
                             lpUseOptions: PByte;
                             cbUseOptions: DWORD;
                             lpAccessName: PAnsiChar;
                             var lpBufferSize: DWORD;
                             lpResult: LPDWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnection4A}

function WNetUseConnection4W(hwndOwner: HWND;
                             lpNetResource: LPNETRESOURCEW;
                             pAuthBuffer: Pointer;
                             cbAuthBuffer: DWORD;
                             dwFlags: DWORD;
                             lpUseOptions: PByte;
                             cbUseOptions: DWORD;
                             lpAccessName: LPWSTR;
                             var lpBufferSize: DWORD;
                             lpResult: LPDWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnection4W}

{ Network Connection Dialogs }
function WNetConnectionDialog(hwnd: HWND;
                              dwType: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog}

function WNetDisconnectDialog(hwnd: HWND;
                              dwType: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog}

function WNetConnectionDialog1A(lpConnDlgStruct: LPCONNECTDLGSTRUCTA): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog1A}

function WNetConnectionDialog1W(lpConnDlgStruct: LPCONNECTDLGSTRUCTW): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog1W}

function WNetDisconnectDialog1A(lpConnDlgStruct: LPDISCDLGSTRUCTA): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog1A}

function WNetDisconnectDialog1W(lpConnDlgStruct: LPDISCDLGSTRUCTW): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog1W}

{ Network Browsing }
function WNetOpenEnumA(dwScope: DWORD;
                       dwType: DWORD;
                       dwUsage: DWORD;
                       lpNetResource: LPNETRESOURCEA;
                       var lphEnum: THandle): DWORD; stdcall;
{$EXTERNALSYM WNetOpenEnumA}

function WNetOpenEnumW(dwScope: DWORD;
                       dwType: DWORD;
                       dwUsage: DWORD;
                       lpNetResource: LPNETRESOURCEW;
                       var lphEnum: THandle): DWORD; stdcall;
{$EXTERNALSYM WNetOpenEnumW}

function WNetEnumResourceA(hEnum: THandle;
                           var lpcCount: DWORD;
                           lpBuffer: Pointer;
                           var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetEnumResourceA}

function WNetEnumResourceW(hEnum: THandle;
                           var lpcCount: DWORD;
                           lpBuffer: Pointer;
                           var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetEnumResourceW}

function WNetCloseEnum(hEnum: THandle): DWORD; stdcall;
{$EXTERNALSYM WNetCloseEnum}

function WNetGetResourceParentA(lpNetResource: LPNETRESOURCEA;
                                lpBuffer: Pointer;
                                var lpcbBuffer: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceParentA}

function WNetGetResourceParentW(lpNetResource: LPNETRESOURCEW;
                                lpBuffer: Pointer;
                                var lpcbBuffer: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceParentW}

function WNetGetResourceInformationA(lpNetResource: LPNETRESOURCEA;
                                     lpBuffer: Pointer;
                                     var lpcbBuffer: DWORD;
                                     var lplpSystem: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceInformationA}

function WNetGetResourceInformationW(lpNetResource: LPNETRESOURCEW;
                                     lpBuffer: Pointer;
                                     var lpcbBuffer: DWORD;
                                     var lplpSystem: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceInformationW}

{ Universal Naming }
function WNetGetUniversalNameA(lpLocalPath: PAnsiChar;
                               dwInfoLevel: DWORD;
                               lpBuffer: Pointer;
                               var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUniversalNameA}

function WNetGetUniversalNameW(lpLocalPath: LPWSTR;
                               dwInfoLevel: DWORD;
                               lpBuffer: Pointer;
                               var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUniversalNameW}

{ Authentication and Logon/Logoff }
function WNetGetUserA(lpName: PAnsiChar;
                      lpUserName: PAnsiChar;
                      var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUserA}

function WNetGetUserW(lpName: LPWSTR;
                      lpUserName: LPWSTR;
                      var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUserW}

{ Other }
function WNetGetProviderNameA(dwNetType: DWORD;
                              lpProviderName: PAnsiChar;
                              var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetProviderNameA}

function WNetGetProviderNameW(dwNetType: DWORD;
                              lpProviderName: LPWSTR;
                              var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetProviderNameW}

function WNetGetNetworkInformationA(lpProvider: PAnsiChar;
                                    lpNetInfoStruct: LPNETINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetGetNetworkInformationA}

function WNetGetNetworkInformationW(lpProvider: LPWSTR;
                                    lpNetInfoStruct: LPNETINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetGetNetworkInformationW}

{ Error Handling }
function WNetGetLastErrorA(lpError: LPDWORD;
                           lpErrorBuf: PAnsiChar;
                           nErrorBufSize: DWORD;
                           lpNameBuf: PAnsiChar;
                           nNameBufSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetLastErrorA}

function WNetGetLastErrorW(lpError: LPDWORD;
                           lpErrorBuf: LPWSTR;
                           nErrorBufSize: DWORD;
                           lpNameBuf: LPWSTR;
                           nNameBufSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetLastErrorW}

{ For Shell }
function MultinetGetConnectionPerformanceA(lpNetResource: LPNETRESOURCEA;
                                           lpNetConnectInfoStruct: LPNETCONNECTINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM MultinetGetConnectionPerformanceA}

function MultinetGetConnectionPerformanceW(lpNetResource: LPNETRESOURCEW;
                                           lpNetConnectInfoStruct: LPNETCONNECTINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM MultinetGetConnectionPerformanceW}


{ Unicode aliases }
function WNetAddConnection(lpRemoteName: LPWSTR;
                           lpPassword: LPWSTR;
                           lpLocalName: LPWSTR): DWORD; inline;

function WNetAddConnection2(lpNetResource: LPNETRESOURCEW;
                            lpPassword: LPWSTR;
                            lpUserName: LPWSTR;
                            dwFlags: DWORD): DWORD; inline;

function WNetAddConnection3(hwndOwner: HWND;
                            lpNetResource: LPNETRESOURCEW;
                            lpPassword: LPWSTR;
                            lpUserName: LPWSTR;
                            dwFlags: DWORD): DWORD; inline;

function WNetCancelConnection(lpName: LPWSTR;
                              fForce: BOOL): DWORD; inline;

function WNetCancelConnection2(lpName: LPWSTR;
                               dwFlags: DWORD;
                               fForce: BOOL): DWORD; inline;

function WNetGetConnection(lpLocalName: LPWSTR;
                           lpRemoteName: LPWSTR;
                           var lpnLength: DWORD): DWORD; inline;

function WNetUseConnection(hwndOwner: HWND;
                           lpNetResource: LPNETRESOURCEW;
                           lpPassword: LPWSTR;
                           lpUserId: LPWSTR;
                           dwFlags: DWORD;
                           lpAccessName: LPWSTR;
                           var lpBufferSize: DWORD;
                           lpResult: LPDWORD): DWORD; inline;

function WNetConnectionDialog1(lpConnDlgStruct: LPCONNECTDLGSTRUCTW): DWORD; inline;

function WNetDisconnectDialog1(lpConnDlgStruct: LPDISCDLGSTRUCTW): DWORD; inline;

function WNetOpenEnum(dwScope: DWORD;
                      dwType: DWORD;
                      dwUsage: DWORD;
                      lpNetResource: LPNETRESOURCEW;
                      var lphEnum: THandle): DWORD; inline;

function WNetEnumResource(hEnum: THandle;
                          var lpcCount: DWORD;
                          lpBuffer: Pointer;
                          var lpBufferSize: DWORD): DWORD; inline;

function WNetGetResourceParent(lpNetResource: LPNETRESOURCEW;
                               lpBuffer: Pointer;
                               var lpcbBuffer: DWORD): DWORD; inline;

function WNetGetResourceInformation(lpNetResource: LPNETRESOURCEW;
                                    lpBuffer: Pointer;
                                    var lpcbBuffer: DWORD;
                                    var lplpSystem: LPWSTR): DWORD; inline;

function WNetGetUniversalName(lpLocalPath: LPWSTR;
                              dwInfoLevel: DWORD;
                              lpBuffer: Pointer;
                              var lpBufferSize: DWORD): DWORD; inline;

function WNetGetUser(lpName: LPWSTR;
                     lpUserName: LPWSTR;
                     var lpnLength: DWORD): DWORD; inline;

function WNetGetProviderName(dwNetType: DWORD;
                             lpProviderName: LPWSTR;
                             var lpBufferSize: DWORD): DWORD; inline;

function WNetGetNetworkInformation(lpProvider: LPWSTR;
                                   lpNetInfoStruct: LPNETINFOSTRUCT): DWORD; inline;

function WNetGetLastError(lpError: LPDWORD;
                          lpErrorBuf: LPWSTR;
                          nErrorBufSize: DWORD;
                          lpNameBuf: LPWSTR;
                          nNameBufSize: DWORD): DWORD; inline;

function MultinetGetConnectionPerformance(lpNetResource: LPNETRESOURCEW;
                                          lpNetConnectInfoStruct: LPNETCONNECTINFOSTRUCT): DWORD; inline;


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.


function WNetAddConnection(lpRemoteName: LPWSTR;
                           lpPassword: LPWSTR;
                           lpLocalName: LPWSTR): DWORD;
begin

  Result := WNetAddConnectionW(lpRemoteName,
                               lpPassword,
                               lpLocalName);
end;


function WNetAddConnection2(lpNetResource: LPNETRESOURCEW;
                            lpPassword: LPWSTR;
                            lpUserName: LPWSTR;
                            dwFlags: DWORD): DWORD;
begin

  Result := WNetAddConnection2W(lpNetResource,
                                lpPassword,
                                lpUserName,
                                dwFlags);
end;


function WNetAddConnection3(hwndOwner: HWND;
                            lpNetResource: LPNETRESOURCEW;
                            lpPassword: LPWSTR;
                            lpUserName: LPWSTR;
                            dwFlags: DWORD): DWORD;
begin

  Result := WNetAddConnection3W(hwndOwner,
                                lpNetResource,
                                lpPassword,
                                lpUserName,
                                dwFlags);
end;


function WNetCancelConnection(lpName: LPWSTR;
                              fForce: BOOL): DWORD;
begin

  Result := WNetCancelConnectionW(lpName,
                                  fForce);
end;


function WNetCancelConnection2(lpName: LPWSTR;
                               dwFlags: DWORD;
                               fForce: BOOL): DWORD;
begin

  Result := WNetCancelConnection2W(lpName,
                                   dwFlags,
                                   fForce);
end;


function WNetGetConnection(lpLocalName: LPWSTR;
                           lpRemoteName: LPWSTR;
                           var lpnLength: DWORD): DWORD;
begin

  Result := WNetGetConnectionW(lpLocalName,
                               lpRemoteName,
                               lpnLength);
end;


function WNetUseConnection(hwndOwner: HWND;
                           lpNetResource: LPNETRESOURCEW;
                           lpPassword: LPWSTR;
                           lpUserId: LPWSTR;
                           dwFlags: DWORD;
                           lpAccessName: LPWSTR;
                           var lpBufferSize: DWORD;
                           lpResult: LPDWORD): DWORD;
begin

  Result := WNetUseConnectionW(hwndOwner,
                               lpNetResource,
                               lpPassword,
                               lpUserId,
                               dwFlags,
                               lpAccessName,
                               lpBufferSize,
                               lpResult);
end;


function WNetConnectionDialog1(lpConnDlgStruct: LPCONNECTDLGSTRUCTW): DWORD;
begin

  Result := WNetConnectionDialog1W(lpConnDlgStruct);
end;


function WNetDisconnectDialog1(lpConnDlgStruct: LPDISCDLGSTRUCTW): DWORD;
begin

  Result := WNetDisconnectDialog1W(lpConnDlgStruct);
end;


function WNetOpenEnum(dwScope: DWORD;
                      dwType: DWORD;
                      dwUsage: DWORD;
                      lpNetResource: LPNETRESOURCEW;
                      var lphEnum: THandle): DWORD;
begin

  Result := WNetOpenEnumW(dwScope,
                          dwType,
                          dwUsage,
                          lpNetResource,
                          lphEnum);
end;

function WNetEnumResource(hEnum: THandle;
                          var lpcCount: DWORD;
                          lpBuffer: Pointer;
                          var lpBufferSize: DWORD): DWORD;
begin

  Result := WNetEnumResourceW(hEnum,
                              lpcCount,
                              lpBuffer,
                              lpBufferSize);
end;


function WNetGetResourceParent(lpNetResource: LPNETRESOURCEW;
                               lpBuffer: Pointer;
                               var lpcbBuffer: DWORD): DWORD;
begin

  Result := WNetGetResourceParentW(lpNetResource,
                                   lpBuffer,
                                   lpcbBuffer);
end;


function WNetGetResourceInformation(lpNetResource: LPNETRESOURCEW;
                                    lpBuffer: Pointer;
                                    var lpcbBuffer: DWORD;
                                    var lplpSystem: LPWSTR): DWORD;
begin

  Result := WNetGetResourceInformationW(lpNetResource,
                                        lpBuffer,
                                        lpcbBuffer,
                                        lplpSystem);
end;


function WNetGetUniversalName(lpLocalPath: LPWSTR;
                              dwInfoLevel: DWORD;
                              lpBuffer: Pointer;
                              var lpBufferSize: DWORD): DWORD;
begin

  Result := WNetGetUniversalNameW(lpLocalPath,
                                  dwInfoLevel,
                                  lpBuffer,
                                  lpBufferSize);
end;


function WNetGetUser(lpName: LPWSTR;
                     lpUserName: LPWSTR;
                     var lpnLength: DWORD): DWORD;
begin

  Result := WNetGetUserW(lpName,
                         lpUserName,
                         lpnLength);
end;


function WNetGetProviderName(dwNetType: DWORD;
                             lpProviderName: LPWSTR;
                             var lpBufferSize: DWORD): DWORD;
begin

  Result := WNetGetProviderNameW(dwNetType,
                                 lpProviderName,
                                 lpBufferSize);
end;


function WNetGetNetworkInformation(lpProvider: LPWSTR;
                                   lpNetInfoStruct: LPNETINFOSTRUCT): DWORD;
begin

  Result := WNetGetNetworkInformationW(lpProvider,
                                       lpNetInfoStruct);
end;


function WNetGetLastError(lpError: LPDWORD;
                          lpErrorBuf: LPWSTR;
                          nErrorBufSize: DWORD;
                          lpNameBuf: LPWSTR;
                          nNameBufSize: DWORD): DWORD;
begin

  Result := WNetGetLastErrorW(lpError,
                              lpErrorBuf,
                              nErrorBufSize,
                              lpNameBuf,
                              nNameBufSize);
end;


function MultinetGetConnectionPerformance(lpNetResource: LPNETRESOURCEW;
                                          lpNetConnectInfoStruct: LPNETCONNECTINFOSTRUCT): DWORD;
begin

  Result := MultinetGetConnectionPerformanceW(lpNetResource,
                                              lpNetConnectInfoStruct);
end;


const
  mprLib = 'mpr.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function WNetAddConnectionA; external mprLib name 'WNetAddConnectionA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnectionW; external mprLib name 'WNetAddConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection2A; external mprLib name 'WNetAddConnection2A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection2W; external mprLib name 'WNetAddConnection2W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection3A; external mprLib name 'WNetAddConnection3A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection3W; external mprLib name 'WNetAddConnection3W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection4A; external mprLib name 'WNetAddConnection4A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetAddConnection4W; external mprLib name 'WNetAddConnection4W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetCancelConnectionA; external mprLib name 'WNetCancelConnectionA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetCancelConnectionW; external mprLib name 'WNetCancelConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetCancelConnection2A; external mprLib name 'WNetCancelConnection2A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetCancelConnection2W; external mprLib name 'WNetCancelConnection2W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetConnectionA; external mprLib name 'WNetGetConnectionA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetConnectionW; external mprLib name 'WNetGetConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetRestoreSingleConnectionW; external mprLib name 'WNetRestoreSingleConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetRestoreConnectionW; external mprLib name 'WNetRestoreConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetUseConnectionA; external mprLib name 'WNetUseConnectionA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetUseConnectionW; external mprLib name 'WNetUseConnectionW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetUseConnection4A; external mprLib name 'WNetUseConnection4A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetUseConnection4W; external mprLib name 'WNetUseConnection4W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetConnectionDialog; external mprLib name 'WNetConnectionDialog' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetDisconnectDialog; external mprLib name 'WNetDisconnectDialog' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetConnectionDialog1A; external mprLib name 'WNetConnectionDialog1A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetConnectionDialog1W; external mprLib name 'WNetConnectionDialog1W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetDisconnectDialog1A; external mprLib name 'WNetDisconnectDialog1A' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetDisconnectDialog1W; external mprLib name 'WNetDisconnectDialog1W' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetOpenEnumA; external mprLib name 'WNetOpenEnumA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetOpenEnumW; external mprLib name 'WNetOpenEnumW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetEnumResourceA; external mprLib name 'WNetEnumResourceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF}
  function WNetEnumResourceW; external mprLib name 'WNetEnumResourceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF}
  function WNetCloseEnum; external mprLib name 'WNetCloseEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetGetResourceParentA; external mprLib name 'WNetGetResourceParentA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetResourceParentW; external mprLib name 'WNetGetResourceParentW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetResourceInformationA; external mprLib name 'WNetGetResourceInformationA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetResourceInformationW; external mprLib name 'WNetGetResourceInformationW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetUniversalNameA; external mprLib name 'WNetGetUniversalNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetUniversalNameW; external mprLib name 'WNetGetUniversalNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetGetUserA; external mprLib name 'WNetGetUserA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetUserW; external mprLib name 'WNetGetUserW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetGetProviderNameA; external mprLib name 'WNetGetProviderNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetProviderNameW; external mprLib name 'WNetGetProviderNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetNetworkInformationA; external mprLib name 'WNetGetNetworkInformationA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetNetworkInformationW; external mprLib name 'WNetGetNetworkInformationW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function WNetGetLastErrorA; external mprLib name 'WNetGetLastErrorA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function WNetGetLastErrorW; external mprLib name 'WNetGetLastErrorW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MultinetGetConnectionPerformanceA; external mprLib name 'MultinetGetConnectionPerformanceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MultinetGetConnectionPerformanceW; external mprLib name 'MultinetGetConnectionPerformanceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
