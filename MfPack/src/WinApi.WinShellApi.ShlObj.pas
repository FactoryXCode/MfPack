// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinShellApi.ShlObj.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2026
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description: Shell api.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
// Translation rules:
// - Do not use Delphi's native Winapi.ShlObj unit!
// - LPWSTR returned through out parameters normally requires CoTaskMemFree by
//   the caller unless the original API documentation says otherwise.
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
// Source: ShlObj.h
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
unit WinApi.WinShellApi.ShlObj;


interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Coml2Api,
  WinApi.WinShellApi.ObjectArray,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.OleIdl,
  {WinShellApi}
  WinApi.WinShellApi.ShlObjIdl_Core,
  WinApi.WinShellApi.ShlObj_Core;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  FCIDM_TOOLBAR = FCIDM_BROWSERFIRST + 0;
  {$EXTERNALSYM FCIDM_TOOLBAR}
  FCIDM_STATUS = FCIDM_BROWSERFIRST + 1;
  {$EXTERNALSYM FCIDM_STATUS}

  IDC_OFFLINE_HAND = 103;
  {$EXTERNALSYM IDC_OFFLINE_HAND}
  IDC_PANTOOL_HAND_OPEN = 104;
  {$EXTERNALSYM IDC_PANTOOL_HAND_OPEN}
  IDC_PANTOOL_HAND_CLOSED = 105;
  {$EXTERNALSYM IDC_PANTOOL_HAND_CLOSED}

  PANE_NONE = DWORD($FFFFFFFF);
  {$EXTERNALSYM PANE_NONE}
  PANE_ZONE = 1;
  {$EXTERNALSYM PANE_ZONE}
  PANE_OFFLINE = 2;
  {$EXTERNALSYM PANE_OFFLINE}
  PANE_PRINTER = 3;
  {$EXTERNALSYM PANE_PRINTER}
  PANE_SSL = 4;
  {$EXTERNALSYM PANE_SSL}
  PANE_NAVIGATION = 5;
  {$EXTERNALSYM PANE_NAVIGATION}
  PANE_PROGRESS = 6;
  {$EXTERNALSYM PANE_PROGRESS}
  PANE_PRIVACY = 7;
  {$EXTERNALSYM PANE_PRIVACY}

  OPENPROPS_NONE = $0000;
  {$EXTERNALSYM OPENPROPS_NONE}
  OPENPROPS_INHIBITPIF = $8000;
  {$EXTERNALSYM OPENPROPS_INHIBITPIF}
  GETPROPS_NONE = $0000;
  {$EXTERNALSYM GETPROPS_NONE}
  SETPROPS_NONE = $0000;
  {$EXTERNALSYM SETPROPS_NONE}
  CLOSEPROPS_NONE = $0000;
  {$EXTERNALSYM CLOSEPROPS_NONE}
  CLOSEPROPS_DISCARD = $0001;
  {$EXTERNALSYM CLOSEPROPS_DISCARD}

  SFBID_PIDLCHANGED = 0;
  {$EXTERNALSYM SFBID_PIDLCHANGED}

  DBC_GS_IDEAL = 0;
  {$EXTERNALSYM DBC_GS_IDEAL}
  DBC_GS_SIZEDOWN = 1;
  {$EXTERNALSYM DBC_GS_SIZEDOWN}

  DBC_HIDE = 0;
  {$EXTERNALSYM DBC_HIDE}
  DBC_SHOW = 1;
  {$EXTERNALSYM DBC_SHOW}
  DBC_SHOWOBSCURE = 2;
  {$EXTERNALSYM DBC_SHOWOBSCURE}

  DBCID_EMPTY = 0;
  {$EXTERNALSYM DBCID_EMPTY}
  DBCID_ONDRAG = 1;
  {$EXTERNALSYM DBCID_ONDRAG}
  DBCID_CLSIDOFBAR = 2;
  {$EXTERNALSYM DBCID_CLSIDOFBAR}
  DBCID_RESIZE = 3;
  {$EXTERNALSYM DBCID_RESIZE}
  DBCID_GETBAR = 4;
  {$EXTERNALSYM DBCID_GETBAR}
  DBCID_UPDATESIZE = 5;
  {$EXTERNALSYM DBCID_UPDATESIZE}

  BMICON_LARGE = 0;
  {$EXTERNALSYM BMICON_LARGE}
  BMICON_SMALL = 1;
  {$EXTERNALSYM BMICON_SMALL}

  TBIF_APPEND = 0;
  {$EXTERNALSYM TBIF_APPEND}
  TBIF_PREPEND = 1;
  {$EXTERNALSYM TBIF_PREPEND}
  TBIF_REPLACE = 2;
  {$EXTERNALSYM TBIF_REPLACE}
  TBIF_DEFAULT = $00000000;
  {$EXTERNALSYM TBIF_DEFAULT}
  TBIF_INTERNETBAR = $00010000;
  {$EXTERNALSYM TBIF_INTERNETBAR}
  TBIF_STANDARDTOOLBAR = $00020000;
  {$EXTERNALSYM TBIF_STANDARDTOOLBAR}
  TBIF_NOTOOLBAR = $00030000;
  {$EXTERNALSYM TBIF_NOTOOLBAR}

  SFVM_REARRANGE = $00000001;
  {$EXTERNALSYM SFVM_REARRANGE}
  SFVM_ADDOBJECT = $00000003;
  {$EXTERNALSYM SFVM_ADDOBJECT}
  SFVM_REMOVEOBJECT = $00000006;
  {$EXTERNALSYM SFVM_REMOVEOBJECT}
  SFVM_UPDATEOBJECT = $00000007;
  {$EXTERNALSYM SFVM_UPDATEOBJECT}
  SFVM_GETSELECTEDOBJECTS = $00000009;
  {$EXTERNALSYM SFVM_GETSELECTEDOBJECTS}
  SFVM_SETITEMPOS = $0000000E;
  {$EXTERNALSYM SFVM_SETITEMPOS}
  SFVM_SETCLIPBOARD = $00000010;
  {$EXTERNALSYM SFVM_SETCLIPBOARD}
  SFVM_SETPOINTS = $00000017;
  {$EXTERNALSYM SFVM_SETPOINTS}

type

  ISFB_MASK = type DWORD;
  {$EXTERNALSYM ISFB_MASK}

  ISFB_STATE = type DWORD;
  {$EXTERNALSYM ISFB_STATE}

  ISFBVIEWMODE = type WORD;
  {$EXTERNALSYM ISFBVIEWMODE}

  SSM_FLAGS = type DWORD;
  {$EXTERNALSYM SSM_FLAGS}

  SCHEME_FLAGS = type DWORD;
  {$EXTERNALSYM SCHEME_FLAGS}

  GADOF_FLAGS = type DWORD;
  {$EXTERNALSYM GADOF_FLAGS}

  SHCDF_FLAGS = type DWORD;
  {$EXTERNALSYM SHCDF_FLAGS}

const
  DWFRF_NORMAL           = DWORD($0000);
  {$EXTERNALSYM DWFRF_NORMAL}
  DWFRF_DELETECONFIGDATA = DWORD($0001);
  {$EXTERNALSYM DWFRF_DELETECONFIGDATA}

  DWFAF_HIDDEN   = DWORD($0001);
  {$EXTERNALSYM DWFAF_HIDDEN}
  DWFAF_GROUP1   = DWORD($0002);
  {$EXTERNALSYM DWFAF_GROUP1}
  DWFAF_GROUP2   = DWORD($0004);
  {$EXTERNALSYM DWFAF_GROUP2}
  DWFAF_AUTOHIDE = DWORD($0010);
  {$EXTERNALSYM DWFAF_AUTOHIDE}

  ISFB_MASK_STATE       = ISFB_MASK($00000001);
  {$EXTERNALSYM ISFB_MASK_STATE}
  ISFB_MASK_BKCOLOR     = ISFB_MASK($00000002);
  {$EXTERNALSYM ISFB_MASK_BKCOLOR}
  ISFB_MASK_VIEWMODE    = ISFB_MASK($00000004);
  {$EXTERNALSYM ISFB_MASK_VIEWMODE}
  ISFB_MASK_SHELLFOLDER = ISFB_MASK($00000008);
  {$EXTERNALSYM ISFB_MASK_SHELLFOLDER}
  ISFB_MASK_IDLIST      = ISFB_MASK($00000010);
  {$EXTERNALSYM ISFB_MASK_IDLIST}
  ISFB_MASK_COLORS      = ISFB_MASK($00000020);
  {$EXTERNALSYM ISFB_MASK_COLORS}

  ISFB_STATE_DEFAULT     = ISFB_STATE($00000000);
  {$EXTERNALSYM ISFB_STATE_DEFAULT}
  ISFB_STATE_DEBOSSED    = ISFB_STATE($00000001);
  {$EXTERNALSYM ISFB_STATE_DEBOSSED}
  ISFB_STATE_ALLOWRENAME = ISFB_STATE($00000002);
  {$EXTERNALSYM ISFB_STATE_ALLOWRENAME}
  ISFB_STATE_NOSHOWTEXT  = ISFB_STATE($00000004);
  {$EXTERNALSYM ISFB_STATE_NOSHOWTEXT}
  ISFB_STATE_CHANNELBAR  = ISFB_STATE($00000010);
  {$EXTERNALSYM ISFB_STATE_CHANNELBAR}
  ISFB_STATE_QLINKSMODE  = ISFB_STATE($00000020);
  {$EXTERNALSYM ISFB_STATE_QLINKSMODE}
  ISFB_STATE_FULLOPEN    = ISFB_STATE($00000040);
  {$EXTERNALSYM ISFB_STATE_FULLOPEN}
  ISFB_STATE_NONAMESORT  = ISFB_STATE($00000080);
  {$EXTERNALSYM ISFB_STATE_NONAMESORT}
  ISFB_STATE_BTNMINSIZE  = ISFB_STATE($00000100);
  {$EXTERNALSYM ISFB_STATE_BTNMINSIZE}

  ISFBVIEWMODE_SMALLICONS = ISFBVIEWMODE($0001);
  {$EXTERNALSYM ISFBVIEWMODE_SMALLICONS}
  ISFBVIEWMODE_LARGEICONS = ISFBVIEWMODE($0002);
  {$EXTERNALSYM ISFBVIEWMODE_LARGEICONS}
  ISFBVIEWMODE_LOGOS      = ISFBVIEWMODE($0003);
  {$EXTERNALSYM ISFBVIEWMODE_LOGOS}

  SSM_CLEAR   = SSM_FLAGS($0000);
  {$EXTERNALSYM SSM_CLEAR}
  SSM_SET     = SSM_FLAGS($0001);
  {$EXTERNALSYM SSM_SET}
  SSM_REFRESH = SSM_FLAGS($0002);
  {$EXTERNALSYM SSM_REFRESH}
  SSM_UPDATE  = SSM_FLAGS($0004);
  {$EXTERNALSYM SSM_UPDATE}

  SCHEME_DISPLAY  = SCHEME_FLAGS($0001);
  {$EXTERNALSYM SCHEME_DISPLAY}
  SCHEME_EDIT     = SCHEME_FLAGS($0002);
  {$EXTERNALSYM SCHEME_EDIT}
  SCHEME_LOCAL    = SCHEME_FLAGS($0004);
  {$EXTERNALSYM SCHEME_LOCAL}
  SCHEME_GLOBAL   = SCHEME_FLAGS($0008);
  {$EXTERNALSYM SCHEME_GLOBAL}
  SCHEME_REFRESH  = SCHEME_FLAGS($0010);
  {$EXTERNALSYM SCHEME_REFRESH}
  SCHEME_UPDATE   = SCHEME_FLAGS($0020);
  {$EXTERNALSYM SCHEME_UPDATE}
  SCHEME_DONOTUSE = SCHEME_FLAGS($0040);
  {$EXTERNALSYM SCHEME_DONOTUSE}
  SCHEME_CREATE   = SCHEME_FLAGS($0080);
  {$EXTERNALSYM SCHEME_CREATE}

  GADOF_DIRTY = GADOF_FLAGS($00000001);
  {$EXTERNALSYM GADOF_DIRTY}

  SHCDF_UPDATEITEM = SHCDF_FLAGS($00000001);
  {$EXTERNALSYM SHCDF_UPDATEITEM}

type
  // Forward declarations
  INewShortcutHookA = interface;
  INewShortcutHookW = interface;
  ICopyHookA = interface;
  ICopyHookW = interface;
  ICurrentWorkingDirectory = interface;
  IDockingWindowFrame = interface;
  IThumbnailCapture = interface;
  IShellFolderBand = interface;
  IDeskBarClient = interface;
  IActiveDesktopP = interface;
  IADesktopP2 = interface;
  IColumnProvider = interface;
  IDocViewSite = interface;
  IInitializeObject = interface;
  IBanneredBar = interface;
  // ShlObj.h only forward-declares IEnumPrivacyRecords here.
  // The real interface lives in Mshtml.h/Mshtml.idl.
  // We provide the small Mshtml dependency in the MfPack additional-prototypes
  // section below, instead of translating the full Mshtml monster header.
  //IEnumPrivacyRecords = interface;

  // ---------------------------------------------------------------------------
  // Missing Shtypes/ShObjId helper aliases
  // ---------------------------------------------------------------------------
  // The C header gets these names from Shtypes.h / PropKey.h.  We keep them
  // local here so this legacy ShlObj wrapper does not have to depend on
  // Delphi's native Winapi.ShlObj unit.

  PSHITEMID = ^SHITEMID;
  {$EXTERNALSYM PSHITEMID}
  LPSHITEMID = ^SHITEMID;
  {$EXTERNALSYM LPSHITEMID}
  LPCSHITEMID = ^SHITEMID;
  {$EXTERNALSYM LPCSHITEMID}
  SHITEMID = record
    cb: USHORT;
    abID: array[0..0] of BYTE;
  end;
  {$EXTERNALSYM SHITEMID}

  PITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM PITEMIDLIST}
  LPITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM LPITEMIDLIST}
  PCITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM PCITEMIDLIST}
  LPCITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM LPCITEMIDLIST}
  ITEMIDLIST = record
    mkid: SHITEMID;
  end;
  {$EXTERNALSYM ITEMIDLIST}

  PIDLIST_ABSOLUTE = PITEMIDLIST;
  {$EXTERNALSYM PIDLIST_ABSOLUTE}
  PCIDLIST_ABSOLUTE = PCITEMIDLIST;
  {$EXTERNALSYM PCIDLIST_ABSOLUTE}
  PUITEMID_CHILD = PITEMIDLIST;
  {$EXTERNALSYM PUITEMID_CHILD}
  PCUITEMID_CHILD = PCITEMIDLIST;
  {$EXTERNALSYM PCUITEMID_CHILD}
  PITEMID_CHILD = PITEMIDLIST;
  {$EXTERNALSYM PITEMID_CHILD}
  PCITEMID_CHILD = PCITEMIDLIST;
  {$EXTERNALSYM PCITEMID_CHILD}

  SHCOLUMNID = PROPERTYKEY;
  {$EXTERNALSYM SHCOLUMNID}
  PSHCOLUMNID = ^SHCOLUMNID;
  {$EXTERNALSYM PSHCOLUMNID}
  LPCSHCOLUMNID = ^SHCOLUMNID;
  {$EXTERNALSYM LPCSHCOLUMNID}

  TVariantArg = TVarData;
  {$EXTERNALSYM TVariantArg}
  PVariantArg = ^TVariantArg;
  {$EXTERNALSYM PVariantArg}

  LPSHChangeProductKeyAsIDList = ^SHChangeProductKeyAsIDList;
  {$EXTERNALSYM LPSHChangeProductKeyAsIDList}
  SHChangeProductKeyAsIDList = record
    cb: USHORT;
    wszProductKey: array[0..38] of WCHAR;
    cbZero: USHORT;
  end;
  {$EXTERNALSYM SHChangeProductKeyAsIDList}

  LPSHCOLUMNINFO = ^SHCOLUMNINFO;
  {$EXTERNALSYM LPSHCOLUMNINFO}
  LPCSHCOLUMNINFO = ^SHCOLUMNINFO;
  {$EXTERNALSYM LPCSHCOLUMNINFO}
  SHCOLUMNINFO = record
    scid: SHCOLUMNID;
    vt: VARTYPE;
    fmt: DWORD;
    cChars: UINT;
    csFlags: DWORD;
    wszTitle: array[0..MAX_COLUMN_NAME_LEN - 1] of WCHAR;
    wszDescription: array[0..MAX_COLUMN_DESC_LEN - 1] of WCHAR;
  end;
  {$EXTERNALSYM SHCOLUMNINFO}

  LPSHCOLUMNINIT = ^SHCOLUMNINIT;
  {$EXTERNALSYM LPSHCOLUMNINIT}
  LPCSHCOLUMNINIT = ^SHCOLUMNINIT;
  {$EXTERNALSYM LPCSHCOLUMNINIT}
  SHCOLUMNINIT = record
    dwFlags: ULONG;
    dwReserved: ULONG;
    wszFolder: array[0..MAX_PATH - 1] of WCHAR;
  end;
  {$EXTERNALSYM SHCOLUMNINIT}

  LPSHCOLUMNDATA = ^SHCOLUMNDATA;
  {$EXTERNALSYM LPSHCOLUMNDATA}
  LPCSHCOLUMNDATA = ^SHCOLUMNDATA;
  {$EXTERNALSYM LPCSHCOLUMNDATA}
  SHCOLUMNDATA = record
    dwFlags: ULONG;
    dwFileAttributes: DWORD;
    dwReserved: ULONG;
    pwszExt: LPWSTR;
    wszFile: array[0..MAX_PATH - 1] of WCHAR;
  end;
  {$EXTERNALSYM SHCOLUMNDATA}

  LPTBINFO = ^TBINFO;
  {$EXTERNALSYM LPTBINFO}
  TBINFO = record
    cbuttons: UINT;
    uFlags: UINT;
  end;
  {$EXTERNALSYM TBINFO}

  LPSFV_SETITEMPOS = ^SFV_SETITEMPOS;
  {$EXTERNALSYM LPSFV_SETITEMPOS}
  PCSFV_SETITEMPOS = ^SFV_SETITEMPOS;
  {$EXTERNALSYM PCSFV_SETITEMPOS}
  SFV_SETITEMPOS = record
    pidl: PCUITEMID_CHILD;
    pt: POINT;
  end;
  {$EXTERNALSYM SFV_SETITEMPOS}

  LPAASHELLMENUFILENAME = ^AASHELLMENUFILENAME;
  {$EXTERNALSYM LPAASHELLMENUFILENAME}
  AASHELLMENUFILENAME = record
    cbTotal: SHORT;
    rgbReserved: array[0..11] of BYTE;
    szFileName: array[0..0] of WCHAR;
  end;
  {$EXTERNALSYM AASHELLMENUFILENAME}

  LPAASHELLMENUITEM = ^AASHELLMENUITEM;
  {$EXTERNALSYM LPAASHELLMENUITEM}
  AASHELLMENUITEM = record
    lpReserved1: Pointer;
    iReserved: Integer;
    uiReserved: UINT;
    lpName: LPAASHELLMENUFILENAME;
    psz: LPWSTR;
  end;
  {$EXTERNALSYM AASHELLMENUITEM}

  // Interface INewShortcutHookA
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INewShortcutHookA);'}
  {$EXTERNALSYM INewShortcutHookA}
  INewShortcutHookA = interface(IUnknown)
    ['{000214E1-0000-0000-C000-000000000046}']

    function SetReferent(pcszReferent: LPCSTR;
                         hwnd: HWND): HRESULT; stdcall;

    function GetReferent(pszReferent: LPSTR;
                         cchReferent: Integer): HRESULT; stdcall;

    function SetFolder(pcszFolder: LPCSTR): HRESULT; stdcall;

    function GetFolder(pszFolder: LPSTR;
                       cchFolder: Integer): HRESULT; stdcall;

    function GetName(pszName: LPSTR;
                     cchName: Integer): HRESULT; stdcall;

    function GetExtension(pszExtension: LPSTR;
                          cchExtension: Integer): HRESULT; stdcall;

  end;
  IID_INewShortcutHookA = INewShortcutHookA;
  {$EXTERNALSYM IID_INewShortcutHookA}

  // Interface INewShortcutHookW
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INewShortcutHookW);'}
  {$EXTERNALSYM INewShortcutHookW}
  INewShortcutHookW = interface(IUnknown)
    ['{000214F7-0000-0000-C000-000000000046}']

    function SetReferent(pcszReferent: LPCWSTR;
                         hwnd: HWND): HRESULT; stdcall;

    function GetReferent(pszReferent: LPWSTR;
                         cchReferent: Integer): HRESULT; stdcall;

    function SetFolder(pcszFolder: LPCWSTR): HRESULT; stdcall;

    function GetFolder(pszFolder: LPWSTR;
                       cchFolder: Integer): HRESULT; stdcall;

    function GetName(pszName: LPWSTR;
                     cchName: Integer): HRESULT; stdcall;

    function GetExtension(pszExtension: LPWSTR;
                          cchExtension: Integer): HRESULT; stdcall;

  end;
  IID_INewShortcutHookW = INewShortcutHookW;
  {$EXTERNALSYM IID_INewShortcutHookW}

  {$IFDEF UNICODE}
  INewShortcutHook = INewShortcutHookW;
  {$EXTERNALSYM INewShortcutHook}
  {$ELSE}
  INewShortcutHook = INewShortcutHookA;
  {$EXTERNALSYM INewShortcutHook}
  {$ENDIF}

  // Interface ICopyHookA
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICopyHookA);'}
  {$EXTERNALSYM ICopyHookA}
  ICopyHookA = interface(IUnknown)
    ['{000214EF-0000-0000-C000-000000000046}']

    function CopyCallback(hwnd: HWND;
                          wFunc: UINT;
                          wFlags: UINT;
                          pszSrcFile: LPCSTR;
                          dwSrcAttribs: DWORD;
                          pszDestFile: LPCSTR;
                          dwDestAttribs: DWORD): UINT; stdcall;

  end;
  IID_ICopyHookA = ICopyHookA;
  {$EXTERNALSYM IID_ICopyHookA}

  LPCOPYHOOKA = ^ICopyHookA;
  {$EXTERNALSYM LPCOPYHOOKA}

  // Interface ICopyHookW
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICopyHookW);'}
  {$EXTERNALSYM ICopyHookW}
  ICopyHookW = interface(IUnknown)
    ['{000214FC-0000-0000-C000-000000000046}']

    function CopyCallback(hwnd: HWND;
                          wFunc: UINT;
                          wFlags: UINT;
                          pszSrcFile: LPCWSTR;
                          dwSrcAttribs: DWORD;
                          pszDestFile: LPCWSTR;
                          dwDestAttribs: DWORD): UINT; stdcall;

  end;
  IID_ICopyHookW = ICopyHookW;
  {$EXTERNALSYM IID_ICopyHookW}

  LPCOPYHOOKW = ^ICopyHookW;
  {$EXTERNALSYM LPCOPYHOOKW}

  {$IFDEF UNICODE}
  ICopyHook = ICopyHookW;
  {$EXTERNALSYM ICopyHook}
  LPCOPYHOOK = LPCOPYHOOKW;
  {$EXTERNALSYM LPCOPYHOOK}
  {$ELSE}
  ICopyHook = ICopyHookA;
  {$EXTERNALSYM ICopyHook}
  LPCOPYHOOK = LPCOPYHOOKA;
  {$EXTERNALSYM LPCOPYHOOK}
  {$ENDIF}

  // Interface ICurrentWorkingDirectory
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICurrentWorkingDirectory);'}
  {$EXTERNALSYM ICurrentWorkingDirectory}
  ICurrentWorkingDirectory = interface(IUnknown)
    ['{91956D21-9276-11D1-921A-006097DF5BD4}']

    function GetDirectory(pwzPath: LPWSTR;
                          cchSize: DWORD): HRESULT; stdcall;

    function SetDirectory(pwzPath: LPCWSTR): HRESULT; stdcall;

  end;
  IID_ICurrentWorkingDirectory = ICurrentWorkingDirectory;
  {$EXTERNALSYM IID_ICurrentWorkingDirectory}

  // Interface IDockingWindowFrame
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDockingWindowFrame);'}
  {$EXTERNALSYM IDockingWindowFrame}
  IDockingWindowFrame = interface(IOleWindow)
    ['{47D2657A-7B27-11D0-8CA9-00A0C92DBFE8}']

    function AddToolbar(punkSrc: IUnknown;
                        pwszItem: LPCWSTR;
                        dwAddFlags: DWORD): HRESULT; stdcall;

    function RemoveToolbar(punkSrc: IUnknown;
                           dwRemoveFlags: DWORD): HRESULT; stdcall;

    function FindToolbar(pwszItem: LPCWSTR;
                         const riid: IID;
                         out ppv): HRESULT; stdcall;

  end;
  IID_IDockingWindowFrame = IDockingWindowFrame;
  {$EXTERNALSYM IID_IDockingWindowFrame}

  // Interface IThumbnailCapture
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IThumbnailCapture);'}
  {$EXTERNALSYM IThumbnailCapture}
  IThumbnailCapture = interface(IUnknown)
    ['{4EA39266-7211-409F-B622-F63DBD16C533}']

    function CaptureThumbnail(const pMaxSize: SIZE;
                              pHTMLDoc2: IUnknown;
                              out phbmThumbnail: HBITMAP): HRESULT; stdcall;

  end;
  IID_IThumbnailCapture = IThumbnailCapture;
  {$EXTERNALSYM IID_IThumbnailCapture}

  PBANDINFOSFB = ^BANDINFOSFB;
  {$EXTERNALSYM PBANDINFOSFB}
  BANDINFOSFB = record
    dwMask: DWORD;
    dwStateMask: DWORD;
    dwState: DWORD;
    crBkgnd: COLORREF;
    crBtnLt: COLORREF;
    crBtnDk: COLORREF;
    wViewMode: WORD;
    wAlign: WORD;
    psf: IShellFolder;
    pidl: PIDLIST_ABSOLUTE;
  end;
  {$EXTERNALSYM BANDINFOSFB}

  // Interface IShellFolderBand
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellFolderBand);'}
  {$EXTERNALSYM IShellFolderBand}
  IShellFolderBand = interface(IUnknown)
    ['{7FE80CC8-C247-11D0-B93A-00A0C90312E1}']

    function InitializeSFB(psf: IShellFolder;
                           pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function SetBandInfoSFB(pbi: PBANDINFOSFB): HRESULT; stdcall;

    function GetBandInfoSFB(pbi: PBANDINFOSFB): HRESULT; stdcall;

  end;
  IID_IShellFolderBand = IShellFolderBand;
  {$EXTERNALSYM IID_IShellFolderBand}

  // Interface IDeskBarClient
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeskBarClient);'}
  {$EXTERNALSYM IDeskBarClient}
  IDeskBarClient = interface(IOleWindow)
    ['{EB0FE175-1A3A-11D0-89B3-00A0C90A90AC}']

    function SetDeskBarSite(punkSite: IUnknown): HRESULT; stdcall;

    function SetModeDBC(dwMode: DWORD): HRESULT; stdcall;

    function UIActivateDBC(dwState: DWORD): HRESULT; stdcall;

    function GetSize(dwWhich: DWORD;
                     prc: LPRECT): HRESULT; stdcall;

  end;
  IID_IDeskBarClient = IDeskBarClient;
  {$EXTERNALSYM IID_IDeskBarClient}

  // Interface IActiveDesktopP
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IActiveDesktopP);'}
  {$EXTERNALSYM IActiveDesktopP}
  IActiveDesktopP = interface(IUnknown)
    ['{52502EE0-EC80-11D0-89AB-00C04FC2972D}']

    function SetSafeMode(dwFlags: DWORD): HRESULT; stdcall;

    function EnsureUpdateHTML(): HRESULT; stdcall;

    function SetScheme(pwszSchemeName: LPCWSTR;
                       dwFlags: DWORD): HRESULT; stdcall;

    function GetScheme(pwszSchemeName: LPWSTR;
                       var pdwcchBuffer: DWORD;
                       dwFlags: DWORD): HRESULT; stdcall;

  end;
  IID_IActiveDesktopP = IActiveDesktopP;
  {$EXTERNALSYM IID_IActiveDesktopP}

  LPACTIVEDESKTOPP = ^IActiveDesktopP;
  {$EXTERNALSYM LPACTIVEDESKTOPP}

  // Interface IADesktopP2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IADesktopP2);'}
  {$EXTERNALSYM IADesktopP2}
  IADesktopP2 = interface(IUnknown)
    ['{B22754E2-4574-11D1-9888-006097DEACF9}']

    function ReReadWallpaper(): HRESULT; stdcall;

    function GetADObjectFlags(out pdwFlags: DWORD;
                              dwMask: DWORD): HRESULT; stdcall;

    function UpdateAllDesktopSubscriptions(): HRESULT; stdcall;

    function MakeDynamicChanges(pOleObj: IOleObject): HRESULT; stdcall;

  end;
  IID_IADesktopP2 = IADesktopP2;
  {$EXTERNALSYM IID_IADesktopP2}

  LPADESKTOPP2 = ^IADesktopP2;
  {$EXTERNALSYM LPADESKTOPP2}

  // Interface IColumnProvider
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IColumnProvider);'}
  {$EXTERNALSYM IColumnProvider}
  IColumnProvider = interface(IUnknown)
    ['{E8025004-1C42-11D2-BE2C-00A0C9A83DA1}']

    function Initialize(psci: LPCSHCOLUMNINIT): HRESULT; stdcall;

    function GetColumnInfo(dwIndex: DWORD;
                           out psci: SHCOLUMNINFO): HRESULT; stdcall;

    function GetItemData(pscid: LPCSHCOLUMNID;
                         pscd: LPCSHCOLUMNDATA;
                         out pvarData: VARIANT): HRESULT; stdcall;

  end;
  IID_IColumnProvider = IColumnProvider;
  {$EXTERNALSYM IID_IColumnProvider}

  // Interface IDocViewSite
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDocViewSite);'}
  {$EXTERNALSYM IDocViewSite}
  IDocViewSite = interface(IUnknown)
    ['{87D605E0-C511-11CF-89A9-00A0C9054129}']

    function OnSetTitle(pvTitle: PVariantArg): HRESULT; stdcall;

  end;
  IID_IDocViewSite = IDocViewSite;
  {$EXTERNALSYM IID_IDocViewSite}

  // Interface IInitializeObject
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeObject);'}
  {$EXTERNALSYM IInitializeObject}
  IInitializeObject = interface(IUnknown)
    ['{4622AD16-FF23-11D0-8D34-00A0C90F2719}']

    function Initialize(): HRESULT; stdcall;

  end;
  IID_IInitializeObject = IInitializeObject;
  {$EXTERNALSYM IID_IInitializeObject}

  // Interface IBanneredBar
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBanneredBar);'}
  {$EXTERNALSYM IBanneredBar}
  IBanneredBar = interface(IUnknown)
    ['{596A9A94-013E-11D1-8D34-00A0C90F2719}']

    function SetIconSize(iIcon: DWORD): HRESULT; stdcall;

    function GetIconSize(out piIcon: DWORD): HRESULT; stdcall;

    function SetBitmap(hBitmap: HBITMAP): HRESULT; stdcall;

    function GetBitmap(out phBitmap: HBITMAP): HRESULT; stdcall;

  end;
  IID_IBanneredBar = IBanneredBar;
  {$EXTERNALSYM IID_IBanneredBar}

procedure SHChangeNotifyRegisterThread(status: SCNRT_STATUS); stdcall;
{$EXTERNALSYM SHChangeNotifyRegisterThread}

function PathQualify(psz: PWSTR): HRESULT; stdcall;
{$EXTERNALSYM PathQualify}

function PathIsSlowA(pszFile: LPCSTR;
                     dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlowA}

function PathIsSlowW(pszFile: LPCWSTR;
                     dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlowW}

{$IFDEF UNICODE}
function PathIsSlow(pszFile: LPCWSTR;
                    dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlow}
{$ELSE}
function PathIsSlow(pszFile: LPCSTR;
                    dwAttr: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSlow}
{$ENDIF}

function GetFileNameFromBrowse(hwnd: HWND;
                               pszFilePath: PWSTR;
                               cchFilePath: UINT;
                               pszWorkingDir: PCWSTR;
                               pszDefExt: PCWSTR;
                               pszFilters: PCWSTR;
                               pszTitle: PCWSTR): BOOL; stdcall;
{$EXTERNALSYM GetFileNameFromBrowse}

function DriveType(iDrive: Integer): Integer; stdcall;
{$EXTERNALSYM DriveType}

function SHCreatePropSheetExtArray(hKey: HKEY;
                                   pszSubKey: PCWSTR;
                                   max_iface: UINT): HPSXA; stdcall;
{$EXTERNALSYM SHCreatePropSheetExtArray}

function SHOpenPropSheetA(pszCaption: LPCSTR;
                          ahkeys: PHKEY;
                          ckeys: UINT;
                          pclsidDefault: PCLSID;
                          pdtobj: IDataObject;
                          psb: IShellBrowser;
                          pStartPage: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SHOpenPropSheetA}

function SHOpenPropSheetW(pszCaption: LPCWSTR;
                          ahkeys: PHKEY;
                          ckeys: UINT;
                          pclsidDefault: PCLSID;
                          pdtobj: IDataObject;
                          psb: IShellBrowser;
                          pStartPage: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM SHOpenPropSheetW}

function SHShellFolderView_Message(hwndMain: HWND;
                                   uMsg: UINT;
                                   lParam: LPARAM): LRESULT; stdcall;
{$EXTERNALSYM SHShellFolderView_Message}

function SHMultiFileProperties(pdtobj: IDataObject;
                               dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM SHMultiFileProperties}

function SHCreateQueryCancelAutoPlayMoniker(out ppmoniker: IMoniker): HRESULT; stdcall;
{$EXTERNALSYM SHCreateQueryCancelAutoPlayMoniker}

procedure PerUserInit(); stdcall;
{$EXTERNALSYM PerUserInit}

function SHRunControlPanel(lpcszCmdLine: PCWSTR;
                           hwndMsgParent: HWND): BOOL; stdcall;
{$EXTERNALSYM SHRunControlPanel}

function ImportPrivacySettings(pszFilename: PCWSTR;
                               var pfParsePrivacyPreferences: BOOL;
                               var pfParsePerSiteRules: BOOL): BOOL; stdcall;
{$EXTERNALSYM ImportPrivacySettings}

// Moved to additiona implementatations in this unit.
//function DoPrivacyDlg(hwndOwner: HWND;
//                      pszUrl: PCWSTR;
//                      pPrivacyEnum: IEnumPrivacyRecords;
//                      fReportAllSites: BOOL): HRESULT; stdcall;
//{$EXTERNALSYM DoPrivacyDlg}

{$IFDEF UNICODE}
function SHOpenPropSheet(pszCaption: LPCWSTR;
                         ahkeys: PHKEY;
                         ckeys: UINT;
                         pclsidDefault: PCLSID;
                         pdtobj: IDataObject;
                         psb: IShellBrowser;
                         pStartPage: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM SHOpenPropSheet}
{$ELSE}
function SHOpenPropSheet(pszCaption: LPCSTR;
                         ahkeys: PHKEY;
                         ckeys: UINT;
                         pclsidDefault: PCLSID;
                         pdtobj: IDataObject;
                         psb: IShellBrowser;
                         pStartPage: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SHOpenPropSheet}
{$ENDIF}


// Additional Prototypes for ALL interfaces

type
  // Small Mshtml.h/Mshtml.idl dependency used by DoPrivacyDlg().
  // ShlObj.h only forward-declares this interface, but Delphi needs the
  // complete interface before the implementation section.
  // imported from Mshtml.h
  IEnumPrivacyRecords = interface(IUnknown)
    ['{3050F842-98B5-11CF-BB82-00AA00BDCE0B}']
    function Reset: HRESULT; stdcall;
    function GetSize(out pSize: ULONG): HRESULT; stdcall;
    function GetPrivacyImpacted(out pState: BOOL): HRESULT; stdcall;
    function Next(out pbstrUrl: BSTR;
                  out pbstrPolicyRef: BSTR;
                  pdwReserved: PLongint;
                  out pdwFlags: DWORD): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumPrivacyRecords}

  function DoPrivacyDlg(hwndOwner: HWND;
                      pszUrl: PCWSTR;
                      pPrivacyEnum: IEnumPrivacyRecords;
                      fReportAllSites: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DoPrivacyDlg}

// End of Additional Prototypes


implementation

const
  Shell32Lib = 'shell32.dll';


{$WARN SYMBOL_PLATFORM OFF}

procedure SHChangeNotifyRegisterThread;   external Shell32Lib name 'SHChangeNotifyRegisterThread' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PathQualify;                     external Shell32Lib name 'PathQualify' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PathIsSlowA;                     external Shell32Lib name 'PathIsSlowA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PathIsSlowW;                     external Shell32Lib name 'PathIsSlowW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$IFDEF UNICODE}
function PathIsSlow;                      external Shell32Lib name 'PathIsSlowW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$ELSE}
function PathIsSlow;                      external Shell32Lib name 'PathIsSlowA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$ENDIF}

function GetFileNameFromBrowse;           external Shell32Lib name 'GetFileNameFromBrowse' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function DriveType;                       external Shell32Lib name 'DriveType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreatePropSheetExtArray;       external Shell32Lib name 'SHCreatePropSheetExtArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHOpenPropSheetA;                external Shell32Lib name 'SHOpenPropSheetA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHOpenPropSheetW;                external Shell32Lib name 'SHOpenPropSheetW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$IFDEF UNICODE}
function SHOpenPropSheet;                 external Shell32Lib name 'SHOpenPropSheetW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$ELSE}
function SHOpenPropSheet;                 external Shell32Lib name 'SHOpenPropSheetA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$ENDIF}

function SHShellFolderView_Message;       external Shell32Lib name 'SHShellFolderView_Message' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHMultiFileProperties;           external Shell32Lib name 'SHMultiFileProperties' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateQueryCancelAutoPlayMoniker; external Shell32Lib name 'SHCreateQueryCancelAutoPlayMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure PerUserInit;                    external Shell32Lib name 'PerUserInit' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHRunControlPanel;               external Shell32Lib name 'SHRunControlPanel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function ImportPrivacySettings;           external Shell32Lib name 'ImportPrivacySettings' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function DoPrivacyDlg;                    external Shell32Lib name 'DoPrivacyDlg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.

