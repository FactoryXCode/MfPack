// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ShTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 25-09-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Remarks: Requires on some points Windows Vista or later.
// 
// 
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
// Source: shtypes.h
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
unit WinApi.ShTypes;

  {$HPPEMIT '#include "shtypes.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


//* Forward Declarations */

//===========================================================================
//
// Object identifiers in the explorer's name space (ItemID and IDList)
//
//  All the items that the user can browse with the explorer (such as files,
// directories, servers, work-groups, etc.) has an identifier which is unique
// among items within the parent folder. Those identifiers are called item
// IDs (SHITEMID). Since all its parent folders have their own item IDs,
// any items can be uniquely identified by a list of item IDs, which is called
// an ID list (ITEMIDLIST).
//
//  ID lists are almost always allocated by the task allocator (see some
// description below as well as OLE 2.0 SDK) and may be passed across
// some of shell interfaces (such as IShellFolder). Each item ID in an ID list
// is only meaningful to its parent folder (which has generated it), and all
// the clients must treat it as an opaque binary data except the first two
// bytes, which indicates the size of the item ID.
//
//  When a shell extension -- which implements the IShellFolder interace --
// generates an item ID, it may put any information in it, not only the data
// with that it needs to identifies the item, but also some additional
// information, which would help implementing some other functions efficiently.
// For example, the shell's IShellFolder implementation of file system items
// stores the primary (long) name of a file or a directory as the item
// identifier, but it also stores its alternative (short) name, size and date
// etc.
//
//  When an ID list is passed to one of shell APIs (such as SHGetPathFromIDList),
// it is always an absolute path -- relative from the root of the name space,
// which is the desktop folder. When an ID list is passed to one of IShellFolder
// member function, it is always a relative path from the folder (unless it
// is explicitly specified).
//
//===========================================================================
//
// SHITEMID -- Item ID  (mkid)
//     USHORT      cb;             // Size of the ID (including cb itself)
//     BYTE        abID[];         // The item ID (variable length)
//
type

  LPSHITEMID = ^_SHITEMID;
  LPCSHITEMID = ^_SHITEMID;
  PSHITEMID = ^_SHITEMID;
  _SHITEMID = record
    cb: USHORT;
    abID: array [0..255] of Byte;
  end;
  {$EXTERNALSYM _SHITEMID}
  SHITEMID = _SHITEMID;
  {$EXTERNALSYM SHITEMID}


//
// ITEMIDLIST -- List if item IDs (combined with 0-terminator)
//

  PItemidlist = ^_ITEMIDLIST;
  _ITEMIDLIST = record
    mkid: SHITEMID;
  end;
  {$EXTERNALSYM _ITEMIDLIST}
  ITEMIDLIST = _ITEMIDLIST;
  {$EXTERNALSYM ITEMIDLIST}

  PItemidlistRelative = ^ITEMIDLIST_RELATIVE;
  ITEMIDLIST_RELATIVE = ITEMIDLIST;
  {$EXTERNALSYM ITEMIDLIST_RELATIVE}

  PItemidChild = ^ITEMID_CHILD;
  ITEMID_CHILD = ITEMIDLIST;
  {$EXTERNALSYM ITEMID_CHILD}

  PItemidlistAbsolute = ^ITEMIDLIST_ABSOLUTE;
  ITEMIDLIST_ABSOLUTE = ITEMIDLIST;
  {$EXTERNALSYM ITEMIDLIST_ABSOLUTE}

  PByteBlob = ^BYTE_BLOB;
  BYTE_BLOB = pointer;
  {$EXTERNALSYM BYTE_BLOB}

  LPITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM LPITEMIDLIST}
  LPCITEMIDLIST = ^ITEMIDLIST;
  {$EXTERNALSYM LPCITEMIDLIST}

  PIDLIST_ABSOLUTE                    = LPITEMIDLIST;
  {$EXTERNALSYM PIDLIST_ABSOLUTE}
  PCIDLIST_ABSOLUTE                   = LPCITEMIDLIST;
  {$EXTERNALSYM PCIDLIST_ABSOLUTE}
  PCUIDLIST_ABSOLUTE                  = LPCITEMIDLIST;
  {$EXTERNALSYM PCUIDLIST_ABSOLUTE}
  PIDLIST_RELATIVE                    = LPITEMIDLIST;
  {$EXTERNALSYM PIDLIST_RELATIVE}
  PCIDLIST_RELATIVE                   = LPCITEMIDLIST;
  {$EXTERNALSYM PCIDLIST_RELATIVE}
  PUIDLIST_RELATIVE                   = LPITEMIDLIST;
  {$EXTERNALSYM PUIDLIST_RELATIVE}
  PCUIDLIST_RELATIVE                  = LPCITEMIDLIST;
  {$EXTERNALSYM PCUIDLIST_RELATIVE}
  PITEMID_CHILD                       = LPITEMIDLIST;
  {$EXTERNALSYM PITEMID_CHILD}
  PCITEMID_CHILD                      = LPCITEMIDLIST;
  {$EXTERNALSYM PCITEMID_CHILD}
  PUITEMID_CHILD                      = LPITEMIDLIST;
  {$EXTERNALSYM PUITEMID_CHILD}
  PCUITEMID_CHILD                     = LPCITEMIDLIST;
  {$EXTERNALSYM PCUITEMID_CHILD}
  PCUITEMID_CHILD_ARRAY               = LPCITEMIDLIST;
  {$EXTERNALSYM PCUITEMID_CHILD_ARRAY}
  PCUIDLIST_RELATIVE_ARRAY            = LPCITEMIDLIST;
  {$EXTERNALSYM PCUIDLIST_RELATIVE_ARRAY}
  PCIDLIST_ABSOLUTE_ARRAY             = LPCITEMIDLIST;
  {$EXTERNALSYM PCIDLIST_ABSOLUTE_ARRAY}
  PCUIDLIST_ABSOLUTE_ARRAY            = LPCITEMIDLIST;
  {$EXTERNALSYM PCUIDLIST_ABSOLUTE_ARRAY}

type

  PWIN32_FIND_DATAA = ^_WIN32_FIND_DATAA;
  LPWIN32_FIND_DATAA = ^_WIN32_FIND_DATAA;
  {$EXTERNALSYM LPWIN32_FIND_DATAA}
  PWin32FindDataa = ^_WIN32_FIND_DATAA;
  _WIN32_FIND_DATAA = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..259] of AnsiChar;
    cAlternateFileName: array[0..13] of AnsiChar;
  end;
  {$EXTERNALSYM _WIN32_FIND_DATAA}
  WIN32_FIND_DATAA = _WIN32_FIND_DATAA;
  {$EXTERNALSYM WIN32_FIND_DATAA}



  PWIN32_FIND_DATAW = ^_WIN32_FIND_DATAW;
  LPWIN32_FIND_DATAW = ^_WIN32_FIND_DATAW;
  {$EXTERNALSYM LPWIN32_FIND_DATAW}
  PWin32FindDataw = ^_WIN32_FIND_DATAW;
  _WIN32_FIND_DATAW = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..259] of WideChar;
    cAlternateFileName: array[0..13] of WideChar;
  end;
  {$EXTERNALSYM _WIN32_FIND_DATAW}
  WIN32_FIND_DATAW = _WIN32_FIND_DATAW;
  {$EXTERNALSYM WIN32_FIND_DATAW}


//-------------------------------------------------------------------------
//
// struct STRRET
//
// structure for returning strings from IShellFolder member functions
//
//-------------------------------------------------------------------------
//
//  uType indicate which union member to use
//    STRRET_WSTR    Use STRRET.pOleStr     must be freed by caller of GetDisplayNameOf
//    STRRET_OFFSET  Use STRRET.uOffset     Offset into SHITEMID for ANSI string
//    STRRET_CSTR    Use STRRET.cStr        ANSI Buffer
//
  PStrretType = ^tagSTRRET_TYPE;
  tagSTRRET_TYPE  = (
    STRRET_WSTR   = 0,
    STRRET_OFFSET = $1,
    STRRET_CSTR   = $2);
  {$EXTERNALSYM tagSTRRET_TYPE}
  STRRET_TYPE = tagSTRRET_TYPE;
  {$EXTERNALSYM STRRET_TYPE}


   // Contains strings returned from the IShellFolder interface methods.
   // To convert this structure to a string, use StrRetToBuf or StrRetToStr (Shlwapi.h).
  LPSTRRET = ^STRRET;
  PSTRRET = ^_STRRET;
  _STRRET = record
    uType: UINT; //
    case integer of
      0: (pOleStr: PWideChar;);
      1: (uOffset: UINT;);
      2: (cStr: array[0..MAX_PATH -1 {=259}] of AnsiChar;);
    end;
  {$EXTERNALSYM _STRRET}
  STRRET = _STRRET;
  {$EXTERNALSYM STRRET}
  //Members
  // uType: UINT;
  //  A value that specifies the desired format of the string.
  //  This can be one of the following values.
  //    STRRET_CSTR
  //      The string is returned in the cStr member.
  //    STRRET_OFFSET
  //      The uOffset member value indicates the number of bytes from the beginning of the item identifier list where the string is located.
  //    STRRET_WSTR
  //      The string is at the address specified by pOleStr member.
  //
  // pOleStr
  //  Type: LPWSTR
  //   A pointer to the string.
  //   This memory must be allocated with CoTaskMemAlloc.
  //   It is the calling application's responsibility to free this memory
  //   with CoTaskMemFree when it is no longer needed.
  // uOffset
  //  Type: UINT
  //   The offset into the item identifier list.
  // cStr
  //  Type: CHAR[MAX_PATH]
  //   The buffer to receive the display name.




//-------------------------------------------------------------------------
//
// struct SHELLDETAILS
//
// structure for returning strings from IShellDetails
//
//-------------------------------------------------------------------------
//
//  fmt;            // LVCFMT_* value (header only)
//  cxChar;         // Number of 'average' characters (header only)
//  str;            // String information
//

type

  LPSHELLDETAILS = ^_SHELLDETAILS;
  PShelldetails = ^_SHELLDETAILS;
  _SHELLDETAILS = record
    fmt: Integer;
    cxChar: Integer;
    str: STRRET;
  end;
  {$EXTERNALSYM _SHELLDETAILS}
  SHELLDETAILS = _SHELLDETAILS;
  {$EXTERNALSYM SHELLDETAILS}


  PPerceived = ^PERCEIVED;
  tagPERCEIVED                 = (
    PERCEIVED_TYPE_FIRST       = - 3,
    PERCEIVED_TYPE_CUSTOM      = - 3,
    PERCEIVED_TYPE_UNSPECIFIED = - 2,
    PERCEIVED_TYPE_FOLDER      = - 1,
    PERCEIVED_TYPE_UNKNOWN     = 0,
    PERCEIVED_TYPE_TEXT        = 1,
    PERCEIVED_TYPE_IMAGE       = 2,
    PERCEIVED_TYPE_AUDIO       = 3,
    PERCEIVED_TYPE_VIDEO       = 4,
    PERCEIVED_TYPE_COMPRESSED  = 5,
    PERCEIVED_TYPE_DOCUMENT    = 6,
    PERCEIVED_TYPE_SYSTEM      = 7,
    PERCEIVED_TYPE_APPLICATION = 8,
    PERCEIVED_TYPE_GAMEMEDIA   = 9,
    PERCEIVED_TYPE_CONTACTS    = 10,
    PERCEIVED_TYPE_LAST        = 10);
  {$EXTERNALSYM tagPERCEIVED}
  PERCEIVED = tagPERCEIVED;
  {$EXTERNALSYM PERCEIVED}


const

  PERCEIVEDFLAG_UNDEFINED             = $0000;
  {$EXTERNALSYM PERCEIVEDFLAG_UNDEFINED}
  PERCEIVEDFLAG_SOFTCODED             = $0001;
  {$EXTERNALSYM PERCEIVEDFLAG_SOFTCODED}
  PERCEIVEDFLAG_HARDCODED             = $0002;
  {$EXTERNALSYM PERCEIVEDFLAG_HARDCODED}
  PERCEIVEDFLAG_NATIVESUPPORT         = $0004;
  {$EXTERNALSYM PERCEIVEDFLAG_NATIVESUPPORT}
  PERCEIVEDFLAG_GDIPLUS               = $0010;
  {$EXTERNALSYM PERCEIVEDFLAG_GDIPLUS}
  PERCEIVEDFLAG_WMSDK                 = $0020;
  {$EXTERNALSYM PERCEIVEDFLAG_WMSDK}
  PERCEIVEDFLAG_ZIPFOLDER             = $0040;
  {$EXTERNALSYM PERCEIVEDFLAG_ZIPFOLDER}

type

  PPERCEIVEDFLAG = ^PERCEIVEDFLAG;
  PERCEIVEDFLAG = DWORD;
  {$EXTERNALSYM PERCEIVEDFLAG}


// >= Vista
  PComdlgFilterspec = ^COMDLG_FILTERSPEC;
  _COMDLG_FILTERSPEC = record
    pszName: PWideChar;
    { [string] }
    pszSpec: PWideChar;
    { [string] }
  end;
  {$EXTERNALSYM _COMDLG_FILTERSPEC}
  COMDLG_FILTERSPEC = _COMDLG_FILTERSPEC;
  {$EXTERNALSYM COMDLG_FILTERSPEC}


  PKnownfolderid = ^KNOWNFOLDERID;
  KNOWNFOLDERID = TGUID;
  {$EXTERNALSYM KNOWNFOLDERID}

  REFKNOWNFOLDERID = ^KNOWNFOLDERID;

  PKF_REDIRECT_FLAGS = ^KF_REDIRECT_FLAGS;
  KF_REDIRECT_FLAGS = DWORD;
  {$EXTERNALSYM KF_REDIRECT_FLAGS}

  PFOLDERTYPEID = ^FOLDERTYPEID;
  FOLDERTYPEID = TGUID;
  {$EXTERNALSYM FOLDERTYPEID}

  REFFOLDERTYPEID = ^FOLDERTYPEID;

  PTaskownerid = ^TASKOWNERID;
  TASKOWNERID = TGUID;
  {$EXTERNALSYM TASKOWNERID}

  REFTASKOWNERID = ^TASKOWNERID;

  PElementid = ^ELEMENTID;
  ELEMENTID = TGUID;
  {$EXTERNALSYM ELEMENTID}

  REFELEMENTID = ^ELEMENTID;

  PLogfonta = ^LOGFONTA;
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
    lfFaceName: array[0..31] of AnsiChar;
  end;
  {$EXTERNALSYM tagLOGFONTA}
  LOGFONTA = tagLOGFONTA;
  {$EXTERNALSYM LOGFONTA}


  PLogfontw = ^LOGFONTW;
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
    lfFaceName: array[0..31] of WideChar;
  end;
  {$EXTERNALSYM tagLOGFONTW}
  LOGFONTW = tagLOGFONTW;
  {$EXTERNALSYM LOGFONTW}

  LOGFONT = LOGFONTW;
  {$EXTERNALSYM LOGFONT}
  PLogfont = ^LOGFONT;

  PShcolstate = ^SHCOLSTATE;
  tagSHCOLSTATE                   = (
    SHCOLSTATE_DEFAULT            = 0,
    SHCOLSTATE_TYPE_STR           = $1,
    SHCOLSTATE_TYPE_INT           = $2,
    SHCOLSTATE_TYPE_DATE          = $3,
    SHCOLSTATE_TYPEMASK           = $F,
    SHCOLSTATE_ONBYDEFAULT        = $10,
    SHCOLSTATE_SLOW               = $20,
    SHCOLSTATE_EXTENDED           = $40,
    SHCOLSTATE_SECONDARYUI        = $80,
    SHCOLSTATE_HIDDEN             = $100,
    SHCOLSTATE_PREFER_VARCMP      = $200,
    SHCOLSTATE_PREFER_FMTCMP      = $400,
    SHCOLSTATE_NOSORTBYFOLDERNESS = $800,
    SHCOLSTATE_VIEWONLY           = $10000,
    SHCOLSTATE_BATCHREAD          = $20000,
    SHCOLSTATE_NO_GROUPBY         = $40000,
    SHCOLSTATE_FIXED_WIDTH        = $1000,
    SHCOLSTATE_NODPISCALE         = $2000,
    SHCOLSTATE_FIXED_RATIO        = $4000,
    SHCOLSTATE_DISPLAYMASK        = $F000);
  {$EXTERNALSYM tagSHCOLSTATE}
  SHCOLSTATE = tagSHCOLSTATE;
  {$EXTERNALSYM SHCOLSTATE}

  PShcolstatef = ^SHCOLSTATEF;
  SHCOLSTATEF = DWORD;
  {$EXTERNALSYM SHCOLSTATEF}

  PShcolumnid = ^SHCOLUMNID;
  SHCOLUMNID = PROPERTYKEY;
  {$EXTERNALSYM SHCOLUMNID}

  LPCSHCOLUMNID = ^SHCOLUMNID;

  PDEVICE_SCALE_FACTOR = ^DEVICE_SCALE_FACTOR;
  DEVICE_SCALE_FACTOR           = (
    DEVICE_SCALE_FACTOR_INVALID = 0,
    SCALE_100_PERCENT           = 100,
    SCALE_120_PERCENT           = 120,
    SCALE_125_PERCENT           = 125,
    SCALE_140_PERCENT           = 140,
    SCALE_150_PERCENT           = 150,
    SCALE_160_PERCENT           = 160,
    SCALE_175_PERCENT           = 175,
    SCALE_180_PERCENT           = 180,
    SCALE_200_PERCENT           = 200,
    SCALE_225_PERCENT           = 225,
    SCALE_250_PERCENT           = 250,
    SCALE_300_PERCENT           = 300,
    SCALE_350_PERCENT           = 350,
    SCALE_400_PERCENT           = 400,
    SCALE_450_PERCENT           = 450,
    SCALE_500_PERCENT           = 500);
  {$EXTERNALSYM DEVICE_SCALE_FACTOR}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
