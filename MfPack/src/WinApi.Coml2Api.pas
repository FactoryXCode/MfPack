// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Coml2Api.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Structured storage, property sets, and related APIs.
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
// Source: coml2api.h
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
unit WinApi.Coml2Api;

  {$HPPEMIT '#include "coml2api.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.PropIdl;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const
  //
  // Common typedefs for paramaters used in Storage API's, gleamed from storage.h
  // Also contains Storage error codes, which should be moved into the storage
  // idl files.
  //

  CWCSTORAGENAME                      = 32;
  {$EXTERNALSYM CWCSTORAGENAME}

  //* Storage instantiation modes *//

  STGM_DIRECT                         = $00000000;
  {$EXTERNALSYM STGM_DIRECT}
  STGM_TRANSACTED                     = $00010000;
  {$EXTERNALSYM STGM_TRANSACTED}
  STGM_SIMPLE                         = $08000000;
  {$EXTERNALSYM STGM_SIMPLE}

  STGM_READ                           = $00000000;
  {$EXTERNALSYM STGM_READ}
  STGM_WRITE                          = $00000001;
  {$EXTERNALSYM STGM_WRITE}
  STGM_READWRITE                      = $00000002;
  {$EXTERNALSYM STGM_READWRITE}

  STGM_SHARE_DENY_NONE                = $00000040;
  {$EXTERNALSYM STGM_SHARE_DENY_NONE}
  STGM_SHARE_DENY_READ                = $00000030;
  {$EXTERNALSYM STGM_SHARE_DENY_READ}
  STGM_SHARE_DENY_WRITE               = $00000020;
  {$EXTERNALSYM STGM_SHARE_DENY_WRITE}
  STGM_SHARE_EXCLUSIVE                = $00000010;
  {$EXTERNALSYM STGM_SHARE_EXCLUSIVE}

  STGM_PRIORITY                       = $00040000;
  {$EXTERNALSYM STGM_PRIORITY}
  STGM_DELETEONRELEASE                = $04000000;
  {$EXTERNALSYM STGM_DELETEONRELEASE}

//#if (WINVER >= 400)
  STGM_NOSCRATCH                      = $00100000;
  {$EXTERNALSYM STGM_NOSCRATCH}
//#endif /* WINVER */

  STGM_CREATE                         = $00001000;
  {$EXTERNALSYM STGM_CREATE}
  STGM_CONVERT                        = $00020000;
  {$EXTERNALSYM STGM_CONVERT}
  STGM_FAILIFTHERE                    = $00000000;
  {$EXTERNALSYM STGM_FAILIFTHERE}

  STGM_NOSNAPSHOT                     = $00200000;
  {$EXTERNALSYM STGM_NOSNAPSHOT}
//#if (_WIN32_WINNT >= 0x0500)
  STGM_DIRECT_SWMR                    = $00400000;
  {$EXTERNALSYM STGM_DIRECT_SWMR}
//#endif

type
  PSTGFMT = ^STGFMT;
  STGFMT = DWORD;
  {$EXTERNALSYM STGFMT}
const
  STGFMT_STORAGE                      = STGFMT(0);
  {$EXTERNALSYM STGFMT_STORAGE}
  STGFMT_NATIVE                       = STGFMT(1);
  {$EXTERNALSYM STGFMT_NATIVE}
  STGFMT_FILE                         = STGFMT(3);
  {$EXTERNALSYM STGFMT_FILE}
  STGFMT_ANY                          = STGFMT(4);
  {$EXTERNALSYM STGFMT_ANY}
  STGFMT_DOCFILE                      = STGFMT(5);
  {$EXTERNALSYM STGFMT_DOCFILE}


  // This is a legacy define to allow old component to builds

  STGFMT_DOCUMENT                     = 0;
  {$EXTERNALSYM STGFMT_DOCUMENT}

  // Structured storage APIs

  function StgCreateDocfile(pwcsName: POLESTR;
                            grfMode: DWORD;
                            reserved: DWORD;
                            out ppstgOpen: IStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreateDocfile}

  function StgCreateDocfileOnILockBytes(plkbyt: ILockBytes;
                                        grfMode: DWORD;
                                        reserved: DWORD;
                                        out ppstgOpen: IStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreateDocfileOnILockBytes}


  function StgOpenStorage(pwcsName: POLESTR;
                          stgPriority: IStorage;
                          grfMode: DWORD;
                          snbExclude: SNB;
                          reserved: DWORD): HResult; stdcall;
  {$EXTERNALSYM StgOpenStorage}

  function StgOpenStorageOnILockBytes(plkbyt: ILockBytes;
                                      pstgPriority: IStorage;
                                      grfMode: DWORD;
                                      snbExclude: SNB;
                                      reserved: DWORD;
                                      out ppstgOpen: IStorage): HResult; stdcall;
  {$EXTERNALSYM StgOpenStorageOnILockBytes}

  function StgIsStorageFile(pwcsName: POLESTR): HResult; stdcall;
  {$EXTERNALSYM StgIsStorageFile}

  function StgIsStorageILockBytes(plkbyt: ILockBytes): HResult; stdcall;
  {$EXTERNALSYM StgIsStorageILockBytes}


const
  // STG initialization options for StgCreateStorageEx and StgOpenStorageEx

//#if _WIN32_WINNT == 0x500
  //STGOPTIONS_VERSION                  = 1;
//#elif _WIN32_WINNT > 0x500
  STGOPTIONS_VERSION                  = 2;
  {$EXTERNALSYM STGOPTIONS_VERSION}
//#else
  //STGOPTIONS_VERSION                  = 0;

type
  PSTGOPTIONS = ^tagSTGOPTIONS;
  tagSTGOPTIONS = record
    usVersion: USHORT;               // Versions 1 and 2 supported
    reserved: USHORT;                // must be 0 for padding
    ulSectorSize: ULONG;             // docfile header sector size (512)
    pwcsTemplateFile: PWideChar;     // version 2 or above
  end;
  {$EXTERNALSYM tagSTGOPTIONS}
  STGOPTIONS = tagSTGOPTIONS;
  {$EXTERNALSYM STGOPTIONS}


  function StgCreateStorageEx(const pwcsName: PWideChar;
                              grfMode: DWORD;
                              stgfmt: DWORD;
                              grfAttrs: DWORD;
                              pStgOptions: PSTGOPTIONS;
                              pSecurityDescriptor: SECURITY_DESCRIPTOR;
                              const riid: TGUID;
                              out ppObjectOpen: Pointer): HResult; stdcall;
  {$EXTERNALSYM StgCreateStorageEx}


  function StgOpenStorageEx(const pwcsName: PWideChar;
                            grfMode: DWORD;
                            stgfmt: DWORD;
                            grfAttrs: DWORD;
                            pStgOptions: PSTGOPTIONS;
                            pSecurityDescriptor: PSECURITY_DESCRIPTOR;
                            const riid: TGUID;
                            out ppObjectOpen: Pointer): HResult; stdcall;
  {$EXTERNALSYM StgOpenStorageEx}

  function StgCreatePropStg(pUnk: IUnknown;
                            const fmtid: TGUID;
                            const pclsid: TGUID;
                            grfFlags: DWORD;
                            dwReserved: DWORD;
                            out ppPropStg: IPropertyStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreatePropStg}

  function StgOpenPropStg(pUnk: IUnknown;
                          const fmtid: TGUID;
                          grfFlags: DWORD;
                          dwReserved: DWORD;
                          out ppPropStg: IPropertyStorage): HResult; stdcall;
  {$EXTERNALSYM StgOpenPropStg}

  function StgCreatePropSetStg(pStorage: IStorage;
                               dwReserved: DWORD;
                               out ppPropSetStg: IPropertySetStorage): HResult; stdcall;
  {$EXTERNALSYM StgCreatePropSetStg}

const

  CCH_MAX_PROPSTG_NAME                = 31;
  {$EXTERNALSYM CCH_MAX_PROPSTG_NAME}


  function FmtIdToPropStgName(const pfmtid: TGUID;
                              oszName: POLESTR): HResult; stdcall;
  {$EXTERNALSYM FmtIdToPropStgName}

  function PropStgNameToFmtId(const oszName: POLESTR;
                              out pfmtid: FMTID): HResult; stdcall;
  {$EXTERNALSYM PropStgNameToFmtId}


  // Helper functions
  function ReadClassStg(pstg: IStorage;
                        out pclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM ReadClassStg}

  function WriteClassStg(pstg: IStorage;
                         const rclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM WriteClassStg}

  function ReadClassStm(pstm: IStream;
                        out clsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM ReadClassStm}

  function WriteClassStm(pstm: IStream;
                         const pclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM WriteClassStm}


  // Storage utility APIs

  function GetHGlobalFromILockBytes(plkbyt: ILockBytes;
                                    out _hGlobal: HGlobal): HResult; stdcall;
  {$EXTERNALSYM GetHGlobalFromILockBytes}

  function CreateILockBytesOnHGlobal(_hGlobal: HGlobal;
                                     fDeleteOnRelease: BOOL;
                                     out pplkbyt: ILockBytes): HResult; stdcall;
  {$EXTERNALSYM CreateILockBytesOnHGlobal}


  // ConvertTo APIs

  function GetConvertStg(stg: IStorage): HResult; stdcall;
  {$EXTERNALSYM GetConvertStg}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  Ole32Lib = 'Ole32.dll';

{$WARN SYMBOL_PLATFORM OFF}

function StgCreateDocfile;             external Ole32Lib name 'StgCreateDocfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgCreateDocfileOnILockBytes; external Ole32Lib name 'StgCreateDocfileOnILockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenStorage;               external Ole32Lib name 'StgOpenStorage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenStorageOnILockBytes;   external Ole32Lib name 'StgOpenStorageOnILockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgIsStorageFile;             external Ole32Lib name 'StgIsStorageFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgIsStorageILockBytes;       external Ole32Lib name 'StgIsStorageILockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function StgSetTimes(pszName: POLESTR;
                     ctime: PFILETIME;
                     atime: PFILETIME;
                     mtime: PFILETIME): HResult; external Ole32Lib name 'StgSetTimes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function StgCreateStorageEx;           external Ole32Lib name 'StgCreateStorageEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenStorageEx;             external Ole32Lib name 'StgCreatePropStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgCreatePropStg;             external Ole32Lib name 'StgCreatePropStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenPropStg;               external Ole32Lib name 'StgOpenPropStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgCreatePropSetStg;          external Ole32Lib name 'StgCreatePropSetStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function FmtIdToPropStgName;           external Ole32Lib name 'FmtIdToPropStgName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function PropStgNameToFmtId;           external Ole32Lib name 'PropStgNameToFmtId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function ReadClassStg;                 external Ole32Lib name 'ReadClassStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function WriteClassStg;                external Ole32Lib name 'WriteClassStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function ReadClassStm;                 external Ole32Lib name 'ReadClassStm' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function WriteClassStm;                external Ole32Lib name 'WriteClassStm' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetHGlobalFromILockBytes;     external Ole32Lib name 'GetHGlobalFromILockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateILockBytesOnHGlobal;    external Ole32Lib name 'CreateILockBytesOnHGlobal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetConvertStg;                external Ole32Lib name 'GetConvertStg' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.


end.
