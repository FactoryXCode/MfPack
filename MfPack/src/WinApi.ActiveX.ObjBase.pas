// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ActiveX.ObjBase.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: Component object model definitions.
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
// 25/10/2020 Tony                Fixed some pointer issues.
//------------------------------------------------------------------------------
//
// Remarks:
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
// Source: objbase.h
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
unit WinApi.ActiveX.ObjBase;

  {$HPPEMIT '#include "objbase.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.Win.ComObj,
  {ActiveX}
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.ObjIdlbase;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  // COM initialization flags; passed to CoInitialize.
  PCOINIT = ^tagCOINIT;
  {$EXTERNALSYM tagCOINIT}
  tagCOINIT = DWORD;
  {$EXTERNALSYM COINIT}
  COINIT = tagCOINIT;

const
  //COINIT_MULTITHREADED     = COINITBASE_MULTITHREADED; // combaseapi

  COINIT_MULTITHREADED     = $0; // = combaseapi COINITBASE_MULTITHREADED
  {$EXTERNALSYM COINIT_MULTITHREADED}
  COINIT_APARTMENTTHREADED = $2;   // Apartment model

  // These constants are only valid on Windows NT 4.0
  {$EXTERNALSYM COINIT_APARTMENTTHREADED}
  COINIT_DISABLE_OLE1DDE   = $4;   // Don't use DDE for Ole1 support.
  {$EXTERNALSYM COINIT_DISABLE_OLE1DDE}
  COINIT_SPEED_OVER_MEMORY = $8;   // Trade memory for speed.
  {$EXTERNALSYM COINIT_SPEED_OVER_MEMORY}


  // interface marshaling definitions
  MARSHALINTERFACE_MIN                = 500;  // minimum number of bytes for interface marshal
  {$EXTERNALSYM MARSHALINTERFACE_MIN}


  //*  flags for internet asynchronous and layout docfile *//
  ASYNC_MODE_COMPATIBILITY            = $00000001;
  {$EXTERNALSYM ASYNC_MODE_COMPATIBILITY}
  ASYNC_MODE_DEFAULT                  = $00000000;
  {$EXTERNALSYM ASYNC_MODE_DEFAULT}

  STGTY_REPEAT                        = $00000100;
  {$EXTERNALSYM STGTY_REPEAT}
  STG_TOEND                           = MAXDWORD;
  {$EXTERNALSYM STG_TOEND}

  STG_LAYOUT_SEQUENTIAL               = $00000000;
  {$EXTERNALSYM STG_LAYOUT_SEQUENTIAL}
  STG_LAYOUT_INTERLEAVED              = $00000001;
  {$EXTERNALSYM STG_LAYOUT_INTERLEAVED}



  //****** STD Object API Prototypes *****************************************//

  function CoBuildVersion(): DWORD; stdcall;
  {$EXTERNALSYM CoBuildVersion}

  function CoInitialize(pvReserved: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoInitialize}

  function CoRegisterMallocSpy(pmallocSpy: IMallocSpy): HResult; stdcall;
  {$EXTERNALSYM CoRegisterMallocSpy}

  function CoRevokeMallocSpy(): HResult stdcall;
  {$EXTERNALSYM CoRevokeMallocSpy}

  function CoCreateStandardMalloc(memctx: DWORD;
                                  out ppmalloc: IMalloc): HResult; stdcall;
  {$EXTERNALSYM CoCreateStandardMalloc}

//#if (_WIN32_WINNT >= 0x0501)

  function CoRegisterInitializeSpy(pSpy: IInitializeSpy;
                                   out puliCookie: ULARGE_INTEGER): HResult; stdcall;
  {$EXTERNALSYM CoRegisterInitializeSpy}

  function CoRevokeInitializeSpy(uliCookie: ULARGE_INTEGER): HResult; stdcall;
  {$EXTERNALSYM CoRevokeInitializeSpy}


type
  // COM System Security Descriptors (used when the corresponding registry
  // entries are absent)

  PCOMSD = ^tagCOMSD;
  tagCOMSD                = (
    SD_LAUNCHPERMISSIONS  = 0,       // Machine wide launch permissions
    SD_ACCESSPERMISSIONS  = 1,       // Machine wide acesss permissions
    SD_LAUNCHRESTRICTIONS = 2,       // Machine wide launch limits
    SD_ACCESSRESTRICTIONS = 3        // Machine wide access limits
  );
  {$EXTERNALSYM tagCOMSD}
  COMSD = tagCOMSD;
  {$EXTERNALSYM COMSD}

  function CoGetSystemSecurityPermissions(comSDType: COMSD;
                                          ppSD: PSECURITY_DESCRIPTOR): HResult; stdcall;
  {$EXTERNALSYM CoGetSystemSecurityPermissions}


  //* dll loading helpers; keeps track of ref counts and unloads all on exit *//
  function CoLoadLibrary(pszLibName: PWideChar;
                         bAutoFree: BOOL): THandle; stdcall;
  {$EXTERNALSYM CoLoadLibrary}

  procedure CoFreeLibrary(hInst: THandle); stdcall;
  {$EXTERNALSYM CoFreeLibrary}

  procedure CoFreeAllLibraries(); stdcall;
  {$EXTERNALSYM CoFreeAllLibraries}


  function CoGetInstanceFromFile(pServerInfo: PCoServerInfo;
                                 const pclsid: PCLSID; // = ^GUID
                                 punkOuter: IUnknown;
                                 dwClsCtx: DWORD;
                                 grfMode: DWORD;
                                 pwszName: PWideChar;
                                 dwCount: DWORD;
                                 pResults: PMULTI_QI): HResult; stdcall;
  {$EXTERNALSYM CoGetInstanceFromFile}

  function CoGetInstanceFromIStorage(pServerInfo: PCoServerInfo;
                                     const pclsid: PCLSID;
                                     punkOuter: IUnknown;
                                     dwClsCtx: DWORD;
                                     pstg: IUnknown; {IStorage}
                                     dwCount: DWORD;
                                     rgmqResults: PMULTI_QI): HResult; stdcall;
  {$EXTERNALSYM CoGetInstanceFromIStorage}


  //* Call related APIs *//

  function CoAllowSetForegroundWindow(pUnk: IUnknown;
                                      lpvReserved: Pointer): HResult; stdcall;
  {$EXTERNALSYM CoAllowSetForegroundWindow}


  function DcomChannelSetHResult(pvReserved: Pointer;
                                 pulReserved: ULONG;
                                 appsHR: HResult): HResult; stdcall;
  {$EXTERNALSYM DcomChannelSetHResult}



  //* other helpers *//

  function CoIsOle1Class(const clsid: TGUID): BOOL; stdcall;
  {$EXTERNALSYM CoIsOle1Class}

  function CLSIDFromProgIDEx(lpszProgID: PWideChar;
                              out lpclsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM CLSIDFromProgIDEx}

  function CoFileTimeToDosDateTime(lpfiletime: PFILETIME;
                                   out lpDosDate: Word;
                                   out lpDosTime: Word): BOOL; overload; stdcall;
  {$EXTERNALSYM CoFileTimeToDosDateTime}

  function CoFileTimeToDosDateTime(const lpfiletime: FILETIME;
                                   out lpDosDate: Word;
                                   out lpDosTime: Word): BOOL; inline; overload;
  {$EXTERNALSYM CoFileTimeToDosDateTime}

  function CoDosDateTimeToFileTime(nDosDate: Word;
                                   nDosTime: Word;
                                   out lpfiletime: FILETIME): BOOL; stdcall;
  {$EXTERNALSYM CoDosDateTimeToFileTime}

  function CoFileTimeNow(out filetime: FILETIME): HResult; stdcall;
  {$EXTERNALSYM CoFileTimeNow}

  function CoRegisterMessageFilter(lpMessageFilter: IMessageFilter;
                                   out lplpMessageFilter: IMessageFilter): HResult; stdcall;
  {$EXTERNALSYM CoRegisterMessageFilter}

  function CoRegisterChannelHook(const ExtensionUuid: TGUID;
                                 pChannelHook: IChannelHook): HResult; stdcall;
  {$EXTERNALSYM CoRegisterChannelHook}


  //* TreatAs APIS *//

  function CoTreatAsClass(const clsidOld: TGUID;
                          out clsidNew: TGUID): HResult; stdcall;
  {$EXTERNALSYM CoTreatAsClass}


  //****** DV APIs ***********************************************************//

  function CreateDataAdviseHolder(out ppDAHolder: IDataAdviseHolder): HResult; stdcall;
  {$EXTERNALSYM CreateDataAdviseHolder}

  function CreateDataCache(unkOuter: IUnknown;
                           const rclsid: TGUID;
                           const iid: TGUID;
                           var ppv: pointer): HResult; stdcall;
  {$EXTERNALSYM CreateDataCache}


  //****** Storage API Prototypes ********************************************//

  function StgOpenAsyncDocfileOnIFillLockBytes(pflb: IFillLockBytes;
                                               grfMode: DWORD;
                                               asyncFlags: DWORD;
                                               var ppstgOpen: IStorage): HResult; stdcall;
  {$EXTERNALSYM StgOpenAsyncDocfileOnIFillLockBytes}

  function StgGetIFillLockBytesOnILockBytes(pilb: ILockBytes;
                                            var ppflb: IFillLockBytes): HResult; stdcall;
  {$EXTERNALSYM StgGetIFillLockBytesOnILockBytes}

  function StgGetIFillLockBytesOnFile(pwcsName: PWideChar;
                                      var ppflb: IFillLockBytes): HResult; stdcall;
  {$EXTERNALSYM StgGetIFillLockBytesOnFile}

  function StgOpenLayoutDocfile(pwcsDfName: PWideChar;
                                grfMode: DWORD;
                                reserved: DWORD;
                                var ppstgOpen: IStorage): HResult; stdcall;
  {$EXTERNALSYM StgOpenLayoutDocfile}



  function CoInstall(pbc: IBindCtx;
                     dwFlags: DWORD;
                     pClassSpec: PuCLSSPEC;
                     pQuery: QUERYCONTEXT;
                     pszCodeBase: LPWSTR): HResult; stdcall;
  {$EXTERNALSYM CoInstall}

  //
  //  Moniker APIs
  //

  function BindMoniker(pmk: IMoniker;
                       grfOpt: DWORD;
                       const iidResult: TGUID;
                       out vResult): HResult; stdcall;
  {$EXTERNALSYM BindMoniker}

  function CoGetObject(lpszName: PWideChar;
                       pBindOptions: BIND_OPTS;
                       const iid: TGUID;
                       var ppv: pointer): HResult; stdcall;
  {$EXTERNALSYM CoGetObject}

  function MkParseDisplayName(pbc: IBindCtx;
                              szUserName: PWideChar;
                              out pchEaten: ULONG;
                              out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM MkParseDisplayName}

  function MonikerRelativePathTo(pmkSrc: IMoniker;
                                 pmkDest: IMoniker;
                                 out ppmkRelPath: PIMoniker;
                                 dwReserved: BOOL): HResult; stdcall;
  {$EXTERNALSYM MonikerRelativePathTo}

  function MonikerCommonPrefixWith(pmkThis: IMoniker;
                                   pmkOther: IMoniker;
                                   out ppmkCommon: IMoniker): HResult; stdcall;
  {$EXTERNALSYM MonikerCommonPrefixWith}

  function CreateBindCtx(reserved: DWORD;
                         out ppbc: PIBindCtx): HResult; stdcall;
  {$EXTERNALSYM CreateBindCtx}

  function CreateGenericComposite(pmkFirst: IMoniker;
                                  pmkRest: IMoniker;
                                  out ppmkComposite: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateGenericComposite}

  function GetClassFile(szFilename: PWideChar;
                        out clsid: TGUID): HResult; stdcall;
  {$EXTERNALSYM GetClassFile}

  function CreateFileMoniker(lpszPathName: PWideChar;
                             out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateFileMoniker}

  function CreateClassMoniker(const rclsid: TGUID;
                             out ppmk: PIMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateClassMoniker}

  function CreateItemMoniker(lpszDelim: PWideChar;
                             lpszItem: PWideChar;
                             out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateItemMoniker}

  function CreateAntiMoniker(out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateAntiMoniker}

  function CreatePointerMoniker(punk: IUnknown;
                                out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreatePointerMoniker}


  function CreateObjrefMoniker(punk: PUNKNOWN;
                               out ppmk: IMoniker): HResult; stdcall;
  {$EXTERNALSYM CreateObjrefMoniker}

  function GetRunningObjectTable(reserved: DWORD;
                                 out pprot: IRunningObjectTable): HResult; stdcall;
  {$EXTERNALSYM GetRunningObjectTable}


  //
  // Standard Progress Indicator implementation
  //
  // urlmon.h >> Internet Explorer 3.0 XP !!!

  // function CreateStdProgressIndicator(hwndParent: HWND;
  //                                     pszTitle: POLESTR;
  //                                     pIbscCaller: IBindStatusCallback;
  //                                     out ppIbsc: IBindStatusCallback);



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  Ole32Lib = 'ole32.dll';

{$WARN SYMBOL_PLATFORM OFF}
function CoBuildVersion;                      external Ole32Lib name 'CoBuildVersion' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoInitialize;                        external Ole32Lib name 'CoInitialize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterMallocSpy;                 external Ole32Lib name 'CoRegisterMallocSpy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRevokeMallocSpy;                   external Ole32Lib name 'CoRevokeMallocSpy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoCreateStandardMalloc;              external Ole32Lib name 'CoCreateStandardMalloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterInitializeSpy;             external Ole32Lib name 'CoRegisterInitializeSpy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRevokeInitializeSpy;               external Ole32Lib name 'CoRevokeInitializeSpy' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetSystemSecurityPermissions;      external Ole32Lib name 'CoGetSystemSecurityPermissions' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoLoadLibrary;                       external Ole32Lib name 'CoLoadLibrary' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeLibrary;                      external Ole32Lib name 'CoFreeLibrary' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
procedure CoFreeAllLibraries;                 external Ole32Lib name 'CoFreeAllLibraries' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetInstanceFromFile;               external Ole32Lib name 'CoGetInstanceFromFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetInstanceFromIStorage;           external Ole32Lib name 'CoGetInstanceFromIStorage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoAllowSetForegroundWindow;          external Ole32Lib name 'CoAllowSetForegroundWindow' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function DcomChannelSetHResult;               external Ole32Lib name 'DcomChannelSetHResult' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoIsOle1Class;                       external Ole32Lib name 'CoIsOle1Class' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CLSIDFromProgIDEx;                   external Ole32Lib name 'CLSIDFromProgIDEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function CoFileTimeToDosDateTime(lpfiletime: PFILETIME;
                                 out lpDosDate: Word;
                                 out lpDosTime: Word): BOOL; external Ole32Lib name 'CoFileTimeToDosDateTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function CoDosDateTimeToFileTime;             external Ole32Lib name 'CoDosDateTimeToFileTime' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoFileTimeNow;                       external Ole32Lib name 'CoFileTimeNow' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterMessageFilter;             external Ole32Lib name 'CoRegisterMessageFilter' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoRegisterChannelHook;               external Ole32Lib name 'CoRegisterChannelHook' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoTreatAsClass;                      external Ole32Lib name 'CoTreatAsClass' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateDataAdviseHolder;              external Ole32Lib name 'CreateDataAdviseHolder' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateDataCache;                     external Ole32Lib name 'CreateDataCache' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenAsyncDocfileOnIFillLockBytes; external Ole32Lib name 'StgOpenAsyncDocfileOnIFillLockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgGetIFillLockBytesOnILockBytes;    external Ole32Lib name 'StgGetIFillLockBytesOnILockBytes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgGetIFillLockBytesOnFile;          external Ole32Lib name 'StgGetIFillLockBytesOnFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function StgOpenLayoutDocfile;                external Ole32Lib name 'StgOpenLayoutDocfile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoInstall;                           external Ole32Lib name 'CoInstall' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function BindMoniker;                         external Ole32Lib name 'BindMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CoGetObject;                         external Ole32Lib name 'CoGetObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function MkParseDisplayName;                  external Ole32Lib name 'MkParseDisplayName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function MonikerRelativePathTo;               external Ole32Lib name 'MonikerRelativePathTo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function MonikerCommonPrefixWith;             external Ole32Lib name 'MonikerCommonPrefixWith' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateBindCtx;                       external Ole32Lib name 'CreateBindCtx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateGenericComposite;              external Ole32Lib name 'CreateGenericComposite' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetClassFile;                        external Ole32Lib name 'GetClassFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateFileMoniker;                   external Ole32Lib name 'CreateFileMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateClassMoniker;                  external Ole32Lib name 'CreateClassMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateItemMoniker;                   external Ole32Lib name 'CreateItemMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateAntiMoniker;                   external Ole32Lib name 'CreateAntiMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreatePointerMoniker;                external Ole32Lib name 'CreatePointerMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateObjrefMoniker;                 external Ole32Lib name 'CreateObjrefMoniker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetRunningObjectTable;               external Ole32Lib name 'GetRunningObjectTable' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

function CoFileTimeToDosDateTime(const lpfiletime: FILETIME;
                                 out lpDosDate: Word;
                                 out lpDosTime: Word): BOOL; inline;
begin
  Result := CoFileTimeToDosDateTime(@lpfiletime,
                                    lpDosDate,
                                    lpDosTime);
end;

  // Implement Additional functions here.

end.
