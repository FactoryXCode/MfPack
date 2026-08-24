// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ComCat.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Requires Windows Vista or later.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
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
// Source: comcat.h
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
unit WinApi.ComCat;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils;

const
  CATDESC_MAX = 128;
  {$EXTERNALSYM CATDESC_MAX}

type
  CATID = TGUID;
  {$EXTERNALSYM CATID}
  PCATID = ^CATID;
  {$EXTERNALSYM PCATID}

  REFCATID = PGUID;
  {$EXTERNALSYM REFCATID}

const
  CATID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
  {$EXTERNALSYM CATID_NULL}

type
  IEnumGUID = interface;
  IEnumCATEGORYINFO = interface;
  ICatRegister = interface;
  ICatInformation = interface;

  LPENUMGUID = IEnumGUID;
  {$EXTERNALSYM LPENUMGUID}
  IEnumCLSID = IEnumGUID;
  {$EXTERNALSYM IEnumCLSID}
  LPENUMCLSID = IEnumGUID;
  {$EXTERNALSYM LPENUMCLSID}
  IEnumCATID = IEnumGUID;
  {$EXTERNALSYM IEnumCATID}

  LPENUMCATEGORYINFO = IEnumCATEGORYINFO;
  {$EXTERNALSYM LPENUMCATEGORYINFO}
  LPCATREGISTER = ICatRegister;
  {$EXTERNALSYM LPCATREGISTER}
  LPCATINFORMATION = ICatInformation;
  {$EXTERNALSYM LPCATINFORMATION}

  PCATEGORYINFO = ^CATEGORYINFO;
  LPCATEGORYINFO = PCATEGORYINFO;
  {$EXTERNALSYM LPCATEGORYINFO}
  tagCATEGORYINFO = record
    catid: CATID;
    lcid: LCID;
    szDescription: array[0..CATDESC_MAX - 1] of OLECHAR;
  end;
  {$EXTERNALSYM tagCATEGORYINFO}
  CATEGORYINFO = tagCATEGORYINFO;
  {$EXTERNALSYM CATEGORYINFO}



  // Interface IEnumGUID
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumGUID);'}
  {$EXTERNALSYM IEnumGUID}
  IEnumGUID = interface(IUnknown)
    ['{0002E000-0000-0000-C000-000000000046}']

    function Next(celt: ULONG;
                  rgelt: PGUID;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumGUID): HRESULT; stdcall;
  end;
  IID_IEnumGUID = IEnumGUID;
  {$EXTERNALSYM IID_IEnumGUID}



  // Interface IEnumCATEGORYINFO
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumCATEGORYINFO);'}
  {$EXTERNALSYM IEnumCATEGORYINFO}
  IEnumCATEGORYINFO = interface(IUnknown)
    ['{0002E011-0000-0000-C000-000000000046}']

    function Next(celt: ULONG;
                  rgelt: PCATEGORYINFO;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumCATEGORYINFO): HRESULT; stdcall;
  end;
  IID_IEnumCATEGORYINFO = IEnumCATEGORYINFO;
  {$EXTERNALSYM IID_IEnumCATEGORYINFO}



  // Interface ICatRegister
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICatRegister);'}
  {$EXTERNALSYM ICatRegister}
  ICatRegister = interface(IUnknown)
    ['{0002E012-0000-0000-C000-000000000046}']

    function RegisterCategories(cCategories: ULONG;
                                rgCategoryInfo: PCATEGORYINFO): HRESULT; stdcall;

    function UnRegisterCategories(cCategories: ULONG;
                                  rgcatid: PCATID): HRESULT; stdcall;

    function RegisterClassImplCategories(const rclsid: TGUID;
                                         cCategories: ULONG;
                                         rgcatid: PCATID): HRESULT; stdcall;

    function UnRegisterClassImplCategories(const rclsid: TGUID;
                                           cCategories: ULONG;
                                           rgcatid: PCATID): HRESULT; stdcall;

    function RegisterClassReqCategories(const rclsid: TGUID;
                                        cCategories: ULONG;
                                        rgcatid: PCATID): HRESULT; stdcall;

    function UnRegisterClassReqCategories(const rclsid: TGUID;
                                          cCategories: ULONG;
                                          rgcatid: PCATID): HRESULT; stdcall;
  end;
  IID_ICatRegister = ICatRegister;
  {$EXTERNALSYM IID_ICatRegister}



  // Interface ICatInformation
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICatInformation);'}
  {$EXTERNALSYM ICatInformation}
  ICatInformation = interface(IUnknown)
    ['{0002E013-0000-0000-C000-000000000046}']

    function EnumCategories(lcid: LCID;
                            out ppenumCategoryInfo: IEnumCATEGORYINFO): HRESULT; stdcall;

    function GetCategoryDesc(const rcatid: TGUID;
                             lcid: LCID;
                             out pszDesc: LPWSTR): HRESULT; stdcall;

    function EnumClassesOfCategories(cImplemented: ULONG;
                                     rgcatidImpl: PCATID;
                                     cRequired: ULONG;
                                     rgcatidReq: PCATID;
                                     out ppenumClsid: IEnumGUID): HRESULT; stdcall;

    function IsClassOfCategories(const rclsid: TGUID;
                                 cImplemented: ULONG;
                                 rgcatidImpl: PCATID;
                                 cRequired: ULONG;
                                 rgcatidReq: PCATID): HRESULT; stdcall;

    function EnumImplCategoriesOfClass(const rclsid: TGUID;
                                       out ppenumCatid: IEnumGUID): HRESULT; stdcall;

    function EnumReqCategoriesOfClass(const rclsid: TGUID;
                                      out ppenumCatid: IEnumGUID): HRESULT; stdcall;
  end;
  IID_ICatInformation = ICatInformation;
  {$EXTERNALSYM IID_ICatInformation}

function IsEqualCATID(const rcatid1: CATID;
                      const rcatid2: CATID): BOOL; inline;

const
  CLSID_StdComponentCategoriesMgr: TGUID = '{0002E005-0000-0000-C000-000000000046}';
  {$EXTERNALSYM CLSID_StdComponentCategoriesMgr}

  CATID_Insertable: TGUID = '{40FC6ED3-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_Insertable}
  CATID_Control: TGUID = '{40FC6ED4-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_Control}
  CATID_Programmable: TGUID = '{40FC6ED5-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_Programmable}
  CATID_IsShortcut: TGUID = '{40FC6ED6-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_IsShortcut}
  CATID_NeverShowExt: TGUID = '{40FC6ED7-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_NeverShowExt}
  CATID_DocObject: TGUID = '{40FC6ED8-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_DocObject}
  CATID_Printable: TGUID = '{40FC6ED9-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_Printable}
  CATID_RequiresDataPathHost: TGUID = '{0DE86A50-2BAA-11CF-A229-00AA003D7352}';
  {$EXTERNALSYM CATID_RequiresDataPathHost}
  CATID_PersistsToMoniker: TGUID = '{40FC6EDA-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToMoniker}
  CATID_PersistsToStorage: TGUID = '{40FC6EDB-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToStorage}
  CATID_PersistsToStreamInit: TGUID = '{40FC6EDC-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToStreamInit}
  CATID_PersistsToStream: TGUID = '{40FC6EDD-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToStream}
  CATID_PersistsToMemory: TGUID = '{40FC6EDE-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToMemory}
  CATID_PersistsToFile: TGUID = '{40FC6EDF-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToFile}
  CATID_PersistsToPropertyBag: TGUID = '{40FC6EE0-2438-11CF-A3DB-080036F12502}';
  {$EXTERNALSYM CATID_PersistsToPropertyBag}
  CATID_InternetAware: TGUID = '{0DE86A51-2BAA-11CF-A229-00AA003D7352}';
  {$EXTERNALSYM CATID_InternetAware}
  CATID_DesignTimeUIActivatableControl: TGUID = '{0DE86A52-2BAA-11CF-A229-00AA003D7352}';
  {$EXTERNALSYM CATID_DesignTimeUIActivatableControl}

implementation

function IsEqualCATID(const rcatid1: CATID;
                      const rcatid2: CATID): BOOL;
begin

  Result := IsEqualGUID(rcatid1,
                        rcatid2);
end;

end.
