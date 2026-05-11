// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.FunctionDiscoveryApi.FunctionDiscoveryApi.pas
// Kind: Pascal / Delphi unit
// Release date: 05-05-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description:
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
// Source: functiondiscoveryapi.idl
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
unit WinApi.FunctionDiscoveryApi.FunctionDiscoveryApi;

 {$HPPEMIT '#include "functiondiscoveryapi.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ServProv,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl;



{$MINENUMSIZE 4}

const

  CLSID_FunctionDiscovery: TGUID =  '{C72BE2EC-8E90-452c-B29A-AB8FF1C071FC}';

  ///(D1: $C72BE2EC;
  //                                  D2: $8E90;
  //                                  D3: $452C;
  //                                  D4: ($B2, $9A, $AB, $4F, $99, $BA, $CA, $3E));

type

  // Forward declarations
  IFunctionDiscovery = interface;
  IFunctionInstance = interface;
  IFunctionInstanceCollection = interface;
  IFunctionInstanceQuery = interface;
  IFunctionInstanceCollectionQuery = interface;
  IPropertyStoreCollection = interface;
  IFunctionDiscoveryNotification = interface;
  // IFunctionInstanceCollectionQueryCollection = interface;

  PFDQUERYCONTEXT = ^FDQUERYCONTEXT;
  FDQUERYCONTEXT = DWORDLONG;
  {$EXTERNALSYM FDQUERYCONTEXT}

  tagSystemVisibilityFlags = (
    SVF_SYSTEM = 0,
    SVF_USER  = 1
  );
  {$EXTERNALSYM tagSystemVisibilityFlags}
  SystemVisibilityFlags = tagSystemVisibilityFlags;
  {$EXTERNALSYM SystemVisibilityFlags}
  PSystemVisibilityFlags = ^tagSystemVisibilityFlags;
  {$EXTERNALSYM PSystemVisibilityFlags}


  tagQueryUpdateAction = (
    QUA_ADD = 0,
    QUA_REMOVE = 1,
    QUA_CHANGE = 2
  );
  {$EXTERNALSYM tagQueryUpdateAction}
  QueryUpdateAction = tagQueryUpdateAction;
  {$EXTERNALSYM QueryUpdateAction}
  PQueryUpdateAction = ^tagQueryUpdateAction;
  {$EXTERNALSYM PQueryUpdateAction}

  tagQueryCategoryType = (
    QCT_PROVIDER = 0,
    QCT_LAYERED = 1
  );
  {$EXTERNALSYM tagQueryCategoryType}
  QueryCategoryType = tagQueryCategoryType;
  {$EXTERNALSYM QueryCategoryType}
  PQueryCategoryType = ^tagQueryCategoryType;
  {$EXTERNALSYM PQueryCategoryType}


  // PROPERTY Constraint defines
  // ===========================
  //
  // From FunctionDiscoveryConstraints.h, required by
  // IFunctionInstanceCollectionQuery.AddPropertyConstraint.
  //
  tagPropertyConstraint = (
    QC_EQUALS = 0,
    QC_NOTEQUAL = 1,
    QC_LESSTHAN = 2,
    QC_LESSTHANOREQUAL = 3,
    QC_GREATERTHAN = 4,
    QC_GREATERTHANOREQUAL = 5,
    QC_STARTSWITH = 6,   // Strings only
    QC_EXISTS = 7,
    QC_DOESNOTEXIST = 8,
    QC_CONTAINS = 9      // Strings and VT_VECTOR only
  );
  {$EXTERNALSYM tagPropertyConstraint}
  PropertyConstraint = tagPropertyConstraint;
  {$EXTERNALSYM PropertyConstraint}
  PPropertyConstraint = ^tagPropertyConstraint;
  {$EXTERNALSYM PPropertyConstraint}


  // This interface should be implemented by clients performing async queries...
  //
  // Interface IFunctionDiscoveryNotification
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionDiscoveryNotification);'}
  {$EXTERNALSYM IFunctionDiscoveryNotification}
  IFunctionDiscoveryNotification = interface(IUnknown)
    ['{5F6C1BA8-5330-422E-A368-572B244D3F87}']

    function OnUpdate(enumQueryUpdateAction: QueryUpdateAction;
                      fdqcQueryContext: FDQUERYCONTEXT;
                      const pIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function OnError(hr: HRESULT;
                     fdqcQueryContext: FDQUERYCONTEXT;
                     pszProvider: LPCWSTR): HRESULT; stdcall;

    function OnEvent(dwEventID: DWORD;
                     fdqcQueryContext: FDQUERYCONTEXT;
                     pszProvider: LPCWSTR): HRESULT; stdcall;

  end;
  IID_IFunctionDiscoveryNotification = IFunctionDiscoveryNotification;
  {$EXTERNALSYM IID_IFunctionDiscoveryNotification}


  // Interface IFunctionDiscovery
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionDiscovery);'}
  {$EXTERNALSYM IFunctionDiscovery}
  IFunctionDiscovery = interface(IUnknown)
    ['{4DF99B70-E148-4432-B004-4C9EEB535A5E}']

    // Discovery
    function GetInstanceCollection(pszCategory: LPCWSTR;
                                   pszSubCategory: LPCWSTR;
                                   fIncludeAllSubCategories: BOOL;
                                   out ppIFunctionInstanceCollection: IFunctionInstanceCollection): HRESULT; stdcall;

    function GetInstance(pszFunctionInstanceIdentity: LPCWSTR;
                         out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function CreateInstanceCollectionQuery(pszCategory: LPCWSTR;
                                           pszSubCategory: LPCWSTR;
                                           fIncludeAllSubCategories: BOOL;
                                           const pIFunctionDiscoveryNotification: IFunctionDiscoveryNotification;
                                           pfdqcQueryContext: PFDQUERYCONTEXT;
                                           out ppIFunctionInstanceCollectionQuery: IFunctionInstanceCollectionQuery): HRESULT; stdcall;

    function CreateInstanceQuery(pszFunctionInstanceIdentity: LPCWSTR;
                                 const pIFunctionDiscoveryNotification: IFunctionDiscoveryNotification;
                                 pfdqcQueryContext: PFDQUERYCONTEXT;
                                 out ppIFunctionInstanceQuery: IFunctionInstanceQuery): HRESULT; stdcall;

    // Provider Specific
    function AddInstance(enumSystemVisibility: SystemVisibilityFlags;
                         pszCategory: LPCWSTR;
                         pszSubCategory: LPCWSTR;
                         pszCategoryIdentity: LPCWSTR;
                         out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function RemoveInstance(enumSystemVisibility: SystemVisibilityFlags;
                            pszCategory: LPCWSTR;
                            pszSubCategory: LPCWSTR;
                            pszCategoryIdentity: LPCWSTR): HRESULT; stdcall;

  end;
  IID_IFunctionDiscovery = IFunctionDiscovery;
  {$EXTERNALSYM IID_IFunctionDiscovery}


  // Interface IFunctionInstance
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionInstance);'}
  {$EXTERNALSYM IFunctionInstance}
  IFunctionInstance = interface(IServiceProvider)
    ['{33591C10-0BED-4F02-B0AB-1530D5533EE9}']

    // Meta Data
    function GetID(out ppszCoMemIdentity: LPWSTR): HRESULT; stdcall;

    function GetProviderInstanceID(out ppszCoMemProviderInstanceIdentity: LPWSTR): HRESULT; stdcall;

    function OpenPropertyStore(dwStgAccess: DWORD;
                               out ppIPropertyStore: IPropertyStore): HRESULT; stdcall;

    // Meta Data
    function GetCategory(out ppszCoMemCategory: LPWSTR;
                         out ppszCoMemSubCategory: LPWSTR): HRESULT; stdcall;

  end;
  IID_IFunctionInstance = IFunctionInstance;
  {$EXTERNALSYM IID_IFunctionInstance}


  // Interface IFunctionInstanceCollection
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionInstanceCollection);'}
  {$EXTERNALSYM IFunctionInstanceCollection}
  IFunctionInstanceCollection = interface(IUnknown)
    ['{F0A3D895-855C-42A2-948D-2F97D450ECB1}']

    function GetCount(out pdwCount: DWORD): HRESULT; stdcall;

    function Get(pszInstanceIdentity: LPCWSTR;
                 out pdwIndex: DWORD;
                 out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function Item(dwIndex: DWORD;
                  out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function Add(const pIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function Remove(dwIndex: DWORD;
                    out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

    function Delete(dwIndex: DWORD): HRESULT; stdcall;

    function DeleteAll(): HRESULT; stdcall;

  end;
  IID_IFunctionInstanceCollection = IFunctionInstanceCollection;
  {$EXTERNALSYM IID_IFunctionInstanceCollection}


  // Interface IPropertyStoreCollection
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyStoreCollection);'}
  {$EXTERNALSYM IPropertyStoreCollection}
  IPropertyStoreCollection = interface(IUnknown)
    ['{D14D9C30-12D2-42D8-BCE4-C60C2BB226FA}']

    function GetCount(out pdwCount: DWORD): HRESULT; stdcall;

    function Get(pszInstanceIdentity: LPCWSTR;
                 out pdwIndex: DWORD;
                 out ppIPropertyStore: IPropertyStore): HRESULT; stdcall;

    function Item(dwIndex: DWORD;
                  out ppIPropertyStore: IPropertyStore): HRESULT; stdcall;

    function Add(const pIPropertyStore: IPropertyStore): HRESULT; stdcall;

    function Remove(dwIndex: DWORD;
                    out pIPropertyStore: IPropertyStore): HRESULT; stdcall;

    function Delete(dwIndex: DWORD): HRESULT; stdcall;

    function DeleteAll(): HRESULT; stdcall;

  end;
  IID_IPropertyStoreCollection = IPropertyStoreCollection;
  {$EXTERNALSYM IID_IPropertyStoreCollection}


  //////////////////////////////////
  // Query interfaces
  //////////////////////////////////

  // Interface IFunctionInstanceQuery
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionInstanceQuery);'}
  {$EXTERNALSYM IFunctionInstanceQuery}
  IFunctionInstanceQuery = interface(IUnknown)
    ['{6242BC6B-90EC-4B37-BB46-E229FD84ED95}']

    function Execute(out ppIFunctionInstance: IFunctionInstance): HRESULT; stdcall;

  end;
  IID_IFunctionInstanceQuery = IFunctionInstanceQuery;
  {$EXTERNALSYM IID_IFunctionInstanceQuery}


  // Interface IFunctionInstanceCollectionQuery
  // ==========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFunctionInstanceCollectionQuery);'}
  {$EXTERNALSYM IFunctionInstanceCollectionQuery}
  IFunctionInstanceCollectionQuery = interface(IUnknown)
    ['{57CC6FD2-C09A-4289-BB72-25F04142058E}']

    function AddQueryConstraint(pszConstraintName: LPCWSTR;
                                pszConstraintValue: LPCWSTR): HRESULT; stdcall;

    function AddPropertyConstraint(const Key: PROPERTYKEY;
                                   pv: PPropVariant;
                                   enumPropertyConstraint: PropertyConstraint): HRESULT; stdcall;

    function Execute(out ppIFunctionInstanceCollection: IFunctionInstanceCollection): HRESULT; stdcall;

  end;
  IID_IFunctionInstanceCollectionQuery = IFunctionInstanceCollectionQuery;
  {$EXTERNALSYM IID_IFunctionInstanceCollectionQuery}

  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.
