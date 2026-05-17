// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ObjectArray.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Shell API.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
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
// Source: ObjectArray.h
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
unit WinApi.ObjectArray;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi;

type

  IObjectArray = interface;
  IObjectCollection = interface;



  // Interface IObjectArray
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectArray);'}
  {$EXTERNALSYM IObjectArray}
  IObjectArray = interface(IUnknown)
    ['{92CA9DCD-5622-4BBA-A805-5E9F541BD8C9}']

    function GetCount(out pcObjects: UINT): HRESULT; stdcall;

    function GetAt(uiIndex: UINT;
                   const riid: TGUID;
                   out ppv): HRESULT; stdcall;
  end;
  IID_IObjectArray = IObjectArray;
  {$EXTERNALSYM IID_IObjectArray}



  // Interface IObjectCollection
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectCollection);'}
  {$EXTERNALSYM IObjectCollection}
  IObjectCollection = interface(IObjectArray)
    ['{5632B1A4-E38A-400A-928A-D4CD63230295}']

    function AddObject(punk: IUnknown): HRESULT; stdcall;

    function AddFromArray(poaSource: IObjectArray): HRESULT; stdcall;

    function RemoveObjectAt(uiIndex: UINT): HRESULT; stdcall;

    function Clear(): HRESULT; stdcall;
  end;
  IID_IObjectCollection = IObjectCollection;
  {$EXTERNALSYM IID_IObjectCollection}

implementation

end.
