// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ServProv.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: IServiceProvider description.
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
// Remarks: - Requires Windows Vista or later.
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
// Source: servprov.h
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
//
//==============================================================================
unit WinApi.ServProv;

  {$HPPEMIT '#include "servprov.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

type

  // Interface IServiceProvider
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IServiceProvider);'}
  {$EXTERNALSYM IServiceProvider}
  IServiceProvider = interface(IUnknown)

  ['{6d5140c1-7436-11ce-8034-00aa006009fa}']
    function  QueryService(const guidService: TGUID;
                           const riid: TGUID;
                           var ppvObject: Pointer): HResult; stdcall;
  end;
  IID_IServiceProvider = IServiceProvider;
  {$EXTERNALSYM IID_IServiceProvider}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
