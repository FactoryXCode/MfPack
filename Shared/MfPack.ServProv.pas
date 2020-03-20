// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.ServProv.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
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
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
//
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
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
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
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
unit MfPack.ServProv;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "servprov.h"'}
  {$HPPEMIT ''}

interface

uses
  MfPack.MfpTypes;



type

  // Interface IServiceProvider
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IServiceProvider);'}
  {$EXTERNALSYM IServiceProvider}
  IServiceProvider = interface(IUnknown)

  ['{6d5140c1-7436-11ce-8034-00aa006009fa}']
    function  QueryService(const guidService: REFGUID;
                           const riid: REFIID;
                           out ppvObject: Pointer): HResult; stdcall;
  end;
  IID_IServiceProvider = IServiceProvider;
  {$EXTERNALSYM IID_IServiceProvider}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
