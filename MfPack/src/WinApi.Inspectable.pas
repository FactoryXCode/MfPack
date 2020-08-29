// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Inspectable.pas
// Kind: Pascal / Delphi unit
// Release date: 30-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Provides functionality required for all Windows Runtime classes.
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
// Remarks: Requires Windows 8 or later.
//
//          When to implement
//          The IInspectable interface enables projecting Win32 and COM features into
//          JavaScript and other languages,
//          such as C# and Visual Basic.
//          Implement the IInspectable interface when you want your class to be available in
//          other programming environments.
// 
//          When to use
//          The IInspectable interface is the base interface for all Windows Runtime classes.
//          All Windows Runtime classes must implement the IInspectable sinterface.
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
// Source: inspectable.h
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
unit WinApi.Inspectable;

  {$HPPEMIT '#include "inspectable.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type

  PINSPECTABLE = ^IInspectable;
  LPINSPECTABLE = ^IInspectable;

  PTrustLevel = ^TrustLevel;
  TrustLevel = (
    BaseTrust    = 0,
    PartialTrust = 1,
    FullTrust    = 2
  );
  {$EXTERNALSYM TrustLevel}


  // IInspectable interface
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInspectable);'}
  {$EXTERNALSYM IInspectable}
  IInspectable = interface(IUnknown)
  ['{AF86E2E0-B12D-4c6a-9C5A-D7AA65101E90}']

    function GetIids(out iidCount: ULONG;
                     out iids: PGuid): HRESULT; stdcall;

    function GetRuntimeClassName(out className: string): HRESULT; stdcall;

    function GetTrustLevel(out _trustLevel: TrustLevel): HRESULT; stdcall;

  end;
  IID_IInspectable = IInspectable;
  {$EXTERNALSYM IID_IInspectable}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
