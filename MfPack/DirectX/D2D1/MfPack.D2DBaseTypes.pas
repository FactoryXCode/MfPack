// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - D2D1
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.D2DBaseTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Defines drawing primitives for Direct2D, such as points and rectangles.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks:  Delphi : The IUnknown entries of functions should be casted like this:
//           IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
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
// Source: d2dbasetypes.h
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
unit MfPack.D2DBaseTypes;

  {$HPPEMIT '#include "d2dbasetypes.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {MfPack}
  MfPack.DXGIType;
  {or use Dx.DxTypes; // contains DxGiType;}

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}

type

  PD2dColorF = ^D2D_COLOR_F;
  D2D_COLOR_F = D3DCOLORVALUE;
  {$EXTERNALSYM D2D_COLOR_F}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
