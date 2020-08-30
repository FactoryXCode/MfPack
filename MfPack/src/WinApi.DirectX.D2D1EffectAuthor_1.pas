// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1EffectAuthor_1.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Direct2D: Hardware-accelerated, immediate-mode, 2-D graphics API that
//              provides high performance and high-quality rendering for 2-D geometry,
//              bitmaps, and text.
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
// Source: d2d1effectauthor_1.h
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
unit WinApi.DirectX.D2D1EffectAuthor_1;

  {$HPPEMIT '#include "d2d1effectauthor_1.h"'}

interface

uses

  {WinApi.DirectX}
  WinApi.DirectX.D2D1EffectAuthor,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1_3,
  WinApi.DirectX.DXGICommon;

  {$WEAKPACKAGEUNIT ON}

type

  // Interfaces

  // Interface ID2D1EffectContext1
  // =============================
  // The internal context handed to effect authors to create transforms from effects
  // and any other operation tied to context which is not useful to the application
  // facing API.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1EffectContext1);'}
  {$EXTERNALSYM ID2D1EffectContext1}
  ID2D1EffectContext1 = interface(ID2D1EffectContext)
  ['{84ab595a-fc81-4546-bacd-e8ef4d8abe7a}']
    // Creates a 3D lookup table for mapping a 3-channel input to a 3-channel output.
    // The table data must be provided in 4-channel format.

    function CreateLookupTable3D(precision: D2D1_BUFFER_PRECISION;
                                 extents: UINT32;
                                 data: PByte;
                                 dataCount: UINT32;
                                 strides: UINT32;
                                 out lookupTable: ID2D1LookupTable3D): HResult; stdcall;

  end;
  IID_ID2D1EffectContext1 = ID2D1EffectContext1;
  {$EXTERNALSYM IID_ID2D1EffectContext1}


//#if NTDDI_VERSION >= NTDDI_WIN10_RS2


  // Interface ID2D1EffectContext2
  // =============================
  // The internal context handed to effect authors to create transforms from effects
  // and any other operation tied to context which is not useful to the application
  // facing API.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1EffectContext2);'}
  {$EXTERNALSYM ID2D1EffectContext2}
  ID2D1EffectContext2 = interface(ID2D1EffectContext1)
  ['{577ad2a0-9fc7-4dda-8b18-dab810140052}']
    // Creates a color context from a DXGI color space type. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromDxgiColorSpace(colorSpace: DXGI_COLOR_SPACE_TYPE;
                                                  out colorContext: ID2D1ColorContext1): HResult; stdcall;

    // Creates a color context from a simple color profile. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromSimpleColorProfile(simpleProfile: D2D1_SIMPLE_COLOR_PROFILE;
                                                      out colorContext: ID2D1ColorContext1): HResult; stdcall;

  end;
  IID_ID2D1EffectContext2 = ID2D1EffectContext2;
  {$EXTERNALSYM IID_ID2D1EffectContext2}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
