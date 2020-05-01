// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - D2D1
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.D2d1Effects_1.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Image effects parts of the Direct2D API for Windows 8 and later.
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
// Remarks: -
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
// Source: d2d1effects_1.h
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
unit MfPack.D2D1Effects_1;

  {$HPPEMIT '#include "d2d1effects.h"'}

interface

uses

  WinApi.Windows,
  {MfPack}
  MfPack.D2D1Effects,
  MfPack.MfpTypes;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

const
  // Built in effect CLSIDs
  {$EXTERNALSYM CLSID_D2D1YCbCr}
  CLSID_D2D1YCbCr : TGUID = (D1: $99503cc1;
                             D2: $66c7;
                             D3: $45c9;
                             D4: ($a8, $75, $8a, $d8, $a7, $91, $44, $01));

type
  /// <summary>
  /// The enumeration of the YCbCr effect's top level properties.
  /// Effect description: An effect that takes a Y plane as input 0 and a CbCr plane
  /// as input 1 and outputs RGBA.  The CbCr plane can be chroma subsampled.  Useful
  /// for JPEG color conversion.
  /// </summary>
  PD2D1_YCBCR_PROP = ^D2D1_YCBCR_PROP;
  D2D1_YCBCR_PROP                      = (
    /// <summary>
    /// Property Name: "ChromaSubsampling"
    /// Property Type: D2D1_YCBCR_CHROMA_SUBSAMPLING
    /// </summary>
    D2D1_YCBCR_PROP_CHROMA_SUBSAMPLING = 0,
    /// <summary>
    /// Property Name: "TransformMatrix"
    /// Property Type: D2D1_MATRIX_3X2_F
    /// </summary>
    D2D1_YCBCR_PROP_TRANSFORM_MATRIX   = 1,
    /// <summary>
    /// Property Name: "InterpolationMode"
    /// Property Type: D2D1_YCBCR_INTERPOLATION_MODE
    /// </summary>
    D2D1_YCBCR_PROP_INTERPOLATION_MODE = 2,
    D2D1_YCBCR_PROP_FORCE_DWORD        = FORCEDWORD);
  {$EXTERNALSYM D2D1_YCBCR_PROP}


  PD2D1_YCBCR_CHROMA_SUBSAMPLING = ^D2D1_YCBCR_CHROMA_SUBSAMPLING;
  D2D1_YCBCR_CHROMA_SUBSAMPLING               = (
    D2D1_YCBCR_CHROMA_SUBSAMPLING_AUTO        = 0,
    D2D1_YCBCR_CHROMA_SUBSAMPLING_420         = 1,
    D2D1_YCBCR_CHROMA_SUBSAMPLING_422         = 2,
    D2D1_YCBCR_CHROMA_SUBSAMPLING_444         = 3,
    D2D1_YCBCR_CHROMA_SUBSAMPLING_440         = 4,
    D2D1_YCBCR_CHROMA_SUBSAMPLING_FORCE_DWORD = FORCEDWORD);
  {$EXTERNALSYM D2D1_YCBCR_CHROMA_SUBSAMPLING}


  PD2D1_YCBCR_INTERPOLATION_MODE = ^D2D1_YCBCR_INTERPOLATION_MODE;
  D2D1_YCBCR_INTERPOLATION_MODE                       = (
    D2D1_YCBCR_INTERPOLATION_MODE_NEAREST_NEIGHBOR    = 0,
    D2D1_YCBCR_INTERPOLATION_MODE_LINEAR              = 1,
    D2D1_YCBCR_INTERPOLATION_MODE_CUBIC               = 2,
    D2D1_YCBCR_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR = 3,
    D2D1_YCBCR_INTERPOLATION_MODE_ANISOTROPIC         = 4,
    D2D1_YCBCR_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC  = 5,
    D2D1_YCBCR_INTERPOLATION_MODE_FORCE_DWORD         = FORCEDWORD);
  {$EXTERNALSYM D2D1_YCBCR_INTERPOLATION_MODE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
