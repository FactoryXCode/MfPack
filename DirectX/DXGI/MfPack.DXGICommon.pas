// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MFPack - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DXGICommon.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Microsoft DirectX Graphics Infrastructure API
//
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
// Remarks: -
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
// Source: dxgicommon.h
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
unit MfPack.DXGICommon;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "dxgicommon.h"'}
  {$HPPEMIT ''}

interface

uses
  MfPack.MfpTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

const
  // The following values are used with DXGI_SAMPLE_DESC.Quality:
  DXGI_STANDARD_MULTISAMPLE_QUALITY_PATTERN = $FFFFFFFF;
  {$EXTERNALSYM DXGI_STANDARD_MULTISAMPLE_QUALITY_PATTERN}
  DXGI_CENTER_MULTISAMPLE_QUALITY_PATTERN   = $FFFFFFFE;
  {$EXTERNALSYM DXGI_CENTER_MULTISAMPLE_QUALITY_PATTERN}

type

  PDXGI_RATIONAL = ^DXGI_RATIONAL;
  DXGI_RATIONAL = record
    Numerator: UINT;
    Denominator: UINT;
  end;
  {$EXTERNALSYM DXGI_RATIONAL}

  PDXGI_SAMPLE_DESC = ^DXGI_SAMPLE_DESC;
  DXGI_SAMPLE_DESC = record
    Count: UINT;
    Quality: UINT;
  end;
  {$EXTERNALSYM DXGI_SAMPLE_DESC}

  PDXGI_COLOR_SPACE_TYPE = ^DXGI_COLOR_SPACE_TYPE;
  DXGI_COLOR_SPACE_TYPE                               = (
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709           = 0,
    DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709           = 1,
    DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P709         = 2,
    DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P2020        = 3,
    DXGI_COLOR_SPACE_RESERVED                         = 4,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601    = 5,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601       = 6,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P601         = 7,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709       = 8,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P709         = 9,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020      = 10,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020        = 11,
    DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020        = 12,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_LEFT_P2020    = 13,
    DXGI_COLOR_SPACE_RGB_STUDIO_G2084_NONE_P2020      = 14,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_TOPLEFT_P2020   = 15,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_TOPLEFT_P2020 = 16,
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P2020          = 17,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_GHLG_TOPLEFT_P2020  = 18,
    DXGI_COLOR_SPACE_YCBCR_FULL_GHLG_TOPLEFT_P2020    = 19,
    DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P709         = 20,
    DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P2020        = 21,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P709       = 22,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P2020      = 23,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_TOPLEFT_P2020   = 24,
    DXGI_COLOR_SPACE_CUSTOM                           = FORCEDWORD);
  {$EXTERNALSYM DXGI_COLOR_SPACE_TYPE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
