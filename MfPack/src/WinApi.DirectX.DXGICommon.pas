// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGICommon.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: -
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
unit WinApi.DirectX.DXGICommon;

  {$HPPEMIT '#include "dxgicommon.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}

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

type
  PDXGI_COLOR_SPACE_TYPE = ^DXGI_COLOR_SPACE_TYPE;
  DXGI_COLOR_SPACE_TYPE = DWord;
  {$EXTERNALSYM DXGI_COLOR_SPACE_TYPE}
const
  DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709           = DXGI_COLOR_SPACE_TYPE(0);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709}
  DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709           = DXGI_COLOR_SPACE_TYPE(1);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709}
  DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P709         = DXGI_COLOR_SPACE_TYPE(2);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P709}
  DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P2020        = DXGI_COLOR_SPACE_TYPE(3);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P2020}
  DXGI_COLOR_SPACE_RESERVED                         = DXGI_COLOR_SPACE_TYPE(4);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RESERVED}
  DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601    = DXGI_COLOR_SPACE_TYPE(5);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601       = DXGI_COLOR_SPACE_TYPE(6);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601}
  DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P601         = DXGI_COLOR_SPACE_TYPE(7);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P601}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709       = DXGI_COLOR_SPACE_TYPE(8);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709}
  DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P709         = DXGI_COLOR_SPACE_TYPE(9);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P709}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020      = DXGI_COLOR_SPACE_TYPE(10);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020}
  DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020        = DXGI_COLOR_SPACE_TYPE(11);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020}
  DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020        = DXGI_COLOR_SPACE_TYPE(12);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_LEFT_P2020    = DXGI_COLOR_SPACE_TYPE(13);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_LEFT_P2020}
  DXGI_COLOR_SPACE_RGB_STUDIO_G2084_NONE_P2020      = DXGI_COLOR_SPACE_TYPE(14);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_STUDIO_G2084_NONE_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_TOPLEFT_P2020   = DXGI_COLOR_SPACE_TYPE(15);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_TOPLEFT_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_TOPLEFT_P2020 = DXGI_COLOR_SPACE_TYPE(16);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_TOPLEFT_P2020}
  DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P2020          = DXGI_COLOR_SPACE_TYPE(17);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_GHLG_TOPLEFT_P2020  = DXGI_COLOR_SPACE_TYPE(18);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_GHLG_TOPLEFT_P2020}
  DXGI_COLOR_SPACE_YCBCR_FULL_GHLG_TOPLEFT_P2020    = DXGI_COLOR_SPACE_TYPE(19);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_FULL_GHLG_TOPLEFT_P2020}
  DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P709         = DXGI_COLOR_SPACE_TYPE(20);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P709}
  DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P2020        = DXGI_COLOR_SPACE_TYPE(21);
  {$EXTERNALSYM DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P709       = DXGI_COLOR_SPACE_TYPE(22);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P709}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P2020      = DXGI_COLOR_SPACE_TYPE(23);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P2020}
  DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_TOPLEFT_P2020   = DXGI_COLOR_SPACE_TYPE(24);
  {$EXTERNALSYM DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_TOPLEFT_P2020}
  //DXGI_COLOR_SPACE_CUSTOM               = FORCEDWORD);


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
