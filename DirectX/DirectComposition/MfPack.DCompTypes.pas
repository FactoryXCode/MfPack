// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DCompTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
//
// Description: Enables high-performance bitmap composition with transforms,
//              effects, and animations.
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
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX261
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: dcomptypes.h
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
unit MfPack.DCompTypes;

interface

uses
  {WinApi}
  WinApi.Windows,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DXGIType,
  MfPack.DXGICommon;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

// #if (NTDDI_VERSION >= NTDDI_WIN8)

//
// DirectComposition types
//

type
  PDCOMPOSITION_BITMAP_INTERPOLATION_MODE = ^DCOMPOSITION_BITMAP_INTERPOLATION_MODE;
  DCOMPOSITION_BITMAP_INTERPOLATION_MODE = (
    DCOMPOSITION_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR = 0,
    DCOMPOSITION_BITMAP_INTERPOLATION_MODE_LINEAR           = 1,
    DCOMPOSITION_BITMAP_INTERPOLATION_MODE_INHERIT          = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_BITMAP_INTERPOLATION_MODE}

  PDCOMPOSITION_BORDER_MODE = ^DCOMPOSITION_BORDER_MODE;
  DCOMPOSITION_BORDER_MODE = (
    DCOMPOSITION_BORDER_MODE_SOFT       = 0,
    DCOMPOSITION_BORDER_MODE_HARD       = 1,
    DCOMPOSITION_BORDER_MODE_INHERIT    = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_BORDER_MODE}

  PDCOMPOSITION_COMPOSITE_MODE = ^DCOMPOSITION_COMPOSITE_MODE;
  DCOMPOSITION_COMPOSITE_MODE = (
    DCOMPOSITION_COMPOSITE_MODE_SOURCE_OVER        = 0,
    DCOMPOSITION_COMPOSITE_MODE_DESTINATION_INVERT = 1,
// #if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
    DCOMPOSITION_COMPOSITE_MODE_MIN_BLEND          = 2,
// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
    DCOMPOSITION_COMPOSITE_MODE_INHERIT            = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_COMPOSITE_MODE}


// #if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
  PDCOMPOSITION_BACKFACE_VISIBILITY = ^DCOMPOSITION_BACKFACE_VISIBILITY;
  DCOMPOSITION_BACKFACE_VISIBILITY  = (
    DCOMPOSITION_BACKFACE_VISIBILITY_VISIBLE    = 0,
    DCOMPOSITION_BACKFACE_VISIBILITY_HIDDEN     = 1,
    DCOMPOSITION_BACKFACE_VISIBILITY_INHERIT    = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_BACKFACE_VISIBILITY}

  PDCOMPOSITION_OPACITY_MODE = ^DCOMPOSITION_OPACITY_MODE;
  DCOMPOSITION_OPACITY_MODE = (
    DCOMPOSITION_OPACITY_MODE_LAYER     = 0,
    DCOMPOSITION_OPACITY_MODE_MULTIPLY  = 1,
    DCOMPOSITION_OPACITY_MODE_INHERIT   = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_OPACITY_MODE}

// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

// #if (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)
  PDCOMPOSITION_DEPTH_MODE = ^DCOMPOSITION_DEPTH_MODE;
  DCOMPOSITION_DEPTH_MODE = (
    DCOMPOSITION_DEPTH_MODE_TREE    = 0,
    DCOMPOSITION_DEPTH_MODE_SPATIAL = 1,
    DCOMPOSITION_DEPTH_MODE_SORTED  = 3,
    DCOMPOSITION_DEPTH_MODE_INHERIT = FORCEDWORD);
  {$EXTERNALSYM DCOMPOSITION_DEPTH_MODE}

// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)

  PDCOMPOSITION_FRAME_STATISTICS = ^DCOMPOSITION_FRAME_STATISTICS;
  DCOMPOSITION_FRAME_STATISTICS = record
    lastFrameTime: LARGE_INTEGER;
    currentCompositionRate: DXGI_RATIONAL;
    currentTime: LARGE_INTEGER;
    timeFrequency: LARGE_INTEGER;
    nextEstimatedFrameTime: LARGE_INTEGER;
  end;
  {$EXTERNALSYM DCOMPOSITION_FRAME_STATISTICS}


//
// Composition object specific access flags
//
const

  COMPOSITIONOBJECT_READ              = $0001;
  {$EXTERNALSYM COMPOSITIONOBJECT_READ}
  COMPOSITIONOBJECT_WRITE             = $0002;
  {$EXTERNALSYM COMPOSITIONOBJECT_WRITE}
  COMPOSITIONOBJECT_ALL_ACCESS        = (COMPOSITIONOBJECT_READ or COMPOSITIONOBJECT_WRITE);
  {$EXTERNALSYM COMPOSITIONOBJECT_ALL_ACCESS}

// #endif // NTDDI_WIN8


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
