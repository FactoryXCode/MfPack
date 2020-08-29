// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DCompTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
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
// Source: dcomptypes.h
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
unit WinApi.DirectX.DCompTypes;

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DXGIType,
  WinApi.DirectX.DXGICommon;

  {$WEAKPACKAGEUNIT ON}


// #if (NTDDI_VERSION >= NTDDI_WIN8)

//
// DirectComposition types
//

type
  PDCOMPOSITION_BITMAP_INTERPOLATION_MODE = ^DCOMPOSITION_BITMAP_INTERPOLATION_MODE;
  DCOMPOSITION_BITMAP_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM DCOMPOSITION_BITMAP_INTERPOLATION_MODE}
const
  DCOMPOSITION_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR = DCOMPOSITION_BITMAP_INTERPOLATION_MODE(0);
  {$EXTERNALSYM DCOMPOSITION_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR}
  DCOMPOSITION_BITMAP_INTERPOLATION_MODE_LINEAR           = DCOMPOSITION_BITMAP_INTERPOLATION_MODE(1);
  {$EXTERNALSYM DCOMPOSITION_BITMAP_INTERPOLATION_MODE_LINEAR}
  //DCOMPOSITION_BITMAP_INTERPOLATION_MODE_INHERIT      = FORCEDWORD;

type
  PDCOMPOSITION_BORDER_MODE = ^DCOMPOSITION_BORDER_MODE;
  DCOMPOSITION_BORDER_MODE = DWord;
  {$EXTERNALSYM DCOMPOSITION_BORDER_MODE}
const
  DCOMPOSITION_BORDER_MODE_SOFT     = DCOMPOSITION_BORDER_MODE(0);
  {$EXTERNALSYM DCOMPOSITION_BORDER_MODE_SOFT}
  DCOMPOSITION_BORDER_MODE_HARD     = DCOMPOSITION_BORDER_MODE(1);
  {$EXTERNALSYM DCOMPOSITION_BORDER_MODE_HARD}
  //DCOMPOSITION_BORDER_MODE_INHERIT  = FORCEDWORD;

type
  PDCOMPOSITION_COMPOSITE_MODE = ^DCOMPOSITION_COMPOSITE_MODE;
  DCOMPOSITION_COMPOSITE_MODE = DWord;
  {$EXTERNALSYM DCOMPOSITION_COMPOSITE_MODE}
const
  DCOMPOSITION_COMPOSITE_MODE_SOURCE_OVER        = DCOMPOSITION_COMPOSITE_MODE(0);
  {$EXTERNALSYM DCOMPOSITION_COMPOSITE_MODE_SOURCE_OVER}
  DCOMPOSITION_COMPOSITE_MODE_DESTINATION_INVERT = DCOMPOSITION_COMPOSITE_MODE(1);
  {$EXTERNALSYM DCOMPOSITION_COMPOSITE_MODE_DESTINATION_INVERT}
// #if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
  DCOMPOSITION_COMPOSITE_MODE_MIN_BLEND          = DCOMPOSITION_COMPOSITE_MODE(2);
  {$EXTERNALSYM DCOMPOSITION_COMPOSITE_MODE_MIN_BLEND}
// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
  //DCOMPOSITION_COMPOSITE_MODE_INHERIT      = FORCEDWORD;

type
// #if (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)
  PDCOMPOSITION_BACKFACE_VISIBILITY = ^DCOMPOSITION_BACKFACE_VISIBILITY;
  DCOMPOSITION_BACKFACE_VISIBILITY  = DWord;
  {$EXTERNALSYM DCOMPOSITION_BACKFACE_VISIBILITY}
const
  DCOMPOSITION_BACKFACE_VISIBILITY_VISIBLE  = DCOMPOSITION_BACKFACE_VISIBILITY(0);
  {$EXTERNALSYM DCOMPOSITION_BACKFACE_VISIBILITY_VISIBLE}
  DCOMPOSITION_BACKFACE_VISIBILITY_HIDDEN   = DCOMPOSITION_BACKFACE_VISIBILITY(1);
  {$EXTERNALSYM DCOMPOSITION_BACKFACE_VISIBILITY_HIDDEN}
  //DCOMPOSITION_BACKFACE_VISIBILITY_INHERIT  = FORCEDWORD;

type
  PDCOMPOSITION_OPACITY_MODE = ^DCOMPOSITION_OPACITY_MODE;
  DCOMPOSITION_OPACITY_MODE = DWord;
  {$EXTERNALSYM DCOMPOSITION_OPACITY_MODE}
const
  DCOMPOSITION_OPACITY_MODE_LAYER     = DCOMPOSITION_OPACITY_MODE(0);
  {$EXTERNALSYM DCOMPOSITION_OPACITY_MODE_LAYER}
  DCOMPOSITION_OPACITY_MODE_MULTIPLY  = DCOMPOSITION_OPACITY_MODE(1);
  {$EXTERNALSYM DCOMPOSITION_OPACITY_MODE_MULTIPLY}
  //DCOMPOSITION_OPACITY_MODE_INHERIT   = FORCEDWORD;

// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINBLUE)

// #if (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)
type
  PDCOMPOSITION_DEPTH_MODE = ^DCOMPOSITION_DEPTH_MODE;
  DCOMPOSITION_DEPTH_MODE = DWord;
  {$EXTERNALSYM DCOMPOSITION_DEPTH_MODE}
const
  DCOMPOSITION_DEPTH_MODE_TREE    = DCOMPOSITION_DEPTH_MODE(0);
  {$EXTERNALSYM DCOMPOSITION_DEPTH_MODE_TREE}
  DCOMPOSITION_DEPTH_MODE_SPATIAL = DCOMPOSITION_DEPTH_MODE(1);
  {$EXTERNALSYM DCOMPOSITION_DEPTH_MODE_SPATIAL}
  DCOMPOSITION_DEPTH_MODE_SORTED  = DCOMPOSITION_DEPTH_MODE(2);
  {$EXTERNALSYM DCOMPOSITION_DEPTH_MODE_SORTED}
  //DCOMPOSITION_DEPTH_MODE_INHERIT = FORCEDWORD;

// #endif  // (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)

type

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
