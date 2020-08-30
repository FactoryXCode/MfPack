// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1_2Helper.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Helper files over the D2D interfaces and APIs.
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
// Remarks: Minimum supported client: Windows 8.1
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
// Source: d2d1_2helper.h
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
unit WinApi.DirectX.D2D1_2Helper;

  {$HPPEMIT '#include "d2d1_2helper.h"'}

interface

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

//#if NTDDI_VERSION >= NTDDI_WINBLUE

uses
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_2,
  WinApi.DirectX.D2D1Helper,
  WinApi.DirectX.DCommon;


  function ComputeFlatteningTolerance(matrix: D2D1_MATRIX_3X2_F;
                                      dpiX: Single = 96.0;
                                      dpiY: Single = 96.0;
                                      maxZoomFactor: Single = 1.0): Single; inline;
  {$EXTERNALSYM ComputeFlatteningTolerance}

//#if NTDDI_VERSION >= NTDDI_WINBLUE

  // Computes the maximum factor by which a given transform can stretch any vector.
  // See: https://docs.microsoft.com/en-us/windows/desktop/api/d2d1_2/nf-d2d1_2-d2d1computemaximumscalefactor
  // Delphi Note: This function has been moved from D2D1_2.pas to prevent circular reference error.
  //function D2D1ComputeMaximumScaleFactor(matrix: D2D1_MATRIX_3X2_F): Single; stdcall;

//#endif // #if NTDDI_VERSION >= NTDDI_WINBLUE


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


function ComputeFlatteningTolerance(matrix: D2D1_MATRIX_3X2_F;
                                    dpiX: Single = 96.0;
                                    dpiY: Single = 96.0;
                                    maxZoomFactor: Single = 1.0): Single; inline;
var
  dpiDependentTransform: D2D1_MATRIX_3X2_F;
  absMaxZoomFactor: Single;
  x: Single;
  y: Single;

begin
  x := dpiX / 96.0;
  y := dpiY / 96.0;

  dpiDependentTransform :=  matrix * D2D1_MATRIX_3X2_F.Scale(x,
                                                             y,
                                                             Point2F());
  if (maxZoomFactor > 0) then
        absMaxZoomFactor := maxZoomFactor
  else
        absMaxZoomFactor := -maxZoomFactor;

  Result := D2D1_DEFAULT_FLATTENING_TOLERANCE / (absMaxZoomFactor * D2D1ComputeMaximumScaleFactor(dpiDependentTransform));
end;

  // Implement Additional functions here.

end.
