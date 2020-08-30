// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1_3Helper.pas
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
// Source: d2d1_3helper.h
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
unit WinApi.DirectX.D2D1_3Helper;

  {$HPPEMIT '#include "d2d1_3helper.h"'}

interface

uses
  {WinApi.DirectX}
  WinApi.Windows,
  {MfPack}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_3,
  WinApi.DirectX.D2DBaseTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


  // D2D1_GRADIENT_MESH_PATCH
  function GradientMeshPatch(point00: D2D1_POINT_2F;
                             point01: D2D1_POINT_2F;
                             point02: D2D1_POINT_2F;
                             point03: D2D1_POINT_2F;
                             point10: D2D1_POINT_2F;
                             point11: D2D1_POINT_2F;
                             point12: D2D1_POINT_2F;
                             point13: D2D1_POINT_2F;
                             point20: D2D1_POINT_2F;
                             point21: D2D1_POINT_2F;
                             point22: D2D1_POINT_2F;
                             point23: D2D1_POINT_2F;
                             point30: D2D1_POINT_2F;
                             point31: D2D1_POINT_2F;
                             point32: D2D1_POINT_2F;
                             point33: D2D1_POINT_2F;
                             color00: D2D1_COLOR_F;
                             color03: D2D1_COLOR_F;
                             color30: D2D1_COLOR_F;
                             color33: D2D1_COLOR_F;
                             topEdgeMode: D2D1_PATCH_EDGE_MODE;
                             leftEdgeMode: D2D1_PATCH_EDGE_MODE;
                             bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
                             rightEdgeMode: D2D1_PATCH_EDGE_MODE): D2D1_GRADIENT_MESH_PATCH; inline;
  {$EXTERNALSYM GradientMeshPatch}


  function GradientMeshPatchFromCoonsPatch(point0: D2D1_POINT_2F;
                                           point1: D2D1_POINT_2F;
                                           point2: D2D1_POINT_2F;
                                           point3: D2D1_POINT_2F;
                                           point4: D2D1_POINT_2F;
                                           point5: D2D1_POINT_2F;
                                           point6: D2D1_POINT_2F;
                                           point7: D2D1_POINT_2F;
                                           point8: D2D1_POINT_2F;
                                           point9: D2D1_POINT_2F;
                                           point10: D2D1_POINT_2F;
                                           point11: D2D1_POINT_2F;
                                           color0: D2D1_COLOR_F;
                                           color1: D2D1_COLOR_F;
                                           color2: D2D1_COLOR_F;
                                           color3: D2D1_COLOR_F;
                                           topEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           leftEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           rightEdgeMode: D2D1_PATCH_EDGE_MODE): D2D1_GRADIENT_MESH_PATCH; inline;
  {$EXTERNALSYM GradientMeshPatchFromCoonsPatch}


  function InkPoint(const point: D2D1_POINT_2F;
                    radius: Single): D2D1_INK_POINT;
  {$EXTERNALSYM InkPoint}


  function InkBezierSegment(const point1: D2D1_INK_POINT;
                            const point2: D2D1_INK_POINT;
                            const point3: D2D1_INK_POINT): D2D1_INK_BEZIER_SEGMENT; inline;
  {$EXTERNALSYM InkBezierSegment}


  function InkStyleProperties(nibShape: D2D1_INK_NIB_SHAPE;
                              nibTransform: D2D1_MATRIX_3X2_F): D2D1_INK_STYLE_PROPERTIES; inline;
  {$EXTERNALSYM InkStyleProperties}


  function InfiniteRectU(): D2D1_RECT_U; inline;
  {$EXTERNALSYM InfiniteRectU}


  function SimpleColorProfile(const redPrimary: D2D1_POINT_2F;
                              const greenPrimary: D2D1_POINT_2F;
                              const bluePrimary: D2D1_POINT_2F;
                              const gamma: D2D1_GAMMA1;
                              const whitePointXZ: D2D1_POINT_2F): D2D1_SIMPLE_COLOR_PROFILE; inline;
  {$EXTERNALSYM SimpleColorProfile}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation


function GradientMeshPatch(point00: D2D1_POINT_2F;
                           point01: D2D1_POINT_2F;
                           point02: D2D1_POINT_2F;
                           point03: D2D1_POINT_2F;
                           point10: D2D1_POINT_2F;
                           point11: D2D1_POINT_2F;
                           point12: D2D1_POINT_2F;
                           point13: D2D1_POINT_2F;
                           point20: D2D1_POINT_2F;
                           point21: D2D1_POINT_2F;
                           point22: D2D1_POINT_2F;
                           point23: D2D1_POINT_2F;
                           point30: D2D1_POINT_2F;
                           point31: D2D1_POINT_2F;
                           point32: D2D1_POINT_2F;
                           point33: D2D1_POINT_2F;
                           color00: D2D1_COLOR_F;
                           color03: D2D1_COLOR_F;
                           color30: D2D1_COLOR_F;
                           color33: D2D1_COLOR_F;
                           topEdgeMode: D2D1_PATCH_EDGE_MODE;
                           leftEdgeMode: D2D1_PATCH_EDGE_MODE;
                           bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
                           rightEdgeMode: D2D1_PATCH_EDGE_MODE): D2D1_GRADIENT_MESH_PATCH; inline;
var
  newPatch: D2D1_GRADIENT_MESH_PATCH;
 begin

        newPatch.point00 := point00;
        newPatch.point01 := point01;
        newPatch.point02 := point02;
        newPatch.point03 := point03;
        newPatch.point10 := point10;
        newPatch.point11 := point11;
        newPatch.point12 := point12;
        newPatch.point13 := point13;
        newPatch.point20 := point20;
        newPatch.point21 := point21;
        newPatch.point22 := point22;
        newPatch.point23 := point23;
        newPatch.point30 := point30;
        newPatch.point31 := point31;
        newPatch.point32 := point32;
        newPatch.point33 := point33;

        newPatch.color00 := color00;
        newPatch.color03 := color03;
        newPatch.color30 := color30;
        newPatch.color33 := color33;

        newPatch.topEdgeMode := topEdgeMode;
        newPatch.leftEdgeMode := leftEdgeMode;
        newPatch.bottomEdgeMode := bottomEdgeMode;
        newPatch.rightEdgeMode := rightEdgeMode;

        Result := newPatch;
end;


function GradientMeshPatchFromCoonsPatch(point0: D2D1_POINT_2F;
                                           point1: D2D1_POINT_2F;
                                           point2: D2D1_POINT_2F;
                                           point3: D2D1_POINT_2F;
                                           point4: D2D1_POINT_2F;
                                           point5: D2D1_POINT_2F;
                                           point6: D2D1_POINT_2F;
                                           point7: D2D1_POINT_2F;
                                           point8: D2D1_POINT_2F;
                                           point9: D2D1_POINT_2F;
                                           point10: D2D1_POINT_2F;
                                           point11: D2D1_POINT_2F;
                                           color0: D2D1_COLOR_F;
                                           color1: D2D1_COLOR_F;
                                           color2: D2D1_COLOR_F;
                                           color3: D2D1_COLOR_F;
                                           topEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           leftEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
                                           rightEdgeMode: D2D1_PATCH_EDGE_MODE): D2D1_GRADIENT_MESH_PATCH; inline;
var
  newPatch: D2D1_GRADIENT_MESH_PATCH;

begin
  newPatch.point00 := point0;
  newPatch.point01 := point1;
  newPatch.point02 := point2;
  newPatch.point03 := point3;
  newPatch.point13 := point4;
  newPatch.point23 := point5;
  newPatch.point33 := point6;
  newPatch.point32 := point7;
  newPatch.point31 := point8;
  newPatch.point30 := point9;
  newPatch.point20 := point10;
  newPatch.point10 := point11;

  D2D1GetGradientMeshInteriorPointsFromCoonsPatch(point0,
                                                  point1,
                                                  point2,
                                                  point3,
                                                  point4,
                                                  point5,
                                                  point6,
                                                  point7,
                                                  point8,
                                                  point9,
                                                  point10,
                                                  point11,
                                                  newPatch.point11,
                                                  newPatch.point12,
                                                  newPatch.point21,
                                                  newPatch.point22);
  newPatch.color00 := color0;
  newPatch.color03 := color1;
  newPatch.color33 := color2;
  newPatch.color30 := color3;
  newPatch.topEdgeMode := topEdgeMode;
  newPatch.leftEdgeMode := leftEdgeMode;
  newPatch.bottomEdgeMode := bottomEdgeMode;
  newPatch.rightEdgeMode := rightEdgeMode;

  Result := newPatch;
end;


function InkPoint(const point: D2D1_POINT_2F;
                  radius: Single): D2D1_INK_POINT; inline;
var
  inkPoint: D2D1_INK_POINT;

begin
  inkPoint.x := point.x;
  inkPoint.y := point.y;
  inkPoint.radius := radius;

  Result := inkPoint;
end;


function InkBezierSegment(const point1: D2D1_INK_POINT;
                            const point2: D2D1_INK_POINT;
                            const point3: D2D1_INK_POINT): D2D1_INK_BEZIER_SEGMENT; inline;
var
  inkBezierSegment: D2D1_INK_BEZIER_SEGMENT;

begin
  inkBezierSegment.point1 := point1;
  inkBezierSegment.point2 := point2;
  inkBezierSegment.point3 := point3;

  Result := inkBezierSegment;
end;



function InkStyleProperties(nibShape: D2D1_INK_NIB_SHAPE;
                            nibTransform: D2D1_MATRIX_3X2_F): D2D1_INK_STYLE_PROPERTIES; inline;
var
  inkStyleProperties: D2D1_INK_STYLE_PROPERTIES;

begin
  inkStyleProperties.nibShape := nibShape;
  inkStyleProperties.nibTransform := nibTransform;

  Result := inkStyleProperties;
end;


function InfiniteRectU(): D2D1_RECT_U; inline;
var
  rect: D2D1_RECT_U;

begin
  rect.left := 0;
  rect.top := 0;
  rect.right := $7FFFFFFF;
  rect.bottom := $7FFFFFFF;

  Result := rect;
end;



function SimpleColorProfile(const redPrimary: D2D1_POINT_2F;
                            const greenPrimary: D2D1_POINT_2F;
                            const bluePrimary: D2D1_POINT_2F;
                            const gamma: D2D1_GAMMA1;
                            const whitePointXZ: D2D1_POINT_2F): D2D1_SIMPLE_COLOR_PROFILE; inline;
var
  simpleColorProfile: D2D1_SIMPLE_COLOR_PROFILE;

begin
  simpleColorProfile.redPrimary := redPrimary;
  simpleColorProfile.greenPrimary := greenPrimary;
  simpleColorProfile.bluePrimary := bluePrimary;
  simpleColorProfile.gamma := gamma;
  simpleColorProfile.whitePointXZ := whitePointXZ;

  Result := simpleColorProfile;
end;

  // Implement Additional functions here.

end.
