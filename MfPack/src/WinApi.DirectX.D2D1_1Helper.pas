// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1_1Helper.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Helper files over the D2D interfaces and APIs.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Jim Hawkins.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
// 20/12/2021 Jim Hawkins         Added system.math in uses clause, removed Single casting.
//------------------------------------------------------------------------------
//
// Remarks: Here we use Objects as records in place for records to make inheritance possible.
//          Note: Objects are placed on the heap instead of stack.
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: Test if the concept of objects works.
//
//==============================================================================
// Source: d2d1_1helper.h
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
unit WinApi.DirectX.D2D1_1Helper;

  {$HPPEMIT '#include "d2d1_1helper.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Math,
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.D2D1helper,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

type

  //Matrix4x3F as D2D1_MATRIX_4X3_F;
  // In Delphi you can't inherid from a record
  Matrix4x3F = Object(D2D1_MATRIX_4X3_F)

        constructor Create(m11: Single = 1;
                           m12: Single = 0;
                           m13: Single = 0;

                           m21: Single = 0;
                           m22: Single = 1;
                           m23: Single = 0;

                           m31: Single = 0;
                           m32: Single = 0;
                           m33: Single = 1;

                           m41: Single = 0;
                           m42: Single = 0;
                           m43: Single = 0);
  end;


  Matrix4x4F = Object(D2D1_MATRIX_4X4_F)
    public

        constructor Create(m11: Single = 1;
                           m12: Single = 0;
                           m13: Single = 0;
                           m14: Single = 0;

                           m21: Single = 0;
                           m22: Single = 1;
                           m23: Single = 0;
                           m24: Single = 0;

                           m31: Single = 0;
                           m32: Single = 0;
                           m33: Single = 1;
                           m34: Single = 0;

                           m41: Single = 0;
                           m42: Single = 0;
                           m43: Single = 0;
                           m44: Single = 1);

        function Translation(x: Single;
                             y: Single;
                             z: Single): Matrix4x4F; inline;

        function Scale(x: Single;
                       y: Single;
                       z: Single): Matrix4x4F; inline;

        function RotationX(degreeX: Single): Matrix4x4F; inline;

        function RotationY(degreeY: Single): Matrix4x4F; inline;

        function RotationZ(degreeZ: Single): Matrix4x4F; inline;

        //
        // 3D Rotation matrix for an arbitrary axis specified by x, y and z
        //

        function RotationArbitraryAxis(x: Single;
                                       y: Single;
                                       z: Single;
                                       degree: Single): Matrix4x4F; inline;

        function SkewX(degreeX: Single): Matrix4x4F; inline;

        function SkewY(degreeY: Single): Matrix4x4F; inline;

        function PerspectiveProjection(depth: Single): Matrix4x4F; inline;


        //
        // Functions for convertion from the base D2D1_MATRIX_4X4_f to
        // this type without making a copy
        //

        function ReinterpretBaseType(const pMatrix: D2D1_MATRIX_4X4_F): Matrix4x4F;

        function Determinant(): Single; inline;

        function IsIdentity(): Boolean; inline;

        procedure SetProduct(var a: Matrix4x4F;
                             var b: Matrix4x4F); inline;

        function product(matrix: Matrix4x4F): Matrix4x4F; inline;

    end;

  Matrix5x4F = Object(D2D1_MATRIX_5X4_F)
  public

    constructor Create(m11: Single = 1;
                       m12: Single = 0;
                       m13: Single = 0;
                       m14: Single = 0;

                       m21: Single = 0;
                       m22: Single = 1;
                       m23: Single = 0;
                       m24: Single = 0;

                       m31: Single = 0;
                       m32: Single = 0;
                       m33: Single = 1;
                       m34: Single = 0;

                       m41: Single = 0;
                       m42: Single = 0;
                       m43: Single = 0;
                       m44: Single = 1;

                       m51: Single = 0;
                       m52: Single = 0;
                       m53: Single = 0;
                       m54: Single = 0);

  end;




function ConvertColorSpace(sourceColorSpace: D2D1_COLOR_SPACE;
                           destinationColorSpace: D2D1_COLOR_SPACE;
                           color: D2D1_COLOR_F): D2D1_COLOR_F;
{$EXTERNALSYM ConvertColorSpace}

function DrawingStateDescription1(transform: D2D1_MATRIX_3X2_F;
                                  antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                  textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                  tag1: D2D1_TAG = 0;
                                  tag2: D2D1_TAG = 0;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
{$EXTERNALSYM DrawingStateDescription1}

function DrawingStateDescription1(antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                  textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                  tag1: D2D1_TAG = 0;
                                  tag2: D2D1_TAG = 0;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
{$EXTERNALSYM DrawingStateDescription1}

function DrawingStateDescription1(desc: D2D1_DRAWING_STATE_DESCRIPTION;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
{$EXTERNALSYM DrawingStateDescription1}

function BitmapProperties1(_pixelFormat: D2D1_PIXEL_FORMAT;
                           bitmapOptions: D2D1_BITMAP_OPTIONS = D2D1_BITMAP_OPTIONS_NONE;
                           dpiX: Single = 96.0;
                           dpiY: Single = 96.0;
                           colorContext: ID2D1ColorContext = nil): D2D1_BITMAP_PROPERTIES1; overload;
{$EXTERNALSYM BitmapProperties1}

function BitmapProperties1(bitmapOptions: D2D1_BITMAP_OPTIONS = D2D1_BITMAP_OPTIONS_NONE;
                           dpiX: Single = 96.0;
                           dpiY: Single = 96.0;
                           colorContext: ID2D1ColorContext = nil): D2D1_BITMAP_PROPERTIES1; overload;
{$EXTERNALSYM BitmapProperties1}

function LayerParameters1(contentBounds: D2D1_RECT_F;
                          maskTransform: D2D1_MATRIX_3X2_F;
                          geometricMask: ID2D1Geometry = Nil;
                          maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                          opacity: Single = 1.0;
                          opacityBrush: ID2D1Brush = nil;
                          layerOptions: D2D1_LAYER_OPTIONS1 = D2D1_LAYER_OPTIONS1_NONE): D2D1_LAYER_PARAMETERS1; overload;
{$EXTERNALSYM LayerParameters1}

function LayerParameters1(geometricMask: ID2D1Geometry = nil;
                          maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                          opacity: Single = 1.0;
                          opacityBrush: ID2D1Brush = nil;
                          layerOptions: D2D1_LAYER_OPTIONS1 = D2D1_LAYER_OPTIONS1_NONE): D2D1_LAYER_PARAMETERS1; overload;
{$EXTERNALSYM LayerParameters1}

function StrokeStyleProperties1(startCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                endCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                dashCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                lineJoin: D2D1_LINE_JOIN = D2D1_LINE_JOIN_MITER;
                                miterLimit: single = 10.0;
                                dashStyle: D2D1_DASH_STYLE = D2D1_DASH_STYLE_SOLID;
                                dashOffset: Single = 0.0;
                                transformType: D2D1_STROKE_TRANSFORM_TYPE = D2D1_STROKE_TRANSFORM_TYPE_NORMAL): D2D1_STROKE_STYLE_PROPERTIES1;
{$EXTERNALSYM StrokeStyleProperties1}

function ImageBrushProperties(sourceRectangle: D2D1_RECT_F;
                              extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                              extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                              interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR): D2D1_IMAGE_BRUSH_PROPERTIES;
{$EXTERNALSYM ImageBrushProperties}

function BitmapBrushProperties1(extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR): D2D1_BITMAP_BRUSH_PROPERTIES1;
{$EXTERNALSYM BitmapBrushProperties1}

function PrintControlProperties(fontSubsetMode: D2D1_PRINT_FONT_SUBSET_MODE = D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT;
                                rasterDpi: Single = 150.0;
                                colorSpace: D2D1_COLOR_SPACE = D2D1_COLOR_SPACE_SRGB): D2D1_PRINT_CONTROL_PROPERTIES;
{$EXTERNALSYM PrintControlProperties}

function RenderingControls(bufferPrecision: D2D1_BUFFER_PRECISION;
                           tileSize: D2D1_SIZE_U): D2D1_RENDERING_CONTROLS;
{$EXTERNALSYM RenderingControls}

function EffectInputDescription(effect: ID2D1Effect;
                                inputIndex: UINT32;
                                inputRectangle: D2D1_RECT_F): D2D1_EFFECT_INPUT_DESCRIPTION;
{$EXTERNALSYM EffectInputDescription}

function CreationProperties(threadingMode: D2D1_THREADING_MODE;
                            debugLevel: D2D1_DEBUG_LEVEL;
                            options: D2D1_DEVICE_CONTEXT_OPTIONS): D2D1_CREATION_PROPERTIES;
{$EXTERNALSYM CreationProperties}

function Vector2F(x: Single = 0.0;
                  y: Single = 0.0): D2D1_VECTOR_2F;
{$EXTERNALSYM Vector2F}

function Vector3F(x: Single = 0.0;
                  y: Single = 0.0;
                  z: Single = 0.0): D2D1_VECTOR_3F;
{$EXTERNALSYM Vector3F}

function Vector4F(x: Single = 0.0;
                  y: Single = 0.0;
                  z: Single = 0.0;
                  w: Single = 0.0): D2D1_VECTOR_4F;
{$EXTERNALSYM Vector4F}

function Point2L(x: INT32 = 0;
                 y: INT32 = 0): D2D1_POINT_2L;
{$EXTERNALSYM Point2L}

function RectL(left: INT32 = 0;
               top: INT32 = 0;
               right: INT32 = 0;
               bottom: INT32 = 0): D2D1_RECT_L;
{$EXTERNALSYM RectL}

//
// Sets a bitmap as an effect input, while inserting a DPI compensation effect
// to preserve visual appearance as the device context's DPI changes.
//
function SetDpiCompensatedEffectInput(deviceContext: ID2D1DeviceContext;
                                      effect: ID2D1Effect;
                                      inputIndex: UINT32;
                                      inputBitmap: ID2D1Bitmap = nil;
                                      interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR;
                                      borderMode: D2D1_BORDER_MODE = D2D1_BORDER_MODE_HARD): HRESULT;
{$EXTERNALSYM SetDpiCompensatedEffectInput}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


constructor Matrix4x3F.Create(m11: Single = 1;
                              m12: Single = 0;
                              m13: Single = 0;

                              m21: Single = 0;
                              m22: Single = 1;
                              m23: Single = 0;

                              m31: Single = 0;
                              m32: Single = 0;
                              m33: Single = 1;

                              m41: Single = 0;
                              m42: Single = 0;
                              m43: Single = 0);
begin
  _11 := m11;
  _12 := m12;
  _13 := m13;

  _21 := m21;
  _22 := m22;
  _23 := m23;

  _31 := m31;
  _32 := m32;
  _33 := m33;

  _41 := m41;
  _42 := m42;
  _43 := m43;
end;


constructor Matrix4x4F.Create(m11: Single = 1;
                              m12: Single = 0;
                              m13: Single = 0;
                              m14: Single = 0;

                              m21: Single = 0;
                              m22: Single = 1;
                              m23: Single = 0;
                              m24: Single = 0;

                              m31: Single = 0;
                              m32: Single = 0;
                              m33: Single = 1;
                              m34: Single = 0;

                              m41: Single = 0;
                              m42: Single = 0;
                              m43: Single = 0;
                              m44: Single = 1);
begin
  inherited;
  _11 := m11;
  _12 := m12;
  _13 := m13;
  _14 := m14;

  _21 := m21;
  _22 := m22;
  _23 := m23;
  _24 := m24;

  _31 := m31;
  _32 := m32;
  _33 := m33;
  _34 := m34;

  _41 := m41;
  _42 := m42;
  _43 := m43;
  _44 := m44;
end;


function Matrix4x4f.Translation(x: Single;
                                y: Single;
                                z: Single): Matrix4x4F;
var
  transl: Matrix4x4F;

begin

  transl._11 := 1.0;
  transl._12 := 0.0;
  transl._13 := 0.0;
  transl._14 := 0.0;

  transl._21 := 0.0;
  transl._22 := 1.0;
  transl._23 := 0.0;
  transl._24 := 0.0;

  transl._31 := 0.0;
  transl._32 := 0.0;
  transl._33 := 1.0;
  transl._34 := 0.0;

  transl._41 := x;
  transl._42 := y;
  transl._43 := z;
  transl._44 := 1.0;

  Result := transl;
end;


function Matrix4x4f.Scale(x: Single;
                         y: Single;
                         z: Single): Matrix4x4F;
var
   scl: Matrix4x4F;

begin
   scl._11 := x;
   scl._12 := 0.0;
   scl._13 := 0.0;
   scl._14 := 0.0;

   scl._21 := 0.0;
   scl._22 := y;
   scl._23 := 0.0;
   scl._24 := 0.0;

   scl._31 := 0.0;
   scl._32 := 0.0;
   scl._33 := z;
   scl._34 := 0.0;

   scl._41 := 0.0;
   scl._42 := 0.0;
   scl._43 := 0.0;
   scl._44 := 1.0;

   Result := scl;
end;


function Matrix4x4f.RotationX(degreeX: Single): Matrix4x4F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;
  MRotationX: Matrix4x4F;

begin
  // angleInRadian := degreeX * (Pi / 180.0);  // same as DegToRad(degreeX)
  angleInRadian := DegToRad(degreeX);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  MRotationX._11 := 1;
  MRotationX._12 := 0;
  MRotationX._13 := 0;
  MRotationX._14 := 0;

  MRotationX._21 := 0;
  MRotationX._22 := cosAngle;
  MRotationX._23 := sinAngle;
  MRotationX._24 := 0;

  MRotationX._31 := 0;
  MRotationX._32 := -sinAngle;
  MRotationX._33 := cosAngle;
  MRotationX._34 := 0;

  MRotationX._41 := 0;
  MRotationX._42 := 0;
  MRotationX._43 := 0;
  MRotationX._44 := 1;

  Result := MRotationX;
end;


function Matrix4x4F.RotationZ(degreeZ: Single): Matrix4x4F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;
  MRotationZ: Matrix4x4F;

begin
  // angleInRadian := degreeZ * (Pi / 180.0); same as DegToRad(degreeZ)
  angleInRadian := DegToRad(degreeZ);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  MRotationZ._11 := cosAngle;
  MRotationZ._12 := sinAngle;
  MRotationZ._13 := 0;
  MRotationZ._14 := 0;

  MRotationZ._21 := -sinAngle;
  MRotationZ._22 := cosAngle;
  MRotationZ._23 := 0;
  MRotationZ._24 := 0;

  MRotationZ._31 := 0;
  MRotationZ._32 := 0;
  MRotationZ._33 := 1;
  MRotationZ._34 := 0;

  MRotationZ._41 := 0;
  MRotationZ._42 := 0;
  MRotationZ._43 := 0;
  MRotationZ._44 := 1;

  Result := MRotationZ;
end;


function Matrix4x4F.RotationArbitraryAxis(x: Single;
                                          y: Single;
                                          z: Single;
                                          degree: Single): Matrix4x4F;
var
  magnitude: Single;
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;
  oneMinusCosAngle: Single;
  MRotationArb: Matrix4x4F;

begin
  // Normalize the vector represented by x, y, and z
  magnitude := D2D1Vec3Length(x,
                              y,
                              z);
  x := x / magnitude;
  y := y / magnitude;
  z := z / magnitude;

  // angleInRadian := degree * (Pi / 180.0); // same as DegToRad(degree)
  angleInRadian := DegToRad(degree);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  oneMinusCosAngle := 1 - cosAngle;

  MRotationArb._11 := (1 + oneMinusCosAngle * (x * x - 1));
  MRotationArb._12 := z * sinAngle + oneMinusCosAngle * x * y;
  MRotationArb._13 := -y * sinAngle + oneMinusCosAngle * x * z;
  MRotationArb._14 := 0;

  MRotationArb._21 := -z * sinAngle + oneMinusCosAngle * y * x;
  MRotationArb._22 := 1 + oneMinusCosAngle * y * y - 1;
  MRotationArb._23 := x * sinAngle + oneMinusCosAngle * y * z;
  MRotationArb._24 := 0;

  MRotationArb._31 := y * sinAngle + oneMinusCosAngle * z * x;
  MRotationArb._32 := -x * sinAngle + oneMinusCosAngle * z * y;
  MRotationArb._33 := 1             + oneMinusCosAngle * (z * z - 1) ;
  MRotationArb._34 := 0;

  MRotationArb._41 := 0;
  MRotationArb._42 := 0;
  MRotationArb._43 := 0;
  MRotationArb._44 := 1;

  Result := MRotationArb;
end;


function Matrix4x4F.SkewX(degreeX: Single): Matrix4x4F;
var
  angleInRadian: Single;
  tanAngle: Single;
  MSkewX: Matrix4x4F;

begin

  // angleInRadian := degreeX * (Pi / 180.0); // same as DegToRad(degreeX)
  angleInRadian := DegToRad(degreeX);
  tanAngle := D2D1Tan(angleInRadian);

  MSkewX._11 := 1;
  MSkewX._12 := 0;
  MSkewX._13 := 0;
  MSkewX._14 := 0;

  MSkewX._21 := tanAngle;
  MSkewX._22 := 1;
  MSkewX._23 := 0;
  MSkewX._24 := 0;

  MSkewX._31 := 0;
  MSkewX._32 := 0;
  MSkewX._33 := 1;
  MSkewX._34 := 0;

  MSkewX._41 := 0;
  MSkewX._42 := 0;
  MSkewX._43 := 0;
  MSkewX._44 := 1;

  Result := MSkewX;
end;


function Matrix4x4F.SkewY(degreeY: Single): Matrix4x4F;
var
  angleInRadian: Single;
  tanAngle: Single;
  MSkewY: Matrix4x4F;

begin

  // angleInRadian := degreeY * (Pi / 180.0); // same as DegToRad(degreeY)
  angleInRadian := DegToRad(degreeY);
  tanAngle := D2D1Tan(angleInRadian);

  MSkewY._11 := 1;
  MSkewY._12 := tanAngle;
  MSkewY._13 := 0;
  MSkewY._14 := 0;

  MSkewY._21 := 0;
  MSkewY._22 := 1;
  MSkewY._23 := 0;
  MSkewY._24 := 0;

  MSkewY._31 := 0;
  MSkewY._32 := 0;
  MSkewY._33 := 1;
  MSkewY._34 := 0;

  MSkewY._41 := 0;
  MSkewY._42 := 0;
  MSkewY._43 := 0;
  MSkewY._44 := 1;

  Result := MSkewY;
end;

function Matrix4x4F.PerspectiveProjection(depth: Single): Matrix4x4F;
var
  proj: Single;
  MProjection: Matrix4x4F;

begin
  proj := 0;

  if (depth > 0) then
    proj := -1 / depth;


  MProjection._11 := 1;
  MProjection._12 := 0;
  MProjection._13 := 0;
  MProjection._14 := 0;

  MProjection._21 := 0;
  MProjection._22 := 1;
  MProjection._23 := 0;
  MProjection._24 := 0;

  MProjection._31 := 0;
  MProjection._32 := 0;
  MProjection._33 := 1;
  MProjection._34 := proj;

  MProjection._41 := 0;
  MProjection._42 := 0;
  MProjection._43 := 0;
  MProjection._44 := 1;

  Result := MProjection;
end;


function Matrix4x4F.ReinterpretBaseType(const pMatrix: D2D1_MATRIX_4X4_F): Matrix4x4F;
begin
  Result := Matrix4x4F(pMatrix);
end;


function Matrix4x4F.IsIdentity(): Boolean;
begin
  Result := ( _11 = 1.0 ) And
            ( _12 = 0.0 ) And
            ( _13 = 0.0 ) And
            ( _14 = 0.0 ) And

            ( _21 = 0.0 ) And
            ( _22 = 1.0 ) And
            ( _23 = 0.0 ) And
            ( _24 = 0.0 ) And

            ( _31 = 0.0 ) And
            ( _32 = 0.0 ) And
            ( _33 = 1.0 ) And
            ( _34 = 0.0 ) And

            ( _41 = 0.0 ) And
            ( _42 = 0.0 ) And
            ( _43 = 0.0 ) And
            ( _44 = 1.0);
end;


function Matrix4x4F.RotationY(degreeY: Single): Matrix4x4F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;
  MRotationY: Matrix4x4F;

begin
  // angleInRadian := degreeY * (Pi / 180.0); // same as DegToRad(degreeY)
  angleInRadian := DegToRad(degreeY);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  MRotationY._11 := cosAngle;
  MRotationY._12 := 0;
  MRotationY._13 := -sinAngle;
  MRotationY._14 := 0;

  MRotationY._21 := 0;
  MRotationY._22 := 1;
  MRotationY._23 := 0;
  MRotationY._24 := 0;

  MRotationY._31 := sinAngle;
  MRotationY._32 := 0;
  MRotationY._33 := cosAngle;
  MRotationY._34 := 0;

  MRotationY._41 := 0;
  MRotationY._42 := 0;
  MRotationY._43 := 0;
  MRotationY._44 := 1;

  Result := MRotationY;
end;


function Matrix4x4f.Determinant(): Single;
var
  minor1: Single;
  minor2: Single;
  minor3: Single;
  minor4: Single;

begin
  minor1 := _41 * (_12 * (_23 * _34 - _33 * _24) - _13 * (_22 * _34 - _24 * _32) + _14 * (_22 * _33 - _23 * _32));
  minor2 := _42 * (_11 * (_21 * _34 - _31 * _24) - _13 * (_21 * _34 - _24 * _31) + _14 * (_21 * _33 - _23 * _31));
  minor3 := _43 * (_11 * (_22 * _34 - _32 * _24) - _12 * (_21 * _34 - _24 * _31) + _14 * (_21 * _32 - _22 * _31));
  minor4 := _44 * (_11 * (_22 * _33 - _32 * _23) - _12 * (_21 * _33 - _23 * _31) + _13 * (_21 * _32 - _22 * _31));

  Result := (minor1 - minor2) + (minor3 - minor4);
end;


procedure Matrix4x4f.SetProduct(var a: Matrix4x4F;
                                var b: Matrix4x4F);
begin
  _11 := a._11 * b._11 + a._12 * b._21 + a._13 * b._31 + a._14 * b._41;
  _12 := a._11 * b._12 + a._12 * b._22 + a._13 * b._32 + a._14 * b._42;
  _13 := a._11 * b._13 + a._12 * b._23 + a._13 * b._33 + a._14 * b._43;
  _14 := a._11 * b._14 + a._12 * b._24 + a._13 * b._34 + a._14 * b._44;

  _21 := a._21 * b._11 + a._22 * b._21 + a._23 * b._31 + a._24 * b._41;
  _22 := a._21 * b._12 + a._22 * b._22 + a._23 * b._32 + a._24 * b._42;
  _23 := a._21 * b._13 + a._22 * b._23 + a._23 * b._33 + a._24 * b._43;
  _24 := a._21 * b._14 + a._22 * b._24 + a._23 * b._34 + a._24 * b._44;

  _31 := a._31 * b._11 + a._32 * b._21 + a._33 * b._31 + a._34 * b._41;
  _32 := a._31 * b._12 + a._32 * b._22 + a._33 * b._32 + a._34 * b._42;
  _33 := a._31 * b._13 + a._32 * b._23 + a._33 * b._33 + a._34 * b._43;
  _34 := a._31 * b._14 + a._32 * b._24 + a._33 * b._34 + a._34 * b._44;

  _41 := a._41 * b._11 + a._42 * b._21 + a._43 * b._31 + a._44 * b._41;
  _42 := a._41 * b._12 + a._42 * b._22 + a._43 * b._32 + a._44 * b._42;
  _43 := a._41 * b._13 + a._42 * b._23 + a._43 * b._33 + a._44 * b._43;
  _44 := a._41 * b._14 + a._42 * b._24 + a._43 * b._34 + a._44 * b._44;
end;


function Matrix4x4f.product(matrix: Matrix4x4F): Matrix4x4F;
begin
  result.SetProduct(Self,
                    matrix);
end;


constructor Matrix5x4f.Create(m11: Single = 1;
                              m12: Single = 0;
                              m13: Single = 0;
                              m14: Single = 0;

                              m21: Single = 0;
                              m22: Single = 1;
                              m23: Single = 0;
                              m24: Single = 0;

                              m31: Single = 0;
                              m32: Single = 0;
                              m33: Single = 1;
                              m34: Single = 0;

                              m41: Single = 0;
                              m42: Single = 0;
                              m43: Single = 0;
                              m44: Single = 1;

                              m51: Single = 0;
                              m52: Single = 0;
                              m53: Single = 0;
                              m54: Single = 0);
begin
  inherited;
  _11 := m11;
  _12 := m12;
  _13 := m13;
  _14 := m14;

  _21 := m21;
  _22 := m22;
  _23 := m23;
  _24 := m24;

  _31 := m31;
  _32 := m32;
  _33 := m33;
  _34 := m34;

  _41 := m41;
  _42 := m42;
  _43 := m43;
  _44 := m44;

  _51 := m51;
  _52 := m52;
  _53 := m53;
  _54 := m54;
end;



function ConvertColorSpace(sourceColorSpace: D2D1_COLOR_SPACE;
                           destinationColorSpace: D2D1_COLOR_SPACE;
                           color: D2D1_COLOR_F): D2D1_COLOR_F;
begin
  Result := D2D1ConvertColorSpace(sourceColorSpace,
                                  destinationColorSpace,
                                  color);
end;


function DrawingStateDescription1(transform: D2D1_MATRIX_3X2_F;
                                  antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                  textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                  tag1: D2D1_TAG = 0;
                                  tag2: D2D1_TAG = 0;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
begin
  Result.antialiasMode := antialiasMode;
  Result.textAntialiasMode := textAntialiasMode;
  Result.tag1 := tag1;
  Result.tag2 := tag2;
  Result.transform := transform;
  Result.primitiveBlend := primitiveBlend;
  Result.unitMode := unitMode;
end;


function DrawingStateDescription1(antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                  textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                  tag1: D2D1_TAG = 0;
                                  tag2: D2D1_TAG = 0;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
begin
  Result.antialiasMode := antialiasMode;
  Result.textAntialiasMode := textAntialiasMode;
  Result.tag1 := tag1;
  Result.tag2 := tag2;
  Result.transform := IdentityMatrix();
  Result.primitiveBlend := primitiveBlend;
  Result.unitMode := unitMode;
end;


function DrawingStateDescription1(desc: D2D1_DRAWING_STATE_DESCRIPTION;
                                  primitiveBlend: D2D1_PRIMITIVE_BLEND = D2D1_PRIMITIVE_BLEND_SOURCE_OVER;
                                  unitMode: D2D1_UNIT_MODE = D2D1_UNIT_MODE_DIPS): D2D1_DRAWING_STATE_DESCRIPTION1; overload;
begin
  Result.antialiasMode := desc.antialiasMode;
  Result.textAntialiasMode := desc.textAntialiasMode;
  Result.tag1 := desc.tag1;
  Result.tag2 := desc.tag2;
  Result.transform := desc.transform;
  Result.primitiveBlend := primitiveBlend;
  Result.unitMode := unitMode;
end;


function BitmapProperties1(_pixelFormat: D2D1_PIXEL_FORMAT;
                           bitmapOptions: D2D1_BITMAP_OPTIONS = D2D1_BITMAP_OPTIONS_NONE;
                           dpiX: Single = 96.0;
                           dpiY: Single = 96.0;
                           colorContext: ID2D1ColorContext = Nil): D2D1_BITMAP_PROPERTIES1; overload;
begin
  Result.bitmapOptions := bitmapOptions;
  Result.colorContext := colorContext;
  Result.dpiX := dpiX;
  Result.dpiY := dpiY;
  Result._pixelFormat := _pixelFormat;
end;


function BitmapProperties1(bitmapOptions: D2D1_BITMAP_OPTIONS = D2D1_BITMAP_OPTIONS_NONE;
                           dpiX: Single = 96.0;
                           dpiY: Single = 96.0;
                           colorContext: ID2D1ColorContext = Nil): D2D1_BITMAP_PROPERTIES1; overload;
begin
  Result.bitmapOptions := bitmapOptions;
  Result.colorContext := colorContext;
  Result.dpiX := dpiX;
  Result.dpiY := dpiY;
  Result._pixelFormat := PixelFormat();
end;


function LayerParameters1(contentBounds: D2D1_RECT_F;
                          maskTransform: D2D1_MATRIX_3X2_F;
                          geometricMask: ID2D1Geometry = Nil;
                          maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                          opacity: Single = 1.0;
                          opacityBrush: ID2D1Brush = Nil;
                          layerOptions: D2D1_LAYER_OPTIONS1 = D2D1_LAYER_OPTIONS1_NONE): D2D1_LAYER_PARAMETERS1; overload;
begin
  Result.contentBounds := contentBounds;
  Result.geometricMask := geometricMask;
  Result.maskAntialiasMode := maskAntialiasMode;
  Result.maskTransform := maskTransform;
  Result.opacity := opacity;
  Result.opacityBrush := opacityBrush;
  Result.layerOptions := layerOptions;
end;


function LayerParameters1(geometricMask: ID2D1Geometry = Nil;
                          maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                          opacity: Single = 1.0;
                          opacityBrush: ID2D1Brush = Nil;
                          layerOptions: D2D1_LAYER_OPTIONS1 = D2D1_LAYER_OPTIONS1_NONE): D2D1_LAYER_PARAMETERS1; overload;
begin
  Result.contentBounds := InfiniteRect();
  Result.geometricMask := geometricMask;
  Result.maskAntialiasMode := maskAntialiasMode;
  Result.maskTransform := IdentityMatrix();
  Result.opacity := opacity;
  Result.opacityBrush := opacityBrush;
  Result.layerOptions := layerOptions;
end;


function StrokeStyleProperties1(startCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                endCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                dashCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                lineJoin: D2D1_LINE_JOIN = D2D1_LINE_JOIN_MITER;
                                miterLimit: single = 10.0;
                                dashStyle: D2D1_DASH_STYLE = D2D1_DASH_STYLE_SOLID;
                                dashOffset: Single = 0.0;
                                transformType: D2D1_STROKE_TRANSFORM_TYPE = D2D1_STROKE_TRANSFORM_TYPE_NORMAL): D2D1_STROKE_STYLE_PROPERTIES1;
begin
  Result.startCap := startCap;
  Result.endCap := endCap;
  Result.dashCap := dashCap;
  Result.lineJoin := lineJoin;
  Result.miterLimit := miterLimit;
  Result.dashStyle := dashStyle;
  Result.dashOffset := dashOffset;
  Result.transformType := transformType;
end;


function ImageBrushProperties(sourceRectangle: D2D1_RECT_F;
                              extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                              extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                              interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR): D2D1_IMAGE_BRUSH_PROPERTIES;
begin
  Result.extendModeX := extendModeX;
  Result.extendModeY := extendModeY;
  Result.interpolationMode := interpolationMode;
  Result.sourceRectangle := sourceRectangle;
end;


function BitmapBrushProperties1(extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR): D2D1_BITMAP_BRUSH_PROPERTIES1;
begin
  Result.extendModeX := extendModeX;
  Result.extendModeY := extendModeY;
  Result.interpolationMode := interpolationMode;
end;


function PrintControlProperties(fontSubsetMode: D2D1_PRINT_FONT_SUBSET_MODE = D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT;
                                rasterDpi: Single = 150.0;
                                colorSpace: D2D1_COLOR_SPACE = D2D1_COLOR_SPACE_SRGB): D2D1_PRINT_CONTROL_PROPERTIES;
begin
  Result.fontSubset := fontSubsetMode;
  Result.rasterDPI := rasterDpi;
  Result.colorSpace := colorSpace;
end;


function RenderingControls(bufferPrecision: D2D1_BUFFER_PRECISION;
                           tileSize: D2D1_SIZE_U): D2D1_RENDERING_CONTROLS;
begin
  Result.bufferPrecision := bufferPrecision;
  Result.tileSize := tileSize;
end;


function EffectInputDescription(effect: ID2D1Effect;
                                inputIndex: UINT32;
                                inputRectangle: D2D1_RECT_F): D2D1_EFFECT_INPUT_DESCRIPTION;
begin
  Result.effect := effect;
  Result.inputIndex := inputIndex;
  Result.inputRectangle := inputRectangle;
end;


function CreationProperties(threadingMode: D2D1_THREADING_MODE;
                            debugLevel: D2D1_DEBUG_LEVEL;
                            options: D2D1_DEVICE_CONTEXT_OPTIONS): D2D1_CREATION_PROPERTIES;
begin
  Result.threadingMode := threadingMode;
  Result.debugLevel := debugLevel;
  Result.options := options;
end;


function Vector2F(x: Single = 0.0;
                  y: Single = 0.0): D2D1_VECTOR_2F;
begin
  Result.x := x;
  Result.y := y;
end;


function Vector3F(x: Single = 0.0;
                  y: Single = 0.0;
                  z: Single = 0.0): D2D1_VECTOR_3F;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;


function Vector4F(x: Single = 0.0;
                  y: Single = 0.0;
                  z: Single = 0.0;
                  w: Single = 0.0): D2D1_VECTOR_4F;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;


function Point2L(x: INT32 = 0;
                 y: INT32 = 0): D2D1_POINT_2L;
begin
  Result.x := x;
  Result.y := y;
end;


function RectL(left: INT32 = 0;
               top: INT32 = 0;
               right: INT32 = 0;
               bottom: INT32 = 0): D2D1_RECT_L;
begin
  Result.left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;



function SetDpiCompensatedEffectInput(deviceContext: ID2D1DeviceContext;
                                      effect: ID2D1Effect;
                                      inputIndex: UINT32;
                                      inputBitmap: ID2D1Bitmap = nil;
                                      interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR;
                                      borderMode: D2D1_BORDER_MODE = D2D1_BORDER_MODE_HARD): HRESULT;
var
  hr: HResult;
  dpiCompensationEffect: ID2D1Effect;
  bitmapDpi: D2D1_POINT_2F;

procedure SetInputEffect(index: UINT32;
                         inputEffect: ID2D1Effect = nil;
                         invalidate: BOOL = TRUE);
var
  output: ID2D1Image;

begin

  if Assigned(inputEffect) then
    inputEffect.GetOutput(output);

  effect.SetInput(index,
                  output,
                  invalidate);
end;

begin

  if not Assigned(inputBitmap) then
    begin
      effect.SetInput(inputIndex,
                      Nil);
      Result := E_POINTER;
      Exit;
    end;

  hr := deviceContext.CreateEffect(CLSID_D2D1DpiCompensation,
                                   dpiCompensationEffect);

  if SUCCEEDED(hr) then
    begin
      dpiCompensationEffect.SetInput(0,
                                     inputBitmap);
      inputBitmap.GetDpi(bitmapDpi.x,
                         bitmapDpi.y);
      hr := dpiCompensationEffect.SetValue(Ord(D2D1_DPICOMPENSATION_PROP_INPUT_DPI),
                                           D2D1_PROPERTY_TYPE_UNKNOWN,
                                           @bitmapDpi,
                                           SizeOf(bitmapDpi));
    end;

  if SUCCEEDED(hr) then
    begin
      hr := dpiCompensationEffect.SetValue(Ord(D2D1_DPICOMPENSATION_PROP_INTERPOLATION_MODE),
                                           D2D1_PROPERTY_TYPE_UNKNOWN,
                                           @interpolationMode,
                                           SizeOf(interpolationMode));
    end;

  if SUCCEEDED(hr) then
    begin
      hr := dpiCompensationEffect.SetValue(Ord(D2D1_DPICOMPENSATION_PROP_BORDER_MODE),
                                           D2D1_PROPERTY_TYPE_UNKNOWN,
                                           @borderMode,
                                           SizeOf(borderMode));
    end;

  if SUCCEEDED(hr) then
    SetInputEffect(inputIndex,
                   dpiCompensationEffect);
  Result := hr;
end;



  // Implement Additional functions here.

end.
