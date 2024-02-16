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
// Revision Version: 3.1.6
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Here we use Objects as records in place for records to make inheritance possible.
//          Note: Objects are placed on the heap instead of stack.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
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
  Matrix4x3F = record helper for D2D_MATRIX_4X3_F
  public
    procedure Init();
  end;


  Matrix4x4F = record helper for D2D_MATRIX_4X4_F
  public
    procedure Init(); inline;

    function Translation(x: Single;
                         y: Single;
                         z: Single): D2D_MATRIX_4X4_F; inline;

    function Scale(x: Single;
                       y: Single;
                       z: Single): D2D_MATRIX_4X4_F; inline;

    function RotationX(degreeX: Single): D2D_MATRIX_4X4_F; inline;

    function RotationY(degreeY: Single): D2D_MATRIX_4X4_F; inline;

    function RotationZ(degreeZ: Single): D2D_MATRIX_4X4_F; inline;

    //
    // 3D Rotation matrix for an arbitrary axis specified by x, y and z
    //

    function RotationArbitraryAxis(x: Single;
                                   y: Single;
                                   z: Single;
                                   degree: Single): D2D_MATRIX_4X4_F; inline;

    function SkewX(degreeX: Single): D2D_MATRIX_4X4_F; inline;

    function SkewY(degreeY: Single): D2D_MATRIX_4X4_F; inline;

    function PerspectiveProjection(depth: Single): D2D_MATRIX_4X4_F; inline;


    //
    // Functions for convertion from the base D2D1_MATRIX_4X4_f to
    // this type without making a copy
    //

    function ReinterpretBaseType(const pMatrix: D2D1_MATRIX_4X4_F): D2D_MATRIX_4X4_F;

    function Determinant(pUnion: Word): Single; inline;

    function IsIdentity(): Boolean; inline;

    procedure SetProduct(var a: D2D_MATRIX_4X4_F;
                         var b: D2D_MATRIX_4X4_F); inline;

    function product(matrix: D2D_MATRIX_4X4_F): D2D_MATRIX_4X4_F; inline;

  end;

  Matrix5x4F = record helper for D2D_MATRIX_5X4_F
  public
    procedure Init(); inline;

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


procedure Matrix4x3F.Init();
begin
  struct._11 := 1;
  struct._12 := 0;
  struct._13 := 0;

  struct._21 := 0;
  struct._22 := 1;
  struct._23 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;

end;


procedure Matrix4x4f.Init();
begin
  struct._11 := 1;
  struct._12 := 0;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := 0;
  struct._22 := 1;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

end;


function Matrix4x4f.Translation(x: Single;
                                y: Single;
                                z: Single): D2D_MATRIX_4X4_F;
begin
  struct._11 := 1.0;
  struct._12 := 0.0;
  struct._13 := 0.0;
  struct._14 := 0.0;

  struct._21 := 0.0;
  struct._22 := 1.0;
  struct._23 := 0.0;
  struct._24 := 0.0;

  struct._31 := 0.0;
  struct._32 := 0.0;
  struct._33 := 1.0;
  struct._34 := 0.0;

  struct._41 := x;
  struct._42 := y;
  struct._43 := z;
  struct._44 := 1.0;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4f.Scale(x: Single;
                         y: Single;
                         z: Single): D2D_MATRIX_4X4_F;
begin
  struct._11 := x;
  struct._12 := 0.0;
  struct._13 := 0.0;
  struct._14 := 0.0;

  struct._21 := 0.0;
  struct._22 := y;
  struct._23 := 0.0;
  struct._24 := 0.0;

  struct._31 := 0.0;
  struct._32 := 0.0;
  struct._33 := z;
  struct._34 := 0.0;

  struct._41 := 0.0;
  struct._42 := 0.0;
  struct._43 := 0.0;
  struct._44 := 1.0;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4f.RotationX(degreeX: Single): D2D_MATRIX_4X4_F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;

begin
  // angleInRadian := degreeX * (Pi / 180.0);  // same as DegToRad(degreeX)
  angleInRadian := DegToRad(degreeX);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  struct._11 := 1;
  struct._12 := 0;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := 0;
  struct._22 := cosAngle;
  struct._23 := sinAngle;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := -sinAngle;
  struct._33 := cosAngle;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4F.RotationZ(degreeZ: Single): D2D_MATRIX_4X4_F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;

begin
  // angleInRadian := degreeZ * (Pi / 180.0); same as DegToRad(degreeZ)
  angleInRadian := DegToRad(degreeZ);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  struct._11 := cosAngle;
  struct._12 := sinAngle;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := -sinAngle;
  struct._22 := cosAngle;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4F.RotationArbitraryAxis(x: Single;
                                          y: Single;
                                          z: Single;
                                          degree: Single): D2D_MATRIX_4X4_F;
var
  magnitude: Single;
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;
  oneMinusCosAngle: Single;

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

  struct._11 := (1 + oneMinusCosAngle * (x * x - 1));
  struct._12 := z * sinAngle + oneMinusCosAngle * x * y;
  struct._13 := -y * sinAngle + oneMinusCosAngle * x * z;
  struct._14 := 0;

  struct._21 := -z * sinAngle + oneMinusCosAngle * y * x;
  struct._22 := 1 + oneMinusCosAngle * y * y - 1;
  struct._23 := x * sinAngle + oneMinusCosAngle * y * z;
  struct._24 := 0;

  struct._31 := y * sinAngle + oneMinusCosAngle * z * x;
  struct._32 := -x * sinAngle + oneMinusCosAngle * z * y;
  struct._33 := 1             + oneMinusCosAngle * (z * z - 1) ;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;


  Result := Self;
end;


function Matrix4x4F.SkewX(degreeX: Single): D2D_MATRIX_4X4_F;
var
  angleInRadian: Single;
  tanAngle: Single;

begin

  // angleInRadian := degreeX * (Pi / 180.0); // same as DegToRad(degreeX)
  angleInRadian := DegToRad(degreeX);
  tanAngle := D2D1Tan(angleInRadian);

  struct._11 := 1;
  struct._12 := 0;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := tanAngle;
  struct._22 := 1;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4F.SkewY(degreeY: Single): D2D_MATRIX_4X4_F;
var
  angleInRadian: Single;
  tanAngle: Single;

begin

  // angleInRadian := degreeY * (Pi / 180.0); // same as DegToRad(degreeY)
  angleInRadian := DegToRad(degreeY);
  tanAngle := D2D1Tan(angleInRadian);

  struct._11 := 1;
  struct._12 := tanAngle;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := 0;
  struct._22 := 1;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4F.PerspectiveProjection(depth: Single): D2D_MATRIX_4X4_F;
var
  proj: Single;

begin
  proj := 0;

  if (depth > 0) then
    proj := -1 / depth;

  struct._11 := 1;
  struct._12 := 0;
  struct._13 := 0;
  struct._14 := 0;

  struct._21 := 0;
  struct._22 := 1;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := 0;
  struct._32 := 0;
  struct._33 := 1;
  struct._34 := proj;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  Result := Self;
end;


function Matrix4x4F.ReinterpretBaseType(const pMatrix: D2D1_MATRIX_4X4_F): D2D_MATRIX_4X4_F;
begin
  Result := D2D_MATRIX_4X4_F(pMatrix);
end;


function Matrix4x4F.IsIdentity(): Boolean;
begin
  Result := False;

  if ( struct._11 = 1.0 ) and
            ( struct._12 = 0.0 ) and
            ( struct._13 = 0.0 ) and
            ( struct._14 = 0.0 ) and

            ( struct._21 = 0.0 ) and
            ( struct._22 = 1.0 ) and
            ( struct._23 = 0.0 ) and
            ( struct._24 = 0.0 ) and

            ( struct._31 = 0.0 ) and
            ( struct._32 = 0.0 ) and
            ( struct._33 = 1.0 ) and
            ( struct._34 = 0.0 ) and

            ( struct._41 = 0.0 ) and
            ( struct._42 = 0.0 ) and
            ( struct._43 = 0.0 ) and
            ( struct._44 = 1.0) then

            Result := True
  else if

            (m[0,0] = 1.0) and
            (m[0,1] = 0.0) and
            (m[0,2] = 0.0) and
            (m[0,3] = 0.0) and

            (m[1,0] = 0.0) and
            (m[1,1] = 1.0) and
            (m[1,2] = 0.0) and
            (m[1,3] = 0.0) and

            (m[2,0] = 0.0) and
            (m[2,1] = 0.0) and
            (m[2,2] = 1.0) and
            (m[2,3] = 0.0) and

            (m[3,0] = 0.0) and
            (m[3,1] = 0.0) and
            (m[3,2] = 0.0) and
            (m[3,3] = 1.0) then
            Result := True;

end;


function Matrix4x4F.RotationY(degreeY: Single): D2D_MATRIX_4X4_F;
var
  angleInRadian: Single;
  sinAngle: Single;
  cosAngle: Single;

begin
  // angleInRadian := degreeY * (Pi / 180.0); // same as DegToRad(degreeY)
  angleInRadian := DegToRad(degreeY);

  sinAngle := 0.0;
  cosAngle := 0.0;
  D2D1SinCos(angleInRadian,
             sinAngle,
             cosAngle);

  struct._11 := cosAngle;
  struct._12 := 0;
  struct._13 := -sinAngle;
  struct._14 := 0;

  struct._21 := 0;
  struct._22 := 1;
  struct._23 := 0;
  struct._24 := 0;

  struct._31 := sinAngle;
  struct._32 := 0;
  struct._33 := cosAngle;
  struct._34 := 0;

  struct._41 := 0;
  struct._42 := 0;
  struct._43 := 0;
  struct._44 := 1;


  m[0,0] := cosAngle;
  m[0,1] := 0.0;
  m[0,2] := -sinAngle;
  m[0,3] := 0.0;

  m[1,0] := 0.0;
  m[1,1] := 1.0;
  m[1,2] := 0.0;
  m[1,3] := 0.0;

  m[2,0] := sinAngle;
  m[2,1] := 0.0;
  m[2,2] := cosAngle;
  m[2,3] := 0.0;

  m[3,0] := 0.0;
  m[3,1] := 0.0;
  m[3,2] := 0.0;
  m[3,3] := 1.0;

  Result := Self;
end;


function Matrix4x4f.Determinant(pUnion: Word): Single;
var
  minor1: Single;
  minor2: Single;
  minor3: Single;
  minor4: Single;

begin
  if (pUnion = 0) then
    begin
      minor1 := struct._41 * (struct._12 * (struct._23 * struct._34 - struct._33 * struct._24) - struct._13 * (struct._22 * struct._34 - struct._24 * struct._32) + struct._14 * (struct._22 * struct._33 - struct._23 * struct._32));
      minor2 := struct._42 * (struct._11 * (struct._21 * struct._34 - struct._31 * struct._24) - struct._13 * (struct._21 * struct._34 - struct._24 * struct._31) + struct._14 * (struct._21 * struct._33 - struct._23 * struct._31));
      minor3 := struct._43 * (struct._11 * (struct._22 * struct._34 - struct._32 * struct._24) - struct._12 * (struct._21 * struct._34 - struct._24 * struct._31) + struct._14 * (struct._21 * struct._32 - struct._22 * struct._31));
      minor4 := struct._44 * (struct._11 * (struct._22 * struct._33 - struct._32 * struct._23) - struct._12 * (struct._21 * struct._33 - struct._23 * struct._31) + struct._13 * (struct._21 * struct._32 - struct._22 * struct._31));
    end
  else
    begin
      minor1 := m[3,0] * (m[0,1] * (m[1,2] * m[2,3] - m[2,2] * m[1,3]) - m[0,2] * (m[1,1] * m[2,3] - m[1,3] * m[2,1]) + m[0,3] * (m[1,1] * m[2,2] - m[1,2] * m[2,1]));
      minor2 := m[3,1] * (m[0,1] * (m[1,0] * m[2,3] - m[2,0] * m[1,3]) - m[0,2] * (m[1,0] * m[2,3] - m[1,3] * m[2,0]) + m[0,3] * (m[1,0] * m[2,2] - m[1,2] * m[2,0]));
      minor3 := m[3,2] * (m[0,0] * (m[1,1] * m[2,3] - m[2,1] * m[1,3]) - m[0,1] * (m[1,0] * m[2,3] - m[1,3] * m[2,0]) + m[0,3] * (m[1,0] * m[2,1] - m[1,1] * m[2,0]));
      minor4 := m[3,3] * (m[0,0] * (m[1,1] * m[2,2] - m[2,1] * m[1,2]) - m[0,1] * (m[1,0] * m[2,2] - m[1,2] * m[2,0]) + m[0,2] * (m[1,0] * m[2,1] - m[1,1] * m[2,0]));
    end;

  Result := (minor1 - minor2) + (minor3 - minor4);

end;


procedure Matrix4x4f.SetProduct(var a: D2D_MATRIX_4X4_F;
                                var b: D2D_MATRIX_4X4_F);
begin
  struct._11 := a.struct._11 * b.struct._11 + a.struct._12 * b.struct._21 + a.struct._13 * b.struct._31 + a.struct._14 * b.struct._41;
  struct._12 := a.struct._11 * b.struct._12 + a.struct._12 * b.struct._22 + a.struct._13 * b.struct._32 + a.struct._14 * b.struct._42;
  struct._13 := a.struct._11 * b.struct._13 + a.struct._12 * b.struct._23 + a.struct._13 * b.struct._33 + a.struct._14 * b.struct._43;
  struct._14 := a.struct._11 * b.struct._14 + a.struct._12 * b.struct._24 + a.struct._13 * b.struct._34 + a.struct._14 * b.struct._44;

  struct._21 := a.struct._21 * b.struct._11 + a.struct._22 * b.struct._21 + a.struct._23 * b.struct._31 + a.struct._24 * b.struct._41;
  struct._22 := a.struct._21 * b.struct._12 + a.struct._22 * b.struct._22 + a.struct._23 * b.struct._32 + a.struct._24 * b.struct._42;
  struct._23 := a.struct._21 * b.struct._13 + a.struct._22 * b.struct._23 + a.struct._23 * b.struct._33 + a.struct._24 * b.struct._43;
  struct._24 := a.struct._21 * b.struct._14 + a.struct._22 * b.struct._24 + a.struct._23 * b.struct._34 + a.struct._24 * b.struct._44;

  struct._31 := a.struct._31 * b.struct._11 + a.struct._32 * b.struct._21 + a.struct._33 * b.struct._31 + a.struct._34 * b.struct._41;
  struct._32 := a.struct._31 * b.struct._12 + a.struct._32 * b.struct._22 + a.struct._33 * b.struct._32 + a.struct._34 * b.struct._42;
  struct._33 := a.struct._31 * b.struct._13 + a.struct._32 * b.struct._23 + a.struct._33 * b.struct._33 + a.struct._34 * b.struct._43;
  struct._34 := a.struct._31 * b.struct._14 + a.struct._32 * b.struct._24 + a.struct._33 * b.struct._34 + a.struct._34 * b.struct._44;

  struct._41 := a.struct._41 * b.struct._11 + a.struct._42 * b.struct._21 + a.struct._43 * b.struct._31 + a.struct._44 * b.struct._41;
  struct._42 := a.struct._41 * b.struct._12 + a.struct._42 * b.struct._22 + a.struct._43 * b.struct._32 + a.struct._44 * b.struct._42;
  struct._43 := a.struct._41 * b.struct._13 + a.struct._42 * b.struct._23 + a.struct._43 * b.struct._33 + a.struct._44 * b.struct._43;
  struct._44 := a.struct._41 * b.struct._14 + a.struct._42 * b.struct._24 + a.struct._43 * b.struct._34 + a.struct._44 * b.struct._44;


  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

end;


function Matrix4x4f.product(matrix: D2D_MATRIX_4X4_F): D2D_MATRIX_4X4_F;
begin
  result.SetProduct(Self,
                    matrix);
end;


procedure Matrix5x4f.Init();
begin
  struct._11 := 1.0;
  struct._12 := 0.0;
  struct._13 := 0.0;
  struct._14 := 0.0;

  struct._21 := 0.0;
  struct._22 := 1.0;
  struct._23 := 0.0;
  struct._24 := 0.0;

  struct._31 := 0.0;
  struct._32 := 0.0;
  struct._33 := 1.0;
  struct._34 := 0.0;

  struct._41 := 0.0;
  struct._42 := 0.0;
  struct._43 := 0.0;
  struct._44 := 1.0;

  struct._51 := 0.0;
  struct._52 := 0.0;
  struct._53 := 0.0;
  struct._54 := 0.0;

  m[0,0] := struct._11;
  m[0,1] := struct._12;
  m[0,2] := struct._13;
  m[0,3] := struct._14;

  m[1,0] := struct._21;
  m[1,1] := struct._22;
  m[1,2] := struct._23;
  m[1,3] := struct._24;

  m[2,0] := struct._31;
  m[2,1] := struct._32;
  m[2,2] := struct._33;
  m[2,3] := struct._34;

  m[3,0] := struct._41;
  m[3,1] := struct._42;
  m[3,2] := struct._43;
  m[3,3] := struct._44;

  m[4,0] := struct._51;
  m[4,1] := struct._52;
  m[4,2] := struct._53;
  m[4,3] := struct._54;

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
