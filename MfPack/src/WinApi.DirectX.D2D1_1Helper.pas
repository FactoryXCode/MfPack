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
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.D2D1helper,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


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
