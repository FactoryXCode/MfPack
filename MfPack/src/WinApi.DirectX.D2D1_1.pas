// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2d1_1.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 4.1.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Tilo G�ldner (TiloGueldner),
//                 (wsandor)
//                 Frank Siek (Cypheros-de)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
// 09/09/2025 Frank Siek          Fixed wrong parameter definition of procedure DrawBitmap.
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: d2d1_1.h
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
unit WinApi.DirectX.D2D1_1;

  {$HPPEMIT '#include "d2d1_1.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinCodec,{or WinApi.DirectX.WinCodec,}
  WinApi.ActiveX,
  {WinApi.DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DocumentTarget;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const
  D2D1_INVALID_PROPERTY_INDEX = $FFFFFFFF; // max value for an unsigned int;

type
  PUINT32 = ^UINT32;

  // Function pointer that sets a property on an effect.
  // Delphi Note: Moved from D2D1EffectAuthor.pas to prevent circular references
  PD2D1_PROPERTY_SET_FUNCTION = function({in} effect: IUnknown;
                                         {in} data: PByte;
                                         dataSize: UINT32): HRESULT; stdcall;
  {$EXTERNALSYM PD2D1_PROPERTY_SET_FUNCTION}


  // Function pointer that gets a property from an effect.
  // Delphi Note: Moved from D2D1EffectAuthor.pas to prevent circular references
  PD2D1_PROPERTY_GET_FUNCTION = function({in} effect: IUnknown;
                                         data: PByte;
                                         dataSize: UINT32;
                                         actualSize: PUINT32): HRESULT; stdcall;
  {$EXTERNALSYM PD2D1_PROPERTY_GET_FUNCTION}

type
  // Specifies how the bitmap can be used.
  PD2D1_BITMAP_OPTIONS = ^D2D1_BITMAP_OPTIONS;
  D2D1_BITMAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_BITMAP_OPTIONS}
const
  // The bitmap is created with default properties.
  D2D1_BITMAP_OPTIONS_NONE           = D2D1_BITMAP_OPTIONS($00000000);
  // The bitmap can be specified as a target in ID2D1DeviceContext.SetTarget
  D2D1_BITMAP_OPTIONS_TARGET         = D2D1_BITMAP_OPTIONS($00000001);
  // The bitmap cannot be used as an input to DrawBitmap, DrawImage, in a bitmap
  // brush or as an input to an effect.
  D2D1_BITMAP_OPTIONS_CANNOT_DRAW    = D2D1_BITMAP_OPTIONS($00000002);
  // The bitmap can be read from the CPU.
  D2D1_BITMAP_OPTIONS_CPU_READ       = D2D1_BITMAP_OPTIONS($00000004);
  // The bitmap works with the ID2D1GdiInteropRenderTarget.GetDC API.
  D2D1_BITMAP_OPTIONS_GDI_COMPATIBLE = D2D1_BITMAP_OPTIONS($00000008);
  // D2D1_BITMAP_OPTIONS_FORCE_DWORD = FORCEDWORD;


type
  // Specifies how the layer contents should be prepared.
  PD2D1_LAYER_OPTIONS1 = ^D2D1_LAYER_OPTIONS1;
  D2D1_LAYER_OPTIONS1 = Dword;
  {$EXTERNALSYM D2D1_LAYER_OPTIONS1}
const
  D2D1_LAYER_OPTIONS1_NONE                       = D2D1_LAYER_OPTIONS1(0);
  D2D1_LAYER_OPTIONS1_INITIALIZE_FROM_BACKGROUND = D2D1_LAYER_OPTIONS1(1);
  D2D1_LAYER_OPTIONS1_IGNORE_ALPHA               = D2D1_LAYER_OPTIONS1(2);
  //D2D1_LAYER_OPTIONS1_FORCE_DWORD                = FORCEDWORD;


type
  // This enum defines the valid property types that can be used in an effect property
  // interface.
  PD2D1_PROPERTY_TYPE = ^D2D1_PROPERTY_TYPE;
  D2D1_PROPERTY_TYPE = DWord;
  {$EXTERNALSYM D2D1_PROPERTY_TYPE}
const
  D2D1_PROPERTY_TYPE_UNKNOWN       = D2D1_PROPERTY_TYPE(0);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UNKNOWN}
  D2D1_PROPERTY_TYPE_STRING        = D2D1_PROPERTY_TYPE(1);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_STRING}
  D2D1_PROPERTY_TYPE_BOOL          = D2D1_PROPERTY_TYPE(2);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BOOL}
  D2D1_PROPERTY_TYPE_UINT32        = D2D1_PROPERTY_TYPE(3);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UINT32}
  D2D1_PROPERTY_TYPE_INT32         = D2D1_PROPERTY_TYPE(4);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_INT32}
  D2D1_PROPERTY_TYPE_FLOAT         = D2D1_PROPERTY_TYPE(5);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_FLOAT}
  D2D1_PROPERTY_TYPE_VECTOR2       = D2D1_PROPERTY_TYPE(6);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR2}
  D2D1_PROPERTY_TYPE_VECTOR3       = D2D1_PROPERTY_TYPE(7);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR3}
  D2D1_PROPERTY_TYPE_VECTOR4       = D2D1_PROPERTY_TYPE(8);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR4}
  D2D1_PROPERTY_TYPE_BLOB          = D2D1_PROPERTY_TYPE(9);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BLOB}
  D2D1_PROPERTY_TYPE_IUNKNOWN      = D2D1_PROPERTY_TYPE(10);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_IUNKNOWN}
  D2D1_PROPERTY_TYPE_ENUM          = D2D1_PROPERTY_TYPE(11);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ENUM}
  D2D1_PROPERTY_TYPE_ARRAY         = D2D1_PROPERTY_TYPE(12);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ARRAY}
  D2D1_PROPERTY_TYPE_CLSID         = D2D1_PROPERTY_TYPE(13);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_CLSID}
  D2D1_PROPERTY_TYPE_MATRIX_3X2    = D2D1_PROPERTY_TYPE(14);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_3X2}
  D2D1_PROPERTY_TYPE_MATRIX_4X3    = D2D1_PROPERTY_TYPE(15);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X3}
  D2D1_PROPERTY_TYPE_MATRIX_4X4    = D2D1_PROPERTY_TYPE(16);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X4}
  D2D1_PROPERTY_TYPE_MATRIX_5X4    = D2D1_PROPERTY_TYPE(17);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_5X4}
  D2D1_PROPERTY_TYPE_COLOR_CONTEXT = D2D1_PROPERTY_TYPE(18);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_COLOR_CONTEXT}
  //D2D1_PROPERTY_TYPE_FORCE_DWORD   = FORCEDWORD;

type
  // This defines the list of system properties present on the root effect property
  // interface.
  PD2D1_PROPERTY = ^D2D1_PROPERTY;
  D2D1_PROPERTY = DWord;
  {$EXTERNALSYM D2D1_PROPERTY}
const
  D2D1_PROPERTY_CLSID       = D2D1_PROPERTY($80000000);
  {$EXTERNALSYM D2D1_PROPERTY_CLSID}
  D2D1_PROPERTY_DISPLAYNAME = D2D1_PROPERTY($80000001);
  {$EXTERNALSYM D2D1_PROPERTY_DISPLAYNAME}
  D2D1_PROPERTY_AUTHOR      = D2D1_PROPERTY($80000002);
  {$EXTERNALSYM D2D1_PROPERTY_AUTHOR}
  D2D1_PROPERTY_CATEGORY    = D2D1_PROPERTY($80000003);
  {$EXTERNALSYM D2D1_PROPERTY_CATEGORY}
  D2D1_PROPERTY_DESCRIPTION = D2D1_PROPERTY($80000004);
  {$EXTERNALSYM D2D1_PROPERTY_DESCRIPTION}
  D2D1_PROPERTY_INPUTS      = D2D1_PROPERTY($80000005);
  {$EXTERNALSYM D2D1_PROPERTY_INPUTS}
  D2D1_PROPERTY_CACHED      = D2D1_PROPERTY($80000006);
  {$EXTERNALSYM D2D1_PROPERTY_CACHED}
  D2D1_PROPERTY_PRECISION   = D2D1_PROPERTY($80000007);
  {$EXTERNALSYM D2D1_PROPERTY_PRECISION}
  D2D1_PROPERTY_MIN_INPUTS  = D2D1_PROPERTY($80000008);
  {$EXTERNALSYM D2D1_PROPERTY_MIN_INPUTS}
  D2D1_PROPERTY_MAX_INPUTS  = D2D1_PROPERTY($80000009);
  {$EXTERNALSYM D2D1_PROPERTY_MAX_INPUTS}
  //D2D1_PROPERTY_FORCE_DWORD = FORCEDWORD;

type
  // This defines the indices of sub-properties that may be present on any parent
  // property.
  PD2D1_SUBPROPERTY = ^D2D1_SUBPROPERTY;
  D2D1_SUBPROPERTY = Dword;
  {$EXTERNALSYM D2D1_SUBPROPERTY}
const
  D2D1_SUBPROPERTY_DISPLAYNAME = D2D1_SUBPROPERTY($80000000);
  {$EXTERNALSYM D2D1_SUBPROPERTY_DISPLAYNAME}
  D2D1_SUBPROPERTY_ISREADONLY  = D2D1_SUBPROPERTY($80000001);
  {$EXTERNALSYM D2D1_SUBPROPERTY_ISREADONLY}
  D2D1_SUBPROPERTY_MIN         = D2D1_SUBPROPERTY($80000002);
  {$EXTERNALSYM D2D1_SUBPROPERTY_MIN}
  D2D1_SUBPROPERTY_MAX         = D2D1_SUBPROPERTY($80000003);
  {$EXTERNALSYM D2D1_SUBPROPERTY_MAX}
  D2D1_SUBPROPERTY_DEFAULT     = D2D1_SUBPROPERTY($80000004);
  {$EXTERNALSYM D2D1_SUBPROPERTY_DEFAULT}
  D2D1_SUBPROPERTY_FIELDS      = D2D1_SUBPROPERTY($80000005);
  {$EXTERNALSYM D2D1_SUBPROPERTY_FIELDS}
  D2D1_SUBPROPERTY_INDEX       = D2D1_SUBPROPERTY($80000006);
  {$EXTERNALSYM D2D1_SUBPROPERTY_INDEX}
  //D2D1_SUBPROPERTY_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the composite mode that will be applied.
  PD2D1_COMPOSITE_MODE = ^D2D1_COMPOSITE_MODE;
  D2D1_COMPOSITE_MODE = DWord;
  {$EXTERNALSYM D2D1_COMPOSITE_MODE}
const
  D2D1_COMPOSITE_MODE_SOURCE_OVER         = D2D1_COMPOSITE_MODE(0);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OVER}
  D2D1_COMPOSITE_MODE_DESTINATION_OVER    = D2D1_COMPOSITE_MODE(1);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OVER}
  D2D1_COMPOSITE_MODE_SOURCE_IN           = D2D1_COMPOSITE_MODE(2);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_IN}
  D2D1_COMPOSITE_MODE_DESTINATION_IN      = D2D1_COMPOSITE_MODE(3);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_IN}
  D2D1_COMPOSITE_MODE_SOURCE_OUT          = D2D1_COMPOSITE_MODE(4);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OUT}
  D2D1_COMPOSITE_MODE_DESTINATION_OUT     = D2D1_COMPOSITE_MODE(5);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OUT}
  D2D1_COMPOSITE_MODE_SOURCE_ATOP         = D2D1_COMPOSITE_MODE(6);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_ATOP}
  D2D1_COMPOSITE_MODE_DESTINATION_ATOP    = D2D1_COMPOSITE_MODE(7);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_ATOP}
  D2D1_COMPOSITE_MODE_XOR                 = D2D1_COMPOSITE_MODE(8);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_XOR}
  D2D1_COMPOSITE_MODE_PLUS                = D2D1_COMPOSITE_MODE(9);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_PLUS}
  D2D1_COMPOSITE_MODE_SOURCE_COPY         = D2D1_COMPOSITE_MODE(10);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY = D2D1_COMPOSITE_MODE(11);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_MASK_INVERT         = D2D1_COMPOSITE_MODE(12);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_MASK_INVERT}
  //D2D1_COMPOSITE_MODE_FORCE_DWORD         = FORCEDWORD;

type
  // This specifies the precision that should be used in buffers allocated by D2D.
  PD2D1_BUFFER_PRECISION = ^D2D1_BUFFER_PRECISION;
  D2D1_BUFFER_PRECISION = DWord;
  {$EXTERNALSYM D2D1_BUFFER_PRECISION}
const
  D2D1_BUFFER_PRECISION_UNKNOWN         = D2D1_BUFFER_PRECISION(0);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_UNKNOWN}
  D2D1_BUFFER_PRECISION_8BPC_UNORM      = D2D1_BUFFER_PRECISION(1);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM}
  D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB = D2D1_BUFFER_PRECISION(2);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB}
  D2D1_BUFFER_PRECISION_16BPC_UNORM     = D2D1_BUFFER_PRECISION(3);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_UNORM}
  D2D1_BUFFER_PRECISION_16BPC_FLOAT     = D2D1_BUFFER_PRECISION(4);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_FLOAT}
  D2D1_BUFFER_PRECISION_32BPC_FLOAT     = D2D1_BUFFER_PRECISION(5);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_32BPC_FLOAT}
  //D2D1_BUFFER_PRECISION_FORCE_DWORD     = FORCEDWORD;

type
  // This describes how the individual mapping operation should be performed.
  PD2D1_MAP_OPTIONS = ^D2D1_MAP_OPTIONS;
  D2D1_MAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_MAP_OPTIONS}
const
  // The mapped pointer has undefined behavior.
  D2D1_MAP_OPTIONS_NONE        = D2D1_MAP_OPTIONS(0);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_NONE}

  // The mapped pointer can be read from.
  D2D1_MAP_OPTIONS_READ        = D2D1_MAP_OPTIONS(1);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_READ}

  // The mapped pointer can be written to.
  D2D1_MAP_OPTIONS_WRITE       = D2D1_MAP_OPTIONS(2);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_WRITE}

  // The previous contents of the bitmap are discarded when it is mapped.
  D2D1_MAP_OPTIONS_DISCARD     = D2D1_MAP_OPTIONS(4);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_DISCARD}
  //D2D1_MAP_OPTIONS_FORCE_DWORD = FORCEDWORD;


type
  // This is used to specify the quality of image scaling with
  // ID2D1DeviceContext.DrawImage and with the 2D Affine Transform Effect.
  PD2D1_INTERPOLATION_MODE = ^D2D1_INTERPOLATION_MODE;
  D2D1_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE}
const
  D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR    = D2D1_INTERPOLATION_MODE_DEFINITION_NEAREST_NEIGHBOR;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR}
  D2D1_INTERPOLATION_MODE_LINEAR              = D2D1_INTERPOLATION_MODE_DEFINITION_LINEAR;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_LINEAR}
  D2D1_INTERPOLATION_MODE_CUBIC               = D2D1_INTERPOLATION_MODE_DEFINITION_CUBIC;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_CUBIC}
  D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR = D2D1_INTERPOLATION_MODE_DEFINITION_MULTI_SAMPLE_LINEAR;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR}
  D2D1_INTERPOLATION_MODE_ANISOTROPIC         = D2D1_INTERPOLATION_MODE_DEFINITION_ANISOTROPIC;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_ANISOTROPIC}
  D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC  = D2D1_INTERPOLATION_MODE_DEFINITION_HIGH_QUALITY_CUBIC;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC}
  //D2D1_INTERPOLATION_MODE_FORCE_DWORD         = FORCEDWORD;

type
  // This specifies what units should be accepted by the D2D API.
  PD2D1_UNIT_MODE = ^D2D1_UNIT_MODE;
  D2D1_UNIT_MODE = DWord;
  {$EXTERNALSYM D2D1_UNIT_MODE}
const
  D2D1_UNIT_MODE_DIPS        = D2D1_UNIT_MODE(0);
  {$EXTERNALSYM D2D1_UNIT_MODE_DIPS}
  D2D1_UNIT_MODE_PIXELS      = D2D1_UNIT_MODE(1);
  {$EXTERNALSYM D2D1_UNIT_MODE_PIXELS}
  //D2D1_UNIT_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Defines a color space.
  PD2D1_COLOR_SPACE = ^D2D1_COLOR_SPACE;
  D2D1_COLOR_SPACE = DWord;
  {$EXTERNALSYM D2D1_COLOR_SPACE}
const
  // The color space is described by accompanying data); such as a color profile.
  D2D1_COLOR_SPACE_CUSTOM      = D2D1_COLOR_SPACE(0);
  {$EXTERNALSYM D2D1_COLOR_SPACE_CUSTOM}
  // The sRGB color space.
  D2D1_COLOR_SPACE_SRGB        = D2D1_COLOR_SPACE(1);
  {$EXTERNALSYM D2D1_COLOR_SPACE_SRGB}
  // The scRGB color space.
  D2D1_COLOR_SPACE_SCRGB       = D2D1_COLOR_SPACE(2);
  {$EXTERNALSYM D2D1_COLOR_SPACE_SCRGB}
  //D2D1_COLOR_SPACE_FORCE_DWORD = FORCEDWORD;

type
  // This specifies options that apply to the device context for its lifetime.
  PD2D1_DEVICE_CONTEXT_OPTIONS = ^D2D1_DEVICE_CONTEXT_OPTIONS;
  D2D1_DEVICE_CONTEXT_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_DEVICE_CONTEXT_OPTIONS}
const
  D2D1_DEVICE_CONTEXT_OPTIONS_NONE                               = D2D1_DEVICE_CONTEXT_OPTIONS(0);
  // Geometry rendering will be performed on many threads in parallel); a single
  // thread is the default.
  D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS = D2D1_DEVICE_CONTEXT_OPTIONS(1);
  //D2D1_DEVICE_CONTEXT_OPTIONS_FORCE_DWORD                        = FORCEDWORD;

type
  // Defines how the world transform); dots per inch (dpi)); and stroke width affect
  // the shape of the pen used to stroke a primitive.
  PD2D1_STROKE_TRANSFORM_TYPE = ^D2D1_STROKE_TRANSFORM_TYPE;
  D2D1_STROKE_TRANSFORM_TYPE = DWord;
  {$EXTERNALSYM D2D1_STROKE_TRANSFORM_TYPE}
const
  // The stroke respects the world transform); the DPI); and the stroke width.
  D2D1_STROKE_TRANSFORM_TYPE_NORMAL      = D2D1_STROKE_TRANSFORM_TYPE(0);
  // The stroke does not respect the world transform); but it does respect the DPI and
  // the stroke width.
  D2D1_STROKE_TRANSFORM_TYPE_FIXED       = D2D1_STROKE_TRANSFORM_TYPE(1);
  // The stroke is forced to one pixel wide.
  D2D1_STROKE_TRANSFORM_TYPE_HAIRLINE    = D2D1_STROKE_TRANSFORM_TYPE(2);
  //D2D1_STROKE_TRANSFORM_TYPE_FORCE_DWORD = FORCEDWORD;

type
  // A blend mode that applies to all primitives drawn on the context.
  PD2D1_PRIMITIVE_BLEND = ^D2D1_PRIMITIVE_BLEND;
  D2D1_PRIMITIVE_BLEND = DWord;
  {$EXTERNALSYM D2D1_PRIMITIVE_BLEND}
const
  D2D1_PRIMITIVE_BLEND_SOURCE_OVER = D2D1_PRIMITIVE_BLEND(0);
  D2D1_PRIMITIVE_BLEND_COPY        = D2D1_PRIMITIVE_BLEND(1);
  D2D1_PRIMITIVE_BLEND_MIN         = D2D1_PRIMITIVE_BLEND(2);
  D2D1_PRIMITIVE_BLEND_ADD         = D2D1_PRIMITIVE_BLEND(3);
  D2D1_PRIMITIVE_BLEND_MAX         = D2D1_PRIMITIVE_BLEND(4);
  //D2D1_PRIMITIVE_BLEND_FORCE_DWORD = FORCEDWORD;

type
  // This specifies the threading mode used while simultaneously creating the device);
  // factory); and device context.
  PD2D1_THREADING_MODE = ^D2D1_THREADING_MODE;
  D2D1_THREADING_MODE = DWord;
  {$EXTERNALSYM D2D1_THREADING_MODE}
const
  // Resources may only be invoked serially.  Reference counts on resources are
  // interlocked); however); resource and render target state is not protected from
  // multi-threaded access
  D2D1_THREADING_MODE_SINGLE_THREADED = D2D1_FACTORY_TYPE_SINGLE_THREADED;
  {$EXTERNALSYM D2D1_THREADING_MODE_SINGLE_THREADED}
  // Resources may be invoked from multiple threads. Resources use interlocked
  // reference counting and their state is protected.
  D2D1_THREADING_MODE_MULTI_THREADED  = D2D1_FACTORY_TYPE_MULTI_THREADED;
  {$EXTERNALSYM D2D1_THREADING_MODE_MULTI_THREADED}
  //D2D1_THREADING_MODE_FORCE_DWORD     = FORCEDWORD;

type
  // This specifies how colors are interpolated.
  PD2D1_COLOR_INTERPOLATION_MODE = ^D2D1_COLOR_INTERPOLATION_MODE;
  D2D1_COLOR_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE}
const
  // Colors will be interpolated in straight alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT      = D2D1_COLOR_INTERPOLATION_MODE(0);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT}
  // Colors will be interpolated in premultiplied alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED = D2D1_COLOR_INTERPOLATION_MODE(1);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED}
  //D2D1_COLOR_INTERPOLATION_MODE_FORCE_DWORD   = FORCEDWORD;


type
  // Defines when font resources should be subset during printing.
  PD2D1_PRINT_FONT_SUBSET_MODE = ^D2D1_PRINT_FONT_SUBSET_MODE;
  D2D1_PRINT_FONT_SUBSET_MODE = DWord;
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE}
const
  // Subset for used glyphs, send and discard font resource after every five pages
  D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT     = D2D1_PRINT_FONT_SUBSET_MODE(0);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT}
  // Subset for used glyphs, send and discard font resource after each page
  D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE    = D2D1_PRINT_FONT_SUBSET_MODE(1);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE}
  // Do not subset, reuse font for all pages, send it after first page
  D2D1_PRINT_FONT_SUBSET_MODE_NONE        = D2D1_PRINT_FONT_SUBSET_MODE(2);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_NONE}
  //D2D1_PRINT_FONT_SUBSET_MODE_FORCE_DWORD = FORCEDWORD;


type

  // Forward interface declarations

  PID2D1ColorContext = ^ID2D1ColorContext;
  ID2D1ColorContext = interface;

  PDwriteGlyphRunDescription = ^DWRITE_GLYPH_RUN_DESCRIPTION;
  DwriteGlyphRunDescription = DWRITE_GLYPH_RUN_DESCRIPTION;

  PID2D1Effect = ^ID2D1Effect;
  ID2D1Effect = interface;

  PID2D1Device = ^ID2D1Device;
  ID2D1Device = interface;


  // Function pointer to construct a new effect once registered.
  PD2D1_EFFECT_FACTORY = function(out effectImpl: IUnknown): HResult; stdcall;
  {$EXTERNALSYM PD2D1_EFFECT_FACTORY}


  PD2D1_RECT_L = ^D2D1_RECT_L;
  D2D1_RECT_L = D2D_RECT_L;
  {$EXTERNALSYM D2D1_RECT_L}

  PD2D1_POINT_2L = ^D2D1_POINT_2L;
  D2D1_POINT_2L = D2D_POINT_2L;
  {$EXTERNALSYM D2D1_POINT_2L}


  PD2D_VECTOR_2F = ^D2D_VECTOR_2F;
  D2D1_VECTOR_2F = D2D_VECTOR_2F;
  {$EXTERNALSYM D2D1_VECTOR_2F}

  PD2D_VECTOR_3F = ^D2D_VECTOR_3F;
  D2D1_VECTOR_3F = D2D_VECTOR_3F;
  {$EXTERNALSYM D2D1_VECTOR_3F}

  PD2D_VECTOR_4F = ^D2D_VECTOR_4F;
  D2D1_VECTOR_4F = D2D_VECTOR_4F;
  {$EXTERNALSYM D2D1_VECTOR_4F}


  // Extended bitmap properties.
  PD2D1_BITMAP_PROPERTIES1 = ^D2D1_BITMAP_PROPERTIES1;
  D2D1_BITMAP_PROPERTIES1 = record
    _pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
    // Specifies how the bitmap can be used.
    bitmapOptions: D2D1_BITMAP_OPTIONS;
    colorContext: ID2D1ColorContext;
  end;
  {$EXTERNALSYM D2D1_BITMAP_PROPERTIES1}


  // Describes mapped memory from the ID2D1Bitmap1.Map API.
  PD2D1_MAPPED_RECT = ^D2D1_MAPPED_RECT;
  D2D1_MAPPED_RECT = record
    pitch: UINT32;
    bits: PByte;
  end;
  {$EXTERNALSYM D2D1_MAPPED_RECT}


  // This controls advanced settings of the Direct2D imaging pipeline.
  PD2D1_RENDERING_CONTROLS = ^D2D1_RENDERING_CONTROLS;
  D2D1_RENDERING_CONTROLS = record
    // The default buffer precision, used if the precision isn't otherwise specified.
    bufferPrecision: D2D1_BUFFER_PRECISION;
    // The size of allocated tiles used to render imaging effects.
    tileSize: D2D1_SIZE_U;
  end;
  {$EXTERNALSYM D2D1_RENDERING_CONTROLS}


  // This identifies a certain input connection of a certain effect.
  PD2D1_EFFECT_INPUT_DESCRIPTION = ^D2D1_EFFECT_INPUT_DESCRIPTION;
  D2D1_EFFECT_INPUT_DESCRIPTION = record
    // The effect whose input connection is being specified.
    effect: ID2D1Effect;
    // The index of the input connection into the specified effect.
    inputIndex: UINT32;
    // The rectangle which would be available on the specified input connection during
    // render operations.
    inputRectangle: D2D1_RECT_F;
  end;
  {$EXTERNALSYM D2D1_EFFECT_INPUT_DESCRIPTION}


  PD2D1_MATRIX_4X3_F = ^D2D_MATRIX_4X3_F;
  D2D1_MATRIX_4X3_F = D2D_MATRIX_4X3_F;
  {$EXTERNALSYM D2D1_MATRIX_4X3_F}

  PD2D1_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D1_MATRIX_4X4_F = D2D_MATRIX_4X4_F;
  {$EXTERNALSYM D2D1_MATRIX_4X4_F}

  PD2D1_MATRIX_5X4_F = ^D2D_MATRIX_5X4_F;
  D2D1_MATRIX_5X4_F = D2D_MATRIX_5X4_F;
  {$EXTERNALSYM D2D1_MATRIX_5X4_F}


  // Describes a point along a path.
  PD2D1_POINT_DESCRIPTION = ^D2D1_POINT_DESCRIPTION;
  D2D1_POINT_DESCRIPTION = record
    point: D2D1_POINT_2F;
    unitTangentVector: D2D1_POINT_2F;
    endSegment: UINT32;
    endFigure: UINT32;
    lengthToEndSegment: Single;
  end;
  {$EXTERNALSYM D2D1_POINT_DESCRIPTION}


  // Creation properties for an image brush.
  PD2D1_IMAGE_BRUSH_PROPERTIES = ^D2D1_IMAGE_BRUSH_PROPERTIES;
  D2D1_IMAGE_BRUSH_PROPERTIES = record
    sourceRectangle: D2D1_RECT_F;
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_IMAGE_BRUSH_PROPERTIES}


  // Describes the extend modes and the interpolation mode of an ID2D1BitmapBrush.
  PD2D1_BITMAP_BRUSH_PROPERTIES1 = ^D2D1_BITMAP_BRUSH_PROPERTIES1;
  D2D1_BITMAP_BRUSH_PROPERTIES1 = record
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_BITMAP_BRUSH_PROPERTIES1}


  // This defines how geometries should be drawn and widened.
  PD2D1_STROKE_STYLE_PROPERTIES1 = ^D2D1_STROKE_STYLE_PROPERTIES1;
  D2D1_STROKE_STYLE_PROPERTIES1 = record
    startCap: D2D1_CAP_STYLE;
    endCap: D2D1_CAP_STYLE;
    dashCap: D2D1_CAP_STYLE;
    lineJoin: D2D1_LINE_JOIN;
    miterLimit: Single;
    dashStyle: D2D1_DASH_STYLE;
    dashOffset: Single;
    // How the nib of the stroke is influenced by the context properties.
    transformType: D2D1_STROKE_TRANSFORM_TYPE;
  end;
  {$EXTERNALSYM D2D1_STROKE_STYLE_PROPERTIES1}


  // D2D1_LAYER_OPTIONS1  


  // All parameters related to pushing a layer.
  PD2D1_LAYER_PARAMETERS1 = ^D2D1_LAYER_PARAMETERS1;
  D2D1_LAYER_PARAMETERS1 = record
    contentBounds: D2D1_RECT_F;
    geometricMask: ID2D1Geometry;
    maskAntialiasMode: D2D1_ANTIALIAS_MODE;
    maskTransform: D2D1_MATRIX_3X2_F;
    opacity: Single;
    opacityBrush: ID2D1Brush;
    layerOptions: D2D1_LAYER_OPTIONS1;
  end;
  {$EXTERNALSYM D2D1_LAYER_PARAMETERS1}


  // This describes the drawing state.
  PD2D1_DRAWING_STATE_DESCRIPTION1 = ^D2D1_DRAWING_STATE_DESCRIPTION1;
  D2D1_DRAWING_STATE_DESCRIPTION1 = record
    antialiasMode: D2D1_ANTIALIAS_MODE;
    textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE;
    tag1: D2D1_TAG;
    tag2: D2D1_TAG;
    transform: D2D1_MATRIX_3X2_F;
    primitiveBlend: D2D1_PRIMITIVE_BLEND;
    unitMode: D2D1_UNIT_MODE;
  end;
  {$EXTERNALSYM D2D1_DRAWING_STATE_DESCRIPTION1}

  // The creation properties for a ID2D1PrintControl object.
  PD2D1_PRINT_CONTROL_PROPERTIES = ^D2D1_PRINT_CONTROL_PROPERTIES;
  D2D1_PRINT_CONTROL_PROPERTIES = record
    fontSubset: D2D1_PRINT_FONT_SUBSET_MODE;
    // DPI for rasterization of all unsupported D2D commands or options, defaults to
    // 150.0
    rasterDPI: Single;
    // Color space for vector graphics in XPS package
    colorSpace: D2D1_COLOR_SPACE;
  end;
  {$EXTERNALSYM D2D1_PRINT_CONTROL_PROPERTIES}


  // This specifies the options while simultaneously creating the device, factory,
  // and device context.
  PD2D1_CREATION_PROPERTIES = ^D2D1_CREATION_PROPERTIES;
  D2D1_CREATION_PROPERTIES = record
    // Describes locking behavior of D2D resources
    threadingMode: D2D1_THREADING_MODE;
    debugLevel: D2D1_DEBUG_LEVEL;
    options: D2D1_DEVICE_CONTEXT_OPTIONS;
  end;
  {$EXTERNALSYM D2D1_CREATION_PROPERTIES}


  // Defines a property binding to a function. The name must match the property
  // defined in the registration schema.
  // Delphi Note: Moved from D2D1EffectAuthor to D2D1_1 (to prevent circular reference)
  //
  PD2D1_PROPERTY_BINDING = ^D2D1_PROPERTY_BINDING;
  D2D1_PROPERTY_BINDING = record
    // The name of the property.
    propertyName: PWidechar;
    // The function that will receive the data to set.
    setFunction: PD2D1_PROPERTY_SET_FUNCTION;
    // The function that will be asked to write the output data.
    getFunction: PD2D1_PROPERTY_GET_FUNCTION;
  end;
  {$EXTERNALSYM D2D1_PROPERTY_BINDING}


  //
  // INTERFACES ///////////////////////////////////////////
  //


  // Interface ID2D1GdiMetafileSink
  // ==============================
  // User-implementable interface for introspecting on a metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafileSink);'}
  {$EXTERNALSYM ID2D1GdiMetafileSink}
  ID2D1GdiMetafileSink = interface(IUnknown)
  ['{82237326-8111-4f7c-bcf4-b5c1175564fe}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function ProcessRecord(recordType: DWORD; recordData: Pointer; recordDataSize: DWORD): HRESULT; stdcall; overload;
  end;
  IID_ID2D1GdiMetafileSink = ID2D1GdiMetafileSink;
  {$EXTERNALSYM IID_ID2D1GdiMetafileSink}


  // Interface ID2D1GdiMetafile
  // ==========================
  // Interface encapsulating a GDI/GDI+ metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafile);'}
  {$EXTERNALSYM ID2D1GdiMetafile}
  ID2D1GdiMetafile = interface(ID2D1Resource)
  ['{2f543dc3-cfc1-4211-864f-cfd91c6f3395}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function Stream(sink: ID2D1GdiMetafileSink): HRESULT; stdcall; overload;

    function GetBounds(bounds: PD2D1_RECT_F): HRESULT; stdcall; overload;
  end;
  IID_ID2D1GdiMetafile = ID2D1GdiMetafile;
  {$EXTERNALSYM IID_ID2D1GdiMetafile}


  // Interface ID2D1CommandSink
  // ==========================
  // Caller-supplied implementation of an interface to receive the recorded command
  // list.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink);'}
  {$EXTERNALSYM ID2D1CommandSink}
  ID2D1CommandSink = interface(IUnknown)
  ['{54D7898A-A061-40A7-BEC7-E465BCBA2C4F}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function BeginDraw(): HRESULT; stdcall; overload;

    function EndDraw(): HRESULT; stdcall; overload;

    function SetAntialiasMode(antialiasMode: D2D1_ANTIALIAS_MODE): HRESULT; stdcall; overload;

    function SetTags(tag1: D2D1_TAG; tag2: D2D1_TAG): HRESULT; stdcall; overload;

    function SetTextAntialiasMode(textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE): HRESULT; stdcall; overload;

    function SetTextRenderingParams(textRenderingParams: IDWriteRenderingParams): HRESULT; stdcall; overload;

    function SetTransform(transform: PD2D1_MATRIX_3X2_F): HRESULT; stdcall; overload;

    function SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND): HRESULT; stdcall; overload;

    function SetUnitMode(unitMode: D2D1_UNIT_MODE): HRESULT; stdcall; overload;

    function Clear(color: PD2D1_COLOR_F): HRESULT; stdcall; overload;

    function DrawGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION; foregroundBrush: ID2D1Brush; measuringMode: DWRITE_MEASURING_MODE): HRESULT; stdcall; overload;

    function DrawLine(point0: D2D1_POINT_2F; point1: D2D1_POINT_2F; brush: ID2D1Brush; strokeWidth: Single; strokeStyle: ID2D1StrokeStyle): HRESULT; stdcall; overload;

    function DrawGeometry(geometry: ID2D1Geometry; brush: ID2D1Brush; strokeWidth: Single; strokeStyle: ID2D1StrokeStyle): HRESULT; stdcall; overload;

    function DrawRectangle(rect: PD2D1_RECT_F; brush: ID2D1Brush; strokeWidth: Single; strokeStyle: ID2D1StrokeStyle): HRESULT; stdcall; overload;

    function DrawBitmap(bitmap: ID2D1Bitmap; destinationRectangle: PD2D1_RECT_F; opacity: Single; interpolationMode: D2D1_INTERPOLATION_MODE; sourceRectangle: PD2D1_RECT_F; perspectiveTransform: PD2D1_MATRIX_4X4_F): HRESULT; stdcall; overload;

    function DrawImage(image: ID2D1Image; targetOffset: PD2D1_POINT_2F; imageRectangle: PD2D1_RECT_F; interpolationMode: D2D1_INTERPOLATION_MODE; compositeMode: D2D1_COMPOSITE_MODE): HRESULT; stdcall; overload;

    function DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile; targetOffset: PD2D1_POINT_2F): HRESULT; stdcall; overload;

    function FillMesh(mesh: ID2D1Mesh; brush: ID2D1Brush): HRESULT; stdcall; overload;

    function FillOpacityMask(opacityMask: ID2D1Bitmap; brush: ID2D1Brush; destinationRectangle: PD2D1_RECT_F; sourceRectangle: PD2D1_RECT_F): HRESULT; stdcall; overload;

    function FillGeometry(geometry: ID2D1Geometry; brush: ID2D1Brush; opacityBrush: ID2D1Brush): HRESULT; stdcall; overload;

    function FillRectangle(rect: PD2D1_RECT_F; brush: ID2D1Brush): HRESULT; stdcall; overload;

    function PushAxisAlignedClip(clipRect: PD2D1_RECT_F; antialiasMode: D2D1_ANTIALIAS_MODE): HRESULT; stdcall; overload;

    function PushLayer(layerParameters1: PD2D1_LAYER_PARAMETERS1; layer: ID2D1Layer): HRESULT; stdcall; overload;

    function PopAxisAlignedClip(): HRESULT; stdcall; overload;

    function PopLayer(): HRESULT; stdcall; overload;
  end;
  IID_ID2D1CommandSink = ID2D1CommandSink;
  {$EXTERNALSYM IID_ID2D1CommandSink}


  // Interface ID2D1CommandList
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandList);'}
  {$EXTERNALSYM ID2D1CommandList}
  ID2D1CommandList = interface(ID2D1Image)
  ['{b4f34a19-2383-4d76-94f6-ec343657c3dc}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function Stream(sink: ID2D1CommandSink): HRESULT; stdcall; overload;

    function Close(): HRESULT; stdcall; overload;
  end;
  IID_ID2D1CommandList = ID2D1CommandList;
  {$EXTERNALSYM IID_ID2D1CommandList}


  // Interface ID2D1PrintControl
  // ===========================
  // Converts Direct2D primitives stored in an ID2D1CommandList into a fixed page
  // representation. The print sub-system then consumes the primitives.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PrintControl);'}
  {$EXTERNALSYM ID2D1PrintControl}
  ID2D1PrintControl = interface(IUnknown)
  ['{2c1d867d-c290-41c8-ae7e-34a98702e9a5}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function AddPage(commandList: ID2D1CommandList; pageSize: D2D_SIZE_F; pagePrintTicketStream: IStream; tag1: PD2D1_TAG; tag2: PD2D1_TAG): HRESULT; stdcall; overload;

    function Close(): HRESULT; stdcall; overload;
  end;
  IID_ID2D1PrintControl = ID2D1PrintControl;
  {$EXTERNALSYM IID_ID2D1PrintControl}


  // Interface ID2D1ImageBrush
  // =========================
  // Provides a brush that can take any effect, command list or bitmap and use it to
  // fill a 2D shape.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageBrush);'}
  {$EXTERNALSYM ID2D1ImageBrush}
  ID2D1ImageBrush = interface(ID2D1Brush)
  ['{fe9e984d-3f95-407c-b5db-cb94d4e8f87c}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure SetImage(image: ID2D1Image); stdcall; overload;

    procedure SetExtendModeX(extendModeX: D2D1_EXTEND_MODE); stdcall; overload;

    procedure SetExtendModeY(extendModeY: D2D1_EXTEND_MODE); stdcall; overload;

    procedure SetInterpolationMode(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall; overload;

    procedure SetSourceRectangle(sourceRectangle: PD2D1_RECT_F); stdcall; overload;

    procedure GetImage(out image: ID2D1Image); stdcall; overload;

    function GetExtendModeX(): D2D1_EXTEND_MODE; stdcall; overload;

    function GetExtendModeY(): D2D1_EXTEND_MODE; stdcall; overload;

    function GetInterpolationMode(): D2D1_INTERPOLATION_MODE; stdcall; overload;

    procedure GetSourceRectangle(sourceRectangle: PD2D1_RECT_F); stdcall; overload;
  end;
  IID_ID2D1ImageBrush = ID2D1ImageBrush;
  {$EXTERNALSYM IID_ID2D1ImageBrush}


  // Interface ID2D1BitmapBrush1
  // ===========================
  // A bitmap brush allows a bitmap to be used to fill a geometry.  Interpolation
  // mode is specified with D2D1_INTERPOLATION_MODE
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BitmapBrush1);'}
  {$EXTERNALSYM ID2D1BitmapBrush1}
  ID2D1BitmapBrush1 = interface(ID2D1BitmapBrush)
  ['{41343a53-e41a-49a2-91cd-21793bbb62e5}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure SetInterpolationMode1(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall; overload;

    function GetInterpolationMode1(): D2D1_INTERPOLATION_MODE; stdcall; overload;
  end;
  IID_ID2D1BitmapBrush1 = ID2D1BitmapBrush1;
  {$EXTERNALSYM IID_ID2D1BitmapBrush1}


  // Interface ID2D1StrokeStyle1
  // ===========================
  // Extends a stroke style to allow nominal width strokes.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1StrokeStyle1);'}
  {$EXTERNALSYM ID2D1StrokeStyle1}
  ID2D1StrokeStyle1 = interface(ID2D1StrokeStyle)
  ['{10a72a66-e91c-43f4-993f-ddf4b82b0b4a}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function GetStrokeTransformType(): D2D1_STROKE_TRANSFORM_TYPE; stdcall; overload;
  end;
  IID_ID2D1StrokeStyle1 = ID2D1StrokeStyle1;
  {$EXTERNALSYM IID_ID2D1StrokeStyle1}


  // Interface ID2D1PathGeometry1
  // ============================
  // The ID2D1PathGeometry1 interface adds functionality to ID2D1PathGeometry. In
  // particular, it provides the path geometry-specific
  // ComputePointAndSegmentAtLength method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PathGeometry1);'}
  {$EXTERNALSYM ID2D1PathGeometry1}
  ID2D1PathGeometry1 = interface(ID2D1PathGeometry)
  ['{62baa2d2-ab54-41b7-b872-787e0106a421}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function ComputePointAndSegmentAtLength(length: Single; startSegment: UINT32; worldTransform: PD2D1_MATRIX_3X2_F; flatteningTolerance: Single; pointDescription: PD2D1_POINT_DESCRIPTION): HRESULT; stdcall; overload;
  end;
  IID_ID2D1PathGeometry1 = ID2D1PathGeometry1;
  {$EXTERNALSYM IID_ID2D1PathGeometry1}


  // Interface ID2D1Properties
  // =========================
  // Represents a set of run-time bindable and discoverable properties that allow a
  // data-driven application to modify the state of a Direct2D effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Properties);'}
  {$EXTERNALSYM ID2D1Properties}
  ID2D1Properties = interface(IUnknown)
  ['{483473d7-cd46-4f9d-9d3a-3112aa80159d}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function GetPropertyCount(): UINT32; stdcall; overload;

    function GetPropertyName(index: UINT32; name: PWideChar; nameCount: UINT32): HRESULT; stdcall; overload;

    function GetPropertyNameLength(index: UINT32): UINT32; stdcall; overload;

    function GetType(index: UINT32): D2D1_PROPERTY_TYPE; stdcall; overload;

    function GetPropertyIndex(name: PWideChar): UINT32; stdcall; overload;

    function SetValueByName(name: PWideChar; _type: D2D1_PROPERTY_TYPE; data: PByte; dataSize: UINT32): HRESULT; stdcall; overload;

    function SetValue(index: UINT32; _type: D2D1_PROPERTY_TYPE; data: PByte; dataSize: UINT32): HRESULT; stdcall; overload;

    function GetValueByName(name: PWideChar; _type: D2D1_PROPERTY_TYPE; data: PByte; dataSize: UINT32): HRESULT; stdcall; overload;

    function GetValue(index: UINT32; _type: D2D1_PROPERTY_TYPE; data: PByte; dataSize: UINT32): HRESULT; stdcall; overload;

    function GetValueSize(index: UINT32): UINT32; stdcall; overload;

    function GetSubProperties(index: UINT32; out subProperties: ID2D1Properties): HRESULT; stdcall; overload;
  end;
  IID_ID2D1Properties = ID2D1Properties;
  {$EXTERNALSYM IID_ID2D1Properties}


  // Interface ID2D1Effect
  // =====================
  // The effect interface. Properties control how the effect is rendered. The effect
  // is Drawn with the DrawImage call.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Effect);'}
  {$EXTERNALSYM ID2D1Effect}
  ID2D1Effect = interface(ID2D1Properties)
  ['{28211a43-7d89-476f-8181-2d6159b220ad}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure SetInput(index: UINT32; input: ID2D1Image; invalidate: BOOL); stdcall; overload;

    function SetInputCount(inputCount: UINT32): HRESULT; stdcall; overload;

    procedure GetInput(index: UINT32; out input: ID2D1Image); stdcall; overload;

    function GetInputCount(): UINT32; stdcall; overload;

    procedure GetOutput(out outputImage: ID2D1Image); stdcall; overload;
  end;
  IID_ID2D1Effect = ID2D1Effect;
  {$EXTERNALSYM IID_ID2D1Effect}


  // Interface ID2D1Bitmap1
  // ======================
  // Represents a bitmap that can be used as a surface for an ID2D1DeviceContext or
  // mapped into system memory, and can contain additional color context information.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Bitmap1);'}
  {$EXTERNALSYM ID2D1Bitmap1}
  ID2D1Bitmap1 = interface(ID2D1Bitmap)
  ['{a898a84c-3873-4588-b08b-ebbf978df041}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure GetColorContext(out colorContext: ID2D1ColorContext); stdcall; overload;

    function GetOptions(): D2D1_BITMAP_OPTIONS; stdcall; overload;

    function GetSurface(out dxgiSurface: IDXGISurface): HRESULT; stdcall; overload;

    function Map(options: D2D1_MAP_OPTIONS; mappedRect: PD2D1_MAPPED_RECT): HRESULT; stdcall; overload;

    function Unmap(): HRESULT; stdcall; overload;
  end;
  IID_ID2D1Bitmap1 = ID2D1Bitmap1;
  {$EXTERNALSYM IID_ID2D1Bitmap1}


  // Interface ID2D1ColorContext
  // ===========================
  // Represents a color context that can be used with an ID2D1Bitmap1 object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ColorContext);'}
  {$EXTERNALSYM ID2D1ColorContext}
  ID2D1ColorContext = interface(ID2D1Resource)
  ['{1c4820bb-5771-4518-a581-2fe4dd0ec657}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function GetColorSpace(): D2D1_COLOR_SPACE; stdcall; overload;

    function GetProfileSize(): UINT32; stdcall; overload;

    function GetProfile(profile: PByte; profileSize: UINT32): HRESULT; stdcall; overload;
  end;
  IID_ID2D1ColorContext = ID2D1ColorContext;
  {$EXTERNALSYM IID_ID2D1ColorContext}


  // Interface ID2D1GradientStopCollection1
  // ======================================
  // Represents an collection of gradient stops that can then be the source resource
  // for either a linear or radial gradient brush.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientStopCollection1);'}
  {$EXTERNALSYM ID2D1GradientStopCollection1}
  ID2D1GradientStopCollection1 = interface(ID2D1GradientStopCollection)
  ['{ae1572f4-5dd0-4777-998b-9279472ae63b}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure GetGradientStops1(gradientStops: PD2D1_GRADIENT_STOP; gradientStopsCount: UINT32); stdcall; overload;

    function GetPreInterpolationSpace(): D2D1_COLOR_SPACE; stdcall; overload;

    function GetPostInterpolationSpace(): D2D1_COLOR_SPACE; stdcall; overload;

    function GetBufferPrecision(): D2D1_BUFFER_PRECISION; stdcall; overload;

    function GetColorInterpolationMode(): D2D1_COLOR_INTERPOLATION_MODE; stdcall; overload;
  end;
  IID_ID2D1GradientStopCollection1 = ID2D1GradientStopCollection1;
  {$EXTERNALSYM IID_ID2D1GradientStopCollection1}


  // Interface ID2D1DrawingStateBlock1
  // =================================
  // Represents drawing state.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DrawingStateBlock1);'}
  {$EXTERNALSYM ID2D1DrawingStateBlock1}
  ID2D1DrawingStateBlock1 = interface(ID2D1DrawingStateBlock)
  ['{689f1f85-c72e-4e33-8f19-85754efd5ace}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    procedure GetDescription(stateDescription: PD2D1_DRAWING_STATE_DESCRIPTION1); stdcall; overload;

    procedure SetDescription(stateDescription: PD2D1_DRAWING_STATE_DESCRIPTION1); stdcall; overload;
  end;
  IID_ID2D1DrawingStateBlock1 = ID2D1DrawingStateBlock1;
  {$EXTERNALSYM IID_ID2D1DrawingStateBlock1}


  // Interface ID2D1DeviceContext
  // ============================
  // The device context represents a set of state and a command buffer that is used
  // to render to a target bitmap.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext);'}
  {$EXTERNALSYM ID2D1DeviceContext}
  ID2D1DeviceContext = interface(ID2D1RenderTarget)
  ['{e8f7fe7a-191c-466d-ad95-975678bda998}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function CreateBitmap(size: D2D1_SIZE_U; sourceData: Pointer; pitch: UINT32; bitmapProperties: PD2D1_BITMAP_PROPERTIES1; out bitmap: ID2D1Bitmap1): HRESULT; stdcall; overload;

    function CreateBitmapFromWicBitmap(wicBitmapSource: IWICBitmapSource; bitmapProperties: PD2D1_BITMAP_PROPERTIES1; out bitmap: ID2D1Bitmap1): HRESULT; stdcall; overload;

    function CreateColorContext(space: D2D1_COLOR_SPACE; profile: PByte; profileSize: UINT32; out colorContext: ID2D1ColorContext): HRESULT; stdcall; overload;

    function CreateColorContextFromFilename(filename: PWideChar; out colorContext: ID2D1ColorContext): HRESULT; stdcall; overload;

    function CreateColorContextFromWicColorContext(wicColorContext: IWICColorContext; out colorContext: ID2D1ColorContext): HRESULT; stdcall; overload;

    function CreateBitmapFromDxgiSurface(surface: IDXGISurface; bitmapProperties: PD2D1_BITMAP_PROPERTIES1; out bitmap: ID2D1Bitmap1): HRESULT; stdcall; overload;

    function CreateEffect(const effectId: TGUID; out effect: ID2D1Effect): HRESULT; stdcall; overload;

    function CreateGradientStopCollection(straightAlphaGradientStops: PD2D1_GRADIENT_STOP; straightAlphaGradientStopsCount: UINT32; preInterpolationSpace: D2D1_COLOR_SPACE; postInterpolationSpace: D2D1_COLOR_SPACE; bufferPrecision: D2D1_BUFFER_PRECISION; extendMode: D2D1_EXTEND_MODE; colorInterpolationMode: D2D1_COLOR_INTERPOLATION_MODE; out gradientStopCollection1: ID2D1GradientStopCollection1): HRESULT; stdcall; overload;

    function CreateImageBrush(image: ID2D1Image; imageBrushProperties: PD2D1_IMAGE_BRUSH_PROPERTIES; brushProperties: PD2D1_BRUSH_PROPERTIES; out imageBrush: ID2D1ImageBrush): HRESULT; stdcall; overload;

    function CreateBitmapBrush(bitmap: ID2D1Bitmap; bitmapBrushProperties: PD2D1_BITMAP_BRUSH_PROPERTIES1; brushProperties: PD2D1_BRUSH_PROPERTIES; out bitmapBrush: ID2D1BitmapBrush1): HRESULT; stdcall; overload;

    function CreateCommandList(out commandList: ID2D1CommandList): HRESULT; stdcall; overload;

    function IsDxgiFormatSupported(format: DXGI_FORMAT): BOOL; stdcall; overload;

    function IsBufferPrecisionSupported(bufferPrecision: D2D1_BUFFER_PRECISION): BOOL; stdcall; overload;

    function GetImageLocalBounds(image: ID2D1Image; localBounds: PD2D1_RECT_F): HRESULT; stdcall; overload;

    function GetImageWorldBounds(image: ID2D1Image; worldBounds: PD2D1_RECT_F): HRESULT; stdcall; overload;

    function GetGlyphRunWorldBounds(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; measuringMode: DWRITE_MEASURING_MODE; bounds: PD2D1_RECT_F): HRESULT; stdcall; overload;

    procedure GetDevice(out device: ID2D1Device); stdcall; overload;

    procedure SetTarget(image: ID2D1Image); stdcall; overload;

    procedure GetTarget(out image: ID2D1Image); stdcall; overload;

    procedure SetRenderingControls(renderingControls: PD2D1_RENDERING_CONTROLS); stdcall; overload;

    procedure GetRenderingControls(renderingControls: PD2D1_RENDERING_CONTROLS); stdcall; overload;

    procedure SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND); stdcall; overload;

    function GetPrimitiveBlend(): D2D1_PRIMITIVE_BLEND; stdcall; overload;

    procedure SetUnitMode(unitMode: D2D1_UNIT_MODE); stdcall; overload;

    function GetUnitMode(): D2D1_UNIT_MODE; stdcall; overload;

    procedure DrawGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION; foregroundBrush: ID2D1Brush; measuringMode: DWRITE_MEASURING_MODE); stdcall; overload;

    procedure DrawImage(image: ID2D1Image; targetOffset: PD2D1_POINT_2F; imageRectangle: PD2D1_RECT_F; interpolationMode: D2D1_INTERPOLATION_MODE; compositeMode: D2D1_COMPOSITE_MODE); stdcall; overload;

    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile; targetOffset: PD2D1_POINT_2F); stdcall; overload;

    procedure DrawBitmap(bitmap: ID2D1Bitmap; destinationRectangle: PD2D1_RECT_F; opacity: Single; interpolationMode: D2D1_INTERPOLATION_MODE; sourceRectangle: PD2D1_RECT_F; perspectiveTransform: PD2D1_MATRIX_4X4_F); stdcall; overload;

    procedure PushLayer(layerParameters: PD2D1_LAYER_PARAMETERS1; layer: ID2D1Layer); stdcall; overload;

    function InvalidateEffectInputRectangle(effect: ID2D1Effect; input: UINT32; inputRectangle: PD2D1_RECT_F): HRESULT; stdcall; overload;

    function GetEffectInvalidRectangleCount(effect: ID2D1Effect; rectangleCount: PUINT32): HRESULT; stdcall; overload;

    function GetEffectInvalidRectangles(effect: ID2D1Effect; rectangles: PD2D1_RECT_F; rectanglesCount: UINT32): HRESULT; stdcall; overload;

    function GetEffectRequiredInputRectangles(renderEffect: ID2D1Effect; renderImageRectangle: PD2D1_RECT_F; inputDescriptions: PD2D1_EFFECT_INPUT_DESCRIPTION; requiredInputRects: PD2D1_RECT_F; inputCount: UINT32): HRESULT; stdcall; overload;

    procedure FillOpacityMask(opacityMask: ID2D1Bitmap; brush: ID2D1Brush; destinationRectangle: PD2D1_RECT_F; sourceRectangle: PD2D1_RECT_F); stdcall; overload;
  end;
  IID_ID2D1DeviceContext = ID2D1DeviceContext;
  {$EXTERNALSYM IID_ID2D1DeviceContext}


  // Interface ID2D1Device
  // =====================
  // The device defines a resource domain whose objects and device contexts can be
  // used together.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device);'}
  {$EXTERNALSYM ID2D1Device}
  ID2D1Device = interface(ID2D1Resource)
  ['{47dd575d-ac05-4cdd-8049-9b02cd16f44c}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext: ID2D1DeviceContext): HRESULT; stdcall; overload;

    function CreatePrintControl(wicFactory: IWICImagingFactory; documentTarget: IPrintDocumentPackageTarget; printControlProperties: PD2D1_PRINT_CONTROL_PROPERTIES; out printControl: ID2D1PrintControl): HRESULT; stdcall; overload;

    procedure SetMaximumTextureMemory(maximumInBytes: UINT64); stdcall; overload;

    function GetMaximumTextureMemory(): UINT64; stdcall; overload;

    procedure ClearResources(millisecondsSinceUse: UINT32); stdcall; overload;
  end;
  IID_ID2D1Device = ID2D1Device;
  {$EXTERNALSYM IID_ID2D1Device}


  // Interface ID2D1Factory1
  // =======================
  // Creates Direct2D resources.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory1);'}
  {$EXTERNALSYM ID2D1Factory1}
  ID2D1Factory1 = interface(ID2D1Factory)
  ['{bb12d362-daee-4b9a-aa1d-14ba401cfa1f}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice: ID2D1Device): HRESULT; stdcall; overload;

    function CreateStrokeStyle(strokeStyleProperties: PD2D1_STROKE_STYLE_PROPERTIES1; dashes: PSingle; dashesCount: UINT32; out strokeStyle: ID2D1StrokeStyle1): HRESULT; stdcall; overload;

    function CreatePathGeometry(out pathGeometry: ID2D1PathGeometry1): HRESULT; stdcall; overload;

    function CreateDrawingStateBlock(drawingStateDescription: PD2D1_DRAWING_STATE_DESCRIPTION1; textRenderingParams: IDWriteRenderingParams; out drawingStateBlock: ID2D1DrawingStateBlock1): HRESULT; stdcall; overload;

    function CreateGdiMetafile(metafileStream: IStream; out metafile: ID2D1GdiMetafile): HRESULT; stdcall; overload;

    function RegisterEffectFromStream(const classId: TGUID; propertyXml: IStream; bindings: PD2D1_PROPERTY_BINDING; bindingsCount: UINT32; effectFactory: PD2D1_EFFECT_FACTORY): HRESULT; stdcall; overload;

    function RegisterEffectFromString(const classId: TGUID; propertyXml: PWideChar; bindings: PD2D1_PROPERTY_BINDING; bindingsCount: UINT32; effectFactory: PD2D1_EFFECT_FACTORY): HRESULT; stdcall; overload;

    function UnregisterEffect(const classId: TGUID): HRESULT; stdcall; overload;

    function GetRegisteredEffects(effects: PCLSID; effectsCount: UINT32; effectsReturned: PUINT32; effectsRegistered: PUINT32): HRESULT; stdcall; overload;

    function GetEffectProperties(const effectId: TGUID; out properties: ID2D1Properties): HRESULT; stdcall; overload;
  end;
  IID_ID2D1Factory1 = ID2D1Factory1;
  {$EXTERNALSYM IID_ID2D1Factory1}


  // Interface ID2D1Multithread
  // ==========================
  //
  // A locking mechanism from a Direct2D factory that Direct2D uses to control
  // exclusive resource access in an app that is uses multiple threads.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Multithread);'}
  {$EXTERNALSYM ID2D1Multithread}
  ID2D1Multithread = interface(IUnknown)
  ['{31e6e7bc-e0ff-4d46-8c64-a0a8c41c15d3}']
    // SDK native virtual slots; inline convenience overloads are excluded.
    function GetMultithreadProtected(): BOOL; stdcall; overload;

    procedure Enter(); stdcall; overload;

    procedure Leave(); stdcall; overload;
  end;
  IID_ID2D1Multithread = ID2D1Multithread;
  {$EXTERNALSYM IID_ID2D1Multithread}




  // FUNCTIONS ////////////////////////////////////////////


  function D2D1CreateDevice(dxgiDevice: IDXGIDevice; creationProperties: PD2D1_CREATION_PROPERTIES; out d2dDevice: ID2D1Device): HRESULT; stdcall;
  {$EXTERNALSYM D2D1CreateDevice}


  function D2D1CreateDeviceContext(dxgiSurface: IDXGISurface; creationProperties: PD2D1_CREATION_PROPERTIES; out d2dDeviceContext: ID2D1DeviceContext): HRESULT; stdcall;
  {$EXTERNALSYM D2D1CreateDeviceContext}

  function D2D1ConvertColorSpace(sourceColorSpace: D2D1_COLOR_SPACE; destinationColorSpace: D2D1_COLOR_SPACE; color: PD2D1_COLOR_F): D2D1_COLOR_F; stdcall;
  {$EXTERNALSYM D2D1ConvertColorSpace}

  procedure D2D1SinCos(angle: Single; s: PSingle; c: PSingle); stdcall;
  {$EXTERNALSYM D2D1SinCos}

  function D2D1Tan(angle: Single): Single; stdcall;
  {$EXTERNALSYM D2D1Tan}

  function D2D1Vec3Length(x: Single; y: Single; z: Single): Single; stdcall;
  {$EXTERNALSYM D2D1Vec3Length}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  D2D1lib = 'd2d1.dll';
  {$EXTERNALSYM D2D1lib}


{$WARN SYMBOL_PLATFORM OFF}
  function D2D1CreateDevice;        external D2D1lib name 'D2D1CreateDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function D2D1CreateDeviceContext; external D2D1lib name 'D2D1CreateDeviceContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function D2D1ConvertColorSpace;   external D2D1lib name 'D2D1ConvertColorSpace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure D2D1SinCos;             external D2D1lib name 'D2D1SinCos';
  function D2D1Tan;                 external D2D1lib name 'D2D1Tan';
  function D2D1Vec3Length;          external D2D1lib name 'D2D1Vec3Length';
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
