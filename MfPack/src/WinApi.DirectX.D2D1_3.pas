// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1_3.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 4.1.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
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
// Source: d2d1_3.h
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
unit WinApi.DirectX.D2D1_3;

  {$HPPEMIT '#include "d2d1_3.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WIC.WinCodec,
  WinApi.ActiveX.ObjIdlbase,
  {WinApi.DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DWrite_3,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1_2,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.D2D1Effects_2,
  WinApi.DirectX.D2D1Svg,
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGI;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type
  PIDXGISurface = ^IDXGISurface;

// Enums =======================================================================

type
  // Specifies the appearance of the ink nib (pen tip) as part of an
  // D2D1_INK_STYLE_PROPERTIES structure.
  PD2D1_INK_NIB_SHAPE = ^D2D1_INK_NIB_SHAPE;
  D2D1_INK_NIB_SHAPE = DWord;
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE}
const
  D2D1_INK_NIB_SHAPE_ROUND     = D2D1_INK_NIB_SHAPE(0);
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE_ROUND}
  D2D1_INK_NIB_SHAPE_SQUARE    = D2D1_INK_NIB_SHAPE(1);
  {$EXTERNALSYM D2D1_INK_NIB_SHAPE_SQUARE}
  //D2D1_INK_NIB_SHAPE_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the orientation of an image.
  PD2D1_ORIENTATION = ^D2D1_ORIENTATION;
  D2D1_ORIENTATION = DWord;
  {$EXTERNALSYM D2D1_ORIENTATION}
const
  D2D1_ORIENTATION_DEFAULT                             = D2D1_ORIENTATION(1);
  {$EXTERNALSYM D2D1_ORIENTATION_DEFAULT}
  D2D1_ORIENTATION_FLIP_HORIZONTAL                     = D2D1_ORIENTATION(2);
  {$EXTERNALSYM D2D1_ORIENTATION_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE180                 = D2D1_ORIENTATION(3);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE180}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE180_FLIP_HORIZONTAL = D2D1_ORIENTATION(4);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE180_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE90_FLIP_HORIZONTAL  = D2D1_ORIENTATION(5);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE90_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE270                 = D2D1_ORIENTATION(6);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE270}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE270_FLIP_HORIZONTAL = D2D1_ORIENTATION(7);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE270_FLIP_HORIZONTAL}
  D2D1_ORIENTATION_ROTATE_CLOCKWISE90                  = D2D1_ORIENTATION(8);
  {$EXTERNALSYM D2D1_ORIENTATION_ROTATE_CLOCKWISE90}
  //D2D1_ORIENTATION_FORCE_DWORD             = FORCEDWORD;

type
  // Option flags controlling how images sources are loaded during
  // CreateImageSourceFromWic.
  PD2D1_IMAGE_SOURCE_LOADING_OPTIONS = ^D2D1_IMAGE_SOURCE_LOADING_OPTIONS;
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS}
const
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_NONE}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_NONE            = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(0);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_RELEASE_SOURCE}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_RELEASE_SOURCE  = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(1);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_LOADING_OPTIONS_CACHE_ON_DEMAND}
  D2D1_IMAGE_SOURCE_LOADING_OPTIONS_CACHE_ON_DEMAND = D2D1_IMAGE_SOURCE_LOADING_OPTIONS(2);
  //D2D1_IMAGE_SOURCE_LOADING_OPTIONS_FORCE_DWORD   = FORCEDWORD;

type
  // Option flags controlling primary conversion performed by
  // CreateImageSourceFromDxgi); if any.
  PD2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS = ^D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS;
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS}
const
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_NONE                           = D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS(0);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_NONE}
  D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_LOW_QUALITY_PRIMARY_CONVERSION = D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS(1);
  {$EXTERNALSYM D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_LOW_QUALITY_PRIMARY_CONVERSION}
  //D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS_FORCE_DWORD          = FORCEDWORD;

type
  // Option flags for transformed image sources.
  PD2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS = ^D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS;
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS}
const
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_NONE              = D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS(0);
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_NONE}
  // Prevents the image source from being automatically scaled (by a ratio of the
  // context DPI divided by 96) while drawn.
  D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_DISABLE_DPI_SCALE = D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS(1);
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_DISABLE_DPI_SCALE}
  //D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies how to render gradient mesh edges.
  PD2D1_PATCH_EDGE_MODE = ^D2D1_PATCH_EDGE_MODE;
  D2D1_PATCH_EDGE_MODE = DWord;
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE}
const
  // Render this edge aliased.
  D2D1_PATCH_EDGE_MODE_ALIASED          = D2D1_PATCH_EDGE_MODE(0);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ALIASED}
  // Render this edge antialiased.
  D2D1_PATCH_EDGE_MODE_ANTIALIASED      = D2D1_PATCH_EDGE_MODE(1);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ANTIALIASED}
  // Render this edge aliased and inflated out slightly.
  D2D1_PATCH_EDGE_MODE_ALIASED_INFLATED = D2D1_PATCH_EDGE_MODE(2);
  {$EXTERNALSYM D2D1_PATCH_EDGE_MODE_ALIASED_INFLATED}
  //D2D1_PATCH_EDGE_MODE_FORCE_DWORD    = FORCEDWORD;

type
  PD2D1_SPRITE_OPTIONS = ^D2D1_SPRITE_OPTIONS;
  D2D1_SPRITE_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS}
const
  // Use default sprite rendering behavior.
  D2D1_SPRITE_OPTIONS_NONE                      = D2D1_SPRITE_OPTIONS(0);
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS_NONE}
  // Bitmap interpolation will be clamped to the sprite's source rectangle.
  D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE = D2D1_SPRITE_OPTIONS(1);
  {$EXTERNALSYM D2D1_SPRITE_OPTIONS_CLAMP_TO_SOURCE_RECTANGLE}
  //D2D1_SPRITE_OPTIONS_FORCE_DWORD         = FORCEDWORD;

type
  // Specifies the pixel snapping policy when rendering color bitmap glyphs.
  PD2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = ^D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION;
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = DWord;
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION}
const
  // Color bitmap glyph positions are snapped to the nearest pixel if the bitmap
  // resolution matches that of the device context.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(0);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT}
  // Color bitmap glyph positions are not snapped.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(1);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE}
  //D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_FORCE_DWORD = FORCEDWORD;

type
  // This determines what gamma is used for interpolation/blending.
  PD2D1_GAMMA1 = ^D2D1_GAMMA1;
  D2D1_GAMMA1 = DWord;
  {$EXTERNALSYM D2D1_GAMMA1}
const
  // Colors are manipulated in 2.2 gamma color space.
  D2D1_GAMMA1_G22     = D2D1_GAMMA_2_2;
  // Colors are manipulated in 1.0 gamma color space.
  D2D1_GAMMA1_G10     = D2D1_GAMMA_1_0;
  // Colors are manipulated in ST.2084 PQ gamma color space.
  D2D1_GAMMA1_G2084     = D2D1_GAMMA1(2);
  //D2D1_GAMMA1_FORCE_DWORD = FORCEDWORD;

type
  // Specifies which way a color profile is defined.
  PD2D1_COLOR_CONTEXT_TYPE = ^D2D1_COLOR_CONTEXT_TYPE;
  D2D1_COLOR_CONTEXT_TYPE = DWord;
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE}
const
  D2D1_COLOR_CONTEXT_TYPE_ICC     = D2D1_COLOR_CONTEXT_TYPE(0);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_ICC}
  D2D1_COLOR_CONTEXT_TYPE_SIMPLE  = D2D1_COLOR_CONTEXT_TYPE(1);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_SIMPLE}
  D2D1_COLOR_CONTEXT_TYPE_DXGI    = D2D1_COLOR_CONTEXT_TYPE(2);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_DXGI}
  //D2D1_COLOR_CONTEXT_TYPE_FORCE_DWORD = FORCEDWORD;

// =============================================================================

type
  // Properties of a transformed image source.
  PD2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES = ^D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES;
  D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES = record
    // The orientation at which the image source is drawn.
    orientation: D2D1_ORIENTATION;
    // The horizontal scale factor at which the image source is drawn.
    scaleX: Single;
    // The vertical scale factor at which the image source is drawn.
    scaleY: Single;
    // The interpolation mode used when the image source is drawn.  This is ignored if
    // the image source is drawn using the DrawImage method, or using an image brush.
    interpolationMode: D2D1_INTERPOLATION_MODE;
    // Option flags.
    options: D2D1_TRANSFORMED_IMAGE_SOURCE_OPTIONS;
  end;
  {$EXTERNALSYM D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES}


  // Represents a point, radius pair that makes up part of a D2D1_INK_BEZIER_SEGMENT.
  PD2D1_INK_POINT = ^D2D1_INK_POINT;
  D2D1_INK_POINT = record
    x: Single;
    y: Single;
    radius: Single;
  end;
  {$EXTERNALSYM D2D1_INK_POINT}


  // Represents a Bezier segment to be used in the creation of an ID2D1Ink object.
  // This structure differs from D2D1_BEZIER_SEGMENT in that it is composed of
  // D2D1_INK_POINT s, which contain a radius in addition to x- and y-coordinates.
  PD2D1_INK_BEZIER_SEGMENT = ^D2D1_INK_BEZIER_SEGMENT;
  D2D1_INK_BEZIER_SEGMENT = record
    point1: D2D1_INK_POINT;
    point2: D2D1_INK_POINT;
    point3: D2D1_INK_POINT;
  end;
  {$EXTERNALSYM D2D1_INK_BEZIER_SEGMENT}


  // Defines the general pen tip shape and the transform used in an ID2D1InkStyle
  // object.
  PD2D1_INK_STYLE_PROPERTIES = ^D2D1_INK_STYLE_PROPERTIES;
  D2D1_INK_STYLE_PROPERTIES = record
    // The general shape of the nib used to draw a given ink object.
    nibShape: D2D1_INK_NIB_SHAPE;
    // The transform applied to shape of the nib. _31 and _32 are ignored.
    nibTransform: D2D1_MATRIX_3X2_F;
  end;
  {$EXTERNALSYM D2D1_INK_STYLE_PROPERTIES}


  // Represents a tensor patch with 16 control points, 4 corner colors, and boundary
  // flags. An ID2D1GradientMesh is made up of 1 or more gradient mesh patches. Use
  // the GradientMeshPatch function or the GradientMeshPatchFromCoonsPatch function
  // to create one.
  PD2D1_GRADIENT_MESH_PATCH = ^D2D1_GRADIENT_MESH_PATCH;
  D2D1_GRADIENT_MESH_PATCH = record
    // The gradient mesh patch control point at position 00.
    point00: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 01.
    point01: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 02.
    point02: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 03.
    point03: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 10.
    point10: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 11.
    point11: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 12.
    point12: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 13.
    point13: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 20.
    point20: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 21.
    point21: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 22.
    point22: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 23.
    point23: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 30.
    point30: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 31.
    point31: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 32.
    point32: D2D1_POINT_2F;
    // The gradient mesh patch control point at position 33.
    point33: D2D1_POINT_2F;
    // The color associated with control point at position 00.
    color00: D2D1_COLOR_F;
    // The color associated with control point at position 03.
    color03: D2D1_COLOR_F;
    // The color associated with control point at position 30.
    color30: D2D1_COLOR_F;
    // The color associated with control point at position 33.
    color33: D2D1_COLOR_F;
    // The edge mode for the top edge of the patch.
    topEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the left edge of the patch.
    leftEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the bottom edge of the patch.
    bottomEdgeMode: D2D1_PATCH_EDGE_MODE;
    // The edge mode for the right edge of the patch.
    rightEdgeMode: D2D1_PATCH_EDGE_MODE;
  end;
  {$EXTERNALSYM D2D1_GRADIENT_MESH_PATCH}


  // Simple description of a color space.
  PD2D1_SIMPLE_COLOR_PROFILE = ^D2D1_SIMPLE_COLOR_PROFILE;
  D2D1_SIMPLE_COLOR_PROFILE = record
    // The XY coordinates of the red primary in CIEXYZ space.
    redPrimary: D2D1_POINT_2F;
    // The XY coordinates of the green primary in CIEXYZ space.
    greenPrimary: D2D1_POINT_2F;
    // The XY coordinates of the blue primary in CIEXYZ space.
    bluePrimary: D2D1_POINT_2F;
    // The X/Z tristimulus values for the whitepoint, normalized for relative
    // luminance.
    whitePointXZ: D2D1_POINT_2F;
    // The gamma encoding to use for this color space.
    gamma: D2D1_GAMMA1;
  end;
  {$EXTERNALSYM D2D1_SIMPLE_COLOR_PROFILE}
  //
  // INTERFACES ////////////////////////////////////////////////////////////////
  //

  // Interface ID2D1InkStyle
  // =======================
  // Represents a collection of style properties to be used by methods like
  // ID2D1DeviceContext2.DrawInk when rendering ink. The ink style defines the nib
  // (pen tip) shape and transform.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1InkStyle);'}
  {$EXTERNALSYM ID2D1InkStyle}
  ID2D1InkStyle = interface(ID2D1Resource)
  ['{bae8b344-23fc-4071-8cb5-d05d6f073848}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    procedure SetNibTransform(transform: PD2D1_MATRIX_3X2_F); stdcall; overload;
    procedure GetNibTransform(out transform: D2D1_MATRIX_3X2_F); stdcall; overload;
    procedure SetNibShape(nibShape: D2D1_INK_NIB_SHAPE); stdcall; overload;
    function GetNibShape(): D2D1_INK_NIB_SHAPE; stdcall; overload;


  end;
  IID_ID2D1InkStyle = ID2D1InkStyle;
  {$EXTERNALSYM IID_ID2D1InkStyle}


  // Interface ID2D1Ink
  // ==================
  // Represents a single continuous stroke of variable-width ink, as defined by a
  // series of Bezier segments and widths.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Ink);'}
  {$EXTERNALSYM ID2D1Ink}
  ID2D1Ink = interface(ID2D1Resource)
  ['{b499923b-7029-478f-a8b3-432c7c5f5312}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    procedure SetStartPoint(startPoint: PD2D1_INK_POINT); stdcall; overload;
    procedure GetStartPoint(out point: D2D1_INK_POINT); stdcall; overload;
    function AddSegments(segments: PD2D1_INK_BEZIER_SEGMENT; segmentsCount: UINT32): HRESULT; stdcall; overload;
    function RemoveSegmentsAtEnd(segmentsCount: UINT32): HRESULT; stdcall; overload;
    function SetSegments(startSegment: UINT32; segments: PD2D1_INK_BEZIER_SEGMENT; segmentsCount: UINT32): HRESULT; stdcall; overload;
    function SetSegmentAtEnd(segment: PD2D1_INK_BEZIER_SEGMENT): HRESULT; stdcall; overload;
    function GetSegmentCount(): UINT32; stdcall; overload;
    function GetSegments(startSegment: UINT32; segments: PD2D1_INK_BEZIER_SEGMENT; segmentsCount: UINT32): HRESULT; stdcall; overload;
    function StreamAsGeometry(inkStyle: ID2D1InkStyle; worldTransform: PD2D1_MATRIX_3X2_F; flatteningTolerance: Single; geometrySink: ID2D1SimplifiedGeometrySink): HRESULT; stdcall; overload;
    function GetBounds(inkStyle: ID2D1InkStyle; worldTransform: PD2D1_MATRIX_3X2_F; out bounds: D2D1_RECT_F): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Ink = ID2D1Ink;
  {$EXTERNALSYM IID_ID2D1Ink}


  // Interface ID2D1GradientMesh
  // ===========================
  // Represents a device-dependent representation of a gradient mesh composed of
  // patches. Use the ID2D1DeviceContext2::CreateGradientMesh method to create an
  // instance of ID2D1GradientMesh.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientMesh);'}
  {$EXTERNALSYM ID2D1GradientMesh}
  ID2D1GradientMesh = interface(ID2D1Resource)
  ['{f292e401-c050-4cde-83d7-04962d3b23c2}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function GetPatchCount(): UINT32; stdcall; overload;
    function GetPatches(startIndex: UINT32; patches: PD2D1_GRADIENT_MESH_PATCH; patchesCount: UINT32): HRESULT; stdcall; overload;


  end;
  IID_ID2D1GradientMesh = ID2D1GradientMesh;
  {$EXTERNALSYM IID_ID2D1GradientMesh}


  // Interface ID2D1ImageSource
  // ==========================
  // Represents a producer of pixels that can fill an arbitrary 2D plane.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageSource);'}
  {$EXTERNALSYM ID2D1ImageSource}
  ID2D1ImageSource = interface(ID2D1Image)
  ['{c9b664e5-74a1-4378-9ac2-eefc37a3f4d8}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function OfferResources(): HRESULT; stdcall; overload;
    function TryReclaimResources(out resourcesDiscarded: BOOL): HRESULT; stdcall; overload;


  end;
  IID_ID2D1ImageSource = ID2D1ImageSource;
  {$EXTERNALSYM IID_ID2D1ImageSource}


  // Interface ID2D1ImageSourceFromWic
  // =================================
  // Produces 2D pixel data that has been sourced from WIC.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageSourceFromWic);'}
  {$EXTERNALSYM ID2D1ImageSourceFromWic}
  ID2D1ImageSourceFromWic = interface(ID2D1ImageSource)
  ['{77395441-1c8f-4555-8683-f50dab0fe792}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function EnsureCached(rectangleToFill: PD2D1_RECT_U): HRESULT; stdcall; overload;
    function TrimCache(rectangleToPreserve: PD2D1_RECT_U): HRESULT; stdcall; overload;
    procedure GetSource(out wicBitmapSource: IWICBitmapSource); stdcall; overload;


  end;
  IID_ID2D1ImageSourceFromWic = ID2D1ImageSourceFromWic;
  {$EXTERNALSYM IID_ID2D1ImageSourceFromWic}


  // Interface ID2D1TransformedImageSource
  // =====================================
  // Represents an image source which shares resources with an original image source.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TransformedImageSource);'}
  {$EXTERNALSYM ID2D1TransformedImageSource}
  ID2D1TransformedImageSource = interface(ID2D1Image)
  ['{7f1f79e5-2796-416c-8f55-700f911445e5}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    procedure GetSource(out imageSource: ID2D1ImageSource); stdcall; overload;
    procedure GetProperties(out properties: D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES); stdcall; overload;


  end;
  IID_ID2D1TransformedImageSource = ID2D1TransformedImageSource;
  {$EXTERNALSYM IID_ID2D1TransformedImageSource}


  // Interface ID2D1LookupTable3D
  // ============================
  // A container for 3D lookup table data that can be passed to the LookupTable3D
  // effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1LookupTable3D);'}
  {$EXTERNALSYM ID2D1LookupTable3D}
  ID2D1LookupTable3D = interface(ID2D1Resource)
  ['{53dd9855-a3b0-4d5b-82e1-26e25c5e5797}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    


  end;
  IID_ID2D1LookupTable3D = ID2D1LookupTable3D;
  {$EXTERNALSYM IID_ID2D1LookupTable3D}


  // Interface ID2D1DeviceContext2
  // =============================
  // This interface performs all the same functions as the ID2D1DeviceContext1
  // interface, plus it enables functionality such as ink rendering, gradient mesh
  // rendering, and improved image loading.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext2);'}
  {$EXTERNALSYM ID2D1DeviceContext2}
  ID2D1DeviceContext2 = interface(ID2D1DeviceContext1)
  ['{394ea6a3-0c34-4321-950b-6ca20f0be6c7}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateInk(startPoint: PD2D1_INK_POINT; out ink: ID2D1Ink): HRESULT; stdcall; overload;
    function CreateInkStyle(inkStyleProperties: PD2D1_INK_STYLE_PROPERTIES; out inkStyle: ID2D1InkStyle): HRESULT; stdcall; overload;
    function CreateGradientMesh(patches: PD2D1_GRADIENT_MESH_PATCH; patchesCount: UINT32; out gradientMesh: ID2D1GradientMesh): HRESULT; stdcall; overload;
    function CreateImageSourceFromWic(wicBitmapSource: IWICBitmapSource; loadingOptions: D2D1_IMAGE_SOURCE_LOADING_OPTIONS; alphaMode: D2D1_ALPHA_MODE; out imageSource: ID2D1ImageSourceFromWic): HRESULT; stdcall; overload;
    function CreateLookupTable3D(precision: D2D1_BUFFER_PRECISION; extents: PUINT32; data: PByte; dataCount: UINT32; strides: PUINT32; out lookupTable: ID2D1LookupTable3D): HRESULT; stdcall; overload;
    function CreateImageSourceFromDxgi(surfaces: PIDXGISurface; surfaceCount: UINT32; colorSpace: DXGI_COLOR_SPACE_TYPE; options: D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS; out imageSource: ID2D1ImageSource): HRESULT; stdcall; overload;
    function GetGradientMeshWorldBounds(gradientMesh: ID2D1GradientMesh; out pBounds: D2D1_RECT_F): HRESULT; stdcall; overload;
    procedure DrawInk(ink: ID2D1Ink; brush: ID2D1Brush; inkStyle: ID2D1InkStyle); stdcall; overload;
    procedure DrawGradientMesh(gradientMesh: ID2D1GradientMesh); stdcall; overload;
    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile; destinationRectangle: PD2D1_RECT_F; sourceRectangle: PD2D1_RECT_F); stdcall; overload;
    function CreateTransformedImageSource(imageSource: ID2D1ImageSource; properties: PD2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES; out transformedImageSource: ID2D1TransformedImageSource): HRESULT; stdcall; overload;


  end;
  IID_ID2D1DeviceContext2 = ID2D1DeviceContext2;
  {$EXTERNALSYM IID_ID2D1DeviceContext2}


  // Interface ID2D1Device2
  // ======================
  // Represents a resource domain whose objects and device contexts can be used
  // together. This interface performs all the same functions as the existing
  // ID2D1Device1 interface. It also enables the creation of ID2D1DeviceContext2
  // objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device2);'}
  {$EXTERNALSYM ID2D1Device2}
  ID2D1Device2 = interface(ID2D1Device1)
  ['{a44472e1-8dfb-4e60-8492-6e2861c9ca8b}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext2: ID2D1DeviceContext2): HRESULT; stdcall; overload;
    procedure FlushDeviceContexts(bitmap: ID2D1Bitmap); stdcall; overload;
    function GetDxgiDevice(out dxgiDevice: IDXGIDevice): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Device2 = ID2D1Device2;
  {$EXTERNALSYM IID_ID2D1Device2}


  // Interface ID2D1Factory3
  // =======================
  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device2 objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory3);'}
  {$EXTERNALSYM ID2D1Factory3}
  ID2D1Factory3 = interface(ID2D1Factory2)
  ['{0869759f-4f00-413f-b03e-2bda45404d0f}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice2: ID2D1Device2): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Factory3 = ID2D1Factory3;
  {$EXTERNALSYM IID_ID2D1Factory3}


  // Interface ID2D1CommandSink2
  // ===========================
  // This interface performs all the same functions as the existing ID2D1CommandSink1
  // interface. It also enables access to ink rendering and gradient mesh rendering.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink2);'}
  {$EXTERNALSYM ID2D1CommandSink2}
  ID2D1CommandSink2 = interface(ID2D1CommandSink1)
  ['{3bab440e-417e-47df-a2e2-bc0be6a00916}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function DrawInk(ink: ID2D1Ink; brush: ID2D1Brush; inkStyle: ID2D1InkStyle): HRESULT; stdcall; overload;
    function DrawGradientMesh(gradientMesh: ID2D1GradientMesh): HRESULT; stdcall; overload;
    function DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile; destinationRectangle: PD2D1_RECT_F; sourceRectangle: PD2D1_RECT_F): HRESULT; stdcall; overload;


  end;
  IID_ID2D1CommandSink2 = ID2D1CommandSink2;
  {$EXTERNALSYM IID_ID2D1CommandSink2}


  // Interface ID2D1GdiMetafile1
  // ===========================
  // Interface encapsulating a GDI/GDI+ metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafile1);'}
  {$EXTERNALSYM ID2D1GdiMetafile1}
  ID2D1GdiMetafile1 = interface(ID2D1GdiMetafile)
  ['{2e69f9e8-dd3f-4bf9-95ba-c04f49d788df}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function GetDpi(out dpiX: Single; out dpiY: Single): HRESULT; stdcall; overload;
    function GetSourceBounds(out bounds: D2D1_RECT_F): HRESULT; stdcall; overload;


  end;
  IID_ID2D1GdiMetafile1 = ID2D1GdiMetafile1;
  {$EXTERNALSYM IID_ID2D1GdiMetafile1}


  // Interface ID2D1GdiMetafileSink1
  // ===============================
  // User-implementable interface for introspecting on a metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafileSink1);'}
  {$EXTERNALSYM ID2D1GdiMetafileSink1}
  ID2D1GdiMetafileSink1 = interface(ID2D1GdiMetafileSink)
  ['{fd0ecb6b-91e6-411e-8655-395e760f91b4}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function ProcessRecord(recordType: DWORD; recordData: Pointer; recordDataSize: DWORD; flags: UINT32): HRESULT; stdcall; overload;


  end;
  IID_ID2D1GdiMetafileSink1 = ID2D1GdiMetafileSink1;
  {$EXTERNALSYM IID_ID2D1GdiMetafileSink1}


// #if NTDDI_VERSION >= NTDDI_WIN10_TH2
  // Interface ID2D1SpriteBatch
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SpriteBatch);'}
  {$EXTERNALSYM ID2D1SpriteBatch}
  ID2D1SpriteBatch = interface(ID2D1Resource)
  ['{4dc583bf-3a10-438a-8722-e9765224f1f1}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function AddSprites(spriteCount: UINT32; destinationRectangles: PD2D1_RECT_F; sourceRectangles: PD2D1_RECT_U; colors: PD2D1_COLOR_F; transforms: PD2D1_MATRIX_3X2_F; destinationRectanglesStride: UINT32; sourceRectanglesStride: UINT32; colorsStride: UINT32; transformsStride: UINT32): HRESULT; stdcall; overload;
    function SetSprites(startIndex: UINT32; spriteCount: UINT32; destinationRectangles: PD2D1_RECT_F; sourceRectangles: PD2D1_RECT_U; colors: PD2D1_COLOR_F; transforms: PD2D1_MATRIX_3X2_F; destinationRectanglesStride: UINT32; sourceRectanglesStride: UINT32; colorsStride: UINT32; transformsStride: UINT32): HRESULT; stdcall; overload;
    function GetSprites(startIndex: UINT32; spriteCount: UINT32; destinationRectangles: PD2D1_RECT_F; sourceRectangles: PD2D1_RECT_U; colors: PD2D1_COLOR_F; transforms: PD2D1_MATRIX_3X2_F): HRESULT; stdcall; overload;
    function GetSpriteCount(): UINT32; stdcall; overload;
    procedure Clear(); stdcall; overload;


  end;
  IID_ID2D1SpriteBatch = ID2D1SpriteBatch;
  {$EXTERNALSYM IID_ID2D1SpriteBatch}


  // Interface ID2D1DeviceContext3
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext3);'}
  {$EXTERNALSYM ID2D1DeviceContext3}
  ID2D1DeviceContext3 = interface(ID2D1DeviceContext2)
  ['{235a7496-8351-414c-bcd4-6672ab2d8e00}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateSpriteBatch(out spriteBatch: ID2D1SpriteBatch): HRESULT; stdcall; overload;
    procedure DrawSpriteBatch(spriteBatch: ID2D1SpriteBatch; startIndex: UINT32; spriteCount: UINT32; bitmap: ID2D1Bitmap; interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE; spriteOptions: D2D1_SPRITE_OPTIONS); stdcall; overload;


  end;
  IID_ID2D1DeviceContext3 = ID2D1DeviceContext3;
  {$EXTERNALSYM IID_ID2D1DeviceContext3}


  // Interface ID2D1Device3
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device3);'}
  {$EXTERNALSYM ID2D1Device3}
  ID2D1Device3 = interface(ID2D1Device2)
  ['{852f2087-802c-4037-ab60-ff2e7ee6fc01}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext3: ID2D1DeviceContext3): HRESULT; stdcall; overload;

  end;
  IID_ID2D1Device3 = ID2D1Device3;
  {$EXTERNALSYM IID_ID2D1Device3}



  // Interface ID2D1Factory4
  // =======================
  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device3 objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory4);'}
  {$EXTERNALSYM ID2D1Factory4}
  ID2D1Factory4 = interface(ID2D1Factory3)
  ['{bd4ec2d2-0662-4bee-ba8e-6f29f032e096}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice3: ID2D1Device3): HRESULT; stdcall; overload;

  end;
  IID_ID2D1Factory4 = ID2D1Factory4;
  {$EXTERNALSYM IID_ID2D1Factory4}


  // Interface ID2D1CommandSink3
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink3);'}
  {$EXTERNALSYM ID2D1CommandSink3}
  ID2D1CommandSink3 = interface(ID2D1CommandSink2)
  ['{18079135-4cf3-4868-bc8e-06067e6d242d}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function DrawSpriteBatch(spriteBatch: ID2D1SpriteBatch; startIndex: UINT32; spriteCount: UINT32; bitmap: ID2D1Bitmap; interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE; spriteOptions: D2D1_SPRITE_OPTIONS): HRESULT; stdcall; overload;


  end;
  IID_ID2D1CommandSink3 = ID2D1CommandSink3;
  {$EXTERNALSYM IID_ID2D1CommandSink3}


//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_TH2
//#if NTDDI_VERSION >= NTDDI_WIN10_RS1


  // Interface ID2D1SvgGlyphStyle
  // ============================
  // This object supplies the values for context-fill, context-stroke, and
  // context-value that are used when rendering SVG glyphs.
  //
  PID2D1SvgGlyphStyle = ^ID2D1SvgGlyphStyle;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgGlyphStyle);'}
  {$EXTERNALSYM ID2D1SvgGlyphStyle}
  ID2D1SvgGlyphStyle = interface(ID2D1Resource)
  ['{af671749-d241-4db8-8e41-dcc2e5c1a438}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function SetFill(brush: ID2D1Brush): HRESULT; stdcall; overload;
    procedure GetFill(out brush: ID2D1Brush); stdcall; overload;
    function SetStroke(brush: ID2D1Brush; strokeWidth: Single; dashes: PSingle; dashesCount: UINT32; dashOffset: Single): HRESULT; stdcall; overload;
    function GetStrokeDashesCount(): UINT32; stdcall; overload;
    procedure GetStroke(out brush: ID2D1Brush; out strokeWidth: Single; dashes: PSingle; dashesCount: UINT32; out dashOffset: Single); stdcall; overload;


  end;
  IID_ID2D1SvgGlyphStyle = ID2D1SvgGlyphStyle;
  {$EXTERNALSYM IID_ID2D1SvgGlyphStyle}


  // Interface ID2D1DeviceContext4
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext4);'}
  {$EXTERNALSYM ID2D1DeviceContext4}
  ID2D1DeviceContext4 = interface(ID2D1DeviceContext3)
  ['{8c427831-3d90-4476-b647-c4fae349e4db}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateSvgGlyphStyle(out svgGlyphStyle: ID2D1SvgGlyphStyle): HRESULT; stdcall; overload;
    procedure DrawText(_string: PWideChar; stringLength: UINT32; textFormat: IDWriteTextFormat; layoutRect: PD2D1_RECT_F; defaultFillBrush: ID2D1Brush; svgGlyphStyle: ID2D1SvgGlyphStyle; colorPaletteIndex: UINT32; options: D2D1_DRAW_TEXT_OPTIONS; measuringMode: DWRITE_MEASURING_MODE); stdcall; overload;
    procedure DrawTextLayout(origin: D2D1_POINT_2F; textLayout: IDWriteTextLayout; defaultFillBrush: ID2D1Brush; svgGlyphStyle: ID2D1SvgGlyphStyle; colorPaletteIndex: UINT32; options: D2D1_DRAW_TEXT_OPTIONS); stdcall; overload;
    procedure DrawColorBitmapGlyphRun(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS; baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; measuringMode: DWRITE_MEASURING_MODE; bitmapSnapOption: D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION); stdcall; overload;
    procedure DrawSvgGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; defaultFillBrush: ID2D1Brush; svgGlyphStyle: ID2D1SvgGlyphStyle; colorPaletteIndex: UINT32; measuringMode: DWRITE_MEASURING_MODE); stdcall; overload;
    function GetColorBitmapGlyphImage(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS; glyphOrigin: D2D1_POINT_2F; fontFace: IDWriteFontFace; fontEmSize: Single; glyphIndex: UINT16; isSideways: BOOL; worldTransform: PD2D1_MATRIX_3X2_F; dpiX: Single; dpiY: Single; out glyphTransform: D2D1_MATRIX_3X2_F; out glyphImage: ID2D1Image): HRESULT; stdcall; overload;
    function GetSvgGlyphImage(glyphOrigin: D2D1_POINT_2F; fontFace: IDWriteFontFace; fontEmSize: Single; glyphIndex: UINT16; isSideways: BOOL; worldTransform: PD2D1_MATRIX_3X2_F; defaultFillBrush: ID2D1Brush; svgGlyphStyle: ID2D1SvgGlyphStyle; colorPaletteIndex: UINT32; out glyphTransform: D2D1_MATRIX_3X2_F; out glyphImage: ID2D1CommandList): HRESULT; stdcall; overload;


  end;
  IID_ID2D1DeviceContext4 = ID2D1DeviceContext4;
  {$EXTERNALSYM IID_ID2D1DeviceContext4}


  // Interface ID2D1Device4
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device4);'}
  {$EXTERNALSYM ID2D1Device4}
  ID2D1Device4 = interface(ID2D1Device3)
  ['{d7bdb159-5683-4a46-bc9c-72dc720b858b}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext4: ID2D1DeviceContext4): HRESULT; stdcall; overload;
    procedure SetMaximumColorGlyphCacheMemory(maximumInBytes: UINT64); stdcall; overload;
    function GetMaximumColorGlyphCacheMemory(): UINT64; stdcall; overload;


  end;
  IID_ID2D1Device4 = ID2D1Device4;
  {$EXTERNALSYM IID_ID2D1Device4}



  // Interface ID2D1Factory5
  // =======================
  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device4 objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory5);'}
  {$EXTERNALSYM ID2D1Factory5}
  ID2D1Factory5 = interface(ID2D1Factory4)
  ['{c4349994-838e-4b0f-8cab-44997d9eeacc}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice4: ID2D1Device4): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Factory5 = ID2D1Factory5;
  {$EXTERNALSYM IID_ID2D1Factory5}


//#endif // #if NTDDI_VERSION >= NTDDI_WIN10_RS1
//#if NTDDI_VERSION >= NTDDI_WIN10_RS2

  // Interface ID2D1CommandSink4
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink4);'}
  {$EXTERNALSYM ID2D1CommandSink4}
  ID2D1CommandSink4 = interface(ID2D1CommandSink3)
  ['{c78a6519-40d6-4218-b2de-beeeb744bb3e}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function SetPrimitiveBlend2(primitiveBlend: D2D1_PRIMITIVE_BLEND): HRESULT; stdcall; overload;


  end;
  IID_ID2D1CommandSink4 = ID2D1CommandSink4;
  {$EXTERNALSYM IID_ID2D1CommandSink4}


  // Interface ID2D1ColorContext1
  // ============================
  // Represents a color context to be used with the Color Management Effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ColorContext1);'}
  {$EXTERNALSYM ID2D1ColorContext1}
  ID2D1ColorContext1 = interface(ID2D1ColorContext)
  ['{1ab42875-c57f-4be9-bd85-9cd78d6f55ee}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function GetColorContextType(): D2D1_COLOR_CONTEXT_TYPE; stdcall; overload;
    function GetDXGIColorSpace(): DXGI_COLOR_SPACE_TYPE; stdcall; overload;
    function GetSimpleColorProfile(out simpleProfile: D2D1_SIMPLE_COLOR_PROFILE): HRESULT; stdcall; overload;


  end;
  IID_ID2D1ColorContext1 = ID2D1ColorContext1;
  {$EXTERNALSYM IID_ID2D1ColorContext1}


  // Interface ID2D1DeviceContext5
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext5);'}
  {$EXTERNALSYM ID2D1DeviceContext5}
  ID2D1DeviceContext5 = interface(ID2D1DeviceContext4)
  ['{7836d248-68cc-4df6-b9e8-de991bf62eb7}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateSvgDocument(inputXmlStream: IStream; viewportSize: D2D1_SIZE_F; out svgDocument: ID2D1SvgDocument): HRESULT; stdcall; overload;
    procedure DrawSvgDocument(svgDocument: ID2D1SvgDocument); stdcall; overload;
    function CreateColorContextFromDxgiColorSpace(colorSpace: DXGI_COLOR_SPACE_TYPE; out colorContext: ID2D1ColorContext1): HRESULT; stdcall; overload;
    function CreateColorContextFromSimpleColorProfile(simpleProfile: PD2D1_SIMPLE_COLOR_PROFILE; out colorContext: ID2D1ColorContext1): HRESULT; stdcall; overload;

  end;
  IID_ID2D1DeviceContext5 = ID2D1DeviceContext5;
  {$EXTERNALSYM IID_ID2D1DeviceContext5}


  // Interface ID2D1Device5
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device5);'}
  {$EXTERNALSYM ID2D1Device5}
  ID2D1Device5 = interface(ID2D1Device4)
  ['{d55ba0a4-6405-4694-aef5-08ee1a4358b4}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext5: ID2D1DeviceContext5): HRESULT; stdcall; overload;

  end; // interface ID2D1Device5


  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device5 objects.
  ID2D1Factory6 = interface(ID2D1Factory5)
  ['{f9976f46-f642-44c1-97ca-da32ea2a2635}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice5: ID2D1Device5): HRESULT; stdcall; overload;

  end;
  IID_ID2D1Device5 = ID2D1Device5;
  {$EXTERNALSYM IID_ID2D1Device5}


//#if NTDDI_VERSION >= NTDDI_WIN10_RS3

  // Interface ID2D1CommandSink5
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink5);'}
  {$EXTERNALSYM ID2D1CommandSink5}
  ID2D1CommandSink5 = interface(ID2D1CommandSink4)
  ['{7047dd26-b1e7-44a7-959a-8349e2144fa8}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function BlendImage(image: ID2D1Image; blendMode: D2D1_BLEND_MODE; targetOffset: PD2D1_POINT_2F; imageRectangle: PD2D1_RECT_F; interpolationMode: D2D1_INTERPOLATION_MODE): HRESULT; stdcall; overload;


  end;
  IID_ID2D1CommandSink5 = ID2D1CommandSink5;
  {$EXTERNALSYM IID_ID2D1CommandSink5}


  // Interface ID2D1DeviceContext6
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext6);'}
  {$EXTERNALSYM ID2D1DeviceContext6}
  ID2D1DeviceContext6 = interface(ID2D1DeviceContext5)
  ['{985f7e37-4ed0-4a19-98a3-15b0edfde306}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    procedure BlendImage(image: ID2D1Image; blendMode: D2D1_BLEND_MODE; targetOffset: PD2D1_POINT_2F; imageRectangle: PD2D1_RECT_F; interpolationMode: D2D1_INTERPOLATION_MODE); stdcall; overload;


  end;
  IID_ID2D1DeviceContext6 = ID2D1DeviceContext6;
  {$EXTERNALSYM IID_ID2D1DeviceContext6}


  // Interface ID2D1Device6
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device6);'}
  {$EXTERNALSYM ID2D1Device6}
  ID2D1Device6 = interface(ID2D1Device5)
  ['{7bfef914-2d75-4bad-be87-e18ddb077b6d}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext6: ID2D1DeviceContext6): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Device6 = ID2D1Device6;
  {$EXTERNALSYM IID_ID2D1Device6}


  // ID2D1Device6 objects.
  // Interface ID2D1Factory7
  // =======================
  // Creates Direct2D resources. This interface also enables the creation of
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory7);'}
  {$EXTERNALSYM ID2D1Factory7}
  ID2D1Factory7 = interface(ID2D1Factory6)
  ['{bdc2bdd3-b96c-4de6-bdf7-99d4745454de}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice6: ID2D1Device6): HRESULT; stdcall; overload;

  end;
  IID_ID2D1Factory7 = ID2D1Factory7;
  {$EXTERNALSYM IID_ID2D1Factory7}


// May 2024 update  NTDDI_VERSION >= NTDDI_WIN10_CU ////////////////////////////


  DWRITE_PAINT_FEATURE_LEVEL = WinApi.DirectX.DWrite_3.DWRITE_PAINT_FEATURE_LEVEL;

  /// <summary>
  /// Get the maximum paint feature level supported by DrawPaintGlyphRun.
  /// </summary>
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext7);'}
  {$EXTERNALSYM ID2D1DeviceContext7}
  ID2D1DeviceContext7 = interface(ID2D1DeviceContext6)
  ['{ec891cf7-9b69-4851-9def-4e0915771e62}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function GetPaintFeatureLevel(): DWRITE_PAINT_FEATURE_LEVEL; stdcall; overload;
    procedure DrawPaintGlyphRun(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; defaultFillBrush: ID2D1Brush; colorPaletteIndex: UINT32; measuringMode: DWRITE_MEASURING_MODE); stdcall; overload;
    procedure DrawGlyphRunWithColorSupport(baselineOrigin: D2D1_POINT_2F; glyphRun: PDWRITE_GLYPH_RUN; glyphRunDescription: PDWRITE_GLYPH_RUN_DESCRIPTION; foregroundBrush: ID2D1Brush; svgGlyphStyle: ID2D1SvgGlyphStyle; colorPaletteIndex: UINT32; measuringMode: DWRITE_MEASURING_MODE; bitmapSnapOption: D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION); stdcall; overload;


  end;
  IID_ID2D1DeviceContext7 = ID2D1DeviceContext7;
  {$EXTERNALSYM IID_ID2D1DeviceContext7}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device7);'}
  {$EXTERNALSYM ID2D1Device7}
  ID2D1Device7 = interface(ID2D1Device6)
  ['{f07c8968-dd4e-4ba6-9cbd-eb6d3752dcbb}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS; out deviceContext: ID2D1DeviceContext7): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Device7 = ID2D1Device7;
  {$EXTERNALSYM IID_ID2D1Device7}


  /// <summary>
  /// Creates Direct2D resources. This interface also enables the creation of
  /// ID2D1Device7 objects.
  /// </summary>
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device7);'}
  {$EXTERNALSYM ID2D1Device7}
  ID2D1Factory8 = interface(ID2D1Factory7)
  ['{677c9311-f36d-4b1f-ae86-86d1223ffd3a}']
    // SDK native virtual methods; inline C++ helpers are excluded.
    function CreateDevice(dxgiDevice: IDXGIDevice; out d2dDevice6: ID2D1Device7): HRESULT; stdcall; overload;


  end;
  IID_ID2D1Factory8 = ID2D1Factory8;
  {$EXTERNALSYM IID_ID2D1Factory8}

// End NTDDI_VERSION >= NTDDI_WIN10_CU /////////////////////////////////////////

// #if NTDDI_VERSION >= NTDDI_WINTHRESHOLD

    // Returns the interior points for a gradient mesh patch based on the points defining a Coons patch.
    // Note
    //   This function is called by the GradientMeshPatchFromCoonsPatch function and
    //   is not intended to be used directly.
    // Minimum supported client	Windows 10
    procedure D2D1GetGradientMeshInteriorPointsFromCoonsPatch(pPoint0: PD2D1_POINT_2F; pPoint1: PD2D1_POINT_2F; pPoint2: PD2D1_POINT_2F; pPoint3: PD2D1_POINT_2F; pPoint4: PD2D1_POINT_2F; pPoint5: PD2D1_POINT_2F; pPoint6: PD2D1_POINT_2F; pPoint7: PD2D1_POINT_2F; pPoint8: PD2D1_POINT_2F; pPoint9: PD2D1_POINT_2F; pPoint10: PD2D1_POINT_2F; pPoint11: PD2D1_POINT_2F; out pTensorPoint11: D2D1_POINT_2F; out pTensorPoint12: D2D1_POINT_2F; out pTensorPoint21: D2D1_POINT_2F; out pTensorPoint22: D2D1_POINT_2F); stdcall;
    {$EXTERNALSYM D2D1GetGradientMeshInteriorPointsFromCoonsPatch}

//#endif // #if NTDDI_VERSION >= NTDDI_WINTHRESHOLD


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation

const
  D2D1_3Lib = 'D2d1.dll';

{$WARN SYMBOL_PLATFORM OFF}

procedure D2D1GetGradientMeshInteriorPointsFromCoonsPatch; external D2D1_3Lib name 'D2D1GetGradientMeshInteriorPointsFromCoonsPatch' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
