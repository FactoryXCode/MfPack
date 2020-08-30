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
// Revision Version: 3.0.0
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit WinApi.DirectX.D2D1_3;

  {$HPPEMIT '#include "d2d1_3.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinCodec, {or MfPack.WinCodec}
  WinApi.ActiveX, {or MfPack.ObjIdlbase}
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2D1_2,
  WinApi.DirectX.D2D1Effects,
  WinApi.DirectX.D2D1Effects_2,
  WinApi.DirectX.D2D1Svg,
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DWrite;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

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

    procedure SetNibTransform(transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure GetNibTransform(out transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure SetNibShape(nibShape: D2D1_INK_NIB_SHAPE); stdcall;

    function GetNibShape(): D2D1_INK_NIB_SHAPE;

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

    // Resets the ink start point.
    procedure SetStartPoint(startPoint: D2D1_INK_POINT); stdcall;

    // Retrieve the start point with which the ink was initialized.
    function GetStartPoint(): D2D1_INK_POINT; stdcall;

    // Add one or more segments to the end of the ink.
    function AddSegments(segments: D2D1_INK_BEZIER_SEGMENT;
                         segmentsCount: UINT32): HResult; stdcall;

    // Remove one or more segments from the end of the ink.
    function RemoveSegmentsAtEnd(isegmentsCount: UINT32): HResult; stdcall;

    // Updates the specified segments with new control points.
    function SetSegments(startSegment: UINT32;
                         segments: PD2D1_INK_BEZIER_SEGMENT; // pointer to array of segments
                         segmentsCount: UINT32): HResult; stdcall;

    // Update the last segment with new control points.
    function SetSegmentAtEnd(segment: PD2D1_INK_BEZIER_SEGMENT): HResult; stdcall;

    // Returns the number of segments the ink is composed of.
    function GetSegmentCount(): UINT32;

    // Retrieve the segments stored in the ink.
    function GetSegments(startSegment: UINT32;
                         {out} segments: PD2D1_INK_BEZIER_SEGMENT;  // returns pointer to array of segments
                         {out} segmentsCount: UINT32): HResult; stdcall;

    // Construct a geometric representation of the ink.
    function StreamAsGeometry({in_opt} inkStyle: ID2D1InkStyle;
                              {in_opt} worldTransform: PD2D1_MATRIX_3X2_F;
                              flatteningTolerance: Single;
                              geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;

    // Retrieve the bounds of the ink, with an optional applied transform.
    function GetBounds({in_opt} inkStyle: ID2D1InkStyle;
                       {in_opt} worldTransform: PD2D1_MATRIX_3X2_F;
                       out bounds: D2D1_RECT_F): HResult; stdcall;

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

    // Returns the number of patches of the gradient mesh.
    function GetPatchCount(): UINT32; stdcall;

    // Retrieve the patch data stored in the gradient mesh.
    function GetPatches(startIndex: UINT32;
                        out patches: PD2D1_GRADIENT_MESH_PATCH; // returns pointer to array of patches
                        patchesCount: UINT32): HResult; stdcall;

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

    function OfferResources(): HResult; stdcall;

    function TryReclaimResources(out resourcesDiscarded: BOOL): HResult; stdcall;

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

    function EnsureCached({in_opt} rectangleToFill: PD2D1_RECT_U): HResult; stdcall;

    function TrimCache({in_opt} rectangleToPreserve: PD2D1_RECT_U): HResult; stdcall;

    procedure GetSource(out wicBitmapSource: IWICBitmapSource); stdcall;

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

    procedure GetSource(out imageSource: ID2D1ImageSource); stdcall;

    procedure GetProperties(out properties: D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES); stdcall;

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

    function CreateInk(startPoint: D2D1_INK_POINT;
                       out ink: ID2D1Ink): HResult; stdcall;

    // Creates a new ink style.
    function CreateInkStyle({in_opt} inkStyleProperties: PD2D1_INK_STYLE_PROPERTIES;
                            out inkStyle: ID2D1InkStyle): HResult; stdcall;

    function CreateGradientMesh(patches: D2D1_GRADIENT_MESH_PATCH;
                                patchesCount: UINT32;
                                out gradientMesh: ID2D1GradientMesh): HResult; stdcall;

    function CreateImageSourceFromWic(wicBitmapSource: IWICBitmapSource;
                                      loadingOptions: D2D1_IMAGE_SOURCE_LOADING_OPTIONS;
                                      alphaMode: D2D1_ALPHA_MODE;
                                      out imageSource: ID2D1ImageSourceFromWic): HResult; stdcall;

    // Creates a 3D lookup table for mapping a 3-channel input to a 3-channel output.
    // The table data must be provided in 4-channel format.
    function CreateLookupTable3D(precision: D2D1_BUFFER_PRECISION;
                                 extents: UINT32;
                                 data: PByte;
                                 dataCount: UINT32;
                                 strides: UINT32;
                                 out lookupTable: ID2D1LookupTable3D): HResult; stdcall;

    function CreateImageSourceFromDxgi(surfaces: IDXGISurface;
                                       surfaceCount: UINT32;
                                       colorSpace: DXGI_COLOR_SPACE_TYPE;
                                       options: D2D1_IMAGE_SOURCE_FROM_DXGI_OPTIONS;
                                       out imageSource: ID2D1ImageSource): HResult; stdcall;

    // Retrieves the world-space bounds in DIPs of the gradient mesh using the device
    // context DPI.
    function GetGradientMeshWorldBounds(gradientMesh: ID2D1GradientMesh;
                                        out pBounds: D2D1_RECT_F): HResult; stdcall;

    procedure DrawInk(ink: ID2D1Ink;
                      brush: ID2D1Brush;
                      {in_opt} inkStyle: ID2D1InkStyle); stdcall;

    procedure DrawGradientMesh(gradientMesh: ID2D1GradientMesh); stdcall;

    // Draw a metafile to the device context.
    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                              {in_opt} destinationRectangle: PD2D1_RECT_F = Nil;
                              {in_opt} sourceRectangle: PD2D1_RECT_F = Nil); stdcall;

    // Creates an image source which shares resources with an original.
    function CreateTransformedImageSource(imageSource: ID2D1ImageSource;
                                          properties: D2D1_TRANSFORMED_IMAGE_SOURCE_PROPERTIES;
                                          out transformedImageSource: ID2D1TransformedImageSource): HResult; stdcall;

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

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext2: ID2D1DeviceContext2): HResult; stdcall;

    // Flush all device contexts that reference a given bitmap.
    procedure FlushDeviceContexts(bitmap: ID2D1Bitmap); stdcall;

    // Returns the DXGI device associated with this D2D device.
    function GetDxgiDevice(out dxgiDevice: IDXGIDevice): HResult; stdcall;

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

    // This creates a new Direct2D device from the given IDXGIDevice.
    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice2: ID2D1Device2): HResult; stdcall;

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

    function DrawInk(ink: ID2D1Ink;
                     brush: ID2D1Brush;
                     {in_opt} inkStyle: ID2D1InkStyle): HResult; stdcall;

    function DrawGradientMesh(gradientMesh: ID2D1GradientMesh): HResult; stdcall;

    function DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                             {in_opt} destinationRectangle: PD2D1_RECT_F;
                             {in_opt} sourceRectangle: PD2D1_RECT_F): HResult; stdcall;

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

    // Returns the DPI reported by the metafile.
    function GetDpi(out dpiX: Single;
                    out dpiY: Single): HResult; stdcall;

    // Gets the bounds (in DIPs) of the metafile (as specified by the frame rect
    // declared in the metafile).
    function GetSourceBounds(out bounds: D2D1_RECT_F): HResult; stdcall;

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

    // Callback for examining a metafile record.
    function ProcessRecord(recordType: DWORD;
                          {in_opt} recordData: Pointer;
                          recordDataSize: DWORD;
                          flags: UINT32): HResult; stdcall;

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

    // Adds sprites to the end of the sprite batch.
    function AddSprites(spriteCount: UINT32;
                        destinationRectangles: D2D1_RECT_F;   // pointer to array of rectangles
                        sourceRectangles: PD2D1_RECT_U = Nil; // pointer to array of rectangles
                        colors: PD2D1_COLOR_F = Nil;          // pointer to array of colors
                        transforms: PD2D1_MATRIX_3X2_F = Nil; // pointer to array of transforms
                        destinationRectanglesStride: UINT32 = SizeOf(D2D1_RECT_F);
                        sourceRectanglesStride: UINT32 = SizeOf(D2D1_RECT_U);
                        colorsStride: UINT32 = SizeOf(D2D1_COLOR_F);
                        transformsStride: UINT32 = SizeOf(D2D1_MATRIX_3X2_F)): HResult; stdcall;

    // Set properties for existing sprites. All properties not specified are
    // unmodified.
    function SetSprites(startIndex: UINT32;
                        spriteCount: UINT32;
                        destinationRectangles: PD2D1_RECT_F = Nil;
                        sourceRectangles: PD2D1_RECT_U = Nil;
                        colors: PD2D1_COLOR_F = Nil;
                        transforms: PD2D1_MATRIX_3X2_F = Nil;
                        destinationRectanglesStride: UINT32 = SizeOf(D2D1_RECT_F);
                        sourceRectanglesStride: UINT32 = SizeOf(D2D1_RECT_U);
                        colorsStride: UINT32 = SizeOf(D2D1_COLOR_F);
                        transformsStride: UINT32 = SizeOf(D2D1_MATRIX_3X2_F)): HResult; stdcall;

    // Retrieves sprite properties.
    function GetSprites(startIndex: UINT32;
                        spriteCount: UINT32;
                        {out} destinationRectangles: PD2D1_RECT_F = Nil;
                        {out} sourceRectangles: PD2D1_RECT_U = Nil;
                        {out} colors: PD2D1_COLOR_F = Nil;
                        {out} transforms: PD2D1_MATRIX_3X2_F = Nil): HResult; stdcall;

    // Retrieves the number of sprites in the sprite batch.
    function GetSpriteCount(): UINT32;

    // Removes all sprites from the sprite batch.
    procedure Clear(); stdcall;

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

    // Creates a new sprite batch.
    function CreateSpriteBatch(out spriteBatch: ID2D1SpriteBatch): HResult; stdcall;

    // Draws sprites in a sprite batch.
    procedure DrawSpriteBatch(spriteBatch: ID2D1SpriteBatch;
                              startIndex: UINT32;
                              spriteCount: UINT32;
                              bitmap: ID2D1Bitmap;
                              interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
                              spriteOptions: D2D1_SPRITE_OPTIONS = D2D1_SPRITE_OPTIONS_NONE); stdcall;

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

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext3: ID2D1DeviceContext3): HResult; stdcall;
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

    // This creates a new Direct2D device from the given IDXGIDevice.
    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice3: ID2D1Device3): HResult; stdcall;
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

    function DrawSpriteBatch(spriteBatch: ID2D1SpriteBatch;
                             startIndex: UINT32;
                             spriteCount: UINT32;
                             bitmap: ID2D1Bitmap;
                             interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE;
                             spriteOptions: D2D1_SPRITE_OPTIONS): HResult; stdcall;

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
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgGlyphStyle);'}
  {$EXTERNALSYM ID2D1SvgGlyphStyle}
  ID2D1SvgGlyphStyle = interface(ID2D1Resource)
  ['{af671749-d241-4db8-8e41-dcc2e5c1a438}']

    // Provides values to an SVG glyph for fill. The brush with opacity set to 1 is
    // used as the 'context-fill'. The opacity of the brush is used as the
    // 'context-fill-opacity' value.

    // <param name="brush">A nil brush will cause the context-fill value to come from
    // the defaultFillBrush. If the defaultFillBrush is also nil, the context-fill
    // value will be 'none'.</param>
    function SetFill({in_opt} brush: ID2D1Brush): HResult; stdcall;

    // Returns the requested fill parameters.
    procedure GetFill(out brush: ID2D1Brush); stdcall;

    // Provides values to an SVG glyph for stroke properties. The brush with opacity
    // set to 1 is used as the 'context-stroke'. The opacity of the brush is used as
    // the 'context-stroke-opacity' value.

    // <param name="brush">A null brush will cause the context-stroke value to be
    // 'none'.</param>
    // <param name="strokeWidth">Specifies the 'context-value' for the 'stroke-width'
    // property.</param>
    // <param name="dashes">Specifies the 'context-value' for the 'stroke-dasharray'
    // property. A null value will cause the stroke-dasharray to be set to 'none'.
    // </param>
    // <param name="dashOffset">Specifies the 'context-value' for the
    // 'stroke-dashoffset' property.</param>
    function SetStroke({in_opt} brush: ID2D1Brush;
                       strokeWidth: Single = 1.0;
                       dashes: Single = 0.0;
                       dashesCount: UINT32 = 0;
                       dashOffset: Single = 1.0): HResult; stdcall;

    // Returns the number of dashes in the dash array.
    function GetStrokeDashesCount(): UINT32;

    // Returns the requested stroke parameters.
    procedure GetStroke(out brush: ID2D1Brush;
                        {out} strokeWidth: Single = 0.0;
                        {out} dashes: Single = 0.0;
                        dashesCount: UINT32 = 0;
                        {out} dashOffset: Single = 0.0); stdcall;

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

    // Creates an SVG glyph style object.
    function CreateSvgGlyphStyle(out svgGlyphStyle: ID2D1SvgGlyphStyle): HResult; stdcall;

    // Draws the text within the given layout rectangle. By default, this method
    // performs baseline snapping and renders color versions of glyphs in color fonts.

    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font.</param>
    procedure DrawText(_string: PWideChar;
                       stringLength: UINT32;
                       textFormat: IDWriteTextFormat;
                       layoutRect: D2D1_RECT_F;
                       {in_opt} defaultFillBrush: ID2D1Brush;
                       {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                       colorPaletteIndex: UINT32 = 0;
                       options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
                       measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;


    // Draw a text layout object. If the layout is not subsequently changed, this can
    // be more efficient than DrawText when drawing the same layout repeatedly.

    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font.</param>
    // <param name="options">The specified text options. If D2D1_DRAW_TEXT_OPTIONS_CLIP
    // is used, the text is clipped to the layout bounds. These bounds are derived from
    // the origin and the layout bounds of the corresponding IDWriteTextLayout object.
    // </param>
    procedure DrawTextLayout(origin: D2D1_POINT_2F;
                             textLayout: IDWriteTextLayout;
                             {in_opt} defaultFillBrush: ID2D1Brush;
                             {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                             colorPaletteIndex: UINT32 = 0;
                             options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT); stdcall;

    // Draws a color glyph run using one (and only one) of the bitmap formats-
    // DWRITE_GLYPH_IMAGE_FORMATS_PNG, DWRITE_GLYPH_IMAGE_FORMATS_JPEG,
    // DWRITE_GLYPH_IMAGE_FORMATS_TIFF, or
    // DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8.
    procedure DrawColorBitmapGlyphRun(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;
                                      baselineOrigin: D2D1_POINT_2F;
                                      glyphRun: DWRITE_GLYPH_RUN;
                                      measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL;
                                      bitmapSnapOption: D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT); stdcall;


    // Draws a color glyph run that has the format of DWRITE_GLYPH_IMAGE_FORMATS_SVG.
    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font. Note that this not the same as the paletteIndex in the
    // DWRITE_COLOR_GLYPH_RUN struct, which is not relevant for SVG glyphs.</param>
    procedure DrawSvgGlyphRun(baselineOrigin: D2D1_POINT_2F;
                              glyphRun: DWRITE_GLYPH_RUN;
                              {in_opt} defaultFillBrush: ID2D1Brush = Nil;
                              {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle = Nil;
                              colorPaletteIndex: UINT32 = 0;
                              measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;


    // Retrieves an image of the color bitmap glyph from the color glyph cache. If the
    // cache does not already contain the requested resource, it will be created. This
    // method may be used to extend the lifetime of a glyph image even after it is
    // evicted from the color glyph cache.

    // <param name="fontEmSize">The specified font size affects the choice of which
    // bitmap to use from the font. It also affects the output glyphTransform, causing
    // it to properly scale the glyph.</param>
    // <param name="glyphTransform">Output transform, which transforms from the glyph's
    // space to the same output space as the worldTransform. This includes the input
    // glyphOrigin, the glyph's offset from the glyphOrigin, and any other required
    // transformations.</param>
    function GetColorBitmapGlyphImage(glyphImageFormat: DWRITE_GLYPH_IMAGE_FORMATS;
                                      glyphOrigin: D2D1_POINT_2F;
                                      fontFace: IDWriteFontFace;
                                      fontEmSize: Single;
                                      glyphIndex: UINT16;
                                      isSideways: BOOL;
                                      {in_opt} worldTransform: PD2D1_MATRIX_3X2_F;
                                      dpiX: Single;
                                      dpiY: Single;
                                      out glyphTransform: D2D1_MATRIX_3X2_F;
                                      out glyphImage: ID2D1Image): HResult; stdcall;


    // Retrieves an image of the SVG glyph from the color glyph cache. If the cache
    // does not already contain the requested resource, it will be created. This method
    // may be used to extend the lifetime of a glyph image even after it is evicted
    // from the color glyph cache.

    // <param name="fontEmSize">The specified font size affects the output
    // glyphTransform, causing it to properly scale the glyph.</param>
    // <param name="svgGlyphStyle">Object used to style SVG glyphs.</param>
    // <param name="colorPaletteIndex">The index used to select a color palette within
    // a color font. Note that this not the same as the paletteIndex in the
    // DWRITE_COLOR_GLYPH_RUN struct, which is not relevant for SVG glyphs.</param>
    // <param name="glyphTransform">Output transform, which transforms from the glyph's
    // space to the same output space as the worldTransform. This includes the input
    // glyphOrigin, the glyph's offset from the glyphOrigin, and any other required
    // transformations.</param>
    function GetSvgGlyphImage(glyphOrigin: D2D1_POINT_2F;
                              fontFace: IDWriteFontFace;
                              fontEmSize: Single;
                              glyphIndex: UINT16;
                              isSideways: BOOL;
                              {in_opt} worldTransform: PD2D1_MATRIX_3X2_F;
                              {in_opt} defaultFillBrush: ID2D1Brush;
                              {in_opt} svgGlyphStyle: ID2D1SvgGlyphStyle;
                              colorPaletteIndex: UINT32;
                              out glyphTransform: D2D1_MATRIX_3X2_F;
                              out glyphImage: ID2D1CommandList): HResult; stdcall;

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

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext4: ID2D1DeviceContext4): HResult; stdcall;

    // Sets the maximum capacity of the color glyph cache. This cache is used to store
    // color bitmap glyphs and SVG glyphs, enabling faster performance if the same
    // glyphs are needed again. If the application still references a glyph using
    // GetColorBitmapGlyphImage or GetSvgGlyphImage after it has been evicted, this
    // glyph does not count toward the cache capacity.
    procedure SetMaximumColorGlyphCacheMemory(maximumInBytes: UINT64); stdcall;

    // Gets the maximum capacity of the color glyph cache.
    function GetMaximumColorGlyphCacheMemory(): UINT64;

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

    // This creates a new Direct2D device from the given IDXGIDevice.

    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice4: ID2D1Device4): HResult; stdcall;

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

    // A new function to set blend mode that respects the new MAX blend.
    //
    // Implementers of SetPrimitiveBlend2 should expect and handle blend mode:
    // D2D1_PRIMITIVE_BLEND_MAX
    //
    // Implementers of SetPrimitiveBlend1 should expect and handle blend modes:
    // D2D1_PRIMITIVE_BLEND_MIN and D2D1_PRIMITIVE_BLEND_ADD
    //
    // Implementers of SetPrimitiveBlend should expect and handle blend modes:
    // D2D1_PRIMITIVE_BLEND_SOURCE_OVER and D2D1_PRIMITIVE_BLEND_COPY
    function SetPrimitiveBlend2(primitiveBlend: D2D1_PRIMITIVE_BLEND): HResult; stdcall;

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

    // Retrieves the color context type.
    function GetColorContextType(): D2D1_COLOR_CONTEXT_TYPE;

    // Retrieves the DXGI color space of this context. Returns DXGI_COLOR_SPACE_CUSTOM
    // when color context type is ICC.
    function GetDXGIColorSpace(): DXGI_COLOR_SPACE_TYPE; stdcall;

    // Retrieves a set simple color profile.
    function GetSimpleColorProfile(out simpleProfile: D2D1_SIMPLE_COLOR_PROFILE): HResult; stdcall;

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

    // Creates an SVG document from a stream.

    // <param name="inputXmlStream">An input stream containing the SVG XML document. If
    // nil, an empty document is created.</param>
    // <param name="viewportSize">Size of the initial viewport of the document.</param>
    // <param name="svgDocument">When this method returns, contains a pointer to the
    // SVG document.</param>
    function CreateSvgDocument({in_opt} inputXmlStream: IStream;
                               viewportSize: D2D1_SIZE_F;
                               out svgDocument: ID2D1SvgDocument): HResult; stdcall;

    // Draw an SVG document.
    procedure DrawSvgDocument(svgDocument: ID2D1SvgDocument); stdcall;

    // Creates a color context from a DXGI color space type. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromDxgiColorSpace(colorSpace: DXGI_COLOR_SPACE_TYPE;
                                                  out colorContext: ID2D1ColorContext1): HResult; stdcall;

    // Creates a color context from a simple color profile. It is only valid to use
    // this with the Color Management Effect in 'Best' mode.
    function CreateColorContextFromSimpleColorProfile(simpleProfile: D2D1_SIMPLE_COLOR_PROFILE;
                                                      out colorContext: ID2D1ColorContext1): HResult; stdcall;
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

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext5: ID2D1DeviceContext5): HResult; stdcall;
  end; // interface ID2D1Device5


  // Creates Direct2D resources. This interface also enables the creation of
  // ID2D1Device5 objects.
  ID2D1Factory6 = interface(ID2D1Factory5)
  ['{f9976f46-f642-44c1-97ca-da32ea2a2635}']

    // This creates a new Direct2D device from the given IDXGIDevice.
    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice5: ID2D1Device5): HResult; stdcall;
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

    function BlendImage(image: ID2D1Image;
                        blendMode: D2D1_BLEND_MODE;
                        {in_opt} targetOffset: PD2D1_POINT_2F;
                        {in_opt} imageRectangle: PD2D1_RECT_F;
                        interpolationMode: D2D1_INTERPOLATION_MODE): HResult; stdcall;

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

    // Draw an image to the device context.
    procedure BlendImage(image: ID2D1Image;
                         blendMode: D2D1_BLEND_MODE;
                         {in_opt} targetOffset: PD2D1_POINT_2F = Nil;
                         {in_opt} imageRectangle: PD2D1_RECT_F = Nil;
                         interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR); stdcall;

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

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext6: ID2D1DeviceContext6): HResult; stdcall;

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

    // This creates a new Direct2D device from the given IDXGIDevice.
    function CreateDevice(dxgiDevice: IDXGIDevice;
                          out d2dDevice6: ID2D1Device6): HResult; stdcall;
  end;
  IID_ID2D1Factory7 = ID2D1Factory7;
  {$EXTERNALSYM IID_ID2D1Factory7}



// #if NTDDI_VERSION >= NTDDI_WINTHRESHOLD

    // Returns the interior points for a gradient mesh patch based on the points defining a Coons patch.
    // Note
    //   This function is called by the GradientMeshPatchFromCoonsPatch function and
    //   is not intended to be used directly.
    // Minimum supported client	Windows 10
    procedure D2D1GetGradientMeshInteriorPointsFromCoonsPatch(
      pPoint0:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 0.
      pPoint1:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 1.
      pPoint2:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 2.
      pPoint3:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 3.
      pPoint4:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 4.
      pPoint5:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 5.
      pPoint6:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 6.
      pPoint7:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 7.
      pPoint8:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 8.
      pPoint9:  D2D1_POINT_2F; // The coordinate-space location of the control point at position 9.
      pPoint10: D2D1_POINT_2F; // The coordinate-space location of the control point at position 10.
      pPoint11: D2D1_POINT_2F; // The coordinate-space location of the control point at position 11.
      out pTensorPoint11: D2D1_POINT_2F; // Returns the interior point for the gradient mesh corresponding to point11 in the D2D1_GRADIENT_MESH_PATCH structure.
      out pTensorPoint12: D2D1_POINT_2F; // Returns the interior point for the gradient mesh corresponding to point12 in the D2D1_GRADIENT_MESH_PATCH structure.
      out pTensorPoint21: D2D1_POINT_2F; // Returns the interior point for the gradient mesh corresponding to point21 in the D2D1_GRADIENT_MESH_PATCH structure.
      out pTensorPoint22: D2D1_POINT_2F  // Returns the interior point for the gradient mesh corresponding to point22 in the D2D1_GRADIENT_MESH_PATCH structure.
    ); stdcall;
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
