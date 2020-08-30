// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: Direct2D: Hardware-accelerated, immediate-mode, 2-D graphics API that
//              provides high performance and high-quality rendering for 2-D geometry,
//              bitmaps, and text.
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
// Source: d2d1.h
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
unit WinApi.DirectX.D2D1;

  {$HPPEMIT '#include "d2d1.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinCodec, {or use MfPack.WinCodec, if your included Delphi version is not up to date}
  {System}
  System.Types,
  {WinApi.DirectX}
  WinApi.DirectX.D2DBaseTypes,
  WinApi.DirectX.D2DErr,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D3DCommon,
  WinApi.DirectX.D3D9Types,
  WinApi.DirectX.DWrite,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

var
  TD2D1_RECT_F: D2D1_RECT_F;
  {$EXTERNALSYM TD2D1_RECT_F}

const

  D2D1_INVALID_TAG                    = ULONGLONG_MAX;
  {$EXTERNALSYM D2D1_INVALID_TAG}
  D2D1_DEFAULT_FLATTENING_TOLERANCE   = 0.25;
  {$EXTERNALSYM D2D1_DEFAULT_FLATTENING_TOLERANCE}

  // This defines the superset of interpolation mode supported by D2D APIs
  // and built-in effects

  D2D1_INTERPOLATION_MODE_DEFINITION_NEAREST_NEIGHBOR    = DWord(0);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_NEAREST_NEIGHBOR}
  D2D1_INTERPOLATION_MODE_DEFINITION_LINEAR              = DWord(1);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_LINEAR}
  D2D1_INTERPOLATION_MODE_DEFINITION_CUBIC               = DWord(2);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_CUBIC}
  D2D1_INTERPOLATION_MODE_DEFINITION_MULTI_SAMPLE_LINEAR = DWord(3);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_MULTI_SAMPLE_LINEAR}
  D2D1_INTERPOLATION_MODE_DEFINITION_ANISOTROPIC         = DWord(4);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_ANISOTROPIC}
  D2D1_INTERPOLATION_MODE_DEFINITION_HIGH_QUALITY_CUBIC  = DWord(5);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_HIGH_QUALITY_CUBIC}
  D2D1_INTERPOLATION_MODE_DEFINITION_FANT                = DWord(6);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_FANT}
  D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR       = DWord(7);
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR}


// Enums =======================================================================


type
  // This determines what gamma is used for interpolation/blending.
  PD2D1_GAMMA = ^D2D1_GAMMA;
  D2D1_GAMMA = DWord;
  {$EXTERNALSYM D2D1_GAMMA}
const
  // Colors are manipulated in 2.2 gamma color space.
  D2D1_GAMMA_2_2         = D2D1_GAMMA(0);
  {$EXTERNALSYM D2D1_GAMMA_2_2}
  // Colors are manipulated in 1.0 gamma color space.
  D2D1_GAMMA_1_0         = D2D1_GAMMA(1);
  {$EXTERNALSYM D2D1_GAMMA_1_0}
  //D2D1_GAMMA_FORCE_DWORD = FORCEDWORD;

type
  // Specifies what the contents are of an opacity mask.
  PD2D1_OPACITY_MASK_CONTENT = ^D2D1_OPACITY_MASK_CONTENT;
  D2D1_OPACITY_MASK_CONTENT = DWord;
  {$EXTERNALSYM D2D1_OPACITY_MASK_CONTENT}
const
  // The mask contains geometries or bitmaps.
  D2D1_OPACITY_MASK_CONTENT_GRAPHICS            = D2D1_OPACITY_MASK_CONTENT(0);
  {$EXTERNALSYM D2D1_OPACITY_MASK_CONTENT_GRAPHICS}
  // The mask contains text rendered using one of the natural text modes.
  D2D1_OPACITY_MASK_CONTENT_TEXT_NATURAL        = D2D1_OPACITY_MASK_CONTENT(1);
  {$EXTERNALSYM D2D1_OPACITY_MASK_CONTENT_TEXT_NATURAL}
  // The mask contains text rendered using one of the GDI compatible text modes.
  D2D1_OPACITY_MASK_CONTENT_TEXT_GDI_COMPATIBLE = D2D1_OPACITY_MASK_CONTENT(2);
  {$EXTERNALSYM D2D1_OPACITY_MASK_CONTENT_TEXT_GDI_COMPATIBLE}
  //D2D1_OPACITY_MASK_CONTENT_FORCE_DWORD         = FORCEDWORD;

type
  // Enum which describes how to sample from a source outside its base tile.
  PD2D1_EXTEND_MODE = ^D2D1_EXTEND_MODE;
  D2D1_EXTEND_MODE = (
    // Extend the edges of the source out by clamping sample points outside the source
    // to the edges.
    D2D1_EXTEND_MODE_CLAMP       = DWord(0),
    // The base tile is drawn untransformed and the remainder are filled by repeating
    // the base tile.
    D2D1_EXTEND_MODE_WRAP        = DWord(1),
    // The same as wrap, but alternate tiles are flipped  The base tile is drawn
    // untransformed.
    D2D1_EXTEND_MODE_MIRROR      = DWord(2)
    //D2D1_EXTEND_MODE_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_EXTEND_MODE}

type
  // Enum which describes the manner in which we render edges of non-text primitives.
  PD2D1_ANTIALIAS_MODE = ^D2D1_ANTIALIAS_MODE;
  D2D1_ANTIALIAS_MODE = DWord;
  {$EXTERNALSYM D2D1_ANTIALIAS_MODE}
const
  // The edges of each primitive are antialiased sequentially.
  D2D1_ANTIALIAS_MODE_PER_PRIMITIVE = D2D1_ANTIALIAS_MODE(0);
  {$EXTERNALSYM D2D1_ANTIALIAS_MODE_PER_PRIMITIVE}
  // Each pixel is rendered if its pixel center is contained by the geometry.
  D2D1_ANTIALIAS_MODE_ALIASED       = D2D1_ANTIALIAS_MODE(1);
  {$EXTERNALSYM D2D1_ANTIALIAS_MODE_ALIASED}
  //D2D1_ANTIALIAS_MODE_FORCE_DWORD   = FORCEDWORD;

type
  // Describes the antialiasing mode used for drawing text.
  PD2D1_TEXT_ANTIALIAS_MODE = ^D2D1_TEXT_ANTIALIAS_MODE;
  D2D1_TEXT_ANTIALIAS_MODE = DWord;
  {$EXTERNALSYM D2D1_TEXT_ANTIALIAS_MODE}
const
  // Render text using the current system setting.
  D2D1_TEXT_ANTIALIAS_MODE_DEFAULT     = D2D1_TEXT_ANTIALIAS_MODE(0);
  {$EXTERNALSYM D2D1_TEXT_ANTIALIAS_MODE_DEFAULT}
  // Render text using ClearType.
  D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE   = D2D1_TEXT_ANTIALIAS_MODE(1);
  {$EXTERNALSYM D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE}
  // Render text using gray-scale.
  D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE   = D2D1_TEXT_ANTIALIAS_MODE(2);
  {$EXTERNALSYM D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE}
  // Render text aliased.
  D2D1_TEXT_ANTIALIAS_MODE_ALIASED     = D2D1_TEXT_ANTIALIAS_MODE(3);
  {$EXTERNALSYM D2D1_TEXT_ANTIALIAS_MODE_ALIASED}
  //D2D1_TEXT_ANTIALIAS_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the algorithm that is used when images are scaled or rotated. Note
  // Starting in Windows 8, more interpolations modes are available. See
  // D2D1_INTERPOLATION_MODE for more info.
  PD2D1_BITMAP_INTERPOLATION_MODE = ^D2D1_BITMAP_INTERPOLATION_MODE;
  D2D1_BITMAP_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_BITMAP_INTERPOLATION_MODE}
const
  // Nearest Neighbor filtering. Also known as nearest pixel or nearest point
  // sampling.
  D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR = D2D1_INTERPOLATION_MODE_DEFINITION_NEAREST_NEIGHBOR;
  {$EXTERNALSYM D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR}
  // Linear filtering.
  D2D1_BITMAP_INTERPOLATION_MODE_LINEAR           = D2D1_INTERPOLATION_MODE_DEFINITION_LINEAR;
  {$EXTERNALSYM D2D1_BITMAP_INTERPOLATION_MODE_LINEAR}
  //D2D1_BITMAP_INTERPOLATION_MODE_FORCE_DWORD      = FORCEDWORD;

type
  // Modifications made to the draw text call that influence how the text is
  // rendered.
  PD2D1_DRAW_TEXT_OPTIONS = ^D2D1_DRAW_TEXT_OPTIONS;
  D2D1_DRAW_TEXT_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS}
const
  // Do not snap the baseline of the text vertically.
  D2D1_DRAW_TEXT_OPTIONS_NO_SNAP                       = D2D1_DRAW_TEXT_OPTIONS($00000001);
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS_NO_SNAP}
  // Clip the text to the content bounds.
  D2D1_DRAW_TEXT_OPTIONS_CLIP                          = D2D1_DRAW_TEXT_OPTIONS($00000002);
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS_CLIP}
  // Render color versions of glyphs if defined by the font.
  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT             = D2D1_DRAW_TEXT_OPTIONS($00000004);
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT}
  // Bitmap origins of color glyph bitmaps are not snapped.
  D2D1_DRAW_TEXT_OPTIONS_DISABLE_COLOR_BITMAP_SNAPPING = D2D1_DRAW_TEXT_OPTIONS($00000008);
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS_DISABLE_COLOR_BITMAP_SNAPPING}
  D2D1_DRAW_TEXT_OPTIONS_NONE                          = D2D1_DRAW_TEXT_OPTIONS($00000000);
  {$EXTERNALSYM D2D1_DRAW_TEXT_OPTIONS_NONE}
  //D2D1_DRAW_TEXT_OPTIONS_FORCE_DWORD                   = FORCEDWORD;


type
  // Differentiates which of the two possible arcs could match the given arc
  // parameters.
  PD2D1_ARC_SIZE = ^D2D1_ARC_SIZE;
  D2D1_ARC_SIZE = DWord;
  {$EXTERNALSYM D2D1_ARC_SIZE}
const
  D2D1_ARC_SIZE_SMALL       = D2D1_ARC_SIZE(0);
  {$EXTERNALSYM D2D1_ARC_SIZE_SMALL}
  D2D1_ARC_SIZE_LARGE       = D2D1_ARC_SIZE(1);
  {$EXTERNALSYM D2D1_ARC_SIZE_LARGE}
  //D2D1_ARC_SIZE_FORCE_DWORD = FORCEDWORD;

type
  // Enum which describes the drawing of the ends of a line.
  PD2D1_CAP_STYLE = ^D2D1_CAP_STYLE;
  D2D1_CAP_STYLE = DWord;
  {$EXTERNALSYM D2D1_CAP_STYLE}
const
  // Flat line cap.
  D2D1_CAP_STYLE_FLAT        = D2D1_CAP_STYLE(0);
  {$EXTERNALSYM D2D1_CAP_STYLE_FLAT}
  // Square line cap.
  D2D1_CAP_STYLE_SQUARE      = D2D1_CAP_STYLE(1);
  {$EXTERNALSYM D2D1_CAP_STYLE_SQUARE}
  // Round line cap.
  D2D1_CAP_STYLE_ROUND       = D2D1_CAP_STYLE(2);
  {$EXTERNALSYM D2D1_CAP_STYLE_ROUND}
  // Triangle line cap.
  D2D1_CAP_STYLE_TRIANGLE    = D2D1_CAP_STYLE(3);
  {$EXTERNALSYM D2D1_CAP_STYLE_TRIANGLE}
  //D2D1_CAP_STYLE_FORCE_DWORD = FORCEDWORD;

type
  // Describes the sequence of dashes and gaps in a stroke.
  PD2D1_DASH_STYLE = ^D2D1_DASH_STYLE;
  D2D1_DASH_STYLE = DWord;
  {$EXTERNALSYM D2D1_DASH_STYLE}
const
  D2D1_DASH_STYLE_SOLID         = D2D1_DASH_STYLE(0);
  {$EXTERNALSYM D2D1_DASH_STYLE_SOLID}
  D2D1_DASH_STYLE_DASH         = D2D1_DASH_STYLE(1);
  {$EXTERNALSYM D2D1_DASH_STYLE_DASH}
  D2D1_DASH_STYLE_DOT          = D2D1_DASH_STYLE(2);
  {$EXTERNALSYM D2D1_DASH_STYLE_DOT}
  D2D1_DASH_STYLE_DASH_DOT     = D2D1_DASH_STYLE(3);
  {$EXTERNALSYM D2D1_DASH_STYLE_DASH_DOT}
  D2D1_DASH_STYLE_DASH_DOT_DOT = D2D1_DASH_STYLE(4);
  {$EXTERNALSYM D2D1_DASH_STYLE_DASH_DOT_DOT}
  D2D1_DASH_STYLE_CUSTOM       = D2D1_DASH_STYLE(5);
  {$EXTERNALSYM D2D1_DASH_STYLE_CUSTOM}
  //D2D1_DASH_STYLE_FORCE_DWORD  = FORCEDWORD;

type
  // Enum which describes the drawing of the corners on the line.
  PD2D1_LINE_JOIN = ^D2D1_LINE_JOIN;
  D2D1_LINE_JOIN = DWord;
  {$EXTERNALSYM D2D1_LINE_JOIN}
const
  // Miter join.
  D2D1_LINE_JOIN_MITER          = D2D1_LINE_JOIN(0);
  {$EXTERNALSYM D2D1_LINE_JOIN_MITER}
  // Bevel join.
  D2D1_LINE_JOIN_BEVEL          = D2D1_LINE_JOIN(1);
  {$EXTERNALSYM D2D1_LINE_JOIN_BEVEL}
  // Round join.
  D2D1_LINE_JOIN_ROUND          = D2D1_LINE_JOIN(2);
  {$EXTERNALSYM D2D1_LINE_JOIN_ROUND}
  // Miter/Bevel join.
  D2D1_LINE_JOIN_MITER_OR_BEVEL = D2D1_LINE_JOIN(3);
  {$EXTERNALSYM D2D1_LINE_JOIN_MITER_OR_BEVEL}
  //D2D1_LINE_JOIN_FORCE_DWORD    = FORCEDWORD;

type
  // This enumeration describes the type of combine operation to be performed.
  PD2D1_COMBINE_MODE = ^D2D1_COMBINE_MODE;
  D2D1_COMBINE_MODE = DWord;
  {$EXTERNALSYM D2D1_COMBINE_MODE}
const
  // Produce a geometry representing the set of points contained in either the first
  // or the second geometry.
  D2D1_COMBINE_MODE_UNION       = D2D1_COMBINE_MODE(0);
  {$EXTERNALSYM D2D1_COMBINE_MODE_UNION}
  // Produce a geometry representing the set of points common to the first and the
  // second geometries.
  D2D1_COMBINE_MODE_INTERSECT   = D2D1_COMBINE_MODE(1);
  {$EXTERNALSYM D2D1_COMBINE_MODE_INTERSECT}
  // Produce a geometry representing the set of points contained in the first
  // geometry or the second geometry); but not both.
  D2D1_COMBINE_MODE_XOR         = D2D1_COMBINE_MODE(2);
  {$EXTERNALSYM D2D1_COMBINE_MODE_XOR}
  // Produce a geometry representing the set of points contained in the first
  // geometry but not the second geometry.
  D2D1_COMBINE_MODE_EXCLUDE     = D2D1_COMBINE_MODE(3);
  {$EXTERNALSYM D2D1_COMBINE_MODE_EXCLUDE}
  //D2D1_COMBINE_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Describes how one geometry object is spatially related to another geometry
  // object.
  PD2D1_GEOMETRY_RELATION = ^D2D1_GEOMETRY_RELATION;
  D2D1_GEOMETRY_RELATION = DWord;
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION}
const
  // The relation between the geometries couldn't be determined. This value is never
  // returned by any D2D method.
  D2D1_GEOMETRY_RELATION_UNKNOWN      = D2D1_GEOMETRY_RELATION(0);
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION_UNKNOWN}
  // The two geometries do not intersect at all.
  D2D1_GEOMETRY_RELATION_DISJOINT     = D2D1_GEOMETRY_RELATION(1);
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION_DISJOINT}
  // The passed in geometry is entirely contained by the object.
  D2D1_GEOMETRY_RELATION_IS_CONTAINED = D2D1_GEOMETRY_RELATION(2);
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION_IS_CONTAINED}
  // The object entirely contains the passed in geometry.
  D2D1_GEOMETRY_RELATION_CONTAINS     = D2D1_GEOMETRY_RELATION(3);
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION_CONTAINS}
  // The two geometries overlap but neither completely contains the other.
  D2D1_GEOMETRY_RELATION_OVERLAP      = D2D1_GEOMETRY_RELATION(4);
  {$EXTERNALSYM D2D1_GEOMETRY_RELATION_OVERLAP}
  //D2D1_GEOMETRY_RELATION_FORCE_DWORD  = FORCEDWORD;

type
  // Specifies how simple the output of a simplified geometry sink should be.
  PD2D1_GEOMETRY_SIMPLIFICATION_OPTION = ^D2D1_GEOMETRY_SIMPLIFICATION_OPTION;
  D2D1_GEOMETRY_SIMPLIFICATION_OPTION = DWord;
  {$EXTERNALSYM D2D1_GEOMETRY_SIMPLIFICATION_OPTION}
const
  D2D1_GEOMETRY_SIMPLIFICATION_OPTION_CUBICS_AND_LINES = D2D1_GEOMETRY_SIMPLIFICATION_OPTION(0);
  {$EXTERNALSYM D2D1_GEOMETRY_SIMPLIFICATION_OPTION_CUBICS_AND_LINES}
  D2D1_GEOMETRY_SIMPLIFICATION_OPTION_LINES            = D2D1_GEOMETRY_SIMPLIFICATION_OPTION(1);
  {$EXTERNALSYM D2D1_GEOMETRY_SIMPLIFICATION_OPTION_LINES}
  //D2D1_GEOMETRY_SIMPLIFICATION_OPTION_FORCE_DWORD      = FORCEDWORD;

type
  // Indicates whether the given figure is filled or hollow.
  PD2D1_FIGURE_BEGIN = ^D2D1_FIGURE_BEGIN;
  D2D1_FIGURE_BEGIN = DWord;
  {$EXTERNALSYM D2D1_FIGURE_BEGIN}
const
  D2D1_FIGURE_BEGIN_FILLED      = D2D1_FIGURE_BEGIN(0);
  {$EXTERNALSYM D2D1_FIGURE_BEGIN_FILLED}
  D2D1_FIGURE_BEGIN_HOLLOW      = D2D1_FIGURE_BEGIN(1);
  {$EXTERNALSYM D2D1_FIGURE_BEGIN_HOLLOW}
  //D2D1_FIGURE_BEGIN_FORCE_DWORD = FORCEDWORD);

type
  // Indicates whether the figure is open or closed on its end point.
  PD2D1_FIGURE_END = ^D2D1_FIGURE_END;
  D2D1_FIGURE_END = DWord;
  {$EXTERNALSYM D2D1_FIGURE_END}
const
  D2D1_FIGURE_END_OPEN        = D2D1_FIGURE_END(0);
  {$EXTERNALSYM D2D1_FIGURE_END_OPEN}
  D2D1_FIGURE_END_CLOSED      = D2D1_FIGURE_END(1);
  {$EXTERNALSYM D2D1_FIGURE_END_CLOSED}
  //D2D1_FIGURE_END_FORCE_DWORD = FORCEDWORD;

type
  // Indicates whether the given segment should be stroked, or, if the join between
  // this segment and the previous one should be smooth.
  PD2D1_PATH_SEGMENT = ^D2D1_PATH_SEGMENT;
  D2D1_PATH_SEGMENT = DWord;
  {$EXTERNALSYM D2D1_PATH_SEGMENT}
const
  {$EXTERNALSYM D2D1_PATH_SEGMENT_NONE}
  D2D1_PATH_SEGMENT_NONE                  = D2D1_PATH_SEGMENT($00000000);
  {$EXTERNALSYM D2D1_PATH_SEGMENT_FORCE_UNSTROKED}
  D2D1_PATH_SEGMENT_FORCE_UNSTROKED       = D2D1_PATH_SEGMENT($00000001);
  {$EXTERNALSYM D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN}
  D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN = D2D1_PATH_SEGMENT($00000002);
  //D2D1_PATH_SEGMENT_FORCE_DWORD           = FORCEDWORD;

type
  // Defines the direction that an elliptical arc is drawn.
  PD2D1_SWEEP_DIRECTION = ^D2D1_SWEEP_DIRECTION;
  D2D1_SWEEP_DIRECTION = DWord;
  {$EXTERNALSYM D2D1_SWEEP_DIRECTION}
const
  {$EXTERNALSYM D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE}
  D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE = D2D1_SWEEP_DIRECTION(0);
  {$EXTERNALSYM D2D1_SWEEP_DIRECTION_CLOCKWISE}
  D2D1_SWEEP_DIRECTION_CLOCKWISE         = D2D1_SWEEP_DIRECTION(1);
  //D2D1_SWEEP_DIRECTION_FORCE_DWORD       = FORCEDWORD;

type
  // Specifies how the intersecting areas of geometries or figures are combined to
  // form the area of the composite geometry.
  PD2D1_FILL_MODE = ^D2D1_FILL_MODE;
  D2D1_FILL_MODE = (
    D2D1_FILL_MODE_ALTERNATE   = DWord(0),
    D2D1_FILL_MODE_WINDING     = DWord(1)
    //D2D1_FILL_MODE_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_FILL_MODE}


type
  // Specified options that can be applied when a layer resource is applied to create
  // a layer.
  PD2D1_LAYER_OPTIONS = ^D2D1_LAYER_OPTIONS;
  D2D1_LAYER_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_LAYER_OPTIONS}
const
  D2D1_LAYER_OPTIONS_NONE = D2D1_LAYER_OPTIONS($00000000);
  {$EXTERNALSYM D2D1_LAYER_OPTIONS_NONE}
  // The layer will render correctly for ClearType text. If the render target was set
  // to ClearType previously, the layer will continue to render ClearType. If the
  // render target was set to ClearType and this option is not specified, the render
  // target will be set to render gray-scale until the layer is popped. The caller
  // can override this default by calling SetTextAntialiasMode while within the
  // layer. This flag is slightly slower than the default.
  {$EXTERNALSYM D2D1_LAYER_OPTIONS_INITIALIZE_FOR_CLEARTYPE}
  D2D1_LAYER_OPTIONS_INITIALIZE_FOR_CLEARTYPE = D2D1_LAYER_OPTIONS($00000001);
  //D2D1_LAYER_OPTIONS_FORCE_DWORD              = FORCEDWORD;

type
  // Describes whether a window is occluded.
  PD2D1_WINDOW_STATE = ^D2D1_WINDOW_STATE;
  D2D1_WINDOW_STATE = DWord;
  {$EXTERNALSYM D2D1_WINDOW_STATE}
const
  D2D1_WINDOW_STATE_NONE        = D2D1_WINDOW_STATE($0000000);
  {$EXTERNALSYM D2D1_WINDOW_STATE_NONE}
  D2D1_WINDOW_STATE_OCCLUDED    = D2D1_WINDOW_STATE($0000001);
  {$EXTERNALSYM D2D1_WINDOW_STATE_OCCLUDED}
  //D2D1_WINDOW_STATE_FORCE_DWORD = FORCEDWORD;

type
  // Describes whether a render target uses hardware or software rendering, or if
  // Direct2D should select the rendering mode.
  PD2D1_RENDER_TARGET_TYPE = ^D2D1_RENDER_TARGET_TYPE;
  D2D1_RENDER_TARGET_TYPE = DWord;
  {$EXTERNALSYM D2D1_RENDER_TARGET_TYPE}
const
  // D2D is free to choose the render target type for the caller.
  D2D1_RENDER_TARGET_TYPE_DEFAULT     = D2D1_RENDER_TARGET_TYPE(0);
  {$EXTERNALSYM D2D1_RENDER_TARGET_TYPE_DEFAULT}
  // The render target will render using the CPU.
  D2D1_RENDER_TARGET_TYPE_SOFTWARE    = D2D1_RENDER_TARGET_TYPE(1);
  {$EXTERNALSYM D2D1_RENDER_TARGET_TYPE_SOFTWARE}
  // The render target will render using the GPU.
  D2D1_RENDER_TARGET_TYPE_HARDWARE    = D2D1_RENDER_TARGET_TYPE(2);
  {$EXTERNALSYM D2D1_RENDER_TARGET_TYPE_HARDWARE}
  //D2D1_RENDER_TARGET_TYPE_FORCE_DWORD = FORCEDWORD;

type
  // Describes the minimum DirectX support required for hardware rendering by a
  // render target.
  PD2D1_FEATURE_LEVEL = ^D2D1_FEATURE_LEVEL;
  D2D1_FEATURE_LEVEL = D3D_FEATURE_LEVEL;
  {$EXTERNALSYM D2D1_FEATURE_LEVEL}
const
  // The caller does not require a particular underlying D3D device level.
  D2D1_FEATURE_LEVEL_DEFAULT     = D2D1_FEATURE_LEVEL(0);
  {$EXTERNALSYM D2D1_FEATURE_LEVEL_DEFAULT}
  // The D3D device level is DX9 compatible.
  D2D1_FEATURE_LEVEL_9           = D3D_FEATURE_LEVEL_9_1;
  {$EXTERNALSYM D2D1_FEATURE_LEVEL_9}
  // The D3D device level is DX10 compatible.
  D2D1_FEATURE_LEVEL_10          = D3D_FEATURE_LEVEL_10_0;
  {$EXTERNALSYM D2D1_FEATURE_LEVEL_10}
  //D2D1_FEATURE_LEVEL_FORCE_DWORD = FORCEDWORD;

type
  // Describes how a render target is remoted and whether it should be
  // GDI-compatible. This enumeration allows a bitwise combination of its member
  // values.
  PD2D1_RENDER_TARGET_USAGE = ^D2D1_RENDER_TARGET_USAGE;
  D2D1_RENDER_TARGET_USAGE = DWord;
  {$EXTERNALSYM D2D1_RENDER_TARGET_USAGE}
const
  D2D1_RENDER_TARGET_USAGE_NONE                  = D2D1_RENDER_TARGET_USAGE($00000000);
  {$EXTERNALSYM D2D1_RENDER_TARGET_USAGE_NONE}
  // Rendering will occur locally, if a terminal-services session is established, the
  // bitmap updates will be sent to the terminal services client.
  D2D1_RENDER_TARGET_USAGE_FORCE_BITMAP_REMOTING = D2D1_RENDER_TARGET_USAGE($00000001);
  {$EXTERNALSYM D2D1_RENDER_TARGET_USAGE_FORCE_BITMAP_REMOTING}
  // The render target will allow a call to GetDC on the ID2D1GdiInteropRenderTarget
  // interface. Rendering will also occur locally.
  D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE        = D2D1_RENDER_TARGET_USAGE($00000002);
  {$EXTERNALSYM D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE}
  //D2D1_RENDER_TARGET_USAGE_FORCE_DWORD           = FORCEDWORD;

type
  // Describes how present should behave.
  PD2D1_PRESENT_OPTIONS = ^D2D1_PRESENT_OPTIONS;
  D2D1_PRESENT_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_PRESENT_OPTIONS}
const
  D2D1_PRESENT_OPTIONS_NONE            = D2D1_PRESENT_OPTIONS($00000000);
  {$EXTERNALSYM D2D1_PRESENT_OPTIONS_NONE}
  // Keep the target contents intact through present.
  D2D1_PRESENT_OPTIONS_RETAIN_CONTENTS = D2D1_PRESENT_OPTIONS($00000001);
  {$EXTERNALSYM D2D1_PRESENT_OPTIONS_RETAIN_CONTENTS}
  // Do not wait for display refresh to commit changes to display.
  D2D1_PRESENT_OPTIONS_IMMEDIATELY     = D2D1_PRESENT_OPTIONS($00000002);
  {$EXTERNALSYM D2D1_PRESENT_OPTIONS_IMMEDIATELY}
  //D2D1_PRESENT_OPTIONS_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies additional features supportable by a compatible render target when it
  // is created. This enumeration allows a bitwise combination of its member values.
  PD2D1_COMPATIBLE_RENDER_TARGET_OPTIONS = ^D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS;
  D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS}
const
  D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE           = D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS($00000000);
  {$EXTERNALSYM D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE}
  // The compatible render target will allow a call to GetDC on the
  // ID2D1GdiInteropRenderTarget interface. This can be specified even if the parent
  // render target is not GDI compatible.
  D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_GDI_COMPATIBLE = D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS($00000001);
  {$EXTERNALSYM D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_GDI_COMPATIBLE}
  //D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_FORCE_DWORD    = FORCEDWORD);

type
  // Specifies how a device context is initialized for GDI rendering when it is
  // retrieved from the render target.
  PD2D1_DC_INITIALIZE_MODE = ^D2D1_DC_INITIALIZE_MODE;
  D2D1_DC_INITIALIZE_MODE = DWord;
  {$EXTERNALSYM D2D1_DC_INITIALIZE_MODE}
const
  // The contents of the D2D render target will be copied to the DC.
  D2D1_DC_INITIALIZE_MODE_COPY        = D2D1_DC_INITIALIZE_MODE(0);
  {$EXTERNALSYM D2D1_DC_INITIALIZE_MODE_COPY}
  // The contents of the DC will be cleared.
  D2D1_DC_INITIALIZE_MODE_CLEAR       = D2D1_DC_INITIALIZE_MODE(1);
  {$EXTERNALSYM D2D1_DC_INITIALIZE_MODE_CLEAR}
  //D2D1_DC_INITIALIZE_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Indicates the debug level to be output by the debug layer.
  PD2D1_DEBUG_LEVEL = ^D2D1_DEBUG_LEVEL;
  D2D1_DEBUG_LEVEL = DWord;
  {$EXTERNALSYM D2D1_DEBUG_LEVEL}
const
  D2D1_DEBUG_LEVEL_NONE        = D2D1_DEBUG_LEVEL(0); // Direct2D does not produce any debugging output.
  {$EXTERNALSYM D2D1_DEBUG_LEVEL_NONE}
  D2D1_DEBUG_LEVEL_ERROR       = D2D1_DEBUG_LEVEL(1); // Direct2D sends error messages to the debug layer.
  {$EXTERNALSYM D2D1_DEBUG_LEVEL_ERROR}
  D2D1_DEBUG_LEVEL_WARNING     = D2D1_DEBUG_LEVEL(2); // Direct2D sends error messages and warnings to the debug layer.
  {$EXTERNALSYM D2D1_DEBUG_LEVEL_WARNING}
  D2D1_DEBUG_LEVEL_INFORMATION = D2D1_DEBUG_LEVEL(3); // Direct2D sends error messages, warnings, and additional diagnostic information that can help improve performance to the debug layer.
  {$EXTERNALSYM D2D1_DEBUG_LEVEL_INFORMATION}
  //D2D1_DEBUG_LEVEL_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the threading model of the created factory and all of its derived
  // resources.
  PD2D1_FACTORY_TYPE = ^D2D1_FACTORY_TYPE;
  D2D1_FACTORY_TYPE = DWord;
  {$EXTERNALSYM D2D1_FACTORY_TYPE}
const
  // The resulting factory and derived resources may only be invoked serially.
  // Reference counts on resources are interlocked, however, resource and render
  // target state is not protected from multi-threaded access.
  D2D1_FACTORY_TYPE_SINGLE_THREADED = D2D1_FACTORY_TYPE(0);
  {$EXTERNALSYM D2D1_FACTORY_TYPE_SINGLE_THREADED}
  // The resulting factory may be invoked from multiple threads. Returned resources
  // use interlocked reference counting and their state is protected.
  D2D1_FACTORY_TYPE_MULTI_THREADED  = D2D1_FACTORY_TYPE(1);
  {$EXTERNALSYM D2D1_FACTORY_TYPE_MULTI_THREADED}
  //D2D1_FACTORY_TYPE_FORCE_DWORD     = FORCEDWORD;

// =============================================================================

type

  //
  // Forward type declarations here
  //

  PID2D1Brush = ^ID2D1Brush;
  ID2D1Brush = interface;

  PID2D1Geometry = ^ID2D1Geometry;
  ID2D1Geometry = interface;

  PID2D1SimplifiedGeometrySink = ^ID2D1SimplifiedGeometrySink;
  ID2D1SimplifiedGeometrySink = interface;

  // Delphi Note: Moved from DWrite.pas to prevent cricular reference error F2047
  PIDWriteGeometrySink = ^IDWriteGeometrySink;
  IDWriteGeometrySink = ID2D1SimplifiedGeometrySink;

  PID2D1TessellationSink = ^ID2D1TessellationSink;
  ID2D1TessellationSink = interface;

  PID2D1RenderTarget = ^ID2D1RenderTarget;
  ID2D1RenderTarget = interface;

  PID2D1BitmapRenderTarget = ^ID2D1BitmapRenderTarget;
  ID2D1BitmapRenderTarget = interface;

  PID2D1Factory = ^ID2D1Factory;
  ID2D1Factory = interface;


  PD2D1_POINT_2U = ^D2D1_POINT_2U;
  D2D1_POINT_2U = D2D_POINT_2U;
  {$EXTERNALSYM D2D1_POINT_2U}

  PD2D1_POINT_2F = ^D2D1_POINT_2F;
  D2D1_POINT_2F = D2D_POINT_2F;
  {$EXTERNALSYM D2D1_POINT_2F}

  PD2D1_RECT_F = ^D2D1_RECT_F;
  D2D1_RECT_F = D2D_RECT_F;
  {$EXTERNALSYM D2D1_RECT_F}

  PD2D1_RECT_U = ^D2D1_RECT_U;
  D2D1_RECT_U = D2D_RECT_U;
  {$EXTERNALSYM D2D1_RECT_U}

  PD2D1_SIZE_F = ^D2D1_SIZE_F;
  D2D1_SIZE_F = D2D_SIZE_F;
  {$EXTERNALSYM D2D1_SIZE_F}

  PD2D1_SIZE_U = ^D2D1_SIZE_U;
  D2D1_SIZE_U = D2D_SIZE_U;
  {$EXTERNALSYM D2D1_SIZE_U}

  PD2D1_COLOR_F = ^D2D1_COLOR_F;
  D2D1_COLOR_F = D2D_COLOR_F;
  {$EXTERNALSYM D2D1_COLOR_F}

  PD2D1_MATRIX_3X2_F = ^D2D1_MATRIX_3X2_F;
  D2D1_MATRIX_3X2_F = D2D_MATRIX_3X2_F;
  {$EXTERNALSYM D2D1_MATRIX_3X2_F}

  PD2D1_TAG = ^D2D1_TAG;
  D2D1_TAG = UINT64;
  {$EXTERNALSYM D2D1_TAG}


  // Describes the pixel format and dpi of a bitmap.
  PD2D1_BITMAP_PROPERTIES = ^D2D1_BITMAP_PROPERTIES;
  D2D1_BITMAP_PROPERTIES = record
    _pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
  end;
  {$EXTERNALSYM D2D1_BITMAP_PROPERTIES}


  // Contains the position and color of a gradient stop.
  PD2D1_GRADIENT_STOP = ^D2D1_GRADIENT_STOP;
  D2D1_GRADIENT_STOP = record
    position: Single;
    color: D2D1_COLOR_F;
  end;
  {$EXTERNALSYM D2D1_GRADIENT_STOP}


  // Describes the opacity and transformation of a brush.
  PD2D1_BRUSH_PROPERTIES = ^D2D1_BRUSH_PROPERTIES;
  D2D1_BRUSH_PROPERTIES = record
    opacity: Single;
    transform: D2D1_MATRIX_3X2_F;
  end;
  {$EXTERNALSYM D2D1_BRUSH_PROPERTIES}


  // Describes the extend modes and the interpolation mode of an ID2D1BitmapBrush.
  PD2D1_BITMAP_BRUSH_PROPERTIES = ^D2D1_BITMAP_BRUSH_PROPERTIES;
  D2D1_BITMAP_BRUSH_PROPERTIES = record
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_BITMAP_BRUSH_PROPERTIES}


  // Contains the starting point and endpoint of the gradient axis for an
  // ID2D1LinearGradientBrush.
  PD2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES = ^D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
  D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES = record
    startPoint: D2D1_POINT_2F;
    endPoint: D2D1_POINT_2F;
  end;
  {$EXTERNALSYM D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES}


  // Contains the gradient origin offset and the size and position of the gradient
  // ellipse for an ID2D1RadialGradientBrush.
  PD2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES = ^D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
  D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES = record
    center: D2D1_POINT_2F;
    gradientOriginOffset: D2D1_POINT_2F;
    radiusX: Single;
    radiusY: Single;
  end;
  {$EXTERNALSYM D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES}


  // Describes a cubic bezier in a path.
  PD2D1_BEZIER_SEGMENT = ^D2D1_BEZIER_SEGMENT;
  D2D1_BEZIER_SEGMENT = record
    point1: D2D1_POINT_2F;
    point2: D2D1_POINT_2F;
    point3: D2D1_POINT_2F;
  end;
  {$EXTERNALSYM D2D1_BEZIER_SEGMENT}


  // Describes a triangle.
  PD2D1_TRIANGLE = ^D2D1_TRIANGLE;
  D2D1_TRIANGLE = record
    point1: D2D1_POINT_2F;
    point2: D2D1_POINT_2F;
    point3: D2D1_POINT_2F;
  end;
  {$EXTERNALSYM D2D1_TRIANGLE}


  // Describes an arc that is defined as part of a path.
  PD2D1_ARC_SEGMENT = ^D2D1_ARC_SEGMENT;
  D2D1_ARC_SEGMENT = record
    point: D2D1_POINT_2F;
    size: D2D1_SIZE_F;
    rotationAngle: Single;
    sweepDirection: D2D1_SWEEP_DIRECTION;
    arcSize: D2D1_ARC_SIZE;
  end;
  {$EXTERNALSYM D2D1_ARC_SEGMENT}

  // Contains the control point and end point for a quadratic Bezier segment.
  PD2D1_QUADRATIC_BEZIER_SEGMENT = ^D2D1_QUADRATIC_BEZIER_SEGMENT;
  D2D1_QUADRATIC_BEZIER_SEGMENT = record
    point1: D2D1_POINT_2F;
    point2: D2D1_POINT_2F;
  end;
  {$EXTERNALSYM D2D1_QUADRATIC_BEZIER_SEGMENT}

  // Contains the center point, x-radius, and y-radius of an ellipse.
  PD2D1_ELLIPSE = ^D2D1_ELLIPSE;
  D2D1_ELLIPSE = record
    point: D2D1_POINT_2F;
    radiusX: Single;
    radiusY: Single;
  end;
  {$EXTERNALSYM D2D1_ELLIPSE}

  // Contains the dimensions and corner radii of a rounded rectangle.
  PD2D1_ROUNDED_RECT = ^D2D1_ROUNDED_RECT;
  D2D1_ROUNDED_RECT = record
    rect: D2D1_RECT_F;
    radiusX: Single;
    radiusY: Single;
  end;
  {$EXTERNALSYM D2D1_ROUNDED_RECT}

  // Properties, aside from the width, that allow geometric penning to be specified.
  PD2D1_STROKE_STYLE_PROPERTIES = ^D2D1_STROKE_STYLE_PROPERTIES;
  D2D1_STROKE_STYLE_PROPERTIES = record
    startCap: D2D1_CAP_STYLE;
    endCap: D2D1_CAP_STYLE;
    dashCap: D2D1_CAP_STYLE;
    lineJoin: D2D1_LINE_JOIN;
    miterLimit: Single;
    dashStyle: D2D1_DASH_STYLE;
    dashOffset: Single;
  end;
  {$EXTERNALSYM D2D1_STROKE_STYLE_PROPERTIES}


  // Contains the content bounds, mask information, opacity settings, and other
  // options for a layer resource.

  PD2D1_LAYER_PARAMETERS = ^D2D1_LAYER_PARAMETERS;
  D2D1_LAYER_PARAMETERS = record
    // The rectangular clip that will be applied to the layer. The clip is affected by
    // the world transform. Content outside of the content bounds will not render.
    contentBounds: D2D1_RECT_F;

    // A general mask that can be optionally applied to the content. Content not inside
    // the fill of the mask will not be rendered.
    geometricMask: ID2D1Geometry;


    // Specifies whether the mask should be aliased or antialiased.
    maskAntialiasMode: D2D1_ANTIALIAS_MODE;

    // An additional transform that may be applied to the mask in addition to the
    // current world transform.
    maskTransform: D2D1_MATRIX_3X2_F;

    // The opacity with which all of the content in the layer will be blended back to
    // the target when the layer is popped.
    opacity: Single;

    // An additional brush that can be applied to the layer. Only the opacity channel
    // is sampled from this brush and multiplied both with the layer content and the
    // over-all layer opacity.
    opacityBrush: ID2D1Brush;

    // Specifies if ClearType will be rendered into the layer.
    layerOptions: D2D1_LAYER_OPTIONS;
  end;
  {$EXTERNALSYM D2D1_LAYER_PARAMETERS}


  // Contains rendering options (hardware or software), pixel format, DPI
  // information, remoting options, and Direct3D support requirements for a render
  // target.
  PD2D1_RENDER_TARGET_PROPERTIES = ^D2D1_RENDER_TARGET_PROPERTIES;
  D2D1_RENDER_TARGET_PROPERTIES = record
    _type: D2D1_RENDER_TARGET_TYPE;
    _pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
    usage: D2D1_RENDER_TARGET_USAGE;
    minLevel: D2D1_FEATURE_LEVEL;
  end;
  {$EXTERNALSYM D2D1_RENDER_TARGET_PROPERTIES}

  // Contains the HWND, pixel size, and presentation options for an
  // ID2D1HwndRenderTarget.
  PD2D1_HWND_RENDER_TARGET_PROPERTIES = ^D2D1_HWND_RENDER_TARGET_PROPERTIES;
  D2D1_HWND_RENDER_TARGET_PROPERTIES = record
    _hwnd: HWND;
    _pixelSize: D2D1_SIZE_U;
    presentOptions: D2D1_PRESENT_OPTIONS;
  end;
  {$EXTERNALSYM D2D1_HWND_RENDER_TARGET_PROPERTIES}


  // Allows the drawing state to be atomically created. This also specifies the
  // drawing state that is saved into an IDrawingStateBlock object.
  PD2D1_DRAWING_STATE_DESCRIPTION = ^D2D1_DRAWING_STATE_DESCRIPTION;
  D2D1_DRAWING_STATE_DESCRIPTION = record
    antialiasMode: D2D1_ANTIALIAS_MODE;
    textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE;
    tag1: D2D1_TAG;
    tag2: D2D1_TAG;
    transform: D2D1_MATRIX_3X2_F;
  end;
  {$EXTERNALSYM D2D1_DRAWING_STATE_DESCRIPTION}


  // Allows additional parameters for factory creation.
  PD2D1_FACTORY_OPTIONS = ^D2D1_FACTORY_OPTIONS;
  D2D1_FACTORY_OPTIONS = record
    // Requests a certain level of debugging information from the debug layer.
    // This parameter is ignored if the debug layer DLL is not present.
    debugLevel: D2D1_DEBUG_LEVEL;
  end;
  {$EXTERNALSYM D2D1_FACTORY_OPTIONS}


  // INTERFACES /////////////////////////////////////////////////////////////////

  // Interface ID2D1Resource
  // =======================
  // The root interface for all resources in D2D.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Resource);'}
  {$EXTERNALSYM ID2D1Resource}
  ID2D1Resource = interface(IUnknown)
  ['{2cd90691-12e2-11dc-9fed-001143a055f9}']

    // Retrieve the factory associated with this resource.
    procedure GetFactory(out factory: ID2D1Factory); stdcall;

  end;
  IID_ID2D1Resource = ID2D1Resource;
  {$EXTERNALSYM IID_ID2D1Resource}


  // Interface ID2D1Image
  // ====================
  // Represents a producer of pixels that can fill an arbitrary 2D plane.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Image);'}
  {$EXTERNALSYM ID2D1Image}
  ID2D1Image = interface(ID2D1Resource)
  ['{65019f75-8da2-497c-b32c-dfa34e48ede6}']

  end;
  IID_ID2D1Image = ID2D1Image;
  {$EXTERNALSYM IID_ID2D1Image}


  // Interface ID2D1Bitmap
  // =====================
  // Root bitmap resource, linearly scaled on a draw call.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Bitmap);'}
  {$EXTERNALSYM ID2D1Bitmap}
  ID2D1Bitmap = interface(ID2D1Image)
  ['{a2296057-ea42-4099-983b-539fb6505426}']
    // Returns the size of the bitmap in resolution independent units.
    procedure GetSize(out size: D2D1_SIZE_F); stdcall;

    // Returns the size of the bitmap in resolution dependent units, (pixels).
    procedure GetPixelSize(out pixelSize: D2D1_SIZE_U); stdcall;

    // Retrieve the format of the bitmap.
    procedure GetPixelFormat(out pixelFormat: D2D1_PIXEL_FORMAT); stdcall;

    // Return the DPI of the bitmap.
    procedure GetDpi(out dpiX;
                     out dpiY: Single); stdcall;

    function CopyFromBitmap(var destPoint: D2D1_POINT_2U;
                            const bitmap: ID2D1Bitmap;
                            var srcRect: D2D1_RECT_U): HResult; stdcall;

    function CopyFromRenderTarget(var destPoint: D2D1_POINT_2U;
                                  const renderTarget: ID2D1RenderTarget;
                                  var srcRect: D2D1_RECT_U): HResult; stdcall;

    function CopyFromMemory(var dstRect: D2D1_RECT_U;
                            srcData: Pointer;
                            pitch: Cardinal): HResult; stdcall;

  end;
  IID_ID2D1Bitmap = ID2D1Bitmap;
  {$EXTERNALSYM IID_ID2D1Bitmap}


  // Interface ID2D1GradientStopCollection
  // =====================================
  // Represents an collection of gradient stops that can then be the source resource
  // for either a linear or radial gradient brush.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientStopCollection);'}
  {$EXTERNALSYM ID2D1GradientStopCollection}
  ID2D1GradientStopCollection = interface(ID2D1Resource)
  ['{2cd906a7-12e2-11dc-9fed-001143a055f9}']
    // Returns the number of stops in the gradient.
    function GetGradientStopCount(): UINT32; stdcall;

    // Copies the gradient stops from the collection into the caller's interface.
    // The returned colors have straight alpha.
    procedure GetGradientStops(out gradientStops: D2D1_GRADIENT_STOP;
                               gradientStopsCount: UINT32); stdcall;

    // Returns whether the interpolation occurs with 1.0 or 2.2 gamma.
    function GetColorInterpolationGamma(): D2D1_GAMMA; stdcall;

    function GetExtendMode(): D2D1_EXTEND_MODE; stdcall;
  end;
  IID_ID2D1GradientStopCollection = ID2D1GradientStopCollection;
  {$EXTERNALSYM IID_ID2D1GradientStopCollection}


  // Interface ID2D1Brush
  // ====================
  // The root brush interface. All brushes can be used to fill or pen a geometry.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Brush);'}
  {$EXTERNALSYM ID2D1Brush}
  ID2D1Brush = interface(ID2D1Resource)
  ['{2cd906a8-12e2-11dc-9fed-001143a055f9}']

    // Sets the opacity for when the brush is drawn over the entire fill of the brush.
    procedure SetOpacity(opacity: Single); stdcall;

    // Sets the transform that applies to everything drawn by the brush.
    procedure SetTransform(transform: D2D1_MATRIX_3X2_F); stdcall;

    function GetOpacity(): Single; stdcall;

    procedure GetTransform(out transform: D2D1_MATRIX_3X2_F); stdcall;

  end;
  IID_ID2D1Brush = ID2D1Brush;
  {$EXTERNALSYM IID_ID2D1Brush}


  // Interface ID2D1BitmapBrush
  // ==========================
  // A bitmap brush allows a bitmap to be used to fill a geometry.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BitmapBrush);'}
  {$EXTERNALSYM ID2D1BitmapBrush}
  ID2D1BitmapBrush = interface(ID2D1Brush)
  ['{2cd906aa-12e2-11dc-9fed-001143a055f9}']

    // Sets how the bitmap is to be treated outside of its natural extent on the X
    // axis.
    procedure SetExtendModeX(extendModeX: D2D1_EXTEND_MODE); stdcall;

    // Sets how the bitmap is to be treated outside of its natural extent on the X
    // axis.
    procedure SetExtendModeY(extendModeY: D2D1_EXTEND_MODE); stdcall;

    // Sets the interpolation mode used when this brush is used.
    procedure SetInterpolationMode(interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE); stdcall;

    // Sets the bitmap associated as the source of this brush.
    procedure SetBitmap(bitmap: ID2D1Bitmap); stdcall;

    function GetExtendModeX(): D2D1_EXTEND_MODE; stdcall;

    function GetExtendModeY(): D2D1_EXTEND_MODE; stdcall;

    function GetInterpolationMode(): D2D1_BITMAP_INTERPOLATION_MODE; stdcall;

    procedure GetBitmap(out bitmap: ID2D1Bitmap); stdcall;

  end;
  IID_ID2D1BitmapBrush = ID2D1BitmapBrush;
  {$EXTERNALSYM IID_ID2D1BitmapBrush}


  // Interface ID2D1SolidColorBrush
  // ==============================
  // Paints an area with a solid color.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SolidColorBrush);'}
  {$EXTERNALSYM ID2D1SolidColorBrush}
  ID2D1SolidColorBrush = interface(ID2D1Brush)
  ['{2cd906a9-12e2-11dc-9fed-001143a055f9}']

    procedure SetColor(color: D2D1_COLOR_F); stdcall;

    function  GetColor(): D2D1_COLOR_F; stdcall;

  end;
  IID_ID2D1SolidColorBrush = ID2D1SolidColorBrush;
  {$EXTERNALSYM IID_ID2D1SolidColorBrush}


  // Interface ID2D1LinearGradientBrush
  // ==================================
  // Paints an area with a linear gradient.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1LinearGradientBrush);'}
  {$EXTERNALSYM ID2D1LinearGradientBrush}
  ID2D1LinearGradientBrush = interface(ID2D1Brush)
  ['{2cd906ab-12e2-11dc-9fed-001143a055f9}']

    procedure SetStartPoint(startPoint: D2D1_POINT_2F); stdcall;

    // Sets the end point of the gradient in local coordinate space. This is not
    // influenced by the geometry being filled.
    procedure SetEndPoint(endPoint: D2D1_POINT_2F); stdcall;

    function GetStartPoint(): D2D1_POINT_2F; stdcall;

    function GetEndPoint(): D2D1_POINT_2F; stdcall;

    procedure GetGradientStopCollection(out gradientStopCollection: ID2D1GradientStopCollection); stdcall;

  end;
  IID_ID2D1LinearGradientBrush = ID2D1LinearGradientBrush;
  {$EXTERNALSYM IID_ID2D1LinearGradientBrush}


  // Interface ID2D1RadialGradientBrush
  // ==================================
  // Paints an area with a radial gradient.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1RadialGradientBrush);'}
  {$EXTERNALSYM ID2D1RadialGradientBrush}
  ID2D1RadialGradientBrush = interface(ID2D1Brush)
  ['{2cd906ac-12e2-11dc-9fed-001143a055f9}']

    // Sets the center of the radial gradient. This will be in local coordinates and
    // will not depend on the geometry being filled.
    procedure SetCenter(center: D2D1_POINT_2F); stdcall;

    // Sets offset of the origin relative to the radial gradient center.
    procedure SetGradientOriginOffset(gradientOriginOffset: D2D1_POINT_2F); stdcall;

    procedure SetRadiusX(radiusX: Single); stdcall;

    procedure SetRadiusY(radiusY: Single); stdcall;

    function GetCenter(): D2D1_POINT_2F; stdcall;

    function GetGradientOriginOffset(): D2D1_POINT_2F;

    function GetRadiusX(): Single; stdcall;

    function GetRadiusY(): Single; stdcall;

    procedure GetGradientStopCollection(out gradientStopCollection: ID2D1GradientStopCollection); stdcall;

  end;
  IID_ID2D1RadialGradientBrush = ID2D1RadialGradientBrush;
  {$EXTERNALSYM IID_ID2D1RadialGradientBrush}


  // Interface ID2D1StrokeStyle
  // ==========================
  // Resource interface that holds pen style properties.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1StrokeStyle);'}
  {$EXTERNALSYM ID2D1StrokeStyle}
  ID2D1StrokeStyle = interface(ID2D1Resource)
  ['{2cd9069d-12e2-11dc-9fed-001143a055f9}']

    function GetStartCap(): D2D1_CAP_STYLE;

    function GetEndCap(): D2D1_CAP_STYLE;

    function GetDashCap(): D2D1_CAP_STYLE;

    function GetMiterLimit(): Single;

    function GetLineJoin(): D2D1_LINE_JOIN;

    function GetDashOffset(): Single;

    function GetDashStyle(): D2D1_DASH_STYLE;

    function GetDashesCount(): UINT32;

    // Returns the dashes from the object into a user allocated array. The user must
    // call GetDashesCount to retrieve the required size.
    procedure GetDashes(out dashes: Single;
                        dashesCount: UINT32);

  end;
  IID_ID2D1StrokeStyle = ID2D1StrokeStyle;
  {$EXTERNALSYM IID_ID2D1StrokeStyle}


  // Interface ID2D1Geometry
  // =======================
  // Represents a geometry resource and defines a set of helper methods for
  // manipulating and measuring geometric shapes. Interfaces that inherit from
  // ID2D1Geometry define specific shapes.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Geometry);'}
  {$EXTERNALSYM ID2D1Geometry}
  ID2D1Geometry = interface(ID2D1Resource)
  ['{2cd906a1-12e2-11dc-9fed-001143a055f9}']

    // Retrieve the bounds of the geometry, with an optional applied transform.
    function GetBounds(worldTransform: D2D1_MATRIX_3X2_F;
                       out bounds: D2D1_RECT_F): HResult; stdcall;

    // Get the bounds of the corresponding geometry after it has been widened or have
    // an optional pen style applied.
    function GetWidenedBounds(strokeWidth: Single;
                              strokeStyle: ID2D1StrokeStyle;
                              worldTransform: D2D1_MATRIX_3X2_F;
                              flatteningTolerance: Single;
                              out bounds: D2D1_RECT_F): HResult; stdcall;

    // Checks to see whether the corresponding penned and widened geometry contains the
    // given point.
    function StrokeContainsPoint(point: D2D1_POINT_2F;
                                 strokeWidth: Single;
                                 strokeStyle: ID2D1StrokeStyle;
                                 worldTransform: D2D1_MATRIX_3X2_F;
                                 flatteningTolerance: Single;
                                 out contains: BOOL): HResult; stdcall;

    // Test whether the given fill of this geometry would contain this point.
    function FillContainsPoint(point: D2D1_POINT_2F;
                               worldTransform: D2D1_MATRIX_3X2_F;
                               flatteningTolerance: Single;
                               out contains: BOOL): HResult; stdcall;

    // Compare how one geometry intersects or contains another geometry.
    function CompareWithGeometry(inputGeometry: ID2D1Geometry;
                                 inputGeometryTransform: D2D1_MATRIX_3X2_F;
                                 flatteningTolerance: Single;
                                 out relation: D2D1_GEOMETRY_RELATION): HResult; stdcall;

    // Converts a geometry to a simplified geometry that has arcs and quadratic beziers
    // removed.
    function Simplify(simplificationOption: D2D1_GEOMETRY_SIMPLIFICATION_OPTION;
                      worldTransform: D2D1_MATRIX_3X2_F;
                      flatteningTolerance: Single;
                      geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;

    // Tessellates a geometry into triangles.
    function Tessellate(worldTransform: D2D1_MATRIX_3X2_F;
                        flatteningTolerance: Single;
                        tessellationSink: ID2D1TessellationSink): HResult; stdcall;

    // Performs a combine operation between the two geometries to produce a resulting
    // geometry.
    function CombineWithGeometry(inputGeometry: ID2D1Geometry;
                                 combineMode: D2D1_COMBINE_MODE;
                                 inputGeometryTransform: D2D1_MATRIX_3X2_F;
                                 flatteningTolerance: Single;
                                 geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;

    // Computes the outline of the geometry. The result is written back into a
    // simplified geometry sink.
    function Outline(worldTransform: D2D1_MATRIX_3X2_F;
                     flatteningTolerance: Single;
                     geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;

    // Computes the area of the geometry.
    function ComputeArea(worldTransform: D2D1_MATRIX_3X2_F;
                         flatteningTolerance: Single;
                         out area: Single): HResult; stdcall;

    // Computes the length of the geometry.
    function ComputeLength(worldTransform: D2D1_MATRIX_3X2_F;
                           flatteningTolerance: Single;
                           out length: Single): HResult; stdcall;

    // Computes the point and tangent a given distance along the path.
    function ComputePointAtLength(length: Single;
                                  worldTransform: D2D1_MATRIX_3X2_F;
                                  flatteningTolerance: Single;
                                  out point: D2D1_POINT_2F;
                                  out unitTangentVector: D2D1_POINT_2F): HResult; stdcall;

    // Get the geometry and widen it as well as apply an optional pen style.
    function Widen(strokeWidth: Single;
                   strokeStyle: ID2D1StrokeStyle;
                   worldTransform: D2D1_MATRIX_3X2_F;
                   flatteningTolerance: Single;
                   geometrySink: ID2D1SimplifiedGeometrySink): HResult; stdcall;


  end;
  IID_ID2D1Geometry = ID2D1Geometry;
  {$EXTERNALSYM IID_ID2D1Geometry}


  // Interface ID2D1RectangleGeometry
  // ================================
  // Describes a two-dimensional rectangle.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1RectangleGeometry);'}
  {$EXTERNALSYM ID2D1RectangleGeometry}
  ID2D1RectangleGeometry = interface(ID2D1Geometry)
  ['{2cd906a2-12e2-11dc-9fed-001143a055f9}']

    procedure GetRect(out rect: D2D1_RECT_F); stdcall;

  end;
  IID_ID2D1RectangleGeometry = ID2D1RectangleGeometry;
  {$EXTERNALSYM IID_ID2D1RectangleGeometry}


  // Interface ID2D1RoundedRectangleGeometry
  // =======================================
  // Describes a rounded rectangle.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1RoundedRectangleGeometry);'}
  {$EXTERNALSYM ID2D1RoundedRectangleGeometry}
  ID2D1RoundedRectangleGeometry = interface(ID2D1Geometry)
  ['{2cd906a3-12e2-11dc-9fed-001143a055f9}']

    procedure GetRoundedRect(out roundedRect: D2D1_ROUNDED_RECT); stdcall;

  end;
  IID_ID2D1RoundedRectangleGeometry = ID2D1RoundedRectangleGeometry;
  {$EXTERNALSYM IID_ID2D1RoundedRectangleGeometry}


  // Interface ID2D1EllipseGeometry
  // ==============================
  // Represents an ellipse.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1EllipseGeometry);'}
  {$EXTERNALSYM ID2D1EllipseGeometry}
  ID2D1EllipseGeometry = interface(ID2D1Geometry)
  ['{2cd906a4-12e2-11dc-9fed-001143a055f9}']

    procedure GetEllipse(out ellipse: D2D1_ELLIPSE); stdcall;

  end;
  IID_ID2D1EllipseGeometry = ID2D1EllipseGeometry;
  {$EXTERNALSYM IID_ID2D1EllipseGeometry}


  // Interface ID2D1GeometryGroup
  // ============================
  // Represents a composite geometry, composed of other ID2D1Geometry objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GeometryGroup);'}
  {$EXTERNALSYM ID2D1GeometryGroup}
  ID2D1GeometryGroup = interface(ID2D1Geometry)
  ['{2cd906a6-12e2-11dc-9fed-001143a055f9}']

    function GetFillMode(): D2D1_FILL_MODE; stdcall;

    function GetSourceGeometryCount(): UINT32; stdcall;

    procedure GetSourceGeometries(out geometries: ID2D1Geometry;
                                  geometriesCount: UINT32); stdcall;

  end;
  IID_ID2D1GeometryGroup = ID2D1GeometryGroup;
  {$EXTERNALSYM IID_ID2D1GeometryGroup}


  // Interface ID2D1TransformedGeometry
  // ==================================
  // Represents a geometry that has been transformed.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TransformedGeometry);'}
  {$EXTERNALSYM ID2D1TransformedGeometry}
  ID2D1TransformedGeometry = interface(ID2D1Geometry)
  ['{2cd906bb-12e2-11dc-9fed-001143a055f9}']

    procedure GetSourceGeometry(out sourceGeometry: ID2D1Geometry); stdcall;

    procedure GetTransform(out transform: D2D1_MATRIX_3X2_F); stdcall;

  end;
  IID_ID2D1TransformedGeometry = ID2D1TransformedGeometry;
  {$EXTERNALSYM IID_ID2D1TransformedGeometry}



  // Interface ID2D1SimplifiedGeometrySink
  // =====================================
  // Describes a geometric path that does not contain quadratic bezier curves or
  // arcs.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SimplifiedGeometrySink);'}
  {$EXTERNALSYM ID2D1SimplifiedGeometrySink}
  ID2D1SimplifiedGeometrySink = interface(IUnknown)
  ['{2cd9069e-12e2-11dc-9fed-001143a055f9}']

    procedure SetFillMode(fillMode: D2D1_FILL_MODE);

    procedure SetSegmentFlags(vertexFlags: D2D1_PATH_SEGMENT);

    procedure BeginFigure(startPoint: D2D1_POINT_2F;
                          figureBegin: D2D1_FIGURE_BEGIN);

    procedure AddLines(points: PD2D1_POINT_2F; // pointer to array of points
                       pointsCount: UINT32);

    procedure AddBeziers(beziers: PD2D1_BEZIER_SEGMENT; // pointer to array of beziers
                         beziersCount: UINT32);

    procedure EndFigure(figureEnd: D2D1_FIGURE_END);

    function Close(): HResult; stdcall;

  end;
  IID_ID2D1SimplifiedGeometrySink = ID2D1SimplifiedGeometrySink;
  {$EXTERNALSYM IID_ID2D1SimplifiedGeometrySink}



  // Interface ID2D1GeometrySink
  // ===========================
  // Describes a geometric path that can contain lines, arcs, cubic Bezier curves,
  // and quadratic Bezier curves.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GeometrySink);'}
  {$EXTERNALSYM ID2D1GeometrySink}
  ID2D1GeometrySink = interface(ID2D1SimplifiedGeometrySink)
  ['{2cd9069f-12e2-11dc-9fed-001143a055f9}']

    procedure AddLine(point: D2D1_POINT_2F); stdcall;

    procedure AddBezier(bezier: D2D1_BEZIER_SEGMENT); stdcall;

    procedure AddQuadraticBezier(bezier: D2D1_QUADRATIC_BEZIER_SEGMENT); stdcall;

    procedure AddQuadraticBeziers(beziers: PD2D1_QUADRATIC_BEZIER_SEGMENT; // pointer to array of beziers
                                  beziersCount: UINT32); stdcall;

    procedure AddArc(arc: D2D1_ARC_SEGMENT); stdcall;

  end;
  IID_ID2D1GeometrySink = ID2D1GeometrySink;
  {$EXTERNALSYM IID_ID2D1GeometrySink}


  // Interface ID2D1TessellationSink
  // ===============================
  // Populates an ID2D1Mesh object with triangles.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1TessellationSink);'}
  {$EXTERNALSYM ID2D1TessellationSink}
  ID2D1TessellationSink = interface(IUnknown)
  ['{2cd906c1-12e2-11dc-9fed-001143a055f9}']

    procedure AddTriangles(triangles: PD2D1_TRIANGLE;    // pointer to array of triangles
                           trianglesCount: UINT32); stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_ID2D1TessellationSink = ID2D1TessellationSink;
  {$EXTERNALSYM IID_ID2D1TessellationSink}


  // Interface ID2D1PathGeometry
  // ===========================
  // Represents a complex shape that may be composed of arcs, curves, and lines.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PathGeometry);'}
  {$EXTERNALSYM ID2D1PathGeometry}
  ID2D1PathGeometry = interface(ID2D1Geometry)
  ['{2cd906a5-12e2-11dc-9fed-001143a055f9}']

    // Opens a geometry sink that will be used to create this path geometry.
    function Open(out geometrySink: ID2D1GeometrySink): HResult; stdcall;

    // Retrieve the contents of this geometry. The caller passes an implementation of a
    // ID2D1GeometrySink interface to receive the data.
    function Stream(geometrySink: ID2D1GeometrySink): HResult; stdcall;

    function GetSegmentCount(out count: UINT32): HResult; stdcall;

    function GetFigureCount(out count: UINT32): HResult; stdcall;

  end;
  IID_ID2D1PathGeometry = ID2D1PathGeometry;
  {$EXTERNALSYM IID_ID2D1PathGeometry}


  // Interface ID2D1Mesh
  // ===================
  // Represents a set of vertices that form a list of triangles.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Mesh);'}
  {$EXTERNALSYM ID2D1Mesh}
  ID2D1Mesh = interface(ID2D1Resource)
  ['{2cd906c2-12e2-11dc-9fed-001143a055f9}']

    // Opens the mesh for population.
    function Open(out tessellationSink: ID2D1TessellationSink): HResult; stdcall;

  end;
  IID_ID2D1Mesh = ID2D1Mesh;
  {$EXTERNALSYM IID_ID2D1Mesh}


  // Interface ID2D1Layer
  // ====================
  // Represents the backing store required to render a layer.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Layer);'}
  {$EXTERNALSYM ID2D1Layer}
  ID2D1Layer = interface(ID2D1Resource)
  ['{2cd9069b-12e2-11dc-9fed-001143a055f9}']

    function GetSize(): D2D1_SIZE_F; stdcall;

  end;
  IID_ID2D1Layer = ID2D1Layer;
  {$EXTERNALSYM IID_ID2D1Layer}


  // Interface ID2D1DrawingStateBlock
  // ================================
  // Represents the drawing state of a render target: the antialiasing mode,
  // transform, tags, and text-rendering options.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DrawingStateBlock);'}
  {$EXTERNALSYM ID2D1DrawingStateBlock}
  ID2D1DrawingStateBlock = interface(ID2D1Resource)
  ['{28506e39-ebf6-46a1-bb47-fd85565ab957}']

    // Retrieves the state currently contained within this state block resource.
    procedure GetDescription(out stateDescription: D2D1_DRAWING_STATE_DESCRIPTION); stdcall;

    // Sets the state description of this state block resource.
    procedure SetDescription(stateDescription: D2D1_DRAWING_STATE_DESCRIPTION); stdcall;

    // Sets the text rendering parameters of this state block resource.
    procedure SetTextRenderingParams(textRenderingParams: IDWriteRenderingParams); stdcall;

    // Retrieves the text rendering parameters contained within this state block
    // resource. If a Nil text rendering parameter was specified, Nil will be
    // returned.
    procedure GetTextRenderingParams(out textRenderingParams: IDWriteRenderingParams); stdcall;

  end;
  IID_ID2D1DrawingStateBlock = ID2D1DrawingStateBlock;
  {$EXTERNALSYM IID_ID2D1DrawingStateBlock}



  // Interface ID2D1RenderTarget
  // ===========================
  // Represents an object that can receive drawing commands. Interfaces that inherit
  // from ID2D1RenderTarget render the drawing commands they receive in different
  // ways.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1RenderTarget);'}
  {$EXTERNALSYM ID2D1RenderTarget}
  ID2D1RenderTarget = interface(ID2D1Resource)
  ['{2cd90694-12e2-11dc-9fed-001143a055f9}']

    // Create a D2D bitmap by copying from memory, or create uninitialized.
    function CreateBitmap(size: D2D1_SIZE_U;
                          srcData: Pointer;
                          pitch: UINT32;
                          const bitmapProperties: D2D1_BITMAP_PROPERTIES;
                          out bitmap: ID2D1Bitmap): HResult; stdcall;

    // Create a D2D bitmap by copying a WIC bitmap.
    function CreateBitmapFromWicBitmap(const wicBitmapSource: IWICBitmapSource;
                                       bitmapProperties: PD2D1_BITMAP_PROPERTIES;
                                       out bitmap: ID2D1Bitmap): HResult; stdcall;

    // Create a D2D bitmap by sharing bits from another resource. The bitmap must be
    // compatible with the render target for the call to succeed. For example, an
    // IWICBitmap can be shared with a software target, or a DXGI surface can be shared
    // with a DXGI render target.
    function CreateSharedBitmap(const riid: REFIID;
                                var data: Pointer;
                                bitmapProperties: PD2D1_BITMAP_PROPERTIES;
                                out bitmap: ID2D1Bitmap): HResult; stdcall;

    // Creates a bitmap brush. The bitmap is scaled, rotated, skewed or tiled to fill
    // or pen a geometry.
    function CreateBitmapBrush(const bitmap: ID2D1Bitmap;
                               bitmapBrushProperties: PD2D1_BITMAP_BRUSH_PROPERTIES;
                               brushProperties: PD2D1_BRUSH_PROPERTIES;
                               out bitmapBrush: ID2D1BitmapBrush): HResult; stdcall;

    function CreateSolidColorBrush(const color: D2D1_COLOR_F;
                                   brushProperties: PD2D1_BRUSH_PROPERTIES;
                                   out solidColorBrush: ID2D1SolidColorBrush): HResult; stdcall;

    // A gradient stop collection represents a set of stops in an ideal unit length.
    // This is the source resource for a linear gradient and radial gradient brush.

    // <param name="colorInterpolationGamma">Specifies which space the color
    // interpolation occurs in.</param>
    // <param name="extendMode">Specifies how the gradient will be extended outside of
    // the unit length.</param>
    function CreateGradientStopCollection(const gradientStops: PD2D1_GRADIENT_STOP;
                                          gradientStopsCount: UINT32;
                                          colorInterpolationGamma: D2D1_GAMMA;
                                          extendMode: D2D1_EXTEND_MODE;
                                          out gradientStopCollection: ID2D1GradientStopCollection): HResult; stdcall;

    function CreateLinearGradientBrush(const linearGradientBrushProperties: D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
                                       brushProperties: PD2D1_BRUSH_PROPERTIES;
                                       gradientStopCollection: ID2D1GradientStopCollection;
                                       out linearGradientBrush: ID2D1LinearGradientBrush): HResult; stdcall;

    function CreateRadialGradientBrush(const radialGradientBrushProperties: D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
                                       brushProperties: PD2D1_BRUSH_PROPERTIES;
                                       gradientStopCollection: ID2D1GradientStopCollection;
                                       out radialGradientBrush: ID2D1RadialGradientBrush): HResult; stdcall;

    // Creates a bitmap render target whose bitmap can be used as a source for
    // rendering in the API.

    // <param name="desiredSize">The requested size of the target in DIPs. If the pixel
    // size is not specified, the DPI is inherited from the parent target. However, the
    // render target will never contain a fractional number of pixels.</param>
    // <param name="desiredPixelSize">The requested size of the render target in
    // pixels. If the DIP size is also specified, the DPI is calculated from these two
    // values. If the desired size is not specified, the DPI is inherited from the
    // parent render target. If neither value is specified, the compatible render
    // target will be the same size and have the same DPI as the parent target.</param>
    // <param name="desiredFormat">The desired pixel format. The format must be
    // compatible with the parent render target type. If the format is not specified,
    // it will be inherited from the parent render target.</param>
    // <param name="options">Allows the caller to retrieve a GDI compatible render
    // target.</param>
    // <param name="bitmapRenderTarget">The returned bitmap render target.</param>
    function CreateCompatibleRenderTarget(desiredSize: PD2D1_SIZE_F;
                                          desiredPixelSize: PD2D1_SIZE_U;
                                          desiredFormat: PD2D1_PIXEL_FORMAT;
                                          options: D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS;
                                          out bitmapRenderTarget: ID2D1BitmapRenderTarget): HResult; stdcall;

    // Creates a layer resource that can be used on any target and which will resize
    // under the covers if necessary.

    // <param name="size">The resolution independent minimum size hint for the layer
    // resource. Specify this to prevent unwanted reallocation of the layer backing
    // store. The size is in DIPs, but, it is unaffected by the current world
    // transform. If the size is unspecified, the returned resource is a placeholder
    // and the backing store will be allocated to be the minimum size that can hold the
    // content when the layer is pushed.</param>
    function CreateLayer(size: PD2D1_SIZE_F;
                         out layer: ID2D1Layer): HResult; stdcall;

    // Create a D2D mesh.
    function CreateMesh(out mesh: ID2D1Mesh): HResult; stdcall;

    procedure DrawLine(point0: D2D1_POINT_2F;
                       point1: D2D1_POINT_2F;
                       const brush: ID2D1Brush;
                       strokeWidth: Single = 1.0;
                       const strokeStyle: ID2D1StrokeStyle = Nil); stdcall;

    procedure DrawRectangle(const rect: D2D1_RECT_F;
                            const brush: ID2D1Brush;
                            strokeWidth: Single = 1.0;
                            const strokeStyle: ID2D1StrokeStyle = Nil); stdcall;

    procedure FillRectangle(const rect: D2D1_RECT_F;
                            const brush: ID2D1Brush); stdcall;

    procedure DrawRoundedRectangle(const roundedRect: D2D1_ROUNDED_RECT;
                                   const brush: ID2D1Brush;
                                   const strokeWidth: Single = 1.0;
                                   const strokeStyle: ID2D1StrokeStyle = Nil); stdcall;

    procedure FillRoundedRectangle(const roundedRect: D2D1_ROUNDED_RECT;
                                   const brush: ID2D1Brush); stdcall;

    procedure DrawEllipse(const ellipse: D2D1_ELLIPSE;
                          const brush: ID2D1Brush;
                          const strokeWidth: Single = 1.0;
                          const strokeStyle: ID2D1StrokeStyle = Nil); stdcall;

    procedure FillEllipse(const ellipse: D2D1_ELLIPSE;
                          const brush: ID2D1Brush);

    procedure DrawGeometry(const geometry: ID2D1Geometry;
                           const brush: ID2D1Brush;
                           const strokeWidth: Single = 1.0;
                           const strokeStyle: ID2D1StrokeStyle = Nil); stdcall;

    // <param name="opacityBrush">An optionally specified opacity brush. Only the alpha
    // channel of the corresponding brush will be sampled and will be applied to the
    // entire fill of the geometry. If this brush is specified, the fill brush must be
    // a bitmap brush with an extend mode of D2D1_EXTEND_MODE_CLAMP.</param>
    procedure FillGeometry(const geometry: ID2D1Geometry;
                           const brush: ID2D1Brush;
                           const opacityBrush: ID2D1Brush = Nil); stdcall;

    // Fill a mesh. Since meshes can only render aliased content, the render target
    // antialiasing mode must be set to aliased.
    procedure FillMesh(const mesh: ID2D1Mesh;
                       const brush: ID2D1Brush); stdcall;

    // Fill using the alpha channel of the supplied opacity mask bitmap. The brush
    // opacity will be modulated by the mask. The render target antialiasing mode must
    // be set to aliased.
    procedure FillOpacityMask(opacityMask: ID2D1Bitmap;
                              brush: ID2D1Brush;
                              content: D2D1_OPACITY_MASK_CONTENT;
                              destinationRectangle: PD2D1_RECT_F = Nil;
                              sourceRectangle: PD2D1_RECT_F = Nil); stdcall;

    procedure DrawBitmap(const bitmap: ID2D1Bitmap;
                         destinationRectangle: PD2D1_RECT_F = Nil;
                         opacity: Single = 1.0;
                         interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
                         sourceRectangle: PD2D1_RECT_F = Nil); stdcall;

    // Draws the text within the given layout rectangle and by default also performs
    // baseline snapping.
    procedure DrawText(_string: PWideChar;
                       const stringLength: UINT32;
                       const textFormat: IDWriteTextFormat;
                       const layoutRect: D2D1_RECT_F;
                       defaultFillBrush: ID2D1Brush;
                       options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_NONE;
                       measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;

    // Draw a text layout object. If the layout is not subsequently changed, this can
    // be more efficient than DrawText when drawing the same layout repeatedly.

    // <param name="options">The specified text options. If D2D1_DRAW_TEXT_OPTIONS_CLIP
    // is used, the text is clipped to the layout bounds. These bounds are derived from
    // the origin and the layout bounds of the corresponding IDWriteTextLayout object.
    // </param>
    procedure DrawTextLayout(origin: D2D1_POINT_2F;
                             const textLayout: IDWriteTextLayout;
                             const defaultFillBrush: ID2D1Brush;
                             options: D2D1_DRAW_TEXT_OPTIONS = D2D1_DRAW_TEXT_OPTIONS_NONE); stdcall;

    procedure DrawGlyphRun(baselineOrigin: D2D1_POINT_2F;
                           glyphRun: DWRITE_GLYPH_RUN;
                           const foregroundBrush: ID2D1Brush;
                           measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;

    procedure SetTransform(const transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure GetTransform(var transform: D2D1_MATRIX_3X2_F); stdcall;

    procedure SetAntialiasMode(antialiasMode: D2D1_ANTIALIAS_MODE); stdcall;

    function GetAntialiasMode(): D2D1_ANTIALIAS_MODE; stdcall;

    procedure SetTextAntialiasMode(textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE); stdcall;

    function GetTextAntialiasMode(): D2D1_TEXT_ANTIALIAS_MODE; stdcall;

    procedure SetTextRenderingParams(const textRenderingParams: IDWriteRenderingParams = Nil); stdcall;

    // Retrieve the text render parameters. NOTE: If Nil is specified to
    // SetTextRenderingParameters, Nil will be returned.
    procedure GetTextRenderingParams(out textRenderingParams: IDWriteRenderingParams); stdcall;

    // Set a tag to correspond to the succeeding primitives. If an error occurs
    // rendering a primitive, the tags can be returned from the Flush or EndDraw call.
    procedure SetTags(tag1: D2D1_TAG;
                      tag2: D2D1_TAG); stdcall;

    // Retrieves the currently set tags. This does not retrieve the tags corresponding
    // to any primitive that is in error.
    procedure GetTags(tag1: PD2D1_TAG = Nil;
                      tag2: PD2D1_TAG = Nil); stdcall;

    // Start a layer of drawing calls. The way in which the layer must be resolved is
    // specified first as well as the logical resource that stores the layer
    // parameters. The supplied layer resource might grow if the specified content
    // cannot fit inside it. The layer will grow monotonically on each axis.  If a NULL
    // ID2D1Layer is provided, then a layer resource will be allocated automatically.
    procedure PushLayer(layerParameters: D2D1_LAYER_PARAMETERS;
                        layer: ID2D1Layer); stdcall;

    // Ends a layer that was defined with particular layer resources.
    procedure PopLayer(); stdcall;

    function Flush(tag1: PD2D1_TAG = Nil;
                   tag2: PD2D1_TAG = Nil): HResult; stdcall;

    // Gets the current drawing state and saves it into the supplied
    // IDrawingStatckBlock.
    procedure SaveDrawingState(var drawingStateBlock: ID2D1DrawingStateBlock); stdcall;

    // Copies the state stored in the block interface.
    procedure RestoreDrawingState(drawingStateBlock: ID2D1DrawingStateBlock); stdcall;

    // Pushes a clip. The clip can be antialiased. The clip must be axis aligned. If
    // the current world transform is not axis preserving, then the bounding box of the
    // transformed clip rect will be used. The clip will remain in effect until a
    // PopAxisAligned clip call is made.
    procedure PushAxisAlignedClip(clipRect: D2D1_RECT_F;
                                  antialiasMode: D2D1_ANTIALIAS_MODE); stdcall;

    procedure PopAxisAlignedClip(); stdcall;

    procedure Clear(const clearColor: D2D1_COLOR_F); stdcall;

    // Start drawing on this render target. Draw calls can only be issued between a
    // BeginDraw and EndDraw call.
    procedure BeginDraw(); stdcall;

    // Ends drawing on the render target, error results can be retrieved at this time,
    // or when calling flush.
    function EndDraw(tag1: PD2D1_TAG = Nil;
                     tag2: PD2D1_TAG = Nil): HResult; stdcall;

    procedure GetPixelFormat(out pixelFormat: D2D1_PIXEL_FORMAT); stdcall;

    // Sets the DPI on the render target. This results in the render target being
    // interpreted to a different scale. Neither DPI can be negative. If zero is
    // specified for both, the system DPI is chosen. If one is zero and the other
    // unspecified, the DPI is not changed.
    procedure SetDpi(dpiX: Single;
                     dpiY: Single); stdcall;

    // Return the current DPI from the target.
    procedure GetDpi(out dpiX: Single;
                     out dpiY: Single); stdcall;

    // Returns the size of the render target in DIPs.
    procedure GetSize(out size: D2D1_SIZE_F); stdcall;

    // Returns the size of the render target in pixels.
    procedure GetPixelSize(out pixelSize: D2D1_SIZE_U); stdcall;

    // Returns the maximum bitmap and render target size that is guaranteed to be
    // supported by the render target.
    function GetMaximumBitmapSize(): UINT32; stdcall;

    // Returns true if the given properties are supported by this render target.
    // The DPI is ignored. NOTE: If the render target type is software, then neither
    // D2D1_FEATURE_LEVEL_9 nor D2D1_FEATURE_LEVEL_10 will be considered to be
    // supported.
    function IsSupported(const renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES): BOOL; stdcall;

  end;
  IID_ID2D1RenderTarget = ID2D1RenderTarget;
  {$EXTERNALSYM IID_ID2D1RenderTarget}



  // Interface ID2D1BitmapRenderTarget
  // =================================
  // Renders to an intermediate texture created by the CreateCompatibleRenderTarget
  // method.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BitmapRenderTarget);'}
  {$EXTERNALSYM ID2D1BitmapRenderTarget}
  ID2D1BitmapRenderTarget = interface(ID2D1RenderTarget)
  ['{2cd90694-12e2-11dc-9fed-001143a055f9}']

    function GetBitmap(out bitmap: ID2D1Bitmap): HResult; stdcall;

  end;
  IID_ID2D1BitmapRenderTarget = ID2D1BitmapRenderTarget;
  {$EXTERNALSYM IID_ID2D1BitmapRenderTarget}


  // Interface ID2D1HwndRenderTarget
  // ===============================
  // Renders drawing instructions to a window.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1HwndRenderTarget);'}
  {$EXTERNALSYM ID2D1HwndRenderTarget}
  ID2D1HwndRenderTarget = interface(ID2D1RenderTarget)
  ['{2cd90698-12e2-11dc-9fed-001143a055f9}']

    function CheckWindowState(): D2D1_WINDOW_STATE; stdcall;

    // Resize the buffer underlying the render target. This operation might fail if
    // there is insufficient video memory or system memory, or if the render target is
    // resized beyond the maximum bitmap size. If the method fails, the render target
    // will be placed in a zombie state and D2DERR_RECREATE_TARGET will be returned
    // from it when EndDraw is called. In addition an appropriate failure result will
    // be returned from Resize.
    function Resize(var pixelSize: D2D1_SIZE_U): HResult; stdcall;

    function GetHwnd(): HWND; stdcall;

  end;
  IID_ID2D1HwndRenderTarget = ID2D1HwndRenderTarget;
  {$EXTERNALSYM IID_ID2D1HwndRenderTarget}


  // Interface ID2D1GdiInteropRenderTarget
  // =====================================
  // Provides access to an device context that can accept GDI drawing commands.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiInteropRenderTarget);'}
  {$EXTERNALSYM ID2D1GdiInteropRenderTarget}
  ID2D1GdiInteropRenderTarget = interface(IUnknown)
  ['{e0db51c3-6f77-4bae-b3d5-e47509b35838}']

    function GetDC(mode: D2D1_DC_INITIALIZE_MODE;
                   out hdc: HDC): HResult; stdcall;

    function ReleaseDC(update: TRECT): HResult; stdcall;

  end;
  IID_ID2D1GdiInteropRenderTarget = ID2D1GdiInteropRenderTarget;
  {$EXTERNALSYM IID_ID2D1GdiInteropRenderTarget}


  // Interface ID2D1DCRenderTarget
  // =============================
  // Issues drawing commands to a GDI device context.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DCRenderTarget);'}
  {$EXTERNALSYM ID2D1DCRenderTarget}
  ID2D1DCRenderTarget = interface(ID2D1RenderTarget)
  ['{1c51bc64-de61-46fd-9899-63a5d8f03950}']

    function BindDC(hDC: HDC;
                    pSubRect: TRECT): HResult; stdcall;

  end;
  IID_ID2D1DCRenderTarget = ID2D1DCRenderTarget;
  {$EXTERNALSYM IID_ID2D1DCRenderTarget}


  // Interface ID2D1Factory
  // ======================
  // The root factory interface for all of D2D's objects.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Factory);'}
  {$EXTERNALSYM ID2D1Factory}
  ID2D1Factory = interface(IUnknown)
  ['{06152247-6f50-465a-9245-118bfd3b6007}']

    // Cause the factory to refresh any system metrics that it might have been snapped
    // on factory creation.
    function ReloadSystemMetrics(): HResult; stdcall;

    // Retrieves the current desktop DPI. To refresh this, call ReloadSystemMetrics.
    procedure GetDesktopDpi(out dpiX: Single;
                            out dpiY: Single); stdcall;

    function CreateRectangleGeometry(const rectangle: D2D1_RECT_F;
                                     out rectangleGeometry: ID2D1RectangleGeometry): HResult; stdcall;

    function CreateRoundedRectangleGeometry(const roundedRectangle: D2D1_ROUNDED_RECT;
                                            out roundedRectangleGeometry: ID2D1RoundedRectangleGeometry): HResult; stdcall;

    function CreateEllipseGeometry(const ellipse: D2D1_ELLIPSE;
                                   out ellipseGeometry: ID2D1EllipseGeometry): HResult; stdcall;

    // Create a geometry which holds other geometries.
    function CreateGeometryGroup(fillMode: D2D1_FILL_MODE;
                                 geometries: ID2D1Geometry; // pointer to array of geometries
                                 geometriesCount: UINT32;
                                 out geometryGroup: ID2D1GeometryGroup): HResult; stdcall;

    function CreateTransformedGeometry(const sourceGeometry: ID2D1Geometry;
                                       transform: D2D1_MATRIX_3X2_F;
                                       out transformedGeometry: ID2D1TransformedGeometry): HResult; stdcall;

    // Returns an initially empty path geometry interface. A geometry sink is created
    // off the interface to populate it.
    function CreatePathGeometry(out pathGeometry: ID2D1PathGeometry): HResult; stdcall;

    // Allows a non-default stroke style to be specified for a given geometry at draw
    // time.
    function CreateStrokeStyle(const strokeStyleProperties: D2D1_STROKE_STYLE_PROPERTIES;
                               dashes: PSingle;
                               dashesCount: UINT32;
                               out strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    // Creates a new drawing state block, this can be used in subsequent
    // SaveDrawingState and RestoreDrawingState operations on the render target.
    function CreateDrawingStateBlock(drawingStateDescription: PD2D1_DRAWING_STATE_DESCRIPTION;
                                     textRenderingParams: IDWriteRenderingParams;
                                     out drawingStateBlock: ID2D1DrawingStateBlock): HResult; stdcall;

    // Creates a render target which is a source of bitmaps.
    function CreateWicBitmapRenderTarget(target: IWICBitmap;
                                         renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
                                         out renderTarget: ID2D1RenderTarget): HResult; stdcall;

    // Creates a render target that appears on the display.
    function CreateHwndRenderTarget(const renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
                                    const hwndRenderTargetProperties: D2D1_HWND_RENDER_TARGET_PROPERTIES;
                                    out hwndRenderTarget: ID2D1HwndRenderTarget): HResult; stdcall;

    // Creates a render target that draws to a DXGI Surface. The device that owns the
    // surface is used for rendering.
    function CreateDxgiSurfaceRenderTarget(const dxgiSurface: IDXGISurface;
                                           var renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
                                           out renderTarget: ID2D1RenderTarget): HResult; stdcall;

    // Creates a render target that draws to a GDI device context.
    function CreateDCRenderTarget(const renderTargetProperties: D2D1_RENDER_TARGET_PROPERTIES;
                                  out dcRenderTarget: ID2D1DCRenderTarget): HResult; stdcall;

  end;
  IID_ID2D1Factory = ID2D1Factory;
  {$EXTERNALSYM IID_ID2D1Factory}



// FUNCTIONS ///////////////////////////////////////////////////////////////////

    //
    // This export cannot be in a namespace because compiler name mangling isn't consistent
    // also, this must be 'C' callable.
    //

    function D2D1CreateFactory(factoryType: D2D1_FACTORY_TYPE;
                               const riid: REFIID;
                               pFactoryOptions: PD2D1_FACTORY_OPTIONS;
                               out ppIFactory): HResult; stdcall;
    {$EXTERNALSYM D2D1CreateFactory}

    procedure D2D1MakeRotateMatrix(angle: Single;
                                   center: D2D1_POINT_2F;
                                   out matrix: D2D1_MATRIX_3X2_F); stdcall;
    {$EXTERNALSYM D2D1MakeRotateMatrix}

    procedure D2D1MakeSkewMatrix(angleX: Single;
                                 angleY: Single;
                                 center: D2D1_POINT_2F;
                                 out matrix: D2D1_MATRIX_3X2_F); stdcall;
    {$EXTERNALSYM D2D1MakeSkewMatrix}

    function D2D1IsMatrixInvertible(matrix: D2D1_MATRIX_3X2_F): BOOL; stdcall;
    {$EXTERNALSYM D2D1IsMatrixInvertible}

    function D2D1InvertMatrix(var matrix: D2D1_MATRIX_3X2_F): BOOL; stdcall;
    {$EXTERNALSYM D2D1InvertMatrix}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation

const
  D2D1Lib                = 'D2d1.dll';
  {$EXTERNALSYM D2D1Lib}


{$WARN SYMBOL_PLATFORM OFF}
    function D2D1CreateFactory;      external D2D1Lib name 'D2D1CreateFactory' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
    procedure D2D1MakeRotateMatrix;  external D2D1Lib name 'D2D1MakeRotateMatrix' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
    procedure D2D1MakeSkewMatrix;    external D2D1Lib name 'D2D1MakeSkewMatrix' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
    function D2D1IsMatrixInvertible; external D2D1Lib name 'D2D1IsMatrixInvertible' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
    function D2D1InvertMatrix;       external D2D1Lib name 'D2D1InvertMatrix' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.

