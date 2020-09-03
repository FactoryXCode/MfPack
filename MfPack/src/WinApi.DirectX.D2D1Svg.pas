// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1Svg.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Tilo Güldner (TiloGueldner), (pyscripter)
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
// Source: d2d1svg.h
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
unit WinApi.DirectX.D2D1Svg;

  {$HPPEMIT '#include "d2d1svg.h"'}

interface

uses

  {WinApi}
  WinApi.ActiveX,
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2d1_1;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


// Enums =======================================================================

type
  // Specifies the paint type for an SVG fill or stroke.
  PD2D1_SVG_PAINT_TYPE = ^D2D1_SVG_PAINT_TYPE;
  D2D1_SVG_PAINT_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE}
const
  // The fill or stroke is not rendered.
  D2D1_SVG_PAINT_TYPE_NONE        = D2D1_SVG_PAINT_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_NONE}
  // A solid color is rendered.
  D2D1_SVG_PAINT_TYPE_COLOR       = D2D1_SVG_PAINT_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_COLOR}
  // The current color is rendered.
  D2D1_SVG_PAINT_TYPE_CURRENT_COLOR   = D2D1_SVG_PAINT_TYPE(2);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_CURRENT_COLOR}
  // A paint server); defined by another element in the SVG document); is used.
  D2D1_SVG_PAINT_TYPE_URI         = D2D1_SVG_PAINT_TYPE(3);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to D2D1_SVG_PAINT_TYPE_NONE.
  D2D1_SVG_PAINT_TYPE_URI_NONE      = D2D1_SVG_PAINT_TYPE(4);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_NONE}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to D2D1_SVG_PAINT_TYPE_COLOR.
  D2D1_SVG_PAINT_TYPE_URI_COLOR     = D2D1_SVG_PAINT_TYPE(5);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_COLOR}
  // A paint server); defined by another element in the SVG document); is used. If the
  // paint server reference is invalid); fall back to
  // D2D1_SVG_PAINT_TYPE_CURRENT_COLOR.
  D2D1_SVG_PAINT_TYPE_URI_CURRENT_COLOR = D2D1_SVG_PAINT_TYPE(6);
  {$EXTERNALSYM D2D1_SVG_PAINT_TYPE_URI_CURRENT_COLOR}
  //D2D1_SVG_PAINT_TYPE_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies the units for an SVG length.
  PD2D1_SVG_LENGTH_UNITS = ^D2D1_SVG_LENGTH_UNITS;
  D2D1_SVG_LENGTH_UNITS = (
    // The length is unitless.
    D2D1_SVG_LENGTH_UNITS_NUMBER      = DWord(0),
    // The length is a percentage value.
    D2D1_SVG_LENGTH_UNITS_PERCENTAGE  = DWord(1)
   // D2D1_SVG_LENGTH_UNITS_FORCE_DWORD = FORCEDWORD
  );

type
  // Specifies a value for the SVG display property.
  PD2D1_SVG_DISPLAY = ^D2D1_SVG_DISPLAY;
  D2D1_SVG_DISPLAY = (
    // The element uses the default display behavior.
    D2D1_SVG_DISPLAY_INLINE      = DWord(0),
    // The element and all children are not rendered directly.
    D2D1_SVG_DISPLAY_NONE        = DWord(1)
    //D2D1_SVG_DISPLAY_FORCE_DWORD = FORCEDWORD
 );
  {$EXTERNALSYM D2D1_SVG_DISPLAY}

type
  // Specifies a value for the SVG visibility property.
  PD2D1_SVG_VISIBILITY = ^D2D1_SVG_VISIBILITY;
  D2D1_SVG_VISIBILITY = (
    // The element is visible.
    D2D1_SVG_VISIBILITY_VISIBLE     = DWord(0),
    // The element is invisible.
    D2D1_SVG_VISIBILITY_HIDDEN      = DWord(1)
    //D2D1_SVG_VISIBILITY_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_SVG_VISIBILITY}


type
  // Specifies a value for the SVG overflow property.
  PD2D1_SVG_OVERFLOW = ^D2D1_SVG_OVERFLOW;
  D2D1_SVG_OVERFLOW = (
    // The element is not clipped to its viewport.
    D2D1_SVG_OVERFLOW_VISIBLE   = DWord(0),
    // The element is clipped to its viewport.
    D2D1_SVG_OVERFLOW_HIDDEN    = DWord(1)
    //D2D1_SVG_OVERFLOW_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_SVG_OVERFLOW}


type
  // Specifies a value for the SVG stroke-linecap property.
  PD2D1_SVG_LINE_CAP = ^D2D1_SVG_LINE_CAP;
  D2D1_SVG_LINE_CAP = (
    // The property is set to SVG's 'butt' value.
    D2D1_SVG_LINE_CAP_BUTT    = D2D1_CAP_STYLE_FLAT,
    // The property is set to SVG's 'square' value.
    D2D1_SVG_LINE_CAP_SQUARE   = D2D1_CAP_STYLE_SQUARE,
    // The property is set to SVG's 'round' value.
    D2D1_SVG_LINE_CAP_ROUND    = D2D1_CAP_STYLE_ROUND
    //D2D1_SVG_LINE_CAP_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_SVG_LINE_CAP}


type
  // Specifies a value for the SVG stroke-linejoin property.
  PD2D1_SVG_LINE_JOIN = ^D2D1_SVG_LINE_JOIN;
  D2D1_SVG_LINE_JOIN = (
    // The property is set to SVG's 'bevel' value.
    D2D1_SVG_LINE_JOIN_BEVEL     = D2D1_LINE_JOIN_BEVEL,

    // The property is set to SVG's 'miter' value. Note that this is equivalent to
    // D2D1_LINE_JOIN_MITER_OR_BEVEL); not D2D1_LINE_JOIN_MITER.
    D2D1_SVG_LINE_JOIN_MITER     = D2D1_LINE_JOIN_MITER_OR_BEVEL,

    // The property is set to SVG's 'round' value.
    D2D1_SVG_LINE_JOIN_ROUND     = D2D1_LINE_JOIN_ROUND
    //D2D1_SVG_LINE_JOIN_FORCE_DWORD = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_SVG_LINE_JOIN}


type
  // The alignment portion of the SVG preserveAspectRatio attribute.
  PD2D1_SVG_ASPECT_ALIGN = ^D2D1_SVG_ASPECT_ALIGN;
  D2D1_SVG_ASPECT_ALIGN = DWord;
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN}
const
  // The alignment is set to SVG's 'none' value.
  D2D1_SVG_ASPECT_ALIGN_NONE        = D2D1_SVG_ASPECT_ALIGN(0);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_NONE}
  // The alignment is set to SVG's 'xMinYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MIN = D2D1_SVG_ASPECT_ALIGN(1);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MIN}
  // The alignment is set to SVG's 'xMidYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MIN = D2D1_SVG_ASPECT_ALIGN(2);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MIN}
  // The alignment is set to SVG's 'xMaxYMin' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MIN = D2D1_SVG_ASPECT_ALIGN(3);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MIN}
  // The alignment is set to SVG's 'xMinYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MID = D2D1_SVG_ASPECT_ALIGN(4);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MID}
  // The alignment is set to SVG's 'xMidYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MID = D2D1_SVG_ASPECT_ALIGN(5);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MID}
  // The alignment is set to SVG's 'xMaxYMid' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MID = D2D1_SVG_ASPECT_ALIGN(6);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MID}
  // The alignment is set to SVG's 'xMinYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MAX = D2D1_SVG_ASPECT_ALIGN(7);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MIN_Y_MAX}
  // The alignment is set to SVG's 'xMidYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MAX = D2D1_SVG_ASPECT_ALIGN(8);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MID_Y_MAX}
  // The alignment is set to SVG's 'xMaxYMax' value.
  D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MAX = D2D1_SVG_ASPECT_ALIGN(9);
  {$EXTERNALSYM D2D1_SVG_ASPECT_ALIGN_X_MAX_Y_MAX}
  //D2D1_SVG_ASPECT_ALIGN_FORCE_DWORD = FORCEDWORD;

type
  // The meetOrSlice portion of the SVG preserveAspectRatio attribute.
  PD2D1_SVG_ASPECT_SCALING = ^D2D1_SVG_ASPECT_SCALING;
  D2D1_SVG_ASPECT_SCALING = DWord;
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING}
const
  // Scale the viewBox up as much as possible such that the entire viewBox is visible
  // within the viewport.
  D2D1_SVG_ASPECT_SCALING_MEET    = D2D1_SVG_ASPECT_SCALING(0);
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING_MEET}
  // Scale the viewBox down as much as possible such that the entire viewport is
  // covered by the viewBox.
  D2D1_SVG_ASPECT_SCALING_SLICE     = D2D1_SVG_ASPECT_SCALING(1);
  {$EXTERNALSYM D2D1_SVG_ASPECT_SCALING_SLICE}
  //D2D1_SVG_ASPECT_SCALING_FORCE_DWORD = FORCEDWORD;

type
  // Represents a path commmand. Each command may reference floats from the segment
  // data. Commands ending in _ABSOLUTE interpret data as absolute coordinate.
  // Commands ending in _RELATIVE interpret data as being relative to the previous
  // point.
  PD2D1_SVG_PATH_COMMAND = ^D2D1_SVG_PATH_COMMAND;
  D2D1_SVG_PATH_COMMAND = DWord;
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND}
const
  // Closes the current subpath. Uses no segment data.
  D2D1_SVG_PATH_COMMAND_CLOSE_PATH        = D2D1_SVG_PATH_COMMAND(0);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CLOSE_PATH}
  // Starts a new subpath at the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_MOVE_ABSOLUTE       = D2D1_SVG_PATH_COMMAND(1);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_MOVE_ABSOLUTE}
  // Starts a new subpath at the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_MOVE_RELATIVE       = D2D1_SVG_PATH_COMMAND(2);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_MOVE_RELATIVE}
  // Draws a line to the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_LINE_ABSOLUTE       = D2D1_SVG_PATH_COMMAND(3);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_LINE_ABSOLUTE}
  // Draws a line to the coordinate (x y). Uses 2 floats of segment data.
  D2D1_SVG_PATH_COMMAND_LINE_RELATIVE       = D2D1_SVG_PATH_COMMAND(4);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_LINE_RELATIVE}
  // Draws a cubic Bezier curve (x1 y1 x2 y2 x y). The curve ends at (x); y) and is
  // defined by the two control points (x1); y1) and (x2); y2). Uses 6 floats of
  // segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_ABSOLUTE      = D2D1_SVG_PATH_COMMAND(5);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_ABSOLUTE}
  // Draws a cubic Bezier curve (x1 y1 x2 y2 x y). The curve ends at (x); y) and is
  // defined by the two control points (x1); y1) and (x2); y2). Uses 6 floats of
  // segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_RELATIVE      = D2D1_SVG_PATH_COMMAND(6);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_RELATIVE}
  // Draws a quadratic Bezier curve (x1 y1 x y). The curve ends at (x); y) and is
  // defined by the control point (x1 y1). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_ABSOLUTE    = D2D1_SVG_PATH_COMMAND(7);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_ABSOLUTE}
  // Draws a quadratic Bezier curve (x1 y1 x y). The curve ends at (x); y) and is
  // defined by the control point (x1 y1). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_RELATIVE    = D2D1_SVG_PATH_COMMAND(8);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_RELATIVE}
  // Draws an elliptical arc (rx ry x-axis-rotation large-arc-flag sweep-flag x y).
  // The curve ends at (x); y) and is defined by the arc parameters. The two flags are
  // considered set if their values are non-zero. Uses 7 floats of segment data.
  D2D1_SVG_PATH_COMMAND_ARC_ABSOLUTE        = D2D1_SVG_PATH_COMMAND(9);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_ARC_ABSOLUTE}
  // Draws an elliptical arc (rx ry x-axis-rotation large-arc-flag sweep-flag x y).
  // The curve ends at (x); y) and is defined by the arc parameters. The two flags are
  // considered set if their values are non-zero. Uses 7 floats of segment data.
  D2D1_SVG_PATH_COMMAND_ARC_RELATIVE        = D2D1_SVG_PATH_COMMAND(10);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_ARC_RELATIVE}
  // Draws a horizontal line to the coordinate (x). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_HORIZONTAL_ABSOLUTE     = D2D1_SVG_PATH_COMMAND(11);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_HORIZONTAL_ABSOLUTE}
  // Draws a horizontal line to the coordinate (x). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_HORIZONTAL_RELATIVE     = D2D1_SVG_PATH_COMMAND(12);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_HORIZONTAL_RELATIVE}
  // Draws a vertical line to the coordinate (y). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_VERTICAL_ABSOLUTE     = D2D1_SVG_PATH_COMMAND(13);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_VERTICAL_ABSOLUTE}
  // Draws a vertical line to the coordinate (y). Uses 1 float of segment data.
  D2D1_SVG_PATH_COMMAND_VERTICAL_RELATIVE     = D2D1_SVG_PATH_COMMAND(14);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_VERTICAL_RELATIVE}
  // Draws a smooth cubic Bezier curve (x2 y2 x y). The curve ends at (x); y) and is
  // defined by the control point (x2); y2). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_ABSOLUTE   = D2D1_SVG_PATH_COMMAND(15);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_ABSOLUTE}
  // Draws a smooth cubic Bezier curve (x2 y2 x y). The curve ends at (x); y) and is
  // defined by the control point (x2); y2). Uses 4 floats of segment data.
  D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_RELATIVE   = D2D1_SVG_PATH_COMMAND(16);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_CUBIC_SMOOTH_RELATIVE}
  // Draws a smooth quadratic Bezier curve ending at (x); y). Uses 2 floats of segment
  // data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_ABSOLUTE = D2D1_SVG_PATH_COMMAND(17);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_ABSOLUTE}
  // Draws a smooth quadratic Bezier curve ending at (x); y). Uses 2 floats of segment
  // data.
  D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_RELATIVE = D2D1_SVG_PATH_COMMAND(18);
  {$EXTERNALSYM D2D1_SVG_PATH_COMMAND_QUADRADIC_SMOOTH_RELATIVE}
  //D2D1_SVG_PATH_COMMAND_FORCE_DWORD         = FORCEDWORD;


type
  // Defines the coordinate system used for SVG gradient or clipPath elements.
  PD2D1_SVG_UNIT_TYPE = ^D2D1_SVG_UNIT_TYPE;
  D2D1_SVG_UNIT_TYPE = (
    // The property is set to SVG's 'userSpaceOnUse' value.
    D2D1_SVG_UNIT_TYPE_USER_SPACE_ON_USE   = 0,
    // The property is set to SVG's 'objectBoundingBox' value.
    D2D1_SVG_UNIT_TYPE_OBJECT_BOUNDING_BOX = 1
    //D2D1_SVG_UNIT_TYPE_FORCE_DWORD         = FORCEDWORD
  );
  {$EXTERNALSYM D2D1_SVG_UNIT_TYPE}

type
  // Defines the type of SVG string attribute to set or get.
  PD2D1_SVG_ATTRIBUTE_STRING_TYPE = ^D2D1_SVG_ATTRIBUTE_STRING_TYPE;
  D2D1_SVG_ATTRIBUTE_STRING_TYPE = (
    // The attribute is a string in the same form as it would appear in the SVG XML.
    //
    // Note that when getting values of this type); the value returned may not exactly
    // match the value that was set. Instead); the output value is a normalized version
    // of the value. For example); an input color of 'red' may be output as '#FF0000'.
    D2D1_SVG_ATTRIBUTE_STRING_TYPE_SVG     = DWord(0),
    // The attribute is an element ID.
    D2D1_SVG_ATTRIBUTE_STRING_TYPE_ID      = DWord(1)
    //D2D1_SVG_ATTRIBUTE_STRING_TYPE_FORCE_DWORD = FORCEDWORD;
  );
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_STRING_TYPE}

type
  // Defines the type of SVG POD attribute to set or get.
  PD2D1_SVG_ATTRIBUTE_POD_TYPE = ^D2D1_SVG_ATTRIBUTE_POD_TYPE;
  D2D1_SVG_ATTRIBUTE_POD_TYPE = DWord;
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE}
const
  // The attribute is a FLOAT.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT                = D2D1_SVG_ATTRIBUTE_POD_TYPE(0);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT}
  // The attribute is a D2D1_COLOR_F.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR                = D2D1_SVG_ATTRIBUTE_POD_TYPE(1);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR}
  // The attribute is a D2D1_FILL_MODE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_FILL_MODE            = D2D1_SVG_ATTRIBUTE_POD_TYPE(2);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_FILL_MODE}
  // The attribute is a D2D1_SVG_DISPLAY.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_DISPLAY              = D2D1_SVG_ATTRIBUTE_POD_TYPE(3);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_DISPLAY}
  // The attribute is a D2D1_SVG_OVERFLOW.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_OVERFLOW             = D2D1_SVG_ATTRIBUTE_POD_TYPE(4);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_OVERFLOW}
  // The attribute is a D2D1_SVG_LINE_CAP.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_CAP             = D2D1_SVG_ATTRIBUTE_POD_TYPE(5);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_CAP}
  // The attribute is a D2D1_SVG_LINE_JOIN.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_JOIN            = D2D1_SVG_ATTRIBUTE_POD_TYPE(6);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LINE_JOIN}
  // The attribute is a D2D1_SVG_VISIBILITY.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_VISIBILITY           = D2D1_SVG_ATTRIBUTE_POD_TYPE(7);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_VISIBILITY}
  // The attribute is a D2D1_MATRIX_3X2_F.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_MATRIX               = D2D1_SVG_ATTRIBUTE_POD_TYPE(8);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_MATRIX}
  // The attribute is a D2D1_SVG_UNIT_TYPE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_UNIT_TYPE             = D2D1_SVG_ATTRIBUTE_POD_TYPE(9);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_UNIT_TYPE}
  // The attribute is a D2D1_EXTEND_MODE.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_EXTEND_MODE           = D2D1_SVG_ATTRIBUTE_POD_TYPE(10);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_EXTEND_MODE}
  // The attribute is a D2D1_SVG_PRESERVE_ASPECT_RATIO.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_PRESERVE_ASPECT_RATIO = D2D1_SVG_ATTRIBUTE_POD_TYPE(11);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_PRESERVE_ASPECT_RATIO}
  // The attribute is a D2D1_SVG_VIEWBOX.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX               = D2D1_SVG_ATTRIBUTE_POD_TYPE(12);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX}
  // The attribute is a D2D1_SVG_LENGTH.
  D2D1_SVG_ATTRIBUTE_POD_TYPE_LENGTH                = D2D1_SVG_ATTRIBUTE_POD_TYPE(13);
  {$EXTERNALSYM D2D1_SVG_ATTRIBUTE_POD_TYPE_LENGTH}
  //D2D1_SVG_ATTRIBUTE_POD_TYPE_FORCE_DWORD         = FORCEDWORD;


// =============================================================================

type

  // Forward interface declarations
  ID2D1SvgAttribute = interface;
  PID2D1SvgAttribute = ^ID2D1SvgAttribute;

  ID2D1SvgPaint = interface;
  PID2D1SvgPaint = ^ID2D1SvgPaint;

  ID2D1SvgStrokeDashArray = interface;
  PID2D1SvgStrokeDashArray = ^ID2D1SvgStrokeDashArray;

  ID2D1SvgPointCollection = interface;
  PID2D1SvgPointCollection = ^ID2D1SvgPointCollection;

  ID2D1SvgPathData = interface;
  PID2D1SvgPathData = ^ID2D1SvgPathData;

  ID2D1SvgElement = interface;
  PID2D1SvgElement = ^ID2D1SvgElement;

  ID2D1SvgDocument = interface;
  PID2D1SvgDocument = ^ID2D1SvgDocument;



  // Represents an SVG length.
  PD2D1_SVG_LENGTH = ^D2D1_SVG_LENGTH;
  D2D1_SVG_LENGTH = record
    value: Single;
    units: D2D1_SVG_LENGTH_UNITS;
  end;
  {$EXTERNALSYM D2D1_SVG_LENGTH}


  // Represents all SVG preserveAspectRatio settings.
  PD2D1_SVG_PRESERVE_ASPECT_RATIO = ^D2D1_SVG_PRESERVE_ASPECT_RATIO;
  D2D1_SVG_PRESERVE_ASPECT_RATIO = record
    // Sets the 'defer' portion of the preserveAspectRatio settings. This field only
    // has an effect on an 'image' element that references another SVG document. As
    // this is not currently supported, the field has no impact on rendering.
    defer: BOOL;
    // Sets the align portion of the preserveAspectRatio settings.
    align: D2D1_SVG_ASPECT_ALIGN;
    // Sets the meetOrSlice portion of the preserveAspectRatio settings.
    meetOrSlice: D2D1_SVG_ASPECT_SCALING;
  end;
  {$EXTERNALSYM D2D1_SVG_PRESERVE_ASPECT_RATIO}


  // Represents an SVG viewBox.
  PD2D1_SVG_VIEWBOX = ^D2D1_SVG_VIEWBOX;
  D2D1_SVG_VIEWBOX = record
    x: Single;
    y: Single;
    width: Single;
    height: Single;
  end;
  {$EXTERNALSYM D2D1_SVG_VIEWBOX}


//#if NTDDI_VERSION >= NTDDI_WIN10_RS2


// INTERFACES //////////////////////////////////////////////////////////////////

  // Interface ID2D1SvgAttribute
  // ===========================
  // Interface describing an SVG attribute.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgAttribute);'}
  {$EXTERNALSYM ID2D1SvgAttribute}
  ID2D1SvgAttribute = interface(ID2D1Resource)
  ['{c9cdb0dd-f8c9-4e70-b7c2-301c80292c5e}']

    // Returns the element on which this attribute is set. Returns null if the
    // attribute is not set on any element.
   procedure GetElement(out element: ID2D1SvgElement); stdcall;

    // Creates a clone of this attribute value. On creation, the cloned attribute is
    // not set on any element.
    function Clone(out attribute: ID2D1SvgAttribute): HResult; stdcall;

  end;
  IID_ID2D1SvgAttribute = ID2D1SvgAttribute;
  {$EXTERNALSYM IID_ID2D1SvgAttribute}


  // Interface ID2D1SvgPaint
  // =======================
  // Interface describing an SVG 'fill' or 'stroke' value.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPaint);'}
  {$EXTERNALSYM ID2D1SvgPaint}
  ID2D1SvgPaint = interface(ID2D1SvgAttribute)
  ['{d59bab0a-68a2-455b-a5dc-9eb2854e2490}']

    // Sets the paint type.
    function SetPaintType(paintType: D2D1_SVG_PAINT_TYPE): HResult; stdcall;

    // Gets the paint type.
    function GetPaintType(): D2D1_SVG_PAINT_TYPE; stdcall;

    // Sets the paint color that is used if the paint type is
    // D2D1_SVG_PAINT_TYPE_COLOR.
    function SetColor(const color: D2D1_COLOR_F): HResult; stdcall;

    // Gets the paint color that is used if the paint type is
    // D2D1_SVG_PAINT_TYPE_COLOR.
    procedure GetColor(out color: D2D1_COLOR_F); stdcall;

    // Sets the element id which acts as the paint server. This id is used if the paint
    // type is D2D1_SVG_PAINT_TYPE_URI.
    function SetId(id: LPCWSTR): HResult; stdcall;

    // Gets the element id which acts as the paint server. This id is used if the paint
    // type is D2D1_SVG_PAINT_TYPE_URI.
    function GetId(out id: PWideChar;
                   idCount: UINT32): HResult; stdcall;

    // Gets the string length of the element id which acts as the paint server. This id
    // is used if the paint type is D2D1_SVG_PAINT_TYPE_URI. The returned string length
    // does not include room for the null terminator.
    function GetIdLength(): UINT32; stdcall;

  end;
  IID_ID2D1SvgPaint = ID2D1SvgPaint;
  {$EXTERNALSYM IID_ID2D1SvgPaint}


  // Interface ID2D1SvgStrokeDashArray
  // =================================
  // Interface describing an SVG 'stroke-dasharray' value.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgStrokeDashArray);'}
  {$EXTERNALSYM ID2D1SvgStrokeDashArray}
  ID2D1SvgStrokeDashArray = interface(ID2D1SvgAttribute)
  ['{f1c0ca52-92a3-4f00-b4ce-f35691efd9d9}']

    // Removes dashes from the end of the array.
    // <param name="dashesCount">Specifies how many dashes to remove.</param>
    function RemoveDashesAtEnd(dashesCount: UINT32): HResult; stdcall;

    // Updates the array. Existing dashes not updated by this method are preserved. The
    // array is resized larger if necessary to accomodate the new dashes.

    // <param name="dashes">The dashes array.</param>
    // <param name="dashesCount">The number of dashes to update.</param>
    // <param name="startIndex">The index at which to begin updating dashes. Must be
    // less than or equal to the size of the array.</param>
    function UpdateDashes(dashes: PSingle;
                          dashesCount: UINT32;
                          startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Updates the array. Existing dashes not updated by this method are preserved. The
    // array is resized larger if necessary to accomodate the new dashes.

    // <param name="dashes">The dashes array.</param>
    // <param name="dashesCount">The number of dashes to update.</param>
    // <param name="startIndex">The index at which to begin updating dashes. Must be
    // less than or equal to the size of the array.</param>
    function UpdateDashes(dashes: PD2D1_SVG_LENGTH;
                          dashesCount: UINT32;
                          startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets dashes from the array.

    // <param name="dashes">Buffer to contain the dashes.</param>
    // <param name="dashesCount">The element count of buffer.</param>
    // <param name="startIndex">The index of the first dash to retrieve.</param>
    function GetDashes(out dashes: PSingle;
                       dashesCount: UINT32;
                       startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets dashes from the array.

    // <param name="dashes">Pointer to buffer to contain the dashes.</param>
    // <param name="dashesCount">The element count of buffer.</param>
    // <param name="startIndex">The index of the first dash to retrieve.</param>
    function GetDashes(out dashes: PD2D1_SVG_LENGTH;
                       dashesCount: UINT32;
                       startIndex: UINT32 = 0): HResult; overload; stdcall;

    // Gets the number of the dashes in the array.
    function GetDashesCount(): UINT32; stdcall;

  end;
  IID_ID2D1SvgStrokeDashArray = ID2D1SvgStrokeDashArray;
  {$EXTERNALSYM IID_ID2D1SvgStrokeDashArray}


  // Interface ID2D1SvgPointCollection
  // =================================
  // Interface describing an SVG 'points' value in a 'polyline' or 'polygon' element.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPointCollection);'}
  {$EXTERNALSYM ID2D1SvgPointCollection}
  ID2D1SvgPointCollection = interface(ID2D1SvgAttribute)
   ['{9dbe4c0d-3572-4dd9-9825-5530813bb712}']

    // Removes points from the end of the array.
    // <param name="pointsCount">Specifies how many points to remove.</param>
    function RemovePointsAtEnd(pointsCount: UINT32): HResult; stdcall;

    // Updates the points array. Existing points not updated by this method are
    // preserved. The array is resized larger if necessary to accomodate the new
    // points.

    // <param name="points">The points array.</param>
    // <param name="pointsCount">The number of points to update.</param>
    // <param name="startIndex">The index at which to begin updating points. Must be
    // less than or equal to the size of the array.</param>
    function UpdatePoints(points: PD2D1_POINT_2F;
                          pointsCount: UINT32;
                          startIndex: UINT32 = 0): HResult; stdcall;


    // Gets points from the points array.

    // <param name="points">Buffer to contain the points.</param>
    // <param name="pointsCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first point to retrieve.</param>
    function GetPoints(out points: D2D1_POINT_2F;
                       pointsCount: UINT32;
                       startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the number of points in the array.
    function GetPointsCount(): UINT32;

  end; // interface ID2D1SvgPointCollection
  IID_ID2D1SvgPointCollection = ID2D1SvgPointCollection;
  {$EXTERNALSYM IID_ID2D1SvgPointCollection}


  // Interface ID2D1SvgPathData
  // ==========================
  // Interface describing SVG path data. Path data can be set as the 'd' attribute on
  // a 'path' element.
  //
  // The path data set is factored into two arrays. The segment data array stores all
  // numbers and the commands array stores the set of commands. Unlike the string
  // data set in the d attribute, each command in this representation uses a fixed
  // number of elements in the segment data array. Therefore, the path 'M 0,0 100,0
  // 0,100 Z' is represented as: 'M0,0 L100,0 L0,100 Z'. This is split into two
  // arrays, with the segment data containing '0,0 100,0 0,100', and the commands
  // containing 'M L L Z'.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgPathData);'}
  {$EXTERNALSYM ID2D1SvgPathData}
  ID2D1SvgPathData = interface(ID2D1SvgAttribute)
   ['{c095e4f4-bb98-43d6-9745-4d1b84ec9888}']

    // Removes data from the end of the segment data array.
    // <param name="dataCount">Specifies how much data to remove.</param>
    function RemoveSegmentDataAtEnd(dataCount: UINT32): HResult; stdcall;

    // Updates the segment data array. Existing segment data not updated by this method
    // are preserved. The array is resized larger if necessary to accomodate the new
    // segment data.

    // <param name="data">The data array.</param>
    // <param name="dataCount">The number of data to update.</param>
    // <param name="startIndex">The index at which to begin updating segment data. Must
    // be less than or equal to the size of the segment data array.</param>
    function UpdateSegmentData(data: PSingle;
                               dataCount: UINT32;
                               startIndex: UINT32 = 0): HResult; stdcall;

    // Gets data from the segment data array.
    // <param name="data">Buffer to contain the segment data array.</param>
    // <param name="dataCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first segment data to retrieve.
    // </param>
    function GetSegmentData(out data: PSingle;
                            dataCount: UINT32;
                            startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the size of the segment data array.
    function GetSegmentDataCount(): UINT32; stdcall;

    // Removes commands from the end of the commands array.
    // <param name="commandsCount">Specifies how many commands to remove.</param>
    function RemoveCommandsAtEnd(commandsCount: UINT32): HResult; stdcall;

    // Updates the commands array. Existing commands not updated by this method are
    // preserved. The array is resized larger if necessary to accomodate the new
    // commands.

    // <param name="commands">The commands array.</param>
    // <param name="commandsCount">The number of commands to update.</param>
    // <param name="startIndex">The index at which to begin updating commands. Must be
    // less than or equal to the size of the commands array.</param>
    function UpdateCommands(commands: PD2D1_SVG_PATH_COMMAND;
                            commandsCount: UINT32;
                            startIndex: UINT32 = 0): HResult; stdcall;

    // Gets commands from the commands array.

    // <param name="commands">Buffer to contain the commands</param>
    // <param name="commandsCount">The element count of the buffer.</param>
    // <param name="startIndex">The index of the first commands to retrieve.</param>
    function GetCommands(out commands: PD2D1_SVG_PATH_COMMAND;
                         commandsCount: UINT32;
                         startIndex: UINT32 = 0): HResult; stdcall;

    // Gets the size of the commands array.
    function GetCommandsCount(): UINT32; stdcall;

    // Creates a path geometry object representing the path data.
    function CreatePathGeometry(fillMode: D2D1_FILL_MODE;
                                out pathGeometry: ID2D1PathGeometry1): HResult; stdcall;

  end;
  IID_ID2D1SvgPathData = ID2D1SvgPathData;
  {$EXTERNALSYM IID_ID2D1SvgPathData}


  // Interface ID2D1SvgElement
  // =========================
  // Interface for all SVG elements.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgElement);'}
  {$EXTERNALSYM ID2D1SvgElement}
  ID2D1SvgElement = interface(ID2D1Resource)
   ['{ac7b67a6-183e-49c1-a823-0ebe40b0db29}']

    // Gets the document that contains this element. Returns null if the element has
    // been removed from the tree.
    procedure GetDocument(document: ID2D1SvgDocument); stdcall;

    // Gets the tag name.
    function GetTagName(name: PWideChar;
                        nameCount: UINT32): HResult; stdcall;

    // Gets the string length of the tag name. The returned string length does not
    // include room for the null terminator.
    function GetTagNameLength(): UINT32; stdcall;

    // Returns TRUE if this element represents text content, e.g. the content of a
    // 'title' or 'desc' element. Text content does not have a tag name.
    function IsTextContent(): BOOL; stdcall;

    // Gets the parent element.
    procedure GetParent(out parent: ID2D1SvgElement); stdcall;

    // Returns whether this element has children.
    function HasChildren(): BOOL; stdcall;

    // Gets the first child of this element.
    procedure GetFirstChild(out child: ID2D1SvgElement); stdcall;

    // Gets the last child of this element.
    procedure GetLastChild(out child: ID2D1SvgElement); stdcall;

    // Gets the previous sibling of the referenceChild element.
    // <param name="referenceChild">The referenceChild must be an immediate child of
    // this element.</param>
    // <param name="previousChild">The output previousChild element will be non-nil if
    // the referenceChild has a previous sibling. If the referenceChild is the first
    // child, the output is nil.</param>
    function GetPreviousChild(const referenceChild: ID2D1SvgElement;
                              out previousChild: ID2D1SvgElement): HResult; stdcall;

    // Gets the next sibling of the referenceChild element.
    // <param name="referenceChild">The referenceChild must be an immediate child of
    // this element.</param>
    // <param name="nextChild">The output nextChild element will be non-nil if the
    // referenceChild has a next sibling. If the referenceChild is the last child, the
    // output is nil.</param>
    function GetNextChild(const referenceChild: ID2D1SvgElement;
                          out nextChild: ID2D1SvgElement): HResult; stdcall;

    // Inserts newChild as a child of this element, before the referenceChild element.
    // If the newChild element already has a parent, it is removed from this parent as
    // part of the insertion. Returns an error if this element cannot accept children
    // of the type of newChild. Returns an error if the newChild is an ancestor of this
    // element.
    // <param name="newChild">The element to be inserted.</param>
    // <param name="referenceChild">The element that the child should be inserted
    // before. If referenceChild is nil, the newChild is placed as the last child. If
    // referenceChild is non-nil, it must be an immediate child of this element.
    // </param>
    function InsertChildBefore(newChild: ID2D1SvgElement;
                               {In_opt} referenceChild: PID2D1SvgElement = Nil): HResult; stdcall;

    // Appends newChild to the list of children. If the newChild element already has a
    // parent, it is removed from this parent as part of the append operation. Returns
    // an error if this element cannot accept children of the type of newChild. Returns
    // an error if the newChild is an ancestor of this element.
    // <param name="newChild">The element to be appended.</param>
    function AppendChild(newChild: ID2D1SvgElement): HResult; stdcall;

    // Replaces the oldChild element with the newChild. This operation removes the
    // oldChild from the tree. If the newChild element already has a parent, it is
    // removed from this parent as part of the replace operation. Returns an error if
    // this element cannot accept children of the type of newChild. Returns an error if
    // the newChild is an ancestor of this element.
    // <param name="newChild">The element to be inserted.</param>
    // <param name="oldChild">The child element to be replaced. The oldChild element
    // must be an immediate child of this element.</param>
    function ReplaceChild(newChild: ID2D1SvgElement;
                          oldChild: ID2D1SvgElement): HResult; stdcall;

    // Removes the oldChild from the tree. Children of oldChild remain children of
    // oldChild.
    // <param name="oldChild">The child element to be removed. The oldChild element
    // must be an immediate child of this element.</param>
    function RemoveChild(oldChild: ID2D1SvgElement): HResult; stdcall;

    // Creates an element from a tag name. The element is appended to the list of
    // children. Returns an error if this element cannot accept children of the
    // specified type.
    // <param name="tagName">The tag name of the new child. An empty string is
    // interpreted to be a text content element.</param>
    // <param name="newChild">The new child element.</param>
    function CreateChild(tagName: LPWSTR;
                         out newChild: ID2D1SvgElement): HResult; stdcall;

    // Returns true if the attribute is explicitly set on the element or if it is
    // present within an inline style. Returns FALSE if the attribute is not a valid
    // attribute on this element.
    // <param name="name">The name of the attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function IsAttributeSpecified(name: LPWSTR;
                                  _inherited: PBOOL = Nil): BOOL; stdcall;

    // Returns the number of specified attributes on this element. Attributes are only
    // considered specified if they are explicitly set on the element or present within
    // an inline style. Properties that receive their value through CSS inheritance are
    // not considered specified. An attribute can become specified if it is set through
    // a method call. It can become unspecified if it is removed via RemoveAttribute.
    function GetSpecifiedAttributeCount(): UINT32; stdcall;

    // Gets the name of the specified attribute at the given index.
    // <param name="index">The specified index of the attribute.</param>
    // <param name="name">Outputs the name of the attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function GetSpecifiedAttributeName(index: UINT32;
                                       out name: PWideChar;
                                       nameCount: UINT32;
                                       _inherited: PBOOL = Nil): HResult; stdcall;

    // Gets the string length of the name of the specified attribute at the given
    // index. The output string length does not include room for the null terminator.
    // <param name="index">The specified index of the attribute.</param>
    // <param name="nameLength">Outputs the string length of the name of the specified
    // attribute.</param>
    // <param name="inherited">Outputs whether the attribute is set to the 'inherit'
    // value.</param>
    function GetSpecifiedAttributeNameLength(index: UINT32;
                                             out nameLength: UINT32;
                                             {out} _inherited: PBOOL = Nil): HResult; stdcall;

    // Removes the attribute from this element. Also removes this attribute from within
    // an inline style if present. Returns an error if the attribute name is not valid
    // on this element.
    function RemoveAttribute(name: LPWSTR): HResult; stdcall;

    // Sets the value of a text content element.
    function SetTextValue(name: PWideChar;
                          nameCount: UINT32): HResult; stdcall;

    // Gets the value of a text content element.
    function GetTextValue(out name: PWideChar;
                          nameCount: UINT32): HResult; stdcall;

    // Gets the length of the text content value. The returned string length does not
    // include room for the null terminator.
    function GetTextValueLength(): UINT32; stdcall;

    //==========================================================================
    // From here the sequence of the implemented interface methods differs from
    // the C-header file. This has been issued to Microsoft and Idera.
    //==========================================================================

    // Sets an attribute of this element using an interface. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified interface type. Returns an error if the
    // attribute object is already set on an element. A given attribute object may only
    // be set on one element in one attribute location at a time.
    function SetAttributeValue(name: LPWSTR;
                               value: ID2D1SvgAttribute): HResult; overload; stdcall;

    // Sets an attribute of this element using a POD type. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified type.
    function SetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_POD_TYPE;
                               value: Pointer;
                               valueSizeInBytes: UINT32): HResult; overload; stdcall;

    // Sets an attribute of this element using a string. Returns an error if the
    // attribute name is not valid on this element. Returns an error if the attribute
    // cannot be expressed as the specified type.
    function SetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                               value: LPWSTR): HResult; overload; stdcall;

    // Gets an attribute of this element as an interface type. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified interface type.
    // <param name="riid">The interface ID of the attribute value.</param>
    function GetAttributeValue(name: LPWSTR;
                               const riid: TGUID;    // The interface ID of the attribute value.
                               out value {IUnknown}  // The value of the attribute = interface
                               ): HResult; overload; stdcall;

    // Gets an attribute of this element as a POD type. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified POD type.
    function GetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_POD_TYPE;
                               value: Pointer;
                               valueSizeInBytes: UINT32): HResult; overload; stdcall;

    // Gets an attribute of this element as a string. Returns an error if the attribute
    // is not specified. Returns an error if the attribute name is not valid on this
    // element. Returns an error if the attribute cannot be expressed as the specified
    // string type.
    function GetAttributeValue(name: LPWSTR;
                               _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                               out value: PWideChar;
                               valueCount: UINT32): HResult; overload; stdcall;

    // Gets the string length of an attribute of this element. The returned string
    // length does not include room for the null terminator. Returns an error if the
    // attribute is not specified. Returns an error if the attribute name is not valid
    // on this element. Returns an error if the attribute cannot be expressed as the
    // specified string type.
    function GetAttributeValueLength(name: LPWSTR;
                                     _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE;
                                     out valueLength: UINT32): HResult; stdcall;

  end;
  IID_ID2D1SvgElement = ID2D1SvgElement;
  {$EXTERNALSYM IID_ID2D1SvgElement}


  // Interface ID2D1SvgDocument
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1SvgDocument);'}
  {$EXTERNALSYM ID2D1SvgDocument}
  ID2D1SvgDocument = interface(ID2D1Resource)
   ['{86b88e4d-afa4-4d7b-88e4-68a51c4a0aec}']

    // Sets the size of the initial viewport.
    function SetViewportSize(viewportSize: D2D1_SIZE_F): HResult; stdcall;

    // Returns the size of the initial viewport.
    procedure GetViewportSize(out size: D2D1_SIZE_F); stdcall;

    // Sets the root element of the document. The root element must be an 'svg'
    // element. If the element already exists within an svg tree, it is first removed.
    function SetRoot({In_opt} root: ID2D1SvgElement): HResult; stdcall;

    // Gets the root element of the document.
    procedure GetRoot(out root: ID2D1SvgElement); stdcall;

    // Gets the SVG element with the specified ID. If the element cannot be found, the
    // returned element will be Nil.
    function FindElementById(id: PWideChar;
                             out svgElement: ID2D1SvgElement): HResult; stdcall;

    // Serializes an element and its subtree to XML. The output XML is encoded as
    // UTF-8.
    // <param name="outputXmlStream">An output stream to contain the SVG XML subtree.
    // </param>
    // <param name="subtree">The root of the subtree. If null, the entire document is
    // serialized.</param>
    function Serialize(outputXmlStream: IStream;
                       {in_opt} subtree: ID2D1SvgElement = nil): HResult; stdcall;

    // Deserializes a subtree from the stream. The stream must have only one root
    // element, but that root element need not be an 'svg' element. The output element
    // is not inserted into this document tree.
    // <param name="inputXmlStream">An input stream containing the SVG XML subtree.
    // </param>
    // <param name="subtree">The root of the subtree.</param>
    function Deserialize(inputXmlStream: IStream;
                         out subtree: ID2D1SvgElement): HResult; stdcall;

    // Creates a paint object which can be used to set the 'fill' or 'stroke'
    // properties.
    // <param name="color">The color used if the paintType is
    // D2D1_SVG_PAINT_TYPE_COLOR.</param>
    // <param name="id">The element id which acts as the paint server. This id is used
    // if the paint type is D2D1_SVG_PAINT_TYPE_URI.</param>
    function CreatePaint(paintType: D2D1_SVG_PAINT_TYPE;
                         {in_opt} color: PD2D1_COLOR_F;
                         {in_opt} id: PWideChar;
                         out paint: ID2D1SvgPaint): HResult; stdcall;

    // Creates a dash array object which can be used to set the 'stroke-dasharray'
    // property.
    function CreateStrokeDashArray(dashes: PD2D1_SVG_LENGTH;
                                   dashesCount: UINT32;
                                   out strokeDashArray: ID2D1SvgStrokeDashArray): HResult; stdcall;

    // Creates a points object which can be used to set a 'points' attribute on a
    // 'polygon' or 'polyline' element.
    function CreatePointCollection(points: PD2D1_POINT_2F;
                                   pointsCount: UINT32;
                                   out pointCollection: ID2D1SvgPointCollection): HResult; stdcall;

    // Creates a path data object which can be used to set a 'd' attribute on a 'path'
    // element.
    function CreatePathData(segmentData: PSingle;
                            segmentDataCount: UINT32;
                            commands: PD2D1_SVG_PATH_COMMAND;
                            commandsCount: UINT32;
                            out pathData: ID2D1SvgPathData): HResult; stdcall;

  end;
  IID_ID2D1SvgDocument = ID2D1SvgDocument;
  {$EXTERNALSYM IID_ID2D1SvgDocument}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
