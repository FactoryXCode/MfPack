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
// Revision Version: 4.1.0
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.DirectX.D2D1Svg;

  {$HPPEMIT '#include "d2d1svg.h"'}

interface

uses

  {WinApi}
  WinApi.ActiveX.ObjIdlbase,
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2d1_1;

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
  D2D1_SVG_LINE_CAP = DWord;
const
  D2D1_SVG_LINE_CAP_BUTT = D2D1_SVG_LINE_CAP(0);
  D2D1_SVG_LINE_CAP_SQUARE = D2D1_SVG_LINE_CAP(1);
  D2D1_SVG_LINE_CAP_ROUND = D2D1_SVG_LINE_CAP(2);
  {$EXTERNALSYM D2D1_SVG_LINE_CAP}


type
  // Specifies a value for the SVG stroke-linejoin property.
  PD2D1_SVG_LINE_JOIN = ^D2D1_SVG_LINE_JOIN;
  D2D1_SVG_LINE_JOIN = DWord;
const
  D2D1_SVG_LINE_JOIN_BEVEL = D2D1_SVG_LINE_JOIN(1);
  D2D1_SVG_LINE_JOIN_MITER = D2D1_SVG_LINE_JOIN(3);
  D2D1_SVG_LINE_JOIN_ROUND = D2D1_SVG_LINE_JOIN(2);
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
    // Native virtual slots; C++ inline helpers are not entries.
    procedure GetElement(out element: ID2D1SvgElement); stdcall;
    function Clone(out attribute: ID2D1SvgAttribute): HRESULT; stdcall;


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
    // Native virtual slots; C++ inline helpers are not entries.
    function SetPaintType(paintType: D2D1_SVG_PAINT_TYPE): HRESULT; stdcall;
    function GetPaintType(): D2D1_SVG_PAINT_TYPE; stdcall;
    function SetColor(const color: D2D1_COLOR_F): HRESULT; stdcall;
    procedure GetColor(out color: D2D1_COLOR_F); stdcall;
    function SetId(id: PWideChar): HRESULT; stdcall;
    function GetId(id: PWideChar; idCount: UINT32): HRESULT; stdcall;
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
    // Native virtual slots; C++ inline helpers are not entries.
    function RemoveDashesAtEnd(dashesCount: UINT32): HRESULT; stdcall;
    function UpdateDashes(dashes: PD2D1_SVG_LENGTH; dashesCount: UINT32; startIndex: UINT32): HRESULT; stdcall; overload;
    function UpdateDashes(dashes: PSingle; dashesCount: UINT32; startIndex: UINT32): HRESULT; stdcall; overload;
    function GetDashes(dashes: PD2D1_SVG_LENGTH; dashesCount: UINT32; startIndex: UINT32): HRESULT; stdcall; overload;
    function GetDashes(dashes: PSingle; dashesCount: UINT32; startIndex: UINT32): HRESULT; stdcall; overload;
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
    // Native virtual slots; C++ inline helpers are not entries.
    function RemovePointsAtEnd(pointsCount: UINT32): HRESULT; stdcall;
    function UpdatePoints(points: PD2D1_POINT_2F; pointsCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetPoints(points: PD2D1_POINT_2F; pointsCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetPointsCount(): UINT32; stdcall;


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
    // Native virtual slots; C++ inline helpers are not entries.
    function RemoveSegmentDataAtEnd(dataCount: UINT32): HRESULT; stdcall;
    function UpdateSegmentData(data: PSingle; dataCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetSegmentData(data: PSingle; dataCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetSegmentDataCount(): UINT32; stdcall;
    function RemoveCommandsAtEnd(commandsCount: UINT32): HRESULT; stdcall;
    function UpdateCommands(commands: PD2D1_SVG_PATH_COMMAND; commandsCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetCommands(commands: PD2D1_SVG_PATH_COMMAND; commandsCount: UINT32; startIndex: UINT32): HRESULT; stdcall;
    function GetCommandsCount(): UINT32; stdcall;
    function CreatePathGeometry(fillMode: D2D1_FILL_MODE; out pathGeometry: ID2D1PathGeometry1): HRESULT; stdcall;


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
    // Native virtual slots; C++ inline helpers are not entries.
    procedure GetDocument(out document: ID2D1SvgDocument); stdcall;
    function GetTagName(name: PWideChar; nameCount: UINT32): HRESULT; stdcall;
    function GetTagNameLength(): UINT32; stdcall;
    function IsTextContent(): BOOL; stdcall;
    procedure GetParent(out parent: ID2D1SvgElement); stdcall;
    function HasChildren(): BOOL; stdcall;
    procedure GetFirstChild(out child: ID2D1SvgElement); stdcall;
    procedure GetLastChild(out child: ID2D1SvgElement); stdcall;
    function GetPreviousChild(referenceChild: ID2D1SvgElement; out previousChild: ID2D1SvgElement): HRESULT; stdcall;
    function GetNextChild(referenceChild: ID2D1SvgElement; out nextChild: ID2D1SvgElement): HRESULT; stdcall;
    function InsertChildBefore(newChild: ID2D1SvgElement; referenceChild: ID2D1SvgElement): HRESULT; stdcall;
    function AppendChild(newChild: ID2D1SvgElement): HRESULT; stdcall;
    function ReplaceChild(newChild: ID2D1SvgElement; oldChild: ID2D1SvgElement): HRESULT; stdcall;
    function RemoveChild(oldChild: ID2D1SvgElement): HRESULT; stdcall;
    function CreateChild(tagName: PWideChar; out newChild: ID2D1SvgElement): HRESULT; stdcall;
    function IsAttributeSpecified(name: PWideChar; _inherited: PBOOL): BOOL; stdcall;
    function GetSpecifiedAttributeCount(): UINT32; stdcall;
    function GetSpecifiedAttributeName(index: UINT32; name: PWideChar; nameCount: UINT32; _inherited: PBOOL): HRESULT; stdcall;
    function GetSpecifiedAttributeNameLength(index: UINT32; out nameLength: UINT32; _inherited: PBOOL): HRESULT; stdcall;
    function RemoveAttribute(name: PWideChar): HRESULT; stdcall;
    function SetTextValue(name: PWideChar; nameCount: UINT32): HRESULT; stdcall;
    function GetTextValue(name: PWideChar; nameCount: UINT32): HRESULT; stdcall;
    function GetTextValueLength(): UINT32; stdcall;
    function SetAttributeValue(name: PWideChar; value: ID2D1SvgAttribute): HRESULT; stdcall; overload;
    function SetAttributeValue(name: PWideChar; _type: D2D1_SVG_ATTRIBUTE_POD_TYPE; value: Pointer; valueSizeInBytes: UINT32): HRESULT; stdcall; overload;
    function SetAttributeValue(name: PWideChar; _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE; value: PWideChar): HRESULT; stdcall; overload;
    function GetAttributeValue(name: PWideChar; const riid: TGUID; out value): HRESULT; stdcall; overload;
    function GetAttributeValue(name: PWideChar; _type: D2D1_SVG_ATTRIBUTE_POD_TYPE; value: Pointer; valueSizeInBytes: UINT32): HRESULT; stdcall; overload;
    function GetAttributeValue(name: PWideChar; _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE; value: PWideChar; valueCount: UINT32): HRESULT; stdcall; overload;
    function GetAttributeValueLength(name: PWideChar; _type: D2D1_SVG_ATTRIBUTE_STRING_TYPE; out valueLength: UINT32): HRESULT; stdcall;


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
    // Native virtual slots; C++ inline helpers are not entries.
    function SetViewportSize(viewportSize: D2D1_SIZE_F): HRESULT; stdcall;
    procedure GetViewportSize(out size: D2D1_SIZE_F); stdcall;
    function SetRoot(root: ID2D1SvgElement): HRESULT; stdcall;
    procedure GetRoot(out root: ID2D1SvgElement); stdcall;
    function FindElementById(id: PWideChar; out svgElement: ID2D1SvgElement): HRESULT; stdcall;
    function Serialize(outputXmlStream: IStream; subtree: ID2D1SvgElement): HRESULT; stdcall;
    function Deserialize(inputXmlStream: IStream; out subtree: ID2D1SvgElement): HRESULT; stdcall;
    function CreatePaint(paintType: D2D1_SVG_PAINT_TYPE; color: PD2D1_COLOR_F; id: PWideChar; out paint: ID2D1SvgPaint): HRESULT; stdcall;
    function CreateStrokeDashArray(dashes: PD2D1_SVG_LENGTH; dashesCount: UINT32; out strokeDashArray: ID2D1SvgStrokeDashArray): HRESULT; stdcall;
    function CreatePointCollection(points: PD2D1_POINT_2F; pointsCount: UINT32; out pointCollection: ID2D1SvgPointCollection): HRESULT; stdcall;
    function CreatePathData(segmentData: PSingle; segmentDataCount: UINT32; commands: PD2D1_SVG_PATH_COMMAND; commandsCount: UINT32; out pathData: ID2D1SvgPathData): HRESULT; stdcall;


  end;
  IID_ID2D1SvgDocument = ID2D1SvgDocument;
  {$EXTERNALSYM IID_ID2D1SvgDocument}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
