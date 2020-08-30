// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2D1Helper.pas
// Kind: Pascal / Delphi unit
// Release date: 29-03-2019
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
// Remarks: Because Delphi does not accept complex types (like functions) as default values,
//          the methods parameter will be set in the methods body.
//          Also, parameters with none default values after a parameter with a default value are not
//          accepted eather, so we chanched the parameter order the concerning methods.
//          For internal methods that should not be a problem.
//
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.18362.0 (19H1)
//
// Todo: -
//
//==============================================================================
// Source: d2d1helper.h
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
unit WinApi.DirectX.D2D1Helper;

  {$HPPEMIT '#include "d2d1helper.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  {WinApi.DirectX}
  WinApi.DirectX.D2D1,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}


  //
  // Forward declared IdentityMatrix function to allow matrix class to use
  // these constructors.
  //

  function IdentityMatrix(): D2D1_MATRIX_3X2_F; inline;
  {$EXTERNALSYM IdentityMatrix}

type
    //
    // The default trait type for objects in D2D is Single.
    //
    PTypeTraits = ^TypeTraits;
    TypeTraits = record
      Point: D2D1_POINT_2F;
      Size: D2D1_SIZE_F;
      Rect: D2D1_RECT_F;
    end;
    {$EXTERNALSYM TypeTraits}


    PTypeTraitsUINT32 = ^TypeTraitsUINT32;
    TypeTraitsUINT32 = record
      Point: D2D1_POINT_2U;
      Size: D2D1_SIZE_U;
      Rect: D2D1_RECT_U;
    end;
    {$EXTERNALSYM TypeTraitsUINT32}

    function FloatMax(): Single; inline;
    {$EXTERNALSYM FloatMax}


    //
    // Construction helpers
    //

    // D2D1_POINT_2F
    function Point2F(x: Single = 0.0;
                     y: Single = 0.0): D2D1_POINT_2F; inline;
    {$EXTERNALSYM Point2F}

    // D2D1_POINT_2U
    function Point2U(x: UINT32 = 0;
                     y: UINT32 = 0): D2D1_POINT_2U; inline;
    {$EXTERNALSYM Point2U}

    // D2D1_SIZE_F
    function SizeF(Width: Single = 0.0;
                   Height: Single = 0.0): D2D1_SIZE_F; inline;
    {$EXTERNALSYM SizeF}

    // D2D1_SIZE_U
    function SizeU(Width: UINT32 = 0;
                   Height: UINT32 = 0): D2D1_SIZE_U; inline;
    {$EXTERNALSYM SizeU}

    function Identity(): D2D1_MATRIX_3X2_F;
    {$EXTERNALSYM Identity}


type

    D2D1_RECT_F = D2D_RECT_F;
    {$EXTERNALSYM D2D1_RECT_F}

    function RectF(left: Single = 0.0;
                   top: Single = 0.0;
                   right: Single = 0.0;
                   bottom: Single = 0.0): D2D1_RECT_F; inline;
    {$EXTERNALSYM RectF}

    function RectU(left: UINT32 = 0;
                   top: UINT32 = 0;
                   right: UINT32 = 0;
                   bottom: UINT32 = 0): D2D1_RECT_U; inline;
    {$EXTERNALSYM RectU}

    function InfiniteRect(): D2D1_RECT_F;
    {$EXTERNALSYM InfiniteRect}

    function ArcSegment(point: D2D1_POINT_2F;
                        size: D2D1_SIZE_F;
                        rotationAngle: Single;
                        sweepDirection: D2D1_SWEEP_DIRECTION;
                        arcSize: D2D1_ARC_SIZE): D2D1_ARC_SEGMENT; inline;
    {$EXTERNALSYM ArcSegment}

    function BezierSegment(point1: D2D1_POINT_2F;
                           point2: D2D1_POINT_2F;
                           point3: D2D1_POINT_2F): D2D1_BEZIER_SEGMENT; inline;
    {$EXTERNALSYM BezierSegment}

    function Ellipse(center: D2D1_POINT_2F;
                     radiusX: Single;
                     radiusY: Single): D2D1_ELLIPSE; inline;
    {$EXTERNALSYM Ellipse}

    function RoundedRect(rect: D2D1_RECT_F;
                         radiusX: Single;
                         radiusY: Single): D2D1_ROUNDED_RECT; inline;
    {$EXTERNALSYM RoundedRect}

    // Delphi Note: The default value will be set in the funtionbody
    function BrushProperties(transform: D2D1_MATRIX_3X2_F{ = IdentityMatrix()};
                         opacity: Single = 1.0): D2D1_BRUSH_PROPERTIES; inline;
    {$EXTERNALSYM BrushProperties}

    function GradientStop(position: Single;
                          color: D2D1_COLOR_F): D2D1_GRADIENT_STOP; inline;
    {$EXTERNALSYM GradientStop}

    function QuadraticBezierSegment(point1: D2D1_POINT_2F;
                                    point2: D2D1_POINT_2F): D2D1_QUADRATIC_BEZIER_SEGMENT; inline;
    {$EXTERNALSYM QuadraticBezierSegment}

    function StrokeStyleProperties(startCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                   endCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                   dashCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                                   lineJoin: D2D1_LINE_JOIN = D2D1_LINE_JOIN_MITER;
                                   miterLimit: Single = 10.0;
                                   dashStyle: D2D1_DASH_STYLE = D2D1_DASH_STYLE_SOLID;
                                   dashOffset: Single = 0.0): D2D1_STROKE_STYLE_PROPERTIES; inline;
    {$EXTERNALSYM StrokeStyleProperties}

    function BitmapBrushProperties(extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                   extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                                   interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR): D2D1_BITMAP_BRUSH_PROPERTIES; inline;
    {$EXTERNALSYM BitmapBrushProperties}

    function LinearGradientBrushProperties(startPoint: D2D1_POINT_2F;
                                           endPoint: D2D1_POINT_2F): D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES; inline;
    {$EXTERNALSYM LinearGradientBrushProperties}

    function RadialGradientBrushProperties(center: D2D1_POINT_2F;
                                           gradientOriginOffset: D2D1_POINT_2F;
                                           radiusX: Single;
                                           radiusY: Single): D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES; inline;
    {$EXTERNALSYM RadialGradientBrushProperties}

    //
    // PixelFormat
    //
    // Delphi Note: To prevent confusion about the function PixelFormat() and recordfield PixelFormat,
    //              We changed the names of all recordfields to _pixelformat
    function PixelFormat(dxgiFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         alphaMode: D2D1_ALPHA_MODE = D2D1_ALPHA_MODE_UNKNOWN): D2D1_PIXEL_FORMAT; inline;
    {$EXTERNALSYM PixelFormat}

    //
    // Bitmaps
    //

    // Delphi Note: The default value will be set in the funtionbody
    function BitmapProperties(_pixelFormat: D2D1_PIXEL_FORMAT{ = PixelFormat()};
                              dpiX: Single = 96.0;
                              dpiY: Single = 96.0): D2D1_BITMAP_PROPERTIES; inline;
    {$EXTERNALSYM BitmapProperties}


    //
    // Render Targets
    //
    // Delphi Note: The default value will be set in the funtionbody
    function RenderTargetProperties(_pixelFormat: D2D1_PIXEL_FORMAT{ = PixelFormat()};
                                    _type: D2D1_RENDER_TARGET_TYPE = D2D1_RENDER_TARGET_TYPE_DEFAULT;
                                    dpiX: Single = 0.0;
                                    dpiY: Single = 0.0;
                                    usage: D2D1_RENDER_TARGET_USAGE = D2D1_RENDER_TARGET_USAGE_NONE;
                                    minLevel: D2D1_FEATURE_LEVEL = D2D1_FEATURE_LEVEL_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES; inline;
    {$EXTERNALSYM RenderTargetProperties}


    function HwndRenderTargetProperties(_hwnd: HWND;
                                        pixelSize: D2D1_SIZE_U {= Size(UINT32(0), UINT32(0))};
                                        presentOptions: D2D1_PRESENT_OPTIONS = D2D1_PRESENT_OPTIONS_NONE): D2D1_HWND_RENDER_TARGET_PROPERTIES; inline;
    {$EXTERNALSYM HwndRenderTargetProperties}


    function LayerParameters(contentBounds: D2D1_RECT_F{ = InfiniteRect()}; // Delphi Note: You have to set the default value for D2D1_RECT_F before calling this function.
                             maskTransform: D2D1_MATRIX_3X2_F{ = IdentityMatrix()};
                             geometricMask: ID2D1Geometry = Nil;
                             maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                             opacity: Single = 1.0;
                             opacityBrush: ID2D1Brush = Nil;
                             layerOptions: D2D1_LAYER_OPTIONS = D2D1_LAYER_OPTIONS_NONE): D2D1_LAYER_PARAMETERS; inline;
    {$EXTERNALSYM LayerParameters}


    function DrawingStateDescription(transform: D2D1_MATRIX_3X2_F{ = IdentityMatrix()};
                                     antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                     textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                     tag1: D2D1_TAG = 0;
                                     tag2: D2D1_TAG = 0): D2D1_DRAWING_STATE_DESCRIPTION; inline;
    {$EXTERNALSYM DrawingStateDescription}




  //
  // Colors, this enum defines a set of predefined colors.
  //
  // Delphi Note: We don't use the enum, but public constants.
  //              The values are translated to TColor values, to meet the equal colors,
  //              specified in the original headerfile.

const

  AliceBlue =            TColor($F0F8FF);
  {$EXTERNALSYM AliceBlue}
  AntiqueWhite =         TColor($FAEBD7);
  {$EXTERNALSYM AntiqueWhite}
  Aqua =                 TColor($00FFFF);
  {$EXTERNALSYM Aqua}
  Aquamarine =           TColor($7FFFD4);
  {$EXTERNALSYM Aquamarine}
  Azure =                TColor($F0FFFF);
  {$EXTERNALSYM Azure}
  Beige =                TColor($F5F5DC);
  {$EXTERNALSYM Beige}
  Bisque =               TColor($FFE4C4);
  {$EXTERNALSYM Bisque}
  Black =                TColor($000000);
  {$EXTERNALSYM Black}
  BlanchedAlmond =       TColor($FFEBCD);
  {$EXTERNALSYM BlanchedAlmond}
  Blue =                 TColor($0000FF);
  {$EXTERNALSYM Blue}
  BlueViolet =           TColor($8A2BE2);
  {$EXTERNALSYM BlueViolet}
  Brown =                TColor($A52A2A);
  {$EXTERNALSYM Brown}
  BurlyWood =            TColor($DEB887);
  {$EXTERNALSYM BurlyWood}
  CadetBlue =            TColor($5F9EA0);
  {$EXTERNALSYM CadetBlue}
  Chartreuse =           TColor($7FFF00);
  {$EXTERNALSYM Chartreuse}
  Chocolate =            TColor($D2691E);
  {$EXTERNALSYM Chocolate}
  Coral =                TColor($FF7F50);
  {$EXTERNALSYM Coral}
  CornflowerBlue =       TColor($6495ED);
  {$EXTERNALSYM CornflowerBlue}
  Cornsilk =             TColor($FFF8DC);
  {$EXTERNALSYM Cornsilk}
  Crimson =              TColor($DC143C);
  {$EXTERNALSYM Crimson}
  Cyan =                 TColor($00FFFF);
  {$EXTERNALSYM Cyan}
  DarkBlue =             TColor($00008B);
  {$EXTERNALSYM DarkBlue}
  DarkCyan =             TColor($008B8B);
  {$EXTERNALSYM DarkCyan}
  DarkGoldenrod =        TColor($B8860B);
  {$EXTERNALSYM DarkGoldenrod}
  DarkGray =             TColor($A9A9A9);
  {$EXTERNALSYM DarkGray}
  DarkGreen =            TColor($006400);
  {$EXTERNALSYM DarkGreen}
  DarkKhaki =            TColor($BDB76B);
  {$EXTERNALSYM DarkKhaki}
  DarkMagenta =          TColor($8B008B);
  {$EXTERNALSYM DarkMagenta}
  DarkOliveGreen =       TColor($556B2F);
  {$EXTERNALSYM DarkOliveGreen}
  DarkOrange =           TColor($FF8C00);
  {$EXTERNALSYM DarkOrange}
  DarkOrchid =           TColor($9932CC);
  {$EXTERNALSYM DarkOrchid}
  DarkRed =              TColor($8B0000);
  {$EXTERNALSYM DarkRed}
  DarkSalmon =           TColor($E9967A);
  {$EXTERNALSYM DarkSalmon}
  DarkSeaGreen =         TColor($8FBC8F);
  {$EXTERNALSYM DarkSeaGreen}
  DarkSlateBlue =        TColor($483D8B);
  {$EXTERNALSYM DarkSlateBlue}
  DarkSlateGray =        TColor($2F4F4F);
  {$EXTERNALSYM DarkSlateGray}
  DarkTurquoise =        TColor($00CED1);
  {$EXTERNALSYM DarkTurquoise}
  DarkViolet =           TColor($9400D3);
  {$EXTERNALSYM DarkViolet}
  DeepPink =             TColor($FF1493);
  {$EXTERNALSYM DeepPink}
  DeepSkyBlue =          TColor($00BFFF);
  {$EXTERNALSYM DeepSkyBlue}
  DimGray =              TColor($696969);
  {$EXTERNALSYM DimGray}
  DodgerBlue =           TColor($1E90FF);
  {$EXTERNALSYM DodgerBlue}
  Firebrick =            TColor($B22222);
  {$EXTERNALSYM Firebrick}
  FloralWhite =          TColor($FFFAF0);
  {$EXTERNALSYM FloralWhite}
  ForestGreen =          TColor($228B22);
  {$EXTERNALSYM ForestGreen}
  Fuchsia =              TColor($FF00FF);
  {$EXTERNALSYM Fuchsia}
  Gainsboro =            TColor($DCDCDC);
  {$EXTERNALSYM Gainsboro}
  GhostWhite =           TColor($F8F8FF);
  {$EXTERNALSYM GhostWhite}
  Gold =                 TColor($FFD700);
  {$EXTERNALSYM Gold}
  Goldenrod =            TColor($DAA520);
  {$EXTERNALSYM Goldenrod}
  Gray =                 TColor($808080);
  {$EXTERNALSYM Gray}
  Green =                TColor($008000);
  {$EXTERNALSYM Green}
  GreenYellow =          TColor($ADFF2F);
  {$EXTERNALSYM GreenYellow}
  Honeydew =             TColor($F0FFF0);
  {$EXTERNALSYM Honeydew}
  HotPink =              TColor($FF69B4);
  {$EXTERNALSYM HotPink}
  IndianRed =            TColor($CD5C5C);
  {$EXTERNALSYM IndianRed}
  Indigo =               TColor($4B0082);
  {$EXTERNALSYM Indigo}
  Ivory =                TColor($FFFFF0);
  {$EXTERNALSYM Ivory}
  Khaki =                TColor($F0E68C);
  {$EXTERNALSYM Khaki}
  Lavender =             TColor($E6E6FA);
  {$EXTERNALSYM Lavender}
  LavenderBlush =        TColor($FFF0F5);
  {$EXTERNALSYM LavenderBlush}
  LawnGreen =            TColor($7CFC00);
  {$EXTERNALSYM LawnGreen}
  LemonChiffon =         TColor($FFFACD);
  {$EXTERNALSYM LemonChiffon}
  LightBlue =            TColor($ADD8E6);
  {$EXTERNALSYM LightBlue}
  LightCoral =           TColor($F08080);
  {$EXTERNALSYM LightCoral}
  LightCyan =            TColor($E0FFFF);
  {$EXTERNALSYM LightCyan}
  LightGoldenrodYellow = TColor($FAFAD2);
  {$EXTERNALSYM LightGoldenrodYellow}
  LightGreen =           TColor($90EE90);
  {$EXTERNALSYM LightGreen}
  LightGray =            TColor($D3D3D3);
  {$EXTERNALSYM LightGray}
  LightPink =            TColor($FFB6C1);
  {$EXTERNALSYM LightPink}
  LightSalmon =          TColor($FFA07A);
  {$EXTERNALSYM LightSalmon}
  LightSeaGreen =        TColor($20B2AA);
  {$EXTERNALSYM LightSeaGreen}
  LightSkyBlue =         TColor($87CEFA);
  {$EXTERNALSYM LightSkyBlue}
  LightSlateGray =       TColor($778899);
  {$EXTERNALSYM LightSlateGray}
  LightSteelBlue =       TColor($B0C4DE);
  {$EXTERNALSYM LightSteelBlue}
  LightYellow =          TColor($FFFFE0);
  {$EXTERNALSYM LightYellow}
  Lime =                 TColor($00FF00);
  {$EXTERNALSYM Lime}
  LimeGreen =            TColor($32CD32);
  {$EXTERNALSYM LimeGreen}
  Linen =                TColor($FAF0E6);
  {$EXTERNALSYM Linen}
  Magenta =              TColor($FF00FF);
  {$EXTERNALSYM Magenta}
  Maroon =               TColor($800000);
  {$EXTERNALSYM Maroon}
  MediumAquamarine =     TColor($66CDAA);
  {$EXTERNALSYM MediumAquamarine}
  MediumBlue =           TColor($0000CD);
  {$EXTERNALSYM MediumBlue}
  MediumOrchid =         TColor($BA55D3);
  {$EXTERNALSYM MediumOrchid}
  MediumPurple =         TColor($9370DB);
  {$EXTERNALSYM MediumPurple}
  MediumSeaGreen =       TColor($3CB371);
  {$EXTERNALSYM MediumSeaGreen}
  MediumSlateBlue =      TColor($7B68EE);
  {$EXTERNALSYM MediumSlateBlue}
  MediumSpringGreen =    TColor($00FA9A);
  {$EXTERNALSYM MediumSpringGreen}
  MediumTurquoise =      TColor($48D1CC);
  {$EXTERNALSYM MediumTurquoise}
  MediumVioletRed =      TColor($C71585);
  {$EXTERNALSYM MediumVioletRed}
  MidnightBlue =         TColor($191970);
  {$EXTERNALSYM MidnightBlue}
  MintCream =            TColor($F5FFFA);
  {$EXTERNALSYM MintCream}
  MistyRose =            TColor($FFE4E1);
  {$EXTERNALSYM MistyRose}
  Moccasin =             TColor($FFE4B5);
  {$EXTERNALSYM Moccasin}
  NavajoWhite =          TColor($FFDEAD);
  {$EXTERNALSYM NavajoWhite}
  Navy =                 TColor($000080);
  {$EXTERNALSYM Navy}
  OldLace =              TColor($FDF5E6);
  {$EXTERNALSYM OldLace}
  Olive =                TColor($808000);
  {$EXTERNALSYM Olive}
  OliveDrab =            TColor($6B8E23);
  {$EXTERNALSYM OliveDrab}
  Orange =               TColor($FFA500);
  {$EXTERNALSYM Orange}
  OrangeRed =            TColor($FF4500);
  {$EXTERNALSYM OrangeRed}
  Orchid =               TColor($DA70D6);
  {$EXTERNALSYM Orchid}
  PaleGoldenrod =        TColor($EEE8AA);
  {$EXTERNALSYM PaleGoldenrod}
  PaleGreen =            TColor($98FB98);
  {$EXTERNALSYM PaleGreen}
  PaleTurquoise =        TColor($AFEEEE);
  {$EXTERNALSYM PaleTurquoise}
  PaleVioletRed =        TColor($DB7093);
  {$EXTERNALSYM PaleVioletRed}
  PapayaWhip =           TColor($FFEFD5);
  {$EXTERNALSYM PapayaWhip}
  PeachPuff =            TColor($FFDAB9);
  {$EXTERNALSYM PeachPuff}
  Peru =                 TColor($CD853F);
  {$EXTERNALSYM Peru}
  Pink =                 TColor($FFC0CB);
  {$EXTERNALSYM Pink}
  Plum =                 TColor($DDA0DD);
  {$EXTERNALSYM Plum}
  PowderBlue =           TColor($B0E0E6);
  {$EXTERNALSYM PowderBlue}
  Purple =               TColor($800080);
  {$EXTERNALSYM Purple}
  Red =                  TColor($FF0000);
  {$EXTERNALSYM Red}
  RosyBrown =            TColor($BC8F8F);
  {$EXTERNALSYM RosyBrown}
  RoyalBlue =            TColor($4169E1);
  {$EXTERNALSYM RoyalBlue}
  SaddleBrown =          TColor($8B4513);
  {$EXTERNALSYM SaddleBrown}
  Salmon =               TColor($FA8072);
  {$EXTERNALSYM Salmon}
  SandyBrown =           TColor($F4A460);
  {$EXTERNALSYM SandyBrown}
  SeaGreen =             TColor($2E8B57);
  {$EXTERNALSYM SeaGreen}
  SeaShell =             TColor($FFF5EE);
  {$EXTERNALSYM SeaShell}
  Sienna =               TColor($A0522D);
  {$EXTERNALSYM Sienna}
  Silver =               TColor($C0C0C0);
  {$EXTERNALSYM Silver}
  SkyBlue =              TColor($87CEEB);
  {$EXTERNALSYM SkyBlue}
  SlateBlue =            TColor($6A5ACD);
  {$EXTERNALSYM SlateBlue}
  SlateGray =            TColor($708090);
  {$EXTERNALSYM SlateGray}
  Snow =                 TColor($FFFAFA);
  {$EXTERNALSYM Snow}
  SpringGreen =          TColor($00FF7F);
  {$EXTERNALSYM SpringGreen}
  SteelBlue =            TColor($4682B4);
  {$EXTERNALSYM SteelBlue}
  Tan =                  TColor($D2B48C);
  {$EXTERNALSYM Tan}
  Teal =                 TColor($008080);
  {$EXTERNALSYM Teal}
  Thistle =              TColor($D8BFD8);
  {$EXTERNALSYM Thistle}
  Tomato =               TColor($FF6347);
  {$EXTERNALSYM Tomato}
  Turquoise =            TColor($40E0D0);
  {$EXTERNALSYM Turquoise}
  Violet =               TColor($EE82EE);
  {$EXTERNALSYM Violet}
  Wheat =                TColor($F5DEB3);
  {$EXTERNALSYM Wheat}
  White =                TColor($FFFFFF);
  {$EXTERNALSYM White}
  WhiteSmoke =           TColor($F5F5F5);
  {$EXTERNALSYM WhiteSmoke}
  Yellow =               TColor($FFFF00);
  {$EXTERNALSYM Yellow}
  YellowGreen =          TColor($9ACD32);
  {$EXTERNALSYM YellowGreen}


type

    // Record helpers were introduced in Delphi XE3.
    // To keep the code usable for earlier versions, we added the helper methods here and
    // a separate record helper in D2D1Helper.pas for later versions

{$IF CompilerVersion > 23}  // > XE2

  // ColorF => D2D1_COLOR_F , see DXGIType.pas > D2D1_COLOR_F > Helpers

  D2D1ColorFHelper = record helper for D2D1_COLOR_F

    //
    // Construct a color, note that the alpha value from the "rgb" component
    // is never used.
    //

    function Init(const rgb: UINT32;
                  const alpha: Single = 1.0): D2D1_COLOR_F; overload;

    function Init(const knownColor: TColor;
                  const alpha: Single = 1.0): D2D1_COLOR_F; overload;

    function Init(const red: Single;
                  const green: Single;
                  const blue: Single;
                  const alpha: Single = 1.0): D2D1_COLOR_F; overload;

    class function Implicit(const val: DWORD): D2D1_COLOR_F; overload; static;

    class function Implicit(const val: D2D1_COLOR_F): DWORD; overload; static;

    class function Equal(const left: D2D1_COLOR_F;
                         const right: D2D1_COLOR_F): Boolean; static;
  end;
  {$EXTERNALSYM D2D1ColorFHelper}

{$ENDIF}


{$IF CompilerVersion > 23}  // > XE2

  // D2D1_MATRIX_3X2_F
  Matrix3x2FHelper = record helper for D2D1_MATRIX_3X2_F
    public
    //
    // Creates an initialized matrix
    //

    class function Init(const m11: Single;
                        const m12: Single;
                        const m21: Single;
                        const m22: Single;
                        const dx: Single;
                        const dy: Single): D2D1_MATRIX_3X2_F; static;

    //
    // Conversion methods TRect <-> D2D_RECT_F
    //


    //
    // Creates an uninitialized matrix
    //
    class function Identity(): D2D1_MATRIX_3X2_F; static;

    //
    // Named quasi-constructors
    //

    class function Translation(const size: D2D1_SIZE_F): D2D1_MATRIX_3X2_F; overload; static;

    class function Translation(const x: Single;
                               const y: Single): D2D1_MATRIX_3X2_F; overload; static;


    class function Scale(const size: D2D1_SIZE_F;
                         const center: D2D1_POINT_2F { = Point2F() }): D2D1_MATRIX_3X2_F; overload; static;

    class function Scale(const x: Single;
                         const y: Single;
                         const center: D2D1_POINT_2F { = Point2F() }): D2D1_MATRIX_3X2_F; overload; static;



    class function Rotation(const angle: Single;
                            const center: D2D1_POINT_2F { = Point2F() }): D2D1_MATRIX_3X2_F; static;

    class function Skew(const angleX: Single;
                        const angleY: Single;
                        const center: D2D1_POINT_2F { = Point2F() }): D2D1_MATRIX_3X2_F; static;

    //
    // Functions for convertion from the base D2D1_MATRIX_3X2_F to this type
    // without making a copy
    //

    class function ReinterpretBaseType(const pMatrix: PD2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F; overload; static;

    class function ReinterpretBaseType(const pMatrix: D2D1_MATRIX_3X2_F): PD2D1_MATRIX_3X2_F; overload; static;


    function Determinant(): Single;

    function IsInvertible(): Boolean;

    function Invert(): Boolean;

    function IsIdentity(): Boolean;


    function SetProduct(const a: D2D1_MATRIX_3X2_F;
                        const b: D2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F; overload;

    function SetProduct(const matrix: D2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F; overload;


    function TransformPoint(const point: D2D1_POINT_2F): D2D1_POINT_2F;

    function Equal(const size1: D2D1_SIZE_U;
                   const size2: D2D1_SIZE_U): Boolean;
  end;
  {$EXTERNALSYM Matrix3x2FHelper}
{$ENDIF}



  // Additional Prototypes

  function D2D1PointF(const x: Single;
                      const y: Single) : D2D1_POINT_2F;
  {$EXTERNALSYM D2D1SizeF}

  function D2D1SizeF(const width: Single;
                     const height: Single) : D2D1_SIZE_F;
  {$EXTERNALSYM D2D1PointF}

  function D2D1SizeU(const Width: UINT32;
                     const Height: UINT32) : D2D1_SIZE_U;
  {$EXTERNALSYM D2D1RectF}

  function D2D1RectF(const left: Single;
                     const top: Single;
                     const right: Single;
                     const bottom: Single): D2D1_RECT_F;
  {$EXTERNALSYM D2D1SizeU}

  procedure D2D1RectFDefault(var crRect: D2D1_RECT_F); inline;
  {$EXTERNALSYM D2D1RectFDefault}

  function D2D1ArcSegment(const Point: D2D1_POINT_2F;
                          const Size: D2D1_SIZE_F;
                          RotationAngle: Single;
                          const sweepDirection: D2D1_SWEEP_DIRECTION;
                          const ArcSize: D2D1_ARC_SIZE): D2D1_ARC_SEGMENT;
  {$EXTERNALSYM D2D1ArcSegment}


{$IFDEF D2D1ColorF_DEFINED}
  function D2D1ColorF(const r: Single;
                      const g: Single;
                      const b: Single;
                      const a: Single): D2D1_COLOR_F;  overload;
  {$EXTERNALSYM D2D1ColorF}

  // See also: D2D1colorhelperF
  function D2D1ColorF(color: TColor; const alpha: Single = 1.0): D2D1_COLOR_F; overload;
  {$EXTERNALSYM D2D1ColorF}
{$ENDIF}

  function D2D1BezierSegment(const a: D2D1_POINT_2F;
                             const b: D2D1_POINT_2F;
                             const c: D2D1_POINT_2F) : D2D1_BEZIER_SEGMENT;
  {$EXTERNALSYM D2D1BezierSegment}

  function D2D1GradientStop(const Position: Single;
                            const Color: D2D1_COLOR_F): D2D1_GRADIENT_STOP;
  {$EXTERNALSYM D2D1GradientStop}

  function D2D1QuadraticBezierSegment(const a: D2D1_POINT_2F;
                                      const b: D2D1_POINT_2F) : D2D1_QUADRATIC_BEZIER_SEGMENT;
  {$EXTERNALSYM D2D1QuadraticBezierSegment}

  function D2D1StrokeStyleProperties(StartCap: D2D1_CAP_STYLE;
                                     EndCap: D2D1_CAP_STYLE;
                                     DashCap: D2D1_CAP_STYLE;
                                     LineJoin: D2D1_LINE_JOIN;
                                     MiterLimit: Single;
                                     DashStyle: D2D1_DASH_STYLE;
                                     DashOffset: Single): D2D1_STROKE_STYLE_PROPERTIES;
  {$EXTERNALSYM D2D1StrokeStyleProperties}

  function D2D1Ellipse(center: D2D1_POINT_2F;
                       const rx: Single;
                       const ry: Single): D2D1_ELLIPSE;
  {$EXTERNALSYM D2D1Ellipse}

  function D2D1RoundedRect(const Rect: D2D1_RECT_F;
                           RadiusX: Single;
                           RadiusY: Single): D2D1_ROUNDED_RECT;
  {$EXTERNALSYM D2D1RoundedRect}

  function D2D1LinearGradientBrushProperties(const StartPoint: D2D1_POINT_2F;
                                             const EndPoint: D2D1_POINT_2F): D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
  {$EXTERNALSYM D2D1LinearGradientBrushProperties}

  function D2D1RadialGradientBrushProperties(const Center: D2D1_POINT_2F;
                                             const GradientOriginOffset: D2D1_POINT_2F;
                                             RadiusX: Single;
                                             RadiusY: Single): D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
  {$EXTERNALSYM D2D1RadialGradientBrushProperties}

  function D2D1PixelFormat(const DxgiFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                           AlphaMode: D2D1_ALPHA_MODE = D2D1_ALPHA_MODE_UNKNOWN): D2D1_PIXEL_FORMAT;
  {$EXTERNALSYM D2D1PixelFormat}

  function D2D1RenderTargetProperties(_Type: D2D1_RENDER_TARGET_TYPE = D2D1_RENDER_TARGET_TYPE_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES; overload;
  {$EXTERNALSYM D2D1RenderTargetProperties}


  function D2D1RenderTargetProperties(_Type: D2D1_RENDER_TARGET_TYPE;
                                      const _PixelFormat: D2D1_PIXEL_FORMAT;
                                      DpiX: Single = 0;
                                      DpiY: Single = 0;
                                      Usage: D2D1_RENDER_TARGET_USAGE = D2D1_RENDER_TARGET_USAGE_NONE;
                                      MinLevel: D2D1_FEATURE_LEVEL = D2D1_FEATURE_LEVEL_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES; overload;
  {$EXTERNALSYM D2D1RenderTargetProperties}

  function D2D1HwndRenderTargetProperties(_hwnd: HWND): D2D1_HWND_RENDER_TARGET_PROPERTIES; overload;
  {$EXTERNALSYM D2D1HwndRenderTargetProperties}

  function D2D1HwndRenderTargetProperties(_hwnd: HWND;
                                          _PixelSize: D2D1_SIZE_U;
                                          PresentOptions: D2D1_PRESENT_OPTIONS = D2D1_PRESENT_OPTIONS_NONE): D2D1_HWND_RENDER_TARGET_PROPERTIES; overload;
  {$EXTERNALSYM D2D1HwndRenderTargetProperties}

  function D2D1BitmapProperties(): D2D1_BITMAP_PROPERTIES; overload;
  {$EXTERNALSYM D2D1BitmapProperties}

  function D2D1BitmapProperties(_PixleFormat: D2D1_PIXEL_FORMAT;
                                DpiX: Single = 96.0;
                                DpiY: Single = 96.0): D2D1_BITMAP_PROPERTIES; overload;
  {$EXTERNALSYM D2D1BitmapProperties}

  // End of Additional Prototypes


implementation


function FloatMax(): Single;
begin
  {$IFDEF FLT_MAX}
    Result := FLT_MAX;
  {$ELSE}
    Result := 3.402823466e+38;
  {$ENDIF}
end;


// D2D1_POINT_2F
function Point2F(x: Single = 0.0;
                 y: Single = 0.0): D2D1_POINT_2F;
begin
  Result.x := x;
  Result.y := y;
end;


// D2D1_POINT_2U
function Point2U(x: UINT32 = 0;
                 y: UINT32 = 0): D2D1_POINT_2U;
begin
  Result.x := x;
  Result.y := y;
end;

// D2D1_SIZE_F
function SizeF(Width: Single = 0.0;
               Height: Single = 0.0): D2D1_SIZE_F;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

// D2D1_SIZE_U
function SizeU(Width: UINT32 = 0;
               Height: UINT32 = 0): D2D1_SIZE_U;
begin
  Result.Width := Width;
  Result.Height := Height;
end;


function Identity(): D2D1_MATRIX_3X2_F;
begin
  with Result do
    begin
      _11 := 1.0;
      _12 := 0.0;
      _21 := 0.0;
      _22 := 1.0;
      _31 := 0.0;
      _32 := 0.0;
    end;
end;

// D2D1_RECT_F
function RectF(left: Single = 0.0;
               top: Single = 0.0;
               right: Single = 0.0;
               bottom: Single = 0.0): D2D1_RECT_F;
begin
  Result.left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;

// D2D1_RECT_U
function RectU(left: UINT32 = 0;
               top: UINT32 = 0;
               right: UINT32 = 0;
               bottom: UINT32 = 0): D2D1_RECT_U;
begin
  Result.left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;


function InfiniteRect(): D2D1_RECT_F;
var
  rectf: D2D1_RECT_F;
begin
  with rectf do
    begin
      left := -FloatMax();
      top := -FloatMax();
      right := -FloatMax();
      bottom := -FloatMax();
    end;
  Result := rectf;
end;


function ArcSegment(point: D2D1_POINT_2F;
                    size: D2D1_SIZE_F;
                    rotationAngle: Single;
                    sweepDirection: D2D1_SWEEP_DIRECTION;
                    arcSize: D2D1_ARC_SIZE): D2D1_ARC_SEGMENT;
begin
  Result.point := point;
  Result.size := size;
  Result.rotationAngle := rotationAngle;
  Result.sweepDirection := sweepDirection;
  Result.arcSize := arcSize;
end;


function BezierSegment(point1: D2D1_POINT_2F;
                       point2: D2D1_POINT_2F;
                       point3: D2D1_POINT_2F): D2D1_BEZIER_SEGMENT;
begin
  Result.point1 := point1;
  Result.point2 := point2;
  Result.point3 := point3;
end;


function Ellipse(center: D2D1_POINT_2F;
                 radiusX: Single;
                 radiusY: Single): D2D1_ELLIPSE;
begin
  Result.point := center;
  Result.radiusX := radiusX;
  Result.radiusY := radiusY;
end;


function RoundedRect(rect: D2D1_RECT_F;
                     radiusX: Single;
                     radiusY: Single): D2D1_ROUNDED_RECT;
begin
  Result.rect := rect;
  Result.radiusX := radiusX;
  Result.radiusY := radiusY;
end;


function BrushProperties(transform: D2D1_MATRIX_3X2_F{ = IdentityMatrix()};
                         opacity: Single = 1.0): D2D1_BRUSH_PROPERTIES;
var
  tf: D2D1_MATRIX_3X2_F;

begin
  // To check if a record has a default value or not.
  if CompareMem(@tf,
                @transform,
                SizeOf(tf)) = True then
    tf := Identity() // IdentityMatrix()
  else
    tf := transform;

  Result.opacity := opacity;
  Result.transform := tf;
end;


function GradientStop(position: Single;
                      color: D2D1_COLOR_F): D2D1_GRADIENT_STOP;
begin
  Result.position := position;
  Result.color := color;
end;


function QuadraticBezierSegment(point1: D2D1_POINT_2F;
                                point2: D2D1_POINT_2F): D2D1_QUADRATIC_BEZIER_SEGMENT;
begin
  Result.point1 := point1;
  Result.point2 := point2;
end;


function StrokeStyleProperties(startCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                               endCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                               dashCap: D2D1_CAP_STYLE = D2D1_CAP_STYLE_FLAT;
                               lineJoin: D2D1_LINE_JOIN = D2D1_LINE_JOIN_MITER;
                               miterLimit: Single = 10.0;
                               dashStyle: D2D1_DASH_STYLE = D2D1_DASH_STYLE_SOLID;
                               dashOffset: Single = 0.0): D2D1_STROKE_STYLE_PROPERTIES;
begin
  Result.startCap := startCap;
  Result.endCap := endCap;
  Result.dashCap := dashCap;
  Result.lineJoin := lineJoin;
  Result.miterLimit := miterLimit;
  Result.dashStyle := dashStyle;
  Result.dashOffset := dashOffset;
end;


function BitmapBrushProperties(extendModeX: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                               extendModeY: D2D1_EXTEND_MODE = D2D1_EXTEND_MODE_CLAMP;
                               interpolationMode: D2D1_BITMAP_INTERPOLATION_MODE = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR): D2D1_BITMAP_BRUSH_PROPERTIES;
begin
  Result.extendModeX := extendModeX;
  Result.extendModeY := extendModeY;
  Result.interpolationMode := interpolationMode;
end;


function LinearGradientBrushProperties(startPoint: D2D1_POINT_2F;
                                       endPoint: D2D1_POINT_2F): D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
begin
  Result.startPoint := startPoint;
  Result.endPoint := endPoint;
end;


function RadialGradientBrushProperties(center: D2D1_POINT_2F;
                                           gradientOriginOffset: D2D1_POINT_2F;
                                           radiusX: Single;
                                           radiusY: Single): D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
begin
  Result.center := center;
  Result.gradientOriginOffset := gradientOriginOffset;
  Result.radiusX := radiusX;
  Result.radiusY := radiusY;
end;


function PixelFormat(dxgiFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                     alphaMode: D2D1_ALPHA_MODE = D2D1_ALPHA_MODE_UNKNOWN): D2D1_PIXEL_FORMAT;
begin
  Result.format := dxgiFormat;
  Result.alphaMode := alphaMode;
end;


function BitmapProperties(_pixelFormat: D2D1_PIXEL_FORMAT;
                          dpiX: Single = 96.0;
                          dpiY: Single = 96.0): D2D1_BITMAP_PROPERTIES;
var
  pf: D2D1_PIXEL_FORMAT;

begin
  // To check if a record has a default value or not.
  if CompareMem(@pf,
                @_pixelFormat,
                SizeOf(pf)) = True then
    pf := PixelFormat()
  else
    pf := _pixelFormat;


  Result._pixelFormat := _pixelFormat;
  Result.dpiX := dpiX;
  Result.dpiY := dpiY;
end;


function RenderTargetProperties(_pixelFormat: D2D1_PIXEL_FORMAT;
                                _type: D2D1_RENDER_TARGET_TYPE = D2D1_RENDER_TARGET_TYPE_DEFAULT;
                                dpiX: Single = 0.0;
                                dpiY: Single = 0.0;
                                usage: D2D1_RENDER_TARGET_USAGE = D2D1_RENDER_TARGET_USAGE_NONE;
                                minLevel: D2D1_FEATURE_LEVEL = D2D1_FEATURE_LEVEL_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES;
var
  pf: D2D1_PIXEL_FORMAT;

begin
  // To check if a record has a default value or not.
  if CompareMem(@pf,
                @_pixelFormat,
                SizeOf(pf)) = True then
    pf := PixelFormat()
  else
    pf := _pixelFormat;

  Result._type := _type;
  Result._pixelFormat := _pixelFormat;
  Result.dpiX := dpiX;
  Result.dpiY := dpiY;
  Result.usage := usage;
  Result.minLevel := minLevel;
end;


function HwndRenderTargetProperties(_hwnd: HWND;
                                    pixelSize: D2D1_SIZE_U;
                                    presentOptions: D2D1_PRESENT_OPTIONS = D2D1_PRESENT_OPTIONS_NONE): D2D1_HWND_RENDER_TARGET_PROPERTIES; inline;
var
  ps : D2D1_SIZE_U;

begin
  // To check if a record has a default value or not.
  if CompareMem(@ps,
                @pixelSize,
                SizeOf(ps)) = True then
    begin
      ps.width := 0;
      ps.height := 0;
    end
  else
    ps := pixelSize;

  Result._hwnd := _hwnd;
  Result._pixelSize := ps;
  Result.presentOptions := presentOptions;
end;


function LayerParameters(contentBounds: D2D1_RECT_F;
                         maskTransform: D2D1_MATRIX_3X2_F;
                         geometricMask: ID2D1Geometry = Nil;
                         maskAntialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                         opacity: Single = 1.0;
                         opacityBrush: ID2D1Brush = Nil;
                         layerOptions: D2D1_LAYER_OPTIONS = D2D1_LAYER_OPTIONS_NONE): D2D1_LAYER_PARAMETERS; inline;
var
  cb: D2D1_RECT_F;
  mt: D2D1_MATRIX_3X2_F;

begin
  // To check if a record has a default value or not.
  if CompareMem(@cb,
                @contentBounds,
                SizeOf(cb)) = True then
    cb := InfiniteRect()
  else
    cb := contentBounds;

  if CompareMem(@mt,
                @maskTransform,
                SizeOf(mt)) = True then
    mt := Identity() //IdentityMatrix()
  else
    mt := maskTransform;

  Result.contentBounds := cb;
  Result.geometricMask := geometricMask;
  Result.maskAntialiasMode := maskAntialiasMode;
  Result.maskTransform := mt;
  Result.opacity := opacity;
  Result.opacityBrush := opacityBrush;
  Result.layerOptions := layerOptions;
end;


function DrawingStateDescription(transform: D2D1_MATRIX_3X2_F;
                                 antialiasMode: D2D1_ANTIALIAS_MODE = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
                                 textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT;
                                 tag1: D2D1_TAG = 0;
                                 tag2: D2D1_TAG = 0): D2D1_DRAWING_STATE_DESCRIPTION; inline;
var
  tf: D2D1_MATRIX_3X2_F;

begin
  // To check if a record has a default value or not.
  if CompareMem(@tf,
                @Transform,
                SizeOf(tf)) = True then
    tf := Identity() //IdentityMatrix()
  else
    tf := transform;

  Result.transform := tf;
  Result.antialiasMode := antialiasMode;
  Result.textAntialiasMode := textAntialiasMode;
  Result.tag1 := tag1;
  Result.tag2 := tag2;
end;


//{$IF CompilerVersion > 23}  // > XE2

//
// ColorF / D2D1ColorFHelper
//
function D2D1ColorFHelper.Init(const rgb: UINT32;
                               const alpha: Single = 1.0): D2D1_COLOR_F;
const
  sc_redShift   = 16;
  sc_greenShift = 8;
  sc_blueShift  = 0;
{$WRITEABLECONST ON}
  sc_redMask = $ff shl sc_redShift;
  sc_greenMask = $ff shl sc_greenShift;
  sc_blueMask = $ff shl sc_blueShift;
{$WRITEABLECONST OFF}

begin
  with Result do
    begin
      r := ((rgb AND sc_redMask) shr sc_redShift) / 255.0;
      g := ((rgb AND sc_greenMask) shr sc_greenShift) / 255.0;
      b := ((rgb AND sc_blueMask) shr sc_blueShift) / 255.0;
      a := alpha;
    end;
end;


function D2D1ColorFHelper.Init(const knownColor: TColor;
                               const alpha: Single = 1.0): D2D1_COLOR_F;
begin
  Result := Init(knownColor,
                 alpha);
end;


function D2D1ColorFHelper.Init(const red: Single;
                               const green: Single;
                               const blue: Single;
                               const alpha: Single = 1.0): D2D1_COLOR_F;
begin
  with Result do
    begin
      r := red;
      g := green;
      b := blue;
      a := alpha;
    end;
end;


class function D2D1ColorFHelper.Equal(const left: D2D1_COLOR_F;
                                      const right: D2D1_COLOR_F): Boolean;
begin
  Result:= (left.r = right.r) and
           (left.g = right.g) and
           (left.b = right.b) and
           (left.a = right.a);
end;


class function D2D1ColorFHelper.Implicit(const val: DWORD): D2D1_COLOR_F;
const
  fval = 1/255; // 0,0039215686274509803921568627451?

begin
  with Result do
    begin
      r:= fval * Byte(val shr 16);
      g:= fval * Byte(val shr 8);
      b:= fval * Byte(val{shr 0, which is a noop});
      a:= fval * Byte(val shr 24);
    end;
end;

class function D2D1ColorFHelper.Implicit(const val: D2D1_COLOR_F): DWORD;
var
  rd: DWord;
  gr: DWord;
  bl: DWord;
  al: DWord;

begin
  if (val.r > 1.0) then
    rd:= 255
  else if (val.r < 0) then
    rd:= 0
  else
    rd:= DWord(Trunc((val.r * 255.0) + 0.5));

  if (val.g > 1.0) then
    gr:= 255
  else if (val.g < 0) then
    gr:= 0
  else
    gr:= DWORD(Trunc((val.g * 255.0) + 0.5));

  if (val.b > 1.0) then
    bl:= 255
  else if (val.b < 0) then
    bl:= 0
  else
    bl:= DWORD(Trunc((val.b * 255.0) + 0.5));

  if (val.a > 1.0) then
    al:= 255
  else if (val.a < 0) then
    al:= 0
  else
    al:= DWORD(Trunc((val.a * 255.0) + 0.5));

  Result := (al shl 24) or
            (rd shl 16) or
            (gr shl 8) or bl;
end;


//
// Matrix3x2F // D2D1Matrix3x2FHelper
//
class function Matrix3x2FHelper.Init(const m11: Single;
                                     const m12: Single;
                                     const m21: Single;
                                     const m22: Single;
                                     const dx: Single;
                                     const dy: Single): D2D1_MATRIX_3X2_F;
begin
  with Result do
    begin
      _11 := m11;
      _12 := m12;
      _21 := m21;
      _22 := m22;
      _31 := dx;
      _32 := dy;
   end;
end;


// Quasi constructors
class function Matrix3x2FHelper.Identity(): D2D1_MATRIX_3X2_F;
begin
  with Result do
    begin
      _11 := 1.0;
      _12 := 0.0;
      _21 := 0.0;
      _22 := 1.0;
      _31 := 0.0;
      _32 := 0.0;
    end;
end;


class function Matrix3x2FHelper.Translation(const size: D2D1_SIZE_F): D2D1_MATRIX_3X2_F;
begin
  with Result do
    begin
      _11 := 1.0;
      _12 := 0.0;
      _21 := 0.0;
      _22 := 1.0;
      _31 := size.width;
      _32 := size.height;
    end;
end;


class function Matrix3x2FHelper.Translation(const x: Single;
                                            const y: Single): D2D1_MATRIX_3X2_F;
begin
  Result := Translation(SizeF(x,
                              y));
end;


class function Matrix3x2FHelper.Scale(const size: D2D1_SIZE_F;
                                      const center: D2D1_POINT_2F): D2D1_MATRIX_3X2_F;
var
  ct: D2D1_POINT_2F;

begin
  // Check if a record has a default value or not.
  if CompareMem(@ct,
                @center,
                SizeOf(ct)) = True then
    ct := Point2F()
  else
    ct := center;

  with Result do
    begin
      _11 := size.width;
      _12 := 0.0;
      _21 := 0.0;
      _22 := size.height;
      _31 := ct.x - size.width * ct.x;
      _32 := ct.y - size.height * ct.y;
    end;

end;


class function Matrix3x2FHelper.Scale(const x: Single;
                                      const y: Single;
                                      const center: D2D1_POINT_2F): D2D1_MATRIX_3X2_F;
var
  cnt: D2D1_POINT_2F;

begin
  // Check if a record has a default value or not.
  if CompareMem(@cnt,
                @center,
                SizeOf(cnt)) = True then
    cnt := Point2F()
  else
    cnt := center;

  Result := D2D1_MATRIX_3X2_F.Scale(SizeF(x,
                                          y),
                                    cnt);
end;


class function Matrix3x2FHelper.Rotation(const angle: Single;
                                         const center: D2D1_POINT_2F): D2D1_MATRIX_3X2_F;
var
  cnt: D2D1_POINT_2F;
  rotation: D2D1_MATRIX_3X2_F;

begin
  // Check if a record has a default value or not.
  if CompareMem(@cnt,
                @center,
                SizeOf(cnt)) = True then
    cnt := Point2F()
  else
    cnt := center;

  D2D1MakeRotateMatrix(angle,
                       cnt,
                       rotation);
  Result := rotation;
end;


class function Matrix3x2FHelper.Skew(const angleX: Single;
                                     const angleY: Single;
                                     const center: D2D1_POINT_2F): D2D1_MATRIX_3X2_F;
var
  cnt: D2D1_POINT_2F;
  skew: D2D1_MATRIX_3X2_F;

begin
  // Check if a record has a default value or not.
  if CompareMem(@cnt,
                @center,
                SizeOf(cnt)) = True then
    cnt := Point2F()
  else
    cnt := center;

  D2D1MakeSkewMatrix(angleX,
                     angleY,
                     cnt,
                     skew);
  Result := skew;
end;


class function Matrix3x2FHelper.ReinterpretBaseType(const pMatrix: PD2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F;
begin
  Result := pMatrix^;
end;


class function Matrix3x2FHelper.ReinterpretBaseType(const pMatrix: D2D1_MATRIX_3X2_F): PD2D1_MATRIX_3X2_F;
begin
  Result := @pMatrix;
end;


function Matrix3x2FHelper.Determinant(): Single;
begin
  Result := (_11 * _21) - (_12 * _21);
end;


function Matrix3x2FHelper.IsInvertible(): Boolean;
begin
  Result := D2D1IsMatrixInvertible(Self);
end;


function Matrix3x2FHelper.Invert(): Boolean;
begin
  Result := D2D1InvertMatrix(Self);
end;


function Matrix3x2FHelper.IsIdentity(): Boolean;
begin
  Result := (_11 = 1.0) and
            (_12 = 0.0) and
            (_21 = 0.0) and
            (_22 = 1.0) and
            (_31 = 0.0) and
            (_32 = 0.0);
end;


function Matrix3x2FHelper.SetProduct(const a: D2D1_MATRIX_3X2_F;
                                     const b: D2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F;
begin
  with Result do
    begin
      _11 := (a._11 * b._11) + (a._12 * b._21);
      _12 := (a._11 * b._12) + (a._12 * b._22);
      _21 := (a._21 * b._11) + (a._22 * b._21);
      _22 := (a._21 * b._12) + (a._22 * b._22);
      _31 := (a._31 * b._11) + ((a._32 * b._21) + b._31);
      _32 := (a._31 * b._12) + ((a._32 * b._22) + b._32);
    end;
end;


function Matrix3x2FHelper.SetProduct(const matrix: D2D1_MATRIX_3X2_F): D2D1_MATRIX_3X2_F;
begin
  Result := SetProduct(Self,
                       matrix);
end;


function Matrix3x2FHelper.TransformPoint(const point: D2D1_POINT_2F): D2D1_POINT_2F;
begin
  Result.x := (point.x * _11) + ((point.y * _21) + _31);
  Result.y := (point.x * _12) + ((point.y * _22) + _32);
end;


function Matrix3x2FHelper.Equal(const size1: D2D1_SIZE_U;
                                const size2: D2D1_SIZE_U): Boolean;
begin
  Result := (size1.width = size2.width) AND (size1.height = size2.height);
end;

//{$ENDIF}

function IdentityMatrix(): D2D1_MATRIX_3X2_F;
begin
  // Result := D2D1_MATRIX_3X2_F.Identity(); // [dcc32 Error] : E2076 This form of method call only allowed for class methods or constructor
  // So, we made a hardcopy
  with Result do
    begin
      _11 := 1.0;
      _12 := 0.0;
      _21 := 0.0;
      _22 := 1.0;
      _31 := 0.0;
      _32 := 0.0;
    end;
end;



// Implement Additional functions here.


function D2D1PointF(const x: Single;
                    const y: Single) : D2D1_POINT_2F;
begin
  result.x := x;
  result.y := y;
end;


function D2D1SizeF(const width: Single;
                   const height: Single) : D2D1_SIZE_F;
begin
  result.width := width;
  result.height := height;
end;


function D2D1SizeU(const Width: UINT32;
                   const Height: UINT32) : D2D1_SIZE_U;
begin
  result.width := width;
  result.height := height;
end;


function D2D1RectF(const left: Single;
                   const top: Single;
                   const right: Single;
                   const bottom: Single): D2D1_RECT_F;
begin
  Result.left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;


procedure D2D1RectFDefault(var crRect: D2D1_RECT_F);
begin
  crRect.left   := 0.0;
  crRect.top    := 0.0;
  crRect.right  := 0.0;
  crRect.bottom := 0.0;
end;


function D2D1ArcSegment(const Point: D2D1_POINT_2F;
                        const Size: D2D1_SIZE_F;
                        RotationAngle: Single;
                        const sweepDirection: D2D1_SWEEP_DIRECTION;
                        const ArcSize: D2D1_ARC_SIZE): D2D1_ARC_SEGMENT;
begin
  Result.point := Point;
  Result.size := Size;
  Result.rotationAngle := RotationAngle;
  Result.sweepDirection := sweepDirection;
  Result.arcSize := ArcSize;
end;


function D2D1ColorF(const r: Single;
                    const g: Single;
                    const b: Single;
                    const a: Single) : D2D1_COLOR_F; overload;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;


function D2D1ColorF(color: TColor;
                    const alpha: Single = 1.0): D2D1_COLOR_F; overload;
const
  sc_redShift   = 16;
  sc_greenShift = 8;
  sc_blueShift  = 0;
{$WRITEABLECONST ON}
  sc_redMask = $ff shl sc_redShift;
  sc_greenMask = $ff shl sc_greenShift;
  sc_blueMask = $ff shl sc_blueShift;
{$WRITEABLECONST OFF}

begin
  with Result do
    begin
      r := ((color AND sc_redMask) shr sc_redShift) / 255.0;
      g := ((color AND sc_greenMask) shr sc_greenShift) / 255.0;
      b := ((color AND sc_blueMask) shr sc_blueShift) / 255.0;
      a := alpha;
    end;
end;


function D2D1BezierSegment(const a: D2D1_POINT_2F;
                           const b: D2D1_POINT_2F;
                           const c: D2D1_POINT_2F) : D2D1_BEZIER_SEGMENT;
begin
  Result.point1 := a;
  Result.point2 := b;
  Result.point3 := c;
end;


function D2D1GradientStop(const Position: Single;
                          const Color: D2D1_COLOR_F): D2D1_GRADIENT_STOP;
begin
  Result.position := Position;
  Result.color := Color;
end;


function D2D1QuadraticBezierSegment(const a: D2D1_POINT_2F;
                                    const b: D2D1_POINT_2F) : D2D1_QUADRATIC_BEZIER_SEGMENT;
begin
  Result.point1 := a;
  Result.point2 := b;
end;


function D2D1StrokeStyleProperties(StartCap: D2D1_CAP_STYLE;
                                   EndCap: D2D1_CAP_STYLE;
                                   DashCap: D2D1_CAP_STYLE;
                                   LineJoin: D2D1_LINE_JOIN;
                                   MiterLimit: Single;
                                   DashStyle: D2D1_DASH_STYLE;
                                   DashOffset: Single): D2D1_STROKE_STYLE_PROPERTIES;

begin
  Result.startCap   := StartCap;
  Result.endCap     := EndCap;
  Result.dashCap    := DashCap;
  Result.lineJoin   := LineJoin;
  Result.miterLimit := MiterLimit;
  Result.dashStyle  := DashStyle;
  Result.dashOffset := DashOffset;
end;


function D2D1Ellipse(center: D2D1_POINT_2F;
                     const rx: Single;
                     const ry: Single): D2D1_ELLIPSE;
begin
  Result.point := center;
  Result.radiusX := rx;
  Result.radiusY := ry;
end;


function D2D1RoundedRect(const Rect: D2D1_RECT_F;
                         RadiusX: Single;
                         RadiusY: Single): D2D1_ROUNDED_RECT;
begin
  Result.rect := Rect;
  Result.radiusX := RadiusX;
  Result.radiusY := RadiusY;
end;

function D2D1LinearGradientBrushProperties(const StartPoint: D2D1_POINT_2F;
                                           const EndPoint: D2D1_POINT_2F): D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
begin
  Result.startPoint := StartPoint;
  Result.endPoint := EndPoint;
end;

function D2D1RadialGradientBrushProperties(const Center: D2D1_POINT_2F;
                                           const GradientOriginOffset: D2D1_POINT_2F;
                                           RadiusX: Single;
                                           RadiusY: Single): D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES;
begin
  Result.center := Center;
  Result.gradientOriginOffset := GradientOriginOffset;
  Result.radiusX :=  RadiusX;
  Result.radiusY := RadiusY;
end;


function D2D1PixelFormat(const DxgiFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         AlphaMode: D2D1_ALPHA_MODE = D2D1_ALPHA_MODE_UNKNOWN): D2D1_PIXEL_FORMAT;
begin
  Result.format := DxgiFormat;
  Result.alphaMode := AlphaMode;
end;


function D2D1RenderTargetProperties(_Type: D2D1_RENDER_TARGET_TYPE = D2D1_RENDER_TARGET_TYPE_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES; overload;
begin
  Result := D2D1RenderTargetProperties(_Type, D2D1PixelFormat());
end;


function D2D1RenderTargetProperties(_Type: D2D1_RENDER_TARGET_TYPE;
                                    const _PixelFormat: D2D1_PIXEL_FORMAT;
                                    DpiX: Single = 0;
                                    DpiY: Single = 0;
                                    Usage: D2D1_RENDER_TARGET_USAGE = D2D1_RENDER_TARGET_USAGE_NONE;
                                    MinLevel: D2D1_FEATURE_LEVEL = D2D1_FEATURE_LEVEL_DEFAULT): D2D1_RENDER_TARGET_PROPERTIES;
begin
  Result._type := _Type;
  Result._pixelFormat := _PixelFormat;
  Result.dpiX := DpiX;
  Result.dpiY := DpiY;
  Result.usage := Usage;
  Result.minLevel := MinLevel;
end;



function D2D1HwndRenderTargetProperties(_hwnd: HWND): D2D1_HWND_RENDER_TARGET_PROPERTIES; overload;
begin
  Result := D2D1HwndRenderTargetProperties(_hwnd, D2D1SizeU(0,0));
end;


function D2D1HwndRenderTargetProperties(_Hwnd: HWND;
                                        _PixelSize: D2D1_SIZE_U;
                                        PresentOptions: D2D1_PRESENT_OPTIONS = D2D1_PRESENT_OPTIONS_NONE): D2D1_HWND_RENDER_TARGET_PROPERTIES; overload;
begin
  Result._hwnd := _Hwnd;
  Result._pixelSize := _PixelSize;
  Result.presentOptions := PresentOptions;
end;


function D2D1BitmapProperties(): D2D1_BITMAP_PROPERTIES;
begin
  Result := D2D1BitmapProperties(D2D1PixelFormat());
end;


function D2D1BitmapProperties(_PixleFormat: D2D1_PIXEL_FORMAT;
                              DpiX: Single = 96;
                              DpiY: Single = 96): D2D1_BITMAP_PROPERTIES;
begin
  Result._pixelFormat := _PixleFormat;
  Result.dpiX := DpiX;
  Result.dpiY := DpiY;
end;


end.
