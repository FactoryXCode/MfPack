// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DCommon.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.1.6
//
// Description: Public API definitions shared by DWrite, D2D, and DImage.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//

//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows Vista or later.
//
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: dcommon.h
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
unit WinApi.DirectX.DCommon;

  {$HPPEMIT '#include "dcommon.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  {System}
  System.Types,
  System.SysUtils,
  {WinApi.DirectX}
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

// Enums =======================================================================

type
  // The measuring method used for text layout.
  PDWRITE_MEASURING_MODE = ^DWRITE_MEASURING_MODE;
  DWRITE_MEASURING_MODE = DWord;
  {$EXTERNALSYM DWRITE_MEASURING_MODE}
const
  // Text is measured using glyph ideal metrics whose values are independent to the current display resolution.
  DWRITE_MEASURING_MODE_NATURAL     = DWRITE_MEASURING_MODE(0);
  {$EXTERNALSYM DWRITE_MEASURING_MODE_NATURAL}
  // Text is measured using glyph display compatible metrics whose values tuned for the current display resolution.
  DWRITE_MEASURING_MODE_GDI_CLASSIC = DWRITE_MEASURING_MODE(1);
  {$EXTERNALSYM DWRITE_MEASURING_MODE_GDI_CLASSIC}
  // Text is measured using the same glyph display metrics as text measured by GDI using a font
  // created with CLEARTYPE_NATURAL_QUALITY.
  DWRITE_MEASURING_MODE_GDI_NATURAL = DWRITE_MEASURING_MODE(3);
  {$EXTERNALSYM DWRITE_MEASURING_MODE_GDI_NATURAL}


// #if NTDDI_VERSION >= NTDDI_WIN10_RS1

type
  // Fonts may contain multiple drawable data formats for glyphs. These flags specify which formats
  // are supported in the font, either at a font-wide level or per glyph, and the app may use them
  // to tell DWrite which formats to return when splitting a color glyph run.
  PDWRITE_GLYPH_IMAGE_FORMATS = ^DWRITE_GLYPH_IMAGE_FORMATS;
  DWRITE_GLYPH_IMAGE_FORMATS = DWord;
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS}
const
  // Indicates no data is available for this glyph.
  DWRITE_GLYPH_IMAGE_FORMATS_NONE                   = DWRITE_GLYPH_IMAGE_FORMATS($00000000);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_NONE}
  // The glyph has TrueType outlines.
  DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE               = DWRITE_GLYPH_IMAGE_FORMATS($00000001);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_TRUETYPE}
  // The glyph has CFF outlines.
  DWRITE_GLYPH_IMAGE_FORMATS_CFF                    = DWRITE_GLYPH_IMAGE_FORMATS($00000002);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_CFF}
  // The glyph has multilayered COLR data.
  DWRITE_GLYPH_IMAGE_FORMATS_COLR                   = DWRITE_GLYPH_IMAGE_FORMATS($00000004);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_COLR}
  // The glyph has SVG outlines as standard XML.
  // <remarks>
  // Fonts may store the content gzip'd rather than plain text);
  // indicated by the first two bytes as gzip header {$1F $8B}.
  // </remarks>
  DWRITE_GLYPH_IMAGE_FORMATS_SVG                    = DWRITE_GLYPH_IMAGE_FORMATS($00000008);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_SVG}
  // The glyph has PNG image data); with standard PNG IHDR.
  DWRITE_GLYPH_IMAGE_FORMATS_PNG                    = DWRITE_GLYPH_IMAGE_FORMATS($00000010);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_PNG}
  // The glyph has JPEG image data); with standard JIFF SOI header.
  DWRITE_GLYPH_IMAGE_FORMATS_JPEG                   = DWRITE_GLYPH_IMAGE_FORMATS($00000020);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_JPEG}
  // The glyph has TIFF image data.
  DWRITE_GLYPH_IMAGE_FORMATS_TIFF                   = DWRITE_GLYPH_IMAGE_FORMATS($00000040);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_TIFF}
  // The glyph has raw 32-bit premultiplied BGRA data.
  DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8 = DWRITE_GLYPH_IMAGE_FORMATS($00000080);
  {$EXTERNALSYM DWRITE_GLYPH_IMAGE_FORMATS_PREMULTIPLIED_B8G8R8A8}

// #endif  NTDDI_WIN10_RS1

type
  // Qualifies how alpha is to be treated in a bitmap or render target containing
  // alpha.
  PD2D1_ALPHA_MODE = ^D2D1_ALPHA_MODE;
  D2D1_ALPHA_MODE = DWord;
  {$EXTERNALSYM D2D1_ALPHA_MODE}
const
  // Alpha mode should be determined implicitly. Some target surfaces do not supply
  // or imply this information in which case alpha must be specified.
  D2D1_ALPHA_MODE_UNKNOWN       = D2D1_ALPHA_MODE(0);
  // Treat the alpha as premultipled.
  D2D1_ALPHA_MODE_PREMULTIPLIED = D2D1_ALPHA_MODE(1);
  // Opacity is in the 'A' component only.
  D2D1_ALPHA_MODE_STRAIGHT      = D2D1_ALPHA_MODE(2);
  // Ignore any alpha channel information.
  D2D1_ALPHA_MODE_IGNORE        = D2D1_ALPHA_MODE(3);
  //D2D1_ALPHA_MODE_FORCE_DWORD   = FORCEDWORD;

// =============================================================================

type
  // Description of a pixel format.
  PD2D1_PIXEL_FORMAT = ^D2D1_PIXEL_FORMAT;
  D2D1_PIXEL_FORMAT = record
    format: DXGI_FORMAT;
    alphaMode: D2D1_ALPHA_MODE;
  end;
  {$EXTERNALSYM D2D1_PIXEL_FORMAT}

  // Represents an x-coordinate and y-coordinate pair in two-dimensional space.
  PD2D_POINT_2U = ^D2D_POINT_2U;
  D2D_POINT_2U = record
    x: UINT32;
    y: UINT32;
  end;
  {$EXTERNALSYM D2D_POINT_2U}

  // Represents an x-coordinate and y-coordinate pair in two-dimensional space.
  PD2D_POINT_2F = ^D2D_POINT_2F;
  D2D_POINT_2F = record
    x: FLOAT;
    y: FLOAT;

    // Delphi Note: Translations to/from Delphi TPoint
    class operator Implicit(AValue: TPoint): D2D_POINT_2F;
    class operator Explicit(AValue: D2D_POINT_2F): TPoint;
  end;
  {$EXTERNALSYM D2D_POINT_2F}


  PD2D_POINT_2L = ^D2D_POINT_2L;
  D2D_POINT_2L = TPoint;
  {$EXTERNALSYM D2D_POINT_2L}


  // A vector of 2 FLOAT values (x, y).
  PD2D_VECTOR_2F = ^D2D_VECTOR_2F;
  D2D_VECTOR_2F = record
    x: FLOAT;
    y: FLOAT;
  end;
  {$EXTERNALSYM D2D_VECTOR_2F}


  // A vector of 3 FLOAT values (x, y, z).
  PD2D_VECTOR_3F = ^D2D_VECTOR_3F;
  D2D_VECTOR_3F = record
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
  end;
  {$EXTERNALSYM D2D_VECTOR_3F}


  // A vector of 4 FLOAT values (x, y, z, w).
  PD2D_VECTOR_4F = ^D2D_VECTOR_4F;
  D2D_VECTOR_4F = record
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    w: FLOAT;
  end;
  {$EXTERNALSYM D2D_VECTOR_4F}


  // Represents a rectangle defined by the coordinates of the upper-left corner
  // (left, top) and the coordinates of the lower-right corner (right, bottom).
  PD2D_RECT_F = ^D2D_RECT_F;
  D2D_RECT_F = record
    left: FLOAT;
    top: FLOAT;
    right: FLOAT;
    bottom: FLOAT;
    // Delphi Note:  TRect conversion methods
    class operator Implicit(aValue: TRect): D2D_RECT_F;
    class operator Explicit(aValue: D2D_RECT_F): TRect;
  end;
  {$EXTERNALSYM D2D_RECT_F}


  // Represents a rectangle defined by the coordinates of the upper-left corner
  // (left, top) and the coordinates of the lower-right corner (right, bottom).
  PD2D_RECT_U = ^D2D_RECT_U;
  D2D_RECT_U = record
    left: UINT32;
    top: UINT32;
    right: UINT32;
    bottom: UINT32;
  end;
  {$EXTERNALSYM D2D_RECT_U}


  PD2D_RECT_L = ^D2D_RECT_L;
  D2D_RECT_L = TRect;
  {$EXTERNALSYM D2D_RECT_L}


  // Stores an ordered pair of floats, typically the width and height of a rectangle.
  PD2D_SIZE_F = ^D2D_SIZE_F;
  D2D_SIZE_F = record
    width: FLOAT;
    height: FLOAT;
  end;
  {$EXTERNALSYM D2D_SIZE_F}


  // Stores an ordered pair of integers, typically the width and height of a
  // rectangle.
  PD2D_SIZE_U = ^D2D_SIZE_U;
  D2D_SIZE_U = record
    width: UINT32;
    height: UINT32;
  end;
  {$EXTERNALSYM D2D_SIZE_U}


  // Represents a 3-by-2 matrix.

  PD2D_MATRIX_3X2_F = ^D2D_MATRIX_3X2_F;
  D2D_MATRIX_3X2_F = record
    // Horizontal scaling / cosine of rotation
    _11: FLOAT;
    // Vertical shear / sine of rotation
    _12: FLOAT;

    // Horizontal shear / negative sine of rotation
    _21: FLOAT;
    // Vertical scaling / cosine of rotation
    _22: FLOAT;

    // Horizontal shift (always orthogonal regardless of rotation)
    _31: FLOAT;
    // Vertical shift (always orthogonal regardless of rotation)
    _32: FLOAT;


    // Helpers
    // =========================================================================
    class function Init(const m11: FLOAT;
                        const m12: FLOAT;
                        const m21: FLOAT;
                        const m22: FLOAT;
                        const dx: FLOAT;
                        const dy: FLOAT): D2D_MATRIX_3X2_F; static;

    class operator Multiply(const left: D2D_MATRIX_3X2_F;
                            const right: D2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F;

    class function SetProduct(const mrx1: D2D_MATRIX_3X2_F;
                              const mrx2: D2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F; static;

{$IF CompilerVersion < 23}  // < XE3

    // Record helpers were introduced in Delphi XE3.
    // To keep the code usable for earlier Delphi versions and preventing circular reference errors,
    // we added the helper methods here from MfPack.D2D1Helper.pas

    class operator Equal(const matrix1: D2D_MATRIX_3X2_F;
                         const matrix2: D2D_MATRIX_3X2_F): Boolean;

    class function Identity(): D2D_MATRIX_3X2_F; static;

    class function Translation(const size: D2D_SIZE_F): D2D_MATRIX_3X2_F; static;

    class function ReinterpretBaseType(const pMatrix: PD2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F; overload; static;
    class function ReinterpretBaseType(const pMatrix: D2D_MATRIX_3X2_F): PD2D_MATRIX_3X2_F; overload; static;

    function Determinant(): FLOAT;

    function IsIdentity(): Boolean;

    function TransformPoint(point: D2D_POINT_2F): D2D_POINT_2F;

    function Equal(const size1: D2D_SIZE_U;
                   const size2: D2D_SIZE_U): Boolean;
{$ENDIF}
  end;
  {$EXTERNALSYM D2D_MATRIX_3X2_F}


// When using Delphi version < XE3 the D2D_MATRIX_3X2_F record should be used like this:
//
// Delphi < XE3
//
//  {DUMMYSTRUCTNAME}
//  D2D_MATRIX_3X2_F_Struct1 = record
//    m11: FLOAT;
//    m12: FLOAT;
//    m21: FLOAT;
//    m22: FLOAT;
//    dx:  FLOAT;
//    dy:  FLOAT;
//  end;

//  {DUMMYSTRUCTNAME2}
//  D2D_MATRIX_3X2_F_Struct2 = record
//    _11: FLOAT;
//    _12: FLOAT;
//    _21: FLOAT;
//    _22: FLOAT;
//    _31: FLOAT;
//    _32: FLOAT;
//  end;
//
//  PD2D_MATRIX_3X2_F = ^D2D_MATRIX_3X2_F;
//  D2D_MATRIX_3X2_F = record
//  case Integer of
//    0: (struct1: D2D_MATRIX_3X2_F_Struct1);
//    1: (struct2: D2D_MATRIX_3X2_F_Struct2);
//    2: (m: array [0..2, 0..1] of FLOAT);
//  end;
//  {$EXTERNALSYM D2D_MATRIX_3X2_F}
//


  // D2D_MATRIX_4X3_F
  D2D_MATRIX_4X3_F_Struct = record
  public
    _11: FLOAT;
    _12: FLOAT;
    _13: FLOAT;

    _21: FLOAT;
    _22: FLOAT;
    _23: FLOAT;

    _31: FLOAT;
    _32: FLOAT;
    _33: FLOAT;

    _41: FLOAT;
    _42: FLOAT;
    _43: FLOAT;
  end;

  // Represents a 4-by-3 matrix.
  PD2D_MATRIX_4X3_F = ^D2D_MATRIX_4X3_F;
  D2D_MATRIX_4X3_F = record
    case Integer of
      0: ({DUMMYSTRUCTNAME} struct: D2D_MATRIX_4X3_F_Struct);
      1: (m: array [0..3, 0..2] of FLOAT);
  end;
  {$EXTERNALSYM D2D_MATRIX_4X3_F}


  // PD2D_MATRIX_4X4_F
  D2D_MATRIX_4X4_F_Struct = record
  public
    _11: FLOAT;
    _12: FLOAT;
    _13: FLOAT;
    _14: FLOAT;
    _21: FLOAT;
    _22: FLOAT;
    _23: FLOAT;
    _24: FLOAT;
    _31: FLOAT;
    _32: FLOAT;
    _33: FLOAT;
    _34: FLOAT;
    _41: FLOAT;
    _42: FLOAT;
    _43: FLOAT;
    _44: FLOAT;
  end;

  // Represents a 4-by-4 matrix.
  PD2D_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D_MATRIX_4X4_F = record
    case Integer of
      0: ({DUMMYSTRUCTNAME} struct: D2D_MATRIX_4X4_F_Struct);
      1: (m: array [0..3, 0..3] of FLOAT);
  end;
  {$EXTERNALSYM D2D_MATRIX_4X4_F}


  // D2D_MATRIX_5X4_F
  D2D_MATRIX_5X4_F_Struct = record
  public
    _11: FLOAT;
    _12: FLOAT;
    _13: FLOAT;
    _14: FLOAT;
    _21: FLOAT;
    _22: FLOAT;
    _23: FLOAT;
    _24: FLOAT;
    _31: FLOAT;
    _32: FLOAT;
    _33: FLOAT;
    _34: FLOAT;
    _41: FLOAT;
    _42: FLOAT;
    _43: FLOAT;
    _44: FLOAT;
    _51: FLOAT;
    _52: FLOAT;
    _53: FLOAT;
    _54: FLOAT;
  end;

  // Represents a 5-by-4 matrix.
  PD2D_MATRIX_5X4_F = ^D2D_MATRIX_5X4_F;
  D2D_MATRIX_5X4_F = record
  case Integer of
    0: ({DUMMYSTRUCTNAME} struct: D2D_MATRIX_5X4_F_Struct);
    1: (m: array [0..4, 0..3] of FLOAT);
  end;
  {$EXTERNALSYM D2D_MATRIX_5X4_F}



  PD2D1_POINT_2F = ^D2D1_POINT_2F;
  D2D1_POINT_2F = D2D_POINT_2F;
  {$EXTERNALSYM D2D1_POINT_2F}

  PD2D1_POINT_2U = ^D2D1_POINT_2U;
  PD2d1Point2u = ^D2D1_POINT_2U;
  D2D1_POINT_2U = D2D_POINT_2U;
  {$EXTERNALSYM D2D1_POINT_2U}

  PD2D1_POINT_2L = ^D2D1_POINT_2L;
  D2D1_POINT_2L = D2D_POINT_2L;
  {$EXTERNALSYM D2D1_POINT_2L}

  PD2D1_RECT_F = ^D2D1_RECT_F;
  D2D1_RECT_F = D2D_RECT_F;
  {$EXTERNALSYM D2D1_RECT_F}

  PD2D1_RECT_U = ^D2D1_RECT_U;
  D2D1_RECT_U = D2D_RECT_U;
  {$EXTERNALSYM D2D1_RECT_U}

  PD2D1_RECT_L = ^D2D1_RECT_L;
  D2D1_RECT_L = D2D_RECT_L;

  PD2D1_SIZE_F = ^D2D1_SIZE_F;
  D2D1_SIZE_F = D2D_SIZE_F;
  {$EXTERNALSYM D2D1_SIZE_F}

  PD2D1_SIZE_U = ^D2D1_SIZE_U;
  D2D1_SIZE_U = D2D_SIZE_U;
  {$EXTERNALSYM D2D1_SIZE_U}

  PD2D1_MATRIX_3X2_F = ^D2D1_MATRIX_3X2_F;
  D2D1_MATRIX_3X2_F = D2D_MATRIX_3X2_F;
  {$EXTERNALSYM D2D1_MATRIX_3X2_F}


// HELPERS /////////////////////////////////////////////////////////////////////

  // For those see: WinApi.D2D1.pas and WinApi.D2D1Helper.pas



  // Additional Prototypes for ALL interfaces

  function D2SizeF(width: FLOAT;
                   height: FLOAT): D2D_SIZE_F;
  {$EXTERNALSYM D2SizeF}

  function D2SizeU(width: UINT32;
                   height: UINT32): D2D_SIZE_U;
  {$EXTERNALSYM D2SizeU}

  // End of Additional Prototypes

implementation


function D2SizeF(width: FLOAT;
                 height: FLOAT): D2D_SIZE_F;
begin
  Result.width := width;
  Result.height := height;
end;


function D2SizeU(width: UINT32;
                 height: UINT32): D2D_SIZE_U;
begin
  Result.width := width;
  Result.height := height;
end;


// D2D_POINT_2F
class operator D2D_POINT_2F.Implicit(aValue: TPoint): D2D_POINT_2F;
begin
  Result.x := aValue.X;
  Result.y := aValue.Y;
end;

class operator D2D_POINT_2F.Explicit(aValue: D2D_POINT_2F): TPoint;
begin
  Result.x := Trunc(aValue.X);
  Result.y := Trunc(aValue.Y);
end;


// D2D_RECT_F
class operator D2D_RECT_F.Implicit(aValue: TRect): D2D_RECT_F;
begin
  Result.top := aValue.Top;
  Result.left := aValue.Left;
  Result.bottom := aValue.Bottom;
  Result.right := aValue.Right;
end;

class operator D2D_RECT_F.Explicit(aValue: D2D_RECT_F): TRect;
begin
  Result.top := Trunc(aValue.Top);
  Result.left := Trunc(aValue.Left);
  Result.bottom := Trunc(aValue.Bottom);
  Result.right := Trunc(aValue.Right);
end;


// D2D_MATRIX_3X2_F
class function D2D_MATRIX_3X2_F.Init(const m11: FLOAT;
                                     const m12: FLOAT;
                                     const m21: FLOAT;
                                     const m22: FLOAT;
                                     const dx: FLOAT;
                                     const dy: FLOAT): D2D1_MATRIX_3X2_F;
begin
  Result._11 := m11;
  Result._12 := m12;
  Result._21 := m21;
  Result._22 := m22;
  Result._31 := dx;
  Result._32 := dy;
end;



class operator D2D_MATRIX_3X2_F.Multiply(const left: D2D_MATRIX_3X2_F;
                                         const right: D2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F;
begin
  Result := SetProduct(left,
                       right);

end;



class function D2D_MATRIX_3X2_F.SetProduct(const mrx1: D2D_MATRIX_3X2_F;
                                           const mrx2: D2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F;
begin
  Result._11 := (mrx1._11 * mrx2._11) + (mrx1._12 * mrx2._21);
  Result._12 := (mrx1._11 * mrx2._12) + (mrx1._12 * mrx2._22);
  Result._21 := (mrx1._21 * mrx2._11) + (mrx1._22 * mrx2._21);
  Result._22 := (mrx1._21 * mrx2._12) + (mrx1._22 * mrx2._22);
  Result._31 := (mrx1._31 * mrx2._11) + ((mrx1._32 * mrx2._21) + mrx2._31);
  Result._32 := (mrx1._31 * mrx2._12) + ((mrx1._32 * mrx2._22) + mrx2._32);
end;

{$IF CompilerVersion < 23}  // < XE3

class operator D2D_MATRIX_3X2_F.Equal(const matrix1: D2D_MATRIX_3X2_F;
                                      const matrix2: D2D_MATRIX_3X2_F): Boolean;
begin

  Result := (matrix1._11 = matrix2._11) AND
            (matrix1._12 = matrix2._12) AND
            (matrix1._21 = matrix2._21) AND
            (matrix1._22 = matrix2._22) AND
            (matrix1._31 = matrix2._31) AND
            (matrix1._32 = matrix2._32);
end;


class function D2D_MATRIX_3X2_F.Identity(): D2D_MATRIX_3X2_F;
begin
  Result._11 := 1.0;
  Result._12 := 0.0;
  Result._21 := 0.0;
  Result._22 := 1.0;
  Result._31 := 0.0;
  Result._32 := 0.0;
end;


class function D2D_MATRIX_3X2_F.Translation(const size: D2D1_SIZE_F): D2D_MATRIX_3X2_F;
begin
  Result._11 := 1.0;
  Result._12 := 0.0;
  Result._21 := 0.0;
  Result._22 := 1.0;
  Result._31 := size.width;
  Result._32 := size.height;
end;


class function D2D_MATRIX_3X2_F.ReinterpretBaseType(const pMatrix: PD2D_MATRIX_3X2_F): D2D_MATRIX_3X2_F;
begin
  Result := pMatrix^;
end;


class function D2D_MATRIX_3X2_F.ReinterpretBaseType(const pMatrix: D2D_MATRIX_3X2_F): PD2D_MATRIX_3X2_F;
begin
  Result := @pMatrix;
end;


function D2D_MATRIX_3X2_F.Determinant(): FLOAT;
begin
  Result := (_11 * _21) - (_12 * _21);
end;


function D2D_MATRIX_3X2_F.IsIdentity(): Boolean;
begin
  Result := (_11 = 1.0) and
            (_12 = 0.0) and
            (_21 = 0.0) and
            (_22 = 1.0) and
            (_31 = 0.0) and
            (_32 = 0.0);
end;


function D2D_MATRIX_3X2_F.TransformPoint(point: D2D1_POINT_2F): D2D_POINT_2F;
begin
  Result.x := (point.x * _11) + ((point.y * _21) + _31);
  Result.y := (point.x * _12) + ((point.y * _22) + _32);
end;


function D2D_MATRIX_3X2_F.Equal(const size1: D2D1_SIZE_U;
                                const size2: D2D1_SIZE_U): Boolean;
begin
  Result := (size1.width = size2.width) AND (size1.height = size2.height);
end;

{$ENDIF} // end

  // Implement Additional functions here.

end.
