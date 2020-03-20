// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DXGIType.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Microsoft DirectX Graphics Infrastructure API
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: dxgitype.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
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
unit MfPack.DXGIType;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "dxgitype.h"'}
  {$HPPEMIT ''}

interface

uses
  {Vcl}
  Vcl.Graphics,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DXGICommon,
  MfPack.DXGIFormat,
  MfPack.WinError;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'MfPack.inc'}

const

  _FACDXGI                            = $87A;

  // DXGI error messages have moved to winerror.h

  DXGI_CPU_ACCESS_NONE                = 0;
  {$EXTERNALSYM DXGI_CPU_ACCESS_NONE}
  DXGI_CPU_ACCESS_DYNAMIC             = 1;
  {$EXTERNALSYM DXGI_CPU_ACCESS_DYNAMIC}
  DXGI_CPU_ACCESS_READ_WRITE          = 2;
  {$EXTERNALSYM DXGI_CPU_ACCESS_READ_WRITE}
  DXGI_CPU_ACCESS_SCRATCH             = 3;
  {$EXTERNALSYM DXGI_CPU_ACCESS_SCRATCH}
  DXGI_CPU_ACCESS_FIELD               = 15;

  // Internal functions
  function MAKE_DXGI_HRESULT(code: DWORD): HResult;

  function MAKE_DXGI_STATUS(code: DWORD): HResult;


type

  PDXGI_RGB = ^DXGI_RGB;
  DXGI_RGB = record
    Red: Single;
    Green: Single;
    Blue: Single;
  end;
  {$EXTERNALSYM DXGI_RGB}

  PD3DCOLORVALUE = ^_D3DCOLORVALUE;
  _D3DCOLORVALUE = record
    r: Single;
    g: Single;
    b: Single;
    a: Single;

    //==========================================================================
    // Helpers
    //
    // Record helpers were introduced in Delphi XE3.
    // To keep the code usable for earlier versions, we added the helper methods here and
    // the record helper in D2D1Helper.pas for later versions

{$IF CompilerVersion < 24}  // < XE3
    class function Init(const rgb: UINT32;
                        const alpha: FLOAT = 1.0): _D3DCOLORVALUE; overload; static;

    class function Init(const knownColor: TColor;
                        const alpha: FLOAT = 1.0): _D3DCOLORVALUE; overload; static;

    class function Init(const red: FLOAT;
                        const green: FLOAT;
                        const blue: FLOAT;
                        const alpha: FLOAT = 1.0): _D3DCOLORVALUE; overload; static;

    class operator Implicit(const val: DWORD): _D3DCOLORVALUE; inline;

    class operator Implicit(const val: _D3DCOLORVALUE): DWORD; inline;

    class operator Equal(const left: _D3DCOLORVALUE;
                         const right: _D3DCOLORVALUE): Boolean; inline;
{$ENDIF}
  end;
  D3DCOLORVALUE = _D3DCOLORVALUE;
  {$EXTERNALSYM _D3DCOLORVALUE}

  PDXGI_RGBA = ^DXGI_RGBA;
  DXGI_RGBA = D3DCOLORVALUE;
  {$EXTERNALSYM DXGI_RGBA}

  PDXGI_GAMMA_CONTROL = ^DXGI_GAMMA_CONTROL;
  DXGI_GAMMA_CONTROL = record
    Scale: DXGI_RGB;
    Offset: DXGI_RGB;
    GammaCurve: array[0..1024] of DXGI_RGB;
  end;
  {$EXTERNALSYM DXGI_GAMMA_CONTROL}

  PDXGI_GAMMA_CONTROL_CAPABILITIES = ^DXGI_GAMMA_CONTROL_CAPABILITIES;
  DXGI_GAMMA_CONTROL_CAPABILITIES = record
    ScaleAndOffsetSupported: BOOL;
    MaxConvertedValue: Single;
    MinConvertedValue: Single;
    NumGammaControlPoints: UINT;
    ControlPointPositions: array[0..1024] of Single;
  end;
  {$EXTERNALSYM DXGI_GAMMA_CONTROL_CAPABILITIES}

  PDXGI_MODE_SCANLINE_ORDER = ^DXGI_MODE_SCANLINE_ORDER;
  DXGI_MODE_SCANLINE_ORDER                     = (
    DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED       = 0,
    DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE       = 1,
    DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST = 2,
    DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST = 3);
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER}

  PDXGI_MODE_SCALING = ^DXGI_MODE_SCALING;
  DXGI_MODE_SCALING               = (
    DXGI_MODE_SCALING_UNSPECIFIED = 0,
    DXGI_MODE_SCALING_CENTERED    = 1,
    DXGI_MODE_SCALING_STRETCHED   = 2);
  {$EXTERNALSYM DXGI_MODE_SCALING}

  PDXGI_MODE_ROTATION = ^DXGI_MODE_ROTATION;
  DXGI_MODE_ROTATION               = (
    DXGI_MODE_ROTATION_UNSPECIFIED = 0,
    DXGI_MODE_ROTATION_IDENTITY    = 1,
    DXGI_MODE_ROTATION_ROTATE90    = 2,
    DXGI_MODE_ROTATION_ROTATE180   = 3,
    DXGI_MODE_ROTATION_ROTATE270   = 4);
  {$EXTERNALSYM DXGI_MODE_ROTATION}

  PDXGI_MODE_DESC = ^DXGI_MODE_DESC;
  DXGI_MODE_DESC = record
    Width: UINT;
    Height: UINT;
    RefreshRate: DXGI_RATIONAL;
    Format: DXGI_FORMAT;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
  end;
  {$EXTERNALSYM DXGI_MODE_DESC}

  PDXGI_JPEG_DC_HUFFMAN_TABLE = ^DXGI_JPEG_DC_HUFFMAN_TABLE;
  DXGI_JPEG_DC_HUFFMAN_TABLE = record
    CodeCounts: array[0..11] of Byte;
    CodeValues: array[0..11] of Byte;
  end;
  {$EXTERNALSYM DXGI_JPEG_DC_HUFFMAN_TABLE}

  PDXGI_JPEG_AC_HUFFMAN_TABLE = ^DXGI_JPEG_AC_HUFFMAN_TABLE;
  DXGI_JPEG_AC_HUFFMAN_TABLE = record
    CodeCounts: array[0..15] of Byte;
    CodeValues: array[0..161] of Byte;
  end;
  {$EXTERNALSYM DXGI_JPEG_AC_HUFFMAN_TABLE}

  PDXGI_JPEG_QUANTIZATION_TABLE = ^DXGI_JPEG_QUANTIZATION_TABLE;
  DXGI_JPEG_QUANTIZATION_TABLE = record
    Elements: array[0..63] of Byte;
  end;
  {$EXTERNALSYM DXGI_JPEG_QUANTIZATION_TABLE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation


function MAKE_DXGI_HRESULT(code: DWORD): HResult;
begin
  Result := MAKE_HRESULT (1,
                          _FACDXGI,
                          code);
end;


function MAKE_DXGI_STATUS(code: DWORD): HResult;
begin
  Result := MAKE_HRESULT (0,
                          _FACDXGI,
                          code);
end;


{$IF CompilerVersion < 24}  // < XE3

//
// D3DCOLORVALUE Helpers
//
class function _D3DCOLORVALUE.Init(const rgb: UINT32;
                                   const alpha: FLOAT = 1.0): _D3DCOLORVALUE;
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


class function _D3DCOLORVALUE.Init(const knownColor: TColor;
                                   const alpha: FLOAT = 1.0): _D3DCOLORVALUE;
begin
  Result := Init(knownColor,
                 alpha);
end;


class function _D3DCOLORVALUE.Init(const red: FLOAT;
                                   const green: FLOAT;
                                   const blue: FLOAT;
                                   const alpha: FLOAT = 1.0): _D3DCOLORVALUE;
begin
  Result.r := red;
  Result.g := green;
  Result.b := blue;
  Result.a := alpha;
end;


class operator _D3DCOLORVALUE.Equal(const left: _D3DCOLORVALUE;
                                    const right: _D3DCOLORVALUE): Boolean;
begin
  Result:= (left.r = right.r) and
           (left.g = right.g) and
           (left.b = right.b) and
           (left.a = right.a);
end;


class operator _D3DCOLORVALUE.Implicit(const val: DWORD): _D3DCOLORVALUE;
const
  fval = 1/255; // 0,0039215686274509803921568627451‬

begin
  with Result do
    begin
      r:= fval * Byte(val shr 16);
      g:= fval * Byte(val shr  8);
      b:= fval * Byte(val{shr 0, which is a noop});
      a:= fval * Byte(val shr 24);
    end;
end;

class operator _D3DCOLORVALUE.Implicit(const val: _D3DCOLORVALUE): DWORD;
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
            (gr shl 8) or
            bl;
end;
{$ENDIF}

  //Implement Additional Prototypes here.

end.
