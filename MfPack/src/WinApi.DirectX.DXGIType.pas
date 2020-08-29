// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGIType.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
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
// Source: dxgitype.h
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
unit WinApi.DirectX.DXGIType;

  {$HPPEMIT '#include "dxgitype.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {Vcl}
  Vcl.Graphics,
  {DirectX}
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}

const

  _FACDXGI                            = $87A;

  // DXGI error messages have moved to winerror.h (WinApi.WinError.pas)

  DXGI_CPU_ACCESS_NONE                = UINT(0);
  {$EXTERNALSYM DXGI_CPU_ACCESS_NONE}
  DXGI_CPU_ACCESS_DYNAMIC             = UINT(1);
  {$EXTERNALSYM DXGI_CPU_ACCESS_DYNAMIC}
  DXGI_CPU_ACCESS_READ_WRITE          = UINT(2);
  {$EXTERNALSYM DXGI_CPU_ACCESS_READ_WRITE}
  DXGI_CPU_ACCESS_SCRATCH             = UINT(3);
  {$EXTERNALSYM DXGI_CPU_ACCESS_SCRATCH}
  DXGI_CPU_ACCESS_FIELD               = UINT(15);
  {$EXTERNALSYM DXGI_CPU_ACCESS_FIELD}

type
  DXGI_USAGE = UINT;
  {$EXTERNALSYM DXGI_USAGE}
const
  DXGI_USAGE_SHADER_INPUT             = DXGI_USAGE(1 shl (0 + 4)); // Use the surface or resource as an input to a shader.
  {$EXTERNALSYM DXGI_USAGE_SHADER_INPUT}
  DXGI_USAGE_RENDER_TARGET_OUTPUT     = DXGI_USAGE(1 shl (1 + 4)); // Use the surface or resource as an output render target.
  {$EXTERNALSYM DXGI_USAGE_RENDER_TARGET_OUTPUT}
  DXGI_USAGE_BACK_BUFFER              = DXGI_USAGE(1 shl (2 + 4)); // The surface or resource is used as a back buffer.
  {$EXTERNALSYM DXGI_USAGE_BACK_BUFFER}                            // You don’t need to pass DXGI_USAGE_BACK_BUFFER when you create a swap chain.
                                                                   // But you can determine whether a resource belongs to a swap chain when
                                                                   // you call IDXGIResource.GetUsage and get DXGI_USAGE_BACK_BUFFER.

  DXGI_USAGE_SHARED                   = DXGI_USAGE(1 shl (3 + 4)); // Share the surface or resource.
  {$EXTERNALSYM DXGI_USAGE_SHARED}
  DXGI_USAGE_READ_ONLY                = DXGI_USAGE(1 shl (4 + 4)); // Use the surface or resource for reading only.
  {$EXTERNALSYM DXGI_USAGE_READ_ONLY}
  DXGI_USAGE_DISCARD_ON_PRESENT       = DXGI_USAGE(1 shl (5 + 4)); // This flag is for internal use only.
  {$EXTERNALSYM DXGI_USAGE_DISCARD_ON_PRESENT}
  DXGI_USAGE_UNORDERED_ACCESS         = DXGI_USAGE(1 shl (6 + 4)); // Use the surface or resource for unordered access.
  {$EXTERNALSYM DXGI_USAGE_UNORDERED_ACCESS}


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

    class operator Implicit(const val: DWORD): _D3DCOLORVALUE; overload; inline;

    class operator Implicit(const val: D3DCOLORVALUE): DWORD; overload; inline;

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

type
  PDXGI_MODE_SCANLINE_ORDER = ^DXGI_MODE_SCANLINE_ORDER;
  DXGI_MODE_SCANLINE_ORDER = DWord;
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER}
const
  DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED       = DXGI_MODE_SCANLINE_ORDER(0);
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED}
  DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE       = DXGI_MODE_SCANLINE_ORDER(1);
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE}
  DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST = DXGI_MODE_SCANLINE_ORDER(2);
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST}
  DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST = DXGI_MODE_SCANLINE_ORDER(3);
  {$EXTERNALSYM DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST}

type
  PDXGI_MODE_SCALING = ^DXGI_MODE_SCALING;
  DXGI_MODE_SCALING = DWord;
  {$EXTERNALSYM DXGI_MODE_SCALING}
const
  DXGI_MODE_SCALING_UNSPECIFIED = DXGI_MODE_SCALING(0);
  {$EXTERNALSYM DXGI_MODE_SCALING_UNSPECIFIED}
  DXGI_MODE_SCALING_CENTERED    = DXGI_MODE_SCALING(1);
  {$EXTERNALSYM DXGI_MODE_SCALING_CENTERED}
  DXGI_MODE_SCALING_STRETCHED   = DXGI_MODE_SCALING(2);
  {$EXTERNALSYM DXGI_MODE_SCALING_STRETCHED}

type
  PDXGI_MODE_ROTATION = ^DXGI_MODE_ROTATION;
  DXGI_MODE_ROTATION = DWord;
  {$EXTERNALSYM DXGI_MODE_ROTATION}
const
  DXGI_MODE_ROTATION_UNSPECIFIED = DXGI_MODE_ROTATION(0);
  {$EXTERNALSYM DXGI_MODE_ROTATION_UNSPECIFIED}
  DXGI_MODE_ROTATION_IDENTITY    = DXGI_MODE_ROTATION(1);
  {$EXTERNALSYM DXGI_MODE_ROTATION_IDENTITY}
  DXGI_MODE_ROTATION_ROTATE90    = DXGI_MODE_ROTATION(2);
  {$EXTERNALSYM DXGI_MODE_ROTATION_ROTATE90}
  DXGI_MODE_ROTATION_ROTATE180   = DXGI_MODE_ROTATION(3);
  {$EXTERNALSYM DXGI_MODE_ROTATION_ROTATE180}
  DXGI_MODE_ROTATION_ROTATE270   = DXGI_MODE_ROTATION(4);
  {$EXTERNALSYM DXGI_MODE_ROTATION_ROTATE270}


type

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

  type PIUnknown = ^IUnknown;


{$IFNDEF MFP_SECURITY_ATTRIBUTES_}
  LPSECURITY_ATTRIBUTES = ^PSECURITY_ATTRIBUTES;
  PSECURITY_ATTRIBUTES = ^_SECURITY_ATTRIBUTES;
  _SECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: Pointer;
    bInheritHandle: LongBool;
  end;
  {$EXTERNALSYM _SECURITY_ATTRIBUTES}
  SECURITY_ATTRIBUTES = _SECURITY_ATTRIBUTES;
  {$EXTERNALSYM SECURITY_ATTRIBUTES}
{$ENDIF}

  // End of Additional Prototypes


implementation


function MAKE_DXGI_HRESULT(code: DWORD): HResult;
begin
  Result := MAKE_HRESULT(1,
                         _FACDXGI,
                         code);
end;


function MAKE_DXGI_STATUS(code: DWORD): HResult;
begin
  Result := MAKE_HRESULT(0,
                         _FACDXGI,
                         code);
end;


{$IF CompilerVersion < 24}  // < XE3

//
// D3DCOLORVALUE Helpers
//
class function D3DCOLORVALUE.Init(const rgb: UINT32;
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


class function D3DCOLORVALUE.Init(const knownColor: TColor;
                                  const alpha: FLOAT = 1.0): _D3DCOLORVALUE;
begin
  Result := Init(knownColor,
                 alpha);
end;


class function D3DCOLORVALUE.Init(const red: FLOAT;
                                  const green: FLOAT;
                                  const blue: FLOAT;
                                  const alpha: FLOAT = 1.0): _D3DCOLORVALUE;
begin
  Result.r := red;
  Result.g := green;
  Result.b := blue;
  Result.a := alpha;
end;


class operator D3DCOLORVALUE.Equal(const left: _D3DCOLORVALUE;
                                   const right: _D3DCOLORVALUE): Boolean;
begin
  Result:= (left.r = right.r) and
           (left.g = right.g) and
           (left.b = right.b) and
           (left.a = right.a);
end;


class operator D3DCOLORVALUE.Implicit(const val: DWORD): _D3DCOLORVALUE;
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

class operator D3DCOLORVALUE.Implicit(const val: _D3DCOLORVALUE): DWORD;
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
