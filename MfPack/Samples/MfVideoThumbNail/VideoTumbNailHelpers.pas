// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: VideoTumbNailHelpers.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Helper methods.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.20348.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft samples.
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
unit VideoTumbNailHelpers;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {Vcl}
  Vcl.Graphics,
  {DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1Helper;


type

  D2DMatrix3x2FHelper = Matrix3x2FHelper;


  function WidthD2D1RectF(const d2d1rect: D2D1_RECT_F): FLOAT; inline;
  function HeightD2D1RectF(const d2d1rect: D2D1_RECT_F): FLOAT; inline;

  function SumD2D1RectF(pl: D2D1_RECT_F; pr: D2D1_RECT_F): D2D1_RECT_F; inline;
  function SubtD2D1RectF(pl: D2D1_RECT_F; pr: D2D1_RECT_F): D2D1_RECT_F; inline;
  function multD2D1RectFxF(pl: D2D1_RECT_F; pr: FLOAT): D2D1_RECT_F; inline;

  function LetterBoxRectF(aspectRatio: D2D1_SIZE_F;
                          const rcDest: D2D1_RECT_F): D2D1_RECT_F;

  function D2DMatrix3X2FTransformPoint(const pmat: D2D1_MATRIX_3X2_F;
                                       point: D2D_POINT_2F): D2D_POINT_2F;

  function D2D1TColorF(dcolor: TColor;
           {opacity} a: Single = 1.0): D2D1_COLOR_F;

implementation




function WidthD2D1RectF(const d2d1rect: D2D1_RECT_F): FLOAT; inline;
begin
  Result := d2d1rect.right - d2d1rect.left;
end;

function HeightD2D1RectF(const d2d1rect: D2D1_RECT_F): FLOAT; inline;
begin
  Result := d2d1rect.bottom - d2d1rect.top;
end;

function SumD2D1RectF(pl: D2D1_RECT_F; pr: D2D1_RECT_F): D2D1_RECT_F; inline;
begin
  Result.left   := pl.left + pr.left;
  Result.top    := pl.top + pr.top;
  Result.right  := pl.right + pr.right;
  Result.bottom := pl.bottom + pr.bottom;
end;

function SubtD2D1RectF(pl: D2D1_RECT_F; pr: D2D1_RECT_F): D2D1_RECT_F; inline;
begin
  Result.left   := pl.left - pr.left;
  Result.top    := pl.top - pr.top;
  Result.right  := pl.right - pr.right;
  Result.bottom := pl.bottom - pr.bottom;
end;

function multD2D1RectFxF(pl: D2D1_RECT_F; pr: FLOAT): D2D1_RECT_F; inline;
begin
  Result.left   := pl.left * pr;
  Result.top    := pl.top * pr;
  Result.right  := pl.right * pr;
  Result.bottom := pl.bottom * pr;
end;


//-------------------------------------------------------------------
// LetterBoxRectF
//
// Given a destination rectangle (rcDest) and an aspect ratio,
// returns a letterboxed rectangle within rcDest.
//-------------------------------------------------------------------
function LetterBoxRectF(aspectRatio: D2D1_SIZE_F;
                        const rcDest: D2D1_RECT_F): D2D1_RECT_F;
var
  _width, _height: Single;
  SrcWidth, SrcHeight, DestWidth, DestHeight: Single;
  rcResult: D2D1_RECT_F;

begin

  SrcWidth   := aspectRatio.width;
  SrcHeight  := aspectRatio.height;
  DestWidth  := WidthD2D1RectF(rcDest);
  DestHeight := HeightD2D1RectF(rcDest);

  // Avoid divide by zero (even though MulDiv handles this)
  if (SrcWidth = 0) or (SrcHeight = 0) then
    begin
      Result:= rcResult;  // result = empty rect
      Exit;
    end;

  // First try: Letterbox along the sides. ("pillarbox")
  _width := (DestHeight * SrcWidth) / SrcHeight;
  _height := DestHeight;

  if (_width > DestWidth) then
    begin
      // Letterbox along the top and bottom.
      _width := DestWidth;
      _height := (DestWidth * SrcHeight) / SrcWidth;
    end;

  // Fill in the rectangle
  rcResult.left   := rcDest.left + ((DestWidth - _width) / 2);
  rcResult.right  := rcResult.left + _width;
  rcResult.top    := rcDest.top + ((DestHeight - _height) / 2);
  rcResult.bottom := rcResult.top + _height;

  Result := rcResult;
end;


// This function is available in DirectX version >= 11
function D2DMatrix3X2FTransformPoint(const pmat: D2D1_MATRIX_3X2_F;
                                     point: D2D_POINT_2F): D2D_POINT_2F;
begin
  Result.x := point.x * pmat._11 + point.y * pmat._21 + pmat._31;
  Result.y := point.x * pmat._12 + point.y * pmat._22 + pmat._32;
end;

// Note that the Alphachannel will not be altered
function D2D1TColorF(dcolor: TColor;
           {opacity} a: Single = 1.0): D2D1_COLOR_F;
begin
  Result.r := ((dcolor AND $00FF0000) shr 16) / 255.0;
  Result.g := ((dcolor AND $0000FF00) shr 8) / 255.0;
  Result.b := ((dcolor AND $000000FF)) / 255.0;
  Result.a := a;
end;


function D2D1SizeF(width: FLOAT;
                   height: FLOAT): D2D1_SIZE_F;
begin
  Result.width := width;
  Result.height := height;
end;

function D2D1SizeU(width: UINT32;
                   height: UINT32): D2D1_SIZE_U;
begin
  Result.width := width;
  Result.height := height;
end;


function D2D1PointF(x: FLOAT;
                    y: FLOAT): D2D_POINT_2F;
begin
  Result.x := x;
  Result.y := y;
end;


function D2D1PointU(x: UINT32;
                    y: UINT32): D2D_POINT_2U;
begin
  Result.x := x;
  Result.y := y;
end;

end.
