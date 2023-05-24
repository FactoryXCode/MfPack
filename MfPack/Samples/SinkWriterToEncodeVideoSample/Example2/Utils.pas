// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
// https://github.com/FactoryXCode/MfPack
// Module: Utils.pas
// Kind: Pascal / Delphi unit
// Release date: 11-12-2012
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Contains bitmap helpers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Renate Schaaf.
//
// ------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
// 18/05/2023 Renate              Fixed runtime error on selecting multiple bitmaps.
//                                Speedup of bitmap resizing using D2D1_1
// ------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPack/Samples/SinkWriterToEncodeVideoSample
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// ==============================================================================
// Source:
//   https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// ==============================================================================
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
// ==============================================================================
unit Utils;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Types,
  System.SysUtils,
  {Vcl}
  Vcl.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;

  // Now uses Direct2D for resizing. Better quality and much faster than version 3.1.4
  procedure ResizeBitmap(aBitmap: TBitmap;
                         toWidth: Integer;
                         toHeight: Integer);

  // Calculates nanoseconds to milliseconds.
  function NsecToMsec(nSec: Int64): Int64; inline;

  // Calculates frame duration in 100 nanoseconds units.
  function CalcFrameDuration(aLatency: UINT32;
                             aFrameRate: Double): HNSTIME;


implementation

uses
  {Vcl}
  Vcl.Direct2D,
  {DirectX}
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGIType;


procedure ScaleDirect2D1(NewWidth: Integer;
                         NewHeight: Integer;
                         const Source: TBitmap;
                         const target: TBitmap);
var
  DeviceContext: ID2D1DeviceContext;
  xscale: Single;
  yscale: Single;
  bmPitch: Integer;
  bmProps: D2D1_BITMAP_PROPERTIES1;
  bmBits: Pointer;
  D2D1Bitmap: ID2D1Bitmap1;
  CanvasD2D: TDirect2DCanvas;

begin
  target.PixelFormat := pf32bit;
  target.SetSize(NewWidth,
                 NewHeight);
  Source.AlphaFormat := afIgnored;
  CanvasD2D := TDirect2DCanvas.Create(target.Canvas.Handle,
                                      Rect(0, 0, NewWidth, NewHeight));
  try
    if Supports(CanvasD2D.RenderTarget,
                ID2D1DeviceContext,
                DeviceContext) then
      begin
        bmPitch := -4 * Source.Width;
        bmProps := Default(D2D1_BITMAP_PROPERTIES1); // ZeroMemory
        bmProps._pixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
        bmProps._pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
        xscale := NewWidth / Source.Width;
        yscale := NewHeight / Source.Height;
        bmBits := Source.ScanLine[0];
        DeviceContext.SetTransform(D2D_Matrix_3x2_F.Init(xscale, 0, 0,
                                                         yscale, 0, 0));
{$IFOPT R+}
{$DEFINE R_Plus}
{$R-}
{$ENDIF}
        DeviceContext.CreateBitmap(D2SizeU(Source.Width, Source.Height),
                                   bmBits,
                                   bmPitch,
                                   @bmProps,
                                   D2D1Bitmap);
{$IFDEF R_Plus}
{$R+}
{$ENDIF}
        DeviceContext.BeginDraw;
        DeviceContext.Clear(Default(_D3DColorValue));
        DeviceContext.DrawImage(D2D1Bitmap,
                                nil,
                                nil,
                                D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC); // D2D1_INTERPOLATION_MODE_DEFINITION_Cubic);
        DeviceContext.EndDraw;
      end
    else
      begin
        CanvasD2D.BeginDraw;
        CanvasD2D.StretchDraw(Rect(0, 0, NewWidth, NewHeight),
                              Source);
        CanvasD2D.EndDraw;
      end;
  finally
    CanvasD2D.Free;
  end;
end;


procedure ResizeBitmap(aBitmap: TBitmap;
                       toWidth: Integer;
                       toHeight: Integer);
var
  bTmp: TBitmap;

begin
  if (aBitmap.PixelFormat <> pf32bit) then
    aBitmap.PixelFormat := pf32bit;
  // Create a temporary bitmap
  bTmp := TBitmap.Create;
  try
    ScaleDirect2D1(toWidth,
                   toHeight,
                   aBitmap,
                   bTmp);
    aBitmap.Assign(bTmp);
  finally
    bTmp.Free;
  end;
end;


//
function NsecToMsec(nSec: Int64): Int64; inline;
begin
  Result := Round(nSec / 1000000);
end;


//
function CalcFrameDuration(aLatency: UINT32;
                           aFrameRate: Double): HNSTIME;
const
  msec = (1000 * 1000); // One millisecond = 1,000,000 nano seconds

begin
  Result := Round(aLatency * msec / aFrameRate);
end;


end.
