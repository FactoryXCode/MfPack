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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
// ------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPack/Samples/SinkWriterToEncodeVideoSample
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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

const
  // Standard definitions for format versus framerate:
  // 240p: 18 fps
  // 360p: 24 fps
  // 480p: 24 fps
  // 720p: 24 fps  HD Ready
  // 1080p: 30 fps  Full HD
  // 4k: 60 fps Ultra HD
  // 5k: 60 fps (supported by action camera's like GoPro)
  // 8k: 60 to 240 fps
  //                                         fps       Usage
  //                                         ===       ===================================================================================
  FrameRateArray: array [0..16] of Single = (8,        // Primitive animations.
                                             10,       // Primitive animations.
                                             12,       // Primitive animations.
                                             15,       // Primitive animations.
                                             16,       // Animations.
                                             18,       // Animations.
                                             23.976,   // Television / movies (intended color).
                                             24,       // Television / movies (intended b&w).
                                             25,       // European television standard.
                                             29.97,    // North American traditional television standard to support b&w TV.
                                             30,       // North American standard and live TV.
                                             45,       // Rarely used.
                                             48,       // Rarely used.
                                             60,       // Games, sports, action footage and fast movements (limited slow motion purpose).
                                             90,       // Rarely used. Games, sports, action footage and fast movements (slow motion purpose).
                                             120,      // Games, sports, action footage and fast movements (slow motion purpose).
                                             240);     // Extremely slow motion footage and creating slow motion videos.

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
  System.Win.ComObj,
  {DirectX}
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGIType;


procedure ScaleDirect2D1(NewWidth: Integer;
                         NewHeight: Integer;
                         const Source: TBitmap;
                         const Target: TBitmap);
var
  Factory: ID2D1Factory;
  RenderTarget: ID2D1DCRenderTarget;
  Bitmap: ID2D1Bitmap;
  Properties: D2D1_RENDER_TARGET_PROPERTIES;
  BitmapProperties: D2D1_BITMAP_PROPERTIES;
  BitmapSize: D2D1_SIZE_U;
  Destination: D2D1_RECT_F;
  Bounds: TRect;
  Pixels: TBytes;
  Row: Integer;
begin
  if (NewWidth <= 0) or (NewHeight <= 0) or
     (Source.Width <= 0) or (Source.Height <= 0) then
    raise EArgumentException.Create('Bitmap dimensions must be positive');
  Source.PixelFormat := pf32bit;
  Target.PixelFormat := pf32bit;
  Target.SetSize(NewWidth,NewHeight);
  SetLength(Pixels,Source.Width * Source.Height * 4);
  for Row := 0 to Source.Height - 1 do
    Move(Source.ScanLine[Row]^,Pixels[Row * Source.Width * 4],Source.Width * 4);

  OleCheck(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,ID2D1Factory,nil,Factory));
  Properties := Default(D2D1_RENDER_TARGET_PROPERTIES);
  Properties._pixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Properties._pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
  Properties.dpiX := 96;
  Properties.dpiY := 96;
  OleCheck(Factory.CreateDCRenderTarget(@Properties,RenderTarget));
  Bounds := Rect(0,0,NewWidth,NewHeight);
  OleCheck(RenderTarget.BindDC(Target.Canvas.Handle,@Bounds));
  BitmapProperties := Default(D2D1_BITMAP_PROPERTIES);
  BitmapProperties._pixelFormat := Properties._pixelFormat;
  BitmapProperties.dpiX := 96;
  BitmapProperties.dpiY := 96;
  BitmapSize.Width := Source.Width;
  BitmapSize.Height := Source.Height;
  OleCheck(RenderTarget.CreateBitmap(BitmapSize,@Pixels[0],Source.Width * 4,@BitmapProperties,Bitmap));
  Destination.Left := 0;
  Destination.Top := 0;
  Destination.Right := NewWidth;
  Destination.Bottom := NewHeight;
  RenderTarget.BeginDraw();
  RenderTarget.DrawBitmap(Bitmap,@Destination,1,D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,nil);
  OleCheck(RenderTarget.EndDraw(nil,nil));
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
