// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastArtwork.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.5
// Description: Frame pump. Reads decoded RGB32 video frames, burns subtitles into
//              the frames, and writes the result to a new video stream.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// =============================================================================
// Source: -
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
unit MfCastArtwork;

interface

uses
  Winapi.Windows, System.SysUtils;

function MfCastLoadArtworkRgb32(const FileName: WideString;
                                Width, Height: UINT32;
                                out Pixels: TBytes): HRESULT;

implementation

uses
  Winapi.ActiveX, WinApi.WinApiTypes, WinApi.WIC.WinCodec;
function MfCastLoadArtworkRgb32(const FileName: WideString;
                                               Width: UINT32;
                                               Height: UINT32;
                                               out Pixels: TBytes): HRESULT;
var
  factory: IWICImagingFactory;
  decoder: IWICBitmapDecoder;
  frame: IWICBitmapFrameDecode;
  scaler: IWICBitmapScaler;
  converter: IWICFormatConverter;
  sourceWidth: UINT;
  sourceHeight: UINT;
  scaledWidth: UINT;
  scaledHeight: UINT;
  left: UINT;
  top: UINT;
  sourcePixels: TBytes;
  sourceRow: UINT;
  targetRow: UINT;
  sourceOffset: NativeInt;
  targetOffset: NativeInt;

begin

  SetLength(Pixels,
            0);
  factory := nil;
  decoder := nil;
  frame := nil;
  scaler := nil;
  converter := nil;

  if (FileName = '') or (Width = 0) or (Height = 0) or
     (UInt64(Width) * UInt64(Height) > UInt64(MaxInt div 4)) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Result := CoCreateInstance(CLSID_WICImagingFactory1,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IWICImagingFactory,
                             factory);
  if FAILED(Result) then
    Exit;

  Result := factory.CreateDecoderFromFilename(PWideChar(FileName),
                                               nil,
                                               GENERIC_READ,
                                               WICDecodeMetadataCacheOnLoad,
                                               decoder);
  if FAILED(Result) then
    Exit;

  // A GIF is deliberately treated as static artwork: use its first frame.
  Result := decoder.GetFrame(0,
                             frame);
  if FAILED(Result) then
    Exit;

  Result := frame.GetSize(@sourceWidth,
                          @sourceHeight);
  if FAILED(Result) or (sourceWidth = 0) or (sourceHeight = 0) then
    begin
      if SUCCEEDED(Result) then
        Result := E_INVALIDARG;
      Exit;
    end;

  if (UInt64(Width) * UInt64(sourceHeight)) <=
     (UInt64(Height) * UInt64(sourceWidth)) then
    begin
      scaledWidth := Width;
      scaledHeight := UINT((UInt64(sourceHeight) * UInt64(Width)) div sourceWidth);
    end
  else
    begin
      scaledHeight := Height;
      scaledWidth := UINT((UInt64(sourceWidth) * UInt64(Height)) div sourceHeight);
    end;

  if scaledWidth = 0 then
    scaledWidth := 1;
  if scaledHeight = 0 then
    scaledHeight := 1;

  Result := factory.CreateBitmapScaler(scaler);
  if FAILED(Result) then
    Exit;

  Result := scaler.Initialize(frame,
                              scaledWidth,
                              scaledHeight,
                              WICBitmapInterpolationModeHighQualityCubic);
  if FAILED(Result) then
    Exit;

  Result := factory.CreateFormatConverter(converter);
  if FAILED(Result) then
    Exit;

  Result := converter.Initialize(scaler,
                                 GUID_WICPixelFormat32bppBGRA,
                                 WICBitmapDitherTypeNone,
                                 nil,
                                 0.0,
                                 WICBitmapPaletteTypeCustom);
  if FAILED(Result) then
    Exit;

  SetLength(sourcePixels,
            NativeInt(scaledWidth) * NativeInt(scaledHeight) * 4);
  Result := converter.CopyPixels(nil,
                                 scaledWidth * 4,
                                 Length(sourcePixels),
                                 PByte(sourcePixels));
  if FAILED(Result) then
    Exit;

  // The sink writer's RGB32 input is bottom-up. Centre the decoded picture on
  // a black 16:9 canvas and reverse the WIC top-down scan-line order.
  SetLength(Pixels,
            NativeInt(Width) * NativeInt(Height) * 4);

  FillChar(Pixels[0],
           Length(Pixels),
           0);

  left := (Width - scaledWidth) div 2;
  top := (Height - scaledHeight) div 2;

  for sourceRow := 0 to scaledHeight - 1 do
    begin
      targetRow := Height - 1 - (top + sourceRow);
      sourceOffset := NativeInt(sourceRow) * NativeInt(scaledWidth) * 4;
      targetOffset := (NativeInt(targetRow) * NativeInt(Width) + NativeInt(left)) * 4;
      CopyMemory(@Pixels[targetOffset],
                 @sourcePixels[sourceOffset],
                 scaledWidth * 4);
    end;

  Result := S_OK;
end;


end.
