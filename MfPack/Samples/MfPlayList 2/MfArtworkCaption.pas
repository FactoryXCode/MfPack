// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfArtworkCaption.pas
// Kind: Pascal / Delphi unit
// Release date: 26-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: WIC image decoding with Direct2D/DirectWrite artwork captions.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (Carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
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
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/sequencer-source-example-code
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
unit MfArtworkCaption;

{$IFDEF FPC}
{$I MfPack.Fpc.inc}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Windows,
  SysUtils,
  Classes,
  Math,
  Graphics;
{$ELSE}
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {Vcl}
  Vcl.Graphics;
{$ENDIF}

  // Render in memory; never change the supplied artwork file.
  procedure RenderArtworkCaption(const AFileName: string;
                                 const AArtist: string;
                                 const ATitle: string;
                                 const APicture: TPicture);

implementation

uses

  {WinApi}
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.ActiveX.ObjBase,

{$IFNDEF FPC}
  Winapi.Wincodec,
{$ENDIF}

  WinApi.WIC.WinCodec,
  WinApi.DirectX.D2D1,

  WinApi.DirectX.DWrite,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DCommon;


procedure Check(const AStatus: HRESULT;
                const AOperation: string);

begin

  if (AStatus < 0) then
    raise Exception.CreateFmt('%s failed ($%.8x).',
                              [AOperation, Cardinal(AStatus)]);
end;

// Helpers =====================================================================

function RectF(const L: Single;
               const T: Single;
               const R: Single;
               const B: Single): D2D1_RECT_F;

begin

  Result.left := L;
  Result.top := T;
  Result.right := R;
  Result.bottom := B;
end;


function ColorF(const R: Single;
                const G: Single;
                const B: Single;
                const A: Single): D2D1_COLOR_F;

begin

  Result.r := R;
  Result.g := G;
  Result.b := B;
  Result.a := A;
end;

// =============================================================================


procedure RenderArtworkCaption(const AFileName,
                               AArtist,
                               ATitle: string;
                               const APicture: TPicture);
var
  Wic: IWICImagingFactory;
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  Scaler: IWICBitmapScaler;
  Converter: IWICFormatConverter;
  Output: IWICBitmap;
  Factory: ID2D1Factory;
  Target: ID2D1RenderTarget;
  Bitmap: ID2D1Bitmap;
  Brush: ID2D1SolidColorBrush;
  GradientStops: ID2D1GradientStopCollection;
  Gradient: ID2D1LinearGradientBrush;
  Stops: array[0..1] of D2D1_GRADIENT_STOP;
  GradientProperties: D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES;
  WriteUnknown: IUnknown;
  WriteFactory: IDWriteFactory;
  ArtistFormat, TitleFormat: IDWriteTextFormat;
  Properties: D2D1_RENDER_TARGET_PROPERTIES;
  Color: D2D1_COLOR_F;
  Bounds: D2D1_RECT_F;
  W: Cardinal;
  H: Cardinal;
  SourceW: Cardinal;
  SourceH: Cardinal;
  Stride: Cardinal;
  ByteCount: Cardinal;
  Scale: Single;
  Padding: Single;
  BandTop: Single;
  Header: BITMAPFILEHEADER;
  Info: BITMAPINFOHEADER;
  Pixels: TBytes;
  Stream: TMemoryStream;
  Dib: TBitmap;


  procedure MakeFormat(const ASize: Single;
                       const AWeight: DWRITE_FONT_WEIGHT;
                       out AFormat: IDWriteTextFormat);
  var
    Trimming: DWRITE_TRIMMING;
    Ellipsis: IDWriteInlineObject;

  begin

    Check(WriteFactory.CreateTextFormat('Segoe UI',
                                        nil,
                                        AWeight,
                                        DWRITE_FONT_STYLE_NORMAL,
                                        DWRITE_FONT_STRETCH_NORMAL,
                                        ASize,
                                        '',
                                        AFormat),
          'Create text format');
    Check(AFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP),
          'Set single-line text');

    Check(AFormat.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER),
          'Align text');

    FillChar(Trimming,
             SizeOf(Trimming),
             0);

    Trimming.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;

    Check(WriteFactory.CreateEllipsisTrimmingSign(AFormat,
                                                  Ellipsis),
          'Create ellipsis');

    Check(AFormat.SetTrimming(@Trimming,
                              Ellipsis),
          'Set text trimming');
  end;

begin

  Check(CoCreateInstance(CLSID_WICImagingFactory1,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IWICImagingFactory,
                         Wic),
        'Create WIC factory');

  Check(Wic.CreateDecoderFromFilename(PWideChar(AFileName),
                                      nil,
                                      GENERIC_READ,
                                      WICDecodeMetadataCacheOnLoad,
                                      Decoder),
        'Decode artwork');

  Check(Decoder.GetFrame(0,
                         Frame),
        'Get artwork frame');

  Check(Frame.GetSize(@SourceW,
                      @SourceH),
        'Read artwork dimensions');

  if (SourceW = 0) or (SourceH = 0) then
    raise Exception.Create('Artwork has no pixels.');

  // Bound the display composition, including very large photographs.
  Scale := Min(1.0,
               1600.0 / Max(SourceW,
                            SourceH));
  W := Max(1,
           Round(SourceW * Scale));

  H := Max(1,
           Round(SourceH * Scale));

  Check(Wic.CreateBitmapScaler(Scaler),
        'Create WIC scaler');

  Check(Scaler.Initialize(Frame,
                          W,
                          H,
                          WICBitmapInterpolationModeFant),
        'Scale artwork');

  Check(Wic.CreateFormatConverter(Converter),
        'Create WIC format converter');

  Check(Converter.Initialize(Scaler,
                             GUID_WICPixelFormat32bppPBGRA,
                             WICBitmapDitherTypeNone,
                             nil,
                             0,
                             WICBitmapPaletteTypeCustom),
        'Convert artwork');

  Check(Wic.CreateBitmap(W,
                         H,
                         GUID_WICPixelFormat32bppPBGRA,
                         WICBitmapCacheOnLoad,
                         Output),
        'Create caption surface');

  Check(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                          IID_ID2D1Factory,
                          nil,
                          Factory),
        'Create Direct2D factory');

  FillChar(Properties,
           SizeOf(Properties),
           0);

  Properties._type := D2D1_RENDER_TARGET_TYPE_SOFTWARE;
  Properties._pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Properties._pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
  Properties.dpiX := 96;
  Properties.dpiY := 96;

{$IFDEF FPC}
  Check(Factory.CreateWicBitmapRenderTarget(Output,
                                            @Properties,
                                            Target),
        'Create WIC render target');

  Check(Target.CreateBitmapFromWicBitmap(Converter,
                                         nil,
                                         Bitmap),
        'Create artwork bitmap');
{$ELSE}
  // Delphi's MfPack D2D1 declarations use the RTL's WIC interface types.
  // Query the same native interfaces while decoding through MfPack's WIC unit.
  Check(Factory.CreateWicBitmapRenderTarget(Output as Winapi.Wincodec.IWICBitmap,
                                            @Properties,
                                            Target),
        'Create WIC render target');

  Check(Target.CreateBitmapFromWicBitmap(Converter as Winapi.Wincodec.IWICBitmapSource,
                                         nil,
                                         Bitmap),
        'Create artwork bitmap');
{$ENDIF}

  Color := ColorF(0,
                  0,
                  0,
                  1);

  Check(Target.CreateSolidColorBrush(@Color,
                                     nil,
                                     Brush),
        'Create caption brush');

  Check(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED,
                            IID_IDWriteFactory,
                            WriteUnknown),
        'Create DirectWrite factory');

  WriteFactory := WriteUnknown as IDWriteFactory;
  Scale := Min(W,
               H) / 1024.0;

  Padding := 44 * Scale;
  BandTop := H - 230 * Scale;

  MakeFormat(32 * Scale,
             DWRITE_FONT_WEIGHT_NORMAL,
             ArtistFormat);

  MakeFormat(56 * Scale,
             DWRITE_FONT_WEIGHT_SEMI_BOLD,
             TitleFormat);

  Stops[0].position := 0;

  Stops[0].color := ColorF(0.015,
                           0.025,
                           0.045,
                           0);
  Stops[1].position := 1;

  Stops[1].color := ColorF(0.015,
                           0.025,
                           0.045,
                           0.94);

  Check(Target.CreateGradientStopCollection(@Stops[0],
                                            2,
                                            D2D1_GAMMA_2_2,
                                            D2D1_EXTEND_MODE_CLAMP,
                                            GradientStops),
        'Create caption fade');

  FillChar(GradientProperties,
           SizeOf(GradientProperties),
           0);

  GradientProperties.startPoint.y := BandTop;
  GradientProperties.endPoint.y := H;

  Check(Target.CreateLinearGradientBrush(@GradientProperties,
                                         nil,
                                         GradientStops,
                                         Gradient),
        'Create caption background');

  Target.BeginDraw();

  try
    Target.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE);

    Color := ColorF(0.015,
                    0.025,
                    0.045,
                    1);

    Target.Clear(@Color);

    Bounds := RectF(0,
                    0,
                    W,
                    H);

    Target.DrawBitmap(Bitmap,
                      @Bounds,
                      1,
                      D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,
                      nil);

    // Fade a dark scrim into the lower part of the cover for readable type.
    Bounds := RectF(0,
                    BandTop,
                    W,
                    H);

    Target.FillRectangle(@Bounds,
                         Gradient);

    Color := ColorF(0.35,
                    0.88,
                    0.97,
                    1);

    Brush.SetColor(@Color);

    Bounds := RectF(Padding,
                    H - 164 * Scale,
                    W - Padding,
                    H - 112 * Scale);

    Target.DrawText(PWideChar(AArtist),
                    Length(AArtist),
                    ArtistFormat,
                    @Bounds,
                    Brush,
                    D2D1_DRAW_TEXT_OPTIONS_CLIP,
                    DWRITE_MEASURING_MODE_NATURAL);

    Color := ColorF(1,
                    1,
                    1,
                    1);

    Brush.SetColor(@Color);

    Bounds := RectF(Padding,
                    H - 115 * Scale,
                    W - Padding,
                    H - 30 * Scale);

    Target.DrawText(PWideChar(ATitle),
                    Length(ATitle),
                    TitleFormat,
                    @Bounds,
                    Brush,
                    D2D1_DRAW_TEXT_OPTIONS_CLIP,
                    DWRITE_MEASURING_MODE_NATURAL);

  finally
    Check(Target.EndDraw(nil,
                         nil),
          'Finish artwork caption');
  end;

  // Transfer the opaque WIC result to either VCL or LCL using a top-down BMP.
  Stride := W * 4;
  ByteCount := Stride * H;

  SetLength(Pixels,
            ByteCount);

  Check(Output.CopyPixels(nil,
                          Stride,
                          ByteCount,
                          @Pixels[0]),
        'Copy caption pixels');

  FillChar(Header,
           SizeOf(Header),
           0);

  Header.bfType := $4D42;
  Header.bfOffBits := SizeOf(Header) + SizeOf(Info);
  Header.bfSize := Header.bfOffBits + ByteCount;

  FillChar(Info,
           SizeOf(Info),
           0);

  Info.biSize := SizeOf(Info);
  Info.biWidth := W;
  Info.biHeight := -Longint(H);
  Info.biPlanes := 1;
  Info.biBitCount := 32;
  Info.biCompression := BI_RGB;
  Info.biSizeImage := ByteCount;
  Stream := TMemoryStream.Create();

  try
    Stream.WriteBuffer(Header,
                       SizeOf(Header));

    Stream.WriteBuffer(Info,
                       SizeOf(Info));

    Stream.WriteBuffer(Pixels[0],
                       ByteCount);

    Stream.Position := 0;
    Dib := TBitmap.Create();

    try
      Dib.LoadFromStream(Stream);
      APicture.Assign(Dib);

    finally
      Dib.Free();
    end;

  finally
    Stream.Free();
  end;
end;

end.
