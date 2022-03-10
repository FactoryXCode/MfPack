// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleConverter.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.1
//
// Description:
//   This unit uses the D2D1 API to perform the translation from sample to bitmap.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX311/Samples/MFFrameSample
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
//==============================================================================
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit SampleConverter;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  {$IF CompilerVersion > 33}
  // Delphi 10.4 or above
  // WinApi.DXGI included with Delphi <= 10.3.3 is not up to date!
  WinApi.D2D1,
  WinApi.DxgiFormat,
  {$ENDIF}
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {VCL}
  VCL.Graphics,
  {System}
  System.Classes,
  System.Types,
  {Application}
  Support;

type
  TSampleConverter = class(TPersistent)
  protected
    FRenderTarget: ID2D1DCRenderTarget;

  private
    FDPI: Integer;
    F2DBitmapProperties: D2D1_BITMAP_PROPERTIES;
    procedure CreateDirect2DBitmapProperties;
    function CreateRenderTarget: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function BitmapFromSample(const ASample: IMFSample;
                              const AVideoInfo: TVideoFormatInfo;
                              var AError: string;
                              var AImage: TBitmap): Boolean;
  end;

implementation

uses
  {System}
  System.SysUtils;


constructor TSampleConverter.Create;
begin
  inherited;
  FDPI := DevicePixelPerInch;
  CreateRenderTarget;
  CreateDirect2DBitmapProperties;
end;


destructor TSampleConverter.Destroy;
begin
  inherited;
  FRenderTarget.Flush();
  SafeRelease(FRenderTarget);
end;


function TSampleConverter.CreateRenderTarget: Boolean;
var
  pFactory: ID2D1Factory;
  oProperties: D2D1_RENDER_TARGET_PROPERTIES;

begin
  Result := SUCCEEDED(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                                        IID_ID2D1Factory,
                                        Nil,
                                        pFactory));
  if Result then
    begin
      {$IF CompilerVersion > 33}
      // Delphi 10.4 or above
      oProperties.&type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
      oProperties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties.PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      {$ELSE}
      oProperties._type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
      oProperties._PixelFormat.Format := WinApi.DirectX.DXGIFormat.DXGI_FORMAT_B8G8R8A8_UNORM;
      oProperties._PixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      {$ENDIF}

      oProperties.dpiX := 0;
      oProperties.dpiY := 0;

      oProperties.usage := D2D1_RENDER_TARGET_USAGE_NONE;
      oProperties.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
      Result := SUCCEEDED(pFactory.CreateDCRenderTarget(oProperties,
                                                        FRenderTarget));
    end;
end;


procedure TSampleConverter.CreateDirect2DBitmapProperties;
var
  oPixelFormat: D2D1_PIXEL_FORMAT;
begin
  oPixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;

  {$IF CompilerVersion > 33}
  // Delphi 10.4 or above
  oPixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  F2DBitmapProperties.PixelFormat := oPixelFormat;
  {$ELSE}
  oPixelFormat.Format := WinApi.DirectX.DXGIFormat.DXGI_FORMAT_B8G8R8A8_UNORM;
  F2DBitmapProperties._PixelFormat := oPixelFormat;
  {$ENDIF}

  F2DBitmapProperties.dpiX := FDPI;
  F2DBitmapProperties.dpiY := FDPI;
end;


function TSampleConverter.BitmapFromSample(const ASample: IMFSample;
                                           const AVideoInfo: TVideoFormatInfo;
                                           var AError: string;
                                           var AImage: TBitmap): Boolean;
var
  pBuffer: IMFMediaBuffer;
  pBitmapData: PByte;
  cbBitmapData: DWord;
  o2DBitmap: ID2D1Bitmap;
  oSize: D2D1_SIZE_U;
  iPitch: Integer;
  iActualBMPDataSize: Integer;
  iExpectedBMPDataSize: Integer;
  pClipRect: PRect;
begin
  AError := '';

  // Converts a sample with multiple buffers into a sample with a single buffer.
  Result := SUCCEEDED(ASample.ConvertToContiguousBuffer(pBuffer));

  try

  if Result then
    begin
      Result := SUCCEEDED(pBuffer.Lock(pBitmapData,
                                       Nil,
                                       @cbBitmapData));
      try
        iPitch := 4 * AVideoInfo.iVideoWidth;
        // For full frame capture, use the buffer dimensions for the data size check
        iExpectedBMPDataSize := (AVideoInfo.iBufferWidth * 4) * AVideoInfo.iBufferHeight;
        iActualBMPDataSize := Integer(cbBitmapData);
        if iActualBMPDataSize <> iExpectedBMPDataSize then
          AError := Format('Sample size does not match expected size. Current: %d. Expected: %d',
                           [iActualBMPDataSize, iExpectedBMPDataSize]);
        if Result then
          begin
            // Bind the render target to the bitmap
            {$IF CompilerVersion > 33}
             // Delphi 10.4 or above
            Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle, AImage.Canvas.ClipRect));
            {$ELSE}

            CopyTRectToPRect(AImage.Canvas.ClipRect,
                             pClipRect);
            Result := SUCCEEDED(FRenderTarget.BindDC(AImage.Canvas.Handle,
                                                     pClipRect));
            {$ENDIF}

            if Result then
              begin
                // Create the 2D bitmap interface
                oSize.Width := AVideoInfo.iVideoWidth;
                oSize.Height := AVideoInfo.iVideoHeight;
                Result := SUCCEEDED(FRenderTarget.CreateBitmap(oSize,
                                                               pBitmapData,
                                                               iPitch,
                                                               F2DBitmapProperties,
                                                               o2DBitmap));
              end;

            // Draw the 2D bitmap to the render target
            if Result then
              begin
                FRenderTarget.BeginDraw;
                FRenderTarget.DrawBitmap(o2DBitmap);
                FRenderTarget.EndDraw();
              end;
          end;

      finally
        pBuffer.Unlock;
        SafeRelease(pBuffer);
      end;
    end;

  finally
    pBitmapData := Nil;
    pClipRect := Nil;
  end;

end;

end.
