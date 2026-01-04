// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  PreviewRenderer.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description:
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Renders GPU output to a preview object (TPanel in this case)
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit PreviewRenderer;

interface

uses

  {WinApi}
  WinApi.Windows,
  {D3D11}
  WinApi.DirectX.D3D11,
  {DXGI}
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  {Application}
  Helpers;

type
  TPreviewRenderer = class
  private

    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FSwapChain: IDXGISwapChain;
    FRenderTargetView: ID3D11RenderTargetView;

    FHWND: HWND;

    FWidth: UINT;
    FHeight: UINT;
    FSampleRate: UINT32;

    procedure ReleaseSwapChain();
    procedure CreateSwapChainInternal(hWnd: HWND;
                                      Width,
                                      Height: UINT;
                                      SampleRate: UINT32);
    procedure CreateRenderTarget();

  public

    constructor Create(const ADevice: ID3D11Device;
                       const AContext: ID3D11DeviceContext);
    destructor Destroy(); override;

    procedure CreateSwapChainForHWND(hWnd: HWND;
                                     Width,
                                     Height: UINT;
                                     SampleRate: UINT32);

    procedure RenderFrame(const SrcTex: ID3D11Texture2D);
  end;


implementation

{ TPreviewRenderer }

constructor TPreviewRenderer.Create(const ADevice: ID3D11Device;
                                    const AContext: ID3D11DeviceContext);
begin

  inherited Create;

  FDevice := ADevice;
  FContext := AContext;

  FSwapChain := nil;
  FRenderTargetView := nil;

  FHWND := 0;
  FWidth := 0;
  FHeight := 0;
  FSampleRate := 0;
end;


destructor TPreviewRenderer.Destroy();
begin

  ReleaseSwapChain;
  FContext := nil;
  FDevice := nil;

  inherited;
end;


procedure TPreviewRenderer.ReleaseSwapChain();
begin

  FRenderTargetView := nil;
  FSwapChain := nil;
end;


procedure TPreviewRenderer.CreateRenderTarget();
var
  hr   : HResult;
  back : ID3D11Texture2D;

begin

  FRenderTargetView := nil;

  hr := FSwapChain.GetBuffer(0,
                             ID3D11Texture2D,
                             back);
  CheckHR(hr, 'Preview: GetBuffer');

  hr := FDevice.CreateRenderTargetView(back,
                                       nil,
                                       @FRenderTargetView);
  CheckHR(hr, 'Preview: CreateRenderTargetView');
end;


procedure TPreviewRenderer.CreateSwapChainInternal(hWnd: HWND;
                                                   Width,
                                                   Height: UINT;
                                                   SampleRate: UINT32);
var
  hr: HResult;
  dxgiDev: IDXGIDevice;
  adapter: IDXGIAdapter;
  factory: IDXGIFactory;
  sd: DXGI_SWAP_CHAIN_DESC;

begin

  ReleaseSwapChain();

  FHWND := hWnd;
  FWidth := Width;
  FHeight := Height;
  FSampleRate := SampleRate;

  hr := FDevice.QueryInterface(IDXGIDevice,
                               dxgiDev);
  CheckHR(hr, 'Preview: QI IDXGIDevice');

  hr := dxgiDev.GetAdapter(adapter);
  CheckHR(hr, 'Preview: GetAdapter');

  hr := adapter.GetParent(IDXGIFactory,
                          Pointer(factory));
  CheckHR(hr, 'Preview: GetParent IDXGIFactory');

  ZeroMemory(@sd,
             SizeOf(sd));

  sd.BufferCount := 2;
  sd.BufferDesc.Width := FWidth;
  sd.BufferDesc.Height := FHeight;
  sd.BufferDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  sd.BufferDesc.RefreshRate.Numerator := FSampleRate;
  sd.BufferDesc.RefreshRate.Denominator := 1;
  sd.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  sd.OutputWindow := hWnd;
  sd.SampleDesc.Count := 1;
  sd.SampleDesc.Quality := 0;
  sd.Windowed := True;
  sd.SwapEffect := DXGI_SWAP_EFFECT_FLIP_DISCARD;
  sd.Flags := 0;

  hr := factory.CreateSwapChain(FDevice,
                                sd,
                                FSwapChain);
  CheckHR(hr, 'Preview: CreateSwapChain');

  CreateRenderTarget();
end;


procedure TPreviewRenderer.CreateSwapChainForHWND(hWnd: HWND;
                                                  Width,
                                                  Height: UINT;
                                                  SampleRate: UINT32);
begin

  CreateSwapChainInternal(hWnd,
                          Width,
                          Height,
                          SampleRate);
end;


procedure TPreviewRenderer.RenderFrame(const SrcTex: ID3D11Texture2D);
var
  ctx: ID3D11DeviceContext;
  backBuf: ID3D11Texture2D;
  hr: HResult;
  d: D3D11_TEXTURE2D_DESC;

begin

  if (FDevice = nil) or
     (FContext = nil) or
     (SrcTex = nil) then
    Exit;

  SrcTex.GetDesc(d);

  // Auto-create / auto-resize swapchain to match the real capture texture size.
  if (FSwapChain = nil) or
     (FHWND = 0) or
     (d.Width <> FWidth) or
     (d.Height <> FHeight) then
    begin
      if (FHWND <> 0) then
        CreateSwapChainInternal(FHWND,
                                d.Width,
                                d.Height,
                                FSampleRate);
    end;

  if (FSwapChain = nil) then
    Exit;

  FDevice.GetImmediateContext(ctx);

  hr := FSwapChain.GetBuffer(0,
                             ID3D11Texture2D,
                             backBuf);
  CheckHR(hr, 'Preview: GetBuffer');

  // Requires matching size+format (now guaranteed by auto-resize)
  ctx.CopyResource(backBuf,
                   SrcTex);

  hr := FSwapChain.Present(1,
                           0);
  CheckHR(hr, 'Preview: Present');
end;

end.

