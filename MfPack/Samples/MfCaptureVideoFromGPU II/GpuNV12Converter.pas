// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  GpuNV12Converter.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Converter for NV12 to BGRA.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Carmen (carmenh), Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX320
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
unit GpuNV12Converter;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  System.AnsiStrings,
  {D3D11}
  WinApi.DirectX.D3D11,
  WinApi.DirectX.D3DCommon,
  {DXGI}
  WinApi.DirectX.DXGI,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.D3DCompiler;

type

  // Simple full-screen quad vertex
  TVertex = record
    Pos: array[0..1] of Single;  // POSITION
    Tex: array[0..1] of Single;  // TEXCOORD0
  end;

  /// GPU-assisted BGRA → NV12 converter.
  ///
  /// Usage:
  ///   - Construct once (after you have a D3D11 device/context and know Width/Height)
  ///   - Each frame:
  ///       * SrcBGRA = desktop duplication texture (DXGI_FORMAT_B8G8R8A8_UNORM)
  ///       * DestNV12 = mapped staging texture (DXGI_FORMAT_NV12)
  ///     Call Convert(SrcBGRA, DestNV12);

  TGpuNV12Converter = class
  private

    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FWidth: UINT;
    FHeight: UINT;

    // Shaders & pipeline
    FVS: ID3D11VertexShader;
    FPS_Y: ID3D11PixelShader;
    FPS_UV: ID3D11PixelShader;
    FLayout: ID3D11InputLayout;
    FVB: ID3D11Buffer;
    FIB: ID3D11Buffer; // index buffer for fullscreen quad
    FSampler: ID3D11SamplerState;

    // Intermediate render targets (GPU)
    FTexY: ID3D11Texture2D;        // R8_UNORM, full size
    FTexUV: ID3D11Texture2D;        // R8G8_UNORM, (Width/2) x (Height/2)
    FRtvY: ID3D11RenderTargetView;
    FRtvUV: ID3D11RenderTargetView;

    // CPU staging for Y/UV
    FStagingY: ID3D11Texture2D;
    FStagingUV: ID3D11Texture2D;

    procedure CreateTargets();
    procedure CreateShadersAndLayout();
    procedure CreateGeometry();

    procedure RenderToYUV(const SrcBGRA: ID3D11Texture2D);
    procedure DownloadYUVAndPackNV12(const NV12: D3D11_MAPPED_SUBRESOURCE);

  public

    constructor Create(const ADevice: ID3D11Device;
                       const AContext: ID3D11DeviceContext;
                       AWidth,
                       AHeight: UINT);
    destructor Destroy(); override;

    /// Convert a BGRA desktop texture into NV12 in a mapped staging texture.
    /// - SrcBGRA is a GPU BGRA texture (desktop duplication).
    /// - DestNV12 is a mapped D3D11 staging texture with NV12 layout:
    ///     Y plane:   Width x Height  bytes, RowPitch = DestNV12.RowPitch
    ///     UV plane:  Width x Height/2 bytes, starts at (YPlane + RowPitch*Height)
    ///
    procedure Convert(const SrcBGRA: ID3D11Texture2D;
                      const DestNV12: D3D11_MAPPED_SUBRESOURCE);
  end;


implementation

uses
  Helpers;

const
  // Simple HLSL for fullscreen pass

  // Vertex shader
  HLSL_VS: AnsiString =
    'struct VSInput {'#10 +
    '  float2 pos : POSITION;'#10 +
    '  float2 tex : TEXCOORD0;'#10 +
    '};'#10 +
    ''#10 +
    'struct PSInput {'#10 +
    '  float4 pos : SV_POSITION;'#10 +
    '  float2 tex : TEXCOORD0;'#10 +
    '};'#10 +
    ''#10 +
    'PSInput VSMain(VSInput input)'#10 +
    '{'#10 +
    '  PSInput o;'#10 +
    '  o.pos = float4(input.pos, 0.0, 1.0);'#10 +
    '  o.tex = input.tex;'#10 +
    '  return o;'#10 +
    '}'#0;

  // Pixel shader for Y plane (R8_UNORM render target)
  // Input: BGRA in [0..1]
  HLSL_PS_Y: AnsiString =
    'Texture2D TexSrc : register(t0);'#10 +
    'SamplerState Sampler0 : register(s0);'#10 +
    ''#10 +
    'struct PSInput {'#10 +
    '  float4 pos : SV_POSITION;'#10 +
    '  float2 tex : TEXCOORD0;'#10 +
    '};'#10 +
    ''#10 +
    'float PSMainY(PSInput input) : SV_Target'#10 +
    '{'#10 +
    '  float4 c = TexSrc.Sample(Sampler0, input.tex);'#10 +
    '  float B = c.b;'#10 +
    '  float G = c.g;'#10 +
    '  float R = c.r;'#10 +
    '  // BT.601 luma, full range 0..1'#10 +
    '  float Y = 0.2990 * R + 0.5870 * G + 0.1140 * B;'#10 +
    '  return saturate(Y);'#10 +
    '}'#0;

  // Pixel shader for UV plane (R8G8_UNORM half-size render target)
  // Outputs (U,V) = (Cb,Cr) in [0..1] each
  HLSL_PS_UV: AnsiString =
    'Texture2D TexSrc : register(t0);'#10 +
    'SamplerState Sampler0 : register(s0);'#10 +
    ''#10 +
    'struct PSInput {'#10 +
    '  float4 pos : SV_POSITION;'#10 +
    '  float2 tex : TEXCOORD0;'#10 +
    '};'#10 +
    ''#10 +
    'float2 PSMainUV(PSInput input) : SV_Target'#10 +
    '{'#10 +
    '  float4 c = TexSrc.Sample(Sampler0, input.tex);'#10 +
    '  float B = c.b;'#10 +
    '  float G = c.g;'#10 +
    '  float R = c.r;'#10 +
    '  // BT.601 Cb/Cr, normalized to 0..1'#10 +
    '  float Cb = -0.168736 * R - 0.331264 * G + 0.500000 * B + 0.5;'#10 +
    '  float Cr =  0.500000 * R - 0.418688 * G - 0.081312 * B + 0.5;'#10 +
    '  return float2(saturate(Cb), saturate(Cr));'#10 +
    '}'#0;

{ TGpuNV12Converter }

constructor TGpuNV12Converter.Create(const ADevice: ID3D11Device;
                                     const AContext: ID3D11DeviceContext;
                                     AWidth,
                                     AHeight: UINT);
begin

  inherited Create();

  if ADevice = nil then
    raise Exception.Create('GpuNV12Converter: Device is nil');

  FDevice := ADevice;
  FContext := AContext;
  FWidth := AWidth;
  FHeight := AHeight;

  CreateTargets();
  CreateShadersAndLayout();
  CreateGeometry();
end;


destructor TGpuNV12Converter.Destroy;
begin

  FStagingUV := nil;
  FStagingY := nil;

  FRtvUV := nil;
  FRtvY := nil;

  FTexUV := nil;
  FTexY := nil;

  FSampler := nil;
  FVB := nil;
  FIB := nil;
  FLayout := nil;
  FPS_UV := nil;
  FPS_Y := nil;
  FVS := nil;

  FContext := nil;
  FDevice := nil;

  inherited;
end;


procedure TGpuNV12Converter.CreateTargets;
var
  hr: HResult;
  texDesc: D3D11_TEXTURE2D_DESC;

begin

  ZeroMemory(@texDesc,
             SizeOf(texDesc));

  // Y target: full resolution, R8_UNORM
  texDesc.Width := FWidth;
  texDesc.Height := FHeight;
  texDesc.MipLevels := 1;
  texDesc.ArraySize := 1;
  texDesc.Format := DXGI_FORMAT_R8_UNORM;
  texDesc.SampleDesc.Count := 1;
  texDesc.SampleDesc.Quality := 0;
  texDesc.Usage := D3D11_USAGE_DEFAULT;
  texDesc.BindFlags := D3D11_BIND_RENDER_TARGET or D3D11_BIND_SHADER_RESOURCE;
  texDesc.CPUAccessFlags := 0;
  texDesc.MiscFlags := 0;

  hr := FDevice.CreateTexture2D(texDesc,
                                nil,
                                @FTexY);
  CheckHR(hr, 'GpuNV12: CreateTexture2D Y');

  hr := FDevice.CreateRenderTargetView(FTexY,
                                       nil,
                                       @FRtvY);
  CheckHR(hr, 'GpuNV12: Create RTV Y');

  // UV target: half resolution, R8G8_UNORM
  texDesc.Width := FWidth div 2;
  texDesc.Height := FHeight div 2;
  texDesc.Format := DXGI_FORMAT_R8G8_UNORM;

  hr := FDevice.CreateTexture2D(texDesc,
                                nil,
                                @FTexUV);
  CheckHR(hr, 'GpuNV12: CreateTexture2D UV');

  hr := FDevice.CreateRenderTargetView(FTexUV,
                                       nil,
                                       @FRtvUV);
  CheckHR(hr, 'GpuNV12: Create RTV UV');

  // Staging textures for CPU read-back (Y & UV)
  ZeroMemory(@texDesc,
             SizeOf(texDesc));

  texDesc.Width := FWidth;
  texDesc.Height := FHeight;
  texDesc.MipLevels := 1;
  texDesc.ArraySize := 1;
  texDesc.Format := DXGI_FORMAT_R8_UNORM;
  texDesc.SampleDesc.Count := 1;
  texDesc.SampleDesc.Quality := 0;
  texDesc.Usage := D3D11_USAGE_STAGING;
  texDesc.BindFlags := 0;
  texDesc.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
  texDesc.MiscFlags := 0;

  hr := FDevice.CreateTexture2D(texDesc,
                                nil,
                                @FStagingY);
  CheckHR(hr, 'GpuNV12: CreateTexture2D StagingY');

  ZeroMemory(@texDesc,
             SizeOf(texDesc));

  texDesc.Width := FWidth div 2;
  texDesc.Height := FHeight div 2;
  texDesc.MipLevels := 1;
  texDesc.ArraySize := 1;
  texDesc.Format := DXGI_FORMAT_R8G8_UNORM;
  texDesc.SampleDesc.Count := 1;
  texDesc.SampleDesc.Quality := 0;
  texDesc.Usage := D3D11_USAGE_STAGING;
  texDesc.BindFlags := 0;
  texDesc.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
  texDesc.MiscFlags := 0;

  hr := FDevice.CreateTexture2D(texDesc,
                                nil,
                                @FStagingUV);
  CheckHR(hr, 'GpuNV12: CreateTexture2D StagingUV');
end;


procedure TGpuNV12Converter.CreateShadersAndLayout;
var
  hr: HResult;
  vsBlob: ID3DBlob;
  psBlobY: ID3DBlob;
  psBlobUV: ID3DBlob;
  inputElems: array[0..1] of D3D11_INPUT_ELEMENT_DESC;
  sampDesc: D3D11_SAMPLER_DESC;

begin

  // Compile & create vertex shader
  hr := D3DCompile(PAnsiChar(HLSL_VS),
                   Length(HLSL_VS),
                   'VS',
                   nil,
                   nil,
                   'VSMain',
                   'vs_4_0',
                   0,
                   0,
                   vsBlob,
                   nil);
  CheckHR(hr, 'GpuNV12: Compile VS');

  hr := FDevice.CreateVertexShader(vsBlob.GetBufferPointer,
                                   vsBlob.GetBufferSize,
                                   nil,
                                   @FVS);
  CheckHR(hr, 'GpuNV12: Create VS');

  // Compile & create pixel shader for Y
  hr := D3DCompile(PAnsiChar(HLSL_PS_Y),
                   Length(HLSL_PS_Y),
                   'PSY',
                   nil,
                   nil,
                   'PSMainY',
                   'ps_4_0',
                   0,
                   0,
                   psBlobY,
                   nil);
  CheckHR(hr, 'GpuNV12: Compile PS_Y');

  hr := FDevice.CreatePixelShader(psBlobY.GetBufferPointer,
                                  psBlobY.GetBufferSize,
                                  nil,
                                  @FPS_Y);
  CheckHR(hr, 'GpuNV12: Create PS_Y');

  // Compile & create pixel shader for UV
  hr := D3DCompile(PAnsiChar(HLSL_PS_UV),
                   Length(HLSL_PS_UV),
                   'PSUV',
                   nil,
                   nil,
                   'PSMainUV',
                   'ps_4_0',
                   0,
                   0,
                   psBlobUV,
                   nil);
  CheckHR(hr, 'GpuNV12: Compile PS_UV');

  hr := FDevice.CreatePixelShader(psBlobUV.GetBufferPointer,
                                  psBlobUV.GetBufferSize,
                                  nil,
                                  @FPS_UV);
  CheckHR(hr, 'GpuNV12: Create PS_UV');

  // Input layout: POSITION (float2) + TEXCOORD (float2)
  ZeroMemory(@inputElems, SizeOf(inputElems));

  inputElems[0].SemanticName := 'POSITION';
  inputElems[0].SemanticIndex := 0;
  inputElems[0].Format := DXGI_FORMAT_R32G32_FLOAT;
  inputElems[0].InputSlot := 0;
  inputElems[0].AlignedByteOffset := 0;
  inputElems[0].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
  inputElems[0].InstanceDataStepRate := 0;

  inputElems[1].SemanticName := 'TEXCOORD';
  inputElems[1].SemanticIndex := 0;
  inputElems[1].Format := DXGI_FORMAT_R32G32_FLOAT;
  inputElems[1].InputSlot := 0;
  inputElems[1].AlignedByteOffset := SizeOf(Single) * 2;
  inputElems[1].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
  inputElems[1].InstanceDataStepRate := 0;

  hr := FDevice.CreateInputLayout(@inputElems[0],
                                  Length(inputElems),
                                  vsBlob.GetBufferPointer,
                                  vsBlob.GetBufferSize,
                                  @FLayout);
  CheckHR(hr, 'GpuNV12: CreateInputLayout');

  // Sampler: simple linear clamp
  ZeroMemory(@sampDesc, SizeOf(sampDesc));
  sampDesc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
  sampDesc.AddressU := D3D11_TEXTURE_ADDRESS_CLAMP;
  sampDesc.AddressV := D3D11_TEXTURE_ADDRESS_CLAMP;
  sampDesc.AddressW := D3D11_TEXTURE_ADDRESS_CLAMP;
  sampDesc.ComparisonFunc := D3D11_COMPARISON_ALWAYS;
  sampDesc.MinLOD := 0;
  sampDesc.MaxLOD := D3D11_FLOAT32_MAX;

  hr := FDevice.CreateSamplerState(sampDesc, @FSampler);
  CheckHR(hr, 'GpuNV12: CreateSamplerState');
end;


// Full-screen quad geometry: 4 vertices, triangle strip
procedure TGpuNV12Converter.CreateGeometry;
var
  hr: HResult;
  quad: array[0..3] of TVertex;
  vbDesc: D3D11_BUFFER_DESC;
  vbData: D3D11_SUBRESOURCE_DATA;
  indices: array[0..3] of Word;
  ibDesc: D3D11_BUFFER_DESC;
  ibData: D3D11_SUBRESOURCE_DATA;

begin
  // Full-screen quad in NDC with standard UVs
  quad[0].Pos[0] := -1.0; quad[0].Pos[1] := -1.0;  // bottom-left
  quad[0].Tex[0] :=  0.0; quad[0].Tex[1] :=  1.0;

  quad[1].Pos[0] := -1.0; quad[1].Pos[1] :=  1.0;  // top-left
  quad[1].Tex[0] :=  0.0; quad[1].Tex[1] :=  0.0;

  quad[2].Pos[0] :=  1.0; quad[2].Pos[1] := -1.0;  // bottom-right
  quad[2].Tex[0] :=  1.0; quad[2].Tex[1] :=  1.0;

  quad[3].Pos[0] :=  1.0; quad[3].Pos[1] :=  1.0;  // top-right
  quad[3].Tex[0] :=  1.0; quad[3].Tex[1] :=  0.0;

  // Immutable vertex buffer for the quad
  ZeroMemory(@vbDesc,
             SizeOf(vbDesc));

  vbDesc.Usage := D3D11_USAGE_IMMUTABLE;
  vbDesc.ByteWidth := SizeOf(quad); // 4 * SizeOf(TVertex)
  vbDesc.BindFlags := D3D11_BIND_VERTEX_BUFFER;
  vbDesc.CPUAccessFlags := 0;
  vbDesc.MiscFlags := 0;

  ZeroMemory(@vbData, SizeOf(vbData));
  vbData.pSysMem := @quad[0];

  hr := FDevice.CreateBuffer(vbDesc,
                             @vbData,
                             @FVB);
  CheckHR(hr, 'GpuNV12: CreateBuffer VB');

  // Immutable index buffer: triangle strip 0-1-2-3
  indices[0] := 0;
  indices[1] := 1;
  indices[2] := 2;
  indices[3] := 3;

  ZeroMemory(@ibDesc,
             SizeOf(ibDesc));

  ibDesc.Usage := D3D11_USAGE_IMMUTABLE;
  ibDesc.ByteWidth := SizeOf(indices);
  ibDesc.BindFlags := D3D11_BIND_INDEX_BUFFER;
  ibDesc.CPUAccessFlags := 0;
  ibDesc.MiscFlags := 0;
  ibDesc.StructureByteStride := 0;

  ZeroMemory(@ibData,
             SizeOf(ibData));

  ibData.pSysMem := @indices[0];

  hr := FDevice.CreateBuffer(ibDesc,
                             @ibData,
                             @FIB);
  CheckHR(hr, 'GpuNV12: CreateBuffer IB');
end;


// Do the GPU passes: BGRA -> Y & UV render targets
procedure TGpuNV12Converter.RenderToYUV(const SrcBGRA: ID3D11Texture2D);
var
  hr: HResult;
  ctx: ID3D11DeviceContext;
  srv: ID3D11ShaderResourceView;
  srvDesc: D3D11_SHADER_RESOURCE_VIEW_DESC;
  stride: UINT;
  offset: UINT;
  vp: D3D11_VIEWPORT;
  nullRTV: ID3D11RenderTargetView;
  nullSRV: ID3D11ShaderResourceView;

begin

  if (FDevice = nil) or
     (SrcBGRA = nil) then
    Exit;

  FDevice.GetImmediateContext(ctx);

  // Create SRV for source BGRA (desktop duplication is usually B8G8R8A8_UNORM)
  ZeroMemory(@srvDesc,
             SizeOf(srvDesc));

  srvDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  srvDesc.ViewDimension := D3D11_SRV_DIMENSION_TEXTURE2D;
  srvDesc.Texture2D.MostDetailedMip := 0;
  srvDesc.Texture2D.MipLevels := 1;

  hr := FDevice.CreateShaderResourceView(SrcBGRA,
                                         @srvDesc,
                                         @srv);
  CheckHr(hr, 'GpuNV12: CreateShaderResourceView');  //$80070057 =  E_INVALIDARG

  // Common IA setup
  stride := SizeOf(TVertex);
  offset := 0;

  ctx.IASetInputLayout(FLayout);
  ctx.IASetVertexBuffers(0,
                         1,
                         @FVB,
                         @stride,
                         @offset);

  // Bind index buffer for triangle strip quad
  ctx.IASetIndexBuffer(FIB,
                       DXGI_FORMAT_R16_UINT,
                       0);

  ctx.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);

  // Bind common VS, sampler & SRV
  ctx.VSSetShader(FVS, nil, 0);

  ctx.PSSetSamplers(0,
                    1,
                    @FSampler);

  ctx.PSSetShaderResources(0,
                           1,
                           @srv);

  // ---------- Pass 1: Y ------------------------------------------------------
  ZeroMemory(@vp,
             SizeOf(vp));

  vp.TopLeftX := 0;
  vp.TopLeftY := 0;
  vp.Width := FWidth;
  vp.Height := FHeight;
  vp.MinDepth := 0.0;
  vp.MaxDepth := 1.0;

  ctx.RSSetViewports(1,
                     @vp);

  ctx.OMSetRenderTargets(1,
                         @FRtvY,
                         nil);

  ctx.PSSetShader(FPS_Y,
                  nil,
                  0);

  ctx.DrawIndexed(4, 0, 0);

  // ---------- Pass 2: UV -----------------------------------------------------
  ZeroMemory(@vp,
             SizeOf(vp));

  vp.TopLeftX := 0;
  vp.TopLeftY := 0;
  vp.Width := FWidth div 2;   // half width
  vp.Height := FHeight div 2; // half height
  vp.MinDepth := 0.0;
  vp.MaxDepth := 1.0;

  ctx.RSSetViewports(1,
                     @vp);

  ctx.OMSetRenderTargets(1,
                         @FRtvUV,
                         nil);

  ctx.PSSetShader(FPS_UV,
                  nil,
                  0);

  ctx.DrawIndexed(4,
                  0,
                  0);

  // Unbind SRV/RTVs to avoid binding conflicts later
  nullRTV := nil;

  ctx.OMSetRenderTargets(1,
                         @nullRTV,
                         nil);

  nullSRV := nil;
  ctx.PSSetShaderResources(0,
                           1,
                           @nullSRV);

  srv := nil;
end;


// Read back Y/UV from staging textures and pack into NV12 layout
procedure TGpuNV12Converter.DownloadYUVAndPackNV12(const NV12: D3D11_MAPPED_SUBRESOURCE);
var
  ctx: ID3D11DeviceContext;
  hr: HResult;
  mapY: D3D11_MAPPED_SUBRESOURCE;
  mapUV: D3D11_MAPPED_SUBRESOURCE;
  yDst: PByte;
  uvDst: PByte;
  ySrc: PByte;
  uvSrc: PByte;
  yPitchSrc: UINT;
  uvPitchSrc: UINT;
  yPitchDst: UINT;
  uvPitchDst: UINT;
  row: UINT;
  srcRowSize: UINT;
  srcUVRowSize: UINT;

begin

  if (FDevice = nil) then
    Exit;

  FDevice.GetImmediateContext(ctx);

  // Copy GPU render targets into CPU staging textures
  ctx.CopyResource(FStagingY,
                   FTexY);

  ctx.CopyResource(FStagingUV,
                   FTexUV);

  // Map staging Y
  ZeroMemory(@mapY, SizeOf(mapY));
  hr := ctx.Map(FStagingY,
                0,
                D3D11_MAP_READ,
                0,
                mapY);
  CheckHR(hr, 'GpuNV12: Map(FStagingY)');

  try
    // Map staging UV
    ZeroMemory(@mapUV,
               SizeOf(mapUV));

    hr := ctx.Map(FStagingUV,
                  0,
                  D3D11_MAP_READ,
                  0,
                  mapUV);
    CheckHR(hr, 'GpuNV12: Map(FStagingUV)');

    try
      // Source pitches
      yPitchSrc := mapY.RowPitch;
      uvPitchSrc := mapUV.RowPitch;

      // Destination NV12 layout
      yPitchDst := NV12.RowPitch;
      uvPitchDst := NV12.RowPitch;

      ySrc := PByte(mapY.pData);
      yDst := PByte(NV12.pData);

      srcRowSize := FWidth; // Y: 1 byte per pixel

      // Copy Y plane line by line
      for row := 0 to FHeight - 1 do
        begin
          Move(ySrc^,
               yDst^,
               srcRowSize);

          Inc(ySrc,
              yPitchSrc);

          Inc(yDst,
              yPitchDst);
        end;

      // UV plane: FHeight/2 rows, Width bytes each (interleaved U,V)
      uvSrc := PByte(mapUV.pData);
      uvDst := PByte(NV12.pData);
      Inc(uvDst,
          yPitchDst * FHeight); // UV plane starts after all Y rows

      srcUVRowSize := FWidth; // each row: (Width/2)*2

      for row := 0 to (FHeight div 2) - 1 do
        begin
          Move(uvSrc^,
               uvDst^,
               srcUVRowSize);

          Inc(uvSrc,
              uvPitchSrc);

          Inc(uvDst,
              uvPitchDst);
        end;
    finally
      ctx.Unmap(FStagingUV, 0);
    end;
  finally
    ctx.Unmap(FStagingY, 0);
  end;
end;


procedure TGpuNV12Converter.Convert(const SrcBGRA: ID3D11Texture2D;
                                    const DestNV12: D3D11_MAPPED_SUBRESOURCE);
begin

  if (FDevice = nil) or
     (SrcBGRA = nil) then
    Exit;

  // 1) Render BGRA -> Y/UV GPU render targets
  RenderToYUV(SrcBGRA);

  // 2) Read back Y/UV and pack into NV12 mapped staging buffer
  DownloadYUVAndPackNV12(DestNV12);
end;

end.

