// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11_3.pas
// Kind: Pascal / Delphi unit
// Release date: 31-08-2022
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description: Contains interface definitions for the D3D11.3 API.
//              Microsoft DirectX D3D11 used by Media Foundation.
//              You can use Direct3D 11 graphics to create 3-D graphics for games,
//              scientific and desktop apps.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 31/08/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks:  Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: D3D11_3.h
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
unit WinApi.DirectX.D3D11_3;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Types,
  {DirectX}
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.D3DCommon,
  WinApi.DirectX.D3D11,
  WinApi.DirectX.D3D11_1,
  WinApi.DirectX.D3D11_2,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI1_3;

  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type

  PD3D11_CONTEXT_TYPE = ^D3D11_CONTEXT_TYPE;
  D3D11_CONTEXT_TYPE           = (
    D3D11_CONTEXT_TYPE_ALL     = 0,
    D3D11_CONTEXT_TYPE_3D      = 1,
    D3D11_CONTEXT_TYPE_COMPUTE = 2,
    D3D11_CONTEXT_TYPE_COPY    = 3,
    D3D11_CONTEXT_TYPE_VIDEO   = 4
  );
  {$EXTERNALSYM D3D11_CONTEXT_TYPE}

  PD3D11_TEXTURE_LAYOUT = ^D3D11_TEXTURE_LAYOUT;
  D3D11_TEXTURE_LAYOUT                        = (
    D3D11_TEXTURE_LAYOUT_UNDEFINED            = 0,
    D3D11_TEXTURE_LAYOUT_ROW_MAJOR            = 1,
    D3D11_TEXTURE_LAYOUT_64K_STANDARD_SWIZZLE = 2
  );
  {$EXTERNALSYM D3D11_TEXTURE_LAYOUT}


 ///////////////////////////////////////////////////////////////////////////////
 //
 // Texture2D
 //
 ///////////////////////////////////////////////////////////////////////////////

type

  D3D11_TEXTURE2D_DESC1 = record
    Width: UINT;
    Height: UINT;
    MipLevels: UINT;
    ArraySize: UINT;
    Format: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
    Usage: D3D11_USAGE;
    BindFlags: UINT;
    CPUAccessFlags: UINT;
    MiscFlags: UINT;
    TextureLayout: D3D11_TEXTURE_LAYOUT;

    public
      constructor Create(const o: D3D11_TEXTURE2D_DESC1); overload;

      constructor Create(aFormat: DXGI_FORMAT;
                         aWidth: UINT;
                         aHeight: UINT;
                         aArraySize: UINT = 1;
                         aMipLevels: UINT = 0;
                         aBindFlags: UINT = D3D11_BIND_SHADER_RESOURCE;
                         aUsage: D3D11_USAGE = D3D11_USAGE_DEFAULT;
                         acpuaccessFlags: UINT = 0;
                         aSampleCount: UINT = 1;
                         aSampleQuality: UINT = 0;
                         aMiscFlags: UINT = 0;
                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED); overload;

     constructor Create(aDesc: D3D11_TEXTURE2D_DESC;
                        aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED); overload;
  end;
  {$EXTERNALSYM D3D11_TEXTURE2D_DESC1}


  // Interface ID3D11Texture2D1
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Texture2D1);'}
  {$EXTERNALSYM ID3D11Texture2D1}
  ID3D11Texture2D1 = interface(ID3D11Texture2D)
    ['{51218251-1E33-4617-9CCB-4D3A4367E7BB}']

    procedure GetDesc1(out pDesc: D3D11_TEXTURE2D_DESC1); stdcall;

  end;
  IID_ID3D11Texture2D1 = ID3D11Texture2D1;
  {$EXTERNALSYM IID_ID3D11Texture2D1}



  //////////////////////////////////////////////////////////////////////////////
  //
  // Texture3D
  //
  //////////////////////////////////////////////////////////////////////////////


  PD3D11_TEXTURE3D_DESC1 = ^D3D11_TEXTURE3D_DESC1;
  D3D11_TEXTURE3D_DESC1 = record
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    MipLevels: UINT;
    Format: DXGI_FORMAT;
    Usage: D3D11_USAGE;
    BindFlags: UINT;
    CPUAccessFlags: UINT;
    MiscFlags: UINT;
    TextureLayout: D3D11_TEXTURE_LAYOUT;

    public
      constructor Create(const o: D3D11_TEXTURE3D_DESC1); overload;

      constructor Create(aFormat: DXGI_FORMAT;
                         aWidth: UINT;
                         aHeight: UINT;
                         aDepth: UINT;
                         aMipLevels: UINT = 0;
                         aBindFlags: UINT = D3D11_BIND_SHADER_RESOURCE;
                         aUsage: D3D11_USAGE = D3D11_USAGE_DEFAULT;
                         aCpuAccessFlags: UINT = 0;
                         aMiscFlags: UINT = 0;
                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED); overload;

      constructor Create(aDesc: D3D11_TEXTURE3D_DESC;
                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED); overload;

  end;
  {$EXTERNALSYM D3D11_TEXTURE3D_DESC1}


  // Interface ID3D11Texture3D1
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Texture3D1);'}
  {$EXTERNALSYM ID3D11Texture3D1}
  ID3D11Texture3D1 = interface(ID3D11Texture3D)
    ['{0C711683-2853-4846-9BB0-F3E60639E46A}']

    procedure GetDesc1(out pDesc: D3D11_TEXTURE3D_DESC1); stdcall;

  end;
  IID_ID3D11Texture3D1 = ID3D11Texture3D1;
  {$EXTERNALSYM IID_ID3D11Texture3D1}



  //////////////////////////////////////////////////////////////////////////////
  //
  // Rasterizer State
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_CONSERVATIVE_RASTERIZATION_MODE = ^D3D11_CONSERVATIVE_RASTERIZATION_MODE;
  D3D11_CONSERVATIVE_RASTERIZATION_MODE       = (
    D3D11_CONSERVATIVE_RASTERIZATION_MODE_OFF = 0,  // Default
    D3D11_CONSERVATIVE_RASTERIZATION_MODE_ON  = 1
  );
  {$EXTERNALSYM D3D11_CONSERVATIVE_RASTERIZATION_MODE}

  D3D11_RASTERIZER_DESC2 = record
    FillMode: D3D11_FILL_MODE;
    CullMode: D3D11_CULL_MODE;
    FrontCounterClockwise: BOOL;
    DepthBias: INT;
    DepthBiasClamp: FLOAT;
    SlopeScaledDepthBias: FLOAT;
    DepthClipEnable: BOOL;
    ScissorEnable: BOOL;
    MultisampleEnable: BOOL;
    AntialiasedLineEnable: BOOL;
    ForcedSampleCount: UINT;
    ConservativeRaster: D3D11_CONSERVATIVE_RASTERIZATION_MODE;

    public
      constructor Create(const o: D3D11_RASTERIZER_DESC2); overload;

      constructor Create(Default: Boolean); overload;

      constructor Create(aFillMode: D3D11_FILL_MODE;
                         aCullMode: D3D11_CULL_MODE;
                         aFrontCounterClockwise: BOOL;
                         aDepthBias: INT;
                         aDepthBiasClamp: FLOAT;
                         aSlopeScaledDepthBias: FLOAT;
                         aDepthClipEnable: BOOL;
                         aScissorEnable: BOOL;
                         aMultisampleEnable: BOOL;
                         aAntialiasedLineEnable: BOOL;
                         aForcedSampleCount: UINT;
                         aConservativeRaster: D3D11_CONSERVATIVE_RASTERIZATION_MODE); overload;

  end;
  {$EXTERNALSYM D3D11_RASTERIZER_DESC2}


  // Interface ID3D11RasterizerState2
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11RasterizerState2);'}
  {$EXTERNALSYM ID3D11RasterizerState2}
  ID3D11RasterizerState2 = interface(ID3D11RasterizerState1)
    ['{6fbd02fb-209f-46c4-b059-2ed15586a6ac}']

    procedure GetDesc2(out pDesc: D3D11_RASTERIZER_DESC2); stdcall;

  end;
  IID_ID3D11RasterizerState2 = ID3D11RasterizerState2;
  {$EXTERNALSYM IID_ID3D11RasterizerState2}


  //////////////////////////////////////////////////////////////////////////////
  //
  // ShaderResourceView
  //
  //////////////////////////////////////////////////////////////////////////////

  D3D11_TEX2D_SRV1 = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_SRV1}

  PD3D11_TEX2D_ARRAY_SRV1 = ^D3D11_TEX2D_ARRAY_SRV1;
  D3D11_TEX2D_ARRAY_SRV1 = record
    MostDetailedMip: UINT;
    MipLevels: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_ARRAY_SRV1}

  D3D11_SHADER_RESOURCE_VIEW_DESC1 = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D11_SRV_DIMENSION;

    public
      constructor Create(const o: D3D11_SHADER_RESOURCE_VIEW_DESC1); overload;

      constructor Create(aViewDimension: D3D11_SRV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMostDetailedMip: UINT = 0; // FirstElement for BUFFER
                         aMipLevels: UINT = UINT(-1); // NumElements for BUFFER
                         aFirstArraySlice: UINT = 0; // First2DArrayFace for TEXTURECUBEARRAY
                         aArraySize: UINT = UINT(-1); // NumCubes for TEXTURECUBEARRAY
                         aFlags: UINT = 0; // BUFFEREX only
                         aPlaneSlice: UINT = 0 {Texture2D and Texture2DArray only}); overload;

      constructor Create(const aBuffer: ID3D11Buffer;
                         aFormat: DXGI_FORMAT;
                         aFirstElement: UINT;
                         aNumElements: UINT;
                         aFlags: UINT = 0); overload;

      constructor Create(const pTex1D: ID3D11Texture1D;
                         aViewDimension: D3D11_SRV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMostDetailedMip: UINT = 0;
                         aMipLevels: UINT = UINT(-1);
                         afirstArraySlice: UINT = 0;
                         aArraySize: UINT = UINT(-1)); overload;

      constructor Create(const pTex2D: ID3D11Texture2D;
                         aViewDimension: D3D11_SRV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMostDetailedMip: UINT = 0;
                         aMipLevels: UINT = UINT(-1);
                         aFirstArraySlice: UINT = 0; // First2DArrayFace for TEXTURECUBEARRAY
                         aArraySize: UINT = UINT(-1);  // NumCubes for TEXTURECUBEARRAY
                         aPlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D or TEXTURE2DARRAY}); overload;

      constructor Create(const pTex3D: ID3D11Texture3D;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMostDetailedMip: UINT = 0;
                         aMipLevels: UINT = UINT(-1)); overload;

    {union}
    case Byte of
      0: (Buffer: D3D11_BUFFER_SRV);
      1: (Texture1D: D3D11_TEX1D_SRV);
      2: (Texture1DArray: D3D11_TEX1D_ARRAY_SRV);
      3: (Texture2D: D3D11_TEX2D_SRV1);
      4: (Texture2DArray: D3D11_TEX2D_ARRAY_SRV1);
      5: (Texture2DMS: D3D11_TEX2DMS_SRV);
      6: (Texture2DMSArray: D3D11_TEX2DMS_ARRAY_SRV);
      7: (Texture3D: D3D11_TEX3D_SRV);
      8: (TextureCube: D3D11_TEXCUBE_SRV);
      9: (TextureCubeArray: D3D11_TEXCUBE_ARRAY_SRV);
      10: (BufferEx: D3D11_BUFFEREX_SRV);
  end;
  {$EXTERNALSYM D3D11_SHADER_RESOURCE_VIEW_DESC1}


  // Interface ID3D11ShaderResourceView1
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11ShaderResourceView1);'}
  {$EXTERNALSYM ID3D11ShaderResourceView1}
  ID3D11ShaderResourceView1 = interface(ID3D11ShaderResourceView)
    ['{91308b87-9040-411d-8c67-c39253ce3802}']

    procedure GetDesc1(out pDesc1: D3D11_SHADER_RESOURCE_VIEW_DESC1);

  end;
  IID_ID3D11ShaderResourceView1 = ID3D11ShaderResourceView1;
  {$EXTERNALSYM IID_ID3D11ShaderResourceView1}


  //////////////////////////////////////////////////////////////////////////////
  //
  // RenderTargetView
  //
  //////////////////////////////////////////////////////////////////////////////


  PD3D11_TEX2D_RTV1 = ^D3D11_TEX2D_RTV1;
  D3D11_TEX2D_RTV1 = record
    MipSlice: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_RTV1}

  PD3D11_TEX2D_ARRAY_RTV1 = ^D3D11_TEX2D_ARRAY_RTV1;
  D3D11_TEX2D_ARRAY_RTV1 = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_ARRAY_RTV1}


  D3D11_RENDER_TARGET_VIEW_DESC1 = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D11_RTV_DIMENSION;

    public
      constructor Create(const o: D3D11_RENDER_TARGET_VIEW_DESC1); overload;

      constructor Create(aViewDimension: D3D11_RTV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0; // FirstElement for BUFFER
                         aFirstArraySlice: UINT = 0; // NumElements for BUFFER, FirstWSlice for TEXTURE3D
                         aArraySize: UINT = UINT(-1); // WSize for TEXTURE3D
                         aPlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D and TEXTURE2DARRAY}); overload;

      constructor Create(const aBuffer: ID3D11Buffer;
                         aFormat: DXGI_FORMAT;
                         aFirstElement: UINT;
                         aNumElements: UINT); overload;

      constructor Create(const pTex1D: ID3D11Texture1D;
                         aViewDimension: D3D11_SRV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstArraySlice: UINT = 0;
                         aArraySize: UINT = UINT(-1)); overload;

      constructor Create(const pTex2D: ID3D11Texture2D;
                         aViewDimension: D3D11_RTV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstArraySlice: UINT = 0;
                         aArraySize: UINT = UINT(-1);
                         aPlaneSlice: UINT = 0); overload;

      constructor Create(const pTex3D: ID3D11Texture3D;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstWSlice: UINT = 0;
                         aWSize: UINT = UINT(-1)); overload;

    {union}
    case Byte of
      0: (Buffer: D3D11_BUFFER_RTV);
      1: (Texture1D: D3D11_TEX1D_RTV);
      2: (Texture1DArray: D3D11_TEX1D_ARRAY_RTV);
      3: (Texture2D: D3D11_TEX2D_RTV1);
      4: (Texture2DArray: D3D11_TEX2D_ARRAY_RTV1);
      5: (Texture2DMS: D3D11_TEX2DMS_RTV);
      6: (Texture2DMSArray: D3D11_TEX2DMS_ARRAY_RTV);
      7: (Texture3D: D3D11_TEX3D_RTV);
  end;
  {$EXTERNALSYM D3D11_RENDER_TARGET_VIEW_DESC1}


  // Interface ID3D11RenderTargetView1
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11RenderTargetView1);'}
  {$EXTERNALSYM ID3D11RenderTargetView1}
  PID3D11RenderTargetView1 = ^ID3D11RenderTargetView1;
  ID3D11RenderTargetView1 = interface(ID3D11RenderTargetView)
  ['{ffbe2e23-f011-418a-ac56-5ceed7c5b94b}']

    procedure GetDesc(out pDesc1: D3D11_RENDER_TARGET_VIEW_DESC1); stdcall;

  end;
  IID_ID3D11RenderTargetView1 = ID3D11RenderTargetView1;
  {$EXTERNALSYM IID_ID3D11RenderTargetView1}



  //////////////////////////////////////////////////////////////////////////////
  //
  // UnorderedAccessView
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_TEX2D_UAV1 = ^D3D11_TEX2D_UAV1;
  D3D11_TEX2D_UAV1 = record
    MipSlice: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_UAV1}

  PD3D11_TEX2D_ARRAY_UAV1 = ^D3D11_TEX2D_ARRAY_UAV1;
  D3D11_TEX2D_ARRAY_UAV1 = record
    MipSlice: UINT;
    FirstArraySlice: UINT;
    ArraySize: UINT;
    PlaneSlice: UINT;
  end;
  {$EXTERNALSYM D3D11_TEX2D_ARRAY_UAV1}


  D3D11_UNORDERED_ACCESS_VIEW_DESC1 = record
    Format: DXGI_FORMAT;
    ViewDimension: D3D11_UAV_DIMENSION;

    public
      constructor Create(const o: D3D11_UNORDERED_ACCESS_VIEW_DESC1); overload;

      constructor Create(aViewDimension: D3D11_RTV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0; // FirstElement for BUFFER
                         aFirstArraySlice: UINT = 0; // NumElements for BUFFER, FirstWSlice for TEXTURE3D
                         aArraySize: UINT = UINT(-1); // WSize for TEXTURE3D
                         APlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D and TEXTURE2DARRAY}); overload;

      constructor Create(const aBuffer: ID3D11Buffer;
                         aFormat: DXGI_FORMAT;
                         aFirstElement: UINT;
                         aNumElements: UINT); overload;

      constructor Create(const pTex1D: ID3D11Texture1D;
                         aViewDimension: D3D11_RTV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstArraySlice: UINT = 0;
                         aArraySize: UINT = UINT(-1)); overload;

      constructor Create(const pTex2D: ID3D11Texture2D;
                         aViewDimension: D3D11_UAV_DIMENSION;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstArraySlice: UINT = 0;
                         aArraySize: UINT = UINT(-1);
                         aPlaneSlice: UINT = 0); overload;

      constructor Create(const pTex3D: ID3D11Texture3D;
                         aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                         aMipSlice: UINT = 0;
                         aFirstWSlice: UINT = 0;
                         aWSize: UINT = UINT(-1)); overload;

    {union}
    case Byte of
      0:  (Buffer: D3D11_BUFFER_UAV);
      1:  (Texture1D: D3D11_TEX1D_UAV);
      2:  (Texture1DArray: D3D11_TEX1D_ARRAY_UAV);
      3:  (Texture2D: D3D11_TEX2D_UAV1);
      4:  (Texture2DArray: D3D11_TEX2D_ARRAY_UAV1);
      5:  (Texture3D: D3D11_TEX3D_UAV);
  end;
  {$EXTERNALSYM D3D11_UNORDERED_ACCESS_VIEW_DESC1}


  // Interface ID3D11UnorderedAccessView1
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11UnorderedAccessView1);'}
  {$EXTERNALSYM ID3D11UnorderedAccessView1}
  PID3D11UnorderedAccessView1 = ^ID3D11SamplerState;
  ID3D11UnorderedAccessView1 = interface(ID3D11UnorderedAccessView)
  ['{7b3b6153-a886-4544-ab37-6537c8500403}']

    procedure GetDesc1(out pDesc1: D3D11_UNORDERED_ACCESS_VIEW_DESC1); stdcall;

  end;
  IID_ID3D11UnorderedAccessView1 = ID3D11UnorderedAccessView1;
  {$EXTERNALSYM IID_ID3D11UnorderedAccessView1}


  //////////////////////////////////////////////////////////////////////////////
  //
  // Query
  //
  //////////////////////////////////////////////////////////////////////////////

  D3D11_QUERY_DESC1 = record
    Query: D3D11_QUERY;
    MiscFlags: UINT;
    ContextType: D3D11_CONTEXT_TYPE;

    public
      constructor Create(const o: D3D11_QUERY_DESC1); overload;

      constructor Create(aQuery: D3D11_QUERY;
                         aMiscFlags: UINT = 0;
                         aContextType: D3D11_CONTEXT_TYPE = D3D11_CONTEXT_TYPE_ALL); overload;

  end;
  {$EXTERNALSYM D3D11_QUERY_DESC1}


  // Interface ID3D11Query1
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Query1);'}
  {$EXTERNALSYM ID3D11Query1}
  ID3D11Query1 = interface(ID3D11Query)
  ['{631b4766-36dc-461d-8db6-c47e13e60916}']

    procedure GetDesc1(out pDesc1: D3D11_QUERY_DESC1); stdcall;

  end;
  IID_ID3D11Query1 = ID3D11Query1;
  {$EXTERNALSYM IID_ID3D11Query1}


  PD3D11_FENCE_FLAG = ^D3D11_FENCE_FLAG;
  D3D11_FENCE_FLAG                        = (
    D3D11_FENCE_FLAG_NONE                 = $0,
    D3D11_FENCE_FLAG_SHARED               = $2,
    D3D11_FENCE_FLAG_SHARED_CROSS_ADAPTER = $4,
    D3D11_FENCE_FLAG_NON_MONITORED        = $8
  );
  {$EXTERNALSYM D3D11_FENCE_FLAG}

{$DEFINE DEFINE_ENUM_FLAG_OPERATORS(D3D11_FENCE_FLAG)}



  //////////////////////////////////////////////////////////////////////////////
  //
  // DeviceContext3
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11DeviceContext3
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11DeviceContext3);'}
  {$EXTERNALSYM ID3D11DeviceContext3}
  ID3D11DeviceContext3 = interface(ID3D11DeviceContext2)
  ['{b4e3c01d-e79e-4637-91b2-510e9f4c9b8f}']

    procedure Flush1(ContextType: D3D11_CONTEXT_TYPE;
                     hEvent: THandle); stdcall;

    procedure SetHardwareProtectionState(HwProtectionEnable: BOOL); stdcall;

    procedure GetHardwareProtectionState(out pHwProtectionEnable: BOOL); stdcall;

  end;
  IID_ID3D11DeviceContext3 = ID3D11DeviceContext3;
  {$EXTERNALSYM IID_ID3D11DeviceContext3}


  //////////////////////////////////////////////////////////////////////////////
  //
  // ID3D11Fence
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11Fence
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Fence);'}
  {$EXTERNALSYM ID3D11Fence}
  ID3D11Fence = interface(ID3D11DeviceChild)
  ['{affde9d1-1df7-4bb7-8a34-0f46251dab80}']

    function CreateSharedHandle(pAttributes: PSECURITY_ATTRIBUTES;
                                dwAccess: DWORD;
                                lpName: PWideChar;
                                out pHandle: THandle): HRESULT; stdcall;

    function GetCompletedValue(): UINT64; stdcall;

    function SetEventOnCompletion(Value: UINT64;
                                  hEvent: THandle): HRESULT; stdcall;

  end;
  IID_ID3D11Fence = ID3D11Fence;
  {$EXTERNALSYM IID_ID3D11Fence}


  /////////////////////////////////////////////////////////////////////////////
  //
  // DeviceContext4
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11DeviceContext4
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11DeviceContext4);'}
  {$EXTERNALSYM ID3D11DeviceContext4}
  ID3D11DeviceContext4 = interface(ID3D11DeviceContext3)
  ['{917600da-f58c-4c33-98d8-3e15b390fa24}']

    function Signal(const pFence: ID3D11Fence;
                    Value: UINT64): HRESULT; stdcall;

    function Wait(const pFence: ID3D11Fence;
                  Value: UINT64): HRESULT; stdcall;

  end;
  IID_ID3D11DeviceContext4 = ID3D11DeviceContext4;
  {$EXTERNALSYM IID_ID3D11DeviceContext4}


  //////////////////////////////////////////////////////////////////////////////
  //
  // Device3
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11Device3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device3);'}
  {$EXTERNALSYM ID3D11Device3}
  ID3D11Device3 = interface(ID3D11Device2)
  ['{A05C8C37-D2C6-4732-B3A0-9CE0B0DC9AE6}']

    function CreateTexture2D1(pDesc1: D3D11_TEXTURE2D_DESC1;
                             pInitialData: D3D11_SUBRESOURCE_DATA;
                             [ref] const ppTexture2D: ID3D11Texture2D1): HRESULT; stdcall;

    function CreateTexture3D1(pDesc1: D3D11_TEXTURE3D_DESC1;
                              pInitialData: PD3D11_SUBRESOURCE_DATA;
                              [ref] const ppTexture3D: ID3D11Texture3D1): HRESULT; stdcall;

    function CreateRasterizerState2(pRasterizerDesc: D3D11_RASTERIZER_DESC2;
                                    [ref] const ppRasterizerState: ID3D11RasterizerState2): HRESULT; stdcall;

    function CreateShaderResourceView1(pResource: ID3D11Resource;
                                       pDesc1: D3D11_SHADER_RESOURCE_VIEW_DESC1;
                                       [ref] const ppSRView1: ID3D11ShaderResourceView1): HRESULT; stdcall;

    function CreateUnorderedAccessView1(pResource: ID3D11Resource;
                                        pDesc1: D3D11_UNORDERED_ACCESS_VIEW_DESC1;
                                        [ref] const ppUAView1: ID3D11UnorderedAccessView1): HRESULT; stdcall;

    function CreateRenderTargetView1(pResource: ID3D11Resource;
                                     pDesc1: D3D11_RENDER_TARGET_VIEW_DESC1;
                                     [ref] const ppRTView1: ID3D11RenderTargetView1): HRESULT; stdcall;

    function CreateQuery1(pQueryDesc1: D3D11_QUERY_DESC1;
                         [ref] const ppQuery1: ID3D11Query1): HRESULT; stdcall;

    procedure GetImmediateContext3([ref] const ppImmediateContext: ID3D11DeviceContext3); stdcall;

    function CreateDeferredContext3(ContextFlags: UINT;
                                   [ref] const ppDeferredContext: ID3D11DeviceContext3): HRESULT; stdcall;

    procedure WriteToSubresource(const pDstResource: ID3D11Resource;
                                 DstSubresource: UINT;
                                 pDstBox: PD3D11_BOX;
                                 pSrcData: Pointer;
                                 SrcRowPitch: UINT;
                                 SrcDepthPitch: UINT); stdcall;

    procedure ReadFromSubresource(out pDstData: Pointer;
                                  DstRowPitch: UINT;
                                  DstDepthPitch: UINT;
                                  const pSrcResource: PID3D11Resource;
                                  SrcSubresource: UINT;
                                  pSrcBox: PD3D11_BOX); stdcall;
  end;
  IID_ID3D11Device3 = ID3D11Device3;
  {$EXTERNALSYM IID_ID3D11Device3}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation



{D3D11_TEXTURE2D_DESC1}

constructor D3D11_TEXTURE2D_DESC1.Create(const o: D3D11_TEXTURE2D_DESC1);
begin
  Self := o;
end;


constructor D3D11_TEXTURE2D_DESC1.Create(aFormat: DXGI_FORMAT;
                                         aWidth: UINT;
                                         aHeight: UINT;
                                         aArraySize: UINT = 1;
                                         aMipLevels: UINT = 0;
                                         aBindFlags: UINT = D3D11_BIND_SHADER_RESOURCE;
                                         aUsage: D3D11_USAGE = D3D11_USAGE_DEFAULT;
                                         acpuaccessFlags: UINT = 0;
                                         aSampleCount: UINT = 1;
                                         aSampleQuality: UINT = 0;
                                         aMiscFlags: UINT = 0;
                                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED);
begin
  Width := aWidth;
  Height := aHeight;
  MipLevels := aMipLevels;
  ArraySize := aArraySize;
  Format := aFormat;
  SampleDesc.Count := aSampleCount;
  SampleDesc.Quality := aSampleQuality;
  Usage := aUsage;
  BindFlags := aBindFlags;
  CPUAccessFlags := aCpuaccessFlags;
  MiscFlags := aMiscFlags;
  TextureLayout := aTextureLayout;
end;


constructor D3D11_TEXTURE2D_DESC1.Create(aDesc: D3D11_TEXTURE2D_DESC;
                                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED);
begin
  Width := aDesc.Width;
  Height := aDesc.Height;
  MipLevels := aDesc.MipLevels;
  ArraySize := aDesc.ArraySize;
  Format := aDesc.Format;
  SampleDesc.Count := aDesc.SampleDesc.Count;
  SampleDesc.Quality := aDesc. SampleDesc.Quality;
  Usage := aDesc.Usage;
  BindFlags := aDesc.BindFlags;
  CPUAccessFlags := aDesc.CPUAccessFlags;
  MiscFlags := aDesc.MiscFlags;
  TextureLayout := aTextureLayout;
end;


{D3D11_TEXTURE3D_DESC1}

constructor D3D11_TEXTURE3D_DESC1.Create(const o: D3D11_TEXTURE3D_DESC1);
begin
  Self := o;
end;


constructor D3D11_TEXTURE3D_DESC1.Create(aFormat: DXGI_FORMAT;
                                         aWidth: UINT;
                                         aHeight: UINT;
                                         aDepth: UINT;
                                         aMipLevels: UINT = 0;
                                         aBindFlags: UINT = D3D11_BIND_SHADER_RESOURCE;
                                         aUsage: D3D11_USAGE = D3D11_USAGE_DEFAULT;
                                         aCpuAccessFlags: UINT = 0;
                                         aMiscFlags: UINT = 0;
                                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED);
begin
 Width := aWidth;
 Height := aHeight;
 Depth := aDepth;
 MipLevels := aMipLevels;
 Format := aFormat;
 Usage := aUsage;
 BindFlags := aBindFlags;
 CPUAccessFlags := aCpuAccessFlags;
 MiscFlags := aMiscFlags;
 TextureLayout := aTextureLayout;
end;


constructor D3D11_TEXTURE3D_DESC1.Create(aDesc: D3D11_TEXTURE3D_DESC;
                                         aTextureLayout: D3D11_TEXTURE_LAYOUT = D3D11_TEXTURE_LAYOUT_UNDEFINED);
begin
  Width := aDesc.Width;
  Height := aDesc.Height;
  Depth := aDesc.Depth;
  MipLevels := aDesc.MipLevels;
  Format := aDesc.Format;
  Usage := aDesc.Usage;
  BindFlags := aDesc.BindFlags;
  CPUAccessFlags := aDesc.CPUAccessFlags;
  MiscFlags := aDesc.MiscFlags;
  TextureLayout := aTextureLayout;
end;


{PD3D11_RASTERIZER_DESC2}

constructor D3D11_RASTERIZER_DESC2.Create(const o: D3D11_RASTERIZER_DESC2);
begin
  Self := o;
end;


constructor D3D11_RASTERIZER_DESC2.Create(Default: Boolean);
begin
  FillMode := D3D11_FILL_SOLID;
  CullMode := D3D11_CULL_BACK;
  FrontCounterClockwise := FALSE;
  DepthBias := D3D11_DEFAULT_DEPTH_BIAS;
  DepthBiasClamp := D3D11_DEFAULT_DEPTH_BIAS_CLAMP;
  SlopeScaledDepthBias := D3D11_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
  DepthClipEnable := TRUE;
  ScissorEnable := FALSE;
  MultisampleEnable := FALSE;
  AntialiasedLineEnable := FALSE;
  ForcedSampleCount := 0;
  ConservativeRaster := D3D11_CONSERVATIVE_RASTERIZATION_MODE_OFF;
end;


constructor D3D11_RASTERIZER_DESC2.Create(aFillMode: D3D11_FILL_MODE;
                                          aCullMode: D3D11_CULL_MODE;
                                          aFrontCounterClockwise: BOOL;
                                          aDepthBias: INT;
                                          aDepthBiasClamp: FLOAT;
                                          aSlopeScaledDepthBias: FLOAT;
                                          aDepthClipEnable: BOOL;
                                          aScissorEnable: BOOL;
                                          aMultisampleEnable: BOOL;
                                          aAntialiasedLineEnable: BOOL;
                                          aForcedSampleCount: UINT;
                                          aConservativeRaster: D3D11_CONSERVATIVE_RASTERIZATION_MODE);
begin
  FillMode := aFillMode;
  CullMode := aCullMode;
  FrontCounterClockwise := aFrontCounterClockwise;
  DepthBias := aDepthBias;
  DepthBiasClamp := aDepthBiasClamp;
  SlopeScaledDepthBias := aSlopeScaledDepthBias;
  DepthClipEnable := aDepthClipEnable;
  ScissorEnable := aScissorEnable;
  MultisampleEnable := aMultisampleEnable;
  AntialiasedLineEnable := aAntialiasedLineEnable;
  ForcedSampleCount := aForcedSampleCount;
  ConservativeRaster := aConservativeRaster;
end;


{D3D11_SHADER_RESOURCE_VIEW_DESC1}

constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(const o: D3D11_SHADER_RESOURCE_VIEW_DESC1);
begin
  Self := o;
end;


constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(aViewDimension: D3D11_SRV_DIMENSION;
                                                    aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                    aMostDetailedMip: UINT = 0; // FirstElement for BUFFER
                                                    aMipLevels: UINT = UINT(-1); // NumElements for BUFFER
                                                    aFirstArraySlice: UINT = 0; // First2DArrayFace for TEXTURECUBEARRAY
                                                    aArraySize: UINT = UINT(-1); // NumCubes for TEXTURECUBEARRAY
                                                    aFlags: UINT = 0; // BUFFEREX only
                                                    aPlaneSlice: UINT = 0 {Texture2D and Texture2DArray only});
begin
  Format := aFormat;
  ViewDimension := aViewDimension;


  case aViewDimension of

    D3D11_SRV_DIMENSION_BUFFER: begin
                                  Buffer.FirstElement := aMostDetailedMip;
                                  Buffer.NumElements := aMipLevels;
                                end;

    D3D11_SRV_DIMENSION_TEXTURE1D: begin
                                     Texture1D.MostDetailedMip := aMostDetailedMip;
                                     Texture1D.MipLevels := aMipLevels;
                                   end;

    D3D11_SRV_DIMENSION_TEXTURE1DARRAY: begin
                                          Texture1DArray.MostDetailedMip := aMostDetailedMip;
                                          Texture1DArray.MipLevels := aMipLevels;
                                          Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture1DArray.ArraySize := aArraySize;
                                        end;

    D3D11_SRV_DIMENSION_TEXTURE2D: begin
                                     Texture2D.MostDetailedMip := aMostDetailedMip;
                                     Texture2D.MipLevels := aMipLevels;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_SRV_DIMENSION_TEXTURE2DARRAY: begin
                                          Texture2DArray.MostDetailedMip := aMostDetailedMip;
                                          Texture2DArray.MipLevels := aMipLevels;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                        end;

    D3D11_SRV_DIMENSION_TEXTURE2DMS: begin
                                       // Nothing todo.
                                     end;

    D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY: begin
                                            Texture2DMSArray.FirstArraySlice := aFirstArraySlice;
                                            Texture2DMSArray.ArraySize := aArraySize;
                                          end;

    D3D11_SRV_DIMENSION_TEXTURE3D: begin
                                     Texture3D.MostDetailedMip := aMostDetailedMip;
                                     Texture3D.MipLevels := aMipLevels;
                                   end;

    D3D11_SRV_DIMENSION_TEXTURECUBE: begin
                                       TextureCube.MostDetailedMip := aMostDetailedMip;
                                       TextureCube.MipLevels := aMipLevels;
                                     end;

    D3D11_SRV_DIMENSION_TEXTURECUBEARRAY: begin
                                            TextureCubeArray.MostDetailedMip := aMostDetailedMip;
                                            TextureCubeArray.MipLevels := aMipLevels;
                                            TextureCubeArray.First2DArrayFace := aFirstArraySlice;
                                            TextureCubeArray.NumCubes := aArraySize;
                                          end;

    D3D11_SRV_DIMENSION_BUFFEREX: begin
                                    BufferEx.FirstElement := aMostDetailedMip;
                                    BufferEx.NumElements := aMipLevels;
                                    BufferEx.Flags := aFlags;
                                  end;
  end;
end;


constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(const aBuffer: ID3D11Buffer;
                                                    aFormat: DXGI_FORMAT;
                                                    aFirstElement: UINT;
                                                    aNumElements: UINT;
                                                    aFlags: UINT = 0);
begin
  Format := aFormat;
  ViewDimension := D3D11_SRV_DIMENSION_BUFFEREX;
  BufferEx.FirstElement := aFirstElement;
  BufferEx.NumElements := aNumElements;
  BufferEx.Flags := aFlags;
end;


constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(const pTex1D: ID3D11Texture1D;
                                                    aViewDimension: D3D11_SRV_DIMENSION;
                                                    aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                    aMostDetailedMip: UINT = 0;
                                                    aMipLevels: UINT = UINT(-1);
                                                    aFirstArraySlice: UINT = 0;
                                                    aArraySize: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE1D_DESC;

begin
  ViewDimension := viewDimension;

  if ( (DXGI_FORMAT_UNKNOWN = aFormat) or (aMipLevels = UINT(-1)) or
            ( (aArraySize = UINT(-1)) and (D3D11_SRV_DIMENSION_TEXTURE1DARRAY = aViewDimension) )) then

    begin
      pTex1D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = format) then
        format := TexDesc.Format;
      if (aMipLevels = UINT(-1)) then
        aMipLevels := TexDesc.MipLevels - aMostDetailedMip;
      if (aArraySize = UINT(-1)) then
        aArraySize := TexDesc.ArraySize - aFirstArraySlice;
    end;

  Format := aFormat;

  case aViewDimension of

    D3D11_SRV_DIMENSION_TEXTURE1D: begin
                                     Texture1D.MostDetailedMip := aMostDetailedMip;
                                     Texture1D.MipLevels := aMipLevels;
                                   end;

    D3D11_SRV_DIMENSION_TEXTURE1DARRAY: begin
                                          Texture1DArray.MostDetailedMip := aMostDetailedMip;
                                          Texture1DArray.MipLevels := aMipLevels;
                                          Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture1DArray.ArraySize := aArraySize;
                                        end;
  end;

end;


constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(const pTex2D: ID3D11Texture2D;
                                                    aViewDimension: D3D11_SRV_DIMENSION;
                                                    aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                    aMostDetailedMip: UINT = 0;
                                                    aMipLevels: UINT = UINT(-1);
                                                    aFirstArraySlice: UINT = 0; // First2DArrayFace for TEXTURECUBEARRAY
                                                    aArraySize: UINT = UINT(-1);  // NumCubes for TEXTURECUBEARRAY
                                                    aPlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D or TEXTURE2DARRAY});
var
  TexDesc: D3D11_TEXTURE2D_DESC;

begin
  ViewDimension := aViewDimension;
  if (DXGI_FORMAT_UNKNOWN = aFormat) or
            ((aMipLevels = Uint(-1)) and
                (D3D11_SRV_DIMENSION_TEXTURE2DMS <> aViewDimension) and
                (D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY <> viewDimension)) or
            ((aArraySize = UINT(-1)) and
                ((D3D11_SRV_DIMENSION_TEXTURE2DARRAY = aViewDimension) or
                (D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY = aViewDimension) or
                (D3D11_SRV_DIMENSION_TEXTURECUBEARRAY = aViewDimension))) then
    begin
      pTex2D.GetDesc(TexDesc);

      if (DXGI_FORMAT_UNKNOWN = format) then
        format := TexDesc.Format;
      if (aMipLevels = UINT(-1)) then
        aMipLevels := TexDesc.MipLevels - aMostDetailedMip;
      if (aArraySize = UINT(-1)) then
        begin
          aArraySize := TexDesc.ArraySize - aFirstArraySlice;
          if (D3D11_SRV_DIMENSION_TEXTURECUBEARRAY = aViewDimension) then
            aArraySize := aArraySize div 6;
        end;
    end;

  Format := aFormat;

  case aViewDimension of

    D3D11_SRV_DIMENSION_TEXTURE2D: begin;
                                     Texture2D.MostDetailedMip := aMostDetailedMip;
                                     Texture2D.MipLevels := aMipLevels;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_SRV_DIMENSION_TEXTURE2DARRAY: begin;
                                          Texture2DArray.MostDetailedMip := aMostDetailedMip;
                                          Texture2DArray.MipLevels := aMipLevels;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                         end;

    D3D11_SRV_DIMENSION_TEXTURE2DMS: begin;
                                       // Nothing todo.
                                     end;

    D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY: begin;
                                            Texture2DMSArray.FirstArraySlice := aFirstArraySlice;
                                            Texture2DMSArray.ArraySize := aArraySize;
                                          end;

    D3D11_SRV_DIMENSION_TEXTURECUBE: begin;
                                       TextureCube.MostDetailedMip := aMostDetailedMip;
                                       TextureCube.MipLevels := aMipLevels;
                                     end;

    D3D11_SRV_DIMENSION_TEXTURECUBEARRAY: begin;
                                            TextureCubeArray.MostDetailedMip := aMostDetailedMip;
                                            TextureCubeArray.MipLevels := aMipLevels;
                                            TextureCubeArray.First2DArrayFace := aFirstArraySlice;
                                            TextureCubeArray.NumCubes := aArraySize;
                                          end;
  end;
end;


constructor D3D11_SHADER_RESOURCE_VIEW_DESC1.Create(const pTex3D: ID3D11Texture3D;
                                                    aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                    aMostDetailedMip: UINT = 0;
                                                    aMipLevels: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE3D_DESC;

begin
  ViewDimension := D3D11_SRV_DIMENSION_TEXTURE3D;
  if (DXGI_FORMAT_UNKNOWN = aFormat) or (aMipLevels = UINT(-1)) then
    begin
      pTex3D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = aFormat) then
        format := TexDesc.Format;
      if (aMipLevels = UINT(-1)) then
        aMipLevels := TexDesc.MipLevels - aMostDetailedMip;
    end;
  Format := aFormat;
  Texture3D.MostDetailedMip := aMostDetailedMip;
  Texture3D.MipLevels := aMipLevels;
end;


{D3D11_RENDER_TARGET_VIEW_DESC1}

constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(const o: D3D11_RENDER_TARGET_VIEW_DESC1);
begin
  Self := o;
end;


constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(aViewDimension: D3D11_RTV_DIMENSION;
                                                  aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                  aMipSlice: UINT = 0; // FirstElement for BUFFER
                                                  aFirstArraySlice: UINT = 0; // NumElements for BUFFER, FirstWSlice for TEXTURE3D
                                                  aArraySize: UINT = UINT(-1); // WSize for TEXTURE3D
                                                  aPlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D and TEXTURE2DARRAY});
begin
  Format := aFormat;
  ViewDimension := aViewDimension;

  case aViewDimension of

    D3D11_RTV_DIMENSION_BUFFER: begin
                                  Buffer.FirstElement := aMipSlice;
                                  Buffer.NumElements := aFirstArraySlice;
                                end;

    D3D11_RTV_DIMENSION_TEXTURE1D: begin
                                     Texture1D.MipSlice := aMipSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE1DARRAY: begin
                                          Texture1DArray.MipSlice := aMipSlice;
                                          Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture1DArray.ArraySize := aArraySize;
                                        end;

    D3D11_RTV_DIMENSION_TEXTURE2D: begin
                                     Texture2D.MipSlice := aMipSlice;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE2DARRAY: begin
                                          Texture2DArray.MipSlice := aMipSlice;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                        end;

    D3D11_RTV_DIMENSION_TEXTURE2DMS: begin
                                       // Nothing todo.
                                     end;

    D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY: begin
                                            Texture2DMSArray.FirstArraySlice := aFirstArraySlice;
                                            Texture2DMSArray.ArraySize := aArraySize;
                                          end;

    D3D11_RTV_DIMENSION_TEXTURE3D: begin
                                     Texture3D.MipSlice := aMipSlice;
                                     Texture3D.FirstWSlice := aFirstArraySlice;
                                     Texture3D.WSize := aArraySize;
                                   end;
  end;
end;


constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(const aBuffer: ID3D11Buffer;
                                                  aFormat: DXGI_FORMAT;
                                                  aFirstElement: UINT;
                                                  aNumElements: UINT);
begin
  Format := aFormat;
  ViewDimension := D3D11_RTV_DIMENSION_BUFFER;
  Buffer.FirstElement := aFirstElement;
  Buffer.NumElements := aNumElements
end;


constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(const pTex1D: ID3D11Texture1D;
                                                  aViewDimension: D3D11_SRV_DIMENSION;
                                                  aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                  aMipSlice: UINT = 0;
                                                  aFirstArraySlice: UINT = 0;
                                                  aArraySize: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE1D_DESC;

begin
  ViewDimension := viewDimension;
  if ( (DXGI_FORMAT_UNKNOWN = aFormat) or
            ( (aArraySize = UINT(-1)) and (DWORD(D3D11_RTV_DIMENSION_TEXTURE1DARRAY) = aViewDimension) )) then
    begin
      pTex1D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = aFormat) then
        format := TexDesc.Format;
      if (aArraySize = UINT(-1)) then
        aArraySize := TexDesc.ArraySize - aFirstArraySlice;
    end;

  Format := aFormat;

  case aViewDimension of
    DWORD(D3D11_RTV_DIMENSION_TEXTURE1D): begin
                                            Texture1D.MipSlice := aMipSlice;
                                          end;

    DWORD(D3D11_RTV_DIMENSION_TEXTURE1DARRAY): begin
                                                 Texture1DArray.MipSlice := aMipSlice;
                                                 Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                                 Texture1DArray.ArraySize := aArraySize;
                                               end;
  end;
end;


constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(const pTex2D: ID3D11Texture2D;
                                                  aViewDimension: D3D11_RTV_DIMENSION;
                                                  aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                  aMipSlice: UINT = 0;
                                                  aFirstArraySlice: UINT = 0;
                                                  aArraySize: UINT = UINT(-1);
                                                  aPlaneSlice: UINT = 0);
var
  TexDesc: D3D11_TEXTURE2D_DESC;

begin
  ViewDimension := aViewDimension;

  if ( (DXGI_FORMAT_UNKNOWN = format) or
            ( (aArraySize = UINT(-1)) and
                ( (D3D11_RTV_DIMENSION_TEXTURE2DARRAY = aViewDimension) or
                  (D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY = aViewDimension) ))) then
    begin
      pTex2D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = format) then
        format := TexDesc.Format;
      if (aArraySize = UINT(-1)) then
        aArraySize := TexDesc.ArraySize - aFirstArraySlice;
    end;

  Format := aFormat;

  case aViewDimension of
    D3D11_RTV_DIMENSION_TEXTURE2D: begin
                                     Texture2D.MipSlice := aMipSlice;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE2DARRAY: begin
                                          Texture2DArray.MipSlice := aMipSlice;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                        end;

    D3D11_RTV_DIMENSION_TEXTURE2DMS: begin
                                       // Nothing todo.
                                     end;

    D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY: begin
                                            Texture2DMSArray.FirstArraySlice := aFirstArraySlice;
                                            Texture2DMSArray.ArraySize := aArraySize;
                                          end;
  end;
end;


constructor D3D11_RENDER_TARGET_VIEW_DESC1.Create(const pTex3D: ID3D11Texture3D;
                                                  aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                  aMipSlice: UINT = 0;
                                                  aFirstWSlice: UINT = 0;
                                                  aWSize: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE3D_DESC;

begin
  ViewDimension := D3D11_RTV_DIMENSION_TEXTURE3D;
  if ( (DXGI_FORMAT_UNKNOWN = aFormat) or (aWSize = UINT(-1)) ) then
    begin
      pTex3D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = aFormat) then
        format := TexDesc.Format;
      if (aWSize = UINT(-1)) then
        aWSize := TexDesc.Depth - aFirstWSlice;
    end;

  Format := format;
  Texture3D.MipSlice := aMipSlice;
  Texture3D.FirstWSlice:= aFirstWSlice;
  Texture3D.WSize := aWSize;
end;


{D3D11_UNORDERED_ACCESS_VIEW_DESC1}

constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(const o: D3D11_UNORDERED_ACCESS_VIEW_DESC1);
begin
  Self := o;
end;


constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(aViewDimension: D3D11_RTV_DIMENSION;
                                                     aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                     aMipSlice: UINT = 0; // FirstElement for BUFFER
                                                     aFirstArraySlice: UINT = 0; // NumElements for BUFFER, FirstWSlice for TEXTURE3D
                                                     aArraySize: UINT = UINT(-1); // WSize for TEXTURE3D
                                                     APlaneSlice: UINT = 0 {PlaneSlice for TEXTURE2D and TEXTURE2DARRAY});
begin
  Format := aFormat;
  DWORD(ViewDimension) := DWORD(aViewDimension);

  case aViewDimension of

    D3D11_RTV_DIMENSION_BUFFER: begin
                                  Buffer.FirstElement := aMipSlice;
                                  Buffer.NumElements := aFirstArraySlice;
                                end;

    D3D11_RTV_DIMENSION_TEXTURE1D: begin
                                     Texture1D.MipSlice := aMipSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE1DARRAY: begin
                                          Texture1DArray.MipSlice := aMipSlice;
                                          Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture1DArray.ArraySize := aArraySize;
                                        end;

    D3D11_RTV_DIMENSION_TEXTURE2D: begin
                                     Texture2D.MipSlice := aMipSlice;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE2DARRAY: begin
                                          Texture2DArray.MipSlice := aMipSlice;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                        end;

    D3D11_RTV_DIMENSION_TEXTURE2DMS: begin
                                       // Nothing todo.
                                     end;
    D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY: begin
                                            Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                            Texture2DArray.ArraySize := aArraySize;
                                          end;
    D3D11_RTV_DIMENSION_TEXTURE3D: begin
                                     Texture3D.MipSlice := aMipSlice;
                                     Texture3D.FirstWSlice := aFirstArraySlice;
                                     Texture3D.WSize := aArraySize;
                                   end;
  end;
end;


constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(const aBuffer: ID3D11Buffer;
                                                     aFormat: DXGI_FORMAT;
                                                     aFirstElement: UINT;
                                                     aNumElements: UINT);
begin
  Format := aFormat;
  DWORD(ViewDimension) := DWORD(D3D11_RTV_DIMENSION_BUFFER);
  Buffer.FirstElement := aFirstElement;
  Buffer.NumElements := aNumElements;
end;


constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(const pTex1D: ID3D11Texture1D;
                                                     aViewDimension: D3D11_RTV_DIMENSION;
                                                     aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                     aMipSlice: UINT = 0;
                                                     aFirstArraySlice: UINT = 0;
                                                     aArraySize: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE1D_DESC;

begin
  ViewDimension := viewDimension;
  if (DXGI_FORMAT_UNKNOWN = format) or
    ((aArraySize = UINT(-1)) and (D3D11_RTV_DIMENSION_TEXTURE1DARRAY = aViewDimension)) then
    begin
      pTex1D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = format) then
        format := TexDesc.Format;
      if (aArraySize = UINT(-1)) then
        aArraySize := TexDesc.ArraySize - aFirstArraySlice;
    end;

  Format := format;

  case aViewDimension of

    D3D11_RTV_DIMENSION_TEXTURE1D: begin
                                     Texture1D.MipSlice := aMipSlice;
                                   end;

    D3D11_RTV_DIMENSION_TEXTURE1DARRAY: begin
                                          Texture1DArray.MipSlice := aMipSlice;
                                          Texture1DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture1DArray.ArraySize := aArraySize;
                                        end;
  end;
end;


constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(const pTex2D: ID3D11Texture2D;
                                                     aViewDimension: D3D11_UAV_DIMENSION;
                                                     aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                     aMipSlice: UINT = 0;
                                                     aFirstArraySlice: UINT = 0;
                                                     aArraySize: UINT = UINT(-1);
                                                     aPlaneSlice: UINT = 0);
var
  TexDesc: D3D11_TEXTURE2D_DESC;

begin
  ViewDimension := aViewDimension;

  if (DXGI_FORMAT_UNKNOWN = aFormat) or
     ( (aArraySize = UINT(-1)) and (D3D11_UAV_DIMENSION_TEXTURE2DARRAY = aViewDimension) ) then
    begin
      pTex2D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = aFormat) then
        Format := TexDesc.Format;
      if (aArraySize = UINT(-1)) then
        aArraySize := TexDesc.ArraySize - aFirstArraySlice;
    end;

  Format := aFormat;

  case aViewDimension of
    D3D11_UAV_DIMENSION_TEXTURE2D: begin
                                     Texture2D.MipSlice := aMipSlice;
                                     Texture2D.PlaneSlice := aPlaneSlice;
                                   end;

    D3D11_UAV_DIMENSION_TEXTURE2DARRAY: begin
                                          Texture2DArray.MipSlice := aMipSlice;
                                          Texture2DArray.FirstArraySlice := aFirstArraySlice;
                                          Texture2DArray.ArraySize := aArraySize;
                                          Texture2DArray.PlaneSlice := aPlaneSlice;
                                        end;
  end;
end;


constructor D3D11_UNORDERED_ACCESS_VIEW_DESC1.Create(const pTex3D: ID3D11Texture3D;
                                                     aFormat: DXGI_FORMAT = DXGI_FORMAT_UNKNOWN;
                                                     aMipSlice: UINT = 0;
                                                     aFirstWSlice: UINT = 0;
                                                     aWSize: UINT = UINT(-1));
var
  TexDesc: D3D11_TEXTURE3D_DESC;

begin
  ViewDimension := D3D11_UAV_DIMENSION_TEXTURE3D;
  if (DXGI_FORMAT_UNKNOWN = aFormat) or (aWSize = UINT(-1)) then
    begin
      pTex3D.GetDesc(TexDesc);
      if (DXGI_FORMAT_UNKNOWN = format) then
        format := TexDesc.Format;
      if (aWSize = UINT(-1)) then
        aWSize := TexDesc.Depth - aFirstWSlice;
    end;

  Format := aFormat;
  Texture3D.MipSlice := aMipSlice;
  Texture3D.FirstWSlice := aFirstWSlice;
  Texture3D.WSize := aWSize;
end;


{D3D11_QUERY_DESC1}

constructor D3D11_QUERY_DESC1.Create(const o: D3D11_QUERY_DESC1);
begin
  Self := o;
end;


constructor D3D11_QUERY_DESC1.Create(aQuery: D3D11_QUERY;
                                     aMiscFlags: UINT = 0;
                                     aContextType: D3D11_CONTEXT_TYPE = D3D11_CONTEXT_TYPE_ALL);
begin
  Query := aQuery;
  MiscFlags := aMiscFlags;
  ContextType := aContextType;
end;

  // Implement Additional Prototypes here.

end.
