// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11_2.pas
// Kind: Pascal / Delphi unit
// Release date: 31-08-2022
// Language: ENU
//
// Revision Version: 3.1.3
//
// Description: Contains interface definitions for the D3D11.2 API.
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
// Related projects: MfPackX312
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: D3D11_2.h
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
unit WinApi.DirectX.D3D11_2;

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
  PD3D11_TILED_RESOURCE_COORDINATE = ^D3D11_TILED_RESOURCE_COORDINATE;
  D3D11_TILED_RESOURCE_COORDINATE = record
    X: UINT;
    Y: UINT;
    Z: UINT;
    Subresource: UINT;
  end;
  {$EXTERNALSYM D3D11_TILED_RESOURCE_COORDINATE}

  PD3D11_TILE_REGION_SIZE = ^D3D11_TILE_REGION_SIZE;
  D3D11_TILE_REGION_SIZE = record
    NumTiles: UINT;
    bUseBox: BOOL;
    Width: UINT;
    Height: UINT16;
    Depth: UINT16;
  end;
  {$EXTERNALSYM D3D11_TILE_REGION_SIZE}

  PD3D11_TILE_MAPPING_FLAG = ^D3D11_TILE_MAPPING_FLAG;
  D3D11_TILE_MAPPING_FLAG           = (
    D3D11_TILE_MAPPING_NO_OVERWRITE = $00000001
  );
  {$EXTERNALSYM D3D11_TILE_MAPPING_FLAG}

  PD3D11_TILE_RANGE_FLAG = ^D3D11_TILE_RANGE_FLAG;
  D3D11_TILE_RANGE_FLAG                = (
    D3D11_TILE_RANGE_NULL              = $00000001,
    D3D11_TILE_RANGE_SKIP              = $00000002,
    D3D11_TILE_RANGE_REUSE_SINGLE_TILE = $00000004
  );
  {$EXTERNALSYM D3D11_TILE_RANGE_FLAG}

  PD3D11_SUBRESOURCE_TILING = ^D3D11_SUBRESOURCE_TILING;
  D3D11_SUBRESOURCE_TILING = record
    WidthInTiles: UINT;
    HeightInTiles: UINT16;
    DepthInTiles: UINT16;
    StartTileIndexInOverallResource: UINT;
  end;
  {$EXTERNALSYM D3D11_SUBRESOURCE_TILING}

const
  D3D11_PACKED_TILE = UINT($ffffffff);

type

  PD3D11_TILE_SHAPE = ^D3D11_TILE_SHAPE;
  D3D11_TILE_SHAPE = record
    WidthInTexels: UINT;
    HeightInTexels: UINT;
    DepthInTexels: UINT;
  end;
  {$EXTERNALSYM D3D11_TILE_SHAPE}

  PD3D11_PACKED_MIP_DESC = ^D3D11_PACKED_MIP_DESC;
  D3D11_PACKED_MIP_DESC = record
    NumStandardMips: UINT8;
    NumPackedMips: UINT8;
    NumTilesForPackedMips: UINT;
    StartTileIndexInOverallResource: UINT;
  end;
  {$EXTERNALSYM D3D11_PACKED_MIP_DESC}

  PD3D11_CHECK_MULTISAMPLE_QUALITY_LEVELS_FLAG = ^D3D11_CHECK_MULTISAMPLE_QUALITY_LEVELS_FLAG;
  D3D11_CHECK_MULTISAMPLE_QUALITY_LEVELS_FLAG             = (
    D3D11_CHECK_MULTISAMPLE_QUALITY_LEVELS_TILED_RESOURCE = $00000001
  );
  {$EXTERNALSYM D3D11_CHECK_MULTISAMPLE_QUALITY_LEVELS_FLAG}

  PD3D11_TILE_COPY_FLAG = ^D3D11_TILE_COPY_FLAG;
  D3D11_TILE_COPY_FLAG                                       = (
    D3D11_TILE_COPY_NO_OVERWRITE                             = $00000001,
    D3D11_TILE_COPY_LINEAR_BUFFER_TO_SWIZZLED_TILED_RESOURCE = $00000002,
    D3D11_TILE_COPY_SWIZZLED_TILED_RESOURCE_TO_LINEAR_BUFFER = $00000004
  );
  {$EXTERNALSYM D3D11_TILE_COPY_FLAG}


  //////////////////////////////////////////////////////////////////////////////
  //
  // DeviceContext2
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11DeviceContext2
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11DeviceContext2);'}
  {$EXTERNALSYM ID3D11DeviceContext2}
  ID3D11DeviceContext2 = interface(ID3D11DeviceContext1)
    ['{420d5b32-b90c-4da4-bef0-359f6a24a83a}']

    function UpdateTileMappings(const pTiledResource: ID3D11Resource;
                                NumTiledResourceRegions: UINT;
                                pTiledResourceRegionStartCoordinates: D3D11_TILED_RESOURCE_COORDINATE;
                                pTiledResourceRegionSizes: D3D11_TILE_REGION_SIZE;
                                const pTilePool: ID3D11Buffer;
                                NumRanges: UINT;
                                pRangeFlags: UINT;
                                pTilePoolStartOffsets: UINT;
                                pRangeTileCounts: UINT;
                                Flags: UINT): HResult; stdcall;

    function CopyTileMappings(const pDestTiledResource: ID3D11Resource;
                              pDestRegionStartCoordinate: D3D11_TILED_RESOURCE_COORDINATE;
                              const pSourceTiledResource: ID3D11Resource;
                              pSourceRegionStartCoordinate: D3D11_TILED_RESOURCE_COORDINATE;
                              pTileRegionSize: PD3D11_TILE_REGION_SIZE;
                              Flags: UINT): HResult; stdcall;

    procedure CopyTiles(const pTiledResource: ID3D11Resource;
                        pTileRegionStartCoordinate: D3D11_TILED_RESOURCE_COORDINATE;
                        pTileRegionSize: D3D11_TILE_REGION_SIZE;
                        const pBuffer: ID3D11Buffer;
                        BufferStartOffsetInBytes: UINT64;
                        Flags: UINT); stdcall;

    procedure UpdateTiles(const pDestTiledResource: ID3D11Resource;
                          pDestTileRegionStartCoordinate: D3D11_TILED_RESOURCE_COORDINATE;
                          pDestTileRegionSize: D3D11_TILE_REGION_SIZE;
                          const pSourceTileData: Pointer;
                          Flags: UINT); stdcall;

    function ResizeTilePool(const pTilePool: ID3D11Buffer;
                            NewSizeInBytes: UINT64): HResult; stdcall;

    procedure TiledResourceBarrier(const pTiledResourceOrViewAccessBeforeBarrier: ID3D11DeviceChild;
                                   const pTiledResourceOrViewAccessAfterBarrier: ID3D11DeviceChild); stdcall;

    function IsAnnotationEnabled(): BOOL; stdcall;

    procedure SetMarkerInt(pLabel: PWideChar;
                           Data: INT); stdcall;

    procedure BeginEventInt(pLabel: PWideChar;
                            Data: INT); stdcall;

    procedure EndEvent(); stdcall;

  end;
  IID_ID3D11DeviceContext2 = ID3D11DeviceContext2;
  {$EXTERNALSYM IID_ID3D11DeviceContext2}


  // Interface ID3D11DeviceContext2
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device2);'}
  {$EXTERNALSYM ID3D11Device2}
  ID3D11Device2 = interface(ID3D11Device1)
    ['{9d06dffa-d1e5-4d07-83a8-1bb123f2f841}']

    procedure GetImmediateContext2([ref] const ppImmediateContext: ID3D11DeviceContext2); stdcall;

    function CreateDeferredContext2(ContextFlags: UINT;
                                   [ref] const ppDeferredContext: ID3D11DeviceContext2): HResult; stdcall;

    procedure GetResourceTiling(const pTiledResource: ID3D11Resource;
                                out pNumTilesForEntireResource: UINT;
                                out pPackedMipDesc: D3D11_PACKED_MIP_DESC;
                                out pStandardTileShapeForNonPackedMips: D3D11_TILE_SHAPE;
                                var pNumSubresourceTilings: UINT;
                                FirstSubresourceTilingToGet: UINT;
                                out pSubresourceTilingsForNonPackedMips: D3D11_SUBRESOURCE_TILING); stdcall;

    function CheckMultisampleQualityLevels1(Format: DXGI_FORMAT;
                                            SampleCount: UINT;
                                            Flags: UINT;
                                            out pNumQualityLevels: UINT): HResult; stdcall;

  end;
  IID_ID3D11Device2 = ID3D11Device2;
  {$EXTERNALSYM IID_ID3D11Device2}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
