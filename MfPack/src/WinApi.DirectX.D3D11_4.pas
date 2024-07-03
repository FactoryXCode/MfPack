// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11_4.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Microsoft DirectX D3D11 used by Media Foundation.
//              You can use Direct3D 11 graphics to create 3-D graphics for games,
//              scientific and desktop apps.
//              Contains interface definitions for the D3D11.4 API.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D11.pas is outdated!
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: D3D11_4.h
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
unit WinApi.DirectX.D3D11_4;

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
  WinApi.DirectX.D3D11_3,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI1_3,
  WinApi.DirectX.DXGI1_4,
  WinApi.DirectX.DXGI1_5;

  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


  //////////////////////////////////////////////////////////////////////////////
  //
  // Device4
  //
  //////////////////////////////////////////////////////////////////////////////


type

  // Interface ID3D11Device4
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device4);'}
  {$EXTERNALSYM ID3D11Device4}
  ID3D11Device4 = interface(ID3D11Device3)
    ['{8992ab71-02e6-4b8d-ba48-b056dcda42c4}']

    function RegisterDeviceRemovedEvent(hEvent: THandle;
                                        out pdwCookie: DWORD): HRESULT; stdcall;

    procedure UnregisterDeviceRemoved(dwCookie: DWORD); stdcall;

  end;
  IID_ID3D11Device4 = ID3D11Device4;
  {$EXTERNALSYM IID_ID3D11Device4}


  //////////////////////////////////////////////////////////////////////////////
  //
  // Device5
  //
  //////////////////////////////////////////////////////////////////////////////


  // Interface ID3D11Device5
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device5);'}
  {$EXTERNALSYM ID3D11Device5}
  ID3D11Device5 = interface(ID3D11Device4)
    ['{8ffde202-a0e7-45df-9e01-e837801b5ea0}']

    function OpenSharedFence(hFence: THandle;
                             const ReturnedInterface: REFIID;
                             out ppFence: Pointer): HRESULT; stdcall;

    function CreateFence(InitialValue: UINT64;
                         Flags: D3D11_FENCE_FLAG;
                         const ReturnedInterface: REFIID;
                         out ppFence: Pointer): HRESULT; stdcall;
  end;
  IID_ID3D11Device5 = ID3D11Device5;
  {$EXTERNALSYM IID_ID3D11Device5}


  //============================================================================
  //
  // Multithread Interface
  //
  //============================================================================

  // Interface ID3D11Multithread
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Multithread);'}
  {$EXTERNALSYM ID3D11Multithread}
  ID3D11Multithread = interface(IUnknown)
    ['{9B7E4E00-342C-4106-A19F-4F2704F689F0}']

    procedure Enter(); stdcall;

    procedure Leave(); stdcall;

    function SetMultithreadProtected(bMTProtect: BOOL): BOOL; stdcall;

    function GetMultithreadProtected(): BOOL; stdcall;

  end;
  IID_ID3D11Multithread = ID3D11Multithread;
  {$EXTERNALSYM IID_ID3D11Multithread}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoContext2
  //
  //////////////////////////////////////////////////////////////////////////////

  // Interface ID3D11VideoContext2
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11VideoContext2);'}
  {$EXTERNALSYM ID3D11VideoContext2}
  ID3D11VideoContext2 = interface(ID3D11VideoContext1)
    ['{C4E7374C-6243-4D1B-AE87-52B4F740E261}']

    procedure VideoProcessorSetOutputHDRMetaData(const pVideoProcessor: ID3D11VideoProcessor;
                                                 Type_: DXGI_HDR_METADATA_TYPE;
                                                 Size: UINT;
                                                 pHDRMetaData: Pointer); stdcall;

    procedure VideoProcessorGetOutputHDRMetaData(const pVideoProcessor: ID3D11VideoProcessor;
                                                 out pType: DXGI_HDR_METADATA_TYPE;
                                                 Size: UINT;
                                                 pMetaData: Pointer); stdcall;

    procedure VideoProcessorSetStreamHDRMetaData(const pVideoProcessor: ID3D11VideoProcessor;
                                                 StreamIndex: UINT;
                                                 Type_: DXGI_HDR_METADATA_TYPE;
                                                 Size: UINT;
                                                 pHDRMetaData: Pointer); stdcall;

    procedure VideoProcessorGetStreamHDRMetaData(const pVideoProcessor: ID3D11VideoProcessor;
                                                 StreamIndex: UINT;
                                                 out pType: DXGI_HDR_METADATA_TYPE;
                                                 Size: UINT;
                                                 out pMetaData: Pointer); stdcall;


  end;
  IID_ID3D11VideoContext2 = ID3D11VideoContext2;
  {$EXTERNALSYM IID_ID3D11VideoContext2}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoDevice2
  //
  //////////////////////////////////////////////////////////////////////////////


  PD3D11_FEATURE_VIDEO = ^D3D11_FEATURE_VIDEO;
  D3D11_FEATURE_VIDEO                     = (
    D3D11_FEATURE_VIDEO_DECODER_HISTOGRAM = 0
  );
  {$EXTERNALSYM D3D11_FEATURE_VIDEO}

  PD3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT = ^D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT;
  D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT     = (
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_Y = 0,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_U = 1,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_V = 2,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_R = 0,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_G = 1,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_B = 2,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_A = 3
  );
  {$EXTERNALSYM D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT}

  // We had to do the 1 shl x trick here.
  PD3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS = ^D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS;
  D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS       = (
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_NONE = $0,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_Y    = 1,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_U    = 2,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_V    = 4,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_R    = 1,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_G    = 2,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_B    = 4,
    D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAG_A    = 8
  );
  {$EXTERNALSYM D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS}

  // DEFINE_ENUM_FLAG_OPERATORS(D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS)

  PD3D11_FEATURE_DATA_VIDEO_DECODER_HISTOGRAM = ^D3D11_FEATURE_DATA_VIDEO_DECODER_HISTOGRAM;
  D3D11_FEATURE_DATA_VIDEO_DECODER_HISTOGRAM = record
    DecoderDesc: D3D11_VIDEO_DECODER_DESC;                       // in
    Components: D3D11_VIDEO_DECODER_HISTOGRAM_COMPONENT_FLAGS;   // out
    BinCount: UINT;                                              // out
    CounterBitDepth: UINT;                                       // out
  end;
  {$EXTERNALSYM D3D11_FEATURE_DATA_VIDEO_DECODER_HISTOGRAM}

  PD3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS = ^D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS;
  D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS       = (
    D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAG_NONE = $0
  );
  {$EXTERNALSYM D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS}


//  DEFINE_ENUM_FLAG_OPERATORS(D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS)

  // Interface ID3D11VideoDevice2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device5);'}
  {$EXTERNALSYM ID3D11VideoDevice2}
  ID3D11VideoDevice2 = interface(ID3D11Device4)
    ['{59C0CB01-35F0-4A70-8F67-87905C906A53}']

    function CheckFeatureSupport(Feature: D3D11_FEATURE_VIDEO;
                                 out pFeatureSupportData: Pointer;
                                 FeatureSupportDataSize: UINT): HRESULT; stdcall;

    function NegotiateCryptoSessionKeyExchangeMT(pCryptoSession: ID3D11CryptoSession;
                                                 flags: D3D11_CRYPTO_SESSION_KEY_EXCHANGE_FLAGS;
                                                 DataSize: UINT;
                                                 [ref] const pData: Pointer): HRESULT; stdcall;

  end;
  IID_ID3D11VideoDevice2 = ID3D11VideoDevice2;
  {$EXTERNALSYM IID_ID3D11VideoDevice2}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoContext3
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_VIDEO_DECODER_BUFFER_DESC2 = ^D3D11_VIDEO_DECODER_BUFFER_DESC2;
  D3D11_VIDEO_DECODER_BUFFER_DESC2 = record
    BufferType: D3D11_VIDEO_DECODER_BUFFER_TYPE;
    DataOffset: UINT;
    DataSize: UINT;
    pIV: Pointer;
    IVSize: UINT;
    pSubSampleMappingBlock: PD3D11_VIDEO_DECODER_SUB_SAMPLE_MAPPING_BLOCK;
    SubSampleMappingCount: UINT;
    cBlocksStripeEncrypted: UINT;
    cBlocksStripeClear: UINT;
  end;
  {$EXTERNALSYM D3D11_VIDEO_DECODER_BUFFER_DESC2}


  // Interface ID3D11VideoContext3
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11VideoContext3);'}
  {$EXTERNALSYM ID3D11VideoContext3}
  ID3D11VideoContext3 = interface(ID3D11VideoContext2)
    ['{A9E2FAA0-CB39-418F-A0B7-D8AAD4DE672E}']

    function DecoderBeginFrame1(const pDecoder: ID3D11VideoDecoder;
                const pView: ID3D11VideoDecoderOutputView;
                ContentKeySize: UINT;
                pContentKey: Pointer;
                NumComponentHistograms: UINT;
                pHistogramOffsets: UINT;
                ppHistogramBuffers: PID3D11Buffer): HRESULT; stdcall;

    function SubmitDecoderBuffers2(const pDecoder: ID3D11VideoDecoder;
                                   NumBuffers: UINT;
                                   pBufferDesc: D3D11_VIDEO_DECODER_BUFFER_DESC2): HRESULT; stdcall;


  end;
  IID_ID3D11VideoContext3 = ID3D11VideoContext3;
  {$EXTERNALSYM IID_ID3D11VideoContext3}


  PD3D11_FEATURE_DATA_D3D11_OPTIONS4 = ^D3D11_FEATURE_DATA_D3D11_OPTIONS4;
  D3D11_FEATURE_DATA_D3D11_OPTIONS4 = record
    ExtendedNV12SharedTextureSupported: BOOL;
  end;
  {$EXTERNALSYM D3D11_FEATURE_DATA_D3D11_OPTIONS4}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
