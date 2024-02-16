// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11_1.pas
// Kind: Pascal / Delphi unit
// Release date: 31-08-2022
// Language: ENU
//
// Revision Version: 3.1.6
//
// Description: Contains interface definitions for the D3D11.1 API.
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks:  Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: D3D11_1.h
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
unit WinApi.DirectX.D3D11_1;

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
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI1_2;

  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type
  // Delphi spec.
  FixedColorFLOATArray = array [0..3] of FLOAT;


  PD3d11CopyFlags = ^D3D11_COPY_FLAGS;
  D3D11_COPY_FLAGS          = (
    D3D11_COPY_NO_OVERWRITE = $00000001,
    D3D11_COPY_DISCARD      = $00000002
  );
  {$EXTERNALSYM D3D11_COPY_FLAGS}

  PD3d11LogicOp = ^D3D11_LOGIC_OP;
  D3D11_LOGIC_OP         = (
    // Operation:
    // (s         == PS output, d = RTV contents)
    D3D11_LOGIC_OP_CLEAR = 0,       // 0
    D3D11_LOGIC_OP_SET,             // 1
    D3D11_LOGIC_OP_COPY,            // s
    D3D11_LOGIC_OP_COPY_INVERTED,   // ~s
    D3D11_LOGIC_OP_NOOP,            // d
    D3D11_LOGIC_OP_INVERT,          // ~d
    D3D11_LOGIC_OP_AND,             // s  d
    D3D11_LOGIC_OP_NAND,            // ~(s  d)
    D3D11_LOGIC_OP_OR,              // s | d
    D3D11_LOGIC_OP_NOR,             // ~(s | d)
    D3D11_LOGIC_OP_XOR,             // s ^ d
    D3D11_LOGIC_OP_EQUIV,           // ~(s ^ d)
    D3D11_LOGIC_OP_AND_REVERSE,     // s  ~d
    D3D11_LOGIC_OP_AND_INVERTED,    // ~s  d
    D3D11_LOGIC_OP_OR_REVERSE,      // s | ~d
    D3D11_LOGIC_OP_OR_INVERTED      // ~s | d
  );
  {$EXTERNALSYM D3D11_LOGIC_OP}


  PD3D11_RENDER_TARGET_BLEND_DESC1 = ^D3D11_RENDER_TARGET_BLEND_DESC1;
  D3D11_RENDER_TARGET_BLEND_DESC1 = record
    BlendEnable: BOOL;
    LogicOpEnable: BOOL;            // LogicOpEnable and BlendEnable can't both be True
    SrcBlend: D3D11_BLEND;
    DestBlend: D3D11_BLEND;
    BlendOp: D3D11_BLEND_OP;
    SrcBlendAlpha: D3D11_BLEND;
    DestBlendAlpha: D3D11_BLEND;
    BlendOpAlpha: D3D11_BLEND_OP;
    LogicOp: D3D11_LOGIC_OP;        // applies to RGBA
    RenderTargetWriteMask: UINT8;   // D3D11_COLOR_WRITE_ENABLE
  end;
  {$EXTERNALSYM D3D11_RENDER_TARGET_BLEND_DESC1}


  PD3d11BlendDesc1 = ^D3D11_BLEND_DESC1;
  D3D11_BLEND_DESC1 = record
    AlphaToCoverageEnable: BOOL;     // relevant to multisample antialiasing only
    IndependentBlendEnable: BOOL;    // if FALSE, then replicate the first entry in RenderTarget array to other entries
    RenderTarget: array[0..D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT - 1] of D3D11_RENDER_TARGET_BLEND_DESC1;
  end;
  {$EXTERNALSYM D3D11_BLEND_DESC1}

// Note, the array size for RenderTarget[] above is D3D11_SIMULTANEOUS_RENDERTARGET_COUNT.
// IDL processing/generation of this header replaces the define; this comment is merely explaining what happened.


  // Interface ID3D11BlendState1
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11BlendState1);'}
  {$EXTERNALSYM ID3D11BlendState1}
  ID3D11BlendState1 = interface(ID3D11BlendState)
    ['{cc86fabe-da55-401d-85e7-e3c9de2877e9}']

    procedure GetDesc1(out pDesc: D3D11_BLEND_DESC1); stdcall;

  end;
  IID_ID3D11BlendState1 = ID3D11BlendState1;
  {$EXTERNALSYM IID_ID3D11BlendState1}


  PD3D11_RASTERIZER_DESC1 = ^D3D11_RASTERIZER_DESC1;
  {$EXTERNALSYM D3D11_RASTERIZER_DESC1}
  D3D11_RASTERIZER_DESC1 = record
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
  end;


  // Interface ID3D11RasterizerState1
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11RasterizerState1);'}
  {$EXTERNALSYM ID3D11RasterizerState1}
  PID3D11RasterizerState1 = ^ID3D11RasterizerState1;
  ID3D11RasterizerState1 = interface(ID3D11RasterizerState)
    ['{1217d7a6-5039-418c-b042-9cbe256afd6e}']

    procedure GetDesc1(out pDesc: D3D11_RASTERIZER_DESC1); stdcall;
  end;
  IID_ID3D11RasterizerState1 = ID3D11RasterizerState1;
  {$EXTERNALSYM IID_ID3D11RasterizerState1}



  //////////////////////////////////////////////////////////////////////////////
  //
  // DeviceContextState
  //
  //////////////////////////////////////////////////////////////////////////////
  PD3D11_1_CREATE_DEVICE_CONTEXT_STATE_FLAG = ^D3D11_1_CREATE_DEVICE_CONTEXT_STATE_FLAG;
  D3D11_1_CREATE_DEVICE_CONTEXT_STATE_FLAG = (
    D3D11_1_CREATE_DEVICE_CONTEXT_STATE_SINGLETHREADED = $1
    );
  {$EXTERNALSYM D3D11_1_CREATE_DEVICE_CONTEXT_STATE_FLAG}


  // Interface ID3DDeviceContextState
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DDeviceContextState);'}
  {$EXTERNALSYM ID3DDeviceContextState}
  PID3DDeviceContextState = ^ID3DDeviceContextState;
  ID3DDeviceContextState = interface(ID3D11DeviceChild)
    ['{5c1e0d8a-7c23-48f9-8c59-a92958ceff11}']

  end;
  IID_ID3DDeviceContextState = ID3DDeviceContextState;
  {$EXTERNALSYM IID_ID3DDeviceContextState}


  //////////////////////////////////////////////////////////////////////////////
  //
  // DeviceContext
  //
  //////////////////////////////////////////////////////////////////////////////

  // Interface ID3D11DeviceContext1
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11DeviceContext1);'}
  {$EXTERNALSYM ID3D11DeviceContext1}
  ID3D11DeviceContext1 = interface(ID3D11DeviceContext)
    ['{bb2c6faa-b5fb-4082-8e6b-388b8cfa90e1}']

    procedure CopySubresourceRegion1(pDstResource: ID3D11Resource;
                                     DstSubresource: UINT;
                                     DstX: UINT;
                                     DstY: UINT;
                                     DstZ: UINT;
                                     pSrcResource: ID3D11Resource;
                                     SrcSubresource: UINT;
                                     {in_opt }pSrcBox: PD3D11_BOX;
                                     CopyFlags: UINT); stdcall;

    procedure UpdateSubresource1(pDstResource: ID3D11Resource;
                                 DstSubresource: UINT;
                                 {opt} pDstBox: PD3D11_BOX;
                                 pSrcData: Pointer;
                                 SrcRowPitch: UINT;
                                 SrcDepthPitch: UINT;
                                 CopyFlags: UINT); stdcall;

    procedure DiscardResource(pResource: ID3D11Resource); stdcall;

    procedure DiscardView (pResourceView: ID3D11View); stdcall;

    procedure VSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer {Array of constant buffers being given to the device.};
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure HSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure DSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure GSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure PSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure CSSetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure VSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure HSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure DSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure GSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure PSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure CSGetConstantBuffers1(StartSlot: UINT;
                                    NumBuffers: UINT;
                                    ppConstantBuffers: PID3D11Buffer;
                                    pFirstConstant: UINT;
                                    pNumConstants: UINT); stdcall;

    procedure SwapDeviceContextState(pState: ID3DDeviceContextState;
                                     {optional} out ppPreviousState: ID3DDeviceContextState); stdcall;

    procedure ClearView(pView: ID3D11View;
                        Color: FixedColorFLOATArray;
                        pRect: D3D11_RECT;
                        NumRects: UINT); stdcall;

    procedure DiscardView1(pResourceView: ID3D11View;
                           pRects: D3D11_RECT;
                           NumRects: UINT); stdcall;
  end;
  IID_ID3D11DeviceContext1 = ID3D11DeviceContext1;
  {$EXTERNALSYM IID_ID3D11DeviceContext1}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoDecoder
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_VIDEO_DECODER_SUB_SAMPLE_MAPPING_BLOCK = ^D3D11_VIDEO_DECODER_SUB_SAMPLE_MAPPING_BLOCK;
  D3D11_VIDEO_DECODER_SUB_SAMPLE_MAPPING_BLOCK = record
    ClearSize: UINT;
    EncryptedSize: UINT;
  end;
  {$EXTERNALSYM D3D11_VIDEO_DECODER_SUB_SAMPLE_MAPPING_BLOCK}


  PD3D11_VIDEO_DECODER_BUFFER_DESC1 = ^D3D11_VIDEO_DECODER_BUFFER_DESC1;
  D3D11_VIDEO_DECODER_BUFFER_DESC1 = record
    BufferType: D3D11_VIDEO_DECODER_BUFFER_TYPE;
    DataOffset: UINT;
    DataSize: UINT;
    IVSize: UINT;
    SubSampleMappingCount: UINT;
  end;
  {$EXTERNALSYM D3D11_VIDEO_DECODER_BUFFER_DESC1}


  PD3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION = ^D3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION;
  D3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION = record
    pCryptoSession: ID3D11CryptoSession;
    BlobSize: UINT;
    pBlob: Pointer;
    pKeyInfoId: TGUID;
    PrivateDataSize: UINT;
    pPrivateData: Pointer;
  end;
  TD3d11VideoDecoderBeginFrameCryptoSession = D3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION;
  {$EXTERNALSYM D3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION}


  PD3D11_VIDEO_DECODER_CAPS = ^D3D11_VIDEO_DECODER_CAPS;
  D3D11_VIDEO_DECODER_CAPS                       = (
    D3D11_VIDEO_DECODER_CAPS_DOWNSAMPLE          = $1,
    D3D11_VIDEO_DECODER_CAPS_NON_REAL_TIME       = $02,
    D3D11_VIDEO_DECODER_CAPS_DOWNSAMPLE_DYNAMIC  = $04,
    D3D11_VIDEO_DECODER_CAPS_DOWNSAMPLE_REQUIRED = $08,
    D3D11_VIDEO_DECODER_CAPS_UNSUPPORTED         = $10
  );
  {$EXTERNALSYM D3D11_VIDEO_DECODER_CAPS}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoProcessor
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_VIDEO_PROCESSOR_BEHAVIOR_HINTS = ^D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINTS;
  D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINTS                                            = (
    D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINT_MULTIPLANE_OVERLAY_ROTATION               = $01,
    D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINT_MULTIPLANE_OVERLAY_RESIZE                 = $02,
    D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINT_MULTIPLANE_OVERLAY_COLOR_SPACE_CONVERSION = $04,
    D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINT_TRIPLE_BUFFER_OUTPUT                      = $08
  );
  {$EXTERNALSYM D3D11_VIDEO_PROCESSOR_BEHAVIOR_HINTS}


  PD3D11_VIDEO_PROCESSOR_STREAM_BEHAVIOR_HINT = ^D3D11_VIDEO_PROCESSOR_STREAM_BEHAVIOR_HINT;
  D3D11_VIDEO_PROCESSOR_STREAM_BEHAVIOR_HINT = record
    Enable: BOOL;
    Width: UINT;
    Height: UINT;
    Format: DXGI_FORMAT;
  end;
  {$EXTERNALSYM D3D11_VIDEO_PROCESSOR_STREAM_BEHAVIOR_HINT}


  //////////////////////////////////////////////////////////////////////////////
  //
  // CryptoSession
  //
  //////////////////////////////////////////////////////////////////////////////

  PD3D11_CRYPTO_SESSION_STATUS = ^D3D11_CRYPTO_SESSION_STATUS;
  D3D11_CRYPTO_SESSION_STATUS                        = (
    D3D11_CRYPTO_SESSION_STATUS_OK                   = 0,
    D3D11_CRYPTO_SESSION_STATUS_KEY_LOST             = 1,
    D3D11_CRYPTO_SESSION_STATUS_KEY_AND_CONTENT_LOST = 2
  );
  {$EXTERNALSYM D3D11_CRYPTO_SESSION_STATUS}


  PD3D11_KEY_EXCHANGE_HW_PROTECTION_INPUT_DATA = ^D3D11_KEY_EXCHANGE_HW_PROTECTION_INPUT_DATA;
  D3D11_KEY_EXCHANGE_HW_PROTECTION_INPUT_DATA = record
    PrivateDataSize: UINT;
    HWProtectionDataSize: UINT;
    pbInput: array[0..3] of Byte;
  end;
  {$EXTERNALSYM D3D11_KEY_EXCHANGE_HW_PROTECTION_INPUT_DATA}


  PD3D11_KEY_EXCHANGE_HW_PROTECTION_OUTPUT_DATA = ^D3D11_KEY_EXCHANGE_HW_PROTECTION_OUTPUT_DATA;
  D3D11_KEY_EXCHANGE_HW_PROTECTION_OUTPUT_DATA = record
    PrivateDataSize: UINT;
    MaxHWProtectionDataSize: UINT;
    HWProtectionDataSize: UINT;
    TransportTime: UINT64;
    ExecutionTime: UINT64;
    pbOutput: array[0..3] of Byte;
  end;
  {$EXTERNALSYM D3D11_KEY_EXCHANGE_HW_PROTECTION_OUTPUT_DATA}


  PD3D11_KEY_EXCHANGE_HW_PROTECTION_DATA = ^D3D11_KEY_EXCHANGE_HW_PROTECTION_DATA;
  D3D11_KEY_EXCHANGE_HW_PROTECTION_DATA = record
    HWProtectionFunctionID: UINT;
    pInputData: D3D11_KEY_EXCHANGE_HW_PROTECTION_INPUT_DATA;
    pOutputData: D3D11_KEY_EXCHANGE_HW_PROTECTION_OUTPUT_DATA;
    Status: HResult;
  end;
  {$EXTERNALSYM D3D11_KEY_EXCHANGE_HW_PROTECTION_DATA}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoSample
  //
  //////////////////////////////////////////////////////////////////////////////
  PD3D11_VIDEO_SAMPLE_DESC = ^D3D11_VIDEO_SAMPLE_DESC;
  D3D11_VIDEO_SAMPLE_DESC = record
    Width: UINT;
    Height: UINT;
    Format: DXGI_FORMAT;
    ColorSpace: DXGI_COLOR_SPACE_TYPE;
  end;
  {$EXTERNALSYM D3D11_VIDEO_SAMPLE_DESC}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoContext1
  //
  //////////////////////////////////////////////////////////////////////////////

  // Interface ID3D11VideoContext1
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11VideoContext1);'}
  {$EXTERNALSYM ID3D11VideoContext1}
  ID3D11VideoContext1 = interface(ID3D11VideoContext)
  ['{A7F026DA-A5F8-4487-A564-15E34357651E}']

    function SubmitDecoderBuffers1(const pDecoder: ID3D11VideoDecoder;
                                   NumBuffers: UINT;
                                   pBufferDesc: D3D11_VIDEO_DECODER_BUFFER_DESC1): HResult; stdcall;

    function GetDataForNewHardwareKey(const pCryptoSession: ID3D11CryptoSession;
                                      PrivateInputSize: UINT;
                                      pPrivatInputData: Pointer;
                                      out pPrivateOutputData: PUINT64): HResult; stdcall;

    function  CheckCryptoSessionStatus(const pCryptoSession: ID3D11CryptoSession;
                                       out pStatus: D3D11_CRYPTO_SESSION_STATUS): HResult; stdcall;

    function  DecoderEnableDownsampling(const pDecoder: ID3D11VideoDecoder;
                                        InputColorSpace: DXGI_COLOR_SPACE_TYPE;
                                        pOutputDesc: D3D11_VIDEO_SAMPLE_DESC;
                                        ReferenceFrameCount: UINT): HResult; stdcall;

    function  DecoderUpdateDownsampling(const pDecoder: ID3D11VideoDecoder;
                                        pOutputDesc: D3D11_VIDEO_SAMPLE_DESC): HResult; stdcall;

    procedure VideoProcessorSetOutputColorSpace1(const pVideoProcessor: ID3D11VideoProcessor;
                                                 ColorSpace: DXGI_COLOR_SPACE_TYPE); stdcall;

    procedure VideoProcessorSetOutputShaderUsage(const pVideoProcessor: ID3D11VideoProcessor;
                                                 ShaderUsage: BOOL); stdcall;

    procedure VideoProcessorGetOutputColorSpace1(const pVideoProcessor: ID3D11VideoProcessor;
                                                 out pColorSpace: PDXGI_COLOR_SPACE_TYPE); stdcall;

    procedure VideoProcessorGetOutputShaderUsage(const pVideoProcessor: ID3D11VideoProcessor;
                                                 out pShaderUsage: BOOL); stdcall;

    procedure VideoProcessorSetStreamColorSpace1(const pVideoProcessor: ID3D11VideoProcessor;
                                                 const StreamIndex: UINT;
                                                 ColorSpace: DXGI_COLOR_SPACE_TYPE); stdcall;

    procedure VideoProcessorSetStreamMirror(const pVideoProcessor: ID3D11VideoProcessor;
                                            const StreamIndex: UINT;
                                            Enable: BOOL;
                                            FlipHorizontal: BOOL;
                                            FlipVertical: BOOL); stdcall;

    procedure VideoProcessorGetStreamColorSpace1(const pVideoProcessor: ID3D11VideoProcessor;
                                                 const StreamIndex: UINT;
                                                 out pColorSpace: PDXGI_COLOR_SPACE_TYPE); stdcall;

    procedure VideoProcessorGetStreamMirror(const pVideoProcessor: ID3D11VideoProcessor;
                                            const StreamIndex: UINT;
                                            out pEnable: BOOL;
                                            out pFlipHorizontal: BOOL;
                                            out pFlipVertical: BOOL); stdcall;

    function VideoProcessorGetBehaviorHints(const pVideoProcessor: ID3D11VideoProcessor;
                                            OutputWidth: UINT;
                                            OutputHeight: UINT;
                                            OutputFormat: DXGI_FORMAT;
                                            StreamCount: UINT;
                                            pStreams: D3D11_VIDEO_PROCESSOR_STREAM_BEHAVIOR_HINT;
                                            out pBehaviorHints: UINT): HResult; stdcall;
  end;
  IID_ID3D11VideoContext1 = ID3D11VideoContext1;
  {$EXTERNALSYM IID_ID3D11VideoContext1}


  //////////////////////////////////////////////////////////////////////////////
  //
  // Device
  //
  //////////////////////////////////////////////////////////////////////////////

  // Interface ID3D11VideoDevice1
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11VideoDevice1);'}
  {$EXTERNALSYM ID3D11VideoDevice1}
  ID3D11VideoDevice1 = interface(ID3D11VideoDevice)
  ['{29DA1D51-1321-4454-804B-F5FC9F861F0F}']

    function GetCryptoSessionPrivateDataSize(const pCryptoType: TGUID;
                                             const pDecoderProfile: TGUID;
                                             const pKeyExchangeType: TGUID;
                                             out pPrivateInputSize: UINT;
                                             out pPrivateOutputSize: UINT): HResult; stdcall;

    function GetVideoDecoderCaps(const pDecoderProfile: TGUID;
                                 SampleWidth: UINT;
                                 SampleHeight: UINT;
                                 pFrameRate: DXGI_RATIONAL;
                                 BitRate: UINT;
                                 const pCryptoType: TGUID;
                                 out pDecoderCaps: UINT): HResult; stdcall;

    function CheckVideoDecoderDownsampling(pInputDesc: D3D11_VIDEO_DECODER_DESC;
                                           InputColorSpace: DXGI_COLOR_SPACE_TYPE;
                                           pInputConfig: D3D11_VIDEO_DECODER_CONFIG;
                                           pFrameRate: DXGI_RATIONAL;
                                           pOutputDesc: D3D11_VIDEO_SAMPLE_DESC;
                                           out pSupported: BOOL;
                                           out pRealTimeHint: BOOL): HResult; stdcall;

    function RecommendVideoDecoderDownsampleParameters(pInputDesc: D3D11_VIDEO_DECODER_DESC;
                                                       InputColorSpace: DXGI_COLOR_SPACE_TYPE;
                                                       pInputConfig: D3D11_VIDEO_DECODER_CONFIG;
                                                       pFrameRate: DXGI_RATIONAL;
                                                       out pRecommendedOutputDesc: D3D11_VIDEO_SAMPLE_DESC): HResult; stdcall;
  end;
  IID_ID3D11VideoDevice1 = ID3D11VideoDevice1;
  {$EXTERNALSYM IID_ID3D11VideoDevice1}


  //////////////////////////////////////////////////////////////////////////////
  //
  // VideoProcessorEnum
  //
  //////////////////////////////////////////////////////////////////////////////

  // Interface ID3D11VideoProcessorEnumerator1
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11VideoProcessorEnumerator1);'}
  {$EXTERNALSYM ID3D11VideoProcessorEnumerator1}
  ID3D11VideoProcessorEnumerator1 = interface(ID3D11VideoProcessorEnumerator)
  ['{465217F2-5568-43CF-B5B9-F61D54531CA1}']

    function CheckVideoProcessorFormatConversion(InputFormat: DXGI_FORMAT;
                                                 InputColorSpace: DXGI_COLOR_SPACE_TYPE;
                                                 OutputFormat: DXGI_FORMAT;
                                                 OutputColorSpace: DXGI_COLOR_SPACE_TYPE;
                                                 pSupported: BOOL): HResult; stdcall;

  end;
  IID_ID3D11VideoProcessorEnumerator1 = ID3D11VideoProcessorEnumerator1;
  {$EXTERNALSYM IID_ID3D11VideoProcessorEnumerator1}


  // Interface ID3D11Device1
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11Device1);'}
  {$EXTERNALSYM ID3D11Device1}
  ID3D11Device1 = interface(ID3D11Device)
  ['{a04bfb29-08ef-43d6-a49c-a9bdbdcbe686}']

    procedure GetImmediateContext1([ref] const ppImmediateContext: ID3D11DeviceContext1); stdcall;

    function CreateDeferredContext1(ContextFlags: UINT;  // Reserved parameter; must be 0
                                   [ref] const ppDeferredContext: ID3D11DeviceContext1): HResult; stdcall;

    function CreateBlendState1(pBlendStateDesc: D3D11_BLEND_DESC1;
                               [ref] const ppBlendState: ID3D11BlendState1): HResult; stdcall;

    function CreateRasterizerState1(pRasterizerDesc: D3D11_RASTERIZER_DESC1;
                                    [ref] const ppRasterizerState: ID3D11RasterizerState1): HResult; stdcall;

    function CreateDeviceContextState(Flags: UINT;
                                      pFeatureLevels: D3D_FEATURE_LEVEL;
                                      FeatureLevels: UINT;
                                      SDKVersion: UINT;
                                      const EmulatedInterface: REFIID;
                                      out pChosenFeatureLevel: D3D_FEATURE_LEVEL;
                                      [ref] const ppContextState: ID3DDeviceContextState): HResult; stdcall;

    function OpenSharedResource1(const hResource: THandle;
                                 const returnedInterface: REFIID;
                                 [ref] const ppResource: Pointer): HResult; stdcall;

    function OpenSharedResourceByName(lpName: PWideChar;
                                      dwDesiredAccess: DWORD;
                                      const returnedInterface: REFIID;
                                      [ref] const ppResource: Pointer): HResult; stdcall;
  end;
  IID_ID3D11Device1 = ID3D11Device1;
  {$EXTERNALSYM IID_ID3D11Device1}


  // Interface ID3DUserDefinedAnnotation
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DUserDefinedAnnotation);'}
  {$EXTERNALSYM ID3DUserDefinedAnnotation}
  ID3DUserDefinedAnnotation = interface(IUnknown)
  ['{b2daad8b-03d4-4dbf-95eb-32ab4b63d0ab}']

    function BeginEvent(Name: PWideChar): INT; stdcall;

    function EndEvent( ): INT; stdcall;

    procedure SetMarker(Name: PWideChar); stdcall;

    function GetStatus(): BOOL; stdcall;
  end;
  IID_ID3DUserDefinedAnnotation = ID3DUserDefinedAnnotation;
  {$EXTERNALSYM IID_ID3DUserDefinedAnnotation}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
