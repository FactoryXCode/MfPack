// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D9.pas
// Kind: Pascal / Delphi unit
// Release date: 14-01-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Direct3D include file
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: New apps should use the latest Direct3D API
// 
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: d3d9.h
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
unit WinApi.DirectX.D3D9;

  {$HPPEMIT '#include "d3d9.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Types,
  {DirectX}
  WinApi.DirectX.D3d9Types,
  WinApi.DirectX.D3d9Caps;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


{$IFNDEF DIRECT3D_VERSION}
const
  DIRECT3D_VERSION                    = $0900;
  {$EXTERNALSYM DIRECT3D_VERSION}
{$ENDIF}  //DIRECT3D_VERSION

  //include this file content only if compiling for DX9 interfaces
  //#if(DIRECT3D_VERSION >= $0900)


  ///This identifier is passed to Direct3DCreate9 in order to ensure that an
  //* application was built against the correct header files. This number is
  //* incremented whenever a header (or other) change would require applications
  //* to be rebuilt. If the version doesn't match, Direct3DCreate9 will fail.
  //(The number itself has no meaning.)

  {$IFDEF D3D_DEBUG_INFO}
  D3D_SDK_VERSION                     = (32 or $80000000);
  {$EXTERNALSYM D3D_SDK_VERSION}
  D3D9b_SDK_VERSION                   = (31 or $80000000);
  {$EXTERNALSYM D3D9b_SDK_VERSION}
  {$ELSE}
  D3D_SDK_VERSION                     = 32;
  {$EXTERNALSYM D3D_SDK_VERSION}
  D3D9b_SDK_VERSION                   = 31;
  {$EXTERNALSYM D3D9b_SDK_VERSION}
  {$ENDIF}

type
 {IFDEF HMONITOR_DECLARED AND (WINVER < $0500)}
  {$EXTERNALSYM HMONITOR}
  HMONITOR = THandle;
 {DEFINE HMONITOR_DECLARED}
 {ENDIF}


type

  // Forward Interface declarations
  // ==============================

//  PIDirect3D9 = ^IDirect3D9;
//  IDirect3D9 = interface;

  PIDirect3DDevice9 = ^IDirect3DDevice9;
  IDirect3DDevice9 = interface;

  PIDirect3DStateBlock9 = ^IDirect3DStateBlock9;
  IDirect3DStateBlock9 = interface;

  PIDirect3DVertexDeclaration9 = ^IDirect3DVertexDeclaration9;
  IDirect3DVertexDeclaration9 = interface;

  PIDirect3DVertexShader9 = ^IDirect3DVertexShader9;
  IDirect3DVertexShader9 = interface;

  PIDirect3DPixelShader9 = ^IDirect3DPixelShader9;
  IDirect3DPixelShader9 = interface;

  PIDirect3DResource9 = ^IDirect3DResource9;
  IDirect3DResource9 = interface;

  PIDirect3DBaseTexture9 = ^IDirect3DBaseTexture9;
  IDirect3DBaseTexture9 = interface;

  PIDirect3DTexture9 = ^IDirect3DTexture9;
  IDirect3DTexture9 = interface;

  PIDirect3DVolumeTexture9 = ^IDirect3DVolumeTexture9;
  IDirect3DVolumeTexture9 = interface;

  PIDirect3DCubeTexture9 = ^IDirect3DCubeTexture9;
  IDirect3DCubeTexture9 = interface;

  PIDirect3DVertexBuffer9 = ^IDirect3DVertexBuffer9;
  IDirect3DVertexBuffer9 = interface;

  PIDirect3DIndexBuffer9 = ^IDirect3DIndexBuffer9;
  IDirect3DIndexBuffer9 = interface;

  PIDirect3DSurface9 = ^IDirect3DSurface9;
  IDirect3DSurface9 = interface;

  PIDirect3DVolume9 = ^IDirect3DVolume9;
  IDirect3DVolume9 = interface;

  PIDirect3DSwapChain9 = ^IDirect3DSwapChain9;
  IDirect3DSwapChain9 = interface;

  PIDirect3DQuery9 = ^IDirect3DQuery9;
  IDirect3DQuery9 = interface;



  // Interfaces



  // Interface IDirect3D9
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3D9);'}
  {$EXTERNALSYM IDirect3D9}
  IDirect3D9 = interface(IUnknown)
  ['{81bdcbca-64d4-426d-ae8d-ad0147f4275c}']

    //*** IDirect3D9 methods ***//
    function RegisterSoftwareDevice(pInitializeFunction: Pointer): HResult; stdcall;

    function GetAdapterCount: DWORD; stdcall;

    function GetAdapterIdentifier(Adapter: DWORD;
                                  Flags: DWORD;
                                  out pIdentifier: D3DADAPTER_IDENTIFIER9): HResult; stdcall;

    function GetAdapterModeCount(Adapter: DWORD;
                                 Format: D3DFORMAT): DWORD; stdcall;

    function EnumAdapterModes(Adapter: DWORD;
                              Format: D3DFORMAT;
                              Mode: DWORD;
                              out pMode: D3DDISPLAYMODE): HResult; stdcall;

    function GetAdapterDisplayMode(Adapter: DWORD;
                                   out pMode: D3DDISPLAYMODE): HResult; stdcall;

    function CheckDeviceType(Adapter: DWORD;
                             CheckType: D3DDEVTYPE;
                             AdapterFormat: D3DFORMAT;
                             BackBufferFormat: D3DFORMAT;
                             Windowed: BOOL): HResult; stdcall;

    function CheckDeviceFormat(Adapter: DWORD;
                               DeviceType: D3DDEVTYPE;
                               AdapterFormat: D3DFORMAT;
                               Usage: DWORD;
                               RType: D3DResourceType;
                               CheckFormat: D3DFORMAT): HResult; stdcall;

    function CheckDeviceMultiSampleType(Adapter: DWORD;
                                        DeviceType: D3DDEVTYPE;
                                        SurfaceFormat: D3DFORMAT;
                                        Windowed: BOOL;
                                        MultiSampleType: D3DMULTISAMPLE_TYPE;
                                        pQualityLevels: PDWORD): HResult; stdcall;

    function CheckDepthStencilMatch(Adapter: DWORD;
                                    DeviceType: D3DDEVTYPE;
                                    AdapterFormat: D3DFORMAT;
                                    RenderTargetFormat: D3DFORMAT;
                                    DepthStencilFormat: D3DFORMAT): HResult; stdcall;

    function CheckDeviceFormatConversion(Adapter: DWORD;
                                         DeviceType: D3DDEVTYPE;
                                         SourceFormat: D3DFORMAT;
                                         TargetFormat: D3DFORMAT): HResult; stdcall;

    function GetDeviceCaps(Adapter: DWORD;
                           DeviceType: D3DDEVTYPE;
                           out pCaps: D3DCAPS9): HResult; stdcall;

    function GetAdapterMonitor(Adapter: DWORD): HMONITOR; stdcall;

    function CreateDevice(Adapter: DWORD;
                          DeviceType: D3DDEVTYPE;
                          hFocusWindow: HWND;
                          BehaviorFlags: DWORD;
                          pPresentationParameters: PD3DPRESENT_PARAMETERS;
                          out ppReturnedDeviceInterface: IDirect3DDevice9): HResult; stdcall;
  end;
  IID_IDirect3D9 = IDirect3D9;
  {$EXTERNALSYM IID_IDirect3D9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3D9_DEBUG_INFO = class
    Version: PWideChar;
  end;
  {$EXTERNALSYM IDIRECT3D9_DEBUG_INFO}



  // Interface IDirect3DDevice9
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DDevice9);'}
  {$EXTERNALSYM IDirect3DDevice9}
  IDirect3DDevice9 = interface(IUnknown)
  ['{d0223b96-bf7a-43fd-92bd-a43b0d82b9eb}']
    //*** IDirect3DDevice9 methods ***//
    function TestCooperativeLevel(): HResult; stdcall;

    function GetAvailableTextureMem(): DWORD; stdcall;

    function EvictManagedResources(): HResult; stdcall;

    function GetDirect3D(out ppD3D9: IDirect3D9): HResult; stdcall;

    function GetDeviceCaps(out pCaps: D3DCAPS9): HResult; stdcall;

    function GetDisplayMode(iSwapChain: DWORD;
                            out pMode: D3DDISPLAYMODE): HResult; stdcall;

    function GetCreationParameters(out pParameters: D3DDEVICE_CREATION_PARAMETERS): HResult; stdcall;

    function SetCursorProperties(XHotSpot: DWORD;
                                 YHotSpot: DWORD;
                                 pCursorBitmap: IDirect3DSurface9): HResult; stdcall;

    procedure SetCursorPosition(XScreenSpace: DWORD;
                                YScreenSpace: DWORD;
                                Flags: DWORD); stdcall;

    function ShowCursor(bShow: BOOL): BOOL; stdcall;

    function CreateAdditionalSwapChain(const pPresentationParameters: D3DPRESENT_PARAMETERS;
                                       out pSwapChain: IDirect3DSwapChain9): HResult; stdcall;

    function GetSwapChain(iSwapChain: DWORD;
                          out pSwapChain: IDirect3DSwapChain9): HResult; stdcall;

    function GetNumberOfSwapChains: DWORD; stdcall;

    function Reset(const pPresentationParameters: D3DPRESENT_PARAMETERS): HResult; stdcall;

    function Present(pSourceRect: PRect;
                     pDestRect: PRect;
                     hDestWindowOverride: HWND;
                     pDirtyRegion: PRgnData): HResult; stdcall;

    function GetBackBuffer(iSwapChain: DWORD;
                           iBackBuffer: DWORD;
                           const _Type: D3DBACKBUFFER_TYPE;
                           out ppBackBuffer: IDirect3DSurface9): HResult; stdcall;

    function GetRasterStatus(iSwapChain: DWORD;
                             out pRasterStatus: D3DRASTER_STATUS): HResult; stdcall;

    function SetDialogBoxMode(bEnableDialogs: BOOL): HResult; stdcall;

    procedure SetGammaRamp(iSwapChain: DWORD;
                           Flags: DWORD;
                           const pRamp: D3DGammaRamp); stdcall;

    procedure GetGammaRamp(iSwapChain: DWORD;
                           out pRamp: D3DGammaRamp); stdcall;

    function CreateTexture(Width: DWORD;
                           Height: DWORD;
                           Levels: DWORD;
                           Usage: DWORD;
                           Format: D3DFORMAT;
                           Pool: D3DPOOL;
                           out ppTexture: IDirect3DTexture9;
                           pSharedHandle: PHandle): HResult; stdcall;

    function CreateVolumeTexture(Width: DWORD;
                                 Height: DWORD;
                                 Depth: DWORD;
                                 Levels: DWORD;
                                 Usage: DWORD;
                                 Format: D3DFORMAT;
                                 Pool: D3DPOOL;
                                 out ppVolumeTexture: IDirect3DVolumeTexture9;
                                 pSharedHandle: PHandle): HResult; stdcall;

    function CreateCubeTexture(EdgeLength: DWORD;
                               Levels: DWORD;
                               Usage: DWORD;
                               Format: D3DFORMAT;
                               Pool: D3DPOOL;
                               out ppCubeTexture: IDirect3DCubeTexture9;
                               pSharedHandle: PHandle): HResult; stdcall;

    function CreateVertexBuffer(Length: DWORD;
                                Usage: DWORD;
                                FVF: DWORD;
                                Pool: D3DPOOL;
                                out ppVertexBuffer: IDirect3DVertexBuffer9;
                                pSharedHandle: PHandle): HResult; stdcall;

    function CreateIndexBuffer(Length: DWORD;
                               Usage: DWORD;
                               Format: D3DFORMAT;
                               Pool: D3DPOOL;
                               out ppIndexBuffer: IDirect3DIndexBuffer9;
                               pSharedHandle: PHandle): HResult; stdcall;

    function CreateRenderTarget(Width,
                                Height: DWORD;
                                Format: D3DFORMAT;
                                MultiSample: D3DMULTISAMPLE_TYPE;
                                MultisampleQuality: DWORD;
                                Lockable: BOOL;
                                out ppSurface: IDirect3DSurface9;
                                pSharedHandle: PHandle): HResult; stdcall;

    function CreateDepthStencilSurface(Width: DWORD;
                                       Height: DWORD;
                                       Format: D3DFORMAT;
                                       MultiSample: D3DMULTISAMPLE_TYPE;
                                       MultisampleQuality: DWORD;
                                       Discard: BOOL;
                                       out ppSurface: IDirect3DSurface9;
                                       pSharedHandle: PHandle): HResult; stdcall;

    function UpdateSurface(pSourceSurface: IDirect3DSurface9;
                           pSourceRect: PRect;
                           pDestinationSurface: IDirect3DSurface9;
                           pDestPoint: PPoint): HResult; stdcall;

    function UpdateTexture(pSourceTexture: IDirect3DBaseTexture9;
                           pDestinationTexture: IDirect3DBaseTexture9): HResult; stdcall;

    function GetRenderTargetData(pRenderTarget: IDirect3DSurface9;
                                 pDestSurface: IDirect3DSurface9): HResult; stdcall;

    function GetFrontBufferData(iSwapChain: DWORD;
                                pDestSurface: IDirect3DSurface9): HResult; stdcall;

    function StretchRect(pSourceSurface: IDirect3DSurface9;
                         pSourceRect: PRect;
                         pDestSurface: IDirect3DSurface9;
                         pDestRect: PRect;
                         Filter: D3DTEXTUREFILTERTYPE): HResult; stdcall;

    function ColorFill(pSurface: IDirect3DSurface9;
                       pRect: PRect;
                       color: D3DCOLOR): HResult; stdcall;

    function CreateOffscreenPlainSurface(Width: DWORD;
                                         Height: DWORD;
                                         Format: D3DFORMAT;
                                         Pool: D3DPOOL;
                                         out ppSurface: IDirect3DSurface9;
                                         pSharedHandle: PHandle): HResult; stdcall;

    function SetRenderTarget(RenderTargetIndex: DWORD;
                             pRenderTarget: IDirect3DSurface9): HResult; stdcall;

    function GetRenderTarget(RenderTargetIndex: DWORD;
                             out ppRenderTarget: IDirect3DSurface9): HResult; stdcall;

    function SetDepthStencilSurface(pNewZStencil: IDirect3DSurface9): HResult; stdcall;

    function GetDepthStencilSurface(out ppZStencilSurface: IDirect3DSurface9): HResult; stdcall;

    function BeginScene: HResult; stdcall;

    function EndScene: HResult; stdcall;

    function Clear(Count: DWORD;
                   pRects: PD3DRECT;
                   Flags: DWORD;
                   Color: D3DCOLOR;
                   Z: Single;
                   Stencil: DWORD): HResult; stdcall;

    function SetTransform(State: D3DTRANSFORMSTATETYPE;
                          const pMatrix: D3DMATRIX): HResult; stdcall;

    function GetTransform(State: D3DTRANSFORMSTATETYPE;
                          out pMatrix: D3DMATRIX): HResult; stdcall;

    function MultiplyTransform(State: D3DTRANSFORMSTATETYPE;
                               const pMatrix: D3DMATRIX): HResult; stdcall;

    function SetViewport(const pViewport: D3DVIEWPORT9): HResult; stdcall;

    function GetViewport(out pViewport: D3DVIEWPORT9): HResult; stdcall;

    function SetMaterial(const pMaterial: D3DMATERIAL9): HResult; stdcall;

    function GetMaterial(out pMaterial: D3DMATERIAL9): HResult; stdcall;

    function SetLight(Index: DWORD;
                      const pLight: D3DLIGHT9): HResult; stdcall;

    function GetLight(Index: DWORD;
                      out pLight: D3DLIGHT9): HResult; stdcall;

    function LightEnable(Index: DWORD;
                         Enable: BOOL): HResult; stdcall;

    function GetLightEnable(Index: DWORD;
                            out pEnable: BOOL): HResult; stdcall;

    function SetClipPlane(Index: DWORD;
                          pPlane: PSingle): HResult; stdcall;

    function GetClipPlane(Index: DWORD;
                          pPlane: PSingle): HResult; stdcall;

    function SetRenderState(State: D3DRENDERSTATETYPE;
                            Value: DWORD): HResult; stdcall;

    function GetRenderState(State: D3DRENDERSTATETYPE;
                            out pValue: DWORD): HResult; stdcall;

    function CreateStateBlock(_Type: D3DSTATEBLOCKTYPE;
                              out ppSB: IDirect3DStateBlock9): HResult; stdcall;

    function BeginStateBlock: HResult; stdcall;

    function EndStateBlock(out ppSB: IDirect3DStateBlock9): HResult; stdcall;

    function SetClipStatus(const pClipStatus: D3DCLIPSTATUS9): HResult; stdcall;

    function GetClipStatus(out pClipStatus: D3DCLIPSTATUS9): HResult; stdcall;

    function GetTexture(Stage: DWORD;
                        out ppTexture: IDirect3DBaseTexture9): HResult; stdcall;

    function SetTexture(Stage: DWORD; pTexture: IDirect3DBaseTexture9): HResult; stdcall;

    function GetTextureStageState(Stage: UINT;
                                  _Type: D3DTEXTURESTAGESTATETYPE;
                                  out pValue: DWORD): HResult; stdcall;

    function SetTextureStageState(Stage: UINT;
                                  _Type: D3DTEXTURESTAGESTATETYPE;
                                  Value: DWORD): HResult; stdcall;

    function GetSamplerState(Sampler: DWORD;
                             _Type: D3DSAMPLERSTATETYPE;
                             out pValue: DWORD): HResult; stdcall;

    function SetSamplerState(Sampler: UINT;
                             _Type: D3DSAMPLERSTATETYPE;
                             Value: DWORD): HResult; stdcall;

    function ValidateDevice(out pNumPasses: DWORD): HResult; stdcall;

    function SetPaletteEntries(PaletteNumber: UINT;
                               pEntries: PPALETTEENTRY): HResult; stdcall;

    function GetPaletteEntries(PaletteNumber: UINT;
                               pEntries: PPALETTEENTRY): HResult; stdcall;

    function SetCurrentTexturePalette(PaletteNumber: UINT): HResult; stdcall;

    function GetCurrentTexturePalette(out PaletteNumber: UINT): HResult; stdcall;

    function SetScissorRect(pRect: PRect): HResult; stdcall;

    function GetScissorRect(out pRect: TRect): HResult; stdcall;

    function SetSoftwareVertexProcessing(bSoftware: BOOL): HResult; stdcall;

    function GetSoftwareVertexProcessing: BOOL; stdcall;

    function SetNPatchMode(nSegments: Single): HResult; stdcall;

    function GetNPatchMode: Single; stdcall;

    function DrawPrimitive(PrimitiveType: D3DPRIMITIVETYPE;
                           StartVertex: UINT;
                           PrimitiveCount: UINT): HResult; stdcall;

    function DrawIndexedPrimitive(_Type: D3DPRIMITIVETYPE;
                                  BaseVertexIndex: Integer;
                                  MinVertexIndex: UINT;
                                  NumVertices: UINT;
                                  startIndex: UINT;
                                  primCount: UINT): HResult; stdcall;

    function DrawPrimitiveUP(PrimitiveType: D3DPRIMITIVETYPE;
                             PrimitiveCount: UINT;
                             const pVertexStreamZeroData;
                             VertexStreamZeroStride: UINT): HResult; stdcall;

    function DrawIndexedPrimitiveUP(PrimitiveType: D3DPRIMITIVETYPE;
                                    MinVertexIndex: UINT;
                                    NumVertice: UINT;
                                    PrimitiveCount: UINT;
                                    const pIndexData; IndexDataFormat: D3DFORMAT;
                                    const pVertexStreamZeroData;
                                    VertexStreamZeroStride: UINT): HResult; stdcall;

    function ProcessVertices(SrcStartIndex,
                             DestIndex: UINT;
                             VertexCount: UINT;
                             pDestBuffer: IDirect3DVertexBuffer9;
                             pVertexDecl: IDirect3DVertexDeclaration9;
                             Flags: UINT): HResult; stdcall;

    function CreateVertexDeclaration(pVertexElements: PD3DVertexElement9;
                                     out ppDecl: IDirect3DVertexDeclaration9): HResult; stdcall;

    function SetVertexDeclaration(pDecl: IDirect3DVertexDeclaration9): HResult; stdcall;

    function GetVertexDeclaration(out ppDecl: IDirect3DVertexDeclaration9): HResult; stdcall;

    function SetFVF(FVF: DWORD): HResult; stdcall;

    function GetFVF(out FVF: DWORD): HResult; stdcall;

    function CreateVertexShader(pFunction: PDWord;
                                out ppShader: IDirect3DVertexShader9): HResult; stdcall;

    function SetVertexShader(pShader: IDirect3DVertexShader9): HResult; stdcall;

    function GetVertexShader(out ppShader: IDirect3DVertexShader9): HResult; stdcall;

    function SetVertexShaderConstantF(StartRegister: UINT;
                                      pConstantData: PSingle;
                                      Vector4fCount: UINT): HResult; stdcall;

    function GetVertexShaderConstantF(StartRegister: UINT;
                                      pConstantData: PSingle;
                                      Vector4fCount: UINT): HResult; stdcall;

    function SetVertexShaderConstantI(StartRegister: UINT;
                                      pConstantData: PInteger;
                                      Vector4iCount: UINT): HResult; stdcall;

    function GetVertexShaderConstantI(StartRegister: UINT;
                                      pConstantData: PInteger;
                                      Vector4iCount: UINT): HResult; stdcall;

    function SetVertexShaderConstantB(StartRegister: UINT;
                                     pConstantData: PBOOL;
                                     BoolCount: UINT): HResult; stdcall;

    function GetVertexShaderConstantB(StartRegister: UINT;
                                      pConstantData: PBOOL;
                                      BoolCount: UINT): HResult; stdcall;

    function SetStreamSource(StreamNumber: UINT;
                             pStreamData: IDirect3DVertexBuffer9;
                             OffsetInBytes: UINT;
                             Stride: UINT): HResult; stdcall;

    function GetStreamSource(StreamNumber: UINT;
                             out ppStreamData: IDirect3DVertexBuffer9;
                             out pOffsetInBytes: UINT;
                             pStride: UINT): HResult; stdcall;

    function SetStreamSourceFreq(StreamNumber: UINT;
                                 Setting: UINT): HResult; stdcall;

    function GetStreamSourceFreq(StreamNumber: UINT;
                                 out Setting: UINT): HResult; stdcall;

    function SetIndices(pIndexData: IDirect3DIndexBuffer9): HResult; stdcall;

    function GetIndices(out ppIndexData: IDirect3DIndexBuffer9): HResult; stdcall;

    function CreatePixelShader(pFunction: PDWORD;
                               out ppShader: IDirect3DPixelShader9): HResult; stdcall;

    function SetPixelShader(pShader: IDirect3DPixelShader9): HResult; stdcall;

    function GetPixelShader(out ppShader: IDirect3DPixelShader9): HResult; stdcall;

    function SetPixelShaderConstantF(StartRegister: UINT;
                                     pConstantData: PSingle;
                                     Vector4fCount: UINT): HResult; stdcall;

    function GetPixelShaderConstantF(StartRegister: UINT;
                                     pConstantData: PSingle;
                                     Vector4fCount: UINT): HResult; stdcall;

    function SetPixelShaderConstantI(StartRegister: UINT;
                                     pConstantData: PInteger;
                                     Vector4iCount: UINT): HResult; stdcall;

    function GetPixelShaderConstantI(StartRegister: UINT;
                                     pConstantData: PInteger;
                                     Vector4iCount: UINT): HResult; stdcall;

    function SetPixelShaderConstantB(StartRegister: UINT;
                                     pConstantData: PBOOL;
                                     BoolCount: DWORD): HResult; stdcall;

    function GetPixelShaderConstantB(StartRegister: UINT;
                                     pConstantData: PBOOL;
                                     BoolCount: UINT): HResult; stdcall;

    function DrawRectPatch(Handle: UINT;
                           pNumSegs: PSingle;
                           pTriPatchInfo: PD3DRECTPATCH_INFO): HResult; stdcall;

    function DrawTriPatch(Handle: UINT;
                          pNumSegs: PSingle;
                          pTriPatchInfo: PD3DRECTPATCH_INFO): HResult; stdcall;

    function DeletePatch(Handle: UINT): HResult; stdcall;

    function CreateQuery(_Type: D3DQUERYTYPE;
                         out ppQuery: IDirect3DQuery9): HResult; stdcall;

  end;
  IID_IDirect3DDevice9 = IDirect3DDevice9;
  {$EXTERNALSYM IID_IDirect3DDevice9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DDEVICE9_DEBUG_INFO = class
    CreationParameters: D3DDEVICE_CREATION_PARAMETERS;
    PresentParameters: D3DPRESENT_PARAMETERS;
    DisplayMode: D3DDISPLAYMODE;
    Caps: D3DCAPS9;

    AvailableTextureMem: UINT;
    SwapChains: UINT;
    Textures: UINT;
    VertexBuffers: UINT;
    IndexBuffers: UINT;
    VertexShaders: UINT;
    PixelShaders: UINT;

    Viewport: D3DVIEWPORT9;
    ProjectionMatrix: D3DMATRIX;
    ViewMatrix: D3DMATRIX;
    WorldMatrix: D3DMATRIX;
    TextureMatrices: array [0..7] of D3DMATRIX;

    FVF: DWORD;
    VertexSize: UINT;
    VertexShaderVersion: DWORD;
    PixelShaderVersion: DWORD;
    SoftwareVertexProcessing: BOOL;

    Material: D3DMATERIAL9;
    Lights: array [0..15] of D3DLIGHT9;
    LightsEnabled: array [0..15] of BOOL;

    GammaRamp: D3DGAMMARAMP;
    ScissorRect: TRect;
    DialogBoxMode: BOOL;
  end;
  {$EXTERNALSYM IDIRECT3DDEVICE9_DEBUG_INFO}



  // Interface IDirect3DStateBlock9
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DStateBlock9);'}
  {$EXTERNALSYM IDirect3DStateBlock9}
  IDirect3DStateBlock9 = interface(IUnknown)
  ['{B07C4FE5-310D-4ba8-A23C-4F0F206F218B}']
     //*** IDirect3DStateBlock9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;
    function Capture(): HResult; stdcall;
    function Apply(): HResult; stdcall;
  end;
  IID_IDirect3DStateBlock9 = IDirect3DStateBlock9;
  {$EXTERNALSYM IID_IDirect3DStateBlock9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DSTATEBLOCK9_DEBUG_INFO = class
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DSTATEBLOCK9_DEBUG_INFO}



  // Interface IDirect3DSwapChain9
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DSwapChain9);'}
  {$EXTERNALSYM IDirect3DSwapChain9}
  IDirect3DSwapChain9 = interface(IUnknown)
  ['{794950F2-ADFC-458a-905E-10A10B0B503B}']
    //*** IDirect3DSwapChain9 methods ***//
    function Present(pSourceRect: PRect;
                     pDestRect: PRect;
                     hDestWindowOverride: HWND;
                     pDirtyRegion: PRgnData;
                     dwFlags: DWORD): HResult; stdcall;

    function GetFrontBufferData(pDestSurface: IDirect3DSurface9): HResult; stdcall;

    function GetBackBuffer(iBackBuffer: UINT;
                           _Type: D3DBackBuffer_Type;
                           out ppBackBuffer: IDirect3DSurface9): HResult; stdcall;

    function GetRasterStatus(out pRasterStatus: D3DRASTER_STATUS): HResult; stdcall;

    function GetDisplayMode(out pMode: D3DDISPLAYMODE): HResult; stdcall;

    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function GetPresentParameters(out pPresentationParameters: D3DPRESENT_PARAMETERS): HResult; stdcall;
  end;
  IID_IDirect3DSwapChain9 = IDirect3DSwapChain9;
  {$EXTERNALSYM IID_IDirect3DSwapChain9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DSWAPCHAIN9_DEBUG_INFO = class
    PresentParameters: D3DPRESENT_PARAMETERS;
    DisplayMode: D3DDISPLAYMODE;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DSWAPCHAIN9_DEBUG_INFO}



  // Interface IDirect3DResource9
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DResource9);'}
  {$EXTERNALSYM IDirect3DResource9}
  IDirect3DResource9 = interface(IUnknown)
  ['{05EEC05D-8F7D-4362-B999-D1BAF357C704}']
    //*** IDirect3DResource9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function SetPrivateData(const refguid: TGUID;
                            const pData: PVOID;
                            SizeOfData,
                            Flags: DWORD): HResult; stdcall;

    function GetPrivateData(const refguid: TGUID;
                            pData: PVOID;
                            out pSizeOfData: DWORD): HResult; stdcall;

    function FreePrivateData(const refguid: TGUID): HResult; stdcall;

    function SetPriority(PriorityNew: DWORD): DWORD; stdcall;

    function GetPriority(): DWORD; stdcall;

    procedure PreLoad(); stdcall;

    function GetType(): D3DRESOURCETYPE; stdcall;
  end;
  IID_IDirect3DResource9 = IDirect3DResource9;
  {$EXTERNALSYM IID_IDirect3DResource9}



  // Interface IDirect3DVertexDeclaration9
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DVertexDeclaration9);'}
  {$EXTERNALSYM IDirect3DVertexDeclaration9}
  IDirect3DVertexDeclaration9 = interface(IUnknown)
  ['{DD13C59C-36FA-4098-A8FB-C7ED39DC8546}']
    //*** IDirect3DVertexDeclaration9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function GetDeclaration(pElement: PD3DVertexElement9;
                            out pNumElements: UINT): HResult; stdcall;
  end;
  IID_IDirect3DVertexDeclaration9 = IDirect3DVertexDeclaration9;
  {$EXTERNALSYM IID_IDirect3DVertexDeclaration9}



  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DVERTEXDECLARATION9_DEBUG_INFO = class
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DVERTEXDECLARATION9_DEBUG_INFO}



  // Interface IDirect3DVertexShader9
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DVertexShader9);'}
  {$EXTERNALSYM IDirect3DVertexShader9}
  IDirect3DVertexShader9 = interface(IUnknown)
  ['{EFC5557E-6265-4613-8A94-43857889EB36}']
    //*** IDirect3DVertexShader9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function GetFunction(pData: Pointer;
                         out pSizeOfData: UINT): HResult; stdcall;
  end;
  IID_IDirect3DVertexShader9 = IDirect3DVertexShader9;
  {$EXTERNALSYM IID_IDirect3DVertexShader9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DVERTEXSHADER9_DEBUG_INFO = class
    Version: DWORD;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DVERTEXSHADER9_DEBUG_INFO}



  // Interface IDirect3DPixelShader9
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DPixelShader9);'}
  {$EXTERNALSYM IDirect3DPixelShader9}
  IDirect3DPixelShader9 = interface(IUnknown)
  ['{6D3BDBDC-5B02-4415-B852-CE5E8BCCB289}']
    //*** IDirect3DPixelShader9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function GetFunction(pData: Pointer;
                         out pSizeOfData: UINT): HResult; stdcall;
  end;
  IID_IDirect3DPixelShader9 = IDirect3DPixelShader9;
  {$EXTERNALSYM IID_IDirect3DPixelShader9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DPIXELSHADER9_DEBUG_INFO = class
    Version: DWORD;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DPIXELSHADER9_DEBUG_INFO}


  // Interface IDirect3DBaseTexture9
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DBaseTexture9);'}
  {$EXTERNALSYM IDirect3DBaseTexture9}
  IDirect3DBaseTexture9 = interface(IDirect3DResource9)
  ['{580ca87e-1d3c-4d54-991d-b7d3e3c298ce}']
    //*** IDirect3DBaseTexture9 methods ***//
    function SetLOD(LODNew: DWORD): DWORD; stdcall;

    function GetLOD(): DWORD; stdcall;

    function GetLevelCount(): DWORD; stdcall;

    function SetAutoGenFilterType(FilterType: D3DTEXTUREFILTERTYPE): HResult; stdcall;

    function GetAutoGenFilterType(): D3DTEXTUREFILTERTYPE; stdcall;

    procedure GenerateMipSubLevels();
  end;
  IID_IDirect3DBaseTexture9 = IDirect3DBaseTexture9;
  {$EXTERNALSYM IID_IDirect3DBaseTexture9}



  // Interface IDirect3DTexture9
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DTexture9);'}
  {$EXTERNALSYM IDirect3DTexture9}
  IDirect3DTexture9 = interface(IDirect3DBaseTexture9)
  ['{85c31227-3de5-4f00-9b3a-f11ac38c18b5}']
    //*** IDirect3DTexture9 methods ***//
    function GetLevelDesc(Level: UINT;
                          out pDesc: D3DSURFACE_DESC): HResult; stdcall;

    function GetSurfaceLevel(Level: UINT;
                             out ppSurfaceLevel: IDirect3DSurface9): HResult; stdcall;

    function LockRect(Level: UINT;
                      out pLockedRect: D3DLOCKED_RECT;
                      pRect: PRect;
                      Flags: DWORD): HResult; stdcall;

    function UnlockRect(Level: UINT): HResult; stdcall;

    function AddDirtyRect(pDirtyRect: PRect): HResult; stdcall;
  end;
  IID_IDirect3DTexture9 = IDirect3DTexture9;
  {$EXTERNALSYM IID_IDirect3DTexture9}



  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDirect3DTexture9_DEBUG_INFO = class
    Name: LPCWSTR;
    Width: UINT;
    Height: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: D3DFORMAT;
    Pool: D3DPOOL;
    Priority: DWORD;
    LOD: DWORD;
    FilterType: D3DTEXTUREFILTERTYPE;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDirect3DTexture9_DEBUG_INFO}



  // Interface IDirect3DVolumeTexture9
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DVolumeTexture9);'}
  {$EXTERNALSYM IDirect3DVolumeTexture9}
  IDirect3DVolumeTexture9 = interface(IDirect3DBaseTexture9)
  ['{2518526C-E789-4111-A7B9-47EF328D13E6}']
    //*** IDirect3DVolumeTexture9 methods ***//
    function GetLevelDesc(Level: UINT;
                          out pDesc: D3DVOLUME_DESC): HResult; stdcall;

    function GetVolumeLevel(Level: UINT;
                            out ppVolumeLevel: IDirect3DVolume9): HResult; stdcall;

    function LockBox(Level: UINT;
                     out pLockedVolume: D3DLOCKED_BOX;
                     pBox: PD3DBOX; // Specifying Nil for this parameter locks the entire volume level.
                     Flags: DWORD): HResult; stdcall;

    function UnlockBox(Level: UINT): HResult; stdcall;

    function AddDirtyBox(pDirtyBox: PD3DBox): HResult; stdcall; // Specifying Nil expands the dirty region to cover the entire volume texture.
  end;
  IID_IDirect3DVolumeTexture9 = IDirect3DVolumeTexture9;
  {$EXTERNALSYM IID_IDirect3DVolumeTexture9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDirect3DVolumeTexture9_DEBUG_INFO = class
    Name: LPCWSTR;
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: D3DFORMAT;
    Pool: D3DPOOL;
    Priority: DWORD;
    LOD: DWORD;
    FilterType: D3DTEXTUREFILTERTYPE;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDirect3DVolumeTexture9_DEBUG_INFO}



  // Interface IDirect3DCubeTexture9
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DCubeTexture9);'}
  {$EXTERNALSYM IDirect3DCubeTexture9}
  IDirect3DCubeTexture9 = interface(IDirect3DBaseTexture9)
  ['{fff32f81-d953-473a-9223-93d652aba93f}']
    //*** IDirect3DCubeTexture9 methods ***//
    function GetLevelDesc(Level: UINT;
                          out pDesc: D3DSURFACE_DESC): HResult; stdcall;

    function GetCubeMapSurface(FaceType: D3DSURFACE_DESC;
                               Level: UINT;
                               out ppCubeMapSurface: IDirect3DSurface9): HResult; stdcall;

    function LockRect(FaceType: D3DCUBEMAP_FACES;
                      Level: UINT;
                      out pLockedRect: D3DLOCKED_RECT;
                      pRect: PRect;
                      Flags: DWORD): HResult; stdcall;

    function UnlockRect(FaceType: D3DCUBEMAP_FACES;
                        Level: UINT): HResult; stdcall;

    function AddDirtyRect(FaceType: D3DCUBEMAP_FACES;
                          pDirtyRect: PRect): HResult; stdcall;
  end;
  IID_IDirect3DCubeTexture9 = IDirect3DCubeTexture9;
  {$EXTERNALSYM IID_IDirect3DCubeTexture9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDirect3DCubeTexture9Helper = class
    Name: LPCWSTR;
    Width: UINT;
    Height: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: D3DFORMAT;
    Pool: D3DPOOL;
    Priority: DWORD;
    LOD: DWORD;
    FilterType: D3DTEXTUREFILTERTYPE;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDirect3DCubeTexture9Helper}



  // Interface IDirect3DVertexBuffer9
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DVertexBuffer9);'}
  {$EXTERNALSYM IDirect3DVertexBuffer9}
  IDirect3DVertexBuffer9 = interface(IDirect3DResource9)
  ['{b64bb1b5-fd70-4df6-bf91-19d0a12455e3}']
    //*** IDirect3DVertexBuffer9 methods ***//
    function Lock(OffsetToLock: UINT;
                  SizeToLock: UINT;
                  out ppbData: Pointer;
                  Flags: DWORD): HResult; stdcall;

    function Unlock(): HResult; stdcall;

    function GetDesc(out pDesc: D3DVERTEXBUFFER_DESC): HResult; stdcall;
  end;
  IID_IDirect3DVertexBuffer9 = IDirect3DVertexBuffer9;
  {$EXTERNALSYM IID_IDirect3DVertexBuffer9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DVERTEXBUFFER9_DEBUG_INFO = class
    Name: LPCWSTR;
    Length: UINT;
    Usage: DWORD;
    FVF: DWORD;
    Pool: D3DPOOL;
    Priority: DWORD;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DVERTEXBUFFER9_DEBUG_INFO}



  // Interface IDirect3DIndexBuffer9
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DIndexBuffer9);'}
  {$EXTERNALSYM IDirect3DIndexBuffer9}
  IDirect3DIndexBuffer9 = interface(IDirect3DResource9)
  ['{7C9DD65E-D3F7-4529-ACEE-785830ACDE35}']
    //*** IDirect3DIndexBuffer9 methods ***//
    function Lock(OffsetToLock: DWORD;
                  SizeToLock: DWORD;
                  out ppbData: Pointer;
                  Flags: DWord): HResult; stdcall;

    function Unlock(): HResult; stdcall;

    function GetDesc(out pDesc: D3DINDEXBUFFER_DESC): HResult; stdcall;
  end;
  IID_IDirect3DIndexBuffer9 = IDirect3DIndexBuffer9;
  {$EXTERNALSYM IID_IDirect3DIndexBuffer9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDirect3DIndexBuffer9_DEBUG_INFO = class
    Name: LPCWSTR;
    Length: UINT;
    Usage: DWORD;
    FVF: DWORD;
    Pool: D3DPOOL;
    Priority: DWORD;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDirect3DIndexBuffer9_DEBUG_INFO}



  // Interface IDirect3DSurface9
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DSurface9);'}
  {$EXTERNALSYM IDirect3DSurface9}
  IDirect3DSurface9 = interface(IDirect3DResource9)
  ['{0cfbaf3a-9ff6-429a-99b3-a2796af8b89b}']
    //*** IDirect3DSurface9 methods ***//
    function GetContainer(const riid: TGuid;
                          out ppContainer{: Pointer}): HResult; stdcall;

    function GetDesc(out pDesc: D3DSURFACE_DESC): HResult; stdcall;

    function LockRect(out pLockedRect: D3DLOCKED_RECT;
                      pRect: PRect;
                      Flags: DWORD): HResult; stdcall;

    function UnlockRect: HResult; stdcall;

    function GetDC(out phdc: HDC): HResult; stdcall;

    function ReleaseDC(hdc: HDC): HResult; stdcall;
  end;
  IID_IDirect3DSurface9 = IDirect3DSurface9;
  {$EXTERNALSYM IID_IDirect3DSurface9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDirect3DSurface9_DEBUG_INFO = class
    Name: LPCWSTR;
    Width: UINT;
    Height: UINT;
    Usage: DWORD;
    Format: D3DFORMAT;
    Pool: D3DPOOL;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    MultiSampleQuality: DWORD;
    Priority: DWORD;
    LockCount: UINT;
    DCCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDirect3DSurface9_DEBUG_INFO}



  // Interface IDirect3DVolume9
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DVolume9);'}
  {$EXTERNALSYM IDirect3DVolume9}
  IDirect3DVolume9 = interface (IUnknown)
  ['{24F416E6-1F67-4aa7-B88E-D33F6F3128A1}']
    //*** IDirect3DVolume9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function SetPrivateData(const refguid: TGuid;
                            const pData; SizeOfData,
                            Flags: DWord): HResult; stdcall;

    function GetPrivateData(const refguid: TGuid;
                            pData: Pointer;
                            out pSizeOfData: DWORD): HResult; stdcall;

    function FreePrivateData(const refguid: TGUID): HResult; stdcall;

    function GetContainer(const riid: TGUID;
                          var ppContainer: Pointer): HResult; stdcall;

    function GetDesc(out pDesc: D3DVolume_Desc): HResult; stdcall;

    function LockBox(out pLockedVolume: D3DLOCKED_BOX;
                     pBox: PD3DBOX;
                     Flags: DWORD): HResult; stdcall;

    function UnlockBox(): HResult; stdcall;
  end;
  IID_IDirect3DVolume9 = IDirect3DVolume9;
  {$EXTERNALSYM IID_IDirect3DVolume9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DVOLUME9_DEBUG_INFO = class
    Name: LPCWSTR;
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    Usage: DWORD;
    Format: D3DFORMAT;
    Pool: D3DPOOL;
    LockCount: UINT;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DVOLUME9_DEBUG_INFO}



  // Interface IDirect3DQuery9
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DQuery9);'}
  {$EXTERNALSYM IDirect3DQuery9}
  IDirect3DQuery9 = interface(IUnknown)
  ['{d9771460-a695-4f26-bbd3-27b840b541cc}']
    //*** IDirect3DQuery9 methods ***//
    function GetDevice(out ppDevice: IDirect3DDevice9): HResult; stdcall;

    function GetType(): D3DQUERYTYPE; stdcall;

    function GetDataSize(): DWORD; stdcall;

    function Issue(dwIssueFlags: DWORD): HResult; stdcall;

    function GetData(pData: Pointer;
                     dwSize: DWORD;
                     dwGetDataFlags: DWORD): HResult; stdcall;
  end;
  IID_IDirect3DQuery9 = IDirect3DQuery9;
  {$EXTERNALSYM IID_IDirect3DQuery9}


  // Debug class
  // To prevent moving this code outside the interfaces section, the parameters are moved into this class
  // D3D_DEBUG_INFO
  IDIRECT3DQUERY9_DEBUG_INFO = class
    _Type: D3DQUERYTYPE;
    DataSize: DWORD;
    CreationCallStack: LPCWSTR;
  end;
  {$EXTERNALSYM IDIRECT3DQUERY9_DEBUG_INFO}

// Interfaces end -------------------------------------------------------------


  //*
  //* DLL Function for creating a Direct3D9 object. This object supports
  //* enumeration and allows the creation of Direct3DDevice9 objects.
  //* Pass the value of the constant D3D_SDK_VERSION to this function, so
  //* that the run-time can validate that your application was compiled
  //* against the right headers.
  //*/

  function _Direct3DCreate9(SDKVersion: LongWord): Pointer; stdcall;
  {$EXTERNALSYM _Direct3DCreate9}

  // Calling this function directly will not work, so we create a workaround
  function Direct3DCreate9(SDKVersion: LongWord): IDirect3D9; stdcall;
  {$EXTERNALSYM Direct3DCreate9}


  //*
  //* Stubs for graphics profiling.
  //*//


  function D3DPERF_BeginEvent(col: D3DCOLOR;
                              wszName: LPCWSTR): Integer; stdcall;
  {$EXTERNALSYM D3DPERF_BeginEvent}

  function D3DPERF_EndEvent(): Integer; stdcall;
  {$EXTERNALSYM D3DPERF_EndEvent}

  procedure D3DPERF_SetMarker(col: D3DCOLOR;
                              wszName: LPCWSTR); stdcall;
  {$EXTERNALSYM D3DPERF_SetMarker}

  procedure D3DPERF_SetRegion(col: D3DCOLOR;
                              wszName: LPCWSTR); stdcall;
  {$EXTERNALSYM D3DPERF_SetRegion}

  function D3DPERF_QueryRepeatFrame(): BOOL; stdcall;
  {$EXTERNALSYM D3DPERF_QueryRepeatFrame}

  procedure D3DPERF_SetOptions(dwOptions: DWORD); stdcall;
  {$EXTERNALSYM D3DPERF_SetOptions}

  function D3DPERF_GetStatus(): DWORD; stdcall;
  {$EXTERNALSYM D3DPERF_GetStatus}





 //****************************************************************************
 //* Flags for SetPrivateData method on all D3D9 interfaces
 //*
 //* The passed pointer is an IUnknown ptr. The SizeOfData argument to SetPrivateData
 //* must be set to sizeof(IUnknown*). Direct3D will call AddRef through this
 //* pointer and Release when the private data is destroyed. The data will be
 //* destroyed when another SetPrivateData with the same GUID is set, when
 //* FreePrivateData is called, or when the D3D9 object is freed.
 //****************************************************************************/

const

  D3DSPD_IUNKNOWN                     = $00000001;
  {$EXTERNALSYM D3DSPD_IUNKNOWN}

  //***************************************************************************
  //*
  //* Flags for IDirect3D9::CreateDevice's BehaviorFlags
  //*
  //***************************************************************************/

  D3DCREATE_FPU_PRESERVE              = $00000002;
  {$EXTERNALSYM D3DCREATE_FPU_PRESERVE}
  D3DCREATE_MULTITHREADED             = $00000004;
  {$EXTERNALSYM D3DCREATE_MULTITHREADED}

  D3DCREATE_PUREDEVICE                = $00000010;
  {$EXTERNALSYM D3DCREATE_PUREDEVICE}
  D3DCREATE_SOFTWARE_VERTEXPROCESSING = $00000020;
  {$EXTERNALSYM D3DCREATE_SOFTWARE_VERTEXPROCESSING}
  D3DCREATE_HARDWARE_VERTEXPROCESSING = $00000040;
  {$EXTERNALSYM D3DCREATE_HARDWARE_VERTEXPROCESSING}
  D3DCREATE_MIXED_VERTEXPROCESSING    = $00000080;
  {$EXTERNALSYM D3DCREATE_MIXED_VERTEXPROCESSING}

  D3DCREATE_DISABLE_DRIVER_MANAGEMENT = $00000100;
  {$EXTERNALSYM D3DCREATE_DISABLE_DRIVER_MANAGEMENT}
  D3DCREATE_ADAPTERGROUP_DEVICE       = $00000200;
  {$EXTERNALSYM D3DCREATE_ADAPTERGROUP_DEVICE}
  D3DCREATE_DISABLE_DRIVER_MANAGEMENT_EX = $00000400;
  {$EXTERNALSYM D3DCREATE_DISABLE_DRIVER_MANAGEMENT_EX}

  // This flag causes the D3D runtime not to alter the focus
  // window in any way. Use with caution- the burden of supporting
  // focus management events (alt-tab, etc.) falls on the
  // application, and appropriate responses (switching display
  // mode, etc.) should be coded.
  D3DCREATE_NOWINDOWCHANGES           = $00000800;
  {$EXTERNALSYM D3DCREATE_NOWINDOWCHANGES}

  //* D3D9Ex only -- *//
  //{$IFDEF D3D_DISABLE_9EX}

  // Disable multithreading for software vertex processing
  D3DCREATE_DISABLE_PSGP_THREADING    = $00002000;
  {$EXTERNALSYM D3DCREATE_DISABLE_PSGP_THREADING}
  // This flag enables present statistics on device.
  D3DCREATE_ENABLE_PRESENTSTATS       = $00004000;
  {$EXTERNALSYM D3DCREATE_ENABLE_PRESENTSTATS}
  // This flag disables printscreen support in the runtime for this device
  D3DCREATE_DISABLE_PRINTSCREEN       = $00008000;
  {$EXTERNALSYM D3DCREATE_DISABLE_PRINTSCREEN}

  D3DCREATE_SCREENSAVER               = $10000000;
  {$EXTERNALSYM D3DCREATE_SCREENSAVER}


  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only *//


  //***************************************************************************
  //*
  //* Parameter for IDirect3D9::CreateDevice's Adapter argument
  //*
  //***************************************************************************/

  D3DADAPTER_DEFAULT                  = 0;
  {$EXTERNALSYM D3DADAPTER_DEFAULT}

  //***************************************************************************
  //*
  //* Flags for IDirect3D9::EnumAdapters
  //*
  //***************************************************************************/

  //*
  //* The D3DENUM_WHQL_LEVEL value has been retired for 9Ex and future versions,
  //* but it needs to be defined here for compatibility with DX9 and earlier versions.
  //* See the DirectX SDK for sample code on discovering driver signatures.
  //*/

  D3DENUM_WHQL_LEVEL                  = $00000002;
  {$EXTERNALSYM D3DENUM_WHQL_LEVEL}

  //* D3D9Ex only -- *//
  //{$IFDEF D3D_DISABLE_9EX}

  //* NO_DRIVERVERSION will not fill out the DriverVersion field, nor will the
  //  DriverVersion be incorporated into the DeviceIdentifier GUID. WINNT only */

  D3DENUM_NO_DRIVERVERSION            = $00000004;
  {$EXTERNALSYM D3DENUM_NO_DRIVERVERSION}

  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only *//


  //***************************************************************************
  //*
  //* Maximum number of back-buffers supported in DX9
  //*
  //***************************************************************************/

  D3DPRESENT_BACK_BUFFERS_MAX         = 3;
  {$EXTERNALSYM D3DPRESENT_BACK_BUFFERS_MAX}

  //* D3D9Ex only -- *//
  //{$IFDEF D3D_DISABLE_9EX}

  //***************************************************************************
  //*
  //* Maximum number of back-buffers supported when apps use CreateDeviceEx
  //*
  //***************************************************************************/

  D3DPRESENT_BACK_BUFFERS_MAX_EX      = 30;
  {$EXTERNALSYM D3DPRESENT_BACK_BUFFERS_MAX_EX}

  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only *//

  //***************************************************************************
  //*
  //* Flags for IDirect3DDevice9::SetGammaRamp
  //*
  //***************************************************************************/

  D3DSGR_NO_CALIBRATION               = $00000000;
  {$EXTERNALSYM D3DSGR_NO_CALIBRATION}
  D3DSGR_CALIBRATE                    = $00000001;
  {$EXTERNALSYM D3DSGR_CALIBRATE}

  //***************************************************************************
  //*
  //* Flags for IDirect3DDevice9::SetCursorPosition
  //*
  //***************************************************************************/

  D3DCURSOR_IMMEDIATE_UPDATE          = $00000001;
  {$EXTERNALSYM D3DCURSOR_IMMEDIATE_UPDATE}

  //***************************************************************************
  //*
  //* Flags for IDirect3DSwapChain9::Present
  //*
  //***************************************************************************/

  D3DPRESENT_DONOTWAIT                = $00000001;
  {$EXTERNALSYM D3DPRESENT_DONOTWAIT}
  D3DPRESENT_LINEAR_CONTENT           = $00000002;
  {$EXTERNALSYM D3DPRESENT_LINEAR_CONTENT}

  //* D3D9Ex only -- *//
  //{$IFDEF D3D_DISABLE_9EX}

  D3DPRESENT_DONOTFLIP                = $00000004;
  {$EXTERNALSYM D3DPRESENT_DONOTFLIP}
  D3DPRESENT_FLIPRESTART              = $00000008;
  {$EXTERNALSYM D3DPRESENT_FLIPRESTART}
  D3DPRESENT_VIDEO_RESTRICT_TO_MONITOR = $00000010;
  {$EXTERNALSYM D3DPRESENT_VIDEO_RESTRICT_TO_MONITOR}
  D3DPRESENT_UPDATEOVERLAYONLY        = $00000020;
  {$EXTERNALSYM D3DPRESENT_UPDATEOVERLAYONLY}
  D3DPRESENT_HIDEOVERLAY              = $00000040;
  {$EXTERNALSYM D3DPRESENT_HIDEOVERLAY}
  D3DPRESENT_UPDATECOLORKEY           = $00000080;
  {$EXTERNALSYM D3DPRESENT_UPDATECOLORKEY}
  D3DPRESENT_FORCEIMMEDIATE           = $00000100;
  {$EXTERNALSYM D3DPRESENT_FORCEIMMEDIATE}

  //{$ENDIF} // !D3D_DISABLE_9EX
  //* -- D3D9Ex only *//


  //***************************************************************************
  //*
  //* Flags for DrawPrimitive/DrawIndexedPrimitive
  //*   Also valid for Begin/BeginIndexed
  //*   Also valid for VertexBuffer::CreateVertexBuffer
  //***************************************************************************/

  //*
  //*  DirectDraw error codes
  //*/
  _FACD3D                             = $876;
  {$EXTERNALSYM _FACD3D}

  // Have to work around, because a macro can't be assigned to a const
  // The macro MAKE_HRESULT is defined in WinError.pas
  // HRESULT MAKE_HRESULT(sev: WORD;  fac: WORD;  code: WORD): HResult;
  // So, what we do here is
  // MAKE_D3DHRESULT( code )  MAKE_HRESULT( 1, _FACD3D, code )
  //                 1             _FACD3D         code = 0
  MAKE_D3DHRESULT_R = (1 shl 31) OR (_FACD3D shl 16);
  {$EXTERNALSYM MAKE_D3DHRESULT_R}
  //
  //MAKE_D3DSTATUS( code )  MAKE_HRESULT( 0, _FACD3D, code )
  //                0             _FACD3D          code = 0
  MAKE_D3DSTATUS_R = (0 shl 31) OR (_FACD3D shl 16);
  {$EXTERNALSYM MAKE_D3DSTATUS_R}

  //*
  //* Direct3D Errors
  //*/
  D3D_OK                              = S_OK;
  {$EXTERNALSYM D3D_OK}

  D3DERR_WRONGTEXTUREFORMAT           = MAKE_D3DHRESULT_R OR (2072);
  {$EXTERNALSYM D3DERR_WRONGTEXTUREFORMAT}
  D3DERR_UNSUPPORTEDCOLOROPERATION    = MAKE_D3DHRESULT_R OR (2073);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDCOLOROPERATION}
  D3DERR_UNSUPPORTEDCOLORARG          = MAKE_D3DHRESULT_R OR (2074);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDCOLORARG}
  D3DERR_UNSUPPORTEDALPHAOPERATION    = MAKE_D3DHRESULT_R OR (2075);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDALPHAOPERATION}
  D3DERR_UNSUPPORTEDALPHAARG          = MAKE_D3DHRESULT_R OR (2076);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDALPHAARG}
  D3DERR_TOOMANYOPERATIONS            = MAKE_D3DHRESULT_R OR (2077);
  {$EXTERNALSYM D3DERR_TOOMANYOPERATIONS}
  D3DERR_CONFLICTINGTEXTUREFILTER     = MAKE_D3DHRESULT_R OR (2078);
  {$EXTERNALSYM D3DERR_CONFLICTINGTEXTUREFILTER}
  D3DERR_UNSUPPORTEDFACTORVALUE       = MAKE_D3DHRESULT_R OR (2079);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDFACTORVALUE}
  D3DERR_CONFLICTINGRENDERSTATE       = MAKE_D3DHRESULT_R OR (2081);
  {$EXTERNALSYM D3DERR_CONFLICTINGRENDERSTATE}
  D3DERR_UNSUPPORTEDTEXTUREFILTER     = MAKE_D3DHRESULT_R OR (2082);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDTEXTUREFILTER}
  D3DERR_CONFLICTINGTEXTUREPALETTE    = MAKE_D3DHRESULT_R OR (2086);
  {$EXTERNALSYM D3DERR_CONFLICTINGTEXTUREPALETTE}
  D3DERR_DRIVERINTERNALERROR          = MAKE_D3DHRESULT_R OR (2087);
  {$EXTERNALSYM D3DERR_DRIVERINTERNALERROR}

  D3DERR_NOTFOUND                     = MAKE_D3DHRESULT_R OR (2150);
  {$EXTERNALSYM D3DERR_NOTFOUND}
  D3DERR_MOREDATA                     = MAKE_D3DHRESULT_R OR (2151);
  {$EXTERNALSYM D3DERR_MOREDATA}
  D3DERR_DEVICELOST                   = MAKE_D3DHRESULT_R OR (2152);
  {$EXTERNALSYM D3DERR_DEVICELOST}
  D3DERR_DEVICENOTRESET               = MAKE_D3DHRESULT_R OR (2153);
  {$EXTERNALSYM D3DERR_DEVICENOTRESET}
  D3DERR_NOTAVAILABLE                 = MAKE_D3DHRESULT_R OR (2154);
  {$EXTERNALSYM D3DERR_NOTAVAILABLE}
  D3DERR_OUTOFVIDEOMEMORY             = MAKE_D3DHRESULT_R OR (380);
  {$EXTERNALSYM D3DERR_OUTOFVIDEOMEMORY}
  D3DERR_INVALIDDEVICE                = MAKE_D3DHRESULT_R OR (2155);
  {$EXTERNALSYM D3DERR_INVALIDDEVICE}
  D3DERR_INVALIDCALL                  = MAKE_D3DHRESULT_R OR (2156);
  {$EXTERNALSYM D3DERR_INVALIDCALL}
  D3DERR_DRIVERINVALIDCALL            = MAKE_D3DHRESULT_R OR (2157);
  {$EXTERNALSYM D3DERR_DRIVERINVALIDCALL}
  D3DERR_WASSTILLDRAWING              = MAKE_D3DHRESULT_R OR (540);
  {$EXTERNALSYM D3DERR_WASSTILLDRAWING}
  D3DOK_NOAUTOGEN                     = MAKE_D3DSTATUS_R OR (2159);
  {$EXTERNALSYM D3DOK_NOAUTOGEN}

  //* D3D9Ex only -- *//
  //{$IFDEF D3D_DISABLE_9EX}


  D3DERR_DEVICEREMOVED                = MAKE_D3DHRESULT_R OR (2160);
  {$EXTERNALSYM D3DERR_DEVICEREMOVED}
  S_NOT_RESIDENT                      = MAKE_D3DSTATUS_R OR (2165);
  {$EXTERNALSYM S_NOT_RESIDENT}
  S_RESIDENT_IN_SHARED_MEMORY         = MAKE_D3DSTATUS_R OR (2166);
  {$EXTERNALSYM S_RESIDENT_IN_SHARED_MEMORY}
  S_PRESENT_MODE_CHANGED              = MAKE_D3DSTATUS_R OR (2167);
  {$EXTERNALSYM S_PRESENT_MODE_CHANGED}
  S_PRESENT_OCCLUDED                  = MAKE_D3DSTATUS_R OR (2168);
  {$EXTERNALSYM S_PRESENT_OCCLUDED}
  D3DERR_DEVICEHUNG                   = MAKE_D3DHRESULT_R OR (2164);
  {$EXTERNALSYM D3DERR_DEVICEHUNG}
  D3DERR_UNSUPPORTEDOVERLAY           = MAKE_D3DHRESULT_R OR (2171);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDOVERLAY}
  D3DERR_UNSUPPORTEDOVERLAYFORMAT     = MAKE_D3DHRESULT_R OR (2172);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDOVERLAYFORMAT}
  D3DERR_CANNOTPROTECTCONTENT         = MAKE_D3DHRESULT_R OR (2173);
  {$EXTERNALSYM D3DERR_CANNOTPROTECTCONTENT}
  D3DERR_UNSUPPORTEDCRYPTO            = MAKE_D3DHRESULT_R OR (2174);
  {$EXTERNALSYM D3DERR_UNSUPPORTEDCRYPTO}
  D3DERR_PRESENT_STATISTICS_DISJOINT  = MAKE_D3DHRESULT_R OR (2180);
  {$EXTERNALSYM D3DERR_PRESENT_STATISTICS_DISJOINT}


  //*********************
  //* D3D9Ex interfaces
  //*********************/


type

  // Forward Interface Identifiers
  // =============================

  PIDirect3D9Ex = ^IDirect3D9Ex;
  IDirect3D9Ex = interface;

  PIDirect3DDevice9Ex = ^IDirect3DDevice9Ex;
  IDirect3DDevice9Ex = interface;

  PIDirect3DSwapChain9Ex = ^IDirect3DSwapChain9Ex;
  IDirect3DSwapChain9Ex = interface;

  PIDirect3D9ExOverlayExtension = ^IDirect3D9ExOverlayExtension;
  IDirect3D9ExOverlayExtension = interface;

  PIDirect3DDevice9Video = ^IDirect3DDevice9Video;
  IDirect3DDevice9Video = interface;

  PIDirect3DAuthenticatedChannel9 = ^IDirect3DAuthenticatedChannel9;
  IDirect3DAuthenticatedChannel9 = interface;

  PIDirect3DCryptoSession9 = ^IDirect3DCryptoSession9;
  IDirect3DCryptoSession9 = interface;


  //Interfaces
  //==========



  // Interface IDirect3D9Ex
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3D9Ex);'}
  {$EXTERNALSYM IDirect3D9Ex}
  IDirect3D9Ex = interface(IDirect3D9)
  ['{02177241-69FC-400C-8FF1-93A44DF6861D}']
    //*** IDirect3D9Ex methods ***//
    function CreateDeviceEx(Adapter: UINT;
                            DeviceType: D3DDEVTYPE;
                            hFocusWindow: HWND;
                            BehaviorFlags: DWORD;
                            var pPresentationParameters: D3DPRESENT_PARAMETERS;
                            var pFullscreenDisplayMode: D3DDISPLAYMODEEX;
                            out ppReturnedDeviceInterface: PIDirect3DDevice9Ex): HResult; stdcall;

    function EnumAdapterModesEx(Adapter: UINT;
                                const pFilter: D3DDISPLAYMODEFILTER;
                                Mode: UINT;
                                out pMode: D3DDISPLAYMODEEX): HResult; stdcall;

    function GetAdapterDisplayModeEx(Adapter: UINT;
                                     var pMode: D3DDISPLAYMODEEX;
                                     var pRotation: D3DDISPLAYROTATION): HResult; stdcall;

    function GetAdapterLUID(Adapter: UINT;
                            pLUID: LUID): HResult; stdcall;

    function GetAdapterModeCountEx(Adapter: UINT;
                                   const pFilter: D3DDISPLAYMODEFILTER): UINT; stdcall;

  end;
  IID_IDirect3D9Ex = IDirect3D9Ex;
  {$EXTERNALSYM IID_IDirect3D9Ex}



  // Interface IDirect3DDevice9Ex
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DDevice9Ex);'}
  {$EXTERNALSYM IDirect3DDevice9Ex}
  IDirect3DDevice9Ex = interface(IDirect3DDevice9)
  ['{02177241-69FC-400C-8FF1-93A44DF6861D}']
    //*** IDirect3DDevice9Ex methods ***//
    function CheckDeviceState(hWindow: HWND): HResult; stdcall;

    function CheckResourceResidency(pResourceArray: IDirect3DResource9;
                                    NumResources: UINT32): HResult; stdcall;

    function ComposeRects(pSource: IDirect3DSurface9;
                          pDestination: IDirect3DSurface9;
                          pSrcRectDescriptors: IDirect3DVertexBuffer9;
                          NumRects: UINT;
                          pDstRectDescriptors: IDirect3DVertexBuffer9;
                          Operation: D3DCOMPOSERECTSOP;
                          XOffset: Integer;
                          YOffset: Integer): HResult; stdcall;

    function CreateDepthStencilSurfaceEx(Width: UINT;
                                         Height: UINT;
                                         Format: D3DFORMAT;
                                         MultiSample: D3DMULTISAMPLE_TYPE;
                                         MultisampleQuality: DWORD;
                                         Discard: BOOL;
                                         out ppSurface: IDirect3DSurface9;
                                         pSharedHandle: THandle;
                                         Usage: DWORD): HResult; stdcall;

    function CreateOffscreenPlainSurfaceEx(Width: UINT;
                                           Height: UINT;
                                           Format: D3DFORMAT;
                                           Pool: D3DPOOL;
                                           var ppSurface: IDirect3DSurface9;
                                           var pSharedHandle: THandle;
                                           Usage: DWORD): HResult; stdcall;

    function CreateRenderTargetEx(Width: UINT;
                                  Height: UINT;
                                  Format: D3DFORMAT;
                                  MultiSample: D3DMULTISAMPLE_TYPE;
                                  MultisampleQuality: DWORD;
                                  Lockable: BOOL;
                                  var ppSurface: IDirect3DSurface9;
                                  var pSharedHandle: THandle;
                                  Usage: DWORD): HResult; stdcall;

    function GetDisplayModeEx(iSwapChain: UINT;
                              out pMode: D3DDISPLAYMODEEX;
                              out pRotation: D3DDISPLAYROTATION): HResult; stdcall;

    function GetGPUThreadPriority(pPriority: Integer): HResult; stdcall;

    function GetMaximumFrameLatency(out pMaxLatency: UINT): HResult; stdcall;

    function PresentEx(pSourceRect: TRect;
                       pDestRect: TRect;
                       hDestWindowOverride: HWND;
                       pDirtyRegion: RGNDATA;
                       dwFlags: DWORD): HResult; stdcall;

    function ResetEx(var pPresentationParameters: D3DPRESENT_PARAMETERS;
                     var pFullscreenDisplayMode: D3DDISPLAYMODEEX): HResult; stdcall;

    function SetConvolutionMonoKernel(Width: UINT;
                                      Height: UINT;
                                      RowWeights: Single;
                                      ColumnWeights: Single): HResult; stdcall;

    function SetGPUThreadPriority(pPriority: Integer): HResult; stdcall;

    function SetMaximumFrameLatency(pMaxLatency: UINT): HResult; stdcall;

    function TestCooperativeLevel(): HResult; stdcall;

    function WaitForVBlank(SwapChainIndex: UINT): HResult; stdcall;

  end;
  IID_IDirect3DDevice9Ex = IDirect3DDevice9Ex;
  {$EXTERNALSYM IID_IDirect3DDevice9Ex}


  // Interface IDirect3DSwapChain9Ex
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DSwapChain9Ex);'}
  {$EXTERNALSYM IDirect3DSwapChain9Ex}
  IDirect3DSwapChain9Ex = interface(IDirect3DSwapChain9)
  ['{91886CAF-1C3D-4d2e-A0AB-3E4C7D8D3303}']
    //*** IDirect3DSwapChain9Ex methods ***//
    function GetDisplayModeEx(out pMode: D3DDISPLAYMODEEX;
                              out pRotation: D3DDISPLAYROTATION): HResult; stdcall;

    function GetLastPresentCount(out pLastPresentCount: UINT): HResult; stdcall;

    function GetPresentStatistics(out pPresentationStatistics: D3DPRESENTSTATS): HResult; stdcall;

  end;
  IID_IDirect3DSwapChain9Ex = IDirect3DSwapChain9Ex;
  {$EXTERNALSYM IID_IDirect3DSwapChain9Ex}

  //{$ENDIF} // !D3D_DISABLE_9EX


  // Interface IDirect3D9ExOverlayExtension
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3D9ExOverlayExtension);'}
  {$EXTERNALSYM IDirect3D9ExOverlayExtension}
  IDirect3D9ExOverlayExtension = interface(IUnknown)
  ['{187aeb13-aaf5-4c59-876d-e059088c0df8}']
    //*** IDirect3D9ExOverlayExtension methods ***//
    function CheckDeviceOverlayType(Adapter: UINT;
                                    DevType: D3DDEVTYPE;
                                    OverlayWidth: UINT;
                                    OverlayHeight: UINT;
                                    OverlayFormat: D3DFORMAT;
                                    pDisplayMode: D3DDISPLAYMODEEX;
                                    DisplayRotation: D3DDISPLAYROTATION;
                                    out pOverlayCaps: D3DOVERLAYCAPS): HResult; stdcall;
  end;
  IID_IDirect3D9ExOverlayExtension = IDirect3D9ExOverlayExtension;
  {$EXTERNALSYM IID_IDirect3D9ExOverlayExtension}



  // Interface IDirect3DDevice9Video
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DDevice9Video);'}
  {$EXTERNALSYM IDirect3DDevice9Video}
  IDirect3DDevice9Video = interface(IUnknown)
  ['{26dc4561-a1ee-4ae7-96da-118a36c0ec95}']
    //*** IDirect3DDevice9Video methods ***//
    function CreateAuthenticatedChannel(ChannelType: D3DAUTHENTICATEDCHANNELTYPE;
                                        out ppAuthenticatedChannel: IDirect3DAuthenticatedChannel9;
                                        out pChannelHandle: THandle): HResult; stdcall;

    function CreateCryptoSession(pCryptoType: TGuid;
                                 pDecodeProfile: TGuid;
                                 out ppCryptoSession: IDirect3DCryptoSession9;
                                 out pCryptoHandle: THandle): HResult; stdcall;

    function GetContentProtectionCaps(pCryptoType: TGuid;
                                      pDecodeProfile: TGuid;
                                      out pCaps: D3DCONTENTPROTECTIONCAPS): HResult; stdcall;

  end;
  IID_IDirect3DDevice9Video = IDirect3DDevice9Video;
  {$EXTERNALSYM IID_IDirect3DDevice9Video}



  // Interface IDirect3DAuthenticatedChannel9
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DAuthenticatedChannel9);'}
  {$EXTERNALSYM IDirect3DAuthenticatedChannel9}
  IDirect3DAuthenticatedChannel9 = interface(IUnknown)
  ['{ff24beee-da21-4beb-98b5-d2f899f98af9}']
    //*** IDirect3DAuthenticatedChannel9 methods ***//

    function GetCertificateSize(out pCertificateSize: UINT): HResult; stdcall;

    function GetCertificate(CertifacteSize: UINT;
                            out ppCertificate: PByte): HResult; stdcall;

    function NegotiateKeyExchange(DataSize: UINT;
                                  var pData): HResult; stdcall;

    function Query(InputSize: UINT;
                   var pInput;
                   OutputSize: UINT;
                   var pOutput): HResult; stdcall;
  end;
  IID_IDirect3DAuthenticatedChannel9 = IDirect3DAuthenticatedChannel9;
  {$EXTERNALSYM IID_IDirect3DAuthenticatedChannel9}



  // Interface IDirect3DCryptoSession9
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirect3DCryptoSession9);'}
  {$EXTERNALSYM IDirect3DCryptoSession9}
  IDirect3DCryptoSession9 = interface(IUnknown)
  ['{fa0ab799-7a9c-48ca-8c5b-237e71a54434}']
    //*** IDirect3DCryptoSession9 methods ***//
    function EncryptionBlt(pSrcSurface: IDirect3DSurface9;
                           out pDstSurface: IDirect3DSurface9;
                           DstSurfaceSize: UINT;
                           var pIV): HResult; stdcall;

    function FinishSessionKeyRefresh(): HResult; stdcall;

    function GetCertificate(CertifacteSize: UINT;
                            out ppCertificate: PByte): HResult; stdcall;

    function GetCertificateSize(out pCertificateSize: UINT): HResult; stdcall;

    function GetEncryptionBltKey(out pReadbackKey; KeySize: UINT): HResult; stdcall;

    function GetSurfacePitch(pSrcSurface: IDirect3DSurface9;
                             out pSurfacePitch: UINT): HResult; stdcall;

    function NegotiateKeyExchange(DataSize: UINT;
                                  out pData): HResult; cdecl;

    function StartSessionKeyRefresh(out pRandomNumber; RandomNumberSize: UINT): HResult; stdcall;
  end;
  IID_IDirect3DCryptoSession9 = IDirect3DCryptoSession9;
  {$EXTERNALSYM IID_IDirect3DCryptoSession9}



  function Direct3DCreate9Ex(SDKVersion: UINT;
                             out ppD3D: PIDirect3D9Ex): HResult; stdcall;
  {$EXTERNALSYM Direct3DCreate9Ex}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  D3D9Lib                = 'D3d9.dll';


  // Macro's
  function MAKE_D3DHRESULT(Code: DWORD): DWORD;
  begin
    Result:= DWORD((1 shl 31) or (_FACD3D shl 16)) or Code;
  end;

  function MAKE_D3DSTATUS(Code: DWORD): DWORD;
  begin
    Result:= DWORD((0 shl 31) or (_FACD3D shl 16)) or Code;
  end;


{$WARN SYMBOL_PLATFORM OFF}

  function _Direct3DCreate9;         external D3D9Lib name 'Direct3DCreate9' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function Direct3DCreate9Ex;        external D3D9Lib name 'Direct3DCreate9Ex' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function D3DPERF_BeginEvent;       external D3D9Lib name 'D3DPERF_BeginEvent' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function D3DPERF_EndEvent;         external D3D9Lib name 'D3DPERF_EndEvent' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure D3DPERF_SetMarker;       external D3D9Lib name 'D3DPERF_SetMarker' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure D3DPERF_SetRegion;       external D3D9Lib name 'D3DPERF_SetRegion' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function D3DPERF_QueryRepeatFrame; external D3D9Lib name 'D3DPERF_QueryRepeatFrame' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure D3DPERF_SetOptions;      external D3D9Lib name 'D3DPERF_SetOptions' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function D3DPERF_GetStatus;        external D3D9Lib name 'D3DPERF_GetStatus' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}


// Implement Additional functions here.

// Helper for Direct3DCreate9
function Direct3DCreate9(SDKVersion: LongWord): IDirect3D9; stdcall;
begin
  // Cast returned pointer to IDirect3D9 pointer.
  Result:= IDirect3D9(_Direct3DCreate9(SDKVersion));
  // release from autoincrement reference count
  if Assigned(Result) then
    Result._Release;
end;

end.
