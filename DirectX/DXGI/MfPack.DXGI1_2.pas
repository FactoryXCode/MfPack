// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DXGI1_2.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Microsoft DirectX Graphics Infrastructure API
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Tilo Güldner.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: dxgi1_2.h
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
unit MfPack.DXGI1_2;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "dxgi1_2.h"'}
  {$HPPEMIT ''}

interface

uses
  {WinApi}
  WinApi.Windows,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DXGI,
  MfPack.DXGICommon,
  MfPack.DXGIFormat,
  MfPack.DXGIType;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

const

  DXGI_ENUM_MODES_STEREO          : UINT = 4;
  {$EXTERNALSYM DXGI_ENUM_MODES_STEREO}
  DXGI_ENUM_MODES_DISABLED_STEREO : UINT = 8;
  {$EXTERNALSYM DXGI_ENUM_MODES_DISABLED_STEREO}

  DXGI_SHARED_RESOURCE_READ       : DWORD = $80000000;
  {$EXTERNALSYM DXGI_SHARED_RESOURCE_READ}
  DXGI_SHARED_RESOURCE_WRITE      : DWORD = 1;
  {$EXTERNALSYM DXGI_SHARED_RESOURCE_WRITE}

type

  // Forward declarations
  PIDXGIDisplayControl = ^IDXGIDisplayControl;
  PIDXGIOutputDuplication = ^IDXGIOutputDuplication;
  PIDXGISurface2 = ^IDXGISurface2;
  PIDXGIResource1 = ^IDXGIResource1;
  PIDXGIDevice2 = ^IDXGIDevice2;
  PIDXGISwapChain1 = ^IDXGISwapChain1;
  PIDXGIFactory2 = ^IDXGIFactory2;
  PIDXGIAdapter2 = ^IDXGIAdapter2;
  PIDXGIOutput1 = ^IDXGIOutput1;


  // Interface IDXGIDisplayControl
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDisplayControl);'}
  {$EXTERNALSYM IDXGIDisplayControl}
  IDXGIDisplayControl = interface(IUnknown)
  ['{ea9dbf1a-c88e-4486-854a-98aa0138f30c}']

    function IsStereoEnabled(): BOOL; stdcall;

    procedure SetStereoEnabled(enabled: BOOL);

  end;
  IID_IDXGIDisplayControl = IDXGIDisplayControl;
  {$EXTERNALSYM IID_IDXGIDisplayControl}

  //----------------------------------------------------------------------------
  // IDXGIOutputDuplication structures
  //----------------------------------------------------------------------------
  PDXGI_OUTDUPL_MOVE_RECT = ^DXGI_OUTDUPL_MOVE_RECT;
  DXGI_OUTDUPL_MOVE_RECT = record
    SourcePoint: POINT;
    DestinationRect: TRect;  // RECT
  end;
  {$EXTERNALSYM DXGI_OUTDUPL_MOVE_RECT}

  PDXGI_OUTDUPL_DESC = ^DXGI_OUTDUPL_DESC;
  DXGI_OUTDUPL_DESC = record
    ModeDesc: DXGI_MODE_DESC;
    Rotation: DXGI_MODE_ROTATION;
    DesktopImageInSystemMemory: BOOL;
  end;
  {$EXTERNALSYM DXGI_OUTDUPL_DESC}

  PDXGI_OUTDUPL_POINTER_POSITION = ^DXGI_OUTDUPL_POINTER_POSITION;
  DXGI_OUTDUPL_POINTER_POSITION = record
    Position: POINT;
    Visible: BOOL;
  end;
  {$EXTERNALSYM DXGI_OUTDUPL_POINTER_POSITION}

  PDXGI_OUTDUPL_POINTER_SHAPE_TYPE = ^DXGI_OUTDUPL_POINTER_SHAPE_TYPE;
  DXGI_OUTDUPL_POINTER_SHAPE_TYPE                = (
    DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MONOCHROME   = $00000001,
    DXGI_OUTDUPL_POINTER_SHAPE_TYPE_COLOR        = $00000002,
    DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MASKED_COLOR = $00000004);
  {$EXTERNALSYM DXGI_OUTDUPL_POINTER_SHAPE_TYPE}

  PDXGI_OUTDUPL_POINTER_SHAPE_INFO = ^DXGI_OUTDUPL_POINTER_SHAPE_INFO;
  DXGI_OUTDUPL_POINTER_SHAPE_INFO = record
    _Type: UINT;
    Width: UINT;
    Height: UINT;
    Pitch: UINT;
    HotSpot: POINT;
  end;
  {$EXTERNALSYM DXGI_OUTDUPL_POINTER_SHAPE_INFO}

  PDXGI_OUTDUPL_FRAME_INFO = ^DXGI_OUTDUPL_FRAME_INFO;
  DXGI_OUTDUPL_FRAME_INFO = record
    LastPresentTime: LARGE_INTEGER;
    LastMouseUpdateTime: LARGE_INTEGER;
    AccumulatedFrames: UINT;
    RectsCoalesced: BOOL;
    ProtectedContentMaskedOut: BOOL;
    PointerPosition: DXGI_OUTDUPL_POINTER_POSITION;
    TotalMetadataBufferSize: UINT;
    PointerShapeBufferSize: UINT;
  end;
  {$EXTERNALSYM DXGI_OUTDUPL_FRAME_INFO}


  // Interface IDXGIOutputDuplication
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutputDuplication);'}
  {$EXTERNALSYM IDXGIOutputDuplication}
  IDXGIOutputDuplication = interface(IDXGIObject)
  ['{191cfac3-a341-470d-b26e-a864f428319c}']

    procedure GetDesc(out pDesc: DXGI_OUTDUPL_DESC);

    function AcquireNextFrame(TimeoutInMilliseconds: UINT;
                              out pFrameInfo: DXGI_OUTDUPL_FRAME_INFO;
                              out ppDesktopResource: IDXGIResource): HResult; stdcall;

    function GetFrameDirtyRects(DirtyRectsBufferSize: UINT;
                                out pDirtyRectsBuffer: TRect;
                                out pDirtyRectsBufferSizeRequired: UINT): HResult; stdcall;

    function GetFrameMoveRects(MoveRectsBufferSize: UINT;
                               out pMoveRectBuffer: DXGI_OUTDUPL_MOVE_RECT;
                               out pMoveRectsBufferSizeRequired: UINT): HResult; stdcall;

    function GetFramePointerShape(PointerShapeBufferSize: UINT;
                                  out pPointerShapeBuffer: Pointer;
                                  out pPointerShapeBufferSizeRequired: UINT;
                                  out pPointerShapeInfo: DXGI_OUTDUPL_POINTER_SHAPE_INFO): HResult; stdcall;

    function MapDesktopSurface(out pLockedRect: DXGI_MAPPED_RECT): HResult; stdcall;

    function UnMapDesktopSurface(): HResult; stdcall;

    function ReleaseFrame(): HResult; stdcall;

  end;
  IID_IDXGIOutputDuplication = IDXGIOutputDuplication;
  {$EXTERNALSYM IID_IDXGIOutputDuplication}


  PDXGI_ALPHA_MODE = ^DXGI_ALPHA_MODE;
  DXGI_ALPHA_MODE                 = (
    DXGI_ALPHA_MODE_UNSPECIFIED   = 0,
    DXGI_ALPHA_MODE_PREMULTIPLIED = 1,
    DXGI_ALPHA_MODE_STRAIGHT      = 2,
    DXGI_ALPHA_MODE_IGNORE        = 3,
    DXGI_ALPHA_MODE_FORCE_DWORD   = FORCEDWORD);
  {$EXTERNALSYM DXGI_ALPHA_MODE}


  // Interface IDXGISurface2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISurface2);'}
  {$EXTERNALSYM IDXGISurface2}
  IDXGISurface2 = interface(IDXGISurface1)
  ['{aba496dd-b617-4cb8-a866-bc44d7eb1fa2}']

    function GetResource(const riid: REFIID;
                         out ppParentResource: Pointer;
                         out pSubresourceIndex: UINT): HResult; stdcall;

  end;
  IID_IDXGISurface2 = IDXGISurface2;
  {$EXTERNALSYM IID_IDXGISurface2}


  // Interface IDXGIResource1
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIResource1);'}
  {$EXTERNALSYM IDXGIResource1}
  IDXGIResource1 = interface(IDXGIResource)
  ['{30961379-4609-4a41-998e-54fe567ee0c1}']

    function CreateSubresourceSurface(index: UINT;
                                      out ppSurface: IDXGISurface2): HResult; stdcall;

    function CreateSharedHandle(const pAttributes: PSECURITY_ATTRIBUTES;
                                dwAccess: DWORD;
                                lpName: PWideChar;
                                out _pHandle: THandle): HResult; stdcall;

  end;
  IID_IDXGIResource1 = IDXGIResource1;
  {$EXTERNALSYM IID_IDXGIResource1}


  PDXGI_OFFER_RESOURCE_PRIORITY = ^DXGI_OFFER_RESOURCE_PRIORITY;
  {$EXTERNALSYM _DXGI_OFFER_RESOURCE_PRIORITY}
  _DXGI_OFFER_RESOURCE_PRIORITY      = (
    DXGI_OFFER_RESOURCE_PRIORITY_LOW = 1,
    DXGI_OFFER_RESOURCE_PRIORITY_NORMAL,
    DXGI_OFFER_RESOURCE_PRIORITY_HIGH);
  DXGI_OFFER_RESOURCE_PRIORITY = _DXGI_OFFER_RESOURCE_PRIORITY;


  // Interface IDXGIDevice2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDevice2);'}
  {$EXTERNALSYM IDXGIDevice2}
  IDXGIDevice2 = interface(IDXGIDevice1)
  ['{05008617-fbfd-4051-a790-144884b4f6a9}']

    function OfferResources(NumResources: UINT;
                            ppResources: IDXGIResource;
                            Priority: DXGI_OFFER_RESOURCE_PRIORITY): HResult; stdcall;

    function ReclaimResources(NumResources: UINT;
                              ppResources: IDXGIResource;
                              out pDiscarded: BOOL): HResult; stdcall;

    function EnqueueSetEvent(hEvent: THandle): HResult; stdcall;

  end;
  IID_IDXGIDevice2 = IDXGIDevice2;
  {$EXTERNALSYM IID_IDXGIDevice2}


  PDXGI_MODE_DESC1 = ^DXGI_MODE_DESC1;
  DXGI_MODE_DESC1 = record
    Width: UINT;
    Height: UINT;
    RefreshRate: DXGI_RATIONAL;
    Format: DXGI_FORMAT;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
    Stereo: BOOL;
  end;
  {$EXTERNALSYM DXGI_MODE_DESC1}


  //----------------------------------------------------------------------------
  // IDXGISwapchain1 structures
  //----------------------------------------------------------------------------
  PDXGI_SCALING = ^DXGI_SCALING;
  DXGI_SCALING                        = (
    DXGI_SCALING_STRETCH              = 0,
    DXGI_SCALING_NONE                 = 1,
    DXGI_SCALING_ASPECT_RATIO_STRETCH = 2);
  {$EXTERNALSYM DXGI_SCALING}

  PDXGI_SWAP_CHAIN_DESC1 = ^DXGI_SWAP_CHAIN_DESC1;
  DXGI_SWAP_CHAIN_DESC1 = record
    Width: UINT;
    Height: UINT;
    Format: DXGI_FORMAT;
    Stereo: BOOL;
    SampleDesc: DXGI_SAMPLE_DESC;
    BufferUsage: DXGI_USAGE;
    BufferCount: UINT;
    Scaling: DXGI_SCALING;
    SwapEffect: DXGI_SWAP_EFFECT;
    AlphaMode: DXGI_ALPHA_MODE;
    Flags: UINT;                    // DXGI_SWAP_CHAIN_FLAG
  end;
  {$EXTERNALSYM DXGI_SWAP_CHAIN_DESC1}

  PDXGI_SWAP_CHAIN_FULLSCREEN_DESC = ^DXGI_SWAP_CHAIN_FULLSCREEN_DESC;
  DXGI_SWAP_CHAIN_FULLSCREEN_DESC = record
    RefreshRate: DXGI_RATIONAL;
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER;
    Scaling: DXGI_MODE_SCALING;
    Windowed: BOOL;
  end;
  {$EXTERNALSYM DXGI_SWAP_CHAIN_FULLSCREEN_DESC}

  PDXGI_PRESENT_PARAMETERS = ^DXGI_PRESENT_PARAMETERS;
  DXGI_PRESENT_PARAMETERS = record
    DirtyRectsCount: UINT;
    pDirtyRects: PRect;
    pScrollRect: TRect;
    pScrollOffset: POINT;
  end;
  {$EXTERNALSYM DXGI_PRESENT_PARAMETERS}


  // Interface IDXGISwapChain1
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISwapChain1);'}
  {$EXTERNALSYM IDXGISwapChain1}
  IDXGISwapChain1 = interface(IDXGISwapChain)
  ['{790a45f7-0d42-4876-983a-0a55cfe6f4aa}']

    function GetDesc1(out pDesc: DXGI_SWAP_CHAIN_DESC1): HResult; stdcall;

    function GetFullscreenDesc(out pDesc: DXGI_SWAP_CHAIN_FULLSCREEN_DESC): HResult; stdcall;

    function GetHwnd(out _pHwnd: HWND): HResult; stdcall;

    function GetCoreWindow(refiid: REFIID;
            out ppUnk: Pointer): HResult; stdcall;

    function Present1(SyncInterval: UINT;
                      PresentFlags: UINT;
                      pPresentParameters: DXGI_PRESENT_PARAMETERS): HResult; stdcall;

    function IsTemporaryMonoSupported(): BOOL; stdcall;

    function GetRestrictToOutput(out ppRestrictToOutput: IDXGIOutput): HResult; stdcall;

    function SetBackgroundColor(const pColor: PDXGI_RGBA): HResult; stdcall;

    function GetBackgroundColor(out pColor: DXGI_RGBA): HResult; stdcall;

    function SetRotation(Rotation: DXGI_MODE_ROTATION): HResult; stdcall;

    function GetRotation(pRotation: DXGI_MODE_ROTATION): HResult; stdcall;

  end;
  IID_IDXGISwapChain1 = IDXGISwapChain1;
  {$EXTERNALSYM IID_IDXGISwapChain1}


  // Interface IDXGIFactory2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory2);'}
  {$EXTERNALSYM IDXGIFactory2}
  IDXGIFactory2 = interface(IDXGIFactory1)
  ['{50c83a1c-e072-4c48-87b0-3630fa36a6d0}']

    function IsWindowedStereoEnabled(): BOOL; stdcall;

    function CreateSwapChainForHwnd(pDevice: IUnknown;
                                    _hWnd: HWND;
                                    const pDesc: PDXGI_SWAP_CHAIN_DESC1;
                                    const pFullscreenDesc: PDXGI_SWAP_CHAIN_FULLSCREEN_DESC;
                                    pRestrictToOutput: IDXGIOutput;
                                    out ppSwapChain: IDXGISwapChain1): HResult; stdcall;

    function CreateSwapChainForCoreWindow(pDevice: IUnknown;
                                          pWindow: IUnknown;
                                          const pDesc: PDXGI_SWAP_CHAIN_DESC1;
                                          pRestrictToOutput: IDXGIOutput;
                                          out ppSwapChain: IDXGISwapChain1): HResult; stdcall;

    function GetSharedResourceAdapterLuid(hResource: THandle;
                                          out _pLuid: LUID): HResult; stdcall;

    function RegisterStereoStatusWindow(WindowHandle: HWND;
                                        wMsg: UINT;
                                        out pdwCookie: DWORD): HResult; stdcall;

    function RegisterStereoStatusEvent(hEvent: THandle;
                                       out pdwCookie: DWORD): HResult; stdcall;

    procedure UnregisterStereoStatus(dwCookie: DWORD); stdcall;

    function RegisterOcclusionStatusWindow(WindowHandle: HWND;
                                           wMsg: UINT;
                                           out pdwCookie: DWORD): HResult; stdcall;

    function RegisterOcclusionStatusEvent(hEvent: THandle;
                                          out pdwCookie: DWORD): HResult; stdcall;

    procedure UnregisterOcclusionStatus(dwCookie: DWORD); stdcall;

    function CreateSwapChainForComposition(pDevice: IUnknown;
                                           const pDesc: PDXGI_SWAP_CHAIN_DESC1;
                                           pRestrictToOutput: IDXGIOutput;
                                           out ppSwapChain: IDXGISwapChain1): HResult; stdcall;

  end;
  IID_IDXGIFactory2 = IDXGIFactory2;
  {$EXTERNALSYM IID_IDXGIFactory2}

  //----------------------------------------------------------------------------
  // IDXGIAdapter2 structures
  //----------------------------------------------------------------------------
  PDXGI_GRAPHICS_PREEMPTION_GRANULARITY = ^DXGI_GRAPHICS_PREEMPTION_GRANULARITY;
  DXGI_GRAPHICS_PREEMPTION_GRANULARITY            = (
    DXGI_GRAPHICS_PREEMPTION_DMA_BUFFER_BOUNDARY  = 0,
    DXGI_GRAPHICS_PREEMPTION_PRIMITIVE_BOUNDARY   = 1,
    DXGI_GRAPHICS_PREEMPTION_TRIANGLE_BOUNDARY    = 2,
    DXGI_GRAPHICS_PREEMPTION_PIXEL_BOUNDARY       = 3,
    DXGI_GRAPHICS_PREEMPTION_INSTRUCTION_BOUNDARY = 4);
  {$EXTERNALSYM DXGI_GRAPHICS_PREEMPTION_GRANULARITY}


  PDXGI_COMPUTE_PREEMPTION_GRANULARITY = ^DXGI_COMPUTE_PREEMPTION_GRANULARITY;
  DXGI_COMPUTE_PREEMPTION_GRANULARITY             = (
    DXGI_COMPUTE_PREEMPTION_DMA_BUFFER_BOUNDARY   = 0,
    DXGI_COMPUTE_PREEMPTION_DISPATCH_BOUNDARY     = 1,
    DXGI_COMPUTE_PREEMPTION_THREAD_GROUP_BOUNDARY = 2,
    DXGI_COMPUTE_PREEMPTION_THREAD_BOUNDARY       = 3,
    DXGI_COMPUTE_PREEMPTION_INSTRUCTION_BOUNDARY  = 4);
  {$EXTERNALSYM DXGI_COMPUTE_PREEMPTION_GRANULARITY}

  PDXGI_ADAPTER_DESC2 = ^DXGI_ADAPTER_DESC2;
  DXGI_ADAPTER_DESC2 = record
    Description: array[0..127] of WideChar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: LUID;
    Flags: UINT;
    GraphicsPreemptionGranularity: DXGI_GRAPHICS_PREEMPTION_GRANULARITY;
    ComputePreemptionGranularity: DXGI_COMPUTE_PREEMPTION_GRANULARITY;
  end;
  {$EXTERNALSYM DXGI_ADAPTER_DESC2}


  // Interface IDXGIAdapter2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIAdapter2);'}
  {$EXTERNALSYM IDXGIAdapter2}
  IDXGIAdapter2 = interface(IDXGIAdapter1)
  ['{0AA1AE0A-FA0E-4B84-8644-E05FF8E5ACB5}']

    function GetDesc2(out pDesc: DXGI_ADAPTER_DESC2): HResult; stdcall;

  end;
  IID_IDXGIAdapter2 = IDXGIAdapter2;
  {$EXTERNALSYM IID_IDXGIAdapter2}


  // Interface IDXGIOutput1
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutput1);'}
  {$EXTERNALSYM IDXGIOutput1}
  IDXGIOutput1 = interface(IDXGIOutput)
  ['{00cddea8-939b-4b83-a340-a685226666cc}']

    function GetDisplayModeList1(EnumFormat: DXGI_FORMAT;
                                 Flags: UINT;
                                 var pNumModes: UINT;
                                 out pDesc: PDXGI_MODE_DESC1): HResult; stdcall;

    function FindClosestMatchingMode1(const pModeToMatch: PDXGI_MODE_DESC1;
                                      out pClosestMatch: DXGI_MODE_DESC1;
                                      pConcernedDevice: IUnknown): HResult; stdcall;

    function GetDisplaySurfaceData1(pDestination: IDXGIResource): HResult; stdcall;

    function DuplicateOutput(pDevice: IUnknown;
                             out ppOutputDuplication: IDXGIOutputDuplication): HResult; stdcall;

  end;
  IID_IDXGIOutput1 = IDXGIOutput1;
  {$EXTERNALSYM IID_IDXGIOutput1}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
