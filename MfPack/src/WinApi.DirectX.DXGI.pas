// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGI.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Microsoft DirectX Graphics Infrastructure API
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
// Remarks: -
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
// Source: dxgi.h
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
unit WinApi.DirectX.DXGI;

  {$HPPEMIT '#include "dxgi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {DirectX}
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGIType,
  WinApi.DirectX.DXGIFormat;

  {$WEAKPACKAGEUNIT ON}


//--------------------------------------------------------------------------------------------------------
// DXGI API-only types
//--------------------------------------------------------------------------------------------------------

const
//--------------------------------------------------------------------------------------------------------
// DXGI usages are a UINT with the following fields:
// 33222222222211111111110000000000
// 10987654321098765432109876543210
// [PC][PRIVATE][     DXGI    ][DA]
//
// DA is a 4-bit field that encodes CPU data access patterns.
//    0     No CPU access. Maps should be validated to fail on this access.
//    1     Dynamic: Frequent CPU write-only access, high-performance GPU read-only access.
//    2     CPU-friendly: Frequent CPU read/write access, non-optimal GPU read-only access.
//    3     CPU-scratch: Frequent CPU read/write access, no GPU access.
  DXGI_CPU_ACCESS_NONE                = 0;
  {$EXTERNALSYM DXGI_CPU_ACCESS_NONE}
  DXGI_CPU_ACCESS_DYNAMIC             = 1;
  {$EXTERNALSYM DXGI_CPU_ACCESS_DYNAMIC}
  DXGI_CPU_ACCESS_READ_WRITE          = 2;
  {$EXTERNALSYM DXGI_CPU_ACCESS_READ_WRITE}
  DXGI_CPU_ACCESS_SCRATCH             = 3;
  {$EXTERNALSYM DXGI_CPU_ACCESS_SCRATCH}
  DXGI_CPU_ACCESS_FIELD               = 15;
  {$EXTERNALSYM DXGI_CPU_ACCESS_FIELD}

//
// DXGI is a 15-bit field containing DXGI-defined usages. In DXGI’s first revision, these usages refer only to 2D
//    non-mipped resources: the only resource type that DXGI intends to share cross-process, cross-API. Further
//    2D non-mipped uses can be added here (for example, DXVA extensions)
//
//    Bitfield meanings: (Shift these into bits 18:4)
  DXGI_USAGE_SHADER_INPUT             = $00000010;
  {$EXTERNALSYM DXGI_USAGE_SHADER_INPUT}
  DXGI_USAGE_RENDER_TARGET_OUTPUT     = $00000020;
  {$EXTERNALSYM DXGI_USAGE_RENDER_TARGET_OUTPUT}
  DXGI_USAGE_BACK_BUFFER              = $00000040;
  {$EXTERNALSYM DXGI_USAGE_BACK_BUFFER}
  DXGI_USAGE_SHARED                   = $00000080;
  {$EXTERNALSYM DXGI_USAGE_SHARED}
  DXGI_USAGE_READ_ONLY                = $00000100;
  {$EXTERNALSYM DXGI_USAGE_READ_ONLY}
  DXGI_USAGE_DISCARD_ON_PRESENT       = $00000200;
  {$EXTERNALSYM DXGI_USAGE_DISCARD_ON_PRESENT}
  DXGI_USAGE_UNORDERED_ACCESS         = $00000400;
  {$EXTERNALSYM DXGI_USAGE_UNORDERED_ACCESS}

// See dxgiinternal.idl for private DXGI USAGE BITS, e.g.
// #define DXGI_USAGE_REMOTE_SWAPCHAIN_BUFFER  ( 1L << (15 + 4) )
// #define DXGI_USAGE_GDI_COMPATIBLE           ( 1L << (16 + 4) )

//--------------------------------------------------------------------------------------------------------

  DXGI_RESOURCE_PRIORITY_MINIMUM: UINT = $28000000;
  {$EXTERNALSYM DXGI_RESOURCE_PRIORITY_MINIMUM}
  DXGI_RESOURCE_PRIORITY_LOW:     UINT = $50000000;
  {$EXTERNALSYM DXGI_RESOURCE_PRIORITY_LOW}
  DXGI_RESOURCE_PRIORITY_NORMAL:  UINT = $78000000;
  {$EXTERNALSYM DXGI_RESOURCE_PRIORITY_NORMAL}
  DXGI_RESOURCE_PRIORITY_HIGH:    UINT = $a0000000;
  {$EXTERNALSYM DXGI_RESOURCE_PRIORITY_HIGH}
  DXGI_RESOURCE_PRIORITY_MAXIMUM: UINT = $c8000000;
  {$EXTERNALSYM DXGI_RESOURCE_PRIORITY_MAXIMUM}

  DXGI_MAP_READ:    UINT = 1;
  {$EXTERNALSYM DXGI_MAP_READ}
  DXGI_MAP_WRITE:   UINT = 2;
  {$EXTERNALSYM DXGI_MAP_WRITE}
  DXGI_MAP_DISCARD: UINT = 4;
  {$EXTERNALSYM DXGI_MAP_DISCARD}

  DXGI_ENUM_MODES_INTERLACED: UINT = 1;
  {$EXTERNALSYM DXGI_ENUM_MODES_INTERLACED}
  DXGI_ENUM_MODES_SCALING:    UINT = 2;
  {$EXTERNALSYM DXGI_ENUM_MODES_SCALING}

  DXGI_MAX_SWAP_CHAIN_BUFFERS         = 16;
  {$EXTERNALSYM DXGI_MAX_SWAP_CHAIN_BUFFERS}
  DXGI_PRESENT_TEST                   = $00000001;
  {$EXTERNALSYM DXGI_PRESENT_TEST}
  DXGI_PRESENT_DO_NOT_SEQUENCE        = $00000002;
  {$EXTERNALSYM DXGI_PRESENT_DO_NOT_SEQUENCE}
  DXGI_PRESENT_RESTART                = $00000004;
  {$EXTERNALSYM DXGI_PRESENT_RESTART}
  DXGI_PRESENT_DO_NOT_WAIT            = $00000008;
  {$EXTERNALSYM DXGI_PRESENT_DO_NOT_WAIT}
  DXGI_PRESENT_STEREO_PREFER_RIGHT    = $00000010;
  {$EXTERNALSYM DXGI_PRESENT_STEREO_PREFER_RIGHT}
  DXGI_PRESENT_STEREO_TEMPORARY_MONO  = $00000020;
  {$EXTERNALSYM DXGI_PRESENT_STEREO_TEMPORARY_MONO}
  DXGI_PRESENT_RESTRICT_TO_OUTPUT     = $00000040;
  {$EXTERNALSYM DXGI_PRESENT_RESTRICT_TO_OUTPUT}
  // See dxgidwm.idl for private DXGI Present Bits.
  // DXGI_PRESENT_DDA_PROTECTED_CONTENT = $00000080 // reserved
  DXGI_PRESENT_USE_DURATION           = $00000100;
  {$EXTERNALSYM DXGI_PRESENT_USE_DURATION}
  DXGI_PRESENT_ALLOW_TEARING          = $00000200;
  {$EXTERNALSYM DXGI_PRESENT_ALLOW_TEARING}

  DXGI_MWA_NO_WINDOW_CHANGES          = (1 shl 0);
  {$EXTERNALSYM DXGI_MWA_NO_WINDOW_CHANGES}
  DXGI_MWA_NO_ALT_ENTER               = (1 shl 1);
  {$EXTERNALSYM DXGI_MWA_NO_ALT_ENTER}
  DXGI_MWA_NO_PRINT_SCREEN            = (1 shl 2);
  {$EXTERNALSYM DXGI_MWA_NO_PRINT_SCREEN}
  DXGI_MWA_VALID                      = $7;
  {$EXTERNALSYM DXGI_MWA_VALID}

type
//
//
// PRIVATE is a 9-bit field that has usages private to a particular producer implementation that are irrelevant to 2D bitmaps (for example, d3d index buffers)
//
// PC is a producer-code that defines the namespace within which PRIVATE bits exist. The
// following PC codes are defined:
//    0           DXGI (implying that DXGI usages can be extended to 24 bits (DXGI field plus the 9-bit private field)
//    1           D3D (covering d3d10 and later revisions)typedef DXGI_USAGE UINT;
  PDXGI_USAGE = ^DXGI_USAGE;
  DXGI_USAGE = UINT;
  {$EXTERNALSYM DXGI_USAGE}

//--------------------------------------------------------------------------------------------------------
  PDXGI_FRAME_STATISTICS = ^DXGI_FRAME_STATISTICS;
  DXGI_FRAME_STATISTICS = record
    PresentCount: UINT;
    PresentRefreshCount: UINT;
    SyncRefreshCount: UINT;
    SyncQPCTime: LARGE_INTEGER;
    SyncGPUTime: LARGE_INTEGER;
  end;
  {$EXTERNALSYM DXGI_FRAME_STATISTICS}

//--------------------------------------------------------------------------------------------------------
  PDXGI_MAPPED_RECT = ^DXGI_MAPPED_RECT;
  DXGI_MAPPED_RECT = record
    Pitch: Integer;
    pBits: PByte;
  end;
  {$EXTERNALSYM DXGI_MAPPED_RECT}

//--------------------------------------------------------------------------------------------------------
  // The system LUID struct isn't defined in wtypes, so we repeat it here just
  // for the MIDL compiler.
  // Delphi Note: The LUID (// Locally Unique Identifier) is also defined in MfpTypes.pas

{$IFNDEF MFP_LUID}
  PLUID = ^_LUID;
  _LUID = record
    LowPart: DWORD;
    HighPart: LONG;
  end;
  LUID = _LUID;
  {$EXTERNALSYM _LUID}
{$ENDIF}

//--------------------------------------------------------------------------------------------------------
  PDXGI_ADAPTER_DESC = ^DXGI_ADAPTER_DESC;
  DXGI_ADAPTER_DESC = record
    Description: array[0..127] of WideChar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: LUID;
  end;
  {$EXTERNALSYM DXGI_ADAPTER_DESC}

//--------------------------------------------------------------------------------------------------------
// HMONITOR: This appears to be best (!) practice for preprocessor-declared windows macros that
// need to be expressed as actual types in midl...

{$IFNDEF MFP_HMONITOR}
  HMONITOR = THandle;
  PHMONITOR = ^HMONITOR;
{$ENDIF}

  PDXGI_OUTPUT_DESC = ^DXGI_OUTPUT_DESC;
  DXGI_OUTPUT_DESC = record
    DeviceName: array[0..31] of WideChar;  // Device name for GDI (ex. \\.\DISPLAY1)
    DesktopCoordinates: TRect;
    AttachedToDesktop: BOOL;
    Rotation: DXGI_MODE_ROTATION;
    Monitor: HMONITOR;
  end;
  {$EXTERNALSYM DXGI_OUTPUT_DESC}

//--------------------------------------------------------------------------------------------------------
  PDXGI_SHARED_RESOURCE = ^DXGI_SHARED_RESOURCE;
  DXGI_SHARED_RESOURCE = record
    Handle: THandle;
  end;
  {$EXTERNALSYM DXGI_SHARED_RESOURCE}

//--------------------------------------------------------------------------------------------------------
type
  PDXGI_RESIDENCY = ^DXGI_RESIDENCY;
  DXGI_RESIDENCY = DWord;
  {$EXTERNALSYM DXGI_RESIDENCY}
const
  DXGI_RESIDENCY_FULLY_RESIDENT            = DXGI_RESIDENCY(1);
  {$EXTERNALSYM DXGI_RESIDENCY_FULLY_RESIDENT}
  DXGI_RESIDENCY_RESIDENT_IN_SHARED_MEMORY = DXGI_RESIDENCY(2);
  {$EXTERNALSYM DXGI_RESIDENCY_RESIDENT_IN_SHARED_MEMORY}
  DXGI_RESIDENCY_EVICTED_TO_DISK           = DXGI_RESIDENCY(3);
  {$EXTERNALSYM DXGI_RESIDENCY_EVICTED_TO_DISK}

//--------------------------------------------------------------------------------------------------------
type
  PDXGI_SURFACE_DESC = ^DXGI_SURFACE_DESC;
  DXGI_SURFACE_DESC = record
    Width: UINT;
    Height: UINT;
    Format: DXGI_FORMAT;
    SampleDesc: DXGI_SAMPLE_DESC;
  end;
  {$EXTERNALSYM DXGI_SURFACE_DESC}

//--------------------------------------------------------------------------------------------------------
type
  PDXGI_SWAP_EFFECT = ^DXGI_SWAP_EFFECT;
  DXGI_SWAP_EFFECT = DWord;
  {$EXTERNALSYM DXGI_SWAP_EFFECT}
const
  DXGI_SWAP_EFFECT_DISCARD         = DXGI_SWAP_EFFECT(0);
  {$EXTERNALSYM DXGI_SWAP_EFFECT_DISCARD}
  DXGI_SWAP_EFFECT_SEQUENTIAL      = DXGI_SWAP_EFFECT(1);
  {$EXTERNALSYM DXGI_SWAP_EFFECT_SEQUENTIAL}
  DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL = DXGI_SWAP_EFFECT(3);
  {$EXTERNALSYM DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL}
  DXGI_SWAP_EFFECT_FLIP_DISCARD    = DXGI_SWAP_EFFECT(4);
  {$EXTERNALSYM DXGI_SWAP_EFFECT_FLIP_DISCARD}

//--------------------------------------------------------------------------------------------------------
type
  PDXGI_SWAP_CHAIN_FLAG = ^DXGI_SWAP_CHAIN_FLAG;
  DXGI_SWAP_CHAIN_FLAG = DWord;
  {$EXTERNALSYM DXGI_SWAP_CHAIN_FLAG}
const
  DXGI_SWAP_CHAIN_FLAG_NONPREROTATED                          = DXGI_SWAP_CHAIN_FLAG(1);
  DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH                      = DXGI_SWAP_CHAIN_FLAG(2);
  DXGI_SWAP_CHAIN_FLAG_GDI_COMPATIBLE                         = DXGI_SWAP_CHAIN_FLAG(4);
  DXGI_SWAP_CHAIN_FLAG_RESTRICTED_CONTENT                     = DXGI_SWAP_CHAIN_FLAG(8);
  DXGI_SWAP_CHAIN_FLAG_RESTRICT_SHARED_RESOURCE_DRIVER        = DXGI_SWAP_CHAIN_FLAG(16);
  DXGI_SWAP_CHAIN_FLAG_DISPLAY_ONLY                           = DXGI_SWAP_CHAIN_FLAG(32);
  DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT          = DXGI_SWAP_CHAIN_FLAG(64);
  DXGI_SWAP_CHAIN_FLAG_FOREGROUND_LAYER                       = DXGI_SWAP_CHAIN_FLAG(128);
  DXGI_SWAP_CHAIN_FLAG_FULLSCREEN_VIDEO                       = DXGI_SWAP_CHAIN_FLAG(256);
  DXGI_SWAP_CHAIN_FLAG_YUV_VIDEO                              = DXGI_SWAP_CHAIN_FLAG(512);
  DXGI_SWAP_CHAIN_FLAG_HW_PROTECTED                           = DXGI_SWAP_CHAIN_FLAG(1024);
  DXGI_SWAP_CHAIN_FLAG_ALLOW_TEARING                          = DXGI_SWAP_CHAIN_FLAG(2048);
  DXGI_SWAP_CHAIN_FLAG_RESTRICTED_TO_ALL_HOLOGRAPHIC_DISPLAYS = DXGI_SWAP_CHAIN_FLAG(4096);

type
  PDXGI_ADAPTER_FLAG = ^DXGI_ADAPTER_FLAG;
  DXGI_ADAPTER_FLAG = DWord;
  {$EXTERNALSYM DXGI_ADAPTER_FLAG}
const
  DXGI_ADAPTER_FLAG_NONE        = DXGI_ADAPTER_FLAG(0);
  DXGI_ADAPTER_FLAG_REMOTE      = DXGI_ADAPTER_FLAG(1);
  DXGI_ADAPTER_FLAG_SOFTWARE    = DXGI_ADAPTER_FLAG(2);
  //DXGI_ADAPTER_FLAG_FORCE_DWORD = FORCEDWORD;

//--------------------------------------------------------------------------------------------------------
type
  PDXGISwapChainDesc = ^DXGI_SWAP_CHAIN_DESC;
  PDXGI_SWAP_CHAIN_DESC = ^DXGI_SWAP_CHAIN_DESC;
  DXGI_SWAP_CHAIN_DESC = record
    BufferDesc: DXGI_MODE_DESC;
    SampleDesc: DXGI_SAMPLE_DESC;
    BufferUsage: DXGI_USAGE;
    BufferCount: UINT;
    OutputWindow: HWND;
    Windowed: BOOL;
    SwapEffect: DXGI_SWAP_EFFECT;
    Flags: UINT;                    // DXGI_SWAP_CHAIN_FLAG
  end;
  {$EXTERNALSYM DXGI_SWAP_CHAIN_DESC}


  //--------------------------------------------------------------------------------------------------------
  // DXGI object hierarchy base interfaces
  //--------------------------------------------------------------------------------------------------------

  IDXGIOutput = interface;
  IDXGIAdapter1 = interface;


  // INTERFACES ////////////////////////////////////////////////////////////////

  // Interface IDXGIObject
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIObject);'}
  {$EXTERNALSYM IDXGIObject}
  IDXGIObject = interface(IUnknown)
  ['{aec22fb8-76f3-4639-9be0-28eb43a67a2e}']

    //ULONG   Release();
    function SetPrivateData(const Name: TGuid;
                            DataSize: UINT;
                            pData: Pointer): HResult; stdcall;

    function SetPrivateDataInterface(const Name: TGuid;
                                     pUnknown: IUnknown): HResult; stdcall;

    function GetPrivateData(const Name: TGuid;
                            var pDataSize: UINT;
                      {out} var pData: Pointer): HResult; stdcall;

    function GetParent(const riid: TGuid;
                       var ppParent: pointer ): HResult; stdcall;

  end;
  IID_IDXGIObject = IDXGIObject;
  {$EXTERNALSYM IID_IDXGIObject}


//--------------------------------------------------------------------------------------------------------

  // Interface IDXGIDeviceSubObject
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDeviceSubObject);'}
  {$EXTERNALSYM IDXGIDeviceSubObject}
  IDXGIDeviceSubObject = interface(IDXGIObject)
  ['{3d3e0379-f9de-4d58-bb6c-18d62992f1a6}']

    function GetDevice(const riid: TGuid;
                       var ppDevice: pointer ): HResult; stdcall;

  end;
  IID_IDXGIDeviceSubObject = IDXGIDeviceSubObject;
  {$EXTERNALSYM IID_IDXGIDeviceSubObject}


//--------------------------------------------------------------------------------------------------------

  // Interface IDXGIResource
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIResource);'}
  {$EXTERNALSYM IDXGIResource}
  IDXGIResource = interface(IDXGIDeviceSubObject)
  ['{035f3ab4-482e-4e50-b41f-8a7f8bd8960b}']

    function GetSharedHandle(out pSharedHandle: THandle): HResult; stdcall;

    function GetUsage(out pUsage: DXGI_USAGE): HResult; stdcall;

    function SetEvictionPriority(EvictionPriority: UINT): HResult; stdcall;

    function GetEvictionPriority(out pEvictionPriority: UINT): HResult; stdcall;

  end;
  IID_IDXGIResource = IDXGIResource;
  {$EXTERNALSYM IID_IDXGIResource}

//--------------------------------------------------------------------------------------------------------

  // Interface IDXGIKeyedMutex
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIKeyedMutex);'}
  {$EXTERNALSYM IDXGIKeyedMutex}
  IDXGIKeyedMutex = interface(IDXGIDeviceSubObject)
  ['{9d8e1289-d7b3-465f-8126-250e349af85d}']

    function AcquireSync(Key: UINT64;
                         dwMilliseconds: DWORD): HResult; stdcall;

    function ReleaseSync(Key: UINT64): HResult; stdcall;

  end;
  IID_IDXGIKeyedMutex = IDXGIKeyedMutex;
  {$EXTERNALSYM IID_IDXGIKeyedMutex}


//--------------------------------------------------------------------------------------------------------
// The DXGISurface
//--------------------------------------------------------------------------------------------------------

  // Interface IDXGISurface
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISurface);'}
  {$EXTERNALSYM IDXGISurface}
  IDXGISurface = interface(IDXGIDeviceSubObject)
  ['{cafcb56c-6ac3-4889-bf47-9e23bbd260ec}']

    function GetDesc(out pDesc: DXGI_SURFACE_DESC): HResult; stdcall;

    function Map(out pLockedRect: PDXGI_MAPPED_RECT;
                 MapFlags: UINT): HResult; stdcall;

    function Unmap(): HResult; stdcall;

  end;
  IID_IDXGISurface = IDXGISurface;
  {$EXTERNALSYM IID_IDXGISurface}


//--------------------------------------------------------------------------------------------------------
// The DXGISurface1
//--------------------------------------------------------------------------------------------------------

  // Interface IDXGISurface1
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISurface1);'}
  {$EXTERNALSYM IDXGISurface1}
  IDXGISurface1 = interface(IDXGISurface)
  ['{4AE63092-6327-4c1b-80AE-BFE12EA32B86}']

    function GetDC(Discard: BOOL;
                   out phdc: HDC): HResult; stdcall;

    function ReleaseDC(pDirtyRect: TRect): HResult; stdcall;

  end;
  IID_IDXGISurface1 = IDXGISurface1;
  {$EXTERNALSYM IID_IDXGISurface1}


  //--------------------------------------------------------------------------------------------------------
  // The DXGIAdapter
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGIAdapter
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIAdapter);'}
  {$EXTERNALSYM IDXGIAdapter}
  IDXGIAdapter = interface(IDXGIObject)
  ['{2411e7e1-12ac-4ccf-bd14-9798e8534dc0}']

    function EnumOutputs(Output: UINT;
                         var ppOutput: IDXGIOutput): HResult; stdcall;

    function GetDesc(out pDesc: DXGI_ADAPTER_DESC): HResult; stdcall;

    function CheckInterfaceSupport(const InterfaceName: TGuid;
                                   out pUMDVersion: LARGE_INTEGER): HResult; stdcall;

  end;
  IID_IDXGIAdapter = IDXGIAdapter;
  {$EXTERNALSYM IID_IDXGIAdapter}

//--------------------------------------------------------------------------------------------------------

// Additional mode enumerations in DXGI 1.2

//--------------------------------------------------------------------------------------------------------
// The DXGIOutput
//--------------------------------------------------------------------------------------------------------

  // Interface IDXGIOutput
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutput);'}
  {$EXTERNALSYM IDXGIOutput}
  IDXGIOutput = interface(IDXGIObject)
  ['{ae02eedb-c735-4690-8d52-5a8dc20213aa}']

    function GetDesc(out pDesc: DXGI_OUTPUT_DESC): HResult; stdcall;

    function GetDisplayModeList(EnumFormat: DXGI_FORMAT;
                                Flags: UINT;
                                var pNumModes: UINT;
                                out pDesc: PDXGI_MODE_DESC): HResult; stdcall;

    function FindClosestMatchingMode(const pModeToMatch: DXGI_MODE_DESC;
                                     out pClosestMatch: DXGI_MODE_DESC;
                                     const pConcernedDevice: IUnknown): HResult; stdcall;

    function WaitForVBlank(): HResult; stdcall;

    function TakeOwnership(pDevice: IUnknown;
                           Exclusive: BOOL): HResult; stdcall;

    procedure ReleaseOwnership();

    //The following methods can only be called when this output is owned by a device.

    function GetGammaControlCapabilities(out pGammaCaps: DXGI_GAMMA_CONTROL_CAPABILITIES): HResult; stdcall;

    function SetGammaControl(const pArray: DXGI_GAMMA_CONTROL): HResult; stdcall;

    function GetGammaControl(out pArray: PDXGI_GAMMA_CONTROL): HResult; stdcall;

    function SetDisplaySurface(const pScanoutSurface: IDXGISurface): HResult; stdcall;

    function GetDisplaySurfaceData(const pDestination: IDXGISurface): HResult; stdcall;

    function GetFrameStatistics(out pStats: DXGI_FRAME_STATISTICS): HResult; stdcall;

  end;
  IID_IDXGIOutput = IDXGIOutput;
  {$EXTERNALSYM IID_IDXGIOutput}

  //--------------------------------------------------------------------------------------------------------
  // The DXGI SwapChain
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGISwapChain
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISwapChain);'}
  {$EXTERNALSYM IDXGISwapChain}
  IDXGISwapChain = interface(IDXGIDeviceSubObject)
  ['{310d36a0-d2e7-4c0a-aa04-6a9d23b8886a}']

    function Present(SyncInterval: UINT;
                     Flags: UINT): HResult; stdcall;

    function GetBuffer(Buffer: UINT;
                       const riid: TGuid;
                       var ppSurface): HResult; stdcall;

    function SetFullscreenState(Fullscreen: BOOL;
                                const pTarget: IDXGIOutput): HResult; stdcall;

    function GetFullscreenState(out pFullscreen: BOOL;
                                out ppTarget: IDXGIOutput): HResult; stdcall;

    function GetDesc(out pDesc: DXGI_SWAP_CHAIN_DESC): HResult; stdcall;

    function ResizeBuffers(BufferCount: UINT;
                           Width: UINT;
                           Height: UINT;
                           NewFormat: DXGI_FORMAT;
                           SwapChainFlags: UINT): HResult; stdcall;

    function ResizeTarget(pNewTargetParameters: DXGI_MODE_DESC): HResult; stdcall;

    function GetContainingOutput(out ppOutput: IDXGIOutput): HResult; stdcall;

    function GetFrameStatistics(out pStats: DXGI_FRAME_STATISTICS): HResult; stdcall;

    function GetLastPresentCount(out pLastPresentCount: UINT): HResult; stdcall;

  end;
  IID_IDXGISwapChain = IDXGISwapChain;
  {$EXTERNALSYM IID_IDXGISwapChain}

  //--------------------------------------------------------------------------------------------------------
  // The DXGIFactory
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGIFactory
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory);'}
  {$EXTERNALSYM IDXGIFactory}
  IDXGIFactory = interface(IDXGIObject)
  ['{7b7166ec-21c7-44ae-b21a-c9ae321ae369}']

    function EnumAdapters(Adapter: UINT;
                          out ppAdapter: IDXGIAdapter): HResult; stdcall;

    function MakeWindowAssociation(WindowHandle: HWND;
                                   Flags: UINT): HResult; stdcall; //pass NULL to break the association

    function GetWindowAssociation(out pWindowHandle: HWND): HResult; stdcall;

    function CreateSwapChain(pDevice: IUnknown;
                             const pDesc: DXGI_SWAP_CHAIN_DESC;
                             out ppSwapChain: IDXGISwapChain): HResult; stdcall;

    function CreateSoftwareAdapter(Module: HMODULE;
                                   out ppAdapter: IDXGIAdapter): HResult; stdcall;

  end;
  IID_IDXGIFactory = IDXGIFactory;
  {$EXTERNALSYM IID_IDXGIFactory}


  //----------------------------- ---------------------------------------------------------------------------
  // The DXGIDevice base interface
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGIDevice
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDevice);'}
  {$EXTERNALSYM IDXGIDevice}
  PIDXGIDevice = ^IDXGIDevice;
  IDXGIDevice = interface(IDXGIObject)
  ['{54ec77fa-1377-44e6-8c32-88fd5f44c84c}']

    function GetAdapter(out pAdapter: IDXGIAdapter): HResult; stdcall;

    function CreateSurface(pDesc: DXGI_SURFACE_DESC;
                           NumSurfaces: UINT;
                           Usage: DXGI_USAGE;
                           const pSharedResource: DXGI_SHARED_RESOURCE;
                           out ppSurface: IDXGISurface): HResult; stdcall;

    function QueryResourceResidency(const ppResources: PIUnknown;
                                    out pResidencyStatus: DXGI_RESIDENCY;
                                    NumResources: UINT): HResult; stdcall;

    function SetGPUThreadPriority(Priority: Integer): HResult; stdcall;

    function GetGPUThreadPriority(out pPriority: Integer): HResult; stdcall;

  end;
  IID_IDXGIDevice = IDXGIDevice;
  {$EXTERNALSYM IID_IDXGIDevice}


  //--------------------------------------------------------------------------------------------------------
  // DXGI 1.1
  //--------------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------------
  PDXGI_ADAPTER_DESC1 = ^DXGI_ADAPTER_DESC1;
  DXGI_ADAPTER_DESC1 = record
    Description: array[0..127] of WideChar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: LUID;
    Flags: UINT;                    // DXGI_ADAPTER_FLAG
  end;
  {$EXTERNALSYM DXGI_ADAPTER_DESC1}

//--------------------------------------------------------------------------------------------------------
  PDXGI_DISPLAY_COLOR_SPACE = ^DXGI_DISPLAY_COLOR_SPACE;
  DXGI_DISPLAY_COLOR_SPACE = record
    PrimaryCoordinates: array[0..7, 0..1] of Single;
    WhitePoints: array[0..15, 0..1] of Single;
  end;
  {$EXTERNALSYM DXGI_DISPLAY_COLOR_SPACE}

//--------------------------------------------------------------------------------------------------------
// The DXGIFactory1
//--------------------------------------------------------------------------------------------------------

  // Interface IDXGIFactory1
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory1);'}
  {$EXTERNALSYM IDXGIFactory1}
  IDXGIFactory1 = interface(IDXGIFactory)
  ['{770aae78-f26f-4dba-a829-253c83d1b387}']

    function EnumAdapters1(Adapter: UINT;
                           out ppAdapter: IDXGIAdapter1): HResult; stdcall;

    function IsCurrent(): BOOL; stdcall;

  end;
  IID_IDXGIFactory1 = IDXGIFactory1;
  {$EXTERNALSYM IID_IDXGIFactory1}

  //--------------------------------------------------------------------------------------------------------
  // The DXGIAdapter1
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGIAdapter1
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIAdapter1);'}
  {$EXTERNALSYM IDXGIAdapter1}
  IDXGIAdapter1 = interface(IDXGIAdapter)
  ['{29038f61-3839-4626-91fd-086879011a05}']

    function GetDesc1(out pDesc: DXGI_ADAPTER_DESC1): HResult; stdcall;

  end;
  IID_IDXGIAdapter1 = IDXGIAdapter1;
  {$EXTERNALSYM IID_IDXGIAdapter1}

  //--------------------------------------------------------------------------------------------------------
  // The DXGIDevice1
  //--------------------------------------------------------------------------------------------------------
  // Interface IDXGIDevice1
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIDevice1);'}
  {$EXTERNALSYM IDXGIDevice1}
  IDXGIDevice1 = interface(IDXGIDevice)
  ['{77db970f-6276-48ba-ba28-070143b4392c}']

    function SetMaximumFrameLatency(MaxLatency: UINT): HResult; stdcall;

    function GetMaximumFrameLatency(out pMaxLatency: UINT): HResult; stdcall;

  end;
  IID_IDXGIDevice1 = IDXGIDevice1;
  {$EXTERNALSYM IID_IDXGIDevice1}


  //--------------------------------------------------------------------------------------------------------
  // DXGI instantiation
  //--------------------------------------------------------------------------------------------------------

  function CreateDXGIFactory(const riid: TGuid;
                             out ppFactory: IDXGIFactory): HResult; stdcall
  {$EXTERNALSYM CreateDXGIFactory}

  function CreateDXGIFactory1(const riid: TGuid;
                              var ppFactory: pointer): HResult; stdcall
  {$EXTERNALSYM CreateDXGIFactory1}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  DXGIlib = 'dxgi.dll';

{$WARN SYMBOL_PLATFORM OFF}
function CreateDXGIFactory; external DXGIlib name 'CreateDXGIFactory' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function CreateDXGIFactory1; external DXGIlib name 'CreateDXGIFactory1' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
