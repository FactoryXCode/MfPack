// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGI1_6.pas
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
// Source: dxgi1_6.h
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
unit WinApi.DirectX.DXGI1_6;

  {$HPPEMIT '#include "dxgi1_6.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DXGIType,
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGI1_4,
  WinApi.DirectX.DXGI1_5;

  {$WEAKPACKAGEUNIT ON}

// Enums =======================================================================

type
  PDXGI_ADAPTER_FLAG3 = ^DXGI_ADAPTER_FLAG3;
  DXGI_ADAPTER_FLAG3 = DWord;
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3}
const
  DXGI_ADAPTER_FLAG3_NONE                         = DXGI_ADAPTER_FLAG3(0);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_NONE}
  DXGI_ADAPTER_FLAG3_REMOTE                       = DXGI_ADAPTER_FLAG3(1);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_REMOTE}
  DXGI_ADAPTER_FLAG3_SOFTWARE                     = DXGI_ADAPTER_FLAG3(2);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_SOFTWARE}
  DXGI_ADAPTER_FLAG3_ACG_COMPATIBLE               = DXGI_ADAPTER_FLAG3(4);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_ACG_COMPATIBLE}
  DXGI_ADAPTER_FLAG3_SUPPORT_MONITORED_FENCES     = DXGI_ADAPTER_FLAG3(8);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_SUPPORT_MONITORED_FENCES}
  DXGI_ADAPTER_FLAG3_SUPPORT_NON_MONITORED_FENCES = DXGI_ADAPTER_FLAG3($10);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_SUPPORT_NON_MONITORED_FENCES}
  DXGI_ADAPTER_FLAG3_KEYED_MUTEX_CONFORMANCE      = DXGI_ADAPTER_FLAG3($20);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3_KEYED_MUTEX_CONFORMANCE}
  //DXGI_ADAPTER_FLAG3_FORCE_DWORD                  = FORCEDWORD;

type
  PDXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS = ^DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS;
  DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS = DWord;
  {$EXTERNALSYM DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS}
const
  DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_FULLSCREEN       = DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS(1);
  {$EXTERNALSYM DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_FULLSCREEN}
  DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_WINDOWED         = DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS(2);
  {$EXTERNALSYM DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_WINDOWED}
  DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_CURSOR_STRETCHED = DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS(4);
  {$EXTERNALSYM DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_CURSOR_STRETCHED}

type
  //+-----------------------------------------------------------------------------
  //  Enum for GPU Preference categories
  //------------------------------------------------------------------------------
  PDXGI_GPU_PREFERENCE = ^DXGI_GPU_PREFERENCE;
  DXGI_GPU_PREFERENCE = DWord;
  {$EXTERNALSYM DXGI_GPU_PREFERENCE}
const
  DXGI_GPU_PREFERENCE_UNSPECIFIED      = DXGI_GPU_PREFERENCE(0);
  {$EXTERNALSYM DXGI_GPU_PREFERENCE_UNSPECIFIED}
  DXGI_GPU_PREFERENCE_MINIMUM_POWER    = DXGI_GPU_PREFERENCE(1);
  {$EXTERNALSYM DXGI_GPU_PREFERENCE_MINIMUM_POWER}
  DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE = DXGI_GPU_PREFERENCE(2);
  {$EXTERNALSYM DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE}

// =============================================================================


type
  PDXGI_ADAPTER_DESC3 = ^DXGI_ADAPTER_DESC3;
  DXGI_ADAPTER_DESC3 = record
    Description: array[0..127] of WideChar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: LUID;
    Flags: DXGI_ADAPTER_FLAG3;
    GraphicsPreemptionGranularity: DXGI_GRAPHICS_PREEMPTION_GRANULARITY;
    ComputePreemptionGranularity: DXGI_COMPUTE_PREEMPTION_GRANULARITY;
  end;
  {$EXTERNALSYM DXGI_ADAPTER_DESC3}


  // Interface IDXGIAdapter4
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIAdapter4);'}
  {$EXTERNALSYM IDXGIAdapter4}
  IDXGIAdapter4 = interface(IDXGIAdapter3)
  ['{3c8d99d1-4fbf-4181-a82c-af66bf7bd24e}']

    function GetDesc3(out pDesc: DXGI_ADAPTER_DESC3): HResult; stdcall;

  end;
  IID_IDXGIAdapter4 = IDXGIAdapter4;
  {$EXTERNALSYM IID_IDXGIAdapter4}

//--------------------------------------------------------------------------------------------------------
  PDXGI_OUTPUT_DESC1 = ^DXGI_OUTPUT_DESC1;
  DXGI_OUTPUT_DESC1 = record
    // Device name for GDI (ex. \\.\DISPLAY1)
    DeviceName: array[0..31] of WideChar;
    DesktopCoordinates: TRect;
    AttachedToDesktop: BOOL;
    Rotation: DXGI_MODE_ROTATION;
    Monitor: HMONITOR;
    // Number of bits per color channel being
    // used for scanout on this output
    BitsPerColor: UINT;
    // Represents whether the current OS state can take
    // advantage of color values larger than sRGB and also
    // specifies if there is any headroom
    ColorSpace: DXGI_COLOR_SPACE_TYPE;
    // Color primaries of this output in xy coordinates
    RedPrimary: array[0..1] of Single;
    GreenPrimary: array[0..1] of Single;
    BluePrimary: array[0..1] of Single;
    // White point of this output in xy coordinates
    WhitePoint: array[0..1] of Single;
    // Minimum luminance supported on this output in nits
    MinLuminance: Single;
    // Maximum peak luminance supported on this output
    // in nits; usually only possible on a small fraction
    // of the display
    MaxLuminance: Single;
    // Maximum average luminance supported on this
    // output in nits; valid for the full frame of the
    // display
    MaxFullFrameLuminance: Single;
  end;
  {$EXTERNALSYM DXGI_OUTPUT_DESC1}


//--------------------------------------------------------------------------------------------------------


  // Interface IDXGIOutput6
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutput6);'}
  {$EXTERNALSYM IDXGIOutput6}
  IDXGIOutput6 = interface(IDXGIOutput5)
  ['{068346e8-aaec-4b84-add7-137f513f77a1}']

    function GetDesc1(out pDesc: DXGI_OUTPUT_DESC1): HResult; stdcall;

    function CheckHardwareCompositionSupport(out pFlags: UINT): HResult; stdcall;

  end;
  IID_IDXGIOutput6 = IDXGIOutput6;
  {$EXTERNALSYM IID_IDXGIOutput6}


  // Interface IDXGIFactory6
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory6);'}
  {$EXTERNALSYM IDXGIFactory6}
  IDXGIFactory6 = interface(IDXGIFactory5)
  ['{c1b6694f-ff09-44a9-b03c-77900a0a1d17}']

    function EnumAdapterByGpuPreference(Adapter: UINT;
                                        GpuPreference: DXGI_GPU_PREFERENCE;
                                        riid: TGuid;
                                        out ppvAdapter: Pointer): HResult; stdcall;

  end;
  IID_IDXGIFactory6 = IDXGIFactory6;
  {$EXTERNALSYM IID_IDXGIFactory6}


  // Interface IDXGIFactory7
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory7);'}
  {$EXTERNALSYM IDXGIFactory7}
  IDXGIFactory7 = interface(IDXGIFactory6)
  ['{a4966eed-76db-44da-84c1-ee9a7afb20a8}']

    function RegisterAdaptersChangedEvent(hEvent: THandle;
                                          out pdwCookie: DWORD): HResult; stdcall;

    function UnregisterAdaptersChangedEvent(dwCookie: DWORD): HResult; stdcall;

  end;
  IID_IDXGIFactory7 = IDXGIFactory7;
  {$EXTERNALSYM IID_IDXGIFactory7}

//--------------------------------------------------------------------------------------------------------

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
