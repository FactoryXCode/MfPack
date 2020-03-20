// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - DXGI
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DXGI1_6.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
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
// Source: dxgi1_6.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
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
unit MfPack.DXGI1_6;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "dxgi1_6.h"'}
  {$HPPEMIT ''}

interface

uses
  {WinApi}
  WinApi.Windows,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.DXGIType,
  Mfpack.DXGICommon,
  Mfpack.DXGI1_2,
  Mfpack.DXGI1_4,
  Mfpack.DXGI1_5;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$INCLUDE 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

type

  PDXGI_ADAPTER_FLAG3 = ^DXGI_ADAPTER_FLAG3;
  DXGI_ADAPTER_FLAG3                                = (
    DXGI_ADAPTER_FLAG3_NONE                         = 0,
    DXGI_ADAPTER_FLAG3_REMOTE                       = 1,
    DXGI_ADAPTER_FLAG3_SOFTWARE                     = 2,
    DXGI_ADAPTER_FLAG3_ACG_COMPATIBLE               = 4,
    DXGI_ADAPTER_FLAG3_SUPPORT_MONITORED_FENCES     = 8,
    DXGI_ADAPTER_FLAG3_SUPPORT_NON_MONITORED_FENCES = $10,
    DXGI_ADAPTER_FLAG3_KEYED_MUTEX_CONFORMANCE      = $20,
    DXGI_ADAPTER_FLAG3_FORCE_DWORD                  = FORCEDWORD);
  {$EXTERNALSYM DXGI_ADAPTER_FLAG3}


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
    RedPrimary: array[0..1] of FLOAT;
    GreenPrimary: array[0..1] of FLOAT;
    BluePrimary: array[0..1] of FLOAT;
    // White point of this output in xy coordinates
    WhitePoint: array[0..1] of FLOAT;
    // Minimum luminance supported on this output in nits
    MinLuminance: FLOAT;
    // Maximum peak luminance supported on this output
    // in nits; usually only possible on a small fraction
    // of the display
    MaxLuminance: FLOAT;
    // Maximum average luminance supported on this
    // output in nits; valid for the full frame of the
    // display
    MaxFullFrameLuminance: FLOAT;
  end;
  {$EXTERNALSYM DXGI_OUTPUT_DESC1}


//--------------------------------------------------------------------------------------------------------
  PDXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS = ^DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS;
  DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS                   = (
    DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_FULLSCREEN       = 1,
    DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_WINDOWED         = 2,
    DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAG_CURSOR_STRETCHED = 4);
  {$EXTERNALSYM DXGI_HARDWARE_COMPOSITION_SUPPORT_FLAGS}


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

  //+-----------------------------------------------------------------------------
  //
  //  Enum for GPU Preference categories
  //
  //------------------------------------------------------------------------------
  PDXGI_GPU_PREFERENCE = ^DXGI_GPU_PREFERENCE;
  DXGI_GPU_PREFERENCE               = (
    DXGI_GPU_PREFERENCE_UNSPECIFIED = 0,
    DXGI_GPU_PREFERENCE_MINIMUM_POWER,
    DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE);
  {$EXTERNALSYM DXGI_GPU_PREFERENCE}


  // Interface IDXGIFactory6
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory6);'}
  {$EXTERNALSYM IDXGIFactory6}
  IDXGIFactory6 = interface(IDXGIFactory5)
  ['{c1b6694f-ff09-44a9-b03c-77900a0a1d17}']

    function EnumAdapterByGpuPreference(Adapter: UINT;
                                        GpuPreference: DXGI_GPU_PREFERENCE;
                                        riid: REFIID;
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
