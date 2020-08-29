// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DXGI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXGI1_4.pas
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
// Source: dxgi1_4.h
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
unit WinApi.DirectX.DXGI1_4;

  {$HPPEMIT '#include "dxgi1_4.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DXGICommon,
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGI1_2,
  WinApi.DirectX.DXGI1_3;

  {$WEAKPACKAGEUNIT ON}


// Enums =======================================================================

type
  //+---------------------------------------------------------------------------
  //
  //  Return flags from CheckColorSpaceSupport
  //
  //----------------------------------------------------------------------------
  PDXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG = ^DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG;
  DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG = DWord;
  {$EXTERNALSYM DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG}
const
  DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG_PRESENT         = DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG($00000001);
  {$EXTERNALSYM DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG_PRESENT}
  DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG_OVERLAY_PRESENT = DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG($00000002);
  {$EXTERNALSYM DXGI_SWAP_CHAIN_COLOR_SPACE_SUPPORT_FLAG_OVERLAY_PRESENT}

type
  //+-----------------------------------------------------------------------------
  //
  //  Return flags from CheckOverlayColorSpaceSupport
  //
  //------------------------------------------------------------------------------
  PDXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG = ^DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG;
  DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG = DWord;
  {$EXTERNALSYM DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG}
const
  DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG_PRESENT = DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG($00000001);
  {$EXTERNALSYM DXGI_OVERLAY_COLOR_SPACE_SUPPORT_FLAG_PRESENT}

type
  PDXGI_MEMORY_SEGMENT_GROUP = ^DXGI_MEMORY_SEGMENT_GROUP;
  DXGI_MEMORY_SEGMENT_GROUP = DWord;
  {$EXTERNALSYM DXGI_MEMORY_SEGMENT_GROUP}
const
  DXGI_MEMORY_SEGMENT_GROUP_LOCAL     = DXGI_MEMORY_SEGMENT_GROUP(0);
  {$EXTERNALSYM DXGI_MEMORY_SEGMENT_GROUP_LOCAL}
  DXGI_MEMORY_SEGMENT_GROUP_NON_LOCAL = DXGI_MEMORY_SEGMENT_GROUP(1);
  {$EXTERNALSYM DXGI_MEMORY_SEGMENT_GROUP_NON_LOCAL}

// =============================================================================

type

  // Interface IDXGISwapChain3
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGISwapChain3);'}
  {$EXTERNALSYM IDXGISwapChain3}
  IDXGISwapChain3 = interface(IDXGISwapChain2)
  ['{94d99bdb-f1f8-4ab0-b236-7da0170edab1}']

    function GetCurrentBackBufferIndex(): UINT; stdcall;

    function CheckColorSpaceSupport(ColorSpace: DXGI_COLOR_SPACE_TYPE;
                                    out pColorSpaceSupport: UINT): HResult; stdcall;

    function SetColorSpace1(ColorSpace: DXGI_COLOR_SPACE_TYPE): HResult; stdcall;

    function ResizeBuffers1(BufferCount: UINT;
                            Width: UINT;
                            Height: UINT;
                            Format: DXGI_FORMAT;
                            SwapChainFlags: UINT;
                            pCreationNodeMask: UINT;
                            const ppPresentQueue: IUnknown): HResult; stdcall;

  end;
  IID_IDXGISwapChain3 = IDXGISwapChain3;
  {$EXTERNALSYM IID_IDXGISwapChain3}


  // Interface IDXGIOutput4
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIOutput4);'}
  {$EXTERNALSYM IDXGIOutput4}
  IDXGIOutput4 = interface(IDXGIOutput3)
  ['{dc7dca35-2196-414d-9F53-617884032a60}']

    function CheckOverlayColorSpaceSupport(Format: DXGI_FORMAT;
                                           ColorSpace: DXGI_COLOR_SPACE_TYPE;
                                           pConcernedDevice: IUnknown;
                                           out pFlags: UINT): HResult; stdcall;

  end;
  IID_IDXGIOutput4 = IDXGIOutput4;
  {$EXTERNALSYM IID_IDXGIOutput4}


  // Interface IDXGIFactory4
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIFactory4);'}
  {$EXTERNALSYM IDXGIFactory4}
  IDXGIFactory4 = interface(IDXGIFactory3)
  ['{1bc6ea02-ef36-464f-bf0c-21ca39e5168a}']

    function EnumAdapterByLuid(AdapterLuid: LUID;
                               riid: Tguid;
                               out ppvAdapter {IUnknown} ): HResult; stdcall;

    function EnumWarpAdapter(riid: TGuid;
                             out ppvAdapter {IUnknown} ): HResult; stdcall;

  end;
  IID_IDXGIFactory4 = IDXGIFactory4;
  {$EXTERNALSYM IID_IDXGIFactory4}


  PDXGI_QUERY_VIDEO_MEMORY_INFO = ^DXGI_QUERY_VIDEO_MEMORY_INFO;
  DXGI_QUERY_VIDEO_MEMORY_INFO = record
    Budget: UINT64;
    CurrentUsage: UINT64;
    AvailableForReservation: UINT64;
    CurrentReservation: UINT64;
  end;
  {$EXTERNALSYM DXGI_QUERY_VIDEO_MEMORY_INFO}


  // Interface IDXGIAdapter3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXGIAdapter3);'}
  {$EXTERNALSYM IDXGIAdapter3}
  IDXGIAdapter3 = interface(IDXGIAdapter2)
  ['{645967A4-1392-4310-A798-8053CE3E93FD}']

    function RegisterHardwareContentProtectionTeardownStatusEvent(hEvent: THandle;
                                                                  out pdwCookie: DWORD): HResult; stdcall;

    procedure UnregisterHardwareContentProtectionTeardownStatus(dwCookie: DWORD); stdcall;

    function QueryVideoMemoryInfo(NodeIndex: UINT;
                                  MemorySegmentGroup: DXGI_MEMORY_SEGMENT_GROUP;
                                  out pVideoMemoryInfo: DXGI_QUERY_VIDEO_MEMORY_INFO): HResult; stdcall;

    function SetVideoMemoryReservation(NodeIndex: UINT;
                                       MemorySegmentGroup: DXGI_MEMORY_SEGMENT_GROUP;
                                       Reservation: UINT64): HResult; stdcall;

    function RegisterVideoMemoryBudgetChangeNotificationEvent(hEvent: THandle;
                                                              out pdwCookie: DWORD): HResult; stdcall;

    procedure UnregisterVideoMemoryBudgetChangeNotification(dwCookie: DWORD); stdcall;

  end;
  IID_IDXGIAdapter3 = IDXGIAdapter3;
  {$EXTERNALSYM IID_IDXGIAdapter3}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
