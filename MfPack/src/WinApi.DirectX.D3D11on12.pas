// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D11on12.pas
// Kind: Pascal Unit
// Release date: 27-08-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: D3D11On12 is a mapping layer, which maps graphics commands from D3D11 to D3D12.
//              D3D11On12 is not an implementation of the D3D11 API,
//              but is instead an implementation of the D3D11 usermode DDI (device driver interface).
//              That means it is not a binary named d3d11.dll, but is named d3d11on12.dll.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: d3d11on12.h
//
// Copyright (c) Microsoft Corporation. Licensed under the MIT license.
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
unit WinApi.DirectX.D3D11on12;

interface

  {$HPPEMIT '#include "windows.h"'}
  {$HPPEMIT '#include "d3d11.h"'}
  {$HPPEMIT '#include "d3d12.h"'}
  {$HPPEMIT '#include "d3dcommon.h"'}

uses
  {WinApi}
  Winapi.Windows,
  {DirectX}
  Winapi.DirectX.D3DCommon,
  Winapi.DirectX.D3D11,
  Winapi.DirectX.D3D12;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  //////////////////////////////////////////////////////////////////////////////
  // D3D11On12CreateDevice
  // ------------------
  //
  // pDevice
  //      Specifies a pre-existing D3D12 device to use for D3D11 interop.
  //      May not be nil.
  // Flags
  //      Any of those documented for D3D11CreateDeviceAndSwapChain.
  // pFeatureLevels
  //      Array of any of the following:
  //          D3D_FEATURE_LEVEL_12_1
  //          D3D_FEATURE_LEVEL_12_0
  //          D3D_FEATURE_LEVEL_11_1
  //          D3D_FEATURE_LEVEL_11_0
  //          D3D_FEATURE_LEVEL_10_1
  //          D3D_FEATURE_LEVEL_10_0
  //          D3D_FEATURE_LEVEL_9_3
  //          D3D_FEATURE_LEVEL_9_2
  //          D3D_FEATURE_LEVEL_9_1
  //       The first feature level which is less than or equal to the
  //       D3D12 device's feature level will be used to perform D3D11 validation.
  //       Creation will fail if no acceptable feature levels are provided.
  //       Providing NULL will default to the D3D12 device's feature level.
  // FeatureLevels
  //      Size of feature levels array.
  // ppCommandQueues
  //      Array of unique queues for D3D11On12 to use. Valid queue types:
  //          3D command queue.
  //      Flags must be compatible with device flags, and its NodeMask must
  //      be a subset of the NodeMask provided to this API.
  // NumQueues
  //      Size of command queue array.
  // NodeMask
  //      Which node of the D3D12 device to use.  Only 1 bit may be set.
  // ppDevice
  //      Pointer to returned interface. May be nil.
  // ppImmediateContext
  //      Pointer to returned interface. May be nil.
  // pChosenFeatureLevel
  //      Pointer to returned feature level. May be nil.
  //
  // Return Values
  //  Any of those documented for
  //          D3D11CreateDevice
  //
  //////////////////////////////////////////////////////////////////////////////

  function D3D11On12CreateDevice(pDevice: IUnknown;
                                 Flags: UINT;
                                 const pFeatureLevels: PD3D_FEATURE_LEVEL;
                                 FeatureLevels: UINT;
                                 const [Ref] ppCommandQueues: IUnknown;
                                 NumQueues: UINT;
                                 NodeMask: UINT;
                                 [Ref] ppDevice: ID3D11Device;
                                 [Ref] ppImmediateContext: ID3D11DeviceContext;
                                 out pChosenFeatureLevel: D3D_FEATURE_LEVEL): HRESULT; stdcall;
  {$EXTERNALSYM D3D11On12CreateDevice}


type
  // Forward Declarations
  ID3D11On12Device = interface;

  PD3D11_RESOURCE_FLAGS = ^D3D11_RESOURCE_FLAGS;
  D3D11_RESOURCE_FLAGS = record
    BindFlags: UINT;
    MiscFlags: UINT;
    CPUAccessFlags: UINT;
    StructureByteStride: UINT;
  end;
  {$EXTERNALSYM D3D11_RESOURCE_FLAGS}


  // Interface ID3D11On12Device
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D11On12Device);'}
  {$EXTERNALSYM ID3D11On12Device}
  ID3D11On12Device = interface(IUnknown)
    ['{85611E73-70A9-490E-9614-A9E302777904}']

    function CreateWrappedResource(pResource12: IUnknown;
                                   const pFlags11: PD3D11_RESOURCE_FLAGS;
                                   InState: D3D12_RESOURCE_STATES;
                                   OutState: D3D12_RESOURCE_STATES;
                                   const riid: TGUID;
                                   ppResource11: PPointer): HRESULT; stdcall;

    procedure ReleaseWrappedResources([Ref] const ppResources: ID3D11Resource;
                                      NumResources: UINT); stdcall;

    procedure AcquireWrappedResources([Ref] const ppResources: ID3D11Resource;
                                      NumResources: UINT); stdcall;

  end;
  IID_ID3D11On12Device = ID3D11On12Device;
  {$EXTERNALSYM IID_ID3D11On12Device}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.


const
  D3D11on12lib = 'd3d11on12.dll';

{$WARN SYMBOL_PLATFORM OFF}

function D3D11On12CreateDevice; external D3D11on12lib name 'D3D11On12CreateDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

end.

