// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D3D12Compatibility.pas
// Kind: Pascal Unit
// Release date: 27-08-2022
// Language: ENU
//
// Revision Version: 4.1.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D12Compatibility is outdated!
//          Search "Update May 2024", to find updates until May 2024.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: d3d12compatibility.h
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
unit WinApi.DirectX.D3D12Compatibility;

interface

  {$HPPEMIT '#include "windows.h"'}
  {$HPPEMIT '#include "d3d12.h"'}
  {$HPPEMIT '#include "d3d11on12.h"'}


uses
  {WinApi}
  Winapi.Windows,
  WinApi.WinApiTypes,
  {D3D12}
  Winapi.DirectX.D3D12,
  WinApi.DirectX.D3D11on12;

  {$MINENUMSIZE 4}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


type
  PD3D12_COMPATIBILITY_SHARED_FLAGS = ^D3D12_COMPATIBILITY_SHARED_FLAGS;
  D3D12_COMPATIBILITY_SHARED_FLAGS = UINT;
  {$EXTERNALSYM D3D12_COMPATIBILITY_SHARED_FLAGS}
const
    D3D12_COMPATIBILITY_SHARED_FLAG_NONE          = 0;
    D3D12_COMPATIBILITY_SHARED_FLAG_NON_NT_HANDLE = $1;
    D3D12_COMPATIBILITY_SHARED_FLAG_KEYED_MUTEX   = $2;
    D3D12_COMPATIBILITY_SHARED_FLAG_9_ON_12       = $4;


type

  PD3D12_REFLECT_SHARED_PROPERTY = ^D3D12_REFLECT_SHARED_PROPERTY;
  D3D12_REFLECT_SHARED_PROPERTY = (D3D12_REFLECT_SHARED_PROPERTY_D3D11_RESOURCE_FLAGS,        // D3D11_RESOURCE_FLAGS
                                   D3D12_REFELCT_SHARED_PROPERTY_COMPATIBILITY_SHARED_FLAGS,  // D3D12_COMPATIBILITY_SHARED_FLAGS
                                   D3D12_REFLECT_SHARED_PROPERTY_NON_NT_SHARED_HANDLE         // HANDLE
                                  );
  {$EXTERNALSYM D3D12_REFLECT_SHARED_PROPERTY}


  // Interface ID3D12CompatibilityDevice
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12CompatibilityDevice);'}
  {$EXTERNALSYM ID3D12CompatibilityDevice}
  ID3D12CompatibilityDevice = interface(IUnknown)
    ['{8F1C0E3C-FAE3-4A82-B098-BFE1708207FF}']

    function CreateSharedResource(pHeapProperties: PD3D12_HEAP_PROPERTIES;
                                  HeapFlags: D3D12_HEAP_FLAGS;
                                  pDesc: PD3D12_RESOURCE_DESC;
                                  InitialResourceState: D3D12_RESOURCE_STATES;
                                  pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                  pFlags11: PD3D11_RESOURCE_FLAGS;
                                  CompatibilityFlags: D3D12_COMPATIBILITY_SHARED_FLAGS;
                                  pLifetimeTracker: ID3D12LifetimeTracker;
                                  pOwningSwapchain: ID3D12SwapChainAssistant;
                                  const riid: TGUID;
                                  ppResource: PPointer): HRESULT; stdcall;

    function CreateSharedHeap(pHeapDesc: PD3D12_HEAP_DESC;
                              CompatibilityFlags: D3D12_COMPATIBILITY_SHARED_FLAGS;
                              const riid: TGUID;
                              ppHeap: PPointer): HRESULT; stdcall;

    function ReflectSharedProperties(pHeapOrResource: ID3D12Object;
                                     ReflectType: D3D12_REFLECT_SHARED_PROPERTY;
                                     pData: Pointer;
                                     DataSize: UINT): HRESULT; stdcall;
  end;
  IID_ID3D12CompatibilityDevice = ID3D12CompatibilityDevice;
  {$EXTERNALSYM IID_ID3D12CompatibilityDevice}


  // Interface D3D11On12CreatorID
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(D3D11On12CreatorID);'}
  {$EXTERNALSYM D3D11On12CreatorID}
  D3D11On12CreatorID = interface(IUnknown)
    ['{EDBF5678-2960-4E81-8429-99D4B2630C4E}']
  end;
  IID_D3D11On12CreatorID = D3D11On12CreatorID;
  {$EXTERNALSYM IID_D3D11On12CreatorID}


  // Interface D3D9On12CreatorID
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(D3D9On12CreatorID);'}
  {$EXTERNALSYM D3D9On12CreatorID}
  D3D9On12CreatorID = interface(IUnknown)
    ['{FFFCBB7F-15D3-42A2-841E-9D8D32F37DDD}']
  end;
  IID_D3D9On12CreatorID = D3D9On12CreatorID;
  {$EXTERNALSYM IID_D3D9On12CreatorID}


  // Interface OpenGLOn12CreatorID
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(OpenGLOn12CreatorID);'}
  {$EXTERNALSYM OpenGLOn12CreatorID}
  OpenGLOn12CreatorID = interface(IUnknown)
    ['{6BB3CD34-0D19-45AB-97ED-D720BA3DFC80}']
  end;
  IID_OpenGLOn12CreatorID = OpenGLOn12CreatorID;
  {$EXTERNALSYM IID_OpenGLOn12CreatorID}


  // Interface OpenCLOn12CreatorID
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(OpenCLOn12CreatorID);'}
  {$EXTERNALSYM OpenCLOn12CreatorID}
  OpenCLOn12CreatorID = interface(IUnknown)
    ['{3F76BB74-91B5-4A88-B126-20CA0331CD60}']
  end;
  IID_OpenCLOn12CreatorID = OpenCLOn12CreatorID;
  {$EXTERNALSYM IID_OpenCLOn12CreatorID}

  // Update May 2024

  // Interface VulkanOn12CreatorID
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(VulkanOn12CreatorID);'}
  {$EXTERNALSYM VulkanOn12CreatorID}
  VulkanOn12CreatorID = interface(IUnknown)
    ['{BC806E01-3052-406C-A3E8-9FC07F048F98}']
  end;
  IID_VulkanOn12CreatorID = VulkanOn12CreatorID;
  {$EXTERNALSYM IID_VulkanOn12CreatorID}

  // ==

  // Interface DirectMLTensorFlowCreatorID
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(DirectMLTensorFlowCreatorID);'}
  {$EXTERNALSYM DirectMLTensorFlowCreatorID}
  DirectMLTensorFlowCreatorID = interface(IUnknown)
    ['{CB7490AC-8A0F-44EC-9B7B-6F4CAFE8E9AB}']
  end;
  IID_DirectMLTensorFlowCreatorID = DirectMLTensorFlowCreatorID;
  {$EXTERNALSYM IID_DirectMLTensorFlowCreatorID}


  // Interface DirectMLPyTorchCreatorID
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(DirectMLPyTorchCreatorID);'}
  {$EXTERNALSYM DirectMLPyTorchCreatorID}
  DirectMLPyTorchCreatorID = interface(IUnknown)
    ['{AF029192-FBA1-4B05-9116-235E06560354}']
  end;
  IID_DirectMLPyTorchCreatorID = DirectMLPyTorchCreatorID;
  {$EXTERNALSYM IID_DirectMLPyTorchCreatorID}

  { 10.0.28000.0  }
  // Interface DirectMLWebNNCreatorID
  // =================================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(DirectMLWebNNCreatorID);'}
  {$EXTERNALSYM DirectMLWebNNCreatorID}
  DirectMLWebNNCreatorID = interface(IUnknown)
    ['{FDF01A76-1E11-450F-902B-74F04EA08094}']
  end;
  IID_DirectMLWebNNCreatorID = DirectMLWebNNCreatorID;
  {$EXTERNALSYM IID_DirectMLWebNNCreatorID}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
