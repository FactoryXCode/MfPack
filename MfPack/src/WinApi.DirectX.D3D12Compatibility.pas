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
// Revision Version: 3.1.7
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
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D12 is outdated!
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
  D3D12_REFLECT_SHARED_PROPERTY = (
    D3D12_REFLECT_SHARED_PROPERTY_D3D11_RESOURCE_FLAGS,        // D3D11_RESOURCE_FLAGS
    D3D12_REFELCT_SHARED_PROPERTY_COMPATIBILITY_SHARED_FLAGS,  // D3D12_COMPATIBILITY_SHARED_FLAGS
    D3D12_REFLECT_SHARED_PROPERTY_NON_NT_SHARED_HANDLE         // HANDLE
  );
  {$EXTERNALSYM D3D12_REFLECT_SHARED_PROPERTY}


  // Interface ID3D12CompatibilityDevice
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3D12CompatibilityDevice);'}
  {$EXTERNALSYM ID3D12CompatibilityDevice}
  ID3D12CompatibilityDevice = Interface(IUnknown)
    ['{8f1c0e3c-fae3-4a82-b098-bfe1708207ff}']

    function CreateSharedResource(pHeapProperties: D3D12_HEAP_PROPERTIES;
                                  HeapFlags: D3D12_HEAP_FLAGS;
                                  pDesc: D3D12_RESOURCE_DESC;
                                  InitialResourceState: D3D12_RESOURCE_STATES;
                                  pOptimizedClearValue: PD3D12_CLEAR_VALUE;
                                  pFlags11: PD3D11_RESOURCE_FLAGS;
                                  CompatibilityFlags: D3D12_COMPATIBILITY_SHARED_FLAGS;
                                  pLifetimeTracker: ID3D12LifetimeTracker;
                                  pOwningSwapchain: ID3D12SwapChainAssistant;
                                  const riid: REFIID;
                                  out ppResource): HRESULT; stdcall;

    function CreateSharedHeap(pHeapDesc: D3D12_HEAP_DESC;
                              CompatibilityFlags: D3D12_COMPATIBILITY_SHARED_FLAGS;
                              const riid: REFIID;
                              out ppHeap): HRESULT; stdcall;

    function ReflectSharedProperties(pHeapOrResource: ID3D12Object;
                                     ReflectType: D3D12_REFLECT_SHARED_PROPERTY;
                                     out pData: Pointer;
                                     DataSize: UINT): HRESULT; stdcall;

  end;
  IID_ID3D12CompatibilityDevice = ID3D12CompatibilityDevice;
  {$EXTERNALSYM IID_ID3D12CompatibilityDevice}


  // Interface D3D11On12CreatorID
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(D3D11On12CreatorID);'}
  {$EXTERNALSYM D3D11On12CreatorID}
  D3D11On12CreatorID = Interface(IUnknown)
    ['{edbf5678-2960-4e81-8429-99d4b2630c4e}']

  end;
  IID_D3D11On12CreatorID = D3D11On12CreatorID;
  {$EXTERNALSYM IID_D3D11On12CreatorID}


  // Interface D3D9On12CreatorID
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(D3D9On12CreatorID);'}
  {$EXTERNALSYM D3D9On12CreatorID}
  D3D9On12CreatorID = Interface(IUnknown)
    ['{fffcbb7f-15d3-42a2-841e-9d8d32f37ddd}']

  end;
  IID_D3D9On12CreatorID = D3D9On12CreatorID;
  {$EXTERNALSYM IID_D3D9On12CreatorID}


  // Interface OpenGLOn12CreatorID
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(OpenGLOn12CreatorID);'}
  {$EXTERNALSYM OpenGLOn12CreatorID}
  OpenGLOn12CreatorID = Interface(IUnknown)
    ['{6bb3cd34-0d19-45ab-97ed-d720ba3dfc80}']

  end;
  IID_OpenGLOn12CreatorID = OpenGLOn12CreatorID;
  {$EXTERNALSYM IID_OpenGLOn12CreatorID}


  // Interface OpenCLOn12CreatorID
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(OpenCLOn12CreatorID);'}
  {$EXTERNALSYM OpenCLOn12CreatorID}
  OpenCLOn12CreatorID = Interface(IUnknown)
    ['{3f76bb74-91b5-4a88-b126-20ca0331cd60}']

  end;
  IID_OpenCLOn12CreatorID = OpenCLOn12CreatorID;
  {$EXTERNALSYM IID_OpenCLOn12CreatorID}



  // Interface DirectMLTensorFlowCreatorID
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(DirectMLTensorFlowCreatorID);'}
  {$EXTERNALSYM DirectMLTensorFlowCreatorID}
  DirectMLTensorFlowCreatorID = Interface(IUnknown)
    ['{cb7490ac-8a0f-44ec-9b7b-6f4cafe8e9ab}']

  end;
  IID_DirectMLTensorFlowCreatorID = DirectMLTensorFlowCreatorID;
  {$EXTERNALSYM IID_DirectMLTensorFlowCreatorID}


  // Interface DirectMLPyTorchCreatorID
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(DirectMLPyTorchCreatorID);'}
  {$EXTERNALSYM DirectMLPyTorchCreatorID}
  DirectMLPyTorchCreatorID = Interface(IUnknown)
    ['{af029192-fba1-4b05-9116-235e06560354}']

  end;
  IID_DirectMLPyTorchCreatorID = DirectMLPyTorchCreatorID;
  {$EXTERNALSYM IID_DirectMLPyTorchCreatorID}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
