// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.HolographicSpaceInterop.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.6
// Description: This unit allows Windows Mixed Reality apps to use Direct3D 12
//              See: https://learn.microsoft.com/en-us/windows/win32/api/_direct3d12/
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Embarcadero's <= Delphi 10.4 D3D12 is outdated!
//          OS: Windows 10 NTDDI_WIN10_RS2 and above.
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
// Source: HolographicSpaceInterop.h
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

unit WinApi.DirectX.HolographicSpaceInterop;

interface

  {$HPPEMIT '#include "windows.h"'}
  {$HPPEMIT '#include "inspectable.h"'}
  {$HPPEMIT '#include "d3d12.h"'}


uses
  {WinApi}
  WinApi.Windows,
  WinApi.Inspectable,
  WinApi.WinApiTypes,
  {DirectX}
  WinApi.DirectX.D3D12;

  {$MINENUMSIZE 4}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  // Interface IHolographicCameraInterop
  // ====================================
  // Extends HolographicCamera to allow 2D texture resources to be created and used as content buffers for apps using Direct3D 12.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHolographicCameraInterop);'}
  {$EXTERNALSYM IHolographicCameraInterop}
  IHolographicCameraInterop = Interface(IInspectable)
    ['{7cc1f9c5-6d02-41fa-9500-e1809eb48eec}']

    // Creates a D3D12 resource for use as a content buffer for the camera.
    // The D3D12_RESOURCE_DESC struct can contain any set of valid initial values. Any
    // values that will not work with this HolographicCamera will be overridden in the
    // struct indicated by pDesc. This parameter is not optional.
    // The resource is created so that it is already committed to a heap.

    function CreateDirect3D12BackBufferResource(pDevice: ID3D12Device;  {The Direct3D 12 device.}
                                                // The Direct3D 12 resource description.
                                                // The API will make a best effort to respect the values in this descriptor. The app
                                                // should inspect the descriptor for the texture returned by ppCreatedTexture2DResource
                                                // and respond appropriately to any differences from what was specified.
                                                pTexture2DDesc: PD3D12_RESOURCE_DESC;
                                                // If successful, the Direct3D 12 2D texture resource for use as a content buffer. Otherwise null.
                                                out ppCreatedTexture2DResource: PID3D12Resource): HRESULT; stdcall;

    // Creates a D3D12 resource for use as a content buffer for the camera with optional hardware protection.
    // Behavior is the same as CreateDirect3D12BackBufferResource but accepts an optional ID3D12ProtectedResourceSession.
    // Use this optional parameter to to create a hardware protected resource buffer.
    //
    function CreateDirect3D12HardwareProtectedBackBufferResource(// The Direct3D 12 device.
                                                                 pDevice: ID3D12Device;
                                                                 // The Direct3D 12 resource description.
                                                                 // The API will make a best effort to respect the values in this descriptor. The app
                                                                 // should inspect the descriptor for the texture returned by ppCreatedTexture2DResource
                                                                 // and respond appropriately to any differences from what was specified.
                                                                 pTexture2DDesc: D3D12_RESOURCE_DESC;
                                                                 // An optional D3D12 protected resource session. Passing a protected session will attempt to create
                                                                 // A D3D12 hardware protected resource.
                                                                 pProtectedResourceSession: ID3D12ProtectedResourceSession;
                                                                 // If successful, the Direct3D 12 2D texture resource for use as a content buffer. Otherwise null.
                                                                 out ppCreatedTexture2DResource: PID3D12Resource): HRESULT; stdcall;

    // Acquires a Direct3D12 buffer resource.
    // After committing a resource to a HolographicFrame by calling
    // IHolographicCameraRenderingParameters4::CommitDirect3D12Resource, the app should
    // consider control of that resource to be held by the system. That control can last
    // for a few frames as the frame the buffer was committed to makes its way through the
    // presentation queue. To know when the system has relinquished control, call
    // AcquireDirect3D12TextureResource. If the buffer cannot be acquired by the
    // time the app is ready to start rendering a new HolographicFrame, the app should create
    // a new resource and add it to the buffer queue.
    // This method accepts an optional timeout value. When this value is specified, the system
    // will wait for that many milliseconds for the buffer to become available. The default
    // behavior is to not wait.
    // When no timeout value is specified, if this method is called and the buffer is not ready
    // to be acquired, the method call will fail with the error code E_NOTREADY.

    function AcquireDirect3D12BufferResource(// The Direct3D 12 resource to acquire.
                                             // The resource will be in the D3D12_RESOURCE_STATE_COMMON when it is acquired.
                                             pResourceToAcquire: ID3D12Resource;
                                             // The Direct3D 12 command queue to use for transitioning
                                             // the state of this resource when acquiring it for the app.
                                             pCommandQueue: ID3D12CommandQueue): HRESULT; stdcall;

    function AcquireDirect3D12BufferResourceWithTimeout(pResourceToAcquire: ID3D12Resource;
                                                        // The Direct3D 12 command queue to use for transitioning
                                                        // the state of this resource when acquiring it for the app.
                                                        // The resource will be in the D3D12_RESOURCE_STATE_COMMON when it is acquired.
                                                        pCommandQueue: ID3D12CommandQueue;
                                                        // If this parameter is set, the call will wait for that amount of
                                                        // time for the buffer to be acquired.
                                                        // If the timeout period elapses before the buffer can be acquired,
                                                        // the method will fail with the error code E_TIMEOUT.
                                                        // This parameter is in 100-nanosecond units, similar to the
                                                        // Windows.Foundation.TimeSpan Duration property.
                                                        duration: UINT64): HRESULT; stdcall;

    // Un-Acquires a Direct3D12 buffer resource.
    // A resource which has been acquired but not submitted should be unacquired to
    // return control of the buffer back to the app, allowing it to be re-aqcuired later.
    function UnacquireDirect3D12BufferResource(pResourceToUnacquire: ID3D12Resource): HRESULT; stdcall;

  end;
  IID_IHolographicCameraInterop = IHolographicCameraInterop;
  {$EXTERNALSYM IID_IHolographicCameraInterop}


  // Interface IHolographicCameraRenderingParametersInterop
  // =======================================================
  // Extends HolographicCameraRenderingParameters to support setting a content buffer
  // per-frame.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHolographicCameraRenderingParametersInterop);'}
  {$EXTERNALSYM IHolographicCameraRenderingParametersInterop}
  IHolographicCameraRenderingParametersInterop = Interface(IInspectable)
    ['{f75b68d6-d1fd-4707-aafd-fa6f4c0e3bf4}']

    function CommitDirect3D12Resource(// The Direct3D 12 texture resource with content to display when presenting the
                                      // HolographicFrame used to retrieve this object.
                                      pColorResourceToCommit: ID3D12Resource;
                                      // A fence used to signal work completion on pColorResourceToCommit.
                                      pColorResourceFence: ID3D12Fence;
                                      // The value used to signal work completion on the texture resource fence.
                                      colorResourceFenceSignalValue: UINT64): HRESULT; stdcall;

    function CommitDirect3D12ResourceWithDepthData(// The Direct3D 12 texture resource with content to display when presenting the
                                                   // HolographicFrame used to retrieve this object.
                                                   pColorResourceToCommit: ID3D12Resource;
                                                   // A fence used to signal work completion on pColorResourceToCommit.
                                                   pColorResourceFence: ID3D12Fence;
                                                   // The fence value used to signal work completion on the texture resource.
                                                   colorResourceFenceSignalValue: UINT64;
                                                   // The Direct3D 12 depth buffer with depth data for image stabilization.
                                                   // Typically used when rendering to pColorResourceToCommit, or derived from the
                                                   // same rendering pass.
                                                   pDepthResourceToCommit: ID3D12Resource;
                                                   // A fence used to signal work completion on pDepthResourceToCommit.
                                                   pDepthResourceFence: ID3D12Fence;
                                                   // The value used to signal work completion on the depth resource fence.
                                                   depthResourceFenceSignalValue: UINT64): HRESULT; stdcall;

  end;
  IID_IHolographicCameraRenderingParametersInterop = IHolographicCameraRenderingParametersInterop;
  {$EXTERNALSYM IID_IHolographicCameraRenderingParametersInterop}


  // Interface IHolographicQuadLayerInterop
  // =======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHolographicQuadLayerInterop);'}
  {$EXTERNALSYM IHolographicQuadLayerInterop}
  IHolographicQuadLayerInterop = Interface(IInspectable)
    ['{cfa688f0-639e-4a47-83d7-6b7f5ebf7fed}']

    // Creates a D3D12 resource for use as a content buffer for the layer.
    // The D3D12_RESOURCE_DESC struct can contain any set of valid initial values. Any
    // values that will not work with this quad layer object will be overridden in the
    // struct indicated by pDesc. This parameter is not optional.
    // The resource is created so that it is already committed to a heap.
    function CreateDirect3D12ContentBufferResource(// The Direct3D 12 device.
                                                   pDevice: ID3D12Device;
                                                   // The Direct3D 12 resource description.
                                                   // The API will make a best effort to respect the values in this descriptor. The app
                                                   // should inspect the descriptor for the texture returned by ppCreatedTexture2DResource
                                                   // and respond appropriately to any differences from what was specified.
                                                   pTexture2DDesc: D3D12_RESOURCE_DESC;
                                                   // If successful, the Direct3D 12 2D texture resource for use as a content buffer. Otherwise null.
                                                   out ppTexture2DResource: PID3D12Resource): HRESULT; stdcall;

    //  Creates a D3D12 resource for use as a content buffer for the camera with optional hardware protection.
    // Behavior is the same as CreateDirect3D12ContentBufferResource but accepts an optional ID3D12ProtectedResourceSession.
    // Use this optional parameter to to create a hardware protected resource buffer.
    function CreateDirect3D12HardwareProtectedContentBufferResource(// The Direct3D 12 device.
                                                                    pDevice: ID3D12Device;
                                                                    // The Direct3D 12 resource description.
                                                                    // The API will make a best effort to respect the values in this descriptor. The app
                                                                    // should inspect the descriptor for the texture returned by ppCreatedTexture2DResource
                                                                    // and respond appropriately to any differences from what was specified.
                                                                    pTexture2DDesc: D3D12_RESOURCE_DESC;
                                                                    // An optional D3D12 protected resource session. Passing a protected session will attempt to create
                                                                    // A D3D12 hardware protected resource.
                                                                    pProtectedResourceSession: ID3D12ProtectedResourceSession;
                                                                    // If successful, the Direct3D 12 2D texture resource for use as a content buffer. Otherwise null.
                                                                    out ppCreatedTexture2DResource: PID3D12Resource): HRESULT; stdcall;

    // Acquires a Direct3D12 buffer resource.
    // After committing a resource to a HolographicFrame by calling
    // IHolographicCameraRenderingParameters4.CommitDirect3D12Resource, the app should
    // consider control of that resource to be held by the system. That control can last
    // for a few frames as the frame the buffer was committed to makes its way through the
    // presentation queue. To know when the system has relinquished control, call
    // AcquireDirect3D12TextureResource. If the buffer cannot be acquired by the
    // time the app is ready to start rendering a new HolographicFrame, the app should create
    // a new resource and add it to the buffer queue.
    // This method accepts an optional timeout value. When this value is specified, the system
    // will wait for that many milliseconds for the buffer to become available. The default
    // behavior is to not wait.
    // When no timeout value is specified, if this method is called and the buffer is not ready
    // to be acquired, the method call will fail with the error code E_NOTREADY.
    function AcquireDirect3D12BufferResource(// The Direct3D 12 resource to acquire.
                                             // The resource will be in the D3D12_RESOURCE_STATE_COMMON when it is acquired.
                                             pResourceToAcquire: ID3D12Resource;
                                             // The Direct3D 12 command queue to use for transitioning
                                             // the state of this resource when acquiring it for the app.
                                             pCommandQueue: ID3D12CommandQueue): HRESULT; stdcall;

    function AcquireDirect3D12BufferResourceWithTimeout(// The Direct3D 12 resource to acquire.
                                                        // The resource will be in the D3D12_RESOURCE_STATE_COMMON when it is acquired.
                                                        pResourceToAcquire: ID3D12Resource;
                                                        // The Direct3D 12 command queue to use for transitioning
                                                        // the state of this resource when acquiring it for the app.
                                                        pCommandQueue: ID3D12CommandQueue;
                                                        // If this parameter is set, the call will wait for that amount of
                                                        // time for the buffer to be acquired.
                                                        // If the timeout period elapses before the buffer can be acquired,
                                                        // the method will fail with the error code E_TIMEOUT.
                                                        // This parameter is in 100-nanosecond units, similar to the
                                                        // Windows.Foundation.TimeSpan Duration property.
                                                        duration: UINT64): HRESULT; stdcall;

    // Un-Acquires a Direct3D12 buffer resource.
    // A resource which has been acquired but not submitted should be unacquired to
    // return control of the buffer back to the app, allowing it to be re-aqcuired later.
    function UnacquireDirect3D12BufferResource(pResourceToUnacquire: ID3D12Resource): HRESULT; stdcall;

  end;
  IID_IHolographicQuadLayerInterop = IHolographicQuadLayerInterop;
  {$EXTERNALSYM IID_IHolographicQuadLayerInterop}


  // Interface IHolographicQuadLayerUpdateParametersInterop
  // =======================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHolographicQuadLayerUpdateParametersInterop);'}
  {$EXTERNALSYM IHolographicQuadLayerUpdateParametersInterop}
  IHolographicQuadLayerUpdateParametersInterop = Interface(IInspectable)
    ['{e5f549cd-c909-444f-8809-7cc18a9c8920}']

    function CommitDirect3D12Resource(// The Direct3D 12 texture resource with content to display when presenting the
                                      // HolographicFrame used to retrieve this object.
                                      pColorResourceToCommit: ID3D12Resource;
                                      // A fence used to signal work completion on pColorResourceToCommit.
                                      pColorResourceFence: ID3D12Fence;
                                      // The value used to signal work completion on the texture resource fence.
                                      colorResourceFenceSignalValue: UINT64): HRESULT; stdcall;

  end;
  IID_IHolographicQuadLayerUpdateParametersInterop = IHolographicQuadLayerUpdateParametersInterop;
  {$EXTERNALSYM IID_IHolographicQuadLayerUpdateParametersInterop}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
