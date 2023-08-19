// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfD3D12.pas
// Kind: Pascal Unit
// Release date: 19-08-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Provides APIs for synchronizing access to Direct3D resources
//              between Media Foundation producers and consumers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/08/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: mfd3d12.h
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.MediaFoundationApi.MfD3D12;

  {$HPPEMIT '#include "mfd3d12.h"'}
  {$HPPEMIT '#include "Mfidl.h"'}
  {$HPPEMIT '#include "d3d12.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl,
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

  // Interface IMFD3D12SynchronizationObjectCommands
  // ===============================================
  //
  // Synchronization object commands allow a producer or a consumer of D3D12 resource to signal down-stream
  // components when the resource is ready for use or can be released.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFD3D12SynchronizationObjectCommands);'}
  {$EXTERNALSYM IMFD3D12SynchronizationObjectCommands}
  IMFD3D12SynchronizationObjectCommands = interface(IUnknown)
    ['{09D0F835-92FF-4E53-8EFA-40FAA551F233}']

    function EnqueueResourceReady(pProducerCommandQueue: ID3D12CommandQueue): HResult; stdcall;
    // Queues a fence on the specified producer command queue that will signal to a downstream consumer
    // when the associated D3D12 resource is ready to be used. This method also signals that the resource is
    // no longer in use and has been released by the producer.
    //
    // param name = "pProducerCommandQueue"
    //  Pointer to the producer command queue into which the fence should be inserted.

    function EnqueueResourceReadyWait(pConsumerCommandQueue: ID3D12CommandQueue): HResult; stdcall;
    // Queues a wait command on the specified consumer command queue, starting a wait for the
    // Resource Ready signal from the producer command queue.  This function allows the consumer
    // to immediately start scheduling commands its GPU engine.  The wait will ensure
    // that the commands scheduled after the wait are not executed until the corresponding
    // Ready Signal is fired by the producer GPU engine.
    //
    // param name = "pCommandQueue"
    //  Pointer to the consumer command queue onto which the wait command should be queued.

    function SignalEventOnResourceReady(hEvent: THandle): HResult; stdcall;
    // Stores an event handle that will be set when the D3D12 resource is
    // ready. This event can be used by a CPU thread to wait until the resource
    // producer GPU tasks have finished executing, and the producer fires the Resource Ready
    // signal. If the event handle has restricted access rights, the handle must have at least
    // the EVENT_MODIFY_STATE right.
    //
    // param name = "hEvent"
    //  Handle to the event that will be set when the resource is ready.

    function EnqueueResourceRelease(pConsumerCommandQueue: ID3D12CommandQueue): HResult; stdcall;
    // Queues a fence into the specified command queue that will signal to the synchronization
    // object when GPU is finished processing the consumer commands.  This method signals when
    // the resource is no longer in use and has been released by the consumer.
    //
    // param name = "pCommandQueue"
    //  Pointer to the consumer command queue onto which the fence should be queued.

  end;
  IID_IMFD3D12SynchronizationObjectCommands = IMFD3D12SynchronizationObjectCommands;
  {$EXTERNALSYM IID_IMFD3D12SynchronizationObjectCommands}


  // Interface IMFD3D12SynchronizationObject
  // =======================================
  // The synchronization object interface allows a D3D12 resource allocator to
  // manage the lifetime of a D3D12 resource.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFD3D12SynchronizationObject);'}
  {$EXTERNALSYM IMFD3D12SynchronizationObject}
  IMFD3D12SynchronizationObject = interface(IUnknown)
    ['{802302B0-82DE-45E1-B421-F19EE5BDAF23}']

    function SignalEventOnFinalResourceRelease(hEvent: THandle): HResult; stdcall;
    // Stores an event handle that will be set when the resource is free and can be recycled,
    // reused, or destroyed.  The handle is signaled when there are no longer any pending
    // Resource Release or Resource Ready signals for the current resource.  If the event handle
    // has restricted access rights, the handle must have at least the EVENT_MODIFY_STATE right.
    //
    // param name = "hEvent"
    //  Handle to the event that will be set when the resource is freed.

    function Reset(): HResult; stdcall;
    // Resets the synchronization object state, allowing the allocator to reuse the resource and
    // this corresponding synchronization object.

  end;
  IID_IMFD3D12SynchronizationObject = IMFD3D12SynchronizationObject;
  {$EXTERNALSYM IID_IMFD3D12SynchronizationObject}


  // (NTDDI_VERSION >= NTDDI_WIN10_CO)

  function MFCreateD3D12SynchronizationObject(pDevice: ID3D12Device;
                                              const riid: REFIID;
                                              out ppvSyncObject: Pointer): HResult; stdcall;
  {$EXTERNALSYM MFCreateD3D12SynchronizationObject}
  // This function is used to instantiate an MF D3D12 synchronization primitive used to synchronize
  // access to a D3D12 resource stored in an MF object.
  // param name = "pDevice"
  //  The D3D12 device that corresponds to the resource and primitive being created.
  //
  // param name = "riid"
  //  The GUID identifying the interface of the synchronization object that will be created.
  //
  // param name = "ppvSyncObject"
  //  Specifies a pointer to the pointer to the synchronization object that will be created.

  // End (NTDDI_VERSION >= NTDDI_WIN10_CO)


type
  //
  // This enumeration indicates the D3D version of the resource used in the stream associated with a media type.
  //
  PMF_MT_D3D_RESOURCE_VERSION_ENUM = ^MF_MT_D3D_RESOURCE_VERSION_ENUM;
  MF_MT_D3D_RESOURCE_VERSION_ENUM = (
    MF_D3D11_RESOURCE,
    MF_D3D12_RESOURCE
  );
  {$EXTERNALSYM MF_MT_D3D_RESOURCE_VERSION_ENUM}

const

  //
  // MF D3D12 synchronization attributes
  //

  // MF_D3D12_SYNCHRONIZATION_OBJECT
  // Data type: IUnknown
  // This identifier is used with IMFDXGIBuffer::SetUnknown and IMFDXGIBuffer::GetUnknown
  // to access a D3D12 MF synchronization object.
  // {2a7c8d6a-85a6-494d-a046-06-ea-1a-13-8f-4b}
  MF_D3D12_SYNCHRONIZATION_OBJECT : TGUID = (D1: $2a7c8d6a;
                                             D2: $85a6;
                                             D3: $494d;
                                             D4: ($a0, $46, $60, $ea, $1a, $13, $8f, $4b));

  //
  //
  //
  // MF D3D12 media type attributes.
  //
  //

  // MF_MT_D3D_RESOURCE_VERSION
  // Data type: UINT32 (treat as member of the MF_MT_D3D_RESOURCE_VERSION_ENUM enumeration)
  // This attribute specifies the D3D version of the resources stored in the data stream associated with the media type.  The
  // value of this attribute corresponds a value from the MF_MT_D3D_RESOURCE_VERSION_ENUM enumeration.
  // {174f1e85-fe26-453d-b52e-5b-dd-4e-55-b9-44}
  MF_MT_D3D_RESOURCE_VERSION : TGUID = (D1: $174f1e85;
                                        D2: $fe26;
                                        D3: $453d;
                                        D4: ($b5, $2e, $5b, $dd, $4e, $55, $b9, $44));


  // MF_MT_D3D12_CPU_READBACK
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether CPU access is required for the D3D12 resources.  Default value is 0 (FALSE).
  // {28ee9fe3-d481-46a6-b98a-7f-69-d5-28-0e-82}
  MF_MT_D3D12_CPU_READBACK : TGUID = (D1: $28ee9fe3;
                                      D2: $d481;
                                      D3: $46a6;
                                      D4: ($b9, $8a, $7f, $69, $d5, $28, $e, $82));

  // MF_MT_D3D12_TEXTURE_LAYOUT
  // Data type: UINT32
  // This attribute indicates the texture layout options that were used to create the resources.  The value of this attribute
  // corresponds to values in the D3D12_TEXTURE_LAYOUT enumeration.
  // {97c85caa-beb-4ee1-9715-f2-2f-ad-8c-10-f5}
  MF_MT_D3D12_TEXTURE_LAYOUT : TGUID = (D1: $97c85caa;
                                        D2: $beb;
                                        D3: $4ee1;
                                        D4: ($97, $15, $f2, $2f, $ad, $8c, $10, $f5));

  // MF_MT_D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether render target view can be created for the resources in the stream associated
  // with the media type.  The value of this attribute corresponds to the D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET value from
  // the D3D12_RESOURCE_FLAGS enumeration.
  // {eeac2585-3430-498c-84a2-77-b1-bb-a5-70-f6}
  MF_MT_D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET : TGUID = (D1: $eeac2585;
                                                         D2: $3430;
                                                         D3: $498c;
                                                        D4: ($84, $a2, $77, $b1, $bb, $a5, $70, $f6));

  // MF_MT_D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether depth stencil view can be created for the resources in the stream associated
  // with the media type.  The value of this attribute corresponds to the D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL value
  // from the D3D12_RESOURCE_FLAGS enumeration.
  // {b1138dc3-01d5-4c14-9bdc-cd-c9-33-6f-55-b9}
  MF_MT_D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL : TGUID = (D1: $b1138dc3;
                                                           D2: $1d5;
                                                           D3: $4c14;
                                                           D4: ($9b, $dc, $cd, $c9, $33, $6f, $55, $b9));

  // MF_MT_D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether unordered access view can be created for the resources in the stream associated
  // with the media type.  The value of this attribute corresponds to the D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS value from
  // the D3D12_RESOURCE_FLAGS enumeration.
  // {82c85647-5057-4960-9559-f4-5b-8e-27-14-27}
  MF_MT_D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS : TGUID = (D1: $82c85647;
                                                              D2: $5057;
                                                              D3: $4960;
                                                              D4: ($95, $59, $f4, $5b, $8e, $27, $14, $27));

  // MF_MT_D3D12_RESOURCE_FLAG_DENY_SHADER_RESOURCE
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether shader resource view creation is disallowed for the resources in the stream associated
  // with the media type.  The value of this attribute corresponds to the D3D12_RESOURCE_FLAG_DENY_SHADER_RESOURCE value from
  // the D3D12_RESOURCE_FLAGS enumeration.
  // {ba06bfac-ffe3-474a-ab55-16-1e-e4-41-7a-2e}
  MF_MT_D3D12_RESOURCE_FLAG_DENY_SHADER_RESOURCE : TGUID = (D1: $ba06bfac;
                                                            D2: $ffe3;
                                                            D3: $474a;
                                                            D4: ($ab, $55, $16, $1e, $e4, $41, $7a, $2e));

  // MF_MT_D3D12_RESOURCE_FLAG_ALLOW_CROSS_ADAPTER
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether the resources in the stream can be used for cross-adapter data.  The value of this
  // attribute corresponds to the D3D12_RESOURCE_FLAG_ALLOW_CROSS_ADAPTER value from the D3D12_RESOURCE_FLAGS enumeration.
  // {a6a1e439-2f96-4ab5-98dc-ad-f7-49-73-50-5d}
  MF_MT_D3D12_RESOURCE_FLAG_ALLOW_CROSS_ADAPTER : TGUID = (D1: $a6a1e439;
                                                           D2: $2f96;
                                                           D3: $4ab5;
                                                           D4: ($98, $dc, $ad, $f7, $49, $73, $50, $5d));

  // MF_MT_D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS
  // Data type: UINT32 (treat as Boolean)
  // This attribute indicates whether the resources in the stream can be simultaneously accessed by multiple different
  // command queues.  The value of this attribute corresponds to the D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS value from the
  // D3D12_RESOURCE_FLAGS enumeration.
  // {a4940b2-cfd6-4738-9d02-98-11-37-34-01-5a}
  MF_MT_D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS : TGUID = (D1: $a4940b2;
                                                                 D2: $cfd6;
                                                                 D3: $4738;
                                                                 D4: ($9d, $2, $98, $11, $37, $34, $1, $5a));

  // MF_SA_D3D12_HEAP_FLAGS
  // Data type: UINT32
  // This attribute contains the value with the heap options used for the D3D12 resources in the stream.  The attribute
  // contains a bitwise-OR’d combination of D3D12_HEAP_FLAGS enumeration values.
  // {496b3266-d28f-4f8c-93a7-4a-59-6b-1a-31-a1}
  MF_SA_D3D12_HEAP_FLAGS : TGUID = (D1: $496b3266;
                                    D2: $d28f;
                                    D3: $4f8c;
                                    D4: ($93, $a7, $4a, $59, $6b, $1a, $31, $a1));

  // MF_SA_D3D12_HEAP_TYPE
  // Data type: UINT32
  // This attribute contains the value specifying the type of heap used for the D3D12 resources in the stream.  The value of
  // this attribute corresponds to a value from the D3D12_HEAP_TYPE enumeration.
  // {56f26a76-bbc1-4ce0-bb11-e2-23-68-d8-74-ed}
  MF_SA_D3D12_HEAP_TYPE : TGUID = (D1: $56f26a76;
                                   D2: $bbc1;
                                   D3: $4ce0;
                                   D4: ($bb, $11, $e2, $23, $68, $d8, $74, $ed));

  // MF_SA_D3D12_CLEAR_VALUE
  // Data type: BLOB
  // This attribute contains a blob with the information used to optimize clear operations for the D3D12 resources
  // in the stream.  The blob contains an instance of the D3D12_CLEAR_VALUE structure.
  // {86ba9a39-0526-495d-9ab5-54-ec-9f-ad-6f-c3}
  MF_SA_D3D12_CLEAR_VALUE : TGUID = (D1: $86ba9a39;
                                     D2: $526;
                                     D3: $495d;
                                     D4: ($9a, $b5, $54, $ec, $9f, $ad, $6f, $c3));


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

const
  d3d12Lib = 'd3d12.dll';


{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateD3D12SynchronizationObject; external d3d12Lib name 'MFCreateD3D12SynchronizationObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};


end.
