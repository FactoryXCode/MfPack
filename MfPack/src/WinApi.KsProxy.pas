// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.KsProxy.pas
// Kind: Pascal / Delphi unit
// Release date: 02-06-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Interface definitions for WDM-CSA proxy filters.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
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
// =============================================================================
// Source: ksproxy.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// =============================================================================
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
// =============================================================================
unit WinApi.KsProxy;

  {$HPPEMIT '#include "ksproxy.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Ks,
  WinApi.StrMif;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

const

  STATIC_IID_IKsObject              : TGUID = (D1: $423c13a2;
                                               D2: $2070;
                                               D3: $11d0;
                                               D4: ($9e, $f7, $00, $aa, $00, $a2, $16, $a1));
  {$EXTERNALSYM STATIC_IID_IKsObject}

  STATIC_IID_IKsPinEx               : TGUID = (D1: $7bb38260;
                                               D2: $d19c;
                                               D3: $11d2;
                                               D4: ($b3, $8a, $00, $a0, $c9, $5e, $c2, $2e));
  {$EXTERNALSYM STATIC_IID_IKsPinEx}

  STATIC_IID_IKsPin                 :	TGUID = (D1: $b61178d1;
                                               D2: $a2d9;
                                               D3: $11cf;
                                               D4: ($9e, $53, $00, $aa, $00, $a2, $16, $a1));
   {$EXTERNALSYM STATIC_IID_IKsPin}

  STATIC_IID_IKsPinPipe             : TGUID = (D1: $e539cd90;
                                               D2: $a8b4;
                                               D3: $11d1;
                                               D4: ($81, $89, $00, $a0, $c9, $06, $28, $02));
   {$EXTERNALSYM STATIC_IID_IKsPinPipe}

  STATIC_IID_IKsDataTypeHandler     :	TGUID = (D1: $5ffbaa02;
                                               D2: $49a3;
                                               D3: $11d0;
                                               D4: ($9f, $36, $00, $aa, $00, $a2, $16, $a1));
   {$EXTERNALSYM STATIC_IID_IKsDataTypeHandler}

  STATIC_IID_IKsDataTypeCompletion  : TGUID = (D1: $827D1A0E;
                                               D2: $0F73;
                                               D3: $11D2;
                                               D4: ($B2, $7A, $00, $A0, $C9, $22, $31, $96));
   {$EXTERNALSYM STATIC_IID_IKsDataTypeCompletion}

  STATIC_IID_IKsInterfaceHandler    :	TGUID = (D1: $D3ABC7E0;
                                               D2: $9A61;
                                               D3: $11D0;
                                               D4: ($A4, $0D, $00, $A0, $C9, $22, $31, $96));
   {$EXTERNALSYM STATIC_IID_IKsInterfaceHandler}

  STATIC_IID_IKsClockPropertySet    : TGUID = (D1: $5C5CBD84;
                                               D2: $E755;
                                               D3: $11D0;
                                               D4: ($AC, $18, $00, $A0, $C9, $22, $31, $96));
   {$EXTERNALSYM STATIC_IID_IKsClockPropertySet}

  STATIC_IID_IKsAllocator           : TGUID = (D1: $8da64899;
                                               D2: $c0d9;
                                               D3: $11d0;
                                               D4: ($84, $13, $00, $00, $f8, $22, $fe, $8a));
  {$EXTERNALSYM STATIC_IID_IKsAllocator}

  STATIC_IID_IKsAllocatorEx         : TGUID = (D1: $091bb63a;
                                               D2: $603f;
                                               D3: $11d1;
                                               D4: ($b0, $67, $00, $a0, $c9, $06, $28, $02));
  {$EXTERNALSYM STATIC_IID_IKsAllocatorEx}

  STATIC_IID_IKsTopology            : TGUID = (D1: $28F54683;
                                               D2: $06FD;
                                               D3: $11D2;
                                               D4: ($B2, $7A, $00, $A0, $C9, $22, $31, $96));
  {$EXTERNALSYM STATIC_IID_IKsTopology}

  STATIC_IID_IKsAggregateControl    : TGUID = (D1: $7F40EAC0;
                                               D2: $3947;
                                               D3: $11D2;
                                               D4: ($87, $4E, $00, $A0, $C9, $22, $31, $96));
   {$EXTERNALSYM STATIC_IID_IKsAggregateControl}

  CLSID_Proxy                       : TGUID = (D1: $17CCA71B;
                                               D2: $ECD7;
                                               D3: $11D0;
                                               D4: ($B9, $08, $00, $A0, $C9, $22, $31, $96));
  {$EXTERNALSYM CLSID_Proxy}

  STATIC_IID_IKsQualityForwarder    : TGUID = (D1: $97EBAACB;
                                               D2: $95BD;
                                               D3: $11D0;
                                               D4: ($A3, $EA, $00, $A0, $C9, $22, $31, $96));
  {$EXTERNALSYM STATIC_IID_IKsQualityForwarder}

  STATIC_IID_IKsPropertySet         :	TGUID = (D1: $31EFAC30;
                                               D2: $515C;
                                               D3: $11d0;
                                               D4: ($A9, $AA, $00, $AA, $00, $61, $BE, $93));
  {$EXTERNALSYM STATIC_IID_IKsPropertySet}

  STATIC_IID_IKsControl             :	TGUID = (D1: $28F54685;
                                               D2: $06FD;
                                               D3: $11D2;
                                               D4: ($B2, $7A, $00, $A0, $C9, $22, $31, $96));
  {$EXTERNALSYM STATIC_IID_IKsControl}



type

  PKSALLOCATORMODE = ^KSALLOCATORMODE;
  KSALLOCATORMODE = (
    KsAllocatorMode_User,
    KsAllocatorMode_Kernel
  );
  {$EXTERNALSYM KSALLOCATORMODE}


  PFRAMING_PROP = ^FRAMING_PROP;
  FRAMING_PROP = (
    FramingProp_Uninitialized,
    FramingProp_None,
    FramingProp_Old,
    FramingProp_Ex
  );
  {$EXTERNALSYM FRAMING_PROP}


  PFRAMING_CACHE_OPS = ^FRAMING_CACHE_OPS;
  FRAMING_CACHE_OPS = (
    Framing_Cache_Update,     // request to bypass cache when read/write
    Framing_Cache_ReadLast,
    Framing_Cache_ReadOrig,
    Framing_Cache_Write
  );
  {$EXTERNALSYM FRAMING_CACHE_OPS}

  POPTIMAL_WEIGHT_TOTALS = ^OPTIMAL_WEIGHT_TOTALS;
  OPTIMAL_WEIGHT_TOTALS = record
    MinTotalNominator: LONGLONG;
    MaxTotalNominator: LONGLONG;
    TotalDenominator: LONGLONG;
  end;
  {$EXTERNALSYM OPTIMAL_WEIGHT_TOTALS}


  //
  // allocators strategy is defined by graph manager
  //

const
  AllocatorStrategy_DontCare = 0;
  {$EXTERNALSYM AllocatorStrategy_DontCare}

  //
  // what to optimize
  //
  AllocatorStrategy_MinimizeNumberOfFrames     = $00000001;
  {$EXTERNALSYM AllocatorStrategy_MinimizeNumberOfFrames}
  AllocatorStrategy_MinimizeFrameSize          = $00000002;
  {$EXTERNALSYM AllocatorStrategy_MinimizeFrameSize}
  AllocatorStrategy_MinimizeNumberOfAllocators = $00000004;
  {$EXTERNALSYM AllocatorStrategy_MinimizeNumberOfAllocators}
  AllocatorStrategy_MaximizeSpeed              = $00000008;
  {$EXTERNALSYM AllocatorStrategy_MaximizeSpeed}

  //
  // factors (flags) defining the Pipes properties
  //
  PipeFactor_None               = 0;
  {$EXTERNALSYM PipeFactor_None}
  PipeFactor_UserModeUpstream   = $00000001;
  {$EXTERNALSYM PipeFactor_UserModeUpstream}
  PipeFactor_UserModeDownstream = $00000002;
  {$EXTERNALSYM PipeFactor_UserModeDownstream}
  PipeFactor_MemoryTypes        = $00000004;
  {$EXTERNALSYM PipeFactor_MemoryTypes}
  PipeFactor_Flags              = $00000008;
  {$EXTERNALSYM PipeFactor_Flags}
  PipeFactor_PhysicalRanges     = $00000010;
  {$EXTERNALSYM PipeFactor_PhysicalRanges}
  PipeFactor_OptimalRanges      = $00000020;
  {$EXTERNALSYM PipeFactor_OptimalRanges}
  PipeFactor_FixedCompression   = $00000040;
  {$EXTERNALSYM PipeFactor_FixedCompression}
  PipeFactor_UnknownCompression = $00000080;
  {$EXTERNALSYM PipeFactor_UnknownCompression}
  PipeFactor_Buffers            = $00000100;
  {$EXTERNALSYM PipeFactor_Buffers}
  PipeFactor_Align              = $00000200;
  {$EXTERNALSYM PipeFactor_Align}

  PipeFactor_PhysicalEnd        = $00000400;
  {$EXTERNALSYM PipeFactor_PhysicalEnd}
  PipeFactor_LogicalEnd         = $00000800;
  {$EXTERNALSYM PipeFactor_LogicalEnd}

type

  // Forward Interface declarations

  IKsInterfaceHandler = interface;
  IKsAllocatorEx = interface;
  IKsPin = interface;


  PPIPE_STATE = ^PIPE_STATE;
  PIPE_STATE = (
    PipeState_DontCare,
    PipeState_RangeNotFixed,
    PipeState_RangeFixed,
    PipeState_CompressionUnknown,
    PipeState_Finalized
  );
  {$EXTERNALSYM PIPE_STATE}

  //
  // pipe dimensions relative to BeginPin.
  //
  PPIPE_DIMENSIONS = ^PIPE_DIMENSIONS;
  PIPE_DIMENSIONS = record
    AllocatorPin    : KS_COMPRESSION;
    MaxExpansionPin : KS_COMPRESSION;
    EndPin          : KS_COMPRESSION;
  end;
  {$EXTERNALSYM PIPE_DIMENSIONS}


  PPIPE_ALLOCATOR_PLACE = ^PIPE_ALLOCATOR_PLACE;
  PIPE_ALLOCATOR_PLACE = (
    Pipe_Allocator_None,
    Pipe_Allocator_FirstPin,
    Pipe_Allocator_LastPin,
    Pipe_Allocator_MiddlePin
  );
  {$EXTERNALSYM PIPE_ALLOCATOR_PLACE}


  PKS_LogicalMemoryType = ^KS_LogicalMemoryType;
  KS_LogicalMemoryType = (
    KS_MemoryTypeDontCare,
    KS_MemoryTypeKernelPaged,
    KS_MemoryTypeKernelNonPaged,
    KS_MemoryTypeDeviceHostMapped,
    KS_MemoryTypeDeviceSpecific,
    KS_MemoryTypeUser,
    KS_MemoryTypeAnyHost
  );
  {$EXTERNALSYM KS_LogicalMemoryType}

  PPIPE_TERMINATION = ^PIPE_TERMINATION;
  PIPE_TERMINATION = record
    Flags          : ULONG;
    OutsideFactors : ULONG;
    Weigth         : ULONG;             // outside weight
    PhysicalRange  : KS_FRAMING_RANGE;
    OptimalRange   : KS_FRAMING_RANGE_WEIGHTED;
    Compression    : KS_COMPRESSION;   // relative to the connected pin on a neighboring filter.
  end;
  {$EXTERNALSYM PIPE_TERMINATION}


  //
  // extended allocator properties
  //
  PALLOCATOR_PROPERTIES_EX =^ALLOCATOR_PROPERTIES_EX;
  ALLOCATOR_PROPERTIES_EX = record
    cBuffers          : long;
    cbBuffer          : long;
    cbAlign           : long;
    cbPrefix          : long;
  // new part
    MemoryType        : TGUID;
    BusType           : TGUID;              // one of the buses this pipe is using
    State             : PIPE_STATE;
    Input             : PIPE_TERMINATION;
    Output            : PIPE_TERMINATION;
    Strategy          : ULONG;
    Flags             : ULONG;
    Weight            : ULONG;
    LogicalMemoryType : KS_LogicalMemoryType;
    AllocatorPlace    : PIPE_ALLOCATOR_PLACE;
    Dimensions        : PIPE_DIMENSIONS;
    PhysicalRange     : KS_FRAMING_RANGE;   // on allocator pin
    PrevSegment       : IKsAllocatorEx;     // doubly-linked list of KS allocators
    CountNextSegments : ULONG;              // possible multiple dependent pipes
    NextSegments      : IKsAllocatorEx;
    InsideFactors     : ULONG;              // existing factors (different from "don't care")
    NumberPins        : ULONG;
  end;
  {$EXTERNALSYM ALLOCATOR_PROPERTIES_EX}



  // INTERFACES  ///////////////////////////////////////////////////////////////


  // Interface IKsClockPropertySet
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsClockPropertySet);'}
  {$EXTERNALSYM IKsClockPropertySet}
  IKsClockPropertySet = interface(IUnknown)
  ['{5C5CBD84-E755-11D0-AC18-00A0C9223196}']

    procedure KsGetTime(out Time: LONGLONG); stdcall;

    procedure KsSetTime(Time: LONGLONG); stdcall;

    procedure KsGetPhysicalTime(out Time: LONGLONG); stdcall;

    procedure KsSetPhysicalTime(Time: LONGLONG); stdcall;

    procedure KsGetCorrelatedTime(out CorrelatedTime: KSCORRELATED_TIME); stdcall;

    procedure KsSetCorrelatedTime(CorrelatedTime: KSCORRELATED_TIME); stdcall;

    procedure KsGetCorrelatedPhysicalTime(out CorrelatedTime: KSCORRELATED_TIME); stdcall;

    procedure KsSetCorrelatedPhysicalTime(CorrelatedTime: KSCORRELATED_TIME); stdcall;

    procedure KsGetResolution(out Resolution: KSRESOLUTION); stdcall;

    procedure KsGetState(out State: KSSTATE); stdcall;
  end;
  IID_IKsClockPropertySet = IKsClockPropertySet;
  {$EXTERNALSYM IID_IKsClockPropertySet}


  // Interface IKsAllocator
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsAllocator);'}
  {$EXTERNALSYM IKsAllocator}
  IKsAllocator = interface(IUnknown)
  ['{8da64899-c0d9-11d0-8413-0000f822fe8a}']

    function  KsGetAllocatorHandle: THandle; stdcall;

    function  KsGetAllocatorMode: KSALLOCATORMODE; stdcall;

    procedure KsGetAllocatorStatus(AllocatorStatus: PKSSTREAMALLOCATOR_STATUS); stdcall;

    procedure KsSetAllocatorMode(Mode: KSALLOCATORMODE); stdcall;
  end;
  IID_IKsAllocator = IKsAllocator;
  {$EXTERNALSYM IID_IKsAllocator}


  // Interface IKsAllocatorEx
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsAllocatorEx);'}
  {$EXTERNALSYM IKsAllocatorEx}
  IKsAllocatorEx = interface(IKsAllocator)
  ['{091bb63a-603f-11d1-b067-00a0c9062802}']

    function  KsGetProperties: PALLOCATOR_PROPERTIES_EX; stdcall;

    procedure KsSetProperties(PROPERTIES: PALLOCATOR_PROPERTIES_EX); stdcall;

    procedure KsSetAllocatorHandle(AllocatorHandle: THandle); stdcall;

    function  KsCreateAllocatorAndGetHandle(KsPin: IKsPin): THandle; stdcall;
  end;
  IID_IKsAllocatorEx = IKsAllocatorEx;
  {$EXTERNALSYM IID_IKsAllocatorEx}


  PKSPEEKOPERATION = ^KSPEEKOPERATION;
  KSPEEKOPERATION = (
    KsPeekOperation_PeekOnly,
    KsPeekOperation_AddRef
  );
  {$EXTERNALSYM KSPEEKOPERATION}

  PKSSTREAM_SEGMENT = ^KSSTREAM_SEGMENT;


  // Interface IKsPin
  // ================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPin);'}
  {$EXTERNALSYM IKsPin}
  IKsPin = interface(IUnknown)
  ['{b61178d1-a2d9-11cf-9e53-00aa00a216a1}']

    function KsQueryMediums(out MediumList: PKSMULTIPLE_ITEM): HResult; stdcall;

    function KsQueryInterfaces(InterfaceList: PKSMULTIPLE_ITEM): HResult; stdcall;

    function KsCreateSinkPinHandle(_Interface: KSPIN_INTERFACE;
                                   Medium: KSPIN_MEDIUM): HResult; stdcall;

    function KsGetCurrentCommunication(Communication: PKSPIN_COMMUNICATION;
                                       _Interface: PKSPIN_INTERFACE;
                                       Medium: PKSPIN_MEDIUM): HResult; stdcall;

    function KsPropagateAcquire: HResult; stdcall;

    function KsDeliver(Sample: IMediaSample;
                       Flags: ULONG): HResult; stdcall;

    function KsMediaSamplesCompleted(StreamSegment: PKSSTREAM_SEGMENT): HResult; stdcall;

    function KsPeekAllocator(Operation: KSPEEKOPERATION): IMemAllocator; stdcall;

    function KsReceiveAllocator(MemAllocator: IMemAllocator): HResult; stdcall;

    function KsRenegotiateAllocator: HResult; stdcall;

    function KsIncrementPendingIoCount: Long; stdcall;

    function KsDecrementPendingIoCount: Long; stdcall;

    function KsQualityNotify(Proportion: ULONG;
                             TimeDelta: ReferenceTime): HResult; stdcall;
  end;
  IID_IKsPin = IKsPin;
  {$EXTERNALSYM IID_IKsPin}


  // Interface IKsPinEx
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPinEx);'}
  {$EXTERNALSYM IKsPinEx}
  IKsPinEx = interface(IKsPin)
  ['{7bb38260-d19c-11d2-b38a-00a0c95ec22e}']

    procedure KsNotifyError(Sample: IMediaSample;
                            hr: HResult);
  end;
  IID_IKsPinEx = IKsPinEx;
  {$EXTERNALSYM IID_IKsPinEx}


  // Interface IKsPinPipe
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPinPipe);'}
  {$EXTERNALSYM IKsPinPipe}
  IKsPinPipe = interface(IUnknown)
  ['{e539cd90-a8b4-11d1-8189-00a0c9062802}']

    procedure KsGetPinFramingCache(FramingEx: PKSALLOCATOR_FRAMING_EX;
                                  FramingProp: PFRAMING_PROP;
                                  Option: FRAMING_CACHE_OPS); stdcall;

    procedure KsSetPinFramingCache(FramingEx: PKSALLOCATOR_FRAMING_EX;
                                   FramingProp: PFRAMING_PROP;
                                   Option: FRAMING_CACHE_OPS); stdcall;

    function KsGetConnectedPin(): IPin; stdcall;

    function KsGetPipe(Operation: KSPEEKOPERATION): IKsAllocatorEx; stdcall;

    procedure KsSetPipe(KsAllocator: IKsAllocatorEx); stdcall;

    function KsGetPipeAllocatorFlag: ULONG; stdcall;

    procedure KsSetPipeAllocatorFlag(Flag: ULONG); stdcall;

    function KsGetPinBusCache(): TGUID; stdcall;

    procedure KsSetPinBusCache(const Bus: TGUID); stdcall;

    //
    // very useful methods for tracing.
    //

    function KsGetPinName: PWCHAR; stdcall;

    function KsGetFilterName: PWCHAR; stdcall;
  end;
  IID_IKsPinPipe = IKsPinPipe;
  {$EXTERNALSYM IID_IKsPinPipe}


  // Interface IKsPinFactory
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPinFactory);'}
  {$EXTERNALSYM IKsPinFactory}
  IKsPinFactory = interface(IUnknown)
  ['{CD5EBE6B-8B6E-11D1-8AE0-00A0C9223196}']

    procedure KsPinFactory(out PinFactory: ULONG); stdcall;
  end;
  IID_IKsPinFactory = IKsPinFactory;
  {$EXTERNALSYM IID_IKsPinFactory}


  PKSIOOPERATION = ^KSIOOPERATION;
  KSIOOPERATION = (
    KsIoOperation_Write,
    KsIoOperation_Read
  );
  {$EXTERNALSYM KSIOOPERATION}


  // Interface IKsDataTypeHandler
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsDataTypeHandler);'}
  {$EXTERNALSYM IKsDataTypeHandler}
  IKsDataTypeHandler = interface(IUnknown)
  ['{5ffbaa02-49a3-11d0-9f36-00aa00a216a1}']

    procedure KsCompleteIoOperation(var Sample: IMediaSample;
                                    var StreamHeader: Pointer;
                                    IoOperation: KSIOOPERATION;
                                    Cancelled: BOOL); stdcall;

    procedure KsIsMediaTypeInRanges(DataRanges: Pointer); stdcall;

    procedure KsPrepareIoOperation(var Sample: IMediaSample;
                                   var StreamHeader: Pointer;
                                   IoOperation: KSIOOPERATION); stdcall;

    procedure KsQueryExtendedSize(out ExtendedSize: ULONG); stdcall;

    procedure KsSetMediaType(const _AmMediaType: AM_MEDIA_TYPE); stdcall;
  end;
  IID_IKsDataTypeHandler = IKsDataTypeHandler;
  {$EXTERNALSYM IID_IKsDataTypeHandler}


  // Interface IKsDataTypeCompletion
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsDataTypeCompletion);'}
  {$EXTERNALSYM IKsDataTypeCompletion}
  IKsDataTypeCompletion = interface(IUnknown)
  ['{827D1A0E-0F73-11D2-B27A-00A0C9223196}']

    procedure KsCompleteMediaType(FilterHandle: THandle;
                                  PinFactoryId: ULONG;
                                  var _AmMediaType: AM_MEDIA_TYPE); stdcall;
  end;
  IID_IKsDataTypeCompletion = IKsDataTypeCompletion;
  {$EXTERNALSYM IID_IKsDataTypeCompletion}


  // PKSSTREAM_SEGMENT = ^_KSSTREAM_SEGMENT;  see^
  _KSSTREAM_SEGMENT = record
    KsInterfaceHandler: IKsInterfaceHandler;
    KsDataTypeHandler: IKsDataTypeHandler;
    IoOperation: KSIOOPERATION;
    CompletionEvent: THandle;
  end;
  {$EXTERNALSYM _KSSTREAM_SEGMENT}
  KSSTREAM_SEGMENT = _KSSTREAM_SEGMENT;
  {$EXTERNALSYM KSSTREAM_SEGMENT}

  // Interface IKsInterfaceHandler
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsInterfaceHandler);'}
  {$EXTERNALSYM IKsInterfaceHandler}
  IKsInterfaceHandler = interface(IUnknown)
  ['{D3ABC7E0-9A61-11d0-A40D-00A0C9223196}']

    procedure KsSetPin(KsPin: IKsPin); stdcall;

    procedure KsProcessMediaSamples(KsDataTypeHandler: IKsDataTypeHandler;
                                    SampleList: IMediaSample;
                                    var SampleCount: LONG;
                                    IoOperation: KSIOOPERATION;
                                    out StreamSegment: KSSTREAM_SEGMENT); stdcall;

    procedure KsCompleteIo(var StreamSegment: KSSTREAM_SEGMENT); stdcall;
  end;
  IID_IKsInterfaceHandler = IKsInterfaceHandler;
  {$EXTERNALSYM IID_IKsInterfaceHandler}


  //
  // This structure definition is the common header required by the proxy to
  // dispatch the stream segment to the interface handler.  Interface handlers
  // will create extended structures to include other information such as
  // media samples, extended header size and so on.
  //


  // Interface IKsObject
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsObject);'}
  {$EXTERNALSYM IKsObject}
  IKsObject = interface(IUnknown)
  ['{423c13a2-2070-11d0-9ef7-00aa00a216a1}']

    function KsGetObjectHandle: THandle; stdcall;
  end;
  IID_IKsObject = IKsObject;
  {$EXTERNALSYM IID_IKsObject}


  // Interface IKsQualityForwarder
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsQualityForwarder);'}
  {$EXTERNALSYM IKsQualityForwarder}
  IKsQualityForwarder = interface(IUnknown)
  ['{97ebaacb-95bd-11d0-a3ea-00a0c9223196}']

    procedure KsFlushClient(Pin: IKsPin); stdcall;
  end;
  IID_IKsQualityForwarder = IKsQualityForwarder;
  {$EXTERNALSYM IID_IKsQualityForwarder}


  // Interface IKsNotifyEvent
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsNotifyEvent);'}
  {$EXTERNALSYM IKsNotifyEvent}
  IKsNotifyEvent = interface(IUnknown)
  ['{412bd695-f84b-46c1-ac73-54196dbc8fa7}']

    procedure KsNotifyEvent(Event: ULONG;
                            lParam1: ULONG_PTR;
                            lParam2: ULONG_PTR); stdcall;
  end;
  IID_IKsNotifyEvent = IKsNotifyEvent;
  {$EXTERNALSYM IID_IKsNotifyEvent}


  // Interface IKsPropertySet
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPropertySet);'}
  {$EXTERNALSYM IKsPropertySet}
  IKsPropertySet = interface(IUnknown)
  ['{31EFAC30-515C-11d0-A9AA-00aa0061be93}']

    function _Set(const PropSet: TGUID;
                  Id: ULONG;
                  InstanceData: Pointer;
                  InstanceLength: ULONG;
                  PropertyData: Pointer;
                  DataLength: ULONG): HRESULT; stdcall;

    function Get(const PropSet: TGUID;
                 Id: ULONG;
                 InstanceData: Pointer;
                 InstanceLength: ULONG;
                 out PropertyData: Pointer;
                 DataLength: ULONG;
                 out BytesReturned: PULONG): HRESULT; stdcall;

    function QuerySupported(const PropSet: TGUID;
                            Id: ULONG;
                            out TypeSupport: PULONG): HRESULT; stdcall;
  end;
  IID_IKsPropertySet = IKsPropertySet;
  {$EXTERNALSYM IID_IKsPropertySet}


  // Interface IKsControl
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsControl);'}
  {$EXTERNALSYM IKsControl}
  IKsControl = interface(IUnknown)
  ['{28F54685-06FD-11D2-B27A-00A0C9223196}']

    function KsProperty(_Property: PKSPROPERTY;
                        PropertyLength: ULONG;
                        var PropertyData: Pointer;
                        DataLength: ULONG;
                        var {OPTIONAL} BytesReturned: PULONG): HRESULT; stdcall;

    function KsMethod(Method: PKSMETHOD;
                      MethodLength: ULONG;
                      var MethodData: Pointer;
                      DataLength: ULONG;
                      var {OPTIONAL} BytesReturned: PULONG): HRESULT; stdcall;

    function KsEvent(Event: PKSEVENT;
                     EventLength: ULONG;
                     var EventData: Pointer;
                     DataLength: ULONG;
                     var {OPTIONAL} BytesReturned: PULONG): HRESULT; stdcall;
  end;
  IID_IKsControl = IKsControl;
  {$EXTERNALSYM IID_IKsControl}


  // Interface IKsAggregateControl
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsAggregateControl);'}
  {$EXTERNALSYM IKsAggregateControl}
  IKsAggregateControl = interface(IUnknown)
  ['{7F40EAC0-3947-11D2-874E-00A0C9223196}']

    procedure KsAddAggregate(const AggregateClass: TGUID); stdcall;

    procedure KsRemoveAggregate(const AggregateClass: TGUID); stdcall;
  end;
  IID_IKsAggregateControl = IKsAggregateControl;
  {$EXTERNALSYM IID_IKsAggregateControl}


  // Interface IKsTopology
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsTopology);'}
  {$EXTERNALSYM IKsTopology}
  IKsTopology = interface(IUnknown)
  ['{28F54683-06FD-11D2-B27A-00A0C9223196}']

    procedure CreateNodeInstance(NodeId: ULONG;
                                 Flags: ULONG;
                                  DesiredAccess: ACCESS_MASK;
                                 {OPTIONAL}UnkOuter: IUnknown;
                                 const InterfaceId: TGUID;
                                 out _Interface); stdcall;
  end;
  IID_IKsTopology = IKsTopology;
  {$EXTERNALSYM IID_IKsTopology}


  function KsResolveRequiredAttributes(DataRange: PKSDATARANGE;
                                       {In OPTIONAL} Attributes: PKSMULTIPLE_ITEM): HResult; stdcall;
  {$EXTERNALSYM KsResolveRequiredAttributes}

  function KsOpenDefaultDevice(Category: TGUID;
                               Access: ACCESS_MASK;
                               out DeviceHandle: PHANDLE): HResult; stdcall;
  {$EXTERNALSYM KsOpenDefaultDevice}

  function KsSynchronousDeviceControl(Handle: THandle;
                                      IoControl: ULONG;
                                      InBuffer: Pointer; {In OPTIONAL}
                                      InLength: ULONG;
                                      out OutBuffer: Pointer; {Out OPTIONAL}
                                      OutLength: ULONG;
                                      var BytesReturned: PULONG {Inout OPTIONAL}): HResult; stdcall;
  {$EXTERNALSYM KsSynchronousDeviceControl}

  function KsGetMultiplePinFactoryItems(FilterHandle: THandle;
                                        PinFactoryId: ULONG;
                                        PropertyId: ULONG;
                                        out Items: Pointer): HResult; stdcall;
  {$EXTERNALSYM KsGetMultiplePinFactoryItems}

  function KsGetMediaTypeCount(FilterHandle: THandle;
                               PinFactoryId: ULONG;
                               out MediaTypeCount: ULONG): HResult; stdcall;
  {$EXTERNALSYM KsGetMediaTypeCount}

  function KsGetMediaType(Position: Integer;
                          out AmMediaType: AM_MEDIA_TYPE;
                          FilterHandle: THandle;
                          PinFactoryId: ULONG): HResult; stdcall;
  {$EXTERNALSYM KsGetMediaType}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

const
  KsProxyLib = 'ksproxy.dll';

{$WARN SYMBOL_PLATFORM OFF}
function KsResolveRequiredAttributes ; external KsProxyLib name 'KsResolveRequiredAttributes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function KsOpenDefaultDevice         ; external KsProxyLib name 'KsOpenDefaultDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function KsSynchronousDeviceControl  ; external KsProxyLib name 'KsSynchronousDeviceControl' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function KsGetMultiplePinFactoryItems; external KsProxyLib name 'KsGetMultiplePinFactoryItems' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function KsGetMediaTypeCount         ; external KsProxyLib name 'KsGetMediaTypeCount' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function KsGetMediaType              ; external KsProxyLib name 'KsGetMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.
