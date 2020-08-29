// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Evntcons.pas
// Kind: Pascal / Delphi unit
// Release date: 18-12-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This unit defines the event consumer API.
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
// Remarks: Requires Windows Vista or later.
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
// Source: evntcons.h
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
//==============================================================================
unit WinApi.Evntcons;

  {$HPPEMIT '#include "evntcons.h"'}

interface

uses

  {Winapi}
  Winapi.Windows,
  WinApi.Evntprov;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  EVENT_HEADER_EXT_TYPE_RELATED_ACTIVITYID  = $0001;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_RELATED_ACTIVITYID}
  EVENT_HEADER_EXT_TYPE_SID                 = $0002;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_SID}
  EVENT_HEADER_EXT_TYPE_TS_ID               = $0003;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_TS_ID}
  EVENT_HEADER_EXT_TYPE_INSTANCE_INFO       = $0004;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_INSTANCE_INFO}
  EVENT_HEADER_EXT_TYPE_STACK_TRACE32       = $0005;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_STACK_TRACE32}
  EVENT_HEADER_EXT_TYPE_STACK_TRACE64       = $0006;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_STACK_TRACE64}
  EVENT_HEADER_EXT_TYPE_PEBS_INDEX          = $0007;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_PEBS_INDEX}
  EVENT_HEADER_EXT_TYPE_PMC_COUNTERS        = $0008;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_PMC_COUNTERS}
  EVENT_HEADER_EXT_TYPE_PSM_KEY             = $0009;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_PSM_KEY}
  EVENT_HEADER_EXT_TYPE_EVENT_KEY           = $000A;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_EVENT_KEY}
  EVENT_HEADER_EXT_TYPE_EVENT_SCHEMA_TL     = $000B;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_EVENT_SCHEMA_TL}
  EVENT_HEADER_EXT_TYPE_PROV_TRAITS         = $000C;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_PROV_TRAITS}
  EVENT_HEADER_EXT_TYPE_PROCESS_START_KEY   = $000D;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_PROCESS_START_KEY}
  EVENT_HEADER_EXT_TYPE_CONTROL_GUID        = $000E;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_CONTROL_GUID}
  EVENT_HEADER_EXT_TYPE_QPC_DELTA           = $000F;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_QPC_DELTA}
  EVENT_HEADER_EXT_TYPE_MAX                 = $0010;
  {$EXTERNALSYM EVENT_HEADER_EXT_TYPE_MAX}


type
// #ifndef EVENT_HEADER_EXTENDED_DATA_ITEM_DEF
// #define EVENT_HEADER_EXTENDED_DATA_ITEM_DEF


 PEVENT_HEADER_EXTENDED_DATA_ITEM = ^EVENT_HEADER_EXTENDED_DATA_ITEM;
 _EVENT_HEADER_EXTENDED_DATA_ITEM = record
  public
    Value: UINT;                             // Same as 'flags' used to acces all bitvalues
    Reserved1: USHORT;                       // Reserved for internal use
    ExtType: USHORT;                         // Extended info type
    DataSize: USHORT;                        // Size of extended info data
    DataPtr: ULONGLONG;                      // Pointer to extended info data
  private
    function ReadBits(const aIndex: Integer): Integer;
    procedure WriteBits(const aIndex: Integer; const aValue: Integer);
  public
    property Linkage: Integer index $0001 read ReadBits write WriteBits;      // 1 bit at offset 0
    property Reserved2: Integer index $0215 read ReadBits write WriteBits;    // 15 bits at offset 2
  end;
  EVENT_HEADER_EXTENDED_DATA_ITEM = _EVENT_HEADER_EXTENDED_DATA_ITEM;


  // Structures for extended items.
  // ==============================

  PEVENT_EXTENDED_ITEM_INSTANCE = ^EVENT_EXTENDED_ITEM_INSTANCE;
  _EVENT_EXTENDED_ITEM_INSTANCE = record
    InstanceId: ULONG;
    ParentInstanceId: ULONG;
    ParentGuid: TGUID;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_INSTANCE}
  EVENT_EXTENDED_ITEM_INSTANCE = _EVENT_EXTENDED_ITEM_INSTANCE;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_INSTANCE}


  PEVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = ^EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID;
  _EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = record
    RelatedActivityId: TGUID;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID}
  EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = _EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID}


  PEVENT_EXTENDED_ITEM_TS_ID = ^EVENT_EXTENDED_ITEM_TS_ID;
  _EVENT_EXTENDED_ITEM_TS_ID = record
    SessionId: ULONG;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_TS_ID}
  EVENT_EXTENDED_ITEM_TS_ID = _EVENT_EXTENDED_ITEM_TS_ID;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_TS_ID}


  PEVENT_EXTENDED_ITEM_STACK_TRACE32 = ^EVENT_EXTENDED_ITEM_STACK_TRACE32;
  _EVENT_EXTENDED_ITEM_STACK_TRACE32 = record
    MatchId: ULONG64;
    Address: array[0..ANYSIZE_ARRAY - 1] of ULONG;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_STACK_TRACE32}
  EVENT_EXTENDED_ITEM_STACK_TRACE32 = _EVENT_EXTENDED_ITEM_STACK_TRACE32;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_STACK_TRACE32}


  PEVENT_EXTENDED_ITEM_STACK_TRACE64 = ^EVENT_EXTENDED_ITEM_STACK_TRACE64;
  _EVENT_EXTENDED_ITEM_STACK_TRACE64 = record
    MatchId: ULONG64;
    Address: array[0..ANYSIZE_ARRAY - 1] of ULONG64;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_STACK_TRACE64}
  EVENT_EXTENDED_ITEM_STACK_TRACE64 = _EVENT_EXTENDED_ITEM_STACK_TRACE64;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_STACK_TRACE64}


  PEVENT_EXTENDED_ITEM_PEBS_INDEX = ^EVENT_EXTENDED_ITEM_PEBS_INDEX;
  _EVENT_EXTENDED_ITEM_PEBS_INDEX = record
    PebsIndex: ULONG64;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_PEBS_INDEX}
  EVENT_EXTENDED_ITEM_PEBS_INDEX = _EVENT_EXTENDED_ITEM_PEBS_INDEX;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_PEBS_INDEX}


  PEVENT_EXTENDED_ITEM_PMC_COUNTERS = ^EVENT_EXTENDED_ITEM_PMC_COUNTERS;
  _EVENT_EXTENDED_ITEM_PMC_COUNTERS = record
    Counter: array[0..ANYSIZE_ARRAY - 1] of ULONG64;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_PMC_COUNTERS}
  EVENT_EXTENDED_ITEM_PMC_COUNTERS = _EVENT_EXTENDED_ITEM_PMC_COUNTERS;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_PMC_COUNTERS}


  PEVENT_EXTENDED_ITEM_PROCESS_START_KEY = ^EVENT_EXTENDED_ITEM_PROCESS_START_KEY;
  _EVENT_EXTENDED_ITEM_PROCESS_START_KEY = record
    ProcessStartKey: ULONG64;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_PROCESS_START_KEY}
  EVENT_EXTENDED_ITEM_PROCESS_START_KEY = _EVENT_EXTENDED_ITEM_PROCESS_START_KEY;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_PROCESS_START_KEY}


  PEVENT_EXTENDED_ITEM_EVENT_KEY = ^EVENT_EXTENDED_ITEM_EVENT_KEY;
  _EVENT_EXTENDED_ITEM_EVENT_KEY = record
    Key: ULONG64;
  end;
  {$EXTERNALSYM _EVENT_EXTENDED_ITEM_EVENT_KEY}
  EVENT_EXTENDED_ITEM_EVENT_KEY = _EVENT_EXTENDED_ITEM_EVENT_KEY;
  {$EXTERNALSYM EVENT_EXTENDED_ITEM_EVENT_KEY}


const

  EVENT_HEADER_PROPERTY_XML             = $0001;
  {$EXTERNALSYM EVENT_HEADER_PROPERTY_XML}
  EVENT_HEADER_PROPERTY_FORWARDED_XML   = $0002;
  {$EXTERNALSYM EVENT_HEADER_PROPERTY_FORWARDED_XML}
  EVENT_HEADER_PROPERTY_LEGACY_EVENTLOG = $0004;
  {$EXTERNALSYM EVENT_HEADER_PROPERTY_LEGACY_EVENTLOG}
  EVENT_HEADER_PROPERTY_RELOGGABLE      = $0008;
  {$EXTERNALSYM EVENT_HEADER_PROPERTY_RELOGGABLE}

  EVENT_HEADER_FLAG_EXTENDED_INFO       = $0001;
    {$EXTERNALSYM EVENT_HEADER_FLAG_EXTENDED_INFO}
  EVENT_HEADER_FLAG_PRIVATE_SESSION     = $0002;
  {$EXTERNALSYM EVENT_HEADER_FLAG_PRIVATE_SESSION}
  EVENT_HEADER_FLAG_STRING_ONLY         = $0004;
  {$EXTERNALSYM EVENT_HEADER_FLAG_STRING_ONLY}
  EVENT_HEADER_FLAG_TRACE_MESSAGE       = $0008;
  {$EXTERNALSYM EVENT_HEADER_FLAG_TRACE_MESSAGE}
  EVENT_HEADER_FLAG_NO_CPUTIME          = $0010;
  {$EXTERNALSYM EVENT_HEADER_FLAG_NO_CPUTIME}
  EVENT_HEADER_FLAG_32_BIT_HEADER       = $0020;
  {$EXTERNALSYM EVENT_HEADER_FLAG_32_BIT_HEADER}
  EVENT_HEADER_FLAG_64_BIT_HEADER       = $0040;
  {$EXTERNALSYM EVENT_HEADER_FLAG_64_BIT_HEADER}
  EVENT_HEADER_FLAG_DECODE_GUID         = $0080;  // ProviderId is decode GUID.
  {$EXTERNALSYM EVENT_HEADER_FLAG_DECODE_GUID}
  EVENT_HEADER_FLAG_CLASSIC_HEADER      = $0100;
  {$EXTERNALSYM EVENT_HEADER_FLAG_CLASSIC_HEADER}
  EVENT_HEADER_FLAG_PROCESSOR_INDEX     = $0200;
  {$EXTERNALSYM EVENT_HEADER_FLAG_PROCESSOR_INDEX}

//#ifndef EVENT_HEADER_DEF
//#define EVENT_HEADER_DEF

type

  PEVENT_HEADER = ^EVENT_HEADER;
  _EVENT_HEADER = record
    Size: USHORT;                   // Event Size
    HeaderType: USHORT;             // Header Type
    Flags: USHORT;                  // Flags
    EventProperty: USHORT;          // User given event property
    ThreadId: ULONG;                // Thread Id
    ProcessId: ULONG;               // Process Id
    TimeStamp: LARGE_INTEGER;       // Event Timestamp
    ProviderId: TGUID;              // Provider Id
    EventDescriptor: EVENT_DESCRIPTOR;    // Event Descriptor
    DUMMYSTRUCTNAME : record
    case Boolean of
      True  : (KernelTime: ULONG;         // Kernel Mode CPU ticks
               UserTime: ULONG);          // User mode CPU ticks
      False : (ProcessorTime: ULONG64);   // Processor Clock
    end;
  {$EXTERNALSYM _EVENT_HEADER}
    ActivityId: TGUID;              // Activity Id
  end;
  EVENT_HEADER = _EVENT_HEADER;
  {$EXTERNALSYM EVENT_HEADER}


  {$IFNDEF ETW_BUFFER_CONTEXT_DEF}
    {$DEFINE ETW_BUFFER_CONTEXT_DEF}
  {$ENDIF}
  // ATTENTION: This struct is normally defined in Evntrace.h
  //            To prevent circulair references it's defined here.

  PETW_BUFFER_CONTEXT = ^ETW_BUFFER_CONTEXT;
  _ETW_BUFFER_CONTEXT = record
    DUMMYSTRUCTNAME: record
    case Boolean of
      True  : (ProcessorNumber: UCHAR;
               Alignment: UCHAR);
      False : (ProcessorIndex: USHORT);
    end;
  {$EXTERNALSYM _ETW_BUFFER_CONTEXT}
    LoggerId: USHORT;
  end;
  ETW_BUFFER_CONTEXT = _ETW_BUFFER_CONTEXT;
  {$EXTERNALSYM ETW_BUFFER_CONTEXT}


  {$IFNDEF EVENT_RECORD_DEF}
    {$DEFINE EVENT_RECORD_DEF}
  {$ENDIF}

  PEVENT_RECORD = ^EVENT_RECORD;
  PCEVENT_RECORD = ^EVENT_RECORD;
  _EVENT_RECORD = record
    EventHeader: EVENT_HEADER;          // Event header
    BufferContext: ETW_BUFFER_CONTEXT;  // Buffer context
    ExtendedDataCount: USHORT;          // Number of extended
                                        // data items
    UserDataLength: USHORT;             // User data length
    ExtendedData: PEVENT_HEADER_EXTENDED_DATA_ITEM;   // extended data items
    UserData: Pointer;                  // Pointer to user data
    UserContext: Pointer;               // Context from OpenTrace
  end;
  {$EXTERNALSYM _EVENT_RECORD}
  EVENT_RECORD = _EVENT_RECORD;
  {$EXTERNALSYM EVENT_RECORD}

const

  EVENT_ENABLE_PROPERTY_SID               = $00000001;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_SID}
  EVENT_ENABLE_PROPERTY_TS_ID             = $00000002;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_TS_ID}
  EVENT_ENABLE_PROPERTY_STACK_TRACE       = $00000004;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_STACK_TRACE}
  EVENT_ENABLE_PROPERTY_PSM_KEY           = $00000008;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_PSM_KEY}
  EVENT_ENABLE_PROPERTY_IGNORE_KEYWORD_0  = $00000010;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_IGNORE_KEYWORD_0}
  EVENT_ENABLE_PROPERTY_PROVIDER_GROUP    = $00000020;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_PROVIDER_GROUP}
  EVENT_ENABLE_PROPERTY_ENABLE_KEYWORD_0  = $00000040;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_ENABLE_KEYWORD_0}
  EVENT_ENABLE_PROPERTY_PROCESS_START_KEY = $00000080;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_PROCESS_START_KEY}
  EVENT_ENABLE_PROPERTY_EVENT_KEY         = $00000100;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_EVENT_KEY}
  EVENT_ENABLE_PROPERTY_EXCLUDE_INPRIVATE = $00000200;
  {$EXTERNALSYM EVENT_ENABLE_PROPERTY_EXCLUDE_INPRIVATE}


  // Consumer API
  // ============

  PROCESS_TRACE_MODE_REAL_TIME        = $00000100;
  {$EXTERNALSYM PROCESS_TRACE_MODE_REAL_TIME}
  PROCESS_TRACE_MODE_RAW_TIMESTAMP    = $00001000;
  {$EXTERNALSYM PROCESS_TRACE_MODE_RAW_TIMESTAMP}
  PROCESS_TRACE_MODE_EVENT_RECORD     = $10000000;
  {$EXTERNALSYM PROCESS_TRACE_MODE_EVENT_RECORD}


  function GetEventProcessorIndex(EventRecord: EVENT_RECORD): ULONG;


  // Provider Trait APIs
  // ===================

type

  PETW_PROVIDER_TRAIT_TYPE = ^ETW_PROVIDER_TRAIT_TYPE;
  ETW_PROVIDER_TRAIT_TYPE      = (
    EtwProviderTraitTypeGroup  = 1,  // Provider group GUID.
    EtwProviderTraitDecodeGuid = 2,  // Decode GUID (when different from control GUID)
    EtwProviderTraitTypeMax    = 3
  );
  {$EXTERNALSYM ETW_PROVIDER_TRAIT_TYPE}
  TEtwProviderTraitType = ETW_PROVIDER_TRAIT_TYPE;
  {$EXTERNALSYM TEtwProviderTraitType}


  // Event Security APIs
  // ===================

  PEVENTSECURITYOPERATION = ^EVENTSECURITYOPERATION;
  EVENTSECURITYOPERATION = (
    EventSecuritySetDACL = 0,
    EventSecuritySetSACL = 1,
    EventSecurityAddDACL = 2,
    EventSecurityAddSACL = 3,
    EventSecurityMax     = 4
  );
  {$EXTERNALSYM EVENTSECURITYOPERATION}
  TEventsecurityoperation = EVENTSECURITYOPERATION;
  {$EXTERNALSYM TEventsecurityoperation}

//#if (WINVER >= _WIN32_WINNT_LONGHORN)

  function EventAccessControl(const Guid: TGUID;
                              Operation: ULONG;
                              Sid: PSID;
                              Rights: ULONG;
                              AllowOrDeny: BOOLEAN): ULONG; stdcall;
  {$EXTERNALSYM EventAccessControl}

  function EventAccessQuery(const Guid: PGUID;
                            out Buffer: PSECURITY_DESCRIPTOR;
                            var BufferSize: PULONG): ULONG; stdcall;
  {$EXTERNALSYM EventAccessQuery}

  function EventAccessRemove(const Guid: PGUID): ULONG; stdcall;
  {$EXTERNALSYM EventAccessRemove}
//#endif




  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


  // Sechost.dll on Windows 8.1 and Windows Server 2012;

const

  EvntconsLib = 'Sechost.dll';

  // Advapi32.dll on Windows 8, Windows Server 2012, Windows 7, Windows Server 2008 R2,
  // Windows Server 2008 and Windows Vista
  // const EvntconsLib = 'Advapi32.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function EventAccessControl; external EvntconsLib name 'EventAccessControl' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EventAccessQuery; external EvntconsLib name 'EventAccessQuery' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EventAccessRemove; external EvntconsLib name 'EventAccessRemove' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}


  // _EVENT_HEADER_EXTENDED_DATA_ITEM helpers
  // ========================================
  function _EVENT_HEADER_EXTENDED_DATA_ITEM.ReadBits(const aIndex: Integer): Integer;
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits:= aIndex and $FF;
    Offset:= aIndex shr 8;
    Mask:= ((1 shl NrBits) - 1);
    Result:= (Value shr Offset) and Mask;
  end;

  procedure _EVENT_HEADER_EXTENDED_DATA_ITEM.WriteBits(const aIndex: Integer; const aValue: Integer);
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;

  begin
    NrBits:= aIndex and $FF;
    Offset:= aIndex shr 8;
    Mask:= ((1 shl NrBits) - 1);
    Assert(aValue <= Mask);
    Value:= (Value and (not (UINT(Mask) shl UINT(Offset)))) or (UINT(aValue) shl UINT(Offset));
  end;


  function GetEventProcessorIndex(EventRecord: EVENT_RECORD): ULONG;
  begin
    if (EventRecord.EventHeader.Flags AND EVENT_HEADER_FLAG_PROCESSOR_INDEX) <> 0 then
      Result:= EventRecord.BufferContext.DUMMYSTRUCTNAME.ProcessorNumber
    else
      Result:= EventRecord.BufferContext.DUMMYSTRUCTNAME.ProcessorIndex;
  end;

  // Implement Additional Prototypes here.

end.
