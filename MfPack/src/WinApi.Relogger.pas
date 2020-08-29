// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Relogger.pas
// Kind: Pascal / Delphi unit
// Release date: 24-12-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Source: relogger.h
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
unit WinApi.Relogger;

  {$HPPEMIT '#include "relogger.h"'}

interface

uses
  {Winapi}
  Winapi.Windows,
  WinApi.WinApiTypes,
  WinApi.Evntrace;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  // Forward Interface Declarations

  PITraceRelogger = ^ITraceRelogger;
  ITraceRelogger = interface;


  PEVENT_DESCRIPTOR = ^EVENT_DESCRIPTOR;
  PCEVENT_DESCRIPTOR = ^EVENT_DESCRIPTOR;
  _EVENT_DESCRIPTOR = record
    Id: USHORT;
    Version: UCHAR;
    Channel: UCHAR;
    Level: UCHAR;
    Opcode: UCHAR;
    Task: USHORT;
    Keyword: ULONGLONG;
  end;
  {$EXTERNALSYM _EVENT_DESCRIPTOR}
  EVENT_DESCRIPTOR = _EVENT_DESCRIPTOR;
  {$EXTERNALSYM EVENT_DESCRIPTOR}


  PEVENT_HEADER = ^EVENT_HEADER;
  _EVENT_HEADER = record
    Size: USHORT;                        // Event Size
    HeaderType: USHORT;                  // Header Type
    Flags: USHORT;                       // Flags
    EventProperty: USHORT;               // User given event property
    ThreadId: ULONG;                     // Thread Id
    ProcessId: ULONG;                    // Process Id
    TimeStamp: LARGE_INTEGER;            // Event Timestamp
    ProviderId: TGUID;                   // Provider Id
    EventDescriptor: EVENT_DESCRIPTOR;   // Event Descriptor
      DUMMYSTRUCTNAME: record
      case Boolean of
        True  : (KernelTime: ULONG;      // Kernel Mode CPU ticks
                 UserTime: ULONG);       // User mode CPU ticks
        False : (ProcessorTime: ULONG64;);  // Processor Clock for private session event
      end;
    ActivityId: TGUID;                  // Activity Id
  end;
  {$EXTERNALSYM _EVENT_HEADER}
  EVENT_HEADER = _EVENT_HEADER;
  {$EXTERNALSYM EVENT_HEADER}


  PETW_BUFFER_CONTEXT = ^_ETW_BUFFER_CONTEXT;
  _ETW_BUFFER_CONTEXT = record
    DUMMYSTRUCTNAME: record
    case Boolean of
      True  : (ProcessorNumber: UCHAR;
               Alignment: UCHAR);
      False : (ProcessorIndex: USHORT;);
    end;
    LoggerId: USHORT;
  end;
  {$EXTERNALSYM _ETW_BUFFER_CONTEXT}
  ETW_BUFFER_CONTEXT = _ETW_BUFFER_CONTEXT;
  {$EXTERNALSYM ETW_BUFFER_CONTEXT}


  PEVENT_HEADER_EXTENDED_DATA_ITEM = ^EVENT_HEADER_EXTENDED_DATA_ITEM;
  _EVENT_HEADER_EXTENDED_DATA_ITEM = record
    Reserved1: USHORT;              // Reserved for internal use
    ExtType: USHORT;                // Extended info type
    Linkage: USHORT;                // Indicates additional extended
    DataSize: USHORT;               // Size of extended info data
    DataPtr: ULONGLONG;             // Pointer to extended info data
  end;
  {$EXTERNALSYM _EVENT_HEADER_EXTENDED_DATA_ITEM}
  EVENT_HEADER_EXTENDED_DATA_ITEM = _EVENT_HEADER_EXTENDED_DATA_ITEM;
  {$EXTERNALSYM EVENT_HEADER_EXTENDED_DATA_ITEM}


  PEVENT_RECORD = ^EVENT_RECORD;
  PCEVENT_RECORD = ^EVENT_RECORD;
  _EVENT_RECORD = record
    EventHeader: EVENT_HEADER;                        // Event header
    BufferContext: ETW_BUFFER_CONTEXT;                // Buffer context
    ExtendedDataCount: USHORT;                        // Number of extended
                                                      // data items
    UserDataLength: USHORT;                           // User data length              // Pointer to an array of
    ExtendedData: PEVENT_HEADER_EXTENDED_DATA_ITEM;   // extended data items
    UserData: Pointer;                                // Pointer to user data
    UserContext: Pointer;                            // Context from OpenTrace
  end;
  {$EXTERNALSYM _EVENT_RECORD}
  EVENT_RECORD = _EVENT_RECORD;
  {$EXTERNALSYM EVENT_RECORD}




  // Interface ITraceEvent
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITraceEvent);'}
  {$EXTERNALSYM ITraceEvent}
  ITraceEvent = interface(IUnknown)
	['{8CC97F40-9028-4FF3-9B62-7D1F79CA7BCB}']

    function Clone(var NewEvent: ITraceEvent): HResult; stdcall;

    function GetUserContext(UserContext: PPointer): HResult; stdcall;

    function GetEventRecord(var EventRecord: PEVENT_RECORD): HResult; stdcall;

    function SetPayload(Payload: PByte;
                        PayloadSize: ULONG): HResult; stdcall;

    function SetEventDescriptor(EventDescriptor: EVENT_DESCRIPTOR): HResult; stdcall;

    function SetProcessId(ProcessId: ULONG): HResult; stdcall;

    function SetProcessorIndex(ProcessorIndex: ULONG): HResult; stdcall;

    function SetThreadId(ThreadId: ULONG): HResult; stdcall;

    function SetThreadTimes(KernelTime: ULONG;
                            UserTime: ULONG): HResult; stdcall;

    function SetActivityId(ActivityId: TGUID): HResult; stdcall;

    function SetTimeStamp(var TimeStamp: LARGE_INTEGER): HResult; stdcall;

    function SetProviderId(ProviderId: TGUID): HResult; stdcall;

  end;
  IID_ITraceEvent = ITraceEvent;
  {$EXTERNALSYM IID_ITraceEvent}



  // Interface ITraceEventCallback
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITraceEventCallback);'}
  {$EXTERNALSYM ITraceEventCallback}
  ITraceEventCallback = interface(IUnknown)
	['{3ED25501-593F-43E9-8F38-3AB46F5A4A52}']

    function OnBeginProcessTrace(var HeaderEvent: ITraceEvent;
                                 var Relogger: ITraceRelogger): HResult; stdcall;

    function OnFinalizeProcessTrace(var Relogger: ITraceRelogger): HResult; stdcall;

    function OnEvent(var Event: ITraceEvent;
                     var Relogger: ITraceRelogger): HResult; stdcall;

  end;
  IID_ITraceEventCallback = ITraceEventCallback;
  {$EXTERNALSYM IID_ITraceEventCallback}



  // Interface ITraceRelogger
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITraceRelogger);'}
  {$EXTERNALSYM ITraceRelogger}
  ITraceRelogger = interface(IUnknown)
	['{F754AD43-3BCC-4286-8009-9C5DA214E84E}']

    function AddLogfileTraceStream(LogfileName: BSTR;
                                   UserContext: Pointer;
                                   var TraceHandle: TRACEHANDLE): HResult; stdcall;

    function AddRealtimeTraceStream(LoggerName: BSTR;
                                    UserContext: Pointer;
                                    var TraceHandle: TRACEHANDLE): HResult; stdcall;

    function RegisterCallback(var Callback: ITraceEventCallback): HResult; stdcall;

    function Inject(var Event: ITraceEvent): HResult; stdcall;

    function CreateEventInstance(TraceHandle: TRACEHANDLE;
                                 Flags: ULONG;
                                 var Event: ITraceEvent): HResult; stdcall;

    function ProcessTrace(): HResult; stdcall;

    function SetOutputFilename(LogfileName: BSTR): HResult; stdcall;

    function SetCompressionMode(CompressionMode: BOOLEAN): HResult; stdcall;

    function Cancel(): HResult; stdcall;

  end;
  IID_ITraceRelogger = ITraceRelogger;
  {$EXTERNALSYM IID_ITraceRelogger}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
