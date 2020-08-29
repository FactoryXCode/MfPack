// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Evntrace.pas
// Kind: Pascal / Delphi unit
// Release date: 24-11-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Public headers for event tracing control applications,
// consumers and providers.
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
// Remarks: LIBRARY CHANGES:
//          Sechost.dll on Windows 8.1 and Windows Server 2012 R2;
//          Advapi32.dll on Windows 8, Windows Server 2012, Windows 7,
//          Windows Server 2008 R2, Windows Server 2008, Windows Vista and Windows XP
//          See: const EvntraceLib declaration.
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
// Source: evntrace.h
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
unit WinApi.Evntrace;

  {$HPPEMIT '#include "evntrace.h"'}

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.Wmistr,
  Winapi.evntprov,
  Winapi.Evntcons;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  // EventTraceGuid is used to identify a event tracing session
  // ==========================================================
  // 68fdd900-4a3e-11d1-84f4-0000f80464e3
  EventTraceGuid  :  TGUID = (D1: $68fdd900; D2: $4a3e; D3: $11d1; D4: ($84, $f4, $00, $00, $f8, $04, $64, $e3));
  {$EXTERNALSYM EventTraceGuid}

  // SystemTraceControlGuid. Used to specify event tracing for kernel
  // ================================================================
  // 9e814aad-3204-11d2-9a82-006008a86939
  SystemTraceControlGuid  :  TGUID = (D1: $9e814aad; D2: $3204; D3: $11d2; D4: ($9a, $82, $00, $60, $08, $a8, $69, $39));
  {$EXTERNALSYM SystemTraceControlGuid}

  // EventTraceConfigGuid. Used to report system configuration records
  // =================================================================
  // 01853a65-418f-4f36-aefc-dc0f1d2fd235
  EventTraceConfigGuid  :  TGUID = (D1: $01853a65; D2: $418f; D3: $4f36; D4: ($ae, $fc, $dc, $0f, $1d, $2f, $d2, $35));
  {$EXTERNALSYM EventTraceConfigGuid}

  // DefaultTraceSecurityGuid. Specifies the default event tracing security
  // ======================================================================
  // 0811c1af-7a07-4a06-82ed-869455cdf713
  DefaultTraceSecurityGuid  :  TGUID = (D1: $0811c1af; D2: $7a07; D3: $4a06; D4: ($82, $ed, $86, $94, $55, $cd, $f7, $13));
  {$EXTERNALSYM DefaultTraceSecurityGuid}

  // PrivateLoggerNotificationGuid. Used for private cross-process logger notifications.
  // ===================================================================================
  PrivateLoggerNotificationGuid : TGUID = (D1: $3595ab5c;
  {$EXTERNALSYM PrivateLoggerNotificationGuid}
                                           D2: $042a;
                                           D3: $4c8e;
                                           D4: ($b9, $42, $2d, $05, $9b, $fe, $b1, $b1));


  KERNEL_LOGGER_NAMEW                 = 'NT Kernel Logger';
  {$EXTERNALSYM KERNEL_LOGGER_NAMEW}
  GLOBAL_LOGGER_NAMEW                 = 'GlobalLogger';
  {$EXTERNALSYM GLOBAL_LOGGER_NAMEW}
  EVENT_LOGGER_NAMEW                  = 'EventLog';
  {$EXTERNALSYM EVENT_LOGGER_NAMEW}
  DIAG_LOGGER_NAMEW                   = 'DiagLog';
  {$EXTERNALSYM DIAG_LOGGER_NAMEW}

  KERNEL_LOGGER_NAMEA                 = 'NT Kernel Logger';
  {$EXTERNALSYM KERNEL_LOGGER_NAMEA}
  GLOBAL_LOGGER_NAMEA                 = 'GlobalLogger';
  {$EXTERNALSYM GLOBAL_LOGGER_NAMEA}
  EVENT_LOGGER_NAMEA                  = 'EventLog';
  {$EXTERNALSYM EVENT_LOGGER_NAMEA}
  DIAG_LOGGER_NAMEA                   = 'DiagLog';
  {$EXTERNALSYM DIAG_LOGGER_NAMEA}

  MAX_MOF_FIELDS                      = 16;  // Limit of USE_MOF_PTR fields
  {$EXTERNALSYM MAX_MOF_FIELDS}

type
{$IFNDEF _TRACEHANDLE_DEFINED}
  PTRACEHANDLE = ^ULONG64;
  TRACEHANDLE = ULONG64;
  {$EXTERNALSYM TRACEHANDLE}
{$DEFINE _TRACEHANDLE_DEFINED}
{$ENDIF}

const
  // types for event data going to System Event Logger

  SYSTEM_EVENT_TYPE                   = 1;
  {$EXTERNALSYM SYSTEM_EVENT_TYPE}


  // predefined generic event types ($00 to $09 reserved).
  // =======================================================

  EVENT_TRACE_TYPE_INFO               = $00;  // Info or point event
  {$EXTERNALSYM EVENT_TRACE_TYPE_INFO}
  EVENT_TRACE_TYPE_START              = $01;  // Start event
  {$EXTERNALSYM EVENT_TRACE_TYPE_START}
  EVENT_TRACE_TYPE_END                = $02;  // End event
  {$EXTERNALSYM EVENT_TRACE_TYPE_END}
  EVENT_TRACE_TYPE_STOP               = $02;  // Stop event (WinEvent compatible)
  {$EXTERNALSYM EVENT_TRACE_TYPE_STOP}
  EVENT_TRACE_TYPE_DC_START           = $03;  // Collection start marker
  {$EXTERNALSYM EVENT_TRACE_TYPE_DC_START}
  EVENT_TRACE_TYPE_DC_END             = $04;  // Collection end marker
  {$EXTERNALSYM EVENT_TRACE_TYPE_DC_END}
  EVENT_TRACE_TYPE_EXTENSION          = $05;  // Extension/continuation
  {$EXTERNALSYM EVENT_TRACE_TYPE_EXTENSION}
  EVENT_TRACE_TYPE_REPLY              = $06;  // Reply event
  {$EXTERNALSYM EVENT_TRACE_TYPE_REPLY}
  EVENT_TRACE_TYPE_DEQUEUE            = $07;  // De-queue event
  {$EXTERNALSYM EVENT_TRACE_TYPE_DEQUEUE}
  EVENT_TRACE_TYPE_RESUME             = $07;  // Resume event (WinEvent compatible)
  {$EXTERNALSYM EVENT_TRACE_TYPE_RESUME}
  EVENT_TRACE_TYPE_CHECKPOINT         = $08;  // Generic checkpoint event
  {$EXTERNALSYM EVENT_TRACE_TYPE_CHECKPOINT}
  EVENT_TRACE_TYPE_SUSPEND            = $08;  // Suspend event (WinEvent compatible)
  {$EXTERNALSYM EVENT_TRACE_TYPE_SUSPEND}
  EVENT_TRACE_TYPE_WINEVT_SEND        = $09;  // Send Event (WinEvent compatible)
  {$EXTERNALSYM EVENT_TRACE_TYPE_WINEVT_SEND}
  EVENT_TRACE_TYPE_WINEVT_RECEIVE     = $F0;  // Receive Event (WinEvent compatible)
  {$EXTERNALSYM EVENT_TRACE_TYPE_WINEVT_RECEIVE}


  // Predefined Event Tracing Levels for Software/Debug Tracing
  //
  //
  // Trace Level is UCHAR and passed in through the EnableLevel parameter
  // in EnableTrace API. It is retrieved by the provider using the
  // GetTraceEnableLevel macro.It should be interpreted as an integer value
  // to mean everything at or below that level will be traced.

  // Here are the possible Levels.
  // =============================

  TRACE_LEVEL_NONE                    = 0;  // Tracing is not on
  {$EXTERNALSYM TRACE_LEVEL_NONE}
  TRACE_LEVEL_CRITICAL                = 1;  // Abnormal exit or termination
  {$EXTERNALSYM TRACE_LEVEL_CRITICAL}
  TRACE_LEVEL_FATAL                   = 1;  // Deprecated name for Abnormal exit or termination
  {$EXTERNALSYM TRACE_LEVEL_FATAL}
  TRACE_LEVEL_ERROR                   = 2;  // Severe errors that need logging
  {$EXTERNALSYM TRACE_LEVEL_ERROR}
  TRACE_LEVEL_WARNING                 = 3;  // Warnings such as allocation failure
  {$EXTERNALSYM TRACE_LEVEL_WARNING}
  TRACE_LEVEL_INFORMATION             = 4;  // Includes non-error cases(e.g.,Entry-Exit)
  {$EXTERNALSYM TRACE_LEVEL_INFORMATION}
  TRACE_LEVEL_VERBOSE                 = 5;  // Detailed traces from intermediate steps
  {$EXTERNALSYM TRACE_LEVEL_VERBOSE}
  TRACE_LEVEL_RESERVED6               = 6;
  {$EXTERNALSYM TRACE_LEVEL_RESERVED6}
  TRACE_LEVEL_RESERVED7               = 7;
  {$EXTERNALSYM TRACE_LEVEL_RESERVED7}
  TRACE_LEVEL_RESERVED8               = 8;
  {$EXTERNALSYM TRACE_LEVEL_RESERVED8}
  TRACE_LEVEL_RESERVED9               = 9;
  {$EXTERNALSYM TRACE_LEVEL_RESERVED9}



  // Event types for Process & Threads
  // ==================================

  EVENT_TRACE_TYPE_LOAD               = $0A;  // Load image
  {$EXTERNALSYM EVENT_TRACE_TYPE_LOAD}
  EVENT_TRACE_TYPE_TERMINATE          = $0B;  // Terminate Process
  {$EXTERNALSYM EVENT_TRACE_TYPE_TERMINATE}


  // Event types for IO subsystem
  // ============================

  EVENT_TRACE_TYPE_IO_READ            = $0A;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_READ}
  EVENT_TRACE_TYPE_IO_WRITE           = $0B;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_WRITE}
  EVENT_TRACE_TYPE_IO_READ_INIT       = $0C;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_READ_INIT}
  EVENT_TRACE_TYPE_IO_WRITE_INIT      = $0D;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_WRITE_INIT}
  EVENT_TRACE_TYPE_IO_FLUSH           = $0E;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_FLUSH}
  EVENT_TRACE_TYPE_IO_FLUSH_INIT      = $0F;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_FLUSH_INIT}
  EVENT_TRACE_TYPE_IO_REDIRECTED_INIT = $10;
  {$EXTERNALSYM EVENT_TRACE_TYPE_IO_REDIRECTED_INIT}


  // Event types for Memory subsystem
  // ================================

  EVENT_TRACE_TYPE_MM_TF              = $0A;  // Transition fault
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_TF}
  EVENT_TRACE_TYPE_MM_DZF             = $0B;  // Demand Zero fault
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_DZF}
  EVENT_TRACE_TYPE_MM_COW             = $0C;  // Copy on Write
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_COW}
  EVENT_TRACE_TYPE_MM_GPF             = $0D;  // Guard Page fault
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_GPF}
  EVENT_TRACE_TYPE_MM_HPF             = $0E;  // Hard page fault
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_HPF}
  EVENT_TRACE_TYPE_MM_AV              = $0F;  // Access violation
  {$EXTERNALSYM EVENT_TRACE_TYPE_MM_AV}


  // Event types for Network subsystem, all protocols
  // ================================================

  EVENT_TRACE_TYPE_SEND               = $0A;  // Send
  {$EXTERNALSYM EVENT_TRACE_TYPE_SEND}
  EVENT_TRACE_TYPE_RECEIVE            = $0B;  // Receive
  {$EXTERNALSYM EVENT_TRACE_TYPE_RECEIVE}
  EVENT_TRACE_TYPE_CONNECT            = $0C;  // Connect
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONNECT}
  EVENT_TRACE_TYPE_DISCONNECT         = $0D;  // Disconnect
  {$EXTERNALSYM EVENT_TRACE_TYPE_DISCONNECT}
  EVENT_TRACE_TYPE_RETRANSMIT         = $0E;  // ReTransmit
  {$EXTERNALSYM EVENT_TRACE_TYPE_RETRANSMIT}
  EVENT_TRACE_TYPE_ACCEPT             = $0F;  // Accept
  {$EXTERNALSYM EVENT_TRACE_TYPE_ACCEPT}
  EVENT_TRACE_TYPE_RECONNECT          = $10;  // ReConnect
  {$EXTERNALSYM EVENT_TRACE_TYPE_RECONNECT}
  EVENT_TRACE_TYPE_CONNFAIL           = $11;  // Fail
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONNFAIL}
  EVENT_TRACE_TYPE_COPY_TCP           = $12;  // Copy in PendData
  {$EXTERNALSYM EVENT_TRACE_TYPE_COPY_TCP}
  EVENT_TRACE_TYPE_COPY_ARP           = $13;  // NDIS_STATUS_RESOURCES Copy
  {$EXTERNALSYM EVENT_TRACE_TYPE_COPY_ARP}
  EVENT_TRACE_TYPE_ACKFULL            = $14;  // A full data ACK
  {$EXTERNALSYM EVENT_TRACE_TYPE_ACKFULL}
  EVENT_TRACE_TYPE_ACKPART            = $15;  // A Partial data ACK
  {$EXTERNALSYM EVENT_TRACE_TYPE_ACKPART}
  EVENT_TRACE_TYPE_ACKDUP             = $16;  // A Duplicate data ACK
  {$EXTERNALSYM EVENT_TRACE_TYPE_ACKDUP}


  // Event Types for the Header (to handle internal event headers)
  // =============================================================

  EVENT_TRACE_TYPE_GUIDMAP            = $0A;
  {$EXTERNALSYM EVENT_TRACE_TYPE_GUIDMAP}
  EVENT_TRACE_TYPE_CONFIG             = $0B;
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG}
  EVENT_TRACE_TYPE_SIDINFO            = $0C;
  {$EXTERNALSYM EVENT_TRACE_TYPE_SIDINFO}
  EVENT_TRACE_TYPE_SECURITY           = $0D;
  {$EXTERNALSYM EVENT_TRACE_TYPE_SECURITY}
  EVENT_TRACE_TYPE_DBGID_RSDS         = $40;
  {$EXTERNALSYM EVENT_TRACE_TYPE_DBGID_RSDS}


  // Event Types for Registry subsystem
  // ==================================

  EVENT_TRACE_TYPE_REGCREATE          = $0A;  // NtCreateKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGCREATE}
  EVENT_TRACE_TYPE_REGOPEN            = $0B;  // NtOpenKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGOPEN}
  EVENT_TRACE_TYPE_REGDELETE          = $0C;  // NtDeleteKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGDELETE}
  EVENT_TRACE_TYPE_REGQUERY           = $0D;  // NtQueryKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGQUERY}
  EVENT_TRACE_TYPE_REGSETVALUE        = $0E;  // NtSetValueKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGSETVALUE}
  EVENT_TRACE_TYPE_REGDELETEVALUE     = $0F;  // NtDeleteValueKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGDELETEVALUE}
  EVENT_TRACE_TYPE_REGQUERYVALUE      = $10;  // NtQueryValueKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGQUERYVALUE}
  EVENT_TRACE_TYPE_REGENUMERATEKEY    = $11;  // NtEnumerateKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGENUMERATEKEY}
  EVENT_TRACE_TYPE_REGENUMERATEVALUEKEY= $12;  // NtEnumerateValueKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGENUMERATEVALUEKEY}
  EVENT_TRACE_TYPE_REGQUERYMULTIPLEVALUE= $13;  // NtQueryMultipleValueKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGQUERYMULTIPLEVALUE}
  EVENT_TRACE_TYPE_REGSETINFORMATION  = $14;  // NtSetInformationKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGSETINFORMATION}
  EVENT_TRACE_TYPE_REGFLUSH           = $15;  // NtFlushKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGFLUSH}
  EVENT_TRACE_TYPE_REGKCBCREATE       = $16;  // KcbCreate
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGKCBCREATE}
  EVENT_TRACE_TYPE_REGKCBDELETE       = $17;  // KcbDelete
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGKCBDELETE}
  EVENT_TRACE_TYPE_REGKCBRUNDOWNBEGIN = $18;  // KcbRundownBegin
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGKCBRUNDOWNBEGIN}
  EVENT_TRACE_TYPE_REGKCBRUNDOWNEND   = $19;  // KcbRundownEnd
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGKCBRUNDOWNEND}
  EVENT_TRACE_TYPE_REGVIRTUALIZE      = $1A;  // VirtualizeKey
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGVIRTUALIZE}
  EVENT_TRACE_TYPE_REGCLOSE           = $1B;  // NtClose (KeyObject)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGCLOSE}
  EVENT_TRACE_TYPE_REGSETSECURITY     = $1C;  // SetSecurityDescriptor (KeyObject)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGSETSECURITY}
  EVENT_TRACE_TYPE_REGQUERYSECURITY   = $1D;  // QuerySecurityDescriptor (KeyObject)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGQUERYSECURITY}
  EVENT_TRACE_TYPE_REGCOMMIT          = $1E;  // CmKtmNotification (TRANSACTION_NOTIFY_COMMIT)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGCOMMIT}
  EVENT_TRACE_TYPE_REGPREPARE         = $1F;  // CmKtmNotification (TRANSACTION_NOTIFY_PREPARE)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGPREPARE}
  EVENT_TRACE_TYPE_REGROLLBACK        = $20;  // CmKtmNotification (TRANSACTION_NOTIFY_ROLLBACK)
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGROLLBACK}
  EVENT_TRACE_TYPE_REGMOUNTHIVE       = $21;  // NtLoadKey variations + system hives
  {$EXTERNALSYM EVENT_TRACE_TYPE_REGMOUNTHIVE}


  // Event types for system configuration records
  // ============================================

  EVENT_TRACE_TYPE_CONFIG_CPU          = $0A;  // CPU Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_CPU}
  EVENT_TRACE_TYPE_CONFIG_PHYSICALDISK = $0B;  // Physical Disk Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PHYSICALDISK}
  EVENT_TRACE_TYPE_CONFIG_LOGICALDISK  = $0C;  // Logical Disk Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_LOGICALDISK}
  EVENT_TRACE_TYPE_CONFIG_NIC          = $0D;  // NIC Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_NIC}
  EVENT_TRACE_TYPE_CONFIG_VIDEO        = $0E;  // Video Adapter Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_VIDEO}
  EVENT_TRACE_TYPE_CONFIG_SERVICES     = $0F;  // Active Services
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_SERVICES}
  EVENT_TRACE_TYPE_CONFIG_POWER        = $10;  // ACPI Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_POWER}
  EVENT_TRACE_TYPE_CONFIG_NETINFO      = $11;  // Networking Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_NETINFO}
  EVENT_TRACE_TYPE_CONFIG_OPTICALMEDIA = $12;  // Optical Media Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_OPTICALMEDIA}

  EVENT_TRACE_TYPE_CONFIG_IRQ             = $15;  // IRQ assigned to devices
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_IRQ}
  EVENT_TRACE_TYPE_CONFIG_PNP             = $16;  // PnP device info
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PNP}
  EVENT_TRACE_TYPE_CONFIG_IDECHANNEL      = $17;  // Primary/Secondary IDE channel Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_IDECHANNEL}
  EVENT_TRACE_TYPE_CONFIG_NUMANODE        = $18;  // Numa configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_NUMANODE}
  EVENT_TRACE_TYPE_CONFIG_PLATFORM        = $19;  // Platform Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PLATFORM}
  EVENT_TRACE_TYPE_CONFIG_PROCESSORGROUP  = $1A;  // Processor Group Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PROCESSORGROUP}
  EVENT_TRACE_TYPE_CONFIG_PROCESSORNUMBER = $1B;  // ProcessorIndex -> ProcNumber mapping
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PROCESSORNUMBER}
  EVENT_TRACE_TYPE_CONFIG_DPI             = $1C;  // Display DPI Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_DPI}
  EVENT_TRACE_TYPE_CONFIG_CI_INFO         = $1D;  // Display System Code Integrity Information
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_CI_INFO}
  EVENT_TRACE_TYPE_CONFIG_MACHINEID       = $1E;  // SQM Machine Id
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_MACHINEID}
  EVENT_TRACE_TYPE_CONFIG_DEFRAG          = $1F;  // Logical Disk Defragmenter Information
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_DEFRAG}
  EVENT_TRACE_TYPE_CONFIG_MOBILEPLATFORM  = $20;  // Mobile Platform Configuration
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_MOBILEPLATFORM}
  EVENT_TRACE_TYPE_CONFIG_DEVICEFAMILY    = $21;  // Device Family Information
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_DEVICEFAMILY}
  EVENT_TRACE_TYPE_CONFIG_FLIGHTID        = $22;  // Flights on the machine
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_FLIGHTID}
  EVENT_TRACE_TYPE_CONFIG_PROCESSOR       = $23;  // CentralProcessor records
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_PROCESSOR}
  EVENT_TRACE_TYPE_CONFIG_VIRTUALIZATION  = $24;  // virtualization config info
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_VIRTUALIZATION}
  EVENT_TRACE_TYPE_CONFIG_BOOT            = $25;  // boot config info
  {$EXTERNALSYM EVENT_TRACE_TYPE_CONFIG_BOOT}


  // Event types for Optical IO subsystem
  // ====================================

  EVENT_TRACE_TYPE_OPTICAL_IO_READ       = $37;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_READ}
  EVENT_TRACE_TYPE_OPTICAL_IO_WRITE      = $38;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_WRITE}
  EVENT_TRACE_TYPE_OPTICAL_IO_FLUSH      = $39;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_FLUSH}
  EVENT_TRACE_TYPE_OPTICAL_IO_READ_INIT  = $3A;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_READ_INIT}
  EVENT_TRACE_TYPE_OPTICAL_IO_WRITE_INIT = $3B;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_WRITE_INIT}
  EVENT_TRACE_TYPE_OPTICAL_IO_FLUSH_INIT = $3C;
  {$EXTERNALSYM EVENT_TRACE_TYPE_OPTICAL_IO_FLUSH_INIT}


  // Event types for Filter Manager
  // ==============================

  EVENT_TRACE_TYPE_FLT_PREOP_INIT        = $60;  // Minifilter preop initiation
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_PREOP_INIT}
  EVENT_TRACE_TYPE_FLT_POSTOP_INIT       = $61;  // Minifilter postop initiation
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_POSTOP_INIT}
  EVENT_TRACE_TYPE_FLT_PREOP_COMPLETION  = $62;  // Minifilter preop completion
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_PREOP_COMPLETION}
  EVENT_TRACE_TYPE_FLT_POSTOP_COMPLETION = $63;  // Minifilter postop completion
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_POSTOP_COMPLETION}
  EVENT_TRACE_TYPE_FLT_PREOP_FAILURE     = $64;  // Minifilter failed preop
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_PREOP_FAILURE}
  EVENT_TRACE_TYPE_FLT_POSTOP_FAILURE    = $65;  // Minifilter failed postop
  {$EXTERNALSYM EVENT_TRACE_TYPE_FLT_POSTOP_FAILURE}


  // Enable flags for Kernel Events
  // ==============================

  EVENT_TRACE_FLAG_PROCESS            = $00000001;  // process start & end
  {$EXTERNALSYM EVENT_TRACE_FLAG_PROCESS}
  EVENT_TRACE_FLAG_THREAD             = $00000002;  // thread start & end
  {$EXTERNALSYM EVENT_TRACE_FLAG_THREAD}
  EVENT_TRACE_FLAG_IMAGE_LOAD         = $00000004;  // image load
  {$EXTERNALSYM EVENT_TRACE_FLAG_IMAGE_LOAD}

  EVENT_TRACE_FLAG_DISK_IO            = $00000100;  // physical disk IO
  {$EXTERNALSYM EVENT_TRACE_FLAG_DISK_IO}
  EVENT_TRACE_FLAG_DISK_FILE_IO       = $00000200;  // requires disk IO
  {$EXTERNALSYM EVENT_TRACE_FLAG_DISK_FILE_IO}

  EVENT_TRACE_FLAG_MEMORY_PAGE_FAULTS = $00001000;  // all page faults
  {$EXTERNALSYM EVENT_TRACE_FLAG_MEMORY_PAGE_FAULTS}
  EVENT_TRACE_FLAG_MEMORY_HARD_FAULTS = $00002000;  // hard faults only
  {$EXTERNALSYM EVENT_TRACE_FLAG_MEMORY_HARD_FAULTS}

  EVENT_TRACE_FLAG_NETWORK_TCPIP      = $00010000;  // tcpip send & receive
  {$EXTERNALSYM EVENT_TRACE_FLAG_NETWORK_TCPIP}

  EVENT_TRACE_FLAG_REGISTRY           = $00020000;  // registry calls
  {$EXTERNALSYM EVENT_TRACE_FLAG_REGISTRY}
  EVENT_TRACE_FLAG_DBGPRINT           = $00040000;  // DbgPrint(ex) Calls
  {$EXTERNALSYM EVENT_TRACE_FLAG_DBGPRINT}


  // Enable flags for Kernel Events on Vista and above
  // =================================================

  EVENT_TRACE_FLAG_PROCESS_COUNTERS   = $00000008;  // process perf counters
  {$EXTERNALSYM EVENT_TRACE_FLAG_PROCESS_COUNTERS}
  EVENT_TRACE_FLAG_CSWITCH            = $00000010;  // context switches
  {$EXTERNALSYM EVENT_TRACE_FLAG_CSWITCH}
  EVENT_TRACE_FLAG_DPC                = $00000020;  // deferred procedure calls
  EVENT_TRACE_FLAG_INTERRUPT          = $00000040;  // interrupts
  {$EXTERNALSYM EVENT_TRACE_FLAG_INTERRUPT}
  EVENT_TRACE_FLAG_SYSTEMCALL         = $00000080;  // system calls
  {$EXTERNALSYM EVENT_TRACE_FLAG_SYSTEMCALL}

  EVENT_TRACE_FLAG_DISK_IO_INIT       = $00000400;  // physical disk IO initiation
  {$EXTERNALSYM EVENT_TRACE_FLAG_DISK_IO_INIT}
  EVENT_TRACE_FLAG_ALPC               = $00100000;  // ALPC traces
  {$EXTERNALSYM EVENT_TRACE_FLAG_ALPC}
  EVENT_TRACE_FLAG_SPLIT_IO           = $00200000;  // split io traces (VolumeManager)
  {$EXTERNALSYM EVENT_TRACE_FLAG_SPLIT_IO}

  EVENT_TRACE_FLAG_DRIVER             = $00800000;  // driver delays
  {$EXTERNALSYM EVENT_TRACE_FLAG_DRIVER}
  EVENT_TRACE_FLAG_PROFILE            = $01000000;  // sample based profiling
  {$EXTERNALSYM EVENT_TRACE_FLAG_PROFILE}
  EVENT_TRACE_FLAG_FILE_IO            = $02000000;  // file IO
  {$EXTERNALSYM EVENT_TRACE_FLAG_FILE_IO}
  EVENT_TRACE_FLAG_FILE_IO_INIT       = $04000000;  // file IO initiation
  {$EXTERNALSYM EVENT_TRACE_FLAG_FILE_IO_INIT}


  // Enable flags for Kernel Events on Win7 and above
  // ================================================

  EVENT_TRACE_FLAG_DISPATCHER         = $00000800;  // scheduler (ReadyThread)
  {$EXTERNALSYM EVENT_TRACE_FLAG_DISPATCHER}
  EVENT_TRACE_FLAG_VIRTUAL_ALLOC      = $00004000;  // VM operations
  {$EXTERNALSYM EVENT_TRACE_FLAG_VIRTUAL_ALLOC}


  // Enable flags for Kernel Events on Win8 and above
  // ================================================

  EVENT_TRACE_FLAG_VAMAP              = $00008000;  // map/unmap (excluding images)
  {$EXTERNALSYM EVENT_TRACE_FLAG_VAMAP}
  EVENT_TRACE_FLAG_NO_SYSCONFIG       = $10000000;  // Do not do sys config rundown
  {$EXTERNALSYM EVENT_TRACE_FLAG_NO_SYSCONFIG}


  // Enable flags for Kernel Events on Threshold and above
  // =====================================================

  EVENT_TRACE_FLAG_JOB                = $00080000;  // job start & end
  {$EXTERNALSYM EVENT_TRACE_FLAG_JOB}
  EVENT_TRACE_FLAG_DEBUG_EVENTS       = $00400000;  // debugger events (break/continue/...)
  {$EXTERNALSYM EVENT_TRACE_FLAG_DEBUG_EVENTS}


  // Pre-defined Enable flags for everybody else
  // ===========================================

  EVENT_TRACE_FLAG_EXTENSION          = $80000000;  // Indicates more flags
  {$EXTERNALSYM EVENT_TRACE_FLAG_EXTENSION}
  EVENT_TRACE_FLAG_FORWARD_WMI        = $40000000;  // Can forward to WMI
  {$EXTERNALSYM EVENT_TRACE_FLAG_FORWARD_WMI}
  EVENT_TRACE_FLAG_ENABLE_RESERVE     = $20000000;  // Reserved
  {$EXTERNALSYM EVENT_TRACE_FLAG_ENABLE_RESERVE}


  // Logger Mode flags
  // =================
  EVENT_TRACE_FILE_MODE_NONE          = $00000000;  // Logfile is off
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_NONE}
  EVENT_TRACE_FILE_MODE_SEQUENTIAL    = $00000001;  // Log sequentially
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_SEQUENTIAL}
  EVENT_TRACE_FILE_MODE_CIRCULAR      = $00000002;  // Log in circular manner
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_CIRCULAR}
  EVENT_TRACE_FILE_MODE_APPEND        = $00000004;  // Append sequential log
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_APPEND}

  EVENT_TRACE_REAL_TIME_MODE          = $00000100;  // Real time mode on
  {$EXTERNALSYM EVENT_TRACE_REAL_TIME_MODE}
  EVENT_TRACE_DELAY_OPEN_FILE_MODE    = $00000200;  // Delay opening file
  {$EXTERNALSYM EVENT_TRACE_DELAY_OPEN_FILE_MODE}
  EVENT_TRACE_BUFFERING_MODE          = $00000400;  // Buffering mode only
  {$EXTERNALSYM EVENT_TRACE_BUFFERING_MODE}
  EVENT_TRACE_PRIVATE_LOGGER_MODE     = $00000800;  // Process Private Logger
  {$EXTERNALSYM EVENT_TRACE_PRIVATE_LOGGER_MODE}
  EVENT_TRACE_ADD_HEADER_MODE         = $00001000;  // Add a logfile header
  {$EXTERNALSYM EVENT_TRACE_ADD_HEADER_MODE}

  EVENT_TRACE_USE_GLOBAL_SEQUENCE     = $00004000;  // Use global sequence no.
  {$EXTERNALSYM EVENT_TRACE_USE_GLOBAL_SEQUENCE}
  EVENT_TRACE_USE_LOCAL_SEQUENCE      = $00008000;  // Use local sequence no.
  {$EXTERNALSYM EVENT_TRACE_USE_LOCAL_SEQUENCE}

  EVENT_TRACE_RELOG_MODE              = $00010000;  // Relogger
  {$EXTERNALSYM EVENT_TRACE_RELOG_MODE}

  EVENT_TRACE_USE_PAGED_MEMORY        = $01000000;  // Use pageable buffers
  {$EXTERNALSYM EVENT_TRACE_USE_PAGED_MEMORY}


  // Logger Mode flags on XP and above
  // =================================

  EVENT_TRACE_FILE_MODE_NEWFILE       = $00000008;  // Auto-switch log file
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_NEWFILE}
  EVENT_TRACE_FILE_MODE_PREALLOCATE   = $00000020;  // Pre-allocate mode
  {$EXTERNALSYM EVENT_TRACE_FILE_MODE_PREALLOCATE}


  // Logger Mode flags on Vista and above
  // ====================================

  EVENT_TRACE_NONSTOPPABLE_MODE       = $00000040;  // Session cannot be stopped (Autologger only)
  {$EXTERNALSYM EVENT_TRACE_NONSTOPPABLE_MODE}
  EVENT_TRACE_SECURE_MODE             = $00000080;  // Secure session
  {$EXTERNALSYM EVENT_TRACE_SECURE_MODE}
  EVENT_TRACE_USE_KBYTES_FOR_SIZE     = $00002000;  // Use KBytes as file size unit
  {$EXTERNALSYM EVENT_TRACE_USE_KBYTES_FOR_SIZE}
  EVENT_TRACE_PRIVATE_IN_PROC         = $00020000;  // In process private logger
  {$EXTERNALSYM EVENT_TRACE_PRIVATE_IN_PROC}

  EVENT_TRACE_MODE_RESERVED           = $00100000;  // Reserved bit, used to signal Heap/Critsec tracing
  {$EXTERNALSYM EVENT_TRACE_MODE_RESERVED}


  // Logger Mode flags on Win7 and above
  // ===================================

  EVENT_TRACE_NO_PER_PROCESSOR_BUFFERING= $10000000;  // Use this for low frequency sessions.
  {$EXTERNALSYM EVENT_TRACE_NO_PER_PROCESSOR_BUFFERING}


  // Logger Mode flags on Win8 and above
  // ===================================

  EVENT_TRACE_SYSTEM_LOGGER_MODE         = $02000000;  // Receive events from SystemTraceProvider
  {$EXTERNALSYM EVENT_TRACE_SYSTEM_LOGGER_MODE}
  EVENT_TRACE_ADDTO_TRIAGE_DUMP          = $80000000;  // Add ETW buffers to triage dumps
  {$EXTERNALSYM EVENT_TRACE_ADDTO_TRIAGE_DUMP}
  EVENT_TRACE_STOP_ON_HYBRID_SHUTDOWN    = $00400000;  // Stop on hybrid shutdown
  {$EXTERNALSYM EVENT_TRACE_STOP_ON_HYBRID_SHUTDOWN}
  EVENT_TRACE_PERSIST_ON_HYBRID_SHUTDOWN = $00800000;  // Persist on hybrid shutdown
  {$EXTERNALSYM EVENT_TRACE_PERSIST_ON_HYBRID_SHUTDOWN}


  // Logger Mode flags on Blue and above
  // ===================================

  EVENT_TRACE_INDEPENDENT_SESSION_MODE   = $08000000;  // Independent logger session
  {$EXTERNALSYM EVENT_TRACE_INDEPENDENT_SESSION_MODE}


  // Logger Mode flags on Redstone and above
  // =======================================

  EVENT_TRACE_COMPRESSED_MODE            = $04000000;  // Compressed logger session.
  {$EXTERNALSYM EVENT_TRACE_COMPRESSED_MODE}


  // ControlTrace Codes
  // ==================

  EVENT_TRACE_CONTROL_QUERY               = 0;
  {$EXTERNALSYM EVENT_TRACE_CONTROL_QUERY}
  EVENT_TRACE_CONTROL_STOP                = 1;
  {$EXTERNALSYM EVENT_TRACE_CONTROL_STOP}
  EVENT_TRACE_CONTROL_UPDATE              = 2;
  {$EXTERNALSYM EVENT_TRACE_CONTROL_UPDATE}


  // Flush ControlTrace Codes for XP and above
  // =========================================

  EVENT_TRACE_CONTROL_FLUSH               = 3;  // Flushes all the buffers
  {$EXTERNALSYM EVENT_TRACE_CONTROL_FLUSH}

  //
  // Supported for RS5 and above.
  //

  EVENT_TRACE_CONTROL_INCREMENT_FILE      = 4;  // Causes a session with EVENT_TRACE_FILE_MODE_NEWFILE
                                                // to switch to the next file before the automatic
                                                // switching criteria is met
  {$EXTERNALSYM EVENT_TRACE_CONTROL_INCREMENT_FILE}

  //
  // Supported for Manganese and above.
  //

  EVENT_TRACE_CONTROL_CONVERT_TO_REALTIME = 5;  // Transitions from file mode tracing to real-time.
  {$EXTERNALSYM EVENT_TRACE_CONTROL_CONVERT_TO_REALTIME}


  // Flags used by WMI Trace Message
  // Note that the order or value of these flags should NOT be changed as they are processed
  // in this order.
  // =======================================================================================

  TRACE_MESSAGE_SEQUENCE              = 1;  // Message should include a sequence number
  {$EXTERNALSYM TRACE_MESSAGE_SEQUENCE}
  TRACE_MESSAGE_GUID                  = 2;  // Message includes a GUID
  {$EXTERNALSYM TRACE_MESSAGE_GUID}
  TRACE_MESSAGE_COMPONENTID           = 4;  // Message has no GUID, Component ID instead
  {$EXTERNALSYM TRACE_MESSAGE_COMPONENTID}
  TRACE_MESSAGE_TIMESTAMP             = 8;  // Message includes a timestamp
  {$EXTERNALSYM TRACE_MESSAGE_TIMESTAMP}
  TRACE_MESSAGE_PERFORMANCE_TIMESTAMP = 16;  // *Obsolete* Clock type is controlled by the logger
  {$EXTERNALSYM TRACE_MESSAGE_PERFORMANCE_TIMESTAMP}
  TRACE_MESSAGE_SYSTEMINFO            = 32;  // Message includes system information TID,PID
  {$EXTERNALSYM TRACE_MESSAGE_SYSTEMINFO}


  // Vista flags set by system to indicate provider pointer size.
  // ============================================================

  TRACE_MESSAGE_POINTER32             = $0040;  // Message logged by 32 bit provider
  {$EXTERNALSYM TRACE_MESSAGE_POINTER32}
  TRACE_MESSAGE_POINTER64             = $0080;  // Message logged by 64 bit provider
  {$EXTERNALSYM TRACE_MESSAGE_POINTER64}

  TRACE_MESSAGE_FLAG_MASK             = $FFFF;  // Only the lower 16 bits of flags are placed in the message
  {$EXTERNALSYM TRACE_MESSAGE_FLAG_MASK}
                                                 // those above 16 bits are reserved for local processing

  // Maximum size allowed for a single TraceMessage message.
  //
  // N.B. This limit was increased from 8K to 64K in Win8.
  // =======================================================

  {$EXTERNALSYM EVENT_TRACE_FLAG_DPC}
  {$EXTERNALSYM TRACE_MESSAGE_MAXIMUM_SIZE}
  TRACE_MESSAGE_MAXIMUM_SIZE          = (64 * 1024);


  // Flags to indicate to consumer which fields
  // in the EVENT_TRACE_HEADER are valid
  // ==========================================

  EVENT_TRACE_USE_PROCTIME            = $0001;  // ProcessorTime field is valid
  {$EXTERNALSYM EVENT_TRACE_USE_PROCTIME}
  EVENT_TRACE_USE_NOCPUTIME           = $0002;  // No Kernel/User/Processor Times
  {$EXTERNALSYM EVENT_TRACE_USE_NOCPUTIME}


  // TRACE_HEADER_FLAG values are used in the Flags field of EVENT_TRACE_HEADER
  // structure while calling into TraceEvent API
  // ==========================================================================

  TRACE_HEADER_FLAG_USE_TIMESTAMP     = $00000200;
  {$EXTERNALSYM TRACE_HEADER_FLAG_USE_TIMESTAMP}
  TRACE_HEADER_FLAG_TRACED_GUID       = $00020000;  // denotes a trace
  {$EXTERNALSYM TRACE_HEADER_FLAG_TRACED_GUID}
  TRACE_HEADER_FLAG_LOG_WNODE         = $00040000;  // request to log Wnode
  {$EXTERNALSYM TRACE_HEADER_FLAG_LOG_WNODE}
  TRACE_HEADER_FLAG_USE_GUID_PTR      = $00080000;  // Guid is actually a pointer
  {$EXTERNALSYM TRACE_HEADER_FLAG_USE_GUID_PTR}
  TRACE_HEADER_FLAG_USE_MOF_PTR       = $00100000;  // MOF data are dereferenced
  {$EXTERNALSYM TRACE_HEADER_FLAG_USE_MOF_PTR}


type

  ETW_COMPRESSION_RESUMPTION_MODE = (
    EtwCompressionModeRestart     = 0,
    EtwCompressionModeNoDisable   = 1,
    EtwCompressionModeNoRestart   = 2);
  {$EXTERNALSYM ETW_COMPRESSION_RESUMPTION_MODE}
  EtwCompressionResumptionMode = ETW_COMPRESSION_RESUMPTION_MODE;
  {$EXTERNALSYM EtwCompressionResumptionMode}


  // Trace header for all legacy events.
  // ===================================

  //
  // An EVENT_TRACE consists of a fixed header (EVENT_TRACE_HEADER) and
  // optionally a variable portion pointed to by MofData. The datablock
  // layout of the variable portion is unknown to the Logger and must
  // be obtained from WBEM CIMOM database.
  //
  PEVENT_TRACE_HEADER = ^EVENT_TRACE_HEADER;
  EVENT_TRACE_HEADER = record         // overlays WNODE_HEADER
    Size: USHORT;                     // Size of entire record

    DUMMYUNIONNAME: record
      case integer of
        0: (FieldTypeFlags: USHORT);  // Indicates valid fields
        1: (HeaderType: UCHAR;        // Header type - internal use only
            MarkerFlags: UCHAR);      // Marker - internal use only
      end;

    DUMMYUNIONNAME2: record
      case integer of
        0: (Version: ULONG);
        1: (_Type: UCHAR;             // event type
            Level: UCHAR;             // trace instrumentation level
            _Version: USHORT);        // version of trace record
      end;

    ThreadId: ULONG;                  // Thread Id
    ProcessId: ULONG;                 // Process Id
    TimeStamp: LARGE_INTEGER;         // time when event happens


    DUMMYUNIONNAME3: record
      case integer of
        0: (Guid: TGUID);             // Guid that identifies event
        1: (GuidPtr: ULONGLONG);      // use with WNODE_FLAG_USE_GUID_PTR
      end;

    DUMMYUNIONNAME4: record
      case integer of
        0: (KernelTime: ULONG;              // Kernel Mode CPU ticks
            UserTime: ULONG);               // User mode CPU ticks
        1: (ProcessorTime: ULONG64);        // Processor Clock
        2: (ClientContext: ULONG;           // Reserved
            Flags: ULONG);                  // Event Flags
      end;
  end;
  {$EXTERNALSYM EVENT_TRACE_HEADER}


  // This header is used to trace and track transaction co-relations
  // ===============================================================
  PEVENT_INSTANCE_HEADER = ^EVENT_INSTANCE_HEADER;
  _EVENT_INSTANCE_HEADER = record
    Size : USHORT;
    _FieldTypeFlags: record
    case boolean of
      True:  (FieldTypeFlags : USHORT); // Indicates valid fields
      False: (HeaderType     : UCHAR;   // Header type - internal use only
              MarkerFlags    : UCHAR);  // Marker - internal use only
    end;
    _Version : record
    case Boolean of
      True  : (Version1 : ULONG);
      False : (Type_    : UCHAR;        // event type
               Level    : UCHAR;        // trace instrumentation level
               Version2 : USHORT);      // version of trace record
    end;

    ThreadId         : ULONG;
    ProcessId        : ULONG;
    TimeStamp        : LARGE_INTEGER;
    RegHandle        : ULONGLONG;
    InstanceId       : ULONG;
    ParentInstanceId : ULONG;

    _KernelTime : record
      case Short of
        0 : (KernelTime    : ULONG;    // Kernel Mode CPU ticks
             UserTime      : ULONG);   // User mode CPU ticks
        1 : (ProcessorTime : ULONG64); // Processor Clock
        2 : (ClientContext : ULONG;    // Reserved
             Flags         : ULONG)    // Event Flags
    end;
    ParentRegHandle : ULONGLONG;
  end;
  {$EXTERNALSYM _EVENT_INSTANCE_HEADER}
  EVENT_INSTANCE_HEADER  = _EVENT_INSTANCE_HEADER;
  {$EXTERNALSYM EVENT_INSTANCE_HEADER}


  // Following are structures and macros for use with USE_MOF_PTR
  //=============================================================

const

  // Trace data types
  ETW_NULL_TYPE_VALUE                 = 0;
  {$EXTERNALSYM ETW_NULL_TYPE_VALUE}
  ETW_OBJECT_TYPE_VALUE               = 1;
  {$EXTERNALSYM ETW_OBJECT_TYPE_VALUE}
  ETW_STRING_TYPE_VALUE               = 2;
  {$EXTERNALSYM ETW_STRING_TYPE_VALUE}
  ETW_SBYTE_TYPE_VALUE                = 3;
  {$EXTERNALSYM ETW_SBYTE_TYPE_VALUE}
  ETW_BYTE_TYPE_VALUE                 = 4;
  {$EXTERNALSYM ETW_BYTE_TYPE_VALUE}
  ETW_INT16_TYPE_VALUE                = 5;
  {$EXTERNALSYM ETW_INT16_TYPE_VALUE}
  ETW_UINT16_TYPE_VALUE               = 6;
  {$EXTERNALSYM ETW_UINT16_TYPE_VALUE}
  ETW_INT32_TYPE_VALUE                = 7;
  {$EXTERNALSYM ETW_INT32_TYPE_VALUE}
  ETW_UINT32_TYPE_VALUE               = 8;
  {$EXTERNALSYM ETW_UINT32_TYPE_VALUE}
  ETW_INT64_TYPE_VALUE                = 9;
  {$EXTERNALSYM ETW_INT64_TYPE_VALUE}
  ETW_UINT64_TYPE_VALUE               = 10;
  {$EXTERNALSYM ETW_UINT64_TYPE_VALUE}
  ETW_CHAR_TYPE_VALUE                 = 11;
  {$EXTERNALSYM ETW_CHAR_TYPE_VALUE}
  ETW_SINGLE_TYPE_VALUE               = 12;
  {$EXTERNALSYM ETW_SINGLE_TYPE_VALUE}
  ETW_DOUBLE_TYPE_VALUE               = 13;
  {$EXTERNALSYM ETW_DOUBLE_TYPE_VALUE}
  ETW_BOOLEAN_TYPE_VALUE              = 14;
  {$EXTERNALSYM ETW_BOOLEAN_TYPE_VALUE}
  ETW_DECIMAL_TYPE_VALUE              = 15;
  {$EXTERNALSYM ETW_DECIMAL_TYPE_VALUE}

  // Extended types
  ETW_GUID_TYPE_VALUE                         = 101;
  {$EXTERNALSYM ETW_GUID_TYPE_VALUE}
  ETW_ASCIICHAR_TYPE_VALUE                    = 102;
  {$EXTERNALSYM ETW_ASCIICHAR_TYPE_VALUE}
  ETW_ASCIISTRING_TYPE_VALUE                  = 103;
  {$EXTERNALSYM ETW_ASCIISTRING_TYPE_VALUE}
  ETW_COUNTED_STRING_TYPE_VALUE               = 104;
  {$EXTERNALSYM ETW_COUNTED_STRING_TYPE_VALUE}
  ETW_POINTER_TYPE_VALUE                      = 105;
  {$EXTERNALSYM ETW_POINTER_TYPE_VALUE}
  ETW_SIZET_TYPE_VALUE                        = 106;
  {$EXTERNALSYM ETW_SIZET_TYPE_VALUE}
  ETW_HIDDEN_TYPE_VALUE                       = 107;
  {$EXTERNALSYM ETW_HIDDEN_TYPE_VALUE}
  ETW_BOOL_TYPE_VALUE                         = 108;
  {$EXTERNALSYM ETW_BOOL_TYPE_VALUE}
  ETW_COUNTED_ANSISTRING_TYPE_VALUE           = 109;
  {$EXTERNALSYM ETW_COUNTED_ANSISTRING_TYPE_VALUE}
  ETW_REVERSED_COUNTED_STRING_TYPE_VALUE      = 110;
  {$EXTERNALSYM ETW_REVERSED_COUNTED_STRING_TYPE_VALUE}
  ETW_REVERSED_COUNTED_ANSISTRING_TYPE_VALUE  = 111;
  {$EXTERNALSYM ETW_REVERSED_COUNTED_ANSISTRING_TYPE_VALUE}
  ETW_NON_NULL_TERMINATED_STRING_TYPE_VALUE   = 112;
  {$EXTERNALSYM ETW_NON_NULL_TERMINATED_STRING_TYPE_VALUE}
  ETW_REDUCED_ANSISTRING_TYPE_VALUE           = 113;
  {$EXTERNALSYM ETW_REDUCED_ANSISTRING_TYPE_VALUE}
  ETW_REDUCED_STRING_TYPE_VALUE               = 114;
  {$EXTERNALSYM ETW_REDUCED_STRING_TYPE_VALUE}
  ETW_SID_TYPE_VALUE                          = 115;
  {$EXTERNALSYM ETW_SID_TYPE_VALUE}
  ETW_VARIANT_TYPE_VALUE                      = 116;
  {$EXTERNALSYM ETW_VARIANT_TYPE_VALUE}
  ETW_PTVECTOR_TYPE_VALUE                     = 117;
  {$EXTERNALSYM ETW_PTVECTOR_TYPE_VALUE}
  ETW_WMITIME_TYPE_VALUE                      = 118;
  {$EXTERNALSYM ETW_WMITIME_TYPE_VALUE}
  ETW_DATETIME_TYPE_VALUE                     = 119;
  {$EXTERNALSYM ETW_DATETIME_TYPE_VALUE}
  ETW_REFRENCE_TYPE_VALUE                     = 120;
  {$EXTERNALSYM ETW_REFRENCE_TYPE_VALUE}

type

  PMOF_FIELD = ^_MOF_FIELD;
  _MOF_FIELD = record
    DataPtr: ULONG64;      // Pointer to the field. Up to 64-bits only
    Length: ULONG;         // Length of the MOF field
    DataType: ULONG;       // Type of data
  end;
  {$EXTERNALSYM _MOF_FIELD}
  MOF_FIELD = _MOF_FIELD;
  {$EXTERNALSYM MOF_FIELD}


  // This is the header for every logfile. The memory for LoggerName
  // and LogFileName must be contiguous adjacent to this structure
  // Allows both user-mode and kernel-mode to understand the header.
  //
  // TRACE_LOGFILE_HEADER32 and TRACE_LOGFILE_HEADER64 structures
  // are also provided to simplify cross platform decoding of the
  // header event.
  //================================================================

   PTRACE_LOGFILE_HEADER = ^TRACE_LOGFILE_HEADER;
   _TRACE_LOGFILE_HEADER = record
    BufferSize    : ULONG;  // Logger buffer size in Kbytes
    VersionDetail : record
    case Boolean of
      True  : (Version      : ULONG);  // Logger version
      False : (MajorVersion : UCHAR;
               MinorVersion : UCHAR;
               SubVersion   : UCHAR;
               SubMinorVersion : UCHAR);
    end;

    ProviderVersion : ULONG;    // defaults to NT version
    NumberOfProcessors : ULONG; // Number of Processors
    EndTime : LARGE_INTEGER;    // Time when logger stops
    TimerResolution : ULONG;    // assumes timer is constant!!!
    MaximumFileSize : ULONG;    // Maximum in Mbytes
    LogFileMode : ULONG;        // specify logfile mode
    BuffersWritten : ULONG;     // used to file start of Circular File

    LogInstanceGuid : record
    case Boolean of
      True  : (LogInstanceGuid : TGUID);    // For RealTime Buffer Delivery
      False : (StartBuffers    : ULONG;     // Count of buffers written at start.
               PointerSize     : ULONG;     // Size of pointer type in bits
               EventsLost      : ULONG;     // Events losts during log session
               CpuSpeedInMHz   : ULONG);    // Cpu Speed in MHz
    end;

    LoggerName  : LPWSTR;
    LogFileName : LPWSTR;
    TimeZone    : TIME_ZONE_INFORMATION;

    BootTime  : LARGE_INTEGER;
    PerfFreq  : LARGE_INTEGER;  // Reserved
    StartTime : LARGE_INTEGER;  // Reserved
    ReservedFlags : ULONG;      // ClockType
    BuffersLost   : ULONG;
  end;
  {$EXTERNALSYM _TRACE_LOGFILE_HEADER}
  TRACE_LOGFILE_HEADER = _TRACE_LOGFILE_HEADER;
  {$EXTERNALSYM TRACE_LOGFILE_HEADER}



  PTRACE_LOGFILE_HEADER32 = ^TRACE_LOGFILE_HEADER32;
  _TRACE_LOGFILE_HEADER32 = record
    BufferSize: ULONG;    // Logger buffer size in Kbytes
    VersionDetail : record
    case Boolean of
      True  :  (Version: ULONG);   // Logger version
      False :  (MajorVersion: UCHAR;
                MinorVersion: UCHAR;
                SubVersion: UCHAR;
                SubMinorVersion: UCHAR);
    end;

    ProviderVersion    : ULONG;     // defaults to NT version
    NumberOfProcessors : ULONG;     // Number of Processors
    EndTime            : LARGE_INTEGER;  // Time when logger stops
    TimerResolution    : ULONG;     // assumes timer is constant!!!
    MaximumFileSize    : ULONG;     // Maximum in Mbytes
    LogFileMode        : ULONG;     // specify logfile mode
    BuffersWritten     : ULONG;     // used to file start of Circular File

    LogInstanceGuid : record
    case Boolean of
      True  : (LogInstanceGuid: TGUID);   // For RealTime Buffer Delivery
      False : (StartBuffers: ULONG;       // Count of buffers written at start.
               PointerSize: ULONG;        // Size of pointer type in bits
               EventsLost: ULONG;         // Events lost during log session
               CpuSpeedInMHz: ULONG);     // Cpu Speed in MHz
    end;

    LoggerName  : ULONG32;
    LogFileName : ULONG32;
    TimeZone    : TIME_ZONE_INFORMATION;

    BootTime      : LARGE_INTEGER;
    PerfFreq      : LARGE_INTEGER;    // Reserved
    StartTime     : LARGE_INTEGER;    // Reserved
    ReservedFlags : ULONG;            // ClockType
    BuffersLost   : ULONG;
   end;
   {$EXTERNALSYM _TRACE_LOGFILE_HEADER32}
   TRACE_LOGFILE_HEADER32 =  _TRACE_LOGFILE_HEADER32;
   {$EXTERNALSYM TRACE_LOGFILE_HEADER32}


  PTRACE_LOGFILE_HEADER64 = ^TRACE_LOGFILE_HEADER64;
  _TRACE_LOGFILE_HEADER64 = record
    BufferSize : ULONG;          // Logger buffer size in Kbytes
    VersionDetail : record
    case Boolean of
      True :  (Version         : ULONG);             // Logger version
      False : (MajorVersion    : UCHAR;
               MinorVersion    : UCHAR;
               SubVersion      : UCHAR;
               SubMinorVersion : UCHAR);
    end;

    ProviderVersion    : ULONG;     // defaults to NT version
    NumberOfProcessors : ULONG;     // Number of Processors
    EndTime            : LARGE_INTEGER;  // Time when logger stops
    TimerResolution    : ULONG;     // assumes timer is constant!!!
    MaximumFileSize    : ULONG;     // Maximum in Mbytes
    LogFileMode        : ULONG;     // specify logfile mode
    BuffersWritten     : ULONG;     // used to file start of Circular File

    LogInstanceGuid : record
    case Boolean of
      True  :  (LogInstanceGuid : TGUID);      // For RealTime Buffer Delivery
      False :  (StartBuffers    : ULONG;       // Count of buffers written at start.
                PointerSize     : ULONG;       // Size of pointer type in bits
                EventsLost      : ULONG;       // Events lost during log session
                CpuSpeedInMHz   : ULONG);      // Cpu Speed in MHz
    end;

    LoggerName: ULONG64;
    LogFileName: ULONG64;
    TimeZone: TIME_ZONE_INFORMATION;

    BootTime: LARGE_INTEGER;
    PerfFreq: LARGE_INTEGER;            // Reserved
    StartTime: LARGE_INTEGER;           // Reserved
    ReservedFlags: ULONG;       // ClockType
    BuffersLost: ULONG;
  end;
  {$EXTERNALSYM _TRACE_LOGFILE_HEADER64}
  TRACE_LOGFILE_HEADER64 = _TRACE_LOGFILE_HEADER64;
  {$EXTERNALSYM TRACE_LOGFILE_HEADER64}


  // Instance Information to track parent child relationship of Instances.
  // =====================================================================

  PEVENT_INSTANCE_INFO = ^EVENT_INSTANCE_INFO;
  EVENT_INSTANCE_INFO = record
    RegHandle: THandle;
    InstanceId: ULONG;
  end;
  {$EXTERNALSYM EVENT_INSTANCE_INFO}


  // Structures that have UNICODE and ANSI versions are defined here
  // ===============================================================


  // Logger configuration and running statistics. This structure is used
  // by user-mode callers, such as PDH library

  PEVENT_FILTER_DESCRIPTOR = ^_EVENT_FILTER_DESCRIPTOR;
  EVENT_FILTER_DESCRIPTOR = _EVENT_FILTER_DESCRIPTOR;
  {$EXTERNALSYM EVENT_FILTER_DESCRIPTOR}


  PEVENT_TRACE_PROPERTIES = ^EVENT_TRACE_PROPERTIES;
  _EVENT_TRACE_PROPERTIES = record
    Wnode: WNODE_HEADER;
    //
    // data provided by caller
    BufferSize: ULONG;                     // buffer size for logging (kbytes)
    MinimumBuffers: ULONG;                 // minimum to preallocate
    MaximumBuffers: ULONG;                 // maximum buffers allowed
    MaximumFileSize: ULONG;                // maximum logfile size (in MBytes)
    LogFileMode: ULONG;                    // sequential, circular
    FlushTimer: ULONG;                     // buffer flush timer, in seconds
    EnableFlags: ULONG;                    // trace enable flags

    DUMMYUNIONNAME : record
    case Boolean of
      True  : (AgeLimit : LONG);           // unused
      False : (FlushThreshold: LONG);      // Number of buffers to fill before flushing
    end;

    // data returned to caller
    NumberOfBuffers     : ULONG;           // no of buffers in use
    FreeBuffers         : ULONG;           // no of buffers free
    EventsLost          : ULONG;           // event records lost
    BuffersWritten      : ULONG;           // no of buffers written to file
    LogBuffersLost      : ULONG;           // no of logfile write failures
    RealTimeBuffersLost : ULONG;           // no of rt delivery failures
    LoggerThreadId      : THandle;         // thread id of Logger
    LogFileNameOffset   : ULONG;           // Offset to LogFileName
    LoggerNameOffset    : ULONG;           // Offset to LoggerName
  end;
  {$EXTERNALSYM _EVENT_TRACE_PROPERTIES}
  EVENT_TRACE_PROPERTIES = _EVENT_TRACE_PROPERTIES;
  {$EXTERNALSYM EVENT_TRACE_PROPERTIES}
  TEventTracePropertiesArray = array [0..65535] of EVENT_TRACE_PROPERTIES;


  PEVENT_TRACE_PROPERTIES_V2 = ^_EVENT_TRACE_PROPERTIES_V2;
  _EVENT_TRACE_PROPERTIES_V2 = record
    Wnode: WNODE_HEADER;                   // Always have WNODE_FLAG_VERSIONED_PROPERTIES.
    //
    // data provided by caller
    BufferSize      : ULONG;               // buffer size for logging (kbytes)
    MinimumBuffers  : ULONG;               // minimum to preallocate
    MaximumBuffers  : ULONG;               // maximum buffers allowed
    MaximumFileSize : ULONG;               // maximum logfile size (in MBytes)
    LogFileMode     : ULONG;               // sequential, circular
    FlushTimer      : ULONG;               // buffer flush timer, in seconds
    EnableFlags     : ULONG;               // trace enable flags

    DUMMYUNIONNAME : record
    case Boolean of
      True  : (AgeLimit: LONG);            // unused
      False : (FlushThreshold: LONG);      // Number of buffers to fill before flushing
    end;

    // data returned to caller
    NumberOfBuffers     : ULONG;           // no of buffers in use
    FreeBuffers         : ULONG;           // no of buffers free
    EventsLost          : ULONG;           // event records lost
    BuffersWritten      : ULONG;           // no of buffers written to file
    LogBuffersLost      : ULONG;           // no of logfile write failures
    RealTimeBuffersLost : ULONG;           // no of rt delivery failures
    LoggerThreadId      : THandle;         // thread id of Logger
    LogFileNameOffset   : ULONG;           // Offset to LogFileName
    LoggerNameOffset    : ULONG;           // Offset to LoggerName

    // V2 data
    DUMMYUNIONNAME2 : record
    case Boolean of
      True  : (VersionNumber: ULONG);     // Should be set to 2 for this version.
      False : (V2Control: ULONG);
    end;

    FilterDescCount : ULONG;                     // Number of filters
    FilterDesc      : EVENT_FILTER_DESCRIPTOR;   // Only applicable for Private Loggers

    DUMMYUNIONNAME3 : record
    case Integer of
      0  : (Wow: ULONG);
      1  : (QpcDeltaTracking: ULONG);   // QPC delta tracking events are enabled.
      2  : (V2Options: ULONG64);        // Logger was started by a WOW64 process (output only).
      3  : (LargeMdlPages: ULONG);      // Buffers allocated via large MDL pages.
      4  : (ExcludeKernelStack: ULONG); // Exclude kernel stack from stack walk.
    end;
  end;
  {$EXTERNALSYM _EVENT_TRACE_PROPERTIES_V2}
  EVENT_TRACE_PROPERTIES_V2 = _EVENT_TRACE_PROPERTIES_V2;
  {$EXTERNALSYM EVENT_TRACE_PROPERTIES_V2}


  // Data Provider structures
  //
  // Used by RegisterTraceGuids()
  // ============================

  PTRACE_GUID_REGISTRATION = ^TRACE_GUID_REGISTRATION;
  _TRACE_GUID_REGISTRATION = record
    Guid: TGUID;                 // Guid of data block being registered or updated.
    RegHandle: THandle;          // Guid Registration Handle is returned.
  end;
  {$EXTERNALSYM _TRACE_GUID_REGISTRATION}
  TRACE_GUID_REGISTRATION = _TRACE_GUID_REGISTRATION;
  {$EXTERNALSYM TRACE_GUID_REGISTRATION}


  // Data consumer structures
  // ========================

  PTRACE_GUID_PROPERTIES = ^TRACE_GUID_PROPERTIES;
  _TRACE_GUID_PROPERTIES = record
    Guid: TGUID;
    GuidType: ULONG;
    LoggerId: ULONG;
    EnableLevel: ULONG;
    EnableFlags: ULONG;
    IsEnable: BOOLEAN;
  end;
  {$EXTERNALSYM _TRACE_GUID_PROPERTIES}
  TRACE_GUID_PROPERTIES = _TRACE_GUID_PROPERTIES;
  {$EXTERNALSYM TRACE_GUID_PROPERTIES}


const

  // Provider Information Flags used on Vista and above.
  // ===================================================
  TRACE_PROVIDER_FLAG_LEGACY     = ($00000001);
  {$EXTERNALSYM TRACE_PROVIDER_FLAG_LEGACY}
  TRACE_PROVIDER_FLAG_PRE_ENABLE = ($00000002);
  {$EXTERNALSYM TRACE_PROVIDER_FLAG_PRE_ENABLE}


type

  // Enable Information for Provider Instance
  // Used on Vista and above
  //=========================================

  PTRACE_ENABLE_INFO = ^TRACE_ENABLE_INFO;
  _TRACE_ENABLE_INFO = record
    IsEnabled: ULONG;
    Level: UCHAR;
    Reserved1: UCHAR;
    LoggerId: USHORT;
    EnableProperty: ULONG;
    Reserved2: ULONG;
    MatchAnyKeyword: ULONGLONG;
    MatchAllKeyword: ULONGLONG;
  end;
  {$EXTERNALSYM _TRACE_ENABLE_INFO}
  TRACE_ENABLE_INFO = _TRACE_ENABLE_INFO;


  // Instance Information for Provider
  // Used on Vista and above
  // =================================
  PTRACE_PROVIDER_INSTANCE_INFO = ^TRACE_PROVIDER_INSTANCE_INFO;
  _TRACE_PROVIDER_INSTANCE_INFO = record
    NextOffset: ULONG;
    EnableCount: ULONG;
    Pid: ULONG;
    Flags: ULONG;
  end;
  {$EXTERNALSYM _TRACE_PROVIDER_INSTANCE_INFO}
  TRACE_PROVIDER_INSTANCE_INFO = _TRACE_PROVIDER_INSTANCE_INFO;
  {$EXTERNALSYM TRACE_PROVIDER_INSTANCE_INFO}


  // GUID Information Used on Vista and above
  // ========================================
  PTRACE_GUID_INFO = ^TRACE_GUID_INFO;
  _TRACE_GUID_INFO = record
    InstanceCount: ULONG;
    Reserved: ULONG;
  end;
  {$EXTERNALSYM _TRACE_GUID_INFO}
  TRACE_GUID_INFO = _TRACE_GUID_INFO;
  {$EXTERNALSYM TRACE_GUID_INFO}


  PPROFILE_SOURCE_INFO = ^PROFILE_SOURCE_INFO;
  _PROFILE_SOURCE_INFO = record
    NextEntryOffset: ULONG;
    Source: ULONG;
    MinInterval: ULONG;
    MaxInterval: ULONG;
    Reserved: ULONG64;
    Description: array[0..ANYSIZE_ARRAY - 1] of WideChar;
  end;
  {$EXTERNALSYM _PROFILE_SOURCE_INFO}
  PROFILE_SOURCE_INFO = _PROFILE_SOURCE_INFO;
  {$EXTERNALSYM PROFILE_SOURCE_INFO}


  // An EVENT_TRACE consists of a fixed header (EVENT_TRACE_HEADER) and
  // optionally a variable portion pointed to by MofData. The datablock
  // layout of the variable portion is unknown to the Logger and must
  // be obtained from WBEM CIMOM database.
  // ==================================================================

  PEVENT_TRACE = ^EVENT_TRACE;
  _EVENT_TRACE = record
    Header: EVENT_TRACE_HEADER; // Event trace header
    InstanceId: ULONG;          // Instance Id of this event
    ParentInstanceId: ULONG;    // Parent Instance Id.
    ParentGuid: TGUID;          // Parent Guid;
    MofData: PVOID;             // Pointer to Variable Data
    MofLength: ULONG;           // Variable Datablock Length
    case Boolean of
        True  : (ClientContext: ULONG);
        False : (BufferContext: ETW_BUFFER_CONTEXT);
  end;
  {$EXTERNALSYM _EVENT_TRACE}
  EVENT_TRACE = _EVENT_TRACE;
  {$EXTERNALSYM EVENT_TRACE}



  // pre-definitions
  PEVENT_TRACE_LOGFILEA = ^EVENT_TRACE_LOGFILEA;
  PEVENT_TRACE_LOGFILEW = ^EVENT_TRACE_LOGFILEW;
  {$EXTERNALSYM PEVENT_TRACE_LOGFILEW}

  PEVENT_TRACE_BUFFER_CALLBACKW = ^EVENT_TRACE_BUFFER_CALLBACKW;
  EVENT_TRACE_BUFFER_CALLBACKW = function (Logfile: PEVENT_TRACE_LOGFILEW): ULONG; stdcall;
  {$EXTERNALSYM EVENT_TRACE_BUFFER_CALLBACKW}

  PEVENT_TRACE_BUFFER_CALLBACKA = ^EVENT_TRACE_BUFFER_CALLBACKA;
  EVENT_TRACE_BUFFER_CALLBACKA = function (Logfile: PEVENT_TRACE_LOGFILEA): ULONG; stdcall;
  {$EXTERNALSYM EVENT_TRACE_BUFFER_CALLBACKA}

  PEVENT_CALLBACK = ^EVENT_CALLBACK;
  EVENT_CALLBACK = procedure (pEvent: PEVENT_TRACE); stdcall;
  {$EXTERNALSYM EVENT_CALLBACK}

  PEVENT_RECORD_CALLBACK = ^EVENT_RECORD_CALLBACK;
  EVENT_RECORD_CALLBACK = procedure (EventRecord: PEVENT_RECORD); stdcall;
  {$EXTERNALSYM EVENT_RECORD_CALLBACK}


  // Prototype for service request callback. Data providers register with WMI
  // by passing a service request callback function that is called for all
  // wmi requests.
  // ========================================================================

   WMIDPREQUEST = function(RequestCode: WMIDPREQUESTCODE;
                           const RequestContext: Pointer;
                           var BufferSize: PULONG;
                           var Buffer: Pointer): ULONG; stdcall;
   {$EXTERNALSYM WMIDPREQUEST}

  _EVENT_TRACE_LOGFILEW = record
    LogFileName: LPWSTR;    // Logfile Name
    LoggerName: LPWSTR;     // LoggerName
    CurrentTime: LONGLONG;  // timestamp of last event
    BuffersRead: ULONG;     // buffers read to date
    LogFileMode: record
    case Boolean of
        // Mode of the logfile
        True  : (LogFileMode: ULONG);
        // Processing flags used on Vista and above
        False : (ProcessTraceMode: ULONG);
    end;

    CurrentEvent: EVENT_TRACE;                       // Current Event from this stream.
    LogfileHeader: TRACE_LOGFILE_HEADER;             // logfile header structure
    BufferCallback: PEVENT_TRACE_BUFFER_CALLBACKW;   // callback before each buffer is read
    //
    // following variables are filled for BufferCallback.
    //
    BufferSize: ULONG;
    Filled: ULONG;
    EventsLost: ULONG;
    //
    // following needs to be propagated to each buffer
    //
    EventCallback: record
      case Boolean of
        // Callback with EVENT_TRACE
        True : (EventCallback : PEVENT_CALLBACK);
        // Callback with EVENT_RECORD on Vista and above
        False : (EventRecordCallback : PEVENT_RECORD_CALLBACK);
    end;

    IsKernelTrace: ULONG;    // TRUE for kernel logfile
    Context: PVOID;          // reserved for internal use
  end;
  {$EXTERNALSYM _EVENT_TRACE_LOGFILEW}
  EVENT_TRACE_LOGFILEW = _EVENT_TRACE_LOGFILEW;
  {$EXTERNALSYM EVENT_TRACE_LOGFILEW}


  _EVENT_TRACE_LOGFILEA = record
    LogFileName: LPSTR;     // Logfile Name
    LoggerName: LPSTR;      // LoggerName
    CurrentTime: LONGLONG;  // timestamp of last event
    BuffersRead: ULONG;     // buffers read to date
    LogFileMode: record
    case Boolean of
      // Mode of the logfile
      True : (LogFileMode : ULONG);
      // Processing flags used on Vista and above
      False : (ProcessTraceMode : ULONG);
    end;

    CurrentEvent: EVENT_TRACE;                      // Current Event from this stream.
    LogfileHeader: TRACE_LOGFILE_HEADER;            // logfile header structure
    BufferCallback: PEVENT_TRACE_BUFFER_CALLBACKW;  // callback before each buffer is read

    // following variables are filled for BufferCallback.
    //
    BufferSize: ULONG;
    Filled : ULONG;
    EventsLost: ULONG;

    // following needs to be propaged to each buffer
    //
    EventCallback: record
    case Boolean of
      // Callback with EVENT_TRACE
      True  : (EventCallback: PEVENT_CALLBACK);
      // Callback with EVENT_RECORD on Vista and above
      False : (EventRecordCallback: PEVENT_RECORD_CALLBACK);
    end;
    IsKernelTrace: ULONG;   // TRUE for kernel logfile
    Context: PVOID;         // reserved for internal use
  end;
  {$EXTERNALSYM _EVENT_TRACE_LOGFILEA}
  EVENT_TRACE_LOGFILEA = _EVENT_TRACE_LOGFILEA;
  {$EXTERNALSYM EVENT_TRACE_LOGFILEA}


  // Define generic structures
  // =========================



{$IFDEF DEFINE_UNICODE}

type

  PEVENT_TRACE_BUFFER_CALLBACK    = PEVENT_TRACE_BUFFER_CALLBACKW;
  PEVENT_TRACE_LOGFILE            = PEVENT_TRACE_LOGFILEW;
  EVENT_TRACE_LOGFILE             = EVENT_TRACE_LOGFILEW;
  {$EXTERNALSYM EVENT_TRACE_LOGFILE}

{$ELSE}

  PEVENT_TRACE_BUFFER_CALLBACK    = PEVENT_TRACE_BUFFER_CALLBACKA;
  PEVENT_TRACE_LOGFILE            = PEVENT_TRACE_LOGFILEA;
  EVENT_TRACE_LOGFILE             = EVENT_TRACE_LOGFILEA;
  {$EXTERNALSYM EVENT_TRACE_LOGFILE}

{$ENDIF DEFINE_UNICODE}


{$IFDEF DEFINE_UNICODE}

const

  KERNEL_LOGGER_NAME              = KERNEL_LOGGER_NAMEW;
  {$EXTERNALSYM KERNEL_LOGGER_NAME}
  GLOBAL_LOGGER_NAME              = GLOBAL_LOGGER_NAMEW;
  {$EXTERNALSYM GLOBAL_LOGGER_NAME}
  EVENT_LOGGER_NAME               = EVENT_LOGGER_NAMEW;
  {$EXTERNALSYM EVENT_LOGGER_NAME}

{$ELSE}

const

  KERNEL_LOGGER_NAME              = KERNEL_LOGGER_NAMEA;
  {$EXTERNALSYM KERNEL_LOGGER_NAME}
  GLOBAL_LOGGER_NAME              = GLOBAL_LOGGER_NAMEA;
  {$EXTERNALSYM GLOBAL_LOGGER_NAME}
  EVENT_LOGGER_NAME               = EVENT_LOGGER_NAMEA;
  {$EXTERNALSYM EVENT_LOGGER_NAME}

{$ENDIF DEFINE_UNICODE}


  // Logger control APIs
  // ===================


  // Use the routine below to start an event trace session
  // =====================================================

  function StartTrace(out TraceHandle: PTRACEHANDLE;
                      InstanceName: LPTSTR;
                      var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StartTrace}

  function StartTraceW(out TraceHandle: PTRACEHANDLE;
                       InstanceName: PWideChar;
                       var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StartTraceW}

  function StartTraceA(out TraceHandle: PTRACEHANDLE;
                       InstanceName: PAnsiChar;
                       var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StartTraceA}


  // Use the routine below to stop an event trace session
  // ====================================================

  function StopTrace(const TraceHandle: TRACEHANDLE;
                     InstanceName: LPTSTR;
                     var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StopTrace}

  function StopTraceW(const TraceHandle: TRACEHANDLE;
                      InstanceName: PWideChar;
                      var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StopTraceW}

  function StopTraceA(const TraceHandle: TRACEHANDLE;
                      InstanceName: PAnsiChar;
                      var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM StopTraceA}



  // Use the routine below to query the properties of an event trace session
  // =======================================================================

  function QueryTrace(const TraceHandle: TRACEHANDLE;
                      InstanceName: LPTSTR;
                      var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM QueryTrace}

  function QueryTraceW(const TraceHandle: TRACEHANDLE;
                       InstanceName: PWideChar;
                       var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM QueryTraceW}

  function QueryTraceA(const TraceHandle: TRACEHANDLE;
                       InstanceName: PAnsiChar;
                       var Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM QueryTraceA}


  // Use the routine below to update certain properties of an event trace session
  // ============================================================================

  function UpdateTrace(TraceHandle: TRACEHANDLE;
                       InstanceName: LPTSTR;
                       Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM UpdateTrace}

  function UpdateTraceW(TraceHandle: TRACEHANDLE;
                        InstanceName: PWideChar;
                        Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM UpdateTraceW}

  function UpdateTraceA(TraceHandle: TRACEHANDLE;
                        InstanceName: PAnsiChar;
                        Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM UpdateTraceA}



  // Use the routine below to request that all active buffers an event trace
  // session be "flushed", or written out.
  // =======================================================================

  function FlushTrace(TraceHandle: TRACEHANDLE;
                      InstanceName: LPTSTR;
                      Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM FlushTrace}

  function FlushTraceW(TraceHandle: TRACEHANDLE;
                       InstanceName: PWideChar;
                       Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM FlushTraceW}

  function FlushTraceA(TraceHandle: TRACEHANDLE;
                       InstanceName: PAnsiChar;
                       Properties: EVENT_TRACE_PROPERTIES): ULONG; stdcall;
  {$EXTERNALSYM FlushTraceA}



  // Generic trace control routine
  // =============================

  function ControlTrace(TraceHandle: TRACEHANDLE;
                        InstanceName: LPTSTR;
                        var Properties: EVENT_TRACE_PROPERTIES;
                        ControlCode: ULONG): ULONG; stdcall;
  {$EXTERNALSYM ControlTrace}

  function ControlTraceW(TraceHandle: TRACEHANDLE;
                         InstanceName: PWideChar;
                         var Properties: EVENT_TRACE_PROPERTIES;
                         ControlCode: ULONG): ULONG; stdcall;
  {$EXTERNALSYM ControlTraceW}

  function ControlTraceA(TraceHandle: TRACEHANDLE;
                         InstanceName: PAnsiChar;
                         var Properties: EVENT_TRACE_PROPERTIES;
                         ControlCode: ULONG): ULONG; stdcall;
  {$EXTERNALSYM ControlTraceA}

  function QueryAllTraces(var PropertyArray: TEventTracePropertiesArray;
                          PropertyArrayCount: ULONG;
                          LoggerCount: PULONG): ULONG; stdcall;
  {$EXTERNALSYM QueryAllTraces}

  function QueryAllTracesW(var PropertyArray: TEventTracePropertiesArray;
                           PropertyArrayCount: ULONG;
                           LoggerCount: PULONG): ULONG; stdcall;
  {$EXTERNALSYM QueryAllTracesW}

  function QueryAllTracesA(var PropertyArray: TEventTracePropertiesArray;
                           PropertyArrayCount: ULONG;
                           LoggerCount: PULONG): ULONG; stdcall;
  {$EXTERNALSYM QueryAllTracesA}


  // Data Provider Enable APIs
  // =========================

  function EnableTrace(Enable: ULONG;
                       EnableFlag: ULONG;
                       EnableLevel: ULONG;
                       ControlGuid: PGUID;
                       TraceHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM EnableTrace}


//#if (WINVER >= _WIN32_WINNT_VISTA)

  function EnableTraceEx(const ProviderId: TGUID;
                         const SourceId: TGUID;
                         TraceHandle: TRACEHANDLE;
                         IsEnabled: ULONG;
                         Level: UCHAR;
                         MatchAnyKeyword: ULONGLONG;
                         MatchAllKeyword: ULONGLONG;
                         EnableProperty: ULONG;
                         EnableFilterDesc: EVENT_FILTER_DESCRIPTOR): ULONG; stdcall;
  {$EXTERNALSYM EnableTraceEx}
//#endif

const

  ENABLE_TRACE_PARAMETERS_VERSION    = 1;
  {$EXTERNALSYM ENABLE_TRACE_PARAMETERS_VERSION}
  ENABLE_TRACE_PARAMETERS_VERSION_2  = 2;
  {$EXTERNALSYM ENABLE_TRACE_PARAMETERS_VERSION_2}


type

  PENABLE_TRACE_PARAMETERS_V1 = ^ENABLE_TRACE_PARAMETERS_V1;
  _ENABLE_TRACE_PARAMETERS_V1 = record
    Version: ULONG;
    EnableProperty: ULONG;
    ControlFlags: ULONG;
    SourceId: TGUID;
    EnableFilterDesc: PEVENT_FILTER_DESCRIPTOR;
  end;
  {$EXTERNALSYM _ENABLE_TRACE_PARAMETERS_V1}
  ENABLE_TRACE_PARAMETERS_V1 = _ENABLE_TRACE_PARAMETERS_V1;
  {$EXTERNALSYM ENABLE_TRACE_PARAMETERS_V1}


  PENABLE_TRACE_PARAMETERS = ^ENABLE_TRACE_PARAMETERS;
  _ENABLE_TRACE_PARAMETERS = record
    Version: ULONG;
    EnableProperty: ULONG;
    ControlFlags: ULONG;
    SourceId: TGUID;
    EnableFilterDesc: PEVENT_FILTER_DESCRIPTOR;
    FilterDescCount: ULONG;
  end;
  {$EXTERNALSYM _ENABLE_TRACE_PARAMETERS}
  ENABLE_TRACE_PARAMETERS = _ENABLE_TRACE_PARAMETERS;
  {$EXTERNALSYM ENABLE_TRACE_PARAMETERS}


//#if (WINVER >= _WIN32_WINNT_WIN7)
  function EnableTraceEx2(const TraceHandle: TRACEHANDLE;
                          const ProviderId: TGUID;
                          ControlCode: ULONG;
                          Level: UCHAR;
                          MatchAnyKeyword: ULONGLONG;
                          MatchAllKeyword: ULONGLONG;
                          Timeout: ULONG;
                          EnableParameters: PENABLE_TRACE_PARAMETERS = Nil): ULONG; stdcall;
  {$EXTERNALSYM EnableTraceEx2}
//#endif


type
  //
  // TRACE_QUERY_INFO_CLASS/TRACE_INFO_CLASS
  // This enumeration contains values that are passed to one or more of the
  // ETW query/set APIs: EnumerateTraceGuidsEx, TraceSetInformation, TraceQueryInformation.
  //
  PRACE_INFO_CLASS = ^TRACE_INFO_CLASS;
  PRACE_QUERY_INFO_CLASS = ^TRACE_QUERY_INFO_CLASS;
  _TRACE_QUERY_INFO_CLASS = (
    //
    // TraceGuidQueryList:
    // - EnumerateTraceGuidsEx.
    //      Returns a list of provider GUIDs that are currently registered with the kernel.
    //      Note: This is not the same thing as having an installed manifest.
    //
    //      Input Format: None.
    //      Output Format: An array of GUIDs.
    //
    TraceGuidQueryList = 0,

    //
    // TraceGuidQueryList:
    // - EnumerateTraceGuidsEx.
    //      Returns the current registration and enablement information for the input GUID.
    //
    //      Input Format: GUID
    //      Output Format: TRACE_GUID_INFO followed by TRACE_GUID_INFO.InstanceCount
    //                     TRACE_PROVIDER_INSTANCE_INFO structs, each bundled with
    //                     TRACE_PROVIDER_INSTANCE_INFO.EnableCount TRACE_ENABLE_INFO.
    //                For example, a GUID with two provider registrations, the first enabled
    //                  by two loggers and the second by three loggers would look like:
    //                  {
    //                      TRACE_GUID_INFO;  // Where InstanceCount = 2
    //                      TRACE_PROVIDER_INSTANCE_INFO; // Where EnableCount = 2
    //                      TRACE_ENABLE_INFO;
    //                      TRACE_ENABLE_INFO;
    //                      TRACE_PROVIDER_INSTANCE_INFO; // Where EnableCount = 3
    //                      TRACE_ENABLE_INFO;
    //                      TRACE_ENABLE_INFO;
    //                      TRACE_ENABLE_INFO;
    //                  }
    //
    TraceGuidQueryInfo = 1,

    //
    // TraceGuidQueryProcess:
    // - EnumerateTraceGuidsEx.
    //      Returns a list of provider GUIDs that are registered in the current process.
    //      Note: This is not the same thing as having an installed manifest.
    //
    //      Input Format: None.
    //      Output Format: An array of GUIDs.
    //
    TraceGuidQueryProcess = 2,

    //
    // TraceStackTracingInfo:
    // - TraceSetInformation.
    //      Turns on stack trace collection for the specified kernel events for the specified logger.
    //      It also turns off stack tracing for all kernel events not on this list, regardless of prior status.
    //
    //      Input Format: An array of CLASSIC_EVENT_ID structs.
    //
    TraceStackTracingInfo = 3,

    //
    // TraceSystemTraceEnableFlagsInfo:
    // - TraceSetInformation
    //      Sets the Group Mask state for the specified logging session.
    //
    //      Input Format: PERFINFO_GROUPMASK
    //
    // - TraceQueryInformation
    //      Queries the current Group Mask state for the specified logging session.
    //
    //      Return Format: PERFINFO_GROUPMASK
    //
    TraceSystemTraceEnableFlagsInfo = 4,

    //
    // TraceSampledProfileIntervalInfo:
    // - TraceSetInformation
    //      Sets the Sample Profile interval for the system.
    //      Expects NULL SessionHandle parameter.
    //
    //      Input Format: TRACE_PROFILE_INTERVAL
    //
    // - TraceQueryInformation
    //      Queries the current Sample Profile Interval for the system.
    //      Expects NULL SessionHandle parameter.
    //
    //      Output Format: TRACE_PROFILE_INTERVAL
    //
    TraceSampledProfileIntervalInfo = 5,

    //
    // TraceProfileSourceConfigInfo:
    // - TraceSetInformation
    //      Sets a list of sources to be used for PMC Profiling system-wide.
    //      Expects NULL SessionHandle parameter.
    //
    //      Input Format: An array of ULONGs specifying the IDs of the sources.
    //
    TraceProfileSourceConfigInfo = 6,

    //
    // TraceProfileSourceListInfo:
    // - TraceQueryInformation
    //      Queries the list of PMC Profiling sources available on the system.
    //      Expects NULL SessionHandle parameter.
    //
    //      Output Format: An array of PROFILE_SOURCE_INFO structs.
    //
    TraceProfileSourceListInfo = 7,

    //
    // TracePmcEventListInfo:
    // - TraceSetInformation
    //      Updates the list of kernel events for which PMC counters will be collected
    //      for the specified logger.  This can only be set once per logger and cannot
    //      be updated.
    //      The counters collected are specified by
    //      TraceSetInformation(TracePmcCounterListInfo, ...) described below.
    //
    //      Input Format: An array of CLASSIC_EVENT_ID structs.
    //
    TracePmcEventListInfo = 8,

    //
    // TracePmcCounterListInfo:
    // - TraceSetInformation
    //      Sets the list of PMC counters to be collected on system events.
    //      This can only be set once per logger and cannot be updated.
    //      The specified counters will be collected on the events specified by
    //      TraceSetInformation(TracePmcEventListInfo, ...) described above.
    //
    //      Input Format: An array of ULONGs.
    //
    TracePmcCounterListInfo = 9,

    //
    // TraceSetDisallowList:
    // - TraceSetInformation
    //      Sets a list of provider GUIDs that should not be enabled via
    //      Provider Groups on the specified logging session.
    //
    //      Input Format: An array of GUIDs.
    //
    TraceSetDisallowList = 10,

    //
    // TraceVersionInfo:
    // - TraceQueryInformation
    //      Queries the version number of the trace processing code.
    //
    //      Output Format: TRACE_VERSION_INFO
    //
    TraceVersionInfo = 11,

    //
    // TraceGroupQueryList:
    // - EnumerateTraceGuidsEx.
    //      Returns a list of Group GUIDs that are currently known to the kernel.
    //
    //      Input Format: None.
    //      Output Format: An array of GUIDs.
    //
    TraceGroupQueryList = 12,

    //
    // TraceGroupQueryInfo:
    // - EnumerateTraceGuidsEx.
    //      Returns the current enablement information and list of member providers
    //      for the input Group GUID.
    //
    //      Input Format: GUID
    //      Output Format:  a) ULONG - Length of the following TRACE_ENABLE_INFO array.
    //                      b) Array of TRACE_ENABLE_INFO. Size of the array is inferred from (a)
    //                      c) ULONG - Count of the Number of Unique Providers that belong to this Group
    //                      d) Array of GUID - Size of the array is specified by (c)
    //
    //                  PseudoStructure -
    //                      struct TRACE_GROUP_INFO {
    //                          ULONG TraceEnableInfoSize;
    //                          TRACE_ENABLE_INFO TraceEnableInfos[TraceEnableInfoSize];
    //                          ULONG GuidArraySize;
    //                          GUID UniqueProviders[GuidArraySize];
    //                      }
    //
    TraceGroupQueryInfo = 13,

    //
    // TraceDisallowListQuery:
    // - TraceQueryInformation
    //      Queries the list of provider GUIDs that should not be enabled via
    //      Provider Groups on the specified logging session.
    //
    //      Output Format: An array of GUIDs.
    //
    TraceDisallowListQuery = 14,

    TraceInfoReserved15 = 15,

    //
    // TracePeriodicCaptureStateListInfo:
    // - TraceSetInformation
    //      Sets the list of providers for which capture stat should be collected
    //      at periodic time intervals for the specified logging session.
    //      If a NULL input buffer is specified, then the current periodic capture state
    //      settings are cleared.
    //
    //      Input Format: TRACE_PERIODIC_CAPTURE_STATE_INFO followed by an array of ProviderCount
    //                      Provider GUIDs. Or a NULL Buffer.
    //
    TracePeriodicCaptureStateListInfo = 16,

    //
    // TracePeriodicCaptureStatInfo:
    // - TraceQueryInformation
    //      Queries the limits of periodic capture settings on this system, including
    //      the minimum time frequency and the maximum number of providers that can be
    //      enabled for periodic capture state.
    //
    //      Output Format: TRACE_PERIODIC_CAPTURE_STATE_INFO
    //
    TracePeriodicCaptureStateInfo = 17,

    //
    // TraceProviderBinaryTracking:
    // - TraceSetInformation
    //      Instructs ETW to begin tracking binaries for all providers that are enabled
    //      to the session. The tracking applies retroactively for providers that were
    //      enabled to the session prior to the call, as well as for all future providers
    //      that are enabled to the session.
    //
    //      ETW fabricates tracking events for these tracked providers that contain a
    //      mapping between provider GUID(s). ETW also fabricates the file path that describes
    //      where the registered provider is located on disk. If the session is in realtime,
    //      the events are provided live in the realtime buffers. If the session is file-based
    //      (i.e. trace is saved to an .etl file), the events are aggregated and written to the
    //      file header; they will be among some of the first events the ETW runtime provides
    //      when the .etl file is played back.
    //
    //      The binary tracking events will come from the EventTraceGuid provider, with an opcode
    //      of WMI_LOG_TYPE_BINARY_PATH.
    //
    //      Input Format: BOOLEAN (The 1-byte type, rather than the 4-byte BOOL.)
    //                    True to turn tracking on. False to turn tracking off.
    //
    TraceProviderBinaryTracking = 18,

    //
    // TraceMaxLoggersQuery:
    // - TraceQueryInformation
    //      Queries the maximum number of system-wide loggers that can be running at a time
    //      on this system.
    //
    //      Output Format: ULONG
    //
    TraceMaxLoggersQuery = 19,

    //
    // TraceLbrConfigurationInfo:
    // - TraceSetInformation
    //      Sets a bitfield of configuration options for Last Branch Record tracing.
    //
    //      Input Format: ULONG
    //
    TraceLbrConfigurationInfo = 20,

    //
    // TraceLbrEventListInfo:
    // - TraceSetInformation
    //      Provides a list of kernel events to collect Last Branch Records on.
    //      The events are specified by their HookIds.
    //
    //      Input Format: An array of ULONGs
    //
    TraceLbrEventListInfo   = 21,

    //
    // TraceMaxPmcCounterQuery:
    // - TraceQueryInformation
    //      Queries the maximum number of PMC counters supported on this platform
    //
    //      Output Format: ULONG
    //
    TraceMaxPmcCounterQuery = 22,

    //
    // TraceStreamCount:
    // - TraceQueryInformation
    //      Queries the number of streams that a given session can be expected
    //      to emit. This is usually proportional to CPU count, or 1 if no
    //      per-processor buffering is enabled.
    //
    //      Output Format: ULONG
    //
    TraceStreamCount       = 23,
    MaxTraceSetInfoClass   = 23
  );
  {$EXTERNALSYM _TRACE_QUERY_INFO_CLASS}
  TRACE_QUERY_INFO_CLASS = _TRACE_QUERY_INFO_CLASS;
  {$EXTERNALSYM TRACE_QUERY_INFO_CLASS}
  TRACE_INFO_CLASS = TRACE_QUERY_INFO_CLASS;
  {$EXTERNALSYM TRACE_INFO_CLASS}


//#if (WINVER >= _WIN32_WINNT_VISTA)
  function EnumerateTraceGuidsEx(TraceQueryInfoClass: TRACE_QUERY_INFO_CLASS;
                                 InBuffer: Pointer;
                                 InBufferSize: ULONG;
                                 out OutBuffer: Pointer;
                                 OutBufferSize: ULONG;
                                 out ReturnLength: ULONG): ULONG; stdcall;
  {$EXTERNALSYM EnumerateTraceGuidsEx}
//#endif

type

  PCLASSIC_EVENT_ID = ^CLASSIC_EVENT_ID;
  _CLASSIC_EVENT_ID = record
    EventGuid: TGUID;
    _Type: UCHAR;
    Reserved: array[0..6] of UCHAR;
  end;
  {$EXTERNALSYM _CLASSIC_EVENT_ID}
  CLASSIC_EVENT_ID = _CLASSIC_EVENT_ID;
  {$EXTERNALSYM CLASSIC_EVENT_ID}

  PTRACE_PROFILE_INTERVAL = ^TRACE_PROFILE_INTERVAL;
  _TRACE_PROFILE_INTERVAL = record
    Source: ULONG;
    Interval: ULONG;
  end;
  {$EXTERNALSYM _TRACE_PROFILE_INTERVAL}
  TRACE_PROFILE_INTERVAL = _TRACE_PROFILE_INTERVAL;
  {$EXTERNALSYM TRACE_PROFILE_INTERVAL}

  PTRACE_VERSION_INFO = ^TRACE_VERSION_INFO;
  _TRACE_VERSION_INFO = record
    EtwTraceProcessingVersion: UINT;
    Reserved: UINT;
  end;
  {$EXTERNALSYM _TRACE_VERSION_INFO}
  TRACE_VERSION_INFO = _TRACE_VERSION_INFO;
  {$EXTERNALSYM TRACE_VERSION_INFO}

  PTRACE_PERIODIC_CAPTURE_STATE_INFO = ^TRACE_PERIODIC_CAPTURE_STATE_INFO;
  _TRACE_PERIODIC_CAPTURE_STATE_INFO = record
    CaptureStateFrequencyInSeconds: ULONG;
    ProviderCount: USHORT;
    Reserved: USHORT;
  end;
  {$EXTERNALSYM _TRACE_PERIODIC_CAPTURE_STATE_INFO}
  TRACE_PERIODIC_CAPTURE_STATE_INFO = _TRACE_PERIODIC_CAPTURE_STATE_INFO;
  {$EXTERNALSYM TRACE_PERIODIC_CAPTURE_STATE_INFO}


//#if (WINVER >= _WIN32_WINNT_WIN7)
  function TraceSetInformation(SessionHandle: TRACEHANDLE;
                               InformationClass: TRACE_INFO_CLASS;
                               TraceInformation: Pointer;
                               InformationLength: ULONG): ULONG; stdcall;
  {$EXTERNALSYM TraceSetInformation}
//#endif

//#if (WINVER >= _WIN32_WINNT_WIN8)
  function TraceQueryInformation(SessionHandle: TRACEHANDLE;
                                 InformationClass: TRACE_INFO_CLASS;
                                 out TraceInformation: Pointer;
                                 InformationLength: ULONG;
                                 out ReturnLength: ULONG): ULONG; stdcall;
  {$EXTERNALSYM TraceQueryInformation}
//#endif


  // Data Provider APIs
  // ==================

  function CreateTraceInstanceId(RegHandle: THandle;
                                 var InstInfo: EVENT_INSTANCE_INFO): ULONG; stdcall;
  {$EXTERNALSYM CreateTraceInstanceId}


  // Use the routine below to generate and record an event trace
  //============================================================

  function TraceEvent(TraceHandle: TRACEHANDLE;
                      EventTrace: EVENT_TRACE_HEADER): ULONG; stdcall;
  {$EXTERNALSYM TraceEvent}

  function TraceEventInstance(TraceHandle: TRACEHANDLE;
                              EventTrace: EVENT_INSTANCE_HEADER;
                              InstInfo: EVENT_INSTANCE_INFO;
                              ParentInstInfo: EVENT_INSTANCE_INFO): ULONG; stdcall;
  {$EXTERNALSYM TraceEventInstance}


  // Use the routine below to register a guid for tracing.
  // =====================================================


  function RegisterTraceGuids(RequestAddress: WMIDPREQUEST;
                              RequestContext: Pointer;
                              ControlGuid: TGUID;
                              GuidCount: ULONG;
                              TraceGuidReg: TRACE_GUID_REGISTRATION;
                              MofImagePath: LPCSTR;
                              MofResourceName: LPCSTR;
                              out RegistrationHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM RegisterTraceGuids}


  function RegisterTraceGuidsW(RequestAddress: WMIDPREQUEST;
                               RequestContext: Pointer;
                               const ControlGuid: TGUID;
                               GuidCount: ULONG;
                               TraceGuidReg: TRACE_GUID_REGISTRATION;
                               MofImagePath: PWideChar;
                               MofResourceName: PWideChar;
                               out RegistrationHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM RegisterTraceGuidsW}


  function RegisterTraceGuidsA(RequestAddress: WMIDPREQUEST;
                               RequestContext: Pointer;
                               const ControlGuid: TGUID;
                               GuidCount: ULONG;
                               TraceGuidReg: TRACE_GUID_REGISTRATION;
                               MofImagePath: PAnsiChar;
                               MofResourceName: PAnsiChar;
                               out RegistrationHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM RegisterTraceGuidsA}


//#if (WINVER >= _WIN32_WINNT_WINXP)
  function EnumerateTraceGuids(var GuidPropertiesArray: TRACE_GUID_PROPERTIES;
                               PropertyArrayCount: ULONG;
                               out GuidCount: ULONG): ULONG; stdcall;
  {$EXTERNALSYM EnumerateTraceGuids}
//#endif



  function UnregisterTraceGuids(RegistrationHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM UnregisterTraceGuids}

  function GetTraceLoggerHandle (Buffer: Pointer): TRACEHANDLE stdcall;
  {$EXTERNALSYM GetTraceLoggerHandle}

  function GetTraceEnableLevel(TraceHandle: TRACEHANDLE): UCHAR; stdcall;
  {$EXTERNALSYM GetTraceEnableLevel}

  function GetTraceEnableFlags(TraceHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM GetTraceEnableFlags}

type

  // Structures and enums for QueryTraceProcessingHandle
  // ===================================================

  PETW_PROCESS_HANDLE_INFO_TYPE = ^ETW_PROCESS_HANDLE_INFO_TYPE;
  _ETW_PROCESS_HANDLE_INFO_TYPE  = (
    EtwQueryPartitionInformation = 1,
    EtwQueryProcessHandleInfoMax = 2);
  {$EXTERNALSYM _ETW_PROCESS_HANDLE_INFO_TYPE}
  ETW_PROCESS_HANDLE_INFO_TYPE = _ETW_PROCESS_HANDLE_INFO_TYPE;
  {$EXTERNALSYM ETW_PROCESS_HANDLE_INFO_TYPE}

  PETW_TRACE_PARTITION_INFORMATION = ^ETW_TRACE_PARTITION_INFORMATION;
  _ETW_TRACE_PARTITION_INFORMATION = record
    PartitionId: TGUID;
    ParentId: TGUID;
    QpcOffsetFromRoot: ULONG64;
    PartitionType: ULONG;
  end;
  {$EXTERNALSYM _ETW_TRACE_PARTITION_INFORMATION}
  ETW_TRACE_PARTITION_INFORMATION = _ETW_TRACE_PARTITION_INFORMATION;
  {$EXTERNALSYM ETW_TRACE_PARTITION_INFORMATION}

  PETW_TRACE_PARTITION_INFORMATION_V2 = ^_ETW_TRACE_PARTITION_INFORMATION_V2;
  _ETW_TRACE_PARTITION_INFORMATION_V2 = record
    QpcOffsetFromRoot: LONG64;
    PartitionType: ULONG;
    PartitionId: PWideChar;
    ParentId: PWideChar;
  end;
  {$EXTERNALSYM _ETW_TRACE_PARTITION_INFORMATION_V2}
  ETW_TRACE_PARTITION_INFORMATION_V2 = _ETW_TRACE_PARTITION_INFORMATION_V2;
  {$EXTERNALSYM ETW_TRACE_PARTITION_INFORMATION_V2}



  function QueryTraceProcessingHandle(const ProcessingHandle: TRACEHANDLE;
                                      InformationClass: ETW_PROCESS_HANDLE_INFO_TYPE;
                                      InBuffer: Pointer;
                                      InBufferSize: ULONG;
                                      out OutBuffer: Pointer;
                                      OutBufferSize: ULONG;
                                      var ReturnLength: ULONG): ULONG; stdcall;
  {$EXTERNALSYM QueryTraceProcessingHandle}


  // Data Consumer APIs and structures start here
  // ============================================


  function OpenTraceW(var Logfile: PEVENT_TRACE_LOGFILEW): TRACEHANDLE; stdcall;
  {$EXTERNALSYM OpenTraceW}

  function ProcessTrace(HandleArray: TRACEHANDLE;
                        HandleCount: ULONG;
                        StartTime: FILETIME;
                        EndTime: FILETIME): ULONG; stdcall;
  {$EXTERNALSYM ProcessTrace}

  function CloseTrace(_TraceHandle: TRACEHANDLE): ULONG; stdcall;
  {$EXTERNALSYM CloseTrace}

  function SetTraceCallback(const pGuid: PGUID;
                            EventCallback: EVENT_CALLBACK): ULONG; stdcall;
  {$EXTERNALSYM SetTraceCallback}

  function RemoveTraceCallback(const pGuid: TGUID): ULONG; stdcall;
  {$EXTERNALSYM RemoveTraceCallback}


  // The routines for tracing Messages follow
  // ========================================

  function TraceMessage(LoggerHandle: TRACEHANDLE;
                        MessageFlags: ULONG;
                        const MessageGuid: TGUID;
                        MessageNumber: USHORT): ULONG; stdcall;
  {$EXTERNALSYM TraceMessage}

  function TraceMessageVa(LoggerHandle: TRACEHANDLE;
                          MessageFlags: ULONG;
                          const MessageGuid: TGUID;
                          MessageNumber: USHORT;
                          MessageArgList: va_list): ULONG; cdecl;
  {$EXTERNALSYM TraceMessageVa}



const

  INVALID_PROCESSTRACE_HANDLE = TRACEHANDLE(INVALID_HANDLE_VALUE);
  {$EXTERNALSYM INVALID_PROCESSTRACE_HANDLE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const

  // Sechost.dll on Windows 8.1 and Windows Server 2012 R2;
  EvntraceLib = 'Sechost.dll';
  // Advapi32.dll on Windows 8, Windows Server 2012, Windows 7,
  // Windows Server 2008 R2, Windows Server 2008, Windows Vista and Windows XP
  // EvntraceLib = 'Advapi32.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function StartTrace; external EvntraceLib name 'StartTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StartTraceW; external EvntraceLib name 'StartTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StartTraceA; external EvntraceLib name 'StartTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StopTrace; external EvntraceLib name 'StopTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StopTraceW; external EvntraceLib name 'StopTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function StopTraceA; external EvntraceLib name 'StopTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryTrace; external EvntraceLib name 'QueryTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryTraceW; external EvntraceLib name 'QueryTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryTraceA; external EvntraceLib name 'QueryTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UpdateTrace; external EvntraceLib name 'UpdateTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UpdateTraceW; external EvntraceLib name 'UpdateTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UpdateTraceA; external EvntraceLib name 'UpdateTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function FlushTrace; external EvntraceLib name 'FlushTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function FlushTraceW; external EvntraceLib name 'FlushTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function FlushTraceA; external EvntraceLib name 'FlushTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function ControlTrace; external EvntraceLib name 'ControlTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function ControlTraceW; external EvntraceLib name 'ControlTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function ControlTraceA; external EvntraceLib name 'ControlTraceA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryAllTraces; external EvntraceLib name 'QueryAllTraces' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryAllTracesW; external EvntraceLib name 'QueryAllTracesW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryAllTracesA; external EvntraceLib name 'QueryAllTracesA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnableTrace; external EvntraceLib name 'EnableTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnableTraceEx; external EvntraceLib name 'EnableTraceEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnableTraceEx2; external EvntraceLib name 'EnableTraceEx2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceSetInformation; external EvntraceLib name 'TraceSetInformation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceQueryInformation; external EvntraceLib name 'TraceQueryInformation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function QueryTraceProcessingHandle; external EvntraceLib name 'QueryTraceProcessingHandle' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceMessage; external EvntraceLib name 'TraceMessage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceMessageVa; external EvntraceLib name 'TraceMessageVa' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnumerateTraceGuidsEx; external EvntraceLib name 'EnumerateTraceGuidsEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CreateTraceInstanceId; external EvntraceLib name 'CreateTraceInstanceId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceEvent; external EvntraceLib name 'TraceEvent' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function TraceEventInstance; external EvntraceLib name 'TraceEventInstance' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function RegisterTraceGuids; external EvntraceLib name 'RegisterTraceGuids' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function RegisterTraceGuidsW; external EvntraceLib name 'RegisterTraceGuidsW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function RegisterTraceGuidsA; external EvntraceLib name 'RegisterTraceGuidsA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnumerateTraceGuids; external EvntraceLib name 'EnumerateTraceGuids' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UnregisterTraceGuids; external EvntraceLib name 'UnregisterTraceGuids' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetTraceLoggerHandle; external EvntraceLib name 'GetTraceLoggerHandle' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetTraceEnableLevel; external EvntraceLib name 'GetTraceEnableLevel' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetTraceEnableFlags; external EvntraceLib name 'GetTraceEnableFlags' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function OpenTraceW; external EvntraceLib name 'OpenTraceW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function ProcessTrace; external EvntraceLib name 'ProcessTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CloseTrace; external EvntraceLib name 'CloseTrace' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function SetTraceCallback; external EvntraceLib name 'SetTraceCallback' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function RemoveTraceCallback ; external EvntraceLib name 'RemoveTraceCallback' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
