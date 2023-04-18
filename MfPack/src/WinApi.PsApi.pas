// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: System.PsApi.pas
// Kind: Pascal / Delphi unit
// Release date: 16-04-2023
// Language: ENU
//
// Revision Version: 3.1.5
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
// 16/04/2023 All                 Pixies release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//          BUILD Version: 0001    // Increment this if a change has global effects
//          File for APIs provided by PSAPI.DLL. 
//          (Note that this API is NOT part of Kernel32.dll as MS docs says.)
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: psapi.h
//
// Copyright (c) 1994-1999  Microsoft Corporation.
// Author: Richard Shupak   [richards]  06-Jan-1994.
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

unit WinApi.PsApi;

  {$HPPEMIT '#include "psapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const
  LIST_MODULES_DEFAULT                = $0;  // This is the default one app would get without any flag.
  {$EXTERNALSYM LIST_MODULES_DEFAULT}
  LIST_MODULES_32BIT                  = $01;  // list 32bit modules in the target process.
  {$EXTERNALSYM LIST_MODULES_64BIT}
  LIST_MODULES_64BIT                  = $02;  // list all 64bit modules. 32bit exe will be stripped off.
  {$EXTERNALSYM LIST_MODULES_32BIT}

  // list all the modules
  LIST_MODULES_ALL                    = (LIST_MODULES_32BIT or LIST_MODULES_64BIT);
  {$EXTERNALSYM LIST_MODULES_ALL}

  PSAPI_VERSION = 2;  // >= Win 7


  function EnumProcesses(out lpidProcess: DWORD;
                         cb: DWORD;
                         out lpcbNeeded: PDWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcesses}


  function EnumProcessModules(const hProcess: THandle;
                              out lphModule: HMODULE;
                              cb: DWORD;
                              out lpcbNeeded: PDWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcessModules}

  function EnumProcessModulesEx(const hProcess: THandle;
                                out lphModule: HMODULE;
                                cb: DWORD;
                                out lpcbNeeded: PDWORD;
                                dwFilterFlag: DWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcessModulesEx}

  function GetModuleBaseName(const hProcess: THandle;
                              {In_opt} hModule: HMODULE;
                              out lpBaseName: PWideChar;
                              nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetModuleBaseName}

  function GetModuleBaseNameA(const hProcess: THandle;
                              {In_opt} hModule: HMODULE;
                              out lpBaseName: PAnsiChar;
                              nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetModuleBaseNameA}

  function GetModuleBaseNameW(const hProcess: THandle;
                              {In_opt} hModule: HMODULE;
                              out lpBaseName: PWideChar;
                              nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetModuleBaseNameW}


  function GetModuleFileNameEx({In_opt} hProcess: THandle;
                               {In_opt} hModule: HMODULE;
                               out lpFilename: PWideChar;
                               nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetModuleFileNameEx}

  function GetModuleFileNameExA({In_opt} hProcess: THandle;
                                {In_opt} hModule: HMODULE;
                                out lpFilename: PAnsiChar;
                                nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetModuleFileNameExA}

  function GetModuleFileNameExW({In_opt} hProcess: THandle;
                                {In_opt} hModule: HMODULE;
                                out lpFilename: PWideChar;
                                nSize: DWORD): DWord; stdcall; // Success > 0
  {$EXTERNALSYM GetModuleFileNameExW}


type

  PMODULEINFO = ^MODULEINFO;
  _MODULEINFO = record
    lpBaseOfDll: Pointer;
    SizeOfImage: DWORD;
    EntryPoint: Pointer;
  end;
  {$EXTERNALSYM _MODULEINFO}
  MODULEINFO = _MODULEINFO;
  {$EXTERNALSYM MODULEINFO}
  LPMODULEINFO = ^_MODULEINFO;
  {$EXTERNALSYM LPMODULEINFO}


  function GetModuleInformation(const hProcess: THandle;
                                hModule: HMODULE;
                                out lpmodinfo: LPMODULEINFO;
                                cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetModuleInformation}

  function EmptyWorkingSet(const hProcess: THandle): BOOL; stdcall;
  {$EXTERNALSYM EmptyWorkingSet}

  function InitializeProcessForWsWatch(const hProcess: THandle): BOOL; stdcall;
  {$EXTERNALSYM InitializeProcessForWsWatch}


type

  _PSAPI_WS_WATCH_INFORMATION = record
    FaultingPc: Pointer;
    FaultingVa: Pointer;
  end;
  {$EXTERNALSYM _PSAPI_WS_WATCH_INFORMATION}
  PSAPI_WS_WATCH_INFORMATION = _PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PSAPI_WS_WATCH_INFORMATION}
  PPSAPI_WS_WATCH_INFORMATION = ^_PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PPSAPI_WS_WATCH_INFORMATION}


  _PSAPI_WS_WATCH_INFORMATION_EX = record
    BasicInfo: PSAPI_WS_WATCH_INFORMATION;
    FaultingThreadId: ULONG_PTR;
    Flags: ULONG_PTR;               // Reserved
  end;
  {$EXTERNALSYM _PSAPI_WS_WATCH_INFORMATION_EX}
  PSAPI_WS_WATCH_INFORMATION_EX = _PSAPI_WS_WATCH_INFORMATION_EX;
  {$EXTERNALSYM PSAPI_WS_WATCH_INFORMATION_EX}
  PPSAPI_WS_WATCH_INFORMATION_EX = ^_PSAPI_WS_WATCH_INFORMATION_EX;
  {$EXTERNALSYM PPSAPI_WS_WATCH_INFORMATION_EX}



  function GetWsChanges(const hProcess: THandle;
                        out lpWatchInfo: PPSAPI_WS_WATCH_INFORMATION;
                        cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetWsChanges}

  function GetWsChangesEx(const hProcess: THandle;
                          out lpWatchInfoEx: PPSAPI_WS_WATCH_INFORMATION_EX;
                          var cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetWsChangesEx}


  function GetMappedFileName(const hProcess: THandle;
                             lpv: Pointer;
                             out lpFilename: PWideChar;
                             nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetMappedFileName}

  function GetMappedFileNameW(const hProcess: THandle;
                              lpv: Pointer;
                              out lpFilename: PWideChar;
                              nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetMappedFileNameW}

  function GetMappedFileNameA(const hProcess: THandle;
                              lpv: Pointer;
                              out lpFilename: PAnsiChar;
                              nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetMappedFileNameA}


  function EnumDeviceDrivers(out lpImageBase: Pointer;
                             cb: DWORD;
                             out lpcbNeeded: PDWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumDeviceDrivers}


  function GetDeviceDriverBaseName(ImageBase: Pointer;
                                    out lpBaseName: PWideChar;
                                    nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseName}

  function GetDeviceDriverBaseNameA(ImageBase: Pointer;
                                    out lpFilename: PAnsiChar;
                                    nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseNameA}

  function GetDeviceDriverBaseNameW(ImageBase: Pointer;
                                    out lpBaseName: PWideChar;
                                    nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseNameW}


  function GetDeviceDriverFileName(ImageBase: Pointer;
                                   out lpFilename: PWideChar;
                                   nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileName}

  function GetDeviceDriverFileNameA(ImageBase: Pointer;
                                    out lpFilename: PAnsiChar;
                                    nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileNameA}

  function GetDeviceDriverFileNameW(ImageBase: Pointer;
                                    out lpFilename: PWideChar;
                                    nSize: DWORD): DWord; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileNameW}


//
// Working set information structures. All non-specified bits are reserved.
//

type

  STRUCT_PSAPI_WORKING_SET_BLOCK = record
  private
    Flags: ULONG_PTR;
    function GetBits(const aIndex: Integer): ULONG_PTR;
    procedure SetBits(const aIndex: Integer;
                      const aValue: ULONG_PTR);

  public
    property Protection: ULONG_PTR  index $0005 read GetBits write SetBits;    // 5 bits at offset 0
    property ShareCount: ULONG_PTR  index $0503 read GetBits write SetBits;    // 3 bits at offset 5
    property Shared: ULONG_PTR      index $0801 read GetBits write SetBits;    // 1 bit at offset 8
    property Reserved: ULONG_PTR    index $0903 read GetBits write SetBits;    // 3 bits at offset 9
    {$IFDEF WIN64}
    property VirtualPage: ULONG_PTR index $1253 read GetBits write SetBits;    // 52 bits at offset 12
    {$ELSE}
    property VirtualPage: ULONG_PTR index $1220 read GetBits write SetBits;    // 20 bits at offset 9
    {$ENDIF}
  end;

  PSAPI_WORKING_SET_BLOCK = record
  private
    Flags: ULONG_PTR;
  public
    case Integer of
      0: ( struct: STRUCT_PSAPI_WORKING_SET_BLOCK );
    end;
  {$EXTERNALSYM PSAPI_WORKING_SET_BLOCK}
  PPSAPI_WORKING_SET_BLOCK = ^PSAPI_WORKING_SET_BLOCK;
  {$EXTERNALSYM PPSAPI_WORKING_SET_BLOCK}



  _PSAPI_WORKING_SET_INFORMATION = record
    NumberOfEntries: ULONG_PTR;
    WorkingSetInfo: array[0..0] of PSAPI_WORKING_SET_BLOCK;
  end;
  {$EXTERNALSYM _PSAPI_WORKING_SET_INFORMATION}
  PSAPI_WORKING_SET_INFORMATION = _PSAPI_WORKING_SET_INFORMATION;
  {$EXTERNALSYM PSAPI_WORKING_SET_INFORMATION}
  PPSAPI_WORKING_SET_INFORMATION = ^_PSAPI_WORKING_SET_INFORMATION;
  {$EXTERNALSYM PPSAPI_WORKING_SET_INFORMATION}


type

  STRUCT_0_PSAPI_WORKING_SET_EX_BLOCK = record
  private
    Flags: ULONG_PTR;
    function GetBits(const aIndex: Integer): ULONG_PTR;
    procedure SetBits(const aIndex: Integer;
                      const aValue: ULONG_PTR);
  public
    property Valid: ULONG_PTR            index $0001 read GetBits write SetBits;  // 1 bit at offset 0
    property ShareCount: ULONG_PTR       index $0103 read GetBits write SetBits;  // 3 bits at offset 1
    property Win32Protection: ULONG_PTR  index $0411 read GetBits write SetBits;  // 11 bits at offset 4
    property Shared: ULONG_PTR           index $1501 read GetBits write SetBits;  // 1 bit at offset 15
    property Node: ULONG_PTR             index $1606 read GetBits write SetBits;  // 6 bits at offset 16
    property Locked: ULONG_PTR           index $2201 read GetBits write SetBits;  // 1 bit at offset 22
    property LargePage: ULONG_PTR        index $2301 read GetBits write SetBits;  // 1 bit at offset 23
    property Reserved: ULONG_PTR         index $2407 read GetBits write SetBits;  // 7 bits at offset 24
    property Bad: ULONG_PTR              index $3101 read GetBits write SetBits;  // 1 bit at offset 31
    {$IFDEF WIN64}
    property ReservedUlong: ULONG_PTR    index $3232 read GetBits write SetBits;  // 32 bits at offset 32
    {$ENDIF}
  end;

  STRUCT_1_PSAPI_WORKING_SET_EX_BLOCK = record
  private
    Flags: ULONG_PTR;
    function GetBits(const aIndex: Integer): ULONG_PTR;
    procedure SetBits(const aIndex: Integer;
                      const aValue: NativeUInt);
  public
    property Valid: ULONG_PTR            index $0001 read GetBits write SetBits;  // 1 bit at offset 0
    property Reserved0: ULONG_PTR        index $0114 read GetBits write SetBits;  // 14 bits at offset 1
    property Shared: ULONG_PTR           index $1501 read GetBits write SetBits;  // 1 bit at offset 15
    property Reserved1: ULONG_PTR        index $0005 read GetBits write SetBits;  // 15 bits at offset 16
    property Bad: ULONG_PTR              index $3101 read GetBits write SetBits;  // 1 bit at offset 31
    {$IFDEF WIN64}
    property ReservedUlong: ULONG_PTR    index $3232 read GetBits write SetBits;  // 32 bits at offset 32
    {$ENDIF}
  end;



  PSAPI_WORKING_SET_EX_BLOCK = record
    case Integer of
      0:  ( valid: STRUCT_0_PSAPI_WORKING_SET_EX_BLOCK );
      1:  ( invalid: STRUCT_1_PSAPI_WORKING_SET_EX_BLOCK );
    end;
  {$EXTERNALSYM PSAPI_WORKING_SET_EX_BLOCK}
  PPSAPI_WORKING_SET_EX_BLOCK = ^PSAPI_WORKING_SET_EX_BLOCK;
  {$EXTERNALSYM PPSAPI_WORKING_SET_EX_BLOCK}


  _PSAPI_WORKING_SET_EX_INFORMATION = record
    VirtualAddress: Pointer;
    VirtualAttributes: PSAPI_WORKING_SET_EX_BLOCK;
  end;
   {$EXTERNALSYM _PSAPI_WORKING_SET_EX_INFORMATION}
  PSAPI_WORKING_SET_EX_INFORMATION = _PSAPI_WORKING_SET_EX_INFORMATION;
  {$EXTERNALSYM PSAPI_WORKING_SET_EX_INFORMATION}
  PPSAPI_WORKING_SET_EX_INFORMATION = ^_PSAPI_WORKING_SET_EX_INFORMATION;
  {$EXTERNALSYM PPSAPI_WORKING_SET_EX_INFORMATION}


  function QueryWorkingSet(const hProcess: THandle;
                           out pv: Pointer;
                           cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM QueryWorkingSet}

  function QueryWorkingSetEx(const hProcess: THandle;
                             out pv;
                             cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM QueryWorkingSetEx}


type
  // Structure for GetProcessMemoryInfo()

  _PROCESS_MEMORY_COUNTERS = record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
  end;
  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS}
  PROCESS_MEMORY_COUNTERS = _PROCESS_MEMORY_COUNTERS;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS}
  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS}

// #if (_WIN32_WINNT >= 0x0501)


  _PROCESS_MEMORY_COUNTERS_EX = record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivateUsage: SIZE_T;
  end;
  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS_EX}
  PROCESS_MEMORY_COUNTERS_EX = _PROCESS_MEMORY_COUNTERS_EX;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS_EX}
  PPROCESS_MEMORY_COUNTERS_EX = ^PROCESS_MEMORY_COUNTERS_EX;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS_EX}

  function GetProcessMemoryInfo(const Process: THandle;
                                ppsmemCounters: PROCESS_MEMORY_COUNTERS;
                                cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetProcessMemoryInfo}

type

  _PERFORMANCE_INFORMATION = record
    cb: DWORD;
    CommitTotal: SIZE_T;
    CommitLimit: SIZE_T;
    CommitPeak: SIZE_T;
    PhysicalTotal: SIZE_T;
    PhysicalAvailable: SIZE_T;
    SystemCache: SIZE_T;
    KernelTotal: SIZE_T;
    KernelPaged: SIZE_T;
    KernelNonpaged: SIZE_T;
    PageSize: SIZE_T;
    HandleCount: DWord;
    ProcessCount: DWord;
    ThreadCount: DWord;
  end;
  {$EXTERNALSYM _PERFORMANCE_INFORMATION}
  PERFORMANCE_INFORMATION = _PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PERFORMANCE_INFORMATION}
  PPERFORMANCE_INFORMATION = ^_PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PPERFORMANCE_INFORMATION}
  PERFORMACE_INFORMATION = _PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PERFORMACE_INFORMATION}
  PPERFORMACE_INFORMATION = ^_PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PPERFORMACE_INFORMATION}

  function GetPerformanceInfo(pPerformanceInformation: PERFORMANCE_INFORMATION;
                              cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetPerformanceInfo}


type

  PEnumPageFileInformation = ^TEnumPageFileInformation;
  {$EXTERNALSYM _ENUM_PAGE_FILE_INFORMATION}
  _ENUM_PAGE_FILE_INFORMATION = record
    cb: DWORD;
    Reserved: DWORD;
    TotalSize: SIZE_T;
    TotalInUse: SIZE_T;
    PeakUsage: SIZE_T;
  end;
  {$EXTERNALSYM ENUM_PAGE_FILE_INFORMATION}
  ENUM_PAGE_FILE_INFORMATION = _ENUM_PAGE_FILE_INFORMATION;
  {$EXTERNALSYM PENUM_PAGE_FILE_INFORMATION}
  PENUM_PAGE_FILE_INFORMATION = ^_ENUM_PAGE_FILE_INFORMATION;
  TEnumPageFileInformation = _ENUM_PAGE_FILE_INFORMATION;


// Callbacks
type


  PENUM_PAGE_FILE_CALLBACKW = function(pContext: Pointer;
                                       pPageFileInfo: PENUM_PAGE_FILE_INFORMATION;
                                       lpFilename: PWideChar): BOOL; stdcall;

  PENUM_PAGE_FILE_CALLBACKA = function(pContext: Pointer;
                                       pPageFileInfo: PENUM_PAGE_FILE_INFORMATION;
                                       lpFilename: PAnsiChar): BOOL; stdcall;


  function EnumPageFiles(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACKW;
                         pContext: Pointer): BOOL; stdcall;
  {$EXTERNALSYM EnumPageFiles}

  function EnumPageFilesW(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACKW;
                          pContext: Pointer): BOOL; stdcall;
  {$EXTERNALSYM EnumPageFilesW}

  function EnumPageFilesA(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACKA;
                          pContext: Pointer): BOOL; stdcall;
  {$EXTERNALSYM EnumPageFilesA}


  function GetProcessImageFileName(const hProcess: THandle;
                                    out lpImageFileName: PWideChar;
                                    nSize: DWord): DWord; stdcall;
  {$EXTERNALSYM GetProcessImageFileName}

  function GetProcessImageFileNameA(const hProcess: THandle;
                                    out lpImageFileName: PAnsiChar;
                                    nSize: DWord): DWord; stdcall;
  {$EXTERNALSYM GetProcessImageFileNameA}


  function GetProcessImageFileNameW(const hProcess: THandle;
                                    out lpImageFileName: PWideChar;
                                    nSize: DWord): DWord; stdcall;
  {$EXTERNALSYM GetProcessImageFileNameW}



implementation

const
  PsApiLib = 'psapi.dll';

{$WARN SYMBOL_PLATFORM OFF}
function EnumProcesses; external PsApiLib name 'K32EnumProcesses' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EnumProcessModules; external PsApiLib name 'K32EnumProcessModules' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EnumProcessModulesEx; external PsApiLib name 'K32EnumProcessModulesEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetModuleBaseName; external PsApiLib name 'K32GetModuleBaseNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetModuleBaseNameA; external PsApiLib name 'K32GetModuleBaseNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetModuleBaseNameW; external PsApiLib name 'K32GetModuleBaseNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetModuleFileNameEx; external PsApiLib name 'K32GetModuleFileNameExW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetModuleFileNameExA; external PsApiLib name 'K32GetModuleFileNameExA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetModuleFileNameExW; external PsApiLib name 'K32GetModuleFileNameExW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetModuleInformation; external PsApiLib name 'K32GetModuleInformation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EmptyWorkingSet; external PsApiLib name 'K32EmptyWorkingSet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function InitializeProcessForWsWatch; external PsApiLib name 'K32InitializeProcessForWsWatch' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetWsChanges; external PsApiLib name 'K32GetWsChanges' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetWsChangesEx; external PsApiLib name 'K32GetWsChangesEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetMappedFileName; external PsApiLib name 'K32GetMappedFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetMappedFileNameW; external PsApiLib name 'K32GetMappedFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetMappedFileNameA; external PsApiLib name 'K32GetMappedFileNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function EnumDeviceDrivers; external PsApiLib name 'K32EnumDeviceDrivers' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetDeviceDriverBaseName; external PsApiLib name 'K32GetDeviceDriverBaseNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetDeviceDriverBaseNameA; external PsApiLib name 'K32GetDeviceDriverBaseNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetDeviceDriverBaseNameW; external PsApiLib name 'K32GetDeviceDriverBaseNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetDeviceDriverFileName; external PsApiLib name 'K32GetDeviceDriverFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetDeviceDriverFileNameA; external PsApiLib name 'K32GetDeviceDriverFileNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetDeviceDriverFileNameW; external PsApiLib name 'K32GetDeviceDriverFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function QueryWorkingSet; external PsApiLib name 'K32QueryWorkingSet' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function QueryWorkingSetEx; external PsApiLib name 'K32QueryWorkingSetEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetProcessMemoryInfo; external PsApiLib name 'K32GetProcessMemoryInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetPerformanceInfo; external PsApiLib name 'K32GetPerformanceInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function EnumPageFiles; external PsApiLib name 'K32EnumPageFilesW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EnumPageFilesW; external PsApiLib name 'K32EnumPageFilesW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function EnumPageFilesA; external PsApiLib name 'K32EnumPageFilesA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

function GetProcessImageFileName; external PsApiLib name 'K32GetProcessImageFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetProcessImageFileNameA; external PsApiLib name 'K32GetProcessImageFileNameA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetProcessImageFileNameW; external PsApiLib name 'K32GetProcessImageFileNameW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}


// Record helpers //////////////////////////////////////////////////////////////

function GetUBits(const Bits: ULONG_PTR;
                  const aIndex: Integer): ULONG_PTR;
begin
  Result := (Bits shr (aIndex shr 8))        // offset
             and ((1 shl Byte(aIndex)) - 1); // mask
end;

procedure SetUBits(var Bits: ULONG_PTR;
                   const aIndex: Integer;
                   const aValue: ULONG_PTR);
var
  Offset: Byte;
  Mask: Integer;

begin
  Mask := ((1 shl Byte(aIndex)) - 1);
  Assert(aValue <= Mask);

  Offset := aIndex shr 8;
  Bits := (Bits and (not (Mask shl Offset)))
          or DWORD(aValue shl Offset);
end;


// PSAPI_WORKING_SET_BLOCK /////////////////////////////////////////////////////
function STRUCT_PSAPI_WORKING_SET_BLOCK.GetBits(const aIndex: Integer): ULONG_PTR;
begin
  Result := GetUBits(Flags,
                     aIndex);
end;

procedure STRUCT_PSAPI_WORKING_SET_BLOCK.SetBits(const aIndex: Integer;
                                                 const aValue: ULONG_PTR);
begin
  SetUBits(Flags,
           aIndex,
           aValue);
end;
// /////////////////////////////////////////////////////////////////////////////

// PSAPI_WORKING_SET_EX_BLOCK //////////////////////////////////////////////////
function STRUCT_0_PSAPI_WORKING_SET_EX_BLOCK.GetBits(const aIndex: Integer): ULONG_PTR;
begin
  Result := GetUBits(Flags,
                     aIndex);
end;

procedure STRUCT_0_PSAPI_WORKING_SET_EX_BLOCK.SetBits(const aIndex: Integer;
                                                      const aValue: ULONG_PTR);
begin
  SetUBits(Flags,
           aIndex,
           aValue);
end;


function STRUCT_1_PSAPI_WORKING_SET_EX_BLOCK.GetBits(const aIndex: Integer): ULONG_PTR;
begin
  Result := GetUBits(Flags,
                     aIndex);
end;

procedure STRUCT_1_PSAPI_WORKING_SET_EX_BLOCK.SetBits(const aIndex: Integer;
                                                      const aValue: ULONG_PTR);
begin
  SetUBits(Flags,
           aIndex,
           aValue);
end;

// /////////////////////////////////////////////////////////////////////////////

end.
