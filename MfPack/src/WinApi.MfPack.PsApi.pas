// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MfPack.PsApi.pas
// Kind: Pascal / Delphi unit
// Release date: 16-04-2023
// Language: ENU
//
// Revision Version: 4.1.0
// Description: -
//
// Organisation: FactoryX                          `
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
// Remarks: The process status application programming interface (PSAPI) is a helper library that
//          makes it easier for you to obtain information about processes and device drivers.
//
//          Requiress Windows 7 or higher.
//
//          Version: 2
//
//          File for APIs provided by PSAPI.DLL.
//          Notes: - The version included up to Delphi 11 is not complete ie missing some records and
//                   64 bit function EnumProcessModulesEx.
//                 - We have to use run-time dynamic linking for this API and therefore
//                   we have to load psapi.dll (See hPSAPI := LoadLibrary(PsApiLib)).
//                   https://learn.microsoft.com/en-us/windows/win32/psapi/process-status-helper.
//
// Related objects: -
// Related projects: MfPackX410
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

unit WinApi.MfPack.PsApi;

  {$HPPEMIT '#include "psapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const
  // See: https://learn.microsoft.com/en-us/windows/win32/api/psapi/nf-psapi-enumprocessmodulesex
  LIST_MODULES_DEFAULT                = $0;  // This is the default one app would get without any flag.
  {$EXTERNALSYM LIST_MODULES_DEFAULT}
  LIST_MODULES_32BIT                  = $01;  // list 32bit modules in the target process.
  {$EXTERNALSYM LIST_MODULES_32BIT}
  LIST_MODULES_64BIT                  = $02;  // list all 64bit modules. 32bit exe will be stripped off.
  {$EXTERNALSYM LIST_MODULES_64BIT}

  // list all the modules
  LIST_MODULES_ALL                    = (LIST_MODULES_32BIT or LIST_MODULES_64BIT);
  {$EXTERNALSYM LIST_MODULES_ALL}

// NTDDI_VERSION >= NTDDI_WIN7
  PSAPI_VERSION                       = 2;
  {$EXTERNALSYM PSAPI_VERSION}

type

  PPointer = ^Pointer;

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



  TGetPerformanceInfo = function(pPerformanceInformation: PPERFORMANCE_INFORMATION;
                                 cb: DWORD): BOOL; stdcall;

  TEnumProcesses = function(lpidProcess: LPDWORD;
                            cb: DWORD;
                            var cbNeeded: DWORD): BOOL; stdcall;
  // 32 bit os
  TEnumProcessModules = function(hProcess: THandle;
                                 lphModule: PHMODULE;
                                 cb: DWORD;
                                 var lpcbNeeded: DWORD): BOOL; stdcall;
  // 64 bit os
  TEnumProcessModulesEx = function(hProcess: THandle;
                             {out} lphModule: PHMODULE;
                                   cb: DWORD;
                             {out} var lpcbNeeded: DWORD;
                                   dwFilterFlag: DWORD): BOOL; stdcall;

  TGetModuleBaseNameA = function(hProcess: THandle;
                                 hModule: HMODULE;
                                 lpBaseName: LPSTR;
                                 nSize: DWORD): DWORD; stdcall;

  TGetModuleBaseNameW = function(hProcess: THandle;
                                 hModule: HMODULE;
                                 lpBaseName: LPWSTR;
                                 nSize: DWORD): DWORD; stdcall;

  TGetModuleBaseName = TGetModuleBaseNameW; // Default is Unicode

  TGetModuleFileNameExA = function(hProcess: THandle;
                                   hModule: HMODULE;
                                   lpFilename: LPSTR;
                                   nSize: DWORD): DWORD; stdcall;

  TGetModuleFileNameExW = function(hProcess: THandle;
                                   hModule: HMODULE;
                                   lpFilename: LPWSTR;
                                   nSize: DWORD): DWORD; stdcall;

  TGetModuleFileNameEx = TGetModuleFileNameExW; // Default is Unicode


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


  TGetModuleInformation = function(hProcess: THandle;
                                   hModule: HMODULE;
                                   lpmodinfo: LPMODULEINFO;
                                   cb: DWORD): BOOL; stdcall;

  TEmptyWorkingSet = function(hProcess: THandle): BOOL; stdcall;

  TInitializeProcessForWsWatch = function(hProcess: THandle): BOOL; stdcall;


  _PSAPI_WS_WATCH_INFORMATION = record
    FaultingPc: Pointer;
    FaultingVa: Pointer;
  end;
  {$EXTERNALSYM _PSAPI_WS_WATCH_INFORMATION}
  PSAPI_WS_WATCH_INFORMATION = _PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PSAPI_WS_WATCH_INFORMATION}
  PPSAPI_WS_WATCH_INFORMATION = ^_PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PPSAPI_WS_WATCH_INFORMATION}

  TGetWsChanges = function(hProcess: THandle;
                           pWatchInfo: PPSAPI_WS_WATCH_INFORMATION;
                           cb: DWORD): BOOL; stdcall;

  TGetMappedFileNameA = function(hProcess: THandle;
                                 lpv: Pointer;
                                 {out} lpFilename: LPSTR;
                                 nSize: DWORD): DWORD; stdcall;


  TGetMappedFileNameW = function(hProcess: THandle;
                                 lpv: Pointer;
                                 {out} lpFilename: LPWSTR;
                                 nSize: DWORD): DWORD; stdcall;

  TGetMappedFileName = TGetMappedFileNameW; // Default is Unicode

  TGetDeviceDriverBaseNameA = function(ImageBase: Pointer;
                                       {out} lpBaseName: LPSTR;
                                       nSize: DWORD): DWORD; stdcall;

  TGetDeviceDriverBaseNameW = function(ImageBase: Pointer;
                                       {out} lpBaseName: LPWSTR;
                                       nSize: DWORD): DWORD; stdcall;

  TGetDeviceDriverBaseName = TGetDeviceDriverBaseNameW; // Default is Unicode

  TGetDeviceDriverFileNameA = function(ImageBase: Pointer;
                                       {out} lpFileName: LPSTR;
                                       nSize: DWORD): DWORD; stdcall;

  TGetDeviceDriverFileNameW = function(ImageBase: Pointer;
                                       {out} lpFileName: LPWSTR;
                                       nSize: DWORD): DWORD; stdcall;

  TGetDeviceDriverFileName = TGetDeviceDriverFileNameW; // Default is Unicode

  TEnumDeviceDrivers = function(lpImageBase: PPointer;
                                cb: DWORD;
                                var lpcbNeeded: DWORD): BOOL; stdcall;

//
// Working set information structures. All non-specified bits are reserved.
//

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
    property VirtualPage: ULONG_PTR index $0C34 read GetBits write SetBits;    // 52 bits at offset 12
    {$ELSE}
    property VirtualPage: ULONG_PTR index $0C14 read GetBits write SetBits;    // 20 bits at offset 12
    {$ENDIF}
  end;

  PSAPI_WORKING_SET_BLOCK = record
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


  STRUCT_0_PSAPI_WORKING_SET_EX_BLOCK = record
  private
    Flags: ULONG_PTR;
    function GetBits(const aIndex: Integer): ULONG_PTR;
    procedure SetBits(const aIndex: Integer;
                      const aValue: ULONG_PTR);
  public
    property Valid: ULONG_PTR            index $0001 read GetBits write SetBits;  // 1 bit at offset 0
    property ShareCount: ULONG_PTR       index $0103 read GetBits write SetBits;  // 3 bits at offset 1
    property Win32Protection: ULONG_PTR  index $040B read GetBits write SetBits;  // 11 bits at offset 4
    property Shared: ULONG_PTR           index $0F01 read GetBits write SetBits;  // 1 bit at offset 15
    property Node: ULONG_PTR             index $1006 read GetBits write SetBits;  // 6 bits at offset 16
    property Locked: ULONG_PTR           index $1601 read GetBits write SetBits;  // 1 bit at offset 22
    property LargePage: ULONG_PTR        index $1701 read GetBits write SetBits;  // 1 bit at offset 23
    property Reserved: ULONG_PTR         index $1807 read GetBits write SetBits;  // 7 bits at offset 24
    property Bad: ULONG_PTR              index $1F01 read GetBits write SetBits;  // 1 bit at offset 31
    {$IFDEF WIN64}
    property ReservedUlong: ULONG_PTR    index $2020 read GetBits write SetBits;  // 32 bits at offset 32
    {$ENDIF}
  end;

  STRUCT_1_PSAPI_WORKING_SET_EX_BLOCK = record
  private
    Flags: ULONG_PTR;
    function GetBits(const aIndex: Integer): ULONG_PTR;
    procedure SetBits(const aIndex: Integer;
                      const aValue: ULONG_PTR);
  public
    property Valid: ULONG_PTR            index $0001 read GetBits write SetBits;  // 1 bit at offset 0
    property Reserved0: ULONG_PTR        index $010E read GetBits write SetBits;  // 14 bits at offset 1
    property Shared: ULONG_PTR           index $0F01 read GetBits write SetBits;  // 1 bit at offset 15
    property Reserved1: ULONG_PTR        index $100F read GetBits write SetBits;  // 15 bits at offset 16
    property Bad: ULONG_PTR              index $1F01 read GetBits write SetBits;  // 1 bit at offset 31
    {$IFDEF WIN64}
    property ReservedUlong: ULONG_PTR    index $2020 read GetBits write SetBits;  // 32 bits at offset 32
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


  TQueryWorkingSet = function(hProcess: THandle;
                              pv: Pointer;
                              cb: DWORD): BOOL; stdcall;

  TQueryWorkingSetEx = function(hProcess: THandle;
                                pv: Pointer;
                                cb: DWORD): BOOL; stdcall;

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
  PPROCESS_MEMORY_COUNTERS = ^_PROCESS_MEMORY_COUNTERS;
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

  TGetProcessMemoryInfo = function (Process: THandle;
                                    ppsmemCounters: PPROCESS_MEMORY_COUNTERS;
                                    cb: DWORD): BOOL; stdcall;



  function GetPerformanceInfo(pPerformanceInformation: PPERFORMANCE_INFORMATION;
                              cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetPerformanceInfo}


  function EnumProcesses(lpidProcess: LPDWORD;
                         cb: DWORD;
                         var cbNeeded: DWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcesses}

  // 32 bit os
  function EnumProcessModules(const hProcess: THandle;
                              lphModule: PHMODULE;
                              cb: DWORD;
                              var lpcbNeeded: DWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcessModules}

  // 64 bit os
  function EnumProcessModulesEx(const hProcess: THandle;
                          {out} lphModule: PHMODULE;
                                cb: DWORD;
                                var lpcbNeeded: DWORD;
                                dwFilterFlag: DWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumProcessModulesEx}

  function GetModuleBaseName(hProcess: THandle;
                             hModule: HMODULE;
                             {out} lpBaseName: LPWSTR; nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleBaseName}

  function GetModuleBaseNameA(hProcess: THandle;
                              hModule: HMODULE;
                              {out} lpBaseName: LPSTR; nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleBaseNameA}

  function GetModuleBaseNameW(hProcess: THandle;
                              hModule: HMODULE;
                              {out} lpBaseName: LPWSTR; nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleBaseNameW}

  function GetModuleFileNameEx(hProcess: THandle;
                               hModule: HMODULE;
                               {out} lpFilename: LPWSTR; nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleFileNameEx}

  function GetModuleFileNameExA(hProcess: THandle;
                                hModule: HMODULE;
                                {out} lpFilename: LPSTR; nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleFileNameExA}

  function GetModuleFileNameExW(hProcess: THandle;
                                hModule: HMODULE;
                                {out} lpFilename: LPWSTR;
                                nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetModuleFileNameExW}

  function GetModuleInformation(hProcess: THandle;
                                hModule: HMODULE;
                                {out} lpmodinfo: LPMODULEINFO;
                                cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetModuleInformation}

  function EmptyWorkingSet(hProcess: THandle): BOOL; stdcall;
  {$EXTERNALSYM EmptyWorkingSet}

  function QueryWorkingSet(hProcess: THandle;
                           pv: Pointer;
                           cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM QueryWorkingSet}

  function InitializeProcessForWsWatch(hProcess: THandle): BOOL; stdcall;
  {$EXTERNALSYM InitializeProcessForWsWatch}

  function GetMappedFileName(hProcess: THandle;
                              lpv: Pointer;
                              {out} lpFilename: LPWSTR;
                              nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetMappedFileName}

  function GetMappedFileNameA(hProcess: THandle;
                              lpv: Pointer;
                              {outz} lpFilename: LPSTR;
                              nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetMappedFileNameA}

  function GetMappedFileNameW(hProcess: THandle;
                              lpv: Pointer;
                              {out} lpFilename: LPWSTR;
                              nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetMappedFileNameW}

  function GetDeviceDriverBaseName(ImageBase: Pointer;
                                   {out} lpBaseName: LPWSTR;
                                   nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseName}

  function GetDeviceDriverBaseNameA(ImageBase: Pointer;
                                    {out} lpBaseName: LPSTR;
                                    nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseNameA}

  function GetDeviceDriverBaseNameW(ImageBase: Pointer;
                                    {out} lpBaseName: LPWSTR;
                                    nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverBaseNameW}

  function GetDeviceDriverFileName(ImageBase: Pointer;
                                   {out} lpFileName: LPWSTR;
                                   nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileName}

  function GetDeviceDriverFileNameA(ImageBase: Pointer;
                                    {out} lpFileName: LPSTR;
                                    nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileNameA}

  function GetDeviceDriverFileNameW(ImageBase: Pointer;
                                    {out} lpFileName: LPWSTR;
                                    nSize: DWORD): DWORD; stdcall;
  {$EXTERNALSYM GetDeviceDriverFileNameW}

  function EnumDeviceDrivers({out} lpImageBase: PPointer;
                             cb: DWORD;
                             var lpcbNeeded: DWORD): BOOL; stdcall;
  {$EXTERNALSYM EnumDeviceDrivers}

  function GetProcessMemoryInfo(Process: THandle;
                                {out} ppsmemCounters: PPROCESS_MEMORY_COUNTERS;
                                cb: DWORD): BOOL; stdcall;
  {$EXTERNALSYM GetProcessMemoryInfo}



implementation

const
  PsApiLib = 'psapi.dll';

var
  hPSAPI: THandle;   // Do not forget to release when finished!
  PsApiLock: TRTLCriticalSection;
  _GetPerformanceInfo: TGetPerformanceInfo;
  _EnumProcesses: TEnumProcesses;
  _EnumProcessModules: TEnumProcessModules; // 32 bit
  _EnumProcessModulesEx: TEnumProcessModulesEx;  // 64 bit
  _GetModuleBaseName: TGetModuleBaseNameW;
  _GetModuleFileNameEx: TGetModuleFileNameExW;
  _GetModuleBaseNameA: TGetModuleBaseNameA;
  _GetModuleFileNameExA: TGetModuleFileNameExA;
  _GetModuleBaseNameW: TGetModuleBaseNameW;
  _GetModuleFileNameExW: TGetModuleFileNameExW;
  _GetModuleInformation: TGetModuleInformation;
  _EmptyWorkingSet: TEmptyWorkingSet;
  _QueryWorkingSet: TQueryWorkingSet;
  _InitializeProcessForWsWatch: TInitializeProcessForWsWatch;
  _GetMappedFileName: TGetMappedFileNameW;
  _GetDeviceDriverBaseName: TGetDeviceDriverBaseNameW;
  _GetDeviceDriverFileName: TGetDeviceDriverFileNameW;
  _GetMappedFileNameA: TGetMappedFileNameA;
  _GetDeviceDriverBaseNameA: TGetDeviceDriverBaseNameA;
  _GetDeviceDriverFileNameA: TGetDeviceDriverFileNameA;
  _GetMappedFileNameW: TGetMappedFileNameW;
  _GetDeviceDriverBaseNameW: TGetDeviceDriverBaseNameW;
  _GetDeviceDriverFileNameW: TGetDeviceDriverFileNameW;
  _EnumDeviceDrivers: TEnumDeviceDrivers;
  _GetProcessMemoryInfo: TGetProcessMemoryInfo;


procedure PsApiUnavailable;
begin
  if hPSAPI = 0 then
    SetLastError(ERROR_MOD_NOT_FOUND)
  else
    SetLastError(ERROR_PROC_NOT_FOUND);
end;

function CheckPSAPILoaded: Boolean;
begin
  EnterCriticalSection(PsApiLock);
  try

  if (hPSAPI = 0) then
    begin
      // Load the dll
      hPSAPI := LoadLibrary(PsApiLib);

      if (hPSAPI = 0) then
        begin
          hPSAPI := 0;
          Result := False;
          Exit;
        end;

      _GetPerformanceInfo := TGetPerformanceInfo(GetProcAddress(hPSAPI,
                                             'GetPerformanceInfo'));

      _EnumProcesses := TEnumProcesses(GetProcAddress(hPSAPI,
                                             'EnumProcesses'));

      // 32 bit os
      _EnumProcessModules := TEnumProcessModules(GetProcAddress(hPSAPI,
                                             'EnumProcessModules'));
      // 64 bit os
      _EnumProcessModulesEx := TEnumProcessModulesEx(GetProcAddress(hPSAPI,
                                             'EnumProcessModulesEx'));

      _GetModuleBaseName := TGetModuleBaseName(GetProcAddress(hPSAPI,
                                             'GetModuleBaseNameW'));

      _GetModuleFileNameEx := TGetModuleFileNameEx(GetProcAddress(hPSAPI,
                                             'GetModuleFileNameExW'));

      _GetModuleBaseNameA := TGetModuleBaseNameA(GetProcAddress(hPSAPI,
                                             'GetModuleBaseNameA'));

      _GetModuleFileNameExA := TGetModuleFileNameExA(GetProcAddress(hPSAPI,
                                             'GetModuleFileNameExA'));

      _GetModuleBaseNameW := TGetModuleBaseNameW(GetProcAddress(hPSAPI,
                                             'GetModuleBaseNameW'));

      _GetModuleFileNameExW := TGetModuleFileNameExW(GetProcAddress(hPSAPI,
                                             'GetModuleFileNameExW'));

      _GetModuleInformation := TGetModuleInformation(GetProcAddress(hPSAPI,
                                             'GetModuleInformation'));

      _EmptyWorkingSet := TEmptyWorkingSet(GetProcAddress(hPSAPI,
                                             'EmptyWorkingSet'));

      _QueryWorkingSet := TQueryWorkingSet(GetProcAddress(hPSAPI,
                                             'QueryWorkingSet'));

      _InitializeProcessForWsWatch := TInitializeProcessForWsWatch(GetProcAddress(hPSAPI,
                                             'InitializeProcessForWsWatch'));

      _GetMappedFileName := TGetMappedFileName(GetProcAddress(hPSAPI,
                                             'GetMappedFileNameW'));

      _GetDeviceDriverBaseName := TGetDeviceDriverBaseName(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverBaseNameW'));

      _GetDeviceDriverFileName := TGetDeviceDriverFileName(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverFileNameW'));

      _GetMappedFileNameA := TGetMappedFileNameA(GetProcAddress(hPSAPI,
                                             'GetMappedFileNameA'));

      _GetDeviceDriverBaseNameA := TGetDeviceDriverBaseNameA(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverBaseNameA'));

      _GetDeviceDriverFileNameA := TGetDeviceDriverFileNameA(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverFileNameA'));

      _GetMappedFileNameW := TGetMappedFileNameW(GetProcAddress(hPSAPI,
                                             'GetMappedFileNameW'));

      _GetDeviceDriverBaseNameW := TGetDeviceDriverBaseNameW(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverBaseNameW'));

      _GetDeviceDriverFileNameW := TGetDeviceDriverFileNameW(GetProcAddress(hPSAPI,
                                             'GetDeviceDriverFileNameW'));

      _EnumDeviceDrivers := TEnumDeviceDrivers(GetProcAddress(hPSAPI,
                                             'EnumDeviceDrivers'));

      _GetProcessMemoryInfo := TGetProcessMemoryInfo(GetProcAddress(hPSAPI,
                                             'GetProcessMemoryInfo'));

    end;
  Result := True;
  finally
    LeaveCriticalSection(PsApiLock);
  end;
end;


function GetPerformanceInfo(pPerformanceInformation: PPERFORMANCE_INFORMATION;
                            cb: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_GetPerformanceInfo) then
    Result := _GetPerformanceInfo(pPerformanceInformation,
                                  cb)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function EnumProcesses(lpidProcess: LPDWORD;
                        cb: DWORD;
                        var cbNeeded: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_EnumProcesses) then
    Result := _EnumProcesses(lpidProcess,
                             cb,
                             cbNeeded)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function EnumProcessModules(const hProcess: THandle;
                            lphModule: PHMODULE;
                            cb: DWORD;
                            var lpcbNeeded: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_EnumProcessModules) then
    Result := _EnumProcessModules(hProcess,
                                  lphModule,
                                  cb,
                                  lpcbNeeded)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function EnumProcessModulesEx(const hProcess: THandle;
                              lphModule: PHMODULE;
                              cb: DWORD;
                              var lpcbNeeded: DWORD;
                              dwFilterFlag: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_EnumProcessModulesEx) then
    Result := _EnumProcessModulesEx(hProcess,
                                    lphModule,
                                    cb,
                                    lpcbNeeded,
                                    dwFilterFlag)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;



function GetModuleBaseName(hProcess: THandle;
                           hModule: HMODULE;
                           lpBaseName: LPWSTR;
                           nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleBaseName) then
    Result := _GetModuleBaseName(hProcess,
                                 hModule,
                                 lpBaseName,
                                 nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleBaseNameA(hProcess: THandle;
                            hModule: HMODULE;
                            lpBaseName: LPSTR;
                            nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleBaseNameA) then
    Result := _GetModuleBaseNameA(hProcess,
                                  hModule,
                                  lpBaseName,
                                  nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleBaseNameW(hProcess: THandle;
                            hModule: HMODULE;
                            lpBaseName: LPWSTR;
                            nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleBaseNameW) then
    Result := _GetModuleBaseNameW(hProcess,
                                  hModule,
                                  lpBaseName,
                                  nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleFileNameEx(hProcess: THandle;
                             hModule: HMODULE;
                             lpFilename: LPWSTR;
                             nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleFileNameEx) then
    Result := _GetModuleFileNameEx(hProcess,
                                   hModule,
                                   lpFileName,
                                   nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleFileNameExA(hProcess: THandle;
                              hModule: HMODULE;
                              lpFilename: LPSTR;
                              nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleFileNameExA) then
    Result := _GetModuleFileNameExA(hProcess,
                                    hModule,
                                    lpFileName,
                                    nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleFileNameExW(hProcess: THandle;
                              hModule: HMODULE;
                              lpFilename: LPWSTR;
                              nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleFileNameExW) then
    Result := _GetModuleFileNameExW(hProcess,
                                    hModule,
                                    lpFileName,
                                    nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetModuleInformation(hProcess: THandle;
                              hModule: HMODULE;
                              lpmodinfo: LPMODULEINFO;
                              cb: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_GetModuleInformation) then
    Result := _GetModuleInformation(hProcess,
                                    hModule,
                                    lpmodinfo,
                                    cb)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function EmptyWorkingSet(hProcess: THandle): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_EmptyWorkingSet) then
    Result := _EmptyWorkingSet(hProcess)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function QueryWorkingSet(hProcess: THandle;
                         pv: Pointer;
                         cb: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_QueryWorkingSet) then
    Result := _QueryWorkingSet(hProcess,
                               pv,
                               cb)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function InitializeProcessForWsWatch(hProcess: THandle): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_InitializeProcessForWsWatch) then
    Result := _InitializeProcessForWsWatch(hProcess)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function GetMappedFileName(hProcess: THandle;
                           lpv: Pointer;
                           lpFilename: LPWSTR;
                           nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetMappedFileName) then
    Result := _GetMappedFileName(hProcess,
                                 lpv,
                                 lpFileName,
                                 nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetMappedFileNameA(hProcess: THandle;
                            lpv: Pointer;
                            lpFilename: LPSTR;
                            nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetMappedFileNameA) then
    Result := _GetMappedFileNameA(hProcess,
                                  lpv,
                                  lpFileName,
                                  nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetMappedFileNameW(hProcess: THandle;
                            lpv: Pointer;
                            lpFilename: LPWSTR;
                            nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetMappedFileNameW) then
    Result := _GetMappedFileNameW(hProcess,
                                  lpv,
                                  lpFileName,
                                  nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverBaseName(ImageBase: Pointer;
                                 lpBaseName: LPWSTR;
                                 nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverBaseName) then
    Result := _GetDeviceDriverBasename(ImageBase,
                                       lpBaseName,
                                       nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverBaseNameA(ImageBase: Pointer;
                                  lpBaseName: LPSTR;
                                  nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverBaseNameA) then
    Result := _GetDeviceDriverBasenameA(ImageBase,
                                        lpBaseName,
                                        nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverBaseNameW(ImageBase: Pointer;
                                  lpBaseName: LPWSTR;
                                  nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverBaseNameW) then
    Result := _GetDeviceDriverBasenameW(ImageBase,
                                        lpBaseName,
                                        nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverFileName(ImageBase: Pointer;
                                 lpFileName: LPWSTR;
                                 nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverFileName) then
    Result := _GetDeviceDriverFileName(ImageBase,
                                       lpFileName,
                                       nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverFileNameA(ImageBase: Pointer;
                                  lpFileName: LPSTR;
                                  nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverFileNameA) then
    Result := _GetDeviceDriverFileNameA(ImageBase,
                                        lpFileName,
                                        nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function GetDeviceDriverFileNameW(ImageBase: Pointer;
                                  lpFileName: LPWSTR;
                                  nSize: DWORD): DWORD;
begin
  if CheckPSAPILoaded and Assigned(_GetDeviceDriverFileNameW) then
    Result := _GetDeviceDriverFileNameW(ImageBase,
                                        lpFileName,
                                        nSize)
  else
    begin
      PsApiUnavailable;
      Result := 0;
    end;
end;


function EnumDeviceDrivers(lpImageBase: PPointer;
                           cb: DWORD;
                           var lpcbNeeded: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_EnumDeviceDrivers) then
    Result := _EnumDeviceDrivers(lpImageBase,
                                 cb,
                                 lpcbNeeded)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


function GetProcessMemoryInfo(Process: THandle;
                               ppsmemCounters: PPROCESS_MEMORY_COUNTERS;
                               cb: DWORD): BOOL;
begin
  if CheckPSAPILoaded and Assigned(_GetProcessMemoryInfo) then
    Result := _GetProcessMemoryInfo(Process,
                                    ppsmemCounters,
                                    cb)
  else
    begin
      PsApiUnavailable;
      Result := False;
    end;
end;


// Record helpers //////////////////////////////////////////////////////////////

function GetUBits(const Bits: ULONG_PTR;
                  const aIndex: Integer): ULONG_PTR;
var
  Width: Byte;
  Mask: ULONG_PTR;
begin
  Width := Byte(aIndex);
  if Width = SizeOf(ULONG_PTR) * 8 then
    Mask := High(ULONG_PTR)
  else
    Mask := (ULONG_PTR(1) shl Width) - 1;
  Result := (Bits shr (aIndex shr 8)) and Mask;
end;

procedure SetUBits(var Bits: ULONG_PTR;
                   const aIndex: Integer;
                   const aValue: ULONG_PTR);
var
  Offset, Width: Byte;
  Mask: ULONG_PTR;
begin
  Offset := aIndex shr 8;
  Width := Byte(aIndex);
  if Width = SizeOf(ULONG_PTR) * 8 then
    Mask := High(ULONG_PTR)
  else
    Mask := (ULONG_PTR(1) shl Width) - 1;
  Assert((aValue and not Mask) = 0);
  Bits := (Bits and not (Mask shl Offset)) or ((aValue and Mask) shl Offset);
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

initialization
  InitializeCriticalSection(PsApiLock);

finalization
  if hPSAPI <> 0 then
    FreeLibrary(hPSAPI);
  DeleteCriticalSection(PsApiLock);

end.
