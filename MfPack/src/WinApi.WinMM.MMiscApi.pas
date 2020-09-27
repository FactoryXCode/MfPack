// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MMiscApi.pas
// Kind: Pascal / Delphi unit
// Release date: 15-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-misc-l1-1
//              This header is used by Windows Multimedia.
//              For more information, see: https://docs.microsoft.com/en-us/windows/win32/api/mmiscapi/
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
// =============================================================================
// Source: mmiscapi.h
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
unit WinApi.WinMM.MMiscApi;

interface

{$HPPEMIT '#include "MMiscApi.h"'}

uses
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.WinMM.MMSysCom;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


  //***************************************************************************
  //
  //                        Installable driver support
  //
  //***************************************************************************

type

  PDRVCONFIGINFOEX = ^DRVCONFIGINFOEX;
  {$EXTERNALSYM PDRVCONFIGINFOEX}
  DRVCONFIGINFOEX = record
    dwDCISize: DWORD;
    lpszDCISectionName: PWideChar;
    lpszDCIAliasName: PWideChar;
    dnDevNode: DWORD;
  end;

  { Driver messages }

const

  {$EXTERNALSYM DRV_LOAD}
  DRV_LOAD                            = $0001;
  {$EXTERNALSYM DRV_ENABLE}
  DRV_ENABLE                          = $0002;
  {$EXTERNALSYM DRV_OPEN}
  DRV_OPEN                            = $0003;
  {$EXTERNALSYM DRV_CLOSE}
  DRV_CLOSE                           = $0004;
  {$EXTERNALSYM DRV_DISABLE}
  DRV_DISABLE                         = $0005;
  {$EXTERNALSYM DRV_FREE}
  DRV_FREE                            = $0006;
  {$EXTERNALSYM DRV_CONFIGURE}
  DRV_CONFIGURE                       = $0007;
  {$EXTERNALSYM DRV_QUERYCONFIGURE}
  DRV_QUERYCONFIGURE                  = $0008;
  {$EXTERNALSYM DRV_INSTALL}
  DRV_INSTALL                         = $0009;
  {$EXTERNALSYM DRV_REMOVE}
  DRV_REMOVE                          = $000A;
  {$EXTERNALSYM DRV_EXITSESSION}
  DRV_EXITSESSION                     = $000B;
  {$EXTERNALSYM DRV_POWER}
  DRV_POWER                           = $000F;
  {$EXTERNALSYM DRV_RESERVED}
  DRV_RESERVED                        = $0800;
  {$EXTERNALSYM DRV_USER}
  DRV_USER                            = $4000;

 { LPARAM of DRV_CONFIGURE message }

type

  PDRVCONFIGINFO = ^tagDRVCONFIGINFO;
  {$EXTERNALSYM PDRVCONFIGINFO}
  tagDRVCONFIGINFO = record
    dwDCISize: DWORD;
    lpszDCISectionName: PWideChar;
    lpszDCIAliasName: PWideChar;
  end;
  {$EXTERNALSYM tagDRVCONFIGINFO}
  DRVCONFIGINFO = tagDRVCONFIGINFO;
  {$EXTERNALSYM DRVCONFIGINFO}

 { Supported return values for DRV_CONFIGURE message }

const

  {$EXTERNALSYM DRVCNF_CANCEL}
  DRVCNF_CANCEL                       = $0000;
  {$EXTERNALSYM DRVCNF_OK}
  DRVCNF_OK                           = $0001;
  {$EXTERNALSYM DRVCNF_RESTART}
  DRVCNF_RESTART                      = $0002;

 { installable driver function prototypes }

type
  // See https://docs.microsoft.com/en-us/windows/win32/api/mmiscapi/nc-mmiscapi-driverproc
  DRIVERPROC = function(Arg1: DWORD_PTR;
                        Arg2: HDRVR;
                        Arg3: UINT;
                        Arg4: LPARAM;
                        Arg5: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM DRIVERPROC}

  function CloseDriver(hDriver: HDRVR;
                       lParam1: LPARAM;
                       lParam2: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM CloseDriver}

  function OpenDriver(szDriverName: PWideChar;
                      szSectionName: PWideChar;
                      lParam2: LPARAM): HDRVR; stdcall;
  {$EXTERNALSYM OpenDriver}

  function SendDriverMessage(hDriver: HDRVR;
                             _message: UINT;
                             lParam1: LPARAM;
                             lParam2: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM SendDriverMessage}

  function DrvGetModuleHandle(hDriver: HDRVR): HMODULE; stdcall;
  {$EXTERNALSYM DrvGetModuleHandle}

  function GetDriverModuleHandle(hDriver: HDRVR): HMODULE; stdcall;
  {$EXTERNALSYM GetDriverModuleHandle}

  function DefDriverProc(dwDriverIdentifier: DWORD_PTR;
                         hdrvr: HDRVR;
                         uMsg: UINT;
                         lParam1: LPARAM;
                         lParam2: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM DefDriverProc}


  { return values from DriverProc() function }

const

  DRV_CANCEL                          = DRVCNF_CANCEL;
  {$EXTERNALSYM DRV_CANCEL}
  DRV_OK                              = DRVCNF_OK;
  {$EXTERNALSYM DRV_OK}
  DRV_RESTART                         = DRVCNF_RESTART;
  {$EXTERNALSYM DRV_RESTART}

  DRV_MCI_FIRST                       = DRV_RESERVED;
  {$EXTERNALSYM DRV_MCI_FIRST}
  DRV_MCI_LAST                        = (DRV_RESERVED + $FFF);
  {$EXTERNALSYM DRV_MCI_LAST}

  //**************************************************************************
  //
  //                      Driver Helper function moved from mmddk.h
  //
  //**************************************************************************


  //  Parameters
  //    * dwCallback
  //      Address of the callback function, a window handle, or a task handle,
  //      depending on the flag specified in the dwFlags parameter.
  //
  //    * dwFlags
  //      Notification flags. It can be ONE of these values:
  //
  //      PARAMETERS              Value	Meaning
  //      ======================= ==================================================
  //      DCB_NOSWITCH            The system is prevented from switching stacks.
  //                              This value is only used if enough stack space for the
  //                              callback function is known to exist.
  //
  //      DCB_FUNCTION            The dwCallback parameter is the address of an application-defined callback function.
  //                              The system sends the callback message to the callback function.
  //
  //      DCB_WINDOW              The dwCallback parameter is the handle of an application-defined window.
  //                              The system sends subsequent notifications to the window.
  //
  //      DCB_TASK                The dwCallback parameter is the handle of an application or task.
  //                              The system sends subsequent notifications to the application or task.
  //
  //    * hDevice
  //      Handle of the installable driver instance.
  //
  //    * dwMsg
  //      Message value.
  //
  //    * dwUser
  //      32-bit user-instance data supplied by the application when the device was opened.
  //
  //    * dwParam1
  //      32-bit message-dependent parameter.
  //
  //    * dwParam2
  //      32-bit message-dependent parameter.

  function DriverCallback(dwCallback: DWORD_PTR;
                          dwFlags: DWORD;
                          hDevice: HDRVR;
                          dwMsg: DWORD;
                          dwUser: DWORD_PTR;
                          dwParam1: DWORD_PTR;
                          dwParam2: DWORD_PTR): BOOL; stdcall;
  {$EXTERNALSYM DriverCallback}


  //***************************************************************************
  //
  //  Sound schemes
  //
  //***************************************************************************

  function sndOpenSound(EventName: PWideChar;
                        AppName: PWideChar;
                        Flags: INT32;
                        {_Outptr_} FileHandle: PHANDLE): LONG; stdcall;
  {$EXTERNALSYM sndOpenSound}


  //
  // removed from winmmi.h
  //

  //***************************************************************************
  //
  //  API to install/remove/query a MMSYS driver
  //
  //***************************************************************************

  // generic prototype for audio device driver entry-point functions
  // midMessage(), modMessage(), widMessage(), wodMessage(), auxMessage()
  //

type

  DRIVERMSGPROC = function(arg1: DWORD;
                           arg2: DWORD;
                           arg3: DWORD_PTR;
                           arg4: DWORD_PTR;
                           arg5: DWORD_PTR): LRESULT; stdcall;
  {$EXTERNALSYM DRIVERMSGPROC}

  function mmDrvInstall(hDriver: HDRVR;
                        wszDrvEntry: PWideChar;
                        drvMessage: DRIVERMSGPROC;
                        wFlags: UINT): UINT; stdcall;
  {$EXTERNALSYM mmDrvInstall}


  //***************************************************************************
  //
  //                        Multimedia File I/O support
  //
  //***************************************************************************

  { MMIO error return values }

const

  MMIOERR_BASE                        = 256;
  {$EXTERNALSYM MMIOERR_BASE}
  MMIOERR_FILENOTFOUND                = (MMIOERR_BASE + 1);  { file not found }
  {$EXTERNALSYM MMIOERR_FILENOTFOUND}
  MMIOERR_OUTOFMEMORY                 = (MMIOERR_BASE + 2);  { out of memory }
  {$EXTERNALSYM MMIOERR_OUTOFMEMORY}
  MMIOERR_CANNOTOPEN                  = (MMIOERR_BASE + 3);  { cannot open }
  {$EXTERNALSYM MMIOERR_CANNOTOPEN}
  MMIOERR_CANNOTCLOSE                 = (MMIOERR_BASE + 4);  { cannot close }
  {$EXTERNALSYM MMIOERR_CANNOTCLOSE}
  MMIOERR_CANNOTREAD                  = (MMIOERR_BASE + 5);  { cannot read }
  {$EXTERNALSYM MMIOERR_CANNOTREAD}
  MMIOERR_CANNOTWRITE                 = (MMIOERR_BASE + 6);  { cannot write }
  {$EXTERNALSYM MMIOERR_CANNOTWRITE}
  MMIOERR_CANNOTSEEK                  = (MMIOERR_BASE + 7);  { cannot seek }
  {$EXTERNALSYM MMIOERR_CANNOTSEEK}
  MMIOERR_CANNOTEXPAND                = (MMIOERR_BASE + 8);  { cannot expand file }
  {$EXTERNALSYM MMIOERR_CANNOTEXPAND}
  MMIOERR_CHUNKNOTFOUND               = (MMIOERR_BASE + 9);  { chunk not found }
  {$EXTERNALSYM MMIOERR_CHUNKNOTFOUND}
  MMIOERR_UNBUFFERED                  = (MMIOERR_BASE + 10);  {  }
  {$EXTERNALSYM MMIOERR_UNBUFFERED}
  MMIOERR_PATHNOTFOUND                = (MMIOERR_BASE + 11);  { path incorrect }
  {$EXTERNALSYM MMIOERR_PATHNOTFOUND}
  MMIOERR_ACCESSDENIED                = (MMIOERR_BASE + 12);  { file was protected }
  {$EXTERNALSYM MMIOERR_ACCESSDENIED}
  MMIOERR_SHARINGVIOLATION            = (MMIOERR_BASE + 13);  { file in use }
  {$EXTERNALSYM MMIOERR_SHARINGVIOLATION}
  MMIOERR_NETWORKERROR                = (MMIOERR_BASE + 14);  { network not responding }
  {$EXTERNALSYM MMIOERR_NETWORKERROR}
  MMIOERR_TOOMANYOPENFILES            = (MMIOERR_BASE + 15);  { no more file handles  }
  {$EXTERNALSYM MMIOERR_TOOMANYOPENFILES}
  MMIOERR_INVALIDFILE                 = (MMIOERR_BASE + 16);  { default error file error }
  {$EXTERNALSYM MMIOERR_INVALIDFILE}

  { MMIO constants }

  CFSEPCHAR = '+';  { compound file name separator char. }
  {$EXTERNALSYM CFSEPCHAR}

type
  { MMIO data types }
  // Note: this data type is also declared in WinApi.WinApiTypes.
  PFOURCC = ^FOURCC;
  {$EXTERNALSYM FOURCC}
  FOURCC = DWORD;

  { a four character code }
  PHuge = ^huge;
  {$EXTERNALSYM _huge}
  _huge = AnsiChar;
  huge = _huge;
  { a huge version of LPSTR }

  PHMMIO = ^HMMIO;
  HMMIO = IntPtr;      { a handle to an open file }
  {$EXTERNALSYM HMMIO}


type

  PMMIOPROC = ^MMIOPROC;
  MMIOPROC = function(lpmmioinfo: PAnsiChar;
                      uMsg: UINT;
                      lParam1: LPARAM;
                      lParam2: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM MMIOPROC}
  LPMMIOPROC = ^MMIOPROC;
  {$EXTERNALSYM LPMMIOPROC}

  { general MMIO information data structure }

type

  PMMIOINFO = ^_MMIOINFO;
  {$EXTERNALSYM PMMIOINFO}
  _MMIOINFO = record
    { general fields }
    dwFlags: DWORD;                  { general status flags }
    fccIOProc: FOURCC;               { pointer to I/O procedure }
    pIOProc: LPMMIOPROC;             { pointer to I/O procedure }
    wErrorRet: UINT;                 { place for error to be returned }
    htask: HTASK;                    { alternate local task }
    { fields maintained by MMIO functions during buffered I/O }
    cchBuffer: LONG;                 { size of I/O buffer (or 0L) }
    pchBuffer: HPSTR;                { start of I/O buffer (or NULL) }
    pchNext: HPSTR;                  { pointer to next byte to read/write }
    pchEndRead: HPSTR;               { pointer to last valid byte to read }
    pchEndWrite: HPSTR;              { pointer to last byte to write }
    lBufOffset: LONG;                { disk offset of start of buffer }
    { fields maintained by I/O procedure }
    lDiskOffset: LONG;               { disk offset of next read or write }
    adwInfo: array[0..2] of DWORD;   { data specific to type of MMIOPROC }
    { other fields maintained by MMIO }
    dwReserved1: DWORD;              { reserved for MMIO use }
    dwReserved2: DWORD;              { reserved for MMIO use }
    hmmio: HMMIO;                    { handle to open file }
  end;
  {$EXTERNALSYM _MMIOINFO}
  MMIOINFO = _MMIOINFO;
  {$EXTERNALSYM MMIOINFO}
  LPMMIOINFO = ^MMIOINFO;
  {$EXTERNALSYM LPMMIOINFO}
  LPCMMIOINFO = ^MMIOINFO;
  {$EXTERNALSYM LPCMMIOINFO}

  { RIFF chunk information data structure }

type

  PMMCKINFO = ^_MMCKINFO;
  {$EXTERNALSYM PMMCKINFO}
  _MMCKINFO = record
    ckid: FOURCC;                   { chunk ID }
    cksize: DWORD;                  { chunk size }
    fccType: FOURCC;                { form type or list type }
    dwDataOffset: DWORD;            { offset of data portion of chunk }
    dwFlags: DWORD;                 { flags used by MMIO functions }
  end;
  {$EXTERNALSYM _MMCKINFO}
  MMCKINFO = _MMCKINFO;
  {$EXTERNALSYM MMCKINFO}
  LPMMCKINFO = ^MMCKINFO;
  {$EXTERNALSYM LPMMCKINFO}
  LPCMMCKINFO = ^MMCKINFO;
  {$EXTERNALSYM LPCMMCKINFO}

  { bit field masks }

const

  MMIO_RWMODE                         = $00000003;  { open file for reading/writing/both }
  {$EXTERNALSYM MMIO_RWMODE}
  MMIO_SHAREMODE                      = $00000070;  { file sharing mode number }
  {$EXTERNALSYM MMIO_SHAREMODE}

  { constants for dwFlags field of MMIOINFO }
  MMIO_CREATE                         = $00001000;  { create new file (or truncate file) }
  {$EXTERNALSYM MMIO_CREATE}
  MMIO_PARSE                          = $00000100;  { parse new file returning path }
  {$EXTERNALSYM MMIO_PARSE}
  MMIO_DELETE                         = $00000200;  { create new file (or truncate file) }
  {$EXTERNALSYM MMIO_DELETE}
  MMIO_EXIST                          = $00004000;  { checks for existence of file }
  {$EXTERNALSYM MMIO_EXIST}
  MMIO_ALLOCBUF                       = $00010000;  { mmioOpen() should allocate a buffer }
  {$EXTERNALSYM MMIO_ALLOCBUF}
  MMIO_GETTEMP                        = $00020000;  { mmioOpen() should retrieve temp name }
  {$EXTERNALSYM MMIO_GETTEMP}

  MMIO_DIRTY                          = $10000000;  { I/O buffer is dirty }
  {$EXTERNALSYM MMIO_DIRTY}

  { read/write mode numbers (bit field MMIO_RWMODE) }
  MMIO_READ                           = $00000000;  { open file for reading only }
  {$EXTERNALSYM MMIO_READ}
  MMIO_WRITE                          = $00000001;  { open file for writing only }
  {$EXTERNALSYM MMIO_WRITE}
  MMIO_READWRITE                      = $00000002;  { open file for reading and writing }
  {$EXTERNALSYM MMIO_READWRITE}

  { share mode numbers (bit field MMIO_SHAREMODE) }
  MMIO_COMPAT                         = $00000000;  { compatibility mode }
  {$EXTERNALSYM MMIO_COMPAT}
  MMIO_EXCLUSIVE                      = $00000010;  { exclusive-access mode }
  {$EXTERNALSYM MMIO_EXCLUSIVE}
  MMIO_DENYWRITE                      = $00000020;  { deny writing to other processes }
  {$EXTERNALSYM MMIO_DENYWRITE}
  MMIO_DENYREAD                       = $00000030;  { deny reading to other processes }
  {$EXTERNALSYM MMIO_DENYREAD}
  MMIO_DENYNONE                       = $00000040;  { deny nothing to other processes }
  {$EXTERNALSYM MMIO_DENYNONE}

  { various MMIO flags }
  MMIO_FHOPEN                         = $0010;  { mmioClose: keep file handle open }
  {$EXTERNALSYM MMIO_FHOPEN}
  MMIO_EMPTYBUF                       = $0010;  { mmioFlush: empty the I/O buffer }
  {$EXTERNALSYM MMIO_EMPTYBUF}
  MMIO_TOUPPER                        = $0010;  { mmioStringToFOURCC: to u-case }
  {$EXTERNALSYM MMIO_TOUPPER}
  MMIO_INSTALLPROC                    = $00010000;  { mmioInstallIOProc: install MMIOProc }
  {$EXTERNALSYM MMIO_INSTALLPROC}
  MMIO_GLOBALPROC                     = $10000000;  { mmioInstallIOProc: install globally }
  {$EXTERNALSYM MMIO_GLOBALPROC}
  MMIO_REMOVEPROC                     = $00020000;  { mmioInstallIOProc: remove MMIOProc }
  {$EXTERNALSYM MMIO_REMOVEPROC}
  MMIO_UNICODEPROC                    = $01000000;  { mmioInstallIOProc: Unicode MMIOProc }
  {$EXTERNALSYM MMIO_UNICODEPROC}
  MMIO_FINDPROC                       = $00040000;  { mmioInstallIOProc: find an MMIOProc }
  {$EXTERNALSYM MMIO_FINDPROC}
  MMIO_FINDCHUNK                      = $0010;  { mmioDescend: find a chunk by ID }
  {$EXTERNALSYM MMIO_FINDCHUNK}
  MMIO_FINDRIFF                       = $0020;  { mmioDescend: find a LIST chunk }
  {$EXTERNALSYM MMIO_FINDRIFF}
  MMIO_FINDLIST                       = $0040;  { mmioDescend: find a RIFF chunk }
  {$EXTERNALSYM MMIO_FINDLIST}
  MMIO_CREATERIFF                     = $0020;  { mmioCreateChunk: make a LIST chunk }
  {$EXTERNALSYM MMIO_CREATERIFF}
  MMIO_CREATELIST                     = $0040;  { mmioCreateChunk: make a RIFF chunk }
  {$EXTERNALSYM MMIO_CREATELIST}

  { message numbers for MMIOPROC I/O procedure functions }

  MMIOM_READ                          = MMIO_READ;  { read }
  {$EXTERNALSYM MMIOM_READ}
  MMIOM_WRITE                         = MMIO_WRITE;  { write }
  {$EXTERNALSYM MMIOM_WRITE}
  MMIOM_SEEK                          = 2;  { seek to a new position in file }
  {$EXTERNALSYM MMIOM_SEEK}
  MMIOM_OPEN                          = 3;  { open file }
  {$EXTERNALSYM MMIOM_OPEN}
  MMIOM_CLOSE                         = 4;  { close file }
  {$EXTERNALSYM MMIOM_CLOSE}
  MMIOM_WRITEFLUSH                    = 5;  { write and flush }
  {$EXTERNALSYM MMIOM_WRITEFLUSH}
  MMIOM_RENAME                        = 6;  { rename specified file }
  {$EXTERNALSYM MMIOM_RENAME}
  MMIOM_USER                          = $8000;  { beginning of user-defined messages }
  {$EXTERNALSYM MMIOM_USER}

  { standard four character codes }

  FOURCC_RIFF = ord('R') or ord('I') shl 8 or (ord('F') shl 16) or (ord('F') shl 24);
  {$EXTERNALSYM FOURCC_RIFF}
  FOURCC_LIST = ord('L') or ord('I') shl 8 or (ord('S') shl 16) or (ord('T') shl 24);
  {$EXTERNALSYM FOURCC_LIST}

  { four character codes used to identify standard built-in I/O procedures }

  FOURCC_DOS  =  ord('D') or ord('O') shl 8 or (ord('S') shl 16) or (ord(' ') shl 24);
  {$EXTERNALSYM FOURCC_DOS}
  FOURCC_MEM  = ord('M') or ord('E') shl 8 or (ord('M') shl 16) or (ord(' ') shl 24);
  {$EXTERNALSYM FOURCC_MEM}


  { flags for mmioSeek() }

  SEEK_SET                            = 0;  { seek to an absolute position }
  {$EXTERNALSYM SEEK_SET}
  SEEK_CUR                            = 1;  { seek relative to current position }
  {$EXTERNALSYM SEEK_CUR}
  SEEK_END                            = 2;  { seek relative to end of file }
  {$EXTERNALSYM SEEK_END}

  { other constants }
  MMIO_DEFAULTBUFFER                  = 8192;  { default buffer size }
  {$EXTERNALSYM MMIO_DEFAULTBUFFER}

  { MMIO macros }
  //#define mmioFOURCC(ch0, ch1, ch2, ch3)  MAKEFOURCC(ch0, ch1, ch2, ch3)
  // For MAKEFOURCC see: WinApi.MmReg

  { MMIO function prototypes }

  function mmioStringToFOURCC(sz: PWideChar;
                              uFlags: UINT): FOURCC; stdcall;
  {$EXTERNALSYM mmioStringToFOURCC}

  function mmioStringToFOURCCA(sz: PAnsiChar;
                               uFlags: UINT): FOURCC; stdcall;
  {$EXTERNALSYM mmioStringToFOURCCA}

  function mmioStringToFOURCCW(sz: PWideChar;
                               uFlags: UINT): FOURCC; stdcall;
  {$EXTERNALSYM mmioStringToFOURCCW}


  function mmioInstallIOProc(fccIOProc: FOURCC;
                              {_In_opt_} pIOProc: LPMMIOPROC;
                              dwFlags: DWORD): LPMMIOPROC; stdcall;
  {$EXTERNALSYM mmioInstallIOProc}

  function mmioInstallIOProcA(fccIOProc: FOURCC;
                              {_In_opt_} pIOProc: LPMMIOPROC;
                              dwFlags: DWORD): LPMMIOPROC; stdcall;
  {$EXTERNALSYM mmioInstallIOProcA}

  function mmioInstallIOProcW(fccIOProc: FOURCC;
                              {_In_opt_} pIOProc: LPMMIOPROC;
                              dwFlags: DWORD): LPMMIOPROC; stdcall;
  {$EXTERNALSYM mmioInstallIOProcW}


  function mmioOpen(pszFileName: PWideChar;
                    pmmioinfo: LPCMMIOINFO;
                    fdwOpen: DWORD): HMMIO; stdcall;
  {$EXTERNALSYM mmioOpen}

  function mmioOpenA(pszFileName: PAnsiChar;
                    pmmioinfo: LPCMMIOINFO;
                    fdwOpen: DWORD): HMMIO; stdcall;
  {$EXTERNALSYM mmioOpenA}

  function mmioOpenW(pszFileName: PWideChar;
                     pmmioinfo: LPCMMIOINFO;
                     fdwOpen: DWORD): HMMIO; stdcall;
  {$EXTERNALSYM mmioOpenW}


  function mmioRename(pszFileName: PWideChar;
                      pszNewFileName: PWideChar;
                      {_In_opt_} pmmioinfo: LPCMMIOINFO;
                      fdwRename: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mmioRename}

  function mmioRenameA(pszFileName: PAnsiChar;
                       pszNewFileName: PAnsiChar;
                       {_In_opt_} pmmioinfo: LPCMMIOINFO;
                       fdwRename: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mmioRenameA}

  function mmioRenameW(pszFileName: PWideChar;
                       pszNewFileName: PWideChar;
                       {_In_opt_} pmmioinfo: LPCMMIOINFO;
                       fdwRename: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mmioRenameW}

  function mmioClose(hmmio: HMMIO;
                     uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioClose}

  function mmioRead(hmmio: HMMIO;
                    pch: PAnsiChar;
                    cch: Longint): Longint; stdcall;
  {$EXTERNALSYM mmioRead}

  function mmioWrite(hmmio: HMMIO;
                     pch: PAnsiChar;
                     cch: Longint): Longint; stdcall;
  {$EXTERNALSYM mmioWrite}

  function mmioSeek(hmmio: HMMIO;
                    lOffset: LONG;
                    iOrigin: Int): Longint; stdcall;
  {$EXTERNALSYM mmioSeek}

  function mmioGetInfo(hmmio: HMMIO;
                       lpmmioinfo: LPMMIOINFO;
                       uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioGetInfo}

  function mmioSetInfo(hmmio: HMMIO;
                       lpmmioinfo: LPMMIOINFO;
                       uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioSetInfo}

  function mmioSetBuffer(hmmio: HMMIO;
                         pchBuffer: PAnsiChar;
                         cchBuffer: LONG;
                         uFlags: Word): MMRESULT; stdcall;
  {$EXTERNALSYM mmioSetBuffer}

  function mmioFlush(hmmio: HMMIO;
                     uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioFlush}

  function mmioAdvance(hmmio: HMMIO;
                       lpmmioinfo: LPMMIOINFO; uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioAdvance}

  function mmioSendMessage(hmmio: HMMIO;
                           uMessage: UINT;
                           lParam1: DWORD;
                           lParam2: DWORD): Longint; stdcall;
  {$EXTERNALSYM mmioSendMessage}

  function mmioDescend(hmmio: HMMIO;
                       lpck: LPMMCKINFO;
                       lpckParent: LPMMCKINFO;
                       uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioDescend}

  function mmioAscend(hmmio: HMMIO;
                      lpck: LPMMCKINFO;
                      uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioAscend}

  function mmioCreateChunk(hmmio: HMMIO;
                           lpck: LPMMCKINFO;
                           uFlags: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mmioCreateChunk}


  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation

const
  MMiscApiLib = 'Winmm.dll';

function CloseDriver; external MMiscApiLib name 'CloseDriver';
function OpenDriver; external MMiscApiLib name 'OpenDriver';
function SendDriverMessage; external MMiscApiLib name 'SendDriverMessage';
function DrvGetModuleHandle; external MMiscApiLib name 'DrvGetModuleHandle';
function GetDriverModuleHandle; external MMiscApiLib name 'GetDriverModuleHandle';
function DefDriverProc; external MMiscApiLib name 'DefDriverProc';
function DriverCallback; external MMiscApiLib name 'DriverCallback';
function sndOpenSound; external MMiscApiLib name 'sndOpenSound';
function mmDrvInstall; external MMiscApiLib name 'mmDrvInstall';

function mmioStringToFOURCC; external MMiscApiLib name 'mmioStringToFOURCCW';
function mmioStringToFOURCCA; external MMiscApiLib name 'mmioStringToFOURCCA';
function mmioStringToFOURCCW; external MMiscApiLib name 'mmioStringToFOURCCW';

function mmioInstallIOProc; external MMiscApiLib name 'mmioInstallIOProcW';
function mmioInstallIOProcA; external MMiscApiLib name 'mmioInstallIOProcA';
function mmioInstallIOProcW; external MMiscApiLib name 'mmioInstallIOProcW';

function mmioOpen; external MMiscApiLib name 'mmioOpenW';
function mmioOpenA; external MMiscApiLib name 'mmioOpenA';
function mmioOpenW; external MMiscApiLib name 'mmioOpenW';

function mmioRename; external MMiscApiLib name 'mmioRenameW';
function mmioRenameA; external MMiscApiLib name 'mmioRenameA';
function mmioRenameW; external MMiscApiLib name 'mmioRenameW';

function mmioClose; external MMiscApiLib name 'mmioClose';
function mmioRead; external MMiscApiLib name 'mmioRead';
function mmioWrite; external MMiscApiLib name 'mmioWrite';
function mmioSeek; external MMiscApiLib name 'mmioSeek';
function mmioGetInfo; external MMiscApiLib name 'mmioGetInfo';
function mmioSetInfo; external MMiscApiLib name 'mmioSetInfo';
function mmioSetBuffer; external MMiscApiLib name 'mmioSetBuffer';
function mmioFlush; external MMiscApiLib name 'mmioFlush';
function mmioAdvance; external MMiscApiLib name 'mmioAdvance';
function mmioSendMessage; external MMiscApiLib name 'mmioSendMessage';
function mmioDescend; external MMiscApiLib name 'mmioDescend';
function mmioAscend; external MMiscApiLib name 'mmioAscend';
function mmioCreateChunk; external MMiscApiLib name 'mmioCreateChunk';

  // Implement Additional functions here.

end.
