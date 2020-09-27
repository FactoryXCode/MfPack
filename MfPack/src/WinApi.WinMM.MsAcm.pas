// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MsAcm.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Audio Compression Manager Public Header File.
//              This header is used by Windows Multimedia.
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
// Source: msasm.h
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
unit WinApi.WinMM.MsAcm;

interface

uses
  WinApi.Windows,
  WinApi.WinMM.MMDdk,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMReg;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  DRV_MAPPER_PREFERRED_INPUT_GET      = (DRV_USER + 0);
  {$EXTERNALSYM DRV_MAPPER_PREFERRED_INPUT_GET}
  DRV_MAPPER_PREFERRED_OUTPUT_GET     = (DRV_USER + 2);
  {$EXTERNALSYM DRV_MAPPER_PREFERRED_OUTPUT_GET}

  DRVM_MAPPER                         = ($2000);
  {$EXTERNALSYM DRVM_MAPPER}
  DRVM_MAPPER_STATUS                  = (DRVM_MAPPER + 0);
  {$EXTERNALSYM DRVM_MAPPER_STATUS}

  WIDM_MAPPER_STATUS                  = (DRVM_MAPPER_STATUS + 0);
  {$EXTERNALSYM WIDM_MAPPER_STATUS}
  WAVEIN_MAPPER_STATUS_DEVICE         = 0;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_DEVICE}
  WAVEIN_MAPPER_STATUS_MAPPED         = 1;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_MAPPED}
  WAVEIN_MAPPER_STATUS_FORMAT         = 2;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_FORMAT}

  WODM_MAPPER_STATUS                  = (DRVM_MAPPER_STATUS + 0);
  {$EXTERNALSYM WODM_MAPPER_STATUS}
  WAVEOUT_MAPPER_STATUS_DEVICE        = 0;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_DEVICE}
  WAVEOUT_MAPPER_STATUS_MAPPED        = 1;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_MAPPED}
  WAVEOUT_MAPPER_STATUS_FORMAT        = 2;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_FORMAT}


  //--------------------------------------------------------------------------;
  //
  //  ACM General API's and Defines
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //
  //  there are four types of 'handles' used by the ACM. the first three
  //  are unique types that define specific objects:
  //
  //  HACMDRIVERID: used to _identify_ an ACM driver. this identifier can be
  //  used to _open_ the driver for querying details, etc about the driver.
  //
  //  HACMDRIVER: used to manage a driver (codec, filter, etc). this handle
  //  is much like a handle to other media drivers--you use it to send
  //  messages to the converter, query for capabilities, etc.
  //
  //  HACMSTREAM: used to manage a 'stream' (conversion channel) with the
  //  ACM. you use a stream handle to convert data from one format/type
  //  to another--much like dealing with a file handle.
  //
  //
  //  the fourth handle type is a generic type used on ACM functions that
  //  can accept two or more of the above handle types (for example the
  //  acmMetrics and acmDriverID functions).
  //
  //  HACMOBJ: used to identify ACM objects. this handle is used on functions
  //  that can accept two or more ACM handle types.
  //

type

  HACMDRIVERID = type THandle;
  {$EXTERNALSYM HACMDRIVERID}
  PHACMDRIVERID = ^HACMDRIVERID;
  {$EXTERNALSYM PHACMDRIVERID}
  LPHACMDRIVERID = ^HACMDRIVERID;
  {$EXTERNALSYM LPHACMDRIVERID}

  HACMDRIVER = type THandle;
  {$EXTERNALSYM HACMDRIVER}
  PHACMDRIVER = ^HACMDRIVER;
  {$EXTERNALSYM PHACMDRIVER}
  LPHACMDRIVER = ^HACMDRIVER;
  {$EXTERNALSYM LPHACMDRIVER}

  HACMSTREAM = type THandle;
  {$EXTERNALSYM HACMSTREAM}
  PHACMSTREAM = ^HACMSTREAM;
  {$EXTERNALSYM PHACMSTREAM}
  LPHACMSTREAM = ^HACMSTREAM;
  {$EXTERNALSYM LPHACMSTREAM}

  HACMOBJ = type THandle;
  {$EXTERNALSYM HACMOBJ}
  PHACMOBJ = ^HACMOBJ;
  {$EXTERNALSYM PHACMOBJ}
  LPHACMOBJ = ^HACMOBJ;
  {$EXTERNALSYM LPHACMOBJ}

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  //  ACM Error Codes
  //
  //  Note that these error codes are specific errors that apply to the ACM
  //  directly--general errors are defined as MMSYSERR_*.
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  MMRESULT = type UINT;
  {$EXTERNALSYM MMRESULT}

const

  ACMERR_BASE                         = (512);
  {$EXTERNALSYM ACMERR_BASE}
  ACMERR_NOTPOSSIBLE                  = (ACMERR_BASE + 0);
  {$EXTERNALSYM ACMERR_NOTPOSSIBLE}
  ACMERR_BUSY                         = (ACMERR_BASE + 1);
  {$EXTERNALSYM ACMERR_BUSY}
  ACMERR_UNPREPARED                   = (ACMERR_BASE + 2);
  {$EXTERNALSYM ACMERR_UNPREPARED}
  ACMERR_CANCELED                     = (ACMERR_BASE + 3);
  {$EXTERNALSYM ACMERR_CANCELED}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  //  ACM Window Messages
  //
  //  These window messages are sent by the ACM or ACM drivers to notify
  //  applications of events.
  //
  //  Note that these window message numbers will also be defined in
  //  mmsystem.
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  MM_ACM_OPEN                         = (MM_STREAM_OPEN);  // conversion callback messages
  {$EXTERNALSYM MM_ACM_OPEN}
  MM_ACM_CLOSE                        = (MM_STREAM_CLOSE);
  {$EXTERNALSYM MM_ACM_CLOSE}
  MM_ACM_DONE                         = (MM_STREAM_DONE);
  {$EXTERNALSYM MM_ACM_DONE}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmGetVersion()
  //
  //  the ACM version is a 32 bit number that is broken into three parts as
  //  follows:
  //
  //      bits 24 - 31:   8 bit _major_ version number
  //      bits 16 - 23:   8 bit _minor_ version number
  //      bits  0 - 15:   16 bit build number
  //
  //  this is then displayed as follows:
  //
  //      bMajor = (BYTE)(dwVersion >> 24)
  //      bMinor = (BYTE)(dwVersion >> 16) &
  //      wBuild = LOWORD(dwVersion)
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmGetVersion(): DWORD; stdcall;
  {$EXTERNALSYM acmGetVersion}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmMetrics()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmMetrics(hao: HACMOBJ;
                      uMetric: UINT;
                      pMetric: Pointer): MMRESULT; stdcall;
  {$EXTERNALSYM acmMetrics}

const

  ACM_METRIC_COUNT_DRIVERS            = 1;
  {$EXTERNALSYM ACM_METRIC_COUNT_DRIVERS}
  ACM_METRIC_COUNT_CODECS             = 2;
  {$EXTERNALSYM ACM_METRIC_COUNT_CODECS}
  ACM_METRIC_COUNT_CONVERTERS         = 3;
  {$EXTERNALSYM ACM_METRIC_COUNT_CONVERTERS}
  ACM_METRIC_COUNT_FILTERS            = 4;
  {$EXTERNALSYM ACM_METRIC_COUNT_FILTERS}
  ACM_METRIC_COUNT_DISABLED           = 5;
  {$EXTERNALSYM ACM_METRIC_COUNT_DISABLED}
  ACM_METRIC_COUNT_HARDWARE           = 6;
  {$EXTERNALSYM ACM_METRIC_COUNT_HARDWARE}
  ACM_METRIC_COUNT_LOCAL_DRIVERS      = 20;
  {$EXTERNALSYM ACM_METRIC_COUNT_LOCAL_DRIVERS}
  ACM_METRIC_COUNT_LOCAL_CODECS       = 21;
  {$EXTERNALSYM ACM_METRIC_COUNT_LOCAL_CODECS}
  ACM_METRIC_COUNT_LOCAL_CONVERTERS   = 22;
  {$EXTERNALSYM ACM_METRIC_COUNT_LOCAL_CONVERTERS}
  ACM_METRIC_COUNT_LOCAL_FILTERS      = 23;
  {$EXTERNALSYM ACM_METRIC_COUNT_LOCAL_FILTERS}
  ACM_METRIC_COUNT_LOCAL_DISABLED     = 24;
  {$EXTERNALSYM ACM_METRIC_COUNT_LOCAL_DISABLED}
  ACM_METRIC_HARDWARE_WAVE_INPUT      = 30;
  {$EXTERNALSYM ACM_METRIC_HARDWARE_WAVE_INPUT}
  ACM_METRIC_HARDWARE_WAVE_OUTPUT     = 31;
  {$EXTERNALSYM ACM_METRIC_HARDWARE_WAVE_OUTPUT}
  ACM_METRIC_MAX_SIZE_FORMAT          = 50;
  {$EXTERNALSYM ACM_METRIC_MAX_SIZE_FORMAT}
  ACM_METRIC_MAX_SIZE_FILTER          = 51;
  {$EXTERNALSYM ACM_METRIC_MAX_SIZE_FILTER}
  ACM_METRIC_DRIVER_SUPPORT           = 100;
  {$EXTERNALSYM ACM_METRIC_DRIVER_SUPPORT}
  ACM_METRIC_DRIVER_PRIORITY          = 101;
  {$EXTERNALSYM ACM_METRIC_DRIVER_PRIORITY}


  //--------------------------------------------------------------------------;
  //
  //  ACM Drivers
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverEnum()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type

 ACMDRIVERENUMCB = function(hadid: HACMDRIVERID;
                            dwInstance: DWORD_PTR;
                            fdwSupport: DWORD): BOOL; stdcall;
 {$EXTERNALSYM ACMDRIVERENUMCB}

 function acmDriverEnum(fnCallback: ACMDRIVERENUMCB;
                        dwInstance: DWORD_PTR;
                        fdwEnum: DWORD): MMRESULT; stdcall;
 {$EXTERNALSYM acmDriverEnum}

const

  ACM_DRIVERENUMF_NOLOCAL             = $40000000;
  {$EXTERNALSYM ACM_DRIVERENUMF_NOLOCAL}
  ACM_DRIVERENUMF_DISABLED            = $80000000;
  {$EXTERNALSYM ACM_DRIVERENUMF_DISABLED}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverID()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverID(hao: HACMOBJ;
                       phadid: LPHACMDRIVERID;
                       fdwDriverID: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverID}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverAdd()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverAddA(phadid: LPHACMDRIVERID;
                         hinstModule: HMODULE; // Do not use HINSTANCE, this clashes, use HINST or HMODULE (preffered)
                         lParam: LPARAM;
                         dwPriority: DWORD;
                         fdwAdd: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverAddA}

  function acmDriverAddW(phadid: LPHACMDRIVERID;
                         hinstModule: HMODULE; // Do not use HINSTANCE, this clashes, use HINST or HMODULE (preffered)
                         lParam: LPARAM;
                         dwPriority: DWORD;
                         fdwAdd: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverAddW}

  function acmDriverAdd(phadid: LPHACMDRIVERID;
                        hinstModule: HMODULE; // Do not use HINSTANCE, this clashes. use HINST or HMODULE (preffered)
                        lParam: LPARAM;
                        dwPriority: DWORD;
                        fdwAdd: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverAdd}

const

  ACM_DRIVERADDF_NAME                 = $00000001;
  {$EXTERNALSYM ACM_DRIVERADDF_NAME}
  ACM_DRIVERADDF_FUNCTION             = $00000003;  // lParam is a procedure
  {$EXTERNALSYM ACM_DRIVERADDF_FUNCTION}
  ACM_DRIVERADDF_NOTIFYHWND           = $00000004;  // lParam is notify hwnd
  {$EXTERNALSYM ACM_DRIVERADDF_NOTIFYHWND}
  ACM_DRIVERADDF_TYPEMASK             = $00000007;  // driver type mask
  {$EXTERNALSYM ACM_DRIVERADDF_TYPEMASK}
  ACM_DRIVERADDF_LOCAL                = $00000000;  // is local to current task
  {$EXTERNALSYM ACM_DRIVERADDF_LOCAL}
  ACM_DRIVERADDF_GLOBAL               = $00000008;  // is global
  {$EXTERNALSYM ACM_DRIVERADDF_GLOBAL}

  //
  //  prototype for ACM driver procedures that are installed as _functions_
  //  or _notifations_ instead of as a standalone installable driver.
  //

type
  ACMDRIVERPROC = function(Arg1: DWORD_PTR;
                           Arg2: HACMDRIVERID;
                           Arg3: UINT;
                           Arg4: LPARAM;
                           Arg5: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM ACMDRIVERPROC}
  LPACMDRIVERPROC = ^ACMDRIVERPROC;
  {$EXTERNALSYM LPACMDRIVERPROC}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverRemove()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverRemove(hadid: HACMDRIVERID;
                           fdwRemove: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverRemove}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverOpen()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverOpen(phad: LPHACMDRIVER;
                         hadid: HACMDRIVERID;
                         fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverOpen}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverClose()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverClose(had: HACMDRIVER;
                          fdwClose: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverClose}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverMessage()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverMessage(had: HACMDRIVER;
                            uMsg: UINT;
                            lParam1: LPARAM;
                            lParam2: LPARAM): LRESULT; stdcall;
  {$EXTERNALSYM acmDriverMessage}


  //
  //
  //
  //
const
  ACMDM_USER                          = (DRV_USER + $0000);
  {$EXTERNALSYM ACMDM_USER}
  ACMDM_RESERVED_LOW                  = (DRV_USER + $2000);
  {$EXTERNALSYM ACMDM_RESERVED_LOW}
  ACMDM_RESERVED_HIGH                 = (DRV_USER + $2FFF);
  {$EXTERNALSYM ACMDM_RESERVED_HIGH}

  ACMDM_BASE                          = ACMDM_RESERVED_LOW;
  {$EXTERNALSYM ACMDM_BASE}

  ACMDM_DRIVER_ABOUT                  = (ACMDM_BASE + 11);
  {$EXTERNALSYM ACMDM_DRIVER_ABOUT}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverPriority
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmDriverPriority(hadid: HACMDRIVERID;
                             dwPriority: DWORD;
                             fdwPriority: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverPriority}

const

  ACM_DRIVERPRIORITYF_ENABLE          = $00000001;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_ENABLE}
  ACM_DRIVERPRIORITYF_DISABLE         = $00000002;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_DISABLE}
  ACM_DRIVERPRIORITYF_ABLEMASK        = $00000003;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_ABLEMASK}
  ACM_DRIVERPRIORITYF_BEGIN           = $00010000;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_BEGIN}
  ACM_DRIVERPRIORITYF_END             = $00020000;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_END}
  ACM_DRIVERPRIORITYF_DEFERMASK       = $00030000;
  {$EXTERNALSYM ACM_DRIVERPRIORITYF_DEFERMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmDriverDetails()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  //
  //  ACMDRIVERDETAILS
  //
  //  the ACMDRIVERDETAILS structure is used to get various capabilities from
  //  an ACM driver (codec, converter, filter).
  //
  ACMDRIVERDETAILS_SHORTNAME_CHARS    = 32;
  {$EXTERNALSYM ACMDRIVERDETAILS_SHORTNAME_CHARS}
  ACMDRIVERDETAILS_LONGNAME_CHARS     = 128;
  {$EXTERNALSYM ACMDRIVERDETAILS_LONGNAME_CHARS}
  ACMDRIVERDETAILS_COPYRIGHT_CHARS    = 80;
  {$EXTERNALSYM ACMDRIVERDETAILS_COPYRIGHT_CHARS}
  ACMDRIVERDETAILS_LICENSING_CHARS    = 128;
  {$EXTERNALSYM ACMDRIVERDETAILS_LICENSING_CHARS}
  ACMDRIVERDETAILS_FEATURES_CHARS     = 512;
  {$EXTERNALSYM ACMDRIVERDETAILS_FEATURES_CHARS}

type

  PACMDRIVERDETAILSA = ^tACMDRIVERDETAILSA;
  {$EXTERNALSYM PACMDRIVERDETAILSA}
  tACMDRIVERDETAILSA = record
    cbStruct: DWORD;                 // number of valid bytes in structure
    fccType: FOURCC;                 // compressor type 'audc'
    fccComp: FOURCC;                 // sub-type (not used; reserved)
    wMid: WORD;                      // manufacturer id
    wPid: WORD;                      // product id
    vdwACM: DWORD;                   // version of the ACM *compiled* for
    vdwDriver: DWORD;                // version of the driver
    fdwSupport: DWORD;               // misc. support flags
    cFormatTags: DWORD;              // total unique format tags supported
    cFilterTags: DWORD;              // total unique filter tags supported
    hicon: HICON;                    // handle to custom icon
    szShortName: array[0..ACMDRIVERDETAILS_SHORTNAME_CHARS - 1] of AnsiChar;
    szLongName: array[0..ACMDRIVERDETAILS_LONGNAME_CHARS - 1] of AnsiChar;
    szCopyright: array[0..ACMDRIVERDETAILS_COPYRIGHT_CHARS - 1] of AnsiChar;
    szLicensing: array[0..ACMDRIVERDETAILS_LICENSING_CHARS - 1] of AnsiChar;
    szFeatures: array[0..ACMDRIVERDETAILS_FEATURES_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tACMDRIVERDETAILSA}
  _ACMDRIVERDETAILSA = tACMDRIVERDETAILSA;
  LPACMDRIVERDETAILSA = ^tACMDRIVERDETAILSA;
  {$EXTERNALSYM LPACMDRIVERDETAILSA}

  PACMDRIVERDETAILSW = ^tACMDRIVERDETAILSW;
  {$EXTERNALSYM PACMDRIVERDETAILSW}
  tACMDRIVERDETAILSW = record
    cbStruct: DWORD;                 // number of valid bytes in structure
    fccType: FOURCC;                 // compressor type 'audc'
    fccComp: FOURCC;                 // sub-type (not used; reserved)
    wMid: WORD;                      // manufacturer id
    wPid: WORD;                      // product id
    vdwACM: DWORD;                   // version of the ACM *compiled* for
    vdwDriver: DWORD;                // version of the driver
    fdwSupport: DWORD;               // misc. support flags
    cFormatTags: DWORD;              // total unique format tags supported
    cFilterTags: DWORD;              // total unique filter tags supported
    hicon: HICON;                    // handle to custom icon
    szShortName: array[0..ACMDRIVERDETAILS_SHORTNAME_CHARS - 1] of WideChar;
    szLongName: array[0..ACMDRIVERDETAILS_LONGNAME_CHARS - 1] of WideChar;
    szCopyright: array[0..ACMDRIVERDETAILS_COPYRIGHT_CHARS - 1] of WideChar;
    szLicensing: array[0..ACMDRIVERDETAILS_LICENSING_CHARS - 1] of WideChar;
    szFeatures: array[0..ACMDRIVERDETAILS_FEATURES_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tACMDRIVERDETAILSW}
  _ACMDRIVERDETAILSW = tACMDRIVERDETAILSW;
  LPACMDRIVERDETAILSW = ^tACMDRIVERDETAILSW;
  {$EXTERNALSYM LPACMDRIVERDETAILSW}

  { unicode }
  _ACMDRIVERDETAILS = tACMDRIVERDETAILSW;
  PACMDRIVERDETAILS = tACMDRIVERDETAILSW;
  {$EXTERNALSYM PACMDRIVERDETAILS}
  LPACMDRIVERDETAILS = ^tACMDRIVERDETAILSW;
  {$EXTERNALSYM LPACMDRIVERDETAILS}


  //
  //  ACMDRIVERDETAILS.fccType
  //
  //  ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC: the FOURCC used in the fccType
  //  field of the ACMDRIVERDETAILS structure to specify that this is an ACM
  //  codec designed for audio.
  //
  //
  //  ACMDRIVERDETAILS.fccComp
  //
  //  ACMDRIVERDETAILS_FCCCOMP_UNDEFINED: the FOURCC used in the fccComp
  //  field of the ACMDRIVERDETAILS structure. this is currently an unused
  //  field.
  //

const

  ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC = ord('a') or (ord('u') shl 8) or (ord('d') shl 16) or (ord('c') shl 24);
  {$EXTERNALSYM ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC}
  ACMDRIVERDETAILS_FCCCOMP_UNDEFINED  = ord('0') or (ord('0') shl 8) or (ord('0') shl 16) or (ord('0') shl 24);
  {$EXTERNALSYM ACMDRIVERDETAILS_FCCCOMP_UNDEFINED}

  //
  //  the following flags are used to specify the type of conversion(s) that
  //  the converter/codec/filter supports. these are placed in the fdwSupport
  //  field of the ACMDRIVERDETAILS structure. note that a converter can
  //  support one or more of these flags in any combination.
  //
  //  ACMDRIVERDETAILS_SUPPORTF_CODEC: this flag is set if the driver supports
  //  conversions from one format tag to another format tag. for example, if a
  //  converter compresses WAVE_FORMAT_PCM to WAVE_FORMAT_ADPCM, then this bit
  //  should be set.
  //
  //  ACMDRIVERDETAILS_SUPPORTF_CONVERTER: this flags is set if the driver
  //  supports conversions on the same format tag. as an example, the PCM
  //  converter that is built into the ACM sets this bit (and only this bit)
  //  because it converts only PCM formats (bits, sample rate).
  //
  //  ACMDRIVERDETAILS_SUPPORTF_FILTER: this flag is set if the driver supports
  //  transformations on a single format. for example, a converter that changed
  //  the 'volume' of PCM data would set this bit. 'echo' and 'reverb' are
  //  also filter types.
  //
  //  ACMDRIVERDETAILS_SUPPORTF_HARDWARE: this flag is set if the driver supports
  //  hardware input and/or output through a waveform device.
  //
  //  ACMDRIVERDETAILS_SUPPORTF_ASYNC: this flag is set if the driver supports
  //  async conversions.
  //
  //
  //  ACMDRIVERDETAILS_SUPPORTF_LOCAL: this flag is set _by the ACM_ if a
  //  driver has been installed local to the current task. this flag is also
  //  set in the fdwSupport argument to the enumeration callback function
  //  for drivers.
  //
  //  ACMDRIVERDETAILS_SUPPORTF_DISABLED: this flag is set _by the ACM_ if a
  //  driver has been disabled. this flag is also passed set in the fdwSupport
  //  argument to the enumeration callback function for drivers.
  //
  ACMDRIVERDETAILS_SUPPORTF_CODEC     = $00000001;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_CODEC}
  ACMDRIVERDETAILS_SUPPORTF_CONVERTER = $00000002;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_CONVERTER}
  ACMDRIVERDETAILS_SUPPORTF_FILTER    = $00000004;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_FILTER}
  ACMDRIVERDETAILS_SUPPORTF_HARDWARE  = $00000008;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_HARDWARE}
  ACMDRIVERDETAILS_SUPPORTF_ASYNC     = $00000010;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_ASYNC}
  ACMDRIVERDETAILS_SUPPORTF_LOCAL     = $40000000;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_LOCAL}
  ACMDRIVERDETAILS_SUPPORTF_DISABLED  = $80000000;
  {$EXTERNALSYM ACMDRIVERDETAILS_SUPPORTF_DISABLED}


  function acmDriverDetailsA(hadid: HACMDRIVERID;
                             padd: LPACMDRIVERDETAILSA;
                             fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverDetailsA}

  function acmDriverDetailsW(hadid: HACMDRIVERID;
                             padd: LPACMDRIVERDETAILSW;
                             fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverDetailsW}

  { unicode }
  function acmDriverDetails(hadid: HACMDRIVERID;
                            padd: LPACMDRIVERDETAILSW;
                            fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmDriverDetails}


  //--------------------------------------------------------------------------;
  //
  //  ACM Format Tags
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatTagDetails()
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;
  {$EXTERNALSYM ACMFORMATTAGDETAILS_FORMATTAG_CHARS}

type

  PACMFORMATTAGDETAILSA = ^tACMFORMATTAGDETAILSA;
  {$EXTERNALSYM PACMFORMATTAGDETAILSA}
  tACMFORMATTAGDETAILSA = record
    cbStruct: DWORD;
    dwFormatTagIndex: DWORD;
    dwFormatTag: DWORD;
    cbFormatSize: DWORD;
    fdwSupport: DWORD;
    cStandardFormats: DWORD;
    szFormatTag: array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tACMFORMATTAGDETAILSA}
  _ACMFORMATTAGDETAILSA = tACMFORMATTAGDETAILSA;
  LPACMFORMATTAGDETAILSA = ^tACMFORMATTAGDETAILSA;
  {$EXTERNALSYM LPACMFORMATTAGDETAILSA}

  PACMFORMATTAGDETAILSW = ^tACMFORMATTAGDETAILSW;
  {$EXTERNALSYM PACMFORMATTAGDETAILSW}
  tACMFORMATTAGDETAILSW = record
    cbStruct: DWORD;
    dwFormatTagIndex: DWORD;
    dwFormatTag: DWORD;
    cbFormatSize: DWORD;
    fdwSupport: DWORD;
    cStandardFormats: DWORD;
    szFormatTag: array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tACMFORMATTAGDETAILSW}
  _ACMFORMATTAGDETAILSW = tACMFORMATTAGDETAILSW;
  LPACMFORMATTAGDETAILSW = ^tACMFORMATTAGDETAILSW;
  {$EXTERNALSYM LPACMFORMATTAGDETAILSW}

  { unicode }
  _ACMFORMATTAGDETAILS = tACMFORMATTAGDETAILSW;
  PACMFORMATTAGDETAILS = ^tACMFORMATTAGDETAILSW;
  {$EXTERNALSYM PACMFORMATTAGDETAILS}
  LPACMFORMATTAGDETAILS = ^tACMFORMATTAGDETAILSW;
  {$EXTERNALSYM LPACMFORMATTAGDETAILS}


  function acmFormatTagDetailsA(had: HACMDRIVER;
                                paftd: LPACMFORMATTAGDETAILSA;
                                fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagDetailsA}

  function acmFormatTagDetailsW(had: HACMDRIVER;
                                paftd: LPACMFORMATTAGDETAILSW;
                                fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagDetailsW}

  { unicode }
  function acmFormatTagDetails(had: HACMDRIVER;
                               paftd: LPACMFORMATTAGDETAILSW;
                               fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagDetails}

const

  ACM_FORMATTAGDETAILSF_INDEX         = $00000000;
  {$EXTERNALSYM ACM_FORMATTAGDETAILSF_INDEX}
  ACM_FORMATTAGDETAILSF_FORMATTAG     = $00000001;
  {$EXTERNALSYM ACM_FORMATTAGDETAILSF_FORMATTAG}
  ACM_FORMATTAGDETAILSF_LARGESTSIZE   = $00000002;
  {$EXTERNALSYM ACM_FORMATTAGDETAILSF_LARGESTSIZE}
  ACM_FORMATTAGDETAILSF_QUERYMASK     = $0000000F;
  {$EXTERNALSYM ACM_FORMATTAGDETAILSF_QUERYMASK}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatTagEnum()
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type
  ACMFORMATTAGENUMCBA = function(hadid: HACMDRIVERID;
                                 paftd: LPACMFORMATTAGDETAILSA;
                                 dwInstance: DWORD_PTR;
                                 fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFORMATTAGENUMCBA}

  function acmFormatTagEnumA(had: HACMDRIVER;
                             paftd: LPACMFORMATTAGDETAILSA;
                             fnCallback: ACMFORMATTAGENUMCBA;
                             dwInstance: DWORD_PTR;
                             fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagEnumA}

type
  ACMFORMATTAGENUMCBW = function(hadid: HACMDRIVERID;
                                 paftd: LPACMFORMATTAGDETAILSW;
                                 dwInstance: DWORD_PTR;
                                 fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFORMATTAGENUMCBW}

  function acmFormatTagEnumW(had: HACMDRIVER;
                             paftd: LPACMFORMATTAGDETAILSW;
                             fnCallback: ACMFORMATTAGENUMCBW;
                             dwInstance: DWORD_PTR;
                             fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagEnumW}

  { unicode }
  function acmFormatTagEnum(had: HACMDRIVER;
                            paftd: LPACMFORMATTAGDETAILSW;
                            fnCallback: ACMFORMATTAGENUMCBW;
                            dwInstance: DWORD_PTR;
                            fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatTagEnum}


  //--------------------------------------------------------------------------;
  //
  //  ACM Formats
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatDetails()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  ACMFORMATDETAILS_FORMAT_CHARS       = 128;
  {$EXTERNALSYM ACMFORMATDETAILS_FORMAT_CHARS}


type

  tACMFORMATDETAILSA = record
    cbStruct: DWORD;
    dwFormatIndex: DWORD;
    dwFormatTag: DWORD;
    fdwSupport: DWORD;
    pwfx: LPWAVEFORMATEX;
    cbwfx: DWORD;
    szFormat: array[0..ACMFORMATDETAILS_FORMAT_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tACMFORMATDETAILSA}
  _ACMFORMATDETAILSA = tACMFORMATDETAILSA;
  PACMFORMATDETAILSA = ^tACMFORMATDETAILSA;
  {$EXTERNALSYM PACMFORMATDETAILSA}
  LPACMFORMATDETAILSA = ^tACMFORMATDETAILSA;
  {$EXTERNALSYM LPACMFORMATDETAILSA}

  tACMFORMATDETAILSW = record
    cbStruct: DWORD;
    dwFormatIndex: DWORD;
    dwFormatTag: DWORD;
    fdwSupport: DWORD;
    pwfx: LPWAVEFORMATEX;
    cbwfx: DWORD;
    szFormat: array[0..ACMFORMATDETAILS_FORMAT_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tACMFORMATDETAILSW}
  _ACMFORMATDETAILSW = tACMFORMATDETAILSW;
  PACMFORMATDETAILSW = ^tACMFORMATDETAILSW;
  {$EXTERNALSYM PACMFORMATDETAILSW}
  LPACMFORMATDETAILSW = ^tACMFORMATDETAILSW;
  {$EXTERNALSYM LPACMFORMATDETAILSW}

  { unicode }
  _ACMFORMATDETAILS = tACMFORMATDETAILSW;
  PACMFORMATDETAILS = ^tACMFORMATDETAILSW;
  {$EXTERNALSYM PACMFORMATDETAILS}
  LPACMFORMATDETAILS = ^tACMFORMATDETAILSW;
  {$EXTERNALSYM LPACMFORMATDETAILS}


  function acmFormatDetailsA(had: HACMDRIVER;
                             pafd: LPACMFORMATDETAILSA;
                             fdwDetails: DWORD): MMRESULT; stdcall;

  function acmFormatDetailsW(had: HACMDRIVER;
                             pafd: LPACMFORMATDETAILSW;
                             fdwDetails: DWORD): MMRESULT; stdcall;

  { unicode }
  function acmFormatDetails(had: HACMDRIVER;
                            pafd: LPACMFORMATDETAILSW;
                            fdwDetails: DWORD): MMRESULT; stdcall;

const

  ACM_FORMATDETAILSF_INDEX            = $00000000;
  {$EXTERNALSYM ACM_FORMATDETAILSF_INDEX}
  ACM_FORMATDETAILSF_FORMAT           = $00000001;
  {$EXTERNALSYM ACM_FORMATDETAILSF_FORMAT}
  ACM_FORMATDETAILSF_QUERYMASK        = $0000000F;
  {$EXTERNALSYM ACM_FORMATDETAILSF_QUERYMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatEnum()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type
  ACMFORMATENUMCBA = function(hadid: HACMDRIVERID;
                              pafd: LPACMFORMATDETAILSA;
                              dwInstance: DWORD_PTR;
                              fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFORMATENUMCBA}

  function acmFormatEnumA(had: HACMDRIVER;
                          pafd: LPACMFORMATDETAILSA;
                          fnCallback: ACMFORMATENUMCBA;
                          dwInstance: DWORD_PTR;
                          fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatEnumA}

type
  ACMFORMATENUMCBW = function(hadid: HACMDRIVERID;
                              pafd: LPACMFORMATDETAILSW;
                              dwInstance: DWORD_PTR;
                              fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFORMATENUMCBW}

  function acmFormatEnumW(had: HACMDRIVER;
                          pafd: LPACMFORMATDETAILSW;
                          fnCallback: ACMFORMATENUMCBW;
                          dwInstance: DWORD_PTR;
                          fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatEnumW}

  { unicode }
  function acmFormatEnum(had: HACMDRIVER;
                          pafd: LPACMFORMATDETAILSW;
                          fnCallback: ACMFORMATENUMCBW;
                          dwInstance: DWORD_PTR;
                          fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatEnum}

const

  ACM_FORMATENUMF_WFORMATTAG          = $00010000;
  {$EXTERNALSYM ACM_FORMATENUMF_WFORMATTAG}
  ACM_FORMATENUMF_NCHANNELS           = $00020000;
  {$EXTERNALSYM ACM_FORMATENUMF_NCHANNELS}
  ACM_FORMATENUMF_NSAMPLESPERSEC      = $00040000;
  {$EXTERNALSYM ACM_FORMATENUMF_NSAMPLESPERSEC}
  ACM_FORMATENUMF_WBITSPERSAMPLE      = $00080000;
  {$EXTERNALSYM ACM_FORMATENUMF_WBITSPERSAMPLE}
  ACM_FORMATENUMF_CONVERT             = $00100000;
  {$EXTERNALSYM ACM_FORMATENUMF_CONVERT}
  ACM_FORMATENUMF_SUGGEST             = $00200000;
  {$EXTERNALSYM ACM_FORMATENUMF_SUGGEST}
  ACM_FORMATENUMF_HARDWARE            = $00400000;
  {$EXTERNALSYM ACM_FORMATENUMF_HARDWARE}
  ACM_FORMATENUMF_INPUT               = $00800000;
  {$EXTERNALSYM ACM_FORMATENUMF_INPUT}
  ACM_FORMATENUMF_OUTPUT              = $01000000;
  {$EXTERNALSYM ACM_FORMATENUMF_OUTPUT}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatSuggest()
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmFormatSuggest(had: HACMDRIVER;
                            pwfxSrc: LPWAVEFORMATEX;
                            pwfxDst: LPWAVEFORMATEX;
                            cbwfxDst: DWORD;
                            fdwSuggest: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatSuggest}

const

  ACM_FORMATSUGGESTF_WFORMATTAG       = $00010000;
  {$EXTERNALSYM ACM_FORMATSUGGESTF_WFORMATTAG}
  ACM_FORMATSUGGESTF_NCHANNELS        = $00020000;
  {$EXTERNALSYM ACM_FORMATSUGGESTF_NCHANNELS}
  ACM_FORMATSUGGESTF_NSAMPLESPERSEC   = $00040000;
  {$EXTERNALSYM ACM_FORMATSUGGESTF_NSAMPLESPERSEC}
  ACM_FORMATSUGGESTF_WBITSPERSAMPLE   = $00080000;
  {$EXTERNALSYM ACM_FORMATSUGGESTF_WBITSPERSAMPLE}

  ACM_FORMATSUGGESTF_TYPEMASK         = $00FF0000;
  {$EXTERNALSYM ACM_FORMATSUGGESTF_TYPEMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFormatChoose()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  {$EXTERNALSYM ACMHELPMSGSTRINGA}
  ACMHELPMSGSTRINGA                   = 'acmchoose_help';
  {$EXTERNALSYM ACMHELPMSGSTRINGW}
  ACMHELPMSGSTRINGW                   = 'acmchoose_help';
  {$EXTERNALSYM ACMHELPMSGCONTEXTMENUA}
  ACMHELPMSGCONTEXTMENUA              = 'acmchoose_contextmenu';
  {$EXTERNALSYM ACMHELPMSGCONTEXTMENUW}
  ACMHELPMSGCONTEXTMENUW              = 'acmchoose_contextmenu';
  {$EXTERNALSYM ACMHELPMSGCONTEXTHELPA}
  ACMHELPMSGCONTEXTHELPA              = 'acmchoose_contexthelp';
  {$EXTERNALSYM ACMHELPMSGCONTEXTHELPW}
  ACMHELPMSGCONTEXTHELPW              = 'acmchoose_contexthelp';

  { unicode }
{$IFDEF UNICODE}
  {$EXTERNALSYM ACMHELPMSGSTRING}
  ACMHELPMSGSTRING                    = ACMHELPMSGSTRINGW;
  {$EXTERNALSYM ACMHELPMSGCONTEXTMENU}
  ACMHELPMSGCONTEXTMENU               = ACMHELPMSGCONTEXTMENUW;
  {$EXTERNALSYM ACMHELPMSGCONTEXTHELP}
  ACMHELPMSGCONTEXTHELP               = ACMHELPMSGCONTEXTHELPW;
{$ELSE}
  {$EXTERNALSYM ACMHELPMSGSTRING}
  ACMHELPMSGSTRING                    = ACMHELPMSGSTRINGA;
  {$EXTERNALSYM ACMHELPMSGCONTEXTMENU}
  ACMHELPMSGCONTEXTMENU               = ACMHELPMSGCONTEXTMENUA;
  {$EXTERNALSYM ACMHELPMSGCONTEXTHELP}
  ACMHELPMSGCONTEXTHELP               = ACMHELPMSGCONTEXTHELPA;
{$ENDIF}

  //
  //  MM_ACM_FORMATCHOOSE is sent to hook callbacks by the Format Chooser
  //  Dialog...
  //

const

  MM_ACM_FORMATCHOOSE                 = ($8000);
  {$EXTERNALSYM MM_ACM_FORMATCHOOSE}

  FORMATCHOOSE_MESSAGE                = 0;
  {$EXTERNALSYM FORMATCHOOSE_MESSAGE}
  FORMATCHOOSE_FORMATTAG_VERIFY       = (FORMATCHOOSE_MESSAGE + 0);
  {$EXTERNALSYM FORMATCHOOSE_FORMATTAG_VERIFY}
  FORMATCHOOSE_FORMAT_VERIFY          = (FORMATCHOOSE_MESSAGE + 1);
  {$EXTERNALSYM FORMATCHOOSE_FORMAT_VERIFY}
  FORMATCHOOSE_CUSTOM_VERIFY          = (FORMATCHOOSE_MESSAGE + 2);
  {$EXTERNALSYM FORMATCHOOSE_CUSTOM_VERIFY}

type
  ACMFORMATCHOOSEHOOKPROCA = function(hwnd: HWND;
                                      uMsg: UINT;
                                      wParam: WPARAM;
                                      lParam: LPARAM): UINT; stdcall;
  {$EXTERNALSYM ACMFORMATCHOOSEHOOKPROCA}

type
  ACMFORMATCHOOSEHOOKPROCW = function(hwnd: HWND;
                                      uMsg: UINT;
                                      wParam: WPARAM;
                                      lParam: LPARAM): UINT; stdcall;
  {$EXTERNALSYM ACMFORMATCHOOSEHOOKPROCW}

{$IFDEF UNICODE}
type
  ACMFORMATCHOOSEHOOKPROC             = ACMFORMATCHOOSEHOOKPROCW;
  {$EXTERNALSYM ACMFORMATCHOOSEHOOKPROC}
{$ELSE}
type
  ACMFORMATCHOOSEHOOKPROC             = ACMFORMATCHOOSEHOOKPROCA;
  {$EXTERNALSYM ACMFORMATCHOOSEHOOKPROC}
{$ENDIF}

  //
  //
  //
  //

type

  tACMFORMATCHOOSEA = record
    cbStruct: DWORD;                     // sizeof(ACMFORMATCHOOSE)
    fdwStyle: DWORD;                     // chooser style flags
    hwndOwner: HWND;                     // caller's window handle
    pwfx: LPWAVEFORMATEX;                // ptr to wfx buf to receive choice
    cbwfx: DWORD;                        // size of mem buf for pwfx
    pszTitle: PAnsiChar;                 // dialog box title bar
    szFormatTag: array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS - 1] of AnsiChar;
    szFormat: array[0..ACMFORMATDETAILS_FORMAT_CHARS - 1] of AnsiChar;
    pszName: PAnsiChar;                  // custom name selection
    cchName: DWORD;                      // size in chars of mem buf for pszName
    fdwEnum: DWORD;                      // format enumeration restrictions
    pwfxEnum: LPWAVEFORMATEX;            // format describing restrictions
    _hInstance: HINST;                   // app instance containing dlg template
    pszTemplateName: PAnsiChar;          // custom template name
    lCustData: LPARAM;                   // data passed to hook fn.
    pfnHook: ACMFORMATCHOOSEHOOKPROCA;   // ptr to hook function
  end;
  {$EXTERNALSYM tACMFORMATCHOOSEA}
  _ACMFORMATCHOOSEA = tACMFORMATCHOOSEA;
  PACMFORMATCHOOSEA = ^tACMFORMATCHOOSEA;
  {$EXTERNALSYM PACMFORMATCHOOSEA}
  LPACMFORMATCHOOSEA = ^tACMFORMATCHOOSEA;
  {$EXTERNALSYM LPACMFORMATCHOOSEA}

  tACMFORMATCHOOSEW = record
    cbStruct: DWORD;                     // sizeof(ACMFORMATCHOOSE)
    fdwStyle: DWORD;                     // chooser style flags
    hwndOwner: HWND;                     // caller's window handle
    pwfx: LPWAVEFORMATEX;                // ptr to wfx buf to receive choice
    cbwfx: DWORD;                        // size of mem buf for pwfx
    pszTitle: PWideChar;                 // dialog box title bar
    szFormatTag: array[0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS - 1] of WideChar;
    szFormat: array[0..ACMFORMATDETAILS_FORMAT_CHARS - 1] of WideChar;
    pszName: PWideChar;                  // custom name selection
    cchName: DWORD;                      // size in chars of mem buf for pszName
    fdwEnum: DWORD;                      // format enumeration restrictions
    pwfxEnum: LPWAVEFORMATEX;            // format describing restrictions
    _hInstance: HINST;                   // app instance containing dlg template
    pszTemplateName: PWideChar;          // custom template name
    lCustData: LPARAM;                   // data passed to hook fn.
    pfnHook: ACMFORMATCHOOSEHOOKPROCW;   // ptr to hook function
  end;
  {$EXTERNALSYM tACMFORMATCHOOSEW}
  _ACMFORMATCHOOSEW = tACMFORMATCHOOSEW;
  PACMFORMATCHOOSEW = ^tACMFORMATCHOOSEW;
  {$EXTERNALSYM PACMFORMATCHOOSEW}
  LPACMFORMATCHOOSEW = ^tACMFORMATCHOOSEW;
  {$EXTERNALSYM LPACMFORMATCHOOSEW}

{$IFDEF UNICODE}
  {$EXTERNALSYM ACMFORMATCHOOSE}
  _ACMFORMATCHOOSE                    = _ACMFORMATCHOOSEW;
  PACMFORMATCHOOSE                    = PACMFORMATCHOOSEW;
  {$EXTERNALSYM LPACMFORMATCHOOSE}
  LPACMFORMATCHOOSE                   = LPACMFORMATCHOOSEW;
{$ELSE}
  {$EXTERNALSYM ACMFORMATCHOOSE}
  _ACMFORMATCHOOSE                    = _ACMFORMATCHOOSEA;
  {$EXTERNALSYM PACMFORMATCHOOSE}
  PACMFORMATCHOOSE                    = PACMFORMATCHOOSEA;
  {$EXTERNALSYM LPACMFORMATCHOOSE}
  LPACMFORMATCHOOSE                   = LPACMFORMATCHOOSEA;
{$ENDIF}

  //
  //  ACMFORMATCHOOSE.fdwStyle
  //
  //
  //

const

  ACMFORMATCHOOSE_STYLEF_SHOWHELP     = $00000004;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_SHOWHELP}
  ACMFORMATCHOOSE_STYLEF_ENABLEHOOK   = $00000008;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_ENABLEHOOK}
  ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE= $00000010;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE}
  ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE= $00000020;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE}
  ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT= $00000040;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT}
  ACMFORMATCHOOSE_STYLEF_CONTEXTHELP  = $00000080;
  {$EXTERNALSYM ACMFORMATCHOOSE_STYLEF_CONTEXTHELP}


  function acmFormatChooseA(pafmtc: LPACMFORMATCHOOSEA): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatChooseA}

  function acmFormatChooseW(pafmtc: LPACMFORMATCHOOSEW): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatChooseW}

  { unicode }
  function acmFormatChoose(pafmtc: LPACMFORMATCHOOSEW): MMRESULT; stdcall;
  {$EXTERNALSYM acmFormatChoose}


  //--------------------------------------------------------------------------;
  //
  //  ACM Filter Tags
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFilterTagDetails()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  ACMFILTERTAGDETAILS_FILTERTAG_CHARS = 48;
  {$EXTERNALSYM ACMFILTERTAGDETAILS_FILTERTAG_CHARS}

type

  PACMFILTERTAGDETAILSA = ^tACMFILTERTAGDETAILSA;
  {$EXTERNALSYM PACMFILTERTAGDETAILSA}
  tACMFILTERTAGDETAILSA = record
    cbStruct: DWORD;
    dwFilterTagIndex: DWORD;
    dwFilterTag: DWORD;
    cbFilterSize: DWORD;
    fdwSupport: DWORD;
    cStandardFilters: DWORD;
    szFilterTag: array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tACMFILTERTAGDETAILSA}
  _ACMFILTERTAGDETAILSA = tACMFILTERTAGDETAILSA;
  LPACMFILTERTAGDETAILSA = ^tACMFILTERTAGDETAILSA;
  {$EXTERNALSYM LPACMFILTERTAGDETAILSA}

  tACMFILTERTAGDETAILSW = record
    cbStruct: DWORD;
    dwFilterTagIndex: DWORD;
    dwFilterTag: DWORD;
    cbFilterSize: DWORD;
    fdwSupport: DWORD;
    cStandardFilters: DWORD;
    szFilterTag: array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tACMFILTERTAGDETAILSW}
  _ACMFILTERTAGDETAILSW = tACMFILTERTAGDETAILSW;
  PACMFILTERTAGDETAILSW = ^tACMFILTERTAGDETAILSW;
  {$EXTERNALSYM PACMFILTERTAGDETAILSW}
  LPACMFILTERTAGDETAILSW = ^tACMFILTERTAGDETAILSW;
  {$EXTERNALSYM LPACMFILTERTAGDETAILSW}

  { unicode }
  _ACMFILTERTAGDETAILS = tACMFILTERTAGDETAILSW;
  PACMFILTERTAGDETAILS = ^tACMFILTERTAGDETAILSW;
  {$EXTERNALSYM PACMFILTERTAGDETAILS}
  LPACMFILTERTAGDETAILS = ^tACMFILTERTAGDETAILSW;
  {$EXTERNALSYM LPACMFILTERTAGDETAILS}

  function acmFilterTagDetailsA(had: HACMDRIVER;
                                paftd: LPACMFILTERTAGDETAILSA;
                                fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagDetailsA}

  function acmFilterTagDetailsW(had: HACMDRIVER;
                                paftd: LPACMFILTERTAGDETAILSW;
                                fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagDetailsW}

  function acmFilterTagDetails(had: HACMDRIVER;
                               paftd: LPACMFILTERTAGDETAILSW;
                               fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagDetails}

const

  ACM_FILTERTAGDETAILSF_INDEX         = $00000000;
  {$EXTERNALSYM ACM_FILTERTAGDETAILSF_INDEX}
  ACM_FILTERTAGDETAILSF_FILTERTAG     = $00000001;
  {$EXTERNALSYM ACM_FILTERTAGDETAILSF_FILTERTAG}
  ACM_FILTERTAGDETAILSF_LARGESTSIZE   = $00000002;
  {$EXTERNALSYM ACM_FILTERTAGDETAILSF_LARGESTSIZE}
  ACM_FILTERTAGDETAILSF_QUERYMASK     = $0000000F;
  {$EXTERNALSYM ACM_FILTERTAGDETAILSF_QUERYMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFilterTagEnum()
  //
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type
  ACMFILTERTAGENUMCBA = function(hadid: HACMDRIVERID;
                                 paftd: LPACMFILTERTAGDETAILSA;
                                 dwInstance: DWORD_PTR;
                                 fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFILTERTAGENUMCBA}

  function acmFilterTagEnumA(had: HACMDRIVER;
                             paftd: LPACMFILTERTAGDETAILSA;
                             fnCallback: ACMFILTERTAGENUMCBA;
                             dwInstance: DWORD_PTR;
                             fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagEnumA}

type
  ACMFILTERTAGENUMCBW = function(hadid: HACMDRIVERID;
                                 paftd: LPACMFILTERTAGDETAILSW;
                                 dwInstance: DWORD_PTR;
                                 fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFILTERTAGENUMCBW}

  function acmFilterTagEnumW(had: HACMDRIVER;
                             paftd: LPACMFILTERTAGDETAILSW;
                             fnCallback: ACMFILTERTAGENUMCBW;
                             dwInstance: DWORD_PTR;
                             fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagEnumW}

  { unicode }
type
  ACMFILTERTAGENUMCB = ACMFILTERTAGENUMCBW;
  {$EXTERNALSYM ACMFILTERTAGENUMCB}

  function acmFilterTagEnum(had: HACMDRIVER;
                            paftd: LPACMFILTERTAGDETAILSW;
                            fnCallback: ACMFILTERTAGENUMCBW;
                            dwInstance: DWORD_PTR;
                            fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterTagEnum}



  //--------------------------------------------------------------------------;
  //
  //  ACM Filters
  //
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFilterDetails()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

  ACMFILTERDETAILS_FILTER_CHARS       = 128;
  {$EXTERNALSYM ACMFILTERDETAILS_FILTER_CHARS}

type

  PACMFILTERDETAILSA = ^tACMFILTERDETAILSA;
  {$EXTERNALSYM PACMFILTERDETAILSA}
  tACMFILTERDETAILSA = record
    cbStruct: DWORD;
    dwFilterIndex: DWORD;
    dwFilterTag: DWORD;
    fdwSupport: DWORD;
    pwfltr: LPWAVEFILTER;
    cbwfltr: DWORD;
    szFilter: array[0..ACMFILTERDETAILS_FILTER_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tACMFILTERDETAILSA}
  _ACMFILTERDETAILSA = tACMFILTERDETAILSA;
  LPACMFILTERDETAILSA = ^tACMFILTERDETAILSA;
  {$EXTERNALSYM LPACMFILTERDETAILSA}

  PACMFILTERDETAILSW = ^tACMFILTERDETAILSW;
  {$EXTERNALSYM PACMFILTERDETAILSW}
  tACMFILTERDETAILSW = record
    cbStruct: DWORD;
    dwFilterIndex: DWORD;
    dwFilterTag: DWORD;
    fdwSupport: DWORD;
    pwfltr: LPWAVEFILTER;
    cbwfltr: DWORD;
    szFilter: array[0..ACMFILTERDETAILS_FILTER_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tACMFILTERDETAILSW}
  _ACMFILTERDETAILSW = tACMFILTERDETAILSW;
  LPACMFILTERDETAILSW = ^tACMFILTERDETAILSW;
  {$EXTERNALSYM LPACMFILTERDETAILSW}

{$IFDEF UNICODE}
  _ACMFILTERDETAILS                   = _ACMFILTERDETAILSW;
  {$EXTERNALSYM ACMFILTERDETAILS}
  PACMFILTERDETAILS                   = PACMFILTERDETAILSW;
  {$EXTERNALSYM PACMFILTERDETAILS}
  LPACMFILTERDETAILS                  = LPACMFILTERDETAILSW;
  {$EXTERNALSYM LPACMFILTERDETAILS}
{$ELSE}
  _ACMFILTERDETAILS                   = _ACMFILTERDETAILSA;
  {$EXTERNALSYM ACMFILTERDETAILS}
  PACMFILTERDETAILS                   = PACMFILTERDETAILSA;
  {$EXTERNALSYM PACMFILTERDETAILS}
  LPACMFILTERDETAILS                  = LPACMFILTERDETAILSA;
  {$EXTERNALSYM LPACMFILTERDETAILS}
{$ENDIF}


  function acmFilterDetailsA(had: HACMDRIVER;
                             pafd: LPACMFILTERDETAILSA;
                             fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterDetailsA}

  function acmFilterDetailsW(had: HACMDRIVER;
                             pafd: LPACMFILTERDETAILSW;
                             fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterDetailsW}

  { unicode }
  function acmFilterDetails(had: HACMDRIVER;
                            pafd: LPACMFILTERDETAILSW;
                            fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterDetails}

const

  ACM_FILTERDETAILSF_INDEX            = $00000000;
  {$EXTERNALSYM ACM_FILTERDETAILSF_INDEX}
  ACM_FILTERDETAILSF_FILTER           = $00000001;
  {$EXTERNALSYM ACM_FILTERDETAILSF_FILTER}
  ACM_FILTERDETAILSF_QUERYMASK        = $0000000F;
  {$EXTERNALSYM ACM_FILTERDETAILSF_QUERYMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFilterEnum()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

type
  ACMFILTERENUMCBA = function(hadid: HACMDRIVERID;
                              pafd: LPACMFILTERDETAILSA;
                              dwInstance: DWORD_PTR;
                              fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFILTERENUMCBA}

  function acmFilterEnumA(had: HACMDRIVER;
                          pafd: LPACMFILTERDETAILSA;
                          fnCallback: ACMFILTERENUMCBA;
                          dwInstance: DWORD_PTR;
                          fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterEnumA}

type
  ACMFILTERENUMCBW = function(hadid: HACMDRIVERID;
                              pafd: LPACMFILTERDETAILSW;
                              dwInstance: DWORD_PTR;
                              fdwSupport: DWORD): BOOL; stdcall;
  {$EXTERNALSYM ACMFILTERENUMCBW}

  function acmFilterEnumW(had: HACMDRIVER;
                          pafd: LPACMFILTERDETAILSW;
                          fnCallback: ACMFILTERENUMCBW;
                          dwInstance: DWORD_PTR;
                          fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterEnumW}

  { unicode }
type
  ACMFILTERENUMCB = ACMFILTERENUMCBW;
  {$EXTERNALSYM ACMFILTERENUMCB}

  function acmFilterEnum(had: HACMDRIVER;
                         pafd: LPACMFILTERDETAILSW;
                         fnCallback: ACMFILTERENUMCBW;
                         dwInstance: DWORD_PTR;
                         fdwEnum: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterEnum}

const

  ACM_FILTERENUMF_DWFILTERTAG         = $00010000;
  {$EXTERNALSYM ACM_FILTERENUMF_DWFILTERTAG}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmFilterChoose()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  //
  //  MM_ACM_FILTERCHOOSE is sent to hook callbacks by the Filter Chooser
  //  Dialog...
  //

const

  MM_ACM_FILTERCHOOSE                 = ($8000);
  {$EXTERNALSYM MM_ACM_FILTERCHOOSE}

  FILTERCHOOSE_MESSAGE                = 0;
  {$EXTERNALSYM FILTERCHOOSE_MESSAGE}
  FILTERCHOOSE_FILTERTAG_VERIFY       = (FILTERCHOOSE_MESSAGE + 0);
  {$EXTERNALSYM FILTERCHOOSE_FILTERTAG_VERIFY}
  FILTERCHOOSE_FILTER_VERIFY          = (FILTERCHOOSE_MESSAGE + 1);
  {$EXTERNALSYM FILTERCHOOSE_FILTER_VERIFY}
  FILTERCHOOSE_CUSTOM_VERIFY          = (FILTERCHOOSE_MESSAGE + 2);
  {$EXTERNALSYM FILTERCHOOSE_CUSTOM_VERIFY}

type
  ACMFILTERCHOOSEHOOKPROCA = function(hwnd: HWND;
                                      uMsg: UINT;
                                      wParam: WPARAM;
                                      lParam: LPARAM): UINT; stdcall;
  {$EXTERNALSYM ACMFILTERCHOOSEHOOKPROCA}

type
  ACMFILTERCHOOSEHOOKPROCW = function(hwnd: HWND;
                                      uMsg: UINT;
                                      wParam: WPARAM;
                                      lParam: LPARAM): UINT; stdcall;
  {$EXTERNALSYM ACMFILTERCHOOSEHOOKPROCW}

  { unicode }
type
  ACMFILTERCHOOSEHOOKPROC = function(hwnd: HWND;
                                     uMsg: UINT;
                                     wParam: WPARAM;
                                     lParam: LPARAM): UINT; stdcall;
  {$EXTERNALSYM ACMFILTERCHOOSEHOOKPROC}


  //
  //  ACMFILTERCHOOSE
  //
  //

type

  PACMFILTERCHOOSEA = ^tACMFILTERCHOOSEA;
  {$EXTERNALSYM PACMFILTERCHOOSEA}
  tACMFILTERCHOOSEA = record
    cbStruct: DWORD;                     // sizeof(ACMFILTERCHOOSE)
    fdwStyle: DWORD;                     // chooser style flags
    hwndOwner: HWND;                     // caller's window handle
    pwfltr: LPWAVEFILTER;                // ptr to wfltr buf to receive choice
    cbwfltr: DWORD;                      // size of mem buf for pwfltr
    pszTitle: PAnsiChar;
    szFilterTag: array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS - 1] of AnsiChar;
    szFilter: array[0..ACMFILTERDETAILS_FILTER_CHARS - 1] of AnsiChar;
    pszName: PAnsiChar;                  // custom name selection
    cchName: DWORD;                      // size in chars of mem buf for pszName
    fdwEnum: DWORD;                      // filter enumeration restrictions
    pwfltrEnum: LPWAVEFILTER;            // filter describing restrictions
    _hInstance: HINST;                   // app instance containing dlg template
    pszTemplateName: PAnsiChar;          // custom template name
    lCustData: LPARAM;                   // data passed to hook fn.
    pfnHook: ACMFILTERCHOOSEHOOKPROCA;   // ptr to hook function
  end;
  {$EXTERNALSYM tACMFILTERCHOOSEA}
  _ACMFILTERCHOOSEA = tACMFILTERCHOOSEA;
  LPACMFILTERCHOOSEA = ^tACMFILTERCHOOSEA;
  {$EXTERNALSYM LPACMFILTERCHOOSEA}

  PACMFILTERCHOOSEW = ^tACMFILTERCHOOSEW;
  {$EXTERNALSYM PACMFILTERCHOOSEW}
  tACMFILTERCHOOSEW = record
    cbStruct: DWORD;                     // sizeof(ACMFILTERCHOOSE)
    fdwStyle: DWORD;                     // chooser style flags
    hwndOwner: HWND;                     // caller's window handle
    pwfltr: LPWAVEFILTER;                // ptr to wfltr buf to receive choice
    cbwfltr: DWORD;                      // size of mem buf for pwfltr
    pszTitle: PWideChar;
    szFilterTag: array[0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS - 1] of WideChar;
    szFilter: array[0..ACMFILTERDETAILS_FILTER_CHARS - 1] of WideChar;
    pszName: PWideChar;                  // custom name selection
    cchName: DWORD;                      // size in chars of mem buf for pszName
    fdwEnum: DWORD;                      // filter enumeration restrictions
    pwfltrEnum: LPWAVEFILTER;            // filter describing restrictions
    _hInstance: HINST;                   // app instance containing dlg template
    pszTemplateName: PWideChar;          // custom template name
    lCustData: LPARAM;                   // data passed to hook fn.
    pfnHook: ACMFILTERCHOOSEHOOKPROCW;   // ptr to hook function
  end;
  {$EXTERNALSYM tACMFILTERCHOOSEW}
  _ACMFILTERCHOOSEW = tACMFILTERCHOOSEW;
  LPACMFILTERCHOOSEW = ^tACMFILTERCHOOSEW;
  {$EXTERNALSYM LPACMFILTERCHOOSEW}

{$IFDEF UNICODE}
  _ACMFILTERCHOOSE                    = _ACMFILTERCHOOSEW;
  {$EXTERNALSYM ACMFILTERCHOOSE}
  PACMFILTERCHOOSE                    = PACMFILTERCHOOSEW;
  {$EXTERNALSYM PACMFILTERCHOOSE}
  LPACMFILTERCHOOSE                   = LPACMFILTERCHOOSEW;
  {$EXTERNALSYM LPACMFILTERCHOOSE}
{$ELSE}
  _ACMFILTERCHOOSE                    = _ACMFILTERCHOOSEA;
  {$EXTERNALSYM ACMFILTERCHOOSE}
  PACMFILTERCHOOSE                    = PACMFILTERCHOOSEA;
  {$EXTERNALSYM PACMFILTERCHOOSE}
  LPACMFILTERCHOOSE                   = LPACMFILTERCHOOSEA;
  {$EXTERNALSYM LPACMFILTERCHOOSE}
{$ENDIF}

  //
  //  ACMFILTERCHOOSE.fdwStyle
  //
  //

const

  ACMFILTERCHOOSE_STYLEF_SHOWHELP     = $00000004;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_SHOWHELP}
  ACMFILTERCHOOSE_STYLEF_ENABLEHOOK   = $00000008;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_ENABLEHOOK}
  ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATE= $00000010;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATE}
  ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATEHANDLE= $00000020;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATEHANDLE}
  ACMFILTERCHOOSE_STYLEF_INITTOFILTERSTRUCT= $00000040;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_INITTOFILTERSTRUCT}
  ACMFILTERCHOOSE_STYLEF_CONTEXTHELP  = $00000080;
  {$EXTERNALSYM ACMFILTERCHOOSE_STYLEF_CONTEXTHELP}


  function acmFilterChooseA(pafltrc: LPACMFILTERCHOOSEA): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterChooseA}

  function acmFilterChooseW(pafltrc: LPACMFILTERCHOOSEW): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterChooseW}

  { unicode }
  function acmFilterChoose(pafltrc: LPACMFILTERCHOOSEW): MMRESULT; stdcall;
  {$EXTERNALSYM acmFilterChoose}


  //--------------------------------------------------------------------------;
  //
  //  ACM Stream API's
  //
  //
  //
  //--------------------------------------------------------------------------;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamOpen()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

const

{$IFDEF WIN32}
  _DRVRESERVED                        = 10;
  {$EXTERNALSYM _DRVRESERVED}
{$ELSE}
  _DRVRESERVED                        = 15;  // WIN64
  {$EXTERNALSYM _DRVRESERVED}
{$ENDIF}


type

  tACMSTREAMHEADER = record
    cbStruct: DWORD;                                         // sizeof(ACMSTREAMHEADER)
    fdwStatus: DWORD;                                        // ACMSTREAMHEADER_STATUSF_*
    dwUser: DWORD_PTR;                                       // user instance data for hdr
    pbSrc: PByte;
    cbSrcLength: DWORD;
    cbSrcLengthUsed: DWORD;
    dwSrcUser: DWORD_PTR;                                    // user instance data for src
    pbDst: PByte;
    cbDstLength: DWORD;
    cbDstLengthUsed: DWORD;
    dwDstUser: DWORD_PTR;                                    // user instance data for dst
    dwReservedDriver: array[0.._DRVRESERVED - 1] of DWORD;  // driver reserved work space
  end;
  {$EXTERNALSYM tACMSTREAMHEADER}
  ACMSTREAMHEADER = tACMSTREAMHEADER;
  {$EXTERNALSYM ACMSTREAMHEADER}
  PACMSTREAMHEADER = ^tACMSTREAMHEADER;
  {$EXTERNALSYM PACMSTREAMHEADER}
  LPACMSTREAMHEADER = ^tACMSTREAMHEADER;
  {$EXTERNALSYM LPACMSTREAMHEADER}

  //
  //  ACMSTREAMHEADER.fdwStatus
  //
  //  ACMSTREAMHEADER_STATUSF_DONE: done bit for async conversions.
  //

const

  ACMSTREAMHEADER_STATUSF_DONE        = $00010000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_DONE}
  ACMSTREAMHEADER_STATUSF_PREPARED    = $00020000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_PREPARED}
  ACMSTREAMHEADER_STATUSF_INQUEUE     = $00100000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_INQUEUE}


  function acmStreamOpen(phas: LPHACMSTREAM;      // pointer to stream handle
                         had: HACMDRIVER;         // optional driver handle
                         pwfxSrc: LPWAVEFORMATEX; // source format to convert
                         pwfxDst: LPWAVEFORMATEX; // required destination format
                         pwfltr: LPWAVEFILTER;    // optional filter
                         dwCallback: DWORD_PTR;   // callback
                         dwInstance: DWORD_PTR;   // callback instance data
                         fdwOpen: DWORD): MMRESULT; stdcall;

const

  ACM_STREAMOPENF_QUERY               = $00000001;
  {$EXTERNALSYM ACM_STREAMOPENF_QUERY}
  ACM_STREAMOPENF_ASYNC               = $00000002;
  {$EXTERNALSYM ACM_STREAMOPENF_ASYNC}
  ACM_STREAMOPENF_NONREALTIME         = $00000004;
  {$EXTERNALSYM ACM_STREAMOPENF_NONREALTIME}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamClose()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamClose(has: HACMSTREAM;
                          fdwClose: DWORD): MMRESULT; stdcall;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamSize()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamSize(has: HACMSTREAM;
                         cbInput: DWORD;
                         pdwOutputBytes: PDWORD;
                         fdwSize: DWORD): MMRESULT; stdcall;

const

  ACM_STREAMSIZEF_SOURCE              = $00000000;
  {$EXTERNALSYM ACM_STREAMSIZEF_SOURCE}
  ACM_STREAMSIZEF_DESTINATION         = $00000001;
  {$EXTERNALSYM ACM_STREAMSIZEF_DESTINATION}
  ACM_STREAMSIZEF_QUERYMASK           = $0000000F;
  {$EXTERNALSYM ACM_STREAMSIZEF_QUERYMASK}


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamReset()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamReset(has: HACMSTREAM;
                          fdwReset: DWORD): MMRESULT; stdcall;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamMessage()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamMessage(has: HACMSTREAM;
                            uMsg: UINT;
                            lParam1: LPARAM;
                            lParam2: LPARAM): MMRESULT; stdcall;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamConvert()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamConvert(has: HACMSTREAM;
                            pash: LPACMSTREAMHEADER;
                            fdwConvert: DWORD): MMRESULT; stdcall;

const

  ACM_STREAMCONVERTF_BLOCKALIGN       = $00000004;
  {$EXTERNALSYM ACM_STREAMCONVERTF_BLOCKALIGN}
  ACM_STREAMCONVERTF_START            = $00000010;
  {$EXTERNALSYM ACM_STREAMCONVERTF_START}
  ACM_STREAMCONVERTF_END              = $00000020;
  {$EXTERNALSYM ACM_STREAMCONVERTF_END}

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamPrepareHeader()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamPrepareHeader(has: HACMSTREAM;
                                  pash: LPACMSTREAMHEADER;
                                  fdwPrepare: DWORD): MMRESULT; stdcall;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
  //
  //  acmStreamUnprepareHeader()
  //
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  function acmStreamUnprepareHeader(has: HACMSTREAM;
                                    pash: LPACMSTREAMHEADER;
                                    fdwUnprepare: DWORD): MMRESULT; stdcall;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  MsAcmLib = 'Msacm32.dll';

function acmGetVersion; external MsAcmLib name 'acmGetVersion';
function acmMetrics;    external MsAcmLib name 'acmMetrics';

function acmDriverEnum; external MsAcmLib name 'acmDriverEnum';
function acmDriverID;   external MsAcmLib name 'acmDriverID';

function acmDriverAddA; external MsAcmLib name 'acmDriverAddA';
function acmDriverAddW; external MsAcmLib name 'acmDriverAddW';
function acmDriverAdd;  external MsAcmLib name 'acmDriverAddW';

function acmDriverRemove;   external MsAcmLib name 'acmDriverRemove';
function acmDriverOpen;     external MsAcmLib name 'acmDriverOpen';
function acmDriverClose;    external MsAcmLib name 'acmDriverClose';
function acmDriverMessage;  external MsAcmLib name 'acmDriverMessage';
function acmDriverPriority; external MsAcmLib name 'acmDriverPriority';

function acmDriverDetailsA; external MsAcmLib name 'acmDriverDetailsA';
function acmDriverDetailsW; external MsAcmLib name 'acmDriverDetailsW';
function acmDriverDetails;  external MsAcmLib name 'acmDriverDetailsW';

function acmFormatTagDetailsA; external MsAcmLib name 'acmFormatTagDetailsA';
function acmFormatTagDetailsW; external MsAcmLib name 'acmFormatTagDetailsW';
function acmFormatTagDetails;  external MsAcmLib name 'acmFormatTagDetailsW';

function acmFormatTagEnumA; external MsAcmLib name 'acmFormatTagEnumA';
function acmFormatTagEnumW; external MsAcmLib name 'acmFormatTagEnumW';
function acmFormatTagEnum;  external MsAcmLib name 'acmFormatTagEnumW';

function acmFormatDetailsA; external MsAcmLib name 'acmFormatDetailsA';
function acmFormatDetailsW; external MsAcmLib name 'acmFormatDetailsW';
function acmFormatDetails;  external MsAcmLib name 'acmFormatDetailsW';

function acmFormatEnumA; external MsAcmLib name 'acmFormatEnumA';
function acmFormatEnumW; external MsAcmLib name 'acmFormatEnumW';
function acmFormatEnum;  external MsAcmLib name 'acmFormatEnumW';

function acmFormatSuggest; external MsAcmLib name 'acmFormatSuggest';

function acmFormatChooseA; external MsAcmLib name 'acmFormatChooseA';
function acmFormatChooseW; external MsAcmLib name 'acmFormatChooseW';
function acmFormatChoose;  external MsAcmLib name 'acmFormatChooseW';

function acmFilterTagDetailsA; external MsAcmLib name 'acmFilterTagDetailsA';
function acmFilterTagDetailsW; external MsAcmLib name 'acmFilterTagDetailsW';
function acmFilterTagDetails;  external MsAcmLib name 'acmFilterTagDetailsW';

function acmFilterTagEnumA; external MsAcmLib name 'acmFilterTagEnumA';
function acmFilterTagEnumW; external MsAcmLib name 'acmFilterTagEnumW';
function acmFilterTagEnum;  external MsAcmLib name 'acmFilterTagEnumW';

function acmFilterDetailsA; external MsAcmLib name 'acmFilterDetailsA';
function acmFilterDetailsW; external MsAcmLib name 'acmFilterDetailsW';
function acmFilterDetails;  external MsAcmLib name 'acmFilterDetailsW';

function acmFilterEnumA; external MsAcmLib name 'acmFilterEnumA';
function acmFilterEnumW; external MsAcmLib name 'acmFilterEnumW';
function acmFilterEnum;  external MsAcmLib name 'acmFilterEnumW';

function acmFilterChooseA; external MsAcmLib name 'acmFilterChooseA';
function acmFilterChooseW; external MsAcmLib name 'acmFilterChooseW';
function acmFilterChoose;  external MsAcmLib name 'acmFilterChooseW';

function acmStreamOpen;            external MsAcmLib name 'acmStreamOpen';
function acmStreamClose;           external MsAcmLib name 'acmStreamClose';
function acmStreamSize;            external MsAcmLib name 'acmStreamSize';
function acmStreamReset;           external MsAcmLib name 'acmStreamReset';
function acmStreamMessage;         external MsAcmLib name 'acmStreamMessage';
function acmStreamConvert;         external MsAcmLib name 'acmStreamConvert';
function acmStreamPrepareHeader;   external MsAcmLib name 'acmStreamPrepareHeader';
function acmStreamUnprepareHeader; external MsAcmLib name 'acmStreamUnprepareHeader';

  // Implement Additional Prototypes here.

end.
