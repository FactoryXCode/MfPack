// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MmDdk.pas
// Kind: Pascal / Delphi unit
// Release date: 22-05-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Include file for Multimedia Device Development Kit.
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
// Source: mmddk.h
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
unit WinApi.MmDdk;

  {$HPPEMIT '#include "mmddk.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.MMSystem;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


{$ifndef _INC_MMDDK}
{$define _INC_MMDDK}



//    If defined, the following flags inhibit inclusion
//    of the indicated items:
//
//        MMNOMIDIDEV         - MIDI support
//        MMNOWAVEDEV         - Waveform support
//        MMNOAUXDEV          - Auxiliary output support
//        MMNOMIXERDEV        - Mixer support
//        MMNOTIMERDEV        - Timer support
//        MMNOJOYDEV          - Joystick support
//        MMNOMCIDEV          - MCI support
//        MMNOTASKDEV         - Task support
//

{$ifdef MMNOTIMER}
  {$define MMNOTIMERDEV}
{$endif}
{$ifdef MMNOWAVE}
  {$define MMNOWAVEDEV}
{$endif}
{$ifdef MMNOMIDI}
  {$define MMNOMIDIDEV}
{$endif}
{$ifdef MMNOAUX}
  {$define MMNOAUXDEV}
{$endif}
{$ifdef MMNOJOY}
  {$define MMNOJOYDEV}
{$endif}
{$ifdef MMNOMMIO}
  {$define MMNOMMIODEV}
{$endif}
{$ifdef MMNOMCI}
  {$define MMNOMCIDEV}
{$endif}


///*****************************************************************************
//
//                      Helper functions for drivers
//
///*****************************************************************************

{$ifndef NODRIVERS}
const

  DRV_LOAD                            = $0001;
  {$EXTERNALSYM DRV_LOAD}
  DRV_ENABLE                          = $0002;
  {$EXTERNALSYM DRV_ENABLE}
  DRV_OPEN                            = $0003;
  {$EXTERNALSYM DRV_OPEN}
  DRV_CLOSE                           = $0004;
  {$EXTERNALSYM DRV_CLOSE}
  DRV_DISABLE                         = $0005;
  {$EXTERNALSYM DRV_DISABLE}
  DRV_FREE                            = $0006;
  {$EXTERNALSYM DRV_FREE}
  DRV_CONFIGURE                       = $0007;
  {$EXTERNALSYM DRV_CONFIGURE}
  DRV_QUERYCONFIGURE                  = $0008;
  {$EXTERNALSYM DRV_QUERYCONFIGURE}
  DRV_INSTALL                         = $0009;
  {$EXTERNALSYM DRV_INSTALL}
  DRV_REMOVE                          = $000A;
  {$EXTERNALSYM DRV_REMOVE}

  DRV_RESERVED                        = $0800;
  {$EXTERNALSYM DRV_RESERVED}
  DRV_USER                            = $4000;
  {$EXTERNALSYM DRV_USER}

  DRIVERS_SECTION                     = 'DRIVERS32';  // Section name for installed drivers
  {$EXTERNALSYM DRIVERS_SECTION}
  MCI_SECTION                         = 'MCI32';  // Section name for installed MCI drivers
  {$EXTERNALSYM MCI_SECTION}

{$endif} // NODRIVERS

  DCB_NOSWITCH                        = $0008;  // don't switch stacks for callback
  {$EXTERNALSYM DCB_NOSWITCH}
  DCB_TYPEMASK                        = $0007;  // callback type mask
  {$EXTERNALSYM DCB_TYPEMASK}
  DCB_NULL                            = $0000;  // unknown callback type
  {$EXTERNALSYM DCB_NULL}

  // flags for wFlags parameter of DriverCallback()
  DCB_WINDOW                          = $0001;  // dwCallback is a HWND
  {$EXTERNALSYM DCB_WINDOW}
  DCB_TASK                            = $0002;  // dwCallback is a HTASK
  {$EXTERNALSYM DCB_TASK}
  DCB_FUNCTION                        = $0003;  // dwCallback is a FARPROC
  {$EXTERNALSYM DCB_FUNCTION}
  DCB_EVENT                           = $0005;  // dwCallback is an EVENT
  {$EXTERNALSYM DCB_EVENT}


  // generic prototype for audio device driver entry-point functions
  // midMessage(), modMessage(), widMessage(), wodMessage(), auxMessage()
type

  PSounddevmsgproc = ^TSounddevmsgproc;
  LPSOUNDDEVMSGPROC = ^SOUNDDEVMSGPROC;
  SOUNDDEVMSGPROC = DWORD;
  {$EXTERNALSYM SOUNDDEVMSGPROC}
  TSounddevmsgproc = DWORD;
  {$EXTERNALSYM TSounddevmsgproc}

const
  DRVM_INIT                           = 100;
  {$EXTERNALSYM DRVM_INIT}
  DRVM_EXIT                           = 101;
  {$EXTERNALSYM DRVM_EXIT}
  DRVM_DISABLE                        = 102;
  {$EXTERNALSYM DRVM_DISABLE}
  DRVM_ENABLE                         = 103;
  {$EXTERNALSYM DRVM_ENABLE}
  DRVM_INIT_EX                        = 104;
  {$EXTERNALSYM DRVM_INIT_EX}


// message base for driver specific messages.
//
{$ifndef DRVM_MAPPER}
  DRVM_MAPPER                         = $2000;
  {$EXTERNALSYM DRVM_MAPPER}
{$endif}
  DRVM_USER                           = $4000;
  {$EXTERNALSYM DRVM_USER}
  DRVM_MAPPER_STATUS                  = (DRVM_MAPPER + 0);
  {$EXTERNALSYM DRVM_MAPPER_STATUS}
  DRVM_MAPPER_RECONFIGURE             = (DRVM_MAPPER + 1);
  {$EXTERNALSYM DRVM_MAPPER_RECONFIGURE}
  DRVM_MAPPER_PREFERRED_GET           = (DRVM_MAPPER + 21);
  {$EXTERNALSYM DRVM_MAPPER_PREFERRED_GET}
  DRVM_MAPPER_CONSOLEVOICECOM_GET     = (DRVM_MAPPER + 23);
  {$EXTERNALSYM DRVM_MAPPER_CONSOLEVOICECOM_GET}

  DRV_QUERYDEVNODE                    = (DRV_RESERVED + 2);
  {$EXTERNALSYM DRV_QUERYDEVNODE}
  DRV_QUERYMAPPABLE                   = (DRV_RESERVED + 5);
  {$EXTERNALSYM DRV_QUERYMAPPABLE}
  DRV_QUERYMODULE                     = (DRV_RESERVED + 9);
  {$EXTERNALSYM DRV_QUERYMODULE}
  DRV_PNPINSTALL                      = (DRV_RESERVED + 11);
  {$EXTERNALSYM DRV_PNPINSTALL}
  DRV_QUERYDEVICEINTERFACE            = (DRV_RESERVED + 12);
  {$EXTERNALSYM DRV_QUERYDEVICEINTERFACE}
  DRV_QUERYDEVICEINTERFACESIZE        = (DRV_RESERVED + 13);
  {$EXTERNALSYM DRV_QUERYDEVICEINTERFACESIZE}
  DRV_QUERYSTRINGID                   = (DRV_RESERVED + 14);
  {$EXTERNALSYM DRV_QUERYSTRINGID}
  DRV_QUERYSTRINGIDSIZE               = (DRV_RESERVED + 15);
  {$EXTERNALSYM DRV_QUERYSTRINGIDSIZE}
  DRV_QUERYIDFROMSTRINGID             = (DRV_RESERVED + 16);
  {$EXTERNALSYM DRV_QUERYIDFROMSTRINGID}
  DRV_QUERYFUNCTIONINSTANCEID         = (DRV_RESERVED + 17);
  {$EXTERNALSYM DRV_QUERYFUNCTIONINSTANCEID}
  DRV_QUERYFUNCTIONINSTANCEIDSIZE     = (DRV_RESERVED + 18);
  {$EXTERNALSYM DRV_QUERYFUNCTIONINSTANCEIDSIZE}

  //
  // DRVM_MAPPER_PREFERRED_GET flags
  //
  DRVM_MAPPER_PREFERRED_FLAGS_PREFERREDONLY = $00000001;
  {$EXTERNALSYM DRVM_MAPPER_PREFERRED_FLAGS_PREFERREDONLY}



  //
  // messages that have IOCTL format
  //    dw1 = NULL or handle
  //    dw2 = NULL or ptr to DRVM_IOCTL_DATA
  //    return is MMRESULT
  //
  DRVM_IOCTL                          = $100;
  {$EXTERNALSYM DRVM_IOCTL}
  DRVM_ADD_THRU                       = (DRVM_IOCTL + 1);
  {$EXTERNALSYM DRVM_ADD_THRU}
  DRVM_REMOVE_THRU                    = (DRVM_IOCTL + 2);
  {$EXTERNALSYM DRVM_REMOVE_THRU}
  DRVM_IOCTL_LAST                     = (DRVM_IOCTL + 5);
  {$EXTERNALSYM DRVM_IOCTL_LAST}

type

  LPDRVM_IOCTL_DATA = ^DRVM_IOCTL_DATA;
  DRVM_IOCTL_DATA = record
    dwSize: DWORD;                   // size of this structure (inclusive)
    dwCmd: DWORD;                   // IOCTL command code, 0x80000000 and above reserved for system
  end;
  {$EXTERNALSYM DRVM_IOCTL_DATA}

const
  // command code ranges for dwCmd field of DRVM_IOCTL message
  // codes from 0 to $7FFFFFFF are user defined
  // codes from $80000000 to $FFFFFFFF are reserved for future
  // definition by microsoft
  //
  DRVM_IOCTL_CMD_USER                 = $00000000;
  {$EXTERNALSYM DRVM_IOCTL_CMD_USER}
  DRVM_IOCTL_CMD_SYSTEM               = $80000000;
  {$EXTERNALSYM DRVM_IOCTL_CMD_SYSTEM}

  // device ID for 386 AUTODMA VxD
  VADMAD_Device_ID                    = $0444;
  {$EXTERNALSYM VADMAD_Device_ID}

type
  // PnP version of media device caps

  PMDEVICECAPSEX = ^MDEVICECAPSEX;
  MDEVICECAPSEX = record
    cbSize: DWORD;
    pCaps: Pointer;
  end;
  {$EXTERNALSYM MDEVICECAPSEX}

const
{$ifndef MMNOWAVEDEV}
  ///*****************************************************************************
  //
  //                     Waveform device driver support
  //
  ///*****************************************************************************

  {$EXTERNALSYM WODM_INIT}
  WODM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM WIDM_INIT}
  WIDM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM WODM_INIT_EX}
  WODM_INIT_EX                        = DRVM_INIT_EX;
  {$EXTERNALSYM WIDM_INIT_EX}
  WIDM_INIT_EX                        = DRVM_INIT_EX;

type

  // waveform input and output device open information structure
  LPWAVEOPENDESC = ^WAVEOPENDESC;
  Pwaveopendesc_tag = ^waveopendesc_tag;
  waveopendesc_tag = record
    hWave: HWAVE;                    // handle
    lpFormat: PWAVEFORMAT;           // format of wave data
    dwCallback: DWORD_PTR;           // callback
    dwInstance: DWORD_PTR;           // app's private instance information
    uMappedDeviceID: UINT;           // device to map to if WAVE_MAPPED set
    dnDevNode: DWORD_PTR;           { if device is PnP }
  end;
  {$EXTERNALSYM waveopendesc_tag}
  WAVEOPENDESC = waveopendesc_tag;
  {$EXTERNALSYM WAVEOPENDESC}


const

  // messages sent to wodMessage() entry-point function
  WODM_GETNUMDEVS                     = 3;
  {$EXTERNALSYM WODM_GETNUMDEVS}
  WODM_GETDEVCAPS                     = 4;
  {$EXTERNALSYM WODM_GETDEVCAPS}
  WODM_OPEN                           = 5;
  {$EXTERNALSYM WODM_OPEN}
  WODM_CLOSE                          = 6;
  {$EXTERNALSYM WODM_CLOSE}
  WODM_PREPARE                        = 7;
  {$EXTERNALSYM WODM_PREPARE}
  WODM_UNPREPARE                      = 8;
  {$EXTERNALSYM WODM_UNPREPARE}
  WODM_WRITE                          = 9;
  {$EXTERNALSYM WODM_WRITE}
  WODM_PAUSE                          = 10;
  {$EXTERNALSYM WODM_PAUSE}
  WODM_RESTART                        = 11;
  {$EXTERNALSYM WODM_RESTART}
  WODM_RESET                          = 12;
  {$EXTERNALSYM WODM_RESET}
  WODM_GETPOS                         = 13;
  {$EXTERNALSYM WODM_GETPOS}
  WODM_GETPITCH                       = 14;
  {$EXTERNALSYM WODM_GETPITCH}
  WODM_SETPITCH                       = 15;
  {$EXTERNALSYM WODM_SETPITCH}
  WODM_GETVOLUME                      = 16;
  {$EXTERNALSYM WODM_GETVOLUME}
  WODM_SETVOLUME                      = 17;
  {$EXTERNALSYM WODM_SETVOLUME}
  WODM_GETPLAYBACKRATE                = 18;
  {$EXTERNALSYM WODM_GETPLAYBACKRATE}
  WODM_SETPLAYBACKRATE                = 19;
  {$EXTERNALSYM WODM_SETPLAYBACKRATE}
  WODM_BREAKLOOP                      = 20;
  {$EXTERNALSYM WODM_BREAKLOOP}
  WODM_PREFERRED                      = 21;
  {$EXTERNALSYM WODM_PREFERRED}
// #if (WINVER >= 0x030B)
  WODM_MAPPER_STATUS                  = (DRVM_MAPPER_STATUS + 0);
  {$EXTERNALSYM WODM_MAPPER_STATUS}
  WAVEOUT_MAPPER_STATUS_DEVICE        = 0;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_DEVICE}
  WAVEOUT_MAPPER_STATUS_MAPPED        = 1;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_MAPPED}
  WAVEOUT_MAPPER_STATUS_FORMAT        = 2;
  {$EXTERNALSYM WAVEOUT_MAPPER_STATUS_FORMAT}
// #endif // WINVER >= 0x030B
  WODM_BUSY                           = 21;
  {$EXTERNALSYM WODM_BUSY}

  // messages sent to widMessage() entry-point function
  WIDM_GETNUMDEVS                     = 50;
  {$EXTERNALSYM WIDM_GETNUMDEVS}
  WIDM_GETDEVCAPS                     = 51;
  {$EXTERNALSYM WIDM_GETDEVCAPS}
  WIDM_OPEN                           = 52;
  {$EXTERNALSYM WIDM_OPEN}
  WIDM_CLOSE                          = 53;
  {$EXTERNALSYM WIDM_CLOSE}
  WIDM_PREPARE                        = 54;
  {$EXTERNALSYM WIDM_PREPARE}
  WIDM_UNPREPARE                      = 55;
  {$EXTERNALSYM WIDM_UNPREPARE}
  WIDM_ADDBUFFER                      = 56;
  {$EXTERNALSYM WIDM_ADDBUFFER}
  WIDM_START                          = 57;
  {$EXTERNALSYM WIDM_START}
  WIDM_STOP                           = 58;
  {$EXTERNALSYM WIDM_STOP}
  WIDM_RESET                          = 59;
  {$EXTERNALSYM WIDM_RESET}
  WIDM_GETPOS                         = 60;
  {$EXTERNALSYM WIDM_GETPOS}
  WIDM_PREFERRED                      = 61;
  {$EXTERNALSYM WIDM_PREFERRED}
// #if (WINVER >= 0x030B)
  WIDM_MAPPER_STATUS                  = (DRVM_MAPPER_STATUS + 0);
  {$EXTERNALSYM WIDM_MAPPER_STATUS}
  WAVEIN_MAPPER_STATUS_DEVICE         = 0;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_DEVICE}
  WAVEIN_MAPPER_STATUS_MAPPED         = 1;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_MAPPED}
  WAVEIN_MAPPER_STATUS_FORMAT         = 2;
  {$EXTERNALSYM WAVEIN_MAPPER_STATUS_FORMAT}
// #endif // WINVER >= 0x30B

{$endif} // ifndef MMNOWAVEDEV


{$ifndef MMNOMIDIDEV}

  ///*****************************************************************************
  //
  //                          MIDI device driver support
  //
  ///*****************************************************************************

const

  MODM_USER                           = DRVM_USER;
  {$EXTERNALSYM MODM_USER}
  MIDM_USER                           = DRVM_USER;
  {$EXTERNALSYM MIDM_USER}
  MODM_MAPPER                         = DRVM_MAPPER;
  {$EXTERNALSYM MODM_MAPPER}
  MIDM_MAPPER                         = DRVM_MAPPER;
  {$EXTERNALSYM MIDM_MAPPER}

  MODM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM MODM_INIT}
  MIDM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM MIDM_INIT}
  MODM_INIT_EX                        = DRVM_INIT_EX;
  {$EXTERNALSYM MODM_INIT_EX}
  MIDM_INIT_EX                        = DRVM_INIT_EX;
  {$EXTERNALSYM MIDM_INIT_EX}

{$ifndef MMNOMIDI}   // This protects the definition of HMIDI in WINMM.H
                     // Win 3.1 works the same way
type
  PMIDIOPENSTRMID = ^MIDIOPENSTRMID;
  PMidiopenstrmid_tag = ^midiopenstrmid_tag;
  midiopenstrmid_tag = record
    dwStreamID: DWORD;
    uDeviceID: UINT;
  end;
  {$EXTERNALSYM midiopenstrmid_tag}
  MIDIOPENSTRMID = midiopenstrmid_tag;
  {$EXTERNALSYM MIDIOPENSTRMID}

  // MIDI input and output device open information structure

  PMidiopendesc_tag = ^midiopendesc_tag;
  LPMIDIOPENDESC = ^MIDIOPENDESC;
  {$EXTERNALSYM LPMIDIOPENDESC}
  PMIDIOPENDESC = ^MIDIOPENDESC;
  midiopendesc_tag = record
    hMidi: HMIDI;                           // handle
    dwCallback: DWORD_PTR;                  // callback
    dwInstance: DWORD_PTR;                  // app's private instance information
    dnDevNode: DWORD_PTR;                   // DevNode
    cIds: DWORD;                            // If stream open, # stream ids
    rgIds: array [0..0] of MIDIOPENSTRMID;  // Array of device ID's (actually [cIds])
  end;
  {$EXTERNALSYM midiopendesc_tag}
  MIDIOPENDESC = midiopendesc_tag;
  {$EXTERNALSYM MIDIOPENDESC}
{$endif} // MMNOMIDI

const

// Flags for MODM_OPEN
  MIDI_IO_PACKED                      = $00000000;  { Compatibility mode }
  {$EXTERNALSYM MIDI_IO_PACKED}
  MIDI_IO_COOKED                      = $00000002;
  {$EXTERNALSYM MIDI_IO_COOKED}

// messages sent to modMessage() entry-point function
  MODM_GETNUMDEVS                     = 1;
  {$EXTERNALSYM MODM_GETNUMDEVS}
  MODM_GETDEVCAPS                     = 2;
  {$EXTERNALSYM MODM_GETDEVCAPS}
  MODM_OPEN                           = 3;
  {$EXTERNALSYM MODM_OPEN}
  MODM_CLOSE                          = 4;
  {$EXTERNALSYM MODM_CLOSE}
  MODM_PREPARE                        = 5;
  {$EXTERNALSYM MODM_PREPARE}
  MODM_UNPREPARE                      = 6;
  {$EXTERNALSYM MODM_UNPREPARE}
  MODM_DATA                           = 7;
  {$EXTERNALSYM MODM_DATA}
  MODM_LONGDATA                       = 8;
  {$EXTERNALSYM MODM_LONGDATA}
  MODM_RESET                          = 9;
  {$EXTERNALSYM MODM_RESET}
  MODM_GETVOLUME                      = 10;
  {$EXTERNALSYM MODM_GETVOLUME}
  MODM_SETVOLUME                      = 11;
  {$EXTERNALSYM MODM_SETVOLUME}
  MODM_CACHEPATCHES                   = 12;
  {$EXTERNALSYM MODM_CACHEPATCHES}
  MODM_CACHEDRUMPATCHES               = 13;
  {$EXTERNALSYM MODM_CACHEDRUMPATCHES}

// {$if WINVER >= 400}
  MODM_STRMDATA                       = 14;
  {$EXTERNALSYM MODM_STRMDATA}
  MODM_GETPOS                         = 17;
  {$EXTERNALSYM MODM_GETPOS}
  MODM_PAUSE                          = 18;
  {$EXTERNALSYM MODM_PAUSE}
  MODM_RESTART                        = 19;
  {$EXTERNALSYM MODM_RESTART}
  MODM_STOP                           = 20;
  {$EXTERNALSYM MODM_STOP}
  MODM_PROPERTIES                     = 21;
  {$EXTERNALSYM MODM_PROPERTIES}
  MODM_PREFERRED                      = 22;
  {$EXTERNALSYM MODM_PREFERRED}
  MODM_RECONFIGURE                    = (MODM_USER + $0768);
  {$EXTERNALSYM MODM_RECONFIGURE}
// {$endif}


// messages sent to midMessage() entry-point function
  MIDM_GETNUMDEVS                     = 53;
  {$EXTERNALSYM MIDM_GETNUMDEVS}
  MIDM_GETDEVCAPS                     = 54;
  {$EXTERNALSYM MIDM_GETDEVCAPS}
  MIDM_OPEN                           = 55;
  {$EXTERNALSYM MIDM_OPEN}
  MIDM_CLOSE                          = 56;
  {$EXTERNALSYM MIDM_CLOSE}
  MIDM_PREPARE                        = 57;
  {$EXTERNALSYM MIDM_PREPARE}
  MIDM_UNPREPARE                      = 58;
  {$EXTERNALSYM MIDM_UNPREPARE}
  MIDM_ADDBUFFER                      = 59;
  {$EXTERNALSYM MIDM_ADDBUFFER}
  MIDM_START                          = 60;
  {$EXTERNALSYM MIDM_START}
  MIDM_STOP                           = 61;
  {$EXTERNALSYM MIDM_STOP}
  MIDM_RESET                          = 62;
  {$EXTERNALSYM MIDM_RESET}

{$endif} // ifndef MMNOMIDIDEV


{$ifndef MMNOAUXDEV}
  ///*****************************************************************************
  //
  //                    Auxiliary audio device driver support
  //
  ///*****************************************************************************

  AUXM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM AUXM_INIT}
  AUXM_INIT_EX                        = DRVM_INIT_EX;
  {$EXTERNALSYM AUXM_INIT_EX}

  // messages sent to auxMessage() entry-point function
  AUXDM_GETNUMDEVS                    = 3;
  {$EXTERNALSYM AUXDM_GETNUMDEVS}
  AUXDM_GETDEVCAPS                    = 4;
  {$EXTERNALSYM AUXDM_GETDEVCAPS}
  AUXDM_GETVOLUME                     = 5;
  {$EXTERNALSYM AUXDM_GETVOLUME}
  AUXDM_SETVOLUME                     = 6;
  {$EXTERNALSYM AUXDM_SETVOLUME}

{$endif} // ifndef MMNOAUXDEV

// #if (WINVER >= 0x030B)
{$ifndef MMNOMIXERDEV}

type

  //
  //  mixer device open information structure
  //
  //

  PMIXEROPENDESC = ^_tMIXEROPENDESC;
  LPMIXEROPENDESC = ^_tMIXEROPENDESC;
  {$EXTERNALSYM LPMIXEROPENDESC}
  _tMIXEROPENDESC = record
    hmx: HMIXER;                     // handle that will be used
    pReserved0: Pointer;             // reserved--driver should ignore
    dwCallback: DWORD_PTR;           // callback
    dwInstance: DWORD_PTR;           // app's private instance information
    dnDevNode: DWORD_PTR;            // if device is PnP
  end;
  {$EXTERNALSYM _tMIXEROPENDESC}
  MIXEROPENDESC = _tMIXEROPENDESC;
  {$EXTERNALSYM MIXEROPENDESC}

const

  MXDM_INIT                           = DRVM_INIT;
  {$EXTERNALSYM MXDM_INIT}
  MXDM_INIT_EX                        = DRVM_INIT_EX;
  {$EXTERNALSYM MXDM_INIT_EX}
  MXDM_USER                           = DRV_USER;
  {$EXTERNALSYM MXDM_USER}

  MXDM_BASE                           = (1);
  {$EXTERNALSYM MXDM_BASE}
  MXDM_GETNUMDEVS                     = (MXDM_BASE + 0);
  {$EXTERNALSYM MXDM_GETNUMDEVS}
  MXDM_GETDEVCAPS                     = (MXDM_BASE + 1);
  {$EXTERNALSYM MXDM_GETDEVCAPS}
  MXDM_OPEN                           = (MXDM_BASE + 2);
  {$EXTERNALSYM MXDM_OPEN}
  MXDM_CLOSE                          = (MXDM_BASE + 3);
  {$EXTERNALSYM MXDM_CLOSE}
  MXDM_GETLINEINFO                    = (MXDM_BASE + 4);
  {$EXTERNALSYM MXDM_GETLINEINFO}
  MXDM_GETLINECONTROLS                = (MXDM_BASE + 5);
  {$EXTERNALSYM MXDM_GETLINECONTROLS}
  MXDM_GETCONTROLDETAILS              = (MXDM_BASE + 6);
  {$EXTERNALSYM MXDM_GETCONTROLDETAILS}
  MXDM_SETCONTROLDETAILS              = (MXDM_BASE + 7);
  {$EXTERNALSYM MXDM_SETCONTROLDETAILS}

{$endif} // MMNOMIXERDEV
// #endif // ifdef WINVER >= 0x030B

{$ifndef MMNOTIMERDEV}
  ///*****************************************************************************
  //
  //                        Timer device driver support
  //
  // Note : This function is obsolete.
  // New applications should use CreateTimerQueueTimer to create a timer-queue timer.
  //
  ///*****************************************************************************
  //type
  //  PTimerevent = ^TTimerevent;
  //  LPTIMEREVENT = ^TIMEREVENT;
  //  timerevent_tag = record
  //    wDelay: WORD;                    // delay required
  //    wResolution: WORD;               // resolution required
  //    lpFunction: LPTIMECALLBACK;      // ptr to callback function
  //    dwUser: DWORD;                   // user DWORD
  //    wFlags: WORD;                    // defines how to program event
  //    wReserved1: WORD;                // structure packing
  //  end;
  //  TIMEREVENT = timerevent_tag;
  //  TTimerevent = timerevent_tag;

const

  // messages sent to tddMessage() function
  TDD_KILLTIMEREVENT                  = (DRV_RESERVED + 0);   // indices into a table of
  {$EXTERNALSYM TDD_KILLTIMEREVENT}
  TDD_SETTIMEREVENT                   = (DRV_RESERVED + 4);   // functions; thus offset by
  {$EXTERNALSYM TDD_SETTIMEREVENT}
  TDD_GETSYSTEMTIME                   = (DRV_RESERVED + 8);   // four each time...
  {$EXTERNALSYM TDD_GETSYSTEMTIME}
  TDD_GETDEVCAPS                      = (DRV_RESERVED + 12);  // room for future expansion
  {$EXTERNALSYM TDD_GETDEVCAPS}
  TDD_BEGINMINPERIOD                  = (DRV_RESERVED + 16);  // room for future expansion
  {$EXTERNALSYM TDD_BEGINMINPERIOD}
  TDD_ENDMINPERIOD                    = (DRV_RESERVED + 20);  // room for future expansion
  {$EXTERNALSYM TDD_ENDMINPERIOD}

{$endif} // ifndef MMNOTIMERDEV


{$ifndef MMNOJOYDEV}
  ///*****************************************************************************
  //
  //                       Joystick device driver support
  //
  ///*****************************************************************************

  // RegisterWindowMessage with this to get msg id of config changes
  JOY_CONFIGCHANGED_MSGSTRING         = 'MSJSTICK_VJOYD_MSGSTR';
  {$EXTERNALSYM JOY_CONFIGCHANGED_MSGSTRING}

  // pre-defined joystick types
  JOY_HW_NONE                         = 0;
  {$EXTERNALSYM JOY_HW_NONE}
  JOY_HW_CUSTOM                       = 1;
  {$EXTERNALSYM JOY_HW_CUSTOM}
  JOY_HW_2A_2B_GENERIC                = 2;
  {$EXTERNALSYM JOY_HW_2A_2B_GENERIC}
  JOY_HW_2A_4B_GENERIC                = 3;
  {$EXTERNALSYM JOY_HW_2A_4B_GENERIC}
  JOY_HW_2B_GAMEPAD                   = 4;
  {$EXTERNALSYM JOY_HW_2B_GAMEPAD}
  JOY_HW_2B_FLIGHTYOKE                = 5;
  {$EXTERNALSYM JOY_HW_2B_FLIGHTYOKE}
  JOY_HW_2B_FLIGHTYOKETHROTTLE        = 6;
  {$EXTERNALSYM JOY_HW_2B_FLIGHTYOKETHROTTLE}
  JOY_HW_3A_2B_GENERIC                = 7;
  {$EXTERNALSYM JOY_HW_3A_2B_GENERIC}
  JOY_HW_3A_4B_GENERIC                = 8;
  {$EXTERNALSYM JOY_HW_3A_4B_GENERIC}
  JOY_HW_4B_GAMEPAD                   = 9;
  {$EXTERNALSYM JOY_HW_4B_GAMEPAD}
  JOY_HW_4B_FLIGHTYOKE                = 10;
  {$EXTERNALSYM JOY_HW_4B_FLIGHTYOKE}
  JOY_HW_4B_FLIGHTYOKETHROTTLE        = 11;
  {$EXTERNALSYM JOY_HW_4B_FLIGHTYOKETHROTTLE}
  JOY_HW_LASTENTRY                    = 12;
  {$EXTERNALSYM JOY_HW_LASTENTRY}

  // calibration flags
  JOY_ISCAL_XY                        = $00000001;  // XY are calibrated
  {$EXTERNALSYM JOY_ISCAL_XY}
  JOY_ISCAL_Z                         = $00000002;  // Z is calibrated
  {$EXTERNALSYM JOY_ISCAL_Z}
  JOY_ISCAL_R                         = $00000004;  // R is calibrated
  {$EXTERNALSYM JOY_ISCAL_R}
  JOY_ISCAL_U                         = $00000008;  // U is calibrated
  {$EXTERNALSYM JOY_ISCAL_U}
  JOY_ISCAL_V                         = $00000010;  // V is calibrated
  {$EXTERNALSYM JOY_ISCAL_V}
  JOY_ISCAL_POV                       = $00000020;  // POV is calibrated
  {$EXTERNALSYM JOY_ISCAL_POV}
  // point of view constants
  JOY_POV_NUMDIRS                     = 4;
  {$EXTERNALSYM JOY_POV_NUMDIRS}
  JOY_POVVAL_FORWARD                  = 0;
  {$EXTERNALSYM JOY_POVVAL_FORWARD}
  JOY_POVVAL_BACKWARD                 = 1;
  {$EXTERNALSYM JOY_POVVAL_BACKWARD}
  JOY_POVVAL_LEFT                     = 2;
  {$EXTERNALSYM JOY_POVVAL_LEFT}
  JOY_POVVAL_RIGHT                    = 3;
  {$EXTERNALSYM JOY_POVVAL_RIGHT}

  // Specific settings for joystick hardware
  JOY_HWS_HASZ                        = $00000001;  // has Z info?
  {$EXTERNALSYM JOY_HWS_HASZ}
  JOY_HWS_HASPOV                      = $00000002;  // point of view hat present
  {$EXTERNALSYM JOY_HWS_HASPOV}
  JOY_HWS_POVISBUTTONCOMBOS           = $00000004;  // pov done through combo of buttons
  {$EXTERNALSYM JOY_HWS_POVISBUTTONCOMBOS}
  JOY_HWS_POVISPOLL                   = $00000008;  // pov done through polling
  {$EXTERNALSYM JOY_HWS_POVISPOLL}
  JOY_HWS_ISYOKE                      = $00000010;  // joystick is a flight yoke
  {$EXTERNALSYM JOY_HWS_ISYOKE}
  JOY_HWS_ISGAMEPAD                   = $00000020;  // joystick is a game pad
  {$EXTERNALSYM JOY_HWS_ISGAMEPAD}
  JOY_HWS_ISCARCTRL                   = $00000040;  // joystick is a car controller
  {$EXTERNALSYM JOY_HWS_ISCARCTRL}
  // X defaults to J1 X axis
  JOY_HWS_XISJ1Y                      = $00000080;  // X is on J1 Y axis
  {$EXTERNALSYM JOY_HWS_XISJ1Y}
  JOY_HWS_XISJ2X                      = $00000100;  // X is on J2 X axis
  {$EXTERNALSYM JOY_HWS_XISJ2X}
  JOY_HWS_XISJ2Y                      = $00000200;  // X is on J2 Y axis
  {$EXTERNALSYM JOY_HWS_XISJ2Y}
  // Y defaults to J1 Y axis
  JOY_HWS_YISJ1X                      = $00000400;  // Y is on J1 X axis
  {$EXTERNALSYM JOY_HWS_YISJ1X}
  JOY_HWS_YISJ2X                      = $00000800;  // Y is on J2 X axis
  {$EXTERNALSYM JOY_HWS_YISJ2X}
  JOY_HWS_YISJ2Y                      = $00001000;  // Y is on J2 Y axis
  {$EXTERNALSYM JOY_HWS_YISJ2Y}
  // Z defaults to J2 Y axis
  JOY_HWS_ZISJ1X                      = $00002000;  // Z is on J1 X axis
  {$EXTERNALSYM JOY_HWS_ZISJ1X}
  JOY_HWS_ZISJ1Y                      = $00004000;  // Z is on J1 Y axis
  {$EXTERNALSYM JOY_HWS_ZISJ1Y}
  JOY_HWS_ZISJ2X                      = $00008000;  // Z is on J2 X axis
  {$EXTERNALSYM JOY_HWS_ZISJ2X}
  // POV defaults to J2 Y axis, if it is not button based
  JOY_HWS_POVISJ1X                    = $00010000;  // pov done through J1 X axis
  {$EXTERNALSYM JOY_HWS_POVISJ1X}
  JOY_HWS_POVISJ1Y                    = $00020000;  // pov done through J1 Y axis
  {$EXTERNALSYM JOY_HWS_POVISJ1Y}
  JOY_HWS_POVISJ2X                    = $00040000;  // pov done through J2 X axis
  {$EXTERNALSYM JOY_HWS_POVISJ2X}
  // R defaults to J2 X axis
  JOY_HWS_HASR                        = $00080000;  // has R (4th axis) info
  {$EXTERNALSYM JOY_HWS_HASR}
  JOY_HWS_RISJ1X                      = $00100000;  // R done through J1 X axis
  {$EXTERNALSYM JOY_HWS_RISJ1X}
  JOY_HWS_RISJ1Y                      = $00200000;  // R done through J1 Y axis
  {$EXTERNALSYM JOY_HWS_RISJ1Y}
  JOY_HWS_RISJ2Y                      = $00400000;  // R done through J2 X axis
  {$EXTERNALSYM JOY_HWS_RISJ2Y}
  // U & V for future hardware
  JOY_HWS_HASU                        = $00800000;  // has U (5th axis) info
  {$EXTERNALSYM JOY_HWS_HASU}
  JOY_HWS_HASV                        = $01000000;  // has V (6th axis) info
  {$EXTERNALSYM JOY_HWS_HASV}

  // Usage settings
  JOY_US_HASRUDDER                    = $00000001;  // joystick configured with rudder
  {$EXTERNALSYM JOY_US_HASRUDDER}
  JOY_US_PRESENT                      = $00000002;  // is joystick actually present?
  {$EXTERNALSYM JOY_US_PRESENT}
  JOY_US_ISOEM                        = $00000004;  // joystick is an OEM defined type
  {$EXTERNALSYM JOY_US_ISOEM}

type

  // struct for storing x, y, z, and rudder values
  PJOYPOS = ^JOYPOS;
  LPJOYPOS = ^joypos_tag;
  {$EXTERNALSYM LPJOYPOS}
  PJoypos_tag = ^joypos_tag;
  joypos_tag = record
    dwX: DWORD;
    dwY: DWORD;
    dwZ: DWORD;
    dwR: DWORD;
    dwU: DWORD;
    dwV: DWORD;
  end;
  {$EXTERNALSYM joypos_tag}
  JOYPOS = joypos_tag;
  {$EXTERNALSYM JOYPOS}

  // struct for storing ranges
  LPJOYRANGE = ^joyrange_tag;
  PJoyrange = ^TJoyrange;
  joyrange_tag = record
    jpMin: JOYPOS;
    jpMax: JOYPOS;
    jpCenter: JOYPOS;
  end;
  {$EXTERNALSYM joyrange_tag}
  JOYRANGE = joyrange_tag;
  {$EXTERNALSYM JOYRANGE}
  TJoyrange = joyrange_tag;
  {$EXTERNALSYM TJoyrange}

  PJOYREGUSERVALUES = ^JOYREGUSERVALUES;
  LPJOYREGUSERVALUES = ^joyreguservalues_tag;
  {$EXTERNALSYM LPJOYREGUSERVALUES}
  PJoyreguservalues_tag = ^joyreguservalues_tag;
  joyreguservalues_tag = record
    dwTimeOut: DWORD;                // value at which to timeout joystick polling
    jrvRanges: JOYRANGE;             // range of values app wants returned for axes
    jpDeadZone: JOYPOS;              // area around center to be considered
                                     //   as "dead". specified as a percentage
                                     //   (0-100). Only X  Y handled by system driver
  end;
  {$EXTERNALSYM joyreguservalues_tag}
  JOYREGUSERVALUES = joyreguservalues_tag;
  {$EXTERNALSYM JOYREGUSERVALUES}

  PJOYREGHWSETTINGS = ^JOYREGHWSETTINGS;
  LPJOYHWSETTINGS = ^joyreghwsettings_tag;
  {$EXTERNALSYM LPJOYHWSETTINGS}
  PJoyreghwsettings_tag = ^joyreghwsettings_tag;
  joyreghwsettings_tag = record
    dwFlags: DWORD;
    dwNumButtons: DWORD;            // number of buttons
  end;
  {$EXTERNALSYM joyreghwsettings_tag}
  JOYREGHWSETTINGS = joyreghwsettings_tag;
  {$EXTERNALSYM JOYREGHWSETTINGS}

// range of values returned by the hardware (filled in by calibration)

  LPJOYREGHWVALUES = ^joyreghwvalues_tag;
  PJoyreghwvalues = ^TJoyreghwvalues;
  joyreghwvalues_tag = record
    jrvHardware: JOYRANGE;                                 // values returned by hardware
    dwPOVValues: array[0..JOY_POV_NUMDIRS - 1] of DWORD;   // POV values returned by hardware
    dwCalFlags: DWORD;                                     // what has been calibrated
  end;
  {$EXTERNALSYM joyreghwvalues_tag}
  JOYREGHWVALUES = joyreghwvalues_tag;
  {$EXTERNALSYM JOYREGHWVALUES}
  TJoyreghwvalues = joyreghwvalues_tag;
  {$EXTERNALSYM TJoyreghwvalues}

  // hardware configuration
  PJOYREGHWCONFIG = ^JOYREGHWCONFIG;
  LPJOYREGHWCONFIG = ^joyreghwconfig_tag;
  {$EXTERNALSYM LPJOYREGHWCONFIG}
  PJoyreghwconfig_tag = ^joyreghwconfig_tag;
  joyreghwconfig_tag = record
    hws: JOYREGHWSETTINGS;           // hardware settings
    dwUsageSettings: DWORD;          // usage settings
    hwv: JOYREGHWVALUES;             // values returned by hardware
    dwType: DWORD;                   // type of joystick
    dwReserved: DWORD;               // reserved for OEM drivers
  end;
  {$EXTERNALSYM joyreghwconfig_tag}
  JOYREGHWCONFIG = joyreghwconfig_tag;
  {$EXTERNALSYM JOYREGHWCONFIG}

  // joystick calibration info structure
  PJOYCALIBRATE = ^JOYCALIBRATE;
  LPJOYCALIBRATE = ^JOYCALIBRATE;
  {$EXTERNALSYM LPJOYCALIBRATE}
  PJoycalibrate_tag = ^joycalibrate_tag;
  joycalibrate_tag = record
    wXbase: WORD;
    wXdelta: WORD;
    wYbase: WORD;
    wYdelta: WORD;
    wZbase: WORD;
    wZdelta: WORD;
  end;
  {$EXTERNALSYM joycalibrate_tag}
  JOYCALIBRATE = joycalibrate_tag;
  {$EXTERNALSYM JOYCALIBRATE}

  // prototype for joystick message function
  LPJOYDEVMSGPROC = ^JOYDEVMSGPROC;
  {$EXTERNALSYM LPJOYDEVMSGPROC}
  JOYDEVMSGPROC = DWORD;
  {$EXTERNALSYM JOYDEVMSGPROC}

const

  // messages sent to joystick driver's DriverProc() function
  JDD_GETNUMDEVS                      = (DRV_RESERVED + $0001);
  {$EXTERNALSYM JDD_GETNUMDEVS}
  JDD_GETDEVCAPS                      = (DRV_RESERVED + $0002);
  {$EXTERNALSYM JDD_GETDEVCAPS}
  JDD_GETPOS                          = (DRV_RESERVED + $0101);
  {$EXTERNALSYM JDD_GETPOS}
  JDD_SETCALIBRATION                  = (DRV_RESERVED + $0102);
  {$EXTERNALSYM JDD_SETCALIBRATION}
  JDD_CONFIGCHANGED                   = (DRV_RESERVED + $0103);
  {$EXTERNALSYM JDD_CONFIGCHANGED}
  JDD_GETPOSEX                        = (DRV_RESERVED + $0104);
  {$EXTERNALSYM JDD_GETPOSEX}

{$endif} // ifndef MMNOJOYDEV


{$ifndef MMNOMCIDEV}
  //******************************************************************************
  //
  //                        MCI device driver support
  //
  //******************************************************************************


  // internal MCI messages
  MCI_OPEN_DRIVER                     = $0801;
  {$EXTERNALSYM MCI_OPEN_DRIVER}
  MCI_CLOSE_DRIVER                    = $0802;
  {$EXTERNALSYM MCI_CLOSE_DRIVER}

  // string return values only used with MAKEMCIRESOURCE
  MCI_FALSE                           = (MCI_STRING_OFFSET + 19);
  {$EXTERNALSYM MCI_FALSE}
  MCI_TRUE                            = (MCI_STRING_OFFSET + 20);
  {$EXTERNALSYM MCI_TRUE}

  // resource string return values
  MCI_FORMAT_MILLISECONDS_S           = (MCI_STRING_OFFSET + 21);
  {$EXTERNALSYM MCI_FORMAT_MILLISECONDS_S}
  MCI_FORMAT_RETURN_BASE              = MCI_FORMAT_MILLISECONDS_S;
  {$EXTERNALSYM MCI_FORMAT_RETURN_BASE}
  MCI_FORMAT_HMS_S                    = (MCI_STRING_OFFSET + 22);
  {$EXTERNALSYM MCI_FORMAT_HMS_S}
  MCI_FORMAT_MSF_S                    = (MCI_STRING_OFFSET + 23);
  {$EXTERNALSYM MCI_FORMAT_MSF_S}
  MCI_FORMAT_FRAMES_S                 = (MCI_STRING_OFFSET + 24);
  {$EXTERNALSYM MCI_FORMAT_FRAMES_S}
  MCI_FORMAT_SMPTE_24_S               = (MCI_STRING_OFFSET + 25);
  {$EXTERNALSYM MCI_FORMAT_SMPTE_24_S}
  MCI_FORMAT_SMPTE_25_S               = (MCI_STRING_OFFSET + 26);
  {$EXTERNALSYM MCI_FORMAT_SMPTE_25_S}
  MCI_FORMAT_SMPTE_30_S               = (MCI_STRING_OFFSET + 27);
  {$EXTERNALSYM MCI_FORMAT_SMPTE_30_S}
  MCI_FORMAT_SMPTE_30DROP_S           = (MCI_STRING_OFFSET + 28);
  {$EXTERNALSYM MCI_FORMAT_SMPTE_30DROP_S}
  MCI_FORMAT_BYTES_S                  = (MCI_STRING_OFFSET + 29);
  {$EXTERNALSYM MCI_FORMAT_BYTES_S}
  MCI_FORMAT_SAMPLES_S                = (MCI_STRING_OFFSET + 30);
  {$EXTERNALSYM MCI_FORMAT_SAMPLES_S}
  MCI_FORMAT_TMSF_S                   = (MCI_STRING_OFFSET + 31);
  {$EXTERNALSYM MCI_FORMAT_TMSF_S}

  MCI_VD_FORMAT_TRACK_S               = (MCI_VD_OFFSET + 5);
  {$EXTERNALSYM MCI_VD_FORMAT_TRACK_S}

  WAVE_FORMAT_PCM_S                   = (MCI_WAVE_OFFSET + 0);
  {$EXTERNALSYM WAVE_FORMAT_PCM_S}
  WAVE_MAPPER_S                       = (MCI_WAVE_OFFSET + 1);
  {$EXTERNALSYM WAVE_MAPPER_S}

  MCI_SEQ_MAPPER_S                    = (MCI_SEQ_OFFSET + 5);
  {$EXTERNALSYM MCI_SEQ_MAPPER_S}
  MCI_SEQ_FILE_S                      = (MCI_SEQ_OFFSET + 6);
  {$EXTERNALSYM MCI_SEQ_FILE_S}
  MCI_SEQ_MIDI_S                      = (MCI_SEQ_OFFSET + 7);
  {$EXTERNALSYM MCI_SEQ_MIDI_S}
  MCI_SEQ_SMPTE_S                     = (MCI_SEQ_OFFSET + 8);
  {$EXTERNALSYM MCI_SEQ_SMPTE_S}
  MCI_SEQ_FORMAT_SONGPTR_S            = (MCI_SEQ_OFFSET + 9);
  {$EXTERNALSYM MCI_SEQ_FORMAT_SONGPTR_S}
  MCI_SEQ_NONE_S                      = (MCI_SEQ_OFFSET + 10);
  {$EXTERNALSYM MCI_SEQ_NONE_S}
  MIDIMAPPER_S                        = (MCI_SEQ_OFFSET + 11);
  {$EXTERNALSYM MIDIMAPPER_S}

  MCI_TABLE_NOT_PRESENT               = (UINT( -1));
  {$EXTERNALSYM MCI_TABLE_NOT_PRESENT}

type

  // parameters for internal version of MCI_OPEN message sent from
  // mciOpenDevice() to the driver
  PMCI_OPEN_DRIVER_PARMS = ^MCI_OPEN_DRIVER_PARMS;
  LPMCI_OPEN_DRIVER_PARMS = ^MCI_OPEN_DRIVER_PARMS;
  {$EXTERNALSYM LPMCI_OPEN_DRIVER_PARMS}
  MCI_OPEN_DRIVER_PARMS = record
    wDeviceID: MCIDEVICEID;          // device ID
    lpstrParams: PWideChar;          // parameter string for entry in SYSTEM.INI
    wCustomCommandTable: UINT;       // custom command table ((-1) if none)
                                     // filled in by the driver
    wType: UINT;                     // driver type
                                     // filled in by the driver
  end;
  {$EXTERNALSYM MCI_OPEN_DRIVER_PARMS}

const

  // maximum length of an MCI device type

  MCI_MAX_DEVICE_TYPE_LENGTH          = 80;
  {$EXTERNALSYM MCI_MAX_DEVICE_TYPE_LENGTH}

  // flags for mciSendCommandInternal() which direct mciSendString() how to
  // interpret the return value
  MCI_RESOURCE_RETURNED               = $00010000;  // resource ID
  {$EXTERNALSYM MCI_RESOURCE_RETURNED}
  MCI_COLONIZED3_RETURN               = $00020000;  // colonized ID, 3 bytes data
  {$EXTERNALSYM MCI_COLONIZED3_RETURN}
  MCI_COLONIZED4_RETURN               = $00040000;  // colonized ID, 4 bytes data
  {$EXTERNALSYM MCI_COLONIZED4_RETURN}
  MCI_INTEGER_RETURNED                = $00080000;  // integer conversion needed
  {$EXTERNALSYM MCI_INTEGER_RETURNED}
  MCI_RESOURCE_DRIVER                 = $00100000;  // driver owns returned resource
  {$EXTERNALSYM MCI_RESOURCE_DRIVER}

  // invalid command table ID
  MCI_NO_COMMAND_TABLE                = UINT(-1);

  // command table information type tags
  MCI_COMMAND_HEAD                    = 0;
  {$EXTERNALSYM MCI_COMMAND_HEAD}
  MCI_STRING                          = 1;
  {$EXTERNALSYM MCI_STRING}
  MCI_INTEGER                         = 2;
  {$EXTERNALSYM MCI_INTEGER}
  MCI_END_COMMAND                     = 3;
  {$EXTERNALSYM MCI_END_COMMAND}
  MCI_RETURN                          = 4;
  {$EXTERNALSYM MCI_RETURN}
  MCI_FLAG                            = 5;
  {$EXTERNALSYM MCI_FLAG}
  MCI_END_COMMAND_LIST                = 6;
  {$EXTERNALSYM MCI_END_COMMAND_LIST}
  MCI_RECT                            = 7;
  {$EXTERNALSYM MCI_RECT}
  MCI_CONSTANT                        = 8;
  {$EXTERNALSYM MCI_CONSTANT}
  MCI_END_CONSTANT                    = 9;
  {$EXTERNALSYM MCI_END_CONSTANT}
  MCI_HWND                            = 10;
  {$EXTERNALSYM MCI_HWND}
  MCI_HPAL                            = 11;
  {$EXTERNALSYM MCI_HPAL}
  MCI_HDC                             = 12;
  {$EXTERNALSYM MCI_HDC}

{$ifdef _WIN64}
  MCI_INTEGER64                       = 13;
  {$EXTERNALSYM MCI_INTEGER64}
{$endif} // _WIN64

{$endif} // ifndef MMNOMCIDEV


{$ifndef MMNOTASKDEV}
  //******************************************************************************
  //
  //                               Task support
  //
  //******************************************************************************

  // error return values
  TASKERR_NOTASKSUPPORT               = 1;
  {$EXTERNALSYM TASKERR_NOTASKSUPPORT}
  TASKERR_OUTOFMEMORY                 = 2;
  {$EXTERNALSYM TASKERR_OUTOFMEMORY}


//type

  // task support function prototypes
  // typedef VOID (TASKCALLBACK) (dwInst: DWORD_PTR);

  // LPTASKCALLBACK = ^TASKCALLBACK;
  // {$EXTERNALSYM LPTASKCALLBACK}

  // The mmTaskCreate function is deprecated. Applications should not use this function.
  // function mmTaskCreate(lpfn: LPTASKCALLBACK;
  //                       var lphLP: THandle;
  //                       dwInst: DWORD_PTR): UINT; stdcall; deprecated 'Applications should not use this function.';

  // The mmTaskBlock function is deprecated. Applications should not use this function.
  // procedure mmTaskBlock(h: DWORD); stdcall; deprecated 'Applications should not use this function.';

  // The mmTaskSignal function is deprecated. Applications should not use this function.
  // function mmTaskSignal(h: DWORD): BOOL; stdcall; deprecated 'Applications should not use this function.';

  // The mmTaskYield function is deprecated. Applications should not use this function.
  // procedure mmTaskYield(); stdcall; deprecated 'Applications should not use this function.';

  // The mmGetCurrentTask function is deprecated. Applications should not use this function.
  // function mmGetCurrentTask(): DWORD; stdcall; deprecated 'Applications should not use this function.';


{$endif} // endif MMNOTASKDEV

{$define MMDDKINC}

{$endif} // _INC_MMDDK


  function MAKELRESULT(dwlow: WORD;
                       dwhigh: WORD): DWORD; inline;
  {$EXTERNALSYM MAKELRESULT}

  function MAKEMCIRESOURCE(wRet: WORD;
                           wRes: WORD): WORD; inline;
  {$EXTERNALSYM MAKEMCIRESOURCE}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  MmDdkLib = 'Winmm.dll';


function MAKELRESULT(dwlow: WORD;
                     dwhigh: WORD): DWORD; inline;
begin
  Result := MAKELONG(dwlow,
                     dwhigh);
end;

function MAKEMCIRESOURCE(wRet: WORD;
                         wRes: WORD): WORD; inline;
begin
  Result := MAKELRESULT(wRet,
                        wRes);
end;

// External
// DEPRECATED! /////////////////////////////////////////////////////////////////
//  function mmTaskCreate;     external MmDdkLib name 'mmTaskCreate';
//  procedure mmTaskBlock;     external MmDdkLib name 'mmTaskBlock';
//  function mmTaskSignal;     external MmDdkLib name 'mmTaskSignal';
//  procedure mmTaskYield;     external MmDdkLib name 'mmTaskYield';
//  function mmGetCurrentTask; external MmDdkLib name 'mmGetCurrentTask';
////////////////////////////////////////////////////////////////////////////////

  // Implement Additional Prototypes here.

end.
