// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MMSysCom.pas
// Kind: Pascal / Delphi unit
// Release date: 15-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Commonm Include file for Multimedia API's
//              Version 4.00
//              For more information, see: https://docs.microsoft.com/en-us/windows/win32/api/mmsyscom/
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
// Source: mmsyscom.h
//
// Copyright (C) 1992-1998 Microsoft Corporation.  All Rights Reserved.
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
unit WinApi.MMSysCom;

interface

uses
  WinApi.Windows,
  WinApi.WinApiTypes;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


//****************************************************************************
//
//                    General constants and data types
//
//****************************************************************************

const
  //* general constants */
  MAXPNAMELEN                         = 32;  //* max product name length (including Nil) */
  {$EXTERNALSYM MAXPNAMELEN}
  MAXERRORLENGTH                      = 256;  //* max error text length (including Nil) */
  {$EXTERNALSYM MAXERRORLENGTH}
  MAX_JOYSTICKOEMVXDNAME              = 260;  //* max oem vxd name length (including Nil) */
  {$EXTERNALSYM MAX_JOYSTICKOEMVXDNAME}

  //*
  //*  Microsoft Manufacturer and Product ID's (these have been moved to
  //*  MMREG.H for Windows 4.00 and above).
  //*/

  MM_MIDI_MAPPER                      = 1;  { MIDI Mapper }
  {$EXTERNALSYM MM_MIDI_MAPPER}
  MM_WAVE_MAPPER                      = 2;  { Wave Mapper }
  {$EXTERNALSYM MM_WAVE_MAPPER}
  MM_SNDBLST_MIDIOUT                  = 3;  { Sound Blaster MIDI output port }
  {$EXTERNALSYM MM_SNDBLST_MIDIOUT}
  MM_SNDBLST_MIDIIN                   = 4;  { Sound Blaster MIDI input port }
  {$EXTERNALSYM MM_SNDBLST_MIDIIN}
  MM_SNDBLST_SYNTH                    = 5;  { Sound Blaster internal synthesizer }
  {$EXTERNALSYM MM_SNDBLST_SYNTH}
  MM_SNDBLST_WAVEOUT                  = 6;  { Sound Blaster waveform output }
  {$EXTERNALSYM MM_SNDBLST_WAVEOUT}
  MM_SNDBLST_WAVEIN                   = 7;  { Sound Blaster waveform input }
  {$EXTERNALSYM MM_SNDBLST_WAVEIN}
  MM_ADLIB                            = 9;  { Ad Lib-compatible synthesizer }
  {$EXTERNALSYM MM_ADLIB}
  MM_MPU401_MIDIOUT                   = 10;  { MPU401-compatible MIDI output port }
  {$EXTERNALSYM MM_MPU401_MIDIOUT}
  MM_MPU401_MIDIIN                    = 11;  { MPU401-compatible MIDI input port }
  {$EXTERNALSYM MM_MPU401_MIDIIN}
  MM_PC_JOYSTICK                      = 12;  { Joystick adapter }
  {$EXTERNALSYM MM_PC_JOYSTICK}


  //* general data types */
type
  VERSION = UINT;               { major (high byte), minor (low byte) }
  {$EXTERNALSYM VERSION}
  MMVERSION = UINT;             { major (high byte), minor (low byte) }
  {$EXTERNALSYM MMVERSION}
  MMRESULT = UINT;              { error return code, 0 means no error }
  {$EXTERNALSYM MMRESULT}

  //* MMTIME data structure */

  // Time format. It can be one of the following values.

  //* types for wType field in MMTIME struct */
const
  TIME_MS                             = $0001;  { time in milliseconds }
  {$EXTERNALSYM TIME_MS}
  TIME_SAMPLES                        = $0002;  { number of wave samples }
  {$EXTERNALSYM TIME_SAMPLES}
  TIME_BYTES                          = $0004;  { current byte offset }
  {$EXTERNALSYM TIME_BYTES}
  TIME_SMPTE                          = $0008;  { SMPTE time }
  {$EXTERNALSYM TIME_SMPTE}
  TIME_MIDI                           = $0010;  { MIDI time }
  {$EXTERNALSYM TIME_MIDI}
  TIME_TICKS                          = $0020;  { Ticks within MIDI stream }
  {$EXTERNALSYM TIME_TICKS}

type
  PMMTIME = ^mmtime_tag;
  {$EXTERNALSYM PMMTIME}
  mmtime_tag = record
    case wType: UINT of        { indicates the contents of the variant record }

     TIME_MS:      (ms: DWORD);      //* milliseconds */
     TIME_SAMPLES: (sample: DWORD);  //* samples */
     TIME_BYTES:   (cb: DWORD);      //* byte count */
     TIME_TICKS:   (ticks: DWORD);   //* ticks in MIDI stream */

     TIME_SMPTE: (
        hour: Byte;
        min: Byte;
        sec: Byte;
        frame: Byte;
        fps: Byte;
        dummy: Byte;
        pad: array[0..1] of Byte);
      TIME_MIDI : (songptrpos: DWORD);
  end;
  {$EXTERNALSYM mmtime_tag}
  MMTIME = mmtime_tag;
  {$EXTERNALSYM MMTIME}
  NPMMTIME = ^mmtime_tag;
  {$EXTERNALSYM NPMMTIME}
  LPMMTIME = ^mmtime_tag;
  {$EXTERNALSYM LPMMTIME}


  // This function is also defined in WinApi.MmReg.pas and WinApi.MediaFoundationApi.MfMetLib.pas
  function MAKEFOURCC(const ch0: AnsiChar;
                    const ch1: AnsiChar;
                    const ch2: AnsiChar;
                    const ch3: AnsiChar): FOURCC; inline;
  {$EXTERNALSYM MAKEFOURCC}


  //****************************************************************************
  //
  //                  Multimedia Extensions Window Messages
  //
  //****************************************************************************
const
  {$EXTERNALSYM MM_JOY1MOVE}
  MM_JOY1MOVE                         = $3A0;  { joystick }
  {$EXTERNALSYM MM_JOY2MOVE}
  MM_JOY2MOVE                         = $3A1;
  {$EXTERNALSYM MM_JOY1ZMOVE}
  MM_JOY1ZMOVE                        = $3A2;
  {$EXTERNALSYM MM_JOY2ZMOVE}
  MM_JOY2ZMOVE                        = $3A3;
  {$EXTERNALSYM MM_JOY1BUTTONDOWN}
  MM_JOY1BUTTONDOWN                   = $3B5;
  {$EXTERNALSYM MM_JOY2BUTTONDOWN}
  MM_JOY2BUTTONDOWN                   = $3B6;
  {$EXTERNALSYM MM_JOY1BUTTONUP}
  MM_JOY1BUTTONUP                     = $3B7;
  {$EXTERNALSYM MM_JOY2BUTTONUP}
  MM_JOY2BUTTONUP                     = $3B8;

  {$EXTERNALSYM MM_MCINOTIFY}
  MM_MCINOTIFY                        = $3B9;  { MCI }

  {$EXTERNALSYM MM_WOM_OPEN}
  MM_WOM_OPEN                         = $3BB;  { waveform output }
  {$EXTERNALSYM MM_WOM_CLOSE}
  MM_WOM_CLOSE                        = $3BC;
  {$EXTERNALSYM MM_WOM_DONE}
  MM_WOM_DONE                         = $3BD;

  {$EXTERNALSYM MM_WIM_OPEN}
  MM_WIM_OPEN                         = $3BE;  { waveform input }
  {$EXTERNALSYM MM_WIM_CLOSE}
  MM_WIM_CLOSE                        = $3BF;
  {$EXTERNALSYM MM_WIM_DATA}
  MM_WIM_DATA                         = $3C0;

  {$EXTERNALSYM MM_MIM_OPEN}
  MM_MIM_OPEN                         = $3C1;  { MIDI input }
  {$EXTERNALSYM MM_MIM_CLOSE}
  MM_MIM_CLOSE                        = $3C2;
  {$EXTERNALSYM MM_MIM_DATA}
  MM_MIM_DATA                         = $3C3;
  {$EXTERNALSYM MM_MIM_LONGDATA}
  MM_MIM_LONGDATA                     = $3C4;
  {$EXTERNALSYM MM_MIM_ERROR}
  MM_MIM_ERROR                        = $3C5;
  {$EXTERNALSYM MM_MIM_LONGERROR}
  MM_MIM_LONGERROR                    = $3C6;

  {$EXTERNALSYM MM_MOM_OPEN}
  MM_MOM_OPEN                         = $3C7;  { MIDI output }
  {$EXTERNALSYM MM_MOM_CLOSE}
  MM_MOM_CLOSE                        = $3C8;
  {$EXTERNALSYM MM_MOM_DONE}
  MM_MOM_DONE                         = $3C9;

  //* these are also in msvideo.h */
  {$EXTERNALSYM MM_DRVM_OPEN}
  MM_DRVM_OPEN                        = $3D0;  { installable drivers }
  {$EXTERNALSYM MM_DRVM_CLOSE}
  MM_DRVM_CLOSE                       = $3D1;
  {$EXTERNALSYM MM_DRVM_DATA}
  MM_DRVM_DATA                        = $3D2;
  {$EXTERNALSYM MM_DRVM_ERROR}
  MM_DRVM_ERROR                       = $3D3;


  //* these are used by msacm.h */
  {$EXTERNALSYM MM_STREAM_OPEN}
  MM_STREAM_OPEN                      = $3D4;
  {$EXTERNALSYM MM_STREAM_CLOSE}
  MM_STREAM_CLOSE                     = $3D5;
  {$EXTERNALSYM MM_STREAM_DONE}
  MM_STREAM_DONE                      = $3D6;
  {$EXTERNALSYM MM_STREAM_ERROR}
  MM_STREAM_ERROR                     = $3D7;


  {$EXTERNALSYM MM_MOM_POSITIONCB}
  MM_MOM_POSITIONCB                   = $3CA;  { Callback for MEVT_POSITIONCB }

  {$EXTERNALSYM MM_MCISIGNAL}
  MM_MCISIGNAL                        = $3CB;

  {$EXTERNALSYM MM_MIM_MOREDATA}
  MM_MIM_MOREDATA                     = $3CC;  { MIM_DONE w/ pending events }


  {$EXTERNALSYM MM_MIXM_LINE_CHANGE}
  MM_MIXM_LINE_CHANGE                 = $3D0;  { mixer line change notify }
  {$EXTERNALSYM MM_MIXM_CONTROL_CHANGE}
  MM_MIXM_CONTROL_CHANGE              = $3D1;  { mixer control change notify }

  //****************************************************************************
  //
  //              String resource number bases (internal use)
  //
  //****************************************************************************

  {$EXTERNALSYM MMSYSERR_BASE}
  MMSYSERR_BASE                       = 0;
  {$EXTERNALSYM WAVERR_BASE}
  WAVERR_BASE                         = 32;
  {$EXTERNALSYM MIDIERR_BASE}
  MIDIERR_BASE                        = 64;
  {$EXTERNALSYM TIMERR_BASE}
  TIMERR_BASE                         = 96;
  {$EXTERNALSYM JOYERR_BASE}
  JOYERR_BASE                         = 160;
  {$EXTERNALSYM MCIERR_BASE}
  MCIERR_BASE                         = 256;
  {$EXTERNALSYM MIXERR_BASE}
  MIXERR_BASE                         = 1024;

  {$EXTERNALSYM MCI_STRING_OFFSET}
  MCI_STRING_OFFSET                   = 512;
  {$EXTERNALSYM MCI_VD_OFFSET}
  MCI_VD_OFFSET                       = 1024;
  {$EXTERNALSYM MCI_CD_OFFSET}
  MCI_CD_OFFSET                       = 1088;
  {$EXTERNALSYM MCI_WAVE_OFFSET}
  MCI_WAVE_OFFSET                     = 1152;
  {$EXTERNALSYM MCI_SEQ_OFFSET}
  MCI_SEQ_OFFSET                      = 1216;

  //****************************************************************************
  //
  //                     General error return values
  //
  //****************************************************************************

  //* general error return values */
  {$EXTERNALSYM MMSYSERR_NOERROR}
  MMSYSERR_NOERROR                    = 0;  { no error }
  {$EXTERNALSYM MMSYSERR_ERROR}
  MMSYSERR_ERROR                      = (MMSYSERR_BASE + 1);  { unspecified error }
  {$EXTERNALSYM MMSYSERR_BADDEVICEID}
  MMSYSERR_BADDEVICEID                = (MMSYSERR_BASE + 2);  { device ID out of range }
  {$EXTERNALSYM MMSYSERR_NOTENABLED}
  MMSYSERR_NOTENABLED                 = (MMSYSERR_BASE + 3);  { driver failed enable }
  {$EXTERNALSYM MMSYSERR_ALLOCATED}
  MMSYSERR_ALLOCATED                  = (MMSYSERR_BASE + 4);  { device already allocated }
  {$EXTERNALSYM MMSYSERR_INVALHANDLE}
  MMSYSERR_INVALHANDLE                = (MMSYSERR_BASE + 5);  { device handle is invalid }
  {$EXTERNALSYM MMSYSERR_NODRIVER}
  MMSYSERR_NODRIVER                   = (MMSYSERR_BASE + 6);  { no device driver present }
  {$EXTERNALSYM MMSYSERR_NOMEM}
  MMSYSERR_NOMEM                      = (MMSYSERR_BASE + 7);  { memory allocation error }
  {$EXTERNALSYM MMSYSERR_NOTSUPPORTED}
  MMSYSERR_NOTSUPPORTED               = (MMSYSERR_BASE + 8);  { function isn't supported }
  {$EXTERNALSYM MMSYSERR_BADERRNUM}
  MMSYSERR_BADERRNUM                  = (MMSYSERR_BASE + 9);  { error value out of range }
  {$EXTERNALSYM MMSYSERR_INVALFLAG}
  MMSYSERR_INVALFLAG                  = (MMSYSERR_BASE + 10);  { invalid flag passed }
  {$EXTERNALSYM MMSYSERR_INVALPARAM}
  MMSYSERR_INVALPARAM                 = (MMSYSERR_BASE + 11);  { invalid parameter passed }
  {$EXTERNALSYM MMSYSERR_HANDLEBUSY}
  MMSYSERR_HANDLEBUSY                 = (MMSYSERR_BASE + 12);  { handle being used }
                                                               { simultaneously on another }
                                                               { thread (eg callback) }
  {$EXTERNALSYM MMSYSERR_INVALIDALIAS}
  MMSYSERR_INVALIDALIAS               = (MMSYSERR_BASE + 13);  { specified alias not found }
  {$EXTERNALSYM MMSYSERR_BADDB}
  MMSYSERR_BADDB                      = (MMSYSERR_BASE + 14);  { bad registry database }
  {$EXTERNALSYM MMSYSERR_KEYNOTFOUND}
  MMSYSERR_KEYNOTFOUND                = (MMSYSERR_BASE + 15);  { registry key not found }
  {$EXTERNALSYM MMSYSERR_READERROR}
  MMSYSERR_READERROR                  = (MMSYSERR_BASE + 16);  { registry read error }
  {$EXTERNALSYM MMSYSERR_WRITEERROR}
  MMSYSERR_WRITEERROR                 = (MMSYSERR_BASE + 17);  { registry write error }
  {$EXTERNALSYM MMSYSERR_DELETEERROR}
  MMSYSERR_DELETEERROR                = (MMSYSERR_BASE + 18);  { registry delete error }
  {$EXTERNALSYM MMSYSERR_VALNOTFOUND}
  MMSYSERR_VALNOTFOUND                = (MMSYSERR_BASE + 19);  { registry value not found }
  {$EXTERNALSYM MMSYSERR_NODRIVERCB}
  MMSYSERR_NODRIVERCB                 = (MMSYSERR_BASE + 20);  { driver does not call DriverCallback }
  {$EXTERNALSYM MMSYSERR_MOREDATA}
  MMSYSERR_MOREDATA                   = (MMSYSERR_BASE + 21);  { more data to be returned }
  {$EXTERNALSYM MMSYSERR_LASTERROR}
  MMSYSERR_LASTERROR                  = (MMSYSERR_BASE + 21);  { last error in range }

type
  HDRVR = IntPtr;
  {$EXTERNALSYM HDRVR}


  //****************************************************************************
  //
  //                        Driver callback support
  //
  //****************************************************************************

  //* flags used with waveOutOpen(), waveInOpen(), midiInOpen(), and */
  //* midiOutOpen() to specify the type of the dwCallback parameter. */

const

  {$EXTERNALSYM CALLBACK_TYPEMASK}
  CALLBACK_TYPEMASK                   = $00070000;  { callback type mask }
  {$EXTERNALSYM CALLBACK_NULL}
  CALLBACK_NULL                       = $00000000;  { no callback }
  {$EXTERNALSYM CALLBACK_WINDOW}
  CALLBACK_WINDOW                     = $00010000;  { dwCallback is a HWND }
  {$EXTERNALSYM CALLBACK_TASK}
  CALLBACK_TASK                       = $00020000;  { dwCallback is a HTASK }
  {$EXTERNALSYM CALLBACK_FUNCTION}
  CALLBACK_FUNCTION                   = $00030000;  { dwCallback is a FARPROC }

  {$EXTERNALSYM CALLBACK_THREAD}
  CALLBACK_THREAD                     = (CALLBACK_TASK);  { thread ID replaces 16 bit task }
  {$EXTERNALSYM CALLBACK_EVENT}
  CALLBACK_EVENT                      = $00050000;  { dwCallback is an EVENT Handle }

type
  DRVCALLBACK = procedure(hdrvr: HDRVR;
                          uMsg: UINT;
                          dwUser: DWORD_PTR;
                          dw1: DWORD_PTR;
                          dw2: DWORD_PTR);
  {$EXTERNALSYM DRVCALLBACK}
  LPDRVCALLBACK = ^DRVCALLBACK;
  {$EXTERNALSYM LPDRVCALLBACK}
  PDRVCALLBACK = ^DRVCALLBACK;
  {$EXTERNALSYM PDRVCALLBACK}

  // Additional Prototypes for ALL Interfaces

  // End of Additional Prototypes

implementation


function MAKEFOURCC(const ch0: AnsiChar;
                    const ch1: AnsiChar;
                    const ch2: AnsiChar;
                    const ch3: AnsiChar): FOURCC; inline;
begin
  Result:= DWORD(Ord(ch0)) OR
           (DWORD(Ord(ch1)) shl 8) OR
           (DWORD(Ord(ch2)) shl 16) OR
           (DWORD(Ord(ch3)) shl 24);
end;

  // Implement Additional functions here.

end.
