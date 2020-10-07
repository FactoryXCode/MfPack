// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MMeApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-mme-l1-1-0
//              Part of Windows Multimedia
//              See: https://docs.microsoft.com/en-us/windows/win32/api/_multimedia/
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
// Source: mmeapi.h
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
unit WinApi.WinMM.MMeApi;

interface
uses
  WinApi.Windows,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMiscApi2,
  WinApi.WinMM.PlaySoundApi;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}



  //****************************************************************************
  //
  //                       Waveform audio support
  //
  //****************************************************************************

  { waveform audio error return values }

const

  {$EXTERNALSYM WAVERR_BADFORMAT}
  WAVERR_BADFORMAT                    = (WAVERR_BASE + 0);  { unsupported wave format }
  {$EXTERNALSYM WAVERR_STILLPLAYING}
  WAVERR_STILLPLAYING                 = (WAVERR_BASE + 1);  { still something playing }
  {$EXTERNALSYM WAVERR_UNPREPARED}
  WAVERR_UNPREPARED                   = (WAVERR_BASE + 2);  { header not prepared }
  {$EXTERNALSYM WAVERR_SYNC}
  WAVERR_SYNC                         = (WAVERR_BASE + 3);  { device is synchronous }
  {$EXTERNALSYM WAVERR_LASTERROR}
  WAVERR_LASTERROR                    = (WAVERR_BASE + 3);  { last error in range }

  { waveform audio data types }

type

  PHWAVE = ^HWAVE;
  HWAVE = IntPtr;
  {$EXTERNALSYM HWAVE}

  PHWAVEIN = ^HWAVEIN;
  HWAVEIN = IntPtr;
  {$EXTERNALSYM HWAVEIN}
  LPHWAVEIN = ^HWAVEIN;
  {$EXTERNALSYM LPHWAVEIN}

  PHWAVEOUT = ^HWAVEOUT;
  HWAVEOUT = IntPtr;
  {$EXTERNALSYM HWAVEOUT}
  LPHWAVEOUT = ^HWAVEOUT;
  {$EXTERNALSYM LPHWAVEOUT}

  PWAVECALLBACK = ^WAVECALLBACK;
  WAVECALLBACK = DRVCALLBACK;
  {$EXTERNALSYM WAVECALLBACK}
  LPWAVECALLBACK = ^WAVECALLBACK;
  {$EXTERNALSYM LPWAVECALLBACK}


  { wave callback messages }

const

  WOM_OPEN                            = MM_WOM_OPEN;
  {$EXTERNALSYM WOM_OPEN}
  WOM_CLOSE                           = MM_WOM_CLOSE;
  {$EXTERNALSYM WOM_CLOSE}
  WOM_DONE                            = MM_WOM_DONE;
  {$EXTERNALSYM WOM_DONE}
  WIM_OPEN                            = MM_WIM_OPEN;
  {$EXTERNALSYM WIM_OPEN}
  WIM_CLOSE                           = MM_WIM_CLOSE;
  {$EXTERNALSYM WIM_CLOSE}
  WIM_DATA                            = MM_WIM_DATA;
  {$EXTERNALSYM WIM_DATA}

  { device ID for wave device mapper }
  {$EXTERNALSYM WAVE_MAPPER}
  WAVE_MAPPER                         = UINT(-1);

  { flags for dwFlags parameter in waveOutOpen() and waveInOpen() }
  {$EXTERNALSYM WAVE_FORMAT_QUERY}
  WAVE_FORMAT_QUERY                   = $0001;
  {$EXTERNALSYM WAVE_ALLOWSYNC}
  WAVE_ALLOWSYNC                      = $0002;

  {$EXTERNALSYM WAVE_MAPPED}
  WAVE_MAPPED                         = $0004;
  {$EXTERNALSYM WAVE_FORMAT_DIRECT}
  WAVE_FORMAT_DIRECT                  = $0008;
  {$EXTERNALSYM WAVE_FORMAT_DIRECT_QUERY}
  WAVE_FORMAT_DIRECT_QUERY            = (WAVE_FORMAT_QUERY or WAVE_FORMAT_DIRECT);
  {$EXTERNALSYM WAVE_MAPPED_DEFAULT_COMMUNICATION_DEVICE}
  WAVE_MAPPED_DEFAULT_COMMUNICATION_DEVICE= $0010;


  { wave data block header }

type

  PWAVEHDR = ^wavehdr_tag;
  wavehdr_tag = record
    lpData: PAnsiChar;              { pointer to locked data buffer }
    dwBufferLength: DWORD;          { length of data buffer }
    dwBytesRecorded: DWORD;         { used for input only }
    dwUser: DWORD_PTR;              { for client's use }
    dwFlags: DWORD;                 { assorted flags (see defines) }
    dwLoops: DWORD;                 { loop control counter }
    lpNext: PWAVEHDR;               { reserved for driver }
    reserved: DWORD_PTR;            { reserved for driver }
  end;
  {$EXTERNALSYM wavehdr_tag}
  WAVEHDR = wavehdr_tag;
  {$EXTERNALSYM WAVEHDR}
  LPWAVEHDR = ^wavehdr_tag;
  {$EXTERNALSYM LPWAVEHDR}


  { flags for dwFlags field of WAVEHDR }

const

  WHDR_DONE                           = $00000001;  { done bit }
  {$EXTERNALSYM WHDR_DONE}
  WHDR_PREPARED                       = $00000002;  { set if this header has been prepared }
  {$EXTERNALSYM WHDR_PREPARED}
  WHDR_BEGINLOOP                      = $00000004;  { loop start block }
  {$EXTERNALSYM WHDR_BEGINLOOP}
  WHDR_ENDLOOP                        = $00000008;  { loop end block }
  {$EXTERNALSYM WHDR_ENDLOOP}
  WHDR_INQUEUE                        = $00000010;  { reserved for driver }
  {$EXTERNALSYM WHDR_INQUEUE}


  { waveform output device capabilities structure }

type

  PWAVEOUTCAPSA = ^tagWAVEOUTCAPSA;
  {$EXTERNALSYM PWAVEOUTCAPSA}
  tagWAVEOUTCAPSA = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of sources supported }
    wReserved1: WORD;                                { packing }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagWAVEOUTCAPSA}
  WAVEOUTCAPSA = tagWAVEOUTCAPSA;
  {$EXTERNALSYM WAVEOUTCAPSA}
  NPWAVEOUTCAPSA = ^tagWAVEOUTCAPSA;
  {$EXTERNALSYM NPWAVEOUTCAPSA}
  LPWAVEOUTCAPSA = ^tagWAVEOUTCAPSA;
  {$EXTERNALSYM LPWAVEOUTCAPSA}

  { UniCode }
  PWAVEOUTCAPSW = ^tagWAVEOUTCAPSW;
  {$EXTERNALSYM PWAVEOUTCAPSW}
  tagWAVEOUTCAPSW = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of sources supported }
    wReserved1: WORD;                                { packing }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagWAVEOUTCAPSW}
  WAVEOUTCAPSW = tagWAVEOUTCAPSW;
  {$EXTERNALSYM WAVEOUTCAPSW}
  NPWAVEOUTCAPSW = ^tagWAVEOUTCAPSW;
  {$EXTERNALSYM NPWAVEOUTCAPSW}
  LPWAVEOUTCAPSW = ^tagWAVEOUTCAPSW;
  {$EXTERNALSYM LPWAVEOUTCAPSW}

  { Delphi }
  WAVEOUTCAPS = tagWAVEOUTCAPSW;
  NPWAVEOUTCAPS = ^tagWAVEOUTCAPSW;
  LPWAVEOUTCAPS = ^tagWAVEOUTCAPSW;


  PWAVEOUTCAPS2A = ^tagWAVEOUTCAPS2A;
  {$EXTERNALSYM PWAVEOUTCAPS2A}
  tagWAVEOUTCAPS2A = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of sources supported }
    wReserved1: WORD;                                { packing }
    dwSupport: DWORD;                                { functionality supported by driver }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagWAVEOUTCAPS2A}
  WAVEOUTCAPS2A = tagWAVEOUTCAPS2A;
  {$EXTERNALSYM WAVEOUTCAPS2A}
  NPWAVEOUTCAPS2A = ^tagWAVEOUTCAPS2A;
  {$EXTERNALSYM NPWAVEOUTCAPS2A}
  LPWAVEOUTCAPS2A = ^tagWAVEOUTCAPS2A;
  {$EXTERNALSYM LPWAVEOUTCAPS2A}

  { UniCode }
  PWAVEOUTCAPS2W = ^tagWAVEOUTCAPS2W;
  {$EXTERNALSYM PWAVEOUTCAPS2W}
  tagWAVEOUTCAPS2W = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of sources supported }
    wReserved1: WORD;                                { packing }
    dwSupport: DWORD;                                { functionality supported by driver }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagWAVEOUTCAPS2W}
  WAVEOUTCAPS2W = tagWAVEOUTCAPS2W;
  {$EXTERNALSYM WAVEOUTCAPS2W}
  NPWAVEOUTCAPS2W = ^tagWAVEOUTCAPS2W;
  {$EXTERNALSYM NPWAVEOUTCAPS2W}
  LPWAVEOUTCAPS2W = ^tagWAVEOUTCAPS2W;
  {$EXTERNALSYM LPWAVEOUTCAPS2W}

  { Delphi }
  WAVEOUTCAPS2 = tagWAVEOUTCAPS2W;
  PWAVEOUTCAPS2 = ^tagWAVEOUTCAPS2W;
  LPWAVEOUTCAPS2 = ^tagWAVEOUTCAPS2W;


  { flags for dwSupport field of WAVEOUTCAPS }
const

  {$EXTERNALSYM WAVECAPS_PITCH}
  WAVECAPS_PITCH                      = $0001;  { supports pitch control }
  {$EXTERNALSYM WAVECAPS_PLAYBACKRATE}
  WAVECAPS_PLAYBACKRATE               = $0002;  { supports playback rate control }
  {$EXTERNALSYM WAVECAPS_VOLUME}
  WAVECAPS_VOLUME                     = $0004;  { supports volume control }
  {$EXTERNALSYM WAVECAPS_LRVOLUME}
  WAVECAPS_LRVOLUME                   = $0008;  { separate left-right volume control }
  {$EXTERNALSYM WAVECAPS_SYNC}
  WAVECAPS_SYNC                       = $0010;
  {$EXTERNALSYM WAVECAPS_SAMPLEACCURATE}
  WAVECAPS_SAMPLEACCURATE             = $0020;


  { waveform input device capabilities structure }

type

  PWAVEINCAPSA = ^tagWAVEINCAPSA;
  {$EXTERNALSYM PWAVEINCAPSA}
  tagWAVEINCAPSA = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name (NULL terminated string) }
    dwFormats: DWORD;                                 { formats supported }
    wChannels: WORD;                                  { number of channels supported }
    wReserved1: WORD;                                { structure packing }
  end;
  {$EXTERNALSYM tagWAVEINCAPSA}
  WAVEINCAPSA = tagWAVEINCAPSA;
  {$EXTERNALSYM WAVEINCAPSA}
  NPWAVEINCAPSA = ^tagWAVEINCAPSA;
  {$EXTERNALSYM NPWAVEINCAPSA}
  LPWAVEINCAPSA = ^tagWAVEINCAPSA;
  {$EXTERNALSYM LPWAVEINCAPSA}

  { UniCode }
  PWAVEINCAPSW = ^tagWAVEINCAPSW;
  {$EXTERNALSYM PWAVEINCAPSW}
  tagWAVEINCAPSW = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;   { product name (NULL terminated string) }
    dwFormats: DWORD;                                 { formats supported }
    wChannels: WORD;                                  { number of channels supported }
    wReserved1: WORD;                                { structure packing }
  end;
  {$EXTERNALSYM tagWAVEINCAPSW}
  WAVEINCAPSW = tagWAVEINCAPSW;
  {$EXTERNALSYM WAVEINCAPSW}
  NPWAVEINCAPSW = ^tagWAVEINCAPSW;
  {$EXTERNALSYM NPWAVEINCAPSW}
  LPWAVEINCAPSW = ^tagWAVEINCAPSW;
  {$EXTERNALSYM LPWAVEINCAPSW}

  { Delphi }
  WAVEINCAPS = tagWAVEINCAPSW;
  NPWAVEINCAPS = ^tagWAVEINCAPSW;
  LPWAVEINCAPS = ^tagWAVEINCAPSW;


  PWAVEINCAPS2A = ^tagWAVEINCAPS2A;
  {$EXTERNALSYM PWAVEINCAPS2A}
  tagWAVEINCAPS2A = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of channels supported }
    wReserved1: WORD;                                { structure packing }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagWAVEINCAPS2A}
  WAVEINCAPS2A = tagWAVEINCAPS2A;
  {$EXTERNALSYM WAVEINCAPS2A}
  NPWAVEINCAPS2A = ^tagWAVEINCAPS2A;
  {$EXTERNALSYM NPWAVEINCAPS2A}
  LPWAVEINCAPS2A = ^tagWAVEINCAPS2A;
  {$EXTERNALSYM LPWAVEINCAPS2A}

  { UniCode }
  PWAVEINCAPS2W = ^tagWAVEINCAPS2W;
  {$EXTERNALSYM PWAVEINCAPS2W}
  tagWAVEINCAPS2W = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    dwFormats: DWORD;                                { formats supported }
    wChannels: WORD;                                 { number of channels supported }
    wReserved1: WORD;                                { structure packing }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagWAVEINCAPS2W}
  WAVEINCAPS2W = tagWAVEINCAPS2W;
  {$EXTERNALSYM WAVEINCAPS2W}
  NPWAVEINCAPS2W = ^tagWAVEINCAPS2W;
  {$EXTERNALSYM NPWAVEINCAPS2W}
  LPWAVEINCAPS2W = ^tagWAVEINCAPS2W;
  {$EXTERNALSYM LPWAVEINCAPS2W}

  { Delphi }
  WAVEINCAPS2 = tagWAVEINCAPS2W;
  PWAVEINCAPS2 = ^tagWAVEINCAPS2W;
  LPWAVEINCAPS2 = ^tagWAVEINCAPS2W;


  { defines for dwFormat field of WAVEINCAPS and WAVEOUTCAPS }

const

  WAVE_INVALIDFORMAT                  = $00000000;  { invalid format }
  {$EXTERNALSYM WAVE_INVALIDFORMAT}
  WAVE_FORMAT_1M08                    = $00000001;  { 11.025 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_1M08}
  WAVE_FORMAT_1S08                    = $00000002;  { 11.025 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_1S08}
  WAVE_FORMAT_1M16                    = $00000004;  { 11.025 kHz, Mono, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_1M16}
  WAVE_FORMAT_1S16                    = $00000008;  { 11.025 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_1S16}
  WAVE_FORMAT_2M08                    = $00000010;  { 22.05 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_2M08}
  WAVE_FORMAT_2S08                    = $00000020;  { 22.05 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_2S08}
  WAVE_FORMAT_2M16                    = $00000040;  { 22.05 kHz, Mono, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_2M16}
  WAVE_FORMAT_2S16                    = $00000080;  { 22.05 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_2S16}
  WAVE_FORMAT_4M08                    = $00000100;  { 44.1 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_4M08}
  WAVE_FORMAT_4S08                    = $00000200;  { 44.1 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_4S08}
  WAVE_FORMAT_4M16                    = $00000400;  { 44.1 kHz, Mono, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_4M16}
  WAVE_FORMAT_4S16                    = $00000800;  { 44.1 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_4S16}

  WAVE_FORMAT_44M08                   = $00000100;  { 44.1 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_44M08}
  WAVE_FORMAT_44S08                   = $00000200;  { 44.1 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_44S08}
  WAVE_FORMAT_44M16                   = $00000400;  { 44.1 kHz, Mono, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_44M16}
  WAVE_FORMAT_44S16                   = $00000800;  { 44.1 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_44S16}
  WAVE_FORMAT_48M08                   = $00001000;  { 48 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_48M08}
  WAVE_FORMAT_48S08                   = $00002000;  { 48 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_48S08}
  WAVE_FORMAT_48M16                   = $00004000;  { 48 kHz, Mono, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_48M16}
  WAVE_FORMAT_48S16                   = $00008000;  { 48 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_48S16}
  WAVE_FORMAT_96M08                   = $00010000;  { 96 kHz, Mono, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_96M08}
  WAVE_FORMAT_96S08                   = $00020000;  { 96 kHz, Stereo, 8-bit }
  {$EXTERNALSYM WAVE_FORMAT_96S08}
  WAVE_FORMAT_96M16                   = $00040000;  { 96 kHz, Mono,   16-bit }
  {$EXTERNALSYM WAVE_FORMAT_96M16}
  WAVE_FORMAT_96S16                   = $00080000;  { 96 kHz, Stereo, 16-bit }
  {$EXTERNALSYM WAVE_FORMAT_96S16}


  { OLD general waveform format structure (information common to all formats) }

type
  // left here for backward compatability
  PWAVEFORMAT_OLD = ^waveformat_tag_OLD;
  waveformat_tag_OLD = record
    wFormatTag: WORD;               { format type }
    nChannels: WORD;                { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD;          { sample rate }
    nAvgBytesPerSec: DWORD;         { for buffer estimation }
    nBlockAlign: WORD;              { block size of data }
  end;
  WAVEFORMAT_OLD = waveformat_tag_OLD;
  LPWAVEFORMAT_OLD = ^waveformat_tag_OLD;

  { flags for wFormatTag field of WAVEFORMAT }

const

  WAVE_FORMAT_PCM                     = 1;
  {$EXTERNALSYM WAVE_FORMAT_PCM}


  { specific waveform format structure for PCM data }

type
  // Allows the managed application programming interface (API) to have access to the
  // unmanaged portions of the Microsoft DirectX API.
  // This is not intended to be used directly from your code.
  PPCMWAVEFORMAT = ^pcmwaveformat_tag;
  {$EXTERNALSYM PPCMWAVEFORMAT}
  pcmwaveformat_tag = record
    wf: WAVEFORMAT_OLD;
    wBitsPerSample: WORD;
  end;
  {$EXTERNALSYM pcmwaveformat_tag}
  PCMWAVEFORMAT = pcmwaveformat_tag;
  {$EXTERNALSYM PCMWAVEFORMAT}
  LPPCMWAVEFORMAT = ^pcmwaveformat_tag;
  {$EXTERNALSYM LPPCMWAVEFORMAT}



 {
 *  extended waveform format structure used for all non-PCM formats. this
 *  structure is common to all non-PCM formats.
 // NOTE: This structure is also defined in MMReg
 }

{$IFNDEF _WAVEFORMATEX_DEFINED}
type

  PWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM PWAVEFORMATEX}
  tWAVEFORMATEX = record
    wFormatTag: WORD;               { format type }
    nChannels: WORD;                { number of channels (i.e. mono, stereo...) }
    nSamplesPerSec: DWORD;          { sample rate }
    nAvgBytesPerSec: DWORD;         { for buffer estimation }
    nBlockAlign: WORD;              { block size of data }
    wBitsPerSample: WORD;           { number of bits per sample of mono data }
    cbSize: WORD;                   { the count in bytes of the size of }
                                    { extra information (after cbSize) }
  end;
  {$EXTERNALSYM tWAVEFORMATEX}
  WAVEFORMATEX = tWAVEFORMATEX;
  {$EXTERNALSYM WAVEFORMATEX}
  NPWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM NPWAVEFORMATEX}
  LPWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM LPWAVEFORMATEX}
  LPCWAVEFORMATEX = ^WAVEFORMATEX;
  {$EXTERNALSYM LPCWAVEFORMATEX}
{$DEFINE _WAVEFORMATEX_DEFINED}
{$ENDIF} // _WAVEFORMATEX_

  { waveform audio function prototypes }

  function waveOutGetNumDevs(): UINT; stdcall;
  {$EXTERNALSYM waveOutGetNumDevs}


  function waveOutGetDevCapsA(uDeviceID: UINT_PTR;
                              out pwoc: LPWAVEOUTCAPSA;
                              cbwoc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetDevCapsA}

  function waveOutGetDevCapsW(uDeviceID: UINT_PTR;
                              out pwoc: LPWAVEOUTCAPSW;
                              cbwoc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetDevCapsW}

  { Delphi }
  function waveOutGetDevCaps(uDeviceID: UINT_PTR;
                             out pwoc: LPWAVEOUTCAPSW;
                             cbwoc: UINT): MMRESULT; stdcall;

  function waveOutGetVolume({_In_opt_} hwo: HWAVEOUT;
                            out pdwVolume: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetVolume}

  function waveOutSetVolume({_In_opt_} hwo: HWAVEOUT;
                            dwVolume: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutSetVolume}

  function waveOutGetErrorTextA(mmrError: MMRESULT;
                                out pszText: PAnsiChar;
                                cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetErrorTextA}

  { UniCode }
  function waveOutGetErrorTextW(mmrError: MMRESULT;
                                {out} pszText: PWideChar;
                                cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetErrorTextW}

  { Delphi }
  function waveOutGetErrorText(mmrError: MMRESULT;
                               pszText: PWideChar;
                               cchText: UINT): MMRESULT; stdcall;

  function waveOutOpen({outopt_} phwo: LPHWAVEOUT;
                       uDeviceID: UINT;
                       pwfx: LPCWAVEFORMATEX;
                       {_In_opt_} dwCallback: DWORD_PTR;
                       {_In_opt_} dwInstance: DWORD_PTR;
                       fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutOpen}

  function waveOutClose(hwo: HWAVEOUT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutClose}


  function waveOutPrepareHeader(hwo: HWAVEOUT;
                                {_Inout_} pwh: LPWAVEHDR;
                                cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutPrepareHeader}

  function waveOutUnprepareHeader(hwo: HWAVEOUT;
                                  {_Inout_} pwh: LPWAVEHDR;
                                  cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutUnprepareHeader}

  function waveOutWrite(hwo: HWAVEOUT;
                        {_Inout_} pwh: LPWAVEHDR;
                        cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutWrite}

  function waveOutPause(hwo: HWAVEOUT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutPause}

  function waveOutRestart(hwo: HWAVEOUT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutRestart}

  function waveOutReset(hwo: HWAVEOUT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutReset}

  function waveOutBreakLoop(hwo: HWAVEOUT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutBreakLoop}

  function waveOutGetPosition(hwo: HWAVEOUT;
                              {_Inout_} pmmt: LPMMTIME;
                              cbmmt: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetPosition}

  function waveOutGetPitch(hwo: HWAVEOUT;
                           out pdwPitch: PDWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetPitch}

  function waveOutSetPitch(hwo: HWAVEOUT;
                           dwPitch: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutSetPitch}

  function waveOutGetPlaybackRate(hwo: HWAVEOUT;
                                  {out} pdwRate: PDWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetPlaybackRate}

  function waveOutSetPlaybackRate(hwo: HWAVEOUT;
                                  dwRate: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutSetPlaybackRate}

  function waveOutGetID(hwo: HWAVEOUT;
                        {out} puDeviceID: PUINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutGetID}

  function waveOutMessage({_In_opt_} hwo: HWAVEOUT;
                          uMsg: UINT;
                          dw1: DWORD_PTR;
                          dw2: DWORD_PTR): MMRESULT; stdcall;
  {$EXTERNALSYM waveOutMessage}

  function waveInGetNumDevs(): UINT; stdcall;
  {$EXTERNALSYM waveInGetNumDevs}


  function waveInGetDevCapsA(uDeviceID: UINT_PTR;
                             {out} pwic: LPWAVEINCAPSA;
                             cbwic: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetDevCapsA}

  function waveInGetDevCapsW(uDeviceID: UINT_PTR;
                             {out} pwic: LPWAVEINCAPSW;
                             cbwic: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetDevCapsW}

  { Delphi }
  function waveInGetDevCaps(uDeviceID: UINT_PTR;
                            {out} pwic: LPWAVEINCAPSW;
                            cbwic: UINT): MMRESULT; stdcall;


  function waveInGetErrorTextA(mmrError: MMRESULT;
                               {out} pszText: PAnsiChar;
                               cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetErrorTextA}

  function waveInGetErrorTextW(mmrError: MMRESULT;
                               {out} pszText: PWideChar;
                               cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetErrorTextW}

  { Delphi }
  function waveInGetErrorText(mmrError: MMRESULT;
                              {out} pszText: PWideChar;
                              cchText: UINT): MMRESULT; stdcall;


  function waveInOpen({outopt_} phwi: LPHWAVEIN;
                      uDeviceID: UINT;
                      pwfx: LPCWAVEFORMATEX;
                      {_In_opt_} dwCallback: DWORD_PTR;
                      {_In_opt_} dwInstance: DWORD_PTR;
                      fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM waveInOpen}

  function waveInClose(hwi: HWAVEIN): MMRESULT; stdcall;
  {$EXTERNALSYM waveInClose}

  function waveInPrepareHeader(hwi: HWAVEIN;
                               {_Inout_} pwh: LPWAVEHDR;
                               cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInPrepareHeader}

  function waveInUnprepareHeader(hwi: HWAVEIN;
                                 {_Inout_} pwh: LPWAVEHDR;
                                 cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInUnprepareHeader}

  function waveInAddBuffer(hwi: HWAVEIN;
                           {_Inout_} pwh: LPWAVEHDR;
                           cbwh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInAddBuffer}

  function waveInStart(hwi: HWAVEIN): MMRESULT; stdcall;
  {$EXTERNALSYM waveInStart}

  function waveInStop(hwi: HWAVEIN): MMRESULT; stdcall;
  {$EXTERNALSYM waveInStop}

  function waveInReset(hwi: HWAVEIN): MMRESULT; stdcall;
  {$EXTERNALSYM waveInReset}

  function waveInGetPosition(hwi: HWAVEIN;
                             {_Inout_} pmmt: LPMMTIME;
                             cbmmt: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetPosition}

  function waveInGetID(hwi: HWAVEIN;
                       puDeviceID: PUINT): MMRESULT; stdcall;
  {$EXTERNALSYM waveInGetID}

  function waveInMessage({_In_opt_} const hwi: HWAVEIN;
                         uMsg: UINT;
                         {_In_opt_} dw1: DWORD_PTR;
                         {_In_opt_} dw2: DWORD_PTR): MMRESULT; stdcall;
  {$EXTERNALSYM waveInMessage}

  //****************************************************************************
  //
  //                        MIDI audio support
  //
  //****************************************************************************

  { MIDI error return values }

const

  {$EXTERNALSYM MIDIERR_UNPREPARED}
  MIDIERR_UNPREPARED                  = (MIDIERR_BASE + 0);  { header not prepared }
  {$EXTERNALSYM MIDIERR_STILLPLAYING}
  MIDIERR_STILLPLAYING                = (MIDIERR_BASE + 1);  { still something playing }
  {$EXTERNALSYM MIDIERR_NOMAP}
  MIDIERR_NOMAP                       = (MIDIERR_BASE + 2);  { no configured instruments }
  {$EXTERNALSYM MIDIERR_NOTREADY}
  MIDIERR_NOTREADY                    = (MIDIERR_BASE + 3);  { hardware is still busy }
  {$EXTERNALSYM MIDIERR_NODEVICE}
  MIDIERR_NODEVICE                    = (MIDIERR_BASE + 4);  { port no longer connected }
  {$EXTERNALSYM MIDIERR_INVALIDSETUP}
  MIDIERR_INVALIDSETUP                = (MIDIERR_BASE + 5);  { invalid MIF }
  {$EXTERNALSYM MIDIERR_BADOPENMODE}
  MIDIERR_BADOPENMODE                 = (MIDIERR_BASE + 6);  { operation unsupported w/ open mode }
  {$EXTERNALSYM MIDIERR_DONT_CONTINUE}
  MIDIERR_DONT_CONTINUE               = (MIDIERR_BASE + 7);  { thru device 'eating' a message }
  {$EXTERNALSYM MIDIERR_LASTERROR}
  MIDIERR_LASTERROR                   = (MIDIERR_BASE + 7);  { last error in range }

  { MIDI audio data types }

type
  PHMIDI = ^HMIDI;
  HMIDI = IntPtr;
  {$EXTERNALSYM HMIDI}
  LPHMIDI = ^HMIDI;
  {$EXTERNALSYM LPHMIDI}

  PHMIDIIN = ^HMIDIIN;
  HMIDIIN = IntPtr;
  {$EXTERNALSYM HMIDIIN}
  LPHMIDIIN = ^HMIDIIN;
  {$EXTERNALSYM LPHMIDIIN}

  PHMIDIOUT = ^HMIDIOUT;
  HMIDIOUT = IntPtr;
  {$EXTERNALSYM HMIDIOUT}
  LPHMIDIOUT = ^HMIDIOUT;
  {$EXTERNALSYM LPHMIDIOUT}

  PHMIDISTRM = ^HMIDISTRM;
  HMIDISTRM = IntPtr;
  {$EXTERNALSYM HMIDISTRM}
  LPHMIDISTRM = ^HMIDISTRM;
  {$EXTERNALSYM LPHMIDI}

  MIDICALLBACK = DRVCALLBACK;
  {$EXTERNALSYM MIDICALLBACK}
  LPMIDICALLBACK = ^MIDICALLBACK;
  {$EXTERNALSYM LPMIDICALLBACK}

const

  MIDIPATCHSIZE                       = 128;
  {$EXTERNALSYM MIDIPATCHSIZE}

type

  PATCHARRAY = array[0..MIDIPATCHSIZE - 1] of WORD;
  {$EXTERNALSYM PATCHARRAY}
  LPPATCHARRAY = ^WORD;
  {$EXTERNALSYM LPPATCHARRAY}
  KEYARRAY = array[0..MIDIPATCHSIZE - 1] of WORD;
  {$EXTERNALSYM KEYARRAY}
  LPKEYARRAY = ^WORD;
  {$EXTERNALSYM LPKEYARRAY}

  { MIDI callback messages }

const

  MIM_OPEN                            = MM_MIM_OPEN;
  {$EXTERNALSYM MIM_OPEN}
  MIM_CLOSE                           = MM_MIM_CLOSE;
  {$EXTERNALSYM MIM_CLOSE}
  MIM_DATA                            = MM_MIM_DATA;
  {$EXTERNALSYM MIM_DATA}
  MIM_LONGDATA                        = MM_MIM_LONGDATA;
  {$EXTERNALSYM MIM_LONGDATA}
  MIM_ERROR                           = MM_MIM_ERROR;
  {$EXTERNALSYM MIM_ERROR}
  MIM_LONGERROR                       = MM_MIM_LONGERROR;
  {$EXTERNALSYM MIM_LONGERROR}
  MOM_OPEN                            = MM_MOM_OPEN;
  {$EXTERNALSYM MOM_OPEN}
  MOM_CLOSE                           = MM_MOM_CLOSE;
  {$EXTERNALSYM MOM_CLOSE}
  MOM_DONE                            = MM_MOM_DONE;
  {$EXTERNALSYM MOM_DONE}


  MIM_MOREDATA                        = MM_MIM_MOREDATA;
  {$EXTERNALSYM MIM_MOREDATA}
  MOM_POSITIONCB                      = MM_MOM_POSITIONCB;
  {$EXTERNALSYM MOM_POSITIONCB}


  { device ID for MIDI mapper }
  MIDIMAPPER                          = UINT(-1);
  {$EXTERNALSYM MIDIMAPPER}
  MIDI_MAPPER                         = UINT(-1);
  {$EXTERNALSYM MIDI_MAPPER}


  { flags for dwFlags parm of midiInOpen() }
  MIDI_IO_STATUS                      = $00000020;
  {$EXTERNALSYM MIDI_IO_STATUS}

  { flags for wFlags parm of midiOutCachePatches(), midiOutCacheDrumPatches() }

  MIDI_CACHE_ALL                      = 1;
  {$EXTERNALSYM MIDI_CACHE_ALL}
  MIDI_CACHE_BESTFIT                  = 2;
  {$EXTERNALSYM MIDI_CACHE_BESTFIT}
  MIDI_CACHE_QUERY                    = 3;
  {$EXTERNALSYM MIDI_CACHE_QUERY}
  MIDI_UNCACHE                        = 4;
  {$EXTERNALSYM MIDI_UNCACHE}


  { MIDI output device capabilities structure }

type

  PMIDIOUTCAPSA = ^tagMIDIOUTCAPSA;
  {$EXTERNALSYM PMIDIOUTCAPSA}
  tagMIDIOUTCAPSA = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    wTechnology: WORD;                               { type of device }
    wVoices: WORD;                                   { # of voices (internal synth only) }
    wNotes: WORD;                                    { max # of notes (internal synth only) }
    wChannelMask: WORD;                              { channels used (internal synth only) }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagMIDIOUTCAPSA}
  MIDIOUTCAPSA = tagMIDIOUTCAPSA;
  {$EXTERNALSYM MIDIOUTCAPSA}
  NPMIDIOUTCAPSA = ^tagMIDIOUTCAPSA;
  {$EXTERNALSYM NPMIDIOUTCAPSA}
  LPMIDIOUTCAPSA = ^tagMIDIOUTCAPSA;
  {$EXTERNALSYM LPMIDIOUTCAPSA}

  { UniCode }
  PMIDIOUTCAPSW = ^tagMIDIOUTCAPSW;
  {$EXTERNALSYM PMIDIOUTCAPSW}
  tagMIDIOUTCAPSW = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    wTechnology: WORD;                               { type of device }
    wVoices: WORD;                                   { # of voices (internal synth only) }
    wNotes: WORD;                                    { max # of notes (internal synth only) }
    wChannelMask: WORD;                              { channels used (internal synth only) }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagMIDIOUTCAPSW}
  MIDIOUTCAPSW = tagMIDIOUTCAPSW;
  {$EXTERNALSYM MIDIOUTCAPSW}
  NPMIDIOUTCAPSW = ^tagMIDIOUTCAPSW;
  {$EXTERNALSYM NPMIDIOUTCAPSW}
  LPMIDIOUTCAPSW = ^tagMIDIOUTCAPSW;
  {$EXTERNALSYM LPMIDIOUTCAPSW}

  { Delphi }
  MIDIOUTCAPS = tagMIDIOUTCAPSW;
  NPMIDIOUTCAPS = ^tagMIDIOUTCAPSW;
  LPMIDIOUTCAPW = ^tagMIDIOUTCAPSW;


  PMIDIOUTCAPS2A = ^tagMIDIOUTCAPS2A;
  {$EXTERNALSYM PMIDIOUTCAPS2A}
  tagMIDIOUTCAPS2A = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name (NULL terminated string) }
    wTechnology: WORD;                                { type of device }
    wVoices: WORD;                                    { # of voices (internal synth only) }
    wNotes: WORD;                                     { max # of notes (internal synth only) }
    wChannelMask: WORD;                               { channels used (internal synth only) }
    dwSupport: DWORD;                                 { functionality supported by driver }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIDIOUTCAPS2A}
  MIDIOUTCAPS2A = tagMIDIOUTCAPS2A;
  {$EXTERNALSYM MIDIOUTCAPS2A}
  NPMIDIOUTCAPS2A = ^tagMIDIOUTCAPS2A;
  {$EXTERNALSYM NPMIDIOUTCAPS2A}
  LPMIDIOUTCAPS2A = ^tagMIDIOUTCAPS2A;
  {$EXTERNALSYM LPMIDIOUTCAPS2A}

  PMIDIOUTCAPS2W = ^tagMIDIOUTCAPS2W;
  {$EXTERNALSYM PMIDIOUTCAPS2W}
  tagMIDIOUTCAPS2W = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;   { product name (NULL terminated string) }
    wTechnology: WORD;                                { type of device }
    wVoices: WORD;                                    { # of voices (internal synth only) }
    wNotes: WORD;                                     { max # of notes (internal synth only) }
    wChannelMask: WORD;                               { channels used (internal synth only) }
    dwSupport: DWORD;                                 { functionality supported by driver }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIDIOUTCAPS2W}
  MIDIOUTCAPS2W = tagMIDIOUTCAPS2W;
  {$EXTERNALSYM MIDIOUTCAPS2W}
  NPMIDIOUTCAPS2W = ^tagMIDIOUTCAPS2W;
  {$EXTERNALSYM NPMIDIOUTCAPS2W}
  LPMIDIOUTCAPS2W = ^tagMIDIOUTCAPS2W;
  {$EXTERNALSYM LPMIDIOUTCAPS2W}

  { Delphi }
  MIDIOUTCAPS2 = tagMIDIOUTCAPS2W;
  PMIDIOUTCAPS2 = ^tagMIDIOUTCAPS2W;
  LPMIDIOUTCAPS2 = ^tagMIDIOUTCAPS2W;


  { flags for wTechnology field of MIDIOUTCAPS structure }

const

  {$EXTERNALSYM MOD_MIDIPORT}
  MOD_MIDIPORT                        = 1;  { output port }
  {$EXTERNALSYM MOD_SYNTH}
  MOD_SYNTH                           = 2;  { generic internal synth }
  {$EXTERNALSYM MOD_SQSYNTH}
  MOD_SQSYNTH                         = 3;  { square wave internal synth }
  {$EXTERNALSYM MOD_FMSYNTH}
  MOD_FMSYNTH                         = 4;  { FM internal synth }
  {$EXTERNALSYM MOD_MAPPER}
  MOD_MAPPER                          = 5;  { MIDI mapper }
  {$EXTERNALSYM MOD_WAVETABLE}
  MOD_WAVETABLE                       = 6;  { hardware wavetable synth }
  {$EXTERNALSYM MOD_SWSYNTH}
  MOD_SWSYNTH                         = 7;  { software synth }

  { flags for dwSupport field of MIDIOUTCAPS structure }
  {$EXTERNALSYM MIDICAPS_VOLUME}
  MIDICAPS_VOLUME                     = $0001;  { supports volume control }
  {$EXTERNALSYM MIDICAPS_LRVOLUME}
  MIDICAPS_LRVOLUME                   = $0002;  { separate left-right volume control }
  {$EXTERNALSYM MIDICAPS_CACHE}
  MIDICAPS_CACHE                      = $0004;
  {$EXTERNALSYM MIDICAPS_STREAM}
  MIDICAPS_STREAM                     = $0008;  { driver supports midiStreamOut directly }


  { MIDI input device capabilities structure }

type

  PMIDIINCAPSA = ^tagMIDIINCAPSA;
  {$EXTERNALSYM PMIDIINCAPSA}
  tagMIDIINCAPSA = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagMIDIINCAPSA}
  MIDIINCAPSA = tagMIDIINCAPSA;
  {$EXTERNALSYM MIDIINCAPSA}
  NPMIDIINCAPSA = ^tagMIDIINCAPSA;
  {$EXTERNALSYM NPMIDIINCAPSA}
  LPMIDIINCAPSA = ^tagMIDIINCAPSA;
  {$EXTERNALSYM LPMIDIINCAPSA}

  { UniCode }
  PMIDIINCAPSW = ^tagMIDIINCAPSW;
  {$EXTERNALSYM PMIDIINCAPSW}
  tagMIDIINCAPSW = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagMIDIINCAPSW}
  MIDIINCAPSW = tagMIDIINCAPSW;
  {$EXTERNALSYM MIDIINCAPSW}
  NPMIDIINCAPSW = ^tagMIDIINCAPSW;
  {$EXTERNALSYM NPMIDIINCAPSW}
  LPMIDIINCAPSW = ^tagMIDIINCAPSW;
  {$EXTERNALSYM LPMIDIINCAPSW}

  { Delphi }
  MIDIINCAPS = tagMIDIINCAPSW;
  NPMIDIINCAPS = ^tagMIDIINCAPSW;
  LPMIDIINCAPS = ^tagMIDIINCAPSW;


  PMIDIINCAPS2A = ^tagMIDIINCAPS2A;
  {$EXTERNALSYM PMIDIINCAPS2A}
  tagMIDIINCAPS2A = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;  { product name (NULL terminated string) }
    dwSupport: DWORD;                                { functionality supported by driver }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIDIINCAPS2A}
  MIDIINCAPS2A = tagMIDIINCAPS2A;
  {$EXTERNALSYM MIDIINCAPS2A}
  NPMIDIINCAPS2A = ^tagMIDIINCAPS2A;
  {$EXTERNALSYM NPMIDIINCAPS2A}
  LPMIDIINCAPS2A = ^tagMIDIINCAPS2A;
  {$EXTERNALSYM LPMIDIINCAPS2A}

  PMIDIINCAPS2W = ^tagMIDIINCAPS2W;
  {$EXTERNALSYM PMIDIINCAPS2W}
  tagMIDIINCAPS2W = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    dwSupport: DWORD;                                { functionality supported by driver }
    ManufacturerGuid: TGUID;                         { for extensible MID mapping }
    ProductGuid: TGUID;                              { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIDIINCAPS2W}
  MIDIINCAPS2W = tagMIDIINCAPS2W;
  {$EXTERNALSYM MIDIINCAPS2W}
  NPMIDIINCAPS2W = ^tagMIDIINCAPS2W;
  {$EXTERNALSYM NPMIDIINCAPS2W}
  LPMIDIINCAPS2W = ^tagMIDIINCAPS2W;
  {$EXTERNALSYM LPMIDIINCAPS2W}

  { Delphi }
  MIDIINCAPS2 = tagMIDIINCAPS2W;
  NPMIDIINCAPS2 = ^tagMIDIINCAPS2W;
  LPMIDIINCAPS2 = ^tagMIDIINCAPS2W;


  { MIDI data block header }

  PMIDIHDR = ^midihdr_tag;
  {$EXTERNALSYM PMIDIHDR}
  midihdr_tag = record
    lpData: PAnsiChar;                      { pointer to locked data block }
    dwBufferLength: DWORD;                  { length of data in data block }
    dwBytesRecorded: DWORD;                 { used for input only }
    dwUser: DWORD_PTR;                      { for client's use }
    dwFlags: DWORD;                         { assorted flags (see defines) }
    lpNext: PMIDIHDR;                       { reserved for driver }
    reserved: DWORD_PTR;                    { reserved for driver }
    dwOffset: DWORD;                        { Callback offset into buffer }
    dwReserved: array[0..7] of DWORD_PTR;  { Reserved for MMSYSTEM }
  end;
  {$EXTERNALSYM midihdr_tag}
  MIDIHDR = midihdr_tag;
  {$EXTERNALSYM MIDIHDR}
  LPMIDIHDR = ^midihdr_tag;
  {$EXTERNALSYM LPMIDIHDR}

  PMIDIEVENT = ^midievent_tag;
  midievent_tag = record
    dwDeltaTime: DWORD;             { Ticks since last event }
    dwStreamID: DWORD;              { Reserved; must be zero }
    dwEvent: DWORD;                 { Event type and parameters }
    dwParms: array[0..0] of DWORD;  { Parameters if this is a long event }
  end;
  {$EXTERNALSYM midievent_tag}
  MIDIEVENT = midievent_tag;
  {$EXTERNALSYM MIDIEVENT}

  PMIDISTRMBUFFVER = ^midistrmbuffver_tag;
  midistrmbuffver_tag = record
    dwVersion: DWORD;                { Stream buffer format version }
    dwMid: DWORD;                    { Manufacturer ID as defined in MMREG.H }
    dwOEMVersion: DWORD;            { Manufacturer version for custom ext }
  end;
  {$EXTERNALSYM midistrmbuffver_tag}
  MIDISTRMBUFFVER = midistrmbuffver_tag;
  {$EXTERNALSYM MIDISTRMBUFFVER}


  { flags for dwFlags field of MIDIHDR structure }

const

  {$EXTERNALSYM MHDR_DONE}
  MHDR_DONE                           = $00000001;  { done bit }
  {$EXTERNALSYM MHDR_PREPARED}
  MHDR_PREPARED                       = $00000002;  { set if header prepared }
  {$EXTERNALSYM MHDR_INQUEUE}
  MHDR_INQUEUE                        = $00000004;  { reserved for driver }
  {$EXTERNALSYM MHDR_ISSTRM}
  MHDR_ISSTRM                         = $00000008;  { Buffer is stream buffer }


  // Type codes which go in the high byte of the event DWORD of a stream buffer
  //
  // Type codes 00-7F contain parameters within the low 24 bits
  // Type codes 80-FF contain a length of their parameter in the low 24
  // bits, followed by their parameter data in the buffer. The event
  // DWORD contains the exact byte length; the parm data itself must be
  // padded to be an even multiple of 4 bytes long.

const

  {$EXTERNALSYM MEVT_F_SHORT}
  MEVT_F_SHORT                        = $00000000;
  {$EXTERNALSYM MEVT_F_LONG}
  MEVT_F_LONG                         = $80000000;
  {$EXTERNALSYM MEVT_F_CALLBACK}
  MEVT_F_CALLBACK                     = $40000000;

  { Macro's }
  function MEVT_EVENTTYPE(x: DWORD): DWORD; inline;
  {$EXTERNALSYM MEVT_EVENTTYPE}

  function MEVT_EVENTPARM(x: DWORD): DWORD; inline;
  {$EXTERNALSYM MEVT_EVENTPARM}


const

  MEVT_SHORTMSG                       = $00;  { parm = shortmsg for midiOutShortMsg }
  {$EXTERNALSYM MEVT_SHORTMSG}
  MEVT_TEMPO                          = $01;  { parm = new tempo in microsec/qn     }
  {$EXTERNALSYM MEVT_TEMPO}
  MEVT_NOP                            = $02;  { parm = unused; does nothing         }
  {$EXTERNALSYM MEVT_NOP}

  { 0x04-0x7F reserved }

  MEVT_LONGMSG                        = $80;  { parm = bytes to send verbatim       }
  {$EXTERNALSYM MEVT_LONGMSG}
  MEVT_COMMENT                        = $82;  { parm = comment data                 }
  {$EXTERNALSYM MEVT_COMMENT}
  MEVT_VERSION                        = $84;  { parm = MIDISTRMBUFFVER struct       }
  {$EXTERNALSYM MEVT_VERSION}

  { 0x81-0xFF reserved }

  MIDISTRM_ERROR                      = (- 2);
  {$EXTERNALSYM MIDISTRM_ERROR}


  { Structures and defines for midiStreamProperty }

  MIDIPROP_SET                        = $80000000;
  {$EXTERNALSYM MIDIPROP_SET}
  MIDIPROP_GET                        = $40000000;
  {$EXTERNALSYM MIDIPROP_GET}

  { These are intentionally both non-zero so the app cannot accidentally }
  { leave the operation off and happen to appear to work due to default }
  { action. }

  MIDIPROP_TIMEDIV                    = $00000001;
  {$EXTERNALSYM MIDIPROP_TIMEDIV}
  MIDIPROP_TEMPO                      = $00000002;
  {$EXTERNALSYM MIDIPROP_TEMPO}

type

  PMIDIPROPTIMEDIV = ^midiproptimediv_tag;
  midiproptimediv_tag = record
    cbStruct: DWORD;
    dwTimeDiv: DWORD;
  end;
  {$EXTERNALSYM midiproptimediv_tag}
  MIDIPROPTIMEDIV = midiproptimediv_tag;
  {$EXTERNALSYM MIDIPROPTIMEDIV}
  LPMIDIPROPTIMEDIV = ^midiproptimediv_tag;
  {$EXTERNALSYM LPMIDIPROPTIMEDIV}

  PMIDIPROPTEMPO = ^midiproptempo_tag;
  midiproptempo_tag = record
    cbStruct: DWORD;
    dwTempo: DWORD;
  end;
  {$EXTERNALSYM midiproptempo_tag}
  MIDIPROPTEMPO = midiproptempo_tag;
  {$EXTERNALSYM MIDIPROPTEMPO}
  LPMIDIPROPTEMPO = ^midiproptempo_tag;
  {$EXTERNALSYM LPMIDIPROPTEMPO}


  { MIDI function prototypes }

  function midiOutGetNumDevs(): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetNumDevs}

  function midiStreamOpen(phms: LPHMIDISTRM;
                          puDeviceID: PUINT;
                          cMidi: DWORD;
                          {_In_opt_} dwCallback: DWORD_PTR;
                          {_In_opt_} dwInstance: DWORD_PTR;
                          fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamOpen}

  function midiStreamClose(hms: HMIDISTRM): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamClose}

  function midiStreamProperty(hms: HMIDISTRM;
                              lppropdata: PByte;
                              dwProperty: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamProperty}

  function midiStreamPosition(hms: HMIDISTRM;
                              lpmmt: LPMMTIME;
                              cbmmt: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamPosition}

  function midiStreamOut(hms: HMIDISTRM;
                         {out} pmh: LPMIDIHDR;
                         cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamOut}

  function midiStreamPause(hms: HMIDISTRM): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamPause}

  function midiStreamRestart(hms: HMIDISTRM): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamRestart}

  function midiStreamStop(hms: HMIDISTRM): MMRESULT; stdcall;
  {$EXTERNALSYM midiStreamStop}


  function midiConnect(hmi: HMIDI;
                       hmo: HMIDIOUT;
                       {_In_opt_} pReserved: Pointer): MMRESULT; stdcall;
  {$EXTERNALSYM midiConnect}

  function midiDisconnect(hmi: HMIDI;
                          hmo: HMIDIOUT;
                          {_In_opt_} pReserved: Pointer): MMRESULT; stdcall;
  {$EXTERNALSYM midiDisconnect}


  function midiOutGetDevCapsA(uDeviceID: UINT_PTR;
                              {out} pmoc: LPMIDIOUTCAPSA;
                              cbmoc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetDevCapsA}

  function midiOutGetDevCapsW(uDeviceID: UINT_PTR;
                              {out} pmoc: LPMIDIOUTCAPSW;
                              cbmoc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetDevCapsW}

  function midiOutGetDevCaps(uDeviceID: UINT_PTR;
                              {out} pmoc: LPMIDIOUTCAPSW;
                              cbmoc: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetDevCaps}


  function midiOutGetVolume({_In_opt_} hmo: HMIDIOUT;
                            {out} pdwVolume: PDWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetVolume}

  function midiOutSetVolume({_In_opt_} hmo: HMIDIOUT;
                            dwVolume: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutSetVolume}


  function midiOutGetErrorTextA(mmrError: MMRESULT;
                                {out} pszText: PAnsiChar;
                                cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetErrorTextA}

  function midiOutGetErrorTextW(mmrError: MMRESULT;
                                {out} pszText: PWideChar;
                                cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetErrorTextW}

  function midiOutGetErrorText(mmrError: MMRESULT;
                               {out} pszText: PWideChar;
                               cchText: UINT): MMRESULT; stdcall;


  function midiOutOpen({out} phmo: LPHMIDIOUT;
                       uDeviceID: UINT;
                       {_In_opt_} dwCallback: DWORD_PTR;
                       {_In_opt_} dwInstance: DWORD_PTR;
                       fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutOpen}

  function midiOutClose(hmo: HMIDIOUT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutClose}

  function midiOutPrepareHeader(hmo: HMIDIOUT;
                                {_Inout_} pmh: LPMIDIHDR;
                                cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutPrepareHeader}

  function midiOutUnprepareHeader(hmo: HMIDIOUT;
                                  {_Inout_} pmh: LPMIDIHDR;
                                  cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutUnprepareHeader}

  function midiOutShortMsg(hmo: HMIDIOUT;
                           dwMsg: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutShortMsg}

  function midiOutLongMsg(hmo: HMIDIOUT;
                          pmh: LPMIDIHDR;
                          cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutLongMsg}

  function midiOutReset(hmo: HMIDIOUT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutReset}

  function midiOutCachePatches(hmo: HMIDIOUT;
                               uBank: UINT;
                               pwpa: PWORD;
                               fuCache: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutCachePatches}

  function midiOutCacheDrumPatches(hmo: HMIDIOUT;
                                   uPatch: UINT;
                                   pwkya: PWORD;
                                   fuCache: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutCacheDrumPatches}

  function midiOutGetID(hmo: HMIDIOUT;
                        {out} puDeviceID: PUINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutGetID}

  function midiOutMessage({_In_opt_} hmo: HMIDIOUT;
                          uMsg: UINT;
                          {_In_opt_} dw1: DWORD_PTR;
                          {_In_opt_} dw2: DWORD_PTR): MMRESULT; stdcall;
  {$EXTERNALSYM midiOutMessage}

  function midiInGetNumDevs(): UINT; stdcall;
  {$EXTERNALSYM midiInGetNumDevs}

  function midiInGetDevCapsA(uDeviceID: UINT_PTR;
                             {out} pmic: LPMIDIINCAPSA;
                             cbmic: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetDevCapsA}

  function midiInGetDevCapsW(uDeviceID: UINT_PTR;
                             {out} pmic: LPMIDIINCAPSW;
                             cbmic: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetDevCapsW}

  { Delphi }
  function midiInGetDevCaps(uDeviceID: UINT_PTR;
                            {out} pmic: LPMIDIINCAPSW;
                            cbmic: UINT): MMRESULT; stdcall;


  function midiInGetErrorTextA(mmrError: MMRESULT;
                               {out} pszText: PAnsiChar;
                               cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetErrorTextA}

  function midiInGetErrorTextW(mmrError: MMRESULT;
                               {out} pszText: PWideChar;
                               cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetErrorTextW}

  { Delphi }
  function midiInGetErrorText(mmrError: MMRESULT;
                              {out} pszText: PWideChar;
                              cchText: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetErrorText}


  function midiInOpen({out} phmi: LPHMIDIIN;
                      uDeviceID: UINT;
                      {_In_opt_} dwCallback: DWORD_PTR;
                      {_In_opt_} dwInstance: DWORD_PTR;
                      fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM midiInOpen}

  function midiInClose(hmi: HMIDIIN): MMRESULT; stdcall;
  {$EXTERNALSYM midiInClose}

  function midiInPrepareHeader(hmi: HMIDIIN;
                               {_Inout_} pmh: LPMIDIHDR;
                               cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInPrepareHeader}

  function midiInUnprepareHeader(hmi: HMIDIIN;
                                 {_Inout_} pmh: LPMIDIHDR;
                                 cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInUnprepareHeader}

  function midiInAddBuffer(hmi: HMIDIIN;
                           {out} pmh: LPMIDIHDR;
                           cbmh: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInAddBuffer}

  function midiInStart(hmi: HMIDIIN): MMRESULT; stdcall;
  {$EXTERNALSYM midiInStart}

  function midiInStop(hmi: HMIDIIN): MMRESULT; stdcall;
  {$EXTERNALSYM midiInStop}

  function midiInReset(hmi: HMIDIIN): MMRESULT; stdcall;
  {$EXTERNALSYM midiInReset}

  function midiInGetID(hmi: HMIDIIN;
                       {out} puDeviceID: PUINT): MMRESULT; stdcall;
  {$EXTERNALSYM midiInGetID}

  function midiInMessage({_In_opt_} hmi: HMIDIIN;
                         uMsg: UINT;
                         {_In_opt_} dw1: DWORD_PTR;
                         {_In_opt_} dw2: DWORD_PTR): MMRESULT; stdcall;
  {$EXTERNALSYM midiInMessage}


  //****************************************************************************
  //
  //                    Auxiliary audio support
  //
  //****************************************************************************

  { device ID for aux device mapper }

const

  {$EXTERNALSYM AUX_MAPPER}
  AUX_MAPPER                          = UINT(- 1);


  { Auxiliary audio device capabilities structure }

type

  PAUXCAPSA = ^tagAUXCAPSA;
  {$EXTERNALSYM PAUXCAPSA}
  tagAUXCAPSA = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name (NULL terminated string) }
    wTechnology: WORD;                                { type of device }
    wReserved1: WORD;                                 { padding }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagAUXCAPSA}
  AUXCAPSA = tagAUXCAPSA;
  {$EXTERNALSYM AUXCAPSA}
  NPAUXCAPSA = ^tagAUXCAPSA;
  {$EXTERNALSYM NPAUXCAPSA}
  LPAUXCAPSA = ^tagAUXCAPSA;
  {$EXTERNALSYM LPAUXCAPSA}

  PAUXCAPSW = ^tagAUXCAPSW;
  {$EXTERNALSYM PAUXCAPSW}
  tagAUXCAPSW = record
    wMid: WORD;                                      { manufacturer ID }
    wPid: WORD;                                      { product ID }
    vDriverVersion: MMVERSION;                       { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;  { product name (NULL terminated string) }
    wTechnology: WORD;                               { type of device }
    wReserved1: WORD;                                { padding }
    dwSupport: DWORD;                                { functionality supported by driver }
  end;
  {$EXTERNALSYM tagAUXCAPSW}
  AUXCAPSW = tagAUXCAPSW;
  {$EXTERNALSYM AUXCAPSW}
  NPAUXCAPSW = ^tagAUXCAPSW;
  {$EXTERNALSYM NPAUXCAPSW}
  LPAUXCAPSW = ^tagAUXCAPSW;
  {$EXTERNALSYM LPAUXCAPSW}

  { Delphi }
  AUXCAPS = tagAUXCAPSW;
  PAUXCAPS = ^tagAUXCAPSW;
  NPAUXCAPS = ^tagAUXCAPSW;
  LPAUXCAPS = ^tagAUXCAPSW;


  PAUXCAPS2A = ^tagAUXCAPS2A;
  {$EXTERNALSYM PAUXCAPS2A}
  tagAUXCAPS2A = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name (NULL terminated string) }
    wTechnology: WORD;                                { type of device }
    wReserved1: WORD;                                 { padding }
    dwSupport: DWORD;                                 { functionality supported by driver }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagAUXCAPS2A}
  AUXCAPS2A = tagAUXCAPS2A;
  {$EXTERNALSYM AUXCAPS2A}
  NPAUXCAPS2A = ^tagAUXCAPS2A;
  {$EXTERNALSYM NPAUXCAPS2A}
  LPAUXCAPS2A = ^tagAUXCAPS2A;
  {$EXTERNALSYM LPAUXCAPS2A}

  PAUXCAPS2W = ^tagAUXCAPS2W;
  {$EXTERNALSYM PAUXCAPS2W}
  tagAUXCAPS2W = record
    wMid: WORD;                                       { manufacturer ID }
    wPid: WORD;                                       { product ID }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;   { product name (NULL terminated string) }
    wTechnology: WORD;                                { type of device }
    wReserved1: WORD;                                 { padding }
    dwSupport: DWORD;                                 { functionality supported by driver }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagAUXCAPS2W}
  AUXCAPS2W = tagAUXCAPS2W;
  {$EXTERNALSYM AUXCAPS2W}
  NPAUXCAPS2W = ^tagAUXCAPS2W;
  {$EXTERNALSYM NPAUXCAPS2W}
  LPAUXCAPS2W = ^tagAUXCAPS2W;
  {$EXTERNALSYM LPAUXCAPS2W}

  { Delphi}
  AUXCAPS2 = tagAUXCAPS2W;
  PAUXCAPS2 = ^tagAUXCAPS2W;
  LPAUXCAPS2 = ^tagAUXCAPS2W;


  { flags for wTechnology field in AUXCAPS structure }

const

  {$EXTERNALSYM AUXCAPS_CDAUDIO}
  AUXCAPS_CDAUDIO                     = 1;  { audio from internal CD-ROM drive }
  {$EXTERNALSYM AUXCAPS_AUXIN}
  AUXCAPS_AUXIN                       = 2;  { audio from auxiliary input jacks }

  { flags for dwSupport field in AUXCAPS structure }
  {$EXTERNALSYM AUXCAPS_VOLUME}
  AUXCAPS_VOLUME                      = $0001;  { supports volume control }
  {$EXTERNALSYM AUXCAPS_LRVOLUME}
  AUXCAPS_LRVOLUME                    = $0002;  { separate left-right volume control }

  { auxiliary audio function prototypes }

  function auxGetNumDevs(): MMRESULT; stdcall;
  {$EXTERNALSYM auxGetNumDevs}


  function auxGetDevCapsA(uDeviceID: UINT_PTR;
                          {out} pac: LPAUXCAPSA;
                          cbac: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM auxGetDevCapsA}

  function auxGetDevCapsW(uDeviceID: UINT_PTR;
                          {out} pac: LPAUXCAPSW;
                          cbac: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM auxGetDevCapsW}

  { Delphi }
  function auxGetDevCaps(uDeviceID: UINT_PTR;
                         {out} pac: LPAUXCAPSW;
                         cbac: UINT): MMRESULT; stdcall;


  function auxSetVolume(uDeviceID: UINT;
                        dwVolume: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM auxSetVolume}

  function auxGetVolume(uDeviceID: UINT;
                        {out} pdwVolume: PDWORD): MMRESULT; stdcall;
  {$EXTERNALSYM auxGetVolume}


  function auxOutMessage(uDeviceID: UINT;
                         uMsg: UINT;
                         {_In_opt_} dw1: DWORD_PTR;
                         {_In_opt_} dw2: DWORD_PTR): MMRESULT; stdcall;
  {$EXTERNALSYM auxOutMessage}


  //****************************************************************************
  //
  //                        Mixer Support
  //
  //****************************************************************************/

type

  PHMIXEROBJ = ^HMIXEROBJ;
  HMIXEROBJ = IntPtr;
  {$EXTERNALSYM HMIXEROBJ}
  LPHMIXEROBJ = ^HMIXEROBJ;

  PHMIXER = ^HMIXER;
  HMIXER = IntPtr;
  {$EXTERNALSYM HMIXER}
  LPHMIXER = ^HMIXER;

const

  {$EXTERNALSYM MIXER_SHORT_NAME_CHARS}
  MIXER_SHORT_NAME_CHARS              = 16;
  {$EXTERNALSYM MIXER_LONG_NAME_CHARS}
  MIXER_LONG_NAME_CHARS               = 64;

  {  MMRESULT error return values specific to the mixer API }

const

  {$EXTERNALSYM MIXERR_INVALLINE}
  MIXERR_INVALLINE                    = (MIXERR_BASE + 0);
  {$EXTERNALSYM MIXERR_INVALCONTROL}
  MIXERR_INVALCONTROL                 = (MIXERR_BASE + 1);
  {$EXTERNALSYM MIXERR_INVALVALUE}
  MIXERR_INVALVALUE                   = (MIXERR_BASE + 2);
  {$EXTERNALSYM MIXERR_LASTERROR}
  MIXERR_LASTERROR                    = (MIXERR_BASE + 2);

  {$EXTERNALSYM MIXER_OBJECTF_HANDLE}
  MIXER_OBJECTF_HANDLE                = $80000000;
  {$EXTERNALSYM MIXER_OBJECTF_MIXER}
  MIXER_OBJECTF_MIXER                 = $00000000;
  {$EXTERNALSYM MIXER_OBJECTF_HMIXER}
  MIXER_OBJECTF_HMIXER                = (MIXER_OBJECTF_HANDLE or MIXER_OBJECTF_MIXER);
  {$EXTERNALSYM MIXER_OBJECTF_WAVEOUT}
  MIXER_OBJECTF_WAVEOUT               = $10000000;
  {$EXTERNALSYM MIXER_OBJECTF_HWAVEOUT}
  MIXER_OBJECTF_HWAVEOUT              = (MIXER_OBJECTF_HANDLE or MIXER_OBJECTF_WAVEOUT);
  {$EXTERNALSYM MIXER_OBJECTF_WAVEIN}
  MIXER_OBJECTF_WAVEIN                = $20000000;
  {$EXTERNALSYM MIXER_OBJECTF_HWAVEIN}
  MIXER_OBJECTF_HWAVEIN               = (MIXER_OBJECTF_HANDLE or MIXER_OBJECTF_WAVEIN);
  {$EXTERNALSYM MIXER_OBJECTF_MIDIOUT}
  MIXER_OBJECTF_MIDIOUT               = $30000000;
  {$EXTERNALSYM MIXER_OBJECTF_HMIDIOUT}
  MIXER_OBJECTF_HMIDIOUT              = (MIXER_OBJECTF_HANDLE or MIXER_OBJECTF_MIDIOUT);
  {$EXTERNALSYM MIXER_OBJECTF_MIDIIN}
  MIXER_OBJECTF_MIDIIN                = $40000000;
  {$EXTERNALSYM MIXER_OBJECTF_HMIDIIN}
  MIXER_OBJECTF_HMIDIIN               = (MIXER_OBJECTF_HANDLE or MIXER_OBJECTF_MIDIIN);
  {$EXTERNALSYM MIXER_OBJECTF_AUX}
  MIXER_OBJECTF_AUX                   = $50000000;

  function mixerGetNumDevs(): UINT; stdcall;

type

  PMIXERCAPSA = ^tagMIXERCAPSA;
  {$EXTERNALSYM PMIXERCAPSA}
  tagMIXERCAPSA = record
    wMid: WORD;                                       { manufacturer id }
    wPid: WORD;                                       { product id }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name }
    fdwSupport: DWORD;                                { misc. support bits }
    cDestinations: DWORD;                            { count of destinations }
  end;
  {$EXTERNALSYM tagMIXERCAPSA}
  MIXERCAPSA = tagMIXERCAPSA;
  {$EXTERNALSYM MIXERCAPSA}
  LPMIXERCAPSA = ^tagMIXERCAPSA;
  {$EXTERNALSYM LPMIXERCAPSA}

  PMIXERCAPSW = ^tagMIXERCAPSW;
  {$EXTERNALSYM PMIXERCAPSW}
  tagMIXERCAPSW = record
    wMid: WORD;                                       { manufacturer id }
    wPid: WORD;                                       { product id }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;   { product name }
    fdwSupport: DWORD;                                { misc. support bits }
    cDestinations: DWORD;                            { count of destinations }
  end;
  {$EXTERNALSYM tagMIXERCAPSW}
  MIXERCAPSW = tagMIXERCAPSW;
  {$EXTERNALSYM MIXERCAPSW}
  LPMIXERCAPSW = ^tagMIXERCAPSW;
  {$EXTERNALSYM LPMIXERCAPSW}

  { Delphi }
  MIXERCAPS = tagMIXERCAPSW;
  PMIXERCAPS = ^tagMIXERCAPSW;
  LPMIXERCAPS = ^tagMIXERCAPSW;


  tagMIXERCAPS2A = record
    wMid: WORD;                                       { manufacturer id }
    wPid: WORD;                                       { product id }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   { product name }
    fdwSupport: DWORD;                                { misc. support bits }
    cDestinations: DWORD;                             { count of destinations }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIXERCAPS2A}
  MIXERCAPS2A = tagMIXERCAPS2A;
  {$EXTERNALSYM MIXERCAPS2A}
  PMIXERCAPS2A = ^tagMIXERCAPS2A;
  {$EXTERNALSYM PMIXERCAPS2A}
  LPMIXERCAPS2A = ^tagMIXERCAPS2A;
  {$EXTERNALSYM LPMIXERCAPS2A}

  tagMIXERCAPS2W = record
    wMid: WORD;                                       { manufacturer id }
    wPid: WORD;                                       { product id }
    vDriverVersion: MMVERSION;                        { version of the driver }
    szPname: array[0..MAXPNAMELEN - 1] of WideChar;   { product name }
    fdwSupport: DWORD;                                { misc. support bits }
    cDestinations: DWORD;                             { count of destinations }
    ManufacturerGuid: TGUID;                          { for extensible MID mapping }
    ProductGuid: TGUID;                               { for extensible PID mapping }
    NameGuid: TGUID;                                 { for name lookup in registry }
  end;
  {$EXTERNALSYM tagMIXERCAPS2W}
  MIXERCAPS2W = tagMIXERCAPS2W;
  {$EXTERNALSYM MIXERCAPS2W}
  PMIXERCAPS2W = ^tagMIXERCAPS2W;
  {$EXTERNALSYM PMIXERCAPS2W}
  LPMIXERCAPS2W = ^tagMIXERCAPS2W;
  {$EXTERNALSYM LPMIXERCAPS2W}

  { Delphi }
  MIXERCAPS2 = tagMIXERCAPS2W;
  PMIXERCAPS2 = ^tagMIXERCAPS2W;
  LPMIXERCAPS2 = ^tagMIXERCAPS2W;


  function mixerGetDevCapsA(uMxId: UINT_PTR;
                            {out} pmxcaps: LPMIXERCAPSA;
                            cbmxcaps: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetDevCapsA}

  function mixerGetDevCapsW(uMxId: UINT_PTR;
                            {out} pmxcaps: LPMIXERCAPSW;
                            cbmxcaps: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetDevCapsW}

  function mixerGetDevCaps(uMxId: UINT_PTR;
                           {out} pmxcaps: PMIXERCAPS;
                           cbmxcaps: UINT): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetDevCaps}

  function mixerOpen({outopt_} phmx: LPHMIXER;
                     uMxId: UINT;
                     {_In_opt_} dwCallback: DWORD_PTR;
                     {_In_opt_} dwInstance: DWORD_PTR;
                     fdwOpen: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerOpen}

  function mixerClose(hmx: HMIXER): MMRESULT; stdcall;
  {$EXTERNALSYM mixerClose}

  function mixerMessage({_In_opt_} hmx: HMIXER;
                        uMsg: UINT;
                        {_In_opt_} dwParam1: DWORD_PTR;
                        {_In_opt_} dwParam2: DWORD_PTR): DWORD; stdcall;
  {$EXTERNALSYM mixerMessage}

type

  PMIXERLINEA = ^tagMIXERLINEA;
  {$EXTERNALSYM PMIXERLINEA}
  tagMIXERLINEA = record
    cbStruct: DWORD;                                    { size of MIXERLINE structure }
    dwDestination: DWORD;                               { zero based destination index }
    dwSource: DWORD;                                    { zero based source index (if source) }
    dwLineID: DWORD;                                    { unique line id for mixer device }
    fdwLine: DWORD;                                     { state/information about line }
    dwUser: DWORD_PTR;                                  { driver specific information }
    dwComponentType: DWORD;                             { component type line connects to }
    cChannels: DWORD;                                   { number of channels line supports }
    cConnections: DWORD;                                { number of connections [possible] }
    cControls: DWORD;                                   { number of controls at this line }
    szShortName: array[0..MIXER_SHORT_NAME_CHARS - 1] of AnsiChar;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of AnsiChar;
    Target: record
      dwType: DWORD;                                    { MIXERLINE_TARGETTYPE_xxxx }
      dwDeviceID: DWORD;                                { target device ID of device type }
      wMid: WORD;                                       { of target device }
      wPid: WORD;                                       {      " }
      vDriverVersion: MMVERSION;                        {      " }
      szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;   {      " }
    end;
  end;
  {$EXTERNALSYM tagMIXERLINEA}
  MIXERLINEA = tagMIXERLINEA;
  {$EXTERNALSYM MIXERLINEA}
  LPMIXERLINEA = ^tagMIXERLINEA;
  {$EXTERNALSYM LPMIXERLINEA}

  PMIXERLINEW = ^tagMIXERLINEW;
  {$EXTERNALSYM PMIXERLINEW}
  tagMIXERLINEW = record
    cbStruct: DWORD;                                    { size of MIXERLINE structure }
    dwDestination: DWORD;                               { zero based destination index }
    dwSource: DWORD;                                    { zero based source index (if source) }
    dwLineID: DWORD;                                    { unique line id for mixer device }
    fdwLine: DWORD;                                     { state/information about line }
    dwUser: DWORD_PTR;                                  { driver specific information }
    dwComponentType: DWORD;                             { component type line connects to }
    cChannels: DWORD;                                   { number of channels line supports }
    cConnections: DWORD;                                { number of connections [possible] }
    cControls: DWORD;                                   { number of controls at this line }
    szShortName: array[0..MIXER_SHORT_NAME_CHARS - 1] of WideChar;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of WideChar;
    Target: record
      dwType: DWORD;                                    { MIXERLINE_TARGETTYPE_xxxx }
      dwDeviceID: DWORD;                                { target device ID of device type }
      wMid: WORD;                                       { of target device }
      wPid: WORD;                                       {      " }
      vDriverVersion: MMVERSION;                        {      " }
      szPname: array[0..MAXPNAMELEN - 1] of WideChar;   {      " }
    end;
  end;
  {$EXTERNALSYM tagMIXERLINEW}
  MIXERLINEW = tagMIXERLINEW;
  {$EXTERNALSYM MIXERLINEW}
  LPMIXERLINEW = ^tagMIXERLINEW;
  {$EXTERNALSYM LPMIXERLINEW}

  { Delphi }
  MIXERLINE = tagMIXERLINEW;
  PMIXERLINE = ^tagMIXERLINEW;
  LPMIXERLINE = ^tagMIXERLINEW;


  {  MIXERLINE.fdwLine }

const

  {$EXTERNALSYM MIXERLINE_LINEF_ACTIVE}
  MIXERLINE_LINEF_ACTIVE              = $00000001;
  {$EXTERNALSYM MIXERLINE_LINEF_DISCONNECTED}
  MIXERLINE_LINEF_DISCONNECTED        = $00008000;
  {$EXTERNALSYM MIXERLINE_LINEF_SOURCE}
  MIXERLINE_LINEF_SOURCE              = $80000000;


  {  MIXERLINE.dwComponentType }

  {  component types for destinations and sources }

const

  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_FIRST}
  MIXERLINE_COMPONENTTYPE_DST_FIRST   = $00000000;
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_UNDEFINED}
  MIXERLINE_COMPONENTTYPE_DST_UNDEFINED= (MIXERLINE_COMPONENTTYPE_DST_FIRST + 0);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_DIGITAL}
  MIXERLINE_COMPONENTTYPE_DST_DIGITAL = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 1);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_LINE}
  MIXERLINE_COMPONENTTYPE_DST_LINE    = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 2);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_MONITOR}
  MIXERLINE_COMPONENTTYPE_DST_MONITOR = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 3);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_SPEAKERS}
  MIXERLINE_COMPONENTTYPE_DST_SPEAKERS= (MIXERLINE_COMPONENTTYPE_DST_FIRST + 4);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_HEADPHONES}
  MIXERLINE_COMPONENTTYPE_DST_HEADPHONES= (MIXERLINE_COMPONENTTYPE_DST_FIRST + 5);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_TELEPHONE}
  MIXERLINE_COMPONENTTYPE_DST_TELEPHONE= (MIXERLINE_COMPONENTTYPE_DST_FIRST + 6);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_WAVEIN}
  MIXERLINE_COMPONENTTYPE_DST_WAVEIN  = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 7);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_VOICEIN}
  MIXERLINE_COMPONENTTYPE_DST_VOICEIN = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 8);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_DST_LAST}
  MIXERLINE_COMPONENTTYPE_DST_LAST    = (MIXERLINE_COMPONENTTYPE_DST_FIRST + 8);

  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_FIRST}
  MIXERLINE_COMPONENTTYPE_SRC_FIRST   = $00001000;
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED}
  MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 0);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_DIGITAL}
  MIXERLINE_COMPONENTTYPE_SRC_DIGITAL = (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 1);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_LINE}
  MIXERLINE_COMPONENTTYPE_SRC_LINE    = (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 2);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE}
  MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 3);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER}
  MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 4);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC}
  MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 5);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE}
  MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 6);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER}
  MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 7);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT}
  MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT = (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 8);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY}
  MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY= (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 9);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_ANALOG}
  MIXERLINE_COMPONENTTYPE_SRC_ANALOG  = (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 10);
  {$EXTERNALSYM MIXERLINE_COMPONENTTYPE_SRC_LAST}
  MIXERLINE_COMPONENTTYPE_SRC_LAST    = (MIXERLINE_COMPONENTTYPE_SRC_FIRST + 10);


  {  MIXERLINE.Target.dwType }
const
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_UNDEFINED}
  MIXERLINE_TARGETTYPE_UNDEFINED      = 0;
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_WAVEOUT}
  MIXERLINE_TARGETTYPE_WAVEOUT        = 1;
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_WAVEIN}
  MIXERLINE_TARGETTYPE_WAVEIN         = 2;
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_MIDIOUT}
  MIXERLINE_TARGETTYPE_MIDIOUT        = 3;
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_MIDIIN}
  MIXERLINE_TARGETTYPE_MIDIIN         = 4;
  {$EXTERNALSYM MIXERLINE_TARGETTYPE_AUX}
  MIXERLINE_TARGETTYPE_AUX            = 5;


  function mixerGetLineInfoA({_In_opt_} hmxobj: HMIXEROBJ;
                             {_Inout_} pmxl: LPMIXERLINEA;
                             fdwInfo: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetLineInfoA}

  function mixerGetLineInfoW({_In_opt_} hmxobj: HMIXEROBJ;
                             {_Inout_} pmxl: LPMIXERLINEW;
                             fdwInfo: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetLineInfoW}

  { Delphi }
  function mixerGetLineInfo({_In_opt_} hmxobj: HMIXEROBJ;
                            {_Inout_} pmxl: LPMIXERLINEW;
                            fdwInfo: DWORD): MMRESULT; stdcall;


const

  {$EXTERNALSYM MIXER_GETLINEINFOF_DESTINATION}
  MIXER_GETLINEINFOF_DESTINATION      = $00000000;
  {$EXTERNALSYM MIXER_GETLINEINFOF_SOURCE}
  MIXER_GETLINEINFOF_SOURCE           = $00000001;
  {$EXTERNALSYM MIXER_GETLINEINFOF_LINEID}
  MIXER_GETLINEINFOF_LINEID           = $00000002;
  {$EXTERNALSYM MIXER_GETLINEINFOF_COMPONENTTYPE}
  MIXER_GETLINEINFOF_COMPONENTTYPE    = $00000003;
  {$EXTERNALSYM MIXER_GETLINEINFOF_TARGETTYPE}
  MIXER_GETLINEINFOF_TARGETTYPE       = $00000004;

  {$EXTERNALSYM MIXER_GETLINEINFOF_QUERYMASK}
  MIXER_GETLINEINFOF_QUERYMASK        = $0000000F;


  function mixerGetID({_In_opt_} hmxobj: HMIXEROBJ;
                      out puMxId: UINT;
                      fdwId: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetID}


  { MIXERCONTROL }

type

  PMIXERCONTROLA = ^tagMIXERCONTROLA;
  {$EXTERNALSYM PMIXERCONTROLA}
  tagMIXERCONTROLA = record
    cbStruct: DWORD;                      { size in bytes of MIXERCONTROL }
    dwControlID: DWORD;                   { unique control id for mixer device }
    dwControlType: DWORD;                 { MIXERCONTROL_CONTROLTYPE_xxx }
    fdwControl: DWORD;                    { MIXERCONTROL_CONTROLF_xxx }
    cMultipleItems: DWORD;                { if MIXERCONTROL_CONTROLF_MULTIPLE set }
    szShortName: array[0..MIXER_SHORT_NAME_CHARS - 1] of AnsiChar;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of AnsiChar;

    Bounds: record
      case Integer of
        0: (lMinimum: LONG;
            lMaximum: LONG);

        1: (dwMinimum: DWORD;
            dwMaximum: DWORD);

        2: (dwReserved: array[0..5] of DWORD);
    end;

    Metrics: record
      case Integer of
        0: (cSteps: DWORD);        { # of steps between min & max }

        1: (cbCustomData: DWORD);  { size in bytes of custom data }

        2: (dwReserved: array[0..5] of DWORD);
    end;
  end;
  {$EXTERNALSYM tagMIXERCONTROLA}
  MIXERCONTROLA = tagMIXERCONTROLA;
  {$EXTERNALSYM MIXERCONTROLA}
  LPMIXERCONTROLA = ^tagMIXERCONTROLA;
  {$EXTERNALSYM LPMIXERCONTROLA}

  PMIXERCONTROLW = ^tagMIXERCONTROLW;
  {$EXTERNALSYM PMIXERCONTROLW}
  tagMIXERCONTROLW = record
    cbStruct: DWORD;                      { size in bytes of MIXERCONTROL }
    dwControlID: DWORD;                   { unique control id for mixer device }
    dwControlType: DWORD;                 { MIXERCONTROL_CONTROLTYPE_xxx }
    fdwControl: DWORD;                    { MIXERCONTROL_CONTROLF_xxx }
    cMultipleItems: DWORD;                { if MIXERCONTROL_CONTROLF_MULTIPLE set }
    szShortName: array[0..MIXER_SHORT_NAME_CHARS - 1] of WideChar;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of WideChar;

    Bounds: record
      case Integer of
        0: (lMinimum: LONG;
            lMaximum: LONG);

        1: (dwMinimum: DWORD;
            dwMaximum: DWORD);

        2: (dwReserved: array[0..5] of DWORD);
    end;

    Metrics: record
      case Integer of
        0: (cSteps: DWORD);        { # of steps between min & max }

        1: (cbCustomData: DWORD);  { size in bytes of custom data }

        2: (dwReserved: array[0..5] of DWORD);
    end;
  end;
  {$EXTERNALSYM tagMIXERCONTROLW}
  MIXERCONTROLW = tagMIXERCONTROLW;
  {$EXTERNALSYM MIXERCONTROLW}
  LPMIXERCONTROLW = ^tagMIXERCONTROLW;
  {$EXTERNALSYM LPMIXERCONTROLW}

  { Delphi }
  MIXERCONTROL = tagMIXERCONTROLW;
  PMIXERCONTROL = ^tagMIXERCONTROLW;
  LPMIXERCONTROL = ^tagMIXERCONTROLW;


  {  MIXERCONTROL.fdwControl }

const

  {$EXTERNALSYM MIXERCONTROL_CONTROLF_UNIFORM}
  MIXERCONTROL_CONTROLF_UNIFORM       = $00000001;
  {$EXTERNALSYM MIXERCONTROL_CONTROLF_MULTIPLE}
  MIXERCONTROL_CONTROLF_MULTIPLE      = $00000002;
  {$EXTERNALSYM MIXERCONTROL_CONTROLF_DISABLED}
  MIXERCONTROL_CONTROLF_DISABLED      = $80000000;

  {  MIXERCONTROL_CONTROLTYPE_xxx building block defines }

  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_MASK}
  MIXERCONTROL_CT_CLASS_MASK          = $F0000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_CUSTOM}
  MIXERCONTROL_CT_CLASS_CUSTOM        = $00000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_METER}
  MIXERCONTROL_CT_CLASS_METER         = $10000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_SWITCH}
  MIXERCONTROL_CT_CLASS_SWITCH        = $20000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_NUMBER}
  MIXERCONTROL_CT_CLASS_NUMBER        = $30000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_SLIDER}
  MIXERCONTROL_CT_CLASS_SLIDER        = $40000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_FADER}
  MIXERCONTROL_CT_CLASS_FADER         = $50000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_TIME}
  MIXERCONTROL_CT_CLASS_TIME          = $60000000;
  {$EXTERNALSYM MIXERCONTROL_CT_CLASS_LIST}
  MIXERCONTROL_CT_CLASS_LIST          = $70000000;

  {$EXTERNALSYM MIXERCONTROL_CT_SUBCLASS_MASK}
  MIXERCONTROL_CT_SUBCLASS_MASK       = $0F000000;

  {$EXTERNALSYM MIXERCONTROL_CT_SC_SWITCH_BOOLEAN}
  MIXERCONTROL_CT_SC_SWITCH_BOOLEAN   = $00000000;
  {$EXTERNALSYM MIXERCONTROL_CT_SC_SWITCH_BUTTON}
  MIXERCONTROL_CT_SC_SWITCH_BUTTON    = $01000000;

  {$EXTERNALSYM MIXERCONTROL_CT_SC_METER_POLLED}
  MIXERCONTROL_CT_SC_METER_POLLED     = $00000000;

  {$EXTERNALSYM MIXERCONTROL_CT_SC_TIME_MICROSECS}
  MIXERCONTROL_CT_SC_TIME_MICROSECS   = $00000000;
  {$EXTERNALSYM MIXERCONTROL_CT_SC_TIME_MILLISECS}
  MIXERCONTROL_CT_SC_TIME_MILLISECS   = $01000000;

  {$EXTERNALSYM MIXERCONTROL_CT_SC_LIST_SINGLE}
  MIXERCONTROL_CT_SC_LIST_SINGLE      = $00000000;
  {$EXTERNALSYM MIXERCONTROL_CT_SC_LIST_MULTIPLE}
  MIXERCONTROL_CT_SC_LIST_MULTIPLE    = $01000000;

  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_MASK}
  MIXERCONTROL_CT_UNITS_MASK          = $00FF0000;
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_CUSTOM}
  MIXERCONTROL_CT_UNITS_CUSTOM        = $00000000;
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_BOOLEAN}
  MIXERCONTROL_CT_UNITS_BOOLEAN       = $00010000;
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_SIGNED}
  MIXERCONTROL_CT_UNITS_SIGNED        = $00020000;
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_UNSIGNED}
  MIXERCONTROL_CT_UNITS_UNSIGNED      = $00030000;
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_DECIBELS}
  MIXERCONTROL_CT_UNITS_DECIBELS      = $00040000;  { in 10ths }
  {$EXTERNALSYM MIXERCONTROL_CT_UNITS_PERCENT}
  MIXERCONTROL_CT_UNITS_PERCENT       = $00050000;  { in 10ths }


  {  Commonly used control types for specifying MIXERCONTROL.dwControlType }

const

  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_CUSTOM}
  MIXERCONTROL_CONTROLTYPE_CUSTOM     = (MIXERCONTROL_CT_CLASS_CUSTOM or MIXERCONTROL_CT_UNITS_CUSTOM);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_BOOLEANMETER}
  MIXERCONTROL_CONTROLTYPE_BOOLEANMETER= (MIXERCONTROL_CT_CLASS_METER or MIXERCONTROL_CT_SC_METER_POLLED or MIXERCONTROL_CT_UNITS_BOOLEAN);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SIGNEDMETER}
  MIXERCONTROL_CONTROLTYPE_SIGNEDMETER= (MIXERCONTROL_CT_CLASS_METER or MIXERCONTROL_CT_SC_METER_POLLED or MIXERCONTROL_CT_UNITS_SIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_PEAKMETER}
  MIXERCONTROL_CONTROLTYPE_PEAKMETER  = (MIXERCONTROL_CONTROLTYPE_SIGNEDMETER + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_UNSIGNEDMETER}
  MIXERCONTROL_CONTROLTYPE_UNSIGNEDMETER= (MIXERCONTROL_CT_CLASS_METER or MIXERCONTROL_CT_SC_METER_POLLED or MIXERCONTROL_CT_UNITS_UNSIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_BOOLEAN}
  MIXERCONTROL_CONTROLTYPE_BOOLEAN    = (MIXERCONTROL_CT_CLASS_SWITCH or MIXERCONTROL_CT_SC_SWITCH_BOOLEAN or MIXERCONTROL_CT_UNITS_BOOLEAN);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_ONOFF}
  MIXERCONTROL_CONTROLTYPE_ONOFF      = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MUTE}
  MIXERCONTROL_CONTROLTYPE_MUTE       = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 2);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MONO}
  MIXERCONTROL_CONTROLTYPE_MONO       = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 3);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_LOUDNESS}
  MIXERCONTROL_CONTROLTYPE_LOUDNESS   = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 4);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_STEREOENH}
  MIXERCONTROL_CONTROLTYPE_STEREOENH  = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 5);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_BASS_BOOST}
  MIXERCONTROL_CONTROLTYPE_BASS_BOOST = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + $00002277);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_BUTTON}
  MIXERCONTROL_CONTROLTYPE_BUTTON     = (MIXERCONTROL_CT_CLASS_SWITCH or MIXERCONTROL_CT_SC_SWITCH_BUTTON or MIXERCONTROL_CT_UNITS_BOOLEAN);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_DECIBELS}
  MIXERCONTROL_CONTROLTYPE_DECIBELS   = (MIXERCONTROL_CT_CLASS_NUMBER or MIXERCONTROL_CT_UNITS_DECIBELS);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SIGNED}
  MIXERCONTROL_CONTROLTYPE_SIGNED     = (MIXERCONTROL_CT_CLASS_NUMBER or MIXERCONTROL_CT_UNITS_SIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_UNSIGNED}
  MIXERCONTROL_CONTROLTYPE_UNSIGNED   = (MIXERCONTROL_CT_CLASS_NUMBER or MIXERCONTROL_CT_UNITS_UNSIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_PERCENT}
  MIXERCONTROL_CONTROLTYPE_PERCENT    = (MIXERCONTROL_CT_CLASS_NUMBER or MIXERCONTROL_CT_UNITS_PERCENT);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SLIDER}
  MIXERCONTROL_CONTROLTYPE_SLIDER     = (MIXERCONTROL_CT_CLASS_SLIDER or MIXERCONTROL_CT_UNITS_SIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_PAN}
  MIXERCONTROL_CONTROLTYPE_PAN        = (MIXERCONTROL_CONTROLTYPE_SLIDER + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_QSOUNDPAN}
  MIXERCONTROL_CONTROLTYPE_QSOUNDPAN  = (MIXERCONTROL_CONTROLTYPE_SLIDER + 2);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_FADER}
  MIXERCONTROL_CONTROLTYPE_FADER      = (MIXERCONTROL_CT_CLASS_FADER or MIXERCONTROL_CT_UNITS_UNSIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_VOLUME}
  MIXERCONTROL_CONTROLTYPE_VOLUME     = (MIXERCONTROL_CONTROLTYPE_FADER + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_BASS}
  MIXERCONTROL_CONTROLTYPE_BASS       = (MIXERCONTROL_CONTROLTYPE_FADER + 2);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_TREBLE}
  MIXERCONTROL_CONTROLTYPE_TREBLE     = (MIXERCONTROL_CONTROLTYPE_FADER + 3);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_EQUALIZER}
  MIXERCONTROL_CONTROLTYPE_EQUALIZER  = (MIXERCONTROL_CONTROLTYPE_FADER + 4);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SINGLESELECT}
  MIXERCONTROL_CONTROLTYPE_SINGLESELECT= (MIXERCONTROL_CT_CLASS_LIST or MIXERCONTROL_CT_SC_LIST_SINGLE or MIXERCONTROL_CT_UNITS_BOOLEAN);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MUX}
  MIXERCONTROL_CONTROLTYPE_MUX        = (MIXERCONTROL_CONTROLTYPE_SINGLESELECT + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT}
  MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT= (MIXERCONTROL_CT_CLASS_LIST or MIXERCONTROL_CT_SC_LIST_MULTIPLE or MIXERCONTROL_CT_UNITS_BOOLEAN);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MIXER}
  MIXERCONTROL_CONTROLTYPE_MIXER      = (MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT + 1);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MICROTIME}
  MIXERCONTROL_CONTROLTYPE_MICROTIME  = (MIXERCONTROL_CT_CLASS_TIME or MIXERCONTROL_CT_SC_TIME_MICROSECS or MIXERCONTROL_CT_UNITS_UNSIGNED);
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_MILLITIME}
  MIXERCONTROL_CONTROLTYPE_MILLITIME  = (MIXERCONTROL_CT_CLASS_TIME or MIXERCONTROL_CT_SC_TIME_MILLISECS or MIXERCONTROL_CT_UNITS_UNSIGNED);


  {  MIXERLINECONTROLS }

type

  PMIXERLINECONTROLSA = ^tagMIXERLINECONTROLSA;
  {$EXTERNALSYM PMIXERLINECONTROLSA}
  tagMIXERLINECONTROLSA = record

    cbStruct: DWORD;                    { size in bytes of MIXERLINECONTROLS }
    dwLineID: DWORD;                    { line id (from MIXERLINE.dwLineID) }

    case Integer of
      0: (dwControlID: DWORD);          { MIXER_GETLINECONTROLSF_ONEBYID }
      1: (dwControlType: DWORD;         { MIXER_GETLINECONTROLSF_ONEBYTYPE }
          cControls: DWORD;             { count of controls pmxctrl points to }
          cbmxctrl: DWORD;              { size in bytes of _one_ MIXERCONTROL }
          pamxctrl: LPMIXERCONTROLA);   { pointer to first MIXERCONTROL array }
  end;
  {$EXTERNALSYM tagMIXERLINECONTROLSA}
  MIXERLINECONTROLSA = tagMIXERLINECONTROLSA;
  {$EXTERNALSYM MIXERLINECONTROLSA}
  LPMIXERLINECONTROLSA = ^tagMIXERLINECONTROLSA;
  {$EXTERNALSYM LPMIXERLINECONTROLSA}

  PMIXERLINECONTROLSW = ^tagMIXERLINECONTROLSW;
  {$EXTERNALSYM PMIXERLINECONTROLSW}
  tagMIXERLINECONTROLSW = record

    cbStruct: DWORD;                   { size in bytes of MIXERLINECONTROLS }
    dwLineID: DWORD;                   { line id (from MIXERLINE.dwLineID) }

    case Integer of
      0: (dwControlID: DWORD);          { MIXER_GETLINECONTROLSF_ONEBYID }
      1: (dwControlType: DWORD;         { MIXER_GETLINECONTROLSF_ONEBYTYPE }
          cControls: DWORD;             { count of controls pmxctrl points to }
          cbmxctrl: DWORD;              { size in bytes of _one_ MIXERCONTROL }
          pamxctrl: LPMIXERCONTROLW);   { pointer to first MIXERCONTROL array }
  end;
  {$EXTERNALSYM tagMIXERLINECONTROLSW}
  MIXERLINECONTROLSW = tagMIXERLINECONTROLSW;
  {$EXTERNALSYM MIXERLINECONTROLSW}
  LPMIXERLINECONTROLSW = ^tagMIXERLINECONTROLSW;
  {$EXTERNALSYM LPMIXERLINECONTROLSW}

  { Delphi }
  MIXERLINECONTROLS = tagMIXERLINECONTROLSW;
  PMIXERLINECONTROLS = ^tagMIXERLINECONTROLSW;
  LPMIXERLINECONTROLS = ^tagMIXERLINECONTROLSW;


  function mixerGetLineControlsA({_In_opt_} hmxobj: HMIXEROBJ;
                                 {_Inout_} pmxlc: LPMIXERLINECONTROLSA;
                                 fdwControls: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetLineControlsA}

  function mixerGetLineControlsW({_In_opt_} hmxobj: HMIXEROBJ;
                                 {_Inout_} pmxlc: LPMIXERLINECONTROLSW;
                                 fdwControls: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetLineControlsW}

  { Delphi }
  function mixerGetLineControls({_In_opt_} hmxobj: HMIXEROBJ;
                                {_Inout_} pmxlc: LPMIXERLINECONTROLSW;
                                fdwControls: DWORD): MMRESULT; stdcall;

const

  {$EXTERNALSYM MIXER_GETLINECONTROLSF_ALL}
  MIXER_GETLINECONTROLSF_ALL          = $00000000;
  {$EXTERNALSYM MIXER_GETLINECONTROLSF_ONEBYID}
  MIXER_GETLINECONTROLSF_ONEBYID      = $00000001;
  {$EXTERNALSYM MIXER_GETLINECONTROLSF_ONEBYTYPE}
  MIXER_GETLINECONTROLSF_ONEBYTYPE    = $00000002;

  {$EXTERNALSYM MIXER_GETLINECONTROLSF_QUERYMASK}
  MIXER_GETLINECONTROLSF_QUERYMASK    = $0000000F;

type

  PMIXERCONTROLDETAILS = ^tMIXERCONTROLDETAILS;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS}
  tMIXERCONTROLDETAILS = record

    cbStruct: DWORD;                   { size in bytes of MIXERCONTROLDETAILS }
    dwControlID: DWORD;                { control id to get/set details on }
    cChannels: DWORD;                  { number of channels in paDetails array }

    case Integer of
      0: (hwndOwner: HWND;             { for MIXER_SETCONTROLDETAILSF_CUSTOM }
          cbDetails: DWORD;            { size of _one_ details_XX struct }
          paDetails: Pointer);         { pointer to array of details_XX structs }

      1: (cMultipleItems: DWORD);      { if _MULTIPLE, the number of items per channel }
  end;
  {$EXTERNALSYM tMIXERCONTROLDETAILS}
  MIXERCONTROLDETAILS = tMIXERCONTROLDETAILS;
  {$EXTERNALSYM MIXERCONTROLDETAILS}
  LPMIXERCONTROLDETAILS = ^tMIXERCONTROLDETAILS;
  {$EXTERNALSYM PMIXERCONTROLDETAILS}


  {  MIXER_GETCONTROLDETAILSF_LISTTEXT }

type

  PMIXERCONTROLDETAILS_LISTTEXTA = ^tagMIXERCONTROLDETAILS_LISTTEXTA;
  {$EXTERNALSYM PMIXERCONTROLDETAILS_LISTTEXTA}
  tagMIXERCONTROLDETAILS_LISTTEXTA = record
    dwParam1: DWORD;
    dwParam2: DWORD;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of AnsiChar;
  end;
  {$EXTERNALSYM tagMIXERCONTROLDETAILS_LISTTEXTA}
  MIXERCONTROLDETAILS_LISTTEXTA = tagMIXERCONTROLDETAILS_LISTTEXTA;
  {$EXTERNALSYM MIXERCONTROLDETAILS_LISTTEXTA}
  LPMIXERCONTROLDETAILS_LISTTEXTA = ^tagMIXERCONTROLDETAILS_LISTTEXTA;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS_LISTTEXTA}

  PMIXERCONTROLDETAILS_LISTTEXTW = ^tagMIXERCONTROLDETAILS_LISTTEXTW;
  {$EXTERNALSYM PMIXERCONTROLDETAILS_LISTTEXTW}
  tagMIXERCONTROLDETAILS_LISTTEXTW = record
    dwParam1: DWORD;
    dwParam2: DWORD;
    szName: array[0..MIXER_LONG_NAME_CHARS - 1] of WideChar;
  end;
  {$EXTERNALSYM tagMIXERCONTROLDETAILS_LISTTEXTW}
  MIXERCONTROLDETAILS_LISTTEXTW = tagMIXERCONTROLDETAILS_LISTTEXTW;
  {$EXTERNALSYM MIXERCONTROLDETAILS_LISTTEXTW}
  LPMIXERCONTROLDETAILS_LISTTEXTW = ^tagMIXERCONTROLDETAILS_LISTTEXTW;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS_LISTTEXTW}

  { Delphi }
  MIXERCONTROLDETAILS_LISTTEXT = tagMIXERCONTROLDETAILS_LISTTEXTW;
  PMIXERCONTROLDETAILS_LISTTEXT = ^tagMIXERCONTROLDETAILS_LISTTEXTW;
  LPMIXERCONTROLDETAILS_LISTTEXT = ^tagMIXERCONTROLDETAILS_LISTTEXTW;


  { MIXER_GETCONTROLDETAILSF_VALUE }

type

  PMIXERCONTROLDETAILS_BOOLEAN = ^tMIXERCONTROLDETAILS_BOOLEAN;
  {$EXTERNALSYM PMIXERCONTROLDETAILS_BOOLEAN}
  tMIXERCONTROLDETAILS_BOOLEAN = record
    fValue: LONG;
  end;
  {$EXTERNALSYM tMIXERCONTROLDETAILS_BOOLEAN}
  MIXERCONTROLDETAILS_BOOLEAN = tMIXERCONTROLDETAILS_BOOLEAN;
  {$EXTERNALSYM MIXERCONTROLDETAILS_BOOLEAN}
  LPMIXERCONTROLDETAILS_BOOLEAN = ^tMIXERCONTROLDETAILS_BOOLEAN;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS_BOOLEAN}


  PMIXERCONTROLDETAILS_SIGNED = ^tMIXERCONTROLDETAILS_SIGNED;
  {$EXTERNALSYM PMIXERCONTROLDETAILS_SIGNED}
  tMIXERCONTROLDETAILS_SIGNED = record
    lValue: LONG;
  end;
  {$EXTERNALSYM tMIXERCONTROLDETAILS_SIGNED}
  MIXERCONTROLDETAILS_SIGNED = tMIXERCONTROLDETAILS_SIGNED;
  {$EXTERNALSYM MIXERCONTROLDETAILS_SIGNED}
  LPMIXERCONTROLDETAILS_SIGNED = ^tMIXERCONTROLDETAILS_SIGNED;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS_SIGNED}


  PMIXERCONTROLDETAILS_UNSIGNED = ^tMIXERCONTROLDETAILS_UNSIGNED;
  {$EXTERNALSYM PMIXERCONTROLDETAILS_UNSIGNED}
  tMIXERCONTROLDETAILS_UNSIGNED = record
    dwValue: DWORD;
  end;
  {$EXTERNALSYM tMIXERCONTROLDETAILS_UNSIGNED}
  MIXERCONTROLDETAILS_UNSIGNED = tMIXERCONTROLDETAILS_UNSIGNED;
  {$EXTERNALSYM MIXERCONTROLDETAILS_UNSIGNED}
  LPMIXERCONTROLDETAILS_UNSIGNED = ^tMIXERCONTROLDETAILS_UNSIGNED;
  {$EXTERNALSYM LPMIXERCONTROLDETAILS_UNSIGNED}


  function mixerGetControlDetailsA({_In_opt_} hmxobj: HMIXEROBJ;
                                   {_Inout_} pmxcd: LPMIXERCONTROLDETAILS;
                                   fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetControlDetailsA}

  function mixerGetControlDetailsW({_In_opt_} hmxobj: HMIXEROBJ;
                                   {_Inout_} pmxcd: LPMIXERCONTROLDETAILS;
                                   fdwDetails: DWORD): MMRESULT; stdcall;
  {$EXTERNALSYM mixerGetControlDetailsW}

  { Delphi }
  function mixerGetControlDetails({_In_opt_} hmxobj: HMIXEROBJ;
                                  {_Inout_} pmxcd: LPMIXERCONTROLDETAILS;
                                  fdwDetails: DWORD): MMRESULT; stdcall;

const

  {$EXTERNALSYM MIXER_GETCONTROLDETAILSF_VALUE}
  MIXER_GETCONTROLDETAILSF_VALUE      = $00000000;
  {$EXTERNALSYM MIXER_GETCONTROLDETAILSF_LISTTEXT}
  MIXER_GETCONTROLDETAILSF_LISTTEXT   = $00000001;

  {$EXTERNALSYM MIXER_GETCONTROLDETAILSF_QUERYMASK}
  MIXER_GETCONTROLDETAILSF_QUERYMASK  = $0000000F;


  function mixerSetControlDetails({_In_opt_} hmxobj: HMIXEROBJ;
                                  pmxcd: LPMIXERCONTROLDETAILS;
                                  fdwDetails: DWORD): MMRESULT; stdcall;

const

  {$EXTERNALSYM MIXER_SETCONTROLDETAILSF_VALUE}
  MIXER_SETCONTROLDETAILSF_VALUE      = $00000000;
  {$EXTERNALSYM MIXER_SETCONTROLDETAILSF_CUSTOM}
  MIXER_SETCONTROLDETAILSF_CUSTOM     = $00000001;

  {$EXTERNALSYM MIXER_SETCONTROLDETAILSF_QUERYMASK}
  MIXER_SETCONTROLDETAILSF_QUERYMASK  = $0000000F;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  MMeApiLib = 'winmm.dll';

function MEVT_EVENTTYPE(x: DWORD): DWORD;
begin
  Result := ((byte(x) shr 24) and $FF);
end;

function MEVT_EVENTPARM(x: DWORD): DWORD;
begin
  Result := DWORD(x) and $00FFFFFF;
end;

function waveOutGetNumDevs;  external MMeApiLib name 'waveOutGetNumDevs';

function waveOutGetDevCapsA; external MMeApiLib name 'auxGetDevCapsA';
function waveOutGetDevCapsW; external MMeApiLib name 'auxGetDevCapsW';
function waveOutGetDevCaps;  external MMeApiLib name 'auxGetDevCapsW';

function waveOutGetVolume;   external MMeApiLib name 'waveOutGetVolume';
function waveOutSetVolume;   external MMeApiLib name 'waveOutSetVolume';

function waveOutGetErrorTextA;  external MMeApiLib name 'waveOutGetErrorTextA';
function waveOutGetErrorTextW;  external MMeApiLib name 'waveOutGetErrorTextW';
function waveOutGetErrorText;   external MMeApiLib name 'waveOutGetErrorTextW';

function waveOutOpen;   external MMeApiLib name 'waveOutOpen';
function waveOutClose;  external MMeApiLib name 'waveOutClose';

function waveOutPrepareHeader;   external MMeApiLib name 'waveOutPrepareHeader';
function waveOutUnprepareHeader; external MMeApiLib name 'waveOutUnprepareHeader';

function waveOutWrite;           external MMeApiLib name 'waveOutWrite';
function waveOutPause;           external MMeApiLib name 'waveOutRestart';
function waveOutRestart;         external MMeApiLib name 'waveOutRestart';
function waveOutReset;           external MMeApiLib name 'waveOutReset';
function waveOutBreakLoop;       external MMeApiLib name 'waveOutBreakLoop';
function waveOutGetPosition;     external MMeApiLib name 'waveOutGetPosition';
function waveOutGetPitch;        external MMeApiLib name 'waveOutGetPitch';
function waveOutSetPitch;        external MMeApiLib name 'waveOutSetPitch';
function waveOutGetPlaybackRate; external MMeApiLib name 'waveOutGetPlaybackRate';
function waveOutSetPlaybackRate; external MMeApiLib name 'waveOutSetPlaybackRate';

function waveOutGetID;       external MMeApiLib name 'waveOutGetID';
function waveOutMessage;     external MMeApiLib name 'waveOutMessage';
function waveInGetNumDevs;   external MMeApiLib name 'waveInGetNumDevs';

function waveInGetDevCapsA;   external MMeApiLib name 'waveInGetDevCapsA';
function waveInGetDevCapsW;   external MMeApiLib name 'waveInGetDevCapsW';
function waveInGetDevCaps;    external MMeApiLib name 'waveInGetDevCapsW';

function waveInGetErrorTextA;   external MMeApiLib name 'waveInGetErrorTextA';
function waveInGetErrorTextW;   external MMeApiLib name 'waveInGetErrorTextW';
function waveInGetErrorText;    external MMeApiLib name 'waveInGetErrorTextW';

function waveInOpen;    external MMeApiLib name 'waveInOpen';
function waveInClose;   external MMeApiLib name 'waveInClose';

function waveInPrepareHeader;   external MMeApiLib name 'waveInPrepareHeader';
function waveInUnprepareHeader; external MMeApiLib name 'waveInUnprepareHeader';
function waveInAddBuffer;       external MMeApiLib name 'waveInAddBuffer';

function waveInStart;       external MMeApiLib name 'waveInStart';
function waveInStop;        external MMeApiLib name 'waveInStop';
function waveInReset;       external MMeApiLib name 'waveInReset';
function waveInGetPosition; external MMeApiLib name 'waveInGetPosition';
function waveInGetID;       external MMeApiLib name 'waveInGetID';
function waveInMessage;     external MMeApiLib name 'waveInMessage';

function midiOutGetNumDevs;   external MMeApiLib name 'midiOutGetNumDevs';
function midiStreamOpen;      external MMeApiLib name 'midiStreamOpen';
function midiStreamClose;     external MMeApiLib name 'midiStreamClose';
function midiStreamProperty;  external MMeApiLib name 'midiStreamProperty';
function midiStreamPosition;  external MMeApiLib name 'midiStreamPosition';
function midiStreamOut;       external MMeApiLib name 'midiStreamOut';
function midiStreamPause;     external MMeApiLib name 'midiStreamPause';
function midiStreamRestart;   external MMeApiLib name 'midiStreamRestart';
function midiStreamStop;      external MMeApiLib name 'midiStreamStop';
function midiConnect;         external MMeApiLib name 'midiConnect';
function midiDisconnect;      external MMeApiLib name 'midiDisconnect';

function midiOutGetDevCapsA;   external MMeApiLib name 'midiOutGetDevCapsA';
function midiOutGetDevCapsW;   external MMeApiLib name 'midiOutGetDevCapsW';
function midiOutGetDevCaps;    external MMeApiLib name 'midiOutGetDevCapsW';

function midiOutGetVolume;   external MMeApiLib name 'midiOutGetVolume';
function midiOutSetVolume;   external MMeApiLib name 'midiOutSetVolume';

function midiOutGetErrorTextA;   external MMeApiLib name 'midiOutGetErrorTextA';
function midiOutGetErrorTextW;   external MMeApiLib name 'midiOutGetErrorTextW';
function midiOutGetErrorText;    external MMeApiLib name 'midiOutGetErrorTextW';

function midiOutOpen;             external MMeApiLib name 'midiOutOpen';
function midiOutClose;            external MMeApiLib name 'midiOutClose';
function midiOutPrepareHeader;    external MMeApiLib name 'midiOutPrepareHeader';
function midiOutUnprepareHeader;  external MMeApiLib name 'midiOutUnprepareHeader';
function midiOutShortMsg;         external MMeApiLib name 'midiOutShortMsg';
function midiOutLongMsg;          external MMeApiLib name 'midiOutLongMsg';
function midiOutReset;            external MMeApiLib name 'midiOutReset';
function midiOutCachePatches;     external MMeApiLib name 'midiOutCachePatches';
function midiOutCacheDrumPatches; external MMeApiLib name 'midiOutCacheDrumPatches';
function midiOutGetID;            external MMeApiLib name 'midiOutGetID';
function midiOutMessage;          external MMeApiLib name 'midiOutMessage';
function midiInGetNumDevs;        external MMeApiLib name 'midiInGetNumDevs';

function midiInGetDevCapsA;   external MMeApiLib name 'midiInGetDevCapsA';
function midiInGetDevCapsW;   external MMeApiLib name 'midiInGetDevCapsW';
function midiInGetDevCaps;    external MMeApiLib name 'midiInGetDevCapsW';

function midiInGetErrorTextA;   external MMeApiLib name 'midiInGetErrorTextA';
function midiInGetErrorTextW;   external MMeApiLib name 'midiInGetErrorTextW';
function midiInGetErrorText;    external MMeApiLib name 'midiInGetErrorTextW';

function midiInOpen;            external MMeApiLib name 'midiInOpen';
function midiInClose;           external MMeApiLib name 'midiInClose';
function midiInPrepareHeader;   external MMeApiLib name 'midiInPrepareHeader';
function midiInUnprepareHeader; external MMeApiLib name 'midiInUnprepareHeader';
function midiInAddBuffer;       external MMeApiLib name 'midiInAddBuffer';

function midiInStart;   external MMeApiLib name 'midiInStart';
function midiInStop;    external MMeApiLib name 'midiInStop';
function midiInReset;   external MMeApiLib name 'midiInReset';
function midiInGetID;   external MMeApiLib name 'midiInGetID';
function midiInMessage; external MMeApiLib name 'midiInMessage';

function auxGetNumDevs;   external MMeApiLib name 'x';

function auxGetDevCapsA;  external MMeApiLib name 'auxGetDevCapsA';
function auxGetDevCapsW;  external MMeApiLib name 'auxGetDevCapsW';
function auxGetDevCaps;   external MMeApiLib name 'auxGetDevCapsW';

function auxSetVolume;   external MMeApiLib name 'auxSetVolume';
function auxGetVolume;   external MMeApiLib name 'auxGetVolume';
function auxOutMessage;  external MMeApiLib name 'auxOutMessage';

function mixerGetNumDevs;   external MMeApiLib name 'mixerGetNumDevs';
function mixerGetDevCapsA;  external MMeApiLib name 'mixerGetDevCapsA';
function mixerGetDevCapsW;  external MMeApiLib name 'mixerGetDevCapsW';
function mixerGetDevCaps;   external MMeApiLib name 'mixerGetDevCapsW';

function mixerOpen;     external MMeApiLib name 'mixerOpen';
function mixerClose;    external MMeApiLib name 'mixerClose';
function mixerMessage;  external MMeApiLib name 'mixerMessage';

function mixerGetLineInfoA;  external MMeApiLib name 'mixerGetLineInfoA';
function mixerGetLineInfoW;  external MMeApiLib name 'mixerGetLineInfoW';
function mixerGetLineInfo;   external MMeApiLib name 'mixerGetLineInfoW';

function mixerGetID;   external MMeApiLib name 'mixerGetID';

function mixerGetLineControlsA;  external MMeApiLib name 'mixerGetLineControlsA';
function mixerGetLineControlsW;  external MMeApiLib name 'mixerGetLineControlsW';
function mixerGetLineControls;   external MMeApiLib name 'mixerGetLineControlsW';

function mixerGetControlDetailsA;  external MMeApiLib name 'mixerGetControlDetailsA';
function mixerGetControlDetailsW;  external MMeApiLib name 'mixerGetControlDetailsW';
function mixerGetControlDetails;   external MMeApiLib name 'mixerGetControlDetailsW';

function mixerSetControlDetails;   external MMeApiLib name 'mixerSetControlDetails';

  // Implement Additional Prototypes here.

end.
