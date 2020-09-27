// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.PlaySoundApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-playsound-l1-1-0
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
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
// Source: evntprov.h
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
unit WinApi.WinMM.PlaySoundApi;

interface

uses
  WinApi.WinApiTypes,
  WinApi.WinMM.MMSysCom;


   {***************************************************************************

                            Sound support

    ***************************************************************************}


  function sndPlaySound({_In_opt_} pszSound: LPCWSTR;
                         fuSound: UINT): BOOL; stdcall;

  function sndPlaySoundA({_In_opt_} pszSound: LPCSTR;
                         fuSound: UINT): BOOL; stdcall;

  { UniCode }
  function sndPlaySoundW({_In_opt_} pszSound: LPCWSTR;
                         fuSound: UINT): BOOL; stdcall;


  {
   *  flag values for fuSound and fdwSound arguments on [snd]PlaySound
  }

const

  SND_SYNC                            = $0000 { play synchronously (default)};
  {$EXTERNALSYM SND_SYNC}
  SND_ASYNC                           = $0001 { play asynchronously };
  {$EXTERNALSYM SND_ASYNC}
  SND_NODEFAULT                       = $0002 { silence (! default)if sound not found };
  {$EXTERNALSYM SND_NODEFAULT}
  SND_MEMORY                          = $0004 { pszSound points to a memory file };
  {$EXTERNALSYM SND_MEMORY}
  SND_LOOP                            = $0008 { loop the sound until next sndPlaySound };
  {$EXTERNALSYM SND_LOOP}
  SND_NOSTOP                          = $0010 { don ' t stop any currently playing sound };
  {$EXTERNALSYM SND_NOSTOP}

  SND_NOWAIT                          = $00002000 { don ' t wait if the driver is busy };
  {$EXTERNALSYM SND_NOWAIT}
  SND_ALIAS                           = $00010000 { name is a registry alias };
  {$EXTERNALSYM SND_ALIAS}
  SND_ALIAS_ID                        = $00110000 { alias is a predefined ID };
  {$EXTERNALSYM SND_ALIAS_ID}
  SND_FILENAME                        = $00020000 { name is file name };
  {$EXTERNALSYM SND_FILENAME}
  SND_RESOURCE                        = $00040004 { name is resource name or atom };
  {$EXTERNALSYM SND_RESOURCE}

  SND_PURGE                           = $0040 { purge non - static events for task };
  {$EXTERNALSYM SND_PURGE}
  SND_APPLICATION                     = $0080 { look for application specific association };
  {$EXTERNALSYM SND_APPLICATION}

  SND_SENTRY                          = $00080000 { Generate a SoundSentry event with this sound };
  {$EXTERNALSYM SND_SENTRY}
  SND_RING                            = $00100000 { Treat this as a 'ring' from a communications app - don ' t duck me };
  {$EXTERNALSYM SND_RING}
  SND_SYSTEM                          = $00200000 { Treat this as a system sound };
  {$EXTERNALSYM SND_SYSTEM}

  SND_ALIAS_START                     = 0 { alias base };
  {$EXTERNALSYM SND_ALIAS_START}

  function sndAlias(ch0: Char; ch1: Char): DWord; inline;
  {$EXTERNALSYM sndAlias}

  // This needs a work-around, because we can't use (macro) sndAlias
  // example: SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('*')) shl 8))
  //
const
  {$EXTERNALSYM SND_ALIAS_SYSTEMASTERISK}
  SND_ALIAS_SYSTEMASTERISK            = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('*')) shl 8)); // sndAlias('S', '*')
  {$EXTERNALSYM SND_ALIAS_SYSTEMQUESTION}
  SND_ALIAS_SYSTEMQUESTION            = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('?')) shl 8)); // sndAlias('S', '?')
  {$EXTERNALSYM SND_ALIAS_SYSTEMHAND}
  SND_ALIAS_SYSTEMHAND                = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('H')) shl 8)); // sndAlias('S', 'H')
  {$EXTERNALSYM SND_ALIAS_SYSTEMEXIT}
  SND_ALIAS_SYSTEMEXIT                = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('E')) shl 8)); // sndAlias('S', 'E')
  {$EXTERNALSYM SND_ALIAS_SYSTEMSTART}
  SND_ALIAS_SYSTEMSTART               = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('S')) shl 8)); // sndAlias('S', 'S')
  {$EXTERNALSYM SND_ALIAS_SYSTEMWELCOME}
  SND_ALIAS_SYSTEMWELCOME             = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('W')) shl 8)); // sndAlias('S', 'W')
  {$EXTERNALSYM SND_ALIAS_SYSTEMEXCLAMATION}
  SND_ALIAS_SYSTEMEXCLAMATION         = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('!')) shl 8)); // sndAlias('S', '!')
  {$EXTERNALSYM SND_ALIAS_SYSTEMDEFAULT}
  SND_ALIAS_SYSTEMDEFAULT             = SND_ALIAS_START + (DWORD(Ord('S')) or (DWORD(Ord('D')) shl 8)); // sndAlias('S', 'D')


  // use unicode by default
  function PlaySound({_In_opt_} pszSound: LPCWSTR;
                     {_In_opt_} hmod: HMODULE;
                     fdwSound: DWORD): BOOL; stdcall;

  function PlaySoundA({_In_opt_} pszSound: LPCSTR;
                      {_In_opt_} hmod: HMODULE;
                      fdwSound: DWORD): BOOL; stdcall;

  { UniCode }
  function PlaySoundW({_In_opt_} pszSound: LPCWSTR;
                      {_In_opt_} hmod: HMODULE;
                      fdwSound: DWORD): BOOL; stdcall;


  // Additional Prototypes for ALL interfaces


  // end of Additional Prototypes

implementation

const
  PlaySoundApiLib = 'Winmm.dll';


function sndAlias(ch0: Char;
                  ch1: Char): DWord;
begin
  Result := (SND_ALIAS_START + (DWORD(Ord(ch0)) or (DWORD(Ord(ch1)) shl 8)));
end;

function sndPlaySound; external PlaySoundApiLib name 'sndPlaySoundW';
function sndPlaySoundA; external PlaySoundApiLib name 'sndPlaySoundA';
function sndPlaySoundW; external PlaySoundApiLib name 'sndPlaySoundW';
function PlaySound; external PlaySoundApiLib name 'PlaySoundW';
function PlaySoundA; external PlaySoundApiLib name 'PlaySoundA';
function PlaySoundW; external PlaySoundApiLib name 'PlaySoundW';

  // Implement Additional Prototypes here.

end.
