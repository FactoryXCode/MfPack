// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MMSystem.pas
// Kind: Pascal / Delphi unit
// Release date: 11-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description:
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
// Remarks:  *  Define:         Prevent inclusion of:
//           *  --------------  ------------------------------------------------
//           *  MMNODRV         Installable driver support
//           *  MMNOSOUND       Sound support
//           *  MMNOWAVE        Waveform support
//           *  MMNOMIDI        MIDI support
//           *  MMNOAUX         Auxiliary audio support
//           *  MMNOMIXER       Mixer support
//           *  MMNOTIMER       Timer support
//           *  MMNOJOY         Joystick support
//           *  MMNOMCI         MCI support
//           *  MMNOMMIO        Multimedia file I/O support
//           *  MMNOMMSYSTEM    General MMSYSTEM functions
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
// Source: mmsystem.h, mmeapi.h
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
unit WinApi.WinMM.MMSystem;

interface

 {$HPPEMIT '#include "mmsystem.h"'}
 {$HPPEMIT '#include "mciapi.h"'}
 {$HPPEMIT '#include "mmiscapi.h"'}
 {$HPPEMIT '#include "mmiscapi2.h"'}
 {$HPPEMIT '#include "mmeapi.h"'}
 {$HPPEMIT '#include "timeapi.h"'}

uses
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}


  //****************************************************************************
  //
  //                  Multimedia Extensions Window Messages
  //
  //****************************************************************************

  { MMNOMCI         MCI support }
// #include <mciapi.h>

  { MMNODRV - Installable driver support }
//#include <mmiscapi.h>  >  WinApi.MMiscApi.pas
//#include <mmiscapi2.h> >  WinApi.MMiscApi2.pas

  { MMNOSOUND  Sound support }
//#include <playsoundapi.h> >  WinApi.WinMM.PlaySoundApi.pas

//#include <mmeapi.h>  >  WinApi.WinMM.MMeApi.pas



  //****************************************************************************
  //
  //                          Timer support
  //
  //****************************************************************************

  // #include <timeapi.h>  >  WinApi.TimeApi.pas

  //
  // Joystickapi API Set contract
  //
  // #include <joystickapi.h>  >  WinApi.WinMM.JoyStickApi.pas

  //****************************************************************************
  //
  //         DISPLAY Driver extensions
  //
  //****************************************************************************

const

  NEWTRANSPARENT = 3 { use with SetBkMode ()};
  {$EXTERNALSYM NEWTRANSPARENT}

  QUERYROPSUPPORT = 40 { use to determine ROP support };
  {$EXTERNALSYM QUERYROPSUPPORT}

{***************************************************************************

                        DIB Driver extensions

***************************************************************************}

  SELECTDIB = 41 { DIB . DRV select dib escape };
  {$EXTERNALSYM SELECTDIB}

  // To avoid type incompatibilities, this macro is translated to a function.
  function DIBINDEX(n: WORD): LONG;
  {$EXTERNALSYM DIBINDEX}


{***************************************************************************

                        ScreenSaver support

    The current application will receive a syscommand of SC_SCREENSAVE just
    before the screen saver is invoked.  If the app wishes to prevent a
    screen save, return non-zero value, otherwise call DefWindowProc().

***************************************************************************}

const

  SC_SCREENSAVE = $F140;
  {$EXTERNALSYM SC_SCREENSAVE}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation


function DIBINDEX(n: WORD): LONG;
begin
  Result := MAKELONG(n,
                     $10FF);
end;

  // Implement Additional Prototypes here.

end.
