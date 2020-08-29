// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AudEvCod.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: List of Audio device error event codes and the expected params used by DirectShow.
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
// Related projects: - MfPackX265
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: audevcod.h
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
// 
//==============================================================================
unit WinApi.AudEvCod;

  {$HPPEMIT '#include "audevcod.h"'}

interface

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const
  EC_SND_DEVICE_ERROR_BASE       = $0200;
  {$EXTERNALSYM EC_SND_DEVICE_ERROR_BASE}

type
  PSNDDEV_ERR = ^SNDDEV_ERR;
  _tagSND_DEVICE_ERROR           = (
    SNDDEV_ERROR_Open            = 1,
    SNDDEV_ERROR_Close           = 2,
    SNDDEV_ERROR_GetCaps         = 3,
    SNDDEV_ERROR_PrepareHeader   = 4,
    SNDDEV_ERROR_UnprepareHeader = 5,
    SNDDEV_ERROR_Reset           = 6,
    SNDDEV_ERROR_Restart         = 7,
    SNDDEV_ERROR_GetPosition     = 8,
    SNDDEV_ERROR_Write           = 9,
    SNDDEV_ERROR_Pause           = 10,
    SNDDEV_ERROR_Stop            = 11,
    SNDDEV_ERROR_Start           = 12,
    SNDDEV_ERROR_AddBuffer       = 13,
    SNDDEV_ERROR_Query           = 14);
  {$EXTERNALSYM _tagSND_DEVICE_ERROR}
  SNDDEV_ERR = _tagSND_DEVICE_ERROR;
  {$EXTERNALSYM SNDDEV_ERR}


// Sound device error event codes
// ==============================
//
// All audio device error events are always passed on to the application, and are
// never processed by the filter graph

const

  EC_SNDDEV_IN_ERROR                  = (EC_SND_DEVICE_ERROR_BASE + $0);
  {$EXTERNALSYM EC_SNDDEV_IN_ERROR}
  EC_SNDDEV_OUT_ERROR                 = (EC_SND_DEVICE_ERROR_BASE + $1);
  {$EXTERNALSYM EC_SNDDEV_OUT_ERROR}

// Parameters: (DWORD, DWORD)
// lParam1 is an enum SND_DEVICE_ERROR which notifies the app how the device was
// being accessed when the failure occurred.
//
// lParam2 is the error returned from the sound device call.
//


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
