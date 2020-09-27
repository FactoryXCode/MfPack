// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MCIAvi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Multimedia Systems Media Control Interface
//              AVI driver external header file
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
// Remarks: Depends on MMSYSTEM.H and WINDOWS.h
//        	Version:	1.00
//        	Date:		16-JUL-1992
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
// Source: mciavi.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//
//   THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
//   KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
//   IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
//   PURPOSE.
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
unit WinApi.WinMM.MCIAvi;

interface

uses
  WinApi.WinMM.MCIApi;

  //
  // These three flags apply to the 'play' command:
  //	play <alias> window		Play in normal window
  //	play <alias> fullscreen		Play in 320x240 full-screen mode
  //	play <alias> fullscreen by 2	Play fullscreen, zoomed by 2
  //

const

  MCI_MCIAVI_PLAY_WINDOW              = $01000000;
  {$EXTERNALSYM MCI_MCIAVI_PLAY_WINDOW}
  MCI_MCIAVI_PLAY_FULLSCREEN          = $02000000;
  {$EXTERNALSYM MCI_MCIAVI_PLAY_FULLSCREEN}
  MCI_MCIAVI_PLAY_FULLBY2             = $04000000;
  {$EXTERNALSYM MCI_MCIAVI_PLAY_FULLBY2}

  //
  // Debugging constants for AVI diagnostics
  //
  //
  // Returns number of frames not drawn during last play.  If this number
  // is more than a small fraction of the number of frames that should have
  // been displayed, things aren't looking good.
  //

  MCI_AVI_STATUS_FRAMES_SKIPPED       = $8001;
  {$EXTERNALSYM MCI_AVI_STATUS_FRAMES_SKIPPED}

  //
  // Returns a number representing how well the last AVI play worked.
  // A result of 1000 indicates that the AVI sequence took the amount
  // of time to play that it should have; a result of 2000, for instance,
  // would indicate that a 5-second AVI sequence took 10 seconds to play,
  // implying that the audio and video were badly broken up.
  //

  MCI_AVI_STATUS_LAST_PLAY_SPEED      = $8002;
  {$EXTERNALSYM MCI_AVI_STATUS_LAST_PLAY_SPEED}

  //
  // Returns the number of times that the audio definitely broke up.
  // (We count one for every time we're about to write some audio data
  // to the driver, and we notice that it's already played all of the
  // data we have.
  //

  MCI_AVI_STATUS_AUDIO_BREAKS         = $8003;
  {$EXTERNALSYM MCI_AVI_STATUS_AUDIO_BREAKS}

  MCI_AVI_SETVIDEO_DRAW_PROCEDURE     = $8000;
  {$EXTERNALSYM MCI_AVI_SETVIDEO_DRAW_PROCEDURE}

  MCI_AVI_SETVIDEO_PALETTE_COLOR      = $8100;
  {$EXTERNALSYM MCI_AVI_SETVIDEO_PALETTE_COLOR}


  //
  // This constant specifies that the "halftone" palette should be
  // used, rather than the default palette.
  //

  MCI_AVI_SETVIDEO_PALETTE_HALFTONE   = $0000FFFF;
  {$EXTERNALSYM MCI_AVI_SETVIDEO_PALETTE_HALFTONE}

  //
  //	Custom error return values
  //

  MCIERR_AVI_OLDAVIFORMAT             = (MCIERR_CUSTOM_DRIVER_BASE + 100);
  {$EXTERNALSYM MCIERR_AVI_OLDAVIFORMAT}
  MCIERR_AVI_NOTINTERLEAVED           = (MCIERR_CUSTOM_DRIVER_BASE + 101);
  {$EXTERNALSYM MCIERR_AVI_NOTINTERLEAVED}
  MCIERR_AVI_NODISPDIB                = (MCIERR_CUSTOM_DRIVER_BASE + 102);
  {$EXTERNALSYM MCIERR_AVI_NODISPDIB}
  MCIERR_AVI_CANTPLAYFULLSCREEN       = (MCIERR_CUSTOM_DRIVER_BASE + 103);
  {$EXTERNALSYM MCIERR_AVI_CANTPLAYFULLSCREEN}
  MCIERR_AVI_TOOBIGFORVGA             = (MCIERR_CUSTOM_DRIVER_BASE + 104);
  {$EXTERNALSYM MCIERR_AVI_TOOBIGFORVGA}
  MCIERR_AVI_NOCOMPRESSOR             = (MCIERR_CUSTOM_DRIVER_BASE + 105);
  {$EXTERNALSYM MCIERR_AVI_NOCOMPRESSOR}
  MCIERR_AVI_DISPLAYERROR             = (MCIERR_CUSTOM_DRIVER_BASE + 106);
  {$EXTERNALSYM MCIERR_AVI_DISPLAYERROR}
  MCIERR_AVI_AUDIOERROR               = (MCIERR_CUSTOM_DRIVER_BASE + 107);
  {$EXTERNALSYM MCIERR_AVI_AUDIOERROR}
  MCIERR_AVI_BADPALETTE               = (MCIERR_CUSTOM_DRIVER_BASE + 108);
  {$EXTERNALSYM MCIERR_AVI_BADPALETTE}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
