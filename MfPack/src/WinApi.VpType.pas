// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.VpType.pas
// Kind: Pascal / Delphi unit
// Release date: 02-06-2016
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This file includes all the data structures defined for the IVPConfig
// interface.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
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
// Source: VPType.h
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
unit WinApi.VpType;

  {$HPPEMIT '#include "VPType.h"'}

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


type
  // enum to specify the criterion, which the vpmixer is supposed to use
  // in order to select the video format
  PAMVP_SELECT_FORMAT_BY = ^AMVP_SELECT_FORMAT_BY;
  _AMVP_SELECT_FORMAT_BY = (
    AMVP_DO_NOT_CARE          = 0,
    AMVP_BEST_BANDWIDTH       = 1,
    AMVP_INPUT_SAME_AS_OUTPUT = 2
  );
  {$EXTERNALSYM _AMVP_SELECT_FORMAT_BY}
  AMVP_SELECT_FORMAT_BY = _AMVP_SELECT_FORMAT_BY;
  {$EXTERNALSYM AMVP_SELECT_FORMAT_BY}

  // enum to specify the various mode
  PAMVP_MODE = ^AMVP_MODE;
  _AMVP_MODE = (
    AMVP_MODE_WEAVE             = 0,
    AMVP_MODE_BOBINTERLEAVED    = 1,
    AMVP_MODE_BOBNONINTERLEAVED = 2,
    AMVP_MODE_SKIPEVEN          = 3,
    AMVP_MODE_SKIPODD           = 4
  );
  {$EXTERNALSYM _AMVP_MODE}
  AMVP_MODE = _AMVP_MODE;
  {$EXTERNALSYM AMVP_MODE}

  // struct to specify the width and height. The context could be anything
  // such as scaling cropping etc.
  LPAMVPSIZE = ^_AMVPSIZE;
  _AMVPSIZE = record
    dwWidth: DWORD;                 // the width
    dwHeight: DWORD;                // the height
  end;
  {$EXTERNALSYM _AMVPSIZE}
  AMVPSIZE = _AMVPSIZE;
  {$EXTERNALSYM AMVPSIZE}

  // struct to specify the dimensional characteristics of the input stream
  LPAMVPDIMINFO = ^_AMVPDIMINFO;
  _AMVPDIMINFO = record
    dwFieldWidth: DWORD;             // Field height of the data
    dwFieldHeight: DWORD;            // Field width of the data
    dwVBIWidth: DWORD;               // Width of the VBI data
    dwVBIHeight: DWORD;              // Height of the VBI data
    rcValidRegion: TRECT;            // The vaild rectangle, used for cropping
  end;
  {$EXTERNALSYM _AMVPDIMINFO}
  AMVPDIMINFO = _AMVPDIMINFO;
  {$EXTERNALSYM AMVPDIMINFO}

  // struct to specify the various data specific characteristics of the input stream
  LPAMVPDATAINFO = ^_AMVPDATAINFO;
  _AMVPDATAINFO = record
    dwSize: DWORD;                   // Size of the struct
    dwMicrosecondsPerField: DWORD;   // Time taken by each field
    amvpDimInfo: AMVPDIMINFO;        // Dimensional Information
    dwPictAspectRatioX: DWORD;       // X dimension of Picture Aspect Ratio
    dwPictAspectRatioY: DWORD;       // Y dimension of Picture Aspect Ratio
    bEnableDoubleClock: BOOL;        // Videoport should enable double clocking
    bEnableVACT: BOOL;               // Videoport should use an external VACT signal
    bDataIsInterlaced: BOOL;         // Indicates that the signal is interlaced
    lHalfLinesOdd: LONG;             // number of halflines in the odd field
    bFieldPolarityInverted: BOOL;    // Device inverts the polarity by default
    dwNumLinesInVREF: DWORD;         // Number of lines of data in VREF
    lHalfLinesEven: LONG;            // number of halflines in the even field
    dwReserved1: DWORD;              // Reserved for future use
  end;
  {$EXTERNALSYM _AMVPDATAINFO}
  AMVPDATAINFO = _AMVPDATAINFO;
  {$EXTERNALSYM AMVPDATAINFO}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
