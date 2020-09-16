// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Mpeg2Bits.pas
// Kind: Pascal / Delphi unit
// Release date: 16-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This file defines the MPEG-2 section header bitfields. These are
//              defined here instead of in mpegstructs.idl because of MIDL
//              compiler conflicts with bitfield definitions.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (TopPlay)
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
// Source: Mpeg2Bits.h
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
unit WinApi.Mpeg2Bits;

interface


//
// PID structure
//

type

  PPID_BITS_MIDL = ^PID_BITS_MIDL;
  PID_BITS_MIDL = record
    Bits: WORD;
  end;
  {$EXTERNALSYM PID_BITS_MIDL}

//
// Generic MPEG packet header structure
//

type

  PMPEG_HEADER_BITS_MIDL = ^MPEG_HEADER_BITS_MIDL;
  MPEG_HEADER_BITS_MIDL = record
    Bits: WORD;
  end;
  {$EXTERNALSYM MPEG_HEADER_BITS_MIDL}


//
// Long MPEG packet header structure
//

type

  PMPEG_HEADER_VERSION_BITS_MIDL = ^MPEG_HEADER_VERSION_BITS_MIDL;
  MPEG_HEADER_VERSION_BITS_MIDL = record
    Bits: Byte;
  end;
  {$EXTERNALSYM MPEG_HEADER_VERSION_BITS_MIDL}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
