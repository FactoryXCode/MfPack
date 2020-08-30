// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D2D1
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.D2DErr.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Helper files over the D2D interfaces and APIs.
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
// Source: D2DErr.h
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
unit WinApi.DirectX.D2DErr;

  {$HPPEMIT '#include "D2DErr.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError;

const

  //+---------------------------------------------------------------------------
  //
  // D2D error codes
  //
  //----------------------------------------------------------------------------

  //
  //  Error codes shared with WINCODECS
  //

  //
  // The pixel format is not supported.
  //
  D2DERR_UNSUPPORTED_PIXEL_FORMAT     = WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT;
  {$EXTERNALSYM D2DERR_UNSUPPORTED_PIXEL_FORMAT}

  //
  // Error codes that were already returned in prior versions and were part of the
  // MIL facility.

  //
  // Error codes mapped from WIN32 where there isn't already another HRESULT based
  // define
  //

  //
  // The supplied buffer was too small to accommodate the data.
  //
  D2DERR_INSUFFICIENT_BUFFER          = HRESULT(ERROR_INSUFFICIENT_BUFFER);
  {$EXTERNALSYM D2DERR_INSUFFICIENT_BUFFER}


  //
  // The file specified was not found.
  //
  D2DERR_FILE_NOT_FOUND               = HRESULT(ERROR_FILE_NOT_FOUND);
  {$EXTERNALSYM D2DERR_FILE_NOT_FOUND}

  //
  // D2D specific codes now live in winerror.h / MfPack.WinError.pas
  //

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
