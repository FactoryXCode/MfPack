// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MPEGError.pas
// Kind: Pascal / Delphi unit
// Release date: 16-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Interface specific HRESULT error codes for MPEG-2 tables.
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
// Source: MPEGError.h
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
unit WinApi.MPEGError;

interface

uses
  WinApi.WinError;

  // Interface specific SUCCESS and ERROR macros
  function MAKE_S_ITF_HRESULT(x: Cardinal): HResult;
  {$EXTERNALSYM MAKE_S_ITF_HRESULT}

  function MAKE_E_ITF_HRESULT(x: Cardinal): HResult;
  {$EXTERNALSYM MAKE_E_ITF_HRESULT}

const
  // MPEG-2 base HRESULT code (must be at least 0x200)
  MPEG2_BASE = $200;

  // Have to work around, because a macro can't be assigned to a const
  // The macro MAKE_HRESULT is defined in WinError.pas
  // HRESULT MAKE_HRESULT(sev: WORD;  fac: WORD;  code: WORD): HResult;
  // So, what we do here is
  // MAKE_S_ITFHRESULT( code )  MAKE_HRESULT( SEVERITY_ERROR, MPEG2_BASE, code )
  //              SEVERITY_ERROR             MPEG2_BASE         code = 0
  MAKE_S_ITFHRESULT = (0 shl 31) OR (MPEG2_BASE shl 16);
  MAKE_E_ITFHRESULT = (1 shl 31) OR (MPEG2_BASE shl 16);


  // MPEG-2 Success HRESULTs

  MPEG2_S_MORE_DATA_AVAILABLE            = MAKE_S_ITFHRESULT OR $00;
  MPEG2_S_NO_MORE_DATA_AVAILABLE         = MAKE_S_ITFHRESULT OR $01;
  MPEG2_S_SG_INFO_FOUND                  = MAKE_S_ITFHRESULT OR $02;
  MPEG2_S_SG_INFO_NOT_FOUND              = MAKE_S_ITFHRESULT OR $03;
  MPEG2_S_MPE_INFO_FOUND                 = MAKE_S_ITFHRESULT OR $04;
  MPEG2_S_MPE_INFO_NOT_FOUND             = MAKE_S_ITFHRESULT OR $05;
  MPEG2_S_NEW_MODULE_VERSION             = MAKE_S_ITFHRESULT OR $06;

  // MPEG-2 Error HRESULTs
  MPEG2_E_UNINITIALIZED                  = MAKE_E_ITFHRESULT OR $00;
  MPEG2_E_ALREADY_INITIALIZED            = MAKE_E_ITFHRESULT OR $01;
  MPEG2_E_OUT_OF_BOUNDS                  = MAKE_E_ITFHRESULT OR $02;
  MPEG2_E_MALFORMED_TABLE                = MAKE_E_ITFHRESULT OR $03;
  MPEG2_E_UNDEFINED                      = MAKE_E_ITFHRESULT OR $04;
  MPEG2_E_NOT_PRESENT                    = MAKE_E_ITFHRESULT OR $05;
  MPEG2_E_SECTION_NOT_FOUND              = MAKE_E_ITFHRESULT OR $06;
  MPEG2_E_TX_STREAM_UNAVAILABLE          = MAKE_E_ITFHRESULT OR $07;
  MPEG2_E_SERVICE_ID_NOT_FOUND           = MAKE_E_ITFHRESULT OR $08;
  MPEG2_E_SERVICE_PMT_NOT_FOUND          = MAKE_E_ITFHRESULT OR $09;
  MPEG2_E_DSI_NOT_FOUND                  = MAKE_E_ITFHRESULT OR $0A;
  MPEG2_E_SERVER_UNAVAILABLE             = MAKE_E_ITFHRESULT OR $0B;
  MPEG2_E_INVALID_CAROUSEL_ID            = MAKE_E_ITFHRESULT OR $0C;
  MPEG2_E_MALFORMED_DSMCC_MESSAGE        = MAKE_E_ITFHRESULT OR $0D;
  MPEG2_E_INVALID_SG_OBJECT_KIND         = MAKE_E_ITFHRESULT OR $0E;
  MPEG2_E_OBJECT_NOT_FOUND               = MAKE_E_ITFHRESULT OR $0F;
  MPEG2_E_OBJECT_KIND_NOT_A_DIRECTORY    = MAKE_E_ITFHRESULT OR $10;
  MPEG2_E_OBJECT_KIND_NOT_A_FILE         = MAKE_E_ITFHRESULT OR $11;
  MPEG2_E_FILE_OFFSET_TOO_BIG            = MAKE_E_ITFHRESULT OR $12;
  MPEG2_E_STREAM_STOPPED                 = MAKE_E_ITFHRESULT OR $13;
  MPEG2_E_REGISTRY_ACCESS_FAILED         = MAKE_E_ITFHRESULT OR $14;
  MPEG2_E_INVALID_UDP_PORT               = MAKE_E_ITFHRESULT OR $15;
  MPEG2_E_DATA_SOURCE_FAILED             = MAKE_E_ITFHRESULT OR $16;
  MPEG2_E_DII_NOT_FOUND                  = MAKE_E_ITFHRESULT OR $17;
  MPEG2_E_DSHOW_PIN_NOT_FOUND            = MAKE_E_ITFHRESULT OR $18;
  MPEG2_E_BUFFER_TOO_SMALL               = MAKE_E_ITFHRESULT OR $19;
  MPEG2_E_MISSING_SECTIONS               = MAKE_E_ITFHRESULT OR $1A;
  MPEG2_E_TOO_MANY_SECTIONS              = MAKE_E_ITFHRESULT OR $1B;
  MPEG2_E_NEXT_TABLE_OPS_NOT_AVAILABLE   = MAKE_E_ITFHRESULT OR $1C;
  MPEG2_E_INCORRECT_DESCRIPTOR_TAG       = MAKE_E_ITFHRESULT OR $1D;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

function MAKE_S_ITF_HRESULT(x: Cardinal): HResult;
begin
  Result := MAKE_HRESULT(SEVERITY_SUCCESS,
                         FACILITY_ITF,
                         x);
end;

function MAKE_E_ITF_HRESULT(x: Cardinal): HResult;
begin
  Result := MAKE_HRESULT(SEVERITY_ERROR,
                         FACILITY_ITF,
                         x);
end;

  // Implement Additional functions here.

end.
