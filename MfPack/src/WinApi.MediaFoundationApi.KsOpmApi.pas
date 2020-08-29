// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.KsOpmApi.pas
// Kind: Pascal / Delphi unit
// Release date: 28-07-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Definitions for kernel mode code OPM communication.
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
// Source: ksopmapi.h
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
unit WinApi.MediaFoundationApi.KsOpmApi;

  {$HPPEMIT '#include "ksopmapi.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


const

  //=============================================================================
  // Description:
  //
  // Ks Property set to use with AVStram drivers
  // KSPROPSETID_OPMVideoOutput {06F414BB-F43A-4fe2-A566-774B4C81F0DB}
  KSPROPSETID_OPMVideoOutput : TGUID = '{06F414BB-F43A-4fe2-A566-774B4C81F0DB}';

type

  PKsmethodOpmvideooutput = ^KSMETHOD_OPMVIDEOOUTPUT;
  KSMETHOD_OPMVIDEOOUTPUT                        = (
    //  Output is OPM_RANDOM_NUMBER followed by certifiate
    KSMETHOD_OPMVIDEOOUTPUT_STARTINITIALIZATION  = 0,
    //  Input OPM_ENCRYPTED_INITIALIZATION_PARAMETERS
    //  Output OPM_STANDARD_INFORMATION
    KSMETHOD_OPMVIDEOOUTPUT_FINISHINITIALIZATION = 1,
    //  Input is OPM_GET_INFO_PARAMETERS, output is OPM_REQUESTED_INFORMATION
    //  Use KsMethod - both input and output in the buffer (not after the KSMETHOD structure)
    KSMETHOD_OPMVIDEOOUTPUT_GETINFORMATION       = 2
  );
  {$EXTERNALSYM KSMETHOD_OPMVIDEOOUTPUT}

const

  //  Currently on this GetInformation call is supported
  PM_GET_CODEC_INFO  : TGUID = (D1: $4f374491;
                                D2: $8f5f;
                                D3: $4445;
                                D4: ($9d, $ba, $95, $58, $8f, $6b, $58, $b4));

type

  POPM_RANDOM_NUMBER = ^_OPM_RANDOM_NUMBER;
  {$EXTERNALSYM POPM_RANDOM_NUMBER}
  _OPM_RANDOM_NUMBER = record
    abRandomNumber: array[0..15] of Byte;
  end;
  {$EXTERNALSYM _OPM_RANDOM_NUMBER}
  OPM_RANDOM_NUMBER = _OPM_RANDOM_NUMBER;
  {$EXTERNALSYM OPM_RANDOM_NUMBER}


const

  {enum}
  OPM_OMAC_SIZE                                  = 16;
  OPM_128_BIT_RANDOM_NUMBER_SIZE                 = 16;
  OPM_ENCRYPTED_INITIALIZATION_PARAMETERS_SIZE   = 256;
  OPM_CONFIGURE_SETTING_DATA_SIZE                = 4056;
  OPM_GET_INFORMATION_PARAMETERS_SIZE            = 4056;
  OPM_REQUESTED_INFORMATION_SIZE                 = 4076;
  OPM_HDCP_KEY_SELECTION_VECTOR_SIZE             = 5;
  OPM_PROTECTION_TYPE_SIZE                       = 4;
  OPM_BUS_TYPE_MASK                              = $FFFF;
  OPM_BUS_IMPLEMENTATION_MODIFIER_MASK           = $7FFF;

type

  POPM_OMAC = ^OPM_OMAC;
  _OPM_OMAC = record
    abOMAC: array[0..OPM_OMAC_SIZE - 1] of Byte;
  end;
  {$EXTERNALSYM _OPM_OMAC}
  OPM_OMAC = _OPM_OMAC;
  {$EXTERNALSYM OPM_OMAC}


  POPM_GET_INFO_PARAMETERS = ^OPM_GET_INFO_PARAMETERS;
  _OPM_GET_INFO_PARAMETERS = record
    omac: OPM_OMAC;
    rnRandomNumber: OPM_RANDOM_NUMBER;
    guidInformation: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..OPM_GET_INFORMATION_PARAMETERS_SIZE - 1] of Byte;
  end;
  {$EXTERNALSYM _OPM_GET_INFO_PARAMETERS}
  OPM_GET_INFO_PARAMETERS = _OPM_GET_INFO_PARAMETERS;
  {$EXTERNALSYM OPM_GET_INFO_PARAMETERS}


  POPM_REQUESTED_INFORMATION = ^OPM_REQUESTED_INFORMATION;
  _OPM_REQUESTED_INFORMATION = record
    omac: OPM_OMAC;
    cbRequestedInformationSize: ULONG;
    abRequestedInformation: array[0..OPM_REQUESTED_INFORMATION_SIZE - 1] of Byte;
  end;
  {$EXTERNALSYM _OPM_REQUESTED_INFORMATION}
  OPM_REQUESTED_INFORMATION = _OPM_REQUESTED_INFORMATION;
  {$EXTERNALSYM OPM_REQUESTED_INFORMATION}


  POPM_ENCRYPTED_INITIALIZATION_PARAMETERS = ^OPM_ENCRYPTED_INITIALIZATION_PARAMETERS;
  _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = record
    abEncryptedInitializationParameters: array[0..OPM_ENCRYPTED_INITIALIZATION_PARAMETERS_SIZE - 1] of Byte;
  end;
  {$EXTERNALSYM _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS}
  OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS;
  {$EXTERNALSYM OPM_ENCRYPTED_INITIALIZATION_PARAMETERS}


  POPM_STANDARD_INFORMATION =^OPM_STANDARD_INFORMATION;
  _OPM_STANDARD_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulInformation: ULONG;
    ulReserved: ULONG;
    ulReserved2: ULONG;
  end;
  {$EXTERNALSYM _OPM_STANDARD_INFORMATION}
  OPM_STANDARD_INFORMATION = _OPM_STANDARD_INFORMATION;
  {$EXTERNALSYM OPM_STANDARD_INFORMATION}


  POPM_GET_CODEC_INFO_PARAMETERS = ^OPM_GET_CODEC_INFO_PARAMETERS;
  _OPM_GET_CODEC_INFO_PARAMETERS = record
    cbVerifier: DWORD;
    Verifier: array[0..OPM_GET_INFORMATION_PARAMETERS_SIZE - 4 - 1] of Byte;  // Class ID of MFT or symbolic link for AVStream
                                                                              // drivers
  end;
  {$EXTERNALSYM _OPM_GET_CODEC_INFO_PARAMETERS}
  OPM_GET_CODEC_INFO_PARAMETERS = _OPM_GET_CODEC_INFO_PARAMETERS;
  {$EXTERNALSYM OPM_GET_CODEC_INFO_PARAMETERS}


  POPM_GET_CODEC_INFO_INFORMATION = ^OPM_GET_CODEC_INFO_INFORMATION;
  _OPM_GET_CODEC_INFO_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    Merit: DWORD;                   // Merit assigned to the codec
  end;
  {$EXTERNALSYM _OPM_GET_CODEC_INFO_INFORMATION}
  OPM_GET_CODEC_INFO_INFORMATION = _OPM_GET_CODEC_INFO_INFORMATION;
  {$EXTERNALSYM OPM_GET_CODEC_INFO_INFORMATION}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes


implementation

  // Implement Additional Prototypes here.

end.
