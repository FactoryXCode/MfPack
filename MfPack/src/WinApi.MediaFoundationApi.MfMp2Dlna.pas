// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMp2DlNa.pas
// Kind: Pascal / Delphi unit
// Release date: 12-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Initialize Media Foundation MPEG-2 sink
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8.1 or later.
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
// Source: mfmp2dlna.h
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
unit WinApi.MediaFoundationApi.MfMp2Dlna;

  {$HPPEMIT '#include "mfmp2dlna.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  CLSID_MPEG2DLNASink       : TGUID = '{fa5fe7c5-6a1d-4b11-b41f-f959d6c76500}';
  {$EXTERNALSYM CLSID_MPEG2DLNASink}
  // Class ID for MPEG-2 media sink object
  // fa5fe7c5-6a1d-4b11-b41f-f959d6c76500

  MF_MP2DLNA_USE_MMCSS      : TGUID = '{54f3e2ee-a2a2-497d-9834-973afde521eb}';
  {$EXTERNALSYM MF_MP2DLNA_USE_MMCSS}
  // Attribute to set realtime priority - UINT32 (BOOL)
  // 54f3e2ee-a2a2-497d-9834-973afde521eb

  MF_MP2DLNA_VIDEO_BIT_RATE : TGUID = '{E88548DE-73B4-42d7-9C75-ADFA0A2A6E4C}';
  {$EXTERNALSYM MF_MP2DLNA_VIDEO_BIT_RATE}
  // Target video bit rate - UINT32
  // E88548DE-73B4-42d7-9C75-ADFA0A2A6E4C

  MF_MP2DLNA_AUDIO_BIT_RATE : TGUID = '{2d1c070e-2b5f-4ab3-a7e6-8d943ba8d00a}';
  {$EXTERNALSYM MF_MP2DLNA_AUDIO_BIT_RATE}
  // Target audio bit rate - UINT32
  // 2d1c070e-2b5f-4ab3-a7e6-8d943ba8d00a

  MF_MP2DLNA_ENCODE_QUALITY : TGUID = '{b52379d7-1d46-4fb6-a317-a4a5f60959f8}';
  {$EXTERNALSYM MF_MP2DLNA_ENCODE_QUALITY}
  // Encode quality - 0 = fastest, 18 = highest quality - default 9
  // b52379d7-1d46-4fb6-a317-a4a5f60959f8

  MF_MP2DLNA_STATISTICS     : TGUID = '{75e488a3-d5ad-4898-85e0-bcce24a722d7}';
  {$EXTERNALSYM MF_MP2DLNA_STATISTICS}
  // Attribute to get stats - format MFMPEG2DLNASINKSTATS - type BLOB
  // 75e488a3-d5ad-4898-85e0-bcce24a722d7

type

  // INTERFACES ////////////////////////////////////////////////////////////////

  // Interface IMFDLNASinkInit
  // =========================
  // Initializes the Digital Living Network Alliance (DLNA) media sink.
  // The DLNA media sink exposes this interface.
  // To get a pointer to this interface, call CoCreateInstance. The CLSID is CLSID_MPEG2DLNASink.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDLNASinkInit);'}
  {$EXTERNALSYM IMFDLNASinkInit}
  IMFDLNASinkInit = interface(IUnknown)
	['{0c012799-1b61-4c10-bda9-04445be5f561}']

    function Initialize(pByteStream: IMFByteStream;
                        const fPal: BOOL): HResult; stdcall;
    // Initialize the MPEG2 DLNA sink
    // <param name = "pByteStream">
    //    Byte stream to write to.  Must be writable
    // <param name = "fPal">
    //    if TRUE video size and frame rate will be one of the accepted values for PAL
    //    if FALSE video size and frame frame will be one of the accepted values for NTSC

   end;
   // IMFDLNASinkInit
  IID_IMFDLNASinkInit = IMFDLNASinkInit;
  {$EXTERNALSYM IID_IMFDLNASinkInit}


  PMfmpeg2dlnasinkstats = ^MFMPEG2DLNASINKSTATS;
  _MFMPEG2DLNASINKSTATS = record
    cBytesWritten: DWORDLONG;
    fPAL: BOOL;
    fccVideo: DWORD;
    dwVideoWidth: DWORD;
    dwVideoHeight: DWORD;
    cVideoFramesReceived: DWORDLONG;
    cVideoFramesEncoded: DWORDLONG;
    cVideoFramesSkipped: DWORDLONG;
    cBlackVideoFramesEncoded: DWORDLONG;
    cVideoFramesDuplicated: DWORDLONG;
    cAudioSamplesPerSec: DWORD;
    cAudioChannels: DWORD;
    cAudioBytesReceived: DWORDLONG;
    cAudioFramesEncoded: DWORDLONG
  end;
  {$EXTERNALSYM _MFMPEG2DLNASINKSTATS}
  MFMPEG2DLNASINKSTATS = _MFMPEG2DLNASINKSTATS;
  {$EXTERNALSYM MFMPEG2DLNASINKSTATS}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
