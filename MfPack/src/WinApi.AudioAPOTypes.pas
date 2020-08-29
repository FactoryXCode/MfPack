// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Remote Desktop
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AudioAPOTypes.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: This header is used by Remote Desktop Services.
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
// Source: audioapotypes.h
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
unit WinApi.AudioAPOTypes;

  {$HPPEMIT '#include "audioapotypes.h"'}

interface


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type

// Validation flags for APO_CONNECTION_PROPERTY. Each APO connection has
// an APO_CONNECTION_PROPERTY structure associated with it. The buffer
// for each connection may be either invalid, valid, or silent.

  _tagAPO_BUFFER_FLAGS = (
    // BUFFER_INVALID means that there is no valid data in  the connection
    // buffer. The buffer pointer will still be valid and capable of holding
    // the amount of valid audio data specified in the APO_CONNECTION_PROPERTY.
    // The processor will mark every connection BUFFER_INVALID before running
    // the IAudioOutputEndpoint::GetOutputDataPointer,
    // IAudioInputEndpointRT::GetInputDataPointer, or
    // IAudioProcessingObjectRT::APOProcess each time its IAudioProcess::Process
    // routine is called.
    BUFFER_INVALID = 0,
    // Connection buffer has valid data. This is the "normal" operational
    // state of the connection buffer. An APO will set this flag once it
    // writes valid data into a buffer.
    BUFFER_VALID   = 1,
    // The connection buffer should be treated as if it contains silence.
    // APOs will mark their output connection buffers as silent (instead
    // of writing silence into the buffer) if they generate a buffer of
    // silence. This typically only happens when the buffer(s) going in
    // are marked BUFFER_SILENT.
    BUFFER_SILENT  = 2
  );
  {$EXTERNALSYM _tagAPO_BUFFER_FLAGS}
  APO_BUFFER_FLAGS = _tagAPO_BUFFER_FLAGS;
  {$EXTERNALSYM APO_BUFFER_FLAGS}

// This structure contains the dynamically changing connection properties.
// The connection between APOs ends up resolving to the APO_CONNECTION_PROPERTY
// structure for the IAudioProcessingObjectRT::APOProcess call.  This structure
// is passed in IAudioInputEndpointRT.GetInputDataPointer and
// IAudioOutputEndpointRT.ReleaseOutputDataPointer.

  APO_CONNECTION_PROPERTY = record
    // The connection buffer. APOs use this buffer to read and write
    // audio data.
    //
    // Alignment required
    // (128 bit or frame aligned)
    //            |
    //      +-----+
    //      V
    //      +-------------------------------------------------------------+
    //      |                                                             |
    //      |                                                             |
    //      |                     audio buffer                            |
    //      |                                                             |
    //      |                                                             |
    //      +-------------------------------------------------------------+
    //      ^
    //      |
    //   pBuffer
    //
    pBuffer: UIntPtr; //UINT_PTR;
    // Number of valid frames in the connection buffer. This must
    // be less than or equal to APO_CONNECTION_DESCRIPTOR.u32MaxFrameCount.
    // An APO will use the valid frame count to determine how much data to
    // process on an input buffer. An APO will set the valid frame count
    // upon writing data into its output connection(s).
    u32ValidFrameCount: UINT32;
    // Connection flags for this buffer.  Tells APOs if the buffer is valid,
    // in valid, or silent. See APO_BUFFER_FLAGS.
    u32BufferFlags: APO_BUFFER_FLAGS;
    // A tag identifying a valid APO_CONNECTION_PROPERTY structure.
    u32Signature: UINT32;
  end;
  {$EXTERNALSYM APO_CONNECTION_PROPERTY}
  PAPO_CONNECTION_PROPERTY = ^APO_CONNECTION_PROPERTY;

  // reintroduced from KsMedia.h
{$IFNDEF _AUDIO_CURVE_TYPE_DEFINED}
  PAudioCurveType = ^AudioCurveType;
  PAUDIO_CURVE_TYPE = ^AUDIO_CURVE_TYPE;
  AUDIO_CURVE_TYPE                = (
    AUDIO_CURVE_TYPE_NONE         = 0,
    AUDIO_CURVE_TYPE_WINDOWS_FADE = 1
  );
  {$EXTERNALSYM AUDIO_CURVE_TYPE}
  AudioCurveType = AUDIO_CURVE_TYPE;
{$DEFINE _AUDIO_CURVE_TYPE_DEFINED}
{$ENDIF}

  // Additional Prototypes for ALL interfaces

  // Used by IAudioProcessingObjectRT
  {$NODEFINE TApoConnectionPropertyArray}
  TApoConnectionPropertyArray = array [0..65535] of APO_CONNECTION_PROPERTY;

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
