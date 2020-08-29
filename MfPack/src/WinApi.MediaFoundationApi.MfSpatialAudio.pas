// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfSpatialAudio.pas
// Kind: Pascal / Delphi unit
// Release date: 29-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: SpatialAudioClient API interface definition.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 RedStone (rs) 1 or later.
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
// Source: mfspatialaudio.h
//
// Copyright (c) 1997-2019 Microsoft Corporation. All rights reserved
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
unit WinApi.MediaFoundationApi.MfSpatialAudio;

  {$HPPEMIT '#include "mfspatialaudio.h"'}

interface

uses

  {MfPack}
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.CoreAudioApi.SpatialAudioMetadata,
  WinApi.CoreAudioApi.SpatialAudioClient;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  // IMFSpatialAudioObjectBuffer interface
  // =====================================
  //
  // <summary>
  //     The IMFSpatialAudioObjectBuffer interface represents a section of audio data with
  //     associated positional and rendering information metadata. Spatial audio
  //     objects are stored in IMFSpatialAudioSample instances, and allow passing of
  //     spatial audio information between Media Foundation components.
  //
  //	   To get the audio data contained in the spatial audio object, use the IMFMediaBuffer
  //	   Lock() and Unlock() methods.
  // </summary>
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSpatialAudioObjectBuffer);'}
  {$EXTERNALSYM IMFSpatialAudioObjectBuffer}
  IMFSpatialAudioObjectBuffer = interface(IMFMediaBuffer)
  ['{d396ec8c-605e-4249-978d-72ad1c312872}']

    function SetID(u32ID: INT32): HRESULT; stdcall;
    // <summary>
    //     The SetID() method sets the ID of the spatial audio object.
    // </summary>
    // <param name="u32ID">
    //     A 32-bit unsigned unique ID of the audio object.
    // </param>
    // <remarks>
    //     The object ID must be unique for each spatial audio sample.  Subsequent samples can
    //     contain spatial audio objects with the same IDs to represent moving objects or constant
    //     static objects (speaker channels).
    // </remarks>

    function GetID(out pu32ID: UINT32): HRESULT; stdcall;
    // <summary>
    //     The GetID() method returns 32-bit unique, unsigned ID of the spatial audio object.
    //     If SetID() method was not called, this method returns the invalid object ID -1
    //     ($ffffffff).  The invalid ID indicates that the object buffer is unused and
    //     contains invalid data.
    // </summary>
    // <param name="pu32ID">
    //     Pointer to a 32-bit variable where the object ID will be stored.
    // </param>

    function SetType(_type: AudioObjectType): HRESULT; stdcall;
    // <summary>
    //     The SetType() method sets the type of audio object.
    // </summary>
    // <param name="_type">
    //     A value from the AudioObjectType enumeration, specifying the type of the audio object.
    // </param>
    // <remarks>
    //     A spatial audio object can represent a dynamic moving object with changing coordinates
    //     (AudioObjectType_Dynamic), or it can be a pre-defined speaker channel such as
    //     AudioObjectType_FrontLeft.
    // </remarks>

    function GetType(out pType: AudioObjectType): HRESULT; stdcall;
    // <summary>
    //     The GetType() method returns the audio object type of the spatial audio object.  If
    //     SetType() method was not called, this method returns a default value of
    //     AudioObjectType_None.
    // </summary>
    // <param name="pType">
    //     Pointer to an AudioObjectType variable where the audio object type will be stored.
    // </param>

    function GetMetadataItems(out ppMetadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // <summary>
    //     The GetMetadataItems() method retrieves a pointer to a buffer that may
    //     contain metadata.  The metadata is written to the ISpatialAudioMetadtaItems
    //     collection in a format identified by the MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_ID
    //     media type attribute specified during media type negotiation phase of MF
    //     topology construction.
    // </summary>
    // <param name="ppMetadataItems">
    //     Pointer to a ISpatialAudioMetadataItems object which will contain a collection
    //     of metadata metadata items.
    // </param>

  end;
  IID_IMFSpatialAudioObjectBuffer = IMFSpatialAudioObjectBuffer;
  {$EXTERNALSYM IID_IMFSpatialAudioObjectBuffer}

  // Interface IMFSpatialAudioSample
  // ===============================
  //
  // <summary>
  //     The IMFSpatialAudioSample interface represents a multimedia sample
  //     with spatial audio information.  Each spatial audio sample contains one
  //     or more IMFSpatialAudioObjectBuffer objects.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSpatialAudioSample);'}
  {$EXTERNALSYM IMFSpatialAudioSample}
  IMFSpatialAudioSample = interface(IMFSample)
  ['{abf28a9B-3393-4290-ba79-5ffc46d986b2}']

    function GetObjectCount(out pdwObjectCount: DWORD): HRESULT; stdcall;
    // <summary>
    //     The GetObjectCount method returns the number of spatial audio objects in the
    //     sample.
    // </summary>
    // <param name="pdwObjectCount">
    //     Pointer to a 32 bit variable where the total number of audio objects in the
    //     sample will be stored.
    // </param>

    function AddSpatialAudioObject(pAudioObjBuffer: IMFSpatialAudioObjectBuffer): HRESULT; stdcall;
    // <summary>
    //     The AddSpatialAudioObject method adds a new spatial audio object to the spatial
    //     sample.
    // </summary>
    // <param name="pAudioObjBuffer">
    //     Pointer to the new IMFSpatialAudioObject.
    // </param>

    function GetSpatialAudioObjectByIndex(const dwIndex: DWORD;
                                          out ppAudioObjBuffer: IMFSpatialAudioObjectBuffer): HRESULT; stdcall;
    // <summary>
    //     The GetSpatialAudioObjectByIndex() method returns an audio object specified
    //     with the passed-in index.
    // </summary>
    // <param name="dwIndex">
    //     A 32 bit variable with the 0-based index of the audio object requested.
    // </param>
    // <param name="ppSpatialAudioObjectBuffer">
    //     The out param where a pointer to the IMFSpatialAudioObjectBuffer with the
    //     specified index will be stored.
    // </param>

  end;
  IID_IMFSpatialAudioSample = IMFSpatialAudioSample;
  {$EXTERNALSYM IID_IMFSpatialAudioSample}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
