// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.SpatialAudioHrtf.pas
// Kind: Pascal / Delphi unit
// Release date: 29-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: Shared part of the Core Audio Interfaces
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
// Remarks: Requires Windows 10 RedStone 1 or later.
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
// Source: SpatialAudioHrtf.h
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
unit WinApi.CoreAudioApi.SpatialAudioHrtf;

  {$HPPEMIT '#include "SpatialAudioHrtf.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  WinApi.WinMM.MMReg,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes,
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

  PSpatialAudioHrtfDirectivityType = ^SpatialAudioHrtfDirectivityType;
  SpatialAudioHrtfDirectivityType               = (
    SpatialAudioHrtfDirectivity_OmniDirectional = 0,  // The sound emission is in all directions.
    SpatialAudioHrtfDirectivity_Cardioid,             // The sound emission is a cardiod shape.
    SpatialAudioHrtfDirectivity_Cone                  // The sound emission is a cone.
  );
  {$EXTERNALSYM SpatialAudioHrtfDirectivityType}

  PSpatialAudioHrtfEnvironmentType = ^SpatialAudioHrtfEnvironmentType;
  SpatialAudioHrtfEnvironmentType     = (
    SpatialAudioHrtfEnvironment_Small = 0,  // A small room.
    SpatialAudioHrtfEnvironment_Medium,     // A medium-sized room.
    SpatialAudioHrtfEnvironment_Large,      // A large enclosed space.
    SpatialAudioHrtfEnvironment_Outdoors,   // An outdoor space.
    SpatialAudioHrtfEnvironment_Average     // Reserved. Do not use.
  );
  {$EXTERNALSYM SpatialAudioHrtfEnvironmentType}

  PSpatialAudioHrtfDistanceDecayType = ^SpatialAudioHrtfDistanceDecayType;
  SpatialAudioHrtfDistanceDecayType            = (
    SpatialAudioHrtfDistanceDecay_NaturalDecay = 0,  // Simulates natural decay with distance, as constrained by minimum and maximum gain distance limits. Drops to silence at rolloff distance.
    SpatialAudioHrtfDistanceDecay_CustomDecay        // Used to set up a custom gain curve, within the maximum and minimum gain limit.
  );
  {$EXTERNALSYM SpatialAudioHrtfDistanceDecayType}

//#pragma pack(push, 1)
{$ALIGN 1}

  PSpatialAudioHrtfDirectivity = ^SpatialAudioHrtfDirectivity;
  SpatialAudioHrtfDirectivity = record
    _Type: SpatialAudioHrtfDirectivityType;  // Indicates the type of directivity.
    Scaling: FLOAT;                          // A normalized value between zero and one. Specifies the amount of linear interpolation between omnidirectional sound and the full directivity pattern, where 0 is fully omnidirectional and 1 is fully directional.
  end;
  {$EXTERNALSYM SpatialAudioHrtfDirectivity}

  PSpatialAudioHrtfDirectivityCardioid = ^SpatialAudioHrtfDirectivityCardioid;
  SpatialAudioHrtfDirectivityCardioid = record
    directivity: SpatialAudioHrtfDirectivity;
    Order: FLOAT;
  end;
  {$EXTERNALSYM SpatialAudioHrtfDirectivityCardioid}

  PSpatialAudioHrtfDirectivityCone = ^SpatialAudioHrtfDirectivityCone;
  SpatialAudioHrtfDirectivityCone = record
    directivity: SpatialAudioHrtfDirectivity;
    InnerAngle: FLOAT;
    OuterAngle: FLOAT;
  end;
  {$EXTERNALSYM SpatialAudioHrtfDirectivityCone}

  PSpatialAudioHrtfDirectivityUnion = ^SpatialAudioHrtfDirectivityUnion;
  SpatialAudioHrtfDirectivityUnion = record
    case integer of
      0: (Cone: SpatialAudioHrtfDirectivityCone);
      1: (Cardiod: SpatialAudioHrtfDirectivityCardioid);
      2: (Omni: SpatialAudioHrtfDirectivity);
  end;
  {$EXTERNALSYM SpatialAudioHrtfDirectivityUnion}

  PSpatialAudioHrtfDistanceDecay = ^SpatialAudioHrtfDistanceDecay;
  SpatialAudioHrtfDistanceDecay = record
    _Type: SpatialAudioHrtfDistanceDecayType; // The Type of decay behavior, natural or custom.
    MaxGain: FLOAT;                           // The maximum gain limit applied at any distance. Applies to both natural and custom decay. This value is specified in dB, with a range from -96 to 12 inclusive. The default value is 12 dB.
    MinGain: FLOAT;                           // The minimum gain limit applied at any distance. Applies to both natural and custom decay. This value is specified in dB, with a range from -96 to 12 inclusive. The default value is -96 dB.
    UnityGainDistance: FLOAT;                 // The distance at which the gain is 0dB. Applies to natural decay only. This value is specified in meters, with a range from 0.05 to infinity (FLT_MAX). The default value is 1 meter.
    CutoffDistance: FLOAT;                    // The distance at which output is silent. Applies to natural decay only. This value is specified in meters, with a range from zero (non-inclusive) to infinity (FLT_MAX). The default value is INFINITY.
  end;
  {$EXTERNALSYM SpatialAudioHrtfDistanceDecay}


  SpatialAudioHrtfOrientation = array[0..8] of Single;
  {$EXTERNALSYM SpatialAudioHrtfOrientation}
  // Indicates the orientation of an HRTF directivity object. This is a row-major 3x3 rotation matrix.

  PSpatialAudioHrtfActivationParams = ^SpatialAudioHrtfActivationParams;
  SpatialAudioHrtfActivationParams = record
    ObjectFormat: WAVEFORMATEX;                            // Format descriptor for a single spatial audio objects. All objects must have the same format and must be of type WAVEFORMATEX or WAVEFORMATEXTENSIBLE.
    StaticObjectTypeMask: AudioObjectType;                  // (static channel bed mask) mask of static audio object type that are allowed
    MinDynamicObjectCount: UINT32;                          // Minimum number of dynamic audio objects. If at least this count cannot be granted, no dynamic objects will be granted.
    MaxDynamicObjectCount: UINT32;                          // Maximum number of dynamic audio objects that can be activated via ISpatialAudioObjectRenderStreamForMetadata.
    Category: AUDIO_STREAM_CATEGORY;                        // Specifies the category of an audio stream and its spatial audio objects.
    EventHandle: THandle;                                   // event that will signal the need for more audio data. This handle will be duplicated internally before getting used
    NotifyObject: ISpatialAudioObjectRenderStreamNotify;    // Notification sink (can be nullptr)
    DistanceDecay: SpatialAudioHrtfDistanceDecay;           // Optional Distance Decay Settings - All dynamic objects from this stream will default to this setting  (nullptr if unused)
    Directivity: SpatialAudioHrtfDirectivityUnion;          // Optional Directivity - All dynamic objects from this stream will default to this value   (nullptr if unused)
    Environment: SpatialAudioHrtfEnvironmentType;           // Optional Environment - All dynamic objects from this stream will default to this value  (nullptr if unused)
    Orientation: SpatialAudioHrtfOrientation;               // Optional Orientation - All dynamic objects from this stream will default to this value  (nullptr if unused)
  end;
  {$EXTERNALSYM SpatialAudioHrtfActivationParams}

//#pragma pack(pop) set back to initial alignment
  {$IFNDEF WIN32}
    {$ALIGN 8} // for Win64
  {$ENDIF}


  // Interface ISpatialAudioObjectForHrtf
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectForHrtf);'}
  {$EXTERNALSYM ISpatialAudioObjectForHrtf}
  ISpatialAudioObjectForHrtf = interface(ISpatialAudioObjectBase)
  ['{D7436ADE-1978-4E14-ABA0-555BD8EB83B4}']
    // The position of the sound relative to the listener.
    function SetPosition(x: FLOAT;
                         y: FLOAT;
                         z: FLOAT): HResult; stdcall;

    // The custom direct path gain value for the current source position. Valid only for sounds played with the SpatialAudioHrtfDistanceDecay_CustomDecay type.
    function SetGain(gain: FLOAT): HResult; stdcall;

    // The rotation matrix for the source orientation, with respect to the listener's frame of reference (the listener's coordinate system).
    function SetOrientation(orientation: SpatialAudioHrtfOrientation): HResult; stdcall;

    // Selects the acoustic environment to simulate.
    function SetEnvironment(environment: SpatialAudioHrtfEnvironmentType): HResult; stdcall;

    // Specifies the decay rate with respect to distance
    function SetDistanceDecay(distanceDecay: SpatialAudioHrtfDistanceDecay): HResult; stdcall;

    // Specifies the directional shape of the sound emitter, omni-directional, Cardioid, or cone
    function SetDirectivity(directivity: SpatialAudioHrtfDirectivityUnion): HResult; stdcall;
  end;
  IID_ISpatialAudioObjectForHrtf = ISpatialAudioObjectForHrtf;
  {$EXTERNALSYM IID_ISpatialAudioObjectForHrtf}


  // Interface ISpatialAudioObjectRenderStreamForHrtf
  // ================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectRenderStreamForHrtf);'}
  {$EXTERNALSYM ISpatialAudioObjectRenderStreamForHrtf}
  ISpatialAudioObjectRenderStreamForHrtf = interface(ISpatialAudioObjectRenderStreamBase)
  ['{E08DEEF9-5363-406E-9FDC-080EE247BBE0}']
    // Activation method for Microsoft Hrtf Spatial Audio Objects
    function ActivateSpatialAudioObjectForHrtf(_type: AudioObjectType;
                                               out audioObject: ISpatialAudioObjectForHrtf): HResult; stdcall;
  end;
  IID_ISpatialAudioObjectRenderStreamForHrtf = ISpatialAudioObjectRenderStreamForHrtf;
  {$EXTERNALSYM IID_ISpatialAudioObjectRenderStreamForHrtf}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
