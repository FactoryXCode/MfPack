// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MFPack - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.HrtfApoApi.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 2.6.4
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: hrtfapoapi.h
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
unit MfPack.HrtfApoApi;

  {$HPPEMIT '#include "hrtfapoapi.h"'}

interface

uses

  {MfPack}
  MfPack.MfpTypes,
  MfPack.XApo;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}

const

  FLT_MAX = 3.402823466e+38; // float.h
  {$EXTERNALSYM FLT_MAX}

  HRTF_MAX_GAIN_LIMIT                 = 12.0;
  {$EXTERNALSYM HRTF_MAX_GAIN_LIMIT}
  HRTF_MIN_GAIN_LIMIT                 = -96.0;
  {$EXTERNALSYM HRTF_MIN_GAIN_LIMIT}
  HRTF_MIN_UNITY_GAIN_DISTANCE        = 0.5;
  {$EXTERNALSYM HRTF_MIN_UNITY_GAIN_DISTANCE}
  HRTF_DEFAULT_UNITY_GAIN_DISTANCE    = 1.0;
  {$EXTERNALSYM HRTF_DEFAULT_UNITY_GAIN_DISTANCE}
  HRTF_DEFAULT_CUTOFF_DISTANCE        = FLT_MAX;
  {$EXTERNALSYM HRTF_DEFAULT_CUTOFF_DISTANCE}

type
  //! Represents a position in 3D space, using a right-handed coordinate system.
  PHrtfPosition = ^HrtfPosition;
  HrtfPosition = record
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
  end;
  {$EXTERNALSYM HrtfPosition}

  //! Indicates the orientation of an HRTF directivity object. This is a row-major 3x3 rotation matrix.
  PHrtfOrientation = ^HrtfOrientation;
  HrtfOrientation = record
    element: array[0..8] of FLOAT;
  end;
  {$EXTERNALSYM HrtfOrientation}

  //! Indicates one of several stock directivity patterns.
  PHrtfDirectivityType = ^HrtfDirectivityType;
  HrtfDirectivityType = (
                         //! The sound emission is in all directions.
                         OmniDirectional = 0,
                         //! The sound emission is a cardiod shape.
                         Cardioid,
                         //! The sound emission is a cone.
                         Cone);
  {$EXTERNALSYM HrtfDirectivityType}

  //! Indicates one of several stock environment types.
  PHrtfEnvironment = ^HrtfEnvironment;
  HrtfEnvironment = (
                     //! A small room.
                     Small = 0,
                     //! A medium-sized room.
                     Medium,
                     //! A large enclosed space.
                     Large,
                     //! An outdoor space.
                     Outdoors);
  {$EXTERNALSYM HrtfEnvironment}

  //
  //! Base directivity pattern descriptor. Describes the type of directivity applied to a sound.
  //! The scaling parameter is used to interpolate between directivity behavior and omnidirectional;
  //  it determines how much attenuation is applied to the source outside of the directivity pattern and
  //  controls how directional the source is.
  //
  PHrtfDirectivity = ^HrtfDirectivity;
  HrtfDirectivity = record
    //! Indicates the type of directivity.
    _type: HrtfDirectivityType;
    //! A normalized value between zero and one.
    //  Specifies the amount of linear interpolation between omnidirectional sound and the full directivity pattern,
    //  where 0 is fully omnidirectional and 1 is fully directional.
    scaling: FLOAT;
  end;
  {$EXTERNALSYM HrtfDirectivity}

  //! Describes a cardioid directivity pattern.
  PHrtfDirectivityCardioid = ^HrtfDirectivityCardioid;
  HrtfDirectivityCardioid = record
    //! Descriptor for the cardioid pattern. The type parameter must be set to HrtfDirectivityType.Cardioid.
    directivity: HrtfDirectivity;
    //! Order controls the shape of the cardioid. The higher order the shape, the narrower it is.
    //  Must be greater than 0 and less than or equal to 32.
    order: Single;
  end;
  {$EXTERNALSYM HrtfDirectivityCardioid}

  //
  //! Describes a cone directivity.
  //! Attenuation is 0 inside the inner cone.
  //! Attenuation is linearly interpolated between the inner cone, which is defined by innerAngle,
  //  and the outer cone, which is defined by outerAngle.
  //
  PHrtfDirectivityCone = ^HrtfDirectivityCone;
  HrtfDirectivityCone = record
    //! Descriptor for the cone pattern. The type parameter must be set to HrtfDirectivityType.Cone.
    directivity: HrtfDirectivity;
    //! Angle, in radians, that defines the inner cone. Must be between 0 and 2 * pi.
    innerAngle: FLOAT;
    //! Angle, in radians, that defines the outer cone. Must be between 0 and 2 * pi.
    outerAngle: FLOAT;
  end;
  {$EXTERNALSYM HrtfDirectivityCone}

  //
  //! Indicates a distance-based decay type applied to a sound.
  //
  PHrtfDistanceDecayType = ^HrtfDistanceDecayType;
  HrtfDistanceDecayType = (
                           //! Simulates natural decay with distance, as constrained by minimum and
                           //  maximum gain distance limits. Drops to silence at rolloff distance.
                           NaturalDecay = 0,
                           //! Used to set up a custom gain curve, within the maximum and minimum gain limit.
                           CustomDecay);
  {$EXTERNALSYM HrtfDistanceDecayType}

  //
  //! Describes a distance-based decay behavior.
  //
  PHrtfDistanceDecay = ^HrtfDistanceDecay;
  HrtfDistanceDecay = record
    //! The type of decay behavior, natural or custom.
    _type: HrtfDistanceDecayType;
    //! The maximum gain limit applied at any distance. Applies to both natural and custom decay.
    //  This value is specified in dB, with a range from -96 to 12 inclusive. The default value is 12 dB.
    maxGain: FLOAT;
    //! The minimum gain limit applied at any distance. Applies to both natural and custom decay.
    //  This value is specified in dB, with a range from -96 to 12 inclusive. The default value is -96 dB.
    minGain: FLOAT;
    //! The distance at which the gain is 0dB. Applies to natural decay only.
    //  This value is specified in meters, with a range from 0.05 to infinity (FLT_MAX). The default value is 1 meter.
    unityGainDistance: FLOAT;
    //! The distance at which output is silent. Applies to natural decay only.
    //  This value is specified in meters, with a range from zero (non-inclusive) to infinity (FLT_MAX).
    //  The default value is infinity.
    cutoffDistance: FLOAT;
  end;
  {$EXTERNALSYM HrtfDistanceDecay}

  //
  //! Specifies parameters used to initialize HRTF.
  //!
  //! Instances of the XAPO interface are created by using the CreateHrtfApo() API:
  //!   ```STDAPI CreateHrtfApo(_In_ const HrtfApoInit* pInit, _Outptr_ IXAPO** ppXapo);```
  //!
  //
  PHrtfApoInit = ^HrtfApoInit;
  HrtfApoInit = record
    //! The decay type. If you pass in nullptr, the default value will be used. The default is natural decay.
    distanceDecay: HrtfDistanceDecay;
    //! The directivity type. If you pass in nullptr, the default value will be used. The default directivity is omni-directional.
    directivity: HrtfDirectivity;
  end;
  {$EXTERNALSYM HrtfApoInit}

  //! Creates an instance of the XAPO object.
  //! Format requirements:
  //! * Input: mono, 48 kHz, 32-bit float PCM.
  //! * Output: stereo, 48 kHz, 32-bit float PCM.
  //! Audio is processed in blocks of 1024 samples.
  //! Returns:
  //!     S_OK for success, any other value indicates failure.
  //!     Returns E_NOTIMPL on unsupported platforms.
  function CreateHrtfApo(init: HrtfApoInit; //! Pointer to an HrtfApoInit struct. Specifies parameters for XAPO interface initialization.
                         out xApo: IXAPO): HRESULT; stdcall;
  {$EXTERNALSYM CreateHrtfApo}


type

  // Interface IXAPOHrtfParameters
  // =============================
  // The interface used to set parameters that control how HRTF is applied to a sound.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IXAPOHrtfParameters);'}
  {$EXTERNALSYM IXAPOHrtfParameters}
  IXAPOHrtfParameters = interface(IUnknown)
  ['{15B3CD66-E9DE-4464-B6E6-2BC3CF63D455}']

    // HRTF params
    //! The position of the sound relative to the listener.
    function SetSourcePosition(position: HrtfPosition): HRESULT; stdcall;

    //! The rotation matrix for the source orientation, with respect to the listener's frame of reference (the listener's coordinate system).
    function SetSourceOrientation(orientation: HrtfOrientation): HRESULT; stdcall;

    //! The custom direct path gain value for the current source position. Valid only for sounds played with the HrtfDistanceDecayType. Custom decay type.
    function SetSourceGain(gain: FLOAT): HRESULT; stdcall;

    // Distance cue params
    //! Selects the acoustic environment to simulate.
    function SetEnvironment(environment: HrtfEnvironment): HRESULT; stdcall;

  end;
  IID_IXAPOHrtfParameters = IXAPOHrtfParameters;
  {$EXTERNALSYM IID_IXAPOHrtfParameters}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  HrtfApoLib = 'HrtfApo.dll';


{$WARN SYMBOL_PLATFORM OFF}
  function CreateHrtfApo; external HrtfApoLib name 'CreateHrtfApo' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

// Implement Additional Prototypes here.

end.
