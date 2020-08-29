// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
//
// Module: WinApi.DirectX.X3DAudio.pas
// Kind: Pascal / Delphi Unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: X3DAudio is an API used with XAudio2 to position sound in 3D space
//              to create the illusion of sound coming from a point in space
//              relative to the position of the camera.
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
// Remarks:
//
//             1.  Definition of terms:
//                     LFE: Low Frequency Effect -- always omnidirectional.
//                     LPF: Low Pass Filter, divided into two classifications:
//                          Direct -- Applied to the direct signal path,
//                                    used for obstruction/occlusion effects.
//                          Reverb -- Applied to the reverb signal path,
//                                    used for occlusion effects only.
// 
//             2.  Volume level is expressed as a linear amplitude scaler:
//                 1.0 represents no attenuation applied to the original signal,
//                 0.5 denotes an attenuation of 6dB, and 0.0 results in silence.
//                 Amplification (volume > 1.0) is also allowed, and is not clamped.
// 
//                 LPF values range from 1.0 representing all frequencies pass through,
//                 to 0.0 which results in silence as all frequencies are filtered out.
// 
//             3.  X3DAudio uses a left-handed Cartesian coordinate system with values
//                 on the x-axis increasing from left to right, on the y-axis from
//                 bottom to top, and on the z-axis from near to far.
//                 Azimuths are measured clockwise from a given reference direction.
// 
//                 Distance measurement is with respect to user-defined world units.
//                 Applications may provide coordinates using any system of measure
//                 as all non-normalized calculations are scale invariant, with such
//                 operations natively occurring in user-defined world unit space.
//                 Metric constants are supplied only as a convenience.
//                 Distance is calculated using the Euclidean norm formula.
// 
//             4.  Only real values are permissible with functions using 32-bit
//                 float parameters -- NAN and infinite values are not accepted.
//                 All computation occurs in 32-bit precision mode.
// 
//          See https://docs.microsoft.com/en-gb/windows/desktop/xaudio2/x3daudio how to
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
// Source: x3daudio.h
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
unit WinApi.DirectX.XAudio2.X3DAudio;

  {$HPPEMIT '#include "x3daudio.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {WinApi, Clootie's DX or MfPack}
  WinApi.DirectX.D3D9Types;

  {$WEAKPACKAGEUNIT ON}
  {$I 'WinApiTypes.inc'}
  {$I 'XAudio2.inc'}

  //Delphi, Note:  When using pointer array calculations, POINTERMATH should be turned ON
  //               {$POINTERMATH ON}

const

  // speaker geometry configuration flags, specifies assignment of channels to speaker positions, defined as per WAVEFORMATEXTENSIBLE.dwChannelMask
{$IFNDEF _SPEAKER_POSITIONS_}
  {$DEFINE _SPEAKER_POSITIONS_}

  SPEAKER_FRONT_LEFT                  = $00000001;
  {$EXTERNALSYM SPEAKER_FRONT_LEFT}
  SPEAKER_FRONT_RIGHT                 = $00000002;
  {$EXTERNALSYM SPEAKER_FRONT_RIGHT}
  SPEAKER_FRONT_CENTER                = $00000004;
  {$EXTERNALSYM SPEAKER_FRONT_CENTER}
  SPEAKER_LOW_FREQUENCY               = $00000008;
  {$EXTERNALSYM SPEAKER_LOW_FREQUENCY}
  SPEAKER_BACK_LEFT                   = $00000010;
  {$EXTERNALSYM SPEAKER_BACK_LEFT}
  SPEAKER_BACK_RIGHT                  = $00000020;
  {$EXTERNALSYM SPEAKER_BACK_RIGHT}
  SPEAKER_FRONT_LEFT_OF_CENTER        = $00000040;
  {$EXTERNALSYM SPEAKER_FRONT_LEFT_OF_CENTER}
  SPEAKER_FRONT_RIGHT_OF_CENTER       = $00000080;
  {$EXTERNALSYM SPEAKER_FRONT_RIGHT_OF_CENTER}
  SPEAKER_BACK_CENTER                 = $00000100;
  {$EXTERNALSYM SPEAKER_BACK_CENTER}
  SPEAKER_SIDE_LEFT                   = $00000200;
  {$EXTERNALSYM SPEAKER_SIDE_LEFT}
  SPEAKER_SIDE_RIGHT                  = $00000400;
  {$EXTERNALSYM SPEAKER_SIDE_RIGHT}
  SPEAKER_TOP_CENTER                  = $00000800;
  {$EXTERNALSYM SPEAKER_TOP_CENTER}
  SPEAKER_TOP_FRONT_LEFT              = $00001000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_LEFT}
  SPEAKER_TOP_FRONT_CENTER            = $00002000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_CENTER}
  SPEAKER_TOP_FRONT_RIGHT             = $00004000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_RIGHT}
  SPEAKER_TOP_BACK_LEFT               = $00008000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_LEFT}
  SPEAKER_TOP_BACK_CENTER             = $00010000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_CENTER}
  SPEAKER_TOP_BACK_RIGHT              = $00020000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_RIGHT}
  SPEAKER_RESERVED                    = $7FFC0000;  // bit mask locations reserved for future use
  {$EXTERNALSYM SPEAKER_RESERVED}
  SPEAKER_ALL                         = $80000000;  // used to specify that any possible permutation of speaker configurations
  {$EXTERNALSYM SPEAKER_ALL}
{$ENDIF}

  // standard speaker geometry configurations, used with X3DAudioInitialize
{$IFNDEF SPEAKER_MONO}
  SPEAKER_MONO                        = SPEAKER_FRONT_CENTER;
  {$EXTERNALSYM SPEAKER_MONO}
  SPEAKER_STEREO                      = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT);
  {$EXTERNALSYM SPEAKER_STEREO}
  SPEAKER_2POINT1                     = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_LOW_FREQUENCY);
  {$EXTERNALSYM SPEAKER_2POINT1}
  SPEAKER_SURROUND                    = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_FRONT_CENTER or SPEAKER_BACK_CENTER);
  {$EXTERNALSYM SPEAKER_SURROUND}
  SPEAKER_QUAD                        = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);
  {$EXTERNALSYM SPEAKER_QUAD}
  SPEAKER_4POINT1                     = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_LOW_FREQUENCY or SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);
  {$EXTERNALSYM SPEAKER_4POINT1}
  SPEAKER_5POINT1                     = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);
  {$EXTERNALSYM SPEAKER_5POINT1}
  SPEAKER_7POINT1                     = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or SPEAKER_FRONT_LEFT_OF_CENTER or SPEAKER_FRONT_RIGHT_OF_CENTER);
  {$EXTERNALSYM SPEAKER_7POINT1}
  SPEAKER_5POINT1_SURROUND            = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or SPEAKER_SIDE_LEFT or SPEAKER_SIDE_RIGHT);
  {$EXTERNALSYM SPEAKER_5POINT1_SURROUND}
  SPEAKER_7POINT1_SURROUND            = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or SPEAKER_SIDE_LEFT or SPEAKER_SIDE_RIGHT);
  {$EXTERNALSYM SPEAKER_7POINT1_SURROUND}
{$ENDIF}

  // size of instance handle in bytes
  X3DAUDIO_HANDLE_BYTESIZE            = 20;
  {$EXTERNALSYM X3DAUDIO_HANDLE_BYTESIZE}

  // float math constants
  X3DAUDIO_PI                         = 3.141592654;
  {$EXTERNALSYM X3DAUDIO_PI}
  X3DAUDIO_2PI                        = 6.283185307;
  {$EXTERNALSYM X3DAUDIO_2PI}

  // speed of sound in meters per second for dry air at approximately 20C, used with X3DAudioInitialize
  X3DAUDIO_SPEED_OF_SOUND             = 343.5;
  {$EXTERNALSYM X3DAUDIO_SPEED_OF_SOUND}

  // calculation control flags, used with X3DAudioCalculate
  X3DAUDIO_CALCULATE_MATRIX           = $00000001;  // enable matrix coefficient table calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_MATRIX}
  X3DAUDIO_CALCULATE_DELAY            = $00000002;  // enable delay time array calculation (stereo final mix only)
  {$EXTERNALSYM X3DAUDIO_CALCULATE_DELAY}
  X3DAUDIO_CALCULATE_LPF_DIRECT       = $00000004;  // enable LPF direct-path coefficient calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_LPF_DIRECT}
  X3DAUDIO_CALCULATE_LPF_REVERB       = $00000008;  // enable LPF reverb-path coefficient calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_LPF_REVERB}
  X3DAUDIO_CALCULATE_REVERB           = $00000010;  // enable reverb send level calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_REVERB}
  X3DAUDIO_CALCULATE_DOPPLER          = $00000020;  // enable doppler shift factor calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_DOPPLER}
  X3DAUDIO_CALCULATE_EMITTER_ANGLE    = $00000040;  // enable emitter-to-listener interior angle calculation
  {$EXTERNALSYM X3DAUDIO_CALCULATE_EMITTER_ANGLE}

  X3DAUDIO_CALCULATE_ZEROCENTER       = $00010000;  // do not position to front center speaker, signal positioned to remaining speakers instead, front center destination channel will be zero in returned matrix coefficient table, valid only for matrix calculations with final mix formats that have a front center channel
  {$EXTERNALSYM X3DAUDIO_CALCULATE_ZEROCENTER}
  X3DAUDIO_CALCULATE_REDIRECT_TO_LFE  = $00020000;  // apply equal mix of all source channels to LFE destination channel, valid only for matrix calculations with sources that have no LFE channel and final mix formats that have an LFE channel
  {$EXTERNALSYM X3DAUDIO_CALCULATE_REDIRECT_TO_LFE}

  //--------------<D-A-T-A---T-Y-P-E-S>---------------------------------------//
  //#pragma pack(push, 1) // set packing alignment to ensure consistency across arbitrary build environments
  // Within Delphi the default alignment is 8 bytes (quad word), unless the project alignment settings are changed.
  // Disable field aligned. All record and class structures will be packed.
  {$ALIGN 1}


type
  // primitive types
  //PFloat32 = ^TFloat32; 
  //FLOAT32 = Single; 
   
  // 32-bit IEEE float 
  PX3DAUDIO_VECTOR = PD3DVECTOR;
  X3DAUDIO_VECTOR = D3DVECTOR;    // float 3D vector
  {$EXTERNALSYM X3DAUDIO_VECTOR}


  // instance handle of precalculated constants
  X3DAUDIO_HANDLE = array[0..X3DAUDIO_HANDLE_BYTESIZE - 1] of Byte;
  {$EXTERNALSYM X3DAUDIO_HANDLE}


  // Distance curve point:
  // Defines a DSP setting at a given normalized distance.
  PX3DAUDIO_DISTANCE_CURVE_POINT = ^X3DAUDIO_DISTANCE_CURVE_POINT;
  LPX3DAUDIO_DISTANCE_CURVE_POINT = ^X3DAUDIO_DISTANCE_CURVE_POINT;
  {$EXTERNALSYM LPX3DAUDIO_DISTANCE_CURVE_POINT}
  X3DAUDIO_DISTANCE_CURVE_POINT = record
    Distance: FLOAT32;               // normalized distance, must be within [0.0f, 1.0f]
    DSPSetting: FLOAT32;             // DSP setting
  end;
  {$EXTERNALSYM X3DAUDIO_DISTANCE_CURVE_POINT}

  // If you don't want to use a pointer to array
  TX3DAudioDistancePointArray = array [0..65535] of X3DAUDIO_DISTANCE_CURVE_POINT;
  {$EXTERNALSYM TX3DAudioDistancePointArray}

  // Distance curve:
  // A piecewise curve made up of linear segments used to
  // define DSP behaviour with respect to normalized distance.
  //
  // Note that curve point distances are normalized within [0.0f, 1.0f].
  // X3DAUDIO_EMITTER.CurveDistanceScaler must be used to scale the
  // normalized distances to user-defined world units.
  // For distances beyond CurveDistanceScaler * 1.0f,
  // pPoints[PointCount-1].DSPSetting is used as the DSP setting.
  //
  // All distance curve spans must be such that:
  //      pPoints[k-1].DSPSetting + ((pPoints[k].DSPSetting-pPoints[k-1].DSPSetting) / (pPoints[k].Distance-pPoints[k-1].Distance)) * (pPoints[k].Distance-pPoints[k-1].Distance) != NAN or infinite values
  // For all points in the distance curve where 1 <= k < PointCount.

  PX3DAUDIO_DISTANCE_CURVE = ^X3DAUDIO_DISTANCE_CURVE;
  LPX3DAUDIO_DISTANCE_CURVE = ^X3DAUDIO_DISTANCE_CURVE;
  {$EXTERNALSYM LPX3DAUDIO_DISTANCE_CURVE}
  X3DAUDIO_DISTANCE_CURVE = record
    pPoints: PX3DAUDIO_DISTANCE_CURVE_POINT;  // distance curve point array, must have at least PointCount elements with no duplicates and be sorted in ascending order with respect to Distance
    PointCount: UINT32;                       // number of distance curve points, must be >= 2 as all distance curves must have at least two endpoints, defining DSP settings at 0.0f and 1.0f normalized distance
  end;
  {$EXTERNALSYM X3DAUDIO_DISTANCE_CURVE}

const

  //static const X3DAUDIO_DISTANCE_CURVE_POINT X3DAudioDefault_LinearCurvePoints[2] = { 0.0f, 1.0f, 1.0f, 0.0f };
  X3DAudioDefault_LinearCurvePoints: array[0..1] of X3DAUDIO_DISTANCE_CURVE_POINT = ((Distance: 0.0;
                                                                                      DSPSetting: 1.0),
                                                                                     (Distance: 1.0;
                                                                                      DSPSetting: 0.0));
  {$EXTERNALSYM X3DAudioDefault_LinearCurvePoints}

  //static const X3DAUDIO_DISTANCE_CURVE   X3DAudioDefault_LinearCurve  = { (X3DAUDIO_DISTANCE_CURVE_POINT*)&X3DAudioDefault_LinearCurvePoints[0], 2 };
  X3DAudioDefault_LinearCurve: X3DAUDIO_DISTANCE_CURVE = (pPoints: @X3DAudioDefault_LinearCurvePoints[0]; PointCount: 2);
  {$EXTERNALSYM X3DAudioDefault_LinearCurve}

type
  // Cone:
  // Specifies directionality for a listener or single-channel emitter by
  // modifying DSP behaviour with respect to its front orientation.
  // This is modeled using two sound cones: an inner cone and an outer cone.
  // On/within the inner cone, DSP settings are scaled by the inner values.
  // On/beyond the outer cone, DSP settings are scaled by the outer values.
  // If on both the cones, DSP settings are scaled by the inner values only.
  // Between the two cones, the scaler is linearly interpolated between the
  // inner and outer values.  Set both cone angles to 0 or X3DAUDIO_2PI for
  // omnidirectionality using only the outer or inner values respectively.
  PX3DAUDIO_CONE = ^X3DAUDIO_CONE;
  LPX3DAUDIO_CONE = ^X3DAUDIO_CONE;
  {$EXTERNALSYM LPX3DAUDIO_CONE}
  X3DAUDIO_CONE = record
    InnerAngle: FLOAT32;             // inner cone angle in radians, must be within [0.0f, X3DAUDIO_2PI]
    OuterAngle: FLOAT32;             // outer cone angle in radians, must be within [InnerAngle, X3DAUDIO_2PI]
    InnerVolume: FLOAT32;            // volume level scaler on/within inner cone, used only for matrix calculations, must be within [0.0f, 2.0f] when used
    OuterVolume: FLOAT32;            // volume level scaler on/beyond outer cone, used only for matrix calculations, must be within [0.0f, 2.0f] when used
    InnerLPF: FLOAT32;               // LPF (both direct and reverb paths) coefficient subtrahend on/within inner cone, used only for LPF (both direct and reverb paths) calculations, must be within [0.0f, 1.0f] when used
    OuterLPF: FLOAT32;               // LPF (both direct and reverb paths) coefficient subtrahend on/beyond outer cone, used only for LPF (both direct and reverb paths) calculations, must be within [0.0f, 1.0f] when used
    InnerReverb: FLOAT32;            // reverb send level scaler on/within inner cone, used only for reverb calculations, must be within [0.0f, 2.0f] when used
    OuterReverb: FLOAT32;            // reverb send level scaler on/beyond outer cone, used only for reverb calculations, must be within [0.0f, 2.0f] when used
  end;
  {$EXTERNALSYM X3DAUDIO_CONE}

const

  X3DAudioDefault_DirectionalCone: X3DAUDIO_CONE = (InnerAngle: X3DAUDIO_PI / 2;
                                                    OuterAngle: X3DAUDIO_PI;
                                                    InnerVolume: 1.0;
                                                    OuterVolume: 0.708;
                                                    InnerLPF: 1.0;
                                                    OuterLPF: 0.75;
                                                    InnerReverb: 0.708;
                                                    OuterReverb: 1.0 );
  {$EXTERNALSYM X3DAudioDefault_DirectionalCone}


type

  // Listener:
  // Defines a point of 3D audio reception.
  //
  // The cone is directed by the listener's front orientation.

  PX3DAUDIO_LISTENER = ^X3DAUDIO_LISTENER;
  LPX3DAUDIO_LISTENER = ^X3DAUDIO_LISTENER;
  {$EXTERNALSYM LPX3DAUDIO_LISTENER}
  X3DAUDIO_LISTENER = record
    OrientFront: X3DAUDIO_VECTOR;    // orientation of front direction, used only for matrix and delay calculations or listeners with cones for matrix, LPF (both direct and reverb paths), and reverb calculations, must be normalized when used
    OrientTop: X3DAUDIO_VECTOR;      // orientation of top direction, used only for matrix and delay calculations, must be orthonormal with OrientFront when used
    Position: X3DAUDIO_VECTOR;       // position in user-defined world units, does not affect Velocity
    Velocity: X3DAUDIO_VECTOR;       // velocity vector in user-defined world units/second, used only for doppler calculations, does not affect Position
    pCone: PX3DAUDIO_CONE;           // sound cone, used only for matrix, LPF (both direct and reverb paths), and reverb calculations, NULL specifies omnidirectionality
  end;
  {$EXTERNALSYM X3DAUDIO_LISTENER}


  // Emitter:
  // Defines a 3D audio source, divided into two classifications:
  //
  // Single-point -- For use with single-channel sounds.
  //                 Positioned at the emitter base, i.e. the channel radius
  //                 and azimuth are ignored if the number of channels == 1.
  //
  //                 May be omnidirectional or directional using a cone.
  //                 The cone originates from the emitter base position,
  //                 and is directed by the emitter's front orientation.
  //
  // Multi-point  -- For use with multi-channel sounds.
  //                 Each non-LFE channel is positioned using an
  //                 azimuth along the channel radius with respect to the
  //                 front orientation vector in the plane orthogonal to the
  //                 top orientation vector.  An azimuth of X3DAUDIO_2PI
  //                 specifies a channel is an LFE.  Such channels are
  //                 positioned at the emitter base and are calculated
  //                 with respect to pLFECurve only, never pVolumeCurve.
  //
  //                 Multi-point emitters are always omnidirectional,
  //                 i.e. the cone is ignored if the number of channels > 1.
  //
  // Note that many properties are shared among all channel points,
  // locking certain behaviour with respect to the emitter base position.
  // For example, doppler shift is always calculated with respect to the
  // emitter base position and so is constant for all its channel points.
  // Distance curve calculations are also with respect to the emitter base
  // position, with the curves being calculated independently of each other.
  // For instance, volume and LFE calculations do not affect one another.
  PX3DAUDIO_EMITTER = ^X3DAUDIO_EMITTER;
  LPX3DAUDIO_EMITTER = ^X3DAUDIO_EMITTER;
  {$EXTERNALSYM LPX3DAUDIO_EMITTER}
  X3DAUDIO_EMITTER = record
    pCone: PX3DAUDIO_CONE;                       // sound cone, used only with single-channel emitters for matrix, LPF (both direct and reverb paths), and reverb calculations, NULL specifies omnidirectionality
    OrientFront: X3DAUDIO_VECTOR;                // orientation of front direction, used only for emitter angle calculations or with multi-channel emitters for matrix calculations or single-channel emitters with cones for matrix, LPF (both direct and reverb paths), and reverb calculations, must be normalized when used
    OrientTop: X3DAUDIO_VECTOR;                  // orientation of top direction, used only with multi-channel emitters for matrix calculations, must be orthonormal with OrientFront when used
    Position: X3DAUDIO_VECTOR;                   // position in user-defined world units, does not affect Velocity
    Velocity: X3DAUDIO_VECTOR;                   // velocity vector in user-defined world units/second, used only for doppler calculations, does not affect Position
    InnerRadius: FLOAT32;                        // inner radius, must be within [0.0f, FLT_MAX]
    InnerRadiusAngle: FLOAT32;                   // inner radius angle, must be within [0.0f, X3DAUDIO_PI/4.0)
    ChannelCount: UINT32;                        // number of sound channels, must be > 0
    ChannelRadius: FLOAT32;                      // channel radius, used only with multi-channel emitters for matrix calculations, must be >= 0.0f when used
    pChannelAzimuths: PFLOAT32;                  // channel azimuth array, used only with multi-channel emitters for matrix calculations, contains positions of each channel expressed in radians along the channel radius with respect to the front orientation vector in the plane orthogonal to the top orientation vector, or X3DAUDIO_2PI to specify an LFE channel, must have at least ChannelCount elements, all within [0.0f, X3DAUDIO_2PI] when used
    pVolumeCurve: PX3DAUDIO_DISTANCE_CURVE;      // volume level distance curve, used only for matrix calculations, NULL specifies a default curve that conforms to the inverse square law, calculated in user-defined world units with distances <= CurveDistanceScaler clamped to no attenuation
    pLFECurve: PX3DAUDIO_DISTANCE_CURVE;         // LFE level distance curve, used only for matrix calculations, NULL specifies a default curve that conforms to the inverse square law, calculated in user-defined world units with distances <= CurveDistanceScaler clamped to no attenuation
    pLPFDirectCurve: PX3DAUDIO_DISTANCE_CURVE;   // LPF direct-path coefficient distance curve, used only for LPF direct-path calculations, NULL specifies the default curve: [0.0f,1.0f], [1.0f,0.75f]
    pLPFReverbCurve: PX3DAUDIO_DISTANCE_CURVE;   // LPF reverb-path coefficient distance curve, used only for LPF reverb-path calculations, NULL specifies the default curve: [0.0f,0.75f], [1.0f,0.75f]
    pReverbCurve: PX3DAUDIO_DISTANCE_CURVE;      // reverb send level distance curve, used only for reverb calculations, NULL specifies the default curve: [0.0f,1.0f], [1.0f,0.0f]
    CurveDistanceScaler: FLOAT32;                // curve distance scaler, used to scale normalized distance curves to user-defined world units and/or exaggerate their effect, used only for matrix, LPF (both direct and reverb paths), and reverb calculations, must be within [FLT_MIN, FLT_MAX] when used
    DopplerScaler: FLOAT32;                      // doppler shift scaler, used to exaggerate doppler shift effect, used only for doppler calculations, must be within [0.0f, FLT_MAX] when used
  end;
  {$EXTERNALSYM X3DAUDIO_EMITTER}

  // DSP settings:
  // Receives results from a call to X3DAudioCalculate to be sent
  // to the low-level audio rendering API for 3D signal processing.
  //
  // The user is responsible for allocating the matrix coefficient table,
  // delay time array, and initializing the channel counts when used.
  PX3DAUDIO_DSP_SETTINGS = ^X3DAUDIO_DSP_SETTINGS;
  LPX3DAUDIO_DSP_SETTINGS = ^X3DAUDIO_DSP_SETTINGS;
  {$EXTERNALSYM LPX3DAUDIO_DSP_SETTINGS}
  X3DAUDIO_DSP_SETTINGS = record
    pMatrixCoefficients: PFloat32;        // [inout] matrix coefficient table, receives an array representing the volume level used to send from source channel S to destination channel D, stored as pMatrixCoefficients[SrcChannelCount * D + S], must have at least SrcChannelCount*DstChannelCount elements
    pDelayTimes: PFloat;                  // [inout] delay time array, receives delays for each destination channel in milliseconds, must have at least DstChannelCount elements (stereo final mix only)
    SrcChannelCount: UINT32;              // [in] number of source channels, must equal number of channels in respective emitter
    DstChannelCount: UINT32;              // [in] number of destination channels, must equal number of channels of the final mix
    LPFDirectCoefficient: FLOAT32;        // [out] LPF direct-path coefficient
    LPFReverbCoefficient: FLOAT32;        // [out] LPF reverb-path coefficient
    ReverbLevel: FLOAT32;                 // [out] reverb send level
    DopplerFactor: FLOAT32;               // [out] doppler shift factor, scales resampler ratio for doppler shift effect, where the effective frequency = DopplerFactor * original frequency
    EmitterToListenerAngle: FLOAT32;      // [out] emitter-to-listener interior angle, expressed in radians with respect to the emitter's front orientation
    EmitterToListenerDistance: FLOAT32;   // [out] distance in user-defined world units from the emitter base to listener position, always calculated
    EmitterVelocityComponent: FLOAT32;    // [out] component of emitter velocity vector projected onto emitter->listener vector in user-defined world units/second, calculated only for doppler
    ListenerVelocityComponent: FLOAT32;   // [out] component of listener velocity vector projected onto emitter->listener vector in user-defined world units/second, calculated only for doppler
  end;
  {$EXTERNALSYM X3DAUDIO_DSP_SETTINGS}


  //--------------<F-U-N-C-T-I-O-N-S>-----------------------------------------//
  // initializes instance handle
  function X3DAudioInitialize(SpeakerChannelMask: UINT32;
                              SpeedOfSound: FLOAT32;
                              out Instance: X3DAUDIO_HANDLE): HRESULT; stdcall;
  {$EXTERNALSYM X3DAudioInitialize}

  // calculates DSP settings with respect to 3D parameters
  procedure X3DAudioCalculate(const Instance: X3DAUDIO_HANDLE;
                              pListener: X3DAUDIO_LISTENER;
                              pEmitter: X3DAUDIO_EMITTER;
                              Flags: UINT32;
                              var pDSPSettings: X3DAUDIO_DSP_SETTINGS); stdcall;
  {$EXTERNALSYM X3DAudioCalculate}


  //#pragma pack(pop) // revert packing alignment
  // set back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation

{$WARN SYMBOL_PLATFORM OFF}
function X3DAudioInitialize; external X3DAUDIO_DLL name 'X3DAudioInitialize' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
procedure X3DAudioCalculate; external X3DAUDIO_DLL name 'X3DAudioCalculate'  {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.
