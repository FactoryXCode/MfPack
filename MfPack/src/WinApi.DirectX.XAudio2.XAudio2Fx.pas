// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
// Module: WinApi.DirectX.XAudio2.XAudio2Fx.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Declarations for the audio effects included with XAudio2.
//              Windows 8 XAudio 2.8 or later
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
// Remarks: This version of XAudio2 is available only in Windows 8 or later.
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
// Source: xaudio2fx.h
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
unit WinApi.DirectX.XAudio2.XAudio2Fx;

  {$HPPEMIT '#include "xaudio2fx.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Math;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}
  {$I 'XAudio2.inc'}


// XAudio 2.8
const

  CLSID_AudioVolumeMeter : TGUID = (D1: $4FC3B166;
                                    D2: $972A;
                                    D3: $40CF;
                                    D4: ($BC, $37, $7D, $B0, $3D, $B2, $FB, $A3));
  {$EXTERNALSYM CLSID_AudioVolumeMeter}

  CLSID_AudioReverb      : TGUID = (D1: $C2633B16;
                                    D2: $471B;
                                    D3: $4498;
                                    D4: ($B8, $C5, $4F, $09, $59, $E2, $EC, $09));
  {$EXTERNALSYM CLSID_AudioReverb}

  // All structures defined in this file should use tight packing
  // #pragma pack(push, 1)
  // Within Delphi the default alignment is 8 bytes (quad word), unless the project alignment settings are changed.
  // Disable field aligned. All record and class structures will be packed.
  {$ALIGN 1}

  (**************************************************************************
  *
  * Effect creation functions.
  *
  * On Xbox the application can link with the debug library to use the debug
  * functionality.
  *
  **************************************************************************)


  function CreateAudioVolumeMeter(out ppApo: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM CreateAudioVolumeMeter}
  function CreateAudioReverb(out ppApo: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM CreateAudioReverb}

  // Note:
  // Because XAudio2CreateVolumeMeter calls CoCreateInstance on Windows,
  // the application must have called the CoInitializeEx method before calling XAudio2CreateVolumeMeter.
  // XAudio2Create has the same requirement, which means CoInitializeEx typically will be
  // called long before XAudio2CreateVolumeMeter is called.
  //
  // A typical calling pattern on Windows would be as follows:

  (* Code
    ...
    CoInitializeEx(nil,
                   COINIT_MULTITHREADED);

    ...
    pXAudio2: IXAudio2;
    hr: HResult;

    If FAILED(hr:= XAudio2Create(pXAudio2,
                                 0,
                                 XAUDIO2_DEFAULT_PROCESSOR)) then
    ...
    Result:= hr;

    ...
    pVolumeMeterAPO: IUnknown;
    hr:= XAudio2CreateVolumeMeter(pVolumeMeterAPO);

  *)




  // inline functions
  function XAudio2CreateVolumeMeter(out ppApo: IUnknown;
                                    Flags: UINT32 = 0): HRESULT; inline;
  {$EXTERNALSYM XAudio2CreateVolumeMeter}


  function XAudio2CreateReverb(out ppApo: IUnknown;
                               Flags: UINT32 = 0): HRESULT; inline;
  {$EXTERNALSYM XAudio2CreateReverb}



  (**************************************************************************
  *
  * Volume meter parameters.
  * The volume meter supports FLOAT32 audio formats and must be used in-place.
  *
  **************************************************************************)

  // XAUDIO2FX_VOLUMEMETER_LEVELS: Receives results from GetEffectParameters().
  // The user is responsible for allocating pPeakLevels, pRMSLevels, and
  // initializing ChannelCount accordingly.
  // The volume meter does not support SetEffectParameters().
type
  PXAUDIO2FX_VOLUMEMETER_LEVELS = ^XAUDIO2FX_VOLUMEMETER_LEVELS;
  XAUDIO2FX_VOLUMEMETER_LEVELS = record
    pPeakLevels: PSingle;             // Peak levels table: receives maximum absolute level for each channel
                                      // over a processing pass; may be nil if pRMSLevls >= nil,
                                      // otherwise must have at least ChannelCount elements.
    pRMSLevels: PSingle;              // Root mean square levels table: receives RMS level for each channel
                                      // over a processing pass; may be nil if pPeakLevels >= nil,
                                      // otherwise must have at least ChannelCount elements.
    ChannelCount: UINT32;             // Number of channels being processed by the volume meter APO
  end;
  {$EXTERNALSYM XAUDIO2FX_VOLUMEMETER_LEVELS}



  (**************************************************************************
  *
  * Reverb parameters.
  * The reverb supports only FLOAT32 audio with the following channel
  * configurations:
  *     Input: Mono   Output: Mono
  *     Input: Mono   Output: 5.1
  *     Input: Stereo Output: Stereo
  *     Input: Stereo Output: 5.1
  * The framerate must be within [20000, 48000] Hz.
  *
  * When using mono input, delay filters associated with the right channel
  * are not executed.  In this case, parameters such as PositionRight and
  * PositionMatrixRight have no effect.  This also means the reverb uses
  * less CPU when hosted in a mono submix.
  *
  **************************************************************************)
const

  XAUDIO2FX_REVERB_MIN_FRAMERATE      = 20000;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_FRAMERATE}
  XAUDIO2FX_REVERB_MAX_FRAMERATE      = 48000;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_FRAMERATE}

  // XAUDIO2FX_REVERB_PARAMETERS: Native parameter set for the reverb effect

type

  PXAUDIO2FX_REVERB_PARAMETERS = ^XAUDIO2FX_REVERB_PARAMETERS;
  XAUDIO2FX_REVERB_PARAMETERS = record
// ratio of wet (processed) signal to dry (original) signal
    WetDryMix: Single;               // [0, 100] (percentage)
// Delay times
    ReflectionsDelay: UINT32;        // [0, 300] in ms
    ReverbDelay: Byte;               // [0, 85] in ms
    RearDelay: Byte;                 // 7.1: [0, 20] in ms, all other: [0, 5] in ms
{$IFDEF _WIN32_WINNT_WIN10}
    SideDelay: Byte;                 // 7.1: [0, 5] in ms, all other: not used, but still validated
{$ENDIF}
// Indexed parameters
    PositionLeft: Byte;              // [0, 30] no units
    PositionRight: Byte;             // [0, 30] no units, ignored when configured to mono
    PositionMatrixLeft: Byte;        // [0, 30] no units
    PositionMatrixRight: Byte;       // [0, 30] no units, ignored when configured to mono
    EarlyDiffusion: Byte;            // [0, 15] no units
    LateDiffusion: Byte;             // [0, 15] no units
    LowEQGain: Byte;                 // [0, 12] no units
    LowEQCutoff: Byte;               // [0, 9] no units
    HighEQGain: Byte;                // [0, 8] no units
    HighEQCutoff: Byte;              // [0, 14] no units
// Direct parameters
    RoomFilterFreq: Single;           // [20, 20000] in Hz
    RoomFilterMain: Single;           // [-100, 0] in dB
    RoomFilterHF: Single;             // [-100, 0] in dB
    ReflectionsGain: Single;          // [-100, 20] in dB
    ReverbGain: Single;               // [-100, 20] in dB
    DecayTime: Single;                // [0.1, inf] in seconds
    Density: Single;                  // [0, 100] (percentage)
    RoomSize: Single;                 // [1, 100] in feet
// component control
    DisableLateField: BOOL;         // TRUE to disable late field reflections
  end;
  {$EXTERNALSYM XAUDIO2FX_REVERB_PARAMETERS}


  // Maximum, minimum and default values for the parameters above
const

  XAUDIO2FX_REVERB_MIN_WET_DRY_MIX         : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_WET_DRY_MIX}
  XAUDIO2FX_REVERB_MIN_REFLECTIONS_DELAY   = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_REFLECTIONS_DELAY}
  XAUDIO2FX_REVERB_MIN_REVERB_DELAY        = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_REVERB_DELAY}
  XAUDIO2FX_REVERB_MIN_REAR_DELAY          = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_REAR_DELAY}
  XAUDIO2FX_REVERB_MIN_7POINT1_SIDE_DELAY  = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_7POINT1_SIDE_DELAY}
  XAUDIO2FX_REVERB_MIN_7POINT1_REAR_DELAY  = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_7POINT1_REAR_DELAY}
  XAUDIO2FX_REVERB_MIN_POSITION            = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_POSITION}
  XAUDIO2FX_REVERB_MIN_DIFFUSION           = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_DIFFUSION}
  XAUDIO2FX_REVERB_MIN_LOW_EQ_GAIN         = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_LOW_EQ_GAIN}
  XAUDIO2FX_REVERB_MIN_LOW_EQ_CUTOFF       = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_LOW_EQ_CUTOFF}
  XAUDIO2FX_REVERB_MIN_HIGH_EQ_GAIN        = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_HIGH_EQ_GAIN}
  XAUDIO2FX_REVERB_MIN_HIGH_EQ_CUTOFF      = 0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_HIGH_EQ_CUTOFF}
  XAUDIO2FX_REVERB_MIN_ROOM_FILTER_FREQ    : Single = 20.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_ROOM_FILTER_FREQ}
  XAUDIO2FX_REVERB_MIN_ROOM_FILTER_MAIN    : Single = -100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_ROOM_FILTER_MAIN}
  XAUDIO2FX_REVERB_MIN_ROOM_FILTER_HF      : Single = -100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_ROOM_FILTER_HF}
  XAUDIO2FX_REVERB_MIN_REFLECTIONS_GAIN    : Single = -100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_REFLECTIONS_GAIN}
  XAUDIO2FX_REVERB_MIN_REVERB_GAIN         : Single = -100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_REVERB_GAIN}
  XAUDIO2FX_REVERB_MIN_DECAY_TIME          : Single = 0.1;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_DECAY_TIME}
  XAUDIO2FX_REVERB_MIN_DENSITY             : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_DENSITY}
  XAUDIO2FX_REVERB_MIN_ROOM_SIZE           : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MIN_ROOM_SIZE}

  XAUDIO2FX_REVERB_MAX_WET_DRY_MIX         : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_WET_DRY_MIX}
  XAUDIO2FX_REVERB_MAX_REFLECTIONS_DELAY   = 300;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_REFLECTIONS_DELAY}
  XAUDIO2FX_REVERB_MAX_REVERB_DELAY        = 85;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_REVERB_DELAY}
  XAUDIO2FX_REVERB_MAX_REAR_DELAY          = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_REAR_DELAY}
  XAUDIO2FX_REVERB_MAX_7POINT1_SIDE_DELAY  = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_7POINT1_SIDE_DELAY}
  XAUDIO2FX_REVERB_MAX_7POINT1_REAR_DELAY  = 20;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_7POINT1_REAR_DELAY}
  XAUDIO2FX_REVERB_MAX_POSITION            = 30;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_POSITION}
  XAUDIO2FX_REVERB_MAX_DIFFUSION           = 15;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_DIFFUSION}
  XAUDIO2FX_REVERB_MAX_LOW_EQ_GAIN         = 12;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_LOW_EQ_GAIN}
  XAUDIO2FX_REVERB_MAX_LOW_EQ_CUTOFF       = 9;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_LOW_EQ_CUTOFF}
  XAUDIO2FX_REVERB_MAX_HIGH_EQ_GAIN        = 8;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_HIGH_EQ_GAIN}
  XAUDIO2FX_REVERB_MAX_HIGH_EQ_CUTOFF      = 14;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_HIGH_EQ_CUTOFF}
  XAUDIO2FX_REVERB_MAX_ROOM_FILTER_FREQ    : Single = 20000.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_ROOM_FILTER_FREQ}
  XAUDIO2FX_REVERB_MAX_ROOM_FILTER_MAIN    : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_ROOM_FILTER_MAIN}
  XAUDIO2FX_REVERB_MAX_ROOM_FILTER_HF      : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_ROOM_FILTER_HF}
  XAUDIO2FX_REVERB_MAX_REFLECTIONS_GAIN    : Single = 20.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_REFLECTIONS_GAIN}
  XAUDIO2FX_REVERB_MAX_REVERB_GAIN         : Single = 20.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_REVERB_GAIN}
  XAUDIO2FX_REVERB_MAX_DENSITY             : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_DENSITY}
  XAUDIO2FX_REVERB_MAX_ROOM_SIZE           : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_MAX_ROOM_SIZE}

  XAUDIO2FX_REVERB_DEFAULT_WET_DRY_MIX        : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_WET_DRY_MIX}
  XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_DELAY  = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_DELAY}
  XAUDIO2FX_REVERB_DEFAULT_REVERB_DELAY       = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_REVERB_DELAY}
  XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY         = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY}
  XAUDIO2FX_REVERB_DEFAULT_7POINT1_SIDE_DELAY = 5;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_7POINT1_SIDE_DELAY}
  XAUDIO2FX_REVERB_DEFAULT_7POINT1_REAR_DELAY = 20;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_7POINT1_REAR_DELAY}
  XAUDIO2FX_REVERB_DEFAULT_POSITION           = 6;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_POSITION}
  XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX    = 27;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX}
  XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION    = 8;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION}
  XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION     = 8;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION}
  XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_GAIN        = 8;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_GAIN}
  XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_CUTOFF      = 4;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_CUTOFF}
  XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_GAIN       = 8;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_GAIN}
  XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_CUTOFF     = 4;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_CUTOFF}
  XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_FREQ   : Single = 5000.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_FREQ}
  XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_MAIN   : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_MAIN}
  XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_HF     : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_HF}
  XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_GAIN   : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_GAIN}
  XAUDIO2FX_REVERB_DEFAULT_REVERB_GAIN        : Single = 0.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_REVERB_GAIN}
  XAUDIO2FX_REVERB_DEFAULT_DECAY_TIME         : Single = 1.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_DECAY_TIME}
  XAUDIO2FX_REVERB_DEFAULT_DENSITY            : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_DENSITY}
  XAUDIO2FX_REVERB_DEFAULT_ROOM_SIZE          : Single = 100.0;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_ROOM_SIZE}
  XAUDIO2FX_REVERB_DEFAULT_DISABLE_LATE_FIELD : BOOL  = False;
  {$EXTERNALSYM XAUDIO2FX_REVERB_DEFAULT_DISABLE_LATE_FIELD}


  // XAUDIO2FX_REVERB_I3DL2_PARAMETERS: Parameter set compliant with the I3DL2 standard
type

  PXAUDIO2FX_REVERB_I3DL2_PARAMETERS = ^XAUDIO2FX_REVERB_I3DL2_PARAMETERS;
  XAUDIO2FX_REVERB_I3DL2_PARAMETERS = record
  // ratio of wet (processed) signal to dry (original) signal
    WetDryMix: Single;               // [0, 100] (percentage)
  // Standard I3DL2 parameters
    Room: INT32;                     // [-10000, 0] in mB (hundredths of decibels)
    RoomHF: INT32;                   // [-10000, 0] in mB (hundredths of decibels)
    RoomRolloffFactor: Single;        // [0.0, 10.0]
    DecayTime: Single;                // [0.1, 20.0] in seconds
    DecayHFRatio: Single;             // [0.1, 2.0]
    Reflections: INT32;              // [-10000, 1000] in mB (hundredths of decibels)
    ReflectionsDelay: Single;         // [0.0, 0.3] in seconds
    Reverb: INT32;                   // [-10000, 2000] in mB (hundredths of decibels)
    ReverbDelay: Single;              // [0.0, 0.1] in seconds
    Diffusion: Single;                // [0.0, 100.0] (percentage)
    Density: Single;                  // [0.0, 100.0] (percentage)
    HFReference: Single;              // [20.0, 20000.0] in Hz
  end;
  {$EXTERNALSYM XAUDIO2FX_REVERB_I3DL2_PARAMETERS}


  // ReverbConvertI3DL2ToNative: Utility function to map from I3DL2 to native parameters

  procedure ReverbConvertI3DL2ToNative(pI3DL2: XAUDIO2FX_REVERB_I3DL2_PARAMETERS;
                                       out pNative: XAUDIO2FX_REVERB_PARAMETERS
                                       {$IFDEF _WINNT_WIN10}
                                       ; sevenDotOneReverb: BOOL = TRUE
                                       {$ENDIF});
  {$EXTERNALSYM ReverbConvertI3DL2ToNative}


  (**************************************************************************
  *
  * Standard I3DL2 reverb presets (100% wet).
  *
  **************************************************************************)
  (*
   See also additional procedure
  *)

const

  XAUDIO2FX_I3DL2_PRESET_DEFAULT: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                       Room: -10000;
                                                                       RoomHF: 0;
                                                                       RoomRolloffFactor: 0.0;
                                                                       DecayTime: 1.00;
                                                                       DecayHFRatio: 0.50;
                                                                       Reflections: -10000;
                                                                       ReflectionsDelay: 0.020;
                                                                       Reverb: -10000;
                                                                       ReverbDelay: 0.040;
                                                                       Diffusion: 100.0;
                                                                       Density: 100.0;
                                                                       HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_DEFAULT}

  XAUDIO2FX_I3DL2_PRESET_GENERIC: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                       Room: -1000;
                                                                       RoomHF: -100;
                                                                       RoomRolloffFactor: 0.0;
                                                                       DecayTime: 1.49;
                                                                       DecayHFRatio: 0.83;
                                                                       Reflections: -2602;
                                                                       ReflectionsDelay: 0.007;
                                                                       Reverb: 200;
                                                                       ReverbDelay: 0.011;
                                                                       Diffusion: 100.0;
                                                                       Density: 100.0;
                                                                       HFReference: 5000.0);
    {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_GENERIC}

  XAUDIO2FX_I3DL2_PRESET_PADDEDCELL: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -6000;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 0.17;
                                                                          DecayHFRatio: 0.10;
                                                                          Reflections: -1204;
                                                                          ReflectionsDelay: 0.001;
                                                                          Reverb: 207;
                                                                          ReverbDelay: 0.002;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
    {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_PADDEDCELL}

  XAUDIO2FX_I3DL2_PRESET_ROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                    Room: -1000;
                                                                    RoomHF: -454;
                                                                    RoomRolloffFactor: 0.0;
                                                                    DecayTime: 0.40;
                                                                    DecayHFRatio: 0.83;
                                                                    Reflections: -1646;
                                                                    ReflectionsDelay: 0.002;
                                                                    Reverb: 53;
                                                                    ReverbDelay: 0.003;
                                                                    Diffusion: 100.0;
                                                                    Density: 100.0;
                                                                    HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_ROOM}

  XAUDIO2FX_I3DL2_PRESET_BATHROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                        Room: -1000;
                                                                        RoomHF: -1200;
                                                                        RoomRolloffFactor: 0.0;
                                                                        DecayTime: 1.49;
                                                                        DecayHFRatio: 0.54;
                                                                        Reflections: -370;
                                                                        ReflectionsDelay: 0.007;
                                                                        Reverb: 1030;
                                                                        ReverbDelay: 0.011;
                                                                        Diffusion: 100.0;
                                                                        Density: 60.0;
                                                                        HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_BATHROOM}

  XAUDIO2FX_I3DL2_PRESET_LIVINGROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -6000;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 0.50;
                                                                          DecayHFRatio: 0.10;
                                                                          Reflections: -1376;
                                                                          ReflectionsDelay: 0.003;
                                                                          Reverb: -1104;
                                                                          ReverbDelay: 0.004;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_LIVINGROOM}

  XAUDIO2FX_I3DL2_PRESET_STONEROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -300;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 2.31;
                                                                         DecayHFRatio: 0.64;
                                                                         Reflections: -711;
                                                                         ReflectionsDelay: 0.012;
                                                                         Reverb: 83;
                                                                         ReverbDelay: 0.017;
                                                                         Diffusion: 100.0;
                                                                         Density: 100.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_STONEROOM}

  XAUDIO2FX_I3DL2_PRESET_AUDITORIUM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -476;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 4.32;
                                                                          DecayHFRatio: 0.59;
                                                                          Reflections: -789;
                                                                          ReflectionsDelay: 0.020;
                                                                          Reverb: -289;
                                                                          ReverbDelay: 0.030;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
   {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_AUDITORIUM}

  XAUDIO2FX_I3DL2_PRESET_CONCERTHALL: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                           Room: -1000;
                                                                           RoomHF: -500;
                                                                           RoomRolloffFactor: 0.0;
                                                                           DecayTime: 3.92;
                                                                           DecayHFRatio: 0.70;
                                                                           Reflections: -1230;
                                                                           ReflectionsDelay: 0.020;
                                                                           Reverb: -2;
                                                                           ReverbDelay: 0.029;
                                                                           Diffusion: 100.0;
                                                                           Density: 100.0;
                                                                           HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_CONCERTHALL}

  XAUDIO2FX_I3DL2_PRESET_CAVE: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                    Room: -1000;
                                                                    RoomHF: 0;
                                                                    RoomRolloffFactor: 0.0;
                                                                    DecayTime: 2.91;
                                                                    DecayHFRatio: 1.30;
                                                                    Reflections: -602;
                                                                    ReflectionsDelay: 0.015;
                                                                    Reverb: -302;
                                                                    ReverbDelay: 0.022;
                                                                    Diffusion: 100.0;
                                                                    Density: 100.0;
                                                                    HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_CAVE}

  XAUDIO2FX_I3DL2_PRESET_ARENA: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                     Room: -1000;
                                                                     RoomHF: -698;
                                                                     RoomRolloffFactor: 0.0;
                                                                     DecayTime: 7.24;
                                                                     DecayHFRatio: 0.33;
                                                                     Reflections: -1166;
                                                                     ReflectionsDelay: 0.020;
                                                                     Reverb: 16;
                                                                     ReverbDelay: 0.030;
                                                                     Diffusion: 100.0;
                                                                     Density: 100.0;
                                                                     HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_ARENA}

  XAUDIO2FX_I3DL2_PRESET_HANGAR: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                      Room: -1000;
                                                                      RoomHF: -1000;
                                                                      RoomRolloffFactor: 0.0;
                                                                      DecayTime: 10.05;
                                                                      DecayHFRatio: 0.23;
                                                                      Reflections: -602;
                                                                      ReflectionsDelay: 0.020;
                                                                      Reverb: 198;
                                                                      ReverbDelay: 0.030;
                                                                      Diffusion: 100.0;
                                                                      Density: 100.0;
                                                                      HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_HANGAR}

  XAUDIO2FX_I3DL2_PRESET_CARPETEDHALLWAY: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                               Room: -1000;
                                                                               RoomHF: -4000;
                                                                               RoomRolloffFactor: 0.0;
                                                                               DecayTime: 0.30;
                                                                               DecayHFRatio: 0.10;
                                                                               Reflections: -1831;
                                                                               ReflectionsDelay: 0.002;
                                                                               Reverb: -1630;
                                                                               ReverbDelay: 0.030;
                                                                               Diffusion: 100.0;
                                                                               Density: 100.0;
                                                                               HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_CARPETEDHALLWAY}

  XAUDIO2FX_I3DL2_PRESET_HALLWAY: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                       Room: -1000;
                                                                       RoomHF: -300;
                                                                       RoomRolloffFactor: 0.0;
                                                                       DecayTime: 1.49;
                                                                       DecayHFRatio: 0.59;
                                                                       Reflections: -1219;
                                                                       ReflectionsDelay: 0.007;
                                                                       Reverb: 441;
                                                                       ReverbDelay: 0.011;
                                                                       Diffusion: 100.0;
                                                                       Density: 100.0;
                                                                       HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_HALLWAY}

  XAUDIO2FX_I3DL2_PRESET_STONECORRIDOR: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                             Room: -1000;
                                                                             RoomHF: -237;
                                                                             RoomRolloffFactor: 0.0;
                                                                             DecayTime: 2.70;
                                                                             DecayHFRatio: 0.79;
                                                                             Reflections: -1214;
                                                                             ReflectionsDelay: 0.013;
                                                                             Reverb: 395;
                                                                             ReverbDelay: 0.020;
                                                                             Diffusion: 100.0;
                                                                             Density: 100.0;
                                                                             HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_STONECORRIDOR}

  XAUDIO2FX_I3DL2_PRESET_ALLEY: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                     Room: -1000;
                                                                     RoomHF: -270;
                                                                     RoomRolloffFactor: 0.0;
                                                                     DecayTime: 1.49;
                                                                     DecayHFRatio: 0.86;
                                                                     Reflections: -1204;
                                                                     ReflectionsDelay: 0.007;
                                                                     Reverb: -4;
                                                                     ReverbDelay: 0.011;
                                                                     Diffusion: 100.0;
                                                                     Density: 100.0;
                                                                     HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_ALLEY}

  XAUDIO2FX_I3DL2_PRESET_FOREST: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                      Room: -1000;
                                                                      RoomHF: -3300;
                                                                      RoomRolloffFactor: 0.0;
                                                                      DecayTime: 1.49;
                                                                      DecayHFRatio: 0.54;
                                                                      Reflections: -2560;
                                                                      ReflectionsDelay: 0.162;
                                                                      Reverb: -613;
                                                                      ReverbDelay: 0.088;
                                                                      Diffusion: 79.0;
                                                                      Density: 100.0;
                                                                      HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_FOREST}

  XAUDIO2FX_I3DL2_PRESET_CITY: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                    Room: -1000;
                                                                    RoomHF: -800;
                                                                    RoomRolloffFactor: 0.0;
                                                                    DecayTime: 1.49;
                                                                    DecayHFRatio: 0.67;
                                                                    Reflections: -2273;
                                                                    ReflectionsDelay: 0.007;
                                                                    Reverb: -2217;
                                                                    ReverbDelay: 0.011;
                                                                    Diffusion: 50.0;
                                                                    Density: 100.0;
                                                                    HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_CITY}

  XAUDIO2FX_I3DL2_PRESET_MOUNTAINS: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -2500;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 1.49;
                                                                         DecayHFRatio: 0.21;
                                                                         Reflections: -2780;
                                                                         ReflectionsDelay: 0.300;
                                                                         Reverb: -2014;
                                                                         ReverbDelay: 0.100;
                                                                         Diffusion: 27.0;
                                                                         Density: 100.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_MOUNTAINS}

  XAUDIO2FX_I3DL2_PRESET_QUARRY: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                      Room: -1000;
                                                                      RoomHF: -1000;
                                                                      RoomRolloffFactor: 0.0;
                                                                      DecayTime: 1.49;
                                                                      DecayHFRatio: 0.83;
                                                                      Reflections: -10000;
                                                                      ReflectionsDelay: 0.061;
                                                                      Reverb: 500;
                                                                      ReverbDelay: 0.025;
                                                                      Diffusion: 100.0;
                                                                      Density: 100.0;
                                                                      HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_QUARRY}

  XAUDIO2FX_I3DL2_PRESET_PLAIN: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                     Room: -1000;
                                                                     RoomHF: -2000;
                                                                     RoomRolloffFactor: 0.0;
                                                                     DecayTime: 1.49;
                                                                     DecayHFRatio: 0.50;
                                                                     Reflections: -2466;
                                                                     ReflectionsDelay: 0.179;
                                                                     Reverb: -2514;
                                                                     ReverbDelay: 0.100;
                                                                     Diffusion: 21.0;
                                                                     Density: 100.0;
                                                                     HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_PLAIN}

  XAUDIO2FX_I3DL2_PRESET_PARKINGLOT: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: 0;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 1.65;
                                                                          DecayHFRatio: 1.50;
                                                                          Reflections: -1363;
                                                                          ReflectionsDelay: 0.008;
                                                                          Reverb: -1153;
                                                                          ReverbDelay: 0.012;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_PARKINGLOT}

  XAUDIO2FX_I3DL2_PRESET_SEWERPIPE: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -1000;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 2.81;
                                                                         DecayHFRatio: 0.14;
                                                                         Reflections: 429;
                                                                         ReflectionsDelay: 0.014;
                                                                         Reverb: 648;
                                                                         ReverbDelay: 0.021;
                                                                         Diffusion: 80.0;
                                                                         Density: 60.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_SEWERPIPE}

  XAUDIO2FX_I3DL2_PRESET_UNDERWATER: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -4000;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 1.49;
                                                                          DecayHFRatio: 0.10;
                                                                          Reflections: -449;
                                                                          ReflectionsDelay: 0.007;
                                                                          Reverb: 1700;
                                                                          ReverbDelay: 0.011;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_UNDERWATER}

  XAUDIO2FX_I3DL2_PRESET_SMALLROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -600;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 1.10;
                                                                         DecayHFRatio: 0.83;
                                                                         Reflections: -400;
                                                                         ReflectionsDelay: 0.005;
                                                                         Reverb: 500;
                                                                         ReverbDelay: 0.010;
                                                                         Diffusion: 100.0;
                                                                         Density: 100.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_SMALLROOM}

  XAUDIO2FX_I3DL2_PRESET_MEDIUMROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -600;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 1.30;
                                                                          DecayHFRatio: 0.83;
                                                                          Reflections: -1000;
                                                                          ReflectionsDelay: 0.010;
                                                                          Reverb: -200;
                                                                          ReverbDelay: 0.020;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_MEDIUMROOM}

  XAUDIO2FX_I3DL2_PRESET_LARGEROOM: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -600;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 1.50;
                                                                         DecayHFRatio: 0.83;
                                                                         Reflections: -1600;
                                                                         ReflectionsDelay: 0.020;
                                                                         Reverb: -1000;
                                                                         ReverbDelay: 0.040;
                                                                         Diffusion: 100.0;
                                                                         Density: 100.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_LARGEROOM}

  XAUDIO2FX_I3DL2_PRESET_MEDIUMHALL: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                          Room: -1000;
                                                                          RoomHF: -600;
                                                                          RoomRolloffFactor: 0.0;
                                                                          DecayTime: 1.80;
                                                                          DecayHFRatio: 0.70;
                                                                          Reflections: -1300;
                                                                          ReflectionsDelay: 0.015;
                                                                          Reverb: -800;
                                                                          ReverbDelay: 0.030;
                                                                          Diffusion: 100.0;
                                                                          Density: 100.0;
                                                                          HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_MEDIUMHALL}

  XAUDIO2FX_I3DL2_PRESET_LARGEHALL: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                         Room: -1000;
                                                                         RoomHF: -600;
                                                                         RoomRolloffFactor: 0.0;
                                                                         DecayTime: 1.80;
                                                                         DecayHFRatio: 0.70;
                                                                         Reflections: -2000;
                                                                         ReflectionsDelay: 0.030;
                                                                         Reverb: -1400;
                                                                         ReverbDelay: 0.060;
                                                                         Diffusion: 100.0;
                                                                         Density: 100.0;
                                                                         HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_LARGEHALL}

  XAUDIO2FX_I3DL2_PRESET_PLATE: XAUDIO2FX_REVERB_I3DL2_PARAMETERS = (WetDryMix: 100;
                                                                     Room: -1000;
                                                                     RoomHF: -200;
                                                                     RoomRolloffFactor: 0.0;
                                                                     DecayTime: 1.30;
                                                                     DecayHFRatio: 0.90;
                                                                     Reflections: 0;
                                                                     ReflectionsDelay: 0.002;
                                                                     Reverb: 0;
                                                                     ReverbDelay: 0.010;
                                                                     Diffusion: 100.0;
                                                                     Density: 75.0;
                                                                     HFReference: 5000.0);
  {$EXTERNALSYM XAUDIO2FX_I3DL2_PRESET_PLATE}


  // Undo the #pragma pack(push, 1) at the top of this file
  //#pragma pack(pop) // revert packing alignment
  // set back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


  // Additional Prototypes for ALL interfaces

  function ClipByteValue(byteval: UINT32): Byte; inline;

  // End of Additional Prototypes


implementation


function XAudio2CreateVolumeMeter(out ppApo: IUnknown;
                                  Flags: UINT32 = 0): HRESULT; inline;
begin
  Result:= CreateAudioVolumeMeter(ppApo);
end;

function XAudio2CreateReverb(out ppApo: IUnknown;
                             Flags: UINT32 = 0): HRESULT; inline;
begin
  Result:= CreateAudioReverb(ppApo);
end;


{$WARN SYMBOL_PLATFORM OFF}
function CreateAudioVolumeMeter; external XAudio2_DLL name 'CreateAudioVolumeMeter' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
function CreateAudioReverb; external XAudio2_DLL name 'CreateAudioReverb' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}


procedure ReverbConvertI3DL2ToNative(pI3DL2: XAUDIO2FX_REVERB_I3DL2_PARAMETERS;
                                     out pNative: XAUDIO2FX_REVERB_PARAMETERS
                                     {$IFDEF _WINNT_WIN10}
                                     ; sevenDotOneReverb: BOOL = True
                                     {$ENDIF});
var
    reflectionsDelay: Single;
    reverbDelay: Single;
    index: INT32;

begin
  // RoomRolloffFactor is ignored

    // These parameters have no equivalent in I3DL2
{$IFDEF _WIN32_WINNT_WIN10}
  if (sevenDotOneReverb = True) then
    begin
        pNative.RearDelay:= XAUDIO2FX_REVERB_DEFAULT_7POINT1_REAR_DELAY; // 20
    end
  else
    begin
        pNative.RearDelay:= XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY; // 5
    end;

    pNative.SideDelay:= XAUDIO2FX_REVERB_DEFAULT_7POINT1_SIDE_DELAY; // 5
{$ELSE}
    pNative.RearDelay:= XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY; // 5
{$ENDIF}
    pNative.PositionLeft:= XAUDIO2FX_REVERB_DEFAULT_POSITION; // 6
    pNative.PositionRight:= XAUDIO2FX_REVERB_DEFAULT_POSITION; // 6
    pNative.PositionMatrixLeft:= XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX; // 27
    pNative.PositionMatrixRight:= XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX; // 27
    pNative.RoomSize:= XAUDIO2FX_REVERB_DEFAULT_ROOM_SIZE; // 100
    pNative.LowEQCutoff:= 4;
    pNative.HighEQCutoff:= 6;

    // The rest of the I3DL2 parameters map to the native property set
    pNative.RoomFilterMain:= (pI3DL2.Room / 100.0);
    pNative.RoomFilterHF:= (pI3DL2.RoomHF / 100);

    if (pI3DL2.DecayHFRatio >= 1.0) then
      begin
        index:= Round(-4.0 * log10(pI3DL2.DecayHFRatio));

        if (index < -8) then
          index:= -8;

        if (index < 0) then
            pNative.LowEQGain:= index + 8
        else
            pNative.LowEQGain:= 8;

        pNative.HighEQGain:= 8;
        pNative.DecayTime:= pI3DL2.DecayTime * pI3DL2.DecayHFRatio;
      end
    else
      begin
        index:= Round(4.0 * log10(pI3DL2.DecayHFRatio));

        if (index < -8) then
          index:= -8;

        pNative.LowEQGain:= 8;

        if (index < 0) then
            pNative.HighEQGain:= index + 8
        else
            pNative.HighEQGain:= 8;

        pNative.DecayTime:= pI3DL2.DecayTime;
     end;

    reflectionsDelay:= pI3DL2.ReflectionsDelay * 1000.0;

    if (reflectionsDelay >= XAUDIO2FX_REVERB_MAX_REFLECTIONS_DELAY) then // 300
      begin
        reflectionsDelay:= (XAUDIO2FX_REVERB_MAX_REFLECTIONS_DELAY - 1);
      end
    else if (reflectionsDelay <= 1) then
      begin
        reflectionsDelay:= 1;
      end;

    pNative.ReflectionsDelay:= Round(reflectionsDelay);

    reverbDelay:= (pI3DL2.ReverbDelay * 1000.0);
    if (reverbDelay >= XAUDIO2FX_REVERB_MAX_REVERB_DELAY) then // 85
      begin
        reverbDelay:= (XAUDIO2FX_REVERB_MAX_REVERB_DELAY - 1);
      end;

    // Implemented a rangechecker here
    pNative.ReverbDelay:= ClipByteValue(Round(reverbDelay));

    pNative.ReflectionsGain:= (pI3DL2.Reflections / 100.0);
    pNative.ReverbGain:= (pI3DL2.Reverb / 100.0);
    pNative.EarlyDiffusion:= Round(15.0 * pI3DL2.Diffusion / 100.0);
    pNative.LateDiffusion:= pNative.EarlyDiffusion;
    pNative.Density:= pI3DL2.Density;
    pNative.RoomFilterFreq:= pI3DL2.HFReference;

    pNative.WetDryMix:= pI3DL2.WetDryMix;
    pNative.DisableLateField:= FALSE;
end;

// Implement Additional Prototypes here.

function ClipByteValue(byteval: UINT32): Byte; inline;
begin
  if byteval > 255 then
    Result := 255
  else
    Result := Byte(byteval);
end;

end.
