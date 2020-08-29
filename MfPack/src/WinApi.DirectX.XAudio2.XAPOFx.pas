// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
// Module: WinApi.DirectX.XAudio2.XAPOFx.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Cross-platform Audio Processing Objects
// Declarations for the audio effects included with XAudio2.
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
//
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
// Source: xapofx.h
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
unit WinApi.DirectX.XAudio2.XAPOFx;

  {$HPPEMIT '#include "xapofx.h"'}

interface

uses

  {WinApi}
  WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}

  {$I 'XAudio2.inc'}


//--------------<D-E-F-I-N-I-T-I-O-N-S>-------------------------------------//


// FX class IDs

const

  IID_FXEQ               : TGUID = '{F5E01117-D6C4-485A-A3F5-695196F3DBFA}';
  {$EXTERNALSYM IID_FXEQ}
  IID_FXMasteringLimiter : TGUID = '{C4137916-2BE1-46FD-8599-441536F49856}';
  {$EXTERNALSYM IID_FXMasteringLimiter}
  IID_FXReverb           : TGUID = '{7D9ACA56-CB68-4807-B632-B137352E8596}';
  {$EXTERNALSYM IID_FXReverb}
  IID_FXEcho             : TGUID = '{5039D740-F736-449A-84D3-A56202557B87}';
  {$EXTERNALSYM IID_FXEcho}


  // EQ parameter bounds (inclusive), used with FXEQ:
  FXEQ_MIN_FRAMERATE                  = 22000;
  {$EXTERNALSYM FXEQ_MIN_FRAMERATE}
  FXEQ_MAX_FRAMERATE                  = 48000;
  {$EXTERNALSYM FXEQ_MAX_FRAMERATE}

  FXEQ_MIN_FREQUENCY_CENTER           = 20.0;
  {$EXTERNALSYM FXEQ_MIN_FREQUENCY_CENTER}
  FXEQ_MAX_FREQUENCY_CENTER           = 20000.0;
  {$EXTERNALSYM FXEQ_MAX_FREQUENCY_CENTER}
  FXEQ_DEFAULT_FREQUENCY_CENTER_0     = 100.0;    // band 0
  {$EXTERNALSYM FXEQ_DEFAULT_FREQUENCY_CENTER_0}
  FXEQ_DEFAULT_FREQUENCY_CENTER_1     = 800.0;    // band 1
  {$EXTERNALSYM FXEQ_DEFAULT_FREQUENCY_CENTER_1}
  FXEQ_DEFAULT_FREQUENCY_CENTER_2     = 2000.0;   // band 2
  {$EXTERNALSYM FXEQ_DEFAULT_FREQUENCY_CENTER_2}
  FXEQ_DEFAULT_FREQUENCY_CENTER_3     = 10000.0;  // band 3
  {$EXTERNALSYM FXEQ_DEFAULT_FREQUENCY_CENTER_3}

  FXEQ_MIN_GAIN                       = 0.126;  // -18dB
  {$EXTERNALSYM FXEQ_MIN_GAIN}
  FXEQ_MAX_GAIN                       = 7.94;   // +18dB
  {$EXTERNALSYM FXEQ_MAX_GAIN}
  FXEQ_DEFAULT_GAIN                   = 1.0;    // 0dB change, all bands
  {$EXTERNALSYM FXEQ_DEFAULT_GAIN}

  FXEQ_MIN_BANDWIDTH                  = 0.1;
  {$EXTERNALSYM FXEQ_MIN_BANDWIDTH}
  FXEQ_MAX_BANDWIDTH                  = 2.0;
  {$EXTERNALSYM FXEQ_MAX_BANDWIDTH}
  FXEQ_DEFAULT_BANDWIDTH              = 1.0;  // all bands
  {$EXTERNALSYM FXEQ_DEFAULT_BANDWIDTH}


  // Mastering limiter parameter bounds (inclusive), used with FXMasteringLimiter:
  FXMASTERINGLIMITER_MIN_RELEASE      = 1;
  {$EXTERNALSYM FXMASTERINGLIMITER_MIN_RELEASE}
  FXMASTERINGLIMITER_MAX_RELEASE      = 20;
  {$EXTERNALSYM FXMASTERINGLIMITER_MAX_RELEASE}
  FXMASTERINGLIMITER_DEFAULT_RELEASE  = 6;
  {$EXTERNALSYM FXMASTERINGLIMITER_DEFAULT_RELEASE}

  FXMASTERINGLIMITER_MIN_LOUDNESS     = 1;
  {$EXTERNALSYM FXMASTERINGLIMITER_MIN_LOUDNESS}
  FXMASTERINGLIMITER_MAX_LOUDNESS     = 1800;
  {$EXTERNALSYM FXMASTERINGLIMITER_MAX_LOUDNESS}
  FXMASTERINGLIMITER_DEFAULT_LOUDNESS = 1000;
  {$EXTERNALSYM FXMASTERINGLIMITER_DEFAULT_LOUDNESS}


  // Reverb parameter bounds (inclusive), used with FXReverb:
  FXREVERB_MIN_DIFFUSION              = 0.0;
  {$EXTERNALSYM FXREVERB_MIN_DIFFUSION}
  FXREVERB_MAX_DIFFUSION              = 1.0;
  {$EXTERNALSYM FXREVERB_MAX_DIFFUSION}
  FXREVERB_DEFAULT_DIFFUSION          = 0.9;
  {$EXTERNALSYM FXREVERB_DEFAULT_DIFFUSION}

  FXREVERB_MIN_ROOMSIZE               = 0.1;
  {$EXTERNALSYM FXREVERB_MIN_ROOMSIZE}
  FXREVERB_MAX_ROOMSIZE               = 1.0;
  {$EXTERNALSYM FXREVERB_MAX_ROOMSIZE}
  FXREVERB_DEFAULT_ROOMSIZE           = 0.6;
  {$EXTERNALSYM FXREVERB_DEFAULT_ROOMSIZE}


  // Echo initialization data/parameter bounds (inclusive), used with FXEcho:
  FXECHO_MIN_WETDRYMIX                = 0.0;
  {$EXTERNALSYM FXECHO_MIN_WETDRYMIX}
  FXECHO_MAX_WETDRYMIX                = 1.0;
  {$EXTERNALSYM FXECHO_MAX_WETDRYMIX}
  FXECHO_DEFAULT_WETDRYMIX            = 0.5;
  {$EXTERNALSYM FXECHO_DEFAULT_WETDRYMIX}

  FXECHO_MIN_FEEDBACK                 = 0.0;
  {$EXTERNALSYM FXECHO_MIN_FEEDBACK}
  FXECHO_MAX_FEEDBACK                 = 1.0;
  {$EXTERNALSYM FXECHO_MAX_FEEDBACK}
  FXECHO_DEFAULT_FEEDBACK             = 0.5;
  {$EXTERNALSYM FXECHO_DEFAULT_FEEDBACK}

  FXECHO_MIN_DELAY                    = 1.0;
  {$EXTERNALSYM FXECHO_MIN_DELAY}
  FXECHO_MAX_DELAY                    = 2000.0;
  {$EXTERNALSYM FXECHO_MAX_DELAY}
  FXECHO_DEFAULT_DELAY                = 500.0;
  {$EXTERNALSYM FXECHO_DEFAULT_DELAY}


  //--------------<D-A-T-A---T-Y-P-E-S>---------------------------------------//
  // #pragma pack(push, 1) // set packing alignment to ensure consistency across arbitrary build environments
  // Within Delphi the default alignment is 8 bytes (quad word), unless the project alignment settings are changed.
  // Disable field aligned. All record and class structures will be packed.
  {$ALIGN 1}


type
  // EQ parameters (4 bands), used with IXAPOParameters.SetParameters:
  // The EQ supports only FLOAT32 audio foramts.
  // The framerate must be within [22000, 48000] Hz.
  PFXEQ_PARAMETERS = ^FXEQ_PARAMETERS;
  FXEQ_PARAMETERS = record
    FrequencyCenter0: Single;        // center frequency in Hz, band 0
    Gain0: Single;                   // boost/cut
    Bandwidth0: Single;              // bandwidth, region of EQ is center frequency +/- bandwidth/2
    FrequencyCenter1: Single;        // band 1
    Gain1: Single;
    Bandwidth1: Single;
    FrequencyCenter2: Single;        // band 2
    Gain2: Single;
    Bandwidth2: Single;
    FrequencyCenter3: Single;        // band 3
    Gain3: Single;
    Bandwidth3: Single;
  end;
  {$EXTERNALSYM FXEQ_PARAMETERS}


  // Mastering limiter parameters, used with IXAPOParameters.SetParameters:
  // The mastering limiter supports only FLOAT32 audio formats.
  PFXMASTERINGLIMITER_PARAMETERS = ^FXMASTERINGLIMITER_PARAMETERS;
  FXMASTERINGLIMITER_PARAMETERS = record
    Release: UINT32;                // release time (tuning factor with no specific units)
    Loudness: UINT32;               // loudness target (threshold)
  end;
  {$EXTERNALSYM FXMASTERINGLIMITER_PARAMETERS}


  // Reverb parameters, used with IXAPOParameters.SetParameters:
  // The reverb supports only FLOAT32 audio formats with the following
  // channel configurations:
  //     Input: Mono   Output: Mono
  //     Input: Stereo Output: Stereo
  PFXREVERB_PARAMETERS = ^FXREVERB_PARAMETERS;
  FXREVERB_PARAMETERS = record
    Diffusion: Single;               // diffusion
    RoomSize: Single;                // room size
  end;
  {$EXTERNALSYM FXREVERB_PARAMETERS}


  // Echo initialization data, used with CreateFX:
  // Use of this structure is optional, the default MaxDelay is FXECHO_DEFAULT_DELAY.
  PFXECHO_INITDATA = ^FXECHO_INITDATA;
  FXECHO_INITDATA = record
    MaxDelay: Single;               // maximum delay (all channels) in milliseconds, must be within [FXECHO_MIN_DELAY, FXECHO_MAX_DELAY]
  end;
  {$EXTERNALSYM FXECHO_INITDATA}


  // Echo parameters, used with IXAPOParameters::SetParameters:
  // The echo supports only FLOAT32 audio formats.
  PFXECHO_PARAMETERS = ^FXECHO_PARAMETERS;
  FXECHO_PARAMETERS = record
    WetDryMix: Single;               // ratio of wet (processed) signal to dry (original) signal
    Feedback: Single;                // amount of output fed back into input
    Delay: Single;                   // delay (all channels) in milliseconds, must be within [FXECHO_MIN_DELAY, FXECHO_PARAMETERS.MaxDelay]
  end;
  {$EXTERNALSYM FXECHO_PARAMETERS}


  //--------------<F-U-N-C-T-I-O-N-S>-----------------------------------------//




  // creates instance of requested XAPO, use Release to free instance
  //  clsid            - ID of the effect to create. Use the __uuidof on the effect class name to get the CLSID for an effect.
  //                     For example, IID_FXReverb is the CLSID for the FXReverb effect.
  //  pEffect          - [out] Receives a pointer to the created XAPO instance. If CreateFX fails, pEffect is untouched.
  //  pInitData        - [in] effect-specific initialization parameters, may be NULL if InitDataByteSize == 0
  //  InitDataByteSize - [in] size of pInitData in bytes, may be 0 if pInitData is NULL
  //
  // Remarks
  //  The created XAPO will have a reference count of 1. Client code must call IUnknown.Release after passing the XAPO
  //  to XAudio2 to allow XAudio2 to dispose of the XAPO when it is no longer needed.
  //  Use IXAudio2.CreateSourceVoice or IXAudio2Voice.SetEffectChain to pass an XAPO to XAudio2.
  function CreateFX(clsid: TGuid;
                    out pEffect: IUnknown;
                    pInitData: Pointer = nil;
                    InitDataByteSize: UINT32 = 0): HRESULT; stdcall;
  {$EXTERNALSYM CreateFX}


  //#pragma pack(pop) // revert packing alignment
  // set back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

//---------------------------------<-EOF->----------------------------------//


  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation


{$WARN SYMBOL_PLATFORM OFF}
function CreateFX; external XAUDIO2_DLL name 'CreateFX' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.
