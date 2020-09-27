// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
// Module: WinApi.DirectX.XAudio2.XAudio2.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Declarations for the XAudio2 game audio API.
//              Windows 10 XAudio2.9 or later
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
// Source: xaudio2.h
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
unit WinApi.DirectX.XAudio2.XAudio2;

  {$HPPEMIT '#include "xaudio2.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinMM.MMReg,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'XAudio2.inc'}   // CHECK FOR DLL SPECS HERE!


 (***************************************************************************
 *
 * XAudio2 COM object class and interface IDs.
 *
 ***************************************************************************)

  // All structures defined in this file use tight field packing
  // #pragma pack(push, 1)
  // The default alignment is 8 bytes (quad word) for win64 and 1 byte for win32, unless the project alignment settings are changed.
  // Disable field aligned. All record and class structures will be packed.
  {$ALIGN 1}


type
  // Used in XAUDIO2_FILTER_PARAMETERS below
  PXAUDIO2_FILTER_TYPE = ^XAUDIO2_FILTER_TYPE;
  XAUDIO2_FILTER_TYPE = DWord;
  {$EXTERNALSYM XAUDIO2_FILTER_TYPE}
const
  LowPassFilter         = XAUDIO2_FILTER_TYPE(0);  // Attenuates frequencies above the cutoff frequency (state-variable filter).
  {$EXTERNALSYM LowPassFilter}
  BandPassFilter        = XAUDIO2_FILTER_TYPE(1);  // Attenuates frequencies outside a given range      (state-variable filter).
  {$EXTERNALSYM BandPassFilter}
  HighPassFilter        = XAUDIO2_FILTER_TYPE(2);  // Attenuates frequencies below the cutoff frequency (state-variable filter).
  {$EXTERNALSYM HighPassFilter}
  NotchFilter           = XAUDIO2_FILTER_TYPE(3);  // Attenuates frequencies inside a given range       (state-variable filter).
  {$EXTERNALSYM NotchFilter}
  LowPassOnePoleFilter  = XAUDIO2_FILTER_TYPE(4);  // Attenuates frequencies above the cutoff frequency (one-pole filter, XAUDIO2_FILTER_PARAMETERS.OneOverQ has no effect)
  {$EXTERNALSYM LowPassOnePoleFilter}
  HighPassOnePoleFilter = XAUDIO2_FILTER_TYPE(5);  // Attenuates frequencies below the cutoff frequency (one-pole filter, XAUDIO2_FILTER_PARAMETERS.OneOverQ has no effect)
  {$EXTERNALSYM HighPassOnePoleFilter}

const

  (**************************************************************************
  *
  * XAudio2 constants, flags and error codes.
  *
  **************************************************************************)

  // Numeric boundary values
  XAUDIO2_MAX_BUFFER_BYTES            = $80000000;      // Maximum bytes allowed in a source buffer
  {$EXTERNALSYM XAUDIO2_MAX_BUFFER_BYTES}
  XAUDIO2_MAX_QUEUED_BUFFERS          = 64;             // Maximum buffers allowed in a voice queue
  {$EXTERNALSYM XAUDIO2_MAX_QUEUED_BUFFERS}
  XAUDIO2_MAX_BUFFERS_SYSTEM          = 2;              // Maximum buffers allowed for system threads (Xbox 360 only)
  {$EXTERNALSYM XAUDIO2_MAX_BUFFERS_SYSTEM}
  XAUDIO2_MAX_AUDIO_CHANNELS          = 64;             // Maximum channels in an audio stream
  {$EXTERNALSYM XAUDIO2_MAX_AUDIO_CHANNELS}
  XAUDIO2_MIN_SAMPLE_RATE             = 1000;           // Minimum audio sample rate supported
  {$EXTERNALSYM XAUDIO2_MIN_SAMPLE_RATE}
  XAUDIO2_MAX_SAMPLE_RATE             = 200000;         // Maximum audio sample rate supported
  {$EXTERNALSYM XAUDIO2_MAX_SAMPLE_RATE}
  XAUDIO2_MAX_VOLUME_LEVEL            = 16777216.0;     // Maximum acceptable volume level (2^24)
  {$EXTERNALSYM XAUDIO2_MAX_VOLUME_LEVEL}
  XAUDIO2_MIN_FREQ_RATIO              = 0.0009765625;   //(1 div 1024.0); // Minimum SetFrequencyRatio argument
  {$EXTERNALSYM XAUDIO2_MIN_FREQ_RATIO}
  XAUDIO2_MAX_FREQ_RATIO              = 1024.0;         // Maximum MaxFrequencyRatio argument
  {$EXTERNALSYM XAUDIO2_MAX_FREQ_RATIO}
  XAUDIO2_DEFAULT_FREQ_RATIO          = 2.0;            // Default MaxFrequencyRatio argument
  {$EXTERNALSYM XAUDIO2_DEFAULT_FREQ_RATIO}
  XAUDIO2_MAX_FILTER_ONEOVERQ         = 1.5;            // Maximum XAUDIO2_FILTER_PARAMETERS.OneOverQ
  {$EXTERNALSYM XAUDIO2_MAX_FILTER_ONEOVERQ}
  XAUDIO2_MAX_FILTER_FREQUENCY        = 1.0;            // Maximum XAUDIO2_FILTER_PARAMETERS.Frequency
  {$EXTERNALSYM XAUDIO2_MAX_FILTER_FREQUENCY}
  XAUDIO2_MAX_LOOP_COUNT              = 254;            // Maximum non-infinite XAUDIO2_BUFFER.LoopCount
  {$EXTERNALSYM XAUDIO2_MAX_LOOP_COUNT}
  XAUDIO2_MAX_INSTANCES               = 8;              // Maximum simultaneous XAudio2 objects on Xbox 360
  {$EXTERNALSYM XAUDIO2_MAX_INSTANCES}

  // For XMA voices on Xbox 360 there is an additional restriction on the MaxFrequencyRatio
  // argument and the voice's sample rate: the product of these numbers cannot exceed 600000
  // for one-channel voices or 300000 for voices with more than one channel.
  XAUDIO2_MAX_RATIO_TIMES_RATE_XMA_MONO         = 600000;
  {$EXTERNALSYM XAUDIO2_MAX_RATIO_TIMES_RATE_XMA_MONO}
  XAUDIO2_MAX_RATIO_TIMES_RATE_XMA_MULTICHANNEL = 300000;
  {$EXTERNALSYM XAUDIO2_MAX_RATIO_TIMES_RATE_XMA_MULTICHANNEL}

  // Numeric values with special meanings
  XAUDIO2_COMMIT_NOW                  = 0;              // Used as an OperationSet argument
  {$EXTERNALSYM XAUDIO2_COMMIT_NOW}
  XAUDIO2_COMMIT_ALL                  = 0;              // Used in IXAudio2.CommitChanges
  {$EXTERNALSYM XAUDIO2_COMMIT_ALL}
  XAUDIO2_INVALID_OPSET               = UINT32(- 1);    // Not allowed for OperationSet arguments
  {$EXTERNALSYM XAUDIO2_INVALID_OPSET}
  XAUDIO2_NO_LOOP_REGION              = 0;              // Used in XAUDIO2_BUFFER.LoopCount
  {$EXTERNALSYM XAUDIO2_NO_LOOP_REGION}
  XAUDIO2_LOOP_INFINITE               = 255;            // Used in XAUDIO2_BUFFER.LoopCount
  {$EXTERNALSYM XAUDIO2_LOOP_INFINITE}
  XAUDIO2_DEFAULT_CHANNELS            = 0;              // Used in CreateMasteringVoice
  {$EXTERNALSYM XAUDIO2_DEFAULT_CHANNELS}
  XAUDIO2_DEFAULT_SAMPLERATE          = 0;              // Used in CreateMasteringVoice
  {$EXTERNALSYM XAUDIO2_DEFAULT_SAMPLERATE}

  // Flags
  XAUDIO2_DEBUG_ENGINE                = $0001;   // Used in XAudio2Create
  {$EXTERNALSYM XAUDIO2_DEBUG_ENGINE}
  XAUDIO2_VOICE_NOPITCH               = $0002;   // Used in IXAudio2::CreateSourceVoice
  {$EXTERNALSYM XAUDIO2_VOICE_NOPITCH}
  XAUDIO2_VOICE_NOSRC                 = $0004;   // Used in IXAudio2::CreateSourceVoice
  {$EXTERNALSYM XAUDIO2_VOICE_NOSRC}
  XAUDIO2_VOICE_USEFILTER             = $0008;   // Used in IXAudio2::CreateSource/SubmixVoice
  {$EXTERNALSYM XAUDIO2_VOICE_USEFILTER}
  XAUDIO2_PLAY_TAILS                  = $0020;   // Used in IXAudio2SourceVoice::Stop
  {$EXTERNALSYM XAUDIO2_PLAY_TAILS}
  XAUDIO2_END_OF_STREAM               = $0040;   // Used in XAUDIO2_BUFFER.Flags
  {$EXTERNALSYM XAUDIO2_END_OF_STREAM}
  XAUDIO2_SEND_USEFILTER              = $0080;   // Used in XAUDIO2_SEND_DESCRIPTOR.Flags
  {$EXTERNALSYM XAUDIO2_SEND_USEFILTER}
  XAUDIO2_VOICE_NOSAMPLESPLAYED       = $0100;   // Used in IXAudio2SourceVoice::GetState
  {$EXTERNALSYM XAUDIO2_VOICE_NOSAMPLESPLAYED}
  XAUDIO2_STOP_ENGINE_WHEN_IDLE       = $2000;   // Used in XAudio2Create to force the engine to Stop when no source voices are Started, and Start when a voice is Started
  {$EXTERNALSYM XAUDIO2_STOP_ENGINE_WHEN_IDLE}
  XAUDIO2_1024_QUANTUM                = $8000;   // Used in XAudio2Create to specify nondefault processing quantum of 21.33 ms (1024 samples at 48KHz)
  {$EXTERNALSYM XAUDIO2_1024_QUANTUM}
  XAUDIO2_NO_VIRTUAL_AUDIO_CLIENT     = $10000;  // Used in CreateMasteringVoice to create a virtual audio client
  {$EXTERNALSYM XAUDIO2_NO_VIRTUAL_AUDIO_CLIENT}

  // Default parameters for the built-in filter
  XAUDIO2_DEFAULT_FILTER_TYPE         = LowPassFilter;                 // Specifies the default filter type to be used with voices and voice sends.
  {$EXTERNALSYM XAUDIO2_DEFAULT_FILTER_TYPE}
  XAUDIO2_DEFAULT_FILTER_FREQUENCY    = XAUDIO2_MAX_FILTER_FREQUENCY;  // Specifies the default filter frequency to be used with voices and voice sends.
  {$EXTERNALSYM XAUDIO2_DEFAULT_FILTER_FREQUENCY}
  XAUDIO2_DEFAULT_FILTER_ONEOVERQ     = 1.0;                           // Specifies the default filter rate of decay to be used with voices and voice sends.
  {$EXTERNALSYM XAUDIO2_DEFAULT_FILTER_ONEOVERQ}

  // Internal XAudio2 constants
  // The audio frame quantum can be calculated by reducing the fraction:
  //     SamplesPerAudioFrame / SamplesPerSecond
  XAUDIO2_QUANTUM_NUMERATOR           = 1;    // On Windows, XAudio2 processes audio
  {$EXTERNALSYM XAUDIO2_QUANTUM_NUMERATOR}
  XAUDIO2_QUANTUM_DENOMINATOR         = 100;  //  in 10ms chunks (= 1/100 seconds)
  {$EXTERNALSYM XAUDIO2_QUANTUM_DENOMINATOR}

  XAUDIO2_QUANTUM_MS                  = 10;   //  (1000.0 * XAUDIO2_QUANTUM_NUMERATOR div XAUDIO2_QUANTUM_DENOMINATOR);
  {$EXTERNALSYM XAUDIO2_QUANTUM_MS}

  // XAudio2 error codes
  FACILITY_XAUDIO2                    = $896;
  {$EXTERNALSYM FACILITY_XAUDIO2}
  XAUDIO2_E_INVALID_CALL              = $88960001;  // An API call or one of its arguments was illegal
  {$EXTERNALSYM XAUDIO2_E_INVALID_CALL}
  XAUDIO2_E_XMA_DECODER_ERROR         = $88960002;  // The XMA hardware suffered an unrecoverable error
  {$EXTERNALSYM XAUDIO2_E_XMA_DECODER_ERROR}
  XAUDIO2_E_XAPO_CREATION_FAILED      = $88960003;  // XAudio2 failed to initialize an XAPO effect
  {$EXTERNALSYM XAUDIO2_E_XAPO_CREATION_FAILED}
  XAUDIO2_E_DEVICE_INVALIDATED        = $88960004;  // An audio device became unusable (unplugged, etc)
  {$EXTERNALSYM XAUDIO2_E_DEVICE_INVALIDATED}


  (**************************************************************************
  *
  * XAudio2 structures and enumerations.
  *
  **************************************************************************)

  // Used in XAudio2Create, specifies which CPU(s) to use.
type

  PXAUDIO2_PROCESSOR = ^XAUDIO2_PROCESSOR;
  XAUDIO2_PROCESSOR = UINT32;
  {$EXTERNALSYM XAUDIO2_PROCESSOR}

const
  Processor1                          = $00000001;
  {$EXTERNALSYM Processor1}
  Processor2                          = $00000002;
  {$EXTERNALSYM Processor2}
  Processor3                          = $00000004;
  {$EXTERNALSYM Processor3}
  Processor4                          = $00000008;
  {$EXTERNALSYM Processor4}
  Processor5                          = $00000010;
  {$EXTERNALSYM Processor5}
  Processor6                          = $00000020;
  {$EXTERNALSYM Processor6}
  Processor7                          = $00000040;
  {$EXTERNALSYM Processor7}
  Processor8                          = $00000080;
  {$EXTERNALSYM Processor8}
  Processor9                          = $00000100;
  {$EXTERNALSYM Processor9}
  Processor10                         = $00000200;
  {$EXTERNALSYM Processor10}
  Processor11                         = $00000400;
  {$EXTERNALSYM Processor11}
  Processor12                         = $00000800;
  {$EXTERNALSYM Processor12}
  Processor13                         = $00001000;
  {$EXTERNALSYM Processor13}
  Processor14                         = $00002000;
  {$EXTERNALSYM Processor14}
  Processor15                         = $00004000;
  {$EXTERNALSYM Processor15}
  Processor16                         = $00008000;
  {$EXTERNALSYM Processor16}
  Processor17                         = $00010000;
  {$EXTERNALSYM Processor17}
  Processor18                         = $00020000;
  {$EXTERNALSYM Processor18}
  Processor19                         = $00040000;
  {$EXTERNALSYM Processor19}
  Processor20                         = $00080000;
  {$EXTERNALSYM Processor20}
  Processor21                         = $00100000;
  {$EXTERNALSYM Processor21}
  Processor22                         = $00200000;
  {$EXTERNALSYM Processor22}
  Processor23                         = $00400000;
  {$EXTERNALSYM Processor23}
  Processor24                         = $00800000;
  {$EXTERNALSYM Processor24}
  Processor25                         = $01000000;
  {$EXTERNALSYM Processor25}
  Processor26                         = $02000000;
  {$EXTERNALSYM Processor26}
  Processor27                         = $04000000;
  {$EXTERNALSYM Processor27}
  Processor28                         = $08000000;
  {$EXTERNALSYM Processor28}
  Processor29                         = $10000000;
  {$EXTERNALSYM Processor29}
  Processor30                         = $20000000;
  {$EXTERNALSYM Processor30}
  Processor31                         = $40000000;
  {$EXTERNALSYM Processor31}
  Processor32                         = $80000000;
  {$EXTERNALSYM Processor32}
  XAUDIO2_ANY_PROCESSOR               = UINT($FFFFFFFF);
  {$EXTERNALSYM XAUDIO2_ANY_PROCESSOR}

// #if (NTDDI_VERSION >= NTDDI_WIN10_19H1)
// This value indicates that XAudio2 will choose the default processor by itself.
// The actual value chosen may vary depending on the hardware platform.
  XAUDIO2_USE_DEFAULT_PROCESSOR       = $00000000;
//#endif
  {$EXTERNALSYM XAUDIO2_USE_DEFAULT_PROCESSOR}

// This definition is included for backwards compatibilty. Implementations targeting Games and WIN10_19H1 and later, should use
// XAUDIO2_USE_DEFAULT_PROCESSOR instead to let XAudio2 select the appropriate default processor for the hardware platform.
  XAUDIO2_DEFAULT_PROCESSOR           = Processor1;
  {$EXTERNALSYM XAUDIO2_DEFAULT_PROCESSOR}

const

  // Values for the TraceMask and BreakMask bitmaps.  Only ERRORS and WARNINGS
  // are valid in BreakMask. WARNINGS implies ERRORS, DETAIL implies INFO, and
  // FUNC_CALLS implies API_CALLS. By default, TraceMask is ERRORS and WARNINGS
  // and all the other settings are zero.

  XAUDIO2_LOG_ERRORS                  = $0001;  // For handled errors with serious effects.
  {$EXTERNALSYM XAUDIO2_LOG_ERRORS}
  XAUDIO2_LOG_WARNINGS                = $0002;  // For handled errors that may be recoverable.
  {$EXTERNALSYM XAUDIO2_LOG_WARNINGS}
  XAUDIO2_LOG_INFO                    = $0004;  // Informational chit-chat (e.g. state changes).
  {$EXTERNALSYM XAUDIO2_LOG_INFO}
  XAUDIO2_LOG_DETAIL                  = $0008;  // More detailed chit-chat.
  {$EXTERNALSYM XAUDIO2_LOG_DETAIL}
  XAUDIO2_LOG_API_CALLS               = $0010;  // Public API function entries and exits.
  {$EXTERNALSYM XAUDIO2_LOG_API_CALLS}
  XAUDIO2_LOG_FUNC_CALLS              = $0020;  // Internal function entries and exits.
  {$EXTERNALSYM XAUDIO2_LOG_FUNC_CALLS}
  XAUDIO2_LOG_TIMING                  = $0040;  // Delays detected and other timing data.
  {$EXTERNALSYM XAUDIO2_LOG_TIMING}
  XAUDIO2_LOG_LOCKS                   = $0080;  // Usage of critical sections and mutexes.
  {$EXTERNALSYM XAUDIO2_LOG_LOCKS}
  XAUDIO2_LOG_MEMORY                  = $0100;  // Memory heap usage information.
  {$EXTERNALSYM XAUDIO2_LOG_MEMORY}
  XAUDIO2_LOG_STREAMING               = $1000;  // Audio streaming information.
  {$EXTERNALSYM XAUDIO2_LOG_STREAMING}

 (**************************************************************************
  *
  * Forward declarations for the XAudio2 interfaces (and classes).
  *
  **************************************************************************)
type

  PIXAudio2 =^IXAudio2;
  IXAudio2 = interface;

{$IFDEF _WINNT_WIN10}
  PIXAudio2Extension = ^IXAudio2Extension;
  IXAudio2Extension = interface;
{$ENDIF}

  PIXAudio2Voice = ^IXAudio2Voice;
  IXAudio2Voice = class;

  PIXAudio2SourceVoice = ^IXAudio2SourceVoice;
  IXAudio2SourceVoice = class;

  PIXAudio2SubmixVoice = ^IXAudio2SubmixVoice;
  IXAudio2SubmixVoice = class;

  PIXAudio2MasteringVoice = ^IXAudio2MasteringVoice;
  IXAudio2MasteringVoice = class;

  PIXAudio2EngineCallback = ^IXAudio2EngineCallback;
  IXAudio2EngineCallback = class;

  PIXAudio2VoiceCallback = ^IXAudio2VoiceCallback;
  IXAudio2VoiceCallback = class;



  // Returned by IXAudio2Voice.GetVoiceDetails
  PXAUDIO2_VOICE_DETAILS = ^XAUDIO2_VOICE_DETAILS;
  XAUDIO2_VOICE_DETAILS = record
    CreationFlags: UINT32;           // Flags the voice was created with.
    ActiveFlags: UINT32;             // Flags currently active.
    InputChannels: UINT32;           // Channels in the voice's input audio.
    InputSampleRate: UINT32;         // Sample rate of the voice's input audio.
  end;
  {$EXTERNALSYM XAUDIO2_VOICE_DETAILS}

  // Used in XAUDIO2_VOICE_SENDS below
  PXAUDIO2_SEND_DESCRIPTOR = ^XAUDIO2_SEND_DESCRIPTOR;
  XAUDIO2_SEND_DESCRIPTOR = record
    Flags: UINT32;                   // Either 0 or XAUDIO2_SEND_USEFILTER.
    pOutputVoice: IXAudio2Voice;     // This send's destination voice.
  end;
  {$EXTERNALSYM XAUDIO2_SEND_DESCRIPTOR}
  // If you don't want to use pointer array's
  TXAudio2SendDescriptor = array [0..65535] of XAUDIO2_SEND_DESCRIPTOR;
  {$EXTERNALSYM TXAudio2SendDescriptor}

  // Used in the voice creation functions and in IXAudio2Voice.SetOutputVoices
  PXAUDIO2_VOICE_SENDS = ^XAUDIO2_VOICE_SENDS;
  XAUDIO2_VOICE_SENDS = record
    SendCount: UINT32;                  // Number of sends from this voice.
    pSends: TXAudio2SendDescriptor;     // PXAUDIO2_SEND_DESCRIPTOR;   // Array of SendCount send descriptors.
  end;
  {$EXTERNALSYM XAUDIO2_VOICE_SENDS}

  // Used in XAUDIO2_EFFECT_CHAIN below
  PXAUDIO2_EFFECT_DESCRIPTOR = ^XAUDIO2_EFFECT_DESCRIPTOR;
  XAUDIO2_EFFECT_DESCRIPTOR = record
    pEffect: IUnknown;              // Pointer to the effect object's IUnknown interface.
    InitialState: BOOL;              // TRUE if the effect should begin in the enabled state.
    OutputChannels: UINT32;          // How many output channels the effect should produce.
  end;
  {$EXTERNALSYM XAUDIO2_EFFECT_DESCRIPTOR}
  // If you don't want to use pointer array's
  TXAudio2EffectDescriptor = array [0..65535] of XAUDIO2_EFFECT_DESCRIPTOR;
  {$EXTERNALSYM TXAudio2EffectDescriptor}

  // Used in the voice creation functions and in IXAudio2Voice.SetEffectChain
  PXAUDIO2_EFFECT_CHAIN = ^XAUDIO2_EFFECT_CHAIN;
  XAUDIO2_EFFECT_CHAIN = record
    EffectCount: UINT32;                              // Number of effects in this voice's effect chain.
    pEffectDescriptors: TXAudio2EffectDescriptor;     // PXAUDIO2_EFFECT_DESCRIPTOR;  // Array of effect descriptors.
  end;
  {$EXTERNALSYM XAUDIO2_EFFECT_CHAIN}

  // Declared at line 123 !
  //=======================
  // Used in XAUDIO2_FILTER_PARAMETERS below
//typedef enum XAUDIO2_FILTER_TYPE
//{
//    LowPassFilter,                      // Attenuates frequencies above the cutoff frequency (state-variable filter).
//    BandPassFilter,                     // Attenuates frequencies outside a given range      (state-variable filter).
//    HighPassFilter,                     // Attenuates frequencies below the cutoff frequency (state-variable filter).
//    NotchFilter,                        // Attenuates frequencies inside a given range       (state-variable filter).
//    LowPassOnePoleFilter,               // Attenuates frequencies above the cutoff frequency (one-pole filter, XAUDIO2_FILTER_PARAMETERS.OneOverQ has no effect)
//    HighPassOnePoleFilter               // Attenuates frequencies below the cutoff frequency (one-pole filter, XAUDIO2_FILTER_PARAMETERS.OneOverQ has no effect)
//} XAUDIO2_FILTER_TYPE;
//=========================

  // Used in IXAudio2Voice.Set/GetFilterParameters and Set/GetOutputFilterParameters
  PXAUDIO2_FILTER_PARAMETERS = ^XAUDIO2_FILTER_PARAMETERS;
  XAUDIO2_FILTER_PARAMETERS = record
    _Type: XAUDIO2_FILTER_TYPE;      // Filter type.
    Frequency: Single;               // Filter coefficient.
                                     //  must be >= 0 and <= XAUDIO2_MAX_FILTER_FREQUENCY
                                     //  See XAudio2CutoffFrequencyToRadians() for state-variable filter types and
                                     //  XAudio2CutoffFrequencyToOnePoleCoefficient() for one-pole filter types.
    OneOverQ: Single;                // Reciprocal of the filter's quality factor Q;
                                     //  must be > 0 and <= XAUDIO2_MAX_FILTER_ONEOVERQ.
                                     //  Has no effect for one-pole filters.
  end;
  {$EXTERNALSYM XAUDIO2_FILTER_PARAMETERS}

  // Used in IXAudio2SourceVoice.SubmitSourceBuffer
  PXAUDIO2_BUFFER = ^XAUDIO2_BUFFER;
  XAUDIO2_BUFFER = record
    Flags: UINT32;                   // Either 0 or XAUDIO2_END_OF_STREAM.
    AudioBytes: UINT32;              // Size of the audio data buffer in bytes.
    pAudioData: PByte;               // Pointer to the audio data buffer.
    PlayBegin: UINT32;               // First sample in this buffer to be played.
    PlayLength: UINT32;              // Length of the region to be played in samples,
                                     //  or 0 to play the whole buffer.
    LoopBegin: UINT32;               // First sample of the region to be looped.
    LoopLength: UINT32;              // Length of the desired loop region in samples,
                                     //  or 0 to loop the entire buffer.
    LoopCount: UINT32;               // Number of times to repeat the loop region,
                                     //  or XAUDIO2_LOOP_INFINITE to loop forever.
    pContext: Pointer;               // Context value to be passed back in callbacks.
  end;
  {$EXTERNALSYM XAUDIO2_BUFFER}

  // Used in IXAudio2SourceVoice.SubmitSourceBuffer when submitting XWMA data.
  // NOTE: If an XWMA sound is submitted in more than one buffer, each buffer's
  // pDecodedPacketCumulativeBytes[PacketCount-1] value must be subtracted from
  // all the entries in the next buffer's pDecodedPacketCumulativeBytes array.
  // And whether a sound is submitted in more than one buffer or not, the final
  // buffer of the sound should use the XAUDIO2_END_OF_STREAM flag, or else the
  // client must call IXAudio2SourceVoice.Discontinuity after submitting it.
  PXAUDIO2_BUFFER_WMA = ^XAUDIO2_BUFFER_WMA;
  XAUDIO2_BUFFER_WMA = record
    pDecodedPacketCumulativeBytes: PUINT32; {or TUINT32Array} // Decoded packet's cumulative size array.
                                             //  Each element is the number of bytes accumulated
                                             //  when the corresponding XWMA packet is decoded in
                                             //  order.  The array must have PacketCount elements.
    PacketCount: UINT32;                     // Number of XWMA packets submitted. Must be >= 1 and
                                             //  divide evenly into XAUDIO2_BUFFER.AudioBytes.
  end;
  {$EXTERNALSYM XAUDIO2_BUFFER_WMA}

  // Returned by IXAudio2SourceVoice.GetState
  PXAUDIO2_VOICE_STATE = ^XAUDIO2_VOICE_STATE;
  {$EXTERNALSYM XAUDIO2_VOICE_STATE}
  XAUDIO2_VOICE_STATE = record
    pCurrentBufferContext: Pointer;  // The pContext value provided in the XAUDIO2_BUFFER
                                     //  that is currently being processed, or NULL if
                                     //  there are no buffers in the queue.
    BuffersQueued: UINT32;           // Number of buffers currently queued on the voice
                                     //  (including the one that is being processed).
    SamplesPlayed: UINT64;           // Total number of samples produced by the voice since
                                     //  it began processing the current audio stream.
                                     //  If XAUDIO2_VOICE_NOSAMPLESPLAYED is specified
                                     //  in the call to IXAudio2SourceVoice::GetState,
                                     //  this member will not be calculated, saving CPU.
  end;

  // Returned by IXAudio2.GetPerformanceData
  PXAUDIO2_PERFORMANCE_DATA = ^XAUDIO2_PERFORMANCE_DATA;
  XAUDIO2_PERFORMANCE_DATA = record
                                          // CPU usage information
    AudioCyclesSinceLastQuery: UINT64;    // CPU cycles spent on audio processing since the
                                          //  last call to StartEngine or GetPerformanceData.
    TotalCyclesSinceLastQuery: UINT64;    // Total CPU cycles elapsed since the last call
                                          //  (only counts the CPU XAudio2 is running on).
    MinimumCyclesPerQuantum: UINT32;      // Fewest CPU cycles spent processing any one
                                          //  audio quantum since the last call.
    MaximumCyclesPerQuantum: UINT32;      // Most CPU cycles spent processing any one
                                          //  audio quantum since the last call.
                                          // Memory usage information
    MemoryUsageInBytes: UINT32;           // Total heap space currently in use.
                                          // Audio latency and glitching information
    CurrentLatencyInSamples: UINT32;      // Minimum delay from when a sample is read from a
                                          //  source buffer to when it reaches the speakers.
    GlitchesSinceEngineStarted: UINT32;   // Audio dropouts since the engine was started.
                                          // Data about XAudio2's current workload
    ActiveSourceVoiceCount: UINT32;       // Source voices currently playing.
    TotalSourceVoiceCount: UINT32;        // Source voices currently existing.
    ActiveSubmixVoiceCount: UINT32;       // Submix voices currently playing/existing.
    ActiveResamplerCount: UINT32;         // Resample xAPOs currently active.
    ActiveMatrixMixCount: UINT32;         // MatrixMix xAPOs currently active.
                                          // Usage of the hardware XMA decoder (Xbox 360 only)
    ActiveXmaSourceVoices: UINT32;        // Number of source voices decoding XMA data.
    ActiveXmaStreams: UINT32;             // A voice can use more than one XMA stream.
  end;
  {$EXTERNALSYM XAUDIO2_PERFORMANCE_DATA}

  // Used in IXAudio2.SetDebugConfiguration
  PXAUDIO2_DEBUG_CONFIGURATION = ^XAUDIO2_DEBUG_CONFIGURATION;
  XAUDIO2_DEBUG_CONFIGURATION = record
    TraceMask: UINT32;               // Bitmap of enabled debug message types.
    BreakMask: UINT32;               // Message types that will break into the debugger.
    LogThreadID: BOOL;               // Whether to log the thread ID with each message.
    LogFileline: BOOL;               // Whether to log the source file and line number.
    LogFunctionName: BOOL;           // Whether to log the function name.
    LogTiming: BOOL;                 // Whether to log message timestamps.
  end;
  {$EXTERNALSYM XAUDIO2_DEBUG_CONFIGURATION}


  (**************************************************************************
  *
  * IXAudio2: Top-level XAudio2 COM interface.
  *
  **************************************************************************)


  // Interface IXAudio2
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IXAudio2);'}
  {$EXTERNALSYM IXAudio2}
  IXAudio2 = interface(IUnknown)
{$IFDEF _WINNT_WIN10}
    ['{2B02E3CF-2E0B-4ec3-BE45-1B2A3FE7210D}']  // XAudio2 v 2.9
{$ELSE}
    ['{60d8dac8-5aa1-4e8e-b597-2f5e2883d484}']  // XAudio2 v 2.8
{$ENDIF}
    // NAME: IXAudio2.RegisterForCallbacks
    // DESCRIPTION: Adds a new client to receive XAudio2's engine callbacks.
    //
    // ARGUMENTS:
    //  pCallback - Callback interface to be called during each processing pass.
    //
    function RegisterForCallbacks(pCallback: IXAudio2EngineCallback): HRESULT; stdcall;

    // NAME: IXAudio2.UnregisterForCallbacks
    // DESCRIPTION: Removes an existing receiver of XAudio2 engine callbacks.
    //
    // ARGUMENTS:
    //  pCallback - Previously registered callback interface to be removed.
    //
    procedure UnregisterForCallbacks(pCallback: IXAudio2EngineCallback); stdcall;

    // NAME: IXAudio2.CreateSourceVoice
    // DESCRIPTION: Creates and configures a source voice.
    //
    // ARGUMENTS:
    //  ppSourceVoice - Returns the new object's IXAudio2SourceVoice interface.
    //  pSourceFormat - Format of the audio that will be fed to the voice.
    //  Flags - XAUDIO2_VOICE flags specifying the source voice's behavior.
    //  MaxFrequencyRatio - Maximum SetFrequencyRatio argument to be allowed.
    //  pCallback - Optional pointer to a client-provided callback interface.
    //  pSendList - Optional list of voices this voice should send audio to.
    //  pEffectChain - Optional list of effects to apply to the audio data.
    //
    function CreateSourceVoice(out ppSourceVoice: IXAudio2SourceVoice;
                               pSourceFormat: TWAVEFORMATEX;
                               Flags: UINT32 = 0;
                               MaxFrequencyRatio: Single = XAUDIO2_DEFAULT_FREQ_RATIO;
                               pCallback: IXAudio2VoiceCallback = nil;
                               pSendList: PXAUDIO2_VOICE_SENDS = nil;
                               pEffectChain: PXAUDIO2_EFFECT_CHAIN = nil): HRESULT; stdcall;

    // NAME: IXAudio2.CreateSubmixVoice
    // DESCRIPTION: Creates and configures a submix voice.
    //
    // ARGUMENTS:
    //  ppSubmixVoice - Returns the new object's IXAudio2SubmixVoice interface.
    //  InputChannels - Number of channels in this voice's input audio data.
    //  InputSampleRate - Sample rate of this voice's input audio data.
    //  Flags - XAUDIO2_VOICE flags specifying the submix voice's behavior.
    //  ProcessingStage - Arbitrary number that determines the processing order.
    //  pSendList - Optional list of voices this voice should send audio to.
    //  pEffectChain - Optional list of effects to apply to the audio data.
    //
    function CreateSubmixVoice(out ppSubmixVoice: IXAudio2SubmixVoice;
                               InputChannels: UINT32;
                               InputSampleRate: UINT32;
                               Flags: UINT32 = 0;
                               ProcessingStage: UINT32 = 0;
                               pSendList: PXAUDIO2_VOICE_SENDS = nil;
                               pEffectChain: PXAUDIO2_EFFECT_CHAIN = nil): HRESULT; stdcall;


    // NAME: IXAudio2.CreateMasteringVoice
    // DESCRIPTION: Creates and configures a mastering voice.
    //
    // ARGUMENTS:
    //  ppMasteringVoice - Returns the new object's IXAudio2MasteringVoice interface.
    //  InputChannels - Number of channels in this voice's input audio data.
    //  InputSampleRate - Sample rate of this voice's input audio data.
    //  Flags - XAUDIO2_VOICE flags specifying the mastering voice's behavior.
    //  szDeviceId - Identifier of the device to receive the output audio.
    //  pEffectChain - Optional list of effects to apply to the audio data.
    //  StreamCategory - The audio stream category to use for this mastering voice
    //
    function CreateMasteringVoice(out ppMasteringVoice: IXAudio2MasteringVoice;
                                  InputChannels: UINT32 = XAUDIO2_DEFAULT_CHANNELS;
                                  InputSampleRate: UINT32 = XAUDIO2_DEFAULT_SAMPLERATE;
                                  Flags: UINT32 = 0;
                                  szDeviceId: LPCWSTR = nil;
                                  pEffectChain: PXAUDIO2_EFFECT_CHAIN = nil;
                                  StreamCategory: AUDIO_STREAM_CATEGORY = AudioCategory_GameEffects): HRESULT; stdcall;

    // NAME: IXAudio2.StartEngine
    // DESCRIPTION: Creates and starts the audio processing thread.
    //
    function StartEngine(): HRESULT; stdcall;

    // NAME: IXAudio2.StopEngine
    // DESCRIPTION: Stops and destroys the audio processing thread.
    //
    procedure StopEngine(); stdcall;

    // NAME: IXAudio2.CommitChanges
    // DESCRIPTION: Atomically applies a set of operations previously tagged
    //              with a given identifier.
    //
    // ARGUMENTS:
    //  OperationSet - Identifier of the set of operations to be applied.
    //
    function CommitChanges(OperationSet: UINT32): HRESULT; stdcall;

    // NAME: IXAudio2.GetPerformanceData
    // DESCRIPTION: Returns current resource usage details: memory, CPU, etc.
    //
    // ARGUMENTS:
    //  pPerfData - Returns the performance data structure.
    //
    procedure GetPerformanceData(out pPerfData: XAUDIO2_PERFORMANCE_DATA); stdcall;

    // NAME: IXAudio2.SetDebugConfiguration
    // DESCRIPTION: Configures XAudio2's debug output (in debug builds only).
    //
    // ARGUMENTS:
    //  pDebugConfiguration - Structure describing the debug output behavior.
    //  pReserved - Optional parameter; must be NULL.
    //
    procedure SetDebugConfiguration(pDebugConfiguration: XAUDIO2_DEBUG_CONFIGURATION;
                                    pReserved: Pointer = nil); stdcall;

  end;
  IID_IXAudio2 = IXAudio2;
  {$EXTERNALSYM IID_IXAudio2}


{$IFDEF _WINNT_WIN10}

  // Interface IXAudio2Extension
  // ===========================
  // This interface extends IXAudio2 with additional functionality.
  // Use IXAudio2.QueryInterface to obtain a pointer to this interface.
  // Only IID_IUnknown, IID_IXAudio2 and IID_IXaudio2Extension are supported.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IXAudio2Extension);'}
  {$EXTERNALSYM IXAudio2Extension}
  IXAudio2Extension = interface(IUnknown)
    ['{84ac29bb-d619-44d2-b197-e4acf7df3ed6}']

    // NAME: IXAudio2::GetProcessingQuantum
    // DESCRIPTION: Returns the processing quantum
    //              quantumMilliseconds = (1000.0f * quantumNumerator / quantumDenominator)
    //
    // ARGUMENTS:
    //  quantumNumerator - Quantum numerator
    //  quantumDenominator - Quantum denominator
    //
    procedure GetProcessingQuantum(quantumNumerator: UINT32;
                                   quantumDenominator: UINT32); stdcall;

    // NAME: IXAudio2.GetProcessor
    // DESCRIPTION: Returns the number of the processor used by XAudio2
    //
    // ARGUMENTS:
    //  processor - Non-zero Processor number
    //
    procedure GetProcessor(processor: XAUDIO2_PROCESSOR);

  end;
  IID_IXAudio2Extension = IXAudio2Extension;
  {$EXTERNALSYM IID_IXAudio2Extension}

{$ENDIF}   // Win 10


  (**************************************************************************
  *
  * IXAudio2Voice: Base voice management interface.
  *
  **************************************************************************)
  //
  // Delphi, Note: As you can see, this class is defined as interface in C++ with the DECLARE_INTERFACE macro.
  //               However, this is not the case: This 'interface' is a pure virtual base class rather than an
  //               interface and NOT derived from IUnknown!

  {$EXTERNALSYM IXAudio2Voice}
  IXAudio2Voice = class(TObject)

    // NAME: IXAudio2Voice.GetVoiceDetails
    // DESCRIPTION: Returns the basic characteristics of this voice.
    //
    // ARGUMENTS:
    //  pVoiceDetails - Returns the voice's details.
    procedure GetVoiceDetails(pVoiceDetails: XAUDIO2_VOICE_DETAILS); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetOutputVoices
    // DESCRIPTION: Replaces the set of submix/mastering voices that receive
    //              this voice's output.
    //
    // ARGUMENTS:
    //  pSendList - Optional list of voices this voice should send audio to.
    function SetOutputVoices(pSendList: XAUDIO2_VOICE_SENDS): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetEffectChain
    // DESCRIPTION: Replaces this voice's current effect chain with a new one.
    //
    // ARGUMENTS:
    //  pEffectChain - Structure describing the new effect chain to be used.
    function SetEffectChain(pEffectChain: XAUDIO2_EFFECT_CHAIN): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.EnableEffect
    // DESCRIPTION: Enables an effect in this voice's effect chain.
    //
    // ARGUMENTS:
    //  EffectIndex - Index of an effect within this voice's effect chain.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function EnableEffect(EffectIndex: UINT32;
                          OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.DisableEffect
    // DESCRIPTION: Disables an effect in this voice's effect chain.
    //
    // ARGUMENTS:
    //  EffectIndex - Index of an effect within this voice's effect chain.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function DisableEffect(EffectIndex: UINT32;
                           OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetEffectState
    // DESCRIPTION: Returns the running state of an effect.
    //
    // ARGUMENTS:
    //  EffectIndex - Index of an effect within this voice's effect chain.
    //  pEnabled - Returns the enabled/disabled state of the given effect.
    procedure GetEffectState(EffectIndex: UINT32;
                             out pEnabled: BOOL); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetEffectParameters
    // DESCRIPTION: Sets effect-specific parameters.
    //
    // REMARKS: Unlike IXAPOParameters.SetParameters, this method may
    //          be called from any thread.  XAudio2 implements
    //          appropriate synchronization to copy the parameters to the
    //          realtime audio processing thread.
    //
    // ARGUMENTS:
    //  EffectIndex - Index of an effect within this voice's effect chain.
    //  pParameters - Pointer to an effect-specific parameters block.
    //  ParametersByteSize - Size of the pParameters array in bytes.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetEffectParameters(EffectIndex: UINT32;
                                 pParameters: Pointer;
                                 ParametersByteSize: UINT32;
                                 OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetEffectParameters
    // DESCRIPTION: Obtains the current effect-specific parameters.
    //
    // ARGUMENTS:
    //  EffectIndex - Index of an effect within this voice's effect chain.
    //  pParameters - Returns the current values of the effect-specific parameters.
    //  ParametersByteSize - Size of the pParameters array in bytes.
    function GetEffectParameters(EffectIndex: UINT32;
                                 out pParameters: Pointer;
                                 out ParametersByteSize: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetFilterParameters
    // DESCRIPTION: Sets this voice's filter parameters.
    //
    // ARGUMENTS:
    //  pParameters - Pointer to the filter's parameter structure.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetFilterParameters(pParameters: XAUDIO2_FILTER_PARAMETERS;
                                 OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetFilterParameters
    // DESCRIPTION: Returns this voice's current filter parameters.
    //
    // ARGUMENTS:
    //  pParameters - Returns the filter parameters.
    procedure GetFilterParameters(out pParameters: XAUDIO2_FILTER_PARAMETERS); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetOutputFilterParameters
    // DESCRIPTION: Sets the filter parameters on one of this voice's sends.
    //
    // ARGUMENTS:
    //  pDestinationVoice - Destination voice of the send whose filter parameters will be set.
    //  pParameters - Pointer to the filter's parameter structure.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetOutputFilterParameters(pDestinationVoice: IXAudio2Voice;
                                       pParameters: XAUDIO2_FILTER_PARAMETERS;
                                       OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetOutputFilterParameters
    // DESCRIPTION: Returns the filter parameters from one of this voice's sends.
    //
    // ARGUMENTS:
    //  pDestinationVoice - Destination voice of the send whose filter parameters will be read.
    //  pParameters - Returns the filter parameters.
    procedure GetOutputFilterParameters(pDestinationVoice: IXAudio2Voice;
                                        out pParameters: XAUDIO2_FILTER_PARAMETERS); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetVolume
    // DESCRIPTION: Sets this voice's overall volume level.
    //
    // ARGUMENTS:
    //  Volume - New overall volume level to be used, as an amplitude factor.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetVolume(Volume: Single;
                       OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetVolume
    // DESCRIPTION: Obtains this voice's current overall volume level.
    //
    // ARGUMENTS:
    //  pVolume: Returns the voice's current overall volume level.
    procedure GetVolume(out pVolume: Single); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetChannelVolumes
    // DESCRIPTION: Sets this voice's per-channel volume levels.
    //
    // ARGUMENTS:
    //  Channels - Used to confirm the voice's channel count.
    //  pVolumes - Array of per-channel volume levels to be used.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetChannelVolumes(Channels: UINT32;
                               pVolumes: PSingle;
                               OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetChannelVolumes
    // DESCRIPTION: Returns this voice's current per-channel volume levels.
    //
    // ARGUMENTS:
    //  Channels - Used to confirm the voice's channel count.
    //  pVolumes - Returns an array of the current per-channel volume levels.
    procedure GetChannelVolumes(Channels: UINT32;
                                {out} pVolumes: PSingle); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.SetOutputMatrix
    // DESCRIPTION: Sets the volume levels used to mix from each channel of this
    //              voice's output audio to each channel of a given destination
    //              voice's input audio.
    //
    // ARGUMENTS:
    //  pDestinationVoice - The destination voice whose mix matrix to change.
    //  SourceChannels - Used to confirm this voice's output channel count
    //   (the number of channels produced by the last effect in the chain).
    //  DestinationChannels - Confirms the destination voice's input channels.
    //  pLevelMatrix - Array of [SourceChannels * DestinationChannels] send
    //   levels.  The level used to send from source channel S to destination
    //   channel D should be in pLevelMatrix[S + SourceChannels * D].
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function SetOutputMatrix(pDestinationVoice: IXAudio2Voice;
                             SourceChannels: UINT32;
                             DestinationChannels: UINT32;
                             pLevelMatrix: PSingle;
                             OperationSet: UINT32): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.GetOutputMatrix
    // DESCRIPTION: Obtains the volume levels used to send each channel of this
    //              voice's output audio to each channel of a given destination
    //              voice's input audio.
    //
    // ARGUMENTS:
    //  pDestinationVoice - The destination voice whose mix matrix to obtain.
    //  SourceChannels - Used to confirm this voice's output channel count
    //   (the number of channels produced by the last effect in the chain).
    //  DestinationChannels - Confirms the destination voice's input channels.
    //  pLevelMatrix - Array of send levels, as above.
    procedure GetOutputMatrix(pDestinationVoice: IXAudio2Voice;
                              SourceChannels: UINT32;
                              DestinationChannels: UINT32;
                              {out} pLevelMatrix: PSingle); virtual; stdcall; abstract;

    // NAME: IXAudio2Voice.DestroyVoice
    // DESCRIPTION: Destroys this voice, stopping it if necessary and removing
    //              it from the XAudio2 graph.
    procedure DestroyVoice(); virtual; stdcall; abstract;

  end;



  (**************************************************************************
  *
  * IXAudio2SourceVoice: Source voice management interface.
  *
  **************************************************************************)

  {$EXTERNALSYM IXAudio2SourceVoice}
  IXAudio2SourceVoice = class(IXAudio2Voice)

    // Methods from IXAudio2Voice base interface

    // Declare_IXAudio2Voice_Methods();


    // NAME: IXAudio2SourceVoice.Start
    // DESCRIPTION: Makes this voice start consuming and processing audio.
    //
    // ARGUMENTS:
    //  Flags - Flags controlling how the voice should be started.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    function Start(Flags: UINT32 = 0;
                   OperationSet: UINT32 = XAUDIO2_COMMIT_NOW): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.Stop
    // DESCRIPTION: Makes this voice stop consuming audio.
    //
    // ARGUMENTS:
    //  Flags - Flags controlling how the voice should be stopped.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    //
    function Stop(Flags: UINT32 = 0;
                  OperationSet: UINT32 = XAUDIO2_COMMIT_NOW): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.SubmitSourceBuffer
    // DESCRIPTION: Adds a new audio buffer to this voice's input queue.
    //
    // ARGUMENTS:
    //  pBuffer - Pointer to the buffer structure to be queued.
    //  pBufferWMA - Additional structure used only when submitting XWMA data.
    //
    function SubmitSourceBuffer(pBuffer: XAUDIO2_BUFFER;
                                pBufferWMA: PXAUDIO2_BUFFER_WMA = nil): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.FlushSourceBuffers
    // DESCRIPTION: Removes all pending audio buffers from this voice's queue.
    //
    function FlushSourceBuffers(): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.Discontinuity
    // DESCRIPTION: Notifies the voice of an intentional break in the stream of
    //              audio buffers (e.g. the end of a sound), to prevent XAudio2
    //              from interpreting an empty buffer queue as a glitch.
    //
    function Discontinuity(): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.ExitLoop
    // DESCRIPTION: Breaks out of the current loop when its end is reached.
    //
    // ARGUMENTS:
    //  OperationSet - Used to identify this call as part of a deferred batch.
    //
    function ExitLoop(OperationSet: UINT32 = XAUDIO2_COMMIT_NOW): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.GetState
    // DESCRIPTION: Returns the number of buffers currently queued on this voice,
    //              the pContext value associated with the currently processing
    //              buffer (if any), and other voice state information.
    //
    // ARGUMENTS:
    //  pVoiceState - Returns the state information.
    //  Flags - Flags controlling what voice state is returned.
    //
    procedure GetState(out pVoiceState: XAUDIO2_VOICE_STATE;
                       Flags: UINT32 = 0); virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.SetFrequencyRatio
    // DESCRIPTION: Sets this voice's frequency adjustment, i.e. its pitch.
    //
    // ARGUMENTS:
    //  Ratio - Frequency change, expressed as source frequency / target frequency.
    //  OperationSet - Used to identify this call as part of a deferred batch.
    //
    function SetFrequencyRatio(Ratio: Single;
                               OperationSet: UINT32 = XAUDIO2_COMMIT_NOW): HRESULT; virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.GetFrequencyRatio
    // DESCRIPTION: Returns this voice's current frequency adjustment ratio.
    //
    // ARGUMENTS:
    //  pRatio - Returns the frequency adjustment.
    //
    procedure GetFrequencyRatio(out pRatio: Single); virtual; stdcall; abstract;

    // NAME: IXAudio2SourceVoice.SetSourceSampleRate
    // DESCRIPTION: Reconfigures this voice to treat its source data as being
    //              at a different sample rate than the original one specified
    //              in CreateSourceVoice's pSourceFormat argument.
    //
    // ARGUMENTS:
    //  UINT32 - The intended sample rate of further submitted source data.
    //
    function SetSourceSampleRate(NewSourceSampleRate: UINT32): HRESULT; virtual; stdcall; abstract;

  end;



  (**************************************************************************
  *
  * IXAudio2SubmixVoice: Submixing voice management interface.
  *
  **************************************************************************)

  {$EXTERNALSYM IXAudio2SubmixVoice}
  IXAudio2SubmixVoice = class(IXAudio2Voice)
    // Methods from IXAudio2Voice base interface

    // Declare IXAudio2Voice Methods here

    // There are currently no methods specific to submix voices.

  end;



  (**************************************************************************
  *
  * IXAudio2MasteringVoice: Mastering voice management interface.
  *
  **************************************************************************)

  {$EXTERNALSYM IXAudio2MasteringVoice}
  IXAudio2MasteringVoice = class(IXAudio2Voice)
    // Methods from IXAudio2Voice base interface
    // Declare_IXAudio2Voice_Methods();

    // NAME: IXAudio2MasteringVoice.GetChannelMask
    // DESCRIPTION: Returns the channel mask for this voice
    //
    // ARGUMENTS:
    //  pChannelMask - returns the channel mask for this voice.  This corresponds
    //                 to the dwChannelMask member of WAVEFORMATEXTENSIBLE.
    //
    function GetChannelMask(Out pChannelmask: DWORD): HRESULT; virtual; stdcall; abstract;

  end;



  (**************************************************************************
  *
  * IXAudio2EngineCallback: Client notification interface for engine events.
  *
  * REMARKS: Contains methods to notify the client when certain events happen
  *          in the XAudio2 engine.  This interface should be implemented by
  *          the client.  XAudio2 will call these methods via the interface
  *          pointer provided by the client when it calls
  *          IXAudio2.RegisterForCallbacks.
  *
  **************************************************************************)

  {$EXTERNALSYM IXAudio2EngineCallback}
  IXAudio2EngineCallback = class

    // Called by XAudio2 just before an audio processing pass begins.
    procedure OnProcessingPassStart(); virtual; stdcall; abstract;

    // Called just after an audio processing pass ends.
    procedure OnProcessingPassEnd(); virtual; stdcall; abstract;

    // Called in the event of a critical system error which requires XAudio2
    // to be closed down and restarted.  The error code is given in Error.
    procedure OnCriticalError(Error: HRESULT); virtual; stdcall; abstract;

  end;



  (**************************************************************************
  *
  * IXAudio2VoiceCallback: Client notification interface for voice events.
  *
  * REMARKS: Contains methods to notify the client when certain events happen
  *          in an XAudio2 voice.  This interface should be implemented by the
  *          client.  XAudio2 will call these methods via an interface pointer
  *          provided by the client in the IXAudio2.CreateSourceVoice call.
  *
  **************************************************************************)

  {$EXTERNALSYM IXAudio2VoiceCallback}
  IXAudio2VoiceCallback = class

    // Called just before this voice's processing pass begins.
    procedure OnVoiceProcessingPassStart(BytesRequired: UINT32); virtual; stdcall; abstract;

    // Called just after this voice's processing pass ends.
    procedure OnVoiceProcessingPassEnd(); virtual; stdcall; abstract;

    // Called when this voice has just finished playing a buffer stream
    // (as marked with the XAUDIO2_END_OF_STREAM flag on the last buffer).
    procedure OnStreamEnd(); virtual; stdcall; abstract;

    // Called when this voice is about to start processing a new buffer.
    procedure OnBufferStart(pBufferContext: pointer); virtual; stdcall; abstract;

    // Called when this voice has just finished processing a buffer.
    // The buffer can now be reused or destroyed.
    procedure OnBufferEnd(pBufferContext: pointer); virtual; stdcall; abstract;

    // Called when this voice has just reached the end position of a loop.
    procedure OnLoopEnd(pBufferContext: pointer); virtual; stdcall; abstract;

    // Called in the event of a critical error during voice processing,
    // such as a failing xAPO or an error from the hardware XMA decoder.
    // The voice may have to be destroyed and re-created to recover from
    // the error.  The callback arguments report which buffer was being
    // processed when the error occurred, and its HRESULT code.
    procedure OnVoiceError(pBufferContext: Pointer; Error: HRESULT); virtual; stdcall; abstract;

  end;




  (**************************************************************************
  *
  * XAudio2Create: Top-level function that creates an XAudio2 instance.
  *
  * ARGUMENTS:
  *
  *  Flags - Flags specifying the XAudio2 object's behavior.
  *
  *  XAudio2Processor - An XAUDIO2_PROCESSOR value that specifies the
  *          hardware threads (Xbox) or processors (Windows) that XAudio2
  *          will use.  Note that XAudio2 supports concurrent processing on
  *          multiple threads, using any combination of XAUDIO2_PROCESSOR
  *          flags.  The values are platform-specific; platform-independent
  *          code can use XAUDIO2_DEFAULT_PROCESSOR to use the default on
  *          each platform.
  *
  **************************************************************************)

  function XAudio2Create(out ppXAudio2: IXAudio2;
                         Flags: UINT32 = 0;
                         XAudio2Processor: XAUDIO2_PROCESSOR = XAUDIO2_DEFAULT_PROCESSOR): HResult; stdcall;
  {$EXTERNALSYM XAudio2Create}

  //#if (NTDDI_VERSION >= NTDDI_WIN10_RS5)
  function XAudio2CreateWithVersionInfo(ppXAudio2: PIXAudio2;
                                        Flags: UINT32;
                                        XAudio2Processor: XAUDIO2_PROCESSOR;
                                        ntddiVersion: DWORD): HResult; stdcall;
  {$EXTERNALSYM XAudio2CreateWithVersionInfo}


  (**************************************************************************
  *
  * Utility functions used to convert from pitch in semitones and volume
  * in decibels to the frequency and amplitude ratio units used by XAudio2.
  * These are only defined if the client #defines XAUDIO2_HELPER_FUNCTIONS
  * prior to #including xaudio2.h.
  *
  **************************************************************************)

  function XAudio2DecibelsToAmplitudeRatio(Decibels: Single): Single; inline;
  {$EXTERNALSYM XAudio2DecibelsToAmplitudeRatio}

  function XAudio2AmplitudeRatioToDecibels(Volume: Single): Single; inline;
  {$EXTERNALSYM XAudio2AmplitudeRatioToDecibels}

  function XAudio2SemitonesToFrequencyRatio(Semitones: Single): Single; inline;
  {$EXTERNALSYM XAudio2SemitonesToFrequencyRatio}

  function XAudio2FrequencyRatioToSemitones(FrequencyRatio: Single): Single; inline;
  {$EXTERNALSYM XAudio2FrequencyRatioToSemitones}

  function XAudio2CutoffFrequencyToRadians(CutoffFrequency: Single;
                                           SampleRate: UINT32): Single; inline;
  {$EXTERNALSYM XAudio2CutoffFrequencyToRadians}

  function XAudio2RadiansToCutoffFrequency(Radians: Single;
                                           SampleRate: Single): Single; inline;
  {$EXTERNALSYM XAudio2RadiansToCutoffFrequency}

  function XAudio2CutoffFrequencyToOnePoleCoefficient(CutoffFrequency: Single;
                                                      SampleRate: UINT32): Single; inline;
  {$EXTERNALSYM XAudio2CutoffFrequencyToOnePoleCoefficient}


  // Undo the #pragma pack(push, 1) directive at the top of this file
  //#pragma pack(pop)
  //set back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


  // Additional Prototypes for ALL interfaces

const

  M_PI : Single = 3.14159265358979323846;   // From math.h
  {$EXTERNALSYM M_PI}

  // End of Additional Prototypes

implementation

uses
  {System}
  System.Math;


{$WARN SYMBOL_PLATFORM OFF}
  function XAudio2Create; external XAudio2_DLL name 'XAudio2Create' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
  //#if (NTDDI_VERSION >= NTDDI_WIN10_RS5)
  function XAudio2CreateWithVersionInfo; external XAudio2_DLL name 'XAudio2CreateWithVersionInfo' {$IF COMPILERVERSION >= 21.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}



// Calculate the argument to SetVolume from a decibel value
function XAudio2DecibelsToAmplitudeRatio(Decibels: Single): Single; inline;
begin
  Result:= power(10.0, Decibels / 20.0);
end;


// Recover a volume in decibels from an amplitude factor
function XAudio2AmplitudeRatioToDecibels(Volume: Single): Single; inline;
begin
  if (Volume = 0) then
    begin
      Result:= -3.402823466e+38; // Smallest Single value (-FLT_MAX)
    end
  else
    Result:= 20.0 * Single(log10(Volume));
end;


// Calculate the argument to SetFrequencyRatio from a semitone value
function XAudio2SemitonesToFrequencyRatio(Semitones: Single): Single; inline;
begin
  // FrequencyRatio = 2 ^ Octaves
  //                = 2 ^ (Semitones / 12)
  Result:= Power(2.0, Semitones / 12.0);
end;


// Recover a pitch in semitones from a frequency ratio
function XAudio2FrequencyRatioToSemitones(FrequencyRatio: Single): Single; inline;
begin
  // Semitones = 12 * log2(FrequencyRatio)
  //           = 12 * log2(10) * log10(FrequencyRatio)
  Result := (39.86313713864835 * log10(FrequencyRatio));
end;


// Convert from filter cutoff frequencies expressed in Hertz to the radian
// frequency values used in XAUDIO2_FILTER_PARAMETERS.Frequency, state-variable
// filter types only.  Use XAudio2CutoffFrequencyToOnePoleCoefficient() for one-pole filter types.
// Note that the highest CutoffFrequency supported is SampleRate/6.
// Higher values of CutoffFrequency will return XAUDIO2_MAX_FILTER_FREQUENCY.
function XAudio2CutoffFrequencyToRadians(CutoffFrequency: Single;
                                         SampleRate: UINT32): Single; inline;
begin
  if Round(CutoffFrequency * 6.0) >= SampleRate then
    begin
      Result:= XAUDIO2_MAX_FILTER_FREQUENCY;
    end
  else
    Result:=  2.0 * Sin(Single(M_PI) * CutoffFrequency / SampleRate);
end;


// Convert from radian frequencies back to absolute frequencies in Hertz
function XAudio2RadiansToCutoffFrequency(Radians: Single;
                                         SampleRate: Single): Single;
begin
    Result:= SampleRate * ArcSin(Radians / 2.0) / Single(M_PI);
end;


// Convert from filter cutoff frequencies expressed in Hertz to the filter
// coefficients used with XAUDIO2_FILTER_PARAMETERS.Frequency,
// LowPassOnePoleFilter and HighPassOnePoleFilter filter types only.
// Use XAudio2CutoffFrequencyToRadians() for state-variable filter types.
function XAudio2CutoffFrequencyToOnePoleCoefficient(CutoffFrequency: Single;
                                                    SampleRate: UINT32): Single; inline;
begin
  if (Round(CutoffFrequency) >= SampleRate) then
    begin
      Result:= XAUDIO2_MAX_FILTER_FREQUENCY;
    end
  else
    Result:= (1.0 - Power(1.0 - 2.0 * CutoffFrequency / SampleRate, 2.0));
end;

// end XAUDIO2_HELPER_FUNCTIONS

//Implement Additional Prototypes here.

end.
