// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.AudioEngineBaseApo.pas
// Kind: Pascal / Delphi unit
// Release date: 04-09-2015
// Language: ENU
//
// Revision Version: 3.0.0
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
// 
//          When reading the original headers (.h) you may see "STDAPI", a macro.
//          "STDAPI" means it uses the "stdcall" calling convention and it returns always a
//          HRESULT,
//          unless it's marked with, for example _BOOL, a boolean is returned.
//          In Delphi it's declared as:
//          [uses Windows;]
//          [function FunctionName(vars: -const, out or var-): HResult; stdcall;]
// 
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
//          Using packed records is not a recommended practice,
//          because it can prevent compatibility with other languages or
//          platforms, it slows data access, and, in the case of a character array,
//          it affects type compatibility.
//          For more information, see Memory management and Implicit Packing of
//          Fields with a Common Type Specification.
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
// Source: audioenginebaseapo.h
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
unit WinApi.CoreAudioApi.AudioengineBaseApo;

  {$HPPEMIT '#include "audioenginebaseapo.h"'}
  {$HPPEMIT '#include "mmdeviceapi.h"'}
  {$HPPEMIT '#include "audiomediatype.h"'}
  {$HPPEMIT '#include "audioapotypes.h"'}
  {$HPPEMIT '#include "propsys.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.AudioAPOTypes,
  WinApi.AudioMediaType,
  {WinApi.ActiveX}
  WinApi.ActiveX.PropSys,
  {WinApi.CoreAudioApi}
  WinApi.CoreAudioApi.MmDeviceApi;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  // Error Codes for APO
  // The facility code for APOs is $887D.

  APOERR_ALREADY_INITIALIZED          = $887D0001; // The object has already been initialized.
  {$EXTERNALSYM APOERR_ALREADY_INITIALIZED}
  APOERR_NOT_INITIALIZED              = $887D0002; // Object/structure is not initialized.
  {$EXTERNALSYM APOERR_NOT_INITIALIZED}
  APOERR_FORMAT_NOT_SUPPORTED         = $887D0003; // a pin supporting the format cannot be found.
  {$EXTERNALSYM APOERR_FORMAT_NOT_SUPPORTED}
  APOERR_INVALID_APO_CLSID            = $887D0004; // Invalid CLSID in an APO Initialization structure.
  {$EXTERNALSYM APOERR_INVALID_APO_CLSID}
  APOERR_BUFFERS_OVERLAP              = $887D0005; // Buffers overlap on an APO that does not accept in-place buffers.
  {$EXTERNALSYM APOERR_BUFFERS_OVERLAP}
  APOERR_ALREADY_UNLOCKED             = $887D0006; // APO is already in an unlocked state.
  {$EXTERNALSYM APOERR_ALREADY_UNLOCKED}
  APOERR_NUM_CONNECTIONS_INVALID      = $887D0007; // number of input or output connections not valid on this APO
  {$EXTERNALSYM APOERR_NUM_CONNECTIONS_INVALID}
  APOERR_INVALID_OUTPUT_MAXFRAMECOUNT = $887D0008; // Output maxFrameCount not large enough.
  {$EXTERNALSYM APOERR_INVALID_OUTPUT_MAXFRAMECOUNT}
  APOERR_INVALID_CONNECTION_FORMAT    = $887D0009; // Invalid connection format for this operation
  {$EXTERNALSYM APOERR_INVALID_CONNECTION_FORMAT}
  APOERR_APO_LOCKED                   = $887D000A; // APO is locked ready to process and can not be changed
  {$EXTERNALSYM APOERR_APO_LOCKED}
  APOERR_INVALID_COEFFCOUNT           = $887D000B; // Invalid coefficient count
  {$EXTERNALSYM APOERR_INVALID_COEFFCOUNT}
  APOERR_INVALID_COEFFICIENT          = $887D000C; // Invalid coefficient
  {$EXTERNALSYM APOERR_INVALID_COEFFICIENT}
  APOERR_INVALID_CURVE_PARAM          = $887D000D; // an invalid curve parameter was specified
  {$EXTERNALSYM APOERR_INVALID_CURVE_PARAM}


  // Signatures for data structures.
  APO_CONNECTION_DESCRIPTOR_SIGNATURE = ' ACDS ';
  {$EXTERNALSYM APO_CONNECTION_DESCRIPTOR_SIGNATURE}
  APO_CONNECTION_PROPERTY_SIGNATURE   = ' ACPS ';
  {$EXTERNALSYM APO_CONNECTION_PROPERTY_SIGNATURE}

  // Min and max framerates for the engine")
  AUDIO_MIN_FRAMERATE                 = 10.0;  // Minimum frame rate for APOs.
  {$EXTERNALSYM AUDIO_MIN_FRAMERATE}
  AUDIO_MAX_FRAMERATE                 = 384000.0;  // Maximum frame rate for APOs.
  {$EXTERNALSYM AUDIO_MAX_FRAMERATE}

  // Min and max # of channels (samples per frame) for the APOs.
  AUDIO_MIN_CHANNELS                  = 1;  // Current minimum number of channels for APOs.
  {$EXTERNALSYM AUDIO_MIN_CHANNELS}
  AUDIO_MAX_CHANNELS                  = 4096;  // Current maximum number of channels for APOs.
  {$EXTERNALSYM AUDIO_MAX_CHANNELS}


type
  // Flags for APO connection creation.
  // These flags are internally used by APO connection objects.
  PApoConnectionBufferType = ^APO_CONNECTION_BUFFER_TYPE;
  APO_CONNECTION_BUFFER_TYPE = (
    // Connection buffer is internally allocated (Initialize)
    APO_CONNECTION_BUFFER_TYPE_ALLOCATED = 0,
    // Connection buffer is passed as parameter (InitializeWithBuffer)
    APO_CONNECTION_BUFFER_TYPE_EXTERNAL  = 1,
    // Connection buffer is extracted from another Connection Object
    // (InitializeWithConnection)
    APO_CONNECTION_BUFFER_TYPE_DEPENDANT = 2
  );
  {$EXTERNALSYM APO_CONNECTION_BUFFER_TYPE}
  ApoConnectionBufferType = APO_CONNECTION_BUFFER_TYPE;
  {$EXTERNALSYM ApoConnectionBufferType}



  // This structure is the descriptor for an APO connection.
  // It is passed into the processor's CreateConnection call.
  // See: IAudioProcessor.CreateConnection.
  APO_CONNECTION_DESCRIPTOR = record
    // Indicates how the connection buffer inside the APO Connection is allocated.
    // This field is set only by APO Connection during initialization.
    // It is a private field that should be cleared before making the CreateConnection call.
    AcbType: APO_CONNECTION_BUFFER_TYPE;
    // The buffer to be used for the APO connection.
    // If NULL, the CreatConnection call will allocate memory.
    // If not NULL, CreateConnetion will use the specified memory region as the connection buffer.
    // pBuffer must be frame-aligned or 128 bit aligned, both at the beginning of the buffer and
    // at the start of the audio buffer section.
    // It must be large enough to hold u32MaxFrameCount frames.
    // It must point to the beginning of the audio buffer area.
    // See the diagram below.
    pBuffer: UINT_PTR;
    // Maximum size, in number of frames, of the connection buffer
    // associated with the connection. Note that the actual space allocated
    // depends on the exact format of the audio data specified in Format.
    u32MaxFrameCount: UINT32;
    // The format of the connection.
    // This also represents the format of the data in the connection buffer.
    pFormat: IAudioMediaType;
    // A tag identifying a valid APO_CONNECTION_DESCRIPTOR structure.
    u32Signature: UINT32;
  end;
  {$EXTERNALSYM APO_CONNECTION_DESCRIPTOR}
  PApoConnectionDescriptor = ^APO_CONNECTION_DESCRIPTOR;
  ApoConnectionDescriptor = APO_CONNECTION_DESCRIPTOR;
  {$EXTERNALSYM ApoConnectionDescriptor}

type
  // These flags specify the basic properties of Audio Processing objects.
  // If the APO is being used directly by a client without using an audio processor,
  // the client needs to ensure these flags are respected.
  PApoFlag = ^APO_FLAG;
  APO_FLAG = DWord;
  {$EXTERNALSYM APO_FLAG}
  ApoFlag = APO_FLAG;
  {$EXTERNALSYM ApoFlag}
const
  // No flags set
  APO_FLAG_NONE                       = APO_FLAG($00000000);
  {$EXTERNALSYM APO_FLAG_NONE}
  // APO can perform in-place processing. This allows the processor to
  // connect a common buffer on input and output.
  APO_FLAG_INPLACE                    = APO_FLAG($00000001);
  {$EXTERNALSYM APO_FLAG_INPLACE}
  // Samples Per Frame of input and output connections must match.
  APO_FLAG_SAMPLESPERFRAME_MUST_MATCH = APO_FLAG($00000002);
  {$EXTERNALSYM APO_FLAG_SAMPLESPERFRAME_MUST_MATCH}
  // Frames per second of input and output connections must match
  APO_FLAG_FRAMESPERSECOND_MUST_MATCH = APO_FLAG($00000004);
  {$EXTERNALSYM APO_FLAG_FRAMESPERSECOND_MUST_MATCH}
  // Bits per sample AND bytes per sample containter of input and output
  // connections must match.
  APO_FLAG_BITSPERSAMPLE_MUST_MATCH   = APO_FLAG($00000008);
  {$EXTERNALSYM APO_FLAG_BITSPERSAMPLE_MUST_MATCH}
  // APO is a mix APO. This flag should be set only for the mixer APO.
  APO_FLAG_MIXER                      = APO_FLAG($00000010);
  {$EXTERNALSYM APO_FLAG_MIXER}
  // standard flags for default APOs
  APO_FLAG_DEFAULT                    = APO_FLAG_SAMPLESPERFRAME_MUST_MATCH or
                                        APO_FLAG_FRAMESPERSECOND_MUST_MATCH or
                                        APO_FLAG_BITSPERSAMPLE_MUST_MATCH;  // In other words: APO_FLAG_DEFAULT = APO_FLAG_SAMPLESPERFRAME_MUST_MATCH
  {$EXTERNALSYM APO_FLAG_DEFAULT}


type

  // Registration properties for an APO.
  // This structure is used by the registration API functions and by IAudioProcessingObject.GetRegistrationProperties().
  PApoRegProperties = ^ApoRegProperties;
  APO_REG_PROPERTIES = record
    // The CLSID that uniquely identifies this COM object.
    clsid: CLSID;
    // The basic properties of the APO.
    Flags: APO_FLAG;
    // String that identifies the friendly name of this APO.
    szFriendlyName: array[0..255] of WideChar;
    // String that lists any relevant copyright information.
    szCopyrightInfo: array[0..255] of WideChar;
    // The major version number of this APO.
    u32MajorVersion: UINT32;
    // The minor version number of this APO.
    u32MinorVersion: UINT32;
    // The minimum number of input connections this APO must have to operate properly.
    u32MinInputConnections: UINT32;
    // The maximum number of input connections this APO can handle.
    u32MaxInputConnections: UINT32;
    // The minimum number of output connections this APO must have to operate properly.
    u32MinOutputConnections: UINT32;
    // The maximum number of output connections this APO can handle.
    u32MaxOutputConnections: UINT32;
    // The maximum number of times this APO can be instantiated system-wide. This is
    // primarily used to indicate a max instance count for a hardware based APO with
    // limited hardware resources. For software APOs with no max instances, set to
    // MAXUINT32.
    u32MaxInstances: UINT32;
    // Number of GUIDs in the iidAPOInterfaceList.
    u32NumAPOInterfaces: UINT32;
    // This is an array of GUIDs that define all the APO interfaces that can be found in this APO.
    // There are u32NumAPOInterfaces in the list.
    iidAPOInterfaceList: array of IID;    //<< Open dynamic array
  end;
  {$EXTERNALSYM APO_REG_PROPERTIES}
  ApoRegProperties = APO_REG_PROPERTIES;


  // This the base initialization header that must preceed other
  // initialization data in IAudioProcessingObject.Initialize
  PAPOInitBaseStruct = ^APOInitBaseStruct;
  APOInitBaseStruct = record
    // This is the total size in bytes of the structure.
    cbSize: UINT32;
    // This is the CLSID of the APO.
    // If the CLSID does not match, this structure was not designed for this APO and this is an error.
    // This may also be used for versioning, if the CLSID of the APO changes between versions.
    // In this case a CLSID of a previous version MAY be still supported by the APO.
    clsid: CLSID;
  end;
  {$EXTERNALSYM APOInitBaseStruct}


  // Two flow types, push and pull may be supported for each SRC APO,
  // and specified in the initialization structure for each particular APO.
  // These flow types specify how the rate converter will consume and
  // generate samples based on the amount of available input, or amount of requested output.
  PAudioFlowType = ^AUDIO_FLOW_TYPE;
  AUDIO_FLOW_TYPE   = (
    AUDIO_FLOW_PULL = 0,                      // If AUDIO_FLOW_PULL is specified then the converter will
                                              // generate a fixed and equal number of output frames on
                                              // each processing pass, and consume the entire input APO Connection buffer.
                                              // The CalcMaxInputFrames API should be called initially to
                                              // determine the maximum frameCount for APOC buffer allocation.
                                              // CalcInputFrames should then be called before every call to
                                              // APOProcess to determine the number of input frames required
                                              // to fill the output APO Connection.  On subsequent calls to APOProcess,
                                              // the returned u32InputFrameCount must equal the input APO Connectionâ€™s
                                              // u32ValidFrameCount value, or the APO will fail.

    AUDIO_FLOW_PUSH = (AUDIO_FLOW_PULL + 1)   // With AUDIO_FLOW_PUSH the converter will generate as much output
                                              // possible with the available input on each processing pass, and
                                              // consume the entire input APO Connection buffer.
                                              // CalcMaxOutputFrames should be called to determine the maximum
                                              // frameCount that may be generated on a processing pass.
                                              // This call should be made after Initialize to determine the
                                              // size of APO output Connection buffer to allocate.
                                              // CalcOutputFrames may be called to query the next output
                                              // frameCount, however, this call is not required when using AUDIO_FLOW_PUSH.
  );
  {$EXTERNALSYM AUDIO_FLOW_TYPE}
  AudioFlowType = AUDIO_FLOW_TYPE;
  {$EXTERNALSYM AudioFlowType}


  // Constriction levels
  PEAudioConstriction = ^EAudioConstriction;
  EAudioConstriction = (
    // order least to most
    eAudioConstrictionOff   = 0,    // Audio is not constricted.
    eAudioConstriction48_16 = (eAudioConstrictionOff  + 1),    // Audio is down sampled to 48 kHz/16-bit.
    eAudioConstriction44_16 = (eAudioConstriction48_16  + 1),  // Audio is down sampled to 44 kHz/16-bit.
    eAudioConstriction14_14 = (eAudioConstriction44_16  + 1),  // Audio is down sampled to 14 kHz/16-bit.
    eAudioConstrictionMute  = (eAudioConstriction14_14  + 1)   // Audio is muted.
  );
  {$EXTERNALSYM EAudioConstriction}


  // This is the structure that gets passed to the system effects APO for initialization.
  PAPOInitSystemEffects = ^APOInitSystemEffects;
  APOInitSystemEffects = record
    APOInit: APOInitBaseStruct;                // currently defined in AudioEngineBaseAPO.idl
    pAPOEndpointProperties: ^IPropertyStore;
    pAPOSystemEffectsProperties: IPropertyStore;
    pReserved: Pointer;                        // TBD for Transport Endpoint interface
    pDeviceCollection: ^IMMDeviceCollection;   // A collection of Endpoint, Topology filter, Wave filter.
  end;
  {$EXTERNALSYM APOInitSystemEffects}


  // This is the structure that gets passed to a mode-aware system effects APO for initialization.
  // The APOInitSystemEffects2 structure was introduced with Windows 8.1,
  // to make it possible to provide additional initialization context to the audio processing object (APO) for initialization.
  PAPOInitSystemEffects2 = ^APOInitSystemEffects2;
  APOInitSystemEffects2 = record
    APOInit: APOInitBaseStruct;
    pAPOEndpointProperties: IPropertyStore;
    pAPOSystemEffectsProperties: IPropertyStore;
    pReserved: Pointer;
    pDeviceCollection: IMMDeviceCollection;
    nSoftwareIoDeviceInCollection: UINT;
    nSoftwareIoConnectorIndex: UINT;
    AudioProcessingMode: TGUID;
    InitializeForDiscoveryOnly: BOOL;
  end;
  {$EXTERNALSYM APOInitSystemEffects2}


  // This is the structure that is passed to the system effects ControlPanel
  // Extension PropertyPage via IShellPropSheetExt.AddPages
  PAudioFXExtensionParams = ^AudioFXExtensionParams;
  AudioFXExtensionParams = record
    AddPageParam: LPARAM;
    pwstrEndpointID: PWideChar;
    pFxProperties: IPropertyStore;
  end;
  {$EXTERNALSYM AudioFXExtensionParams}


//////////////////////////////////////////////////////////////////////////////////////////////////


  // This is the realtime-safe interface for an APO. It may be called from
  // a real-time processing thread. The implementation of these methods do not
  // and should not block, touch paged memory, or call any blocking system routines.

  // This interface is declared "local" and is not callable out of proc.
  // This interface is hidden by the Audio Processor and is not available to
  // clients when creating APOs with IAudioProcessor.CreateAPO.


const
  // _GFX_ properties

  PKEY_FX_Association:         PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 0);
  {$EXTERNALSYM PKEY_FX_Association}

  PKEY_FX_PreMixEffectClsid:   PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 1);
  {$EXTERNALSYM PKEY_FX_PreMixEffectClsid}

  PKEY_FX_PostMixEffectClsid:  PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 2);
  {$EXTERNALSYM PKEY_FX_PostMixEffectClsid}

  PKEY_FX_UserInterfaceClsid:  PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 3);
  {$EXTERNALSYM PKEY_FX_UserInterfaceClsid}

  PKEY_FX_FriendlyName:        PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 4);
  {$EXTERNALSYM PKEY_FX_FriendlyName}

  PKEY_FX_StreamEffectClsid:   PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                      D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                      pid: 5);
   {$EXTERNALSYM PKEY_FX_StreamEffectClsid}

  PKEY_FX_ModeEffectClsid:    PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                     D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                     pid: 6);
   {$EXTERNALSYM PKEY_FX_ModeEffectClsid}

  PKEY_FX_EndpointEffectClsid:PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2:$594B; D3:$4fb6;
                                                     D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                     pid: 7);
  {$EXTERNALSYM PKEY_FX_EndpointEffectClsid:PROPERTYKEY}

  PKEY_FX_KeywordDetector_StreamEffectClsid   :	PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 8);
  {$EXTERNALSYM PKEY_FX_KeywordDetector_StreamEffectClsid}

  PKEY_FX_KeywordDetector_ModeEffectClsid     :	PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 9);
  {$EXTERNALSYM PKEY_FX_KeywordDetector_ModeEffectClsid}

  PKEY_FX_KeywordDetector_EndpointEffectClsid :	PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 10);
  {$EXTERNALSYM PKEY_FX_KeywordDetector_EndpointEffectClsid}

  PKEY_FX_Offload_StreamEffectClsid           :	PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 11);
  {$EXTERNALSYM PKEY_FX_Offload_StreamEffectClsid}

  PKEY_FX_Offload_ModeEffectClsid             : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 12);
   {$EXTERNALSYM PKEY_FX_Offload_ModeEffectClsid}

  PKEY_CompositeFX_StreamEffectClsid          : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 13);
   {$EXTERNALSYM PKEY_CompositeFX_StreamEffectClsid}

  PKEY_CompositeFX_ModeEffectClsid            : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 14);
  {$EXTERNALSYM PKEY_CompositeFX_ModeEffectClsid}

  PKEY_CompositeFX_EndpointEffectClsid        : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                       D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                       pid: 15);
  {$EXTERNALSYM PKEY_CompositeFX_EndpointEffectClsid}

  PKEY_CompositeFX_KeywordDetector_StreamEffectClsid   : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                                D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                                pid: 16);
   {$EXTERNALSYM PKEY_CompositeFX_KeywordDetector_StreamEffectClsid}

  PKEY_CompositeFX_KeywordDetector_ModeEffectClsid     : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                                D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                                pid: 17);
  {$EXTERNALSYM PKEY_CompositeFX_KeywordDetector_ModeEffectClsid}

  PKEY_CompositeFX_KeywordDetector_EndpointEffectClsid : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                                D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                                pid: 18);
   {$EXTERNALSYM PKEY_CompositeFX_KeywordDetector_EndpointEffectClsid}

  PKEY_CompositeFX_Offload_StreamEffectClsid           : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                                D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                                pid: 19);
  {$EXTERNALSYM PKEY_CompositeFX_Offload_StreamEffectClsid}

  PKEY_CompositeFX_Offload_ModeEffectClsid             : PROPERTYKEY = (fmtid: (D1: $D04E05A6; D2: $594B; D3: $4fb6;
                                                                                D4: ($A8, $0D, $01, $AF, $5E, $ED, $7D, $1D));
                                                                                pid: 20);
  {$EXTERNALSYM PKEY_CompositeFX_Offload_ModeEffectClsid}



  // PKEY_SFX_ProcessingModes_Supported_For_Streaming:
  // Lists the signal processing modes supported by the stream effect APO.
  // This list only includes signal processing modes where the APO actually processes the audio signal during streaming.
  // This list does not include any signal processing modes supported by the APO for discovery purposes only.
  PKEY_SFX_ProcessingModes_Supported_For_Streaming:   PROPERTYKEY = (fmtid:  (D1: $d3993a3f; D2:$99c2; D3:$4402;
                                                                              D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                              pid: 5);
  {$EXTERNALSYM PKEY_SFX_ProcessingModes_Supported_For_Streaming}


  // Lists the signal processing modes supported by the mode effect APO.
  // This list only includes signal processing modes where the APO actually processes the audio signal during streaming.
  // This list does not include any signal processing modes supported by the APO for discovery purposes only.
  PKEY_MFX_ProcessingModes_Supported_For_Streaming:   PROPERTYKEY = (fmtid:  (D1: $d3993a3f; D2:$99c2; D3:$4402;
                                                                              D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                              pid: 6);
  {$EXTERNALSYM PKEY_MFX_ProcessingModes_Supported_For_Streaming}

  // Lists the signal processing modes supported by the endpoint effect APO.
  // This list only includes signal processing modes where the APO actually processes the audio signal during streaming.
  // This list does not include any signal processing modes supported by the APO for discovery purposes only.
  PKEY_EFX_ProcessingModes_Supported_For_Streaming:   PROPERTYKEY = (fmtid:  (D1: $d3993a3f; D2:$99c2; D3:$4402;
                                                                              D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                              pid: 7);
  {$EXTERNALSYM PKEY_EFX_ProcessingModes_Supported_For_Streaming}

  PKEY_SFX_KeywordDetector_ProcessingModes_Supported_For_Streaming :	PROPERTYKEY = (fmtid: (D1: $d3993a3f; D2: $99c2; D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                                             pid: 8);
  {$EXTERNALSYM PKEY_SFX_KeywordDetector_ProcessingModes_Supported_For_Streaming}

  PKEY_MFX_KeywordDetector_ProcessingModes_Supported_For_Streaming :	PROPERTYKEY = (fmtid: (D1: $d3993a3f; D2: $99c2; D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                                             pid: 9);
  {$EXTERNALSYM PKEY_MFX_KeywordDetector_ProcessingModes_Supported_For_Streaming}

  PKEY_EFX_KeywordDetector_ProcessingModes_Supported_For_Streaming :	PROPERTYKEY = (fmtid: (D1: $d3993a3f; D2: $99c2; D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                                             pid: 10);
  {$EXTERNALSYM PKEY_EFX_KeywordDetector_ProcessingModes_Supported_For_Streaming}

  PKEY_SFX_Offload_ProcessingModes_Supported_For_Streaming         :	PROPERTYKEY = (fmtid: (D1: $d3993a3f; D2: $99c2; D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                                             pid: 11);
  {$EXTERNALSYM PKEY_SFX_Offload_ProcessingModes_Supported_For_Streaming}

  PKEY_MFX_Offload_ProcessingModes_Supported_For_Streaming         :	PROPERTYKEY = (fmtid: (D1: $d3993a3f; D2: $99c2; D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $3, $67, $66, $4b));
                                                                                             pid: 12);
  {$EXTERNALSYM PKEY_MFX_Offload_ProcessingModes_Supported_For_Streaming}

  // PKEY_APO_SWFallback_ProcessingModes:
  // vartype = VT_VECTOR | VT_LPWSTR
  // {D3993A3F-99C2-4402-B5EC-A92A0367664B},13
  // Lists the signal processing modes supported by the host connector that are available for fallback to SW APO if sufficient HW resources
  // aren't available.
  PKEY_APO_SWFallback_ProcessingModes                              :  PROPERTYKEY = (fmtid: (D1: $d3993a3f;
                                                                                             D2: $99c2;
                                                                                             D3: $4402;
                                                                                             D4: ($b5, $ec, $a9, $2a, $03, $67, $66, $4b));
                                                                                             Pid: 13);
  {$EXTERNALSYM PKEY_APO_SWFallback_ProcessingModes}


type


  // Interface IAudioProcessingObjectRT
  // ==================================
  // Cause the APO to perform a processing pass.
  // Parameters:
  // u32NumInputConnections  - [const] Number of input connections.
  // ppInputConnections      - [const] Array of input connection property structures, one per input connection.
  // u32NumOutputConnections - [const] Number of output connections.
  // ppOutputConnections     - [var] Array of output connection property structures, one per output connection.
  //
  //
  // Remarks:
  // This method is called for the APO to process audio data. The process
  // method must conform to the Real Time processing specification.
  //
  // An APO is required to propagate the time-stamp on the input connection
  // to the output connection. It needs to subtract its own latency when it
  // sets the output connection time-stamp. For example a zero-latency APO
  // will set the same time-stamp on its output connection as that coming in on input.
  //
  // The data to process is described in the array of input connections.
  //
  // The APO may not change the ppOutputConnections array, but it can and
  // should set the properties of the output connections after processing.
  //
  // Note:
  // This method may be called from a real-time processing thread.
  // The implementation of this method does not and should not block, touch
  // paged memory, or call any blocking system routines.
  // The IAudioProcessingObjectRT interface includes the following methods:
  // IAudioProcessingObjectRT.APOProcess
  // IAudioProcessingObjectRT.CalcInputFrames
  // IAudioProcessingObjectRT.CalcOutputFrames
  //
  {$EXTERNALSYM IAudioProcessingObjectRT}
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectRT);'}
  IAudioProcessingObjectRT = interface(IUnknown)
  ['{9E1D6A6D-DDBC-4E95-A4C7-AD64BA37846C}']
    function APOProcess(u32NumInputConnections: UINT32;
                        ppInputConnections: TApoConnectionPropertyArray;
                        u32NumOutputConnections: UINT32;
                        var ppOutputConnections: TApoConnectionPropertyArray): HRESULT; stdcall; overload;

    function CalcInputFrames(u32OutputFrameCount: UINT32): UINT32; stdcall;

    function CalcOutputFrames(u32InputFrameCount: UINT32): UINT32; stdcall;

  end;
  // IAudioProcessingObjectRT
  IID_IAudioProcessingObjectRT = IAudioProcessingObjectRT;
  {$EXTERNALSYM IID_IAudioProcessingObjectRT}


  // Interface IAudioProcessingObjectVBR
  // ===================================
  // The IAudioProcessingObjectVBR interface provides two APIs for calculating the
  // maximum frameCounts needed based on a requested or provided number of output or input frames.
  // Since VBR APOs may consume or output varying numbers of frames,
  // these APIs should be used by clients to determine
  // the largest possible input or output buffer size (in frames) that will be needed during processing.

  // Description:
  // Returns the number of input frames an APO requires to generate a given
  // number of output frames.
  //
  // Parameters:
  // u32OutputFrameCount - [const] Count of desired output frames.
  // Return values:
  // The required number of input frames to generate the given number of output frames.
  //
  // Description:
  // Returns the number of output frames an APO requires to generate a given amount of input frames.
  //
  // u32InputFrameCount - [const] Count of desired input frames.
  // Return values:
  // The number of output frames that will be generated with the given number of input frames.
  //
  // Remarks:
  // This method returns the number of input frames an APO will require
  // to generate a given number of output frames.
  //
  // This routine is useful for providing a "pull" model for audio
  // processing. The main processing loop can use this call to determine
  // the number of frames it should provide the APO in order to
  // generate a specific desired number of output frames.
  //
  // Note:
  // This method may be called from a real-time processing thread.
  // The implementation of this method does not and should not block, touch
  // paged memory, or call any blocking system routines.
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectVBR);'}
  {$EXTERNALSYM IAudioProcessingObjectVBR}
  IAudioProcessingObjectVBR = interface(IUnknown)
  ['{7ba1db8f-78ad-49cd-9591-f79d80a17c81}']
    function CalcMaxInputFrames(u32MaxOutputFrameCount: UINT32;
                                out pu32InputFrameCount: UINT32): HResult; stdcall;
    // Description:
    // Calculates the maximum input frame count.
    //
    // Parameters:
    // u32MaxOutputFrameCount  - [const] maximum output frame count that will be requested
    // pu32InputFrameCount     - [out] pointer to UINT32 to receive input frame count
    //
    // Return values:
    // S_OK
    // E_POINTER
    //
    // Remarks:
    // This method returns the maximum number of input frames that may be required
    // given some number of output frames. This API should be called after APO
    // initialization to determine the size of the input APO connection buffer to be allocated.
    // The return value should be the minimum size of APO connection used on the
    // APO's input connection.
    //
    // The returned count takes into account the maximum range of frequency shift,
    // so that input connections are large enough during processing.
    // This API should not be confused with CalcInputFrames which takes into account the current
    // frame rate set on the APO which may change after initialization.
    //
    // This method may not be called from a real-time processing thread.

    function CalcMaxOutputFrames(u32MaxInputFrameCount: UINT32;
                                 out pu32OutputFrameCount: UINT32): HResult; stdcall;
    // Description:
    // Calculates the max output frameCount.
    //
    // Parameters:
    // u32MaxInputFrameCount   -  [const] maxmimum input frame count that may occur during processing
    //
    // pu32OutputFrameCount    -  [out] pointer to UINT32 to receive the
    //                                  maxOutputFrameCount which may be generated.
    //
    // Return values:
    // S_OK
    // E_POINTER
    //
    // Remarks:
    // This routine uses the input and output max and min frameRate ranges to determine
    // the maximum output frameCount. The return value should be the minimum size
    // connection used on this APOs output connection.
    //
    // The returned count takes into account the maximum range of frequency shift,
    // so that output connections are large enough during processing.
    // This API should not be confused with CalcOutputFrames which takes into account the current
    // frame rate set on the APO which may change after initialization.
    //
    // This method may not be called from a real-time processing thread.

  end;
  // IAudioProcessingObjectVBR
  IID_IAudioProcessingObjectVBR = IAudioProcessingObjectVBR;
  {$EXTERNALSYM IID_IAudioProcessingObjectVBR}

  // Interface IAudioProcessingObjectConfiguration
  // =============================================
  // This is the configuration interface for an APO.
  // The processor uses these methods to lock and unlock APOs for processing.
  // Clients of APOs will not normally need to call this interface if the APOs are used with
  // the AudioProcessor provided by the audio core team.
  //
  // This interface is hidden by the Audio Processor and is not available to
  // clients when creating APOs with IAudioProcessor.CreateAPO.
  //
  // Clients writing their own processor, or using APOs "raw" will need to
  // use this interface to enable processing with the APOs.
  //
  // This interface is declared "local" and is not callable out of proc.
  // Description:
  // Verifies that the APO is ready to process and locks its state if so.
  //
  // Parameters:
  //
  //      u32NumInputConnections  - [const] number of input connections attached to this APO.
  //      ppInputConnections      - [const] connection descriptor of each input connection attached to this APO.
  //      u32NumOutputConnections - [const] number of output connections attached to this APO.
  //      ppOutputConnections     - [const] connection descriptor of each output connection attached to this APO.
  //
  // Return values:
  //
  //      S_OK                                 Object is locked and ready to process.
  //      E_POINTER                            Invalid pointer passed to function.
  //      APOERR_INVALID_CONNECTION_FORMAT     Invalid connection format.
  //      APOERR_NUM_CONNECTIONS_INVALID       Number of input or output connections is not valid on this APO.
  //      APOERR_APO_LOCKED                    APO is already locked.
  //      Other HRESULTs                       When another component is causing a failure.
  //                                           These failures will be tracked by the the Audio Engine.
  //
  // Remarks:
  // LockForProcess performs an internal check to see if the APO is ready to
  // process, meaning that it has been sufficiently initialized to perform
  // useful processing.
  // If this method call succeeds, the object will be locked in this state.
  //
  // Once locked, certain APO configuration changes will be disallowed.
  // These include Add/Remove/Swap of Input/Output connections, as well as any specific
  // APO re-configuration that cannot be performed dynamically.
  //
  // The APO can only be unlocked by calling UnlockForProcess.
  //
  // The latency of an APO must be fixed and unchangeable once a LockForProcess call succeeds.
  // This is so that the latency of the graph cannot change while processing,
  // which allows the client to optimize and not have to query all APO latencies constantly.
  //
  // Each APO will have different initialization requirements.
  // APOs should define an Initialize routine if it needs one.
  // LockForProcess should fail if this Initialize routine hasn’t been called.
  // All non-modifiable parameters should be set in the initalize routine. All parameters that can be set
  // after LockForProcess should have their own interface methods.
  //
  // This call must be made before calling APOProcess (if the APO is being used outside the Audio Processor).
  // The Processor will call LockForProcess before the APO is added into its active APO list.
  // See IAudioProcessor.ActivateAPO.
  //
  //  Note: This method may not be called from a real-time processing thread.
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectConfiguration);'}
  {$EXTERNALSYM IAudioProcessingObjectConfiguration}
  IAudioProcessingObjectConfiguration = interface(IUnknown)
  ['{0E5ED805-ABA6-49c3-8F9A-2B8C889C4FA8}']
    function LockForProcess(const u32NumInputConnections: UINT32;
                            ppInputConnections: PApoConnectionDescriptor;
                            const u32NumOutputConnections: UINT32;
                            ppOutputConnections: PApoConnectionDescriptor): HRESULT; stdcall;
    // Description:
    // Releases the lock imposed by LockForProcess.
    //
    // Return values:
    // S_OK                      Successful completion.
    // APOERR_ALREADY_UNLOCKED   APO was not locked.
    //
    // Remarks:
    // UnlockForProcess releases the lock imposed by LockForProcess.
    // This will allow certain APO configuration changes to occur without causing any problems for the Processor.
    // These changes may include Add/Remove/Swap of Input/Output connections, as well as any specific APO re-configuration
    // that can not be performed dynamically.

    function UnlockForProcess(): HRESULT; stdcall;
    // Description:
    // Releases the lock imposed by LockForProcess.
    //
    // Return values:
    // S_OK                      Successful completion.
    // APOERR_ALREADY_UNLOCKED   APO was not locked.
    //
    // Remarks:
    // UnlockForProcess releases the lock imposed by LockForProcess. This will
    // allow certain APO configuration changes to occur without causing any
    // problems for the Processor. These changes may include Add/Remove/Swap
    // of Input/Output connections, as well as any specific APO re-configuration
    // that can not be performed dynamically.

  end;
  // IAudioProcessingObjectConfiguration
  IID_IAudioProcessingObjectConfiguration = IAudioProcessingObjectConfiguration;
  {$EXTERNALSYM IID_IAudioProcessingObjectConfiguration}


  // Interface IAudioProcessingObject
  // =================================
  // Effects, transforms, and synthesizers are enabled through the creation and
  // usage of Audio Processing Objects (APOs). APOs may be supplied by Microsoft or by third parties.
  //
  // Part of the Audio Engine is an APO SDK that documents and provides examples of creating APOs.
  //
  // APOs typically have one input and one output connection; however, any number
  // of input and output connections (including none) may be processed.
  //
  // For an APO which operates inplace, the same connection may be set for input and output.
  //
  // All APOs must be real-time compatible (RT-compatible).
  // This implies the following.
  //
  // 1) All buffers (including connection buffers) that APOs are
  //    processing must be non-paged.
  //
  // 2) All APO code and data that is used in the processing path must be non-paged.
  //
  // 3) All IXXXRT interface methods must be implemented in a non-blocking fashion.
  //    They should not block, touch paged memory or call any blocking system routines.
  //    (Note that most system routines are blocking.)
  //
  // 4) RT-compliant APOs can be used in non-RT applications.
  //
  // There are three main interfaces for the APO: IAudioProcessingObject,
  // IAudioProcessingObjectConfiguration, and IAudioProcessingObjectRT.
  // APO specific methods not covered in the interface may be handled through a
  // private or public interface that the caller can query for.
  //
  // See AudioEngineCoreAPO.idl for examples of these methods.
  //
  // APOs that require more functionality than what’s provided in the base APO
  // interface may need to create their own interface. APOs may support any number
  // of optional interfaces including IMediaParams, IPersistStream, etc.
  // See individual APOs for examples.
  //
  //
  // The IAudioProcessingObject interface exposes the non-real-time compliant
  // parts of an Audio Processing Object to the client.
  // This interface may not be called from a real time thread.
  //
  // The IAudioProcessingObject interface supports the following methods:
  // IAudioProcessingObject.Reset
  // IAudioProcessingObject.GetInputChannelCount
  // IAudioProcessingObject.GetLatency
  // IAudioProcessingObject.GetRegistrationProperties
  // IAudioProcessingObject.IsInputFormatSupported
  // IAudioProcessingObject.IsOutputFormatSupported
  // IAudioProcessingObject.Initialize
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioProcessingObject);'}
  {$EXTERNALSYM IAudioProcessingObject}
  IAudioProcessingObject = interface(IUnknown)
  ['{FD7F2B29-24D0-4b5c-B177-592C39F9CA10}']
    function Reset(): HRESULT; stdcall;
      // Description:
      // Resets the APO to its initial state.
      //
      // Return values:
      // S_OK        Successful completion.
      //
      // Remarks:
      // This method resets the APO to a known state. It does not cause
      // any changes in the connection objects attached to the input or output of the APO.
      // The APO defines the initial state.
      //
      // See the documentation for each APO for specifics as to exactly what
      // the "known" state of the APO is after this call.
      //
      // Note:
      // This method may not be called from a real-time processing thread. The
      // implementation of this method does not and should not block, touch paged
      // memory, or call any blocking system routines.

    function GetLatency(out pTime: HNSTIME): HRESULT; stdcall;
      // Description:
      // Returns the latency for this APO.
      //
      // Parameters:
      // pTime - [out] Points to a HNSTIME structure that will receive the
      //               number of 100 nanosecond units of delay that this APO introduces.
      //
      // Return values:
      // S_OK        Successful completion.
      // E_POINTER   Invalid pointer passed to function.
      //
      // Remarks:
      // Get the latency in 100 nanosecond units.  If the caller knows the
      // sampling rate, they can calculate the latency in number of frames. The
      // latency is with respect to this APO.  To get the total latency on the
      // processing stream, the caller would query every APO in the processing
      // chain for its latency and sum the results.
      //
      // Latency is the amount of time it takes a frame to traverse the
      // processing pass of this APO. That is, it describes how long an APO
      // will hold on to a frame before releasing it downstream to the remainder
      // of the audio graph. Another way of looking at it is that it is the
      // amount of time by which the timestamp of the output connection will
      // be offset relative to the timestamp of the input connection.
      //
      // For instance, a typical APO such as volume or mixing will not introduce
      // any latency. A frame going in the input connection buffer will come out at the
      // corresponding position of the output connection buffer, and the timestamp on the
      // output connection will match that of the input connection in a single APOProcess()
      // call. An APO such as a delay or frame rate converter will introduce a
      // latency into the stream. That is, the input frames will be held by the
      // APO for a time (the latency time) and only output at a later time. So a
      // frame coming in at time 10 on the input will still come out at time 10
      // on the output, but if there is a 5 time-unit latency for the APO, the
      // APOProcess() pass made with an input connection of time 10 would generate an
      // output connection of time 5. In effect, the input frames have been held or
      // delayed by 5 time units. The APO would report a latency of 5 units.
      //
      // Note: This method may not be called from a real-time processing thread.

    function GetInputChannelCount(out pu32ChannelCount: UINT32): HResult; stdcall;
    // Description:
    // Returns the input channel count (samples-per-frame) for this APO.
    //
    // Parameters:
    // pu32ChannelCount - [out] Points to a UINT32 that will receive the channel count.
    //
    // Return values:
    // S_OK                    Successful completion.
    // E_POINTER               Invalid pointer passed to function.
    // APOERR_NOT_INITIALIZED  APO is not initialized.
    //
    // Remarks:
    // The interface returns the number of channels on the input side of the APO.

    function GetRegistrationProperties(out ppRegProps: ApoRegProperties): HRESULT; stdcall;
    // Description:
    // Returns the registration properties of the APO.
    //
    // Parameters:
    // ppRegProps - [out] Pointer to an APO_REG_PROPERTIES pointer to be filled
    //                    in by the APO with its registration properties pointer.
    //
    // Return values:
    // S_OK        Successful completion.
    // E_POINTER   Invalid pointer passed to this function.
    //
    // Remarks:
    // This method returns the registration properties of the APO.  See the APO
    // Registration API section for details on this struct.
    //
    // The caller must free the memory returned by this function using CoTaskMemFree.
    //
    // Note:
    // This method may not be called from a real-time processing thread.


    function IsInputFormatSupported(pOppositeFormat: IAudioMediaType;
                                    pRequestedInputFormat: IAudioMediaType;
                                    out ppSupportedInputFormat: IAudioMediaType): HResult; stdcall;
    // Description:
    // Verifies that a specific input format is supported.
    //
    // Parameters:
    // pOppositeFormat        - [in] the output format, or NULL for any
    // pRequestedInputFormat  - [in] the input format that is to be verified
    // ppSupportedInputFormat - [out] the closest input format supported
    //
    // Return values:
    // S_OK                         Successful completion.
    //                              ppSupportedInputFormat returns pRequestedInputFormat.
    //
    // S_FALSE                      Format or input/output format pair is not supported.
    //                              ppSupportedInputFormat returns new suggested format.
    //
    // APOERR_FORMAT_NOT_SUPPORTED  Format is not supported.
    //                              ppSupportedInputFormat is left untouched.
    //
    // E_POINTER                    Invalid pointer passed to this function.
    //
    // Other HRESULTs               When another component is causing a failure.
    //                              These failures will be tracked by the the Audio Engine.
    //
    // Remarks:
    // When pOppositeFormat is not NULL, the APO will answer this query
    // for the input/output (I/O) format pair. Some APOs may give
    // different answers depending on the opposite format.  For instance, a format
    // converter may only support integer input if the output is float.
    //
    // If the APO can accept the requested format or I/O format pair it should add a
    // reference to the requested format, return this as the supported output format, and return S_OK.
    //
    // If the APO cannot accept the requested format or I/O format pair it may suggest
    // an alternate requested format.  In this case it should create and return the
    // suggested format, and return S_FALSE.
    //
    // The returned supported format should be 'closest' to
    // the requested format (where 'close' means 'same sample format / bit depth /
    // number of channels / sample rate' in roughly that order). This format may
    // only be different from the requested format if S_FALSE is returned
    //
    // When returning any failure the suggested format should be left untouched.
    //
    // This API  may be called at any time. The answers
    // given will depend on the internal state of the APO which may
    // be manipulated by external user-interfaces.  Once the APO is locked for
    // processing, however, these I/O formats cannot and will not change.
    //
    // Note:
    // This method may not be called from a real-time processing thread.

    function IsOutputFormatSupported(pOppositeFormat: IAudioMediaType;
                                     pRequestedOutputFormat: IAudioMediaType;
                                     out ppSupportedOutputFormat: IAudioMediaType): HResult; stdcall;
    // Description:
    // Verifies that a specific output format is supported.
    //
    // Parameters:
    // pOppositeFormat  -        [in] the input format, or NULL for any
    // pRequestedOutputFormat -  [in] the output format that is to be verified
    // ppSupportedOutputFormat - [out] the closest output format supported
    //
    // Return values:
    // S_OK                         Successful completion.
    //                              ppSupportedOutputFormat returns pRequestedOutputFormat.
    //
    // S_FALSE                      Format or input/output format pair is not supported.
    //                              ppSupportedOutputFormat returns new suggested format.
    //
    // APOERR_FORMAT_NOT_SUPPORTED  Format is not supported.
    //                              ppSupportedOutputFormat is left untouched.
    //
    // E_POINTER                    Invalid pointer passed to this function.
    //
    // Other HRESULTs               When another component is causing a failure.
    //                              These failures will be tracked by the the Audio Engine.
    //
    // Remarks:
    // When pOppositeFormat is not NULL, the APO will answer this query
    // for the input/output (I/O) format pair. Some APOs may give
    // different answers depending on the opposite format.  For instance, a format
    // converter may only support integer input if the output is float.
    //
    // If the APO can accept the requested format or I/O format pair it should add a
    // reference to the requested format, return this as the supported output
    // format, and return S_OK.
    //
    // If the APO cannot accept the requested format or I/O format pair it may suggest
    // an alternate requested format.  In this case it should create and return the
    // suggested format, and return S_FALSE.

    function Initialize(cbDataSize: UINT32;
                        pbyData: PByte): HResult; stdcall;
    // Parameters:
    // cbDataSize [const] This is the size, in bytes, of the initialization data.
    //
    // pbyData    [const] This is initialization data that is specific to this APO.
    //
    // Return value:
    // The Initialize method returns a value of S_OK if the call was successful.
    // Otherwise, this method returns one of the following error codes:
    // Return code                  Description
    // ---------------------------  ---------------------------------------------
    // E_POINTER                    Invalid pointer passed to the function.
    // E_INVALIDARG                 Invalid argument.
    // APOERR_ALREADY_INITIALIZED   APO already initialized.
    // Other HRESULTS               These additional error conditions are tracked by the audio engine.
    //
    // Remarks:
    // If this method is used to initialize an APO without the need to initialize any data,
    // it is acceptable to supply a NULL as the value of the pbyData parameter and a 0 (zero)
    // as the value of the cbDataSize parameter.
    // The data that is supplied is of variable length and must have the following format:
    //
    //    MyAPOInitializationData = record
    //      APOInit : TAPOInitBaseStruct;    << declared in this unit.
    //      list additional struct members here
    //      ...
    //    end;
    //


  end;
  // IAudioProcessingObject
  IID_IAudioProcessingObject = IAudioProcessingObject;
  {$EXTERNALSYM IID_IAudioProcessingObject}


  // Interface IAudioDeviceModulesClient
  // ===================================
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioDeviceModulesClient);'}
  {$EXTERNALSYM IAudioDeviceModulesClient}
  IAudioDeviceModulesClient = interface(IUnknown)
  ['{98F37DAC-D0B6-49F5-896A-AA4D169A4C48}']
    function SetAudioDeviceModulesManager(pAudioDeviceModulesManager: IUnknown): HResult; stdcall;
  end;
  // IAudioDeviceModulesClient
  IID_IAudioDeviceModulesClient = IAudioDeviceModulesClient;
  {$EXTERNALSYM IID_IAudioDeviceModulesClient}

  // Interface IAudioSystemEffects
  // =============================
  // System Effects settings/definitions
  // This is the interface by which system effects get identified.
  // The IAudioSystemEffects interface uses the basic methods that are inherited from IUnknown,
  // and must implement an Initialize method.
  // The parameters that are passed to this Initialize method must be passed directly to the
  // IAudioProcessingObject.Initialize method.
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioSystemEffects);'}
  {$EXTERNALSYM IAudioSystemEffects}
  IAudioSystemEffects = interface(IUnknown)
  ['{5FA00F27-ADD6-499a-8A9D-6B98521FA75B}']
    function Initialize(cbDataSize: UINT32;
                        pbyData: PByte): HResult; stdcall;
    //Parameters:
    //  cbDataSize [const] : This is the size, in bytes, of the initialization data.
    //  pbyData    [const] : This is initialization data that is specific to this APO.
    //
    //Return value:
    //The Initialize method returns a value of S_OK if the call was successful.
    //Otherwise, this method returns one of the following error codes:
    //
    // Return code                      Description
    // ------------------------------   -----------------------------------------------------
    // E_POINTER                        Invalid pointer passed to the function.
    // E_INVALIDARG                     Invalid argument.
    // APOERR_ALREADY_INITIALIZED       APO already initialized.
    // Other HRESULTS                   These additional error conditions are tracked by the
    //                                  audio engine.

  end;
  // IAudioSystemEffects
  IID_IAudioSystemEffects = IAudioSystemEffects;
  {$EXTERNALSYM IID_IAudioSystemEffects}


  // Interface IAudioSystemEffects2
  // ==============================
  // This is the interface by which mode-aware system effects get identified.
  // >= win 8.1
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioSystemEffects2);'}
  {$EXTERNALSYM IAudioSystemEffects2}
  IAudioSystemEffects2 = interface(IAudioSystemEffects)
    ['{BAFE99D2-7436-44CE-9E0E-4D89AFBFFF56}']
      function GetEffectsList(out ppEffectsIds: LPGUID;
                              out pcEffects: UINT;
                              Event: THandle): HResult; stdcall;
      // The GetEffectsList method is used for retrieving the list of audio processing effects that are currently active,
      // and stores an event to be signaled if the list changes.
      // Parameters:
      // ppEffectsIds [out]   Pointer to the list of GUIDs that represent audio processing effects.
      //                      The caller is responsible for freeing this memory by calling CoTaskMemFree.
      // pcEffects    [out]   A count of the audio processing effects in the list.
      // Event        [const] The HANDLE of the event that will be signaled if the list changes.
      //
      // Return value:
      // The GetEffectsList method returns S_OK, If the method call is successful.
      // If there are no effects in the list, the function still succeeds, ppEffectsIds returns a NULL pointer,
      // and pcEffects returns a count of 0.
      //
      // Remarks:
      // The APO signals the specified event when the list of audio processing effects changes from the list that
      // was returned by GetEffectsList.
      // The APO uses this event until either GetEffectsList is called again, or the APO is destroyed.
      // The passed handle can be NULL, in which case the APO stops using any previous handle and does not signal an event.
      // An APO implements this method to allow Windows to discover the current effects applied by the APO.
      // The list of effects may depend on the processing mode that the APO initialized,
      // and on any end user configuration.
      // The processing mode is indicated by the AudioProcessingMode member of APOInitSystemEffects2.
      // APOs should identify effects using GUIDs defined by Windows, such as AUDIO_EFFECT_TYPE_ACOUSTIC_ECHO_CANCELLATION.
      // An APO should only define and return a custom GUID in rare cases where the type of effect is
      // clearly different from the ones defined by Windows.

  end;
  // IAudioSystemEffects2
  IID_IAudioSystemEffects2 = IAudioSystemEffects2;
  {$EXTERNALSYM IID_IAudioSystemEffects2}


  // Interface IAudioSystemEffectsCustomFormats
  // ==========================================
  // System Effects APOs that support some form of encoding or want
  // to drive the device at an atypical format (one not normally exposed
  // in the audio control panel applet) can support this interface.
  //
  // The IAudioSystemEffectsCustomFormats interface is supported in Windows Vista and later versions of Windows.
  // When you develop an audio processing object (APO) to drive an audio adapter with an atypical format,
  // the APO must support the IAudioSystemEffectsCustomFormats interface.
  // The Windows operating system can instantiate your APO outside the audio engine and use the
  // IAudioSystemEffectsCustomFormats interface to retrieve information about the atypical format.
  // The associated user interface displays the data that is retrieved.
  // IMPORTANT:
  //  Although the IAudioSystemEffectsCustomFormats interface continues to be supported in Windows,
  //  note that the type of APO to which you can apply this interface depends on the version of Windows you are targeting.
  //  The following table provides more information:
  //
  // Target OS                    Target APO type
  // ---------------------------  --------------------------------------------
  // Windows Vista                Global effects (GFX)
  // Windows 7                    Global effects (GFX)
  // Windows 8                    Global effects (GFX)
  // Windows 8.1                  Endpoint effects (EFX)
  //
  // The IAudioSystemEffectsCustomFormats interface inherits from IUnknown and also supports the following methods:
  // IAudioSystemEffectsCustomFormats.GetFormat
  // IAudioSystemEffectsCustomFormats.GetFormatCount
  // IAudioSystemEffectsCustomFormats.GetFormatRepresentation
  //
  {$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IAudioSystemEffectsCustomFormats);'}
  {$EXTERNALSYM IAudioSystemEffectsCustomFormats}
  IAudioSystemEffectsCustomFormats = interface(IUnknown)
    ['{B1176E34-BB7F-4f05-BEBD-1B18A534E097}']
      function GetFormatCount(out pcFormats: UINT): HResult; stdcall;
      // Description:
      // Retrieves the number of custom formats supported by this APO
      //
      // Parameters:
      // pcFormats   [out]  Receives the number of formats supported
      //
      // Return values:
      // S_OK            Successful completion.
      // E_POINTER       Invalid pointer passed to function.

      function GetFormat(nFormat: UINT;
                         out ppFormat: IAudioMediaType): HResult; stdcall;
      // Description:
      // Retrieves a IAudioMediaType representation of a custom format
      //
      // Parameters:
      // nFormat    [const] the index of the format (in the range 0 to GetFormatCount() result - 1)
      // ppFormat   [out]   the address of a pointer that will receive the format
      //
      // Return values:
      // S_OK            Successful completion.
      // E_POINTER       Invalid pointer passed to function.
      // E_OUTOFMEMORY   Return buffer cannot be allocated.
      // E_INVALIDARG    nFormat is out of range.
      //
      // Remarks:
      // The caller is responsible for deleting the IAudioMediaType object pointed to by *ppFormat.

      function GetFormatRepresentation(nFormat: UINT;
                                       out ppwstrFormatRep: PWideChar): HResult; stdcall;
      // Description:
      // Retrieves a string representation of a custom format for UI purposes
      //
      // Parameters:
      // nFormat           [const] the index of the format (in the range 0 to GetFormatCount() result - 1)
      // ppwstrFormatRep   [out]   the address of a buffer pointer that will receive a null-
      //                           terminated string describing the format
      //
      // Return values:
      // S_OK            Successful completion.
      // E_POINTER       Invalid pointer passed to function.
      // E_OUTOFMEMORY   Return buffer cannot be allocated.
      // E_INVALIDARG    nFormat is out of range.
      //
      // Remarks:
      // The caller is responsible for freeing the buffer pointed to by *pwstrFormatRep
      // using CoTaskMemFree.

  end;
  // IAudioSystemEffectsCustomFormats
  IID_IAudioSystemEffectsCustomFormats = IAudioSystemEffectsCustomFormats;
  {$EXTERNALSYM IID_IAudioSystemEffectsCustomFormats}



  // APO registration functions
  // ===========================================================================

type
  FNAPONOTIFICATIONCALLBACK = function(pProperties : APO_REG_PROPERTIES;
                                       pvRefData : Pointer): HRESULT; stdcall;
  {$EXTERNALSYM FNAPONOTIFICATIONCALLBACK}


  function RegisterAPO(var pProperties: APO_REG_PROPERTIES): HResult; stdcall;
  {$EXTERNALSYM RegisterAPO}

  function UnregisterAPO(clsid: REFCLSID): HResult; stdcall;
  {$EXTERNALSYM UnregisterAPO}

  function RegisterAPONotification(hEvent: THandle): HResult; stdcall;
  {$EXTERNALSYM RegisterAPONotification}

  function UnregisterAPONotification(hEvent: THandle): HResult; stdcall;
  {$EXTERNALSYM UnregisterAPONotification}

  function EnumerateAPOs(pfnCallback: FNAPONOTIFICATIONCALLBACK;
                         pvRefData: Pointer): HResult; stdcall;
  {$EXTERNALSYM EnumerateAPOs}

  function GetAPOProperties(clsid: REFCLSID;
                            var pProperties: APO_REG_PROPERTIES): HResult; stdcall;
  {$EXTERNALSYM GetAPOProperties}


    // Additional Prototypes for ALL interfaces

    // End of Additional Prototypes

implementation

    // Implement Additional functions here.

const
  AudioEngineBaseApoLib = 'audioses.dll';  // nvapo64v.dll > NVidia driver

{$WARN SYMBOL_PLATFORM OFF}
  function RegisterAPO; external AudioEngineBaseApoLib name 'RegisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UnregisterAPO; external AudioEngineBaseApoLib name 'UnregisterAPO' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function RegisterAPONotification; external AudioEngineBaseApoLib name 'RegisterAPONotification' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function UnregisterAPONotification; external AudioEngineBaseApoLib name 'UnregisterAPONotification' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function EnumerateAPOs; external AudioEngineBaseApoLib name 'EnumerateAPOs' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function GetAPOProperties; external AudioEngineBaseApoLib name 'GetAPOProperties' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
