// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation - CoreAudioApi - Windows Sonic
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.SpatialAudioClient.pas
// Kind: Pascal / Delphi unit
// Release date: 29-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: SpatialAudioClient API interface definition.
//              of the Core Audio Interfaces
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
// Source: SpatialAudioClient.h
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
unit WinApi.CoreAudioApi.SpatialAudioClient;

  {$HPPEMIT '#include "SpatialAudioClient.h"'}

interface

uses
  {WinApi}
  WinApi.PropSys,
  WinApi.WinApiTypes,
  WinApi.WinMM.MMReg,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.Audiosessiontypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type
  // Spatial Audio Object are mono PCM data plus set of attributes that
  // describes and gives information about the PCM data called metadata.
  //
  // Dynamic Spatial Audio Objects (AudioObjectType_Dynamic), are comprised of a mono buffer and metadata,
  // including position metadata, which can move over time
  //
  // Static Spatial Audio Objects also have a mono buffer and metadata, but the position is fixed.
  // Each static audio object represents an audio channel, which corresponds to a real or virtualized speaker
  // Static audio object are less resource intensive compared to dynamic audio objects
  // type from AudioObjectType_FrontLeft to AudioObjectType_BackCenter are static objects
  PAudioObjectType = ^AudioObjectType;
  AudioObjectType = (
    AudioObjectType_None             = 0,
    AudioObjectType_Dynamic          = (1 SHL 0),
    AudioObjectType_FrontLeft        = (1 SHL 1),   // SPEAKER_FRONT_LEFT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_FrontRight       = (1 SHL 2),   // SPEAKER_FRONT_RIGHT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_FrontCenter      = (1 SHL 3),   // SPEAKER_FRONT_CENTER is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_LowFrequency     = (1 SHL 4),   // SPEAKER_LOW_FREQUENCY  is the WAVEFORMATEXTENSIBLE channel mask equivalent - This audio object is not spatialized and therefore doesn't count against spatial audio object resource limits
    AudioObjectType_SideLeft         = (1 SHL 5),   // SPEAKER_SIDE_LEFT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_SideRight        = (1 SHL 6),   // SPEAKER_SIDE_RIGHT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_BackLeft         = (1 SHL 7),   // SPEAKER_BACK_LEFT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_BackRight        = (1 SHL 8),   // SPEAKER_BACK_RIGHT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_TopFrontLeft     = (1 SHL 9),   // SPEAKER_TOP_FRONT_LEFT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_TopFrontRight    = (1 SHL 10),  // SPEAKER_TOP_FRONT_RIGHT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_TopBackLeft      = (1 SHL 11),  // SPEAKER_TOP_BACK_LEFT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_TopBackRight     = (1 SHL 12),  // SPEAKER_TOP_BACK_RIGHT is the WAVEFORMATEXTENSIBLE channel mask equivalent
    AudioObjectType_BottomFrontLeft  = (1 SHL 13),
    AudioObjectType_BottomFrontRight = (1 SHL 14),
    AudioObjectType_BottomBackLeft   = (1 SHL 15),
    AudioObjectType_BottomBackRight  = (1 SHL 16),
    AudioObjectType_BackCenter       = (1 SHL 17));
  {$EXTERNALSYM AudioObjectType}


type

  // Forwarded inteface declarations

  PISpatialAudioObjectRenderStreamNotify = ^ISpatialAudioObjectRenderStreamNotify;
  ISpatialAudioObjectRenderStreamNotify = interface;


  PSpatialAudioObjectRenderStreamActivationParams = ^SpatialAudioObjectRenderStreamActivationParams;
  SpatialAudioObjectRenderStreamActivationParams = record
    ObjectFormat: WAVEFORMATEX;              // Format descriptor for a single spatial audio objects. All objects must have the same format and must be of type WAVEFORMATEX or WAVEFORMATEXTENSIBLE.
    StaticObjectTypeMask: AudioObjectType;   // (static channel bed mask) mask of static audio object type that are allowed
    MinDynamicObjectCount: UINT32;           // Minimum number of dynamic audio objects. If at least this count cannot be granted, stream activation will fail with SPTLAUDCLNT_E_NO_MORE_OBJECTS.
    MaxDynamicObjectCount: UINT32;           // Maximum number of dynamic audio objects that can be activated via ISpatialAudioObjectRenderStream
    Category: AUDIO_STREAM_CATEGORY;         // Specifies the category of the audio stream and its spatial audio objects
    EventHandle: THandle;                    // Event that will signal the need for more audio data. This handle will be duplicated internally before getting used. This handle must be unique across stream instances.
    NotifyObject: ISpatialAudioObjectRenderStreamNotify;
  end;
  {$EXTERNALSYM SpatialAudioObjectRenderStreamActivationParams}



  // Interface IAudioFormatEnumerator
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioFormatEnumerator);'}
  {$EXTERNALSYM IAudioFormatEnumerator}
  IAudioFormatEnumerator = interface(IUnknown)
  ['{DCDAA858-895A-4A22-A5EB-67BDA506096D}']

    function GetCount(out count: UINT32): HRESULT; stdcall;

    function GetFormat(index: UINT32;
                       out format: WAVEFORMATEX): HRESULT; stdcall;
    // This method returns format in order of importance, first format is the most favorable format

  end;
  IID_IAudioFormatEnumerator = IAudioFormatEnumerator;
  {$EXTERNALSYM IID_IAudioFormatEnumerator}



  // Interface ISpatialAudioObjectBase
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectBase);'}
  {$EXTERNALSYM ISpatialAudioObjectBase}
  ISpatialAudioObjectBase = interface(IUnknown)
  ['{CCE0B8F2-8D4D-4EFB-A8CF-3D6ECF1C30E0}']
    
    function GetBuffer(out buffer: PByte;
                       out bufferLength: UINT32): HRESULT; stdcall;
    //
    // Called to get buffer to pass data for the current processing pass.
    // The buffer length value returned by this method "bufferLength" is the "frameCount" value
    // retrieved by BeginUpdatingAudioObjects multiplied by WAVEFORMATEX.nBlockAlign of objectFormat
    // passed to ActivateSpatialAudioObjectRenderStream
    //
    // The first time this method is called after activation, ActivateSpatialAudioObject,
    // the audio object life starts.
    // To keep the audio object alive after that, this method must be called on every processing pass,
    // otherwise ISpatialAudioObject.SetEndOfStream() gets called implicitly on the audio object
    // and the audio object cannot be reused again without reactivation
    //
    // BeginUpdatingAudioObjects() should be called before
    // calling this method, otherwise this method will return SPTLAUDCLNT_E_OUT_OF_ORDER
    //
    // If ISpatialAudioObject.SetEndOfStream() is called explicitly or implicitly in a previous pass,
    // then the audio object is revoked and no longer usable and this method will return
    // SPTLAUDCLNT_E_RESOURCES_INVALIDATED
    // SetEndOfStream will be implicitly called if GetBuffer is not called during any processing pass
    //
    // The pointers retrieved by ISpatialAudioObject::GetBuffer should not be used after calling
    // EndUpdatingAudioObjects
    
    function SetEndOfStream(frameCount: UINT32): HRESULT; stdcall;
    // Should be called when submitting the last block of data for the audio object.
    // The frameCount value passed to this function represents the length of the last block of data in frames,
    // which could be smaller than or equal to frameCount retrieved by BeginUpdatingAudioObjects.
    // When this method is called, the spatial audio rendering engine starts flushing
    // data out of the audio object then deactivates the audio object resources so they are available for future use.
    //
    // BeginUpdatingAudioObjects() should be called before
    // calling this method, otherwise this method will return SPTLAUDCLNT_E_OUT_OF_ORDER
    //
    // If ISpatialAudioObject::SetEndOfStream() is called explicitly or implicitly in a previous pass,
    // then the audio object is revoked and no longer usable and this method will return
    // SPTLAUDCLNT_E_RESOURCES_INVALIDATED
    //
    // ISpatialAudioObject->Release() should be called after calling this method to make the audio object
    // resources available in future passes
    
    function IsActive(out isActive: BOOL): HRESULT; stdcall;
    // When isActive is false, the object cannot be used anymore and Release() should be called
    // to make this audio object resource available in the future
    // This happens after SetEndOfStream is called explicitly or implicitly on the audio object
    // SetEndOfStream will be implicitly called if GetBuffer is not called during any processing pass
    // The rendering engine will deactivate objects when system resource is not available but before that
    // happen, it will send a notification via ISpatialAudioObjectRenderStreamNotify

    function GetAudioObjectType(out audioObjectType: AudioObjectType): HRESULT; stdcall;
    // Retrieve the audio object type submitted via ActivateSpatialAudioObject

  end;
  IID_ISpatialAudioObjectBase = ISpatialAudioObjectBase;
  {$EXTERNALSYM IID_ISpatialAudioObjectBase}



  // Interface ISpatialAudioObject
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObject);'}
  {$EXTERNALSYM ISpatialAudioObject}
  ISpatialAudioObject = interface(IUnknown)
  ['{DDE28967-521B-46E5-8F00-BD6F2BC8AB1D}']


    // Right-handed Cartesian, where each unit represents 1 meter
    // x=0.0, y=0.0, z=0.0 represents the center point between the listener's ears
    //
    // x controls the horizontal movement of the audio object, right(+x) or left(-x)
    // y controls the elevation movement of the audio object, up (+y) or down (-y)
    // z controls the forward (-z) or backward (+z) movement of the audio object
    //
    // If the API client does not call this method, the last value will be used,
    // if there is no last value the default value will be used of 0.0, 0.0, 0.0 (in your head)
    //
    // BeginUpdatingAudioObjects() should be called before
    // calling this method, otherwise this method will return SPTLAUDCLNT_E_OUT_OF_ORDER
    //
    // This method returns SPTLAUDCLNT_E_PROPERTY_NOT_SUPPORTED if the audio object type
    // is not AudioObjectType_Dynamic
    //
    // If ISpatialAudioObject.SetEndOfStream() is called explicitly or implicitly in a previous pass,
    // then the audio object is revoked and no longer usable and this method will return
    // SPTLAUDCLNT_E_RESOURCES_INVALIDATED
    function SetPosition(x: FLOAT;
                         y: FLOAT;
                         z: FLOAT): HRESULT; stdcall;

    
    function SetVolume(volume: FLOAT): HRESULT; stdcall; // Volume scale, value between 0.0 and 1.0
    // Set audio-object amplitude, PCM, values multiplier which will be applied before passing data
    // to the audio rendering engine
    //
    // If the API client does not call this method, the last value will be used,
    // if there is no last value the default value will be used of 1.0
    //
    // BeginUpdatingAudioObjects() should be called before
    // calling this method, otherwise this method will return SPTLAUDCLNT_E_OUT_OF_ORDER
    //
    // If ISpatialAudioObject.SetEndOfStream() is called explicitly or implicitly in a previous pass,
    // then the audio object is revoked and no longer usable and this method will return
    // SPTLAUDCLNT_E_RESOURCES_INVALIDATED

  end;
  IID_ISpatialAudioObject = ISpatialAudioObject;
  {$EXTERNALSYM IID_ISpatialAudioObject}


  // Interface ISpatialAudioObjectRenderStreamBase
  // =============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectRenderStreamBase);'}
  {$EXTERNALSYM ISpatialAudioObjectRenderStreamBase}
  ISpatialAudioObjectRenderStreamBase = interface(IUnknown)
  ['{FEAAF403-C1D8-450D-AA05-E0CCEE7502A8}']

    function GetAvailableDynamicObjectCount(out value: UINT32): HRESULT; stdcall;
    // Get available dynamic object count for this stream

    function GetService(const riid: REFIID;
                        out service: Pointer): HRESULT; stdcall;
    // Accesses additional services from the spatial audio client
    
    function Start(): HRESULT; stdcall;
    // Streaming control method that starts the spatial audio stream.
    // Starting the stream causes data flow between the endpoint buffer and the audio engine.
    // The first time this method is called, the stream's audio clock position will be at 0.
    // Otherwise, the clock resumes from its position at the time that the stream was last paused.
    
    function Stop(): HRESULT; stdcall;
    // Streaming control method to pause the spatial audio stream.
    // Pausing the stream causes data to stop flowing between the endpoint buffer and the audio engine.
    // Pausing the stream freezes the stream's audio clock at its current stream position.
    // A subsequent call to Start() causes the stream to resume running from that position.
    // This method fails if it is called when the stream is not started.
    
    function Reset(): HRESULT; stdcall;
    // Streaming control method to reset a stopped audio stream.
    // Resetting the stream flushes all pending data and resets the audio clock stream position to 0.
    // Resetting the stream will cause all active ISpatialAudioObjectBase to be revoked.
    // A subsequent call to Start() causes the stream to start from 0 position.
    // This method fails if it is called on a stream that is not stopped.
    
    function BeginUpdatingAudioObjects(out availableDynamicObjectCount: UINT32;
                                       out frameCountPerBuffer: UINT32): HRESULT; stdcall;
    // Begin Supplying Audio-Object Data
    // frameCountPerBuffer is buffer length of in frames for the buffer returned by ISpatialAudioObject::GetBuffer
    // availableDynamicObjectCount is available dynamic object count for current processing pass
    //
    // This method must be called each time the event passed to ActivateSpatialAudioObjectRenderStream is signaled,
    // even if there no audio object data to submit
    //
    // availableDynamicObjectCount is the number of dynamic audio objects available for rendering
    // for this pass.
    //
    // All allocated static audio objects can be rendered in each processing pass
    //
    // For each BeginUpdatingAudioObjects() call, there should be a corresponding EndUpdatingAudioObjects() call.
    // If BeginUpdatingAudioObjects() is called twice without calling EndUpdatingAudioObjects(), the second call to
    // BeginUpdatingAudioObjects() will return SPTLAUDCLNT_E_OUT_OF_ORDER
    
    function EndUpdatingAudioObjects(): HRESULT; stdcall;
    // End Supplying Audio-Object Data
    // This method must be called after BeginUpdatingAudioObjects was successfully executed and the API caller
    // is done supplying audio object data
    //
    // The pointers retrieved by ISpatialAudioObject::GetBuffer cannot be used after calling this method.
    //
    // If EndUpdatingAudioObjects() is called before calling BeginUpdatingAudioObjects() first, this method will
    // return SPTLAUDCLNT_E_OUT_OF_ORDER

  end;
  IID_ISpatialAudioObjectRenderStreamBase = ISpatialAudioObjectRenderStreamBase;
  {$EXTERNALSYM IID_ISpatialAudioObjectRenderStreamBase}


  // Interface ISpatialAudioObjectRenderStream
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectRenderStream);'}
  {$EXTERNALSYM ISpatialAudioObjectRenderStream}
  ISpatialAudioObjectRenderStream = interface(ISpatialAudioObjectRenderStreamBase)
  ['{BAB5F473-B423-477B-85F5-B5A332A04153}']
    
    function ActivateSpatialAudioObject(_type: AudioObjectType ;
                                        out audioObject: ISpatialAudioObject): HRESULT; stdcall;
    // Activate an ISpatialAudioObject for rendering, counts against total resources
    // This method will return SPTLAUDCLNT_E_NO_MORE_OBJECTS if all audio objects are
    // being used
    // To avoid this error, call Release() when object life ends
    // and there is no more data to feed or after SetEndOfStream()

  end;
  IID_ISpatialAudioObjectRenderStream = ISpatialAudioObjectRenderStream;
  {$EXTERNALSYM IID_ISpatialAudioObjectRenderStream}



  // Interface ISpatialAudioObjectRenderStreamNotify
  // ===============================================
  // Notify interface to be implemented by API clients to respond to changes in
  // ISpatialAudioObjectRenderStreamBase state.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectRenderStreamNotify);'}
  {$EXTERNALSYM ISpatialAudioObjectRenderStreamNotify}
  ISpatialAudioObjectRenderStreamNotify = interface(IUnknown)
  ['{DDDF83E6-68D7-4C70-883F-A1836AFB4A50}']
    
    function OnAvailableDynamicObjectCountChange(sender: ISpatialAudioObjectRenderStreamBase;
                                                 hnsComplianceDeadlineTime: LONGLONG;         // When the spatial resource limit will be enforced in 100-nanosecond units, 0 = Now
                                                 availableDynamicObjectCountChange: UINT32): HRESULT; stdcall; // How many dynamic audio objects will be available in hnsComplianceDeadlineTime
    // Called when audio object rendering capacity is about to change for the stream
    // and let API client knows how many dynamic audio objects will be available
    // in hnsComplianceDeadlineTime
  end;
  IID_ISpatialAudioObjectRenderStreamNotify = ISpatialAudioObjectRenderStreamNotify;
  {$EXTERNALSYM IID_ISpatialAudioObjectRenderStreamNotify}



  // Interface ISpatialAudioClient
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioClient);'}
  {$EXTERNALSYM ISpatialAudioClient}
  ISpatialAudioClient = interface(IUnknown)
  ['{BBF8E066-AAAA-49BE-9A4D-FD2A858EA27F}']
    
    function GetStaticObjectPosition(_type: AudioObjectType;
                                     out x: FLOAT;
                                     out y: FLOAT;
                                     out z: FLOAT): HRESULT; stdcall;
    // Return the position of the input static object type
    // This method returns E_INVALIDARG if "type" is not a static audio object type
    
    function GetNativeStaticObjectTypeMask(out mask: AudioObjectType): HRESULT; stdcall;
    // Return the native static object mask / channel bed mask of the currently active spatial
    // rendering engine

    function GetMaxDynamicObjectCount(out value: UINT32): HRESULT; stdcall;
    // Get maximum dynamic object count for this client

    function GetSupportedAudioObjectFormatEnumerator(out enumerator: IAudioFormatEnumerator): HRESULT; stdcall;
    // List all supported object formats, the first on in the list is the most preferable format
    
    function GetMaxFrameCount(objectFormat: WAVEFORMATEX;
                              out frameCountPerBuffer: UINT32): HRESULT; stdcall;
    // Get max possible frame count per processing pass.
    // This value will change when the endpoint cadence gets changed
    // The value returned by this method can be used to allocate source buffer
    // Must specify same format which stream will be created
    
    function IsAudioObjectFormatSupported(objectFormat: WAVEFORMATEX): HRESULT; stdcall;
    // Indicates whether ISpatialAudioObjectRenderStream supports a particular format
    // If format is not supported, this method returns AUDCLNT_E_UNSUPPORTED_FORMAT
    // otherwise it will return S_OK
    
    function IsSpatialAudioStreamAvailable(const streamUuid: REFIID;
                                           auxiliaryInfo: PROPVARIANT): HRESULT; stdcall;
    // Check whether the currently active spatial rendering engine supports spatial audio
    // stream type such as ISpatialAudioObjectRenderStreamForMetadata.
    // Also check metadata format ID if supported by passing the GUID via auxiliaryInfo
    // example how to set auxiliaryInfo:
    //      TMfPPROPVARIANT auxiliaryInfo;
    //      auxiliaryInfo.vt = VT_CLSID;
    //      auxiliaryInfo.puuid = const_cast<CLSID*>(&CONTOSO_SPATIAL_METADATA_V1_0);
    //
    // If the stream cannot be activated for the currently active rendering engine, this
    // method returns SPTLAUDCLNT_E_STREAM_IS_NOT_AVAILABLE.
    // If the format is not supported, the method returns SPTLAUDCLNT_E_METADATA_FORMAT_IS_NOT_SUPPORTED
    
    //
    function ActivateSpatialAudioStream(activationParams: PROPVARIANT;  // activation parameters for the required streams for example SpatialAudioObjectRenderStreamForMetadataActivationParams
                                        const riid: REFIID;                    // spatial audio stream interface UUID for example ISpatialAudioObjectRenderStreamForMetadata
                                        out stream: Pointer): HRESULT; stdcall;
    // Activate and initialize spatial audio stream using one of the spatial audio stream activation structures.
    // for example:
    //    SpatialAudioObjectRenderStreamForMetadataActivationParams params = {};
    //    PROPVARIANT activateParams;
    //    PropVariantInit(&activateParams);
    //    activateParams.vt = VT_BLOB;
    //    activateParams.blob.cbSize = sizeof(params);
    //    activateParams.blob.pBlobData = reinterpret_cast<BYTE*>(&params);

  end;
  IID_ISpatialAudioClient = ISpatialAudioClient;
  {$EXTERNALSYM IID_ISpatialAudioClient}


// SpatialAudioClientActivationParams
  // SpatialAudioClientActivationParams is an optional activation parameter for ISpatialAudioClient
  //
  // ISpatialAudioClient implementations log various things via ETW tracing
  // including a \"context\" identifier and version information.
  //
  // The \"tracing context\" identifier helps with correlation of which audio client instance belongs to which application context
  //
  // Sample app code:
  //
  // pvar: PROPVARIANT;
  // p: SpatialAudioClientActivationParams;
  //
  // begin
  //   PropVariantInit(var);
  //   CoTaskMemAlloc(sizeof(SpatialAudioClientActivationParams));
  //
  // p.tracingContextId = /* context identifier */;
  // p.appId = /* app identifier */;
  // p.majorVersion = /* app version info */;
  // p.majorVersionN = /* app version info */;
  //
  // pvar.vt = VT_BLOB;
  // pvar.blob.cbSize = sizeof(*p);
  // pvar.blob.pBlobData = reinterpret_cast<BYTE *>(p);
  // hr:= ActivateAudioInterfaceAsync(device,
  //                                  IID_ISpatialAudioClient,
  //                                  pvar, ...);
  // ...
  // PropVariantClear(var);


  PSpatialAudioClientActivationParams = ^SpatialAudioClientActivationParams;
  SpatialAudioClientActivationParams = record
    tracingContextId: TGUID;
    appId: TGUID;
    majorVersion:  Integer;
    minorVersion1: Integer;
    minorVersion2: Integer;
    minorVersion3: Integer;
  end;
  {$EXTERNALSYM SpatialAudioClientActivationParams}

const
  // error codes

  //Since XE2 you have to hardcode this.
  //The AUDCLNT_ERR macro is defined in PfPack.AudioClient.pas

  SPTLAUDCLNT_E_DESTROYED                           =   $88890100; //   AUDCLNT_ERR($0100)
  {$EXTERNALSYM SPTLAUDCLNT_E_DESTROYED}
  SPTLAUDCLNT_E_OUT_OF_ORDER                        =   $88890101; //   AUDCLNT_ERR($0101)
  {$EXTERNALSYM SPTLAUDCLNT_E_OUT_OF_ORDER}
  SPTLAUDCLNT_E_RESOURCES_INVALIDATED               =   $88890102; //   AUDCLNT_ERR($0102)
  {$EXTERNALSYM SPTLAUDCLNT_E_RESOURCES_INVALIDATED}
  SPTLAUDCLNT_E_NO_MORE_OBJECTS                     =   $88890103; //   AUDCLNT_ERR($0103)
  {$EXTERNALSYM SPTLAUDCLNT_E_NO_MORE_OBJECTS}
  SPTLAUDCLNT_E_PROPERTY_NOT_SUPPORTED              =   $88890104; //   AUDCLNT_ERR($0104)
  {$EXTERNALSYM SPTLAUDCLNT_E_PROPERTY_NOT_SUPPORTED}
  SPTLAUDCLNT_E_ERRORS_IN_OBJECT_CALLS              =   $88890105; //   AUDCLNT_ERR($0105)
  {$EXTERNALSYM SPTLAUDCLNT_E_ERRORS_IN_OBJECT_CALLS}
  SPTLAUDCLNT_E_METADATA_FORMAT_NOT_SUPPORTED       =   $88890106; //   AUDCLNT_ERR($0106)
  {$EXTERNALSYM SPTLAUDCLNT_E_METADATA_FORMAT_NOT_SUPPORTED}
  SPTLAUDCLNT_E_STREAM_NOT_AVAILABLE                =   $88890107; //   AUDCLNT_ERR($0107)
  {$EXTERNALSYM SPTLAUDCLNT_E_STREAM_NOT_AVAILABLE}
  SPTLAUDCLNT_E_INVALID_LICENSE                     =   $88890108; //   AUDCLNT_ERR($0108)
  {$EXTERNALSYM SPTLAUDCLNT_E_INVALID_LICENSE}
  SPTLAUDCLNT_E_STREAM_NOT_STOPPED                  =   $8889010a; //   AUDCLNT_ERR($010a)
  {$EXTERNALSYM SPTLAUDCLNT_E_STREAM_NOT_STOPPED}
  SPTLAUDCLNT_E_STATIC_OBJECT_NOT_AVAILABLE         =   $8889010b; //   AUDCLNT_ERR($010b)
  {$EXTERNALSYM SPTLAUDCLNT_E_STATIC_OBJECT_NOT_AVAILABLE}
  SPTLAUDCLNT_E_OBJECT_ALREADY_ACTIVE               =   $8889010c; //   AUDCLNT_ERR($010c)
  {$EXTERNALSYM SPTLAUDCLNT_E_OBJECT_ALREADY_ACTIVE}
  SPTLAUDCLNT_E_INTERNAL                            =   $8889010d; //   AUDCLNT_ERR($010d)
  {$EXTERNALSYM SPTLAUDCLNT_E_INTERNAL}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
