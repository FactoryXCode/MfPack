// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - AudioEndPoints
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudio.AudioEngineExtensionApo
// Kind: Pascal / Delphi unit
// Release date: 29-06-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This header is used by Audio Devices DDI Reference. For more information, see:
//              Audio Devices DDI Reference (https://docs.microsoft.com/en-us/windows/win32/api/_audio/)
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//

//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
//          The interface and type definitions for base APO functionality.
//          Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: audioengineextensionapo.idl
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.CoreAudioApi.AudioEngineExtensionApo;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ServProv,
  WinApi.WinApiTypes,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX,
  WinApi.ActiveX.PropSys,
  {$ENDIF}
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioEngineBaseApo,
  WinApi.CoreAudioApi.EndPointVolume,
  WinApi.CoreAudioApi.AudioMediaType;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const
  SID_AudioProcessingObjectRTQueue         : TGUID = '{458c1a1f-6899-4c12-99ac-e2e6ac253104}';
  {$EXTERNALSYM SID_AudioProcessingObjectRTQueue}
  SID_AudioProcessingObjectLoggingService  : TGUID = '{8B8008AF-09F9-456E-A173-BDB58499BCE7}';
  {$EXTERNALSYM SID_AudioProcessingObjectLoggingService}


type

  PAUDIO_SYSTEMEFFECT_STATE = ^AUDIO_SYSTEMEFFECT_STATE;
  AUDIO_SYSTEMEFFECT_STATE       = (
    AUDIO_SYSTEMEFFECT_STATE_OFF = 0,
    AUDIO_SYSTEMEFFECT_STATE_ON
  );
  {$EXTERNALSYM AUDIO_SYSTEMEFFECT_STATE}


  PAUDIO_SYSTEMEFFECT = ^AUDIO_SYSTEMEFFECT;
  {$EXTERNALSYM AUDIO_SYSTEMEFFECT}
  AUDIO_SYSTEMEFFECT = record
    id: TGUID;
    canSetState: BOOL;
    state: AUDIO_SYSTEMEFFECT_STATE;
  end;
  TAudioSystemeffect = AUDIO_SYSTEMEFFECT;


  // Interface IAudioSystemEffects3
  // ==============================
  //
  // This is the interface by which system effects the implement and use the new settings, notification, logging and threading frameworks get identified.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioSystemEffects3);'}
  {$EXTERNALSYM IAudioSystemEffects3}
  IAudioSystemEffects3 = interface(IAudioSystemEffects2)
  ['{C58B31CD-FC6A-4255-BC1F-AD29BB0A4A17}']

    function GetControllableSystemEffectsList(out effects: PAUDIO_SYSTEMEFFECT;
                                              out numEffects: UINT;
                                              {in} event: THandle): HResult; stdcall;

    function SetAudioSystemEffectState(const effectId: TGUID;
                                       state: AUDIO_SYSTEMEFFECT_STATE): HResult; stdcall;
  end;
  IID_IAudioSystemEffects3 = IAudioSystemEffects3;
  {$EXTERNALSYM IID_IAudioSystemEffects3}


  PAPOInitSystemEffects3 = ^APOInitSystemEffects3;
  APOInitSystemEffects3 = record
    APOInit: APOInitBaseStruct;
    pAPOEndpointProperties: IPropertyStore;
    pServiceProvider: IServiceProvider;
    pDeviceCollection: IMMDeviceCollection;
    nSoftwareIoDeviceInCollection: UINT;
    nSoftwareIoConnectorIndex: UINT;
    AudioProcessingMode: TGUID;
    InitializeForDiscoveryOnly: BOOL;
  end;
  {$EXTERNALSYM APOInitSystemEffects3}


  //-----------------------------------------------------------------------------
  // Description: This structure contains expanded information pertaining to the
  // configuration of the loopback provided to the AEC.
  //
  PAcousticEchoCancellerReferenceInput = ^AcousticEchoCanceller_Reference_Input;
  AcousticEchoCanceller_Reference_Input = record
    apoInitSystemEffects: APOInitSystemEffects3;
    streamProperties: APO_REFERENCE_STREAM_PROPERTIES;
  end;
  {$EXTERNALSYM AcousticEchoCanceller_Reference_Input}


  // Interface IAudioProcessingObjectRTQueueService
  // ==============================================
  //
  // Interface used by the threading framework to get the work queue id.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectRTQueueService);'}
  {$EXTERNALSYM IAudioProcessingObjectRTQueueService}
  IAudioProcessingObjectRTQueueService = interface(IUnknown)
  ['{ACD65E2F-955B-4B57-B9BF-AC297BB752C9}']

    function GetRealTimeWorkQueue(out workQueueId: DWORD): HResult; stdcall;

  end;
  IID_IAudioProcessingObjectRTQueueService = IAudioProcessingObjectRTQueueService;
  {$EXTERNALSYM IID_IAudioProcessingObjectRTQueueService}

  //
  // Interface used by the logging framework to associate all trace logging from the same APO with the same etw activity id.
  //
  PAPO_LOG_LEVEL = ^APO_LOG_LEVEL;
  APO_LOG_LEVEL            = (
    APO_LOG_LEVEL_ALWAYS   = 0,
    APO_LOG_LEVEL_CRITICAL = 1,     // Abnormal exit or termination events.
    APO_LOG_LEVEL_ERROR    = 2,     // Severe error events.
    APO_LOG_LEVEL_WARNING  = 3,     // Warning events such as allocation failures.
    APO_LOG_LEVEL_INFO     = 4,     // Non-error events such as entry or exit events.
    APO_LOG_LEVEL_VERBOSE  = 5      // Detailed trace events.
  );
  {$EXTERNALSYM APO_LOG_LEVEL}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectLoggingService);'}
  {$EXTERNALSYM IAudioProcessingObjectLoggingService}
  IAudioProcessingObjectLoggingService = interface(IUnknown)
  ['{698f0107-1745-4708-95a5-d84478a62a65}']

    // This will generate events with the format specified in the API call.
    procedure ApoLog(level: APO_LOG_LEVEL;
                     format: PWideChar);

  end;
  IID_IAudioProcessingObjectLoggingService = IAudioProcessingObjectLoggingService;
  {$EXTERNALSYM IID_IAudioProcessingObjectLoggingService}



  // An enumeration of the type of notifications that may be requested by an APO that is
  // populated by the OS and sent to APOs that have registered for these notifications using
  // IAudioProcessingObjectNotifications.GetApoNotificationRegistrationInfo
  PAPO_NOTIFICATION_TYPE = ^APO_NOTIFICATION_TYPE;
  APO_NOTIFICATION_TYPE                                  = (
    APO_NOTIFICATION_TYPE_NONE                           = 0,
    // Endpoint volume notifications for an endpoint.
    APO_NOTIFICATION_TYPE_ENDPOINT_VOLUME                = 1,
    // Property change notifications for an endpoint.
    APO_NOTIFICATION_TYPE_ENDPOINT_PROPERTY_CHANGE       = 2,
    // Audio system effects property change notifications for an endpoint.
    APO_NOTIFICATION_TYPE_SYSTEM_EFFECTS_PROPERTY_CHANGE = 3,
    // Endpoint volume notifications for an endpoint that includes master and channel volume in dB.
    APO_NOTIFICATION_TYPE_ENDPOINT_VOLUME2               = 4,
    // Orientation notifications for the device.
    APO_NOTIFICATION_TYPE_DEVICE_ORIENTATION             = 5,
    // Microphone boost notifications
    APO_NOTIFICATION_TYPE_MICROPHONE_BOOST               = 6,
    // Audio environement state changed
    APO_NOTIFICATION_TYPE_AUDIO_ENVIRONMENT_STATE_CHANGE = 7
  );
  {$EXTERNALSYM APO_NOTIFICATION_TYPE}


  // When an endpoint volume changes, the OS will send an object with the following structure to the APOs that are interested in volume change notifications.
  PAUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION = ^AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION;
  AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION = record
    // Device that the volume has changed on.
    endpoint: IMMDevice;
    // New volume as a pointer to AUDIO_VOLUME_NOTIFICATION_DATA structure.
    volume: PAUDIO_VOLUME_NOTIFICATION_DATA;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION}


  // When an endpoint property is changed, the OS will send an object with the following structure to
  // the APOs that are interested in endpoint property change notifications.
  PAUDIO_ENDPOINT_PROPERTY_CHANGE_NOTIFICATION = ^AUDIO_ENDPOINT_PROPERTY_CHANGE_NOTIFICATION;
  AUDIO_ENDPOINT_PROPERTY_CHANGE_NOTIFICATION = record
    // Device that the property has changed on.
    endpoint: IMMDevice;
    // The property store that the change occurred on. Use this to query the new value of propertyKey below.
    propertyStore: IPropertyStore;
    // The PROPERTYKEY that has a new value.
    propertyKey: PROPERTYKEY;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_PROPERTY_CHANGE_NOTIFICATION}


  // When an audio system effects property is changed, the OS will send an object with the
  // following structure to the APOs that are interested in audio system effects property change notifications.
  PAUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION = ^AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION;
  AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION = record
    // Device that the volume has changed on.
    endpoint: IMMDevice;
    // The property store context on this endpoint.
    propertyStoreContext: TGUID;
    // The property store type used to indicate if the default/user or volatile property store changed.
    propertyStoreType: AUDIO_SYSTEMEFFECTS_PROPERTYSTORE_TYPE;
    // The property store that the change occurred on. Use this to query the new value of propertyKey below.
    propertyStore: IPropertyStore;
    // The PROPERTYKEY that has a new value.
    propertyKey: PROPERTYKEY;
  end;
  {$EXTERNALSYM AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION}


  PAUDIO_VOLUME_NOTIFICATION_DATA2 = ^AUDIO_VOLUME_NOTIFICATION_DATA2;
  {$EXTERNALSYM PAUDIO_VOLUME_NOTIFICATION_DATA2}
  AUDIO_VOLUME_NOTIFICATION_DATA2 = record
    notificationData: AUDIO_VOLUME_NOTIFICATION_DATA;
    // Specifies the current master volume level of the audio stream in dB.
    masterVolumeInDb: Single;
    // The minimum volume level of the endpoint in decibels. This value remains constant for the lifetime of
    // audio device specified in AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR.
    volumeMinInDb: Single;
    // The maximum volume level of the endpoint in decibels. This value remains constant for the lifetime of
    // the audio device specified in AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR.
    volumeMaxInDb: Single;
    // The volume increment in decibels. This increment remains constant for the lifetime
    // the audio device specified in AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR.
    volumeIncrementInDb: Single;
    // Current step in the volume range. Is a value in the range from 0 to stepCount-1, where 0 represents
    // the minimum volume level and stepCountâ€“1 represents the maximum level. Audio applications can call the
    // IAudioEndpointVolume::VolumeStepUp and IAudioEndpointVolume::VolumeStepDown methods to increase or
    // decrease the volume level by one interval.
    step: UINT;
    // The number of steps in the volume range. This number remains constant for the lifetime of
    // the audio device specified in AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR.
    stepCount: UINT;
    // The first element in an array of channel volumes in dB. This element contains the current volume level
    // of channel 0 in the audio stream. If the audio stream contains more than one channel, the volume
    // levels for the additional channels immediately follow the AUDIO_VOLUME_NOTIFICATION_DATA2 structure.
    channelVolumesInDb: array[0..0] of Single;
  end;
  {$EXTERNALSYM AUDIO_VOLUME_NOTIFICATION_DATA2}


  PAUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION2 = ^AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION2;
  AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION2 = record
    endpoint: IMMDevice;
    volume: AUDIO_VOLUME_NOTIFICATION_DATA2;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION2}


  PDEVICE_ORIENTATION_TYPE = ^DEVICE_ORIENTATION_TYPE;
  DEVICE_ORIENTATION_TYPE = (
    DEVICE_NOT_ROTATED,
    DEVICE_ROTATED_90_DEGREES_CLOCKWISE,
    DEVICE_ROTATED_180_DEGREES_CLOCKWISE,
    DEVICE_ROTATED_270_DEGREES_CLOCKWISE
  );
  {$EXTERNALSYM DEVICE_ORIENTATION_TYPE}


  PAUDIO_MICROPHONE_BOOST_NOTIFICATION = ^AUDIO_MICROPHONE_BOOST_NOTIFICATION;
  AUDIO_MICROPHONE_BOOST_NOTIFICATION = record
    // Device associated with mic boost notification.
    endpoint: IMMDevice;
    // Context associated with the originator of the event. A client can use this method to keep track of
    // control changes made by other processes and by the hardware. The functions IAudioVolumeLevel::SetLevel
    // and IAudioMute::SetMute use the context. When this notification is recieved, a client can inspect the
    // context GUID to discover whether it or another client is the source of the notification.
    eventContext: TGUID;
    // Indicates the presence of a "Microphone Boost" part (connector or subunit) of an audio capture device
    // topology.
    microphoneBoostEnabled: BOOL;
    // The volume level in decibels.
    levelInDb: Single;
    // The minimum volume level in decibels.
    levelMinInDb: Single;
    // The maximum volume level in decibels.
    levelMaxInDb: Single;
    // The stepping value between consecutive volume levels in the range levelMinInDb to levelMaxInDb
    levelStepInDb: Single;
    // Indicates if the IAudioMute interface is supported by the "Microphone Boost" part of the audio
    // capture device topology.
    muteSupported: BOOL;
    // The current state (enabled or disabled) of the mute control
    mute: BOOL;
  end;
  {$EXTERNALSYM AUDIO_MICROPHONE_BOOST_NOTIFICATION}

  // PKEY_AudioEnvironment_xxx
  // GUID for PKEY_AudioEnvironment_XXX (public): 4AFB7B88-A653-44A5-99DB-687FD74AF0BB

  // PKEY_AudioEnvironment_SpatialStreams_Active: Boolean that when TRUE indicates that spatial audio is in use, FALSE otherwise.
  // vartype = VT_BOOL
const
  PKEY_AudioEnvironment_SpatialAudioActive  :  PROPERTYKEY = (fmtid: (D1: $4AFB7B88;
                                                                      D2: $A653;
                                                                      D3: $44A5;
                                                                      D4: ($99, $DB, $68, $7F, $D7, $4A, $F0, $BB));
                                                                      Pid: 2);

type
  PAUDIO_ENVIRONMENT_STATE_CHANGE_NOTIFICATION = ^AUDIO_ENVIRONMENT_STATE_CHANGE_NOTIFICATION;
  AUDIO_ENVIRONMENT_STATE_CHANGE_NOTIFICATION = record
    // The property store that the change occurred on. Use this to query the new value of propertyKey below.
    propertyStore: IPropertyStore;
    // The PROPERTYKEY that has a new value.
    propertyKey: PROPERTYKEY;
  end;
  {$EXTERNALSYM AUDIO_ENVIRONMENT_STATE_CHANGE_NOTIFICATION}


  // This structure is used to describe the type of notification that is sent from the
  // OS to the APO using IAudioProcessingObjectNotifications.HandleNotification.
  PAPO_NOTIFICATION = ^APO_NOTIFICATION;
  APO_NOTIFICATION = record

    _type: APO_NOTIFICATION_TYPE;

    DUMMYUNIONNAME: record

    case Integer of

         // Used when type is APO_NOTIFICATION_TYPE_ENDPOINT_VOLUME.
      0: (audioEndpointVolumeChange: PAUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION);

         // Used when type is APO_NOTIFICATION_TYPE_ENDPOINT_PROPERTY_CHANGE.
      1: (audioEndpointPropertyChange: PAUDIO_ENDPOINT_PROPERTY_CHANGE_NOTIFICATION);

         // Used when type is AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION.
      2: (audioSystemEffectsPropertyChange: PAUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_NOTIFICATION);

         // Used when type is APO_NOTIFICATION_TYPE_ENDPOINT_VOLUME2.
      4: (audioEndpointVolumeChange2: PAUDIO_ENDPOINT_VOLUME_CHANGE_NOTIFICATION2);

         // Used when type is APO_NOTIFICATION_TYPE_DEVICE_ORIENTATION.
      5: (deviceOrientation: PDEVICE_ORIENTATION_TYPE;);

         // Used when type is APO_NOTIFICATION_TYPE_MICROPHONE_BOOST.
      6: (audioMicrophoneBoostChange: PAUDIO_MICROPHONE_BOOST_NOTIFICATION);

         // Used when type is APO_NOTIFICATION_TYPE_AUDIO_ENVIRONMENT_STATE_CHANGE.
      7: (audioEnvironmentChange: PAUDIO_ENVIRONMENT_STATE_CHANGE_NOTIFICATION);

    end;

    procedure Clear();
  end;
  {$EXTERNALSYM APO_NOTIFICATION}



  // Used to request endpoint volume change notifications on a specific endpoint.
  PAUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR = ^AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR;
  AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR = record
    device: IMMDevice;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR}


  // Used to request endpoint property change notifications on a specific endpoint.
  PAUDIO_ENDPOINT_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR = ^AUDIO_ENDPOINT_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR;
  AUDIO_ENDPOINT_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR = record
    device: IMMDevice;
  end;
  {$EXTERNALSYM AUDIO_ENDPOINT_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR}


  // Used to request audio system effects property change notifications on a specific endpoint and property context.
  PAUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR = ^AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR;
  AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR = record
    device: IMMDevice;
    propertyStoreContext: TGUID;
  end;
  {$EXTERNALSYM AUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR}


  // Used to request microphone boost notifications.
  PAUDIO_MICROPHONE_BOOST_APO_NOTIFICATION_DESCRIPTOR = ^AUDIO_MICROPHONE_BOOST_APO_NOTIFICATION_DESCRIPTOR;
  AUDIO_MICROPHONE_BOOST_APO_NOTIFICATION_DESCRIPTOR = record
    device: IMMDevice;
  end;
  {$EXTERNALSYM AUDIO_MICROPHONE_BOOST_APO_NOTIFICATION_DESCRIPTOR}


  //
  // This is the structure provided by the APO to indicate the type of notification it is interested in.
  //
  PAPO_NOTIFICATION_DESCRIPTOR = ^APO_NOTIFICATION_DESCRIPTOR;
  APO_NOTIFICATION_DESCRIPTOR = record

    _type: APO_NOTIFICATION_TYPE;

    DUMMYUNIONNAME: record
    case integer of

       // Used for volume notifications on a specific endpoint.
    0: (audioEndpointVolume: PAUDIO_ENDPOINT_VOLUME_APO_NOTIFICATION_DESCRIPTOR);

       // Used for property change notifications on a specific endpoint.
    1: (audioEndpointPropertyChange: PAUDIO_ENDPOINT_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR);

       // Used for audio system effects property store notifications on a specific endpoint, property context pair.
    2: (audioSystemEffectsPropertyChange: PAUDIO_SYSTEMEFFECTS_PROPERTY_CHANGE_APO_NOTIFICATION_DESCRIPTOR);

       // Used for microphone boost notifications.
    3: (audioMicrophoneBoost: PAUDIO_MICROPHONE_BOOST_APO_NOTIFICATION_DESCRIPTOR;);
    end;
    procedure Clear();
  end;
  {$EXTERNALSYM APO_NOTIFICATION_DESCRIPTOR}


  // Interface IAudioProcessingObjectPreferredFormatSupport
  // ======================================================
  //
  // Interface to be implemented by an APO that wishes to report its preferred audio input format
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectPreferredFormatSupport);'}
  IAudioProcessingObjectPreferredFormatSupport = interface(IUnknown)
    ['{51CBD3C4-F1F3-4D2F-A0E1-7E9C4DD0FEB3}']

    function GetPreferredInputFormat(outputFormat: IAudioMediaType;
                                     out preferredFormat: IAudioMediaType): HResult; stdcall;


    function GetPreferredOutputFormat(inputFormat: IAudioMediaType;
                                      out preferredFormat: IAudioMediaType): HResult; stdcall;

  end;
  {$EXTERNALSYM IAudioProcessingObjectPreferredFormatSupport}
  IID_IAudioProcessingObjectPreferredFormatSupport = IAudioProcessingObjectPreferredFormatSupport;
  {$EXTERNALSYM IID_IAudioProcessingObjectPreferredFormatSupport}


  // Interface IAudioProcessingObjectNotifications
  // =============================================
  //
  // Interface to be implemented by an APO that wishes to use the notification service provided by the OS.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectNotifications);'}
  IAudioProcessingObjectNotifications = interface(IUnknown)
  ['{56B0C76F-02FD-4B21-A52E-9F8219FC86E4}']

    // The APO provides an array of APO_NOTIFICATION_DESCRIPTORs indicating the type of notifications it is interested in.
    function GetApoNotificationRegistrationInfo(out apoNotifications: PAPO_NOTIFICATION_DESCRIPTOR;
                                                out count: DWORD): HResult; stdcall;

    // This method is invoked by the OS to send notifications to the APO.
    procedure HandleNotification(apoNotification: APO_NOTIFICATION); stdcall;

  end;
  {$EXTERNALSYM IAudioProcessingObjectNotifications}
  IID_IAudioProcessingObjectNotifications = IAudioProcessingObjectNotifications;
  {$EXTERNALSYM IID_IAudioProcessingObjectNotifications}


  // Interface IAudioProcessingObjectNotifications2
  // ==============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioProcessingObjectNotifications2);'}
  IAudioProcessingObjectNotifications2 = interface(IAudioProcessingObjectNotifications)
  ['{ca2cfbde-a9d6-4eb0-bc95-c4d026b380f0}']

    function GetApoNotificationRegistrationInfo2(maxApoNotificationTypeSupported: APO_NOTIFICATION_TYPE;
                                                 out apoNotifications: PAPO_NOTIFICATION_DESCRIPTOR;
                                                 out count: DWORD): HResult; stdcall;

  end;
  {$EXTERNALSYM IAudioProcessingObjectNotifications2}
  IID_IAudioProcessingObjectNotifications2 = IAudioProcessingObjectNotifications2;
  {$EXTERNALSYM IID_IAudioProcessingObjectNotifications2}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

procedure APO_NOTIFICATION.Clear();
begin
  DUMMYUNIONNAME.audioEndpointVolumeChange := nil;
  DUMMYUNIONNAME.audioEndpointPropertyChange := nil;
  DUMMYUNIONNAME.audioSystemEffectsPropertyChange := nil;
  DUMMYUNIONNAME.audioEndpointVolumeChange2 := nil;
  DUMMYUNIONNAME.audioMicrophoneBoostChange := nil;
end;


procedure APO_NOTIFICATION_DESCRIPTOR.Clear();
begin
  DUMMYUNIONNAME.audioEndpointVolume := nil;
  DUMMYUNIONNAME.audioEndpointPropertyChange := nil;
  DUMMYUNIONNAME.audioSystemEffectsPropertyChange := nil;
  DUMMYUNIONNAME.audioMicrophoneBoost := nil;
end;

end.
