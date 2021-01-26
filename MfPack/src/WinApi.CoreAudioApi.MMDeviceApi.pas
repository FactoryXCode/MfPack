// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - MMDeviceApi
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.MMDeviceApi.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Mike (fc234), (Ciaran).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
// 18/01/2021 Tony                Corrected some pointer issues.
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
//          The interface and type definitions for base APO functionality.
//          Requires Windows Vista or later.
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
// Source: mmdeviceapi.h
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
unit WinApi.CoreAudioApi.MMDeviceApi;

interface

  {$HPPEMIT '#include "mmdeviceapi.h"'}

uses

  {WinApi}
  WinApi.WinError,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {CoreAudioApi}
  WinApi.CoreAudioApi.DeviceTopology;


  {$WEAKPACKAGEUNIT ON}
  {$ALIGN ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  CLSID_MMDeviceEnumerator            : TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  {$EXTERNALSYM CLSID_MMDeviceEnumerator}

  // Error types
  E_NOTFOUND                          = HRESULT(ERROR_NOT_FOUND);
  {$EXTERNALSYM E_NOTFOUND}
  E_UNSUPPORTED_TYPE                  = HRESULT(ERROR_UNSUPPORTED_TYPE);
  {$EXTERNALSYM E_UNSUPPORTED_TYPE}

  // The DEVICE_STATE_XXX constants indicate the current state of an audio endpoint device.

  DEVICE_STATE_ACTIVE                 = $00000001;  // The audio endpoint device is active.
  {$EXTERNALSYM DEVICE_STATE_ACTIVE}                // That is, the audio adapter that connects to the endpoint device is present and enabled.
                                                    // In addition, if the endpoint device plugs into a jack on the adapter,
                                                    // then the endpoint device is plugged in.

  DEVICE_STATE_DISABLED               = $00000002;  // The audio endpoint device is disabled.
  {$EXTERNALSYM DEVICE_STATE_DISABLED}              // The user has disabled the device in the Windows multimedia control panel,
                                                    // Mmsys.cpl.

  DEVICE_STATE_NOTPRESENT             = $00000004;  // The audio endpoint device is not present because the audio adapter that connects to
  {$EXTERNALSYM DEVICE_STATE_NOTPRESENT}            // the endpoint device has been removed from the system,
                                                    // or the user has disabled the adapter device in Device Manager.

  DEVICE_STATE_UNPLUGGED              = $00000008;  // The audio endpoint device is unplugged.
  {$EXTERNALSYM DEVICE_STATE_UNPLUGGED}              // The audio adapter that contains the jack for the endpoint device is present and enabled,
                                                    // but the endpoint device is not plugged into the jack.
                                                    // Only a device with jack-presence detection can be in this state.
                                                    // For more information about jack-presence detection, see Audio Endpoint Devices.

  DEVICE_STATEMASK_ALL                = $0000000F;  // Includes audio endpoint devices in all states—active, disabled, not present, and unplugged.
  {$EXTERNALSYM DEVICE_STATEMASK_ALL}


  ENDPOINT_SYSFX_ENABLED              = $00000000;  // System Effects are enabled.
  {$EXTERNALSYM ENDPOINT_SYSFX_ENABLED}

  ENDPOINT_SYSFX_DISABLED             = $00000001;  // System Effects are disabled.
  {$EXTERNALSYM ENDPOINT_SYSFX_DISABLED}


  // PKEY_AUDIOENDPOINT_XXX
  // Indicates the physical attributes of the audio endpoint device.
  PKEY_AudioEndpoint_FormFactor: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 0);
  {$EXTERNALSYM PKEY_AudioEndpoint_FormFactor}

  // Specifies the CLSID of the registered provider of the device-properties extension for the audio endpoint device.
  PKEY_AudioEndpoint_ControlPanelPageProvider: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 1);
  {$EXTERNALSYM PKEY_AudioEndpoint_ControlPanelPageProvider}

  // Associates a kernel-streaming (KS) pin category with an audio endpoint device.
  PKEY_AudioEndpoint_Association: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 2);
  {$EXTERNALSYM PKEY_AudioEndpoint_Association}

  // Defines the physical speaker configuration for the audio endpoint device.
  PKEY_AudioEndpoint_PhysicalSpeakers: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 3);
  {$EXTERNALSYM PKEY_AudioEndpoint_PhysicalSpeakers}

  // Specifies the CLSID of the registered provider of the device-properties extension for the audio endpoint device.
  PKEY_AudioEndpoint_GUID: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 4);
  {$EXTERNALSYM PKEY_AudioEndpoint_GUID}

  // FX - Indicates whether system effects are enabled in the shared-mode stream that flows to or from the audio endpoint device.
  // Note: You may also access the DirectSound FX interfaces to do this. (See also: DSFx.pas (not included with Media Foundation))
  PKEY_AudioEndpoint_Disable_SysFx: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 5);
  {$EXTERNALSYM PKEY_AudioEndpoint_Disable_SysFx}

  // Specifies the channel-configuration mask for the full-range speakers that are connected to the audio endpoint device.
  PKEY_AudioEndpoint_FullRangeSpeakers: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 6);
  {$EXTERNALSYM PKEY_AudioEndpoint_FullRangeSpeakers}

  // Indicates whether the endpoint supports the event-driven mode. The values are populated by the OEM in an .inf file.
  PKEY_AudioEndpoint_Supports_EventDriven_Mode: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 7);
  {$EXTERNALSYM PKEY_AudioEndpoint_Supports_EventDriven_Mode}

  // Contains an output category GUID for an audio endpoint device.
  PKEY_AudioEndpoint_JackSubType: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 8);
  {$EXTERNALSYM PKEY_AudioEndpoint_JackSubType}

  PKEY_AudioEndpoint_Default_VolumeInDb: PROPERTYKEY = (
                fmtid: (D1:$1da5d803; D2:$d492; D3:$4edd;
                D4: ($8c, $23, $e0, $c0, $ff, $ee, $7f, $0e));
                pid: 9);
  {$EXTERNALSYM PKEY_AudioEndpoint_Default_VolumeInDb}


// Defined in WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey if OS >= Vista
//  PKEY_Device_FriendlyName: PROPERTYKEY = (
//                fmtid: (D1:$a45c254e; D2:$df1c; D3:$4efd;
//                D4: ($80, $20, $67, $d1, $46, $a8, $50, $e0));
//                pid: 14);
//  {$EXTERNALSYM PKEY_Device_FriendlyName}


  //PKEY_AudioEndpoint_XXX
  PKEY_AudioEngine_DeviceFormat: PROPERTYKEY = (
                fmtid: (D1:$f19f064d; D2:$82c; D3:$4e27;
                D4: ($bc, $73, $68, $82, $a1, $bb, $8e, $4c));
                pid: 0);
  {$EXTERNALSYM PKEY_AudioEngine_DeviceFormat}

  PKEY_AudioEngine_OEMFormat: PROPERTYKEY = (
                fmtid: (D1:$e4870e26; D2:$3cc5; D3:$4cd2;
                D4: ($ba, $46, $ca, $a, $9a, $70, $ed, $4));
                pid: 3);
  {$EXTERNALSYM PKEY_AudioEngine_OEMFormat}

  PKEY_AudioEndpointLogo_IconEffects :  PROPERTYKEY = (
                fmtid: (D1: $f1ab780d; D2: $2010; D3: $4ed3;
                D4: ($a3, $a6, $8b, $87, $f0, $f0, $c4, $76));
                pid: 0);
  {$EXTERNALSYM PKEY_AudioEndpointLogo_IconEffects}

  PKEY_AudioEndpointLogo_IconPath :  PROPERTYKEY = (
                fmtid: (D1: $f1ab780d; D2: $2010; D3: $4ed3;
                D4: ($a3, $a6, $8b, $87, $f0, $f0, $c4, $76));
                pid: 1);
  {$EXTERNALSYM PKEY_AudioEndpointLogo_IconPath}

  PKEY_AudioEndpointSettings_MenuText :  PROPERTYKEY = (
                fmtid: (D1: $14242002; D2: $0320; D3: $4de4;
                D4: ($95, $55, $a7, $d8, $2b, $73, $c2, $86));
                pid: 0);
  {$EXTERNALSYM PKEY_AudioEndpointSettings_MenuText}

  PKEY_AudioEndpointSettings_LaunchContract :  PROPERTYKEY = (
                fmtid: (D1: $14242002; D2: $0320; D3: $4de4;
                D4: ($95, $55, $a7, $d8, $2b, $73, $c2, $86));
                pid: 1);
  {$EXTERNALSYM PKEY_AudioEndpointSettings_LaunchContract}


type
  // LPCGUID = PGUID;   // declared in WinApi.MediaFoundationApi.MfTypes, See: ComObj for TGuid specs
  HANDLE = System.THandle;

  // Note:
  // Skip the auto wrapper MIDL recordnames generated for COM enumerations (__MIDL___MIDL_itf_name_xxx_xxx)


  PDIRECTX_AUDIO_ACTIVATION_PARAMS = ^tagDIRECTX_AUDIO_ACTIVATION_PARAMS;
  tagDIRECTX_AUDIO_ACTIVATION_PARAMS = record
    cbDirectXAudioActivationParams: DWORD;
    guidAudioSession: TGUID;
    dwAudioStreamFlags: DWORD;
  end;
  {$EXTERNALSYM tagDIRECTX_AUDIO_ACTIVATION_PARAMS}
  DIRECTX_AUDIO_ACTIVATION_PARAMS = tagDIRECTX_AUDIO_ACTIVATION_PARAMS;
  {$EXTERNALSYM DIRECTX_AUDIO_ACTIVATION_PARAMS}


  PEDataFlow = ^TEDataFlow;
  __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0001 = (
    eRender              = 0,              // Audio rendering stream.
                                           // Audio data flows from the application to the audio endpoint device,
                                           // which renders the stream.

    eCapture             = (eRender + 1),  // Audio capture stream.
                                           // Audio data flows from the audio endpoint device that captures the stream,
                                           // to the application.

    eAll                 = (eCapture + 1), // Audio rendering or capture stream.
                                           // Audio data can flow either from the application to the audio endpoint device,
                                           // or from the audio endpoint device to the application.

    EDataFlow_enum_count = (eAll + 1)      // The number of members in the EDataFlow enumeration (not counting the EDataFlow_enum_count member).
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0001}
  EDataFlow = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0001;
  {$EXTERNALSYM EDataFlow}
  TEDataFlow = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0001;
  {$EXTERNALSYM TEDataFlow}
  _tagEDataFlow = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0001; // For compatibility with earlier MfPack versions
  {$NODEFINE _tagEDataFlow}
  // Remarks:
  // The IMMDeviceEnumerator.GetDefaultAudioEndpoint, IMMDeviceEnumerator.EnumAudioEndpoints, IMMEndpoint.GetDataFlow and
  // IMMNotificationClient.OnDefaultDeviceChanged methods use the constants defined in the EDataFlow enumeration.

  // ERole

  PeRole = ^TeRole;
  __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0002 = (
    eConsole         = 0,                    // Games, system notification sounds, and voice commands.
    eMultimedia      = (eConsole + 1),       // Music, movies, narration, and live music recording.
    eCommunications  = (eMultimedia + 1),    // Voice communications (talking to another person).
    eRole_enum_count = (eCommunications + 1) // The number of members in the ERole enumeration (not counting the ERole_enum_count member).
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0002}
  ERole = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0002;
  {$EXTERNALSYM ERole}
  TERole = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0002;
  {$NODEFINE _tagERole}
  _tagERole = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0002; // For compatibility with earlier MfPack versions


  // EndpointFormFactor enum
  // The EndpointFormFactor enumeration defines constants that indicate the general physical attributes of an audio endpoint device.
  PEndpointFormFactor = ^TEndpointFormFactor;
  __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0003 = (
    RemoteNetworkDevice           = 0,
    Speakers                      = (RemoteNetworkDevice + 1),
    LineLevel                     = (Speakers + 1),
    Headphones                    = (LineLevel + 1),
    Microphone                    = (Headphones + 1),
    Headset                       = (Microphone + 1),
    Handset                       = (Headset + 1),
    UnknownDigitalPassthrough     = (Handset + 1),
    SPDIF                         = (UnknownDigitalPassthrough + 1),
    DigitalAudioDisplayDevice     = (SPDIF + 1),
    UnknownFormFactor             = (DigitalAudioDisplayDevice + 1),
    EndpointFormFactor_enum_count = (UnknownFormFactor + 1),
    HDMI                          = DigitalAudioDisplayDevice
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0003}
  EndpointFormFactor = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0003;
  {$EXTERNALSYM EndpointFormFactor}
  TEndpointFormFactor = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0003;
  {$NODEFINE _tagEndpointFormFactor}
  _tagEndpointFormFactor = __MIDL___MIDL_itf_mmdeviceapi_0000_0000_0003; // For compatibility with earlier MfPack versions


const
  // ----------------------------------------------------------------------
  // Device Interface Classes
  // ----------------------------------------------------------------------
  // {E6327CAD-DCEC-4949-AE8A-991E976A79D2}
  DEVINTERFACE_AUDIO_RENDER  : TGUID = '{e6327cad-dcec-4949-ae8a-991e976a79d2}';
  {$EXTERNALSYM DEVINTERFACE_AUDIO_RENDER}
  // {2EEF81BE-33FA-4800-9670-1CD474972C3F}
  DEVINTERFACE_AUDIO_CAPTURE : TGUID = '{2eef81be-33fa-4800-9670-1cd474972c3f}';
  {$EXTERNALSYM DEVINTERFACE_AUDIO_CAPTURE}

//#if (NTDDI_VERSION > NTDDI_WINBLUE || \
//    (NTDDI_VERSION == NTDDI_WINBLUE && defined(WINBLUE_KBSPRING14)))

  // {6DC23320-AB33-4CE4-80D4-BBB3EBBF2814}
  DEVINTERFACE_MIDI_OUTPUT   : TGUID = '{6dc23320-ab33-4ce4-80d4-bbb3ebbf2814}';
  {$EXTERNALSYM DEVINTERFACE_MIDI_OUTPUT}
  // {504BE32C-CCF6-4D2C-B73F-6F8B3747E22B}
  DEVINTERFACE_MIDI_INPUT    : TGUID = '{504be32c-ccf6-4d2c-b73f-6f8b3747e22b}';
  {$EXTERNALSYM DEVINTERFACE_MIDI_INPUT}

// #endif

type

  // Forward Interface declarations

  PIActivateAudioInterfaceAsyncOperation = ^IActivateAudioInterfaceAsyncOperation;
  IActivateAudioInterfaceAsyncOperation = interface;


  // Interface IMMNotificationClient
  // ===============================
  {
   The IMMNotificationClient interface provides notifications when an audio endpoint device
   is added or removed, when the state or properties of an endpoint device change,
   or when there is a change in the default role assigned to an endpoint device.
   Unlike the other interfaces in this section, which are implemented by the MMDevice API system component,
   an MMDevice API client implements the IMMNotificationClient interface.
   To receive notifications, the client passes a pointer to its IMMNotificationClient interface instance
   as a parameter to the IMMDeviceEnumerator.RegisterEndpointNotificationCallback method.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMNotificationClient);'}
  {$EXTERNALSYM IMMNotificationClient}
  IMMNotificationClient = interface(IUnknown)
  ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']

     function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                  dwNewState: DWord): HRESULT; stdcall;
    // Parameters
    // pwstrDeviceId [in]
    //  Pointer to the endpoint ID string that identifies the audio endpoint device.
    //  This parameter points to a null-terminated, wide-character string containing the endpoint ID.
    //  The string remains valid for the duration of the call.
    // dwNewState [in]
    //  Specifies the new state of the endpoint device.
    //  The value of this parameter is one of the following DEVICE_STATE_XXX constants:
    //    DEVICE_STATE_ACTIVE
    //    DEVICE_STATE_DISABLED
    //    DEVICE_STATE_NOTPRESENT
    //    DEVICE_STATE_UNPLUGGED

    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;
    // The OnDeviceAdded method indicates that a new audio endpoint device has been added.
    // Parameters
    //   pwstrDeviceId [in]
    //   Pointer to the endpoint ID string that identifies the audio endpoint device.
    //   This parameter points to a null-terminated, wide-character string containing
    //   the endpoint ID. The string remains valid for the duration of the call.

    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;
    // Parameters
    // flow [in]
    //  The data-flow direction of the endpoint device.
    //  This parameter is set to one of the following EDataFlow enumeration values:
    //    - eRender
    //    - eCapture
    //  The data-flow direction for a rendering device is eRender.
    //  The data-flow direction for a capture device is eCapture.
    // role [in]
    //  The device role of the audio endpoint device.
    //  This parameter is set to one of the following ERole enumeration values:
    //    - eConsole
    //    - eMultimedia
    //    - eCommunications
    // pwstrDefaultDeviceId [in]
    //  Pointer to the endpoint ID string that identifies the audio endpoint device.
    //  This parameter points to a null-terminated, wide-character string containing the endpoint ID.
    //  The string remains valid for the duration of the call.
    //  If the user has removed or disabled the default device for a particular role,
    //  and no other device is available to assume that role, then pwstrDefaultDeviceId is NULL.

    function OnDefaultDeviceChanged(flow: EDataFlow;
                                    role: ERole;
                                    pwstrDefaultDeviceId: LPCWSTR): HRESULT; stdcall;
    // The OnDefaultDeviceChanged method notifies the client that the default audio endpoint device for a particular device role has changed.
    // Parameters
    //   flow [in]
    //    The data-flow direction of the endpoint device.
    //    This parameter is set to one of the following EDataFlow enumeration values:
    //     - eRender  : The data-flow direction for a rendering device is eRender.
    //     - eCapture : The data-flow direction for a capture device is eCapture.
    //   role [in]
    //    The device role of the audio endpoint device.
    //    This parameter is set to one of the following ERole enumeration values:
    //    - eConsole
    //    - eMultimedia
    //    - eCommunications
    //   pwstrDefaultDevice [in]
    //    Pointer to the endpoint ID string that identifies the audio endpoint device.
    //    This parameter points to a null-terminated, wide-character string containing the endpoint ID.
    //    The string remains valid for the duration of the call.
    //    If the user has removed or disabled the default device for a particular role,
    //    and no other device is available to assume that role, then pwstrDefaultDevice is NIL.

    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                    key: PROPERTYKEY): HRESULT; stdcall;
    // The OnPropertyValueChanged method indicates that the value of a property belonging to an audio endpoint device has changed.
    // Parameters
    //  pwstrDeviceId [in]
    //   Pointer to the endpoint ID string that identifies the audio endpoint device.
    //   This parameter points to a null-terminated, wide-character string that contains the endpoint ID.
    //   The string remains valid for the duration of the call.
    //  key [in]
    //   A PROPERTYKEY structure that specifies the property.
    //   The structure contains the property-set GUID and an index identifying a property within the set.
    //   The structure is passed by value. It remains valid for the duration of the call.
    //   For more information about PROPERTYKEY, see the Windows SDK documentation.
    //
    // Remarks
    //   A call to the IPropertyStore.SetValue method that successfully changes the value of
    //   a property of an audio endpoint device generates a call to OnPropertyValueChanged.
    //   For more information about IPropertyStore.SetValue, see the Windows SDK documentation.
    //   A client can use the key parameter to retrieve the new property value.
  end;
  IID_IMMNotificationClient = IMMNotificationClient;
  {$EXTERNALSYM IID_IMMNotificationClient}


  // Interface IMMDevice
  // ===================
  {
   The IMMDevice interface encapsulates the generic features of a multimedia device resource.
   In the current implementation of the MMDevice API, the only type of device resource that an
   IMMDevice interface can represent is an audio endpoint device.

   NOTE:
    A client can obtain an IMMDevice interface from one of the following methods:
      IMMDeviceCollection.Item
      IMMDeviceEnumerator.GetDefaultAudioEndpoint
      IMMDeviceEnumerator.GetDevice
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMDevice);'}
  {$EXTERNALSYM IMMDevice}
  IMMDevice = interface(IUnknown)
  ['{D666063F-1587-4E43-81F1-B948E807363F}']

    function Activate(const iid: REFIID;
                      dwClsCtx: DWORD;
                      {In_opt} pActivationParams: PPROPVARIANT;
                      out ppInterface: Pointer): HRESULT; stdcall;  // Replaced IUNKOWN pointer to a pointer, as described on ms-docs:
                                                                    // https://docs.microsoft.com/us-en/windows/win32/api/mmdeviceapi/nf-mmdeviceapi-immdevice-activate
    function OpenPropertyStore(stgmAccess: DWORD;
                               out ppProperties: IPropertyStore): HRESULT; stdcall;

    function GetId(out ppstrId: PWideChar): HRESULT; stdcall;    //250815a, modified; issue reported by mbergstrand

    function GetState(out pdwState: UINT): HRESULT; stdcall;
    // Parameters
    // pdwState [out]
    //  Pointer to a DWORD variable into which the method writes the current state of the device.
    //  The device-state value is one of the following DEVICE_STATE_XXX constants:
    //    DEVICE_STATE_ACTIVE
    //    DEVICE_STATE_DISABLED
    //    DEVICE_STATE_NOTPRESENT
    //    DEVICE_STATE_UNPLUGGED

  end;
  IID_IMMDevice = IMMDevice;
  {$EXTERNALSYM IID_IMMDevice}


  // Interface IMMDeviceCollection
  // =============================
  {
   The IMMDeviceCollection interface represents a collection of multimedia device resources.
   In the current implementation, the only device resources that the MMDevice API can create collections
   of all audio endpoint devices.
   A client can obtain a reference to an IMMDeviceCollection interface instance by calling the
   IMMDeviceEnumerator.EnumAudioEndpoints method.
   This method creates a collection of endpoint objects, each of which represents an
   audio endpoint device in the system.
   Each endpoint object in the collection supports the IMMDevice and IMMEndpoint interfaces.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMDeviceCollection);'}
  {$EXTERNALSYM IMMDeviceCollection}
  IMMDeviceCollection = interface(IUnknown)
  ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']

    function GetCount(out pcDevices: UINT): HRESULT; stdcall;

    function Item(nDevice: UINT;
                  out ppDevice: IMMDevice): HRESULT; stdcall;

  end;
  IID_IMMDeviceCollection = IMMDeviceCollection;
  {$EXTERNALSYM IID_IMMDeviceCollection}


  // Interface IMMEndpoint
  // =====================
  {
   The IMMEndpoint interface represents an audio endpoint device.
   A client obtains a reference to an IMMEndpoint interface instance by following these steps:
    By using one of the techniques described in IMMDevice Interface,
    obtain a reference to the IMMDevice interface of an audio endpoint device.
   Call the IMMDevice.QueryInterface method with parameter iid set to REFIID IID_IMMEndpoint.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMEndpoint);'}
  {$EXTERNALSYM IMMEndpoint}
  IMMEndpoint = interface(IUnknown)
  ['{1BE09788-6894-4089-8586-9A2A6C265AC5}']

    function GetDataFlow(out pDataFlow: eDataFlow): HRESULT; stdcall;

  end;
  IID_IMMEndpoint = IMMEndpoint;
  {$EXTERNALSYM IID_IMMEndpoint}


  // Interface IMMDeviceEnumerator
  // =============================
  {
   The IMMDeviceEnumerator interface provides methods for enumerating multimedia device resources.
   In the current implementation of the MMDevice API, the only device resources that this interface
   can enumerate are audio endpoint devices.
   A client obtains a reference to an IMMDeviceEnumerator interface by calling the CoCreateInstance function,
   as described previously (see MMDevice API).

   NOTE:
     The device resources enumerated by the methods in the IMMDeviceEnumerator interface are represented
     as collections of objects with IMMDevice interfaces.
     A collection has an IMMDeviceCollection interface.
     The IMMDeviceEnumerator.EnumAudioEndpoints method creates a device collection.

   To obtain a pointer to the IMMDevice interface of an item in a device collection,
   the client calls the IMMDeviceCollection.Item method.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMDeviceEnumerator);'}
  {$EXTERNALSYM IMMDeviceEnumerator}
  IMMDeviceEnumerator = interface(IUnknown)
  ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']

    function EnumAudioEndpoints(dataFlow: EDataFlow;
                                const dwStateMask: DWORD;   // DEVICE_STATE_XXX constants
                                out ppDevices: IMMDeviceCollection): HRESULT; stdcall;

    function GetDefaultAudioEndpoint(dataFlow: EDataFlow;
                                     role: eRole;
                                     out ppEndpoint: IMMDevice): HRESULT; stdcall;

    function GetDevice(pwstrId: PWChar;  // Pointer to a string containing the endpoint ID.
                                         // The caller typically obtains this string from the IMMDevice.GetId method or
                                         // from one of the methods in the IMMNotificationClient interface.
                       out ppDevice: IMMDevice): HRESULT; stdcall;

    function RegisterEndpointNotificationCallback(pClient: IMMNotificationClient): HRESULT; stdcall;

    function UnregisterEndpointNotificationCallback(pClient: IMMNotificationClient): HRESULT; stdcall;

  end;
  IID_IMMDeviceEnumerator = IMMDeviceEnumerator;
  {$EXTERNALSYM IID_IMMDeviceEnumerator}


  // Interface IMMDeviceActivator
  // ============================
  // Interface provided by an object that can be activated on a device.  i.e Components that
  // provide interfaces produced by IMMDevice.Activate must support this interface.
  // NOTE: IMMDeviceActivator is reserved for system use.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMMDeviceActivator);'}
  {$EXTERNALSYM IMMDeviceActivator}
  IMMDeviceActivator = interface(IUnknown)
  ['{3B0D0EA4-D0A9-4B0E-935B-09516746FAC0}']
    function Activate(const iid: REFIID;
                      pDevice: IMMDevice;
                      pActivationParams: PROPVARIANT;
                      out ppInterface: Pointer): HRESULT; stdcall;
    // Parameters:
    //      iid               - [in] The specified interface
    //      pDevice           - [in] The specified device
    //      pActivationParams - [in] Component specific data.
    //                               This is supplied by clients in a call to IMMDevice.Activate
    //      ppInterface       - [out] Address of a pointer that will receive the specified interface
    //
    // Remarks:
    //      The caller is responsible for releasing ppInterface using IUnknown.Release()
    //
  end;
  IID_IMMDeviceActivator = IMMDeviceActivator;
  {$EXTERNALSYM IID_IMMDeviceActivator}



  // Interface IActivateAudioInterfaceCompletionHandler
  // ==================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IActivateAudioInterfaceCompletionHandler);'}
  {$EXTERNALSYM IActivateAudioInterfaceCompletionHandler}
  IActivateAudioInterfaceCompletionHandler = interface(IUnknown)
  ['{41D949AB-9862-444A-80F6-C261334DA5EB}']

    function ActivateCompleted(activateOperation: IActivateAudioInterfaceAsyncOperation): HRESULT; stdcall;

  end;
  IID_IActivateAudioInterfaceCompletionHandler = IActivateAudioInterfaceCompletionHandler;
  {$EXTERNALSYM IID_IActivateAudioInterfaceCompletionHandler}



  // Interface IActivateAudioInterfaceAsyncOperation
  // ===============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IActivateAudioInterfaceAsyncOperation);'}
  {$EXTERNALSYM IActivateAudioInterfaceAsyncOperation}
  IActivateAudioInterfaceAsyncOperation = interface(IUnknown)
  ['{72A22D78-CDE4-431D-B8CC-843A71199B6D}']

    function GetActivateResult(out activateResult: HResult;
                               out activatedInterface: IUnknown): HResult; stdcall;

  end;
  IID_IActivateAudioInterfaceAsyncOperation = IActivateAudioInterfaceAsyncOperation;
  {$EXTERNALSYM IID_IActivateAudioInterfaceAsyncOperation}


  // ----------------------------------------------------------------------
  // Function: ActivateAudioInterfaceAsync
  // This function takes
  // * a device interface instance identifier representing either
  //     - an audio device interface instance (e.g., built-in speakers), or
  //     - an device interface class (e.g., audio render devices)
  // * a COM interface identifier
  // * activation parameters specific to the interface being activated
  //   and asynchronously returns a pointer to the specified interface
  // ----------------------------------------------------------------------
  function ActivateAudioInterfaceAsync(deviceInterfacePath: LPCWSTR;
                                       const riid: REFIID;
                                       {opt} activationParams: PROPVARIANT;
                                       completionHandler: IActivateAudioInterfaceCompletionHandler;
                                       out activationOperation: IActivateAudioInterfaceAsyncOperation): HResult; stdcall;
  {$EXTERNALSYM ActivateAudioInterfaceAsync}


type

  PAudioExtensionParams = ^__MIDL___MIDL_itf_mmdeviceapi_0000_0008_0001;
  __MIDL___MIDL_itf_mmdeviceapi_0000_0008_0001 = record
    AddPageParam: LPARAM;
    pEndpoint: IMMDevice;
    pPnpInterface: IMMDevice;
    pPnpDevnode: IMMDevice;
  end;
  {$EXTERNALSYM __MIDL___MIDL_itf_mmdeviceapi_0000_0008_0001}
  AudioExtensionParams = __MIDL___MIDL_itf_mmdeviceapi_0000_0008_0001;
  {$EXTERNALSYM AudioExtensionParams}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

const
  MMDeviceApiLib = 'Mmdevapi.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function ActivateAudioInterfaceAsync; external MMDeviceApiLib name 'ActivateAudioInterfaceAsync' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
