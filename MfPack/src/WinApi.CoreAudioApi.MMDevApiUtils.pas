// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - CoreAudio - Utilities
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.MMDevApiUtils.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.1.7
// Description: MMDevApiUtils, Device api helper routines >= Win 8
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
// 28/05/2024 Tony                Added funtion GetDeviceDataFlow.
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
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
// Source: https://learn.microsoft.com/en-us/windows/win32/coreaudio/interoperability-with-legacy-audio-apis
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
unit WinApi.CoreAudioApi.MMDevApiUtils;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.MMSystem,
  WinApi.WinApiTypes,
  WinApi.StrMif,
  WinApi.Coml2Api,
  WinApi.ComBaseApi,
  {WinMM}
  WinApi.WinMM.MMreg,
  WinApi.WinMM.MMDdk,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjBase,
  {$ENDIF}


  {System}
  System.Classes,
  System.Win.ComObj,
  System.SysUtils,
  System.VarUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.EndPointVolume,
  WinApi.CoreAudioApi.DeviceTopology,
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const
  STRSAFE_MAX_CCH = 2147483647;  // Maximum supported buffer size, in characters (same as INT_MAX)

  // State indicators
  DEV_STATE_ACTIVE     : string = 'Active';
  DEV_STATE_DISABLED   : string = 'Disabled';
  DEV_STATE_NOTPRESENT : string = 'Not Present';
  DEV_STATE_UNPLUGGED  : string = 'Unplugged';
  DEV_STATEMASK_ALL    : string = 'All States'; // only use for queries

type
  PEndPointDevice = ^EndPointDevice;
  {$NODEFINE PEndPointDevice}
  _EndPointDevice = record
   {Device Properties}
    DevInterfaceName: LPWSTR;  // The friendly name of the audio adapter to which the endpoint device is attached (for example, "XYZ Audio Adapter").
    DeviceDesc: LPWSTR;        // The device description of the endpoint device (for example, "Speakers").
    DeviceName: LPWSTR;        // The friendly name of the endpoint device (for example, "Speakers (XYZ Audio Adapter)").
    pwszID: LPWSTR;            // Internal ID.
    dwState: DWord;            // State of the device (disconnected, active, unplugged or not present).
    sState: string;            // State of the device in readable string.
    iID: Integer;              // Device index ID, starting with 0.
    DataFlow: EDataFlow;       // eRender or eCapture.
    Device: IMMDevice;         // Device interface.
  end;
  EndPointDevice = _EndPointDevice;
  TEndPointDevice = _EndPointDevice;

  TEndPointDeviceArray = array of TEndPointDevice;

type
 //-----------------------------------------------------------
 // Register the application to receive notifications when the
 // volume level changes on the default process-specific audio
 // session (with session GUID value GUID_NULL) on the audio
 // endpoint device with the specified data-flow direction
 // (eRender or eCapture) and device role.
 // See for more info:
 //   https://docs.microsoft.com/en-us/windows/win32/coreaudio/audio-events-for-legacy-audio-applications
 //-----------------------------------------------------------

  TAudioVolumeEvents = class(TObject)
  private
    hrStatus: HRESULT;
    pManager: IAudioSessionManager;
    pControl: IAudioSessionControl;
    pAudioEvents: IAudioSessionEvents;

  public

    constructor Create(flow: eDataFlow;
                       role: eRole;
                       AudioEvents: IAudioSessionEvents);

    destructor Destroy; override;

    function GetStatus(): HRESULT;  { return _hrStatus; }

  end;

  //-----------------------------------------------------------
  // Get endpoint devices
  //
  // This function enumerates all audio rendering or capture endpoint devices.
  // It returns an array of TEndPointDevice.
  // Params:
  //   {in}     flow: eRender or eCapture
  //   {in}     state: Set this parameter to the bitwise OR of one or more DEVICE_STATE_XXX constants
  //   {out}    endpointdevices: Returns an array of TEndPointDevices.
  //   {out}    Number of endpoints returned.
  //-----------------------------------------------------------
  function GetEndpointDevices(const flow: EDataFlow;
                              state: DWord;
                              out endpointdevices: TEndPointDeviceArray;
                              out devicesCount: DWord): HRESULT;

  // Get audioendpoint by a string containing the endpoint ID.
  // The caller typically obtains this string from the IMMDevice.GetId method (see: GetEndpointDevices) or
  // from one of the methods in the IMMNotificationClient interface.
  function GetEndPointDeviceByID(const deviceId: PWideChar;
                                 out audioEndPoint: IMMDevice): HResult;

  function GetDefaultEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                         Role: eRole = eMultimedia;
                                         dataFlow: EDataFlow = eRender): HRESULT;

  // Get the device descriptions of a device
  function GetDeviceDescriptions(DefaultDevice: IMMDevice; // the zero based index, where 0 is the default (active) endpoint.
                                 const DevicePkey: PROPERTYKEY; // Possible values are: PKEY_Device_FriendlyName, PKEY_Device_DeviceDesc or PKEY_DeviceInterface_FriendlyName.
                                 out deviceDesc: WideString): HRESULT;

  function GetDeviceStateAsString(state: DWord): string;

  // The DirectShow API does not provide a means for an application to select the
  // audio endpoint device that is assigned to a particular device role.
  // However, in Windows Vista, the core audio APIs can be used in conjunction with
  // a DirectShow application to enable device selection based on device role.
  // With the help of the core audio APIs, the application can:
  // - Identify the audio endpoint device that the user has assigned to a particular device role.
  // - Create a DirectShow audio rendering filter with an IBaseFilter interface that
  //   encapsulates the audio endpoint device.
  // - Build a DirectShow graph that incorporates the filter.
  // For more information about DirectShow and IBaseFilter, see the Windows SDK documentation.

  //-----------------------------------------------------------
  // Create a DirectShow audio rendering filter that
  // encapsulates the audio endpoint device that is currently
  // assigned to the specified device role.
  //-----------------------------------------------------------
  function CreateDShowAudioRenderer(const role: eRole;
                                    ppAudioRenderer: PIBaseFilter): HRESULT;


  // The DirectSound API does not provide a means for an application to select the
  // audio endpoint device that the user has assigned to a particular device role.
  // However, in Windows Vista, the core audio APIs can be used in conjunction with
  // a DirectSound application to enable device selection based on device role.
  // With the help of the core audio APIs, the application can identify the audio
  // endpoint device that is assigned to a particular role,
  // get the DirectSound device GUID for the endpoint device,
  // and call the DirectSoundCreate or DirectSoundCaptureCreate function to create
  // an IDirectSound or IDirectSoundCapture interface instance that encapsulates
  // the endpoint device.
  // For more information about DirectSound, see the Windows SDK documentation.

  //-----------------------------------------------------------
  // Get the DirectSound or DirectSoundCapture device GUID for
  // an audio endpoint device. If flow = eRender, the function
  // gets the DirectSound device GUID for the rendering device
  // with the specified device role. If flow = eCapture, the
  // function gets the DirectSoundCapture device GUID for the
  // capture device with the specified device role.
  //-----------------------------------------------------------
  function GetDirectSoundGuid(flow: eDataFlow;
                              role: eRole;
                              out pDevGuid: TGuid): HRESULT;


  // The legacy Windows multimedia waveOutXxx and waveInXxx functions provide no
  // means for an application to select the audio endpoint device that the user
  // has assigned to a particular device role. However, in Windows Vista,
  // the core audio APIs can be used in conjunction with a Windows multimedia application
  // to enable device selection based on device role.
  // For example, with the help of the MMDevice API, a waveOutXxx application can
  // identify the audio endpoint device that is assigned to a role,
  // identify the corresponding waveform output device, and call the waveOutOpen
  // function to open an instance of the device.
  // For more information about waveOutXxx and waveInXxx, see the Windows SDK documentation.
  //
  //-----------------------------------------------------------
  // This function gets the waveOut ID of the audio endpoint
  // device that is currently assigned to the specified device
  // role. The caller can use the waveOut ID to open the
  // waveOut device that corresponds to the endpoint device.
  //-----------------------------------------------------------
  function GetWaveOutId(role: ERole;
                        out pWaveOutId: PInteger): HRESULT;

  //-----------------------------------------------------------
  // Get the IKsJackDescription interface that describes the
  // audio jack or jacks that the endpoint device plugs into.
  //-----------------------------------------------------------
  function GetJackInfo(pDevice: IMMDevice;
                       out ppJackDesc: IKsJackDescription): HResult;

  //-----------------------------------------------------------
  // Get the dataflow direction of an endpoint device.
  //-----------------------------------------------------------
  function GetDeviceDataFlow(pDevice: IMMDevice;
                             out pDataFlow: eDataFlow): HResult;


implementation

// Constructor
constructor TAudioVolumeEvents.Create(flow: EDataFlow;
                                      role: ERole;
                                      AudioEvents: IAudioSessionEvents);
var
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

begin
  inherited Create;

  hrStatus := S_OK;
  pManager := Nil;
  pControl := Nil;
  pAudioEvents := AudioEvents;

try

  if not Assigned(pAudioEvents) then
    begin
      hrStatus := E_POINTER;
      Abort;
    end;


  // Get the enumerator for the audio endpoint devices
  // on this system.
  hrStatus := CoCreateInstance(CLSID_MMDeviceEnumerator,
                               Nil,
                               CLSCTX_INPROC_SERVER,
                               IID_IMMDeviceEnumerator,
                               pEnumerator);

  if Failed(hrStatus) then
    Abort;

  // Get the audio endpoint device with the specified data-flow
  // direction (eRender or eCapture) and device role.
  hrStatus := pEnumerator.GetDefaultAudioEndpoint(flow,
                                                  role,
                                                  pDevice);
  if Failed(hrStatus) then
    Abort;

  // Get the session manager for the endpoint device.
  hrStatus := pDevice.Activate(IID_IAudioSessionManager,
                               CLSCTX_INPROC_SERVER,
                               Nil,
                               Pointer(pManager));
  if Failed(hrStatus) then
    Abort;


  // Get the control interface for the process-specific audio
  // session with session GUID = GUID_NULL. This is the session
  // that an audio stream for a DirectSound, DirectShow, waveOut,
  // or PlaySound application stream belongs to by default.
  hrStatus := pManager.GetAudioSessionControl(Nil,
                                              0,
                                              pControl);
  if Failed(hrStatus) then
    Abort;

  hrStatus := pControl.RegisterAudioSessionNotification(pAudioEvents);
  if Failed(hrStatus) then
    Abort;
except
  // Do nothing
end;

end;

// Destructor
destructor TAudioVolumeEvents.Destroy();
begin
  if Assigned(pControl) then
    begin
      pControl.UnregisterAudioSessionNotification(pAudioEvents);
    end;
  SafeRelease(pManager);
  SafeRelease(pControl);
  inherited Destroy;
end;


function TAudioVolumeEvents.GetStatus(): HResult;
begin
  Result := hrStatus;
end;

// eof AudioVolumeEvents


function GetEndPointDeviceByID(const deviceId: PWideChar;
                               out audioEndPoint: IMMDevice): HResult;
var
  pdeviceEnumerator: IMMDeviceEnumerator;
  hr: HRESULT;

begin
  hr := E_FAIL;
try
try
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pdeviceEnumerator);
  if Failed(hr) then
    Abort;

  hr := pdeviceEnumerator.GetDevice(deviceId,
                                    audioEndPoint);
  if Failed(hr) then
    Abort;

except
  // Do Nothing. Caller is responsible for error handling.
end;
finally
  Result := hr;
end;
end;


// Get GetEndpointDevices
// Remarks: the index of endpointdevices indicates also the device index.
// Parameters
// flow: eRender, eCapture or eAll.
// state: value of this parameter is one of the following: DEVICE_STATE_ACTIVE
//                                                         DEVICE_STATE_DISABLED
//                                                         DEVICE_STATE_NOTPRESENT
//                                                         DEVICE_STATE_UNPLUGGED
// endpointdevices: Dynamic array that holds EndPointDevice properties.
// devicesCount: Number of specified devices found
function GetEndpointDevices(const flow: EDataFlow;
                            state: DWord;
                            out endpointdevices: TEndPointDeviceArray;
                            out devicesCount: DWord): HRESULT;
var
  hr: HResult;
  pEnumerator: IMMDeviceEnumerator;
  pCollection: IMMDeviceCollection;
  pEndpoint: IMMDevice;
  pProps: IPropertyStore;
  DevIfaceName: PROPVARIANT;
  DevDesc: PROPVARIANT;
  DevName: PROPVARIANT;
  pwszID: PWideChar;
  count: UINT;
  i: Integer;
  dwState: DWord;

label
  done;

  procedure CheckHr(hres: HResult);
    begin
      if FAILED(hres) then
        Abort;
    end;

begin
  hr := S_OK;

  SetLength(endpointdevices,
            0);

  if (state = 0) then
    state := $0000000F; {15 = ALL}

  // Create the enumerator
  CheckHr(CoCreateInstance(CLSID_MMDeviceEnumerator,
                           nil,
                           CLSCTX_ALL,
                           IID_IMMDeviceEnumerator,
                           pEnumerator));

  CheckHr(pEnumerator.EnumAudioEndpoints(flow,
                                         state,
                                         pCollection));

  CheckHr(pCollection.GetCount(count));

  // No endpoints found.
  if (count = 0) then
    begin
      devicesCount := 0;
      hr := MF_E_NOT_FOUND;
      goto done;
    end;

  // Store devices found
  devicesCount := count;
  // expand the array (in case we have an open array)
  SetLength(endpointdevices,
            count);
  // Initialize containers for property values.
  PropVariantInit(DevIfaceName);
  PropVariantInit(DevDesc);
  PropVariantInit(DevName);

  // Each loop gets the properties of an endpoint device.
  for i := 0 to count -1 do
    begin
      // Get pointer to endpoint i.
      CheckHr(pCollection.Item(i,
                               pEndpoint));
      // Get the endpoint ID string.
      CheckHr(pEndpoint.GetId(pwszID));
      // Get the endpoint state
      CheckHr(pEndpoint.GetState(dwState));
      // Open propertystore, to get device descriptions
      CheckHr(pEndpoint.OpenPropertyStore(STGM_READ,
                                          pProps));

      // Get the endpoint's friendly-name property.
      CheckHr(pProps.GetValue(PKEY_DeviceInterface_FriendlyName,
                              DevIfaceName));
      // Get the endpoint's device description property.
      CheckHr(pProps.GetValue(PKEY_Device_DeviceDesc,
                              DevDesc));
      // Get the endpoint's device name property.
      CheckHr(pProps.GetValue(PKEY_Device_FriendlyName,
                              DevName));

      // Store endpoint's properties in array.
      endpointdevices[i].DevInterfaceName := DevIfaceName.pwszVal;
      endpointdevices[i].DeviceDesc := DevDesc.pwszVal;
      endpointdevices[i].DeviceName := DevName.pwszVal;
      endpointdevices[i].pwszID := pwszID;
      endpointdevices[i].dwState := dwState;
      endpointdevices[i].sState := GetDeviceStateAsString(dwState);
      endpointdevices[i].iID := i;
      endpointdevices[i].DataFlow := EDataFlow(flow);
      endpointdevices[i].Device := pEndpoint;
      SafeRelease(pProps);
      SafeRelease(pEndpoint);
    end;

done:
  PropVariantClearSafe(DevIfaceName);
  PropVariantClearSafe(DevDesc);
  PropVariantClearSafe(DevName);
  pwszID := nil;
  Result := hr;
end;


// Get a reference to the endpoint of the default communication device for
// rendering an audio stream.
function GetDefaultEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                       Role: eRole = eMultimedia;
                                       dataFlow: EDataFlow = eRender): HRESULT;
var
  pdeviceEnumerator: IMMDeviceEnumerator;
  hr: HRESULT;

begin
  hr := E_FAIL;
try
try
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pdeviceEnumerator);
  if Failed(hr) then
    Abort;

  if (dataFlow = eRender) or (dataFlow = eCapture) then  // Can only be eRender or eCapture
    begin
      hr := pdeviceEnumerator.GetDefaultAudioEndpoint(dataFlow,
                                                      Role,
                                                      audioEndPoint);
      if Failed(hr) then
        Abort;
    end
  else
    hr := E_INVALIDARG;

except
  // Do Nothing. Caller is responsible for error handling.
end;
finally
  Result := hr;
end;
end;


// Get the device descriptions of a device
function GetDeviceDescriptions(DefaultDevice: IMMDevice;
                               const DevicePkey: PROPERTYKEY; // Possible values are: PKEY_Device_FriendlyName, PKEY_Device_DeviceDesc or PKEY_DeviceInterface_FriendlyName.
                               out deviceDesc: WideString): HRESULT;
var
  hr: HResult;
  psPropertyStore: IPropertyStore;
  pvvar: PROPVARIANT;

label
  done;

begin
  PropVariantInit(pvvar);
  hr := DefaultDevice.OpenPropertyStore(STGM_READ,
                                        psPropertyStore);
  if SUCCEEDED(hr) then
    begin
      hr := psPropertyStore.GetValue(DevicePkey,
                                     pvvar);
      if FAILED(hr) then
        begin
          OleCheck(hr);
          goto done;
        end;

      deviceDesc := pvvar.pwszVal;
    end;

done:
  PropVariantClear(pvvar);
  Result := hr;
end;


function GetDeviceStateAsString(state: DWord): string;
begin
  case state of
    DEVICE_STATE_ACTIVE:     Result := DEV_STATE_ACTIVE;
    DEVICE_STATE_DISABLED:   Result := DEV_STATE_DISABLED;
    DEVICE_STATE_NOTPRESENT: Result := DEV_STATE_NOTPRESENT;
    DEVICE_STATE_UNPLUGGED:  Result := DEV_STATE_UNPLUGGED;
    DEVICE_STATEMASK_ALL:    Result := DEV_STATEMASK_ALL;
  end;
end;

//-----------------------------------------------------------
// Create a DirectShow audio rendering filter that
// encapsulates the audio endpoint device that is currently
// assigned to the specified device role.
// Source: http://msdn.microsoft.com/en-us/library/windows/desktop/dd370815
//-----------------------------------------------------------
function CreateDShowAudioRenderer(const role: eRole;
                                  ppAudioRenderer: PIBaseFilter): HRESULT;
var
  hr: HRESULT;
  daap: PDIRECTX_AUDIO_ACTIVATION_PARAMS;
  pVar: PROPVARIANT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  guidAudioSessionId: TGUID;

begin
  hr := E_FAIL;  // to prevent might not have been initialised warning

  if (ppAudioRenderer = Nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

try
try

  // Activate the IBaseFilter interface on the
  // audio renderer with the specified role.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         IUnknown(pEnumerator));
  if Failed(hr) then
    Abort;

  hr := pEnumerator.GetDefaultAudioEndpoint(EDataFlow(eRender),
                                            ERole(role),
                                            pDevice);
  if Failed(hr) then
    Abort;

  // Create this application's audio session GUID
  // We could just write this, however not every Delphi compiler supports this: guidAudioSessionId := TGUID.NewGuid;
  hr := CreateGUID(guidAudioSessionId);
  if Failed(hr) then
    Abort;

  // daap  DIRECTX_AUDIO_ACTIVATION_PARAMS
  daap.cbDirectXAudioActivationParams := SizeOf(daap);
  daap.guidAudioSession := guidAudioSessionId;
  daap.dwAudioStreamFlags := AUDCLNT_STREAMFLAGS_CROSSPROCESS;

  //
  PropVariantInit(pVar);

  pVar.vt := VT_BLOB;
  pVar.blob.cbSize := SizeOf(daap);
  pVar.blob.pBlobData := @daap;

  hr := pDevice.Activate(IID_IBaseFilter,
                         CLSCTX_ALL,
                         @pVar,
                         Pointer(ppAudioRenderer));
  if Failed(hr) then
    Abort;
except
  // Do Nothing. Caller is responsible for error handling.
end;
finally
  Result := hr;
end;
end;


//-----------------------------------------------------------
// Get the DirectSound or DirectSoundCapture device GUID for
// an audio endpoint device. If flow = eRender, the function
// gets the DirectSound device GUID for the rendering device
// with the specified device role. If flow = eCapture, the
// function gets the DirectSoundCapture device GUID for the
// capture device with the specified device role.
// Source: http://msdn.microsoft.com/en-us/library/windows/desktop/dd370817
//-----------------------------------------------------------
function GetDirectSoundGuid(flow: eDataFlow;
                            role: eRole;
                            out pDevGuid: TGuid): HRESULT;
var
  hr: HRESULT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  pProps: IPropertyStore;
  pVar: PROPVARIANT;

begin
  hr:= S_OK;
try
try
  PropVariantInit(pVar);

  if (SizeOf(pDevGuid) = 0) then
    begin
      Result := E_POINTER;
      Abort;
    end;

  // Get a device enumerator for the audio endpoint
  // devices in the system.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         IUnknown(pEnumerator));
  if Failed(hr) then
    Abort;

  // Get the endpoint device with the specified dataflow
  // direction (eRender or eCapture) and device role.
  hr := pEnumerator.GetDefaultAudioEndpoint(flow,
                                            role,
                                            pDevice);
  if Failed(hr) then
    Abort;

  hr := pDevice.OpenPropertyStore(STGM_READ,
                                  pProps);
  if Failed(hr) then
    Abort;

  // Get the DirectSound or DirectSoundCapture device GUID
  // (in WCHAR string format) for the endpoint device.
  hr := pProps.GetValue(PKEY_AudioEndpoint_GUID,
                        pVar);
  if Failed(hr) then
    Abort;

  // Convert the WCHAR string to a GUID structure.
  hr := CLSIDFromString(pVar.pwszVal,
                        pDevGuid);
  if Failed(hr) then
    Abort;

except
  // Do Nothing. Caller is responsible for error handling.
end;
finally
  PropVariantClear(pVar);
  Result := hr;
end;
end;


function GetWaveOutId(role: ERole;
                      out pWaveOutId: PInteger): HRESULT;
var
  hr: HResult;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  pstrEndpointIdKey: PWideChar;
  pstrEndpointId: PWideChar;
  cbEndpointIdKey: size_t;
  waveOutId: Integer;
  cWaveOutDevices: Integer;
  mmr: MMRESULT;
  cbEndpointId: SIZE_T;

label
  leave;

begin

  pstrEndpointIdKey := nil;
  pstrEndpointId := Nil;
  cbEndpointId := 0;

  if (pWaveOutId = nil) then
    begin
      Result:= E_POINTER;
      Exit;
    end;

  // Create an audio endpoint device enumerator.
  hr := CoCreateInstance(IID_IMMDeviceEnumerator,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if Failed(hr) then
    goto leave;

  // Get the audio endpoint device that the user has
  // assigned to the specified device role.
  hr := pEnumerator.GetDefaultAudioEndpoint(EDataFlow(eRender),
                                            role,
                                            pDevice);
  if Failed(hr) then
    goto leave;

  // Get the endpoint ID string of the audio endpoint device.
  hr := pDevice.GetId(pstrEndpointIdKey);
  if Failed(hr) then
    goto leave;

  // Get the size of the endpoint ID string.
  cbEndpointIdKey := (Length(pstrEndpointIdKey) * SizeOf(WCHAR));

  // Include terminating null in string size.
  cbEndpointIdKey := cbEndpointIdKey + sizeof(WCHAR);

  // Allocate a buffer for a second string of the same size.
  pstrEndpointId := CoTaskMemAlloc(cbEndpointIdKey);
  if (pstrEndpointId = nil) then
    begin
      hr := E_OUTOFMEMORY;
      goto leave;
    end;

  // Each for-loop iteration below compares the endpoint ID
  // string of the audio endpoint device to the endpoint ID
  // string of an enumerated waveOut device. If the strings
  // match, then we've found the waveOut device that is
  // assigned to the specified device role.

  cWaveOutDevices := waveOutGetNumDevs();

  for waveOutId := 0 to waveOutId -1 do
    begin
      if (waveOutId < cWaveOutDevices) then
        begin
          // Get the size (including the terminating null) of
          // the endpoint ID string of the waveOut device.
          mmr := waveOutMessage(HWAVEOUT(waveOutId),
                                DRV_QUERYFUNCTIONINSTANCEIDSIZE,
                                DWORD_PTR(cbEndpointId),
                                DWORD_PTR(Nil));

          if (mmr <> MMSYSERR_NOERROR) Or
             (cbEndpointIdKey <> cbEndpointId) then // do sizes match?
            begin
              continue;  // not a matching device
            end;

          // Get the endpoint ID string for this waveOut device.
          mmr := waveOutMessage(HWAVEOUT(waveOutId),
                                DRV_QUERYFUNCTIONINSTANCEID,
                                DWORD_PTR(pstrEndpointId),
                                cbEndpointId);

          if (mmr <> MMSYSERR_NOERROR) then
            begin
              continue;
            end;

          // Check whether the endpoint ID string of this waveOut
          // device matches that of the audio endpoint device.
          if (lstrcmpi(pstrEndpointId, pstrEndpointIdKey) = 0) then
            begin
              pWaveOutId := @waveOutId;  // found match
              hr := S_OK;
              Break;
            end;
        end;
    end;

    if (waveOutId = cWaveOutDevices) then
      begin
        // We reached the end of the for-loop above without
        // finding a waveOut device with a matching endpoint
        // ID string. This behavior is quite unexpected.
        hr := E_UNEXPECTED;
      end;

leave:
    CoTaskMemFree(pstrEndpointIdKey);  // Nil pointer okay
    CoTaskMemFree(pstrEndpointId);
    Result := hr;
end;


function GetJackInfo(pDevice: IMMDevice;
                     out ppJackDesc: IKsJackDescription): HResult;
var
  hr: HResult;
  pDeviceTopology: IDeviceTopology;
  pConnFrom: IConnector;
  pConnTo: IConnector;
  pPart: IPart;
  pJackDesc: IKsJackDescription;

label
  leave;

begin

  // Get the endpoint device's IDeviceTopology interface.
  hr := pDevice.Activate(IID_IDeviceTopology,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pDeviceTopology));

  if FAILED(hr) then
    goto leave;

  // The device topology for an endpoint device always
  // contains just one connector (connector number 0).
  hr := pDeviceTopology.GetConnector(0,
                                     pConnFrom);
  if FAILED(hr) then
    goto leave;

  // Step across the connection to the jack on the adapter.
  hr := pConnFrom.GetConnectedTo(pConnTo);

  if (hr = ERROR_PATH_NOT_FOUND) then
    // The adapter device is not currently active.
    hr := E_NOINTERFACE;

  if FAILED(hr) then
    goto leave;


  // Get the connector's IPart interface.
  hr := pConnTo.QueryInterface(IID_IPart,
                               Pointer(pPart));
  if FAILED(hr) then
    goto leave;

  // Activate the connector's IKsJackDescription interface.
  hr := pPart.Activate(CLSCTX_INPROC_SERVER,
                       IID_IKsJackDescription,
                       Pointer(pJackDesc));
  if FAILED(hr) then
    goto leave;

  ppJackDesc := pJackDesc;

leave:
  Result := hr;
end;


function GetDeviceDataFlow(pDevice: IMMDevice;
                           out pDataFlow: eDataFlow): HResult;
var
  hr: HResult;
  endPoint: IMMEndpoint;

begin

  // Query for IMMEndpoint.
  hr := pDevice.QueryInterface(IID_IMMEndpoint,
                               IUnknown(endPoint));

  if SUCCEEDED(hr) then
    hr := endPoint.GetDataFlow(pDataFlow);

  Result := hr;
end;


end.
