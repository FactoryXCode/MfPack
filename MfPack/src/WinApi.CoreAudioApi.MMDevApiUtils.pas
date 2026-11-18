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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
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
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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

  // Entries in MfEnumerateEndpoints carry this real object, never a cast string pointer.
  TEndpointIdObject = class(TObject)
  private
    FId: string;
  public
    constructor Create(const AId: string);
    property Id: string read FId;
  end;

  // "eState" (dwStateMask) as used by IMMDeviceEnumerator.EnumAudioEndpoints
  // Possible values or combinations:
  //   DEVICE_STATE_ACTIVE, DEVICE_STATE_DISABLED, DEVICE_STATE_NOTPRESENT, DEVICE_STATE_UNPLUGGED
  //   or DEVICE_STATEMASK_ALL (DEVICE_STATEMASK_ALL should only be used in queries).
  eState = DWORD;
const
   DEVICE_STATE_ACTIVE      = eState($00000001);
   DEVICE_STATE_DISABLED    = eState($00000002);
   DEVICE_STATE_NOTPRESENT  = eState($00000004);
   DEVICE_STATE_UNPLUGGED   = eState($00000008);
   DEVICE_STATEMASK_ALL     = eState($0000000F);

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
    FRegistered: Boolean;
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
                              state: eState;
                              out endpointdevices: TEndPointDeviceArray;
                              out devicesCount: DWord): HRESULT;

  // Returned LPWSTR fields own CoTaskMem allocations. Clear before reuse and on completion.
  procedure ClearEndpointDevices(var Devices: TEndPointDeviceArray);

  // Frees endpoint ID objects and clears the list.
  procedure ClearEndpointList(List: TStrings);

  // Get audioendpoint by a string containing the endpoint ID.
  // The caller typically obtains this string from the IMMDevice.GetId method (see: GetEndpointDevices) or
  // from one of the methods in the IMMNotificationClient interface.
  function GetEndPointDeviceByID(const deviceId: PWideChar;
                                 out audioEndPoint: IMMDevice): HResult;

  function GetDefaultEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                         Role: eRole = eMultimedia;
                                         dataFlow: EDataFlow = eRender): HRESULT;

  // Same as function GetDefaultEndPointAudioDevice, but with extra option to get the device by ID.
  function GetEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                  const deviceID: string;
                                  Role: eRole = eMultimedia;
                                  dataFlow: EDataFlow = eRender): HRESULT;

  // Get the device descriptions of a device
  function GetDeviceDescriptions(DefaultDevice: IMMDevice; // the zero based index, where 0 is the default (active) endpoint.
                                 const DevicePkey: PROPERTYKEY; // Possible values are: PKEY_Device_FriendlyName, PKEY_Device_DeviceDesc or PKEY_DeviceInterface_FriendlyName.
                                 out deviceDesc: WideString): HRESULT;

  function GetDeviceStateAsString(state: DWord): string;


  function MfGetEndpointFriendlyName(Device: IMMDevice): string;
  function MfDeviceIdToFriendlyName(const DeviceID: string): string;

  // Objects are TEndpointIdObject instances; FPC list display text is UTF-8.
  procedure MfEnumerateEndpoints(DataFlow: EDataFlow;
                                 List: TStrings);



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
                        out pWaveOutId: UINT): HRESULT;

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

  //===========================================================
  // WASAPI streams request RAW mode bypass for Windows 10/11 Audio enhangements.
  // Audio enhancements is not recommended when using custom MFT's because it
  // consumes processor (CPU) time and audio latency.
  // It can also introduce "hiss" and "cracks" in the audio stream.
  //===========================================================
  procedure ConfigureRawAudioStream(const AAudioClient: IAudioClient;
                                    const ACategory: AUDIO_STREAM_CATEGORY;
                                    const AContext: string);

implementation

constructor TEndpointIdObject.Create(const AId: string);
begin
  inherited Create;
  FId := AId;
end;

constructor TAudioVolumeEvents.Create(flow: EDataFlow;
                                      role: ERole;
                                      AudioEvents: IAudioSessionEvents);
var
  Enumerator: IMMDeviceEnumerator;
  Device: IMMDevice;
begin
  inherited Create;
  pAudioEvents := AudioEvents;
  hrStatus := E_POINTER;
  if pAudioEvents = nil then
    Exit;
  hrStatus := CoCreateInstance(CLSID_MMDeviceEnumerator,nil,CLSCTX_INPROC_SERVER,
                               IID_IMMDeviceEnumerator,Enumerator);
  if FAILED(hrStatus) then
    Exit;
  hrStatus := Enumerator.GetDefaultAudioEndpoint(flow,role,Device);
  if FAILED(hrStatus) then
    Exit;
  hrStatus := Device.Activate(IID_IAudioSessionManager,CLSCTX_INPROC_SERVER,nil,Pointer(pManager));
  if FAILED(hrStatus) then
    Exit;
  hrStatus := pManager.GetAudioSessionControl(nil,0,pControl);
  if FAILED(hrStatus) then
    Exit;
  hrStatus := pControl.RegisterAudioSessionNotification(pAudioEvents);
  FRegistered := SUCCEEDED(hrStatus);
end;

destructor TAudioVolumeEvents.Destroy;
begin
  if FRegistered and (pControl <> nil) then
    pControl.UnregisterAudioSessionNotification(pAudioEvents);
  pControl := nil;
  pManager := nil;
  pAudioEvents := nil;
  inherited Destroy;
end;

function TAudioVolumeEvents.GetStatus(): HRESULT;
begin
  Result := hrStatus;
end;

function GetEndPointDeviceByID(const deviceId: PWideChar;
                               out audioEndPoint: IMMDevice): HRESULT;
var
  Enumerator: IMMDeviceEnumerator;
begin
  audioEndPoint := nil;
  if deviceId = nil then
    Exit(E_POINTER);
  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,nil,CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,Enumerator);
  if SUCCEEDED(Result) then
    Result := Enumerator.GetDevice(deviceId,audioEndPoint);
end;

function GetDefaultEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                       Role: ERole = eMultimedia;
                                       dataFlow: EDataFlow = eRender): HRESULT;
var
  Enumerator: IMMDeviceEnumerator;
begin
  audioEndPoint := nil;
  if (dataFlow <> eRender) and (dataFlow <> eCapture) then
    Exit(E_INVALIDARG);
  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,nil,CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,Enumerator);
  if SUCCEEDED(Result) then
    Result := Enumerator.GetDefaultAudioEndpoint(dataFlow,Role,audioEndPoint);
end;

function GetEndPointAudioDevice(out audioEndPoint: IMMDevice;
                                const deviceID: string;
                                Role: ERole = eMultimedia;
                                dataFlow: EDataFlow = eRender): HRESULT;
begin
  if deviceID = '' then
    Result := GetDefaultEndPointAudioDevice(audioEndPoint,Role,dataFlow)
  else
    Result := GetEndPointDeviceByID(PWideChar(deviceID),audioEndPoint);
end;

function GetDeviceDescriptions(DefaultDevice: IMMDevice;
                               const DevicePkey: PROPERTYKEY;
                               out deviceDesc: WideString): HRESULT;
var
  Store: IPropertyStore;
  Value: PROPVARIANT;
begin
  deviceDesc := '';
  if DefaultDevice = nil then
    Exit(E_POINTER);
  Result := DefaultDevice.OpenPropertyStore(STGM_READ,Store);
  if FAILED(Result) then
    Exit;
  PropVariantInit(Value);
  try
    Result := Store.GetValue(DevicePkey,Value);
    if SUCCEEDED(Result) then
    begin
      if Value.vt = VT_LPWSTR then
        deviceDesc := WideString(Value.pwszVal)
      else if Value.vt <> VT_EMPTY then
        Result := E_UNEXPECTED;
    end;
  finally
    PropVariantClear(Value);
  end;
end;

function DuplicateTaskString(const Value: WideString): LPWSTR;
var
  Bytes: SIZE_T;
begin
  Bytes := (SIZE_T(Length(Value))+1)*SizeOf(WideChar);
  Result := CoTaskMemAlloc(Bytes);
  if Result = nil then
    raise EOutOfMemory.Create('Endpoint string allocation failed');
  if Length(Value) = 0 then
    Result[0] := #0
  else
    Move(PWideChar(Value)^,Result^,Bytes);
end;

procedure ClearEndpointDevices(var Devices: TEndPointDeviceArray);
var
  I: Integer;
begin
  for I := 0 to High(Devices) do
  begin
    CoTaskMemFree(Devices[I].DevInterfaceName);
    CoTaskMemFree(Devices[I].DeviceDesc);
    CoTaskMemFree(Devices[I].DeviceName);
    CoTaskMemFree(Devices[I].pwszID);
    Devices[I].DevInterfaceName := nil;
    Devices[I].DeviceDesc := nil;
    Devices[I].DeviceName := nil;
    Devices[I].pwszID := nil;
  end;
  Devices := nil;
end;

function GetEndpointDevices(const flow: EDataFlow;
                            state: eState;
                            out endpointdevices: TEndPointDeviceArray;
                            out devicesCount: DWORD): HRESULT;
var
  Enumerator: IMMDeviceEnumerator;
  Collection: IMMDeviceCollection;
  Device: IMMDevice;
  Count: UINT;
  I: UINT;
  Description: WideString;
begin
  endpointdevices := nil;
  devicesCount := 0;
  if (flow <> eRender) and (flow <> eCapture) and (flow <> eAll) then
    Exit(E_INVALIDARG);
  if state = 0 then
    state := DEVICE_STATEMASK_ALL;
  if (state and not DEVICE_STATEMASK_ALL) <> 0 then
    Exit(E_INVALIDARG);
  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,nil,CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,Enumerator);
  if FAILED(Result) then
    Exit;
  Result := Enumerator.EnumAudioEndpoints(flow,state,Collection);
  if FAILED(Result) then
    Exit;
  Result := Collection.GetCount(Count);
  if FAILED(Result) then
    Exit;
  if Count = 0 then
    Exit(MF_E_NOT_FOUND);
  SetLength(endpointdevices,Count);
  try
    try
      for I := 0 to Count-1 do
      begin
        Result := Collection.Item(I,Device);
        if FAILED(Result) then
          Exit;
        Result := Device.GetId(endpointdevices[I].pwszID);
        if FAILED(Result) then
          Exit;
        Result := Device.GetState(endpointdevices[I].dwState);
        if FAILED(Result) then
          Exit;
        Result := GetDeviceDescriptions(Device,PKEY_DeviceInterface_FriendlyName,Description);
        if FAILED(Result) then
          Exit;
        endpointdevices[I].DevInterfaceName := DuplicateTaskString(Description);
        Result := GetDeviceDescriptions(Device,PKEY_Device_DeviceDesc,Description);
        if FAILED(Result) then
          Exit;
        endpointdevices[I].DeviceDesc := DuplicateTaskString(Description);
        Result := GetDeviceDescriptions(Device,PKEY_Device_FriendlyName,Description);
        if FAILED(Result) then
          Exit;
        endpointdevices[I].DeviceName := DuplicateTaskString(Description);
        endpointdevices[I].sState := GetDeviceStateAsString(endpointdevices[I].dwState);
        endpointdevices[I].iID := I;
        Result := GetDeviceDataFlow(Device,endpointdevices[I].DataFlow);
        if FAILED(Result) then
          Exit;
        endpointdevices[I].Device := Device;
      end;
      devicesCount := Count;
      Result := S_OK;
    except
      on E: EOutOfMemory do
        Result := E_OUTOFMEMORY;
    end;
  finally
    if FAILED(Result) then
    begin
      ClearEndpointDevices(endpointdevices);
      devicesCount := 0;
    end;
  end;
end;

function GetDeviceStateAsString(state: DWORD): string;
begin
  case state of
    DEVICE_STATE_ACTIVE: Result := DEV_STATE_ACTIVE;
    DEVICE_STATE_DISABLED: Result := DEV_STATE_DISABLED;
    DEVICE_STATE_NOTPRESENT: Result := DEV_STATE_NOTPRESENT;
    DEVICE_STATE_UNPLUGGED: Result := DEV_STATE_UNPLUGGED;
    DEVICE_STATEMASK_ALL: Result := DEV_STATEMASK_ALL;
    else Result := 'Unknown';
  end;
end;

function MfGetEndpointFriendlyName(Device: IMMDevice): string;
var
  Name: WideString;
begin
  Result := '';
  if SUCCEEDED(GetDeviceDescriptions(Device,PKEY_Device_FriendlyName,Name)) then
    Result := string(Name);
end;

function MfDeviceIdToFriendlyName(const DeviceID: string): string;
var
  Device: IMMDevice;
begin
  Result := '';
  if SUCCEEDED(GetEndPointDeviceByID(PWideChar(DeviceID),Device)) then
    Result := MfGetEndpointFriendlyName(Device);
end;

procedure ClearEndpointList(List: TStrings);
var
  I: Integer;
begin
  if List = nil then
    Exit;
  if (List is TStringList) and TStringList(List).OwnsObjects then
  begin
    List.Clear;
    Exit;
  end;
  for I := 0 to List.Count-1 do
    if List.Objects[I] is TEndpointIdObject then
    begin
      List.Objects[I].Free;
      List.Objects[I] := nil;
    end;
  List.Clear;
end;

procedure MfEnumerateEndpoints(DataFlow: EDataFlow;
                               List: TStrings);
var
  Devices: TEndPointDeviceArray;
  Count: DWORD;
  I: Integer;
  Entry: TEndpointIdObject;
begin
  if List = nil then
    Exit;
  ClearEndpointList(List);
  if FAILED(GetEndpointDevices(DataFlow,DEVICE_STATE_ACTIVE,Devices,Count)) then
    Exit;
  try
    for I := 0 to High(Devices) do
    begin
      Entry := TEndpointIdObject.Create(string(Devices[I].pwszID));
      try
        List.AddObject({$IFDEF FPC}UTF8Encode(string(Devices[I].DeviceName)){$ELSE}
                       string(Devices[I].DeviceName){$ENDIF},Entry);
      except
        Entry.Free;
        raise;
      end;
    end;
  finally
    ClearEndpointDevices(Devices);
  end;
end;

function CreateDShowAudioRenderer(const role: ERole;
                                  ppAudioRenderer: PIBaseFilter): HRESULT;
var
  Device: IMMDevice;
  Params: DIRECTX_AUDIO_ACTIVATION_PARAMS;
  Value: PROPVARIANT;
begin
  if ppAudioRenderer = nil then
    Exit(E_POINTER);
  ppAudioRenderer^ := nil;
  Result := GetDefaultEndPointAudioDevice(Device,role,eRender);
  if FAILED(Result) then
    Exit;
  FillChar(Params,SizeOf(Params),0);
  Params.cbDirectXAudioActivationParams := SizeOf(Params);
  Result := CreateGUID(Params.guidAudioSession);
  if FAILED(Result) then
    Exit;
  Params.dwAudioStreamFlags := AUDCLNT_STREAMFLAGS_CROSSPROCESS;
  PropVariantInit(Value);
  Value.vt := VT_BLOB;
  Value.blob.cbSize := SizeOf(Params);
  Value.blob.pBlobData := @Params;
  // The blob borrows stack storage only for this synchronous Activate call.
  Result := Device.Activate(IID_IBaseFilter,CLSCTX_ALL,@Value,Pointer(ppAudioRenderer^));
end;

function GetDirectSoundGuid(flow: EDataFlow;
                            role: ERole;
                            out pDevGuid: TGUID): HRESULT;
var
  Device: IMMDevice;
  Text: WideString;
begin
  pDevGuid := GUID_NULL;
  Result := GetDefaultEndPointAudioDevice(Device,role,flow);
  if FAILED(Result) then
    Exit;
  Result := GetDeviceDescriptions(Device,PKEY_AudioEndpoint_GUID,Text);
  if SUCCEEDED(Result) then
    Result := CLSIDFromString(PWideChar(Text),pDevGuid);
end;

function GetWaveOutId(role: ERole;
                      out pWaveOutId: UINT): HRESULT;
var
  Device: IMMDevice;
  EndpointId: LPWSTR;
  Buffer: LPWSTR;
  Bytes: DWORD;
  DeviceIndex: UINT;
  DeviceCount: UINT;
  Status: MMRESULT;
begin
  pWaveOutId := High(UINT);
  EndpointId := nil;
  Buffer := nil;
  Result := GetDefaultEndPointAudioDevice(Device,role,eRender);
  if FAILED(Result) then
    Exit;
  Result := Device.GetId(EndpointId);
  if FAILED(Result) then
    Exit;
  try
    DeviceCount := waveOutGetNumDevs;
    if DeviceCount = 0 then
      Exit(E_UNEXPECTED);
    for DeviceIndex := 0 to DeviceCount-1 do
    begin
      Bytes := 0;
      Status := waveOutMessage(HWAVEOUT(NativeUInt(DeviceIndex)),DRV_QUERYFUNCTIONINSTANCEIDSIZE,
                               DWORD_PTR(@Bytes),0);
      if (Status <> MMSYSERR_NOERROR) or (Bytes < SizeOf(WideChar)) or
         (Bytes > 65536) or ((Bytes mod SizeOf(WideChar)) <> 0) then
        Continue;
      Buffer := CoTaskMemAlloc(Bytes);
      if Buffer = nil then
        Exit(E_OUTOFMEMORY);
      try
        FillChar(Buffer^,Bytes,0);
        Status := waveOutMessage(HWAVEOUT(NativeUInt(DeviceIndex)),DRV_QUERYFUNCTIONINSTANCEID,
                                 DWORD_PTR(Buffer),Bytes);
        if Status <> MMSYSERR_NOERROR then
          Continue;
        Buffer[(Bytes div SizeOf(WideChar))-1] := #0;
        if lstrcmpiW(Buffer,EndpointId) = 0 then
        begin
          pWaveOutId := DeviceIndex;
          Exit(S_OK);
        end;
      finally
        CoTaskMemFree(Buffer);
        Buffer := nil;
      end;
    end;
    Result := E_UNEXPECTED;
  finally
    CoTaskMemFree(EndpointId);
  end;
end;

function GetJackInfo(pDevice: IMMDevice;
                     out ppJackDesc: IKsJackDescription): HRESULT;
var
  Topology: IDeviceTopology;
  FromConnector: IConnector;
  ToConnector: IConnector;
  Part: IPart;
begin
  ppJackDesc := nil;
  if pDevice = nil then
    Exit(E_POINTER);
  Result := pDevice.Activate(IID_IDeviceTopology,CLSCTX_ALL,nil,Pointer(Topology));
  if FAILED(Result) then
    Exit;
  Result := Topology.GetConnector(0,FromConnector);
  if FAILED(Result) then
    Exit;
  Result := FromConnector.GetConnectedTo(ToConnector);
  if Result = HRESULT($80070003) then
    Result := E_NOINTERFACE;
  if FAILED(Result) then
    Exit;
  Result := ToConnector.QueryInterface(IID_IPart,Part);
  if SUCCEEDED(Result) then
    Result := Part.Activate(CLSCTX_INPROC_SERVER,IID_IKsJackDescription,ppJackDesc);
end;

function GetDeviceDataFlow(pDevice: IMMDevice;
                           out pDataFlow: EDataFlow): HRESULT;
var
  Endpoint: IMMEndpoint;
begin
  pDataFlow := eAll;
  if pDevice = nil then
    Exit(E_POINTER);
  Result := pDevice.QueryInterface(IID_IMMEndpoint,Endpoint);
  if SUCCEEDED(Result) then
    Result := Endpoint.GetDataFlow(pDataFlow);
end;

procedure ConfigureRawAudioStream(const AAudioClient: IAudioClient;
                                  const ACategory: AUDIO_STREAM_CATEGORY;
                                  const AContext: string);
var
  Client2: IAudioClient2;
  Props: AudioClientProperties;
  Status: HRESULT;
begin
  if AAudioClient = nil then
    Exit;
  if not Supports(AAudioClient,IAudioClient2,Client2) then
    Exit;
  FillChar(Props,SizeOf(Props),0);
  Props.cbSize := SizeOf(Props);
  Props.eCategory := ACategory;
  Props.Options := AUDCLNT_STREAMOPTIONS_RAW;
  Status := Client2.SetClientProperties(Props);
  if FAILED(Status) then
    OutputDebugStringW(PWideChar(AContext+': AUDCLNT_STREAMOPTIONS_RAW rejected, hr='+
                                 string(IntToHex(Cardinal(Status),8))));
end;

end.
