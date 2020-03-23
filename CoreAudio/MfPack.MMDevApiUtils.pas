// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - CoreAudio - Utilities
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.MMDevApiUtils.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 2.6.4
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
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
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
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: https://docs.microsoft.com/en-us/windows/win32/coreaudio/device-roles-for-directsound-applications
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
unit MfPack.MMDevApiUtils;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.MMSystem,
  {System}
  System.Classes,
  System.SysUtils,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.StrMif,
  MfPack.PropIdl,
  MfPack.PropSys,
  MfPack.ObjBase,
  MfPack.Coml2Api,
  MfPack.ComBaseApi,
  MfPack.Mmreg,
  MfPack.MMDeviceApi,
  MfPack.AudioPolicy,
  MfPack.AudioClient,
  {MfPack WASAPI}
  MfPack.Audiosessiontypes,
  MfPack.MmDdk,
  MfPack.DeviceTopology;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}

const
  STRSAFE_MAX_CCH = 2147483647;  // Maximum supported buffer size, in characters (same as INT_MAX)

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
                       pAudioEvents: IAudioSessionEvents);

    destructor Destroy; override;

    function GetStatus(): HRESULT;  { return _hrStatus; }

  end;

  // Get endpointnames
  function GetEndpointNames(out slEndPoints: TStrings;
                            DataFlow: eDataFlow;
                            DeviceState: OLE_ENUM): HRESULT;

  function GetDefaultComDeviceEndPoint: HRESULT;

  // Get the devicename of a device
  function GetImmDeviceName(Device: IMMDevice;
                            out deviceName: string): HRESULT;

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
                                    var ppAudioRenderer: IBaseFilter): HRESULT;


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


implementation

// Constructor
constructor TAudioVolumeEvents.Create(flow: EDataFlow;
                                      role: ERole; 
                                      pAudioEvents: IAudioSessionEvents);
var
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

begin
  inherited Create;

  hrStatus := S_OK;
  pManager := Nil;
  pControl := Nil;
  pAudioEvents := pAudioEvents;

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
                               pManager);
  if Failed(hrStatus) then
    Abort;


  // Get the control interface for the process-specific audio
  // session with session GUID = GUID_NULL. This is the session
  // that an audio stream for a DirectSound, DirectShow, waveOut,
  // or PlaySound application stream belongs to by default.
  hrStatus := pManager.GetAudioSessionControl(GUID_NULL,
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
destructor TAudioVolumeEvents.Destroy;
begin
  if Assigned(pControl) then
    begin
      pControl.UnregisterAudioSessionNotification(pAudioEvents);
    end;
  SafeRelease(pManager);
  SafeRelease(pControl);
  SafeRelease(pAudioEvents);
  inherited Destroy;
end;


function TAudioVolumeEvents.GetStatus(): HRESULT;  { return _hrStatus; }
begin
  Result := hrStatus;
end;

// eof AudioVolumeEvents


// Get endpointnames
// Source: Microsoft, http://msdn.microsoft.com/en-us/library/windows/desktop/dd370812(v=vs.85).aspx
// Remarks: the index of slEndPoints indicates also the device index.
function GetEndpointNames(out slEndPoints: TStrings;
                          DataFlow: eDataFlow;
                          DeviceState: OLE_ENUM): HRESULT;
var
  hr: HRESULT;
  Enumerator: IMMDeviceEnumerator;
  Collection: IMMDeviceCollection;
  Endpoint: IMMDevice;
  Props: IPropertyStore;
  wszID: LPWSTR;
  dwCount: UINT;
  i: integer;
  varName: MfPROPVARIANT;

begin
  slEndPoints.Clear;
  hr:= S_OK;

try
try

  hr:= CoCreateInstance(CLSID_MMDeviceEnumerator,
                        nil,
                        CLSCTX_ALL,
                        IID_IMMDeviceEnumerator,
                        Enumerator);
  if Failed(hr) then
    Abort;

  hr:= Enumerator.EnumAudioEndpoints(DataFlow,
                                     DeviceState,
                                     Collection);
  if Failed(hr) then
    Abort;

  hr := Collection.GetCount(dwCount);
  if Failed(hr) then
    Abort;

  if (dwCount = 0) then
    Abort;

  // Each loop prints the name of an endpoint device.
  for i := 0 to dwCount -1 do
    begin
      // Get pointer to endpoint number _i
      hr := Collection.Item(i,
                            Endpoint);
      if Failed(hr) then
        Abort;

      // Get the endpoint ID string.
      hr := Endpoint.GetId(wszID);
      if Failed(hr) then
        Abort;

      hr := Endpoint.OpenPropertyStore(Ord(STGM_READ),
                                       Props);
      if Failed(hr) then
        Abort;

      // Initialize container for property value.
      PropVariantInit(varName);

      // Get the endpoint's friendly-name property.
      hr:= Props.GetValue(PKEY_Device_FriendlyName,
                          varName);
      if Failed(hr) then
        Abort;

      // endpoint friendly name and endpoint ID.
      slEndPoints.Append(string(varName.pwszVal));

      PropVariantClear(varName);
    end;

except
  //Do Nothing
end;
finally
  CoTaskMemFree(wszID);
  Result := hr;
end;
end;



// Get a reference to the endpoint of the default communication device for
// rendering an audio stream.
function GetDefaultComDeviceEndPoint(): HRESULT;
var
  pdeviceEnumerator: IMMDeviceEnumerator;
  DefaultAudioEndpoint: IMMDevice;
  hr: HRESULT;

begin
  hr := E_FAIL;  //to prevent might not have been initialised warning
try
try
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                          nil,
                          CLSCTX_ALL,
                          IID_IMMDeviceEnumerator,
                          pdeviceEnumerator);
  if Failed(hr) then
    Abort;

  hr := pdeviceEnumerator.GetDefaultAudioEndpoint(eRender,
                                                  eCommunications,
                                                  DefaultAudioEndpoint);
  if Failed(hr) then
    Abort;

except
  //Do Nothing
end;
finally
  Result := hr;
end;
end;


// Get the devicename of a device
function GetImmDeviceName(Device: IMMDevice;
                          out deviceName: string): HRESULT;
var
  PropStore: IPropertyStore;
  pvar: MfPROPVARIANT;
  hr: HRESULT;

begin
  deviceName := '';
  hr := E_FAIL;  //to prevent might not have been initialised warning
try
try
  hr := Device.OpenPropertyStore(STGM_READ,
                                 PropStore);
  if SUCCEEDED(hr) then
    begin
      // Get the DirectSound or DirectSoundCapture device GUID
      // (in WCHAR string format) for the endpoint device.
      hr := PropStore.GetValue(PKEY_Device_FriendlyName,
                               pvar);
      if Failed(hr) then
        Abort
      else
        deviceName:= string(pvar.pwszVal);
    end;
except
  //do nothing
end;
finally
  Result := hr;
end;
end;

//-----------------------------------------------------------
// Create a DirectShow audio rendering filter that
// encapsulates the audio endpoint device that is currently
// assigned to the specified device role.
// Source: http://msdn.microsoft.com/en-us/library/windows/desktop/dd370815
//-----------------------------------------------------------
function CreateDShowAudioRenderer(const role: eRole;
                                  var ppAudioRenderer: IBaseFilter): HRESULT;
const
  // This application's audio session GUID
  guidAudioSessionId: TGUID = '{b13ff52e-a5cf-4fca-9fc3-42265b0b14fb}';

var
  hr: HRESULT;
  daap: PDIRECTX_AUDIO_ACTIVATION_PARAMS;
  pVar: MfPROPVARIANT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

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
  hr:= CoCreateInstance(CLSID_MMDeviceEnumerator,
                        nil, 
                        CLSCTX_INPROC_SERVER,
                        IID_IMMDeviceEnumerator,
                        IUnknown(pEnumerator));
  if Failed(hr) then
    Abort;

  hr:= pEnumerator.GetDefaultAudioEndpoint(eRender,
                                           role,
                                           pDevice);

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
                         ppAudioRenderer);
  if Failed(hr) then
    Abort;
except
  //do nothing
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
  pVar: MfPROPVARIANT;

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
  //do nothing
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

  pEnumerator:= Nil;
  pDevice:= Nil;
  pstrEndpointIdKey:= Nil;
  pstrEndpointId:= Nil;
  cbEndpointId:= 0;

  if (pWaveOutId = Nil) then
    begin
      Result:= E_POINTER;
      Exit;
    end;

  // Create an audio endpoint device enumerator.
  hr:= CoCreateInstance(IID_IMMDeviceEnumerator,
                        Nil,
                        CLSCTX_INPROC_SERVER,
                        IID_IMMDeviceEnumerator,
                        pEnumerator);
  if Failed(hr) then
    goto leave;

  // Get the audio endpoint device that the user has
  // assigned to the specified device role.
  hr:= pEnumerator.GetDefaultAudioEndpoint(eRender,
                                           role,
                                           pDevice);
  if Failed(hr) then
    goto leave;

  // Get the endpoint ID string of the audio endpoint device.
  hr:= pDevice.GetId(pstrEndpointIdKey);
  if Failed(hr) then
    goto leave;

  // Get the size of the endpoint ID string.
  //hr:= StringCbLength(pstrEndpointIdKey,
  //                    (STRSAFE_MAX_CCH * sizeof(WCHAR)),
  //                    cbEndpointIdKey);

  //  EXIT_ON_ERROR(hr)
  cbEndpointIdKey := (Length(pstrEndpointIdKey) * SizeOf(WCHAR));

  // Include terminating null in string size.
  cbEndpointIdKey := cbEndpointIdKey + sizeof(WCHAR);

  // Allocate a buffer for a second string of the same size.
  pstrEndpointId := CoTaskMemAlloc(cbEndpointIdKey);
  if (pstrEndpointId = Nil) then
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


end.
