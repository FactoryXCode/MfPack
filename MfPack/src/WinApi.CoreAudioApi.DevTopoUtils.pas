// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Device Topology Utilities
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.DevTopoUtils.pas
// Kind: Pascal / Delphi unit
// Release date: 18-12-2015
// Language: ENU
//
// Revision Version: 4.1.0
// Description: DeviceTopology API Utils, provides helpers for the DeviceTopology API.
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
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: Msdn (Some parts)
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
unit WinApi.CoreAudioApi.DevTopoUtils;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
	WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Coml2Api,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {$ENDIF}
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
	WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.DeviceTopology,
  WinApi.CoreAudioApi.AudioClient {WASAPI},
  WinApi.CoreAudioApi.Functiondiscoverykeys_devpkey;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  PEndPoint = ^TEndPoint;
  {$NODEFINE TEndPoint}
  TEndPoint = record
   sName: LPWSTR;
   pwszID: LPWSTR;
   iID: integer;
  end;
  // Mfpack < ver 264
  FEndPoint = TEndPoint;
  PfEndPoint = ^TEndPoint;
  //
  {$NODEFINE TEndPointArray}
  TEndPointArray = array of TEndPoint;


  // The following code shows how to obtain the IDeviceTopology interface for
  // the input multiplexer device from the IMMDevice interface for the endpoint device
  // for the line input or microphone:

  // The GetHardwareDeviceTopology function in the previous code example performs the following
  // steps to obtain the IDeviceTopology interface for the input multiplexer device:
  //  1 Call the IMMDevice.Activate method to get the IDeviceTopology interface for the endpoint device.
  //  2 With the IDeviceTopology interface obtained in the preceding step,
  //    call the IDeviceTopology.GetConnector method to get the IConnector interface of the
  //    single connector (connector number 0) in the endpoint device.
  //  3 With the IConnector interface obtained in the preceding step,
  //    call the IConnector.GetConnectedTo method to get the IConnector interface of the
  //    connector in the input multiplexer device.
  //  4 Query the IConnector interface obtained in the preceding step for its IPart interface.
  //  5 With the IPart interface obtained in the preceding step,
  //    call the IPart.GetTopologyObject method to get the IDeviceTopology interface for the
  //    input multiplexer device.


  // The input argument to this function is a pointer to the
  // IMMDevice interface of an endpoint device. The function
  // outputs a pointer (counted reference) to the
  // IDeviceTopology interface of the adapter device that
  // connects to the endpoint device.
  //-----------------------------------------------------------
  function GetHardwareDeviceTopology(pEndptDev: IMMDevice;
                                     out ppDevTopo: IDeviceTopology): HResult;

  // Before the user can record from the microphone in the preceding diagram,
  // the client application must make certain that the multiplexer selects the microphone input.
  // The following code shows how a client can traverse the data path from the microphone until
  // it finds the multiplexer, which it then programs to select the microphone input:
  //
  // The input argument to this function is a pointer to the
  // IMMDevice interface for a capture endpoint device. The
  // function traverses the data path that extends from the
  // endpoint device to the system bus (for example, PCI)
  // or external bus (USB). If the function discovers a MUX
  // (input selector) in the path, it selects the MUX input
  // that connects to the stream from the endpoint device.
  //-----------------------------------------------------------
  // Returns E_UNEXPECTED if traversal exceeds 1024 links. Non-MUX E_NOINTERFACE is expected.
  function SelectCaptureDevice(pEndptDev: IMMDevice): HResult;

  //-----------------------------------------------------------
  // This function enumerates all active (plugged in) audio
  // rendering endpoint devices. It gets the friendly name
  // and endpoint ID (string and integer) of each endpoint device.
  //-----------------------------------------------------------
  // Returns owned CoTaskMem strings. Release them with ClearEndpointNames.
  // Clear an earlier result before reusing the output array.
  function GetEndpointNames(out aEp: TEndPointArray): HRESULT;

  procedure ClearEndpointNames(var aEp: TEndPointArray);


implementation

function GetHardwareDeviceTopology(pEndptDev: IMMDevice;
                                   out ppDevTopo: IDeviceTopology): HRESULT;
var
  EndpointTopology: IDeviceTopology;
  EndpointConnector: IConnector;
  HardwareConnector: IConnector;
  HardwarePart: IPart;
begin
  ppDevTopo := nil;
  if pEndptDev = nil then
    Exit(E_POINTER);
  Result := pEndptDev.Activate(IID_IDeviceTopology,CLSCTX_ALL,nil,Pointer(EndpointTopology));
  if FAILED(Result) then
    Exit;
  Result := EndpointTopology.GetConnector(0,EndpointConnector);
  if FAILED(Result) then
    Exit;
  Result := EndpointConnector.GetConnectedTo(HardwareConnector);
  if FAILED(Result) then
    Exit;
  Result := HardwareConnector.QueryInterface(IID_IPart,HardwarePart);
  if FAILED(Result) then
    Exit;
  Result := HardwarePart.GetTopologyObject(ppDevTopo);
end;

function SelectCaptureDevice(pEndptDev: IMMDevice): HRESULT;
var
  Topology: IDeviceTopology;
  FromConnector: IConnector;
  ToConnector: IConnector;
  PreviousPart: IPart;
  NextPart: IPart;
  Parts: IPartsList;
  Selector: IAudioInputSelector;
  Flow: DataFlow;
  Connected: BOOL;
  ConnectionType: ConnectorType;
  Kind: PartType;
  LocalId: UINT;
  Steps: Integer;
begin
  if pEndptDev = nil then
    Exit(E_POINTER);
  Result := pEndptDev.Activate(IID_IDeviceTopology,CLSCTX_ALL,nil,Pointer(Topology));
  if FAILED(Result) then
    Exit;
  Result := Topology.GetConnector(0,FromConnector);
  if FAILED(Result) then
    Exit;
  Result := FromConnector.GetDataFlow(Flow);
  if FAILED(Result) then
    Exit;
  if Flow = _Out then
    Exit(HRESULT(AUDCLNT_E_WRONG_ENDPOINT_TYPE));
  Steps := 0;
  while True do
  begin
    Inc(Steps);
    if Steps > 1024 then
      Exit(E_UNEXPECTED);
    Result := FromConnector.IsConnected(Connected);
    if FAILED(Result) then
      Exit;
    if not Connected then
    begin
      Result := FromConnector.GetType(ConnectionType);
      if FAILED(Result) then
        Exit;
      if ConnectionType <> Software_IO then
        Result := E_FAIL;
      Exit;
    end;
    Result := FromConnector.GetConnectedTo(ToConnector);
    if FAILED(Result) then
      Exit;
    FromConnector := nil;
    Result := ToConnector.QueryInterface(IID_IPart,PreviousPart);
    if FAILED(Result) then
      Exit;
    ToConnector := nil;
    while True do
    begin
      Inc(Steps);
      if Steps > 1024 then
        Exit(E_UNEXPECTED);
      Result := PreviousPart.EnumPartsOutgoing(Parts);
      if FAILED(Result) then
        Exit;
      Result := Parts.GetPart(0,NextPart);
      Parts := nil;
      if FAILED(Result) then
        Exit;
      Result := NextPart.GetPartType(Kind);
      if FAILED(Result) then
        Exit;
      if Kind = Connector then
      begin
        Result := NextPart.QueryInterface(IID_IConnector,FromConnector);
        if FAILED(Result) then
          Exit;
        PreviousPart := nil;
        NextPart := nil;
        Break;
      end;
      Result := NextPart.Activate(CLSCTX_ALL,IID_IAudioInputSelector,Selector);
      if SUCCEEDED(Result) then
      begin
        Result := PreviousPart.GetLocalId(LocalId);
        if FAILED(Result) then
          Exit;
        Result := Selector.SetSelection(LocalId,GUID_NULL);
        Selector := nil;
        if FAILED(Result) then
          Exit;
      end
      else if Result <> E_NOINTERFACE then
        Exit;
      PreviousPart := NextPart;
      NextPart := nil;
    end;
  end;
end;

procedure ClearEndpointNames(var aEp: TEndPointArray);
var
  I: Integer;
begin
  for I := 0 to High(aEp) do
  begin
    CoTaskMemFree(aEp[I].sName);
    CoTaskMemFree(aEp[I].pwszID);
    aEp[I].sName := nil;
    aEp[I].pwszID := nil;
  end;
  aEp := nil;
end;

function GetEndpointNames(out aEp: TEndPointArray): HRESULT;
var
  Enumerator: IMMDeviceEnumerator;
  Collection: IMMDeviceCollection;
  Endpoint: IMMDevice;
  Props: IPropertyStore;
  Name: PROPVARIANT;
  Id: LPWSTR;
  Count: UINT;
  I: UINT;
  Bytes: SIZE_T;
begin
  aEp := nil;
  Id := nil;
  PropVariantInit(Name);
  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,nil,CLSCTX_ALL,
                             IID_IMMDeviceEnumerator,Enumerator);
  if FAILED(Result) then
    Exit;
  Result := Enumerator.EnumAudioEndpoints(eRender,DEVICE_STATE_ACTIVE,Collection);
  if FAILED(Result) then
    Exit;
  Result := Collection.GetCount(Count);
  if FAILED(Result) or (Count = 0) then
    Exit;
  SetLength(aEp,Count);
  try
    for I := 0 to Count-1 do
    begin
      Result := Collection.Item(I,Endpoint);
      if FAILED(Result) then
        Exit;
      Result := Endpoint.GetId(Id);
      if FAILED(Result) then
        Exit;
      Result := Endpoint.OpenPropertyStore(STGM_READ,Props);
      if FAILED(Result) then
        Exit;
      Result := Props.GetValue(PKEY_Device_FriendlyName,Name);
      if FAILED(Result) then
        Exit;
      if (Name.vt <> VT_LPWSTR) or (Name.pwszVal = nil) or (Id = nil) then
      begin
        Result := E_UNEXPECTED;
        Exit;
      end;
      Bytes := (SIZE_T(StrLen(Name.pwszVal))+1)*SizeOf(WideChar);
      aEp[I].sName := CoTaskMemAlloc(Bytes);
      if aEp[I].sName = nil then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;
      Move(Name.pwszVal^,aEp[I].sName^,Bytes);
      aEp[I].pwszID := Id;
      Id := nil;
      aEp[I].iID := I;
      PropVariantClear(Name);
      Props := nil;
      Endpoint := nil;
    end;
  finally
    CoTaskMemFree(Id);
    PropVariantClear(Name);
    if FAILED(Result) then
      ClearEndpointNames(aEp);
  end;
end;

end.
