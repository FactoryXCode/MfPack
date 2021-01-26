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
// Revision Version: 3.0.1
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
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
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
// 
//==============================================================================
unit WinApi.CoreAudioApi.DevTopoUtils;

interface

uses
  {WinApi}
	WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Coml2Api,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
	WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.DeviceTopology,
  WinApi.CoreAudioApi.AudioClient {WASAPI},
  WinApi.CoreAudioApi.Functiondiscoverykeys_devpkey;

  {$WEAKPACKAGEUNIT ON}
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
  {$NODEFINE EndPointArray}
  EndPointArray = array [0..65535] of TEndPoint;


  // The following code shows how to obtain the IDeviceTopology interface for
  // the input multiplexer device from the IMMDevice interface for the endpoint device
  // for the line input or microphone:

  // The GetHardwareDeviceTopology function in the previous code example performs the following
  // steps to obtain the IDeviceTopology interface for the input multiplexer device:
  //  1 Call the IMMDevice::Activate method to get the IDeviceTopology interface for the endpoint device.
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
  function SelectCaptureDevice(pEndptDev: IMMDevice): HResult;

  //-----------------------------------------------------------
  // This function enumerates all active (plugged in) audio
  // rendering endpoint devices. It gets the friendly name
  // and endpoint ID (string and integer) of each endpoint device.
  //-----------------------------------------------------------
  function GetEndpointNames(var aEp: EndPointArray): HRESULT;


implementation


function GetHardwareDeviceTopology(pEndptDev: IMMDevice;
                                   out ppDevTopo: IDeviceTopology): HResult;
var
  hr: HResult;
  pDevTopoEndpt: IDeviceTopology;
  pConnEndpt: IConnector;
  pConnHWDev: IConnector;
  pPartConn: IPart;

begin
  // Initialize vars
  hr:= S_OK;

try
try
  // Get the endpoint device's IDeviceTopology interface.
  hr:= pEndptDev.Activate(IID_IDeviceTopology,
                          UINT(CLSCTX_ALL),
                          Nil,
                          Pointer(pDevTopoEndpt));
  if Failed(hr) then
    Abort;

  // The device topology for an endpoint device always
  // contains just one connector (connector number 0).

  hr:= pDevTopoEndpt.GetConnector(0,
                                  pConnEndpt);
  if Failed(hr) then
    Abort;

  // Use the connector in the endpoint device to get the
  // connector in the adapter device.
  hr:= pConnEndpt.GetConnectedTo(pConnHWDev);
  if Failed(hr) then
    Abort;

  // Query the connector in the adapter device for
  // its IPart interface.
  hr:= pConnHWDev.QueryInterface(IID_IPart,
                                 pPartConn);
  if Failed(hr) then
    Abort;

  // Use the connector's IPart interface to get the
  // IDeviceTopology interface for the adapter device.
  hr:= pPartConn.GetTopologyObject(ppDevTopo);
  if Failed(hr) then
    Abort;

except
  //do nothing, or something if you need to...
end;
finally
  // When going out of scope, the interface objects will be automatically eliminated
  Result:= hr;
end;
end;


function SelectCaptureDevice(pEndptDev: IMMDevice): HResult;
{$WARNINGS OFF}
var
  hr: HRESULT;
  bConnected: BOOL;
  flow: DataFlow;
  pDeviceTopology: IDeviceTopology;
  pConnFrom: IConnector;
  pConnTo: IConnector;
  pPartPrev: IPart;
  pPartNext: IPart;
  pSelector: IAudioInputSelector;
  connType: ConnectorType;
  _parttype: PartType;
  localId: UINT;
  pParts: IPartsList;

begin
  //intialize
  bConnected := True;
  hr := E_FAIL;

try
try

  if (pEndptDev = NIL) then
    begin
      hr := E_POINTER;
      Abort;
    end;

  // Get the endpoint device's IDeviceTopology interface.
  hr := pEndptDev.Activate(IID_IDeviceTopology,
                           CLSCTX_ALL,
                           Nil,
                           Pointer(pDeviceTopology));
  if Failed(hr) then
    Abort;

  // The device topology for an endpoint device always
  // contains just one connector (connector number 0).
  hr := pDeviceTopology.GetConnector(0,
                                     pConnFrom);
  SafeRelease(pDeviceTopology);
  if Failed(hr) then
    Abort;

  // Make sure that this is a capture device.
  hr := pConnFrom.GetDataFlow(flow);
  if Failed(hr) then
    Abort;

  if (flow = _Out) then
    begin
      // Error -- this is a rendering device.
      hr := HRESULT(AUDCLNT_E_WRONG_ENDPOINT_TYPE);
    end;

  // Outer loop: Each iteration traverses the data path
  // through a device topology starting at the input
  // connector and ending at the output connector.
  while True do
    begin
      hr := pConnFrom.IsConnected(bConnected);
      if Failed(hr) then
        Abort;

      // Does this connector connect to another device?
      if (bConnected = False) then
        begin
          // This is the end of the data path that
          // stretches from the endpoint device to the
          // system bus or external bus. Verify that
          // the connection type is Software_IO.
          hr := pConnFrom.GetType(connType);
          if Failed(hr) then
            Abort;

          if (connType = Software_IO) then
            break  // finished
          else
            begin
              hr := E_FAIL;
              Abort;
            end;
        end;

      // Get the connector in the next device topology,
      // which lies on the other side of the connection.
      hr := pConnFrom.GetConnectedTo(pConnTo);
      if Failed(hr) then
        Abort;

      SafeRelease(pConnFrom);

      // Get the connector's IPart interface.
      hr := pConnTo.QueryInterface(IID_IPart,
                                   pPartPrev);
      if Failed(hr) then
        Abort;
      SafeRelease(pConnTo);

      // Inner loop: Each iteration traverses one link in a
      // device topology and looks for input multiplexers.
      while True do
        begin
          // Follow downstream link to next part.
          hr := pPartPrev.EnumPartsOutgoing(pParts);
          if Failed(hr) then
            Abort;

          hr := pParts.GetPart(0, pPartNext);
            SafeRelease(pParts);
          if Failed(hr) then
            Abort;

          hr := pPartNext.GetPartType(_parttype);
          if Failed(hr) then
            Abort;

          if (_parttype = Connector) then
            begin
              // We've reached the output connector that
              // lies at the end of this device topology.
              hr := pPartNext.QueryInterface(IID_IConnector,
                                             pConnFrom);
              if Failed(hr) then
                Abort;

              SafeRelease(pPartPrev);
              SafeRelease(pPartNext);
              break;
            end;

          // Failure of the following call means only that
          // the part is not a MUX (input selector).
          hr := pPartNext.Activate(CLSCTX_ALL,
                                   IID_IAudioInputSelector,
                                   pSelector);
          if (hr = S_OK) then
            begin
              // We found a MUX (input selector), so select
              // the input from our endpoint device.
              hr := pPartPrev.GetLocalId(localId);
              if Failed(hr) then
                Abort;

              hr := pSelector.SetSelection(localId,
                                           GUID_NULL);
              if Failed(hr) then
                Abort;

              SafeRelease(pSelector);

            end;

        SafeRelease(pSelector);
        pPartPrev := pPartNext;
        pPartNext := Nil;

      end;  // Inner loop

    end; // Outer loop



except
  // Do nothing, or something....
end;
finally
  Result:= hr;
{$WARNINGS ON}
end;
end;


function GetEndpointNames(var aEp: EndPointArray): HRESULT;
var
  hr: HResult;
  pEnumerator: IMMDeviceEnumerator;
  pCollection: IMMDeviceCollection;
  pEndpoint: IMMDevice;
  pProps: IPropertyStore;
  varName: PROPVARIANT;
  pwszID: LPWSTR;
  count: UINT;
  i: ULONG;

label
  leave;

begin

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    goto leave;

  hr := pEnumerator.EnumAudioEndpoints(eRender,
                                       DEVICE_STATE_ACTIVE,
                                       pCollection);
  if FAILED(hr) then
    goto leave;

  hr:= pCollection.GetCount(count);
  if FAILED(hr) then
    goto leave;

  // No endpoints found.
  if (count = 0) then
    goto leave;

  // Each loop prints the name of an endpoint device.
  for i := 0 to count -1 do
    begin
        // Get pointer to endpoint number i.
        hr := pCollection.Item(i,
                               pEndpoint);
        if FAILED(hr) then
          goto leave;

        // Get the endpoint ID string.
        hr := pEndpoint.GetId(pwszID);
        if FAILED(hr) then
          goto leave;

        hr := pEndpoint.OpenPropertyStore(STGM_READ,
                                          pProps);
        if FAILED(hr) then
          goto leave;

        // Initialize container for property value.
        PropVariantInit(varName);

        // Get the endpoint's friendly-name property.
        hr := pProps.GetValue(PKEY_Device_FriendlyName,
                              varName);
        if FAILED(hr) then
          goto leave;

        // expand the array (in case we an open array)
        //SetLength(aEp,
        //          i);

        // Set endpoint friendly name and endpoint ID.
        aEp[i].sName := varName.pwszVal;
        aEp[i].pwszID := pwszID;
        aEp[i].iID := i;

        CoTaskMemFree(pwszID);
        pwszID := Nil;

        PropVariantClear(varName);
        SafeRelease(pProps);
        SafeRelease(pEndpoint);
    end;


leave: // Error
    Result:= hr;
    CoTaskMemFree(pwszID);
end;


end.
