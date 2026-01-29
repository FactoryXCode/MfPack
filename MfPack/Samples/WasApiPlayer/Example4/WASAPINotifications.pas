// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WASAPINotifications.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: PCM converter unit.
//
// Notes:
//  - Designed for real-time use in the WASAPI render thread.
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example4
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit WASAPINotifications;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.CoreAudioApi.MMDeviceApi;  // IMMNotificationClient, EDataFlow, ERole

type

  // We implement this in our engine to receive "Device Changed" notifications.
  IWasApiDeviceNotifySink = interface
  ['{C6B4A1A2-6D15-4A2B-9CC0-2F0D27B1B2D1}']

    procedure OnWasApiDefaultDeviceChanged(Flow: EDataFlow;
                                           Role: ERole;
                                           const DeviceId: UnicodeString);

    procedure OnWasApiDeviceStateChanged(const DeviceId: UnicodeString;
                                         NewState: DWORD);

    procedure OnWasApiDeviceRemoved(const DeviceId: UnicodeString);
  end;


  // COM callback object
  TWasApiNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private

    FSink: IWasApiDeviceNotifySink;
  public

    constructor Create(const ASink: IWasApiDeviceNotifySink);

    // IMMNotificationClient
    function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                  dwNewState: DWORD): HRESULT; stdcall;

    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;

    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;

    function OnDefaultDeviceChanged(flow: EDataFlow;
                                    role: ERole;
                                    pwstrDefaultDeviceId:
                                    LPCWSTR): HRESULT; stdcall;

    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR; const
                                    key: PROPERTYKEY): HRESULT; stdcall;
  end;


implementation


function WStrToUStr(P: LPCWSTR): UnicodeString;
begin

  if (P <> nil) then
    Result := P
  else
    Result := '';
end;


constructor TWasApiNotificationClient.Create(const ASink: IWasApiDeviceNotifySink);
begin

  inherited Create;

  FSink := ASink;
end;


function TWasApiNotificationClient.OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                                        dwNewState: DWORD): HRESULT;
var
  Id: UnicodeString;

begin

  Id := WStrToUStr(pwstrDeviceId);
  if (FSink <> nil) then
    FSink.OnWasApiDeviceStateChanged(Id,
                                     dwNewState);
  Result := S_OK;
end;


function TWasApiNotificationClient.OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT;
begin

  // Optional
  Result := S_OK;
end;


function TWasApiNotificationClient.OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT;
var
  Id: UnicodeString;

begin

  Id := WStrToUStr(pwstrDeviceId);
  if (FSink <> nil) then
    FSink.OnWasApiDeviceRemoved(Id);
  Result := S_OK;
end;


function TWasApiNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow;
                                                          role: ERole;
                                                          pwstrDefaultDeviceId: LPCWSTR): HRESULT;
var
  Id: UnicodeString;
begin
  Id := WStrToUStr(pwstrDefaultDeviceId);
  if FSink <> nil then
    FSink.OnWasApiDefaultDeviceChanged(flow, role, Id);
  Result := S_OK;
end;


function TWasApiNotificationClient.OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                                          const key: PROPERTYKEY): HRESULT;
begin

  // Optional
  Result := S_OK;
end;

end.

