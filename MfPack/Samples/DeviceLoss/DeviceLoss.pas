// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: DeviceLoss.pas
// Kind: Pascal / Delphi unit
// Release date: 26-02-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This sample describes how to detect device loss when using a
//              video capture device.
//              It contains the following steps:
//
//               1 Register For Device Notification.
//               2 Get the Symbolic Link of the Device.
//               3 implement a messagehandler to handle WM_DEVICECHANGE.
//               4 Unregister for notification when you close your app.
//
//  Create FDeviceLoss object example:
//    FDeviceLoss := TDeviceLoss.Create(Handle, DeviceEnumIndex, dmo_Message);
//    Handle = Application Window handle
//    DeviceEnumIndex = The device index that has been picked.
//    dmo_Message = When a device is lost, FDeviceLoss sends a message to the Application Window.
//  Note:
//    The Application Window must have a message handler to intercept the WM_DEVICELOST message when
//    dmo_Message or dmo_Both are set.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
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
// Source: FactoryX/project samples and Microsoft Corporation.
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
unit DeviceLoss;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.Ks,
  WinApi.KsMedia,
  WinApi.WinError,
  {System}
  System.Services.Dbt,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfIdl;

const
  WM_DEVICELOST = WM_USER + 1001; // You can give a number from 1 to 31743

type
  TDeviceLossMessageOptions = (dmo_MessageBox,
                               dmo_Message,
                               dmo_Both);

  TDeviceLoss = class(TObject)
  private
    p_HWND: HWND;
    p_hdevnotify: HDEVNOTIFY;
    p_hwndSource: HWND;
    p_ActiveDeviceIndex: Integer;
    p_pwszSymbolicLink: PWideChar;
    p_pwszFriendlyName: PWideChar;
    p_DeviceLossMessageOptions: TDeviceLossMessageOptions;

  public
    p_dpa: TDevicePropertiesArray;

    constructor Create(hwndAppWindow: HWND;
                       aDeviceIndex: Integer;
                       msgOptions: TDeviceLossMessageOptions); reintroduce;
    destructor Destroy(); override;

    // Before you start capturing from a device, call the RegisterDeviceNotification function to
    // register for device notifications.
    function RegisterForDeviceNotification(): Boolean;

    // Enumerate the video devices on the system.
    function EnumDevices(): Integer;

    procedure CloseDeviceNotify();
    function CheckDeviceLost(pHdr: PDEV_BROADCAST_HDR;
                             out pbDeviceLost: Boolean): HResult;

    procedure OnDeviceChange(var aMessage: TMessage); message WM_DEVICECHANGE;
  end;

var
  FDeviceLoss: TDeviceLoss;


implementation

uses
  System.SysUtils;


constructor TDeviceLoss.Create(hwndAppWindow: HWND;
                               aDeviceIndex: Integer;
                               msgOptions: TDeviceLossMessageOptions);
begin
  inherited Create();
  p_HWND := AllocateHWnd(OnDeviceChange);
  p_hwndSource := hwndAppWindow;
  p_DeviceLossMessageOptions := msgOptions;
  if not RegisterForDeviceNotification() then
    Self.Free;
  EnumDevices();
end;


destructor TDeviceLoss.Destroy();
begin
  DeallocateHWnd(p_HWND);
  if Assigned(p_hdevnotify) then
    CloseDeviceNotify();
  inherited Destroy();
end;


function TDeviceLoss.RegisterForDeviceNotification(): Boolean;
var
  devbroadcastdevice: DEV_BROADCAST_DEVICEINTERFACE;
  iSize: Integer;
begin

  if (p_HWND > 0) then
    begin
      iSize := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
      ZeroMemory(@devbroadcastdevice,
                 iSize);

      devbroadcastdevice.dbcc_size := iSize;
      devbroadcastdevice.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
      devbroadcastdevice.dbcc_reserved := 0;
      devbroadcastdevice.dbcc_classguid := KSCATEGORY_VIDEO_CAMERA; // KSCATEGORY_CAPTURE : Since windows 10 you should not use this guid to register for device loss! Otherwise it will return a wrong symoliclink when detecting a device lost.
      devbroadcastdevice.dbcc_name := #0;

      p_hdevnotify := RegisterDeviceNotification(p_HWND,
                                                 @devbroadcastdevice,
                                                 DEVICE_NOTIFY_WINDOW_HANDLE);
    end;
  Result := Assigned(p_hdevnotify);
end;


procedure TDeviceLoss.CloseDeviceNotify();
begin
  if Assigned(p_hdevnotify) then
    UnregisterDeviceNotification(p_hdevnotify);
end;


function TDeviceLoss.EnumDevices(): Integer;
var
  hr: HResult;

begin
  hr := EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                                 p_dpa);
  if SUCCEEDED(hr) then
    Result := Length(p_dpa)
  else
    Result := -1;
end;


procedure TDeviceLoss.OnDeviceChange(var aMessage: TMessage);
var
  hr: HResult;
  bDeviceLost: Boolean;

begin

  if not Assigned(p_hdevnotify) then
    Exit;

  bDeviceLost := False;

  if (AMessage.LParam <> 0) then
    begin
      hr := CheckDeviceLost(PDEV_BROADCAST_HDR(Pointer(AMessage.LParam)),
                            bDeviceLost);

      if (FAILED(hr) or bDeviceLost) then
        begin
          if (p_DeviceLossMessageOptions = dmo_Message) or (p_DeviceLossMessageOptions = dmo_Both) then
            begin
              // Send a WM_DEVICELOST message to the application window
              SendMessage(p_hwndSource,
                          WM_DEVICELOST,
                          WPARAM(Pointer(p_pwszFriendlyName)),
                          LPARAM(p_ActiveDeviceIndex));
            end;

          if (p_DeviceLossMessageOptions = dmo_MessageBox) or (p_DeviceLossMessageOptions = dmo_Both) then
            begin
              // Show a MessageBoxDialog from the application window
              MessageBox(p_hwndSource,
                         LPCWSTR(Format('Lost capture device: %s', [p_pwszFriendlyName])),
                         LPCWSTR('Capture device'),
                         MB_OK);
            end;
        end;
    end;
end;


function TDeviceLoss.CheckDeviceLost(pHdr: PDEV_BROADCAST_HDR;
                                     out pbDeviceLost: Boolean): HResult;
var
  hr: HRESULT;
  pDi: PDEV_BROADCAST_DEVICEINTERFACE;
  pDeviceName: PWideChar;
  pSymbolicLink: PWideChar;

label
  done;

begin
  hr := S_OK;
  pbDeviceLost := False;

  if (pHdr = nil) then
    begin
      hr := S_OK;
      goto done;
    end;


  if (pHdr.dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE) then
    begin
      hr := S_OK;
      goto done;
    end;

  // Unregister existing device
  UnregisterForDeviceNotification(p_hdevnotify);
  p_hdevnotify := nil;

  pDi := PDEV_BROADCAST_DEVICEINTERFACE(pHdr);
  // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
  // Dereference the struct's field dbcc_name (array [0..0] of WideChar) for a readable string.
  pDeviceName := PChar(@pDi^.dbcc_name);

  // Compare the device name with the symbolic link.
  if Assigned(p_pwszSymbolicLink) then
    begin
      pSymbolicLink := p_pwszSymbolicLink;
      // Perform a lowercase comparison of strings.
      if (StrIComp(pSymbolicLink,
                   pDeviceName) = 0) then
        begin
          pbDeviceLost := True;
          hr := ERROR_DEVICE_REMOVED;
        end;
    end
  else // Unknown device
    begin
      pbDeviceLost := True;
      hr := E_POINTER;
    end;

done:
  Result := hr;
end;

end.
