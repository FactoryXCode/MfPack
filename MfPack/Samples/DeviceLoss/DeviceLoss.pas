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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
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
  WinApi.ComBaseApi,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
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
    procedure ClearDevices;
  private
    FSymbolicLink: UnicodeString;
    FFriendlyName: UnicodeString;
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


{$POINTERMATH ON}
implementation

uses
  System.SysUtils;


constructor TDeviceLoss.Create(hwndAppWindow: HWND;aDeviceIndex: Integer;msgOptions: TDeviceLossMessageOptions);
begin
 inherited Create;
 p_ActiveDeviceIndex:=aDeviceIndex;p_hwndSource:=hwndAppWindow;p_DeviceLossMessageOptions:=msgOptions;
 if (aDeviceIndex<0) or (EnumDevices<=aDeviceIndex) then raise Exception.Create('Invalid capture device index');
 FSymbolicLink:=UnicodeString(p_dpa[aDeviceIndex].lpSymbolicLink);
 FFriendlyName:=UnicodeString(p_dpa[aDeviceIndex].lpFriendlyName);
 p_pwszSymbolicLink:=PWideChar(FSymbolicLink);p_pwszFriendlyName:=PWideChar(FFriendlyName);
 p_HWND:=AllocateHWnd(OnDeviceChange);
 if not RegisterForDeviceNotification then RaiseLastOSError;
end;


destructor TDeviceLoss.Destroy();
begin
 CloseDeviceNotify;
 if p_HWND<>0 then DeallocateHWnd(p_HWND);
 ClearDevices;
 inherited;
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
  Result := (NativeUInt(p_hdevnotify)<>0);
end;


procedure TDeviceLoss.CloseDeviceNotify();
begin
 if NativeUInt(p_hdevnotify)<>0 then begin
  UnregisterDeviceNotification(p_hdevnotify);
  FillChar(p_hdevnotify,SizeOf(p_hdevnotify),0);
 end;
end;


procedure TDeviceLoss.ClearDevices;
var I: Integer;
begin
 for I:=0 to Length(p_dpa)-1 do begin
  CoTaskMemFree(p_dpa[I].lpFriendlyName);CoTaskMemFree(p_dpa[I].lpSymbolicLink);
 end;
 SetLength(p_dpa,0);
end;
function TDeviceLoss.EnumDevices(): Integer;
var Attributes: IMFAttributes; Devices: PIMFActivate; Count,N: UINT32; I: Integer; HR: HResult;
begin
 ClearDevices;Devices:=nil;Count:=0;
 HR:=MFCreateAttributes(Attributes,1);
 if SUCCEEDED(HR) then HR:=Attributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
 if SUCCEEDED(HR) then HR:=MFEnumDeviceSources(Attributes,Devices,Count);
 if FAILED(HR) then Exit(-1);
 try
  SetLength(p_dpa,Count);
  for I:=0 to Integer(Count)-1 do begin
   p_dpa[I].riId:=MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID;p_dpa[I].iDeviceIndex:=I;p_dpa[I].pActivate:=Devices[I];
   Devices[I].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,p_dpa[I].lpFriendlyName,N);
   Devices[I].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,p_dpa[I].lpSymbolicLink,N);
  end;
  Result:=Count;
 finally
  for I:=0 to Integer(Count)-1 do Devices[I]:=nil;
  CoTaskMemFree(Devices);
 end;
end;


procedure TDeviceLoss.OnDeviceChange(var aMessage: TMessage);
var
  hr: HResult;
  bDeviceLost: Boolean;

begin

  if not (NativeUInt(p_hdevnotify)<>0) then
    Exit;

  bDeviceLost := False;

  if (AMessage.Msg=WM_DEVICECHANGE) and (AMessage.WParam=DBT_DEVICEREMOVECOMPLETE) and (AMessage.LParam <> 0) then
    begin
      hr := CheckDeviceLost(PDEV_BROADCAST_HDR(Pointer(AMessage.LParam)),
                            bDeviceLost);

      if (SUCCEEDED(hr) and bDeviceLost) then
        begin
          if (p_DeviceLossMessageOptions = dmo_Message) or (p_DeviceLossMessageOptions = dmo_Both) then
            begin
              // Send a WM_DEVICELOST message to the application window
              SendMessage(p_hwndSource,
                          WM_DEVICELOST,
                          WPARAM(p_pwszFriendlyName),
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


function TDeviceLoss.CheckDeviceLost(pHdr: PDEV_BROADCAST_HDR;out pbDeviceLost: Boolean): HResult;
var Device: PDEV_BROADCAST_DEVICEINTERFACE; Name: PWideChar; Count,I: Integer;
begin
 Result:=S_OK;pbDeviceLost:=False;
 if pHdr=nil then Exit;
 if (pHdr.dbch_size<SizeOf(DEV_BROADCAST_HDR)) then Exit(E_INVALIDARG);
 if pHdr.dbch_devicetype<>DBT_DEVTYP_DEVICEINTERFACE then Exit;
 Device:=PDEV_BROADCAST_DEVICEINTERFACE(pHdr);
 Count:=(Integer(pHdr.dbch_size)-(NativeUInt(@Device.dbcc_name)-NativeUInt(Device))) div SizeOf(WideChar);
 if Count<=0 then Exit(E_INVALIDARG);
 Name:=PWideChar(@Device.dbcc_name);I:=0;
 while (I<Count) and (Name[I]<>#0) do Inc(I);
 if I=Count then Exit(E_INVALIDARG);
 pbDeviceLost:=(FSymbolicLink<>'') and SameText(FSymbolicLink,UnicodeString(Name));
end;

end.
