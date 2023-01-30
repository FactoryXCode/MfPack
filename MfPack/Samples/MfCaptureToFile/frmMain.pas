// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 01-02-2022
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Application Mainform.
//              This sample demonstrates how to capture video from camera to a file.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft docs
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
unit frmMain;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.Ks,
  WinApi.ActiveX.ObjBase,
  WinApi.ComBaseApi,
  WinApi.WinError,
  {System}
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Services.Dbt,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  Capture;

const
  IDC_CAPTURE_MP4 = 1001;
  IDC_CAPTURE_WMV = 1002;


  FileContainer_MP4 = IDC_CAPTURE_MP4;
  FileContainer_WMV = IDC_CAPTURE_WMV;

  TARGET_BIT_RATE: UINT32 = (240 * 1000);


type
  TdlgMfCaptureToFile = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edOutputFile: TEdit;
    cbDeviceList: TComboBox;
    rbMP4: TRadioButton;
    rbWMV: TRadioButton;
    butCapture: TButton;
    procedure FormCreate(Sender: TObject);
    procedure rbMP4Click(Sender: TObject);
    procedure rbWMVClick(Sender: TObject);
    procedure butCaptureClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    { Private declarations }
    hDlg: HWND;
    g_hdevnotify: HDEVNOTIFY;
    sActiveDeviceFriendlyName: string;
    bDeviceLost: Boolean;
    DeviceList: TDeviceList;
    CaptureEngine: TMfCaptureEngine;

    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;
    function GetSelectedDevice(out Activate: IMFActivate): HResult;
    procedure SelectEncodingType(const sifile: SmallInt);
    function UpdateDeviceList(): HResult;
    procedure UpdateUI();
    function StartCapture(): HResult;
    function StopCapture(): HResult;
  end;

var
  dlgMfCaptureToFile: TdlgMfCaptureToFile;

implementation

{$R *.dfm}


procedure TdlgMfCaptureToFile.UpdateUI();
var
  hr: HResult;

begin

  if (DeviceList.Count = 0) then  // Are there any capture devices?
    begin
      butCapture.Caption := 'Start Capture';
      cbDeviceList.Enabled := False;
      rbMP4.Enabled := False;
      rbWMV.Enabled := False;
      edOutputFile.Enabled := False;
      if MessageDlg('There is no capture device found on this system.'  + #13 +
                    'Activate a capture device and press Ok or Cancel to ' + #13 +
                    'close the application?',
                    mtWarning,
                    mbOkCancel,
                    0) = mrOk then
        begin
          // Enumerate the video capture devices again.
          hr := UpdateDeviceList();
          if SUCCEEDED(hr) and Assigned(CaptureEngine) then
            begin
              butCapture.Caption := 'Start Capture';
              cbDeviceList.Enabled := True;
              rbMP4.Enabled := True;
              rbWMV.Enabled := True;
              edOutputFile.Enabled := True;
            end;
          Exit;
        end
      else
        Close();
        Exit;
    end;

  if not Assigned(CaptureEngine) then
    begin
      StartCapture();
      butCapture.Caption := 'Stop Capture';
      cbDeviceList.Enabled := False;
      rbMP4.Enabled := False;
      rbWMV.Enabled := False;
      edOutputFile.Enabled := False;
      Exit;
    end;

  if Assigned(CaptureEngine) then
    begin
      if CaptureEngine.IsCapturing() = State_Capturing then   // Is video capture in progress now?
        begin
          StopCapture();
          butCapture.Caption := 'Start Capture';
          cbDeviceList.Enabled := True;
          rbMP4.Enabled := True;
          rbWMV.Enabled := True;
          edOutputFile.Enabled := True;
          Exit;
        end
    end;



  if bDeviceLost then
    begin
      if MessageDlg('Capture device ' + sActiveDeviceFriendlyName + ' is removed or lost.'  + #13 +
                    'There are IntToStr(DeviceList.Count) available capturedevices on this system' + #13 + #13 +
                    'Close the application?',
                    mtWarning,
                    mbYesNo,
                    0) = mrNo then
        begin
          butCapture.Caption := 'Start Capture';
          cbDeviceList.Enabled := True;
          rbMP4.Enabled := True;
          rbWMV.Enabled := True;
          edOutputFile.Enabled := True;
          bDeviceLost := False; // reset this value
        end
      else
        Close();
    end;
end;


procedure TdlgMfCaptureToFile.butCaptureClick(Sender: TObject);
begin
  UpdateUI();
end;



//-----------------------------------------------------------------------------
// OnDeviceChange
//
// Handles WM_DEVICECHANGE messages.
//-----------------------------------------------------------------------------
procedure TdlgMfCaptureToFile.OnDeviceChange(var AMessage: TMessage);
var
  PDevBroadcastHeader: PDEV_BROADCAST_HDR;
  pDevBroadCastIntf: PDEV_BROADCAST_DEVICEINTERFACE;
  pwDevSymbolicLink: PWideChar;
  hr: HResult;

begin

  if AMessage.WParam = DBT_DEVICEREMOVECOMPLETE then
    begin
      // Check for added/removed devices, regardless of whether
      // the application is capturing video at this time.
      UpdateDeviceList();

      // Now check if the current video capture device was lost.

      if PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE then
        Exit;

      // Get the symboliclink of the lost device and check.
      PDevBroadcastHeader := PDEV_BROADCAST_HDR(AMessage.LParam);
      pDevBroadCastIntf := PDEV_BROADCAST_DEVICEINTERFACE(PDevBroadcastHeader);
      // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
      pwDevSymbolicLink := PChar(@pDevBroadCastIntf^.dbcc_name);

      hr := S_OK;
      bDeviceLost := False;

      if Assigned(CaptureEngine) then
        if CaptureEngine.IsCapturing() = State_Capturing then
          begin
            if StrIComp(PWideChar(CaptureEngine.DeviceSymbolicLink),
                        PWideChar(pwDevSymbolicLink)) = 0 then
              bDeviceLost := True;

            if FAILED(hr) or bDeviceLost then
              StopCapture();
          end;
    end;
end;


procedure TdlgMfCaptureToFile.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(CaptureEngine) then
    begin
      CaptureEngine.EndCaptureSession();
      SafeDelete(CaptureEngine);
    end;

  if Assigned(DeviceList) then
    begin
       DeviceList.Free;
       DeviceList := nil;
    end;

  if Assigned(g_hdevnotify) then
    UnregisterDeviceNotification(g_hdevnotify);

  DeAllocateHwnd(hDlg);

  MFShutdown();
  CoUninitialize();

end;


procedure TdlgMfCaptureToFile.FormCreate(Sender: TObject);
var
  hr: HResult;
  devbroadcastdevice: DEV_BROADCAST_DEVICEINTERFACE;
  iSize: Integer;

begin

  iSize := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  ZeroMemory(@devbroadcastdevice, iSize);

  sActiveDeviceFriendlyName := 'Unknown';

  hDlg := AllocateHwnd(OnDeviceChange);  // don't use Self.Handle, because during the lifetime this handle could be changed!

  DeviceList := TDeviceList.Create;

  // Initialize the COM library
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED or
                       COINIT_DISABLE_OLE1DDE);

  // Initialize Media Foundation
  if SUCCEEDED(hr) then
    hr := MFStartup();

  // Register for device notifications
  if SUCCEEDED(hr) then
    begin

      devbroadcastdevice.dbcc_size := iSize;
      devbroadcastdevice.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
      devbroadcastdevice.dbcc_reserved := 0;
      devbroadcastdevice.dbcc_classguid := KSCATEGORY_VIDEO_CAMERA; // KSCATEGORY_CAPTURE : Since windows 10 you should not use this guid to register for device loss! Otherwise it will return a wrong symoliclink when detecting a device lost.
      devbroadcastdevice.dbcc_name := #0;

      g_hdevnotify := RegisterDeviceNotification(hDlg,
                                                 @devbroadcastdevice,
                                                 DEVICE_NOTIFY_WINDOW_HANDLE);

      if not Assigned(g_hdevnotify) then
        hr := HRESULT_FROM_WIN32(GetLastError());

    end;

  // Enumerate the video capture devices.
  if SUCCEEDED(hr) then
    hr := UpdateDeviceList();

  if Failed(hr) then
    begin
      MessageBox(hDlg,
                 LPCWSTR('Could not find a video capture device.'),
                 LPCWSTR('No device found'),
                 MB_OK);
      Close();
    end;
end;


// ==============================================================

//-----------------------------------------------------------------------------
// CreateSelectedDevice
//
// Create a media source for the video capture device selected by the user.
//-----------------------------------------------------------------------------

function TdlgMfCaptureToFile.GetSelectedDevice(out Activate: IMFActivate): HResult;
var
  iListIndex: Integer;
  hr : HResult;

label
  done;

begin

  // First get the index of the selected item in the combo box.
  iListIndex := cbDeviceList.ItemIndex;

  if (iListIndex = CB_ERR) then
    begin
      hr := HRESULT_FROM_WIN32(GetLastError());
      goto done;
    end;


  // Now create the media source.
  hr := DeviceList.GetDevice(iListIndex, Activate);

done:
    Result := hr;
end;


function TdlgMfCaptureToFile.UpdateDeviceList(): HResult;
var
  hr: HResult;
  szFiendlyName: PWideChar;
  iDevice: UINT32;

label
  done;

begin
  if not Assigned(DeviceList) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  cbDeviceList.Clear;
  cbDeviceList.Sorted := False;

  hr := DeviceList.EnumerateDevices();

  if FAILED(hr) then
    goto done;

  for iDevice := 0 to DeviceList.Count-1 do
    begin
      // Get the fiendly name of the device.
      hr := DeviceList.GetFriendlyName(iDevice,
                                       szFiendlyName);

      if FAILED(hr) then
        goto done;

      // Add the string to the combo-box. This message returns the index in the list.
      cbDeviceList.Items.Append(WideCharToString(szFiendlyName));

      CoTaskMemFree(szFiendlyName);
      szFiendlyName := Nil;

    if (DeviceList.Count > 0) then
      begin
        // Select the first item.
        cbDeviceList.ItemIndex := 0;
      end
    else
      hr := ERROR_SYSTEM_DEVICE_NOT_FOUND;
    end;

done:
  Result := hr;
end;

//-----------------------------------------------------------------------------
// OnSelectEncodingType
//
// Called when the user toggles between file-format types.
//-----------------------------------------------------------------------------

procedure TdlgMfCaptureToFile.SelectEncodingType(const sifile: SmallInt);
var
  sFile: string;

begin
  sFile := edOutputFile.Text;

  case (sifile) of

    FileContainer_MP4: begin
                         sFile := ChangeFileExt(sFile, '.mp4');
                         rbMP4.Checked := True;
                       end;

    FileContainer_WMV: begin
                         sFile := ChangeFileExt(sFile, '.wmv');
                         rbWMV.Checked := True;
                       end;

    else               begin
                         sFile := ChangeFileExt(sFile, '.mp4');
                         rbMP4.Checked := True;
                       end;
  end;

  edOutputFile.Text := sFile;

end;


procedure TdlgMfCaptureToFile.rbMP4Click(Sender: TObject);
begin
  SelectEncodingType(FileContainer_MP4);
end;

procedure TdlgMfCaptureToFile.rbWMVClick(Sender: TObject);
begin
  SelectEncodingType(FileContainer_WMV);
end;


//-----------------------------------------------------------------------------
// StartCapture
//
// Starts video capture.
//-----------------------------------------------------------------------------

function TdlgMfCaptureToFile.StartCapture(): HResult;
var
  params: EncodingParameters;
  hr: HResult;
  pActivate: IMFActivate;
  szFile: PWideChar;

label
  done;

begin

  if rbWMV.Checked then
    params.subtype := MFVideoFormat_WMV3
  else
    params.subtype := MFVideoFormat_H264;

  params.bitrate := TARGET_BIT_RATE;

  // Get the name of the target file.
  szFile := PWideChar(edOutputFile.Text);
  if Length(szFile) > 0 then
    hr := S_OK
  else
    begin
      hr := ERROR_INVALID_NAME;
      goto done;
    end;

  // Create the media source for the capture device.
  if SUCCEEDED(hr) then
    hr := GetSelectedDevice(pActivate);

  // Start capturing.
  if SUCCEEDED(hr) then
    hr := TMfCaptureEngine.CreateInstance(hDlg,
                                          CaptureEngine);

  if SUCCEEDED(hr) then
    hr := CaptureEngine.StartCapture(pActivate,
                                     szFile,
                                     params);

  if SUCCEEDED(hr) then
    sActiveDeviceFriendlyName := WideCharToString(CaptureEngine.DeviceFriendlyName);

done:
   Result := hr;

{$IF DEBUG}
  if FAILED(hr) then
     OutputDebugString(PChar('Error starting capture. hr = ') + PChar(IntToStr(hr));
{$ENDIF}

end;


//-----------------------------------------------------------------------------
// StopCapture
//
// Stops video capture.
//-----------------------------------------------------------------------------
function TdlgMfCaptureToFile.StopCapture(): HResult;
var
  hr: HResult;

begin
  hr := CaptureEngine.EndCaptureSession();

  SafeRelease(CaptureEngine);

  UpdateDeviceList();

  // NOTE: Updating the device list releases the existing IMFActivate
  // pointers. This ensures that the current instance of the video capture
  // source is released.

{$IF DEBUG}
  if FAILED(hr) then
     OutputDebugString(PChar('Error stopping capture. File might be corrupt. hr = ') + PChar(IntToStr(hr));
{$ENDIF}

  Result := hr

end;


end.
