// FactoryX
//
// Copyright © by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmMfCaptureD3D9.pas
// Kind: Pascal Unit
// Release date: 08-03-2018
// Language: ENU
//
// Version: 3.1.3
//
// Description: Preview window.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
// 29/01/2222 Tony                Changed OnDeviceChange for compatibility with Win 10/11
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: MFCaptureD3D Sample
//         main.cpp : Direct3D preview window.
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
unit frmMfCaptureD3D9;

interface

uses
  {WinAPI}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  WinApi.Ks,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  System.Services.Dbt,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {Project}
  dlgSelDevice,
  Preview;


const
  WM_WINDOWVISIBLE = WM_APP + 111;


type

  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    SelectDevice1: TMenuItem;
    N1: TMenuItem;
    procedure SelectDevice1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);

  private
    { Private declarations }

    hwWindowHandle: HWND; // handle to recieve messages
    g_hdevnotify: HDEVNOTIFY;

    function DelayedOnCreate(): BOOL;
    procedure OnChooseDevice(const hw: HWND;
                             const bPrompt: BOOL);
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;
    // Callback
    function MessageHook(var AMessage: TMessage): Boolean;
    // Messages
    procedure OnMove(var message: TWMMove); message WM_MOVE;
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    procedure OnWindowVisible(var message: TMessage); message WM_WINDOWVISIBLE;

  public
    { Public declarations }

    function ShutDown(): Boolean;
    procedure CleanUp();

  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

function TfrmMain.MessageHook(var AMessage: TMessage): Boolean;
begin
  Result := False;
  case AMessage.Msg of

    WM_APP_PREVIEW_ERROR:    begin
                               ShowMessage('Preview error ' + IntToStr(AMessage.wParam));
                               Result:= True;
                             end;

    WM_ERASEBKGND:           begin
                               Result:= True;
                             end;

    WM_QUERYENDSESSION:      begin
                               //
                               // Here you should do savings, destroy objects etc.
                               //
                               Result:= True;
                             end;
  end;

  AMessage.Result := DefWindowProc(hwWindowHandle,
                                   AMessage.Msg,
                                   AMessage.wParam,
                                   AMessage.lParam);
end;



procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  if ShutDown() then
    CanClose := True;
end;


// Post message OK to init d3d objects
procedure TfrmMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle,
              WM_WINDOWVISIBLE,
              0,
              0);
end;

procedure TfrmMain.SelectDevice1Click(Sender: TObject);
begin
  //Initialize for new device.
  OnChooseDevice(hwWindowHandle,
                 True);
end;




//-------------------------------------------------------------------
// CleanUp
//
// Releases resources.
//-------------------------------------------------------------------
procedure TfrmMain.CleanUp();
begin

  if Assigned(g_hdevnotify) then
    begin
      UnregisterDeviceNotification(g_hdevnotify);
      g_hdevnotify := Nil;
    end;

  if Assigned(g_pPreview) then
    begin
      g_pPreview.Free;
      g_pPreview := Nil;
    end;

  // Release the selectdialog device
  if Assigned(dlgSelectDevice) then
    dlgSelectDevice.Close();
end;


//-------------------------------------------------------------------
//  OnChooseDevice
//
//  Select a video capture device.
//
//  hwnd:    A handle to the application window.
/// bPrompt: If TRUE, prompt to user to select the device. Otherwise,
//           select the first device in the list.
//-------------------------------------------------------------------
procedure TfrmMain.OnChooseDevice(const hw: HWND;
                                  const bPrompt: BOOL);
var
  hr: HRESULT;
  iDevice: UINT;
  bCancel: BOOL;
  pAttributes: IMFAttributes;
  mResult: Integer;
  i, x: Integer;
  uiNameLen: UINT32;
  szName: LPWSTR;

label
  done;

begin
  // No dialog initiated on startup, this happens when for instance the mainform is not visible yet
  if not Assigned(dlgSelectDevice) then
    begin
      dlgSelectDevice := TdlgSelectDevice.Create(Self);
    end;

  iDevice := 0;   // Index into the array of devices
  uiNameLen := 0;

  bCancel := False;

  // Check if preview is assigned, else create a new instance of preview

  if not assigned(g_pPreview) then
    begin
      // Create the object that manages video preview.
      hr := TCPreview.CreateInstance(hwWindowHandle,
                                     hwWindowHandle,
                                     g_pPreview);

      if FAILED(hr) then
        begin
          ShowMessage('TCPreview.CreateInstance failed!');
          goto done;
        end;
    end;

  // Initialize an attribute store to specify enumeration parameters.

  hr := MFCreateAttributes(pAttributes,
                           1);

  if FAILED(hr) then
    goto done;

  // Ask for source type = video capture devices.

  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  if FAILED(hr) then
    goto done;

  // Enumerate devices.
  hr := MFEnumDeviceSources(pAttributes,
                            param.ppDevices,
                            param.count);

  if FAILED(hr) then
    goto done;

  // NOTE: param.count might be zero.

{$POINTERMATH ON}

  if (bPrompt = True) then
    begin
      if Assigned(dlgSelectDevice) then
        begin
          // Clear the combobox
          dlgSelectDevice.ComboBox1.Clear;
          // Fill the combobox with found capture devices
          for x := 0 to param.count - 1 do
            begin
              // Try to get the display-friendly-name.
              hr := param.ppDevices[x].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                                          szName,
                                                          uiNameLen);
              // Append the friendly name to the combobox.
              dlgSelectDevice.ComboBox1.Items.Append(szName);
              // Show the first in the list
              dlgSelectDevice.ComboBox1.ItemIndex:= 0;
            end;

          // Show dialog and let user select a device.
          mResult := dlgSelectDevice.ShowModal;

          if (mResult = IDOK) then
            iDevice := param.selection
          else
            begin
             bCancel := True; // User cancelled
            end;

          if ((not bCancel) and (param.count > 0)) then
            // Give this source to the CPlayer object for preview.
            hr := g_pPreview.SetDevice(param.ppDevices[iDevice]);
        end;
    end
  else   // Skip the dialog and select the first capture device - if any -
    begin
      if (param.count > 0) then
        hr := g_pPreview.SetDevice(param.ppDevices[0])
      else
        hr := E_FAIL;
    end;

done:

  pAttributes := Nil;

  for i := 0 to param.count-1 do
    SafeRelease(param.ppDevices[i]);

  CoTaskMemFree(param.ppDevices);

{$POINTERMATH OFF}

  if FAILED(hr) then
    ShowMessage('Cannot create a video capture device. ' + IntToStr(hr));

end;


//------------------------------------------------------------------------------
//  OnDeviceChange
//
//  Handles WM_DEVICECHANGE messages. (see messagehook method)
//------------------------------------------------------------------------------
procedure TfrmMain.OnDeviceChange(var AMessage: TMessage);
var
  bDeviceLost: BOOL;
  PDevBroadcastHeader: PDEV_BROADCAST_HDR;
  pDevBroadCastIntf: PDEV_BROADCAST_DEVICEINTERFACE;
  pwDevSymbolicLink: PWideChar;

begin

  if (g_pPreview = Nil) then
    Exit;

  bDeviceLost := False;

  // Check if the current device was lost.
  if (AMessage.WParam = DBT_DEVICEREMOVECOMPLETE) then
    begin
      // Check if the current video capture device was lost.

      if PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE then
        Exit;

      // Get the symboliclink of the lost device and check.
      PDevBroadcastHeader := PDEV_BROADCAST_HDR(AMessage.LParam);
      pDevBroadCastIntf := PDEV_BROADCAST_DEVICEINTERFACE(PDevBroadcastHeader);
      // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
      pwDevSymbolicLink := PChar(@pDevBroadCastIntf^.dbcc_name);
      bDeviceLost := False;
      if StrIComp(PWideChar(g_pPreview.DeviceSymbolicLink),
                  PWideChar(pwDevSymbolicLink)) = 0 then
        bDeviceLost := True;
    end;


  if (bDeviceLost = True) then
    begin
      g_pPreview.CloseDevice();

      MessageBox(hwWindowHandle,
                 lpcwstr('Lost the capture device.'),
                 lpcwstr(frmMain.caption),
                 MB_OK);
    end;
end;

// OnMove
procedure TfrmMain.OnMove(var Message: TWMMove);
begin
  inherited;
  //
end;

// OnResize
procedure TfrmMain.OnSize(var Message: TWMSize);
begin
  inherited;

  if Assigned(g_pPreview) and (Self.WindowState <> wsMinimized) then
    begin
      // Send a request to adjust the source rect to destination rect.
      // This must be handled after a sample has token. See Preview.pas OnReadSample event.
      g_pPreview.m_request := reqResize;
   end;
end;

// To prevent calling OnResize twice at init,
// wait until the mainwindow is in visible state and then
// initiate the app (See ONCREATE sample C++)
//
// An application should never pass a window handle to Direct3D while handling WM_CREATE or
// when it's state is not visible during startup.
// Only when the form is in visible state, it's safe to initiate D3D9 objects.
procedure TfrmMain.OnWindowVisible(var message: TMessage);
begin
  if not DelayedOnCreate() then
    application.Terminate;  //
end;


function TfrmMain.ShutDown(): Boolean;
begin
  // Clean up all instances
  CleanUp();
  CoTaskMemFree(param.ppDevices);
  Application.UnhookMainWindow(MessageHook);
  Result := True;
end;

//
// Initiate the D3D9 objects
//
function TfrmMain.DelayedOnCreate(): BOOL;
var
  di: DEV_BROADCAST_DEVICEINTERFACE;
  sz: Integer;
  hr: HRESULT;

begin
  Result := False;
  hwWindowHandle := Self.Handle;

  // Register this window to get device notification messages.
  sz := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  ZeroMemory(@di, sz);
  di.dbcc_size := sz;
  di.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  di.dbcc_reserved := 0;
  di.dbcc_classguid := KSCATEGORY_VIDEO_CAMERA; // KSCATEGORY_CAPTURE : Since windows 10 you should not use this guid to register for device loss!
                                                // Otherwise it will return a wrong symoliclink when detecting a device lost.
  di.dbcc_name := #0;

  g_hdevnotify := RegisterDeviceNotification(hwWindowHandle,
                                             @di,
                                             DEVICE_NOTIFY_WINDOW_HANDLE);

  if (g_hdevnotify = Nil) then
    begin
      ShowMessage('RegisterDeviceNotification failed. ' + IntToStr(GetLastError()));
      Exit;
    end;

  // Delphi specific
  Application.HookMainWindow(MessageHook);

  // Create the object that manages video preview.
  hr := TCPreview.CreateInstance(hwWindowHandle,
                                 hwWindowHandle,
                                 g_pPreview);

  if (FAILED(hr)) then
    begin
      ShowMessage('CPreview.CreateInstance failed. ' + IntToStr(hr));
      Exit;
    end;

  // Select the first available device (if any).
  frmMain.OnChooseDevice(hwWindowHandle, FALSE);

  ZeroMemory(@di, sz);
  Result := True;
end;



procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close();
end;

// Since the mainform is the first unit to be initialized, we put the
// initialization code here.

initialization
  // Initialize the COM library
  if SUCCEEDED(CoInitializeEx(Nil,
                              COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE)) then

    if FAILED(MFStartup(MF_VERSION,
                        MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                   MB_ICONSTOP);
      end;


finalization
  //
  MFShutdown();
  CoUninitialize();

end.

