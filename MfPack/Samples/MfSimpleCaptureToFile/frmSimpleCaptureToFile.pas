// FactoryX
//
// Copyright © by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmSimpleCaptureToFile.pas
// Kind: Pascal Unit
// Release date: 26-01-2018
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description: Main window
//
// Company: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: MFSimpleCapture Example.
//
// https://github.com/Microsoft/Windows-classic-samples/tree/master/Samples/Win7Samples/multimedia/mediafoundation/MFCaptureToFile
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
unit frmSimpleCaptureToFile;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Services.Dbt,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {ActiveX}
  WinApi.ComBaseApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  MfCaptureToFileClass;

type

  //-------------------------------------------------------------------
  // ChooseDeviceParam struct
  //
  // Contains an array of IMFActivate pointers. Each pointer represents
  // a video capture device. This struct is passed to the dialog where
  // the user selects a device.
  //-------------------------------------------------------------------
  ChooseDeviceParam = record
    ppDevices: PIMFActivate;
    count: UINT32;
    selection: UINT32;
  end;

const
  TARGET_BIT_RATE = UINT32(240 * 1000);

type

  TFrm_SimpleCapture = class(TForm)
    pnlControls: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    butStopCapture: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    cbxSelectDevice: TComboBox;
    edOutputFile: TEdit;
    Label2: TLabel;
    rbMp4: TRadioButton;
    rbWmf: TRadioButton;
    Bevel1: TBevel;
    butStartCapture: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure rbMp4Click(Sender: TObject);
    procedure rbWmfClick(Sender: TObject);
    procedure butStartCaptureClick(Sender: TObject);
    procedure butStopCaptureClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private declarations }
    pszFile: PWideChar;
    bAppIsClosing: Boolean;
    m_hdevnotify: HDEVNOTIFY;  {pointer, function RegisterForDeviceNotification}
    bDeviceLost: Boolean;
    iSelectedDevice: Integer;
    MfCaptureToFileEngine: TCaptureToFile;

    //Quits the session and releases the capture engine
    procedure QuitSession();
    function GetFmCapture(): HRESULT;
    function UpdateDeviceList(): HRESULT;
    function GetSelectedDevice(var ppActivate: IMFActivate): HRESULT;

    function StartCapture(): HRESULT;
    procedure StopCapture();

    // Listen for WM_DEVICECHANGE messages.
    // The lParam message parameter is a pointer to a DEV_BROADCAST_HDR structure.
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;

  public
    { Public declarations }

  end;

var
  Frm_SimpleCapture: TFrm_SimpleCapture;


implementation

{$R *.dfm}


//-------------------------------------------------------------------
// CleanUp: Frees resources before the application exits
//-------------------------------------------------------------------
procedure TFrm_SimpleCapture.QuitSession();
begin
  StopCapture();
  MfCaptureToFileEngine.Free;
  MfCaptureToFileEngine := nil;
  butStartCapture.Enabled := True;
  butStopCapture.Enabled := False;
end;


procedure TFrm_SimpleCapture.rbMp4Click(Sender: TObject);
begin
  if rbMp4.Checked then
    edOutputFile.Text:= ChangeFileExt(edOutputFile.Text, '.mp4');
end;


procedure TFrm_SimpleCapture.rbWmfClick(Sender: TObject);
begin
  if rbWmf.Checked then
    edOutputFile.Text:= ChangeFileExt(edOutputFile.Text, '.wmf');
end;

// Start capturing
procedure TFrm_SimpleCapture.butStartCaptureClick(Sender: TObject);
begin
  StartCapture();
end;

// Stop capturing
procedure TFrm_SimpleCapture.butStopCaptureClick(Sender: TObject);
begin
  StopCapture();
end;


procedure TFrm_SimpleCapture.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TFrm_SimpleCapture.FormCloseQuery(Sender: TObject;
                                            var CanClose: Boolean);
begin
  CanClose := False;
  // Before closing the app, clean up.
  QuitSession();
  UnRegisterForDeviceNotification(m_hdevnotify);
  CanClose := True;
end;


procedure TFrm_SimpleCapture.FormCreate(Sender: TObject);
var
  hr: HRESULT;  // For debugging issues only
begin
  bAppIsClosing := False;
  // Get the capture engine
  hr := GetFmCapture();
  // Get the capture device list
  if SUCCEEDED(hr) then
    hr := UpdateDeviceList();

  if (RegisterForDeviceNotification(Handle,
                                    m_hdevnotify) = False) then
        MessageBox(0,
                   PWideChar('RegisterForDeviceNotification failed!'),
                   nil,
                   MB_OK);
  if FAILED(hr) then
    ShowMessage('Function UpdateDeviceList() Failed!');
end;


//-----------------------------------------------------------------------------
// UpdateDeviceList
//
// Enumerates the video capture devices and populates the list of device
// names in the dialog UI.
//-----------------------------------------------------------------------------
function TFrm_SimpleCapture.UpdateDeviceList(): HRESULT;
var
  hr: HRESULT;
  szFriendlyName: string;
  iDevice: Integer;

label
  Done;

begin

  if Assigned(MfDeviceList) then
    begin
      cbxSelectDevice.Clear;
      szFriendlyName := '';
      MfDeviceList.Clear();

      hr := MfDeviceList.EnumerateDevices();

      if FAILED(hr) then
        goto Done;

      for iDevice := 0 to MfDeviceList.Count() - 1 do
        begin
          // Get the friendly name of the device.
          hr := MfDeviceList.GetDeviceName(iDevice,
                                           szFriendlyName);

          if FAILED(hr) then
            goto Done;

          // Add the string to the combo-box. This message returns the index in the list.
          cbxSelectDevice.Items.Append(szFriendlyName);

          // The list might be sorted, so the list index is not always the same as the
          // array index. Therefore, set the array index as item data.
          cbxSelectDevice.ItemIndex := iDevice;

          szFriendlyName := '';
        end;
      cbxSelectDevice.ItemIndex := 0;
    end
  else
    hr:= E_FAIL;

Done:
    Result:= hr;

end;


//-----------------------------------------------------------------------------
// CreateSelectedDevice
//
// Create a media source for the video capture device selected by the user.
//-----------------------------------------------------------------------------
function TFrm_SimpleCapture.GetSelectedDevice(var ppActivate: IMFActivate): HRESULT;
var
  hr: HRESULT;
  iListIndex: UINT32;

begin
  // First get the index of the selected item in the combo box.
  iListIndex := cbxSelectDevice.ItemIndex;

  // Now find the index of the device within the device list.
  //
  // This index is stored as item data in the combo box, so that
  // the order of the combo box items does not need to match the
  // order of the device list.

  // Now create the media source.
  hr := MfDeviceList.GetDevice(iListIndex,
                               ppActivate);
  iSelectedDevice := iListIndex;
  Result := hr;
end;



//-----------------------------------------------------------------------------
// StartCapture
//
// Starts video capture.
//-----------------------------------------------------------------------------
function TFrm_SimpleCapture.StartCapture(): HRESULT;
var
  hr: HRESULT;
  params: EncodingParameters;
  pActivate: IMFActivate;

begin

  hr := S_OK;

  if rbWmf.checked then
    params.subtype := MFVideoFormat_WMV3
  else
    params.subtype := MFVideoFormat_H264;

  //Set filename, extension depends on selected format.
  params.bitrate := TARGET_BIT_RATE;

  // Get the name of the target file.
  if (Length(edOutputFile.Text) = 0) or (Length(edOutputFile.Text) > MAX_PATH) then
    hr := E_INVALIDARG;

  // Create the media source for the capture device.
  if SUCCEEDED(hr) then
    hr := GetSelectedDevice(pActivate);

  // Start capturing.

  if SUCCEEDED(hr) then
    MfCaptureToFileEngine.Create(Frm_SimpleCapture.Handle);

  if SUCCEEDED(hr) then
    begin
      pszFile := StrToPWideChar(edOutputFile.Text);
      hr := MfCaptureToFileEngine.StartCapture(pActivate,
                                               pszFile,
                                               params);
    end;

  if FAILED(hr) then
    begin
      ShowMessage('Error: Starting capture. Result: ' + IntToStr(hr));
      butStartCapture.Enabled := True;
      butStopCapture.Enabled := False;
    end
  else
    begin
      butStartCapture.Enabled := False;
      butStopCapture.Enabled := True;
    end;

  Result := hr;
end;


procedure TFrm_SimpleCapture.StopCapture();
var
  hr: HRESULT; // For debugging issues only, discard compiler messages.

begin
  hr := S_OK;
  if not Assigned(MfCaptureToFileEngine) then
    Exit;

  if (MfCaptureToFileEngine.State = State_Capturing) then
    hr := MfCaptureToFileEngine.EndCaptureSession();


  if SUCCEEDED(hr) then
    hr := UpdateDeviceList();

  // NOTE: Updating the device list releases the existing IMFActivate
  // pointers. This ensures that the current instance of the video capture
  // source is released.

  if FAILED(hr) then
    begin
      ShowMessage('Error: Stopping capture. File might be corrupted. Result: ' + IntToStr(hr));
      butStartCapture.Enabled := True;
      butStopCapture.Enabled := True;
    end
  else
    begin
      butStartCapture.Enabled := True;
      butStopCapture.Enabled := False;
    end;

end;


function TFrm_SimpleCapture.GetFmCapture(): HRESULT;
begin
  Result := E_FAIL;

  // Create the devicelist object
  if not Assigned(MfDeviceList) then
    begin
      MfDeviceList := TDeviceList.Create();
    end;

  // Create the cature engine
  if not Assigned(MfCaptureToFileEngine) then
    begin
      MfCaptureToFileEngine := nil;
      MfCaptureToFileEngine := TCaptureToFile.Create(Frm_SimpleCapture.Handle);  // Must be main form or a parent window !!!
      Result := S_OK;
    end;
end;


// Message listener
// Listen for WM_DEVICECHANGE messages.
// The lParam message parameter is a pointer to a DEV_BROADCAST_HDR structure.
//-----------------------------------------------------------------------------
// OnDeviceChange
//
// Handles WM_DEVICECHANGE messages.
//-----------------------------------------------------------------------------
procedure TFrm_SimpleCapture.OnDeviceChange(var AMessage: TMessage);
var
  PDevBroadcastHeader: PDEV_BROADCAST_HDR;
  pDevBroadCastIntf: PDEV_BROADCAST_DEVICEINTERFACE;
  pwDevSymbolicLink: PWideChar;
  hr: HResult;

begin

  if (AMessage.WParam = DBT_DEVICEREMOVECOMPLETE) then
    begin
      // Check for added/removed devices, regardless of whether
      // the application is capturing video at this time.
      UpdateDeviceList();

      // Check if the current video capture device was lost.
      if (PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE) then
        Exit;

      // Get the symboliclink of the lost device and check.
      PDevBroadcastHeader := PDEV_BROADCAST_HDR(AMessage.LParam);
      pDevBroadCastIntf := PDEV_BROADCAST_DEVICEINTERFACE(PDevBroadcastHeader);

      // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
      // Dereference the struct's field dbcc_name (array [0..0] of WideChar) for a readable string.
      pwDevSymbolicLink := PChar(@pDevBroadCastIntf^.dbcc_name);

      hr := S_OK;
      bDeviceLost := False;

      if Assigned(MfCaptureToFileEngine) then
        if MfCaptureToFileEngine.IsCapturing() then
          begin
            if (StrIComp(PWideChar(MfCaptureToFileEngine.DeviceSymbolicLink),
                         PWideChar(pwDevSymbolicLink)) = 0) then
              bDeviceLost := True;

            if (FAILED(hr) or bDeviceLost) then
              begin
                // Show dialog with info about which device is disconnected.
                MessageDlg(Format('Lost capture device %s.', [MfCaptureToFileEngine.DeviceName]),
                mtError,
                mbOKCancel,
                0);
                StopCapture();
              end;
          end;
    end;
end;
end.
