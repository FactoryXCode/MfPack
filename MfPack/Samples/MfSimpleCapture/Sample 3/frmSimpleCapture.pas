// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmSimpleCapture.pas
// Kind: Pascal Unit
// Release date: 21/09/2024
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description: This is sample 3 of SimpleCapture that shows you how to implement
//              Media Foundation camera interfaces and source-level KS controls.
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Ciaran (Ciaran3)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher; optional camera defaults require Windows 11.
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
// =============================================================================
// Source: Parts of CPreview Examples.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// FactoryX, Copyright (c) FactoryX. All rights reserved.
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

unit frmSimpleCapture;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Services.Dbt,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  WinApi.MediaFoundationApi.MfMetLib,
  MfDeviceCaptureClass,
  dlgMfCameraControls;

type

  TFrm_SimpleCapture = class(TForm)
    pnlCommands: TPanel;
    btnSelect: TButton;
    btnStart: TButton;
    btnCloseCamera: TButton;
    btnControls: TButton;
    chkAutoPreview: TCheckBox;
    lblRotation: TLabel;
    cboRotation: TComboBox;
    pnlVideo: TPanel;
    pnlStatus: TPanel;
    lblDevice: TLabel;
    lblStatus: TLabel;
    StateTimer: TTimer;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SelectClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure CloseCameraClick(Sender: TObject);
    procedure ControlsClick(Sender: TObject);
    procedure HandleRotationChanged(Sender: TObject);
    procedure VideoResize(Sender: TObject);
    procedure StateTimerTimer(Sender: TObject);

  private
    FEngine: TMfCaptureEngine;
    FDialog: TMfCameraControlsDialog;
    FCameraCount: Integer;
    FDeviceNames: TArray<string>;
    FDeviceLinks: TArray<string>;
    FClosing: Boolean;
    FSelectedDevice: Integer;

    procedure UpdateControls();
    procedure WMDeviceChange(var Message: TMessage); message WM_DEVICECHANGE;

  public
    function RefreshCameras(): HRESULT;
    function OpenCamera(const AIndex: Integer): HRESULT;
    procedure CloseCamera();
    property Engine: TMfCaptureEngine read FEngine;
    property CameraCount: Integer read FCameraCount;
    property ControlsDialog: TMfCameraControlsDialog read FDialog;
  end;

var
  Frm_SimpleCapture: TFrm_SimpleCapture;

implementation

uses
  WinApi.WinError,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  frmdlgChooseDevice;

{$R *.dfm}

procedure TFrm_SimpleCapture.FormCreate(Sender: TObject);

begin

  FDialog := TMfCameraControlsDialog.Create(Self);
  FSelectedDevice := -1;
  // No camera is opened automatically, including by the --smoke check.
  FCameraCount := 0;
  if not SameText(ParamStr(1), '--smoke') then
    RefreshCameras();
  UpdateControls();
end;


procedure TFrm_SimpleCapture.FormDestroy(Sender: TObject);

begin

  FClosing := True;
  StateTimer.Enabled := False;
  CloseCamera();
  FreeAndNil(FDialog);
  SetLength(FDevicePropertiesArray,
            0);
  SetLength(FDeviceNames,
            0);
  SetLength(FDeviceLinks,
            0);
end;


function TFrm_SimpleCapture.RefreshCameras(): HRESULT;
var
  Attributes: IMFAttributes;
  Devices: PIMFActivate;
  Cursor: PIMFActivate;
  Count: UINT32;
  I: Integer;
  Text: PWideChar;
  TextLength: UINT32;

begin

  SetLength(FDevicePropertiesArray,
            0);
  SetLength(FDeviceNames,
            0);
  SetLength(FDeviceLinks,
            0);
  Devices := nil;
  Count := 0;
  Result := MFCreateAttributes(Attributes,
                               1);
  if SUCCEEDED(Result) then
    Result := Attributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                 MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  if SUCCEEDED(Result) then
    Result := MFEnumDeviceSources(Attributes,
                                  Devices,
                                  Count);
  if SUCCEEDED(Result) then
    try
      SetLength(FDevicePropertiesArray,
                Count);
      SetLength(FDeviceNames,
                Count);
      SetLength(FDeviceLinks,
                Count);
      Cursor := Devices;
      for I := 0 to Integer(Count) - 1 do
        begin
          FDevicePropertiesArray[I].pActivate := Cursor^;
          FDevicePropertiesArray[I].riId := MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID;
          FDevicePropertiesArray[I].iDeviceIndex := I;
          Text := nil;
          Result := Cursor^.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                               Text,
                                               TextLength);
          try
            if SUCCEEDED(Result) then
              FDeviceNames[I] := string(Text);
          finally
            CoTaskMemFree(Text);
          end;
          if FAILED(Result) then
            Break;
          Text := nil;
          Result := Cursor^.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                               Text,
                                               TextLength);
          try
            if SUCCEEDED(Result) then
              FDeviceLinks[I] := string(Text);
          finally
            CoTaskMemFree(Text);
          end;
          if FAILED(Result) then
            Break;
          FDevicePropertiesArray[I].lpFriendlyName := PWideChar(FDeviceNames[I]);
          FDevicePropertiesArray[I].lpDisplayName := PWideChar(FDeviceNames[I]);
          FDevicePropertiesArray[I].lpSymbolicLink := PWideChar(FDeviceLinks[I]);
          Inc(Cursor);
        end;
    finally
      Cursor := Devices;
      for I := 0 to Integer(Count) - 1 do
        begin
          Cursor^ := nil;
          Inc(Cursor);
        end;
      CoTaskMemFree(Devices);
      if FAILED(Result) then
        SetLength(FDevicePropertiesArray,
                  0);
    end;
  FCameraCount := Length(FDevicePropertiesArray);
  if FAILED(Result) then
    lblStatus.Caption := Format('Camera enumeration failed ($%.8x).',
                                [Cardinal(Result)])
  else
    lblStatus.Caption := Format('%d camera(s) found. Select a capture device.',
                                [FCameraCount]);
end;


function TFrm_SimpleCapture.OpenCamera(const AIndex: Integer): HRESULT;

begin

  if (AIndex < 0) or (AIndex >= FCameraCount) then
    Exit(E_INVALIDARG);
  CloseCamera();
  // The MF source, Media Session, partial topology and EVR come from Sample 2.
  Result := TMfCaptureEngine.CreateInstance(pnlVideo.Handle,
                                            Handle,
                                            FEngine);
  if SUCCEEDED(Result) then
    begin
      FEngine.AutoPreview := chkAutoPreview.Checked;
      FEngine.Rotation := cboRotation.ItemIndex * 90;
      Result := FEngine.SetDevice(FDevicePropertiesArray[AIndex]);
    end;
  if SUCCEEDED(Result) then
    begin
      FSelectedDevice := AIndex;
      lblDevice.Caption := 'Camera: ' + string(FDevicePropertiesArray[AIndex].lpFriendlyName);
    end
  else
    begin
      CloseCamera();
      lblStatus.Caption := Format('Opening camera failed ($%.8x).',
                                  [Cardinal(Result)]);
    end;
  UpdateControls();
end;


procedure TFrm_SimpleCapture.CloseCamera();

begin

  if Assigned(FDialog) then
    begin
      FDialog.Hide();
      FDialog.Bind(nil);
    end;
  if Assigned(FEngine) then
    begin
      FEngine.ShutDownEngine();
      FreeAndNil(FEngine);
    end;
  lblDevice.Caption := 'No camera open';
  FSelectedDevice := -1;
  pnlVideo.Invalidate();
  UpdateControls();
end;


procedure TFrm_SimpleCapture.UpdateControls();

begin

  btnControls.Enabled := Assigned(FEngine);
  btnCloseCamera.Enabled := Assigned(FEngine);
  btnStart.Enabled := Assigned(FEngine) and
                      (FEngine.State in [Ready, TopologyReady, Stopped]);
  chkAutoPreview.Enabled := not Assigned(FEngine);
end;


procedure TFrm_SimpleCapture.SelectClick(Sender: TObject);
var
  Dialog: TdlgChooseDevice;
  Status: HRESULT;

begin

  Status := RefreshCameras();
  if FAILED(Status) or (FCameraCount = 0) then
    Exit;
  Dialog := TdlgChooseDevice.Create(Self);
  try
    if (Dialog.ShowModal() = 111) then
      OpenCamera(Dialog.iSelectedDevice);
  finally
    Dialog.Free();
  end;
end;


procedure TFrm_SimpleCapture.StartClick(Sender: TObject);
var
  Status: HRESULT;

begin

  if Assigned(FEngine) then
    begin
      Status := FEngine.StartPreview();
      if FAILED(Status) then
        lblStatus.Caption := Format('Starting preview failed ($%.8x).',
                                    [Cardinal(Status)]);
    end;
end;


procedure TFrm_SimpleCapture.CloseCameraClick(Sender: TObject);

begin

  CloseCamera();
end;


procedure TFrm_SimpleCapture.ControlsClick(Sender: TObject);

begin

  if Assigned(FEngine) then
    begin
      FEngine.CameraControls.Refresh();
      FDialog.Bind(FEngine.CameraControls);
      FDialog.Show();
      FDialog.BringToFront();
    end;
end;


procedure TFrm_SimpleCapture.HandleRotationChanged(Sender: TObject);
var
  DeviceIndex: Integer;

begin

  if not Assigned(FEngine) then
    Exit;
  // Reopen the selected source so rotation is negotiated on a fresh topology.
  // OpenCamera retains the automatic/deferred preview selection.
  DeviceIndex := FSelectedDevice;
  OpenCamera(DeviceIndex);
end;


procedure TFrm_SimpleCapture.FormPaint(Sender: TObject);

begin

  if Assigned(FEngine) then
    FEngine.UpdateVideo();
end;


procedure TFrm_SimpleCapture.VideoResize(Sender: TObject);

begin

  if Assigned(FEngine) then
    FEngine.ResizeVideo(nil);
end;


procedure TFrm_SimpleCapture.StateTimerTimer(Sender: TObject);

begin

  if FClosing then
    Exit;
  UpdateControls();
  if Assigned(FEngine) then
    begin
      if FAILED(FEngine.LastStatus) then
        lblStatus.Caption := Format('Capture error ($%.8x).',
                                    [Cardinal(FEngine.LastStatus)])
      else
        case FEngine.State of
          Ready: lblStatus.Caption := 'Camera open. Configure pre-start controls, then start preview.';
          OpenPending: lblStatus.Caption := 'Preparing the Media Foundation topology...';
          TopologyReady: lblStatus.Caption := 'Ready. Set pre-start controls if needed, then start preview.';
          Starting: lblStatus.Caption := 'Starting preview...';
          Started: lblStatus.Caption := 'Media Foundation camera preview is running.';
          Stopped: lblStatus.Caption := 'Preview stopped.';
        end;
    end;
end;


procedure TFrm_SimpleCapture.WMDeviceChange(var Message: TMessage);
var
  Lost: Boolean;
  Header: PDEV_BROADCAST_HDR;

begin

  if Assigned(FEngine) and (Message.LParam <> 0) then
    begin
      Header := PDEV_BROADCAST_HDR(Message.LParam);
      Lost := False;
      FEngine.CheckCaptureDeviceLost(Header,
                                     Lost);
      if Lost then
        begin
          CloseCamera();
          lblStatus.Caption := 'The selected camera was disconnected.';
        end;
    end;
end;

end.
