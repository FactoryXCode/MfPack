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
// Revision Version: 3.1.7
//
// Description: This is sample 2 of SimpleCapture that shows you how to implement
//              IAMCameraControl and IAMVideoProcAmp to control camera and video.
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
// 21/09/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or higher.
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

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.Ks,
  WinApi.KsMedia,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  {System}
  System.Services.Dbt,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {DirectShow}
  WinApi.StrmIf,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfPlay,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  frmdlgChooseDevice,
  MfDeviceCaptureClass;

type

  TFrm_SimpleCapture = class(TForm)
    pnlControls: TPanel;
    Button1: TButton;
    butGetDevice: TButton;
    pnlVideo: TPanel;
    pnlCameraAndVideoControl: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbxCameraControlProperty: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    cbxVideoControlProperty: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    cbxCameraControlFlags: TComboBox;
    Label7: TLabel;
    cbxVideoControlFlags: TComboBox;
    Label8: TLabel;
    lblRotation: TLabel;
    cboRotation: TComboBox;
    cbxCameraValues: TComboBox;
    butShowProperties: TButton;
    cbxVideoValues: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure butGetDeviceClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HandleRotationChanged(Sender: TObject);
    procedure cbxCameraControlPropertySelect(Sender: TObject);
    procedure cbxVideoControlPropertySelect(Sender: TObject);
    procedure cbxCameraControlFlagsSelect(Sender: TObject);
    procedure cbxCameraValuesSelect(Sender: TObject);
    procedure butShowPropertiesClick(Sender: TObject);
    procedure cbxVideoValuesSelect(Sender: TObject);

  private
    { Private declarations }
    //p_hdevnotify: HDEVNOTIFY;
    MfDeviceCapture: TMfCaptureEngine;
    bAppIsClosing: Boolean;
    bFullScreenMode: Boolean;
    iSelectedDevice: Integer;

    // Camera & video control.
    fSelectedCamaraControl: TCameraPropSet;
    fSelectedVideoControl: TVideoPropSet;

    // Quits the session and releases the capture engine
    procedure QuitSession();
    function GetFmCapture(): HRESULT;

    procedure SetCbxValues(pCbx: TComboBox;
                           iContrPropIndex,
                           iMax,
                           iMin,
                           iStep,
                           iDefault,
                           iFlags: Integer;
                           pCtl: LONG);

    function SetCameraProperties(pIndex: Integer): HRESULT;
    function SetVideoProperties(pIndex: Integer): HRESULT;

    procedure SetCameraPropFlags();
    procedure SetVideoPropFlags();

    // Listen for WM_DEVICECHANGE messages.
    // The lParam message parameter is a pointer to a DEV_BROADCAST_HDR structure.
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;

  public
    { Public declarations }

    bDeviceLost: Boolean;

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

  if Assigned(MfDeviceCapture) then
    begin
      if SUCCEEDED(MfDeviceCapture.ShutDownEngine()) then
        FreeAndNil(MfDeviceCapture);
    end;
end;


// TEST

procedure TFrm_SimpleCapture.SetCbxValues(pCbx: TComboBox;
                                          iContrPropIndex,
                                          iMax,
                                          iMin,
                                          iStep,
                                          iDefault,
                                          iFlags: Integer;
                                          pCtl: LONG);
var
  steps,
  val,
  i: Integer;

begin

  pCbx.Clear;

  Steps := (iMax - iMin) div iStep + 1; // +1 to include the maximum value in the combo box.
  val := iMin;

  for i := 0 to Steps - 1 do
    begin

      pCbx.Items.Append(IntToStr(val));
      inc(val,
          iStep);
    end;

  // Set the default value.
  i := pCbx.Items.IndexOf(IntToStr(iDefault));

  if (i <> -1) then
    pCbx.ItemIndex := i // If found, set the default index.
  else
    pCbx.ItemIndex := 0; // Fallback to the first item if default value not found.

  // Set the rest.
  pCbx.Enabled := True;

  if (pCbx.Name = 'cbxCameraValues') then
    begin
      fSelectedCamaraControl.cpCameraControlProperty := pCtl;
      cbxCameraControlProperty.ItemIndex := iContrPropIndex;
      cbxCameraControlFlags.ItemIndex := iFlags;
    end
  else // Video.
    begin
      fSelectedVideoControl.cpVideoProcAmpProperty := pCtl;
      cbxVideoControlProperty.ItemIndex := iContrPropIndex;
      cbxVideoControlFlags.ItemIndex := iFlags;
    end;
end;


procedure TFrm_SimpleCapture.butGetDeviceClick(Sender: TObject);
var
  hr: HRESULT;

begin

  if not Assigned(dlgChooseDevice) then
    Application.CreateForm(TdlgChooseDevice,
                           dlgChooseDevice);

  hr := EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                                 FDevicePropertiesArray);

  if SUCCEEDED(hr) then
    if (dlgChooseDevice.ShowModal = 111) then
      begin
        iSelectedDevice := dlgChooseDevice.iSelectedDevice;
        // Set the device
        hr := MfDeviceCapture.SetDevice(FDevicePropertiesArray[iSelectedDevice]);
        if FAILED(hr) then
          begin
            GetLastError();
            Exit;
          end;
      end;

  if (MfDeviceCapture.State = OpenPending) then
    butShowProperties.Enabled := True
  else
    butShowProperties.Enabled := False;

  // Get the device properties.
  hr := SetCameraProperties(4);  // Exposure.
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Set camera properties failed with error code: ' + IntToStr(hr) + ''),
                 lpcwstr('Camera properties Failure!'),
                         MB_ICONSTOP);
      Exit;
    end;

  hr := SetVideoProperties(0); // Brightness.
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Set video properties failed with error code: ' + IntToStr(hr) + ''),
                 lpcwstr('Video properties Failure!'),
                         MB_ICONSTOP);
    end;
end;


procedure TFrm_SimpleCapture.Button1Click(Sender: TObject);
begin

  Close();
end;


procedure TFrm_SimpleCapture.butShowPropertiesClick(Sender: TObject);
begin

  if (MfDeviceCapture.State = TopologyReady) and not pnlCameraAndVideoControl.Visible then
    pnlCameraAndVideoControl.Visible := True
  else
    pnlCameraAndVideoControl.Visible := False;
end;


procedure TFrm_SimpleCapture.SetCameraPropFlags();
begin

  case cbxCameraControlFlags.ItemIndex of
    0: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_AUTO);
    1: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL);
    2: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_ASYNCHRONOUS);
    3: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_ABSOLUTE);
    4: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_RELATIVE);
  end;
end;


procedure TFrm_SimpleCapture.SetVideoPropFlags();
begin

  case cbxVideoControlFlags.ItemIndex of
    0: fSelectedVideoControl.cpFlags := LONG(KSPROPERTY_VIDEOPROCAMP_FLAGS_AUTO);
    1: fSelectedVideoControl.cpFlags := LONG(KSPROPERTY_VIDEOPROCAMP_FLAGS_MANUAL);
  end;
end;


procedure TFrm_SimpleCapture.cbxCameraControlFlagsSelect(Sender: TObject);
begin

  SetCameraPropFlags();
  // Set changes.
  //
end;


procedure TFrm_SimpleCapture.cbxCameraControlPropertySelect(Sender: TObject);
begin
  if FAILED(SetCameraProperties(cbxCameraControlProperty.ItemIndex)) then
    MessageBox(0,
               lpcwstr('Could not get camera property, because the engine in not initialized.'),
               lpcwstr('Failure'),
                       MB_ICONSTOP);
end;


procedure TFrm_SimpleCapture.cbxCameraValuesSelect(Sender: TObject);
var
  hr: HRESULT;

begin

  fSelectedCamaraControl.cpValue := StrToInt(cbxCameraValues.Text);
  SetCameraPropFlags();

  hr := MfDeviceCapture.SetCameraProps(fSelectedCamaraControl);

  if FAILED(hr) and (hr <> E_POINTER) then
    MessageBox(0,
               lpcwstr('Could not set camera property.'),
               lpcwstr('Failure'),
                       MB_ICONSTOP);
end;


procedure TFrm_SimpleCapture.cbxVideoControlPropertySelect(Sender: TObject);
begin

  if FAILED(SetVideoProperties(cbxVideoControlProperty.ItemIndex)) then
    MessageBox(0,
               lpcwstr('Could not get camera property, because the engine in not initialized.'),
               lpcwstr('Failure'),
                       MB_ICONSTOP);
end;


procedure TFrm_SimpleCapture.cbxVideoValuesSelect(Sender: TObject);
var
  hr: HRESULT;

begin

  fSelectedVideoControl.cpValue := StrToInt(cbxVideoValues.Text);
  SetVideoPropFlags();

  hr := MfDeviceCapture.SetVideoProps(fSelectedVideoControl);

  if FAILED(hr) and (hr <> E_POINTER) then
    MessageBox(0,
               lpcwstr('Could not set video property.'),
               lpcwstr('Failure'),
                       MB_ICONSTOP);
end;


procedure TFrm_SimpleCapture.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin

  CanClose := False;
  // Before closing the app, clean up.
  QuitSession();
  CanClose := True;
end;


procedure TFrm_SimpleCapture.FormCreate(Sender: TObject);
var
  hr: HRESULT;

begin

  bAppIsClosing := False;
  bFullScreenMode := False;
  // Get the capture engine.
  hr := GetFmCapture();
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Initialization failed with error code: ' + IntToStr(hr) + ''),
                 lpcwstr('Initialization Failure!'),
                         MB_ICONSTOP);
      // Nothing to do, terminate the app.
      Application.Terminate();
    end;
end;


procedure TFrm_SimpleCapture.FormPaint(Sender: TObject);
var
  ps: PAINTSTRUCT;
  vHdc: HDC;

begin

  vHdc := 0;
  BeginPaint(Self.Handle,
             ps);

  try
    if Assigned(MfDeviceCapture) and (MfDeviceCapture.VideoDetected()) then
      MfDeviceCapture.UpdateVideo()
    else
      FillRect(vHdc,
               ps.rcPaint,
               HBRUSH(COLOR_APPWORKSPACE + 1));
  finally
    EndPaint(Self.Handle,
             ps);
  end;
end;


// Create the capture engine.
function TFrm_SimpleCapture.GetFmCapture(): HRESULT;
var
  hr: HRESULT;

begin

  // Quit previous session.
  if Assigned(MfDeviceCapture) then
    QuitSession();

  // We want the video to be played on the VideoPanel, so, we use that handle.
  hr := TMfCaptureEngine.CreateInstance(pnlVideo.Handle,   // The clipping window / control.
                                        Frm_SimpleCapture.Handle,  // The handle of the form that handle messages,
                                        MfDeviceCapture);          // must be main form or parent window !!!
  if FAILED(hr) then
    Exit(hr);

  // If you want to switch to a different clipping surface while the session is active:
  // MfDeviceCapture.SetVideoSurface := myFormOrControl.Handle;

  Result := hr;
end;


function TFrm_SimpleCapture.SetCameraProperties(pIndex: Integer): HRESULT;
begin

  Result := S_OK;

  if not Assigned(MfDeviceCapture) then
    Exit(E_POINTER);

  cbxCameraValues.Clear;
  cbxCameraValues.Items.Append('X'); // Not supported.
  cbxCameraValues.ItemIndex := 0;
  cbxCameraValues.Enabled := False;

  {
   In this order:
   Pan      = 0
   Tilt     = 1
   Roll     = 2
   Zoom     = 3
   Exposure = 4
   Iris     = 5
   Focus    = 6
  }

  case pIndex of
    0: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Pan.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Pan.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Pan.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Pan.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Pan.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Pan.prflags,
                          LONG(CameraControl_Pan));
           end;
       end;

    1: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Tilt.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Tilt.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Tilt.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Tilt.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Tilt.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Tilt.prflags,
                          LONG(CameraControl_Tilt));
           end;
       end;

    2: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Roll.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Roll.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Roll.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Roll.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Roll.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Roll.prflags,
                          LONG(CameraControl_Roll));
           end;
       end;

    3: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Zoom.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Zoom.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Zoom.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Zoom.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Zoom.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Zoom.prflags,
                          LONG(CameraControl_Zoom));
           end;
       end;

    4: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Exposure.IsSupported then
           begin
             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Exposure.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Exposure.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Exposure.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Exposure.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Exposure.prflags,
                          LONG(CameraControl_Exposure));
           end;
       end;

    5: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Iris.IsSupported then
           begin
             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Iris.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Iris.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Iris.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Iris.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Iris.prflags,
                          LONG(CameraControl_Iris));
           end;
       end;

    6: begin

         if MfDeviceCapture.CameraProperties.CameraCtl_Focus.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          MfDeviceCapture.CameraProperties.CameraCtl_Focus.prMax,
                          MfDeviceCapture.CameraProperties.CameraCtl_Focus.prMin,
                          MfDeviceCapture.CameraProperties.CameraCtl_Focus.prDelta,
                          MfDeviceCapture.CameraProperties.CameraCtl_Focus.prDefault,
                          MfDeviceCapture.CameraProperties.CameraCtl_Focus.prflags,
                          LONG(CameraControl_Focus));
           end;
       end;
  end;
end;


function TFrm_SimpleCapture.SetVideoProperties(pIndex: Integer): HRESULT;
begin

  Result := S_OK;

  if not Assigned(MfDeviceCapture) then
    Exit(E_POINTER);

  cbxVideoValues.Clear;
  cbxVideoValues.Items.Append('X');  // Not supported.
  cbxVideoValues.ItemIndex := 0;
  cbxVideoValues.Enabled := False;
  {
   In this order:
   Brightness = 0
   Contrast   = 1
   Hue        = 2
   Saturation = 3
   Sharpness  = 4
   Gamma      = 5
   ColorEnable = 6
   WhiteBalance = 7
   BacklightCompensation = 8
   Gain = 9
  }

  case pIndex of
    0: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Brightness.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    1: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Contrast.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Contrast.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Contrast.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Contrast.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Contrast.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    2: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Hue.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Hue.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Hue.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Hue.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Hue.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    3: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Saturation.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Saturation.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Saturation.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Saturation.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Saturation.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Saturation.prflags,
                          LONG(VideoProcAmp_Saturation));
           end;
       end;
    4: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Sharpness.prflags,
                          LONG(VideoProcAmp_Sharpness));
           end;
       end;
    5: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Gamma.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gamma.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gamma.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gamma.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gamma.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gamma.prflags,
                          LONG(VideoProcAmp_Gamma));
           end;
       end;
    6: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_ColorEnable.prflags,
                          LONG(VideoProcAmp_ColorEnable));
           end;
       end;
    7: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_WhiteBalance.prflags,
                          LONG(VideoProcAmp_WhiteBalance));
           end;
       end;
    8: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_BacklightCompensation.prflags,
                          LONG(VideoProcAmp_BacklightCompensation));
           end;
       end;
    9: begin
         if MfDeviceCapture.VideoProperties.VideoCtl_Gain.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gain.prMax,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gain.prMin,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gain.prDelta,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gain.prDefault,
                          MfDeviceCapture.VideoProperties.VideoCtl_Gain.prflags,
                          LONG(VideoProcAmp_Gain));
           end;
       end;
  end;
end;


procedure TFrm_SimpleCapture.HandleRotationChanged(Sender: TObject);
begin

  MfDeviceCapture.Rotation := StrToInt(cboRotation.Text);
end;


// Message listener.
// Listen for WM_DEVICECHANGE messages. The lParam message parameter is a pointer to a DEV_BROADCAST_HDR structure.
procedure TFrm_SimpleCapture.WMDeviceChange(var Msg: TMessage);
var
  hr: HResult;

begin

  hr := S_OK;

  if not Assigned(MfDeviceCapture) then
    Exit;

  if (Msg.lParam <> 0) then
    hr := MfDeviceCapture.CheckCaptureDeviceLost(PDEV_BROADCAST_HDR(Msg.LParam),
                                                 bDeviceLost);

  if (Failed(hr) or bDeviceLost) then
    begin
      QuitSession();
      // Show dialog with info about which device is disconnected.
      MessageDlg(Format('Lost capture device %s.', [FDevicePropertiesArray[iSelectedDevice].lpFriendlyName]),
                 mtError,
                 mbOKCancel,
                 0);

      // Update DevicePropertiesArray.
      EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                               FDevicePropertiesArray);
    end;
end;


procedure TFrm_SimpleCapture.pnlVideoResize(Sender: TObject);
var
  crD: TRECT;
  pcrD: LPRECT;

begin

  // Set video size.
  if Assigned(MfDeviceCapture) then
    begin
      crD.left := 0;
      crD.top := 0;
      crD.right := pnlVideo.ClientWidth;
      crD.bottom := pnlVideo.ClientHeight;
      pcrD := @crD;
      MfDeviceCapture.ResizeVideo(pcrD);
    end;
end;


// initialization and finalization =============================================


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();

end.
