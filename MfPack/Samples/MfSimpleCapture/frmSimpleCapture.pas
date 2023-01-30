// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmSimpleCapture.pas
// Kind: Pascal Unit
// Release date: 08-02-2018
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description: This is the basic class of MfSimpleCapture,
//              containing the necessary methodes to capture media streams.
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// =============================================================================
// Source: Parts of CPreview Examples.
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
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfPlay,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  frmdlgChooseDevice,
  MfDeviceCaptureClass;

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


type

  TFrm_SimpleCapture = class(TForm)
    pnlControls: TPanel;
    Button1: TButton;
    butGetDevice: TButton;
    pnlVideo: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure butGetDeviceClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private declarations }

    bAppIsClosing: Boolean;
    bFullScreenMode: Boolean;

    // Quits the session and releases the capture engine
    procedure QuitSession();
    function GetFmCapture(): HRESULT;
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
    if SUCCEEDED(MfDeviceCapture.ShutDownEngine()) then
      begin
        MfDeviceCapture.Free();
        MfDeviceCapture := Nil;
      end;
end;


procedure TFrm_SimpleCapture.butGetDeviceClick(Sender: TObject);
begin
  Application.CreateForm(TdlgChooseDevice, dlgChooseDevice);
end;


procedure TFrm_SimpleCapture.Button1Click(Sender: TObject);
begin
  Close();
end;


procedure TFrm_SimpleCapture.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // Before closing the app, clean up.
  QuitSession();
  CoUninitialize;
  CanClose:= True;
end;

procedure TFrm_SimpleCapture.FormCreate(Sender: TObject);
var
  hr: HRESULT;

begin
  bAppIsClosing := False;
  bFullScreenMode := False;

  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED);

  if SUCCEEDED(hr) then
    // Get the capture engine
    GetFmCapture();
end;


procedure TFrm_SimpleCapture.FormPaint(Sender: TObject);
var
  ps: PAINTSTRUCT;
  vHdc: HDC;

begin
  vHdc := 0;
  BeginPaint(Self.Handle, ps);

  try
    if Assigned(MfDeviceCapture) AND (MfDeviceCapture.VideoDetected()) then
      MfDeviceCapture.UpdateVideo()
    else
      FillRect(vHdc,
               ps.rcPaint,
               HBRUSH(COLOR_APPWORKSPACE + 1));
  finally
    EndPaint(Self.Handle, ps);
  end;
end;


// Create the capture engine
function TFrm_SimpleCapture.GetFmCapture(): HRESULT;
begin
  Result:= E_FAIL;
  if not Assigned(MfDeviceCapture) then
    begin
      MfDeviceCapture:= Nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      TMfCaptureEngine.CreateInstance(pnlVideo.Handle,       // The clipping window / control
                                      Frm_SimpleCapture.Handle,
                                      MfDeviceCapture);  // Must be main form or parent window !!!
      // If you want to switch to a different clipping surface while the session is active:
      // MfDeviceCapture.SetVideoSurface := myFormOrControl.Handle;
      Result := S_OK;
    end;
end;

// Message listener
// Listen for WM_DEVICECHANGE messages. The lParam message parameter is a pointer to a DEV_BROADCAST_HDR structure.
procedure TFrm_SimpleCapture.WMDeviceChange(var Msg: TMessage);
var
  hr: HResult;

begin
  hr := S_OK;

  if Not Assigned(MfDeviceCapture) then
    Exit;

  if (Msg.lParam <> 0) then
    hr := MfDeviceCapture.CheckCaptureDeviceLost(PDEV_BROADCAST_HDR(Msg.LParam),
                                                 bDeviceLost);

  if (Failed(hr) or bDeviceLost) then
    begin
      MfDeviceCapture.ShutDownEngine();
      MessageDlg('Lost the capture device.',
                 mtError,
                 mbOKCancel,
                 0);
    end;
end;


procedure TFrm_SimpleCapture.pnlVideoResize(Sender: TObject);
var
  crD: TRECT;
  pcrD: LPRECT;

begin
  // Set video size
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

end.
