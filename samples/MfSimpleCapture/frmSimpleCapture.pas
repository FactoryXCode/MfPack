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
// Version: 2.6.4
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
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
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
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit frmSimpleCapture;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  //WinApi.ActiveX {opt},
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfApi,
  MfPack.MfPlay,
  MfPack.Dbt,
  MfPack.Ks,
  MfPack.KsMedia,
  MfPack.MfObjects,
  MfPack.MfpUtils,
  MfPack.ObjBase,
  MfPack.ComBaseApi,
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
    procedure pnlControlsClick(Sender: TObject);

  private
    { Private declarations }

    bAppIsClosing: Boolean;
    bFullScreenMode: Boolean;

    //Quits the session and releases the capture engine
    procedure QuitSession();
    function GetFmCapture(): HRESULT;

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


procedure TFrm_SimpleCapture.pnlControlsClick(Sender: TObject);
var
  bvc: DEV_BROADCAST_HDR;
  isLost: BOOL;

begin
  MfDeviceCapture.CheckCaptureDeviceLost (bvc, isLost);
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
