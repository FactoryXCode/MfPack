// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - EndpointVolume
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmVolumeControl.pas
// Kind: Pascal / Delphi unit
// Release date: 04-07-2025
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Sample how to use the Audio Endpoint Volume API.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: endpointvolume.h
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
unit frmVolumeControl;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  MfAudioEndPoint,
  WinApi.CoreAudioApi.EndPointVolume, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TVolumeControl = class(TForm)
    MfAudioEndPoint1: TMfAudioEndPoint;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    tbDbVolume: TTrackBar;
    tbScVolume: TTrackBar;
    butClose: TButton;
    procedure MfAudioEndPoint1Notify(Sender: TObject;
      pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA);
    procedure tbScVolumeChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure tbDbVolumeChange(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VolumeControl: TVolumeControl;

implementation

{$R *.dfm}

procedure TVolumeControl.butCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TVolumeControl.CheckBox1Click(Sender: TObject);
begin
  MfAudioEndPoint1.Mute := BOOL(CheckBox1.Checked);
end;


procedure TVolumeControl.MfAudioEndPoint1Notify(Sender: TObject;
  pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA);
begin
 // Bogus, just for checking the pointer values.
 if (pNotify = Nil) then
   Exit;

end;


procedure TVolumeControl.tbScVolumeChange(Sender: TObject);
var
  sVal: Single;

begin
  sVal := tbScVolume.Position / 10;
  MfAudioEndPoint1.MasterScalarVolume := sVal;
  Edit2.Text := sVal.ToString();

end;


procedure TVolumeControl.tbDbVolumeChange(Sender: TObject);
var
  sVal: Single;

begin
  sVal := tbDbVolume.Position / 10;
  MfAudioEndPoint1.MasterDBVolume := sVal;
  Edit1.Text := sVal.ToString();
end;

end.
