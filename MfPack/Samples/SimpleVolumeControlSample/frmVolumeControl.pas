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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
    procedure FormCreate(Sender: TObject);
  private
    FRefreshing: Boolean;
    procedure RefreshVolume;
  public
    MfAudioEndPoint1: TMfAudioEndPoint;
  end;

var
  VolumeControl: TVolumeControl;

implementation

{$R *.dfm}

procedure TVolumeControl.FormCreate(Sender: TObject);
begin
  // Owner-managed runtime creation keeps the form usable without an IDE package.
  MfAudioEndPoint1 := TMfAudioEndPoint.Create(Self);
  MfAudioEndPoint1.Name := 'MfAudioEndPoint1';
  MfAudioEndPoint1.OnNotify := MfAudioEndPoint1Notify;
  MfAudioEndPoint1.FollowDefaultDevice := True;
  MfAudioEndPoint1.GetDefaultDevice;
  RefreshVolume;
end;


procedure TVolumeControl.RefreshVolume;
var
  MinDb: Single;
  MaxDb: Single;
  StepDb: Single;
begin
  FRefreshing := True;
  try
    tbScVolume.Min := 0;
    tbScVolume.Max := 1000;
    MfAudioEndPoint1.GetVolumeRange(MinDb,MaxDb,StepDb);
    tbDbVolume.Min := Trunc(MinDb * 10);
    tbDbVolume.Max := Round(MaxDb * 10);
    tbScVolume.Enabled := MfAudioEndPoint1.IMMDeviceInterface <> nil;
    tbDbVolume.Enabled := tbScVolume.Enabled and (MinDb < MaxDb);
    CheckBox1.Enabled := tbScVolume.Enabled;
    tbScVolume.Position := Round(MfAudioEndPoint1.MasterScalarVolume * 1000);
    tbDbVolume.Position := Round(MfAudioEndPoint1.MasterDbVolume * 10);
    CheckBox1.Checked := MfAudioEndPoint1.Mute;
    Edit1.Text := FormatFloat('0.0',MfAudioEndPoint1.MasterDbVolume);
    Edit2.Text := FormatFloat('0.000',MfAudioEndPoint1.MasterScalarVolume);
  finally
    FRefreshing := False;
  end;
end;


procedure TVolumeControl.butCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TVolumeControl.CheckBox1Click(Sender: TObject);
begin
  if FRefreshing then
    Exit;
  try
    MfAudioEndPoint1.Mute := CheckBox1.Checked;
  finally
    RefreshVolume;
  end;
end;


procedure TVolumeControl.MfAudioEndPoint1Notify(Sender: TObject;
                                             pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA);
begin
  if pNotify <> nil then
    RefreshVolume;
end;


procedure TVolumeControl.tbScVolumeChange(Sender: TObject);
begin
  if FRefreshing then
    Exit;
  try
    MfAudioEndPoint1.MasterScalarVolume := tbScVolume.Position / 1000;
  finally
    RefreshVolume;
  end;
end;


procedure TVolumeControl.tbDbVolumeChange(Sender: TObject);
begin
  if FRefreshing then
    Exit;
  try
    MfAudioEndPoint1.MasterDbVolume := tbDbVolume.Position / 10;
  finally
    RefreshVolume;
  end;
end;

end.
