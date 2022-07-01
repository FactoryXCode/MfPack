// FactoryX
//
// Copyright © by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: dlgSelDevice.pas
// Kind: Pascal Unit
// Release date: 08-03-2018
// Language: ENU
//
// Version: 3.1.1
//
// Description: Select device dialog.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/06/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX312
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: MFCaptureD3D Sample
//         main.cpp : Select device dialog.
//
// Copyright (c) 1997-2018 Microsoft Corporation. All rights reserved
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
//
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit dlgSelDevice;

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  {Project}
  Preview;

type
  TdlgSelectDevice = class(TForm)
    butOk: TButton;
    butCancel: TButton;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    procedure ComboBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgSelectDevice: TdlgSelectDevice;



implementation

{$R *.dfm}

procedure TdlgSelectDevice.ComboBox1Click(Sender: TObject);
begin
  param.selection := ComboBox1.ItemIndex;
  param.sSelection := LPWSTR(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

end.
