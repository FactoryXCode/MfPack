// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmMfPlayer.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 2.6.4
// Description: This is the basic class of MfPlayer,
//              containing the necessary methodes to play a mediafile
//              For indepth information see the included examples (CPlayer)
//              and text files containing the complete information about
//              MfPlayer.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 20H1)
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
// Source: Parts of CPlayer Examples
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

unit frmMfPlayer;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
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
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  {Project}
  MfPlayerClass;

type
  // By default the form is the videowindow.

  Tfrm_MfPlayer = class(TForm)
    dlgOpenUrl: TOpenDialog;
    MainMenu1: TMainMenu;
    muFile: TMenuItem;
    muOpen: TMenuItem;
    muSeparator1: TMenuItem;
    muExit: TMenuItem;
    procedure muOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure muExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    { Private declarations }

    procedure WMKEYDOWN (var msg: TMessage); message WM_KEYDOWN;

  public
    { Public declarations }
    MfPlayer: TMfPlayer;

  end;


var
  frm_MfPlayer: Tfrm_MfPlayer;


implementation

{$R *.dfm}

uses
  Vcl.ClipBrd;


procedure Tfrm_MfPlayer.muExitClick(Sender: TObject);
begin
  Close();
end;


procedure Tfrm_MfPlayer.muOpenClick(Sender: TObject);
begin
  if dlgOpenUrl.Execute then
    MfPlayer.OpenURL(PWideChar(dlgOpenUrl.Filename));
end;


procedure Tfrm_MfPlayer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(MfPlayer) then
    begin
      MfPlayer.Free;
      MfPlayer := Nil;
    end;
end;


procedure Tfrm_MfPlayer.FormCreate(Sender: TObject);
begin
  MfPlayer:= TMfPlayer.Create(frm_MfPlayer.Handle,
                              frm_MfPlayer.Handle);


  if (MfPlayer = Nil) then
    Abort();
end;


procedure Tfrm_MfPlayer.FormResize(Sender: TObject);
var
  crD: TRect;
  pcrD: LPRECT;

begin

  if Assigned(MfPlayer) then
    begin
     crD.left:= 0;
     crD.top:= 0;
     crD.right:= frm_MfPlayer.ClientWidth;
     crD.bottom:= frm_MfPlayer.ClientHeight;

     pcrD:= @crD;
     //Stop flickering of controls and subtitle when resizing.
     //SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);
     MfPlayer.ResizeVideo(pcrD);
     //SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
     //RedrawWindow(Handle,
     //             Nil,
     //             0,
     //             RDW_ERASE OR RDW_FRAME OR RDW_INVALIDATE OR RDW_ALLCHILDREN);
    end;
end;


procedure Tfrm_MfPlayer.WMKEYDOWN(var msg: TMessage);
var
  bm: TBitmap;

begin
  case Msg.WParam of

    VK_SPACE: if Assigned(MfPlayer) then
                case MfPlayer.GetState of
                  CmdStart: MfPlayer.SendPlayerCommand(CmdPause);
                  CmdOpenPending: MfPlayer.SendPlayerCommand(CmdStartPlayBack);
                  CmdStop, CmdPause: MfPlayer.SendPlayerCommand(CmdStart);
                end;

    VK_ESCAPE: if Assigned(MfPlayer) then
                 MfPlayer.SendPlayerCommand(CmdStop);

    VK_F12: muOpenClick(nil);

    VK_F11: MfPlayer := Nil;

    //take a snapshot and copy the bitmap to the clipboard
    VK_F8: begin
             bm:= TBitmap.Create;
             MfPlayer.TakeSnapShot(bm);
             Clipboard.Assign(bm);
             FreeAndNil(bm);
           end;

    VK_SUBTRACT: begin
                   MfPlayer.SetVolume(MfPlayer.Volume - 0.1);
                 end;

    VK_ADD: begin
              MfPlayer.SetVolume(MfPlayer.Volume + 0.1);
            end;
  end;
end;

end.

