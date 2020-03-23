// FactoryX
//
// Copyright �2003 - 2018 by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmFullScreenLayer.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 2.6.3
// Description: Requires Windows 7 or later.
//              This unit is used as fullscreen layer, if not using the EVR's
//              fullscreen option which is deprecated.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2019                     WIN10 May 2019 update (version 1903)
// 03/06/2019                     IS�K release.
// 18/06/2019                     Prodigy release.
// 24/12/2019                     Underworld release.
// ----------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: MfPack Samples 2.6.3
// Related projects: MfPackX263
// Known Issues: -
// Compiler version: 23 up to 33
// TODO: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: -
//
//
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
//                                                  Ramyses De Macedo Rodrigues
// Contributor(s): Ramyses De Macedo Rodrigues,
//                 Tony Kalf (maXcomX),
//                 Peter Larson (ozships).
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit frmFullScreenLayer;

interface

uses
  {WinApi}
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
  Vcl.Menus;

type
  TFullScreenLayer = class(TForm)
    PopupMenu1: TPopupMenu;
    Fullscreen1: TMenuItem;
    mnuEnableSubtitling: TMenuItem;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Fullscreen1Click(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);

  private
    { Private declarations }

  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
  Vcl.ClipBrd, MfPlayerClass, frmMfPlayer;



procedure TFullScreenLayer.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
    PopUpMenu1.Popup(X, Y);
end;


procedure TFullScreenLayer.Fullscreen1Click(Sender: TObject);
begin
  frm_MfPlayer.butFullScreenClick(Nil);
end;


// a sort of dejavu...
procedure TFullScreenLayer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  bm: TBitmap;

begin
case Key of
    VK_SPACE:   if Assigned(MfPlayer) then
                  begin
                    case MfPlayer.State of
                      Started: frm_MfPlayer.butPauseClick(Nil);
                      OpenPending: frm_MfPlayer.butPlayClick(Nil);
                      Stopped, Paused: frm_MfPlayer.butPlayClick(Nil);
                    end;

                  end;
    VK_END:     if Assigned(MfPlayer) then
                  begin
                    MfPlayer.SendPlayerRequest(reqStop);
                  end;
    VK_F11:     begin  //DestroyMfPlayer;   //Kick ass...
                  if Assigned(MfPlayer) then
                    MfPlayer.ShutDown();
                end;

    VK_F12:     frm_MfPlayer.muOpenClick(Nil);

    //take a snapshot and copy the bitmap to the clipboard
    VK_F8:      begin
                  bm := TBitmap.Create;
                  MfPlayer.TakeSnapShot(bm);
                  Clipboard.Assign(bm);
                  FreeAndNil(bm);
                end;

    // Use left and right arrows to adjust the volume.
    // since there is a trackbar, this one will have the focus.
    VK_LEFT:    begin
                  MfPlayer.SetVolume(MfPlayer.Volume - 0.1);
                end;

    VK_RIGHT:   begin
                  MfPlayer.SetVolume(MfPlayer.Volume + 0.1);
                end;

    VK_ESCAPE:  begin
                  frm_MfPlayer.butFullScreenClick(Nil);
                end;
  end;
end;

end.
