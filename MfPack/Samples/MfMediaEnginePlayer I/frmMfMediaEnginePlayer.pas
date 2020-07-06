// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfMediaEnginePlayer.pas
// Kind: Pascal Unit
// Release date: 24-04-2019
// Language: ENU
//
// Version: 2.6.4
// Description: This player version is based on the IMFMediaEngine interface,
//
// Company: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright © FactoryX, Netherlands/Australia. All rights reserved.
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit frmMfMediaEnginePlayer;

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
  MfPack.MfpUtils,
  MfPack.MfpTypes,
  MfPack.MfApi,
  MfPack.MfpMetLib,  // replacement for MfAdditional/MfMethods.pas
  {Project}
  MediaEngineClass;

type
  TFeMediaEnginePlayer = class(TForm)
    pnlControls: TPanel;
    butStop: TButton;
    butPause: TButton;
    butPlay: TButton;
    trbVolume: TTrackBar;
    prbProgress: TProgressBar;
    butFullScreen: TButton;
    pnlVideo: TPanel;
    MainMenu1: TMainMenu;
    muFile: TMenuItem;
    muOpen: TMenuItem;
    muSeparator1: TMenuItem;
    muExit: TMenuItem;
    muExtra: TMenuItem;
    mnuTakeScreenshot: TMenuItem;
    dlgOpenUrl: TOpenDialog;
    trbBalance: TTrackBar;
    cbMute: TCheckBox;

    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure muOpenClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butStopClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure trbVolumeChange(Sender: TObject);
    procedure prbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure butFullScreenClick(Sender: TObject);
    procedure mnuTakeScreenshotClick(Sender: TObject);
    procedure trbBalanceChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure trbBalanceKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure muExitClick(Sender: TObject);
    procedure cbMuteClick(Sender: TObject);

  private
    { Private declarations }
    bIsFullScreen: Boolean;

    procedure OnMeTimerUpdate(var Msg: TMsg); message WM_TIMERUPDATE;

  public
    { Public declarations }

    procedure SetWindowStyle(bFullScreen: Boolean);
    procedure ResetInterface();
    procedure RealignInterface();
    function ReleaseEngine(): HResult;
  end;


var
  FeMediaEnginePlayer: TFeMediaEnginePlayer;
  gi_MediaEngine: TcMediaEngine;


implementation

{$R *.dfm}

uses
  Vcl.ClipBrd;



procedure TFeMediaEnginePlayer.ResetInterface();
begin
  prbProgress.Position:= 0;
  pnlControls.Enabled:= False;
  mnuTakeScreenshot.Enabled:= False;
end;


procedure TFeMediaEnginePlayer.trbVolumeChange(Sender: TObject);
begin
  if not Assigned(gi_MediaEngine) then
    Exit;

  // Because trackbar has no MouseUp event, jou can use this trick.
  // if GetAsyncKeyState(VK_LBUTTON) = 0 then

  gi_MediaEngine.SetVolume(trbVolume.Position * 0.01);

end;


procedure TFeMediaEnginePlayer.trbBalanceChange(Sender: TObject);
begin
  if not Assigned(gi_MediaEngine) then
    Exit;
  gi_MediaEngine.SetBalance(trbBalance.Position * 0.01);
end;


// Reset balance
procedure TFeMediaEnginePlayer.trbBalanceKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) then
    trbBalance.Position := 0;
end;


procedure TFeMediaEnginePlayer.RealignInterface();
var
  crD: TRect;

begin

  if (prbProgress <> Nil) then
    prbProgress.Max:= prbProgress.Width;

  // Set video size
  if Assigned(gi_MediaEngine) then
    begin
      crD.left:= 0;
      crD.top := 0;
      crD.right := pnlVideo.ClientWidth;
      crD.bottom := pnlVideo.ClientHeight;
      gi_MediaEngine.ResizeVideo(crD);
    end;
end;


function TFeMediaEnginePlayer.ReleaseEngine(): HResult;
begin
  if Assigned(gi_MediaEngine) then
    begin
      // To safely release all objects, you need to call this function first!
      gi_MediaEngine.Flush();
      gi_MediaEngine.Free;
      gi_MediaEngine := Nil;
      Result := S_OK;
    end
  else
    Result := E_POINTER;
end;


// There is no way of getting a picture frame while the MediaEngine is in rendering mode.
// As work a round we take a 1:1 snapshot from the video surface.
procedure TFeMediaEnginePlayer.mnuTakeScreenshotClick(Sender: TObject);
var
  rc: Trect;
  ScrCanvas: TCanvas;
  bm: TBitmap;

begin

  if Assigned(gi_MediaEngine) then
    if gi_MediaEngine.pu_RenderingState in [rsPlaying, rsPaused] then
      begin

        rc := pnlVideo.BoundsRect;

        MapWindowPoints(Handle,
                        HWND_DESKTOP,
                        rc, // A pointer to an array of POINT structures that contain the set of points to be converted. The points are in device units. This parameter can also point to a RECT structure, in which case the cPoints parameter should be set to 2.
                        2);
        // Create canvas
        ScrCanvas := TCanvas.Create;
        // Create bitmap
        bm := TBitmap.Create();
      try
        // Get canvas handle
        ScrCanvas.Handle := GetDC(0);
        // Set bitmap properties
        bm := TBitmap.Create();
        bm.PixelFormat := pf32bit;
        bm.Width := pnlVideo.Width;
        bm.Height := pnlVideo.Height;

        bm.Canvas.CopyRect(bm.Canvas.ClipRect,
                           ScrCanvas,
                           rc);

        ClipBoard.Assign(bm);

      finally
        bm.free;
        ReleaseDC(0,
                  ScrCanvas.Handle);
        ScrCanvas.Handle := 0;
        ScrCanvas.Free;
      end;
  end;
end;


procedure TFeMediaEnginePlayer.muExitClick(Sender: TObject);
begin
  Close();
end;


procedure TFeMediaEnginePlayer.muOpenClick(Sender: TObject);
var
  hr: HResult;

begin
  // End and release any previous session first!
  ReleaseEngine();

  gi_MediaEngine := TcMediaEngine.Create(pnlVideo.Handle,
                                         Self.Handle,
                                         Self.WindowHandle,
                                         hr);

  if FAILED(hr) then
    begin
      Close();
    end;

  // Get the mediafile to play
  if dlgOpenUrl.Execute then
    begin
      hr := gi_MediaEngine.OpenURL(PWideChar(dlgOpenUrl.Filename));
    end
  else // User pressed cancel
    ReleaseEngine();
end;


procedure TFeMediaEnginePlayer.prbProgressMouseUp(Sender: TObject; Button: TMouseButton;
                                                  Shift: TShiftState; X, Y: Integer);
var
  fPos: Double;

begin
  if (X <= 0) then
    fPos := 0.0
  else
    fPos := ((X / prbProgress.Width) * (gi_MediaEngine.pu_Duration));

  gi_MediaEngine.SetPosition(fPos); // set new StartPosition
  // You might ask: Why div 100?
  // The answer is: The callback timer resolution is hns/100 = 100 milliseconds or 1/10 of a second
  //prbProgress.Position := X div 100;
end;


procedure TFeMediaEnginePlayer.SetWindowStyle(bFullScreen: Boolean);
begin
  if bFullScreen = True then
    begin
      BorderStyle := bsNone;
      pnlControls.Visible := False;
      muFile.Visible := False;
      muExtra.Visible := False;
      WindowState := wsMaximized;
      bIsFullScreen := True;
    end
  else
    begin
      bIsFullScreen := False;
      pnlControls.Visible := True;
      muFile.Visible := True;
      muExtra.Visible := True;
      WindowState := wsNormal;
      BorderStyle := bsSizeable;
    end;
end;

// Get to fullscreen
// user needs to hit ESC button to go back to windowed mode
procedure TFeMediaEnginePlayer.butFullScreenClick(Sender: TObject);
begin
  if bIsFullScreen then
    SetWindowStyle(False)
  else
    SetWindowStyle(True);
end;


// Pause
procedure TFeMediaEnginePlayer.butPauseClick(Sender: TObject);
begin
  {void} gi_MediaEngine.Pause();
end;


// Play
procedure TFeMediaEnginePlayer.butPlayClick(Sender: TObject);
begin
  if not Assigned(gi_MediaEngine) then
    Exit;

  if FAILED(gi_MediaEngine.Play()) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying to play the media file.'),
                 LPCWSTR('Error'),
                 MB_ICONEXCLAMATION);
    end
  else
    mnuTakeScreenshot.Enabled:= True;
end;


procedure TFeMediaEnginePlayer.butStopClick(Sender: TObject);
begin
  // Note: This calls: Pause()
  //       The file pointer will be set to 0.0
  //       So, if hitting Play, the file will start at position 0.0
  //       See explanation at method gi_MediaEngine.Stop()
  {void} gi_MediaEngine.Stop();
  mnuTakeScreenshot.Enabled:= False;
  prbProgress.Position:= 0;
end;


procedure TFeMediaEnginePlayer.cbMuteClick(Sender: TObject);
var
  bchecked: Bool;

begin
  if Assigned(gi_MediaEngine) then
    begin
      bchecked:= cbMute.Checked;
      if Succeeded(gi_MediaEngine.Mute(bchecked)) then
        cbMute.Checked:= bchecked;
    end;
end;


procedure TFeMediaEnginePlayer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  if Assigned(gi_MediaEngine) then
    begin
      if Succeeded(ReleaseEngine()) then
        CanClose := True
      else
        begin
          MessageBox(0,
                     LPCWSTR('Unable to release the MediaEngine.'),
                     LPCWSTR('Error!'),
                     MB_ICONEXCLAMATION);
          Application.Terminate;
        end;
    end
  else
    CanClose := True;
end;


procedure TFeMediaEnginePlayer.OnMeTimerUpdate(var Msg: TMsg);
begin
  if Not Assigned(gi_MediaEngine) then
    Exit;
  if gi_MediaEngine.pu_Duration > 0 then
    prbProgress.Position := Trunc((prbProgress.Width / gi_MediaEngine.pu_Duration) *
                                  gi_MediaEngine.pu_CurrPosition);
end;


procedure TFeMediaEnginePlayer.FormCreate(Sender: TObject);
begin
  bIsFullScreen := False;
  RealignInterface();
end;


procedure TFeMediaEnginePlayer.FormKeyUp(Sender: TObject; var Key: Word;
                               Shift: TShiftState);
begin
  case Key of
    // Play/pause
    VK_SPACE:   if Assigned(gi_MediaEngine) then
                  begin
                    case gi_MediaEngine.pu_RenderingState of
                      rsPlaying: butPauseClick(Self);
                      rsStopped, rsPaused: butPlayClick(Self);
                    end;
                  end;
    // Stop
    VK_END:     if Assigned(gi_MediaEngine) then
                  begin
                   butStopClick(Self);
                  end;
    // Exit
    VK_F11:       begin
                    Close();
                  end;
    // Open file
    VK_F12:     begin muOpenClick(Self); end;

    // Take a snapshot and copy the bitmap to the clipboard
    VK_F8:      begin
                  // See comments at mnuTakeScreenshotClick()
                  mnuTakeScreenshotClick(Self);
                  // See: https://docs.microsoft.com/windows/desktop/api/mfmediaengine/nf-mfmediaengine-imfmediaengine-transfervideoframe
                end;

    // Use left and right arrows to adjust the volume.
    // since there is a trackbar, this one will have the focus.
    // We did not implement a balance method.
    VK_LEFT:    begin
                  trbVolume.Position := trbVolume.Position - 1;
                  if Assigned(gi_MediaEngine) then
                    gi_MediaEngine.SetVolume(trbVolume.Position * 0.01);
                end;

    VK_RIGHT:   begin
                  trbVolume.Position := trbVolume.Position + 1;
                  if Assigned(gi_MediaEngine) then
                    gi_MediaEngine.SetVolume(trbVolume.Position * 0.01);
                end;

    VK_ESCAPE:  begin
                  butFullScreenClick(Self);
                end;
  end;
end;


procedure TFeMediaEnginePlayer.FormResize(Sender: TObject);
begin
  if Assigned(gi_MediaEngine) then
    RealignInterface();
end;


end.
