// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmDuckingMediaPlayer.pas
// Kind: Pascal Unit
// Release date: 05-07-2020
// Language: ENU
//
// Version: 3.1.6
// Description: This sample implements a simple media player that responds to the "ducking"
//              feature in Windows 7 and higher.
//              It also implements a volume control which tracks
//              to the volume control in the volume mixer.
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//          This sample implements a simple media player that responds to the "ducking"
//          feature in Windows 7 and higher.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Ducking Media Player Sample Project
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
unit frmDuckingMediaPlayer;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  WinApi.WinApiTypes,
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
  {Project}
  MediaPlayer;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edFileName: TEdit;
    butBrowse: TButton;
    Bevel1: TBevel;
    Label2: TLabel;
    trbPlayBackPosition: TTrackBar;
    Bevel2: TBevel;
    butPlay: TButton;
    butPause: TButton;
    butStop: TButton;
    Label3: TLabel;
    Bevel3: TBevel;
    cbCheckPauseOnDuck: TCheckBox;
    cbOptOutOnDucking: TCheckBox;
    tbVolumeSlider: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    butOK: TButton;
    butCancel: TButton;
    cbCheckMute: TCheckBox;
    dlgOpen: TOpenDialog;
    tmrProgress: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure butOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbCheckPauseOnDuckClick(Sender: TObject);
    procedure cbOptOutOnDuckingClick(Sender: TObject);
    procedure butBrowseClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure tbVolumeSliderChange(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure cbCheckMuteClick(Sender: TObject);
  private
    { Private declarations }
    AppHandle: HWND;
    volume: Single;
    mute: BOOL; // = LongBool
    fileName: WideString;
    //progressTimer: UIntPtr;

    procedure MediaPlayerDialogProc(var Msg: TMsg; var Handled: Boolean);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  g_MediaPlayer: TCMediaPlayer;

implementation

{$R *.dfm}

//
//  If the user hit the "Browse" button, bring up the file common dialog box.
//
//  If the user hit "OK" to the dialog then update the edit control to include the filename and load
//  the file into the player.
//
procedure TForm1.butBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    begin
      trbPlayBackPosition.Max := 1000;
      edFileName.Text := dlgOpen.Filename;
      fileName := dlgOpen.FileName;

      //
      //  If we're playing (the stop button is enabled), stop playing.
      //
      if butStop.Enabled then
        begin
          g_MediaPlayer.Stop();

          tmrProgress.Enabled := False;
          //KillTimer(AppHandle, progressTimer);

          ButPlay.Enabled := True;
          //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
          butPause.Enabled := False;
          //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
          butStop.Enabled := False;
          //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
        end;

      g_MediaPlayer.SetFileName(PWideChar(fileName));

      butPlay.Enabled := True;
      //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
      butPause.Enabled := False;
      //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
      butStop.Enabled := False;
      //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
    end;
end;

//
//  The user hit the "Pause/Continue" button.
//
//  Toggle the "Pause" state in the player and update the button text as appropriate.
//
procedure TForm1.butPauseClick(Sender: TObject);
begin
  if g_MediaPlayer.TogglePauseState() then
    begin
      butPause.Caption := 'Continue';
      //SetWindowText(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), lpcwstr('Continue'));
      tmrProgress.Enabled := False;
      //KillTimer(AppHandle, progressTimer);

    end
  else
    begin
      butPause.Caption := 'Pause';
      //SetWindowText(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), lpcwstr('Pause'));
      tmrProgress.Enabled := True;
      //SetTimer(AppHandle, progressTimer, 40, Nil);
    end;
end;

//
//  The user hit the "Play" button.
//
//  Sync the "Pause On Duck" and "Ducking Opt Out" buttons with the player and then start playback.
//
//
//  Then disable the "Play" button and enable the "Pause" and "Stop" buttons.
//
procedure TForm1.butPlayClick(Sender: TObject);
begin
  g_MediaPlayer.SyncPauseOnDuck(cbCheckPauseOnDuck.Checked);
  g_MediaPlayer.SyncDuckingOptOut(cbOptOutOnDucking.Checked);

  g_MediaPlayer.Play();

  tmrProgress.Enabled := True;
  //SetTimer(AppHandle, progressTimer, 40, Nil);

  ButPlay.Enabled := False;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
  butPause.Enabled := True;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
  butStop.Enabled := True;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
end;


procedure TForm1.butStopClick(Sender: TObject);
begin
  g_MediaPlayer.Stop();

  tmrProgress.Enabled := False;
  //KillTimer(AppHandle, progressTimer);

  ButPlay.Enabled := True;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
  butPause.Enabled := False;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
  butStop.Enabled := False;
  //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
end;


procedure TForm1.butOKClick(Sender: TObject);
begin
  Close(); // Close the dialog or use this method
  // EndDialog(AppHandle, True);
end;


procedure TForm1.cbCheckMuteClick(Sender: TObject);
begin
  g_MediaPlayer.SetMute(cbCheckMute.Checked);
end;


procedure TForm1.cbCheckPauseOnDuckClick(Sender: TObject);
begin
  g_MediaPlayer.SyncPauseOnDuck(cbCheckPauseOnDuck.Checked);
end;


procedure TForm1.cbOptOutOnDuckingClick(Sender: TObject);
begin
  g_MediaPlayer.SyncDuckingOptOut(cbOptOutOnDucking.Checked);
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  g_MediaPlayer.Shutdown();
  FreeAndNil(g_MediaPlayer);
  CanClose := True;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  hr: HResult;

begin
  AppHandle := Application.Handle;
  Application.OnMessage := MediaPlayerDialogProc;

  g_MediaPlayer := TCMediaPlayer.Create(AppHandle);

  if not Assigned(g_MediaPlayer) then
    begin
      MessageBox(AppHandle,
                 lpcwstr('Unable to allocate media player'),
                 lpcwstr('Initialization Failure'),
                 MB_OK);
      Exit;
      // or use EndDialog(AppHandle, -1);
    end
  else
    begin
      hr := g_MediaPlayer.Initialize();
      if Failed(hr) then
        begin
          MessageBox(AppHandle,
                     lpcwstr('Unable to initialize media player'),
                     lpcwstr('Initialization Failure'),
                     MB_OK);
          Exit;
          // or use EndDialog(AppHandle, -1);
        end;
    end;

  // Set progress max
  trbPlayBackPosition.Max := g_MediaPlayer.Duration;

  // Get&set volume and mute
  volume := g_MediaPlayer.GetVolume();
  tbVolumeSlider.Position := Trunc(volume * 100.0);
  mute := g_MediaPlayer.GetMute();
  cbCheckMute.Checked := mute;
end;

//
// Update the volume for the media player.
//
procedure TForm1.tbVolumeSliderChange(Sender: TObject);
var
  sVolumePosition: Single;
  sVolume: Single;

begin
  sVolumePosition := tbVolumeSlider.Position;
  sVolume := (sVolumePosition / 100);
  g_MediaPlayer.SetVolume(sVolume);
end;

//
// Update the progress slider to match the current playback position.
// NOTE:
//  We used the TTimerclass which use the Windows API timer functions SetTimer and KillTimer as been used in the official MS sample.
//  Since Delphi doesn't have a slider component, we use the TTrackBar for this. Using a TProgressBar is also used (see player samples).
procedure TForm1.tmrProgressTimer(Sender: TObject);
var
  position: LongInt;

begin
  position := g_MediaPlayer.GetPosition();
  trbPlayBackPosition.Position := position;
end;

//
//  FUNCTION: MediaPlayerDialogProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
procedure TForm1.MediaPlayerDialogProc(var Msg: TMsg; var Handled: Boolean);
var
  slposition: LongInt;

begin

 if Msg.hwnd = Application.Handle then
  begin

    if Msg.message = WM_TIMER then
      begin
        //
        //  Update the progress slider to match the current playback position.
        //
        slposition := g_MediaPlayer.GetPosition();
        trbPlayBackPosition.Position := slposition;
        // or
        //SendMessage(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TBM_SETPOS, True, position);

        Handled := True;
        Exit;
      end;

    //
    //  Let the media player know about the DShow graph event.
    //  If we come to the end of the track, reset the slider to the beginning of the track.
    //
    if Msg.message = WM_APP_GRAPHNOTIFY then
      begin
        if g_MediaPlayer.HandleGraphEvent() then
          begin
            //  Reset the slider and timer, we're at the end of the track.
            trbPlayBackPosition.Position := 0;
            //SendMessage(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TBM_SETPOS, True, 0);
            tmrProgress.Enabled := False;
            //KillTimer(AppHandle, progressTimer);

            ButPlay.Enabled := True;
            //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), TRUE);
            butPause.Enabled := False;
            //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
            butStop.Enabled := False;
            //EnableWindow(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), FALSE);
          end;

        Handled := True;
        Exit;
      end;

    //
    //  Called when the media player receives a ducking notification.  Lets the media player know that the session has been ducked and pauses the player.
    //
    if Msg.message = WM_APP_SESSION_DUCKED then
      begin
        if g_MediaPlayer.Pause() then
          begin
            //butPause.Click;
            // or
            SetWindowText(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), 'Continue');
            tmrProgress.Enabled := False;
            //KillTimer(hWnd, progressTimer);
          end;
      end;

    if Msg.message = WM_APP_SESSION_UNDUCKED then
      begin
        if g_MediaPlayer.Continue() then
          begin
            // butPause.Click
            // or
            SetWindowText(GetDlgItem(AppHandle, GetDlgCtrlID(AppHandle)), 'Pause');
            tmrProgress.Enabled := True;
            //SetTimer(AppHandle, progressTimer, 40, Nil);
            Handled := True;
            Exit;
          end;
      end;

  end;
  Handled := False;
end;

end.
