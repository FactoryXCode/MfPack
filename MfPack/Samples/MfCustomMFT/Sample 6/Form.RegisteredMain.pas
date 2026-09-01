// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.RegisteredMain.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Sample 6: Media Session playback through the MFT registered by Sample 5.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// =============================================================================
// Source: Microsoft Learn.
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit Form.RegisteredMain;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  {Application}
  RegisteredTopologyPlayer;

type
  TfrmMain = class(TForm)
    pnlCommands: TPanel;
    btnOpen: TButton;
    btnPlayPause: TButton;
    btnStop: TButton;
    lblState: TLabel;
    pnlVideo: TPanel;
    memLog: TMemo;
    dlgOpenVideo: TOpenDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);

  private

    FPlayer: ITopologyPlayer;
    FSession: Cardinal;
    FReady: Boolean;
    FPlaying: Boolean;

    procedure WMTopologyEvent(var message: TMessage); message WM_TOPOLOGY_PLAYER_EVENT;

    procedure ClosePlayer(const ADetachWindow: Boolean);
    procedure OpenVideo(const AFileName: string);
    procedure LogError(const E: Exception);
  end;

var
  frmMain: TfrmMain;


implementation

{$R Form.Main.dfm}


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  Caption := 'MfCustomMFT Sample 6 - Playback through the registered MFT DLL';
  DoubleBuffered := True;
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;

  lblState.Caption := 'No video loaded';

  dlgOpenVideo.Filter := 'Video files|*.mp4;*.m4v;*.mov;*.wmv;*.avi|All files|*.*';
  memLog.Lines.Add('Open a video to discover and use the registered grayscale MFT.');
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Msg: TMsg;

begin

  ClosePlayer(True);

  while PeekMessage(Msg,
                    Handle,
                    WM_TOPOLOGY_PLAYER_EVENT,
                    WM_TOPOLOGY_PLAYER_EVENT,
                    PM_REMOVE) do
    TTopologyEventNotice(Pointer(Msg.LParam)).Free;
end;


procedure TfrmMain.ClosePlayer(const ADetachWindow: Boolean);
begin

  FReady := False;
  FPlaying := False;
  btnPlayPause.Caption := 'Play';
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;

  if Assigned(FPlayer) then
    FPlayer.Close(ADetachWindow);

  FPlayer := nil;
  Inc(FSession);
end;


procedure TfrmMain.OpenVideo(const AFileName: string);
begin

  ClosePlayer(False);

  memLog.Clear();
  memLog.Lines.Add(ExtractFileName(AFileName));
  memLog.Lines.Add('Enumerating registered RGB32 video effects...');

  lblState.Caption := 'Discovering registered MFT...';

  FPlayer := CreateTopologyPlayer(pnlVideo.Handle,
                                  Handle,
                                  FSession);
  FPlayer.Open(AFileName);
end;


procedure TfrmMain.LogError(const E: Exception);
begin

  lblState.Caption := 'Error';
  memLog.Lines.Add(E.Message);
end;


procedure TfrmMain.WMTopologyEvent(var message: TMessage);
var
  Notice: TTopologyEventNotice;

begin

  Notice := TTopologyEventNotice(Pointer(message.LParam));

  try
    if not Assigned(Notice) or (Notice.Session <> FSession) then
      Exit;

    memLog.Lines.Add(Notice.Text);

    case Notice.Kind of
      tekTopologyReady:
        begin
          FReady := True;
          FPlaying := False;
          btnPlayPause.Enabled := True;
          btnStop.Enabled := True;
          lblState.Caption := 'Registered MFT topology ready';
          FPlayer.ResizeVideo(pnlVideo.ClientWidth,
                              pnlVideo.ClientHeight);
        end;
      tekStarted:
        begin
          FPlaying := True;
          btnPlayPause.Caption := 'Pause';
          lblState.Caption := 'Playing through registered DLL';
        end;
      tekPaused:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Paused';
        end;
      tekStopped:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Stopped';
        end;
      tekEnded:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'End of video';
        end;
      tekError:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Media Session error';
        end;
    end;

  finally
    Notice.Free;
  end;
end;


procedure TfrmMain.btnOpenClick(Sender: TObject);
begin

  if not dlgOpenVideo.Execute then
    Exit;

  try
    OpenVideo(dlgOpenVideo.FileName);
  except
    on E: Exception do
      begin
        ClosePlayer(False);
        LogError(E);
      end;
  end;
end;


procedure TfrmMain.btnPlayPauseClick(Sender: TObject);
begin

  if not FReady or not Assigned(FPlayer) then
    Exit;

  try
    if FPlaying then
      FPlayer.Pause()
    else
      FPlayer.Start();
  except
    on E: Exception do
      LogError(E);
  end;
end;


procedure TfrmMain.btnStopClick(Sender: TObject);
begin

  if not FReady or not Assigned(FPlayer) then
    Exit;

  try
    FPlayer.Stop();
  except
    on E: Exception do
      LogError(E);
  end;
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin

  if Assigned(FPlayer) then
    FPlayer.ResizeVideo(pnlVideo.ClientWidth,
                        pnlVideo.ClientHeight);
end;


procedure TfrmMain.FormPaint(Sender: TObject);
begin

  if Assigned(FPlayer) then
    FPlayer.Repaint();
end;

end.
