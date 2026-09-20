// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.AudioTopology.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: VCL player for live audio playback through the local
//              delay MFT topology.
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
unit Form.AudioTopology;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  System.UITypes,
  {Vcl}
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.StdCtrls,
  {Application}
  AudioTopologyPlayer;

type
  TfrmAudioTopology = class(TForm)
    btnOpen: TButton;
    btnPlayPause: TButton;
    btnStop: TButton;
    lblDelay: TLabel;
    tbDelay: TTrackBar;
    lblMix: TLabel;
    tbMix: TTrackBar;
    lblState: TLabel;
    memLog: TMemo;
    dlgOpenAudio: TOpenDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tbEffectChange(Sender: TObject);

  private
    FPlayer: IAudioTopologyPlayer;
    FSessionNumber: Cardinal;
    FReady: Boolean;
    FPlaying: Boolean;

    procedure WMAudioTopologyEvent(var Message: TMessage); message WM_AUDIO_TOPOLOGY_EVENT;
    procedure ClosePlayer(const ADetachWindow: Boolean);
    procedure LogError(const E: Exception);
  end;

var
  frmAudioTopology: TfrmAudioTopology;


implementation

{$R *.dfm}


procedure TfrmAudioTopology.FormCreate(Sender: TObject);
begin

  tbEffectChange(nil);
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;
  lblState.Caption := 'No audio loaded';
  dlgOpenAudio.Filter := 'Audio files|*.wav;*.mp3;*.wma;*.m4a;*.aac;*.flac|All files|*.*';
  memLog.Lines.Add('Open an audio file to build the Media Session topology.');
end;


procedure TfrmAudioTopology.FormDestroy(Sender: TObject);
var
  Msg: TMsg;

begin

  ClosePlayer(True);

  while PeekMessage(Msg,
                    Handle,
                    WM_AUDIO_TOPOLOGY_EVENT,
                    WM_AUDIO_TOPOLOGY_EVENT,
                    PM_REMOVE) do
    TAudioTopologyNotice(Pointer(Msg.LParam)).Free;
end;


procedure TfrmAudioTopology.ClosePlayer(const ADetachWindow: Boolean);
begin

  FReady := False;
  FPlaying := False;
  btnPlayPause.Caption := 'Play';
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;

  if Assigned(FPlayer) then
    FPlayer.Close(ADetachWindow);

  FPlayer := nil;
  Inc(FSessionNumber);
end;


procedure TfrmAudioTopology.LogError(const E: Exception);
begin

  lblState.Caption := 'Error';
  memLog.Lines.Add(E.Message);
end;


procedure TfrmAudioTopology.WMAudioTopologyEvent(var Message: TMessage);
var
  Notice: TAudioTopologyNotice;

begin

  Notice := TAudioTopologyNotice(Pointer(Message.LParam));

  try
    if not Assigned(Notice) or (Notice.SessionNumber <> FSessionNumber) then
      Exit;

    memLog.Lines.Add(Notice.Text);

    case Notice.Kind of
      atekReady:
        begin
          FReady := True;
          btnPlayPause.Enabled := True;
          btnStop.Enabled := True;
          lblState.Caption := 'Topology ready';
        end;

      atekStarted:
        begin
          FPlaying := True;
          btnPlayPause.Caption := 'Pause';
          lblState.Caption := 'Playing';
        end;

      atekPaused:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Paused';
        end;

      atekStopped:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Stopped';
        end;

      atekEnded:
        begin
          FPlaying := False;
          btnPlayPause.Caption := 'Play';
          btnPlayPause.Enabled := False;
          lblState.Caption := 'End of presentation';
        end;

      atekError:
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


procedure TfrmAudioTopology.btnOpenClick(Sender: TObject);
begin

  if not dlgOpenAudio.Execute then
    Exit;

  ClosePlayer(False);
  memLog.Clear;
  memLog.Lines.Add(ExtractFileName(dlgOpenAudio.FileName));
  memLog.Lines.Add('Creating audio topology...');
  lblState.Caption := 'Resolving topology';

  try
    FPlayer := CreateAudioTopologyPlayer(Handle,
                                         FSessionNumber);

    FPlayer.Open(dlgOpenAudio.FileName,
                 Cardinal(tbDelay.Position),
                 Cardinal(tbMix.Position));

  except
    on E: Exception do
    begin
      ClosePlayer(False);
      LogError(E);
    end;
  end;
end;


procedure TfrmAudioTopology.tbEffectChange(Sender: TObject);
begin

  lblDelay.Caption := Format('Delay: %d ms',
                             [tbDelay.Position]);
  lblMix.Caption := Format('Wet mix: %d%%',
                           [tbMix.Position]);

  if Assigned(FPlayer) then
    try
      FPlayer.UpdateEffect(Cardinal(tbDelay.Position),
                           Cardinal(tbMix.Position));
    except
      on E: Exception do LogError(E);
    end;
end;


procedure TfrmAudioTopology.btnPlayPauseClick(Sender: TObject);
begin

  if not FReady or not Assigned(FPlayer) then
    Exit;

  try
    if FPlaying then
      FPlayer.Pause()
    else FPlayer.Start();

  except
    on E: Exception do LogError(E);
  end;
end;


procedure TfrmAudioTopology.btnStopClick(Sender: TObject);
begin

  if not FReady or not Assigned(FPlayer) then
    Exit;

  try
    FPlayer.Stop();

  except
    on E: Exception do LogError(E);
  end;
end;

end.
