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
// Description: VCL form for seeking, pausing, and changing delay
//              during registered-MFT audio playback.
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
  Vcl.ExtCtrls,
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
    lblTimeline: TLabel;
    tbTimeline: TTrackBar;
    memLog: TMemo;
    dlgOpenAudio: TOpenDialog;
    tmrTimeline: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tbEffectChange(Sender: TObject);
    procedure tbTimelineChange(Sender: TObject);
    procedure tbTimelineKeyDown(Sender: TObject; var Key: Word;
                                Shift: TShiftState);
    procedure tbTimelineKeyUp(Sender: TObject; var Key: Word;
                              Shift: TShiftState);
    procedure tbTimelineExit(Sender: TObject);
    procedure tmrTimelineTimer(Sender: TObject);

  private
    FPlayer: IAudioTopologyPlayer;
    FSessionNumber: Cardinal;
    FReady: Boolean;
    FPlaying: Boolean;
    FPaused: Boolean;
    FEnded: Boolean;
    FRestartOnPlay: Boolean;
    FCanSeek: Boolean;
    FDragging: Boolean;
    FKeyboardSeeking: Boolean;
    FSeekPending: Boolean;
    FReturnToPause: Boolean;
    FUpdatingTimeline: Boolean;
    FDuration: Int64;

    procedure WMAudioTopologyEvent(var Message: TMessage); message WM_AUDIO_TOPOLOGY_EVENT;
    procedure ClosePlayer(const ADetachWindow: Boolean);
    procedure LogError(const E: Exception);
    function FormatTimelineTime(const ATime: Int64): string;
    function TimelinePosition(): Int64;
    procedure UpdateTimeline();
    procedure SeekTimeline();
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
  tbTimeline.Enabled := False;
  tmrTimeline.Enabled := False;
  lblTimeline.Caption := '00:00 / --:--';
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
  FPaused := False;
  FEnded := False;
  FRestartOnPlay := False;
  btnPlayPause.Caption := 'Play';
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;
  tmrTimeline.Enabled := False;
  tbTimeline.Enabled := False;
  FCanSeek := False;
  FDragging := False;
  FKeyboardSeeking := False;
  FSeekPending := False;
  FReturnToPause := False;
  FDuration := 0;
  FUpdatingTimeline := True;

  try
    tbTimeline.Position := 0;
  finally
    FUpdatingTimeline := False;
  end;
  lblTimeline.Caption := '00:00 / --:--';

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
          FPaused := False;
          FEnded := False;
          FRestartOnPlay := False;
          btnPlayPause.Enabled := True;
          btnStop.Enabled := False;
          lblState.Caption := 'Topology ready';
          UpdateTimeline();
          tmrTimeline.Enabled := True;
        end;

      atekStarted:
        begin
          FPlaying := True;
          FPaused := False;
          FEnded := False;
          FRestartOnPlay := False;
          btnStop.Enabled := True;
          btnPlayPause.Caption := 'Pause';
          lblState.Caption := 'Playing';

          if FReturnToPause then
            begin
              // A seek uses Start, even from Pause. Wait for Started before
              // requesting Pause again, then finish the seek on Paused.
              FReturnToPause := False;
              try
                FPlayer.Pause();
              except
                on E: Exception do
                  begin
                    FSeekPending := False;
                    LogError(E);
                  end;
              end;
            end
          else
            FSeekPending := False;

          btnPlayPause.Enabled := not FSeekPending;
          tbTimeline.Enabled := FCanSeek and not FSeekPending;
        end;

      atekPaused:
        begin
          FPlaying := False;
          FPaused := True;
          FSeekPending := False;
          btnPlayPause.Enabled := True;
          tbTimeline.Enabled := FCanSeek;
          btnPlayPause.Caption := 'Play';
          lblState.Caption := 'Paused';
        end;

      atekStopped:
        begin
          FPlaying := False;
          FPaused := False;
          FSeekPending := False;
          FReturnToPause := False;
          FRestartOnPlay := True;
          btnPlayPause.Enabled := True;
          btnStop.Enabled := False;
          tbTimeline.Enabled := FCanSeek;
          btnPlayPause.Caption := 'Play';
          if not FEnded then
            lblState.Caption := 'Stopped';
        end;

      atekEnded:
        begin
          FPlaying := False;
          FPaused := False;
          FEnded := True;
          FRestartOnPlay := True;
          FSeekPending := False;
          FReturnToPause := False;
          btnPlayPause.Caption := 'Play';
          btnPlayPause.Enabled := True;
          btnStop.Enabled := False;
          tbTimeline.Enabled := FCanSeek;
          lblState.Caption := 'End of presentation';
        end;

      atekError:
        begin
          FReady := False;
          FPlaying := False;
          FPaused := False;
          FEnded := False;
          FRestartOnPlay := False;
          FSeekPending := False;
          FReturnToPause := False;
          btnPlayPause.Caption := 'Play';
          btnPlayPause.Enabled := False;
          btnStop.Enabled := False;
          tmrTimeline.Enabled := False;
          tbTimeline.Enabled := False;
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

  // Both labels also update before a file is open. Once the transform exists,
  // the same event sends the new values to the running Media Session.
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


function TfrmAudioTopology.FormatTimelineTime(const ATime: Int64): string;
var
  Seconds: Int64;

begin

  Seconds := ATime div 10000000;
  if (Seconds >= 3600) then
    Result := Format('%d:%.2d:%.2d',
                     [Seconds div 3600,
                      (Seconds div 60) mod 60,
                      Seconds mod 60])
  else
    Result := Format('%.2d:%.2d',
                     [Seconds div 60,
                      Seconds mod 60]);
end;


function TfrmAudioTopology.TimelinePosition(): Int64;
begin

  Result := Trunc((FDuration / tbTimeline.Max) *
                  tbTimeline.Position);
end;


procedure TfrmAudioTopology.UpdateTimeline();
var
  Position: Int64;
  Duration: Int64;
  CanSeek: Boolean;

begin

  if not Assigned(FPlayer) or
     not FPlayer.GetTimeline(Position,
                             Duration,
                             CanSeek) then
    begin
      FCanSeek := False;
      FDuration := 0;
      tbTimeline.Enabled := False;
      lblTimeline.Caption := '00:00 / --:--';
      Exit;
    end;

  FDuration := Duration;
  FCanSeek := CanSeek;
  tbTimeline.Enabled := FReady and FCanSeek and not FSeekPending;

  // The presentation clock can retain its last value after playback stops.
  // Keep the timeline at the end after natural completion and at zero after
  // an explicit Stop; the next Play starts from zero in either case.
  if FEnded then
    Position := Duration
  else if FRestartOnPlay then
    Position := 0;

  if FDragging or FKeyboardSeeking or FSeekPending then
    Exit;

  FUpdatingTimeline := True;
  try
    tbTimeline.Position := Trunc((Position / Duration) *
                                 tbTimeline.Max);
  finally
    FUpdatingTimeline := False;
  end;

  lblTimeline.Caption := FormatTimelineTime(Position) + ' / ' +
                         FormatTimelineTime(Duration);
end;


procedure TfrmAudioTopology.SeekTimeline();
var
  Position: Int64;

begin

  if not FReady or not FCanSeek or FSeekPending or
     (FDuration <= 0) or not Assigned(FPlayer) then
    Exit;

  Position := TimelinePosition();
  FSeekPending := True;
  FReturnToPause := FPaused;
  btnPlayPause.Enabled := False;
  tbTimeline.Enabled := False;

  try
    // One Start request is sent when dragging ends or a keyboard step ends.
    // Wait for MESessionStarted before accepting another seek.
    FPlayer.Seek(Position);
  except
    on E: Exception do
      begin
        FSeekPending := False;
        FReturnToPause := False;
        btnPlayPause.Enabled := True;
        tbTimeline.Enabled := FCanSeek;
        LogError(E);
      end;
  end;
end;


procedure TfrmAudioTopology.tbTimelineChange(Sender: TObject);
begin

  if not FUpdatingTimeline and (FDuration > 0) then
    begin
      // XE7's TTrackBar publishes OnChange, but no mouse-up event. The timer
      // waits for the mouse button to be released before sending one seek.
      if not FKeyboardSeeking then
        FDragging := True;

      lblTimeline.Caption := FormatTimelineTime(TimelinePosition()) + ' / ' +
                             FormatTimelineTime(FDuration);
    end;
end;


procedure TfrmAudioTopology.tbTimelineKeyDown(Sender: TObject;
                                              var Key: Word;
                                              Shift: TShiftState);
begin

  if Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    FKeyboardSeeking := True;
end;


procedure TfrmAudioTopology.tbTimelineKeyUp(Sender: TObject;
                                            var Key: Word;
                                            Shift: TShiftState);
begin

  if Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    begin
      FKeyboardSeeking := False;
      SeekTimeline();
    end;
end;


procedure TfrmAudioTopology.tbTimelineExit(Sender: TObject);
begin

  if FKeyboardSeeking then
    begin
      FKeyboardSeeking := False;
      SeekTimeline();
    end;
end;


procedure TfrmAudioTopology.tmrTimelineTimer(Sender: TObject);
begin

  if FDragging and (GetAsyncKeyState(VK_LBUTTON) >= 0) then
    begin
      FDragging := False;
      SeekTimeline();
    end;

  if FReady and not FSeekPending and not FDragging and
     not FKeyboardSeeking then
    UpdateTimeline();
end;


procedure TfrmAudioTopology.btnPlayPauseClick(Sender: TObject);
begin

  if not FReady or FSeekPending or not Assigned(FPlayer) then
    Exit;

  try
    if FPlaying then
      FPlayer.Pause()
    else
      FPlayer.Start(FRestartOnPlay);

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
    btnPlayPause.Enabled := False;
    btnStop.Enabled := False;

  except
    on E: Exception do LogError(E);
  end;
end;

end.
