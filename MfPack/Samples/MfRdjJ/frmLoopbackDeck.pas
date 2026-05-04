// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmLoopbackDeck.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Loopbackdeck MDI child form.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit frmLoopbackDeck;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Diagnostics,
  System.TimeSpan,
  System.UITypes,
  System.Classes,
  System.Math,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  RDJ_Common,
  LoopBackCapture,
  MfLoopbackDeckEngine,
  RDJ.InternalMixer,
  MfWasApiEffectsRack,
  MfParametricEqComponent,
  MPxpButton,
  MfTrackBar,
  MfPeakMeterMmcs,
  frmProcessPicker,
  MfBeatLed;

type

  TfrmLoopbackDeck = class(TForm)
    pnlTop: TPanel;
    tbVolume: TMfTrackBar;
    lblVol: TLabel;
    Bevel3: TBevel;
    lblVolumePerc: TLabel;
    tbBalance: TMfTrackBar;
    lblBalLeft: TLabel;
    lblBalRight: TLabel;
    vuInputL: TMfPeakMeterMmcs;
    vuInputR: TMfPeakMeterMmcs;
    lblPitch: TLabel;
    tbPitch: TMfTrackBar;
    lblP: TLabel;
    Bevel5: TBevel;
    lblBpm: TLabel;
    bldBeat: TMfBeatLed;
    Bevel6: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    tbEqQ: TMfTrackBar;
    tbEqCenterFreqHz: TMfTrackBar;
    tbEqGainDb: TMfTrackBar;
    Timer1: TTimer;
    lblInputGainValue: TLabel;
    tbInputGain: TMfTrackBar;
    lblGain: TLabel;
    Bevel7: TBevel;
    Label1: TLabel;
    Shape2: TShape;
    Shape1: TShape;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    shpLive: TShape;
    lblLive: TLabel;
    shpLiveCap: TShape;
    pnlBottom: TPanel;
    lblStatus: TLabel;
    Bevel8: TBevel;
    lblPlayed: TLabel;
    lblAudioFormat: TLabel;
    pnlMid: TPanel;
    Bevel1: TBevel;
    lblProcess: TLabel;
    lblProcessId: TLabel;
    edtProcessName: TEdit;
    btnSelectProcess: TMPxpButton;
    btnPlayStop: TMPxpButton;
    chkMute: TMPxpButton;
    chkCrossFade: TMPxpButton;
    btnPFL: TMPxpButton;
    edtPID: TEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure btnSelectProcessClick(Sender: TObject);
    procedure btnPlayStopClick(Sender: TObject);

    procedure chkMuteClick(Sender: TObject);
    procedure chkCrossFadeClick(Sender: TObject);
    procedure btnPFLClick(Sender: TObject);

    procedure tbVolumeChange(Sender: TObject);
    procedure tbBalanceChange(Sender: TObject);
    procedure tbPitchChange(Sender: TObject);

    // Events
    procedure WasApiStateEvent(Sender: TObject;
                               const AState: TWasApiDeviceState);

    procedure DeckTick(Sender: TObject;
                       const Position100ns: Int64;
                       const CurrentBpm: Double;
                       const BeatPhase: Double);

    procedure DeckBeat(Sender: TObject;
                       const Position100ns: Int64;
                       const BeatNumber: Int64;
                       const CurrentBpm: Double);

    procedure btnSetDownbeatClick(Sender: TObject);
    procedure tbEqQChange(Sender: TObject);
    procedure tbEqCenterFreqHzChange(Sender: TObject);
    procedure tbEqGainDbChange(Sender: TObject);
    procedure tbVolumeDblClick(Sender: TObject);
    procedure tbBalanceDblClick(Sender: TObject);
    procedure tbPitchDblClick(Sender: TObject);
    procedure tbInputGainChange(Sender: TObject);
    procedure tbInputGainDblClick(Sender: TObject);
    procedure tbEqQDblClick(Sender: TObject);
    procedure tbEqCenterFreqHzDblClick(Sender: TObject);
    procedure tbEqGainDbDblClick(Sender: TObject);

  private

    FMixerChannel: TRDJMixerChannel;
    FEngine: TMfLoopbackDeckEngine;
    FProcessId: Cardinal;
    FProcessName: string;
    FMute: Boolean;
    FTempoPercent: Single;
    FBeatPulseActive: Boolean;
    FIgnoreBpmEditChange: Boolean;
    FAudioRack: TMfWasApiEffectsRack;
    FUpdatingAudioRackGui: Boolean;
    // We use timers here, to prevent distortions during capture.
    // The timer is set to 10 millisecond resolution.
    prStopWatch: TStopwatch;
    FTimerRunning: Boolean;
    FVolPosLast: Integer;
    FApplyingXFade: Boolean;

    function GetEqEffect(): TMfParametricEqEffect;
    procedure BindAudioRack();
    procedure LoadAudioRackToGui();

    procedure RegisterToMixer();
    procedure UnregisterFromMixer();
    procedure UpdateMixerChannelState();

    procedure UpdateUiState();
    procedure UpdateProcessUi();
    procedure UpdateLiveLamp(const ALive: Boolean);
    procedure UpdateVolumeUi();
    procedure UpdateBalanceUi();
    procedure UpdatePitchUi();
    procedure UpdateAudioFormatUi();

    procedure DetachDeckEngineEvents();

    procedure UpdateTimeLabel();
    // bpm
    procedure UpdateBpmGuiFromEngine();
    procedure ResetBeatLamp();

    procedure ApplyVolumeToMixer();
    procedure ApplyBalanceToMixer();
    procedure ApplyMuteToMixer();
    procedure ApplyPflToMixer();
    procedure ApplyCrossFadeToMixer();
    procedure ApplyTempoToEngine();
    procedure ApplyInputGainToEngine();
    procedure ApplyTwoDeckXFade(const ANewPos: Integer);

    // Helpers volume ----------------------------------------------------------
    function Clamp01(const AValue: Single): Single;
    function GetVolumeNorm(): Single;
    function GetBalanceNorm(): Single;
    function GetTempoPercentFromTrackBar(): Single;
    procedure PushMainGainsToMixer();

    function CanStartCapture(): Boolean;

    function SelectTargetProcess(out AProcessId: Cardinal;
                                 out AProcessName: string): Boolean;

    function MixerReadOutputPcmFloat32(const Frames: Integer;
                                       const OutBuffer: PSingle;
                                       out Flags: DWORD): HRESULT;

  public

    procedure ApplyExternalCrossFadeDelta(const ADelta: Integer);
    procedure StartCapture();
    procedure StopCapture();
  end;

var
  FfrmLoopbackDeck: TfrmLoopbackDeck;


implementation

{$R *.dfm}

uses
  frmMainMDI,
  frmChannelDeck;


procedure TfrmLoopbackDeck.FormCreate(Sender: TObject);
begin

  FEngine := TMfLoopbackDeckEngine.Create();
  FMixerChannel := nil;
  BindAudioRack();
  RegisterToMixer();

  FEngine.OnState := WasApiStateEvent;
  // BPM
  FEngine.OnDeckTick := DeckTick;
  FEngine.OnBeat := DeckBeat;

  FProcessId := 0;
  FProcessName := '';
  FMute := chkMute.Checked;
  FTempoPercent := 0.0;
  FVolPosLast := tbVolume.Position;
  FApplyingXFade := False;

  edtProcessName.ReadOnly := True;

  UpdateProcessUi();
  UpdateVolumeUi();
  UpdateBalanceUi();
  UpdatePitchUi();
  UpdateAudioFormatUi();
  UpdateLiveLamp(False);
  UpdateUiState();

  FIgnoreBpmEditChange := False;
  FBeatPulseActive := False;

  prStopWatch := TStopwatch.Create();

  lblBpm.Caption := '--.-- BPM';
  ResetBeatLamp();
end;


procedure TfrmLoopbackDeck.FormDestroy(Sender: TObject);
begin

  StopCapture();
  DetachDeckEngineEvents();

  UnregisterFromMixer();
  FreeAndNil(FEngine);
end;


procedure TfrmLoopbackDeck.FormShow(Sender: TObject);
begin

  UpdateUiState();
  UpdatePitchUi();
  LoadAudioRackToGui();
  UpdateBpmGuiFromEngine();

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);
 // Height := 1538;
 // Width := 415;
end;


procedure TfrmLoopbackDeck.btnSelectProcessClick(Sender: TObject);
begin

  if SelectTargetProcess(FProcessId,
                         FProcessName) then
    UpdateProcessUi();
end;


procedure TfrmLoopbackDeck.btnSetDownbeatClick(Sender: TObject);
begin

  if Assigned(FEngine) then
    FEngine.BeatOffset100ns := FEngine.Position100ns;
end;


procedure TfrmLoopbackDeck.btnPlayStopClick(Sender: TObject);
begin

  if not Assigned(FEngine) then
    Exit;

  if FEngine.Active then
    StopCapture()
  else
    StartCapture();
  UpdateUiState();
end;


procedure TfrmLoopbackDeck.chkMuteClick(Sender: TObject);
begin

  FMute := chkMute.Checked;
  ApplyMuteToMixer();
  UpdateMixerChannelState();
  UpdateUiState();
end;


procedure TfrmLoopbackDeck.chkCrossFadeClick(Sender: TObject);
var
  I: Integer;
  Count: Integer;
begin

  if not chkCrossFade.Checked then
    begin
      ApplyCrossFadeToMixer();
      UpdateMixerChannelState();
      Exit;
    end;

  Count := 0;
  for I := 0 to Screen.FormCount - 1 do
    begin
      if (Screen.Forms[I] is TfrmChannelDeck) then
        begin
          if TfrmChannelDeck(Screen.Forms[I]).chkCrossFade.Checked then
            Inc(Count);
        end
      else if (Screen.Forms[I] is TfrmLoopbackDeck) then
        begin
          if TfrmLoopbackDeck(Screen.Forms[I]).chkCrossFade.Checked then
            Inc(Count);
        end;
    end;

  if (Count > 2) then
    begin
      for I := 0 to Screen.FormCount - 1 do
        begin
          if (Screen.Forms[I] = Self) then
            Continue;

          if (Screen.Forms[I] is TfrmChannelDeck) then
            begin
              if TfrmChannelDeck(Screen.Forms[I]).chkCrossFade.Checked then
                begin
                  TfrmChannelDeck(Screen.Forms[I]).chkCrossFade.Checked := False;
                  Break;
                end;
            end
          else if (Screen.Forms[I] is TfrmLoopbackDeck) then
            begin
              if TfrmLoopbackDeck(Screen.Forms[I]).chkCrossFade.Checked then
                begin
                  TfrmLoopbackDeck(Screen.Forms[I]).chkCrossFade.Checked := False;
                  Break;
                end;
            end;
        end;
    end;

  ApplyCrossFadeToMixer();
  UpdateMixerChannelState();
end;

procedure TfrmLoopbackDeck.btnPFLClick(Sender: TObject);
begin

  ApplyPflToMixer();
  UpdateMixerChannelState();
end;


procedure TfrmLoopbackDeck.tbVolumeChange(Sender: TObject);
begin

  if not FApplyingXFade then
    ApplyTwoDeckXFade(tbVolume.Position);

  FVolPosLast := tbVolume.Position;

  UpdateVolumeUi();
  ApplyVolumeToMixer();
  UpdateMixerChannelState();
end;

procedure TfrmLoopbackDeck.tbVolumeDblClick(Sender: TObject);
var
  TargetPos: Integer;

begin

  if (tbVolume.Position <> 0) then
    TargetPos := 0
  else
    TargetPos := tbVolume.Maximum;

  tbVolume.AnimateTrackBarToPosition(TargetPos,
                                     2);
end;


procedure TfrmLoopbackDeck.tbBalanceChange(Sender: TObject);
begin

  UpdateBalanceUi();
  ApplyBalanceToMixer();
  UpdateMixerChannelState();
end;


procedure TfrmLoopbackDeck.tbBalanceDblClick(Sender: TObject);
begin

  tbBalance.AnimateTrackBarToPosition(0,
                                      2);
end;


procedure TfrmLoopbackDeck.tbEqCenterFreqHzChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  Eq.CenterFreqHz := tbEqCenterFreqHz.Position;
end;


procedure TfrmLoopbackDeck.tbEqCenterFreqHzDblClick(Sender: TObject);
begin

  tbEqCenterFreqHz.AnimateTrackBarToPosition(1500,
                                             2);
end;


procedure TfrmLoopbackDeck.tbEqGainDbChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  Eq.GainDb := tbEqGainDb.Position;
end;


procedure TfrmLoopbackDeck.tbEqGainDbDblClick(Sender: TObject);
begin

  tbEqGainDb.AnimateTrackBarToPosition(0,
                                       2);
end;


procedure TfrmLoopbackDeck.tbEqQChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;
  NewQ: Single;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();

  if not Assigned(Eq) then
    Exit;

  NewQ := tbEqQ.Position / 10.0;
  if (NewQ <= 0) then
    NewQ := 0.1;

  Eq.Q := NewQ;
end;


procedure TfrmLoopbackDeck.tbEqQDblClick(Sender: TObject);
begin

  tbEqQ.AnimateTrackBarToPosition(10,
                                  2);
end;


procedure TfrmLoopbackDeck.tbInputGainChange(Sender: TObject);
begin

  ApplyInputGainToEngine();
end;


procedure TfrmLoopbackDeck.tbInputGainDblClick(Sender: TObject);
begin

  tbInputGain.AnimateTrackBarToPosition(0,
                                        2);
end;


procedure TfrmLoopbackDeck.tbPitchChange(Sender: TObject);
begin

  FTempoPercent := GetTempoPercentFromTrackBar();
  UpdatePitchUi();
  ApplyTempoToEngine();
end;


procedure TfrmLoopbackDeck.tbPitchDblClick(Sender: TObject);
begin

  tbPitch.AnimateTrackBarToPosition(0,
                                    2);
end;


procedure TfrmLoopbackDeck.UpdateTimeLabel();
var
  TS: TTimeSpan;
  Hours: Integer;
  Minutes: Integer;
  Seconds: Integer;
  Hundredths: Integer;

begin

  TS := prStopwatch.Elapsed;

  Hours := Trunc(TS.TotalHours);
  Minutes := TS.Minutes;
  Seconds := TS.Seconds;
  Hundredths := TS.Milliseconds div 10;

  lblPlayed.Caption := Format('Played: %.2d:%.2d:%.2d.%.2d',
                              [Hours, Minutes, Seconds, Hundredths]);
end;


procedure TfrmLoopbackDeck.WasApiStateEvent(Sender: TObject;
                                            const AState: TWasApiDeviceState);
begin

  if not Assigned(FEngine) then
    Exit;

  case AState of
    dsReady: begin

             end;
    dsPlay:  begin

               prStopwatch := TStopwatch.StartNew();
               FTimerRunning := True;
               Timer1.Enabled := True;
             end;

    dsStop:  begin

               // Stop the timer and stopwatch.
               prStopwatch.Stop;
               FTimerRunning := False;
               Timer1.Enabled := False;
               UpdateTimeLabel();
               prStopWatch.Reset;
             end;
  end;
end;


// BPM -------------------------------------------------------------------------
procedure TfrmLoopbackDeck.DeckTick(Sender: TObject;
                                    const Position100ns: Int64;
                                    const CurrentBpm: Double;
                                    const BeatPhase: Double);
begin

  bldBeat.UpdatePulse(Position100ns);

  if (CurrentBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [CurrentBpm])
  else
    UpdateBpmGuiFromEngine();
end;


procedure TfrmLoopbackDeck.DeckBeat(Sender: TObject;
                                    const Position100ns: Int64;
                                    const BeatNumber: Int64;
                                    const CurrentBpm: Double);
begin

  bldBeat.TriggerPulse(Position100ns);

  if (CurrentBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [CurrentBpm])
  else
    UpdateBpmGuiFromEngine();
end;
// BPM end ---------------------------------------------------------------------

procedure TfrmLoopbackDeck.StartCapture();
var
  hr: HRESULT;
  CaptureMode: TMfLoopbackCaptureMode;

begin

  if not Assigned(FEngine) then
    Exit;

  if not CanStartCapture() then
    Exit;

  ApplyTempoToEngine();
  UpdateBpmGuiFromEngine();

  CaptureMode := lcmIncludeProcessTree;

  hr := FEngine.PrepareProcess(FProcessId,
                               CaptureMode,
                               0);
  if Failed(hr) then
    begin

      lblStatus.Caption := 'State: Prepare failed';
      lblStatus.Hint := lblStatus.Caption;
      UpdateLiveLamp(False);
      Exit;
    end;

  hr := FEngine.Start();

  if FAILED(hr) then
    begin

      lblStatus.Caption := 'Start: Failed';
      lblStatus.Hint := lblStatus.Caption;
    end;
end;


procedure TfrmLoopbackDeck.StopCapture();
var
  hr: HRESULT;

begin

  if not Assigned(FEngine) then
    Exit;

  hr := FEngine.Stop();
  if FAILED(hr)  then
    begin

      lblStatus.Caption := Format('Stop failed with error %d',
                                  [hr]);
      lblStatus.Hint := lblStatus.Caption;
    end;

  UpdateMixerChannelState();
  UpdateUiState();
end;


procedure TfrmLoopbackDeck.BindAudioRack();
begin

  FAudioRack := nil;

  if Assigned(FEngine) then
    FAudioRack := FEngine.AudioRack;
end;


procedure TfrmLoopbackDeck.LoadAudioRackToGui();
var
  Eq: TMfParametricEqEffect;

begin

  if not Assigned(FAudioRack) then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  FUpdatingAudioRackGui := True;
  try

    tbEqGainDb.Position := Round(Eq.GainDb);
    tbEqCenterFreqHz.Position := Round(Eq.CenterFreqHz);
    tbEqQ.Position := Round(Eq.Q * 10.0);
  finally

    FUpdatingAudioRackGui := False;
  end;
end;


function TfrmLoopbackDeck.GetEqEffect(): TMfParametricEqEffect;
var
  i: Integer;
  Fx: TObject;

begin

  Result := nil;

  if not Assigned(FAudioRack) then
    Exit;

  for i := 0 to FAudioRack.Slots.Count - 1 do
    begin

      Fx := TMfWasApiFxSlot(FAudioRack.Slots[i]).Effect;
      if Assigned(Fx) and
        (Fx is TMfParametricEqEffect) then
        begin

          Result := TMfParametricEqEffect(Fx);
          Exit;
        end;
    end;
end;


procedure TfrmLoopbackDeck.RegisterToMixer();
begin

  if Assigned(FMixerChannel) then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  if not Assigned(MainMDIFrm.InternalMixer) then
    Exit;

  if not Assigned(FEngine) then
    Exit;

  FMixerChannel := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);

  if not Assigned(FMixerChannel) then
    FMixerChannel := MainMDIFrm.InternalMixer.AddChannel();

  FMixerChannel.DeckEngineObj := FEngine;
  FMixerChannel.OnReadOutputPcmFloat32 := MixerReadOutputPcmFloat32;

  UpdateMixerChannelState();
end;


procedure TfrmLoopbackDeck.UnregisterFromMixer();
begin

  if Assigned(FMixerChannel) and
     Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.InternalMixer) then
    MainMDIFrm.InternalMixer.RemoveChannel(FMixerChannel);

  FMixerChannel := nil;
end;


procedure TfrmLoopbackDeck.UpdateMixerChannelState();
begin

  if (FEngine.Active = False) then
    Exit;

  if not Assigned(FMixerChannel) then
    begin

      RegisterToMixer();
      if not Assigned(FMixerChannel) then
        Exit;
    end;

  FMixerChannel.Enabled := True;

  ApplyMuteToMixer();
  ApplyPflToMixer();
  ApplyVolumeToMixer();
  ApplyBalanceToMixer();
  ApplyCrossFadeToMixer();
end;


procedure TfrmLoopbackDeck.UpdateUiState();
begin

  btnSelectProcess.Enabled := not (Assigned(FEngine) and FEngine.Active);

  btnPlayStop.Enabled := (FProcessId <> 0);

  if Assigned(FEngine) and FEngine.Active then
    begin

      btnPlayStop.Caption := 'Stop';
      lblStatus.Caption := 'State: Capturing';
      lblStatus.Hint := lblStatus.Caption;
      UpdateLiveLamp(True);
    end
  else
    begin

      btnPlayStop.Caption := 'Play';
      if (FProcessId <> 0) then
        lblStatus.Caption := 'State: Ready'
      else
        lblStatus.Caption := 'State: Stopped';
      lblStatus.Hint := lblStatus.Caption;
      UpdateLiveLamp(False);
    end;
end;


procedure TfrmLoopbackDeck.UpdateProcessUi();
begin

  if (FProcessName <> '') then
    edtProcessName.Text := FProcessName
  else
    edtProcessName.Text := '-';

  if (FProcessId <> 0) then
    edtPID.Text := IntToStr(FProcessId)
  else
    edtPID.Text := '-';

  UpdateUiState();
end;


procedure TfrmLoopbackDeck.UpdateLiveLamp(const ALive: Boolean);
const
  ON_COLOR = clRed;
  OFF_COLOR = $00568000;

begin

  if ALive then
    begin

      shpLive.Brush.Color := ON_COLOR;
      shpLive.Pen.Color := ON_COLOR;
      shpLiveCap.Pen.Color := ON_COLOR;
      lblLive.Font.Color := ON_COLOR;
    end
  else
    begin

      shpLive.Brush.Color := OFF_COLOR;
      shpLive.Pen.Color := OFF_COLOR;
      shpLiveCap.Pen.Color := OFF_COLOR;
      lblLive.Font.Color := OFF_COLOR;
    end;
end;


procedure TfrmLoopbackDeck.UpdateVolumeUi();
begin

  lblVolumePerc.Caption := IntToStr(tbVolume.Position) + '%';
end;


procedure TfrmLoopbackDeck.UpdateBalanceUi();
begin

  { Replace with preferred balance text if needed. }
end;


procedure TfrmLoopbackDeck.UpdatePitchUi();
begin

  lblP.Caption := Format('%.1f%%',
                         [FTempoPercent]);
end;


procedure TfrmLoopbackDeck.UpdateAudioFormatUi();
begin

  lblAudioFormat.Caption := '44.1 kHz / stereo';
end;


procedure TfrmLoopbackDeck.ApplyVolumeToMixer();
begin

  if not Assigned(FMixerChannel) then
    Exit;

  PushMainGainsToMixer();
end;


procedure TfrmLoopbackDeck.ApplyBalanceToMixer();
begin

  if not Assigned(FMixerChannel) then
    Exit;

  PushMainGainsToMixer();
end;


procedure TfrmLoopbackDeck.ApplyMuteToMixer();
begin

  if not Assigned(FMixerChannel) then
    Exit;

  FMixerChannel.Muted := chkMute.Checked;
end;


procedure TfrmLoopbackDeck.ApplyPflToMixer();
begin

  if not Assigned(FMixerChannel) then
    Exit;

  FMixerChannel.CueEnabled := btnPFL.Checked;
  FMixerChannel.CueVolL := 1.0;
  FMixerChannel.CueVolR := 1.0;
end;


procedure TfrmLoopbackDeck.ApplyCrossFadeToMixer();
begin

  // Same behavior as the channel deck crossfade checkbox:
  // it only marks this deck as one of the two linked decks.
  // The actual linked inverse volume movement is handled in ApplyTwoDeckXFade.
end;


procedure TfrmLoopbackDeck.ApplyTwoDeckXFade(const ANewPos: Integer);
var
  Delta: Integer;
  I: Integer;

  function ClampI(const X, A, B: Integer): Integer; inline;
  begin
    if (X < A) then
      Exit(A);
    if (X > B) then
      Exit(B);
    Result := X;
  end;

begin

  if FApplyingXFade then
    Exit;

  if not chkCrossFade.Checked then
    Exit;

  Delta := ANewPos - FVolPosLast;
  if (Delta = 0) then
    Exit;

  for I := 0 to Screen.FormCount - 1 do
    begin
      if (Screen.Forms[I] = Self) then
        Continue;

      if (Screen.Forms[I] is TfrmChannelDeck) then
        begin
          if TfrmChannelDeck(Screen.Forms[I]).chkCrossFade.Checked then
            begin
              TfrmChannelDeck(Screen.Forms[I]).ApplyExternalCrossFadeDelta(Delta);
              Exit;
            end;
        end
      else if (Screen.Forms[I] is TfrmLoopbackDeck) then
        begin
          if TfrmLoopbackDeck(Screen.Forms[I]).chkCrossFade.Checked then
            begin
              TfrmLoopbackDeck(Screen.Forms[I]).ApplyExternalCrossFadeDelta(Delta);
              Exit;
            end;
        end;
    end;
end;


procedure TfrmLoopbackDeck.ApplyExternalCrossFadeDelta(const ADelta: Integer);
  function ClampI(const X, A, B: Integer): Integer; inline;
  begin
    if (X < A) then
      Exit(A);
    if (X > B) then
      Exit(B);
    Result := X;
  end;
begin

  if (ADelta = 0) then
    Exit;

  FApplyingXFade := True;
  try
    tbVolume.Position := ClampI(tbVolume.Position - ADelta,
                                tbVolume.Minimum,
                                tbVolume.Maximum);
    FVolPosLast := tbVolume.Position;
    ApplyVolumeToMixer();
    UpdateMixerChannelState();
  finally
    FApplyingXFade := False;
  end;
end;


procedure TfrmLoopbackDeck.ApplyTempoToEngine();
begin

  if not Assigned(FEngine) then
    Exit;

  { NOTE:
    The actual FIFO/ring buffer and resampling implementation belongs in
    TMfLoopbackDeckEngine. This form only forwards the user tempo value. }
  FEngine.SetTempoPercent(FTempoPercent);
  UpdateBpmGuiFromEngine();
end;


procedure TfrmLoopbackDeck.ApplyInputGainToEngine();
var
  GainDb: Single;

begin

  GainDb := tbInputGain.Position / 10.0;

  if (GainDb > 0) then
    lblInputGainValue.Caption := '+' + FormatFloat('0.0',
                                                   GainDb) + ' dB'
  else
    lblInputGainValue.Caption := FormatFloat('0.0',
                                             GainDb) + ' dB';

  if Assigned(FEngine) then
    FEngine.InputGainDb := GainDb;
end;


// Helpers volume --------------------------------------------------------------
function TfrmLoopbackDeck.Clamp01(const AValue: Single): Single;
begin

  if (AValue < 0.0) then
    Result := 0.0
  else if (AValue > 1.0) then
    Result := 1.0
  else
    Result := AValue;
end;


function TfrmLoopbackDeck.GetVolumeNorm(): Single;
var
  Range: Integer;

begin

  Range := tbVolume.Maximum - tbVolume.Minimum;
  if (Range <= 0) then
    Exit(1.0);

  Result := Clamp01((tbVolume.Position - tbVolume.Minimum) / Range);
end;


function TfrmLoopbackDeck.GetBalanceNorm(): Single;
var
  Range: Integer;

begin

  Range := tbBalance.Maximum - tbBalance.Minimum;
  if (Range <= 0) then
    Exit(0.0);

  Result := ((tbBalance.Position - tbBalance.Minimum) / Range) * 2.0 - 1.0;
  if (Result < -1.0) then
    Result := -1.0
  else if (Result > 1.0) then
    Result := 1.0;
end;


function TfrmLoopbackDeck.GetTempoPercentFromTrackBar(): Single;
var
  Range: Integer;
  MidPos: Double;

begin

  Range := tbPitch.Maximum - tbPitch.Minimum;
  if (Range <= 0) then
    Exit(0.0);

  MidPos := tbPitch.Minimum + (Range / 2.0);
  Result := ((tbPitch.Position - MidPos) / (Range / 2.0)) * 16.0;

  if (Result < -16.0) then
    Result := -16.0
  else if (Result > 16.0) then
    Result := 16.0;
end;


procedure TfrmLoopbackDeck.PushMainGainsToMixer();
var
  BaseVol: Single;
  Pan: Single;
  Angle: Double;

begin

  if not Assigned(FMixerChannel) then
    Exit;

  BaseVol := GetVolumeNorm();
  Pan := GetBalanceNorm();

  { Equal-power pan:
    Pan = -1.0 => full left
    Pan =  0.0 => center
    Pan = +1.0 => full right
  }
  Angle := (Pan + 1.0) * (Pi / 4.0);

  FMixerChannel.VolL := BaseVol * Cos(Angle);
  FMixerChannel.VolR := BaseVol * Sin(Angle);
end;


function TfrmLoopbackDeck.CanStartCapture(): Boolean;
begin

  Result := Assigned(FEngine) and
            (FProcessId <> 0);
end;


function TfrmLoopbackDeck.SelectTargetProcess(out AProcessId: Cardinal;
                                              out AProcessName: string): Boolean;
begin

  AProcessId := 0;
  AProcessName := '';


  // Create the dialog if it's not allready done.
  if not Assigned(dlgProcessPicker) then
    begin

      //Application.CreateForm(TdlgProcessInfo,
      //                       dlgProcessInfo);
      //dlgProcessInfo.Visible := False;

      Application.CreateForm(TdlgProcessPicker,
                             dlgProcessPicker);
      dlgProcessPicker.Visible := False;

    end;

  // Ask the user to select one.
  if (dlgProcessPicker.ShowModal = mrOk) then
    begin

      FProcessId := dlgProcessPicker.SelectedPID;
      edtPID.Text := IntToStr(FProcessId);
      FProcessName := dlgProcessPicker.SelectedProcName;
      edtProcessName.Text := FProcessName;
      Result := True;
    end
  else
    begin

      // User canceled.
      Result := False;
    end;
end;


function TfrmLoopbackDeck.MixerReadOutputPcmFloat32(const Frames: Integer;
                                                    const OutBuffer: PSingle;
                                                    out Flags: DWORD): HRESULT;
var
  hr: HRESULT;
  ByteCount: DWORD;
  Wfx: TWAVEFORMATEX;
begin

  Flags := 0;

  if (Frames <= 0) or (OutBuffer = nil) then
    Exit(E_INVALIDARG);

  if not Assigned(FEngine) then
    Exit(E_POINTER);

  hr := FEngine.ReadOutputPcmFloat32(Frames,
                                     OutBuffer,
                                     Flags);
  if Failed(hr) then
    Exit(hr);

  FillChar(Wfx, SizeOf(Wfx), 0);
  Wfx.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
  Wfx.nChannels := 2;
  Wfx.nSamplesPerSec := 44100;
  Wfx.wBitsPerSample := 32;
  Wfx.nBlockAlign := Wfx.nChannels * (Wfx.wBitsPerSample div 8);
  Wfx.nAvgBytesPerSec := Wfx.nSamplesPerSec * Wfx.nBlockAlign;

  ByteCount := DWORD(Frames * Wfx.nBlockAlign);

  if Assigned(vuInputL) then
    vuInputL.PushPcm(PByte(OutBuffer),
                     ByteCount,
                     @Wfx);

  if Assigned(vuInputR) then
    vuInputR.PushPcm(PByte(OutBuffer),
                     ByteCount,
                     @Wfx);

  Result := S_OK;
end;


// bpm -------------------------------------------------------------------------
procedure TfrmLoopbackDeck.UpdateBpmGuiFromEngine();
begin

  if not Assigned(FEngine) then
    Exit;

  if (FEngine.GetCurrentBpm() > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [FEngine.GetCurrentBpm()])
  else if (FEngine.TrackBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [FEngine.TrackBpm])
  else
    lblBpm.Caption := '--.-- BPM';
end;


procedure TfrmLoopbackDeck.ResetBeatLamp();
begin

  FBeatPulseActive := False;
  bldBeat.LedOffColor := $004A3809;
end;


procedure TfrmLoopbackDeck.DetachDeckEngineEvents();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.OnDeckTick := nil;
  FEngine.OnBeat := nil;
end;

// bpm end ---------------------------------------------------------------------

end.
