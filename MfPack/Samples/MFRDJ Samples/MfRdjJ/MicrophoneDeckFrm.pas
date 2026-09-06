// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MicrophoneDeckFrm.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Microphone deck GUI.
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
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
unit MicrophoneDeckFrm;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  MfBeatLed,
  MfLevelProgressBar,
  MfTrackBar,
  MfPeakMeterMmcs,
  MPxpButton,
  MfMicrophoneDeckEngine,
  RDJ.Setup,
  // parametric eq
  MfWasApiEffectsRack,
  MfParametricEqComponent,
  RDJ.InternalMixer;

type

  PMicInputDeviceItem = ^TMicInputDeviceItem;
  TMicInputDeviceItem = record
    DeviceID: string;
    FriendlyName: string;
  end;


  TFrmMicrophoneDeck = class(TForm)
    pnlTop: TPanel;
    Bevel4: TBevel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    pmLeft: TMfPeakMeterMmcs;
    pmRight: TMfPeakMeterMmcs;
    lblVolumePerc: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lblVol: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblGain: TLabel;
    lblPeq: TLabel;
    lblInputGainValue: TLabel;
    tbVolume: TMfTrackBar;
    tbBalance: TMfTrackBar;
    tbEqQ: TMfTrackBar;
    tbEqCenterFreqHz: TMfTrackBar;
    tbEqGainDb: TMfTrackBar;
    tbInputGain: TMfTrackBar;
    pnlMid: TPanel;
    chkMute: TMPxpButton;
    chkPFL: TMPxpButton;
    tmrUi: TTimer;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    shpClip: TShape;
    shpSignalCap: TShape;
    shpSignal: TShape;
    lblSignal: TLabel;
    pnlCompEcho: TPanel;
    pnlCompressor: TPanel;
    lblCompThresholdValue: TLabel;
    lblCompRatioValue: TLabel;
    lblCompAttackValue: TLabel;
    lblCompReleaseValue: TLabel;
    lblCompMakeupValue: TLabel;
    lblCompKneeValue: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    tbCompKnee: TMfTrackBar;
    tbCompMakeup: TMfTrackBar;
    tbCompRelease: TMfTrackBar;
    tbCompAttack: TMfTrackBar;
    tbCompRatio: TMfTrackBar;
    tbCompThreshold: TMfTrackBar;
    pnlEcho: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lblEchoMixValue: TLabel;
    lblEchoDelayValue: TLabel;
    lblEchoFeedbackValue: TLabel;
    lblEchoToneValue: TLabel;
    lblEchoSpringValue: TLabel;
    lblEchoWowDepthValue: TLabel;
    lblEchoWowRateValue: TLabel;
    tbEchoMix: TMfTrackBar;
    tbEchoDelay: TMfTrackBar;
    tbEchoFeedback: TMfTrackBar;
    tbEchoTone: TMfTrackBar;
    tbEchoSpring: TMfTrackBar;
    tbEchoWowDepth: TMfTrackBar;
    tbEchoWowRate: TMfTrackBar;
    chkEchoEnabled: TMPxpButton;
    chkCompEnabled: TMPxpButton;
    Label13: TLabel;
    btnStart: TMPxpButton;
    btnStop: TMPxpButton;
    pnlNoiseGate: TPanel;
    lblGateThresholdValue: TLabel;
    lblGateReleaseValue: TLabel;
    lblGateAttackValue: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    tbGateAttack: TMfTrackBar;
    tbGateRelease: TMfTrackBar;
    tbGateThreshold: TMfTrackBar;
    chkGateEnabled: TMPxpButton;
    tbGateFloor: TMfTrackBar;
    lblGateFloorValue: TLabel;
    Label18: TLabel;
    lblGateHoldValue: TLabel;
    tbGateHold: TMfTrackBar;
    Label15: TLabel;
    pnlFXButtons: TPanel;
    chkCompressorSettings: TMPxpButton;
    chkEchoSettings: TMPxpButton;
    chkNoiseGate: TMPxpButton;


    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbVolumeChange(Sender: TObject);
    procedure tbBalanceChange(Sender: TObject);
    procedure chkMuteClick(Sender: TObject);
    procedure chkPFLClick(Sender: TObject);
    procedure chkCompressorSettingsClick(Sender: TObject);
    procedure chkEchoSettingsClick(Sender: TObject);
    procedure tbInputGainChange(Sender: TObject);
    procedure chkEchoEnabledClick(Sender: TObject);
    procedure chkCompEnabledClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tbCompThresholdChange(Sender: TObject);
    procedure tbCompRatioChange(Sender: TObject);
    procedure tbCompAttackChange(Sender: TObject);
    procedure tbCompReleaseChange(Sender: TObject);
    procedure tbCompMakeupChange(Sender: TObject);
    procedure tbCompKneeChange(Sender: TObject);
    procedure tbEchoMixChange(Sender: TObject);
    procedure tbEchoDelayChange(Sender: TObject);
    procedure tbEchoFeedbackChange(Sender: TObject);
    procedure tbEchoToneChange(Sender: TObject);
    procedure tbEchoSpringChange(Sender: TObject);
    procedure tbEchoWowDepthChange(Sender: TObject);
    procedure tbEchoWowRateChange(Sender: TObject);
    // Parametric eq
    procedure tbEqQChange(Sender: TObject);
    procedure tbEqCenterFreqHzChange(Sender: TObject);
    procedure tbEqGainDbChange(Sender: TObject);

    procedure tbEqQDblClick(Sender: TObject);
    procedure tbEqCenterFreqHzDblClick(Sender: TObject);
    procedure tbEqGainDbDblClick(Sender: TObject);
    procedure tbVolumeDblClick(Sender: TObject);
    procedure tbInputGainDblClick(Sender: TObject);
    procedure tmrUiTimer(Sender: TObject);
    procedure tbGateThresholdChange(Sender: TObject);
    procedure tbGateAttackChange(Sender: TObject);
    procedure tbGateReleaseChange(Sender: TObject);
    procedure tbGateFloorChange(Sender: TObject);
    procedure chkGateEnabledClick(Sender: TObject);
    procedure tbGateHoldChange(Sender: TObject);
    procedure chkNoiseGateClick(Sender: TObject);
    // Parametric eq end

  private
    { Private declarations }

    FEngine: TMfMicrophoneDeckEngine;

    FMixerRegistered: Boolean;
    FMixerSourceIndex: Integer;
    FInitializingUi: Boolean;
    FCurrentDeviceId: string;
    FLastPeakL: Single;
    FLastPeakR: Single;
    // Parametric eq
    FAudioRack: TMfWasApiEffectsRack;
    FUpdatingAudioRackGui: Boolean;
    // Parametric eq
    function GetEqEffect(): TMfParametricEqEffect;
    procedure BindAudioRack();
    procedure LoadAudioRackToGui();
    // VU
    procedure UpdateMeterUi();
    procedure ClearMeters();

    procedure BuildDefaultUi();
    procedure ApplyAllSettings();
    procedure ApplyInputGain();
    procedure ApplyVolumeAndBalance();
    procedure ApplyMute();
    procedure ApplyPFL();
    procedure ApplyCompressor();
    procedure ApplyEcho();

    procedure UpdateAllCaptions();
    procedure UpdateInputGainCaption();
    procedure UpdateVolumeCaption();
    procedure UpdateCompCaptions();
    procedure UpdateEchoCaptions();

    procedure SetUiRunning(const AValue: Boolean);
    procedure SetSignalLamp(const ALive: Boolean);
    procedure SetClipLamp(const AActive: Boolean);

    function TrackToInputGainDb(): Single;
    function TrackToVolumeLinear(): Single;
    function TrackToBalance(): Single;

    function TrackToCompThresholdDb(): Single;
    function TrackToCompRatio(): Single;
    function TrackToCompAttackMs(): Single;
    function TrackToCompReleaseMs(): Single;
    function TrackToCompMakeupDb(): Single;
    function TrackToCompKneeDb(): Single;

    function TrackToEchoMix(): Single;
    function TrackToEchoDelayMs(): Single;
    function TrackToEchoFeedback(): Single;
    function TrackToEchoTone(): Single;
    function TrackToEchoSpring(): Single;
    function TrackToEchoWowDepthMs(): Single;
    function TrackToEchoWowRateHz(): Single;

    procedure ApplyNoiseGate();
    procedure UpdateNoiseGateCaptions();

    function TrackToGateThresholdDb(): Single;
    function TrackToGateAttackMs(): Single;
    function TrackToGateReleaseMs(): Single;
    function TrackToGateFloorDb(): Single;
    function TrackToGateHoldMs(): Single;

    procedure StartEngine();
    procedure StopEngine();

    procedure RegisterToMixer();
    procedure UnregisterFromMixer();
    procedure ApplyVolumeToMixer();
    procedure ApplyMuteToMixer();
    procedure ApplyPflToMixer();

  public
    { Public declarations }

    function MixerReadOutputPcmFloat32(const Frames: Integer;
                                       const OutBuffer: PSingle;
                                       out Flags: DWORD): HResult;

  end;


implementation

{$R *.dfm}

uses
  System.Math,
  WinApi.CoreAudioApi.AudioClient,
  frmMainMDI;


procedure TFrmMicrophoneDeck.FormCreate(Sender: TObject);
begin

  FEngine := TMfMicrophoneDeckEngine.Create;

  // Parametric eq
  FAudioRack := nil;
  FUpdatingAudioRackGui := False;
  BindAudioRack();

  FMixerRegistered := False;
  FMixerSourceIndex := -1;
  FInitializingUi := True;
  FCurrentDeviceId := '';
  FLastPeakL := 0.0;
  FLastPeakR := 0.0;

  chkGateEnabled.Checked := True;

  tbGateThreshold.Position := -45;
  tbGateAttack.Position := 5;
  tbGateRelease.Position := 120;
  tbGateFloor.Position := -35;
  tbGateHold.Position := 35;

  ClearMeters();
  BuildDefaultUi();
  UpdateAllCaptions();
  SetUiRunning(False);
  FInitializingUi := False;
end;


procedure TFrmMicrophoneDeck.FormDestroy(Sender: TObject);
begin

  tmrUi.Enabled := False;

  StopEngine();
  UnregisterFromMixer();

  FreeAndNil(FEngine);
  FAudioRack := nil;
end;


procedure TFrmMicrophoneDeck.FormShow(Sender: TObject);
begin

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);
 // Height := 1538;
//  Width := 479 - 10;

  LoadAudioRackToGui();
  UpdateAllCaptions();
end;


procedure TFrmMicrophoneDeck.FormClose(Sender: TObject;
                                       var Action: TCloseAction);
begin

  tmrUi.Enabled := False;
  StopEngine();
  UnregisterFromMixer();
  Action := caFree;
end;


procedure TFrmMicrophoneDeck.tbVolumeChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyVolumeAndBalance();
end;


procedure TFrmMicrophoneDeck.tbVolumeDblClick(Sender: TObject);
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


procedure TFrmMicrophoneDeck.tmrUiTimer(Sender: TObject);
begin

  UpdateMeterUi();
end;


procedure TFrmMicrophoneDeck.tbBalanceChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyVolumeAndBalance();
end;


procedure TFrmMicrophoneDeck.tbCompThresholdChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbCompRatioChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbCompAttackChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbCompReleaseChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbCompMakeupChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbCompKneeChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.tbEchoMixChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbEchoDelayChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbEchoFeedbackChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbEchoToneChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbEchoSpringChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbEchoWowDepthChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.tbInputGainChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyInputGain();
end;


procedure TFrmMicrophoneDeck.tbInputGainDblClick(Sender: TObject);
begin

  tbInputGain.AnimateTrackBarToPosition(0,
                                        2);
end;


procedure TFrmMicrophoneDeck.tbEchoWowRateChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;


procedure TFrmMicrophoneDeck.chkMuteClick(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyMute();
end;


procedure TFrmMicrophoneDeck.chkPFLClick(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyPFL();
end;


procedure TFrmMicrophoneDeck.chkNoiseGateClick(Sender: TObject);
begin

  pnlNoiseGate.BringToFront;
  chkNoiseGate.Checked := True;
  chkEchoSettings.Checked := False;
  chkCompressorSettings.Checked := False;
end;


procedure TFrmMicrophoneDeck.chkCompressorSettingsClick(Sender: TObject);
begin

  pnlCompressor.BringToFront;
  chkEchoSettings.Checked := False;
  chkNoiseGate.Checked := False;
  chkCompressorSettings.Checked := True;
end;


procedure TFrmMicrophoneDeck.chkEchoSettingsClick(Sender: TObject);
begin

  pnlEcho.BringToFront;
  chkEchoSettings.Checked := True;
  chkNoiseGate.Checked := False;
  chkCompressorSettings.Checked := False;
end;


procedure TFrmMicrophoneDeck.chkCompEnabledClick(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyCompressor();
end;


procedure TFrmMicrophoneDeck.chkEchoEnabledClick(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyEcho();
end;

// Parametric eq
procedure TFrmMicrophoneDeck.chkGateEnabledClick(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.BindAudioRack();
begin

  FAudioRack := nil;

  if Assigned(FEngine) then
    FAudioRack := FEngine.AudioRack;
end;


procedure TFrmMicrophoneDeck.LoadAudioRackToGui();
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


function TFrmMicrophoneDeck.GetEqEffect(): TMfParametricEqEffect;
var
  I: Integer;
  Fx: TObject;

begin

  Result := nil;

  if not Assigned(FAudioRack) then
    Exit;

  for I := 0 to FAudioRack.Slots.Count - 1 do
    begin

      Fx := TMfWasApiFxSlot(FAudioRack.Slots[I]).Effect;
      if Assigned(Fx) and
         (Fx is TMfParametricEqEffect) then
        begin

          Result := TMfParametricEqEffect(Fx);
          Exit;
        end;
    end;
end;


procedure TFrmMicrophoneDeck.tbEqQChange(Sender: TObject);
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
  if (NewQ <= 0.0) then
    NewQ := 0.1;

  Eq.Q := NewQ;
end;


procedure TFrmMicrophoneDeck.tbEqCenterFreqHzChange(Sender: TObject);
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


procedure TFrmMicrophoneDeck.tbEqGainDbChange(Sender: TObject);
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


// Noise gate

procedure TFrmMicrophoneDeck.tbGateThresholdChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.tbGateAttackChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.tbGateReleaseChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.tbGateFloorChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.tbGateHoldChange(Sender: TObject);
begin

  if FInitializingUi then
    Exit;

  ApplyNoiseGate();
end;


procedure TFrmMicrophoneDeck.tbEqQDblClick(Sender: TObject);
begin

  tbEqQ.AnimateTrackBarToPosition(10,
                                  2);
end;


procedure TFrmMicrophoneDeck.tbEqCenterFreqHzDblClick(Sender: TObject);
begin

  tbEqCenterFreqHz.AnimateTrackBarToPosition(1500,
                                             2);
end;


procedure TFrmMicrophoneDeck.tbEqGainDbDblClick(Sender: TObject);
begin

  tbEqGainDb.AnimateTrackBarToPosition(0,
                                       2);
end;


// VU
procedure TFrmMicrophoneDeck.ClearMeters();
begin

  pmLeft.AudioEnded();
  pmRight.AudioEnded();
  SetClipLamp(False);
end;


procedure TFrmMicrophoneDeck.UpdateMeterUi();
var
  PeakL: Single;
  PeakR: Single;
  //PeakMax: Single;
  MeterWfx: TWAVEFORMATEX;
  //SampleL: array[0..1] of Single;
  //SampleR: array[0..1] of Single;
  SampleL: array[0..15] of Single;
  SampleR: array[0..15] of Single;
  I: Integer;

begin

  if not Assigned(FEngine) then
    begin
      ClearMeters();
      Exit;
    end;

  PeakL := EnsureRange(FEngine.GetMeterPeakL(),
                       0.0,
                       1.0);
  PeakR := EnsureRange(FEngine.GetMeterPeakR(),
                       0.0,
                       1.0);

  FLastPeakL := PeakL;
  FLastPeakR := PeakR;
  //PeakMax := Max(PeakL,
  //               PeakR);

  FillChar(MeterWfx,
           SizeOf(MeterWfx),
           0);
  MeterWfx.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
  MeterWfx.nChannels := 2;
  MeterWfx.nSamplesPerSec := 44100;
  MeterWfx.wBitsPerSample := 32;
  MeterWfx.nBlockAlign := (MeterWfx.nChannels * MeterWfx.wBitsPerSample) div 8;
  MeterWfx.nAvgBytesPerSec := MeterWfx.nSamplesPerSec * MeterWfx.nBlockAlign;

  // Feed a very small synthetic stereo block.
  // Left meter gets left-level signal only, right meter gets right-level signal only.
  //SampleL[0] := PeakL;
  //SampleL[1] := PeakL;

  //SampleR[0] := PeakR;
  //SampleR[1] := PeakR;
  // This gives a smoother VU effect.
  for I := 0 to 7 do
    begin

      SampleL[I * 2] := PeakL;
      SampleL[I * 2 + 1] := PeakL;

      SampleR[I * 2] := PeakR;
      SampleR[I * 2 + 1] := PeakR;
    end;

  pmLeft.PushPcm(PByte(@SampleL[0]),
                 SizeOf(SampleL),
                 @MeterWfx);

  pmRight.PushPcm(PByte(@SampleR[0]),
                  SizeOf(SampleR),
                  @MeterWfx);

  SetClipLamp((PeakL >= 0.98) or
              (PeakR >= 0.98));
end;


procedure TfrmMicrophoneDeck.BuildDefaultUi();
begin


  tbInputGain.Minimum := -240;
  tbInputGain.Maximum := 360;
  //tbInputGain.Frequency := 60;
  tbInputGain.Position := 0;

  tbVolume.Minimum := 0;
  tbVolume.Maximum := 200;
  //tbVolume.Frequency := 25;

  tbBalance.Minimum := -100;
  tbBalance.Maximum := 100;
  //tbBalance.Frequency := 25;
  tbBalance.Position := 0;

  chkMute.Checked := False;
  chkPFL.Checked := False;
  chkCompEnabled.Checked := True;

  tbCompThreshold.Minimum := -600;
  tbCompThreshold.Maximum := 0;
  //tbCompThreshold.Frequency := 60;
  tbCompThreshold.Position := -180;

  tbCompRatio.Minimum := 10;
  tbCompRatio.Maximum := 200;
  //tbCompRatio.Frequency := 10;
  tbCompRatio.Position := 25;

  tbCompAttack.Minimum := 1;
  tbCompAttack.Maximum := 250;
  //tbCompAttack.Frequency := 10;
  tbCompAttack.Position := 8;

  tbCompRelease.Minimum := 5;
  tbCompRelease.Maximum := 2000;
  //tbCompRelease.Frequency := 100;
  tbCompRelease.Position := 120;

  tbCompMakeup.Minimum := -120;
  tbCompMakeup.Maximum := 240;
  //tbCompMakeup.Frequency := 20;
  tbCompMakeup.Position := 20;

  tbCompKnee.Minimum := 0;
  tbCompKnee.Maximum := 240;
  //tbCompKnee.Frequency := 20;
  tbCompKnee.Position := 40;

  chkEchoEnabled.Checked := True;

  tbEchoMix.Minimum := 0;
  tbEchoMix.Maximum := 100;
  //tbEchoMix.Frequency := 10;
  tbEchoMix.Position := 14;

  tbEchoDelay.Minimum := 20;
  tbEchoDelay.Maximum := 500;
  //tbEchoDelay.Frequency := 20;
  tbEchoDelay.Position := 95;

  tbEchoFeedback.Minimum := 0;
  tbEchoFeedback.Maximum := 92;
  //tbEchoFeedback.Frequency := 10;
  tbEchoFeedback.Position := 22;

  tbEchoTone.Minimum := 0;
  tbEchoTone.Maximum := 100;
  //tbEchoTone.Frequency := 10;
  tbEchoTone.Position := 45;

  tbEchoSpring.Minimum := 0;
  tbEchoSpring.Maximum := 100;
  //tbEchoSpring.Frequency := 10;
  tbEchoSpring.Position := 35;

  tbEchoWowDepth.Minimum := 0;
  tbEchoWowDepth.Maximum := 80;
  //tbEchoWowDepth.Frequency := 10;
  tbEchoWowDepth.Position := 7;

  tbEchoWowRate.Minimum := 1;
  tbEchoWowRate.Maximum := 800;
  //tbEchoWowRate.Frequency := 50;
  tbEchoWowRate.Position := 45;

  tmrUi.Interval := 40;
  tmrUi.Enabled := False;

  SetSignalLamp(False);
  SetClipLamp(False);
end;


procedure TfrmMicrophoneDeck.ApplyAllSettings();
begin

  ApplyInputGain();
  ApplyNoiseGate();
  ApplyVolumeAndBalance();
  ApplyMute();
  ApplyPFL();
  ApplyCompressor();
  ApplyEcho();
  UpdateAllCaptions();
end;


procedure TfrmMicrophoneDeck.ApplyInputGain();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetInputGainDb(TrackToInputGainDb);
  UpdateInputGainCaption();
end;


procedure TfrmMicrophoneDeck.ApplyVolumeAndBalance();
var
  Vol: Single;
  Bal: Single;
  L: Single;
  R: Single;

begin

  if not Assigned(FEngine) then
    Exit;

  Vol := TrackToVolumeLinear();
  Bal := TrackToBalance();

  if (Bal < 0.0) then
    begin
      L := Vol;
      R := Vol * (1.0 + Bal);
    end
  else
    begin
      L := Vol * (1.0 - Bal);
      R := Vol;
    end;

  L := EnsureRange(L,
                   0.0,
                   1.0);
  R := EnsureRange(R,
                   0.0,
                   1.0);

  FEngine.SetVolume(L,
                    R);

  ApplyVolumeToMixer();
  UpdateVolumeCaption();
end;


procedure TfrmMicrophoneDeck.ApplyMute();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetMute(Assigned(chkMute) and chkMute.Checked);
  ApplyMuteToMixer();
end;


procedure TfrmMicrophoneDeck.ApplyPFL();
begin

  ApplyPflToMixer();
end;


procedure TfrmMicrophoneDeck.ApplyCompressor();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetCompressorEnabled(Assigned(chkCompEnabled) and chkCompEnabled.Checked);
  FEngine.SetCompressorParams(TrackToCompThresholdDb(),
                              TrackToCompRatio(),
                              TrackToCompAttackMs(),
                              TrackToCompReleaseMs(),
                              TrackToCompMakeupDb(),
                              TrackToCompKneeDb());
  UpdateCompCaptions();
end;


procedure TfrmMicrophoneDeck.ApplyEcho();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetEchoEnabled(chkEchoEnabled.Checked);
  if chkEchoEnabled.Checked then
    FEngine.SetEchoParams(TrackToEchoMix(),
                          TrackToEchoDelayMs(),
                          TrackToEchoFeedback(),
                          TrackToEchoTone(),
                          TrackToEchoSpring(),
                          TrackToEchoWowDepthMs(),
                          TrackToEchoWowRateHz());
  UpdateEchoCaptions();
end;


procedure TfrmMicrophoneDeck.UpdateAllCaptions();
begin

  UpdateInputGainCaption();
  UpdateNoiseGateCaptions();
  UpdateVolumeCaption();
  UpdateCompCaptions();
  UpdateEchoCaptions();
end;


procedure TfrmMicrophoneDeck.UpdateInputGainCaption();
begin

  lblInputGainValue.Caption := Format('%+.1f dB',
                                      [TrackToInputGainDb]);
end;


procedure TfrmMicrophoneDeck.UpdateVolumeCaption();
begin

  lblVolumePerc.Caption := Format('%.0f %%',
                                  [TrackToVolumeLinear * 50.0]);
end;


procedure TfrmMicrophoneDeck.UpdateCompCaptions();
begin

    lblCompThresholdValue.Caption := Format('%.1f dB',
                                            [TrackToCompThresholdDb]);
    lblCompRatioValue.Caption := Format('%.1f : 1',
                                        [TrackToCompRatio]);
    lblCompAttackValue.Caption := Format('%.0f ms',
                                         [TrackToCompAttackMs]);
    lblCompReleaseValue.Caption := Format('%.0f ms',
                                          [TrackToCompReleaseMs]);
    lblCompMakeupValue.Caption := Format('%.1f dB',
                                         [TrackToCompMakeupDb]);
    lblCompKneeValue.Caption := Format('%.1f dB',
                                       [TrackToCompKneeDb]);
end;


procedure TfrmMicrophoneDeck.UpdateEchoCaptions();
begin

    lblEchoMixValue.Caption := Format('%.0f %%',
                                      [TrackToEchoMix * 100.0]);
    lblEchoDelayValue.Caption := Format('%.0f ms',
                                        [TrackToEchoDelayMs]);
    lblEchoFeedbackValue.Caption := Format('%.0f %%',
                                           [TrackToEchoFeedback * 100.0]);
    lblEchoToneValue.Caption := Format('%.0f %%',
                                       [TrackToEchoTone * 100.0]);
    lblEchoSpringValue.Caption := Format('%.0f %%',
                                         [TrackToEchoSpring * 100.0]);
    lblEchoWowDepthValue.Caption := Format('%.2f ms',
                                           [TrackToEchoWowDepthMs]);
    lblEchoWowRateValue.Caption := Format('%.2f Hz',
                                          [TrackToEchoWowRateHz]);
end;


procedure TfrmMicrophoneDeck.SetUiRunning(const AValue: Boolean);
begin

  if Assigned(tmrUi) then
    tmrUi.Enabled := AValue;

  pmLeft.Enabled := AValue;
  pmRight.Enabled := AValue;
  SetSignalLamp(AValue);
end;


procedure TfrmMicrophoneDeck.SetSignalLamp(const ALive: Boolean);
const
  ON_COLOR = clRed;
  ON_BRIGHT_COLOR = $001B4DFA; // bright red/orange
  OFF_COLOR = $00568000;

begin

  if ALive then
    begin

      shpSignal.Brush.Color := ON_COLOR;
      shpSignal.Pen.Color := ON_COLOR;
      shpSignalCap.Pen.Color := ON_COLOR;
      lblSignal.Font.Color := ON_BRIGHT_COLOR;
      lblSignal.Caption := 'MIC ON';
    end
  else
    begin

      shpSignal.Brush.Color := OFF_COLOR;
      shpSignal.Pen.Color := OFF_COLOR;
      shpSignalCap.Pen.Color := OFF_COLOR;
      lblSignal.Font.Color := OFF_COLOR;
      lblSignal.Caption := 'MIC OFF';
    end;
end;


procedure TfrmMicrophoneDeck.SetClipLamp(const AActive: Boolean);
begin

  if AActive then
    shpClip.Brush.Color := clRed
  else
    shpClip.Brush.Color := clDkGray;
end;


function TfrmMicrophoneDeck.TrackToInputGainDb(): Single;
begin

  Result := tbInputGain.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToVolumeLinear(): Single;
begin

  Result := tbVolume.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToBalance(): Single;
begin

  Result := tbBalance.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToCompThresholdDb(): Single;
begin

  Result := tbCompThreshold.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToCompRatio(): Single;
begin

  Result := tbCompRatio.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToCompAttackMs(): Single;
begin

   Result := tbCompAttack.Position;
end;


function TfrmMicrophoneDeck.TrackToCompReleaseMs(): Single;
begin

  Result := tbCompRelease.Position;
end;


function TfrmMicrophoneDeck.TrackToCompMakeupDb(): Single;
begin

  Result := tbCompMakeup.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToCompKneeDb(): Single;
begin

  Result := tbCompKnee.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToEchoMix(): Single;
begin

  Result := tbEchoMix.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToEchoDelayMs(): Single;
begin

    Result := tbEchoDelay.Position;
end;


function TfrmMicrophoneDeck.TrackToEchoFeedback(): Single;
begin

  Result := tbEchoFeedback.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToEchoTone(): Single;
begin

  Result := tbEchoTone.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToEchoSpring(): Single;
begin

  Result := tbEchoSpring.Position / 100.0;
end;


function TfrmMicrophoneDeck.TrackToEchoWowDepthMs(): Single;
begin

  Result := tbEchoWowDepth.Position / 10.0;
end;


function TfrmMicrophoneDeck.TrackToEchoWowRateHz(): Single;
begin

  Result := tbEchoWowRate.Position / 100.0;
end;


// Noise gate
procedure TFrmMicrophoneDeck.UpdateNoiseGateCaptions();
begin

  lblGateThresholdValue.Caption := Format('%.0f dB',
                                          [TrackToGateThresholdDb()]);

  lblGateAttackValue.Caption := Format('%.0f ms',
                                       [TrackToGateAttackMs()]);

  lblGateReleaseValue.Caption := Format('%.0f ms',
                                        [TrackToGateReleaseMs()]);

  lblGateFloorValue.Caption := Format('%.0f dB',
                                      [TrackToGateFloorDb()]);

  lblGateHoldValue.Caption := Format('%.0f ms',
                                     [TrackToGateHoldMs()]);
end;


procedure TFrmMicrophoneDeck.ApplyNoiseGate();
begin

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetNoiseGateEnabled(chkGateEnabled.Checked);

  FEngine.SetNoiseGateParams(TrackToGateThresholdDb(),
                             TrackToGateAttackMs(),
                             TrackToGateReleaseMs(),
                             TrackToGateFloorDb(),
                             TrackToGateHoldMs());

  UpdateNoiseGateCaptions();
end;


function TFrmMicrophoneDeck.TrackToGateThresholdDb(): Single;
begin

  Result := tbGateThreshold.Position;
end;


function TFrmMicrophoneDeck.TrackToGateAttackMs(): Single;
begin

  Result := tbGateAttack.Position;
end;


function TFrmMicrophoneDeck.TrackToGateReleaseMs(): Single;
begin

  Result := tbGateRelease.Position;
end;


function TFrmMicrophoneDeck.TrackToGateFloorDb(): Single;
begin

  Result := tbGateFloor.Position;
end;


function TFrmMicrophoneDeck.TrackToGateHoldMs(): Single;
begin

  Result := tbGateHold.Position;
end;


procedure TfrmMicrophoneDeck.StartEngine();
var
  setup: TRDJSetup;
  hr: HResult;
  DevId: string;

begin

  if not Assigned(FEngine) then
    Exit;

  setup := MainMDIFrm.Setup;

  DevId := setup.MicDeviceId;
  FCurrentDeviceId := DevId;

  hr := FEngine.OpenDevice(DevId);
  if Failed(hr) then
    raise Exception.CreateFmt('Could not open microphone input. HResult = $%.8x',
                              [Cardinal(hr)]);

  ApplyAllSettings();

  hr := FEngine.Start();
  if Failed(hr) then
    raise Exception.CreateFmt('Could not start microphone input. HResult = $%.8x',
                              [Cardinal(hr)]);

  RegisterToMixer();
  SetUiRunning(True);
end;


procedure TfrmMicrophoneDeck.StopEngine();
begin

  if Assigned(FEngine) then
    FEngine.Stop();

  SetUiRunning(False);
  ClearMeters();
end;


procedure TfrmMicrophoneDeck.RegisterToMixer();
var
  Ch: TRDJMixerChannel;

begin

  if FMixerRegistered then
    Exit;

  if not Assigned(MainMDIFrm) or
     not Assigned(MainMDIFrm.InternalMixer) or
     not Assigned(FEngine) then
    Exit;

  Ch := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);
  if (Ch = nil) then
    begin

      Ch := MainMDIFrm.InternalMixer.AddChannel();
      if (Ch = nil) then
        Exit;
    end;

  Ch.DeckEngineObj := FEngine;
  Ch.OnReadOutputPcmFloat32 := MixerReadOutputPcmFloat32;
  Ch.Enabled := True;
  Ch.CueEnabled := False;
  Ch.CueVolL := 0.0;
  Ch.CueVolR := 0.0;

  FMixerSourceIndex := MainMDIFrm.InternalMixer.Channels.IndexOf(Ch);
  FMixerRegistered := True;

  ApplyVolumeToMixer();
  ApplyMuteToMixer();
  ApplyPflToMixer();
end;


procedure TfrmMicrophoneDeck.UnregisterFromMixer();
var
  Ch: TRDJMixerChannel;

begin

  if not FMixerRegistered then
    Exit;

  if Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.InternalMixer) and
     Assigned(FEngine) then
    begin
      Ch := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);
      if (Ch <> nil) then
        MainMDIFrm.InternalMixer.RemoveChannel(Ch);
    end;

  FMixerSourceIndex := -1;
  FMixerRegistered := False;
end;


procedure TfrmMicrophoneDeck.ApplyVolumeToMixer;
var
  Vol: Single;
  Bal: Single;
  L: Single;
  R: Single;
  Ch: TRDJMixerChannel;

begin

  if not FMixerRegistered then
    Exit;

  Vol := TrackToVolumeLinear();
  Bal := TrackToBalance();

  if (Bal < 0.0) then
    begin

      L := Vol;
      R := Vol * (1.0 + Bal);
    end
  else
    begin

      L := Vol * (1.0 - Bal);
      R := Vol;
    end;

  L := EnsureRange(L,
                   0.0,
                   2.0);
  R := EnsureRange(R,
                   0.0,
                   2.0);

  // debug
  {  OutputDebugString(PChar(Format('Mic ApplyVolumeToMixer: Vol=%.6f Bal=%.6f L=%.6f R=%.6f',
                                 [Vol,
                                  Bal,
                                  L,
                                  R])));
  }
  if Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.InternalMixer) and
     Assigned(FEngine) then
    begin

      Ch := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);
      if (Ch <> nil) then
        begin

          Ch.VolL := L;
          Ch.VolR := R;
        end;
    end;
end;


procedure TFrmMicrophoneDeck.btnStartClick(Sender: TObject);
begin


  StartEngine();
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  SetUiRunning(True);
end;


procedure TFrmMicrophoneDeck.btnStopClick(Sender: TObject);
begin

   StopEngine();
   btnStart.Enabled := True;
   btnStop.Enabled := False;
   SetUiRunning(False);
end;


procedure TfrmMicrophoneDeck.ApplyMuteToMixer();
var
  Ch: TRDJMixerChannel;

begin

  if not FMixerRegistered then
    Exit;

  if Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.InternalMixer) and
     Assigned(FEngine) then
    begin

      Ch := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);
      if (Ch <> nil) then
        Ch.Muted := chkMute.Checked;
    end;
end;


procedure TfrmMicrophoneDeck.ApplyPflToMixer();
var
  Ch: TRDJMixerChannel;
  Vol: Single;
  Bal: Single;
  L: Single;
  R: Single;
  CueActive: Boolean;

begin

  if not FMixerRegistered then
    Exit;

  if not Assigned(MainMDIFrm) or
     not Assigned(MainMDIFrm.InternalMixer) then
    Exit;

  Ch := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FEngine);
  if not Assigned(Ch) then
    Exit;

  CueActive := False;

  if Assigned(chkPFL) then
    CueActive := chkPFL.Checked and MainMDIFrm.Setup.PFLEnabled;

  Ch.CueEnabled := CueActive;

  if CueActive then
    begin

      Vol := TrackToVolumeLinear;
      Bal := TrackToBalance;

      if (Bal < 0.0) then
        begin

          L := Vol;
          R := Vol * (1.0 + Bal);
        end
      else
        begin

          L := Vol * (1.0 - Bal);
          R := Vol;
        end;

      L := EnsureRange(L,
                       0.0,
                       2.0);
      R := EnsureRange(R,
                       0.0,
                       2.0);

      if Assigned(chkMute) and chkMute.Checked then
        begin

          L := 0.0;
          R := 0.0;
        end;

      Ch.CueVolL := L;
      Ch.CueVolR := R;
    end
  else
    begin

      Ch.CueVolL := 0.0;
      Ch.CueVolR := 0.0;
    end;
end;


function TfrmMicrophoneDeck.MixerReadOutputPcmFloat32(const Frames: Integer;
                                                      const OutBuffer: PSingle;
                                                      out Flags: DWORD): HResult;
begin

  Flags := 0;

  if (OutBuffer = nil) or
     (Frames <= 0) then
    Exit(S_OK);

  if not Assigned(FEngine) or
     not FEngine.IsRunning then
    begin

      FillChar(OutBuffer^,
               Frames * 2 * SizeOf(Single),
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  Result := FEngine.ReadOutputPcmFloat32(OutBuffer,
                                         Frames);

  if Failed(Result) then
    begin

      FillChar(OutBuffer^,
               Frames * 2 * SizeOf(Single),
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Result := S_OK;
    end;
end;

end.
