// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 24-08-2024
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Main window that only is the frontend, no calculations or whatever that
//              could interference with the threading model of this application.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source:  https://learn.microsoft.com/en-us/windows/win32/coreaudio/rendering-a-stream
//          https://matthewvaneerde.wordpress.com/2008/12/10/sample-playing-silence-via-wasapi-event-driven-pull-mode/
//
// Copyright (c) FactoryX All rights reserved.
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
unit MainFrm;

interface

uses

  {Winapi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Messages,
  {ActiveX}
  Winapi.ActiveX,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Diagnostics,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  WASAPIEngine,
  // MfPack components.
  MfPeakMeter,
  // FX components.
  MfWasApiFxComponentBase,
  MfFlangerEchoComponent,
  MfParametricEqComponent,
  MfCompressorLimiterComponent,
  MfWasApiEffectsRack,
  MfWasApiPlayerEngineComponent;

const


  // Global hotkey's.

  HK_TOGGLE_PLAY = 1;
  HK_TOGGLE_PAUSE = 2;
  HK_TOGGLE_STOP = 3;
  HK_VOLUME_UP = 4;
  HK_VOLUME_DOWN = 5;
  HK_GUI_SHOW = 6;
  HK_GUI_HIDE = 7;

type

  TfrmMain = class(TForm)

    pnlControls: TPanel;
    pnlTrackbar: TPanel;
    butPlayPause: TButton;
    butStop: TButton;
    lblDuration: TLabel;
    lblProcessed: TLabel;
    lblPlayed: TLabel;
    pmRight: TMfPeakMeter;
    pmLeft: TMfPeakMeter;
    Label1: TLabel;
    Label2: TLabel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    cbLockVolumeSliders: TCheckBox;
    trbVolumeL: TTrackBar;
    trbVolumeR: TTrackBar;
    mnuMain: TMainMenu;
    OpenAudioFile1: TMenuItem;
    Exit1: TMenuItem;
    dlgOpen: TOpenDialog;
    pbProgress: TProgressBar;
    lblBarPositionInSTime: TLabel;
    lblBarPositionInSamples: TLabel;
    stxtStatus: TStaticText;

    // ------------------------------------------------------------------------
    // FX UI controls (created in DFM)
    // ------------------------------------------------------------------------

    gbEQ: TGroupBox;
    cbEqEnabled: TCheckBox;
    cbEqUseBW: TCheckBox;
    cbEqTruePeak: TCheckBox;
    lblEqGain: TLabel;
    lblEqFreq: TLabel;
    lblEqQ: TLabel;
    lblEqBW: TLabel;
    lblEqTP: TLabel;
    trbEqGain: TTrackBar;
    trbEqFreq: TTrackBar;
    trbEqQ: TTrackBar;
    trbEqBW: TTrackBar;

    gbFlanger: TGroupBox;
    cbFlEnabled: TCheckBox;
    lblFlWet: TLabel;
    lblFlDelay: TLabel;
    lblFlDepth: TLabel;
    lblFlRate: TLabel;
    lblFlFeedback: TLabel;
    trbFlWet: TTrackBar;
    trbFlDelay: TTrackBar;
    trbFlDepth: TTrackBar;
    trbFlRate: TTrackBar;
    trbFlFeedback: TTrackBar;

    gbDynamics: TGroupBox;
    cbDynEnabled: TCheckBox;
    cbDynRms: TCheckBox;
    cbDynAutoMakeup: TCheckBox;

    lblDynThresh: TLabel;
    lblDynRatio: TLabel;
    lblDynAttack: TLabel;
    lblDynRelease: TLabel;
    lblDynKnee: TLabel;
    lblDynMakeup: TLabel;
    lblDynLimCeil: TLabel;
    lblDynLimLook: TLabel;
    lblDynLimRel: TLabel;
    lblDynTP: TLabel;
    lblDynCompGR: TLabel;
    lblDynLimGR: TLabel;

    trbDynThresh: TTrackBar;   // dB * 10  (-600..0)
    trbDynRatio: TTrackBar;    // ratio * 10 (10..500)
    trbDynAttack: TTrackBar;   // ms * 10 (1..5000 => 0.1..500ms)
    trbDynRelease: TTrackBar;  // ms (1..5000)
    trbDynKnee: TTrackBar;     // dB * 10 (0..240)
    trbDynMakeup: TTrackBar;   // dB * 10 (0..240)
    trbDynLimCeil: TTrackBar;  // dB * 10 (-240..0)
    trbDynLimLook: TTrackBar;  // ms (0..20)
    trbDynLimRel: TTrackBar;   // ms (1..5000)
    cbDynTruePeak: TCheckBox;
    cmbDynTPOS: TComboBox;
    btnLoad: TButton;
    Bevel1: TBevel;

    // FX Components
    MfWasApiPlayerEngine: TMfWasApiPlayerEngine;
    FXCompressorLimiter: TMfCompressorLimiterEffect;
    FXParametricEq: TMfParametricEqEffect;
    FXFlangerEcho: TMfFlangerEchoEffect;
    waFxRack: TMfWasApiEffectsRack;

    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butPauseClick(Sender: TObject);

    // Seek with progressbar.
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure FormCreate(Sender: TObject);

    // EQ UI events
    procedure cbEqEnabledClick(Sender: TObject);
    procedure cbEqUseBWClick(Sender: TObject);
    procedure cbEqTruePeakClick(Sender: TObject);
    procedure trbEqGainChange(Sender: TObject);
    procedure trbEqFreqChange(Sender: TObject);
    procedure trbEqQChange(Sender: TObject);
    procedure trbEqBWChange(Sender: TObject);

    // Flanger UI events
    procedure cbFlEnabledClick(Sender: TObject);
    procedure trbFlWetChange(Sender: TObject);
    procedure trbFlDelayChange(Sender: TObject);
    procedure trbFlDepthChange(Sender: TObject);
    procedure trbFlRateChange(Sender: TObject);
    procedure trbFlFeedbackChange(Sender: TObject);

    // Dynamics UI events
    procedure cbDynEnabledClick(Sender: TObject);
    procedure cbDynRmsClick(Sender: TObject);
    procedure cbDynAutoMakeupClick(Sender: TObject);
    procedure cbDynTruePeakClick(Sender: TObject);
    procedure cmbDynTPOSChange(Sender: TObject);
    procedure trbDynThreshChange(Sender: TObject);
    procedure trbDynRatioChange(Sender: TObject);
    procedure trbDynAttackChange(Sender: TObject);
    procedure trbDynReleaseChange(Sender: TObject);
    procedure trbDynKneeChange(Sender: TObject);
    procedure trbDynMakeupChange(Sender: TObject);
    procedure trbDynLimCeilChange(Sender: TObject);
    procedure trbDynLimLookChange(Sender: TObject);
    procedure trbDynLimRelChange(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);

  private
    { Private declarations }

    //FEffectsRack: TMfWasApiEffectsRack;

    FAudioFileUrl: TFileName;
    FFileName: string; // Filename without path.
    llAudioDuration: LONGLONG;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();

    /// <summary>Keep track of data been played.</summary>
    procedure OnAudioDataProcessed(Sender: TObject;
                                   const Position100ns: Int64;
                                   const RawPosition: UInt64);

    /// <summary>Signals the audio is ready to play.</summary>
    procedure OnAudioReady(Sender: TObject);

    /// <summary>Signals the audio reached end.</summary>
    procedure OnAudioEnded(Sender: TObject);

    /// <summary>Signals an engine error.</summary>
    procedure OnEngineError(Sender: TObject;
                            const Msg: string;
                            const Hr: HRESULT);

    /// <summary>Signals the rendering engine state.</summary>
    procedure OnEngineState(Sender: TObject;
                            const NewState: TDeviceState);

    // FX UI helpers ----------------------------------------------------------

    function EqFreqSliderToHz(const Slider: Integer): Single;
    function EqHzToFreqSlider(const Hz: Single): Integer;

    procedure UpdateEqLabels();
    procedure UpdateFlangerLabels();
    procedure UpdateDynamicsLabels();
    procedure UpdateDynamicsMeters();

    procedure ApplyEqFromUI();
    procedure ApplyFlangerFromUI();
    procedure ApplyDynamicsFromUI();

    // Hot keys
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
  public
    { Public declarations }

  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

uses
  System.Math;

procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  hr: HResult;

begin

  // Select an audiofile.
  fAudioFileUrl := GetAudioFile();
  if (fAudioFileUrl = 'No audiofile selected.') then
    Exit;

  fFileName := ExtractFileName(fAudioFileUrl);

  // Get the length of the audiofile.
  hr := GetFileDuration(StrToPWideChar(fAudioFileUrl),
                        llAudioDuration);
  if FAILED(hr) then
    begin

      ShowMessage('Could not retrieve the duration of the audio file.');
      llAudioDuration := 0;
    end;

  if SUCCEEDED(hr) then
    begin

      lblDuration.Caption := Format('Duration: %s',
                                    [HnsTimeToStr(llAudioDuration, False)]);

      // Set progressbar max
      pbProgress.Max := llAudioDuration div 1000000;
      // Engine is provided by component (created in FormCreate)
    end;

  if SUCCEEDED(hr) then
    begin

      // Wire engine events (Sample 4 principle: callbacks/events only) >> done in constructor
      //fPlayerEngine.OnReady := OnAudioReady;
     // fPlayerEngine.OnProcessed := OnAudioDataProcessed;
     // fPlayerEngine.OnEnded := OnAudioEnded;
     // fPlayerEngine.OnError := OnEngineError;
     // fPlayerEngine.OnStateChanged := OnEngineState;

      stxtStatus.Caption := Format('Selected file: %s.',
                                   [fFileName]);
    end;

  // Initialize the engine.
  if SUCCEEDED(hr) then
    hr := MfWasApiPlayerEngine.OpenFile(fAudioFileUrl,
                                        llAudioDuration);

  if FAILED(hr) then
    stxtStatus.Caption := Format('Selected file: %s open file failed with error: %d.',
                                [fFileName, hr]);
end;


procedure TfrmMain.butPauseClick(Sender: TObject);
begin

  // Play/Pause is handled by the Play button.
  butPlayPauseClick(Sender);
end;


procedure TfrmMain.butPlayPauseClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(MfWasApiPlayerEngine) then
    Exit;

  case MfWasApiPlayerEngine.DeviceState of

    dsPlay:
      begin

        hr := MfWasApiPlayerEngine.Pause();
      end;

    dsReady,
     dsStop,
     dsPause:
       begin

         // Activate the peakmeters.
         pmLeft.Enabled := True;
         pmRight.Enabled := True;

        // Keep volume on previous volume.
        SetVolumeChannels();
        hr := MfWasApiPlayerEngine.Start();
      end;
  else

    Exit;
  end;

  if FAILED(hr) then
    stxtStatus.Caption := Format('Play/Pause failed for file: %s with Error: %d',
                                [fFileName, hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(MfWasApiPlayerEngine) then
    Exit;

  hr := MfWasApiPlayerEngine.Stop();

  if SUCCEEDED(hr) then
    stxtStatus.Caption := Format('Stopped: %s.',
                                [fFileName])
  else
    stxtStatus.Caption := Format('Stopped: %s failed with Error: %s',
                                [fFileName, hr])
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;

  FreeAndNil(pmLeft);
  FreeAndNil(pmRight);

  CanClose := True;
end;


procedure TfrmMain.WMHotKey(var Msg: TWMHotKey);
begin

  inherited;

  // Global hotkey values.
  // HK_TOGGLE_PLAY   = 1
  // HK_TOGGLE_PAUSE  = 2
  // HK_TOGGLE_STOP   = 3
  // HK_VOLUME_UP     = 4
  // HK_VOLUME_DOWN   = 5
  // HK_GUI_SHOW      = 6
  // HK_GUI_HIDE      = 7

  case Msg.HotKey of

    HK_TOGGLE_PLAY,
    HK_TOGGLE_PAUSE:
      begin

        butPlayPauseClick(nil);
      end;

    HK_TOGGLE_STOP:
      begin

        butStopClick(nil)
      end;

    HK_VOLUME_UP:
      begin

        trbVolumeL.Position :=  trbVolumeL.Position - 1;
        trbVolumeRChange(trbVolumeL);

        trbVolumeR.Position := trbVolumeR.Position - 1;
        trbVolumeRChange(trbVolumeR);
      end;

    HK_VOLUME_DOWN:
      begin

        trbVolumeL.Position := trbVolumeL.Position + 1;
        trbVolumeLChange(trbVolumeL);

        trbVolumeR.Position := trbVolumeR.Position + 1;
        trbVolumeRChange(trbVolumeR);
      end;

    HK_GUI_SHOW:
      begin

        Self.Show();
      end;

    HK_GUI_HIDE:
      begin

        Self.Hide();
      end;
  end;
end;


function TfrmMain.GetAudioFile(): string;
begin

  Result := 'No audiofile selected.';
  dlgOpen.FileName := '';
  if not dlgOpen.Execute(Handle) then
    Exit;

  Result := dlgOpen.FileName;
end;


procedure TfrmMain.SetVolumeChannels();
var
  hr: HResult;

begin

  hr := E_FAIL;

  if not Assigned(MfWasApiPlayerEngine) then
    Exit;

  // Stereo
  // The first stereo channel (0) is always the LEFT one! SetVolumes
  if (MfWasApiPlayerEngine.SoundChannels = 2) then
    begin
      hr := MfWasApiPlayerEngine.SetVolumes(Abs(trbVolumeL.Position) * 0.01,
                                            Abs(trbVolumeR.Position) * 0.01);
    end;

  if FAILED(hr) then
    stxtStatus.Caption := Format('Adjusting volumes failed with error: %d.',
                                [hr]);
end;


// FX UI helpers =============================================================

function TfrmMain.EqFreqSliderToHz(const Slider: Integer): Single;
var
  t: Double;
  loHz,
  hiHz: Double;

begin

  // Slider 0..1000 -> 20..20000 Hz logarithmic mapping.
  loHz := 10.0;
  hiHz := 22000.0;

  t := EnsureRange(Slider / 1000.0,
                    0.0,
                    1.0);

  Result := (loHz * Power(hiHz / loHz, t)) * 1.0;
end;


function TfrmMain.EqHzToFreqSlider(const Hz: Single): Integer;
var
  loHz, hiHz: Double;
  x: Double;

begin

  loHz := 10.0;
  hiHz := 22000.0;

  x := EnsureRange(Hz,
                   loHz,
                   hiHz);

  Result := Round(1000.0 * (Ln(x / loHz) / Ln(hiHz / loHz)));

  Result := EnsureRange(Result,
                        0,
                        1000);
end;


procedure TfrmMain.UpdateEqLabels();
var
  g: Single;
  f: Single;
  q: Single;
  bw: Single;

begin

  g := trbEqGain.Position / 10.0;
  f := EqFreqSliderToHz(trbEqFreq.Position);
  q := trbEqQ.Position / 10.0;
  bw := trbEqBW.Position / 10.0;

  lblEqGain.Caption := Format('Gain: %.1f dB',
                              [g]);

  lblEqFreq.Caption := Format('Freq: %.0f Hz',
                              [f]);

  if cbEqUseBW.Checked then
    begin

      lblEqBW.Caption := Format('BW: %.2f oct',
                                [bw]);
      lblEqQ.Caption := 'Q: (derived)';
    end
  else
    begin

      lblEqBW.Caption := Format('Q: %.2f',
                                [q]);
      lblEqBW.Caption := 'BW: (off)';
    end;

  lblEqTP.Caption := 'True-peak ceiling: -1.0 dBTP';
end;


procedure TfrmMain.UpdateFlangerLabels();
var
  wet: Single;
  delayMs: Integer;
  depthMs: Integer;
  rateHz: Single;
  fb: Single;

begin

  wet := trbFlWet.Position / 100.0;
  delayMs := trbFlDelay.Position;
  depthMs := trbFlDepth.Position;
  rateHz := trbFlRate.Position / 100.0;
  fb := trbFlFeedback.Position / 100.0;

  lblFlWet.Caption := Format('Wet: %.2f',
                             [wet]);

  lblFlDelay.Caption := Format('Delay: %d ms',
                               [delayMs]);

  lblFlDepth.Caption := Format('Depth: %d ms',
                               [depthMs]);

  lblFlRate.Caption := Format('Rate: %.2f Hz',
                              [rateHz]);

  lblFlFeedback.Caption := Format('Feedback: %.2f',
                                  [fb]);
end;


procedure TfrmMain.ApplyEqFromUI();
var
  gainDb: Single;
  freqHz: Single;
  q: Single;
  bw: Single;
  slot: TMfWasApiFxSlot;

begin

  if not Assigned(fxParametricEq) then
    Exit;

  gainDb := trbEqGain.Position / 10.0;
  freqHz := EqFreqSliderToHz(trbEqFreq.Position);
  q := trbEqQ.Position / 10.0;
  bw := trbEqBW.Position / 10.0;

  // Slot.Enabled is the single source of truth for bypass.
  if Assigned(waFxRack) then
    begin

      slot := waFxRack.FindSlotByEffect(fxParametricEq);
      if (slot = nil) then
        slot := waFxRack.FindFirstSlotByEffectClass(TMfParametricEqEffect);
        if (slot = nil) then
          slot := waFxRack.FindSlotByEffectName('FXParametricEq');
          if (slot <> nil) then
            slot.Enabled := cbEqEnabled.Checked;
    end;

  fxParametricEq.GainDb := gainDb;
  fxParametricEq.CenterFreqHz := freqHz;

  if cbEqUseBW.Checked then

    fxParametricEq.BandwidthOctaves := bw
  else
    begin

      fxParametricEq.BandwidthOctaves := 0.0;
      fxParametricEq.Q := q;
    end;

  // True-peak guard: hard requirement.
  fxParametricEq.TruePeakGuard := cbEqTruePeak.Checked;
  fxParametricEq.TruePeakCeilingDbTP := -1.0;
  fxParametricEq.TruePeakOversample := 4;

  UpdateEqLabels();
end;


procedure TfrmMain.ApplyFlangerFromUI();
var
  wet: Single;
  fb: Single;
  slot: TMfWasApiFxSlot;

begin

  if not Assigned(fxFlangerEcho) then
    Exit;

  wet := trbFlWet.Position / 100.0;
  fb := trbFlFeedback.Position / 100.0;

  if (fb > 0.98) then
    fb := 0.98;

  // Slot.Enabled is the single source of truth for bypass.
  if Assigned(waFxRack) then
    begin

      slot := waFxRack.FindSlotByEffect(fxFlangerEcho);
      if (slot = nil) then
        slot := waFxRack.FindFirstSlotByEffectClass(TMfFlangerEchoEffect);
        if (slot = nil) then
          slot := waFxRack.FindSlotByEffectName('FXFlangerEcho');
          if (slot <> nil) then
            slot.Enabled := cbFlEnabled.Checked;
    end;

  fxFlangerEcho.Wet := wet;
  fxFlangerEcho.BaseDelayMs := trbFlDelay.Position;
  fxFlangerEcho.DepthMs := trbFlDepth.Position;
  fxFlangerEcho.RateHz := trbFlRate.Position / 100.0;
  fxFlangerEcho.Feedback := fb;

  UpdateFlangerLabels();
end;


procedure TfrmMain.UpdateDynamicsLabels();
var
  th: Single;
  ra: Single;
  atMs: Single;
  relMs: Single;
  knee: Single;
  makeup: Single;
  limCeil: Single;
  limLook: Integer;
  limRel: Integer;

begin

  th := trbDynThresh.Position / 10.0;
  ra := trbDynRatio.Position / 10.0;
  atMs := trbDynAttack.Position / 10.0;
  relMs := trbDynRelease.Position;
  knee := trbDynKnee.Position / 10.0;
  makeup := trbDynMakeup.Position / 10.0;
  limCeil := trbDynLimCeil.Position / 10.0;
  limLook := trbDynLimLook.Position;
  limRel := trbDynLimRel.Position;

  lblDynThresh.Caption := Format('Threshold: %.1f dB',
                                 [th]);

  lblDynRatio.Caption := Format('Ratio: %.1f:1',
                                [ra]);

  lblDynAttack.Caption := Format('Attack: %.1f ms',
                                 [atMs]);

  lblDynRelease.Caption := Format('Release: %d ms',
                                  [Round(relMs)]);

  lblDynKnee.Caption := Format('Knee: %.1f dB',
                               [knee]);

  lblDynMakeup.Caption := Format('Makeup: %.1f dB',
                                 [makeup]);

  lblDynLimCeil.Caption := Format('Limiter ceiling: %.1f dB',
                                  [limCeil]);

  lblDynLimLook.Caption := Format('Lookahead: %d ms',
                                  [limLook]);

  lblDynLimRel.Caption := Format('Limiter release: %d ms',
                                 [limRel]);

  lblDynTP.Caption := Format('True-peak: %s, OS=%s, ceil=%.1f dBTP',
                            [BoolToStr(cbDynTruePeak.Checked),
                             cmbDynTPOS.Text,
                             -1.0]);
end;


procedure TfrmMain.ApplyDynamicsFromUI();
var
  os: Integer;
  slot: TMfWasApiFxSlot;

begin

  if not Assigned(fxCompressorLimiter) then
    Exit;

  // Slot.Enabled is the single source of truth for bypass.
  if Assigned(waFxRack) then
    begin

      slot := waFxRack.FindSlotByEffect(fxCompressorLimiter);
        if (slot = nil) then
          slot := waFxRack.FindFirstSlotByEffectClass(TMfCompressorLimiterEffect);
          if (slot = nil) then
            slot := waFxRack.FindSlotByEffectName('FXCompressorLimiter');
            if (slot <> nil) then
              slot.Enabled := cbDynEnabled.Checked;
    end;

  // Detector + auto makeup
  fxCompressorLimiter.RmsDetector := cbDynRms.Checked;
  fxCompressorLimiter.CompAutoMakeup := cbDynAutoMakeup.Checked;

  // Compressor
  fxCompressorLimiter.CompThresholdDb := trbDynThresh.Position / 10.0;
  fxCompressorLimiter.CompRatio := trbDynRatio.Position / 10.0;
  fxCompressorLimiter.CompAttackMs := trbDynAttack.Position / 10.0;
  fxCompressorLimiter.CompReleaseMs := trbDynRelease.Position;
  fxCompressorLimiter.CompKneeDb := trbDynKnee.Position / 10.0;
  fxCompressorLimiter.CompMakeupDb := trbDynMakeup.Position / 10.0;

  // Limiter
  fxCompressorLimiter.LimCeilingDb := trbDynLimCeil.Position / 10.0;
  fxCompressorLimiter.LimLookaheadMs := trbDynLimLook.Position;
  fxCompressorLimiter.LimReleaseMs := trbDynLimRel.Position;

  // True-peak (hard requirement)
  fxCompressorLimiter.TruePeakGuard := cbDynTruePeak.Checked;
  fxCompressorLimiter.TruePeakCeilingDbTP := -1.0;

  os := 4;

  if SameText(cmbDynTPOS.Text, '2x') then
    os := 2
  else if SameText(cmbDynTPOS.Text, '8x') then
    os := 8;

  fxCompressorLimiter.TruePeakOversample := os;

  UpdateDynamicsLabels();
end;


procedure TfrmMain.UpdateDynamicsMeters();
var
  cgr,
  lgr: Single;

begin

  if not Assigned(fxCompressorLimiter) then
    Exit;

  cgr := fxCompressorLimiter.CompressorGRdB;
  lgr := fxCompressorLimiter.LimiterGRdB;

  lblDynCompGR.Caption := Format('Comp GR: %.1f dB',
                                 [cgr]);

  lblDynLimGR.Caption := Format('Lim GR: %.1f dB',
                                [lgr]);
end;


// FX UI event handlers ======================================================

procedure TfrmMain.cbEqEnabledClick(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.cbEqUseBWClick(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.cbEqTruePeakClick(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.trbEqGainChange(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.trbEqFreqChange(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.trbEqQChange(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.trbEqBWChange(Sender: TObject);
begin

  ApplyEqFromUI();
end;


procedure TfrmMain.cbFlEnabledClick(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.trbFlWetChange(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.trbFlDelayChange(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.trbFlDepthChange(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.trbFlRateChange(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.trbFlFeedbackChange(Sender: TObject);
begin

  ApplyFlangerFromUI();
end;


procedure TfrmMain.cbDynEnabledClick(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.cbDynRmsClick(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.cbDynAutoMakeupClick(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.cbDynTruePeakClick(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.cmbDynTPOSChange(Sender: TObject);

begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynThreshChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynRatioChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynAttackChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynReleaseChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynKneeChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynMakeupChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynLimCeilChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynLimLookChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbDynLimRelChange(Sender: TObject);
begin

  ApplyDynamicsFromUI;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  // Initialize FX UI ranges

  // EQ
  trbEqGain.Min := -240;
  trbEqGain.Max := 240;
  trbEqGain.Frequency := 40;
  trbEqFreq.Min := 0;
  trbEqFreq.Max := 1000;
  trbEqFreq.Frequency := 100;

  trbEqQ.Min := 2;
  trbEqQ.Max := 120;
  trbEqQ.Frequency := 10;

  trbEqBW.Min := 1;
  trbEqBW.Max := 40;
  trbEqBW.Frequency := 5;

  // Flanger
  trbFlWet.Min := 0;
  trbFlWet.Max := 100;
  trbFlWet.Frequency := 10;

  trbFlDelay.Min := 0;
  trbFlDelay.Max := 2000;
  trbFlDelay.Frequency := 100;

  trbFlDepth.Min := 0;
  trbFlDepth.Max := 50;
  trbFlDepth.Frequency := 5;

  trbFlRate.Min := 0;
  trbFlRate.Max := 500; // 0..5.00 Hz (x100)
  trbFlRate.Frequency := 50;

  trbFlFeedback.Min := 0;
  trbFlFeedback.Max := 98; // 0..0.98 (x100)
  trbFlFeedback.Frequency := 10;

  // Dynamics
  trbDynThresh.Min := -600;
  trbDynThresh.Max := 0;
  trbDynThresh.Frequency := 60;

  trbDynRatio.Min := 10;
  trbDynRatio.Max := 500;
  trbDynRatio.Frequency := 50;

  trbDynAttack.Min := 1;
  trbDynAttack.Max := 5000; // 0.1..500.0 ms (x10)
  trbDynAttack.Frequency := 250;

  trbDynRelease.Min := 1;
  trbDynRelease.Max := 5000;
  trbDynRelease.Frequency := 250;

  trbDynKnee.Min := 0;
  trbDynKnee.Max := 240;
  trbDynKnee.Frequency := 40;

  trbDynMakeup.Min := 0;
  trbDynMakeup.Max := 240;
  trbDynMakeup.Frequency := 40;

  trbDynLimCeil.Min := -240;
  trbDynLimCeil.Max := 0;
  trbDynLimCeil.Frequency := 40;

  trbDynLimLook.Min := 0;
  trbDynLimLook.Max := 20;
  trbDynLimLook.Frequency := 5;

  trbDynLimRel.Min := 1;
  trbDynLimRel.Max := 5000;
  trbDynLimRel.Frequency := 250;

  cmbDynTPOS.Items.Clear;
  cmbDynTPOS.Items.Add('2x');
  cmbDynTPOS.Items.Add('4x');
  cmbDynTPOS.Items.Add('8x');
  cmbDynTPOS.ItemIndex := 1; // 4x

  // Default UI state
  cbEqEnabled.Checked := True;
  cbEqUseBW.Checked := True;
  cbEqTruePeak.Checked := True;
  cbFlEnabled.Checked := True;

  // Pull defaults from components if they are present.
  if Assigned(fxParametricEq) then
    begin

      trbEqGain.Position := EnsureRange(Round(fxParametricEq.GainDb * 10.0),
                                        trbEqGain.Min,
                                        trbEqGain.Max);

      trbEqFreq.Position := EqHzToFreqSlider(fxParametricEq.CenterFreqHz);

      trbEqQ.Position := EnsureRange(Round(fxParametricEq.Q * 10.0),
                                     trbEqQ.Min,
                                     trbEqQ.Max);

      trbEqBW.Position := EnsureRange(Round(fxParametricEq.BandwidthOctaves * 10.0),
                                      trbEqBW.Min,
                                      trbEqBW.Max);

      cbEqEnabled.Checked := fxParametricEq.Enabled;
      cbEqTruePeak.Checked := fxParametricEq.TruePeakGuard;
      cbEqUseBW.Checked := (fxParametricEq.BandwidthOctaves > 0.0001);
    end;

  if Assigned(fxFlangerEcho) then
    begin

      trbFlWet.Position := EnsureRange(Round(fxFlangerEcho.Wet * 100.0),
                                       trbFlWet.Min,
                                       trbFlWet.Max);

      trbFlDelay.Position := EnsureRange(Round(fxFlangerEcho.BaseDelayMs),
                                         trbFlDelay.Min,
                                         trbFlDelay.Max);

      trbFlDepth.Position := EnsureRange(Round(fxFlangerEcho.DepthMs),
                                         trbFlDepth.Min,
                                         trbFlDepth.Max);

      trbFlRate.Position := EnsureRange(Round(fxFlangerEcho.RateHz * 100.0),
                                        trbFlRate.Min,
                                        trbFlRate.Max);

      trbFlFeedback.Position := EnsureRange(Round(fxFlangerEcho.Feedback * 100.0),
                                            trbFlFeedback.Min,
                                            trbFlFeedback.Max);

      cbFlEnabled.Checked := fxFlangerEcho.Enabled;
    end;

  if Assigned(fxCompressorLimiter) then
    begin

      // Compressor defaults

      trbDynThresh.Position := EnsureRange(Round(fxCompressorLimiter.CompThresholdDb * 10.0),
                                           trbDynThresh.Min,
                                           trbDynThresh.Max);

      trbDynRatio.Position := EnsureRange(Round(fxCompressorLimiter.CompRatio * 10.0),
                                          trbDynRatio.Min,
                                          trbDynRatio.Max);

      trbDynAttack.Position := EnsureRange(Round(fxCompressorLimiter.CompAttackMs * 10.0),
                                           trbDynAttack.Min,
                                           trbDynAttack.Max);

      trbDynRelease.Position := EnsureRange(Round(fxCompressorLimiter.CompReleaseMs),
                                                  trbDynRelease.Min,
                                                  trbDynRelease.Max);

      trbDynKnee.Position := EnsureRange(Round(fxCompressorLimiter.CompKneeDb * 10.0),
                                         trbDynKnee.Min,
                                         trbDynKnee.Max);

      trbDynMakeup.Position := EnsureRange(Round(fxCompressorLimiter.CompMakeupDb * 10.0),
                                           trbDynMakeup.Min,
                                           trbDynMakeup.Max);

      // Limiter defaults

      trbDynLimCeil.Position := EnsureRange(Round(fxCompressorLimiter.LimCeilingDb * 10.0),
                                            trbDynLimCeil.Min,
                                            trbDynLimCeil.Max);

      trbDynLimLook.Position := EnsureRange(Round(fxCompressorLimiter.LimLookaheadMs),
                                            trbDynLimLook.Min,
                                            trbDynLimLook.Max);

      trbDynLimRel.Position := EnsureRange(Round(fxCompressorLimiter.LimReleaseMs),
                                           trbDynLimRel.Min,
                                           trbDynLimRel.Max);

      cbDynEnabled.Checked := fxCompressorLimiter.Enabled;
      cbDynRms.Checked := fxCompressorLimiter.RmsDetector;
      cbDynAutoMakeup.Checked := fxCompressorLimiter.CompAutoMakeup;
      cbDynTruePeak.Checked := fxCompressorLimiter.TruePeakGuard;
    end;

    if Assigned(waFxRack) then
      begin

      // -----------------------------------------------------------------------
      // EngineComponent + EffectsRack wiring.
      // -----------------------------------------------------------------------

      // Create rack (owns the slot list)
      //FEffectsRack := TMfWasApiEffectsRack.Create(Self);

      // Assign slots (these are your non-visual FX components already on the form)
      // Order matters: EQ -> Dynamics -> Flanger

      // NOTE:
      //   Assigning like these, will throw a rangecheck error, leave it, because this is set in the Rack component
      //   waFxRack.Slots[0].Effect := fxParametricEq;
      //   waFxRack.Slots[1].Effect := fxCompressorLimiter;
      //   waFxRack.Slots[2].Effect := fxFlangerEcho;

      // Create the component engine wrapper.
      if not Assigned(MfWasApiPlayerEngine) then
        MfWasApiPlayerEngine := MfWasApiPlayerEngine.Create(Self);

      // Wire rack into engine wrapper (this sets Engine.OnProcessPcm := Rack.ProcessPcm)
      MfWasApiPlayerEngine.EffectsRack := waFxRack;

      // Wire events to YOUR EXISTING handlers (these already exist in MainFrm.pas)
      MfWasApiPlayerEngine.OnReady := OnAudioReady;
      MfWasApiPlayerEngine.OnProcessed := OnAudioDataProcessed;
      MfWasApiPlayerEngine.OnEnded := OnAudioEnded;
      MfWasApiPlayerEngine.OnError := OnEngineError;
      MfWasApiPlayerEngine.OnStateChanged := OnEngineState;
    end;

  ApplyEqFromUI;
  ApplyFlangerFromUI;
  ApplyDynamicsFromUI;
end;


procedure TfrmMain.trbVolumeLChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeR.Position := trbVolumeL.Position;

  SetVolumeChannels();

  vol := (MapRange((trbVolumeL.Position),
                    trbVolumeL.Max,
                    trbVolumeL.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblLeftVolume.Caption := Format('%d',
                                  [Trunc(vol)]) + '%';
end;


procedure TfrmMain.trbVolumeRChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeL.Position := trbVolumeR.Position;

  SetVolumeChannels();

  vol := (MapRange((trbVolumeR.Position),
                    trbVolumeR.Max,
                    trbVolumeR.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblRightVolume.Caption := Format('%d',
                                   [Trunc(vol)]) + '%';
end;


procedure TfrmMain.Open1Click(Sender: TObject);
var
  hr: HResult;

begin

  // Select an audiofile.
  fAudioFileUrl := GetAudioFile();
  if (fAudioFileUrl = 'No audiofile selected.') then
    Exit;

  fFileName := ExtractFileName(fAudioFileUrl);

  // Get the length of the audiofile.
  hr := GetFileDuration(StrToPWideChar(fAudioFileUrl),
                        llAudioDuration);

  if FAILED(hr) then
    begin

      ShowMessage('Could not retrieve the duration of the audio file.');
      llAudioDuration := 0;
    end;

  if SUCCEEDED(hr) then
    begin

      lblDuration.Caption := Format('Duration: %s',
                                    [HnsTimeToStr(llAudioDuration, False)]);

      // Set progressbar max
      pbProgress.Max := llAudioDuration div 1000000;
      // Engine is provided by component (created in FormCreate)
    end;

  if SUCCEEDED(hr) then
    begin

      stxtStatus.Caption := Format('Selected file: %s.',
                                   [fFileName]);
    end;

  // Initialize the engine.
  if SUCCEEDED(hr) then
    hr := MfWasApiPlayerEngine.OpenFile(fAudioFileUrl,
                                        llAudioDuration);
  if FAILED(hr) then
    stxtStatus.Caption := Format('Selected file: %s open file failed with error: %d.',
                                [fFileName, hr]);
end;



procedure TfrmMain.pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  secPos: Integer;
  hnsPos: Int64;

begin

  if (pbProgress.Max <= 0) or (llAudioDuration <= 0) then
    Exit;

  // Show only when playing/pause

  if MfWasApiPlayerEngine.DeviceState in [dsPlay, dsPause] then
    begin

      secPos := Trunc((X / pbProgress.Width) * pbProgress.Max);

      if (secPos < 0) then
        secPos := 0
      else
        if (secPos > pbProgress.Max) then
          secPos := pbProgress.Max;

      pbProgress.ShowHint := True;
      pbProgress.Hint := Format('Position: %d s', [secPos]);
      lblBarPositionInSamples.Caption := Format('Position: %d s', [secPos]);

      hnsPos := Int64(secPos) * 10000000;

      lblBarPositionInSTime.Caption := Format('Position: %s', [HnsTimeToStr(hnsPos, False)]);
    end;
end;


procedure TfrmMain.pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  hr: HResult;
  secPos: Int64;
  posHns: Int64;

begin

  if (MfWasApiPlayerEngine = nil) then
    Exit;

  if (pbProgress.Width <= 0) then
    Exit;

  if (pbProgress.Max <= 0) then
    Exit;

  if (X <= 0) then
    secPos := 0
  else
    if (X >= pbProgress.Width) then
      secPos := pbProgress.Max
  else
    secPos := Trunc((X / pbProgress.Width) * pbProgress.Max); // seconds

  // Seconds -> 100ns
  posHns := secPos * 10000000;

  // clamp to duration (optional safety)
  if (llAudioDuration > 0) and (posHns > llAudioDuration) then
    posHns := llAudioDuration;

  hr := MfWasApiPlayerEngine.SeekTo(posHns);

  if SUCCEEDED(hr) then
    pbProgress.Position := Integer(secPos)
  else
    stxtStatus.Caption := Format('SeekTo failed. (hr=%d)',
                                 [hr]);
end;


// Event handlers ==============================================================

procedure TfrmMain.OnAudioDataProcessed(Sender: TObject;
                                        const Position100ns: Int64;
                                        const RawPosition: UInt64);
var
  iProgress: LONGLONG;
  iSamples: LONGLONG;
  tstr: string;
  secPos: Integer;

begin

  iProgress := Position100ns;
  iSamples := RawPosition;

  if (pbProgress.Max <= 0) then
    Exit;

  secPos := Integer(Position100ns div 10000000);

  if (secPos < 0) then
    secPos := 0;

  if (secPos > pbProgress.Max) then
    secPos := pbProgress.Max;

  pbProgress.Position := secPos;

  tstr := HnsTimeToStr(iProgress,
                       False);

  lblProcessed.Caption := Format('Samples: %d',
                                 [iSamples]);
  lblPlayed.Caption := Format('Played: %s',
                              [tstr]);

  UpdateDynamicsMeters();
end;


procedure TfrmMain.OnAudioReady(Sender: TObject);
var
  durSec: Int64;

begin

  if (MfWasApiPlayerEngine = nil) then
    Exit;

  if (llAudioDuration > 0) then
    durSec := llAudioDuration div 10000000
  else
    durSec := 0;

  if (durSec > High(Integer)) then
    pbProgress.Max := High(Integer)
  else
    pbProgress.Max := Integer(durSec);

  pbProgress.Position := 0;
  pbProgress.Enabled := (pbProgress.Max > 0);
  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
  SetVolumeChannels();
end;


procedure TfrmMain.OnAudioEnded(Sender: TObject);
begin

  stxtStatus.Caption := Format('Stopped: %s',
                              [fFileName]);

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
end;


procedure TfrmMain.OnEngineError(Sender: TObject; const Msg: string; const Hr: HRESULT);
begin

  stxtStatus.Caption := Format('%s (error 0x%.8x)',
                              [Msg, Cardinal(Hr)]);
end;


procedure TfrmMain.OnEngineState(Sender: TObject;
                            const NewState: TDeviceState);
begin

  case NewState of

    dsReady,
    dsStop:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := False;
        pbProgress.Position := 0;
        lblPlayed.Caption := 'Played: 00:00:00';
        lblProcessed.Caption := 'Samples: 0';
      end;

    dsPlay:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Pause';
        butStop.Enabled := True;
        stxtStatus.Caption := Format('Playing: %s',
                                     [fFileName]);
      end;

    dsPause:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := True;
        stxtStatus.Caption := Format('Pauzed: %s',
                                     [fFileName]);
      end;

    dsUninitialized,
    dsInitialized:
      begin

        butPlayPause.Enabled := False;
        butPlayPause.Caption := 'Play';

        butStop.Enabled := False;

      end;



    dsError:

      begin

        butPlayPause.Enabled := False;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := False;
        stxtStatus.Caption := Format('Yoo! we have an error: %d', [GetLastError()]);
      end;
  end;
end;


// initialization and finalization =============================================


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;


finalization

  MFShutdown();


end.