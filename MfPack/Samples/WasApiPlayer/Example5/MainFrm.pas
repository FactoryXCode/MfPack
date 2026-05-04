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
// Intiator(s): Tony Kalf (maXcomX), Carmen.
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
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
  System.IniFiles,
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

  // MfPack components.
  WASAPIEngine,
  MfPeakMeter,
  // FX
  MfWasApiFxComponentBase,
  MfAudioEffectMFTBase,
  MfFlangerEchoComponent,
  MfParametricEqComponent,
  MfCompressorLimiterComponent,
  MfWasApiEffectsRack,
  MfWasApiPlayerEngineComponent,
  MfChorusComponent,
  //MfPitchTempoComponent,
  //MfPitchTempoMFT,
  // Other
  MfAudioEndPoint,
  MfAudioMixVisualizer;

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

    gbChorus: TGroupBox;
    cbChEnabled: TCheckBox;
    rbChRateFree: TRadioButton;
    rbChRateSync: TRadioButton;
    lblChMix: TLabel;
    lblChBaseDelay: TLabel;
    lblChDepth: TLabel;
    lblChFeedback: TLabel;
    lblChRate: TLabel;
    lblChTempo: TLabel;
    lblChWidth: TLabel;
    lblChSmooth: TLabel;
    trbChMix: TTrackBar;
    trbChBaseDelay: TTrackBar;
    trbChDepth: TTrackBar;
    trbChFeedback: TTrackBar;
    trbChRate: TTrackBar;
    edtChBpm: TEdit;
    cmbChNoteDiv: TComboBox;
    trbChWidth: TTrackBar;
    trbChSmooth: TTrackBar;

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

    GroupBox2: TGroupBox;
    trbMainVolumeL: TTrackBar;
    trbMainVolumeR: TTrackBar;
    CheckBox1: TCheckBox;
    cbxMute: TCheckBox;
    lblMainLeftVolume: TLabel;
    lblMainRightVolume: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;

    visAudioMix: TMfAudioMixVisualizer;
    fxChorus: TMfChorusEffect;
    fxCompressorLimiter: TMfCompressorLimiterEffect;
    fxParametricEq: TMfParametricEqEffect;
    fxFlangerEcho: TMfFlangerEchoEffect;
    waFxRack: TMfWasApiEffectsRack;
    MfWasApiPlayerEngine: TMfWasApiPlayerEngine;
    rbSpectrum: TRadioButton;
    rbVu: TRadioButton;
    aepMaster: TMfAudioEndPoint;

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
    // Chorus UI events
    procedure cbChEnabledClick(Sender: TObject);
    procedure rbChRateModeClick(Sender: TObject);
    procedure trbChMixChange(Sender: TObject);
    procedure trbChBaseDelayChange(Sender: TObject);
    procedure trbChDepthChange(Sender: TObject);
    procedure trbChFeedbackChange(Sender: TObject);
    procedure trbChRateChange(Sender: TObject);
    procedure edtChBpmChange(Sender: TObject);
    procedure cmbChNoteDivChange(Sender: TObject);
    procedure trbChWidthChange(Sender: TObject);
    procedure trbChSmoothChange(Sender: TObject);

    // Pitch/Tempo UI events
    //procedure cbPtEnabledClick(Sender: TObject);
    //procedure cbPtFormantsClick(Sender: TObject);
   // procedure trbPtPitchChange(Sender: TObject);
   // procedure trbPtTempoChange(Sender: TObject);
   // procedure trbPtOverlapChange(Sender: TObject);
   // procedure cmbPtWindowChange(Sender: TObject);
   // procedure cmbPtModeChange(Sender: TObject);

    //procedure ApplyPitchTempoFromUI();
    //procedure UpdatePitchTempoLabels();

    procedure ApplyChorusFromUI();
    procedure UpdateChorusLabels();
    procedure UpdateChorusRateUi();


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
    procedure cbxMuteClick(Sender: TObject);
    procedure trbMainVolumeLChange(Sender: TObject);
    procedure trbMainVolumeRChange(Sender: TObject);
    procedure MfWasApiPlayerEngineEnded(Sender: TObject);
    procedure MfWasApiPlayerEngineError(Sender: TObject; const Msg: string;
      const Hr: HRESULT);
    procedure MfWasApiPlayerEngineProcessed(Sender: TObject;
      const Position100ns: Int64; const RawPosition: UInt64);
    procedure MfWasApiPlayerEngineReady(Sender: TObject);
    procedure MfWasApiPlayerEngineStateChanged(Sender: TObject;
      const NewState: TDeviceState);
    procedure rbVuClick(Sender: TObject);
    procedure rbSpectrumClick(Sender: TObject);

  private
    { Private declarations }

    FAudioFileUrl: TFileName;
    FFileName: string; // Filename without path.
    llAudioDuration: LONGLONG;

    // If we don't want a WasApiPlayer component for testing TWasApiEngine.
    //MfWasApiPlayerEngine: TWasApiEngine;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();
    /// <summary>Set Left and/or Right Main volume.</summary>
    procedure SetMainVolumeChannels();

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

    // INI persistence (FX rack)
    function GetIniFileName(): string;
    procedure LoadFxFromIni();
    procedure SaveFxToIni();

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

      // Set progressbar max.
      pbProgress.Max := llAudioDuration div 1000000;
      // Engine is provided by component (created in FormCreate).
    end;

  if SUCCEEDED(hr) then
    stxtStatus.Caption := Format('Selected file: %s.',
                                 [fFileName]);

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
        SetMainVolumeChannels();
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
                                [fFileName, hr]);
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;

  // 1) Stop callbacks into UI/FX immediately (engine thread safe)
  MfWasApiPlayerEngine.OnProcessed := nil;
  MfWasApiPlayerEngine.OnError := nil;
  MfWasApiPlayerEngine.OnEnded := nil;
  MfWasApiPlayerEngine.OnReady := nil;
  MfWasApiPlayerEngine.OnStateChanged := nil;

  // 2) Stop audio and WAIT until the render thread has exited
  MfWasApiPlayerEngine.Stop();  // True = wait/join. If you don't have this, you must add it.
  MfWasApiPlayerEngine.WaitForStop(1000);

  // // The bare engine (not the component) for testing..
  // FreeAndNil(MfWasApiPlayerEngine);

  // 3) Now it's safe to free visual meters / FX rack / UI objects
  FreeAndNil(pmLeft);
  FreeAndNil(pmRight);

  // 4) Save settings.
  SaveFxToIni();

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


procedure TfrmMain.SetMainVolumeChannels();
begin

  if not Assigned(MfWasApiPlayerEngine) or (aepMaster = nil) then
    Exit;

  // Stereo
  // The first stereo channel (0) is always the LEFT one! SetVolumes
  if (MfWasApiPlayerEngine.SoundChannels = 2) then
    begin
      // NOTE: The maximum of channels is 8!
      aepMaster.ChannelVolume[0] := Abs(trbMainVolumeL.Position) * 0.01;
      aepMaster.ChannelVolume[1] := Abs(trbMainVolumeR.Position) * 0.01;
    end;
end;


procedure TfrmMain.trbMainVolumeLChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbMainVolumeR.Position := trbMainVolumeL.Position;

  SetMainVolumeChannels();

  vol := (MapRange((trbMainVolumeL.Position),
                    trbMainVolumeL.Max,
                    trbMainVolumeL.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblMainLeftVolume.Caption := Format('%d',
                                  [Trunc(vol)]) + '%';
end;


procedure TfrmMain.trbMainVolumeRChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbMainVolumeL.Position := trbMainVolumeR.Position;

  SetMainVolumeChannels();

  vol := (MapRange((trbMainVolumeR.Position),
                    trbMainVolumeR.Max,
                    trbMainVolumeR.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblMainRightVolume.Caption := Format('%d',
                                       [Trunc(vol)]) + '%';
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
  loHz,
  hiHz: Double;
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


{procedure TfrmMain.UpdatePitchTempoLabels();
var
  pitch: Single;
  tempo: Single;
  overlap: Single;
  win: Integer;
  modeName: string;
begin

  pitch := trbPtPitch.Position / 10.0;
  tempo := trbPtTempo.Position;
  overlap := trbPtOverlap.Position / 100.0;
  win := StrToIntDef(Trim(cmbPtWindow.Text),
                     1024);

  if (cmbPtMode.ItemIndex = 0) then
    modeName := 'Clean'
  else
    modeName := 'DJ';

  lblPtPitch.Caption := Format('Pitch: %.1f st',
                               [pitch]);
  lblPtTempo.Caption := Format('Tempo: %.0f %%',
                               [tempo]);
  lblPtOverlap.Caption := Format('Overlap: %.2f',
                                 [overlap]);
  lblPtWindow.Caption := Format('Window: %d',
                                [win]);
  lblPtMode.Caption := Format('Mode: %s',
                              [modeName]);
end;}


{procedure TfrmMain.ApplyPitchTempoFromUI();
var
  pitch: Single;
  tempo: Single;
  overlap: Single;
  win: Integer;
  mode: TPitchTempoMode;
  slot: TMfWasApiFxSlot;

begin

  if not Assigned(fxPitchTempo) then
    Exit;

  pitch := trbPtPitch.Position / 10.0;
  tempo := trbPtTempo.Position;
  overlap := trbPtOverlap.Position / 100.0;
  win := StrToIntDef(Trim(cmbPtWindow.Text),
                     fxPitchTempo.WindowSize);

  mode := ptmClean;
  if (cmbPtMode.ItemIndex >= 0) then
    mode := TPitchTempoMode(EnsureRange(cmbPtMode.ItemIndex,
                                        0,
                                        Ord(High(TPitchTempoMode))));

  // Slot.Enabled is the single source of truth for bypass.
  if Assigned(waFxRack) then
    begin

      slot := waFxRack.FindFirstSlotByEffectClass(TMfPitchTempoEffect);
      if (slot = nil) then
        slot := waFxRack.FindSlotByEffectName('FXPitchTempo');

      if (slot <> nil) then
        slot.Enabled := cbPtEnabled.Checked;
    end;

  fxPitchTempo.Enabled := cbPtEnabled.Checked;

  fxPitchTempo.PitchSemitones := pitch;
  fxPitchTempo.TempoPercent := tempo;
  fxPitchTempo.Overlap := overlap;
  fxPitchTempo.WindowSize := win;
  fxPitchTempo.PreserveFormants := cbPtFormants.Checked;
  fxPitchTempo.Mode := mode;

  UpdatePitchTempoLabels();
end;}


procedure TfrmMain.UpdateChorusRateUi();
begin

  if rbChRateFree.Checked then
    begin

      trbChRate.Enabled := True;
      edtChBpm.Enabled := False;
      cmbChNoteDiv.Enabled := False;
    end
  else
    begin

      trbChRate.Enabled := False;
      edtChBpm.Enabled := True;
      cmbChNoteDiv.Enabled := True;
    end;
end;


procedure TfrmMain.UpdateChorusLabels();
var
  mix: Single;
  baseMs: Integer;
  depthMs: Single;
  fb: Single;
  rateHz: Single;
  bpm: Single;
  divText: string;
  width: Integer;
  smooth: Integer;

begin

  mix := trbChMix.Position / 100.0;
  baseMs := trbChBaseDelay.Position;
  depthMs := trbChDepth.Position / 10.0;
  fb := trbChFeedback.Position / 100.0;
  rateHz := trbChRate.Position / 100.0;
  bpm := StrToFloatDef(edtChBpm.Text,
                       120.0);
  width := trbChWidth.Position;
  smooth := trbChSmooth.Position;

  lblChMix.Caption := Format('Mix: %.2f',
                             [mix]);
  lblChBaseDelay.Caption := Format('Base delay: %d ms',
                                   [baseMs]);
  lblChDepth.Caption := Format('Depth: %.1f ms',
                               [depthMs]);
  lblChFeedback.Caption := Format('Feedback: %.2f',
                                  [fb]);
  lblChRate.Caption := Format('Rate: %.2f Hz',
                              [rateHz]);
  lblChWidth.Caption := Format('Width: %d %%',
                               [width]);
  lblChSmooth.Caption := Format('Smoothing: %d ms',
                                [smooth]);

  if cmbChNoteDiv.ItemIndex >= 0 then
    divText := cmbChNoteDiv.Items[cmbChNoteDiv.ItemIndex]
  else
    divText := '1/8';

  lblChTempo.Caption := Format('Tempo: %.0f BPM / %s', [bpm, divText]);
end;


procedure TfrmMain.ApplyChorusFromUI();
var
  slot: TMfWasApiFxSlot;
  s: TChorusSettings;

begin

  if not Assigned(fxChorus) then
    Exit;

  // Slot.Enabled is the single source of truth for bypass.
  if Assigned(waFxRack) then
    begin
      slot := waFxRack.FindSlotByEffect(fxChorus);
      if (slot = nil) then
        slot := waFxRack.FindFirstSlotByEffectClass(TMfChorusEffect);
        if (slot = nil) then
          slot := waFxRack.FindSlotByEffectName('FXChorus');
          if (slot <> nil) then
           slot.Enabled := cbChEnabled.Checked;
    end;

  s := fxChorus.Settings;
  s.Enabled := cbChEnabled.Checked;

  s.Mix := trbChMix.Position / 100.0;
  s.Feedback := trbChFeedback.Position / 100.0;
  if (s.Feedback > 0.95) then
    s.Feedback := 0.95;

  s.BaseDelayMs := trbChBaseDelay.Position;
  s.DepthMs := trbChDepth.Position / 10.0;

  s.WidthPct := trbChWidth.Position;
  s.SmoothMs := trbChSmooth.Position;

  if rbChRateSync.Checked then
    begin
      s.RateMode := crmTempoSync;
      s.TempoBpm := StrToFloatDef(edtChBpm.Text,
                                  120.0);
      s.NoteDiv := TMfChorusNoteDiv(EnsureRange(cmbChNoteDiv.ItemIndex,
                                    0,
                                    Ord(High(TMfChorusNoteDiv))));
    end
  else
    begin

      s.RateMode := crmFreeHz;
      s.RateHz := trbChRate.Position / 100.0;
    end;

  fxChorus.Settings := s;

  UpdateChorusRateUi();
  UpdateChorusLabels();
end;


procedure TfrmMain.cbChEnabledClick(Sender: TObject);
begin

  ApplyChorusFromUI();
end;



{
procedure TfrmMain.cbPtEnabledClick(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.cbPtFormantsClick(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.trbPtPitchChange(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.trbPtTempoChange(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.trbPtOverlapChange(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.cmbPtWindowChange(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;


procedure TfrmMain.cmbPtModeChange(Sender: TObject);
begin

  ApplyPitchTempoFromUI();
end;
}

procedure TfrmMain.rbChRateModeClick(Sender: TObject);
begin

  UpdateChorusRateUi();
  ApplyChorusFromUI();
end;


procedure TfrmMain.rbSpectrumClick(Sender: TObject);
begin

  if rbSpectrum.Checked then
    visAudioMix.View := vmSpectrum;
end;


procedure TfrmMain.rbVuClick(Sender: TObject);
begin

  if rbVu.Checked then
    visAudioMix.View := vmMeters;
end;


procedure TfrmMain.trbChMixChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChBaseDelayChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChDepthChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChFeedbackChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChRateChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.edtChBpmChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.cmbChNoteDivChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChWidthChange(Sender: TObject);
begin

  ApplyChorusFromUI();
end;


procedure TfrmMain.trbChSmoothChange(Sender: TObject);
begin

  ApplyChorusFromUI();
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

  if SameText(cmbDynTPOS.Text,
             '2x') then
    os := 2
  else if SameText(cmbDynTPOS.Text,
                   '8x') then
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


procedure TfrmMain.cbxMuteClick(Sender: TObject);
begin

  aepMaster.Mute := cbxMute.Checked;
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

  // Test with bare engine.
  // MfWasApiPlayerEngine := TWasApiEngine.Create();

  //MfWasApiPlayerEngine.OnReady := OnAudioReady;
  //MfWasApiPlayerEngine.OnProcessed := OnAudioDataProcessed;
  //MfWasApiPlayerEngine.OnEnded := OnAudioEnded;
  //MfWasApiPlayerEngine.OnError := OnEngineError;
  //MfWasApiPlayerEngine.OnStateChanged := OnEngineState;
  // end test.

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


  // Chorus
  trbChMix.Min := 0;
  trbChMix.Max := 100;
  trbChMix.Frequency := 10;

  trbChFeedback.Min := 0;
  trbChFeedback.Max := 95;
  trbChFeedback.Frequency := 10;

  trbChBaseDelay.Min := 1;
  trbChBaseDelay.Max := 60;
  trbChBaseDelay.Frequency := 5;

  trbChDepth.Min := 0;
  trbChDepth.Max := 250; // 0..25.0 ms (x10)
  trbChDepth.Frequency := 25;

  trbChRate.Min := 1;
  trbChRate.Max := 2000; // 0.01..20.00 Hz (x100)
  trbChRate.Frequency := 200;

  trbChWidth.Min := 0;
  trbChWidth.Max := 100;
  trbChWidth.Frequency := 10;

  trbChSmooth.Min := 0;
  trbChSmooth.Max := 200;
  trbChSmooth.Frequency := 20;

  cmbChNoteDiv.Items.Clear;
  cmbChNoteDiv.Items.Add('1/1');
  cmbChNoteDiv.Items.Add('1/2');
  cmbChNoteDiv.Items.Add('1/4');
  cmbChNoteDiv.Items.Add('1/8');
  cmbChNoteDiv.Items.Add('1/16');

  // ------------------------------------------------------------------------
  // Pitch/Tempo UI ranges (matches MfPitchTempoMFT constraints)
  // ------------------------------------------------------------------------
  {
  trbPtPitch.Min := -240;    // -24.0 st
  trbPtPitch.Max := 240;     // +24.0 st
  trbPtPitch.Frequency := 20;

  trbPtTempo.Min := 50;      // 50%
  trbPtTempo.Max := 200;     // 200%
  trbPtTempo.Frequency := 10;

  trbPtOverlap.Min := 25;    // 0.25
  trbPtOverlap.Max := 75;    // 0.75
  trbPtOverlap.Frequency := 5;
 } {
  cmbPtWindow.Items.Clear;
  cmbPtWindow.Items.Add('512');
  cmbPtWindow.Items.Add('1024');
  cmbPtWindow.Items.Add('2048');
  cmbPtWindow.Items.Add('4096');
  cmbPtWindow.ItemIndex := 1; // 1024 default

  cmbPtMode.Items.Clear;
  cmbPtMode.Items.Add('Clean');
  cmbPtMode.Items.Add('DJ');
  cmbPtMode.ItemIndex := 0;

  cmbChNoteDiv.Items.Add('1/8T');
  cmbChNoteDiv.Items.Add('1/16T');
  cmbChNoteDiv.ItemIndex := 3; // 1/8
  }
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

  if Assigned(fxChorus) then
    begin

      trbChMix.Position := EnsureRange(Round(fxChorus.Mix * 100.0),
                                      trbChMix.Min,
                                      trbChMix.Max);

      trbChFeedback.Position := EnsureRange(Round(fxChorus.Feedback * 100.0),
                                           trbChFeedback.Min,
                                           trbChFeedback.Max);

      trbChBaseDelay.Position := EnsureRange(Round(fxChorus.BaseDelayMs),
                                             trbChBaseDelay.Min,
                                             trbChBaseDelay.Max);

      trbChDepth.Position := EnsureRange(Round(fxChorus.DepthMs * 10.0),
                                         trbChDepth.Min,
                                         trbChDepth.Max);

      trbChRate.Position := EnsureRange(Round(fxChorus.RateHz * 100.0),
                                        trbChRate.Min,
                                        trbChRate.Max);

      trbChWidth.Position := EnsureRange(Round(fxChorus.WidthPct),
                                         trbChWidth.Min,
                                         trbChWidth.Max);

      trbChSmooth.Position := EnsureRange(Round(fxChorus.SmoothMs),
                                          trbChSmooth.Min,
                                          trbChSmooth.Max);

      cbChEnabled.Checked := fxChorus.Enabled;

      rbChRateFree.Checked := (fxChorus.RateMode = crmFreeHz);
      rbChRateSync.Checked := (fxChorus.RateMode = crmTempoSync);

      edtChBpm.Text := Format('%.0f', [fxChorus.TempoBpm]);
      cmbChNoteDiv.ItemIndex := Ord(fxChorus.NoteDiv);

      UpdateChorusRateUi();
      UpdateChorusLabels();
    end;

  {if Assigned(fxPitchTempo) then
    begin

      trbPtPitch.Position := EnsureRange(Round(fxPitchTempo.PitchSemitones * 10.0),
                                         trbPtPitch.Min,
                                         trbPtPitch.Max);

      trbPtTempo.Position := EnsureRange(Round(fxPitchTempo.TempoPercent),
                                         trbPtTempo.Min,
                                         trbPtTempo.Max);

      trbPtOverlap.Position := EnsureRange(Round(fxPitchTempo.Overlap * 100.0),
                                           trbPtOverlap.Min,
                                           trbPtOverlap.Max);

      cbPtEnabled.Checked := fxPitchTempo.Enabled;
      cbPtFormants.Checked := fxPitchTempo.PreserveFormants;

      cmbPtMode.ItemIndex := EnsureRange(Ord(fxPitchTempo.Mode),
                                         0,
                                         cmbPtMode.Items.Count - 1);

      case fxPitchTempo.WindowSize of
        512:  cmbPtWindow.ItemIndex := 0;
        1024: cmbPtWindow.ItemIndex := 1;
        2048: cmbPtWindow.ItemIndex := 2;
        4096: cmbPtWindow.ItemIndex := 3;
      else
        cmbPtWindow.ItemIndex := 1;
      end;
    end;}



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

  ApplyEqFromUI();
  ApplyFlangerFromUI();
  ApplyDynamicsFromUI();
  ApplyChorusFromUI();
  //ApplyPitchTempoFromUI();

  UpdateChorusRateUi();
  UpdateEqLabels();
  UpdateFlangerLabels();
  UpdateDynamicsLabels();
  UpdateChorusLabels();
  //UpdatePitchTempoLabels();

  // Override with persisted UI settings (if ini exists)
  LoadFxFromIni();
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

  aepMaster.GetDefaultDevice();
  SetVolumeChannels();
  SetMainVolumeChannels();
end;


procedure TfrmMain.OnAudioEnded(Sender: TObject);
begin

  visAudioMix.Reset();

  stxtStatus.Caption := Format('Stopped: %s',
                               [fFileName]);

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
end;


procedure TfrmMain.OnEngineError(Sender: TObject;
                                 const Msg: string;
                                 const Hr: HRESULT);
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
        stxtStatus.Caption := Format('OOPS! We have an error: %d',
                                     [GetLastError()]);
      end;
  end;
end;


function TfrmMain.GetIniFileName(): string;
begin

  // Same folder as exe by default (classic Delphi behavior).
  Result := ChangeFileExt(ParamStr(0),
                          '.ini');
end;


procedure TfrmMain.LoadFxFromIni();
var
  Ini: TIniFile;
  fn: string;

begin

  fn := GetIniFileName();
  if not FileExists(fn) then
    Exit;

  Ini := TIniFile.Create(fn);

  try

    // EQ
    cbEqEnabled.Checked := Ini.ReadBool('EQ',
                                        'Enabled',
                                        cbEqEnabled.Checked);

    cbEqUseBW.Checked := Ini.ReadBool('EQ',
                                      'UseBW',
                                      cbEqUseBW.Checked);

    cbEqTruePeak.Checked := Ini.ReadBool('EQ',
                                         'TruePeak',
                                         cbEqTruePeak.Checked);

    trbEqGain.Position := EnsureRange(Ini.ReadInteger('EQ',
                                                      'Gain_x10',
                                                      trbEqGain.Position),
                                                      trbEqGain.Min,
                                                      trbEqGain.Max);

    trbEqFreq.Position := EnsureRange(Ini.ReadInteger('EQ',
                                                      'FreqSlider',
                                                      trbEqFreq.Position),
                                                      trbEqFreq.Min,
                                                      trbEqFreq.Max);
    trbEqQ.Position := EnsureRange(Ini.ReadInteger('EQ',
                                                   'Q_x10',
                                                   trbEqQ.Position),
                                                   trbEqQ.Min,
                                                   trbEqQ.Max);

    trbEqBW.Position := EnsureRange(Ini.ReadInteger('EQ',
                                                    'BW_x10',
                                                    trbEqBW.Position),
                                                    trbEqBW.Min,
                                                    trbEqBW.Max);

    // Dynamics
    cbDynEnabled.Checked := Ini.ReadBool('Dynamics',
                                         'Enabled',
                                         cbDynEnabled.Checked);

    cbDynRms.Checked := Ini.ReadBool('Dynamics',
                                     'UseRms',
                                     cbDynRms.Checked);

    cbDynAutoMakeup.Checked := Ini.ReadBool('Dynamics',
                                            'AutoMakeup',
                                            cbDynAutoMakeup.Checked);

    cbDynTruePeak.Checked := Ini.ReadBool('Dynamics',
                                          'TruePeak',
                                          cbDynTruePeak.Checked);

    trbDynThresh.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                         'Thresh_x10',
                                                         trbDynThresh.Position),
                                                         trbDynThresh.Min,
                                                         trbDynThresh.Max);

    trbDynRatio.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                        'Ratio_x10',
                                                        trbDynRatio.Position),
                                                        trbDynRatio.Min,
                                                        trbDynRatio.Max);

    trbDynAttack.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                         'Attack_x10',
                                                         trbDynAttack.Position),
                                                         trbDynAttack.Min,
                                                         trbDynAttack.Max);

    trbDynRelease.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                          'Release_x10',
                                                          trbDynRelease.Position),
                                                          trbDynRelease.Min,
                                                          trbDynRelease.Max);

    trbDynKnee.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                       'Knee_x10',
                                                       trbDynKnee.Position),
                                                       trbDynKnee.Min,
                                                       trbDynKnee.Max);

    trbDynMakeup.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                         'Makeup_x10',
                                                         trbDynMakeup.Position),
                                                         trbDynMakeup.Min,
                                                         trbDynMakeup.Max);

    trbDynLimCeil.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                          'LimCeil_x10',
                                                          trbDynLimCeil.Position),
                                                          trbDynLimCeil.Min,
                                                          trbDynLimCeil.Max);

    trbDynLimLook.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                          'LimLook_ms',
                                                          trbDynLimLook.Position),
                                                          trbDynLimLook.Min,
                                                          trbDynLimLook.Max);

    trbDynLimRel.Position := EnsureRange(Ini.ReadInteger('Dynamics',
                                                      'LimRel_x10',
                                                      trbDynLimRel.Position),
                                                      trbDynLimRel.Min,
                                                      trbDynLimRel.Max);

    cmbDynTPOS.ItemIndex := EnsureRange(Ini.ReadInteger('Dynamics',
                                                        'TPOSIndex',
                                                        cmbDynTPOS.ItemIndex),
                                                        0,
                                                        cmbDynTPOS.Items.Count - 1);

    // Flanger
    cbFlEnabled.Checked := Ini.ReadBool('Flanger',
                                        'Enabled',
                                        cbFlEnabled.Checked);

    trbFlWet.Position := EnsureRange(Ini.ReadInteger('Flanger',
                                                      'Wet',
                                                      trbFlWet.Position),
                                                      trbFlWet.Min,
                                                      trbFlWet.Max);

    trbFlDelay.Position := EnsureRange(Ini.ReadInteger('Flanger',
                                                      'Delay',
                                                      trbFlDelay.Position),
                                                      trbFlDelay.Min,
                                                      trbFlDelay.Max);

    trbFlDepth.Position := EnsureRange(Ini.ReadInteger('Flanger',
                                                      'Depth',
                                                      trbFlDepth.Position),
                                                      trbFlDepth.Min,
                                                      trbFlDepth.Max);

    trbFlRate.Position := EnsureRange(Ini.ReadInteger('Flanger',
                                                      'Rate_x100',
                                                      trbFlRate.Position),
                                                      trbFlRate.Min,
                                                      trbFlRate.Max);

    trbFlFeedback.Position := EnsureRange(Ini.ReadInteger('Flanger',
                                                          'Feedback_x100',
                                                          trbFlFeedback.Position),
                                                          trbFlFeedback.Min,
                                                          trbFlFeedback.Max);

    // Chorus
    cbChEnabled.Checked := Ini.ReadBool('Chorus',
                                        'Enabled',
                                        cbChEnabled.Checked);

    trbChMix.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                     'Mix',
                                                     trbChMix.Position),
                                                     trbChMix.Min,
                                                     trbChMix.Max);

    trbChFeedback.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                          'Feedback',
                                                          trbChFeedback.Position),
                                                          trbChFeedback.Min,
                                                          trbChFeedback.Max);

    trbChBaseDelay.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                           'BaseDelayMs',
                                                           trbChBaseDelay.Position),
                                                           trbChBaseDelay.Min,
                                                           trbChBaseDelay.Max);

    trbChDepth.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                       'Depth_x10',
                                                       trbChDepth.Position),
                                                       trbChDepth.Min,
                                                       trbChDepth.Max);

    rbChRateFree.Checked := Ini.ReadBool('Chorus',
                                         'RateFree',
                                         rbChRateFree.Checked);
    rbChRateSync.Checked := not rbChRateFree.Checked;

    trbChRate.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                      'Rate_x100',
                                                      trbChRate.Position),
                                                      trbChRate.Min,
                                                      trbChRate.Max);
    edtChBpm.Text := Ini.ReadString('Chorus',
                                    'TempoBpm',
                                    edtChBpm.Text);

    cmbChNoteDiv.ItemIndex := EnsureRange(Ini.ReadInteger('Chorus',
                                                          'NoteDivIndex',
                                                          cmbChNoteDiv.ItemIndex),
                                                          0,
                                                          cmbChNoteDiv.Items.Count - 1);

    trbChWidth.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                       'WidthPct',
                                                       trbChWidth.Position),
                                                       trbChWidth.Min,
                                                       trbChWidth.Max);

    trbChSmooth.Position := EnsureRange(Ini.ReadInteger('Chorus',
                                                        'SmoothMs',
                                                        trbChSmooth.Position),
                                                        trbChSmooth.Min,
                                                        trbChSmooth.Max);


    // Pitch/Tempo
    {fxPitchTempo.Enabled := Ini.ReadBool('PitchTempo',
                                         'Enabled',
                                         cbPtEnabled.Checked);
    cbPtEnabled.Checked := fxPitchTempo.Enabled;

    cbPtFormants.Checked := Ini.ReadBool('PitchTempo',
                                         'PreserveFormants',
                                         cbPtFormants.Checked);

    trbPtPitch.Position := EnsureRange(Ini.ReadInteger('PitchTempo',
                                                       'Pitch_x10',
                                                       trbPtPitch.Position),
                                                       trbPtPitch.Min,
                                                       trbPtPitch.Max);

    trbPtTempo.Position := EnsureRange(Ini.ReadInteger('PitchTempo',
                                                       'TempoPct',
                                                       trbPtTempo.Position),
                                                       trbPtTempo.Min,
                                                       trbPtTempo.Max);

    trbPtOverlap.Position := EnsureRange(Ini.ReadInteger('PitchTempo',
                                                         'Overlap_x100',
                                                         trbPtOverlap.Position),
                                                         trbPtOverlap.Min,
                                                         trbPtOverlap.Max);

    case Ini.ReadInteger('PitchTempo',
                         'WindowSize',
                         StrToIntDef(Trim(cmbPtWindow.Text), 1024)) of
      512:  cmbPtWindow.ItemIndex := 0;
      1024: cmbPtWindow.ItemIndex := 1;
      2048: cmbPtWindow.ItemIndex := 2;
      4096: cmbPtWindow.ItemIndex := 3;
    else
      cmbPtWindow.ItemIndex := 1;
    end;

    cmbPtMode.ItemIndex := EnsureRange(Ini.ReadInteger('PitchTempo',
                                                       'ModeIndex',
                                                       cmbPtMode.ItemIndex),
                                                       0,
                                                       cmbPtMode.Items.Count - 1);

    }
  finally
    Ini.Free;
  end;

  // Refresh UI enable/labels
  UpdateChorusRateUi();
  UpdateEqLabels();
  UpdateDynamicsLabels();
  UpdateFlangerLabels();
  UpdateChorusLabels();
  //UpdatePitchTempoLabels();

  // Apply loaded values to FX
  ApplyEqFromUI();
  ApplyDynamicsFromUI();
  ApplyFlangerFromUI();
  ApplyChorusFromUI();

end;


procedure TfrmMain.MfWasApiPlayerEngineEnded(Sender: TObject);
begin

  OnAudioEnded(Sender);
end;


procedure TfrmMain.MfWasApiPlayerEngineError(Sender: TObject;
                                             const Msg: string;
                                             const Hr: HRESULT);
begin

  OnEngineError(Sender,
                Msg,
                Hr);
end;


procedure TfrmMain.MfWasApiPlayerEngineProcessed(Sender: TObject;
                                                 const Position100ns: Int64;
                                                 const RawPosition: UInt64);
begin

  OnAudioDataProcessed(Sender,
                       Position100ns,
                       RawPosition);
end;


procedure TfrmMain.MfWasApiPlayerEngineReady(Sender: TObject);
begin

  OnAudioReady(Sender);
end;


procedure TfrmMain.MfWasApiPlayerEngineStateChanged(Sender: TObject;
                                                    const NewState: TDeviceState);
begin

  OnEngineState(Sender,
                NewState);
end;


procedure TfrmMain.SaveFxToIni();
var
  Ini: TIniFile;
  fn: string;

begin

  fn := GetIniFileName();
  Ini := TIniFile.Create(fn);

  try

    // EQ
    Ini.WriteBool('EQ',
                  'Enabled',
                   cbEqEnabled.Checked);
    Ini.WriteBool('EQ',
                  'UseBW',
                  cbEqUseBW.Checked);
    Ini.WriteBool('EQ',
                  'TruePeak',
                  cbEqTruePeak.Checked);
    Ini.WriteInteger('EQ',
                     'Gain_x10',
                     trbEqGain.Position);
    Ini.WriteInteger('EQ',
                     'FreqSlider',
                     trbEqFreq.Position);
    Ini.WriteInteger('EQ',
                     'Q_x10',
                     trbEqQ.Position);
    Ini.WriteInteger('EQ',
                     'BW_x10',
                     trbEqBW.Position);

    // Dynamics
    Ini.WriteBool('Dynamics',
                  'Enabled',
                  cbDynEnabled.Checked);
    Ini.WriteBool('Dynamics',
                  'UseRms',
                   cbDynRms.Checked);
    Ini.WriteBool('Dynamics',
                  'AutoMakeup',
                  cbDynAutoMakeup.Checked);

    Ini.WriteBool('Dynamics',
                  'TruePeak',
                  cbDynTruePeak.Checked);

    Ini.WriteInteger('Dynamics',
                     'Thresh_x10',
                     trbDynThresh.Position);
    Ini.WriteInteger('Dynamics',
                     'Ratio_x10',
                     trbDynRatio.Position);
    Ini.WriteInteger('Dynamics',
                     'Attack_x10',
                     trbDynAttack.Position);
    Ini.WriteInteger('Dynamics',
                     'Release_x10',
                     trbDynRelease.Position);
    Ini.WriteInteger('Dynamics',
                     'Knee_x10',
                     trbDynKnee.Position);
    Ini.WriteInteger('Dynamics',
                     'Makeup_x10',
                     trbDynMakeup.Position);
    Ini.WriteInteger('Dynamics',
                     'LimCeil_x10',
                     trbDynLimCeil.Position);
    Ini.WriteInteger('Dynamics',
                     'LimLook_ms',
                     trbDynLimLook.Position);
    Ini.WriteInteger('Dynamics',
                     'LimRel_x10',
                     trbDynLimRel.Position);
    Ini.WriteInteger('Dynamics',
                     'TPOSIndex',
                     cmbDynTPOS.ItemIndex);

    // Flanger
    Ini.WriteBool('Flanger',
                  'Enabled',
                  cbFlEnabled.Checked);
    Ini.WriteInteger('Flanger',
                     'Wet',
                     trbFlWet.Position);
    Ini.WriteInteger('Flanger',
                     'Delay',
                     trbFlDelay.Position);
    Ini.WriteInteger('Flanger',
                     'Depth',
                     trbFlDepth.Position);
    Ini.WriteInteger('Flanger',
                     'Rate_x100',
                     trbFlRate.Position);
    Ini.WriteInteger('Flanger',
                     'Feedback_x100',
                     trbFlFeedback.Position);

    // Chorus
    Ini.WriteBool('Chorus',
                  'Enabled',
                  cbChEnabled.Checked);
    Ini.WriteInteger('Chorus',
                     'Mix',
                     trbChMix.Position);
    Ini.WriteInteger('Chorus',
                     'Feedback',
                     trbChFeedback.Position);
    Ini.WriteInteger('Chorus',
                     'BaseDelayMs',
                     trbChBaseDelay.Position);
    Ini.WriteInteger('Chorus',
                     'Depth_x10',
                     trbChDepth.Position);

    Ini.WriteBool('Chorus',
                  'RateFree',
                  rbChRateFree.Checked);
    Ini.WriteInteger('Chorus',
                     'Rate_x100',
                     trbChRate.Position);
    Ini.WriteString('Chorus',
                    'TempoBpm',
                     Trim(edtChBpm.Text));
    Ini.WriteInteger('Chorus',
                     'NoteDivIndex',
                     cmbChNoteDiv.ItemIndex);

    Ini.WriteInteger('Chorus',
                     'WidthPct',
                     trbChWidth.Position);
    Ini.WriteInteger('Chorus',
                     'SmoothMs',
                     trbChSmooth.Position);


   { // Pitch/Tempo
    Ini.WriteBool('PitchTempo',
                  'Enabled',
                  cbPtEnabled.Checked);
    Ini.WriteBool('PitchTempo',
                  'PreserveFormants',
                  cbPtFormants.Checked);
    Ini.WriteInteger('PitchTempo',
                     'Pitch_x10',
                     trbPtPitch.Position);
    Ini.WriteInteger('PitchTempo',
                     'TempoPct',
                     trbPtTempo.Position);
    Ini.WriteInteger('PitchTempo',
                     'Overlap_x100',
                     trbPtOverlap.Position);
    Ini.WriteInteger('PitchTempo',
                     'WindowSize',
                     StrToIntDef(Trim(cmbPtWindow.Text), 1024));
    Ini.WriteInteger('PitchTempo',
                     'ModeIndex',
                     cmbPtMode.ItemIndex);  }


  finally

    Ini.Free;
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