// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: EqSettingsFrm.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
//
// Description: The main form where all user (front end) contol takes place.
//              It also reads and write the MFT parameters from/to the ini file.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example3
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
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

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Math,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  MfAudioHighMidLowTypes,
  WASAPIEngine,
  MfPeakMeter,
  EqSettingsFrm,
  EqPlotUtils;


type

  TfrmMain = class(TForm)
    btnLoad: TButton;
    butPlayPause: TButton;
    butStop: TButton;
    lblFile: TLabel;
    stxtStatus: TStaticText;
    Panel2: TPanel;
    lblBarPositionInSTime: TLabel;
    lblBarPositionInSamples: TLabel;
    pbProgress: TProgressBar;
    pnlControls: TPanel;
    lblLow: TLabel;
    lblMid: TLabel;
    lblHigh: TLabel;
    lblRamp: TLabel;
    lblRampMs: TLabel;
    chkEQ: TCheckBox;
    tbLow: TTrackBar;
    tbMid: TTrackBar;
    tbHigh: TTrackBar;
    cbxRamp: TComboBox;
    edtRampMs: TEdit;
    OpenDialog1: TOpenDialog;
    Bevel3: TBevel;
    pmLeft: TMfPeakMeter;
    Bevel2: TBevel;
    pmRight: TMfPeakMeter;
    Label1: TLabel;
    Label2: TLabel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    cbLockVolumeSliders: TCheckBox;
    trbVolumeL: TTrackBar;
    trbVolumeR: TTrackBar;
    Bevel1: TBevel;
    lblDuration: TLabel;
    lblPlayed: TLabel;
    MainMenu1: TMainMenu;
    Application1: TMenuItem;
    Settings1: TMenuItem;
    pnlEq: TPanel;
    imgSpectrumAnalizer: TImage;
    stxtHighIndex: TStaticText;
    stxtMidIndex: TStaticText;
    stxtLowIndex: TStaticText;
    stxtProcessed: TStaticText;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure butPlayPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure chkEQClick(Sender: TObject);
    procedure tbLowChange(Sender: TObject);
    procedure tbMidChange(Sender: TObject);
    procedure tbHighChange(Sender: TObject);
    procedure cbxRampChange(Sender: TObject);
    procedure edtRampMsChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Settings1Click(Sender: TObject);

    private

    FUpdatingUi: Boolean;
    FEqTuning: TEqTuning;
    FWasApiEngine: TWasApiEngine;
    FFileName: string;
    llAudioDuration: LONGLONG;
    FSamplesThrottle: Word;
    FfrmEqSettings: TfrmEqSettings;

    function IniFileName: string;

    procedure LoadEqFromIni(); // Loads the values for the MFT.
    procedure SaveEqLiveToIni();
    procedure ApplyEqTuning(const T: TEqTuning);
    procedure ApplyEqLiveControls(const Enabled: Boolean;
                                  const LowDb,
                                  MidDb,
                                  HighDb: Integer;
                                  const RampModeIndex: Integer;
                                  const RampMs: Integer);

    procedure SetVolumeChannels();
    function RampModeFromCombo(): TMfRampMode;
    procedure UpdateSpectrumPlot();

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

    // Hot keys
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
  end;

const

  // Global hotkey's.
  HK_TOGGLE_PLAY   = 1;
  HK_TOGGLE_PAUSE  = 2;
  HK_TOGGLE_STOP   = 3;

  HK_VOL_UP        = 4;
  HK_VOL_DOWN      = 5;

  HK_GUI_SHOW      = 6;
  HK_GUI_HIDE      = 7;

  {     // Set initial plot.
      LoadEqFromIni();          // If we do INI-based tuning.
      LoadEqTuningFromIni();
      ApplyEqTuning(FEqTuning); // Sends params to engine/MFT.
      FWasApiEngine.ApplyEqTuningImmediate(FEqTuning);
      UpdateSpectrumPlot();     // Coefficients reflect the tuning.}


var
  frmMain: TfrmMain;


implementation

{$R *.dfm}


// Slider helper to invert slider values.
function InvertDb(const V: Integer): Integer;
begin

  Result := -V;
end;


function TfrmMain.IniFileName: string;
begin

  // Keep it simple: same folder, same base name as exe.
  Result := ChangeFileExt(Application.ExeName,
                          '.ini');
end;


procedure TfrmMain.ApplyEqTuning(const T: TEqTuning);
begin

  FEqTuning := T;

  if not Assigned(FWasApiEngine) then
    Exit;

  // Fine tuning (frequency bands / filter shapes)
  FWasApiEngine.SetLowFreqHz(FEqTuning.LowFreqHz);
  FWasApiEngine.SetMidFreqHz(FEqTuning.MidFreqHz);
  FWasApiEngine.SetHighFreqHz(FEqTuning.HighFreqHz);

  FWasApiEngine.SetMidMode(FEqTuning.MidMode);
  FWasApiEngine.SetMidQ(FEqTuning.MidQ);

  FWasApiEngine.SetLowShelfSlope(FEqTuning.LowShelfSlope);
  FWasApiEngine.SetHighShelfSlope(FEqTuning.HighShelfSlope);
end;


procedure TfrmMain.ApplyEqLiveControls(const Enabled: Boolean;
                                       const LowDb,
                                       MidDb,
                                       HighDb: Integer;
                                       const RampModeIndex: Integer;
                                       const RampMs: Integer);
begin

  // Update UI without re-triggering events
  FUpdatingUi := True;

  try

    chkEQ.Checked := Enabled;

    tbLow.Position := EnsureRange(LowDb,
                                  tbLow.Min,
                                  tbLow.Max);

    tbMid.Position := EnsureRange(MidDb,
                                  tbMid.Min,
                                  tbMid.Max);

    tbHigh.Position := EnsureRange(HighDb,
                                   tbHigh.Min,
                                   tbHigh.Max);

    cbxRamp.ItemIndex := EnsureRange(RampModeIndex,
                                     0,
                                     cbxRamp.Items.Count - 1);

    edtRampMs.Text := IntToStr(EnsureRange(RampMs,
                                           0,
                                           2000));
  finally

    FUpdatingUi := False;
  end;

  // Apply to engine (threaded; enqueued commands)
  if Assigned(FWasApiEngine) then
    begin

      FWasApiEngine.EnableEQ(chkEQ.Checked);

      FWasApiEngine.SetLowDb(InvertDb(tbLow.Position));
      FWasApiEngine.SetMidDb(InvertDb(tbMid.Position));
      FWasApiEngine.SetHighDb(InvertDb(tbHigh.Position));

      FWasApiEngine.SetRampMode(RampModeFromCombo);
      FWasApiEngine.SetRampTimeMs(StrToIntDef(edtRampMs.Text,
                                              30));
    end;
end;


procedure TfrmMain.LoadEqFromIni();
var
  Ini: TIniFile;
  T: TEqTuning;
  enabled: Boolean;
  lowDb,
  midDb,
  highDb: Integer;
  rampModeIdx: Integer;
  rampMs: Integer;

begin

  Ini := TIniFile.Create(IniFileName);

  try

    // -------------------------------------------------------------------------
    // 1) Fine tuning (saved by settings dialog) - section [EQ]
    // -------------------------------------------------------------------------
    T.LowFreqHz := Ini.ReadFloat('EQ',
                                 'LowHz',
                                 100.0);

    T.MidFreqHz := Ini.ReadFloat('EQ',
                                 'MidHz',
                                 1000.0);

    T.HighFreqHz := Ini.ReadFloat('EQ',
                                  'HighHz',
                                  10000.0);

    T.MidQ := Ini.ReadFloat('EQ',
                            'MidQ',
                            1.0);

    T.LowShelfSlope := Ini.ReadFloat('EQ',
                                     'LowShelfSlope',
                                     1.0);

    T.HighShelfSlope := Ini.ReadFloat('EQ',
                                      'HighShelfSlope',
                                      1.0);

    T.MidMode := TMfMidMode(Ini.ReadInteger('EQ',
                                            'MidMode',
                                            Ord(mmPeaking)));

    ApplyEqTuning(T);

    // -------------------------------------------------------------------------
    // 2) Live controls (main form) - section [EQLive]
    // -------------------------------------------------------------------------
    enabled := Ini.ReadBool('EQLive',
                            'Enabled',
                            chkEQ.Checked);

    lowDb := Ini.ReadInteger('EQLive',
                             'LowDb',
                             tbLow.Position);

    midDb := Ini.ReadInteger('EQLive',
                             'MidDb',
                             tbMid.Position);

    highDb := Ini.ReadInteger('EQLive',
                              'HighDb',
                              tbHigh.Position);

    rampModeIdx := Ini.ReadInteger('EQLive',
                                   'RampModeIndex',
                                   cbxRamp.ItemIndex);

    rampMs := Ini.ReadInteger('EQLive',
                              'RampMs',
                              StrToIntDef(edtRampMs.Text,
                                          30));

    ApplyEqLiveControls(enabled,
                        lowDb,
                        midDb,
                        highDb,
                        rampModeIdx,
                        rampMs);
  finally
    Ini.Free;
  end;
end;


procedure TfrmMain.SaveEqLiveToIni();
var
  Ini: TIniFile;

begin

  Ini := TIniFile.Create(IniFileName);

  try
    Ini.WriteBool('EQLive',
                  'Enabled',
                  chkEQ.Checked);

    Ini.WriteInteger('EQLive',
                     'LowDb',
                     tbLow.Position);

    Ini.WriteInteger('EQLive',
                     'MidDb',
                     tbMid.Position);

    Ini.WriteInteger('EQLive',
                     'HighDb',
                     tbHigh.Position);

    Ini.WriteInteger('EQLive',
                     'RampModeIndex',
                     cbxRamp.ItemIndex);

    Ini.WriteInteger('EQLive',
                     'RampMs',
                     StrToIntDef(edtRampMs.Text,
                                 30));
  finally

    Ini.Free();
  end;
end;


function TfrmMain.RampModeFromCombo: TMfRampMode;
begin

  case cbxRamp.ItemIndex of
    0: Result := rmOff;
    1: Result := rmFast;
    2: Result := rmSmooth;
    3: Result := rmCustom;
  else
    Result := rmSmooth;
  end;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FWasApiEngine := TWasApiEngine.Create();

  // Wire engine events (Sample 4 principle: callbacks/events only)
  fWasApiEngine.OnReady := OnAudioReady;
  fWasApiEngine.OnProcessed := OnAudioDataProcessed;
  fWasApiEngine.OnEnded := OnAudioEnded;
  fWasApiEngine.OnError := OnEngineError;
  fWasApiEngine.OnStateChanged := OnEngineState;

  // Register global hotkey's.
  RegisterHotKey(Handle,
                 HK_TOGGLE_PLAY,
                 MOD_CONTROL or MOD_ALT,
                 VK_RIGHT);

  RegisterHotKey(Handle,
                 HK_TOGGLE_PAUSE,
                 MOD_CONTROL or MOD_ALT,
                 VK_LEFT);

  RegisterHotKey(Handle,
                 HK_TOGGLE_STOP,
                 MOD_CONTROL or MOD_ALT,
                 VK_SPACE);

  RegisterHotKey(Handle,
                 HK_VOL_UP,
                 MOD_CONTROL or MOD_ALT,
                 VK_UP);

  RegisterHotKey(Handle,
                 HK_VOL_DOWN,
                 MOD_CONTROL or MOD_ALT,
                 VK_DOWN);

  RegisterHotKey(Handle,
                 HK_GUI_SHOW,
                 MOD_CONTROL or MOD_ALT,
                 VK_ESCAPE);

  RegisterHotKey(Handle,
                 HK_GUI_HIDE,
                 MOD_CONTROL or MOD_ALT,
                 VK_F1);

  tbLow.Min := -24;
  tbLow.Max := 24;
  tbLow.Position := 0;

  tbMid.Min := -24;
  tbMid.Max := 24;
  tbMid.Position := 0;

  tbHigh.Min := -24;
  tbHigh.Max := 24;
  tbHigh.Position := 0;

  cbxRamp.Items.Clear;
  cbxRamp.Items.Add('Off');
  cbxRamp.Items.Add('Fast');
  cbxRamp.Items.Add('Smooth');
  cbxRamp.Items.Add('Custom');
  cbxRamp.ItemIndex := 2;

  edtRampMs.Text := '30';

  chkEQ.Checked := True;
  FWasApiEngine.EnableEQ(True);
  FWasApiEngine.SetRampMode(RampModeFromCombo);
  FWasApiEngine.SetRampTimeMs(StrToIntDef(edtRampMs.Text,
                                          30));
  FSamplesThrottle := 0;
  UpdateSpectrumPlot();
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  // Unregister the global hotkey's
  UnregisterHotKey(Handle,
                   HK_TOGGLE_PLAY);
  UnregisterHotKey(Handle,
                   HK_TOGGLE_PAUSE);
  UnregisterHotKey(Handle,
                   HK_TOGGLE_STOP);
  UnregisterHotKey(Handle,
                   HK_VOL_UP);
  UnregisterHotKey(Handle,
                   HK_VOL_DOWN);
  UnregisterHotKey(Handle,
                   HK_GUI_SHOW);
  UnregisterHotKey(Handle,
                   HK_GUI_HIDE);

  FreeAndNil(FWasApiEngine);
end;


procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  iPos: Integer;

begin

  // Life operations with focus on visible GUI.

  // Set volume slider positions to 0.
  if (Shift = [ssShift]) and (Key = VK_ESCAPE) then
    begin

      trbVolumeL.Position := 0;
      trbVolumeR.Position := 0;
      Key := 0;
      Exit;
    end;

  case Key of
    VK_F12:     begin
                  btnLoadClick(nil);
                end;

    VK_F11:     begin

                  iPos := trbVolumeL.Position + trbVolumeR.Position div 2;
                  trbVolumeL.Position := iPos;
                  trbVolumeR.Position := iPos;
                end;

  end;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  hr: HResult;

begin

  if OpenDialog1.Execute then
    begin

      FFileName := OpenDialog1.FileName;
      stxtStatus.Caption := Format('Loaded file: %s', [ExtractFileName(FFileName)]);

      // Get the length of the audiofile.
      hr := GetFileDuration(StrToPWideChar(FFileName),
                            llAudioDuration);
      if FAILED(hr) then
        begin

          ShowMessage('Could not retrieve the duration of the audio file.');
          llAudioDuration := 0;
        end;

      lblDuration.Caption := Format('Duration: %s',
                                    [HnsTimeToStr(llAudioDuration, False)]);

      // Set progressbar max
      pbProgress.Max := llAudioDuration div 1000000;
      LoadEqFromIni();
      FWasApiEngine.OpenFile(FFileName,
                             llAudioDuration);
    end;
end;


procedure TfrmMain.butPlayPauseClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fWasApiEngine) then
    Exit;

  case fWasApiEngine.DeviceState of

    dsPlay:
      begin
        hr := fWasApiEngine.Pause();
      end;

    dsReady,
    dsStop,
    dsPause:
      begin

        LoadEqFromIni();

        // Activate the peakmeters.
        pmLeft.Enabled := True;
        pmRight.Enabled := True;

        // Keep volume on previous volume.
        SetVolumeChannels();

        fWasApiEngine.EnableEQ(chkEQ.Checked);

        hr := fWasApiEngine.Start();
      end;

  else
    Exit;
  end;

  if FAILED(hr) then
    stxtStatus.Caption := Format('Play/Pause failed for file: %s with Error: %d',
                                 [ExtractFileName(FFileName), hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  FWasApiEngine.Stop();
end;


procedure TfrmMain.chkEQClick(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    FWasApiEngine.EnableEQ(chkEQ.Checked);

  SaveEqLiveToIni();
end;


procedure TfrmMain.tbLowChange(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    begin

      FWasApiEngine.SetLowDb(InvertDb(tbLow.Position));
      stxtLowIndex.Caption := IntToStr(InvertDb(tbLow.Position));

      UpdateSpectrumPlot();
      SaveEqLiveToIni();
    end;

end;


procedure TfrmMain.tbMidChange(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    begin

      FWasApiEngine.SetMidDb(InvertDb(tbMid.Position));
      stxtMidIndex.Caption := IntToStr(InvertDb(tbMid.Position));
      UpdateSpectrumPlot();
      SaveEqLiveToIni();
    end;
end;


procedure TfrmMain.tbHighChange(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    begin

      FWasApiEngine.SetHighDb(InvertDb(tbHigh.Position));
      stxtHighIndex.Caption := IntToStr(InvertDb(tbHigh.Position));
      UpdateSpectrumPlot();
      SaveEqLiveToIni();
    end;
end;


procedure TfrmMain.Settings1Click(Sender: TObject);
begin

  if not assigned(FfrmEqSettings) then
    FfrmEqSettings := TfrmEqSettings.Create(Self);

  if (FfrmEqSettings.ShowModal = mrOk) then
    begin

      // Do something.
    end
  else
    begin

      // User canceled.
      // Do something.
    end;
end;


procedure TfrmMain.SetVolumeChannels();
var
  hr: HResult;

begin

  hr := E_FAIL;

  if not Assigned(fWasApiEngine) then
    Exit;

  // Stereo
  // The first stereo channel (0) is always the LEFT one! SetVolumes
  if (fWasApiEngine.SoundChannels = 2) then
    begin
      hr := fWasApiEngine.SetVolumes(Abs(trbVolumeL.Position) * 0.01,
                                     Abs(trbVolumeR.Position) * 0.01);
    end;

  if FAILED(hr) then
    stxtStatus.Caption := Format('Adjusting volumes failed with error: %d.',
                                [hr]);
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


procedure TfrmMain.cbxRampChange(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    FWasApiEngine.SetRampMode(RampModeFromCombo);
  UpdateSpectrumPlot();
  SaveEqLiveToIni();
end;


procedure TfrmMain.edtRampMsChange(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  if Assigned(FWasApiEngine) then
    FWasApiEngine.SetRampTimeMs(StrToIntDef(edtRampMs.Text,
                                            30));
  UpdateSpectrumPlot();
  SaveEqLiveToIni();
end;


procedure TfrmMain.UpdateSpectrumPlot();
var
  R: TRect;
  Low,
  Mid,
  High: TBiquadCoeffs;
  Fs: Double;

begin

  R := Rect(0,
            0,
            imgSpectrumAnalizer.Width,
            imgSpectrumAnalizer.Height);

  if (R.Right <= 0) or (R.Bottom <= 0) then
    Exit;

  imgSpectrumAnalizer.Picture.Bitmap.SetSize(R.Right,
                                             R.Bottom);

  imgSpectrumAnalizer.Picture.Bitmap.Canvas.Brush.Color := $00001A00;
  imgSpectrumAnalizer.Picture.Bitmap.Canvas.FillRect(R);

  if Assigned(FWasApiEngine) and
     FWasApiEngine.GetEqBiquadCoeffs(Low,
                                     Mid,
                                     High,
                                     Fs) then
    PlotEqResponse(imgSpectrumAnalizer.Picture.Bitmap.Canvas,
                   R,
                   Fs,
                   Low,
                   Mid,
                   High);

  //imgSpectrumAnalizer.Invalidate;
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


  lblPlayed.Caption := Format('Played: %s',
                              [tstr]);

  inc(FSamplesThrottle);
  if (FSamplesThrottle >= 60) and (iSamples > 1024) then
    begin

      stxtProcessed.Caption := Format('Samples: %d kb',
                                      [iSamples div 1024]);
      FSamplesThrottle := 0;
    end;
end;


procedure TfrmMain.OnAudioReady(Sender: TObject);
var
  durSec: Int64;

begin

  if (fWasApiEngine = nil) then
    Exit;

  if (llAudioDuration > 0) then
    durSec := llAudioDuration div 10000000
  else
    durSec := 0;

  if (durSec > High(Integer)) then
    pbProgress.Max := High(Integer)
  else
    pbProgress.Max := Integer(durSec);

  FSamplesThrottle := 0;
  pbProgress.Position := 0;
  pbProgress.Enabled := (pbProgress.Max > 0);
  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
  SetVolumeChannels();

  // EQ Bass/treble ============================================================

  if chkEQ.Checked then
    begin

      // re-apply current UI values to engine (important after plugging a new MFT or new file)
      fWasApiEngine.SetLowDb(InvertDb(tbLow.Position));
      fWasApiEngine.SetMidDb(InvertDb(tbMid.Position));
      fWasApiEngine.SetHighDb(InvertDb(tbHigh.Position));
      cbxRampChange(nil);
    end;
end;


procedure TfrmMain.OnAudioEnded(Sender: TObject);
begin

  stxtStatus.Caption := Format('Stopped: %s',
                              [fFileName]);

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  // Resets controls, engine.stop has allready being called in the engine loop.
  butStop.OnClick(nil);
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
        stxtProcessed.Caption := 'Samples: 0';
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

  // ===========================================================================


procedure TfrmMain.pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  secPos: Integer;
  hnsPos: Int64;

begin

  if (pbProgress.Max <= 0) or (llAudioDuration <= 0) then
    Exit;

  // Show only when playing/pause
  if fWasApiEngine.DeviceState in [dsPlay, dsPause] then
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

  if (fWasApiEngine = nil) then
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

  hr := fWasApiEngine.SeekTo(posHns);

  if SUCCEEDED(hr) then
    pbProgress.Position := Integer(secPos)
  else
    stxtStatus.Caption := Format('SeekTo failed. (hr=%d)',
                                 [hr]);
end;



procedure TfrmMain.WMHotKey(var Msg: TWMHotKey);
begin
  inherited;

  // Global hotkey values.
  // HK_TOGGLE_PLAY   = 1
  // HK_TOGGLE_PAUSE  = 2
  // HK_TOGGLE_STOP   = 3
  // HK_VOL_UP        = 4
  // HK_VOL_DOWN      = 5
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


    HK_VOL_UP:
      begin

        trbVolumeL.Position :=  trbVolumeL.Position - 1;

        trbVolumeRChange(trbVolumeL);

        trbVolumeR.Position := trbVolumeR.Position - 1;

        trbVolumeRChange(trbVolumeR);
      end;

    HK_VOL_DOWN:
      begin

        trbVolumeL.Position := trbVolumeL.Position + 1;
        trbVolumeLChange(trbVolumeL);

        trbVolumeR.Position := trbVolumeR.Position + 1;
        trbVolumeRChange(trbVolumeR);
      end;

    HK_GUI_SHOW:
      begin

        Self.Show;
      end;

    HK_GUI_HIDE:
      begin

        Self.Hide;
      end;
  end;
end;



// initialization and finalization =============================================


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version.' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();

end.