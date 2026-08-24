// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: EqSettingsFrm.pas
// Kind: Pascal Unit
// Release date: 20-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Set, stores and reads EQ and Compressor/limiter settings and
//              directly feed them to the engine when running.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX400/Samples/WasApiPlayer/Example4
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
unit EqSettingsFrm;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {Application}
  MfAudioHighMidLowTypes,
  AudioDynamicsDSP;

type

  TEqApplyEvent = procedure(Sender: TObject;
                            const Tuning: TEqTuning) of object;

  TDynamicsApplyEvent = procedure(Sender: TObject;
                                  const Settings: TDynamicsSettings) of object;

  TfrmEqSettings = class(TForm)
    pcMain: TPageControl;
    tsEQ: TTabSheet;
    tsDynamics: TTabSheet;
    grpLow: TGroupBox;
    lblLowFreqHz: TLabel;
    edtLowFreqHz: TEdit;
    udLowFreqHz: TUpDown;
    lblLowSlope: TLabel;
    edtLowSlope: TEdit;
    udLowSlope: TUpDown;
    grpMid: TGroupBox;
    lblMidFreqHz: TLabel;
    edtMidFreqHz: TEdit;
    udMidFreqHz: TUpDown;
    lblMidQ: TLabel;
    edtMidQ: TEdit;
    udMidQ: TUpDown;
    rgMidMode: TRadioGroup;
    grpHigh: TGroupBox;
    lblHighFreqHz: TLabel;
    edtHighFreqHz: TEdit;
    udHighFreqHz: TUpDown;
    lblHighSlope: TLabel;
    edtHighSlope: TEdit;
    udHighSlope: TUpDown;
    grpComp: TGroupBox;
    chkCompEnabled: TCheckBox;
    lblCompThreshold: TLabel;
    edtCompThreshold: TEdit;
    udCompThreshold: TUpDown;
    lblCompRatio: TLabel;
    edtCompRatio: TEdit;
    udCompRatio: TUpDown;
    lblCompAttack: TLabel;
    edtCompAttack: TEdit;
    udCompAttack: TUpDown;
    lblCompRelease: TLabel;
    edtCompRelease: TEdit;
    udCompRelease: TUpDown;
    lblCompMakeup: TLabel;
    edtCompMakeup: TEdit;
    udCompMakeup: TUpDown;
    chkCompAutoMakeup: TCheckBox;
    grpLimiter: TGroupBox;
    chkLimEnabled: TCheckBox;
    lblLimCeiling: TLabel;
    edtLimCeiling: TEdit;
    udLimCeiling: TUpDown;
    lblLimRelease: TLabel;
    edtLimRelease: TEdit;
    udLimRelease: TUpDown;
    lblLimLookahead: TLabel;
    edtLimLookahead: TEdit;
    udLimLookahead: TUpDown;
    lblLimKnee: TLabel;
    edtLimKnee: TEdit;
    udLimKnee: TUpDown;
    lblLimDetector: TLabel;
    cbLimDetector: TComboBox;
    lblLimRmsWindow: TLabel;
    edtLimRmsWindow: TEdit;
    udLimRmsWindow: TUpDown;
    chkLimTruePeak: TCheckBox;
    cbLimOversample: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnDefaults: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edPeakMeterFreq: TEdit;

    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edPeakMeterFreqExit(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private

    FOnApply: TEqApplyEvent;
    FOnApplyDynamics: TDynamicsApplyEvent;

    // Live apply (modeless)
    FApplyDynTimer: TTimer;
    FApplyDynPending: Boolean;

    FDefaults: TEqTuning;
    FTuning: TEqTuning;

    FDynDefaults: TDynamicsSettings;
    FDynSettings: TDynamicsSettings;

    FIniFileName: string;
    FIniSection: string;

    procedure ScheduleApplyDynamics;
    procedure ApplyDynamicsNow(const SaveToIni: Boolean);
    procedure DynUiChanged(Sender: TObject);
    procedure ApplyDynTimerTick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    function ReadTuningFromUi(): TEqTuning;
    procedure WriteTuningToUi(const T: TEqTuning);

    function ReadDynamicsFromUi(): TDynamicsSettings;
    procedure WriteDynamicsToUi(const S: TDynamicsSettings);

  public

    // Optional: if configured, Apply/OK can auto-save to ini.
    procedure ConfigureStorage(const IniFileName: string;
                               const Section: string;
                               const Defaults: TEqTuning);

    procedure LoadFromIni(const IniFileName: string;
                          const Section: string;
                          const Defaults: TEqTuning);

    procedure SaveToIni(const IniFileName: string;
                        const Section: string;
                        const T: TEqTuning);

    // Dynamics storage (can use the same Ini + Section)
    procedure ConfigureDynamicsStorage(const IniFileName: string;
                                       const Section: string;
                                       const Defaults: TDynamicsSettings);

    procedure LoadDynamicsFromIni(const IniFileName: string;
                                  const Section: string;
                                  const Defaults: TDynamicsSettings);

    procedure SaveDynamicsToIni(const IniFileName: string;
                                const Section: string;
                                const S: TDynamicsSettings);

    property OnApply: TEqApplyEvent read FOnApply write FOnApply;
    property OnApplyDynamics: TDynamicsApplyEvent read FOnApplyDynamics write FOnApplyDynamics;

    procedure SetInitialTuning(const T: TEqTuning);
    procedure SetInitialDynamics(const S: TDynamicsSettings);

    function GetCurrentDynamics: TDynamicsSettings;
  end;

var
  frmEqSettings: TfrmEqSettings;


implementation

{$R *.dfm}

uses
  System.Math;


// Helpers ---------------------------------------------------------------------

function TryReadSingle(const S: string; out V: Single): Boolean;
var
  fs: TFormatSettings;
  tmp: string;
  d: Double;

begin

  // Accept both "," and "." as decimal separator.
  tmp := StringReplace(S,
                       ',',
                       '.',
                       [rfReplaceAll]);

  fs := TFormatSettings.Create();
  fs.DecimalSeparator := '.';
  Result := TryStrToFloat(tmp,
                          d,
                          fs);
  if Result then
    V := d * 1.0;
end;


function StrToFloatDefUS(const S: string;
                         const Def: Single): Single;
var
  fs: TFormatSettings;

begin

  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';

  // Accept both "," and "." as decimal separator.
  Result := StrToFloatDef(StringReplace(S,
                                        ',',
                                        '.',
                                        [rfReplaceAll]),
                                        Def,
                                        fs);
end;


function FloatToStrUS(const V: Single): string;
var
  fs: TFormatSettings;

begin

  fs := TFormatSettings.Create();
  fs.DecimalSeparator := '.';
  Result := FloatToStr(V, fs);
end;


function ClampInt(const V, VMin, VMax: Integer): Integer;
begin
  Result := V;
  if Result < VMin then
    Result := VMin
  else
    if Result > VMax then
      Result := VMax;
end;

// -----------------------------------------------------------------------------


procedure TfrmEqSettings.FormCreate(Sender: TObject);
begin

  // UpDowns are integer-only; we store scaled ints for slope/Q (x100).
  udLowFreqHz.Min := 10;
  udLowFreqHz.Max := 400;
  udMidFreqHz.Min := 200;
  udMidFreqHz.Max := 6000;
  udHighFreqHz.Min := 2000;
  udHighFreqHz.Max := 22000;

  udMidQ.Min := 30;
  udMidQ.Max := 600; // 0.30 .. 6.00
  udLowSlope.Min := 10;
  udLowSlope.Max := 400; // 0.10 .. 4.00
  udHighSlope.Min := 10;
  udHighSlope.Max := 400;

  rgMidMode.Items.Clear;
  rgMidMode.Items.Add('Peaking (Bell)');
  rgMidMode.Items.Add('Notch (Band-stop)');
  rgMidMode.ItemIndex := 0;

  // Dynamics ranges
  udCompThreshold.Min := -60;
  udCompThreshold.Max := 0;

  udCompRatio.Min := 10;   // 1.0
  udCompRatio.Max := 200;  // 20.0

  udCompAttack.Min := 1;
  udCompAttack.Max := 200;

  udCompRelease.Min := 10;
  udCompRelease.Max := 2000;

  udCompMakeup.Min := -24;
  udCompMakeup.Max := 24;

  udLimCeiling.Min := -24;
  udLimCeiling.Max := 0;

  udLimRelease.Min := 10;
  udLimRelease.Max := 2000;

  udLimLookahead.Min := 0;
  udLimLookahead.Max := 50;

  udLimKnee.Min := 0;
  udLimKnee.Max := 24;

  udLimRmsWindow.Min := 1;
  udLimRmsWindow.Max := 200;

  cbLimDetector.Items.Clear;
  cbLimDetector.Items.Add('Peak');
  cbLimDetector.Items.Add('RMS');
  cbLimDetector.ItemIndex := 0;

  cbLimOversample.Items.Clear;
  cbLimOversample.Items.Add('1x');
  cbLimOversample.Items.Add('2x');
  cbLimOversample.Items.Add('4x');
  cbLimOversample.ItemIndex := 0;

  // Reasonable built-in defaults (can be overridden by ConfigureStorage)
  FillChar(FDefaults, SizeOf(FDefaults), 0);
  FDefaults.LowFreqHz := 100.0;
  FDefaults.MidFreqHz := 1000.0;
  FDefaults.HighFreqHz := 10000.0;
  FDefaults.MidQ := 1.0;
  FDefaults.LowShelfSlope := 1.0;
  FDefaults.HighShelfSlope := 1.0;
  FDefaults.MidMode := mmPeaking;

  FTuning := FDefaults;

  FDynDefaults := TDynamicsSettings.Defaults;
  FDynSettings := FDynDefaults;

  WriteTuningToUi(FTuning);
  WriteDynamicsToUi(FDynSettings);

  // Live apply timer (Dynamics)
  FApplyDynTimer := TTimer.Create(Self);
  FApplyDynTimer.Enabled := False;
  FApplyDynTimer.Interval := 75;
  FApplyDynTimer.OnTimer := ApplyDynTimerTick;

  // Wire dynamics UI change events at runtime (keeps DFM stable)
  chkCompEnabled.OnClick := DynUiChanged;
  chkCompAutoMakeup.OnClick := DynUiChanged;
  chkLimEnabled.OnClick := DynUiChanged;
  chkLimTruePeak.OnClick := DynUiChanged;

  cbLimDetector.OnChange := DynUiChanged;
  cbLimOversample.OnChange := DynUiChanged;

  edtCompThreshold.OnChange := DynUiChanged;
  edtCompRatio.OnChange := DynUiChanged;
  edtCompAttack.OnChange := DynUiChanged;
  edtCompRelease.OnChange := DynUiChanged;
  edtCompMakeup.OnChange := DynUiChanged;

  edtLimCeiling.OnChange := DynUiChanged;
  edtLimRelease.OnChange := DynUiChanged;
  edtLimLookahead.OnChange := DynUiChanged;
  edtLimKnee.OnChange := DynUiChanged;
  edtLimRmsWindow.OnChange := DynUiChanged;



  // Modeless default behavior: hide on close
  OnClose := FormClose;

end;


procedure TfrmEqSettings.SetInitialTuning(const T: TEqTuning);
begin

  FTuning := T;
  WriteTuningToUi(FTuning);
end;


procedure TfrmEqSettings.SetInitialDynamics(const S: TDynamicsSettings);
begin

  FDynSettings := S;
  WriteDynamicsToUi(FDynSettings);
end;


function TfrmEqSettings.GetCurrentDynamics: TDynamicsSettings;
begin

  Result := FDynSettings;
end;


procedure TfrmEqSettings.btnApplyClick(Sender: TObject);
var
  T: TEqTuning;
  S: TDynamicsSettings;

begin

  T := ReadTuningFromUi;
  FTuning := T;
  // Normalize UI to clamped values (but do NOT revert unrelated fields).
  WriteTuningToUi(FTuning);

  S := ReadDynamicsFromUi;
  FDynSettings := S;
  WriteDynamicsToUi(FDynSettings);

  // If storage is configured, save what we just applied.
  if (FIniFileName <> '') and (FIniSection <> '') then
    begin

      SaveToIni(FIniFileName,
                FIniSection,
                FTuning);

      SaveDynamicsToIni(FIniFileName,
                        FIniSection,
                        FDynSettings);
    end;

  if Assigned(FOnApply) then
    FOnApply(Self,
             T);

  if Assigned(FOnApplyDynamics) then
    FOnApplyDynamics(Self,
                     S);
end;


procedure TfrmEqSettings.btnCancelClick(Sender: TObject);
begin

  Hide();
end;


procedure TfrmEqSettings.btnDefaultsClick(Sender: TObject);
begin

  FTuning := FDefaults;
  WriteTuningToUi(FTuning);

  FDynSettings := FDynDefaults;
  WriteDynamicsToUi(FDynSettings);

  if Assigned(FOnApply) then
    FOnApply(Self,
             FTuning);

  if Assigned(FOnApplyDynamics) then
    FOnApplyDynamics(Self,
                     FDynSettings);

  if (FIniFileName <> '') and (FIniSection <> '') then
    begin

      SaveToIni(FIniFileName,
                FIniSection,
                FTuning);

      SaveDynamicsToIni(FIniFileName,
                        FIniSection,
                        FDynSettings);
    end;
end;


procedure TfrmEqSettings.btnOKClick(Sender: TObject);
begin

  // Apply once on OK for convenience
  btnApplyClick(Sender);

  // Modeless: hide instead of returning a modal result
  Hide();
end;


function TfrmEqSettings.ReadTuningFromUi(): TEqTuning;
var
  v: Single;

begin

  // Start from current tuning so a single invalid/empty field doesn't
  // reset everything back to defaults.
  Result := FTuning;

  if TryReadSingle(Trim(edtLowFreqHz.Text),
                        v) then
    Result.LowFreqHz := v;

  if TryReadSingle(Trim(edtMidFreqHz.Text),
                        v) then
    Result.MidFreqHz := v;

  if TryReadSingle(Trim(edtHighFreqHz.Text),
                        v) then
    Result.HighFreqHz := v;

  if TryReadSingle(Trim(edtMidQ.Text),
                        v) then
    Result.MidQ := v;

  if TryReadSingle(Trim(edtLowSlope.Text),
                        v) then
    Result.LowShelfSlope := v;

  if TryReadSingle(Trim(edtHighSlope.Text),
                        v) then
    Result.HighShelfSlope := v;

  // Notch or Bell
  if (rgMidMode.ItemIndex = 1) then
    Result.MidMode := mmNotch
  else
    Result.MidMode := mmPeaking;

  // Clamp UI values (engine also clamps, but keep UI sane)
  Result.LowFreqHz := EnsureRange(Result.LowFreqHz,
                              10.0,
                              400.0);
  Result.MidFreqHz := EnsureRange(Result.MidFreqHz,
                              200.0,
                              6000.0);
  Result.HighFreqHz := EnsureRange(Result.HighFreqHz,
                               2000.0,
                               22000.0);

  Result.MidQ := EnsureRange(Result.MidQ,
                             0.30,
                             6.00);

  Result.LowShelfSlope := EnsureRange(Result.LowShelfSlope,
                                      0.10,
                                      4.00);

  Result.HighShelfSlope := EnsureRange(Result.HighShelfSlope,
                                       0.10,
                                       4.00);
end;


procedure TfrmEqSettings.WriteTuningToUi(const T: TEqTuning);
begin

  edtLowFreqHz.Text := IntToStr(Round(EnsureRange(T.LowFreqHz,
                                                  10.0,
                                                  400.0)));

  edtMidFreqHz.Text := IntToStr(Round(EnsureRange(T.MidFreqHz,
                                                  200.0,
                                                  6000.0)));

  edtHighFreqHz.Text := IntToStr(Round(EnsureRange(T.HighFreqHz,
                                                   2000.0,
                                                   22000.0)));

  edtMidQ.Text := FloatToStrUS(EnsureRange(T.MidQ,
                                           0.30,
                                           6.00));

  edtLowSlope.Text := FloatToStrUS(EnsureRange(T.LowShelfSlope,
                                               0.10,
                                               4.00));

  edtHighSlope.Text := FloatToStrUS(EnsureRange(T.HighShelfSlope,
                                                0.10,
                                                4.00));

  udLowFreqHz.Position := StrToIntDef(edtLowFreqHz.Text,
                                      100);

  udMidFreqHz.Position := StrToIntDef(edtMidFreqHz.Text,
                                      1000);

  udHighFreqHz.Position := StrToIntDef(edtHighFreqHz.Text,
                                       10000);

  udMidQ.Position := Round(StrToFloatDefUS(edtMidQ.Text,
                                           1.0) * 100);

  udLowSlope.Position := Round(StrToFloatDefUS(edtLowSlope.Text,
                                               1.0) * 100);

  udHighSlope.Position := Round(StrToFloatDefUS(edtHighSlope.Text,
                                                1.0) * 100);

  if (T.MidMode = mmNotch) then
    rgMidMode.ItemIndex := 1
  else
    rgMidMode.ItemIndex := 0;
end;


function TfrmEqSettings.ReadDynamicsFromUi: TDynamicsSettings;
var
  v: Integer;

begin
  Result := FDynSettings;

  Result.CompEnabled := chkCompEnabled.Checked;
  Result.CompAutoMakeup := chkCompAutoMakeup.Checked;

  v := StrToIntDef(Trim(edtCompThreshold.Text),
                        Result.CompThresholdDb);
  Result.CompThresholdDb := ClampInt(v,
                                     -60,
                                     0);

  v := StrToIntDef(Trim(edtCompRatio.Text),
                        Result.CompRatioX10);
  Result.CompRatioX10 := ClampInt(v,
                                  10,
                                  200);

  v := StrToIntDef(Trim(edtCompAttack.Text),
                        Result.CompAttackMs);
  Result.CompAttackMs := ClampInt(v,
                                  1,
                                  200);

  v := StrToIntDef(Trim(edtCompRelease.Text),
                        Result.CompReleaseMs);
  Result.CompReleaseMs := ClampInt(v,
                                   10,
                                   2000);

  v := StrToIntDef(Trim(edtCompMakeup.Text),
                        Result.CompMakeupDb);
  Result.CompMakeupDb := ClampInt(v,
                                  -24,
                                  24);

  Result.LimEnabled := chkLimEnabled.Checked;
  Result.LimTruePeak := chkLimTruePeak.Checked;

  v := StrToIntDef(Trim(edtLimCeiling.Text),
                        Result.LimCeilingDb);
  Result.LimCeilingDb := ClampInt(v,
                                  -24,
                                  0);

  v := StrToIntDef(Trim(edtLimRelease.Text),
                        Result.LimReleaseMs);
  Result.LimReleaseMs := ClampInt(v,
                                  10,
                                  2000);

  v := StrToIntDef(Trim(edtLimLookahead.Text),
                        Result.LimLookaheadMs);
  Result.LimLookaheadMs := ClampInt(v,
                                    0,
                                    50);

  v := StrToIntDef(Trim(edtLimKnee.Text),
                        Result.LimKneeDb);
  Result.LimKneeDb := ClampInt(v,
                               0,
                               24);

  if (cbLimDetector.ItemIndex = 1) then
    Result.LimDetector := ldRms
  else
    Result.LimDetector := ldPeak;

  v := StrToIntDef(Trim(edtLimRmsWindow.Text),
                        Result.LimRmsWindowMs);
  Result.LimRmsWindowMs := ClampInt(v,
                                    1,
                                     200);

  case cbLimOversample.ItemIndex of
    1: Result.LimOversample := 2;
    2: Result.LimOversample := 4;
  else
    Result.LimOversample := 1;
  end;

  // Sanity: TP only meaningful with limiter
  if not Result.LimEnabled then
    Result.LimTruePeak := False;
end;


procedure TfrmEqSettings.WriteDynamicsToUi(const S: TDynamicsSettings);
begin

  chkCompEnabled.Checked := S.CompEnabled;
  chkCompAutoMakeup.Checked := S.CompAutoMakeup;

  edtCompThreshold.Text := IntToStr(ClampInt(S.CompThresholdDb,
                                             -60,
                                             0));
  udCompThreshold.Position := StrToIntDef(edtCompThreshold.Text,
                                          -18);

  edtCompRatio.Text := IntToStr(ClampInt(S.CompRatioX10,
                                         10,
                                         200));
  udCompRatio.Position := StrToIntDef(edtCompRatio.Text,
                                      40);

  edtCompAttack.Text := IntToStr(ClampInt(S.CompAttackMs,
                                          1,
                                          200));
  udCompAttack.Position := StrToIntDef(edtCompAttack.Text,
                                       10);

  edtCompRelease.Text := IntToStr(ClampInt(S.CompReleaseMs,
                                           10,
                                           2000));
  udCompRelease.Position := StrToIntDef(edtCompRelease.Text,
                                        150);

  edtCompMakeup.Text := IntToStr(ClampInt(S.CompMakeupDb,
                                          -24,
                                          24));
  udCompMakeup.Position := StrToIntDef(edtCompMakeup.Text,
                                       0);

  chkLimEnabled.Checked := S.LimEnabled;
  chkLimTruePeak.Checked := S.LimTruePeak;

  edtLimCeiling.Text := IntToStr(ClampInt(S.LimCeilingDb,
                                          -24,
                                          0));
  udLimCeiling.Position := StrToIntDef(edtLimCeiling.Text,
                                       -1);

  edtLimRelease.Text := IntToStr(ClampInt(S.LimReleaseMs,
                                          10,
                                          2000));
  udLimRelease.Position := StrToIntDef(edtLimRelease.Text,
                                       120);

  edtLimLookahead.Text := IntToStr(ClampInt(S.LimLookaheadMs,
                                            0,
                                            50));
  udLimLookahead.Position := StrToIntDef(edtLimLookahead.Text,
                                         5);

  edtLimKnee.Text := IntToStr(ClampInt(S.LimKneeDb,
                                       0,
                                       24));
  udLimKnee.Position := StrToIntDef(edtLimKnee.Text,
                                    0);

  if S.LimDetector = ldRms then
    cbLimDetector.ItemIndex := 1
  else
    cbLimDetector.ItemIndex := 0;

  edtLimRmsWindow.Text := IntToStr(ClampInt(S.LimRmsWindowMs,
                                            1,
                                            200));
  udLimRmsWindow.Position := StrToIntDef(edtLimRmsWindow.Text,
                                         50);

  case S.LimOversample of
    2: cbLimOversample.ItemIndex := 1;
    4: cbLimOversample.ItemIndex := 2;
  else
    cbLimOversample.ItemIndex := 0;
  end;
end;


procedure TfrmEqSettings.ConfigureStorage(const IniFileName: string;
                                          const Section: string;
                                          const Defaults: TEqTuning);
begin

  FIniFileName := IniFileName;
  FIniSection := Section;
  FDefaults := Defaults;
  // Keep a sane base.
  if (FTuning.LowFreqHz = 0) and
     (FTuning.MidFreqHz = 0) and
     (FTuning.HighFreqHz = 0) then
    FTuning := FDefaults;
end;


procedure TfrmEqSettings.ConfigureDynamicsStorage(const IniFileName: string;
                                                  const Section: string;
                                                  const Defaults: TDynamicsSettings);
begin

  FIniFileName := IniFileName;
  FIniSection := Section;
  FDynDefaults := Defaults;
  if (FDynSettings.CompRatioX10 = 0) and (FDynSettings.LimReleaseMs = 0) then
    FDynSettings := FDynDefaults;
end;


procedure TfrmEqSettings.LoadFromIni(const IniFileName: string;
                                     const Section: string;
                                     const Defaults: TEqTuning);
var
  ini: TIniFile;
  t: TEqTuning;

begin

  t := Defaults;

  if not FileExists(IniFileName) then
    begin
      WriteTuningToUi(t);
      Exit;
    end;

  ini := TIniFile.Create(IniFileName);

  try

    t.LowFreqHz := ini.ReadFloat(Section,
                                 'LowFreqHz',
                                 t.LowFreqHz);

    t.MidFreqHz := ini.ReadFloat(Section,
                                 'MidFreqHz',
                                 t.MidFreqHz);

    t.HighFreqHz := ini.ReadFloat(Section,
                                  'HighFreqHz',
                                  t.HighFreqHz);

    t.MidQ := ini.ReadFloat(Section,
                            'MidQ',
                            t.MidQ);

    t.LowShelfSlope := ini.ReadFloat(Section,
                                     'LowShelfSlope',
                                     t.LowShelfSlope);

    t.HighShelfSlope := ini.ReadFloat(Section,
                                      'HighShelfSlope',
                                      t.HighShelfSlope);

    t.MidMode := TMfMidMode(ini.ReadInteger(Section,
                                            'MidMode',
                                            Ord(t.MidMode)));

    // Peakmeters frequency.
    edPeakMeterFreq.Text := IntToStr(ini.ReadInteger('PeakMeters',
                                                     'Freq',
                                                     100));
  finally

    ini.Free();
  end;

  WriteTuningToUi(t);

  // Remember as current
  FTuning := t;
  FDefaults := Defaults;
  FIniFileName := IniFileName;
  FIniSection := Section;
end;


procedure TfrmEqSettings.LoadDynamicsFromIni(const IniFileName: string;
                                             const Section: string;
                                             const Defaults: TDynamicsSettings);
var
  ini: TIniFile;
  s: TDynamicsSettings;

begin

  s := Defaults;

  if not FileExists(IniFileName) then
    begin

      WriteDynamicsToUi(s);
      Exit;
    end;

  ini := TIniFile.Create(IniFileName);

  try

    s.CompEnabled := ini.ReadBool(Section,
                                  'Dyn_CompEnabled',
                                  s.CompEnabled);

    s.CompThresholdDb := ini.ReadInteger(Section,
                                         'Dyn_CompThresholdDb',
                                         s.CompThresholdDb);

    s.CompRatioX10 := ini.ReadInteger(Section,
                                      'Dyn_CompRatioX10',
                                      s.CompRatioX10);

    s.CompAttackMs := ini.ReadInteger(Section,
                                      'Dyn_CompAttackMs',
                                      s.CompAttackMs);

    s.CompReleaseMs := ini.ReadInteger(Section,
                                       'Dyn_CompReleaseMs',
                                       s.CompReleaseMs);

    s.CompMakeupDb := ini.ReadInteger(Section,
                                      'Dyn_CompMakeupDb',
                                      s.CompMakeupDb);

    s.CompAutoMakeup := ini.ReadBool(Section,
                                     'Dyn_CompAutoMakeup',
                                     s.CompAutoMakeup);

    s.LimEnabled := ini.ReadBool(Section,
                                 'Dyn_LimEnabled',
                                 s.LimEnabled);

    s.LimCeilingDb := ini.ReadInteger(Section,
                                      'Dyn_LimCeilingDb',
                                      s.LimCeilingDb);

    s.LimReleaseMs := ini.ReadInteger(Section,
                                      'Dyn_LimReleaseMs',
                                      s.LimReleaseMs);

    s.LimLookaheadMs := ini.ReadInteger(Section,
                                        'Dyn_LimLookaheadMs',
                                        s.LimLookaheadMs);

    s.LimKneeDb := ini.ReadInteger(Section,
                                   'Dyn_LimKneeDb',
                                   s.LimKneeDb);

    s.LimDetector := TLimiterDetector(ini.ReadInteger(Section,
                                                      'Dyn_LimDetector',
                                                      Ord(s.LimDetector)));

    s.LimRmsWindowMs := ini.ReadInteger(Section,
                                        'Dyn_LimRmsWindowMs',
                                        s.LimRmsWindowMs);

    s.LimTruePeak := ini.ReadBool(Section,
                                  'Dyn_LimTruePeak',
                                  s.LimTruePeak);

    s.LimOversample := ini.ReadInteger(Section,
                                       'Dyn_LimOversample',
                                       s.LimOversample);
  finally

    ini.Free;
  end;

  WriteDynamicsToUi(s);

  FDynSettings := s;
  FDynDefaults := Defaults;
  FIniFileName := IniFileName;
  FIniSection := Section;
end;


procedure TfrmEqSettings.SaveToIni(const IniFileName: string;
                                   const Section: string;
                                   const T: TEqTuning);
var
  ini: TIniFile;

begin

  ini := TIniFile.Create(IniFileName);

  try

    ini.WriteFloat(Section,
                   'LowFreqHz',
                   T.LowFreqHz);

    ini.WriteFloat(Section,
                   'MidFreqHz',
                   T.MidFreqHz);

    ini.WriteFloat(Section,
                   'HighFreqHz',
                   T.HighFreqHz);

    ini.WriteFloat(Section,
                   'MidQ',
                   T.MidQ);

    ini.WriteFloat(Section,
                   'LowShelfSlope',
                   T.LowShelfSlope);

    ini.WriteFloat(Section,
                   'HighShelfSlope',
                   T.HighShelfSlope);

    ini.WriteInteger(Section,
                     'MidMode',
                     Ord(T.MidMode));

    // Peakmeters frequency.
    ini.WriteInteger('PeakMeters',
                     'Freq',
                     StrToIntDef(edPeakMeterFreq.Text,
                                 100));
  finally

    ini.Free();
  end;
end;


procedure TfrmEqSettings.SaveDynamicsToIni(const IniFileName: string;
                                          const Section: string;
                                          const S: TDynamicsSettings);
var
  ini: TIniFile;

begin

  ini := TIniFile.Create(IniFileName);

  try

    ini.WriteBool(Section,
                  'Dyn_CompEnabled',
                  S.CompEnabled);

    ini.WriteInteger(Section,
                     'Dyn_CompThresholdDb',
                     S.CompThresholdDb);

    ini.WriteInteger(Section,
                     'Dyn_CompRatioX10',
                     S.CompRatioX10);

    ini.WriteInteger(Section,
                     'Dyn_CompAttackMs',
                     S.CompAttackMs);

    ini.WriteInteger(Section,
                     'Dyn_CompReleaseMs',
                     S.CompReleaseMs);

    ini.WriteInteger(Section,
                     'Dyn_CompMakeupDb',
                     S.CompMakeupDb);

    ini.WriteBool(Section,
                  'Dyn_CompAutoMakeup',
                  S.CompAutoMakeup);

    ini.WriteBool(Section,
                  'Dyn_LimEnabled',
                  S.LimEnabled);

    ini.WriteInteger(Section,
                     'Dyn_LimCeilingDb',
                     S.LimCeilingDb);

    ini.WriteInteger(Section,
                     'Dyn_LimReleaseMs',
                     S.LimReleaseMs);

    ini.WriteInteger(Section,
                     'Dyn_LimLookaheadMs',
                     S.LimLookaheadMs);

    ini.WriteInteger(Section,
                     'Dyn_LimKneeDb',
                     S.LimKneeDb);

    ini.WriteInteger(Section,
                     'Dyn_LimDetector',
                     Ord(S.LimDetector));

    ini.WriteInteger(Section,
                     'Dyn_LimRmsWindowMs',
                     S.LimRmsWindowMs);

    ini.WriteBool(Section,
                  'Dyn_LimTruePeak',
                  S.LimTruePeak);

    ini.WriteInteger(Section,
                     'Dyn_LimOversample',
                     S.LimOversample);
  finally

    ini.Free();
  end;
end;


procedure TfrmEqSettings.ScheduleApplyDynamics();
begin

  // Throttle rapid UI changes (dragging/updown typing) to avoid spamming the engine
  FApplyDynPending := True;
  if Assigned(FApplyDynTimer) then
    begin

      FApplyDynTimer.Enabled := False;
      FApplyDynTimer.Interval := 75; // ms
      FApplyDynTimer.Enabled := True;
    end;
end;


procedure TfrmEqSettings.ApplyDynamicsNow(const SaveToIni: Boolean);
var
  S: TDynamicsSettings;

begin

  // Read + clamp + reflect to UI
  S := ReadDynamicsFromUi;
  FDynSettings := S;
  WriteDynamicsToUi(FDynSettings);

  if Assigned(FOnApplyDynamics) then
    FOnApplyDynamics(Self,
                     S);
  // DEBUG:
  //OutputDebugString(PChar(Format('UI Apply: LimOversample=%d', [S.LimOversample])));

  if SaveToIni and
     (FIniFileName <> '') and
     (FIniSection <> '') then
    SaveDynamicsToIni(FIniFileName,
                      FIniSection,
                      FDynSettings);
end;


procedure TfrmEqSettings.DynUiChanged(Sender: TObject);
begin

  // Only live-apply when the Dynamics tab is visible (prevents EQ edits from spamming dynamics)
  // If you prefer always-live, remove this guard.
  if Assigned(pcMain) and (pcMain.ActivePage = tsDynamics) then
    ScheduleApplyDynamics();
end;


procedure TfrmEqSettings.edPeakMeterFreqExit(Sender: TObject);
var
  V: Integer;

begin

  V := StrToIntDef(edPeakMeterFreq.Text,
                   100);
  if (V < 10) then
    V := 10
  else
    if (V > 150) then
      V := 150;
  edPeakMeterFreq.Text := IntToStr(V);
end;


procedure TfrmEqSettings.ApplyDynTimerTick(Sender: TObject);
begin

  if Assigned(FApplyDynTimer) then
    FApplyDynTimer.Enabled := False;

  if not FApplyDynPending then
    Exit;

  FApplyDynPending := False;

  // Live apply: do NOT hit disk while user drags controls
  ApplyDynamicsNow(False);
end;

procedure TfrmEqSettings.FormClose(Sender: TObject;
                                   var Action: TCloseAction);
begin

  // Modeless: Hide instead of free, so sliders stay where the user left them
  Action := caHide;
end;

end.
