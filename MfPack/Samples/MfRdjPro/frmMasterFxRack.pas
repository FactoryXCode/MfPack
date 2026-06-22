// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMasterFxRack.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Master effects rack MDI child form.
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
unit frmMasterFxRack;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  {Application}
  RDJ_Common,
  MfWasApiEffectsRack,
  MfLowMidHighEqComponent,
  MfFlangerEchoComponent,
  MfCompressorLimiterComponent,
  MfWasApiFxComponentBase,
  MPxpButton,
  MfTrackBar;

type

  TfrmMasterFxRack = class(TForm)
    pnlCtrls: TPanel;
    btnCompressorLimiter: TMPxpButton;
    btnEQ: TMPxpButton;
    btnFlangerEcho: TMPxpButton;
    pnlEQ: TPanel;
    pnlFlangerEcho: TPanel;
    pnlCompressorLimiter: TPanel;
    lblTitle: TLabel;
    chkEqEnable: TMPxpButton;
    lblHighValue: TLabel;
    lblMidValue: TLabel;
    lblLowValue: TLabel;
    tbLowGainDb: TMfTrackBar;
    tbMidGainDb: TMfTrackBar;
    tbHighGainDb: TMfTrackBar;
    lblHighGainDb: TLabel;
    lblMidGainDb: TLabel;
    lblLowGainDb: TLabel;
    Label1: TLabel;
    lblBaseDelayMs: TLabel;
    lblBaseDelayValue: TLabel;
    tbBaseDelayMs: TMfTrackBar;
    lblDepthMs: TLabel;
    lblDepthValue: TLabel;
    tbDepthMs: TMfTrackBar;
    lblRateHz: TLabel;
    lblRateValue: TLabel;
    tbRateHz: TMfTrackBar;
    lblFeedback: TLabel;
    lblFeedbackValue: TLabel;
    tbFeedback: TMfTrackBar;
    lblWet: TLabel;
    lblWetValue: TLabel;
    tbWet: TMfTrackBar;
    chkFlangerEnable: TMPxpButton;
    btnPresetEcho: TMPxpButton;
    btnPresetFlanger: TMPxpButton;
    Bevel3: TBevel;
    Bevel2: TBevel;
    Bevel4: TBevel;
    lblThresholdDb: TLabel;
    lblRatio: TLabel;
    lblAttackMs: TLabel;
    lblReleaseMs: TLabel;
    lblMakeupDb: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblLimiterCeilingDb: TLabel;
    lblLimiterReleaseMs: TLabel;
    lblLimiterLookaheadMs: TLabel;
    lblTruePeakCeilingDbTP: TLabel;
    lblTruePeakOversample: TLabel;
    lblKneeDb: TLabel;
    tbThresholdDb: TMfTrackBar;
    tbRatio: TMfTrackBar;
    tbAttackMs: TMfTrackBar;
    tbReleaseMs: TMfTrackBar;
    tbMakeupDb: TMfTrackBar;
    tbKneeDb: TMfTrackBar;
    chkAutoMakeup: TMPxpButton;
    chkLimiterEnable: TMPxpButton;
    chkCompEnable: TMPxpButton;
    tbLimiterCeilingDb: TMfTrackBar;
    tbLimiterReleaseMs: TMfTrackBar;
    tbLimiterLookaheadMs: TMfTrackBar;
    chkRmsDetector: TMPxpButton;
    chkTruePeakGuard: TMPxpButton;
    tbTruePeakCeilingDbTP: TMfTrackBar;
    cbTruePeakOversample: TComboBox;
    lblLookaheadValue: TLabel;
    lblThresholdValue: TLabel;
    lblRatioValue: TLabel;
    lblMakeUpValue: TLabel;
    lblAttackValue: TLabel;
    lblCompressorReleaseValue: TLabel;
    lblKneeValue: TLabel;
    lblCeilingValue: TLabel;
    lblLimiterReleaseValue: TLabel;
    lblTPCeilingValue: TLabel;
    btnSetDefaults: TMPxpButton;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    btnMinimize: TMPxpButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEQClick(Sender: TObject);
    procedure btnFlangerEchoClick(Sender: TObject);
    procedure btnCompressorLimiterClick(Sender: TObject);
    procedure tbWetChange(Sender: TObject);
    procedure btnPresetFlangerClick(Sender: TObject);
    procedure btnPresetEchoClick(Sender: TObject);
    procedure chkEqEnableClick(Sender: TObject);
    procedure tbLowGainDbChange(Sender: TObject);
    procedure tbMidGainDbChange(Sender: TObject);
    procedure tbHighGainDbChange(Sender: TObject);
    procedure chkFlangerEnableClick(Sender: TObject);
    procedure tbBaseDelayMsChange(Sender: TObject);
    procedure tbDepthMsChange(Sender: TObject);
    procedure tbRateHzChange(Sender: TObject);
    procedure tbFeedbackChange(Sender: TObject);
    procedure chkCompEnableClick(Sender: TObject);
    procedure tbThresholdDbChange(Sender: TObject);
    procedure tbRatioChange(Sender: TObject);
    procedure tbMakeupDbChange(Sender: TObject);
    procedure tbAttackMsChange(Sender: TObject);
    procedure tbReleaseMsChange(Sender: TObject);
    procedure tbKneeDbChange(Sender: TObject);
    procedure chkAutoMakeupClick(Sender: TObject);
    procedure chkLimiterEnableClick(Sender: TObject);
    procedure tbLimiterCeilingDbChange(Sender: TObject);
    procedure tbLimiterReleaseMsChange(Sender: TObject);
    procedure tbLimiterLookaheadMsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkRmsDetectorClick(Sender: TObject);
    procedure chkTruePeakGuardClick(Sender: TObject);
    procedure tbTruePeakCeilingDbTPChange(Sender: TObject);
    procedure cbTruePeakOversampleChange(Sender: TObject);
    procedure btnSetDefaultsClick(Sender: TObject);
    procedure tbLimiterLookaheadMsChange(Sender: TObject);
    procedure tbHighGainDbDblClick(Sender: TObject);
    procedure tbMidGainDbDblClick(Sender: TObject);
    procedure tbLowGainDbDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private

    FUpdatingGui: Boolean;
    //
    FMasterRack: TMfWasApiEffectsRack;
    // FX High, mid, low
    FMasterEq: TMfLowMidHighEqEffect;
    // FX flanger/echo
    FMasterFlangerEcho: TMfFlangerEchoEffect;
    // FX compressor/limiter
    FMasterCompLim: TMfCompressorLimiterEffect;

    procedure LoadEqFromEffect();
    procedure LoadFlangerFromEffect();
    procedure LoadCompFromEffect();

    procedure SaveEqToEffect();
    procedure SaveFlangerToEffect();
    procedure SaveCompToEffect();

    // Helpers
    // Comp/lim
    function RatioToTrackBar(ARatio: Single): Integer;
    //function TrackBarToRatio(AValue: Integer): Single;


    // EQ
    procedure UpdateGainLabels();
    // Flanger/Echo
    procedure UpdateFlangerEchoValueLabels();
    // Compressor/Limiter
    procedure UpdateCompLimValueLabels();

    // All
    function CanWriteToEffect(const AComp: TMfWasApiFxComponentBase): Boolean;

  public

    procedure BindMasterFx(AMasterRack: TMfWasApiEffectsRack;
                           AMasterEq: TMfLowMidHighEqEffect;
                           AMasterFlangerEcho: TMfFlangerEchoEffect;
                           AMasterCompLim: TMfCompressorLimiterEffect);

    procedure LoadGuiFromEffects();
  end;


implementation

{$R *.dfm}

uses
  frmMainMDI;

procedure TfrmMasterFxRack.FormCreate(Sender: TObject);
begin

  FormStyle := fsMDIChild;
end;


procedure TfrmMasterFxRack.FormDestroy(Sender: TObject);
begin

  FMasterEq := nil;
  FMasterFlangerEcho := nil;
  FMasterCompLim := nil;
  FMasterRack := nil;
end;


procedure TfrmMasterFxRack.FormShow(Sender: TObject);
begin

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);
  //Height := 1538;
 // Width := 722 - 10;
  // Set the MasterEq on top.
  pnlEQ.BringToFront;
end;


// EQ
procedure TfrmMasterFxRack.LoadEqFromEffect();
begin

  if not Assigned(FMasterEq) then
    Exit;

  FUpdatingGui := True;
  try

    chkEqEnable.Checked := FMasterEq.Enabled;
    // Force Fanger/Echo disabled.
    chkFlangerEnable.Checked := False;

    tbLowGainDb.Position := Round(FMasterEq.LowGainDb);
    tbMidGainDb.Position := Round(FMasterEq.MidGainDb);
    tbHighGainDb.Position := Round(FMasterEq.HighGainDb);

    UpdateGainLabels();
  finally
    FUpdatingGui := False;
  end;
end;

// Flanger/Echo
procedure TfrmMasterFxRack.LoadFlangerFromEffect();
begin

  if not Assigned(FMasterFlangerEcho) then
    Exit;

  FUpdatingGui := True;

  try

    chkFlangerEnable.Checked := FMasterFlangerEcho.Enabled;
    tbBaseDelayMs.Position := Round(FMasterFlangerEcho.BaseDelayMs);
    tbDepthMs.Position := Round(FMasterFlangerEcho.DepthMs * 10.0);
    tbRateHz.Position := Round(FMasterFlangerEcho.RateHz * 100.0);
    tbFeedback.Position := Round(FMasterFlangerEcho.Feedback * 100.0);
    tbWet.Position := Round(FMasterFlangerEcho.Wet * 100.0);
    UpdateFlangerEchoValueLabels();
  finally
    FUpdatingGui := False;
  end;
end;


// Compressor/Limiter
procedure TfrmMasterFxRack.LoadCompFromEffect();
begin

  if not Assigned(FMasterCompLim) then
    Exit;

  FUpdatingGui := True;

  try

    // Compressor
    chkCompEnable.Checked := FMasterCompLim.CompEnabled;

    tbThresholdDb.Position := Round(FMasterCompLim.CompThresholdDb);
    tbRatio.Position := RatioToTrackBar(FMasterCompLim.CompRatio);
    tbAttackMs.Position := Round(FMasterCompLim.CompAttackMs);
    tbReleaseMs.Position := Round(FMasterCompLim.CompReleaseMs);
    tbKneeDb.Position := Round(FMasterCompLim.CompKneeDb);
    tbMakeupDb.Position := Round(FMasterCompLim.CompMakeupDb);

    chkAutoMakeup.Checked := FMasterCompLim.CompAutoMakeup;

    // Limiter
    chkLimiterEnable.Checked := FMasterCompLim.LimEnabled;
    tbLimiterCeilingDb.Position := Round(FMasterCompLim.LimCeilingDb);
    tbLimiterReleaseMs.Position := Round(FMasterCompLim.LimReleaseMs);
    tbLimiterLookaheadMs.Position := Round(FMasterCompLim.LimLookaheadMs);

    chkRmsDetector.Checked := FMasterCompLim.RmsDetector;
    chkTruePeakGuard.Checked := FMasterCompLim.TruePeakGuard;
    tbTruePeakCeilingDbTP.Position := Round(FMasterCompLim.TruePeakCeilingDbTP * 10.0);

    case FMasterCompLim.TruePeakOversample of
      2: cbTruePeakOversample.ItemIndex := 0;
      4: cbTruePeakOversample.ItemIndex := 1;
      8: cbTruePeakOversample.ItemIndex := 2;
    else
      cbTruePeakOversample.ItemIndex := 1;
    end;

  finally

    UpdateCompLimValueLabels();
    FUpdatingGui := False;
  end;
end;


procedure TfrmMasterFxRack.SaveEqToEffect();
begin

  if not CanWriteToEffect(FMasterEq) then
    Exit;

  FMasterEq.Enabled := chkEqEnable.Checked;

  FMasterEq.LowGainDb := tbLowGainDb.Position;
  FMasterEq.MidGainDb := tbMidGainDb.Position;
  FMasterEq.HighGainDb := tbHighGainDb.Position;
  UpdateGainLabels();
end;


procedure TfrmMasterFxRack.SaveFlangerToEffect();
begin

  if not CanWriteToEffect(FMasterFlangerEcho) then
    Exit;

  FMasterFlangerEcho.Enabled := chkFlangerEnable.Checked;

  FMasterFlangerEcho.BaseDelayMs := tbBaseDelayMs.Position;
  FMasterFlangerEcho.DepthMs := tbDepthMs.Position / 10.0;
  FMasterFlangerEcho.RateHz := tbRateHz.Position / 100.0;
  FMasterFlangerEcho.Feedback := tbFeedback.Position / 100.0;
  FMasterFlangerEcho.Wet := tbWet.Position / 100.0;

  UpdateFlangerEchoValueLabels();
end;


procedure TfrmMasterFxRack.SaveCompToEffect();
begin

  if not CanWriteToEffect(FMasterCompLim) then
    Exit;

  FMasterCompLim.Enabled := chkCompEnable.Checked;

  FMasterCompLim.CompThresholdDb := tbThresholdDb.Position;
  FMasterCompLim.CompRatio := tbRatio.Position / 10.0;
  FMasterCompLim.CompMakeupDb := tbMakeUpDb.Position;
  FMasterCompLim.CompAttackMs := tbAttackMs.Position;
  FMasterCompLim.CompReleaseMs := tbReleaseMs.Position;
  FMasterCompLim.CompKneeDb := tbKneeDb.Position;
  FMasterCompLim.CompAutoMakeup := chkAutoMakeup.Checked;
  FMasterCompLim.LimLookaheadMs := tbLimiterLookaheadMs.Position;

  FMasterCompLim.RmsDetector := chkRmsDetector.Checked;
  FMasterCompLim.TruePeakGuard := chkTruePeakGuard.Checked;
  FMasterCompLim.TruePeakCeilingDbTP := tbTruePeakCeilingDbTP.Position / 10.0;

  case cbTruePeakOversample.ItemIndex of
    0: FMasterCompLim.TruePeakOversample := 2;
    1: FMasterCompLim.TruePeakOversample := 4;
    2: FMasterCompLim.TruePeakOversample := 8;
    else
      FMasterCompLim.TruePeakOversample := 2;
  end;

  UpdateCompLimValueLabels();
end;


procedure TfrmMasterFxRack.BindMasterFx(AMasterRack: TMfWasApiEffectsRack;
                                        AMasterEq: TMfLowMidHighEqEffect;
                                        AMasterFlangerEcho: TMfFlangerEchoEffect;
                                        AMasterCompLim: TMfCompressorLimiterEffect);
begin

  FMasterRack := AMasterRack;
  FMasterEq := AMasterEq;
  FMasterFlangerEcho := AMasterFlangerEcho;
  FMasterCompLim := AMasterCompLim;

  LoadGuiFromEffects();
end;


procedure TfrmMasterFxRack.LoadGuiFromEffects();
begin

  FUpdatingGui := True;

  try

    if Assigned(FMasterEq) then
      LoadEqFromEffect();

    if Assigned(FMasterFlangerEcho) then
      LoadFlangerFromEffect();

    if Assigned(FMasterCompLim) then
      LoadCompFromEffect();
  finally

    FUpdatingGui := False;
  end;
end;


// NEW =========================================================================

// EQ --------------------------------------------------------------------------

procedure TfrmMasterFxRack.btnEQClick(Sender: TObject);
begin
  //
  pnlEQ.BringToFront;
end;


procedure TfrmMasterFxRack.chkEqEnableClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterEq) then
    Exit;

  FMasterEq.Enabled := chkEqEnable.Checked;
end;


procedure TfrmMasterFxRack.tbHighGainDbChange(Sender: TObject);
begin

  SaveEqToEffect();
end;


procedure TfrmMasterFxRack.tbHighGainDbDblClick(Sender: TObject);
begin

  tbHighGainDb.AnimateTrackBarToPosition(0,
                                         2);
end;


procedure TfrmMasterFxRack.tbMidGainDbChange(Sender: TObject);
begin

  SaveEqToEffect();
end;


procedure TfrmMasterFxRack.tbMidGainDbDblClick(Sender: TObject);
begin

  tbMidGainDb.AnimateTrackBarToPosition(0,
                                        2);
end;


procedure TfrmMasterFxRack.tbLowGainDbChange(Sender: TObject);
begin

  SaveEqToEffect();
end;


procedure TfrmMasterFxRack.tbLowGainDbDblClick(Sender: TObject);
begin

  tbLowGainDb.AnimateTrackBarToPosition(0,
                                        2);
end;


procedure TfrmMasterFxRack.UpdateGainLabels();
begin

  lblLowValue.Caption := FormatFloat('0 dB',
                                     tbLowGainDb.Position);
  lblMidValue.Caption := FormatFloat('0 dB',
                                     tbMidGainDb.Position);
  lblHighValue.Caption := FormatFloat('0 dB',
                                      tbHighGainDb.Position);
end;

// EQ end ----------------------------------------------------------------------


// Flanger/Echo ----------------------------------------------------------------

procedure TfrmMasterFxRack.btnFlangerEchoClick(Sender: TObject);
begin
  //
  pnlFlangerEcho.BringToFront;
end;


procedure TfrmMasterFxRack.FormResize(Sender: TObject);
begin

  if Assigned(MainMDIFrm) then
    MainMDIFrm.AlignMasterDeckWithFxRack();
end;


procedure TfrmMasterFxRack.btnMinimizeClick(Sender: TObject);
begin

  MainMDIFrm.btnEffects.Click;
end;


procedure TfrmMasterFxRack.chkFlangerEnableClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterFlangerEcho) then
    Exit;

  SaveFlangerToEffect();
end;

// Sliders

procedure TfrmMasterFxRack.tbBaseDelayMsChange(Sender: TObject);
begin

  SaveFlangerToEffect();
end;


procedure TfrmMasterFxRack.tbDepthMsChange(Sender: TObject);
begin

  SaveFlangerToEffect();
end;


procedure TfrmMasterFxRack.tbRateHzChange(Sender: TObject);
begin

  SaveFlangerToEffect();
end;


procedure TfrmMasterFxRack.tbFeedbackChange(Sender: TObject);
begin

  SaveFlangerToEffect();
end;


procedure TfrmMasterFxRack.tbWetChange(Sender: TObject);
begin

  SaveFlangerToEffect();
end;
// End sliders


procedure TfrmMasterFxRack.btnPresetFlangerClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterFlangerEcho) then
    Exit;

  if not FMasterFlangerEcho.Enabled then
    Exit;

  FMasterFlangerEcho.BaseDelayMs := 3.0;
  FMasterFlangerEcho.DepthMs := 2.0;
  FMasterFlangerEcho.RateHz := 0.25;
  FMasterFlangerEcho.Feedback := 0.20;
  FMasterFlangerEcho.Wet := 0.35;

  LoadFlangerFromEffect();
end;


procedure TfrmMasterFxRack.btnPresetEchoClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterFlangerEcho) then
    Exit;

  if not FMasterFlangerEcho.Enabled then
    Exit;

  FMasterFlangerEcho.BaseDelayMs := 250.0;
  FMasterFlangerEcho.DepthMs := 0.0;
  FMasterFlangerEcho.RateHz := 0.0;
  FMasterFlangerEcho.Feedback := 0.35;
  FMasterFlangerEcho.Wet := 0.35;

  LoadFlangerFromEffect();
end;


procedure TfrmMasterFxRack.UpdateFlangerEchoValueLabels();
begin

  lblBaseDelayValue.Caption := Format('%d ms',
                                      [tbBaseDelayMs.Position]);

  lblDepthValue.Caption := Format('%.1f ms',
                                  [tbDepthMs.Position / 10.0]);

  lblRateValue.Caption := Format('%.2f Hz',
                                 [tbRateHz.Position / 100.0]);

  lblFeedbackValue.Caption := Format('%d %%',
                                     [tbFeedback.Position]);

  lblWetValue.Caption := Format('%d %%',
                                [tbWet.Position]);
end;

// Flanger/Echo end ------------------------------------------------------------


// Compressor ------------------------------------------------------------------

procedure TfrmMasterFxRack.btnCompressorLimiterClick(Sender: TObject);
begin
  //
  pnlCompressorLimiter.BringToFront;
end;


procedure TfrmMasterFxRack.chkCompEnableClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterCompLim) then
    Exit;

  SaveCompToEffect();
end;

// Sliders ------------
procedure TfrmMasterFxRack.tbThresholdDbChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbRatioChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbMakeupDbChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbAttackMsChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbReleaseMsChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbKneeDbChange(Sender: TObject);
begin

  SaveCompToEffect();
end;
// Sliders end ---------------------

procedure TfrmMasterFxRack.chkAutoMakeupClick(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.UpdateCompLimValueLabels();
begin

  lblThresholdValue.Caption := Format('%d ms',
                                      [tbThresholdDb.Position]);

  lblRatioValue.Caption := Format('%d ms',
                                  [tbRatio.Position]);

  lblMakeUpValue.Caption := Format('%d ms',
                                   [tbMakeupDb.Position]);

  lblAttackValue.Caption := Format('%d ms',
                                   [tbAttackMs.Position]);

  lblCompressorReleaseValue.Caption := Format('%d ms',
                                              [tbReleaseMs.Position]);

  lblKneeValue.Caption := Format('%d dB',
                                 [tbKneeDb.Position]);


  lblCeilingValue.Caption := Format('%d dB',
                                    [tbLimiterCeilingDb.Position]);

  lblLimiterReleaseValue.Caption := Format('%d dB',
                                           [tbLimiterCeilingDb.Position]);

  lblLookaheadValue.Caption := Format('%d ms',
                                      [tbLimiterLookaheadMs.Position]);


  lblTPCeilingValue.Caption := Format('%d dB',
                                      [tbTruePeakCeilingDbTP.Position]);
end;

// Compressor end --------------------------------------------------------------


// Limiter ---------------------------------------------------------------------

procedure TfrmMasterFxRack.chkLimiterEnableClick(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterCompLim) then
    Exit;

  FMasterCompLim.LimEnabled := chkLimiterEnable.Checked;
end;


procedure TfrmMasterFxRack.tbLimiterCeilingDbChange(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterCompLim) then
    Exit;

  FMasterCompLim.LimCeilingDb := tbLimiterCeilingDb.Position;
end;


procedure TfrmMasterFxRack.tbLimiterReleaseMsChange(Sender: TObject);
begin

  if not CanWriteToEffect(FMasterCompLim) then
    Exit;

  FMasterCompLim.LimReleaseMs := tbLimiterReleaseMs.Position;
end;


procedure TfrmMasterFxRack.tbLimiterLookaheadMsChange(Sender: TObject);
begin

  lblLookaheadValue.Caption := Format('%d ms', [tbLimiterLookaheadMs.Position]);
end;

procedure TfrmMasterFxRack.tbLimiterLookaheadMsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  SaveCompToEffect();
end;

// Limiter end -----------------------------------------------------------------

// Detector / True Peak --------------------------------------------------------

procedure TfrmMasterFxRack.chkRmsDetectorClick(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.chkTruePeakGuardClick(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.tbTruePeakCeilingDbTPChange(Sender: TObject);
begin

  SaveCompToEffect();
end;


procedure TfrmMasterFxRack.cbTruePeakOversampleChange(Sender: TObject);
begin

  SaveCompToEffect();
end;

// Detector / True Peak end ----------------------------------------------------

// Set all compressor/limiter values to default.
procedure TfrmMasterFxRack.btnSetDefaultsClick(Sender: TObject);
begin

  chkCompEnable.Checked := True;
  tbThresholdDb.Position := -18;
  tbRatio.Position := 20; // 2.0:1
  tbAttackMs.Position := 20;
  tbReleaseMs.Position := 150;
  tbKneeDb.Position := 6;
  tbMakeupDb.Position := 0;
  chkAutoMakeup.Checked := True;

  chkLimiterEnable.Checked := True;
  tbLimiterCeilingDb.Position := -1;
  tbLimiterReleaseMs.Position := 80;
  tbLimiterLookaheadMs.Position := 3;

  chkRmsDetector.Checked := True;
  chkTruePeakGuard.Checked := False;
  tbTruePeakCeilingDbTP.Position := -10; // -1.0 dBTP
  cbTruePeakOversample.ItemIndex := 1; // 4x

  UpdateCompLimValueLabels();
end;


// HELPERS =====================================================================


// Comp/lim --------------------------------------------------------------------

function TfrmMasterFxRack.RatioToTrackBar(ARatio: Single): Integer;
begin

  if (ARatio < 1.0) then
    ARatio := 1.0;

  Result := Round(ARatio * 10.0);
end;

{ TODO: is it usefull?
function TfrmMasterFxRack.TrackBarToRatio(AValue: Integer): Single;
begin

  Result := AValue / 10.0;
  if (Result < 1.0) then
    Result := 1.0;
end;
}

// Comp/lim end ----------------------------------------------------------------

// All
function TfrmMasterFxRack.CanWriteToEffect(const AComp: TMfWasApiFxComponentBase): Boolean;
begin

  Result := False;

  if FUpdatingGui then
    Exit;

  if not Assigned(AComp) then
    Exit;

  Result := True;
end;

end.
