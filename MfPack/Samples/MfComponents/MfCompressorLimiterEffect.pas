// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCompressorLimiterEffect.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.9
// Description:
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
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
unit MfCompressorLimiterEffect;

interface

uses

  {System}
  System.Classes,
  System.SysUtils,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfWasApiFxComponentBase,
  MfCompressorLimiterMFT;

type

  // Component wrapper for TMfCompressorLimiterMFT.
  TMfCompressorLimiterEffect = class(TMfWasApiFxComponentBase)
  private

    FMft: IMFTransform;
    FCtl: IMfCompressorLimiterControl;
    FIns: IMfCompressorLimiterInspect;

    FEnabled: Boolean;
    FSettings: TDynamicsSettings;

    // True-peak (final safety limiter)
    FTruePeakGuard: Boolean;
    FTruePeakCeilingDbTP: Single;  // default -1.0
    FTruePeakOversample: Integer;  // 2/4/8, default 4

    procedure EnsureCtl();
    procedure PushAll();

    // Published property setters (write-through to MFT)
    procedure SetEnabled(Value: Boolean);

    // Compressor
    procedure SetCompEnabled(Value: Boolean);
    procedure SetCompThresholdDb(Value: Single);
    procedure SetCompRatio(Value: Single);
    procedure SetCompAttackMs(Value: Single);
    procedure SetCompReleaseMs(Value: Single);
    procedure SetCompKneeDb(Value: Single);
    procedure SetCompMakeupDb(Value: Single);
    procedure SetCompAutoMakeup(Value: Boolean);

    // Limiter
    procedure SetLimEnabled(Value: Boolean);
    procedure SetLimCeilingDb(Value: Single);
    procedure SetLimReleaseMs(Value: Single);
    procedure SetLimLookaheadMs(Value: Single);

    // Detector
    procedure SetRmsDetector(Value: Boolean);

    // True-peak
    procedure SetTruePeakGuard(Value: Boolean);
    procedure SetTruePeakCeilingDbTP(Value: Single);
    procedure SetTruePeakOversample(Value: Integer);

    procedure ApplySettingsToMft();

  protected

    procedure EnsureMft(); override;
    function GetMftInstance(): IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); override;

    // Meters (poll from GUI timer)
    function CompressorGRdB(): Single;
    function LimiterGRdB(): Single;

    // Optional: snapshot settings (handy for persistence).
    procedure GetSettings(out S: TDynamicsSettings);

  published

    property Enabled: Boolean read FEnabled write SetEnabled default True;

    // ---------------- Compressor ---------------------------------------------
    property CompEnabled: Boolean read FSettings.CompEnabled write SetCompEnabled default True;
    property CompThresholdDb: Single read FSettings.CompThresholdDb write SetCompThresholdDb; // -60..0
    property CompRatio: Single read FSettings.CompRatio write SetCompRatio;                   // 1..50
    property CompAttackMs: Single read FSettings.CompAttackMs write SetCompAttackMs;          // 0.1..500
    property CompReleaseMs: Single read FSettings.CompReleaseMs write SetCompReleaseMs;       // 1..5000
    property CompKneeDb: Single read FSettings.CompKneeDb write SetCompKneeDb;                // 0..24
    property CompMakeupDb: Single read FSettings.CompMakeupDb write SetCompMakeupDb;          // 0..24
    property CompAutoMakeup: Boolean read FSettings.CompAutoMakeup write SetCompAutoMakeup default True;

    // ---------------- Limiter ------------------------------------------------
    property LimEnabled: Boolean read FSettings.LimEnabled write SetLimEnabled default True;
    property LimCeilingDb: Single read FSettings.LimCeilingDb write SetLimCeilingDb;         // -24..0 (dBFS)
    property LimReleaseMs: Single read FSettings.LimReleaseMs write SetLimReleaseMs;          // 1..5000
    property LimLookaheadMs: Single read FSettings.LimLookaheadMs write SetLimLookaheadMs;    // 0..20 (keep low)

    // ---------------- Detector -----------------------------------------------
    property RmsDetector: Boolean read FSettings.RmsDetector write SetRmsDetector default True;

    // ---------------- True-peak safety limiter -------------------------------
    property TruePeakGuard: Boolean read FTruePeakGuard write SetTruePeakGuard default False;
    property TruePeakCeilingDbTP: Single read FTruePeakCeilingDbTP write SetTruePeakCeilingDbTP; // default -1.0
    property TruePeakOversample: Integer read FTruePeakOversample write SetTruePeakOversample;   // 2/4/8
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfCompressorLimiterEffect]);
end;

{ TMfCompressorLimiterEffect }

constructor TMfCompressorLimiterEffect.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FEnabled := True;

  // Defaults (match the MFT defaults / Sample-4-ish)
  FillChar(FSettings,
           SizeOf(FSettings),
           0);

  FSettings.CompEnabled := True;
  FSettings.CompThresholdDb := -18;
  FSettings.CompRatio := 3.0;
  FSettings.CompAttackMs := 10;
  FSettings.CompReleaseMs := 120;
  FSettings.CompKneeDb := 6;
  FSettings.CompMakeupDb := 0;
  FSettings.CompAutoMakeup := True;

  FSettings.LimEnabled := True;
  FSettings.LimCeilingDb := -1.0;
  FSettings.LimReleaseMs := 80;
  FSettings.LimLookaheadMs := 5;

  FSettings.RmsDetector := True;

  // True-peak defaults required by you
  FTruePeakGuard := False;
  FTruePeakCeilingDbTP := -1.0;
  FTruePeakOversample := 4;

  if not (csDesigning in ComponentState) then
    EnsureMft();
end;


procedure TMfCompressorLimiterEffect.EnsureMft();
begin

  if (FMft <> nil) then
    Exit;

  // Create MFT instance
  FMft := TMfCompressorLimiterMFT.Create as IMFTransform;

  // Grab control/inspect interfaces
  FCtl := FMft as IMfCompressorLimiterControl;
  FIns := FMft as IMfCompressorLimiterInspect;

  PushAll;
end;


procedure TMfCompressorLimiterEffect.EnsureCtl();
begin

  if (csDesigning in ComponentState) then
    Exit;

  EnsureMft();
end;


function TMfCompressorLimiterEffect.GetMftInstance(): IMFTransform;
begin

  Result := FMft;
end;


procedure TMfCompressorLimiterEffect.PushAll();
begin

  if (FCtl = nil) then
    Exit;

  FCtl.EnableFX(FEnabled);
  FCtl.SetSettings(FSettings);

  FCtl.EnableTruePeakGuard(FTruePeakGuard);
  FCtl.SetTruePeakCeilingDbTP(FTruePeakCeilingDbTP);
  FCtl.SetTruePeakOversample(FTruePeakOversample);
end;


procedure TMfCompressorLimiterEffect.ApplySettingsToMft;
begin

  if (csDesigning in ComponentState) then
    Exit;

  EnsureCtl();
  FCtl.SetSettings(FSettings);
end;


procedure TMfCompressorLimiterEffect.SetEnabled(Value: Boolean);
begin

  FEnabled := Value;

  if (csDesigning in ComponentState) then
    Exit;

  EnsureCtl();
  FCtl.EnableFX(Value);
end;


// ---------------- Compressor setters ----------------

procedure TMfCompressorLimiterEffect.SetCompEnabled(Value: Boolean);
begin

  FSettings.CompEnabled := Value;
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompThresholdDb(Value: Single);
begin

  FSettings.CompThresholdDb := EnsureRange(Value,
                                           -60.0,
                                           0.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompRatio(Value: Single);
begin

  FSettings.CompRatio := EnsureRange(Value,
                                     1.0,
                                     50.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompAttackMs(Value: Single);
begin

  FSettings.CompAttackMs := EnsureRange(Value,
                                        0.1,
                                        500.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompReleaseMs(Value: Single);
begin

  FSettings.CompReleaseMs := EnsureRange(Value,
                                         1.0,
                                         5000.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompKneeDb(Value: Single);
begin

  FSettings.CompKneeDb := EnsureRange(Value,
                                      0.0,
                                      24.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompMakeupDb(Value: Single);
begin

  FSettings.CompMakeupDb := EnsureRange(Value,
                                        0.0,
                                        24.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetCompAutoMakeup(Value: Boolean);
begin

  FSettings.CompAutoMakeup := Value;
  ApplySettingsToMft();
end;


// ---------------- Limiter setters ----------------

procedure TMfCompressorLimiterEffect.SetLimEnabled(Value: Boolean);
begin

  FSettings.LimEnabled := Value;
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetLimCeilingDb(Value: Single);
begin

  // dBFS ceiling for limiter (not dBTP)
  FSettings.LimCeilingDb := EnsureRange(Value,
                                        -24.0,
                                        0.0);
  ApplySettingsToMft();
end;


procedure TMfCompressorLimiterEffect.SetLimReleaseMs(Value: Single);
begin

  FSettings.LimReleaseMs := EnsureRange(Value,
                                        1.0,
                                        5000.0);
  ApplySettingsToMft;
end;


procedure TMfCompressorLimiterEffect.SetLimLookaheadMs(Value: Single);
begin

  // Keep sane; user earlier accepted 0..20ms as typical.
  FSettings.LimLookaheadMs := EnsureRange(Value,
                                          0.0,
                                          20.0);
  ApplySettingsToMft();
end;


// ---------------- Detector setters ----------------

procedure TMfCompressorLimiterEffect.SetRmsDetector(Value: Boolean);
begin

  FSettings.RmsDetector := Value;
  ApplySettingsToMft();
end;

// ---------------- True-peak setters ----------------

procedure TMfCompressorLimiterEffect.SetTruePeakGuard(Value: Boolean);
begin

  FTruePeakGuard := Value;

  if (csDesigning in ComponentState) then
    Exit;

  EnsureCtl();
  FCtl.EnableTruePeakGuard(Value);
end;


procedure TMfCompressorLimiterEffect.SetTruePeakCeilingDbTP(Value: Single);
begin

  // Typically -1.0 dBTP
  FTruePeakCeilingDbTP := EnsureRange(Value,
                                      -24.0,
                                      0.0);

  if (csDesigning in ComponentState) then
    Exit;

  EnsureCtl();
  FCtl.SetTruePeakCeilingDbTP(FTruePeakCeilingDbTP);
end;


procedure TMfCompressorLimiterEffect.SetTruePeakOversample(Value: Integer);
begin

  case Value of
    2, 4, 8: FTruePeakOversample := Value;
  else
    FTruePeakOversample := 4;
  end;

  if (csDesigning in ComponentState) then
    Exit;

  EnsureCtl();
  FCtl.SetTruePeakOversample(FTruePeakOversample);
end;


// ---------------- Public helpers ---------------------------------------------

function TMfCompressorLimiterEffect.CompressorGRdB(): Single;
begin

  if (csDesigning in ComponentState) or (FIns = nil) then
    Exit(0.0);

  Result := FIns.GetCompressorGRdB();
end;


function TMfCompressorLimiterEffect.LimiterGRdB(): Single;
begin

  if (csDesigning in ComponentState) or (FIns = nil) then
    Exit(0.0);

  Result := FIns.GetLimiterGRdB();
end;


procedure TMfCompressorLimiterEffect.GetSettings(out S: TDynamicsSettings);
begin

  S := FSettings;
end;

end.

