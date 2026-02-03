// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfParametricEqComponent.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Compressor + limiter MFT with GR meters.
//              Includes smoothed true-peak limiter using Catmull-Rom oversampling (2×/4×/8×),
//              ceiling default -1.0 dBTP, and attack/release smoothing to avoid pumping.
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
// Related objects: MfParametricEqMFT
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
unit MfParametricEqComponent;

interface

uses

  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfWasApiFxComponentBase,
  MfWasApiFxIntf,
  MfAudioEffectMFTBase,
  MfParametricEqMFT;

type

  TMfParametricEqEffect = class(TMfWasApiFxComponentBase)
  private

    FMft: IMFTransform;
    FCtl: IMfParametricEqControl;
    FIns: IMfParametricEqInspect;

    FEnabled: Boolean;
    FGainDb: Single;
    FCenterFreqHz: Single;
    FQ: Single;
    FBandwidthOctaves: Single;

    FRampMode: TMfRampMode;
    FRampTimeMs: Integer;

    FTruePeakGuard: Boolean;
    FTruePeakCeilingDbTP: Single;
    FTruePeakOversample: Integer;

    procedure PushAllToMft();

    procedure SetEnabled(Value: Boolean);
    procedure SetGainDb(Value: Single);
    procedure SetCenterFreqHz(Value: Single);
    procedure SetQ(Value: Single);
    procedure SetBandwidthOctaves(Value: Single);

    procedure SetRampMode(Value: TMfRampMode);
    procedure SetRampTimeMs(Value: Integer);

    procedure SetTruePeakGuard(Value: Boolean);
    procedure SetTruePeakCeilingDbTP(Value: Single);
    procedure SetTruePeakOversample(Value: Integer);

  protected

    procedure CheckForMft(); override;
    function GetMftInstance(): IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); override;

    // Optional inspection (runtime only)
    function GetCurrentCoeffs(out C: TBiquadCoeffs;
                              out SampleRate: Double): Boolean;

    function GetTargetCoeffs(out C: TBiquadCoeffs;
                             out SampleRate: Double): Boolean;

  published

    property Enabled: Boolean read FEnabled write SetEnabled default True;

    property GainDb: Single read FGainDb write SetGainDb;
    property CenterFreqHz: Single read FCenterFreqHz write SetCenterFreqHz;
    property Q: Single read FQ write SetQ;
    property BandwidthOctaves: Single read FBandwidthOctaves write SetBandwidthOctaves;

    property RampMode: TMfRampMode read FRampMode write SetRampMode default rmSmooth;
    property RampTimeMs: Integer read FRampTimeMs write SetRampTimeMs default 30;

    property TruePeakGuard: Boolean read FTruePeakGuard write SetTruePeakGuard default False;
    property TruePeakCeilingDbTP: Single read FTruePeakCeilingDbTP write SetTruePeakCeilingDbTP;
    property TruePeakOversample: Integer read FTruePeakOversample write SetTruePeakOversample;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfParametricEqEffect]);
end;


constructor TMfParametricEqEffect.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // Defaults (match your MFT defaults)
  FEnabled := True;
  FGainDb := 0;
  FCenterFreqHz := 1000;
  FQ := 1.0;
  FBandwidthOctaves := 1.0;

  FRampMode := rmSmooth;
  FRampTimeMs := 30;

  FTruePeakGuard := False;
  FTruePeakCeilingDbTP := -1.0;
  FTruePeakOversample := 4;

  if not (csDesigning in ComponentState) then
    CheckForMft();
end;


procedure TMfParametricEqEffect.CheckForMft();
begin

  if (FMft <> nil) then
    Exit;

  FMft := TMfParametricEqMFT.Create as IMFTransform;
  FCtl := FMft as IMfParametricEqControl;
  FIns := FMft as IMfParametricEqInspect;

  PushAllToMft();
end;


function TMfParametricEqEffect.GetMftInstance: IMFTransform;
begin

  Result := FMft;
end;


procedure TMfParametricEqEffect.PushAllToMft();
begin

  if (FCtl = nil) then
    Exit;

  FCtl.EnableEQ(FEnabled);

  // If BW>0 it drives Q internally; keep your rule.
  FCtl.SetGainDb(FGainDb);
  FCtl.SetCenterFreqHz(FCenterFreqHz);

  if (FBandwidthOctaves > 0) then
    FCtl.SetBandwidthOctaves(FBandwidthOctaves)
  else
    FCtl.SetQ(FQ);

  FCtl.SetRampMode(FRampMode);
  FCtl.SetRampTimeMs(FRampTimeMs);

  FCtl.EnableTruePeakGuard(FTruePeakGuard);
  FCtl.SetTruePeakCeilingDbTP(FTruePeakCeilingDbTP);
  FCtl.SetTruePeakOversample(FTruePeakOversample);
end;


procedure TMfParametricEqEffect.SetEnabled(Value: Boolean);
begin

  FEnabled := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.EnableEQ(Value);
end;


procedure TMfParametricEqEffect.SetGainDb(Value: Single);
begin

  FGainDb := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetGainDb(Value);
end;


procedure TMfParametricEqEffect.SetCenterFreqHz(Value: Single);
begin

  FCenterFreqHz := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetCenterFreqHz(Value);
end;


procedure TMfParametricEqEffect.SetQ(Value: Single);
begin

  FQ := Value;
  // if user sets Q, we want BW to stop driving
  if (Value <> 0) then
    FBandwidthOctaves := 0;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetQ(Value);
end;


procedure TMfParametricEqEffect.SetBandwidthOctaves(Value: Single);
begin

  FBandwidthOctaves := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetBandwidthOctaves(Value);
end;


procedure TMfParametricEqEffect.SetRampMode(Value: TMfRampMode);
begin

  FRampMode := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetRampMode(Value);
end;


procedure TMfParametricEqEffect.SetRampTimeMs(Value: Integer);
begin

  FRampTimeMs := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetRampTimeMs(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakGuard(Value: Boolean);
begin

  FTruePeakGuard := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.EnableTruePeakGuard(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakCeilingDbTP(Value: Single);
begin

  FTruePeakCeilingDbTP := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetTruePeakCeilingDbTP(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakOversample(Value: Integer);
begin

  FTruePeakOversample := Value;

  if (csDesigning in ComponentState) then
    Exit;

  CheckForMft();
  FCtl.SetTruePeakOversample(Value);
end;


function TMfParametricEqEffect.GetCurrentCoeffs(out C: TBiquadCoeffs;
                                                out SampleRate: Double): Boolean;
begin

  if (csDesigning in ComponentState) then
    Exit(False);

  CheckForMft();
  Result := Assigned(FIns) and FIns.GetCurrentCoeffs(C,
                                                     SampleRate);
end;


function TMfParametricEqEffect.GetTargetCoeffs(out C: TBiquadCoeffs;
                                               out SampleRate: Double): Boolean;
begin

  if (csDesigning in ComponentState) then
    Exit(False);

  CheckForMft();
  Result := Assigned(FIns) and FIns.GetTargetCoeffs(C, SampleRate);
end;


end.

