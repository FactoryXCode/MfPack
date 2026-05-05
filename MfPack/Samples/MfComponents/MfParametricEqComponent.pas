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
// Revision Version: 3.2.0
// Description: Compressor + limiter MFT with GR meters.
//              Includes smoothed true-peak limiter using Catmull-Rom oversampling (2×/4×/8×),
//              ceiling default -1.0 dBTP, and attack/release smoothing to avoid pumping.
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
//
// Related objects: MfParametricEqMFT
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
//         https://github.com/BillyDM/awesome-audio-dsp/blob/main/sections/DSP_COOKBOOKS.md
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

  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
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
    FMftObj: TMfParametricEqMFT;

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

    constructor Create(AOwner: TComponent); //override;
    procedure AfterConstruction(); override;
    destructor Destroy(); override;

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


  OutputDebugString(PChar(Format('TMfParametricEqEffect.Create Self=%p',
                                 [Pointer(Self)])));


  // Defaults (match your MFT defaults)
  FEnabled := True;
  FGainDb := 0;
  FCenterFreqHz := 1500;
  FQ := 1.0;
  FBandwidthOctaves := 1.0;

  FRampMode := rmSmooth;
  FRampTimeMs := 30;

  FTruePeakGuard := False;
  FTruePeakCeilingDbTP := -1.0;
  FTruePeakOversample := 4;
  // Do NOT create any MFT by default, this creates memory leaks!
  // Yuo have to do that, when the object is fully created.
  // For that create the MFT in method AfterConstruction().
  // CheckForMft();
end;


procedure TMfParametricEqEffect.AfterConstruction();
begin

  inherited;

  CheckForMft();
end;


destructor TMfParametricEqEffect.Destroy();
begin

  // Release interface views first.
  FIns := nil;
  FCtl := nil;
  FMft := nil;

  // THEN free the actual object
  FreeAndNil(FMftObj);

  inherited;
end;


procedure TMfParametricEqEffect.CheckForMft();
begin

  if (FMftObj <> nil) then
    Exit;

  FMftObj := TMfParametricEqMFT.Create;
  FMft := FMftObj as IMFTransform;
  FCtl := FMftObj as IMfParametricEqControl;
  FIns := FMftObj as IMfParametricEqInspect;

  PushAllToMft(); // Set settings
end;


function TMfParametricEqEffect.GetMftInstance(): IMFTransform;
begin

  Result := FMft;
end;


procedure TMfParametricEqEffect.PushAllToMft();
begin

  if IsDesigning() then
    Exit;

  if (FMft = nil) then
    Exit;

  FCtl.EnableEQ(FEnabled);

  // If BW > 0 it drives Q internally; keep your rule.
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

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.EnableEQ(Value);
end;


procedure TMfParametricEqEffect.SetGainDb(Value: Single);
begin

  FGainDb := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetGainDb(Value);
end;


procedure TMfParametricEqEffect.SetCenterFreqHz(Value: Single);
begin

  FCenterFreqHz := Value;

  if IsDesigning() then
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

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetQ(Value);
end;


procedure TMfParametricEqEffect.SetBandwidthOctaves(Value: Single);
begin

  FBandwidthOctaves := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetBandwidthOctaves(Value);
end;


procedure TMfParametricEqEffect.SetRampMode(Value: TMfRampMode);
begin

  FRampMode := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetRampMode(Value);
end;


procedure TMfParametricEqEffect.SetRampTimeMs(Value: Integer);
begin

  FRampTimeMs := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetRampTimeMs(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakGuard(Value: Boolean);
begin

  FTruePeakGuard := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.EnableTruePeakGuard(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakCeilingDbTP(Value: Single);
begin

  FTruePeakCeilingDbTP := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetTruePeakCeilingDbTP(Value);
end;


procedure TMfParametricEqEffect.SetTruePeakOversample(Value: Integer);
begin

  FTruePeakOversample := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetTruePeakOversample(Value);
end;


function TMfParametricEqEffect.GetCurrentCoeffs(out C: TBiquadCoeffs;
                                                out SampleRate: Double): Boolean;
begin

  if IsDesigning() then
    Exit(False);

  CheckForMft();
  Result := Assigned(FIns) and FIns.GetCurrentCoeffs(C,
                                                     SampleRate);
end;


function TMfParametricEqEffect.GetTargetCoeffs(out C: TBiquadCoeffs;
                                               out SampleRate: Double): Boolean;
begin

  if IsDesigning() then
    Exit(False);

  CheckForMft();
  Result := Assigned(FIns) and FIns.GetTargetCoeffs(C, SampleRate);
end;


end.

