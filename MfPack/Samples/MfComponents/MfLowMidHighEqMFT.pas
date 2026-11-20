// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfLowMidHighEqMFT.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: 3-band EQ (Low / Mid / High) MFT.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          - Intended to be hosted by TMfWasApiEffectsRack (float32 processing path).
//          - Uses RBJ cookbook biquad formulas.
//          - Thread-safe settings updates via critical section + atomic dirty flag.
//          - Parameter changes apply on block boundaries (glitch-free enough for live UI).
//
// This unit assumes MfPack provides TMfAudioEffectMFTBase (IMFTransform helper).
//
// Related objects: MfAudioEffectMFTBase
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
unit MfLowMidHighEqMFT;

// MfPack - 3-band EQ (Low / Mid / High) MFT
//
// Notes
//  - Intended to be hosted by TMfWasApiEffectsRack (float32 processing path).
//  - Uses RBJ cookbook biquad formulas.
//  - Thread-safe settings updates via critical section + atomic dirty flag.
//  - Parameter changes apply on block boundaries (glitch-free enough for live UI).
//
// This unit assumes MfPack provides TMfAudioEffectMFTBase (IMFTransform helper).

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Math,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,  // NormalizeBOOL function.
  {Application}
  PcmLib, // Single arrays.
  MfAudioEffectMFTBase;

type

  TMfEqMidMode = (emmPeaking,
                  emmNotch);

  TMfLowMidHighEqSettings = packed record

    Enabled: LongBool;        // Delphi LongBool (-1/0); normalize when needed

    // Smoothing / control
    RampMs: Integer;          // 0 = off

    // Low shelf
    LowFreqHz: Single;        // e.g. 80
    LowGainDb: Single;        // -24..+24
    LowQ: Single;             // 0.1..4 (used as "shelf Q")

    // Mid
    MidFreqHz: Single;        // e.g. 1000
    MidGainDb: Single;        // -24..+24 (ignored for notch mode)
    MidQ: Single;             // 0.1..12
    MidMode: TMfEqMidMode;

    // High shelf
    HighFreqHz: Single;       // e.g. 8000
    HighGainDb: Single;       // -24..+24
    HighQ: Single;            // 0.1..4

    class function Defaults(): TMfLowMidHighEqSettings; static;
  end;

  IMfLowMidHighEqMft = interface(IUnknown)
  ['{7B42E6F1-1D11-4C6B-A5F1-0B6A9F3B0A31}']

    procedure SetSettings(const S: TMfLowMidHighEqSettings); stdcall;

    function GetSettings(out S: TMfLowMidHighEqSettings): HRESULT; stdcall;

    procedure ResetState; stdcall;
  end;

  // 3-band EQ MFT
  TMfLowMidHighEqMFT = class(TMfAudioEffectMFTBase, IMfLowMidHighEqMft)
  private

    FCS: TCriticalSection;

    // Live settings snapshot used by the processing thread.
    FSettings: TMfLowMidHighEqSettings;

    // Pending settings written by UI thread.
    FPendingSettings: TMfLowMidHighEqSettings;
    FPendingDirty: LongInt;

    // Stream format cached for coefficient/state allocation
    FSampleRate: Integer;
    FChannels: Integer;

    // Owned by the processing thread; targets arrive through pending settings.
    FCurrentLowDb: Double;
    FCurrentMidDb: Double;
    FCurrentHighDb: Double;
    FRampSamplesLeft: Integer;
    FRampUpdateFrames: Integer;

    // Smoothed gains (linear) - currently unity, kept for future crossfade.
    // We keep them for future use.
    // These gain members may be reused later for:
    // 1) smooth EQ bypass transitions,
    // 2) wet/dry style blending or staged gain interpolation.
    //FLowGainLin: Double;
    //FMidGainLin: Double;
    //FHighGainLin: Double;

    // Biquad per band per channel
    // y[n] = b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2
    type

      TBiquad = record
        b0, b1, b2: Double;
        a1, a2: Double;
        x1, x2: Double;
        y1, y2: Double;
        procedure Reset;
      end;

      PSingleArray = ^TSingleArray;

    var
      FLow: array of TBiquad; // per channel
      FMid: array of TBiquad;
      FHigh: array of TBiquad;

    procedure ApplyPendingSettings();
    procedure EnsureChannelState();
    procedure StepGainRamp();
    procedure SnapGainRamp();

    procedure CalcLowShelf(const FreqHz,
                           GainDb, Q: Double;
                           const SampleRate: Double;
                           out B: TBiquad);

    procedure CalcHighShelf(const FreqHz,
                            GainDb, Q: Double;
                            const SampleRate: Double;
                            out B: TBiquad);

    procedure CalcPeaking(const FreqHz,
                          GainDb,
                          Q: Double;
                          const SampleRate: Double;
                          out B: TBiquad);

    procedure CalcNotch(const FreqHz,
                        Q: Double;
                        const SampleRate:
                        Double; out B: TBiquad);

    function ClampHz(const Hz: Double;
                     const SampleRate: Integer): Double;

  protected

    // Processing (in-place interleaved float32)
    procedure ProcessAudioFloat32(pData: PSingle; Frames, Channels, SampleRate: Integer); override;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure SetSettings(const S: TMfLowMidHighEqSettings); stdcall;
    function GetSettings(out S: TMfLowMidHighEqSettings): HRESULT; stdcall;
    procedure ResetState; stdcall;
    procedure GetBiquadCoeffs(out Low,
                                 Mid,
                                 High: TBiquadCoeffs;
                                 out SampleRate: Double);
  end;


implementation


{ TMfLowMidHighEqSettings }

class function TMfLowMidHighEqSettings.Defaults: TMfLowMidHighEqSettings;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  // IMPORTANT: Delphi LongBool True = -1. See comments about BOOL in WinApi.WinApiTypes.
  Result.Enabled := True;
  Result.RampMs := 30;

  Result.LowFreqHz := 80.0;
  Result.LowGainDb := 0.0;
  Result.LowQ := 0.707;

  Result.MidFreqHz := 1000.0;
  Result.MidGainDb := 0.0;
  Result.MidQ := 1.0;
  Result.MidMode := emmPeaking;

  Result.HighFreqHz := 8000.0;
  Result.HighGainDb := 0.0;
  Result.HighQ := 0.707;
end;


{ TMfLowMidHighEqMFT.TBiquad }

procedure TMfLowMidHighEqMFT.TBiquad.Reset;
begin

  x1 := 0.0;
  x2 := 0.0;
  y1 := 0.0;
  y2 := 0.0;
end;

{ TMfLowMidHighEqMFT }

constructor TMfLowMidHighEqMFT.Create();
begin

  inherited Create;

  FCS := TCriticalSection.Create();
  FSettings := TMfLowMidHighEqSettings.Defaults();
  FPendingSettings := FSettings;
  FPendingDirty := 0;

  FSampleRate := 0;
  FChannels := 0;

  //FLowGainLin := 1.0;
  //FMidGainLin := 1.0;
  //FHighGainLin := 1.0;
end;


destructor TMfLowMidHighEqMFT.Destroy();
begin

  FCS.Free();

  inherited Destroy();
end;


procedure TMfLowMidHighEqMFT.SetSettings(const S: TMfLowMidHighEqSettings);
begin

  // UI thread safe: store pending copy
  FCS.Enter();

  try

    FPendingSettings := S;
    InterlockedExchange(FPendingDirty,
                        1);
  finally

    FCS.Leave();
  end;
end;


function TMfLowMidHighEqMFT.GetSettings(out S: TMfLowMidHighEqSettings): HRESULT;
begin

  FCS.Enter();

  try

    S := FSettings;
    Result := S_OK;
  finally

    FCS.Leave();
  end;
end;


procedure TMfLowMidHighEqMFT.GetBiquadCoeffs(out Low,
                                              Mid,
                                              High: TBiquadCoeffs;
                                              out SampleRate: Double);
var
  Settings: TMfLowMidHighEqSettings;
  B: TBiquad;
  Rate: Integer;

  procedure CopyCoeffs(out Coeffs: TBiquadCoeffs);
  begin

    Coeffs.b0 := B.b0;
    Coeffs.b1 := B.b1;
    Coeffs.b2 := B.b2;
    Coeffs.a1 := B.a1;
    Coeffs.a2 := B.a2;
  end;

begin

  // Read the latest requested tuning without changing processing filter state.
  FCS.Enter();
  try
    Settings := FPendingSettings;
    Rate := FSampleRate;
  finally
    FCS.Leave();
  end;

  if Rate <= 0 then
    Rate := 48000;
  SampleRate := Rate;

  FillChar(Low, SizeOf(Low), 0);
  Low.b0 := 1.0;
  Mid := Low;
  High := Low;
  if not Settings.Enabled then
    Exit;

  CalcLowShelf(ClampHz(Settings.LowFreqHz, Rate),
                EnsureRange(Settings.LowGainDb, -24.0, 24.0),
                EnsureRange(Settings.LowQ, 0.1, 4.0),
                Rate,
                B);
  CopyCoeffs(Low);

  if Settings.MidMode = emmNotch then
    CalcNotch(ClampHz(Settings.MidFreqHz, Rate),
               EnsureRange(Settings.MidQ, 0.1, 12.0),
               Rate,
               B)
  else
    CalcPeaking(ClampHz(Settings.MidFreqHz, Rate),
                 EnsureRange(Settings.MidGainDb, -24.0, 24.0),
                 EnsureRange(Settings.MidQ, 0.1, 12.0),
                 Rate,
                 B);
  CopyCoeffs(Mid);

  CalcHighShelf(ClampHz(Settings.HighFreqHz, Rate),
                 EnsureRange(Settings.HighGainDb, -24.0, 24.0),
                 EnsureRange(Settings.HighQ, 0.1, 4.0),
                 Rate,
                 B);
  CopyCoeffs(High);
end;


procedure TMfLowMidHighEqMFT.ApplyPendingSettings;
var
  Settings: TMfLowMidHighEqSettings;
  GainsChanged: Boolean;

begin

  if InterlockedExchange(FPendingDirty, 0) = 0 then
    Exit;

  FCS.Enter();
  try
    Settings := FPendingSettings;
  finally
    FCS.Leave();
  end;

  Settings.LowGainDb := EnsureRange(Settings.LowGainDb, -24.0, 24.0);
  Settings.MidGainDb := EnsureRange(Settings.MidGainDb, -24.0, 24.0);
  Settings.HighGainDb := EnsureRange(Settings.HighGainDb, -24.0, 24.0);
  Settings.RampMs := EnsureRange(Settings.RampMs, 0, 5000);

  GainsChanged := (Settings.LowGainDb <> FSettings.LowGainDb) or
                  (Settings.MidGainDb <> FSettings.MidGainDb) or
                  (Settings.HighGainDb <> FSettings.HighGainDb) or
                  (Settings.RampMs <> FSettings.RampMs);
  FCS.Enter();
  try
    FSettings := Settings;
  finally
    FCS.Leave();
  end;

  if FSettings.RampMs = 0 then
    SnapGainRamp()
  else if GainsChanged then
    begin
      // Retarget from the current gains without resetting the filter histories.
      FRampSamplesLeft := Max(1, Round(FSettings.RampMs * FSampleRate / 1000.0));
      FRampUpdateFrames := 0;
    end;

  EnsureChannelState();
end;


procedure TMfLowMidHighEqMFT.SnapGainRamp();
begin

  FCurrentLowDb := FSettings.LowGainDb;
  FCurrentMidDb := FSettings.MidGainDb;
  FCurrentHighDb := FSettings.HighGainDb;
  FRampSamplesLeft := 0;
  FRampUpdateFrames := 0;
end;


procedure TMfLowMidHighEqMFT.StepGainRamp();
begin

  if FRampSamplesLeft <= 0 then
    Exit;

  FCurrentLowDb := FCurrentLowDb +
                  (FSettings.LowGainDb - FCurrentLowDb) / FRampSamplesLeft;
  FCurrentMidDb := FCurrentMidDb +
                  (FSettings.MidGainDb - FCurrentMidDb) / FRampSamplesLeft;
  FCurrentHighDb := FCurrentHighDb +
                   (FSettings.HighGainDb - FCurrentHighDb) / FRampSamplesLeft;
  Dec(FRampSamplesLeft);
  Inc(FRampUpdateFrames);

  if FRampSamplesLeft = 0 then
    SnapGainRamp();

  // Limit filter redesign work; the cadence is independent of packet sizes.
  if (FRampUpdateFrames >= 32) or (FRampSamplesLeft = 0) then
    begin
      EnsureChannelState();
      FRampUpdateFrames := 0;
    end;
end;


procedure TMfLowMidHighEqMFT.ResetState;
var
  ch: Integer;

begin

  for ch := 0 to Length(FLow) - 1 do
    begin

      FLow[ch].Reset;
      FMid[ch].Reset;
      FHigh[ch].Reset;
    end;
end;


function TMfLowMidHighEqMFT.ClampHz(const Hz: Double;
                                    const SampleRate: Integer): Double;
var
  ny: Double;

begin

  ny := SampleRate * 0.5;
  Result := EnsureRange(Hz,
                        10.0,
                        ny * 0.45);
end;


procedure TMfLowMidHighEqMFT.EnsureChannelState;
var
  ch,
  newCh: Integer;
  sr:
  Double;
  bTmp: TBiquad;
  lf,
  mf,
  hf: Double;
  lq,
  mq,
  hq: Double;
  lg,
  mg,
  hg: Double;
  en: Integer;

begin

  if (FSampleRate <= 0) or (FChannels <= 0) then
    Exit;

  newCh := FChannels;

  if Length(FLow) <> newCh then
    begin

      SetLength(FLow,
                newCh);

      SetLength(FMid,
                newCh);

      SetLength(FHigh,
                newCh);
      ResetState();
    end;

  sr := FSampleRate;

  // Clamp + read settings
  en := NormalizeBOOL(FSettings.Enabled);
  if (en = 0) then
    Exit;

  lf := ClampHz(FSettings.LowFreqHz,
                FSampleRate);

  mf := ClampHz(FSettings.MidFreqHz,
                FSampleRate);

  hf := ClampHz(FSettings.HighFreqHz,
                FSampleRate);

  lq := EnsureRange(FSettings.LowQ,
                    0.1,
                    4.0);

  mq := EnsureRange(FSettings.MidQ,
                    0.1,
                    12.0);

  hq := EnsureRange(FSettings.HighQ,
                    0.1,
                    4.0);

  lg := EnsureRange(FCurrentLowDb,
                   -24.0,
                   24.0);

  mg := EnsureRange(FCurrentMidDb,
                    -24.0,
                    24.0);

  hg := EnsureRange(FCurrentHighDb,
                    -24.0,
                    24.0);

  // Coefficients per band (same for all channels; only state differs).
  CalcLowShelf(lf,
               lg,
               lq,
               sr,
               bTmp);

  for ch := 0 to newCh - 1 do
    begin

      FLow[ch].b0 := bTmp.b0;
      FLow[ch].b1 := bTmp.b1;
      FLow[ch].b2 := bTmp.b2;
      FLow[ch].a1 := bTmp.a1;
      FLow[ch].a2 := bTmp.a2;
    end;

  case FSettings.MidMode of
    emmNotch: CalcNotch(mf,
                        mq,
                        sr,
                        bTmp);
  else
      CalcPeaking(mf,
                  mg,
                  mq,
                  sr,
                  bTmp);
  end;

  for ch := 0 to newCh - 1 do
    begin

      FMid[ch].b0 := bTmp.b0;
      FMid[ch].b1 := bTmp.b1;
      FMid[ch].b2 := bTmp.b2;
      FMid[ch].a1 := bTmp.a1;
      FMid[ch].a2 := bTmp.a2;
    end;

  CalcHighShelf(hf,
                hg,
                hq,
                sr,
                bTmp);

  for ch := 0 to newCh - 1 do
    begin

      FHigh[ch].b0 := bTmp.b0;
      FHigh[ch].b1 := bTmp.b1;
      FHigh[ch].b2 := bTmp.b2;
      FHigh[ch].a1 := bTmp.a1;
      FHigh[ch].a2 := bTmp.a2;
    end;
end;


procedure TMfLowMidHighEqMFT.CalcNotch(const FreqHz,
                                       Q: Double;
                                       const SampleRate: Double;
                                       out B: TBiquad);
var
  w0,
  cosw0,
  sinw0,
  alpha: Double;
  b0,
  b1,
  b2,
  a0,
  a1,
  a2: Double;

begin

  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  alpha := sinw0 / (2.0 * EnsureRange(Q,
                                      0.1,
                                      12.0));

  b0 := 1.0;
  b1 := -2.0 * cosw0;
  b2 := 1.0;
  a0 := 1.0 + alpha;
  a1 := -2.0 * cosw0;
  a2 := 1.0 - alpha;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;


procedure TMfLowMidHighEqMFT.CalcPeaking(const FreqHz,
                                         GainDb,
                                         Q: Double;
                                         const SampleRate: Double;
                                         out B: TBiquad);
var
  A,
  w0,
  cosw0,
  sinw0,
  alpha: Double;
  b0,
  b1,
  b2,
  a0,
  a1,
  a2: Double;

begin

  A := Power(10.0,
             GainDb / 40.0);

  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  alpha := sinw0 / (2.0 * EnsureRange(Q,
                                      0.1,
                                      12.0));

  b0 := 1.0 + alpha * A;
  b1 := -2.0 * cosw0;
  b2 := 1.0 - alpha * A;
  a0 := 1.0 + alpha / A;
  a1 := -2.0 * cosw0;
  a2 := 1.0 - alpha / A;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;


procedure TMfLowMidHighEqMFT.CalcLowShelf(const FreqHz, GainDb, Q: Double; const SampleRate: Double; out B: TBiquad);
var
  A,
  w0,
  cosw0,
  sinw0,
  alpha,
  sqrtA: Double;
  b0,
  b1,
  b2,
  a0,
  a1,
  a2: Double;
  S: Double;

begin

  S := EnsureRange(Q,
                   0.1,
                   4.0);

  A := DbToLin(GainDb);

  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  sqrtA := Sqrt(A);

  alpha := sinw0 / 2.0 * Sqrt((A + 1.0 / A) * (1.0 / S - 1.0) + 2.0);

  b0 := A * ((A + 1.0) - (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha);
  b1 := 2*A * ((A - 1.0) - (A + 1.0) * cosw0);
  b2 := A * ((A + 1.0) - (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha);
  a0 := (A + 1.0) + (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha;
  a1 := -2 * ((A - 1.0) + (A + 1.0) * cosw0);
  a2 := (A + 1.0) + (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;


procedure TMfLowMidHighEqMFT.CalcHighShelf(const FreqHz,
                                           GainDb,
                                           Q: Double;
                                           const SampleRate: Double;
                                           out B: TBiquad);
var
  A,
  w0,
  cosw0,
  sinw0,
  alpha,
  sqrtA: Double;
  b0,
  b1,
  b2,
  a0,
  a1,
  a2: Double;
  S: Double;

begin

  S := EnsureRange(Q,
                   0.1,
                   4.0);

  A := DbToLin(GainDb);

  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  sqrtA := Sqrt(A);

  alpha := sinw0 / 2.0 * Sqrt((A + 1.0 / A) * (1.0 / S - 1.0) + 2.0);

  b0 := A * ((A + 1.0) + (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha);
  b1 := -2*A * ((A - 1.0) + (A + 1.0) * cosw0);
  b2 := A * ((A + 1.0) + (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha);
  a0 := (A + 1.0) - (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha;
  a1 := 2 * ((A - 1.0) - (A + 1.0) * cosw0);
  a2 := (A + 1.0) - (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;


procedure TMfLowMidHighEqMFT.ProcessAudioFloat32(pData: PSingle;
                                                 Frames,
                                                 Channels,
                                                 SampleRate: Integer);
var
  i,
  ch: Integer;
  idx: Integer;
  x,
  y: Double;
  en: Integer;
  FormatChanged: Boolean;
  pS: PSingle;

  // Helper
  function ProcBiquad(var B: TBiquad;
                      const InS: Double): Double;
  begin

    Result := B.b0 * InS + B.b1 * B.x1 + B.b2 * B.x2 - B.a1 * B.y1 - B.a2 * B.y2;
    B.x2 := B.x1;
    B.x1 := InS;
    B.y2 := B.y1;
    B.y1 := Result;
  end;

begin

  if (pData = nil) or
     (Frames <= 0) or
     (Channels <= 0) or
     (SampleRate <= 0) then
    Exit;

  FormatChanged := (SampleRate <> FSampleRate) or (Channels <> FChannels);
  FSampleRate := SampleRate;
  FChannels := Channels;
  ApplyPendingSettings();

  if FormatChanged then
    begin
      // Opening/reopening a stream starts at its configured tuning.
      SnapGainRamp();
      EnsureChannelState();
      ResetState();
    end;

  en := NormalizeBOOL(FSettings.Enabled);
  if (en = 0) then
    Exit;

  idx := 0;

  for i := 0 to Frames - 1 do
    begin
      StepGainRamp();

      for ch := 0 to Channels - 1 do
        begin

          pS := PtrSingleOffset(pData,
                                idx);

          x := pS^;

          y := ProcBiquad(FLow[ch],
                          x);
          y := ProcBiquad(FMid[ch],
                          y);
          y := ProcBiquad(FHigh[ch],
                          y);

          if (y > 1.0) then
            y := 1.0
          else
            if (y < -1.0) then
              y := -1.0;

          pS^ := y;
          Inc(idx);
        end;
    end;
end;

end.
