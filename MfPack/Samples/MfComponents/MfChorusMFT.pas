// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioChorusMFT.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Chorus FX as TMfAudioEffectMFTBase derived MFT (float32 internal).
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
unit MfChorusMFT;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfError,
  {Application}
  MfWasApiFxComponentBase,
  MfAudioEffectMFTBase;

type

  TSmoothedParam = record
    Cur: Double;
    Target: Double;
    Step: Double;
    SamplesLeft: Integer;

    procedure Reset(const AValue: Double);
    procedure SetTargetRamp(const ATarget: Double;
                            RampSamples: Integer);
    function Tick(): Double;
  end;


  TMfChorusMFT = class(TMfAudioEffectMFTBase)
  private

    FEnabled: Boolean;

    // Chorus params
    FRateHz: Single;          // LFO speed
    FDepthMs: Single;         // modulation depth
    FBaseDelayMs: Single;     // base delay
    FMix: Single;             // 0..1
    FFeedback: Single;        // 0.. < 1
    FWidthPct: Single;  // 0..100 width (%)

    // Settings snapshot + smoothing
    FSettings: TChorusSettings;
    FSmoothMs: Single;

    SMix: TSmoothedParam;
    SFeedback: TSmoothedParam;
    SBaseDelayMs: TSmoothedParam;
    SDepthMs: TSmoothedParam;
    SRateHz: TSmoothedParam;
    SWidthPct: TSmoothedParam;


    // State
    FLastSampleRate: Integer;
    FLastChannels: Integer;

    FPhase: Double;
    FPhaseInc: Double;
    FRPhaseOffset: Double;

    FDelayLen: Integer; // ring length in samples
    FWritePos: Integer; // 0..FDelayLen-1

    FDelayBuf: array of array of Single; // [ch][i]

    procedure RebuildState(const SampleRate,
                           Channels: Integer);

    class function Clamp01(const x: Single): Single; static;
    class function ClampFB(const x: Single): Single; static;

    class function NoteDivToBeats(const D: TMfChorusNoteDiv): Double; static;

    class function TempoSyncToHz(const Bpm: Double;
                                 const DivBeats: Double): Double; static;

    class function CatmullRom(const p0,
                              p1,
                              p2,
                              p3,
                              t: Single): Single; static;

    function ReadFrac(const ch: Integer;
                      const pos: Double): Single;

  protected

    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer); override;

  public

    constructor Create();
    destructor Destroy(); override;

    // Enable / params
    procedure SetEnabled(const AEnabled: Boolean);
    procedure SetRateHz(const AValue: Single);
    procedure SetDepthMs(const AValue: Single);
    procedure SetBaseDelayMs(const AValue: Single);
    procedure SetMix(const AValue: Single);
    procedure SetFeedback(const AValue: Single);
    procedure SetWidthPct(const AValue: Single);
    // Backward compat (degrees -> width%). Avoid using in new code.
    procedure SetStereoPhaseDeg(const AValue: Single);
    procedure SetSettings(const S: TChorusSettings);

  published

    property Enabled: Boolean read FEnabled write SetEnabled;
    property ChorusSettings: TChorusSettings read FSettings write SetSettings;
    property RateHz: Single read FRateHz write SetRateHz;                 // 0.01..20
    property DepthMs: Single read FDepthMs write SetDepthMs;              // 0..25
    property BaseDelayMs: Single read FBaseDelayMs write SetBaseDelayMs;  // 1..60
    property Mix: Single read FMix write SetMix;                          // 0..1
    property Feedback: Single read FFeedback write SetFeedback;           // 0..0.95
    property WidthPct: Single read FWidthPct write SetWidthPct; // 0..100
  end;


implementation


procedure TSmoothedParam.Reset(const AValue: Double);
begin
  Cur := AValue;
  Target := AValue;
  Step := 0.0;
  SamplesLeft := 0;
end;

procedure TSmoothedParam.SetTargetRamp(const ATarget: Double; RampSamples: Integer);
begin
  Target := ATarget;
  if RampSamples <= 0 then
  begin
    Cur := Target;
    Step := 0.0;
    SamplesLeft := 0;
    Exit;
  end;

  SamplesLeft := RampSamples;
  Step := (Target - Cur) / RampSamples;
end;

function TSmoothedParam.Tick(): Double;
begin
  if SamplesLeft > 0 then
  begin
    Cur := Cur + Step;
    Dec(SamplesLeft);
    if SamplesLeft = 0 then
      Cur := Target;
  end;
  Result := Cur;
end;

{ TMfChorusMFT }

constructor TMfChorusMFT.Create();
begin
  inherited Create();

  FEnabled := True;

  // Sensible chorus defaults
  FRateHz := 0.35;
  FDepthMs := 8.0;
  FBaseDelayMs := 22.0;
  FMix := 0.35;
  FFeedback := 0.10;
  FWidthPct := 90.0;

  FLastSampleRate := 0;
  FLastChannels := 0;
  FPhase := 0.0;
  FPhaseInc := 0.0;
  FRPhaseOffset := 0.0;

  FDelayLen := 0;
  FWritePos := 0;

  // Smoothing defaults
  FSmoothMs := 20.0;
  SMix.Reset(FMix);
  SFeedback.Reset(FFeedback);
  SBaseDelayMs.Reset(FBaseDelayMs);
  SDepthMs.Reset(FDepthMs);
  SRateHz.Reset(FRateHz);
  SWidthPct.Reset(FWidthPct);
end;


destructor TMfChorusMFT.Destroy();
begin

  SetLength(FDelayBuf,
             0);
  inherited;
end;


procedure TMfChorusMFT.SetEnabled(const AEnabled: Boolean);
begin

  FEnabled := AEnabled;
end;


procedure TMfChorusMFT.SetRateHz(const AValue: Single);
begin

  // keep stable (avoid denorm/0)
  if (AValue < 0.01) then
    FRateHz := 0.01
  else
    if (AValue > 20.0) then
      FRateHz := 20.0
  else
    FRateHz := AValue;

  // phase increment depends on samplerate -> recomputed in RebuildState/Process
end;


procedure TMfChorusMFT.SetDepthMs(const AValue: Single);
begin

  if (AValue < 0) then
    FDepthMs := 0
  else
    if (AValue > 25.0) then
      FDepthMs := 25.0
  else
    FDepthMs := AValue;
end;


procedure TMfChorusMFT.SetBaseDelayMs(const AValue: Single);
begin

  if (AValue < 1.0) then
    FBaseDelayMs := 1.0
  else
    if (AValue > 60.0) then
      FBaseDelayMs := 60.0
  else
    FBaseDelayMs := AValue;
end;


procedure TMfChorusMFT.SetMix(const AValue: Single);
begin

  FMix := Clamp01(AValue);
end;


procedure TMfChorusMFT.SetFeedback(const AValue: Single);
begin

  FFeedback := ClampFB(AValue);
end;


procedure TMfChorusMFT.SetWidthPct(const AValue: Single);
begin

  if (AValue < 0) then
    FWidthPct := 0
  else
    if (AValue > 100.0) then
      FWidthPct := 100.0
  else
    FWidthPct := AValue;
end;


procedure TMfChorusMFT.SetStereoPhaseDeg(const AValue: Single);
// Backward compat: degrees (0..180) -> width% (0..100)
begin

  SetWidthPct((AValue / 180.0) * 100.0);
end;


procedure TMfChorusMFT.SetSettings(const S: TChorusSettings);
var
  RampSamples: Integer;
  TargetHz: Double;
  MixV,
  FbV,
  BaseV,
  DepthV,
  WidthV: Double;

begin

  // Snapshot (useful for debugging / potential GetSettings later)
  FSettings := S;
  FEnabled := S.Enabled;

  FSmoothMs := S.SmoothMs;
  if (FSmoothMs < 0) then
    FSmoothMs := 0;

  if (FSmoothMs > 200) then
    FSmoothMs := 200;

  if (FLastSampleRate > 0) then
    RampSamples := Round((FSmoothMs * 0.001) * FLastSampleRate)
  else
    RampSamples := 0;

  // Tempo sync conversion (to Hz)
  if (S.RateMode = crmTempoSync) then
    TargetHz := TempoSyncToHz(S.TempoBpm,
                              NoteDivToBeats(S.NoteDiv))
  else
    TargetHz := S.RateHz;

  if (TargetHz < 0.01) then
    TargetHz := 0.01;
  if (TargetHz > 20.0) then
    TargetHz := 20.0;

  // Clamp targets
  MixV := EnsureRange(S.Mix,
                      0.0,
                      1.0);

  FbV := EnsureRange(S.Feedback,
                     0.0,
                     0.95);

  BaseV := EnsureRange(S.BaseDelayMs,
                       1.0,
                       60.0);

  DepthV := EnsureRange(S.DepthMs,
                        0.0,
                        25.0);

  WidthV := EnsureRange(S.WidthPct,
                        0.0,
                        100.0);

  // Apply via smoothing ramps
  SMix.SetTargetRamp(MixV,
                     RampSamples);
  SFeedback.SetTargetRamp(FbV,
                          RampSamples);
  SBaseDelayMs.SetTargetRamp(BaseV,
                             RampSamples);
  SDepthMs.SetTargetRamp(DepthV,
                         RampSamples);
  SRateHz.SetTargetRamp(TargetHz,
                        RampSamples);
  SWidthPct.SetTargetRamp(WidthV,
                          RampSamples);

  // Keep legacy fields roughly in sync (optional, used by properties)
  FMix := (MixV) * 1.0;
  FFeedback := (FbV) * 1.0;
  FBaseDelayMs := (BaseV) * 1.0;
  FDepthMs := (DepthV) * 1.0;
  FRateHz := (TargetHz) * 1.0;
  FWidthPct := (WidthV) * 1.0;
end;


class function TMfChorusMFT.Clamp01(const x: Single): Single;
begin

  if (x < 0) then
    Result := 0
  else if (x > 1)
    then Result := 1
  else Result := x;
end;


class function TMfChorusMFT.ClampFB(const x: Single): Single;
begin

  if (x < 0) then
    Result := 0
  else if (x > 0.95) then
    Result := 0.95
  else
    Result := x;
end;


class function TMfChorusMFT.NoteDivToBeats(const D: TMfChorusNoteDiv): Double;
begin

  case D of
    cnd1_1:   Result := 4.0;
    cnd1_2:   Result := 2.0;
    cnd1_4:   Result := 1.0;
    cnd1_8:   Result := 0.5;
    cnd1_16:  Result := 0.25;
    cnd1_8T:  Result := 1.0 / 3.0;
    cnd1_16T: Result := 1.0 / 6.0;
  else
    Result := 0.5;
  end;
end;


class function TMfChorusMFT.TempoSyncToHz(const Bpm: Double;
                                          const DivBeats: Double): Double;
var
  SecPerBeat,
  SecPerCycle: Double;

begin
  if (Bpm <= 1.0) then
    Exit(0.35);

  SecPerBeat := 60.0 / Bpm;
  SecPerCycle := SecPerBeat * DivBeats;
  if (SecPerCycle <= 0.0001) then
    SecPerCycle := 0.0001;

  Result := 1.0 / SecPerCycle;
end;


class function TMfChorusMFT.CatmullRom(const p0, p1, p2, p3, t: Single): Single;
var
  t2,
  t3: Single;

begin

  // Standard Catmull-Rom spline (t in [0..1])
  t2 := t * t;
  t3 := t2 * t;

  Result :=
      0.5 * ((2.0 * p1) +
             (-p0 + p2) * t +
             (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
             (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3);
end;


procedure TMfChorusMFT.RebuildState(const SampleRate,
                                    Channels: Integer);
var
  maxDelayMs: Double;
  maxDelaySamples: Integer;
  ch: Integer;

begin

  FLastSampleRate := SampleRate;
  FLastChannels := Channels;

  // Ring buffer sizing: base + depth + margin for cubic taps.
  // Keep generous but bounded.
  maxDelayMs := FBaseDelayMs + FDepthMs + 5.0; // +5ms safety
  if (maxDelayMs < 10.0) then
    maxDelayMs := 10.0;
  if (maxDelayMs > 120.0) then
    maxDelayMs := 120.0;

  maxDelaySamples := Ceil(maxDelayMs * 0.001 * SampleRate);
  if (maxDelaySamples < 64) then
    maxDelaySamples := 64;

  // Need extra headroom for Catmull-Rom taps (p0..p3) around index.
  FDelayLen := maxDelaySamples + 8;

  SetLength(FDelayBuf, Channels);
  for ch := 0 to Channels - 1 do
    begin

      SetLength(FDelayBuf[ch],
                FDelayLen);
      FillChar(FDelayBuf[ch][0],
               FDelayLen * SizeOf(Single),
               0);
    end;

  FWritePos := 0;

  // LFO
  FPhase := 0.0;
  FPhaseInc := (2.0 * PI) * FRateHz / SampleRate;
  FRPhaseOffset := (FWidthPct * PI / 180.0);
end;


function TMfChorusMFT.ReadFrac(const ch: Integer;
                               const pos: Double): Single;
var
  i1,
  i0,
  i2,
  i3: Integer;
  frac: Single;
  p0,
  p1,
  p2,
  p3: Single;
  p: Double;

  // Helper
  function WrapIndex(i: Integer): Integer;
    begin
      while (i < 0) do
        Inc(i,
            FDelayLen);
      while (i >= FDelayLen) do
        Dec(i,
            FDelayLen);

      Result := i;
    end;

begin

  // pos is in "ring index space" [0..FDelayLen)
  p := pos;
  while (p < 0.0) do
    p := p + FDelayLen;
  while (p >= FDelayLen) do
    p := p - FDelayLen;

  i1 := Floor(p);
  frac := (p - i1) * 1.0;

  i0 := WrapIndex(i1 - 1);
  i2 := WrapIndex(i1 + 1);
  i3 := WrapIndex(i1 + 2);
  i1 := WrapIndex(i1);

  p0 := FDelayBuf[ch][i0];
  p1 := FDelayBuf[ch][i1];
  p2 := FDelayBuf[ch][i2];
  p3 := FDelayBuf[ch][i3];

  Result := CatmullRom(p0,
                       p1,
                       p2,
                       p3,
                       frac);
end;


procedure TMfChorusMFT.ProcessAudioFloat32(pData: PSingle;
                                           Frames,
                                           Channels,
                                           SampleRate: Integer);
var

  frame,
  ch: Integer;
  x,
  y,
  wet,
  outS: Single;

  baseS,
  depthS: Double;
  lfoL,
  lfoR: Double;
  dlyL,
  dlyR: Double;
  readPos: Double;

  mixWet,
  mixDry: Double;
  fb: Double;
  p: PSingle;

begin

  if (pData = nil) or
     (Frames <= 0) or
     (Channels <= 0) or
     (SampleRate <= 0) then
    Exit;

  if not FEnabled then
    Exit;

  // Rebuild if format changed or buffers not ready.
  if (SampleRate <> FLastSampleRate) or
     (Channels <> FLastChannels) or
     (FDelayLen <= 0) then
    RebuildState(SampleRate,
                 Channels)
  else
    begin

      // Rate / stereo phase might have changed at runtime
      FPhaseInc := (2.0 * PI) * FRateHz / SampleRate;
      FRPhaseOffset := (FWidthPct * PI / 180.0);
    end;

  // Precompute scalars
  p := pData;


  for frame := 0 to Frames - 1 do
    begin

      // Tick smoothed params once per frame (zipper-noise free)
      mixWet := SMix.Tick();
      fb := SFeedback.Tick();
      // Convert ms -> samples per current frame
      baseS := (SBaseDelayMs.Tick() * 0.001) * SampleRate;
      depthS := (SDepthMs.Tick() * 0.001) * SampleRate;
      // Rate smoothing already applied in Hz
      FPhaseInc := (2.0 * PI) * SRateHz.Tick() / SampleRate;
      // Width% -> phase offset (0..180deg)
      FRPhaseOffset := ((SWidthPct.Tick() * 180.0 / 100.0) * PI / 180.0);

      mixDry := 1.0 - mixWet;
      // LFOs (L and R with optional phase offset)
      lfoL := Sin(FPhase);
      lfoR := Sin(FPhase + FRPhaseOffset);

      // delays in samples
      dlyL := baseS + depthS * lfoL;
      if dlyL < 1.0 then dlyL := 1.0;

      dlyR := baseS + depthS * lfoR;
      if dlyR < 1.0 then dlyR := 1.0;

      // Process channels (supports mono/stereo/multi, stereo gets width via phase offset)
      for ch := 0 to Channels - 1 do
        begin
          x := p^;

          // Choose delay per channel (L/R for first two channels, then use LFO-L for others)
          if (Channels >= 2) and (ch = 1) then
            readPos := FWritePos - dlyR
          else
            readPos := FWritePos - dlyL;

          // Read delayed sample
          y := ReadFrac(ch, readPos);

          // Write into delay line with feedback
          FDelayBuf[ch][FWritePos] := x + (y * (fb * 1.0));

          // Wet signal is delayed sample
          wet := y;

          // Mix
          outS := x * (mixDry) + wet * (mixWet * 1.0) * 1.0;

          p^ := outS;
          Inc(p);
        end;

      // advance ring write pos
      Inc(FWritePos);
      if (FWritePos >= FDelayLen) then
        FWritePos := 0;

      // advance LFO phase
      FPhase := FPhase + FPhaseInc;
      if FPhase >= (2.0 * PI) then
        FPhase := FPhase - (2.0 * PI);
    end;
end;

end.

