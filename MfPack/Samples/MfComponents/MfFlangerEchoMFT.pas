// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfFlangerEchoMFT.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Flanger/echo MFT.
//              Supports wet/dry mix (equal-power), delay/flange modulation, feedback (capped at 0.98),
//              max buffer size (2000 ms), Catmull-Rom interpolation for fractional delay.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
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
// Related objects: MfAudioEffectMFTBase
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
unit MfFlangerEchoMFT;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  PcmLib,
  MfAudioEffectMFTBase;

type

  // Control interface.
  IMfFlangerEchoControl = interface(IInterface)
  ['{A1A4F7A0-7FA1-4B1C-9A0C-6B7D0F5C3C6E}']

    procedure EnableFX(const AEnabled: Boolean); stdcall;

    // BaseDelayMs: echo uses large, flanger uses small.
    procedure SetBaseDelayMs(const Ms: Single); stdcall;    // 0..2000
    procedure SetDepthMs(const Ms: Single); stdcall;        // 0..50 (practical)
    procedure SetRateHz(const Hz: Single); stdcall;         // 0..20
    procedure SetFeedback(const v: Single); stdcall;        // -0.98..+0.98

    // Wet in [0..1]. Uses equal-power mix internally.
    procedure SetWet(const v: Single); stdcall;

    // Stereo phase offset in degrees (typical 0..180). Used for 2ch; for >2ch, applies same LFO to all.
    procedure SetStereoPhaseDeg(const Deg: Single); stdcall;

    // Presets (optional convenience)
    procedure SetPresetFlanger(); stdcall;
    procedure SetPresetEcho(); stdcall;

    // Ramp / smoothing like EQ
    procedure SetRampMode(const Mode: TMfRampMode); stdcall;
    procedure SetRampTimeMs(const Ms: Integer); stdcall;

    // True-peak guard (shared base)
    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;
    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;   // default -1.0
    procedure SetTruePeakOversample(const Factor: Integer); stdcall; // 2/4/8
  end;

  // Optional inspection
  IMfFlangerEchoInspect = interface(IInterface)
    ['{0D6F9B0D-6A5F-4A54-B7C6-1C5B2DAA5B11}']
    function GetCurrentParams(out BaseDelayMs,
                              DepthMs,
                              RateHz,
                              Feedback,
                              Wet,
                              StereoPhaseDeg: Double): Boolean; stdcall;
  end;

type

  TMfFlangerEchoMFT = class(TMfAudioEffectMFTBase, IMFTransform, IMfFlangerEchoControl, IMfFlangerEchoInspect)
  private

    FEnabledInt: Integer;

    // Targets (set from GUI) - stored lock-free as IEEE754 bits.
    FTBaseDelayBits: Integer;
    FTDepthBits: Integer;
    FTRateBits: Integer;
    FTFeedbackBits: Integer;
    FTWetBits: Integer;
    FTStereoPhaseBits: Integer;

    // Current (smoothed)
    FCBaseDelayMs: Single;
    FCDepthMs: Single;
    FCRateHz: Single;
    FCFeedback: Single;
    FCWet: Single;
    FCStereoPhaseDeg: Single;

    FRampMode: TMfRampMode;
    FRampTimeMs: Integer;

    // Internal delay buffers (float, per-channel)
    FDelay: array of Single;
    FDelayChannels: Integer;
    FDelayFrames: Integer;   // frames in buffer (per channel)
    FWritePos: Integer;      // frame index

    // LFO
    FPhase: Single;

    // cached format
    FLastSampleRate: Integer;
    FLastChannels: Integer;

    procedure EnsureDelayBuffer(const Channels,
                                SampleRate: Integer);

    function ClampFeedback(const v: Single): Single;

    function ClampWet(const v: Single): Single;

    function ClampMs(const v,
                     lo,
                     hi: Single): Single;

    function ClampHz(const v: Single): Single;

    function RampCoefPerSample(const SampleRate: Integer): Single;

    function ReadDelayCubic(const Ch: Integer;
                            const DelayFrames: Single): Single; inline;

    procedure WriteDelay(const Ch: Integer;
                         const v: Single); inline;

  protected

    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer); override;

  public

    constructor Create();
    destructor Destroy(); override;

    // IMfFlangerEchoControl
    procedure EnableFX(const AEnabled: Boolean); stdcall;

    procedure SetBaseDelayMs(const Ms: Single); stdcall;
    procedure SetDepthMs(const Ms: Single); stdcall;
    procedure SetRateHz(const Hz: Single); stdcall;
    procedure SetFeedback(const v: Single); stdcall;
    procedure SetWet(const v: Single); stdcall;
    procedure SetStereoPhaseDeg(const Deg: Single); stdcall;

    procedure SetPresetFlanger(); stdcall;
    procedure SetPresetEcho(); stdcall;

    procedure SetRampMode(const Mode: TMfRampMode); stdcall;
    procedure SetRampTimeMs(const Ms: Integer); stdcall;

    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;
    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;
    procedure SetTruePeakOversample(const Factor: Integer); stdcall;

    // IMfFlangerEchoInspect
    function GetCurrentParams(out BaseDelayMs,
                              DepthMs,
                              RateHz,
                              Feedback,
                              Wet,
                              StereoPhaseDeg: Double): Boolean; stdcall;
  end;


implementation


function FloatToBits(const x: Single): Integer; inline;
begin

  Result := PInteger(@x)^;
end;


function BitsToFloat(const b: Integer): Single; inline;
begin

  Result := PSingle(@b)^;
end;


constructor TMfFlangerEchoMFT.Create();
begin

  inherited Create;

  // Default disabled (true bypass) to avoid unwanted processing when the MFT is present in the chain.
  FEnabledInt := 0;

  // Defaults (subtle flanger), wet starts at 0 for bypass until enabled by UI.
  InterlockedExchange(FTBaseDelayBits,
                      FloatToBits(3.0));

  InterlockedExchange(FTDepthBits,
                      FloatToBits(2.0));

  InterlockedExchange(FTRateBits,
                      FloatToBits(0.25));

  InterlockedExchange(FTFeedbackBits,
                      FloatToBits(0.2));

  InterlockedExchange(FTWetBits,
                      FloatToBits(0.0));

  InterlockedExchange(FTStereoPhaseBits,
                      FloatToBits(180.0));

  // Smoothed state starts at targets.
  FCBaseDelayMs := 3.0;
  FCDepthMs := 2.0;
  FCRateHz := 0.25;
  FCFeedback := 0.2;
  FCWet := 0.0;
  FCStereoPhaseDeg := 180.0;

  FRampMode := rmSmooth;
  FRampTimeMs := 50;

  FDelayChannels := 0;
  FDelayFrames := 0;
  FWritePos := 0;

  FPhase := 0.0;

  FLastSampleRate := 0;
  FLastChannels := 0;
end;


destructor TMfFlangerEchoMFT.Destroy();
begin

  FDelay := nil;

  inherited;
end;


function TMfFlangerEchoMFT.ClampMs(const v,
                                   lo,
                                   hi: Single): Single;
begin

  Result := EnsureRange(v,
                        lo,
                        hi);
end;


function TMfFlangerEchoMFT.ClampHz(const v: Single): Single;
begin

  Result := EnsureRange(v,
                        0.0,
                        20.0);
end;


function TMfFlangerEchoMFT.ClampFeedback(const v: Single): Single;
begin

  Result := EnsureRange(v,
                        -0.98,
                        0.98);
end;


function TMfFlangerEchoMFT.ClampWet(const v: Single): Single;
begin

  Result := EnsureRange(v,
                        0.0,
                        1.0);
end;


procedure TMfFlangerEchoMFT.EnsureDelayBuffer(const Channels,
                                              SampleRate: Integer);
var
  needFrames: Integer;
  total: Integer;

begin

  // 2000ms max + 4 frames safety for cubic taps
  needFrames := Ceil((2000.0 / 1000.0) * SampleRate) + 8;
  if (needFrames < 16) then
    needFrames := 16;

  if (FDelayChannels <> Channels) or (FDelayFrames <> needFrames) then
    begin

      FDelayChannels := Channels;
      FDelayFrames := needFrames;
      total := FDelayChannels * FDelayFrames;
      SetLength(FDelay,
                total);

      FillChar(FDelay[0],
               total * SizeOf(Single),
               0);

      FWritePos := 0;
      FPhase := 0;
    end;
end;


function TMfFlangerEchoMFT.RampCoefPerSample(const SampleRate: Integer): Single;
var
  ms: Integer;
  tau: Double;

begin

  if (SampleRate <= 0) then
    Exit(0.0);

  case FRampMode of
    rmOff: ms := 0;  // Truly off (instant)
    rmFast: ms := 10;
    rmSmooth: ms := 30;
    rmManual: ms := FRampTimeMs;
  else

    ms := 30;
  end;

  if (ms <= 0) then
    Exit(0.0); // Alpha = 0 => State becomes target immediately.

  tau := ms / 1000.0;

  // alpha in (0..1): closer to 1 = more smoothing
  Result := (Exp(-1.0 / (tau * SampleRate))) * 1.0;
end;


function TMfFlangerEchoMFT.ReadDelayCubic(const Ch: Integer;
                                          const DelayFrames: Single): Single;
var
  readPos: Single;
  i1,
  i0,
  i2,
  i3: Integer;
  frac: Single;
  base: Integer;
  idx0,
  idx1,
  idx2,
  idx3: Integer;

begin

  // readPos in frame domain
  readPos := FWritePos - DelayFrames;

  while (readPos < 0 )do
    readPos := readPos + FDelayFrames;

  while (readPos >= FDelayFrames) do
    readPos := readPos - FDelayFrames;

  base := Trunc(readPos);
  frac := readPos - base;

  i1 := base;
  i0 := i1 - 1;

  if i0 < 0 then
  i0 := i0 + FDelayFrames;

  i2 := i1 + 1;
  if (i2 >= FDelayFrames) then
    i2 := i2 - FDelayFrames;

  i3 := i1 + 2;
  if (i3 >= FDelayFrames) then
    i3 := i3 - FDelayFrames;

  idx0 := Ch * FDelayFrames + i0;
  idx1 := Ch * FDelayFrames + i1;
  idx2 := Ch * FDelayFrames + i2;
  idx3 := Ch * FDelayFrames + i3;

  Result := MfCatmullRomS(FDelay[idx0],
                          FDelay[idx1],
                          FDelay[idx2],
                          FDelay[idx3],
                          frac);
end;


procedure TMfFlangerEchoMFT.WriteDelay(const Ch: Integer;
                                       const v: Single);
begin

  FDelay[Ch * FDelayFrames + FWritePos] := v;
end;


procedure TMfFlangerEchoMFT.ProcessAudioFloat32(pData: PSingle;
                                                Frames,
                                                Channels,
                                                SampleRate: Integer);
const
  DENORM: Single = 1e-20;

var
  n: Integer;
  ch: Integer;
  wet: Single;
  dry: Single;
  fb: Single;
  baseDelayS,
  depthS: Single;
  phaseStep: Single;
  lfo: Single;
  delayFrames: Single;
  delayed: Single;
  x,
  y: Single;
  phR: Single;
  phOff: Single;
  p: PSingle;

  // Target params (snapshotted)
  tBase,
  tDepth,
  tRate,
  tFb,
  tWet,
  tPhase: Single;

  // Per-sample smoothing coef
  smS: Single;

  // Enable/disable
  en: Integer;

begin

  if (Frames <= 0) or
     (Channels <= 0) or
     (pData = nil) then
    Exit;

  // Bypass the effect when disabled.
  en := InterlockedCompareExchange(FEnabledInt,
                                   0,
                                   0);
  if (en = 0) then
    Exit;

  FLastSampleRate := SampleRate;
  FLastChannels := Channels;

  EnsureDelayBuffer(Channels,
                    SampleRate);

  // Snapshot targets (lock-free).
  tBase := ClampMs(BitsToFloat(InterlockedCompareExchange(FTBaseDelayBits,
                                                         0,
                                                         0)),
                   0.0,
                   2000.0);

  tDepth := ClampMs(BitsToFloat(InterlockedCompareExchange(FTDepthBits,
                                                          0,
                                                          0)),
                    0.0,
                    50.0);

  tRate := ClampHz(BitsToFloat(InterlockedCompareExchange(FTRateBits,
                                                         0,
                                                         0)));

  tFb := ClampFeedback(BitsToFloat(InterlockedCompareExchange(FTFeedbackBits,
                                                             0,
                                                             0)));

  tWet := ClampWet(BitsToFloat(InterlockedCompareExchange(FTWetBits,
                                                         0,
                                                         0)));

  tPhase := BitsToFloat(InterlockedCompareExchange(FTStereoPhaseBits,
                                                   0,
                                                   0));

  // Ensure delay never exceeds buffer limit
  if (tBase + tDepth > 2000.0) then
    tDepth := EnsureRange(tDepth,
                          0.0,
                          2000.0 - tBase);

  smS := RampCoefPerSample(SampleRate);

  phOff := ((FCStereoPhaseDeg * PI) / 180.0) * 1.0;

  p := pData;

  // Mono fast path.
  if (Channels = 1) then
    begin

      for n := 0 to Frames - 1 do
        begin

          // Smooth toward targets per sample (prevents zipper/clicks at block boundaries)
          FCBaseDelayMs := smS * FCBaseDelayMs + (1 - smS) * tBase;
          FCDepthMs := smS * FCDepthMs + (1 - smS) * tDepth;
          FCRateHz := smS * FCRateHz + (1 - smS) * tRate;
          FCFeedback := smS * FCFeedback + (1 - smS) * tFb;
          FCWet := smS * FCWet + (1 - smS) * tWet;
          FCStereoPhaseDeg := smS * FCStereoPhaseDeg + (1 - smS) * tPhase;

          // Equal-power mix.
          wet := Sin(FCWet * (PI / 2));
          dry := Cos(FCWet * (PI / 2));
          fb := FCFeedback;

          baseDelayS := (FCBaseDelayMs / 1000.0) * SampleRate;
          depthS := (FCDepthMs / 1000.0) * SampleRate;

          phaseStep := (2.0 * PI) * (FCRateHz / SampleRate);
          lfo := Sin(FPhase);
          delayFrames := baseDelayS + depthS * lfo;

          if (delayFrames < 0) then
            delayFrames := 0;

          x := p^ + DENORM;
          delayed := ReadDelayCubic(0,
                                    delayFrames);
          y := dry * x + wet * delayed;

          // Feedback write.
          WriteDelay(0,
                     x + delayed * fb);

          p^ := y - DENORM;
          Inc(p);

          Inc(FWritePos);

          if (FWritePos >= FDelayFrames) then
            FWritePos := 0;

          FPhase := FPhase + phaseStep;
          if (FPhase > 2 * PI ) then
            FPhase := FPhase - 2 * PI;
        end;
      Exit;
    end;

  // Multi-channel.
  for n := 0 to Frames - 1 do
    begin

      // Smooth toward targets per sample
      FCBaseDelayMs := smS * FCBaseDelayMs + (1 - smS) * tBase;
      FCDepthMs := smS * FCDepthMs + (1 - smS) * tDepth;
      FCRateHz := smS * FCRateHz + (1 - smS) * tRate;
      FCFeedback := smS * FCFeedback + (1 - smS) * tFb;
      FCWet := smS * FCWet + (1 - smS) * tWet;
      FCStereoPhaseDeg := smS * FCStereoPhaseDeg + (1 - smS) * tPhase;

      wet := Sin(FCWet * (PI / 2));
      dry := Cos(FCWet * (PI / 2));
      fb := FCFeedback;

      baseDelayS := (FCBaseDelayMs / 1000.0) * SampleRate;
      depthS := (FCDepthMs / 1000.0) * SampleRate;

      phaseStep := (2.0 * PI) * (FCRateHz / SampleRate);
      phOff := ((FCStereoPhaseDeg * PI) / 180.0);

      // LFO phases: stereo (ch 0 uses FPhase, ch 1 uses FPhase+offset).
      // For >2 channels: same phase (keeps it predictable and fast).
      lfo := Sin(FPhase);
      phR := FPhase + phOff;

      for ch := 0 to Channels - 1 do
        begin

          if (Channels = 2) and (ch = 1) then
            delayFrames := baseDelayS + depthS * Sin(phR)
          else
            delayFrames := baseDelayS + depthS * lfo;

          if (delayFrames < 0) then
            delayFrames := 0;

          x := p^ + DENORM;
          delayed := ReadDelayCubic(ch,
                                    delayFrames);

          y := dry * x + wet * delayed;

          WriteDelay(ch,
                     x + delayed * fb);

          p^ := y - DENORM;
          Inc(p);
        end;

      Inc(FWritePos);

      if (FWritePos >= FDelayFrames) then
        FWritePos := 0;

      FPhase := FPhase + phaseStep;
      if (FPhase > 2 * PI) then
        FPhase := FPhase - 2*PI;
    end;
end;


{ IMfFlangerEchoControl }


procedure TMfFlangerEchoMFT.EnableFX(const AEnabled: Boolean);

begin

  InterlockedExchange(FEnabledInt,
                      Ord(AEnabled));
end;


procedure TMfFlangerEchoMFT.SetBaseDelayMs(const Ms: Single);
begin

  InterlockedExchange(FTBaseDelayBits,
                      FloatToBits(Ms));
end;


procedure TMfFlangerEchoMFT.SetDepthMs(const Ms: Single);
begin

  InterlockedExchange(FTDepthBits,
                      FloatToBits(Ms));
end;


procedure TMfFlangerEchoMFT.SetRateHz(const Hz: Single);
begin

  InterlockedExchange(FTRateBits,
                      FloatToBits(Hz));
end;


procedure TMfFlangerEchoMFT.SetFeedback(const v: Single);
begin

  InterlockedExchange(FTFeedbackBits,
                      FloatToBits(v));
end;


procedure TMfFlangerEchoMFT.SetWet(const v: Single);
begin

  InterlockedExchange(FTWetBits,
                      FloatToBits(v));
end;


procedure TMfFlangerEchoMFT.SetStereoPhaseDeg(const Deg: Single);
begin

  InterlockedExchange(FTStereoPhaseBits,
                      FloatToBits(Deg));
end;


procedure TMfFlangerEchoMFT.SetPresetFlanger();
begin

  InterlockedExchange(FTBaseDelayBits,
                      FloatToBits(3.00));

  InterlockedExchange(FTDepthBits,
                      FloatToBits(2.00));

  InterlockedExchange(FTRateBits,
                      FloatToBits(0.25));

  InterlockedExchange(FTFeedbackBits,
                      FloatToBits(0.20));

  InterlockedExchange(FTWetBits,
                      FloatToBits(0.35));

  InterlockedExchange(FTStereoPhaseBits,
                      FloatToBits(180.00));
end;


procedure TMfFlangerEchoMFT.SetPresetEcho();
begin

  InterlockedExchange(FTBaseDelayBits,
                      FloatToBits(250.00));

  InterlockedExchange(FTDepthBits,
                      FloatToBits(0.00));

  InterlockedExchange(FTRateBits,
                      FloatToBits(0.00));

  InterlockedExchange(FTFeedbackBits,
                      FloatToBits(0.35));

  InterlockedExchange(FTWetBits,
                      FloatToBits(0.35));

  InterlockedExchange(FTStereoPhaseBits,
                      FloatToBits(0.00));
end;


procedure TMfFlangerEchoMFT.SetRampMode(const Mode: TMfRampMode);
begin

  FRampMode := Mode;
end;


procedure TMfFlangerEchoMFT.SetRampTimeMs(const Ms: Integer);
begin

  FRampTimeMs := EnsureRange(Ms,
                             0,
                             2000);
end;


procedure TMfFlangerEchoMFT.EnableTruePeakGuard(const AEnabled: Boolean);
begin

  SetTruePeakEnabled(AEnabled);
end;


procedure TMfFlangerEchoMFT.SetTruePeakCeilingDbTP(const DbTP: Single);
begin

  inherited SetTruePeakCeilingDbTP(DbTP);
end;


procedure TMfFlangerEchoMFT.SetTruePeakOversample(const Factor: Integer);
begin

  inherited SetTruePeakOversample(Factor);
end;

{ IMfFlangerEchoInspect }

function TMfFlangerEchoMFT.GetCurrentParams(out BaseDelayMs,
                                            DepthMs,
                                            RateHz,
                                            Feedback,
                                            Wet,
                                            StereoPhaseDeg: Double): Boolean;
begin

  BaseDelayMs := FCBaseDelayMs;
  DepthMs := FCDepthMs;
  RateHz := FCRateHz;
  Feedback := FCFeedback;
  Wet := FCWet;
  StereoPhaseDeg := FCStereoPhaseDeg;
  Result := (FLastSampleRate > 0);
end;

end.

