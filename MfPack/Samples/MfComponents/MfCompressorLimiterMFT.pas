// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCompressorLimiterMFT.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Compressor + limiter MFT with GR meters.
//              Includes smoothed true-peak limiter using Catmull-Rom oversampling (2/4/8),
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
unit MfCompressorLimiterMFT;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  PcmLib,
  MfAudioEffectMFTBase;

type

  // ------------------------ Settings record ----------------------------------
  PDynamicsSettings = ^TDynamicsSettings;
  TDynamicsSettings = packed record
    // Compressor
    CompEnabled: Boolean;
    CompThresholdDb: Single;   // e.g. -30..0
    CompRatio: Single;         // e.g. 1..20
    CompAttackMs: Single;      // e.g. 0.1..200
    CompReleaseMs: Single;     // e.g. 5..2000
    CompKneeDb: Single;        // e.g. 0..24
    CompMakeupDb: Single;      // Manual makeup.
    CompAutoMakeup: Boolean;   // Auto makeup gain.

    // Limiter
    LimEnabled: Boolean;
    LimCeilingDb: Single;      // Peak ceiling in dBFS (e.g. -1)
    LimReleaseMs: Single;      // e.g. 5..2000
    LimLookaheadMs: Single;    // e.g. 0..20

    // Metering / behavior
    RmsDetector: Boolean;     // True = RMS envelope, False = peak envelope.
  end;

  // Control interface
  IMfCompressorLimiterControl = interface(IInterface)
  ['{B0B56E3A-20DE-4F8A-B223-6D5C54B7B514}']

    procedure EnableFX(const AEnabled: Boolean); stdcall;

    procedure SetSettings(const S: TDynamicsSettings); stdcall;

    procedure GetSettings(out S: TDynamicsSettings); stdcall;

    // True-peak guard (delegates to base).
    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;

    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;

    procedure SetTruePeakOversample(const Factor: Integer); stdcall;
  end;

  // Inspection interface.
  IMfCompressorLimiterInspect = interface(IInterface)
  ['{59F0BB8A-7A32-4F10-9E0F-0C0D7A7F8E2A}']

    function GetCompressorGRdB(): Single; stdcall; // Negative (gain reduction).

    function GetLimiterGRdB(): Single; stdcall; // negative (gain reduction).
  end;

type

  // ------------------------ Simple internal DSP ------------------------------
  // NOTE: This is intentionally written so we can replace it later with our
  // Sample-4 TAudioDynamicsDSP implementation with minimal changes.
  TAudioDynamicsDSP = class
  private

    FSR: Integer;
    FCh: Integer;

    // Smoothed gains (linear).
    FCompGain: Single;
    FLimGain: Single;

    // Envelope states
    FEnv: Single;
    FRmsEnv: Single;

    // Lookahead buffer (interleaved float samples).
    FDelay: array of Single;
    FDelayFrames: Integer;
    FDelayPos: Integer;

    // Meters (atomic-ish enough for GUI polling).
    FCompGRdB: Single;
    FLimGRdB: Single;

    procedure CheckForDelay(const LookaheadMs: Single);
    function DbToLin(const dB: Single): Single; inline;
    function LinToDb(const x: Single): Single; inline;
    function SmoothCoefMs(const Ms: Single): Single; inline;

    function SoftKneeGainDb(const InDb,
                            ThresholdDb,
                            Ratio,
                            KneeDb: Single): Single;
  public

    Settings: TDynamicsSettings;

    constructor Create();

    procedure ResetState();
    procedure ConfigureFormat(const SampleRate,
                              Channels: Integer);

    procedure ProcessFloat32(p: PSingle;
                             Frames,
                             Channels: Integer);

    function GetCompGRdB(): Single;
    function GetLimGRdB(): Single;
  end;


  TMfCompressorLimiterMFT = class(TMfAudioEffectMFTBase,
                                  IMFTransform,
                                  IMfCompressorLimiterControl,
                                  IMfCompressorLimiterInspect)
  private

    FEnabledInt: Integer;
    FTruePeakEnabledInt: Integer;
    // Active settings snapshot pointer (points to one of the two buffers below).
    // We use a double-buffered snapshot to avoid any allocation/free races
    // between the GUI thread (calling SetSettings) and the audio thread
    // (reading FSettingsPtr in ProcessAudioFloat32). This prevents rare
    // use-after-free glitches that can manifest as audible disruptions.
    FSettingsPtr: Pointer;
    FSettingsA: TDynamicsSettings;
    FSettingsB: TDynamicsSettings;

    FDsp: TAudioDynamicsDSP;

    // cached format
    FLastSampleRate: Integer;
    FLastChannels: Integer;

    // True-peak guard (internal, so no dependency on base class).
    FTruePeakCeilingDbTP: Single; // default -1.0.
    FTruePeakOS: Integer;  // 2/4/8, default 4.

    // True-peak limiter (smoothed gain)
    FTPGain: Single;      // current applied gain (linear), <= 1
    FTPAttackMs: Single;  // default 1..5 ms
    FTPReleaseMs: Single; // default 50..150 ms

    // Cached lookahead.
    FLastLookaheadMs: Single;


    function DbToLin(const dB: Single): Single; inline;

    function EstimateTruePeakOS(const x: PSingle;
                                Frames,
                                Channels,
                                OS: Integer): Single;

    function SmoothCoefMs(const Ms: Single;
                          const SampleRate: Integer): Single; inline;

    function SmoothCoefPerBlock(const Ms: Single;
                                const SampleRate,
                                Frames: Integer): Single; inline;

    procedure ApplyTruePeakLimiter(const x: PSingle;
                                   Frames,
                                   Channels,
                                   SampleRate: Integer);

  protected

    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer); override;
  public

    constructor Create();
    destructor Destroy(); override;

    // IMfCompressorLimiterControl
    procedure EnableFX(const AEnabled: Boolean); stdcall;
    procedure SetSettings(const S: TDynamicsSettings); stdcall;
    procedure GetSettings(out S: TDynamicsSettings); stdcall;

    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;
    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;
    procedure SetTruePeakOversample(const Factor: Integer); stdcall;

    // IMfCompressorLimiterInspect
    function GetCompressorGRdB: Single; stdcall;
    function GetLimiterGRdB: Single; stdcall;
  end;


implementation

{ TAudioDynamicsDSP }

constructor TAudioDynamicsDSP.Create();
begin

  inherited Create;

  FillChar(Settings,
           SizeOf(Settings),
           0);

  // Reasonable defaults (close to typical Sample-4 values).
  Settings.CompEnabled := True;
  Settings.CompThresholdDb := -18;
  Settings.CompRatio := 3.0;
  Settings.CompAttackMs := 10;
  Settings.CompReleaseMs := 120;
  Settings.CompKneeDb := 6;
  Settings.CompMakeupDb := 0;
  Settings.CompAutoMakeup := True;

  Settings.LimEnabled := True;
  Settings.LimCeilingDb := -1.0;
  Settings.LimReleaseMs := 80;
  Settings.LimLookaheadMs := 5;

  Settings.RmsDetector := True;

  ResetState();
end;


procedure TAudioDynamicsDSP.ResetState();
begin

  FCompGain := 1.0;
  FLimGain := 1.0;
  FEnv := 0.0;
  FRmsEnv := 0.0;
  FDelayPos := 0;
  FCompGRdB := 0.0;
  FLimGRdB := 0.0;
end;


procedure TAudioDynamicsDSP.ConfigureFormat(const SampleRate, Channels: Integer);
begin

  FSR := SampleRate;
  FCh := Channels;
  CheckForDelay(Settings.LimLookaheadMs);
  ResetState();
end;


function TAudioDynamicsDSP.DbToLin(const dB: Single): Single;
begin

  Result := Power(10.0,
                  dB / 20.0);
end;


function TAudioDynamicsDSP.LinToDb(const x: Single): Single;
begin

  Result := 20.0 * Log10(Max(x,
                             1e-20));
end;


function TAudioDynamicsDSP.SmoothCoefMs(const Ms: Single): Single;
var
  t: Double;

begin

  if (FSR <= 0) or (Ms <= 0) then
    Exit(0);
  t := Ms / 1000.0;
  Result := (Exp(-1.0 / (t * FSR))) * 1.0;
end;


procedure TAudioDynamicsDSP.CheckForDelay(const LookaheadMs: Single);
var
  frames: Integer;

begin

  if (FSR <= 0) or (FCh <= 0) then
    Exit;

  frames := Round((LookaheadMs / 1000.0) * FSR);
  frames := EnsureRange(frames,
                        0,
                        2048); // safety (you can raise)

  if (frames <> FDelayFrames) then
    begin
      FDelayFrames := frames;
      SetLength(FDelay,
                Max(1,
                    FDelayFrames * FCh));
      FDelayPos := 0;
    end;
end;


function TMfCompressorLimiterMFT.DbToLin(const dB: Single): Single;
begin

  Result := Power(10.0,
                  dB / 20.0);
end;


function TMfCompressorLimiterMFT.EstimateTruePeakOS(const x: PSingle;
                                                    Frames,
                                                    Channels,
                                                    OS: Integer): Single;
  function ReadS(const Idx: Integer): Single;
  begin

    Result := PSingle(PByte(x) + (Idx * SizeOf(Single)))^;
  end;

var
  ch,
  n,
  k: Integer;
  idx0,
  idx1,
  idx2,
  idx3: Integer;
  y0,
  y1,
  y2,
  y3: Single;
  t: Single;
  v: Single;

begin

  Result := 0.0;

  if (x = nil) or
     (Frames <= 0) or
     (Channels <= 0) then
    Exit;

  case OS of
    2, 4, 8: ;
  else
    OS := 4;
  end;


  // Per channel, walk segments (frame n -> n+1).
  // For each segment, evaluate Catmull-Rom at intermediate points k/OS.
  for ch := 0 to Channels - 1 do
    begin

      // Always check first sample.
      v := Abs(ReadS(ch));
      if (v > Result) then
        Result := v;

      for n := 0 to Frames - 2 do
        begin

          idx1 := (n * Channels) + ch; // y1 at frame n
          idx2 := idx1 + Channels; // y2 at frame n+1

          // Boundary-safe neighbours:
          // y0 = previous sample (or y1 at start).
          // y3 = next-next sample (or y2 at end).
          if (n = 0) then
            idx0 := idx1
          else
            idx0 := idx1 - Channels;

          if (n + 2) < Frames then
            idx3 := idx2 + Channels
          else
            idx3 := idx2;

          y0 := ReadS(idx0);
          y1 := ReadS(idx1);
          y2 := ReadS(idx2);
          y3 := ReadS(idx3);

          // Check endpoints too (y1 already checked for n=0, but cheap).
          v := Abs(y1);
          if (v > Result) then
            Result := v;

          // Intermediate oversample points.
          for k := 1 to OS - 1 do
            begin

              t := k / OS;
              v := Abs(MfCatmullRomS(y0,
                                     y1,
                                     y2,
                                     y3,
                                     t));
              if (v > Result) then
                Result := v;
            end;
        end;

      // Last sample
      v := Abs(ReadS(((Frames - 1) * Channels) + ch));
      if (v > Result) then
        Result := v;
    end;
end;


function TMfCompressorLimiterMFT.SmoothCoefMs(const Ms: Single;
                                              const SampleRate: Integer): Single;
var
  t: Double;

begin

  if (SampleRate <= 0) or (Ms <= 0.0) then
    Exit(0.0);

  t := Ms / 1000.0;
  Result := (Exp(-1.0 / (t * SampleRate))) * 1.0;
end;


function TMfCompressorLimiterMFT.SmoothCoefPerBlock(const Ms: Single;
                                                    const SampleRate,
                                                    Frames: Integer): Single;
var
  aPerSample: Single;

begin

  aPerSample := SmoothCoefMs(Ms,
                             SampleRate);
  if (aPerSample <= 0) then
    Exit(0.0);

  // Convert per-sample smoothing to per-block (Frames samples)
  Result := Single(Power(aPerSample,
                         Frames));
end;


procedure TMfCompressorLimiterMFT.ApplyTruePeakLimiter(const x: PSingle;
                                                       Frames,
                                                       Channels,
                                                       SampleRate: Integer);
var
  tpPeak: Single;
  ceiling: Single;
  gTgt: Single;
  aAtk,
  aRel: Single;
  total, i: Integer;
  p: PSingle;

begin

  if (x = nil) or
     (Frames <= 0) or
     (Channels <= 0) then
    Exit;

  // Estimate true-peak with Catmull-Rom oversampling
  tpPeak := EstimateTruePeakOS(x,
                               Frames,
                               Channels,
                               FTruePeakOS);

  ceiling := DbToLin(FTruePeakCeilingDbTP); // e.g. -1.0 dBTP
  if (ceiling <= 0) then
    Exit;

  if (tpPeak <= 1e-20) then
    gTgt := 1.0
  else
    if (tpPeak <= ceiling) then
      gTgt := 1.0
    else
      gTgt := ceiling / tpPeak;

  // Attack when we need MORE reduction (gain goes down).
  // Release when we can relax (gain goes up).
  // We do it per-block to keep CPU low and stable.
  aAtk := SmoothCoefPerBlock(Max(0.1,
                                 FTPAttackMs),
                             SampleRate,
                             Frames);

  aRel := SmoothCoefPerBlock(Max(1.0,
                                 FTPReleaseMs),
                             SampleRate,
                             Frames);

  if (gTgt < FTPGain) then
    FTPGain := aAtk * FTPGain + (1 - aAtk) * gTgt
  else
    FTPGain := aRel * FTPGain + (1 - aRel) * gTgt;

  // Safety clamp.
  if (FTPGain > 1.0) then
    FTPGain := 1.0;

  if (FTPGain < 0.0) then
    FTPGain := 0.0;

  // Apply to block (interleaved float)
  total := Frames * Channels;
  p := PSingle(x);

  for i := 0 to total - 1 do
    begin
      p^ := p^ * FTPGain;
      Inc(p);
    end;
end;


function TAudioDynamicsDSP.SoftKneeGainDb(const InDb,
                                          ThresholdDb,
                                          Ratio,
                                          KneeDb: Single): Single;
var
  x,
  y: Single;
  k2: Single;

begin

  // Output level in dB after compression curve (soft knee).
  // Based on common soft-knee approximation.
  if (Ratio <= 1.0001) then
    Exit(InDb);

  if (KneeDb <= 0.0001) then
    begin

      if (InDb <= ThresholdDb) then
        Exit(InDb);

     Exit(ThresholdDb + (InDb - ThresholdDb) / Ratio);
  end;

  k2 := KneeDb * 0.5;
  if (InDb <= ThresholdDb - k2) then
    Exit(InDb);

  if (InDb >= ThresholdDb + k2) then
    Exit(ThresholdDb + (InDb - ThresholdDb) / Ratio);

  // in knee region
  x := InDb - (ThresholdDb - k2); // 0..Knee
  // quadratic interpolation between no-compress and full ratio
  y := InDb + ( (1.0 / Ratio - 1.0) * x * x ) / (2.0 * KneeDb);
  Result := y;
end;


procedure TAudioDynamicsDSP.ProcessFloat32(p: PSingle;
                                           Frames,
                                           Channels: Integer);
const
  DENORM: Single = 1e-20;

  function SamplePtr(const Idx: Integer): PSingle;
  begin

    Result := PSingle(PByte(p) + (Idx * SizeOf(Single)));
  end;

var
  n,
  ch: Integer;
  idx: Integer;
  x,
  y: Single;
  absx: Single;
  level: Single;
  env: Single;
  atkC,
  relC: Single;
  compGain: Single;
  limGain: Single;
  makeup: Single;

  // limiter
  ceilingLin: Single;
  lookFrames: Integer;

  delayedIdx: Integer;
  rdPos: Integer;
  wrPos: Integer;

  inDb: Single;
  outDb: Single;
  gainDb: Single;

begin

  if (Frames <= 0) or (Channels <= 0) then
    Exit;

  if (FSR <= 0) then
    Exit;


  // Precompute smooth coefficients.
  atkC := SmoothCoefMs(Max(0.1,
                           Settings.CompAttackMs));

  relC := SmoothCoefMs(Max(1.0,
                       Settings.CompReleaseMs));

  ceilingLin := DbToLin(Settings.LimCeilingDb);
  lookFrames := FDelayFrames;

  // Auto-makeup approximation: half of threshold reduction at ratio.
  if Settings.CompAutoMakeup and Settings.CompEnabled then
    makeup := DbToLin(Max(0.0,
                          -(Settings.CompThresholdDb) * (1.0 - 1.0 / Max(1.0,
                          Settings.CompRatio)) * 0.5))
  else
    makeup := DbToLin(Settings.CompMakeupDb);

  idx := 0;

  // Mono optimization.
  if (Channels = 1) then
    begin

      for n := 0 to Frames - 1 do
        begin

          x := SamplePtr(n)^ + DENORM;

          // Optional delay line for limiter lookahead (mono).
          if (lookFrames > 0) then
            begin

              rdPos := FDelayPos;
              wrPos := FDelayPos;
              // Read delayed sample.
              y := FDelay[rdPos];
              // Write current.
              FDelay[wrPos] := x;
              Inc(FDelayPos);

              if (FDelayPos >= lookFrames) then
                FDelayPos := 0;
            end
          else
            y := x;

          absx := Abs(x);

          // Detector level (peak or RMS).
          if Settings.RmsDetector then
            begin

              // Very light RMS smoothing.
              FRmsEnv := 0.99 * FRmsEnv + 0.01 * (absx * absx);
              level := Sqrt(FRmsEnv);
            end
          else
            level := absx;

          // Envelope follower (attack/release).
          env := FEnv;
          if (level > env) then
            env := atkC * env + (1 - atkC) * level
          else
            env := relC * env + (1 - relC) * level;
          FEnv := env;

          // Compressor curve.
          compGain := 1.0;
          if Settings.CompEnabled then
            begin

              // Map env to dB.
              if (env < 1e-10) then
                compGain := 1.0
              else
                begin
                  // in dB
                  // outDb = soft knee curve; gainDb = outDb - inDb
                  // gainLin = 10^(gainDb/20)
                  // Use env (not x) so compressor is level-based
                  inDb := LinToDb(env);
                  outDb := SoftKneeGainDb(inDb,
                                          Settings.CompThresholdDb,
                                          Max(1.0,
                                              Settings.CompRatio),
                                          Max(0.0,
                                              Settings.CompKneeDb));
                  gainDb := outDb - inDb;
                  compGain := DbToLin(gainDb);
                end;
            end;

      // Smooth comp gain a bit (avoid zipper)
      FCompGain := 0.98 * FCompGain + 0.02 * compGain;

      // Apply compressor + makeup to delayed sample (y)
      y := y * FCompGain * makeup;

      // Limiter (simple peak clamp with release smoothing)
      limGain := 1.0;
      if Settings.LimEnabled then
        begin

          absx := Abs(y);
          if (absx > ceilingLin) then
            limGain := ceilingLin / absx
          else
            limGain := 1.0;
        end;

      // release smoothing for limiter gain
      FLimGain := 0.995 * FLimGain + 0.005 * limGain;

      // meters
      FCompGRdB := LinToDb(Max(1e-10, FCompGain));
      FLimGRdB := LinToDb(Max(1e-10, FLimGain));

      SamplePtr(n)^ := (y * FLimGain) - DENORM;
    end;

    Exit;
  end;

  // Multi-channel
  for n := 0 to Frames - 1 do
    begin

      // Channel-independent detector uses max abs across channels.
      level := 0.0;
      for ch := 0 to Channels - 1 do
        begin

          x := SamplePtr(idx + ch)^ + DENORM;
          level := Max(level,
                       Abs(x));
        end;

      if Settings.RmsDetector then
        begin

          FRmsEnv := 0.99 * FRmsEnv + 0.01 * (level * level);
          level := Sqrt(FRmsEnv);
        end;

      env := FEnv;
      if (level > env) then
        env := atkC * env + (1 - atkC) * level
      else
        env := relC * env + (1 - relC) * level;
      FEnv := env;

      compGain := 1.0;
      if Settings.CompEnabled then
        begin

          if (env > 1e-10) then
            begin

              inDb := LinToDb(env);
              outDb := SoftKneeGainDb(inDb,
                                      Settings.CompThresholdDb,
                                      Max(1.0,
                                          Settings.CompRatio),
                                      Max(0.0,
                                          Settings.CompKneeDb));
              gainDb := outDb - inDb;
              compGain := DbToLin(gainDb);
            end;
        end;

      FCompGain := 0.98 * FCompGain + 0.02 * compGain;

      // Lookahead delay is per-sample interleaved; we delay each channel equally.
      if (lookFrames > 0) then
        begin

          // Read delayed frame and write current frame.
          rdPos := FDelayPos * Channels;
          wrPos := rdPos;
          for ch := 0 to Channels - 1 do
            begin

              delayedIdx := idx + ch;
              y := FDelay[rdPos + ch];
              FDelay[wrPos + ch] := SamplePtr(delayedIdx)^;
              SamplePtr(delayedIdx)^ := y; // Temporarily store delayed signal in-place.
            end;

        Inc(FDelayPos);
        if (FDelayPos >= lookFrames) then
          FDelayPos := 0;
        end;

      // Apply comp+makeup to current frame (delayed or original).
      for ch := 0 to Channels - 1 do
        SamplePtr(idx + ch)^ := (SamplePtr(idx + ch)^ * FCompGain * makeup);

      // Limiter gain from max channel abs.
      limGain := 1.0;
      if Settings.LimEnabled then
        begin

          level := 0.0;
          for ch := 0 to Channels - 1 do
            level := Max(level, Abs(SamplePtr(idx + ch)^));
          if (level > ceilingLin) then
            limGain := ceilingLin / level
          else
            limGain := 1.0;
        end;

      FLimGain := 0.995 * FLimGain + 0.005 * limGain;

      FCompGRdB := LinToDb(Max(1e-10,
                               FCompGain));

      FLimGRdB := LinToDb(Max(1e-10,
                              FLimGain));

      // apply limiter
      for ch := 0 to Channels - 1 do
        SamplePtr(idx + ch)^ := (SamplePtr(idx + ch)^ * FLimGain) - DENORM;

      Inc(idx, Channels);
    end;
end;



function TAudioDynamicsDSP.GetCompGRdB: Single;
begin

  // Gain itself is <=1, so dB is <=0
  Result := FCompGRdB;
end;


function TAudioDynamicsDSP.GetLimGRdB: Single;
begin

  Result := FLimGRdB;
end;


{ TMfCompressorLimiterMFT }

constructor TMfCompressorLimiterMFT.Create();
var
  S: TDynamicsSettings;

begin

  inherited Create();

  InterlockedExchange(FEnabledInt,
                      0);

  InterlockedExchange(FTruePeakEnabledInt,
                      0);

  // Point at a stable settings snapshot buffer. We never allocate/free settings
  // records at runtime; SetSettings writes into the inactive buffer and swaps.
  FSettingsPtr := nil;

  FDsp := TAudioDynamicsDSP.Create();

  // Defaults requested:
  // True-peak guard optional, but we expose it like EQ/Flanger.
  SetTruePeakEnabled(False);
  SetTruePeakCeilingDbTP(-1.0);
  SetTruePeakOversample(4);

  // Initialize settings cleanly
  FillChar(S, SizeOf(S), 0);
  FDsp.Settings := FDsp.Settings; // keep its defaults
  FLastSampleRate := 0;
  FLastChannels := 0;

  // True-peak defaults required: -1.0 dBTP, 4x.
  FTruePeakCeilingDbTP := -1.0;
  FTruePeakOS := 4;

  FTPGain := 1.0;
  FTPAttackMs := 3.0;
  FTPReleaseMs := 80.0;

  // Initial settings snapshot (lock-free, double-buffered)
  FSettingsA := FDsp.Settings;
  FSettingsB := FSettingsA;
  InterlockedExchangePointer(FSettingsPtr,
                             @FSettingsA);

end;


destructor TMfCompressorLimiterMFT.Destroy();
begin

  FreeAndNil(FDsp);

  inherited;
end;


procedure TMfCompressorLimiterMFT.ProcessAudioFloat32(pData: PSingle;
                                                      Frames,
                                                      Channels,
                                                      SampleRate: Integer);
var
  EnabledFx: Boolean;
  EnabledTP: Boolean;
  P: Pointer;
  S: TDynamicsSettings;

begin

  if (pData = nil) or
     (Frames <= 0) or
     (Channels <= 0) then
    Exit;

  EnabledFx := (InterlockedCompareExchange(FEnabledInt,
                                          0,
                                          0) <> 0);

  EnabledTP := (InterlockedCompareExchange(FTruePeakEnabledInt,
                                          0,
                                          0) <> 0);

  // True bypass: touch nothing.
  if (not EnabledFx) and (not EnabledTP) then
    Exit;

  // Configure DSP on format change.
  if (SampleRate <> FLastSampleRate) or (Channels <> FLastChannels) then
    begin

      FLastSampleRate := SampleRate;
      FLastChannels := Channels;
      FTPGain := 1.0;

      FDsp.ConfigureFormat(SampleRate,
                           Channels);
    end;

  // Snapshot settings (lock-free).
  FillChar(S,
           SizeOf(S),
           0);

  P := Pointer(InterlockedCompareExchangePointer(FSettingsPtr,
                                                 nil,
                                                 nil));

  if (P <> nil) then
    S := PDynamicsSettings(P)^;

  FDsp.Settings := S;

  if EnabledFx then
    FDsp.ProcessFloat32(pData,
                        Frames,
                        Channels);

  if EnabledTP then
    ApplyTruePeakLimiter(pData,
                         Frames,
                         Channels,
                         SampleRate);
end;


procedure TMfCompressorLimiterMFT.EnableFX(const AEnabled: Boolean);
begin

  if AEnabled then
    InterlockedExchange(FEnabledInt,
                        1)
  else
    InterlockedExchange(FEnabledInt,
                        0);
end;


procedure TMfCompressorLimiterMFT.SetSettings(const S: TDynamicsSettings);
var
  PActive: Pointer;
  PNew: PDynamicsSettings;

begin

  // Write into the inactive buffer and atomically swap.
  PActive := Pointer(InterlockedCompareExchangePointer(FSettingsPtr,
                                                       nil,
                                                       nil));

  if (PActive = @FSettingsA) then
    PNew := @FSettingsB
  else
    PNew := @FSettingsA;

  PNew^ := S;

  // Clamp dangerous values.
  PNew^.CompRatio := EnsureRange(PNew^.CompRatio,
                                 1.0,
                                 50.0);

  PNew^.CompAttackMs := EnsureRange(PNew^.CompAttackMs,
                                    0.1,
                                    500.0);

  PNew^.CompReleaseMs := EnsureRange(PNew^.CompReleaseMs,
                                     1.0,
                                     5000.0);

  PNew^.CompKneeDb := EnsureRange(PNew^.CompKneeDb,
                                  0.0,
                                  24.0);

  PNew^.CompThresholdDb := EnsureRange(PNew^.CompThresholdDb,
                                       -60.0,
                                       0.0);

  PNew^.CompMakeupDb := EnsureRange(PNew^.CompMakeupDb,
                                    0.0,
                                    24.0);

  PNew^.LimCeilingDb := EnsureRange(PNew^.LimCeilingDb,
                                    -24.0,
                                    0.0);

  PNew^.LimReleaseMs := EnsureRange(PNew^.LimReleaseMs,
                                    1.0,
                                    5000.0);

  PNew^.LimLookaheadMs := EnsureRange(PNew^.LimLookaheadMs,
                                      0.0,
                                      20.0);

  InterlockedExchangePointer(FSettingsPtr,
                             PNew);

  // Do not hard-reset FTPGain here; it causes audible jumps.
  // Let the limiter's release behavior return it smoothly.

  // If lookahead changed, reconfigure buffer
  if (FLastSampleRate > 0) and (FLastChannels > 0) then
    begin

      if Abs(PNew^.LimLookaheadMs - FLastLookaheadMs) > 0.01 then
        begin

          FLastLookaheadMs := PNew^.LimLookaheadMs;
          FDsp.ConfigureFormat(FLastSampleRate, FLastChannels);
        end;
  end;
end;


procedure TMfCompressorLimiterMFT.GetSettings(out S: TDynamicsSettings);
var
  P: Pointer;

begin

  FillChar(S,
           SizeOf(S),
           0);

  P := Pointer(InterlockedCompareExchangePointer(FSettingsPtr,
                                                 nil,
                                                 nil));

  if (P <> nil) then
    S := PDynamicsSettings(P)^;
end;


procedure TMfCompressorLimiterMFT.EnableTruePeakGuard(const AEnabled: Boolean);
begin

  if AEnabled then
    InterlockedExchange(FTruePeakEnabledInt,
                        1)
  else
    InterlockedExchange(FTruePeakEnabledInt,
                        0);
end;


procedure TMfCompressorLimiterMFT.SetTruePeakCeilingDbTP(const DbTP: Single);
begin

  // Keep it sane; your requirement is -1.0 dBTP default
  FTruePeakCeilingDbTP := EnsureRange(DbTP,
                                      -24.0,
                                      0.0);
end;


procedure TMfCompressorLimiterMFT.SetTruePeakOversample(const Factor: Integer);
begin

  case Factor of
    2,
    4,
    8: FTruePeakOS := Factor;
  else
    FTruePeakOS := 4;
  end;
end;


function TMfCompressorLimiterMFT.GetCompressorGRdB: Single;
begin

  Result := FDsp.GetCompGRdB;
end;


function TMfCompressorLimiterMFT.GetLimiterGRdB: Single;
begin

  Result := FDsp.GetLimGRdB;
end;

end.

