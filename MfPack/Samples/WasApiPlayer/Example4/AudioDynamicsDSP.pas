// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: AudioDynamicsDSP.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description:  - Feed-forward compressor (peak detector, linked stereo)
//               - Limiter with optional soft knee, lookahead, and selectable detector (Peak/RMS)
//
// Notes:
//  - Designed for real-time use in the WASAPI render thread.
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
unit AudioDynamicsDSP;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Math;

type

  // Interleaved buffer sample format (matches PcmLib conversions)
  TSampleFormat = (sfFloat32,
                   sfInt16,
                   sfInt24,
                   sfInt32);

  TLimiterDetector = (ldPeak,
                      ldRms);

  TDynamicsSettings = record
    // Compressor.
    CompEnabled      : Boolean;
    CompThresholdDb  : Integer;  // negative
    CompRatioX10     : Integer;  // e.g. 40 = 4.0
    CompAttackMs     : Integer;
    CompReleaseMs    : Integer;
    CompMakeupDb     : Integer;
    CompAutoMakeup   : Boolean;

    // Limiter.
    LimEnabled       : Boolean;
    LimCeilingDb     : Integer;  // negative
    LimReleaseMs     : Integer;
    LimLookaheadMs   : Integer;
    LimKneeDb        : Integer;  // 0 = hard
    LimDetector      : TLimiterDetector;
    LimRmsWindowMs   : Integer;

    // True peak (Catmull-Rom interpolation between samples).
    LimTruePeak      : Boolean;
    LimOversample    : Integer; // 1,2,4,...

    class function Defaults: TDynamicsSettings; static;
  end;

  TAudioDynamicsDSP = class
  private

    FSampleRate: Integer;
    FChannels: Integer;

    // Settings
    FSettings: TDynamicsSettings;

    // Scratch float buffer for int<->float processing
    FDynFloatBuf: PSingle;
    FDynFloatBufSamples: Integer;


    // If True, the DSP must never allocate memory while processing (realtime-safe).
    FNoAllocRealtime: Boolean;
    // Compressor state
    FCompGain: Single;
    FCompAtkCoeff: Single;
    FCompRelCoeff: Single;
    FCompThrLin: Single;
    FRatio: Single;
    FCompMakeupLin: Single;

    // Limiter state
    FLimGain: Single;
    FLimRelCoeff: Single;
    FLimCeilLin: Single;
    FActiveLimOS: Integer; // last applied true-peak oversample factor

    // Lookahead buffer (stores post-compressor samples)
    FDelayBuf: array of Single;
    FDelayPos: Integer;      // in frames
    FDelaySamples: Integer;  // in frames

    // RMS detector state
    FRmsEnv: Single;
    FRmsCoeff: Single;

    // Meters (milli-dB, atomic)
    FCompGRmDb: Integer;
    FLimGRmDb: Integer;

    // True-peak history (per channel) on post-compressor samples
    FHist0: array of Single; // x[n-2]
    FHist1: array of Single; // x[n-1]
    FHist2: array of Single; // x[n]

    procedure UpdateCoefficientsAndBuffers();
    procedure ResetLimiterState();
    procedure SetFormat(ASampleRate,
                        AChannels: Integer);

    function DbToLin(const dB: Single): Single; inline;
    function LinToDb(const x: Single): Single; inline;

    function CatmullRom(const p0,
                        p1,
                        p2,
                        p3,
                        t: Single): Single; inline;

    function TruePeakAbs(const a,
                         b,
                         c,
                         d: Single;
                          OS: Integer): Single;

    function SoftKneeOverDb(const OverDb: Single;
                            const KneeDb: Single): Single; inline;

  public

    constructor Create(aSamplesPerSec: DWord;
                       aChannels: DWord); reintroduce;

    destructor Destroy(); override;

    procedure SetSettings(const S: TDynamicsSettings);
    procedure Reset();


    // Preallocate any scratch buffers needed for realtime processing.
    // Call this once after you know the max frames per WASAPI buffer.
    procedure PreallocRealtimeBuffers(MaxFrames: Integer; Format: TSampleFormat);

    // When enabled (default), ProcessInterleaved will never allocate.
    procedure SetRealtimeNoAlloc(Value: Boolean);
    //
    procedure EnsureFloatBuf(SamplesNeeded: Integer);
    // Core processing on normalized float [-1..1) interleaved.
    procedure ProcessFloat32Interleaved(Buffer: PSingle;
                                        Frames: Integer);

    // Generic entry point: supports Float32 + PCM int16/24/32 via internal conversion (PcmLib).
    procedure ProcessInterleaved(Buffer: Pointer;
                                 Frames: Integer;
                                 Format: TSampleFormat);

    function GetCompressorGRdB(): Single;
    function GetLimiterGRdB(): Single;
  end;

implementation

uses
  PcmLib;

{ TDynamicsSettings }

class function TDynamicsSettings.Defaults(): TDynamicsSettings;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  // Compressor defaults
  Result.CompEnabled     := False;
  Result.CompThresholdDb := -18;
  Result.CompRatioX10    := 40;   // 4:1
  Result.CompAttackMs    := 10;
  Result.CompReleaseMs   := 150;
  Result.CompMakeupDb    := 0;
  Result.CompAutoMakeup  := True;

  // Limiter defaults
  Result.LimEnabled      := True;
  Result.LimCeilingDb    := -1;
  Result.LimReleaseMs    := 60;
  Result.LimLookaheadMs  := 5;
  Result.LimKneeDb       := 3;
  Result.LimDetector     := ldPeak;
  Result.LimRmsWindowMs  := 50;

  // True peak defaults
  Result.LimTruePeak     := True;
  Result.LimOversample   := 4;
end;

{ TAudioDynamicsDSP }

constructor TAudioDynamicsDSP.Create(aSamplesPerSec: DWord;
                                     aChannels: DWord);
begin

  inherited Create();

  FDynFloatBuf := nil;
  FDynFloatBufSamples := 0;
  FNoAllocRealtime := True;
  FActiveLimOS := 0; // Force first-time apply.

  FSettings := TDynamicsSettings.Defaults;

  SetFormat(aSamplesPerSec,
            aChannels);

  ResetLimiterState();
end;



procedure TAudioDynamicsDSP.SetRealtimeNoAlloc(Value: Boolean);
begin

  FNoAllocRealtime := Value;
end;


procedure TAudioDynamicsDSP.EnsureFloatBuf(SamplesNeeded: Integer);
begin
  if SamplesNeeded <= FDynFloatBufSamples then
    Exit;

  if FNoAllocRealtime then
    Exit; // realtime-safe: skip allocation, caller will bypass processing

  ReallocMem(FDynFloatBuf, SamplesNeeded * SizeOf(Single));
  FDynFloatBufSamples := SamplesNeeded;
end;


procedure TAudioDynamicsDSP.PreallocRealtimeBuffers(MaxFrames: Integer; Format: TSampleFormat);
var
  sampleCount: Integer;
begin
  if (MaxFrames <= 0) or (FChannels <= 0) then
    Exit;

  // Only needed when we may convert PCM ints <-> float.
  if (Format <> sfFloat32) then
  begin
    sampleCount := MaxFrames * FChannels;
    // Temporarily allow allocation while preallocating.
    // (Prealloc is called from init/config code, not the audio thread.)
    // We still respect FNoAllocRealtime afterwards during processing.
    ReallocMem(FDynFloatBuf, sampleCount * SizeOf(Single));
    FDynFloatBufSamples := sampleCount;
  end;
end;

destructor TAudioDynamicsDSP.Destroy();
begin

  if Assigned(FDynFloatBuf) then
    begin

      FreeMem(FDynFloatBuf);
      FDynFloatBuf := nil;
      FDynFloatBufSamples := 0;
    end;

  inherited Destroy();
end;


procedure TAudioDynamicsDSP.SetFormat(ASampleRate,
                                      AChannels: Integer);
begin

  FSampleRate := ASampleRate;
  FChannels := Max(1,
                   AChannels);

  // Allocate per-channel true-peak history (no allocations in Process*)
  SetLength(FHist0,
            FChannels);

  SetLength(FHist1,
            FChannels);

  SetLength(FHist2,
            FChannels);

  FActiveLimOS := 0; // force re-apply on next Process

  UpdateCoefficientsAndBuffers();
  ResetLimiterState();
end;


procedure TAudioDynamicsDSP.SetSettings(const S: TDynamicsSettings);
begin

  // NOTE: This is expected to be called from the GUI/control thread.
  // It may reallocate buffers (lookahead), which is why we keep it out of Process*.
  FSettings := S;

  if (FSampleRate > 0) and (FChannels > 0) then
    UpdateCoefficientsAndBuffers();
end;


procedure TAudioDynamicsDSP.Reset();
begin

  ResetLimiterState();
end;


procedure TAudioDynamicsDSP.UpdateCoefficientsAndBuffers();
var
  atkMs, relMs: Integer;
  limRelMs: Integer;
  rmsMs: Integer;
  lookMs: Integer;

begin

  if (FSampleRate <= 0) or (FChannels <= 0) then
    Exit;

  // Cached linear thresholds / ratios
  FCompThrLin := DbToLin(FSettings.CompThresholdDb);
  FLimCeilLin := DbToLin(FSettings.LimCeilingDb);
  FRatio := Max(1.0,
                FSettings.CompRatioX10 / 10.0);

  // Makeup gain constant multiplier.
  if FSettings.CompAutoMakeup then
    // simple (useful) approximation: half of expected GR at threshold for the ratio
    FCompMakeupLin := DbToLin(0.5 * (-FSettings.CompThresholdDb) * (1.0 - 1.0 / FRatio))
  else
    FCompMakeupLin := DbToLin(FSettings.CompMakeupDb);

  // Attack/release coefficients (one-pole).
  atkMs := Max(1,
               FSettings.CompAttackMs);

  relMs := Max(1,
               FSettings.CompReleaseMs);

  limRelMs := Max(1,
                  FSettings.LimReleaseMs);

  // coeff = exp(-1/(tau*Fs)), tau = ms/1000.
  FCompAtkCoeff := Exp(-1.0 / (0.001 * atkMs * FSampleRate));
  FCompRelCoeff := Exp(-1.0 / (0.001 * relMs * FSampleRate));
  FLimRelCoeff  := Exp(-1.0 / (0.001 * limRelMs * FSampleRate));

  // RMS detector coefficient (attackless windowed IIR).
  rmsMs := Max(5,
               FSettings.LimRmsWindowMs);
  FRmsCoeff := Exp(-1.0 / (0.001 * rmsMs * FSampleRate));

  // Lookahead buffer (frames).
  lookMs := Max(0,
                FSettings.LimLookaheadMs);

  FDelaySamples := (FSampleRate * lookMs) div 1000;
  if (FDelaySamples < 0) then
    FDelaySamples := 0;

  if FDelaySamples > 0 then
    SetLength(FDelayBuf,
              FDelaySamples * FChannels)
  else
    SetLength(FDelayBuf, 0);

  if (FDelaySamples = 0) then
    FDelayPos := 0
  else
    if (FDelayPos >= FDelaySamples) then
      FDelayPos := 0;
end;


procedure TAudioDynamicsDSP.ResetLimiterState();
var
  ch: Integer;

begin

  FLimGain := 1.0;
  FRmsEnv := 0.0;
  FDelayPos := 0;

  if Length(FDelayBuf) > 0 then
    FillChar(FDelayBuf[0],
             Length(FDelayBuf) * SizeOf(Single),
             0);

  for ch := 0 to Length(FHist0) - 1 do
    begin

      FHist0[ch] := 0.0;
      FHist1[ch] := 0.0;
      FHist2[ch] := 0.0;
    end;

  InterlockedExchange(FLimGRmDb,
                      0);
end;



function TAudioDynamicsDSP.DbToLin(const dB: Single): Single;
begin

  Result := Power(10.0,
                  dB / 20.0);
end;


function TAudioDynamicsDSP.LinToDb(const x: Single): Single;
begin

  if (x <= 1e-20) then
    Result := -200.0
  else
    Result := 20.0 * Log10(x);
end;


function TAudioDynamicsDSP.SoftKneeOverDb(const OverDb: Single;
                                          const KneeDb: Single): Single;
var
  halfK: Single;
  t: Single;

begin

  if (KneeDb <= 0) then
    Exit(Max(0.0,
             OverDb));

  halfK := KneeDb * 0.5;

  if (OverDb <= -halfK) then
    Result := 0.0
  else
    if (OverDb >= halfK) then
      Result := OverDb
  else
    begin

      t := OverDb + halfK; // 0..K
      Result := (t * t) / (2.0 * KneeDb);
    end;
end;


function TAudioDynamicsDSP.CatmullRom(const p0,
                                      p1,
                                      p2,
                                      p3,
                                      t: Single): Single;
var
  t2,
  t3: Single;

begin

  t2 := t * t;
  t3 := t2 * t;
  Result := 0.5 * ((2.0 * p1) +
                   (-p0 + p2) * t +
                   (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
                   (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3);
end;


function TAudioDynamicsDSP.TruePeakAbs(const a,
                                       b,
                                       c,
                                       d: Single;
                                       OS: Integer): Single;
var
  i: Integer;
  t,
  v: Single;

begin

  Result := Max(Abs(b),
                Abs(c));

  if (OS <= 1) then
    Exit;

  for i := 1 to OS - 1 do
    begin

      t := i / OS;
      v := CatmullRom(a,
                      b,
                      c,
                      d,
                      t);

      Result := Max(Result,
                    Abs(v));
    end;
end;


procedure TAudioDynamicsDSP.ProcessFloat32Interleaved(Buffer: PSingle;
                                                      Frames: Integer);
var
  i,
  ch: Integer;
  x,
  y,
  xd: Single;
  peakIn: Single;
  peakComp: Single;
  det: Single;
  compTarget: Single;
  limTarget: Single;
  overDb,
  overSoftDb: Single;
  os: Integer;
  UseDelay: Boolean;
  DelayIndex: Integer;

  // Pointers for this frame.
  pFrame: PSingle;
  pS: PSingle;

  // Precomputed comp applied gain for this sample (linked).
  compApplied: Single;

begin

  if (Buffer = nil) then
    Exit;

  if (Frames <= 0) then
    Exit;

  if (FSampleRate <= 0) or (FChannels <= 0) then
    Exit;

  os := 1;
  if FSettings.LimEnabled and FSettings.LimTruePeak then
    os := Max(1,
              FSettings.LimOversample);

  // Normalize to supported factors (if you only support 1/2/4).
  if (os <> 1) and
     (os <> 2) and
     (os <> 4) then
    os := 1;

  // REBUILD if changed (both 2->4 and 4->2)
  if (os <> FActiveLimOS) then
    begin

      FActiveLimOS := os;
      ResetLimiterState();
    end;

  UseDelay := FSettings.LimEnabled and
              (FDelaySamples > 0) and
              (Length(FDelayBuf) > 0);

  for i := 0 to Frames - 1 do
    begin

      // Pointer to first sample of this frame (interleaved).
      pFrame := Buffer;
      Inc(pFrame,
          i * FChannels);

      // ---------------------------------------------------------
      // Input peak (linked)
      // ---------------------------------------------------------
      peakIn := 0.0;
      pS := pFrame;

      for ch := 0 to FChannels - 1 do
        begin

          x := pS^;
          if (x < 0) then
            x := -x;
          if (x > peakIn) then
            peakIn := x;
          Inc(pS);
        end;

      // ---------------------------------------------------------
      // Compressor (feed-forward, linked peak)
      // ---------------------------------------------------------
      compTarget := 1.0;
      if (FSettings.CompEnabled and (peakIn > FCompThrLin)) then
        compTarget := DbToLin(-(LinToDb(peakIn) - FSettings.CompThresholdDb) * (1.0 - 1.0 / FRatio));

      if (compTarget < FCompGain) then
        FCompGain := FCompAtkCoeff * FCompGain + (1.0 - FCompAtkCoeff) * compTarget
      else
        FCompGain := FCompRelCoeff * FCompGain + (1.0 - FCompRelCoeff) * compTarget;

      InterlockedExchange(FCompGRmDb,
                          Round((-LinToDb(FCompGain)) * 1000.0));

      compApplied := (FCompGain * FCompMakeupLin);

      // ---------------------------------------------------------
      // Post-compressor peak for limiter detector
      // ---------------------------------------------------------
      peakComp := 0.0;
      pS := pFrame;

      for ch := 0 to FChannels - 1 do
        begin

          x := pS^ * compApplied;
          y := x;
          if( y < 0) then
            y := -y;
          if (y > peakComp) then
            peakComp := y;
          Inc(pS);
        end;

      // ---------------------------------------------------------
      // Limiter detector (peak or RMS)
      // ---------------------------------------------------------
      if (FSettings.LimDetector = ldRms) then
        begin

          FRmsEnv := FRmsCoeff * FRmsEnv + (1.0 - FRmsCoeff) * (peakComp * peakComp);
          det := Sqrt(FRmsEnv);
        end
      else
        det := peakComp;

      // ---------------------------------------------------------
      // True-peak refine (Catmull-Rom interpolation)
      // Uses history per channel: FHist0/FHist1/FHist2 + current x
      // ---------------------------------------------------------
      if FSettings.LimEnabled and
         FSettings.LimTruePeak and
         (os > 1) then
        begin

          pS := pFrame;
          for ch := 0 to FChannels - 1 do
            begin

              x := pS^ * compApplied;
              y := TruePeakAbs(FHist0[ch],
                               FHist1[ch],
                               FHist2[ch],
                               x,
                               os);
              if (y > det) then
                det := y;
              Inc(pS);
            end;
        end;

      // ---------------------------------------------------------
      // Limiter gain (soft knee in dB domain)
      // ---------------------------------------------------------
      limTarget := 1.0;
      if FSettings.LimEnabled and (det > 0.0) then
        begin

          overDb := LinToDb(det) - FSettings.LimCeilingDb;
          overSoftDb := SoftKneeOverDb(overDb,
                                       FSettings.LimKneeDb);

          if (overSoftDb > 0.0) then
            limTarget := DbToLin(-overSoftDb)
          else
            limTarget := 1.0;

          if ((det * limTarget) > FLimCeilLin) then
            limTarget := FLimCeilLin / det;
        end;

      if not FSettings.LimEnabled then
        FLimGain := 1.0
      else
        if (limTarget < FLimGain) then
          FLimGain := limTarget
      else
        FLimGain := FLimRelCoeff * FLimGain + (1.0 - FLimRelCoeff) * limTarget;

      InterlockedExchange(FLimGRmDb,
                          Round((-LinToDb(FLimGain)) * 1000.0));

      // ---------------------------------------------------------
      // Apply with optional lookahead delay
      // ---------------------------------------------------------
      if UseDelay then
        begin

          DelayIndex := FDelayPos * FChannels;

          pS := pFrame;
          for ch := 0 to FChannels - 1 do
            begin

              // delayed (already post-comp)
              xd := FDelayBuf[DelayIndex + ch];

              // current post-comp into delay
              x := pS^ * compApplied;
              FDelayBuf[DelayIndex + ch] := x;

              // output delayed with limiter
              pS^ := xd * FLimGain;

              // Update history with current post-comp sample
              FHist0[ch] := FHist1[ch];
              FHist1[ch] := FHist2[ch];
              FHist2[ch] := x;

              Inc(pS);
            end;

          Inc(FDelayPos);
          if (FDelayPos >= FDelaySamples) then
            FDelayPos := 0;
        end
      else
        begin

          pS := pFrame;
          for ch := 0 to FChannels - 1 do
            begin

              x := pS^ * compApplied;
              y := x;

              if FSettings.LimEnabled then
                y := y * FLimGain;

              pS^ := y;

              // Update history with current post-comp sample
              FHist0[ch] := FHist1[ch];
              FHist1[ch] := FHist2[ch];
              FHist2[ch] := x;

              Inc(pS);
            end;
        end;
    end;
end;


procedure TAudioDynamicsDSP.ProcessInterleaved(Buffer: Pointer;
                                               Frames: Integer;
                                               Format: TSampleFormat);
var
  sampleCount: Integer;

begin

  if (Buffer = nil) then
    Exit;

  if (Frames <= 0) then
    Exit;

  if (FSampleRate <= 0) or (FChannels <= 0) then
    Exit;

  case Format of
    sfFloat32: ProcessFloat32Interleaved(PSingle(Buffer),
                                         Frames);

    sfInt16,
    sfInt24,
    sfInt32: begin

               sampleCount := Frames * FChannels;
               EnsureFloatBuf(sampleCount);

               if (sampleCount > FDynFloatBufSamples) then
                 Exit;

case Format of
                 sfInt16: Int16ToFloat(PByte(Buffer),
                                       FDynFloatBuf,
                                       sampleCount);

                 sfInt24: Int24ToFloat(PByte(Buffer),
                                       FDynFloatBuf,
                                       sampleCount);

                 sfInt32: Int32ToFloat(PByte(Buffer),
                                       FDynFloatBuf,
                                       sampleCount);
               end;

               ProcessFloat32Interleaved(FDynFloatBuf,
                                         Frames);

               case Format of
                 sfInt16: FloatToInt16(FDynFloatBuf,
                                       PByte(Buffer),
                                       sampleCount);

                 sfInt24: FloatToInt24(FDynFloatBuf,
                                       PByte(Buffer),
                                       sampleCount);

                 sfInt32: FloatToInt32(FDynFloatBuf,
                                       PByte(Buffer),
                                       sampleCount);
               end;
             end;
  else
    ; // unknown -> bypass.
  end;
end;


function TAudioDynamicsDSP.GetCompressorGRdB(): Single;
var
  v: Integer;

begin

  v := InterlockedExchangeAdd(FCompGRmDb,
                              0);
  Result := v / 1000.0;
end;


function TAudioDynamicsDSP.GetLimiterGRdB(): Single;
var
  v: Integer;

begin

  v := InterlockedExchangeAdd(FLimGRmDb,
                              0);
  // DEBUG:
  //OutputDebugString(PChar(Format('>>>>> LimiterGRdB=%v', [v / 1000.0])));

  Result := v / 1000.0;
end;

end.
