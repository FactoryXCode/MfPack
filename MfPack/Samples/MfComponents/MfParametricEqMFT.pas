// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfParametricEqMFT.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Parametric EQ implemented as an audio MFT.
//              Controls: gain, center freq, Q/bandwidth (octaves), smoothing/ramp,
//                        coefficient inspection.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
unit MfParametricEqMFT;

interface

uses

  WinApi.Windows,
  System.SysUtils,
  System.Math,
  System.SyncObjs,
  WinApi.MediaFoundationApi.MfTransform,

  PcmLib,
  MfAudioEffectMFTBase;

type

  // Control interface (like your IMfHighMidLowControl style)
  IMfParametricEqControl = interface(IInterface)
    ['{2B64C6C1-5A4B-4D71-9F5F-9CE39C2C0D71}']
    procedure EnableEQ(const AEnabled: Boolean); stdcall;

    procedure SetGainDb(const GainDb: Single); stdcall;          // -24..+24
    procedure SetCenterFreqHz(const Hz: Single); stdcall;        // 20..0.45*Fs
    procedure SetQ(const Q: Single); stdcall;                    // 0.2..12
    procedure SetBandwidthOctaves(const Oct: Single); stdcall;   // e.g. 0.1..4

    procedure SetRampMode(const Mode: TMfRampMode); stdcall;
    procedure SetRampTimeMs(const Ms: Integer); stdcall;         // only for rmManual

    // True-peak guard
    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;
    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;   // default -1.0
    procedure SetTruePeakOversample(const Factor: Integer); stdcall; // 2/4/8
  end;

  // Inspection interface (coeffs + params snapshots)
  IMfParametricEqInspect = interface(IInterface)
    ['{7E2DDE66-9D59-41D6-9E9E-2B4E0A3DFB87}']
    function GetCurrentCoeffs(out C: TBiquadCoeffs; out SampleRate: Double): Boolean; stdcall;
    function GetTargetCoeffs(out C: TBiquadCoeffs; out SampleRate: Double): Boolean; stdcall;
    function GetCurrentParams(out GainDb, FreqHz, Q, BandwidthOct: Double): Boolean; stdcall;
  end;

type
  // Internal biquad state (DF2T)
  TBiquadState = record
    z1, z2: Single;
  end;

  TMfParametricEqMFT = class(TMfAudioEffectMFTBase, IMFTransform, IMfParametricEqControl, IMfParametricEqInspect)
  private
    FLock: TCriticalSection;

    FEQEnabled: Boolean;

    // Target params set from GUI
    FTGainDb: Single;
    FTFreqHz: Single;
    FTQ: Single;
    FTBWOct: Single;

    // Current smoothed params used for coeff calc
    FCGainDb: Single;
    FCFreqHz: Single;
    FCQ: Single;
    FCBWOct: Single;

    FRampMode: TMfRampMode;
    FRampTimeMs: Integer;

    // Cached coeffs (current/target)
    FCoeffCur: TBiquadCoeffs;
    FCoeffTgt: TBiquadCoeffs;

    // Mono optimization + per-channel states
    FStates: array of TBiquadState; // length = channels, or 1 for mono
    FStatesChannels: Integer;

    // last known audio format (for clamping & coeff recompute)
    FLastSampleRate: Integer;
    FLastChannels: Integer;

    procedure EnsureStateForChannels(const Channels: Integer);
    function ClampGainDb(const v: Single): Single;
    function ClampQ(const v: Single): Single;
    function ClampFreqHz(const v: Single; const SampleRate: Integer): Single;

    function BWOctToQ(const BWOct: Single; const FreqHz: Single; const SampleRate: Integer): Single;

    procedure CalcPeakingCoeffs(const GainDb, FreqHz, Q: Double; const SampleRate: Integer; out C: TBiquadCoeffs);

    procedure UpdateCoeffsAndSmoothing(const Frames, Channels, SampleRate: Integer);
    function RampCoefPerBlock(const Frames, SampleRate: Integer): Single;

    function ProcessSampleDF2T(var S: TBiquadState; const x: Single; const C: TBiquadCoeffs): Single; inline;

  protected

    procedure ProcessAudioFloat32(pData: PSingle; Frames, Channels, SampleRate: Integer); override;

  public

    constructor Create;
    destructor Destroy; override;

    // IMfParametricEqControl
    procedure EnableEQ(const AEnabled: Boolean); stdcall;

    procedure SetGainDb(const GainDb: Single); stdcall;
    procedure SetCenterFreqHz(const Hz: Single); stdcall;
    procedure SetQ(const Q: Single); stdcall;
    procedure SetBandwidthOctaves(const Oct: Single); stdcall;

    procedure SetRampMode(const Mode: TMfRampMode); stdcall;
    procedure SetRampTimeMs(const Ms: Integer); stdcall;

    procedure EnableTruePeakGuard(const AEnabled: Boolean); stdcall;
    procedure SetTruePeakCeilingDbTP(const DbTP: Single); stdcall;
    procedure SetTruePeakOversample(const Factor: Integer); stdcall;

    // IMfParametricEqInspect
    function GetCurrentCoeffs(out C: TBiquadCoeffs; out SampleRate: Double): Boolean; stdcall;
    function GetTargetCoeffs(out C: TBiquadCoeffs; out SampleRate: Double): Boolean; stdcall;
    function GetCurrentParams(out GainDb, FreqHz, Q, BandwidthOct: Double): Boolean; stdcall;
  end;


implementation


constructor TMfParametricEqMFT.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;

  FEQEnabled := True;

  // defaults
  FTGainDb := 0;
  FTFreqHz := 1000;
  FTQ := 1.0;
  FTBWOct := 1.0;

  FCGainDb := FTGainDb;
  FCFreqHz := FTFreqHz;
  FCQ := FTQ;
  FCBWOct := FTBWOct;

  FRampMode := rmSmooth;
  FRampTimeMs := 30;

  FLastSampleRate := 0;
  FLastChannels := 0;

  // True-peak defaults required: -1.0 dBTP, 4x
  SetTruePeakEnabled(False);
  SetTruePeakCeilingDbTP(-1.0);
  SetTruePeakOversample(4);
end;

destructor TMfParametricEqMFT.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMfParametricEqMFT.EnsureStateForChannels(const Channels: Integer);
var
  i: Integer;
begin
  if Channels <= 1 then
  begin
    if FStatesChannels <> 1 then
    begin
      SetLength(FStates, 1);
      FStatesChannels := 1;
      FStates[0].z1 := 0;
      FStates[0].z2 := 0;
    end;
    Exit;
  end;

  if FStatesChannels <> Channels then
  begin
    SetLength(FStates, Channels);
    FStatesChannels := Channels;
    for i := 0 to Channels - 1 do
    begin
      FStates[i].z1 := 0;
      FStates[i].z2 := 0;
    end;
  end;
end;

function TMfParametricEqMFT.ClampGainDb(const v: Single): Single;
begin
  Result := EnsureRange(v, -24.0, 24.0);
end;

function TMfParametricEqMFT.ClampQ(const v: Single): Single;
begin
  Result := EnsureRange(v, 0.2, 12.0);
end;

function TMfParametricEqMFT.ClampFreqHz(const v: Single; const SampleRate: Integer): Single;
var
  hi: Single;
begin
  hi := SampleRate * 0.45;
  Result := EnsureRange(v, 20.0, hi);
end;

function TMfParametricEqMFT.BWOctToQ(const BWOct, FreqHz: Single; const SampleRate: Integer): Single;
var
  w0, sw: Double;
  x: Double;
begin
  // Cookbook conversion:
  // Q = 1 / (2*sinh( ln(2)/2 * BW * w0/sin(w0) ))
  if (BWOct <= 0.0001) or (SampleRate <= 0) then Exit(1.0);

  w0 := 2.0 * PI * (FreqHz / SampleRate);
  sw := Sin(w0);
  if Abs(sw) < 1e-9 then Exit(1.0);

  x := (Ln(2.0) / 2.0) * BWOct * (w0 / sw);
  Result := (1.0 / (2.0 * Sinh(x))) * 1.0;
  Result := ClampQ(Result);
end;

procedure TMfParametricEqMFT.CalcPeakingCoeffs(const GainDb, FreqHz, Q: Double;
  const SampleRate: Integer; out C: TBiquadCoeffs);
var
  A, w0, cw, sw, alpha: Double;
  b0, b1, b2, a0, a1, a2: Double;
begin
  // RBJ peaking EQ (a0 normalized to 1)
  A := Power(10.0, GainDb / 40.0);
  w0 := 2.0 * PI * (FreqHz / SampleRate);
  cw := Cos(w0);
  sw := Sin(w0);
  alpha := sw / (2.0 * Q);

  b0 := 1.0 + alpha * A;
  b1 := -2.0 * cw;
  b2 := 1.0 - alpha * A;
  a0 := 1.0 + alpha / A;
  a1 := -2.0 * cw;
  a2 := 1.0 - alpha / A;

  b0 := b0 / a0;
  b1 := b1 / a0;
  b2 := b2 / a0;
  a1 := a1 / a0;
  a2 := a2 / a0;

  C.b0 := b0;
  C.b1 := b1;
  C.b2 := b2;
  C.a1 := a1;
  C.a2 := a2;
end;

function TMfParametricEqMFT.RampCoefPerBlock(const Frames,
                                             SampleRate: Integer): Single;
var
  ms: Integer;
  tau: Double;
  aPerSample: Double;

begin

  case FRampMode of
    rmOff: Exit(0.0);
    rmFast: ms := 10;
    rmSmooth: ms := 30;
    rmManual: ms := FRampTimeMs;
  else
    ms := 30;
  end;

  if (ms <= 0) then
    Exit(0.0);

  tau := ms / 1000.0;
  aPerSample := Exp(-1.0 / (tau * SampleRate));
  Result := (Power(aPerSample, Frames)) * 1.0;  // per-block coefficient
end;

procedure TMfParametricEqMFT.UpdateCoeffsAndSmoothing(const Frames,
                                                      Channels,
                                                      SampleRate: Integer);
var
  a: Single;
  tgtQ: Single;
  curQ: Single;
  tgtFreq,
  curFreq: Single;
  tgtGain,
  curGain: Single;
  bw: Single;

begin

  // Cache last format
  FLastSampleRate := SampleRate;
  FLastChannels := Channels;

  // snapshot targets
  FLock.Enter();
  try
    tgtGain := ClampGainDb(FTGainDb);
    tgtFreq := ClampFreqHz(FTFreqHz,
                           SampleRate);

    // we expose both; target Q comes from either
    bw := FTBWOct;
    if bw > 0 then
      tgtQ := BWOctToQ(bw, tgtFreq, SampleRate)
    else
      tgtQ := ClampQ(FTQ);

    // also keep the exposed BW octave around for inspection
    // (we will report the current BW as whatever you last set)
  finally
    FLock.Leave();
  end;

  a := RampCoefPerBlock(Frames, SampleRate);

  if a <= 0 then
  begin
    curGain := tgtGain;
    curFreq := tgtFreq;
    curQ := tgtQ;
  end
  else
  begin
    curGain := a * FCGainDb + (1 - a) * tgtGain;
    curFreq := a * FCFreqHz + (1 - a) * tgtFreq;
    curQ := a * FCQ + (1 - a) * tgtQ;
  end;

  // clamp again after smoothing
  curGain := ClampGainDb(curGain);
  curFreq := ClampFreqHz(curFreq, SampleRate);
  curQ := ClampQ(curQ);

  FCGainDb := curGain;
  FCFreqHz := curFreq;
  FCQ := curQ;

  // coeffs (current + target for inspection)
  CalcPeakingCoeffs(curGain, curFreq, curQ, SampleRate, FCoeffCur);
  CalcPeakingCoeffs(tgtGain, tgtFreq, tgtQ, SampleRate, FCoeffTgt);

  EnsureStateForChannels(Channels);
end;

function TMfParametricEqMFT.ProcessSampleDF2T(var S: TBiquadState; const x: Single; const C: TBiquadCoeffs): Single;
var
  y: Single;

begin

  // DF2T
  y := ((C.b0) * x + S.z1) * 1.0;
  S.z1 := ((C.b1) * x - (C.a1) * y + S.z2) * 1.0;
  S.z2 := ((C.b2) * x - (C.a2) * y) * 1.0;
  Result := y;
end;


procedure TMfParametricEqMFT.ProcessAudioFloat32(pData: PSingle;
                                                 Frames,
                                                 Channels,
                                                 SampleRate: Integer);

  function IsFiniteS(const v: Single): Boolean; inline;
  var
    u: Cardinal absolute v;

  begin

    Result := (u and $7F800000) <> $7F800000;
  end;

const
  DENORM: Single = 1e-20;

var
  n, ch: Integer;
  x: Single;
  p: PSingle;

  s: Single;
  u: Cardinal absolute s;

begin

  if (Frames <= 0) or
     (Channels <= 0) or
     (pData = nil) then
    Exit;

  UpdateCoeffsAndSmoothing(Frames,
                           Channels,
                           SampleRate);

  // Use pointer arithmetic instead of "open array" indexing so this works with {$R+}.
  p := pData;

  // Mono fast path
  if (Channels = 1) then
    begin

      for n := 0 to Frames - 1 do
        begin

          x := p^ + DENORM;
          x := ProcessSampleDF2T(FStates[0], x, FCoeffCur) - DENORM;
          p^ := x;
          Inc(p);
        end;

      Exit;
    end;

  // Multi-channel: per-channel state (interleaved frames)
  for n := 0 to Frames - 1 do
    begin

      for ch := 0 to Channels - 1 do
        begin

          s := p^;
          if (not IsFiniteS(s)) then
            s := 0.0;

          x := s + DENORM;
          x := ProcessSampleDF2T(FStates[ch],
                                 x,
                                 FCoeffCur) - DENORM;
          p^ := x;
          Inc(p);
        end;
  end;
end;


{ IMfParametricEqControl }

procedure TMfParametricEqMFT.EnableEQ(const AEnabled: Boolean);
begin

  FEQEnabled := AEnabled;
end;


procedure TMfParametricEqMFT.SetGainDb(const GainDb: Single);
begin

  FLock.Enter();

  try

    FTGainDb := GainDb;
  finally

    FLock.Leave();
  end;
end;


procedure TMfParametricEqMFT.SetCenterFreqHz(const Hz: Single);
begin

  FLock.Enter();

  try

    FTFreqHz := Hz;
  finally

    FLock.Leave();
  end;
end;


procedure TMfParametricEqMFT.SetQ(const Q: Single);
begin

  FLock.Enter();

  try

    FTQ := Q;
    // if user explicitly sets Q, disable BW-oct driving by setting BW to 0
    FTBWOct := 0;
  finally

    FLock.Leave();
  end;
end;


procedure TMfParametricEqMFT.SetBandwidthOctaves(const Oct: Single);
begin

  FLock.Enter();

  try

    FTBWOct := Oct;
  finally

    FLock.Leave();
  end;
end;


procedure TMfParametricEqMFT.SetRampMode(const Mode: TMfRampMode);
begin

  FRampMode := Mode;
end;

procedure TMfParametricEqMFT.SetRampTimeMs(const Ms: Integer);
begin

  FRampTimeMs := EnsureRange(Ms,
                             0,
                             2000);
end;


procedure TMfParametricEqMFT.EnableTruePeakGuard(const AEnabled: Boolean);
begin

  SetTruePeakEnabled(AEnabled);
end;


procedure TMfParametricEqMFT.SetTruePeakCeilingDbTP(const DbTP: Single);
begin

  inherited SetTruePeakCeilingDbTP(DbTP);
end;


procedure TMfParametricEqMFT.SetTruePeakOversample(const Factor: Integer);
begin

  inherited SetTruePeakOversample(Factor);
end;

{ IMfParametricEqInspect }

function TMfParametricEqMFT.GetCurrentCoeffs(out C: TBiquadCoeffs;
                                             out SampleRate: Double): Boolean;
begin

  C := FCoeffCur;
  SampleRate := FLastSampleRate;
  Result := (FLastSampleRate > 0);
end;


function TMfParametricEqMFT.GetTargetCoeffs(out C: TBiquadCoeffs;
                                            out SampleRate: Double): Boolean;
begin

  C := FCoeffTgt;
  SampleRate := FLastSampleRate;
  Result := (FLastSampleRate > 0);
end;


function TMfParametricEqMFT.GetCurrentParams(out GainDb,
                                             FreqHz,
                                             Q,
                                             BandwidthOct: Double): Boolean;
begin

  GainDb := FCGainDb;
  FreqHz := FCFreqHz;
  Q := FCQ;

  BandwidthOct := FTBWOct; // last-set octave value (drives target if >0)
  Result := (FLastSampleRate > 0);
end;


end.

