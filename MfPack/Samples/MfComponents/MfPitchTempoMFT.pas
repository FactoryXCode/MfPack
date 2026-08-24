// FactoryX
//
// Project: Media Foundation - MFPack - Samples
// Module: MfPitchTempoMFT.pas
// Kind: Pascal Unit
//
// Description: Pitch + Tempo (time-stretch + pitch-shift) MFT.
// Notes:
//  - Intended to be hosted by TMfWasApiEffectsRack (float32 processing path).
//  - Settings updated thread-safely via critical section + atomic dirty flag.
//  - DSP core is a phase-vocoder + simple bin-shift pitch.
//
// Requires: MfAudioEffectMFTBase (IMFTransform helper), PcmLib (PSingleArray).

// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPitchTempoMFT.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description: Pitch + Tempo (time-stretch + pitch-shift) MFT.
// Notes:
//  - Intended to be hosted by TMfWasApiEffectsRack (float32 processing path).
//  - Settings updated thread-safely via critical section + atomic dirty flag.
//  - DSP core is a phase-vocoder + simple bin-shift pitch. 
//
// Organisation: FactoryX
// Initiator(s): Carmen
// Contributor(s): Carmen (Carmenh) , Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Requires MfAudioEffectMFTBase (IMFTransform helper), PcmLib (PSingleArray).
//
// Related objects: -
// Related projects: MfPackX400/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: -
//==============================================================================
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
unit MfPitchTempoMFT;

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
  WinApi.MediaFoundationApi.MfUtils,  // NormalizeBOOL
  {Application}
  PcmLib, // PSingleArray
  MfAudioEffectMFTBase;

type

  TPitchTempoMode = (ptmClean,
                     ptmDJ);

  TPitchTempoSettings = packed record
    Enabled: INT;
    PitchSemitones: Single;    // -24..+24
    TempoPercent: Single;      // 50..200
    PreserveFormants: LongBool;// reserved
    WindowSize: Integer;       // 512/1024/2048/4096
    Overlap: Single;           // 0.25..0.75
    Mode: TPitchTempoMode;

    class function Defaults(): TPitchTempoSettings; static;
  end;

const

  PITCH_TEMPO_DEFAULTS: TPitchTempoSettings = (PitchSemitones: 0.0;
                                               TempoPercent: 100.0;
                                               PreserveFormants: LongBool(False);
                                               WindowSize: 1024;
                                               Overlap: 0.5;
                                               Mode: ptmClean);

type

  IMfPitchTempoMft = interface(IUnknown)
  ['{9C7B6C9E-5F74-4E53-9B35-60F5E6B6D3A1}']

    procedure EnableFX(const AEnabled: Boolean); stdcall;

    procedure SetSettings(const S: TPitchTempoSettings); stdcall;
    function GetSettings(out S: TPitchTempoSettings): HRESULT; stdcall;
    procedure ResetState; stdcall;
  end;


  // --------------------------------------------------------------------------
  // Minimal complex type
  // --------------------------------------------------------------------------
  TComplex = record
    Re: Double;
    Im: Double;
  end;

  TComplexArray = array of TComplex;

  TChanState = record
    InBuf: array of Single;
    InStart: Integer;
    InCount: Integer;

    OutBuf: array of Single;
    OutStart: Integer;
    OutCount: Integer;

    OLA: array of Double;
    OLAWeight: array of Double;
    OLAPos: Integer;

    PrevPhase: array of Double;
    PhaseAcc: array of Double;

    FFTBuf: TComplexArray;
    Spec: TComplexArray;
    TmpSpec: TComplexArray;
    Mag: array of Double;
    Phase: array of Double;
  end;

  // ---------------------------------------------------------------------------
  // DSP core (not an MFT). Used internally by the MFT wrapper.
  // ---------------------------------------------------------------------------
  TMfPitchTempoDSP = class
  private

    FSampleRate: Integer;
    FChannels: Integer;

    // Smoothed factors (audio thread)
    FCurPitchFactor: Double;
    FCurTempoFactor: Double;

    // Working config
    FWinSize: Integer;
    FHopA: Integer;
    FOverlap: Double;
    FWindow: array of Double;
    FCh: array of TChanState;

    class function ClampF(const V,
                          AMin,
                          AMax: Double): Double; static;

    procedure Rebuild(const WinSize: Integer;
                      const Overlap: Double);

    procedure BuildWindow();
    procedure InternalReset();

    procedure FIFO_Push(var Buf: array of Single;
                        var Start,
                        Count: Integer;
                        const S: Single);

    function FIFO_Pop(var Buf: array of Single;
                      var Start,
                      Count: Integer;
                      out S: Single): Boolean;

    function FIFO_Peek(const Buf: array of Single;
                       const Start,
                       Count,
                       Index: Integer): Single;

    procedure FFTInPlace(var A: TComplexArray;
                         Inverse: Boolean);

    procedure ApplyPitchBinShiftInPlace(const chIndex: Integer;
                                        const PitchFactor: Double);

    procedure ProcessOneFrame(const chIndex: Integer;
                              const PitchFactor,
                              TempoFactor: Double;
                              const Mode: TPitchTempoMode);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure SetFormat(const SampleRate,
                        Channels: Integer);

    procedure ResetState();

    // Main processing (in-place interleaved float32)
    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer;
                                  const Settings: TPitchTempoSettings);
  end;

  // --------------------------------------------------------------------------
  // MFT wrapper (uniform with other FX: Eq/Chorus/etc)
  // --------------------------------------------------------------------------
  TMfPitchTempoMFT = class(TMfAudioEffectMFTBase, IMfPitchTempoMft)
  private

    FCS: TCriticalSection;
    FSettings: TPitchTempoSettings;
    FPendingSettings: TPitchTempoSettings;
    FPendingDirty: LongInt;

    FDsp: TMfPitchTempoDSP;

    procedure ApplyPendingSettings();

  protected

    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer); override;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure EnableFX(const AEnabled: Boolean); stdcall;

    procedure SetSettings(const S: TPitchTempoSettings); stdcall;

    function GetSettings(out S: TPitchTempoSettings): HRESULT; stdcall;

    procedure ResetState(); stdcall;
  end;


implementation

{ TPitchTempoSettings }

class function TPitchTempoSettings.Defaults(): TPitchTempoSettings;
begin

  Result.Enabled := 1;
  Result.PitchSemitones := 0.0;
  Result.TempoPercent := 100.0;
  Result.PreserveFormants := LongBool(False);
  Result.WindowSize := 1024;
  Result.Overlap := 0.5;
  Result.Mode := ptmClean;
end;

{ TMfPitchTempoDSP }

constructor TMfPitchTempoDSP.Create();
begin

  inherited Create();

  FSampleRate := 0;
  FChannels := 0;
  FCurPitchFactor := 1.0;
  FCurTempoFactor := 1.0;
  FWinSize := 0;
  FHopA := 0;
  FOverlap := 0.5;
end;


destructor TMfPitchTempoDSP.Destroy();
begin

  inherited Destroy;
end;


class function TMfPitchTempoDSP.ClampF(const V,
                                       AMin,
                                       AMax: Double): Double;
begin

  if (V < AMin) then
    Exit(AMin);
  if (V > AMax) then
    Exit(AMax);

  Result := V;
end;


procedure TMfPitchTempoDSP.SetFormat(const SampleRate,
                                     Channels: Integer);
begin

  FSampleRate := SampleRate;
  FChannels := Channels;
  // Rebuild will happen on first Process call when settings are known.
end;


procedure TMfPitchTempoDSP.BuildWindow();
var
  i,
  n: Integer;
  w: Double;

begin

  n := FWinSize;
  SetLength(FWindow,
             n);

  for i := 0 to n - 1 do
    begin

      w := 0.5 - 0.5 * Cos((2.0 * Pi * i) / (n - 1));
      FWindow[i] := w;
    end;
end;


procedure TMfPitchTempoDSP.Rebuild(const WinSize: Integer;
                                   const Overlap: Double);
var
  i: Integer;
  inCap,
  outCap,
  olaCap: Integer;

begin

  if (FWinSize = WinSize) and
     (Abs(FOverlap - Overlap) < 1e-9) and
     (Length(FCh) = FChannels) then
    Exit;

  FWinSize := WinSize;
  FOverlap := Overlap;
  FHopA := Max(1,
               Round(FWinSize * (1.0 - FOverlap)));

  BuildWindow();

  SetLength(FCh,
            FChannels);

  inCap := FWinSize * 4;
  outCap := FWinSize * 4;
  olaCap := FWinSize * 4;

  for i := 0 to FChannels - 1 do
    begin

      SetLength(FCh[i].InBuf,
                inCap);

      FCh[i].InStart := 0;
      FCh[i].InCount := 0;

      SetLength(FCh[i].OutBuf,
                outCap);

      FCh[i].OutStart := 0;
      FCh[i].OutCount := 0;

      SetLength(FCh[i].OLA,
                olaCap);

      FillChar(FCh[i].OLA[0],
               Length(FCh[i].OLA) * SizeOf(Double),
               0);

      SetLength(FCh[i].OLAWeight,
                olaCap);

      FillChar(FCh[i].OLAWeight[0],
               Length(FCh[i].OLAWeight) * SizeOf(Double), 0);

      FCh[i].OLAPos := 0;

      SetLength(FCh[i].PrevPhase,
                FWinSize div 2 + 1);

      SetLength(FCh[i].PhaseAcc,
                FWinSize div 2 + 1);

      FillChar(FCh[i].PrevPhase[0],
               Length(FCh[i].PrevPhase) * SizeOf(Double),
               0);

      FillChar(FCh[i].PhaseAcc[0],
               Length(FCh[i].PhaseAcc) * SizeOf(Double),
               0);

      SetLength(FCh[i].FFTBuf,
                FWinSize);

      SetLength(FCh[i].Spec,
                FWinSize);

      SetLength(FCh[i].TmpSpec,
                FWinSize);

      SetLength(FCh[i].Mag,
                FWinSize div 2 + 1);

      SetLength(FCh[i].Phase,
                FWinSize div 2 + 1);
    end;

  InternalReset();
end;


procedure TMfPitchTempoDSP.InternalReset();
var
  ch,
  i: Integer;

begin

  for ch := 0 to Length(FCh) - 1 do
    begin

      FCh[ch].InStart := 0;
      FCh[ch].InCount := 0;
      FCh[ch].OutStart := 0;
      FCh[ch].OutCount := 0;
      FCh[ch].OLAPos := 0;

      for i := 0 to Length(FCh[ch].OLA) - 1 do
        begin
          FCh[ch].OLA[i] := 0.0;
          FCh[ch].OLAWeight[i] := 0.0;
        end;

      for i := 0 to Length(FCh[ch].PrevPhase) - 1 do
        FCh[ch].PrevPhase[i] := 0.0;

      for i := 0 to Length(FCh[ch].PhaseAcc) - 1 do
        FCh[ch].PhaseAcc[i] := 0.0;
    end;

  FCurPitchFactor := 1.0;
  FCurTempoFactor := 1.0;
end;


procedure TMfPitchTempoDSP.ResetState();
begin

  InternalReset();
end;


procedure TMfPitchTempoDSP.FIFO_Push(var Buf: array of Single; var Start, Count: Integer; const S: Single);
var
  idx: Integer;

begin

  if (Count >= Length(Buf)) then
    begin

      Start := (Start + 1) mod Length(Buf);
      Dec(Count);
    end;

  idx := (Start + Count) mod Length(Buf);
  Buf[idx] := S;
  Inc(Count);
end;


function TMfPitchTempoDSP.FIFO_Pop(var Buf: array of Single;
                                   var Start, Count: Integer;
                                   out S: Single): Boolean;
begin

  if (Count <= 0) then
    begin

      S := 0;
      Exit(False);
    end;

  S := Buf[Start];
  Start := (Start + 1) mod Length(Buf);
  Dec(Count);
  Result := True;
end;


function TMfPitchTempoDSP.FIFO_Peek(const Buf: array of Single;
                                    const Start,
                                    Count,
                                    Index: Integer): Single;
var
  idx: Integer;

begin

  if (Index < 0) or (Index >= Count) then
    Exit(0);

  idx := (Start + Index) mod Length(Buf);
  Result := Buf[idx];
end;


procedure TMfPitchTempoDSP.FFTInPlace(var A: TComplexArray;
                                      Inverse: Boolean);
var
  n,
  i,
  j,
  m,
  m2,
  k: Integer;
  t,
  u: TComplex;
  ang,
  wpr,
  wpi,
  wr,
  wi,
  tr,
  ti: Double;

  procedure Swap(var X,
                 Y: TComplex);
  var
    tmp: TComplex;

  begin

    tmp := X;
    X := Y;
    Y := tmp;
  end;

begin

  n := Length(A);

  if (n <= 1) then
    Exit;

  // Bit reversal
  j := 0;
  for i := 1 to n - 1 do
    begin

      m := n shr 1;
      while ((j and m) <> 0) do
        begin

          j := j xor m;
          m := m shr 1;
        end;
      j := j xor m;

      if (i < j) then
        Swap(A[i],
             A[j]);
    end;

  // DanielsonLanczos
  m := 2;

  while (m <= n) do
    begin

      m2 := m shr 1;

      // Twiddle for this stage..
      // This might work if nothing goes wrong.
      // ang := Pi / m2;
      // A more solid aproach.
      ang := (2.0 * Pi) / m;

      if Inverse then
        ang := -ang;

      wpr := Cos(ang);
      wpi := Sin(ang);

      // Iterate groups
      k := 0;

      while (k < n) do
        begin

          wr := 1.0;
          wi := 0.0;

          // Butterflies inside group.
          for j := 0 to m2 - 1 do
            begin

              i := k + j;

              tr := wr * A[i + m2].Re - wi * A[i + m2].Im;
              ti := wr * A[i + m2].Im + wi * A[i + m2].Re;

              u := A[i];

              A[i + m2].Re := u.Re - tr;
              A[i + m2].Im := u.Im - ti;

              A[i].Re := u.Re + tr;
              A[i].Im := u.Im + ti;

             // Advance twiddle.
             t.Re := wr;
             t.Im := wi;
             wr := t.Re * wpr - t.Im * wpi;
             wi := t.Re * wpi + t.Im * wpr;
            end;

          Inc(k,
              m);
        end;

      m := m shl 1;
    end;

  // Normalize inverse
  if Inverse then
    begin

      for i := 0 to n - 1 do
        begin

          A[i].Re := A[i].Re / n;
          A[i].Im := A[i].Im / n;
        end;
    end;
end;


procedure TMfPitchTempoDSP.ApplyPitchBinShiftInPlace(const chIndex: Integer;
                                                     const PitchFactor: Double);
var
  N,
  k,
  src: Integer;
  ch: ^TChanState;

begin

  if Abs(PitchFactor - 1.0) < 1e-6 then
    Exit;

  ch := @FCh[chIndex];
  N := Length(ch^.Spec);
  if (N <= 0) then
    Exit;

  FillChar(ch^.TmpSpec[0],
           N * SizeOf(TComplex),
           0);

  for k := 0 to (N div 2) do
    begin

      src := Round(k / PitchFactor);
      if (src >= 0) and (src <= (N div 2)) then
        ch^.TmpSpec[k] := ch^.Spec[src];
    end;

  for k := 1 to (N div 2) - 1 do
    begin

      ch^.TmpSpec[N - k].Re := ch^.TmpSpec[k].Re;
      ch^.TmpSpec[N - k].Im := -ch^.TmpSpec[k].Im;
    end;

  Move(ch^.TmpSpec[0],
       ch^.Spec[0],
       N * SizeOf(TComplex));
end;


procedure TMfPitchTempoDSP.ProcessOneFrame(const chIndex: Integer;
                                           const PitchFactor, TempoFactor: Double;
                                           const Mode: TPitchTempoMode);
var
  N,
  k,
  i: Integer;
  ch: ^TChanState;
  omega,
  phase,
  delta,
  expected,
  princ: Double;
  mag,
  re,
  im: Double;
  hopA,
  hopS: Integer;
  phaseLock: Double;

  // Helper.
  procedure ConsumeIn(const n: Integer);
  var
    bLen,
    nn: Integer;

  begin

    bLen := Length(ch^.InBuf);
    nn := n;
    if (nn > ch^.InCount) then
      nn := ch^.InCount;
    ch^.InStart := (ch^.InStart + nn) mod bLen;
    Dec(ch^.InCount, nn);
  end;

  procedure PushOutSample(const s: Single);
  begin

    FIFO_Push(ch^.OutBuf,
              ch^.OutStart,
              ch^.OutCount, s);
  end;

  function ReadInSample(const idx: Integer): Single;
  begin

    Result := FIFO_Peek(ch^.InBuf,
                        ch^.InStart,
                        ch^.InCount,
                        idx);
  end;

begin

  ch := @FCh[chIndex];
  N := FWinSize;
  if (ch^.InCount < N) then
    Exit;

  hopA := FHopA;
  hopS := Max(1,
              Round(hopA * TempoFactor));

  phaseLock := 1.0; // DJ: avoid extra phase slop (reduces watery distortion)

  for i := 0 to N - 1 do
    begin

      ch^.FFTBuf[i].Re := Double(ReadInSample(i)) * FWindow[i];
      ch^.FFTBuf[i].Im := 0.0;
    end;

  Move(ch^.FFTBuf[0],
       ch^.Spec[0],
       N * SizeOf(TComplex));

  FFTInPlace(ch^.Spec,
             False);

  for k := 0 to (N div 2) do
    begin

      re := ch^.Spec[k].Re;
      im := ch^.Spec[k].Im;
      mag := Sqrt(re * re + im * im);
      phase := ArcTan2(im, re);
      ch^.Mag[k] := mag;
      ch^.Phase[k] := phase;
    end;

  for k := 0 to (N div 2) do
    begin

      omega := (2.0 * Pi * k) / N;
      expected := omega * hopA;
      delta := ch^.Phase[k] - ch^.PrevPhase[k] - expected;
      princ := delta - 2.0 * Pi * Floor((delta + Pi) / (2.0 * Pi));

      ch^.PhaseAcc[k] := ch^.PhaseAcc[k] + (expected + princ) * (hopS / hopA) * phaseLock;
      ch^.PrevPhase[k] := ch^.Phase[k];

      ch^.Spec[k].Re := ch^.Mag[k] * Cos(ch^.PhaseAcc[k]);
      ch^.Spec[k].Im := ch^.Mag[k] * Sin(ch^.PhaseAcc[k]);

      if (k > 0) and (k < (N div 2)) then
        begin

          ch^.Spec[N - k].Re := ch^.Spec[k].Re;
          ch^.Spec[N - k].Im := -ch^.Spec[k].Im;
        end;
    end;

  ApplyPitchBinShiftInPlace(chIndex,
                            PitchFactor);
  FFTInPlace(ch^.Spec,
             True);

  for i := 0 to N - 1 do
    ch^.OLA[(ch^.OLAPos + i) mod Length(ch^.OLA)] := ch^.OLA[(ch^.OLAPos + i) mod Length(ch^.OLA)] + (ch^.Spec[i].Re * FWindow[i]);

  for i := 0 to hopS - 1 do
    begin

      PushOutSample(Single(ch^.OLA[ch^.OLAPos mod Length(ch^.OLA)]));
      ch^.OLA[ch^.OLAPos mod Length(ch^.OLA)] := 0.0;

      Inc(ch^.OLAPos);

      if (ch^.OLAPos >= Length(ch^.OLA)) then
        ch^.OLAPos := ch^.OLAPos mod Length(ch^.OLA);
    end;

  ConsumeIn(hopA);
end;


procedure TMfPitchTempoDSP.ProcessAudioFloat32(pData: PSingle;
                                               Frames,
                                               Channels,
                                               SampleRate: Integer;
                                               const Settings: TPitchTempoSettings);
var
  win: Integer;
  ovl: Double;
  pitchSemi,
  tempoPct: Double;
  pitchFactorTarget,
  tempoFactorTarget: Double;
  smoothCoeff: Double;
  pitchFactor,
  tempoFactor: Double;
  i,
  ch: Integer;
  idx: Integer;
  s,
  outS: Single;
  mode: TPitchTempoMode;
  enabled: Boolean;
  it: Integer;

begin

  if (pData = nil) or (Frames <= 0) then
    Exit;

  if (SampleRate <> FSampleRate) or (Channels <> FChannels) then
    SetFormat(SampleRate,
              Channels);

  enabled := (Settings.Enabled <> 0);
  if not enabled then
    Exit;

  win := Settings.WindowSize;
  if (win <> 512) and
     (win <> 1024) and
     (win <> 2048) and
     (win <> 4096) then
    win := 1024;

  ovl := ClampF(Settings.Overlap,
                0.50,
                0.90);

  mode := Settings.Mode;
  if (mode = ptmDJ) and (win > 1024) then
    win := 1024;

  Rebuild(win,
          ovl);

  pitchSemi := ClampF(Settings.PitchSemitones,
                      -24,
                      24);

  tempoPct := ClampF(Settings.TempoPercent,
                     50,
                     200);

  pitchFactorTarget := Power(2.0,
                             pitchSemi / 12.0);

  tempoFactorTarget := 100.0 / tempoPct;  // stretch factor (duration multiplier)

  if (mode = ptmClean) then
    smoothCoeff := 0.02
  else
    smoothCoeff := 0.03;

  // Keep tempo smoothing
  FCurTempoFactor := FCurTempoFactor + (tempoFactorTarget - FCurTempoFactor) * smoothCoeff;
  tempoFactor := FCurTempoFactor;

  // Pitch: snap (or fast ramp)
  FCurPitchFactor := FCurPitchFactor + (pitchFactorTarget - FCurPitchFactor) * 0.45; // fast ramp
  FCurPitchFactor := pitchFactorTarget;

  idx := 0;
  for i := 0 to Frames - 1 do
    begin
      for ch := 0 to FChannels - 1 do
        begin

          s := PSingle(PByte(pData) + (idx * SizeOf(Single)))^;
          FIFO_Push(FCh[ch].InBuf,
                    FCh[ch].InStart,
                    FCh[ch].InCount, s);
          Inc(idx);
        end;
    end;

  for ch := 0 to FChannels - 1 do
    begin

      it := 0;

      while (FCh[ch].OutCount < Frames) and (FCh[ch].InCount >= FWinSize) and (it < Max(64, Min(512, (Frames div Max(1,
                                                                                                                     Round(FHopA * tempoFactor))) + 16))) do
        begin

          ProcessOneFrame(ch,
                          pitchFactor,
                          tempoFactor,
                          mode);
          Inc(it);
        end;
    end;

  idx := 0;

  for i := 0 to Frames - 1 do
  begin
    for ch := 0 to FChannels - 1 do
      begin

        if not FIFO_Pop(FCh[ch].OutBuf,
                        FCh[ch].OutStart,
                        FCh[ch].OutCount,
                        outS) then
          outS := PSingle(PByte(pData) + (idx * SizeOf(Single)))^; // dry-pass on underflow

        // Prevent harsh clipping when pitching up.
        if (outS > 1.0) then outS := 1.0 else
          if (outS < -1.0) then outS := -1.0;
        PSingle(PByte(pData) + (idx * SizeOf(Single)))^ := outS;
      Inc(idx);
    end;
  end;
end;


{ TMfPitchTempoMFT }

constructor TMfPitchTempoMFT.Create();
begin

  inherited Create();

  FCS := TCriticalSection.Create();
  FSettings := TPitchTempoSettings.Defaults;
  FPendingSettings := FSettings;
  FPendingDirty := 0;
  FDsp := TMfPitchTempoDSP.Create;
end;


destructor TMfPitchTempoMFT.Destroy();
begin

  FreeAndNil(FDsp);
  FreeAndNil(FCS);

  inherited Destroy();
end;


procedure TMfPitchTempoMFT.ApplyPendingSettings();
begin

  if (InterlockedCompareExchange(FPendingDirty,
                                0,
                                0) = 0) then
    Exit;

  FCS.Enter();

  try

    if (InterlockedExchange(FPendingDirty,
                           0) <> 0) then
      FSettings := FPendingSettings;
  finally

    FCS.Leave();
  end;
end;


procedure TMfPitchTempoMFT.ProcessAudioFloat32(pData: PSingle;
                                               Frames,
                                               Channels,
                                               SampleRate: Integer);
begin

  ApplyPendingSettings();

  if (FDsp <> nil) then
    FDsp.ProcessAudioFloat32(pData,
                             Frames,
                             Channels,
                             SampleRate,
                             FSettings);
end;


procedure TMfPitchTempoMFT.EnableFX(const AEnabled: Boolean); stdcall;
begin

  FCS.Enter();

  try

    // Normalize to 0/1 LongBool (Delphi LongBool True is -1).
    FPendingSettings.Enabled := Abs(Ord(AEnabled));
    InterlockedExchange(FPendingDirty,
                        1);
  finally

    FCS.Leave();
  end;
end;


procedure TMfPitchTempoMFT.SetSettings(const S: TPitchTempoSettings); stdcall;
begin

  FCS.Enter();

  try

    FPendingSettings := S;
    InterlockedExchange(FPendingDirty,
                        1);
  finally

    FCS.Leave();
  end;
end;


function TMfPitchTempoMFT.GetSettings(out S: TPitchTempoSettings): HRESULT; stdcall;
begin

  FCS.Enter();

  try

    S := FPendingSettings;
    Result := S_OK;
  finally

    FCS.Leave();
  end;
end;


procedure TMfPitchTempoMFT.ResetState(); stdcall;
begin

  if (FDsp <> nil) then
    FDsp.ResetState();
end;

end.
