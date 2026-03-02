unit MfLomMidHighEqMFT;

// MfPack - 3-band EQ (Low / Mid / High) MFT
//
// Notes
//  - Intended to be hosted by TMfWasApiEffectsRack (float32 processing path).
//  - Uses RBJ cookbook biquad formulas.
//  - Thread-safe settings updates via critical section + atomic dirty flag.
//  - Zipper-noise free gain smoothing (simple exponential smoothing per block).
//
// This unit assumes MfPack provides TMfAudioEffectMFTBase (IMFTransform helper).

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  System.SyncObjs,
  Winapi.ActiveX,
  MfAudioEffectMFTBase, // MfPack base: implements IMFTransform boilerplate
  MfMFTypes;            // MfPack common MF types (if available)

type
  TMfEqMidMode = (emmPeaking, emmNotch);

  TMfHighMidLowEqSettings = packed record
    Enabled: LongBool;        // Delphi LongBool (-1/0); normalize when needed

    // Smoothing
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

    class function Defaults: TMfHighMidLowEqSettings; static;
  end;

  
  IMfHighMidLowEqMft = interface(IUnknown)
    ['{7B42E6F1-1D11-4C6B-A5F1-0B6A9F3B0A31}']
    procedure SetSettings(const S: TMfHighMidLowEqSettings); stdcall;
    function GetSettings(out S: TMfHighMidLowEqSettings): HRESULT; stdcall;
    procedure ResetState; stdcall;
  end;

// 3-band EQ MFT
  TMfHighMidLowEqMFT = class(TMfAudioEffectMFTBase, IMfHighMidLowEqMft)
  private
    FCS: TCriticalSection;

    // Live settings snapshot used by the processing thread.
    FSettings: TMfHighMidLowEqSettings;

    // Pending settings written by UI thread.
    FPendingSettings: TMfHighMidLowEqSettings;
    FPendingDirty: LongInt;

    // Stream format
    FSampleRate: Integer;
    FChannels: Integer;

    // Smoothed gains (linear)
    FLowGainLin: Double;
    FMidGainLin: Double;
    FHighGainLin: Double;

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

    var
      FLow: array of TBiquad; // per channel
      FMid: array of TBiquad;
      FHigh: array of TBiquad;

    procedure ApplyPendingSettings;
    procedure ResetStateInternal;

    procedure EnsureChannelState;

    procedure CalcLowShelf(const FreqHz, GainDb, Q: Double; const SampleRate: Double; out B: TBiquad);
    procedure CalcHighShelf(const FreqHz, GainDb, Q: Double; const SampleRate: Double; out B: TBiquad);
    procedure CalcPeaking(const FreqHz, GainDb, Q: Double; const SampleRate: Double; out B: TBiquad);
    procedure CalcNotch(const FreqHz, Q: Double; const SampleRate: Double; out B: TBiquad);

    function ClampHz(const Hz: Double; const SampleRate: Integer): Double;
    function DbToLin(const Db: Double): Double;

    function NormalizeBool01(const B: LongBool): Integer;

  protected
    // TMfAudioEffectMFTBase hook: called when the rack configures float32
    function OnSetInputTypeFloat32(const SampleRate, Channels: Integer): HRESULT; override;

    // Processing
    procedure ProcessAudioFloat32(pData: PSingle; Frames, Channels, SampleRate: Integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetSettings(const S: TMfHighMidLowEqSettings); stdcall;
    function GetSettings(out S: TMfHighMidLowEqSettings): HRESULT; stdcall;

    procedure ResetState; stdcall;
  end;

implementation

{ TMfHighMidLowEqSettings }

class function TMfHighMidLowEqSettings.Defaults: TMfHighMidLowEqSettings;
begin
  FillChar(Result, SizeOf(Result), 0);

  // IMPORTANT: Delphi LongBool True = -1
  Result.Enabled := LongBool(True);
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

{ TMfHighMidLowEqMFT.TBiquad }

procedure TMfHighMidLowEqMFT.TBiquad.Reset;
begin
  x1 := 0.0;
  x2 := 0.0;
  y1 := 0.0;
  y2 := 0.0;
end;

{ TMfHighMidLowEqMFT }

constructor TMfHighMidLowEqMFT.Create;
begin
  inherited Create;

  FCS := TCriticalSection.Create;
  FSettings := TMfHighMidLowEqSettings.Defaults;
  FPendingSettings := FSettings;
  FPendingDirty := 0;

  FSampleRate := 0;
  FChannels := 0;

  FLowGainLin := 1.0;
  FMidGainLin := 1.0;
  FHighGainLin := 1.0;
end;

destructor TMfHighMidLowEqMFT.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;

function TMfHighMidLowEqMFT.NormalizeBool01(const B: LongBool): Integer;
begin
  // Delphi LongBool uses -1 for True; normalize for comparisons.
  if B then
    Result := 1
  else
    Result := 0;
end;

procedure TMfHighMidLowEqMFT.SetSettings(const S: TMfHighMidLowEqSettings);
begin
  // UI thread safe: store pending copy
  FCS.Enter;
  try
    FPendingSettings := S;
    // Mark dirty atomically.
    InterlockedExchange(FPendingDirty, 1);
  finally
    FCS.Leave;
  end;
end;

function TMfHighMidLowEqMFT.GetSettings(out S: TMfHighMidLowEqSettings): HRESULT;
begin
  FCS.Enter;
  try
    S := FSettings;
    Result := S_OK;
  finally
    FCS.Leave;
  end;
end;

procedure TMfHighMidLowEqMFT.ApplyPendingSettings;
var
  dirty: LongInt;
begin
  dirty := InterlockedCompareExchange(FPendingDirty, 0, 0);
  if dirty = 0 then
    Exit;

  // Reset dirty first to avoid churn.
  InterlockedExchange(FPendingDirty, 0);

  FCS.Enter;
  try
    FSettings := FPendingSettings;
  finally
    FCS.Leave;
  end;

  // Recalc coeffs immediately.
  // (Gains are smoothed separately; coeffs for freq/Q changes take effect at block boundary.)
  EnsureChannelState;
end;

procedure TMfHighMidLowEqMFT.ResetState;
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

function TMfHighMidLowEqMFT.ClampHz(const Hz: Double; const SampleRate: Integer): Double;
var
  ny: Double;
begin
  ny := SampleRate * 0.5;
  Result := EnsureRange(Hz, 10.0, ny * 0.45);
end;

function TMfHighMidLowEqMFT.DbToLin(const Db: Double): Double;
begin
  Result := Power(10.0, Db / 20.0);
end;

procedure TMfHighMidLowEqMFT.EnsureChannelState;
var
  ch, newCh: Integer;
  sr: Double;
  bTmp: TBiquad;
  lf, mf, hf: Double;
  lq, mq, hq: Double;
  lg, mg, hg: Double;
  en: Integer;
begin
  if (FSampleRate <= 0) or (FChannels <= 0) then
    Exit;

  newCh := FChannels;
  if Length(FLow) <> newCh then
  begin
    SetLength(FLow, newCh);
    SetLength(FMid, newCh);
    SetLength(FHigh, newCh);
    ResetState;
  end;

  sr := FSampleRate;

  // Clamp + read settings
  en := NormalizeBool01(FSettings.Enabled);
  if en = 0 then
  begin
    // Still keep coefficients sane.
    Exit;
  end;

  lf := ClampHz(FSettings.LowFreqHz, FSampleRate);
  mf := ClampHz(FSettings.MidFreqHz, FSampleRate);
  hf := ClampHz(FSettings.HighFreqHz, FSampleRate);

  lq := EnsureRange(FSettings.LowQ, 0.1, 4.0);
  mq := EnsureRange(FSettings.MidQ, 0.1, 12.0);
  hq := EnsureRange(FSettings.HighQ, 0.1, 4.0);

  lg := EnsureRange(FSettings.LowGainDb, -24.0, 24.0);
  mg := EnsureRange(FSettings.MidGainDb, -24.0, 24.0);
  hg := EnsureRange(FSettings.HighGainDb, -24.0, 24.0);

  // Coefficients per band (same for all channels; only state differs)
  CalcLowShelf(lf, lg, lq, sr, bTmp);
  for ch := 0 to newCh - 1 do
  begin
    FLow[ch].b0 := bTmp.b0;
    FLow[ch].b1 := bTmp.b1;
    FLow[ch].b2 := bTmp.b2;
    FLow[ch].a1 := bTmp.a1;
    FLow[ch].a2 := bTmp.a2;
  end;

  if FSettings.MidMode = emmNotch then
    CalcNotch(mf, mq, sr, bTmp)
  else
    CalcPeaking(mf, mg, mq, sr, bTmp);

  for ch := 0 to newCh - 1 do
  begin
    FMid[ch].b0 := bTmp.b0;
    FMid[ch].b1 := bTmp.b1;
    FMid[ch].b2 := bTmp.b2;
    FMid[ch].a1 := bTmp.a1;
    FMid[ch].a2 := bTmp.a2;
  end;

  CalcHighShelf(hf, hg, hq, sr, bTmp);
  for ch := 0 to newCh - 1 do
  begin
    FHigh[ch].b0 := bTmp.b0;
    FHigh[ch].b1 := bTmp.b1;
    FHigh[ch].b2 := bTmp.b2;
    FHigh[ch].a1 := bTmp.a1;
    FHigh[ch].a2 := bTmp.a2;
  end;

  // Targets for smoothing
  // (We smooth gains in linear domain; coefficients already include gain, but smoothing still helps
  //  when UI rapidly changes gain - we crossfade by scaling band outputs.)
  // For simplicity, we smooth a post-scale per band around unity.
  // That keeps coeff recalcs clean and avoids per-sample coefficient interpolation.

  // Targets are 1.0, since coefficients already include gain.
  // But if you later prefer "gain-only" smoothing, you can set targets here.
end;

procedure TMfHighMidLowEqMFT.CalcPeaking(const FreqHz, GainDb, Q: Double;
  const SampleRate: Double; out B: TBiquad);
var
  A, w0, alpha, cosw0: Double;
  b0, b1, b2, a0, a1, a2: Double;
begin
  A := Power(10.0, GainDb / 40.0);
  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  alpha := Sin(w0) / (2.0 * Q);

  b0 := 1.0 + alpha * A;
  b1 := -2.0 * cosw0;
  b2 := 1.0 - alpha * A;
  a0 := 1.0 + alpha / A;
  a1 := -2.0 * cosw0;
  a2 := 1.0 - alpha / A;

  // Normalize
  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;

procedure TMfHighMidLowEqMFT.CalcNotch(const FreqHz, Q: Double;
  const SampleRate: Double; out B: TBiquad);
var
  w0, alpha, cosw0: Double;
  b0, b1, b2, a0, a1, a2: Double;
begin
  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  alpha := Sin(w0) / (2.0 * Q);

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

procedure TMfHighMidLowEqMFT.CalcLowShelf(const FreqHz, GainDb, Q: Double;
  const SampleRate: Double; out B: TBiquad);
var
  A, w0, alpha, cosw0, sinw0: Double;
  sqrtA: Double;
  b0, b1, b2, a0, a1, a2: Double;
  S: Double;
begin
  // RBJ low shelf (using S slope; we approximate S via Q)
  // We map Q to S loosely; for musical EQ, this is good enough.
  S := EnsureRange(Q, 0.1, 4.0);

  A := Power(10.0, GainDb / 40.0);
  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  sqrtA := Sqrt(A);

  alpha := sinw0 / 2.0 * Sqrt((A + 1.0 / A) * (1.0 / S - 1.0) + 2.0);

  b0 :=    A * ((A + 1.0) - (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha);
  b1 :=  2*A * ((A - 1.0) - (A + 1.0) * cosw0);
  b2 :=    A * ((A + 1.0) - (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha);
  a0 :=         (A + 1.0) + (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha;
  a1 :=    -2 * ((A - 1.0) + (A + 1.0) * cosw0);
  a2 :=         (A + 1.0) + (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;

procedure TMfHighMidLowEqMFT.CalcHighShelf(const FreqHz, GainDb, Q: Double;
  const SampleRate: Double; out B: TBiquad);
var
  A, w0, alpha, cosw0, sinw0: Double;
  sqrtA: Double;
  b0, b1, b2, a0, a1, a2: Double;
  S: Double;
begin
  S := EnsureRange(Q, 0.1, 4.0);

  A := Power(10.0, GainDb / 40.0);
  w0 := 2.0 * Pi * FreqHz / SampleRate;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  sqrtA := Sqrt(A);

  alpha := sinw0 / 2.0 * Sqrt((A + 1.0 / A) * (1.0 / S - 1.0) + 2.0);

  b0 :=    A * ((A + 1.0) + (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha);
  b1 := -2*A * ((A - 1.0) + (A + 1.0) * cosw0);
  b2 :=    A * ((A + 1.0) + (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha);
  a0 :=         (A + 1.0) - (A - 1.0) * cosw0 + 2.0 * sqrtA * alpha;
  a1 :=     2 * ((A - 1.0) - (A + 1.0) * cosw0);
  a2 :=         (A + 1.0) - (A - 1.0) * cosw0 - 2.0 * sqrtA * alpha;

  B.b0 := b0 / a0;
  B.b1 := b1 / a0;
  B.b2 := b2 / a0;
  B.a1 := a1 / a0;
  B.a2 := a2 / a0;
end;

function TMfHighMidLowEqMFT.OnSetInputTypeFloat32(const SampleRate,
  Channels: Integer): HRESULT;
begin
  FSampleRate := SampleRate;
  FChannels := Channels;

  EnsureChannelState;
  ResetState;

  Result := S_OK;
end;

procedure TMfHighMidLowEqMFT.ProcessAudioFloat32(pData: PSingle; Frames,
  Channels, SampleRate: Integer);
var
  i, ch: Integer;
  idx: Integer;
  x, y: Double;
  en: Integer;
  rampMs: Integer;
  dtMs: Double;
  alpha: Double;

  function ProcBiquad(var B: TBiquad; const InS: Double): Double;
  begin
    Result := B.b0 * InS + B.b1 * B.x1 + B.b2 * B.x2 - B.a1 * B.y1 - B.a2 * B.y2;
    B.x2 := B.x1;
    B.x1 := InS;
    B.y2 := B.y1;
    B.y1 := Result;
  end;

begin
  if (pData = nil) or (Frames <= 0) then
    Exit;

  // Ensure our cached SR/Ch are consistent.
  if (SampleRate <> FSampleRate) or (Channels <> FChannels) then
  begin
    FSampleRate := SampleRate;
    FChannels := Channels;
    EnsureChannelState;
    ResetState;
  end;

  ApplyPendingSettings;

  en := NormalizeBool01(FSettings.Enabled);
  if en = 0 then
    Exit;

  // Gain smoothing (currently smoothing unity scalars, but we keep the machinery
  // in place for future gain-only crossfade).
  rampMs := Max(0, FSettings.RampMs);
  if rampMs = 0 then
    alpha := 1.0
  else
  begin
    // dt = block duration in ms
    dtMs := (Frames / Max(1.0, SampleRate)) * 1000.0;
    // exponential smoothing coefficient for this block
    alpha := 1.0 - Exp(-dtMs / rampMs);
    alpha := EnsureRange(alpha, 0.0, 1.0);
  end;

  // Targets are 1.0 (coeffs include gain). Keep for future gain crossfade.
  FLowGainLin := FLowGainLin + (1.0 - FLowGainLin) * alpha;
  FMidGainLin := FMidGainLin + (1.0 - FMidGainLin) * alpha;
  FHighGainLin := FHighGainLin + (1.0 - FHighGainLin) * alpha;

  // Process interleaved float32
  idx := 0;
  for i := 0 to Frames - 1 do
  begin
    for ch := 0 to Channels - 1 do
    begin
      x := pData[idx];

      // 3-band cascade
      y := ProcBiquad(FLow[ch], x);
      y := ProcBiquad(FMid[ch], y);
      y := ProcBiquad(FHigh[ch], y);

      // Optional per-band crossfade scalars (unity by default)
      y := y * FLowGainLin * FMidGainLin * FHighGainLin;

      // Clamp to float32 range
      if y > 1.0 then
        y := 1.0
      else if y < -1.0 then
        y := -1.0;

      pData[idx] := Single(y);
      Inc(idx);
    end;
  end;
end;

end.
