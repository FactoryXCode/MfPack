// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEqBaseMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description:
//   Audio MFT class:
//     - 1 input stream, 1 output stream.
//     - Keeps one input sample, produces one output sample.
//     - Derived class processes raw PCM bytes (in-place on output buffer).
//
//   Supported formats (by default):
//     - MFMediaType_Audio + MFAudioFormat_PCM, 16-bit or 32-bit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX400/Samples/WasApiPlayer/Example3
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
unit MfAudioHighMidLowMFT;

interface

uses
  WinApi.Windows,
  WinApi.ActiveX,
  System.SysUtils,
  System.Math,
  System.SyncObjs,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  MfAudioEqBaseMFT,
  MfAudioHighMidLowTypes;

type

  // Simple biquad filter state.
  TBiquad = record
    a0,
    a1,
    a2: Double;
    b1,
    b2: Double;
    x1,
    x2: Double;
    y1,
    y2: Double;

    procedure Reset();
    procedure SetCoeffs(const Aa0,
                        Aa1,
                        Aa2,
                        Bb1,
                        Bb2:
                        Double);

    function Process(const x: Double): Double;
  end;


  THighMidLowMFT = class(TMfAudioEqBaseMFT, IMfHighMidLowControl)
  private

    FEnabled: Boolean;

    // Targets (dB) and current (dB) for smoothing.
    FLowDbTarget,
    FMidDbTarget,
    FHighDbTarget: Single;

    FLowDbCur,
    FMidDbCur,
    FHighDbCur: Single;

    FRampMode: TMfRampMode;
    FRampTimeMs: Integer;
    FRampSamplesLeft: Integer;

    FLowShelfS: Single;
    FHighShelfS: Single;
    FMidMode: TMfMidMode;

    // Mixer-style default frequencies.
    FLowFreqHz: Single;
    FMidFreqHz: Single;
    FHighFreqHz: Single;
    FMidQ: Single;

    // Biquads per channel: LowShelf -> MidBell -> HighShelf.
    FLowL,
    FLowR:
    TBiquad;
    FMidL,
    FMidR: TBiquad;

    FHighL,
    FHighR: TBiquad;

    procedure StepRampLocked(const Frames: Integer);
    procedure UpdateCoeffsLocked();

    procedure ComputeLowShelf(const GainDb,
                              FcHz: Double;
                              out Bq: TBiquad);

    procedure ComputeHighShelf(const GainDb,
                               FcHz: Double;
                               out Bq: TBiquad);

    procedure ComputePeaking(const GainDb,
                             FcHz,
                             Q: Double;
                             out Bq: TBiquad);

    function CurrentSampleRate(): Double;

    function GetBiquadCoeffs(out Low,
                             Mid,
                             High: TBiquadCoeffs;
                             out SampleRate: Double): HRESULT;

  protected

    procedure ClearStateLocked(); override;
    procedure OnFormatChangedLocked(); override;
    function ProcessAudioLocked(pData: PByte;
                                cbData: UINT32): HRESULT; override;

  public

    constructor Create;

    function ProcessInPlace(pData: PByte;
                            cbData: UINT32): HRESULT;

    // IMfHighMidLowControl
    function SetEnabled(const AEnabled: Boolean): HRESULT; stdcall;

    function SetLowDb(const Db: Single): HRESULT; stdcall;
    function SetMidDb(const Db: Single): HRESULT; stdcall;
    function SetHighDb(const Db: Single): HRESULT; stdcall;

    function SetLowFreqHz(const Hz: Single): HRESULT; stdcall;
    function SetMidFreqHz(const Hz: Single): HRESULT; stdcall;
    function SetHighFreqHz(const Hz: Single): HRESULT; stdcall;
    function SetMidQ(const Q: Single): HRESULT; stdcall;

    function SetLowShelfSlope(const S: Single): HRESULT; stdcall;
    function SetHighShelfSlope(const S: Single): HRESULT; stdcall;
    function SetMidMode(const Mode: TMfMidMode): HRESULT; stdcall;


    function SetRampMode(const Mode: TMfRampMode): HRESULT; stdcall;
    function SetRampTimeMs(const Ms: Integer): HRESULT; stdcall;


  end;

function CreateHighMidLowMFT(out pMft: IMFTransform): HRESULT;

implementation

{ TBiquad }

procedure TBiquad.Reset;
begin

  x1 := 0;
  x2 := 0;
  y1 := 0;
  y2 := 0;
end;


procedure TBiquad.SetCoeffs(const Aa0, Aa1, Aa2, Bb1, Bb2: Double);
begin

  a0 := Aa0;
  a1 := Aa1;
  a2 := Aa2;
  b1 := Bb1;
  b2 := Bb2;
end;



function TBiquad.Process(const x: Double): Double;
var
  y: Double;

begin
  y := a0*x + a1*x1 + a2*x2 - b1*y1 - b2*y2;
  x2 := x1;
  x1 := x;
  y2 := y1;
  y1 := y;
  Result := y;
end;


function DbToA(const Db: Double): Double;
begin
  // Source: RBJ cookbook: A = 10^(dBgain/40)
  Result := Power(10.0, Db / 40.0);
end;


{ THighMidLowMFT }

constructor THighMidLowMFT.Create;
begin
  inherited Create;

  // default mixer tuning
  FLowFreqHz := 100.0;
  FMidFreqHz := 1000.0;
  FHighFreqHz := 10000.0;
  FMidQ := 1.0;

  FEnabled := True;

  FLowDbTarget := 0;
  FMidDbTarget := 0;
  FHighDbTarget := 0;

  FLowDbCur := 0;
  FMidDbCur := 0;
  FHighDbCur := 0;

  FRampMode := rmSmooth;
  FRampTimeMs := 30;
  FRampSamplesLeft := 0;
end;


function THighMidLowMFT.ProcessInPlace(pData: PByte;
                                       cbData: Cardinal): HRESULT;
begin

  FLock.Enter();

  try
    Result := ProcessAudioLocked(pData,
                                 cbData);
  finally

    FLock.Leave();
  end;
end;


function THighMidLowMFT.CurrentSampleRate(): Double;
begin

  if (FSampleRate <> 0) then
    Result := FSampleRate * 1.0
  else
    Result := 48000.0;
end;


function THighMidLowMFT.GetBiquadCoeffs(out Low,
                                        Mid,
                                        High: TBiquadCoeffs;
                                        out SampleRate: Double): HRESULT;
begin

  FLock.Enter();

  try

    Low.a0 := FLowL.a0;
    Low.a1 := FLowL.a1;
    Low.a2 := FLowL.a2;
    Low.b1 := FLowL.b1;
    Low.b2 := FLowL.b2;

    Mid.a0 := FMidL.a0;
    Mid.a1 := FMidL.a1;
    Mid.a2 := FMidL.a2;
    Mid.b1 := FMidL.b1;
    Mid.b2 := FMidL.b2;

    High.a0 := FHighL.a0;
    High.a1 := FHighL.a1;
    High.a2 := FHighL.a2;
    High.b1 := FHighL.b1;
    High.b2 := FHighL.b2;

    SampleRate := FSampleRate;

    Result := S_OK;

  finally

    FLock.Leave();
  end;
end;


procedure THighMidLowMFT.ClearStateLocked();
begin

  FLowL.Reset;
  FLowR.Reset;
  FMidL.Reset;
  FMidR.Reset;
  FHighL.Reset;
  FHighR.Reset;
end;


procedure THighMidLowMFT.OnFormatChangedLocked();
begin

  ClearStateLocked();
  FRampSamplesLeft := 0;
  UpdateCoeffsLocked();
end;


procedure THighMidLowMFT.StepRampLocked(const Frames: Integer);
var
  rampMs: Integer;
  step: Single;
  sr: Integer;

  function ModeToMs(const Mode: TMfRampMode): Integer;
    begin

      case Mode of
        rmOff:    Result := 0;
        rmFast:   Result := 10;
        rmSmooth: Result := 30;
        rmCustom: Result := FRampTimeMs;
      else
        Result := 30;
      end;
  end;

begin

  rampMs := ModeToMs(FRampMode);

  if (rampMs <= 0) then
    begin

      FLowDbCur := FLowDbTarget;
      FMidDbCur := FMidDbTarget;
      FHighDbCur := FHighDbTarget;
      FRampSamplesLeft := 0;
      Exit;
    end;

  sr := Round(CurrentSampleRate);

  if (FRampSamplesLeft <= 0) then
    FRampSamplesLeft := Max(1,
                            (rampMs * sr) div 1000);

  step := Min(1.0,
              Frames / (FRampSamplesLeft * 1.0));

  FLowDbCur  := FLowDbCur + (FLowDbTarget - FLowDbCur) * step;
  FMidDbCur  := FMidDbCur + (FMidDbTarget - FMidDbCur) * step;
  FHighDbCur := FHighDbCur + (FHighDbTarget - FHighDbCur) * step;

  FRampSamplesLeft := Max(0,
                          FRampSamplesLeft - Frames);

  UpdateCoeffsLocked();
end;


procedure THighMidLowMFT.UpdateCoeffsLocked();
begin

  ComputeLowShelf(FLowDbCur,
                  FLowFreqHz,
                  FLowL);

  ComputeLowShelf(FLowDbCur,
                  FLowFreqHz,
                  FLowR);

  ComputePeaking(FMidDbCur,
                 FMidFreqHz,
                 FMidQ,
                 FMidL);

  //{$IFDEF DEBUG}
  //OutputDebugString(PWideChar(Format('UpdateCoeffs: MidHz=%.1f MidDb=%.1f Q=%.2f',
  //                                   [FMidFreqHz, FMidDbCur, FMidQ])));
  //{$ENDIF}

  ComputePeaking(FMidDbCur,
                 FMidFreqHz,
                 FMidQ,
                 FMidR);

  ComputeHighShelf(FHighDbCur,
                   FHighFreqHz,
                   FHighL);

  ComputeHighShelf(FHighDbCur,
                   FHighFreqHz,
                   FHighR);
end;


procedure THighMidLowMFT.ComputeLowShelf(const GainDb, FcHz: Double; out Bq: TBiquad);
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
  sr: Double;

begin

  sr := CurrentSampleRate;

  A := DbToA(GainDb);
  w0 := 2 * PI * FcHz / sr;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  // S = 1.0 shelving slope.
  alpha := sinw0 / 2 * Sqrt((A + 1 / A) * 1.0 - 2.0);
  sqrtA := Sqrt(A);

  b0 := A *( (A + 1) - (A - 1) * cosw0 + 2 * sqrtA * alpha);
  b1 := 2 * A * ((A - 1) - (A + 1) * cosw0);
  b2 := A * ((A + 1) - (A - 1) * cosw0 - 2 * sqrtA * alpha);
  a0 := (A+1) + (A-1) * cosw0 + 2 * sqrtA * alpha;
  a1 := -2 * ((A-1) + (A+1) * cosw0 );
  a2 := (A + 1) + ( A - 1) * cosw0 - 2 * sqrtA * alpha;

  Bq.SetCoeffs(b0 / a0,
               b1 / a0,
               b2 / a0,
               a1 / a0,
               a2 / a0);

end;


procedure THighMidLowMFT.ComputeHighShelf(const GainDb, FcHz: Double; out Bq: TBiquad);
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
  sr: Double;

begin

  sr := CurrentSampleRate;

  A := DbToA(GainDb);
  w0 := 2 * PI * FcHz / sr;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  alpha := sinw0 / 2 * Sqrt((A + 1 / A) * 1.0 - 2.0);
  sqrtA := Sqrt(A);

  b0 := A*((A + 1) + (A - 1) * cosw0 + 2 * sqrtA * alpha);
  b1 := -2 * A * ((A-1) + (A + 1) * cosw0);
  b2 := A * ((A + 1) + (A - 1) * cosw0 - 2 * sqrtA * alpha);
  a0 := (A + 1) - (A - 1) * cosw0 + 2 * sqrtA * alpha;
  a1 := 2 * ((A - 1) - (A + 1) * cosw0 );
  a2 := (A + 1) - (A - 1) * cosw0 - 2 * sqrtA * alpha;

  Bq.SetCoeffs(b0 / a0,
               b1 / a0,
               b2 / a0,
               a1 / a0,
               a2 / a0);

end;


procedure THighMidLowMFT.ComputePeaking(const GainDb,
                                        FcHz,
                                        Q: Double;
                                        out Bq: TBiquad);
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
  a2,
  qq: Double;
  sr: Double;

begin

  sr := CurrentSampleRate();
  qq := Q;

  if (qq <= 0.0) then
    qq := 1.0;

  A := DbToA(GainDb);
  w0 := 2 * PI * FcHz / sr;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);
  alpha := sinw0 / (2 * qq);

  // Source: RBJ Audio EQ Cookbook: Peaking EQ
  b0 := 1 + alpha * A;
  b1 := -2 * cosw0;
  b2 := 1 - alpha * A;
  a0 := 1 + alpha / A;
  a1 := -2 * cosw0;
  a2 := 1 - alpha / A;

  Bq.SetCoeffs(b0 / a0,
               b1 / a0,
               b2 / a0,
               a1 / a0,
               a2 / a0);

end;


function THighMidLowMFT.ProcessAudioLocked(pData: PByte; cbData: Cardinal): HRESULT;
var
  frames, i, c: Integer;
  ch: Integer;
  ps16: PSmallInt;
  pf32: PSingle;
  x, y: Double;
  s: Integer;

begin

  if (not FEnabled) or (pData = nil) or (cbData = 0) then
    Exit(S_OK);

  if (FChannels = 0) or (FBlockAlign = 0) then
    Exit(S_OK);

  frames := Integer(cbData div FBlockAlign);
  if (frames <= 0) then
    Exit(S_OK);

  StepRampLocked(frames);
  ch := Integer(FChannels);

  if (FBitsPerSample = 16) then
    begin

      ps16 := PSmallInt(pData);

      for i := 0 to frames - 1 do
        begin

          // Channel 0
          x := ps16^ / 32768.0;
          y := FLowL.Process(x);
          y := FMidL.Process(y);
          y := FHighL.Process(y);
          s := Round(y * 32768.0);
          if (s < -32768) then
            s := -32768
          else
            if (s > 32767) then
              s := 32767;

      ps16^ := s;
      Inc(ps16);

      // Channel 1 if present
      if (ch > 1) then
        begin

          x := ps16^ / 32768.0;
          y := FLowR.Process(x);
          y := FMidR.Process(y);
          y := FHighR.Process(y);
          s := Round(y * 32768.0);
          if (s < -32768) then
            s := -32768
          else
            if (s > 32767) then
              s := 32767;

        ps16^ := s;
        Inc(ps16);
        end;

      // remaining channels pass-through
      for c := 2 to ch - 1 do
        Inc(ps16);
    end;

    Exit(S_OK);
  end;

  if (FBitsPerSample = 32) then
    begin

      // Teaching assumption: 32-bit PCM is float32.
      pf32 := PSingle(pData);

      for i := 0 to frames - 1 do
        begin

          x := pf32^;
          y := FLowL.Process(x);
          y := FMidL.Process(y);
          y := FHighL.Process(y);
          pf32^ := y;
          Inc(pf32);

          if (ch > 1) then
            begin

              x := pf32^;
              y := FLowR.Process(x);
              y := FMidR.Process(y);
              y := FHighR.Process(y);
              pf32^ := y;
              Inc(pf32);
            end;

      for c := 2 to ch - 1 do
        Inc(pf32);
    end;

    Exit(S_OK);
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetEnabled(const AEnabled: Boolean): HRESULT;
begin

  FLock.Enter();

  try

    FEnabled := AEnabled;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function THighMidLowMFT.SetLowDb(const Db: Single): HRESULT;
begin

  FLock.Enter();

  try

    FLowDbTarget := ClampS(Db,
                           -24.0,
                           24.0);
    FRampSamplesLeft := 0;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function THighMidLowMFT.SetMidDb(const Db: Single): HRESULT;
begin

  FLock.Enter();

  try

    FMidDbTarget := ClampS(Db,
                           -24.0,
                           24.0);
    FRampSamplesLeft := 0;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function THighMidLowMFT.SetHighDb(const Db: Single): HRESULT;
begin

  FLock.Enter();

  try
    FHighDbTarget := ClampS(Db,
                            -24.0,
                            24.0);
    FRampSamplesLeft := 0;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function THighMidLowMFT.SetLowFreqHz(const Hz: Single): HRESULT;
begin

  FLock.Enter();

  try
    FLowFreqHz := ClampS(Hz,
                         10.0,
                         400.0);
    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetMidFreqHz(const Hz: Single): HRESULT;
begin

  FLock.Enter();

  try

    FMidFreqHz := ClampS(Hz,
                         200.0,
                         6000.0);
    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetHighFreqHz(const Hz: Single): HRESULT;
begin
  FLock.Enter();
  try
    FHighFreqHz := ClampS(Hz,
                          2000.0,
                          22000.0);
    UpdateCoeffsLocked();
  finally
    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetMidQ(const Q: Single): HRESULT;
begin

  FLock.Enter();

  try

    FMidQ := ClampS(Q,
                    0.3,
                    6.0);
    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetLowShelfSlope(const S: Single): HRESULT;
begin

  FLock.Enter;

  try

    // Source: RBJ shelf slope S: sensible range
    FLowShelfS := ClampS(S,
                         0.1,
                         4.0);
    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetHighShelfSlope(const S: Single): HRESULT;
begin

  FLock.Enter;

  try

    FHighShelfS := ClampS(S,
                          0.1,
                          4.0);
    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetMidMode(const Mode: TMfMidMode): HRESULT;
begin

  FLock.Enter;

  try
    FMidMode := Mode;

    // If you ramp gains, switching filter type should update immediately
    FRampSamplesLeft := 0;

    UpdateCoeffsLocked();
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;



function THighMidLowMFT.SetRampMode(const Mode: TMfRampMode): HRESULT;
begin

  FLock.Enter();

  try

    FRampMode := Mode;
    FRampSamplesLeft := 0;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function THighMidLowMFT.SetRampTimeMs(const Ms: Integer): HRESULT;
begin

  FLock.Enter();

  try

    FRampTimeMs := ClampI(Ms,
                          0,
                          2000);
    FRampSamplesLeft := 0;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function CreateHighMidLowMFT(out pMft: IMFTransform): HRESULT;
begin

  pMft := THighMidLowMFT.Create as IMFTransform;

  Result := S_OK;
end;

end.

