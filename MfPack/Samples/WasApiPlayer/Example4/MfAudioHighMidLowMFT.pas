// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEqBaseMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
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
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example3
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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



  PCoefSet = ^TCoefSet;
  TCoefSet = record
    Low:  TBiquadCoeffs;
    Mid:  TBiquadCoeffs;
    High: TBiquadCoeffs;
    SampleRate: Double;
  end;

  THighMidLowMFT = class(TMfAudioEqBaseMFT, IMfHighMidLowControl)
  private

    // NOTE: This MFT is used directly in a realtime WASAPI render loop.
    // The audio thread must never block. Therefore: NO blocking locks in ProcessInPlace().

    // Atomics (written by UI/control thread, read by audio thread).
    FEnabledI: Integer;

    FLowDbTargetBits: Integer;
    FMidDbTargetBits: Integer;
    FHighDbTargetBits: Integer;

    FLowFreqBits: Integer;
    FMidFreqBits: Integer;
    FHighFreqBits: Integer;
    FMidQBits: Integer;

    FLowShelfSBits: Integer;
    FHighShelfSBits: Integer;

    FMidModeI: Integer;

    FRampModeI: Integer;
    FRampTimeMsI: Integer;

    // Parameter generation counter (incremented on any setter).
    FParamGen: Integer;

    // Request from control thread to restart ramp immediately.
    FResetRampI: Integer;

    // Audio-thread only smoothing state.
    FLowDbCur: Single;
    FMidDbCur: Single;
    FHighDbCur: Single;
    FRampSamplesLeft: Integer;

    // Published coefficient snapshot for GUI/diagnostics (lock-free read via pointer).
    FCoefsA: TCoefSet;
    FCoefsB: TCoefSet;
    FCoefsPtr: Pointer;


    // Biquads per channel: LowShelf -> MidBell -> HighShelf.
    FLowL,
    FLowR:
    TBiquad;
    FMidL,
    FMidR: TBiquad;

    FHighL,
    FHighR: TBiquad;

    procedure StepRampRT(const Frames: Integer);
    procedure UpdateCoeffsRT();

    procedure ComputeLowShelf(const GainDb,
                              FcHz,
                              Slope: Double;
                              out Bq: TBiquad);

    procedure ComputeHighShelf(const GainDb,
                               FcHz,
                               Slope: Double;
                               out Bq: TBiquad);

    procedure ComputePeaking(const GainDb,
                             FcHz,
                             Q: Double;
                             out Bq: TBiquad);

    function CurrentSampleRate(): Double;

    function GetBiquadCoeffs(out ALow,
                             AMid,
                             AHigh: TBiquadCoeffs;
                             out ASampleRate: Double): HRESULT;

  protected

    procedure ClearStateLocked(); override;
    procedure OnFormatChangedLocked(); override;
    function ProcessAudioLocked(pData: PByte;
                                cbData: UINT32): HRESULT; override;

  

    // Lock-free (realtime) processing path.
    function ProcessAudioRT(pData: PByte;
                            cbData: UINT32): HRESULT;
public

    constructor Create();

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


function SingleToBits(const V: Single): Integer; inline;
begin

  Move(V,
       Result, SizeOf(Result));
end;


function BitsToSingle(const B: Integer): Single; inline;
begin

  Move(B,
       Result,
       SizeOf(Result));
end;


function AtomicLoadInt(var V: Integer): Integer; inline;
begin

  Result := InterlockedCompareExchange(V,
                                       0,
                                       0);
end;


procedure AtomicStoreInt(var V: Integer;
                         const NewValue: Integer); inline;
begin

  InterlockedExchange(V,
                      NewValue);
end;


function AtomicLoadBool(var V: Integer): Boolean; inline;
begin

  Result := (InterlockedCompareExchange(V,
                                        0,
                                        0) <> 0);
end;


procedure AtomicStoreBool(var V: Integer;
                          const B: Boolean); inline;
begin

  if B then
    InterlockedExchange(V,
                        1)
  else
    InterlockedExchange(V,
                        0);
end;


function AtomicLoadSingleBits(var VBits: Integer): Single; inline;
begin

  Result := BitsToSingle(InterlockedCompareExchange(VBits,
                                                    0,
                                                    0));
end;


procedure AtomicStoreSingleBits(var VBits: Integer;
                                const V: Single); inline;
begin

  InterlockedExchange(VBits,
                      SingleToBits(V));
end;


function AtomicLoadPtr(var P: Pointer): Pointer; inline;
begin

  Result := InterlockedCompareExchangePointer(P,
                                              nil,
                                              nil);
end;


procedure AtomicStorePtr(var P: Pointer;
                         const NewValue: Pointer); inline;
begin

  InterlockedExchangePointer(P,
                             NewValue);
end;


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

  // Biquad (Direct Form I) with denormal protection.
  y := a0*x + a1*x1 + a2*x2 - b1*y1 - b2*y2;

  // Portable denormal flush (FTZ/DAZ is even better, but thread-local).
  if (Abs(y) < 1.0e-30) then
    y := 0.0;

  x2 := x1;
  x1 := x;

  if (Abs(y1) < 1.0e-30) then
    y1 := 0.0;
  if (Abs(y2) < 1.0e-30) then
    y2 := 0.0;

  y2 := y1;
  y1 := y;

  Result := y;
end;


function DbToA(const Db: Double): Double; inline;
begin

  // Source: RBJ cookbook: A = 10^(dBgain/40)
  Result := Power(10.0,
                  Db / 40.0);
end;


{ THighMidLowMFT }

constructor THighMidLowMFT.Create();
begin

  inherited Create;

  // Default mixer tuning.
  AtomicStoreSingleBits(FLowFreqBits,
                        100.0);

  AtomicStoreSingleBits(FMidFreqBits,
                        1000.0);

  AtomicStoreSingleBits(FHighFreqBits,
                        10000.0);

  AtomicStoreSingleBits(FMidQBits,
                        1.0);

  AtomicStoreBool(FEnabledI, True);

  AtomicStoreSingleBits(FLowDbTargetBits,
                        0.0);

  AtomicStoreSingleBits(FMidDbTargetBits,
                        0.0);

  AtomicStoreSingleBits(FHighDbTargetBits,
                        0.0);

  // Shelf slopes (RBJ: S = 1.0 is a good default).
  AtomicStoreSingleBits(FLowShelfSBits,
                        1.0);

  AtomicStoreSingleBits(FHighShelfSBits,
                        1.0);

  AtomicStoreInt(FMidModeI,
                 0); // default

  AtomicStoreInt(FRampModeI,
                 Ord(rmSmooth));

  AtomicStoreInt(FRampTimeMsI,
                 30);

  InterlockedExchange(FParamGen, 1);
  InterlockedExchange(FResetRampI, 0);

  // Audio-thread smoothing state.
  FLowDbCur := 0.0;
  FMidDbCur := 0.0;
  FHighDbCur := 0.0;
  FRampSamplesLeft := 0;

  // Published coefficient snapshot pointer.
  FCoefsPtr := @FCoefsA;
  FillChar(FCoefsA,
           SizeOf(FCoefsA),
           0);
  FillChar(FCoefsB,
           SizeOf(FCoefsB),
           0);
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


function THighMidLowMFT.GetBiquadCoeffs(out ALow,
                                        AMid,
                                        AHigh: TBiquadCoeffs;
                                        out ASampleRate: Double): HRESULT;
var
  P: PCoefSet;

begin

  // Lock-free snapshot for diagnostics/UI.
  P := PCoefSet(AtomicLoadPtr(FCoefsPtr));

  if (P = nil) then
    begin

      FillChar(ALow,
               SizeOf(ALow),
               0);

      FillChar(AMid,
               SizeOf(AMid),
               0);

      FillChar(AHigh,
               SizeOf(AHigh),
               0);

      ASampleRate := 0.0;
      Exit(S_OK);
    end;

  ALow := P^.Low;
  AMid := P^.Mid;
  AHigh := P^.High;
  ASampleRate := P^.SampleRate;

  Result := S_OK;
end;


procedure THighMidLowMFT.ClearStateLocked();
begin

  FLowL.Reset();
  FLowR.Reset();
  FMidL.Reset();
  FMidR.Reset();
  FHighL.Reset();
  FHighR.Reset();
end;


procedure THighMidLowMFT.OnFormatChangedLocked();
begin

  ClearStateLocked();
  FRampSamplesLeft := 0;
  UpdateCoeffsRT();
end;


procedure THighMidLowMFT.StepRampRT(const Frames: Integer);
var
  rampMs: Integer;
  step: Single;
  sr: Integer;
  mode: TMfRampMode;
  tgtLow,
  tgtMid,
  tgtHigh: Single;

  // Helper
  function ModeToMs(const Mode: TMfRampMode): Integer;
  begin

    case Mode of
      rmOff: Result := 0;
      rmFast: Result := 10;
      rmSmooth: Result := 30;
      rmCustom: Result := AtomicLoadInt(FRampTimeMsI);
    else
      Result := 30;
    end;
  end;

begin

  if (InterlockedExchange(FResetRampI, 0) <> 0) then
    FRampSamplesLeft := 0;

  mode := TMfRampMode(AtomicLoadInt(FRampModeI));
  rampMs := ModeToMs(mode);

  tgtLow := AtomicLoadSingleBits(FLowDbTargetBits);
  tgtMid := AtomicLoadSingleBits(FMidDbTargetBits);
  tgtHigh := AtomicLoadSingleBits(FHighDbTargetBits);

  if (rampMs <= 0) then
    begin

      FLowDbCur := tgtLow;
      FMidDbCur := tgtMid;
      FHighDbCur := tgtHigh;
      FRampSamplesLeft := 0;
      Exit;
    end;

  sr := Round(CurrentSampleRate);

  if (FRampSamplesLeft <= 0) then
    FRampSamplesLeft := Max(1,
                            (rampMs * sr) div 1000);

  step := Min(1.0,
              Frames / (FRampSamplesLeft * 1.0));

  FLowDbCur := FLowDbCur + (tgtLow - FLowDbCur) * step;
  FMidDbCur := FMidDbCur + (tgtMid - FMidDbCur) * step;
  FHighDbCur := FHighDbCur + (tgtHigh - FHighDbCur) * step;

  FRampSamplesLeft := Max(0,
                      FRampSamplesLeft - Frames);
end;


procedure THighMidLowMFT.UpdateCoeffsRT();
var
  lowHz,
  midHz,
  highHz: Double;
  q: Double;
  sLow,
  sHigh: Double;
  newPtr,
  curPtr: PCoefSet;

begin

  lowHz := AtomicLoadSingleBits(FLowFreqBits);
  midHz := AtomicLoadSingleBits(FMidFreqBits);
  highHz := AtomicLoadSingleBits(FHighFreqBits);
  q := AtomicLoadSingleBits(FMidQBits);

  sLow := AtomicLoadSingleBits(FLowShelfSBits);
  sHigh := AtomicLoadSingleBits(FHighShelfSBits);

  ComputeLowShelf(FLowDbCur,
                  lowHz,
                  sLow,
                  FLowL);

  ComputeLowShelf(FLowDbCur,
                  lowHz,
                  sLow,
                  FLowR);

  // Mid mode hook (currently peaking-only).
  ComputePeaking(FMidDbCur,
                 midHz,
                 q,
                 FMidL);

  ComputePeaking(FMidDbCur,
                 midHz,
                 q,
                 FMidR);

  ComputeHighShelf(FHighDbCur,
                  highHz,
                  sHigh,
                  FHighL);

  ComputeHighShelf(FHighDbCur,
                   highHz,
                   sHigh,
                   FHighR);

  // Publish coefficient snapshot (lock-free).
  curPtr := PCoefSet(AtomicLoadPtr(FCoefsPtr));

  if (curPtr = @FCoefsA) then
    newPtr := @FCoefsB
  else
    newPtr := @FCoefsA;

  newPtr^.Low.a0 := FLowL.a0;
  newPtr^.Low.a1 := FLowL.a1;
  newPtr^.Low.a2 := FLowL.a2;
  newPtr^.Low.b1 := FLowL.b1;
  newPtr^.Low.b2 := FLowL.b2;

  newPtr^.Mid.a0 := FMidL.a0;
  newPtr^.Mid.a1 := FMidL.a1;
  newPtr^.Mid.a2 := FMidL.a2;
  newPtr^.Mid.b1 := FMidL.b1;
  newPtr^.Mid.b2 := FMidL.b2;

  newPtr^.High.a0 := FHighL.a0;
  newPtr^.High.a1 := FHighL.a1;
  newPtr^.High.a2 := FHighL.a2;
  newPtr^.High.b1 := FHighL.b1;
  newPtr^.High.b2 := FHighL.b2;

  newPtr^.SampleRate := CurrentSampleRate();

  AtomicStorePtr(FCoefsPtr,
                 newPtr);
end;


procedure THighMidLowMFT.ComputeLowShelf(const GainDb,
                                         FcHz,
                                         Slope: Double;
                                         out Bq: TBiquad);
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
  slSlope: Double;

begin

  sr := CurrentSampleRate();
  slSlope := Slope;

  A := DbToA(GainDb);
  w0 := 2 * PI * FcHz / sr;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  // RBJ cookbook shelf slope parameter S (1.0 is a good default).
  if (slSlope <= 0.0) then
    slSlope := 1.0;

  alpha := sinw0 / 2 * Sqrt((A + 1 / A) * (1 / slSlope - 1) + 2.0);
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


procedure THighMidLowMFT.ComputeHighShelf(const GainDb,
                                          FcHz,
                                          Slope: Double;
                                          out Bq: TBiquad);
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
  slSlope: Double;

begin

  sr := CurrentSampleRate;
  slSlope := Slope;

  A := DbToA(GainDb);
  w0 := 2 * PI * FcHz / sr;
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  // RBJ cookbook shelf slope parameter S (1.0 is a good default).
  if (slSlope <= 0.0) then
    slSlope := 1.0;

  alpha := sinw0 / 2 * Sqrt((A + 1 / A) * (1 / slSlope - 1) + 2.0);
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


function THighMidLowMFT.ProcessAudioLocked(pData: PByte;
                                cbData: Cardinal): HRESULT;
begin

  // Base-class (MFT) processing path (called under FLock).
  Result := ProcessAudioRT(pData, cbData);
end;


function THighMidLowMFT.ProcessAudioRT(pData: PByte;
                                       cbData: Cardinal): HRESULT;
var
  frames,
  i,
  c: Integer;
  ch: Integer;
  ps16: PSmallInt;
  pf32: PSingle;
  x,
  y: Double;
  s16: Integer;

begin

  if (not AtomicLoadBool(FEnabledI)) or
     (pData = nil) or
     (cbData = 0) then
    Exit(S_OK);

  if (FChannels = 0) or (FBlockAlign = 0) then
    Exit(S_OK);

  frames := Integer(cbData div FBlockAlign);
  if (frames <= 0) then
    Exit(S_OK);

  StepRampRT(frames);
  UpdateCoeffsRT();

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

          s16 := Round(y * 32768.0);
          if (s16 < -32768) then
            s16 := -32768
          else
            if (s16 > 32767) then
              s16 := 32767;

          ps16^ := s16;
          Inc(ps16);

          // Channel 1 if present
          if (ch > 1) then
            begin

              x := ps16^ / 32768.0;
              y := FLowR.Process(x);
              y := FMidR.Process(y);
              y := FHighR.Process(y);

              s16 := Round(y * 32768.0);
              if (s16 < -32768) then
                s16 := -32768
              else
                if (s16 > 32767) then
                  s16 := 32767;

              ps16^ := s16;
              Inc(ps16);
            end;

          // Remaining channels pass-through
          for c := 2 to ch - 1 do
            Inc(ps16);
        end;

      Exit(S_OK);
    end;

  if (FBitsPerSample = 32) then
    begin

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

  AtomicStoreBool(FEnabledI,
                  AEnabled);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetLowDb(const Db: Single): HRESULT;
begin

  AtomicStoreSingleBits(FLowDbTargetBits,
                        ClampS(Db,
                               -24.0,
                               24.0));

  InterlockedExchange(FResetRampI,
                      1);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetMidDb(const Db: Single): HRESULT;
begin

  AtomicStoreSingleBits(FMidDbTargetBits,
                        ClampS(Db,
                               -24.0,
                               24.0));

  InterlockedExchange(FResetRampI,
                      1);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetHighDb(const Db: Single): HRESULT;
begin

  AtomicStoreSingleBits(FHighDbTargetBits,
                        ClampS(Db,
                               -24.0,
                               24.0));

  InterlockedExchange(FResetRampI,
                      1);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetLowFreqHz(const Hz: Single): HRESULT;
begin

  AtomicStoreSingleBits(FLowFreqBits,
                        ClampS(Hz,
                               10.0,
                               400.0));

  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetMidFreqHz(const Hz: Single): HRESULT;
begin

  AtomicStoreSingleBits(FMidFreqBits,
                        ClampS(Hz,
                               10.0,
                               22000.0));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetHighFreqHz(const Hz: Single): HRESULT;
begin

  AtomicStoreSingleBits(FHighFreqBits, ClampS(Hz,
                                              10.0,
                                              22000.0));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetMidQ(const Q: Single): HRESULT;
begin

  AtomicStoreSingleBits(FMidQBits, ClampS(Q,
                                          0.10,
                                          10.0));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetLowShelfSlope(const S: Single): HRESULT;
begin

  AtomicStoreSingleBits(FLowShelfSBits, ClampS(S,
                                               0.10,
                                               5.0));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetHighShelfSlope(const S: Single): HRESULT;
begin

  AtomicStoreSingleBits(FHighShelfSBits,
                        ClampS(S,
                               0.10,
                               5.0));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetMidMode(const Mode: TMfMidMode): HRESULT;
begin

  AtomicStoreInt(FMidModeI,
                 Ord(Mode));
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;



function THighMidLowMFT.SetRampMode(const Mode: TMfRampMode): HRESULT;
begin

  AtomicStoreInt(FRampModeI,
                 Ord(Mode));

  InterlockedExchange(FResetRampI,
                      1);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function THighMidLowMFT.SetRampTimeMs(const Ms: Integer): HRESULT;
begin

  AtomicStoreInt(FRampTimeMsI,
                 ClampI(Ms,
                        0,
                        5000));

  InterlockedExchange(FResetRampI,
                      1);
  InterlockedIncrement(FParamGen);
  Result := S_OK;
end;


function CreateHighMidLowMFT(out pMft: IMFTransform): HRESULT;
begin

  pMft := THighMidLowMFT.Create as IMFTransform;

  Result := S_OK;
end;

end.
