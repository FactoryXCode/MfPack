// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfMicrophoneFx.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Microphone effects.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX400
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
unit MfMicrophoneFx;

interface

uses

  {Winapi}
  Winapi.Windows,
  {System}
  System.SysUtils,
  System.Math;

type

  TMfMicrophoneFxHelper = record
  public
    class function ClampFloat(const AValue,
                              AMin,
                              AMax: Single): Single; static; inline;
    class function DbToLinear(const ADb: Single): Single; static; inline;
    class function LinearToDb(const AValue: Single): Single; static; inline;
    class function DenormalKill(const AValue: Single): Single; static; inline;
  end;


  TMfSimpleMicCompressor = class
  private
    FEnabled: Boolean;
    FSampleRate: Single;
    FThresholdDb: Single;
    FRatio: Single;
    FAttackMs: Single;
    FReleaseMs: Single;
    FMakeupDb: Single;
    FSoftKneeDb: Single;
    FEnvelope: Single;
    FGainSmoothed: Single;
    FAttackCoeff: Single;
    FReleaseCoeff: Single;
    procedure UpdateTimeConstants;
    function ComputeDesiredGainLinear(const AInputAbs: Single): Single;

  public

    constructor Create();
    procedure Reset();

    procedure SetSampleRate(const ASampleRate: Single);
    procedure ProcessStereoInterleaved(ABuffer: PSingle;
                                       const AFrames: Integer);
  //published

    property Enabled: Boolean read FEnabled write FEnabled;
    property ThresholdDb: Single read FThresholdDb write FThresholdDb;
    property Ratio: Single read FRatio write FRatio;
    property AttackMs: Single read FAttackMs write FAttackMs;
    property ReleaseMs: Single read FReleaseMs write FReleaseMs;
    property MakeupDb: Single read FMakeupDb write FMakeupDb;
    property SoftKneeDb: Single read FSoftKneeDb write FSoftKneeDb;
  end;


  TMfSimpleNoiseGate = class
  private

    FEnabled: Boolean;
    FSampleRate: Single;
    FThresholdDb: Single;
    FAttackMs: Single;
    FReleaseMs: Single;
    FFloorDb: Single;
    FHoldMs: Single;
    FEnvelope: Single;
    FGainSmoothed: Single;
    FAttackCoeff: Single;
    FReleaseCoeff: Single;
    FHoldSamples: Single;
    FHoldCounter: Integer;
    procedure UpdateTimeConstants();

  public

    constructor Create();
    procedure Reset();

    procedure SetSampleRate(const ASampleRate: Single);
    procedure ProcessStereoInterleaved(ABuffer: PSingle;
                                       const AFrames: Integer);
  //published

    property Enabled: Boolean read FEnabled write FEnabled;
    property ThresholdDb: Single read FThresholdDb write FThresholdDb;
    property AttackMs: Single read FAttackMs write FAttackMs;
    property ReleaseMs: Single read FReleaseMs write FReleaseMs;
    property FloorDb: Single read FFloorDb write FFloorDb;
    property HoldMs: Single read FHoldMs write FHoldMs;
  end;



  TMfSpringEcho = class
  private

    FEnabled: Boolean;
    FSampleRate: Integer;
    FMix: Single;
    FDelayMs: Single;
    FFeedback: Single;
    FTone: Single;
    FSpring: Single;
    FWowDepthMs: Single;
    FWowRateHz: Single;
    FBufferL: array of Single;
    FBufferR: array of Single;
    FWritePos: Integer;
    FLowPassStateL: Single;
    FLowPassStateR: Single;
    FHighPassStateL: Single;
    FHighPassStateR: Single;
    FHighPassPrevInL: Single;
    FHighPassPrevInR: Single;
    FAllPassState1L: Single;
    FAllPassState1R: Single;
    FAllPassState2L: Single;
    FAllPassState2R: Single;
    FLfoPhase: Single;

    procedure AllocateBuffer;
    function GetDelaySamplesBase: Single;
    function ReadDelaySample(const ABuffer: array of Single;
                             const AReadPos: Single): Single;
    function ProcessOneAllPass(const AInput: Single;
                               var AState: Single;
                               const ACoeff: Single): Single;
    function ProcessToneLowPass(const AInput: Single;
                                var AState: Single): Single;
    function ProcessToneHighPass(const AInput: Single;
                                 var AState,
                                     APrevIn: Single): Single;
  public

    constructor Create();
    procedure Reset();

    procedure SetSampleRate(const ASampleRate: Integer);
    procedure ProcessStereoInterleaved(ABuffer: PSingle;
                                       const AFrames: Integer);
  //published

    property Enabled: Boolean read FEnabled write FEnabled;
    property Mix: Single read FMix write FMix;
    property DelayMs: Single read FDelayMs write FDelayMs;
    property Feedback: Single read FFeedback write FFeedback;
    property Tone: Single read FTone write FTone;
    property Spring: Single read FSpring write FSpring;
    property WowDepthMs: Single read FWowDepthMs write FWowDepthMs;
    property WowRateHz: Single read FWowRateHz write FWowRateHz;
  end;


implementation


{ TMfMicrophoneFxHelper }

class function TMfMicrophoneFxHelper.ClampFloat(const AValue,
                                                AMin,
                                                AMax: Single): Single;
begin

  if (AValue < AMin) then
    Exit(AMin);

  if (AValue > AMax) then
    Exit(AMax);

  Result := AValue;
end;


class function TMfMicrophoneFxHelper.DbToLinear(const ADb: Single): Single;
begin

  Result := Power(10.0,
                  ADb / 20.0);
end;


class function TMfMicrophoneFxHelper.LinearToDb(const AValue: Single): Single;
const
  CMinLinear = 1.0E-12;

begin

  Result := 20.0 * Log10(Max(Abs(AValue),
                         CMinLinear));
end;


class function TMfMicrophoneFxHelper.DenormalKill(const AValue: Single): Single;
begin

  if (Abs(AValue) < 1.0E-20) then
    Exit(0.0);

  Result := AValue;
end;


{ TMfSimpleMicCompressor }

constructor TMfSimpleMicCompressor.Create();
begin

  inherited Create;

  FEnabled := True;
  FSampleRate := 44100.0;
  FThresholdDb := -18.0;
  FRatio := 2.5;
  FAttackMs := 8.0;
  FReleaseMs := 120.0;
  FMakeupDb := 2.0;
  FSoftKneeDb := 4.0;
  Reset;
  UpdateTimeConstants;
end;


procedure TMfSimpleMicCompressor.Reset();
begin

  FEnvelope := 0.0;
  FGainSmoothed := 1.0;
end;


procedure TMfSimpleMicCompressor.SetSampleRate(const ASampleRate: Single);
begin

  if (ASampleRate <= 1000.0) then
    Exit;

  FSampleRate := ASampleRate;
  UpdateTimeConstants();
end;


procedure TMfSimpleMicCompressor.UpdateTimeConstants;
var
  AttackSec: Single;
  ReleaseSec: Single;

begin

  AttackSec := Max(0.001,
                   FAttackMs * 0.001);
  ReleaseSec := Max(0.001,
                    FReleaseMs * 0.001);

  FAttackCoeff := Exp(-1.0 / (AttackSec * FSampleRate));
  FReleaseCoeff := Exp(-1.0 / (ReleaseSec * FSampleRate));
end;


function TMfSimpleMicCompressor.ComputeDesiredGainLinear(const AInputAbs: Single): Single;
var
  InDb: Single;
  OverDb: Single;
  OutDb: Single;
  GainReductionDb: Single;
  HalfKnee: Single;
  X: Single;

begin

  InDb := TMfMicrophoneFxHelper.LinearToDb(Max(AInputAbs,
                                               1.0E-12));
  HalfKnee := Max(0.0,
                  FSoftKneeDb * 0.5);

  if (HalfKnee > 0.0) then
    begin

      if (InDb <= (FThresholdDb - HalfKnee)) then
        GainReductionDb := 0.0
      else
        if (InDb >= (FThresholdDb + HalfKnee)) then
          begin

            OverDb := InDb - FThresholdDb;
            OutDb := FThresholdDb + (OverDb / Max(FRatio, 1.0));
            GainReductionDb := OutDb - InDb;
          end
        else
          begin

            X := InDb - (FThresholdDb - HalfKnee);
            GainReductionDb := (1.0 / Max(FRatio, 1.0) - 1.0) * Sqr(X) / Max(2.0 * FSoftKneeDb,
                                                                                   1.0E-9);
          end;
    end
  else
    begin

     if (InDb <= FThresholdDb) then
        GainReductionDb := 0.0
      else
        begin

          OverDb := InDb - FThresholdDb;
          OutDb := FThresholdDb + (OverDb / Max(FRatio, 1.0));
          GainReductionDb := OutDb - InDb;
        end;
    end;

  Result := TMfMicrophoneFxHelper.DbToLinear(GainReductionDb + FMakeupDb);
end;


procedure TMfSimpleMicCompressor.ProcessStereoInterleaved(ABuffer: PSingle;
                                                          const AFrames: Integer);
var
  I: Integer;
  L: Single;
  R: Single;
  InputAbs: Single;
  DesiredGain: Single;
  DetectorCoeff: Single;

begin

  if (ABuffer = nil) or
     (AFrames <= 0) then
    Exit;

  FThresholdDb := TMfMicrophoneFxHelper.ClampFloat(FThresholdDb,
                                                   -60.0,
                                                   0.0);
  FRatio := TMfMicrophoneFxHelper.ClampFloat(FRatio,
                                             1.0,
                                             20.0);
  FAttackMs := TMfMicrophoneFxHelper.ClampFloat(FAttackMs,
                                                0.1,
                                                250.0);
  FReleaseMs := TMfMicrophoneFxHelper.ClampFloat(FReleaseMs,
                                                 5.0,
                                                 2000.0);
  FMakeupDb := TMfMicrophoneFxHelper.ClampFloat(FMakeupDb,
                                                -12.0,
                                                24.0);
  FSoftKneeDb := TMfMicrophoneFxHelper.ClampFloat(FSoftKneeDb,
                                                  0.0,
                                                  24.0);
  UpdateTimeConstants();

  for I := 0 to AFrames - 1 do
    begin
      L := ABuffer^;
      Inc(ABuffer);

      R := ABuffer^;
      Inc(ABuffer);

      InputAbs := Max(Abs(L),
                      Abs(R));

      if (InputAbs > FEnvelope) then
        DetectorCoeff := FAttackCoeff
      else
        DetectorCoeff := FReleaseCoeff;

      FEnvelope := InputAbs + DetectorCoeff * (FEnvelope - InputAbs);
      FEnvelope := TMfMicrophoneFxHelper.DenormalKill(FEnvelope);

      if FEnabled then
        DesiredGain := ComputeDesiredGainLinear(FEnvelope)
      else
        DesiredGain := 1.0;

      if (DesiredGain < FGainSmoothed) then
        DetectorCoeff := FAttackCoeff
      else
        DetectorCoeff := FReleaseCoeff;

      FGainSmoothed := DesiredGain + DetectorCoeff * (FGainSmoothed - DesiredGain);
      FGainSmoothed := TMfMicrophoneFxHelper.DenormalKill(FGainSmoothed);

      Dec(ABuffer,
          2);
      ABuffer^ := L * FGainSmoothed;
      Inc(ABuffer);
      ABuffer^ := R * FGainSmoothed;
      Inc(ABuffer);
    end;
end;


{ TMfSimpleNoiseGate }

constructor TMfSimpleNoiseGate.Create;
begin

  inherited Create;

  FEnabled := True;
  FSampleRate := 44100.0;
  FThresholdDb := -45.0;
  FAttackMs := 5.0;
  FReleaseMs := 120.0;
  FFloorDb := -80.0;
  FHoldMs := 35.0;
  Reset();
  UpdateTimeConstants();
end;


procedure TMfSimpleNoiseGate.Reset();
begin

  FEnvelope := 0.0;
  FGainSmoothed := 1.0;
  FHoldCounter := 0;
end;


procedure TMfSimpleNoiseGate.SetSampleRate(const ASampleRate: Single);
begin

  if (ASampleRate <= 1000.0) then
    Exit;

  FSampleRate := ASampleRate;
  UpdateTimeConstants;
end;


procedure TMfSimpleNoiseGate.UpdateTimeConstants();
var
  AttackSec: Single;
  ReleaseSec: Single;

begin

  AttackSec := Max(0.001,
                   FAttackMs * 0.001);
  ReleaseSec := Max(0.001,
                    FReleaseMs * 0.001);

  FAttackCoeff := Exp(-1.0 / (AttackSec * FSampleRate));
  FReleaseCoeff := Exp(-1.0 / (ReleaseSec * FSampleRate));

  FHoldMs := TMfMicrophoneFxHelper.ClampFloat(FHoldMs,
                                              0.0,
                                              500.0);
  FHoldSamples := Round((FHoldMs * 0.001) * FSampleRate);
  if (FHoldSamples < 0) then
    FHoldSamples := 0;
end;

procedure TMfSimpleNoiseGate.ProcessStereoInterleaved(ABuffer: PSingle;
                                                      const AFrames: Integer);
var
  I: Integer;
  L: Single;
  R: Single;
  InputAbs: Single;
  DetectorCoeff: Single;
  InDb: Single;
  DesiredGain: Single;
  FloorLinear: Single;
  GateOpen: Boolean;
begin

  if (ABuffer = nil) or
     (AFrames <= 0) then
    Exit;

  FThresholdDb := TMfMicrophoneFxHelper.ClampFloat(FThresholdDb,
                                                   -90.0,
                                                   0.0);
  FAttackMs := TMfMicrophoneFxHelper.ClampFloat(FAttackMs,
                                                0.1,
                                                250.0);
  FReleaseMs := TMfMicrophoneFxHelper.ClampFloat(FReleaseMs,
                                                 5.0,
                                                 3000.0);
  FFloorDb := TMfMicrophoneFxHelper.ClampFloat(FFloorDb,
                                               -90.0,
                                               0.0);
  FHoldMs := TMfMicrophoneFxHelper.ClampFloat(FHoldMs,
                                              0.0,
                                              500.0);
  UpdateTimeConstants();

  FloorLinear := TMfMicrophoneFxHelper.DbToLinear(FFloorDb);

  for I := 0 to AFrames - 1 do
    begin

      L := ABuffer^;
      Inc(ABuffer);

      R := ABuffer^;
      Inc(ABuffer);

      InputAbs := Max(Abs(L), Abs(R));

      if (InputAbs > FEnvelope) then
        DetectorCoeff := FAttackCoeff
      else
        DetectorCoeff := FReleaseCoeff;

      FEnvelope := InputAbs + DetectorCoeff * (FEnvelope - InputAbs);
      FEnvelope := TMfMicrophoneFxHelper.DenormalKill(FEnvelope);

      if FEnabled then
        begin
          InDb := TMfMicrophoneFxHelper.LinearToDb(Max(FEnvelope, 1.0E-12));
          GateOpen := (InDb >= FThresholdDb);

          if GateOpen then
            begin

              FHoldCounter := Round(FHoldSamples);
              DesiredGain := 1.0;
            end
          else
            begin

              if (FHoldCounter > 0) then
                begin

                  Dec(FHoldCounter);
                  DesiredGain := 1.0;
                end
              else
                DesiredGain := FloorLinear;
            end;
        end
      else
        begin

          FHoldCounter := 0;
          DesiredGain := 1.0;
        end;

      if (DesiredGain > FGainSmoothed) then
        DetectorCoeff := FAttackCoeff
      else
        DetectorCoeff := FReleaseCoeff;

      FGainSmoothed := DesiredGain + DetectorCoeff * (FGainSmoothed - DesiredGain);
      FGainSmoothed := TMfMicrophoneFxHelper.DenormalKill(FGainSmoothed);

      Dec(ABuffer,
          2);

      ABuffer^ := L * FGainSmoothed;
      Inc(ABuffer);
      ABuffer^ := R * FGainSmoothed;
      Inc(ABuffer);
    end;
end;


{ TMfSpringEcho }

constructor TMfSpringEcho.Create();
begin

  inherited Create;

  FEnabled := True;
  FSampleRate := 44100;
  FMix := 0.14;
  FDelayMs := 95.0;
  FFeedback := 0.22;
  FTone := 0.45;
  FSpring := 0.35;
  FWowDepthMs := 0.7;
  FWowRateHz := 0.45;
  Reset;
  AllocateBuffer;
end;


procedure TMfSpringEcho.SetSampleRate(const ASampleRate: Integer);
begin

  if (ASampleRate < 8000) then
    Exit;

  FSampleRate := ASampleRate;
  AllocateBuffer();
  Reset;
end;


procedure TMfSpringEcho.AllocateBuffer;
var
  BufferFrames: Integer;

begin

  BufferFrames := Max(FSampleRate * 3,
                      16384);

  SetLength(FBufferL, BufferFrames);
  SetLength(FBufferR, BufferFrames);

  FillChar(FBufferL[0],
           Length(FBufferL) * SizeOf(Single),
           0);
  FillChar(FBufferR[0],
           Length(FBufferR) * SizeOf(Single),
           0);

  FWritePos := 0;
end;


procedure TMfSpringEcho.Reset;
begin

  FLowPassStateL := 0.0;
  FLowPassStateR := 0.0;
  FHighPassStateL := 0.0;
  FHighPassStateR := 0.0;
  FHighPassPrevInL := 0.0;
  FHighPassPrevInR := 0.0;
  FAllPassState1L := 0.0;
  FAllPassState1R := 0.0;
  FAllPassState2L := 0.0;
  FAllPassState2R := 0.0;
  FLfoPhase := 0.0;
end;


function TMfSpringEcho.GetDelaySamplesBase: Single;
begin

  Result := (FDelayMs * 0.001) * FSampleRate;
  Result := TMfMicrophoneFxHelper.ClampFloat(Result,
                                             16.0,
                                             Max(32.0,
                                                 Length(FBufferL) - 8.0));
end;


function TMfSpringEcho.ReadDelaySample(const ABuffer: array of Single;
                                       const AReadPos: Single): Single;
var
  Pos0: Integer;
  Pos1: Integer;
  Frac: Single;
  BufferLen: Integer;

begin

  BufferLen := Length(ABuffer);

  if (BufferLen = 0) then
    Exit(0.0);

  Pos0 := Floor(AReadPos);
  Frac := AReadPos - Pos0;

  while (Pos0 < 0) do
    Inc(Pos0, BufferLen);

  while (Pos0 >= BufferLen) do
    Dec(Pos0, BufferLen);

  Pos1 := Pos0 + 1;
  if (Pos1 >= BufferLen) then
    Pos1 := 0;

  Result := ABuffer[Pos0] + (ABuffer[Pos1] - ABuffer[Pos0]) * Frac;
end;


function TMfSpringEcho.ProcessOneAllPass(const AInput: Single;
                                         var AState: Single;
                                         const ACoeff: Single): Single;
var
  Y: Single;

begin

  Y := -ACoeff * AInput + AState;
  AState := AInput + ACoeff * Y;
  Result := Y;
end;


function TMfSpringEcho.ProcessToneLowPass(const AInput: Single;
                                          var AState: Single): Single;
var
  Alpha: Single;
  CutBlend: Single;

begin

  CutBlend := TMfMicrophoneFxHelper.ClampFloat(FTone,
                                               0.0,
                                               1.0);
  Alpha := 0.04 + (0.24 * CutBlend);
  AState := AState + Alpha * (AInput - AState);
  AState := TMfMicrophoneFxHelper.DenormalKill(AState);
  Result := AState;
end;


function TMfSpringEcho.ProcessToneHighPass(const AInput: Single;
                                           var AState,
                                           APrevIn: Single): Single;
var
  Alpha: Single;
  PrevInLocal: Single;

begin

  Alpha := 0.992;
  PrevInLocal := APrevIn;
  APrevIn := AInput;
  Result := Alpha * (AState + AInput - PrevInLocal);
  AState := Result;
end;


procedure TMfSpringEcho.ProcessStereoInterleaved(ABuffer: PSingle;
                                                 const AFrames: Integer);
var
  I: Integer;
  InL: Single;
  InR: Single;
  DryL: Single;
  DryR: Single;
  DelayBase: Single;
  DelayMod: Single;
  ReadPos: Single;
  WetL: Single;
  WetR: Single;
  FbL: Single;
  FbR: Single;
  SpringAmt: Single;
  MixAmt: Single;
  FeedbackAmt: Single;
  BufferLen: Integer;
  LfoValue: Single;
  MonoTap: Single;

begin

  if (ABuffer = nil) or
     (AFrames <= 0) or
     (Length(FBufferL) = 0) or
     (Length(FBufferR) = 0) then
    Exit;

  FMix := TMfMicrophoneFxHelper.ClampFloat(FMix,
                                           0.0,
                                           1.0);
  FDelayMs := TMfMicrophoneFxHelper.ClampFloat(FDelayMs,
                                               20.0,
                                               500.0);
  FFeedback := TMfMicrophoneFxHelper.ClampFloat(FFeedback,
                                                0.0,
                                                0.92);
  FTone := TMfMicrophoneFxHelper.ClampFloat(FTone,
                                            0.0,
                                            1.0);
  FSpring := TMfMicrophoneFxHelper.ClampFloat(FSpring,
                                              0.0,
                                              1.0);
  FWowDepthMs := TMfMicrophoneFxHelper.ClampFloat(FWowDepthMs,
                                                  0.0,
                                                  8.0);
  FWowRateHz := TMfMicrophoneFxHelper.ClampFloat(FWowRateHz,
                                                 0.01,
                                                 8.0);

  if not FEnabled then
    Exit;

  DelayBase := GetDelaySamplesBase;
  BufferLen := Length(FBufferL);
  SpringAmt := FSpring;
  MixAmt := FMix;
  FeedbackAmt := FFeedback;

  for I := 0 to AFrames - 1 do
    begin

      InL := ABuffer^;
      Inc(ABuffer);

      InR := ABuffer^;
      Inc(ABuffer);

      DryL := InL;
      DryR := InR;

      LfoValue := Sin(FLfoPhase * 2.0 * Pi);
      FLfoPhase := FLfoPhase + (FWowRateHz / FSampleRate);
      if (FLfoPhase >= 1.0) then
        FLfoPhase := FLfoPhase - 1.0;

      DelayMod := (FWowDepthMs * 0.001 * FSampleRate) * LfoValue;
      ReadPos := FWritePos - (DelayBase + DelayMod);

      WetL := ReadDelaySample(FBufferL,
                              ReadPos);
      WetR := ReadDelaySample(FBufferR,
                              ReadPos);

      MonoTap := 0.5 * (WetL + WetR);

      WetL := WetL + (0.18 * MonoTap);
      WetR := WetR + (0.18 * MonoTap);

      WetL := ProcessOneAllPass(WetL,
                                FAllPassState1L,
                                0.55 - 0.20 * SpringAmt);
      WetR := ProcessOneAllPass(WetR,
                                FAllPassState1R,
                                0.55 - 0.20 * SpringAmt);

      WetL := ProcessOneAllPass(WetL,
                                FAllPassState2L,
                                0.42 - 0.12 * SpringAmt);
      WetR := ProcessOneAllPass(WetR,
                                FAllPassState2R,
                                0.42 - 0.12 * SpringAmt);

      WetL := ProcessToneHighPass(WetL,
                                  FHighPassStateL,
                                  FHighPassPrevInL);
      WetR := ProcessToneHighPass(WetR,
                                  FHighPassStateR,
                                  FHighPassPrevInR);

      FHighPassPrevInL := WetL;
      FHighPassPrevInR := WetR;

      WetL := ProcessToneLowPass(WetL,
                                 FLowPassStateL);
      WetR := ProcessToneLowPass(WetR,
                                 FLowPassStateR);

      WetL := Tanh(WetL * (1.0 + 0.8 * SpringAmt));
      WetR := Tanh(WetR * (1.0 + 0.8 * SpringAmt));

      FbL := DryL + (WetL * FeedbackAmt);
      FbR := DryR + (WetR * FeedbackAmt);

      FBufferL[FWritePos] := FbL;
      FBufferR[FWritePos] := FbR;

      Dec(ABuffer, 2);
      ABuffer^ := DryL * (1.0 - MixAmt) + WetL * MixAmt;
      Inc(ABuffer);
      ABuffer^ := DryR * (1.0 - MixAmt) + WetR * MixAmt;
      Inc(ABuffer);

      Inc(FWritePos);
      if (FWritePos >= BufferLen) then
        FWritePos := 0;
    end;
end;

end.
