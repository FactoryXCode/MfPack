// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfLoopbackDeckEngine.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Loopback engine to capture soundcards audio.
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
//          Please, read documentation carefully!
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
unit MfLoopbackDeckEngine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  {WinMM}
  WinApi.WinMM.MMReg,
  WinApi.WinMM.MMeApi,
  {Application}
  LoopBackCapture,
  MfWasApiEffectsRack,
  MfParametricEqComponent;

type

  TSingleDynArray = array of Single;

  TWasApiDeviceState = (dsUnknown,
                        dsReady,
                        dsPlay,
                        dsStop);

  TMfLoopbackCaptureMode = (lcmIncludeProcessTree,
                            lcmExcludeProcessTree);

  TWasApiStateEvent = procedure(Sender: TObject;
                                const AState: TWasApiDeviceState) of object;

  TLoopbackDeckTickEvent = procedure(Sender: TObject;
                                     const Position100ns: Int64;
                                     const CurrentBpm: Double;
                                     const BeatPhase: Double) of object;

  TLoopbackDeckBeatEvent = procedure(Sender: TObject;
                                     const Position100ns: Int64;
                                     const BeatNumber: Int64;
                                     const CurrentBpm: Double) of object;

  TMfLoopbackDeckEngine = class
  private

    FLoopback: TLoopbackCapture;
    FLock: TCriticalSection;

    FRing: TBytes;
    FRingSize: Integer;
    FReadPos: Integer;
    FWritePos: Integer;
    FQueuedBytes: Integer;
    FConvertBuf: TBytes;
    FSilentFloatBuf: TBytes;
    FCaptureDropBytes: Int64;
    FCaptureSilentBytes: Int64;

    FSampleRate: Integer;
    FChannels: Integer;
    FBlockAlign: Integer;

    FTempoPercent: Single;
    FTempoFactor: Double;
    FTargetTempoFactor: Double;
    FCurrentTempoFactor: Double;
    FResampleFrac: Double;

    FPrebufferBytes: Integer;
    FStartedOutput: Boolean;

    FTrackBpm: Double;
    FCurrentBpm: Double;
    FPosition100ns: Int64;
    FBeatOffset100ns: Int64;
    FLastBeatIndex: Int64;
    FLastTick100ns: Int64;

    FState: TWasApiDeviceState;
    FOnState: TWasApiStateEvent;
    FOnDeckTick: TLoopbackDeckTickEvent;
    FOnBeat: TLoopbackDeckBeatEvent;
    // Gain
    FInputGainDb: Single;
    FInputGainLinear: Single;

    FProcessId: DWORD;
    FIncludeProcessTree: Boolean;
    FInitialBufferSize: REFERENCE_TIME;
    FActive: Boolean;

    FAudioRack: TMfWasApiEffectsRack;
    FDefaultEq: TMfParametricEqEffect;

    // realtime beat detector
    FAnalysisPrevMono: Single;
    FAnalysisHopFrames: Integer;
    FAnalysisHopPos: Integer;
    FAnalysisAcc: Double;
    FAnalysisEnergyAcc: Double;
    FAnalysisSilenceFloor: Double;
    FBassLowState: Double;
    FBassSubLowState: Double;
    FBassEnv: Double;
    FBassPrevEnv: Double;
    FEnvelopeRate: Integer;
    FEnvelopeBins: TSingleDynArray;
    FEnvelopeCapacity: Integer;
    FLastAnalyzePos100ns: Int64;

    procedure InitRing(const ASize: Integer);
    procedure ResetRing();
    procedure ResetBeatDetector();
    procedure AppendEnvelopeBin(const AValue: Single);
    procedure ProcessBeatDetector(const pData: PSingle;
                                  const Frames: Integer;
                                  const EndPosition100ns: Int64);
    procedure InternalEnqueueBytes(const pData: PByte;
                                   const ByteCount: Integer);
    procedure EnsureConvertBuffer(const ByteCount: Integer);
    procedure EnsureSilentFloatBuffer(const ByteCount: Integer);
    procedure EnqueueCapturedSilence(const CaptureByteCount: Integer;
                                     pwfx: PWAVEFORMATEX);
    procedure ConvertPcm16StereoToFloat32(const pSrc: PByte;
                                          const SrcByteCount: Integer);
    function InternalPeekStereoFrame(const AFrameIndex: Integer;
                                     out AL: Single;
                                     out AR: Single): Boolean;
    procedure InternalConsumeBytes(const AByteCount: Integer);

    function IsPcm16Stereo(const pwfx: PWAVEFORMATEX): Boolean;
    function IsFloat32Stereo(const pwfx: PWAVEFORMATEX): Boolean;
    procedure UpdateCurrentBpm();
    procedure DoBpmTracking(const FramesWritten: Integer);
    procedure SetTrackBpm(const AValue: Double);
    // Gain
    procedure SetInputGainDb(const Value: Single);

    procedure SetState(const AState: TWasApiDeviceState);
    procedure DoState();

  public

    constructor Create();
    destructor Destroy(); override;

    function PrepareProcess(const AProcessId: DWORD;
                            const ACaptureMode: TMfLoopbackCaptureMode = lcmIncludeProcessTree;
                            const AInitialBufferSize: REFERENCE_TIME = 0): HRESULT;

    function Start(): HRESULT;
    function Stop(): HRESULT;

    function ReadOutputPcmFloat32(const Frames: Integer;
                                  const OutBuffer: PSingle;
                                  out Flags: DWORD): HRESULT;

    function GetTempoPercent(): Single;
    function SetTempoPercent(const AValue: Single): HRESULT;
    function GetTempoFactor(): Double;
    function GetCurrentBpm(): Double;

    procedure LoopbackCapturedPcm(Sender: TObject;
                                  pData: PByte;
                                  ByteCount: DWORD;
                                  pwfx: PWAVEFORMATEX;
                                  const CaptureFlags: DWORD);

    property Active: Boolean read FActive;
    property AudioRack: TMfWasApiEffectsRack read FAudioRack;
    property TrackBpm: Double read FTrackBpm write SetTrackBpm;

    property InputGainDb: Single read FInputGainDb write SetInputGainDb;

    property BeatOffset100ns: Int64 read FBeatOffset100ns write FBeatOffset100ns;
    property Position100ns: Int64 read FPosition100ns;

    property State: TWasApiDeviceState read FState;
    property OnState: TWasApiStateEvent read FOnState write FOnState;
    property OnDeckTick: TLoopbackDeckTickEvent read FOnDeckTick write FOnDeckTick;
    property OnBeat: TLoopbackDeckBeatEvent read FOnBeat write FOnBeat;
  end;


implementation

uses
  RDJ_Common;

// Helpers ---------------------------------------------------------------------
procedure SmoothEnvelope(var AEnvelope: TSingleDynArray;
                         const ACount: Integer);
var
  Tmp: TSingleDynArray;
  I: Integer;

begin

  if (ACount < 5) then
    Exit;

  SetLength(Tmp,
            ACount);

  Tmp[0] := AEnvelope[0];
  Tmp[1] := (AEnvelope[0] + AEnvelope[1] + AEnvelope[2]) / 3.0;

  for I := 2 to ACount - 3 do
    Tmp[I] := (AEnvelope[I - 2] +
               AEnvelope[I - 1] +
               AEnvelope[I] +
               AEnvelope[I + 1] +
               AEnvelope[I + 2]) / 5.0;

  Tmp[ACount - 2] := (AEnvelope[ACount - 3] +
                      AEnvelope[ACount - 2] +
                      AEnvelope[ACount - 1]) / 3.0;

  Tmp[ACount - 1] := AEnvelope[ACount - 1];

  Move(Tmp[0],
       AEnvelope[0],
       ACount * SizeOf(Single));
end;


function NormalizeDetectedBpm(const ABpm: Double): Double;
begin

  Result := ABpm;

  while (Result < 80.0) do
    Result := Result * 2.0;

  while (Result > 160.0) do
    Result := Result / 2.0;
end;


function DetectBpmFromEnvelope(const AEnvelope: TSingleDynArray;
                               const ACount: Integer;
                               const AEnvelopeRate: Integer;
                               out ADetectedBpm: Double;
                               out ABeatOffset100ns: Int64): Boolean;
const
  MIN_BPM = 70.0;
  MAX_BPM = 180.0;
  PREFERRED_MIN_BPM = 80.0;
  PREFERRED_MAX_BPM = 160.0;

var
  MinLag: Integer;
  MaxLag: Integer;
  Lag: Integer;
  I: Integer;
  Score: Double;
  BestScore: Double;
  BestLag: Integer;
  Bpm: Double;
  BeatBins: Double;
  PhaseBins: Integer;
  Phase: Integer;
  PhaseScore: Double;
  BestPhase: Integer;
  Pos: Double;
  Step: Double;
  Idx: Integer;

begin

  Result := False;

  ADetectedBpm := 0.0;
  ABeatOffset100ns := 0;

  if (ACount < (AEnvelopeRate * 8)) then
    Exit;

  MinLag := Round((AEnvelopeRate * 60.0) / MAX_BPM);
  MaxLag := Round((AEnvelopeRate * 60.0) / MIN_BPM);

  MinLag := ClampInt(MinLag,
                     1,
                     ACount - 1);

  MaxLag := ClampInt(MaxLag,
                     MinLag,
                     ACount - 1);

  BestScore := -1.0E300;
  BestLag := 0;

  for Lag := MinLag to MaxLag do
    begin

      Score := 0.0;

      for I := 0 to ACount - Lag - 1 do
        Score := Score + (AEnvelope[I] * AEnvelope[I + Lag]);

      Bpm := (60.0 * AEnvelopeRate) / Lag;
      if (Bpm >= 95.0) and (Bpm <= 140.0) then
        Score := Score * 1.05;

      if (Score > BestScore) then
        begin

          BestScore := Score;
          BestLag := Lag;
        end;
    end;

  if (BestLag <= 0) then
    Exit;

  Bpm := (60.0 * AEnvelopeRate) / BestLag;

  while (Bpm < PREFERRED_MIN_BPM) do
    Bpm := Bpm * 2.0;

  while (Bpm > PREFERRED_MAX_BPM) do
    Bpm := Bpm / 2.0;

  BeatBins := (AEnvelopeRate * 60.0) / Bpm;
  if (BeatBins <= 1.0) then
    Exit;

  PhaseBins := Max(1,
                   Round(BeatBins));

  BestPhase := 0;
  BestScore := -1.0E300;

  for Phase := 0 to PhaseBins - 1 do
    begin

      PhaseScore := 0.0;
      Pos := Phase;
      Step := BeatBins;

      while (Pos < ACount) do
        begin

          Idx := Round(Pos);

          if (Idx >= 0) and (Idx < ACount) then
            PhaseScore := PhaseScore + AEnvelope[Idx];

          Pos := Pos + Step;
        end;

      if (PhaseScore > BestScore) then
        begin

          BestScore := PhaseScore;
          BestPhase := Phase;
        end;
    end;

  ADetectedBpm := Bpm;
  ABeatOffset100ns := Round((BestPhase / AEnvelopeRate) * 10000000.0);
  Result := (ADetectedBpm > 0.0);
end;


function NormalizeBeatOffsetToPosition(const AOffset100ns: Int64;
                                       const ABeatLength100ns: Double;
                                       const APosition100ns: Int64): Int64;
begin

  Result := AOffset100ns;

  if (ABeatLength100ns <= 0.0) then
    Exit;

  while (Result > APosition100ns) do
    Result := Round(Result - ABeatLength100ns);

  while ((Result + Round(ABeatLength100ns)) <= APosition100ns) do
    Result := Round(Result + ABeatLength100ns);
end;

function WrapPhaseError100ns(const AError100ns: Int64;
                               const ABeatLength100ns: Double): Double;
begin
  Result := AError100ns;

  if (ABeatLength100ns <= 0.0) then
    Exit;

  while (Result > (ABeatLength100ns * 0.5)) do
    Result := Result - ABeatLength100ns;

  while (Result < -(ABeatLength100ns * 0.5)) do
    Result := Result + ABeatLength100ns;
end;

function ClampDouble(const AValue, AMin, AMax: Double): Double;
begin
  Result := AValue;
  if (Result < AMin) then
    Result := AMin
  else if (Result > AMax) then
    Result := AMax;
end;


procedure ApplyGainFloat32(const ABuffer: PSingle;
                           const AFrames: Integer;
                           const AChannels: Integer;
                           const AGainLinear: Single);
var
  i: Integer;
  SampleCount: Integer;
  P: PSingle;

begin

  if (ABuffer = nil) or
     (AFrames <= 0) or
     (AChannels <= 0) then
    Exit;

  if (Abs(AGainLinear - 1.0) < 1E-6) then
    Exit;

  SampleCount := AFrames * AChannels;
  P := ABuffer;

  for i := 0 to SampleCount - 1 do
    begin

      P^ := P^ * AGainLinear;
      Inc(P);
    end;
end;
// Helpers end -----------------------------------------------------------------

{ TMfLoopbackDeckEngine }

constructor TMfLoopbackDeckEngine.Create();
begin

  inherited Create();

  FLoopback := nil;
  FAudioRack := TMfWasApiEffectsRack.Create(nil);
  FDefaultEq := TMfParametricEqEffect.Create(nil);

  with TMfWasApiFxSlot(FAudioRack.Slots.Add) do
    begin

      Effect := FDefaultEq;
      Enabled := True;
    end;

  FDefaultEq.Enabled := True;
  FDefaultEq.GainDb := 0.0;
  FDefaultEq.CenterFreqHz := 1500.0;
  FDefaultEq.Q := 1.0;

  FLock := TCriticalSection.Create();

  FSampleRate := 44100;
  FChannels := 2;
  FBlockAlign := FChannels * SizeOf(Single);

  FProcessId := 0;
  FIncludeProcessTree := True;
  FInitialBufferSize := 0;
  FActive := False;
  FCaptureDropBytes := 0;
  FCaptureSilentBytes := 0;

  FTempoPercent := 0.0;
  FTempoFactor := 1.0;
  FTargetTempoFactor := 1.0;
  FCurrentTempoFactor := 1.0;
  FResampleFrac := 0.0;

  FPrebufferBytes := (FSampleRate * FBlockAlign) div 2;
  FStartedOutput := False;

  FTrackBpm := 0.0;
  FCurrentBpm := 0.0;
  FPosition100ns := 0;
  FBeatOffset100ns := 0;
  FLastBeatIndex := -1;
  FLastTick100ns := -1;
  // Gain
  FInputGainDb := 0.0;
  FInputGainLinear := 1.0;

  FState := dsStop;
  FOnState := nil;
  FOnDeckTick := nil;
  FOnBeat := nil;

  FAnalysisPrevMono := 0.0;
  FAnalysisHopFrames := 1024;
  FAnalysisHopPos := 0;
  FAnalysisAcc := 0.0;
  FAnalysisEnergyAcc := 0.0;
  FAnalysisSilenceFloor := 0.0;
  FBassLowState := 0.0;
  FBassSubLowState := 0.0;
  FBassEnv := 0.0;
  FBassPrevEnv := 0.0;
  FEnvelopeRate := Max(1,
                       Round(FSampleRate / FAnalysisHopFrames));
  FEnvelopeCapacity := FEnvelopeRate * 24;
  SetLength(FEnvelopeBins,
            0);
  FLastAnalyzePos100ns := 0;

  InitRing(FSampleRate * FBlockAlign * 8);
end;


destructor TMfLoopbackDeckEngine.Destroy();
begin

  Stop();
  FreeAndNil(FLoopback);
  FreeAndNil(FDefaultEq);
  FreeAndNil(FAudioRack);
  FreeAndNil(FLock);
  FRing := nil;
  FConvertBuf := nil;
  FSilentFloatBuf := nil;
  inherited Destroy();
end;


procedure TMfLoopbackDeckEngine.InitRing(const ASize: Integer);
begin

  FRingSize := ASize;
  SetLength(FRing,
            FRingSize);
  ResetRing();
end;


procedure TMfLoopbackDeckEngine.ResetRing();
begin

  FLock.Acquire();

  try

    FReadPos := 0;
    FWritePos := 0;
    FQueuedBytes := 0;
  finally

    FLock.Release();
  end;
end;


procedure TMfLoopbackDeckEngine.ResetBeatDetector();
begin

  FLock.Acquire();

  try
    FAnalysisPrevMono := 0.0;
    FAnalysisHopPos := 0;
    FAnalysisAcc := 0.0;
    FAnalysisEnergyAcc := 0.0;
    FAnalysisSilenceFloor := 0.0;
    FBassLowState := 0.0;
    FBassSubLowState := 0.0;
    FBassEnv := 0.0;
    FBassPrevEnv := 0.0;
    SetLength(FEnvelopeBins,
              0);
    FLastAnalyzePos100ns := 0;
  finally

    FLock.Release();
  end;
end;

procedure TMfLoopbackDeckEngine.AppendEnvelopeBin(const AValue: Single);
var
  Count: Integer;

begin

  Count := Length(FEnvelopeBins);

  if (Count >= FEnvelopeCapacity) and (FEnvelopeCapacity > 1) then
    begin
      Move(FEnvelopeBins[1],
           FEnvelopeBins[0],
           (Count - 1) * SizeOf(Single));
      FEnvelopeBins[Count - 1] := AValue;
      Exit;
    end;

  SetLength(FEnvelopeBins,
            Count + 1);
  FEnvelopeBins[Count] := AValue;
end;


procedure TMfLoopbackDeckEngine.ProcessBeatDetector(const pData: PSingle;
                                                    const Frames: Integer;
                                                    const EndPosition100ns: Int64);
var
  I: Integer;
  InPtr: PSingle;
  L: Single;
  R: Single;
  Mono: Single;
  Bass: Double;
  Rectified: Double;
  Onset: Double;
  HopOnsetAvg: Double;
  HopOnsetValue: Single;
  HopEnergyAvg: Double;
  GateThreshold: Double;
  EnvCopy: TSingleDynArray;
  EnvCount: Integer;
  DetectedBpm: Double;
  DetectedOffsetRel100ns: Int64;
  WindowDuration100ns: Int64;
  WindowStart100ns: Int64;
  NewBeatOffset100ns: Int64;
  BeatLen100ns: Double;
  CurrentTrackBpm: Double;
  StableOffsetAtPos: Int64;
  DetectedOffsetAtPos: Int64;
  PhaseError100ns: Double;
  SnapDeadband100ns: Double;
  MaxAdjust100ns: Double;
  AppliedAdjust100ns: Double;

begin

  if (pData = nil) or (Frames <= 0) then
    Exit;

  InPtr := pData;

  FLock.Acquire();

  try

    for I := 0 to Frames - 1 do
      begin

        L := InPtr^;
        Inc(InPtr);
        R := InPtr^;
        Inc(InPtr);

        Mono := (L + R) * 0.5;

        // Tighter kick-focused cheap band-pass:
        // fast LP ~140 Hz minus slower LP ~55 Hz.
        FBassLowState := FBassLowState + (0.020 * (Mono - FBassLowState));
        FBassSubLowState := FBassSubLowState + (0.0078 * (Mono - FBassSubLowState));
        Bass := FBassLowState - FBassSubLowState;

        Rectified := Abs(Bass);

        // Envelope follower: fast attack, slower release.
        if (Rectified > FBassEnv) then
          FBassEnv := FBassEnv + (0.10 * (Rectified - FBassEnv))
        else
          FBassEnv := FBassEnv + (0.015 * (Rectified - FBassEnv));

        // Onset emphasis: respond to rising bass energy, not sustained bass loudness.
        Onset := FBassEnv - FBassPrevEnv;
        if (Onset < 0.0) then
          Onset := 0.0;

        FBassPrevEnv := FBassEnv;

        FAnalysisAcc := FAnalysisAcc + Onset;
        FAnalysisEnergyAcc := FAnalysisEnergyAcc + FBassEnv;
        Inc(FAnalysisHopPos);

        if (FAnalysisHopPos >= FAnalysisHopFrames) then
          begin

            HopOnsetAvg := FAnalysisAcc / FAnalysisHopFrames;
            HopEnergyAvg := FAnalysisEnergyAcc / FAnalysisHopFrames;

            if (FAnalysisSilenceFloor <= 0.0) then
              FAnalysisSilenceFloor := HopEnergyAvg
            else if (HopEnergyAvg <= FAnalysisSilenceFloor) then
              FAnalysisSilenceFloor := (FAnalysisSilenceFloor * 0.995) + (HopEnergyAvg * 0.005)
            else
              FAnalysisSilenceFloor := (FAnalysisSilenceFloor * 0.999) + (HopEnergyAvg * 0.001);

            GateThreshold := Max(0.0003,
                                 FAnalysisSilenceFloor * 2.5);

            if (HopEnergyAvg < GateThreshold) then
              AppendEnvelopeBin(0.0)
            else
              begin
                HopOnsetValue := HopOnsetAvg;
                AppendEnvelopeBin(HopOnsetValue);
              end;

            FAnalysisHopPos := 0;
            FAnalysisAcc := 0.0;
            FAnalysisEnergyAcc := 0.0;
          end;
      end;

    EnvCount := Length(FEnvelopeBins);
    if (EnvCount < (FEnvelopeRate * 8)) then
      Exit;

    if ((EndPosition100ns - FLastAnalyzePos100ns) < 8000000) then
      Exit;

    SetLength(EnvCopy,
              EnvCount);

    Move(FEnvelopeBins[0],
         EnvCopy[0],
         EnvCount * SizeOf(Single));

    FLastAnalyzePos100ns := EndPosition100ns;
    CurrentTrackBpm := FTrackBpm;
  finally

    FLock.Release();
  end;

  SmoothEnvelope(EnvCopy,
                 EnvCount);
  SmoothEnvelope(EnvCopy,
                 EnvCount);

  if not DetectBpmFromEnvelope(EnvCopy,
                               EnvCount,
                               FEnvelopeRate,
                               DetectedBpm,
                               DetectedOffsetRel100ns) then
    Exit;

  DetectedBpm := NormalizeDetectedBpm(DetectedBpm);
  WindowDuration100ns := Round((EnvCount / FEnvelopeRate) * 10000000.0);
  WindowStart100ns := EndPosition100ns - WindowDuration100ns;

  BeatLen100ns := (60.0 * 10000000.0) / DetectedBpm;
  NewBeatOffset100ns := NormalizeBeatOffsetToPosition(WindowStart100ns + DetectedOffsetRel100ns,
                                                      BeatLen100ns,
                                                      EndPosition100ns);

  FLock.Acquire();

  try

    if (CurrentTrackBpm <= 0.0) then
      FTrackBpm := DetectedBpm
    else if Abs(CurrentTrackBpm - DetectedBpm) <= 8.0 then
      FTrackBpm := (CurrentTrackBpm * 0.88) + (DetectedBpm * 0.12)
    else
      FTrackBpm := DetectedBpm;

    UpdateCurrentBpm();

    if (FCurrentBpm > 0.0) then
      begin

        BeatLen100ns := (60.0 * 10000000.0) / FCurrentBpm;

        if (BeatLen100ns > 0.0) then
          begin

            if (FBeatOffset100ns = 0) then
              FBeatOffset100ns := NewBeatOffset100ns
            else
              begin

                // Quantized beat-grid stabilization:
                // compare detected beat phase against the existing grid at the current position,
                // wrap to nearest beat, then apply only a limited correction per analysis pass.
                StableOffsetAtPos := NormalizeBeatOffsetToPosition(FBeatOffset100ns,
                                                                   BeatLen100ns,
                                                                   EndPosition100ns);
                DetectedOffsetAtPos := NormalizeBeatOffsetToPosition(NewBeatOffset100ns,
                                                                     BeatLen100ns,
                                                                     EndPosition100ns);
                PhaseError100ns := WrapPhaseError100ns(DetectedOffsetAtPos - StableOffsetAtPos,
                                                        BeatLen100ns);
                SnapDeadband100ns := BeatLen100ns * 0.03;
                MaxAdjust100ns := BeatLen100ns * 0.10;

                if Abs(PhaseError100ns) >= SnapDeadband100ns then
                  begin
                    AppliedAdjust100ns := ClampDouble(PhaseError100ns,
                                                      -MaxAdjust100ns,
                                                      MaxAdjust100ns);

                    FBeatOffset100ns := NormalizeBeatOffsetToPosition(
                                          Round(FBeatOffset100ns + AppliedAdjust100ns),
                                          BeatLen100ns,
                                          EndPosition100ns);
                  end
                else
                  FBeatOffset100ns := NormalizeBeatOffsetToPosition(FBeatOffset100ns,
                                                                    BeatLen100ns,
                                                                    EndPosition100ns);
              end;

            FLastBeatIndex := Trunc((FPosition100ns - FBeatOffset100ns) / BeatLen100ns) - 1;
          end
        else
          FLastBeatIndex := -1;
      end
    else
      FLastBeatIndex := -1;
  finally

    FLock.Release();
  end;
end;

function TMfLoopbackDeckEngine.PrepareProcess(const AProcessId: DWORD;
                                              const ACaptureMode: TMfLoopbackCaptureMode = lcmIncludeProcessTree;
                                              const AInitialBufferSize: REFERENCE_TIME = 0): HRESULT;
begin

  Result := Stop();

  if FAILED(Result) and (Result <> E_NOT_VALID_STATE) then
    Exit;

  SetState(dsReady);

  if not Assigned(FLoopback) then
    FLoopback := TLoopbackCapture.Create();

  FLoopback.OnCapturedPcm := LoopbackCapturedPcm;

  FProcessId := AProcessId;
  FIncludeProcessTree := (ACaptureMode = lcmIncludeProcessTree);
  FInitialBufferSize := AInitialBufferSize;

  ResetRing();
  ResetBeatDetector();
  FResampleFrac := 0.0;
  FCurrentTempoFactor := FTargetTempoFactor;
  FStartedOutput := False;
  FActive := False;
  FPosition100ns := 0;
  FTrackBpm := 0.0;
  FCurrentBpm := 0.0;
  FBeatOffset100ns := 0;
  FLastBeatIndex := -1;
  FLastTick100ns := -1;
  FCaptureDropBytes := 0;
  FCaptureSilentBytes := 0;
  UpdateCurrentBpm();
  Result := S_OK;
end;


function TMfLoopbackDeckEngine.Start(): HRESULT;
begin

  if not Assigned(FLoopback) then
    Exit(E_POINTER);

  if (FProcessId = 0) then
    Exit(E_INVALIDARG);

  ResetRing();
  ResetBeatDetector();
  FResampleFrac := 0.0;
  FCurrentTempoFactor := FTargetTempoFactor;
  FStartedOutput := False;
  FPosition100ns := 0;
  FTrackBpm := 0.0;
  FCurrentBpm := 0.0;
  FBeatOffset100ns := 0;
  FLastBeatIndex := -1;
  FLastTick100ns := -1;
  FCaptureDropBytes := 0;
  FCaptureSilentBytes := 0;
  UpdateCurrentBpm();

  Result := FLoopback.StartCaptureAsync(FProcessId,
                                        FIncludeProcessTree,
                                        FInitialBufferSize);
  if Succeeded(Result) then
    begin

      FActive := True;
      SetState(dsPlay);
    end;
end;


function TMfLoopbackDeckEngine.Stop(): HRESULT;
begin

  Result := S_OK;

  if Assigned(FLoopback) then
    begin
      Result := FLoopback.StopCaptureAsync();
      if (Result = E_NOT_VALID_STATE) then
        Result := S_OK;
    end;

  FActive := False;
  SetState(dsStop);

  FActive := False;
  ResetRing();
  ResetBeatDetector();
  FResampleFrac := 0.0;
  FCurrentTempoFactor := FTargetTempoFactor;
  FStartedOutput := False;
  FPosition100ns := 0;
  FTrackBpm := 0.0;
  FCurrentBpm := 0.0;
  FBeatOffset100ns := 0;
  FLastBeatIndex := -1;
  FLastTick100ns := -1;
  FCaptureDropBytes := 0;
  FCaptureSilentBytes := 0;
  UpdateCurrentBpm();
end;



procedure TMfLoopbackDeckEngine.EnsureConvertBuffer(const ByteCount: Integer);
begin

  if (ByteCount > 0) and (Length(FConvertBuf) < ByteCount) then
    SetLength(FConvertBuf,
              ByteCount);
end;


procedure TMfLoopbackDeckEngine.EnsureSilentFloatBuffer(const ByteCount: Integer);
begin

  if (ByteCount <= 0) then
    Exit;

  if (Length(FSilentFloatBuf) < ByteCount) then
    begin

      SetLength(FSilentFloatBuf,
                ByteCount);

      FillChar(FSilentFloatBuf[0],
               Length(FSilentFloatBuf),
               0);
    end;
end;


procedure TMfLoopbackDeckEngine.EnqueueCapturedSilence(const CaptureByteCount: Integer;
                                                       pwfx: PWAVEFORMATEX);
var
  FloatByteCount: Integer;

begin

  if (CaptureByteCount <= 0) or not Assigned(pwfx) then
    Exit;

  if IsFloat32Stereo(pwfx) then
    FloatByteCount := CaptureByteCount
  else if IsPcm16Stereo(pwfx) then
    FloatByteCount := (CaptureByteCount div SizeOf(SmallInt)) * SizeOf(Single)
  else
    Exit;

  if (FloatByteCount <= 0) then
    Exit;

  EnsureSilentFloatBuffer(FloatByteCount);
  InternalEnqueueBytes(@FSilentFloatBuf[0],
                       FloatByteCount);
  Inc(FCaptureSilentBytes,
      FloatByteCount);
end;


procedure TMfLoopbackDeckEngine.InternalEnqueueBytes(const pData: PByte;
                                                     const ByteCount: Integer);
var
  BytesToWrite: Integer;
  SourcePtr: PByte;
  RequiredFree: Integer;
  DropBytes: Integer;
  FirstPart: Integer;
  SecondPart: Integer;

begin

  if (pData = nil) or (ByteCount <= 0) then
    Exit;

  BytesToWrite := ByteCount;
  SourcePtr := pData;

  if (BytesToWrite > FRingSize) then
    begin

      BytesToWrite := (FRingSize div FBlockAlign) * FBlockAlign;
      if (BytesToWrite <= 0) then
        Exit;

      SourcePtr := PByte(NativeUInt(pData) + NativeUInt(ByteCount - BytesToWrite));
    end;

  FLock.Acquire();

  try

    RequiredFree := BytesToWrite - (FRingSize - FQueuedBytes);

    if (RequiredFree > 0) then
      begin
        DropBytes := ((RequiredFree + FBlockAlign - 1) div FBlockAlign) * FBlockAlign;
        InternalConsumeBytes(DropBytes);
        Inc(FCaptureDropBytes,
            DropBytes);

        if not FStartedOutput then
          FResampleFrac := 0.0;
      end;

    FirstPart := BytesToWrite;
    if ((FWritePos + FirstPart) > FRingSize) then
      FirstPart := FRingSize - FWritePos;

    Move(SourcePtr^,
         FRing[FWritePos],
         FirstPart);

    SecondPart := BytesToWrite - FirstPart;
    if (SecondPart > 0) then
      Move(PByte(NativeUInt(SourcePtr) + NativeUInt(FirstPart))^,
           FRing[0],
           SecondPart);

    Inc(FWritePos,
        BytesToWrite);

    if (FWritePos >= FRingSize) then
      Dec(FWritePos,
          FRingSize);

    Inc(FQueuedBytes,
        BytesToWrite);
  finally

    FLock.Release();
  end;
end;


procedure TMfLoopbackDeckEngine.ConvertPcm16StereoToFloat32(const pSrc: PByte;
                                                            const SrcByteCount: Integer);
var
  SampleCount: Integer;
  I: Integer;
  SrcSample: PSmallInt;
  DstSample: PSingle;
  DstByteCount: Integer;

begin

  if (pSrc = nil) or (SrcByteCount <= 0) then
    Exit;

  SampleCount := SrcByteCount div SizeOf(SmallInt);
  if (SampleCount <= 0) then
    Exit;

  DstByteCount := SampleCount * SizeOf(Single);
  EnsureConvertBuffer(DstByteCount);

  SrcSample := PSmallInt(pSrc);
  DstSample := PSingle(@FConvertBuf[0]);

  for I := 0 to SampleCount - 1 do
    begin

      DstSample^ := SrcSample^ / 32768.0;
      Inc(SrcSample);
      Inc(DstSample);
    end;

  InternalEnqueueBytes(@FConvertBuf[0],
                       DstByteCount);
end;


function TMfLoopbackDeckEngine.InternalPeekStereoFrame(const AFrameIndex: Integer;
                                                       out AL: Single;
                                                       out AR: Single): Boolean;
var
  ByteOffset: Integer;
  RingPos: Integer;
  SamplePtr: PSingle;

begin

  Result := False;
  AL := 0.0;
  AR := 0.0;

  if (AFrameIndex < 0) then
    Exit;

  ByteOffset := AFrameIndex * FBlockAlign;
  if ((ByteOffset + FBlockAlign) > FQueuedBytes) then
    Exit;

  RingPos := FReadPos + ByteOffset;
  if (RingPos >= FRingSize) then
    RingPos := RingPos mod FRingSize;

  if ((RingPos + FBlockAlign) <= FRingSize) then
    begin

      SamplePtr := PSingle(@FRing[RingPos]);
      AL := SamplePtr^;
      Inc(SamplePtr);
      AR := SamplePtr^;
      Result := True;
      Exit;
    end;

  Move(FRing[RingPos],
       AL,
       SizeOf(Single));

  RingPos := RingPos + SizeOf(Single);
  if (RingPos >= FRingSize) then
    Dec(RingPos,
        FRingSize);

  Move(FRing[RingPos],
       AR,
       SizeOf(Single));

  Result := True;
end;


procedure TMfLoopbackDeckEngine.InternalConsumeBytes(const AByteCount: Integer);
var
  ConsumeBytes: Integer;

begin

  ConsumeBytes := AByteCount;

  if (ConsumeBytes <= 0) then
    Exit;

  if (ConsumeBytes > FQueuedBytes) then
    ConsumeBytes := FQueuedBytes;

  Inc(FReadPos,
      ConsumeBytes);

  while (FReadPos >= FRingSize) do
    Dec(FReadPos,
        FRingSize);

  Dec(FQueuedBytes,
      ConsumeBytes);
end;


function TMfLoopbackDeckEngine.IsPcm16Stereo(const pwfx: PWAVEFORMATEX): Boolean;
begin

  Result := Assigned(pwfx) and
            (pwfx.nChannels = 2) and
            (pwfx.wBitsPerSample = 16) and
            (pwfx.nBlockAlign = 4) and
            (pwfx.wFormatTag = WAVE_FORMAT_PCM);
end;


function TMfLoopbackDeckEngine.IsFloat32Stereo(const pwfx: PWAVEFORMATEX): Boolean;
begin

  Result := Assigned(pwfx) and
            (pwfx.nChannels = 2) and
            (pwfx.wBitsPerSample = 32) and
            (pwfx.nBlockAlign = 8) and
            ((pwfx.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) or
             (pwfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE));
end;


procedure TMfLoopbackDeckEngine.LoopbackCapturedPcm(Sender: TObject;
                                                    pData: PByte;
                                                    ByteCount: DWORD;
                                                    pwfx: PWAVEFORMATEX;
                                                    const CaptureFlags: DWORD);
begin

  if (ByteCount = 0) then
    Exit;

  if ((CaptureFlags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
    begin

      EnqueueCapturedSilence(Integer(ByteCount),
                             pwfx);
      Exit;
    end;

  if (pData = nil) then
    Exit;

  if IsFloat32Stereo(pwfx) then
    begin

      InternalEnqueueBytes(pData,
                           Integer(ByteCount));
      Exit;
    end;

  if IsPcm16Stereo(pwfx) then
    begin

      ConvertPcm16StereoToFloat32(pData,
                                  Integer(ByteCount));
      Exit;
    end;
end;


function TMfLoopbackDeckEngine.ReadOutputPcmFloat32(const Frames: Integer;
                                                    const OutBuffer: PSingle;
                                                    out Flags: DWORD): HRESULT;
var
  I: Integer;
  OutPtr: PSingle;
  SrcPos: Double;
  FracPos: Double;
  Index0: Integer;
  Index1: Integer;
  WholeFrames: Integer;
  Frac: Double;
  L0: Single;
  R0: Single;
  L1: Single;
  R1: Single;
  AnyAudio: Boolean;
  FramesAvailable: Integer;
  CurrentFactor: Double;
  FramesToSilence: Integer;
  FramesWritten: Integer;
  EndPosition100ns: Int64;
  InputGainLinear: Single;

begin

  Flags := 0;

  if (Frames <= 0) or (OutBuffer = nil) then
    Exit(E_INVALIDARG);

  OutPtr := OutBuffer;
  AnyAudio := False;
  FramesWritten := 0;

  FLock.Acquire();

  try

    if not FStartedOutput then
      begin

        if (FQueuedBytes < FPrebufferBytes) then
          begin
            Flags := AUDCLNT_BUFFERFLAGS_SILENT;
            ZeroMemory(OutBuffer,
                       Frames * FBlockAlign);
            Exit(S_OK);
          end;

        FStartedOutput := True;
        FResampleFrac := 0.0;
      end;

    FramesAvailable := FQueuedBytes div FBlockAlign;
    if (FramesAvailable < 2) then
      begin

        FStartedOutput := False;
        FResampleFrac := 0.0;
        Flags := AUDCLNT_BUFFERFLAGS_SILENT;
        ZeroMemory(OutBuffer,
                   Frames * FBlockAlign);
        Exit(S_OK);
      end;

    FCurrentTempoFactor := FCurrentTempoFactor +
                           ((FTargetTempoFactor - FCurrentTempoFactor) * 0.15);

    if (FCurrentTempoFactor < 0.10) then
      FCurrentTempoFactor := 0.10;

    CurrentFactor := FCurrentTempoFactor;
    FTempoFactor := CurrentFactor;
    SrcPos := FResampleFrac;

    for I := 0 to Frames - 1 do
      begin

        Index0 := Trunc(SrcPos);
        Index1 := Index0 + 1;
        Frac := SrcPos - Index0;

        if not InternalPeekStereoFrame(Index0,
                                       L0,
                                       R0) then
          Break;

        AnyAudio := True;

        if not InternalPeekStereoFrame(Index1,
                                       L1,
                                       R1) then
          begin

            L1 := L0;
            R1 := R0;
          end;

        OutPtr^ := L0 + ((L1 - L0) * Frac);
        Inc(OutPtr);
        OutPtr^ := R0 + ((R1 - R0) * Frac);
        Inc(OutPtr);
        Inc(FramesWritten);

        SrcPos := SrcPos + CurrentFactor;
      end;

    FramesToSilence := Frames - I;
    if (FramesToSilence > 0) then
      begin

        ZeroMemory(OutPtr,
                   FramesToSilence * FBlockAlign);

        if not AnyAudio then
          begin

            FStartedOutput := False;
            FResampleFrac := 0.0;
            Flags := AUDCLNT_BUFFERFLAGS_SILENT;
            Exit(S_OK);
          end;
      end;

    WholeFrames := Trunc(SrcPos);
    if (WholeFrames > 0) then
      InternalConsumeBytes(WholeFrames * FBlockAlign);

    FracPos := SrcPos - WholeFrames;
    if (FracPos < 0.0) then
      FracPos := 0.0;

    FResampleFrac := FracPos;
    InputGainLinear := FInputGainLinear;
  finally

    FLock.Release();
  end;

  if not AnyAudio then
    Flags := AUDCLNT_BUFFERFLAGS_SILENT;

  if (FramesWritten > 0) then
    begin

      ApplyGainFloat32(OutBuffer,
                       FramesWritten,
                       FChannels,
                       InputGainLinear);

      if Assigned(FAudioRack) then
        FAudioRack.ProcessFloat32(OutBuffer,
                                  FramesWritten,
                                  FChannels,
                                  FSampleRate);

      EndPosition100ns := FPosition100ns +
                          Round((FramesWritten * 10000000.0) / FSampleRate);

      ProcessBeatDetector(OutBuffer,
                          FramesWritten,
                          EndPosition100ns);

    end;

  DoBpmTracking(FramesWritten);

  Result := S_OK;
end;


procedure TMfLoopbackDeckEngine.UpdateCurrentBpm();
begin

  if (FTrackBpm > 0.0) then
    FCurrentBpm := FTrackBpm * (1.0 + (FTempoPercent / 100.0))
  else
    FCurrentBpm := 0.0;
end;


procedure TMfLoopbackDeckEngine.DoBpmTracking(const FramesWritten: Integer);
var
  TickCb: TLoopbackDeckTickEvent;
  BeatCb: TLoopbackDeckBeatEvent;
  LocalPosition100ns: Int64;
  LocalCurrentBpm: Double;
  BeatLength100ns: Double;
  BeatPhase: Double;
  BeatIndex: Int64;
  FireBeat: Boolean;
  TickProc: TThreadProcedure;
  BeatProc: TThreadProcedure;

begin

  if (FramesWritten <= 0) then
    Exit;

  LocalPosition100ns := FPosition100ns +
                        Round((FramesWritten * 10000000.0) / FSampleRate);

  TickCb := nil;
  BeatCb := nil;
  LocalCurrentBpm := 0.0;
  BeatPhase := 0.0;
  BeatIndex := -1;
  FireBeat := False;

  FLock.Acquire();

  try

    FPosition100ns := LocalPosition100ns;
    LocalCurrentBpm := FCurrentBpm;

    if (LocalCurrentBpm > 0.0) then
      begin

        BeatLength100ns := (60.0 * 10000000.0) / LocalCurrentBpm;

        if (BeatLength100ns > 0.0) then
          begin
            BeatPhase := Frac((FPosition100ns - FBeatOffset100ns) / BeatLength100ns);
            if (BeatPhase < 0.0) then
              BeatPhase := BeatPhase + 1.0;

            BeatIndex := Trunc((FPosition100ns - FBeatOffset100ns) / BeatLength100ns);

            if (BeatIndex < FLastBeatIndex) then
              FLastBeatIndex := BeatIndex - 1;

            if (BeatIndex > FLastBeatIndex) then
              begin
                FLastBeatIndex := BeatIndex;
                FireBeat := True;
                BeatCb := FOnBeat;
              end;
          end;
      end;

    if ((FPosition100ns - FLastTick100ns) >= 500000) then
      begin

        FLastTick100ns := FPosition100ns;
        TickCb := FOnDeckTick;
      end;
  finally

    FLock.Release();
  end;

  if Assigned(TickCb) then
    begin
      TickProc := procedure
                  begin
                    if Assigned(TickCb) then
                      TickCb(Self,
                             LocalPosition100ns,
                             LocalCurrentBpm,
                             BeatPhase);
                  end;
      TThread.Queue(nil,
                    TickProc);
    end;

  if FireBeat and Assigned(BeatCb) then
    begin
      BeatProc := procedure
                  begin
                    if Assigned(BeatCb) then
                      BeatCb(Self,
                             LocalPosition100ns,
                             BeatIndex,
                             LocalCurrentBpm);
                  end;
      TThread.Queue(nil,
                    BeatProc);
    end;
end;


procedure TMfLoopbackDeckEngine.SetTrackBpm(const AValue: Double);
begin

  FLock.Acquire();

  try

    if (AValue > 0.0) then
      FTrackBpm := AValue
    else
      FTrackBpm := 0.0;

    UpdateCurrentBpm();
    FLastBeatIndex := -1;
  finally

    FLock.Release();
  end;
end;


// Gain
procedure TMfLoopbackDeckEngine.SetInputGainDb(const Value: Single);
var
  NewDb: Single;

begin

  NewDb := EnsureRange(Value,
                       -12.0,
                       6.0);

  FLock.Acquire;

  try

    FInputGainDb := NewDb;
    FInputGainLinear := Power(10.0,
                              FInputGainDb / 20.0);
  finally

    FLock.Release;
  end;
end;


function TMfLoopbackDeckEngine.GetTempoPercent(): Single;
begin

  Result := FTempoPercent;
end;


function TMfLoopbackDeckEngine.SetTempoPercent(const AValue: Single): HRESULT;
var
  NewTempo: Single;

begin

  NewTempo := AValue;

  if (NewTempo < -8.0) then
    NewTempo := -8.0
  else
    if (NewTempo > 8.0) then
      NewTempo := 8.0;

  FLock.Acquire();

  try

    FTempoPercent := NewTempo;
    FTargetTempoFactor := 1.0 + (FTempoPercent / 100.0);

    if (FTargetTempoFactor < 0.10) then
      FTargetTempoFactor := 0.10;

    UpdateCurrentBpm();
  finally

    FLock.Release();
  end;

  Result := S_OK;
end;


function TMfLoopbackDeckEngine.GetCurrentBpm(): Double;
begin

  FLock.Acquire();

  try

    Result := FCurrentBpm;
  finally

    FLock.Release();
  end;
end;


function TMfLoopbackDeckEngine.GetTempoFactor(): Double;
begin

  Result := FTargetTempoFactor;
end;


procedure TMfLoopbackDeckEngine.DoState();
var
  StateProc: TThreadProcedure;

begin

  if Assigned(FOnState) then
    begin
      StateProc := procedure
                  begin

                    if Assigned(FOnState) then
                      FOnState(Self,
                               FState);
                  end;
      TThread.Queue(nil,
                    StateProc);
    end;
end;


procedure TMfLoopbackDeckEngine.SetState(const AState: TWasApiDeviceState);
begin

  if (FState = AState) then
    Exit;

  FState := AState;
  DoState();
end;
end.
