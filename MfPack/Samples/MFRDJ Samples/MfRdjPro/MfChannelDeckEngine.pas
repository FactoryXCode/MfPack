// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfChannelDeckEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Channeldeck spine engine.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
//==============================================================================
// Source: -
//
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
unit MfChannelDeckEngine;

interface

uses

  { WinApi }
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  { System }
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,
  { ActiveX }
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  { MediaFoundationApi }
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.Mfidl,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfError,
  WinApi.CoreAudioApi.AudioClient,
  { WinMM }
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  { Application }
  MfWasApiEffectsRack,
  MfParametricEqComponent,
  MfCompressorLimiterComponent,
  MfWasApiFxComponentBase,
  PcmLib;

const
  REFTIMES_PER_SEC = 10000000;
  HNS_PER_100MS    = 1000000;
  DEFAULT_CACHE_BYTES = 256 * 1024;

type

  TDeviceState = (dsUninitialized,
                  dsError,
                  dsInitialized,
                  dsLoading,
                  dsReady,
                  dsPlay,
                  dsPause,
                  dsStop);
  // Events
  TWasApiStateEvent = procedure(Sender: TObject;
                                const NewState: TDeviceState) of object;

  TWasApiErrorEvent = procedure(Sender: TObject;
                                const Hr: HRESULT;
                                const Msg: string) of object;

  TWasApiReadyEvent = procedure(Sender: TObject) of object;

  TWasApiProcessedEvent = procedure(Sender: TObject;
                                    const Position100ns: Int64;
                                    const RawPosition: UInt64) of object;

  TWasApiOutputPcmEvent = procedure(Sender: TObject;
                                    pData: PByte;
                                    const ByteCount: DWORD;
                                    pwfx: PWAVEFORMATEX) of object;

  TWasApiEndedEvent = procedure(Sender: TObject) of object;

  TDeckTickEvent = procedure(Sender: TObject;
                             const Position100ns: Int64;
                             const CurrentBpm: Double;
                             const BeatPhase: Double) of object;

  TDeckBeatEvent = procedure(Sender: TObject;
                             const Position100ns: Int64;
                             const BeatNumber: Int64;
                             const CurrentBpm: Double) of object;

  TDeckBpmAnalyzedEvent = procedure(Sender: TObject;
                                    const Bpm: Double) of object;


  TMfChannelDeckEngine = class(TComponent)
  private

    FShuttingDown: LongBool;
    FLoadingFile: Boolean;
    FDestroying: LongBool;

    FState: TDeviceState;
    FOnStateChanged: TWasApiStateEvent;
    FOnError: TWasApiErrorEvent;
    FOnReady: TWasApiReadyEvent;
    FOnProcessed: TWasApiProcessedEvent;
    FOnOutputPcm: TWasApiOutputPcmEvent;
    FOnEnded: TWasApiEndedEvent;
    FOnBpmAnalyzed: TDeckBpmAnalyzedEvent;

    FLock: TCriticalSection;

    FFileName: string;
    FDuration100ns: Int64;
    FHasEndedSignaled: Boolean;
    FLastProcessedTick: UInt64;
    FOutputSampleRate: DWORD;
    FOutputChannels: Word;
    FVolLeft: Single;
    FVolRight: Single;
    // Tempo
    FTempo: Integer; // -16 .. +16
    FTempoFactor: Double; // 0.84 .. 1.16
    FSampleCursor: Double;
    // BPM
    FTrackBpm: Double; // original analyzed BPM
    FBeatOffset100ns: Int64; // Where beat 1 starts
    FOnDeckTick: TDeckTickEvent;
    FOnBeat: TDeckBeatEvent;
    FLastTick100ns: Int64;
    FLastBeatIndex: Int64;
    FCurrentBpm: Double; // BPM after tempo change
    FBpmEnvAccum: Double;
    FBpmEnvSampleCount: Integer;
    FBpmEnvWritePos: Integer;
    FBpmEnvFilled: Integer;
    FBpmLastAnalyzeTick: UInt64;
    FBpmLastNotifyTick: UInt64;
    FBpmLastReported: Double;
    FBpmEnvelopeHistory: TArray<Single>;
    FBpmBassPrevIn: Single;
    FBpmBassHpState: Single;
    FBpmBassLpState: Single;
    FBpmBassEnv: Single;
    FBpmBassPrevEnv: Single;

    // Audio rack
    FAudioRack: TMfWasApiEffectsRack;

    FSourceReader: IMFSourceReader;
    FSourceWfx: PWAVEFORMATEX;
    FWaveFormatLength: UINT32;
    FPositionBytes: UInt64;
    FPendingSeek: Boolean;
    FPendingSeek100ns: Int64;
    FReachedEof: Boolean;

    FCache: TBytes;
    FCacheStart: Integer;
    FCacheUsed: Integer;
    // Gain
    FInputGainDb: Single;
    FInputGainLinear: Single;

    procedure ClearEventHandlers();
    function CanUseEngine(): Boolean;

    procedure SetState(const NewState: TDeviceState);
    procedure RaiseError(const Msg: string; const Hr: HRESULT);
    procedure RaiseReady();
    procedure RaiseProcessed(const Position100ns: Int64; const RawPosition: UInt64);
    procedure RaiseEnded();

    procedure ResetSourceState();
    procedure FreeWaveFormat();
    function GetPosition100nsLocked(): Int64;
    function GetPosition100ns(): Int64;
    function GetBytesPerFrame(): Integer;
    function EnsureCacheCapacity(const ARequired: Integer): Boolean;
    procedure ClearCache();
    function PopCacheBytes(pDest: PByte; const ABytesRequested: Integer): Integer;
    procedure DropCacheBytes(const ABytesToDrop: Integer);
    function BuildOutputType(out AOutType: IMFMediaType): HRESULT;
    function LoadFileInternal(const AudioFile: TFileName): HRESULT;
    function FillCacheFromSourceReader(): HRESULT;
    function ApplyPendingSeekLocked(): HRESULT;
    // BPM
    function GetBeatIndex(): Int64;
    procedure SetTrackBpm(const Value: Double);
    procedure UpdateCurrentBpm();
    procedure ResetLiveBpmTracker();
    procedure FeedLiveBpmTracker(const pData: PSingle;
                                 const Frames: Integer;
                                 const Channels: Integer;
                                 const SampleRate: Integer;
                                 const Position100ns: Int64);
    procedure AnalyzeLiveBpm(const Position100ns: Int64);
    // Gain
    procedure SetInputGainDb(const Value: Single);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function OpenFile(const audiofile: TFileName): HRESULT;
    function Start: HRESULT;
    function Stop: HRESULT;
    function Pause: HRESULT;
    function SeekTo(const Pos100ns: Int64): HRESULT;

    function ReadOutputPcmFloat32(const Frames: Integer;
                                  const OutBuffer: PSingle;
                                  out Flags: DWORD): HRESULT;

    function SetVolumes(pVolLeft: Single;
                        pVolRight: Single): HRESULT;
    function SetTempo(const AValue: Integer): HRESULT;

    function AnalyzeBpmFromFile(const AFileName: string): HRESULT;
    function AnalyzeBpmAsync(): HRESULT;
    function GetTempoFactor(): Double;
    function GetCurrentBpm(): Double;
    function GetBeatLength100ns(): Double;
    function GetBeatPhase(): Double;

    function SetTempoFactor(const AFactor: Double): HRESULT;
    function SyncTempoTo(const ATargetBpm: Double): HRESULT;
    function SyncPhaseTo(const ATargetPhase: Double): HRESULT;

    procedure SetOutputFormat(const ASampleRate: DWORD;
                              const AChannels: Word = 2);

    property State: TDeviceState read FState;
    property DeviceState: TDeviceState read FState;
    property SoundChannels: Word read FOutputChannels;
    property Duration100ns: Int64 read FDuration100ns;
    property FileName: string read FFileName;
    property Position100ns: Int64 read GetPosition100ns;
    property AudioRack: TMfWasApiEffectsRack read FAudioRack;

    property TrackBpm: Double read FTrackBpm write SetTrackBpm;
    property BeatOffset100ns: Int64 read FBeatOffset100ns write FBeatOffset100ns;
    // Gain
    property InputGainDb: Single read FInputGainDb write SetInputGainDb;

    property OnDeckTick: TDeckTickEvent read FOnDeckTick write FOnDeckTick;
    property OnBeat: TDeckBeatEvent read FOnBeat write FOnBeat;
    property OnBpmAnalyzed: TDeckBpmAnalyzedEvent read FOnBpmAnalyzed write FOnBpmAnalyzed;

    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnReady: TWasApiReadyEvent read FOnReady write FOnReady;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
    property OnOutputPcm: TWasApiOutputPcmEvent read FOnOutputPcm write FOnOutputPcm;
    property OnProcessedPCM: TWasApiOutputPcmEvent read FOnOutputPcm write FOnOutputPcm;
    property OnEnded: TWasApiEndedEvent read FOnEnded write FOnEnded;
  end;

implementation

uses
  RDJ_Common;

// Helper methods --------------------------------------------------------------

type
  TSingleDynArray = array of Single;


function PtrSingleOffset(const P: PSingle; const Index: Integer): PSingle; inline;
begin

  Result := PSingle(NativeUInt(P) + NativeUInt(Index * SizeOf(Single)));
end;


procedure AppendEnvelopeValue(var AEnvelope: TSingleDynArray;
                              var ACount: Integer;
                              const AValue: Single);
var
  NewLen: Integer;

begin

  if (ACount >= Length(AEnvelope)) then
    begin

      NewLen := Length(AEnvelope);
      if (NewLen <= 0) then
        NewLen := 4096
      else
        NewLen := NewLen * 2;
      SetLength(AEnvelope,
                NewLen);
    end;

  AEnvelope[ACount] := AValue;
  Inc(ACount);
end;


procedure NormalizeEnvelope(var AEnvelope: TSingleDynArray;
                            const ACount: Integer);
var
  I: Integer;
  MeanValue: Double;
  Energy: Double;
  Scale: Double;

begin

  if (ACount <= 0) then
    Exit;

  MeanValue := 0.0;

  for I := 0 to ACount - 1 do
    MeanValue := MeanValue + AEnvelope[I];
  MeanValue := MeanValue / ACount;

  Energy := 0.0;
  for I := 0 to ACount - 1 do
    begin

      AEnvelope[I] := AEnvelope[I] - MeanValue;
      Energy := Energy + (AEnvelope[I] * AEnvelope[I]);
    end;

  if (Energy > 1.0E-12) then
    begin

      Scale := 1.0 / Sqrt(Energy);
      for I := 0 to ACount - 1 do
        AEnvelope[I] := AEnvelope[I] * Scale;
    end;
end;


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

  Tmp[ACount - 2] := (AEnvelope[ACount - 3] + AEnvelope[ACount - 2] + AEnvelope[ACount - 1]) / 3.0;
  Tmp[ACount - 1] := AEnvelope[ACount - 1];

  Move(Tmp[0],
       AEnvelope[0],
       ACount * SizeOf(Single));
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

      // mild preference toward musically typical DJ tempos
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
  ABeatOffset100ns := Round((BestPhase / AEnvelopeRate) * REFTIMES_PER_SEC);
  Result := (ADetectedBpm > 0.0);
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
// Helper methods end ----------------------------------------------------------


{ TMfChannelDeckEngine }

constructor TMfChannelDeckEngine.Create(AOwner: TComponent);
var
  Eq: TMfParametricEqEffect;
  Slot: TMfWasApiFxSlot;

begin

  inherited Create(AOwner);

  FLock := TCriticalSection.Create;
  FAudioRack := TMfWasApiEffectsRack.Create(Self);

  Eq := TMfParametricEqEffect.Create(Self);
  Eq.Enabled := True;
  Eq.GainDb := 0.0;
  Eq.CenterFreqHz := 1500.0;
  Eq.Q := 1.0;

  Slot := TMfWasApiFxSlot(FAudioRack.Slots.Add);
  Slot.Enabled := True;
  Slot.Effect := Eq;

  FState := dsUninitialized;
  FShuttingDown := False;
  FDestroying := False;

  FOutputSampleRate := 44100;
  FOutputChannels := 2;
  FVolLeft := 1.0;
  FVolRight := 1.0;

  FTempo := 0;
  FTempoFactor := 1.0;
  FSampleCursor := 0.0;

  FTrackBpm := 0.0;
  FCurrentBpm := 0.0;
  FBeatOffset100ns := 0;
  FLastTick100ns := 0;
  FLastBeatIndex := -1;
  FBpmEnvAccum := 0.0;
  FBpmEnvSampleCount := 0;
  FBpmEnvWritePos := 0;
  FBpmEnvFilled := 0;
  FBpmLastAnalyzeTick := 0;
  FBpmLastNotifyTick := 0;
  FBpmLastReported := 0.0;
  FBpmBassPrevIn := 0.0;
  FBpmBassHpState := 0.0;
  FBpmBassLpState := 0.0;
  FBpmBassEnv := 0.0;
  FBpmBassPrevEnv := 0.0;
  SetLength(FBpmEnvelopeHistory,
            800);

  FDuration100ns := 0;
  FHasEndedSignaled := False;
  FLastProcessedTick := 0;
  FSourceWfx := nil;
  FWaveFormatLength := 0;
  FPositionBytes := 0;
  FPendingSeek := False;
  FPendingSeek100ns := 0;
  FReachedEof := False;
  SetLength(FCache,
            DEFAULT_CACHE_BYTES);
  FCacheStart := 0;
  FCacheUsed := 0;

  // Gain
  FInputGainDb := 0.0;
  FInputGainLinear := 1.0;
end;


destructor TMfChannelDeckEngine.Destroy();
begin

  FDestroying := True;
  FShuttingDown := True;

  ClearEventHandlers();

  if Assigned(FLock) then
    FLock.Acquire;

  try

    try
      ClearCache();
    except
      // Do nothing.
    end;

    FSourceReader := nil;
    FreeWaveFormat();

    FReachedEof := True;
    FHasEndedSignaled := True;
    FPendingSeek := False;
    FPendingSeek100ns := 0;
    FPositionBytes := 0;
    FDuration100ns := 0;
    FFileName := '';
  finally

    if Assigned(FLock) then
      FLock.Release;
  end;

  FreeAndNil(FAudioRack);
  FreeAndNil(FLock);

  inherited Destroy;
end;


procedure TMfChannelDeckEngine.ClearEventHandlers();
begin

  FOnStateChanged := nil;
  FOnError := nil;
  FOnReady := nil;
  FOnProcessed := nil;
  FOnOutputPcm := nil;
  FOnEnded := nil;
  FOnBpmAnalyzed := nil;
  FOnDeckTick := nil;
  FOnBeat := nil;
end;


function TMfChannelDeckEngine.CanUseEngine(): Boolean;
begin

  Result := Assigned(FLock) and
            (not FShuttingDown) and
            (not FDestroying);
end;


procedure TMfChannelDeckEngine.SetState(const NewState: TDeviceState);
var
  StateCopy: TDeviceState;

begin

  if FDestroying then
    Exit;

  if (FState = NewState) then
    Exit;

  FState := NewState;
  StateCopy := NewState;

  if Assigned(FOnStateChanged) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if (FShuttingDown or FDestroying) then
                      Exit;

                    if Assigned(FOnStateChanged) then
                      FOnStateChanged(Self,
                                      StateCopy);
                  end);
end;


procedure TMfChannelDeckEngine.RaiseError(const Msg: string;
                                          const Hr: HRESULT);
begin

  if FDestroying then
    Exit;

  SetState(dsError);

  if Assigned(FOnError) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if FShuttingDown or FDestroying then
                      Exit;

                    if Assigned(FOnError) then
                      FOnError(Self,
                               Hr,
                               Msg);
                  end);
end;


procedure TMfChannelDeckEngine.RaiseReady;
begin

  if Assigned(FOnReady) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if FShuttingDown or FDestroying then
                      Exit;

                    if Assigned(FOnReady) then
                      FOnReady(Self);
                  end);
end;


procedure TMfChannelDeckEngine.RaiseProcessed(const Position100ns: Int64;
                                              const RawPosition: UInt64);
begin

  if Assigned(FOnProcessed) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if FShuttingDown or FDestroying then
                      Exit;

                    if Assigned(FOnProcessed) then
                      FOnProcessed(Self,
                                   Position100ns,
                                   RawPosition);
                  end);
end;


procedure TMfChannelDeckEngine.RaiseEnded();
begin

  if Assigned(FOnEnded) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if FShuttingDown or FDestroying then
                      Exit;

                    if Assigned(FOnEnded) then
                      FOnEnded(Self);
                  end);
end;


procedure TMfChannelDeckEngine.ResetSourceState();
begin

  FSourceReader := nil;
  FPositionBytes := 0;
  FPendingSeek := False;
  FPendingSeek100ns := 0;
  FReachedEof := False;
  FHasEndedSignaled := False;
  FLastProcessedTick := 0;
  ClearCache();
end;


procedure TMfChannelDeckEngine.FreeWaveFormat();
begin

  if Assigned(FSourceWfx) then
    begin

      CoTaskMemFree(FSourceWfx);
      FSourceWfx := nil;
    end;
  FWaveFormatLength := 0;
end;


function TMfChannelDeckEngine.GetBytesPerFrame(): Integer;
begin

  try

    if Assigned(FSourceWfx) then
      Result := FSourceWfx.nBlockAlign
    else
      Result := Integer(FOutputChannels) * SizeOf(Single);
  except
    // Do nothing.
    Result := 0;
  end;
end;


procedure TMfChannelDeckEngine.ClearCache();
begin

  FCacheStart := 0;
  FCacheUsed := 0;
  FSampleCursor := 0.0;
end;


function TMfChannelDeckEngine.EnsureCacheCapacity(const ARequired: Integer): Boolean;
var
  NewSize: Integer;

begin

  Result := (ARequired <= Length(FCache));
  if Result then
    Exit;

  NewSize := Length(FCache);
  if (NewSize <= 0) then
    NewSize := DEFAULT_CACHE_BYTES;

  while (NewSize < ARequired) do
    begin

      if NewSize > (MaxInt div 2) then
        begin

          NewSize := ARequired;
          Break;
        end;
      NewSize := NewSize * 2;
    end;

  try

    SetLength(FCache,
              NewSize);
    Result := True;
  except

    Result := False;
  end;
end;


function TMfChannelDeckEngine.PopCacheBytes(pDest: PByte;
                                            const ABytesRequested: Integer): Integer;
begin

  Result := Min(ABytesRequested,
                FCacheUsed);

  if (Result <= 0) then
    Exit;

  if (pDest <> nil) then
    Move(FCache[FCacheStart],
         pDest^,
         Result);

  Inc(FCacheStart, Result);
  Dec(FCacheUsed, Result);

  if (FCacheUsed = 0) then
    FCacheStart := 0
  else
    if FCacheStart > (Length(FCache) div 2) then
      begin

        Move(FCache[FCacheStart],
             FCache[0],
             FCacheUsed);

        FCacheStart := 0;
      end;
end;


procedure TMfChannelDeckEngine.DropCacheBytes(const ABytesToDrop: Integer);
begin

  if (ABytesToDrop <= 0) then
    Exit;

  PopCacheBytes(nil,
                ABytesToDrop);
end;


function TMfChannelDeckEngine.GetPosition100nsLocked(): Int64;
begin

  Result := 0;

  if Assigned(FSourceWfx) and (FSourceWfx.nAvgBytesPerSec <> 0) then
    Result := Int64((FPositionBytes * UInt64(REFTIMES_PER_SEC)) div UInt64(FSourceWfx.nAvgBytesPerSec));
end;


function TMfChannelDeckEngine.GetPosition100ns(): Int64;
begin

  Result := 0;

  if not Assigned(FLock) then
    Exit;

  FLock.Acquire;

  try

    Result := GetPosition100nsLocked();
  finally

    FLock.Release;
  end;
end;


procedure TMfChannelDeckEngine.SetOutputFormat(const ASampleRate: DWORD;
                                               const AChannels: Word);
begin

  if (ASampleRate <> 0) then
    FOutputSampleRate := ASampleRate;
  if (AChannels <> 0) then
    FOutputChannels := AChannels;
end;


function TMfChannelDeckEngine.SetVolumes(pVolLeft: Single;
                                         pVolRight: Single): HRESULT;
begin

  FVolLeft := EnsureRange(pVolLeft,
                          0.0,
                          1.0);

  FVolRight := EnsureRange(pVolRight,
                           0.0,
                           1.0);
  Result := S_OK;
end;


function TMfChannelDeckEngine.SetTempo(const AValue: Integer): HRESULT;
var
  NewTempo: Integer;

begin

  if not CanUseEngine() then
    Exit(E_ABORT);

  Result := S_OK;

  NewTempo := AValue;

  if (NewTempo < -16) then
    NewTempo := -16
  else
    if (NewTempo > 16) then
      NewTempo := 16;

  if (FTempo = NewTempo) then
    Exit;

  FTempo := NewTempo;
  FTempoFactor := 1.0 + (FTempo / 100.0);

  if (FTempoFactor < 0.10) then
    FTempoFactor := 0.10;

  UpdateCurrentBpm();
  FLastTick100ns := 0;
  FLastBeatIndex := -1;
end;


function TMfChannelDeckEngine.GetTempoFactor(): Double;
begin

  Result := FTempoFactor;
end;


function TMfChannelDeckEngine.SetTempoFactor(const AFactor: Double): HRESULT;
var
  NewFactor: Double;
  NewTempo: Integer;

begin

  if not CanUseEngine() then
    Exit(E_ABORT);

  Result := S_OK;

  NewFactor := AFactor;

  if (NewFactor < 0.10) then
    NewFactor := 0.10
  else
    if (NewFactor > 4.00) then
      NewFactor := 4.00;

  FTempoFactor := NewFactor;

  NewTempo := Round((FTempoFactor - 1.0) * 100.0);

  if (NewTempo < -16) then
    NewTempo := -16
  else
    if (NewTempo > 16) then
      NewTempo := 16;

  FTempo := NewTempo;
  FTempoFactor := 1.0 + (FTempo / 100.0);

  UpdateCurrentBpm();
  FLastTick100ns := 0;
  FLastBeatIndex := -1;
end;


function TMfChannelDeckEngine.SyncTempoTo(const ATargetBpm: Double): HRESULT;
var
  NeededFactor: Double;
  NeededTempo: Double;

begin

  if (FTrackBpm <= 0.0) or (ATargetBpm <= 0.0) then
    Exit(E_INVALIDARG);

  NeededFactor := ATargetBpm / FTrackBpm;
  NeededTempo := (NeededFactor - 1.0) * 100.0;

  if (NeededTempo < -16.0) or (NeededTempo > 16.0) then
    Exit(E_FAIL);

  Result := SetTempoFactor(NeededFactor);
end;


function TMfChannelDeckEngine.SyncPhaseTo(const ATargetPhase: Double): HRESULT;
var
  BeatLen: Double;
  CurPos: Double;
  CurPhase: Double;
  DeltaPhase: Double;
  NewPos: Int64;

begin

  if (FTrackBpm <= 0.0) then
    Exit(E_INVALIDARG);

  BeatLen := GetBeatLength100ns;
  if (BeatLen <= 0.0) then
    Exit(E_FAIL);

  CurPos := Position100ns;
  CurPhase := GetBeatPhase;
  DeltaPhase := ATargetPhase - CurPhase;

  if (DeltaPhase > 0.5) then
    DeltaPhase := DeltaPhase - 1.0
  else
    if (DeltaPhase < -0.5) then
      DeltaPhase := DeltaPhase + 1.0;

  NewPos := Round(CurPos + (DeltaPhase * BeatLen));
  if (NewPos < 0) then
    NewPos := 0;

  Result := SeekTo(NewPos);
end;


function TMfChannelDeckEngine.AnalyzeBpmFromFile(const AFileName: string): HRESULT;
begin

  Result := S_FALSE;
end;

function TMfChannelDeckEngine.AnalyzeBpmAsync(): HRESULT;
begin

  Result := S_FALSE;
end;

function TMfChannelDeckEngine.BuildOutputType(out AOutType: IMFMediaType): HRESULT;
begin

  AOutType := nil;

  Result := MFCreateMediaType(AOutType);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetGUID(MF_MT_SUBTYPE, MFAudioFormat_Float);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 32);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, FOutputSampleRate);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, FOutputChannels);
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, FOutputChannels * SizeOf(Single));
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                               FOutputSampleRate * FOutputChannels * SizeOf(Single));
  if FAILED(Result) then
    Exit;

  Result := AOutType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT, 1);
end;


function TMfChannelDeckEngine.LoadFileInternal(const AudioFile: TFileName): HRESULT;
var
  hr: HRESULT;
  SourceReaderConfiguration: IMFAttributes;
  NewSourceReader: IMFSourceReader;
  CurrentMediaType: IMFMediaType;
  OutputMediaType: IMFMediaType;
  PropVar: PROPVARIANT;
  NewWfx: PWAVEFORMATEX;
  NewWfxLen: UINT32;
  NewDuration100ns: Int64;

begin

  NewSourceReader := nil;
  NewWfx := nil;
  CurrentMediaType := nil;
  OutputMediaType := nil;

  hr := MFCreateAttributes(SourceReaderConfiguration,
                           2);
  if FAILED(hr) then
    Exit(hr);

  SourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                      1);
  SourceReaderConfiguration.SetUINT32(MF_READWRITE_DISABLE_CONVERTERS,
                                      0);

  hr := MFCreateSourceReaderFromURL(PWideChar(WideString(AudioFile)),
                                    SourceReaderConfiguration,
                                    NewSourceReader);
  if FAILED(hr) then
    Exit(hr);

  hr := BuildOutputType(OutputMediaType);
  if FAILED(hr) then
    Exit(hr);

  hr := NewSourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                            0,
                                            OutputMediaType);
  if FAILED(hr) then
    Exit(hr);

  hr := NewSourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                            @CurrentMediaType);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateWaveFormatExFromMFMediaType(CurrentMediaType,
                                            NewWfx,
                                            NewWfxLen,
                                            MFWaveFormatExConvertFlag_ForceExtensible);
  if FAILED(hr) then
    Exit(hr);

  PropVariantInit(PropVar);

  try

    hr := NewSourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                                   MF_PD_DURATION,
                                                   PropVar);
    if SUCCEEDED(hr) then
      NewDuration100ns := PropVar.hVal.QuadPart
    else
      NewDuration100ns := 0;
  finally

    PropVariantClear(PropVar);
  end;

  FLock.Acquire;
  try
    ResetSourceState();
    FreeWaveFormat;

    FSourceReader := NewSourceReader;
    FSourceWfx := NewWfx;
    FWaveFormatLength := NewWfxLen;
    FDuration100ns := NewDuration100ns;
    FPositionBytes := 0;
    FReachedEof := False;
    FHasEndedSignaled := False;
    FSampleCursor := 0.0;

    if Assigned(FSourceWfx) then
      begin

        FOutputChannels := FSourceWfx.nChannels;
        FOutputSampleRate := FSourceWfx.nSamplesPerSec;
      end;

    NewSourceReader := nil;
    NewWfx := nil;
  finally

    FLock.Release;
  end;

  Result := S_OK;
end;


function TMfChannelDeckEngine.OpenFile(const audiofile: TFileName): HRESULT;
begin
  if not CanUseEngine() then
    Exit(E_ABORT);

  if (audiofile = '') then
    Exit(E_INVALIDARG);

  if not FileExists(audiofile) then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));

  FLoadingFile := True;

  try
    FHasEndedSignaled := False;
    SetState(dsStop);

    Result := LoadFileInternal(audiofile);

    if FAILED(Result) then
    begin
      RaiseError('LoadFileInternal failed', Result);
      Exit;
    end;

    FFileName := audiofile;
    FTrackBpm := 0.0;
    FCurrentBpm := 0.0;
    FBeatOffset100ns := 0;
    FLastTick100ns := 0;
    FLastBeatIndex := -1;

    ResetLiveBpmTracker();

    RaiseProcessed(0,
                   0);
    SetState(dsReady);
    RaiseReady();
  finally

    FLoadingFile := False;
  end;
end;


function TMfChannelDeckEngine.Start(): HRESULT;
begin

  if not CanUseEngine() then
    Exit(E_ABORT);

  FLock.Acquire;

  try

    if (FSourceReader = nil) or (FSourceWfx = nil) then
      Exit(E_UNEXPECTED);

    FHasEndedSignaled := False;
    FLastTick100ns := 0;
    FLastBeatIndex := -1;

    SetState(dsPlay);
    Result := S_OK;
  finally

    FLock.Release;
  end;
end;


function TMfChannelDeckEngine.Stop(): HRESULT;
begin

  if not Assigned(FLock) then
    Exit(E_ABORT);

  FLock.Acquire;

  try

    ClearCache();
    FPositionBytes := 0;
    FPendingSeek := True;
    FPendingSeek100ns := 0;
    FSampleCursor := 0.0;
    FReachedEof := False;
    FHasEndedSignaled := False;
    FLastTick100ns := 0;
    FLastBeatIndex := -1;

    SetState(dsStop);

    Result := S_OK;
  finally

    FLock.Release;
  end;
end;


function TMfChannelDeckEngine.Pause(): HRESULT;
begin

  if not CanUseEngine() then
    Exit(E_ABORT);

  FLock.Acquire;

  try

    if (FState = dsPlay) then
      SetState(dsPause);
    Result := S_OK;
  finally

    FLock.Release;
  end;
end;


function TMfChannelDeckEngine.SeekTo(const Pos100ns: Int64): HRESULT;
var
  Target: Int64;

begin

  if not CanUseEngine() then
    Exit(E_ABORT);

  FLock.Acquire;

  try

    if (FSourceReader = nil) then
      Exit(E_UNEXPECTED);

    if (Pos100ns <= 0) then
      Target := 0
    else
      if (FDuration100ns > 0) and (Pos100ns > FDuration100ns) then
        Target := FDuration100ns
      else
        Target := Pos100ns;

    FPendingSeek := True;
    FPendingSeek100ns := Target;
    FReachedEof := False;
    FHasEndedSignaled := False;
    ClearCache;
    FSampleCursor := 0.0;

    FLastTick100ns := 0;
    FLastBeatIndex := -1;

    Result := S_OK;
  finally

    FLock.Release;
  end;
end;


function TMfChannelDeckEngine.ApplyPendingSeekLocked(): HRESULT;
var
  V: PROPVARIANT;
  BytesPerSec: UInt64;

begin

  if not FPendingSeek then
    Exit(S_OK);

  if FSourceReader = nil then
    Exit(E_UNEXPECTED);

  PropVariantInit(V);

  try

    V.vt := VT_I8;
    V.hVal.QuadPart := FPendingSeek100ns;
    Result := FSourceReader.SetCurrentPosition(GUID_NULL,
                                               V);
  finally

    PropVariantClear(V);
  end;

  if FAILED(Result) then
    Exit;

  ClearCache();
  FSampleCursor := 0.0;
  FReachedEof := False;

  if Assigned(FSourceWfx) and (FSourceWfx.nAvgBytesPerSec <> 0) then
    begin

      BytesPerSec := UInt64(FSourceWfx.nAvgBytesPerSec);
      FPositionBytes := (UInt64(FPendingSeek100ns) * BytesPerSec) div UInt64(REFTIMES_PER_SEC);
      if (FSourceWfx.nBlockAlign > 0) then
        FPositionBytes := (FPositionBytes div UInt64(FSourceWfx.nBlockAlign)) * UInt64(FSourceWfx.nBlockAlign);
    end
  else
    FPositionBytes := 0;

  FLastTick100ns := 0;
  FLastBeatIndex := -1;

  FPendingSeek := False;
end;


// BPM -------------------------------------------------------------------------
function TMfChannelDeckEngine.GetCurrentBpm(): Double;
begin

  Result := FCurrentBpm;
end;


function TMfChannelDeckEngine.GetBeatLength100ns: Double;
begin

  if (FCurrentBpm <= 0.0) then
    Exit(0.0);

  Result := (60.0 * REFTIMES_PER_SEC) / FCurrentBpm;
end;


function TMfChannelDeckEngine.GetBeatPhase(): Double;
var
  BeatLen: Double;
  PosAdj: Double;

begin

  Result := 0.0;

  BeatLen := GetBeatLength100ns;
  if (BeatLen <= 0.0) then
    Exit;

  PosAdj := Position100ns - FBeatOffset100ns;
  if (PosAdj < 0.0) then
    PosAdj := 0.0;

  Result := Frac(PosAdj / BeatLen);
end;


function TMfChannelDeckEngine.GetBeatIndex(): Int64;
var
  BeatLen: Double;
  PosRel: Double;

begin

  Result := 0;

  BeatLen := GetBeatLength100ns();
  if (BeatLen <= 0.0) then
    Exit;

  PosRel := Position100ns - FBeatOffset100ns;
  if (PosRel < 0) then
    PosRel := 0;

  Result := Trunc(PosRel / BeatLen);
end;


procedure TMfChannelDeckEngine.SetTrackBpm(const Value: Double);
begin

  if SameValue(FTrackBpm,
               Value,
               0.0001) then
    Exit;

  FTrackBpm := Value;
  UpdateCurrentBpm();
  FLastTick100ns := 0;
  FLastBeatIndex := -1;
end;


procedure TMfChannelDeckEngine.UpdateCurrentBpm();
begin

  if (FTrackBpm <= 0.0) then
    FCurrentBpm := 0.0
  else
    FCurrentBpm := FTrackBpm * FTempoFactor;
end;


procedure TMfChannelDeckEngine.ResetLiveBpmTracker();
begin

  FBpmEnvAccum := 0.0;
  FBpmEnvSampleCount := 0;
  FBpmEnvWritePos := 0;
  FBpmEnvFilled := 0;
  FBpmLastAnalyzeTick := 0;
  FBpmLastNotifyTick := 0;
  FBpmLastReported := 0.0;
  FBpmBassPrevIn := 0.0;
  FBpmBassHpState := 0.0;
  FBpmBassLpState := 0.0;
  FBpmBassEnv := 0.0;
  FBpmBassPrevEnv := 0.0;

  if Length(FBpmEnvelopeHistory) = 0 then
    SetLength(FBpmEnvelopeHistory,
              800);
end;

procedure TMfChannelDeckEngine.AnalyzeLiveBpm(const Position100ns: Int64);
const
  BPM_ENV_RATE = 100;
var
  Env: TSingleDynArray;
  Count: Integer;
  StartPos: Integer;
  I: Integer;
  DetectedCurrentBpm: Double;
  DummyOffset: Int64;
  BaseBpm: Double;
  NewTrackBpm: Double;
  OldBeatPos: Double;
  NewBeatLen: Double;
begin

  if (FBpmEnvFilled < 400) then
    Exit;

  Count := FBpmEnvFilled;
  SetLength(Env,
            Count);

  StartPos := FBpmEnvWritePos - Count;
  while (StartPos < 0) do
    Inc(StartPos,
        Length(FBpmEnvelopeHistory));

  for I := 0 to Count - 1 do
    Env[I] := FBpmEnvelopeHistory[(StartPos + I) mod Length(FBpmEnvelopeHistory)];

  SmoothEnvelope(Env,
                 Count);
  NormalizeEnvelope(Env,
                    Count);

  DummyOffset := 0;
  if not DetectBpmFromEnvelope(Env,
                               Count,
                               BPM_ENV_RATE,
                               DetectedCurrentBpm,
                               DummyOffset) then
    Exit;

  if (DetectedCurrentBpm <= 0.0) or (FTempoFactor <= 0.0) then
    Exit;

  BaseBpm := DetectedCurrentBpm / FTempoFactor;
  if (BaseBpm < 40.0) or (BaseBpm > 260.0) then
    Exit;

  if (FTrackBpm <= 0.0) then
    NewTrackBpm := BaseBpm
  else if (Abs(BaseBpm - FTrackBpm) <= 8.0) then
    NewTrackBpm := (FTrackBpm * 0.85) + (BaseBpm * 0.15)
  else if (Abs(BaseBpm - FTrackBpm) <= 16.0) then
    NewTrackBpm := (FTrackBpm * 0.92) + (BaseBpm * 0.08)
  else
    Exit;

  if (FCurrentBpm > 0.0) then
    OldBeatPos := (Position100ns - FBeatOffset100ns) / GetBeatLength100ns
  else
    OldBeatPos := 0.0;

  FTrackBpm := NewTrackBpm;
  UpdateCurrentBpm();

  if (FCurrentBpm > 0.0) then
    begin
      NewBeatLen := GetBeatLength100ns;
      if (OldBeatPos > 0.0) then
        FBeatOffset100ns := Position100ns - Round(OldBeatPos * NewBeatLen)
      else
        FBeatOffset100ns := Position100ns;
    end;

  FLastTick100ns := 0;

  if Assigned(FOnBpmAnalyzed) and
     ((FBpmLastReported <= 0.0) or
      (Abs(FCurrentBpm - FBpmLastReported) >= 0.2) or
      ((GetTickCount64 - FBpmLastNotifyTick) >= 3000)) then
    begin
      FBpmLastReported := FCurrentBpm;
      FBpmLastNotifyTick := GetTickCount64;

      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FOnBpmAnalyzed) then
            FOnBpmAnalyzed(Self,
                           FCurrentBpm);
        end);
    end;
end;


procedure TMfChannelDeckEngine.FeedLiveBpmTracker(const pData: PSingle;
                                                  const Frames: Integer;
                                                  const Channels: Integer;
                                                  const SampleRate: Integer;
                                                  const Position100ns: Int64);
const
  BPM_ENV_RATE = 100;
  HP_FREQ_HZ = 45.0;
  LP_FREQ_HZ = 160.0;
  ENV_ATTACK_MS = 10.0;
  ENV_RELEASE_MS = 120.0;
var
  I: Integer;
  Ch: Integer;
  Src: PSingle;
  MonoIn: Double;
  SamplesPerBin: Integer;
  NowTick: UInt64;
  Dt: Double;
  RcHp: Double;
  RcLp: Double;
  AlphaHp: Double;
  AlphaLp: Double;
  AttackCoeff: Double;
  ReleaseCoeff: Double;
  HpOut: Double;
  BandValue: Double;
  Rectified: Double;
  EnvTarget: Double;
  OnsetValue: Double;
begin

  if (pData = nil) or (Frames <= 0) or (Channels <= 0) or (SampleRate <= 0) then
    Exit;

  SamplesPerBin := Max(1,
                       SampleRate div BPM_ENV_RATE);

  Dt := 1.0 / SampleRate;
  RcHp := 1.0 / (2.0 * Pi * HP_FREQ_HZ);
  RcLp := 1.0 / (2.0 * Pi * LP_FREQ_HZ);

  AlphaHp := RcHp / (RcHp + Dt);
  AlphaLp := Dt / (RcLp + Dt);

  AttackCoeff := Dt / ((ENV_ATTACK_MS / 1000.0) + Dt);
  ReleaseCoeff := Dt / ((ENV_RELEASE_MS / 1000.0) + Dt);

  Src := pData;

  for I := 0 to Frames - 1 do
    begin
      MonoIn := 0.0;

      for Ch := 0 to Channels - 1 do
        begin
          MonoIn := MonoIn + Src^;
          Inc(Src);
        end;

      MonoIn := MonoIn / Channels;

      // 1-pole HP then 1-pole LP -> cheap kick-focused band-pass.
      HpOut := AlphaHp * (FBpmBassHpState + MonoIn - FBpmBassPrevIn);
      FBpmBassPrevIn := MonoIn;
      FBpmBassHpState := HpOut;

      FBpmBassLpState := FBpmBassLpState + (AlphaLp * (HpOut - FBpmBassLpState));
      BandValue := FBpmBassLpState;

      Rectified := Abs(BandValue);
      EnvTarget := Rectified;

      if EnvTarget > FBpmBassEnv then
        FBpmBassEnv := FBpmBassEnv + (AttackCoeff * (EnvTarget - FBpmBassEnv))
      else
        FBpmBassEnv := FBpmBassEnv + (ReleaseCoeff * (EnvTarget - FBpmBassEnv));

      // Onset emphasis: respond to rising bass energy, not just loud sustained peaks.
      OnsetValue := FBpmBassEnv - FBpmBassPrevEnv;
      if (OnsetValue < 0.0) then
        OnsetValue := 0.0;

      FBpmBassPrevEnv := FBpmBassEnv;

      FBpmEnvAccum := FBpmEnvAccum + OnsetValue;
      Inc(FBpmEnvSampleCount);

      if (FBpmEnvSampleCount >= SamplesPerBin) then
        begin
          if Length(FBpmEnvelopeHistory) > 0 then
            begin
              FBpmEnvelopeHistory[FBpmEnvWritePos] := FBpmEnvAccum / FBpmEnvSampleCount;
              Inc(FBpmEnvWritePos);

              if (FBpmEnvWritePos >= Length(FBpmEnvelopeHistory)) then
                FBpmEnvWritePos := 0;

              if (FBpmEnvFilled < Length(FBpmEnvelopeHistory)) then
                Inc(FBpmEnvFilled);
            end;

          FBpmEnvAccum := 0.0;
          FBpmEnvSampleCount := 0;
        end;
    end;

  NowTick := GetTickCount64;
  if ((NowTick - FBpmLastAnalyzeTick) >= 1000) then
    begin
      FBpmLastAnalyzeTick := NowTick;
      AnalyzeLiveBpm(Position100ns);
    end;
end;

// BPM end ---------------------------------------------------------------------

// Gain stter.
procedure TMfChannelDeckEngine.SetInputGainDb(const Value: Single);
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


function TMfChannelDeckEngine.FillCacheFromSourceReader(): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  AudioData: PByte;
  AudioDataLength: DWORD;
  Flags: DWORD;
  Required: Integer;
  DestPos: Integer;

begin

  if (FSourceReader = nil) then
    Exit(E_UNEXPECTED);

  if FPendingSeek then
    begin

      Result := ApplyPendingSeekLocked();
      if FAILED(Result) then
        Exit;
    end;

  if FReachedEof then
    Exit(S_FALSE);

  Sample := nil;
  Buffer := nil;
  Flags := 0;

  Result := FSourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                     0,
                                     nil,
                                     @Flags,
                                     nil,
                                     @Sample);
  if FAILED(Result) then
    Exit;

  if ((Flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
    begin

      FReachedEof := True;
      Exit(S_FALSE);
    end;

  if (Sample = nil) then
    Exit(S_OK);

  Result := Sample.ConvertToContiguousBuffer(@Buffer);
  if FAILED(Result) then
    Exit;

  Result := Buffer.Lock(AudioData,
                        nil,
                        @AudioDataLength);
  if FAILED(Result) then
    Exit;

  try

    if (AudioDataLength > 0) then
      begin

        Required := FCacheStart + FCacheUsed + Integer(AudioDataLength);
        if not EnsureCacheCapacity(Required) then
          Exit(E_OUTOFMEMORY);

        DestPos := FCacheStart + FCacheUsed;
        Move(AudioData^,
             FCache[DestPos],
             AudioDataLength);

        Inc(FCacheUsed,
            Integer(AudioDataLength));
      end;
  finally

    Buffer.Unlock;
  end;
end;


function TMfChannelDeckEngine.ReadOutputPcmFloat32(const Frames: Integer;
                                                   const OutBuffer: PSingle;
                                                   out Flags: DWORD): HRESULT;
var
  BytesRequested: Integer;
  TriggerEnded: Boolean;
  Position100ns: Int64;
  NowTick: UInt64;
  NotifyWfx: PWAVEFORMATEX;
  BytesPerFrame: Integer;
  Channels: Integer;
  OutputChannels: Integer;
  OutFrame: Integer;
  CacheFrames: Integer;
  NeedMore: Boolean;
  BasePtr: PSingle;
  SrcIndex0: Integer;
  SrcIndex1: Integer;
  SampleIndex0: Integer;
  SampleIndex1: Integer;
  Frac: Double;
  Ch: Integer;
  V0: Single;
  V1: Single;
  WholeFramesConsumed: Integer;
  DropBytes: Integer;
  OutputPtr: PSingle;
  InputGainLinear: Single;
  // BPM
  CurrentBpm: Double;
  BeatPhase: Double;
  BeatIndex: Int64;

begin

  Flags := 0;
  TriggerEnded := False;

  if (FShuttingDown or FDestroying) then
    Exit(S_OK);

  if (Frames <= 0) or (OutBuffer = nil) then
    Exit(E_INVALIDARG);

  if not Assigned(FLock) then
    Exit(E_ABORT);

  BytesPerFrame := GetBytesPerFrame();
  BytesRequested := Frames * BytesPerFrame;
  FillChar(OutBuffer^,
           BytesRequested,
           0);

  FLock.Acquire;

  try

    if (FShuttingDown or FDestroying) then
      begin

        Flags := AUDCLNT_BUFFERFLAGS_SILENT;
        Result := E_ABORT;
        Exit;
      end;

    if (FState <> dsPlay) or
       (FSourceReader = nil) or
       (FSourceWfx = nil) then
    begin

      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Result := S_OK;
      Exit;
    end;

    Channels := FSourceWfx.nChannels;
    OutputChannels := Channels;

    if (Channels <= 0) or (BytesPerFrame <= 0) then
      begin

        Flags := AUDCLNT_BUFFERFLAGS_SILENT;
        Result := E_UNEXPECTED;
        Exit;
      end;

    OutputPtr := OutBuffer;

    for OutFrame := 0 to Frames - 1 do
      begin

        while True do
          begin

            CacheFrames := FCacheUsed div BytesPerFrame;
            NeedMore := ((not FReachedEof) and ((CacheFrames < 2) or (FSampleCursor >= (CacheFrames - 1))));
            if not NeedMore then
              Break;

            Result := FillCacheFromSourceReader();
            if FAILED(Result) then
              Exit;

            if (Result = S_FALSE) and FReachedEof then
              Break;
          end;

        CacheFrames := FCacheUsed div BytesPerFrame;
        if (CacheFrames <= 0) then
          begin

            Flags := AUDCLNT_BUFFERFLAGS_SILENT;
            SetState(dsStop);

            if not FHasEndedSignaled then
              begin

                FHasEndedSignaled := True;
                TriggerEnded := True;
              end;
            Break;
          end;

        BasePtr := PSingle(@FCache[FCacheStart]);

        SrcIndex0 := Trunc(FSampleCursor);

        if (SrcIndex0 < 0) then
          SrcIndex0 := 0;
        if (SrcIndex0 >= CacheFrames) then
          SrcIndex0 := CacheFrames - 1;

        SrcIndex1 := SrcIndex0 + 1;
        if (SrcIndex1 >= CacheFrames) then
          SrcIndex1 := CacheFrames - 1;

        Frac := FSampleCursor - SrcIndex0;

        if (Frac < 0.0) then
          Frac := 0.0
        else
          if (Frac > 1.0) then
            Frac := 1.0;

        for Ch := 0 to Channels - 1 do
          begin

            SampleIndex0 := (SrcIndex0 * Channels) + Ch;
            SampleIndex1 := (SrcIndex1 * Channels) + Ch;

            V0 := PSingle(NativeUInt(BasePtr) + NativeUInt(SampleIndex0 * SizeOf(Single)))^;
            V1 := PSingle(NativeUInt(BasePtr) + NativeUInt(SampleIndex1 * SizeOf(Single)))^;

            OutputPtr^ := V0 + _Single((V1 - V0) * Frac);
            Inc(OutputPtr);
          end;

        FSampleCursor := FSampleCursor + FTempoFactor;

        WholeFramesConsumed := Trunc(FSampleCursor);

        if (WholeFramesConsumed > 0) then
          begin

            if (CacheFrames > 1) then
              WholeFramesConsumed := Min(WholeFramesConsumed,
                                         CacheFrames - 1)
            else
              WholeFramesConsumed := Min(WholeFramesConsumed,
                                         CacheFrames);

            if (WholeFramesConsumed > 0) then
              begin

                DropBytes := WholeFramesConsumed * BytesPerFrame;
                DropCacheBytes(DropBytes);
                Inc(FPositionBytes,
                    UInt64(DropBytes));
                FSampleCursor := FSampleCursor - WholeFramesConsumed;
              end;
          end;
      end;

    Position100ns := GetPosition100nsLocked;

    CurrentBpm := GetCurrentBpm();
    BeatPhase := GetBeatPhase();
    BeatIndex := GetBeatIndex();
    InputGainLinear := FInputGainLinear;

    if ((Position100ns - FLastTick100ns) >= 500000) then // 50 ms
      begin

        FLastTick100ns := Position100ns;

        if FShuttingDown or FDestroying then
          Exit(E_ABORT);

        if Assigned(FOnDeckTick) then
          TThread.Queue(nil,
                        procedure
                        begin

                          if Assigned(FOnDeckTick) then
                            FOnDeckTick(Self,
                                        Position100ns,
                                        CurrentBpm,
                                        BeatPhase);
                        end);
      end;

    if (BeatIndex > FLastBeatIndex) then
      begin

        FLastBeatIndex := BeatIndex;

        if Assigned(FOnBeat) then
          TThread.Queue(nil,
                        procedure
                        begin

                          if FShuttingDown or FDestroying then
                            Exit;

                          if Assigned(FOnBeat) then
                            FOnBeat(Self,
                                    Position100ns,
                                    BeatIndex,
                                    CurrentBpm);
                        end);
      end;

    NowTick := GetTickCount64;

    if (NowTick - FLastProcessedTick >= 40) or TriggerEnded then
      begin

        FLastProcessedTick := NowTick;
        RaiseProcessed(Position100ns,
                       FPositionBytes);
      end;

    NotifyWfx := FSourceWfx;
    Result := S_OK;
  finally

    FLock.Release;
  end;

  if FShuttingDown or FDestroying then
    Exit(E_ABORT);

  try
    ApplyGainFloat32(OutBuffer,
                     Frames,
                     OutputChannels,
                     InputGainLinear);
  except
    // Only use debug info here!
  end;

  try

    if Assigned(FAudioRack) and Assigned(NotifyWfx) and (BytesRequested > 0) then
      FAudioRack.ProcessPcm(Self,
                            PByte(OutBuffer),
                            BytesRequested,
                            NotifyWfx);
  except
    // Only use debug info here!
  end;

  try

    if ((Flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) and
       Assigned(NotifyWfx) then
      FeedLiveBpmTracker(OutBuffer,
                         Frames,
                         OutputChannels,
                         NotifyWfx.nSamplesPerSec,
                         Position100ns);
  except
    // Only use debug info here!
  end;

  try

    if Assigned(FOnOutputPcm) and Assigned(NotifyWfx) then
      FOnOutputPcm(Self,
                   PByte(OutBuffer),
                   BytesRequested,
                   NotifyWfx);
  except
    // Only use debug info here!
  end;

  if TriggerEnded then
    RaiseEnded;
end;

end.
