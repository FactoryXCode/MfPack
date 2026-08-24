// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: XAudio2Engine.pas
// Kind: Pascal Unit
// Release date: 28-03-2024
// Language: ENU
//
// Revision Version: 4.0.0
// Description: The commander in chief.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Carmen(carmenh), Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: factoryx.code
//
// Copyright © FacctoryX
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
unit XAudio2Engine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX,
  WinApi.ActiveX.PropIdl,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {XAudio2}
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPO,
  WinApi.DirectX.XAudio2.X3DAudio,
  WinApi.DirectX.XAudio2.XAudio2Fx,
  WinApi.DirectX.XAudio2.XAPOFx,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Effects}
  XAudio2_FXReverb,
  XAudio2_FXMasterLimiter;

const

  MIN_PITCH = 0.4;
  MAX_PITCH = 2.0;
  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;

  STREAM_BUFFER_COUNT = 3;
  STREAM_BUFFER_MS = 100; // 100ms chunks (teachable + smooth)

type

  TEngineCommandKind = (cmdNone,
                        cmdPlay,
                        cmdPause,
                        cmdStop,
                        cmdLoad,
                        cmdQuit);

  TEngineCommand = record
    Kind: TEngineCommandKind;
    FileName: string;   // used for cmdLoad
    AutoPlay: Boolean;  // used for cmdLoad

    class function Make(AKind: TEngineCommandKind): TEngineCommand; static;
    class function MakeLoad(const aFileName: string;
                            aAutoPlay: Boolean = False): TEngineCommand; static;
  end;

  TRenderstatus =(rsStopped,
                  rsPlaying,
                  rsPauzed,
                  rsEndOfBuffer,  // = rsPlaying, See comments in OnBufferEnd.
                  rsStartOfBuffer, // = rsPlaying, See comments in OnBufferStart.
                  rsEndOfStream,
                  rsInitializing,
                  rsInitialized,
                  rsProcessingPassStart,  // Do not use unless you need it.
                  rsProcessingPassEnd,  // Do not use unless you need it.
                  rsEndOfLoop,
                  rsError,
                  rsDestroying);

  TXaudio2EventData = record
    SamplesProcessed: LONGLONG;
    Position: LONGLONG;
    TimePlayed: MFTIME;
    procedure Reset();
  end;

  TStreamBuf = record
    Data: PByte;
    CapBytes: Cardinal;
    UsedBytes: Cardinal;
    XA: XAUDIO2_BUFFER;
  end;

  // Voice ID's
  TEffectsOnVoices = (afxMasteringVoice,
                      afxSourceVoice);

  TXaudio2Engine = class(IXAudio2VoiceCallback)

{$region 'IXAudio2VoiceCallback'}
    procedure OnVoiceProcessingPassStart(BytesRequired: UINT32); override; stdcall;
    procedure OnVoiceProcessingPassEnd(); override; stdcall;
    procedure OnStreamEnd(); override; stdcall;
    procedure OnBufferStart(pBufferContext: Pointer); override; stdcall;
    procedure OnBufferEnd(pBufferContext: Pointer); override; stdcall;
    procedure OnLoopEnd(pBufferContext: Pointer); override; stdcall;
    procedure OnVoiceError(pBufferContext: Pointer; Error: HResult); override; stdcall;
{$endregion}

  private

    // Threading / synchronization.
    FLock: TCriticalSection;
    FCmdEvent: TEvent;
    FWorker: TThread;
    FCmd: TEngineCommand;

    pvXAudio2: IXAudio2;
    pvMasteringVoice: IXAudio2MasteringVoice;
    pvSourceVoice: IXAudio2SourceVoice;

    // Effects.
    pvFxReverbEffect: TFxReverb;
    pvFxMasterLimiter: TFxMasterLimiter;

    // Media Foundation streaming decode.
    FReader: IMFSourceReader;
    FReaderAttr: IMFAttributes;
    FReachedEof: Boolean;

    // Streaming buffer ring.
    FStreamBuf: array[0..STREAM_BUFFER_COUNT-1] of TStreamBuf;
    FNextSubmit: Integer;

    // Signals from XAudio2 callback -> worker thread refill.
    FBufEndEvent: TEvent;
    FNeedRefill: Integer; // interlocked counter.

    // Audio buffer.
    pvAudioBytes: UINT32; // Needed for seeking.


    pvFileName: string;
    pvSourceFileDuration: LONGLONG;

    pvWaveformatex: PWAVEFORMATEX;
    pvwaveformatlength: UINT32;
    pvChannels: UINT32;
    pvSamplesPerSecond: UINT32;

    // Volume per channel (compat).
    pvVolumeChannels: TFloatArray;

    // Reverb (compat).
    pvReverbI3DL2ParamArray: TReverbI3DL2ParamArray;

    // Playback bookkeeping.
    pvBufferPrevPlayed: Int64;
    pvBufferStart: UINT64;
    pvNewBufferPosition: UINT64;
    pvSeekBaseSamples: Int64;   // samples offset at last seek (GotoNewPosition).
    pvSeekBaseHns: Int64;       // time offset at last seek (100ns units).

    // Events.
    FOnAudioReadyEvent: TNotifyEvent;
    FOnAudioPlayingEvent: TNotifyEvent;
    FOnAudioPauzedEvent: TNotifyEvent;
    FOnAudioStoppedEvent: TNotifyEvent;
    FOnVoiceProcessingPassStartEvent: TNotifyEvent;
    FOnVoiceProcessingPassEndEvent: TNotifyEvent;
    FOnStreamEndEvent: TNotifyEvent;
    FOnBufferStartEvent: TNotifyEvent;
    FOnBufferEndEvent: TNotifyEvent;

    // Reverb.
    bReverbEffectOnSourceVoice: Boolean;
    bReverbEffectOnMasteringVoice: Boolean;

    // Event data & engine status.
    FXaudio2EventData: TXaudio2EventData;
    pvRenderStatus: TRenderstatus;

    // Threading.
    procedure StartWorker();
    procedure StopWorker();
    procedure RequestCommand(const ACmd: TEngineCommand);
    procedure WorkerProc();
    procedure PollStateAndNotify();

    // Streaming buffer allocation helpers.
    procedure FreeStreamingBuffers();
    procedure AllocStreamingBuffers();

    // MF SourceReader setup (streaming, uncompressed float).
    function SetupSourceReader(const audiofile: TFileName): HResult;
    function LoadFile(const audiofile: TFileName): HResult;
    function InitializeXAudio2(replay: Boolean = False): HResult;

    // Reading PCM bytes from the SourceReader (streaming).
    function ReadPcmInto(var Dest: PByte;
                         var DestCap: Cardinal;
                         out Wrote: Cardinal): HResult;

    // Priming + refilling XAudio2 buffers (the actual multi-buffer engine).
    function SubmitNextStreamBuffer(IsLast: Boolean): HResult;
    function PrimeStreaming: HResult;

    // Reverb preset builder (compat).
    function GetReverbParamsI3DL2(): TReverbI3DL2ParamArray;

    // --- Compatibility getters ---
    function GetPlayStatus: TRenderstatus;
    function GetDuration: MFTIME;
    function GetSamplesPerSec: UINT32;
    function GetSoundChannels: UINT32;
    function GetVolumeChannels: TFloatArray;
    function GetAudioEventData: TXaudio2EventData;

    procedure SetVolumeChannelsProp(const Value: TFloatArray);

  public

    constructor Create();
    destructor Destroy(); override;

    // Life cycle -----------------------------------------------------

    // Threaded/async load.
    procedure LoadAndInitializeAsync(const audiofile: TFileName; AutoPlay: Boolean = True);


    function Play(): HResult;
    function Pause(): HResult;
    function Stop(): HResult;

    function GotoNewPosition(const SamplePos: LONGLONG): HResult;

    // Audio controls -------------------------------------------------

    procedure SetVolume(aValue: Single);
    function GetVolume(): Single;

    procedure SetVolumes(Value: TFloatArray);
    function GetVolumes(): TFloatArray;

    procedure SetPitch(aValue: Single);

    function SetReverb(Voice: TEffectsOnVoices;
                       pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                       pEnable: Boolean): HResult;

    function SetMasterLimiter(MasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS): HResult;

    // Compatibility API expected by MainFrm.pas ----------------------

    property ReverbParameters: TReverbI3DL2ParamArray read pvReverbI3DL2ParamArray;
    property SoundChannels: UINT32 read GetSoundChannels;
    property VolumeChannels: TFloatArray read GetVolumeChannels write SetVolumeChannelsProp;
    property Duration: MFTIME read GetDuration;
    property SamplesPerSec: UINT32 read GetSamplesPerSec;
    property RenderStatus: TRenderstatus read GetPlayStatus;
    property AudioEventData: TXaudio2EventData read GetAudioEventData;

    // Reverb effect assignments.
    property ReverbEffectOnSourceVoice: Boolean read bReverbEffectOnSourceVoice;
    property ReverbEffectOnMasterVoice: Boolean read bReverbEffectOnMasteringVoice;

    // Events.
    property OnAudioReadyEvent: TNotifyEvent read FOnAudioReadyEvent write FOnAudioReadyEvent;
    property OnAudioPlayingEvent: TNotifyEvent read FOnAudioPlayingEvent write FOnAudioPlayingEvent;
    property OnAudioPauzedEvent: TNotifyEvent read FOnAudioPauzedEvent write FOnAudioPauzedEvent;
    property OnAudioStoppedEvent: TNotifyEvent read FOnAudioStoppedEvent write FOnAudioStoppedEvent;

    property OnVoiceProcessingPassStartEvent: TNotifyEvent read FOnVoiceProcessingPassStartEvent write FOnVoiceProcessingPassStartEvent;
    property OnVoiceProcessingPassEndEvent: TNotifyEvent read FOnVoiceProcessingPassEndEvent write FOnVoiceProcessingPassEndEvent;
    property OnStreamEndEvent: TNotifyEvent read FOnStreamEndEvent write FOnStreamEndEvent;
    property OnBufferStartEvent: TNotifyEvent read FOnBufferStartEvent write FOnBufferStartEvent;
    property OnBufferEndEvent: TNotifyEvent read FOnBufferEndEvent write FOnBufferEndEvent;
  end;implementation


{ TEngineCommand }

class function TEngineCommand.Make(AKind: TEngineCommandKind): TEngineCommand;
begin

  Result.Kind := AKind;
  Result.FileName := '';
  Result.AutoPlay := False;
end;


class function TEngineCommand.MakeLoad(const AFileName: string;
                                       AAutoPlay: Boolean = False): TEngineCommand;
begin

  Result.Kind := cmdLoad;
  Result.FileName := AFileName;
  Result.AutoPlay := AAutoPlay;
end;


{ TXaudio2Engine - IXAudio2VoiceCallback }

// Called during each processing pass for each voice, just before XAudio2 reads data from the voice's buffer queue.
procedure TXaudio2Engine.OnVoiceProcessingPassStart(BytesRequired: UINT32);
begin
  // STUB  Do not use, only if you need for special purposes.
end;


procedure TXaudio2Engine.OnVoiceProcessingPassEnd();
begin
  // STUB  Do not use, only if you need for special purposes.
end;


procedure TXaudio2Engine.OnStreamEnd();
begin

  pvRenderStatus := rsEndOfStream;

  if Assigned(FOnStreamEndEvent) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnStreamEndEvent) then
                      FOnStreamEndEvent(Self);
                  end);
end;


procedure TXaudio2Engine.OnBufferStart(pBufferContext: Pointer);
begin

  // HARD STOP: never queue UI work while destroying
  if (pvRenderStatus = rsDestroying) then
    Exit;

  // For internal use.
  // Choose OnBufferEnd or OnBufferStart to indicate the is playing status.
  // pvRenderStatus := rsStartOfBuffer; << Is ok, but don't use rsPlaying
  pvRenderStatus := rsPlaying;

  if Assigned(FOnBufferStartEvent) then
    TThread.Queue(nil,
                  procedure
                  begin
                    if Assigned(FOnBufferStartEvent) then
                      FOnBufferStartEvent(Self);
                  end);
end;


procedure TXaudio2Engine.OnBufferEnd(pBufferContext: Pointer);
begin

  // HARD STOP: never queue UI work while destroying
  if (pvRenderStatus = rsDestroying) then
    Exit;

  // do NOT call XAudio2 here; just signal worker to refill
  TInterlocked.Increment(FNeedRefill);
  if Assigned(FBufEndEvent) then
    FBufEndEvent.SetEvent();

  // For internal use.
  // Choose OnBufferEnd or OnBufferStart to indicate the is playing status.
  // pvRenderStatus := rsEndOfBuffer; << Is ok, but don't use rsPlaying

  if Assigned(FOnBufferEndEvent) then
    TThread.Queue(nil,
                  procedure
                  begin
                    if Assigned(FOnBufferEndEvent) then
                      FOnBufferEndEvent(Self);
                  end);
end;


procedure TXaudio2Engine.OnLoopEnd(pBufferContext: Pointer);
begin

  pvRenderStatus := rsEndOfLoop;
end;


procedure TXaudio2Engine.OnVoiceError(pBufferContext: Pointer; Error: HResult);
begin

  pvRenderStatus := rsError;
end;


{ TXaudio2Engine }

constructor TXaudio2Engine.Create();
begin

  inherited;

  FLock := TCriticalSection.Create();
  FCmdEvent := TEvent.Create(nil,
                             False,
                             False,
                             '');

  FBufEndEvent := TEvent.Create(nil,
                                False,
                                False,
                                '');
  FNeedRefill := 0;
  FReachedEof := False;
  FNextSubmit := 0;


  FCmd := TEngineCommand.Make(cmdNone);

  pvAudioBytes := 0;
  pvSeekBaseSamples := 0;
  pvSeekBaseHns := 0;


  bReverbEffectOnSourceVoice := False;
  bReverbEffectOnMasteringVoice := False;

  pvReverbI3DL2ParamArray := GetReverbParamsI3DL2();

  pvRenderStatus := rsStopped;
  FXaudio2EventData.Reset();

  // Create effects.
  pvFxReverbEffect := TFxReverb.Create();
  pvFxMasterLimiter := TFxMasterLimiter.Create();
end;


destructor TXaudio2Engine.Destroy();
begin

  pvRenderStatus := rsDestroying;

  StopWorker;

  try

    FLock.Acquire();
    try

      if Assigned(pvSourceVoice) then
        begin

          pvSourceVoice.Stop(0,
                             XAUDIO2_COMMIT_NOW);

          pvSourceVoice.FlushSourceBuffers();
          pvSourceVoice.DestroyVoice();
          pvSourceVoice := nil;
        end;

      if Assigned(pvMasteringVoice) then
        begin

          pvMasteringVoice.DestroyVoice();
          pvMasteringVoice := nil;
        end;

      pvXAudio2 := nil;

      FreeStreamingBuffers;
      FreeAndNil(FBufEndEvent);
      FReader := nil;
      FReaderAttr := nil;

    finally
      FLock.Release;
    end;
  except
    // Do nothing.
  end;

  FreeAndNil(pvFxReverbEffect);
  FreeAndNil(pvFxMasterLimiter);


  if Assigned(pvWaveformatex) then
    begin

      CoTaskMemFree(pvWaveformatex);
      pvWaveformatex := nil;
    end;

  FreeAndNil(FCmdEvent);
  FreeAndNil(FLock);

  inherited;
end;


procedure TXaudio2Engine.LoadAndInitializeAsync(const audiofile: TFileName; AutoPlay: Boolean = True);
begin

  StartWorker();
  RequestCommand(TEngineCommand.MakeLoad(audiofile,
                                         AutoPlay));
end;


function TXaudio2Engine.InitializeXAudio2(replay: Boolean = False): HResult;
var
  hr: HResult;

begin

  pvBufferPrevPlayed := 0;
  pvBufferStart := 0;
  pvNewBufferPosition := 0;
  FXaudio2EventData.Reset;

  FreeAndNil(pvFxReverbEffect);
  FreeAndNil(pvFxMasterLimiter);

  FLock.Acquire();

  try

    if (pvXAudio2 = nil) then
      begin

        hr := XAudio2Create(@pvXAudio2,
                            XAUDIO2_STOP_ENGINE_WHEN_IDLE,  // Very important, because it prevents us to write start/stop XAudio engine code every time we write SourceVoive.Start/stop to prevent unexpected thread issues.
                            XAUDIO2_USE_DEFAULT_PROCESSOR);
        if FAILED(hr) then
          Exit(hr);

        hr := pvXAudio2.CreateMasteringVoice(@pvMasteringVoice);
        if FAILED(hr) then
          Exit(hr);
      end;

    // Recreate SourceVoice
    if Assigned(pvSourceVoice) then
      begin

        pvSourceVoice.Stop(0,
                           XAUDIO2_COMMIT_NOW);

        pvSourceVoice.FlushSourceBuffers();
        pvSourceVoice.DestroyVoice();
        pvSourceVoice := nil;
      end;

    hr := pvXAudio2.CreateSourceVoice(@pvSourceVoice,
                                      pvWaveformatex,
                                      0,
                                      XAUDIO2_DEFAULT_FREQ_RATIO,
                                      Self);
    if FAILED(hr) then
      Exit(hr);

    // Streaming model: prime ring buffers from MF SourceReader
    // (replaces single-buffer InitAudioBuffer)
    hr := PrimeStreaming;
    if FAILED(hr) then
      Exit(hr);

    pvFxReverbEffect := TFxReverb.Create();
    pvFxMasterLimiter := TFxMasterLimiter.Create();

    pvRenderStatus := rsInitialized;
    pvBufferPrevPlayed := 0;

    // Ensure refill counter starts clean
    TInterlocked.Exchange(FNeedRefill,
                          0);

  finally

    FLock.Release();
  end;

  if Assigned(FOnAudioReadyEvent) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnAudioReadyEvent) then
                      FOnAudioReadyEvent(Self);
                  end);

  // Do the loop...
  StartWorker();

  if replay then
    begin

      // Start playback (same semantics you had)
      FLock.Acquire();

      try

        if Assigned(pvSourceVoice) and Assigned(pvXAudio2) then
          begin

            hr := pvSourceVoice.Start(0,
                                      XAUDIO2_COMMIT_NOW);

            if SUCCEEDED(hr) then
               pvRenderStatus := rsPlaying
            else
              pvRenderStatus := rsError;
          end
      else
        hr := E_POINTER;
      finally

        FLock.Release();
      end;
  end;

  Result := hr;
end;



procedure TXaudio2Engine.RequestCommand(const ACmd: TEngineCommand);
begin

  if not Assigned(FCmdEvent) then
    Exit;

  FLock.Acquire();
  try

    FCmd := ACmd;
  finally

    FLock.Release();
  end;

  FCmdEvent.SetEvent;
end;


procedure TXaudio2Engine.StartWorker();
begin

  if Assigned(FWorker) then
    Exit;

  FWorker := TThread.CreateAnonymousThread(procedure
                                           begin
                                             WorkerProc;
                                           end);

  FWorker.FreeOnTerminate := False;
  FWorker.Start;
end;


procedure TXaudio2Engine.StopWorker();
begin

  if not Assigned(FWorker) then
    Exit;

  RequestCommand(TEngineCommand.Make(cmdQuit));

  try

    FWorker.WaitFor();
  except
    // Do nothing
  end;

  FreeAndNil(FWorker);
end;


procedure TXaudio2Engine.PollStateAndNotify();
var
  voiceState: XAUDIO2_VOICE_STATE;
  playedAbs: Int64;
  playedDelta: Int64;
  absSamples: Int64;
  absHns: Int64;
  buffersQueued: UINT32;
  needReset: Boolean;
  doPlayingNotify: Boolean;
  playingCb: TNotifyEvent;

begin

  if (pvRenderStatus = rsDestroying) then
    Exit;

  needReset := False;
  doPlayingNotify := False;
  playingCb := nil;

  FLock.Acquire();

  try

    if not Assigned(pvSourceVoice) then
      begin

        // No voice -> consider state reset-worthy.
        needReset := True;
      end
    else
      begin

        // IMPORTANT: flags = 0 so SamplesPlayed is valid.
        pvSourceVoice.GetState(voiceState,
                               0);

        buffersQueued := voiceState.BuffersQueued;
        playedAbs := Int64(voiceState.SamplesPlayed);

        // If we are not actively playing and there are no buffers queued,
        // treat as stopped/ended and reset counters.
        if (pvRenderStatus in [rsStopped, rsEndOfStream]) or
           ((buffersQueued = 0) and (pvRenderStatus <> rsPlaying)) then
          begin

            needReset := True;
          end
        else
          if (pvRenderStatus = rsPlaying) then
            begin

              // SamplesPlayed is cumulative for the lifetime of the SourceVoice.
              // Convert to "since last (re)start/seek" by subtracting our baseline.
              playedDelta := playedAbs - pvBufferPrevPlayed;
              if (playedDelta < 0) then
                playedDelta := 0;

              absSamples := pvSeekBaseSamples + playedDelta;

              FXaudio2EventData.Position := pvSeekBaseSamples;      // base samples (seek).
              FXaudio2EventData.SamplesProcessed := playedDelta;    // delta since seek/start.

              if (pvSamplesPerSecond <> 0) then
                absHns := (absSamples * 10000000) div pvSamplesPerSecond
              else
                absHns := 0;

              FXaudio2EventData.TimePlayed := absHns;

              // Optional: basic "ran dry" detection.
              // For multi-buffer streaming, buffersQueued should normally stay > 0 while playing.
              if (buffersQueued = 0) then
                pvRenderStatus := rsEndOfStream;

              // Prepare notify (DO NOT call while holding lock)
              playingCb := FOnAudioPlayingEvent;
              doPlayingNotify := Assigned(playingCb);
            end;
      end;

    if needReset then
      begin

        // Reset all counters (UI must show 0).
        pvSeekBaseSamples := 0;
        pvBufferPrevPlayed := 0;
        pvBufferStart := 0;
        pvNewBufferPosition := 0;
        FXaudio2EventData.Reset;

        // No "playing" notify when we reset
        doPlayingNotify := False;
        playingCb := nil;
      end;

  finally

    FLock.Release();
  end;

  // Fire callback on the main thread OUTSIDE the lock
  if doPlayingNotify then
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(playingCb) then
          playingCb(Self);
      end);
end;


procedure TXaudio2Engine.WorkerProc();
var
  hr: HResult;
  cmd: TEngineCommand;
  doPoll: Boolean;

  handles: array[0..1] of THandle;
  wr: DWORD;
  refillCount,
  i: Integer;

begin

  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);

  try

    // 0 = command event, 1 = buffer-end/refill event
    handles[0] := FCmdEvent.Handle;
    handles[1] := FBufEndEvent.Handle;

    while not TThread.CurrentThread.CheckTerminated do
      begin

        // Wait for a command OR a buffer-end OR timeout (poll tick)
        wr := WaitForMultipleObjects(2,
                                     @handles[0],
                                     False,
                                     50);

      // ----------------------------------------------------------------------
      // 1) Handle COMMANDS FIRST
      // ----------------------------------------------------------------------
      if (wr = WAIT_OBJECT_0) then
        begin

          cmd := TEngineCommand.Make(cmdNone);

          FLock.Acquire();

          try

            cmd := FCmd;
            FCmd := TEngineCommand.Make(cmdNone);
          finally

            FLock.Release();
          end;

          // Commands -----------------------------------------------------------
          case cmd.Kind of

            cmdPlay:
              begin

                hr := E_POINTER;

                FLock.Acquire();

                try

                  if Assigned(pvSourceVoice) then
                    begin

                      hr := pvSourceVoice.Start(0,
                                                XAUDIO2_COMMIT_NOW);
                      if SUCCEEDED(hr) then
                        pvRenderStatus := rsPlaying
                      else
                        pvRenderStatus := rsError;
                    end;
                finally

                  FLock.Release();
                end;

                if SUCCEEDED(hr) and Assigned(FOnAudioPlayingEvent) then
                  TThread.Queue(nil,
                                procedure
                                begin

                                  if Assigned(FOnAudioPlayingEvent) then
                                    FOnAudioPlayingEvent(Self);
                                end);
              end;

            cmdPause:
              begin

                hr := E_POINTER;

                FLock.Acquire();

                try

                  if Assigned(pvSourceVoice) then
                    begin

                      hr := pvSourceVoice.Stop(0,
                                               XAUDIO2_COMMIT_NOW);
                      if SUCCEEDED(hr) then
                        pvRenderStatus := rsPauzed;
                    end;
                finally

                  FLock.Release();
                end;

                if SUCCEEDED(hr) and Assigned(FOnAudioPauzedEvent) then
                  TThread.Queue(nil,
                                procedure
                                begin
                                  if Assigned(FOnAudioPauzedEvent) then
                                    FOnAudioPauzedEvent(Self);
                                end);
              end;

            cmdStop:
              begin

                // If already stopped, do NOT exit the worker thread.
                FLock.Acquire();

                try
                  if (pvRenderStatus = rsStopped) then
                    Continue;
                finally

                  FLock.Release();
                end;

                FLock.Acquire();

                try

                  if Assigned(pvSourceVoice) then
                    begin

                      pvSourceVoice.Stop(0,
                                         XAUDIO2_COMMIT_NOW);
                      pvSourceVoice.FlushSourceBuffers();
                      pvSourceVoice.Discontinuity();

                      // Prevent any further refill after stop
                      TInterlocked.Exchange(FNeedRefill,
                                            0);

                      // Deterministic reset for UI/Replay
                      pvSeekBaseSamples := 0;
                      pvBufferPrevPlayed := 0;
                      pvBufferStart := 0;
                      pvNewBufferPosition := 0;
                      FXaudio2EventData.Reset;

                      pvRenderStatus := rsStopped;
                    end;
                finally

                  FLock.Release();
                end;

                if Assigned(FOnAudioStoppedEvent) then
                  TThread.Queue(nil,
                                procedure
                                begin

                                  if Assigned(FOnAudioStoppedEvent) then
                                    FOnAudioStoppedEvent(Self);
                                end);
              end;

            cmdLoad:
              begin

                // Stop old voice and clear queued buffers (streaming model)
                FLock.Acquire();

                try

                  if Assigned(pvSourceVoice) then
                    begin

                      pvSourceVoice.Stop(0,
                                         XAUDIO2_COMMIT_NOW);
                      pvSourceVoice.FlushSourceBuffers;
                      pvSourceVoice.DestroyVoice;
                      pvSourceVoice := nil;
                    end;

                  pvRenderStatus := rsStopped;
                  TInterlocked.Exchange(FNeedRefill,
                                        0);
                finally

                  FLock.Release();
                end;

                hr := LoadFile(cmd.FileName);

                if FAILED(hr) then
                  begin

                    FLock.Acquire();

                    try

                      pvRenderStatus := rsError;
                    finally

                      FLock.Release();
                    end;

                    Continue;
                  end;

                hr := InitializeXAudio2(False);

                if FAILED(hr) then
                  begin

                    FLock.Acquire();

                    try

                      pvRenderStatus := rsError;
                    finally

                    FLock.Release();
                    end;

                    Continue;
                  end;

                if cmd.AutoPlay then
                  RequestCommand(TEngineCommand.Make(cmdPlay))
                else
                  begin

                    FLock.Acquire();

                    try

                      pvRenderStatus := rsInitialized;
                    finally

                      FLock.Release();
                    end;
                  end;
              end;

            cmdQuit:
              Break;
          end; // case.
        end;

      // ----------------------------------------------------------------------
      // 2) BUFFER-END REFILL (multi-buffer streaming).
      //    IMPORTANT: drain using Exchange-to-zero (never goes negative).
      // ----------------------------------------------------------------------
      if (wr = WAIT_OBJECT_0 + 1) then
        begin

          refillCount := TInterlocked.Exchange(FNeedRefill,
                                               0);

          for i := 1 to refillCount do
            begin

              // If we stopped, do not submit more.
              FLock.Acquire();

              try
                if (pvRenderStatus <> rsPlaying) or (pvSourceVoice = nil) then
                  Break;
              finally

                FLock.Release();
              end;

              hr := SubmitNextStreamBuffer(False);

              if (hr = S_FALSE) then
                Break // EOF reached (last submitted buffer should have END_OF_STREAM).
              else
                if FAILED(hr) then
                  begin

                    FLock.Acquire();

                    try

                      pvRenderStatus := rsError;
                    finally

                      FLock.Release();
                    end;

                    Break;
                  end;
            end;
        end;

      // ----------------------------------------------------------------------
      // 3) POLL TICK (~20 Hz) - do NOT call PollStateAndNotify under lock.
      // ----------------------------------------------------------------------

      FLock.Acquire();

      try

        doPoll := (pvRenderStatus = rsPlaying);
      finally

        FLock.Release;
      end;

      if doPoll then
        PollStateAndNotify;
    end;

  finally
    CoUninitialize();
  end;
end;



function TXaudio2Engine.LoadFile(const audiofile: TFileName): HResult;
var
  hr: HResult;
  newDuration: LONGLONG;

begin

  if not FileExists(audiofile) then
    Exit(ERROR_FILE_NOT_FOUND);

  // Duration is still useful for UI (progress max etc.)
  newDuration := 0;
  hr := GetFileDuration(StrToPWideChar(audiofile),
                        newDuration);
  if FAILED(hr) then
    Exit(hr);

  // Streaming decode setup:
  // - Creates FReader
  // - Forces uncompressed Float (or PCM)
  // - Creates pvWaveformatex (and sets pvwaveformatlength)
  // - Sets pvChannels, pvSamplesPerSecond, pvVolumeChannels length, resets EOF flags, etc.
  hr := SetupSourceReader(audiofile);
  if FAILED(hr) then
    Exit(hr);

  // Store metadata
  FLock.Acquire();

  try

    pvFileName := audiofile;
    pvSourceFileDuration := newDuration;
  finally

    FLock.Release();
  end;

  Result := S_OK;
end;


function TXaudio2Engine.Play(): HResult;
begin

  RequestCommand(TEngineCommand.Make(cmdPlay));
  Result := S_OK;
end;


function TXaudio2Engine.Pause(): HResult;
begin

  RequestCommand(TEngineCommand.Make(cmdPause));
  Result := S_OK;
end;


function TXaudio2Engine.Stop(): HResult;
begin

  RequestCommand(TEngineCommand.Make(cmdStop));
  Result := S_OK;
end;


function TXaudio2Engine.GotoNewPosition(const SamplePos: LONGLONG): HResult;
var
  hr: HResult;
  wasPlaying: Boolean;
  hns: Int64;
  pv: PROPVARIANT;
  voiceState: XAUDIO2_VOICE_STATE;

begin

  // Snapshot state + validate
  FLock.Acquire();

  try

    wasPlaying := (pvRenderStatus = rsPlaying);

    if (FReader = nil) or
       (pvSourceVoice = nil) or
       (pvWaveformatex = nil) then
      Exit(E_POINTER);

    if (pvSamplesPerSecond = 0) then
      Exit(E_POINTER);
  finally

    FLock.Release();
  end;

  // Convert sample position -> MFTIME (100ns)
  hns := (SamplePos * 10000000) div Int64(pvSamplesPerSecond);
  if (hns < 0) then
    hns := 0;

  // Stop voice and clear queued audio (do not hold lock here)
  pvSourceVoice.Stop(0, XAUDIO2_COMMIT_NOW);
  pvSourceVoice.FlushSourceBuffers();
  pvSourceVoice.Discontinuity();

  // Seek the SourceReader
  PropVariantInit(pv);
  try

    pv.vt := VT_I8;
    pv.hVal.QuadPart := hns;

    hr := FReader.SetCurrentPosition(GUID_NULL,
                                     pv);
    if FAILED(hr) then
      Exit(hr);
  finally

    PropVariantClear(pv);
  end;

  // Reset streaming state/counters BEFORE priming
  FLock.Acquire();

  try

    pvSeekBaseSamples := SamplePos;

    // Reset UI accumulator
    FXaudio2EventData.Reset();

    // Reset streaming state
    FReachedEof := False;
    TInterlocked.Exchange(FNeedRefill, 0);

    // We'll set pvBufferPrevPlayed AFTER priming (baseline of SamplesPlayed)
    pvBufferStart := 0;
    pvNewBufferPosition := 0;

    // Keep status semantic: paused if it was not playing, else playing after restart below
    if wasPlaying then
      pvRenderStatus := rsPlaying
    else
      pvRenderStatus := rsPauzed;
  finally

    FLock.Release();
  end;

  // Prime fresh buffers starting at the seek location
  hr := PrimeStreaming();

  if FAILED(hr) then
    begin

      FLock.Acquire();

    try

      pvRenderStatus := rsError;
    finally

      FLock.Release();
    end;
    Exit(hr);
  end;

  // IMPORTANT: Baseline SamplesPlayed AFTER seek/flush/prime
  // SamplesPlayed is cumulative for the lifetime of the SourceVoice.
  pvSourceVoice.GetState(voiceState,
                         0);

  FLock.Acquire();

  try

    pvBufferPrevPlayed := Int64(voiceState.SamplesPlayed);
  finally

    FLock.Release();
  end;

  // Resume playback if needed
  if wasPlaying then
    begin

      hr := pvSourceVoice.Start(0,
                                XAUDIO2_COMMIT_NOW);
    if FAILED(hr) then
      begin

        FLock.Acquire();

      try

        pvRenderStatus := rsError;
      finally

        FLock.Release();
      end;
      Exit(hr);
    end;
  end;

  Result := S_OK;
end;



procedure TXaudio2Engine.SetVolume(aValue: Single);
begin

  FLock.Acquire();

  try
    if Assigned(pvSourceVoice) then
      pvSourceVoice.SetVolume(aValue,
                              XAUDIO2_COMMIT_NOW);
  finally

    FLock.Release();
  end;
end;


function TXaudio2Engine.GetVolume(): Single;
var
  v: Single;

begin

  v := 1.0;
  FLock.Acquire();

  try

    if Assigned(pvSourceVoice) then
      pvSourceVoice.GetVolume(v);
  finally

    FLock.Release();
  end;
  Result := v;
end;


procedure TXaudio2Engine.SetVolumes(Value: TFloatArray);
begin

  FLock.Acquire();

  try

    pvVolumeChannels := Value;
    if Assigned(pvSourceVoice) and (Length(Value) > 0) then
      pvSourceVoice.SetChannelVolumes(Length(Value),
                                      @Value[0],
                                      XAUDIO2_COMMIT_NOW);
  finally

    FLock.Release();
  end;
end;


function TXaudio2Engine.GetVolumes(): TFloatArray;
begin

  FLock.Acquire();

  try

    Result := pvVolumeChannels;
  finally

    FLock.Release();
  end;
end;


procedure TXaudio2Engine.SetPitch(aValue: Single);
var
  ratio: Single;

begin

  ratio := aValue;

  if (ratio < MIN_PITCH) then
    ratio := MIN_PITCH;
  if (ratio > MAX_PITCH) then
    ratio := MAX_PITCH;

  FLock.Acquire();

  try

    if Assigned(pvSourceVoice) then
      pvSourceVoice.SetFrequencyRatio(ratio,
                                      XAUDIO2_COMMIT_NOW);
  finally

    FLock.Release();
  end;
end;


function TXaudio2Engine.SetReverb(Voice: TEffectsOnVoices;
                                  pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                                  pEnable: Boolean): HResult;
var
  ppVoice: PIXAudio2Voice;

begin

  Result := E_FAIL;

  if not Assigned(pvSourceVoice) or not Assigned(pvMasteringVoice) then
    Exit;

  // == Add reverb effect. ======================================
  if (Voice = afxSourceVoice) then
    ppVoice := @pvSourceVoice
  else
    ppVoice := @pvMasteringVoice;

  Result := pvFxReverbEffect.CreateNativeReverbEffect(ppVoice,
                                                      pReverbParams,
                                                      pvChannels,
                                                      pEnable);

  if SUCCEEDED(Result) then
    begin
      if pEnable then
        Result := ppVoice.EnableEffect(0,
                                       XAUDIO2_COMMIT_NOW)
      else
        Result := ppVoice.DisableEffect(0,
                                        XAUDIO2_COMMIT_NOW);
        pvFxReverbEffect.FxReverbEffectEnabled := SUCCEEDED(Result);
    end;

  if SUCCEEDED(Result) then
    begin

      if (Voice = afxSourceVoice) then
        bReverbEffectOnSourceVoice := pEnable
      else
        bReverbEffectOnMasteringVoice := pEnable;
    end;
end;


function TXaudio2Engine.SetMasterLimiter(MasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS): HResult;
begin

  if not Assigned(pvFxMasterLimiter) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := pvFxMasterLimiter.CreateMasterLimiter(MasterLimiterparams);
end;


procedure TXaudio2Engine.FreeStreamingBuffers();
var
  i: Integer;

begin
  for i := 0 to High(FStreamBuf) do
  begin
    if FStreamBuf[i].Data <> nil then
    begin
      FreeMem(FStreamBuf[i].Data);
      FStreamBuf[i].Data := nil;
    end;
    FStreamBuf[i].CapBytes := 0;
    FStreamBuf[i].UsedBytes := 0;
    FillChar(FStreamBuf[i].XA, SizeOf(FStreamBuf[i].XA), 0);
  end;
  FNextSubmit := 0;
end;


procedure TXaudio2Engine.AllocStreamingBuffers();
var
  i: Integer;
  bytesPerSec: UInt64;
  cap: UInt64;

begin

  FreeStreamingBuffers;

  if (pvWaveformatex = nil) then Exit;
  if (pvWaveformatex.nBlockAlign = 0) or (pvWaveformatex.nAvgBytesPerSec = 0) then Exit;

  bytesPerSec := UInt64(pvWaveformatex.nAvgBytesPerSec);
  cap := (bytesPerSec * STREAM_BUFFER_MS) div 1000;

  // align to block size
  cap := (cap div pvWaveformatex.nBlockAlign) * pvWaveformatex.nBlockAlign;
  if cap = 0 then cap := pvWaveformatex.nBlockAlign * 512;

  // XAUDIO2_BUFFER.AudioBytes is 32-bit (dcc32), keep it sane
  if cap > High(Cardinal) then cap := High(Cardinal);

  for i := 0 to High(FStreamBuf) do
  begin
    GetMem(FStreamBuf[i].Data, Cardinal(cap));
    FStreamBuf[i].CapBytes := Cardinal(cap);
    FStreamBuf[i].UsedBytes := 0;

    FillChar(FStreamBuf[i].XA, SizeOf(FStreamBuf[i].XA), 0);
    FStreamBuf[i].XA.pAudioData := FStreamBuf[i].Data;
    // mark context if you want; we don’t need it for refill counter
    FStreamBuf[i].XA.pContext := Pointer(NativeUInt(i));
  end;

  FNextSubmit := 0;
end;


function TXaudio2Engine.SetupSourceReader(const audiofile: TFileName): HResult;
var
  hr: HResult;
  nativeType,
  partialType,
  curType: IMFMediaType;
  majorType,
  subType: TGUID;

begin

  FReader := nil;
  FReaderAttr := nil;

  hr := MFCreateAttributes(FReaderAttr,
                           1);
  if FAILED(hr) then
    Exit(hr);

  hr := FReaderAttr.SetUINT32(MF_LOW_LATENCY,
                              1);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateSourceReaderFromURL(StrToPWideChar(audiofile),
                                    FReaderAttr,
                                    FReader);
  if FAILED(hr) then
    Exit(hr);

  hr := FReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                   0,
                                   @nativeType);
  if FAILED(hr) then
    Exit(hr);

  hr := nativeType.GetGUID(MF_MT_MAJOR_TYPE,
                           majorType);
  if FAILED(hr) then
    Exit(hr);

  if not IsEqualGUID(majorType,
                     MFMediaType_Audio) then
    Exit(MF_E_INVALIDMEDIATYPE);

  hr := nativeType.GetGUID(MF_MT_SUBTYPE,
                           subType);
  if FAILED(hr) then
    Exit(hr);

  // Force float output (teaches MF decode graph; supports XAPO).
  if not (IsEqualGUID(subType,
                      MFAudioFormat_Float) or
          IsEqualGUID(subType,
                      MFAudioFormat_PCM)) then
    begin

      hr := MFCreateMediaType(partialType);
      if FAILED(hr) then
        Exit(hr);

      hr := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Audio);
      if FAILED(hr) then
        Exit(hr);

      hr := partialType.SetGUID(MF_MT_SUBTYPE,
                                MFAudioFormat_Float);
      if FAILED(hr) then
        Exit(hr);

      hr := FReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                        0,
                                        partialType);
      if FAILED(hr) then
        Exit(hr);
    end;

  hr := FReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                    @curType);
  if FAILED(hr) then
    Exit(hr);

  if Assigned(pvWaveformatex) then
    CoTaskMemFree(pvWaveformatex);

  hr := MFCreateWaveFormatExFromMFMediaType(curType,
                                            pvWaveformatex,
                                            pvwaveformatlength,
                                            MFWaveFormatExConvertFlag_ForceExtensible);
  if FAILED(hr) then
    Exit(hr);

  pvChannels := MFGetAttributeUINT32(curType,
                                     MF_MT_AUDIO_NUM_CHANNELS,
                                     2);
  SetLength(pvVolumeChannels,
            pvChannels);

  pvSamplesPerSecond := MFGetAttributeUINT32(curType,
                                             MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                             0);

  FReachedEof := False;
  Result := S_OK;
end;


function TXaudio2Engine.ReadPcmInto(var Dest: PByte;
                                    var DestCap: Cardinal;
                                    out Wrote: Cardinal): HResult;
var
  hr: HResult;
  uFlags: UINT32;
  sample: IMFSample;
  buf: IMFMediaBuffer;
  pData: PByte;
  cbData: DWORD;
  copyBytes: Cardinal;

begin

  Wrote := 0;
  Result := S_OK;

  if FReachedEof then
    Exit(S_FALSE);

  while Wrote < DestCap do
  begin
    uFlags := 0;
    sample := nil;

    hr := FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                             0, nil, @uFlags, nil, @sample);
    if FAILED(hr) then Exit(hr);

    if (uFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0 then
    begin
      FReachedEof := True;
      Break;
    end;

    if sample = nil then
      Continue;

    hr := sample.ConvertToContiguousBuffer(@buf);
    if FAILED(hr) then Exit(hr);

    hr := buf.Lock(pData, nil, @cbData);
    if FAILED(hr) then Exit(hr);
    try
      // copy as much as fits
      copyBytes := DestCap - Wrote;
      if cbData < copyBytes then copyBytes := cbData;

      Move(pData^, PByte(NativeUInt(Dest) + Wrote)^, copyBytes);
      Inc(Wrote, copyBytes);
    finally
      buf.Unlock;
    end;

    // If sample had more bytes than we took, that’s rare with typical MF audio
    // For full correctness you’d keep a “leftover sample” cache; for teaching
    // MF principles this is usually sufficient because MF delivers reasonable chunking.
    if Wrote >= DestCap then
      Break;
  end;

  if (Wrote = 0) and FReachedEof then
    Result := S_FALSE;
end;


function TXaudio2Engine.SubmitNextStreamBuffer(IsLast: Boolean): HResult;
var
  idx: Integer;
  wrote: Cardinal;
  hr: HResult;

begin
  idx := FNextSubmit;
  Inc(FNextSubmit);
  if FNextSubmit >= STREAM_BUFFER_COUNT then
    FNextSubmit := 0;

  FStreamBuf[idx].UsedBytes := 0;
  FillChar(FStreamBuf[idx].XA, SizeOf(FStreamBuf[idx].XA), 0);
  FStreamBuf[idx].XA.pAudioData := FStreamBuf[idx].Data;
  FStreamBuf[idx].XA.pContext := Pointer(NativeUInt(idx));
  FStreamBuf[idx].XA.Flags := 0;

  hr := ReadPcmInto(FStreamBuf[idx].Data, FStreamBuf[idx].CapBytes, wrote);
  if FAILED(hr) then Exit(hr);

  if (hr = S_FALSE) and (wrote = 0) then
  begin
    // no more data to submit
    Exit(S_FALSE);
  end;

  // align wrote to block
  if (pvWaveformatex <> nil) and (pvWaveformatex.nBlockAlign <> 0) then
    wrote := (wrote div pvWaveformatex.nBlockAlign) * pvWaveformatex.nBlockAlign;

  FStreamBuf[idx].UsedBytes := wrote;
  FStreamBuf[idx].XA.AudioBytes := wrote;

  // End-of-stream marker when we reached EOF and this is the last bytes
  if FReachedEof then
    FStreamBuf[idx].XA.Flags := XAUDIO2_END_OF_STREAM;

  Result := pvSourceVoice.SubmitSourceBuffer(@FStreamBuf[idx].XA);
end;


function TXaudio2Engine.PrimeStreaming: HResult;
var
  i: Integer;
  hr: HResult;

begin

  if (FReader = nil) or (pvSourceVoice = nil) then Exit(E_POINTER);

  // reset refill counter & EOF
  TInterlocked.Exchange(FNeedRefill, 0);
  FReachedEof := False;
  FNextSubmit := 0;

  // allocate small fixed buffers
  AllocStreamingBuffers;

  // submit initial buffers
  for i := 0 to STREAM_BUFFER_COUNT-1 do
  begin
    hr := SubmitNextStreamBuffer(False);
    if hr = S_FALSE then
      Break;
    if FAILED(hr) then
      Exit(hr);
  end;

  Result := S_OK;
end;


{ --- Compatibility getters/setters --- }

function TXaudio2Engine.GetPlayStatus(): TRenderstatus;
begin

  Result := pvRenderStatus;
end;


function TXaudio2Engine.GetDuration(): MFTIME;
begin

  Result := pvSourceFileDuration;
end;


function TXaudio2Engine.GetSamplesPerSec: UINT32;
begin

  Result := pvSamplesPerSecond;
end;


function TXaudio2Engine.GetSoundChannels(): UINT32;
begin

  Result := pvChannels;
end;


function TXaudio2Engine.GetVolumeChannels(): TFloatArray;
begin

  Result := GetVolumes;
end;


procedure TXaudio2Engine.SetVolumeChannelsProp(const Value: TFloatArray);
begin

  SetVolumes(Value);
end;


function TXaudio2Engine.GetAudioEventData(): TXaudio2EventData;
begin

  FLock.Acquire();

  try

    Result := FXaudio2EventData;
  finally

    FLock.Release();
  end;
end;


{ --- Reverb preset list builders (compat) --- }

function TXaudio2Engine.GetReverbParamsI3DL2(): TReverbI3DL2ParamArray;
var
  presets: TReverbI3DL2ParamArray;
  i: Integer;

begin
  // Reuse the official preset list from XAudio2_FXReverb
  presets := XAudio2_FXReverb.GetReverbParams();

  for i := 1 to High(presets) do
    Result[i - 1] := presets[i];
end;


procedure TXaudio2EventData.Reset();
begin

  SamplesProcessed := 0;
  Position := 0;
  TimePlayed := 0;
end;

end.

