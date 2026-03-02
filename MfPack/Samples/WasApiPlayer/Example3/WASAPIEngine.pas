// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WASAPIEngine.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.9
// Description: The main engine that acts as Chief In Command in this sample.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
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
unit WASAPIEngine;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  System.Services.Avrt,
  {VCL}
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.MMDeviceApi,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  MfAudioHighMidLowTypes,
  MfAudioHighMidLowMFT;


const

  REFTIMES_PER_SEC = 10000000;
  MS100_PER_SEC = 1000000;
  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;

type

  TSampleFormat = (sfInt16,
                   sfInt24,
                   sfInt32,
                   sfFloat32);

  TDeviceState = (dsUninitialized,
                  dsError,
                  dsInitialized,
                  dsReady, // File decoded and engine initialized.
                  dsPlay,
                  dsPause,
                  dsStop);

  // Event callbacks (always raised on the main/UI thread via TThread.Queue).
  TWasApiStateEvent = procedure(Sender: TObject;
                                const NewState: TDeviceState) of object;

  TWasApiErrorEvent = procedure(Sender: TObject;
                                const Msg: string;
                                const Hr: HRESULT) of object;

  TWasApiReadyEvent = procedure(Sender: TObject) of object;

  // Position100ns: time position (100ns units), RawPosition: raw audio clock position.
  TWasApiProcessedEvent = procedure(Sender: TObject;
                                    const Position100ns: Int64;
                                    const RawPosition: UInt64) of object;
  TWasApiEndedEvent = procedure(Sender: TObject) of object;


  TEngineCmdKind = (ckLoadFile,
                    ckPlay,
                    ckPause,
                    ckStop,
                    ckSetVolume,
                    ckSeek,
                    ckShutdown,

                    // MFT High / Mid / Low.
                    ckEQEnable,
                    ckEQSetLowDb,
                    ckEQSetMidDb,
                    ckEQSetHighDb,
                    ckEQSetMidMode,
                    ckEQSetMidQ,
                    ckEQSetLowFreqHz,
                    ckEQSetMidFreqHz,
                    ckEQSetHighFreqHz,
                    ckEQSetLowShelfSlope,
                    ckEQSetHighShelfSlope,
                    ckEQSetRampMode,
                    ckEQSetRampTimeMs);

  TEngineCommand = record
    Kind: TEngineCmdKind;
    FileName: string;
    FileDuration: Int64;
    VolL: Single;
    VolR: Single;
    SeekPos100ns: Int64;  // Used to set FBasePos100ns and compute FOffset.

    // EQ MFT (High/Mid/Low)
    EqEnabled: Boolean;
    EqLowDb: Integer;
    EqMidDb: Integer;
    EqHighDb: Integer;
    EqMidMode: Integer;  // Ord(TMfMidMode)
    EqMidQ: Single;
    EqLowFreqHz: Single;
    EqMidFreqHz: Single;
    EqHighFreqHz: Single;
    EqLowShelfSlope: Single;
    EqHighShelfSlope: Single;
    EqRampMode: Integer; // Ord(TRampMode)
    EqRampTimeMs: Integer;

    class function LoadFile(const AFileName: string;
                            ADuration: Int64): TEngineCommand; static;
    class function Play(): TEngineCommand; static;
    class function Pause(): TEngineCommand; static;
    class function Stop(): TEngineCommand; static;
    class function Seek(const APos100ns: Int64): TEngineCommand; static;
    class function SetVolume(aLeft,
                             aRight: Single): TEngineCommand; static;

    // EQ commands
    class function EQEnable(const AEnabled: Boolean): TEngineCommand; static;
    class function EQSetLowDb(const ALowDb: Integer): TEngineCommand; static;
    class function EQSetMidDb(const AMidDb: Integer): TEngineCommand; static;
    class function EQSetHighDb(const AHighDb: Integer): TEngineCommand; static;
    class function EQSetRampMode(const ARampMode: Integer): TEngineCommand; static; // Ord(TRampMode)
    class function EQSetRampTimeMs(const AMs: Integer): TEngineCommand; static;

    class function EQSetMidMode(const AMidMode: Integer): TEngineCommand; static;
    class function EQSetMidQ(const AMidQ: Single): TEngineCommand; static;
    class function EQSetLowFreqHz(const Hz: Single): TEngineCommand; static;
    class function EQSetMidFreqHz(const Hz: Single): TEngineCommand; static;
    class function EQSetHighFreqHz(const Hz: Single): TEngineCommand; static;
    class function EQSetLowShelfSlope(const S: Single): TEngineCommand; static;
    class function EQSetHighShelfSlope(const S: Single): TEngineCommand; static;

    class function Shutdown(): TEngineCommand; static;
  end;


  // Forwarded
  TWasApiEngine = class;

  // TWasApiEngine thread definition.
  TWasApiEngineThread = class(TThread)
  private

    FEngine: TWasApiEngine;
  protected

    procedure Execute; override;
  public

    constructor Create(AEngine: TWasApiEngine);
  end;


  TWasApiEngine = class(TObject)
  private

    // WASAPI
    pvAudioClient: IAudioClient;
    pvAudioStreamVolume: IAudioStreamVolume;
    pvRenderClient: IAudioRenderClient;
    pvAudioClock: IAudioClock;

    pvDeviceState: TDeviceState;

    // Decoded PCM bytes
    pvBytes: PByte;
    pvBytesLength: UINT32;
    pvwaveformatlength: UINT32;

    pvSourceWfx: PWAVEFORMATEX;
    FSourceBytesPerSec: UInt32;
    FSourceBlockAlign:  UInt32;

    pvRenderWfx: PWAVEFORMATEX;
    FClientBlockAlign: Word;
    // PCM converter.
    FDynFloatBuf: PSingle; // Scratch float interleaved.
    FDynFloatBufSamples: Integer; // Number of float samples allocated.

    // Playback
    FOffset: UINT32;
    FSampleFormat: TSampleFormat;
    FBytesPerSample: Integer;
    pvSoundChannels: WORD;

    // Seek
    FBasePos100ns: Int64;
    FDuration100ns: Int64;

    pvErrStatus: HResult;

    // MMCSS.
    pvMmcssHandle: THandle;
    pvMmcssTaskIndex: DWord;

    // Events.
    pvAudioSamplesReadyEvent: THandle;

    // Threading / commands
    FThread: TWasApiEngineThread;
    FCmdEvent: THandle;
    FCmdCS: TCriticalSection;
    FCmdQueue: TQueue<TEngineCommand>;
    FTerminateEvent: THandle;
    FRequestStop: Boolean;
    FRequestPause: Boolean;

    // UI callbacks
    FOnStateChanged: TWasApiStateEvent;
    FOnError: TWasApiErrorEvent;
    FOnReady: TWasApiReadyEvent;
    FOnProcessed: TWasApiProcessedEvent;
    FOnEnded: TWasApiEndedEvent;

    // EQ MFT
    FHighMidLowMFT: IMFTransform;
    FHighMidLowCtrl: IMfHighMidLowControl;
    FEQEnabled: Boolean;
    FEQTypeSet: Boolean;   // media types applied to MFT

    pvBufferFrameCount: UINT32;

    procedure SetState(const NewState: TDeviceState);
    procedure RaiseError(const Msg: string; const Hr: HRESULT);
    procedure RaiseReady();
    procedure RaiseProcessed(const Position100ns: Int64; const RawPosition: UInt64);
    procedure RaiseEnded();

    procedure EnqueueCommand(const Cmd: TEngineCommand);
    function DequeueCommand(out Cmd: TEngineCommand): Boolean;

    function InitializeAudioEngine(): HRESULT;
    function SetFormat(pwfx: PWAVEFORMATEX): HRESULT;

    procedure ResetAudioData(pFreeSourceStream: Boolean);

    function LoadFileInternal(const audiofile: TFileName;
                              fileDuration: LONGLONG): HResult;
    function LoadData(bufferFrameCount: UINT32;
                      pData: PByte;
                      var flags: DWORD): HRESULT;

    function PlayAudioStreamInternal(): HRESULT;
    // Helper for PlayAudioStreamInternal.
    function ProcessEQBuffer(pData: PByte;
                             const ByteCount: DWORD): HRESULT;

    procedure ProcessControlCommand(const Cmd: TEngineCommand);

  public

    constructor Create();
    destructor Destroy(); override;

    // Commands (threaded)
    function OpenFile(const audiofile: TFileName;
                      fileDuration100ns: LONGLONG): HRESULT;
    function Start(): HResult;
    function Stop(): HResult;
    function Pause(): HResult;
    function SeekTo(const Pos100ns: Int64): HRESULT;

    function SetVolumes(pVolLeft: Single;
                        pVolRight: Single): HResult;

    // EQ High / Mid / Low MFT implementation -----------------------------------

    procedure EnableEQ(const AEnabled: Boolean);   // Runtime on/off.

    procedure SetLowDb(const ALowDb: Integer);     // -24..+24
    procedure SetMidDb(const AMidDb: Integer);     // -24..+24
    procedure SetHighDb(const AHighDb: Integer);   // -24..+24

    procedure SetMidMode(const AMidMode: TMfMidMode);
    procedure SetMidQ(const AMidQ: Single);

    procedure SetLowFreqHz(const Hz: Single);
    procedure SetMidFreqHz(const Hz: Single);
    procedure SetHighFreqHz(const Hz: Single);

    procedure SetLowShelfSlope(const S: Single);
    procedure SetHighShelfSlope(const S: Single);

    procedure SetRampMode(const Mode: TMfRampMode); // Off/Fast/Smooth/Manual.
    procedure SetRampTimeMs(const Ms: Integer);     // only for Manual.

    function GetEqBiquadCoeffs(out Low,
                               Mid,
                               High: TBiquadCoeffs;
                               out SampleRate: Double): Boolean;

    procedure ApplyEqTuningImmediate(const T: TEqTuning);

    // -------------------------------------------------------------------------

    property DeviceState: TDeviceState read pvDeviceState;
    property SoundChannels: Word read pvSoundChannels;
    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnReady: TWasApiReadyEvent read FOnReady write FOnReady;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
    property OnEnded: TWasApiEndedEvent read FOnEnded write FOnEnded;
  end;


implementation

uses
  System.Math;


{ TEngineCommand }

class function TEngineCommand.LoadFile(const AFileName: string;
                                       aDuration: Int64): TEngineCommand;
begin

  // NOTE: Delphi does not guarantee record return values are zeroed.
  // If we only set kind, other fields can contain garbage/stale values.
  // So, we always zero-initialize factory results.
  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckLoadFile;
  Result.FileName := AFileName;
  Result.FileDuration := ADuration;
  Result.VolL := 0;
  Result.VolR := 0;
end;


class function TEngineCommand.Play(): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckPlay;
end;


class function TEngineCommand.Pause(): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckPause;
end;


class function TEngineCommand.Stop(): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckStop;
end;


class function TEngineCommand.Seek(const APos100ns: Int64): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckSeek;
  Result.SeekPos100ns := APos100ns;
end;


class function TEngineCommand.SetVolume(aLeft,
                                        aRight: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckSetVolume;
  Result.VolL := ALeft;
  Result.VolR := ARight;
end;


// EQ methods ------------------------------------------------------------------
class function TEngineCommand.EQEnable(const AEnabled: Boolean): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQEnable;
  Result.EqEnabled := AEnabled;
end;


class function TEngineCommand.EQSetLowDb(const ALowDb: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetLowDb;
  Result.EqLowDb := ALowDb;
end;


class function TEngineCommand.EQSetMidDb(const AMidDb: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetMidDb;
  Result.EqMidDb := AMidDb;
end;


class function TEngineCommand.EQSetHighDb(const AHighDb: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetHighDb;
  Result.EqHighDb := AHighDb;
end;


class function TEngineCommand.EQSetMidMode(const AMidMode: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetMidMode;
  Result.EqMidMode := AMidMode;
end;


class function TEngineCommand.EQSetMidQ(const AMidQ: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetMidQ;
  Result.EqMidQ := AMidQ;
end;


class function TEngineCommand.EQSetLowFreqHz(const Hz: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetLowFreqHz;
  Result.EqLowFreqHz := Hz;
end;


class function TEngineCommand.EQSetMidFreqHz(const Hz: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetMidFreqHz;
  Result.EqMidFreqHz := Hz;
end;


class function TEngineCommand.EQSetHighFreqHz(const Hz: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetHighFreqHz;
  Result.EqHighFreqHz := Hz;
end;


class function TEngineCommand.EQSetLowShelfSlope(const S: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetLowShelfSlope;
  Result.EqLowShelfSlope := S;
end;


class function TEngineCommand.EQSetHighShelfSlope(const S: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetHighShelfSlope;
  Result.EqHighShelfSlope := S;
end;


class function TEngineCommand.EQSetRampMode(const ARampMode: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetRampMode;
  Result.EqRampMode := ARampMode;
end;


class function TEngineCommand.EQSetRampTimeMs(const AMs: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);
  Result.Kind := ckEQSetRampTimeMs;
  Result.EqRampTimeMs := AMs;
end;


// -----------------------------------------------------------------------------

class function TEngineCommand.Shutdown(): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckShutdown;
end;


{ TWasApiEngineThread }

constructor TWasApiEngineThread.Create(AEngine: TWasApiEngine);
begin
  inherited Create(False);

  FreeOnTerminate := False;
  FEngine := AEngine;
end;


procedure TWasApiEngineThread.Execute();
var
  hr: HRESULT;
  Cmd: TEngineCommand;
  waitArray: array[0..1] of THandle;

begin

  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);

  try

    // This is done in Mainfrm in initialisation section, we could do it here,
    // but, remove the declaration from Mainfrm / initialisation section.
    //hr := MFStartup(MF_VERSION,
    //                MFSTARTUP_FULL);
    //
    // mfStarted := SUCCEEDED(hr);

    waitArray[0] := FEngine.FTerminateEvent;
    waitArray[1] := FEngine.FCmdEvent;

    // Idle loop: wait for commands or terminate.
    while not Terminated do
      begin

        case WaitForMultipleObjects(2,
                                    @waitArray,
                                    False,
                                    INFINITE) of
          WAIT_OBJECT_0: // terminate
            Break;

          WAIT_OBJECT_0 + 1: // command
            begin

              // drain queue
              while FEngine.DequeueCommand(Cmd) do
                begin

                  if (Cmd.Kind = ckShutdown) then
                    begin
                      Terminate();
                      Break;
                    end;

                  if (Cmd.Kind = ckPlay) then
                    begin

                      FEngine.FRequestStop := False;
                      FEngine.FRequestPause := False;

                      hr := FEngine.PlayAudioStreamInternal();
                      if FAILED(hr) then
                        begin

                          FEngine.SetState(dsError);
                          FEngine.RaiseError('PlayAudioStream failed', hr);
                        end;
                    end
                  else
                    FEngine.ProcessControlCommand(Cmd);
                end;

            end;
        end;
      end;
  finally

    //if mfStarted then  << See MainFrm finalization section.
    //  MFShutdown;
    CoUninitialize();
  end;
end;


{ TWasApiEngine }

constructor TWasApiEngine.Create();
begin
  inherited Create;

  pvAudioClient := nil;
  pvAudioStreamVolume := nil;
  pvRenderClient := nil;
  pvAudioClock := nil;

  pvBytes := nil;
  pvBytesLength := 0;
  pvSourceWfx := nil;
  pvwaveformatlength := 0;

  pvDeviceState := dsUninitialized;
  pvErrStatus := S_OK;

  pvMmcssHandle := 0;
  pvMmcssTaskIndex := 0;

  pvAudioSamplesReadyEvent := 0;

  FOffset := 0;
  FBasePos100ns := 0;
  FBytesPerSample := 0;
  pvSoundChannels := 0;

  FCmdCS := TCriticalSection.Create;
  FCmdQueue := TQueue<TEngineCommand>.Create;

  // auto-reset for commands
  FCmdEvent := CreateEvent(nil,
                           False,
                           False,
                           nil);
  // manual-reset terminate
  FTerminateEvent := CreateEvent(nil,
                                 True,
                                 False,
                                 nil);

  // Start worker thread immediately (engine owner)
  FThread := TWasApiEngineThread.Create(Self);
  SetState(dsInitialized);
end;


destructor TWasApiEngine.Destroy();
begin

  // Request thread shutdown
  EnqueueCommand(TEngineCommand.Shutdown);
  SetEvent(FCmdEvent);
  SetEvent(FTerminateEvent);

  if Assigned(FThread) then
    begin

      FThread.WaitFor;
      FreeAndNil(FThread);
    end;

  ResetAudioData(True);

  if (pvAudioSamplesReadyEvent <> 0) then
     begin

      CloseHandle(pvAudioSamplesReadyEvent);
      pvAudioSamplesReadyEvent := 0;
    end;

  if (FCmdEvent <> 0) then
    CloseHandle(FCmdEvent);

  if (FTerminateEvent <> 0) then
    CloseHandle(FTerminateEvent);

  FreeAndNil(FCmdQueue);
  FreeAndNil(FCmdCS);

  if Assigned(pvSourceWfx) then
    CoTaskMemFree(pvSourceWfx);

  if Assigned(pvRenderWfx) then
    begin
      CoTaskMemFree(pvRenderWfx);
      pvRenderWfx := nil;
    end;

  inherited;
end;


procedure TWasApiEngine.SetState(const NewState: TDeviceState);
begin

  pvDeviceState := NewState;
  if Assigned(FOnStateChanged) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnStateChanged) then
                      FOnStateChanged(Self, NewState);
                  end);
end;


procedure TWasApiEngine.RaiseError(const Msg: string;
                                   const Hr: HRESULT);
begin

  pvErrStatus := Hr;
  if Assigned(FOnError) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnError) then
                      FOnError(Self,
                               Msg,
                               Hr);
                  end);
end;


procedure TWasApiEngine.RaiseReady();
begin

  if Assigned(FOnReady) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnReady) then
                      FOnReady(Self);
                  end);
end;


procedure TWasApiEngine.RaiseProcessed(const Position100ns: Int64;
                                       const RawPosition: UInt64);
begin

  if Assigned(FOnProcessed) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnProcessed) then
                      FOnProcessed(Self, Position100ns, RawPosition);
                  end);
end;


procedure TWasApiEngine.RaiseEnded();
begin
  if Assigned(FOnEnded) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnEnded) then
                      FOnEnded(Self);
                  end);
end;


procedure TWasApiEngine.EnqueueCommand(const Cmd: TEngineCommand);
begin

  FCmdCS.Enter();

  try

    FCmdQueue.Enqueue(Cmd);
    SetEvent(FCmdEvent);
  finally

    FCmdCS.Leave();
  end;
end;


function TWasApiEngine.DequeueCommand(out Cmd: TEngineCommand): Boolean;
begin

  Result := False;

  FCmdCS.Enter();

  try
    if (FCmdQueue.Count > 0) then
      begin
        Cmd := FCmdQueue.Dequeue;
        Result := True;
      end;
  finally

    FCmdCS.Leave();
  end;
end;


procedure TWasApiEngine.ProcessControlCommand(const Cmd: TEngineCommand);
var
  hr: HRESULT;
  pos100ns,
  newOffset: Int64;

begin

  case Cmd.Kind of
    ckLoadFile:
      begin

        hr := LoadFileInternal(Cmd.FileName,
                               Cmd.FileDuration);

        if SUCCEEDED(hr) then
          begin

            SetState(dsReady);
            RaiseReady();
          end
        else
          begin

            SetState(dsError);
            RaiseError('LoadFile failed', hr);
          end;
      end;

    ckPause:
      begin

        // Signal play loop to pause quickly
        FRequestPause := True;
      end;

    ckStop:
      begin

        // Signal play loop to stop quickly
        FRequestStop := True;
        FRequestPause := False;
      end;

ckSeek:
  begin
    if (pvSourceWfx <> nil) and
       (pvBytes <> nil) and
       (pvBytesLength > 0) and
       (pvSourceWfx.nAvgBytesPerSec <> 0) and
       (pvSourceWfx.nBlockAlign <> 0) then
    begin

      // DEBUG:
      //OutputDebugString(PChar(Format('ckSeek: Cmd.SeekPos100ns=%d  Dur=%d',
      //                               [Cmd.SeekPos100ns, FDuration100ns])));

      pos100ns := Cmd.SeekPos100ns;

      if (pos100ns < 0) then
        pos100ns := 0;

      if (FDuration100ns > 0) and (pos100ns > FDuration100ns) then
        pos100ns := FDuration100ns;

      // IMPORTANT: 100ns -> bytes using SOURCE buffer format (pvBytes layout)
      // Use source bytes/sec to avoid mismatch with WASAPI mix/render format.
      newOffset := (UInt64(pos100ns) * UInt64(pvSourceWfx.nAvgBytesPerSec)) div UInt64(REFTIMES_PER_SEC);

      // align to SOURCE block
      newOffset := (newOffset div Int64(pvSourceWfx.nBlockAlign)) * Int64(pvSourceWfx.nBlockAlign);

      // never seek to exact EOF (must be < pvBytesLength)
      if (pvBytesLength > 0) and (newOffset >= UInt64(pvBytesLength)) then
      begin
        if (pvBytesLength > UInt32(pvSourceWfx.nBlockAlign)) then
          newOffset := UInt64(pvBytesLength) - UInt64(pvSourceWfx.nBlockAlign)
        else
          newOffset := 0;
      end;

      // THIS is what makes FBasePos100ns meaningful:
      FBasePos100ns := pos100ns;
      FOffset := UInt32(newOffset);

      // Restart clock position from zero at the new base.
      if Assigned(pvAudioClient) then
      begin
        hr := pvAudioClient.Stop();
        if SUCCEEDED(hr) then
          hr := pvAudioClient.Reset();

        if FAILED(hr) then
        begin
          Self.SetState(dsError);
          Self.RaiseError('AudioClient Stop/Reset failed', hr);
          Exit;
        end;
      end;

      // Flush EQ state on seek (recommended; avoids ringing/history)
      if Assigned(FHighMidLowMFT) then
      begin
        hr := FHighMidLowMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);
        if FAILED(hr) then
        begin
          Self.SetState(dsError);
          Self.RaiseError('ProcessMessage MFT_MESSAGE_COMMAND_FLUSH failed', hr);
          Exit;
        end;
      end;

      // Seek should resume playback unless you explicitly want a "scrub while paused" mode.
      if Assigned(pvAudioClient) then
      begin
        if (pvDeviceState <> dsPlay) then
          SetState(dsPlay);

        hr := pvAudioClient.Start();
        if FAILED(hr) then
        begin
          Self.SetState(dsError);
          Self.RaiseError('AudioClient Start failed after seek', hr);
          Exit;
        end;
      end;
    end;
  end;


    ckSetVolume:
      begin

        if Assigned(pvAudioStreamVolume) then
          begin

            // channel volumes applied in play thread too; safe here on engine thread
            SetVolumes(Cmd.VolL,
                       Cmd.VolR);
          end;
      end;

    // =========================================================================
    // EQ MFT (optional / pluggable) - all are safe no-ops when not plugged.
    // =========================================================================

    ckEQEnable:
      begin

        FEQEnabled := Cmd.EqEnabled;

        // When disabling EQ while playing, flush to drop history.
        if (not FEQEnabled) and Assigned(FHighMidLowMFT) then
          FHighMidLowMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                        0);
      end;

    ckEQSetLowDb:
      begin

        if Assigned(FHighMidLowCtrl) then
          begin

            FHighMidLowCtrl.SetLowDb(Cmd.EqLowDb);
          end;
      end;

    ckEQSetMidDb:
      begin

        if Assigned(FHighMidLowCtrl) then
          begin

            FHighMidLowCtrl.SetMidDb(Cmd.EqMidDb);
          end;
      end;

    ckEQSetHighDb:
      begin

        if Assigned(FHighMidLowCtrl) then
          begin

            FHighMidLowCtrl.SetHighDb(Cmd.EqHighDb);
          end;
      end;

    ckEQSetMidMode:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetMidMode(TMfMidMode(Cmd.EqMidMode));
      end;

    ckEQSetMidQ:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetMidQ(Cmd.EqMidQ);
      end;

    ckEQSetLowFreqHz:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetLowFreqHz(Cmd.EqLowFreqHz);
      end;

    ckEQSetMidFreqHz:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetMidFreqHz(Cmd.EqMidFreqHz);
      end;

    ckEQSetHighFreqHz:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetHighFreqHz(Cmd.EqHighFreqHz);
      end;

    ckEQSetLowShelfSlope:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetLowShelfSlope(Cmd.EqLowShelfSlope);
      end;

    ckEQSetHighShelfSlope:
      begin
        if Assigned(FHighMidLowCtrl) then
          FHighMidLowCtrl.SetHighShelfSlope(Cmd.EqHighShelfSlope);
      end;

    ckEQSetRampMode:
      begin

        if Assigned(FHighMidLowCtrl) then
          begin

            // EqRampMode is Ord(TMfRampMode)
            FHighMidLowCtrl.SetRampMode(TMfRampMode(Cmd.EqRampMode));
          end;
      end;

    ckEQSetRampTimeMs:
      begin

        if Assigned(FHighMidLowCtrl) then
          begin

            FHighMidLowCtrl.SetRampTimeMs(Cmd.EqRampTimeMs);
          end;
      end;
  end;

end;


function TWasApiEngine.OpenFile(const audiofile: TFileName;
                                fileDuration100ns: LONGLONG): HRESULT;
begin

  EnqueueCommand(TEngineCommand.LoadFile(audiofile,
                                         fileDuration100ns));
  Result := S_OK;
end;


function TWasApiEngine.Start(): HResult;
begin

  EnqueueCommand(TEngineCommand.Play);
  Result := S_OK;
end;


function TWasApiEngine.Stop(): HResult;
begin

  EnqueueCommand(TEngineCommand.Stop);
  Result := S_OK;
end;


function TWasApiEngine.Pause(): HResult;
begin

  EnqueueCommand(TEngineCommand.Pause);
  Result := S_OK;
end;


function TWasApiEngine.SeekTo(const Pos100ns: Int64): HRESULT;
begin

  // Seek is valid only after a file is loaded into pvBytes.
  if (pvBytes = nil) or
     (pvBytesLength = 0) or
     (pvSourceWfx = nil) then
    Exit(E_FAIL);

  EnqueueCommand(TEngineCommand.Seek(Pos100ns));
  Result := S_OK;
end;


function TWasApiEngine.SetVolumes(pVolLeft,
                                  pVolRight: Single): HResult;
begin

  // This is expected to run on the engine thread.
  if not Assigned(pvAudioStreamVolume) then
    Exit(E_POINTER);

  if (pvSoundChannels >= 1) then
    pvAudioStreamVolume.SetChannelVolume(0,
                                         pVolLeft);

  if (pvSoundChannels >= 2) then
    pvAudioStreamVolume.SetChannelVolume(1,
                                         pVolRight);

  Result := S_OK;
end;


// EQ bass/ treble MFT implementation ------------------------------------------


procedure TWasApiEngine.EnableEQ(const AEnabled: Boolean);
begin

  EnqueueCommand(TEngineCommand.EQEnable(AEnabled));
end;


procedure TWasApiEngine.SetLowDb(const ALowDb: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetLowDb(ALowDb));
end;


procedure TWasApiEngine.SetMidDb(const AMidDb: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetMidDb(AMidDb));
end;


procedure TWasApiEngine.SetMidMode(const AMidMode: TMfMidMode);
begin

  EnqueueCommand(TEngineCommand.EQSetMidMode(Ord(AMidMode)));
end;


procedure TWasApiEngine.SetMidQ(const AMidQ: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetMidQ(AMidQ));
end;


procedure TWasApiEngine.SetLowFreqHz(const Hz: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetLowFreqHz(Hz));
end;


procedure TWasApiEngine.SetMidFreqHz(const Hz: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetMidFreqHz(Hz));
end;


procedure TWasApiEngine.SetHighFreqHz(const Hz: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetHighFreqHz(Hz));
end;


procedure TWasApiEngine.SetLowShelfSlope(const S: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetLowShelfSlope(S));
end;


procedure TWasApiEngine.SetHighShelfSlope(const S: Single);
begin

  EnqueueCommand(TEngineCommand.EQSetHighShelfSlope(S));
end;


procedure TWasApiEngine.SetHighDb(const AHighDb: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetHighDb(AHighDb));
end;


procedure TWasApiEngine.SetRampMode(const Mode: TMfRampMode);
begin

  // Mode is our MfAudioHighMidLowTypes ramp enum (Off/Fast/Smooth/Manual)
  EnqueueCommand(TEngineCommand.EQSetRampMode(Ord(Mode)));
end;


procedure TWasApiEngine.SetRampTimeMs(const Ms: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetRampTimeMs(Ms));
end;


function TWasApiEngine.GetEqBiquadCoeffs(out Low,
                                         Mid,
                                         High: TBiquadCoeffs;
                                         out SampleRate: Double): Boolean;
var
  hr: HRESULT;

begin

  SampleRate := 0;

  FillChar(Low,
           SizeOf(Low),
           0);

  FillChar(Mid,
           SizeOf(Mid),
           0);

  FillChar(High,
           SizeOf(High),
           0);

  if not Assigned(FHighMidLowCtrl) then
    Exit(False);

  hr := FHighMidLowCtrl.GetBiquadCoeffs(Low,
                                        Mid,
                                        High,
                                        SampleRate);

  Result := SUCCEEDED(hr) and (SampleRate > 0);
end;


procedure TWasApiEngine.ApplyEqTuningImmediate(const T: TEqTuning);
begin

  if Assigned(FHighMidLowCtrl) then
    begin

      FHighMidLowCtrl.SetLowDb(T.LowDb);
      FHighMidLowCtrl.SetMidDb(T.MidDb);
      FHighMidLowCtrl.SetHighDb(T.HighDb);

      FHighMidLowCtrl.SetLowFreqHz(T.LowFreqHz);
      FHighMidLowCtrl.SetMidFreqHz(T.MidFreqHz);
      FHighMidLowCtrl.SetHighFreqHz(T.HighFreqHz);

      FHighMidLowCtrl.SetMidQ(T.MidQ);
      FHighMidLowCtrl.SetMidMode(T.MidMode);
      FHighMidLowCtrl.SetLowShelfSlope(T.LowShelfSlope);
      FHighMidLowCtrl.SetHighShelfSlope(T.HighShelfSlope);
    end;
end;


// End EQ bass/ treble MFT implementation --------------------------------------



procedure TWasApiEngine.ResetAudioData(pFreeSourceStream: Boolean);
begin

  if pFreeSourceStream and (pvBytes <> nil) then
    begin

      FreeMem(pvBytes);
      pvBytes := nil;
      pvBytesLength := 0;
      FOffset := 0;
    end;

  if Assigned(FDynFloatBuf) then
    begin

      FreeMem(FDynFloatBuf);
      FDynFloatBuf := nil;
      FDynFloatBufSamples := 0;
    end;
end;


function TWasApiEngine.LoadData(bufferFrameCount: UINT32;
                                pData: PByte;
                                var flags: DWORD): HRESULT;
var
  bytesToCopy: UInt32;
  bytesRequested64: UInt64;
  bytesRequested: UInt32;
  remainBytes: UInt32;

begin

  flags := 0;

  if (pData = nil) then
    Exit(E_POINTER);

  if (pvBytes = nil) or (pvBytesLength = 0) then
    Exit(E_FAIL);

  if (FClientBlockAlign = 0) then
    Exit(E_FAIL);

  // Harden against overflow: compute requested bytes in 64-bit.
  bytesRequested64 := UInt64(bufferFrameCount) * UInt64(FClientBlockAlign);

  // Clamp to 32-bit addressable buffer size (pData points to WASAPI buffer of that size).
  if (bytesRequested64 > High(UInt32)) then
    bytesRequested := High(UInt32)
  else
    bytesRequested := UInt32(bytesRequested64);

  bytesToCopy := bytesRequested;

  // Already at EOF: produce silence but DO NOT change FOffset.
  if (FOffset >= pvBytesLength) then
    begin

      flags := AUDCLNT_BUFFERFLAGS_SILENT;
      ZeroMemory(pData,
                 bytesRequested);
      Exit(S_OK);
    end;

  // Partial tail: copy remaining bytes, zero-fill rest.
  if (UInt64(FOffset) + UInt64(bytesToCopy)) > UInt64(pvBytesLength) then
    begin

      bytesToCopy := pvBytesLength - FOffset;
      flags := flags or AUDCLNT_BUFFERFLAGS_SILENT;
    end;

  if (bytesToCopy > 0) then
    Move((pvBytes + FOffset)^,
         pData^,
         bytesToCopy);

  // If partial, zero the rest of the buffer for safety.
  if (bytesToCopy < bytesRequested) then
    begin
      remainBytes := bytesRequested - bytesToCopy;
      ZeroMemory(PByte(NativeUInt(pData) + NativeUInt(bytesToCopy)),
                 remainBytes);
    end;

  Inc(FOffset,
      bytesToCopy);

  // DEBUG: print bufferFrameCount, not uninitialized 'remain'
  //OutputDebugString(PChar(Format(
  //  'After LoadData: flags=%x FOffset=%d bytesLen=%d frames=%u reqBytes=%u copyBytes=%u',
  //  [Cardinal(flags), FOffset, pvBytesLength, bufferFrameCount, bytesRequested, bytesToCopy]
  //)));

  Result := S_OK;
end;


function TWasApiEngine.LoadFileInternal(const audiofile: TFileName;
                                        fileDuration: LONGLONG): HResult;
var
  hr: HResult;
  eqHr: HRESULT;
  sourceReaderConfiguration: IMFAttributes;
  sourceReader: IMFSourceReader;
  nativeMediaType: IMFMediaType;
  majorType,
  subType: TGUID;
  currentMediaType: IMFMediaType;
  buffer: IMFMediaBuffer;
  sample: IMFSample;
  flags: DWORD;
  hres: HRESULT;
  audioData: PByte;
  audioDataLength: DWORD;

  nChannels,
  nSampleRate,
  nBits: UINT32;
  wantSubType: TGUID;

  function GetU32Attr(const mt: IMFMediaType;
                      const key: TGUID;
                      def: UINT32): UINT32;
  var
    r: HRESULT;
    v: UINT32;

  begin

    v := def;
    if (mt <> nil) then
      begin
        r := mt.GetUINT32(key, v);
        if FAILED(r) then
          v := def;
      end;
    Result := v;
  end;

  function TrySetReaderType(const aSubType: TGUID;
                            aBits: UINT32;
                            aSR: UINT32;
                            aCh: UINT32): HRESULT;
  var
    r: HRESULT;
    t: IMFMediaType;

  begin

    t := nil;
    r := MFCreateMediaType(t);

    if SUCCEEDED(r) then
      r := t.SetGUID(MF_MT_MAJOR_TYPE,
                     MFMediaType_Audio);

    if SUCCEEDED(r) then
      r := t.SetGUID(MF_MT_SUBTYPE,
                     aSubType);

    // These are optional for SourceReader, but help it match the input format.
    if SUCCEEDED(r) and (aCh <> 0) then
      t.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                  aCh);

    if SUCCEEDED(r) and (aSR <> 0) then
      t.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                  aSR);

    if SUCCEEDED(r) and (aBits <> 0) then
      t.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                  aBits);

    if SUCCEEDED(r) then
      r := sourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                            0,
                                            t);

    Result := r;
  end;

begin

  ResetAudioData(True);

  if Assigned(pvSourceWfx) then
    begin

      CoTaskMemFree(pvSourceWfx);
      pvSourceWfx := nil;
    end;

  hr := MFCreateAttributes(sourceReaderConfiguration,
                           1);
  if SUCCEEDED(hr) then
    hr := sourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                              1);

  if SUCCEEDED(hr) then
    hr := MFCreateSourceReaderFromURL(PWideChar(audiofile),
                                      sourceReaderConfiguration,
                                      sourceReader);
  if FAILED(hr) then
    Exit(hr);

  hr := sourceReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                        0,
                                        @nativeMediaType);
  if FAILED(hr) then
    Exit(hr);

  hr := nativeMediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                majorType);
  if SUCCEEDED(hr) then
    if not IsEqualGUID(MFMediaType_Audio,
                       majorType) then
      Exit(MF_E_INVALID_FILE_FORMAT);

  hr := nativeMediaType.GetGUID(MF_MT_SUBTYPE,
                                subType);
  if FAILED(hr) then
    Exit(hr);

  // Read native attributes (may be missing for some containers; defaults used then).
  nChannels := GetU32Attr(nativeMediaType,
                          MF_MT_AUDIO_NUM_CHANNELS,
                          0);

  nSampleRate := GetU32Attr(nativeMediaType,
                            MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            0);

  nBits := GetU32Attr(nativeMediaType,
                      MF_MT_AUDIO_BITS_PER_SAMPLE,
                      0);

  // ---------------------------------------------------------------------------
  // Request SourceReader output to match input format as closely as possible.
  // - If input is already PCM or Float: try to keep same subtype + bits + SR + CH.
  // - If compressed: decode to PCM with same SR/CH/BPS (if known), else fallback.
  // ---------------------------------------------------------------------------

  wantSubType := subType;

  if not (IsEqualGUID(MFAudioFormat_Float,
                      wantSubType) or
          IsEqualGUID(MFAudioFormat_PCM,
                      wantSubType)) then
    begin

      // Compressed -> decode to PCM
      wantSubType := MFAudioFormat_PCM;
      // If bits unknown, let MF decide; otherwise request the native bits
    end;

  // 1) Try "same as input" (or decoded PCM with native SR/CH/BPS if present)
  hr := TrySetReaderType(wantSubType,
                         nBits,
                         nSampleRate,
                         nChannels);

  // 2) If that fails, try Float32 (best for DSP)
  if FAILED(hr) then
    hr := TrySetReaderType(MFAudioFormat_Float,
                           32,
                           nSampleRate,
                           nChannels);

  // 3) If that fails, fall back to PCM16
  if FAILED(hr) then
    hr := TrySetReaderType(MFAudioFormat_PCM,
                           16,
                           nSampleRate,
                           nChannels);

  if FAILED(hr) then
    Exit(hr);

  // Get the actual current type the SourceReader will output
  hr := sourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                         @currentMediaType);
  if FAILED(hr) then
    Exit(hr);

  // Convert MF media type to WAVEFORMATEX (this defines our decoded bytes layout)
  hr := MFCreateWaveFormatExFromMFMediaType(currentMediaType,
                                            pvSourceWfx,
                                            pvwaveformatlength,
                                            MFWaveFormatExConvertFlag_ForceExtensible);
  if FAILED(hr) then
    Exit(hr);

  // Source layout for pvBytes and seeking math (MUST match pvSourceWfx)
  FSourceBytesPerSec := 0;
  FSourceBlockAlign := 0;

  if (pvSourceWfx <> nil) then
    begin
      FSourceBytesPerSec := pvSourceWfx.nAvgBytesPerSec;
      FSourceBlockAlign := pvSourceWfx.nBlockAlign;
    end;

  // Create device + audio client (Initialize occurs in SetFormat).
  hr := InitializeAudioEngine();
  if FAILED(hr) then
    Exit(hr);

  // Mix format (diagnostics only)
  if Assigned(pvRenderWfx) then
  begin
    CoTaskMemFree(pvRenderWfx);
    pvRenderWfx := nil;
  end;

  hr := pvAudioClient.GetMixFormat(pvRenderWfx);
  if FAILED(hr) then
    Exit(hr);

  // Configure WASAPI for decoded format (pvSourceWfx).
  hr := SetFormat(pvSourceWfx);
  if FAILED(hr) then
    Exit(hr);

  // ---------------------------------------------------------------------------
  // EQ MFT (optional)
  // ---------------------------------------------------------------------------
  eqHr := S_OK;

  if not Assigned(FHighMidLowMFT) then
    eqHr := CreateHighMidLowMFT(FHighMidLowMFT);

  FHighMidLowCtrl := nil;
  FEQTypeSet := False;

  if SUCCEEDED(eqHr) and Assigned(FHighMidLowMFT) then
    begin

      FHighMidLowMFT.QueryInterface(IMfHighMidLowControl,
                                    FHighMidLowCtrl);

      eqHr := FHighMidLowMFT.SetInputType(0,
                                          currentMediaType,
                                          0);
      if SUCCEEDED(eqHr) then
         eqHr := FHighMidLowMFT.SetOutputType(0,
                                             currentMediaType,
                                             0);

      if SUCCEEDED(eqHr) then
        begin

          FHighMidLowMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                        0);

          FHighMidLowMFT.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                        0);

          FHighMidLowMFT.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                        0);

          FEQTypeSet := True;
          FEQEnabled := True;
        end
      else
        begin

          FEQEnabled := False;
          FEQTypeSet := False;
        end;
    end
  else
    begin

      FEQEnabled := False;
      FEQTypeSet := False;
    end;

  // ---------------------------------------------------------------------------
  // Read all samples into pvBytes (decoded PCM bytes in pvSourceWfx layout)
  // ---------------------------------------------------------------------------
  pvBytesLength := 0;
  pvBytes := nil;
  FOffset := 0;
  FBasePos100ns := 0;

  while True do
    begin

      sample := nil;
      buffer := nil;
      flags := 0;

      hr := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                    0,
                                    nil,
                                    @flags,
                                    nil,
                                    @sample);
      if FAILED(hr) then
        Break;

      if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
        Break;

      if (sample = nil) then
        Continue;

      hres := sample.ConvertToContiguousBuffer(@buffer);
      if FAILED(hres) then
        begin

          hr := hres;
          Break;
        end;

      hres := buffer.Lock(audioData,
                          nil,
                          @audioDataLength);
      if FAILED(hres) then
        begin

          hr := hres;
          Break;
        end;

      try
        if (audioDataLength > 0) then
          begin
            ReallocMem(pvBytes,
                       pvBytesLength + audioDataLength);

            Move(audioData^,
                 (pvBytes + pvBytesLength)^,
                 audioDataLength);

            Inc(pvBytesLength,
                audioDataLength);
          end;
      finally

        buffer.Unlock();
      end;
  end;

  // Duration (100ns) based on decoded byte rate.
  FDuration100ns := 0;
  if (pvSourceWfx <> nil) and (pvSourceWfx.nAvgBytesPerSec <> 0) then
    FDuration100ns := Int64((UInt64(pvBytesLength) * UInt64(REFTIMES_PER_SEC)) div UInt64(pvSourceWfx.nAvgBytesPerSec));

  Result := hr;
end;


function TWasApiEngine.InitializeAudioEngine(): HRESULT;
var
  hr: HRESULT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  pasm: IAudioSessionManager2;
  psav: ISimpleAudioVolume;

begin

  ResetAudioData(False);

  // Create event.
  if (pvAudioSamplesReadyEvent = 0) then
    pvAudioSamplesReadyEvent := CreateEvent(nil,
                                            False,
                                            False,
                                            nil);

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    Exit(hr);

  hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                            eMultimedia,
                                            pDevice);
  if FAILED(hr) then
    Exit(hr);

  hr := pDevice.Activate(IID_IAudioClient,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pvAudioClient));
  if FAILED(hr) then
    Exit(hr);

  // Force the audio session to unmuted + unity gain.
  if Assigned(pvAudioClient) then
    begin

      hr := CreateAudioSessionManager2(@pasm);
      if SUCCEEDED(hr) then
        hr := pasm.GetSimpleAudioVolume(nil,
                                        0,
                                        psav);
      if SUCCEEDED(hr) then
        begin

          hr := psav.SetMute(False,
                             nil);

          if SUCCEEDED(hr) then
            hr := psav.SetMasterVolume(1.0,
                                       nil);
        end;
    end;

  // We init later in SetFormat because we need the final WAVEFORMATEX.
  Result := hr;
end;


function TWasApiEngine.SetFormat(pwfx: PWAVEFORMATEX): HRESULT;
var
  hr: HRESULT;
  hnsRequestedDuration: REFERENCE_TIME;
  bufferFrameCount: UINT32;
  ch: Integer;

begin

  if (pvAudioClient = nil) then
    Exit(E_POINTER);

  if (pwfx = nil) then
    Exit(E_POINTER);

  hnsRequestedDuration := REFTIMES_PER_SEC;

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                                 AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or
                                 AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
                                 hnsRequestedDuration,
                                 0,
                                 pwfx,
                                 nil);
  if FAILED(hr) then
    Exit(hr);

  FClientBlockAlign := pwfx.nBlockAlign;

  hr := pvAudioClient.SetEventHandle(pvAudioSamplesReadyEvent);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetBufferSize(bufferFrameCount);
  if FAILED(hr) then
    Exit(hr);

  // If you have this field, keep it in sync (used for safety bounds).
  pvBufferFrameCount := bufferFrameCount;

  // Service interfaces
  hr := pvAudioClient.GetService(IID_IAudioRenderClient,
                                 pvRenderClient);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_IAudioStreamVolume,
                                 pvAudioStreamVolume);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_IAudioClock,
                                 pvAudioClock);
  if FAILED(hr) then
    Exit(hr);

  pvSoundChannels := pwfx.nChannels;

  // Ensure audible output by default (unity gain).
  if Assigned(pvAudioStreamVolume) and (pvSoundChannels > 0) then
    begin
      for ch := 0 to Integer(pvSoundChannels) - 1 do
        pvAudioStreamVolume.SetChannelVolume(ch,
                                             1.0);
    end;

  case pwfx.wBitsPerSample of
    16: begin

          FSampleFormat := sfInt16;
          FBytesPerSample := 2;
        end;

    24: begin

          FSampleFormat := sfInt24;
          FBytesPerSample := 3;
        end;

    32: begin

          FSampleFormat := sfInt32;
          FBytesPerSample := 4;
        end;
      else
        begin

          FSampleFormat := sfInt16;
          FBytesPerSample := 2;
        end;
  end;

  Result := S_OK;
end;


function TWasApiEngine.PlayAudioStreamInternal(): HRESULT;
var
  hr: HRESULT;
  waitArray: array[0..2] of THandle;
  waitResult: DWord;
  // Audio clock
  u64Position: UINT64;
  u64QPCPosition: UINT64;
  u64Frequency: UINT64;
  numFramesAvailable: UINT32;
  numFramesPadding: UINT32;
  pBufferData: PByte;
  flags: DWORD;
  Cmd: TEngineCommand;
  bytesThisBuffer: DWORD;

begin

  if (pvAudioClient = nil) or
     (pvRenderClient = nil) or
     (pvAudioClock = nil) then
    Exit(E_POINTER);

  if (pvBytes = nil) or (pvBytesLength = 0) then
    Exit(E_FAIL);

  // Must have a valid client block align (set by SetFormat after Initialize).
  if (FClientBlockAlign = 0) then
    Exit(E_FAIL);

  // Become MMCSS.
  pvMmcssHandle := AvSetMmThreadCharacteristics(PWideChar('Audio'),
                                                @pvMmcssTaskIndex);

  // If we are resuming from Pause, keep the current byte offset.
  // Otherwise start from the beginning.
  if (pvDeviceState <> dsPause) then
    begin

      FOffset := 0;
      FBasePos100ns := 0;
    end;

  FRequestStop := False;
  FRequestPause := False;

  SetState(dsPlay);

  waitArray[0] := FTerminateEvent;          // Terminate engine thread.
  waitArray[1] := FCmdEvent;                // Control commands available.
  waitArray[2] := pvAudioSamplesReadyEvent; // Audio ready.

  hr := pvAudioClient.Start();
  if FAILED(hr) then
    begin

      RaiseError('IAudioClient.Start failed', hr);
      Exit(hr);
    end;

  // Cache frequency once. Position math uses: seconds = pos / freq
  hr := pvAudioClock.GetFrequency(u64Frequency);
  if FAILED(hr) then
    begin

       pvAudioClient.Stop();
       SetState(dsStop);
       RaiseError('IAudioClock.GetFrequency failed',
                  hr);
       Exit(hr);
    end;

  while (pvDeviceState = dsPlay) and (pvAudioClient <> nil) do
    begin

      waitResult := WaitForMultipleObjects(3,
                                           @waitArray[0],
                                           False,
                                           INFINITE);

      case waitResult of

        WAIT_OBJECT_0: // Terminate
          Break;

        WAIT_OBJECT_0 + 1: // Command event.
          begin
            // Drain all pending commands.
            while DequeueCommand(Cmd) do
              begin

                // Route *all* supported commands through the single implementation.
                ProcessControlCommand(Cmd);

                // Keep the hard shutdown now shortcut if you want.
                if (Cmd.Kind = ckShutdown) then
                  begin

                    SetEvent(FTerminateEvent);
                    Break;
                  end;
              end;

            if FRequestStop then
              begin

                // Hard stop: return to start position.
                FRequestStop := False;

                // Stop the client and reset its clock/buffer.
                pvAudioClient.Stop();
                pvAudioClient.Reset();

                // Reset playback position.
                FOffset := 0;
                FBasePos100ns := 0;

                // Tell GUI immediately.
                RaiseProcessed(0,
                               0);

                SetState(dsStop);
                Break;
              end;

            if FRequestPause then
              begin

                SetState(dsPause);
                Break;
              end;
          end;

        WAIT_OBJECT_0 + 2: // Audio ready.
          begin

            hr := pvAudioClient.GetCurrentPadding(numFramesPadding);
            if FAILED(hr) then
              Break;

            // Use cached total buffer frames (set in SetFormat via GetBufferSize)
            if (pvBufferFrameCount > numFramesPadding) then
              numFramesAvailable := pvBufferFrameCount - numFramesPadding
            else
              numFramesAvailable := 0;

            if (numFramesAvailable = 0) then
              Continue;

            hr := pvRenderClient.GetBuffer(numFramesAvailable,
                                           pBufferData);
            if FAILED(hr) then
              Break;

            flags := 0;

            hr := LoadData(numFramesAvailable,
                           pBufferData,
                           flags);

            if FAILED(hr) then
              begin
                pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                             AUDCLNT_BUFFERFLAGS_SILENT);
                Break;
              end;

            // Only process when buffer has real audio
            if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
              begin

                // --- EQ (optional) ---
                if FEQEnabled and Assigned(FHighMidLowMFT) and FEQTypeSet then
                  begin

                    bytesThisBuffer := DWORD(numFramesAvailable) * DWORD(FClientBlockAlign);
                    hr := ProcessEQBuffer(pBufferData,
                                          bytesThisBuffer);

                    if FAILED(hr) then
                      begin

                        // EQ failure must never stop audio.
                      end;

                  end;
              end;

            hr := pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                               flags);
            if FAILED(hr) then
              Break;

            // Progress
            if (u64Frequency <> 0) then
              begin

                hr := pvAudioClock.GetPosition(@u64Position,
                                               @u64QPCPosition);
                if SUCCEEDED(hr) then
                  RaiseProcessed(FBasePos100ns + Int64((UInt64(u64Position) * UInt64(REFTIMES_PER_SEC)) div UInt64(u64Frequency)),
                                 u64Position);
              end;

            // End reached?
            if (FOffset >= pvBytesLength) then
              begin

                SetState(dsStop);
                RaiseEnded();
                Break;
              end;
          end;
      else  // case
        begin

          hr := E_FAIL;
          Break;
        end;

      end; // Case waitResult.
    end;

  if (pvDeviceState = dsPause) then
    pvAudioClient.Stop()
  else
    begin

      // Stop the client and reset its clock/buffer. ---------------------------
      pvAudioClient.Stop();
      pvAudioClient.Reset();

      // Reset playback position.
      FOffset := 0;
      FBasePos100ns := 0;

      // Tell GUI immediately.
      RaiseProcessed(0,
                     0);

      SetState(dsStop);
      // -----------------------------------------------------------------------
    end;

  if (pvMmcssHandle <> 0) then
    begin

      AvRevertMmThreadCharacteristics(pvMmcssHandle);
      pvMmcssHandle := 0;
      pvMmcssTaskIndex := 0;
    end;

  Result := hr;
end;


// Helper
function TWasApiEngine.ProcessEQBuffer(pData: PByte;
                                       const ByteCount: DWORD): HRESULT;
var
  hr: HRESULT;
  inSample, outSample: IMFSample;
  inBuf, outBuf: IMFMediaBuffer;
  outData: MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  pIn, pOut: PByte;
  cbOut: DWORD;

begin

  if (not FEQEnabled) or (FHighMidLowMFT = nil) or (ByteCount = 0) then
    Exit(S_OK);

  // --- Create input sample ---
  hr := MFCreateSample(inSample);
  if FAILED(hr) then Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount, inBuf);
  if FAILED(hr) then Exit(hr);

  hr := inBuf.Lock(pIn, nil, nil);
  if FAILED(hr) then Exit(hr);
  try
    Move(pData^, pIn^, ByteCount);
  finally
    inBuf.Unlock;
  end;

  hr := inBuf.SetCurrentLength(ByteCount);
  if FAILED(hr) then Exit(hr);

  hr := inSample.AddBuffer(inBuf);
  if FAILED(hr) then Exit(hr);

  // --- Create output sample (MUST be provided!) ---
  hr := MFCreateSample(outSample);
  if FAILED(hr) then Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount, outBuf);
  if FAILED(hr) then Exit(hr);

  hr := outSample.AddBuffer(outBuf);
  if FAILED(hr) then Exit(hr);

  // --- Feed MFT ---
  hr := FHighMidLowMFT.ProcessInput(0, inSample, 0);

  // If MFT is full (has pending input), drain once and retry.
  if (hr = MF_E_NOTACCEPTING) then
    begin

      ZeroMemory(@outData,
                 SizeOf(outData));

      outData.pSample := outSample;
      status := 0;

      hr := FHighMidLowMFT.ProcessOutput(0,
                                         1,
                                         @outData,
                                         status);

      // If it still wants more input, that's fine (nothing to drain).
      if FAILED(hr) and (hr <> MF_E_TRANSFORM_NEED_MORE_INPUT) then
        Exit(hr);

      // retry feeding
      hr := FHighMidLowMFT.ProcessInput(0,
                                        inSample,
                                        0);
    end;

  if FAILED(hr) then
    Exit(hr);

  // --- Pull output for the input we just pushed ---
  ZeroMemory(@outData,
             SizeOf(outData));

  outData.pSample := outSample;
  status := 0;

  hr := FHighMidLowMFT.ProcessOutput(0,
                                     1,
                                     @outData,
                                     status);

  // Common ok condition for some MFTs; your base should not return this here,
  // but keep it harmless.
  if (hr = MF_E_TRANSFORM_NEED_MORE_INPUT) then
    Exit(S_OK);

  if FAILED(hr) then
    Exit(hr);

  // --- Copy processed data back ---
  hr := outBuf.Lock(pOut,
                    nil,
                    @cbOut);

  if FAILED(hr) then
    Exit(hr);

  try

    Move(pOut^,
         pData^,
         Min(cbOut,
             ByteCount));
  finally

    outBuf.Unlock();
  end;

  Result := S_OK;
end;

end.
