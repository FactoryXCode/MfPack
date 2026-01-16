// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
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
// Description: The main engine that acts as Chief In Command about everything in this sample.
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
// Remarks: Requires Windows 7 or higher.
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
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  WinApi.WinError,
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
  MfAudioBassTrebleMFT,
  MfAudioBassTrebleTypes;


const

  REFTIMES_PER_SEC = 10000000;

  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;


type

  TSampleType = (stInt16,
                 stInt24,
                 stInt32,
                 stFloat32);

  TDeviceState = (dsUninitialized,
                  dsError,
                  dsInitialized,
                  dsReady,      // file decoded and engine initialized
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

                    // MFT bass / treble.
                    ckEQEnable,
                    ckEQSetBassDb,
                    ckEQSetTrebleDb,
                    ckEQSetRampMode,
                    ckEQSetRampTimeMs);

  TEngineCommand = record
    Kind: TEngineCmdKind;
    FileName: string;
    FileDuration: Int64;
    VolL: Single;
    VolR: Single;
    SeekPos100ns: Int64;  // <<< added (used to set FBasePos100ns and compute FOffset)

    // EQ MFT (bass & treble)
    EqEnabled: Boolean;
    EqBassDb: Integer;
    EqTrebleDb: Integer;
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
    class function EQSetBassDb(const ABassDb: Integer): TEngineCommand; static;
    class function EQSetTrebleDb(const ATrebleDb: Integer): TEngineCommand; static;
    class function EQSetRampMode(const ARampMode: Integer): TEngineCommand; static; // Ord(TRampMode)
    class function EQSetRampTimeMs(const AMs: Integer): TEngineCommand; static;

    class function Shutdown(): TEngineCommand; static;
  end;


  // Forwarded
  TWasApiEngine = class;

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
    pvBytesLength: Cardinal;
    pvwaveformatlength: Cardinal;

    pvSourceWfx: PWAVEFORMATEX;
    pvRenderWfx: PWAVEFORMATEX;
    FClientBlockAlign: Word;

    // Playback
    FOffset: UINT32;
    FSampleType: TSampleType;
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
    FBassTrebleMFT: IMFTransform;
    FBassTrebleCtrl: IMfBassTrebleControl;
    FEQEnabled: Boolean;
    FEQTypeSet: Boolean;   // Media types applied to MFT.

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

    // EQ bass/ treble MFT implementation --------------------------------------

    procedure SetEQTransform(const AMFT: IMFTransform); // Plug/unplug MFT.
    procedure EnableEQ(const AEnabled: Boolean);        // Rruntime on/off.

    procedure SetBassDb(const ABassDb: Integer);     // -24..+24
    procedure SetTrebleDb(const ATrebleDb: Integer); // -24..+24

    procedure SetRampMode(const Mode: TMfRampMode); // Off/Fast/Smooth/Manual.
    procedure SetRampTimeMs(const Ms: Integer);     // Only for Manual.

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

  // IMPORTANT: Delphi does not guarantee record return values are zeroed.
  // If we only set Kind, other fields can contain garbage/stale values.
  // Always zero-initialize factory results.
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
  FillChar(Result, SizeOf(Result), 0);
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


// EQ methods
class function TEngineCommand.EQEnable(const AEnabled: Boolean): TEngineCommand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := ckEQEnable;
  Result.EqEnabled := AEnabled;
end;

class function TEngineCommand.EQSetBassDb(const ABassDb: Integer): TEngineCommand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := ckEQSetBassDb;
  Result.EqBassDb := ABassDb;
end;

class function TEngineCommand.EQSetTrebleDb(const ATrebleDb: Integer): TEngineCommand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := ckEQSetTrebleDb;
  Result.EqTrebleDb := ATrebleDb;
end;

class function TEngineCommand.EQSetRampMode(const ARampMode: Integer): TEngineCommand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := ckEQSetRampMode;
  Result.EqRampMode := ARampMode;
end;

class function TEngineCommand.EQSetRampTimeMs(const AMs: Integer): TEngineCommand;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Kind := ckEQSetRampTimeMs;
  Result.EqRampTimeMs := AMs;
end;



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
  // mfStarted: Boolean;
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


procedure TWasApiEngine.RaiseError(const Msg: string; const Hr: HRESULT);
begin

  pvErrStatus := Hr;
  if Assigned(FOnError) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnError) then
                      FOnError(Self, Msg, Hr);
                  end);
end;


procedure TWasApiEngine.RaiseReady;
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


procedure TWasApiEngine.RaiseEnded;
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

        if (pvRenderWfx <> nil) and
           (pvBytes <> nil) and
           (pvBytesLength > 0) and
           (pvRenderWfx.nAvgBytesPerSec <> 0) then
         begin

           pos100ns := Cmd.SeekPos100ns;

           if (pos100ns < 0) then
             pos100ns := 0;

           if (FDuration100ns > 0) and (pos100ns > FDuration100ns) then
             pos100ns := FDuration100ns;

           // 100ns -> bytes (render format rate)
           newOffset := (UInt64(pos100ns) * UInt64(pvRenderWfx.nAvgBytesPerSec)) div UInt64(REFTIMES_PER_SEC);

           // align to block
           if (pvRenderWfx.nBlockAlign <> 0) then
             newOffset := (newOffset div Int64(pvRenderWfx.nBlockAlign)) * Int64(pvRenderWfx.nBlockAlign);

           if (newOffset > UInt64(pvBytesLength)) then
             newOffset := UInt64(pvBytesLength);

           // THIS is what makes FBasePos100ns meaningful:
           FBasePos100ns := pos100ns;
           FOffset := UInt32(newOffset);

           // restart clock position from zero at the new base
           if Assigned(pvAudioClient) then
             begin

               pvAudioClient.Stop();
               pvAudioClient.Reset();
             end;

           // Flush EQ state on seek (recommended; avoids ringing/history)
           if Assigned(FBassTrebleMFT) then
             FBassTrebleMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);

           if (pvDeviceState = dsPlay) and Assigned(pvAudioClient) then
             pvAudioClient.Start;
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
        if (not FEQEnabled) and Assigned(FBassTrebleMFT) then
          FBassTrebleMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                        0);
      end;

    ckEQSetBassDb:
      begin

        if Assigned(FBassTrebleCtrl) then
          begin

            FBassTrebleCtrl.SetBassDb(Cmd.EqBassDb);
          end;
      end;

    ckEQSetTrebleDb:
      begin

        if Assigned(FBassTrebleCtrl) then
          begin

            FBassTrebleCtrl.SetTrebleDb(Cmd.EqTrebleDb);
          end;
      end;

    ckEQSetRampMode:
      begin

        if Assigned(FBassTrebleCtrl) then
          begin

            // EqRampMode is Ord(TMfRampMode)
            FBassTrebleCtrl.SetRampMode(TMfRampMode(Cmd.EqRampMode));
          end;
      end;

    ckEQSetRampTimeMs:
      begin

        if Assigned(FBassTrebleCtrl) then
          begin

            FBassTrebleCtrl.SetRampTimeMs(Cmd.EqRampTimeMs);
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

procedure TWasApiEngine.SetEQTransform(const AMFT: IMFTransform);
var
  hr: HResult;

begin

  FBassTrebleMFT := AMFT;
  FBassTrebleCtrl := nil;
  FEQTypeSet := False;

  if Assigned(FBassTrebleMFT) then
    begin
      hr := FBassTrebleMFT.QueryInterface(IMfBassTrebleControl,
                                          FBassTrebleCtrl);
      if FAILED(hr) then
        RaiseError('BassTrebleMFT.QueryInterface failed',
                   hr);
    end;
end;


procedure TWasApiEngine.EnableEQ(const AEnabled: Boolean);
begin

  EnqueueCommand(TEngineCommand.EQEnable(AEnabled));
end;


procedure TWasApiEngine.SetBassDb(const ABassDb: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetBassDb(ABassDb));
end;


procedure TWasApiEngine.SetTrebleDb(const ATrebleDb: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetTrebleDb(ATrebleDb));
end;


procedure TWasApiEngine.SetRampMode(const Mode: TMfRampMode);
begin

  // Mode is our MfAudioBassTrebleTypes ramp enum (Off/Fast/Smooth/Manual)
  EnqueueCommand(TEngineCommand.EQSetRampMode(Ord(Mode)));
end;


procedure TWasApiEngine.SetRampTimeMs(const Ms: Integer);
begin

  EnqueueCommand(TEngineCommand.EQSetRampTimeMs(Ms));
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
end;


function TWasApiEngine.LoadData(bufferFrameCount: UINT32;
                                pData: PByte;
                                var flags: DWORD): HRESULT;
var
  bytesToCopy: UINT32;
  bytesRequested: UINT32;
  remain: UINT32;

begin

  flags := 0;

  if (pData = nil) then
    Exit(E_POINTER);

  if (pvBytes = nil) or (pvBytesLength = 0) then
    Exit(E_FAIL);

  if (FClientBlockAlign = 0) then
    Exit(E_FAIL);

  bytesRequested := bufferFrameCount * UINT32(FClientBlockAlign);
  bytesToCopy := bytesRequested;

  if (FOffset >= pvBytesLength) then
    begin
      flags := AUDCLNT_BUFFERFLAGS_SILENT;
      ZeroMemory(pData,
                 bytesRequested);
      Exit(S_OK);
    end;

  if (FOffset + bytesToCopy) > pvBytesLength then
    begin
      bytesToCopy := pvBytesLength - FOffset;
      flags := AUDCLNT_BUFFERFLAGS_SILENT;
    end;

  if (bytesToCopy > 0) then
    Move((pvBytes + FOffset)^,
         pData^,
         bytesToCopy);

  // If partial, zero the rest of the buffer for safety.
  if (bytesToCopy < bytesRequested) then
    begin
      remain := bytesRequested - bytesToCopy;
      ZeroMemory(PByte(NativeUInt(pData) + NativeUInt(bytesToCopy)),
                 remain);
    end;

  Inc(FOffset,
      bytesToCopy);

  Result := S_OK;
end;


function TWasApiEngine.LoadFileInternal(const audiofile: TFileName;
                                        fileDuration: LONGLONG): HResult;
var
  hr: HResult;
  sourceReaderConfiguration: IMFAttributes;
  sourceReader: IMFSourceReader;
  nativeMediaType: IMFMediaType;
  partialType: IMFMediaType;
  majorType: TGUID;
  subType: TGUID;
  currentMediaType: IMFMediaType;
  buffer: IMFMediaBuffer;
  sample: IMFSample;
  flags: DWORD;
  hres: HRESULT;
  audioData: PByte;
  audioDataLength: DWORD;
  eqHr: HRESULT;

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
                                        nativeMediaType);
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

  // Force uncompressed PCM if needed.
  if not (IsEqualGUID(MFAudioFormat_Float,
                      subType) or
          IsEqualGUID(MFAudioFormat_PCM,
                      subType)) then
    begin

      hr := MFCreateMediaType(partialType);

      if SUCCEEDED(hr) then
        hr := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                  MFMediaType_Audio);

      if SUCCEEDED(hr) then
        hr := partialType.SetGUID(MF_MT_SUBTYPE,
                                  MFAudioFormat_PCM);

      if SUCCEEDED(hr) then
        hr := sourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                               0,
                                               partialType);
      if FAILED(hr) then
        Exit(hr);
    end;

  hr := sourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                         currentMediaType);
  if FAILED(hr) then
    Exit(hr);

  // Convert MF media type to WAVEFORMATEX (this defines our decoded PCM bytes layout).
  hr := MFCreateWaveFormatExFromMFMediaType(currentMediaType,
                                            pvSourceWfx,
                                            pvwaveformatlength,
                                            MFWaveFormatExConvertFlag_ForceExtensible);
  if FAILED(hr) then
    Exit(hr);

  // Create device + audio client (Initialize occurs in SetFormat).
  hr := InitializeAudioEngine();
  if FAILED(hr) then
    Exit(hr);

  // Mix format is not used for rendering bytes, but can be useful for diagnostics/UI.
  if Assigned(pvRenderWfx) then
    begin

      CoTaskMemFree(pvRenderWfx);
      pvRenderWfx := nil;
    end;

  hr := pvAudioClient.GetMixFormat(pvRenderWfx);
  if FAILED(hr) then
    Exit(hr);

  // Configure and init WASAPI for decoded PCM format.
  // NOTE: MUST match LoadData/byte layout -> pvSourceWfx.
  hr := SetFormat(pvSourceWfx);
  if FAILED(hr) then
    Exit(hr);

  // ---------------------------------------------------------------------------
  // Create + configure EQ MFT (optional / pluggable)
  // ---------------------------------------------------------------------------

  // Do not hard-fail file load if EQ cannot be created/configured.
  // Engine must work with or without EQ.
  eqHr := S_OK;

  // If GUI plugged a custom MFT via SetEQTransform, FBassTrebleMFT may already be set.
  if not Assigned(FBassTrebleMFT) then
    eqHr := CreateBassTrebleMFT(FBassTrebleMFT);

  FBassTrebleCtrl := nil;
  FEQTypeSet := False;

  if SUCCEEDED(eqHr) and Assigned(FBassTrebleMFT) then
    begin

      FBassTrebleMFT.QueryInterface(IMfBassTrebleControl,
                                    FBassTrebleCtrl);

      // Configure media types (same in/out).
      eqHr := FBassTrebleMFT.SetInputType(0,
                                          currentMediaType,
                                          0);
      if SUCCEEDED(eqHr) then
        eqHr := FBassTrebleMFT.SetOutputType(0,
                                             currentMediaType,
                                             0);

      if SUCCEEDED(eqHr) then
        begin

          FBassTrebleMFT.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                        0);
          FBassTrebleMFT.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                        0);
          FBassTrebleMFT.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                        0);

          FEQTypeSet := True;
          // Default on; GUI may disable at runtime.
          // (If you want default off, set FEQEnabled := False here.)
          FEQEnabled := True;
        end
      else
        begin
          // Disable EQ on failure (optional component)
          FEQEnabled := False;
          FEQTypeSet := False;
        end;
    end
  else
    begin
      // No EQ available -> pure bypass
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
                       pvBytesLength +
                       audioDataLength);

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

  // Duration (100ns) based on decoded PCM byte rate.
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

  // Create events
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
  isFloat: Boolean;
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

  // Float detection: handle WAVE_FORMAT_IEEE_FLOAT and WAVE_FORMAT_EXTENSIBLE/SubFormat.
  isFloat := False;

  if (pwfx.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) then
    isFloat := True
  else
    if (pwfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
      begin
        isFloat := IsEqualGUID(PWaveFormatExtensible(pwfx)^.SubFormat,
                               KSDATAFORMAT_SUBTYPE_IEEE_FLOAT);
      end;

  if isFloat then
    begin
      FSampleType := stFloat32;
      FBytesPerSample := 4;
    end
  else
    begin
      case pwfx.wBitsPerSample of
        16: begin
              FSampleType := stInt16;
              FBytesPerSample := 2;
            end;

        24: begin
              FSampleType := stInt24;
              FBytesPerSample := 3;
            end;

        32: begin
              FSampleType := stInt32;
              FBytesPerSample := 4;
            end;
      else
        begin
          FSampleType := stInt16;
          FBytesPerSample := 2;
        end;
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
  {$IFDEF DEBUG}
  dwCharacteristics: DWord;
  {$ENDIF}
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
       RaiseError('IAudioClock.GetFrequency failed', hr);
       Exit(hr);
    end;

  {$IFDEF DEBUG}
  hr := pvAudioClock.GetCharacteristics(dwCharacteristics);
  if SUCCEEDED(hr) then
    if (dwCharacteristics <> AUDIOCLOCK_CHARACTERISTIC_FIXED_FREQ) then
      OutputDebugString(PWideChar(Format('>>>> AudioClock dwCharacteristics = %d', [dwCharacteristics])));
  {$ENDIF}

  while (pvDeviceState = dsPlay) and (pvAudioClient <> nil) do
    begin

      waitResult := WaitForMultipleObjects(3,
                                           @waitArray[0],
                                           False,
                                           INFINITE);

      case waitResult of

        WAIT_OBJECT_0: // terminate
          Break;

        WAIT_OBJECT_0 + 1: // command event
          begin
            // Drain all pending commands.
            while DequeueCommand(Cmd) do
              begin

                {case Cmd.Kind of
                  ckPause: FRequestPause := True;

                  ckStop:  FRequestStop := True;

                  ckSeek:
                    begin
                      // Perform seek immediately on the audio/render thread.
                      DoSeekLocked(Cmd.SeekPos100ns);
                    end;

                  ckSetVolume: SetVolumes(Cmd.VolL,
                                          Cmd.VolR);

                  ckShutdown:
                    begin
                      SetEvent(FTerminateEvent);
                      Break;
                    end;
                end; // Case}


                // Route *all* supported commands through the single implementation
                ProcessControlCommand(Cmd);

                // Keep the hard “shutdown now” shortcut if you want
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

        WAIT_OBJECT_0 + 2: // audio ready
          begin

            hr := pvAudioClient.GetCurrentPadding(numFramesPadding);
            if FAILED(hr) then
              Break;

            hr := pvAudioClient.GetBufferSize(numFramesAvailable);
            if FAILED(hr) then
              Break;

            numFramesAvailable := numFramesAvailable - numFramesPadding;

            if (numFramesAvailable > 0) then
              begin

                hr := pvRenderClient.GetBuffer(numFramesAvailable,
                                               pBufferData);
                if FAILED(hr) then
                  Break;

                flags := 0;

                hr := LoadData(numFramesAvailable,
                               pBufferData,
                               flags);

                // After GetBuffer succeeds, ReleaseBuffer MUST be called with the same frame count.
                // If LoadData failed, release as SILENT for safety.
                if FAILED(hr) then
                  begin

                    pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                                 AUDCLNT_BUFFERFLAGS_SILENT);
                    Break;
                  end;

                // Apply EQ only when we are actually outputting audio (not silent)
                if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
                  begin

                    if FEQEnabled and Assigned(FBassTrebleMFT) and FEQTypeSet then
                      begin

                        // bytes in this render buffer:
                        // MUST use the block align of the format used by IAudioClient.Initialize.
                        bytesThisBuffer := DWORD(numFramesAvailable) * DWORD(FClientBlockAlign);

                        hr := ProcessEQBuffer(pBufferData,
                                              bytesThisBuffer);

                        // EQ failure must NEVER stop audio
                        if FAILED(hr) then
                          begin
                            // Optional: RaiseError('EQ failed', hr);
                          end;
                      end;
                  end;

                hr := pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                                   flags);
                if FAILED(hr) then
                  Break;
              end;

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

      else
        begin
          hr := E_FAIL;
          Break;
        end;

      end; // case waitResult
    end;

  pvAudioClient.Stop();

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
  hr: HResult;
  inSample: IMFSample;
  inBuf,
  outBuf: IMFMediaBuffer;
  outData: MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  pOut: PByte;
  cbOut: DWORD;

begin

  if (not FEQEnabled) or
     (FBassTrebleMFT = nil) or
     (ByteCount = 0) then
    Exit(S_OK);

  // --- Create input sample ---
  hr := MFCreateSample(inSample);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount,
                                 inBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := inBuf.Lock(pOut,
                   nil,
                   nil);
  if FAILED(hr) then
    Exit(hr);

  try

    Move(pData^,
         pOut^,
         ByteCount);

  finally

    inBuf.Unlock();
  end;

  inBuf.SetCurrentLength(ByteCount);
  inSample.AddBuffer(inBuf);

  // --- Feed MFT ---
  hr := FBassTrebleMFT.ProcessInput(0,
                                    inSample,
                                    0);
  if FAILED(hr) then
    Exit(hr);

  // --- Prepare output ---
  ZeroMemory(@outData,
             SizeOf(outData));
  outData.pSample := nil;

  hr := FBassTrebleMFT.ProcessOutput(0,
                                     1,
                                     @outData,
                                     status);
  if FAILED(hr) then
    Exit(hr);

  // --- Copy processed data back ---
  hr := outData.pSample.ConvertToContiguousBuffer(@outBuf);
  if FAILED(hr) then
    Exit(hr);

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

  Result := hr;
end;


end.
