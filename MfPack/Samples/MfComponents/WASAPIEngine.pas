// FactoryX
//
// Copyright: @ FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WASAPIEngine.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 4.0.0
// Description: The main engine that acts as Chief In Command about everything in this sample.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
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
  WinApi.CoreAudioApi.EndPointVolume,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {DEBUG}
  PcmLib;


const

  REFTIMES_PER_SEC = 10000000;
  HNS_PER_100MS    = 1000000;
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
  // Pitch/tempo
  TMfInterpQuality = (iqLinear,
                      iqCatmullRom);

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

  // Audio post-decode processing callback (engine thread).
  // Called after decoded PCM is written to the WASAPI render buffer,
  // and before IAudioRenderClient.ReleaseBuffer.
  // ByteCount is the number of valid bytes in pData.
  TWasApiProcessPcmEvent = procedure(Sender: TObject;
                                     pData: PByte;
                                     const ByteCount: DWORD;
                                     pwfx: PWAVEFORMATEX) of object;

  // Audio source-fill callback (engine thread).
  // When assigned, this callback is responsible for filling pData with PCM
  // and/or marking Flags as AUDCLNT_BUFFERFLAGS_SILENT.
  // This is used by the shared master renderer to pull audio from the internal mixer.
  TWasApiFillPcmEvent = function(Sender: TObject;
                                 pData: PByte;
                                 const ByteCount: DWORD;
                                 pwfx: PWAVEFORMATEX;
                                 out Flags: DWORD): HRESULT of object;

  TWasApiEndedEvent = procedure(Sender: TObject) of object;

  // Peakmeter data (push style).
  TOnOutputPcm = procedure(Sender: TObject;
                           pData: PByte;
                           const ByteCount: DWORD;
                           Wfx: PWAVEFORMATEX) of object;

  TEngineCmdKind = (ckLoadFile,
                    ckPlay,
                    ckPause,
                    ckStop,
                    ckSetVolume,
                    ckSetCueVolume,
                    ckSetCueMute,
                    ckSeek,
                    ckSwitchDevice,
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
    CueVol: Single;
    CueMuteActive: Boolean;
    SeekPos100ns: Int64;  // <<< added (used to set FBasePos100ns and compute FOffset)

    // Device switching
    DeviceId: string;
    UseDefaultDevice: Boolean;
    DeviceRole: Integer; // Ord(ERole)
    AutoResume: Boolean;

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


    class function SetCueVolume(const AVol: Single): TEngineCommand; static;
    class function SetCueMute(const AMute: Boolean): TEngineCommand; static;
    // Device switch
    class function SwitchDevice(const ADeviceId: string;
                                const AUseDefault: Boolean;
                                const ARole: Integer;
                                const AAutoResume: Boolean): TEngineCommand; static;

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
    pvSimpleVol: ISimpleAudioVolume; // optional: per-session master/mute
    pvCueSimpleVol: ISimpleAudioVolume; // optional: per-session cue volume/mute

    // Secondary (Cue/PFL) WASAPI
    pvCueAudioClient: IAudioClient;
    pvCueRenderClient: IAudioRenderClient;
    pvCueAudioSamplesReadyEvent: THandle;
    pvCueBufferFrameCount: UINT32;

    // Device selection / switching
    FUseDefaultDevice: Boolean;
    FDeviceRole: ERole;          // default endpoint role when UseDefaultDevice=True
    FDeviceId: string;           // IMMDevice ID when UseDefaultDevice = False
    FDeviceName: string;         // Readable name

    // Secondary (Cue/PFL) output selection
    FCueEnabled: Boolean;
    FCueUseDefaultDevice: Boolean;
    FCueDeviceRole: ERole;       // default endpoint role when CueUseDefaultDevice=True
    FCueDeviceId: string;        // IMMDevice ID when CueUseDefaultDevice=False
    FCueMuted: Boolean;
    FCueVolume: Single;

    FDeviceIndex: Integer;

    pvDeviceState: TDeviceState;

    // Decoded PCM bytes
    pvBytes: PByte;
    pvBytesLength: UINT64;
    pvwaveformatlength: UINT32;

    pvSourceWfx: PWAVEFORMATEX;
    pvRenderWfx: PWAVEFORMATEX;
    FClientBlockAlign: Word;

    // Playback
    FOffset: UINT64;
    FSampleType: TSampleType;
    FBytesPerSample: Integer;
    pvSoundChannels: WORD;
    FMixerSourceMode: Boolean;

    // Seek
    FBasePos100ns: Int64;
    FDuration100ns: Int64;

    pvErrStatus: HResult;

    // MMCSS.
    pvMmcssHandle: THandle;
    pvMmcssTaskIndex: DWord;

    // Events.
    pvAudioSamplesReadyEvent: THandle;
    FStoppedEvent: TEvent;  // manual reset
    FRunning: Integer;      // 0/1

    // Peakmeter coupling.
    FOnOutputPcm: TOnOutputPcm;

    // atomic storage (Single bits)
    FMeterPeakBitsL: LongInt;
    FMeterPeakBitsR: LongInt;
    FMeterRmsBitsL: LongInt;
    FMeterRmsBitsR: LongInt;
    // Calculated gains
    FMeterGainBitsL: LongInt;
    FMeterGainBitsR: LongInt;

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
    FOnFillPcm: TWasApiFillPcmEvent;
    FOnProcessPcm: TWasApiProcessPcmEvent;
    FOnEnded: TWasApiEndedEvent;

    pvBufferFrameCount: UINT32;

    // FX chain (generic IMFTransform list)
    FFxCS: TCriticalSection;
    FFx: TArray<IMFTransform>;
    FFxTypeSet: TArray<Boolean>;

    // Varispeed / pitch (varispeed, pitch changes with tempo) -----------------
    FVarispeedEnabled: LongBool;
    FRateTarget: Double;   // target speed factor (e.g. 0.84..1.16).
    FRateSmooth: Double;   // smoothed speed factor.
    FRampMs: Integer;      // smoothing time, 0 = off.
    FReadPos: Double;      // fractional read pos in *source frames*

    // Quality
    FInterpQuality: TMfInterpQuality;
    // Pitch/tempo control.
    FPitchRangePct: Double;     // e.g. 16.0 => -16..+16%
    FPitchDetentPct: Double;    // default 0.10  (deadzone around 0%).
    FPitchAutoZeroPct: Double;  // default 0.30  (snap-to-zero threshold).

    function RampCoeff(const RampMs,
                       SampleRate,
                       Frames: Integer): Double; inline;

    function LoadData_VarispeedFloat32(const Frames: Integer;
                                       const OutPtr: PSingle;
                                       out Flags: DWORD): HRESULT;
    // -------------------------------------------------------------------------

    // Peakmeter
    procedure MeterPublishFloat32(const p: PSingle;
                                  const Frames,
                                  Channels: Integer);
    // -------------------------------------------------------------------------

    procedure SetState(const NewState: TDeviceState);
    procedure RaiseError(const Msg: string; const Hr: HRESULT);
    procedure RaiseReady();
    procedure RaiseProcessed(const Position100ns: Int64; const RawPosition: UInt64);
    procedure RaiseEnded();

    procedure EnqueueCommand(const Cmd: TEngineCommand);
    function DequeueCommand(out Cmd: TEngineCommand): Boolean;

    function GetDeviceIdByIndex(const AIndex: Integer;
                                out ADeviceId: string): HRESULT;
    function InitializeAudioEngine(): HRESULT;
    function SetFormat(pwfx: PWAVEFORMATEX): HRESULT;


    function InitializeCueAudioEngine(): HRESULT;
    function SetCueFormat(pwfx: PWAVEFORMATEX): HRESULT;
    procedure ResetAudioData(pFreeSourceStream: Boolean);

    // Helper for LoadFileInternal.
    function ConvertPvBytesToFloat32InPlace(): HRESULT;
    function LoadFileInternal(const audiofile: TFileName;
                              fileDuration: LONGLONG): HResult;
    function LoadData(const Frames: UINT32;
                      const pBufferData: PByte;
                      var Flags: DWORD): HRESULT;

    function PlayAudioStreamInternal(): HRESULT;

    procedure ProcessControlCommand(const Cmd: TEngineCommand);

    // FX helpers.
    function CreateAudioMediaTypeFromWfx(pwfx: PWAVEFORMATEX;
                                         out M: IMFMediaType): HRESULT;

    function EnsureFxTypesSetLocked(): HRESULT;

    // FX Core -----------------------------------------------------------------
    function ProcessMftBuffer(const AMft: IMFTransform;
                              pData: PByte;
                              const ByteCount: DWORD): HRESULT;

    function ProcessEffectsBuffer(pData: PByte;
                                  const ByteCount: DWORD): HRESULT;
    // -------------------------------------------------------------------------

  public

    constructor Create();
    destructor Destroy(); override;

    // Commands (threaded)
    function OpenFile(const audiofile: TFileName;
                      fileDuration100ns: LONGLONG): HRESULT;
    function Start(): HResult;
    function StartSourceMode(): HRESULT;
    function Stop(): HResult;
    function WaitForStop(TimeoutMs: DWORD = 2000): HRESULT;
    function Pause(): HResult;
    function SeekTo(const Pos100ns: Int64): HRESULT;


    // Output device switching -------------------------------------------------
    // If AUseDefaultDevice=True, engine will use the current default render endpoint for ARole.
    // If False, it will use the IMMDevice with ADeviceId.
    // Note: switching is performed on the engine thread. If currently playing, it will stop,
    // reinitialize the audio client, and optionally resume.
    function SwitchOutputDevice(const ADeviceId: string;
                                const AUseDefaultDevice: Boolean = True;
                                const ARole: ERole = eMultimedia;
                                const AutoResume: Boolean = True): HRESULT;
    procedure SetUseDefaultOutputDevice(const ARole: ERole = eMultimedia);
    procedure SetOutputDeviceId(const ADeviceId: string);

    function ReadOutputPcmFloat32(const Frames: Integer;
                                  const OutBuffer: PSingle;
                                  out Flags: DWORD): HRESULT;

    function Mute(pActive: Boolean): Boolean;
    function CueMute(pActive: Boolean): Boolean;

    function SetCueVolumeAsync(const VolScalar: Single): HRESULT;
    function SetVolumesAsync(const VolLeft,
                         VolRight: Single): HRESULT;
    function SetVolumes(pVolLeft: Single;
                        pVolRight: Single): HResult;

    // Calculated gains for the post fader VU meters.
    procedure SetMeterFaderGains(const GainL,
                                 GainR: Single);
    procedure GetMeterFaderGains(out GainL,
                                 GainR: Single);

    property MixerSourceMode: Boolean read FMixerSourceMode write FMixerSourceMode;

    // FX ----------------------------------------------------------------------
    procedure ClearEffects();
    procedure AddEffect(const Mft: IMFTransform);
    procedure SetEffects(const Effects: array of IMFTransform);

    // Pitching/tempo ----------------------------------------------------------
    procedure SetPitchPercent(const Pct: Double);
    procedure SetPitchSlider(const SliderPos: Integer);

    property PitchRangePct: Double read FPitchRangePct write FPitchRangePct; // set default 16.0
    property PitchDetentPct: Double read FPitchDetentPct write FPitchDetentPct;       // 0.10
    property PitchAutoZeroPct: Double read FPitchAutoZeroPct write FPitchAutoZeroPct; // 0.30
    property PitchRampMs: Integer read FRampMs write FRampMs;                // default 50
    property VarispeedEnabled: LongBool read FVarispeedEnabled write FVarispeedEnabled;
    // -------------------------------------------------------------------------

    property DeviceState: TDeviceState read pvDeviceState;
    property SoundChannels: Word read pvSoundChannels;

    // Audio Device ------------------------------------------------------------
    property UseDefaultDevice: Boolean read FUseDefaultDevice write FUseDefaultDevice default True;
    property DeviceRole: ERole read FDeviceRole write FDeviceRole default eMultimedia;
    property DeviceId: string read FDeviceId write FDeviceId;
    property DeviceName: string read FDeviceName;
    property DeviceIndex: Integer read FDeviceIndex write FDeviceIndex default 0; // Must be valid when UseDefaultDevice = False

    // Secondary (Cue/PFL) output ----------------------------------------------
    // When CueEnabled = True, the engine will mirror audio to the Cue endpoint as well.
    // Cue endpoint selection: When CueUseDefaultDevice = True, the default endpoint for CueDeviceRole is used.
    // Otherwise CueDeviceId is used.
    property CueEnabled: Boolean read FCueEnabled write FCueEnabled default False;
    property CueUseDefaultDevice: Boolean read FCueUseDefaultDevice write FCueUseDefaultDevice default True;
    property CueDeviceRole: ERole read FCueDeviceRole write FCueDeviceRole default eMultimedia;
    property CueDeviceId: string read FCueDeviceId write FCueDeviceId;
    property CueMuted: Boolean read FCueMuted write FCueMuted;
    // -------------------------------------------------------------------------

    property OnOutputPcm: TOnOutputPcm read FOnOutputPcm write FOnOutputPcm;

    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnReady: TWasApiReadyEvent read FOnReady write FOnReady;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
    property OnFillPcm: TWasApiFillPcmEvent read FOnFillPcm write FOnFillPcm;
    property OnProcessPcm: TWasApiProcessPcmEvent read FOnProcessPcm write FOnProcessPcm;
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


class function TEngineCommand.SetCueVolume(const AVol: Single): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckSetCueVolume;
  Result.CueVol := AVol;
end;


class function TEngineCommand.SetCueMute(const AMute: Boolean): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckSetCueMute;
  Result.CueMuteActive := AMute;
end;



class function TEngineCommand.SwitchDevice(const ADeviceId: string;
                                           const AUseDefault: Boolean;
                                           const ARole: Integer;
                                           const AAutoResume: Boolean): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckSwitchDevice;
  Result.DeviceId := ADeviceId;
  Result.UseDefaultDevice := AUseDefault;
  Result.DeviceRole := ARole;
  Result.AutoResume := AAutoResume;
end;


// EQ methods
class function TEngineCommand.EQEnable(const AEnabled: Boolean): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckEQEnable;
  Result.EqEnabled := AEnabled;
end;


class function TEngineCommand.EQSetBassDb(const ABassDb: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckEQSetBassDb;
  Result.EqBassDb := ABassDb;
end;


class function TEngineCommand.EQSetTrebleDb(const ATrebleDb: Integer): TEngineCommand;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Kind := ckEQSetTrebleDb;
  Result.EqTrebleDb := ATrebleDb;
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
  pvSimpleVol := nil;
  pvCueSimpleVol := nil;

  // Default device selection
  FUseDefaultDevice := True;
  FDeviceRole := eMultimedia;
  FDeviceId := '';

  // Cue/PFL output defaults
  FCueEnabled := False;
  FCueUseDefaultDevice := True;
  FCueDeviceRole := eMultimedia;
  FCueDeviceId := '';
  FCueMuted := False;
  FCueVolume := 1.0;

  pvCueAudioClient := nil;
  pvCueRenderClient := nil;
  pvCueSimpleVol := nil;
  pvCueAudioSamplesReadyEvent := 0;
  pvCueBufferFrameCount := 0;

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

  // Pitching/tempo defaults ---------------------------------------------------
  FVarispeedEnabled := True;
  FPitchRangePct := 16.0;
  FRampMs := 50;

  FPitchDetentPct := 0.10;
  FPitchAutoZeroPct := 0.30;

  FRateTarget := 1.0;
  FRateSmooth := 1.0;
  FReadPos := 0.0;

  FInterpQuality := iqCatmullRom; // Default to high quality.
  // ---------------------------------------------------------------------------

  // Post Fader VU value.
  SetMeterFaderGains(1.0,
                     1.0);

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

  FStoppedEvent := TEvent.Create(nil,
                                 True {ManualReset},
                                 True {InitiallySignaled},
                                 '');
  FRunning := 0;

  // Start worker thread immediately (engine owner)
  FThread := TWasApiEngineThread.Create(Self);
  SetState(dsInitialized);

  //
  FFxCS := TCriticalSection.Create();

  SetLength(FFx,
            0);

  SetLength(FFxTypeSet,
            0);
end;


destructor TWasApiEngine.Destroy();
begin

  // Request thread shutdown
  EnqueueCommand(TEngineCommand.Shutdown);
  SetEvent(FCmdEvent);
  CloseHandle(FCmdEvent);
  FCmdEvent := 0;

  SetEvent(FTerminateEvent);
  CloseHandle(FTerminateEvent);
  FTerminateEvent := 0;

  if Assigned(FThread) then
    begin

      FThread.Terminate;
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

  FreeAndNil(FStoppedEvent);

  FreeAndNil(FCmdQueue);
  FreeAndNil(FCmdCS);

  if Assigned(pvSourceWfx) then
    begin

      CoTaskMemFree(pvSourceWfx);
      pvSourceWfx := nil;
    end;

  if Assigned(pvRenderWfx) then
    begin
      CoTaskMemFree(pvRenderWfx);
      pvRenderWfx := nil;
    end;

  FFxCS.Free();

  CLoseHandle(pvMmcssHandle);
  pvMmcssHandle := 0;

  SafeRelease(pvAudioClient);
  SafeRelease(pvAudioStreamVolume);
  SafeRelease(pvRenderClient);
  SafeRelease(pvAudioClock);
  SafeRelease(pvSimpleVol);
  SafeRelease(pvCueSimpleVol);

  inherited;
end;


// tempo/pitching
function TWasApiEngine.RampCoeff(const RampMs,
                                 SampleRate,
                                 Frames: Integer): Double;
var
  dt,
  tau: Double;

begin

  if (RampMs <= 0) then
    Exit(1.0);

  dt := Frames / SampleRate;
  tau := RampMs / 1000.0;

  // 1 - exp(-dt/tau)
  Result := 1.0 - Exp(-dt / tau);
end;


function TWasApiEngine.LoadData_VarispeedFloat32(const Frames: Integer;
                                                 const OutPtr: PSingle;
                                                 out Flags: DWORD): HRESULT;

  function ReadSample(const Base: PSingle;
                      const Index: Integer): Single; inline;
  begin

    Result := PSingle(NativeUInt(Base) + NativeUInt(Index) * NativeUInt(SizeOf(Single)))^;
  end;

  procedure WriteSample(const Base: PSingle;
                        const Index: Integer;
                        const Value: Single); inline;
  begin

    PSingle(NativeUInt(Base) + NativeUInt(Index) * NativeUInt(SizeOf(Single)))^ := Value;
  end;

var
  channels: Integer;
  srcFramesTotal: Integer;
  srcBase,
  outBase: PSingle;
  maxSampleIndex: Integer;

  i0,
  i,
  ch: Integer;
  frac: Single;
  rate,
  coeff: Double;
  outIdx: Integer;

  idxM1,
  idx0,
  idx1,
  idx2: Integer;
  y0,
  y1,
  y2,
  y3: Single;

  pSilence: PByte;
  silenceBytes: Integer;

begin

  Result := S_OK;
  Flags := 0;

  if (pvBytes = nil) or (pvBytesLength <= 0) or (pvSourceWfx = nil) then
    begin

      FillChar(OutPtr^, Frames * 2 * SizeOf(Single), 0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  // Client format
  channels := pvSourceWfx.nChannels;

  // pvBytes float32 interleaved
  srcFramesTotal := Integer(pvBytesLength) div Integer(channels * SizeOf(Single));

  if (srcFramesTotal <= 1) then
    begin

      FillChar(OutPtr^,
               Frames * channels * SizeOf(Single),
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  srcBase := PSingle(Pointer(pvBytes));
  outBase := PSingle(Pointer(OutPtr));
  maxSampleIndex := (srcFramesTotal * channels) - 1;

  // Smooth rate per block
  if (FRateTarget <= 0.0) then
    FRateTarget := 1.0;

  coeff := RampCoeff(FRampMs, pvSourceWfx.nSamplesPerSec, Frames);
  if (coeff >= 1.0) then
    FRateSmooth := FRateTarget
  else
    FRateSmooth := FRateSmooth + (FRateTarget - FRateSmooth) * coeff;

  rate := FRateSmooth;
  if (rate <= 0.0) then
    rate := 1.0;

  outIdx := 0;

  for i := 0 to Frames - 1 do
    begin

      i0 := Trunc(FReadPos);

      if (i0 >= srcFramesTotal - 1) then
        begin

          pSilence := PByte(NativeUInt(outBase) + NativeUInt(outIdx) * NativeUInt(SizeOf(Single)));
          silenceBytes := (Frames - i) * channels * SizeOf(Single);
          FillChar(pSilence^,
                   silenceBytes,
                   0);

          if (i = 0) then
            Flags := AUDCLNT_BUFFERFLAGS_SILENT;

          FOffset := pvBytesLength;
          Exit(S_OK);
        end;

      frac := (FReadPos - i0) * 1.0;

      for ch := 0 to channels - 1 do
        begin

          idx0 := (i0 * channels) + ch;
          idx1 := ((i0 + 1) * channels) + ch;

          if (idx1 > maxSampleIndex) then
            begin

              pSilence := PByte(NativeUInt(outBase) + NativeUInt(outIdx) * NativeUInt(SizeOf(Single)));
              silenceBytes := (Frames - i) * channels * SizeOf(Single);
              FillChar(pSilence^,
                       silenceBytes,
                       0);

              if (i = 0) then
                Flags := AUDCLNT_BUFFERFLAGS_SILENT;

              FOffset := pvBytesLength;
              Exit(S_OK);
            end;

          if (FInterpQuality = iqCatmullRom) and (i0 >= 1) and (i0 + 2 < srcFramesTotal) then
            begin

              idxM1 := ((i0 - 1) * channels) + ch;
              idx2  := ((i0 + 2) * channels) + ch;

              y0 := ReadSample(srcBase, idxM1);
              y1 := ReadSample(srcBase, idx0);
              y2 := ReadSample(srcBase, idx1);
              y3 := ReadSample(srcBase, idx2);

              WriteSample(outBase,
                          outIdx + ch,
                          PcmLib.MfCatmullRomS(y0,
                                               y1,
                                               y2,
                                               y3,
                                               frac));
            end
          else
            begin

              y1 := ReadSample(srcBase, idx0);
              y2 := ReadSample(srcBase, idx1);
              WriteSample(outBase,
                          outIdx + ch,
                          y1 + (y2 - y1) * frac);
            end;
        end;

      Inc(outIdx, channels);
      FReadPos := FReadPos + rate;
    end;

  // FOffset := Int64(Trunc(FReadPos)) * Int64(channels * SizeOf(Single));
  // or safer:
  FOffset := Int64(Trunc(FReadPos)) * Int64(pvSourceWfx.nBlockAlign);
end;


procedure TWasApiEngine.SetState(const NewState: TDeviceState);
begin

  pvDeviceState := NewState;
  if Assigned(FOnStateChanged) then
    TThread.Queue(nil,
                  procedure
                  begin

                    if Assigned(FOnStateChanged) then
                      FOnStateChanged(Self,
                                      NewState);
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
  newOffset: UINT64;

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

    ckSwitchDevice:
      begin
        // Reinitialize WASAPI client on a different endpoint, keeping decoded PCM (pvBytes) intact.
        // This is executed on the engine thread (safe to touch COM interfaces here).
        // Stop/Reset current client if still alive.
        if Assigned(pvAudioClient) then
          begin

            pvAudioClient.Stop();
            if (pvCueAudioClient <> nil) then
              pvCueAudioClient.Stop();
            pvAudioClient.Reset();
          end;

        // Release WASAPI interfaces so InitializeAudioEngine can activate the new device.
        pvRenderClient := nil;
        pvAudioStreamVolume := nil;
        pvAudioClock := nil;
        pvSimpleVol := nil;
        pvAudioClient := nil;

        // Persist selection (in case command overrides fields)
        FUseDefaultDevice := Cmd.UseDefaultDevice;
        FDeviceId := Cmd.DeviceId;
        if (Cmd.DeviceRole >= Ord(Low(ERole))) and (Cmd.DeviceRole <= Ord(High(ERole))) then
          FDeviceRole := ERole(Cmd.DeviceRole)
        else
          FDeviceRole := eMultimedia;

        hr := InitializeAudioEngine();
        if SUCCEEDED(hr) then
          hr := SetFormat(pvSourceWfx);

        if FAILED(hr) then
        begin
          SetState(dsError);
          RaiseError('SwitchDevice failed', hr);
        end
        else
        begin
          // return to ready state (file still loaded)
          SetState(dsReady);
          RaiseReady();

          if Cmd.AutoResume then
          begin
            // resume playback
            EnqueueCommand(TEngineCommand.Play);
          end;
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
         (pvSourceWfx.nAvgBytesPerSec <> 0) then
         begin

           pos100ns := Cmd.SeekPos100ns;

           if (pos100ns < 0) then
             pos100ns := 0;

           if (FDuration100ns > 0) and (pos100ns > FDuration100ns) then
             pos100ns := FDuration100ns;

           // 100ns -> bytes in SOURCE PCM layout
           newOffset := (UInt64(pos100ns) * UInt64(pvSourceWfx.nAvgBytesPerSec)) div UInt64(REFTIMES_PER_SEC);

           // Align to source block size
           if (pvSourceWfx.nBlockAlign <> 0) then
             newOffset := (newOffset div UInt64(pvSourceWfx.nBlockAlign)) * UInt64(pvSourceWfx.nBlockAlign);

           // Clamp to last full block, not EOF
           if (pvSourceWfx.nBlockAlign <> 0) and (pvBytesLength >= pvSourceWfx.nBlockAlign) then
             begin

               if (newOffset >= UInt64(pvBytesLength)) then
                 newOffset := UInt64(pvBytesLength - pvSourceWfx.nBlockAlign);
             end
           else
             begin

               if (newOffset > UInt64(pvBytesLength)) then
                 newOffset := UInt64(pvBytesLength);
             end;

           if Assigned(pvAudioClient) then
             begin

               pvAudioClient.Stop();

               if (pvCueAudioClient <> nil) then
                 pvCueAudioClient.Stop();

               pvAudioClient.Reset();

               if (pvCueAudioClient <> nil) then
                 pvCueAudioClient.Reset();
             end;

           // Real seek position = source frame index.
           if (pvSourceWfx.nBlockAlign <> 0) then
             FReadPos := Round(newOffset / UInt64(pvSourceWfx.nBlockAlign))
           else
             FReadPos := 0.0;

           // Keep byte mirror in sync with the real read cursor.
           FOffset := Int64(Trunc(FReadPos)) * Int64(pvSourceWfx.nBlockAlign);

           // Re-anchor time to the actual aligned byte position
           FBasePos100ns := Int64((UInt64(FOffset) * UInt64(REFTIMES_PER_SEC)) div UInt64(pvSourceWfx.nAvgBytesPerSec));

           if (FRateTarget <= 0.0) then
             FRateTarget := 1.0;

           // After seek, snap smoothing to the current target.
           FRateSmooth := FRateTarget;

           // Optional: immediately notify UI with actual seeked position
           RaiseProcessed(FBasePos100ns,
                          UInt64(Trunc(FReadPos)));

           if (pvDeviceState = dsPlay) and Assigned(pvAudioClient) then
             begin

               pvAudioClient.Start();
               if (pvCueAudioClient <> nil) then
                 pvCueAudioClient.Start();
             end;
         end;
      end;

    ckSetVolume:
      begin

        if Assigned(pvAudioStreamVolume) then
          begin

            // Channel volumes applied in play thread too; safe here on engine thread.
            SetVolumes(Cmd.VolL,
                       Cmd.VolR);
          end;
      end;

    ckSetCueVolume:
      begin

        // Apply cue session volume (engine thread).
        if Assigned(pvCueSimpleVol) then
          pvCueSimpleVol.SetMasterVolume(Cmd.CueVol,
                                         nil);
      end;

    ckSetCueMute:
      begin

        // Apply cue session mute (engine thread).
        if Assigned(pvCueSimpleVol) then
          pvCueSimpleVol.SetMute(Cmd.CueMuteActive,
                                 nil);
      end;
  end;
end;


// FX Helpers ==================================================================

// Weï¿½ll create a media type from it and apply to each FX before processing.
function TWasApiEngine.CreateAudioMediaTypeFromWfx(pwfx: PWAVEFORMATEX;
                                                   out M: IMFMediaType): HRESULT;
var
  Sub: TGUID;

begin

  M := nil;
  Result := MFCreateMediaType(M);
  if FAILED(Result) then
    Exit;

  Result := M.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  // PCM vs Float32
  if (pwfx.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) and (pwfx.wBitsPerSample = 32) then
    Sub := MFAudioFormat_Float
  else
    Sub := MFAudioFormat_PCM;

  Result := M.SetGUID(MF_MT_SUBTYPE,
                      Sub);
  if FAILED(Result) then
    Exit;

  Result := M.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        pwfx.nChannels);
  if FAILED(Result) then
    Exit;

  Result := M.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        pwfx.nSamplesPerSec);
  if FAILED(Result) then
    Exit;

  Result := M.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        pwfx.wBitsPerSample);
  if FAILED(Result) then
    Exit;

  Result := M.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                        pwfx.nBlockAlign);
  if FAILED(Result) then
    Exit;

  Result := M.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                        pwfx.nAvgBytesPerSec);
end;


function TWasApiEngine.EnsureFxTypesSetLocked(): HRESULT;
var
  M: IMFMediaType;
  i: Integer;

begin

  // IMPORTANT:
  // FX processing runs on the render client buffer (pBufferData).
  // That buffer is in the format used to Initialize() the IAudioClient.
  // In this engine that is the decoded/source format (pvSourceWfx).
  // Using the mix format (pvRenderWfx) here will configure MFTs for a
  // different sample layout than the actual bytes in the render buffer
  // (e.g. float32 vs int16) which can sound like "DSound distortion".
  if (pvSourceWfx = nil) then
    Exit(E_POINTER);

  if (Length(FFx) = 0) then
    Exit(S_OK);

  Result := CreateAudioMediaTypeFromWfx(pvSourceWfx,
                                        M);
  if FAILED(Result) then
    Exit;

  for i := 0 to High(FFx) do
    begin

      if (FFx[i] <> nil) and (not FFxTypeSet[i]) then
        begin

          Result := FFx[i].SetInputType(0,
                                        M,
                                        0);
          if FAILED(Result) then
            Exit;

          Result := FFx[i].SetOutputType(0,
                                         M,
                                         0);
          if FAILED(Result) then
            Exit;

          FFxTypeSet[i] := True;
        end;
  end;
end;


// FX Core ---------------------------------------------------------------------

function TWasApiEngine.ProcessMftBuffer(const AMft: IMFTransform;
                                        pData: PByte;
                                        const ByteCount: DWORD): HRESULT;
var
  hr: HRESULT;
  inSample,
  outSample: IMFSample;
  inBuf,
  outBuf: IMFMediaBuffer;
  outData: MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  pIn,
  pOut: PByte;
  cbOut: DWORD;

begin

  if (AMft = nil) or
     (pData = nil) or
     (ByteCount = 0) then
    Exit(S_OK);

  hr := MFCreateSample(inSample);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount,
                             inBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := inBuf.Lock(pIn,
                   nil,
                   nil);
  if FAILED(hr) then
    Exit(hr);

  try

    Move(pData^,
         pIn^,
         ByteCount);
  finally

    inBuf.Unlock();
  end;

  hr := inBuf.SetCurrentLength(ByteCount);
  if FAILED(hr) then
    Exit(hr);

  hr := inSample.AddBuffer(inBuf);
  if FAILED(hr) then
    Exit(hr);

  // output sample must be provided
  hr := MFCreateSample(outSample);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount,
                             outBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := outSample.AddBuffer(outBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := AMft.ProcessInput(0,
                          inSample,
                          0);

  if (hr = MF_E_NOTACCEPTING) then
    begin

      ZeroMemory(@outData,
                 SizeOf(outData));
      outData.pSample := outSample;
      status := 0;

      hr := AMft.ProcessOutput(0,
                               1,
                               @outData, status);
      if FAILED(hr) and (hr <> MF_E_TRANSFORM_NEED_MORE_INPUT) then
        Exit(hr);

      hr := AMft.ProcessInput(0,
                              inSample,
                              0);
    end;

  if FAILED(hr) then
    Exit(hr);

  ZeroMemory(@outData,
             SizeOf(outData));
  outData.pSample := outSample;
  status := 0;

  hr := AMft.ProcessOutput(0,
                           1,
                           @outData,
                           status);

  if (hr = MF_E_TRANSFORM_NEED_MORE_INPUT) then
    Exit(S_OK);

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

  Result := S_OK;
end;


// Chain runner ////////////////////////////////////////////////////////////////
function TWasApiEngine.ProcessEffectsBuffer(pData: PByte;
                                            const ByteCount: DWORD): HRESULT;
var
  hr: HRESULT;
  i: Integer;
  localFx: TArray<IMFTransform>;
  localTypeSet: TArray<Boolean>;

begin

  Result := S_OK;
  if (pData = nil) or (ByteCount = 0) then
    Exit(S_OK);

  // Snapshot under lock (avoid holding CS during DSP)
  FFxCS.Enter;

  try

    localFx := Copy(FFx);
    localTypeSet := Copy(FFxTypeSet);
  finally

    FFxCS.Leave();
  end;

  if (Length(localFx) = 0) then
  Exit(S_OK);

  // Ensure types are set using the real arrays (we must update TypeSet flags)
  FFxCS.Enter();

  try

    hr := EnsureFxTypesSetLocked();
    if FAILED(hr) then
      Exit(hr);
  finally

    FFxCS.Leave();
  end;

  // Now run chain (no engine lock held)
  for i := 0 to High(localFx) do
  begin

    if (localFx[i] <> nil) then
      begin

        hr := ProcessMftBuffer(localFx[i],
                               pData,
                               ByteCount);
        if FAILED(hr) then
          Exit(hr);
      end;
  end;
end;

// -----------------------------------------------------------------------------
// =============================================================================


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


function TWasApiEngine.StartSourceMode(): HRESULT;
begin

  // Logical playback state only.
  // No endpoint render loop.
  FRequestStop := False;
  FRequestPause := False;

  if (FRateTarget = 0.0) then
    FRateTarget := 1.0;

  // Do not reset FReadPos here unless you want "start from beginning".
  // This should behave like normal transport start/resume.
  SetState(dsPlay);
  Result := S_OK;
end;


function TWasApiEngine.Stop(): HResult;
begin

    EnqueueCommand(TEngineCommand.Stop);
  Result := S_OK;
end;


function TWasApiEngine.WaitForStop(TimeoutMs: DWORD = 2000): HRESULT;
begin
  if InterlockedCompareExchange(FRunning, 0, 0) = 0 then
    Exit(S_OK);

  case FStoppedEvent.WaitFor(TimeoutMs) of
    wrSignaled: Result := S_OK;
    wrTimeout:  Result := HRESULT_FROM_WIN32(WAIT_TIMEOUT);
  else
    Result := E_FAIL;
  end;
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



function TWasApiEngine.SwitchOutputDevice(const ADeviceId: string;
                                          const AUseDefaultDevice: Boolean = True;
                                          const ARole: ERole = eMultimedia;
                                          const AutoResume: Boolean = True): HRESULT;
var
  resume: Boolean;
begin
  // Switching requires a configured stream format (after OpenFile/SetFormat).
  if (pvSourceWfx = nil) or (pvAudioClient = nil) then
    Exit(E_FAIL);

  // Remember desired selection for the next InitializeAudioEngine()
  FUseDefaultDevice := AUseDefaultDevice;
  FDeviceRole := ARole;
  FDeviceId := ADeviceId;

  // If currently playing, stop first then re-init and optionally resume
  resume := AutoResume and (pvDeviceState = dsPlay);

  if (pvDeviceState = dsPlay) or (pvDeviceState = dsPause) then
    EnqueueCommand(TEngineCommand.Stop);

  EnqueueCommand(TEngineCommand.SwitchDevice(ADeviceId,
                                            AUseDefaultDevice,
                                            Ord(ARole),
                                            resume));
  Result := S_OK;
end;


procedure TWasApiEngine.SetUseDefaultOutputDevice(const ARole: ERole = eMultimedia);
begin
  FUseDefaultDevice := True;
  FDeviceRole := ARole;
  FDeviceId := '';

  // Cue/PFL output defaults
  FCueEnabled := False;
  FCueUseDefaultDevice := True;
  FCueDeviceRole := eMultimedia;
  FCueDeviceId := '';
  FCueMuted := False;
  FCueVolume := 1.0;

  pvCueSimpleVol := nil;
  pvCueAudioClient := nil;
  pvCueRenderClient := nil;
  pvCueAudioSamplesReadyEvent := 0;
  pvCueBufferFrameCount := 0;
end;


procedure TWasApiEngine.SetOutputDeviceId(const ADeviceId: string);
begin
  FUseDefaultDevice := False;
  FDeviceId := ADeviceId;
end;


function TWasApiEngine.ReadOutputPcmFloat32(const Frames: Integer;
                                            const OutBuffer: PSingle;
                                            out Flags: DWORD): HRESULT;
var
  ByteCount: DWORD;
  Channels: Integer;

begin

  Flags := 0;

  if (Frames <= 0) or (OutBuffer = nil) then
    Exit(E_INVALIDARG);

  // Output contract of this method is float32 PCM.
  // So when silent / not ready, we can zero the destination directly
  // without depending on pvSourceWfx.
  Channels := pvSoundChannels;
  if (Channels <= 0) then
    Channels := 2; // safe fallback for your mixer path

  if (pvDeviceState <> dsPlay) then
    begin
      FillChar(OutBuffer^,
               Frames * Channels * SizeOf(Single),
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  if (pvSourceWfx = nil) then
    begin
      FillChar(OutBuffer^,
               Frames * Channels * SizeOf(Single),
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  if (pvSourceWfx.wFormatTag <> WAVE_FORMAT_IEEE_FLOAT) or
     (pvSourceWfx.wBitsPerSample <> 32) then
    Exit(MF_E_INVALIDMEDIATYPE);

  // Same generator path as normal playback, but without endpoint rendering.
  Result := LoadData_VarispeedFloat32(Frames,
                                      OutBuffer,
                                      Flags);
  if FAILED(Result) then
    Exit;

  ByteCount := DWORD(Frames) * DWORD(pvSourceWfx.nBlockAlign);

  // Same callback order as render path.
  if Assigned(FOnProcessPcm) then
    begin
      try
        FOnProcessPcm(Self,
                      PByte(OutBuffer),
                      ByteCount,
                      pvSourceWfx);
      except
        on E: Exception do
          begin
            Flags := Flags or AUDCLNT_BUFFERFLAGS_SILENT;
            RaiseError('ReadOutputPcmFloat32.OnProcessPcm exception: ' + E.Message,
                       E_FAIL);
            Exit(E_FAIL);
          end;
      end;
    end;

  // Built-in per-engine rack path stays available.
  // In your new mixer design this rack should normally remain empty.
  if ((Flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
    begin
      Result := ProcessEffectsBuffer(PByte(OutBuffer),
                                     ByteCount);
      if FAILED(Result) then
        Exit;
    end;

  if Assigned(FOnOutputPcm) then
    begin
      try
        if ((Flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
          FOnOutputPcm(Self,
                       nil,
                       0,
                       pvSourceWfx)
        else
          FOnOutputPcm(Self,
                       PByte(OutBuffer),
                       ByteCount,
                       pvSourceWfx);
      except
        on E: Exception do
          begin
            Flags := Flags or AUDCLNT_BUFFERFLAGS_SILENT;
            RaiseError('ReadOutputPcmFloat32.OnOutputPcm exception: ' + E.Message,
                       E_FAIL);
            Exit(E_FAIL);
          end;
      end;
    end;
end;


function TWasApiEngine.Mute(pActive: Boolean): Boolean;
var
  hr: HResult;

begin

  hr := E_POINTER;

  if Assigned(pvSimpleVol) then
    hr := pvSimpleVol.SetMute(pActive,
                              nil);

  Result := Succeeded(hr);
end;


function TWasApiEngine.CueMute(pActive: Boolean): Boolean;
begin
  // Thread-safe from GUI thread: mute will be applied on the engine thread
  // via ProcessControlCommand(ckSetCueMute).
  FCueMuted := pActive;
  EnqueueCommand(TEngineCommand.SetCueMute(pActive));
  Result := True;
end;


function TWasApiEngine.SetCueVolumeAsync(const VolScalar: Single): HRESULT;
var
  v: Single;
begin
  // Thread-safe from GUI thread: cue volume will be applied on the engine thread
  // via ProcessControlCommand(ckSetCueVolume).
  v := VolScalar;
  if (v < 0.0) then
    v := 0.0
  else
    if (v > 1.0) then
      v := 1.0;

  FCueVolume := v;
  EnqueueCommand(TEngineCommand.SetCueVolume(v));
  Result := S_OK;
end;




function TWasApiEngine.SetVolumesAsync(const VolLeft,
                                       VolRight: Single): HRESULT;
begin

  // Thread-safe from GUI thread: Volume will be applied on the engine thread
  // via ProcessControlCommand(ckSetVolume).
  EnqueueCommand(TEngineCommand.SetVolume(VolLeft,
                                          VolRight));
  Result := S_OK;
end;


function TWasApiEngine.SetVolumes(pVolLeft,
                                  pVolRight: Single): HResult;
var
  hr: HResult;

begin

  hr := S_OK;

  // This is expected to run on the engine thread.
  if not Assigned(pvAudioStreamVolume) then
    Exit(E_POINTER);

  if (pvSoundChannels >= 1) then
    hr := pvAudioStreamVolume.SetChannelVolume(0,
                                               pVolLeft);

  if (pvSoundChannels >= 2) then
    hr := pvAudioStreamVolume.SetChannelVolume(1,
                                               pVolRight);

  Result := hr;
end;


procedure TWasApiEngine.SetMeterFaderGains(const GainL,
                                           GainR: Single);
var
  b: LongInt;

begin

  b := PLongInt(@GainL)^;
  InterlockedExchange(FMeterGainBitsL,
                      b);

  b := PLongInt(@GainR)^;
  InterlockedExchange(FMeterGainBitsR,
                      b);
end;


procedure TWasApiEngine.GetMeterFaderGains(out GainL,
                                           GainR: Single);
var
  b: LongInt;

begin

  b := InterlockedExchangeAdd(FMeterGainBitsL,
                              0);
  GainL := PSingle(@b)^;

  b := InterlockedExchangeAdd(FMeterGainBitsR,
                              0);
  GainR := PSingle(@b)^;
end;


// Pitching/tempo

procedure TWasApiEngine.SetPitchPercent(const Pct: Double);
var
  r: Double;

begin

  // Typical tempo range: -16..+16 (%). You may allow more if you want, but this should do.
  r := 1.0 + (Pct / 100.0);

  if (r < 0.5) then
    r := 0.5;

  if (r > 2.0) then
    r := 2.0;
  FRateTarget := r;
end;


procedure TWasApiEngine.SetPitchSlider(const SliderPos: Integer);
var
  pct: Double;
  rate: Double;

begin

  // SliderPos -100..+100 -> pct -PitchRange..+PitchRange
  pct := (SliderPos / 100.0) * FPitchRangePct;

  // Hard detent: treat small region as exactly 0
  if (Abs(pct) <= FPitchDetentPct) then
    pct := 0.0
  else
    begin

      // Auto-zero: if user is close to center, snap to exact 0
      if (Abs(pct) <= FPitchAutoZeroPct) then
        pct := 0.0;
    end;

  // Varispeed factor
  rate := 1.0 + (pct / 100.0);

  // Safety clamp
  if (rate < 0.5) then
    rate := 0.5;

  if (rate > 2.0) then
    rate := 2.0;

  FRateTarget := rate;
end;


procedure TWasApiEngine.MeterPublishFloat32(const p: PSingle;
                                            const Frames,
                                            Channels: Integer);
var
  i: Integer;
  l, r: Single;
  absL, absR: Single;
  peakL, peakR: Single;
  sumL, sumR: Double;
  bits: LongInt;

begin

  if (p = nil) or
     (Frames <= 0) or
     (Channels < 1) then
    Exit;

  peakL := 0;
  peakR := 0;
  sumL := 0;
  sumR := 0;

  if (Channels = 1) then
    begin

      for i := 0 to Frames - 1 do
        begin

          l := PSingle(NativeUInt(p) + NativeUInt(i) * 4)^;
          absL := Abs(l);
          if (absL > peakL) then
            peakL := absL;
          sumL := sumL + (l * l);
        end;

      peakR := peakL;
      sumR := sumL;
    end
  else
    begin

      for i := 0 to Frames - 1 do
        begin

          l := PSingle(NativeUInt(p) + NativeUInt((i * Channels) + 0) * 4)^;
          r := PSingle(NativeUInt(p) + NativeUInt((i * Channels) + 1) * 4)^;

          absL := Abs(l);
          if (absL > peakL) then
            peakL := absL;

          absR := Abs(r);
          if (absR > peakR) then
            peakR := absR;

          sumL := sumL + (l * l);
          sumR := sumR + (r * r);
        end;
    end;

  // RMS
  if (Frames > 0) then
    begin

      l := Sqrt(sumL / Frames);
      r := Sqrt(sumR / Frames);
    end
  else
    begin

      l := 0;
      r := 0;
    end;

  // store as bits atomically
  bits := PLongInt(@peakL)^;
  InterlockedExchange(FMeterPeakBitsL,
                      bits);

  bits := PLongInt(@peakR)^;
  InterlockedExchange(FMeterPeakBitsR,
                      bits);

  bits := PLongInt(@l)^;
  InterlockedExchange(FMeterRmsBitsL,
                      bits);

  bits := PLongInt(@r)^;
  InterlockedExchange(FMeterRmsBitsR,
                      bits);
end;


// FX ==========================================================================

procedure TWasApiEngine.ClearEffects;
begin

  FFxCS.Enter();

  try

    SetLength(FFx,
              0);
    SetLength(FFxTypeSet,
              0);
  finally

    FFxCS.Leave();
  end;
end;


procedure TWasApiEngine.AddEffect(const Mft: IMFTransform);
var
  n: Integer;

begin

  if (Mft = nil) then
    Exit;

  FFxCS.Enter();

  try

    n := Length(FFx);

    SetLength(FFx,
              n + 1);

    SetLength(FFxTypeSet,
              n + 1);

    FFx[n] := Mft;
    FFxTypeSet[n] := False; // Will apply media types on first use.
  finally

    FFxCS.Leave();
  end;
end;


procedure TWasApiEngine.SetEffects(const Effects: array of IMFTransform);
var
  i,
  n: Integer;

begin

  FFxCS.Enter();

  try

    n := Length(Effects);
    SetLength(FFx,
              n);

    SetLength(FFxTypeSet,
              n);

    for i := 0 to n - 1 do
      begin

        FFx[i] := Effects[i];
        FFxTypeSet[i] := False;
      end;
  finally

    FFxCS.Leave;
  end;
end;

// =============================================================================


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


function TWasApiEngine.LoadData(const Frames: UINT32;
                                const pBufferData: PByte;
                                var Flags: DWORD): HRESULT;
begin

  if (pvSourceWfx = nil) then
    Exit(E_POINTER);

  // Client format must be IEEE float32 (this is what IAudioClient.Initialize uses)
  if (pvSourceWfx.wFormatTag <> WAVE_FORMAT_IEEE_FLOAT) or
     (pvSourceWfx.wBitsPerSample <> 32) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if not Boolean(FVarispeedEnabled) then
    begin

      FRateTarget := 1.0;
      if (FRampMs <= 0) then
        FRateSmooth := 1.0;
    end;

  Result := LoadData_VarispeedFloat32(Integer(Frames),
                                      PSingle(Pointer(pBufferData)), // XE7-safe
                                      Flags);
end;


// Helper for LoadFileInternal.
function TWasApiEngine.ConvertPvBytesToFloat32InPlace(): HRESULT;
var
  bits: Integer;
  isFloat: Boolean;
  frames: Integer;
  ch: Integer;
  samples: Integer;
  floatBytes: Integer;
  pNew: PByte;

begin

  if (pvBytes = nil) or (pvBytesLength <= 0) or (pvSourceWfx = nil) then
    Exit(E_POINTER);

  ch := pvSourceWfx.nChannels;
  if (ch <= 0) or (pvSourceWfx.nBlockAlign <= 0) then
    Exit(E_FAIL);

  // How many frames are currently in pvBytes (in pvSourceWfx layout)
  frames := pvBytesLength div pvSourceWfx.nBlockAlign;
  if (frames <= 1) then
    Exit(E_FAIL);

  samples := frames * ch;

  if not GetWfxBitsAndFloat(pvSourceWfx, bits, isFloat) then
    Exit(MF_E_INVALIDMEDIATYPE);

  floatBytes := samples * SizeOf(Single);
  GetMem(pNew, floatBytes);
  FillChar(pNew^, floatBytes, 0);

  try
    if isFloat and (bits = 32) then
    begin
      // Already float32 interleaved
      Move(pvBytes^, pNew^, floatBytes);
    end
    else
    begin
      case bits of
        16: Int16ToFloat(pvBytes, PSingle(pNew), samples);
        24: Int24ToFloat(pvBytes, PSingle(pNew), samples);
        32: Int32ToFloat(pvBytes, PSingle(pNew), samples); // int32 PCM
      else
        Exit(MF_E_INVALIDMEDIATYPE);
      end;
    end;

    // Replace old buffer
    FreeMem(pvBytes);
    pvBytes := pNew;
    pNew := nil;

    pvBytesLength := floatBytes;

    // Update pvSourceWfx to describe float32 interleaved
    // (Keep SR/ch; blockalign/avgbytes change)
    pvSourceWfx.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
    pvSourceWfx.wBitsPerSample := 32;
    pvSourceWfx.nBlockAlign := WORD(ch * SizeOf(Single));
    pvSourceWfx.nAvgBytesPerSec := pvSourceWfx.nSamplesPerSec * pvSourceWfx.nBlockAlign;
    pvSourceWfx.cbSize := 0;

    // Reset varispeed read cursor
    FReadPos := 0.0;
    if (FRateTarget <= 0.0) then FRateTarget := 1.0;
    FRateSmooth := FRateTarget;

    Result := S_OK;
  finally
    if (pNew <> nil) then
      FreeMem(pNew);
  end;
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
                                         @currentMediaType);
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

  if FAILED(hr) then
    Exit(hr);

  // Convert decoded pvBytes (any 16/24/32) -> float32 interleaved.
  hr := ConvertPvBytesToFloat32InPlace();
  if FAILED(hr) then
    Exit(hr);

  // NOW configure and init WASAPI for the final layout (float32).
  hr := SetFormat(pvSourceWfx);
  if FAILED(hr) then
    Exit(hr);

  // Duration calc is now correct for float32 too.
  if (fileDuration > 0) then
    FDuration100ns := fileDuration
  else
    begin

      FDuration100ns := 0;
      if (pvSourceWfx <> nil) and (pvSourceWfx.nAvgBytesPerSec <> 0) then
        FDuration100ns := Int64((UInt64(pvBytesLength) * UInt64(REFTIMES_PER_SEC)) div UInt64(pvSourceWfx.nAvgBytesPerSec));
    end;
  Result := hr;
end;


//
function TWasApiEngine.GetDeviceIdByIndex(const AIndex: Integer;
                                          out ADeviceId: string): HRESULT;
var
  pEnumerator: IMMDeviceEnumerator;
  pCollection: IMMDeviceCollection;
  pDevice: IMMDevice;
  count: UINT;
  pId: PWideChar;

begin

  ADeviceId := '';
  pId := nil;

  Result := CoCreateInstance(CLSID_MMDeviceEnumerator,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IID_IMMDeviceEnumerator,
                             pEnumerator);
  if FAILED(Result) then
    Exit;

  // Render endpoints, active only (match typical â€œdevice listâ€ UI expectations)
  Result := pEnumerator.EnumAudioEndpoints(eRender,
                                           DEVICE_STATE_ACTIVE,
                                           pCollection);
  if FAILED(Result) then
    Exit;

  Result := pCollection.GetCount(count);
  if FAILED(Result) then
    Exit;

  if (AIndex < 0) or (UINT(AIndex) >= count) then
    Exit(E_INVALIDARG);

  Result := pCollection.Item(UINT(AIndex), pDevice);
  if FAILED(Result) then
    Exit;

  Result := pDevice.GetId(pId);
  if SUCCEEDED(Result) then
    begin
      ADeviceId := pId;
      CoTaskMemFree(pId);
    end;
end;


function TWasApiEngine.InitializeAudioEngine(): HRESULT;
var
  hr: HRESULT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

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

  // Select playback device.
  if FUseDefaultDevice then
    begin

      hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                                eMultimedia,
                                                pDevice);
    end
  else
    begin

      // If DeviceID already provided, use it directly.
      if (FDeviceId <> '') then
        begin

          hr := pEnumerator.GetDevice(PWideChar(FDeviceId),
                                      pDevice);
        end
      else
        begin

          // Fallback to device index.
          hr := GetDeviceIdByIndex(FDeviceIndex,
                                   FDeviceId);

          if SUCCEEDED(hr) then
            hr := pEnumerator.GetDevice(PWideChar(FDeviceId),
                                        pDevice);
        end;
    end;

  // Fallback to default multimedia if explicit selection fails
  if FAILED(hr) then
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

  // Do NOT call GetSimpleAudioVolume(@FSessionGuid, ...) here:
  // the deck session does not exist until pvAudioClient.Initialize(..., @FSessionGuid) in SetFormat.
  pvSimpleVol := nil;
  Result := hr;
end;


function TWasApiEngine.InitializeCueAudioEngine(): HRESULT;
var
  hr: HRESULT;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

begin

  // Release any previous cue client
  pvCueAudioClient := nil;
  pvCueRenderClient := nil;
  pvCueBufferFrameCount := 0;

  // (Optional) event not used in this "master-driven" mirroring design
  if (pvCueAudioSamplesReadyEvent <> 0) then
    begin
      CloseHandle(pvCueAudioSamplesReadyEvent);
      pvCueAudioSamplesReadyEvent := 0;
    end;

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    Exit(hr);

  // Select cue endpoint
  if FCueUseDefaultDevice then
    begin
      hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                                FCueDeviceRole,
                                                pDevice);
    end
  else
    begin
      hr := pEnumerator.GetDevice(PWideChar(FCueDeviceId),
                                  pDevice);
    end;

  // Fallback to default multimedia if explicit id fails
  if FAILED(hr) then
    hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                              eMultimedia,
                                              pDevice);
  if FAILED(hr) then
    Exit(hr);

  hr := pDevice.Activate(IID_IAudioClient,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pvCueAudioClient));
  if FAILED(hr) then
    Exit(hr);

  Result := hr;
end;


function TWasApiEngine.SetCueFormat(pwfx: PWAVEFORMATEX): HRESULT;
var
  hr: HRESULT;
  hnsRequestedDuration: REFERENCE_TIME;
  bufferFrameCount: UINT32;

begin

  if (pvCueAudioClient = nil) then
    Exit(E_POINTER);

  if (pwfx = nil) then
    Exit(E_POINTER);

  hnsRequestedDuration := REFTIMES_PER_SEC;

  // Cue output is "master-driven" (we push when master produces audio),
  // so we don't need event callbacks here.
  hr := pvCueAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                    AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or
                                    AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
                                    hnsRequestedDuration,
                                    0,
                                    pwfx,
                                    nil);
  if FAILED(hr) then
    Exit(hr);

  hr := pvCueAudioClient.GetBufferSize(bufferFrameCount);
  if FAILED(hr) then
    Exit(hr);

  pvCueBufferFrameCount := bufferFrameCount;

  hr := pvCueAudioClient.GetService(IID_IAudioRenderClient,
                                    pvCueRenderClient);
  if FAILED(hr) then
    Exit(hr);


  // Cue session volume/mute (per-session)
  pvCueSimpleVol := nil;
  pvCueAudioClient.GetService(IID_ISimpleAudioVolume,
                              pvCueSimpleVol);

  if Assigned(pvCueSimpleVol) then
    begin

      pvCueSimpleVol.SetMute(FCueMuted,
                             nil);
      pvCueSimpleVol.SetMasterVolume(FCueVolume,
                                     nil);
    end;

  Result := S_OK;
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

  hr := pvAudioClient.GetService(IID_ISimpleAudioVolume,
                                 pvSimpleVol);
  if FAILED(hr) then
    Exit(hr);

  if Assigned(pvSimpleVol) then
    begin

      pvSimpleVol.SetMute(False,
                          nil);
      pvSimpleVol.SetMasterVolume(1.0,
                                  nil);
    end;

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
  // Cue output
  cueHr: HRESULT;
  cueFramesPadding: UINT32;
  cueFramesCanWrite: UINT32;
  cueFramesToWrite: UINT32;
  pCueBufferData: PByte;
  cueEnabledNow: Boolean;
  u64Position: UINT64;
  u64QPCPosition: UINT64;
  u64Frequency: UINT64;
  numFramesAvailable: UINT32;
  numFramesPadding: UINT32;
  pBufferData: PByte;
  flags: DWORD;
  Cmd: TEngineCommand;

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

  // Prevents XE bug.
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                    exOverflow, exUnderflow, exPrecision]);

  // Cache cue flag early (used in error paths)
  cueEnabledNow := FCueEnabled;

  // Become MMCSS.
  pvMmcssHandle := AvSetMmThreadCharacteristics(PWideChar('Audio'),
                                                @pvMmcssTaskIndex);

  // If we are resuming from Pause, keep the current byte offset.
  // Otherwise start from the beginning.
  if (pvDeviceState <> dsPause) then
    begin

      FOffset := 0;
      FBasePos100ns := 0;

      // Tempo/pitch
      FReadPos := 0.0;
      if (FRateTarget = 0.0) then
        FRateTarget := 1.0;
      FRateSmooth := FRateTarget;
    end;

  FRequestStop := False;
  FRequestPause := False;

  InterlockedExchange(FRunning,
                      1);
  FStoppedEvent.ResetEvent;
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
      if cueEnabledNow and (pvCueAudioClient <> nil) then
        pvCueAudioClient.Stop();
      SetState(dsStop);
      RaiseError('IAudioClock.GetFrequency failed', hr);
      Exit(hr);
    end;

  // Optional Cue/PFL output (mirrors the master stream to a second endpoint)

  if cueEnabledNow then
    begin

      // Ensure cue client is created and configured.
      if (pvCueAudioClient = nil) or (pvCueRenderClient = nil) then
        begin

          cueHr := InitializeCueAudioEngine();
          if SUCCEEDED(cueHr) then
            cueHr := SetCueFormat(pvSourceWfx);

          if FAILED(cueHr) then
            begin

              // Fail soft: Disable cue and keep master playing.
              cueEnabledNow := False;
              pvCueAudioClient := nil;
              pvCueRenderClient := nil;
              pvCueBufferFrameCount := 0;
              RaiseError('Cue output init failed (disabled)', cueHr);
            end;
        end;

      if cueEnabledNow and (pvCueAudioClient <> nil) then
        begin

          cueHr := pvCueAudioClient.Start();
          if FAILED(cueHr) then
            begin

              cueEnabledNow := False;
              pvCueAudioClient := nil;
              pvCueRenderClient := nil;
              pvCueBufferFrameCount := 0;
              RaiseError('Cue IAudioClient.Start failed (disabled)', cueHr);
            end;
        end;
    end;

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

                // Route *all* supported commands through the single implementation
                ProcessControlCommand(Cmd);

                // Keep the hard shutdown now shortcut if you want
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
                if cueEnabledNow and (pvCueAudioClient <> nil) then
                  pvCueAudioClient.Stop();

                pvAudioClient.Reset();

                if cueEnabledNow and (pvCueAudioClient <> nil) then
                  begin
                    pvCueAudioClient.Stop();
                    pvCueAudioClient.Reset();
                  end;

                // Reset playback position.
                FOffset := 0;
                FBasePos100ns := 0;

                // Tell GUI immediately.
                RaiseProcessed(0,
                               0);

                SetState(dsStop);
                RaiseEnded();
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

                // Source of audio:
                // 1) external fill callback (used by shared master mixer), or
                // 2) engine's own decoded file path.
                if Assigned(FOnFillPcm) then
                  begin
                    if (pvRenderWfx <> nil) then
                      hr := FOnFillPcm(Self,
                                       pBufferData,
                                       DWORD(numFramesAvailable) * DWORD(FClientBlockAlign),
                                       {pvRenderWfx} pvSourceWfx,
                                       flags)
                    else
                      hr := E_POINTER;
                  end
                else
                  hr := LoadData(numFramesAvailable,
                                 pBufferData,
                                 flags);

                // DEBUG : Check if we are only playing silence.
                //if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                //  OutputDebugString('LoadData: SILENT');

                // After GetBuffer succeeds, ReleaseBuffer MUST be called with the same frame count.
                // If LoadData failed, release as SILENT for safety.
                if FAILED(hr) then
                  begin

                    pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                                 AUDCLNT_BUFFERFLAGS_SILENT);
                    Break;
                  end;

                // Peakmeter pre/post.
                if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
                  MeterPublishFloat32(PSingle(Pointer(pBufferData)),
                                      Integer(numFramesAvailable),
                                      pvSourceWfx.nChannels);

                // -------------------------------------------------------------
                // FX chain hook (generic IMFTransform list)
                // Process only when not silent.
                // ByteCount = frames * blockAlign (render format).
                // -------------------------------------------------------------
                if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
                  begin

                    // -----------------------------------------------------------------------
                    // Optional PCM callback (e.g. TMfWasApiEffectsRack). Runs on engine thread.
                    // NOTE: Call it even for silent buffers so the rack can keep state / meters in sync.
                    // -----------------------------------------------------------------------
                    if Assigned(FOnProcessPcm) then
                      begin

                        try

                          FOnProcessPcm(Self,
                                        pBufferData,
                                        DWORD(numFramesAvailable) * DWORD(FClientBlockAlign),
                                        pvSourceWfx);
                         // DEBUG: Print the wav render format.
                         //DebugWfx('TWasApiEngine.PlayAudioStreamInternal', pvRenderWfx);
                        except

                          on E: Exception do
                            begin
                              // Fail-safe: silence this buffer and report.
                              flags := flags or AUDCLNT_BUFFERFLAGS_SILENT;
                              RaiseError('OnProcessPcm exception: ' + E.Message,
                                         E_FAIL);
                             end;
                        end;
                      end
                    else
                      begin

                        // Debug aid: If we expect rack processing but nothing happens,
                        // this tells us the hook is missing.
                        // OutputDebugString('WASAPIEngine: OnProcessPcm not assigned');
                      end;

                    hr := ProcessEffectsBuffer(pBufferData,
                                               DWORD(numFramesAvailable) * DWORD(FClientBlockAlign));
                    if FAILED(hr) then
                      begin

                        // Safety: release as silent to avoid playing partially-processed garbage.
                        pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                                     AUDCLNT_BUFFERFLAGS_SILENT);
                        Break;
                      end;

                    // Peakmeter pre/post = post.
                    if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
                      MeterPublishFloat32(PSingle(Pointer(pBufferData)),
                                          Integer(numFramesAvailable),
                                          pvSourceWfx.nChannels);

                    // -------------------------------------------------------------
                    // Per-engine post-FX output hook (engine thread)
                    // Buffer is in *render* format, so pass pvRenderWfx.
                    // -------------------------------------------------------------
                    if Assigned(FOnOutputPcm) then
                      begin

                        try

                          if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                            FOnOutputPcm(Self,
                                         nil,
                                         0,
                                         pvSourceWfx{pvRenderWfx})
                          else
                            FOnOutputPcm(Self,
                                         pBufferData,
                                         DWORD(numFramesAvailable) * DWORD(FClientBlockAlign),
                                         pvSourceWfx{pvRenderWfx});

                          // DEBUG: Print the wav render format.
                          //DebugWfx('TWasApiEngine.PlayAudioStreamInternal: Render format', pvRenderWfx);
                          //DebugWfx('TWasApiEngine.PlayAudioStreamInternal: Source format', pvSourceWfx);
                          except
                          on E: Exception do
                            begin
                              // Fail-safe: silence this buffer and report.
                              flags := flags or AUDCLNT_BUFFERFLAGS_SILENT;
                              RaiseError('OnOutputPcm exception: ' + E.Message,
                                         E_FAIL);
                            end;
                        end;
                      end;
                  end;
                  // -----------------------------------------------------------


                // -------------------------------------------------------------
                // Mirror to Cue/PFL output (best-effort).
                // We drive cue from the master render cadence. If cue device runs
                // at a slightly different clock, it may occasionally under/over-run.
                // For DJ cueing this is acceptable; master is the timing leader.
                // -------------------------------------------------------------
                if cueEnabledNow and (pvCueAudioClient <> nil) and (pvCueRenderClient <> nil) then
                  begin

                    cueHr := pvCueAudioClient.GetCurrentPadding(cueFramesPadding);
                    if SUCCEEDED(cueHr) then
                      begin

                        cueFramesCanWrite := pvCueBufferFrameCount - cueFramesPadding;

                        // Try to write the same number of frames as the master just rendered.
                        cueFramesToWrite := numFramesAvailable;
                        if (cueFramesToWrite > cueFramesCanWrite) then
                          cueFramesToWrite := cueFramesCanWrite;

                        if (cueFramesToWrite > 0) then
                          begin

                            pCueBufferData := nil;
                            cueHr := pvCueRenderClient.GetBuffer(cueFramesToWrite,
                                                                pCueBufferData);
                            if SUCCEEDED(cueHr) then
                              begin

                                if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                                  ZeroMemory(pCueBufferData,
                                             DWORD(cueFramesToWrite) * DWORD(FClientBlockAlign))
                                else
                                  Move(pBufferData^,
                                       pCueBufferData^,
                                       DWORD(cueFramesToWrite) * DWORD(FClientBlockAlign));

                                pvCueRenderClient.ReleaseBuffer(cueFramesToWrite,
                                                                flags);
                              end;
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

                FBasePos100ns := 0;
                RaiseProcessed(0,
                               0);
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
  if cueEnabledNow and (pvCueAudioClient <> nil) then
    pvCueAudioClient.Stop();

  if (pvMmcssHandle <> 0) then
    begin

      AvRevertMmThreadCharacteristics(pvMmcssHandle);
      pvMmcssHandle := 0;
      pvMmcssTaskIndex := 0;
    end;

  InterlockedExchange(FRunning,
                      0);
  RaiseProcessed(0,
                 0);

  FStoppedEvent.SetEvent;

  Result := hr;
end;

end.
