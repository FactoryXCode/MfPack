// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfWasApiPlayerEngineComponent.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: WasApiEngine component.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
unit MfWasApiPlayerEngineComponent;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  {Application}
  MfWasApiEffectsRack,
  WasApiEngine,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.MMDeviceApi,
  {WinMM}
  WinApi.WinMM.MMeApi;

const

  HNS_PER_100MS = 1000000;
  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;

type

  TMfWasApiPlayerEngine = class(TComponent)
  private

    FEngine: TWasApiEngine;
    FRack: TMfWasApiEffectsRack;

    // Events (component-facing)
    FOnStateChanged: TWasApiStateEvent;
    FOnError: TWasApiErrorEvent;
    FOnReady: TWasApiReadyEvent;
    FOnProcessed: TWasApiProcessedEvent;
    FOnEnded: TWasApiEndedEvent;

    // Threaded PCM callbacks (engine thread)
    FOnOutputPcm: TOnOutputPcm;
    FOnProcessPcm: TWasApiProcessPcmEvent;

    FStopping: Integer; // 0/1 (atomic)

    // Device selection / switching
    FUseDefaultDevice: Boolean;
    //FDeviceRole: ERole;          // default endpoint role when UseDefaultDevice = True
    FDeviceId: string;           // IMMDevice ID when UseDefaultDevice = False

    // Internal forwarders (engine -> component)
    procedure AttachEngineCallbacks();

    procedure EngineStateChanged(Sender: TObject;
                                 const NewState: TDeviceState);

    procedure EngineError(Sender: TObject;
                          const Msg: string;
                          const Hr: HRESULT);

    procedure EngineReady(Sender: TObject);

    procedure EngineProcessed(Sender: TObject;
                              const Position100ns: Int64;
                              const RawPosition: UInt64);

    procedure EngineEnded(Sender: TObject);

    // Threaded PCM forwarding (engine thread)
    procedure EngineOutputPcm(Sender: TObject;
                              pData: PByte;
                              const ByteCount: DWORD;
                              Wfx: PWAVEFORMATEX);

    procedure EngineProcessPcm(Sender: TObject;
                               pData: PByte;
                               const ByteCount: DWORD;
                               pwfx: PWAVEFORMATEX);

    // Prevent callbacks into an owner/form that is already in the destroy path.
    function CanRaiseToOwner: Boolean;

    procedure CheckForEngine();
    procedure SyncRackHook();
    function GetDeviceState(): TDeviceState;
    function GetSoundChannels(): Word;

    procedure SetRack(const Value: TMfWasApiEffectsRack);

  protected

    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Commands (forwarded)
    function OpenFile(const audiofile: TFileName;
                      fileDuration100ns: LONGLONG): HRESULT;
    function Start(): HRESULT;
    function Stop(): HRESULT;
    function WaitForStop(TimeoutMs: DWORD = 100): HRESULT;
    function Pause(): HRESULT;
    function SeekTo(const Pos100ns: Int64): HRESULT;
    function SetVolumes(pVolLeft,
                        pVolRight: Single): HRESULT;

    function Mute(pActive: Boolean): Boolean;

    // Calculated gains for the post fader VU meters.
    procedure SetMeterFaderGains(const GainL,
                                 GainR: Single);
    procedure GetMeterFaderGains(out GainL,
                                 GainR: Single);

    // Output device switching
    function SwitchOutputDevice(const ADeviceId: string;
                                const AUseDefaultDevice: Boolean = True;
                                const ARole: ERole = eMultimedia;
                                const AutoResume: Boolean = True): HRESULT;
    procedure SetUseDefaultOutputDevice(const ARole: ERole = eMultimedia);
    procedure SetOutputDeviceId(const ADeviceId: string);

    // FX
    procedure ClearEffects();
    procedure AddEffect(const Mft: IMFTransform);
    procedure SetEffects(const Effects: array of IMFTransform);

    // Pitch / tempo
    procedure SetPitchPercent(const Pct: Double);
    procedure SetPitchSlider(const SliderPos: Integer);

    // Pitch properties (forwarded)
    function GetPitchRangePct: Double;
    procedure SetPitchRangePct(const Value: Double);
    function GetPitchDetentPct: Double;
    procedure SetPitchDetentPct(const Value: Double);
    function GetPitchAutoZeroPct: Double;
    procedure SetPitchAutoZeroPct(const Value: Double);
    function GetPitchRampMs: Integer;
    procedure SetPitchRampMs(const Value: Integer);
    function GetVarispeedEnabled: LongBool;
    procedure SetVarispeedEnabled(const Value: LongBool);

  published

    property Engine: TWasApiEngine read FEngine;

    // Convenience aliases
    property State: TDeviceState read GetDeviceState;

    property DeviceState: TDeviceState read GetDeviceState;
    property SoundChannels: Word read GetSoundChannels;

    // Audio Device-------------------------------------------------------------
    property UseDefaultDevice: Boolean read FUseDefaultDevice write FUseDefaultDevice default True;
    property DeviceId: string read FDeviceId write FDeviceId;
    // -------------------------------------------------------------------------

    // Rack (optional). If assigned, it will be called for each PCM block on the engine thread.
    property EffectsRack: TMfWasApiEffectsRack read FRack write SetRack;

    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnReady: TWasApiReadyEvent read FOnReady write FOnReady;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
    property OnEnded: TWasApiEndedEvent read FOnEnded write FOnEnded;

    // PCM callbacks (engine thread)
    property OnOutputPcm: TOnOutputPcm read FOnOutputPcm write FOnOutputPcm;
    property OnProcessPcm: TWasApiProcessPcmEvent read FOnProcessPcm write FOnProcessPcm;

    // Pitch properties (forwarded)
    property PitchRangePct: Double read GetPitchRangePct write SetPitchRangePct;
    property PitchDetentPct: Double read GetPitchDetentPct write SetPitchDetentPct;
    property PitchAutoZeroPct: Double read GetPitchAutoZeroPct write SetPitchAutoZeroPct;
    property PitchRampMs: Integer read GetPitchRampMs write SetPitchRampMs;
    property VarispeedEnabled: LongBool read GetVarispeedEnabled write SetVarispeedEnabled;
  end;

procedure Register;

implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfWasApiPlayerEngine]);
end;

{ TMfWasApiPlayerEngine }

constructor TMfWasApiPlayerEngine.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FEngine := nil;
  FRack := nil;
end;


destructor TMfWasApiPlayerEngine.Destroy();
begin

  // During application shutdown, queued engine callbacks may still arrive on the UI thread.
  // Ensure we never call into an owner/form that is already destroying.
  FOnStateChanged := nil;
  FOnError := nil;
  FOnReady := nil;
  FOnProcessed := nil;
  FOnEnded := nil;

  if Assigned(FEngine) then
    begin

      // Detach engine callbacks first so any already queued UI callbacks become no-ops.
      FEngine.OnStateChanged := nil;
      FEngine.OnError := nil;
      FEngine.OnReady := nil;
      FEngine.OnProcessed := nil;
      FEngine.OnEnded := nil;
      FEngine.OnProcessPcm := nil;

      // Best effort stop + join before freeing to avoid thread handle leaks.
      try

        FEngine.Stop();
        // Wait a bit for the engine thread to stop.
        FEngine.WaitForStop(5000);
      except
        // ignore during shutdown
      end;
  end;

  FreeAndNil(FEngine);
  inherited;
end;


function TMfWasApiPlayerEngine.CanRaiseToOwner(): Boolean;
begin

  // Never raise events at design-time.
  if (csDesigning in ComponentState) then
    Exit(False);

  // If this component is being destroyed, ignore callbacks.
  if (csDestroying in ComponentState) then
    Exit(False);

  // If the owning form/component is being destroyed, don't call user handlers.
  if Assigned(Owner) and (csDestroying in Owner.ComponentState) then
    Exit(False);

  Result := True;
end;


procedure TMfWasApiPlayerEngine.Loaded();
begin

  inherited;

  if (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
    CheckForEngine();
end;


procedure TMfWasApiPlayerEngine.Notification(AComponent: TComponent;
                                             Operation: TOperation);
begin

  inherited Notification(AComponent,
                         Operation);

  if (Operation = opRemove) and (AComponent = FRack) then
    SetRack(nil);
end;


procedure TMfWasApiPlayerEngine.AttachEngineCallbacks;
begin

  if not Assigned(FEngine) then
    Exit;

  // Always wire internal engine -> component handlers
  FEngine.OnStateChanged := EngineStateChanged;
  FEngine.OnError := EngineError;
  FEngine.OnReady := EngineReady;
  FEngine.OnProcessed := EngineProcessed;
  FEngine.OnEnded := EngineEnded;
end;


procedure TMfWasApiPlayerEngine.CheckForEngine();
begin

  if not Assigned(FEngine) then
    FEngine := TWasApiEngine.Create;

  AttachEngineCallbacks();  // Always ensure correct wiring.

  if (csDesigning in ComponentState) then
    Exit;

  //FEngine := TWasApiEngine.Create();

  // Forward engine events to the component's published events
  AttachEngineCallbacks();

  // Wire rack/user PCM hooks
  SyncRackHook();
end;


procedure TMfWasApiPlayerEngine.SyncRackHook();
begin

  if not Assigned(FEngine) then
    Exit;

  // Always keep engine callbacks routed through the component so we can chain:
  // Rack -> user event handler.
  FEngine.OnOutputPcm := EngineOutputPcm;
  FEngine.OnProcessPcm := EngineProcessPcm;
end;

procedure TMfWasApiPlayerEngine.SetRack(const Value: TMfWasApiEffectsRack);
begin

  if (FRack = Value) then
    Exit;

  if Assigned(FRack) then
    FRack.RemoveFreeNotification(Self);

  FRack := Value;

  if Assigned(FRack) then
    FRack.FreeNotification(Self);

  // Keep PCM hook chain active
  if Assigned(FEngine) then
    SyncRackHook();
end;


function TMfWasApiPlayerEngine.GetDeviceState(): TDeviceState;
begin

  if Assigned(FEngine) then
    Result := FEngine.DeviceState
  else
    Result := dsUninitialized;
end;


function TMfWasApiPlayerEngine.GetSoundChannels(): Word;
begin

  if Assigned(FEngine) then
    Result := FEngine.SoundChannels
  else
    Result := 0;
end;


// ---- engine -> component event forwarders ----

procedure TMfWasApiPlayerEngine.EngineStateChanged(Sender: TObject; const NewState: TDeviceState);
begin

  if not CanRaiseToOwner then
    Exit;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self,
                    NewState);
end;


procedure TMfWasApiPlayerEngine.EngineError(Sender: TObject;
                                           const Msg: string;
                                           const Hr: HRESULT);
begin

  if not CanRaiseToOwner then
    Exit;

  if Assigned(FOnError) then
    FOnError(Self,
             Msg,
             Hr);
end;


procedure TMfWasApiPlayerEngine.EngineReady(Sender: TObject);
begin

  if not CanRaiseToOwner then
    Exit;

  if Assigned(FOnReady) then
    FOnReady(Self);
end;


procedure TMfWasApiPlayerEngine.EngineProcessed(Sender: TObject;
                                                const Position100ns: Int64;
                                                const RawPosition: UInt64);
begin

  if not CanRaiseToOwner then
    Exit;

  if Assigned(FOnProcessed) then
    FOnProcessed(Self,
                 Position100ns,
                 RawPosition);
end;


procedure TMfWasApiPlayerEngine.EngineEnded(Sender: TObject);
begin

  if not CanRaiseToOwner() then
    Exit;

  if Assigned(FOnEnded) then
    FOnEnded(Self);
end;




procedure TMfWasApiPlayerEngine.EngineOutputPcm(Sender: TObject;
                                               pData: PByte;
                                               const ByteCount: DWORD;
                                               Wfx: PWAVEFORMATEX);
begin
  // Note: Called on the engine thread.
  if Assigned(FOnOutputPcm) then
    FOnOutputPcm(Self,
                 pData,
                 ByteCount,
                 Wfx);
end;


procedure TMfWasApiPlayerEngine.EngineProcessPcm(Sender: TObject;
                                                pData: PByte;
                                                const ByteCount: DWORD;
                                                pwfx: PWAVEFORMATEX);
begin
  // Note: Called on the engine thread.
  // First, process FX rack (if assigned), then call the user handler.
  if Assigned(FRack) then
    FRack.ProcessPcm(Sender,
                     pData,
                     ByteCount,
                     pwfx);

  if Assigned(FOnProcessPcm) then
    FOnProcessPcm(Self,
                  pData,
                  ByteCount,
                  pwfx);
end;
// ---- public API forwarders ----

function TMfWasApiPlayerEngine.OpenFile(const audiofile: TFileName;
                                        fileDuration100ns: LONGLONG): HRESULT;
begin

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.OpenFile(audiofile,
                             fileDuration100ns);
end;


function TMfWasApiPlayerEngine.Start(): HRESULT;
begin

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.Start();
end;


function TMfWasApiPlayerEngine.Stop(): HRESULT;
begin

  // Do not auto-create the engine just to stop it.
  if not Assigned(FEngine) then
    Exit(S_FALSE);

  // IMPORTANT:
  // Do NOT detach engine callbacks here. The engine raises OnEnded/OnStateChanged
  // from its worker thread using TThread.Queue, and the component must stay wired
  // so the GUI receives the stop notification.
  //
  // Also do NOT clear OnProcessPcm here; that would permanently disconnect the FX rack
  // unless the user reassigns it manually.

  Result := FEngine.Stop();
end;


function TMfWasApiPlayerEngine.WaitForStop(TimeoutMs: DWORD = 100): HRESULT;
begin

  // Do not auto-create the engine just to wait for it.
  if not Assigned(FEngine) then
    Exit(S_FALSE);

  Result := FEngine.WaitForStop(TimeoutMs);

  // After a stop/join, ensure wiring is restored so a subsequent Start
  // continues to drive the FX rack and GUI events.
  if (Result = S_OK) then
    begin
      InterlockedExchange(FStopping,
                          0);
      AttachEngineCallbacks();
      SyncRackHook();
    end;
end;


function TMfWasApiPlayerEngine.Pause(): HRESULT;
begin

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.Pause();
end;


function TMfWasApiPlayerEngine.SeekTo(const Pos100ns: Int64): HRESULT;
begin

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.SeekTo(Pos100ns);
end;


function TMfWasApiPlayerEngine.SetVolumes(pVolLeft, pVolRight: Single): HRESULT;
begin

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.SetVolumes(pVolLeft,
                               pVolRight);
end;



function TMfWasApiPlayerEngine.Mute(pActive: Boolean): Boolean;
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(False);

  SyncRackHook();
  Result := FEngine.Mute(pActive);
end;


procedure TMfWasApiPlayerEngine.SetMeterFaderGains(const GainL, GainR: Single);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetMeterFaderGains(GainL,
                             GainR);
end;


procedure TMfWasApiPlayerEngine.GetMeterFaderGains(out GainL, GainR: Single);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    begin
      GainL := 1.0;
      GainR := 1.0;
      Exit;
    end;

  FEngine.GetMeterFaderGains(GainL,
                             GainR);
end;


function TMfWasApiPlayerEngine.SwitchOutputDevice(const ADeviceId: string;
                                                 const AUseDefaultDevice: Boolean;
                                                 const ARole: ERole;
                                                 const AutoResume: Boolean): HRESULT;
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  Result := FEngine.SwitchOutputDevice(ADeviceId,
                                      AUseDefaultDevice,
                                      ARole,
                                      AutoResume);
end;


procedure TMfWasApiPlayerEngine.SetUseDefaultOutputDevice(const ARole: ERole);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetUseDefaultOutputDevice(ARole);
end;


procedure TMfWasApiPlayerEngine.SetOutputDeviceId(const ADeviceId: string);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetOutputDeviceId(ADeviceId);
end;


procedure TMfWasApiPlayerEngine.ClearEffects();
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.ClearEffects();
end;


procedure TMfWasApiPlayerEngine.AddEffect(const Mft: IMFTransform);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.AddEffect(Mft);
end;


procedure TMfWasApiPlayerEngine.SetEffects(const Effects: array of IMFTransform);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetEffects(Effects);
end;


procedure TMfWasApiPlayerEngine.SetPitchPercent(const Pct: Double);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetPitchPercent(Pct);
end;


procedure TMfWasApiPlayerEngine.SetPitchSlider(const SliderPos: Integer);
begin
  CheckForEngine();

  if not Assigned(FEngine) then
    Exit;

  FEngine.SetPitchSlider(SliderPos);
end;


function TMfWasApiPlayerEngine.GetPitchRangePct: Double;
begin
  CheckForEngine();
  if Assigned(FEngine) then
    Result := FEngine.PitchRangePct
  else
    Result := 16.0;
end;

procedure TMfWasApiPlayerEngine.SetPitchRangePct(const Value: Double);
begin
  CheckForEngine();
  if Assigned(FEngine) then
    FEngine.PitchRangePct := Value;
end;

function TMfWasApiPlayerEngine.GetPitchDetentPct: Double;
begin
  CheckForEngine();
  if Assigned(FEngine) then
    Result := FEngine.PitchDetentPct
  else
    Result := 0.10;
end;

procedure TMfWasApiPlayerEngine.SetPitchDetentPct(const Value: Double);
begin
  CheckForEngine();
  if Assigned(FEngine) then
    FEngine.PitchDetentPct := Value;
end;

function TMfWasApiPlayerEngine.GetPitchAutoZeroPct: Double;
begin
  CheckForEngine();
  if Assigned(FEngine) then
    Result := FEngine.PitchAutoZeroPct
  else
    Result := 0.30;
end;

procedure TMfWasApiPlayerEngine.SetPitchAutoZeroPct(const Value: Double);
begin
  CheckForEngine();
  if Assigned(FEngine) then
    FEngine.PitchAutoZeroPct := Value;
end;

function TMfWasApiPlayerEngine.GetPitchRampMs: Integer;
begin
  CheckForEngine();
  if Assigned(FEngine) then
    Result := FEngine.PitchRampMs
  else
    Result := 50;
end;

procedure TMfWasApiPlayerEngine.SetPitchRampMs(const Value: Integer);
begin
  CheckForEngine();
  if Assigned(FEngine) then
    FEngine.PitchRampMs := Value;
end;

function TMfWasApiPlayerEngine.GetVarispeedEnabled: LongBool;
begin
  CheckForEngine();
  if Assigned(FEngine) then
    Result := FEngine.VarispeedEnabled
  else
    Result := False;
end;

procedure TMfWasApiPlayerEngine.SetVarispeedEnabled(const Value: LongBool);
begin
  CheckForEngine();
  if Assigned(FEngine) then
    FEngine.VarispeedEnabled := Value;
end;

end.
