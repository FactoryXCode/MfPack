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
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
  WasApiEngine;

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

    // Internal forwarders (engine -> component)
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
    function Pause(): HRESULT;
    function SeekTo(const Pos100ns: Int64): HRESULT;
    function SetVolumes(pVolLeft,
                        pVolRight: Single): HRESULT;

    property Engine: TWasApiEngine read FEngine;

    property DeviceState: TDeviceState read GetDeviceState;
    property SoundChannels: Word read GetSoundChannels;

  published

    // Rack (optional). If assigned, it will be called for each PCM block on the engine thread.
    property EffectsRack: TMfWasApiEffectsRack read FRack write SetRack;

    property OnStateChanged: TWasApiStateEvent read FOnStateChanged write FOnStateChanged;
    property OnError: TWasApiErrorEvent read FOnError write FOnError;
    property OnReady: TWasApiReadyEvent read FOnReady write FOnReady;
    property OnProcessed: TWasApiProcessedEvent read FOnProcessed write FOnProcessed;
    property OnEnded: TWasApiEndedEvent read FOnEnded write FOnEnded;
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

      // Best effort stop before freeing (thread is joined in engine destructor).
      try

        FEngine.Stop();
      except
        // ignore during shutdown
      end;
  end;

  FreeAndNil(FEngine);
  inherited;
end;


function TMfWasApiPlayerEngine.CanRaiseToOwner: Boolean;
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


procedure TMfWasApiPlayerEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin

  inherited Notification(AComponent,
                         Operation);

  if (Operation = opRemove) and (AComponent = FRack) then
    SetRack(nil);
end;


procedure TMfWasApiPlayerEngine.CheckForEngine();
begin

  if Assigned(FEngine) then
    Exit;

  if (csDesigning in ComponentState) then
    Exit;

  FEngine := TWasApiEngine.Create();

  // Forward engine events to the component's published events
  FEngine.OnStateChanged := EngineStateChanged;
  FEngine.OnError := EngineError;
  FEngine.OnReady := EngineReady;
  FEngine.OnProcessed := EngineProcessed;
  FEngine.OnEnded := EngineEnded;

  // Wire rack if already set
  SyncRackHook();
end;


procedure TMfWasApiPlayerEngine.SyncRackHook();
begin

  // Keep hook stable even if something overwrites it.
  if not Assigned(FEngine) then
    Exit;

  if Assigned(FRack) then
    FEngine.OnProcessPcm := FRack.ProcessPcm   // << No params!
  else
    FEngine.OnProcessPcm := nil;
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

  // Wire into underlying engine (runtime only)
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

  CheckForEngine();

  if not Assigned(FEngine) then
    Exit(E_FAIL);

  SyncRackHook();
  Result := FEngine.Stop();
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


end.
