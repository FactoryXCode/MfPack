// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfGuiRefreshTimer.pas
// Kind: Pascal Unit
// Release date: 05-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Shared worker scheduler for coalesced VCL refresh events.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/11/2026 All                 One worker, one pending GUI message, safe removal.
//------------------------------------------------------------------------------
//
// Remarks: Create, configure and destroy components on the VCL main thread.
//          OnTimer also runs on the VCL main thread. Not an audio clock.
// Compiler version: Delphi XE7 or higher.
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
unit MfGuiRefreshTimer;

interface

uses

  {System}
  System.Classes;

type

  TMfGuiRefreshTimer = class(TComponent)
  private

    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FRegistration: UInt64;

    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure UpdateTimer();
    procedure DoTimer();

  protected

    procedure Loaded(); override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

  published

    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

procedure Register();

implementation

uses

  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  {Vcl}
  Vcl.Forms;

const

  WM_MF_GUI_REFRESH = WM_APP + $379;

type

  TMfRefreshEntry = class
  public

    Registration: UInt64;
    Timer: TMfGuiRefreshTimer;
    Interval: Cardinal;
    NextDue: UInt64;
    Due: Boolean;
    Executing: Boolean;
  end;

  TMfRefreshScheduler = class;

  TMfRefreshThread = class(TThread)
  private

    FScheduler: TMfRefreshScheduler;

  protected

    procedure Execute(); override;

  public

    constructor Create(AScheduler: TMfRefreshScheduler);
  end;

  TMfRefreshScheduler = class
  private

    FLock: TCriticalSection;
    FEntries: TObjectList<TMfRefreshEntry>;
    FWorker: TMfRefreshThread;
    FWakeEvent: THandle;
    FWindow: HWND;
    FNextRegistration: UInt64;
    FMessagePending: Boolean;
    FDispatching: Boolean;

    function FindEntry(const ARegistration: UInt64): TMfRefreshEntry;
    function Advance(): Cardinal;
    procedure DispatchTimers();
    procedure WindowProc(var Message: TMessage);

  public

    constructor Create();
    destructor Destroy(); override;
    function AddTimer(ATimer: TMfGuiRefreshTimer;
                        const AInterval: Cardinal): UInt64;
    procedure RemoveTimer(const ARegistration: UInt64);
  end;

var

  RefreshScheduler: TMfRefreshScheduler;
  RefreshShuttingDown: Boolean;

function RefreshTickCount(): UInt64; stdcall;
  external 'kernel32.dll' name 'GetTickCount64';


procedure CheckMainThread();
begin

  if (GetCurrentThreadId() <> MainThreadID) then
    raise EInvalidOperation.Create('Configure GUI refresh timers on the VCL main thread.');
end;


procedure Register();
begin

  RegisterComponents('MfPack Timer Samples',
                       [TMfGuiRefreshTimer]);
end;


constructor TMfRefreshThread.Create(AScheduler: TMfRefreshScheduler);
begin

  inherited Create(True);
  FreeOnTerminate := False;
  FScheduler := AScheduler;
end;


procedure TMfRefreshThread.Execute();
var
  WaitMs: Cardinal;

begin

  while not Terminated do
    begin

      WaitMs := FScheduler.Advance();
      WaitForSingleObject(FScheduler.FWakeEvent,
                            WaitMs);
    end;
end;


constructor TMfRefreshScheduler.Create();
begin

  inherited Create();
  FLock := TCriticalSection.Create();
  FEntries := TObjectList<TMfRefreshEntry>.Create(True);
  FWakeEvent := CreateEvent(nil,
                              False,
                              False,
                              nil);
  if (FWakeEvent = 0) then
    RaiseLastOSError();

  FWindow := AllocateHWnd(WindowProc);
  FWorker := TMfRefreshThread.Create(Self);
  FWorker.Start();
end;


destructor TMfRefreshScheduler.Destroy();
begin

  if Assigned(FWorker) then
    begin

      FWorker.Terminate();
      SetEvent(FWakeEvent);
      // The worker never synchronizes with, or waits for, the main thread.
      WaitForSingleObject(FWorker.Handle,
                            INFINITE);
      FreeAndNil(FWorker);
    end;

  if (FWindow <> 0) then
    DeallocateHWnd(FWindow);
  if (FWakeEvent <> 0) then
    CloseHandle(FWakeEvent);

  FreeAndNil(FEntries);
  FreeAndNil(FLock);
  inherited Destroy();
end;


function TMfRefreshScheduler.FindEntry(const ARegistration: UInt64): TMfRefreshEntry;
var
  Entry: TMfRefreshEntry;

begin

  Result := nil;
  for Entry in FEntries do
    if (Entry.Registration = ARegistration) then
      Exit(Entry);
end;


function TMfRefreshScheduler.AddTimer(ATimer: TMfGuiRefreshTimer;
                                       const AInterval: Cardinal): UInt64;
var
  Entry: TMfRefreshEntry;

begin

  Entry := TMfRefreshEntry.Create();
  try

    FLock.Acquire();
    try

      Inc(FNextRegistration);
      Entry.Registration := FNextRegistration;
      Entry.Timer := ATimer;
      Entry.Interval := AInterval;
      Entry.NextDue := RefreshTickCount() + UInt64(AInterval);
      Result := Entry.Registration;
      FEntries.Add(Entry);
      Entry := nil;
    finally

      FLock.Release();
    end;
  finally

    Entry.Free();
  end;

  SetEvent(FWakeEvent);
end;


procedure TMfRefreshScheduler.RemoveTimer(const ARegistration: UInt64);
var
  Entry: TMfRefreshEntry;

begin

  FLock.Acquire();
  try

    Entry := FindEntry(ARegistration);
    if Assigned(Entry) then
      FEntries.Remove(Entry);
  finally

    FLock.Release();
  end;

  SetEvent(FWakeEvent);
end;


function TMfRefreshScheduler.Advance(): Cardinal;
var
  Entry: TMfRefreshEntry;
  NowTick, Remaining: UInt64;
  AnyDue: Boolean;

begin

  Result := INFINITE;
  AnyDue := False;
  NowTick := RefreshTickCount();

  FLock.Acquire();
  try

    for Entry in FEntries do
      begin

        if Entry.Executing then
          Continue;

        if not Entry.Due then
          Entry.Due := (NowTick >= Entry.NextDue);

        if Entry.Due then
          AnyDue := True
        else
          begin

            Remaining := Entry.NextDue - NowTick;
            if (Remaining >= INFINITE) then
              Remaining := INFINITE - 1;
            if (Remaining < Result) then
              Result := Cardinal(Remaining);
          end;
      end;

    if AnyDue and not FMessagePending and not FDispatching then
      begin

        FMessagePending := PostMessage(FWindow,
                                        WM_MF_GUI_REFRESH,
                                        0,
                                        0);
        // Retry a full Windows message queue without spinning.
        if not FMessagePending and (Result > 100) then
          Result := 100;
      end;
  finally

    FLock.Release();
  end;
end;


procedure TMfRefreshScheduler.DispatchTimers();
var
  DueRegistrations: TArray<UInt64>;
  Entry: TMfRefreshEntry;
  Timer: TMfGuiRefreshTimer;
  Registration: UInt64;
  Count: Integer;

begin

  FLock.Acquire();
  try

    if FDispatching then
      Exit;

    FMessagePending := False;
    FDispatching := True;
    SetLength(DueRegistrations,
                 FEntries.Count);
    Count := 0;

    for Entry in FEntries do
      if Entry.Due then
        begin

          DueRegistrations[Count] := Entry.Registration;
          Inc(Count);
        end;

    SetLength(DueRegistrations,
                 Count);
  finally

    FLock.Release();
  end;

  try

    for Registration in DueRegistrations do
      begin

        Timer := nil;
        FLock.Acquire();
        try

          Entry := FindEntry(Registration);
          if Assigned(Entry) and Entry.Due then
            begin

              Entry.Due := False;
              Entry.Executing := True;
              Timer := Entry.Timer;
            end;
        finally

          FLock.Release();
        end;

        if not Assigned(Timer) then
          Continue;

        try

          try

            Timer.DoTimer();
          except

            Application.HandleException(Self);
          end;
        finally

          FLock.Acquire();
          try

            // A callback may disable/free itself or another timer.
            // Re-resolve its registration; never dereference Timer here.
            Entry := FindEntry(Registration);
            if Assigned(Entry) then
              begin

                Entry.Executing := False;
                Entry.NextDue := RefreshTickCount() + UInt64(Entry.Interval);
              end;
          finally

            FLock.Release();
          end;
        end;
      end;
  finally

    FLock.Acquire();
    try

      FDispatching := False;
    finally

      FLock.Release();
    end;

    SetEvent(FWakeEvent);
  end;
end;


procedure TMfRefreshScheduler.WindowProc(var Message: TMessage);
begin

  if (Message.Msg = WM_MF_GUI_REFRESH) then
    begin

      Message.Result := 0;
      DispatchTimers();
    end
  else
    Message.Result := DefWindowProc(FWindow,
                                      Message.Msg,
                                      Message.WParam,
                                      Message.LParam);
end;


constructor TMfGuiRefreshTimer.Create(AOwner: TComponent);
begin

  CheckMainThread();
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
end;


destructor TMfGuiRefreshTimer.Destroy();
begin

  CheckMainThread();
  FEnabled := False;
  if Assigned(RefreshScheduler) and (FRegistration <> 0) then
    RefreshScheduler.RemoveTimer(FRegistration);
  FRegistration := 0;
  inherited Destroy();
end;


procedure TMfGuiRefreshTimer.Loaded();
begin

  inherited Loaded();
  UpdateTimer();
end;


procedure TMfGuiRefreshTimer.UpdateTimer();
begin

  if Assigned(RefreshScheduler) and (FRegistration <> 0) then
    RefreshScheduler.RemoveTimer(FRegistration);
  FRegistration := 0;

  if RefreshShuttingDown or
     (csDesigning in ComponentState) or
     (csLoading in ComponentState) or
     (csDestroying in ComponentState) then
    Exit;

  if FEnabled and (FInterval > 0) and Assigned(FOnTimer) then
    begin

      if not Assigned(RefreshScheduler) then
        RefreshScheduler := TMfRefreshScheduler.Create();
      FRegistration := RefreshScheduler.AddTimer(Self,
                                                  FInterval);
    end;
end;


procedure TMfGuiRefreshTimer.SetEnabled(const Value: Boolean);
begin

  CheckMainThread();
  if (FEnabled = Value) then
    Exit;

  FEnabled := Value;
  UpdateTimer();
end;


procedure TMfGuiRefreshTimer.SetInterval(const Value: Cardinal);
begin

  CheckMainThread();
  if (FInterval = Value) then
    Exit;

  FInterval := Value;
  UpdateTimer();
end;


procedure TMfGuiRefreshTimer.SetOnTimer(const Value: TNotifyEvent);
begin

  CheckMainThread();
  FOnTimer := Value;
  UpdateTimer();
end;


procedure TMfGuiRefreshTimer.DoTimer();
var
  Handler: TNotifyEvent;

begin

  Handler := FOnTimer;
  if FEnabled and Assigned(Handler) then
    Handler(Self);
end;

initialization

  System.Classes.RegisterClass(TMfGuiRefreshTimer);

finalization

  RefreshShuttingDown := True;
  FreeAndNil(RefreshScheduler);
  System.Classes.UnRegisterClass(TMfGuiRefreshTimer);

end.
