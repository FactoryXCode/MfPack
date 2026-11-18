// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: UniThreadTimer.pas
// Kind: Pascal Unit
// Release date: 24-02-2012
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Universal threaded timer
//              This timer can be used for a variety of tasks where you need a
//              lightweight, precise and threadsave timer.
//
// Organisation: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: -
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
unit UniThreadTimer;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils;

type

  // Forwarded class
  TUniThreadedTimer = class;

  TTimerThread = class(TThread)
  private
    { Private declarations }
    FTimer: TUniThreadedTimer;
    FStopEvent: THandle;
    isActive: Boolean;

  protected
    { Protected declarations }
    procedure DoTimer();

  public
    { Public declarations }
    isDestroying: Boolean;

    constructor Create(ATimer: TUniThreadedTimer);
    destructor Destroy(); override;
    procedure Execute(); override;

  end;


  TUniThreadedTimer = class(TComponent)
  private
    { Private declarations }
    bEnabled: Boolean;
    dwPeriod: DWord;
    neOnTimer: TNotifyEvent;

    procedure SetEnabled(Value: Boolean);
    procedure SetTimerPeriod(Value: DWord);

  protected
    { Protected declarations }
    thTimerThread: TTimerThread;
    procedure UpdateTimer();

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy(); override;

  published
    { Published declarations}
    property Enabled: Boolean read bEnabled write SetEnabled;

    property Period: DWord read dwPeriod write SetTimerPeriod;
    property OnTimerEvent: TNotifyEvent read neOnTimer write neOnTimer;
  end;


implementation


// TgtTimerThread //////////////////////////////////////////////////////////////

constructor TTimerThread.Create(ATimer: TUniThreadedTimer);
begin
  inherited
  Create(True);
  FreeOnTerminate := False;
  FStopEvent := CreateEvent(nil,True,False,nil);
  isDestroying := False;
  isActive := False;
  FTimer := ATimer;
end;


destructor TTimerThread.Destroy();
begin
  isDestroying := True;
  Terminate();
  SetEvent(FStopEvent);
  WaitFor();
  TThread.RemoveQueuedEvents(Self);
  CloseHandle(FStopEvent);
  inherited Destroy();
end;


procedure TTimerThread.DoTimer();
begin
  if not Terminated and Assigned(FTimer) and FTimer.Enabled and
     Assigned(FTimer.OnTimerEvent) then
    begin
      isActive := True;
      FTimer.OnTimerEvent(FTimer);
    end;
end;


procedure TTimerThread.Execute();
begin
  while (not Self.Terminated) and (FTimer.Enabled) do
    begin
      if WaitForSingleObject(FStopEvent,FTimer.Period) <> WAIT_TIMEOUT then Break;
      TThread.Queue(Self,DoTimer);
    end;
end;


// TUniThreadedTimer ///////////////////////////////////////////////////////////

constructor TUniThreadedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bEnabled := False;
  Period := 100;
end;


destructor TUniThreadedTimer.Destroy();
begin
  bEnabled := False;
  UpdateTimer();

  inherited Destroy();
end;


procedure TUniThreadedTimer.UpdateTimer();
begin
  if Assigned(thTimerThread) then
    begin
      thTimerThread.Free();
      thTimerThread := nil;
    end;

  if (Enabled = True) then
    begin
      thTimerThread := TTimerThread.Create(Self);
      thTimerThread.Start; // Resume = deprecated, start should be used instead.
    end;
end;


procedure TUniThreadedTimer.SetEnabled(Value: Boolean);
begin
  if bEnabled = Value then Exit;
  if Assigned(thTimerThread) then
    thTimerThread.isActive := Value;

  bEnabled := Value;
  UpdateTimer();
end;


procedure TUniThreadedTimer.SetTimerPeriod(Value: DWord);
begin
  if Value = 0 then Value := 1;
  if Value = dwPeriod then Exit;
  dwPeriod := Value;
  UpdateTimer();
end;

end.
