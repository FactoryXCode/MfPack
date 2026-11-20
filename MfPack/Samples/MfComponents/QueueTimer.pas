// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: QueueTimer.pas
// Kind: Pascal Unit
// Release date: 25-02-2016
// Language: ENU
//
// Version: 3.2.0
// Description:
//              This is the basic class of a queue timer,
//              Use this timer when less overhead
//              and high presicion is needed.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//          For use in critical sections,
//          use the ThreadedQueueTimer instead.
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
// Source: Parts of timer queue examples
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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

unit QueueTimer;

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils;

type
  TCallbackQueueTimer = procedure(lpParam: Pointer;
                                  TimerOrWaitFired: Boolean); stdcall;

function CreateTimerQueueTimer(var Timer: THandle;
                               TimerQueue: THandle;
                               Callback: TCallbackQueueTimer;
                               Parameter: Pointer;
                               DueTime: LongWord;
                               Period: LongWord;
                               Flags: LongWord): BOOL; stdcall;
function DeleteTimerQueueTimer(TimerQueue: THandle;
                               Timer: THandle;
                               CompletionEvent: THandle): BOOL; stdcall;
function ChangeTimerQueueTimer(TimerQueue: THandle;
                               Timer: THandle;
                               DueTime: LongWord;
                               Period: LongWord): BOOL; stdcall;

type
  TQTimer = class(TComponent)
  private
    bTimerEnabled: Boolean;
    hTimerHandle: THandle;
    uiDueTime: UINT;
    uiPeriod: UINT;
    neOnTimer: TNotifyEvent;

    procedure EnableQTimer(Value: Boolean);
    procedure SetDueTime(const Value: UINT);
    procedure SetPeriod(const Value: UINT);
  protected
    MsgId: Cardinal;
    gTimerID: TGUID;
    procedure Loaded; override;
    procedure QueueTimerHandler; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ChangeTimerQueue(lwDueTime: LongWord;
                              lwPeriod: LongWord): BOOL;
  published
    property Enabled: Boolean read bTimerEnabled write EnableQTimer default False;
    property DueTime: UINT read uiDueTime write SetDueTime default 0;
    property Period: Cardinal read uiPeriod write SetPeriod default 100;
    property OnTimerEvent: TNotifyEvent read neOnTimer write neOnTimer;
  end;

procedure Register;

implementation

function CreateTimerQueueTimer; external 'kernel32.dll' name 'CreateTimerQueueTimer';
function DeleteTimerQueueTimer; external 'kernel32.dll' name 'DeleteTimerQueueTimer';
function ChangeTimerQueueTimer; external 'kernel32.dll' name 'ChangeTimerQueueTimer';

threadvar
  CurrentQueueTimer: TQTimer;


procedure TimerCallback(Parameter: Pointer;CallbackFired: Boolean); stdcall;
var
  Previous: TQTimer;
begin
  Previous := CurrentQueueTimer;
  CurrentQueueTimer := TQTimer(Parameter);
  try
    if Parameter <> nil then
      TQTimer(Parameter).QueueTimerHandler;
  finally
    CurrentQueueTimer := Previous;
  end;
end;


procedure Register;
begin
  RegisterComponents('MfPack Timer Samples',[TQTimer]);
end;


constructor TQTimer.Create(AOwner: TComponent);
var
  Name: UnicodeString;
begin
  inherited Create(AOwner);
  uiPeriod := 100;
  if CreateGUID(gTimerID) < 0 then
    gTimerID := StringToGUID('{4C68E633-53BF-4609-B7DC-0E4FDBCC585E}');
  Name := UnicodeString(GUIDToString(gTimerID));
  MsgId := RegisterWindowMessageW(PWideChar(Name));
end;


procedure TQTimer.Loaded;
var
  StartEnabled: Boolean;
begin
  inherited Loaded;
  StartEnabled := bTimerEnabled;
  bTimerEnabled := False;
  if StartEnabled then
    EnableQTimer(True);
end;


destructor TQTimer.Destroy;
begin
  EnableQTimer(False);
  inherited Destroy;
end;


procedure TQTimer.EnableQTimer(Value: Boolean);
var
  Completion: THandle;
  Timer: THandle;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    begin
      bTimerEnabled := Value;
      Exit;
    end;
  if Value = bTimerEnabled then
    Exit;
  bTimerEnabled := Value;
  if Value then
    begin
      // Publish Enabled before a zero-due-time callback can run.
      if not CreateTimerQueueTimer(hTimerHandle,0,@TimerCallback,Self,
                                    uiDueTime,uiPeriod,0) then
        begin
          bTimerEnabled := False;
          hTimerHandle := 0;
          RaiseLastOSError;
        end;
    end
  else
    begin
      Timer := hTimerHandle;
      hTimerHandle := 0;
      if Timer <> 0 then
        begin
          Completion := INVALID_HANDLE_VALUE;
          // A callback can disable itself, but cannot wait for its own return.
          if CurrentQueueTimer = Self then
            Completion := 0;
          DeleteTimerQueueTimer(0,Timer,Completion);
        end;
    end;
end;


procedure TQTimer.SetDueTime(const Value: UINT);
begin
  if not bTimerEnabled then
    uiDueTime := Value;
end;


procedure TQTimer.SetPeriod(const Value: UINT);
begin
  if not bTimerEnabled then
    uiPeriod := Value;
end;


function TQTimer.ChangeTimerQueue(lwDueTime: LongWord;
                                 lwPeriod: LongWord): BOOL;
begin
  Result := False;
  if not bTimerEnabled or (hTimerHandle = 0) then
    Exit;
  Result := ChangeTimerQueueTimer(0,hTimerHandle,lwDueTime,lwPeriod);
  if Result then
    begin
      uiDueTime := lwDueTime;
      uiPeriod := lwPeriod;
    end;
end;


procedure TQTimer.QueueTimerHandler;
begin
  try
    if bTimerEnabled and Assigned(neOnTimer) then
      neOnTimer(Self);
  except
    // User exceptions must not unwind through the native thread-pool callback.
  end;
end;

end.
