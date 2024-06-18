// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: ThreadedQueueTimer.pas
// Kind: Pascal Unit
// Release date: 25-02-2016
// Language: ENU
//
// Version: 3.1.6
// Description: This is the basic class of a threaded queue timer,
//              Use this timer when less overhead
//              and high presicion is needed with critical sections.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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

unit ThreadedQueueTimer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {Vcl}
  Vcl.Forms,
  {System}
  System.Classes,
  System.SysUtils;


type
  // Called every CreateTimerQueueTimer.Period value in milliseconds
  TCallbackQueueTimer = procedure(lpParam: Pointer;
                                  TimerOrWaitFired: Boolean); stdcall;

  // Creates a timer-queue timer.
  // This timer expires at the specified due time,
  // then after every specified period.
  // When the timer expires, the callback function is called.
  function CreateTimerQueueTimer(var Timer: THandle;
                                 TimerQueue: THandle;
                                 Callback: TCallbackQueueTimer;
                                 Parameter: Pointer;
                                 DueTime: LongWord;
                                 Period: LongWord;
                                 Flags: LongWord): Boolean; stdcall;
  // Important:
  // Wrong implementations can lock up your system!
  // Please read https://learn.microsoft.com/nl-nl/windows/win32/api/threadpoollegacyapiset/nf-threadpoollegacyapiset-deletetimerqueuetimer
  function DeleteTimerQueueTimer(TimerQueue: THandle;
                                 Timer: THandle;
                                 CompletionEvent: THandle): Boolean; stdcall;

  // Updates a timer-queue timer that was created by the CreateTimerQueueTimer function.
  function ChangeTimerQueueTimer(TimerQueue: THandle;
                                 Timer: THandle;
                                 DueTime: LongWord;
                                 Period: LongWord): BOOL; stdcall;

type
  TThreadedQTimer = class;

  TQTimerThread = class(TThread)
  private
    { Private declarations }
    FTimer: TThreadedQTimer;

  protected
    { Protected declarations }
    procedure DoTimer();

  public
    { Public declarations }
    constructor Create(ATimer: TThreadedQTimer);
    destructor Destroy(); override;
    procedure Execute(); override;
  end;


  TThreadedQTimer = class(TComponent)
  private
    { Private declarations }
    bTimerEnabled: Boolean;
    hTimerHandle: THandle;

    uiDueTime: UINT;
    uiPeriod: UINT;
    uiThreadCount: UINT;
    uiCount: UINT;
    neOnTimer: TNotifyEvent;

    { Private methods }
    procedure EnableQTimer(Value: Boolean);
    procedure SetDueTime(const Value: UINT);
    procedure SetPeriod(const Value: UINT);

  protected
    { Protected declarations }
    FQTimerThread: TQTimerThread;
    MsgId: Cardinal;
    gTimerID: TGuid;

    { Protected methods }
    procedure UpdateTimer();
    procedure QueueTimerHandler(); virtual; // called every elTime milliseconds

  public
    { Public declarations }

    { Public methods }
    constructor Create(aOwner: Tcomponent); override;
    // Disable and destroy the timer. (example: FreeAndNil(YourQueueTimer);
    destructor Destroy(); override;
    // Change the timer settings, while it is running.
    function ChangeTimerQueue(lwDueTime: LongWord;
                              lwPeriod: LongWord): Bool;

  published
    property Enabled: Boolean read bTimerEnabled write EnableQTimer default False;
    property DueTime: UINT read uiDueTime write SetDueTime default 0;  // Immediate
    property Period: Cardinal read uiPeriod write SetPeriod default 100;  // 100 ms
    property OnTimerEvent: TNotifyEvent read neOnTimer write neOnTimer;

  end;

  procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('MfPack Timer Samples', [TThreadedQTimer]);
end;


const
  Kernel32Lib = 'kernel32.dll';    // Also declared in WinApi.Windows


procedure TimerCallback(Timer: TThreadedQTimer;
                        CallbackFired: Boolean); stdcall;
begin
try
  if assigned(Timer) then
    begin
      Timer.QueueTimerHandler();
    end;
except
  // silent exception
end;
end;



// TQTIMER
//========


constructor TThreadedQTimer.Create(aOwner: Tcomponent);
var
  stmp: string;
  wcmp: array[0..37] of WideChar;
begin
  inherited Create(aOwner);

  uiDueTime := 0;
  uiPeriod := 100;

  // Create the contextID, in this case a unique guid during runtime
  if Succeeded(CreateGuid(gTimerID)) then
    stmp := GuidToString(gTimerID)
  else // this should not happen!
    stmp := '{6A6E25F2-A09C-4E30-8E10-DDF3020F6620}';

  // Create a unique msgId by the given name of the qtimer object
  MsgId := RegisterWindowMessage(StringToWideChar(stmp,
                                                  wcmp,
                                                  Length(wcmp)));
end;


// destroy the timer
destructor TThreadedQTimer.Destroy();
begin
  // Stop the timer
  EnableQTimer(False);
  CloseHandle(hTimerHandle);
  inherited Destroy();
end;


procedure TThreadedQTimer.EnableQTimer(Value: Boolean);
begin
try
  if (Value <> Enabled) then
    begin
      if (Value = True) then
        begin
          uiThreadCount:= 0;
          uiCount:= 0;
          bTimerEnabled:= CreateTimerQueueTimer(hTimerHandle,
                                                0,
                                                @TimerCallback,
                                                Self,
                                                uiDueTime,
                                                uiPeriod,
                                                WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION);
          UpdateTimer();
        end
      else
        begin
          // Removes a timer from the timer queue and optionally waits
          // for currently running timer callback functions to
          // complete before deleting the timer.
          // See: https://docs.microsoft.com/en-us/windows/win32/sync/using-timer-queues
          //      about how and when to use timer queues.
          WaitForSingleObject(hTimerHandle,
                              Period);

	        if DeleteTimerQueueTimer(0,
                                   hTimerHandle,
                                   INVALID_HANDLE_VALUE) then
            begin
              bTimerEnabled := False;
              Application.ProcessMessages;
              Sleep(100);
            end;
        end;
    end;
except
  Application.HandleException(Self);
end;
end;


procedure TThreadedQTimer.SetDueTime(const Value: UINT);
begin
  if (Enabled = False) then
    uiDueTime:= Value;
end;


procedure TThreadedQTimer.SetPeriod(const Value: UINT);
begin
  if (Enabled = False) then
    uiPeriod:= Value;
end;


// change the timer setting while it's running
function TThreadedQTimer.ChangeTimerQueue(lwDueTime: LongWord;
                                          lwPeriod: LongWord): Bool;
begin

 Result := ChangeTimerQueueTimer(0,
                                 hTimerHandle,
                                 lwDueTime,
                                 lwPeriod);
end;


procedure TThreadedQTimer.UpdateTimer();
begin
  if Assigned(FQTimerThread) then
    begin
      FQTimerThread.Terminate();
      FQTimerThread := nil;
    end;

  if Enabled then
    begin
      FQTimerThread := TQTimerThread.Create(Self);
      FQTimerThread.Start();
    end;
end;


procedure TThreadedQTimer.QueueTimerHandler();
begin
try
  // Check if the timer is enabled in the primary thread
  // to prevent exceptions when the application is terminated.
  if Assigned(neOnTimer) and (bTimerEnabled) then
    neOnTimer(Self);
except
  // Do nothing (silent exception)
end;
end;

// TMfpTimerThread

constructor TQTimerThread.Create(ATimer: TThreadedQTimer);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FTimer:= ATimer;
end;


destructor TQTimerThread.Destroy();
begin
  inherited Destroy();
end;


procedure TQTimerThread.DoTimer();
begin
  if Assigned(FTimer.OnTimerEvent) then
    FTimer.OnTimerEvent(FTimer);
end;


procedure TQTimerThread.Execute();
begin
  while (not Self.Terminated) do
    begin
      if WaitForSingleObject(FTimer.hTimerHandle,
                             FTimer.Period) = WAIT_OBJECT_0 then
        Synchronize(DoTimer);
    end;
end;


{$WARN SYMBOL_PLATFORM OFF}
  function CreateTimerQueueTimer; external Kernel32Lib name 'CreateTimerQueueTimer';
  function DeleteTimerQueueTimer; external Kernel32Lib name 'DeleteTimerQueueTimer';
  function ChangeTimerQueueTimer; external Kernel32Lib name 'ChangeTimerQueueTimer';
{$WARN SYMBOL_PLATFORM ON}

end.

