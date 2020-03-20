// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: QueueTimer.pas
// Kind: Pascal Unit
// Release date: 25-02-2016
// Language: ENU
//
// Version: 2.6.4
// Description:
//              This is the basic class of a queue timer,
//              Use this timer when less overhead
//              and high presicion is needed.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//          For use in critical sections,
//          use the ThreadedQueueTimer instead.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
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
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit QueueTimer;

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
  // Please read https://msdn.microsoft.com/en-us/library/windows/desktop/ms682569
  function DeleteTimerQueueTimer(TimerQueue: THandle;
                                 Timer: THandle;
                                 CompletionEvent: THandle): Boolean; stdcall;

  // Updates a timer-queue timer that was created by the CreateTimerQueueTimer function.
  function ChangeTimerQueueTimer(TimerQueue: THandle;
                                 Timer: THandle;
                                 DueTime: LongWord;
                                 Period: LongWord): BOOL; stdcall;



type

  TQTimer = class(TObject)
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
    MsgId: Cardinal;

    { Protected methods }
    procedure QueueTimerHandler; virtual; // called every elTime milliseconds

  public
    { Public declarations }

    { Public methods }
    constructor Create(pwTimerName: PWideChar);
    // Disable and destroy the timer. (example: FreeAndNil(YourQueueTimer);
    destructor Destroy(); override;
    // Change the timer settings, while it is running.
    function ChangeTimerQueue(lwDueTime: LongWord;
                              lwPeriod: LongWord): Bool;

    property Enabled: Boolean read bTimerEnabled write EnableQTimer default False;
    property DueTime: UINT read uiDueTime write SetDueTime default 0;  // 0 = start immediate
    property Period: Cardinal read uiPeriod write SetPeriod default 100; // timer interval in ms
    property OnTimerEvent: TNotifyEvent read neOnTimer write neOnTimer;
  end;


implementation

const
  Kernel32Lib = 'kernel32.dll';    // Also declared in WinApi.Windows


{$WARN SYMBOL_PLATFORM OFF}
  function CreateTimerQueueTimer; external Kernel32Lib name 'CreateTimerQueueTimer';
  function DeleteTimerQueueTimer; external Kernel32Lib name 'DeleteTimerQueueTimer';
  function ChangeTimerQueueTimer; external Kernel32Lib name 'ChangeTimerQueueTimer';
{$WARN SYMBOL_PLATFORM ON}


procedure TimerCallback(Timer: TQTimer; CallbackFired: Boolean); stdcall;
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


constructor TQTimer.Create(pwTimerName: PWideChar);
begin
  inherited Create();
  uiDueTime := 0;
  uiPeriod := 100;
  // Create a unique msgId by the given name of the qtimer object
  MsgId := RegisterWindowMessage(pwTimerName);
end;


// destroy the timer
destructor TQTimer.Destroy();
begin
  // Stop the timer
  EnableQTimer(False);
  //DeallocateHWnd(hwWindowHandle);
  inherited Destroy();
end;


procedure TQTimer.EnableQTimer(Value: Boolean);
begin
  if (Value <> bTimerEnabled) then
    begin
      if (Value = True) then
        begin
          uiThreadCount := 0;
          uiCount := 0;
          bTimerEnabled := CreateTimerQueueTimer(hTimerHandle,
                                                 0,
                                                 @TimerCallback,
                                                 Self,
                                                 uiDueTime,
                                                 uiPeriod,
                                                 WT_EXECUTEDEFAULT);
        end
      else
        begin
          // Removes a timer from the timer queue and optionally waits
          // for currently running timer callback functions to
          // complete before deleting the timer.
          // See: https://docs.microsoft.com/en-us/windows/win32/sync/using-timer-queues
          //      about how and when to use timer queues.
	        if DeleteTimerQueueTimer(0,
                                   hTimerHandle,
                                   INVALID_HANDLE_VALUE) then
            begin
              bTimerEnabled := False;
            end;
        end;
    end;
end;


procedure TQTimer.SetDueTime(const Value: UINT);
begin
  if (bTimerEnabled = False) then
    uiDueTime := Value;
end;


procedure TQTimer.SetPeriod(const Value: UINT);
begin
  if (bTimerEnabled = False) then
    uiPeriod := Value;
end;

// change the timer setting while it's running
function TQTimer.ChangeTimerQueue(lwDueTime: LongWord;
                                  lwPeriod: LongWord): Bool;
begin

 Result := ChangeTimerQueueTimer(0,
                                 hTimerHandle,
                                 lwDueTime,
                                 lwPeriod);
end;


procedure TQTimer.QueueTimerHandler;
begin
try
  // Check if the timer is enabled in the primary thread
  // to prevent exceptions when the application is terminated.
  if bTimerEnabled and Assigned(neOnTimer) then
    neOnTimer(Self);
except
  // Do nothing (silent exception)
end;
end;

end.


