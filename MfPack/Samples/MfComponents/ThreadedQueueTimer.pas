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
// Version: 3.2.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
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

unit ThreadedQueueTimer;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.Classes,
  QueueTimer;

type
  TCallbackQueueTimer = QueueTimer.TCallbackQueueTimer;

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
  TThreadedQTimer = class(TQTimer)
  private
    FWindow: HWND;
    FDispatchPending: LongInt;
    procedure WindowProc(var Message: TMessage);
  protected
    procedure QueueTimerHandler; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

const
  WM_MFPACK_QUEUE_TICK = WM_APP + $38A;

function CreateTimerQueueTimer; external 'kernel32.dll' name 'CreateTimerQueueTimer';
function DeleteTimerQueueTimer; external 'kernel32.dll' name 'DeleteTimerQueueTimer';
function ChangeTimerQueueTimer; external 'kernel32.dll' name 'ChangeTimerQueueTimer';


procedure Register;
begin
  RegisterComponents('MfPack Timer Samples',[TThreadedQTimer]);
end;


constructor TThreadedQTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindow := AllocateHWnd(WindowProc);
end;


destructor TThreadedQTimer.Destroy;
var
  Message: TMsg;
begin
  Enabled := False;
  if FWindow <> 0 then
    begin
      while PeekMessage(Message,FWindow,WM_MFPACK_QUEUE_TICK,WM_MFPACK_QUEUE_TICK,PM_REMOVE) do
        ;
      DeallocateHWnd(FWindow);
      FWindow := 0;
    end;
  inherited Destroy;
end;


procedure TThreadedQTimer.QueueTimerHandler;
begin
  if not Enabled or (FWindow = 0) then
    Exit;
  if InterlockedExchange(FDispatchPending,1) <> 0 then
    Exit;
  if not PostMessage(FWindow,WM_MFPACK_QUEUE_TICK,0,0) then
    InterlockedExchange(FDispatchPending,0);
end;


procedure TThreadedQTimer.WindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_MFPACK_QUEUE_TICK then
    begin
      InterlockedExchange(FDispatchPending,0);
      Message.Result := 0;
      if Enabled then
        inherited QueueTimerHandler;
    end
  else
    Message.Result := DefWindowProc(FWindow,Message.Msg,Message.WParam,Message.LParam);
end;

end.
