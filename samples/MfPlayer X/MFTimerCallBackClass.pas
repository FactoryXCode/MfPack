// FactoryX
//
// Copyright � FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MFTimerCallBackClass.pas
// Kind: Pascal Unit
// Release date: 13-08-2019
// Language: ENU
//
// Version: 2.6.4
// Description: A Timercallback class for the IMFTimer interface.
//
// Company: FactoryX
// Intiator(s): Ramyses De Macedo Rodrigues, Tony (maXcomX), Peter (OzShips).
// Contributor(s): Ramyses De Macedo Rodrigues, Tony Kalf (maXcomX), Peter Larson (ozships).
//
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// Source: Parts of CPlayer Examples
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit MFTimerCallBackClass;

interface

uses
  {Winapi}
  Winapi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Forms,
  {MfPack}
  MfPack.MfpUtils,
  MfPack.MfpTypes,
  MfPack.Unknwn,
  MfPack.MfIdl,
  MfPack.MfObjects,
  MfPack.MfError,
  {Project}
  MfPCXConstants;

type
  TMFCallBack = class(TInterfacedPersistent, IMFAsyncCallback)
    private
      m_TimerResolution: LongWord;
      m_hnsClockTime: MFTIME;
      m_hwndOwner: HWnd;
      m_TimerFlags: DWord;

      // The Timer object that must be initiated by owner's PresentationClock
      MfTimer: IMFTimer;

      procedure SetTimerResolution(aValue: LongWord);
      function GetClockProperties(): MFCLOCK_PROPERTIES;

    public

      // Provides configuration information to the dispatching thread for a callback.
      // Implementation of this method is optional.
      function GetParameters(out pdwFlags: DWord;
                             out pdwQueue: DWord): HResult; stdcall;

      // Called when an asynchronous operation is completed.
      // Implementation of this method is required.
      function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

      // Returns the PresentationClock state
      function GetPresentationClockState(): MFCLOCK_STATE;

      // Fires the loop that sets a timer invoking a callback at the specified - TimerResolution - time.
      // NOTE: The PresentationClock.Gettime is relative to when the clock was last started.
      //       The timer will only work if the presentationclock is running.
      //       Otherwise the result will be any of these:
      //         MFCLOCK_STATE_INVALID, MFCLOCK_STATE_STOPPED or MFCLOCK_STATE_PAUSED.
      function SetTimer(ClockTime: LONGLONG;
                        dwFlags: DWord; // Absolute = 0, Relative = DWord(MFTIMER_RELATIVE)
                        CancellationObject: IUnknown): HResult;

      // Cancels a timer that was set using the IMFTimer.SetTimer method.
      function CancelTimer(CancellationObject: IUnknown): HResult;

      // Constructor, destructor
      // These are public methods in Delphi.
      constructor Create(OwnerHandle: HWnd); overload;
      procedure BeforeDestruction(); override;
      destructor Destroy(); override;

      property ClockProperties: MFCLOCK_PROPERTIES read GetClockProperties;
      property TimerResolution: LongWord read m_TimerResolution write SetTimerResolution default 1;
      property ClockTime: MFTIME read m_hnsClockTime;
  end;

var
  MFPresentationClock: IMFPresentationClock;

implementation


constructor TMFCallBack.Create(OwnerHandle: HWnd);
begin
  inherited Create();

  // Check if the presentationclock is created.
  if Not Assigned(MFPresentationClock) then
    raise Exception.Create('Callback initialization failed: MFPresentationClock is not created.');

  m_hwndOwner := OwnerHandle;
  // Create timer object, if it fails, raise exception
  if FAILED(MFPresentationClock.QueryInterface(IID_IMFTimer,
                                               MfTimer)) then
    raise Exception.Create('The PresentationClock failed to create the timer interface.');

end;


procedure TMFCallBack.BeforeDestruction();
begin
  MFPresentationClock.Stop();
  MfTimer := Nil;
  inherited BeforeDestruction();
end;


destructor TMFCallBack.Destroy();
begin
  inherited Destroy();
end;


// PUBLIC METHODS //////////////////////////////////////////////////////////////

// Implementation of this method is optional.
function TMFCallBack.GetParameters(out pdwFlags: DWord;
                                   out pdwQueue: DWord): HResult;
var
  hr: HResult;

begin
  hr:= S_OK;

  Result := hr;
end;


// MfTimer is responsible to trigger invoke the first time!
function TMFCallBack.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

try
  if Not Assigned(MFPresentationClock) then
    begin
      hr := MF_E_NO_CLOCK;
      Exit;
    end;

  hr := MfTimer.SetTimer(m_TimerFlags, // Absolute or Relative
                         TimerResolution,
                         IMFAsyncCallback(Self),
                         Nil,
                         Nil);

  // The timer's return code is one of those:
  //   S_OK = The method succeeded.
  //   MF_E_SHUTDOWN = The clock was shut down.
  //   MF_S_CLOCK_STOPPED = The method succeeded, but the clock is stopped.
  // NOTE: If the clock is stopped, the method returns MF_S_CLOCK_STOPPED.
  //       The callback will not be invoked until the clock is started.

  if (hr = S_OK) then
    begin
      // Gets the clocktime in 100-nano second units.
      hr := MFPresentationClock.GetTime(m_hnsClockTime);
      // Send a message to the owner to update the ontimer depending objects.
      SendMessage(m_hwndOwner,
                  WM_TIMERNOTIFY,
                  WPARAM(GetPresentationClockState()),
                  LPARAM(hr));
    end;

  if (hr = MF_S_CLOCK_STOPPED) then
    begin

    end;

 if (hr = MF_E_SHUTDOWN) then
    begin
      hr := CancelTimer(Nil);
    end;

finally
  Result := hr;
end;
end;


function TMFCallBack.GetPresentationClockState(): MFCLOCK_STATE;
begin
  {void} MFPresentationClock.GetState(0,
                                      Result);
end;


function TMFCallBack.SetTimer(ClockTime: LONGLONG;
                              dwFlags: DWord;
                              CancellationObject: IUnknown): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  m_TimerFlags := dwFlags;
  if (GetPresentationClockState() = MFCLOCK_STATE_RUNNING) then
    try
      hr := MfTimer.SetTimer(dwFlags,   // Absolute = 0, Relative = DWord(MFTIMER_RELATIVE)
                             ClockTime, // 0 = Immediate
                             Self,
                             Nil,
                             Nil);
    except
      Application.HandleException(Self);
    end
  else
    hr := MF_S_CLOCK_STOPPED;

  Result := hr;
end;


function TMFCallBack.CancelTimer(CancellationObject: IUnknown): HResult;
var
  hr: HResult;
begin
  hr := S_OK;
try
  // NOTE: Because the timer is dispatched asynchronously,
  // the application's timer callback might get invoked even if this method succeeds.
  if Assigned(MfTimer) then
    hr := MfTimer.CancelTimer(CancellationObject);
  Result := hr;
except
  Result := hr;
  Application.HandleException(Self);
end;
end;


// PRIVATE METHODS /////////////////////////////////////////////////////////////

procedure TMFCallBack.SetTimerResolution(aValue: LongWord);
begin
  m_TimerResolution := aValue;
end;


function TMFCallBack.GetClockProperties(): MFCLOCK_PROPERTIES;
var
  ClockProps: MFCLOCK_PROPERTIES;

begin
  MFPresentationClock.GetProperties(ClockProps);
  Result := ClockProps;
end;


end.
