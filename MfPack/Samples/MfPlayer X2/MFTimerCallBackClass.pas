// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MFTimerCallBackClass.pas
// Kind: Pascal Unit
// Release date: 13-08-2019
// Language: ENU
//
// Revision Version: 4.0.0
// Description: A Timercallback class for the IMFTimer interface.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 01/08/2026 Tony                Fixed timer interval initialization and async UI notification.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// Source: -
//
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
unit MFTimerCallBackClass;

interface

uses

  {Winapi}
  Winapi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Forms,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  {Project}
  MfPCXConstants;

const
  // MFTIME uses 100-nanosecond units. A 100 ms UI update cadence keeps the
  // progress display responsive without flooding the Media Foundation queue.
  MF_PLAYER_UI_TIMER_INTERVAL_HNS = 100 * 10000;

type

  TMFCallBack = class(TInterfacedPersistent, IMFAsyncCallback)
    private
      m_TimerResolution: LongWord;
      m_hnsClockTime: MFTIME;
      m_hwndOwner: HWnd;
      m_TimerFlags: DWord;
      m_CancellationObject: IUnknown;

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
                        CancellationObject: PIUnknown): HResult;

      // Cancels a timer that was set using the IMFTimer.SetTimer method.
      function CancelTimer(const CancellationObject: IUnknown): HResult;

      // Constructor, destructor
      // These are public methods in Delphi.
      constructor Create(OwnerHandle: HWnd); overload;
      procedure BeforeDestruction(); override;
      destructor Destroy(); override;

      property ClockProperties: MFCLOCK_PROPERTIES read GetClockProperties;
      property TimerResolution: LongWord read m_TimerResolution write SetTimerResolution default MF_PLAYER_UI_TIMER_INTERVAL_HNS;
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
  m_TimerResolution := MF_PLAYER_UI_TIMER_INTERVAL_HNS;
  m_hnsClockTime := 0;
  m_TimerFlags := DWord(MFTIMER_RELATIVE);
  m_CancellationObject := nil;

  // Create timer object, if it fails, raise exception
  if FAILED(MFPresentationClock.QueryInterface(IID_IMFTimer,
                                               MfTimer)) then
    raise Exception.Create('The PresentationClock failed to create the timer interface.');

end;


procedure TMFCallBack.BeforeDestruction();
begin

  if Assigned(MfTimer) and Assigned(m_CancellationObject) then
    MfTimer.CancelTimer(m_CancellationObject);

  m_CancellationObject := nil;
  MfTimer := nil;

  if Assigned(MFPresentationClock) then
    MFPresentationClock.Stop();

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
begin

  // Request the Media Foundation defaults. Returning S_OK without assigning
  // both out parameters leaves the work-queue selection undefined.
  pdwFlags := 0;
  pdwQueue := 0;
  Result := E_NOTIMPL;
end;


// MfTimer is responsible to trigger invoke the first time!
function TMFCallBack.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  hr: HResult;

begin

  hr := S_OK;

  if not Assigned(MFPresentationClock) then
    Exit(MF_E_NO_CLOCK);

  if not Assigned(MfTimer) then
    Exit(E_POINTER);

try
  hr := MfTimer.SetTimer(m_TimerFlags, // Absolute or Relative
                         TimerResolution,
                         IMFAsyncCallback(Self),
                         nil,
                         @m_CancellationObject);

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
      // Invoke runs on a Media Foundation work-queue thread. Post the private
      // message so that this callback never blocks on the VCL/UI thread.
      PostMessage(m_hwndOwner,
                  WM_TIMERNOTIFY,
                  WPARAM(GetPresentationClockState()),
                  LPARAM(hr));
    end;

  if (hr = MF_S_CLOCK_STOPPED) then
    begin
      //
    end;

  if (hr = MF_E_SHUTDOWN) then
    hr := CancelTimer(m_CancellationObject);

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
                              CancellationObject: PIUnknown): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  m_TimerFlags := dwFlags;

  // Always retain the cancellation key for the first timer as well as for
  // recurring ticks. Older callers passed nil here, which made the first
  // scheduled callback impossible to cancel during rapid close/reopen.
  if not Assigned(CancellationObject) then
    CancellationObject := @m_CancellationObject;

  if (GetPresentationClockState() = MFCLOCK_STATE_RUNNING) then
    try
      hr := MfTimer.SetTimer(dwFlags,   // Absolute = 0, Relative = DWord(MFTIMER_RELATIVE)
                             ClockTime, // 0 = Immediate
                             Self,
                             Nil,
                             CancellationObject);
    except
      Application.HandleException(Self);
    end
  else
    hr := MF_S_CLOCK_STOPPED;

  Result := hr;
end;


function TMFCallBack.CancelTimer(const CancellationObject: IUnknown): HResult;
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

  if (aValue = 0) then
    aValue := MF_PLAYER_UI_TIMER_INTERVAL_HNS;

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
