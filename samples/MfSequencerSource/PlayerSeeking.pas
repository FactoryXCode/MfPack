// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: PlayerSeeking.pas
// Kind: Pascal / Delphi unit
// Release date: 29-07-2012
// Language: ENU
//
// Revision Version: 2.6.3
// Description: PlayerSeeking helper for CorePlayerEngine.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/12/2019                     Underworld release.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX263
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.18362.0 (19H1)
//
// Todo: -
//
//==============================================================================
// Source: Microsoft CPlayer example.
//         https://docs.microsoft.com/en-us/windows/win32/medfound/how-to-play-unprotected-media-files
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
//==============================================================================
unit PlayerSeeking;

interface
uses
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics,
  System.Classes,
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.MfApi,
  MfPack.MfIdl,
  MfPack.MfObjects,
  MfPack.PropIdl,
  MfPack.Evr,
  MfPack.Evr9,
  MfPack.WinError,
  MfPack.MfError,
  Helpers;


const
  CMD_0            = $00;
  CMD_PENDING      = $01;
  CMD_PENDING_SEEK = $02;
  CMD_PENDING_RATE = $04;


type

  TCommand = (CmdNone = 0,
              CmdStop,
              CmdStart,
              CmdPause,
              CmdSeek
              );

  // Describes the current or requested state, with respect to seeking and
  // playback rate.
  TSeekState = record
    command: TCommand;
    fRate: Single;       // Playback rate
    bThin: Bool;         // Thinned playback?
    hnsStart: MFTIME;    // Start position
  end;

// Implements seeking and rate control functionality.
//
//    Usage:
//
//    - Call SetTopology when you get the MF_TOPOSTATUS_READY session event.
//    - Call SessionEvent for each session event.
//    - Call Clear before closing the Media Session.
//    - To coordinate rate-change requests with transport state, delegate all
//      stop, pause, and play commands to the PlayerSeeking class.
//

  // The class
  TPlayerSeeking = class
  private

    m_bPending: ShortInt;      // Is a request pending?
    m_state: TSeekState;       // Current nominal state.
    m_request: TSeekState;     // Pending request.

    m_critsec: TMFCritSec;     // Protects the seeking and rate-change states.

    m_caps: DWORD;             // Session caps.
    m_bCanScrub: Boolean;      // Does the current session support rate = 0.

    m_hnsDuration: MFTIME;     // Duration of the current presentation.
    m_fPrevRate: Single;       // Last rate set

    m_pSession: IMFMediaSession;
    m_pRate: IMFRateControl;
    m_pRateSupport: IMFRateSupport;
    m_pClock: IMFPresentationClock;

    function SetPositionInternal(hnsPosition: MFTIME): HResult;
    function CommitRateChange(fRate: Single;  bThin: Boolean): HResult;
    function GetNominalRate(): Float;

    function OnSessionStart(hrStatus: HResult): HResult;
    function OnSessionStop(hrStatus: HResult): HResult;
    function OnSessionPause(hrStatus: HResult): HResult;
    function OnSessionEnded(hrStatus: HResult): HResult;

    function UpdatePendingCommands(cmd: TCommand): HResult;

  public

    constructor Create();
    destructor Destroy; override;

    function SetTopology(pSession: IMFMediaSession;
                         pTopology: IMFTopology): HResult;

    function Clear(): HResult;
    function SessionEvent(pEvent: IMFMediaEvent;
                          mediatype: MediaEventType;
                          hrStatus: HResult = 0): HResult;


    function CanSeek(out pbCanSeek: Boolean): HResult;
    function GetDuration(out phnsDuration: MFTIME): HResult;
    function GetPosition(out phnsPosition: MFTIME): HResult;
    function SetPosition(hnsPosition: MFTIME): HResult;

    function CanScrub(out pbCanScrub: Boolean): HResult;
    function Scrub(bScrub: Boolean): HResult;

    function CanFastForward(out pbCanFF: Boolean): HResult;
    function CanRewind(out pbCanRewind: Boolean): HResult;

    function SetRate(fRate: Single): HResult;
    function GetRate(out fRate: Single): HResult;
    function GetSafeRates(out flFastestRate: Single;
                          out flSlowestRate: Single;
                          RateDirection: MFRATE_DIRECTION): HResult;
    function CheckForValidRate(var fRate: Single;
                               RateDirection: MFRATE_DIRECTION): HResult;

    function FastForward(): HResult;
    function Rewind(): HResult;

    function Start(): HResult;
    function Pause(): HResult;
    function Stop(): HResult;

  end;


implementation


// constructor & destructor
constructor TPlayerSeeking.Create();
begin
  inherited Create();
  m_bPending := CMD_0;
  {void} Clear();
  m_critsec := TMFCritSec.Create();

end;

destructor TPlayerSeeking.Destroy();
begin
  m_pClock := Nil;
  m_pSession := Nil;
  m_pRate := Nil;
  m_pRateSupport := Nil;
  m_critsec.Free;
  inherited;
end;

// Clears any resources for the current topology.

function TPlayerSeeking.Clear(): HResult;
begin
  m_caps := 0;
  m_bCanScrub := False;
  m_hnsDuration := 0;
  m_fPrevRate := 1.0;
  m_bPending := CMD_0;
  m_pClock := Nil;
  m_pSession := Nil;
  m_pRate := Nil;
  m_pRateSupport := Nil;

  ZeroMemory(@m_state,
             SizeOf(m_state));

  m_state.command := CmdStop;
  m_state.fRate := 1.0;

  ZeroMemory(@m_request,
             SizeOf(m_request));

  m_request.command := CmdNone;
  m_request.fRate := 1.0;

  Result := S_OK;
end;


// Call when the full playback topology is ready.
function TPlayerSeeking.SetTopology(pSession: IMFMediaSession;
                                    pTopology: IMFTopology): HResult;
var
  hr: HResult;
  hrTmp: HResult;  // For non-critical failures.
  pClock: IMFClock;
  pPD: IMFPresentationDescriptor;

label
  done;

begin
  Clear();

  // Get the session capabilities.
  hr := pSession.GetSessionCapabilities(m_caps);

  if FAILED(hr) then
    goto done;

  // Get the presentation descriptor from the topology.
  hr := GetPresentationDescriptorFromTopology(pTopology,
                                              pPD);

  if FAILED(hr) then
    goto done;

  // Get the duration from the presentation descriptor (optional)
  {void} pPD.GetUINT64(MF_PD_DURATION,
                       UInt64(m_hnsDuration));

  // Get the presentation clock (optional)
  hrTmp := pSession.GetClock(pClock);

  if Succeeded(hrTmp) then
    begin
      hr := pClock.QueryInterface(IID_IMFClock,
                                  m_pClock);
      if FAILED(hr) then
        goto done;
    end;

  // Get the rate control interface (optional)
  hrTmp := MFGetService(pSession,
                        MF_RATE_CONTROL_SERVICE,
                        IID_IMFRateControl,
                        m_pRate);

  // Get the rate support interface (optional)
  if Succeeded(hrTmp) then
    hrTmp := MFGetService(pSession,
                          MF_RATE_CONTROL_SERVICE,
                          IID_IMFRateSupport,
                          m_pRateSupport);

  // Check if rate 0 (scrubbing) is supported.
  if Succeeded(hrTmp) then
    hrTmp := m_pRateSupport.IsRateSupported(True,
                                            0,
                                            0);

  if Succeeded(hrTmp) then
    m_bCanScrub := True;

  // if m_pRate is Nil, m_bCanScrub must be FALSE.
{$IFDEF DEBUG}
   Assert((m_pRate <> nil) and (m_bCanScrub <> false));
{$ENDIF}

  // Cache a pointer to the session.
  m_pSession := pSession;

done:
  Result := hr;

end;


// Call when media session fires an event.
function TPlayerSeeking.SessionEvent(pEvent: IMFMediaEvent;
                                     mediatype: MediaEventType;
                                     hrStatus: HResult = 0): HResult;
var
  pvar: mfPROPVARIANT;
  hr: HResult;

begin

  case mediatype of
    MESessionStarted: begin
                        OnSessionStart(hrStatus);
                       end;

    MESessionStopped: begin
                        OnSessionStop(hrStatus);
                      end;

    MESessionPaused: begin
                       OnSessionPause(hrStatus);
                     end;

    MESessionRateChanged: begin
                            // If the rate change succeeded, we've already got the rate
                            // cached. If it failed, try to get the actual rate.
                           if Failed(hrStatus) then
                             begin
                               PropVariantInit(pvar);
                               hr := pEvent.GetValue(pvar);
                               if Succeeded(hr) and (pvar.vt = VT_R4) then
                                 m_state.fRate := pvar.fltVal;
                             end;
                          end;

    MESessionEnded: begin
                      OnSessionEnded(hrStatus);
                     end;

    MESessionCapabilitiesChanged: begin
                                    // The session capabilities changed. Get the updated capabilities.
                                    m_caps := MFGetAttributeUINT32(pEvent,
                                                                   MF_EVENT_SESSIONCAPS,
                                                                   m_caps);
                                  end;
  end;
  Result := S_OK;
end;

// Starts playback.

function TPlayerSeeking.Start(): HResult;
var
  hr: HResult;
  varStart: mfPROPVARIANT;

begin
  hr := S_OK;

  m_critsec.Lock;

   // If another operation is pending, cache the request.
   // Otherwise, start the media session.
   if m_bPending = CMD_PENDING then
     m_request.command := CmdStart
   else
     begin
        PropVariantInit(varStart);
        hr := m_pSession.Start(GUID_NULL,
                               varStart);
        m_state.command := CmdStart;
        m_bPending := CMD_0;
     end;

  m_critsec.Unlock;

  Result := hr;
end;

// Pauses playback.
function TPlayerSeeking.Pause(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  m_critsec.Lock;

  // If another operation is pending, cache the request.
  // Otherwise, pause the media session.
  if m_bPending = CMD_PENDING then
    m_request.command := CmdPause
  else
    begin
      hr := m_pSession.Pause();
      m_state.command := CmdPause;
      m_bPending := CMD_0;
    end;
  m_critsec.Unlock;
  Result := hr;
end;

// Stops playback.
function TPlayerSeeking.Stop(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  m_critsec.Lock;

  // If another operation is pending, cache the request.
  // Otherwise, stop the media session.
  if m_bPending = CMD_PENDING then
    m_request.command := CmdStop
  else
    begin
      hr := m_pSession.Stop();
      m_state.command := CmdStop;
      m_bPending := CMD_0;
    end;
  m_critsec.Unlock;
  Result := hr;
end;

// Queries whether the current session supports seeking.
function TPlayerSeeking.CanSeek(out pbCanSeek: Boolean): HResult;
begin
  // Note: The MFSESSIONCAP_SEEK flag is sufficient for seeking. However, to
  // implement a seek bar, an application also needs the duration (to get
  // the valid range) and a presentation clock (to get the current position).

  pbCanSeek := (((m_caps And MFSESSIONCAP_SEEK) = MFSESSIONCAP_SEEK) And
                (m_hnsDuration > 0) And
                 (m_pClock <> Nil));

  Result := S_OK;
end;

// Gets the duration of the current presentation.
function TPlayerSeeking.GetDuration(out phnsDuration: MFTIME): HResult;
begin
  phnsDuration := m_hnsDuration;
  if (m_hnsDuration = 0) then
    Result := MF_E_NO_DURATION
  else
    Result := S_OK;
end;

// Gets the current playback position.
function TPlayerSeeking.GetPosition(out phnsPosition: MFTIME): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  m_critsec.Lock;

  if Not Assigned(m_pClock) then
    begin
      Result := MF_E_NO_CLOCK;
      Exit;
    end;

  // Return, in order:
  // 1. Cached seek request (nominal position).
  // 2. Pending seek operation (nominal position).
  // 3. Presentation time (actual position).

  if (m_request.command = CmdSeek) then
    phnsPosition := m_request.hnsStart
  else if (m_bPending = CMD_PENDING) And (m_request.hnsStart = CMD_PENDING_SEEK) then
    phnsPosition := m_state.hnsStart
  else
    hr := m_pClock.GetTime(phnsPosition);

  m_critsec.Unlock;
  Result := hr;
end;

// Sets the current playback position.
function TPlayerSeeking.SetPosition(hnsPosition: MFTIME): HResult;
var
  hr: HResult;
begin

  hr := S_OK;
  m_critsec.Lock;
  if (m_bPending = CMD_PENDING) then
    begin
      // Currently seeking or changing rates, so cache this request.
      m_request.command := CmdSeek;
      m_request.hnsStart := hnsPosition;
    end
  else
    hr := SetPositionInternal(hnsPosition);
  m_critsec.Unlock;
  Result := hr;
end;

// Queries whether the current session supports scrubbing.
function TPlayerSeeking.CanScrub(out pbCanScrub: Boolean): HResult;
begin
  pbCanScrub := m_bCanScrub;
  Result := S_OK;
end;

// Enables or disables scrubbing.
function TPlayerSeeking.Scrub(bScrub: Boolean): HResult;
var
  hr: HResult;

label
  done;

begin
  // Scrubbing is implemented as rate = 0.0
  m_critsec.Lock;

  if Not Assigned(m_pRate) then
    begin
      hr := MF_E_INVALIDREQUEST;
      goto done;
    end;

  if m_bCanScrub = False then
    begin
      hr := MF_E_INVALIDREQUEST;
      goto done;
    end;

  hr := S_OK;

  if bScrub = True then
    begin
      // Enter scrubbing mode. Cache the current rate.
      if GetNominalRate() <> 0 then
        m_fPrevRate := m_state.fRate;
      hr := SetRate(0.0);
    end
  else
    begin
      // Leaving scrubbing mode. Restore the old rate.
      if GetNominalRate() = 0 then
        hr := SetRate(m_fPrevRate);
    end;

done:
  m_critsec.Unlock;
  Result := hr;
end;

// Queries whether the current session supports fast-forward.
function TPlayerSeeking.CanFastForward(out pbCanFF: Boolean): HResult;
begin
  pbCanFF := ((m_caps And MFSESSIONCAP_RATE_FORWARD) = MFSESSIONCAP_RATE_FORWARD);
  Result := S_OK;
end;

// Queries whether the current session supports rewind (reverse play).
function TPlayerSeeking.CanRewind(out pbCanRewind: Boolean): HResult;
begin
  pbCanRewind := ((m_caps And MFSESSIONCAP_RATE_REVERSE) = MFSESSIONCAP_RATE_REVERSE);
  Result := S_OK;
end;

// Switches to fast-forward playback, as follows:
// - If the current rate is < 0 (reverse play), switch to 1x speed.
// - Otherwise, double the current playback rate.
//
// Note: This method is for convenience; the application can also call SetRate.
function TPlayerSeeking.FastForward(): HResult;
var
  hr: HResult;
  fTarget: Single;

begin
  fTarget := GetNominalRate() * 2;
  if (fTarget <= 0.0) then
    fTarget := 1.0;
  hr := SetRate(fTarget);
  Result := hr;
end;

// Switches to reverse playback, as follows:
// - If the current rate is > 0 (forward playback), switch to -1x speed.
// - Otherwise, double the current (reverse) playback rate.
//
// Note: This method is for convenience; the application can also call SetRate.
function TPlayerSeeking.Rewind(): HResult;
var
  hr: HResult;
  fTarget: Single;

begin
  fTarget := GetNominalRate() * 2;
  if (fTarget >= 0.0) then
    fTarget := -1.0;
  hr := SetRate(fTarget);
  Result := hr;
end;

// Sets the playback rate.
function TPlayerSeeking.SetRate(fRate: Single): HResult;
var
  hr: HResult;
  bThin: BOOL;

label
  done;

begin
  bThin := False;

  m_critsec.Lock;

  if (fRate = GetNominalRate()) then
    begin
      hr := S_OK; // no-op
      goto done;
    end;

  if Not Assigned(m_pRateSupport) then
    begin
      hr := MF_E_INVALIDREQUEST;
      goto done;
    end;

  // Check if this rate is supported. Try non-thinned playback first,
  // then fall back to thinned playback.
  hr := m_pRateSupport.IsRateSupported(bThin,
                                       fRate,
                                       0);

  if Failed(hr) then
    begin
      bThin := True;
      hr := m_pRateSupport.IsRateSupported(bThin,
                                           fRate,
                                           0);
    end;

  if Failed(hr) then
    begin
      // Unsupported rate.
      goto done;
    end;

  // If there is an operation pending, cache the request.
  if (m_bPending = CMD_PENDING) then
    begin
      m_request.fRate := fRate;
      m_request.bThin := bThin;

      // Remember the current transport state (play, paused, etc), so that we
      // can restore it after the rate change, if necessary. However, if
      // anothercommand is already pending, that one takes precedent.

      if (m_request.command = CmdNone) then
        m_request.command := m_state.command;
    end
  else
    begin
      // No pending operation. Commit the new rate.
      hr := CommitRateChange(fRate,
                             bThin);
      m_bPending := CMD_0;
    end;

done:
  m_critsec.Unlock;
  Result := hr;
end;


function TPlayerSeeking.GetRate(out fRate: Single): HResult;
begin
  if (m_pRate = Nil) then
    begin
      fRate := 1.0;
      Result := E_FAIL;
      Exit;
    end;

  Result := m_pRate.GetRate(m_request.bThin,
                            fRate);
end;


function TPlayerSeeking.GetSafeRates(out flFastestRate: Single;
                                     out flSlowestRate: Single;
                                     RateDirection: MFRATE_DIRECTION): HResult;
var
  hr: HResult;

begin
  hr := m_pRateSupport.GetFastestRate(RateDirection,
                                      m_request.bThin,
                                      flFastestRate);
  if Succeeded(hr) then
    hr := m_pRateSupport.GetSlowestRate(RateDirection,
                                        m_request.bThin,
                                        flSlowestRate);

  Result := hr;
end;


function TPlayerSeeking.CheckForValidRate(var fRate: Single;
                                          RateDirection: MFRATE_DIRECTION): HResult;
var
  hr: HResult;
  flFastestRate: Single;
  flSlowestRate: Single;

begin

  hr := m_pRateSupport.GetFastestRate(RateDirection,
                                      m_request.bThin,
                                      flFastestRate);
  if Succeeded(hr) then
    hr := m_pRateSupport.GetSlowestRate(RateDirection,
                                        m_request.bThin,
                                        flSlowestRate);
  if Succeeded(hr) then
    begin
      if fRate > flFastestRate then
        fRate :=  flFastestRate
      else if fRate < flSlowestRate then
        fRate := flSlowestRate;
      // else rate value will be unchanged
    end;
  Result := hr;
end;


// Sets the playback position.
function TPlayerSeeking.SetPositionInternal(hnsPosition: MFTIME): HResult;
var
  hr: HResult;
  varStart: mfPROPVARIANT;

begin

{$IFDEF DEBUG}
  assert (m_bPending = CMD_0);
{$ENDIF}

  if Not Assigned(m_pSession) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

  varStart.vt := VT_I8;
  varStart.hVal.QuadPart := hnsPosition;


  hr := m_pSession.Start(GUID_NULL,
                         varStart);
  // The Start method can also specify a starting position relative to the start
  // of the file; see the API reference topic for more information.

  if Succeeded(hr) then
    begin
      // Store the pending state.
      m_state.command := CmdStart;
      m_state.hnsStart := hnsPosition;
      m_bPending := CMD_0;
    end;

  Result := hr;
end;

// Sets the playback rate.
function TPlayerSeeking.CommitRateChange(fRate: Single; bThin: Boolean): HResult;
var
  hr: HResult;
  hnsSystemTime: MFTIME;
  hnsClockTime: LongLong;
  cmdNow: TCommand;
  pClock: IMFClock;

label
  done;

begin

{$IFDEF DEBUG}
   assert (m_bPending = CMD_0);
{$ENDIF}

  // Caller holds the lock.
  hnsSystemTime := 0;
  hnsClockTime := 0;
  cmdNow := m_state.command;

  // Allowed rate transitions:

  // Positive <-> negative:   Stopped
  // Negative <-> zero:       Stopped
  // Postive <-> zero:        Paused or stopped

  if ((fRate > 0) And (m_state.fRate <= 0) Or (fRate < 0) And (m_state.fRate >= 0)) then
    begin
      // Transition to stopped.
      if (cmdNow = CmdStart) then
        begin
          // Get the current clock position. This will be the restart time.
          hr := m_pSession.GetClock(pClock);

          if Failed(hr) then
            goto done;

          {void} pClock.GetCorrelatedTime(0,
                                          hnsClockTime,
                                          hnsSystemTime);
          {$IFDEF DEBUG}
            assert(hnsSystemTime <> 0);
          {$ENDIF}

          // Stop and set the rate
          hr := Stop();
          if Failed(hr) then
            goto done;

          // Cache Request: Restart from stop.
          m_request.command := CmdSeek;
          m_request.hnsStart := hnsClockTime;
        end
      else if (cmdNow = CmdPause) then
        begin
          // The current state is paused.

          // For this rate change, the session must be stopped. However, the
          // session cannot transition back from stopped to paused.
          // Therefore, this rate transition is not supported while paused.

          hr := MF_E_UNSUPPORTED_STATE_TRANSITION;
          goto done;
        end
      else if (fRate = 0) And (m_state.fRate <> 0) then
        begin
          if (cmdNow <> CmdPause) then
            begin
             // Transition to paused.

             // This transisition requires the paused state.

             // Pause and set the rate.
             hr := Pause();
             if Failed(hr) then
               goto done;
             // Request: Switch back to current state.
             m_request.command := cmdNow;
            end;
        end;
    end;

  // Set the rate.
  hr := m_pRate.SetRate(bThin,
                        fRate);

  if Failed(hr) then
    goto done;

  // Adjust our current rate and requested rate.
  m_state.fRate := fRate;
  m_request.fRate := m_state.fRate;

done:
  Result := hr;
end;


function TPlayerSeeking.GetNominalRate(): Single;
begin
  Result := m_request.fRate;
end;


// Called when playback starts or restarts.
function TPlayerSeeking.OnSessionStart(hrStatus: HResult): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  if Failed(hrStatus) then
    begin
      Result := hrStatus;
      Exit;
    end;

  // The Media Session completed a start/seek operation.
  // Check if there is another seek request pending.
  UpdatePendingCommands(CmdStart);

  Result := hr;
end;


// Called when playback stops.
function TPlayerSeeking.OnSessionStop(hrStatus: HResult): HResult;
var
  hr: HResult;

begin
  hr := S_OK;
  if Failed(hrStatus) then
    begin
      Result := hrStatus;
      Exit;
    end;

  // The Media Session completed a transition to stopped. This might occur
  // because we are changing playback direction (forward/rewind). Check if
  // there is a pending rate-change request.
  UpdatePendingCommands(CmdStop);

  Result := hr;
end;


// Called when playback pauses.
function TPlayerSeeking.OnSessionPause(hrStatus: HResult): HResult;
var
  hr: HResult;

begin
  if Failed(hrStatus) then
    begin
      Result := hrStatus;
      Exit;
    end;

  hr := UpdatePendingCommands(CmdPause);
  Result := hr;
end;


// Called when the session ends.
function TPlayerSeeking.OnSessionEnded(hrStatus: HResult): HResult;
var
  hr: HResult;

begin
  hr := E_FAIL;
  // After the session ends, playback starts from position zero. But if the
  // current playback rate is reversed, playback would end immediately
  // (reversing from position 0). Therefore, reset the rate to 1x.

  if (GetNominalRate() < 0.0) then
    begin
      m_state.command := CmdStop;
      hr := CommitRateChange(1.0,
                             False);
    end;

  Result := hr;
end;

// Called after an operation completes.
// This method executes any cached requests.
function TPlayerSeeking.UpdatePendingCommands(cmd: TCommand): HResult;
var
  hr: HResult;
  varStart: mfPROPVARIANT;

label
  done;

begin
  hr := S_OK;
  PropVariantInit(varStart);

  m_critsec.Lock;

  if ((m_bPending = CMD_PENDING ) And (m_state.command = cmd)) then
    begin
      m_bPending := CMD_0;

      // The current pending command has completed.

      // First look for rate changes.
      if (m_request.fRate <> m_state.fRate) then
        begin
          hr := CommitRateChange(m_request.fRate,
                                 m_request.bThin);
          if Failed(hr) then
            goto done;
        end;

      // Now look for seek requests.
      if m_bPending = CMD_0 then
        begin
          case m_request.command of
            CmdNone:  begin
                        // Nothing to do.
                      end;

            CmdStart: begin
                        Start();
                      end;

            CmdPause: begin
                        Pause();
                      end;

            CmdStop:  begin
                        Stop();
                      end;

            CmdSeek:  begin
                        hr := SetPositionInternal(m_request.hnsStart);
                      end;
            end;
          m_request.command := CmdNone;
        end;
    end;

done:
  m_critsec.Unlock;
  Result := hr;
end;


end.
