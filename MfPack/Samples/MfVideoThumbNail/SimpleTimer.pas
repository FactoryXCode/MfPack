// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SimpleTimer.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Simple timer class.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.20348.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft samples.
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
unit SimpleTimer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.MMSystem,
  {System}
  System.Classes;

type
  TSimpleTimer = class(TObject)

    m_hTimer: THandle;
    m_bBeginPeriod: LongBool;
    m_StartSysTime: DWORD;
    m_PreviousTime: DWORD;
    m_lPeriodMsec: LONG;

    public

      constructor Create();
      destructor Destroy(); override;

      function InitializeTimer(lPeriodMsec: LONG): BOOL;
      function GetFrameNumber(): DWORD;

    public
    { published methods }

      property Handle: THandle read m_hTimer;
  end;

var
  g_Timer: TSimpleTimer;


implementation


constructor TSimpleTimer.Create();
begin
  inherited Create();
  m_hTimer := 0;
  m_bBeginPeriod := FALSE;
  m_StartSysTime := 0;
  m_lPeriodMsec := 0;
  m_PreviousTime := 0;
end;


destructor TSimpleTimer.Destroy();
begin
  if (m_bBeginPeriod = TRUE) then
    begin
      timeEndPeriod(1);
      m_bBeginPeriod := FALSE;
    end;

  if (m_hTimer > 0) then
    begin
      CloseHandle(m_hTimer);
      m_hTimer := 0;
    end;

  inherited Destroy();
end;


function TSimpleTimer.InitializeTimer(lPeriodMsec: LONG): BOOL;
var
  li: TLargeInteger;

begin
  m_hTimer := CreateWaitableTimer(Nil,
                                  FALSE,
                                  Nil);

  if (m_hTimer = 0) then
    begin
      Result := FALSE;
      Exit;
    end;

  li := 0;

  if Not SetWaitableTimer(m_hTimer,
                          li,
                          lPeriodMsec,
                          Nil,
                          Nil,
                          FALSE) then
    begin
      Result := FALSE;
      Exit;
    end;

  m_bBeginPeriod := (timeBeginPeriod(1) = TIMERR_NOERROR);
  m_StartSysTime := timeGetTime();
  m_lPeriodMsec := lPeriodMsec;
  Result:= TRUE;
end;


function TSimpleTimer.GetFrameNumber(): DWORD;
var
  currentTime: DWORD;
  currentSysTime: DWORD;
  frame: DWORD;
  delta: DWORD;

begin
  currentSysTime := TimeGetTime();

  if (m_StartSysTime > currentSysTime) then
    begin
      currentTime := currentSysTime + ($FFFFFFFF - m_StartSysTime);
    end
  else
    begin
      currentTime := currentSysTime - m_StartSysTime;
    end;

  frame := round(currentTime / m_lPeriodMsec);
  delta := round((currentTime - m_PreviousTime) / m_lPeriodMsec);

  if (delta > 0) then
    begin
      m_PreviousTime := currentTime;
    end;

  Result := frame;
end;

end.
