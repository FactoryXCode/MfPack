// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Common.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Helpers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
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
unit Common;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinAPI.Messages,
  {System}
  System.Sysutils,
  System.Threading,
  System.SyncObjs,
  {Vcl}
  Vcl.ComCtrls,
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;

const
  WM_USERINFO = WM_USER + 1;

type

  TMsgOptions  = (optIDE,        // Show info in the IDE Messages
                  optShowMsg);   // Return info in a messagebox

  // Simple error handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure InfoMsg(Options: TMsgOptions;
                    pErrMsg: string;
                    pHr: HResult;
                    hwObj: HWND = 0);

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);

  function EventWait(EventObj: TEvent;
                     Period: Integer = 100): HResult;

  // Delphi MulDiv replacement for Int64 types.
  function _MulDiv64(const aNumber,
                           aNumerator,
                           aDenominator: Int64): Int64;


implementation


// ErrMsg
procedure InfoMsg(Options: TMsgOptions;
                  pErrMsg: string;
                  pHr: HResult;
                  hwObj: HWND = 0);
begin

  if (Options = optIDE) then
    OutputDebugString(StrToPWideChar(Format('%s (hr = %d)',
                                            [pErrMsg, pHr])));

  if (Options = optShowMsg) then
    ShowMessage(format('%s (hr = %d)',
                       [pErrMsg, pHr]));

  if (hwObj > 0) then
    SendMessage(hwObj,
                WM_USERINFO,
                WPARAM(Pointer(format('%s (hr = %d)',
                                      [pErrMsg, pHr]))),
                LPARAM(0));
end;


procedure HandleThreadMessages(AThread: THandle;
                               AWait: Cardinal = INFINITE);
var
  oMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(oMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if oMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(oMsg);
      DispatchMessage(oMsg);
    end;
end;


function EventWait(EventObj: TEvent;
                   Period: Integer = 100): HResult;
var
  hr: HResult;
  wrWaitResult: TWaitResult;

begin

  // Wait for capture to stop
  wrWaitResult := EventObj.WaitFor(Period);

  case wrWaitResult of
    wrSignaled: hr := S_FALSE; // The signal of the event object was set.
    wrTimeout: hr := ERROR_TIMEOUT; // The time specified by the TimeOut parameter elapsed without the signal being set.
    wrAbandoned: hr := ERROR_TIMEOUT;
    wrError: hr := EventObj.LastError;
    wrIOCompletion: hr := S_OK;
    else
      hr := S_OK;
  end;
  Result := hr;
end;


// NOTE:
// MulDiv is 32-bit (Integer in/out). With range checking on, Delphi throws Range check error.
// If working with Int64, use this method, instead of MulDiv.
// Note: In later MfPack versions (> version 3.18), this method will be declared in WinApi.MediaFoundationApi.MfUtils.
function _MulDiv64(const aNumber,
                         aNumerator,
                         aDenominator: Int64): Int64;
begin

  if (aDenominator <= 0) then
    Exit(0);

  // Avoid Int64 overflow: (QpcDelta * 10_000_000) div PerfFreq
  Result := (aNumber div aDenominator) * aNumerator +
            (aNumber mod aDenominator) * aNumerator div aDenominator;
end;

end.
