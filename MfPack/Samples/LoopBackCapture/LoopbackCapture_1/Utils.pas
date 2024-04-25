
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Utils.pas
// Kind: Pascal Unit
// Release date: 12-03-2023
// Language: ENU
//
// Revision Version: 3.1.6
//
// Description:
//   This unit contains helper methods for the LoopBack sample.
//
// Organisation: FactoryX
// Initiator(s): maXcomX
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
// 25/04/2004 Tony                Updated to a more stable and crack free version.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPack/Samples/LoopbackCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: FactoryX
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
unit Utils;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.Sysutils,
  {Vcl}
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;

  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}

  // Simple error handler
  procedure ErrMsg(pErrMsg: string;
                   pHr: HResult);

  // See also WinApi.MediaFoundationApi.MfUtils/HnsTimeToStr
  function TranslateHnsTimeToStr(hns: MFTIME;
                                 ShowMilliSeconds: Boolean = True;
                                 DelimiterFormat: string = ':'): string; inline;

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);


implementation


// ErrMsg
procedure ErrMsg(pErrMsg: string;
                 pHr: HResult);
begin
{$IFDEF SAVE_DEBUG_REPORT}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
end;


// Converts Hns to a time string format
function TranslateHnsTimeToStr(hns: MFTIME;
                               ShowMilliSeconds: Boolean = True;
                               DelimiterFormat: string = ':'): string; inline;
var
  hours,
  mins,
  secs,
  millisec: Word;

begin
try
  hours := hns div MFTIME(36000000000);
  hns := hns mod MFTIME(36000000000);

  mins := hns div 600000000;
  hns := hns mod 600000000;

  secs := hns div 10000000;
  hns := hns mod 10000000;

  millisec := hns div 10000;

  if ShowMilliSeconds then
    Result := Format('%2.2d%s%2.2d%s%2.2d,%3.3d', [hours, DelimiterFormat, mins, DelimiterFormat, secs, DelimiterFormat, millisec])
  else
    Result := Format('%2.2d%s%2.2d%s%2.2d', [hours, DelimiterFormat, mins, DelimiterFormat, secs]);

except
  on exception do Result:= Format('00:00:00%s000', [DelimiterFormat]);
end;
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

end.
