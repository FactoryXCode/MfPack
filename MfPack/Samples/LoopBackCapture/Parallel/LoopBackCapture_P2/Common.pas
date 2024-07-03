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
// Revision Version: 3.1.7
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
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
  WinApi.Windows,
  WinAPI.Messages,
  {System}
  System.Sysutils,
  System.Threading,
  System.SyncObjs,
  System.Classes,
  {Vcl}
  Vcl.Grids,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;

  // Undefine this when not needed!
  {$DEFINE SHOW_IN_MESSAGES_IDE}

  // Simple error message handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure ErrMsg(pErrMsg: string;
                   pHr: HResult);

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);

  function EventWait(EventObj: TEvent;
                     Period: Integer = 100): HResult;

  // Grid sorting methods (Author: Peter Below)
  procedure SortStringgrid(Grid: TStringGrid;
                           byColumn: LongInt;
                           ascending: Boolean );


implementation

uses
  Vcl.Forms,
  Vcl.Controls;


// ErrMsg
procedure ErrMsg(pErrMsg: string;
                 pHr: HResult);
begin
{$IFDEF SHOW_IN_MESSAGES_IDE}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
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


procedure SortStringgrid(Grid: TStringGrid;
                         byColumn: LongInt;
                         ascending: Boolean);
  // Helpers
  procedure ExchangeGridRows(i: Integer;
                             j: Integer);
  var
    k: Integer;
  begin
    for k := 0 To Grid.ColCount -1 Do
      Grid.Cols[k].Exchange(i,
                            j);
  end;

  procedure QuickSort(L: Integer;
                      R: Integer);
  var
    I: Integer;
    J: Integer;
    P: string;
  begin
    repeat
      I := L;
      J := R;
      P := Grid.Cells[byColumn, (L + R) shr 1];
      repeat
        while (CompareStr(Grid.Cells[byColumn, I],
                          P) < 0) do
          Inc(I);
        while (CompareStr(Grid.Cells[byColumn, J],
                          P) > 0) do
          Dec(J);
        if (I <= J) then
          begin
            if (I <> J) Then
              ExchangeGridRows(I,
                               J);
            Inc(I);
            Dec(J);
          end;
      until (I > J);

      if (L < J) then
        QuickSort(L, J);
      L := I;
    until (I >= R);
  end;

  procedure InvertGrid();
  var
    i, j: Integer;
  begin
    i := Grid.Fixedrows;
    j := Grid.Rowcount -1;
    while (i < j) do
      begin
        ExchangeGridRows( I, J );
        Inc(i);
        Dec(j);
      end; { While }
   end;

begin
  Screen.Cursor := crHourglass;
  Grid.Perform(WM_SETREDRAW,
               0,
               0);
  try
    QuickSort(Grid.FixedRows,
              Grid.Rowcount-1 );
    if not ascending Then
      InvertGrid();
  finally
    Grid.Perform(WM_SETREDRAW,
                 1,
                 0);
    Grid.Refresh;
    Screen.Cursor := crDefault;
  end;
end;

end.
