//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Tools.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: The helper unit for the Error Lookup Tool.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit Tools;

interface

uses
  WinApi.Windows,
  Vcl.Dialogs,
  System.SysUtils;

  // Undefine this when not needed!
  {$DEFINE SHOW_IN_IDE_MESSAGES}

  function DecimalToHex(aValue: string): string;
  function IsStringANumber(const aValue: string;
                           out aNumber: Integer): Boolean;
  procedure OxValToHexVal(const aOxVal: string;
                          out aHexVal: string;
                          out aDecVal: string);
  // Simple debugMsg message handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure DebugMsg(pErrMsg: string;
                     pHr: HResult);


implementation

uses
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;


function IsStringANumber(const aValue: string;
                         out aNumber: Integer): Boolean;
begin
  Result := ((pos('$',
                  aValue) = 0) or
             (pos('0x',
                  aValue) = 0)) and
             TryStrToInt(aValue,
                         aNumber);
end;


function DecimalToHex(aValue: string): string;
var
  I: Integer;

begin
  if TryStrToInt(aValue, I) then
    Result := '$' + IntToHex(StrToInt(aValue),
                             8)
  else
    Result := '$00000000';
end;


procedure OxValToHexVal(const aOxVal: string;
                        out aHexVal: string;
                        out aDecVal: string);
var
  I: Integer;

begin
  if (pos('0x',
          aOxVal) > 0) then
    aHexVal := '$' + copy(aOxVal,
                          2,
                          Length(aOxVal))
  else if (pos('$',
               aOxVal) > 0) then
         aHexVal := aOxVal

  else if TryStrToInt(aOxVal,
                      I) then
    aHexVal := DecimalToHex(aOxVal)
  else
    begin
      aHexVal := '$00000000';
      aDecVal := '0';
      Exit;
    end;

  if TryStrToInt(aHexVal,
                      I) then
    aDecVal := IntToStr(StrToInt(aHexVal))
  else
    aDecVal := '0';
end;


// DebugMsg
procedure DebugMsg(pErrMsg: string;
                   pHr: HResult);
begin
{$IFDEF SHOW_IN_IDE_MESSAGES}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
end;

end.
