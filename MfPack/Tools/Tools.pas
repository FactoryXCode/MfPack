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
// Revision Version: 3.1.9
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
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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

  function TryParseHResult(const aValue: string;
                           out aNumber: HResult): Boolean;
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

function TryParseHResult(const aValue: string;
                         out aNumber: HResult): Boolean;
var
  Text: string;
  Index: Integer;
  Base: Integer;
  Digit: Integer;
  Value: UInt64;
  Negative: Boolean;
begin
  Result := False;
  aNumber := 0;
  Text := Trim(aValue);
  if Text = '' then Exit;
  Index := 1;
  Negative := Text[Index] = '-';
  if Negative or (Text[Index] = '+') then Inc(Index);
  if Index > Length(Text) then Exit;
  Base := 10;
  if Text[Index] = '$' then
    begin
      Base := 16;
      Inc(Index);
    end
  else if (Index < Length(Text)) and (Text[Index] = '0') and
          ((Text[Index + 1] = 'x') or (Text[Index + 1] = 'X')) then
    begin
      Base := 16;
      Inc(Index,2);
    end;
  if (Index > Length(Text)) or (Negative and (Base <> 10)) then Exit;
  Value := 0;
  while Index <= Length(Text) do
    begin
      case Text[Index] of
        '0'..'9': Digit := Ord(Text[Index]) - Ord('0');
        'a'..'f': Digit := Ord(Text[Index]) - Ord('a') + 10;
        'A'..'F': Digit := Ord(Text[Index]) - Ord('A') + 10;
        else Exit;
      end;
      if Digit >= Base then Exit;
      Value := Value * UInt64(Base) + UInt64(Digit);
      if Value > UInt64($FFFFFFFF) then Exit;
      Inc(Index);
    end;
  if Negative then
    begin
      if Value > UInt64($80000000) then Exit;
      aNumber := HResult(-Int64(Value));
    end
  else
    aNumber := HResult(DWORD(Value));
  Result := True;
end;


function IsStringANumber(const aValue: string;
                         out aNumber: Integer): Boolean;
var
  Number: HResult;
begin
  Result := (Pos('$',aValue) = 0) and (Pos('x',aValue) = 0) and
            (Pos('X',aValue) = 0) and TryParseHResult(aValue,Number);
  if Result then aNumber := Number;
  if not Result then aNumber := 0;
end;


function DecimalToHex(aValue: string): string;
var
  Number: HResult;
begin
  if TryParseHResult(aValue,Number) then
    Result := '$' + UnicodeString(IntToHex(DWORD(Number),8))
  else
    Result := '$00000000';
end;


procedure OxValToHexVal(const aOxVal: string;
                        out aHexVal: string;
                        out aDecVal: string);
var
  Number: HResult;
begin
  if not TryParseHResult(aOxVal,Number) then Number := 0;
  aHexVal := '$' + UnicodeString(IntToHex(DWORD(Number),8));
  aDecVal := UnicodeString(IntToStr(Number));
end;


procedure DebugMsg(pErrMsg: string;
                   pHr: HResult);
var
  Text: UnicodeString;
begin
  Text := 'Error: ' + pErrMsg + ' (hr = ' + UnicodeString(IntToStr(pHr)) + ')';
{$IFDEF SHOW_IN_IDE_MESSAGES}
  OutputDebugStringW(PWideChar(Text));
{$ELSE}
  ShowMessage({$IFDEF FPC}UTF8Encode({$ENDIF}Text{$IFDEF FPC}){$ENDIF});
{$ENDIF}
end;

end.
