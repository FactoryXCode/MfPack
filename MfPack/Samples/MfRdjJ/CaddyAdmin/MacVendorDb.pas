// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack CaddyAdmin
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MacVendorDb.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Simple UTF-8 text based vendor database engine .
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          Works with RDJ and RDJ Pro Caddy configurations on local or remote servers.
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
//==============================================================================
// Source: https://github.com/ringmast4r/OUI-Master-Database
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
unit MacVendorDb;

interface

uses

  {System}
  System.SysUtils,
  System.StrUtils,
  System.Classes;

type

  TMacVendorDatabase = class
  private
    F24: TStringList;
    F28: TStringList;
    F36: TStringList;
    FLoadedFileName: string;
    function NormalizeMac(const AMac: string): string;
    procedure AddEntry(const ABits: Integer;
                       const APrefix,
                             AVendor: string);
    function ParseDatabaseLine(const ALine: string): Boolean;
    function ParseCsvLine(const ALine: string;
                          const AFields: TStrings): Boolean;
  public

    constructor Create();
    destructor Destroy(); override;

    procedure Clear();
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadNearExecutable(const AFileName: string = 'mac-vendors.txt'): Boolean;
    function Lookup(const AMac: string): string;
    function EntryCount(): Integer;

    property LoadedFileName: string read FLoadedFileName;
  end;


implementation

constructor TMacVendorDatabase.Create();
begin

  inherited Create();

  F24 := TStringList.Create();
  F24.NameValueSeparator := '=';
  F24.CaseSensitive := False;
  F24.Sorted := True;
  F24.Duplicates := dupIgnore;

  F28 := TStringList.Create();
  F28.NameValueSeparator := '=';
  F28.CaseSensitive := False;
  F28.Sorted := True;
  F28.Duplicates := dupIgnore;

  F36 := TStringList.Create();
  F36.NameValueSeparator := '=';
  F36.CaseSensitive := False;
  F36.Sorted := True;
  F36.Duplicates := dupIgnore;
end;


destructor TMacVendorDatabase.Destroy();
begin

  F36.Free();
  F28.Free();
  F24.Free();

  inherited Destroy();
end;


procedure TMacVendorDatabase.Clear();
begin

  F24.Clear();
  F28.Clear();
  F36.Clear();
  FLoadedFileName := '';
end;


function TMacVendorDatabase.NormalizeMac(const AMac: string): string;
var
  I: Integer;
  C: Char;

begin

  Result := '';

  for I := 1 to Length(AMac) do
    begin
      C := UpCase(AMac[I]);
      if CharInSet(C, ['0'..'9', 'A'..'F']) then
        Result := Result + C;
    end;
end;


procedure TMacVendorDatabase.AddEntry(const ABits: Integer;
                                      const APrefix,
                                            AVendor: string);
var
  PrefixText: string;

begin

  PrefixText := NormalizeMac(APrefix);

  if (AVendor = '') then
    Exit;

  { F24/F28/F36 are sorted TStringLists with Duplicates = dupIgnore.
    Never use Values[...] := or Strings[Index] := on a sorted TStringList,
    because both are mutating operations that raise EStringListError.
    Add() is sorted-list safe and duplicate prefixes are ignored. }
  case ABits of
    24: if (Length(PrefixText) >= 6) then
          F24.Add(Copy(PrefixText,
                       1,
                       6) + '=' + AVendor);

    28: if (Length(PrefixText) >= 7) then
          F28.Add(Copy(PrefixText,
                       1,
                       7) + '=' + AVendor);

    36: if (Length(PrefixText) >= 9) then
          F36.Add(Copy(PrefixText,
                       1,
                       9) + '=' + AVendor);
  end;
end;


function TMacVendorDatabase.ParseCsvLine(const ALine: string;
                                         const AFields: TStrings): Boolean;
var
  I: Integer;
  C: Char;
  FieldText: string;
  Quoted: Boolean;

begin

  AFields.Clear();
  FieldText := '';
  Quoted := False;
  I := 1;

  while (I <= Length(ALine)) do
    begin
      C := ALine[I];

      if (C = '"') then
        begin
          if Quoted and (I < Length(ALine)) and (ALine[I + 1] = '"') then
            begin
              FieldText := FieldText + '"';
              Inc(I);
            end
          else
            Quoted := not Quoted;
        end
      else
        if (C = ',') and not Quoted then
          begin
            AFields.Add(FieldText);
            FieldText := '';
          end
        else
          FieldText := FieldText + C;

      Inc(I);
    end;

  AFields.Add(FieldText);
  Result := not Quoted;
end;


function TMacVendorDatabase.ParseDatabaseLine(const ALine: string): Boolean;
var
  Line: string;
  P1: Integer;
  P2: Integer;
  Bits: Integer;
  PrefixText: string;
  VendorText: string;
  BlockType: string;
  Fields: TStringList;

begin

  Result := False;
  Line := Trim(ALine);

  if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then
    Exit;

  // Normalized UTF-8 text format used by UpdateMacVendorList.ps1:
  //    04D9F5=ASUSTeK COMPUTER INC.
  //    480BB25=Solaredge LTD.
  //    001122334=Example vendor
  //
  // Prefix length determines the allocation size: 6 hex digits = MA-L (24),
  // 7 = MA-M (28), and 9 = MA-S/IAB (36).

  P1 := Pos('=',
            Line);

  if (P1 > 1) then
    begin
      PrefixText := Trim(Copy(Line,
                              1,
                              P1 - 1));

      VendorText := Trim(Copy(Line,
                              P1 + 1,
                              MaxInt));

      PrefixText := NormalizeMac(PrefixText);

      case Length(PrefixText) of
        6: Bits := 24;
        7: Bits := 28;
        9: Bits := 36;
      else
        Bits := 0;
      end;

      if (Bits <> 0) and (VendorText <> '') then
        begin
          AddEntry(Bits,
                   PrefixText,
                   VendorText);

          Result := True;
        end;
      Exit;
    end;

  // Native compact format:
  //    24;04D9F5;ASUSTeK COMPUTER INC.
  //    28;480BB25;Solaredge LTD.
  //    36;001122334;Example vendor

  P1 := Pos(';',
            Line);

  if (P1 > 1) then
    begin
      P2 := PosEx(';',
                  Line,
                  P1 + 1);

      if (P2 <= P1 + 1) then
        Exit;

      Bits := StrToIntDef(Trim(Copy(Line,
                                    1,
                                    P1 - 1)),
                          0);

      PrefixText := Trim(Copy(Line,
                              P1 + 1,
                              P2 - P1 - 1));

      VendorText := Trim(Copy(Line,
                              P2 + 1,
                              MaxInt));

      if Bits in [24, 28, 36] then
        begin
          AddEntry(Bits, PrefixText, VendorText);
          Result := True;
        end;
      Exit;
    end;

  { MACLookup CSV format:
      Mac Prefix,Vendor Name,Private,Block Type,Last Updated
      48:0B:B2:5,Solaredge LTD.,false,MA-M,2018/02/15 }
  Fields := TStringList.Create();
  try
    if not ParseCsvLine(Line, Fields) or (Fields.Count < 4) then
      Exit;

    if SameText(Trim(Fields[0]),
                'Mac Prefix') then
      begin
        Result := True;
        Exit;
      end;

    PrefixText := Trim(Fields[0]);
    VendorText := Trim(Fields[1]);
    BlockType := UpperCase(Trim(Fields[3]));

    Bits := 0;
    if (BlockType = 'MA-L') then
      Bits := 24
    else
      if (BlockType = 'MA-M') then
        Bits := 28
      else
        if (BlockType = 'MA-S') or (BlockType = 'IAB') then
          Bits := 36;

    if (Bits <> 0) then
      begin
        AddEntry(Bits,
                 PrefixText,
                 VendorText);
        Result := True;
      end;
  finally
    Fields.Free();
  end;
end;


function TMacVendorDatabase.LoadFromFile(const AFileName: string): Boolean;
var
  Lines: TStringList;
  I: Integer;

begin

  Result := False;
  if not FileExists(AFileName) then
    Exit;

  Clear();
  Lines := TStringList.Create();

  try
    Lines.LoadFromFile(AFileName,
                       TEncoding.UTF8);

    for I := 0 to Lines.Count - 1 do
      ParseDatabaseLine(Lines[I]);

    FLoadedFileName := AFileName;
    Result := (F24.Count + F28.Count + F36.Count) > 0;
  finally
    Lines.Free();
  end;
end;


function TMacVendorDatabase.LoadNearExecutable(const AFileName: string): Boolean;
var
  StartFolder: string;
  Folder: string;
  Candidate: string;
  Level: Integer;

begin

  Result := False;
  StartFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  // mac-vendors.txt is the canonical complete UTF-8 vendor database.
  // Search beside the EXE first, then walk upward so Delphi output folders
  // such as Win32\Debug still find a database stored in the project folder.

  Folder := StartFolder;

  for Level := 0 to 4 do
    begin
      Candidate := IncludeTrailingPathDelimiter(Folder) + AFileName;

      if LoadFromFile(Candidate) then
        begin
          Result := True;
          Exit;
        end;

      Folder := ExtractFileDir(Folder);
      if (Folder = '') then
        Break;
    end;

  { Optional fallback for an unconverted MACLookup CSV database. }
  Folder := StartFolder;
  for Level := 0 to 4 do
    begin
      Candidate := IncludeTrailingPathDelimiter(Folder) +
                   'mac-vendors.csv';

      if LoadFromFile(Candidate) then
        begin
          Result := True;
          Exit;
        end;

      Folder := ExtractFileDir(Folder);
      if Folder = '' then
        Break;
    end;
end;


function TMacVendorDatabase.EntryCount(): Integer;
begin

  Result := F24.Count + F28.Count + F36.Count;
end;


function TMacVendorDatabase.Lookup(const AMac: string): string;
var
  MacText: string;

begin

  Result := '';
  MacText := NormalizeMac(AMac);

  if (Length(MacText) < 12) then
    Exit;

  { Longest prefix wins.  MA-S/IAB beats MA-M, which beats MA-L. }
  Result := F36.Values[Copy(MacText,
                            1,
                            9)];
  if (Result <> '') then
    Exit;

  Result := F28.Values[Copy(MacText,
                            1,
                            7)];
  if (Result <> '') then
    Exit;

  Result := F24.Values[Copy(MacText,
                            1,
                            6)];
end;

end.
