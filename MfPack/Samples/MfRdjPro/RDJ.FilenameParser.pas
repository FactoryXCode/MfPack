// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.FilenameParser.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Parses filenames containing artist and title. This is more
//              convenient than reading tags, because they are most of the
//              time badly maintenanced or not implemented.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit RDJ.FilenameParser;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.StrUtils,
  System.SysUtils;

type

  TFileNameParser = class

  private

  public

    class procedure ResolveArtistTitle(const AFileName: string;
                                       const AArtistTag: string;
                                       const ATitleTag: string;
                                       out AArtist: string;
                                       out ATitle: string);

    class procedure ParseArtistTitleFromFileName(const AFileName: string;
                                                 out AArtist: string;
                                                 out ATitle: string);

    class function BuildIceCastSongText(const AArtist: string;
                                        const ATitle: string;
                                        const AFallback: string = ''): string;
  end;


implementation


class function TFileNameParser.BuildIceCastSongText(const AArtist: string;
                                                    const ATitle: string;
                                                    const AFallback: string = ''): string;

  function CleanText(const S: string): string;
    begin

      Result := Trim(S);
      Result := StringReplace(Result,
                              #13,
                              ' ',
                              [rfReplaceAll]);

      Result := StringReplace(Result,
                              #10,
                              ' ',
                              [rfReplaceAll]);

      while (Pos('  ',
                Result) > 0) do
        Result := StringReplace(Result,
                                '  ',
                                ' ',
                                [rfReplaceAll]);
    end;

var
  ArtistText: string;
  TitleText: string;
  FallbackText: string;

begin

  ArtistText := CleanText(AArtist);
  TitleText := CleanText(ATitle);
  FallbackText := CleanText(AFallback);

  if (ArtistText <> '') and
     (TitleText <> '') then
    Exit(ArtistText + ' - ' + TitleText);

  if (TitleText <> '') then
    Exit(TitleText);

  if (ArtistText <> '') then
    Exit(ArtistText);

  Result := FallbackText;
end;


function StripFileExtOnly(const S: string): string;
begin

  Result := ChangeFileExt(ExtractFileName(S),
                          '');
end;


function CollapseSpaces(const S: string): string;
begin

  Result := Trim(S);

  while (Pos('  ', Result) > 0) do
    Result := StringReplace(Result,
                            '  ',
                            ' ',
                            [rfReplaceAll]);
end;


function IsAllDigits(const S: string): Boolean;
var
  I: Integer;

begin

  Result := (S <> '');
  if not Result then
    Exit;

  for I := 1 to Length(S) do
    if not CharInSet(S[I],
                     ['0'..'9']) then
      Exit(False);
end;


function IsJunkToken(const S: string): Boolean;
var
  T: string;
  N: Integer;

begin

  T := Trim(S);
  if (T = '') then
    Exit(True);

  if IsAllDigits(T) then
    begin

      N := StrToIntDef(T,
                       -1);

      // Throw away likely track numbers, bitrates, years and library ids.
      // Important: also remove "000000", because StrToIntDef returns 0.
      if (N >= 0) and (N <= 999999) then
        Exit(True);
    end;

  Result := False;
end;


function CleanLeadToken(const S: string): string;
var
  T: string;
  I: Integer;

begin

  T := Trim(S);

  // remove leading dashes and spaces
  while (T <> '') and CharInSet(T[1],
                                [' ', '-']) do
    Delete(T,
           1,
           1);

  T := Trim(T);

  // Remove leading numeric track number, e.g. "01 Alex Gaudino"
  I := 1;
  while (I <= Length(T)) and CharInSet(T[I],
                                       ['0'..'9']) do
    Inc(I);

  if (I > 1) and
     (I <= Length(T)) and
     (T[I] = ' ') then
    begin

      Delete(T,
             1,
             I);
      T := Trim(T);
    end;

  Result := CollapseSpaces(T);
end;


class procedure TFileNameParser.ParseArtistTitleFromFileName(const AFileName: string;
                                                             out AArtist: string;
                                                             out ATitle: string);
var
  Base: string;
  Parts: TArray<string>;
  I: Integer;
  FirstUseful: Integer;
  LastUseful: Integer;
  ArtistPart: string;
  TitlePart: string;

begin

  AArtist := '';
  ATitle := '';

  Base := StripFileExtOnly(AFileName);
  Base := StringReplace(Base,
                        #13,
                        ' ',
                        [rfReplaceAll]);

  Base := StringReplace(Base,
                        #10,
                        ' ',
                        [rfReplaceAll]);

  Base := CollapseSpaces(Base);

  if (Base = '') then
    Exit;

  Parts := Base.Split(['-']);
  if (Length(Parts) = 0) then
    Exit;

  FirstUseful := -1;
  LastUseful := -1;

  for I := 0 to High(Parts) do
    begin

      Parts[I] := CollapseSpaces(Parts[I]);

      if (FirstUseful < 0) then
        begin

          if not IsJunkToken(Parts[I]) then
            FirstUseful := I;
        end;
    end;

  for I := High(Parts) downto 0 do
    begin

      if not IsJunkToken(Parts[I]) then
        begin

          LastUseful := I;
          Break;
        end;
    end;

  if (FirstUseful < 0) or
     (LastUseful < 0) or
     (FirstUseful >= LastUseful) then
    begin

      // fallback: no usable split found, return cleaned basename as title
      ATitle := CleanLeadToken(Base);
      Exit;
    end;

  ArtistPart := CleanLeadToken(Parts[FirstUseful]);
  TitlePart := CollapseSpaces(Parts[FirstUseful + 1]);

  // if there are more non-junk pieces between artist and trailing junk,
  // append them to the title with " - " so we do not lose meaningful text.
  if ((FirstUseful + 2) <= LastUseful) then
    begin

      for I := FirstUseful + 2 to LastUseful do
        begin

          if not IsJunkToken(Parts[I]) then
            begin

              if (TitlePart <> '') then
                TitlePart := TitlePart + ' - ' + CollapseSpaces(Parts[I])
              else
                TitlePart := CollapseSpaces(Parts[I]);
            end;
        end;
    end;

  AArtist := CollapseSpaces(ArtistPart);
  ATitle := CollapseSpaces(TitlePart);

  if (AArtist = '') and (ATitle <> '') then
    begin

      AArtist := '';
      Exit;
    end;

  if (ATitle = '') and (AArtist <> '') then
    begin

      ATitle := AArtist;
      AArtist := '';
    end;
end;


class procedure TFileNameParser.ResolveArtistTitle(const AFileName: string;
                                                   const AArtistTag: string;
                                                   const ATitleTag: string;
                                                   out AArtist: string;
                                                   out ATitle: string);
begin

  // Carmen uses filename-based parsing only.
  // Do not trust incoming artist/title values here, because they may already
  // contain an older/fallback/path-derived value.
  ParseArtistTitleFromFileName(AFileName,
                               AArtist,
                               ATitle);

  if (Trim(ATitle) = '') then
    ATitle := CleanLeadToken(StripFileExtOnly(AFileName));

end;

end.
