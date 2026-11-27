// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPlaylistFile.pas
// Kind: Pascal / Delphi unit
// Release date: 26-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: UTF-8 M3U8 persistence for ordered local media paths.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (Carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/sequencer-source-example-code
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
unit MfPlaylistFile;

{$IFDEF FPC}
{$I MfPack.Fpc.inc}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Windows,
  SysUtils,
  Classes;
{$ELSE}
  Winapi.Windows,
  System.SysUtils,
  System.Classes;
{$ENDIF}

  // Load changes AFiles only after the whole document has been read successfully.
  procedure LoadM3u8(const AFileName: string;
                     const AFiles: TStrings);

  // Save replaces the destination atomically after writing a temporary sibling.
  procedure SaveM3u8(const AFileName: string;
                     const AFiles: TStrings);

implementation


function ResolveEntry(const ADirectory,
                      AEntry: string): string;

begin

  if Pos('://', AEntry) > 0 then
    raise EConvertError.Create('This sample supports local media paths, not URLs.');

  if (Length(AEntry) >= 2) and (AEntry[2] = ':') then
    begin

      if (Length(AEntry) < 3) or not CharInSet(AEntry[3], ['\', '/']) then
        raise EConvertError.Create('Drive-relative playlist paths are ambiguous.');
      Result := ExpandFileName(AEntry);
    end
  else
    if Copy(AEntry, 1, 2) = '\\' then
      Result := ExpandFileName(AEntry)
    else
      if (AEntry[1] = '\') or (AEntry[1] = '/') then
        Result := ExpandFileName(ExtractFileDrive(ADirectory) + AEntry)
      else
        Result := ExpandFileName(IncludeTrailingPathDelimiter(ADirectory) + AEntry);
end;


procedure LoadM3u8(const AFileName: string;
                   const AFiles: TStrings);
var
  Stream: TFileStream;
  Bytes: UTF8String;
  Text, Line, Directory: string;
  Lines, Entries: TStringList;
  I: Integer;

begin

  Stream := TFileStream.Create(AFileName,
                               fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > MaxInt then
      raise EReadError.Create('The playlist is too large.');

    SetLength(Bytes,
              Integer(Stream.Size));

    if (Length(Bytes) > 0) then
      Stream.ReadBuffer(Bytes[1],
                        Length(Bytes));
  finally
    Stream.Free();
  end;

  if (Length(Bytes) >= 3) then
    if (Ord(Bytes[1]) = $EF) and (Ord(Bytes[2]) = $BB) and (Ord(Bytes[3]) = $BF) then
      Delete(Bytes,
             1,
             3);

  if (Pos(AnsiChar(#0), Bytes) > 0) then
    raise EConvertError.Create('The playlist contains a null character.');

  Text := UTF8Decode(Bytes);

  if (UTF8Encode(Text) <> Bytes) then
    raise EConvertError.Create('The playlist is not valid UTF-8.');

  Lines := TStringList.Create();
  Entries := TStringList.Create();

  try
    Lines.Text := Text;
    Directory := ExtractFilePath(ExpandFileName(AFileName));

    for I := 0 to Lines.Count - 1 do
      begin

        Line := Lines[I];
        if (Trim(Line) = '') then
          Continue;

        if Line[1] = '#' then
          Continue;
        Entries.Add(ResolveEntry(Directory,
                                 Line));
      end;

    AFiles.Assign(Entries);

  finally
    Entries.Free();
    Lines.Free();
  end;
end;


procedure SaveM3u8(const AFileName: string;
                   const AFiles: TStrings);
var
  Lines: TStringList;
  Bytes: UTF8String;
  Directory: string;
  Destination: string;
  Entry: string;
  TemporaryName: string;
  Temporary: array[0..MAX_PATH] of WideChar;
  Stream: TFileStream;
  I: Integer;

begin

  Destination := ExpandFileName(AFileName);
  Directory := ExtractFilePath(Destination);
  Lines := TStringList.Create();
  TemporaryName := '';

  try
    Lines.Add('#EXTM3U');
    for I := 0 to AFiles.Count - 1 do
      begin

        Entry := AFiles[I];

        if (Trim(Entry) = '') or (Pos(#0, Entry) > 0) or
          (Pos(#10,
               Entry) > 0) or (Pos(#13,
                                   Entry) > 0) then
          raise EConvertError.Create('A media path is empty or contains a line break or null character.');

        if Pos('://', Entry) > 0 then
          raise EConvertError.Create('This sample supports local media paths, not URLs.');

        Entry := ExpandFileName(Entry);
        Entry := ExtractRelativePath(Destination,
                                     Entry);
        if Entry[1] = '#' then
          Entry := '.\' + Entry;

        Lines.Add(Entry);
      end;

    Bytes := UTF8Encode(Lines.Text);

    if GetTempFileNameW(PWideChar(Directory), 'mpl', 0, @Temporary[0]) = 0 then
      RaiseLastOSError();

    TemporaryName := Temporary;
    Stream := TFileStream.Create(TemporaryName,
                                 fmOpenWrite or fmShareExclusive);
    try
      if (Length(Bytes) > 0) then
        Stream.WriteBuffer(Bytes[1],
                           Length(Bytes));

      if not FlushFileBuffers(Stream.Handle) then
        RaiseLastOSError();
    finally
      Stream.Free();
    end;

    if not MoveFileExW(PWideChar(TemporaryName),
                       PWideChar(Destination),
                       MOVEFILE_REPLACE_EXISTING or $00000008 { MOVEFILE_WRITE_THROUGH }) then
      RaiseLastOSError();

    TemporaryName := '';

  finally
    if (TemporaryName <> '') then
      DeleteFileW(PWideChar(TemporaryName));

    Lines.Free();
  end;
end;

end.
