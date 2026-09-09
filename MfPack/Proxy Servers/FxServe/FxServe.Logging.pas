// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Logging.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Logger unit.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit FxServe.Logging;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.SyncObjs;

type

  TFxServeLogger = class
  private
    FFileName: string;
    FRetentionDays: Integer;
    FCurrentLogDay: TDateTime;
    FLastPruneDay: TDateTime;
    FLock: TCriticalSection;

    function ArchiveFileName(const ADay: TDateTime): string;
    function FileModifiedDay(const AFileName: string;
                             const ADefault: TDateTime): TDateTime;
    procedure AppendFile(const ASourceFileName: string;
                         const ADestinationFileName: string);
    procedure RotateActiveLog(const ADay: TDateTime);
    procedure PruneArchives(const AToday: TDateTime);
    procedure PrepareLogFile(const AToday: TDateTime);

  public

    constructor Create(const AFileName: string;
                       const ARetentionDays: Integer = 14);
    destructor Destroy(); override;

    procedure Write(const ALevel: string;
                    const AMessage: string);

    procedure Info(const AMessage: string);
    procedure Error(const AMessage: string);
  end;


implementation


constructor TFxServeLogger.Create(const AFileName: string;
                                  const ARetentionDays: Integer);
begin

  inherited Create;

  FFileName := AFileName;
  FRetentionDays := ARetentionDays;
  if FRetentionDays < 1 then
    FRetentionDays := 1;

  FCurrentLogDay := 0;
  FLastPruneDay := 0;
  FLock := TCriticalSection.Create();
end;


destructor TFxServeLogger.Destroy();
begin

  FLock.Free;

  inherited Destroy();
end;


function TFxServeLogger.ArchiveFileName(const ADay: TDateTime): string;
begin

  Result := ChangeFileExt(FFileName,
                          '') + '-' +
            FormatDateTime('yyyy-mm-dd',
                           ADay) +
            ExtractFileExt(FFileName);
end;


function TFxServeLogger.FileModifiedDay(const AFileName: string;
                                        const ADefault: TDateTime): TDateTime;
begin

  Result := Trunc(ADefault);

  if FileExists(AFileName) then
    Result := Trunc(TFile.GetLastWriteTime(AFileName));
end;


procedure TFxServeLogger.AppendFile(const ASourceFileName: string;
                                    const ADestinationFileName: string);
var
  SourceStream: TFileStream;
  DestinationStream: TFileStream;

begin

  SourceStream := TFileStream.Create(ASourceFileName,
                                     fmOpenRead or fmShareDenyNone);
  try
    if FileExists(ADestinationFileName) then
      DestinationStream := TFileStream.Create(ADestinationFileName,
                                               fmOpenReadWrite or fmShareDenyNone)
    else
      DestinationStream := TFileStream.Create(ADestinationFileName,
                                               fmCreate or fmShareDenyNone);
    try
      DestinationStream.Seek(0,
                             soEnd);
      DestinationStream.CopyFrom(SourceStream,
                                 0);
    finally
      DestinationStream.Free;
    end;
  finally
    SourceStream.Free;
  end;

  if not DeleteFile(ASourceFileName) then
    RaiseLastOSError();
end;


procedure TFxServeLogger.RotateActiveLog(const ADay: TDateTime);
var
  ArchiveName: string;

begin

  if (FFileName = '') or not FileExists(FFileName) then
    Exit;

  ArchiveName := ArchiveFileName(ADay);

  if FileExists(ArchiveName) then
    AppendFile(FFileName,
               ArchiveName)
  else
    if not RenameFile(FFileName,
                      ArchiveName) then
      RaiseLastOSError();
end;


procedure TFxServeLogger.PruneArchives(const AToday: TDateTime);
var
  SearchRec: TSearchRec;
  DirectoryName: string;
  FileStem: string;
  FileExtension: string;
  Prefix: string;
  CandidateStem: string;
  DateText: string;
  ArchiveDay: TDateTime;
  CutoffDay: TDateTime;
  YearValue: Word;
  MonthValue: Word;
  DayValue: Word;
  ValidDate: Boolean;

begin

  if FFileName = '' then
    Exit;

  DirectoryName := IncludeTrailingPathDelimiter(ExtractFileDir(FFileName));
  FileStem := ChangeFileExt(ExtractFileName(FFileName),
                            '');
  FileExtension := ExtractFileExt(FFileName);
  Prefix := FileStem + '-';
  CutoffDay := Trunc(AToday) - (FRetentionDays - 1);

  if FindFirst(DirectoryName + Prefix + '*' + FileExtension,
               faAnyFile,
               SearchRec) <> 0 then
    Exit;

  try
    repeat
      if (SearchRec.Attr and faDirectory) <> 0 then
        Continue;

      CandidateStem := ChangeFileExt(SearchRec.Name,
                                     '');

      if (Length(CandidateStem) <> Length(Prefix) + 10) or
         (Copy(CandidateStem,
               1,
               Length(Prefix)) <> Prefix) then
        Continue;

      DateText := Copy(CandidateStem,
                       Length(Prefix) + 1,
                       10);

      if (DateText[5] <> '-') or (DateText[8] <> '-') then
        Continue;

      YearValue := Word(StrToIntDef(Copy(DateText,
                                         1,
                                         4),
                                    0));
      MonthValue := Word(StrToIntDef(Copy(DateText,
                                          6,
                                          2),
                                     0));
      DayValue := Word(StrToIntDef(Copy(DateText,
                                        9,
                                        2),
                                   0));

      ArchiveDay := 0;
      ValidDate := True;
      try
        ArchiveDay := EncodeDate(YearValue,
                                 MonthValue,
                                 DayValue);
      except
        ValidDate := False;
      end;

      if ValidDate and (ArchiveDay < CutoffDay) then
        DeleteFile(DirectoryName + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;


procedure TFxServeLogger.PrepareLogFile(const AToday: TDateTime);
var
  TodayValue: TDateTime;

begin

  TodayValue := Trunc(AToday);

  if FCurrentLogDay = 0 then
    begin
      if FileExists(FFileName) then
        FCurrentLogDay := FileModifiedDay(FFileName,
                                          TodayValue)
      else
        FCurrentLogDay := TodayValue;
    end;

  if FileExists(FFileName) and (FCurrentLogDay <> TodayValue) then
    RotateActiveLog(FCurrentLogDay);

  FCurrentLogDay := TodayValue;

  if FLastPruneDay <> TodayValue then
    begin
      PruneArchives(TodayValue);
      FLastPruneDay := TodayValue;
    end;
end;


procedure TFxServeLogger.Write(const ALevel: string;
                               const AMessage: string);
var
  Line: UTF8String;
  ConsoleLine: string;
  Stream: TFileStream;
  Dir: string;
  EntryTime: TDateTime;

begin

  EntryTime := Now;
  Line := UTF8String(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',
                                    EntryTime) + ' [' + ALevel + '] ' + AMessage + sLineBreak);

  FLock.Acquire;

  try
    ConsoleLine := string(Line);

    while (ConsoleLine <> '') and
          CharInSet(ConsoleLine[Length(ConsoleLine)], [#10, #13]) do
      Delete(ConsoleLine,
             Length(ConsoleLine),
             1);

    if (GetConsoleOutputCP <> 0) then
      begin

        try
          Writeln(ConsoleLine);
        except
          { Logging must never stop the HTTP server. }
        end;
      end;

    if (FFileName = '') then
      Exit;

    try
      Dir := ExtractFileDir(FFileName);

      if (Dir <> '') and not DirectoryExists(Dir) then
        ForceDirectories(Dir);

      try
        PrepareLogFile(EntryTime);
      except
        on E: Exception do
          OutputDebugString(PChar('FxServe log rotation error: ' + E.Message));
      end;

      if FileExists(FFileName) then
        Stream := TFileStream.Create(FFileName,
                                     fmOpenReadWrite or fmShareDenyNone)
      else
        Stream := TFileStream.Create(FFileName,
                                     fmCreate or fmShareDenyNone);
      try
        Stream.Seek(0,
                    soEnd);

        if (Length(Line) > 0) then
          Stream.WriteBuffer(Line[1],
                             Length(Line));

      finally
        Stream.Free;
      end;

    except
      on E: Exception do
        OutputDebugString(PChar('FxServe logging error: ' + E.Message));
    end;

  finally
    FLock.Release;
  end;
end;


procedure TFxServeLogger.Info(const AMessage: string);
begin

  Write('INFO',
        AMessage);
end;


procedure TFxServeLogger.Error(const AMessage: string);
begin

  Write('ERROR',
        AMessage);
end;

end.
