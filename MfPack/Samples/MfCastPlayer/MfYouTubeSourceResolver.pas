// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfYouTubeSourceResolver.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Resolver for Youtube URL' like "https://www.youtube.com/watch?v=m9Py1mHq45M".
//              It deliberately keeps YouTube-specific behavior out of MfCast and
//              supports both immediate and best-quality media resolution.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// =============================================================================
// Source: -
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
unit MfYouTubeSourceResolver;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  {Cast}
  MfCastInterfaces;

type

  TMfYouTubeResolveMode = (yrmFastCombinedStream,
                           yrmBestQualityDownload);


  TMfYouTubeSourceResolver = class(TInterfacedObject, IMfCastSourceResolver)
  private
    FLastErrorText: string;
    FLastResolvedSourceName: string;
    FMode: TMfYouTubeResolveMode;

    function ExtractVideoId(const ASourceName: string;
                            out AVideoId: string): Boolean;

    function FindYtDlp(out AExecutableName: string): Boolean;

    function FindFFmpeg(const AYtDlpExecutableName: string;
                        out AExecutableName: string): Boolean;

    function RunYtDlp(const AExecutableName: string;
                      const AFFmpegExecutableName: string;
                      const ASourceName: string;
                      const AOutputTemplate: string): HRESULT;

    function RunYtDlpGetUrl(const AExecutableName: string;
                            const ASourceName: string;
                            const AWorkingDirectory: string;
                            out AResolvedUrl: string): HRESULT;

    function FindResolvedFile(const ADirectory: string;
                              const AVideoId: string;
                              out AFileName: string): Boolean;
  public

    constructor Create();

    function CanResolve(const ASourceName: string): Boolean;

    function Resolve(const ASourceName: string;
                     out AResolvedSource: TMfCastResolvedSource): HRESULT;

    function GetLastErrorText(): string;

    procedure Release(const AResolvedSource: TMfCastResolvedSource);

    property Mode: TMfYouTubeResolveMode read FMode write FMode;
    property LastResolvedSourceName: string read FLastResolvedSourceName;
  end;


implementation


constructor TMfYouTubeSourceResolver.Create();
begin

  inherited Create();

  FMode := yrmFastCombinedStream;
  FLastResolvedSourceName := '';
end;

function QuoteArgument(const AValue: string): string;
begin

  Result := '"' + StringReplace(AValue,
                                '"',
                                '%22',
                                [rfReplaceAll]) + '"';
end;


function TMfYouTubeSourceResolver.ExtractVideoId(const ASourceName: string;
                                                 out AVideoId: string): Boolean;
var
  Source: string;
  LowerSource: string;
  StartIndex: Integer;
  EndIndex: Integer;

begin

  AVideoId := '';
  Source := Trim(ASourceName);
  LowerSource := LowerCase(Source);

  StartIndex := Pos('youtu.be/',
                    LowerSource);
  if (StartIndex > 0) then
    Inc(StartIndex,
        Length('youtu.be/'))
  else
    begin
      StartIndex := Pos('youtube.com/watch?',
                        LowerSource);
      if (StartIndex > 0) then
        begin
          StartIndex := Pos('v=',
                            Copy(LowerSource,
                                 StartIndex,
                                 MaxInt));
          if (StartIndex > 0) then
            begin
              StartIndex := Pos('youtube.com/watch?',
                                LowerSource) + StartIndex + 1;
            end;
        end
      else
        begin
          StartIndex := Pos('youtube.com/shorts/',
                            LowerSource);
          if (StartIndex > 0) then
            Inc(StartIndex,
                Length('youtube.com/shorts/'))
          else
            begin
              StartIndex := Pos('youtube.com/embed/',
                                LowerSource);
              if (StartIndex > 0) then
                Inc(StartIndex,
                    Length('youtube.com/embed/'));
            end;
        end;
    end;

  if (StartIndex <= 0) or (StartIndex > Length(Source)) then
    begin
      Result := False;
      Exit;
    end;

  EndIndex := StartIndex;
  while (EndIndex <= Length(Source)) and
        CharInSet(Source[EndIndex],
                  ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) do
    Inc(EndIndex);

  AVideoId := Copy(Source,
                   StartIndex,
                   EndIndex - StartIndex);

  Result := Length(AVideoId) = 11;
  if not Result then
    AVideoId := '';
end;


function TMfYouTubeSourceResolver.FindYtDlp(out AExecutableName: string): Boolean;
var
  SearchDirectory: string;
  ParentDirectory: string;
  CandidateName: string;
  Level: Integer;
  SearchBuffer: array[0..MAX_PATH - 1] of Char;
  SearchLength: DWORD;
  FilePart: PChar;

begin

  AExecutableName := '';
  SearchDirectory := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  // Build output folders can be nested below the project (for example,
  // Win32\Debug). Walk upward to find the persistent project Binaries folder.
  for Level := 0 to 5 do
    begin
      CandidateName := IncludeTrailingPathDelimiter(SearchDirectory) +
                       'Binaries\yt-dlp_x86.exe';

      if FileExists(CandidateName) then
        begin
          AExecutableName := CandidateName;
          Result := True;
          Exit;
        end;

      CandidateName := IncludeTrailingPathDelimiter(SearchDirectory) +
                       'Binaries\yt-dlp.exe';
      if FileExists(CandidateName) then
        begin
          AExecutableName := CandidateName;
          Result := True;
          Exit;
        end;

      ParentDirectory := ExtractFileDir(SearchDirectory);
      if SameText(ParentDirectory,
                  SearchDirectory) then
        Break;
      SearchDirectory := ParentDirectory;
    end;

  FillChar(SearchBuffer,
           SizeOf(SearchBuffer),
           0);

  FilePart := nil;

  SearchLength := SearchPath(nil,
                             'yt-dlp_x86.exe',
                             nil,
                             Length(SearchBuffer),
                             SearchBuffer,
                             FilePart);

  Result := (SearchLength > 0) and
            (SearchLength < DWORD(Length(SearchBuffer)));

  if not Result then
    begin
      FillChar(SearchBuffer,
               SizeOf(SearchBuffer),
               0);

      FilePart := nil;

      SearchLength := SearchPath(nil,
                                 'yt-dlp.exe',
                                 nil,
                                 Length(SearchBuffer),
                                 SearchBuffer,
                                 FilePart);

      Result := (SearchLength > 0) and
                (SearchLength < DWORD(Length(SearchBuffer)));
    end;

  if Result then
    AExecutableName := SearchBuffer
  else
    AExecutableName := '';
end;


function TMfYouTubeSourceResolver.FindFFmpeg(const AYtDlpExecutableName: string;
                                             out AExecutableName: string): Boolean;
var
  BinariesDirectory: string;
  SearchRecord: TSearchRec;
  FindResult: Integer;
  SearchBuffer: array[0..MAX_PATH - 1] of Char;
  SearchLength: DWORD;
  FilePart: PChar;

begin

  BinariesDirectory := ExcludeTrailingPathDelimiter(
                         ExtractFilePath(AYtDlpExecutableName));

  AExecutableName := IncludeTrailingPathDelimiter(BinariesDirectory) +
                     'ffmpeg.exe';

  if FileExists(AExecutableName) then
    begin
      Result := True;
      Exit;
    end;

  // Also accept the versioned top-level directory produced by the standard
  // FFmpeg essentials ZIP, for example ffmpeg-9.0.1-essentials_build\bin.
  FillChar(SearchRecord,
           SizeOf(SearchRecord),
           0);
  FindResult := FindFirst(IncludeTrailingPathDelimiter(BinariesDirectory) +
                          'ffmpeg-*',
                          faDirectory,
                          SearchRecord);

  if FindResult = 0 then
    try
      while (FindResult = 0) do
        begin
          if ((SearchRecord.Attr and faDirectory) <> 0) and
             (SearchRecord.Name <> '.') and (SearchRecord.Name <> '..') then
            begin
              AExecutableName := IncludeTrailingPathDelimiter(BinariesDirectory) +
                                 SearchRecord.Name + '\bin\ffmpeg.exe';

              if FileExists(AExecutableName) then
                begin
                  Result := True;
                  Exit;
                end;
            end;
          FindResult := FindNext(SearchRecord);
        end;
    finally
      FindClose(SearchRecord);
    end;

  AExecutableName := IncludeTrailingPathDelimiter(BinariesDirectory) +
                     'ffmpeg\bin\ffmpeg.exe';

  if FileExists(AExecutableName) then
    begin
      Result := True;
      Exit;
    end;

  FillChar(SearchBuffer,
           SizeOf(SearchBuffer),
           0);

  FilePart := nil;

  SearchLength := SearchPath(nil,
                             'ffmpeg.exe',
                             nil,
                             Length(SearchBuffer),
                             SearchBuffer,
                             FilePart);

  Result := (SearchLength > 0) and
            (SearchLength < DWORD(Length(SearchBuffer)));

  if Result then
    AExecutableName := SearchBuffer
  else
    AExecutableName := '';
end;


function TMfYouTubeSourceResolver.RunYtDlp(const AExecutableName: string;
                                           const AFFmpegExecutableName: string;
                                           const ASourceName: string;
                                           const AOutputTemplate: string): HRESULT;
const
  FormatSelector = 'bv*[ext=mp4][vcodec^=avc1]+ba[ext=m4a]/' +
                   'bv*[vcodec^=avc1]+ba/b';
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttributes: TSecurityAttributes;
  CommandLine: string;
  ExitCode: DWORD;
  LogFileName: string;
  LogHandle: THandle;
  LogLines: TStringList;
  DiagnosticText: string;

begin

  FillChar(StartupInfo,
           SizeOf(StartupInfo),
           0);

  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;

  FillChar(ProcessInfo,
           SizeOf(ProcessInfo),
           0);

  LogFileName := IncludeTrailingPathDelimiter(ExtractFilePath(AOutputTemplate)) +
                 'yt-dlp-last-run.log';

  FillChar(SecurityAttributes,
           SizeOf(SecurityAttributes),
           0);

  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;

  LogHandle := CreateFile(PChar(LogFileName),
                          GENERIC_WRITE,
                          FILE_SHARE_READ,
                          @SecurityAttributes,
                          CREATE_ALWAYS,
                          FILE_ATTRIBUTE_NORMAL,
                          0);

  if (LogHandle = INVALID_HANDLE_VALUE) then
    begin
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  StartupInfo.hStdOutput := LogHandle;
  StartupInfo.hStdError := LogHandle;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

  CommandLine := QuoteArgument(AExecutableName) +
                 ' --no-playlist --no-part --format ' +
                 QuoteArgument(FormatSelector) +
                 ' --merge-output-format mp4 --ffmpeg-location ' +
                 QuoteArgument(AFFmpegExecutableName) +
                 ' --output ' + QuoteArgument(AOutputTemplate) +
                 ' ' + QuoteArgument(ASourceName);

  UniqueString(CommandLine);

  if not CreateProcess(nil,
                       PChar(CommandLine),
                       nil,
                       nil,
                       True,
                       CREATE_NO_WINDOW,
                       nil,
                       PChar(ExtractFilePath(AExecutableName)),
                       StartupInfo,
                       ProcessInfo) then
    begin
      CloseHandle(LogHandle);
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  try
    if (WaitForSingleObject(ProcessInfo.hProcess,
                           INFINITE) <> WAIT_OBJECT_0) then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError());
        Exit;
      end;

    ExitCode := DWORD(-1);
    if not GetExitCodeProcess(ProcessInfo.hProcess,
                              ExitCode) then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError());
        Exit;
      end;

    if (ExitCode = 0) then
      Result := S_OK
    else
      begin
        FLastErrorText := Format('yt-dlp exited with code %d.',
                                 [ExitCode]);
        Result := E_FAIL;
      end;
  finally
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(LogHandle);
  end;

  if FAILED(Result) and FileExists(LogFileName) then
    begin
      LogLines := TStringList.Create();

      try
        LogLines.LoadFromFile(LogFileName);
        DiagnosticText := Trim(LogLines.Text);

        if (Length(DiagnosticText) > 2000) then
          DiagnosticText := Copy(DiagnosticText,
                                 Length(DiagnosticText) - 1999,
                                 2000);

        if (DiagnosticText <> '') then
          FLastErrorText := DiagnosticText;

      finally
        LogLines.Free();
      end;
    end;

  if SUCCEEDED(Result) and FileExists(LogFileName) then
    DeleteFile(LogFileName);
end;


function TMfYouTubeSourceResolver.RunYtDlpGetUrl(const AExecutableName: string;
                                                 const ASourceName: string;
                                                 const AWorkingDirectory: string;
                                                 out AResolvedUrl: string): HRESULT;
const

  // Format 18 is normally selected here: one progressive MP4 containing
  // H.264 video and AAC audio. Chromecast can consume it immediately without
  // first downloading and merging separate YouTube tracks.
  FormatSelector = 'b[ext=mp4][vcodec^=avc1][acodec^=mp4a]/' +
                   'b[ext=mp4]/b';

var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttributes: TSecurityAttributes;
  CommandLine: string;
  ExitCode: DWORD;
  LogFileName: string;
  LogHandle: THandle;
  LogLines: TStringList;
  DiagnosticText: string;
  I: Integer;
  Line: string;

begin

  AResolvedUrl := '';

  FillChar(StartupInfo,
           SizeOf(StartupInfo),
           0);

  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;

  FillChar(ProcessInfo,
           SizeOf(ProcessInfo),
           0);

  LogFileName := IncludeTrailingPathDelimiter(AWorkingDirectory) + 'yt-dlp-fast-last-run.log';

  FillChar(SecurityAttributes,
           SizeOf(SecurityAttributes),
           0);

  SecurityAttributes.nLength := SizeOf(SecurityAttributes);

  SecurityAttributes.bInheritHandle := True;

  LogHandle := CreateFile(PChar(LogFileName),
                          GENERIC_WRITE,
                          FILE_SHARE_READ,
                          @SecurityAttributes,
                          CREATE_ALWAYS,
                          FILE_ATTRIBUTE_NORMAL,
                          0);

  if (LogHandle = INVALID_HANDLE_VALUE) then
    begin
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  StartupInfo.hStdOutput := LogHandle;
  StartupInfo.hStdError := LogHandle;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

  CommandLine := QuoteArgument(AExecutableName) +
                 ' --no-playlist --no-warnings --extractor-args ' +
                 QuoteArgument('youtube:player_client=android') +
                 ' --format ' + QuoteArgument(FormatSelector) +
                 ' --get-url ' + QuoteArgument(ASourceName);


  UniqueString(CommandLine);

  if not CreateProcess(nil,
                       PChar(CommandLine),
                       nil,
                       nil,
                       True,
                       CREATE_NO_WINDOW,
                       nil,
                       PChar(ExtractFilePath(AExecutableName)),
                       StartupInfo,
                       ProcessInfo) then
    begin
      CloseHandle(LogHandle);
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  try
    if WaitForSingleObject(ProcessInfo.hProcess,
                           INFINITE) <> WAIT_OBJECT_0 then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError());
        Exit;
      end;

    ExitCode := DWORD(-1);
    if not GetExitCodeProcess(ProcessInfo.hProcess,
                              ExitCode) then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError());
        Exit;
      end;

    if (ExitCode = 0) then
      Result := S_OK
    else
      begin
        FLastErrorText := Format('yt-dlp exited with code %d.',
                                 [ExitCode]);
        Result := E_FAIL;
      end;
  finally
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(LogHandle);
  end;

  if FileExists(LogFileName) then
    begin
      LogLines := TStringList.Create();
      try
        LogLines.LoadFromFile(LogFileName);

        if SUCCEEDED(Result) then
          for I := 0 to LogLines.Count - 1 do
            begin
              Line := Trim(LogLines[I]);
              if (Pos('https://', LowerCase(Line)) = 1) or
                 (Pos('http://', LowerCase(Line)) = 1) then
                begin
                  AResolvedUrl := Line;
                  Break;
                end;
            end;

        if (AResolvedUrl = '') then
          begin
            DiagnosticText := Trim(LogLines.Text);

            if (Length(DiagnosticText) > 2000) then
              DiagnosticText := Copy(DiagnosticText,
                                     Length(DiagnosticText) - 1999,
                                     2000);

            if (DiagnosticText <> '') then
              FLastErrorText := DiagnosticText;
          end;
      finally
        LogLines.Free();
      end;
    end;

  if SUCCEEDED(Result) and (AResolvedUrl = '') then
    begin
      FLastErrorText := 'yt-dlp completed but did not return a playable media URL.';
      Result := E_FAIL;
    end;

  if SUCCEEDED(Result) and FileExists(LogFileName) then
    DeleteFile(LogFileName);
end;


function TMfYouTubeSourceResolver.FindResolvedFile(const ADirectory: string;
                                                   const AVideoId: string;
                                                   out AFileName: string): Boolean;
var
  SearchRecord: TSearchRec;
  Extension: string;
  SearchResult: Integer;

begin

  AFileName := '';
  FillChar(SearchRecord,
           SizeOf(SearchRecord),
           0);

  SearchResult := FindFirst(IncludeTrailingPathDelimiter(ADirectory) + AVideoId + '.*',
                            faAnyFile,
                            SearchRecord);

  if (SearchResult <> 0) then
    begin
      Result := False;
      Exit;
    end;

  try
    while SearchResult = 0 do
      begin
        Extension := LowerCase(ExtractFileExt(SearchRecord.Name));

        if ((SearchRecord.Attr and faDirectory) = 0) and
           ((Extension = '.mp4') or
            (Extension = '.webm') or
            (Extension = '.mkv') or
            (Extension = '.m4a')) then
          begin
            AFileName := IncludeTrailingPathDelimiter(ADirectory) +
                         SearchRecord.Name;
            Result := True;
            Exit;
          end;
        SearchResult := FindNext(SearchRecord);
      end;
  finally
    FindClose(SearchRecord);
  end;
  Result := False;
end;


function TMfYouTubeSourceResolver.CanResolve(
  const ASourceName: string): Boolean;
var
  VideoId: string;

begin
  Result := ExtractVideoId(ASourceName,
                           VideoId);
end;


function TMfYouTubeSourceResolver.Resolve(const ASourceName: string;
                                          out AResolvedSource: TMfCastResolvedSource): HRESULT;
var
  VideoId: string;
  ExecutableName: string;
  FFmpegExecutableName: string;
  TempPathBuffer: array[0..MAX_PATH - 1] of Char;
  TempDirectory: string;
  OutputTemplate: string;
  ResolvedFileName: string;
  ResolvedUrl: string;

begin

  AResolvedSource.SourceName := '';
  AResolvedSource.Title := '';
  AResolvedSource.IsTemporary := False;
  AResolvedSource.ContentType := '';
  AResolvedSource.ContainerName := '';
  AResolvedSource.PreferDirectPlayback := False;
  FLastErrorText := '';
  FLastResolvedSourceName := '';

  if not ExtractVideoId(ASourceName,
                        VideoId) then
    begin
      FLastErrorText := 'The URL is not a supported YouTube video URL.';
      Result := E_INVALIDARG;
      Exit;
    end;

  if not FindYtDlp(ExecutableName) then
    begin
      FLastErrorText := 'yt-dlp was not found in the MfCastPlayer\Binaries directory or on PATH.';
      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  FillChar(TempPathBuffer,
           SizeOf(TempPathBuffer),
           0);

  if (GetTempPath(Length(TempPathBuffer),
                 TempPathBuffer) = 0) then
    begin
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  TempDirectory := IncludeTrailingPathDelimiter(TempPathBuffer) +
                   'MfCastPlayer';

  if not ForceDirectories(TempDirectory) then
    begin
      FLastErrorText := 'The temporary MfCastPlayer directory could not be created.';
      Result := E_FAIL;
      Exit;
    end;

  if FMode = yrmFastCombinedStream then
    begin
      Result := RunYtDlpGetUrl(ExecutableName,
                               ASourceName,
                               TempDirectory,
                               ResolvedUrl);
      if FAILED(Result) then
        Exit;

      AResolvedSource.SourceName := ResolvedUrl;
      AResolvedSource.Title := 'YouTube ' + VideoId;
      AResolvedSource.IsTemporary := False;
      AResolvedSource.ContentType := 'video/mp4';
      AResolvedSource.ContainerName := 'MP4';
      AResolvedSource.PreferDirectPlayback := True;
      FLastResolvedSourceName := ResolvedUrl;
      Result := S_OK;
      Exit;
    end;

  if not FindFFmpeg(ExecutableName,
                    FFmpegExecutableName) then
    begin
      FLastErrorText := 'This YouTube video has separate video and audio tracks. ' +
                        'ffmpeg.exe was not found in MfCastPlayer\Binaries, ' +
                        'an extracted ffmpeg-*\bin directory, or on PATH.';

      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  OutputTemplate := IncludeTrailingPathDelimiter(TempDirectory) +
                    VideoId + '.%(ext)s';

  Result := RunYtDlp(ExecutableName,
                     FFmpegExecutableName,
                     ASourceName,
                     OutputTemplate);
  if FAILED(Result) then
    Exit;

  if not FindResolvedFile(TempDirectory,
                          VideoId,
                          ResolvedFileName) then
    begin
      FLastErrorText := 'yt-dlp completed but no playable output file was found.';
      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  AResolvedSource.SourceName := ResolvedFileName;
  AResolvedSource.Title := 'YouTube ' + VideoId;
  AResolvedSource.IsTemporary := True;
  Result := S_OK;
end;


function TMfYouTubeSourceResolver.GetLastErrorText(): string;
begin
  Result := FLastErrorText;
end;


procedure TMfYouTubeSourceResolver.Release(
  const AResolvedSource: TMfCastResolvedSource);
begin
  if AResolvedSource.IsTemporary and
     (AResolvedSource.SourceName <> '') and
     FileExists(AResolvedSource.SourceName) then
    DeleteFile(AResolvedSource.SourceName);
end;

end.
