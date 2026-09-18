// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServePublisher.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: fMP4 file publisher for the FxServe web root.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
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
unit FxServePublisher;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
  {Application}
  SimpleAvCapture;

type

  TFxServePublisher = class
  private
    FActive: Boolean;
    FFolder: string;
    FSessionId: string;
    FLastSourceSequence: UInt64;
    FFirstPublishedSequence: UInt64;
    FLastPublishedSequence: UInt64;
    FManifestPublishSequence: UInt64;
    FInitWritten: Boolean;
    FPlayerWritten: Boolean;
    FLastError: string;

    function FileName(const AName: string): string;

    function WriteBytes(const AFileName: string;
                        const ABytes: TBytes): Boolean;

    function WriteUtf8(const AFileName: string;
                       const AText: string): Boolean;

    function CopyAsset(const ASourceFileName: string;
                       const ADestinationName: string): Boolean;

    function CopyPwaAssets(const APlayerSource: string): Boolean;
    procedure CleanupPublishedFiles();
    procedure CleanupOldFragments();

    function WriteManifest(const ALive: Boolean): Boolean;
    procedure SetLastError(const AText: string);

  public

    constructor Create();
    destructor Destroy(); override;

    function Start(const AFolder: string;
                   const APlayerSource: string): Boolean;
    procedure Stop();

    function Service(const ACapture: TSimpleAvCapture;
                     out APublishedCount: Integer): Boolean;

    property Active: Boolean read FActive;
    property Folder: string read FFolder;
    property SessionId: string read FSessionId;
    property FirstPublishedSequence: UInt64 read FFirstPublishedSequence;
    property LastPublishedSequence: UInt64 read FLastPublishedSequence;
    property LastError: string read FLastError;
  end;

implementation

const
  PUBLISH_FRAGMENT_PREFIX = 'patched_frag_';
  PUBLISH_FRAGMENT_EXTENSION = '.m4s';
  PUBLISH_KEEP_FRAGMENTS = 30;
  PUBLISH_MANIFEST_POLL_MS = 250;
  PUBLISH_FRAGMENT_TARGET_MS = 1000;


function BytesFromStream(const AStream: TStream): TBytes;
begin

  SetLength(Result,
            AStream.Size);
  if (AStream.Size > 0) then
    begin
      AStream.Position := 0;
      AStream.ReadBuffer(Result[0],
                         AStream.Size);
    end;
end;


constructor TFxServePublisher.Create();
begin

  inherited Create();

  FActive := False;
  FFolder := '';
  FSessionId := '';
  FLastError := '';
end;


destructor TFxServePublisher.Destroy();
begin

  Stop();

  inherited Destroy;
end;


procedure TFxServePublisher.SetLastError(const AText: string);
begin
  FLastError := AText;
end;


function TFxServePublisher.FileName(const AName: string): string;
begin

  Result := IncludeTrailingPathDelimiter(FFolder) + AName;
end;


function TFxServePublisher.WriteBytes(const AFileName: string;
                                      const ABytes: TBytes): Boolean;
var
  TempName: string;
  Stream: TFileStream;
  I: Integer;
  ErrorCode: DWORD;

begin

  Result := False;
  if (AFileName = '') or (Length(ABytes) = 0) then
    Exit;

  TempName := AFileName + Format('.tmp_%x_%x_%x',
                                [GetCurrentProcessId, GetCurrentThreadId, GetTickCount]);

  try
    Stream := TFileStream.Create(TempName,
                                 fmCreate or fmShareDenyWrite);
    try
      Stream.WriteBuffer(ABytes[0],
                         Length(ABytes));
    finally
      Stream.Free;
    end;

    for I := 0 to 20 do
      begin
        if MoveFileEx(PChar(TempName),
                      PChar(AFileName),
                      MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
          Exit(True);

        ErrorCode := GetLastError;
        Sleep(5);
      end;

    SetLastError(Format('Cannot replace %s (Win32 error %d).',
                        [ExtractFileName(AFileName), ErrorCode]));
  except
    on E: Exception do
      SetLastError(Format('Cannot write %s: %s',
                          [ExtractFileName(AFileName), E.Message]));
  end;

  DeleteFile(TempName);
end;


function TFxServePublisher.WriteUtf8(const AFileName: string;
                                     const AText: string): Boolean;
var
  Utf8: UTF8String;
  Bytes: TBytes;

begin

  Utf8 := UTF8String(AText);

  SetLength(Bytes,
            Length(Utf8));

  if (Length(Utf8) > 0) then
    Move(Utf8[1], Bytes[0], Length(Utf8));
  Result := WriteBytes(AFileName,
                       Bytes);
end;


function TFxServePublisher.CopyAsset(const ASourceFileName: string;
                                     const ADestinationName: string): Boolean;
var
  Stream: TFileStream;
  Bytes: TBytes;

begin

  Result := False;

  if not System.SysUtils.FileExists(ASourceFileName) then
    begin
      SetLastError('PWA asset not found: ' + ASourceFileName);
      Exit;
    end;

  try
    Stream := TFileStream.Create(ASourceFileName,
                                 fmOpenRead or fmShareDenyNone);

    try
      Bytes := BytesFromStream(Stream);
    finally
      Stream.Free;
    end;

    Result := WriteBytes(FileName(ADestinationName),
                         Bytes);
  except
    on E: Exception do
      SetLastError('Cannot publish ' + ADestinationName + ': ' + E.Message);
  end;
end;


function TFxServePublisher.CopyPwaAssets(const APlayerSource: string): Boolean;
var
  SourceFolder: string;

begin

  SourceFolder := IncludeTrailingPathDelimiter(
                    ExtractFilePath(APlayerSource));

  // Deployed/selected WebCam folders keep the PWA assets beside the HTML
  // file. Retain support for the older layout where they lived in a child
  // www directory.
  if not System.SysUtils.FileExists(SourceFolder +
                                    'webcam-manifest.json') then
    SourceFolder := SourceFolder + 'www\';

  Result := CopyAsset(APlayerSource,
                      'webcam_stream.html') and
            CopyAsset(SourceFolder + 'webcam-manifest.json',
                      'webcam-manifest.json') and
            CopyAsset(SourceFolder + 'sw.js', 'sw.js') and
            CopyAsset(SourceFolder + 'webcam-icon-192.png',
                      'webcam-icon-192.png') and
            CopyAsset(SourceFolder + 'webcam-icon-512.png',
                      'webcam-icon-512.png');
end;


procedure TFxServePublisher.CleanupPublishedFiles();
var
  SearchRec: TSearchRec;

begin

  DeleteFile(FileName('live.json'));
  DeleteFile(FileName('init.mp4'));

  if (FindFirst(FileName(PUBLISH_FRAGMENT_PREFIX + '*' + PUBLISH_FRAGMENT_EXTENSION),
                faAnyFile and not faDirectory,
                SearchRec) = 0) then
    try
      repeat
        DeleteFile(FileName(SearchRec.Name));
      until (FindNext(SearchRec) <> 0);

    finally
      FindClose(SearchRec);
    end;
end;


procedure TFxServePublisher.CleanupOldFragments();
var
  KeepFrom: UInt64;
  Sequence: UInt64;

begin

  if (FLastPublishedSequence <= PUBLISH_KEEP_FRAGMENTS) then
    KeepFrom := 1
  else
    KeepFrom := FLastPublishedSequence - PUBLISH_KEEP_FRAGMENTS + 1;

  // Never announce sequence numbers that were not actually published. This
  // also covers a slow first service pass where the in-memory capture window
  // may already start above sequence one.
  if (FFirstPublishedSequence > 0) and (KeepFrom < FFirstPublishedSequence) then
    KeepFrom := FFirstPublishedSequence;

  if (FFirstPublishedSequence = 0) then
    FFirstPublishedSequence := KeepFrom;

  Sequence := FFirstPublishedSequence;
  while (Sequence < KeepFrom) do
    begin
      DeleteFile(FileName(Format(PUBLISH_FRAGMENT_PREFIX + '%.6d' + PUBLISH_FRAGMENT_EXTENSION,
                                 [Sequence])));
      Inc(Sequence);
    end;

  FFirstPublishedSequence := KeepFrom;
end;


function TFxServePublisher.WriteManifest(const ALive: Boolean): Boolean;
var
  Json: string;
  LiveText: string;

begin

  if ALive then
    LiveText := 'true'
  else
    LiveText := 'false';

  Inc(FManifestPublishSequence);
  Json := '{' + sLineBreak +
          '  "version": 1,' + sLineBreak +
          '  "application": "MfWebCamStreamer",' + sLineBreak +
          '  "live": ' + LiveText + ',' + sLineBreak +
          '  "init": "init.mp4",' + sLineBreak +
          Format('  "first": %d,', [FFirstPublishedSequence]) + sLineBreak +
          Format('  "last": %d,', [FLastPublishedSequence]) + sLineBreak +
          Format('  "publishSeq": %d,', [FManifestPublishSequence]) + sLineBreak +
          Format('  "sessionId": "%s",', [FSessionId]) + sLineBreak +
          Format('  "sourceSeq": %d,', [FLastSourceSequence]) + sLineBreak +
          '  "pendingGroupParts": 0,' + sLineBreak +
          '  "prefix": "' + PUBLISH_FRAGMENT_PREFIX + '",' + sLineBreak +
          '  "ext": "' + PUBLISH_FRAGMENT_EXTENSION + '",' + sLineBreak +
          '  "digits": 6,' + sLineBreak +
          '  "codec": "video/mp4; codecs=\"avc1.42C01F, mp4a.40.2\"",' + sLineBreak +
          Format('  "pollMs": %d,', [PUBLISH_MANIFEST_POLL_MS]) + sLineBreak +
          Format('  "fragmentTargetMs": %d,', [PUBLISH_FRAGMENT_TARGET_MS]) + sLineBreak +
          '  "groupSourceFragments": 1,' + sLineBreak +
          Format('  "keepBehind": %d', [PUBLISH_KEEP_FRAGMENTS]) + sLineBreak +
          '}';

  Result := WriteUtf8(FileName('live.json'),
                      Json);
end;


function TFxServePublisher.Start(const AFolder: string;
                                  const APlayerSource: string): Boolean;
var
  SessionGuid: TGUID;

begin

  Result := False;
  Stop;

  FFolder := ExcludeTrailingPathDelimiter(Trim(AFolder));
  if (FFolder = '') then
    begin
      SetLastError('The FxServe publication folder is empty.');
      Exit;
    end;

  if not ForceDirectories(FFolder) then
    begin
      SetLastError('Cannot create the FxServe publication folder: ' + FFolder);
      Exit;
    end;

  CreateGUID(SessionGuid);
  FSessionId := GUIDToString(SessionGuid);

  FSessionId := StringReplace(FSessionId,
                              '{',
                              '',
                              [rfReplaceAll]);

  FSessionId := StringReplace(FSessionId,
                              '}',
                              '',
                              [rfReplaceAll]);

  FLastSourceSequence := 0;
  FFirstPublishedSequence := 0;
  FLastPublishedSequence := 0;
  FManifestPublishSequence := 0;
  FInitWritten := False;
  FPlayerWritten := False;
  FLastError := '';

  CleanupPublishedFiles;
  FPlayerWritten := CopyPwaAssets(APlayerSource);
  if not FPlayerWritten then
    Exit;

  FActive := True;
  Result := True;
end;


procedure TFxServePublisher.Stop();
begin

  if FActive and FInitWritten then
    WriteManifest(False);

  FActive := False;
end;


function TFxServePublisher.Service(const ACapture: TSimpleAvCapture;
                                   out APublishedCount: Integer): Boolean;
var
  InitSegment: TBytes;
  Fragment: TBytes;
  FirstSource: UInt64;
  LastSource: UInt64;
  SourceSequence: UInt64;
  WindowCount: Integer;
  FragmentName: string;

begin

  Result := False;
  APublishedCount := 0;
  if not FActive or not Assigned(ACapture) then
    Exit;

  if not FInitWritten then
    begin
      if not ACapture.GetInitSegment(InitSegment) then
        Exit(True);

      if not WriteBytes(FileName('init.mp4'),
                        InitSegment) then
        Exit;

      FInitWritten := True;
    end;

  if not ACapture.GetFragmentWindow(FirstSource,
                                    LastSource,
                                    WindowCount) or
     (WindowCount = 0) then
    Exit(True);

  if (FLastSourceSequence = 0) then
    SourceSequence := FirstSource
  else
    SourceSequence := FLastSourceSequence + 1;

  if (SourceSequence < FirstSource) then
    begin
      SetLastError(Format('Publisher fell behind; skipped source fragments %d-%d.',
                          [SourceSequence, FirstSource - 1]));
      SourceSequence := FirstSource;
    end;

  while (SourceSequence <= LastSource) do
    begin
      if not ACapture.GetFragment(SourceSequence,
                                  Fragment) then
        Break;

      FragmentName := Format(PUBLISH_FRAGMENT_PREFIX + '%.6d' + PUBLISH_FRAGMENT_EXTENSION,
                             [SourceSequence]);

      if not WriteBytes(FileName(FragmentName),
                        Fragment) then
        Exit;

      FLastSourceSequence := SourceSequence;
      FLastPublishedSequence := SourceSequence;

      if (FFirstPublishedSequence = 0) then
        FFirstPublishedSequence := SourceSequence;

      Inc(APublishedCount);
      Inc(SourceSequence);
    end;

  if (APublishedCount > 0) then
    begin
      CleanupOldFragments();
      Result := WriteManifest(True);
    end
  else
    Result := True;
end;

end.
