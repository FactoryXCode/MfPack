// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.TrackLibrary.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Scans folders, detect supported audio files, read tags, inserts/updates tracks,
//              Supported formats: MP3, later: WAV, FLAC, AAC / M4A, OGG.
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
unit RDJ.LibraryScanner;

interface

uses

  {System}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  {Application}
  RDJ.PlaylistTypes,
  RDJ.TrackLibrary,
  RDJ.TagReader,
  RDJ.TrackQuality;

type

  TRDJScanProgressEvent = procedure(Sender: TObject;
                                    const AFileName: string;
                                    ACurrentIndex: Integer;
                                    ATotalCount: Integer) of object;

  TRDJScanCancelEvent = function(Sender: TObject): Boolean of object;


  TRDJLibraryScanner = class
  private

    FLibrary: TRDJTrackLibrary;
    FOnProgress: TRDJScanProgressEvent;
    FOnCancelCheck: TRDJScanCancelEvent;

    function IsSupportedAudioFile(const AFileName: string): Boolean;
    procedure CollectFiles(const AFolder: string;
                           const ARecursive: Boolean;
                           AFiles: TStrings;
                           var ACancelled: Boolean);
    function CheckCancelled: Boolean;

  public

    constructor Create(ALibrary: TRDJTrackLibrary);

    function ScanFolder(const AFolder: string;
                        const ARecursive: Boolean = True): Integer;

    function ScanFolders(AFolders: TStrings;
                         const ARecursive: Boolean = True): Integer;

    function CollectAudioFiles(const AFolder: string;
                               const ARecursive: Boolean = True): TStringList;

    property LibraryRef: TRDJTrackLibrary read FLibrary;
    property OnProgress: TRDJScanProgressEvent read FOnProgress write FOnProgress;
    property OnCancelCheck: TRDJScanCancelEvent read FOnCancelCheck write FOnCancelCheck;
  end;


implementation


constructor TRDJLibraryScanner.Create(ALibrary: TRDJTrackLibrary);
begin
  inherited Create;
  FLibrary := ALibrary;
end;


function TRDJLibraryScanner.CheckCancelled: Boolean;
begin

  Result := Assigned(FOnCancelCheck) and FOnCancelCheck(Self);
end;


function TRDJLibraryScanner.IsSupportedAudioFile(const AFileName: string): Boolean;
var
  Ext: string;

begin
  Ext := LowerCase(ExtractFileExt(AFileName));

  Result :=
    (Ext = '.mp3') or
    (Ext = '.wav') or
    (Ext = '.flac') or
    (Ext = '.m4a') or
    (Ext = '.aac') or
    (Ext = '.ogg') or
    (Ext = '.wma') or
    (Ext = '.aif') or
    (Ext = '.aiff');
end;


procedure TRDJLibraryScanner.CollectFiles(const AFolder: string;
                                          const ARecursive: Boolean;
                                          AFiles: TStrings;
                                          var ACancelled: Boolean);
var
  SR: TSearchRec;
  Path: string;

begin

  if ACancelled then
    Exit;

  if (AFiles = nil) or (Trim(AFolder) = '') then
    Exit;

  Path := IncludeTrailingPathDelimiter(AFolder);

  if FindFirst(Path + '*.*',
               faAnyFile,
               SR) = 0 then
    begin
      try
        repeat
          if CheckCancelled then
            begin

              ACancelled := True;
              Exit;
            end;

          if (SR.Name = '.') or (SR.Name = '..') then
            Continue;

          if (SR.Attr and faDirectory) <> 0 then
            begin

              if ARecursive then
                CollectFiles(Path + SR.Name,
                             True,
                             AFiles,
                             ACancelled);
            end
          else
            begin

              if IsSupportedAudioFile(SR.Name) then
                AFiles.Add(Path + SR.Name);
            end;

          if ACancelled then
            Exit;

        until (FindNext(SR) <> 0);
      finally

        FindClose(SR);
      end;
    end;
end;


function TRDJLibraryScanner.ScanFolder(const AFolder: string;
                                       const ARecursive: Boolean = True): Integer;
var
  Files: TStringList;
  i: Integer;
  Track: TRDJTrack;
  Cancelled: Boolean;

begin

  Result := 0;

  if (Trim(AFolder) = '') then
    Exit;

  if not DirectoryExists(AFolder) then
    Exit;

  Cancelled := False;

  Files := TStringList.Create;
  try

    Files.Sorted := False;
    Files.Duplicates := dupIgnore;

    CollectFiles(AFolder,
                 ARecursive,
                 Files,
                 Cancelled);

    if Cancelled then
      Exit;

    for i := 0 to Files.Count - 1 do
      begin

        if CheckCancelled then
          Break;

        if Assigned(FOnProgress) then
          FOnProgress(Self,
                      Files[i],
                      i + 1,
                      Files.Count);

        if RDJReadTrackInfo(Files[i],
                            Track) then
          begin

            RDJUpdateTrackQuality(Track);
            FLibrary.AddOrUpdateTrack(Track);
            Inc(Result);
          end;
      end;
  finally

    Files.Free;
  end;
end;


function TRDJLibraryScanner.ScanFolders(AFolders: TStrings;
                                        const ARecursive: Boolean = True): Integer;
var
  i: Integer;

begin

  Result := 0;

  if (AFolders = nil) then
    Exit;

  for i := 0 to AFolders.Count - 1 do
    begin

      if CheckCancelled then
        Break;

      Inc(Result,
          ScanFolder(AFolders[i],
                     ARecursive));
    end;
end;


function TRDJLibraryScanner.CollectAudioFiles(const AFolder: string;
                                              const ARecursive: Boolean = True): TStringList;
var
  Cancelled: Boolean;

begin

  Result := TStringList.Create;
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;

  Cancelled := False;

  if (Trim(AFolder) = '') then
    Exit;

  if not DirectoryExists(AFolder) then
    Exit;

  CollectFiles(AFolder,
               ARecursive,
               Result,
               Cancelled);

  if Cancelled then
    begin

      Result.Free;
      Result := nil;
    end;
end;

end.
