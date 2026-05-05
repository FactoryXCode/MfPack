{
Responsibilities:

create playlists

load/save playlists

manage playlist list
}
// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.PlaylistManager.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Creates playlists, load/save playlists, manage playlist list.
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
unit RDJ.PlaylistManager;

interface

uses

  {System}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  {FireDAC}
  FireDAC.Stan.Factory,
  FireDAC.DApt,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  {Application}
  RDJ.PlaylistTypes,
  RDJ.PlaylistDb,
  RDJ.TrackLibrary;

type

  TRDJPlaylistManager = class
  private

    FDb: TRDJPlaylistDb;
    FLibrary: TRDJTrackLibrary;

    function QueryPlaylistInfo(const Q: TFDQuery): TRDJPlaylistInfo;
    function QueryPlaylistEntry(const Q: TFDQuery): TRDJPlaylistEntry;

  public

    constructor Create(ADb: TRDJPlaylistDb;
                       ALibrary: TRDJTrackLibrary);

    function CreateNewPlaylist(const AName: string;
                               const ANotes: string = ''): Integer;

    function GetAllPlaylists: TList<TRDJPlaylistInfo>;
    function LoadPlaylist(const APlaylistID: Integer): TRDJPlaylist;
    function SavePlaylist(APlaylist: TRDJPlaylist): Boolean;

    procedure RenamePlaylist(const APlaylistID: Integer;
                             const ANewName: string);

    procedure DeletePlaylist(const APlaylistID: Integer);

    function AddTrackToPlaylist(APlaylist: TRDJPlaylist;
                                const ATrackID: Integer): Integer;

    procedure RemovePlaylistItem(APlaylist: TRDJPlaylist;
                                 const AIndex: Integer);

    procedure MovePlaylistItem(APlaylist: TRDJPlaylist;
                               const AFromIndex,
                               AToIndex: Integer);

    property Db: TRDJPlaylistDb read FDb;
    property LibraryRef: TRDJTrackLibrary read FLibrary;
  end;


implementation


constructor TRDJPlaylistManager.Create(ADb: TRDJPlaylistDb;
                                       ALibrary: TRDJTrackLibrary);
begin

  inherited Create;

  FDb := ADb;
  FLibrary := ALibrary;
end;


function TRDJPlaylistManager.QueryPlaylistInfo(const Q: TFDQuery): TRDJPlaylistInfo;
begin

  Result := RDJEmptyPlaylistInfo;

  Result.PlaylistID := Q.FieldByName('PlaylistID').AsInteger;
  Result.Name := Q.FieldByName('Name').AsString;
  Result.Notes := Q.FieldByName('Notes').AsString;

  if Trim(Q.FieldByName('CreatedUtc').AsString) <> '' then
    Result.CreatedUtc := StrToDateTimeDef(Q.FieldByName('CreatedUtc').AsString, 0);

  if Trim(Q.FieldByName('ModifiedUtc').AsString) <> '' then
    Result.ModifiedUtc := StrToDateTimeDef(Q.FieldByName('ModifiedUtc').AsString, 0);
end;


function TRDJPlaylistManager.QueryPlaylistEntry(const Q: TFDQuery): TRDJPlaylistEntry;
begin

  Result := RDJEmptyPlaylistEntry;

  Result.PlaylistItemID := Q.FieldByName('PlaylistItemID').AsInteger;
  Result.PlaylistID := Q.FieldByName('PlaylistID').AsInteger;
  Result.TrackID := Q.FieldByName('TrackID').AsInteger;
  Result.SortOrder := Q.FieldByName('SortOrder').AsInteger;

  Result.CueInMs := Q.FieldByName('CueInMs').AsLargeInt;
  Result.CueOutMs := Q.FieldByName('CueOutMs').AsLargeInt;
  Result.UserNote := Q.FieldByName('UserNote').AsString;

  Result.Track.TrackID := Q.FieldByName('TrackID').AsInteger;
  Result.Track.FullPath := Q.FieldByName('FullPath').AsString;
  Result.Track.FileName := Q.FieldByName('FileName').AsString;
  Result.Track.FileExt := Q.FieldByName('FileExt').AsString;

  Result.Track.Title := Q.FieldByName('Title').AsString;
  Result.Track.Artist := Q.FieldByName('Artist').AsString;
  Result.Track.Album := Q.FieldByName('Album').AsString;
  Result.Track.AlbumArtist := Q.FieldByName('AlbumArtist').AsString;
  Result.Track.Genre := Q.FieldByName('Genre').AsString;
  Result.Track.Comment := Q.FieldByName('Comment').AsString;
  Result.Track.Composer := Q.FieldByName('Composer').AsString;

  Result.Track.Year := Q.FieldByName('Year').AsInteger;
  Result.Track.TrackNumber := Q.FieldByName('TrackNumber').AsInteger;
  Result.Track.DiscNumber := Q.FieldByName('DiscNumber').AsInteger;

  Result.Track.DurationMs := Q.FieldByName('DurationMs').AsLargeInt;
  Result.Track.BitRate := Q.FieldByName('BitRate').AsInteger;
  Result.Track.SampleRate := Q.FieldByName('SampleRate').AsInteger;
  Result.Track.Channels := Q.FieldByName('Channels').AsInteger;

  Result.Track.BPM := Q.FieldByName('BPM').AsFloat;
  Result.Track.MusicalKey := Q.FieldByName('MusicalKey').AsString;
  Result.Track.GainDb := Q.FieldByName('GainDb').AsFloat;

  Result.Track.FileSize := Q.FieldByName('FileSize').AsLargeInt;

  if Trim(Q.FieldByName('FileModifiedUtc').AsString) <> '' then
    Result.Track.FileModifiedUtc := StrToDateTimeDef(Q.FieldByName('FileModifiedUtc').AsString, 0);

  if Trim(Q.FieldByName('DateAddedUtc').AsString) <> '' then
    Result.Track.DateAddedUtc := StrToDateTimeDef(Q.FieldByName('DateAddedUtc').AsString, 0);

  if Trim(Q.FieldByName('LastScanUtc').AsString) <> '' then
    Result.Track.LastScanUtc := StrToDateTimeDef(Q.FieldByName('LastScanUtc').AsString, 0);

  Result.Track.Rating := Q.FieldByName('Rating').AsInteger;
  Result.Track.ColorTag := Q.FieldByName('ColorTag').AsInteger;

  Result.Track.CueInMs := Q.FieldByName('TrackCueInMs').AsLargeInt;
  Result.Track.CueOutMs := Q.FieldByName('TrackCueOutMs').AsLargeInt;

  Result.Track.IsMissing := (Q.FieldByName('IsMissing').AsInteger <> 0);
end;


function TRDJPlaylistManager.CreateNewPlaylist(const AName: string;
                                               const ANotes: string = ''): Integer;
begin

  Result := FDb.CreatePlaylist(AName,
                               ANotes);
end;


function TRDJPlaylistManager.GetAllPlaylists: TList<TRDJPlaylistInfo>;
var
  Q: TFDQuery;

begin

  Result := TList<TRDJPlaylistInfo>.Create;

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FDb.Connection;
    Q.SQL.Text := 'SELECT PlaylistID, Name, Notes, CreatedUtc, ModifiedUtc ' +
                  'FROM Playlists ' +
                  'ORDER BY Name';

    Q.Open;

    while not Q.Eof do
      begin

        Result.Add(QueryPlaylistInfo(Q));
        Q.Next;
      end;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistManager.LoadPlaylist(const APlaylistID: Integer): TRDJPlaylist;
var
  Q: TFDQuery;
  Info: TRDJPlaylistInfo;

begin

  Result := TRDJPlaylist.Create;

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FDb.Connection;

    Q.SQL.Text := 'SELECT PlaylistID, Name, Notes, CreatedUtc, ModifiedUtc ' +
                  'FROM Playlists ' +
                  'WHERE PlaylistID = :PlaylistID';

    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.Open;

    if not Q.Eof then
      begin

        Info := QueryPlaylistInfo(Q);
        Result.Info := Info;
      end;

    Q.Close;
    Q.SQL.Text := 'SELECT ' +
                  '  pi.PlaylistItemID, pi.PlaylistID, pi.TrackID, pi.SortOrder, ' +
                  '  pi.CueInMs, pi.CueOutMs, pi.UserNote, ' +
                  '  t.TrackID, t.FullPath, t.FileName, t.FileExt, ' +
                  '  t.Title, t.Artist, t.Album, t.AlbumArtist, t.Genre, t.Comment, t.Composer, ' +
                  '  t.Year, t.TrackNumber, t.DiscNumber, t.DurationMs, t.BitRate, t.SampleRate, t.Channels, ' +
                  '  t.BPM, t.MusicalKey, t.GainDb, t.FileSize, t.FileModifiedUtc, t.DateAddedUtc, t.LastScanUtc, ' +
                  '  t.Rating, t.ColorTag, t.CueInMs AS TrackCueInMs, t.CueOutMs AS TrackCueOutMs, t.IsMissing ' +
                  'FROM PlaylistItems pi ' +
                  'INNER JOIN Tracks t ON t.TrackID = pi.TrackID ' +
                  'WHERE pi.PlaylistID = :PlaylistID ' +
                  'ORDER BY pi.SortOrder';

    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.Open;

    while not Q.Eof do
      begin
        Result.Add(QueryPlaylistEntry(Q));
        Q.Next;
      end;

    Result.Modified := False;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistManager.SavePlaylist(APlaylist: TRDJPlaylist): Boolean;
var
  i: Integer;
  E: TRDJPlaylistEntry;

begin

  Result := False;

  if (APlaylist = nil) then
    Exit;

  if (APlaylist.Info.PlaylistID = 0) then
    Exit;

  FDb.BeginTx;

  try

    FDb.RenamePlaylist(APlaylist.Info.PlaylistID,
                       APlaylist.Info.Name);

    FDb.ClearPlaylistItems(APlaylist.Info.PlaylistID);

    APlaylist.RebuildSortOrder;

    for i := 0 to APlaylist.Count - 1 do
      begin

        E := APlaylist[i];

        E.PlaylistID := APlaylist.Info.PlaylistID;
        E.PlaylistItemID := FDb.AddPlaylistItem(APlaylist.Info.PlaylistID,
                                                E.TrackID,
                                                E.SortOrder,
                                                E.CueInMs,
                                                E.CueOutMs,
                                                E.UserNote);
        APlaylist[i] := E;
      end;

    FDb.CommitTx;
    APlaylist.Modified := False;
    Result := True;
  except

    FDb.RollbackTx;
    raise;
  end;
end;


procedure TRDJPlaylistManager.RenamePlaylist(const APlaylistID: Integer;
                                             const ANewName: string);
begin

  FDb.RenamePlaylist(APlaylistID,
                     ANewName);
end;


procedure TRDJPlaylistManager.DeletePlaylist(const APlaylistID: Integer);
begin

  FDb.DeletePlaylist(APlaylistID);
end;


function TRDJPlaylistManager.AddTrackToPlaylist(APlaylist: TRDJPlaylist;
                                                const ATrackID: Integer): Integer;
var
  E: TRDJPlaylistEntry;
  T: TRDJTrack;

begin

  Result := -1;

  if (APlaylist = nil) then
    Exit;

  if not FLibrary.FindTrackByID(ATrackID,
                                T) then
    Exit;

  E := RDJEmptyPlaylistEntry;
  E.PlaylistItemID := 0;
  E.PlaylistID := APlaylist.Info.PlaylistID;
  E.TrackID := ATrackID;
  E.SortOrder := APlaylist.Count + 1;
  E.CueInMs := 0;
  E.CueOutMs := 0;
  E.UserNote := '';
  E.Track := T;

  APlaylist.Add(E);
  APlaylist.RebuildSortOrder;

  Result := APlaylist.Count - 1;
end;


procedure TRDJPlaylistManager.RemovePlaylistItem(APlaylist: TRDJPlaylist;
                                                 const AIndex: Integer);
begin

  if (APlaylist = nil) then
    Exit;

  APlaylist.Delete(AIndex);
end;


procedure TRDJPlaylistManager.MovePlaylistItem(APlaylist: TRDJPlaylist;
                                               const AFromIndex,
                                               AToIndex: Integer);
begin

  if (APlaylist = nil) then
    Exit;

  APlaylist.MoveItem(AFromIndex,
                     AToIndex);
end;

end.
