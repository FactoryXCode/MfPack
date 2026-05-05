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
// Revision Version: 3.2.0
// Description: Holds all tracks, search/filter, return track by ID and import tracks.
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
unit RDJ.TrackLibrary;

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
  RDJ.PlaylistDb;

type

  TRDJTrackLibrary = class
  private

    FDb: TRDJPlaylistDb;
    function QueryTrack(const Q: TFDQuery): TRDJTrack;
    function QueryTrackSummary(const Q: TFDQuery): TRDJTrack;

  public

    constructor Create(ADb: TRDJPlaylistDb);

    function AddOrUpdateTrack(const ATrack: TRDJTrack): Integer;
    function FindTrackByID(const ATrackID: Integer;
                           out ATrack: TRDJTrack): Boolean;
    function FindTrackByPath(const AFullPath: string;
                             out ATrack: TRDJTrack): Boolean;

    function SearchTracks(const ASearchText: string): TList<TRDJTrack>;
    function SearchTrackSummaries(const ASearchText: string): TList<TRDJTrack>;
    function GetAllTracks: TList<TRDJTrack>;

    procedure MarkMissingTracks();
    procedure ClearMissingFlags();
    procedure ClearLibrary(const AClearFolders: Boolean = False);
    procedure RemoveMissingTracks();
    procedure DeleteTrackByID(const ATrackID: Integer);

    property Db: TRDJPlaylistDb read FDb;
  end;


implementation


constructor TRDJTrackLibrary.Create(ADb: TRDJPlaylistDb);
begin

  inherited Create;

  FDb := ADb;
end;

// Becarefull with this, when you have a huge music library, this can lead to memory issues.
function TRDJTrackLibrary.QueryTrack(const Q: TFDQuery): TRDJTrack;
begin

  Result := RDJEmptyTrack;

  Result.TrackID := Q.FieldByName('TrackID').AsInteger;

  Result.FullPath := Q.FieldByName('FullPath').AsString;
  Result.FileName := Q.FieldByName('FileName').AsString;
  Result.FileExt := Q.FieldByName('FileExt').AsString;

  Result.Title := Q.FieldByName('Title').AsString;
  Result.Artist := Q.FieldByName('Artist').AsString;
  Result.Album := Q.FieldByName('Album').AsString;
  Result.AlbumArtist := Q.FieldByName('AlbumArtist').AsString;
  Result.Genre := Q.FieldByName('Genre').AsString;
  Result.Comment := Q.FieldByName('Comment').AsString;
  Result.Composer := Q.FieldByName('Composer').AsString;

  Result.Year := Q.FieldByName('Year').AsInteger;
  Result.TrackNumber := Q.FieldByName('TrackNumber').AsInteger;
  Result.DiscNumber := Q.FieldByName('DiscNumber').AsInteger;

  Result.DurationMs := Q.FieldByName('DurationMs').AsLargeInt;
  Result.BitRate := Q.FieldByName('BitRate').AsInteger;
  Result.BitrateKbps := Q.FieldByName('BitrateKbps').AsInteger;
  Result.SampleRate := Q.FieldByName('SampleRate').AsInteger;
  Result.Channels := Q.FieldByName('Channels').AsInteger;
  Result.BitsPerSample := Q.FieldByName('BitsPerSample').AsInteger;
  Result.Codec := Q.FieldByName('Codec').AsString;
  Result.QualityLabel := Q.FieldByName('QualityLabel').AsString;

  Result.BPM := Q.FieldByName('BPM').AsFloat;
  Result.MusicalKey := Q.FieldByName('MusicalKey').AsString;
  Result.GainDb := Q.FieldByName('GainDb').AsFloat;

  Result.FileSize := Q.FieldByName('FileSize').AsLargeInt;

  if (Trim(Q.FieldByName('FileModifiedUtc').AsString) <> '') then
    Result.FileModifiedUtc := StrToDateTimeDef(Q.FieldByName('FileModifiedUtc').AsString, 0);

  if (Trim(Q.FieldByName('DateAddedUtc').AsString) <> '') then
    Result.DateAddedUtc := StrToDateTimeDef(Q.FieldByName('DateAddedUtc').AsString, 0);

  if (Trim(Q.FieldByName('LastScanUtc').AsString) <> '') then
    Result.LastScanUtc := StrToDateTimeDef(Q.FieldByName('LastScanUtc').AsString, 0);

  Result.Rating := Q.FieldByName('Rating').AsInteger;
  Result.ColorTag := Q.FieldByName('ColorTag').AsInteger;

  Result.CueInMs := Q.FieldByName('CueInMs').AsLargeInt;
  Result.CueOutMs := Q.FieldByName('CueOutMs').AsLargeInt;

  Result.IsMissing := (Q.FieldByName('IsMissing').AsInteger <> 0);
end;


function TRDJTrackLibrary.QueryTrackSummary(const Q: TFDQuery): TRDJTrack;
begin

  Result := RDJEmptyTrack();

  Result.TrackID := Q.FieldByName('TrackID').AsInteger;

  Result.FullPath := Q.FieldByName('FullPath').AsString;

  Result.Title := Q.FieldByName('Title').AsString;
  Result.Artist := Q.FieldByName('Artist').AsString;
  Result.Album := Q.FieldByName('Album').AsString;
  Result.AlbumArtist := Q.FieldByName('AlbumArtist').AsString;
  Result.Year := Q.FieldByName('Year').AsInteger;
  Result.Genre := Q.FieldByName('Genre').AsString;

  Result.DurationMs := Q.FieldByName('DurationMs').AsLargeInt;
  Result.QualityLabel := Q.FieldByName('QualityLabel').AsString;
  Result.Codec := Q.FieldByName('Codec').AsString;
  Result.BitrateKbps := Q.FieldByName('BitrateKbps').AsInteger;
  Result.SampleRate := Q.FieldByName('SampleRate').AsInteger;
  Result.Channels := Q.FieldByName('Channels').AsInteger;
  Result.BitsPerSample := Q.FieldByName('BitsPerSample').AsInteger;
end;


function TRDJTrackLibrary.AddOrUpdateTrack(const ATrack: TRDJTrack): Integer;
begin

  Result := FDb.AddOrUpdateTrack(ATrack);
end;


function TRDJTrackLibrary.FindTrackByID(const ATrackID: Integer;
                                        out ATrack: TRDJTrack): Boolean;
var
  Q: TFDQuery;

begin

  Result := False;
  ATrack := RDJEmptyTrack;

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FDb.Connection;
    Q.SQL.Text :=
      'SELECT * ' +
      'FROM Tracks ' +
      'WHERE TrackID = :TrackID';

    Q.ParamByName('TrackID').AsInteger := ATrackID;
    Q.Open;

    if not Q.Eof then
      begin
        ATrack := QueryTrack(Q);
        Result := True;
      end;
  finally

    Q.Free;
  end;
end;


function TRDJTrackLibrary.FindTrackByPath(const AFullPath: string;
                                          out ATrack: TRDJTrack): Boolean;
var
  Q: TFDQuery;

begin

  Result := False;
  ATrack := RDJEmptyTrack;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FDb.Connection;
    Q.SQL.Text :=
      'SELECT * ' +
      'FROM Tracks ' +
      'WHERE FullPath = :FullPath';

    Q.ParamByName('FullPath').AsString := AFullPath;
    Q.Open;

    if not Q.Eof then
      begin
        ATrack := QueryTrack(Q);
        Result := True;
      end;
  finally

    Q.Free;
  end;
end;


function TRDJTrackLibrary.GetAllTracks(): TList<TRDJTrack>;
var
  Q: TFDQuery;

begin

  Result := TList<TRDJTrack>.Create;

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FDb.Connection;
    Q.SQL.Text :=
      'SELECT * ' +
      'FROM Tracks ' +
      'ORDER BY Artist, Album, TrackNumber, Title';

    Q.Open;

    while not Q.Eof do
      begin

        Result.Add(QueryTrack(Q));
        Q.Next;
      end;
  finally

    Q.Free;
  end;
end;


function TRDJTrackLibrary.SearchTrackSummaries(const ASearchText: string): TList<TRDJTrack>;
var
  Q: TFDQuery;
  S: string;

begin

  Result := TList<TRDJTrack>.Create;

  if (Trim(ASearchText) = '') then
    Exit;

  Q := TFDQuery.Create(nil);
  try
    try

     Q.Connection := FDb.Connection;

      Q.SQL.Text :=
        'SELECT ' +
        '  TrackID, FullPath, Title, Artist, Album, AlbumArtist, Year, Genre, DurationMs, QualityLabel, Codec, BitrateKbps, SampleRate, Channels, BitsPerSample ' +
        'FROM Tracks ' +
        'WHERE ' +
        '  Title LIKE :S OR ' +
        '  Artist LIKE :S OR ' +
        '  Album LIKE :S OR ' +
        '  AlbumArtist LIKE :S OR ' +
        '  Genre LIKE :S OR ' +
        '  FileName LIKE :S OR ' +
        '  FullPath LIKE :S OR ' +
        '  QualityLabel LIKE :S OR ' +
        '  Codec LIKE :S OR ' +
        '  CAST(Year AS TEXT) LIKE :S ' +
        'ORDER BY Artist, Album, TrackNumber, Title';

      S := '%' + Trim(ASearchText) + '%';
      Q.ParamByName('S').AsString := S;

      Q.Open;

      while not Q.Eof do
        begin

          Result.Add(QueryTrackSummary(Q));
          Q.Next;
        end;
    except

      Result.Free;
      raise;
    end;
  finally

    Q.Free;
  end;
end;


function TRDJTrackLibrary.SearchTracks(const ASearchText: string): TList<TRDJTrack>;
var
  Q: TFDQuery;
  S: string;

begin

  Result := TList<TRDJTrack>.Create;

  Q := TFDQuery.Create(nil);
  try
    try
      Q.Connection := FDb.Connection;

      if (Trim(ASearchText) = '') then
        Q.SQL.Text :=
          'SELECT ' +
          '  TrackID, FullPath, FileName, FileExt, ' +
          '  Title, Artist, Album, AlbumArtist, Genre, ' +
          '  Year, TrackNumber, DiscNumber, ' +
          '  DurationMs, BitRate, BitrateKbps, SampleRate, Channels, BitsPerSample, Codec, QualityLabel, ' +
          '  BPM, MusicalKey, GainDb, ' +
          '  FileSize, FileModifiedUtc, DateAddedUtc, LastScanUtc, ' +
          '  Rating, ColorTag, CueInMs, CueOutMs, IsMissing ' +
          'FROM Tracks ' +
          'ORDER BY Artist, Album, TrackNumber, Title'
      else
        Q.SQL.Text :=
          'SELECT ' +
          '  TrackID, FullPath, FileName, FileExt, ' +
          '  Title, Artist, Album, AlbumArtist, Genre, ' +
          '  Year, TrackNumber, DiscNumber, ' +
          '  DurationMs, BitRate, BitrateKbps, SampleRate, Channels, BitsPerSample, Codec, QualityLabel, ' +
          '  BPM, MusicalKey, GainDb, ' +
          '  FileSize, FileModifiedUtc, DateAddedUtc, LastScanUtc, ' +
          '  Rating, ColorTag, CueInMs, CueOutMs, IsMissing ' +
          'FROM Tracks ' +
          'WHERE ' +
          '  Title LIKE :S OR ' +
          '  Artist LIKE :S OR ' +
          '  Album LIKE :S OR ' +
          '  AlbumArtist LIKE :S OR ' +
          '  Genre LIKE :S OR ' +
          '  FileName LIKE :S OR ' +
          '  FullPath LIKE :S OR ' +
          '  QualityLabel LIKE :S OR ' +
          '  Codec LIKE :S ' +
          'ORDER BY Artist, Album, TrackNumber, Title';

      if (Trim(ASearchText) <> '') then
        begin

          S := '%' + Trim(ASearchText) + '%';
          Q.ParamByName('S').AsString := S;
        end;

      Q.Open;

      while not Q.Eof do
        begin

          //Result.Add(QueryTrack(Q));
          Result.Add(QueryTrackSummary(Q));
          Q.Next;
        end;
      except

        Result.Free;
        raise;
    end;
  finally
    Q.Free;
  end;
end;


procedure TRDJTrackLibrary.MarkMissingTracks();
var
  QSel: TFDQuery;
  QUpd: TFDQuery;
  Missing: Integer;

begin

  QSel := TFDQuery.Create(nil);
  QUpd := TFDQuery.Create(nil);

  try

    QSel.Connection := FDb.Connection;
    QUpd.Connection := FDb.Connection;

    QSel.SQL.Text := 'SELECT TrackID, FullPath ' +
                     'FROM Tracks';

    QUpd.SQL.Text := 'UPDATE Tracks ' +
                     'SET IsMissing = :IsMissing, LastScanUtc = :LastScanUtc ' +
                     'WHERE TrackID = :TrackID';

    QSel.Open;

    while not QSel.Eof do
      begin

        if FileExists(QSel.FieldByName('FullPath').AsString) then
          Missing := 0
        else
          Missing := 1;

        QUpd.ParamByName('IsMissing').AsInteger := Missing;
        QUpd.ParamByName('LastScanUtc').AsDateTime := Now;
        QUpd.ParamByName('TrackID').AsInteger := QSel.FieldByName('TrackID').AsInteger;
        QUpd.ExecSQL;

        QSel.Next;
      end;
  finally

    QUpd.Free;
    QSel.Free;
  end;
end;


procedure TRDJTrackLibrary.ClearMissingFlags();
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FDb.Connection;
    Q.SQL.Text := 'UPDATE Tracks ' +
                  'SET IsMissing = 0';
    Q.ExecSQL;
  finally

    Q.Free;
  end;
end;


procedure TRDJTrackLibrary.ClearLibrary(const AClearFolders: Boolean = False);
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FDb.Connection;

    FDb.BeginTx;
    try

      Q.SQL.Text := 'DELETE FROM PlaylistItems';
      Q.ExecSQL;

      Q.SQL.Text := 'DELETE FROM Tracks';
      Q.ExecSQL;

      if AClearFolders then
        begin

          Q.SQL.Text := 'DELETE FROM LibraryFolders';
          Q.ExecSQL;
        end;

      FDb.CommitTx;
    except

      FDb.RollbackTx;
      raise;
    end;
  finally

    Q.Free;
  end;
end;


procedure TRDJTrackLibrary.RemoveMissingTracks();
var
  QSel: TFDQuery;
  QDelItems: TFDQuery;
  QDelTracks: TFDQuery;
  TrackID: Integer;
  FullPath: string;

begin

  QSel := TFDQuery.Create(nil);
  QDelItems := TFDQuery.Create(nil);
  QDelTracks := TFDQuery.Create(nil);

  try

    QSel.Connection := FDb.Connection;
    QDelItems.Connection := FDb.Connection;
    QDelTracks.Connection := FDb.Connection;

    QSel.SQL.Text := 'SELECT TrackID, FullPath ' +
                     'FROM Tracks';

    QDelItems.SQL.Text := 'DELETE FROM PlaylistItems ' +
                          'WHERE TrackID = :TrackID';

    QDelTracks.SQL.Text := 'DELETE FROM Tracks ' +
                           'WHERE TrackID = :TrackID';

    FDb.BeginTx();

    try

      QSel.Open;

      while not QSel.Eof do
        begin

          TrackID := QSel.FieldByName('TrackID').AsInteger;
          FullPath := QSel.FieldByName('FullPath').AsString;

          if (Trim(FullPath) = '') or (not FileExists(FullPath)) then
            begin
              QDelItems.ParamByName('TrackID').AsInteger := TrackID;
              QDelItems.ExecSQL;

              QDelTracks.ParamByName('TrackID').AsInteger := TrackID;
              QDelTracks.ExecSQL();
            end;

          QSel.Next;
        end;

      FDb.CommitTx();
    except

      FDb.RollbackTx();
      raise;
    end;
  finally

    QDelTracks.Free();
    QDelItems.Free();
    QSel.Free();
  end;
end;


procedure TRDJTrackLibrary.DeleteTrackByID(const ATrackID: Integer);
var
  Q: TFDQuery;

begin

  if (ATrackID <= 0) then
    Exit;

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FDb.Connection;

    FDb.BeginTx;

    try

      Q.SQL.Text := 'DELETE FROM PlaylistItems ' +
                    'WHERE TrackID = :TrackID';
      Q.ParamByName('TrackID').AsInteger := ATrackID;
      Q.ExecSQL;

      Q.SQL.Text := 'DELETE FROM Tracks ' +
                    'WHERE TrackID = :TrackID';
      Q.ParamByName('TrackID').AsInteger := ATrackID;
      Q.ExecSQL;

      FDb.CommitTx();
    except

      FDb.RollbackTx();
      raise;
    end;
  finally

    Q.Free;
  end;
end;

end.
