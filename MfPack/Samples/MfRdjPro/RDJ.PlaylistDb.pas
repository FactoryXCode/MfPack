// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.PlaylistDb.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Opens SQLite database, create schema, load/save tracks,
//              load/save playlists and runs queries
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
unit RDJ.PlaylistDb;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  {FireDAC}
  FireDAC.Stan.Factory,
  FireDAC.DApt,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.Stan.ExprFuncs,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.Phys,
  FireDAC.Phys.SQLiteCli,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  {Application}
  RDJ.PlaylistTypes;

type

  TRDJPlaylistDb = class
  private

    FConnection: TFDConnection;
    FTransaction: TFDTransaction;
    FDriverLink: TFDPhysSQLiteDriverLink;
    FDbFileName: string;
    FConnected: Boolean;

    procedure SetupConnection;
    procedure ExecSQL(const ASQL: string);
    function QueryInt(const ASQL: string): Integer;
    function ColumnExists(const ATableName: string;
                          const AColumnName: string): Boolean;
    procedure AddColumnIfMissing(const ATableName: string;
                                 const AColumnName: string;
                                 const AColumnSql: string);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Open(const ADbFileName: string);
    procedure Close();
    procedure CreateSchema();

    function IsOpen(): Boolean;

    function AddOrUpdateTrack(const ATrack: TRDJTrack): Integer;
    function FindTrackByPath(const AFullPath: string): Integer;

    function CreatePlaylist(const AName: string;
                            const ANotes: string = ''): Integer;

    procedure RenamePlaylist(const APlaylistID: Integer;
                             const ANewName: string);

    procedure DeletePlaylist(const APlaylistID: Integer);

    procedure ClearPlaylistItems(const APlaylistID: Integer);

    function AddPlaylistItem(const APlaylistID: Integer;
                             const ATrackID: Integer;
                             const ASortOrder: Integer;
                             const ACueInMs: Int64 = 0;
                             const ACueOutMs: Int64 = 0;
                             const AUserNote: string = ''): Integer;

    procedure BeginTx();
    procedure CommitTx();
    procedure RollbackTx();

    property Connection: TFDConnection read FConnection;
    property DbFileName: string read FDbFileName;
  end;

  // Helpers
  function RDJDateTimeToDbStr(const AValue: TDateTime): string;
  function RDJDbStrToDateTime(const S: string): TDateTime;
  procedure RDJShutdownPlaylistDbFireDAC();


implementation


procedure TRDJPlaylistDb.SetupConnection();
begin

  FConnection.LoginPrompt := False;
  FConnection.ResourceOptions.SilentMode := True;
  FConnection.TxOptions.AutoStop := False;
  FConnection.TxOptions.AutoCommit := False;
  FConnection.TxOptions.Isolation := xiReadCommitted;

  FConnection.DriverName := 'SQLite';
  FConnection.Params.Clear;
  FConnection.Params.Add('DriverID=SQLite');
  FConnection.Params.Add('LockingMode=Normal');
  FConnection.Params.Add('Synchronous=Normal');
  FConnection.Params.Add('JournalMode=WAL');
end;


constructor TRDJPlaylistDb.Create();
begin

  inherited Create;

  FDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  FConnection := TFDConnection.Create(nil);
  FTransaction := TFDTransaction.Create(nil);

  FConnection.Transaction := FTransaction;
  FConnection.UpdateTransaction := FTransaction;

  FDbFileName := '';
  FConnected := False;

  SetupConnection;
end;


destructor TRDJPlaylistDb.Destroy();
begin

  Close;

  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  FreeAndNil(FDriverLink);

  inherited Destroy;
end;


procedure TRDJPlaylistDb.Open(const ADbFileName: string);
begin

  if (Trim(ADbFileName) = '') then
    raise Exception.Create('Database filename is empty.');

  if FConnected then
    Close;

  FDbFileName := ADbFileName;

  ForceDirectories(ExtractFilePath(FDbFileName));

  FConnection.Params.Values['Database'] := FDbFileName;
  FConnection.Connected := True;
  FConnected := True;

  CreateSchema();
end;


procedure TRDJPlaylistDb.Close();
begin

  if FTransaction.Active then
    FTransaction.Rollback;

  if FConnection.Connected then
    FConnection.Connected := False;

  FConnected := False;
end;


function TRDJPlaylistDb.IsOpen(): Boolean;
begin

  Result := FConnected and FConnection.Connected;
end;


procedure TRDJPlaylistDb.BeginTx();
begin

  if not FTransaction.Active then
    FTransaction.StartTransaction;
end;


procedure TRDJPlaylistDb.CommitTx();
begin

  if FTransaction.Active then
    FTransaction.Commit;
end;


procedure TRDJPlaylistDb.RollbackTx();
begin

  if FTransaction.Active then
    FTransaction.Rollback;
end;


procedure TRDJPlaylistDb.ExecSQL(const ASQL: string);
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FConnection;
    Q.SQL.Text := ASQL;
    Q.ExecSQL;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistDb.QueryInt(const ASQL: string): Integer;
var
  Q: TFDQuery;

begin

  Result := 0;

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FConnection;
    Q.SQL.Text := ASQL;
    Q.Open;

    if not Q.Eof then
      Result := Q.Fields[0].AsInteger;
  finally

    Q.Free;
  end;
end;



function TRDJPlaylistDb.ColumnExists(const ATableName: string;
                                     const AColumnName: string): Boolean;
var
  Q: TFDQuery;

begin

  Result := False;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'PRAGMA table_info(' + ATableName + ')';
    Q.Open;

    while not Q.Eof do
      begin
        if SameText(Q.FieldByName('name').AsString,
                    AColumnName) then
          begin
            Result := True;
            Exit;
          end;

        Q.Next;
      end;
  finally
    Q.Free;
  end;
end;


procedure TRDJPlaylistDb.AddColumnIfMissing(const ATableName: string;
                                            const AColumnName: string;
                                            const AColumnSql: string);
begin

  if ColumnExists(ATableName,
                  AColumnName) then
    Exit;

  ExecSQL('ALTER TABLE ' + ATableName + ' ADD COLUMN ' + AColumnSql);
end;


procedure TRDJPlaylistDb.CreateSchema();
begin

  ExecSQL('CREATE TABLE IF NOT EXISTS Tracks (' +
          '  TrackID INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  FullPath TEXT NOT NULL UNIQUE,' +
          '  FileName TEXT,' +
          '  FileExt TEXT,' +
          '  Title TEXT,' +
          '  Artist TEXT,' +
          '  Album TEXT,' +
          '  AlbumArtist TEXT,' +
          '  Genre TEXT,' +
          '  Comment TEXT,' +
          '  Composer TEXT,' +
          '  Year INTEGER DEFAULT 0,' +
          '  TrackNumber INTEGER DEFAULT 0,' +
          '  DiscNumber INTEGER DEFAULT 0,' +
          '  DurationMs INTEGER DEFAULT 0,' +
          '  BitRate INTEGER DEFAULT 0,' +
          '  BitrateKbps INTEGER DEFAULT 0,' +
          '  SampleRate INTEGER DEFAULT 0,' +
          '  Channels INTEGER DEFAULT 0,' +
          '  BitsPerSample INTEGER DEFAULT 0,' +
          '  Codec TEXT,' +
          '  QualityLabel TEXT,' +
          '  BPM REAL DEFAULT 0,' +
          '  MusicalKey TEXT,' +
          '  GainDb REAL DEFAULT 0,' +
          '  FileSize INTEGER DEFAULT 0,' +
          '  FileModifiedUtc TEXT,' +
          '  DateAddedUtc TEXT,' +
          '  LastScanUtc TEXT,' +
          '  Rating INTEGER DEFAULT 0,' +
          '  ColorTag INTEGER DEFAULT 0,' +
          '  CueInMs INTEGER DEFAULT 0,' +
          '  CueOutMs INTEGER DEFAULT 0,' +
          '  IsMissing INTEGER DEFAULT 0' +
          ');');

  ExecSQL('CREATE TABLE IF NOT EXISTS LibraryFolders (' +
          '  FolderID INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  FolderPath TEXT NOT NULL UNIQUE,' +
          '  Recursive INTEGER DEFAULT 1,' +
          '  Enabled INTEGER DEFAULT 1,' +
          '  DateAddedUtc TEXT' +
          ');');

  ExecSQL('CREATE TABLE IF NOT EXISTS Playlists (' +
          '  PlaylistID INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  Name TEXT NOT NULL,' +
          '  Notes TEXT,' +
          '  CreatedUtc TEXT,' +
          '  ModifiedUtc TEXT' +
          ');');

  ExecSQL('CREATE TABLE IF NOT EXISTS PlaylistItems (' +
          '  PlaylistItemID INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  PlaylistID INTEGER NOT NULL,' +
          '  TrackID INTEGER NOT NULL,' +
          '  SortOrder INTEGER NOT NULL,' +
          '  CueInMs INTEGER DEFAULT 0,' +
          '  CueOutMs INTEGER DEFAULT 0,' +
          '  UserNote TEXT,' +
          '  FOREIGN KEY (PlaylistID) REFERENCES Playlists(PlaylistID),' +
          '  FOREIGN KEY (TrackID) REFERENCES Tracks(TrackID)' +
          ');');

  AddColumnIfMissing('Tracks',
                     'BitrateKbps',
                     'BitrateKbps INTEGER DEFAULT 0');

  AddColumnIfMissing('Tracks',
                     'BitsPerSample',
                     'BitsPerSample INTEGER DEFAULT 0');

  AddColumnIfMissing('Tracks',
                     'Codec',
                     'Codec TEXT');

  AddColumnIfMissing('Tracks',
                     'QualityLabel',
                     'QualityLabel TEXT');

  ExecSQL('CREATE INDEX IF NOT EXISTS IDX_Tracks_FullPath ON Tracks (FullPath);');
  ExecSQL('CREATE INDEX IF NOT EXISTS IDX_Tracks_Title ON Tracks (Title);');
  ExecSQL('CREATE INDEX IF NOT EXISTS IDX_Tracks_Artist ON Tracks (Artist);');
  ExecSQL('CREATE INDEX IF NOT EXISTS IDX_PlaylistItems_PlaylistID ON PlaylistItems (PlaylistID);');
  ExecSQL('CREATE INDEX IF NOT EXISTS IDX_PlaylistItems_SortOrder ON PlaylistItems (PlaylistID, SortOrder);');
end;


function TRDJPlaylistDb.FindTrackByPath(const AFullPath: string): Integer;
var
  Q: TFDQuery;

begin

  Result := 0;

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FConnection;
    Q.SQL.Text := 'SELECT TrackID FROM Tracks WHERE FullPath = :P';
    Q.ParamByName('P').AsString := AFullPath;
    Q.Open;

    if not Q.Eof then
      Result := Q.FieldByName('TrackID').AsInteger;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistDb.AddOrUpdateTrack(const ATrack: TRDJTrack): Integer;
var
  Q: TFDQuery;
  ExistingID: Integer;

begin

  ExistingID := FindTrackByPath(ATrack.FullPath);

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;

    if ExistingID = 0 then
      begin
        Q.SQL.Text := 'INSERT INTO Tracks (' +
                      '  FullPath, FileName, FileExt, Title, Artist, Album, AlbumArtist, Genre, Comment, Composer,' +
                      '  Year, TrackNumber, DiscNumber, DurationMs, BitRate, BitrateKbps, SampleRate, Channels, BitsPerSample, Codec, QualityLabel, BPM, MusicalKey, GainDb,' +
                      '  FileSize, FileModifiedUtc, DateAddedUtc, LastScanUtc, Rating, ColorTag, CueInMs, CueOutMs, IsMissing' +
                      ') VALUES (' +
                      '  :FullPath, :FileName, :FileExt, :Title, :Artist, :Album, :AlbumArtist, :Genre, :Comment, :Composer,' +
                      '  :Year, :TrackNumber, :DiscNumber, :DurationMs, :BitRate, :BitrateKbps, :SampleRate, :Channels, :BitsPerSample, :Codec, :QualityLabel, :BPM, :MusicalKey, :GainDb,' +
                      '  :FileSize, :FileModifiedUtc, :DateAddedUtc, :LastScanUtc, :Rating, :ColorTag, :CueInMs, :CueOutMs, :IsMissing' +
                      ')';
      end
    else
      begin
        Q.SQL.Text := 'UPDATE Tracks SET ' +
                      '  FileName = :FileName,' +
                      '  FileExt = :FileExt,' +
                      '  Title = :Title,' +
                      '  Artist = :Artist,' +
                      '  Album = :Album,' +
                      '  AlbumArtist = :AlbumArtist,' +
                      '  Genre = :Genre,' +
                      '  Comment = :Comment,' +
                      '  Composer = :Composer,' +
                      '  Year = :Year,' +
                      '  TrackNumber = :TrackNumber,' +
                      '  DiscNumber = :DiscNumber,' +
                      '  DurationMs = :DurationMs,' +
                      '  BitRate = :BitRate,' +
                      '  BitrateKbps = :BitrateKbps,' +
                      '  SampleRate = :SampleRate,' +
                      '  Channels = :Channels,' +
                      '  BitsPerSample = :BitsPerSample,' +
                      '  Codec = :Codec,' +
                      '  QualityLabel = :QualityLabel,' +
                      '  BPM = :BPM,' +
                      '  MusicalKey = :MusicalKey,' +
                      '  GainDb = :GainDb,' +
                      '  FileSize = :FileSize,' +
                      '  FileModifiedUtc = :FileModifiedUtc,' +
                      '  DateAddedUtc = :DateAddedUtc,' +
                      '  LastScanUtc = :LastScanUtc,' +
                      '  Rating = :Rating,' +
                      '  ColorTag = :ColorTag,' +
                      '  CueInMs = :CueInMs,' +
                      '  CueOutMs = :CueOutMs,' +
                      '  IsMissing = :IsMissing ' +
                      'WHERE TrackID = :TrackID';
      end;

    Q.ParamByName('FileName').AsString := ATrack.FileName;
    Q.ParamByName('FileExt').AsString := ATrack.FileExt;
    Q.ParamByName('Title').AsString := ATrack.Title;
    Q.ParamByName('Artist').AsString := ATrack.Artist;
    Q.ParamByName('Album').AsString := ATrack.Album;
    Q.ParamByName('AlbumArtist').AsString := ATrack.AlbumArtist;
    Q.ParamByName('Genre').AsString := ATrack.Genre;
    Q.ParamByName('Comment').AsString := ATrack.Comment;
    Q.ParamByName('Composer').AsString := ATrack.Composer;

    Q.ParamByName('Year').AsInteger := ATrack.Year;
    Q.ParamByName('TrackNumber').AsInteger := ATrack.TrackNumber;
    Q.ParamByName('DiscNumber').AsInteger := ATrack.DiscNumber;

    Q.ParamByName('DurationMs').AsLargeInt := ATrack.DurationMs;
    Q.ParamByName('BitRate').AsInteger := ATrack.BitRate;
    Q.ParamByName('BitrateKbps').AsInteger := ATrack.BitrateKbps;
    Q.ParamByName('SampleRate').AsInteger := ATrack.SampleRate;
    Q.ParamByName('Channels').AsInteger := ATrack.Channels;
    Q.ParamByName('BitsPerSample').AsInteger := ATrack.BitsPerSample;
    Q.ParamByName('Codec').AsString := ATrack.Codec;
    Q.ParamByName('QualityLabel').AsString := ATrack.QualityLabel;

    Q.ParamByName('BPM').AsFloat := ATrack.BPM;
    Q.ParamByName('MusicalKey').AsString := ATrack.MusicalKey;
    Q.ParamByName('GainDb').AsFloat := ATrack.GainDb;

    Q.ParamByName('FileSize').AsLargeInt := ATrack.FileSize;

    Q.ParamByName('FileModifiedUtc').AsString := RDJDateTimeToDbStr(ATrack.FileModifiedUtc);
    Q.ParamByName('DateAddedUtc').AsString := RDJDateTimeToDbStr(ATrack.DateAddedUtc);
    Q.ParamByName('LastScanUtc').AsString := RDJDateTimeToDbStr(ATrack.LastScanUtc);

    Q.ParamByName('Rating').AsInteger := ATrack.Rating;
    Q.ParamByName('ColorTag').AsInteger := ATrack.ColorTag;

    Q.ParamByName('CueInMs').AsLargeInt := ATrack.CueInMs;
    Q.ParamByName('CueOutMs').AsLargeInt := ATrack.CueOutMs;
    Q.ParamByName('IsMissing').AsInteger := Ord(ATrack.IsMissing);

    if (ExistingID = 0) then
      Q.ParamByName('FullPath').AsString := ATrack.FullPath
    else
      Q.ParamByName('TrackID').AsInteger := ExistingID;

    Q.ExecSQL;

    if (ExistingID = 0) then
      Result := QueryInt('SELECT last_insert_rowid()')
    else
      Result := ExistingID;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistDb.CreatePlaylist(const AName: string;
                                       const ANotes: string = ''): Integer;
var
  Q: TFDQuery;
  SNow: string;

begin

  SNow := RDJDateTimeToDbStr(Now);

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FConnection;
    Q.SQL.Text := 'INSERT INTO Playlists (Name, Notes, CreatedUtc, ModifiedUtc) ' +
                  'VALUES (:Name, :Notes, :CreatedUtc, :ModifiedUtc)';

    Q.ParamByName('Name').AsString := AName;
    Q.ParamByName('Notes').AsString := ANotes;
    Q.ParamByName('CreatedUtc').AsString := SNow;
    Q.ParamByName('ModifiedUtc').AsString := SNow;
    Q.ExecSQL;

    Result := QueryInt('SELECT last_insert_rowid()');
  finally

    Q.Free;
  end;
end;


procedure TRDJPlaylistDb.RenamePlaylist(const APlaylistID: Integer;
                                        const ANewName: string);
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try

    Q.Connection := FConnection;
    Q.SQL.Text := 'UPDATE Playlists SET Name = :Name, ModifiedUtc = :ModifiedUtc ' +
                  'WHERE PlaylistID = :PlaylistID';

    Q.ParamByName('Name').AsString := ANewName;
    Q.ParamByName('ModifiedUtc').AsString := RDJDateTimeToDbStr(Now);
    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.ExecSQL;
  finally

    Q.Free;
  end;
end;


procedure TRDJPlaylistDb.DeletePlaylist(const APlaylistID: Integer);
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FConnection;

    Q.SQL.Text := 'DELETE FROM PlaylistItems WHERE PlaylistID = :PlaylistID';
    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.ExecSQL;

    Q.SQL.Text := 'DELETE FROM Playlists WHERE PlaylistID = :PlaylistID';
    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.ExecSQL;
  finally

    Q.Free;
  end;
end;


procedure TRDJPlaylistDb.ClearPlaylistItems(const APlaylistID: Integer);
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'DELETE FROM PlaylistItems WHERE PlaylistID = :PlaylistID';
    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.ExecSQL;
  finally

    Q.Free;
  end;
end;


function TRDJPlaylistDb.AddPlaylistItem(const APlaylistID: Integer;
                                        const ATrackID: Integer;
                                        const ASortOrder: Integer;
                                        const ACueInMs: Int64 = 0;
                                        const ACueOutMs: Int64 = 0;
                                        const AUserNote: string = ''): Integer;
var
  Q: TFDQuery;

begin

  Q := TFDQuery.Create(nil);

  try
    Q.Connection := FConnection;
    Q.SQL.Text := 'INSERT INTO PlaylistItems (' +
                  '  PlaylistID, TrackID, SortOrder, CueInMs, CueOutMs, UserNote' +
                  ') VALUES (' +
                  '  :PlaylistID, :TrackID, :SortOrder, :CueInMs, :CueOutMs, :UserNote' +
                  ')';

    Q.ParamByName('PlaylistID').AsInteger := APlaylistID;
    Q.ParamByName('TrackID').AsInteger := ATrackID;
    Q.ParamByName('SortOrder').AsInteger := ASortOrder;
    Q.ParamByName('CueInMs').AsLargeInt := ACueInMs;
    Q.ParamByName('CueOutMs').AsLargeInt := ACueOutMs;
    Q.ParamByName('UserNote').AsString := AUserNote;
    Q.ExecSQL;

    Result := QueryInt('SELECT last_insert_rowid()');
  finally

    Q.Free;
  end;
end;

// Helpers ---------------------------------------------------------------------

function RDJDateTimeToDbStr(const AValue: TDateTime): string;
begin

  if (AValue <= 0) then
    Exit('');

  Result := FormatDateTime('yyyy"-"mm"-"dd hh":"nn":"ss',
                           AValue);
end;


function RDJDbStrToDateTime(const S: string): TDateTime;
begin

  if (Trim(S) = '') then
    Exit(0);

  Result := StrToDateTime(S);
end;


procedure RDJShutdownPlaylistDbFireDAC();
begin

  try
    if FDManager.Active then
      FDManager.Close;

{$IF CompilerVersion = 28.0}
    // Delphi XE7 FireDAC assigns its SQLite shutdown callback to
    // sqlite3_initialize by mistake, so close the SQLite runtime explicitly.
    sqlite3_shutdown();
{$IFEND}
  except
    on E: Exception do
      OutputDebugString(PChar('RDJ playlist FireDAC shutdown failed: ' +
                              E.Message));
  end;
end;

end.
