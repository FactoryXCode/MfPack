// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.PlaylistTypes.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Contains the records for the playlist editor..
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
unit RDJ.PlaylistTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type

  TRDJTrack = record
    TrackID: Integer;

    FullPath: string;
    FileName: string;
    FileExt: string;

    Title: string;
    Artist: string;
    Album: string;
    AlbumArtist: string;
    Genre: string;
    Comment: string;
    Composer: string;

    Year: Integer;
    TrackNumber: Integer;
    DiscNumber: Integer;

    DurationMs: Int64;
    BitRate: Integer;
    BitrateKbps: Integer;
    SampleRate: Integer;
    Channels: Integer;
    BitsPerSample: Integer;
    Codec: string;
    QualityLabel: string;

    BPM: Double;
    MusicalKey: string;
    GainDb: Double;

    FileSize: Int64;
    FileModifiedUtc: TDateTime;
    DateAddedUtc: TDateTime;
    LastScanUtc: TDateTime;

    Rating: Integer;
    ColorTag: Integer;

    CueInMs: Int64;
    CueOutMs: Int64;

    IsMissing: Boolean;
  end;

  TRDJPlaylistInfo = record
    PlaylistID: Integer;
    Name: string;
    Notes: string;
    CreatedUtc: TDateTime;
    ModifiedUtc: TDateTime;
  end;

  TRDJPlaylistItem = record
    PlaylistItemID: Integer;
    PlaylistID: Integer;
    TrackID: Integer;
    SortOrder: Integer;

    CueInMs: Int64;
    CueOutMs: Int64;

    UserNote: string;
  end;

  TRDJPlaylistEntry = record
    PlaylistItemID: Integer;
    PlaylistID: Integer;
    TrackID: Integer;
    SortOrder: Integer;

    CueInMs: Int64;
    CueOutMs: Int64;
    UserNote: string;

    Track: TRDJTrack;
  end;

  TRDJPlaylist = class
  private

    FInfo: TRDJPlaylistInfo;
    FItems: TList<TRDJPlaylistEntry>;
    FModified: Boolean;

    function GetCount(): Integer;
    function GetItem(Index: Integer): TRDJPlaylistEntry;
    procedure SetItem(Index: Integer; const Value: TRDJPlaylistEntry);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Clear();
    procedure Add(const AEntry: TRDJPlaylistEntry);
    procedure Delete(Index: Integer);
    procedure MoveItem(FromIndex,
                       ToIndex: Integer);
    procedure RebuildSortOrder();

    property Info: TRDJPlaylistInfo
      read FInfo
      write FInfo;
    property Count: Integer
      read GetCount;
    property Items[Index: Integer]: TRDJPlaylistEntry
      read GetItem
      write SetItem; default;
    property Modified: Boolean
      read FModified
      write FModified;
  end;

  function RDJEmptyTrack(): TRDJTrack;
  function RDJEmptyPlaylistInfo(): TRDJPlaylistInfo;
  function RDJEmptyPlaylistItem(): TRDJPlaylistItem;
  function RDJEmptyPlaylistEntry(): TRDJPlaylistEntry;


implementation

function RDJEmptyTrack(): TRDJTrack;
begin

  Result := Default(TRDJTrack);
end;


function RDJEmptyPlaylistInfo(): TRDJPlaylistInfo;
begin

  Result := Default(TRDJPlaylistInfo);
end;


function RDJEmptyPlaylistItem(): TRDJPlaylistItem;
begin

  Result := Default(TRDJPlaylistItem);
end;


function RDJEmptyPlaylistEntry(): TRDJPlaylistEntry;
begin

  Result := Default(TRDJPlaylistEntry);
end;


constructor TRDJPlaylist.Create();
begin

  inherited Create;

  FItems := TList<TRDJPlaylistEntry>.Create;
  FInfo := RDJEmptyPlaylistInfo;
  FModified := False;
end;


destructor TRDJPlaylist.Destroy();
begin

  FreeAndNil(FItems);

  inherited Destroy;
end;


procedure TRDJPlaylist.Clear();
begin

  FItems.Clear;
  FModified := True;
end;


procedure TRDJPlaylist.Add(const AEntry: TRDJPlaylistEntry);
begin

  FItems.Add(AEntry);
  FModified := True;
end
;

procedure TRDJPlaylist.Delete(Index: Integer);
begin

  if (Index < 0) or (Index >= FItems.Count) then
    Exit;

  FItems.Delete(Index);
  RebuildSortOrder();
  FModified := True;
end;


procedure TRDJPlaylist.MoveItem(FromIndex,
                                ToIndex: Integer);
var
  Item: TRDJPlaylistEntry;

begin

  if (FromIndex < 0) or (FromIndex >= FItems.Count) then
    Exit;

  if (ToIndex < 0) or (ToIndex >= FItems.Count) then
    Exit;

  if (FromIndex = ToIndex) then
    Exit;

  Item := FItems[FromIndex];
  FItems.Delete(FromIndex);
  FItems.Insert(ToIndex,
                Item);

  RebuildSortOrder();
  FModified := True;
end;


procedure TRDJPlaylist.RebuildSortOrder();
var
  i: Integer;
  E: TRDJPlaylistEntry;

begin

  for i := 0 to FItems.Count - 1 do
    begin

      E := FItems[i];
      E.SortOrder := i + 1;
      FItems[i] := E;
    end;
end;


function TRDJPlaylist.GetCount(): Integer;
begin

  Result := FItems.Count;
end;


function TRDJPlaylist.GetItem(Index: Integer): TRDJPlaylistEntry;
begin

  Result := FItems[Index];
end;


procedure TRDJPlaylist.SetItem(Index: Integer;
                               const Value: TRDJPlaylistEntry);
begin

  FItems[Index] := Value;
  FModified := True;
end;

end.
