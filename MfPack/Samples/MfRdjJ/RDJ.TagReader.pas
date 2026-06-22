// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.TagReader.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Reads ID3v1Tag from mp3.
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
unit RDJ.TagReader;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  RDJ.PlaylistTypes,
  RDJ.FilenameParser;

  function RDJReadTrackInfo(const AFileName: string;
                            out ATrack: TRDJTrack): Boolean;
  function ReadDurationMf(const AFileName: string;
                          var ATrack: TRDJTrack): Boolean;

implementation

type

  TID3v1Tag = packed record
    TagID: array[0..2] of AnsiChar;      // 'TAG'
    Title: array[0..29] of AnsiChar;
    Artist: array[0..29] of AnsiChar;
    Album: array[0..29] of AnsiChar;
    Year: array[0..3] of AnsiChar;
    Comment: array[0..29] of AnsiChar;
    Genre: Byte;
  end;

const

  ID3V1_GENRES: array[0..147] of string = (
    'Blues','Classic Rock','Country','Dance','Disco','Funk','Grunge','Hip-Hop',
    'Jazz','Metal','New Age','Oldies','Other','Pop','R&B','Rap',
    'Reggae','Rock','Techno','Industrial','Alternative','Ska','Death Metal','Pranks',
    'Soundtrack','Euro-Techno','Ambient','Trip-Hop','Vocal','Jazz+Funk','Fusion','Trance',
    'Classical','Instrumental','Acid','House','Game','Sound Clip','Gospel','Noise',
    'AlternRock','Bass','Soul','Punk','Space','Meditative','Instrumental Pop','Instrumental Rock',
    'Ethnic','Gothic','Darkwave','Techno-Industrial','Electronic','Pop-Folk','Eurodance','Dream',
    'Southern Rock','Comedy','Cult','Gangsta','Top 40','Christian Rap','Pop/Funk','Jungle',
    'Native American','Cabaret','New Wave','Psychadelic','Rave','Showtunes','Trailer','Lo-Fi',
    'Tribal','Acid Punk','Acid Jazz','Polka','Retro','Musical','Rock & Roll','Hard Rock',
    'Folk','Folk/Rock','National Folk','Swing','Fast Fusion','Bebob','Latin','Revival',
    'Celtic','Bluegrass','Avantgarde','Gothic Rock','Progressive Rock','Psychedelic Rock','Symphonic Rock','Slow Rock',
    'Big Band','Chorus','Easy Listening','Acoustic','Humour','Speech','Chanson','Opera',
    'Chamber Music','Sonata','Symphony','Booty Bass','Primus','Porn Groove','Satire','Slow Jam',
    'Club','Tango','Samba','Folklore','Ballad','Power Ballad','Rhythmic Soul','Freestyle',
    'Duet','Punk Rock','Drum Solo','Acapella','Euro-House','Dance Hall','Goa','Drum & Bass',
    'Club-House','Hardcore','Terror','Indie','BritPop','Negerpunk','Polsk Punk','Beat',
    'Christian Gangsta Rap','Heavy Metal','Black Metal','Crossover','Contemporary Christian','Christian Rock','Merengue','Salsa',
    'Thrash Metal','Anime','JPop','SynthPop'
  );

function ReadUInt32LE(const P: PByte): Cardinal;
begin

  Result := Cardinal(P[0]) or
            (Cardinal(P[1]) shl 8) or
            (Cardinal(P[2]) shl 16) or
            (Cardinal(P[3]) shl 24);
end;


function ReadUInt24BE(const P: PByte): Cardinal;
begin

  Result := (Cardinal(P[0]) shl 16) or
            (Cardinal(P[1]) shl 8) or
             Cardinal(P[2]);
end;


function ReadUInt32BE(const P: PByte): Cardinal;
begin

  Result := (Cardinal(P[0]) shl 24) or
            (Cardinal(P[1]) shl 16) or
            (Cardinal(P[2]) shl 8) or
             Cardinal(P[3]);
end;


function ReadUInt64BE(const P: PByte; Count: Integer): UInt64;
var
  i: Integer;

begin

  Result := 0;
  for i := 0 to Count - 1 do
    Result := (Result shl 8) or P[i];
end;


function ReadSyncSafeInt32(const P: PByte): Integer;
begin
  Result := (Integer(P[0] and $7F) shl 21) or
            (Integer(P[1] and $7F) shl 14) or
            (Integer(P[2] and $7F) shl 7) or
             Integer(P[3] and $7F);
end;


function TrimNullsAndSpaces(const S: string): string;
begin
  Result := Trim(StringReplace(S,
                               #0,
                               '',
                               [rfReplaceAll]));
end;


function ParseLeadingInt(const S: string): Integer;
var
  T: string;
  p: Integer;

begin

  T := Trim(S);
  p := Pos('/',
           T);

  if (p > 0) then
    T := Copy(T,
              1,
              p - 1);

  Result := StrToIntDef(T,
                        0);
end;


function GenreFromId3v1(const AIndex: Byte): string;
begin

  if (AIndex <= High(ID3V1_GENRES)) then
    Result := ID3V1_GENRES[AIndex]
  else
    Result := '';

end;

function DecodeLatin1(const B: TBytes): string;
var
  i: Integer;

begin

  SetLength(Result,
            Length(B));

  for i := 0 to Length(B) - 1 do
    Result[i + 1] := Char(B[i]);

  Result := TrimNullsAndSpaces(Result);
end;


function DecodeUtf16LeWithoutBom(const B: TBytes): string;
begin

  Result := TEncoding.Unicode.GetString(B);
  Result := TrimNullsAndSpaces(Result);
end;


function DecodeUtf16BeWithoutBom(const B: TBytes): string;
var
  tmp: TBytes;
  i: Integer;

begin

  SetLength(tmp,
            Length(B));
  i := 0;

  while i + 1 < Length(B) do
    begin

      tmp[i] := B[i + 1];
      tmp[i + 1] := B[i];
      Inc(i,
          2);
    end;

  Result := TEncoding.Unicode.GetString(tmp);
  Result := TrimNullsAndSpaces(Result);
end;


function DecodeId3Text(const B: TBytes): string;
var
  Enc: Byte;
  Data: TBytes;

begin

  Result := '';
  if (Length(B) = 0) then
    Exit;

  Enc := B[0];
  Data := Copy(B,
               1,
               Length(B) - 1);

  case Enc of
    0: Result := DecodeLatin1(Data);                      // ISO-8859-1
    1: begin                                              // UTF-16 with BOM or LE fallback
         if Length(Data) >= 2 then
           begin
             if (Data[0] = $FF) and (Data[1] = $FE) then
               Result := TrimNullsAndSpaces(TEncoding.Unicode.GetString(Copy(Data, 2, Length(Data) - 2)))
             else if (Data[0] = $FE) and (Data[1] = $FF) then
               Result := DecodeUtf16BeWithoutBom(Copy(Data, 2, Length(Data) - 2))
             else
               Result := DecodeUtf16LeWithoutBom(Data);
           end;
       end;
    2: Result := DecodeUtf16BeWithoutBom(Data);           // UTF-16BE no BOM
    3: Result := TrimNullsAndSpaces(TEncoding.UTF8.GetString(Data)); // UTF-8
  else
    Result := DecodeLatin1(Data);
  end;
end;


procedure ApplyVorbisField(const AName,
                           AValue: string;
                           var ATrack: TRDJTrack);
var
  N: string;
  V: string;

begin

  N := UpperCase(Trim(AName));
  V := Trim(AValue);

  if N = 'TITLE' then
    ATrack.Title := V
  else if N = 'ARTIST' then
    ATrack.Artist := V
  else if N = 'ALBUM' then
    ATrack.Album := V
  else if N = 'ALBUMARTIST' then
    ATrack.AlbumArtist := V
  else if N = 'GENRE' then
    ATrack.Genre := V
  else if N = 'COMMENT' then
    ATrack.Comment := V
  else if N = 'COMPOSER' then
    ATrack.Composer := V
  else if (N = 'TRACKNUMBER') or (N = 'TRACK') then
    ATrack.TrackNumber := ParseLeadingInt(V)
  else if (N = 'DISCNUMBER') or (N = 'DISC') then
    ATrack.DiscNumber := ParseLeadingInt(V)
  else if (N = 'DATE') or (N = 'YEAR') then
    ATrack.Year := ParseLeadingInt(V)
  else if N = 'BPM' then
    ATrack.BPM := StrToFloatDef(StringReplace(V, ',', '.', [rfReplaceAll]), 0.0)
  else if N = 'INITIALKEY' then
    ATrack.MusicalKey := V
  else if N = 'REPLAYGAIN_TRACK_GAIN' then
    begin
      V := StringReplace(V, ' dB', '', [rfIgnoreCase, rfReplaceAll]);
      ATrack.GainDb := StrToFloatDef(StringReplace(V, ',', '.', [rfReplaceAll]), 0.0);
    end;
end;


procedure ApplyId3TextFrame(const AFrameID,
                            AValue: string;
                            var ATrack: TRDJTrack);
var
  S: string;

begin
  S := Trim(AValue);

  if AFrameID = 'TIT2' then
    ATrack.Title := S
  else if AFrameID = 'TPE1' then
    ATrack.Artist := S
  else if AFrameID = 'TALB' then
    ATrack.Album := S
  else if AFrameID = 'TPE2' then
    ATrack.AlbumArtist := S
  else if AFrameID = 'TCON' then
    ATrack.Genre := S
  else if AFrameID = 'TCOM' then
    ATrack.Composer := S
  else if AFrameID = 'TRCK' then
    ATrack.TrackNumber := ParseLeadingInt(S)
  else if AFrameID = 'TPOS' then
    ATrack.DiscNumber := ParseLeadingInt(S)
  else if (AFrameID = 'TYER') or (AFrameID = 'TDRC') then
    ATrack.Year := ParseLeadingInt(S)
  else if AFrameID = 'TBPM' then
    ATrack.BPM := StrToFloatDef(StringReplace(S, ',', '.', [rfReplaceAll]), 0.0)
  else if AFrameID = 'TKEY' then
    ATrack.MusicalKey := S;
end;


function ReadId3v1(const AFileName: string;
                   var ATrack: TRDJTrack): Boolean;
var
  FS: TFileStream;
  Tag: TID3v1Tag;
  S: AnsiString;

begin

  Result := False;

  if not SameText(ExtractFileExt(AFileName), '.mp3') then
    Exit;

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);
  try
    if FS.Size < SizeOf(TID3v1Tag) then
      Exit;

    FS.Position := FS.Size - SizeOf(TID3v1Tag);
    if FS.Read(Tag, SizeOf(TID3v1Tag)) <> SizeOf(TID3v1Tag) then
      Exit;

    SetString(S,
              PAnsiChar(@Tag.TagID[0]),
              3);

    if string(S) <> 'TAG' then
      Exit;

    if ATrack.Title = '' then
      ATrack.Title := Trim(string(AnsiString(Tag.Title)));
    if ATrack.Artist = '' then
      ATrack.Artist := Trim(string(AnsiString(Tag.Artist)));
    if ATrack.Album = '' then
      ATrack.Album := Trim(string(AnsiString(Tag.Album)));
    if ATrack.Year = 0 then
      ATrack.Year := StrToIntDef(Trim(string(AnsiString(Tag.Year))), 0);
    if ATrack.Comment = '' then
      ATrack.Comment := Trim(string(AnsiString(Tag.Comment)));
    if ATrack.Genre = '' then
      ATrack.Genre := GenreFromId3v1(Tag.Genre);

    Result := True;
  finally
    FS.Free;
  end;
end;


function ReadId3v2(const AFileName: string;
                   var ATrack: TRDJTrack): Boolean;
var
  FS: TFileStream;
  Hdr: array[0..9] of Byte;
  TagSize: Integer;
  TagData: TBytes;
  P, FrameSize, MajorVer: Integer;
  FrameID: AnsiString;
  FrameBytes: TBytes;
  TextValue: string;

begin
  Result := False;

  if not SameText(ExtractFileExt(AFileName), '.mp3') then
    Exit;

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);
  try
    if FS.Size < 10 then
      Exit;

    if FS.Read(Hdr, SizeOf(Hdr)) <> SizeOf(Hdr) then
      Exit;

    if not ((Hdr[0] = Ord('I')) and (Hdr[1] = Ord('D')) and (Hdr[2] = Ord('3'))) then
      Exit;

    MajorVer := Hdr[3];
    TagSize := ReadSyncSafeInt32(@Hdr[6]);
    if TagSize <= 0 then
      Exit;

    SetLength(TagData,
              TagSize);
    if FS.Read(TagData[0], TagSize) <> TagSize then
      Exit;

    P := 0;
    while P + 10 <= Length(TagData) do
      begin
        FrameID := AnsiString(Char(TagData[P]) +
                              Char(TagData[P + 1]) +
                              Char(TagData[P + 2]) +
                              Char(TagData[P + 3]));

        if Trim(string(FrameID)) = '' then
          Break;

        if MajorVer = 4 then
          FrameSize := ReadSyncSafeInt32(@TagData[P + 4])
        else
          FrameSize := ReadUInt32BE(@TagData[P + 4]);

        if FrameSize <= 0 then
          Break;

        if P + 10 + FrameSize > Length(TagData) then
          Break;

        FrameBytes := Copy(TagData,
                           P + 10,
                           FrameSize);

        if (FrameID = 'COMM') then
          begin
            if Length(FrameBytes) > 4 then
              begin
                // Skip text encoding + 3-byte language, then take the rest.
                TextValue := DecodeId3Text(Copy(FrameBytes,
                                                4,
                                                Length(FrameBytes) - 4));
                if ATrack.Comment = '' then
                  ATrack.Comment := TextValue;
              end;
          end
        else if (FrameID[1] = 'T') then
          begin
            TextValue := DecodeId3Text(FrameBytes);
            ApplyId3TextFrame(string(FrameID),
                              TextValue,
                              ATrack);
          end;

        Inc(P,
            10 + FrameSize);
      end;

    Result := True;
  finally
    FS.Free;
  end;
end;


function ReadFlacTags(const AFileName: string;
                      var ATrack: TRDJTrack): Boolean;
var
  FS: TFileStream;
  Sig: array[0..3] of Byte;
  LastBlock: Boolean;
  BlockType: Byte;
  BlockSize: Cardinal;
  BlockData: TBytes;
  VendorLen, CommentCount, CommentLen: Cardinal;
  P, i, EqPos: Integer;
  Cmt: UTF8String;
  NamePart, ValuePart: string;
  V: UInt64;

begin

  Result := False;

  if not SameText(ExtractFileExt(AFileName), '.flac') then
    Exit;
  LastBlock := False;

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);
  try
    if FS.Size < 4 then
      Exit;

    if FS.Read(Sig, SizeOf(Sig)) <> SizeOf(Sig) then
      Exit;

    if not ((Sig[0] = Ord('f')) and
            (Sig[1] = Ord('L')) and
            (Sig[2] = Ord('a')) and
            (Sig[3] = Ord('C'))) then
      Exit;

    repeat
      if FS.Position + 4 > FS.Size then
        Break;

      FS.ReadBuffer(Sig, 4);

      LastBlock := (Sig[0] and $80) <> 0;
      BlockType := Sig[0] and $7F;
      BlockSize := ReadUInt24BE(@Sig[1]);

      SetLength(BlockData,
                BlockSize);

      if BlockSize > 0 then
        FS.ReadBuffer(BlockData[0], BlockSize);

      case BlockType of
        0: begin
             // STREAMINFO: first 34 bytes
             if Length(BlockData) >= 18 then
               begin
                 V := ReadUInt64BE(@BlockData[10], 8);
                 ATrack.SampleRate := Integer((V shr 44) and $FFFFF);
                 ATrack.Channels := Integer(((V shr 41) and $7) + 1);
                 // bits per sample is available too, but not stored in TRDJTrack
                 if (ATrack.SampleRate > 0) then
                   ATrack.DurationMs := Int64((V and $0FFFFFFFFF) * 1000) div ATrack.SampleRate;
                 if (ATrack.DurationMs <= 0) then
                   ReadDurationMf(AFileName,
                                  ATrack);
               end;
           end;

        4: begin
             // VORBIS_COMMENT
             if Length(BlockData) >= 8 then
               begin
                 P := 0;

                 VendorLen := ReadUInt32LE(@BlockData[P]);
                 Inc(P, 4 + Integer(VendorLen));

                 if P + 4 > Length(BlockData) then
                   Continue;

                 CommentCount := ReadUInt32LE(@BlockData[P]);
                 Inc(P, 4);

                 for i := 0 to Integer(CommentCount) - 1 do
                   begin
                     if P + 4 > Length(BlockData) then
                       Break;

                     CommentLen := ReadUInt32LE(@BlockData[P]);
                     Inc(P, 4);

                     if P + Integer(CommentLen) > Length(BlockData) then
                       Break;

                     SetString(Cmt,
                               PAnsiChar(@BlockData[P]),
                               CommentLen);

                     Inc(P, Integer(CommentLen));

                     EqPos := Pos('=',
                                  string(Cmt));
                     if EqPos > 0 then
                       begin
                         NamePart := Copy(string(Cmt), 1, EqPos - 1);
                         ValuePart := Copy(string(Cmt), EqPos + 1, MaxInt);
                         ApplyVorbisField(NamePart,
                                          ValuePart,
                                          ATrack);
                       end;
                   end;
               end;
           end;
      end;
    until LastBlock;

    Result := True;
  finally
    FS.Free;
  end;
end;


procedure ApplyFallbacks(const AFileName: string;
                         var ATrack: TRDJTrack);
begin

  if ATrack.Title = '' then
    ATrack.Title := ChangeFileExt(ExtractFileName(AFileName), '');

  if ATrack.FileName = '' then
    ATrack.FileName := ExtractFileName(AFileName);

  if ATrack.FileExt = '' then
    ATrack.FileExt := LowerCase(ExtractFileExt(AFileName));
end;


function RDJReadTrackInfo(const AFileName: string;
                          out ATrack: TRDJTrack): Boolean;
begin

  Result := False;
  ATrack := RDJEmptyTrack;

  if (Trim(AFileName) = '') then
    Exit;

  if not FileExists(AFileName) then
    Exit;

  ATrack.FullPath := AFileName;
  ATrack.FileName := ExtractFileName(AFileName);
  ATrack.FileExt := LowerCase(ExtractFileExt(AFileName));

  ATrack.DateAddedUtc := Now;
  ATrack.LastScanUtc := Now;
  ATrack.IsMissing := False;

  // Carmen rule:
  // Use filename parsing, not embedded tags.
  // Tags are often missing/wrong and caused empty/wrong artist fields.
  TFileNameParser.ResolveArtistTitle(AFileName,
                                     '',
                                     '',
                                     ATrack.Artist,
                                     ATrack.Title);

  ReadDurationMf(AFileName,
                 ATrack);

  ApplyFallbacks(AFileName,
                 ATrack);

  Result := True;
end;


function ReadDurationMf(const AFileName: string;
                        var ATrack: TRDJTrack): Boolean;
var
  hr: HRESULT;
  DurHns: LONGLONG;

begin

  Result := False;
  DurHns := 0;

  hr := GetFileDuration(PWideChar(WideString(AFileName)),
                        DurHns);
  if FAILED(hr) then
    Exit;

  if (DurHns <= 0) then
    Exit;

  // MF duration is in 100-ns units.
  ATrack.DurationMs := DurHns div 10000;
  Result := True;
end;

end.
