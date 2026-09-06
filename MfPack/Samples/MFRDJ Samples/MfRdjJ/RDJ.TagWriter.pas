// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.TagWriter.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Writes tags to mp3 tracks.
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
//          Please, read documentation carefully!
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
unit RDJ.TagWriter;

interface

uses

  {Winapi}
  Winapi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  {Application}
  RDJ.PlaylistTypes;

function RDJWriteTrackTags(const AFileName: string;
                           const ATrack: TRDJTrack;
                           out AError: string): Boolean;

implementation

type

  TFlacMetaBlock = record
    BlockType: Byte;
    Data: TBytes;
  end;

  TFlacMetaBlockArray = array of TFlacMetaBlock;

  TMP4AtomInfo = record
    AtomType: AnsiString;
    HeaderSize: Int64;
    AtomSize: Int64;
    DataOffset: Int64;
    DataSize: Int64;
  end;


function ReadUInt24BE(const P: PByte): Cardinal;
begin

  Result := (Cardinal(P[0]) shl 16) or
            (Cardinal(P[1]) shl 8) or
             Cardinal(P[2]);
end;


function ReadUInt32LE(const P: PByte): Cardinal;
begin

  Result := Cardinal(P[0]) or
            (Cardinal(P[1]) shl 8) or
            (Cardinal(P[2]) shl 16) or
            (Cardinal(P[3]) shl 24);
end;


function ReadUInt32BE(const P: PByte): Cardinal;
begin

  Result := (Cardinal(P[0]) shl 24) or
            (Cardinal(P[1]) shl 16) or
            (Cardinal(P[2]) shl 8) or
             Cardinal(P[3]);
end;


function ReadUInt64BE(const P: PByte): UInt64;
begin

  Result := (UInt64(P[0]) shl 56) or
            (UInt64(P[1]) shl 48) or
            (UInt64(P[2]) shl 40) or
            (UInt64(P[3]) shl 32) or
            (UInt64(P[4]) shl 24) or
            (UInt64(P[5]) shl 16) or
            (UInt64(P[6]) shl 8) or
             UInt64(P[7]);
end;


procedure WriteUInt16BEToStream(const AStream: TStream;
                                const AValue: Word);
var
  B: array[0..1] of Byte;

begin

  B[0] := Byte((AValue shr 8) and $FF);
  B[1] := Byte(AValue and $FF);
  AStream.WriteBuffer(B,
                      SizeOf(B));
end;


procedure WriteUInt32BEToStream(const AStream: TStream;
                                const AValue: Cardinal);
var
  B: array[0..3] of Byte;

begin

  B[0] := Byte((AValue shr 24) and $FF);
  B[1] := Byte((AValue shr 16) and $FF);
  B[2] := Byte((AValue shr 8) and $FF);
  B[3] := Byte(AValue and $FF);

  AStream.WriteBuffer(B,
                      SizeOf(B));
end;


procedure WriteUInt32LEToStream(const AStream: TStream;
                                const AValue: Cardinal);
var
  B: array[0..3] of Byte;

begin

  B[0] := Byte(AValue and $FF);
  B[1] := Byte((AValue shr 8) and $FF);
  B[2] := Byte((AValue shr 16) and $FF);
  B[3] := Byte((AValue shr 24) and $FF);

  AStream.WriteBuffer(B,
                      SizeOf(B));
end;

procedure WriteUInt64BEToStream(const AStream: TStream;
                                const AValue: UInt64);
var
  B: array[0..7] of Byte;

begin

  B[0] := Byte((AValue shr 56) and $FF);
  B[1] := Byte((AValue shr 48) and $FF);
  B[2] := Byte((AValue shr 40) and $FF);
  B[3] := Byte((AValue shr 32) and $FF);
  B[4] := Byte((AValue shr 24) and $FF);
  B[5] := Byte((AValue shr 16) and $FF);
  B[6] := Byte((AValue shr 8) and $FF);
  B[7] := Byte(AValue and $FF);

  AStream.WriteBuffer(B,
                      SizeOf(B));
end;


procedure WriteSyncSafeInt32ToStream(const AStream: TStream;
                                     const AValue: Cardinal);
var
  B: array[0..3] of Byte;

begin

  B[0] := Byte((AValue shr 21) and $7F);
  B[1] := Byte((AValue shr 14) and $7F);
  B[2] := Byte((AValue shr 7) and $7F);
  B[3] := Byte(AValue and $7F);

  AStream.WriteBuffer(B,
                      SizeOf(B));
end;


procedure WriteAtomHeader(const AStream: TStream;
                          const AAtomType: AnsiString;
                          const AAtomSize: Int64);
var
  NameBuf: array[0..3] of AnsiChar;

begin

  if (Length(AAtomType) <> 4) then
    raise Exception.Create('MP4 atom name must be 4 chars.');

  if (AAtomSize <= High(Cardinal)) then
    begin

      WriteUInt32BEToStream(AStream,
                            Cardinal(AAtomSize));
      NameBuf[0] := AAtomType[1];
      NameBuf[1] := AAtomType[2];
      NameBuf[2] := AAtomType[3];
      NameBuf[3] := AAtomType[4];
      AStream.WriteBuffer(NameBuf,
                          SizeOf(NameBuf));
    end
  else
    begin

      WriteUInt32BEToStream(AStream,
                            1);
      NameBuf[0] := AAtomType[1];
      NameBuf[1] := AAtomType[2];
      NameBuf[2] := AAtomType[3];
      NameBuf[3] := AAtomType[4];
      AStream.WriteBuffer(NameBuf,
                          SizeOf(NameBuf));
      WriteUInt64BEToStream(AStream,
                            AAtomSize);
    end;
end;


procedure WriteAtomToStream(const AStream: TStream;
                            const AAtomType: AnsiString;
                            const AData: TBytes);
var
  AtomSize: Int64;

begin

  AtomSize := 8 + Length(AData);

  WriteAtomHeader(AStream,
                  AAtomType,
                  AtomSize);

  if (Length(AData) > 0) then
    AStream.WriteBuffer(AData[0],
                        Length(AData));
end;


function StreamToBytes(const AStream: TMemoryStream): TBytes;
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


function Utf16BomTextBytes(const S: string): TBytes;
var
  Raw: TBytes;

begin

  Raw := TEncoding.Unicode.GetBytes(S);

  SetLength(Result,
            Length(Raw) + 3);

  Result[0] := 1;
  Result[1] := $FF;
  Result[2] := $FE;

  if (Length(Raw) > 0) then
    Move(Raw[0],
         Result[3],
         Length(Raw));
end;


function Utf16BomBytesNoEncodingByte(const S: string): TBytes;
var
  Raw: TBytes;

begin

  Raw := TEncoding.Unicode.GetBytes(S);
  SetLength(Result,
            Length(Raw) + 2);

  Result[0] := $FF;
  Result[1] := $FE;

  if (Length(Raw) > 0) then
    Move(Raw[0],
         Result[2],
         Length(Raw));
end;


procedure AddId3TextFrame(const AFrames: TStream;
                          const AFrameID: AnsiString;
                          const AValue: string);
var
  Payload: TBytes;
  Flags: array[0..1] of Byte;

begin

  if (Trim(AValue) = '') then
    Exit;

  Payload := Utf16BomTextBytes(AValue);

  AFrames.WriteBuffer(AFrameID[1],
                      4);
  WriteUInt32BEToStream(AFrames,
                        Length(Payload));

  Flags[0] := 0;
  Flags[1] := 0;

  AFrames.WriteBuffer(Flags,
                      SizeOf(Flags));

  if (Length(Payload) > 0) then
    AFrames.WriteBuffer(Payload[0],
                        Length(Payload));
end;


procedure AddId3CommentFrame(const AFrames: TStream;
                             const AComment: string);
var
  Payload: TBytes;
  Flags: array[0..1] of Byte;
  Lang: array[0..2] of AnsiChar;
  DescBytes: TBytes;
  TextBytes: TBytes;
  P: Integer;
  FrameID: AnsiString;

begin

  if (Trim(AComment) = '') then
    Exit;

  Lang[0] := 'e';
  Lang[1] := 'n';
  Lang[2] := 'g';

  DescBytes := Utf16BomBytesNoEncodingByte('');
  TextBytes := Utf16BomBytesNoEncodingByte(AComment);

  SetLength(Payload,
            1 + 3 + Length(DescBytes) + 2 + Length(TextBytes));
  P := 0;

  Payload[P] := 1;
  Inc(P);

  Move(Lang[0],
       Payload[P], 3);
  Inc(P,
      3);

  if (Length(DescBytes) > 0) then
    begin

      Move(DescBytes[0],
           Payload[P],
           Length(DescBytes));
      Inc(P,
          Length(DescBytes));
    end;

  Payload[P] := 0;
  Inc(P);
  Payload[P] := 0;
  Inc(P);

  if (Length(TextBytes) > 0) then
    Move(TextBytes[0],
         Payload[P],
         Length(TextBytes));

  FrameID := 'COMM';
  AFrames.WriteBuffer(FrameID[1],
                      4);
  WriteUInt32BEToStream(AFrames,
                        Length(Payload));

  Flags[0] := 0;
  Flags[1] := 0;

  AFrames.WriteBuffer(Flags,
                      SizeOf(Flags));

  if (Length(Payload) > 0) then
    AFrames.WriteBuffer(Payload[0],
                        Length(Payload));
end;


function BuildId3v23Tag(const ATrack: TRDJTrack): TBytes;
var
  Frames: TMemoryStream;
  Tag: TMemoryStream;
  Header: array[0..2] of AnsiChar;
  VersionFlags: array[0..2] of Byte;
  S: string;

begin

  Frames := TMemoryStream.Create();

  try

    AddId3TextFrame(Frames,
                    'TIT2',
                    ATrack.Title);

    AddId3TextFrame(Frames,
                    'TPE1',
                    ATrack.Artist);

    AddId3TextFrame(Frames,
                    'TALB',
                    ATrack.Album);

    AddId3TextFrame(Frames,
                    'TPE2',
                    ATrack.AlbumArtist);

    AddId3TextFrame(Frames,
                    'TCON',
                    ATrack.Genre);

    AddId3TextFrame(Frames,
                    'TCOM',
                    ATrack.Composer);

    if ATrack.Year <> 0 then
      AddId3TextFrame(Frames,
                      'TYER',
                      IntToStr(ATrack.Year));

    if ATrack.TrackNumber <> 0 then
      AddId3TextFrame(Frames,
                      'TRCK',
                      IntToStr(ATrack.TrackNumber));

    if ATrack.DiscNumber <> 0 then
      AddId3TextFrame(Frames,
                      'TPOS',
                      IntToStr(ATrack.DiscNumber));

    if (ATrack.BPM <> 0) then
      begin

        S := StringReplace(FloatToStr(ATrack.BPM),
                           ',',
                           '.',
                           [rfReplaceAll]);

        AddId3TextFrame(Frames,
                        'TBPM',
                        S);
      end;

    AddId3TextFrame(Frames,
                    'TKEY',
                    ATrack.MusicalKey);

    AddId3CommentFrame(Frames,
                       ATrack.Comment);

    Tag := TMemoryStream.Create();

    try

      Header[0] := 'I';
      Header[1] := 'D';
      Header[2] := '3';
      Tag.WriteBuffer(Header,
                      SizeOf(Header));

      VersionFlags[0] := 3;
      VersionFlags[1] := 0;
      VersionFlags[2] := 0;

      Tag.WriteBuffer(VersionFlags,
                      SizeOf(VersionFlags));

      WriteSyncSafeInt32ToStream(Tag,
                                 Frames.Size);
      Frames.Position := 0;
      Tag.CopyFrom(Frames,
                   0);

      SetLength(Result,
                Tag.Size);

      if (Tag.Size > 0) then
        begin

          Tag.Position := 0;
          Tag.ReadBuffer(Result[0],
                         Tag.Size);
        end;
    finally

      Tag.Free;
    end;

  finally

    Frames.Free;
  end;
end;


function GetId3v2Size(const AFileName: string): Int64;
var
  FS: TFileStream;
  Hdr: array[0..9] of Byte;

begin

  Result := 0;

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);

  try

    if (FS.Size < 10) then
      Exit;

    if FS.Read(Hdr,
               SizeOf(Hdr)) <> SizeOf(Hdr) then
      Exit;

    if not ((Hdr[0] = Ord('I')) and
           (Hdr[1] = Ord('D')) and
           (Hdr[2] = Ord('3'))) then
      Exit;

    Result := 10 +
              ((Int64(Hdr[6] and $7F) shl 21) or
               (Int64(Hdr[7] and $7F) shl 14) or
               (Int64(Hdr[8] and $7F) shl 7) or
                Int64(Hdr[9] and $7F));
  finally

    FS.Free;
  end;
end;


function HasId3v1Tag(const AFileName: string): Boolean;
var
  FS: TFileStream;
  Hdr: array[0..2] of AnsiChar;

begin

  Result := False;

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);
  try

    if (FS.Size < 128) then
      Exit;

    FS.Position := FS.Size - 128;
    FS.ReadBuffer(Hdr, 3);
    Result := (Hdr[0] = 'T') and
              (Hdr[1] = 'A') and
              (Hdr[2] = 'G');
  finally

    FS.Free;
  end;
end;


function WriteMp3Tags(const AFileName: string;
                      const ATrack: TRDJTrack;
                      out AError: string): Boolean;
var
  InputStream: TFileStream;
  OutputStream: TFileStream;
  TempFileName: string;
  SkipStart: Int64;
  CopyBytes: Int64;
  TailBytes: Int64;
  Id3v1Bytes: Int64;
  Buf: array[0..65535] of Byte;
  ToRead: Integer;
  ReadCount: Integer;
  NewTag: TBytes;

begin

  Result := False;
  AError := '';

  TempFileName := AFileName + '.rdjtmp';
  SkipStart := GetId3v2Size(AFileName);

  if HasId3v1Tag(AFileName) then
    Id3v1Bytes := 128
  else
    Id3v1Bytes := 0;

  InputStream := TFileStream.Create(AFileName,
                                    fmOpenRead or fmShareDenyWrite);

  try

    TailBytes := InputStream.Size - SkipStart - Id3v1Bytes;

    if (TailBytes < 0) then
      TailBytes := 0;

    OutputStream := TFileStream.Create(TempFileName,
                                       fmCreate);

    try

      NewTag := BuildId3v23Tag(ATrack);
      if (Length(NewTag) > 0) then
        OutputStream.WriteBuffer(NewTag[0],
                                 Length(NewTag));

      InputStream.Position := SkipStart;
      CopyBytes := TailBytes;

      while (CopyBytes > 0) do
        begin

          if (CopyBytes > SizeOf(Buf)) then
            ToRead := SizeOf(Buf)
          else
            ToRead := CopyBytes;

          ReadCount := InputStream.Read(Buf,
                                        ToRead);
          if (ReadCount <= 0) then
            Break;

          OutputStream.WriteBuffer(Buf,
                                   ReadCount);
          Dec(CopyBytes,
              ReadCount);
        end;
    finally

      OutputStream.Free;
    end;

  finally

    InputStream.Free;
  end;

  if not ReplaceFile(PChar(AFileName),
                     PChar(TempFileName),
                     nil,
                     0,
                     nil,
                     nil) then
    begin

      AError := SysErrorMessage(GetLastError);
      if FileExists(TempFileName) then
        DeleteFile(TempFileName);
      Exit;
    end;

  Result := True;
end;


procedure AddVorbisPair(const AList: TStrings;
                        const AName,
                              AValue: string);
begin

  if (Trim(AValue) <> '') then
    AList.Add(UpperCase(Trim(AName)) + '=' + AValue);
end;


function BuildVorbisCommentBlockData(const ATrack: TRDJTrack): TBytes;
var
  MS: TMemoryStream;
  Vendor: UTF8String;
  Comments: TStringList;
  i: Integer;
  U: UTF8String;
  S: string;

begin

  MS := TMemoryStream.Create();
  Comments := TStringList.Create();

  try

    Vendor := UTF8String('RDJ Tag Writer');
    WriteUInt32LEToStream(MS,
                          Length(Vendor));

    if (Length(Vendor) > 0) then
      MS.WriteBuffer(Vendor[1],
                     Length(Vendor));

    AddVorbisPair(Comments,
                  'TITLE',
                  ATrack.Title);

    AddVorbisPair(Comments,
                  'ARTIST',
                  ATrack.Artist);

    AddVorbisPair(Comments,
                  'ALBUM',
                  ATrack.Album);

    AddVorbisPair(Comments,
                  'ALBUMARTIST',
                  ATrack.AlbumArtist);

    AddVorbisPair(Comments,
                  'GENRE',
                  ATrack.Genre);

    AddVorbisPair(Comments,
                  'COMPOSER',
                  ATrack.Composer);

    AddVorbisPair(Comments,
                  'COMMENT',
                  ATrack.Comment);


    if ATrack.Year <> 0 then
      AddVorbisPair(Comments,
                    'DATE',
                    IntToStr(ATrack.Year));

    if ATrack.TrackNumber <> 0 then
      AddVorbisPair(Comments,
                    'TRACKNUMBER',
                    IntToStr(ATrack.TrackNumber));

    if ATrack.DiscNumber <> 0 then
      AddVorbisPair(Comments,
                    'DISCNUMBER',
                    IntToStr(ATrack.DiscNumber));

    if ATrack.BPM <> 0 then
    begin
      S := StringReplace(FloatToStr(ATrack.BPM),
                         ',',
                         '.',
                         [rfReplaceAll]);

      AddVorbisPair(Comments,
                    'BPM',
                    S);
    end;

    AddVorbisPair(Comments,
                  'INITIALKEY',
                  ATrack.MusicalKey);

    if (ATrack.GainDb <> 0) then
      begin

        S := StringReplace(FormatFloat('0.00',
                           ATrack.GainDb),
                           ',',
                           '.',
                           [rfReplaceAll]) + ' dB';

        AddVorbisPair(Comments,
                      'REPLAYGAIN_TRACK_GAIN', S);
      end;

    WriteUInt32LEToStream(MS,
                          Comments.Count);

    for i := 0 to Comments.Count - 1 do
      begin

        U := UTF8String(Comments[i]);
        WriteUInt32LEToStream(MS,
                              Length(U));

        if (Length(U) > 0) then
          MS.WriteBuffer(U[1], Length(U));
      end;

    SetLength(Result,
              MS.Size);
    if (MS.Size > 0) then
      begin

        MS.Position := 0;
        MS.ReadBuffer(Result[0],
                      MS.Size);
      end;
  finally

    Comments.Free;
    MS.Free;
  end;
end;


function ReadFlacBlocks(const AFileName: string;
                        out ABlocks: TFlacMetaBlockArray;
                        out AAudioOffset: Int64;
                        out AError: string): Boolean;
var
  FS: TFileStream;
  Sig: array[0..3] of Byte;
  LastBlock: Boolean;
  BlockType: Byte;
  BlockSize: Cardinal;
  BlockIndex: Integer;

begin

  Result := False;
  AError := '';
  AAudioOffset := 0;
  SetLength(ABlocks, 0);

  FS := TFileStream.Create(AFileName,
                           fmOpenRead or fmShareDenyWrite);

  try

    if (FS.Size < 4) then
      begin

        AError := 'File too small.';
        Exit;
      end;

    FS.ReadBuffer(Sig,
                  4);
    if not ((Sig[0] = Ord('f')) and
           (Sig[1] = Ord('L')) and
           (Sig[2] = Ord('a')) and
           (Sig[3] = Ord('C'))) then
      begin

        AError := 'Not a FLAC file.';
        Exit;
      end;

    repeat

      FS.ReadBuffer(Sig,
                    4);

      LastBlock := (Sig[0] and $80) <> 0;
      BlockType := Sig[0] and $7F;
      BlockSize := ReadUInt24BE(@Sig[1]);

      BlockIndex := Length(ABlocks);
      SetLength(ABlocks, BlockIndex + 1);
      ABlocks[BlockIndex].BlockType := BlockType;
      SetLength(ABlocks[BlockIndex].Data,
                BlockSize);

      if (BlockSize > 0) then
        FS.ReadBuffer(ABlocks[BlockIndex].Data[0],
                      BlockSize);
    until LastBlock;

    AAudioOffset := FS.Position;
    Result := True;
  finally

    FS.Free;
  end;
end;


function WriteFlacTags(const AFileName: string;
                       const ATrack: TRDJTrack;
                       out AError: string): Boolean;
var
  Blocks: TFlacMetaBlockArray;
  AudioOffset: Int64;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  TempFileName: string;
  NewVorbis: TBytes;
  i: Integer;
  OutBlocks: TFlacMetaBlockArray;
  OutIndex: Integer;
  Header: array[0..3] of Byte;
  Buf: array[0..65535] of Byte;
  Remaining: Int64;
  ToRead: Integer;
  ReadCount: Integer;

begin

  Result := False;
  AError := '';

  if not ReadFlacBlocks(AFileName,
                        Blocks,
                        AudioOffset,
                        AError) then
    Exit;

  NewVorbis := BuildVorbisCommentBlockData(ATrack);

  SetLength(OutBlocks,
            0);

  for i := 0 to High(Blocks) do

    if (Blocks[i].BlockType <> 4) then
      begin

        OutIndex := Length(OutBlocks);
        SetLength(OutBlocks,
                  OutIndex + 1);
        OutBlocks[OutIndex] := Blocks[i];
      end;

  OutIndex := Length(OutBlocks);
  SetLength(OutBlocks,
            OutIndex + 1);
  OutBlocks[OutIndex].BlockType := 4;
  OutBlocks[OutIndex].Data := NewVorbis;

  TempFileName := AFileName + '.rdjtmp';
  InputStream := TFileStream.Create(AFileName,
                                    fmOpenRead or fmShareDenyWrite);

  try
    OutputStream := TFileStream.Create(TempFileName,
                                       fmCreate);
    try

      Header[0] := Ord('f');
      Header[1] := Ord('L');
      Header[2] := Ord('a');
      Header[3] := Ord('C');

      OutputStream.WriteBuffer(Header,
                               SizeOf(Header));

      for i := 0 to High(OutBlocks) do
        begin

          Header[0] := OutBlocks[i].BlockType and $7F;
          if (i = High(OutBlocks)) then
            Header[0] := Header[0] or $80;

          Header[1] := Byte((Length(OutBlocks[i].Data) shr 16) and $FF);
          Header[2] := Byte((Length(OutBlocks[i].Data) shr 8) and $FF);
          Header[3] := Byte(Length(OutBlocks[i].Data) and $FF);

          OutputStream.WriteBuffer(Header,
                                   SizeOf(Header));

          if (Length(OutBlocks[i].Data) > 0) then
            OutputStream.WriteBuffer(OutBlocks[i].Data[0],
                                     Length(OutBlocks[i].Data));
        end;

      InputStream.Position := AudioOffset;
      Remaining := InputStream.Size - AudioOffset;

      while (Remaining > 0) do
        begin

          if (Remaining > SizeOf(Buf)) then
            ToRead := SizeOf(Buf)
          else
            ToRead := Remaining;

          ReadCount := InputStream.Read(Buf,
                                        ToRead);
          if (ReadCount <= 0) then
            Break;

          OutputStream.WriteBuffer(Buf,
                                  ReadCount);
          Dec(Remaining,
              ReadCount);
        end;
    finally

      OutputStream.Free;
    end;
  finally

    InputStream.Free;
  end;

  if not ReplaceFile(PChar(AFileName),
                     PChar(TempFileName),
                     nil,
                     0,
                     nil,
                     nil) then
    begin

      AError := SysErrorMessage(GetLastError);
      if FileExists(TempFileName) then
        DeleteFile(TempFileName);
      Exit;
    end;

  Result := True;
end;


function ReadMp4AtomInfo(const AStream: TStream;
                         const AOffset: Int64;
                         out AInfo: TMP4AtomInfo): Boolean;
var
  Buf8: array[0..7] of Byte;
  SmallSize: Cardinal;
  LargeSize: UInt64;

begin

  Result := False;
  FillChar(AInfo,
           SizeOf(AInfo),
           0);

  if (AOffset < 0) or ((AOffset + 8) > AStream.Size) then
    Exit;

  AStream.Position := AOffset;
  AStream.ReadBuffer(Buf8, SizeOf(Buf8));

  SmallSize := ReadUInt32BE(@Buf8[0]);
  SetString(AInfo.AtomType,
            PAnsiChar(@Buf8[4]),
            4);

  if (SmallSize = 1) then
    begin

      if ((AOffset + 16) > AStream.Size) then
        Exit;

      AStream.ReadBuffer(Buf8,
                         SizeOf(Buf8));
      LargeSize := ReadUInt64BE(@Buf8[0]);
      if (LargeSize < 16) then
        Exit;

      AInfo.HeaderSize := 16;
      AInfo.AtomSize := LargeSize;
    end
  else
    if (SmallSize = 0) then
      begin
        AInfo.HeaderSize := 8;
        AInfo.AtomSize := AStream.Size - AOffset;
      end
    else
      begin
        if (SmallSize < 8) then
          Exit;

    AInfo.HeaderSize := 8;
    AInfo.AtomSize := SmallSize;
  end;

  if ((AOffset + AInfo.AtomSize) > AStream.Size) then
    Exit;

  AInfo.DataOffset := AOffset + AInfo.HeaderSize;
  AInfo.DataSize := AInfo.AtomSize - AInfo.HeaderSize;
  Result := True;
end;


function FindChildAtom(const AStream: TStream;
                       const AParent: TMP4AtomInfo;
                       const AAtomType: AnsiString;
                       out AChild: TMP4AtomInfo): Boolean;
var
  PosAtom: Int64;
  Cur: TMP4AtomInfo;

begin

  Result := False;
  PosAtom := AParent.DataOffset;

  while (PosAtom < (AParent.DataOffset + AParent.DataSize)) do
    begin

      if not ReadMp4AtomInfo(AStream,
                             PosAtom,
                             Cur) then
        Exit;

      if (Cur.AtomType = AAtomType) then
        begin
          AChild := Cur;
          Result := True;
          Exit;
        end;

      if (Cur.AtomSize <= 0) then
        Exit;
      Inc(PosAtom, Cur.AtomSize);
    end;
end;


procedure CopyBytesRange(const AInput: TStream;
                         const AOutput: TStream;
                         const AOffset,
                               ACount: Int64);
var
  Buf: array[0..65535] of Byte;
  Remaining: Int64;
  ReadNow: Integer;
  ReadCount: Integer;

begin

  if (ACount <= 0) then
    Exit;

  AInput.Position := AOffset;
  Remaining := ACount;

  while (Remaining > 0) do
    begin

      if (Remaining > SizeOf(Buf)) then
        ReadNow := SizeOf(Buf)
      else
        ReadNow := Remaining;

      ReadCount := AInput.Read(Buf,
                               ReadNow);
      if (ReadCount <= 0) then
        raise Exception.Create('Unexpected EOF while copying MP4 data.');

      AOutput.WriteBuffer(Buf,
                          ReadCount);
      Dec(Remaining,
          ReadCount);
  end;
end;


procedure AppendBytes(var ADest: TBytes;
                      const ASrc: TBytes);
var
  OldLen: Integer;

begin

  if (Length(ASrc) = 0) then
    Exit;

  OldLen := Length(ADest);
  SetLength(ADest,
            OldLen + Length(ASrc));
  Move(ASrc[0],
       ADest[OldLen],
       Length(ASrc));
end;


function MakeMp4DataAtomText(const AValue: string): TBytes;
var
  MS: TMemoryStream;
  U: UTF8String;

begin

  MS := TMemoryStream.Create();

  try

    WriteUInt32BEToStream(MS,
                          1);
    WriteUInt32BEToStream(MS,
                          0);

    U := UTF8String(AValue);

    if (Length(U) > 0) then
      MS.WriteBuffer(U[1],
                     Length(U));
    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function MakeMp4DataAtomInteger16(const AValue: Word): TBytes;
var
  MS: TMemoryStream;

begin

  MS := TMemoryStream.Create();

  try

    WriteUInt32BEToStream(MS,
                          $00000015);

    WriteUInt32BEToStream(MS,
                          0);

    WriteUInt16BEToStream(MS,
                          AValue);

    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function MakeMp4DataAtomTrackDisk(const AIndex: Word): TBytes;
var
  MS: TMemoryStream;

begin

  MS := TMemoryStream.Create();

  try

    WriteUInt32BEToStream(MS,
                          0);
    WriteUInt32BEToStream(MS,
                          0);
    WriteUInt16BEToStream(MS,
                          0);
    WriteUInt16BEToStream(MS,
                          AIndex);
    WriteUInt16BEToStream(MS,
                          0);
    WriteUInt16BEToStream(MS,
                          0);
    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function MakeMp4ItemAtom(const AAtomType: AnsiString;
                         const ADataAtomData: TBytes): TBytes;
var
  MS: TMemoryStream;

begin

  MS := TMemoryStream.Create;

  try

    WriteAtomHeader(MS,
                    AAtomType,
                    8 + 8 + Length(ADataAtomData));
    WriteAtomToStream(MS,
                      'data',
                      ADataAtomData);
    Result := StreamToBytes(MS);
  finally

    MS.Free();
  end;
end;


function MakeMp4FreeFormAtom(const AMean,
                                   AName,
                                   AValue: string): TBytes;
var
  MS: TMemoryStream;
  MeanData: TBytes;
  NameData: TBytes;
  ValueData: TBytes;

begin

  MS := TMemoryStream.Create();

  try

    MeanData := MakeMp4DataAtomText(AMean);
    NameData := MakeMp4DataAtomText(AName);
    ValueData := MakeMp4DataAtomText(AValue);

    WriteAtomHeader(MS,
                    '----',
                    8 + (8 + Length(MeanData)) + (8 + Length(NameData)) + (8 + Length(ValueData)));
    WriteAtomToStream(MS,
                      'mean',
                      MeanData);
    WriteAtomToStream(MS,
                      'name',
                      NameData);
    WriteAtomToStream(MS,
                      'data',
                      ValueData);

    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function BuildMp4IlstData(const ATrack: TRDJTrack): TBytes;
var
  MS: TMemoryStream;
  Item: TBytes;
  S: string;
  BPMInt: Integer;

begin

  MS := TMemoryStream.Create;

  try

    if (Trim(ATrack.Title) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'nam',
                                MakeMp4DataAtomText(ATrack.Title));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.Artist) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'ART',
                                MakeMp4DataAtomText(ATrack.Artist));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.Album) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'alb',
                                MakeMp4DataAtomText(ATrack.Album));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.AlbumArtist) <> '') then
      begin

        Item := MakeMp4ItemAtom('aART',
                                MakeMp4DataAtomText(ATrack.AlbumArtist));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.Genre) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'gen',
                                MakeMp4DataAtomText(ATrack.Genre));
      MS.WriteBuffer(Item[0],
                     Length(Item));
      end;

    if (Trim(ATrack.Composer) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'wrt',
                                MakeMp4DataAtomText(ATrack.Composer));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.Comment) <> '') then
      begin

        Item := MakeMp4ItemAtom(#$A9'cmt',
                                MakeMp4DataAtomText(ATrack.Comment));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (ATrack.Year <> 0) then
      begin

        Item := MakeMp4ItemAtom(#$A9'day',
                                MakeMp4DataAtomText(IntToStr(ATrack.Year)));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (ATrack.TrackNumber <> 0) then
      begin

        Item := MakeMp4ItemAtom('trkn',
                                MakeMp4DataAtomTrackDisk(ATrack.TrackNumber));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if ATrack.DiscNumber <> 0 then
      begin

        Item := MakeMp4ItemAtom('disk',
                                MakeMp4DataAtomTrackDisk(ATrack.DiscNumber));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (ATrack.BPM <> 0) then
      begin

        BPMInt := Round(ATrack.BPM);

        if (BPMInt < 0) then
          BPMInt := 0;

        if (BPMInt > High(Word)) then
          BPMInt := High(Word);

        Item := MakeMp4ItemAtom('tmpo',
                                MakeMp4DataAtomInteger16(BPMInt));
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (Trim(ATrack.MusicalKey) <> '') then
      begin

        Item := MakeMp4FreeFormAtom('com.apple.iTunes',
                                    'INITIALKEY',
                                    ATrack.MusicalKey);
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    if (ATrack.GainDb <> 0) then
      begin

        S := StringReplace(FormatFloat('0.00',
                                       ATrack.GainDb),
                                       ',',
                                       '.',
                                       [rfReplaceAll]) + ' dB';

        Item := MakeMp4FreeFormAtom('com.apple.iTunes',
                                    'REPLAYGAIN_TRACK_GAIN', S);
        MS.WriteBuffer(Item[0],
                       Length(Item));
      end;

    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function BuildMp4MetaData(const ATrack: TRDJTrack;
                          const AInput: TStream;

                          const AMetaAtom: TMP4AtomInfo;

                          out AError: string): TBytes;

var

  MetaBody: TMemoryStream;

  CurPos: Int64;

  Child: TMP4AtomInfo;

  IlstData: TBytes;



begin



  Result := nil;

  AError := '';



  MetaBody := TMemoryStream.Create();



  try



    AInput.Position := AMetaAtom.DataOffset;

    CopyBytesRange(AInput,

                   MetaBody,

                   AMetaAtom.DataOffset,

                   4);



    CurPos := AMetaAtom.DataOffset + 4;



    while (CurPos < (AMetaAtom.DataOffset + AMetaAtom.DataSize)) do

      begin



        if not ReadMp4AtomInfo(AInput,

                               CurPos,

                               Child) then

          begin



            AError := 'Invalid MP4 meta child atom.';

            Exit;

          end;



        if (Child.AtomType <> 'ilst') then

          CopyBytesRange(AInput,

                         MetaBody,

                         Child.DataOffset - Child.HeaderSize,

                         Child.AtomSize);



        Inc(CurPos,

            Child.AtomSize);

      end;



    IlstData := BuildMp4IlstData(ATrack);

    WriteAtomToStream(MetaBody,

                      'ilst',

                      IlstData);



    Result := StreamToBytes(MetaBody);

  finally



    MetaBody.Free;

  end;

end;


function BuildMp4MetaAtom(const ATrack: TRDJTrack;
                          const AInput: TStream;
                          const AMetaAtom: TMP4AtomInfo;
                          out AError: string): TBytes;
var
  Data: TBytes;
  MS: TMemoryStream;

begin

  Result := nil;
  AError := '';

  Data := BuildMp4MetaData(ATrack,
                           AInput,
                           AMetaAtom,
                           AError);

  if (AError <> '') then
    Exit;

  MS := TMemoryStream.Create();

  try

    WriteAtomHeader(MS,
                    'meta',
                    8 + Length(Data));

    if (Length(Data) > 0) then
      MS.WriteBuffer(Data[0],
                     Length(Data));
    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function BuildMp4MetaAtomNew(const ATrack: TRDJTrack): TBytes;
var
  MS: TMemoryStream;
  MetaBody: TMemoryStream;
  Handler: TMemoryStream;
  HandlerData: TBytes;

begin

  Handler := TMemoryStream.Create();
  MetaBody := TMemoryStream.Create();
  MS := TMemoryStream.Create();

  try

    WriteUInt32BEToStream(MetaBody,
                          0);

    WriteUInt32BEToStream(Handler,
                          0);

    WriteUInt32BEToStream(Handler,
                          0);

    Handler.WriteBuffer('mdir',
                        4);
    Handler.WriteBuffer('appl',
                        4);

    WriteUInt32BEToStream(Handler,
                          0);
    WriteUInt32BEToStream(Handler,
                          0);
    WriteUInt32BEToStream(Handler,
                          0);

    HandlerData := StreamToBytes(Handler);
    WriteAtomToStream(MetaBody,
                      'hdlr',
                      HandlerData);

    WriteAtomToStream(MetaBody,
                      'ilst',
                      BuildMp4IlstData(ATrack));

    WriteAtomHeader(MS,
                    'meta',
                    8 + MetaBody.Size);

    MetaBody.Position := 0;
    MS.CopyFrom(MetaBody,
                0);
    Result := StreamToBytes(MS);
  finally

    MS.Free;
    MetaBody.Free;
    Handler.Free;
  end;
end;

function BuildMp4UdtaAtom(const ATrack: TRDJTrack;
                          const AInput: TStream;
                          const AUdtaAtom: TMP4AtomInfo;
                          out AError: string): TBytes;
var
  MS: TMemoryStream;
  CurPos: Int64;
  Child: TMP4AtomInfo;
  MetaAtom: TBytes;
  MetaDone: Boolean;

begin

  Result := nil;
  AError := '';
  MetaDone := False;

  MS := TMemoryStream.Create();

  try

    CurPos := AUdtaAtom.DataOffset;

    while (CurPos < (AUdtaAtom.DataOffset + AUdtaAtom.DataSize)) do
      begin

        if not ReadMp4AtomInfo(AInput,
                               CurPos,
                               Child) then
          begin

            AError := 'Invalid MP4 udta child atom.';
            Exit;
          end;

         if (Child.AtomType = 'meta') then
           begin

             MetaAtom := BuildMp4MetaAtom(ATrack,
                                          AInput,
                                          Child,
                                          AError);
             if (AError <> '') then
               Exit;

             if (Length(MetaAtom) > 0) then
               MS.WriteBuffer(MetaAtom[0],
                              Length(MetaAtom));
             MetaDone := True;
           end
         else
           CopyBytesRange(AInput,
                          MS,
                          CurPos,
                          Child.AtomSize);

         Inc(CurPos,
             Child.AtomSize);
      end;

    if not MetaDone then
      begin

        MetaAtom := BuildMp4MetaAtomNew(ATrack);
        if (Length(MetaAtom) > 0) then
          MS.WriteBuffer(MetaAtom[0],
                         Length(MetaAtom));
      end;

    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;


function BuildMp4MoovData(const ATrack: TRDJTrack;
                          const AInput: TStream;
                          const AMoovAtom: TMP4AtomInfo;
                          out AError: string): TBytes;
var
  MS: TMemoryStream;
  CurPos: Int64;
  Child: TMP4AtomInfo;
  UdtaData: TBytes;
  UdtaDone: Boolean;

begin

  Result := nil;
  AError := '';
  UdtaDone := False;

  MS := TMemoryStream.Create();

  try

    CurPos := AMoovAtom.DataOffset;

    while (CurPos < (AMoovAtom.DataOffset + AMoovAtom.DataSize)) do
      begin

        if not ReadMp4AtomInfo(AInput,
                               CurPos,
                               Child) then
          begin

            AError := 'Invalid MP4 moov child atom.';
            Exit;
          end;

        if (Child.AtomType = 'udta') then
          begin

            UdtaData := BuildMp4UdtaAtom(ATrack,
                                         AInput,
                                         Child,
                                         AError);
            if (AError <> '') then
              Exit;
            WriteAtomToStream(MS,
                              'udta',
                              UdtaData);
            UdtaDone := True;
          end
        else
          CopyBytesRange(AInput,
                         MS,
                         CurPos,
                         Child.AtomSize);

        Inc(CurPos,
            Child.AtomSize);
      end;

    if not UdtaDone then
      begin

        UdtaData := BuildMp4MetaAtomNew(ATrack);
        WriteAtomToStream(MS,
                          'udta',
                          UdtaData);
      end;

    Result := StreamToBytes(MS);
  finally

    MS.Free;
  end;
end;

procedure AdjustChunkOffsetsInBytes(var AData: TBytes;
                                    const ADelta: Int64;
                                    out AError: string);
var
  MS: TMemoryStream;
  MoovInfo: TMP4AtomInfo;
  CurPos: Int64;
  Child: TMP4AtomInfo;
  EntryCount: Cardinal;
  i: Cardinal;
  Offset32: Cardinal;
  Offset64: UInt64;
  P: PByte;

begin

  AError := '';
  if (ADelta = 0) or (Length(AData) = 0) then
    Exit;

  MS := TMemoryStream.Create();

  try

    MS.WriteBuffer(AData[0],
                   Length(AData));

    if not ReadMp4AtomInfo(MS,
                           0,
                           MoovInfo) then
      begin

        AError := 'Invalid rebuilt moov atom.';
        Exit;
      end;

    CurPos := MoovInfo.DataOffset;

    while (CurPos < (MoovInfo.DataOffset + MoovInfo.DataSize)) do
      begin

        if not ReadMp4AtomInfo(MS, CurPos, Child) then
          begin

            AError := 'Invalid moov child while adjusting chunk offsets.';
            Exit;
          end;

        if (Child.AtomType = 'stco') and (Child.DataSize >= 8) then
          begin
            MS.Position := Child.DataOffset + 4;
            MS.ReadBuffer(EntryCount,
                          4);
            EntryCount := ReadUInt32BE(@EntryCount);

            if (Child.DataSize < (8 + (Int64(EntryCount) * 4))) then
              begin

                AError := 'Invalid stco atom size.';
                Exit;
              end;

            for i := 0 to EntryCount - 1 do
              begin

                MS.Position := Child.DataOffset + 8 + (Int64(i) * 4);
                MS.ReadBuffer(Offset32,
                              4);
                Offset32 := ReadUInt32BE(@Offset32);

                if (ADelta < 0) then
                  Dec(Offset32,
                      Cardinal(-ADelta))
                else
                  inc(Offset32,
                      Cardinal(ADelta));

                MS.Position := Child.DataOffset + 8 + (Int64(i) * 4);
                WriteUInt32BEToStream(MS,
                                      Offset32);
              end;
          end
        else
          if (Child.AtomType = 'co64') and (Child.DataSize >= 8) then
            begin

              MS.Position := Child.DataOffset + 4;
              MS.ReadBuffer(EntryCount,
                            4);
              EntryCount := ReadUInt32BE(@EntryCount);

              if (Child.DataSize < (8 + (Int64(EntryCount) * 8))) then
                begin

                  AError := 'Invalid co64 atom size.';
                  Exit;
                end;

              for i := 0 to EntryCount - 1 do
                begin

                  MS.Position := Child.DataOffset + 8 + (Int64(i) * 8);
                  MS.ReadBuffer(Offset64,
                                8);
                  Offset64 := ReadUInt64BE(@Offset64);

                  if (ADelta < 0) then
                    Dec(Offset64, UInt64(-ADelta))
                  else
                    Inc(Offset64, UInt64(ADelta));

                  MS.Position := Child.DataOffset + 8 + (Int64(i) * 8);
                  WriteUInt64BEToStream(MS,
                                        Offset64);
                end;
              end;

        Inc(CurPos,
            Child.AtomSize);
      end;

    MS.Position := 0;

    if (Length(AData) > 0) then
      begin

        P := MS.Memory;
        Move(P^,
             AData[0],
             Length(AData));
      end;
  finally

    MS.Free;
  end;
end;


function WriteMp4Tags(const AFileName: string;
                      const ATrack: TRDJTrack;
                      out AError: string): Boolean;
var
  InputStream: TFileStream;
  OutputStream: TFileStream;
  TempFileName: string;
  Atom: TMP4AtomInfo;
  MoovAtom: TMP4AtomInfo;
  MdatAtom: TMP4AtomInfo;
  MoovData: TBytes;
  NewMoov: TBytes;
  Delta: Int64;
  PosAtom: Int64;
  HaveMoov: Boolean;
  HaveMdat: Boolean;
  MS: TMemoryStream;

begin

  Result := False;
  AError := '';
  HaveMoov := False;
  HaveMdat := False;

  InputStream := TFileStream.Create(AFileName,
                                    fmOpenRead or fmShareDenyWrite);

  try

    PosAtom := 0;

    while (PosAtom < InputStream.Size) do
      begin

        if not ReadMp4AtomInfo(InputStream,
                               PosAtom,
                               Atom) then
          begin
            AError := 'Invalid MP4 atom structure.';
            Exit;
          end;

        if (Atom.AtomType = 'moov') then
          begin

            MoovAtom := Atom;
            HaveMoov := True;
          end
        else
          if (Atom.AtomType = 'mdat') then
            begin

              MdatAtom := Atom;
              HaveMdat := True;
            end;

        Inc(PosAtom,
            Atom.AtomSize);
      end;

    if not HaveMoov then
      begin
        AError := 'MP4 moov atom not found.';
        Exit;
      end;

    MoovData := BuildMp4MoovData(ATrack,
                                 InputStream,
                                 MoovAtom,
                                 AError);
    if (AError <> '') then
      Exit;

    MS := TMemoryStream.Create();

    try

      WriteAtomHeader(MS,
                      'moov',
                      8 + Length(MoovData));

      if (Length(MoovData) > 0) then
        MS.WriteBuffer(MoovData[0],
                       Length(MoovData));
      NewMoov := StreamToBytes(MS);
    finally

      MS.Free;
    end;

    Delta := Length(NewMoov) - MoovAtom.AtomSize;

    if HaveMdat and (MoovAtom.DataOffset - MoovAtom.HeaderSize < MdatAtom.DataOffset - MdatAtom.HeaderSize) and (Delta <> 0) then
      begin

        AdjustChunkOffsetsInBytes(NewMoov,
                                  Delta,
                                  AError);
        if (AError <> '') then
          Exit;
      end;
  finally

    InputStream.Free;
  end;

  TempFileName := AFileName + '.rdjtmp';
  InputStream := TFileStream.Create(AFileName,
                                    fmOpenRead or fmShareDenyWrite);

  try

    OutputStream := TFileStream.Create(TempFileName,
                                       fmCreate);

    try

      PosAtom := 0;

      while (PosAtom < InputStream.Size) do
        begin

          if not ReadMp4AtomInfo(InputStream,
                                 PosAtom,
                                 Atom) then
            begin

              AError := 'Invalid MP4 atom while writing.';
              Exit;
            end;

          if (Atom.AtomType = 'moov') then
            begin

              if (Length(NewMoov) > 0) then
              OutputStream.WriteBuffer(NewMoov[0],
                                       Length(NewMoov));
            end
          else
            CopyBytesRange(InputStream,
                           OutputStream,
                           PosAtom,
                           Atom.AtomSize);

          Inc(PosAtom,
              Atom.AtomSize);
        end;
    finally

      OutputStream.Free;
    end;

  finally

    InputStream.Free;
  end;

  if not ReplaceFile(PChar(AFileName),
                     PChar(TempFileName),
                     nil,
                     0,
                     nil,
                     nil) then
    begin

      AError := SysErrorMessage(GetLastError);
      if FileExists(TempFileName) then
        DeleteFile(TempFileName);
      Exit;
    end;

  Result := True;
end;


function RDJWriteTrackTags(const AFileName: string;
                           const ATrack: TRDJTrack;
                           out AError: string): Boolean;
var
  Ext: string;

begin

  Result := False;
  AError := '';

  if (Trim(AFileName) = '') then
    begin

      AError := 'Empty file name.';
      Exit;
    end;

  if not FileExists(AFileName) then
    begin

      AError := 'File not found.';
      Exit;
    end;

  Ext := LowerCase(ExtractFileExt(AFileName));

  if (Ext = '.mp3') then
    Result := WriteMp3Tags(AFileName,
                           ATrack,
                           AError)
  else
    if Ext = '.flac' then
      Result := WriteFlacTags(AFileName,
                              ATrack,
                              AError)
    else
      if (Ext = '.m4a') or
          (Ext = '.m4b') or
          (Ext = '.mp4') then
        Result := WriteMp4Tags(AFileName,
                               ATrack,
                               AError)
      else
        begin
          AError := 'Tag writing is currently supported for MP3, FLAC and MP4/M4A.';
          Exit;
        end;
end;

end.
