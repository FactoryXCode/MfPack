// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfMatroskaSubtitleReader.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Minimal read-only Matroska/EBML adapter for embedded textual
//              subtitle tracks. This unit is used only when the Windows Media
//              Foundation Matroska source does not expose caption streams.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
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
// Source: https://www.matroska.org,
//         https://www.matroska.org/downloads/libraries.html?utm_source=chatgpt.com
//         https://gitlab.com/mbunkus/mkvtoolnix
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
unit MfMatroskaSubtitleReader;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

type
  TMfMatroskaSubtitleFormat = (msfUnknown,
                               msfSrt,
                               msfSsaAss,
                               msfVobSub,
                               msfPgs);

  TMfMatroskaSubtitleTrack = record
    TrackNumber: UInt64;
    CodecId: string;
    Language: string;
    Name: string;
    Format: TMfMatroskaSubtitleFormat;
    DefaultTrack: Boolean;
    ForcedTrack: Boolean;
    Supported: Boolean;
    DefaultDurationNs: UInt64;
    TrackTimestampScale: Double;

    procedure Reset();
  end;

  TMfMatroskaSubtitleTrackArray = array of TMfMatroskaSubtitleTrack;

  TMfMatroskaSubtitleCue = record
    StartMs: Int64;
    StopMs: Int64;
    Text: string;
  end;

  TMfMatroskaSubtitleCueArray = array of TMfMatroskaSubtitleCue;

  TMfMatroskaSubtitleReader = class(TObject)

  public

    class function EnumerateTracks(const FileName: WideString;
                                   out Tracks: TMfMatroskaSubtitleTrackArray): HRESULT; static;

    class function ReadTrack(const FileName: WideString;
                             const TrackNumber: UInt64;
                             out Track: TMfMatroskaSubtitleTrack;
                             out Cues: TMfMatroskaSubtitleCueArray;
                             const CancelEvent: THandle = 0): HRESULT; static;
    class function ReadTrackWindow(const FileName: WideString;
                                   const TrackNumber: UInt64;
                                   const StartMs: Int64;
                                   const EndMs: Int64;
                                   out Track: TMfMatroskaSubtitleTrack;
                                   out Cues: TMfMatroskaSubtitleCueArray;
                                   out EndOfTrack: Boolean;
                                   const CancelEvent: THandle = 0): HRESULT; static;
  end;

implementation

const
  // EBML / Matroska top-level elements.
  EBML_ID_EBML                 = UInt64($1A45DFA3);
  EBML_ID_SEGMENT              = UInt64($18538067);
  EBML_ID_INFO                 = UInt64($1549A966);
  EBML_ID_TRACKS               = UInt64($1654AE6B);
  EBML_ID_CLUSTER              = UInt64($1F43B675);

  // Segment Info.
  EBML_ID_TIMESTAMP_SCALE      = UInt64($2AD7B1);

  // Track elements.
  EBML_ID_TRACK_ENTRY          = UInt64($AE);
  EBML_ID_TRACK_NUMBER         = UInt64($D7);
  EBML_ID_TRACK_TYPE           = UInt64($83);
  EBML_ID_FLAG_DEFAULT         = UInt64($88);
  EBML_ID_FLAG_FORCED          = UInt64($55AA);
  EBML_ID_DEFAULT_DURATION     = UInt64($23E383);
  EBML_ID_TRACK_TS_SCALE       = UInt64($23314F);
  EBML_ID_NAME                 = UInt64($536E);
  EBML_ID_LANGUAGE             = UInt64($22B59C);
  EBML_ID_LANGUAGE_BCP47       = UInt64($22B59D);
  EBML_ID_CODEC_ID             = UInt64($86);

  // Cluster elements.
  EBML_ID_CLUSTER_TIMESTAMP    = UInt64($E7);
  EBML_ID_SIMPLE_BLOCK         = UInt64($A3);
  EBML_ID_BLOCK_GROUP          = UInt64($A0);
  EBML_ID_BLOCK                = UInt64($A1);
  EBML_ID_BLOCK_DURATION       = UInt64($9B);

  MATROSKA_TRACK_TYPE_SUBTITLE = UInt64(17);
  MATROSKA_DEFAULT_TS_SCALE_NS = UInt64(1000000);
  MATROSKA_DEFAULT_CUE_MS      = Int64(2000);
  MATROSKA_WINDOW_CLUSTER_OVERLAP_MS = Int64(30000);
  MATROSKA_MAX_TEXT_FRAME_SIZE = UInt64(16 * 1024 * 1024);
  MATROSKA_MAX_STRING_SIZE     = UInt64(1024 * 1024);

type
  TMfMatroskaSubtitleTrackData = record
    Track: TMfMatroskaSubtitleTrack;
    Cues: TMfMatroskaSubtitleCueArray;
  end;

  TMfMatroskaSubtitleTrackDataArray = array of TMfMatroskaSubtitleTrackData;

  TMatroskaSubtitleCache = record
    FileName: WideString;
    FileSize: Int64;
    LastWriteTime: TFileTime;
    TrackData: TMfMatroskaSubtitleTrackDataArray;
    Valid: Boolean;
  end;

  TEbmlElementHeader = record
    ElementId: UInt64;
    DataSize: UInt64;
    DataStart: Int64;
    DataEnd: Int64;
    UnknownSize: Boolean;
  end;

  TMfMatroskaParser = class(TObject)
  private
    FStream: TFileStream;
    FFileSize: Int64;
    FSegmentStart: Int64;
    FSegmentEnd: Int64;
    FTimestampScaleNs: UInt64;
    FTracks: TMfMatroskaSubtitleTrackArray;
    FCancelEvent: THandle;

    function Cancelled(): Boolean;
    function ReadByte(out Value: Byte): Boolean;
    function ReadVInt(out Value: UInt64;
                      out ByteCount: Integer;
                      const StripMarker: Boolean;
                      out UnknownValue: Boolean): Boolean;
    function ReadElementHeader(const ParentEnd: Int64;
                               out Header: TEbmlElementHeader): Boolean;
    function SeekAbsolute(const Position: Int64): Boolean;
    function SkipTo(const Position: Int64): Boolean;
    function ReadUnsigned(const Size: UInt64;
                          out Value: UInt64): Boolean;
    function ReadFloat(const Size: UInt64;
                       out Value: Double): Boolean;
    function ReadBytes(const Size: UInt64;
                       out Data: TBytes): Boolean;
    function ReadString(const Size: UInt64;
                        out Value: string): Boolean;

    function LocateSegment(): Boolean;
    function ParseInfo(const StartPos: Int64;
                       const EndPos: Int64): Boolean;
    function ParseTracks(const StartPos: Int64;
                         const EndPos: Int64): Boolean;
    function ParseTrackEntry(const StartPos: Int64;
                             const EndPos: Int64;
                             out Track: TMfMatroskaSubtitleTrack;
                             out IsSubtitle: Boolean): Boolean;
    function ParseMetadata(): Boolean;

    function FindClusterTimestamp(const StartPos: Int64;
                                  const EndPos: Int64;
                                  out Timestamp: UInt64): Boolean;

    function FindTrackDataIndex(const TrackNumber: UInt64;
                                const TrackData: TMfMatroskaSubtitleTrackDataArray): Integer;
    function ParseBlockAll(const StartPos: Int64;
                           const Size: UInt64;
                           const ClusterTimestamp: UInt64;
                           const BlockDurationTicks: UInt64;
                           const HasBlockDuration: Boolean;
                           var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;
    function ParseBlockGroupAll(const StartPos: Int64;
                                const EndPos: Int64;
                                const ClusterTimestamp: UInt64;
                                var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;
    function ParseClusterAll(const StartPos: Int64;
                             const EndPos: Int64;
                             const ClusterTimestamp: UInt64;
                             var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;

    function ReadLacedFrames(const DataEnd: Int64;
                             const LacingMode: Integer;
                             var Frames: array of TBytes;
                             out FrameCount: Integer): Boolean;
    function DecodeText(const Data: TBytes): string;
    function TicksToMs(const Ticks: Int64;
                       const TrackScale: Double): Int64;
    function DurationTicksToMs(const Ticks: UInt64;
                               const TrackScale: Double): Int64;
    procedure AppendCue(var Cues: TMfMatroskaSubtitleCueArray;
                        const StartMs: Int64;
                        const StopMs: Int64;
                        const Text: string);
    procedure SortAndNormalizeCues(var Cues: TMfMatroskaSubtitleCueArray);
    procedure QuickSortCues(var Cues: TMfMatroskaSubtitleCueArray;
                            L: Integer;
                            R: Integer);

    class function ClassifyCodecId(const CodecId: string): TMfMatroskaSubtitleFormat; static;
    class function NormalizeLanguageCode(const Language: string): string; static;

  public

    constructor Create(const FileName: WideString;
                       const CancelEvent: THandle = 0);
    destructor Destroy(); override;

    function Enumerate(out Tracks: TMfMatroskaSubtitleTrackArray): HRESULT;

    function ReadAllSubtitleTracks(out TrackData: TMfMatroskaSubtitleTrackDataArray): HRESULT;

    function ReadSubtitleTrackWindow(const TrackNumber: UInt64;
                                     const StartMs: Int64;
                                     const EndMs: Int64;
                                     out Track: TMfMatroskaSubtitleTrack;
                                     out Cues: TMfMatroskaSubtitleCueArray;
                                     out EndOfTrack: Boolean): HRESULT;
  end;


var
  GMatroskaSubtitleCacheLock: TCriticalSection;
  GMatroskaSubtitleCache: TMatroskaSubtitleCache;


function GetMatroskaFileIdentity(const FileName: WideString;
                                 out FileSize: Int64;
                                 out LastWriteTime: TFileTime): Boolean;
var
  FileData: WIN32_FILE_ATTRIBUTE_DATA;

begin

  FileSize := 0;
  LastWriteTime.dwLowDateTime := 0;
  LastWriteTime.dwHighDateTime := 0;

  FillChar(FileData,
           SizeOf(FileData),
           0);

  Result := GetFileAttributesExW(PWideChar(FileName),
                                 GetFileExInfoStandard,
                                 @FileData);
  if not Result then
    Exit;

  FileSize := (Int64(FileData.nFileSizeHigh) shl 32) or
              Int64(FileData.nFileSizeLow);
  LastWriteTime := FileData.ftLastWriteTime;
end;


function MatroskaCacheMatches(const FileName: WideString;
                              const FileSize: Int64;
                              const LastWriteTime: TFileTime): Boolean;
begin

  Result := GMatroskaSubtitleCache.Valid and
            SameText(GMatroskaSubtitleCache.FileName,
                     FileName) and
            (GMatroskaSubtitleCache.FileSize = FileSize) and
            (GMatroskaSubtitleCache.LastWriteTime.dwLowDateTime = LastWriteTime.dwLowDateTime) and
            (GMatroskaSubtitleCache.LastWriteTime.dwHighDateTime = LastWriteTime.dwHighDateTime);
end;


procedure CopyMatroskaCues(const Source: TMfMatroskaSubtitleCueArray;
                           out Destination: TMfMatroskaSubtitleCueArray);
var
  I: Integer;

begin

  SetLength(Destination,
            Length(Source));

  for I := Low(Source) to High(Source) do
    Destination[I] := Source[I];
end;


procedure TMfMatroskaSubtitleTrack.Reset();
begin

  TrackNumber := 0;
  CodecId := '';
  Language := '';
  Name := '';
  Format := msfUnknown;
  DefaultTrack := True;
  ForcedTrack := False;
  Supported := False;
  DefaultDurationNs := 0;
  TrackTimestampScale := 1.0;
end;


constructor TMfMatroskaParser.Create(const FileName: WideString;
                                     const CancelEvent: THandle);
begin

  inherited Create();

  FStream := TFileStream.Create(FileName,
                                fmOpenRead or fmShareDenyNone);
  FFileSize := FStream.Size;
  FSegmentStart := 0;
  FSegmentEnd := FFileSize;
  FTimestampScaleNs := MATROSKA_DEFAULT_TS_SCALE_NS;
  FCancelEvent := CancelEvent;
  SetLength(FTracks, 0);
end;


function TMfMatroskaParser.Cancelled(): Boolean;
begin

  Result := (FCancelEvent <> 0) and
            (WaitForSingleObject(FCancelEvent,
                                 0) = WAIT_OBJECT_0);
end;


destructor TMfMatroskaParser.Destroy();
begin

  FreeAndNil(FStream);

  inherited Destroy();
end;


function TMfMatroskaParser.ReadByte(out Value: Byte): Boolean;
begin

  Value := 0;
  Result := Assigned(FStream) and
            (FStream.Position < FFileSize) and
            (FStream.Read(Value, SizeOf(Value)) = SizeOf(Value));
end;


function TMfMatroskaParser.ReadVInt(out Value: UInt64;
                                    out ByteCount: Integer;
                                    const StripMarker: Boolean;
                                    out UnknownValue: Boolean): Boolean;
var
  FirstByte: Byte;
  NextByte: Byte;
  Marker: Byte;
  I: Integer;
  PayloadBits: Integer;
  MaxValue: UInt64;

begin

  Value := 0;
  ByteCount := 0;
  UnknownValue := False;
  Result := False;

  if not ReadByte(FirstByte) or (FirstByte = 0) then
    Exit;

  Marker := $80;
  ByteCount := 1;

  while (ByteCount <= 8) and ((FirstByte and Marker) = 0) do
    begin
      Marker := Marker shr 1;
      Inc(ByteCount);
    end;

  if (ByteCount > 8) or (Marker = 0) then
    Exit;

  if StripMarker then
    Value := UInt64(FirstByte and (Marker - 1))
  else
    Value := UInt64(FirstByte);

  for I := 2 to ByteCount do
    begin
      if not ReadByte(NextByte) then
        Exit;
      Value := (Value shl 8) or UInt64(NextByte);
    end;

  if StripMarker then
    begin
      PayloadBits := 7 * ByteCount;
      MaxValue := (UInt64(1) shl PayloadBits) - 1;
      UnknownValue := Value = MaxValue;
    end;

  Result := True;
end;


function TMfMatroskaParser.ReadElementHeader(const ParentEnd: Int64;
                                             out Header: TEbmlElementHeader): Boolean;
var
  IdLength: Integer;
  SizeLength: Integer;
  DummyUnknown: Boolean;
  EndPosition: UInt64;

begin

  Header.ElementId := 0;
  Header.DataSize := 0;
  Header.DataStart := 0;
  Header.DataEnd := 0;
  Header.UnknownSize := False;
  Result := False;

  if not Assigned(FStream) or
     (FStream.Position < 0) or
     (FStream.Position >= ParentEnd) then
    Exit;

  if not ReadVInt(Header.ElementId,
                  IdLength,
                  False,
                  DummyUnknown) then
    Exit;

  if not ReadVInt(Header.DataSize,
                  SizeLength,
                  True,
                  Header.UnknownSize) then
    Exit;

  Header.DataStart := FStream.Position;

  if Header.UnknownSize then
    Header.DataEnd := ParentEnd
  else
    begin
      EndPosition := UInt64(Header.DataStart) + Header.DataSize;

      if (EndPosition > UInt64(ParentEnd)) or
         (EndPosition > UInt64(FFileSize)) then
        Exit;

      Header.DataEnd := Int64(EndPosition);
    end;

  Result := Header.DataEnd >= Header.DataStart;
end;


function TMfMatroskaParser.SeekAbsolute(const Position: Int64): Boolean;
begin

  Result := Assigned(FStream) and
            (Position >= 0) and
            (Position <= FFileSize);

  if Result then
    FStream.Seek(Position,
                 soBeginning);
end;


function TMfMatroskaParser.SkipTo(const Position: Int64): Boolean;
begin

  Result := SeekAbsolute(Position);
end;


function TMfMatroskaParser.ReadUnsigned(const Size: UInt64;
                                        out Value: UInt64): Boolean;
var
  I: UInt64;
  B: Byte;

begin

  Value := 0;
  Result := False;

  if (Size = 0) or (Size > 8) then
    Exit;

  I := 0;
  while (I < Size) do
    begin
      if not ReadByte(B) then
        Exit;
      Value := (Value shl 8) or UInt64(B);
      Inc(I);
    end;

  Result := True;
end;


function TMfMatroskaParser.ReadFloat(const Size: UInt64;
                                     out Value: Double): Boolean;
var
  Source: array[0..7] of Byte;
  Target: array[0..7] of Byte;
  I: Integer;
  SingleValue: Single;

begin

  Value := 0.0;
  Result := False;

  FillChar(Source,
           SizeOf(Source),
           0);

  FillChar(Target,
           SizeOf(Target),
           0);

  if (Size <> 4) and (Size <> 8) then
    Exit;

  if FStream.Read(Source[0],
                  Integer(Size)) <> Integer(Size) then
    Exit;

  for I := 0 to Integer(Size) - 1 do
    Target[I] := Source[Integer(Size) - 1 - I];

  if (Size = 4) then
    begin
      Move(Target[0],
           SingleValue,
           SizeOf(SingleValue));

      Value := SingleValue;
    end
  else
    Move(Target[0],
         Value,
         SizeOf(Value));

  Result := True;
end;


function TMfMatroskaParser.ReadBytes(const Size: UInt64;
                                     out Data: TBytes): Boolean;
begin

  SetLength(Data,
            0);
  Result := False;

  if (Size > UInt64(MaxInt)) then
    Exit;

  SetLength(Data,
            Integer(Size));

  if (Size = 0) then
    begin

      Result := True;
      Exit;
    end;

  if FStream.Read(Data[0],
                  Integer(Size)) <> Integer(Size) then
    begin
      SetLength(Data,
                0);
      Exit;
    end;

  Result := True;
end;


function TMfMatroskaParser.ReadString(const Size: UInt64;
                                      out Value: string): Boolean;
var
  Data: TBytes;
  DataLength: Integer;

begin

  Value := '';
  Result := False;

  if (Size > MATROSKA_MAX_STRING_SIZE) then
    Exit;

  if not ReadBytes(Size,
                   Data) then
    Exit;

  DataLength := Length(Data);

  while (DataLength > 0) and (Data[DataLength - 1] = 0) do
    Dec(DataLength);
  SetLength(Data,
            DataLength);

  try

    if (Length(Data) > 0) then
      Value := TEncoding.UTF8.GetString(Data)
    else
      Value := '';

    Result := True;
  except
    Value := '';
  end;
end;


function TMfMatroskaParser.LocateSegment(): Boolean;
var
  Header: TEbmlElementHeader;

begin

  Result := False;
  if not SeekAbsolute(0) then
    Exit;

  while (FStream.Position < FFileSize) do
    begin
      if not ReadElementHeader(FFileSize,
                               Header) then
        Exit;

      if (Header.ElementId = EBML_ID_SEGMENT) then
        begin
          FSegmentStart := Header.DataStart;
          FSegmentEnd := Header.DataEnd;
          Result := True;
          Exit;
        end;

      if not SkipTo(Header.DataEnd) then
        Exit;
    end;
end;


function TMfMatroskaParser.ParseInfo(const StartPos: Int64;
                                     const EndPos: Int64): Boolean;
var
  Header: TEbmlElementHeader;
  Value: UInt64;

begin

  Result := SeekAbsolute(StartPos);

  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin
      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_TIMESTAMP_SCALE) then
        if ReadUnsigned(Header.DataSize,
                        Value) and (Value <> 0) then
            FTimestampScaleNs := Value;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := False;
          Exit;
        end;
    end;
end;


class function TMfMatroskaParser.ClassifyCodecId(const CodecId: string): TMfMatroskaSubtitleFormat;
begin

  if SameText(CodecId,
              'S_TEXT/ASCII') or
     SameText(CodecId,
              'S_TEXT/UTF8') then
    Result := msfSrt

  else
    if SameText(CodecId,
                'S_TEXT/SSA') or
       SameText(CodecId,
                'S_TEXT/ASS') then
      Result := msfSsaAss
    else
      if SameText(CodecId,
                  'S_VOBSUB') then
        Result := msfVobSub
      else
        if SameText(CodecId,
                    'S_HDMV/PGS') then
          Result := msfPgs
        else
          Result := msfUnknown;
end;


class function TMfMatroskaParser.NormalizeLanguageCode(const Language: string): string;
var
  Value: string;

begin

  Value := LowerCase(Trim(Language));

  if (Value = 'eng') then
    Result := 'en'
  else
    if (Value = 'dut') or (Value = 'nld') then
      Result := 'nl'
    else
      if (Value = 'ger') or (Value = 'deu') then
        Result := 'de'
      else
        if (Value = 'fre') or (Value = 'fra') then
          Result := 'fr'
        else
          if (Value = 'spa') then
            Result := 'es'
          else
            if (Value = 'ita') then
              Result := 'it'
            else
              if (Value = 'por') then
                Result := 'pt'
              else
                if (Value = 'rus') then
                  Result := 'ru'
                else
                  if (Value = 'jpn') then
                    Result := 'ja'
                  else
                    if (Value = 'chi') or (Value = 'zho') then
                      Result := 'zh'
                    else
                      if (Value = 'kor') then
                        Result := 'ko'
                      else
                        Result := Value;
end;


function TMfMatroskaParser.ParseTrackEntry(const StartPos: Int64;
                                           const EndPos: Int64;
                                           out Track: TMfMatroskaSubtitleTrack;
                                           out IsSubtitle: Boolean): Boolean;
var
  Header: TEbmlElementHeader;
  UnsignedValue: UInt64;
  FloatValue: Double;
  TrackType: UInt64;
  LanguageBcp47: string;

begin

  Track.Reset();
  TrackType := 0;
  LanguageBcp47 := '';
  IsSubtitle := False;

  Result := SeekAbsolute(StartPos);
  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin
      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_TRACK_NUMBER) then
        begin
          if ReadUnsigned(Header.DataSize,
                          UnsignedValue) then
            Track.TrackNumber := UnsignedValue;
        end
      else
        if (Header.ElementId = EBML_ID_TRACK_TYPE) then
          begin
            if ReadUnsigned(Header.DataSize,
                            UnsignedValue) then
              TrackType := UnsignedValue;
          end
        else
          if (Header.ElementId = EBML_ID_FLAG_DEFAULT) then
            begin
              if ReadUnsigned(Header.DataSize,
                              UnsignedValue) then
                Track.DefaultTrack := UnsignedValue <> 0;
            end
          else
            if (Header.ElementId = EBML_ID_FLAG_FORCED) then
              begin
                if ReadUnsigned(Header.DataSize,
                                UnsignedValue) then
                  Track.ForcedTrack := (UnsignedValue <> 0);
              end
            else
              if (Header.ElementId = EBML_ID_DEFAULT_DURATION) then
                begin
                  if ReadUnsigned(Header.DataSize,
                                  UnsignedValue) then
                    Track.DefaultDurationNs := UnsignedValue;
                end
              else
                if (Header.ElementId = EBML_ID_TRACK_TS_SCALE) then
                  begin
                    if ReadFloat(Header.DataSize,
                                 FloatValue) and (FloatValue > 0.0) then
                      Track.TrackTimestampScale := FloatValue;
                  end
                else
                  if (Header.ElementId = EBML_ID_NAME) then
                    ReadString(Header.DataSize,
                      Track.Name)
                  else
                    if (Header.ElementId = EBML_ID_LANGUAGE) then
                      ReadString(Header.DataSize,
                                 Track.Language)
                    else
                      if (Header.ElementId = EBML_ID_LANGUAGE_BCP47) then
                        ReadString(Header.DataSize,
                                   LanguageBcp47)
                      else
                        if (Header.ElementId = EBML_ID_CODEC_ID) then
                          ReadString(Header.DataSize,
                                     Track.CodecId);

      if not SkipTo(Header.DataEnd) then
        begin

          Result := False;
          Exit;
        end;
    end;

  if (LanguageBcp47 <> '') then
    Track.Language := LanguageBcp47;

  if (Track.Language = '') then
    Track.Language := 'eng';

  Track.Language := NormalizeLanguageCode(Track.Language);

  Track.Format := ClassifyCodecId(Track.CodecId);
  Track.Supported := Track.Format in [msfSrt, msfSsaAss];
  IsSubtitle := (TrackType = MATROSKA_TRACK_TYPE_SUBTITLE);
end;


function TMfMatroskaParser.ParseTracks(const StartPos: Int64;
                                       const EndPos: Int64): Boolean;
var
  Header: TEbmlElementHeader;
  Track: TMfMatroskaSubtitleTrack;
  IsSubtitle: Boolean;
  Index: Integer;

begin

  Result := SeekAbsolute(StartPos);
  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin

      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_TRACK_ENTRY) then
        begin

          if not ParseTrackEntry(Header.DataStart,
                                 Header.DataEnd,
                                 Track,
                                 IsSubtitle) then
            begin
              Result := False;
              Exit;
            end;

          if (IsSubtitle and (Track.TrackNumber <> 0)) then
            begin
              Index := Length(FTracks);
              SetLength(FTracks,
                        Index + 1);
              FTracks[Index] := Track;
            end;
        end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := False;
          Exit;
        end;
    end;
end;


function TMfMatroskaParser.ParseMetadata(): Boolean;
var
  Header: TEbmlElementHeader;
  InfoFound: Boolean;
  TracksFound: Boolean;

begin

  FTimestampScaleNs := MATROSKA_DEFAULT_TS_SCALE_NS;
  SetLength(FTracks,
            0);
  InfoFound := False;
  TracksFound := False;

  if Cancelled() then
    begin
      Result := False;
      Exit;
    end;

  Result := LocateSegment();
  if not Result then
    Exit;

  if not SeekAbsolute(FSegmentStart) then
    begin
      Result := False;
      Exit;
    end;

  while (FStream.Position < FSegmentEnd) do
    begin

      if Cancelled() then
        begin
          Result := False;
          Exit;
        end;

      if not ReadElementHeader(FSegmentEnd,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_INFO) then
        begin

          if not ParseInfo(Header.DataStart,
                           Header.DataEnd) then
            begin
              Result := False;
              Exit;
            end;

          InfoFound := True;
        end
      else
        if (Header.ElementId = EBML_ID_TRACKS) then
          begin

            if not ParseTracks(Header.DataStart,
                               Header.DataEnd) then
              begin
                Result := False;
                Exit;
              end;

            TracksFound := True;
          end
        else
          if (Header.ElementId = EBML_ID_CLUSTER) and TracksFound then
            begin
              // Matroska metadata belongs before the media clusters in normal
              // files. Once the Tracks element is known, do not walk every
              // cluster merely to enumerate subtitle tracks. If Segment Info
              // was omitted, retain the standard one-millisecond timestamp
              // scale.
              Result := True;
              Exit;
            end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := False;
          Exit;
        end;

      if InfoFound and TracksFound then
        begin
          Result := True;
          Exit;
        end;
    end;

  Result := True;
end;

function TMfMatroskaParser.FindClusterTimestamp(const StartPos: Int64;
                                                const EndPos: Int64;
                                                out Timestamp: UInt64): Boolean;
var
  Header: TEbmlElementHeader;

begin

  Timestamp := 0;
  Result := SeekAbsolute(StartPos);
  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin
      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_CLUSTER_TIMESTAMP) then
        begin
          Result := ReadUnsigned(Header.DataSize,
                                 Timestamp);
          Exit;
        end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := False;
          Exit;
        end;
    end;

  // Timestamp defaults to zero only for defensive recovery. A valid Cluster
  // contains the element, but accepting zero is more useful than rejecting the
  // complete subtitle track for a malformed first cluster.
  Result := True;
end;


function TMfMatroskaParser.DecodeText(const Data: TBytes): string;
var
  DataCopy: TBytes;
  Count: Integer;

begin

  Result := '';
  Count := Length(Data);
  while (Count > 0) and (Data[Count - 1] = 0) do
    Dec(Count);

  if (Count <= 0) then
    Exit;

  SetLength(DataCopy,
            Count);
  Move(Data[0],
       DataCopy[0],
       Count);

  try
    Result := TEncoding.UTF8.GetString(DataCopy);
  except
    Result := '';
  end;
end;


function TMfMatroskaParser.TicksToMs(const Ticks: Int64;
                                     const TrackScale: Double): Int64;
var
  Nanoseconds: Extended;

begin

  Nanoseconds := Ticks;
  Nanoseconds := Nanoseconds * FTimestampScaleNs;
  Nanoseconds := Nanoseconds * TrackScale;

  if (Nanoseconds <= 0.0) then
    Result := 0
  else
    Result := Round(Nanoseconds / 1000000.0);
end;


function TMfMatroskaParser.DurationTicksToMs(const Ticks: UInt64;
                                             const TrackScale: Double): Int64;
var
  Nanoseconds: Extended;

begin

  Nanoseconds := Ticks;
  Nanoseconds := Nanoseconds * FTimestampScaleNs;
  Nanoseconds := Nanoseconds * TrackScale;

  if (Nanoseconds <= 0.0) then
    Result := 0
  else
    Result := Round(Nanoseconds / 1000000.0);
end;


procedure TMfMatroskaParser.AppendCue(var Cues: TMfMatroskaSubtitleCueArray;
                                      const StartMs: Int64;
                                      const StopMs: Int64;
                                      const Text: string);
var
  Index: Integer;

begin

  if (Text = '') then
    Exit;

  Index := Length(Cues);
  SetLength(Cues,
            Index + 1);
  Cues[Index].StartMs := StartMs;
  Cues[Index].StopMs := StopMs;
  Cues[Index].Text := Text;
end;


function TMfMatroskaParser.ReadLacedFrames(const DataEnd: Int64;
                                           const LacingMode: Integer;
                                           var Frames: array of TBytes;
                                           out FrameCount: Integer): Boolean;
var
  LaceCountByte: Byte;
  FrameSizes: array of Int64;
  I: Integer;
  B: Byte;
  SizeValue: UInt64;
  SizeLength: Integer;
  UnknownValue: Boolean;
  SignedDelta: Int64;
  Bias: UInt64;
  Remaining: Int64;
  TotalKnown: Int64;
  FrameSize: Int64;

begin

  FrameCount := 0;
  Result := False;

  if (LacingMode = 0) then
    begin
      FrameCount := 1;

      if (Length(Frames) < FrameCount) then
        Exit;

      Remaining := DataEnd - FStream.Position;
      if (Remaining < 0) or (UInt64(Remaining) > MATROSKA_MAX_TEXT_FRAME_SIZE) then
        Exit;

      SetLength(Frames[0],
                Integer(Remaining));

      if (Remaining > 0) and
         (FStream.Read(Frames[0][0],
                       Integer(Remaining)) <> Integer(Remaining)) then
        Exit;

      Result := True;
      Exit;
    end;

  if not ReadByte(LaceCountByte) then
    Exit;

  FrameCount := Integer(LaceCountByte) + 1;
  if (FrameCount <= 0) or (Length(Frames) < FrameCount) then
    Exit;

  SetLength(FrameSizes,
            FrameCount);
  TotalKnown := 0;

  case LacingMode of
    1: // Xiph lacing
      begin
        for I := 0 to FrameCount - 2 do
          begin

            FrameSize := 0;
            repeat
              if not ReadByte(B) then
                Exit;

              Inc(FrameSize, B);
            until (B <> $FF);
            FrameSizes[I] := FrameSize;
            Inc(TotalKnown, FrameSize);
          end;
      end;

    2: // Fixed-size lacing
      begin
        Remaining := DataEnd - FStream.Position;
        if (Remaining < 0) or ((Remaining mod FrameCount) <> 0) then
          Exit;

        FrameSize := Remaining div FrameCount;

        for I := 0 to FrameCount - 1 do
          FrameSizes[I] := FrameSize;
        TotalKnown := Remaining;
      end;

    3: // EBML lacing
      begin
        if not ReadVInt(SizeValue,
                        SizeLength,
                        True,
                        UnknownValue) or UnknownValue then
          Exit;

        FrameSizes[0] := Int64(SizeValue);
        TotalKnown := FrameSizes[0];

        for I := 1 to FrameCount - 2 do
          begin

            if not ReadVInt(SizeValue,
                            SizeLength,
                            True,
                            UnknownValue) or UnknownValue then
              Exit;

            Bias := (UInt64(1) shl ((7 * SizeLength) - 1)) - 1;
            SignedDelta := Int64(SizeValue) - Int64(Bias);
            FrameSizes[I] := FrameSizes[I - 1] + SignedDelta;

            if (FrameSizes[I] < 0) then
              Exit;

            Inc(TotalKnown,
                FrameSizes[I]);
          end;
      end;
  else
    Exit;
  end;

  if (LacingMode <> 2) then
    begin
      Remaining := DataEnd - FStream.Position;
      if (Remaining < TotalKnown) then
        Exit;

      FrameSizes[FrameCount - 1] := Remaining - TotalKnown;
    end;

  for I := 0 to FrameCount - 1 do
    begin
      if (FrameSizes[I] < 0) or
         (UInt64(FrameSizes[I]) > MATROSKA_MAX_TEXT_FRAME_SIZE) then
        Exit;

      SetLength(Frames[I],
                Integer(FrameSizes[I]));

      if (FrameSizes[I] > 0) and
         (FStream.Read(Frames[I][0], Integer(FrameSizes[I])) <>
          Integer(FrameSizes[I])) then
        Exit;
    end;

  Result := FStream.Position = DataEnd;
end;

function TMfMatroskaParser.FindTrackDataIndex(const TrackNumber: UInt64;
                                              const TrackData: TMfMatroskaSubtitleTrackDataArray): Integer;
var
  I: Integer;

begin

  Result := -1;

  for I := Low(TrackData) to High(TrackData) do
    if TrackData[I].Track.Supported and
       (TrackData[I].Track.TrackNumber = TrackNumber) then
      begin
        Result := I;
        Exit;
      end;
end;


function TMfMatroskaParser.ParseBlockAll(const StartPos: Int64;
                                         const Size: UInt64;
                                         const ClusterTimestamp: UInt64;
                                         const BlockDurationTicks: UInt64;
                                         const HasBlockDuration: Boolean;
                                         var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;
var
  DataEnd: Int64;
  BlockTrackNumber: UInt64;
  TrackNumberLength: Integer;
  UnknownValue: Boolean;
  TimecodeHigh: Byte;
  TimecodeLow: Byte;
  RelativeTimecode: Int64;
  RawTimecode: Word;
  Flags: Byte;
  LacingMode: Integer;
  FrameStorage: array[0..255] of TBytes;
  FrameCount: Integer;
  TrackIndex: Integer;
  I: Integer;
  StartMs: Int64;
  StopMs: Int64;
  PerFrameDurationMs: Int64;
  DefaultDurationMs: Int64;
  Text: string;
  Track: TMfMatroskaSubtitleTrack;

begin

  Result := False;

  if Cancelled() then
    Exit;

  if (Size > UInt64(MaxInt)) then
    Exit;

  DataEnd := StartPos + Int64(Size);

  if (DataEnd < StartPos) or (DataEnd > FFileSize) then
    Exit;

  if not SeekAbsolute(StartPos) then
    Exit;

  if not ReadVInt(BlockTrackNumber,
                  TrackNumberLength,
                  True,
                  UnknownValue) or UnknownValue then
    Exit;

  if not ReadByte(TimecodeHigh) or
     not ReadByte(TimecodeLow) or
     not ReadByte(Flags) then
    Exit;

  TrackIndex := FindTrackDataIndex(BlockTrackNumber,
                                   TrackData);
  if (TrackIndex < 0) then
    begin
      Result := SkipTo(DataEnd);
      Exit;
    end;

  Track := TrackData[TrackIndex].Track;

  RawTimecode := (Word(TimecodeHigh) shl 8) or Word(TimecodeLow);
  if (RawTimecode >= $8000) then
    RelativeTimecode := Int64(RawTimecode) - $10000
  else
    RelativeTimecode := RawTimecode;

  LacingMode := (Flags and $06) shr 1;
  if not ReadLacedFrames(DataEnd,
                         LacingMode,
                         FrameStorage,
                         FrameCount) then
    Exit;

  StartMs := TicksToMs(Int64(ClusterTimestamp) + RelativeTimecode,
                       Track.TrackTimestampScale);

  DefaultDurationMs := 0;
  if (Track.DefaultDurationNs > 0) then
    DefaultDurationMs := Int64(Track.DefaultDurationNs div 1000000);

  PerFrameDurationMs := 0;
  if HasBlockDuration and (FrameCount > 0) then
    PerFrameDurationMs := DurationTicksToMs(BlockDurationTicks,
                                            Track.TrackTimestampScale) div FrameCount
  else
    if (DefaultDurationMs > 0) then
      PerFrameDurationMs := DefaultDurationMs;

  for I := 0 to FrameCount - 1 do
    begin
      if Cancelled() then
        Exit;

      Text := DecodeText(FrameStorage[I]);
      StopMs := 0;

      if (PerFrameDurationMs > 0) then
        StopMs := StartMs + PerFrameDurationMs;

      AppendCue(TrackData[TrackIndex].Cues,
                StartMs,
                StopMs,
                Text);

      if (PerFrameDurationMs > 0) then
        Inc(StartMs,
            PerFrameDurationMs);
    end;

  Result := True;
end;


function TMfMatroskaParser.ParseBlockGroupAll(const StartPos: Int64;
                                              const EndPos: Int64;
                                              const ClusterTimestamp: UInt64;
                                              var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;
var
  Header: TEbmlElementHeader;
  BlockStart: Int64;
  BlockSize: UInt64;
  BlockDurationTicks: UInt64;
  HasBlock: Boolean;
  HasBlockDuration: Boolean;

begin

  BlockStart := 0;
  BlockSize := 0;
  BlockDurationTicks := 0;
  HasBlock := False;
  HasBlockDuration := False;

  Result := SeekAbsolute(StartPos);
  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin

      if Cancelled() then
        begin
          Result := False;
          Exit;
        end;

      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_BLOCK) then
        begin
          BlockStart := Header.DataStart;
          BlockSize := Header.DataSize;
          HasBlock := True;
        end
      else
        if (Header.ElementId = EBML_ID_BLOCK_DURATION) then
          begin
            if ReadUnsigned(Header.DataSize,
                            BlockDurationTicks) then
              HasBlockDuration := True;
          end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := False;
          Exit;
        end;
    end;

  if HasBlock then
    Result := ParseBlockAll(BlockStart,
                            BlockSize,
                            ClusterTimestamp,
                            BlockDurationTicks,
                            HasBlockDuration,
                            TrackData)
  else
    Result := True;
end;


function TMfMatroskaParser.ParseClusterAll(const StartPos: Int64;
                                           const EndPos: Int64;
                                           const ClusterTimestamp: UInt64;
                                           var TrackData: TMfMatroskaSubtitleTrackDataArray): Boolean;
var
  Header: TEbmlElementHeader;

begin

  Result := SeekAbsolute(StartPos);
  if not Result then
    Exit;

  while (FStream.Position < EndPos) do
    begin

      if Cancelled() then
        begin
          Result := False;
          Exit;
        end;

      if not ReadElementHeader(EndPos,
                               Header) then
        begin
          Result := False;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_SIMPLE_BLOCK) then
        begin
          if not ParseBlockAll(Header.DataStart,
                               Header.DataSize,
                               ClusterTimestamp,
                               0,
                               False,
                               TrackData) then
            begin

              Result := False;
              Exit;
            end;
        end
      else
        if (Header.ElementId = EBML_ID_BLOCK_GROUP) then
          begin
            if not ParseBlockGroupAll(Header.DataStart,
                                      Header.DataEnd,
                                      ClusterTimestamp,
                                      TrackData) then
              begin
                Result := False;
                Exit;
              end;
          end;

      if not SkipTo(Header.DataEnd) then
        begin

          Result := False;
          Exit;
        end;
    end;
end;

procedure TMfMatroskaParser.QuickSortCues(var Cues: TMfMatroskaSubtitleCueArray;
                                          L: Integer;
                                          R: Integer);

var
  I: Integer;
  J: Integer;
  Pivot: Int64;
  Temp: TMfMatroskaSubtitleCue;

begin

  I := L;
  J := R;
  Pivot := Cues[(L + R) div 2].StartMs;

  repeat
    while (Cues[I].StartMs < Pivot) do
      Inc(I);
    while (Cues[J].StartMs > Pivot) do
      Dec(J);

    if (I <= J) then
      begin

        Temp := Cues[I];
        Cues[I] := Cues[J];
        Cues[J] := Temp;
        Inc(I);
        Dec(J);
      end;
  until (I > J);

  if (L < J) then
    QuickSortCues(Cues,
                  L,
                  J);

  if (I < R) then
    QuickSortCues(Cues,
                  I,
                  R);
end;


procedure TMfMatroskaParser.SortAndNormalizeCues(var Cues: TMfMatroskaSubtitleCueArray);
var
  I: Integer;
  NextStart: Int64;

begin

  if (Length(Cues) = 0) then
    Exit;

  if (Length(Cues) > 1) then
    QuickSortCues(Cues,
                  Low(Cues),
                  High(Cues));

  for I := Low(Cues) to High(Cues) do
    begin

      if (Cues[I].StartMs < 0) then
        Cues[I].StartMs := 0;

      if (Cues[I].StopMs <= Cues[I].StartMs) then
        begin
          NextStart := 0;

          if (I < High(Cues)) then
            NextStart := Cues[I + 1].StartMs;

          if (NextStart > Cues[I].StartMs) then
            Cues[I].StopMs := NextStart
          else
            Cues[I].StopMs := Cues[I].StartMs + MATROSKA_DEFAULT_CUE_MS;
        end;
    end;
end;


function TMfMatroskaParser.Enumerate(out Tracks: TMfMatroskaSubtitleTrackArray): HRESULT;
var
  I: Integer;

begin

  SetLength(Tracks,
            0);

  if not ParseMetadata() then
    begin

      Result := E_FAIL;
      Exit;
    end;

  SetLength(Tracks,
            Length(FTracks));

  for I := Low(FTracks) to High(FTracks) do
    Tracks[I] := FTracks[I];

  if (Length(Tracks) = 0) then
    Result := S_FALSE
  else
    Result := S_OK;
end;


function TMfMatroskaParser.ReadAllSubtitleTracks(out TrackData: TMfMatroskaSubtitleTrackDataArray): HRESULT;
var
  Header: TEbmlElementHeader;
  ClusterTimestamp: UInt64;
  I: Integer;
  SupportedTrackFound: Boolean;
  CueFound: Boolean;

begin

  SetLength(TrackData,
            0);

  if not ParseMetadata() then
    begin
      if Cancelled() then
        Result := E_ABORT
      else
        Result := E_FAIL;
      Exit;
    end;

  SetLength(TrackData,
            Length(FTracks));
  SupportedTrackFound := False;

  for I := Low(FTracks) to High(FTracks) do
    begin
      TrackData[I].Track := FTracks[I];
      SetLength(TrackData[I].Cues,
                0);

      if FTracks[I].Supported then
        SupportedTrackFound := True;
    end;

  if not SupportedTrackFound then
    begin
      Result := S_FALSE;
      Exit;
    end;

  if not SeekAbsolute(FSegmentStart) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  while (FStream.Position < FSegmentEnd) do
    begin
      if Cancelled() then
        begin
          Result := E_ABORT;
          Exit;
        end;

      if not ReadElementHeader(FSegmentEnd,
                               Header) then
        begin
          Result := E_FAIL;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_CLUSTER) then
        begin
          if not FindClusterTimestamp(Header.DataStart,
                                      Header.DataEnd,
                                      ClusterTimestamp) or
             not ParseClusterAll(Header.DataStart,
                                 Header.DataEnd,
                                 ClusterTimestamp,
                                 TrackData) then
            begin
              if Cancelled() then
                Result := E_ABORT
              else
                Result := E_FAIL;
              Exit;
            end;
        end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := E_FAIL;
          Exit;
        end;
    end;

  CueFound := False;

  for I := Low(TrackData) to High(TrackData) do
    begin
      SortAndNormalizeCues(TrackData[I].Cues);
      if (Length(TrackData[I].Cues) > 0) then
        CueFound := True;
    end;

  if CueFound then
    Result := S_OK
  else
    Result := S_FALSE;
end;


function TMfMatroskaParser.ReadSubtitleTrackWindow(const TrackNumber: UInt64;
                                                   const StartMs: Int64;
                                                   const EndMs: Int64;
                                                   out Track: TMfMatroskaSubtitleTrack;
                                                   out Cues: TMfMatroskaSubtitleCueArray;
                                                   out EndOfTrack: Boolean): HRESULT;
var
  Header: TEbmlElementHeader;
  ClusterTimestamp: UInt64;
  ClusterTimeMs: Int64;
  TrackData: TMfMatroskaSubtitleTrackDataArray;
  Filtered: TMfMatroskaSubtitleCueArray;
  TrackIndex: Integer;
  I: Integer;
  Count: Integer;

begin

  Track.Reset();
  SetLength(Cues, 0);
  EndOfTrack := False;

  if (EndMs <= StartMs) or not ParseMetadata() then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  TrackIndex := -1;
  SetLength(TrackData, Length(FTracks));

  for I := Low(FTracks) to High(FTracks) do
    begin
      TrackData[I].Track := FTracks[I];
      SetLength(TrackData[I].Cues, 0);
      if FTracks[I].TrackNumber = TrackNumber then
        begin
          TrackIndex := I;
          Track := FTracks[I];
        end;
    end;

  if (TrackIndex < 0) then
    begin
      Result := HRESULT(MF_E_INVALIDSTREAMNUMBER);
      Exit;
    end;

  if not Track.Supported then
    begin
      Result := HRESULT(MF_E_INVALIDMEDIATYPE);
      Exit;
    end;

  if not SeekAbsolute(FSegmentStart) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  while (FStream.Position < FSegmentEnd) do
    begin
      if Cancelled() then
        begin
          Result := E_ABORT;
          Exit;
        end;

      if not ReadElementHeader(FSegmentEnd, Header) then
        begin
          Result := E_FAIL;
          Exit;
        end;

      if (Header.ElementId = EBML_ID_CLUSTER) then
        begin
          if not FindClusterTimestamp(Header.DataStart, Header.DataEnd,
                                      ClusterTimestamp) then
            begin
              Result := E_FAIL;
              Exit;
            end;

          ClusterTimeMs := TicksToMs(ClusterTimestamp, 1.0);

          if (ClusterTimeMs >= EndMs) then
            Break;

          // Cluster payloads before the requested window can be skipped with
          // a seek. Retain one overlap interval because block timestamps are
          // relative to the cluster and a long cue may cross the boundary.
          if (ClusterTimeMs + MATROSKA_WINDOW_CLUSTER_OVERLAP_MS >= StartMs) then
            if not ParseClusterAll(Header.DataStart, Header.DataEnd,
                                   ClusterTimestamp, TrackData) then
              begin
                if Cancelled() then
                  Result := E_ABORT
                else
                  Result := E_FAIL;
                Exit;
              end;
        end;

      if not SkipTo(Header.DataEnd) then
        begin
          Result := E_FAIL;
          Exit;
        end;
    end;

  EndOfTrack := FStream.Position >= FSegmentEnd;
  SortAndNormalizeCues(TrackData[TrackIndex].Cues);
  SetLength(Filtered, Length(TrackData[TrackIndex].Cues));
  Count := 0;

  for I := Low(TrackData[TrackIndex].Cues) to High(TrackData[TrackIndex].Cues) do
    if (TrackData[TrackIndex].Cues[I].StopMs > StartMs) and
       (TrackData[TrackIndex].Cues[I].StartMs < EndMs) then
      begin
        Filtered[Count] := TrackData[TrackIndex].Cues[I];
        Inc(Count);
      end;

  SetLength(Filtered,
            Count);
  Cues := Filtered;

  if (Count > 0) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

class function TMfMatroskaSubtitleReader.EnumerateTracks(const FileName: WideString;
                                                         out Tracks: TMfMatroskaSubtitleTrackArray): HRESULT;
var
  Parser: TMfMatroskaParser;
  FileSize: Int64;
  LastWriteTime: TFileTime;
  I: Integer;

begin

  SetLength(Tracks,
            0);

  if (FileName = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if GetMatroskaFileIdentity(FileName,
                             FileSize,
                             LastWriteTime) then
    begin
      GMatroskaSubtitleCacheLock.Acquire();

      try
        if MatroskaCacheMatches(FileName,
                                FileSize,
                                LastWriteTime) then
          begin

            SetLength(Tracks,
                      Length(GMatroskaSubtitleCache.TrackData));

            for I := Low(GMatroskaSubtitleCache.TrackData) to
                     High(GMatroskaSubtitleCache.TrackData) do
              Tracks[I] := GMatroskaSubtitleCache.TrackData[I].Track;

            if (Length(Tracks) = 0) then
              Result := S_FALSE
            else
              Result := S_OK;
            Exit;
          end;
      finally
        GMatroskaSubtitleCacheLock.Release();
      end;
    end;

  Parser := nil;

  try
    try
      Parser := TMfMatroskaParser.Create(FileName);
      Result := Parser.Enumerate(Tracks);
    except
      on E: Exception do
        Result := E_FAIL;
    end;
  finally
    Parser.Free();
  end;
end;

class function TMfMatroskaSubtitleReader.ReadTrack(const FileName: WideString;
                                                   const TrackNumber: UInt64;
                                                   out Track: TMfMatroskaSubtitleTrack;
                                                   out Cues: TMfMatroskaSubtitleCueArray;
                                                   const CancelEvent: THandle): HRESULT;
var
  Parser: TMfMatroskaParser;
  TrackData: TMfMatroskaSubtitleTrackDataArray;
  FileSize: Int64;
  LastWriteTime: TFileTime;
  I: Integer;
  CacheHit: Boolean;

begin

  Track.Reset();
  SetLength(Cues,
            0);
  SetLength(TrackData,
            0);

  if (FileName = '') or (TrackNumber = 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if not GetMatroskaFileIdentity(FileName,
                                 FileSize,
                                 LastWriteTime) then
    begin

      Result := E_FAIL;
      Exit;
    end;

  CacheHit := False;

  GMatroskaSubtitleCacheLock.Acquire();
  try
    if MatroskaCacheMatches(FileName,
                            FileSize,
                            LastWriteTime) then
      begin
        for I := Low(GMatroskaSubtitleCache.TrackData) to
                 High(GMatroskaSubtitleCache.TrackData) do
          if (GMatroskaSubtitleCache.TrackData[I].Track.TrackNumber = TrackNumber) then
            begin
              Track := GMatroskaSubtitleCache.TrackData[I].Track;
              CopyMatroskaCues(GMatroskaSubtitleCache.TrackData[I].Cues,
                               Cues);
              CacheHit := True;
              Break;
            end;
      end;
  finally
    GMatroskaSubtitleCacheLock.Release();
  end;

  if CacheHit then
    begin
      if not Track.Supported then
        Result := HRESULT(MF_E_INVALIDMEDIATYPE)
      else
        if (Length(Cues) = 0) then
          Result := S_FALSE
        else
          Result := S_OK;
      Exit;
    end;

  Parser := nil;

  try
    try
      Parser := TMfMatroskaParser.Create(FileName,
                                         CancelEvent);
      Result := Parser.ReadAllSubtitleTracks(TrackData);
    except
      on E: Exception do
        Result := E_FAIL;
    end;
  finally
    Parser.Free();
  end;

  if FAILED(Result) then
    Exit;

  GMatroskaSubtitleCacheLock.Acquire();
  try
    GMatroskaSubtitleCache.FileName := FileName;
    GMatroskaSubtitleCache.FileSize := FileSize;
    GMatroskaSubtitleCache.LastWriteTime := LastWriteTime;
    GMatroskaSubtitleCache.TrackData := TrackData;
    GMatroskaSubtitleCache.Valid := True;
  finally
    GMatroskaSubtitleCacheLock.Release();
  end;

  for I := Low(TrackData) to High(TrackData) do
    if (TrackData[I].Track.TrackNumber = TrackNumber) then
      begin
        Track := TrackData[I].Track;

        if not Track.Supported then
          begin
            Result := HRESULT(MF_E_INVALIDMEDIATYPE);
            Exit;
          end;

        CopyMatroskaCues(TrackData[I].Cues,
                         Cues);

        if (Length(Cues) = 0) then
          Result := S_FALSE
        else
          Result := S_OK;
        Exit;
      end;

  Result := HRESULT(MF_E_INVALIDSTREAMNUMBER);
end;


class function TMfMatroskaSubtitleReader.ReadTrackWindow(const FileName: WideString;
                                                         const TrackNumber: UInt64;
                                                         const StartMs: Int64;
                                                         const EndMs: Int64;
                                                         out Track: TMfMatroskaSubtitleTrack;
                                                         out Cues: TMfMatroskaSubtitleCueArray;
                                                         out EndOfTrack: Boolean;
                                                         const CancelEvent: THandle): HRESULT;
var
  Parser: TMfMatroskaParser;

begin

  Track.Reset();
  SetLength(Cues, 0);
  EndOfTrack := False;

  if (FileName = '') or (TrackNumber = 0) or (EndMs <= StartMs) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Parser := nil;

  try
    try
      Parser := TMfMatroskaParser.Create(FileName, CancelEvent);
      Result := Parser.ReadSubtitleTrackWindow(TrackNumber, StartMs, EndMs,
                                               Track, Cues, EndOfTrack);
    except
      on E: Exception do
        Result := E_FAIL;
    end;
  finally
    Parser.Free();
  end;
end;


initialization
  GMatroskaSubtitleCacheLock := TCriticalSection.Create();
  GMatroskaSubtitleCache.Valid := False;

finalization
  SetLength(GMatroskaSubtitleCache.TrackData,
            0);
  FreeAndNil(GMatroskaSubtitleCacheLock);

end.
