// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SimpleFmp4ByteStream.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Transparent IMFByteStream wrapper for MfSimpleWebCamStreamer.
//              It forwards all byte-stream operations to a real Media Foundation byte
//              stream and passively observes top-level fragmented MP4 boxes.
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
unit SimpleFmp4ByteStream;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError;

type

  TSimpleFmp4Fragment = record
    Sequence: UInt64;
    Data: TBytes;
  end;


  TSimpleFmp4ByteStream = class(TObject, IInterface, IMFByteStream)
  private
    FRefCount: Integer;
    FInner: IMFByteStream;
    FNullOutput: Boolean;
    FNullPosition: UInt64;
    FNullLength: UInt64;
    FAsyncWriteSizes: TQueue<ULONG>;
    FLock: TCriticalSection;

    FParserBuffer: TBytes;
    FParserBufferSize: Integer;

    FInitSegment: TBytes;
    FPendingMoof: TBytes;
    FFragments: TQueue<TSimpleFmp4Fragment>;
    FArchiveFragments: TQueue<TBytes>;
    FTrackDecodeTimes: TDictionary<DWORD, UInt64>;
    FArchiveBaseDecodeTimes: TDictionary<DWORD, UInt64>;

    FNextSequence: UInt64;
    FFragmentCount: UInt64;
    FTotalBytesObserved: UInt64;

    procedure ObserveBytes(const AData: PByte;
                           const ACount: ULONG);
    procedure ProcessBox(const ABoxType: DWORD;
                         const ABoxData: TBytes);
    procedure QueueFragment(const AData: TBytes);

    function PatchFragmentForMse(const AFragment: TBytes;
                                 out APatchedFragment: TBytes): Boolean;

    function RebaseArchiveFragmentTfdt(var AFragment: TBytes): Boolean;

  public

    constructor Create(const AInner: IMFByteStream);
    destructor Destroy; override;

    {IInterface}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    {IMFByteStream}
    function GetCapabilities(out pdwCapabilities: DWORD): HRESULT; stdcall;
    function GetLength(out pqwLength: UInt64): HRESULT; stdcall;
    function SetLength(qwLength: UInt64): HRESULT; stdcall;
    function GetCurrentPosition(out pqwPosition: QWORD): HRESULT; stdcall;
    function SetCurrentPosition(const qwPosition: QWORD): HRESULT; stdcall;
    function IsEndOfStream(out pfEndOfStream: BOOL): HRESULT; stdcall;

    function Read(pb: PByte;
                  cb: ULONG;
                  out pcbRead: ULONG): HRESULT; stdcall;

    function BeginRead(pb: PByte;
                       cb: ULONG;
                       pCallback: IMFAsyncCallback;
                       punkState: IUnknown): HRESULT; stdcall;

    function EndRead(pResult: IMFAsyncResult;
                     out pcbRead: ULONG): HRESULT; stdcall;

    function Write(pb: PByte;
                   cb: ULONG;
                   out pcbWritten: ULONG): HRESULT; stdcall;

    function BeginWrite(pb: PByte;
                        cb: ULONG;
                        pCallback: IMFAsyncCallback;
                        punkState: IUnknown): HRESULT; stdcall;

    function EndWrite(pResult: IMFAsyncResult;
                      out pcbWritten: ULONG): HRESULT; stdcall;

    function Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                  llSeekOffset: LONGLONG;
                  dwSeekFlags: DWORD;
                  out pqwCurrentPosition: UInt64): HRESULT; stdcall;

    function Flush: HRESULT; stdcall;
    function Close: HRESULT; stdcall;

    function GetInitSegment(out ASegment: TBytes): Boolean;

    function GetFragment(const ASequence: UInt64;
                         out AFragment: TBytes): Boolean;

    function GetFragmentWindow(out AFirstSequence: UInt64;
                               out ALastSequence: UInt64;
                               out ACount: Integer): Boolean;

    function TryPopArchiveFragment(out AFragment: TBytes): Boolean;
    procedure ResetArchiveTimeline;

    function GetDiagnostics(out AInitBytes: Integer;
                            out ALastFragmentBytes: Integer;
                            out AFragmentCount: UInt64;
                            out ATotalBytesObserved: UInt64): Boolean;
  end;


implementation


{$POINTERMATH ON}

const
  BOX_FTYP = $66747970;
  BOX_MOOV = $6D6F6F76;
  BOX_MOOF = $6D6F6F66;
  BOX_MDAT = $6D646174;

  SIMPLE_FMP4_MAX_FRAGMENTS = 12;
  SIMPLE_FMP4_MAX_ARCHIVE_FRAGMENTS = 64;


function ReadU32BE(const P: PByte): DWORD; overload;
begin

  Result := (DWORD(P[0]) shl 24) or
            (DWORD(P[1]) shl 16) or
            (DWORD(P[2]) shl 8) or
             DWORD(P[3]);
end;


function ReadU64BE(const P: PByte): UInt64; overload;
begin

  Result := (UInt64(ReadU32BE(P)) shl 32) or
             UInt64(ReadU32BE(P + 4));
end;


procedure AppendBytes(var ADest: TBytes;
                      const ASource: TBytes);
var
  OldLength: Integer;

begin

  if Length(ASource) = 0 then
    Exit;

  OldLength := Length(ADest);
  System.SetLength(ADest,
            OldLength + Length(ASource));

  Move(ASource[0],
       ADest[OldLength],
       Length(ASource));
end;


function ReadU32BE(const AData: TBytes;
                   const AOffset: Integer): DWORD; overload;
begin

  Result := (DWORD(AData[AOffset]) shl 24) or
            (DWORD(AData[AOffset + 1]) shl 16) or
            (DWORD(AData[AOffset + 2]) shl 8) or
             DWORD(AData[AOffset + 3]);
end;


function ReadU64BE(const AData: TBytes;
                   const AOffset: Integer): UInt64; overload;
begin

  Result := (UInt64(AData[AOffset]) shl 56) or
            (UInt64(AData[AOffset + 1]) shl 48) or
            (UInt64(AData[AOffset + 2]) shl 40) or
            (UInt64(AData[AOffset + 3]) shl 32) or
            (UInt64(AData[AOffset + 4]) shl 24) or
            (UInt64(AData[AOffset + 5]) shl 16) or
            (UInt64(AData[AOffset + 6]) shl 8) or
             UInt64(AData[AOffset + 7]);
end;


procedure WriteU32BE(var AData: TBytes;
                     const AOffset: Integer;
                     const AValue: DWORD);
begin

  AData[AOffset] := Byte((AValue shr 24) and $FF);
  AData[AOffset + 1] := Byte((AValue shr 16) and $FF);
  AData[AOffset + 2] := Byte((AValue shr 8) and $FF);
  AData[AOffset + 3] := Byte(AValue and $FF);
end;


procedure WriteU64BE(var AData: TBytes;
                     const AOffset: Integer;
                     const AValue: UInt64);
begin

  AData[AOffset] := Byte((AValue shr 56) and $FF);
  AData[AOffset + 1] := Byte((AValue shr 48) and $FF);
  AData[AOffset + 2] := Byte((AValue shr 40) and $FF);
  AData[AOffset + 3] := Byte((AValue shr 32) and $FF);
  AData[AOffset + 4] := Byte((AValue shr 24) and $FF);
  AData[AOffset + 5] := Byte((AValue shr 16) and $FF);
  AData[AOffset + 6] := Byte((AValue shr 8) and $FF);
  AData[AOffset + 7] := Byte(AValue and $FF);
end;


function CopyBytes(const AData: TBytes;
                   const AOffset: Integer;
                   const ACount: Integer): TBytes;
begin

  System.SetLength(Result,
                   ACount);

  if ACount > 0 then
    Move(AData[AOffset],
         Result[0],
         ACount);
end;


procedure AppendRaw(var ADest: TBytes;
                    const AData: TBytes;
                    const AOffset: Integer;
                    const ACount: Integer);
var
  OldLength: Integer;

begin

  if ACount <= 0 then
    Exit;

  OldLength := Length(ADest);

  System.SetLength(ADest,
                   OldLength + ACount);

  Move(AData[AOffset],
       ADest[OldLength],
       ACount);
end;


function MakeTfdtBox(const ABaseDecodeTime: UInt64): TBytes;
begin

  System.SetLength(Result,
                   20);

  WriteU32BE(Result,
             0,
             20);

  WriteU32BE(Result,
             4,
             $74666474); // tfdt

  WriteU32BE(Result,
             8,
             $01000000); // version 1, flags 0

  WriteU64BE(Result,
             12,
             ABaseDecodeTime);
end;


function PatchTfhdBox(const ABoxData: TBytes;
                      out ATrackId: DWORD): TBytes;
var
  Flags: DWORD;
  NewFlags: DWORD;
  Cursor: Integer;
  Remaining: Integer;

begin

  Result := nil;
  ATrackId := 0;

  if Length(ABoxData) < 16 then
    Exit;

  Flags := ReadU32BE(ABoxData,
                     8) and $00FFFFFF;

  ATrackId := ReadU32BE(ABoxData,
                        12);

  // MF commonly emits base_data_offset. MSE is much happier when fragments
  // are self-contained and use the moof as their addressing base.
  NewFlags := (Flags and not DWORD($000001)) or DWORD($020000);

  System.SetLength(Result,
                   16);

  WriteU32BE(Result,
             4,
             $74666864); // tfhd

  WriteU32BE(Result,
             8,
             NewFlags);

  WriteU32BE(Result,
             12,
             ATrackId);

  Cursor := 16;

  if ((Flags and $000001) <> 0) then
    Inc(Cursor,
        8);

  Remaining := Length(ABoxData) - Cursor;

  if (Remaining > 0) then
    AppendRaw(Result,
              ABoxData,
              Cursor,
              Remaining);

  WriteU32BE(Result,
             0,
             DWORD(Length(Result)));
end;


function GetTfhdDefaultSampleDuration(const ABoxData: TBytes): DWORD;
var
  Flags: DWORD;
  Cursor: Integer;

begin

  Result := 0;

  if (Length(ABoxData) < 16) then
    Exit;

  Flags := ReadU32BE(ABoxData,
                     8) and $00FFFFFF;

  Cursor := 16;

  if ((Flags and $000001) <> 0) then
    Inc(Cursor,
        8);

  if ((Flags and $000002) <> 0) then
    Inc(Cursor,
        4);

  if ((Flags and $000008) <> 0) then
    begin
      if (Cursor + 4 <= Length(ABoxData)) then
        Result := ReadU32BE(ABoxData,
                            Cursor);
    end;
end;


function GetTrunDuration(const ABoxData: TBytes;
                         const ADefaultSampleDuration: DWORD): UInt64;
var
  Flags: DWORD;
  SampleCount: DWORD;
  Cursor: Integer;
  I: DWORD;
  SampleDuration: DWORD;

begin

  Result := 0;
  SampleDuration := 0;

  if Length(ABoxData) < 16 then
    Exit;

  Flags := ReadU32BE(ABoxData,
                     8) and $00FFFFFF;

  SampleCount := ReadU32BE(ABoxData,
                           12);

  Cursor := 16;

  if ((Flags and $000001) <> 0) then
    Inc(Cursor,
        4);

  if ((Flags and $000004) <> 0) then
    Inc(Cursor,
        4);

  for I := 0 to SampleCount - 1 do
    begin
      if ((Flags and $000100) <> 0) then
        begin
          if (Cursor + 4 > Length(ABoxData)) then
            Break;

          SampleDuration := ReadU32BE(ABoxData,
                                      Cursor);

          Inc(Cursor,
              4);
        end
      else
        SampleDuration := ADefaultSampleDuration;

      Inc(Result,
          SampleDuration);

      if ((Flags and $000200) <> 0) then
        Inc(Cursor,
            4);

      if ((Flags and $000400) <> 0) then
        Inc(Cursor,
            4);

      if ((Flags and $000800) <> 0) then
        Inc(Cursor,
            4);
    end;
end;


function PatchTrunDataOffset(var ABoxData: TBytes;
                             const ANewDataOffset: DWORD): Boolean;
var
  Flags: DWORD;

begin

  Result := False;

  if (Length(ABoxData) < 20) then
    Exit;

  Flags := ReadU32BE(ABoxData,
                     8) and $00FFFFFF;

  if (Flags and $000001) = 0 then
    Exit;

  WriteU32BE(ABoxData,
             16,
             ANewDataOffset);

  Result := True;
end;


{ TSimpleFmp4ByteStream }

function TSimpleFmp4ByteStream.PatchFragmentForMse(const AFragment: TBytes;
                                                        out APatchedFragment: TBytes): Boolean;
var
  MoofSize: DWORD;
  MdatSize: DWORD;
  MdatOffset: Integer;
  RawMoof: TBytes;
  PatchedMoof: TBytes;
  PatchedTrafList: TList<TBytes>;
  DataOffsetFixups: TList<Integer>;
  TrafDataOffsets: TList<DWORD>;
  TrafTrackIds: TList<DWORD>;
  TrafDurations: TList<UInt64>;
  Cursor: Integer;
  BoxSize: DWORD;
  BoxType: DWORD;
  SubCursor: Integer;
  SubEnd: Integer;
  SubBoxSize: DWORD;
  SubBoxType: DWORD;
  NewTraf: TBytes;
  NewBox: TBytes;
  TrackId: DWORD;
  BaseDecodeTime: UInt64;
  DefaultDuration: DWORD;
  TrafDuration: UInt64;
  NewMoofSize: DWORD;
  OldTrunDataOffset: DWORD;
  OldDecodeTime: UInt64;
  FixOffset: Integer;
  I: Integer;

begin

  Result := False;
  APatchedFragment := nil;

  if (Length(AFragment) < 16) then
    Exit;

  MoofSize := ReadU32BE(AFragment,
                        0);

  if (MoofSize < 8) or
     (MoofSize > DWORD(Length(AFragment))) or
     (ReadU32BE(AFragment,
                4) <> BOX_MOOF) then
    Exit;

  MdatOffset := Integer(MoofSize);

  if (MdatOffset + 8) > Length(AFragment) then
    Exit;

  MdatSize := ReadU32BE(AFragment,
                        MdatOffset);

  if (MdatSize < 8) or
     (MdatOffset + Integer(MdatSize) > Length(AFragment)) or
     (ReadU32BE(AFragment,
                MdatOffset + 4) <> BOX_MDAT) then
    Exit;

  RawMoof := CopyBytes(AFragment,
                       0,
                       Integer(MoofSize));

  PatchedTrafList := TList<TBytes>.Create();
  DataOffsetFixups := TList<Integer>.Create();
  TrafDataOffsets := TList<DWORD>.Create();
  TrafTrackIds := TList<DWORD>.Create();
  TrafDurations := TList<UInt64>.Create();
  try
    PatchedMoof := nil;

    AppendRaw(PatchedMoof,
              RawMoof,
              0,
              8);

    Cursor := 8;

    while Cursor + 8 <= Integer(MoofSize) do
      begin
        BoxSize := ReadU32BE(RawMoof,
                             Cursor);

        BoxType := ReadU32BE(RawMoof,
                             Cursor + 4);

        if (BoxSize < 8) or
           (Cursor + Integer(BoxSize) > Integer(MoofSize)) then
          Exit;

        if (BoxType <> $74726166) then // traf
          begin
            AppendRaw(PatchedMoof,
                      RawMoof,
                      Cursor,
                      Integer(BoxSize));

            Inc(Cursor,
                Integer(BoxSize));

            Continue;
          end;

        NewTraf := nil;

        AppendRaw(NewTraf,
                  RawMoof,
                  Cursor,
                  8);

        TrackId := 0;
        DefaultDuration := 0;
        TrafDuration := 0;
        FixOffset := -1;
        OldTrunDataOffset := 0;

        SubCursor := Cursor + 8;
        SubEnd := Cursor + Integer(BoxSize);

        while (SubCursor + 8 <= SubEnd) do
          begin
            SubBoxSize := ReadU32BE(RawMoof,
                                    SubCursor);

            SubBoxType := ReadU32BE(RawMoof,
                                    SubCursor + 4);

            if (SubBoxSize < 8) or
               (SubCursor + Integer(SubBoxSize) > SubEnd) then
              Exit;

            if (SubBoxType = $74666864) then // tfhd
              begin
                DefaultDuration := GetTfhdDefaultSampleDuration(
                  CopyBytes(RawMoof,
                            SubCursor,
                            Integer(SubBoxSize)));

                NewBox := PatchTfhdBox(
                  CopyBytes(RawMoof,
                            SubCursor,
                            Integer(SubBoxSize)),
                  TrackId);

                AppendBytes(NewTraf,
                            NewBox);

                if FTrackDecodeTimes.TryGetValue(TrackId,
                                                 BaseDecodeTime) then
                  begin
                  end
                else
                  BaseDecodeTime := 0;

                AppendBytes(NewTraf,
                            MakeTfdtBox(BaseDecodeTime));
              end
            else
              if SubBoxType = $7472756E then // trun
                begin
                  NewBox := CopyBytes(RawMoof,
                                      SubCursor,
                                      Integer(SubBoxSize));

                  Inc(TrafDuration,
                      GetTrunDuration(NewBox,
                                      DefaultDuration));

                  if (Length(NewBox) >= 20) and
                     ((ReadU32BE(NewBox,
                                 8) and $000001) <> 0) then
                    OldTrunDataOffset := ReadU32BE(NewBox,
                                                   16);

                  if PatchTrunDataOffset(NewBox,
                                         0) then
                    FixOffset := Length(NewTraf) + 16;

                  AppendBytes(NewTraf,
                              NewBox);
                end
              else
                AppendRaw(NewTraf,
                          RawMoof,
                          SubCursor,
                          Integer(SubBoxSize));

            Inc(SubCursor,
                Integer(SubBoxSize));
          end;

        WriteU32BE(NewTraf,
                   0,
                   DWORD(Length(NewTraf)));

        PatchedTrafList.Add(NewTraf);
        TrafTrackIds.Add(TrackId);
        TrafDurations.Add(TrafDuration);
        DataOffsetFixups.Add(FixOffset);
        TrafDataOffsets.Add(OldTrunDataOffset);

        Inc(Cursor,
            Integer(BoxSize));
      end;

    for I := 0 to PatchedTrafList.Count - 1 do
      AppendBytes(PatchedMoof,
                  PatchedTrafList[I]);

    NewMoofSize := DWORD(Length(PatchedMoof));

    WriteU32BE(PatchedMoof,
               0,
               NewMoofSize);

    // data_offset is moof-relative after default-base-is-moof is set.
    Cursor := 8;

    for I := 0 to PatchedTrafList.Count - 1 do
      begin
        while (Cursor + 8 <= Integer(NewMoofSize)) and
              (ReadU32BE(PatchedMoof,
                         Cursor + 4) <> $74726166) do
          Inc(Cursor,
              Integer(ReadU32BE(PatchedMoof,
                                Cursor)));

        if (Cursor + 8 > Integer(NewMoofSize)) then
          Break;

        FixOffset := DataOffsetFixups[I];

        if (FixOffset >= 0) then
          WriteU32BE(PatchedMoof,
                     Cursor + FixOffset,
                     NewMoofSize + 8 + TrafDataOffsets[I]);

        Inc(Cursor,
            Integer(ReadU32BE(PatchedMoof,
                              Cursor)));
      end;

    System.SetLength(APatchedFragment,
                     Length(PatchedMoof) + Integer(MdatSize));

    if (Length(PatchedMoof) > 0) then
      Move(PatchedMoof[0],
           APatchedFragment[0],
           Length(PatchedMoof));

    if (MdatSize > 0) then
      Move(AFragment[MdatOffset],
           APatchedFragment[Length(PatchedMoof)],
           Integer(MdatSize));

    for I := 0 to TrafTrackIds.Count - 1 do
      begin
        if FTrackDecodeTimes.TryGetValue(TrafTrackIds[I],
                                         OldDecodeTime) then
          FTrackDecodeTimes[TrafTrackIds[I]] :=
            OldDecodeTime + TrafDurations[I]
        else
          FTrackDecodeTimes.Add(TrafTrackIds[I],
                                TrafDurations[I]);
      end;

    Result := (Length(APatchedFragment) > 0);

  finally
    RawMoof := nil;
    PatchedMoof := nil;
    NewTraf := nil;
    NewBox := nil;

    PatchedTrafList.Free();
    DataOffsetFixups.Free();
    TrafDataOffsets.Free();
    TrafTrackIds.Free();
    TrafDurations.Free();
  end;
end;


function TSimpleFmp4ByteStream.RebaseArchiveFragmentTfdt(var AFragment: TBytes): Boolean;
var
  MoofSize: DWORD;
  Cursor: Integer;
  TrafEnd: Integer;
  BoxSize: DWORD;
  BoxType: DWORD;
  SubCursor: Integer;
  SubBoxSize: DWORD;
  SubBoxType: DWORD;
  TrackId: DWORD;
  DecodeTime: UInt64;
  BaseDecodeTime: UInt64;
  Version: Byte;

begin

  Result := False;

  if (Length(AFragment) < 16) then
    Exit;

  MoofSize := ReadU32BE(AFragment,
                        0);

  if (MoofSize < 8) or
     (MoofSize > DWORD(Length(AFragment))) or
     (ReadU32BE(AFragment,
                4) <> BOX_MOOF) then
    Exit;

  Cursor := 8;

  while (Cursor + 8 <= Integer(MoofSize)) do
    begin
      BoxSize := ReadU32BE(AFragment,
                           Cursor);

      BoxType := ReadU32BE(AFragment,
                           Cursor + 4);

      if (BoxSize < 8) or
         (Cursor + Integer(BoxSize) > Integer(MoofSize)) then
        Exit;

      if (BoxType = $74726166) then // traf
        begin
          TrackId := 0;
          TrafEnd := Cursor + Integer(BoxSize);
          SubCursor := Cursor + 8;

          while (SubCursor + 8 <= TrafEnd) do
            begin
              SubBoxSize := ReadU32BE(AFragment,
                                      SubCursor);

              SubBoxType := ReadU32BE(AFragment,
                                      SubCursor + 4);

              if (SubBoxSize < 8) or
                 (SubCursor + Integer(SubBoxSize) > TrafEnd) then
                Exit;

              if (SubBoxType = $74666864) and // tfhd
                 (SubBoxSize >= 16) then
                TrackId := ReadU32BE(AFragment,
                                     SubCursor + 12)
              else if (SubBoxType = $74666474) and // tfdt
                      (TrackId <> 0) and
                      (SubBoxSize >= 16) then
                begin
                  Version := AFragment[SubCursor + 8];

                  if (Version = 1) then
                    begin
                      if SubBoxSize < 20 then
                        Exit;

                      DecodeTime := ReadU64BE(AFragment,
                                              SubCursor + 12);
                    end
                  else
                    DecodeTime := ReadU32BE(AFragment,
                                            SubCursor + 12);

                  if not FArchiveBaseDecodeTimes.TryGetValue(TrackId,
                                                             BaseDecodeTime) then
                    begin
                      BaseDecodeTime := DecodeTime;
                      FArchiveBaseDecodeTimes.Add(TrackId,
                                                  BaseDecodeTime);
                    end;

                  if (DecodeTime >= BaseDecodeTime) then
                    DecodeTime := DecodeTime - BaseDecodeTime
                  else
                    DecodeTime := 0;

                  if Version = 1 then
                    WriteU64BE(AFragment,
                               SubCursor + 12,
                               DecodeTime)
                  else
                    WriteU32BE(AFragment,
                               SubCursor + 12,
                               DWORD(DecodeTime));
                end;

              Inc(SubCursor,
                  Integer(SubBoxSize));
            end;
        end;

      Inc(Cursor,
          Integer(BoxSize));
    end;

  Result := True;
end;


procedure TSimpleFmp4ByteStream.QueueFragment(const AData: TBytes);
var
  Item: TSimpleFmp4Fragment;
  OldItem: TSimpleFmp4Fragment;

begin

  Item.Sequence := FNextSequence;
  Item.Data := Copy(AData,
                    0,
                    Length(AData));

  Inc(FNextSequence);

  while (FFragments.Count >= SIMPLE_FMP4_MAX_FRAGMENTS) do
    begin
      OldItem := FFragments.Dequeue();
      OldItem.Data := nil;
    end;

  FFragments.Enqueue(Item);

  Inc(FFragmentCount);
end;


constructor TSimpleFmp4ByteStream.Create(const AInner: IMFByteStream);
begin

  inherited Create;

  // The first interface assignment performs the initial AddRef.
  FRefCount := 0;
  FInner := AInner;
  FNullOutput := not Assigned(AInner);
  FNullPosition := 0;
  FNullLength := 0;
  FAsyncWriteSizes := TQueue<ULONG>.Create();
  FLock := TCriticalSection.Create;

  FParserBufferSize := 0;

  FFragments := TQueue<TSimpleFmp4Fragment>.Create();
  FArchiveFragments := TQueue<TBytes>.Create();
  FTrackDecodeTimes := TDictionary<DWORD, UInt64>.Create();
  FArchiveBaseDecodeTimes := TDictionary<DWORD, UInt64>.Create();

  FNextSequence := 1;
  FFragmentCount := 0;
  FTotalBytesObserved := 0;
end;


destructor TSimpleFmp4ByteStream.Destroy;
begin

  FInner := nil;

  FParserBuffer := nil;
  FInitSegment := nil;
  FPendingMoof := nil;

  FreeAndNil(FArchiveBaseDecodeTimes);
  FreeAndNil(FTrackDecodeTimes);
  FreeAndNil(FArchiveFragments);
  FreeAndNil(FFragments);
  FreeAndNil(FAsyncWriteSizes);
  FreeAndNil(FLock);

  inherited Destroy;
end;


function TSimpleFmp4ByteStream.QueryInterface(const IID: TGUID;
                                               out Obj): HResult;
begin

  // Answer for interfaces implemented by this wrapper first.
  if GetInterface(IID,
                  Obj) then
    Exit(S_OK);

  // Forward optional/private interfaces to the real MFCreateFile byte stream.
  // The fragmented MP4 sink can query such interfaces internally.
  Pointer(Obj) := nil;

  if Assigned(FInner) then
    Exit(FInner.QueryInterface(IID, Obj));

  Result := E_NOINTERFACE;
end;


function TSimpleFmp4ByteStream._AddRef: Integer;
begin

  Result := InterlockedIncrement(FRefCount);
end;


function TSimpleFmp4ByteStream._Release: Integer;
begin

  Result := InterlockedDecrement(FRefCount);

  if (Result = 0) then
    Destroy();
end;


procedure TSimpleFmp4ByteStream.ProcessBox(const ABoxType: DWORD;
                                           const ABoxData: TBytes);
var
  Fragment: TBytes;
  PatchedFragment: TBytes;

begin

  case ABoxType of
    BOX_FTYP,
    BOX_MOOV:
      begin
        AppendBytes(FInitSegment,
                    ABoxData);

        OutputDebugString(PChar(
          Format('MfSimpleWebCamStreamer: init box bytes=%d total init=%d',
                 [Length(ABoxData),
                  Length(FInitSegment)])));
      end;

    BOX_MOOF:
      begin
        FPendingMoof := Copy(ABoxData,
                             0,
                             Length(ABoxData));

        OutputDebugString(PChar(
          Format('MfSimpleWebCamStreamer: moof bytes=%d',
                 [Length(ABoxData)])));
      end;

    BOX_MDAT:
      begin
        if (Length(FPendingMoof) = 0) then
          Exit;

        System.SetLength(Fragment,
                  Length(FPendingMoof) + Length(ABoxData));

        Move(FPendingMoof[0],
             Fragment[0],
             Length(FPendingMoof));

        if (Length(ABoxData) > 0) then
          Move(ABoxData[0],
               Fragment[Length(FPendingMoof)],
               Length(ABoxData));

        if PatchFragmentForMse(Fragment,
                               PatchedFragment) then
          begin
            // The same self-contained fragment used by MSE is also the basis
            // for the rolling archive. Unlike the raw MF fragment it no longer
            // contains tfhd.base_data_offset values tied to the original
            // continuous byte-stream position.
            while FArchiveFragments.Count >= SIMPLE_FMP4_MAX_ARCHIVE_FRAGMENTS do
              FArchiveFragments.Dequeue();

            FArchiveFragments.Enqueue(Copy(PatchedFragment,
                                           0,
                                           Length(PatchedFragment)));

            QueueFragment(PatchedFragment);

            OutputDebugString(PChar(
              Format('MfSimpleWebCamStreamer: MSE fragment %d raw=%d patched=%d',
                     [FFragmentCount,
                      Length(Fragment),
                      Length(PatchedFragment)])));
          end
        else
          OutputDebugString(
            'MfSimpleWebCamStreamer: MSE fragment patch failed');

        FPendingMoof := nil;
      end;
  end;
end;


procedure TSimpleFmp4ByteStream.ObserveBytes(const AData: PByte;
                                             const ACount: ULONG);
var
  OldSize: Integer;
  BoxSize32: DWORD;
  BoxSize64: UInt64;
  BoxType: DWORD;
  HeaderSize: Integer;
  Remaining: Integer;
  BoxData: TBytes;

begin

  if (AData = nil) or
     (ACount = 0) then
    Exit;

  BoxSize64 := 0;

  FLock.Enter;

  try
    Inc(FTotalBytesObserved,
        ACount);

    OldSize := FParserBufferSize;

    Inc(FParserBufferSize,
        Integer(ACount));

    System.SetLength(FParserBuffer,
              FParserBufferSize);

    Move(AData^,
         FParserBuffer[OldSize],
         ACount);

    while (FParserBufferSize >= 8) do
      begin
        BoxSize32 := ReadU32BE(@FParserBuffer[0]);
        BoxType := ReadU32BE(@FParserBuffer[4]);
        HeaderSize := 8;

        if (BoxSize32 = 1) then
          begin
            if (FParserBufferSize < 16) then
              Break;

            BoxSize64 := ReadU64BE(@FParserBuffer[8]);
            HeaderSize := 16;
          end
        else
          BoxSize64 := BoxSize32;

        if (BoxSize64 = 0) or
           (BoxSize64 < UInt64(HeaderSize)) or
           (BoxSize64 > UInt64(MaxInt)) then
          begin
            // The observer must never interfere with the real byte stream.
            // If an unexpected top-level layout is seen, reset only our parser.
            FParserBuffer := nil;
            FParserBufferSize := 0;
            Exit;
          end;

        if (UInt64(FParserBufferSize) < BoxSize64) then
          Break;

        System.SetLength(BoxData,
                         Integer(BoxSize64));

        Move(FParserBuffer[0],
             BoxData[0],
             Integer(BoxSize64));

        ProcessBox(BoxType,
                   BoxData);

        Remaining := FParserBufferSize - Integer(BoxSize64);

        if (Remaining > 0) then
          Move(FParserBuffer[Integer(BoxSize64)],
               FParserBuffer[0],
               Remaining);

        FParserBufferSize := Remaining;

        System.SetLength(FParserBuffer,
                         FParserBufferSize);
      end;

  finally
    FLock.Leave;
  end;
end;


function TSimpleFmp4ByteStream.GetCapabilities(out pdwCapabilities: DWORD): HRESULT;
begin

  if FNullOutput then
    begin
      pdwCapabilities := $00000002 or $00000004; // WRITABLE | SEEKABLE
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetCapabilities(pdwCapabilities);
end;


function TSimpleFmp4ByteStream.GetLength(out pqwLength: UInt64): HRESULT;
begin

  if FNullOutput then
    begin
      pqwLength := FNullLength;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetLength(pqwLength);
end;


function TSimpleFmp4ByteStream.SetLength(qwLength: UInt64): HRESULT;
begin

  if FNullOutput then
    begin
      FNullLength := qwLength;

      if (FNullPosition > FNullLength) then
        FNullPosition := FNullLength;

      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.SetLength(qwLength);
end;


function TSimpleFmp4ByteStream.GetCurrentPosition(out pqwPosition: QWORD): HRESULT;
begin

  if FNullOutput then
    begin
      pqwPosition := FNullPosition;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetCurrentPosition(pqwPosition);
end;


function TSimpleFmp4ByteStream.SetCurrentPosition(const qwPosition: QWORD): HRESULT;
begin

  if FNullOutput then
    begin
      FNullPosition := qwPosition;

      if (FNullPosition > FNullLength) then
        FNullLength := FNullPosition;

      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.SetCurrentPosition(qwPosition);
end;


function TSimpleFmp4ByteStream.IsEndOfStream(out pfEndOfStream: BOOL): HRESULT;
begin

  if FNullOutput then
    begin
      pfEndOfStream := BOOL(0);
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.IsEndOfStream(pfEndOfStream);
end;


function TSimpleFmp4ByteStream.Read(pb: PByte;
                                    cb: ULONG;
                                    out pcbRead: ULONG): HRESULT;
begin

  pcbRead := 0;

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Read(pb,
                        cb,
                        pcbRead);
end;


function TSimpleFmp4ByteStream.BeginRead(pb: PByte;
                                         cb: ULONG;
                                         pCallback: IMFAsyncCallback;
                                         punkState: IUnknown): HRESULT;
begin

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.BeginRead(pb,
                             cb,
                             pCallback,
                             punkState);
end;


function TSimpleFmp4ByteStream.EndRead(pResult: IMFAsyncResult;
                                       out pcbRead: ULONG): HRESULT;
begin

  pcbRead := 0;

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.EndRead(pResult,
                           pcbRead);
end;


function TSimpleFmp4ByteStream.Write(pb: PByte;
                                     cb: ULONG;
                                     out pcbWritten: ULONG): HRESULT;
begin

  pcbWritten := 0;

  if FNullOutput then
    begin
      if (pb <> nil) and
         (cb > 0) then
        begin
          ObserveBytes(pb,
                       cb);

          FLock.Enter;
          try
            Inc(FNullPosition,
                cb);

            if (FNullPosition > FNullLength) then
              FNullLength := FNullPosition;
          finally
            FLock.Leave;
          end;
        end;

      pcbWritten := cb;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Write(pb,
                         cb,
                         pcbWritten);

  if SUCCEEDED(Result) and
     (pcbWritten > 0) then
    ObserveBytes(pb,
                 pcbWritten);
end;


function TSimpleFmp4ByteStream.BeginWrite(pb: PByte;
                                          cb: ULONG;
                                          pCallback: IMFAsyncCallback;
                                          punkState: IUnknown): HRESULT;
var
  AsyncResult: IMFAsyncResult;

begin

  if (pb <> nil) and
     (cb > 0) then
    ObserveBytes(pb,
                 cb);

  if FNullOutput then
    begin
      FLock.Enter;
      try
        Inc(FNullPosition,
            cb);

        if (FNullPosition > FNullLength) then
          FNullLength := FNullPosition;

        FAsyncWriteSizes.Enqueue(cb);
      finally
        FLock.Leave;
      end;

      Result := S_OK;

      if Assigned(pCallback) then
        begin
          AsyncResult := nil;

          Result := MFCreateAsyncResult(nil,
                                        pCallback,
                                        punkState,
                                        AsyncResult);

          if SUCCEEDED(Result) then
            Result := pCallback.Invoke(AsyncResult);
        end;

      Exit;
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.BeginWrite(pb,
                              cb,
                              pCallback,
                              punkState);
end;


function TSimpleFmp4ByteStream.EndWrite(pResult: IMFAsyncResult;
                                        out pcbWritten: ULONG): HRESULT;
begin

  pcbWritten := 0;

  if FNullOutput then
    begin

      FLock.Enter;

      try
        if FAsyncWriteSizes.Count > 0 then
          pcbWritten := FAsyncWriteSizes.Dequeue();
      finally
        FLock.Leave;
      end;

      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.EndWrite(pResult,
                            pcbWritten);
end;


function TSimpleFmp4ByteStream.Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                                    llSeekOffset: LONGLONG;
                                    dwSeekFlags: DWORD;
                                    out pqwCurrentPosition: UInt64): HRESULT;
var
  NewPosition: Int64;

begin

  if FNullOutput then
    begin
      if (Integer(SeekOrigin) = 0) then
        NewPosition := llSeekOffset
      else
        NewPosition := Int64(FNullPosition) + llSeekOffset;

      if (NewPosition < 0) then
        NewPosition := 0;

      FNullPosition := UInt64(NewPosition);

      if (FNullPosition > FNullLength) then
        FNullLength := FNullPosition;

      pqwCurrentPosition := FNullPosition;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Seek(SeekOrigin,
                        llSeekOffset,
                        dwSeekFlags,
                        pqwCurrentPosition);
end;


function TSimpleFmp4ByteStream.Flush(): HRESULT;
begin

  if FNullOutput then
    Exit(S_OK);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Flush;
end;


function TSimpleFmp4ByteStream.Close(): HRESULT;
begin

  if FNullOutput then
    begin

      FLock.Enter;

      try
        FAsyncWriteSizes.Clear();
      finally
        FLock.Leave;
      end;

      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Close;
end;


function TSimpleFmp4ByteStream.GetInitSegment(out ASegment: TBytes): Boolean;
begin

  ASegment := nil;

  FLock.Enter;
  try
    Result := Length(FInitSegment) > 0;

    if Result then
      ASegment := Copy(FInitSegment,
                       0,
                       Length(FInitSegment));
  finally
    FLock.Leave;
  end;
end;


function TSimpleFmp4ByteStream.GetFragment(const ASequence: UInt64;
                                            out AFragment: TBytes): Boolean;
var
  Items: TArray<TSimpleFmp4Fragment>;
  I: Integer;

begin

  AFragment := nil;

  FLock.Enter();

  try
    Items := FFragments.ToArray();
    Result := False;

    for I := 0 to Length(Items) - 1 do
      begin
        if (Items[I].Sequence = ASequence) then
          begin
            AFragment := Copy(Items[I].Data,
                              0,
                              Length(Items[I].Data));

            Result := Length(AFragment) > 0;
            Break;
          end;
      end;
  finally
    FLock.Leave();
  end;
end;


function TSimpleFmp4ByteStream.GetFragmentWindow(out AFirstSequence: UInt64;
                                                  out ALastSequence: UInt64;
                                                  out ACount: Integer): Boolean;
var
  Items: TArray<TSimpleFmp4Fragment>;

begin

  AFirstSequence := 0;
  ALastSequence := 0;
  ACount := 0;

  FLock.Enter();

  try
    ACount := FFragments.Count;
    Result := (ACount > 0);

    if Result then
      begin
        Items := FFragments.ToArray();
        AFirstSequence := Items[0].Sequence;
        ALastSequence := Items[Length(Items) - 1].Sequence;
      end;
  finally
    FLock.Leave();
  end;
end;


function TSimpleFmp4ByteStream.TryPopArchiveFragment(out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  FLock.Enter;

  try
    Result := (FArchiveFragments.Count > 0);

    if Result then
      begin
        AFragment := FArchiveFragments.Dequeue();

        Result := RebaseArchiveFragmentTfdt(AFragment);

        if not Result then
          AFragment := nil;
      end;
  finally
    FLock.Leave;
  end;
end;


procedure TSimpleFmp4ByteStream.ResetArchiveTimeline;
begin

  FLock.Enter;

  try
    FArchiveBaseDecodeTimes.Clear();
  finally
    FLock.Leave;
  end;
end;


function TSimpleFmp4ByteStream.GetDiagnostics(out AInitBytes: Integer;
                                              out ALastFragmentBytes: Integer;
                                              out AFragmentCount: UInt64;
                                              out ATotalBytesObserved: UInt64): Boolean;
var
  Items: TArray<TSimpleFmp4Fragment>;

begin

  FLock.Enter;

  try
    AInitBytes := Length(FInitSegment);

    if (FFragments.Count > 0) then
      begin
        Items := FFragments.ToArray();

        ALastFragmentBytes := Length(Items[Length(Items) - 1].Data);
      end
    else
      ALastFragmentBytes := 0;

    AFragmentCount := FFragmentCount;
    ATotalBytesObserved := FTotalBytesObserved;

    Result := True;
  finally
    FLock.Leave;
  end;
end;

end.
