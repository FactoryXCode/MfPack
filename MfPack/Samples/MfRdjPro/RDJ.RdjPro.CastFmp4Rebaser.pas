// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Module: RDJ.RdjPro.CastFmp4Rebaser.pas
// Kind: Pascal Unit
// Language: ENU
//
// Description: Rebases live fMP4 track decode times for a Cast presentation.
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
//==============================================================================
unit RDJ.RdjPro.CastFmp4Rebaser;

interface

uses

  System.SysUtils,
  System.Generics.Collections;

type

  TRdjProCastFmp4Rebaser = class(TObject)
  private
    FTrackOrigins: TDictionary<Cardinal, UInt64>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Reset();
    function Rebase(const AFragment: TBytes;
                    out ARebasedFragment: TBytes): Boolean;
  end;

implementation

const

  BOX_MOOF = $6D6F6F66;
  BOX_TRAF = $74726166;
  BOX_TFHD = $74666864;
  BOX_TFDT = $74666474;


function ReadU32(const AData: TBytes;
                 const AOffset: Integer): Cardinal;
begin

  if (AOffset < 0) or (AOffset + 4 > Length(AData)) then
    Exit(0);
  Result := (Cardinal(AData[AOffset]) shl 24) or
            (Cardinal(AData[AOffset + 1]) shl 16) or
            (Cardinal(AData[AOffset + 2]) shl 8) or
             Cardinal(AData[AOffset + 3]);
end;


function ReadU64(const AData: TBytes;
                 const AOffset: Integer): UInt64;
begin

  Result := (UInt64(ReadU32(AData, AOffset)) shl 32) or
             UInt64(ReadU32(AData, AOffset + 4));
end;


procedure WriteU32(var AData: TBytes;
                   const AOffset: Integer;
                   const AValue: Cardinal);
begin

  if (AOffset < 0) or (AOffset + 4 > Length(AData)) then
    Exit;
  AData[AOffset] := Byte(AValue shr 24);
  AData[AOffset + 1] := Byte(AValue shr 16);
  AData[AOffset + 2] := Byte(AValue shr 8);
  AData[AOffset + 3] := Byte(AValue);
end;


procedure WriteU64(var AData: TBytes;
                   const AOffset: Integer;
                   const AValue: UInt64);
begin

  WriteU32(AData, AOffset, Cardinal(AValue shr 32));
  WriteU32(AData, AOffset + 4, Cardinal(AValue));
end;


function ValidBox(const AData: TBytes;
                  const AOffset,
                        AEnd: Integer;
                  out ASize,
                      AType: Cardinal): Boolean;
begin

  Result := False;
  if (AOffset < 0) or (AOffset + 8 > AEnd) or
     (AEnd > Length(AData)) then
    Exit;
  ASize := ReadU32(AData, AOffset);
  AType := ReadU32(AData, AOffset + 4);
  Result := (ASize >= 8) and
            (AOffset + Integer(ASize) <= AEnd);
end;


constructor TRdjProCastFmp4Rebaser.Create();
begin

  inherited Create();
  FTrackOrigins := TDictionary<Cardinal, UInt64>.Create();
end;


destructor TRdjProCastFmp4Rebaser.Destroy();
begin

  FTrackOrigins.Free();
  inherited Destroy();
end;


procedure TRdjProCastFmp4Rebaser.Reset();
begin

  FTrackOrigins.Clear();
end;


function TRdjProCastFmp4Rebaser.Rebase(const AFragment: TBytes;
                                       out ARebasedFragment: TBytes): Boolean;
var
  Cursor: Integer;
  Size: Cardinal;
  BoxType: Cardinal;
  MoofEnd: Integer;
  TrafCursor: Integer;
  TrafSize: Cardinal;
  TrafType: Cardinal;
  TrafEnd: Integer;
  ChildCursor: Integer;
  ChildSize: Cardinal;
  ChildType: Cardinal;
  TrackId: Cardinal;
  Version: Byte;
  DecodeTime: UInt64;
  Origin: UInt64;
  PatchedCount: Integer;

begin

  ARebasedFragment := Copy(AFragment, 0, Length(AFragment));
  Cursor := 0;
  PatchedCount := 0;

  while ValidBox(ARebasedFragment,
                 Cursor,
                 Length(ARebasedFragment),
                 Size,
                 BoxType) do
    begin
      if BoxType = BOX_MOOF then
        begin
          MoofEnd := Cursor + Integer(Size);
          TrafCursor := Cursor + 8;
          while ValidBox(ARebasedFragment,
                         TrafCursor,
                         MoofEnd,
                         TrafSize,
                         TrafType) do
            begin
              if TrafType = BOX_TRAF then
                begin
                  TrackId := 0;
                  TrafEnd := TrafCursor + Integer(TrafSize);
                  ChildCursor := TrafCursor + 8;
                  while ValidBox(ARebasedFragment,
                                 ChildCursor,
                                 TrafEnd,
                                 ChildSize,
                                 ChildType) do
                    begin
                      if (ChildType = BOX_TFHD) and (ChildSize >= 16) then
                        TrackId := ReadU32(ARebasedFragment,
                                           ChildCursor + 12)
                      else if (ChildType = BOX_TFDT) and
                              (TrackId <> 0) and
                              (ChildSize >= 16) then
                        begin
                          Version := ARebasedFragment[ChildCursor + 8];
                          if Version = 1 then
                            DecodeTime := ReadU64(ARebasedFragment,
                                                  ChildCursor + 12)
                          else
                            DecodeTime := ReadU32(ARebasedFragment,
                                                  ChildCursor + 12);

                          if not FTrackOrigins.TryGetValue(TrackId, Origin) then
                            begin
                              Origin := DecodeTime;
                              FTrackOrigins.Add(TrackId, Origin);
                            end;

                          if DecodeTime >= Origin then
                            DecodeTime := DecodeTime - Origin
                          else
                            DecodeTime := 0;

                          if Version = 1 then
                            WriteU64(ARebasedFragment,
                                     ChildCursor + 12,
                                     DecodeTime)
                          else
                            WriteU32(ARebasedFragment,
                                     ChildCursor + 12,
                                     Cardinal(DecodeTime));
                          Inc(PatchedCount);
                        end;

                      Inc(ChildCursor,
                          Integer(ChildSize));
                    end;
                end;

              Inc(TrafCursor,
                  Integer(TrafSize));
            end;
        end;

      Inc(Cursor,
          Integer(Size));
    end;

  Result := PatchedCount > 0;
end;

end.
