// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Viewers.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: In-memory presence tracking, like MfWebCamStreamer viewers.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit FxServe.Viewers;

interface

uses
  System.SysUtils;

procedure TouchWebCamViewer(const ATarget, AAddress: string);
function WebCamViewerSnapshotJson(): UTF8String;

implementation

uses
  WinApi.Windows,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

const
  VIEWER_TIMEOUT_MS = 15000;

type
  TWebCamViewer = class
  public
    Id: string;
    Address: string;
    LastSeen: Cardinal;
  end;

  TWebCamViewerRegistry = class
  private
    FLock: TCriticalSection;
    FItems: TObjectDictionary<string, TWebCamViewer>;
    procedure RemoveExpired(const ANow: Cardinal);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Touch(const AId, AAddress: string);
    function SnapshotJson(): UTF8String;
  end;

var
  ViewerRegistry: TWebCamViewerRegistry;

function QueryValue(const ATarget, AName: string): string;
var
  QueryPos: Integer;
  Query: string;
  Parts: TStringList;
  I: Integer;
  EqualPos: Integer;
begin
  Result := '';
  QueryPos := Pos('?', ATarget);
  if QueryPos = 0 then
    Exit;

  Query := Copy(ATarget, QueryPos + 1, MaxInt);
  Parts := TStringList.Create();
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := '&';
    Parts.DelimitedText := Query;
    for I := 0 to Parts.Count - 1 do
      begin
        EqualPos := Pos('=', Parts[I]);
        if (EqualPos > 0) and
           SameText(Copy(Parts[I], 1, EqualPos - 1), AName) then
          Exit(Copy(Parts[I], EqualPos + 1, MaxInt));
      end;
  finally
    Parts.Free();
  end;
end;

function SafeViewerId(const AValue: string): string;
var
  I: Integer;
begin
  Result := '';
  if (AValue = '') or (Length(AValue) > 80) then
    Exit;

  for I := 1 to Length(AValue) do
    if not CharInSet(AValue[I], ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then
      Exit;

  Result := AValue;
end;

function MaskAddress(const AAddress: string): string;
var
  Dot: Integer;
  Colon: Integer;
begin
  Result := Trim(AAddress);
  Dot := LastDelimiter('.', Result);
  if Dot > 0 then
    Exit(Copy(Result, 1, Dot) + 'x');

  Colon := Pos(':', Result);
  if Colon > 0 then
    Result := Copy(Result, 1, Colon - 1) + ':…';

  if Result = '' then
    Result := 'unknown';
end;

function IsLoopbackAddress(const AAddress: string): Boolean;
begin
  Result := SameText(AAddress, '127.0.0.x') or
            SameText(AAddress, ':…');
end;

constructor TWebCamViewerRegistry.Create();
begin
  inherited Create();
  FLock := TCriticalSection.Create();
  FItems := TObjectDictionary<string, TWebCamViewer>.Create([doOwnsValues]);
end;

destructor TWebCamViewerRegistry.Destroy();
begin
  FItems.Free();
  FLock.Free();
  inherited Destroy();
end;

procedure TWebCamViewerRegistry.RemoveExpired(const ANow: Cardinal);
var
  Expired: TList<string>;
  Pair: TPair<string, TWebCamViewer>;
  Id: string;
begin
  Expired := TList<string>.Create();
  try
    for Pair in FItems do
      if Cardinal(ANow - Pair.Value.LastSeen) > VIEWER_TIMEOUT_MS then
        Expired.Add(Pair.Key);

    for Id in Expired do
      FItems.Remove(Id);
  finally
    Expired.Free();
  end;
end;

procedure TWebCamViewerRegistry.Touch(const AId, AAddress: string);
var
  Viewer: TWebCamViewer;
  NowTick: Cardinal;
  MaskedAddress: string;
begin
  NowTick := GetTickCount();
  MaskedAddress := MaskAddress(AAddress);
  FLock.Acquire();
  try
    RemoveExpired(NowTick);
    if not FItems.TryGetValue(AId, Viewer) then
      begin
        Viewer := TWebCamViewer.Create();
        Viewer.Id := AId;
        FItems.Add(AId, Viewer);
      end;
    if (Viewer.Address = '') or
       not IsLoopbackAddress(MaskedAddress) or
       IsLoopbackAddress(Viewer.Address) then
      Viewer.Address := MaskedAddress;
    Viewer.LastSeen := NowTick;
  finally
    FLock.Release();
  end;
end;

function TWebCamViewerRegistry.SnapshotJson(): UTF8String;
var
  Builder: TStringBuilder;
  Pair: TPair<string, TWebCamViewer>;
  NowTick: Cardinal;
  First: Boolean;
  AgeSeconds: Cardinal;
begin
  Builder := TStringBuilder.Create();
  FLock.Acquire();
  try
    NowTick := GetTickCount();
    RemoveExpired(NowTick);
    Builder.Append('{"count":');
    Builder.Append(FItems.Count);
    Builder.Append(',"viewers":[');
    First := True;
    for Pair in FItems do
      begin
        if not First then
          Builder.Append(',');
        First := False;
        AgeSeconds := Cardinal(NowTick - Pair.Value.LastSeen) div 1000;
        Builder.Append('{"ip":"');
        Builder.Append(Pair.Value.Address);
        Builder.Append('","ageSeconds":');
        Builder.Append(AgeSeconds);
        Builder.Append('}');
      end;
    Builder.Append(']}');
    Result := UTF8String(Builder.ToString());
  finally
    FLock.Release();
    Builder.Free();
  end;
end;

procedure TouchWebCamViewer(const ATarget, AAddress: string);
var
  ViewerId: string;
begin
  ViewerId := SafeViewerId(QueryValue(ATarget, 'viewer'));
  if ViewerId = '' then
    ViewerId := 'legacy-' + StringReplace(AAddress, ':', '-', [rfReplaceAll]);
  ViewerRegistry.Touch(ViewerId, AAddress);
end;

function WebCamViewerSnapshotJson(): UTF8String;
begin
  Result := ViewerRegistry.SnapshotJson();
end;

initialization
  ViewerRegistry := TWebCamViewerRegistry.Create();

finalization
  ViewerRegistry.Free();

end.
