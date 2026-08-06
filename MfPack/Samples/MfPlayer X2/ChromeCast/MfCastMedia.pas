// FactoryX
//
// Copyright ? FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastMedia.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Media inspection, per-device capability profiles, and automatic
//              route selection between direct playback, external subtitles, and transcoding.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// Source: Parts of CPlayer Examples
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfCastMedia;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TMfCastMediaInspector = class(TInterfacedObject, IMfCastMediaInspector)
  private
    FLogger: IMfCastLogger;

  public

    procedure SetLogger(const ALogger: IMfCastLogger);
    function Inspect(const ASourceName: string;
                     out AMediaInfo: TMfCastMediaInfo): HRESULT;
  end;


  TMfCastCapabilityResolver = class(TInterfacedObject,
                                    IMfCastCapabilityResolver)
  private
    FDefaultProfile: TMfCastDeviceProfile;
    FLogger: IMfCastLogger;

  public

    constructor Create(const ADefaultProfile: TMfCastDeviceProfile);

    procedure SetLogger(const ALogger: IMfCastLogger);
    function ResolveProfile(const ADevice: TMfCastDevice;
                            out AProfile: TMfCastDeviceProfile): HRESULT;
  end;

  TMfCastMediaPlanner = class(TInterfacedObject, IMfCastMediaPlanner)
  private

    FCapabilityResolver: IMfCastCapabilityResolver;
    FLogger: IMfCastLogger;

    function IsContentTypeAllowed(const AContentType: string;
                                  const AProfile: TMfCastDeviceProfile): Boolean;
    function IsSubtypeAllowed(const ASubtype: TGUID;
                              const AAllowed: TMfCastGuidArray): Boolean;
  public

    constructor Create(const ACapabilityResolver: IMfCastCapabilityResolver);

    procedure SetLogger(const ALogger: IMfCastLogger);
    function ChooseMode(const ADevice: TMfCastDevice;
                        const AMediaInfo: TMfCastMediaInfo;
                        const ARequestedMediaMode: TMfCastMediaMode;
                        const ARequestedSubtitleMode: TMfCastSubtitleMode;
                        out ASelectedMediaMode: TMfCastMediaMode;
                        out ASelectedSubtitleMode: TMfCastSubtitleMode): HRESULT;
  end;

implementation


function MfCastIsHttpSource(const ASourceName: string): Boolean;
begin

  Result := SameText(Copy(ASourceName, 1, 7), 'http://') or
            SameText(Copy(ASourceName, 1, 8), 'https://');
end;


function MfCastContainerFromExtension(const AExt: string): string;
begin

  if SameText(AExt, '.mp4') or SameText(AExt, '.m4v') or
     SameText(AExt, '.m4a') then
    Result := 'MP4'
  else if SameText(AExt, '.webm') then
    Result := 'WebM'
  else if SameText(AExt, '.mkv') then
    Result := 'Matroska'
  else if SameText(AExt, '.mov') then
    Result := 'QuickTime'
  else if SameText(AExt, '.mp3') then
    Result := 'MP3'
  else if SameText(AExt, '.aac') then
    Result := 'AAC'
  else if SameText(AExt, '.flac') then
    Result := 'FLAC'
  else if SameText(AExt, '.wav') then
    Result := 'WAV'
  else if SameText(AExt, '.avi') then
    Result := 'AVI'
  else if SameText(AExt, '.ts') or SameText(AExt, '.m2ts') then
    Result := 'MPEG-TS'
  else
    Result := '';
end;


function MfCastContentTypeFromExtension(const AExt: string): string;
begin

  if SameText(AExt, '.mp4') or SameText(AExt, '.m4v') then
    Result := 'video/mp4'
  else if SameText(AExt, '.m4a') then
    Result := 'audio/mp4'
  else if SameText(AExt, '.webm') then
    Result := 'video/webm'
  else if SameText(AExt, '.mkv') then
    Result := 'video/x-matroska'
  else if SameText(AExt, '.mov') then
    Result := 'video/quicktime'
  else if SameText(AExt, '.mp3') then
    Result := 'audio/mpeg'
  else if SameText(AExt, '.aac') then
    Result := 'audio/aac'
  else if SameText(AExt, '.flac') then
    Result := 'audio/flac'
  else if SameText(AExt, '.wav') then
    Result := 'audio/wav'
  else if SameText(AExt, '.avi') then
    Result := 'video/x-msvideo'
  else if SameText(AExt, '.ts') or SameText(AExt, '.m2ts') then
    Result := 'video/mp2t'
  else
    Result := 'application/octet-stream';
end;


function MfCastExtensionHasVideo(const AExt: string): Boolean;
begin

  Result := SameText(AExt, '.mp4') or SameText(AExt, '.m4v') or
            SameText(AExt, '.webm') or SameText(AExt, '.mkv') or
            SameText(AExt, '.mov') or SameText(AExt, '.avi') or
            SameText(AExt, '.ts') or SameText(AExt, '.m2ts');
end;


function MfCastExtensionHasAudio(const AExt: string): Boolean;
begin

  Result := MfCastExtensionHasVideo(AExt) or
            SameText(AExt, '.m4a') or SameText(AExt, '.mp3') or
            SameText(AExt, '.aac') or SameText(AExt, '.flac') or
            SameText(AExt, '.wav');
end;

procedure TMfCastMediaInspector.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastMediaInspector.Inspect(const ASourceName: string;
                                       out AMediaInfo: TMfCastMediaInfo): HRESULT;
var
  SourceName: string;
  Ext: string;
begin

  AMediaInfo.Reset();
  SourceName := Trim(ASourceName);
  AMediaInfo.SourceName := SourceName;

  if (SourceName = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if (not MfCastIsHttpSource(SourceName)) and (not FileExists(SourceName)) then
    begin
      Result := HRESULT($80070002);
      Exit;
    end;

  Ext := LowerCase(ExtractFileExt(SourceName));
  AMediaInfo.Title := ChangeFileExt(ExtractFileName(SourceName), '');
  AMediaInfo.ContainerName := MfCastContainerFromExtension(Ext);
  AMediaInfo.ContentType := MfCastContentTypeFromExtension(Ext);
  AMediaInfo.HasVideo := MfCastExtensionHasVideo(Ext);
  AMediaInfo.HasAudio := MfCastExtensionHasAudio(Ext);
  AMediaInfo.HasTimedText := False;
  AMediaInfo.IsLive := MfCastIsHttpSource(SourceName);
  AMediaInfo.IsSeekable := not AMediaInfo.IsLive;

  if (not AMediaInfo.HasVideo) and (not AMediaInfo.HasAudio) then
    AMediaInfo.HasVideo := True;

  Result := S_OK;
end;


constructor TMfCastCapabilityResolver.Create(const ADefaultProfile: TMfCastDeviceProfile);
begin

  inherited Create();

  FDefaultProfile := ADefaultProfile;
end;


procedure TMfCastCapabilityResolver.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastCapabilityResolver.ResolveProfile(const ADevice: TMfCastDevice;
                                                  out AProfile: TMfCastDeviceProfile): HRESULT;
begin

  AProfile := FDefaultProfile;
  Result := S_OK;
end;


constructor TMfCastMediaPlanner.Create(const ACapabilityResolver: IMfCastCapabilityResolver);
begin

  inherited Create();

  FCapabilityResolver := ACapabilityResolver;
end;


procedure TMfCastMediaPlanner.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastMediaPlanner.ChooseMode(const ADevice: TMfCastDevice;
                                        const AMediaInfo: TMfCastMediaInfo;
                                        const ARequestedMediaMode: TMfCastMediaMode;
                                        const ARequestedSubtitleMode: TMfCastSubtitleMode;
                                        out ASelectedMediaMode: TMfCastMediaMode;
                                        out ASelectedSubtitleMode: TMfCastSubtitleMode): HRESULT;
var
  Profile: TMfCastDeviceProfile;
  ContentAllowed: Boolean;
  VideoAllowed: Boolean;
  AudioAllowed: Boolean;

begin

  ASelectedMediaMode := ARequestedMediaMode;
  ASelectedSubtitleMode := ARequestedSubtitleMode;

  if (ARequestedMediaMode <> cmmAutomatic) then
    begin
      Result := S_OK;
      Exit;
    end;

  if not Assigned(FCapabilityResolver) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FCapabilityResolver.ResolveProfile(ADevice,
                                               Profile);
  if FAILED(Result) then
    Exit;

  ContentAllowed := IsContentTypeAllowed(AMediaInfo.ContentType,
                                         Profile);
  VideoAllowed := (not AMediaInfo.HasVideo) or
                  IsSubtypeAllowed(AMediaInfo.VideoSubtype,
                                   Profile.AllowedVideoSubtypes) or
                  (Profile.AllowUnknownFormats and
                   (Length(Profile.AllowedVideoSubtypes) = 0));
  AudioAllowed := (not AMediaInfo.HasAudio) or
                  IsSubtypeAllowed(AMediaInfo.AudioSubtype,
                                   Profile.AllowedAudioSubtypes) or
                  (Profile.AllowUnknownFormats and
                   (Length(Profile.AllowedAudioSubtypes) = 0));

  if ContentAllowed and VideoAllowed and AudioAllowed then
    begin
      if AMediaInfo.HasTimedText and
         (ARequestedSubtitleMode <> csmNone) and
         (ARequestedSubtitleMode <> csmBurnIntoVideo) then
        begin
          ASelectedMediaMode := cmmDirectWithTextTrack;
          ASelectedSubtitleMode := csmExternalTextTrack;
        end
      else
        begin
          ASelectedMediaMode := cmmDirectFile;
          if (ARequestedSubtitleMode = csmAutomatic) then
            ASelectedSubtitleMode := csmNone;
        end;
    end
  else
    begin

      ASelectedMediaMode := cmmTranscodeBurnedSubtitles;
      if AMediaInfo.HasTimedText and (ARequestedSubtitleMode <> csmNone) then
        ASelectedSubtitleMode := csmBurnIntoVideo
      else
        ASelectedSubtitleMode := csmNone;
    end;

  Result := S_OK;
end;


function TMfCastMediaPlanner.IsContentTypeAllowed(const AContentType: string;
                                                  const AProfile: TMfCastDeviceProfile): Boolean;
var
  I: Integer;

begin

  if (Length(AProfile.AllowedContentTypes) = 0) then
    begin
      Result := AProfile.AllowUnknownFormats;
      Exit;
    end;

  Result := False;

  for I := 0 to Length(AProfile.AllowedContentTypes) - 1 do
    begin
      if SameText(AContentType,
                  AProfile.AllowedContentTypes[I]) then
        begin
          Result := True;
          Exit;
        end;
    end;
end;


function TMfCastMediaPlanner.IsSubtypeAllowed(const ASubtype: TGUID;
                                              const AAllowed: TMfCastGuidArray): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 0 to Length(AAllowed) - 1 do
    begin
       if IsEqualGUID(ASubtype,
                      AAllowed[I]) then
         begin
           Result := True;
           Exit;
         end;
    end;
end;

end.

