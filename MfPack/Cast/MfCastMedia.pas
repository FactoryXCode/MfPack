// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
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
// Source: -
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
unit MfCastMedia;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  {Cast}
  MfCastTypes,
  MfCastInterfaces,
  MfEmbeddedSubtitleReader;

type

  TMfCastMediaInspector = class(TInterfacedObject, IMfCastMediaInspector)
  private
    FLogger: IMfCastLogger;

  public

    procedure SetLogger(const ALogger: IMfCastLogger);
    function Inspect(const ASourceName: string;
                     out AMediaInfo: TMfCastMediaInfo): HRESULT;
    function EnumerateTracks(const ASourceName: string;
                             out ATracks: TMfCastTrackInfoArray): HRESULT;
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


function MfCastCountSourceReaderStreams(
  const AReader: IMFSourceReader): DWORD;
const
  // Protect against media handlers which never report an invalid stream.
  MAX_SOURCE_READER_STREAMS = 1024;
var
  StreamIndex: DWORD;
  MediaType: IMFMediaType;
  Status: HRESULT;
begin
  Result := 0;
  if not Assigned(AReader) then
    Exit;

  StreamIndex := 0;
  while StreamIndex < MAX_SOURCE_READER_STREAMS do
    begin
      MediaType := nil;
      Status := AReader.GetNativeMediaType(StreamIndex,
                                           0,
                                           @MediaType);
      // Probe the same native media types the inspector consumes below. This
      // avoids GetStreamSelection, which can block in the Windows MKV source
      // when called from the cast worker's MTA apartment.
      if Status <> S_OK then
        Break;

      Inc(StreamIndex);
    end;

  Result := StreamIndex;
end;


procedure MfCastAppendTrack(var ATracks: TMfCastTrackInfoArray;
                            const ATrack: TMfCastTrackInfo);
var
  Index: Integer;

begin

  Index := Length(ATracks);
  SetLength(ATracks,
            Index + 1);
  ATracks[Index] := ATrack;
end;


function MfCastGetStreamString(const AReader: IMFSourceReader;
                               const AStreamIndex: DWORD;
                               const AKey: TGUID): string;
var
  Value: PROPVARIANT;

begin

  Result := '';
  PropVariantInit(Value);
  try
    if FAILED(AReader.GetPresentationAttribute(AStreamIndex,
                                               AKey,
                                               Value)) then
      Exit;

    case Value.vt of
      VT_LPWSTR: if Assigned(Value.pwszVal) then
                   Result := Value.pwszVal;
      VT_BSTR:   if Assigned(Value.bstrVal) then
                   Result := Value.bstrVal;
      VT_LPSTR:  if Assigned(Value.pszVal) then
                   Result := string(AnsiString(Value.pszVal));
    end;
  finally
    PropVariantClear(Value);
  end;
end;


function MfCastIsHttpSource(const ASourceName: string): Boolean;
begin

  Result := SameText(Copy(ASourceName,
                          1,
                          7),
                          'http://') or
            SameText(Copy(ASourceName,
                          1,
                          8),
                          'https://');
end;


function MfCastSourcePath(const ASourceName: string): string;
var
  DelimiterPos: Integer;

begin

  Result := ASourceName;

  DelimiterPos := Pos('?',
                      Result);
  if (DelimiterPos > 0) then
    Delete(Result,
           DelimiterPos,
           MaxInt);

  DelimiterPos := Pos('#',
                      Result);
  if (DelimiterPos > 0) then
    Delete(Result,
           DelimiterPos,
           MaxInt);
end;


function MfCastContainerFromExtension(const AExt: string): string;
begin

  if SameText(AExt,
              '.mp4') or
     SameText(AExt,
              '.m4v') or
     SameText(AExt,
              '.m4a') then
    Result := 'MP4'
  else
    if SameText(AExt,
                '.webm') then
      Result := 'WebM'
    else
      if SameText(AExt,
        '.mkv') then
        Result := 'Matroska'
      else
        if SameText(AExt,
                    '.mov') then
          Result := 'QuickTime'
        else
          if SameText(AExt,
                      '.mp3') then
            Result := 'MP3'
          else
            if SameText(AExt,
                        '.aac') then
              Result := 'AAC'
            else
              if SameText(AExt,
                          '.flac') then
                Result := 'FLAC'
              else
                if SameText(AExt,
                            '.wav') then
                  Result := 'WAV'
                else
                  if SameText(AExt,
                              '.ogg') or SameText(AExt,
                                                 '.oga') or SameText(AExt,
                                                                    '.opus') then
                    Result := 'Ogg'
                  else
                    if SameText(AExt,
                                '.avi') then
                      Result := 'AVI'
                    else
                      if SameText(AExt,
                                  '.ts') or SameText(AExt,
                                                     '.m2ts') then
                        Result := 'MPEG-TS'
                      else
                        Result := '';
end;


function MfCastContentTypeFromExtension(const AExt: string): string;
begin

  if SameText(AExt,
              '.mp4') or SameText(AExt,
                                  '.m4v') then
    Result := 'video/mp4'
  else
    if SameText(AExt,
                '.m4a') then
      Result := 'audio/mp4'
    else
      if SameText(AExt,
                  '.webm') then
        Result := 'video/webm'
      else
        if SameText(AExt,
          '.mkv') then
          Result := 'video/x-matroska'
        else
          if SameText(AExt,
                      '.mov') then
            Result := 'video/quicktime'
          else
            if SameText(AExt,
                        '.mp3') then
              Result := 'audio/mpeg'
            else
              if SameText(AExt,
                          '.aac') then
                Result := 'audio/aac'
              else
                if SameText(AExt,
                            '.flac') then
                  Result := 'audio/flac'
                else
                  if SameText(AExt,
                              '.wav') then
                    Result := 'audio/wav'
                  else
                    if SameText(AExt,
                                '.ogg') or SameText(AExt,
                                                   '.oga') or SameText(AExt,
                                                                      '.opus') then
                      Result := 'audio/ogg'
                    else
                      if SameText(AExt,
                                  '.avi') then
                        Result := 'video/x-msvideo'
                      else
                        if SameText(AExt,
                                    '.ts') or SameText(AExt,
                                                       '.m2ts') then
                          Result := 'video/mp2t'
                        else
                          Result := 'application/octet-stream';
end;


function MfCastExtensionHasVideo(const AExt: string): Boolean;
begin

  Result := SameText(AExt,
                     '.mp4') or SameText(AExt,
                                         '.m4v') or
            SameText(AExt,
                     '.webm') or SameText(AExt,
                                          '.mkv') or
            SameText(AExt,
                     '.mov') or SameText(AExt,
                                         '.avi') or
            SameText(AExt,
                     '.ts') or SameText(AExt,
                                        '.m2ts');
end;


function MfCastExtensionHasAudio(const AExt: string): Boolean;
begin

  Result := MfCastExtensionHasVideo(AExt) or
            SameText(AExt,
                     '.m4a') or SameText(AExt,
                                         '.mp3') or
            SameText(AExt,
                     '.aac') or SameText(AExt,
                                         '.flac') or
            SameText(AExt,
                     '.wav') or SameText(AExt,
                                         '.ogg') or
            SameText(AExt,
                     '.oga') or SameText(AExt,
                                         '.opus');
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
  Reader: IMFSourceReader;
  MediaType: IMFMediaType;
  MajorType: TGUID;
  Subtype: TGUID;
  StreamCount: DWORD;
  StreamIndex: DWORD;
  Value: UINT32;
  ActualVideo: Boolean;
  ActualAudio: Boolean;

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

  Ext := LowerCase(ExtractFileExt(MfCastSourcePath(SourceName)));
  AMediaInfo.Title := ChangeFileExt(ExtractFileName(MfCastSourcePath(SourceName)),
                                                    '');
  AMediaInfo.ContainerName := MfCastContainerFromExtension(Ext);
  AMediaInfo.ContentType := MfCastContentTypeFromExtension(Ext);
  AMediaInfo.HasVideo := MfCastExtensionHasVideo(Ext);
  AMediaInfo.HasAudio := MfCastExtensionHasAudio(Ext);
  AMediaInfo.HasTimedText := False;
  AMediaInfo.IsLive := MfCastIsHttpSource(SourceName);
  AMediaInfo.IsSeekable := not AMediaInfo.IsLive;

  if (not AMediaInfo.HasVideo) and (not AMediaInfo.HasAudio) then
    AMediaInfo.HasVideo := True;

  // Keep extension-based inspection as the tolerant baseline, but enrich it
  // with native compressed stream types when Media Foundation can open the
  // source. The planner needs this information to distinguish remuxing from
  // decoding and re-encoding.
  Reader := nil;
  try
    if Assigned(FLogger) then
      FLogger.Log(cllDebug,
                  'MediaInspector',
                  'Opening Media Foundation source reader for "' + SourceName + '".');
    Result := MFCreateSourceReaderFromURL(PWideChar(WideString(SourceName)),
                                          nil,
                                          Reader);
    if Assigned(FLogger) then
      FLogger.Log(cllDebug,
                  'MediaInspector',
                  Format('Media Foundation source reader returned HRESULT $%.8x.',
                         [DWORD(Result)]));
    if SUCCEEDED(Result) then
      begin
      ActualVideo := False;
      ActualAudio := False;
      StreamCount := MfCastCountSourceReaderStreams(Reader);
      if Assigned(FLogger) then
        FLogger.Log(cllDebug,
                    'MediaInspector',
                    Format('Source reader exposes %d stream(s).',
                           [StreamCount]));

      if (StreamCount > 0) then
      for StreamIndex := 0 to StreamCount - 1 do
        begin
          MediaType := nil;
          if FAILED(Reader.GetNativeMediaType(StreamIndex,
                                              0,
                                              @MediaType)) or
             (not Assigned(MediaType)) or
             FAILED(MediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                      MajorType)) or
             FAILED(MediaType.GetGUID(MF_MT_SUBTYPE,
                                      Subtype)) then
            Continue;

          if IsEqualGUID(MajorType,
                         MFMediaType_Video) and (not ActualVideo) then
            begin
              ActualVideo := True;
              AMediaInfo.VideoSubtype := Subtype;

              MFGetAttributeSize(MediaType,
                                 MF_MT_FRAME_SIZE,
                                 AMediaInfo.VideoWidth,
                                 AMediaInfo.VideoHeight);

              MFGetAttributeRatio(MediaType,
                                  MF_MT_FRAME_RATE,
                                  AMediaInfo.FrameRateNumerator,
                                  AMediaInfo.FrameRateDenominator);

              Value := 0;
              if SUCCEEDED(MediaType.GetUINT32(MF_MT_AVG_BITRATE,
                                               Value)) then
                AMediaInfo.VideoBitrate := Value;
            end
          else
            if IsEqualGUID(MajorType,
                           MFMediaType_Audio) and (not ActualAudio) then
            begin
              ActualAudio := True;
              AMediaInfo.AudioSubtype := Subtype;
              Value := 0;

              if SUCCEEDED(MediaType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                               Value)) then
                AMediaInfo.AudioSampleRate := Value;

              Value := 0;

              if SUCCEEDED(MediaType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                               Value)) then
                AMediaInfo.AudioChannels := Value;

              Value := 0;

              if SUCCEEDED(MediaType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                               Value)) then
                AMediaInfo.AudioBitrate := Value * 8;
            end;
        end;

        if ActualVideo or ActualAudio then
          begin
            AMediaInfo.HasVideo := ActualVideo;
            AMediaInfo.HasAudio := ActualAudio;
          end;
      end;
  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.Log(cllWarning,
                    'MediaInspector',
                    'Optional Media Foundation metadata enrichment failed: ' +
                    E.Message);
  end;

  MediaType := nil;
  Reader := nil;

  // Source-reader enrichment is optional. Extension-based inspection above
  // remains sufficient for choosing the safe transcode route.
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

  if Assigned(FCapabilityResolver) then
    FCapabilityResolver.SetLogger(ALogger);
end;


function TMfCastMediaInspector.EnumerateTracks(const ASourceName: string;
                                               out ATracks: TMfCastTrackInfoArray): HRESULT;
var
  EmbeddedTracks: TMfEmbeddedSubtitleTrackInfoArray;
  MajorType: TGUID;
  MediaType: IMFMediaType;
  Reader: IMFSourceReader;
  StreamIndex: DWORD;
  Track: TMfCastTrackInfo;
  I: Integer;
  Hr: HRESULT;
  Subtype: TGUID;
  IsSelected: Boolean;

begin

  SetLength(ATracks,
            0);

  if (Trim(ASourceName) = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Reader := nil;
  Result := MFCreateSourceReaderFromURL(PWideChar(WideString(ASourceName)),
                                        nil,
                                        Reader);
  if FAILED(Result) then
    Exit;

  StreamIndex := 0;

  while (StreamIndex < 256) do
    begin
      MediaType := nil;
      Hr := Reader.GetNativeMediaType(StreamIndex,
                                      0,
                                      @MediaType);

      if Hr = MF_E_INVALIDSTREAMNUMBER then
        Break;

      if SUCCEEDED(Hr) and Assigned(MediaType) and
         SUCCEEDED(MediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                     MajorType)) then
        begin
          Track.Reset();
          Track.Source := ctsMediaFoundation;
          Track.StreamIndex := StreamIndex;
          Track.Supported := True;

          Track.Language := MfCastGetStreamString(Reader,
                                                   StreamIndex,
                                                   MF_SD_LANGUAGE);

          Track.Name := MfCastGetStreamString(Reader,
                                               StreamIndex,
                                               MF_SD_STREAM_NAME);

          IsSelected := False;
          if SUCCEEDED(Reader.GetStreamSelection(StreamIndex,
                                                 IsSelected)) then
            Track.Selected := IsSelected;
          if SUCCEEDED(MediaType.GetGUID(MF_MT_SUBTYPE,
                                         Subtype)) then
            Track.SubType := GUIDToString(Subtype);

          if IsEqualGUID(MajorType,
                         MFMediaType_Video) then
            begin
              Track.Kind := ctkVideo;
              Track.TrackType := 'VIDEO';
              if Track.Name = '' then
                Track.Name := Format('Video track %d', [StreamIndex + 1]);
            end
          else
            if IsEqualGUID(MajorType,
                           MFMediaType_Audio) then
              begin
                Track.Kind := ctkAudio;
                Track.TrackType := 'AUDIO';
                if (Track.Name = '') then
                  Track.Name := Format('Audio track %d',
                                       [StreamIndex + 1]);
              end;

          if (Track.Kind <> ctkUnknown) then
            begin
              Track.TrackId := MfCastMakeTrackId(Track.Kind,
                                                 Track.Source,
                                                 Track.StreamIndex);
              MfCastAppendTrack(ATracks,
                                Track);
            end;
        end;
      Inc(StreamIndex);
    end;

  SetLength(EmbeddedTracks,
            0);

  Hr := TMfEmbeddedSubtitleReader.EnumerateTracks(ASourceName,
                                                  EmbeddedTracks);
  if SUCCEEDED(Hr) then
    for I := Low(EmbeddedTracks) to High(EmbeddedTracks) do
      begin
        Track.Reset();
        Track.Kind := ctkSubtitle;
        if (EmbeddedTracks[I].Source = essMatroska) then
          Track.Source := ctsMatroska
        else
          Track.Source := ctsMediaFoundation;

        Track.StreamIndex := EmbeddedTracks[I].StreamIndex;
        Track.Supported := EmbeddedTracks[I].Supported;
        Track.TrackType := 'TEXT';
        Track.Name := EmbeddedTracks[I].Name;
        Track.Language := EmbeddedTracks[I].Language;
        Track.ContentType := EmbeddedTracks[I].CodecId;
        Track.TrackId := MfCastMakeTrackId(Track.Kind,
                                           Track.Source,
                                           Track.StreamIndex);
        MfCastAppendTrack(ATracks,
                          Track);
      end;

  Result := S_OK;
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
  RemuxCompatible: Boolean;

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

  RemuxCompatible := SameText(AMediaInfo.ContainerName,
                              'Matroska') and
                     AMediaInfo.HasVideo and
                     IsEqualGUID(AMediaInfo.VideoSubtype,
                                 MFVideoFormat_H264) and
                     ((not AMediaInfo.HasAudio) or
                      IsEqualGUID(AMediaInfo.AudioSubtype,
                                  MFAudioFormat_AAC)) and
                     ((not AMediaInfo.HasTimedText) or
                      (ARequestedSubtitleMode = csmNone));

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
    if RemuxCompatible then
      begin
        ASelectedMediaMode := cmmRemuxFile;
        if (ARequestedSubtitleMode = csmAutomatic) then
          ASelectedSubtitleMode := csmNone;
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

