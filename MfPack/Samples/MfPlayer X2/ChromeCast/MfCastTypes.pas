// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastTypes.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Shared enums, device information, media information,
//              load requests, status records, codec profiles, and all configurable settings.
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
unit MfCastTypes;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {WinApi}
  System.SysUtils,
  {WinApi}
  System.Classes,
  {MediaFoundation}
  WinApi.MediaFoundationApi.MfApi;

type

  TMfCastLogLevel = (cllTrace,
                     cllDebug,
                     cllInfo,
                     cllWarning,
                     cllError);

  TMfCastState = (csIdle,
                  csDiscovering,
                  csConnecting,
                  csConnected,
                  csLaunchingReceiver,
                  csPreparingMedia,
                  csBuffering,
                  csPlaying,
                  csPaused,
                  csStopping,
                  csStopped,
                  csError);

  TMfCastMediaMode = (cmmAutomatic,
                      cmmDirectFile,
                      cmmDirectWithTextTrack,
                      cmmTranscodeBurnedSubtitles);

  TMfCastSubtitleMode = (csmAutomatic,
                         csmNone,
                         csmExternalTextTrack,
                         csmBurnIntoVideo);

  TMfCastOutputMode = (comProgressiveMp4,
                       comFragmentedMp4,
                       comHlsFmp4);

  TMfCastStreamType = (cstBuffered,
                       cstLive,
                       cstNone);

  TMfCastTxtEntry = record
    Name: string;
    Value: string;
    procedure Reset();
  end;

  TMfCastTxtEntryArray = array of TMfCastTxtEntry;

  TMfCastDevice = record
    Id: string;
    ServiceInstance: string;
    FriendlyName: string;
    ModelName: string;
    HostName: string;
    Address: string;
    Port: Word;
    RawCapabilities: Cardinal;
    TxtEntries: TMfCastTxtEntryArray;
    LastSeenUtc: TDateTime;
    procedure Reset();
  end;

  TMfCastDeviceArray = array of TMfCastDevice;

  TMfCastSubtitleAsset = record
    Enabled: Boolean;
    ContentType: string;
    Language: string;
    Name: string;
    AspectRatio: Single;
    Data: TBytes;
    procedure Reset();
  end;

  TMfCastTrackInfo = record
    TrackId: Int64;
    TrackType: string;
    ContentId: string;
    ContentType: string;
    Name: string;
    Language: string;
    SubType: string;
    procedure Reset();
  end;

  TMfCastTrackInfoArray = array of TMfCastTrackInfo;
  TMfCastInt64Array = array of Int64;
  TMfCastGuidArray = array of TGUID;
  TMfCastStringArray = array of string;

  TMfCastMediaInfo = record
    SourceName: string;
    Title: string;
    ContentType: string;
    ContainerName: string;
    Duration100ns: Int64;
    IsLive: Boolean;
    IsSeekable: Boolean;
    HasVideo: Boolean;
    HasAudio: Boolean;
    HasTimedText: Boolean;
    VideoSubtype: TGUID;
    AudioSubtype: TGUID;
    VideoWidth: Cardinal;
    VideoHeight: Cardinal;
    FrameRateNumerator: Cardinal;
    FrameRateDenominator: Cardinal;
    VideoBitrate: Cardinal;
    AudioBitrate: Cardinal;
    AudioSampleRate: Cardinal;
    AudioChannels: Cardinal;
    procedure Reset();
  end;

  TMfCastDeviceProfile = record
    Name: string;
    AllowedContentTypes: TMfCastStringArray;
    AllowedVideoSubtypes: TMfCastGuidArray;
    AllowedAudioSubtypes: TMfCastGuidArray;
    MaxVideoWidth: Cardinal;
    MaxVideoHeight: Cardinal;
    MaxFrameRateNumerator: Cardinal;
    MaxFrameRateDenominator: Cardinal;
    MaxVideoBitrate: Cardinal;
    AllowUnknownFormats: Boolean;
    procedure Reset();
  end;

  TMfCastLoadRequest = record
    ContentId: string;
    ContentType: string;
    StreamType: TMfCastStreamType;
    Title: string;
    StartTime100ns: Int64;
    AutoPlay: Boolean;
    Tracks: TMfCastTrackInfoArray;
    ActiveTrackIds: TMfCastInt64Array;
    procedure Reset();
  end;

  TMfCastMediaStatus = record
    MediaSessionId: Int64;
    PlayerState: string;
    IdleReason: string;
    CurrentTime100ns: Int64;
    Duration100ns: Int64;
    Volume: Single;
    Muted: Boolean;
    procedure Reset();
  end;

  TMfCastErrorInfo = record
    HResult: HRESULT;
    Stage: string;
    MessageText: string;
    Detail: string;
    procedure Reset();
  end;

  TMfCastSimpleEvent = procedure of object;
  TMfCastDeviceEvent = procedure(const ADevice: TMfCastDevice) of object;
  TMfCastDeviceRemovedEvent = procedure(const ADeviceId: string) of object;
  TMfCastErrorEvent = procedure(const AError: TMfCastErrorInfo) of object;
  TMfCastStateChangedEvent = procedure(const AOldState: TMfCastState;
                                       const ANewState: TMfCastState) of object;
  TMfCastMediaStatusEvent = procedure(const AStatus: TMfCastMediaStatus) of object;
  TMfCastReceiverReadyEvent = procedure(const ASessionId: string;
                                        const ATransportId: string) of object;

  TMfCastDiscoveryCallbacks = record
    OnStarted: TMfCastSimpleEvent;
    OnStopped: TMfCastSimpleEvent;
    OnDeviceAdded: TMfCastDeviceEvent;
    OnDeviceUpdated: TMfCastDeviceEvent;
    OnDeviceRemoved: TMfCastDeviceRemovedEvent;
    OnError: TMfCastErrorEvent;
    procedure Reset();
  end;

  TMfCastChannelCallbacks = record
    OnReceiverReady: TMfCastReceiverReadyEvent;
    OnReceiverClosed: TMfCastSimpleEvent;
    OnMediaStatus: TMfCastMediaStatusEvent;
    OnError: TMfCastErrorEvent;
    procedure Reset();
  end;

  TMfCastControllerCallbacks = record
    OnDeviceAdded: TMfCastDeviceEvent;
    OnDeviceUpdated: TMfCastDeviceEvent;
    OnDeviceRemoved: TMfCastDeviceRemovedEvent;
    OnStateChanged: TMfCastStateChangedEvent;
    OnMediaStatus: TMfCastMediaStatusEvent;
    OnError: TMfCastErrorEvent;
    procedure Reset();
  end;

  TMfCastProtocolSettings = record
    DiscoveryServiceName: string;
    ControlPort: Word;
    ReceiverApplicationId: string;
    SenderId: string;
    ReceiverId: string;
    NamespaceConnection: string;
    NamespaceHeartbeat: string;
    NamespaceReceiver: string;
    NamespaceMedia: string;
    NamespaceDeviceAuth: string;
    ConnectTimeoutMs: Cardinal;
    ReadTimeoutMs: Cardinal;
    WriteTimeoutMs: Cardinal;
    ReceiverLaunchTimeoutMs: Cardinal;
    HeartbeatIntervalMs: Cardinal;
    HeartbeatTimeoutMs: Cardinal;
    VerifyTlsPeer: Boolean;
    TlsServerName: string;
    procedure Reset();
  end;

  TMfCastDiscoverySettings = record
    LocalInterfaceAddress: string;
    QueryTimeoutMs: Cardinal;
    ResponseWindowMs: Cardinal;
    RefreshIntervalMs: Cardinal;
    DeviceExpiryMs: Cardinal;
    IncludeIPv4: Boolean;
    IncludeIPv6: Boolean;
    procedure Reset();
  end;

  TMfCastHttpSettings = record
    BindAddress: string;
    AdvertisedAddress: string;
    ListenPort: Word;
    UseTls: Boolean;
    BasePath: string;
    ResourceTokenBytes: Cardinal;
    EnableCors: Boolean;
    AllowRangeRequests: Boolean;
    MaxConnections: Cardinal;
    HeaderTimeoutMs: Cardinal;
    ReadTimeoutMs: Cardinal;
    WriteTimeoutMs: Cardinal;
    IdleTimeoutMs: Cardinal;
    procedure Reset();
  end;

  TMfCastEncodingSettings = record
    OutputMode: TMfCastOutputMode;
    VideoSubtype: TGUID;
    AudioSubtype: TGUID;
    VideoBitrate: Cardinal;
    AudioBitrate: Cardinal;
    AudioSampleRate: Cardinal;
    AudioChannels: Cardinal;
    MaxWidth: Cardinal;
    MaxHeight: Cardinal;
    FrameRateNumerator: Cardinal;
    FrameRateDenominator: Cardinal;
    KeyFrameIntervalSeconds: Cardinal;
    SegmentDurationMs: Cardinal;
    FragmentDurationMs: Cardinal;
    UseHardwareTransforms: Boolean;
    LowLatency: Boolean;
    EnableLocalPreview: Boolean;
    procedure Reset();
  end;

  TMfCastSettings = record
    Protocol: TMfCastProtocolSettings;
    Discovery: TMfCastDiscoverySettings;
    Http: TMfCastHttpSettings;
    Encoding: TMfCastEncodingSettings;
    PreferredMediaMode: TMfCastMediaMode;
    PreferredSubtitleMode: TMfCastSubtitleMode;
    class function CreateDefault: TMfCastSettings; static;
  end;

  TMfCastTranscodeRequest = record
    SourceName: string;
    Title: string;
    StartTime100ns: Int64;
    StopTime100ns: Int64;
    SubtitleMode: TMfCastSubtitleMode;
    SubtitleSourceName: string;
    SubtitleLanguage: string;
    SubtitleAspectRatio: Single;
    SubtitleData: TBytes;
    Encoding: TMfCastEncodingSettings;
    procedure Reset();
  end;

  function MfCastStateToString(const AState: TMfCastState): string;
  function MfCastStreamTypeToString(const AStreamType: TMfCastStreamType): string;

implementation


procedure TMfCastTxtEntry.Reset();
begin

  Name := '';
  Value := '';
end;


procedure TMfCastDevice.Reset();
begin

  Id := '';
  ServiceInstance := '';
  FriendlyName := '';
  ModelName := '';
  HostName := '';
  Address := '';
  Port := 0;
  RawCapabilities := 0;
  SetLength(TxtEntries, 0);
  LastSeenUtc := 0;
end;


procedure TMfCastSubtitleAsset.Reset();
begin

  Enabled := False;
  ContentType := '';
  Language := '';
  Name := '';
  AspectRatio := 0.0;
  SetLength(Data, 0);
end;


procedure TMfCastTrackInfo.Reset();
begin

  TrackId := 0;
  TrackType := '';
  ContentId := '';
  ContentType := '';
  Name := '';
  Language := '';
  SubType := '';
end;


procedure TMfCastMediaInfo.Reset();
begin

  SourceName := '';
  Title := '';
  ContentType := '';
  ContainerName := '';
  Duration100ns := 0;
  IsLive := False;
  IsSeekable := False;
  HasVideo := False;
  HasAudio := False;
  HasTimedText := False;
  VideoSubtype := GUID_NULL;
  AudioSubtype := GUID_NULL;
  VideoWidth := 0;
  VideoHeight := 0;
  FrameRateNumerator := 0;
  FrameRateDenominator := 0;
  VideoBitrate := 0;
  AudioBitrate := 0;
  AudioSampleRate := 0;
  AudioChannels := 0;
end;


procedure TMfCastDeviceProfile.Reset();
begin

  Name := '';
  SetLength(AllowedContentTypes, 0);
  SetLength(AllowedVideoSubtypes, 0);
  SetLength(AllowedAudioSubtypes, 0);
  MaxVideoWidth := 0;
  MaxVideoHeight := 0;
  MaxFrameRateNumerator := 0;
  MaxFrameRateDenominator := 0;
  MaxVideoBitrate := 0;
  AllowUnknownFormats := False;
end;


procedure TMfCastLoadRequest.Reset();
begin

  ContentId := '';
  ContentType := '';
  StreamType := cstBuffered;
  Title := '';
  StartTime100ns := 0;
  AutoPlay := True;
  SetLength(Tracks, 0);
  SetLength(ActiveTrackIds, 0);
end;


procedure TMfCastMediaStatus.Reset();
begin

  MediaSessionId := 0;
  PlayerState := '';
  IdleReason := '';
  CurrentTime100ns := 0;
  Duration100ns := 0;
  Volume := 0;
  Muted := False;
end;


procedure TMfCastErrorInfo.Reset();
begin

  HResult := S_OK;
  Stage := '';
  MessageText := '';
  Detail := '';
end;


procedure TMfCastDiscoveryCallbacks.Reset();
begin

  OnStarted := nil;
  OnStopped := nil;
  OnDeviceAdded := nil;
  OnDeviceUpdated := nil;
  OnDeviceRemoved := nil;
  OnError := nil;
end;


procedure TMfCastChannelCallbacks.Reset();
begin

  OnReceiverReady := nil;
  OnReceiverClosed := nil;
  OnMediaStatus := nil;
  OnError := nil;
end;


procedure TMfCastControllerCallbacks.Reset();
begin

  OnDeviceAdded := nil;
  OnDeviceUpdated := nil;
  OnDeviceRemoved := nil;
  OnStateChanged := nil;
  OnMediaStatus := nil;
  OnError := nil;
end;


procedure TMfCastProtocolSettings.Reset();
begin

  DiscoveryServiceName := '';
  ControlPort := 0;
  ReceiverApplicationId := '';
  SenderId := '';
  ReceiverId := '';
  NamespaceConnection := '';
  NamespaceHeartbeat := '';
  NamespaceReceiver := '';
  NamespaceMedia := '';
  NamespaceDeviceAuth := '';
  ConnectTimeoutMs := 0;
  ReadTimeoutMs := 0;
  WriteTimeoutMs := 0;
  ReceiverLaunchTimeoutMs := 0;
  HeartbeatIntervalMs := 0;
  HeartbeatTimeoutMs := 0;
  VerifyTlsPeer := False;
  TlsServerName := '';
end;


procedure TMfCastDiscoverySettings.Reset();
begin

  LocalInterfaceAddress := '';
  QueryTimeoutMs := 0;
  ResponseWindowMs := 0;
  RefreshIntervalMs := 0;
  DeviceExpiryMs := 0;
  IncludeIPv4 := True;
  IncludeIPv6 := False;
end;


procedure TMfCastHttpSettings.Reset();
begin

  BindAddress := '';
  AdvertisedAddress := '';
  ListenPort := 0;
  UseTls := False;
  BasePath := '';
  ResourceTokenBytes := 0;
  EnableCors := True;
  AllowRangeRequests := True;
  MaxConnections := 0;
  HeaderTimeoutMs := 0;
  ReadTimeoutMs := 0;
  WriteTimeoutMs := 0;
  IdleTimeoutMs := 0;
end;


procedure TMfCastEncodingSettings.Reset();
begin

  OutputMode := comHlsFmp4;
  VideoSubtype := GUID_NULL;
  AudioSubtype := GUID_NULL;
  VideoBitrate := 0;
  AudioBitrate := 0;
  AudioSampleRate := 0;
  AudioChannels := 0;
  MaxWidth := 0;
  MaxHeight := 0;
  FrameRateNumerator := 0;
  FrameRateDenominator := 0;
  KeyFrameIntervalSeconds := 0;
  SegmentDurationMs := 0;
  FragmentDurationMs := 0;
  UseHardwareTransforms := True;
  LowLatency := False;
  EnableLocalPreview := True;
end;


class function TMfCastSettings.CreateDefault: TMfCastSettings;
begin

  Result.Protocol.Reset();
  Result.Discovery.Reset();
  Result.Http.Reset();
  Result.Encoding.Reset();

  Result.Protocol.DiscoveryServiceName := '_googlecast._tcp.local';
  Result.Protocol.ControlPort := 8009;
  Result.Protocol.ReceiverApplicationId := 'CC1AD845';
  Result.Protocol.SenderId := 'sender-0';
  Result.Protocol.ReceiverId := 'receiver-0';
  Result.Protocol.NamespaceConnection := 'urn:x-cast:com.google.cast.tp.connection';
  Result.Protocol.NamespaceHeartbeat := 'urn:x-cast:com.google.cast.tp.heartbeat';
  Result.Protocol.NamespaceReceiver := 'urn:x-cast:com.google.cast.receiver';
  Result.Protocol.NamespaceMedia := 'urn:x-cast:com.google.cast.media';
  Result.Protocol.NamespaceDeviceAuth := 'urn:x-cast:com.google.cast.tp.deviceauth';
  Result.Protocol.ConnectTimeoutMs := 5000;
  Result.Protocol.ReadTimeoutMs := 3000;
  Result.Protocol.WriteTimeoutMs := 5000;
  Result.Protocol.ReceiverLaunchTimeoutMs := 30000;
  Result.Protocol.HeartbeatIntervalMs := 5000;
  Result.Protocol.HeartbeatTimeoutMs := 15000;
  Result.Protocol.VerifyTlsPeer := False;

  Result.Discovery.QueryTimeoutMs := 3000;
  Result.Discovery.ResponseWindowMs := 1500;
  Result.Discovery.RefreshIntervalMs := 30000;
  Result.Discovery.DeviceExpiryMs := 120000;
  Result.Discovery.IncludeIPv4 := True;
  Result.Discovery.IncludeIPv6 := False;

  Result.Http.BindAddress := '0.0.0.0';
  Result.Http.AdvertisedAddress := '';
  Result.Http.ListenPort := 0;
  Result.Http.UseTls := False;
  Result.Http.BasePath := '/mfcast';
  Result.Http.ResourceTokenBytes := 16;
  Result.Http.EnableCors := True;
  Result.Http.AllowRangeRequests := True;
  Result.Http.MaxConnections := 8;
  Result.Http.HeaderTimeoutMs := 5000;
  Result.Http.ReadTimeoutMs := 15000;
  Result.Http.WriteTimeoutMs := 15000;
  Result.Http.IdleTimeoutMs := 30000;

  Result.Encoding.OutputMode := comHlsFmp4;
  Result.Encoding.VideoSubtype := MFVideoFormat_H264;
  Result.Encoding.AudioSubtype := MFAudioFormat_AAC;
  Result.Encoding.VideoBitrate := 4000000;
  Result.Encoding.AudioBitrate := 128000;
  Result.Encoding.AudioSampleRate := 48000;
  Result.Encoding.AudioChannels := 2;
  Result.Encoding.MaxWidth := 1920;
  Result.Encoding.MaxHeight := 1080;
  Result.Encoding.FrameRateNumerator := 30;
  Result.Encoding.FrameRateDenominator := 1;
  Result.Encoding.KeyFrameIntervalSeconds := 2;
  Result.Encoding.SegmentDurationMs := 4000;
  Result.Encoding.FragmentDurationMs := 1000;
  Result.Encoding.UseHardwareTransforms := True;
  Result.Encoding.LowLatency := False;
  Result.Encoding.EnableLocalPreview := True;

  Result.PreferredMediaMode := cmmAutomatic;
  Result.PreferredSubtitleMode := csmAutomatic;
end;


procedure TMfCastTranscodeRequest.Reset();
begin

  SourceName := '';
  Title := '';
  StartTime100ns := 0;
  StopTime100ns := 0;
  SubtitleMode := csmAutomatic;
  SubtitleSourceName := '';
  SubtitleLanguage := '';
  SubtitleAspectRatio := 0.0;
  SetLength(SubtitleData, 0);
  Encoding.Reset();
end;


function MfCastStateToString(const AState: TMfCastState): string;
begin

  case AState of
    csIdle: Result := 'Idle';
    csDiscovering: Result := 'Discovering';
    csConnecting: Result := 'Connecting';
    csConnected: Result := 'Connected';
    csLaunchingReceiver: Result := 'Launching receiver';
    csPreparingMedia: Result := 'Preparing media';
    csBuffering: Result := 'Buffering';
    csPlaying: Result := 'Playing';
    csPaused: Result := 'Paused';
    csStopping: Result := 'Stopping';
    csStopped: Result := 'Stopped';
    csError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;


function MfCastStreamTypeToString(const AStreamType: TMfCastStreamType): string;
begin

  case AStreamType of
    cstBuffered: Result := 'BUFFERED';
    cstLive: Result := 'LIVE';
    cstNone: Result := 'NONE';
  else
    Result := 'BUFFERED';
  end;
end;

end.
