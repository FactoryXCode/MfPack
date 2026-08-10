// FactoryX
//
// Copyright Â© FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastController.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: GUI-independent orchestration of discovery, preparation,
//              HTTP publishing, connection, receiver launch, load, playback, and shutdown.
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
unit MfCastController;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  {Cast}
  MfCastTypes,
  MfCastInterfaces;

type

  TMfCastComponents = record
    Discovery: IMfCastDiscovery;
    Channel: IMfCastChannel;
    HttpServer: IMfCastHttpServer;
    MediaInspector: IMfCastMediaInspector;
    MediaPlanner: IMfCastMediaPlanner;
    SegmentPublisher: IMfCastSegmentPublisher;
    TranscodePipeline: IMfCastTranscodePipeline;
    PreviewSink: IMfCastPreviewSink;
    procedure Reset;
  end;

  TMfCastController = class(TInterfacedObject, IMfCastController)
  private
    FSettings: TMfCastSettings;
    FComponents: TMfCastComponents;
    FCallbacks: TMfCastControllerCallbacks;
    FLogger: IMfCastLogger;
    FState: TMfCastState;
    FCurrentDevice: TMfCastDevice;
    FCurrentMedia: TMfCastMediaInfo;
    FPendingLoadRequest: TMfCastLoadRequest;
    FPendingTranscodeRequest: TMfCastTranscodeRequest;
    FHasPendingTranscode: Boolean;
    FActiveLoadRequest: TMfCastLoadRequest;
    FActiveTranscodeRequest: TMfCastTranscodeRequest;
    FUsingTranscodedStream: Boolean;
    FCurrentPublishedPath: string;
    FCurrentSubtitlePublishedPath: string;
    FPlaybackTimeOffset100ns: Int64;
    FLastMediaPosition100ns: Int64;
    FMediaLoadStartTick: Cardinal;
    FMediaPlaybackStarted: Boolean;
    FSeekInProgress: Boolean;
    FReplacementLoadPending: Boolean;

    procedure Log(const ALevel: TMfCastLogLevel;
                  const AMessage: string);

    procedure CleanupCastAttempt();
    function RecreateControlChannel(): HRESULT;

    function FailCastAttempt(const AHResult: HRESULT;
                             const AStage: string;
                             const AMessage: string;
                             const ADetail: string = ''): HRESULT;

    function StartPendingMedia(): HRESULT;
    function CurrentFilePosition100ns(): Int64;
    procedure SetState(const AState: TMfCastState);

    procedure ReportError(const AHResult: HRESULT;
                          const AStage: string;
                          const AMessage: string;
                          const ADetail: string = '');

    function PrepareDirectFile(const ASourceName: string;
                               const ASubtitle: TMfCastSubtitleAsset;
                               const ASubtitleMode: TMfCastSubtitleMode;
                               out ALoadRequest: TMfCastLoadRequest): HRESULT;

    function PrepareTranscodedStream(const ASourceName: string;
                                     const ASubtitle: TMfCastSubtitleAsset;
                                     const ASubtitleMode: TMfCastSubtitleMode;
                                     out ALoadRequest: TMfCastLoadRequest): HRESULT;

    procedure DiscoveryStarted();
    procedure DiscoveryStopped();
    procedure DeviceAdded(const ADevice: TMfCastDevice);
    procedure DeviceUpdated(const ADevice: TMfCastDevice);
    procedure DeviceRemoved(const ADeviceId: string);
    procedure DiscoveryError(const AError: TMfCastErrorInfo);

    procedure ReceiverReady(const ASessionId: string;
                            const ATransportId: string);
    procedure ReceiverClosed();
    procedure ChannelMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure ChannelError(const AError: TMfCastErrorInfo);

  public

    constructor Create(const AComponents: TMfCastComponents);

    function Configure(const ASettings: TMfCastSettings): HRESULT;
    procedure SetCallbacks(const ACallbacks: TMfCastControllerCallbacks);
    function GetCallbacks(): TMfCastControllerCallbacks;
    procedure SetLogger(const ALogger: IMfCastLogger);
    function StartDiscovery: HRESULT;
    function StopDiscovery: HRESULT;
    function RefreshDiscovery: HRESULT;
    function GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
    function GetMediaTracks(const ASourceName: string;
                            out ATracks: TMfCastTrackInfoArray): HRESULT;

    function CastFile(const ADevice: TMfCastDevice;
                      const ASourceName: string;
                      const ASubtitle: TMfCastSubtitleAsset;
                      const AMediaMode: TMfCastMediaMode;
                      const ASubtitleMode: TMfCastSubtitleMode;
                      const AStartTime100ns: Int64 = 0): HRESULT;

    function Play: HRESULT;
    function Pause: HRESULT;
    function Stop: HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SelectAudioTrack(const ATrackId: Int64): HRESULT;
    function SelectSubtitleTrack(const ATrackId: Int64): HRESULT;
    function SelectSubtitle(const ASubtitle: TMfCastSubtitleAsset): HRESULT;
    function DisableSubtitles(): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function Disconnect: HRESULT;
    function GetState: TMfCastState;
    function GetHttpRequestCount(): Cardinal;
  end;


implementation

uses
  MfCastHttpServer,
  MfCastChannel,
  MfCastTransport;


function MfCastIsHttpSource(const ASourceName: string): Boolean;
begin

  Result := SameText(Copy(Trim(ASourceName), 1, 7), 'http://') or
            SameText(Copy(Trim(ASourceName), 1, 8), 'https://');
end;


function MfCastIsConnectionClosed(const AHResult: HRESULT): Boolean;
begin

  Result := (AHResult = HRESULT_FROM_WIN32(WSAECONNABORTED)) or
            (AHResult = HRESULT_FROM_WIN32(WSAECONNRESET)) or
            (AHResult = HRESULT_FROM_WIN32(WSAENOTCONN)) or
            (AHResult = HRESULT_FROM_WIN32(WSAESHUTDOWN));
end;


function MfCastIsLaunchRetryable(const AHResult: HRESULT): Boolean;
begin

  Result := (AHResult = E_UNEXPECTED) or
            (AHResult = HRESULT_FROM_WIN32(ERROR_TIMEOUT)) or
            MfCastIsConnectionClosed(AHResult);
end;


function MfCastAddressToString(const AAddress: TInAddr): string;
begin

  Result := Format('%d.%d.%d.%d',
                   [Integer(AAddress.S_un_b.s_b1),
                    Integer(AAddress.S_un_b.s_b2),
                    Integer(AAddress.S_un_b.s_b3),
                    Integer(AAddress.S_un_b.s_b4)]);
end;


function MfCastResolveHostIPv4(const AHost: string;
                               out AAddress: TInAddr): Boolean;
var
  HostAnsi: AnsiString;
  HostEntry: PHostEnt;

begin

  Result := False;
  FillChar(AAddress, SizeOf(AAddress), 0);
  HostAnsi := AnsiString(Trim(AHost));
  if (HostAnsi = '') then
    Exit;

  AAddress.S_addr := inet_addr(PAnsiChar(HostAnsi));
  if AAddress.S_addr <> u_long(INADDR_NONE) then
    begin
      Result := True;
      Exit;
    end;

  HostEntry := gethostbyname(PAnsiChar(HostAnsi));
  if Assigned(HostEntry) and Assigned(HostEntry^.h_addr_list[0]) then
    begin
      AAddress := PInAddr(HostEntry^.h_addr_list[0])^;
      Result := True;
    end;
end;


function MfCastResolveLocalIPv4ForPeer(const APeerHost: string;
                                       const APeerPort: Word;
                                       out ALocalAddress: string): Boolean;
var
  WsaData: TWSAData;
  Sock: TSocket;
  PeerAddr: TSockAddrIn;
  LocalAddr: TSockAddrIn;
  LocalSize: Integer;
  PeerInAddr: TInAddr;
  Port: Word;

begin

  Result := False;
  ALocalAddress := '';
  Sock := INVALID_SOCKET;

  if not MfCastResolveHostIPv4(APeerHost,
                               PeerInAddr) then
    Exit;

  if (WSAStartup($0202,
                 WsaData) <> 0) then
    Exit;

  try
    Sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if Sock = INVALID_SOCKET then
      Exit;

    Port := APeerPort;
    if (Port = 0) then
      Port := 8009;

    FillChar(PeerAddr,
             SizeOf(PeerAddr),
             0);

    PeerAddr.sin_family := AF_INET;
    PeerAddr.sin_port := htons(Port);
    PeerAddr.sin_addr := PeerInAddr;

    if (WinApi.WinSock.connect(Sock,
                               TSockAddr(PeerAddr),
                               SizeOf(PeerAddr)) = SOCKET_ERROR) then
      Exit;

    FillChar(LocalAddr,
             SizeOf(LocalAddr),
             0);

    LocalSize := SizeOf(LocalAddr);
    if (getsockname(Sock,
                   TSockAddr(LocalAddr),
                   LocalSize) = SOCKET_ERROR) then
      Exit;

    if (LocalAddr.sin_addr.S_addr = 0) or
       (Integer(LocalAddr.sin_addr.S_un_b.s_b1) = 127) then
      Exit;

    ALocalAddress := MfCastAddressToString(LocalAddr.sin_addr);
    Result := (ALocalAddress <> '');

  finally
    if (Sock <> INVALID_SOCKET) then
      WinApi.WinSock.closesocket(Sock);
    WSACleanup();
  end;
end;


procedure TMfCastComponents.Reset;
begin

  Discovery := nil;
  Channel := nil;
  HttpServer := nil;
  MediaInspector := nil;
  MediaPlanner := nil;
  SegmentPublisher := nil;
  TranscodePipeline := nil;
  PreviewSink := nil;
end;


constructor TMfCastController.Create(const AComponents: TMfCastComponents);
var
  DiscoveryCallbacks: TMfCastDiscoveryCallbacks;
  ChannelCallbacks: TMfCastChannelCallbacks;

begin

  inherited Create();

  FComponents := AComponents;
  FState := csIdle;
  FCallbacks.Reset;
  FCurrentDevice.Reset;
  FCurrentMedia.Reset;
  FPendingLoadRequest.Reset;
  FPendingTranscodeRequest.Reset;
  FHasPendingTranscode := False;
  FActiveLoadRequest.Reset;
  FActiveTranscodeRequest.Reset;
  FUsingTranscodedStream := False;
  FPlaybackTimeOffset100ns := 0;
  FLastMediaPosition100ns := 0;
  FMediaLoadStartTick := 0;
  FMediaPlaybackStarted := False;
  FSeekInProgress := False;
  FReplacementLoadPending := False;

  DiscoveryCallbacks.Reset;
  DiscoveryCallbacks.OnStarted := DiscoveryStarted;
  DiscoveryCallbacks.OnStopped := DiscoveryStopped;
  DiscoveryCallbacks.OnDeviceAdded := DeviceAdded;
  DiscoveryCallbacks.OnDeviceUpdated := DeviceUpdated;
  DiscoveryCallbacks.OnDeviceRemoved := DeviceRemoved;
  DiscoveryCallbacks.OnError := DiscoveryError;

  if Assigned(FComponents.Discovery) then
    FComponents.Discovery.SetCallbacks(DiscoveryCallbacks);

  ChannelCallbacks.Reset;
  ChannelCallbacks.OnReceiverReady := ReceiverReady;
  ChannelCallbacks.OnReceiverClosed := ReceiverClosed;
  ChannelCallbacks.OnMediaStatus := ChannelMediaStatus;
  ChannelCallbacks.OnError := ChannelError;

  if Assigned(FComponents.Channel) then
    FComponents.Channel.SetCallbacks(ChannelCallbacks);
end;


procedure TMfCastController.CleanupCastAttempt();
var
  StopResult: HRESULT;

begin

  // Stop the receiver while both the control channel and media URL are still
  // valid. Keep TLS connected until the HTTP client has also been closed.
  if Assigned(FComponents.Channel) then
    begin
      StopResult := FComponents.Channel.Stop();
      OutputDebugString(PChar(Format('MfCast receiver STOP hr=%.8x',
                                    [DWORD(StopResult)])));
    end;

  // Abort the publisher first so a transcoder blocked in a byte-stream write
  // is released before Stop waits for its worker thread.
  if Assigned(FComponents.SegmentPublisher) then
    FComponents.SegmentPublisher.AbortPresentation(E_ABORT);

  if Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Stop();

  if Assigned(FComponents.HttpServer) and
     (FCurrentSubtitlePublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentSubtitlePublishedPath);

  if Assigned(FComponents.HttpServer) and
     (FCurrentPublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentPublishedPath);

  if Assigned(FComponents.HttpServer) then
    FComponents.HttpServer.Stop();

  if Assigned(FComponents.Channel) then
    FComponents.Channel.Disconnect();

  FCurrentPublishedPath := '';
  FCurrentSubtitlePublishedPath := '';
  FPendingLoadRequest.Reset();
  FPendingTranscodeRequest.Reset();
  FHasPendingTranscode := False;
  FActiveLoadRequest.Reset();
  FActiveTranscodeRequest.Reset();
  FUsingTranscodedStream := False;
  FPlaybackTimeOffset100ns := 0;
  FLastMediaPosition100ns := 0;
  FMediaLoadStartTick := 0;
  FMediaPlaybackStarted := False;
  FSeekInProgress := False;
  FReplacementLoadPending := False;
  FCurrentDevice.Reset();
  FCurrentMedia.Reset();
end;


function TMfCastController.FailCastAttempt(
  const AHResult: HRESULT;
  const AStage: string;
  const AMessage: string;
  const ADetail: string): HRESULT;
var
  ErrorHr: HRESULT;

begin

  ErrorHr := AHResult;

  // S_FALSE is numerically successful, but the Cast operation did not
  // complete.
  if (ErrorHr = S_FALSE) then
    ErrorHr := HRESULT_FROM_WIN32(ERROR_TIMEOUT);

  CleanupCastAttempt();

  ReportError(ErrorHr,
              AStage,
              AMessage,
              ADetail);

  Result := ErrorHr;
end;


procedure TMfCastController.Log(const ALevel: TMfCastLogLevel;
                                const AMessage: string);
begin

  if Assigned(FLogger) then
    FLogger.Log(ALevel,
                'Controller',
                AMessage);
end;


function TMfCastController.Configure(const ASettings: TMfCastSettings): HRESULT;
begin

  if (FState <> csIdle) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  FSettings := ASettings;

  if Assigned(FComponents.Discovery) then
    begin
      Result := FComponents.Discovery.Configure(ASettings.Protocol,
                                                ASettings.Discovery);
      if FAILED(Result) then
        Exit;
    end;

  if Assigned(FComponents.Channel) then
    begin
      Result := FComponents.Channel.Configure(ASettings.Protocol);
      if FAILED(Result) then
        Exit;
    end;

  if Assigned(FComponents.HttpServer) then
    begin
      Result := FComponents.HttpServer.Configure(ASettings.Http);
      if FAILED(Result) then
        Exit;
    end;

  if Assigned(FComponents.TranscodePipeline) then
    begin
      Result := FComponents.TranscodePipeline.Configure(ASettings.Encoding);
      if FAILED(Result) then
        Exit;
    end;

  Result := S_OK;
end;


procedure TMfCastController.SetCallbacks(const ACallbacks: TMfCastControllerCallbacks);
begin

  FCallbacks := ACallbacks;
end;


function TMfCastController.GetCallbacks(): TMfCastControllerCallbacks;
begin

  Result := FCallbacks;
end;


procedure TMfCastController.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;

  if Assigned(FComponents.Discovery) then
    FComponents.Discovery.SetLogger(ALogger);

  if Assigned(FComponents.Channel) then
    FComponents.Channel.SetLogger(ALogger);

  if Assigned(FComponents.HttpServer) then
    FComponents.HttpServer.SetLogger(ALogger);

  if Assigned(FComponents.MediaInspector) then
    FComponents.MediaInspector.SetLogger(ALogger);

  if Assigned(FComponents.MediaPlanner) then
    FComponents.MediaPlanner.SetLogger(ALogger);

  if Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.SetLogger(ALogger);
end;


function TMfCastController.StartDiscovery(): HRESULT;
begin

  if not Assigned(FComponents.Discovery) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FComponents.Discovery.Start;
end;


function TMfCastController.StopDiscovery(): HRESULT;
begin

  if not Assigned(FComponents.Discovery) then
    begin
      Result := S_OK;
      Exit;
    end;

  Result := FComponents.Discovery.Stop;
end;


function TMfCastController.RefreshDiscovery(): HRESULT;
begin

  if not Assigned(FComponents.Discovery) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not FComponents.Discovery.IsRunning then
    Result := FComponents.Discovery.Start
  else
    Result := FComponents.Discovery.Refresh;
end;


function TMfCastController.GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
begin

  if not Assigned(FComponents.Discovery) then
    begin
      SetLength(ADevices,
                0);
      Result := E_POINTER;
      Exit;
  end;

  Result := FComponents.Discovery.GetDevices(ADevices);
end;


function TMfCastController.StartPendingMedia(): HRESULT;
var
  hr: HRESULT;

begin

  if FHasPendingTranscode then
    begin
      if not Assigned(FComponents.TranscodePipeline) or
         not Assigned(FComponents.SegmentPublisher) then
        begin
          Result := FailCastAttempt(E_POINTER,
                                    'Start transcoder',
                                    'The Chromecast transcoding pipeline is not available.');
          Exit;
        end;

      hr := FComponents.TranscodePipeline.Start(FPendingTranscodeRequest,
                                                FComponents.SegmentPublisher,
                                                FComponents.PreviewSink);
      if (hr <> S_OK) then
        begin
          Result := FailCastAttempt(hr,
                                    'Start transcoder',
                                    'The Chromecast transcoding pipeline could not be started.');
          Exit;
        end;

      FActiveTranscodeRequest := FPendingTranscodeRequest;
      FUsingTranscodedStream := True;
      FHasPendingTranscode := False;
      FPendingTranscodeRequest.Reset();
    end;

  SetState(csConnected);
  SetState(csBuffering);

  FMediaLoadStartTick := GetTickCount();
  FMediaPlaybackStarted := False;

  Result := FComponents.Channel.LoadMedia(FPendingLoadRequest);

  if (Result = S_OK) then
    FActiveLoadRequest := FPendingLoadRequest;

  if (Result <> S_OK) then
    Result := FailCastAttempt(Result,
                              'Load media',
                              'The Chromecast receiver rejected the media load request.');
end;


function TMfCastController.CurrentFilePosition100ns(): Int64;
begin

  Result := FLastMediaPosition100ns;
  if Result < 0 then
    Result := 0;

  // Receiver time starts at zero after a transcoded seek. Normally the media
  // status callback has already added this offset. Keep track switching safe
  // when a raw receiver status arrived while the replacement LOAD was active.
  if (FPlaybackTimeOffset100ns > 0) and
     (Result < FPlaybackTimeOffset100ns) then
    begin
      Log(cllDebug,
          Format('Normalizing receiver position %.3f to source position %.3f seconds.',
                 [Result / 10000000.0,
                  (Result + FPlaybackTimeOffset100ns) / 10000000.0]));
      Inc(Result,
          FPlaybackTimeOffset100ns);
    end;
end;


function TMfCastController.CastFile(const ADevice: TMfCastDevice;
                                    const ASourceName: string;
                                    const ASubtitle: TMfCastSubtitleAsset;
                                    const AMediaMode: TMfCastMediaMode;
                                    const ASubtitleMode: TMfCastSubtitleMode;
                                    const AStartTime100ns: Int64): HRESULT;
var
  SelectedMediaMode: TMfCastMediaMode;
  SelectedSubtitleMode: TMfCastSubtitleMode;
  HttpSettings: TMfCastHttpSettings;
  DeviceHost: string;
  DevicePort: Word;
  AdvertisedAddress: string;
  StartTime100ns: Int64;

begin

  Log(cllInfo,
      Format('Cast requested: device="%s" address=%s:%d source="%s"',
             [ADevice.FriendlyName,
              ADevice.Address,
              ADevice.Port,
              ASourceName]));

  StartTime100ns := AStartTime100ns;
  if (StartTime100ns < 0) then
    StartTime100ns := 0;

  if (Trim(ASourceName) = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if FState in [csConnecting,
                csConnected,
                csLaunchingReceiver,
                csPreparingMedia,
                csBuffering,
                csPlaying,
                csPaused,
                csStopping] then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_BUSY);
      Exit;
    end;

  if not Assigned(FComponents.MediaInspector) or
     not Assigned(FComponents.MediaPlanner) or
     not Assigned(FComponents.Channel) or
     not Assigned(FComponents.HttpServer) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if FState in [csError, csStopped] then
    CleanupCastAttempt();

  FCurrentDevice := ADevice;
  FPendingLoadRequest.Reset;
  FPendingTranscodeRequest.Reset;
  FHasPendingTranscode := False;
  FActiveLoadRequest.Reset;
  FActiveTranscodeRequest.Reset;
  FUsingTranscodedStream := False;
  FPlaybackTimeOffset100ns := 0;
  FMediaLoadStartTick := 0;
  FMediaPlaybackStarted := False;
  FSeekInProgress := False;
  FReplacementLoadPending := False;
  SetState(csPreparingMedia);

  Result := FComponents.MediaInspector.Inspect(ASourceName,
                                               FCurrentMedia);
  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Inspect media',
                                'The media source could not be inspected.');
      Exit;
    end;

  Log(cllDebug,
      Format('Media inspected: contentType=%s container=%s video=%s audio=%s seekable=%s',
             [FCurrentMedia.ContentType,
              FCurrentMedia.ContainerName,
              BoolToStr(FCurrentMedia.HasVideo, True),
              BoolToStr(FCurrentMedia.HasAudio, True),
              BoolToStr(FCurrentMedia.IsSeekable, True)]));

  FCurrentMedia.HasTimedText := ASubtitle.Enabled and
                                (ASubtitle.Embedded or
                                 (Trim(ASubtitle.SourceName) <> '') or
                                 (Length(ASubtitle.Data) > 0));

  if (ASubtitleMode in [csmExternalTextTrack, csmBurnIntoVideo]) and
     (not FCurrentMedia.HasTimedText) then
    begin
      Result := FailCastAttempt(E_INVALIDARG,
                                'Prepare subtitles',
                                'The requested subtitle mode has no active subtitle data.');
      Exit;
    end;

  Result := FComponents.MediaPlanner.ChooseMode(ADevice,
                                                FCurrentMedia,
                                                AMediaMode,
                                                ASubtitleMode,
                                                SelectedMediaMode,
                                                SelectedSubtitleMode);
  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Choose media mode',
                                'A suitable Chromecast media route could not be selected.');
      Exit;
    end;

  Log(cllInfo,
      Format('Media route selected: media=%s subtitles=%s',
             [MfCastMediaModeToString(SelectedMediaMode),
              MfCastSubtitleModeToString(SelectedSubtitleMode)]));

  if (SelectedMediaMode = cmmTranscodeBurnedSubtitles) and
     ((not Assigned(FComponents.TranscodePipeline)) or
      (not Assigned(FComponents.SegmentPublisher))) then
    begin
      Result := FailCastAttempt(E_NOTIMPL,
                                'Choose media route',
                                'This media requires remuxing or transcoding, but no conversion pipeline is configured.',
                                'Content type: ' + FCurrentMedia.ContentType +
                                '; container: ' + FCurrentMedia.ContainerName);
      Exit;
    end;

  if not MfCastIsHttpSource(ASourceName) then
    begin
      if FComponents.HttpServer.IsRunning then
        FComponents.HttpServer.Stop();

      HttpSettings := FSettings.Http;
      DeviceHost := Trim(ADevice.Address);

      if (DeviceHost = '') then
        DeviceHost := Trim(ADevice.HostName);

      DevicePort := ADevice.Port;
      if (DevicePort = 0) then
        DevicePort := FSettings.Protocol.ControlPort;

      if (Trim(HttpSettings.AdvertisedAddress) = '') and
         MfCastResolveLocalIPv4ForPeer(DeviceHost,
                                       DevicePort,
                                       AdvertisedAddress) then
        HttpSettings.AdvertisedAddress := AdvertisedAddress;

      Result := FComponents.HttpServer.Configure(HttpSettings);
      if (Result <> S_OK) then
        begin
          Result := FailCastAttempt(Result,
                                    'Configure HTTP server',
                                    'The local Chromecast HTTP server could not be configured.');
          Exit;
        end;

      Result := FComponents.HttpServer.Start();
      if (Result <> S_OK) then
        begin
          Result := FailCastAttempt(Result,
                                    'Start HTTP server',
                                    'The local Chromecast HTTP server could not be started.');
          Exit;
        end;

      Log(cllDebug,
          Format('Local HTTP server started: advertisedAddress=%s port=%d',
                 [HttpSettings.AdvertisedAddress,
                  FComponents.HttpServer.GetListenPort()]));
    end;

  case SelectedMediaMode of
    cmmDirectFile,
    cmmDirectWithTextTrack:      Result := PrepareDirectFile(ASourceName,
                                                             ASubtitle,
                                                             SelectedSubtitleMode,
                                                             FPendingLoadRequest);

    cmmTranscodeBurnedSubtitles: Result := PrepareTranscodedStream(ASourceName,
                                                                   ASubtitle,
                                                                   SelectedSubtitleMode,
                                                                   FPendingLoadRequest);
  else
    Result := E_UNEXPECTED;
  end;

  if (Result = S_OK) then
    begin

      if SelectedMediaMode in [cmmDirectFile, cmmDirectWithTextTrack] then
        begin
          FUsingTranscodedStream := False;
          FPendingLoadRequest.StartTime100ns := StartTime100ns;
          FPlaybackTimeOffset100ns := 0;
        end
      else
        begin
          // The transcoder seeks into the source and rebases its output to
          // zero. Add the source offset back to receiver status callbacks so
          // the player UI always sees the original media timeline.
          FPendingTranscodeRequest.StartTime100ns := StartTime100ns;
          FPendingLoadRequest.StartTime100ns := 0;
          FPlaybackTimeOffset100ns := StartTime100ns;
        end;
    end;

  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Prepare media',
                                'The media could not be prepared for Chromecast.');
      Exit;
    end;

  SetState(csConnecting);

  Log(cllInfo,
      'Connecting to Chromecast control channel.');

  Result := FComponents.Channel.Connect(ADevice);
  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Connect',
                                'The Chromecast device could not be reached.');
      Exit;
    end;

  SetState(csLaunchingReceiver);

  Log(cllInfo,
      'Launching the Default Media Receiver.');

  Result := FComponents.Channel.LaunchReceiver();
  if MfCastIsLaunchRetryable(Result) then
    begin
      // Some receivers retain or close a stale sender connection while the
      // Default Media Receiver is being created. Recreate and reconnect the
      // channel once for both closed-connection and bounded timeout failures.
      Log(cllWarning,
          Format('Receiver launch returned HRESULT $%.8x; recreating the control channel and retrying once.',
                 [DWORD(Result)]));

      FComponents.Channel.Disconnect();
      Result := RecreateControlChannel();
      Log(cllDebug,
          Format('Launch recovery: recreate channel HRESULT $%.8x.',
                 [DWORD(Result)]));

      if (Result = S_OK) then
        begin
          Result := FComponents.Channel.Connect(ADevice);
          Log(cllDebug,
              Format('Launch recovery: connect HRESULT $%.8x.',
                     [DWORD(Result)]));
        end;

      if (Result = S_OK) then
        begin
          Result := FComponents.Channel.LaunchReceiver();
          Log(cllDebug,
              Format('Launch recovery: receiver retry HRESULT $%.8x.',
                     [DWORD(Result)]));
        end;
    end;

  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Launch receiver',
                                'The Chromecast receiver could not be started.');
      Exit;
    end;

  if (FState = csError) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  Result := StartPendingMedia();
  if (Result = S_OK) then
    Log(cllInfo,
        'Media LOAD request accepted.');
end;


function TMfCastController.GetMediaTracks(const ASourceName: string;
                                          out ATracks: TMfCastTrackInfoArray): HRESULT;
begin

  if not Assigned(FComponents.MediaInspector) then
    begin
      SetLength(ATracks, 0);
      Result := E_POINTER;
      Exit;
    end;

  Result := FComponents.MediaInspector.EnumerateTracks(ASourceName, ATracks);
end;


function TMfCastController.Play(): HRESULT;
const
  MEDIA_STATUS_RECOVERY_TIMEOUT_MS = 5000;
var
  RecoveryResult: HRESULT;

begin

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Play()
  else
    Result := E_POINTER;

  if MfCastIsConnectionClosed(Result) and
     ((Trim(FCurrentDevice.Address) <> '') or
      (Trim(FCurrentDevice.HostName) <> '')) then
    begin
      Log(cllWarning,
          Format('PLAY found a closed control connection (HRESULT $%.8x); reconnecting to the active receiver.',
                 [DWORD(Result)]));

      RecoveryResult := RecreateControlChannel();
      Log(cllDebug,
          Format('PLAY recovery: recreate channel HRESULT $%.8x.',
                 [DWORD(RecoveryResult)]));

      if SUCCEEDED(RecoveryResult) then
        begin
          RecoveryResult := FComponents.Channel.Connect(FCurrentDevice);
          Log(cllDebug,
              Format('PLAY recovery: connect HRESULT $%.8x.',
                     [DWORD(RecoveryResult)]));
        end;

      if SUCCEEDED(RecoveryResult) then
        begin
          RecoveryResult := FComponents.Channel.LaunchReceiver();
          Log(cllDebug,
              Format('PLAY recovery: attach receiver HRESULT $%.8x.',
                     [DWORD(RecoveryResult)]));
        end;

      if SUCCEEDED(RecoveryResult) then
        begin
          RecoveryResult := FComponents.Channel.SynchronizeMediaStatus(
            MEDIA_STATUS_RECOVERY_TIMEOUT_MS);
          Log(cllDebug,
              Format('PLAY recovery: synchronize media session HRESULT $%.8x.',
                     [DWORD(RecoveryResult)]));
        end;

      if SUCCEEDED(RecoveryResult) then
        begin
          Result := FComponents.Channel.Play();
          Log(cllInfo,
              Format('PLAY recovery retry completed with HRESULT $%.8x.',
                     [DWORD(Result)]));
        end
      else
        Result := RecoveryResult;
    end;

  if SUCCEEDED(Result) and Assigned(FComponents.TranscodePipeline) then
    begin
      RecoveryResult := FComponents.TranscodePipeline.Resume();
      Log(cllDebug,
          Format('Transcode resume completed with HRESULT $%.8x.',
                 [DWORD(RecoveryResult)]));

      if FAILED(RecoveryResult) then
        Result := RecoveryResult;
    end;
end;


function TMfCastController.Pause(): HRESULT;
begin

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Pause()
  else
    Result := E_POINTER;

  if SUCCEEDED(Result) and Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Pause();
end;


function TMfCastController.Stop(): HRESULT;
var
  ReceiverResult: HRESULT;

begin

  if FState in [csStopping, csStopped] then
    begin
      Result := S_OK;
      Exit;
    end;

  SetState(csStopping);

  // Keep the media URL and transcoder alive until the receiver has processed
  // STOP. Aborting the HTTP stream first makes some receivers close the Cast
  // control socket with WSAECONNABORTED before STOP can be sent.
  if Assigned(FComponents.Channel) then
    ReceiverResult := FComponents.Channel.Stop()
  else
    ReceiverResult := S_OK;

  if Assigned(FComponents.SegmentPublisher) then
    FComponents.SegmentPublisher.AbortPresentation(E_ABORT);

  if Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Stop();

  if Assigned(FComponents.HttpServer) and
     (FCurrentSubtitlePublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentSubtitlePublishedPath);

  if Assigned(FComponents.HttpServer) and
     (FCurrentPublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentPublishedPath);

  if Assigned(FComponents.HttpServer) then
    FComponents.HttpServer.Stop();

  if Assigned(FComponents.Channel) then
    FComponents.Channel.Disconnect();

  FCurrentPublishedPath := '';
  FCurrentSubtitlePublishedPath := '';
  FPendingLoadRequest.Reset();
  FPendingTranscodeRequest.Reset();
  FHasPendingTranscode := False;
  FActiveLoadRequest.Reset();
  FActiveTranscodeRequest.Reset();
  FUsingTranscodedStream := False;
  FPlaybackTimeOffset100ns := 0;
  FMediaLoadStartTick := 0;
  FMediaPlaybackStarted := False;
  FSeekInProgress := False;
  FReplacementLoadPending := False;
  FCurrentDevice.Reset();
  FCurrentMedia.Reset();

  if MfCastIsConnectionClosed(ReceiverResult) then
    begin
      Log(cllWarning,
          Format('Receiver closed the control connection while stopping; treating HRESULT $%.8x as a completed stop.',
                 [DWORD(ReceiverResult)]));
      ReceiverResult := S_OK;
    end;

  if SUCCEEDED(ReceiverResult) then
    begin
      SetState(csStopped);
      Result := S_OK;
    end
  else
    begin
      SetState(csError);
      Result := ReceiverResult;
    end;
end;


function TMfCastController.Seek(const APosition100ns: Int64): HRESULT;
var
  EntryPath: string;
  Url: string;
  RestartRequest: TMfCastTranscodeRequest;
  ReloadRequest: TMfCastLoadRequest;
  ReceiverPosition100ns: Int64;
  SeekPosition100ns: Int64;

begin

  SeekPosition100ns := APosition100ns;
  if SeekPosition100ns < 0 then
    SeekPosition100ns := 0;

  Log(cllInfo,
      Format('Seek requested: position=%.3f seconds.',
             [SeekPosition100ns / 10000000.0]));

  if FUsingTranscodedStream then
    begin

      if FSeekInProgress then
        begin
          Result := HRESULT_FROM_WIN32(ERROR_BUSY);
          Exit;
        end;

      if (not Assigned(FComponents.TranscodePipeline)) or
         (not Assigned(FComponents.SegmentPublisher)) or
         (not Assigned(FComponents.HttpServer)) or
         (not Assigned(FComponents.Channel)) then
        begin
          Result := E_POINTER;
          Exit;
        end;

      FSeekInProgress := True;
      FReplacementLoadPending := True;
      try
        FMediaLoadStartTick := 0;
        FMediaPlaybackStarted := False;
        SetState(csPreparingMedia);
        Result := FComponents.TranscodePipeline.Stop();
        if FAILED(Result) then
          begin
            Result := FailCastAttempt(Result,
                                      'Seek transcoder',
                                      'The current Chromecast transcode could not be stopped.');
            Exit;
          end;

        FComponents.SegmentPublisher.AbortPresentation(E_ABORT);
        Result := FComponents.SegmentPublisher.BeginPresentation('video/mp4',
                                                                  EntryPath);
        if FAILED(Result) then
          begin
            Result := FailCastAttempt(Result,
                                      'Seek publisher',
                                      'A new Chromecast stream could not be published.');
            Exit;
          end;

        Result := FComponents.HttpServer.BuildUrl(EntryPath,
                                                   Url);
        if FAILED(Result) then
          begin
            Result := FailCastAttempt(Result,
                                      'Seek URL',
                                      'The new Chromecast stream URL could not be created.');
            Exit;
          end;

        // Force the receiver to treat the republished path as a new media item.
        // The HTTP server intentionally strips the query string for lookup.
        Url := Url + '?seek=' + IntToStr(SeekPosition100ns) +
               '&request=' + IntToStr(GetTickCount());

        RestartRequest := FActiveTranscodeRequest;
        RestartRequest.StartTime100ns := SeekPosition100ns;
        ReloadRequest := FActiveLoadRequest;
        ReloadRequest.ContentId := Url;
        ReloadRequest.StartTime100ns := 0;

        Result := FComponents.TranscodePipeline.Start(RestartRequest,
                                                      FComponents.SegmentPublisher,
                                                      FComponents.PreviewSink);
        if FAILED(Result) then
          begin
            Result := FailCastAttempt(Result,
                                      'Seek transcoder',
                                      'The Chromecast transcoder could not restart at the selected position.');
            Exit;
          end;

        FActiveTranscodeRequest := RestartRequest;
        FActiveLoadRequest := ReloadRequest;
        FPlaybackTimeOffset100ns := SeekPosition100ns;
        SetState(csBuffering);

        FMediaLoadStartTick := GetTickCount();

        Result := FComponents.Channel.LoadMedia(ReloadRequest);
        if (Result <> S_OK) then
          begin

            OutputDebugString(PChar(Format('MfCast seek: direct LOAD failed hr=%.8x; recreating control channel',
                                          [DWORD(Result)])));
            Result := RecreateControlChannel();
            if (Result = S_OK) then
              Result := FComponents.Channel.Connect(FCurrentDevice);

            if (Result = S_OK) then
              Result := FComponents.Channel.LaunchReceiver();

            if (Result <> S_OK) then
              begin
                Result := RecreateControlChannel();

                if (Result = S_OK) then
                  Result := FComponents.Channel.Connect(FCurrentDevice);

                if (Result = S_OK) then
                  Result := FComponents.Channel.LaunchReceiver();
              end;

            if (Result = S_OK) then
              Result := FComponents.Channel.LoadMedia(ReloadRequest);
          end;

        if (Result <> S_OK) then
          Result := FailCastAttempt(Result,
                                    'Seek media',
                                    'The Chromecast receiver rejected the restarted stream.')
        else
          Log(cllInfo,
              'Seek replacement media LOAD request accepted.');
      finally
        FSeekInProgress := False;
      end;
      Exit;
    end;

  // Transcoded output is rebased to zero at its source start position. Keep
  // callers on the original file timeline and translate only for the receiver.
  ReceiverPosition100ns := SeekPosition100ns - FPlaybackTimeOffset100ns;
  if (ReceiverPosition100ns < 0) then
    ReceiverPosition100ns := 0;

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Seek(ReceiverPosition100ns)
  else
    Result := E_POINTER;
end;


function TMfCastController.SelectAudioTrack(const ATrackId: Int64): HRESULT;
var
  Kind: TMfCastTrackKind;
  Source: TMfCastTrackSource;
  StreamIndex: DWORD;

begin

  if not MfCastDecodeTrackId(ATrackId, Kind, Source, StreamIndex) or
     (Kind <> ctkAudio) or (Source <> ctsMediaFoundation) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if not FUsingTranscodedStream then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
      Exit;
    end;

  Log(cllInfo,
      Format('Audio track selection requested: trackId=%d stream=%d.',
             [ATrackId, StreamIndex]));
  FActiveTranscodeRequest.AudioTrackId := ATrackId;
  FActiveTranscodeRequest.AudioStreamIndex := StreamIndex;
  FActiveTranscodeRequest.HasAudioStreamIndex := True;
  Result := Seek(CurrentFilePosition100ns());
end;


function TMfCastController.SelectSubtitle(
  const ASubtitle: TMfCastSubtitleAsset): HRESULT;
begin

  if not ASubtitle.Enabled then
    begin
      Result := DisableSubtitles();
      Exit;
    end;

  if not FUsingTranscodedStream then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
      Exit;
    end;

  Log(cllInfo,
      Format('Subtitle selection requested: trackId=%d stream=%d language="%s".',
             [ASubtitle.TrackId,
              ASubtitle.StreamIndex,
              ASubtitle.Language]));
  FActiveTranscodeRequest.SubtitleMode := csmBurnIntoVideo;
  if ASubtitle.Embedded or (Trim(ASubtitle.SourceName) = '') then
    FActiveTranscodeRequest.SubtitleSourceName := FActiveTranscodeRequest.SourceName
  else
    FActiveTranscodeRequest.SubtitleSourceName := ASubtitle.SourceName;
  FActiveTranscodeRequest.SubtitleLanguage := ASubtitle.Language;
  FActiveTranscodeRequest.SubtitleTrackId := ASubtitle.TrackId;
  FActiveTranscodeRequest.SubtitleStreamIndex := ASubtitle.StreamIndex;
  FActiveTranscodeRequest.HasSubtitleStreamIndex := ASubtitle.HasStreamIndex;
  FActiveTranscodeRequest.SubtitleAspectRatio := ASubtitle.AspectRatio;
  FActiveTranscodeRequest.SubtitleData := Copy(ASubtitle.Data,
                                               0,
                                               Length(ASubtitle.Data));
  Result := Seek(CurrentFilePosition100ns());
end;


function TMfCastController.SelectSubtitleTrack(
  const ATrackId: Int64): HRESULT;
var
  Kind: TMfCastTrackKind;
  Source: TMfCastTrackSource;
  StreamIndex: DWORD;
  Subtitle: TMfCastSubtitleAsset;

begin

  if not MfCastDecodeTrackId(ATrackId, Kind, Source, StreamIndex) or
     (Kind <> ctkSubtitle) or
     (not (Source in [ctsMediaFoundation, ctsMatroska])) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Subtitle.Reset();
  Subtitle.Enabled := True;
  Subtitle.Embedded := True;
  Subtitle.TrackId := ATrackId;
  Subtitle.StreamIndex := StreamIndex;
  Subtitle.HasStreamIndex := True;
  Subtitle.SourceName := FActiveTranscodeRequest.SourceName;
  Result := SelectSubtitle(Subtitle);
end;


function TMfCastController.DisableSubtitles(): HRESULT;
begin

  if not FUsingTranscodedStream then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
      Exit;
    end;

  Log(cllInfo, 'Subtitle disable requested.');
  FActiveTranscodeRequest.SubtitleMode := csmNone;
  FActiveTranscodeRequest.SubtitleSourceName := '';
  FActiveTranscodeRequest.SubtitleLanguage := '';
  FActiveTranscodeRequest.SubtitleTrackId := 0;
  FActiveTranscodeRequest.SubtitleStreamIndex := 0;
  FActiveTranscodeRequest.HasSubtitleStreamIndex := False;
  SetLength(FActiveTranscodeRequest.SubtitleData, 0);
  Result := Seek(CurrentFilePosition100ns());
end;


function TMfCastController.RecreateControlChannel(): HRESULT;
var
  ChannelCallbacks: TMfCastChannelCallbacks;

begin

  FComponents.Channel := nil;
  FComponents.Channel := TMfCastChannel.Create(TMfCastTcpTransport.Create());

  if not Assigned(FComponents.Channel) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  Result := FComponents.Channel.Configure(FSettings.Protocol);
  if (Result <> S_OK) then
    Exit;

  ChannelCallbacks.Reset();
  ChannelCallbacks.OnReceiverReady := ReceiverReady;
  ChannelCallbacks.OnReceiverClosed := ReceiverClosed;
  ChannelCallbacks.OnMediaStatus := ChannelMediaStatus;
  ChannelCallbacks.OnError := ChannelError;
  FComponents.Channel.SetCallbacks(ChannelCallbacks);
  FComponents.Channel.SetLogger(FLogger);
end;


function TMfCastController.SetVolume(const AVolume: Single): HRESULT;
begin

  Log(cllInfo,
      Format('Volume requested: %.0f%%.',
             [AVolume * 100.0]));

  if FUsingTranscodedStream and
     Assigned(FComponents.TranscodePipeline) then
    begin
      Log(cllDebug,
          'Applying Volume to decoded PCM before AAC encoding.');
      Result := FComponents.TranscodePipeline.SetVolume(AVolume);
    end
  else
    if Assigned(FComponents.Channel) then
      Result := FComponents.Channel.SetVolume(AVolume)
    else
      Result := E_POINTER;

  Log(cllDebug,
      Format('Volume command completed with HRESULT $%.8x.',
             [DWORD(Result)]));
end;


function TMfCastController.SetMuted(const AMuted: Boolean): HRESULT;
begin

  if AMuted then
    Log(cllInfo,
        'Mute requested: on.')
  else
    Log(cllInfo,
        'Mute requested: off.');

  if FUsingTranscodedStream and
     Assigned(FComponents.TranscodePipeline) then
    begin
      Log(cllDebug,
          'Applying Mute to decoded PCM before AAC encoding.');
      Result := FComponents.TranscodePipeline.SetMuted(AMuted);
    end
  else
    if Assigned(FComponents.Channel) then
      Result := FComponents.Channel.SetMuted(AMuted)
    else
      Result := E_POINTER;

  Log(cllDebug,
      Format('Mute command completed with HRESULT $%.8x.',
             [DWORD(Result)]));
end;


function TMfCastController.Disconnect(): HRESULT;
begin

  CleanupCastAttempt();
  SetState(csIdle);
  Result := S_OK;
end;


function TMfCastController.GetState(): TMfCastState;
begin

  Result := FState;
end;


function TMfCastController.GetHttpRequestCount(): Cardinal;
begin

  Result := 0;

  if Assigned(FComponents.HttpServer) then
    Result := FComponents.HttpServer.GetRequestCount();
end;


procedure TMfCastController.DiscoveryStarted();
begin

  SetState(csDiscovering);
end;


procedure TMfCastController.DiscoveryStopped();
begin

  if (FState = csDiscovering) then
    SetState(csIdle);
end;


procedure TMfCastController.DeviceAdded(const ADevice: TMfCastDevice);
begin

  if Assigned(FCallbacks.OnDeviceAdded) then
    FCallbacks.OnDeviceAdded(ADevice);
end;


procedure TMfCastController.DeviceUpdated(const ADevice: TMfCastDevice);
begin

  if Assigned(FCallbacks.OnDeviceUpdated) then
    FCallbacks.OnDeviceUpdated(ADevice);
end;

procedure TMfCastController.DeviceRemoved(const ADeviceId: string);
begin

  if Assigned(FCallbacks.OnDeviceRemoved) then
    FCallbacks.OnDeviceRemoved(ADeviceId);
end;


procedure TMfCastController.DiscoveryError(const AError: TMfCastErrorInfo);
begin

  if Assigned(FCallbacks.OnError) then
    FCallbacks.OnError(AError);
end;


procedure TMfCastController.ReceiverReady(const ASessionId,
                                          ATransportId: string);
begin

  if not (FState in [csConnecting,
                     csLaunchingReceiver,
                     csConnected]) then
    Exit;

  // Do not start the transcoder or enter the media-status read loop from
  // inside the receiver-status callback. That nested read could disconnect
  // the channel and surface as E_UNEXPECTED from LaunchReceiver().
  SetState(csConnected);
end;


procedure TMfCastController.ReceiverClosed();
begin

  if (FState = csStopping) then
    Exit;

  SetState(csStopped);
end;


procedure TMfCastController.ChannelMediaStatus(const AStatus: TMfCastMediaStatus);
const
  TRANSCODE_STARTUP_IDLE_GRACE_MS = 15000;

var
  CallbackStatus: TMfCastMediaStatus;
  StartupElapsedMs: Cardinal;

begin

  // A receiver normally answers our STOP with IDLE. The foreground Stop()
  // call owns teardown, so its receive-thread callback must not enter cleanup
  // a second time.
  if (FState = csStopping) then
    Exit;

  // While the old transcoder and publisher are being replaced, status still
  // belongs to the old media session and must not alter the seek transaction.
  if FSeekInProgress and (FMediaLoadStartTick = 0) then
    Exit;

  if SameText(AStatus.PlayerState, 'IDLE') and
      ((not SameText(AStatus.IdleReason, 'ERROR')) or FReplacementLoadPending) and
     FUsingTranscodedStream and
     (not FMediaPlaybackStarted) and
     (FMediaLoadStartTick <> 0) then
    begin
      StartupElapsedMs := GetTickCount() - FMediaLoadStartTick;

      if (StartupElapsedMs < TRANSCODE_STARTUP_IDLE_GRACE_MS) then
        begin
          Log(cllWarning,
              Format('Ignoring transient startup/seek IDLE status: mediaSessionId=%d idleReason="%s" elapsed=%d ms.',
                     [AStatus.MediaSessionId,
                      AStatus.IdleReason,
                      StartupElapsedMs]));
          Exit;
        end;
    end;

  CallbackStatus := AStatus;
  if (FPlaybackTimeOffset100ns > 0) and
     (CallbackStatus.CurrentTime100ns >= 0) then
    Inc(CallbackStatus.CurrentTime100ns,
        FPlaybackTimeOffset100ns);

  if (CallbackStatus.CurrentTime100ns >= 0) then
    FLastMediaPosition100ns := CallbackStatus.CurrentTime100ns;

  if Assigned(FCallbacks.OnMediaStatus) then
    FCallbacks.OnMediaStatus(CallbackStatus);

  if SameText(AStatus.PlayerState,
              'PLAYING') then
    begin
      FMediaPlaybackStarted := True;
      FReplacementLoadPending := False;
      SetState(csPlaying);
    end
  else
    if SameText(AStatus.PlayerState,
                'PAUSED') then
      begin
        FMediaPlaybackStarted := True;
        FReplacementLoadPending := False;
        SetState(csPaused);
      end
    else
      if SameText(AStatus.PlayerState,
                  'BUFFERING') or
         SameText(AStatus.PlayerState,
                  'LOADING') then
        SetState(csBuffering)
      else
        if SameText(AStatus.PlayerState,
                    'IDLE') then
          begin
            if SameText(AStatus.IdleReason,
                        'ERROR') then
              FailCastAttempt(E_FAIL,
                              'Media status',
                              'The Chromecast receiver stopped playback with an error.',
                              AStatus.IdleReason)
            else
              begin
                CleanupCastAttempt();
                SetState(csStopped);
              end;
          end;
end;


procedure TMfCastController.ChannelError(const AError: TMfCastErrorInfo);
begin

  if (FState = csStopping) then
    begin
      Log(cllDebug,
          Format('Ignoring control-channel error during Stop: HRESULT $%.8x.',
                 [DWORD(AError.HResult)]));
      Exit;
    end;

  CleanupCastAttempt();
  SetState(csError);

  if Assigned(FCallbacks.OnError) then
    FCallbacks.OnError(AError);
end;


procedure TMfCastController.SetState(const AState: TMfCastState);
var
  OldState: TMfCastState;

begin

  if (FState = AState) then
    Exit;

  OldState := FState;
  FState := AState;

  Log(cllDebug,
      Format('State changed: %s -> %s',
             [MfCastStateToString(OldState),
              MfCastStateToString(AState)]));

  if Assigned(FCallbacks.OnStateChanged) then
    FCallbacks.OnStateChanged(OldState,
                              AState);
end;


procedure TMfCastController.ReportError(const AHResult: HRESULT;
                                        const AStage: string;
                                        const AMessage: string;
                                        const ADetail: string);
var
  ErrorInfo: TMfCastErrorInfo;
  LogText: string;

begin

  ErrorInfo.Reset;
  ErrorInfo.HResult := AHResult;
  ErrorInfo.Stage := AStage;
  ErrorInfo.MessageText := AMessage;
  ErrorInfo.Detail := ADetail;

  LogText := Format('%s failed: %s (HRESULT $%.8x)',
                    [AStage,
                     AMessage,
                     DWORD(AHResult)]);

  if (ADetail <> '') then
    LogText := LogText + ' Detail: ' + ADetail;

  Log(cllError,
      LogText);

  SetState(csError);

  if Assigned(FCallbacks.OnError) then
    FCallbacks.OnError(ErrorInfo);
end;


function TMfCastController.PrepareDirectFile(const ASourceName: string;
                                             const ASubtitle: TMfCastSubtitleAsset;
                                             const ASubtitleMode: TMfCastSubtitleMode;
                                             out ALoadRequest: TMfCastLoadRequest): HRESULT;
var
  Content: IMfCastHttpContent;
  ResourceName: string;
  PublishedPath: string;
  Url: string;
  SubtitleContent: IMfCastHttpContent;
  SubtitlePath: string;
  SubtitleUrl: string;
  SubtitleContentType: string;
  SubtitleName: string;
  SubtitleLanguage: string;
  SubtitleResourceName: string;
  SubtitleTrackId: Int64;

begin

  ALoadRequest.Reset();

  if MfCastIsHttpSource(ASourceName) then
    begin
      ALoadRequest.ContentId := Trim(ASourceName);
      ALoadRequest.ContentType := FCurrentMedia.ContentType;
      ALoadRequest.StreamType := cstBuffered;
      ALoadRequest.Title := FCurrentMedia.Title;
      ALoadRequest.AutoPlay := True;

      if (ASubtitleMode = csmExternalTextTrack) then
        begin
          Result := E_NOTIMPL;
          Exit;
        end;

      Result := S_OK;
      Exit;
    end;

  if not Assigned(FComponents.HttpServer) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  ResourceName := 'media' + ExtractFileExt(ASourceName);
  if (ResourceName = 'media') then
    ResourceName := 'media.bin';

  Content := TMfCastFileContent.Create(ASourceName,
                                       FCurrentMedia.ContentType);

  if (FCurrentSubtitlePublishedPath <> '') then
    begin
      FComponents.HttpServer.Unpublish(FCurrentSubtitlePublishedPath);
      FCurrentSubtitlePublishedPath := '';
    end;

  if (FCurrentPublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentPublishedPath);

  Result := FComponents.HttpServer.Publish(ResourceName,
                                           Content,
                                           PublishedPath);
  if FAILED(Result) then
    Exit;

  Result := FComponents.HttpServer.BuildUrl(PublishedPath,
                                            Url);
  if FAILED(Result) then
    begin
      FComponents.HttpServer.Unpublish(PublishedPath);
      Exit;
    end;

  FCurrentPublishedPath := PublishedPath;
  OutputDebugString(PChar('MfCast LOAD URL: ' + Url));
  ALoadRequest.ContentId := Url;
  ALoadRequest.ContentType := FCurrentMedia.ContentType;
  ALoadRequest.StreamType := cstBuffered;
  ALoadRequest.Title := FCurrentMedia.Title;
  ALoadRequest.AutoPlay := True;

  if (ASubtitleMode = csmExternalTextTrack) then
    begin

      if (not ASubtitle.Enabled) or (Length(ASubtitle.Data) = 0) then
        begin
          Result := E_INVALIDARG;
          Exit;
        end;

      SubtitleContentType := Trim(ASubtitle.ContentType);

      if (SubtitleContentType = '') then
        SubtitleContentType := 'text/vtt; charset=utf-8';

      if Pos('ttml', LowerCase(SubtitleContentType)) > 0 then
        SubtitleResourceName := 'subtitles.ttml'
      else
        SubtitleResourceName := 'subtitles.vtt';

      SubtitleContent := TMfCastMemoryContent.Create(ASubtitle.Data,
                                                     SubtitleContentType);

      Result := FComponents.HttpServer.Publish(SubtitleResourceName,
                                               SubtitleContent,
                                               SubtitlePath);
      if FAILED(Result) then
        Exit;

      Result := FComponents.HttpServer.BuildUrl(SubtitlePath,
                                                SubtitleUrl);

      if FAILED(Result) then
        begin
          FComponents.HttpServer.Unpublish(SubtitlePath);
          Exit;
        end;

      // Cast receivers may cache an out-of-band text track by URL across
      // sessions. Use a fresh URL for every publication while the HTTP server
      // continues to resolve the resource path without its query string.
      SubtitleUrl := SubtitleUrl + '?v=' + IntToStr(Int64(GetTickCount()));

      FCurrentSubtitlePublishedPath := SubtitlePath;
      SubtitleName := Trim(ASubtitle.Name);

      if (SubtitleName = '') then
        SubtitleName := 'Subtitles';

      SubtitleLanguage := StringReplace(Trim(ASubtitle.Language),
                                        '_',
                                        '-',
                                        [rfReplaceAll]);

      if (SubtitleLanguage = '') then
        SubtitleLanguage := 'und';

      // Some Android Cast receivers retain a text track object across media
      // sessions when its ID is reused. Give each LOAD a fresh track identity.
      SubtitleTrackId := Int64(GetTickCount());
      if SubtitleTrackId = 0 then
        SubtitleTrackId := 1;

      SetLength(ALoadRequest.Tracks, 1);
      ALoadRequest.Tracks[0].Reset();
      ALoadRequest.Tracks[0].TrackId := SubtitleTrackId;
      ALoadRequest.Tracks[0].TrackType := 'TEXT';
      ALoadRequest.Tracks[0].ContentId := SubtitleUrl;
      ALoadRequest.Tracks[0].ContentType := SubtitleContentType;
      ALoadRequest.Tracks[0].Name := SubtitleName;
      ALoadRequest.Tracks[0].Language := SubtitleLanguage;
      ALoadRequest.Tracks[0].SubType := 'SUBTITLES';
      SetLength(ALoadRequest.ActiveTrackIds, 1);
      ALoadRequest.ActiveTrackIds[0] := SubtitleTrackId;
      OutputDebugString(PChar(Format('MfCast subtitle trackId=%d URL: %s',
                                    [SubtitleTrackId, SubtitleUrl])));
    end;

  Result := S_OK;
end;


function TMfCastController.PrepareTranscodedStream(const ASourceName: string;
                                                   const ASubtitle: TMfCastSubtitleAsset;
                                                   const ASubtitleMode: TMfCastSubtitleMode;
                                                   out ALoadRequest: TMfCastLoadRequest): HRESULT;
var
  Request: TMfCastTranscodeRequest;
  EntryPath: string;
  Url: string;

begin

  ALoadRequest.Reset();

  if (not Assigned(FComponents.TranscodePipeline)) or
     (not Assigned(FComponents.SegmentPublisher)) or
     (not Assigned(FComponents.HttpServer)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Request.Reset();
  Request.SourceName := ASourceName;
  Request.Title := FCurrentMedia.Title;
  Request.SubtitleMode := ASubtitleMode;

  if (ASubtitleMode = csmBurnIntoVideo) and ASubtitle.Enabled then
    begin
      if ASubtitle.Embedded or (Trim(ASubtitle.SourceName) = '') then
        Request.SubtitleSourceName := ASourceName
      else
        Request.SubtitleSourceName := ASubtitle.SourceName;

      Request.SubtitleLanguage := ASubtitle.Language;
      Request.SubtitleTrackId := ASubtitle.TrackId;
      Request.SubtitleStreamIndex := ASubtitle.StreamIndex;
      Request.HasSubtitleStreamIndex := ASubtitle.HasStreamIndex;
      Request.SubtitleAspectRatio := ASubtitle.AspectRatio;
      Request.SubtitleData := Copy(ASubtitle.Data,
                                   0,
                                   Length(ASubtitle.Data));
    end;

  Request.Encoding := FSettings.Encoding;
  Request.Encoding.OutputMode := comFragmentedMp4;

  Result := FComponents.SegmentPublisher.BeginPresentation('video/mp4',
                                                           EntryPath);
  if FAILED(Result) then
    Exit;

  Result := FComponents.HttpServer.BuildUrl(EntryPath,
                                            Url);
  if FAILED(Result) then
    begin
      FComponents.SegmentPublisher.AbortPresentation(Result);
      Exit;
    end;

  OutputDebugString(PChar('MfCast TRANSCODE URL: ' + Url));

  // Start only after ReceiverReady. An offline receiver must not leave a
  // worker writing into a presentation replaced by a later attempt.
  FPendingTranscodeRequest := Request;
  FHasPendingTranscode := True;

  ALoadRequest.ContentId := Url;
  ALoadRequest.ContentType := 'video/mp4';
  ALoadRequest.StreamType := cstBuffered;
  ALoadRequest.Title := FCurrentMedia.Title;
  ALoadRequest.AutoPlay := True;

  Result := S_OK;
end;

end.
