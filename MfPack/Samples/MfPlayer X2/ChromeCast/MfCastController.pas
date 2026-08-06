// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
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
// Intiator(s): Tony (maXcomX), Peter (OzShips), Carmen (carmenh).
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

    procedure CleanupCastAttempt();
    function RecreateControlChannel(): HRESULT;
    function FailCastAttempt(const AHResult: HRESULT;
                             const AStage: string;
                             const AMessage: string;
                             const ADetail: string = ''): HRESULT;
    function StartPendingMedia(): HRESULT;
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

    procedure DiscoveryStarted;
    procedure DiscoveryStopped;
    procedure DeviceAdded(const ADevice: TMfCastDevice);
    procedure DeviceUpdated(const ADevice: TMfCastDevice);
    procedure DeviceRemoved(const ADeviceId: string);
    procedure DiscoveryError(const AError: TMfCastErrorInfo);

    procedure ReceiverReady(const ASessionId: string;
                            const ATransportId: string);
    procedure ReceiverClosed;
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
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function Disconnect: HRESULT;
    function GetState: TMfCastState;
  end;


implementation

uses
  MfCastHttpServer,
  MfCastChannel,
  MfCastTransport;


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

  inherited Create;

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
begin

  // Stop the writer before its publisher, byte stream, or HTTP resources
  // are released.
  if Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Stop();

  if Assigned(FComponents.SegmentPublisher) then
    FComponents.SegmentPublisher.AbortPresentation(E_ABORT);

  if Assigned(FComponents.HttpServer) and
     (FCurrentSubtitlePublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentSubtitlePublishedPath);

  if Assigned(FComponents.HttpServer) and
     (FCurrentPublishedPath <> '') then
    FComponents.HttpServer.Unpublish(FCurrentPublishedPath);

  if Assigned(FComponents.Channel) then
    FComponents.Channel.Disconnect();

  if Assigned(FComponents.HttpServer) then
    FComponents.HttpServer.Stop();

  FCurrentPublishedPath := '';
  FCurrentSubtitlePublishedPath := '';
  FPendingLoadRequest.Reset();
  FPendingTranscodeRequest.Reset();
  FHasPendingTranscode := False;
  FActiveLoadRequest.Reset();
  FActiveTranscodeRequest.Reset();
  FUsingTranscodedStream := False;
  FPlaybackTimeOffset100ns := 0;
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

  Result := FComponents.Channel.LoadMedia(FPendingLoadRequest);

  if (Result = S_OK) then
    FActiveLoadRequest := FPendingLoadRequest;

  if (Result <> S_OK) then
    Result := FailCastAttempt(Result,
                              'Load media',
                              'The Chromecast receiver rejected the media load request.');
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

  FCurrentMedia.HasTimedText := ASubtitle.Enabled and
                                (Length(ASubtitle.Data) > 0);

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

  if Result = S_OK then
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

  Result := FComponents.Channel.Connect(ADevice);
  if (Result <> S_OK) then
    begin
      Result := FailCastAttempt(Result,
                                'Connect',
                                'The Chromecast device could not be reached.');
      Exit;
    end;

  SetState(csLaunchingReceiver);

  Result := FComponents.Channel.LaunchReceiver();
  if Result = E_UNEXPECTED then
    begin
      // Some receivers close the first control connection while the Default
      // Media Receiver is being created. Reconnect once instead of requiring
      // the user to start the complete Cast operation a second time.
      FComponents.Channel.Disconnect();
      Result := FComponents.Channel.Connect(ADevice);

      if (Result = S_OK) then
        Result := FComponents.Channel.LaunchReceiver();
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
end;


function TMfCastController.Play(): HRESULT;
begin

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Play()
  else
    Result := E_POINTER;

  if SUCCEEDED(Result) and Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Resume();
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
begin

  if Assigned(FComponents.TranscodePipeline) then
    FComponents.TranscodePipeline.Stop();

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Stop
  else
    Result := S_OK;
end;


function TMfCastController.Seek(const APosition100ns: Int64): HRESULT;
var
  EntryPath: string;
  Url: string;
  RestartRequest: TMfCastTranscodeRequest;
  ReloadRequest: TMfCastLoadRequest;
  ReceiverPosition100ns: Int64;

begin

  if FUsingTranscodedStream then
    begin

      if (not Assigned(FComponents.TranscodePipeline)) or
         (not Assigned(FComponents.SegmentPublisher)) or
         (not Assigned(FComponents.HttpServer)) or
         (not Assigned(FComponents.Channel)) then
        begin
          Result := E_POINTER;
          Exit;
        end;

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
      Url := Url + '?seek=' + IntToStr(APosition100ns) +
             '&request=' + IntToStr(GetTickCount());

      RestartRequest := FActiveTranscodeRequest;
      RestartRequest.StartTime100ns := APosition100ns;
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
      FPlaybackTimeOffset100ns := APosition100ns;
      SetState(csBuffering);

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
                                  'The Chromecast receiver rejected the restarted stream.');
      Exit;
    end;

  // Transcoded output is rebased to zero at its source start position. Keep
  // callers on the original file timeline and translate only for the receiver.
  ReceiverPosition100ns := APosition100ns - FPlaybackTimeOffset100ns;
  if (ReceiverPosition100ns < 0) then
    ReceiverPosition100ns := 0;

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.Seek(ReceiverPosition100ns)
  else
    Result := E_POINTER;
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

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.SetVolume(AVolume)
  else
    Result := E_POINTER;
end;


function TMfCastController.SetMuted(const AMuted: Boolean): HRESULT;
begin

  if Assigned(FComponents.Channel) then
    Result := FComponents.Channel.SetMuted(AMuted)
  else
    Result := E_POINTER;
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


procedure TMfCastController.ReceiverClosed;
begin

  SetState(csStopped);
end;


procedure TMfCastController.ChannelMediaStatus(const AStatus: TMfCastMediaStatus);
var
  CallbackStatus: TMfCastMediaStatus;

begin

  CallbackStatus := AStatus;
  if (FPlaybackTimeOffset100ns > 0) and
     (CallbackStatus.CurrentTime100ns >= 0) then
    Inc(CallbackStatus.CurrentTime100ns,
        FPlaybackTimeOffset100ns);

  if Assigned(FCallbacks.OnMediaStatus) then
    FCallbacks.OnMediaStatus(CallbackStatus);

  if SameText(AStatus.PlayerState,
              'PLAYING') then
    SetState(csPlaying)
  else
    if SameText(AStatus.PlayerState,
                'PAUSED') then
      SetState(csPaused)
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
            if SameText(AStatus.IdleReason, 'ERROR') then
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

begin

  ErrorInfo.Reset;
  ErrorInfo.HResult := AHResult;
  ErrorInfo.Stage := AStage;
  ErrorInfo.MessageText := AMessage;
  ErrorInfo.Detail := ADetail;
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

begin

  ALoadRequest.Reset();

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

      SubtitleContent := TMfCastMemoryContent.Create(ASubtitle.Data,
                                                     SubtitleContentType);

      Result := FComponents.HttpServer.Publish('subtitles.vtt',
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

      SetLength(ALoadRequest.Tracks, 1);
      ALoadRequest.Tracks[0].Reset();
      ALoadRequest.Tracks[0].TrackId := 1;
      ALoadRequest.Tracks[0].TrackType := 'TEXT';
      ALoadRequest.Tracks[0].ContentId := SubtitleUrl;
      ALoadRequest.Tracks[0].ContentType := 'text/vtt';
      ALoadRequest.Tracks[0].Name := SubtitleName;
      ALoadRequest.Tracks[0].Language := SubtitleLanguage;
      ALoadRequest.Tracks[0].SubType := 'SUBTITLES';
      SetLength(ALoadRequest.ActiveTrackIds, 1);
      ALoadRequest.ActiveTrackIds[0] := 1;
      OutputDebugString(PChar('MfCast subtitle URL: ' + SubtitleUrl));
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
      Request.SubtitleSourceName := ASourceName;
      Request.SubtitleLanguage := ASubtitle.Language;
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
