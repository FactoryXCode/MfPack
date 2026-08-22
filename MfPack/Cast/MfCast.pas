// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCast.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Public facade for Cast discovery, connection, media loading,
//              and playback control.
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
// Source: Google Cast V2 wire protocol
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
unit MfCast;

interface

uses

  {WinApi}
  WinApi.Windows,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  {System}
  System.SysUtils,
  {Cast}
  MfCastTypes,
  MfCastInterfaces,
  MfCastMediaInterfaces;

type
  TMfCast = class;

  TMfCastLogEvent = procedure(Sender: TObject;
                              const ALevel: TMfCastLogLevel;
                              const ASource: string;
                              const AMessage: string) of object;

  EMfCast = class(Exception);

  // Public facade for discovery and direct playback. Callbacks are delivered
  // by worker threads; VCL clients must marshal control updates to the main
  // thread (the MfSimpleCastPlayer sample demonstrates this with PostMessage).
  TMfCast = class
  private
    FController: IMfCastController;
    FLogger: IMfCastLogger;
    FPreviewSink: IMfCastPreviewSink;
    FDirectPreviewPlayer: IMfCastDirectPreviewPlayer;
    FMediaFoundationStarted: Boolean;
    FOnDeviceAdded: TMfCastDeviceEvent;
    FOnDeviceUpdated: TMfCastDeviceEvent;
    FOnDeviceRemoved: TMfCastDeviceRemovedEvent;
    FOnStateChanged: TMfCastStateChangedEvent;
    FOnMediaStatus: TMfCastMediaStatusEvent;
    FOnError: TMfCastErrorEvent;
    FOnLog: TMfCastLogEvent;

    procedure ControllerDeviceAdded(const ADevice: TMfCastDevice);
    procedure ControllerDeviceUpdated(const ADevice: TMfCastDevice);
    procedure ControllerDeviceRemoved(const ADeviceId: string);

    procedure ControllerStateChanged(const AOldState: TMfCastState;
                                     const ANewState: TMfCastState);

    procedure ControllerMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure ControllerError(const AError: TMfCastErrorInfo);

    procedure DispatchLog(const ALevel: TMfCastLogLevel;
                          const ASource: string;
                          const AMessage: string);
  public

    constructor Create(const AEnableTranscoding: Boolean = False);
    destructor Destroy(); override;

    function Discover(): HRESULT;
    function StopDiscovery(): HRESULT;
    function RefreshDiscovery(): HRESULT;
    function GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;

    function GetMediaTracks(const ASource: string;
                            out ATracks: TMfCastTrackInfoArray): HRESULT;

    function SetPreviewWindow(const AWindow: HWND): HRESULT;
    function UpdatePreviewWindow(): HRESULT;
    function SetAudioArtwork(const ASourceName: string): HRESULT;
    function SetSourceResolver(const AResolver: IMfCastSourceResolver): HRESULT;

    function Connect(const ADevice: TMfCastDevice): HRESULT;

    function Load(const ASource: string): HRESULT; overload;

    function Load(const ASource: string;
                  const ASubtitle: TMfCastSubtitleAsset;
                  const AMediaMode: TMfCastMediaMode = cmmAutomatic;
                  const ASubtitleMode: TMfCastSubtitleMode = csmAutomatic;
                  const AStartSeconds: Double = 0.0): HRESULT; overload;

    function Cast(const ADevice: TMfCastDevice;
                  const ASource: string): HRESULT; overload;
                  
    function Cast(const ADevice: TMfCastDevice;
                  const ASource: string;
                  const ASubtitle: TMfCastSubtitleAsset;
                  const AMediaMode: TMfCastMediaMode = cmmAutomatic;
                  const ASubtitleMode: TMfCastSubtitleMode = csmAutomatic;
                  const AStartSeconds: Double = 0.0): HRESULT; overload;

    function CastLiveFragmentedMp4(const ADevice: TMfCastDevice;
                                   const AInitSegment: TBytes;
                                   out AByteStream: IMFByteStream): HRESULT;

    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;

    function Seek(const APositionSeconds: Double): HRESULT;
    function SelectAudioTrack(const ATrackId: Int64): HRESULT;
    function SelectSubtitle(const ATrackId: Int64): HRESULT; overload;
    function SelectSubtitle(const ASubtitle: TMfCastSubtitleAsset): HRESULT; overload;
    function DisableSubtitles(): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function Disconnect(): HRESULT;
    function State(): TMfCastState;

    property OnDeviceAdded: TMfCastDeviceEvent read FOnDeviceAdded write FOnDeviceAdded;
    property OnDeviceUpdated: TMfCastDeviceEvent read FOnDeviceUpdated write FOnDeviceUpdated;
    property OnDeviceRemoved: TMfCastDeviceRemovedEvent read FOnDeviceRemoved write FOnDeviceRemoved;
    property OnStateChanged: TMfCastStateChangedEvent read FOnStateChanged write FOnStateChanged;
    property OnMediaStatus: TMfCastMediaStatusEvent read FOnMediaStatus write FOnMediaStatus;
    property OnError: TMfCastErrorEvent read FOnError write FOnError;
    property OnLog: TMfCastLogEvent read FOnLog write FOnLog;
  end;


implementation

uses
  {Cast}
  MfCastTransport,
  MfCastChannel,
  MfCastDiscovery,
  MfCastHttpServer,
  MfCastMedia,
  MfCastWindowPreview,
  MfCastDirectPreviewPlayer,
  MfCastRemux,
  MfCastTranscode,
  MfCastController;

type
  TMfCastLogger = class(TInterfacedObject, IMfCastLogger)
  private
    FOwner: TMfCast;

  public

    constructor Create(const AOwner: TMfCast);

    procedure Detach();
    procedure Log(const ALevel: TMfCastLogLevel;
                  const ASource: string;
                  const AMessage: string);
  end;


constructor TMfCastLogger.Create(const AOwner: TMfCast);
begin

  inherited Create;

  FOwner := AOwner;
end;


procedure TMfCastLogger.Detach();
begin

  FOwner := nil;
end;


procedure TMfCastLogger.Log(const ALevel: TMfCastLogLevel;
                            const ASource: string;
                            const AMessage: string);
begin

  if Assigned(FOwner) then
    FOwner.DispatchLog(ALevel,
                       ASource,
                       AMessage);
end;


constructor TMfCast.Create(const AEnableTranscoding: Boolean);
var
  hr: HRESULT;
  Components: TMfCastComponents;
  Settings: TMfCastSettings;
  Profile: TMfCastDeviceProfile;
  Resolver: IMfCastCapabilityResolver;
  Callbacks: TMfCastControllerCallbacks;

begin

  inherited Create();

  FMediaFoundationStarted := False;
  FPreviewSink := nil;
  FDirectPreviewPlayer := nil;

  // Media inspection and track enumeration also use Media Foundation. Keep
  // transcoding optional, but initialize the platform for every facade.
  hr := MFStartup(MF_VERSION,
                  0);

  if FAILED(Hr) then
    raise EMfCast.CreateFmt('Media Foundation initialization failed (HRESULT $%.8x).',
                            [DWORD(Hr)]);

  FMediaFoundationStarted := True;

  Components.Reset();
  Components.Discovery := TMfCastMdnsDiscovery.Create();
  Components.Channel := TMfCastChannel.Create(TMfCastTcpTransport.Create);
  Components.HttpServer := TMfCastHttpServer.Create();
  Components.MediaInspector := TMfCastMediaInspector.Create();

  Profile.Reset();
  Profile.Name := 'Default Chromecast direct-play profile';

  SetLength(Profile.AllowedContentTypes,
            8);
  Profile.AllowedContentTypes[0] := 'video/mp4';
  Profile.AllowedContentTypes[1] := 'audio/mp4';
  Profile.AllowedContentTypes[2] := 'video/webm';
  Profile.AllowedContentTypes[3] := 'audio/mpeg';
  Profile.AllowedContentTypes[4] := 'audio/aac';
  Profile.AllowedContentTypes[5] := 'audio/flac';
  Profile.AllowedContentTypes[6] := 'audio/wav';
  Profile.AllowedContentTypes[7] := 'audio/ogg';
  Profile.AllowUnknownFormats := True;

  Resolver := TMfCastCapabilityResolver.Create(Profile);
  Components.MediaPlanner := TMfCastMediaPlanner.Create(Resolver);
  FDirectPreviewPlayer := TMfCastDirectPreviewPlayer.Create();
  Components.DirectPreviewPlayer := FDirectPreviewPlayer;

  if AEnableTranscoding then
    begin
      Components.SegmentPublisher := TMfCastSegmentPublisher.Create(Components.HttpServer);
      Components.RemuxPipeline := TMfCastRemuxPipeline.Create();
      Components.TranscodePipeline := TMfCastTranscodePipeline.Create();
      FPreviewSink := TMfCastWindowPreviewSink.Create();
      Components.PreviewSink := FPreviewSink;
    end;

  FController := TMfCastController.Create(Components);
  FLogger := TMfCastLogger.Create(Self);
  FController.SetLogger(FLogger);

  Callbacks.Reset();
  Callbacks.OnDeviceAdded := ControllerDeviceAdded;
  Callbacks.OnDeviceUpdated := ControllerDeviceUpdated;
  Callbacks.OnDeviceRemoved := ControllerDeviceRemoved;
  Callbacks.OnStateChanged := ControllerStateChanged;
  Callbacks.OnMediaStatus := ControllerMediaStatus;
  Callbacks.OnError := ControllerError;
  FController.SetCallbacks(Callbacks);

  Settings := TMfCastSettings.CreateDefault;
  hr := FController.Configure(Settings);
  if FAILED(hr) then
    raise EMfCast.CreateFmt('Chromecast initialization failed (HRESULT $%.8x).',
                            [DWORD(hr)]);
end;


destructor TMfCast.Destroy;
var
  Callbacks: TMfCastControllerCallbacks;

begin

  if Assigned(FController) then
    begin
      Callbacks.Reset();
      FController.SetCallbacks(Callbacks);
      FController.SetLogger(nil);
      FController.StopDiscovery;
      FController.Disconnect;
      FController := nil;
    end;

  if Assigned(FPreviewSink) then
    FPreviewSink.SetWindow(0);
  FPreviewSink := nil;
  if Assigned(FDirectPreviewPlayer) then
    FDirectPreviewPlayer.SetWindow(0);
  FDirectPreviewPlayer := nil;
  FLogger := nil;

  if FMediaFoundationStarted then
    begin
      MFShutdown();
      FMediaFoundationStarted := False;
    end;

  inherited Destroy;
end;


function TMfCast.Discover(): HRESULT;
begin

  Result := FController.StartDiscovery();
end;


function TMfCast.StopDiscovery(): HRESULT;
begin

  Result := FController.StopDiscovery();
end;


function TMfCast.RefreshDiscovery(): HRESULT;
begin

  Result := FController.RefreshDiscovery();
end;


function TMfCast.GetDevices(out ADevices: TMfCastDeviceArray): HRESULT;
begin

  Result := FController.GetDevices(ADevices);
end;


function TMfCast.GetMediaTracks(const ASource: string;
                                out ATracks: TMfCastTrackInfoArray): HRESULT;
begin

  Result := FController.GetMediaTracks(ASource, ATracks);
end;


function TMfCast.SetPreviewWindow(const AWindow: HWND): HRESULT;
var
  SinkResult: HRESULT;

begin

  Result := E_NOTIMPL;

  if Assigned(FDirectPreviewPlayer) then
    Result := FDirectPreviewPlayer.SetWindow(AWindow);

  if Assigned(FPreviewSink) then
    begin
      SinkResult := FPreviewSink.SetWindow(AWindow);
      if FAILED(SinkResult) or (Result = E_NOTIMPL) then
        Result := SinkResult;
    end;
end;


function TMfCast.UpdatePreviewWindow(): HRESULT;
begin

  if Assigned(FDirectPreviewPlayer) then
    Result := FDirectPreviewPlayer.UpdateVideo()
  else
    Result := E_NOTIMPL;
end;


function TMfCast.SetAudioArtwork(const ASourceName: string): HRESULT;
begin

  Result := FController.SetAudioArtwork(ASourceName);
end;


function TMfCast.SetSourceResolver(const AResolver: IMfCastSourceResolver): HRESULT;
begin

  Result := FController.SetSourceResolver(AResolver);
end;


function TMfCast.Cast(const ADevice: TMfCastDevice;
                      const ASource: string): HRESULT;
var
  Subtitle: TMfCastSubtitleAsset;

begin

  Subtitle.Reset();
  Result := Cast(ADevice, 
                 ASource,
                 Subtitle,
                 cmmAutomatic,
                 csmNone,
                 0.0);
end;


function TMfCast.Connect(const ADevice: TMfCastDevice): HRESULT;
begin

  Result := FController.Connect(ADevice);
end;


function TMfCast.Load(const ASource: string): HRESULT;
var
  Subtitle: TMfCastSubtitleAsset;

begin

  Subtitle.Reset();
  Result := Load(ASource, 
                 Subtitle,
                 cmmAutomatic,
                 csmNone,
                 0.0);
end;


function TMfCast.Load(const ASource: string;
                      const ASubtitle: TMfCastSubtitleAsset;
                      const AMediaMode: TMfCastMediaMode;
                      const ASubtitleMode: TMfCastSubtitleMode;
                      const AStartSeconds: Double): HRESULT;
var
  StartTime: Int64;

begin

  StartTime := Round(AStartSeconds * 10000000.0);
  if (StartTime < 0) then
    StartTime := 0;

  Result := FController.LoadFile(ASource,
                                 ASubtitle,
                                 AMediaMode,
                                 ASubtitleMode,
                                 StartTime);
end;


function TMfCast.Cast(const ADevice: TMfCastDevice;
                      const ASource: string;
                      const ASubtitle: TMfCastSubtitleAsset;
                      const AMediaMode: TMfCastMediaMode;
                      const ASubtitleMode: TMfCastSubtitleMode;
                      const AStartSeconds: Double): HRESULT;
var
  StartTime: Int64;

begin

  StartTime := Round(AStartSeconds * 10000000.0);
  if (StartTime < 0) then
    StartTime := 0;

  Result := FController.CastFile(ADevice,
                                 ASource,
                                 ASubtitle,
                                 AMediaMode,
                                 ASubtitleMode,
                                 StartTime);
end;


function TMfCast.CastLiveFragmentedMp4(const ADevice: TMfCastDevice;
                                       const AInitSegment: TBytes;
                                       out AByteStream: IMFByteStream): HRESULT;
begin

  Result := FController.CastLiveFragmentedMp4(ADevice,
                                              AInitSegment,
                                              AByteStream);
end;


function TMfCast.Play(): HRESULT;
begin

  Result := FController.Play();
end;


function TMfCast.Pause(): HRESULT;
begin

  Result := FController.Pause();
end;


function TMfCast.Stop(): HRESULT;
begin

  Result := FController.Stop();
end;


function TMfCast.Seek(const APositionSeconds: Double): HRESULT;
var
  Position: Int64;

begin

  Position := Round(APositionSeconds * 10000000.0);

  if (Position < 0) then
    Position := 0;
  Result := FController.Seek(Position);
end;


function TMfCast.SelectAudioTrack(const ATrackId: Int64): HRESULT;
begin

  Result := FController.SelectAudioTrack(ATrackId);
end;


function TMfCast.SelectSubtitle(const ASubtitle: TMfCastSubtitleAsset): HRESULT;
begin

  Result := FController.SelectSubtitle(ASubtitle);
end;


function TMfCast.SelectSubtitle(const ATrackId: Int64): HRESULT;
begin

  Result := FController.SelectSubtitleTrack(ATrackId);
end;


function TMfCast.DisableSubtitles(): HRESULT;
begin

  Result := FController.DisableSubtitles();
end;


function TMfCast.SetVolume(const AVolume: Single): HRESULT;
var
  Volume: Single;

begin

  Volume := AVolume;

  if (Volume < 0.0) then
    Volume := 0.0
  else
    if (Volume > 1.0) then
      Volume := 1.0;
  Result := FController.SetVolume(Volume);
end;


function TMfCast.SetMuted(const AMuted: Boolean): HRESULT;
begin

  Result := FController.SetMuted(AMuted);
end;


function TMfCast.Disconnect(): HRESULT;
begin

  Result := FController.Disconnect();
end;


function TMfCast.State(): TMfCastState;
begin

  Result := FController.GetState();
end;


procedure TMfCast.ControllerDeviceAdded(const ADevice: TMfCastDevice);
begin

  if Assigned(FOnDeviceAdded) then
    FOnDeviceAdded(ADevice);
end;


procedure TMfCast.ControllerDeviceUpdated(const ADevice: TMfCastDevice);
begin

  if Assigned(FOnDeviceUpdated) then
    FOnDeviceUpdated(ADevice);
end;


procedure TMfCast.ControllerDeviceRemoved(const ADeviceId: string);
begin

  if Assigned(FOnDeviceRemoved) then
    FOnDeviceRemoved(ADeviceId);
end;


procedure TMfCast.ControllerStateChanged(const AOldState: TMfCastState;
                                         const ANewState: TMfCastState);
begin

  if Assigned(FOnStateChanged) then
    FOnStateChanged(AOldState, 
                    ANewState);
end;


procedure TMfCast.ControllerMediaStatus(const AStatus: TMfCastMediaStatus);
begin

  if Assigned(FOnMediaStatus) then
    FOnMediaStatus(AStatus);
end;


procedure TMfCast.ControllerError(const AError: TMfCastErrorInfo);
begin

  if Assigned(FOnError) then
    FOnError(AError);
end;


procedure TMfCast.DispatchLog(const ALevel: TMfCastLogLevel;
                              const ASource: string;
                              const AMessage: string);
var
  Text: string;

begin

  Text := Format('[MfCast][%s][%s] %s',
                 [MfCastLogLevelToString(ALevel),
                  ASource,
                  AMessage]);

  OutputDebugString(PChar(Text));

  if Assigned(FOnLog) then
    FOnLog(Self,
           ALevel,
           ASource,
           AMessage);
end;

end.
