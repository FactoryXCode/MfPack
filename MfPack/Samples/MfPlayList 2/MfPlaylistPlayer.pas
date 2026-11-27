// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPlaylistPlayer.pas
// Kind: Pascal / Delphi unit
// Release date: 26-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: A playlist built with the native Media Foundation sequencer source.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (Carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          This unit uses MfPack's Windows/COM/Media Foundation declarations.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/how-to-create-a-playlist
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
unit MfPlaylistPlayer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.Evr;

const
  WM_MFPLAYLIST_EVENT = WM_APP + 51;

type

  TMfPlaylistState = (plsEmpty,
                      plsBuilding,
                      plsReady,
                      plsStarting,
                      plsPlaying,
                      plsPaused,
                      plsStopping,
                      plsStopped,
                      plsEnded,
                      plsError
                     );

  TMfPlaylistLogEvent = procedure(const AText: string) of object;

  // Each posted packet owns its event interface. The window must free it,
  // including packets belonging to an older playlist generation.
  TMfPlaylistEventPacket = class

  public
    Generation: NativeUInt;
    Status: HRESULT;
    Event: IMFMediaEvent;
  end;

  TMfPlaylistSessionEvents = class(TInterfacedObject,
                                   IMFAsyncCallback)

  private
    FSession: IMFMediaSession;
    FWindow: HWND;
    FGeneration: NativeUInt;
    FLock: TCriticalSection;
    FClosed: TEvent;

    procedure PostEvent(const AEvent: IMFMediaEvent;
                        const AStatus: HRESULT);

  public

    constructor Create(const ASession: IMFMediaSession;
                       const AWindow: HWND;
                       const AGeneration: NativeUInt);

    destructor Destroy(); override;

    procedure DetachWindow();
    function WaitForClosed(const ATimeoutMs: Cardinal): Boolean;
    function GetParameters(out AFlags: DWORD;
                           out AQueue: DWORD): HRESULT; stdcall;
    function Invoke(AResult: IMFAsyncResult): HRESULT; stdcall;
  end;

  TMfPlaylistSegment = record
    FileName: string;
    ElementId: MFSequencerElementId;
    TopologyId: TOPOID;
    Duration100ns: Int64;
    HasVideo: Boolean;
    Source: IMFMediaSource;
  end;

  TMfPlaylistSegments = array of TMfPlaylistSegment;

  TMfPlaylistPlayer = class

  private
    FNotifyWindow: HWND;
    FVideoWindow: HWND;
    FSession: IMFMediaSession;
    FSequencer: IMFSequencerSource;
    FSequenceSource: IMFMediaSource;
    FProvider: IMFMediaSourceTopologyProvider;
    FClock: IMFPresentationClock;
    FVideoDisplay: IMFVideoDisplayControl;
    FAudioActivate: IMFActivate;
    FVideoActivate: IMFActivate;
    FCallback: IMFAsyncCallback;
    FCallbackObject: TMfPlaylistSessionEvents;
    FSegments: TMfPlaylistSegments;
    FState: TMfPlaylistState;
    FCurrentIndex: Integer;
    FGeneration: NativeUInt;
    FPresentationOffset: Int64;
    FStartedMF: Boolean;
    FLastError: string;
    FOnLog: TMfPlaylistLogEvent;
    FQueuedCount: Integer;
    FTransitionCount: Integer;

    procedure Log(const AText: string);
    procedure Fail(const AOperation: string;
                   const AStatus: HRESULT);
    procedure AddSegment(const AFileName: string);
    function CreateTopology(const ASource: IMFMediaSource;
                            const APresentation: IMFPresentationDescriptor;
                            out ATopology: IMFTopology;
                            out AHasVideo: Boolean): HRESULT;
    function QueuePresentation(const APresentation: IMFPresentationDescriptor): HRESULT;
    function EventObject(const AEvent: IMFMediaEvent;
                         out AObject: IUnknown): HRESULT;
    procedure TopologyStatus(const AEvent: IMFMediaEvent);
    procedure AcquireVideoDisplay();
    function GetCount(): Integer;
    function GetHasPlaylist(): Boolean;

  public

    constructor Create(const ANotifyWindow: HWND;
                       const AVideoWindow: HWND);

    destructor Destroy(); override;

    // Call these methods and ProcessEvent on the creating/UI thread.
    // Only the session callback runs on an MF worker; it posts owned packets.
    function BuildPlaylist(const AFiles: TStrings): HRESULT;
    procedure ClosePlaylist();
    procedure ProcessEvent(const APacket: TMfPlaylistEventPacket);
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;

    function PlaySegment(const AIndex: Integer;
                         const AOffset100ns: Int64 = 0): HRESULT;

    function GetPlaybackTime(out APresentation100ns: Int64;
                             out ASegment100ns: Int64;
                             out ADuration100ns: Int64): HRESULT;
    procedure ResizeVideo();
    procedure RepaintVideo();
    function SegmentId(const AIndex: Integer): MFSequencerElementId;
    function SegmentFileName(const AIndex: Integer): string;
    function SegmentHasVideo(const AIndex: Integer): Boolean;

    property State: TMfPlaylistState read FState;
    property CurrentIndex: Integer read FCurrentIndex;
    property Count: Integer read GetCount;
    property HasPlaylist: Boolean read GetHasPlaylist;
    property LastError: string read FLastError;
    property QueuedCount: Integer read FQueuedCount;
    property TransitionCount: Integer read FTransitionCount;
    property OnLog: TMfPlaylistLogEvent read FOnLog write FOnLog;
    // Set before building, after the host has created its current video HWND.
    property VideoWindow: HWND read FVideoWindow write FVideoWindow;
  end;

function MfPlaylistStateText(const AState: TMfPlaylistState): string;


implementation

uses

  {WinApi}
  WinApi.WinError,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropVarUtil,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib;

type

  EMfPlaylist = class(Exception)

  public
    Code: HRESULT;

    constructor CreateStatus(const AOperation: string;
                             const ACode: HRESULT);
  end;


procedure Check(const AStatus: HRESULT;
                const AOperation: string);

begin

  if FAILED(AStatus) then
    raise EMfPlaylist.CreateStatus(AOperation,
                                   AStatus);
end;


constructor EMfPlaylist.CreateStatus(const AOperation: string;
                                     const ACode: HRESULT);

begin

  inherited Create(Format('%s failed (HRESULT $%.8x).',
                          [AOperation, DWORD(ACode)]));
  Code := ACode;
end;


function MfPlaylistStateText(const AState: TMfPlaylistState): string;

begin

  case AState of
    plsEmpty: Result := 'Empty';
    plsBuilding: Result := 'Building';
    plsReady: Result := 'Ready';
    plsStarting: Result := 'Starting';
    plsPlaying: Result := 'Playing';
    plsPaused: Result := 'Paused';
    plsStopping: Result := 'Stopping';
    plsStopped: Result := 'Stopped';
    plsEnded: Result := 'Completed';
    plsError: Result := 'Error';
    else
      Result := 'Unknown';
  end;
end;


constructor TMfPlaylistSessionEvents.Create(const ASession: IMFMediaSession;
                                            const AWindow: HWND;
                                            const AGeneration: NativeUInt);

begin

  inherited Create();

  FLock := TCriticalSection.Create();
  FClosed := TEvent.Create(nil,
                           True,
                           False,
                           '');
  FSession := ASession;
  FWindow := AWindow;
  FGeneration := AGeneration;
end;


destructor TMfPlaylistSessionEvents.Destroy();

begin

  FSession := nil;
  FClosed.Free();
  FLock.Free();

  inherited Destroy();
end;


procedure TMfPlaylistSessionEvents.DetachWindow();

begin

  FLock.Acquire();

  try
    FWindow := 0;

  finally
    FLock.Release();
  end;
end;


function TMfPlaylistSessionEvents.WaitForClosed(const ATimeoutMs: Cardinal): Boolean;

begin

  Result := (FClosed.WaitFor(ATimeoutMs) = wrSignaled);
end;


procedure TMfPlaylistSessionEvents.PostEvent(const AEvent: IMFMediaEvent;
                                             const AStatus: HRESULT);
var
  Packet: TMfPlaylistEventPacket;

begin

  Packet := TMfPlaylistEventPacket.Create();
  Packet.Event := AEvent;
  Packet.Status := AStatus;
  Packet.Generation := FGeneration;
  FLock.Acquire();

  try
    if (FWindow = 0) or
      (not PostMessage(FWindow,
                       WM_MFPLAYLIST_EVENT,
                       0,
                       LPARAM(Packet))) then
      Packet.Free();

  finally
    FLock.Release();
  end;
end;


function TMfPlaylistSessionEvents.GetParameters(out AFlags: DWORD;
                                                out AQueue: DWORD): HRESULT;

begin

  AFlags := 0;
  AQueue := MFASYNC_CALLBACK_QUEUE_STANDARD;
  Result := E_NOTIMPL;
end;


function TMfPlaylistSessionEvents.Invoke(AResult: IMFAsyncResult): HRESULT;
var
  Event: IMFMediaEvent;
  EventType: MediaEventType;
  Status: HRESULT;

begin

  // EndGetEvent and BeginGetEvent stay on the worker. No VCL control is touched.
  // MESessionClosed is signalled here so ClosePlaylist can wait without pumping UI.
  try
    Status := FSession.EndGetEvent(AResult,
                                   Event);
    if FAILED(Status) then
      begin

        PostEvent(nil,
                  Status);
        FClosed.SetEvent();
        Exit(S_OK);
      end;

    Status := Event.GetType(EventType);

    if FAILED(Status) then
      begin

        PostEvent(nil,
                  Status);
        FClosed.SetEvent();
        Exit(S_OK);
      end;

    if (EventType = MESessionClosed) then
      begin

        FClosed.SetEvent();
        Exit(S_OK);
      end;

    PostEvent(Event,
              S_OK);
    Status := FSession.BeginGetEvent(Self,
                                     nil);
    if FAILED(Status) then
      begin

        PostEvent(nil,
                  Status);
        FClosed.SetEvent();
      end;

  except
    PostEvent(nil,
              E_FAIL);
    FClosed.SetEvent();
  end;

  Result := S_OK;
end;


constructor TMfPlaylistPlayer.Create(const ANotifyWindow: HWND;
                                     const AVideoWindow: HWND);

begin

  inherited Create();

  FNotifyWindow := ANotifyWindow;
  FVideoWindow := AVideoWindow;
  FCurrentIndex := -1;

  Check(MFStartup(MF_VERSION,
                  MFSTARTUP_FULL),
        'MFStartup');

  FStartedMF := True;
end;


destructor TMfPlaylistPlayer.Destroy();

begin

  FOnLog := nil;
  ClosePlaylist();

  if FStartedMF then
    MFShutdown();

  inherited Destroy();
end;


procedure TMfPlaylistPlayer.Log(const AText: string);

begin

  if Assigned(FOnLog) then
    FOnLog(AText);
end;


procedure TMfPlaylistPlayer.Fail(const AOperation: string;
                                 const AStatus: HRESULT);

begin

  FLastError := Format('%s failed (HRESULT $%.8x).',
                       [AOperation, DWORD(AStatus)]);
  FState := plsError;
  Log(FLastError);

  if Assigned(FSession) then
    FSession.Stop();
end;


function TMfPlaylistPlayer.CreateTopology(const ASource: IMFMediaSource;
                                          const APresentation: IMFPresentationDescriptor;
                                          out ATopology: IMFTopology;
                                          out AHasVideo: Boolean): HRESULT;
var
  StreamCount: DWORD;
  I: DWORD;
  Selected: BOOL;
  Stream: IMFStreamDescriptor;
  Handler: IMFMediaTypeHandler;
  MajorType: TGUID;
  InputNode: IMFTopologyNode;
  OutputNode: IMFTopologyNode;
  Activate: IMFActivate;
  HaveAudio: Boolean;
  HaveVideo: Boolean;

begin

  ATopology := nil;
  AHasVideo := False;
  Result := MFCreateTopology(ATopology);

  if FAILED(Result) then
    Exit;

  Result := APresentation.GetStreamDescriptorCount(StreamCount);

  if FAILED(Result) then
    Exit;

  HaveAudio := False;
  HaveVideo := False;

  if (StreamCount > 0) then
    for I := 0 to StreamCount - 1 do
      begin

        Result := APresentation.GetStreamDescriptorByIndex(I,
                                                           Selected,
                                                           Stream);
        if FAILED(Result) then
          Exit;

        if not Selected then
          Continue;

        Result := Stream.GetMediaTypeHandler(Handler);
        if FAILED(Result) then
          Exit;

        Result := Handler.GetMajorType(MajorType);
        if FAILED(Result) then
          Exit;

        Activate := nil;
        if IsEqualGUID(MajorType,
          MFMediaType_Audio) and (not HaveAudio) then
          begin

            if not Assigned(FAudioActivate) then
              begin

                Result := MFCreateAudioRendererActivate(FAudioActivate);

                if FAILED(Result) then
                  Exit;
              end;

            Activate := FAudioActivate;
            HaveAudio := True;
          end
        else if IsEqualGUID(MajorType,
          MFMediaType_Video) and (not HaveVideo) then
          begin

            if (FVideoWindow = 0) then
              Exit(E_INVALIDARG);

            if not Assigned(FVideoActivate) then
              begin

                Result := MFCreateVideoRendererActivate(FVideoWindow,
                                                        FVideoActivate);
                if FAILED(Result) then
                  Exit;
              end;

            Activate := FVideoActivate;
            HaveVideo := True;
          end;

        // The learning sample renders one selected audio and video stream.
        // Text/metadata and duplicate selected streams must not remain selected.
        if not Assigned(Activate) then
          begin

            Result := APresentation.DeselectStream(I);

            if FAILED(Result) then
              Exit;
            Continue;
          end;

        // Step 4: a partial topology lets the topology loader insert decoders.
        Result := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                                       InputNode);
        if FAILED(Result) then
          Exit;
        Result := InputNode.SetUnknown(MF_TOPONODE_SOURCE,
                                       ASource);
        if FAILED(Result) then
          Exit;
        Result := InputNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                                       APresentation);
        if FAILED(Result) then
          Exit;
        Result := InputNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                                       Stream);
        if FAILED(Result) then
          Exit;

        Result := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                                       OutputNode);
        if FAILED(Result) then
          Exit;

        Result := OutputNode.SetObject(Activate);
        if FAILED(Result) then
          Exit;

        Result := OutputNode.SetUINT32(MF_TOPONODE_STREAMID,
                                       0);
        if FAILED(Result) then
          Exit;

        Result := ATopology.AddNode(InputNode);
        if FAILED(Result) then
          Exit;

        Result := ATopology.AddNode(OutputNode);
        if FAILED(Result) then
          Exit;
        Result := InputNode.ConnectOutput(0,
                                          OutputNode,
                                          0);
        if FAILED(Result) then
          Exit;
      end;

  if not (HaveAudio or HaveVideo) then
    Result := MF_E_INVALIDMEDIATYPE
  else
    begin

      AHasVideo := HaveVideo;
      Result := S_OK;
    end;
end;


procedure TMfPlaylistPlayer.AddSegment(const AFileName: string);
var
  Resolver: IMFSourceResolver;
  ObjectType: MF_OBJECT_TYPE;
  Unknown: IUnknown;
  Source: IMFMediaSource;
  Presentation: IMFPresentationDescriptor;
  Topology: IMFTopology;
  Id: MFSequencerElementId;
  TopologyId: TOPOID;
  Duration: UInt64;
  Index: Integer;
  Appended: Boolean;
  HasVideo: Boolean;

begin

  Appended := False;

  try
    // Step 3: resolve the native source and inspect its presentation.
    Check(MFCreateSourceResolver(Resolver),
          'MFCreateSourceResolver');

    Check(Resolver.CreateObjectFromURL(PWideChar(AFileName),
                                       MF_RESOLUTION_MEDIASOURCE,
                                       nil,
                                       ObjectType,
                                       Unknown),
          'Resolve ' + AFileName);

    Check(Unknown.QueryInterface(IID_IMFMediaSource,
                                 Source),
          'Query IMFMediaSource');

    Check(Source.CreatePresentationDescriptor(Presentation),
          'CreatePresentationDescriptor');

    Check(CreateTopology(Source,
                         Presentation,
                         Topology,
                         HasVideo),
          'Create playback topology for ' + ExtractFileName(AFileName));

    Check(Topology.GetTopologyID(TopologyId),
          'GetTopologyID');

    // Step 5: exactly one segment carries the Last flag.
    Index := Length(FSegments);

    if (Index > 0) then
      Check(FSequencer.UpdateTopologyFlags(FSegments[Index - 1].ElementId,
                                           0),
            'Clear previous Last flag');

    Check(FSequencer.AppendTopology(Topology,
                                    DWORD(SequencerTopologyFlags_Last),
                                    Id),
          'AppendTopology');

    Appended := True;
    Duration := 0;
    Presentation.GetUINT64(MF_PD_DURATION,
                           Duration);
    SetLength(FSegments,
              Index + 1);
    FSegments[Index].FileName := AFileName;
    FSegments[Index].ElementId := Id;
    FSegments[Index].TopologyId := TopologyId;
    FSegments[Index].Duration100ns := Int64(Duration);
    FSegments[Index].HasVideo := HasVideo;
    FSegments[Index].Source := Source;

    Log(Format('AppendTopology: item %d, segment %d, topology %s, Last flag set: %s',
               [Index + 1, Id, UIntToStr(TopologyId), ExtractFileName(AFileName)]));

  finally
    // Once appended, sequencer Shutdown owns the native-source shutdown.
    if Assigned(Source) and (not Appended) then
      Source.Shutdown();
  end;
end;


function TMfPlaylistPlayer.QueuePresentation(const APresentation: IMFPresentationDescriptor): HRESULT;
var
  Topology: IMFTopology;

begin

  // Steps 6 and 7 use the provider's rewritten topology, never the original
  // native-source topology stored in the playlist construction loop.
  Result := FProvider.GetMediaSourceTopology(APresentation,
                                             Topology);
  if FAILED(Result) then
    Exit;

  Result := FSession.SetTopology(0,
                                 Topology);
  if SUCCEEDED(Result) then
    begin

      Inc(FQueuedCount);
      Log(Format('SetTopology: queued presentation %d through IMFMediaSourceTopologyProvider.',
                 [FQueuedCount]));
    end;
end;


function TMfPlaylistPlayer.BuildPlaylist(const AFiles: TStrings): HRESULT;
var
  I: Integer;
  Presentation: IMFPresentationDescriptor;
  Clock: IMFClock;
  Detail: string;

begin

  ClosePlaylist();
  FLastError := '';
  if (AFiles = nil) or (AFiles.Count = 0) then
    Exit(E_INVALIDARG);

  FState := plsBuilding;

  try
    // Steps 1 and 2: MFStartup is paired with MFShutdown in this object's lifetime.
    Check(MFCreateMediaSession(nil,
                               FSession),
          'MFCreateMediaSession');

    Check(MFCreateSequencerSource(nil,
                                  FSequencer),
          'MFCreateSequencerSource');

    Check(FSequencer.QueryInterface(IID_IMFMediaSource,
                                    FSequenceSource),
          'Query sequencer IMFMediaSource');

    Check(FSequencer.QueryInterface(IID_IMFMediaSourceTopologyProvider,
                                    FProvider),
          'Query IMFMediaSourceTopologyProvider');

    FCallbackObject := TMfPlaylistSessionEvents.Create(FSession,
                                                       FNotifyWindow,
                                                       FGeneration);
    FCallback := FCallbackObject;

    Check(FSession.BeginGetEvent(FCallback,
                                 nil),
          'BeginGetEvent');

    Check(FSession.GetClock(Clock),
          'GetClock');

    Check(Clock.QueryInterface(IID_IMFPresentationClock,
                               FClock),
          'Query presentation clock');

    for I := 0 to AFiles.Count - 1 do
      AddSegment(AFiles[I]);

    // The sequencer's presentation descriptor selects the first segment.
    Check(FSequenceSource.CreatePresentationDescriptor(Presentation),
          'Create sequencer presentation descriptor');
    Check(QueuePresentation(Presentation),
          'Queue first presentation');
    Log('First topology submitted. Waiting for MF_TOPOSTATUS_READY.');
    Result := S_OK;

  except
    on E: Exception do
      begin

        if (E is EMfPlaylist) then
          Result := EMfPlaylist(E).Code
        else if (E is EOutOfMemory) then
          Result := E_OUTOFMEMORY
        else
          Result := E_FAIL;

        Detail := E.Message;
        ClosePlaylist();
        FLastError := Detail;
        FState := plsError;
        Log(Detail);
      end;
  end;
end;


procedure TMfPlaylistPlayer.ClosePlaylist();
var
  CloseResult: HRESULT;

begin

  Inc(FGeneration);
  if Assigned(FCallbackObject) then
    FCallbackObject.DetachWindow();

  FVideoDisplay := nil;
  FClock := nil;

  // Step 8: sequencer Shutdown shuts down the native media sources it cached.
  if Assigned(FSequenceSource) then
    FSequenceSource.Shutdown();

  if Assigned(FSession) then
    begin

      CloseResult := FSession.Close();

      if SUCCEEDED(CloseResult) and Assigned(FCallbackObject) then
        if not FCallbackObject.WaitForClosed(5000) then
          Log('MESessionClosed wait expired; shutting down the session.');

      FSession.Shutdown();
    end;

  FProvider := nil;
  FSequencer := nil;
  FSequenceSource := nil;
  FSession := nil;
  FCallbackObject := nil;
  FCallback := nil;
  FAudioActivate := nil;
  FVideoActivate := nil;
  SetLength(FSegments,
            0);
  FCurrentIndex := -1;
  FQueuedCount := 0;
  FTransitionCount := 0;
  FPresentationOffset := 0;
  FState := plsEmpty;
end;


function TMfPlaylistPlayer.EventObject(const AEvent: IMFMediaEvent;
                                       out AObject: IUnknown): HRESULT;

begin

  AObject := nil;

  // MfPack's helper reads the VT_UNKNOWN event value, takes an independent
  // interface reference and clears the native PROPVARIANT.
  Result := WinApi.MediaFoundationApi.MfMetLib.GetEventObject(AEvent,
                                                              AObject);
  if SUCCEEDED(Result) and (not Assigned(AObject)) then
    Result := E_POINTER;
end;


procedure TMfPlaylistPlayer.TopologyStatus(const AEvent: IMFMediaEvent);
var
  Status: UINT32;
  Unknown: IUnknown;
  Topology: IMFTopology;
  Id: TOPOID;
  Index: Integer;

begin

  if FAILED(AEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
    Status)) then
    Exit;

  Log(Format('MESessionTopologyStatus: %d.',
             [Status]));
  if (Status = DWORD(MF_TOPOSTATUS_READY)) then
    begin

      AcquireVideoDisplay();

      if (FState = plsBuilding) then
        FState := plsReady;
    end;

  if (Status <> DWORD(MF_TOPOSTATUS_STARTED_SOURCE)) and
    (Status <> DWORD(MF_TOPOSTATUS_SINK_SWITCHED)) then
    Exit;

  if FAILED(EventObject(AEvent,
    Unknown)) or
    (not Supports(Unknown,
                  IMFTopology,
                  Topology)) then
    Exit;

  if FAILED(Topology.GetTopologyID(Id)) then
    Exit;

  for Index := 0 to High(FSegments) do
    if (FSegments[Index].TopologyId = Id) then
      begin

        // STARTED_SOURCE can describe preroll. SINK_SWITCHED is the actual
        // handoff; only the first source needs STARTED_SOURCE as an initial cue.
        if (Status = DWORD(MF_TOPOSTATUS_SINK_SWITCHED)) or (FCurrentIndex < 0) then
          begin

            if (FCurrentIndex <> Index) then
              begin

                FCurrentIndex := Index;
                Inc(FTransitionCount);
                Log(Format('Active item %d: %s',
                           [Index + 1, ExtractFileName(FSegments[Index].FileName)]));
              end;
            AcquireVideoDisplay();
          end;
        Break;
      end;
end;


procedure TMfPlaylistPlayer.ProcessEvent(const APacket: TMfPlaylistEventPacket);
var
  EventType: MediaEventType;
  Status: HRESULT;
  OperationStatus: HRESULT;
  Unknown: IUnknown;
  Presentation: IMFPresentationDescriptor;
  Offset: UInt64;

begin

  if (APacket = nil) or (APacket.Generation <> FGeneration) or
    (not Assigned(FSession)) or (FState = plsError) then
    Exit;

  if FAILED(APacket.Status) then
    begin

      Fail('Read session event',
           APacket.Status);
      Exit;
    end;

  Status := APacket.Event.GetStatus(OperationStatus);
  if SUCCEEDED(Status) then
    Status := OperationStatus;

  if FAILED(Status) then
    begin

      Fail('Media Session event',
           Status);
      Exit;
    end;

  if FAILED(APacket.Event.GetType(EventType)) then
    Exit;

  case EventType of
    MESessionTopologyStatus: TopologyStatus(APacket.Event);

    MENewPresentation:
      begin

        Log('MENewPresentation: obtain the next descriptor and queue its topology.');
        Status := EventObject(APacket.Event,
                              Unknown);
        if SUCCEEDED(Status) then
          Status := Unknown.QueryInterface(IID_IMFPresentationDescriptor,
                                           Presentation);
        if SUCCEEDED(Status) then
          Status := QueuePresentation(Presentation);

        if FAILED(Status) then
          Fail('Queue next presentation',
               Status);
      end;

    MESessionStarted:
      begin

        FState := plsPlaying;
        Log('MESessionStarted');
      end;

    MESessionPaused:
      begin

        FState := plsPaused;
        Log('MESessionPaused');
      end;

    MESessionStopped:
      begin

        if (FState <> plsError) then
          FState := plsStopped;
        Log('MESessionStopped');
      end;

    MESessionEnded:
      begin

        FState := plsEnded;
        Log('MESessionEnded: the last playlist segment has completed.');
      end;

    MEEndOfPresentationSegment: Log('MEEndOfPresentationSegment');

    MESessionNotifyPresentationTime:
      begin

        if SUCCEEDED(APacket.Event.GetUINT64(MF_EVENT_PRESENTATION_TIME_OFFSET,
          Offset)) then
          FPresentationOffset := Int64(Offset);
      end;
  end;
end;


function TMfPlaylistPlayer.Play(): HRESULT;
var
  Position: PROPVARIANT;

begin

  if not Assigned(FSession) or
    (not (FState in [plsReady, plsPaused, plsStopped, plsEnded])) then
    Exit(MF_E_INVALIDREQUEST);

  if FState in [plsStopped, plsEnded] then
    Exit(PlaySegment(0));

  PropVariantInit(Position);

  try
    Result := FSession.Start(GUID_NULL,
                             Position);
    if SUCCEEDED(Result) then
      FState := plsStarting;

  finally
    PropVariantClear(Position);
  end;
end;


function TMfPlaylistPlayer.Pause(): HRESULT;

begin

  if (not Assigned(FSession)) or (FState <> plsPlaying) then
    Exit(MF_E_INVALIDREQUEST);

  Result := FSession.Pause();
end;


function TMfPlaylistPlayer.Stop(): HRESULT;

begin

  if not Assigned(FSession) then
    Exit(MF_E_INVALIDREQUEST);

  Result := FSession.Stop();

  if SUCCEEDED(Result) then
    FState := plsStopping;
end;


function TMfPlaylistPlayer.PlaySegment(const AIndex: Integer;
                                       const AOffset100ns: Int64): HRESULT;
var
  Position: PROPVARIANT;

begin

  if (AIndex < 0) or (AIndex >= Length(FSegments)) or (AOffset100ns < 0) or
    (not Assigned(FSession)) or
    (not (FState in [plsReady, plsPlaying, plsPaused, plsStopped, plsEnded])) then
    Exit(E_INVALIDARG);

  if (FSegments[AIndex].Duration100ns > 0) and
    (AOffset100ns >= FSegments[AIndex].Duration100ns) then
    Exit(E_INVALIDARG);

  PropVariantInit(Position);

  try
    Result := MFCreateSequencerSegmentOffset(FSegments[AIndex].ElementId,
                                             AOffset100ns,
                                             Position);
    if SUCCEEDED(Result) then
      Result := FSession.Start(MF_TIME_FORMAT_SEGMENT_OFFSET,
                               Position);
    if SUCCEEDED(Result) then
      begin

        FState := plsStarting;
        Log(Format('Start segment %d through MF_TIME_FORMAT_SEGMENT_OFFSET.',
                   [AIndex + 1]));
      end;

  finally
    PropVariantClear(Position);
  end;
end;


procedure TMfPlaylistPlayer.AcquireVideoDisplay();

begin

  FVideoDisplay := nil;
  // Failure is normal for an audio-only topology.
  if SUCCEEDED(MFGetService(FSession,
    MR_VIDEO_RENDER_SERVICE,
    IID_IMFVideoDisplayControl,
    Pointer(FVideoDisplay))) then
    begin

      FVideoDisplay.SetAspectRatioMode(DWORD(MFVideoARMode_PreservePicture));
      ResizeVideo();
    end;
end;


procedure TMfPlaylistPlayer.ResizeVideo();
var
  Bounds: TRect;

begin

  if Assigned(FVideoDisplay) and (FVideoWindow <> 0) then
    begin

      GetClientRect(FVideoWindow,
                    Bounds);
      FVideoDisplay.SetVideoPosition(nil,
                                     @Bounds);
    end;
end;


procedure TMfPlaylistPlayer.RepaintVideo();

begin

  if Assigned(FVideoDisplay) then
    FVideoDisplay.RepaintVideo();
end;


function TMfPlaylistPlayer.GetPlaybackTime(out APresentation100ns: Int64;
                                           out ASegment100ns: Int64;
                                           out ADuration100ns: Int64): HRESULT;

begin

  APresentation100ns := 0;
  ASegment100ns := 0;
  ADuration100ns := 0;
  if (FCurrentIndex >= 0) and (FCurrentIndex < Length(FSegments)) then
    ADuration100ns := FSegments[FCurrentIndex].Duration100ns;

  if not Assigned(FClock) then
    Exit(S_FALSE);

  Result := FClock.GetTime(APresentation100ns);

  if SUCCEEDED(Result) then
    begin

      ASegment100ns := APresentation100ns - FPresentationOffset;
      if (ASegment100ns < 0) then
        ASegment100ns := 0;
    end;
end;


function TMfPlaylistPlayer.GetCount(): Integer;

begin

  Result := Length(FSegments);
end;


function TMfPlaylistPlayer.GetHasPlaylist(): Boolean;

begin

  Result := Assigned(FSession) and (Length(FSegments) > 0);
end;


function TMfPlaylistPlayer.SegmentId(const AIndex: Integer): MFSequencerElementId;

begin

  if (AIndex < 0) or (AIndex >= Length(FSegments)) then
    raise ERangeError.Create('Playlist item is out of range.');

  Result := FSegments[AIndex].ElementId;
end;


function TMfPlaylistPlayer.SegmentFileName(const AIndex: Integer): string;

begin

  if (AIndex < 0) or (AIndex >= Length(FSegments)) then
    raise ERangeError.Create('Playlist item is out of range.');

  Result := FSegments[AIndex].FileName;
end;


function TMfPlaylistPlayer.SegmentHasVideo(const AIndex: Integer): Boolean;

begin

  if (AIndex < 0) or (AIndex >= Length(FSegments)) then
    raise ERangeError.Create('Playlist item is out of range.');

  Result := FSegments[AIndex].HasVideo;
end;

end.
