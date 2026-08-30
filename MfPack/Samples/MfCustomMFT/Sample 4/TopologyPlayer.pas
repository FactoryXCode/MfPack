// FactoryX
//
// A small video-only Media Session player. The custom grayscale MFT is placed
// directly on a topology node; it is not registered with Windows.

unit TopologyPlayer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.Evr;

const
  WM_TOPOLOGY_PLAYER_EVENT = WM_APP + 401;

type
  TTopologyEventKind = (tekTopologyReady,
                        tekStarted,
                        tekPaused,
                        tekStopped,
                        tekEnded,
                        tekError);

  TTopologyEventNotice = class
  public
    Session: Cardinal;
    Kind: TTopologyEventKind;
    Text: string;

    constructor Create(const ASession: Cardinal;
                       const AKind: TTopologyEventKind;
                       const AText: string);
  end;

  ITopologyPlayer = interface
  ['{02E1054F-C6A5-45B8-BAA1-F6CBAAD17131}']
    procedure Open(const AFileName: string);
    procedure Start();
    procedure Pause();
    procedure Stop();
    procedure ResizeVideo(const AWidth, AHeight: Integer);
    procedure Repaint();
    procedure Close(const ADetachWindow: Boolean);
  end;

function CreateTopologyPlayer(const AVideoWindow: HWND;
                              const ANotifyWindow: HWND;
                              const ASession: Cardinal): ITopologyPlayer;


implementation

uses
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfGrayscaleMFT;

type
  TTopologyPlayer = class(TInterfacedObject, ITopologyPlayer, IMFAsyncCallback)
  private
    FLock: TRTLCriticalSection;
    FVideoWindow: HWND;
    FNotifyWindow: HWND;
    FSessionNumber: Cardinal;
    FSession: IMFMediaSession;
    FSource: IMFMediaSource;
    FTransform: IMFTransform;
    FVideoDisplay: IMFVideoDisplayControl;
    FClosedEvent: THandle;

    procedure Check(const AOperation: string; const AHr: HRESULT);
    function CreateMediaSource(const AFileName: string;
      out ASource: IMFMediaSource): HRESULT;
    function CreatePlaybackTopology(const ASource: IMFMediaSource;
      out ATopology: IMFTopology; out ATransform: IMFTransform): HRESULT;
    procedure PostNotice(const AKind: TTopologyEventKind;
      const AText: string);
    procedure AcquireVideoDisplay(const ASession: IMFMediaSession);
    function GetParameters(out pdwFlags, pdwQueue: DWORD): HRESULT; stdcall;
    function Invoke(pAsyncResult: IMFAsyncResult): HRESULT; stdcall;

  public

    constructor Create(const AVideoWindow: HWND;
                       const ANotifyWindow: HWND;
                       const ASession: Cardinal);

    destructor Destroy(); override;

    procedure Open(const AFileName: string);
    procedure Start();
    procedure Pause();
    procedure Stop();

    procedure ResizeVideo(const AWidth: Integer;
                          const AHeight: Integer);

    procedure Repaint();
    procedure Close(const ADetachWindow: Boolean);
  end;


constructor TTopologyEventNotice.Create(const ASession: Cardinal;
                                        const AKind: TTopologyEventKind;
                                        const AText: string);
begin

  inherited Create;

  Session := ASession;
  Kind := AKind;
  Text := AText;
end;


function CreateTopologyPlayer(const AVideoWindow: HWND;
                              const ANotifyWindow: HWND;
                              const ASession: Cardinal): ITopologyPlayer;
begin

  Result := TTopologyPlayer.Create(AVideoWindow, ANotifyWindow, ASession);
end;


constructor TTopologyPlayer.Create(const AVideoWindow: HWND;
                                   const ANotifyWindow: HWND;
                                   const ASession: Cardinal);
begin

  inherited Create;

  InitializeCriticalSection(FLock);
  FVideoWindow := AVideoWindow;
  FNotifyWindow := ANotifyWindow;
  FSessionNumber := ASession;

  FClosedEvent := CreateEvent(nil,
                              True,
                              False,
                              nil);

  if (FClosedEvent = 0) then
    begin
      DeleteCriticalSection(FLock);
      RaiseLastOSError;
    end;
end;


destructor TTopologyPlayer.Destroy();
begin

  Close(True);

  if (FClosedEvent <> 0) then
    CloseHandle(FClosedEvent);

  DeleteCriticalSection(FLock);

  inherited;
end;


procedure TTopologyPlayer.Check(const AOperation: string;
                                const AHr: HRESULT);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: HRESULT 0x%.8x',
                              [AOperation, Cardinal(AHr)]);
end;


function TTopologyPlayer.CreateMediaSource(const AFileName: string;
                                           out ASource: IMFMediaSource): HRESULT;
var
  Resolver: IMFSourceResolver;
  SourceObject: IUnknown;
  ObjectType: MF_OBJECT_TYPE;

begin

  ASource := nil;
  Resolver := nil;
  SourceObject := nil;
  ObjectType := MF_OBJECT_INVALID;
  Result := MFCreateSourceResolver(Resolver);

  if SUCCEEDED(Result) then
    Result := Resolver.CreateObjectFromURL(PWideChar(AFileName),
                                            DWORD(MF_RESOLUTION_MEDIASOURCE),
                                            nil,
                                            ObjectType,
                                            SourceObject);

  if SUCCEEDED(Result) then
    Result := SourceObject.QueryInterface(IID_IMFMediaSource,
                                          ASource);
end;


function TTopologyPlayer.CreatePlaybackTopology(const ASource: IMFMediaSource;
                                                out ATopology: IMFTopology;
                                                out ATransform: IMFTransform): HRESULT;
var
  Presentation: IMFPresentationDescriptor;
  Stream: IMFStreamDescriptor;
  VideoStream: IMFStreamDescriptor;
  AudioStream: IMFStreamDescriptor;
  Handler: IMFMediaTypeHandler;
  SourceNode: IMFTopologyNode;
  TransformNode: IMFTopologyNode;
  OutputNode: IMFTopologyNode;
  Renderer: IMFActivate;
  AudioSourceNode: IMFTopologyNode;
  AudioOutputNode: IMFTopologyNode;
  AudioRenderer: IMFActivate;
  MajorType: TGUID;
  StreamCount: DWORD;
  I: DWORD;
  Selected: BOOL;
  VideoFound: Boolean;
  AudioFound: Boolean;

begin

  ATopology := nil;
  ATransform := nil;
  Presentation := nil;

  Result := ASource.CreatePresentationDescriptor(Presentation);

  if FAILED(Result) then
    Exit;

  Result := Presentation.GetStreamDescriptorCount(StreamCount);
  if FAILED(Result) then
    Exit;

  if (StreamCount = 0) then
    Exit(MF_E_INVALIDMEDIATYPE);

  VideoFound := False;
  AudioFound := False;
  Stream := nil;
  VideoStream := nil;
  AudioStream := nil;

  for I := 0 to StreamCount - 1 do
    begin
      Result := Presentation.GetStreamDescriptorByIndex(I,
                                                        Selected,
                                                        Stream);
      if FAILED(Result) then
        Exit;

      Result := Stream.GetMediaTypeHandler(Handler);

      if FAILED(Result) then
        Exit;

      Result := Handler.GetMajorType(MajorType);

      if FAILED(Result) then
        Exit;

      if not VideoFound and IsEqualGUID(MajorType,
                                        MFMediaType_Video) then
        begin
          Result := Presentation.SelectStream(I);

          if FAILED(Result) then
            Exit;

          VideoFound := True;
          VideoStream := Stream;
        end
      else if not AudioFound and IsEqualGUID(MajorType,
                                              MFMediaType_Audio) then
        begin
          Result := Presentation.SelectStream(I);

          if FAILED(Result) then
            Exit;

          AudioFound := True;
          AudioStream := Stream;
        end
      else
        begin
          Result := Presentation.DeselectStream(I);

          if FAILED(Result) then
            Exit;
        end;
    end;

  if not VideoFound then
    Exit(MF_E_INVALIDMEDIATYPE);

  Stream := VideoStream;

  Result := MFCreateTopology(ATopology);

  if FAILED(Result) then
    Exit;

  Result := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                                 SourceNode);

  if SUCCEEDED(Result) then
    Result := SourceNode.SetUnknown(MF_TOPONODE_SOURCE,
                                    ASource);

  if SUCCEEDED(Result) then
    Result := SourceNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                                    Presentation);
  if SUCCEEDED(Result) then
    Result := SourceNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                                    Stream);

  if SUCCEEDED(Result) then
    Result := ATopology.AddNode(SourceNode);

  if FAILED(Result) then
    Exit;

  ATransform := TMfGrayscaleMFT.Create as IMFTransform;

  Result := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                                 TransformNode);

  if SUCCEEDED(Result) then
    Result := TransformNode.SetObject(ATransform);

  if SUCCEEDED(Result) then
    Result := ATopology.AddNode(TransformNode);

  if FAILED(Result) then
    Exit;

  Result := MFCreateVideoRendererActivate(FVideoWindow,
                                          Renderer);

  if SUCCEEDED(Result) then
    Result := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                                   OutputNode);

  if SUCCEEDED(Result) then
    Result := OutputNode.SetObject(Renderer);

  if SUCCEEDED(Result) then
    Result := OutputNode.SetUINT32(MF_TOPONODE_STREAMID,
                                   0);

  if SUCCEEDED(Result) then
    Result := OutputNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                                   0);

  if SUCCEEDED(Result) then
    Result := ATopology.AddNode(OutputNode);

  if SUCCEEDED(Result) then
    Result := SourceNode.ConnectOutput(0,
                                       TransformNode,
                                       0);

  if SUCCEEDED(Result) then
    Result := TransformNode.ConnectOutput(0,
                                          OutputNode,
                                          0);

  if SUCCEEDED(Result) and AudioFound then
    Result := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                                   AudioSourceNode);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioSourceNode.SetUnknown(MF_TOPONODE_SOURCE,
                                         ASource);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioSourceNode.SetUnknown(
      MF_TOPONODE_PRESENTATION_DESCRIPTOR,
      Presentation);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioSourceNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                                         AudioStream);

  if SUCCEEDED(Result) and AudioFound then
    Result := ATopology.AddNode(AudioSourceNode);

  if SUCCEEDED(Result) and AudioFound then
    Result := MFCreateAudioRendererActivate(AudioRenderer);

  if SUCCEEDED(Result) and AudioFound then
    Result := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                                   AudioOutputNode);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioOutputNode.SetObject(AudioRenderer);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioOutputNode.SetUINT32(MF_TOPONODE_STREAMID,
                                        0);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioOutputNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                                        0);

  if SUCCEEDED(Result) and AudioFound then
    Result := ATopology.AddNode(AudioOutputNode);

  if SUCCEEDED(Result) and AudioFound then
    Result := AudioSourceNode.ConnectOutput(0,
                                            AudioOutputNode,
                                            0);
end;


procedure TTopologyPlayer.Open(const AFileName: string);
var
  Source: IMFMediaSource;
  Topology: IMFTopology;
  Transform: IMFTransform;
  Session: IMFMediaSession;

begin

  Close(False);
  Source := nil;
  Topology := nil;
  Transform := nil;
  Session := nil;

  try
    Check('Create media source',
          CreateMediaSource(AFileName,
                            Source));

    Check('Create playback topology',
          CreatePlaybackTopology(Source,
                                 Topology,
                                 Transform));

    Check('MFCreateMediaSession',
          MFCreateMediaSession(nil,
                               Session));

    EnterCriticalSection(FLock);

    try
      FSource := Source;
      FTransform := Transform;
      FSession := Session;
      ResetEvent(FClosedEvent);

    finally
      LeaveCriticalSection(FLock);
    end;

    Check('BeginGetEvent',
          Session.BeginGetEvent(Self as IMFAsyncCallback,
                                nil));

    Check('SetTopology',
          Session.SetTopology(0,
                              Topology));
  except
    Close(False);
    raise;
  end;
end;


procedure TTopologyPlayer.AcquireVideoDisplay(const ASession: IMFMediaSession);
var
  Hr: HRESULT;
  Display: IMFVideoDisplayControl;

begin

  Display := nil;

  Hr := MFGetService(ASession,
                     MR_VIDEO_RENDER_SERVICE,
                     IID_IMFVideoDisplayControl,
                     Pointer(Display));
  if FAILED(Hr) then
    begin
      PostNotice(tekError, Format('Get EVR display service failed: HRESULT 0x%.8x',
                                  [Cardinal(Hr)]));
      Exit;
    end;

  Display.SetAspectRatioMode(MFVideoARMode_PreservePicture);
  EnterCriticalSection(FLock);

  try
    FVideoDisplay := Display;
  finally
    LeaveCriticalSection(FLock);
  end;
end;


procedure TTopologyPlayer.PostNotice(const AKind: TTopologyEventKind;
                                     const AText: string);
var
  WindowHandle: HWND;
  Notice: TTopologyEventNotice;

begin

  EnterCriticalSection(FLock);

  try
    WindowHandle := FNotifyWindow;
  finally
    LeaveCriticalSection(FLock);
  end;

  if (WindowHandle = 0) then
    Exit;

  Notice := TTopologyEventNotice.Create(FSessionNumber,
                                        AKind,
                                        AText);

  if not PostMessage(WindowHandle,
                     WM_TOPOLOGY_PLAYER_EVENT,
                     0,
                     LPARAM(Notice)) then
    Notice.Free;
end;


function TTopologyPlayer.GetParameters(out pdwFlags,
                                       pdwQueue: DWORD): HRESULT;
begin

  pdwFlags := 0;
  pdwQueue := 0;
  Result := E_NOTIMPL;
end;


function TTopologyPlayer.Invoke(pAsyncResult: IMFAsyncResult): HRESULT;
var
  Session: IMFMediaSession;
  MediaEvent: IMFMediaEvent;
  EventType: MediaEventType;
  EventStatus: HRESULT;
  TopologyStatus: UINT32;

begin

  Result := S_OK;

  EnterCriticalSection(FLock);

  try
    Session := FSession;
  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Session) then
    Exit;

  Result := Session.EndGetEvent(pAsyncResult,
                                MediaEvent);
  if FAILED(Result) then
    Exit;

  Result := MediaEvent.GetType(EventType);
  if FAILED(Result) then
    Exit;

  EventStatus := S_OK;
  MediaEvent.GetStatus(EventStatus);

  if (EventType = MESessionClosed) then
    begin
      SetEvent(FClosedEvent);
      Exit(S_OK);
    end;

  Result := Session.BeginGetEvent(Self as IMFAsyncCallback,
                                  nil);
  if FAILED(Result) then
    Exit;
  if FAILED(EventStatus) then
    begin
      PostNotice(tekError, Format('Media Session event failed: HRESULT 0x%.8x',
                                 [Cardinal(EventStatus)]));
      Exit(S_OK);
    end;

  case EventType of
    MESessionTopologyStatus:
      begin
        TopologyStatus := 0;
        if SUCCEEDED((MediaEvent as IMFAttributes).GetUINT32(
           MF_EVENT_TOPOLOGY_STATUS, TopologyStatus)) and
           (TopologyStatus = UINT32(MF_TOPOSTATUS_READY)) then
          begin
            AcquireVideoDisplay(Session);
            PostNotice(tekTopologyReady,
              'Topology ready: grayscale video plus audio pass-through.');
          end;
      end;

    MESessionStarted:
      PostNotice(tekStarted, 'Media Session started.');

    MESessionPaused:
      PostNotice(tekPaused, 'Media Session paused.');

    MESessionStopped:
      PostNotice(tekStopped, 'Media Session stopped.');

    MEEndOfPresentation:
      PostNotice(tekEnded, 'End of presentation.');
  end;

  Result := S_OK;
end;


procedure TTopologyPlayer.Start();
var
  Session: IMFMediaSession;
  Position: PROPVARIANT;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Session) then
    Exit;

  PropVariantInit(Position);

  try
    Check('Start Media Session',
          Session.Start(GUID_NULL,
                        Position));
  finally
    PropVariantClear(Position);
  end;
end;


procedure TTopologyPlayer.Pause();
var
  Session: IMFMediaSession;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Session) then
    Check('Pause Media Session',
          Session.Pause());
end;


procedure TTopologyPlayer.Stop();
var
  Session: IMFMediaSession;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Session) then
    Check('Stop Media Session', Session.Stop);
end;


procedure TTopologyPlayer.ResizeVideo(const AWidth: Integer;
                                      const AHeight: Integer);
var
  Display: IMFVideoDisplayControl;
  Destination: TRect;

begin

  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  EnterCriticalSection(FLock);

  try
    Display := FVideoDisplay;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Display) then
    begin
      SetRect(Destination,
              0,
              0,
              AWidth,
              AHeight);

      Display.SetVideoPosition(nil,
                               @Destination);
    end;
end;


procedure TTopologyPlayer.Repaint();
var
  Display: IMFVideoDisplayControl;

begin

  EnterCriticalSection(FLock);

  try
    Display := FVideoDisplay;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Display) then
    Display.RepaintVideo;
end;


procedure TTopologyPlayer.Close(const ADetachWindow: Boolean);
var
  Session: IMFMediaSession;
  Source: IMFMediaSource;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
    Source := FSource;
    FVideoDisplay := nil;

    if ADetachWindow then
      FNotifyWindow := 0;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Assigned(Session) then
    begin
      ResetEvent(FClosedEvent);

      if SUCCEEDED(Session.Close) then
        WaitForSingleObject(FClosedEvent,
                            5000);

      Session.Shutdown;
    end;

  if Assigned(Source) then
    Source.Shutdown();

  EnterCriticalSection(FLock);

  try
    FSession := nil;
    FSource := nil;
    FTransform := nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
