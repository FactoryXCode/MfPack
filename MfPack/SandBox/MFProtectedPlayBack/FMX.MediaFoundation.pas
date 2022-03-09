unit FMX.MediaFoundation;

// https://github.com/FactoryXCode/MfPack
// https://github.com/GoshaDE/SuperMFLib/blob/master/samples/ProtectedPlayback/Player.cs
interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  System.SysUtils,
  System.Win.ComObj,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMediaEngine,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  {FMX}
  FMX.Types,
  FMX.Forms,
  FMX.Graphics,
  FMX.Controls;

{$TYPEINFO ON}

const
  // MediaTimeScale = 10000000;
  NS_E_DRM_LICENSE_NOTACQUIRED = HResult($C00D2759);

type
  TMediaTime = Int64;
  TMediaState = (Unavailable, Playing, Stopped);
  TCommandAction = (caNone, caResize, caSetCurrentTime);

  // TContentprotectionManager

  TEnablerFlags = (SilentOrNonSilent = 0,
    // Use silent if supported, otherwise use non-silent.
    ForceNonSilent = 1 // Use non-silent.
    );

  TEnabler = (Ready,
              SilentInProgress,
              NonSilentInProgress,
              Complete);

  TContentProtectionManager = class(TInterfacedObject,
                                    IMFAsyncCallback,
                                    IMFContentProtectionManager)
  private
    FState: TEnabler;
    FStatus: HResult; // Status code from the most recent event.
    FEnabler: IMFContentEnabler; // Content enabler.
    FpMEG: IMFMediaEventGenerator;
    // The content enabler's event generator interface.
    FAsyncCallback: IMFAsyncCallback;
    FUnkState: IUnknown;

    FOnContentNotify: TNotifyEvent;

    procedure DoNonSilentEnable();
    procedure LogEnableType(guidEnableType: TGuid);

  protected
    function GetParameters(out pdwFlags: DWORD;
                           out pdwQueue: DWORD): HResult; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

    function BeginEnableContent(pEnablerActivate: IMFActivate;
                                pTopo: IMFTopology;
                                pCallback: IMFAsyncCallback;
                                punkState: IUnknown): HResult; stdcall;

    function EndEnableContent(pResult: IMFAsyncResult): HResult; stdcall;

  public
    constructor Create;
    // Handles stuff like checking reference counting before reaching the destructor.
    procedure BeforeDestruction(); override;
    destructor Destroy; override;

    function GetState(): TEnabler;
    function GetStatus(): HResult;

    procedure DoEnable(AFlags: TEnablerFlags);
    procedure CancelEnable();
    procedure CompleteEnable();

    property OnContentNotify: TNotifyEvent read FOnContentNotify write FOnContentNotify;

  end;

  TMediaEngine = class(TControl, IMFMediaEngineNotify)
  private
    // Enables an application to play audio or video files.
    FMediaEngine: IMFMEdiaEngineEx;
    FAction: TCommandAction;
    FRect: TRect;
    FSetCurrentTime: double;

    FVideoWidth: Cardinal;
    FVideoHeight: Cardinal;

    FFilename: String; // The URL of the source that is playing
    FDuration: double; // Duration of the mediasource
    FCurrentTime: double; // Time of the mediasource

    FPlayAfterLoad: Boolean;
    // When true, de mediasource wil be played immediately
    FMediaState: TMediaState;
    FLock: TCriticalSection;

    function GetCurrentPosition: double;
    procedure SetCurrentPosition(const AValue: double);

    procedure SetVolume(dVol: double);
    function GetVolume: double;
    procedure SetFilename(const AValue: String);

    function GetDuration: double;
  protected
    FWnd: HWND; // Handle of the videosurface
    FOverlay: TForm;
    FContentProtectionManager: TContentProtectionManager;

    procedure DoContentNotify(Sender: TObject);

    procedure DoCloseQuery(Sender: TObject;
                           var CanClose: Boolean);

    function EventNotify(event: DWORD;
                         param1: DWORD_PTR;
                         param2: DWORD): HResult; stdcall;

    procedure OnLoadStart(event: DWORD);
    procedure OnProgress(event: DWORD);
    procedure OnSuspend(event: DWORD);
    procedure OnAbort(event: DWORD);
    procedure OnError(event: DWORD;
                      param1: DWORD_PTR;
                      param2: DWORD);
    procedure OnEmptied(event: DWORD);
    procedure OnStalled(event: DWORD);
    procedure OnPlay(event: DWORD);
    procedure OnPause(event: DWORD);
    procedure OnLoadedMetaData(event: DWORD);
    procedure OnLoadedData(event: DWORD);
    procedure OnWaiting(event: DWORD);
    procedure OnPlaying(event: DWORD);
    procedure OnCanPlay(event: DWORD);
    procedure OnCanPlayThrough(event: DWORD);
    procedure OnSeeking(event: DWORD);
    procedure OnSeeked(event: DWORD);
    procedure OnTimeUpdate(event: DWORD);
    procedure OnEnded(event: DWORD);
    procedure OnRateChange(event: DWORD);
    procedure OnDurationChange(event: DWORD);
    procedure OnVolumeChanged(event: DWORD);
    procedure OnFormatChanged(event: DWORD;
                              param1: DWORD_PTR;
                              param2: DWORD);
    // EXTENSIONS on the HTML5 specs
    procedure OnPurgeQueuedEvents(event: DWORD);
    procedure OnTimeLineMarker(event: DWORD);
    procedure OnBalanceChanged(event: DWORD);
    procedure OnDownloadComplete(event: DWORD);
    procedure OnBufferingStarted(event: DWORD);
    procedure OnBufferingEnded(event: DWORD);
    procedure OnFrameStepCompleted(event: DWORD);
    procedure OnNotifyStableState(event: DWORD;
                                  param1: DWORD_PTR;
                                  param2: DWORD);
    procedure OnFirstFrameReady(event: DWORD);
    procedure OnTracksChange(event: DWORD);
    procedure OnOpmInfo(event: DWORD);
    procedure OnResourceLost(event: DWORD);
    procedure OnDelayLoadEventChanged(event: DWORD);
    procedure OnStreamRenderingError(event: DWORD);
    procedure OnSupportedRatesChanged(event: DWORD);
    procedure OnAudioEndPointChanged(event: DWORD);

    procedure InitWnd;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function Play(): HResult;
    procedure Pause();
    procedure Stop();
    procedure FlushPlayer();
    procedure Clear;
    function IsProtected: Boolean;

    procedure SetBalance(dBal: double);
    function Mute(var bMute: BOOL): HResult;
    function FrameStep(goForward: BOOL): HResult;

    // Queries the Media Engine to find out whether a new video frame is ready.
    function GetVideoStreamTick(): LONGLONG;

    property Filename: string read FFilename write SetFilename;
    property Duration: Double read GetDuration;
    property PlayAfterLoad: Boolean read FPlayAfterLoad write FPlayAfterLoad;
    property CurrentTime: Double read GetCurrentPosition write SetCurrentPosition;

    property Volume: Double read GetVolume write SetVolume;
    property State: TMediaState read FMediaState;
    property VideoWidth: Cardinal read FVideoWidth;
    property VideoHeight: Cardinal read FVideoHeight;
  end;


function InitMF(): HResult;
function CloseMF(): HResult;


implementation

uses
  FMX.Dialogs,
  FMX.Platform.Win,
  FMX.Helpers.Win,
  System.Types,
  System.Threading;


procedure TMediaEngine.DoResized;
var
  P: TPointF;
  R: TRect;
  Bounds: TRectF;
  Form: TCommonCustomForm;
  BoundsInt: TRect;
  i: integer;
  sScale: Single;

begin
  inherited;

  if FWnd = 0 then
    Exit;

  if not(csDesigning in ComponentState) and
        (ParentedVisible) and
        (Root <> nil) and
        (Root.GetObject is TCommonCustomForm) then
    begin
      Form := TCommonCustomForm(Root.GetObject);

      sScale := Form.Handle.Scale;

      WinApi.Windows.SetParent(FWnd,
                               WindowHandleToPlatform(Form.Handle).Wnd);
      P := PointF(sScale * FVideoWidth,
                  sScale * FVideoHeight);

      Bounds := TRectF.Create(0,
                              0,
                              P.X,
                              P.Y);

      Bounds.Fit(TRectF.Create(0,
                               0,
                               AbsoluteWidth,
                               AbsoluteHeight));

      Bounds.Offset(AbsoluteRect.TopLeft);
      BoundsInt := TRectF.Create((Bounds.Left * sScale),
                                 (Bounds.Top * sScale),
                                 (Bounds.Right * sScale),
                                 (Bounds.Bottom * sScale)).Round;

      R := TRect.Create(0,
                        0,
                        BoundsInt.Width,
                        BoundsInt.Height);

      FLock.Enter;

      try
        if (R.Width <> FRect.Width) or (R.Height <> FRect.Height) then
          begin
            FRect := R;
            FAction := TCommandAction.caResize;
            WaitForSingleObject(FWnd,
                                INFINITE);
          end;
      finally
        FLock.Leave;
      end;

      SetWindowPos(FWnd,
                   0,
                   BoundsInt.Left,
                   BoundsInt.Top,
                   BoundsInt.Width,
                   BoundsInt.Height,
                   0);

      ShowWindow(FWnd,
                 SW_SHOW);


      if assigned(FOverlay) then
        begin
          for i := ChildrenCount - 1 downto 0 do
            Children[i].Parent := FOverlay;

          FOverlay.StyleBook := Form.StyleBook;
          FOverlay.SetBounds(BoundsRect.Round);
          FOverlay.Visible := true;
          FOverlay.OnMouseMove := OnMouseMove;
          WinApi.Windows.SetParent(WindowHandleToPlatform(FOverlay.Handle).Wnd,
          WindowHandleToPlatform(Form.Handle).Wnd);
        end;

    end
  else
    begin
      for i := FOverlay.ChildrenCount - 1 downto 0 do
        FOverlay.Children[i].Parent := self;

      WinApi.Windows.SetParent(FWnd, ApplicationHWND);
      ShowWindow(FWnd, SW_HIDE);
    end;
end;


procedure TMediaEngine.InitWnd;
var
  WindowClass: TWndClass;

begin
  if not GetClassInfo(SysInit.HInstance, PChar('VMRWindow'), WindowClass) then
    begin
      FillChar(WindowClass, SizeOf(WindowClass), 0);
      WindowClass.Style := CS_HREDRAW or CS_VREDRAW;
      WindowClass.lpfnWndProc := @DefWindowProc;
      WindowClass.cbClsExtra := 0;
      WindowClass.cbWndExtra := 0;
      WindowClass.HInstance := SysInit.HInstance;
      WindowClass.hCursor := LoadCursorW(0, PChar(IDC_ARROW));
      WindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
      WindowClass.lpszMenuName := nil;
      WindowClass.lpszClassName := PChar('VMRWindow');
      if WinApi.Windows.RegisterClass(WindowClass) = 0 then
        RaiseLastOSError;
    end;

  FWnd := CreateWindowEx(0,
                         WindowClass.lpszClassName,
                         Nil,
                         WS_CHILDWINDOW,
                         0,
                         0,
                         0,
                         0,
                         GetDesktopWindow,
                         0,
                         SysInit.HInstance, Nil);
  ShowWindow(FWnd,
             SW_HIDE);

  FOverlay := TForm.CreateNew(self);
  FOverlay.BorderStyle := TFMXFormBorderStyle.None;
  FOverlay.Name := 'VideoOverlayWindow';
  FOverlay.Parent := self;
  FOverlay.Transparency := true;
  FOverlay.OnCloseQuery := DoCloseQuery;
end;


procedure TMediaEngine.DoCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false; // nö
end;

constructor TMediaEngine.Create(AOwner: TComponent);
var
  hr: HResult;
  li_Attributes: IMFAttributes; // Attributes store
  li_MediaEngineClassFactory: IMFMediaEngineClassFactory;

begin
  inherited;

  InitWnd;

  FLock := TCriticalSection.Create;

  hr := InitMF();

  if FAILED(hr) then
  begin
    MessageBox(0,
      LPCWSTR('An error occured while trying to initialize Media Foundation.'),
      LPCWSTR('Fatal Error!'), MB_ICONEXCLAMATION);
    Exit();
  end;

  // IMFMediaEngineClassFactory
  // Creates an instance of the Media Engine.
  // Note: Before using this interface, CoInitializeEx and MFStartup should be initialized.
  // To get a pointer to this interface, call CoCreateInstance.
  // The class identifier is CLSID_MFMediaEngineClassFactory.
  //
  // hr := CoCreateInstance(CLSID_MFMediaEngineClassFactory,
  // Nil,
  // CLSCTX_INPROC_SERVER,
  // IID_IMFMediaEngineClassFactory,
  // li_MediaEngineClassFactory);
  // Or:
  //
  // In Delphi you could use CreateCOMObject function to do the same.
  // FMediaEngineClassFactory is a reference to the IMFMediaEngineClassFactory interface
  li_MediaEngineClassFactory := CreateCOMObject(CLSID_MFMediaEngineClassFactory)
    as IMFMediaEngineClassFactory;

  // Call MFCreateAttributes to create the attribute store.
  hr := MFCreateAttributes(li_Attributes,
                           3);

  hr := li_Attributes.SetUnknown(MF_MEDIA_ENGINE_CALLBACK,
                                 Self);
  //hr := li_Attributes.SetUnknown(MF_SESSION_CONTENT_PROTECTION_MANAGER,
  //                                 FContentProtectionManager);
  hr := li_Attributes.SetUINT64(MF_MEDIA_ENGINE_PLAYBACK_HWND,
                                FWnd);
  hr := li_Attributes.SetUINT32(MF_MEDIA_ENGINE_CONTENT_PROTECTION_FLAGS,
                                MF_MEDIA_ENGINE_ENABLE_PROTECTED_CONTENT);

  // Create the mediaEngine
  hr := li_MediaEngineClassFactory.CreateInstance(0,
                                                  li_Attributes,
                                                  IUnknown(FMediaEngine));
  if SUCCEEDED(hr) then
    begin
      FContentProtectionManager := TContentProtectionManager.Create;
      FContentProtectionManager.OnContentNotify := DoContentNotify;

      hr := (FMediaEngine as IMFMediaEngineProtectedContent).SetContentProtectionManager(FContentProtectionManager);
      if FAILED(hr) then
        ShowMessage('Function SetContentProtectionManager failed');
    end
  else
    ShowMessage('Function CreateInstance failed');
end;


// DESTRUCTOR
destructor TMediaEngine.Destroy();
begin
  // Release the global interfaces
  SafeRelease(FMediaEngine);
  // Release CriticalSection
  FLock.Destroy();

  // Close the media foundation platform and end COM
  { void } CloseMF();

  FContentProtectionManager.Destroy();
  if FWnd <> 0 then
    CloseWindow(FWnd);

  inherited Destroy();
end;


//
// EVENT HANDLERS  /////////////////////////////////////////////////////////////
//

// Queries the Media Engine to find out whether a new video frame is ready.

//
procedure TMediaEngine.OnLoadStart(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnProgress(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnSuspend(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnAbort(event: DWORD);
begin
  FMediaState := TMediaState.Unavailable;
end;

procedure TMediaEngine.OnError(event: DWORD; param1: DWORD_PTR; param2: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnEmptied(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnStalled(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnPlay(event: DWORD);
begin
  FMediaState := TMediaState.Playing;
end;

procedure TMediaEngine.OnPause(event: DWORD);
begin
  FMediaState := TMediaState.Stopped;
end;

procedure TMediaEngine.OnLoadedMetaData(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnLoadedData(event: DWORD);
begin
end;

procedure TMediaEngine.OnWaiting(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnPlaying(event: DWORD);
begin
  FMediaState := TMediaState.Playing;
end;

procedure TMediaEngine.OnCanPlay(event: DWORD);
begin
  // Immediately start playing when the source is loaded.
  if FPlayAfterLoad then
    { void } FMediaEngine.Play();

  FMediaEngine.GetNativeVideoSize(FVideoWidth, FVideoHeight);
  TThread.Synchronize(Nil,
                      DoResized);
end;

procedure TMediaEngine.OnCanPlayThrough(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnSeeking(event: DWORD);
begin
  // Implement code here
end;

procedure TMediaEngine.OnSeeked(event: DWORD);
begin
  // Implement code here
end;

{$HINTS OFF}

procedure TMediaEngine.OnTimeUpdate(event: DWORD);
begin
  // Implement code here
  FLock.Enter;
  try
    if not assigned(FMediaEngine) then
      Exit;

    case FAction of
      caNone:
        begin
          {Do something here}
        end;
      caSetCurrentTime:
        begin
          FMediaEngine.SetCurrentTime(FSetCurrentTime);
        end;
      caResize:
        begin
          FMediaEngine.UpdateVideoStream(nil, @FRect, nil);
        end;
    end;
    FAction := caNone;
    FCurrentTime := FMediaEngine.GetCurrentTime;
  finally
    FLock.Leave;
  end;
end;


function TMediaEngine.GetCurrentPosition: double;
begin
  FLock.Enter;
  try
    Result := FCurrentTime;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaEngine.SetCurrentPosition(const AValue: double);
begin
  FLock.Enter;
  try
    FSetCurrentTime := AValue;
    FAction := caSetCurrentTime;
    WaitForSingleObject(FWnd,
                        INFINITE);
  finally
    FLock.Leave;
  end;
end;

function TMediaEngine.GetDuration: double;
begin
  FLock.Enter;
  Result := FDuration;
  FLock.Leave;
end;

{$HINTS ON}

procedure TMediaEngine.OnEnded(event: DWORD);
begin
  FMediaState := TMediaState.Stopped;
end;


procedure TMediaEngine.OnRateChange(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnDurationChange(event: DWORD);
begin
  FLock.Enter;
  FDuration := FMediaEngine.GetDuration();
  FLock.Leave;
end;


procedure TMediaEngine.OnVolumeChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnFormatChanged(event: DWORD; param1: DWORD_PTR;
  param2: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnPurgeQueuedEvents(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnTimeLineMarker(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnBalanceChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnDownloadComplete(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnBufferingStarted(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnBufferingEnded(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnFrameStepCompleted(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnNotifyStableState(event: DWORD; param1: DWORD_PTR;
  param2: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnFirstFrameReady(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnTracksChange(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnOpmInfo(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnResourceLost(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnDelayLoadEventChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnStreamRenderingError(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnSupportedRatesChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TMediaEngine.OnAudioEndPointChanged(event: DWORD);
begin
  // Implement code here
end;

//
// Notifier for the events
// Parameter  Description
// ~~~~~~~~~  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// event      A member of the MF_MEDIA_ENGINE_EVENT enumeration that specifies the event.
// param1     The first event parameter. The meaning of this parameter depends on the event code.
// param2     The second event parameter. The meaning of this parameter depends on the event code.
//
// Remarks:
// The application receives Media Engine events through the IMFMediaEngineNotify.EventNotify method.
// The EventNotify method includes two event parameters, param1 and param2.
// The meaning of the parameters depends on the event code.
// If the event description does not list any parameters, the values of param1 and param2 should be ignored.
//
// Values below 1000 correspond to events defined in HTML 5 for media elements.
//
function TMediaEngine.EventNotify(event: DWORD;
                                  param1: DWORD_PTR;
                                  param2: DWORD): HResult;
var
  eEvent: MfMediaEngineEvent;

begin
  Result := S_OK;
  eEvent := MfMediaEngineEvent(event);

  case eEvent of
    // The Media Engine has started to load the source. See: IMFMediaEngine.Load.
    MF_MEDIA_ENGINE_EVENT_LOADSTART:
      OnLoadStart(event);

    // The Media Engine is loading the source.
    MF_MEDIA_ENGINE_EVENT_PROGRESS:
      OnProgress(event);

    // The Media Engine has suspended a load operation.
    MF_MEDIA_ENGINE_EVENT_SUSPEND:
      OnSuspend(event);

    // The Media Engine cancelled a load operation that was in progress.
    MF_MEDIA_ENGINE_EVENT_ABORT:
      OnAbort(event);

    // An error occurred.
    // Event Parameter	   Description
    // ~~~~~~~~~~~~~~~     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // param1	             A member of the MF_MEDIA_ENGINE_ERR enumeration.
    // param2	             A HRESULT error code, or zero.
    //
    MF_MEDIA_ENGINE_EVENT_ERROR:
      OnError(event, param1, param2);

    // The Media Engine has switched to the MF_MEDIA_ENGINE_NETWORK_EMPTY state.
    // This can occur when the IMFMediaEngine.Load method is called,
    // or if an error occurs during the Load method. See: IMFMediaEngine.GetNetworkState.
    MF_MEDIA_ENGINE_EVENT_EMPTIED:
      OnEmptied(event);

    // The Load algorithm is stalled, waiting for data.
    MF_MEDIA_ENGINE_EVENT_STALLED:
      OnStalled(event);

    // The Media Engine is switching to the playing state. See: IMFMediaEngine.Play.
    MF_MEDIA_ENGINE_EVENT_PLAY:
      OnPlay(event);

    // The media engine has paused. See: IMFMediaEngine.Pause.
    MF_MEDIA_ENGINE_EVENT_PAUSE:
      OnPause(event);

    // The Media Engine has loaded enough source data to determine the duration and
    // dimensions of the source.
    // This is important, because you can't acces this data before this event.
    MF_MEDIA_ENGINE_EVENT_LOADEDMETADATA:
      OnLoadedMetaData(event);

    // The Media Engine has loaded enough data to render some content (for example, a video frame).
    MF_MEDIA_ENGINE_EVENT_LOADEDDATA:
      OnLoadedData(event);

    // Playback has stopped because the next frame is not available.
    MF_MEDIA_ENGINE_EVENT_WAITING:
      OnWaiting(event);

    // Playback has started. See: IMFMediaEngine.Play.
    MF_MEDIA_ENGINE_EVENT_PLAYING:
      OnPlaying(event);

    // Playback can start, but the Media Engine might need to stop to buffer more data.
    MF_MEDIA_ENGINE_EVENT_CANPLAY:
      OnCanPlay(event);

    // The Media Engine can probably play through to the end of the resource,
    // without stopping to buffer data.
    MF_MEDIA_ENGINE_EVENT_CANPLAYTHROUGH:
      OnCanPlayThrough(event);

    // The Media Engine has started seeking to a new playback position.
    // See: IMFMediaEngine.SetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_SEEKING:
      OnSeeking(event);

    // The Media Engine has seeked to a new playback position. See: IMFMediaEngine.SetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_SEEKED:
      OnSeeked(event);

    // The playback position has changed. See: IMFMediaEngine.GetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_TIMEUPDATE:
      OnTimeUpdate(event);

    // Playback has reached the end of the source. This event is not sent if the GetLoop is TRUE.
    MF_MEDIA_ENGINE_EVENT_ENDED:
      OnEnded(event);

    // The playback rate has changed. See: IMFMediaEngine.SetPlaybackRate.
    MF_MEDIA_ENGINE_EVENT_RATECHANGE:
      OnRateChange(event);

    // The duration of the media source has changed. See: IMFMediaEngine.GetDuration.
    MF_MEDIA_ENGINE_EVENT_DURATIONCHANGE:
      OnDurationChange(event);

    // The audio volume changed. See: IMFMediaEngine.SetVolume.
    MF_MEDIA_ENGINE_EVENT_VOLUMECHANGE:
      OnVolumeChanged(event);

    // The output format of the media source has changed.
    // Event Parameter	   Description
    // ~~~~~~~~~~~~~~~     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // param1	             Zero if the video format changed, 1 if the audio format changed.
    // param2	             Zero.
    //
    MF_MEDIA_ENGINE_EVENT_FORMATCHANGE:
      OnFormatChanged(event, param1, param2);

    //
    // EXTENSIONS
    //

    // The Media Engine flushed any pending events from its queue.
    MF_MEDIA_ENGINE_EVENT_PURGEQUEUEDEVENTS:
      OnPurgeQueuedEvents(event);

    // The playback position reached a timeline marker.
    // See: IMFMediaEngineEx.SetTimelineMarkerTimer.
    MF_MEDIA_ENGINE_EVENT_TIMELINE_MARKER:
      OnTimeLineMarker(event);

    // The audio balance changed. See: IMFMediaEngineEx.SetBalance.
    MF_MEDIA_ENGINE_EVENT_BALANCECHANGE:
      OnBalanceChanged(event);

    // The Media Engine has finished downloading the source data.
    MF_MEDIA_ENGINE_EVENT_DOWNLOADCOMPLETE:
      OnDownloadComplete(event);

    // The media source has started to buffer data.
    MF_MEDIA_ENGINE_EVENT_BUFFERINGSTARTED:
      OnBufferingStarted(event);

    // The media source has stopped buffering data.
    MF_MEDIA_ENGINE_EVENT_BUFFERINGENDED:
      OnBufferingEnded(event);

    // The IMFMediaEngineEx.FrameStep method completed.
    MF_MEDIA_ENGINE_EVENT_FRAMESTEPCOMPLETED:
      OnFrameStepCompleted(event);

    // The Media Engine's Load algorithm is waiting to start.
    // Event Parameter     Description
    // ~~~~~~~~~~~~~~~     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // param1              A handle to a waitable event, of type HANDLE.
    // param2              Zero.
    //
    // If Media Engine is created with the MF_MEDIA_ENGINE_WAITFORSTABLE_STATE flag,
    // the Media Engine sends the MF_MEDIA_ENGINE_EVENT_NOTIFYSTABLESTATE event at
    // the start of the Load algorithm.
    // The param1 parameter is a handle to a waitable event.
    // The Load thread waits for the application to signal the event by calling SetEvent.
    //
    // If the Media Engine is not created with the MF_MEDIA_ENGINE_WAITFORSTABLE_STATE,
    // it does not send this event, and the Load thread does not wait to be signalled.
    MF_MEDIA_ENGINE_EVENT_NOTIFYSTABLESTATE:
      OnNotifyStableState(event, param1, param2);

    // The first frame of the media source is ready to render.
    MF_MEDIA_ENGINE_EVENT_FIRSTFRAMEREADY:
      OnFirstFrameReady(event);

    // Raised when a new track is added or removed.
    // Supported in Windows 8.1 and later.
    MF_MEDIA_ENGINE_EVENT_TRACKSCHANGE:
      OnTracksChange(event);

    // Raised when there is new information about the Output Protection Manager (OPM).
    // This event will be raised when an OPM failure occurs,
    // but ITA allows fallback without the OPM. In this case, constriction can be applied.
    // This event will not be raised when there is an OPM failure and the fallback also fails.
    // For example, if ITA blocks playback entirely when OPM cannot be established.
    // Supported in Windows 8.1 and later.
    MF_MEDIA_ENGINE_EVENT_OPMINFO:
      OnOpmInfo(event);

    // The source URL is deleted or unreachable
    MF_MEDIA_ENGINE_EVENT_RESOURCELOST:
      OnResourceLost(event);

    //
    MF_MEDIA_ENGINE_EVENT_DELAYLOADEVENT_CHANGED:
      OnDelayLoadEventChanged(event);

    // Raised when one of the component streams of a media stream fails.
    // This event is only raised if the media stream contains other component streams that did not fail.
    MF_MEDIA_ENGINE_EVENT_STREAMRENDERINGERROR:
      OnStreamRenderingError(event);

    // One oer more of the supported rates changed.
    MF_MEDIA_ENGINE_EVENT_SUPPORTEDRATES_CHANGED:
      OnSupportedRatesChanged(event);

    // The Audio Endpoint changed. This event occures when user changed,
    // for instance, a headphone or speaker jack
    MF_MEDIA_ENGINE_EVENT_AUDIOENDPOINTCHANGE:
      OnAudioEndPointChanged(event);

  else
    begin
      // Unknown Event
    end;
  end;
end;

// Event handler
function TMediaEngine.GetVideoStreamTick(): LONGLONG;
var
  llstrtick: LONGLONG;

begin
  if SUCCEEDED(FMediaEngine.OnVideoStreamTick(llstrtick)) then
    Result := llstrtick
  else
    Result := -1;
end;

// IMPLEMENTED METHODS /////////////////////////////////////////////////////////

procedure TMediaEngine.DoContentNotify(Sender: TObject);
begin
  // ContentProtection
  case FContentProtectionManager.GetState of
    Ready: // Start the enable action.
      begin
        FContentProtectionManager.DoEnable(SilentOrNonSilent);
      end;

    SilentInProgress:
      begin
        // We are currently in the middle of silent enable.

        // If the status code is NS_E_DRM_LICENSE_NOTACQUIRED,
        // we need to try non-silent enable.
        if FContentProtectionManager.GetStatus = NS_E_DRM_LICENSE_NOTACQUIRED then
          begin
            // Silent enabler failed, attempting non-silent.
            FContentProtectionManager.DoEnable(ForceNonSilent);
            // Try non-silent this time;
          end
        else
          begin
            // Complete the operation. If it succeeded, the content will play.
            // If it failed, the pipeline will queue an event with an error code.
            FContentProtectionManager.CompleteEnable();
          end;
      end;

    NonSilentInProgress:
      begin
        // We are currently in the middle of non-silent enable.
        // Either we succeeded or an error occurred. Either way, complete
        // the operation.
        FContentProtectionManager.CompleteEnable();
      end;
    Complete: // Nothing to do.
  end;
end;


procedure TMediaEngine.SetFilename(const AValue: string);
begin
  if Not assigned(FMediaEngine) then
    Exit;

  if (AValue = '') then
    Exit;

  // Sets the real time mode used for the next call to SetSource or Load.
  if FAILED(FMediaEngine.SetRealTimeMode(true)) then
    Exit;

  if FAILED(FMediaEngine.SetSource(PChar(AValue))) then
    Exit;

  FFilename := AValue;
  FMediaState := TMediaState.Unavailable;
  FVideoWidth := 0;
  FVideoHeight := 0;
end;


procedure TMediaEngine.Clear;
begin
  FlushPlayer;
  FFilename := '';
end;


function TMediaEngine.Play(): HResult;
begin
  if FFilename <> '' then
    begin
      Result := FMediaEngine.EnableTimeUpdateTimer(true);
      if FAILED(result) then
        Exit;

      Result := FMediaEngine.Play();
      if FAILED(result) then
        Exit;
    end
    else
      Result := E_INVALIDARG;
end;


procedure TMediaEngine.Pause();
begin
  // Send request required, as this is an async method
  // The request will be handled during the ontime eventhandler
  if assigned(FMediaEngine) then
    FMediaEngine.Pause;
  FrameStep(true);
end;


// This needs some explanation:
// There is a slight difference between stop or pause,
// At stop the mediafile will be reset to it's begin and starts all over again when playbutton is pressed.
procedure TMediaEngine.Stop();
begin
  if assigned(FMediaEngine) and SUCCEEDED(FMediaEngine.Pause()) then
    FMediaEngine.SetCurrentTime(0.0);
end;


function TMediaEngine.IsProtected: Boolean;
var
  bProt: LongBool;

begin
  Result := assigned(FMediaEngine) and
    SUCCEEDED(FMediaEngine.IsProtected(bProt)) and bProt;
end;


// This will free al resources kept by the MediaEngine.
// The caller has to release the player object and re-initialize,
// before playing a new mediasource.
procedure TMediaEngine.FlushPlayer();
begin
  if assigned(FMediaEngine) then
    FMediaEngine.Shutdown()
end;


function TMediaEngine.GetVolume: double;
begin
  if assigned(FMediaEngine) then
    Result := FMediaEngine.GetVolume
  else
    Result := 0;
end;


procedure TMediaEngine.SetVolume(dVol: double);
  begin
    if assigned(FMediaEngine) then
      FMediaEngine.SetVolume(dVol);
  end;


procedure TMediaEngine.SetBalance(dBal: double);
begin
  if assigned(FMediaEngine) then
    FMediaEngine.SetBalance(dBal);
end;

function TMediaEngine.Mute(var bMute: BOOL): HResult;
begin
  Result := FMediaEngine.SetMuted(bMute);
  // Return the mute state
  bMute := FMediaEngine.GetMuted();
end;


// Use framestep after pause, to get an accurate playback position
// See: https://docs.microsoft.com/en-us/windows/win32/api/mfmediaengine/nf-mfmediaengine-imfmediaengine-getcurrenttime
function TMediaEngine.FrameStep(goForward: BOOL): HResult;
begin
  Result := FMediaEngine.FrameStep(goForward);
end;

function InitMF(): HResult;
var
  hr: HResult;

begin
  // Initialize the COM library.
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED or
                       COINIT_DISABLE_OLE1DDE);
  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('COM library initialisation failure.'),
                 LPCWSTR('COM Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;

  // Intialize the Media Foundation platform and
  // check if the current MF version match user's version
  hr := MFStartup(MF_VERSION);

  if FAILED(hr) then
    begin
      MessageBox(0,
        LPCWSTR('Your computer does not support this Media Foundation API version' +
                IntToStr(MF_VERSION) + '.'),
                LPCWSTR('MFStartup Failure!'),
                MB_ICONSTOP);
      Abort();
    end;
  Result := hr;
end;


function CloseMF(): HResult;
begin
  // Shutdown MF
  Result := MFShutdown();
  // Shutdown COM
  CoUninitialize();
end;

  { TContentProtectionManager }
constructor TContentProtectionManager.Create();
begin
  inherited Create;
  FState := TEnabler.Ready;
end;

// Handles stuff like checking reference counting before reaching the destructor.
procedure TContentProtectionManager.BeforeDestruction();
begin
  if Assigned(FEnabler) then
    Safe_Release(FEnabler);
  if Assigned(FAsyncCallback) then
    Safe_Release(FpMEG);
  if Assigned(FAsyncCallback) then
    Safe_Release(FAsyncCallback);
  if Assigned(FUnkState) then
    Safe_Release(FUnkState);
end;


destructor TContentProtectionManager.Destroy();
begin
  inherited Destroy();
end;


///////////////////////////////////////////////////////////////////////
// Name: BeginEnableContent
// Description:  Called by the PMP session to start the enable action.
/////////////////////////////////////////////////////////////////////////
function TContentProtectionManager.BeginEnableContent(pEnablerActivate: IMFActivate;
                                                      pTopo: IMFTopology;
                                                      pCallback: IMFAsyncCallback;
                                                      punkState: IUnknown): HResult; stdcall;
var
  hr: HResult;

begin
  // Make sure we *never* leave this entry point with an exception
  Beep;
  Result := E_FAIL;

try
  if (FEnabler <> nil) then
    Exception.Create('A previous call is still pending');

    // Save so we can create an async result later
    FAsyncCallback := pCallback;
    FUnkState := punkState;

    // Create the enabler from the IMFActivate pointer.
    hr := pEnablerActivate.ActivateObject(IID_IMFContentEnabler,
                                          FEnabler);
    if FAILED(hr) then
      Exit;

    // Notify the application. The application will call DoEnable from the app thread.
    FState := TEnabler.Ready; // Reset the state.
    if assigned(FOnContentNotify) then
      FOnContentNotify(Self);

    Result := S_OK;
except
   // Do Nothing
end;
end;


/// ////////////////////////////////////////////////////////////////////
// Name: EndEnableContent
// Description:  Completes the enable action.
/// //////////////////////////////////////////////////////////////////////

function TContentProtectionManager.EndEnableContent(pResult: IMFAsyncResult): HResult; stdcall;
begin
  // Make sure we *never* leave this entry point with an exception
  Beep;
try
  if (pResult = Nil) then
    Exception.Create('NULL IMFAsyncResult');

  // Release interfaces, so that we're ready to accept another call
  // to BeginEnableContent.
  Safe_Release(FEnabler);
  Safe_Release(FpMEG);
  Safe_Release(FAsyncCallback);
  Safe_Release(FUnkState);

  Result := FStatus;
except
  Result := E_FAIL;
end;
end;


function TContentProtectionManager.GetParameters(out pdwFlags: DWORD;
                                                 out pdwQueue: DWORD): HResult; stdcall;
begin
  pdwFlags := 0;
  pdwQueue := 0;
  Result := E_NOTIMPL;
end;


///////////////////////////////////////////////////////////////////////
// Name: Invoke
// Description:  Callback for asynchronous BeginGetEvent method.
//
// pAsyncResult: Pointer to the result.
///////////////////////////////////////////////////////////////////////
function TContentProtectionManager.Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
var
  pEvent: IMFMediaEvent;
  meType: MediaEventType;

begin
  // Make sure we *never* leave this entry point with an exception
  Beep;

  try
    meType := MEUnknown; // Event type
    // PropVariant varEventData = new PropVariant();        // Event data

    // Get the event from the event queue.
    Result := FpMEG.EndGetEvent(pAsyncResult,
                                pEvent);
    if FAILED(Result) then
      Exit;

    // Get the event type.
    Result := pEvent.GetType(meType);
    if FAILED(Result) then
      Exit;

    // Get the event status. If the operation that triggered the event did
    // not succeed, the status is a failure code.
    Result := pEvent.GetStatus(FStatus);
    if FAILED(Result) then
      Exit;

    if (FStatus = 862022) then // NS_S_DRM_MONITOR_CANCELLED
      begin
        FStatus := MF_E_OPERATION_CANCELLED;
        FState := TEnabler.Complete;
      end;

    // For the MEEnablerCompleted action, notify the application.
    // Otherwise, request another event.
    if (meType = MEEnablerCompleted) then
      begin
        if assigned(FOnContentNotify) then
          FOnContentNotify(self);
      end
    else
      begin
        Result := FpMEG.BeginGetEvent(self, nil);
        if FAILED(Result) then
          Exit;
      end;

    // Clean up.
    pEvent := Nil;
  except
    Result := E_FAIL;
  end;
end;


function TContentProtectionManager.GetState(): TEnabler;
begin
  Result := FState;
end;


function TContentProtectionManager.GetStatus(): HResult;
begin
  Result := FStatus;
end;


/// ////////////////////////////////////////////////////////////////////
// Name: DoEnable
// Description:  Does the enabler action.
//
// flags: If ForceNonSilent, then always use non-silent enable.
// Otherwise, use silent enable if possible.
/// ////////////////////////////////////////////////////////////////////
procedure TContentProtectionManager.DoEnable(AFlags: TEnablerFlags);
var
  hr: HResult;
  bAutomatic: LongBool;
  guidEnableType: TGuid;

begin
  if not assigned(FEnabler) then
    Exit;

  bAutomatic := false;

  try
    // Get the enable type. (Just for logging. We don't use it.)
    hr := FEnabler.GetEnableType(guidEnableType);
    if FAILED(hr) then
      Exit;

    LogEnableType(guidEnableType);

    // Query for the IMFMediaEventGenerator interface so that we can get the
    // enabler events.
    FpMEG := FEnabler as IMFMediaEventGenerator;

    // Ask for the first event.
    hr := FpMEG.BeginGetEvent(self, nil);
    if FAILED(hr) then
      Exit;

    // Decide whether to use silent or non-silent enabling. If flags is ForceNonSilent,
    // then we use non-silent. Otherwise, we query whether the enabler object supports
    // silent enabling (also called "automatic" enabling).
    if (AFlags = TEnablerFlags.ForceNonSilent) then
      begin
        // Forcing non-silent enable.;
        bAutomatic := false;
      end
    else
      begin
        hr := FEnabler.IsAutomaticSupported(bAutomatic);
        if FAILED(hr) then
          Exit;
      end;

    // Start automatic or non-silent, depending.
    if (bAutomatic) then
      begin
        FState := TEnabler.SilentInProgress;
        // Content enabler: Automatic is supported;
        hr := FEnabler.AutomaticEnable();
        if FAILED(hr) then
          Exit;
      end
    else
      begin
        FState := TEnabler.NonSilentInProgress;
        // Content enabler: Using non-silent enabling;
        DoNonSilentEnable();
      end;
  except
    FStatus := E_FAIL;
  end;
end;


/// ////////////////////////////////////////////////////////////////////
// Name: CancelEnable
// Description:  Cancels the current action.
//
// During silent enable, this cancels the enable action in progress.
// During non-silent enable, this cancels the MonitorEnable thread.
/// ////////////////////////////////////////////////////////////////////
procedure TContentProtectionManager.CancelEnable();
var
  hr: HResult;
  pvar: PROPVARIANT;

begin
  if assigned(FEnabler) and (FState <> TEnabler.Complete) then
    begin
      try
        hr := FEnabler.Cancel();
        if FAILED(hr) then
          Exit;
      except
        hr := E_FAIL;
      end;

    if (hr < 0) then
      begin
        // If Cancel fails for some reason, queue the MEEnablerCompleted
        // event ourselves. This will cause the current action to fail.
        PropVariantInit(pvar);
        hr := FpMEG.QueueEvent(MEEnablerCompleted, TGuid.Empty, hr, pvar);
        PropVariantClear(pvar);
        if FAILED(hr) then
          Exit;
      end;
    end;
end;


/// ////////////////////////////////////////////////////////////////////
// Name: CompleteEnable
// Description:  Completes the current action.
//
// This method invokes the PMP session's callback.
/// ////////////////////////////////////////////////////////////////////
procedure TContentProtectionManager.CompleteEnable();
var
  hr: HResult;
  pResult: IMFAsyncResult;

begin
  FState := TEnabler.Complete;

  // m_pCallback can be NULL if the BeginEnableContent was not called.
  // This is the case when the application initiates the enable action, eg
  // when MFCreatePMPMediaSession fails and returns an IMFActivate pointer.
  if (FAsyncCallback <> nil) then
    begin
      hr := MFCreateAsyncResult(nil, FAsyncCallback, FUnkState, pResult);
      if FAILED(hr) then
        Exit;

      hr := pResult.SetStatus(FStatus);
      if FAILED(hr) then
        Exit;

      MFInvokeCallback(pResult);
    end;
end;


procedure TContentProtectionManager.LogEnableType(guidEnableType: TGuid);
begin
  if (guidEnableType = MFENABLETYPE_WMDRMV1_LicenseAcquisition) then
    begin
      //
    end
  else if (guidEnableType = MFENABLETYPE_WMDRMV7_LicenseAcquisition) then
    begin
      //
    end
  else if (guidEnableType = MFENABLETYPE_WMDRMV7_Individualization) then
    begin
      //
    end
  else if (guidEnableType = MFENABLETYPE_MF_UpdateRevocationInformation) then
    begin
      //
    end
  else if (guidEnableType = MFENABLETYPE_MF_UpdateUntrustedComponent) then
    begin
      //
    end;
end;


/// ////////////////////////////////////////////////////////////////////
// Name: DoNonSilentEnable
// Description:  Performs non-silent enable.
////////////////////////////////////////////////////////////////////////
procedure TContentProtectionManager.DoNonSilentEnable();
var
  hr: HResult;
  sUrl: PWidechar;
  cchURL: Cardinal;
  trustStatus: MF_URL_TRUST_STATUS;

  pPostData: PByte;
  cbPostDataSize: Cardinal;

begin
  if not assigned(FEnabler) then
    Exit;

  hr := FEnabler.GetEnableURL(sUrl, cchURL, trustStatus);
    if FAILED(hr) then
      Exit;

  if (trustStatus <> MF_LICENSE_URL_TRUSTED) then
    Exception.Create('The enabler URL is not trusted. Failing.');

  // Start the thread that monitors the non-silent enable action.
  hr := FEnabler.MonitorEnable();
  if FAILED(hr) then
    Exit;

  // Get the HTTP POST data
  hr := FEnabler.GetEnableData(pPostData,
                               cbPostDataSize);
  if FAILED(hr) then
    Exit;

  // Open the URL and send the HTTP POST data.
  // TODO: m_webHelper.OpenURLWithData(sURL, pPostData, cbPostDataSize, m_hwnd);
  FreeMem(pPostData,
          cbPostDataSize);
end;

end.
