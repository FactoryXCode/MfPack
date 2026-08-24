// FactoryX
//
// Copyright: Â© FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfpPlayerClassX.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: This is the extended player class of MfPlayer X that supports
//              ChromeCast, Android TV, burned inn subtitles etc.
//
// Company: FactoryX
// Intiator(s): Ramyses De Macedo Rodrigues, Tony (maXcomX), Peter (OzShips).
// Contributor(s): Ramyses De Macedo Rodrigues,
//                 Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Jason Nelson (adaloveless)
//                 Ciaran (Ciaran3)
//                 Carmen (carmenh)
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
unit MfPlayerClassX;

  {$TYPEINFO ON}

interface

uses

  {Winapi}
  Winapi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ComBaseApi,
  WinApi.WinError,
  {Vcl}
  VCL.Graphics,
  VCL.ExtCtrls,
  Vcl.Forms,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Win.ComObj,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  TimedTextClass,
  MfMediaTimeline,
  MfSubtitleCompositor,
  MfEmbeddedSubtitleReader,
  MfSubtitleFramePump,
  MfSubtitleTransform,
  MFTimerCallBackClass,
  MfPCXConstants,
  LangTags;

type
  TRedrawStatus = (rdStarted,
                   rdStopped);

  TRequest = (reqNone = 0,
              reqStop,
              reqStart,           // start after Pause or stopped
              reqPause,
              reqSeek,
              reqClose,
              reqRate,
              reqCaptureStart,
              reqCaptureStop,
              reqSnapShot);

  TPlayerState = (
                  // Media session and Media Engine specific
                  Closed = 0,      // No session.
                  Ready,           // Session was created, ready to open a file.
                  OpenPending,     // Session is opening a file.
                  Starting,        // Session initializing Start
                  Started,         // Session or Media Engine is playing a file.
                  Pausing,         // Session initializing pause.
                  Paused,          // Session or mediaengine is paused.
                  Stopping,        // Session initializing Stop.
                  Stopped,         // Session is stopped (ready to play).
                  Closing,         // Application has closed the session, but is waiting for MESessionClosed event.
                  Seeking,         // Session or mediaengine has started seeking to a new playback position..
                  SeekingReady,    // Session or Media Engine has seeked to a new playback position.
                  TopologyReady    // Session topology has been set.
                  );

  // Describes the current or requested state, with respect to seeking and
  // playback rate.
  TSeekState = record
    fRequestedRate: FLOAT;        // Requested playback rate
    bCanThinPb: BOOL;             // Thinned playback, if supported
    bCanScrub: Boolean;           // Scrubbing, if supported
    SeekStart: MFTIME;            // Seek from this position
  end;

  TController = record
    Request: TRequest;            // Request an action
    State: TPlayerState;          // current state the player is in
    SeekState: TSeekState;        // current or requested seekstate
    dwSessionCaps: DWORD;         // Session caps.
    fMaxRateSupported: FLOAT;     // Maximum rate that is supported
    fMinRateSupported: FLOAT;     // Minimum rate that is supported
    fReverseRateSupported: FLOAT; // Fastest reversed playback rate
    fInitialRate: FLOAT;          // Initial or Actual playback rate
    fCurrentRate: FLOAT;          // The current rate
    StartPosition: MFTIME;        // Start from this position
    CurrentPosition: MFTIME;      // Current position in milliseconds
    uiDuration: MFTIME;           // Duration
    uiFileSize: UINT64;           // Filesize
    Volume: FLOAT;                // Volume
    SourceStreams: DWORD;         // Number of sourcestreams
  end;

  TMfVideoAlphaBitmapParamsX2 = record
    dwFlags: DWORD;
    clrSrcKey: COLORREF;
    rcSrc: TRect;
    nrcDest: TRectF;
    fAlpha: FLOAT;
    dwFilterMode: DWORD;
  end;

  TMfVideoAlphaBitmapSourceX2 = record
    case Integer of
      0: (hdc: HDC);
      1: (pDDS: Pointer);
  end;

  TMfVideoAlphaBitmapX2 = record
    GetBitmapFromDC: BOOL;
    Source: TMfVideoAlphaBitmapSourceX2;
    params: TMfVideoAlphaBitmapParamsX2;
  end;

  IMfVideoMixerBitmapX2 = interface(IUnknown)
    ['{814C7B20-0FDB-4eec-AF8F-F957C8F69EDC}']
    function SetAlphaBitmap(var pBmpParms: TMfVideoAlphaBitmapX2): HResult; stdcall;
    function ClearAlphaBitmap(): HResult; stdcall;
    function UpdateAlphaBitmapParameters(var pBmpParms: TMfVideoAlphaBitmapParamsX2): HResult; stdcall;
    function GetAlphaBitmapParameters(out pBmpParms: TMfVideoAlphaBitmapParamsX2): HResult; stdcall;
  end;
  IID_IMfVideoMixerBitmapX2 = IMfVideoMixerBitmapX2;

  // This interface will handle all events comming from the presentationclock.
  // From here you may trigger the events or calls needed for example, to start an stop the IMFTimer.
  TClockStateSink = class(TInterfacedPersistent, IMFClockStateSink)
   private
    { private fields }
     p_MFTime: MFTIME;
     p_flRate: Single;

    { private methods }
    function OnClockStart(hnsSystemTime: MFTIME;
                          llClockStartOffset: LongLong): HResult; stdcall;
    function OnClockStop(hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockPause(hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockRestart(hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockSetRate(hnsSystemTime: MFTIME;
                            flRate: Single): HResult; stdcall;

   public
     { public methods }

     // Constructor, destructor
     constructor Create();
     destructor Destroy(); override;

   published
     property SystemTime: MFTIME read p_MFTime;
     property Rate: Single read p_flRate;
  end;


  TMfPlayerX = class(TInterfacedPersistent, IMFAsyncCallback)
  strict private
    {private fields}

    // Internal objects
    stRedrawStatus:       TRedrawStatus;
    m_dWaitResult:        DWORD;

    m_bPending:           Boolean;       // Is a request pending?
    m_bCanSetRateForward: Boolean;       // Supports forward rate adjustment
    m_bCanSetRateReverse: Boolean;       // Supports reverse rate adjustment
    m_bAppIsClosing:      Boolean;       // Closing flag

    FFileName:            WideString;    // filename incl path
    FSubtitleLanguage:    string;        // Language of the subtitles
    m_dCaps:              DWord;         // MFSESSIONCAP_* (MfApi) = Session caps.

    // Contains requests, state etc.
    mfpControl:           TController;

    // Colorkey needed to draw transparent on the videosurface.
    FBGColor: COLORREF;

    FOnBGColor:           TNotifyEvent;
    sCustomMessage:       string; // Hold custom messages to be send to the control that
                                  // is assigned by m_hwndSub

    nChannels:            UINT32; // holds the number of volumechannels
    FMediaTimeline:       TMfMediaTimeline;
    FSubtitleCompositor:  TMfSubtitleCompositor;
    FSubtitlesEnabled:    Boolean;
    FVideoMixerBitmap:    IMfVideoMixerBitmapX2;
    FSubtitleBitmap:      TBitmap;
    FSubtitleBitmapText:  string;
    FSubtitleBitmapVisible: Boolean;
    FSubtitleBitmapWidth: Integer;
    FSubtitleBitmapHeight: Integer;
    FSubtitleAspectRatio: Single;
    FSubtitlePlaybackTransform: IMFTransform;
    FSubtitlePlaybackControl: IMfSubtitleVideoTransformControl;

    {private methods}

    // Catches all messages to this object.
    // From here all WM_PROGRESSNOTIFY messages will be send.
    procedure WndProc(var Msg: TMessage);

    // This function is called by public SetPosition.
    function SetPositionInternal(tPos: MFTIME): HRESULT;
    procedure GotoNewPosition(val: MFTIME);

    // Rate
    // Rate control checkers
    procedure CheckCanSetRateForward();
    procedure CheckCanSetRateReverse();
    function CommitRateChange(fRate: FLOAT; bThin: Boolean): HResult;
    function GetNominalRate(): FLOAT;

    // Screen settings
    procedure SetVideoScreen(val: HWND);
    function GetVideoScreen(): HWND;

    // Deprecated: Not available after SDK RedStone 4
    // See: https://docs.microsoft.com/en-us/windows/desktop/api/evr/nf-evr-imfvideodisplaycontrol-setfullscreen
    // procedure SetFullScreen(val: BOOL);
    // function IsFullScreen(): BOOL;

    function UpdatePendingCommands(req: TRequest): HRESULT;

    procedure UpdateCaption();
    procedure ClearSubtitleBitmap();
    function EnsureVideoMixerBitmap(): HRESULT;
    function GetSubtitleTargetRect(const ClientRect: TRect): TRect;
    function RenderSubtitleBitmap(const SubtitleText: string): HRESULT;
    procedure ResetSubtitleBitmapCache();
    procedure UpdateSubtitleBitmap(MediaTimeMs: Int64);
    function EnsurePresentationClock(): HRESULT;
    function CreatePlaybackTopologyX2(pSource: IMFMediaSource;
                                      pPD: IMFPresentationDescriptor;
                                      hVideoWnd: HWND;
                                      var ppTopology: IMFTopology;
                                      out SourceStreams: DWORD): HRESULT;
    function AddBranchToPlaybackTopologyX2(pTopology: IMFTopology;
                                           pSource: IMFMediaSource;
                                           pPD: IMFPresentationDescriptor;
                                           dwStream: DWORD;
                                           hVideoWnd: HWND): HRESULT;

    // Conversions
    procedure SetFileName(aValue: WideString);
    procedure SetSubtitleLanguage(aValue: string);
    procedure SetSubtitleAspectRatio(aValue: Single);
    function GetTimedTextFileLoaded(): Boolean;
    function GetSubtitleSourcesAvailable(): Boolean;
    procedure SetSubtitlesEnabled(aValue: Boolean);

    // Status indicators
    function GetState(): TPlayerState;
    procedure SetState(value: TPlayerState);

    // Request indicators
    procedure SetRequest(tcRequest: TRequest);
    function GetRequest(): TRequest;

    // Set custom message
    procedure SetCustomMessage(value: string);


  protected
    {protected fields}

    FhCloseEvent:        THandle;                   // Event to wait on while closing.

    // interfaces
    m_pSession:           IMFMediaSession;           // Media session
    m_pSource:            IMFMediaSource;            // Media source
    m_pVideoDisplay:      IMFVideoDisplayControl;    // Video control
    m_pTopology:          IMFTopology;               // Topology
    m_pTimeSource:        IMFPresentationTimeSource; // Interface needed to get the clock properties
    m_pClockStateSink:    TClockStateSink;           //
    m_pRateControl:       IMFRateControl;            // Rate control
    m_pRateSupport:       IMFRateSupport;            // Rate support
    m_pSourcePD:          IMFPresentationDescriptor; // Describes the details of a presentation.
                                                     // A presentation is a set of related media streams that share a common presentation time.
    MFCallBack:           TMFCallBack;

    // Handles
    m_hwndEvent:         HWnd;           // App window to receive events.
    m_hwndVideo:         HWnd;           // Video window.
    m_hwndMainForm:      HWnd;           // Handle to the main form.
    m_hwndThis:          HWnd;           // Handle to this class.

    m_aStreamCont: TStreamContentsArray;

    {protected methods}

    // Initialize the player
    function Initialize(): HRESULT;
    // close instance of the media session
    function CloseSession(): HRESULT;
    // create a new instance of the media session
    function CreateSession(pPD: IMFPresentationDescriptor): HRESULT;

    // Clean up
    procedure ResetController();
    procedure Clear();

    //--------------------------------------------------------------------------
    // Media event handlers
    //
    // Event handler functions /////////////////////////////////////////////////
    // The functions listed here, are the minimum number of handlers you need.
    // See: Content in the function Invoke() override to direct all session events.
    // The handlers are catched and called by function Invoke

    // Handler for MESessionTopologyStatus event
    function OnSessionTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MESessionTopologySet event
    function OnSessionTopologyReady(pEvent: IMFMediaEvent): HRESULT; virtual;
    // Handler for event MESessionNotifyPresentationTime
    function OnSessionNotifyPresentationTime(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MEEndOfPresentation event
    function OnEndOfPresentation(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MENewPresentation event.
    // This event is sent if the media source has a new presentation, which
    // requires a new topology.
    function OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;

    function OnSessionStarted(pEvent: IMFMediaEvent): HRESULT; // Start event request
    function OnSessionPaused(pEvent: IMFMediaEvent): HRESULT;  // Pause event request
    function OnSessionClosed(pEvent: IMFMediaEvent): HRESULT;  // Called when session is closed asynchronous
    function OnSessionRateChanged(pEvent: IMFMediaEvent): HRESULT; // Called when the rate has been changed
    function OnSessionStopped(pEvent: IMFMediaEvent): HRESULT; // Explicit Stop request completed
    function OnSessionEnded(pEvent: IMFMediaEvent): HRESULT;   // Playback pipeline fully drained

    ////////////////////////////////////////////////////////////////////////////

    // IMFAsyncCallback methods ------------------------------------------------

    // Implementation of this method is optional.
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

    //--------------------------------------------------------------------------

  public
    {public fields}

    m_VolumeChannels: TFloatArray;           // Dynamic array that holds the volume per channel

    // Constructor, destructor
    constructor Create(hwndVideo: HWND;
                       hwndSub: HWND;
                       hwndEvent: HWND;
                       hwndMainForm: HWND);
    // Handles stuff like checking reference counting before reaching the destructor.
    procedure BeforeDestruction(); override;
    destructor Destroy(); override;

    // playback (creates the session)
    function OpenURL(sURL: PWideChar): HRESULT;

    // Play, pause, stop (Use SendPlayerCmd())
    function Start(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function ReloadTimedText(): HRESULT;
    function RefreshEmbeddedSubtitleTracks(): HRESULT;
    function GetEmbeddedSubtitleTracks(out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
    function GetPreferredEmbeddedSubtitleStreamIndex(out StreamIndex: DWORD): HRESULT;
    function SelectEmbeddedSubtitleTrack(StreamIndex: DWORD): HRESULT;
    function SelectSidecarSubtitleLanguage(const LanguageTag: string): HRESULT;
    procedure CommitSubtitleSelection();
    function GetActiveEmbeddedSubtitleStreamIndex(): Integer;
    function GetActiveSubtitleIsEmbedded(): Boolean;

    function ExportActiveSubtitlesAsWebVtt(out AData: TBytes;
                                           out ALanguageTag: string;
                                           out AFriendlyLanguageName: string): HRESULT;

    // Shut down the session and MF
    // Use this funtion to kill the MfPlayer.
    function ShutDown(): HRESULT;

    // Video functionality
    function BurnSubtitlesToFile(const OutputFileName: WideString;
                                 Bitrate: UINT32 = 8000000): HRESULT;
    function Repaint(): HRESULT;

    // Resizes the video rectangle.
    // The application calls this method if the size of the video window changes;
    // e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(pdRect: LPRECT = nil): HRESULT;

    // Start or stops redrawing surfaces (anti flicker)
    procedure SetRedraw();

    // Check if stream contains video
    function HasVideo(): boolean;

    // Retrieves the video dimension
    function GetVideoRectangle(): TRect;

    // Function to control the player
    procedure SendPlayerRequest(req: TRequest);

    // Frame capture
    function TakeSnapShot(var bit: TBitMap): HRESULT;

    // Volume
    procedure SetVolume(Value: TFloatArray);
    function GetVolume(): HRESULT;

    // Sets the current playback position (calls SetPositionInternal).
    function SetPosition(hnsPosition: MFTIME): HRESULT;
    function GetPosition(out hnsPosition: MFTIME): HRESULT;

    // Rate control
    function InitiateRateControl(): HRESULT; // initialise the rate interfaces.
    procedure SetRate(val: FLOAT);
    function GetRate(): FLOAT;

    // Returns the active(current) stream
    function GetActiveStreamType(stType: TMediaTypes;
                                 out iStreamIndex: DWord): HRESULT;
    // Set active stream
    function SetActiveStreamType(stType: TMediaTypes;
                                 iStreamIndex: DWord): HRESULT;

    // Properties
    /////////////////////

    // The Colorkey is the color that the Overlay Mixer Filter used by DSVideoWindowEx sees
    // as transparent, when you draw ontop of the movie, always set the canvas brush
    // color to this color or set the style to bsclear.
    // Note: The colors returned through this method vary depending on the current display mode.
    //       If the colors are 8-bit palettized, they will be bright system colors (such as magenta).
    //       If the display is in a true-color mode, they will be shades of black.
    property BackGroundColor: COLORREF read FBGColor;
    property CanSetRateForward: Boolean read m_bCanSetRateForward;
    property CanSetRateReverse: Boolean read m_bCanSetRateReverse;
    // This property is intended to display a custom on screen text.
    property CustomMessage: string read sCustomMessage write SetCustomMessage;
    property Duration: MFTIME read mfpControl.uiDuration;
    // Deprecated: Not available after SDKversion RedStone4.
    //property FullScreen: BOOL read IsFullScreen write SetFullScreen;
    property MaxPlayBackRate: FLOAT read mfpControl.fMaxRateSupported;
    property MediaFileName: WideString read FFileName write SetFileName;
    property MinPlayBackRate: FLOAT read mfpControl.fMinRateSupported;
    // Event to tell the main application that the video backgroundcolor has changed.
    //  Note: If you have controls placed ontop of a clipping window that need to act as
    //        transparent, set their color to the same as the backgroundcolor.
    property OnBackGroundColorChanged: TNotifyEvent read FOnBGColor write FOnBGColor;
    property PlaybackRate: FLOAT read GetRate write SetRate;
    property Position: MFTIME read mfpControl.CurrentPosition;
    property Request: TRequest read GetRequest write SetRequest;
    property SetNewPosition: MFTIME write GotoNewPosition;
    property SetVideoSurface: HWND read GetVideoScreen write SetVideoScreen;
    property SoundChannels: UINT32 read nChannels;
    property State: TPlayerState read GetState write SetState;
    property StreamContents: TStreamContentsArray read m_aStreamCont;
    property SubtitleLanguage: string read FSubtitleLanguage write SetSubtitleLanguage;
    property SubtitleAspectRatio: Single read FSubtitleAspectRatio write SetSubtitleAspectRatio;
    property SubtitleCompositor: TMfSubtitleCompositor read FSubtitleCompositor;
    property SubtitlesEnabled: Boolean read FSubtitlesEnabled write SetSubtitlesEnabled;
    property TimedTextFileLoaded: Boolean read GetTimedTextFileLoaded;
    property SubtitleSourcesAvailable: Boolean read GetSubtitleSourcesAvailable;
    property VideoRectangle: TRect read GetVideoRectangle;
    property Volumes: TFloatArray read m_VolumeChannels write SetVolume;
  end;

var
  MfPlayerX: TMfPlayerX;


implementation

////////////// TMFPlay /////////////////////////////////////////////////////////

procedure TMfPlayerX.ResetSubtitleBitmapCache();
begin

  FSubtitleBitmapText := '';
  FSubtitleBitmapVisible := False;
  FSubtitleBitmapWidth := 0;
  FSubtitleBitmapHeight := 0;
end;


procedure TMfPlayerX.ClearSubtitleBitmap();
begin

  if Assigned(FVideoMixerBitmap) then
    {void} FVideoMixerBitmap.ClearAlphaBitmap();

  ResetSubtitleBitmapCache();
end;

function TMfPlayerX.EnsureVideoMixerBitmap(): HRESULT;
begin
  if Assigned(FVideoMixerBitmap) then
    begin
      Result := S_OK;
      Exit;
    end;

  if not Assigned(m_pSession) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  try
    Result := (m_pSession as IMFGetService).GetService(MR_VIDEO_MIXER_SERVICE,
                                                       IID_IMfVideoMixerBitmapX2,
                                                       Pointer(FVideoMixerBitmap));
  except
    FVideoMixerBitmap := nil;
    Result := E_NOINTERFACE;
  end;
end;

function TMfPlayerX.GetSubtitleTargetRect(const ClientRect: TRect): TRect;
var
  clientWidth: Integer;
  clientHeight: Integer;
  targetWidth: Integer;
  targetHeight: Integer;
  targetLeft: Integer;
  targetTop: Integer;
  clientRatio: Single;

begin

  Result := ClientRect;
  clientWidth := ClientRect.Right - ClientRect.Left;
  clientHeight := ClientRect.Bottom - ClientRect.Top;

  if (clientWidth <= 0) or (clientHeight <= 0) or (FSubtitleAspectRatio <= 0.0) then
    Exit;

  clientRatio := clientWidth / clientHeight;
  if (clientRatio > FSubtitleAspectRatio) then
    begin
      targetHeight := clientHeight;
      targetWidth := Round(targetHeight * FSubtitleAspectRatio);
      targetLeft := ClientRect.Left + ((clientWidth - targetWidth) div 2);
      targetTop := ClientRect.Top;
    end
  else
    begin
      targetWidth := clientWidth;
      targetHeight := Round(targetWidth / FSubtitleAspectRatio);
      targetLeft := ClientRect.Left;
      targetTop := ClientRect.Top + ((clientHeight - targetHeight) div 2);
    end;

  Result := Rect(targetLeft,
                 targetTop,
                 targetLeft + targetWidth,
                 targetTop + targetHeight);
end;

function TMfPlayerX.RenderSubtitleBitmap(const SubtitleText: string): HRESULT;
const
  TRANSPARENT_COLOR = TColor($010101);

var
  rcClient: TRect;
  rcVideo: TRect;
  rcText: TRect;
  rcDraw: TRect;
  bmpParms: TMfVideoAlphaBitmapX2;
  textValue: WideString;
  textFlags: UINT;
  fontSize: Integer;
  marginX: Integer;
  marginBottom: Integer;
  textHeight: Integer;
  videoWidth: Integer;
  videoHeight: Integer;
  dx: Integer;
  dy: Integer;

begin

  Result := EnsureVideoMixerBitmap();
  if FAILED(Result) then
    Exit;

  if not WinApi.Windows.GetClientRect(m_hwndVideo,
                                      rcClient) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  FSubtitleBitmapWidth := rcClient.Right - rcClient.Left;
  FSubtitleBitmapHeight := rcClient.Bottom - rcClient.Top;
  if (FSubtitleBitmapWidth <= 0) or (FSubtitleBitmapHeight <= 0) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  rcVideo := GetSubtitleTargetRect(rcClient);
  videoWidth := rcVideo.Right - rcVideo.Left;
  videoHeight := rcVideo.Bottom - rcVideo.Top;
  if (videoWidth <= 0) or (videoHeight <= 0) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  if not Assigned(FSubtitleBitmap) then
    FSubtitleBitmap := TBitmap.Create();

  FSubtitleBitmap.PixelFormat := pf24bit;
  FSubtitleBitmap.SetSize(FSubtitleBitmapWidth,
                          FSubtitleBitmapHeight);

  FSubtitleBitmap.Canvas.Brush.Style := bsSolid;
  FSubtitleBitmap.Canvas.Brush.Color := TRANSPARENT_COLOR;
  FSubtitleBitmap.Canvas.FillRect(Rect(0,
                                       0,
                                       FSubtitleBitmapWidth,
                                       FSubtitleBitmapHeight));

  fontSize := videoHeight div 18;
  if (fontSize < 16) then
    fontSize := 16
  else if fontSize > 34 then
    fontSize := 34;

  FSubtitleBitmap.Canvas.Font.Name := 'Segoe UI';
  FSubtitleBitmap.Canvas.Font.Size := fontSize;
  FSubtitleBitmap.Canvas.Font.Style := [fsBold];
  FSubtitleBitmap.Canvas.Font.Quality := fqAntialiased;
  FSubtitleBitmap.Canvas.Brush.Style := bsClear;
  SetBkMode(FSubtitleBitmap.Canvas.Handle,
            TRANSPARENT);

  marginX := videoWidth div 10;
  marginBottom := videoHeight div 20;
  if (marginX < 24) then
    marginX := 24;

  rcText := Rect(rcVideo.Left + marginX,
                 rcVideo.Top,
                 rcVideo.Right - marginX,
                 rcVideo.Bottom);
  if (rcText.Left >= rcText.Right) then
    rcText := rcVideo;
  rcDraw := rcText;
  textValue := SubtitleText;
  textFlags := DT_CENTER or DT_WORDBREAK or DT_NOPREFIX;

  DrawTextW(FSubtitleBitmap.Canvas.Handle,
            PWideChar(textValue),
            Length(textValue),
            rcDraw,
            textFlags or DT_CALCRECT);

  textHeight := rcDraw.Bottom - rcDraw.Top;
  rcDraw.Left := rcText.Left;
  rcDraw.Right := rcText.Right;
  rcDraw.Bottom := rcVideo.Bottom - marginBottom;
  rcDraw.Top := rcDraw.Bottom - textHeight;
  if (rcDraw.Top < rcVideo.Top) then
    rcDraw.Top := rcVideo.Top;

  FSubtitleBitmap.Canvas.Font.Color := clBlack;
  for dx := -2 to 2 do
    for dy := -2 to 2 do
      if (dx <> 0) or (dy <> 0) then
        begin

          rcText := rcDraw;
          OffsetRect(rcText,
                     dx,
                     dy);
          DrawTextW(FSubtitleBitmap.Canvas.Handle,
                    PWideChar(textValue),
                    Length(textValue),
                    rcText,
                    textFlags);
        end;

  FSubtitleBitmap.Canvas.Font.Color := clWhite;
  DrawTextW(FSubtitleBitmap.Canvas.Handle,
            PWideChar(textValue),
            Length(textValue),
            rcDraw,
            textFlags);

  ZeroMemory(@bmpParms,
             SizeOf(bmpParms));
  bmpParms.GetBitmapFromDC := True;
  bmpParms.Source.hdc := FSubtitleBitmap.Canvas.Handle;
  bmpParms.params.dwFlags := DWORD(MFVideoAlphaBitmap_SrcColorKey) or
                              DWORD(MFVideoAlphaBitmap_SrcRect) or
                              DWORD(MFVideoAlphaBitmap_DestRect) or
                              DWORD(MFVideoAlphaBitmap_Alpha);
  bmpParms.params.clrSrcKey := ColorToRGB(TRANSPARENT_COLOR);
  bmpParms.params.rcSrc := Rect(0,
                                0,
                                FSubtitleBitmapWidth,
                                FSubtitleBitmapHeight);
  bmpParms.params.nrcDest.Left := 0.0;
  bmpParms.params.nrcDest.Top := 0.0;
  bmpParms.params.nrcDest.Right := 1.0;
  bmpParms.params.nrcDest.Bottom := 1.0;
  bmpParms.params.fAlpha := 1.0;

  Result := FVideoMixerBitmap.SetAlphaBitmap(bmpParms);
  if SUCCEEDED(Result) then
    begin

      FSubtitleBitmapText := SubtitleText;
      FSubtitleBitmapVisible := True;
    end;
end;

procedure TMfPlayerX.UpdateSubtitleBitmap(MediaTimeMs: Int64);
var
  subtitleText: string;
  track: TSubTitleTrack;
begin
  if (not FSubtitlesEnabled) or
     (not TimedTextFileLoaded) or
     (not Assigned(FSubtitleCompositor)) then
    begin
      if FSubtitleBitmapVisible then
        ClearSubtitleBitmap();
      Exit;
    end;

  if not FSubtitleCompositor.TryGetSubtitleTextAtTime(MediaTimeMs,
                                                      subtitleText,
                                                      track) then
    begin
      if FSubtitleBitmapVisible then
        ClearSubtitleBitmap();
      Exit;
    end;

  subtitleText := Trim(subtitleText);
  if (subtitleText = '') then
    begin
      if FSubtitleBitmapVisible then
        ClearSubtitleBitmap();
      Exit;
    end;

  if FSubtitleBitmapVisible and SameStr(FSubtitleBitmapText,
                                        subtitleText) then
    Exit;

  //void
  RenderSubtitleBitmap(subtitleText);
end;

function TMfPlayerX.EnsurePresentationClock(): HRESULT;
var
  pClock: IMFClock;

begin

  if m_bAppIsClosing then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  if not Assigned(m_pSession) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  pClock := nil;

  if not Assigned(MFPresentationClock) then
    begin
      Result := m_pSession.GetClock(pClock);
      if FAILED(Result) then
        Exit;

      if m_bAppIsClosing then
        begin

          pClock := nil;
          Result := MF_E_SHUTDOWN;
          Exit;
        end;

      Result := pClock.QueryInterface(IID_IMFPresentationClock,
                                      MFPresentationClock);
      if FAILED(Result) then
        Exit;
    end;

  // Closing might have started while GetClock or QueryInterface was running.
  if m_bAppIsClosing then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  if not Assigned(m_pClockStateSink) then
    begin
      m_pClockStateSink := TClockStateSink.Create();

      Result := MFPresentationClock.AddClockStateSink(m_pClockStateSink);
      if FAILED(Result) then
        begin
          FreeAndNil(m_pClockStateSink);
          Exit;
        end;
    end;

  if m_bAppIsClosing then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  if not Assigned(MFCallBack) then
    begin
      MFCallBack := TMFCallBack.Create(m_hwndThis);

      if not Assigned(MFCallBack) then
        begin
          Result := E_OUTOFMEMORY;
          Exit;
        end;
    end;

  Result := S_OK;
end;


function TMfPlayerX.CreatePlaybackTopologyX2(pSource: IMFMediaSource;
                                             pPD: IMFPresentationDescriptor;
                                             hVideoWnd: HWND;
                                             var ppTopology: IMFTopology;
                                             out SourceStreams: DWORD): HRESULT;
var
  tmpTopology: IMFTopology;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  majorType: TGUID;
  fSelected: BOOL;
  streamIndex: Integer;
  firstVideoStream: Integer;
  firstAudioStream: Integer;
  selectedVideoFound: Boolean;
  selectedAudioFound: Boolean;

begin

  ppTopology := nil;
  SourceStreams := 0;
  tmpTopology := nil;

  firstVideoStream := -1;
  firstAudioStream := -1;
  selectedVideoFound := False;
  selectedAudioFound := False;

  Result := MFCreateTopology(tmpTopology);
  if FAILED(Result) then
    Exit;

  Result := pPD.GetStreamDescriptorCount(SourceStreams);
  if FAILED(Result) then
    Exit;

  // First inspect the presentation descriptor. Some Media Foundation sources,
  // notably certain sources with one video stream and no audio stream, can
  // return that video stream initially deselected. The old code attempted to
  // build the branch first and selected the stream only afterwards, leaving a
  // selected stream without source/output nodes in the topology.
  for streamIndex := 0 to Integer(SourceStreams) - 1 do
    begin
      pSD := nil;
      pHandler := nil;
      majorType := GUID_NULL;
      fSelected := False;

      Result := pPD.GetStreamDescriptorByIndex(DWORD(streamIndex),
                                               fSelected,
                                               pSD);
      if FAILED(Result) then
        Exit;

      Result := pSD.GetMediaTypeHandler(pHandler);
      if FAILED(Result) then
        Exit;

      Result := pHandler.GetMajorType(majorType);
      if FAILED(Result) then
        Exit;

      if IsEqualGUID(majorType,
                     MFMediaType_Video) then
        begin
          if (firstVideoStream < 0) then
            firstVideoStream := streamIndex;
          if fSelected then
            selectedVideoFound := True;
        end
      else
        if IsEqualGUID(majorType,
                       MFMediaType_Audio) then
          begin
            if (firstAudioStream < 0) then
              firstAudioStream := streamIndex;
            if fSelected then
              selectedAudioFound := True;
          end
        else
          if fSelected then
            begin
              // MfPlayer X2 renders sidecar subtitles itself. Other selected source
              // streams have no renderer branch and must therefore be deselected.
              Result := pPD.DeselectStream(DWORD(streamIndex));
              if FAILED(Result) then
                Exit;
           end;
    end;

  // Ensure that one default stream of each playable major type is selected.
  // Selection has to happen before AddBranchToPlaybackTopologyX2 reads the
  // descriptor and decides whether to create topology nodes.
  if (not selectedVideoFound) and (firstVideoStream >= 0) then
    begin
      Result := pPD.SelectStream(DWORD(firstVideoStream));
      if FAILED(Result) then
        Exit;
    end;

  if (not selectedAudioFound) and (firstAudioStream >= 0) then
    begin
      Result := pPD.SelectStream(DWORD(firstAudioStream));
      if FAILED(Result) then
        Exit;
    end;

  if (firstVideoStream < 0) and (firstAudioStream < 0) then
    begin
      Result := MF_E_UNSUPPORTED_BYTESTREAM_TYPE;
      Exit;
    end;

  // Now build branches only for the streams selected in the corrected
  // presentation descriptor.
  for streamIndex := 0 to Integer(SourceStreams) - 1 do
    begin
      Result := AddBranchToPlaybackTopologyX2(tmpTopology,
                                              pSource,
                                              pPD,
                                              DWORD(streamIndex),
                                              hVideoWnd);
      if FAILED(Result) then
        Exit;
    end;

  ppTopology := tmpTopology;
  Result := S_OK;
end;


function TMfPlayerX.AddBranchToPlaybackTopologyX2(pTopology: IMFTopology;
                                                  pSource: IMFMediaSource;
                                                  pPD: IMFPresentationDescriptor;
                                                  dwStream: DWORD;
                                                  hVideoWnd: HWND): HRESULT;
var
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  pTransformNode: IMFTopologyNode;
  fSelected: BOOL;
  majorType: TGUID;
  isVideo: Boolean;
  transformObject: TMfSubtitleVideoTransform;
  transformInterface: IMFTransform;
  transformControl: IMfSubtitleVideoTransformControl;

begin

  pSD := nil;
  pHandler := nil;
  pSinkActivate := nil;
  pSourceNode := nil;
  pOutputNode := nil;
  pTransformNode := nil;
  majorType := GUID_NULL;
  isVideo := False;

  Result := pPD.GetStreamDescriptorByIndex(dwStream,
                                           fSelected,
                                           pSD);
  if FAILED(Result) then
    Exit;

  if not fSelected then
    begin
      Result := S_OK;
      Exit;
    end;

  // Determine the stream kind directly. Trying the video helper first and
  // treating every non-video stream as audio makes selected subtitle, data or
  // metadata streams fatal to topology construction.
  Result := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(Result) then
    Exit;

  Result := pHandler.GetMajorType(majorType);
  if FAILED(Result) then
    Exit;

  if IsEqualGUID(majorType,
                 MFMediaType_Video) then
    begin
      isVideo := True;
      Result := CreateVideoMediaSinkActivate(pSD,
                                             hVideoWnd,
                                             pSinkActivate);
    end
  else
    if IsEqualGUID(majorType,
                   MFMediaType_Audio) then
      Result := CreateAudioMediaSinkActivate(pSD,
                                             pSinkActivate)
    else
      begin
        // MfPlayer X2 renders only audio and video source streams. Unsupported
        // selected streams must be deselected, otherwise the source can start a
        // stream for which the topology has no sink branch.
        Result := pPD.DeselectStream(dwStream);
        if SUCCEEDED(Result) then
          Result := S_OK;
        Exit;
      end;

  if FAILED(Result) then
    Exit;

  Result := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);
  if FAILED(Result) then
    Exit;

  Result := AddOutPutNodeA(pTopology,
                           pSinkActivate,
                           0,
                           pOutputNode);
  if FAILED(Result) then
    Exit;

  if isVideo and Assigned(FSubtitleCompositor) and
     (not SameText(ExtractFileExt(FFileName), '.mp4')) then
    begin
      // Keep the local subtitle transform in every video topology. Subtitle
      // sources can be rescanned and selected after OpenURL; omitting the MFT
      // here would make that later selection impossible to render.
      OutputDebugString(PChar('MfPlayer X2: inserting local subtitle video transform'));
      transformObject := TMfSubtitleVideoTransform.Create(FSubtitleCompositor,
                                                          FMediaTimeline);
      transformInterface := transformObject as IMFTransform;
      transformControl := transformObject as IMfSubtitleVideoTransformControl;
      transformControl.SetEnabled(FSubtitlesEnabled);

      Result := AddTransformNodeM(pTopology,
                                  transformInterface,
                                  pTransformNode);
      if FAILED(Result) then
        Exit;

      // The source stream is normally compressed (for example H.264 in MP4),
      // while the subtitle MFT accepts decoded RGB32. Explicitly permit the
      // topology loader to insert the required decoder and color converter
      // upstream of this custom transform.
      Result := pTransformNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                         UINT32(MF_CONNECT_ALLOW_DECODER));
      if FAILED(Result) then
        Exit;

      // Likewise, permit a final color converter if the EVR does not accept
      // the exact RGB32 output type selected for the subtitle transform.
      Result := pOutputNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                      UINT32(MF_CONNECT_ALLOW_CONVERTER));
      if FAILED(Result) then
        Exit;

      Result := pSourceNode.ConnectOutput(0,
                                          pTransformNode,
                                          0);
      if FAILED(Result) then
        Exit;

      Result := pTransformNode.ConnectOutput(0,
                                             pOutputNode,
                                             0);
      if FAILED(Result) then
        Exit;

      FSubtitlePlaybackTransform := transformInterface;
      FSubtitlePlaybackControl := transformControl;
    end
  else
    begin

      if isVideo and SameText(ExtractFileExt(FFileName), '.mp4') then
        OutputDebugString(PChar(
          'MfPlayer X2: preserving native MP4 video path; subtitles use EVR overlay'));

      Result := pSourceNode.ConnectOutput(0,
                                          pOutputNode,
                                          0);
    end;
end;


procedure TMfPlayerX.ResetController();
begin

  with mfpControl do
    begin
      State := Closed;           // No session.
      Request := reqNone;        // No request

      with SeekState do
        begin
          fRequestedRate := 0.0;
          bCanThinPb := False;  // Thinned playback, if supported
          bCanScrub := False;
          SeekStart := 0;        // Seek starting point
        end;

      fMaxRateSupported := 0.0;  // Maximum rate that is supported
      fMinRateSupported := 0.0;  // Minimum rate that is supported
      fReverseRateSupported := 0.0; // Fastest reversed playback rate
      fInitialRate := 0.0;        // Actual playback rate
      fCurrentRate := fInitialRate;
      StartPosition := 0;
      CurrentPosition := 0;
      uiDuration := 0;
      Volume := 1.0;             // Set to the max
      m_dcaps := 0;
    end;
end;


procedure TMfPlayerX.Clear();
begin
  // shut timer down ?
  // We don't because the session wil do that automaticly.

  ResetController();
  nChannels := 0;
  SetLength(m_VolumeChannels, 0);

  // reset
  m_bPending := False;
  m_dCaps := 0;
  FFileName := '';
  FSubtitlesEnabled := False;
  ClearSubtitleBitmap();
  FVideoMixerBitmap := nil;

  if Assigned(FMediaTimeline) then
    FMediaTimeline.Reset();

  if Assigned(FSubtitleCompositor) then
    FSubtitleCompositor.Close();

  FSubtitlePlaybackControl := nil;
  FSubtitlePlaybackTransform := nil;

try

  // Don't free dynamic array's when the program is terminating.
  // They are managed types and therefor, they are freed automatically.
  if not m_bAppIsClosing then
    begin
      Finalize(m_VolumeChannels);
      Finalize(m_aStreamCont);
    end;

  // Release the interfaces
  SafeRelease(m_pTopology);
  FSubtitlePlaybackControl := nil;
  FSubtitlePlaybackTransform := nil;
  SafeRelease(m_pSession);
  SafeRelease(m_pSource);
  SafeRelease(m_pVideoDisplay);
  SafeRelease(m_pTimeSource);
  SafeRelease(m_pRateControl);
  SafeRelease(m_pRateSupport);
  SafeRelease(m_pSourcePD);

  // The following objects need to be deleted from memory.
  // They are created again when a new URL is opened.

  // Free the TimerCallBack object
  if Assigned(MFCallBack) then
    FreeAndnil(MFCallBack);

  if Assigned(m_pClockStateSink) then
    begin
      try
      // This call will also destroy the ClockStateSink.
      if Assigned(MFPresentationClock) then
        {void} MFPresentationClock.RemoveClockStateSink(m_pClockStateSink);
      finally
        MFPresentationClock := nil;
        FreeAndnil(m_pClockStateSink);
      end;
    end;

  SetLength(m_aStreamCont,
            0);
  Finalize(m_aStreamCont);

except
  //
end;
end;


procedure TMfPlayerX.SetCustomMessage(value: string);
begin

  sCustomMessage:= value;
end;


// The IMFMediaSession.Close method is asynchronous, but the
// MfPlayer.CloseSession method waits on the MESessionClosed event.
//
// MESessionClosed is guaranteed to be the last event that the
// media session fires.
function TMfPlayerX.CloseSession(): HRESULT;
var
  hr: HRESULT;
  hrStep: HRESULT;
  dwWaitResult: DWORD;

begin

  hr := S_OK;

  ClearSubtitleBitmap();
  FVideoMixerBitmap := nil;

  // Release services obtained from the session.
  SafeRelease(m_pVideoDisplay);
  SafeRelease(m_pTimeSource);
  SafeRelease(m_pRateControl);
  SafeRelease(m_pRateSupport);

  if Assigned(m_pSession) then
    begin
      if (FhCloseEvent <> 0) then
        ResetEvent(THandle(FhCloseEvent));

      State := Closing;

      // Close is sufficient. Do not require Stop to succeed first.
      hrStep := m_pSession.Close();

      // A repeated shutdown is harmless.
      if (hrStep = MF_E_SHUTDOWN) then
        hrStep := S_OK;

      if FAILED(hrStep) then
        hr := hrStep
      else
        if (FhCloseEvent <> 0) then
          begin
            // MESessionClosed is guaranteed to be the final session event.
            dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent),
                                                5000);
            m_dWaitResult := dwWaitResult;

            case dwWaitResult of
              WAIT_OBJECT_0: ;

              WAIT_TIMEOUT: if SUCCEEDED(hr) then
                              hr := HRESULT_FROM_WIN32(ERROR_TIMEOUT);
            else
              if SUCCEEDED(hr) then
                hr := HRESULT_FROM_WIN32(GetLastError());
            end;
          end;
    end;

  // Always perform best-effort synchronous shutdown, even when Close or
  // waiting for MESessionClosed reported an error.
  if Assigned(m_pSource) then
    begin
      hrStep := m_pSource.Shutdown();

      if (hrStep <> MF_E_SHUTDOWN) and
                    FAILED(hrStep) and
                    SUCCEEDED(hr) then
        hr := hrStep;
    end;

  if Assigned(m_pSession) then
    begin
      hrStep := m_pSession.Shutdown();

      if (hrStep <> MF_E_SHUTDOWN) and
                    FAILED(hrStep) and
                    SUCCEEDED(hr) then
        hr := hrStep;
    end;

  // Release player-owned topology references after the Media Session has
  // stopped using them.
  SafeRelease(m_pTopology);

  FSubtitlePlaybackControl := nil;
  FSubtitlePlaybackTransform := nil;

  SafeRelease(m_pSourcePD);
  SafeRelease(m_pSource);
  SafeRelease(m_pSession);

  State := Closed;
  m_bPending := False;
  mfpControl.Request := reqNone;

  Result := hr;
end;


procedure TMfPlayerX.WndProc(var Msg: TMessage);
var
  ClockPositionHns: MFTIME;
  ClockPositionMs: Int64;
  TimelinePositionMs: Int64;

begin

  // prevent processing messages when app is shutting down.
  if m_bAppIsClosing then
    Exit;

  if (Msg.Msg = WM_TIMERNOTIFY) then // Check for timer messages
    try

      if (Msg.LParam = S_OK) then
        begin
          // Update the cached position before notifying the form. Use a
          // separate HNS variable: CurrentPosition is stored in milliseconds.
          // The fallback timeline also keeps the UI moving when an MKV
          // presentation clock temporarily reports a frozen zero value.
          ClockPositionHns := 0;
          ClockPositionMs := -1;
          TimelinePositionMs := -1;

          if SUCCEEDED(GetPosition(ClockPositionHns)) then
            ClockPositionMs := HnsTimeToMsec(ClockPositionHns);

          if Assigned(FMediaTimeline) then
            TimelinePositionMs := FMediaTimeline.GetPositionMs();

          // Native MP4 playback has a reliable presentation clock.  Do not
          // let the wall-clock fallback run the UI and subtitle position ahead
          // of the actual decoded media position.
          if SameText(ExtractFileExt(FFileName), '.mp4') and
             (ClockPositionMs >= 0) then
            mfpControl.CurrentPosition := ClockPositionMs
          else if (ClockPositionMs >= 0) and (TimelinePositionMs >= 0) then
            begin
              if ClockPositionMs >= TimelinePositionMs then
                mfpControl.CurrentPosition := ClockPositionMs
              else
                mfpControl.CurrentPosition := TimelinePositionMs;
            end
          else
            if ClockPositionMs >= 0 then
              mfpControl.CurrentPosition := ClockPositionMs
            else
              if TimelinePositionMs >= 0 then
                mfpControl.CurrentPosition := TimelinePositionMs;

          if not Assigned(FSubtitlePlaybackControl) then
            UpdateSubtitleBitmap(mfpControl.CurrentPosition);

          SendMessage(m_hwndMainForm,
                      WM_PROGRESSNOTIFY,
                      WPARAM(1),
                      0);

          // X2 burns subtitles into video samples in the playback topology.
          UpdateCaption();
        end
      else
        if (Msg.LParam = MF_S_CLOCK_STOPPED) then
          begin
            // The presentationclock has stopped.
          end
        else // MF_E_SHUTDOWN
          begin
            // The clock was shut down.
          end;
    except
      Application.HandleException(Self);
    end
  else
    begin
      // This is a private AllocateHWnd message target used only by the MF timer
      // callback. Avoid DefWindowProc here; stale private-window messages during
      // media-session transitions can otherwise re-enter user32 from the player
      // object callback.
      Msg.Result := 0;
    end;
end;


// CONSTRUCTOR
constructor TMfPlayerX.Create(hwndVideo: HWND;
                              hwndSub: HWND;
                              hwndEvent: HWND;
                              hwndMainForm: HWND);
var
  hr: HRESULT;

begin

  inherited Create();

  // Check if the current MF version match user's
  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 LPCWSTR('Your computer does not support this Media Foundation API version' +
                       IntToStr(MF_VERSION) + '.'),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;

  m_hwndVideo := hwndVideo;
  m_hwndEvent := hwndEvent;
  m_hwndMainForm := hwndMainForm;

  m_hwndThis := AllocateHWnd(WndProc);
  m_bAppIsClosing := False;

  // Get the default system languagetag (1) when no preffered language is known
  SubTitleLanguage := GetUserDefaultLanguageTag(1);

  FMediaTimeline := TMfMediaTimeline.Create();
  FSubtitleCompositor := TMfSubtitleCompositor.Create();
  FSubtitleBitmap := TBitmap.Create();
  FSubtitleAspectRatio := AR_16_9;
  ResetSubtitleBitmapCache();

  Clear();

  // init a handle for the events
  hr := Initialize();

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while initializing MESessionClosed'),
                 LPCWSTR('Error!'),
                 MB_ICONEXCLAMATION);
     end;
end;


// The destructor
destructor TMfPlayerX.Destroy();
begin

  ClearSubtitleBitmap();
  FVideoMixerBitmap := nil;
  FreeAndnil(FSubtitleBitmap);

  // If you don't de-reference all the interfaces before closing everything,
  // you will get an access violation
  m_pSession := nil;
  m_pSource := nil;
  m_pVideoDisplay := nil;
  m_pTopology := nil;
  m_pTimeSource := nil;
  m_pClockStateSink := nil;
  m_pRateControl := nil;
  m_pRateSupport := nil;
  m_pSourcePD := nil;

  FreeAndnil(FSubtitleCompositor);
  FreeAndnil(FMediaTimeline);

  DeAllocateHWnd(m_hwndThis);
  // Shutdown the Media Foundation platform
  MFShutdown();

  inherited Destroy();
end;


procedure TMfPlayerX.BeforeDestruction();
begin

  // This also makes a direct MfPlayerX.Free safe when the caller forgot
  // to call ShutDown explicitly.
  ShutDown();
  // Release the player-owned helper objects, including:
  //   MFCallBack
  //   m_pClockStateSink
  //   MFPresentationClock
  Clear();

  inherited BeforeDestruction();
end;


// Before the application exits, shut down the Media Session,
// and then call MFShutdown to shut down the Microsoft Media Foundation platform.

// Release all resources held by this object.
// The application must call Shutdown because the media session holds a
// reference count on the Player object. (This happens when Player calls
// IMediaEventGenerator.BeginGetEvent on the media session.) As a result,
// there is a circular reference count between the Player object and the
// media session. Calling Shutdown breaks the circular reference count.

// If CreateInstance failed, the application will not call Shutdown. To
// handle that case, we must call Shutdown() in the destructor. The
// circular ref-count problem does not occcur if CreateInstance has failed.
// Also, calling Shutdown twice is harmless.
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
function TMfPlayerX.ShutDown(): HRESULT;
var
  hr: HRESULT;
  CloseError: DWORD;

begin

  m_bAppIsClosing := True;

  hr := CloseSession();

  if (FhCloseEvent <> 0) then
    begin
      if not CloseHandle(THandle(FhCloseEvent)) then
        begin
          CloseError := GetLastError();

          if SUCCEEDED(hr) then
            hr := HRESULT_FROM_WIN32(CloseError);
       end;

    FhCloseEvent := 0;
  end;

  Result := hr;
end;


function TMfPlayerX.CreateSession(pPD: IMFPresentationDescriptor): HRESULT;
var
  hr: HRESULT;
  hrProtected: HRESULT;
  pEnablerActivate: IMFActivate;

label
  done;

begin

  pEnablerActivate := nil;

  // Close an existing session only. The caller may already have created the
  // new media source and presentation descriptor, which must not be released
  // while selecting the correct session type.
  if Assigned(m_pSession) then
    begin
      hr := CloseSession();
      if FAILED(hr) then
        goto Done;
    end;

  assert(State = Closed);

  if not Assigned(pPD) then
    begin
      hr := E_POINTER;
      goto Done;
    end;

  // S_OK means that at least one stream requires the Protected Media Path.
  // S_FALSE means that an ordinary in-process Media Session is sufficient.
  hrProtected := MFRequireProtectedEnvironment(pPD);

  if (hrProtected = S_OK) then
    begin

      OutputDebugString(PChar('MfPlayer X2: protected presentation detected; creating PMP Media Session'));

      // A PMP proxy is still exposed as IMFMediaSession, so the remainder of
      // the player can use the same session interface. No content-protection
      // manager is supplied here; genuinely encrypted content that requires
      // licence acquisition can still report a later policy/licence error.
      hr := MFCreatePMPMediaSession(0,
                                    nil,
                                    m_pSession,
                                    pEnablerActivate);

      if FAILED(hr) and Assigned(pEnablerActivate) then
        OutputDebugString(PChar(
          'MfPlayer X2: PMP creation returned a content-enabler activation object'));
    end
  else
    if (hrProtected = S_FALSE) then
      begin
        hr := MFCreateMediaSession(nil,
                                   m_pSession);
      end
    else
      begin
        // Preserve an actual failure from MFRequireProtectedEnvironment.
        hr := hrProtected;
      end;

  if FAILED(hr) then
    goto Done;

  mfpControl.State := Ready;

  // Start pulling events from the selected Media Session. For a PMP session,
  // m_pSession is a proxy whose event contract is the same as a normal session.
  hr := m_pSession.BeginGetEvent(IMFAsyncCallback(Self),
                                 nil);

done:
  pEnablerActivate := nil;
  Result := hr;
end;

// Take a snapshot
function TMfPlayerX.TakeSnapShot(var bit: TBitMap): HRESULT;
var
  buffer, data: PByte;
  bufSize: DWORD;
  i: Integer;
  bmi: BITMAPINFOHEADER;
  timestamp: MFTIME;
  position: MFTIME;
  rowBytes: Integer;
  compBuffer: Pointer;
  compStride: Integer;
  hr: HRESULT;

begin

  // Use assertions in debug mode only!
  {$IFDEF DEBUG}
  Assert(bit <> nil);
  {$ENDIF}

  // Set the biSize member of the structure to sizeof(BITMAPINFOHEADER)
  ZeroMemory(@bmi,
             SizeOf(BITMAPINFOHEADER));

  bmi.biSize := SizeOf(BITMAPINFOHEADER);

  data := nil;
  bufsize := $0000;
  hr := E_FAIL;

  if Assigned(m_pVideoDisplay) then
    begin
      hr := m_pVideoDisplay.GetCurrentImage(Bmi,
                                            buffer,
                                            bufSize,
                                            timestamp);
      if FAILED(hr) then
        Exit(E_FAIL);

      data := buffer;
    end;

try

  if (bmi.biSizeImage > 0) and (data <> nil) then
    begin
      if (bmi.biBitCount = 32) and
         FSubtitlesEnabled and
         Assigned(FSubtitleCompositor) and
         FSubtitleCompositor.TimedTextFileLoaded then
        begin

          rowBytes := Abs(bmi.biWidth) * 4;
          if (bmi.biHeight < 0) then
            begin
              compBuffer := buffer;
              compStride := rowBytes;
            end
          else
            begin
              compBuffer := Pointer(NativeInt(buffer) + (NativeInt(Abs(bmi.biHeight) - 1) * NativeInt(rowBytes)));
              compStride := -rowBytes;
            end;

          if SUCCEEDED(GetPosition(position)) then
            {void} FSubtitleCompositor.CompositeRgb32(compBuffer,
                                                       bufSize,
                                                       Abs(bmi.biWidth),
                                                       Abs(bmi.biHeight),
                                                       compStride,
                                                       HnsTimeToMsec(position));
        end;

      // Adjustments
      Bit.PixelFormat := pf32bit;
      Bit.SetSize(abs(bmi.biWidth), abs(bmi.biHeight));
      for i := abs(bmi.biHeight) - 1 downto 0 do // (int y = h - 1; y >= 0; --y)
        begin
          CopyMemory(Bit.ScanLine[i],
                     data,
                     bmi.biWidth * bmi.biBitCount div 8);
          Inc(data,
              bmi.biWidth * bmi.biBitCount div 8);
        end;

      hr := S_OK;
    end;

finally
  Result := hr;
end;
end;


function TMfPlayerX.GetState(): TPlayerState;
begin

  Result:= mfpControl.State;
end;


procedure TMfPlayerX.SetState(value: TPlayerState);
begin

  mfpControl.State:= Value;
end;


procedure TMfPlayerX.SetRequest(tcRequest: TRequest);
begin

  mfpControl.Request := tcRequest;
  // Possible values:
  // reqNone
  // reqStop
  // reqStart           // start after Pause or stopped
  // reqPause
  // reqSeek
  // reqClose
  // reqRate
  // reqCaptureStart
  // reqCaptureStop
end;


function TMfPlayerX.GetRequest(): TRequest;
begin

  Result:= mfpControl.Request;
end;


function TMfPlayerX.GetVolume(): HRESULT;
var
  pVol: IMFAudioStreamVolume;
  uiChan: UINT32;
  hr: HRESULT;
  VolumeChannels: TFloatArray;

begin

  hr := S_OK;
  pVol := nil;
  uiChan := 0;

try
  if not Assigned(m_pSession) then
    begin
      hr := E_POINTER;
      Exit(hr);
    end;

  // A video-only topology has no audio-renderer volume service. That is a
  // valid media layout, so leave the channel array empty when the service is
  // unavailable. The caller can continue starting the Media Session.
  hr := (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                 IID_IMFAudioStreamVolume,
                                                 Pointer(pVol));
  if FAILED(hr) then
    begin

      hr := S_OK;
      Exit(hr);
    end;

  // Get the number of sound channels from the stream.
  hr := pVol.GetChannelCount(uiChan);
  if FAILED(hr) then
    Exit(hr);

  // Do not take the address of element zero when the renderer reports no
  // channels. This can occur while a topology is still settling.
  if uiChan = 0 then
    begin
      hr := S_OK;
      Exit(hr);
    end;

  // Volume levels are in the range 0.0 to 1.0.
  // If balanced volume is needed; use an array of channels.
  SetLength(VolumeChannels,
            uiChan);

  // The GetAllVolumes method retrieves the volume levels for all channels.
  hr := pVol.GetAllVolumes(uiChan,
                           @VolumeChannels[0]);
  if SUCCEEDED(hr) then
    begin
      m_VolumeChannels := VolumeChannels;
      nChannels := uiChan;
    end;

finally
  Result := hr;
end;
end;


function TMfPlayerX.GetParameters(out pdwFlags: DWord;
                                  out pdwQueue: DWord): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfPlayerX.Initialize(): HRESULT;
begin

  if (FhCloseEvent <> 0) then
    begin
      Result := MF_E_ALREADY_INITIALIZED;
      Exit;
    end;

  FhCloseEvent := CreateEvent(nil,
                              True,
                              False,
                              nil);

  if (FhCloseEvent = 0) then
    Result := GetLastError()
  else
    Result := S_OK;
end;


function TMfPlayerX.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  pEvent: IMFMediaEvent;
  meType: MediaEventType;
  pvar: PROPVARIANT;
  hr,
  hrStatus: HRESULT;

label
  done;

begin

  if not Assigned(m_pSession) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Get the event from the event queue.
  hr := m_pSession.EndGetEvent(pAsyncResult,
                               pEvent);
  if (FAILED(hr)) then
    goto done;

  // Get the event type.
  hr := pEvent.GetType(meType);
  if (FAILED(hr)) then
    goto done;

  // If the session is closed, the application is waiting on the event
  // handle. Also, do not request any more events from the session.
  if (meType = MESessionClosed) then
    SetEvent(THandle(FhCloseEvent))
  else
    begin
      // For all other events, ask the media session for the
      // next event in the queue.
      hr := m_pSession.BeginGetEvent(IMFAsyncCallback(Self),
                                     nil);
      if (FAILED(hr)) then
        goto done;
    end;


  // For most events, post the event as a private window message to the
  // application. This lets the application process the event on its main
  // thread.
  // However, if a call to IMFMediaSession.Close is pending, it means the
  // application is waiting on the m_hCloseEvent event handle. (Blocking
  // call.) In that case, we simply discard the event.
  // When IMFMediaSession.Close is called, MESessionClosed is NOT
  // necessarily the next event that we will receive. We may receive any
  // number of other events before receiving MESessionClosed.


  // Get the event status. If the operation that triggered the event
  // did not succeed, the status is a failure code.
  hr := pEvent.GetStatus(hrStatus);
  // Check if the async operation succeeded.
  if (SUCCEEDED(hr) and FAILED(hrStatus)) then
    begin
      OutputDebugString(PChar(Format('MfPlayer X2: media event %d failed, HRESULT=0x%.8x',
                                     [Ord(meType), DWORD(hrStatus)])));
      hr := hrStatus;
    end;
  if (FAILED(hr)) then
    goto done;

  // ALL possible session events are listed here in case statements.
  case meType of
    MEUnknown: begin {hr := OnUnknown(pEvent);} end;
    MEError: begin {hr := OnError(pEvent);} end;
    MEExtendedType: begin {OnExtendedType(pEvent);} end;
    { MEGenericV1Anchor }
    MENonFatalError : begin {hr := OnNonFatalError(pEvent);} end;
    MESessionUnknown: begin {hr := OnSessionUnknown(pEvent);} end;
    MESessionTopologySet: begin {hr := OnSessionTopologySet(pEvent);} end;
    MESessionTopologiesCleared: begin {hr := OnSessionTopologiesCleared(pEvent);} end;
    MESessionStarted: begin hr := OnSessionStarted(pEvent); end;
    MESessionPaused: begin hr := OnSessionPaused(pEvent); end;
    MESessionStopped: begin hr := OnSessionStopped(pEvent); end;
    MESessionClosed: begin hr := OnSessionClosed(pEvent); end;
    MESessionEnded: begin hr := OnSessionEnded(pEvent); end; // The complete pipeline, including queued renderer samples, has finished.
    MESessionRateChanged: begin
                            // If the rate change succeeded, we've already got the rate
                            // cached. If it fails, try to get the actual rate.
                            if FAILED(hrStatus) then
                              begin

                                PropVariantInit(pvar);
                                hr := pEvent.GetValue(pvar);
                                if (SUCCEEDED(hr) and (pvar.vt = VT_R4) ) then
                                  mfpControl.fCurrentRate:= pvar.fltVal;
                                PropVariantClear(pvar);
                              end;
                            // Fire the event
                            hr := OnSessionRateChanged(pEvent);
                          end;
    MESessionScrubSampleComplete: begin {hr := OnSessionScrubSampleComplete(pEvent);} end;
    MESessionCapabilitiesChanged: begin {hr := OnSessionCapabilitiesChanged(pEvent);} end;
    MESessionTopologyStatus: begin hr := OnSessionTopologyStatus(pEvent); end;
    MESessionNotifyPresentationTime: begin hr := OnSessionNotifyPresentationTime(pEvent); end;
    MENewPresentation: begin hr := OnNewPresentation(pEvent); end;

    MELicenseAcquisitionStart: begin {hr := OnLicenseAcquisitionStart(pEvent);} end;
    MELicenseAcquisitionCompleted: begin {hr := OnLicenseAcquisitionCompleted(pEvent);} end;
    MEIndividualizationStart: begin {hr := OnIndividualizationStart(pEvent);} end;
    MEIndividualizationCompleted: begin {hr := OnIndividualizationCompleted(pEvent);} end;
    MEEnablerProgress: begin {hr := OnEnablerProgress(pEvent);} end;
    MEEnablerCompleted: begin {hr := OnEnablerCompleted(pEvent);} end;
    MEPolicyError: begin {hr := OnPolicyError(pEvent);} end;
    MEPolicyReport: begin {hr := OnPolicyReport(pEvent);} end;
    MEBufferingStarted: begin {hr := OnBufferingStarted(pEvent);} end;
    MEBufferingStopped: begin {hr := OnBufferingStopped(pEvent);} end;
    MEConnectStart: begin {hr := OnConnectStart(pEvent);} end;
    MEConnectEnd: begin {hr := OnConnectEnd(pEvent);} end;
    MEReconnectStart: begin {hr := OnReconnectStart(pEvent);} end;
    MEReconnectEnd: begin {hr := OnReconnectStart(pEvent);} end;
    MERendererEvent: begin {hr := OnRendererEvent(pEvent);} end;

    { MESessionV1Anchor }
    MESessionStreamSinkFormatChanged: begin {hr := OnSessionStreamSinkFormatChanged(pEvent);} end;
    MESourceUnknown: begin {hr := OnSourceUnknown(pEvent);} end;
    MESourceStarted: begin {hr := OnSourceStarted(pEvent);} end;
    MEStreamStarted: begin {hr := OnStreamStarted(pEvent);} end;
    MESourceSeeked: begin {hr := OnSourceSeeked(pEvent);} end;
    MEStreamSeeked: begin {hr := OnStreamSeeked(pEvent);} end;
    MENewStream: begin {hr := OnNewStream(pEvent);} end;
    MEUpdatedStream: begin {hr := OnUpdatedStream(pEvent);} end;
    MESourceStopped: begin {hr := OnSourceStopped(pEvent);} end;
    MEStreamStopped: begin {hr := OnStreamStopped(pEvent);} end;
    MESourcePaused: begin {hr := OnSourcePaused(pEvent);} end;
    MEStreamPaused: begin {hr := OnStreamPaused(pEvent);} end;
    MEEndOfPresentation: begin hr := OnEndOfPresentation(pEvent); end;
    MEEndOfStream: begin {hr := OnEndOfStream(pEvent);} end;
    MEMediaSample: begin {hr := OnMediaSample(pEvent);} end;
    MEStreamTick: begin {hr := OnStreamTick(pEvent);} end;
    MEStreamThinMode: begin {hr := OnStreamThinMode(pEvent);} end;
    MEStreamFormatChanged: begin {hr := OnStreamFormatChanged(pEvent);} end;
    MESourceRateChanged: begin {hr := OnSourceRateChanged(pEvent);} end;
    MEEndOfPresentationSegment: begin {hr := OnEndOfPresentationSegment(pEvent);} end;
    MESourceCharacteristicsChanged: begin {hr := OnSourceCharacteristicsChanged(pEvent);} end;
    MESourceRateChangeRequested: begin {hr := OnSourceRateChangeRequested(pEvent);} end;
    MESourceMetadataChanged: begin {hr := OnSourceMetadataChanged(pEvent);} end;

    { MESourceV1Anchor }
    MESequencerSourceTopologyUpdated: begin {hr := OnSequencerSourceTopologyUpdated(pEvent);} end;
    MESinkUnknown: begin {hr := OnSinkUnknown(pEvent);} end;
    MEStreamSinkStarted: begin {hr := OnStreamSinkStarted(pEvent);} end;
    MEStreamSinkStopped: begin {hr := OnStreamSinkStopped(pEvent);} end;
    MEStreamSinkPaused: begin {hr := OnStreamSinkPaused(pEvent);} end;
    MEStreamSinkRateChanged: begin {hr := OnStreamSinkRateChanged(pEvent);} end;
    MEStreamSinkRequestSample: begin {hr := OnStreamSinkRequestSample(pEvent);} end;
    MEStreamSinkMarker: begin {hr := OnStreamSinkMarker(pEvent);} end;
    MEStreamSinkPrerolled: begin {hr := OnStreamSinkPrerolled(pEvent);} end;
    MEStreamSinkScrubSampleComplete: begin {hr := OnStreamSinkScrubSampleComplete(pEvent);} end;
    MEStreamSinkFormatChanged: begin {hr := OnStreamSinkFormatChanged(pEvent);} end;
    MEStreamSinkDeviceChanged: begin {hr := OnStreamSinkDeviceChanged(pEvent);} end;
    MEQualityNotify: begin {hr := OnQualityNotify(pEvent);} end;
    MESinkInvalidated: begin {hr := OnSinkInvalidated(pEvent);} end;
    MEAudioSessionNameChanged: begin {hr := OnAudioSessionNameChanged(pEvent);} end;
    MEAudioSessionVolumeChanged: begin {hr := OnAudioSessionVolumeChanged(pEvent);} end;
    MEAudioSessionDeviceRemoved: begin {hr := OnAudioSessionDeviceRemoved(pEvent);} end;
    MEAudioSessionServerShutdown: begin {hr := OnAudioSessionServerShutdown(pEvent);} end;
    MEAudioSessionGroupingParamChanged: begin {hr := OnAudioSessionGroupingParamChanged(pEvent);} end;
    MEAudioSessionIconChanged: begin {hr := OnAudioSessionIconChanged(pEvent);} end;
    MEAudioSessionFormatChanged: begin {hr := OnAudioSessionFormatChanged(pEvent);} end;
    MEAudioSessionDisconnected: begin {hr := OnAudioSessionDisconnected(pEvent);} end;

    { MESinkV1Anchor }
    MEAudioSessionExclusiveModeOverride: begin {hr := OnAudioSessionExclusiveModeOverride(pEvent);} end;
    MECaptureAudioSessionVolumeChanged: begin {hr := OnCaptureAudioSessionVolumeChanged(pEvent);} end;
    MECaptureAudioSessionDeviceRemoved: begin {hr := OnCaptureAudioSessionDeviceRemoved(pEvent);} end;
    MECaptureAudioSessionFormatChanged: begin {hr := OnCaptureAudioSessionFormatChanged(pEvent);} end;
    MECaptureAudioSessionDisconnected: begin {hr := OnCaptureAudioSessionDisconnected(pEvent);} end;
    MECaptureAudioSessionExclusiveModeOverride: begin {OnCaptureAudioSessionExclusiveModeOverride(pEvent);} end;

    { MESinkV2Anchor }
    MECaptureAudioSessionServerShutdown: begin {hr := OnCaptureAudioSessionServerShutdown(pEvent);} end;
    METrustUnknown: begin {hr := OnTrustUnknown(pEvent);} end;
    MEPolicyChanged: begin {hr := OnPolicyChanged(pEvent);} end;
    MEContentProtectionMessage: begin {hr := OnContentProtectionMessage(pEvent);} end;

    { METrustV1Anchor }
    MEPolicySet: begin {OnPolicySet} end;
    MEWMDRMLicenseBackupCompleted: begin {hr := OnWMDRMLicenseBackupCompleted(pEvent);} end;
    MEWMDRMLicenseBackupProgress: begin {hr := OnWMDRMLicenseBackupProgress(pEvent);} end;
    MEWMDRMLicenseRestoreCompleted: begin {hr := OnWMDRMLicenseRestoreCompleted(pEvent);} end;
    MEWMDRMLicenseRestoreProgress: begin {hr := OnWMDRMLicenseRestoreProgress(pEvent);} end;
    MEWMDRMLicenseAcquisitionCompleted: begin {hr := OnWMDRMLicenseAcquisitionCompleted(pEvent);} end;
    MEWMDRMIndividualizationCompleted: begin {hr := OnWMDRMIndividualizationCompleted(pEvent);} end;
    MEWMDRMIndividualizationProgress: begin {hr := OnWMDRMIndividualizationProgress(pEvent);} end;
    MEWMDRMProximityCompleted: begin {hr := OnWMDRMProximityCompleted(pEvent);} end;
    MEWMDRMLicenseStoreCleaned: begin {hr := OnWMDRMLicenseStoreCleaned(pEvent);} end;

    { MEWMDRMV1Anchor }
    MEWMDRMRevocationDownloadCompleted: begin {hr := OnWMDRMRevocationDownloadCompleted(pEvent);} end;
    METransformUnknown: begin {hr := OnTransformUnknown(pEvent);} end;
    METransformNeedInput: begin {hr := OnTransformNeedInput(pEvent);} end;
    METransformHaveOutput: begin {hr := OnTransformHaveOutput(pEvent);} end;
    METransformDrainComplete: begin {hr := OnTransformDrainComplete(pEvent);} end;
    METransformMarker: begin {hr := OnTransformMarker(pEvent);} end;
    METransformInputStreamStateChanged: begin {hr := OnTransformInputStreamStateChanged(pEvent);} end;
    MEByteStreamCharacteristicsChanged: begin {hr := OnByteStreamCharacteristicsChanged(pEvent);} end;
    MEVideoCaptureDeviceRemoved: begin {hr := OnVideoCaptureDeviceRemoved(pEvent);} end;
    MEVideoCaptureDevicePreempted: begin {hr := OnVideoCaptureDevicePreempted(pEvent);} end;
    MEStreamSinkFormatInvalidated: begin {hr := OnStreamSinkFormatInvalidated(pEvent);} end;
    MEEncodingParameters: begin {hr := OnEncodingParameters(pEvent);} end;
    MEContentProtectionMetadata: begin {hr := OnContentProtectionMetadata(pEvent);} end;
    MEDeviceThermalStateChanged: begin {hr := OnDeviceThermalStateChanged(pEvent);} end;
    MEReservedMax: begin {hr := OnReservedMax(pEvent);} end;
  else
    begin

      {hr := OnunknownEvent(pEvent);}
    end;
  end;
done:
  Result:= hr;
end;



function TMfPlayerX.HasVideo(): boolean;
begin

  Result := (m_pVideoDisplay <> nil);
end;


function TMfPlayerX.OnSessionTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
var
  status: MF_TOPOSTATUS;
  hr: HRESULT;

begin

  if m_bAppIsClosing then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                         UINT32(status));
  if SUCCEEDED(hr) then
    if (status = MF_TOPOSTATUS_READY) then
      begin
        // Call OnTopologyReady
        hr := OnSessionTopologyReady(pEvent);  // Send msg we are ready
      end;
  Result := hr;
end;


// Handler for MESessionTopologyReady event - starts video playback.
function TMfPlayerX.OnSessionTopologyReady(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;
  hrClock: HRESULT;
  hrRate: HRESULT;
  hrCaps: HRESULT;

begin

  if m_bAppIsClosing then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  // release any previous instance of the m_pVideoDisplay interface
  SafeRelease(m_pVideoDisplay);

  // Ask the session for the IMFVideoDisplayControl interface. This interface is
  // implemented by the EVR (Enhanced Video Renderer) and is exposed by the media
  // session as a service. The session will query the topology for the right
  // component and return this EVR interface. The interface will be used to tell the
  // video to repaint whenever the hosting window receives a WM_PAINT window message.
  // This call is expected to fail if the media file does not have a video stream.
  hr := MFGetService(m_pSession,
                     MR_VIDEO_RENDER_SERVICE,
                     IID_IMFVideoDisplayControl,
                     Pointer(m_pVideoDisplay));

  if FAILED(hr) then
    begin
      m_pVideoDisplay := nil;
      hr := S_OK;
    end;

  // Set the target window (or control), at this point this is not really
  // nescesarry, because the previous -MfGetService- did that allready.
  //{void} m_pVideoDisplay.SetVideoWindow(m_hwndVideo);

  // Adjust aspect ratio
  if Assigned(m_pVideoDisplay) then
    ResizeVideo(nil);

// Since the topology is ready, you might start playback, do rate calculations etc.
//
////////////////////////////////////////////////////////////////////////////////

  // Safe guard.
  if not m_bAppIsClosing then
    begin

      // These are auxiliary services. Failure to obtain the application
      // clock callback or rate-control service must not prevent playback.
      // The Media Session itself owns and starts the presentation clock.
      hrClock := EnsurePresentationClock();

      if FAILED(hrClock) then
        OutputDebugString(PChar(Format('Presentation clock not ready, hr=%.8x',
                                       [DWORD(hrClock)])));
    end;

  hrRate := InitiateRateControl();
  if FAILED(hrRate) then
    begin
      mfpControl.SeekState.bCanScrub := False;
      mfpControl.SeekState.bCanThinPb := False;
    end;

  // Obtain capabilities of the current session.
  hrCaps := m_pSession.GetSessionCapabilities(m_dCaps);
  if FAILED(hrCaps) then
    m_dCaps := MFSESSIONCAP_START;

  // You can also implement an option for the user, starting playback directly after
  // a mediastream is loaded.
  //hr:= Play();

  State := TopologyReady;
  m_bPending := False;

  if (mfpControl.Request = reqStart) then
    hr := Start();

////////////////////////////////////////////////////////////////////////////////

  Result := hr;
end;


// Handler for MENewPresentation event.
// This event is sent if the media source has a new presentation, which
// requires a new topology.
function TMfPlayerX.OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;

label
  done;

begin

  SafeRelease(m_pSourcePD);
  SafeRelease(m_pTopology);

  // Get the presentation descriptor from the event.
  hr := GetEventObject(pEvent,
                       m_pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // Create a partial playback topology.
  hr := CreatePlaybackTopologyX2(m_pSource,
                                 m_pSourcePD,
                                 m_hwndVideo,
                                 m_pTopology,
                                 mfpControl.SourceStreams);

  if (FAILED(hr)) then
      goto done;

  // Set the topology on the media session.
  hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                               m_pTopology);

  State := OpenPending;

done:
  Result := hr;  //S_OK
end;


function TMfPlayerX.OnSessionPaused(pEvent: IMFMediaEvent): HRESULT;
begin

  m_bPending := False;
  mfpControl.Request := reqNone;

  if Assigned(FMediaTimeline) then
    FMediaTimeline.Pause();

  State := Paused;
  UpdateCaption();
  Result := S_OK;
end;


function TMfPlayerX.OnSessionClosed(pEvent: IMFMediaEvent): HRESULT;
begin

  State  := Closed;
  SetEvent(THandle(FhCloseEvent));
  Result := S_OK;
end;


function TMfPlayerX.OnSessionRateChanged(pEvent: IMFMediaEvent): HRESULT;
begin

  // See function HandleEvent for details
  Result := S_OK;
end;


// Raised by the Media Session when a new presentation starts.
// This event indicates when the presentation will start and
// the offset between the presentation time and the source time.
function TMfPlayerX.OnSessionNotifyPresentationTime(pEvent: IMFMediaEvent): HRESULT;
var
  eventVal: PROPVARIANT;
  hr: HResult;

begin

  hr := S_OK;

try
  PropVariantInit(eventVal);

  hr := pEvent.GetValue(eventVal);
  if SUCCEEDED(hr) then
    if (eventVal.vt = VT_I8) then
      mfpControl.StartPosition := eventVal.hVal.QuadPart;
finally
  PropVariantClear(eventVal);
  Result := hr;
end;
end;


function TMfPlayerX.OnSessionStarted(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HResult;
  queuedRequest: TRequest;

begin

  queuedRequest := mfpControl.Request;
  m_bPending := False;
  mfpControl.Request := reqNone;
  State := Started;
  // Note: Start is an asynchronous operation. However, we
  // can treat our state as being already started. If Start
  // fails later, we'll get an MESessionStarted event with
  // an error code, and we will update our state then.

  // Start the presentation clock
  // 1 sec = 1,000,000,000 (10^9) nanoseconds.
  // Clock times are always in 100-nanosecond (hns) units,
  // so one second is 10.000.000 (10^7) hns-units, one millisecond is 10.000 hns-units.
  // This corresponds to a frequency of 10 MHz.
  // To get the latest clock time from the presentation clock,
  // call IMFPresentationClock.GetTime.

  if (queuedRequest = reqPause) then
    begin

      Result := Pause();
      Exit;
    end;

  // The timer updates the UI timeline, but is not part of the rendering
  // pipeline. For video-only playback, acquire it again after Start because
  // the session may have selected its system time source only at that point.
  if not Assigned(MFCallBack) then
    begin
      hr := EnsurePresentationClock();

      if FAILED(hr) then
        OutputDebugString(PChar(Format('EnsurePresentationClock failed, hr=%.8x',
                                       [DWORD(hr)])));
    end;

  if Assigned(MFCallBack) and (State = Started) then
    begin
      // Set the recurring interval before arming the immediate first tick.
      // The previous order allowed Invoke to run with an uninitialized zero
      // interval and flood the callback queue.
      MFCallBack.TimerResolution := MF_PLAYER_UI_TIMER_INTERVAL_HNS;
      hr := MFCallBack.SetTimer(0,
                                DWord(MFTIMER_RELATIVE),
                                nil); // Fire the first UI update immediately
      if SUCCEEDED(hr) then
        OutputDebugString(PChar('MfPlayer X2: 100 ms presentation timer started'))
      else
        OutputDebugString(PChar(Format('MfPlayer X2: timer unavailable, hr=%.8x',
                                       [DWORD(hr)])));
    end;

  // A missing UI timer must never turn a successful MESessionStarted event
  // into a playback failure.
  Result := S_OK;
end;


function TMfPlayerX.OnSessionStopped(pEvent: IMFMediaEvent): HRESULT;
begin

  // MESessionStopped is the asynchronous completion event for an explicit
  // IMFMediaSession.Stop call. Do not use it as the normal end-of-file path.
  OutputDebugString(PChar('MfPlayer X2: explicit Stop completed'));
  m_bPending := False;
  mfpControl.Request := reqNone;

  if Assigned(FMediaTimeline) then
    FMediaTimeline.Stop();

  ClearSubtitleBitmap();
  State := Stopped;
  UpdateCaption();
  Result := S_OK;
end;


function TMfPlayerX.OnSessionEnded(pEvent: IMFMediaEvent): HRESULT;
begin

  // The source may announce MEEndOfPresentation while decoded and scheduled
  // samples are still travelling through the topology. MESessionEnded is the
  // point where the last presentation has actually drained from the pipeline.
  OutputDebugString(PChar('MfPlayer X2: session ended after pipeline drain'));
  m_bPending := False;
  mfpControl.Request := reqNone;

  if Assigned(FMediaTimeline) then
    FMediaTimeline.Stop();

  ClearSubtitleBitmap();
  State := Stopped;
  UpdateCaption();

  // Complete the form-level playback lifecycle on the UI thread. This also
  // disconnects an active Chromecast session and stops its transcode worker.
  if m_hwndMainForm <> 0 then
    PostMessage(m_hwndMainForm,
                WM_PROGRESSNOTIFY,
                WPARAM(2),
                0);

  Result := S_OK;
end;


function TMfPlayerX.OnEndOfPresentation(pEvent: IMFMediaEvent): HRESULT;
begin

  // Do NOT call IMFMediaSession.Stop here. MEEndOfPresentation is raised by
  // the media source as soon as it has produced its final sample; frames may
  // still be queued in the decoder, subtitle transform and EVR. Calling Stop
  // here flushes those samples. This is especially visible with video-only
  // files whose source can read ahead to EOS almost immediately.

  // Wait for MESessionEnded before changing the public player state.
  OutputDebugString(PChar('MfPlayer X2: source reached end of presentation; waiting for pipeline drain'));
  Result := S_OK;
end;


// The main method to open and prepare a media file.
function TMfPlayerX.OpenURL(sURL: PWideChar): HRESULT;
var
  hr: HRESULT;
  hrOptional: HRESULT;
  hrTimedText: HRESULT;

  StreamCount: DWORD;
  StreamIndex: DWORD;
  StreamSelected: BOOL;
  StreamDescriptor: IMFStreamDescriptor;
  DecoderCLSID: CLSID;

  CodecDescription: string;
  ErrorCaption: string;

label
  done;

begin

  // 1. Create the media source and presentation descriptor.
  // 2. Do decoder checks
  // 3. Select and create a normal or PMP Media Session.
  // 4. Create the topology.
  // 5. Queue the topology [asynchronous].
  // 6. Read stream information.
  // 7. Obtain the presentation clock after topology resolution.
  // 8. Ready to start playback.

  hr := S_OK;
  ErrorCaption := '';

  StreamCount := 0;
  StreamSelected := False;
  StreamDescriptor := nil;
  DecoderCLSID := GUID_NULL;

  // Release objects belonging to a previous file.
  Clear();

  try
    try
      if not Assigned(sURL) then
        begin
          hr := E_POINTER;
          ErrorCaption := 'MfPlayer X2: no media file was specified.';
          goto done;
        end;

      if (sURL^ = #0) then
        begin
          hr := E_INVALIDARG;
          ErrorCaption := 'MfPlayer X2: no media file was specified.';
          goto done;
        end;

      FFilename := sURL;

      // Create the media source.
      hr := CreateObjectFromUrl(sURL,
                                m_pSource);

      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not open %s (%s)',
                                 [ExtractFileName(FFilename), IntToHex(DWORD(hr), 8)]);
          goto done;
      end;

      // Create the presentation descriptor.
      //
      // The presentation descriptor is required for:
      //   - selecting the correct Media Session;
      //   - selecting source streams;
      //   - inspecting the stream codecs.
      hr := m_pSource.CreatePresentationDescriptor(m_pSourcePD);

      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not examine the streams in %s (%s)',
                                 [ExtractFileName(FFilename), IntToHex(DWORD(hr), 8)]);
          goto done;
        end;

      // Create either a normal Media Session or a PMP Media Session.
      //
      // CreateSession calls MFRequireProtectedEnvironment internally.
      hr := CreateSession(m_pSourcePD);
      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not create the Media Foundation session. (%s)',
                                 [IntToHex(DWORD(hr), 8)]);
          goto done;
      end;

      // Locate and load a sidecar timed-text file before constructing
      // the topology. The subtitle transform is inserted only when
      // timed text is actually available.
      hrTimedText := ReloadTimedText();

      if FAILED(hrTimedText) then
        begin
          FSubtitlesEnabled := False;

          if Assigned(FSubtitleCompositor) then
            FSubtitleCompositor.Close();
        end;

      // Create the partial playback topology.
      //
      // CreatePlaybackTopologyX2 also corrects the stream selection:
      //   - selects a default video stream when necessary;
      //   - selects a default audio stream when necessary;
      //   - deselects unsupported metadata and source subtitle streams.
      //
      // Decoder validation must therefore take place after this call.
      hr := CreatePlaybackTopologyX2(m_pSource,
                                     m_pSourcePD,
                                     m_hwndVideo,
                                     m_pTopology,
                                     mfpControl.SourceStreams);
      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not create the playback topology. (%s)',
                                 [IntToHex(DWORD(hr), 8)]);
          goto done;
        end;

      // Get the number of streams from the corrected presentation
      // descriptor.
      hr := m_pSourcePD.GetStreamDescriptorCount(StreamCount);

      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not enumerate the media streams. (%s)',
                                 [IntToHex(DWORD(hr), 8)]);
          goto done;
        end;

      if (StreamCount = 0) then
        begin
          hr := MF_E_UNSUPPORTED_BYTESTREAM_TYPE;
          ErrorCaption := 'The file contains no playable media streams.';
          goto done;
        end;

      // Validate the decoder for each selected compressed stream.
      //
      // FindDecoderForStream returns:
      //
      //   S_OK
      //     A decoder exists, or the stream is uncompressed.
      //
      //   MF_E_TOPO_CODEC_NOT_FOUND
      //     The stream is compressed, but no matching Media Foundation
      //     decoder is installed.
      for StreamIndex := 0 to StreamCount - 1 do
        begin
          StreamDescriptor := nil;
          StreamSelected := False;
          DecoderCLSID := GUID_NULL;

          hr := m_pSourcePD.GetStreamDescriptorByIndex(StreamIndex,
                                                       StreamSelected,
                                                       StreamDescriptor);
          if FAILED(hr) then
            begin
              ErrorCaption := Format('Could not examine media stream %s (%s)',
                                     [IntToStr(StreamIndex), IntToHex(DWORD(hr), 8)]);
              goto done;
           end;

        // Ignore streams that are not part of the playback topology.
        if not StreamSelected then
          Continue;

        hr := FindDecoderForStream(StreamDescriptor,
                                   DecoderCLSID);

        if FAILED(hr) then
          begin


          CodecDescription := Trim(GetStreamCodecDescription(StreamDescriptor));

          if (CodecDescription = '') then
            CodecDescription := 'Unknown compressed media format';

          if (hr = MF_E_TOPO_CODEC_NOT_FOUND) then
            ErrorCaption := 'Missing Media Foundation decoder: ' + CodecDescription
          else
            ErrorCaption := Format('Could not validate the decoder for %s (%s)',
                                   [CodecDescription, IntToHex(DWORD(hr), 8)]);

          goto done;
        end;
      end;

      // Obtain the duration. This attribute is optional.
      mfpControl.uiDuration := 0;

      hrOptional := m_pSourcePD.GetUINT64(MF_PD_DURATION,
                                          mfpControl.uiDuration);

      if FAILED(hrOptional) then
        mfpControl.uiDuration := 0;

      // Obtain the file size. This attribute is also optional.
      mfpControl.uiFileSize := 0;

      hrOptional := m_pSourcePD.GetUINT64(MF_PD_TOTAL_FILE_SIZE,
                                          mfpControl.uiFileSize);

      if FAILED(hrOptional) then
        mfpControl.uiFileSize := 0;

      // Obtain the capabilities of the current session.
      //
      // Failure to obtain the capability flags should not prevent
      // playback. Use the basic start capability as fallback.
      m_dCaps := 0;

      hrOptional := m_pSession.GetSessionCapabilities(m_dCaps);

      if FAILED(hrOptional) then
        m_dCaps := MFSESSIONCAP_START;

      // Submit the partial topology to the Media Session.
      //
      // Topology resolution is asynchronous. A decoder can still reject
      // a particular profile, level, resolution or codec configuration
      // later through a Media Session event.
      hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                                   m_pTopology);
      if FAILED(hr) then
        begin
          ErrorCaption := Format('Could not submit the playback topology. (%s)',
                                 [IntToHex(DWORD(hr), 8)]);
          goto done;
        end;

      // Retrieve stream information for the UI.
      //
      // This information is optional and must not overwrite the successful
      // result from SetTopology.
      hrOptional := GetStreamContents(m_pSourcePD,
                                      m_pSource,
                                      m_aStreamCont);
      if FAILED(hrOptional) then
        ErrorCaption := Format('Could not retrieve stream information. (%s)',
                               [IntToHex(DWORD(hrOptional), 8)])
      else
        begin
          // Stream metadata is available as soon as the source is opened,
          // before the audio-renderer volume service is necessarily ready.
          nChannels := 0;
          SetLength(m_VolumeChannels, 0);
          for StreamIndex := Low(m_aStreamCont) to High(m_aStreamCont) do
            if (m_aStreamCont[StreamIndex].idStreamMediaType = mtAudio) and
               (m_aStreamCont[StreamIndex].bSelected <> BOOL(0)) and
               (m_aStreamCont[StreamIndex].audio_iAudioChannels > 0) then
              begin
                nChannels := m_aStreamCont[StreamIndex].audio_iAudioChannels;
                SetLength(m_VolumeChannels, nChannels);
                for StreamCount := 0 to nChannels - 1 do
                  m_VolumeChannels[StreamCount] := mfpControl.Volume;
                Break;
              end;

        if Assigned(FMediaTimeline) then
          begin
            // Configure the frame-rate fallback for decoded samples that do
            // not carry a usable timestamp. Some MKV decoder paths need this.
            for StreamIndex := Low(m_aStreamCont) to High(m_aStreamCont) do
              if (m_aStreamCont[StreamIndex].idStreamMediaType = mtVideo) and
                 (m_aStreamCont[StreamIndex].video_FrameRateNumerator > 0) and
                 (m_aStreamCont[StreamIndex].video_FrameRateDenominator > 0) then
                begin
                  FMediaTimeline.SetFrameRate(
                    m_aStreamCont[StreamIndex].video_FrameRateNumerator,
                    m_aStreamCont[StreamIndex].video_FrameRateDenominator);
                  Break;
                end;
          end;
        end;

      // The topology is now being resolved asynchronously.
      State := OpenPending;
      m_bPending := True;

      hr := S_OK;

done:

      if FAILED(hr) then
        begin
          // Clear releases the partially created topology, source and session.
          // It also clears FFileName, so compose ErrorCaption before this call.
          Clear();

          State := Closed;
          m_bPending := False;

          if (ErrorCaption = '') then
            begin

              ErrorCaption := Format('Could not open the media file. (%s)',
                                     [IntToHex(DWORD(hr), 8)]);
            end;

          // m_hwndEvent is the handle of the player form.
          // Do not call UpdateCaption here because it would replace this
          // detailed message with "Session closed."
          if (m_hwndEvent <> 0) then
          SetWindowText(m_hwndEvent,
                        PChar(ErrorCaption));
        end
      else
        UpdateCaption();

    except
      on E: EOleSysError do
        begin

          hr := E.ErrorCode;
          ErrorCaption := Format('Exception: %s (%s)',
                                 [E.Message, IntToHex(DWORD(hr), 8)]);
          Clear();

          State := Closed;
          m_bPending := False;

          if (m_hwndEvent <> 0) then
            SetWindowText(m_hwndEvent,
                          PChar(ErrorCaption));
        end;

      on E: Exception do
        begin

          hr := E_FAIL;
          ErrorCaption := Format('Exception: %s',
                                 [E.Message]);

        Clear();

        State := Closed;
        m_bPending := False;

        if (m_hwndEvent <> 0) then
          SetWindowText(m_hwndEvent,
                        PChar(ErrorCaption));
      end;
    end;

  finally
    StreamDescriptor := nil;
    Result := hr;
  end;
end;


// Pause
function TMfPlayerX.Pause(): HRESULT;
var
  hr: HRESULT;

begin

  if (m_pSession = nil) or (m_pSource = nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if m_bPending then
    begin
      mfpControl.Request := reqPause;
      State := Pausing;
      hr := S_OK;
    end
  else
    begin
      hr := m_pSession.Pause();
      mfpControl.Request := reqPause;
      State := Pausing;
      m_bPending := True;
    end;

  Result:= hr;
end;


// Start playback from the current position.
//==========================================
function TMfPlayerX.Start(): HRESULT;
var
  hr: HRESULT;
  hrClock: HRESULT;
  varStart: PROPVARIANT;
  tPos: MFTIME;
  csClockState: MF_CLOCK_STATE;
  FVideoProcessor: IMFVideoProcessor;
  hrVideo: HRESULT;

begin

  hr:= S_OK;

  if (m_pSession = nil) then
    begin

      Result := E_POINTER;
      Exit;
    end;

try

  if m_bPending then
    begin
      mfpControl.Request := reqStart;
      State := Starting;
    end
  else
    begin
      PropVariantInit(varStart);

      tPos := 0;
      csClockState := MFCLOCK_STATE_INVALID;

      // The Media Session owns the presentation clock. The application clock
      // interface is useful for position reporting, but it is not a prerequisite
      // for IMFMediaSession.Start. In particular, a video-only session may use
      // the system time source instead of the audio renderer.
      if Assigned(MFPresentationClock) then
        begin
          hrClock := MFPresentationClock.GetState(0,
                                                   csClockState);
          if FAILED(hrClock) then
            csClockState := MFCLOCK_STATE_INVALID;
        end;

      // The Start method can also specify a starting position relative to the start
      // of the file; see the API reference topic for more information.
      if (csClockState = MFCLOCK_STATE_PAUSED) and
         Assigned(MFPresentationClock) then
        begin
          hrClock := MFPresentationClock.GetTime(tPos);
          if SUCCEEDED(hrClock) then
            begin
              varStart.vt := VT_I8;
              varStart.hVal.QuadPart := tPos;
            end
          else
            varStart.vt := VT_EMPTY;
        end
      else
        varStart.vt := VT_EMPTY;

      // Start the session. IMFMediaSession.Start starts its presentation clock.
      hr := m_pSession.Start(GUID_NULL,
                             varStart);

      m_bPending := SUCCEEDED(hr);
      {void} PropVariantClear(varStart);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // Get initial volume
      GetVolume();
      mfpControl.Request := reqStart;
      if Assigned(FMediaTimeline) then
        FMediaTimeline.Start(mfpControl.CurrentPosition);
      State := Started;

      // check if there is video present
      if (HasVideo() = True) then
        begin
          hrVideo := (m_pSession as IMFGetService).GetService(MR_VIDEO_MIXER_SERVICE,
                                                              IID_IMFVideoProcessor,
                                                              Pointer(FVideoProcessor));
          if (SUCCEEDED(hrVideo)) then
            {void} FVideoProcessor.GetBackgroundColor(FBGColor);

          // X2 no longer uses the EVR alpha bitmap overlay for subtitles.
          FVideoMixerBitmap := nil;
        end
    end;

finally
  Result:= hr;
end;
end;



function TMfPlayerX.BurnSubtitlesToFile(const OutputFileName: WideString;
                                        Bitrate: UINT32): HRESULT;
var
  pump: TMfSubtitleFramePump;

begin

  if (FFileName = '') or (OutputFileName = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not FSubtitleCompositor.TimedTextFileLoaded then
    begin
      Result := ReloadTimedText();
      if FAILED(Result) then
        Exit;
    end;

  FSubtitleCompositor.SubtitleAspectRatio := FSubtitleAspectRatio;
  pump := TMfSubtitleFramePump.Create(FSubtitleCompositor);

  try
    Result := pump.BurnSubtitlesToFile(FFileName,
                                       OutputFileName,
                                       Bitrate);
  finally
    FreeAndNil(pump);
  end;
end;

// Repaint the video window.
// Call this method on WM_PAINT from the form where the video is playing on.
function TMfPlayerX.Repaint(): HRESULT;
begin

  if Assigned(m_pVideoDisplay) then
    Result := m_pVideoDisplay.RepaintVideo()
  else
    Result := S_OK;
end;


function TMfPlayerX.ResizeVideo(pdRect: LPRECT = nil): HRESULT;
var
  rcpdest: LPRECT;
  rc: TRect;
  hr: HResult;  //debug purpose

begin

  hr := E_NOINTERFACE;
  rcpdest := nil;

  if Assigned(m_pVideoDisplay) then
    begin
      // Stop repaint
      SetRedraw();
      // Set the destination rectangle.
      // If dRect is empty; use the GetClientRect function
      if (pdRect = nil) then
        begin
          WinApi.Windows.GetClientRect(m_hwndVideo,
                                       rc);
          CopyTRectToLPRect(rc,
                            rcpdest);
        end
      else
        rcpDest := pdRect;

      hr := m_pVideoDisplay.SetVideoPosition(nil,
                                             rcpdest);

      if SUCCEEDED(hr) then
        // Set aspect ratio in conjunction with SetVideoPosition
        hr := m_pVideoDisplay.SetAspectRatioMode(MFVideoARMode_PreservePicture);

      ClearSubtitleBitmap();
      UpdateCaption();

      // Start repaint again
      SetRedraw();
    end;

  rcpdest := nil;
  Result := hr;
end;


procedure TMfPlayerX.SetRedraw();
begin

  //Stop flickering of controls and subtitle when resizing.
  if (stRedrawStatus = rdStarted) then
    begin
      SendMessage(m_hwndMainForm,
                  WM_SETREDRAW,
                  WPARAM(False),
                  0);
      stRedrawStatus := rdStopped;
    end
  else
    begin
      SendMessage(m_hwndMainForm,
                  WM_SETREDRAW,
                  WPARAM(True), 0);

      RedrawWindow(m_hwndMainForm,
                   nil,
                   0,
                   RDW_ERASE OR RDW_FRAME OR RDW_INVALIDATE OR RDW_ALLCHILDREN);

      stRedrawStatus := rdStarted;
    end;
end;


procedure TMfPlayerX.SendPlayerRequest(req: TRequest);
begin

  mfpControl.Request := req;

  case req of
    reqNone:           State:= Closed;
    reqStop:           State:= Stopping;
    reqStart:          State:= Starting;
    reqPause:          State:= Pausing;
    reqSeek:           State:= Seeking;
    reqClose:          State:= Closing;
  end;

  {
  Possible states:
  Ready          // Session was created, ready to open a file.
  OpenPending    // Session is opening a file.
  Starting       // Session initializing Start
  Started        // Session is playing a file.
  Pausing        // Session initializing pause.
  Paused         // Session or mediaengine is paused.
  Stopping       // Session initializing Stop.
  Stopped        // Session is stopped (ready to play).
  Closing        // Shutting down.
  Seeking        // Session seeks.
  SeekingDone,   // Session has ended seeking.
  TopologyIsSet  // Session topology has been set.
  }

  case req of
    reqStart: Start();
    reqPause: Pause();
    reqStop:  Stop();
    reqSeek:  SetPosition(mfpControl.StartPosition);
  else
    UpdatePendingCommands(req);
  end;
end;


// Sets the current playback position.
function TMfPlayerX.SetPosition(hnsPosition: MFTIME): HRESULT;
var
  hr: HRESULT;

begin

  hr := S_OK;

  if (m_bPending) then
    begin
      // Currently seeking or changing rates, so cache this request.
      mfpControl.Request := reqSeek;
      mfpControl.StartPosition := hnsPosition;
    end
  else
    hr:= SetPositionInternal(hnsPosition);

  Result:= hr;
end;


// Do not call this function directly!
// This function is called by SetPosition.
function TMfPlayerX.SetPositionInternal(tPos: MFTIME): HRESULT;
var
  varStart: PROPVARIANT;
  hr: HRESULT;

begin

  hr:= E_FAIL;

  if (m_pSession = nil) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

try

  PropVariantInit(varStart);
  varStart.vt := VT_I8;

  varStart.hVal.QuadPart := tPos;

  hr:= m_pSession.Start(GUID_NULL,
                        varStart);

  // The Start method can also specify a starting position relative to the start
  // of the file; see the API reference topic for more information.

  PropVariantClear(varStart);

  if (SUCCEEDED(hr)) then
    begin
      // Store the pending state. CurrentPosition uses milliseconds, while
      // StartPosition remains in Media Foundation 100-ns units.
      mfpControl.StartPosition := tPos;
      mfpControl.CurrentPosition := tPos div ONE_HNS_MSEC;
      mfpControl.Request := reqSeek;

      if Assigned(FMediaTimeline) then
        FMediaTimeline.Seek(tPos div ONE_HNS_MSEC);

      State := Seeking;
      m_bPending := True;
      UpdateCaption();
    end;

finally
  Result:= hr;
end;
end;


procedure TMfPlayerX.GotoNewPosition(val: MFTIME);
begin

  if (val >= 0) then
    mfpControl.StartPosition := val;
end;


// Sets the playback rate.
// NOTE
// In Media Foundation, the playback rate is expressed as the ratio of the current
// playback rate to the normal playback rate.
// For example, a rate of 2.0 is twice normal speed, and 0.5 is half normal speed.
// Negative values indicate reverse playback.
// A playback rate of -2.0 plays backward through the stream at twice the normal speed.
// A rate of zero causes one frame to be rendered; after that,
// the presentation clock does not advance.
// To get another frame at the rate of zero, the application must seek to a new position.
// Applications use the following interfaces to control the playback rate.
//  IMFRateSupport. Used to find out the fastest and slowest playback rates that are possible.
//  IMFRateControl. Used to change the playback rate.
//
// To get these two interfaces, call IMFGetService.GetService on the Media Session. (see function
// The service identifier is MF_RATE_CONTROL_SERVICE.
// By using the rate control service, an application can implement fast forward and reverse playback.
//
// Thinning
//=========
// Thinning is any process that reduces the number of samples in a stream, to reduce the overall bit rate.
// For video, thinning is generally accomplished by dropping the delta frames and delivering only the key frames.
// Often the pipeline can support faster playback rates using thinned playback,
// because the data rate is lower because delta frames are not decoded.
// Thinning does not change the time stamps or durations on the samples.
// For example, if the nominal rate of the video stream is 25 frames per second,
// the duration of each frame is still marked as 40 milliseconds,
// even if the media source is dropping all of the delta frames.
// That means there will be a time gap between the end of one frame and the start of the next.
//
// Scrubbing
//==========
// Scrubbing is the process of instantaneously seeking to specific points in the stream by
// interacting with a scrollbar, timeline, or other visual representation of time.
// The term comes from the era of reel-to-reel tape players when rocking a reel back and
// forth to locate a section was like scrubbing the playback head with the tape.
// Scrubbing is implemented in Media Foundation by setting the playback rate to zero.
// For more information, see How to Perform Scrubbing on MSDN or docs.microsoft.com.
//
// NOTE: The rate interfaces can only be used if the topology is fully completed!
//       Since this is a async operation, it can take a while. The best starting point
//       to initiate those interfaces is within the OnTopologyStatus or OnTopologyReady event.
//
function TMfPlayerX.CommitRateChange(fRate: FLOAT;
                                     bThin: Boolean): HResult;
var
  hr: HResult;
  pClock: IMFClock;
  hnsSystemTime: MFTIME;
  hnsClockTime: LONGLONG;
  cmdNow: TRequest;

label
  done;

begin

  pClock := nil;

  //Assert(m_bPending);  // debug only


  //if m_bPending then   // implement the pending status if needed.
  //  goto done;

  // Caller holds the lock.

  hnsSystemTime := 0;
  hnsClockTime  := 0;

  cmdNow := mfpControl.Request;

  // Allowed rate transitions:

  // Positive <-> negative:   Stopped
  // Negative <-> zero:       Stopped
  // Postive <-> zero:        Paused or stopped

  if ((fRate > 0) And (mfpControl.fCurrentRate <= 0) Or
      (fRate < 0) And (mfpControl.fCurrentRate >= 0)) then
    begin
      // Transition to stopped.
      if (State = Started) {cmdNow = CmdStart} then
        begin

          // Get the current clock position. This will be the restart time.
          hr:= m_pSession.GetClock(pClock);
          if (FAILED(hr)) then
            goto done;

          hr := pClock.GetCorrelatedTime(0,
                                         hnsClockTime,
                                         hnsSystemTime);
          if (FAILED(hr)) then
            goto done;

          //Assert(hnsSystemTime <> 0);
          if (hnsSystemTime = 0) then
            goto done;

          // Stop and set the rate
          hr:= m_pSession.Pause();
          if (FAILED(hr)) then
            goto done;


          // Cache Request: Restart from stop.
          mfpControl.Request := reqSeek;            //m_request.command = CmdSeek;
          mfpControl.StartPosition := hnsClockTime; // m_request.hnsStart = hnsClockTime;
        end
      else
        if (State = Paused)  {cmdNow = CmdPause} then
          begin
            // The current state is paused.

            // For this rate change, the session must be stopped. However, the
            // session cannot transition back from stopped to paused.
            // Therefore, this rate transition is not supported while paused.

            hr := MF_E_UNSUPPORTED_STATE_TRANSITION;
            goto done;
          end;
    end
  else
    if (fRate = 0) And (mfpControl.fCurrentRate <> 0) then
      begin
        if (mfpControl.Request <> reqPause) {cmdNow != CmdPause} then
          begin
            // Transition to paused.
            // This transisition requires the paused state.
            // Pause and set the rate.
            hr := Pause();
            if (FAILED(hr)) then
              goto done;

            // Request: Switch back to current state.
            mfpControl.Request := cmdNow;
        end;
    end;

    // Set the rate.
    hr:= m_pRateControl.SetRate(bThin,
                                fRate);
    if (FAILED(hr)) then
      goto done;

    // Adjust our current rate and requested rate.
    mfpControl.fCurrentRate := fRate;
    mfpControl.SeekState.fRequestedRate := fRate;

done:
  Result:= hr;
end;


function TMfPlayerX.GetNominalRate(): FLOAT;
begin

  Result := mfpControl.fInitialRate;
end;


// Sets the desired clipping window/control
procedure TMfPlayerX.SetVideoScreen(val: HWND);
begin

  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.SetVideoWindow(val);
end;


function TMfPlayerX.GetVideoScreen(): HWND;
begin

  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.GetVideoWindow(Result);
end;


function TMfPlayerX.GetVideoRectangle(): TRect;
var
  rc: TRect;
  nrc: MFVideoNormalizedRect;

begin

  m_pVideoDisplay.GetVideoPosition(nrc,
                                   rc);
  CopyTRectToTRect(rc,
                   Result);
end;


function TMfPlayerX.GetPosition(out hnsPosition: MFTIME): HRESULT;
var
  hr: HRESULT;

begin

  hr:= S_OK;

  if (MFPresentationClock = nil) then
    begin
      Result := MF_E_NO_CLOCK;
      Exit;
    end;

  // Return, in order:
  // 1. Cached seek request (nominal position).
  // 2. Pending seek operation (nominal position).
  // 3. Presentation time (actual position).

  if (mfpControl.Request = reqSeek) then
    hnsPosition := mfpControl.StartPosition
  else
    hr := MFPresentationClock.GetTime(hnsPosition);

  Result := hr;
end;


// RATE CONTROL ////////////////////////////////////////////////////////////////
// Queries whether the current session supports fast-forward.
procedure TMfPlayerX.CheckCanSetRateForward();
begin

  m_bCanSetRateForward := ((m_dcaps and MFSESSIONCAP_RATE_FORWARD) = MFSESSIONCAP_RATE_FORWARD);
end;

// Queries whether the current session supports fast-reverse.
procedure TMfPlayerX.CheckCanSetRateReverse();
begin

  m_bCanSetRateReverse := ((m_dcaps and MFSESSIONCAP_RATE_REVERSE) = MFSESSIONCAP_RATE_REVERSE);
end;


// initialise the rate interfaces.
function TMfPlayerX.InitiateRateControl(): HResult;
var
  hr: HResult;
  fltmprate: FLOAT;

begin

  hr := S_OK;
  fltmprate := 0;

  // Get the RateSupport interface
  //===============================
  // There are 3 possible options to get this interface:
  // - By query on the Media Session
  // hr:= m_pSession.QueryInterface(IID_IMFRateSupport, m_pRateSupport);
  // - By calling MFGetService
  //   The MFGetService function is a helper function that wraps the IMFGetService.GetService method.
  if (SUCCEEDED(hr)) then
    hr:= MFGetService(m_pSession,
                      MF_RATE_CONTROL_SERVICE,
                      IID_IMFRateSupport,
                      Pointer(m_pRateSupport));

  // Get the RateControl interface
  //==============================
  // There are 2 possible options to get this interface:
  // - By query on the Media Session
  // hr:= m_pSession.QueryInterface(IID_IMFRateControl, m_pRateControl);
  // - By calling MFGetService
  //   The MFGetService function is a helper function that wraps the IMFGetService.GetService method.
  if (SUCCEEDED(hr)) then
    hr:= MFGetService(m_pSession,
                      MF_RATE_CONTROL_SERVICE,
                      IID_IMFRateControl,
                      Pointer(m_pRateControl));


  // - By calling IMFGetService.GetService on the Media Session.

  if (SUCCEEDED(hr)) then
    // Check if rate 0 (scrubbing) is supported.
    hr:= m_pRateSupport.IsRateSupported(False,
                                        0,
                                        @fltmprate);

  if (SUCCEEDED(hr)) then
    begin
      mfpControl.SeekState.bCanScrub:= True;
      hr:= m_pRateSupport.GetSlowestRate(MFRATE_FORWARD,
                                         mfpControl.SeekState.bCanScrub,
                                         fltmprate);
      //OleCheck(hr); {debug}

      // Some formats, like .mkv, return a negative rate.
      // Mf can only deal with positive rates, so use Abs() to correct this.
      mfpControl.fMinRateSupported:= Abs(fltmprate);

      if (SUCCEEDED(hr)) then
      //check fastest rate
      hr:= m_pRateSupport.GetFastestRate(MFRATE_FORWARD,
                                         mfpControl.SeekState.bCanScrub,
                                         fltmprate);
      //OleCheck(hr); {debug}

      mfpControl.fMaxRateSupported:= Abs(fltmprate);

    end
  else     // if m_pRate is nil, bCanScrub must be FALSE.
    begin
      mfpControl.SeekState.bCanScrub:= False;
      mfpControl.SeekState.bCanThinPb:= False;
    end;

  // Set rate properties to True or False
  CheckCanSetRateForward();
  CheckCanSetRateReverse();

  Result:= hr;

end;

// Sets the playback rate.
procedure TMfPlayerX.SetRate(val: FLOAT);
var
  hr: HResult;   // for debugging purposes
  bThin: Boolean;
  frval: FLOAT;
  flSuprate: FLOAT;

begin

  bThin := Boolean(0);
  flSuprate := 0;

try

  frval := Abs(val); // Some formats return a negative rate. (like .mkv)

  if (frval = GetNominalRate()) then
    Exit;

  if (m_pRateSupport = nil) then
    Exit;

  // Check if this rate is supported. Try non-thinned playback first,
  // then fall back to thinned playback.

  hr := m_pRateSupport.IsRateSupported(False,
                                       frval,
                                       @flSuprate);

  if (FAILED(hr)) then
    begin
      bThin := True;
      hr := m_pRateSupport.IsRateSupported(True,
                                           frval,
                                           @flSuprate);
    end;

  if (FAILED(hr)) then
    // Unsupported rate.
    Exit;

  // No pending operation? Should be implemented here.

  //Commit the new rate.
  {hr:=} CommitRateChange(frval,
                          bThin);
  if Assigned(FMediaTimeline) then
    FMediaTimeline.SetRate(frval);
  //OleCheck(hr); {debug}

finally
  //
end;
end;


function TMfPlayerX.GetRate(): FLOAT;
begin

  if (m_pRateSupport = nil) then
    begin

      Result := 1.0;
      Exit;
    end;

  Result:= m_pRateControl.GetRate(mfpControl.SeekState.bCanThinPb,
                                  Result);
end;


// Returns the active(current) stream of a media type
function TMfPlayerX.GetActiveStreamType(stType: TMediaTypes; out iStreamIndex: DWord): HRESULT;
var
  hr: HResult;

begin

  if Assigned(m_pSourcePD) then
    hr:= GetActiveStreamIndex(stType,
                              m_pSourcePD,
                              iStreamIndex)
  else
    hr:= E_POINTER;

  Result:= hr;
end;


// Select and deselect streams.
function TMfPlayerX.SetActiveStreamType(stType: TMediaTypes; iStreamIndex: DWord): HRESULT;
var
  iD: DWord;
  hr: HRESULT;

begin

  hr := E_POINTER;
  iD := 0;

  if assigned(m_pSourcePD) then
    begin

      // Get the active given stream
      hr:= GetActiveStreamType(stType,
                               iD);

      // The returned value of iD should always be >= 0.
      if SUCCEEDED(hr) then
        begin
          // Deselect the current active stream
          hr:= m_pSourcePD.DeselectStream(iD);
          // Select the new one
          if SUCCEEDED(hr) then
            hr:= m_pSourcePD.SelectStream(iStreamIndex);

          if SUCCEEDED(hr) then
            begin
              m_aStreamCont[iD].bSelected := BOOL(0);  // False
              // Select given stream to activate
              m_aStreamCont[iStreamIndex].bSelected := BOOL(1); // True
            end;

          // Set the new topology (nodes for selected stream needs to be set)
          if SUCCEEDED(hr) then
            hr := CreatePlaybackTopology(m_pSource,
                                         m_pSourcePD,
                                         m_hwndVideo,
                                         m_pTopology,
                                         mfpControl.SourceStreams);

          // Set the topology back on the media session.
          if SUCCEEDED(hr) then
            hr:= m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                                        m_pTopology);

          // New topo is set. User clicks Play or continues while playing
          if SUCCEEDED(hr) then
            begin

              if (State = Started) or (State = Paused) then
                begin
                  SetVolume(m_VolumeChannels);
                  SetNewPosition := Position;
                  SendPlayerRequest(reqSeek);
                end;
            end;
        end
      else
        hr := E_FAIL;  // Could not find an active stream.
    end;
  Result := hr;
end;


// Set the volumes for the channels.
procedure TMfPlayerX.SetVolume(Value: TFloatArray);
var
  pVol: IMFAudioStreamVolume;
  nChan: UINT32;
  aVolumes: TFloatArray;
  hr: HRESULT;  // used for debugging
  i: integer;

begin

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  aVolumes := Value;

  // Set boundaries to prevent overflow or clipping
  for i := 0 to Length(aVolumes) -1 do
    begin
      if (aVolumes[i] > 1.0) then
        aVolumes[i] := 1.0;
      if (aVolumes[i] < 0.0) then
        aVolumes[i] := 0.0;
    end;

  hr := (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                 IID_IMFAudioStreamVolume,
                                                 Pointer(pVol));

  // Get the number of channels
  if (SUCCEEDED(hr)) then
    hr := pVol.GetChannelCount(nChan);

  // Set the volumes
  if SUCCEEDED(hr) then
    begin
      hr := pVol.SetAllVolumes(nChan,
                               @aVolumes[0]);
      if (FAILED(hr)) then
        Exit;
      nChannels := nChan;
    end;
end;


function TMfPlayerX.Stop(): HRESULT;
var
  hr: HResult;

begin

  if Assigned(m_pSession) then
    begin
      hr := m_pSession.Stop();

      if SUCCEEDED(hr) then
        begin

          mfpControl.Request := reqStop;
          State := Stopping;
          m_bPending := True;
        end;
    end
  else
    hr := E_POINTER;

  Result:= hr;
end;


// Called after an operation completes.
// This method executes any cached requests.
procedure TMfPlayerX.UpdateCaption();
var
  DurationMs: Int64;
  RemainingMs: Int64;
  CaptionText: string;

begin

  case Self.GetState() of
    OpenPending: SetWindowText(m_hwndEvent,
                               'Loaded: ' +
                               ExtractFileName(FFileName));

    Ready:       SetWindowText(m_hwndEvent,
                               'Session is ready.');

    Closing:     SetWindowText(m_hwndEvent,
                               'Closing session...');
    Started:     begin
                   // uiDuration is stored in Media Foundation 100-ns units,
                   // while CurrentPosition is cached in milliseconds.
                   DurationMs := Int64(mfpControl.uiDuration div ONE_HNS_MSEC);

                   CaptionText := 'Playing: ' +
                                  ExtractFileName(FFileName) +
                                  '   Position: ' +
                                  MSecToStr(mfpControl.CurrentPosition, True);

                   if (DurationMs > 0) then
                     begin
                       RemainingMs := DurationMs - mfpControl.CurrentPosition;
                       if (RemainingMs < 0) then
                         RemainingMs := 0;

                       CaptionText := CaptionText +
                                      ' / ' + MSecToStr(DurationMs, True) +
                                      '   Remaining: ' +
                                      MSecToStr(RemainingMs, True);
                     end;

                   SetWindowText(m_hwndEvent,
                                 PChar(CaptionText));
                 end;
    Paused:       SetWindowText(m_hwndEvent,
                                'Paused at ' +
                                MSecToStr(mfpControl.CurrentPosition, True));

    Stopped:      SetWindowText(m_hwndEvent,
                                'Stopped.');

    Closed:       SetWindowText(m_hwndEvent,
                                'Session closed.');

    Seeking:      SetWindowText(m_hwndEvent,
                                'Starting at position ' +
                                MSecToStr(mfpControl.StartPosition div ONE_HNS_MSEC, false));
   end;
end;


procedure TMfPlayerX.SetFileName(aValue: WideString);
begin

  FFileName:= aValue;
end;


procedure TMfPlayerX.SetSubtitleLanguage(aValue: string);
begin

  if (FSubtitleLanguage <> aValue) then
    begin
      FSubtitleLanguage := aValue;
      ClearSubtitleBitmap();

      if (FFileName <> '') and Assigned(FSubtitleCompositor) then
        ReloadTimedText();
    end;
end;



procedure TMfPlayerX.SetSubtitleAspectRatio(aValue: Single);
begin

  if (aValue <= 0.0) then
    aValue := AR_16_9;

  if (Abs(FSubtitleAspectRatio - aValue) > 0.0001) then
    begin
      FSubtitleAspectRatio := aValue;
      if Assigned(FSubtitleCompositor) then
        FSubtitleCompositor.SubtitleAspectRatio := FSubtitleAspectRatio;
      ClearSubtitleBitmap();
    end;
end;


function TMfPlayerX.GetTimedTextFileLoaded(): Boolean;
begin

  Result := Assigned(FSubtitleCompositor) and FSubtitleCompositor.TimedTextFileLoaded;
end;


function TMfPlayerX.GetSubtitleSourcesAvailable(): Boolean;
begin

  Result := Assigned(FSubtitleCompositor) and
            FSubtitleCompositor.HasSubtitleSources();
end;


function TMfPlayerX.RefreshEmbeddedSubtitleTracks(): HRESULT;
begin

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.RefreshEmbeddedSubtitleTracks(m_pSourcePD);
end;


function TMfPlayerX.GetEmbeddedSubtitleTracks(out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
begin

  SetLength(Tracks,
            0);
  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.GetEmbeddedSubtitleTracks(Tracks);
end;


function TMfPlayerX.GetPreferredEmbeddedSubtitleStreamIndex(out StreamIndex: DWORD): HRESULT;
begin

  StreamIndex := 0;

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.GetPreferredEmbeddedSubtitleStreamIndex(StreamIndex);
end;


function TMfPlayerX.SelectEmbeddedSubtitleTrack(StreamIndex: DWORD): HRESULT;
begin

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.SelectEmbeddedSubtitleTrack(StreamIndex);
  if (Result = S_OK) then
    begin
      FSubtitleLanguage := FSubtitleCompositor.PreferredLanguage;
      FSubtitlesEnabled := True;
      ClearSubtitleBitmap();
      if Assigned(FSubtitlePlaybackControl) then
        FSubtitlePlaybackControl.SetEnabled(True);
    end;
end;


function TMfPlayerX.SelectSidecarSubtitleLanguage(const LanguageTag: string): HRESULT;
begin

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.SelectSidecarSubtitleLanguage(LanguageTag);
  if (Result = S_OK) then
    begin
      FSubtitleLanguage := FSubtitleCompositor.PreferredLanguage;
      FSubtitlesEnabled := True;
      ClearSubtitleBitmap();
      if Assigned(FSubtitlePlaybackControl) then
        FSubtitlePlaybackControl.SetEnabled(True);
    end;
end;


procedure TMfPlayerX.CommitSubtitleSelection();
begin

  if not Assigned(FSubtitleCompositor) then
    Exit;

  FSubtitleLanguage := FSubtitleCompositor.PreferredLanguage;
  FSubtitlesEnabled := FSubtitleCompositor.TimedTextFileLoaded;
  ClearSubtitleBitmap();

  if Assigned(FSubtitlePlaybackControl) then
    FSubtitlePlaybackControl.SetEnabled(FSubtitlesEnabled);
end;


function TMfPlayerX.GetActiveEmbeddedSubtitleStreamIndex(): Integer;
begin

  if Assigned(FSubtitleCompositor) then
    Result := FSubtitleCompositor.ActiveEmbeddedStreamIndex
  else
    Result := -1;
end;


function TMfPlayerX.GetActiveSubtitleIsEmbedded(): Boolean;
begin

  Result := Assigned(FSubtitleCompositor) and FSubtitleCompositor.ActiveSubtitleIsEmbedded;
end;


function TMfPlayerX.ExportActiveSubtitlesAsWebVtt(out AData: TBytes;
                                                  out ALanguageTag: string;
                                                  out AFriendlyLanguageName: string): HRESULT;
begin

  SetLength(AData,
            0);
  ALanguageTag := '';
  AFriendlyLanguageName := '';

  if not FSubtitlesEnabled then
    begin
      Result := S_FALSE;
      Exit;
    end;

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := FSubtitleCompositor.ExportActiveWebVtt(AData,
                                                   ALanguageTag,
                                                   AFriendlyLanguageName);
end;

procedure TMfPlayerX.SetSubtitlesEnabled(aValue: Boolean);
begin

  FSubtitlesEnabled := aValue and TimedTextFileLoaded;

  if Assigned(FSubtitlePlaybackControl) then
    FSubtitlePlaybackControl.SetEnabled(FSubtitlesEnabled);

  if not FSubtitlesEnabled then
    ClearSubtitleBitmap()
  else
    FSubtitleBitmapText := '';
end;


function TMfPlayerX.ReloadTimedText(): HRESULT;
var
  hr: HRESULT;

begin

  FSubtitlesEnabled := False;
  ClearSubtitleBitmap();

  if not Assigned(FSubtitleCompositor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if (FFileName = '') then
    begin
      FSubtitleCompositor.Close();
      Result := E_INVALIDARG;
      Exit;
    end;

  FSubtitleCompositor.SubtitleAspectRatio := FSubtitleAspectRatio;
  hr := FSubtitleCompositor.OpenTimedTextFile(FFileName,
                                              SubtitleLanguage,
                                              m_pSourcePD,
                                              False);
  FSubtitlesEnabled := (hr = S_OK) and
                       FSubtitleCompositor.TimedTextFileLoaded;

  OutputDebugString(PChar(Format(
    'MfPlayer X2 subtitle load hr=%s loaded=%s sources=%s enabled=%s',
    [IntToHex(DWORD(hr), 8),
     BoolToStr(FSubtitleCompositor.TimedTextFileLoaded),
     BoolToStr(FSubtitleCompositor.HasSubtitleSources()),
     BoolToStr(FSubtitlesEnabled)])));

  if Assigned(FSubtitlePlaybackControl) then
    FSubtitlePlaybackControl.SetEnabled(FSubtitlesEnabled);

  Result := hr;
end;


// Called after an operation completes.
// This method executes any cached requests.
function TMfPlayerX.UpdatePendingCommands(req: TRequest): HRESULT;
var
  hr: HRESULT;

begin

  hr:= S_OK;

  if (m_bPending) and (mfpControl.Request = req) then
    begin
      m_bPending:= False;
      // The current pending command has completed.

      // First look for rate changes.
      if (mfpControl.fCurrentRate <> mfpControl.SeekState.fRequestedRate) then
        begin
          hr:= CommitRateChange(mfpControl.SeekState.fRequestedRate,
                                mfpControl.SeekState.bCanThinPb);
          if (FAILED(hr)) then
            begin
              Result:= hr;
              Exit;
            end;
        end;

      // Now look for seek requests.
      if not m_bPending then
        case req of
          reqNone: ; // Nothing to do.
          reqStart: begin
                      if SUCCEEDED(Start()) then
                        State:= Started;
                    end;
          reqPause: begin
                      if SUCCEEDED(Pause()) then
                        State:= Paused;
                    end;
          reqStop:  begin
                      Stop();
                    end;
          reqSeek:  SetPosition(mfpControl.StartPosition);
          reqRate: ; // not implemented
        end;
      // Request is done
      mfpControl.Request:= reqNone;
    end;

  // Handle messages in queue.
  HandleMessages(GetCurrentThread());
  UpdateCaption();
  Result:= hr;
end;


// TimedText implementation ////////////////////////////////////////////////////



//// End TMfPlayer class ///////////////////////////////////////////////////////

constructor TClockStateSink.Create();
begin

  inherited Create();

  p_MFTime := 0;
  p_flRate := 1.0;
end;


destructor TClockStateSink.Destroy();
begin

  inherited Destroy();
end;


function TClockStateSink.OnClockStart(hnsSystemTime: MFTIME;
                                      llClockStartOffset: LongLong): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  //
  p_MFTime := hnsSystemTime;
  Result := hr;
end;


// this function is called prior to OnSessionStarted!
function TClockStateSink.OnClockStop(hnsSystemTime: MFTIME): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  p_MFTime := hnsSystemTime;
  // there is no need to do something with the timer, because
  // it will respond on the state of the PresentationClock.
  Result := hr;
end;



function TClockStateSink.OnClockPause(hnsSystemTime: MFTIME): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  p_MFTime := hnsSystemTime;
  //
  Result := hr;
end;



function TClockStateSink.OnClockRestart(hnsSystemTime: MFTIME): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  p_MFTime := hnsSystemTime;
  //
  Result := hr;
end;


function TClockStateSink.OnClockSetRate(hnsSystemTime: MFTIME;
                                        flRate: Single): HResult;
var
  hr: HResult;

begin

  hr := S_OK;
  p_MFTime := hnsSystemTime;
  p_flRate := flRate;
  //
  Result := hr;
end;


initialization
  //

finalization
  // Not needed, but can't harm as well.
  MFShutdown();
end.

