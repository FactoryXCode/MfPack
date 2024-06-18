// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfpPlayerClassX.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 3.1.6
// Description: This is the extended basic player class (version X),
//              containing the necessary methodes to play a mediafile
//              For indepth information see the included examples (CPlayer)
//              and text files containing the complete information about
//              MfPlayer.
//
// Company: FactoryX
// Intiator(s): Ramyses De Macedo Rodrigues, Tony (maXcomX), Peter (OzShips).
// Contributor(s): Ramyses De Macedo Rodrigues,
//                 Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Jason Nelson (adaloveless)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  TimedTextClass,
  FloatingFrm,
  MFTimerCallBackClass,
  MfPCXConstants,
  LangTags,
  {MfComponents}
  UniThreadTimer;  {if getting an error about a missing dcu, install MfComponents first or
                    add the searchpath to MfComponents in your project options}

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
    CurrentPosition: MFTIME;      // Current position
    uiDuration: UINT64;           // Duration
    uiFileSize: UINT64;           // Filesize
    Volume: FLOAT;                // Volume
    SourceStreams: DWORD;         // Number of sourcestreams
  end;

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

    // Conversions
    procedure SetFileName(aValue: WideString);
    procedure SetSubtitleLanguage(aValue: string);

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
    function CreateSession(): HRESULT;

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
    function OnSessionStopped(pEvent: IMFMediaEvent): HRESULT; // Stop event request

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
    m_hwndFloatingForm: HWnd;                // handle of the FloatingForm (TimedText layer)

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

    // Shut down the session and MF
    // Use this funtion to kill the MfPlayer.
    function ShutDown(): HRESULT;

    // Video functionality
    function Repaint(): HRESULT;

    // Resizes the video rectangle.
    // The application calls this method if the size of the video window changes;
    // e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(pdRect: LPRECT = Nil): HRESULT;

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

    // Get the duration if the mediafile
    function GetInitialDuration(out dur: MFTIME): HRESULT;

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
    property Duration: UINT64 read mfpControl.uiDuration;
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
    property VideoRectangle: TRect read GetVideoRectangle;
    property Volumes: TFloatArray read m_VolumeChannels write SetVolume;
  end;

var
  MfPlayerX: TMfPlayerX;


implementation

////////////// TMFPlay /////////////////////////////////////////////////////////

// Reset to initial values
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

  // reset
  m_bPending := False;
  m_dCaps := 0;
  FFileName := '';

try

  // Don't free dynamic array's when the program is terminating.
  // They are managed types and therefor, they are freed automatically.
  if Not m_bAppIsClosing then
    begin
      Finalize(m_VolumeChannels);
      Finalize(m_aStreamCont);
    end;

  // Release the interfaces
  SafeRelease(m_pTopology);
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
    FreeAndNil(MFCallBack);

  if Assigned(m_pClockStateSink) then
    begin
      // This call will also destroy the ClockStateSink.
      if Succeeded(MFPresentationClock.RemoveClockStateSink(m_pClockStateSink)) then
        MFPresentationClock := nil;

      FreeAndNil(m_pClockStateSink);

    end;

  // Close the floating form, if it's loaded.
  if Assigned(FloatingForm) then
    FloatingForm.Close();

  SetLength(m_aStreamCont, 0);
  Finalize(m_aStreamCont);

except
  //
end;
end;


procedure TMfPlayerX.SetCustomMessage(value: string);
begin
  sCustomMessage:= value;
end;


function TMfPlayerX.CloseSession(): HRESULT;
var
  hr: HRESULT;
  dwWaitResult: DWORD;

label
  Done;

begin
  // The IMFMediaSession.Close method is asynchronous, but the
  // MfPlayer.CloseSession method waits on the MESessionClosed event.
  //
  // MESessionClosed is guaranteed to be the last event that the
  // media session fires.

  hr := s_ok;

  // release the video display object
  SafeRelease(m_pVideoDisplay);

  // First close the media session.
  if Assigned(m_pSession) then
    begin
      hr := m_pSession.Stop();

      if FAILED(hr) then
        goto done;

      State := Closing;
      hr := m_pSession.Close();
      if SUCCEEDED(hr) then
        begin
          // Wait for the close operation to complete for one second
          dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent), 2000);

          if (dwWaitResult <> STATUS_WAIT_0) {Wait abandoned} then
            m_dWaitResult := dwWaitResult;

          // Complete shutdown operations.
          // First shut down the media source. (Synchronous operation, no events.)
          if Assigned(m_pSource) then
            {Void} m_pSource.Shutdown();

          // Then close the media session. (Synchronous operation, no events.)
          // Now the following steps needs to be taken o safelyend the session without memory leaks:
          if Assigned(m_pSession) then
            begin
              // 1 Clear queued presentations
              hr := m_pSession.ClearTopologies();
              // 2 Shutdown the session
              if (SUCCEEDED(hr)) then
                hr := m_pSession.Shutdown();
              if (FAILED(hr)) then
                goto Done;
            end;
        end;
    end;

  // Release the interfaces.
  SafeRelease(m_pSource);
  SafeRelease(m_pSession);
  State := Closed;
done:
  Result := hr;
end;


procedure TMfPlayerX.WndProc(var Msg: TMessage);
begin
  // prevent processing messages when app is shutting down.
  if m_bAppIsClosing then
    Exit;

  if (Msg.Msg = WM_TIMERNOTIFY) then // Check for timer messages
    try
      if (Msg.LParam = S_OK) then
        begin
          // Send a message to the owner form to update the progressbar
          SendMessage(m_hwndMainForm,
                      WM_PROGRESSNOTIFY,
                      WPARAM(1),
                      0);

          // Send a message to the floatingform so that it can keep track with the
          // subtitle timing.
          if Assigned(FloatingForm) and (FloatingForm.Handle > 0) then
            SendMessage(FloatingForm.Handle,
                        WM_PROGRESSNOTIFY,
                        0,
                        0);

          // Get position
          GetPosition(mfpControl.CurrentPosition);
          mfpControl.CurrentPosition := HnsTimeToMsec(mfpControl.CurrentPosition);
          UpdateCaption();
        end
      else if (Msg.LParam = MF_S_CLOCK_STOPPED) then
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
  // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
  // NOTE: The first parameter, m_hwndThis, is the handle of the window receiving this message.
  //       It is obtained from the call to AllocateHWnd in the Constructor.
  else
    msg.Result := DefWindowProc(m_hwndThis,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);
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

  //CoInitializeEx(nil,
  //               COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

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

  DeAllocateHWnd(m_hwndThis);
  // Shutdown the Media Foundation platform
  MFShutdown();
  //CoUninitialize();
  inherited Destroy;
end;


//
procedure TMfPlayerX.BeforeDestruction();
begin
  Clear();
  inherited BeforeDestruction;
end;


// Before the application exits, shut down the Media Session,
// and then call MFShutdown to shut down the Microsoft Media Foundation platform.

// Release all resources held by this object.
function TMfPlayerX.ShutDown(): HRESULT;
var
  hr: HRESULT;
  dwWaitResult: DWORD;

begin

  // The application must call Shutdown because the media session holds a
  // reference count on the CPlayer object. (This happens when CPlayer calls
  // IMediaEventGenerator.BeginGetEvent on the media session.) As a result,
  // there is a circular reference count between the CPlayer object and the
  // media session. Calling Shutdown breaks the circular reference count.

  // If CreateInstance failed, the application will not call Shutdown. To
  // handle that case, we must call Shutdown() in the destructor. The
  // circular ref-count problem does not occcur if CreateInstance has failed.
  // Also, calling Shutdown twice is harmless.
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  hr := S_OK;

try
  // set closing flag
  m_bAppIsClosing := True;

  // Close the session
  if SUCCEEDED(Self.Stop()) then
    hr := CloseSession();

  if (FhCloseEvent <> 0) then
    begin
      if CloseHandle(THandle(FhCloseEvent)) then
        begin
          // Wait for the close operation to complete
          dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent), INFINITE);
          if (dwWaitResult <> STATUS_WAIT_0) {Wait abandoned} then
            m_dWaitResult := dwWaitResult;
          FhCloseEvent := 0;
          hr := S_OK;
        end
      else
        hr := E_FAIL;
    end;

finally
  Result := hr;
end;
end;


function TMfPlayerX.CreateSession(): HRESULT;
var
  hr: HRESULT;

label
  Done;

begin
  // Close the old session, if any.
  hr:= CloseSession();
  if (FAILED(hr)) then
    goto done;

  assert(State = Closed);

  // Create the media session.
  hr:= MFCreateMediaSession(Nil,
                            m_pSession);
  if (FAILED(hr)) then
    goto done;

  mfpControl.State := Ready;

  // Start pulling events from the media session
  hr := m_pSession.BeginGetEvent(IMFAsyncCallback(Self),
                                Nil);
  if (FAILED(hr)) then
    goto done;

done:
  Result:= hr;
end;


// Take a snapshot
function TMfPlayerX.TakeSnapShot(var bit: TBitMap): HRESULT;
var
  buffer, data: PByte;
  bufSize: DWORD;
  i: Integer;
  bmi: BITMAPINFOHEADER;
  timestamp: MFTIME;
  hr: HRESULT;

begin
  hr:= S_OK;

try
  // Use assertions in debug mode only!
  Assert(bit <> nil);

  // Set the biSize member of the structure to sizeof(BITMAPINFOHEADER)
  ZeroMemory(@bmi,
             sizeof(BITMAPINFOHEADER));
  bmi.biSize := sizeof(BITMAPINFOHEADER);

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
        begin
          Result := E_FAIL;
          Exit;
        end;
      data := buffer;
    end;

  if (bmi.biSizeImage > 0) and (data <> nil) then
    begin
      // Adjustments
      Bit.PixelFormat := pf32bit;
      Bit.SetSize(abs(bmi.biWidth), abs(bmi.biHeight));
      for i := abs(bmi.biHeight) - 1 downto 0 do // (int y = h - 1; y >= 0; --y)
        begin
          CopyMemory(Bit.ScanLine[i],
                     data,
                     bmi.biWidth * bmi.biBitCount div 8);
          Inc(data, bmi.biWidth * bmi.biBitCount div 8);
        end;

      hr := S_OK;
    end;

finally
  Result := hr;
end;
end;


// Gets the duration of the current presentation.
function TMfPlayerX.GetInitialDuration(out dur: MFTIME): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  hr: HRESULT;
  mdur: MFTIME;

begin
  hr:= S_OK;

try

  if Assigned(m_pSource) then
    begin
      dur := 0;
      hr := m_pSource.CreatePresentationDescriptor(pPD);

      if (SUCCEEDED(hr)) then
        begin
          hr := pPD.GetUINT64(MF_PD_DURATION,
                              UINT64(mdur));
          dur := mdur;
          mfpControl.uiDuration := dur;
        end;
    end
  else
    hr := E_POINTER;

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

begin
  hr:= S_OK;

try
  SetLength(m_VolumeChannels,
            0);

  nChannels := 0;

  hr:= (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                IID_IMFAudioStreamVolume,
                                                Pointer(pVol));
  if SUCCEEDED(hr) then
    begin
      // Get the number of soundchannels from the stream
      hr := pVol.GetChannelCount(uiChan);

      // Volume levels are in the range 0.0 to 1.0.
      // If balanced volume is needed; use an array of channels

      SetLength(m_VolumeChannels,
                uiChan);  // MUST DO!

      // The GetAllVolumes method retrieves the volume levels for all the channels in the audio stream.
      // For example: If the mediafile sound is Dolby Digital 5.1 (AC3) then the number of soundchannels will be 6.
      // 5.1 stands for 5 soundchannels (right front, center, left front, right rear en left rear) and 1 channel
      // for the subwoofer, the LFE-channel (Low Frequency Effects, 20-120 Hz).
      // Therefore, To adjust the volumes of these channels, you will need 6 volumecontrols.
      hr := pVol.GetAllVolumes(uiChan,   // or Length(VolumeChannels) will do too
                               @m_VolumeChannels[0]);
      nChannels := uiChan;
    end;

finally
  Result := hr;
end;
end;


function TMfPlayerX.GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult;
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
                                     Nil);
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
        hr := hrStatus;
      if (FAILED(hr)) then
        goto done;

      // ALL possible session events are listed here in case statements.
      case meType of
        MEUnknown: begin {hr := OnUnknown(pEvent);} end;
        MEError: begin {hr := OnError(pEvent);} end;
        MEExtendedType: begin {OnExtendedType(pEvent);} end;
        {= MEGenericV1Anchor}
        MENonFatalError : begin {hr := OnNonFatalError(pEvent);} end;
        MESessionUnknown: begin {hr := OnSessionUnknown(pEvent);} end;
        MESessionTopologySet: begin {hr := OnSessionTopologySet(pEvent);} end;
        MESessionTopologiesCleared: begin {hr := OnSessionTopologiesCleared(pEvent);} end;
        MESessionStarted: begin hr := OnSessionStarted(pEvent); end;
        MESessionPaused: begin hr := OnSessionPaused(pEvent); end;
        MESessionStopped: begin hr := OnSessionStopped(pEvent); end;
        MESessionClosed: begin hr := OnSessionClosed(pEvent); end;
        MESessionEnded: begin {hr := OnSessionEnded(pEvent); }end; // Raised by the Media Session when it has finished playing the last presentation in the playback queue.
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
        {= MESessionV1Anchor}
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
        {= MESourceV1Anchor}
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
        {= MESinkV1Anchor}
        MEAudioSessionExclusiveModeOverride: begin {hr := OnAudioSessionExclusiveModeOverride(pEvent);} end;
        MECaptureAudioSessionVolumeChanged: begin {hr := OnCaptureAudioSessionVolumeChanged(pEvent);} end;
        MECaptureAudioSessionDeviceRemoved: begin {hr := OnCaptureAudioSessionDeviceRemoved(pEvent);} end;
        MECaptureAudioSessionFormatChanged: begin {hr := OnCaptureAudioSessionFormatChanged(pEvent);} end;
        MECaptureAudioSessionDisconnected: begin {hr := OnCaptureAudioSessionDisconnected(pEvent);} end;
        MECaptureAudioSessionExclusiveModeOverride: begin {OnCaptureAudioSessionExclusiveModeOverride(pEvent);} end;
        {= MESinkV2Anchor}
        MECaptureAudioSessionServerShutdown: begin {hr := OnCaptureAudioSessionServerShutdown(pEvent);} end;
        METrustUnknown: begin {hr := OnTrustUnknown(pEvent);} end;
        MEPolicyChanged: begin {hr := OnPolicyChanged(pEvent);} end;
        MEContentProtectionMessage: begin {hr := OnContentProtectionMessage(pEvent);} end;
        {= METrustV1Anchor}
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
        {= MEWMDRMV1Anchor}
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



function TMfPlayerX.HasVideo: boolean;
begin
  Result := (m_pVideoDisplay <> Nil);
end;


function TMfPlayerX.OnSessionTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
var
  status: MF_TOPOSTATUS;
  hr: HRESULT;

begin
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
begin

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

  if (FAILED(hr)) then
    begin
      //Abort; // silent exception
      // Or continue, without video ie if a soundtrack is loaded.
      Result := S_OK;
      Exit;
    end;

  // Set the target window (or control), at this point this is not really
  // nescesarry, because the previous -MfGetService- did that allready.
  //{void} m_pVideoDisplay.SetVideoWindow(m_hwndVideo);

  // Adjust aspect ratio
  if Assigned(m_pVideoDisplay) then
    begin
      ResizeVideo(Nil);
    end;


// Since the topology is ready, you might start playback, do rate calculations etc.
//
////////////////////////////////////////////////////////////////////////////////

        // At this stage we can do some rate issues
        if SUCCEEDED(hr) then
          hr := InitiateRateControl();

        // Obtain capabilities of the current session
        if SUCCEEDED(hr) then
          hr := m_pSession.GetSessionCapabilities(m_dCaps);
        if FAILED(hr) then
          m_dCaps := MFSESSIONCAP_START;

  // You can also implement an option for the user, starting playback directly after
  // a mediastream is loaded.
  //hr:= Play();

  State := TopologyReady;

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
  hr := CreatePlaybackTopology(m_pSource,
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
var
  hr: HResult;
begin
  mfpControl.Request:= reqPause;
  hr:= UpdatePendingCommands(mfpControl.Request);
  if SUCCEEDED(hr) then
    begin
      State  := Paused;
      Result := S_OK;
    end
  else
    Result := E_FAIL;
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

begin
  {hr :=} UpdatePendingCommands(reqStart);
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

  // trigger the timer
  if Assigned(MFCallBack) and (State = Started) then
    begin
      hr := MFCallBack.SetTimer(0,
                                DWord(MFTIMER_RELATIVE),
                                Nil); // Fire up immediately on default params
      if FAILED(hr) then
        raise Exception.CreateFmt('Could not create Timer. Result: ''%s''', [hr]);
      // Set the timer after firing for the first time to the desired resolution (clocktime).
      MFCallBack.TimerResolution := 1;
    end
  else
    hr := E_FAIL;

  Result := hr;
end;


function TMfPlayerX.OnSessionStopped(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HResult;

begin
  hr := S_OK;
  UpdatePendingCommands(reqStop);
  Result := hr;
end;


function TMfPlayerX.OnEndOfPresentation(pEvent: IMFMediaEvent): HRESULT;
begin
  Stop(); // The session puts itself into the stopped state automatically.
  Result := S_OK;
end;


// The main method to open and play a mediafile
function TMfPlayerX.OpenURL(sURL: PWideChar): HRESULT;
var
  pClock: IMFClock;
  ClockProps: MFCLOCK_PROPERTIES;
  hr: HRESULT;
  pwInt: PWideChar;

label
  done;

begin

  // 1. Create a new media session.
  // 2. Create the media source.
  // 3. Create the topology.
  // 4. Queue the topology [asynchronous]
  // 5. Create the presentation and stream descriptor to get info about the streams
  // 6. Set the presentationclock for rate, scrubbing etc.
  // 7. Ready to Start playback.

  // Release the interfaces
  Clear();
  pClock := Nil;
  hr := S_OK;
  m_hwndFloatingForm := 0;

try
try

  FFilename := sURL;

  // Create the media session.
  hr := CreateSession();
  if (FAILED(hr)) then
    goto done;

  // Create a media source from a URL or stream.
  hr := CreateObjectFromUrl(sURL,
                            m_pSource);
  if (FAILED(hr)) then
    goto done;

  // Create the presentation descriptor for the media source
  hr := m_pSource.CreatePresentationDescriptor(m_pSourcePD);

  if (FAILED(hr)) then
    goto done;

  // Create a (partial) topology, com branches, source nodes and sink nodes.
  // Note: Playback topologies are always partial!
  hr := CreatePlaybackTopology(m_pSource,
                               m_pSourcePD,
                               m_hwndVideo,
                               m_pTopology,
                               mfpControl.SourceStreams);

  if (FAILED(hr)) then
    goto done;

  // Get the duration from the presentation descriptor (optional)
  hr := m_pSourcePD.GetUINT64(MF_PD_DURATION,
                              mfpControl.uiDuration);

  // Get the FileSize from the presentation descriptor (optional)
  hr := m_pSourcePD.GetUINT64(MF_PD_TOTAL_FILE_SIZE,
                              mfpControl.uiFileSize);

  // Obtain capabilities of the current session
  hr := m_pSession.GetSessionCapabilities(m_dCaps);
  if FAILED(hr) then
    m_dCaps := $0001;

  if (FAILED(hr)) then
    goto done;


  // The Media Session automatically creates the presentation clock.
  // Get the presentation clock.
  hr := m_pSession.GetClock(pClock);

  if (SUCCEEDED(hr)) then
    begin

      // Get the presentationclock exposed by the clock.
      hr := pClock.QueryInterface(IID_IMFPresentationClock,
                                  MFPresentationClock);
      if FAILED(hr) then
        goto done; // Without a presentation clock you can't set rate, the
                   // position does't work and no timer!

      // Create the ClockStateSink interface
      m_pClockStateSink := TClockStateSink.Create();

      // Registers an object to be notified whenever the clock starts, stops, or pauses, or changes rate.
      hr := MFPresentationClock.AddClockStateSink(m_pClockStateSink);
      if FAILED(hr) then
        goto done;

      // Get the timesource. The presentation clock cannot start until it has a time source.
      hr := MFCreateSystemTimeSource(m_pTimeSource);
      if FAILED(hr) then
        goto done;

      // Set timesource on the PresentationClock
      hr := MFPresentationClock.SetTimeSource(m_pTimeSource);
      if FAILED(hr) then
        goto done;

      // Retrieves the properties of the clock, to call this function, the timesource must be set!
      hr := MFPresentationClock.GetProperties(ClockProps);
      if FAILED(hr) then
        goto done;

      // Create the Timercallback
      MFCallBack := TMFCallBack.Create(m_hwndThis);
      if Not Assigned(MFCallBack) then
        goto done;
    end
  else
    goto done; // No valid clock

  // Set the topology on the media session.
  // If SetTopology succeeds, the media session will queue an
  // MESessionTopologySet event.
  // This event is evaluated and fired in the OnTopologyStatus method
  hr:= m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                              m_pTopology);
  if (FAILED(hr)) then
    goto done;

  // Get info from the streams, like mediatype, language, compression etc.
  hr := GetStreamContents(m_pSourcePD,
                          m_pSource,
                          m_aStreamCont);


  // Create the FloatingForm for timedtext
  if (FloatingForm = nil) then
    FloatingForm := TFloatingForm.Create(Application,
                                         m_hwndVideo,
                                         sURL,
                                         SubtitleLanguage,
                                         ClockProps);

  // The TimedText file (srt, vtt or sub) will be loaded automaticly by the floatingform/TimedTextClass,
  // if one is present that matches the preferred language.
  // On this point the floatingform did a number of checks.
  // If there are no timedtextfiles, we will shut down the form.
  if assigned(FloatingForm) then
    begin
      if (FloatingForm.TimedTextFileIsLoaded = True) then
        // Store it's handle
        m_hwndFloatingForm := FloatingForm.Handle
      else
        begin
          m_hwndFloatingForm := 0;
          FreeAndNil(FloatingForm);
        end;
    end;

  // Set state to "open pending"
  State := OpenPending;
  m_bPending := True;

done:

  if (FAILED(hr)) then
    begin
      Clear();
      Abort(); // throw exception
    end;

  UpdateCaption();

except
  Raise;
  pwInt := PWideChar(InttoStr(hr));

  MessageBox(0,
             PWideChar('An exception has been returned. (hr: ' + pwInt + ')'),
             PWideChar('Error'),
             MB_ICONERROR);
end;
finally
  Result:= hr;
end;
end;


// Pause
function TMfPlayerX.Pause(): HRESULT;
var
  hr: HRESULT;

begin

  hr:= E_FAIL;
  if (m_pSession = Nil) or (m_pSource = Nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if m_bPending then
    begin
      mfpControl.Request := reqPause;
      State := Pausing;
    end
  else
    begin
      hr := m_pSession.Pause();
      mfpControl.Request := reqNone;
      State := Paused;
      m_bPending := True;
    end;

  Result:= hr;
end;


// Start playback from the current position.
//==========================================
function TMfPlayerX.Start(): HRESULT;
var
  hr: HRESULT;
  varStart: PROPVARIANT;
  tPos: MFTIME;
  csClockState: MF_CLOCK_STATE;
  FVideoProcessor: IMFVideoProcessor;

begin
  hr:= S_OK;

  if (m_pSession = Nil) then
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

      tPos:= 0;

      MFPresentationClock.GetState(0,
                                   csClockState);

      // The Start method can also specify a starting position relative to the start
      // of the file; see the API reference topic for more information.
      if (csClockState = MFCLOCK_STATE_PAUSED) then
        begin
          varStart.vt := VT_I8;
          MFPresentationClock.GetTime(tPos);
          varStart.hVal.QuadPart := tPos;
        end
      else
        begin
          varStart.vt := VT_EMPTY;  // Slow when returning from pause!
        end;

      // Start the session
      // The presentation clock will be started automaticly by the session.
      hr := m_pSession.Start(GUID_NULL,
                             varStart);

      m_bPending := True;
      hr := PropVariantClear(varStart);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // Get initial volume
      GetVolume();
      mfpControl.Request := reqNone;
      State := Started;

      // check if there is video present
      if (HasVideo() = True) then
        begin
          hr:= (m_pSession as IMFGetService).GetService(MR_VIDEO_MIXER_SERVICE,
                                                        IID_IMFVideoProcessor,
                                                        Pointer(FVideoProcessor));
          if (SUCCEEDED(hr)) then
            hr := FVideoProcessor.GetBackgroundColor(FBGColor);
        end
      else
        hr := E_NOTIMPL;  // better is to implement a custom E_VIDEO_NOT_PRESENT or something like that.
    end;

finally
  Result:= hr;
end;
end;


// Repaint the video window.
// Call this method on WM_PAINT from the form where the video is playing on.
function TMfPlayerX.Repaint(): HRESULT;
begin

  if Assigned(m_pVideoDisplay) then
    begin
      Result := m_pVideoDisplay.RepaintVideo();
    end
  else
    begin
      Result := S_OK;
    end;
end;


function TMfPlayerX.ResizeVideo(pdRect: LPRECT = Nil): HRESULT;
var
  rcpdest: LPRECT;
  rc: TRect;
  hr: HResult;  //debug purpose

begin
  hr := E_NOINTERFACE;
  rcpdest := Nil;

  if Assigned(m_pVideoDisplay) then
    begin
      // Stop repaint
      SetRedraw();
      // Set the destination rectangle.
      // If dRect is empty; use the GetClientRect function
      if (pdRect = Nil) then
        begin
          WinApi.Windows.GetClientRect(m_hwndVideo,
                                       rc);
          CopyTRectToLPRect(rc,
                            rcpdest);
        end
      else
        begin
          rcpDest := pdRect;
        end;

      hr := m_pVideoDisplay.SetVideoPosition(Nil,
                                             rcpdest);

      if SUCCEEDED(hr) then
        // Set aspect ratio in conjunction with SetVideoPosition
        hr := m_pVideoDisplay.SetAspectRatioMode(MFVideoARMode_PreservePicture);

      UpdateCaption();

      // Start repaint again
      SetRedraw();
    end;
  rcpdest := Nil;
  Result := hr;
end;


procedure TMfPlayerX.SetRedraw();
begin

  //Stop flickering of controls and subtitle when resizing.
  if (stRedrawStatus = rdStarted) then
    begin
      SendMessage(m_hwndMainForm, WM_SETREDRAW, WPARAM(False), 0);
      stRedrawStatus := rdStopped;
    end
  else
    begin
      SendMessage(m_hwndMainForm,
                  WM_SETREDRAW,
                  WPARAM(True), 0);

      RedrawWindow(m_hwndMainForm,
                   Nil,
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
  Paused         // Session is paused.
  Stopping       // Session initializing Stop.
  Stopped        // Session is stopped (ready to play).
  Closing        // Shutting down.
  Seeking        // Session seeks.
  SeekingDone,   // Session has ended seeking.
  TopologyIsSet  // Session topology has been set.
  }

  UpdatePendingCommands(req);
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
    begin
      hr:= SetPositionInternal(hnsPosition);
    end;

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
      // Store the pending state
      mfpControl.StartPosition := tPos;
      mfpControl.Request := reqNone;
      State := Started;
      m_bPending := true;
      UpdateCaption();
    end;

finally
  Result:= hr;
end;
end;


procedure TMfPlayerX.GotoNewPosition(val: MFTIME);
begin
  if val >= 0 then
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
  hnsSystemTime: MFTIME;
  hnsClockTime: LONGLONG;
  pClock: IMFClock;
  cmdNow: TRequest;

label
  done;

begin
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

  {void}  pClock.GetCorrelatedTime(0,
                                   hnsClockTime,
                                   hnsSystemTime);

          //Assert(hnsSystemTime <> 0);
          if hnsSystemTime = 0 then
            goto done;

          // Stop and set the rate
          hr:= m_pSession.Pause();
          if (FAILED(hr)) then
            goto done;


          // Cache Request: Restart from stop.
          mfpControl.Request := reqSeek;            //m_request.command = CmdSeek;
          mfpControl.StartPosition := hnsClockTime; // m_request.hnsStart = hnsClockTime;
        end
      else if (State = Paused)  {cmdNow = CmdPause} then
        begin
          // The current state is paused.

          // For this rate change, the session must be stopped. However, the
          // session cannot transition back from stopped to paused.
          // Therefore, this rate transition is not supported while paused.

          hr := MF_E_UNSUPPORTED_STATE_TRANSITION;
            goto done;
        end;
  end
    else if (fRate = 0) And (mfpControl.fCurrentRate <> 0) then
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


// Full screen methods
//procedure TMfPlayerX.SetFullScreen(val: BOOL);
//begin
//  if Assigned(m_pVideoDisplay) then
//    m_pVideoDisplay.SetFullscreen(val);
//end;


//function TMfPlayerX.IsFullScreen(): BOOL;
//begin
//  if Assigned(m_pVideoDisplay) then
//    {void} m_pVideoDisplay.GetFullscreen(Result);
//end;


function TMfPlayerX.GetPosition(out hnsPosition: MFTIME): HRESULT;
var
  hr: HRESULT;

begin
  hr:= S_OK;

  if (MFPresentationClock = Nil) then
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
    begin
      hr:= MFGetService(m_pSession,
                        MF_RATE_CONTROL_SERVICE,
                        IID_IMFRateSupport,
                        Pointer(m_pRateSupport));
    end;

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
    begin
      // Check if rate 0 (scrubbing) is supported.
      hr:= m_pRateSupport.IsRateSupported(False,
                                          0,
                                          fltmprate);
    end;

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
  else     // if m_pRate is Nil, bCanScrub must be FALSE.
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

  if (m_pRateSupport = Nil) then
    Exit;

  // Check if this rate is supported. Try non-thinned playback first,
  // then fall back to thinned playback.

  hr := m_pRateSupport.IsRateSupported(False,
                                       frval,
                                       flSuprate);

  if (FAILED(hr)) then
    begin
      bThin := True;
      hr := m_pRateSupport.IsRateSupported(True,
                                           frval,
                                           flSuprate);
    end;

  if (FAILED(hr)) then
    begin
      // Unsupported rate.
      Exit;
    end;

  // No pending operation? Should be implemented here.

  //Commit the new rate.
  {hr:=} CommitRateChange(frval,
                          bThin);
  //OleCheck(hr); {debug}

finally
  //
end;
end;


function TMfPlayerX.GetRate(): FLOAT;
begin
  if (m_pRateSupport = Nil) then
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
        begin
          hr := E_FAIL;  // Could not find an active stream.
        end;
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
      if aVolumes[i] > 1.0 then
        aVolumes[i] := 1.0;
      if aVolumes[i] < 0.0 then
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
      if m_bPending then
        begin
          mfpControl.Request := reqStop;
          hr := UpdatePendingCommands(mfpControl.Request);
          if (SUCCEEDED(hr)) then
            begin
              State := Stopped;
            end
          else
            hr := E_FAIL;
        end
      else
        begin
          hr := m_pSession.Stop();
          m_bPending := True;
        end;
    end
  else
    hr := E_POINTER;

  if Assigned(fTimedText) then
    FreeAndNil(fTimedText);
  Result:= hr;
end;


// Called after an operation completes.
// This method executes any cached requests.
procedure TMfPlayerX.UpdateCaption();
begin

  case Self.GetState() of
    OpenPending: SetWindowText(m_hwndEvent,
                               'Loaded: ' +
                               ExtractFileName(FFileName));

    Ready:       SetWindowText(m_hwndEvent,
                               'Session is ready.');

    Closing:     SetWindowText(m_hwndEvent,
                               'Closing session...');
    Started:       begin
                      SetWindowText(m_hwndEvent,
                                    'Playing: ' +
                                    ExtractFileName(FFileName) +
                                    '   Duration: ' +
                                    MSecToStr(((mfpControl.uiDuration div 1000) - (mfpControl.CurrentPosition)), false) +
                                    ' / ' + MSecToStr(mfpControl.CurrentPosition, True));
                   end;
    Paused:       SetWindowText(m_hwndEvent,
                                'Paused.');

    Stopped:      SetWindowText(m_hwndEvent,
                                'Stopped.');

    Closed:       SetWindowText(m_hwndEvent,
                                'Session closed.');

    Seeking:      SetWindowText(m_hwndEvent,
                                'Starting at position ' + MSecToStr(mfpControl.StartPosition div 1000, false));
   end;
end;


procedure TMfPlayerX.SetFileName(aValue: WideString);
begin
  FFileName:= aValue;
end;


procedure TMfPlayerX.SetSubtitleLanguage(aValue: string);
begin
  FSubtitleLanguage := aValue;
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
                      if SUCCEEDED(Stop()) then
                        State:= Stopped;
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
  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

finalization
  // Not needed, but can't harm as well.
  MFShutdown();
  CoUnInitialize();

end.
