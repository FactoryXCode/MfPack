// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfDeviceCaptureClass.pas
// Kind: Pascal Unit
// Release date: 08-02-2018
// Language: ENU
//
// Revision Version: 3.1.5
//
// Description: This is the basic class of MfSimpleCapture,
//              containing the necessary methodes to capture media streams.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 20/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Parts of CPreview Examples.
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

unit MfDeviceCaptureClass;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.Ks,
  {System}
  System.Services.Dbt,
  System.SysUtils,
  System.Classes,
  System.Types,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {MfPack}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib;

  {$TYPEINFO ON}

type
    // Capture engine states
    TCeState = (Closing = 0,     // Application has closed the session, but is waiting for MESessionClosed.
                Closed,          // No session.
                Ready,           // Session was created, ready to open a file.
                OpenPending,     // Session is opening a file.
                Starting,        // Session initializing Start
                Started,         // Session is playing a file.
                Stopping,        // Session initializing Stop.
                Stopped,         // Session is stopped (ready to play).
                TopologyReady,   // Session topology has been set.
                CaptureStarting, // Session initializing capturing
                CaptureStarted,  // Session is capturing
                CaptureStopping, // Session initializing stop capturing
                CaptureStopped   // Session is capturing
               );

    TRequest = (reqNone = 0,
                reqStartRecording,
                reqStopRecording);

    TRedrawStatus = (rdStarted,
                     rdStopped);

type

  // Note:
  // Applications should use the Media Session for playback.

  TMfCaptureEngine = class(TInterfacedPersistent, IMFAsyncCallback)
  private
    // Constructor is private. Use static CreateInstance method to instantiate.
    constructor Create(hVideo: HWND; hMainForm: HWND); overload;

  protected

    stRedrawStatus     : TRedrawStatus;
    m_bHasVideo        : LongBool;

    m_pwszSymbolicLink : PWideChar;

    m_cchSymbolicLink  : UINT32;

    m_State            : TCeState;
    m_Request          : TRequest;
    m_bPending         : LongBool;

    dwSessionCaps      : DWord;                     // MFSESSIONCAP_* (MfApi) = Session caps.
    hCloseEvent        : THandle;                   // Event to wait on while closing.
    m_dWaitResult      : DWord;                     // Wait, used by function

    // Handles
    m_hwndEvent:         HWND;                      // App window handle to receive events.
    m_hwndVideo:         HWND;                      // Stored handle of videosurface
    m_hwnd_MainForm:     HWND;                      // Handle to the main form

    // Colorkey needed to draw transparent on the videosurface.
    FBGColor: COLORREF;

    // interfaces
    m_pSession:          IMFMediaSession;           // Media Session
    m_pSource:           IMFMediaSource;            // Media Source
    m_pVideoDisplay:     IMFVideoDisplayControl;    // Video control
    m_pActivate:         IMFActivate;               // The device Activate interface

    m_hdevnotify:        HDEVNOTIFY;


    // Methods

    // create a new instance of the media session
    function CreateSession(): HRESULT;
    // close instance of the media session
    function CloseSession(): HRESULT;

    // Initialize the session
    // Note: IMFPMediaPlayer is deprecated, so we work with sessions, as recommended.
    function Initialize(): HRESULT;
    // Clean up, resets all vars and releases all interfaces
    procedure Clear();

    // Start & stop
    function StartRecording(): HRESULT;
    function StopRecording(): HRESULT;

    //--------------------------------------------------------------------------
    // Media event and session handlers
    //==================================
    // Event handler
    function HandleEvent(pEventPtr: UINT_PTR ): HRESULT;
    //
    // Handler for MESessionTopologyStatus event
    function OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
    // Raised after the IMFMediaSession.SetTopology method completes asynchronously.
    function OnSessionTopologySet(pEvent: IMFMediaEvent): HRESULT;
    // Clears all of the presentations that are queued for playback in the Media Session.
    function OnSessionTopologiesCleared(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MESessionTopologySet event
    function OnTopologyReady(pEvent: IMFMediaEvent): HRESULT; virtual;
    // Override to handle additional session events.
    function OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
    // Handler for MENewPresentation event.
    // This event is sent if the media source has a new presentation, which
    // requires a new topology.
    function OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
    //
    // Session handlers
    function OnSessionStart(pEvent: IMFMediaEvent): HRESULT;  // request Start event
    function OnSessionStop(pEvent: IMFMediaEvent): HRESULT;   // request Stop event

    // Pending commands handler
    function UpdatePendingCommands(req: TRequest): HRESULT;
    //--------------------------------------------------------------------------


    // IMFAsyncCallback methods ------------------------------------------------

    // Implementation of this method is optional.
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HRESULT; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HRESULT; stdcall;

    //--------------------------------------------------------------------------

    // Video functionality
    //
    // Start or stops redrawing surfaces (anti flicker)
    procedure SetRedraw();
    // Checker if videodevice is loaded
    function HasVideo(): BOOL;
    // Get or set the videosurface
    procedure SetVideoScreen(val: HWND);
    function GetVideoScreen(): HWND;
    // Get the video Normalized Rectangle
    function GetVideoRectangle(): TRECT;

  public
    // Use this function to create the capture engine !
    class function CreateInstance(hVideo: HWND;
                                  hMainForm: HWND;
                                  out ppCaptureEngine: TMfCaptureEngine): HRESULT;

    // Before Destroy is called, release the interfaces.
    procedure BeforeDestruction(); override;
    // Destructor is private. Caller should call Release.
    destructor Destroy; override;

    // Release all resources held by this object.
    // Call this from the mainform's OnDestroy
    function ShutDownEngine(): HRESULT;

    // public Video functionality
    //
    // Repaint the video rectangle
    function UpdateVideo(): HResult; // Repaint the video rect
    // Resizes the video rectangle.
    // The application calls this method if the size of the video window changes;
    // e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(dRect: LPRECT): HRESULT;

    // Capture methods
    function SetDevice(DeviceProperty: TDeviceProperties): HRESULT;  //Set the device
    function PrepareSession(): HRESULT;  // Prepares the session for recording
    function VideoDetected(): Boolean; // returns m_bHasVideo;

    property State: TCeState read m_State write m_State;
    property Request: TRequest read m_Request write m_Request;
    property SetVideoSurface: HWND read GetVideoScreen write SetVideoScreen;
    property VideoRectangle: TRECT read GetVideoRectangle;

  end;

var
  MfDeviceCapture: TMfCaptureEngine;
  dpa: TDevicePropertiesArray;

  procedure ShowErrorMessage(hwnd: HWND; fmt: LPCWSTR; hrErr: HRESULT);


implementation


// Create a new instance of the media session
function TMfCaptureEngine.CreateSession(): HRESULT;
var
  hr: HRESULT;

label
  Done;

begin
  // Close the old session, if any.
  hr:= CloseSession();
  if FAILED(hr) then
    goto done;

  assert(m_State = Closed);

  // Create the media session.
  hr:= MFCreateMediaSession(Nil, m_pSession);
  if FAILED(hr) then
    goto done;

  m_State:= Ready;

  // Start pulling events from the media session
  hr:= m_pSession.BeginGetEvent(IMFAsyncCallback(Self), Nil);
  if FAILED(hr) then
    goto done;

done:
  Result:= hr;
end;


// Close session
function TMfCaptureEngine.CloseSession(): HRESULT;
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
  if not Assigned(m_pSession) then
    goto Done;

  State := Closing;

  hr := m_pSession.ClearTopologies();
  if FAILED(hr) then
    goto Done;

  hr:= m_pSession.Close;
  if FAILED(hr) then
    goto Done;

  // Wait for the close operation to complete
  dwWaitResult := WaitForSingleObject(THandle(hCloseEvent), 1000);

  if (dwWaitResult <> STATUS_WAIT_0) {Wait abandoned} then
    m_dWaitResult := dwWaitResult;

  // Complete shutdown operations.
  if SUCCEEDED(hr) then
    begin
      // Shut down the media source. (Synchronous operation, no events.)
      if Assigned(m_pSource) then
        {Void} m_pSource.Shutdown;

      // Shut down the media session. (Synchronous operation, no events.)
      if Assigned(m_pSession) then
        {Void} m_pSession.Shutdown;
    end;

  // Release the global interfaces
  SafeRelease(m_pSession);
  SafeRelease(m_pSource);
  SafeRelease(m_pActivate);
  State := Closed;
  // Unregister device notification
  if not UnRegisterForDeviceNotification(m_hdevnotify) then
    hr := E_POINTER;
done:
  Result := hr;
end;

// NOTE:
// Before the application exits, shut down the Media Session,
// and then call MFShutdown to shut down the Microsoft Media Foundation platform.

// Call CloseSession first before releasing all resources held by this object.
function TMfCaptureEngine.ShutDownEngine(): HRESULT;
var
  hr: HRESULT;

begin
  hr:= S_OK;
  // The application must call Shutdown because the media session holds a
  // reference count on the capture engine object. (This happens when capture engine calls
  // IMediaEventGenerator.BeginGetEvent on the media session.) As a result,
  // there is a circular reference count between the capture engine object and the
  // media session. Calling Shutdown breaks this circular reference count.

  // If CreateInstance failed, the application will not call Shutdown. To
  // handle that case, we must call Shutdown() in the destructor. The
  // circular ref-count problem does not occcur if CreateInstance has failed.
  // Also, calling Shutdown twice is harmless.
  //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

try

  if SUCCEEDED(StopRecording()) then
    hr := CloseSession();  // Close the session

  // Shutdown the Media Foundation platform
  hr := MFShutdown();

  if (hCloseEvent <> 0) then
    begin
      if CloseHandle(THandle(hCloseEvent)) then
        begin
          hCloseEvent := 0;
          hr := S_OK;
        end
      else
        hr := E_FAIL;
    end;
  Result := hr;
except
  // Do nothing
  Result := hr;
end;
end;


function TMfCaptureEngine.Initialize(): HRESULT;
var
  h: THandle;

begin
  if (hCloseEvent <> 0) then
    begin
      Result := MF_E_ALREADY_INITIALIZED;
      Exit;
    end;

  h := CreateEvent(Nil,
                   False,
                   False,
                   Nil);

  hCloseEvent := h;

  if (hCloseEvent = 0) then
    Result := GetLastError()
  else
    Result := S_OK;
end;


// Resets all vars and releases all interfaces
procedure TMfCaptureEngine.Clear();
begin
  // reset vars
  m_bHasVideo := False;
  m_pwszSymbolicLink := Nil;
  m_cchSymbolicLink := 0;
  m_State := Closed ;
  m_Request := reqNone;
  dwSessionCaps := 0;
  m_dWaitResult := 0;
end;


// The constructor
//  hVideo:   Handle to the video window.
//  ppPlayer: Receives an AddRef's pointer to the CPreview object.
//            The caller must release the pointer.
//-------------------------------------------------------------------
constructor TMfCaptureEngine.Create(hVideo: HWND; hMainForm: HWND);
var
  hr: HRESULT;

begin
  inherited Create();

  m_pSource:= Nil;

  m_hwndVideo:= hVideo;
  m_hwnd_MainForm:= hMainForm;
  m_hwndEvent:= hMainForm;

  m_bHasVideo:= False;
  m_pwszSymbolicLink:= Nil;
  m_cchSymbolicLink:= 0;

  hr:= Initialize();

  if FAILED(hr) then
    begin
      MessageBox(0,
                 PWideChar('An error occured while trying to initialize.'),
                 PWideChar('Error!'),
                 MB_ICONEXCLAMATION);
     end;
end;


class function TMfCaptureEngine.CreateInstance(hVideo: HWND;
                                               hMainForm: HWND;
                                               out ppCaptureEngine: TMfCaptureEngine): HRESULT;
var
  pCaptureEngine: TMfCaptureEngine;

begin

  pCaptureEngine := TMfCaptureEngine.Create(hVideo,
                                            hMainForm);

  if (pCaptureEngine = Nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  // The CPreview constructor sets the ref count to 1.
  ppCaptureEngine := pCaptureEngine;

  Result := S_OK;
end;


//
procedure TMfCaptureEngine.BeforeDestruction();
begin
  Clear();
  MFShutdown();
  inherited;
end;


// The destructor
destructor TMfCaptureEngine.Destroy;
begin
  inherited Destroy;
end;




// Start & stop
//
function TMfCaptureEngine.StartRecording(): HRESULT;
var
  hr: HRESULT;
  varStart: PROPVARIANT;
  FVideoProcessor: IMFVideoProcessor;

begin
  hr:= S_OK;

  if Not Assigned(m_pSession) then
    begin
      Result := E_POINTER;
      Exit;
    end;

try

  if m_bPending then
    begin
      Request := reqStartRecording;
      State := Starting;
    end
  else
    begin
      PropVariantInit(varStart);
      varStart.vt := Word(VT_EMPTY);  //Slow when returning from pause!
      hr := m_pSession.Start(GUID_NULL, varStart);
      m_bPending := True;
      hr := PropVariantClear(varStart);
    end;

  if SUCCEEDED(hr) then
    begin
      // Note: StartRecording is an asynchronous operation. However, we
      // can treat our state as being already started. If Start
      // fails later, we'll get an MESessionStarted event with
      // an error code, and we will update our state then.

      Request := reqNone;
      State := Started;

      // check if there is video present
      if (HasVideo = true) then
        begin
          hr := (m_pSession as IMFGetService).GetService(MR_VIDEO_MIXER_SERVICE,
                                                         IID_IMFVideoProcessor,
                                                         Pointer(FVideoProcessor));
          if SUCCEEDED(hr) then
            hr := FVideoProcessor.GetBackgroundColor(FBGColor);
        end
      else
        hr:= E_NOTIMPL;  // better is to implement a custom E_VIDEO_NOT_PRESENT or something like that.
    end;

finally
  Result:= hr;
end;
end;


function TMfCaptureEngine.StopRecording(): HRESULT;
var
  hr: HResult;

begin

  if Assigned(m_pSession) then
    begin
      if m_bPending then
        begin
          Request := reqStopRecording;
          hr := UpdatePendingCommands(Request);
          if SUCCEEDED(hr) then
            begin
              State := Stopped;
            end
          else
            hr := E_FAIL;
        end
      else
        begin
          hr := m_pSession.Stop();
          Clear();
          m_bPending := True;
        end;
    end
  else
    hr := E_POINTER;

  Result := hr;
end;


//-------------------------------------------------------------------
//  HandleEvent  (main event handler)
//
//  This method is called by the capture engine object to send events to
//  the application. For live preview, there are not many events to
//  worry about.
//-------------------------------------------------------------------
function TMfCaptureEngine.HandleEvent(pEventPtr: UINT_PTR): HRESULT;
var
  hrStatus: HRESULT;
  meType: MediaEventType;
  pEvent: IMFMediaEvent;
  hr: HRESULT;

label
  Done;

begin

  hrStatus := S_OK;
  meType := MEUnknown;
  pEvent := IMFMediaEvent(pEventPtr);

  if not Assigned(pEvent) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Get the event type.
  hr:= pEvent.GetType(meType);
  if FAILED(hr) then
    goto done;

  // Get the event status. If the operation that triggered the event
  // did not succeed, the status is a failure code.
  hr := pEvent.GetStatus(hrStatus);

  // Check if the async operation succeeded.
  if (SUCCEEDED(hr) and FAILED(hrStatus)) then
    hr := hrStatus;

  if FAILED(hr) then
    goto done;

  // SESSION EVENTS
  //===============
  case meType of
    // In this order when opening
    MESessionTopologiesCleared      : hr := OnSessionTopologiesCleared(pEvent);
    MESessionTopologySet            : hr := OnSessionTopologySet(pEvent);
    // Raised by the Media Session when a new presentation starts.
    // This event indicates when the presentation will start and the
    // offset between the presentation time and the source time.
    MESessionNotifyPresentationTime : hr := S_OK;
     // The session capabilities changed. Get the updated capabilities.
    MESessionCapabilitiesChanged    : dwSessionCaps := MFGetAttributeUINT32(pEvent,
                                                                            MF_EVENT_SESSIONCAPS,
                                                                            dwSessionCaps);

    MESessionTopologyStatus: hr:= OnTopologyStatus(pEvent);

    // From here the event sequences are mandatory
    MENewPresentation      : hr := OnNewPresentation(pEvent);
    MESessionStopped       : hr := OnSessionStop(pEvent);
    MESessionStarted       : hr := OnSessionStart(pEvent);
    MESessionClosed        : hr := S_OK;
    MESessionEnded         : hr := S_OK;

  else  // Undefined event, handle this by OnSessionEvent handler
    hr := OnSessionEvent(pEvent, DWord(meType));
  end;

done:
  Result:= hr;
end;


//--------------------------------------------------------------------------

// Handler for MESessionTopologyStatus event
function TMfCaptureEngine.OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
var
  status: MF_TOPOSTATUS;
  hr: HRESULT;

begin
  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                         UINT32(Status));
  case Status of
    MF_TOPOSTATUS_READY: OnTopologyReady(pEvent);
    MF_TOPOSTATUS_ENDED: ;
    MF_TOPOSTATUS_INVALID: ;
    MF_TOPOSTATUS_STARTED_SOURCE: ;
    else
      hr := E_UNEXPECTED;
  end;
  Result := hr;
end;


function TMfCaptureEngine.OnSessionTopologySet(pEvent: IMFMediaEvent): HRESULT;
var
  status: MF_TOPOSTATUS;
  hr: HRESULT;

begin
  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                         UINT32(status));
  case status of
    MF_TOPOSTATUS_READY: OnTopologyReady(pEvent);
    MF_TOPOSTATUS_ENDED: {} ;
    MF_TOPOSTATUS_INVALID: {} ;
    MF_TOPOSTATUS_STARTED_SOURCE: {} ;
    else
      hr := E_UNEXPECTED; // possible error  -1072875802 ($C00D36E6) The requested attribute was not found.
  end;                    // this error can be ignored, because probably the topology is not completely set at this point.
  Result := hr;
end;


function TMfCaptureEngine.OnSessionTopologiesCleared(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;
begin
  // Do here what you want, after the topology is cleared
  hr := S_OK;
  Result := hr;
end;


// Handler for MESessionTopologySet event
function TMfCaptureEngine.OnTopologyReady(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;

label
  done;

begin
  // release any previous instance of the m_pVideoDisplay interface
  m_pVideoDisplay := Nil;

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
      //Abort; // silent exception
      // Or continue, without video.
      goto done;
    end;

  // Set the target window (or control), at this point this is not really
  // nescesarry, because the previous -MfGetService- did that allready.
  //{void} m_pVideoDisplay.SetVideoWindow(m_hwndVideo);

  // Adjust aspect ratio
  if Assigned(m_pVideoDisplay) then
    ResizeVideo(Nil);

// Since the topology is ready, you might start capturing, do calculations etc.
//
////////////////////////////////////////////////////////////////////////////////

  // Obtain capabilities of the current session
  if SUCCEEDED(hr) then
    hr := m_pSession.GetSessionCapabilities(dwSessionCaps);

  if FAILED(hr) then
    dwSessionCaps := MFSESSIONCAP_START;

  m_bPending:= True;
  // START RECORDING
  // You can also implement an option for the user, starting playback directly after
  // a mediastream is loaded.
  UpdatePendingCommands(Request);
  //hr := StartRecording();

  State := TopologyReady;

done:
  Result := hr;

end;


// Override to handle additional session events.
function TMfCaptureEngine.OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
begin
  //this is more or less a dummy for now
  //but feel free to add your own sessionevent handlers here
  Result := S_OK;
end;

// Handler for MENewPresentation event.
// This event is sent if the media source has a new presentation, which
// requires a new topology.
function TMfCaptureEngine.OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  pTopology: IMFTopology;
  hr: HRESULT;
  dwSourceStreams: DWORD;

label
  done;

begin
  dwSourceStreams:= 0;

  // Get the presentation descriptor from the event.
  hr := GetEventObject(pEvent,
                       pPD);
    if FAILED(hr) then
      goto done;

  // Create a partial topology.
  hr := CreatePlaybackTopology(m_pSource,
                               pPD,
                               m_hwndVideo,
                               pTopology,
                               dwSourceStreams);
    if FAILED(hr) then
      goto done;

  // Set the topology on the media session.
  hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                               pTopology);

  State := OpenPending;

done:
  Result := hr;
end;


//
// Session handlers
//
// request for Start event
function TMfCaptureEngine.OnSessionStart(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(reqStartRecording);
  Result := S_OK;
end;

// request for Stop event
function TMfCaptureEngine.OnSessionStop(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(reqStopRecording);
  Result := S_OK;
end;


// Called after an operation completes.
// This method executes any cached requests.
function TMfCaptureEngine.UpdatePendingCommands(req: TRequest): HRESULT;
var
  hr : HRESULT;

begin
  hr := S_OK;

try

  if (m_bPending) and (Request = req) then
    begin
      m_bPending := False;
      // The current pending command has completed.

      // Now look for seek requests.
      if not m_bPending then
        case req of
          reqNone: ; // Nothing to do.
          reqStartRecording: begin
                               if SUCCEEDED(StartRecording()) then
                                 State := Started;
                             end;
          reqStopRecording:  begin
                               if SUCCEEDED(StopRecording()) then
                                 State := Stopped;
                             end;
        end;
      // Request is done
      Request := reqNone;
    end;

finally
  Result := hr;
end;
end;


function TMfCaptureEngine.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  pEvent: IMFMediaEvent;
  hr: HRESULT;
  meType: MediaEventType;

label
  done;

begin

  if not Assigned(m_pSession) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Get the event from the event queue.
  hr := m_pSession.EndGetEvent(pAsyncResult, pEvent);
  if FAILED(hr) then
    goto done;

  // Get the event type.
  hr := pEvent.GetType(meType);
  if FAILED(hr) then
    goto done;

  if meType = MESessionClosed then
    begin
      // If the session is closed, the application is waiting on the event
      // handle. Also, do not request any more events from the session.
      SetEvent(THandle(hCloseEvent));
    end
  else
    begin
      // For all other events, ask the media session for the
      // next event in the queue.
      hr := m_pSession.BeginGetEvent(IMFAsyncCallback(Self), Nil);
      if FAILED(hr) then
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


  if (State <> Closing) then
    begin
      // Send event message
      HandleEvent(UINT_PTR(pEvent));
    end;

done:
  Result := hr;
end;

function TMfCaptureEngine.GetParameters(out pdwFlags: DWord;
                                        out pdwQueue: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;

////////////////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------
//  SetDevice
//
//  Sets the capture device source
//
//  pActivate: The activation object for the device source.
//-------------------------------------------------------------------
function TMfCaptureEngine.SetDevice(DeviceProperty: TDeviceProperties): HRESULT;
var
  hr: HRESULT;
  pSource: IMFMediaSource;
  pConfig: IMFAttributes;
  ppDevices: PIMFActivate;  // Pointer to array of IMFActivate
  count: UINT32;

begin
  hr := S_OK;
  count := 0;

  // Release the current instance of the player (if any).
  Clear();
  // Close session and release interfaces
  CloseSession();

  // Prepare the device
  // ==================
  // 1. Create a new session.
  // 2. Create an attribute store.
  // 3. Send a request to get video capture device.
  // 4. Enumerate the devices again (optional)
  // 5. Create a media source from the selected device.
  // 6. Get the symbolic link.
  // 7. Prepare session for recording.
  //

try
  // Create a new session for the player.
  hr := CreateSession();

  // Create an attribute store to hold the search criteria.
  hr := MFCreateAttributes(pConfig,
                           1);

  // Request video capture devices.
  if SUCCEEDED(hr) then
    begin
      hr := pConfig.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            DeviceProperty.riid);
    end;

  // Enumerate the devices again, because in the mean while a device could be
  // disconnected.
  if SUCCEEDED(hr) then
    begin
      hr := MFEnumDeviceSources(pConfig,
                                ppDevices,
                                count);
    end;

{$POINTERMATH ON}

   // Create a media source from the selected device.
  if SUCCEEDED(hr) then
    begin
      if (count > 0) then
        begin
          hr := ppDevices[DeviceProperty.iDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                                                      Pointer(pSource));
          if SUCCEEDED(hr) then
            begin
              m_pActivate := ppDevices[DeviceProperty.iDeviceIndex];
            end;
        end
      else
        begin
          hr := MF_E_NOT_FOUND;
        end;
    end;

  // Get the symbolic link. This is needed to handle device-
  // loss notifications. (See CheckCaptureDeviceLost.)
  //
  // Create a new media item for this media source, lookup by symboliclink.
  if SUCCEEDED(hr) then
    hr := CreateVideoCaptureDeviceBySymolicLink(DeviceProperty.lpSymbolicLink,
                                                pSource);
  // Set source and symboliclink, needed for devicelost
  if SUCCEEDED(hr) then
    begin
      m_pSource := pSource;
      m_pwszSymbolicLink := DeviceProperty.lpSymbolicLink;
      m_cchSymbolicLink := DeviceProperty.iDeviceIndex;
    end;

finally

{$POINTERMATH OFF}

  // Prepare the recording session
  if SUCCEEDED(hr) then
    PrepareSession();

  Result:= hr;
end;
end;


// Prepares the recording session
function TMfCaptureEngine.PrepareSession(): HRESULT;
var
  pTopology: IMFTopology;
  pSourcePD: IMFPresentationDescriptor;
  hr: HRESULT;
  dwSourceStreams: DWORD;

label
  done;

begin
  hr := S_OK;
  dwSourceStreams := 0;

try

  // After the session is prepared (function SetDevice)
  // We need to build the topology and set it on the session,
  // ========================================================
  // 1. Create the topology.
  // 2. Create the presentation descriptor.
  // 3. Create the (partial) source topology.
  // 4. Set the topology on the media session.
  // 5. Start request [asynchronous - does not happen in this method, but in OnTopologySet]
  //


  // Create a new topology.
  hr := MFCreateTopology(pTopology);
  if FAILED(hr) then
    goto done;

  // Create the presentation descriptor for the media source
  hr := m_pSource.CreatePresentationDescriptor(pSourcePD);
  if FAILED(hr) then
    goto done;

  //Clear all of the presentations that are queued for playback in the Media Session.
  hr := m_pSession.ClearTopologies();

  // Create a partial source topology, com branches, source nodes and sink nodes.
  // playback topologies are always partial
  hr := CreatePlaybackTopology(m_pSource,
                               pSourcePD,
                               m_hwndVideo,
                               pTopology,
                               dwSourceStreams);

  if FAILED(hr) then
    goto done;

  // Set the topology on the media session.
  hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                               pTopology);
  if FAILED(hr) then
    goto done;

  // If SetTopology succeeds, the media session will queue an
  // MESessionTopologySet event.
  // This event is evaluated and fired in the OnTopologyStatus method
  // When the method completes, MFPlay will call OnMediaPlayerEvent
  // with the MFP_EVENT_TYPE_MEDIAITEM_CREATED event.

  // obtain capabilities of the current session
  hr := m_pSession.GetSessionCapabilities(dwSessionCaps);
  if FAILED(hr) then
    dwSessionCaps := $0001;

  if SUCCEEDED(hr) then
    begin
      // Register device notification

      //
      hr := GetSymbolicLink(m_pActivate,
                            m_pwszSymbolicLink,
                            m_cchSymbolicLink,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK);

      // Set state to "open pending"
      State := OpenPending;
      // Send a request to start capturing (this is fired in OnTopologyReady)
      Request := reqStartRecording;
    end;

done:

  if FAILED(hr) then
    begin
      Clear();
      Abort; // throw exception
    end;

finally
  Result := hr;
end;

end;


// VIDEO
//
// Repaint the video window.
// Call this method on WM_PAINT from the form where the video is playing on.
function TMfCaptureEngine.UpdateVideo(): HRESULT;
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


function TMfCaptureEngine.ResizeVideo(dRect: LPRECT): HRESULT;
var
  rcpdest: LPRECT;
  rc: TRect;
  hr: HResult;  //debug purpose

begin
  hr := E_NOINTERFACE;

  if Assigned(m_pVideoDisplay) then
    begin
      // Stop repaint
      SetRedraw();
      // Set the destination rectangle.
      // If dRect is empty; use the GetClientRect function
      if (dRect = Nil) then
        begin
          WinApi.Windows.GetClientRect(m_hwndVideo, rc);
          CopyTRectToLPRect(rc, rcpdest);
        end
      else
        rcpDest := dRect;

      hr := m_pVideoDisplay.SetVideoPosition(Nil, rcpdest);

      if SUCCEEDED(hr) then
        // Set aspect ratio in conjunction with SetVideoPosition
        hr := m_pVideoDisplay.SetAspectRatioMode(DWORD(MFVideoARMode_PreservePicture));

      // Start repaint again
      SetRedraw();
    end;
  Result := hr;
end;

// Reduces flickering
procedure TMfCaptureEngine.SetRedraw();
begin

  // Reduce flickering of controls when resizing.
  if (stRedrawStatus = rdStarted) then
    begin
      SendMessage(m_hwnd_MainForm, WM_SETREDRAW, WPARAM(False), 0);
      stRedrawStatus := rdStopped;
    end
  else
    begin
      SendMessage(m_hwnd_MainForm,
                  WM_SETREDRAW,
                  WPARAM(True),
                  0);

      RedrawWindow(m_hwnd_MainForm,
                   Nil,
                   0,
                   RDW_ERASE OR RDW_FRAME OR RDW_INVALIDATE OR RDW_ALLCHILDREN);

      stRedrawStatus := rdStarted;
    end;
end;

// protected
function TMfCaptureEngine.HasVideo(): BOOL;
begin
   Result := (m_pVideoDisplay <> Nil);
end;

//public
function TMfCaptureEngine.VideoDetected(): Boolean;
begin
  m_bHasVideo := HasVideo();
  Result := m_bHasVideo;
end;

// Sets the desired clipping window/control
procedure TMfCaptureEngine.SetVideoScreen(val: HWND);
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.SetVideoWindow(val);
end;


function TMfCaptureEngine.GetVideoScreen(): HWND;
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.GetVideoWindow(Result);
end;


function TMfCaptureEngine.GetVideoRectangle(): TRECT;
var
  rc: TRECT;
  nrc: MFVideoNormalizedRect;

begin
  // we don't need the normalized rect.
  m_pVideoDisplay.GetVideoPosition(nrc, rc);
  CopyTRectToTRect(rc, Result);
end;

////////////////////////////////////////////////////////////////////////////////


// Helpers & tools
//================

//-------------------------------------------------------------------
// ShowErrorMessage
//
// Displays an error message.
//-------------------------------------------------------------------
procedure ShowErrorMessage(hwnd: HWND; fmt: LPCWSTR; hrErr: HResult);
var
  hr: HRESULT;
  msg: String;

begin
  hr := S_OK;
  Msg := System.SysUtils.format('%s  HResult = $%u', [fmt, hrErr]);

  if SUCCEEDED(hr) then
    MessageBox(hwnd,
               lpcwstr(msg),
               lpcwstr('Error'),
               MB_ICONERROR);
end;


procedure Initialize();
var
  hr: HResult;

begin
  hr := MFStartup(MF_VERSION, 0);
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' +
                         IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort;
    end;
end;

//  initialization and  finalizationsection

initialization
  Initialize();

finalization
 // see BeforeDestruction.

end.
