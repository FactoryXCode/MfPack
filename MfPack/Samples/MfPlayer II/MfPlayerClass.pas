// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfpPlayerClass.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This is the basic class of MfPlayer,
//              containing the necessary methodes to play a mediafile
//              For indepth information see the included examples (CPlayer)
//              and text files containing the complete information about
//              MfPlayer.
//
// Organisation: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
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

unit MfPlayerClass;

  {$TYPEINFO ON}

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ComBaseApi,
  {vcl}
  VCL.Graphics,
  VCL.ExtCtrls,
  {system}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  {Application}
  UniThreadTimer;

  {$HINTS OFF}    // Disable Hint H2077, HResult return values are implemented for debug purposes.

const
  WM_APP_PLAYER_EVENT = WM_APP + 1;
  WM_PROGRESSNOTIFY = WM_APP + 101;

type

  TRedrawStatus = (rdStarted,
                   rdStopped);

  TRequest = (reqNone = 0,
              reqStop,
              reqStart,           // start after Pause or stopped
              reqPause,
              reqSeek,
              reqClose);

  TPlayerState = (Closed = 0,    // No session.
                  Ready,         // Session was created, ready to open a file.
                  OpenPending,   // Session is opening a file.
                  Starting,      // Session initializing Start
                  Started,       // Session is playing a file.
                  Pausing,       // Session initializing pause.
                  Paused,        // Session is paused.
                  Stopping,      // Session initializing Stop.
                  Stopped,       // Session is stopped (ready to play).
                  Closing,       // Application has closed the session, but is waiting for MESessionClosed.
                  Seeking);      // Session seeks

  TController = record
    Request: TRequest;          // Request an action
    State: TPlayerState;        // current state the player is in
    dwSessionCaps: DWORD;       // Session caps.
    fRate: FLOAT;               // Playback rate
    fMaxRateSupported: FLOAT;   // Maximum rate that is supported
    fMinRateSupported: FLOAT;   // Minimum rate that is supported
    bCanThinPb: boolean;        // Thinned playback, if supported
    bCanScrub: boolean;         // Scrubbing, if supported
    StartPosition: MFTIME;      // Start from this position
    CurrentPosition: MFTIME;    // Current position
    uiDuration: MFTIME;         // Duration
    uiResumingPlayTime: MFTIME; // Resuming playtime
    Volume: FLOAT;              // Volume
  end;


type

  // Note: We don't use the TInterfacedObject, because we don't want a reference counting mechanism.
  // TInterfacedPersistence does not have a reference counting mechanism.
  // It delegates it to it's owner and that is what we want.
  // You may read some more about this issue here:
  // https://www.codeproject.com/Articles/1252175/Fixing-Delphis-Interface-Limitations
  // We always free and nil TInterfacedPersistent when closing the app or otherwise.
  // for this you call FInterfacedPersistent := Free and  FInterfacedPersistent := nil or
  // use FreeAndNil(FInterfacedPersistent);
  TMfPlayer = class(TInterfacedPersistent, IMFAsyncCallback)
  private
  {private fields}
    // Internal objects
    stRedrawStatus:      TRedrawStatus;

    m_dWaitResult:       DWORD;
    m_iTimerInterval:    cardinal;          // Timer interval
    m_bPending:          BOOL;              // Is a request pending?
    FFileName:           PWideChar;         // filename incl path
    m_dCaps:             DWord;             // MFSESSIONCAP_* (MfApi) = Session caps.
    FTimer:              TUniThreadedTimer; // timer
    mfpControl:          TController;       // Contains requests, state etc.

  {private methods}
    //--------------------------------------------------------------------------
    // IMFAsyncCallback methods
    // ========================
    //
    // Provides configuration information to the dispatching thread for a callback
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;
    // Called when an asynchronous operation is completed.
    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
    //
    //--------------------------------------------------------------------------

    // Callback timer
    procedure TimerTimer(sender: TObject);
    procedure SetTimerInterval(val: cardinal);

    // This function is called by public SetPosition.
    function SetPositionInternal(tPos: MFTIME): HRESULT;
    procedure GotoNewPosition(val: MFTIME);

    // Screen settings
    procedure SetVideoScreen(val: HWND);
    function GetVideoScreen(): HWND;

   // procedure SetFullScreen(val: LongBool); deprecated 'This API is not supported and is unavailable since Redstone 5. ';
   // function IsFullScreen(): LongBool;

    function UpdatePendingCommands(req: TRequest): HRESULT;

    procedure UpdateCaption();

    // conversions
    function GetFileName(): string;
    procedure SetFileName(val: string);

    // Clean up
    procedure ResetController();
    function Clear(): HRESULT;

    // Status indicators
    function GetState(): TPlayerState;

  protected
  {protected fields}

    FhCloseEvent:        THandle;         // Event to wait on while closing.
    // interfaces
    m_pSession:          IMFMediaSession;           // Media Session
    m_pSource:           IMFMediaSource;            // Media Source
    m_pVideoDisplay:     IMFVideoDisplayControl;    // Video control
    m_pClock:            IMFPresentationClock;      // Presentation Clock
    m_pRate:             IMFRateControl;            // Rate control
    m_pRateSupport:      IMFRateSupport;            // Rate support

    m_hwndEvent:         HWND;           // App window to receive events.
    m_hwndVideo:         HWND;           // Video window.
    m_hwndSub:           HWND;           // control that receives the subtitles or other info
    m_hwnd_MainForm:     HWND;           // Handle of the mainform


  {protected methods}

    // Initialize the player
    function Initialize(): HRESULT;
    // close instance of the media session
    function CloseSession(): HRESULT;
    // create a new instance of the media session
    function CreateSession(): HRESULT;


    //---------------------------------------------------------------
    // Media event handlers
    //
    // Handler for OnTopologyStatus event
    function OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
    // Handler for OnTopologyReady event
    function OnTopologyReady(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MEEndOfPresentation event
    function OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
    // Override to handle additional session events.
    function OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
    // Handler for MENewPresentation event.
    // This event is sent if the media source has a new presentation, which
    // requires a new topology.
    function OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
    // Session handlers
    function OnSessionStart(pEvent: IMFMediaEvent): HRESULT;  // request Start event
    function OnSessionPause(pEvent: IMFMediaEvent): HRESULT;  // request Pause event
    function OnSessionStop(pEvent: IMFMediaEvent): HRESULT;   // request Stop event

  public
  {public fields}

  {public methods}

    // Constructor, destructor
    // These are not protected methodes in Delphi.
    constructor Create(hwndVideo: HWND;
                       hwndSub: HWND;
                       hwndEvent: HWND;
                       hwndMainForm: HWND); reintroduce;
    // Handle stuff before reaching Destroy
    procedure BeforeDestruction(); override;
    destructor Destroy(); override;

    // playback
    function OpenURL(sURL: PWideChar): HRESULT;

    // play, pause, stop (Use SendPlayerCmd())
    function Start(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    // Shut down the session and MF
    // Use this funtion to kill the MfPlayer.
    function ShutDown(): HRESULT;
    // Event handler
    function HandleEvent(pEventPtr: UINT_PTR ): HRESULT;

    // Video functionality
    function Repaint: HRESULT;
    // Resizes the video rectangle.
    // The application calls this method if the sizes of the video window changes;
    // e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(dRect: LPRECT): HRESULT;
    // Check if stream contains video
    function HasVideo(): boolean;
    // Start or stops redrawing surfaces (anti flicker)
    procedure SetRedraw();
    // function to control the player
    procedure SendPlayerRequest(req: TRequest);
    // Frame capture
    function TakeSnapShot(var bit: TBitMap): HRESULT;
    // Volume
    procedure SetVolume(Value: FLOAT);
    function GetVolume(): FLOAT;
    // Get the duration if the mediafile
    function GetInitialDuration(out dur: MFTIME): HRESULT;
    // Sets the current playback position (calls SetPositionInternal).
    function SetPosition(hnsPosition: MFTIME): HRESULT;
    function GetPosition(out hnsPosition: MFTIME): HRESULT;

    // Properties
    property FileName: string read GetFileName write SetFileName;
    property IntTimer: TUniThreadedTimer read FTimer write FTimer;
    property TimerInterval: cardinal read m_iTimerInterval write SetTimerInterval default 100;
    property Volume: FLOAT read mfpControl.Volume write SetVolume;
    property Duration: MFTIME read mfpControl.uiDuration;
    property SetNewPosition: MFTIME write GotoNewPosition;
    property Position: MFTIME read mfpControl.CurrentPosition;
    property SetVideoSurface: HWND read GetVideoScreen write SetVideoScreen;
    // property FullScreen: LongBool read IsFullScreen write SetFullScreen;  See comment @ IMFVideoDisplayControl
    property State: TPlayerState read GetState;
  end;

var
  MfPlayer: TMfPlayer;


implementation

// IMFAsyncCallback ////////////////////////////////////////////////////////////



function TMfPlayer.GetParameters(out pdwFlags: DWord;
                                 out pdwQueue: DWord): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfPlayer.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  pEvent: IMFMediaEvent;
  hr: HRESULT;
  meType: MediaEventType;

label
  done;

begin
  hr := E_FAIL;

  if not Assigned(m_pSession) then
    begin
      Result := hr;
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

  if meType = MESessionClosed then
    begin
      // If the session is closed, the application is waiting on the event
      // handle. Also, do not request any more events from the session.
      SetEvent(THandle(FhCloseEvent));
    end
  else
    begin
      // For all other events, ask the media session for the
      // next event in the queue.
      hr := m_pSession.BeginGetEvent(Self,
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

  if (mfpControl.State <> Closing) then
    // Send event message
    HandleEvent(UINT_PTR(pEvent));

done:
  Result := hr;
end;


////////////// TMFPlay /////////////////////////////////////////////////////////

// Reset to initial values
procedure TMfPlayer.ResetController;
begin
  with mfpControl do
    begin
      State := Closed;           // No session.
      Request := reqNone;        // No request
      fRate := 1.0;
      fMaxRateSupported := 1.0;  // Maximum rate that is supported
      fMinRateSupported := 1.0;  // Minimum rate that is supported
      bCanThinPb := False;       // Thinned playback, if supported
      bCanThinPb := False;
      bCanScrub := False;
      StartPosition := 0;
      uiDuration := 0;
      uiResumingPlayTime := 0;   // Resuming playtime
      Volume := 1.0;             // Set to the max
    end;
end;


function TMfPlayer.Clear(): HRESULT;
begin
  Result := S_OK;

  if Assigned(FTimer) then
    FTimer.Enabled := False;

  ResetController();

  // reset
  m_bPending := False;
  m_dCaps := MFSESSIONCAP_START;
  FFileName := '';
  stRedrawStatus := rdStarted;

try
  // Release the interfaces
  // Stop the session
  if Assigned(m_pSession) then
    begin
      m_pSession.Close();
      // Always shut down a session and clear the topologies to release MfAsyncCallback,
      // before releasing the session!
      m_pSession.Shutdown();
      m_pSession.ClearTopologies();
      SafeRelease(m_pSession);
    end;
  SafeRelease(m_pSource);
  SafeRelease(m_pVideoDisplay);
  SafeRelease(m_pClock);
  SafeRelease(m_pRate);
  SafeRelease(m_pRateSupport);
  FreeAndNil(FTimer);

except
  //
  Result := E_FAIL;
end;
end;


function TMfPlayer.CloseSession(): HRESULT;
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
  m_pVideoDisplay := Nil;

  // First close the media session.
  if Assigned(m_pSession) then
    begin
      mfpControl.State := Closing;

      hr := m_pSession.Close();

      if (SUCCEEDED(hr)) then
        begin
try
          // Wait for the close operation to complete
          dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent), 2000);

          if (dwWaitResult <> STATUS_WAIT_0) {Wait abandoned} then
            m_dWaitResult := dwWaitResult;

except
  // Do nothing
end;
        end;
end;

  // Complete shutdown operations.
  if (SUCCEEDED(hr)) then
    begin
      // Shut down the media source. (Synchronous operation, no events.)
      if Assigned(m_pSource) then
        {Void} m_pSource.Shutdown;

      // Shut down the media session. (Synchronous operation, no events.)
      if Assigned(m_pSession) then
        begin
          {Void} m_pSession.Shutdown();
          {Void} m_pSession.ClearTopologies();
        end;
    end;

  SafeRelease(m_pSource);
  SafeRelease(m_pSession);

  mfpControl.State := Closed;

done:
  Result := hr;
end;


procedure TMfPlayer.SetTimerInterval(val: cardinal);
begin
  if (val < 1) or (val > 1000000000) then
    val := 1; //reset to default
  FTimer.Period := val;
end;


constructor TMfPlayer.Create(hwndVideo: HWND;
                             hwndSub: HWND;
                             hwndEvent: HWND;
                             hwndMainForm: HWND);
var
  hr: HRESULT;

begin
  inherited Create();
  // Get the OS architecture (32 or 64 bit)
  //ThisOSArchitecture := GetOSArchitecture();

  CoInitializeEx(Nil, COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' +
                       IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort;
    end;

  m_hwndVideo := hwndVideo;
  m_hwndSub := hwndSub;
  m_hwndEvent := hwndEvent;
  m_hwnd_MainForm := hwndMainForm;

  Clear();

  // init a handle for the events
  hr := Initialize();

  if FAILED(hr) then
    begin
      MessageBox(0,
                 PWideChar('An error occured while initializing MESessionClosed'),
                 PWideChar('Error!'),
                 MB_ICONEXCLAMATION);
     end;

  // Create a threaded timer
  FTimer := TUniThreadedTimer.Create(Nil);
  FTimer.Period := 100;
  FTimer.OnTimerEvent := TimerTimer;
end;


// BeforeDestruction, from here all reference counting should be checked
procedure TMfPlayer.BeforeDestruction();
begin
  Clear();
  inherited BeforeDestruction();
end;


// The destructor
destructor TMfPlayer.Destroy();
begin
  // Shutdown the Media Foundation platform
  MFShutdown();
  CoUnInitialize();
  inherited Destroy();
end;

// Before the application exits, shut down the Media Session,
// and then call MFShutdown to shut down the Microsoft Media Foundation platform.

// Release all resources held by this object.
function TMfPlayer.ShutDown(): HRESULT;
var
  hr: HRESULT;

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
  //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  hr := S_OK;
try

  if SUCCEEDED(Self.Stop()) then
    hr := CloseSession(); // Close the session

  if (FhCloseEvent <> 0) then
    begin
      if CloseHandle(THandle(FhCloseEvent)) then
        begin
          FhCloseEvent := 0;
          hr := S_OK;
        end
      else
        hr := E_FAIL;
    end;
  Result := hr;
except
  Result := hr;
  // Do nothing
end;
end;


function TMfPlayer.CreateSession(): HRESULT;
var
  hr: HRESULT;

label
  Done;

begin
  // Close the old session, if any.
  hr := CloseSession();
  if (FAILED(hr)) then
    goto done;

  assert(mfpControl.State = Closed);

  // Create the media session.
  hr := MFCreateMediaSession(Nil,
                            m_pSession);
  if (FAILED(hr)) then
    goto done;

  mfpControl.State := Ready;

  // Start pulling events from the media session
  hr := m_pSession.BeginGetEvent(Self,
                                 Nil);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;

// Take a snapshot
function TMfPlayer.TakeSnapShot(var bit: TBitMap): HRESULT;
var
  buffer, data: PByte;
  bufSize: DWORD;
  i: Integer;
  bmi: BITMAPINFOHEADER;
  timestamp: MFTIME;
  hr: HRESULT;

begin
  hr := S_OK;

try

  Assert(bit <> nil);  // Debug

  // Set the biSize member of the structure to sizeof(BITMAPINFOHEADER)
  ZeroMemory(@bmi, sizeof(BITMAPINFOHEADER));
  bmi.biSize := sizeof(BITMAPINFOHEADER);

  data := Nil;
  buffer := Nil;
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
          Result := hr;
          Exit;
        end;
      data := buffer;
    end;

  if (bmi.biSizeImage > 0) and (data <> nil) then
    begin
      // Adjustments
      Bit.PixelFormat := pf32bit;
      Bit.SetSize(abs(bmi.biWidth),
                  abs(bmi.biHeight));
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
  Buffer := Nil;
  Result := hr;
end;
end;


// Gets the duration of the current presentation.
function TMfPlayer.GetInitialDuration(out dur: MFTIME): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  hr: HRESULT;
  mdur: MFTIME;

begin
  hr := S_OK;

try
  if Assigned(m_pSource) then
    begin
      dur := 0;
      hr := m_pSource.CreatePresentationDescriptor(pPD);

      if (SUCCEEDED(hr)) then
        begin
          hr := pPD.GetUINT64(MF_PD_DURATION, UINT64(mdur));
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


function TMfPlayer.GetState(): TPlayerState;
begin
  Result := mfpControl.State;
end;


function TMfPlayer.GetVolume(): FLOAT;
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  aCurrVolume: array of Float;
  mv: FLOAT;
  hr: HRESULT;  // Debug
  i: integer;

begin
try

  hr := (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                 IID_IMFAudioStreamVolume,
                                                 Pointer(pVol));

  if SUCCEEDED(hr) then
    begin
      hr := pVol.GetChannelCount(nChannels);
      // If balanced volume is needed, use an array f channels
      // to set each channel on desired volume.
      hr := pVol.GetChannelVolume(0,
                                  mfpControl.Volume);


      SetLength(aCurrVolume, nChannels);
      // this function returns a Float array.
      hr := pVol.GetAllVolumes(nChannels,
                               @aCurrVolume[0]);


      // Find the highest soundlevel (because each channel can have a different level)
      mv := aCurrVolume[0];
      for i := 1 to nChannels -1 do
        begin
          if (aCurrVolume[i] >= mv) then
            begin
              mfpControl.Volume := aCurrVolume[i];
              mv := aCurrVolume[i];
            end;
        end;
    end;

  if FAILED(hr) then
    mfpControl.Volume := 0.0;

finally
  Result := mfpControl.Volume;
end;
end;


function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;


function TMfPlayer.Initialize(): HRESULT;
var
  h: THandle;

begin
  Result := S_OK;
  if (FhCloseEvent <> 0) then
    begin
      Result := MF_E_ALREADY_INITIALIZED;
      Exit;
    end;

  h := CreateEvent(Nil,
                   False,
                   False,
                   Nil);

  FhCloseEvent := THandle(h);

  if (FhCloseEvent = 0) then
    begin
      Result := GetLastError();
    end;
end;


function TMfPlayer.HandleEvent(pEventPtr: UINT_PTR): HRESULT;
var
  hrStatus: HRESULT;
  meType: MediaEventType;
  pEvent: IMFMediaEvent;
  hr: HRESULT;
  _pvar: PROPVARIANT;

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
  hr := pEvent.GetType(meType);
  if (FAILED(hr)) then
    goto done;

  // Get the event status. If the operation that triggered the event
  // did not succeed, the status is a failure code.
  hr := pEvent.GetStatus(hrStatus);

  // Check if the async operation succeeded.
  if (SUCCEEDED(hr) and FAILED(hrStatus)) then
    hr := hrStatus;

  if (FAILED(hr)) then
    goto done;

  case meType of
    // In this order when opening
    MESessionTopologySet            : {OnSessionTopologySet;} hr := S_OK;

    // Raised by the Media Session when a new presentation starts.
    // This event indicates when the presentation will start and the offset between the presentation time and the source time.
    MESessionNotifyPresentationTime : hr := S_OK;  //

    MESessionCapabilitiesChanged    : // The session capabilities changed. Get the updated capabilities.
                                      mfpControl.dwSessionCaps := MFGetAttributeUINT32(pEvent,
                                                                                      MF_EVENT_SESSIONCAPS,
                                                                                      mfpControl.dwSessionCaps);
    MESessionTopologyStatus         : hr := OnTopologyStatus(pEvent);
    // end

    MEEndOfPresentation    : hr := OnPresentationEnded(pEvent);
    MENewPresentation      : hr := OnNewPresentation(pEvent);
    MESessionStopped       : hr := OnSessionStop(pEvent);
    MESessionStarted       : hr := OnSessionStart(pEvent);
    MESessionPaused        : hr := OnSessionPause(pEvent);
    MESessionClosed        : hr := S_OK;
    MESessionEnded         : hr := S_OK;

    MESessionRateChanged   : // If the rate change succeeded, we've already got the rate
                             // cached. If it failed, try to get the actual rate.
                             if FAILED(hrStatus) then
                               begin
                                 PropVariantInit(_pvar);
                                 hr := pEvent.GetValue(_pvar);
                                 if (SUCCEEDED(hr) And (_pvar.vt = VT_R4) ) then
                                   mfpControl.fRate := _pvar.fltVal;
                               end;

  else
    hr := OnSessionEvent(pEvent, meType);
  end;

done:
  Result := hr;
end;


function TMfPlayer.HasVideo: boolean;
begin
  Result := m_pVideoDisplay <> nil;
end;


function TMfPlayer.OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
var
  status: MF_TOPOSTATUS;
  hr: HRESULT;

begin
  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS, UINT32(status));
  if SUCCEEDED(hr) then
    if (status = MF_TOPOSTATUS_READY) then
      hr := OnTopologyReady(pEvent);  // Send msg we are ready
  Result := hr;
end;

// Handler for MESessionTopologyReady event - starts video playback.
function TMfPlayer.OnTopologyReady(pEvent: IMFMediaEvent):  HRESULT;
var
  hr: HRESULT;

begin
  // release any previous instance of the m_pVideoDisplay interface
  SafeRelease(m_pVideoDisplay);
  hr := S_OK;

try
try
  // Ask the session for the IMFVideoDisplayControl interface. This interface is
  // implemented by the EVR (Enhanced Video Renderer) and is exposed by the media
  // session as a service. The session will query the topology for the right
  // component and return this EVR interface. The interface will be used to tell the
  // video to repaint whenever the hosting window receives a WM_PAINT window message.
  // This call is expected to fail if the media file does not have a video stream.
  MFGetService(m_pSession,
               MR_VIDEO_RENDER_SERVICE,
               IID_IMFVideoDisplayControl,
               Pointer(m_pVideoDisplay));
  if (FAILED(hr)) then
    Abort; //silent exception

  // Set the target window (or control), at this point this is not really
  // nescesarry, because the previous -MfGetService- did that allready.
  m_pVideoDisplay.SetVideoWindow(m_hwndVideo);

  // Adjust aspect ratio
  if Assigned(m_pVideoDisplay) then
    begin
      ResizeVideo(nil);
    end;

  // since the topology is ready, you might start playback.
  //
  // You can also implement an option for the user, starting playback directly after
  // a mediastream is loaded.
  //hr := Play();


except
  // Do nothing
end;
finally
  Result := hr;
end;
end;

// Handler for MENewPresentation event.
// This event is sent if the media source has a new presentation, which
// requires a new topology.
function TMfPlayer.OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  pTopology: IMFTopology;
  hr: HRESULT;
  dwSourceStreams: DWORD;

label
  done;

begin
  dwSourceStreams := 0;

  // Get the presentation descriptor from the event.
  hr := GetEventObject(pEvent, pPD);
    if (FAILED(hr)) then
      goto done;
  // Create a partial topology.
  hr := CreatePlaybackTopology(m_pSource,
                              pPD,
                              m_hwndVideo,
                              pTopology,
                              dwSourceStreams);
    if (FAILED(hr)) then
      goto done;
  // Set the topology on the media session.
  hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                              pTopology);

  mfpControl.State := OpenPending;

done:
  Result := hr;  //S_OK
end;


function TMfPlayer.OnSessionPause(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HResult;

begin
  mfpControl.Request := reqPause;
  hr := UpdatePendingCommands(mfpControl.Request);
  if SUCCEEDED(hr) then
    begin
      mfpControl.State := Paused;
      Result := hr;
    end
  else
    Result := hr;
end;


function TMfPlayer.OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
begin
  Stop();
  // The session puts itself into the stopped state automatically.

  Result := S_OK;
end;


function TMfPlayer.OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
begin
  //this more or less a dummy
  //but feel free to add your own sessionevent handlers here
  Result := S_OK;
end;


function TMfPlayer.OnSessionStart(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(reqStart);
  Result := S_OK;
end;


function TMfPlayer.OnSessionStop(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(reqStop);
  Result := S_OK;
end;


// The main routine to play a mediafile
function TMfPlayer.OpenURL(sURL: PWideChar): HRESULT;
var
  pTopology: IMFTopology;
  pSourcePD: IMFPresentationDescriptor;
  pClock: IMFClock;
  hr: HRESULT;
  pwInt: PWideChar;
  itime: system.Int64;
  dwSourceStreams: DWORD;

label
  done;

begin

  // 1. Create a new media session.
  // 2. Create the media source.
  // 3. Create the topology.
  // 4. Queue the topology [asynchronous]
  // 5. Start playback [asynchronous - does not happen in this method.]

  hr := S_OK;
  dwSourceStreams := 0;

try

  FFilename := sURL;

  // Create the media session.
  hr := CreateSession();
  if (FAILED(hr)) then
    goto done;

  //  Create a media object from a URL or stream.
  hr := CreateObjectFromUrl(sURL,
                            m_pSource);
  if (FAILED(hr)) then
    goto done;

  // Create a new topology.
  hr := MFCreateTopology(pTopology);
  if (FAILED(hr)) then
    goto done;

  // Create the presentation descriptor for the media source
  hr := m_pSource.CreatePresentationDescriptor(pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // obtain capabilities of the current session
  hr := m_pSession.GetSessionCapabilities(m_dCaps);
  if FAILED(hr) then
    m_dCaps := MFSESSIONCAP_START;

  // Create the presentation clock (optional)
  //hr := MFCreatePresentationClock(m_pClock);
  //if FAILED(hr) then
  //  m_pClock := Nil
  //else
  //  begin
  //    hr := m_pClock.AddClockStateSink(m_pStateSink);
  //
  //    hr := m_pClock.GetTimeSource(m_pTimeSource);
  //    OleCheck(hr);
  //    if FAILED(hr) then
  //      m_pClock := Nil;  // no clock
  //  end;

  // Get the presentation clock (optional)
  hr := m_pSession.GetClock(pClock);
    if (SUCCEEDED(hr)) then
      begin
        hr := pClock.QueryInterface(IID_IMFPresentationClock, m_pClock);
          if FAILED(hr) then
            m_pClock := Nil;
      end;


  //make your choice: Partial or full topology

  // Create a partial topology, com branches, source nodes and sink nodes.
  // playback topologies are always partial
  hr := CreatePlaybackTopology(m_pSource,
                               pSourcePD,
                               m_hwndVideo,
                               pTopology,
                               dwSourceStreams);
  if (FAILED(hr)) then
    goto done;

  // Set the topology on the media session.
  hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                              pTopology);
  if (FAILED(hr)) then
    goto done;

  // If SetTopology succeeds, the media session will queue an
  // MESessionTopologySet event.

  // Get the duration from the presentation descriptor (optional)
  pSourcePD.GetUINT64(MF_PD_DURATION,
                      UINT64(itime));
  mfpControl.uiDuration := itime;

  // Set state to "open pending"
  mfpControl.State := OpenPending;
  m_bPending := True;

done:

  Result := hr;

  if (FAILED(hr)) then
    begin
      Clear();
      Abort; // throw silent exception
    end;

except
  pwInt := PWideChar(InttoStr(hr));
  Result := hr;
  MessageBox(0,
             PWideChar('An error has been returned. (hr: ' + pwInt + ')'),
             PWideChar(''),
             MB_ICONERROR);
end;
end;

// Pause
function TMfPlayer.Pause(): HRESULT;
var
  hr: HRESULT;

begin
  hr := E_FAIL;
  if (m_pSession = nil) or (m_pSource = nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if m_bPending then
    begin
      mfpControl.Request := reqPause;
      mfpControl.State := Pausing;
    end
  else
    begin
      hr := m_pSession.Pause();
      mfpControl.Request := reqNone;
      mfpControl.State := Paused;
      m_bPending := true; //BOOL(CMD_PENDING);
    end;
  Result := hr;
end;


// Start playback from the current position.
//==========================================
function TMfPlayer.Start(): HRESULT;
var
  hr: HRESULT;
  varStart: PROPVARIANT;

begin
   hr := S_OK;

try
  // Use assertions in debug modus only!
  Assert(m_pSession <> Nil);

  if m_bPending then
    begin
      mfpControl.Request := reqStart;
      mfpControl.State := Starting;
    end
  else
    begin
      PropVariantInit(varStart);
      varStart.vt := VT_EMPTY;

      hr := m_pSession.Start(GUID_NULL, varStart);

      // The Start method can also specify a starting position relative to the start
      // of the file; see the API reference topic for more information.

      m_bPending := True;
      hr := PropVariantClear(varStart);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // Note: Start is an asynchronous operation. However, we
      // can treat our state as being already started. If Start
      // fails later, we'll get an MESessionStarted event with
      // an error code, and we will update our state then.

      // Start the presentation clock
      // 1 sec = 10.000.000 nanosec
      // MF is using 100 ns chunks = 1.000.000 of a second
      hr := m_pClock.Start(0);

      mfpControl.Request := reqNone;
      mfpControl.State := Started;
      mfpControl.uiResumingPlayTime := 0;
      FTimer.Enabled := true;

      // Get initial volume
      GetVolume();
    end;

finally
  Result := hr;
end;
end;

// Repaint the video window.
// Call this method on WM_PAINT from the form where the video is playing on.
function TMfPlayer.Repaint(): HRESULT;
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


function TMfPlayer.ResizeVideo(dRect: LPRECT): HRESULT;
var
  rcpdest, rpcSrc: LPRECT;
  rc: TRect;
  hr: HResult;  //debug purpose

begin
  hr := E_NOINTERFACE;
  rcpdest := Nil;
  rpcSrc := dRect;

  if Assigned(m_pVideoDisplay) then
    begin
      // Stop repaint
      SetRedraw();
      // Set the destination rectangle.
      // If dRect is empty; use the GetClientRect function
      if (rpcSrc = Nil) then
        begin
          WinApi.Windows.GetClientRect(m_hwndVideo, rc);
          CopyTRectToLPRect(rc,
                            rcpdest);
        end
      else
        rcpDest := rpcSrc;

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
  rpcSrc := Nil;
  Result := hr;
end;


procedure TMfPlayer.SetRedraw();
begin

  //Stop flickering of controls and subtitle when resizing.
  if (stRedrawStatus = rdStarted) then
    begin
      SendMessage(m_hwnd_MainForm, WM_SETREDRAW, WPARAM(False), 0);
      stRedrawStatus := rdStopped;
    end
  else
    begin
      SendMessage(m_hwnd_MainForm,
                  WM_SETREDRAW,
                  WPARAM(True), 0);

      RedrawWindow(m_hwnd_MainForm,
                   Nil,
                   0,
                   RDW_ERASE OR RDW_FRAME OR RDW_INVALIDATE OR RDW_ALLCHILDREN);

      stRedrawStatus := rdStarted;
    end;
end;


procedure TMfPlayer.SendPlayerRequest(req: TRequest);
begin

  mfpControl.Request := req;

  case req of
    reqNone:           mfpControl.State := Closed;
    reqStop:           mfpControl.State := Stopping;
    reqStart:          mfpControl.State := Starting;
    reqPause:          mfpControl.State := Pausing;
    reqSeek:           mfpControl.State := Seeking;
    reqClose:          mfpControl.State := Closing;
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
  Closing        // Shutting down
  Seeking        // Session seeks
  }

  UpdatePendingCommands(req);
end;


// Sets the current playback position.
function TMfPlayer.SetPosition(hnsPosition: MFTIME): HRESULT;
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
      hr := SetPositionInternal(hnsPosition);
    end;

  //UpdatePendingCommands(reqSeek);
  Result := hr;
end;


// This function is called by SetPosition.
function TMfPlayer.SetPositionInternal(tPos: MFTIME): HRESULT;
var
  varStart: PROPVARIANT;
  hr: HRESULT;

begin
  hr := E_FAIL;

  if (m_pSession = nil) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

try

  PropVariantInit(varStart);
  varStart.vt := VT_I8;

  //test
  //varStart.hVal.QuadPart := 5654567865;  //this works
  varStart.hVal.QuadPart := tPos;


  hr := m_pSession.Start(GUID_NULL,
                        varStart);

  // The Start method can also specify a starting position relative to the start
  // of the file; see the API reference topic for more information.

  if (SUCCEEDED(hr)) then
    begin
      // Store the pending state
      mfpControl.StartPosition := tPos;
      mfpControl.Request := reqNone;
      mfpControl.State := Started;
      m_bPending := true;
      UpdateCaption;
    end;

finally
  PropVariantClear(varStart);
  Result := hr;
end;
end;


procedure TMfPlayer.GotoNewPosition(val: MFTIME);
begin
  if val >= 0 then
    mfpControl.StartPosition := val;
end;


procedure TMfPlayer.SetVideoScreen(val: HWND);
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.SetVideoWindow(val);
end;


function TMfPlayer.GetVideoScreen(): HWND;
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.GetVideoWindow(Result);
end;

{
procedure TMfPlayer.SetFullScreen(val: LongBool);
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.SetFullscreen(val);
end;
}
{
function TMfPlayer.IsFullScreen(): LongBool;
begin
  if Assigned(m_pVideoDisplay) then
    m_pVideoDisplay.GetFullscreen(Result);
end;
}

function TMfPlayer.GetPosition(out hnsPosition: MFTIME): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

  if (m_pClock = Nil) then
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
  //else if (m_bPending { = BOOL(CMD_PENDING_SEEK)}) then
  //  hnsPosition := mfpControl.StartPosition
  else
    hr := m_pClock.GetTime(hnsPosition);

  Result := hr;
end;


// Set the volume and get the initial volume.
procedure TMfPlayer.SetVolume(Value: FLOAT);
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  fVolume: FLOAT;
  //faVolumes: TFloatArray;
  hr: HRESULT;
  i: integer;

begin

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  fVolume := Value;

  // Set boundaries to prevent overflow
  if fVolume > 1.0 then
    fVolume := 1.0;
  if fVolume < 0.0 then
    fVolume := 0.0;

  hr := (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                IID_IMFAudioStreamVolume,
                                                Pointer(pVol));
  // We could also implement stereo or balanced output.
  // but that is a nice goal for using your own creativity.
  if SUCCEEDED(hr) then
    hr := pVol.GetChannelCount(nChannels);

  // Set the volume for each channel
  if SUCCEEDED(hr) then
    begin
      for i := 0 to nChannels - 1 do
        hr := pVol.SetChannelVolume(i, fVolume);
      // Only 1 channel is selected; to do both you could use an array of channels
      // by using the GetAllVolumes function, see example below.
      hr := pVol.GetChannelVolume(0,
                                 mfpControl.Volume);

      // or like this
      //
      // Set the length of the array
      //SetLength(faVolumes, nChannels); // MUST DO!
      //hr := pVol.GetAllVolumes(nChannels,
      //                        faVolumes);
      //if SUCCEEDED(hr) then
      //  begin
      //    for i := 0 to nChannels-1 do
      //      faVolumes[i] := fVolume;
      //
      //    hr := pVol.SetAllVolumes(nChannels,
      //                            faVolumes);
      //  end;
      //
      // NOTE: A dynamic array is a managed type, and so when it's scope ends,
      //       it will be destroyed automaticly, unless the array holds unmanaged types.
      //       In that case each arrayelement should be freed from memory.
      //
    end;
end;


function TMfPlayer.Stop(): HRESULT;
begin
  Result := E_FAIL;

  if Assigned(m_pSession) then
    begin
      if m_bPending then
        begin
          Result := UpdatePendingCommands(mfpControl.Request);
          if SUCCEEDED(Result) then
            begin
              mfpControl.Request := reqStop;
              mfpControl.State := Stopping;
            end;
        end
      else
        begin
          Result := m_pSession.Stop();
          mfpControl.Request := reqNone;
          mfpControl.State := Stopped;
          FTimer.Enabled := false;
          m_bPending := true;
        end;
    end;
end;


// A Timer for some extra duties, like showing progress when playing if
// the source has no time information ()
procedure TMfPlayer.TimerTimer(sender: TObject);
begin
  if (Self.GetState() = Started) then
    begin
      GetPosition(mfpControl.CurrentPosition);
      mfpControl.uiResumingPlayTime := (mfpControl.uiDuration - mfpControl.CurrentPosition);
      // Send msg to the main form, we updated the play position
      SendMessage(m_hwnd_MainForm,
                  WM_PROGRESSNOTIFY,
                  WParam(1),
                  LParam(0));
    end;
 UpdateCaption();
end;


// Called after an operation completes.
// This method executes any cached requests.
procedure TMfPlayer.UpdateCaption;
begin

  case Self.GetState() of
    OpenPending: SetWindowText(m_hwndVideo,
                               'Loaded: ' +
                               ExtractFileName(FFileName));

    Ready:       SetWindowText(m_hwndVideo,
                               'Session is ready.');

    Closing:     SetWindowText(m_hwndEvent,
                               'Closing session...');
    Started:       begin
                      SetWindowText(m_hwndEvent,
                                    'Playing: ' +
                                    ExtractFileName(FFileName) +
                                    '   Duration: ' +
                                    HnsTimeToStr(mfpControl.uiResumingPlayTime, false) +

                                    ' / ' + HnsTimeToStr(mfpControl.CurrentPosition, true));

                      SetWindowText(m_hwndSub,
                                    'Write your own subtitel here');
                    end;
    Paused:       SetWindowText(m_hwndEvent,
                                'Paused.');

    Stopped:      SetWindowText(m_hwndEvent,
                                'Stopped.');

    Closed:       SetWindowText(m_hwndEvent,
                                'Session closed.');

    Seeking:      SetWindowText(m_hwndEvent,
                                'Starting at position ' + HnsTimeToStr(mfpControl.StartPosition, false));
   end;
end;


// some string conversions
function TMfPlayer.GetFileName(): string;
begin
  Result := WideCharToString(FFileName);
end;


procedure TMfPlayer.SetFileName(val: string);
var
 wChars: array[0..MAX_PATH] of WideChar;

begin
  FFileName := StringToWideChar(val,
                               wChars,
                               SizeOf(wChars));
end;


// Called after an operation completes.
// This method executes any cached requests.
function TMfPlayer.UpdatePendingCommands(req: TRequest): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

try

  if (m_bPending) and (mfpControl.Request = req) then
    begin
      m_bPending := False;
      // The current pending command has completed.

      // First look for rate changes.
      {Implement rate issues here -later-}

      // Now look for seek requests.
      if not m_bPending then
        case req of
          reqNone: ; // Nothing to do.
          reqStart: begin
                      if SUCCEEDED(Start()) then
                        mfpControl.State := Started;
                    end;
          reqPause: begin
                      if SUCCEEDED(Pause()) then
                        mfpControl.State := Paused;
                    end;
          reqStop:  begin
                      if SUCCEEDED(Stop()) then
                        mfpControl.State := Stopped;
                    end;
          reqSeek:  SetPositionInternal(mfpControl.StartPosition);
        end;
      mfpControl.Request := reqNone;
    end;

  UpdateCaption();

finally
  Result := hr;
end;
end;

//// End TMfPlayer class ///////////////////////////////////////////////////////

end.
