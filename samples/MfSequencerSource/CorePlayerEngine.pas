// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: CorePlayerEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 29-07-2012
// Language: ENU
//
// Revision Version: 2.6.3
// Description: CPlayer example.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 29/01/2020                     Underworld release.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX263
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.18362.0 (19H1)
//
// Todo: -
//
//==============================================================================
// Source: Microsoft CPlayer example, Microsoft Docs.
//         https://docs.microsoft.com/en-us/windows/win32/medfound/how-to-play-unprotected-media-files
//
// Copyright (c) Microsoft Corporation. All rights reserved
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit CorePlayerEngine;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics,
  System.Classes,
  System.SysUtils,
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.MfApi,
  MfPack.MfIdl,
  MfPack.MfObjects,
  MfPack.PropIdl,
  MfPack.Evr,
  MfPack.Evr9,
  MfPack.WinError,
  MfPack.MfError,
  Helpers,
  UniThreadTimer,
  PlayerSeeking;


type

  TDCPlayer = class(TInterfacedPersistent, IMFAsyncCallback)
  private
    PlayerExtension: TPlayerSeeking; // Extension of this class
    FTimer: TUniThreadedTimer; // Timer
    flVolume: Float;           // Volume
    m_CurrPosition: UInt64;    // The current play position
    m_Duration: UInt64;
    m_ResumingPlayTime: UInt64;
    m_Url: WideString;

    //--------------------------------------------------------------------------
    // IMFAsyncCallback methods
    // ========================
    //
    // Provides configuration information to the dispatching thread for a callback
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;
    // Called when an asynchronous operation is completed.
    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
    //--------------------------------------------------------------------------

    // Playback
    function GetSourceDuration(out pDuration: MFTIME): HResult;
    // Volume
    procedure SetVolume(aVal: Float);
    function GetVolume(): FLOAT;
    //
    function SetPositionInternal(aVal: MFTIME): HRESULT;
    procedure SetPosition(aVal: MFTIME);
    //
    procedure SetVideoSurface(aVal: HWnd);
    procedure SetEventHandle(aVal: HWnd);
    //
    procedure TimerTimer(sender: TObject);
    procedure UpdateCaption();

  public

    function Play(): HResult;
    function Pause(): HResult;
    function Stop(): HResult;
    function Shutdown(): HResult;
    function HandleEvent(pUnkPtr: UINT_PTR): HResult;

    // Video functionality
    function Repaint(): HResult;
    function ResizeVideo(VideoWidth: WORD;
                         VideoHeight: WORD): HResult;
    function HasVideo(): Boolean;

  protected
    m_Session: IMFMediaSession;
    m_Source: IMFMediaSource;
    m_VideoDisplay: IMFVideoDisplayControl;
    m_PresentationClock: IMFPresentationClock;      // Presentation Clock

    m_hwndVideo: HWND;          // Video window.
    m_hwndEvent: HWND;          // App window to receive events.
    m_state: PlayerState;       // Current state of the media session.
    m_hCloseEvent: THandle;     // Event to wait on while closing.

    function Initialize(): HResult;
    function CreateSession(): HResult;
    function CloseSession(): HResult;
    function StartPlayback(): HResult;

    // Media event handlers
    function OnTopologyStatus(pEvent: IMFMediaEvent): HResult; virtual;
    function OnPresentationEnded(pEvent: IMFMediaEvent): HResult; virtual;
    function OnNewPresentation(pEvent: IMFMediaEvent): HResult; virtual;

    // Override to handle additional session events.
    function OnSessionEvent(MediaEvent: IMFMediaEvent;
                            meType: MediaEventType): HResult; virtual;

  public
    // As you can see we do not use CreateInstance as in the C++ sample, but standard Delphi
    // to create an object.
    constructor Create(hVideo: HWND;
                       hEvent: HWND);
    destructor Destroy(); override;

    // Open media url
    function OpenURL(const sURL: PWideChar): HResult;
    // Duration
    function GetDuration(out hnsDuration: MFTIME): HResult;
    // Position
    function GetPosition(out hnsPosition: MFTIME): HResult;
    // Frame capture
    function TakeSnapShot(var bm: TBitMap): HRESULT;
    // Rate > delegated to extension class
    procedure SetRate(mftRate: Single);
    function GetRate(): Single;

    // Player states
    property State: PlayerState read m_state;
    // Volume property
    property Volume: Float read GetVolume write SetVolume;
    property Position: MFTIME read m_CurrPosition write SetPosition;
    property Duration: MFTIME read m_Duration;
    property ResumingPlayTime: MFTIME read m_ResumingPlayTime;
    property VideoSurfaceHandle: HWnd read m_hwndVideo write SetVideoSurface;
    property EventHandle: HWnd read m_hwndEvent write SetEventHandle;
    property FileName: WideString read m_Url;
  end;


implementation

// Constructor & destructor
constructor TDCPlayer.Create(hVideo: HWND;
                             hEvent: HWND);
begin
  inherited Create();
  InitMF();
  m_hwndVideo := hVideo;         // Video window.
  m_hwndEvent := hEvent;         // Window to receive notifications.
  m_state := Closed;
  m_hCloseEvent := 0;
  // Create a threaded timer
  FTimer := TUniThreadedTimer.Create(Nil);
  FTimer.Period := 100;
  FTimer.OnTimerEvent := TimerTimer;
end;

destructor TDCPlayer.Destroy();
begin
  assert(m_Session = Nil);
  Shutdown(); // Shut down the session and release its sources.
  FTimer.Enabled := False;
  FreeAndNil(FTimer);
  inherited;
end;


function TDCPlayer.GetDuration(out hnsDuration: MFTIME): HResult;
begin
  if Assigned(PlayerExtension) then
    Result := PlayerExtension.GetDuration(hnsDuration)
  else
    Result := GetSourceDuration(hnsDuration);
end;


function TDCPlayer.GetPosition(out hnsPosition: MFTIME): HResult;
var
  hr: HResult;

begin
  if Assigned(PlayerExtension) then
    hr := PlayerExtension.GetPosition(hnsPosition)
  else
    if Assigned(m_PresentationClock) then
      hr := m_PresentationClock.GetTime(hnsPosition)
    else
      hr := 0;
  Result := hr;
end;

function TDCPlayer.TakeSnapShot(var bm: TBitMap): HResult;
var
  buffer, data: PByte;
  bufSize: DWord;
  i: Integer;
  bmi: BITMAPINFOHEADER;
  timestamp: MFTIME;
  hr: HRESULT;

begin
  hr := S_OK;

try

{$IFDEF DEBUG}
  Assert(bm <> Nil);
{$ENDIF}

  // Set the biSize member of the structure to sizeof(BITMAPINFOHEADER)
  ZeroMemory(@bmi, sizeof(BITMAPINFOHEADER));
  bmi.biSize := sizeof(BITMAPINFOHEADER);

  data := Nil;
  buffer := Nil;
  bufsize := $0000;
  hr := E_FAIL;

  if Assigned(m_VideoDisplay) then
    begin
      hr := m_VideoDisplay.GetCurrentImage(Bmi,
                                           buffer,
                                           bufSize,
                                           timestamp);
      if FAILED(hr) then
        Exit;

      data := buffer;
    end;

  if (bmi.biSizeImage > 0) and (data <> nil) then
    begin
      // Adjustments
      bm.PixelFormat := pf32bit;
      bm.SetSize(abs(bmi.biWidth),
                 abs(bmi.biHeight));
      for i := abs(bmi.biHeight) - 1 downto 0 do // (int y = h - 1; y >= 0; --y)
        begin
          CopyMemory(bm.ScanLine[i],
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


procedure TDCPlayer.SetRate(mftRate: Single);
begin
  if Assigned(PlayerExtension) then
    PlayerExtension.SetRate(mftRate);
end;


function TDCPlayer.GetRate(): Single;
begin
  if Assigned(PlayerExtension) then
    PlayerExtension.GetRate(Result);
end;


function TDCPlayer.Initialize(): HResult;
var
  hr: HResult;

begin
  // Start up Media Foundation platform.
  hr := ( MFStartup(MF_VERSION) );

  if Succeeded(hr) then
    begin
      m_hCloseEvent := CreateEvent(Nil,
                                   False,
                                   False,
                                   Nil);
      if (m_hCloseEvent = 0) then
        hr := HRESULT_FROM_WIN32(GetLastError());
    end;
  Result := hr;
end;

//  Open a URL for playback.
function TDCPlayer.OpenURL(const sURL: PWideChar): HResult;
var
  hr: HResult;
  pTopology: IMFTopology;
  pSourcePD: IMFPresentationDescriptor;
  pClock: IMFClock;

label
  done;

  // 1. Create a new media session.
  // 2. Create the media source.
  // 3. Create the topology.
  // 4. Queue the topology [asynchronous]
  // 5. Start playback [asynchronous - does not happen in this method.]

begin
  m_Url := sURL;

  // Create the media session.
  hr := CreateSession();
  if FAILED(hr) then
    goto done;

  // Create the media source (from an URL).
  hr := CreateObjectFromUrl(sURL,
                            m_Source);

  if FAILED(hr) then
    goto done;

  // Create the presentation descriptor for the media source.
  hr := m_Source.CreatePresentationDescriptor(pSourcePD);

  if FAILED(hr) then
    goto done;

  // Get the presentation clock (optional)
  hr := m_Session.GetClock(pClock);

  if (SUCCEEDED(hr)) then
    hr := pClock.QueryInterface(IID_IMFPresentationClock,
                                m_PresentationClock);

  // If a PresentationClock can't be created, we can't get filetime positions
  if FAILED(hr) then
    goto done;

  // Create a partial topology.
  hr := CreatePlaybackTopology(m_Source,
                               pSourcePD,
                               m_hwndVideo,
                               pTopology);
  if FAILED(hr) then
    goto done;

  // Set the topology on the media session.
  hr := m_Session.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                               pTopology);
  if FAILED(hr) then
    goto done;

  // Add extension for fastforward, scrubbing etc.
  PlayerExtension := TPlayerSeeking.Create();
  // SetTopology
  hr := PlayerExtension.SetTopology(m_Session,
                                    pTopology);
  if FAILED(hr) then
    goto done;

  m_state := OpenPending;

  // If SetTopology succeeds, the media session will queue an
  // MESessionTopologySet event.

done:
  if FAILED(hr) then
    m_state := Closed;

  Result := hr;
end;

function TDCPlayer.GetSourceDuration(out pDuration: MFTIME): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;

begin
  pDuration := 0;
  hr := m_Source.CreatePresentationDescriptor(pPD);
  if Succeeded(hr) then
    hr := pPD.GetUINT64(MF_PD_DURATION,
                        UInt64(pDuration));
  Result := hr;
end;


// Set the volume and get the initial volume.
procedure TDCPlayer.SetVolume(aVal: Float);
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  fVolume: Float;
  //faVolumes: TFloatArray;
  hr: HRESULT;
  i: integer;

begin

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  fVolume := aVal;

  // Set boundaries to prevent overflow
  if fVolume > 1.0 then
    fVolume := 1.0;
  if fVolume < 0.0 then
    fVolume := 0.0;

  hr := (m_Session as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                IID_IMFAudioStreamVolume,
                                                pVol);
  // We could also implement stereo or balanced output.
  // but that is a nice goal for using your own creativity.
  if Succeeded(hr) then
    hr := pVol.GetChannelCount(nChannels);

  // Set the volume for each channel
  if Succeeded(hr) then
    begin
      for i := 0 to nChannels - 1 do
        hr := pVol.SetChannelVolume(i, fVolume);
      // Only 1 channel is selected; to do both you could use an array of channels
      // by using the GetAllVolumes function, see below.

      if Succeeded(hr) then
        hr := pVol.GetChannelVolume(0,
                                    flVolume);

      if Failed(hr) then
        Exit;

      // or like this
      //
      // Set the length of the array
      //SetLength(faVolumes, nChannels); // MUST DO!
      //hr := pVol.GetAllVolumes(nChannels,
      //                         @faVolumes[0]);
      //if Succeeded(hr) then
      //  begin
      //    for i := 0 to nChannels -1 do
      //      faVolumes[i] := fVolume;
      //
      //    hr := pVol.SetAllVolumes(nChannels,
      //                             @faVolumes);
      //  end;
      //
      // NOTE: A dynamic array is a managed type, and so when it's scope ends,
      //       it will be destroyed automaticly, unless the array holds unmanaged types.
      //       In that case each array element should be freed from memory.
      //
    end;
end;

function TDCPlayer.GetVolume(): Float;
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  aCurrVolume: array of Float;
  mv: FLOAT;
  hr: HRESULT;  // Debug
  i: integer;

begin
try
  hr := (m_Session as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                IID_IMFAudioStreamVolume,
                                                pVol);

  if Succeeded(hr) then
    begin
      hr := pVol.GetChannelCount(nChannels);

      // If balanced volume is needed, use an array f channels
      // to set each channel on desired volume.
      if Succeeded(hr) then
        hr := pVol.GetChannelVolume(0,
                                    flVolume);
      if Succeeded(hr) then
        begin
          SetLength(aCurrVolume, nChannels);
          // This function returns a Float array from the pointer.
          hr := pVol.GetAllVolumes(nChannels,
                                   @aCurrVolume[0]);

          // Find the highest soundlevel (because each channel can have a different level)
          if Succeeded(hr) then
            begin
              mv := aCurrVolume[0];
              for i := 1 to nChannels -1 do
                begin
                  if (aCurrVolume[i] >= mv) then
                    begin
                      flVolume := aCurrVolume[i];
                      mv := aCurrVolume[i];
                   end;
                end;
            end;
        end;
    end;

  if Failed(hr) then
    flVolume := 0.0;

finally
  Result := flVolume;
end;
end;

// Do not call this function directly!
// This function is called by SetPosition.
function TDCPlayer.SetPositionInternal(aVal: MFTIME): HRESULT;
var
  varStart: mfPROPVARIANT;
  hr: HRESULT;

begin

  hr:= E_FAIL;

  if (m_Session = nil) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

try

  PropVariantInit(varStart);
  varStart.vt := VT_I8;

  varStart.hVal.QuadPart := aVal;

  hr := m_Session.Start(GUID_NULL,
                        varStart);

  // The Start method can also specify a starting position relative to the start
  // of the file; see the API reference topic for more information.

  PropVariantClear(varStart);

  if (SUCCEEDED(hr)) then
    begin
      // Store the pending state
      m_CurrPosition := aVal;
      m_state := Started;
    end;

finally
  Result:= hr;
end;
end;


procedure TDCPlayer.SetPosition(aVal: MFTIME);
begin
  if Assigned(PlayerExtension) then
    PlayerExtension.SetPosition(aVal)
  else
    SetPositionInternal(aVal);
end;


procedure TDCPlayer.SetVideoSurface(aVal: HWnd);
begin
  m_hwndVideo := aVal;
  if Assigned(m_VideoDisplay) then
    m_VideoDisplay.SetVideoWindow(aVal);
end;


procedure TDCPlayer.SetEventHandle(aVal: HWnd);
begin
  m_hwndEvent := aVal;
end;


// A Timer for some extra duties, like showing progress when playing if
// the source has no time information ()
procedure TDCPlayer.TimerTimer(sender: TObject);
begin
  if (State = Started) then
    begin
      GetPosition(m_CurrPosition);
      m_ResumingPlayTime := (m_Duration - m_CurrPosition);
      // Send msg to the main form, we updated the play position
      SendMessage(m_hwndEvent,
                  WM_PROGRESSNOTIFY,
                  WParam(1),
                  LParam(0));
    end;
 UpdateCaption();
end;


// Called after an operation completes.
// This method executes any cached requests.
procedure TDCPlayer.UpdateCaption();
begin

  case State of
    OpenPending: SetWindowText(m_hwndEvent,
                               Format('Loaded: %s', [ExtractFileName(m_Url)]));

    Ready:       SetWindowText(m_hwndEvent,
                               'Session is ready.');

    Closing:     SetWindowText(m_hwndEvent,
                               'Closing session...');
    Started:       begin
                      SetWindowText(m_hwndEvent,
                                    Format('Playing: %s   Duration: %s / %s', [ExtractFileName(m_Url),
                                                                               HnsTimeToStr(m_ResumingPlayTime, false),
                                                                               HnsTimeToStr(m_CurrPosition, true)]));
                   end;

    Paused:       SetWindowText(m_hwndEvent,
                                'Paused.');

    Stopped:      SetWindowText(m_hwndEvent,
                                'Stopped.');

    Closed:       SetWindowText(m_hwndEvent,
                                'Session closed.');

   end;
end;


// Create a new instance of the media session.
function TDCPlayer.CreateSession(): HResult;
var
  hr: HResult;
label
  done;

  // This method performs the following steps:
  //
  // 1 Calls CPlayer.CloseSession to close any previous instance of the Media Session.
  // 2 Calls MFCreateMediaSession to create a new instance of the Media Session.
  // 3 Calls the IMFMediaEventGenerator.BeginGetEvent method to request the next event
  //   from the Media Session.
  //   The first parameter to BeginGetEvent is a pointer to the TDCPlayer object (including the callbackinterface) itself,
  //   which implements the IMFAsyncCallback interface.

begin
  // Close the old session, if any.
  hr := CloseSession();

  if FAILED(hr) then
    goto done;

 {$IFDEF DEBUG}
   assert(m_state = Closed);
 {$ENDIF}

  // Create the media session.
  hr := MFCreateMediaSession(Nil,
                             m_Session);
  if FAILED(hr) then
    goto done;

  // Start pulling events from the media session
  hr := m_Session.BeginGetEvent(IMFAsyncCallback(Self),
                                Nil);
  if FAILED(hr) then
    goto done;

  m_state := Ready;

done:
  Result := hr;
end;


function TDCPlayer.GetParameters(out pdwFlags: DWord;
                                 out pdwQueue: DWord): HResult;
begin
  Result := S_OK;
end;

// Callback for the asynchronous BeginGetEvent method.
function TDCPlayer.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  hr: HResult;
  meType: MediaEventType;  // Event type
  pEvent: IMFMediaEvent;

label
  done;

begin
  meType := MEUnknown;

  // Get the event from the event queue.
  hr := m_Session.EndGetEvent(pAsyncResult,
                              pEvent);
  if FAILED(hr) then
    goto done;

  // Get the event type.
  hr := pEvent.GetType(meType);

  if FAILED(hr) then
    goto done;

  if (meType = MESessionClosed) then
    begin
      // The session was closed.
      // The application is waiting on the m_hCloseEvent event handle.
      SetEvent(m_hCloseEvent);
    end
  else
    begin
      // For all other events, get the next event in the queue.
      hr := m_Session.BeginGetEvent(IMFAsyncCallback(Self),
                                     Nil);
      if FAILED(hr) then
        goto done;
    end;

  // Check the application state.

  // If a call to IMFMediaSession.Close is pending, it means the
  // application is waiting on the m_hCloseEvent event and
  // the application's message loop is blocked.

  if (m_state <> Closing) then
    begin
      // Send event message to handle the event by this object
      HandleEvent(UINT_PTR(pEvent));

      // Or / and,
      // post a private window message to the window that should handle the event..
      // DON'T FORGET TO IMPLEMENT THE MESSAGEHANDLER IN THE CALLING WINDOW (or unit)!

      PostMessage(m_hwndEvent,
                  WM_APP_PLAYER_EVENT,
                  WPARAM(0),  // or WPARAM(Pointer(pEvent))
                  LPARAM(meType));
    end;
done:
  Result := hr; // or S_OK if you can live with that..

end;


function TDCPlayer.HandleEvent(pUnkPtr: UINT_PTR): HResult;
var
  hrStatus: HResult;
  hr: HResult;
  meType: MediaEventType;
  pEvent: IMFMediaEvent;

label
  done;

begin
  meType := MEUnknown;
  pEvent := IMFMediaEvent(pUnkPtr);
  hrStatus := S_OK;

  if (pEvent = Nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Get the event type.
  hr := pEvent.GetType(meType);

  if FAILED(hr) then
    goto done;

  // Get the event status. If the operation that triggered the event
  // did not succeed, the status is a failure code.
  hr := pEvent.GetStatus(hrStatus);

  // Check if the async operation succeeded.
  if (SUCCEEDED(hr) And FAILED(hrStatus)) then
    hr := hrStatus;

  if FAILED(hr) then
    goto done;

  case meType of
    MESessionTopologyStatus: hr := OnTopologyStatus(pEvent);
    MEEndOfPresentation:     hr := OnPresentationEnded(pEvent);
    MENewPresentation:       hr := OnNewPresentation(pEvent);
    MESessionStarted:        FTimer.Enabled := True;
    MESessionStopped:        FTimer.Enabled := False;
    else
      hr := OnSessionEvent(pEvent,
                           meType);
   end;

done:
  Result := hr;
end;


function TDCPlayer.OnTopologyStatus(pEvent: IMFMediaEvent): Hresult;
var
  hr: HResult;
  status: UINT32;

begin
  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                         status);

  if (SUCCEEDED(hr) And (status = Dword(MF_TOPOSTATUS_READY))) then
    begin
      m_VideoDisplay := Nil;
      // Get the IMFVideoDisplayControl interface from EVR. This call is
      // expected to fail if the media file does not have a video stream.

      {void} MFGetService(m_Session,
                          MR_VIDEO_RENDER_SERVICE,
                          IID_IMFVideoDisplayControl,
                          m_VideoDisplay);

      // Get the duration of the source
      hr := GetDuration(m_Duration);
      hr := StartPlayback();
    end;
  Result := hr;
end;

//  Handler for MEEndOfPresentation event.
function TDCPlayer.OnPresentationEnded(pEvent: IMFMediaEvent): Hresult;
begin
  // The session puts itself into the stopped state automatically.
  m_state := Stopped;
  PlayerExtension.Clear();
  Result := S_OK;
end;

//  Handler for MENewPresentation event.
//
//  This event is sent if the media source has a new presentation, which
//  requires a new topology.
function TDCPlayer.OnNewPresentation(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;
  pTopology: IMFTopology;

label
  done;

begin
  // Get the presentation descriptor from the event.
  hr := GetEventObject(pEvent,
                       pPD);
  if FAILED(hr) then
    goto done;

  // Create a partial topology.
  hr := CreatePlaybackTopology(m_Source,
                               pPD,
                               m_hwndVideo,
                               pTopology);
  if FAILED(hr) then
    goto done;

  // Set the topology on the media session.
  hr := m_Session.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                              pTopology);
  if FAILED(hr) then
    goto done;

  PlayerExtension := TPlayerSeeking.Create();  //SetTopology
  hr := PlayerExtension.SetTopology(m_Session,
                                    pTopology);

  if FAILED(hr) then
    goto done;

  m_state := OpenPending;

done:
  Result := hr;
end;

// Nothing todo here.
function TDCPlayer.OnSessionEvent(MediaEvent: IMFMediaEvent;
                                  meType: MediaEventType): HResult;
begin
  PlayerExtension.SessionEvent(MediaEvent,
                               meType);

  Result := S_OK;
end;



// Start playback from the current position.
function TDCPlayer.StartPlayback(): HResult;
var
  hr: HResult;
  varStart: mfPROPVARIANT;

begin

{$IFDEF DEBUG}
  assert(m_Session <> Nil);
{$ENDIF}

  PropVariantInit(varStart);

  hr := m_Session.Start(GUID_NULL,
                        varStart);
  // Set the state
  if Succeeded(hr) then
    m_state := Started
  else
    m_state := Closed;

  PropVariantClear(varStart);
  Result := hr;
end;

//  Start playback from paused or stopped.
function TDCPlayer.Play(): HResult;
var
  hr: HResult;
begin
  if (m_state <> Paused) And (m_state <> Stopped) And (m_state <> OpenPending) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

  if (m_Session = Nil) Or (m_Source = Nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if Assigned(PlayerExtension) then
    hr := PlayerExtension.Start()
  else
    hr := StartPlayback();

  if Succeeded(hr) then
    begin
      // Note: Start is an asynchronous operation. However, we
      // can treat our state as being already started. If Start
      // fails later, we'll get an MESessionStarted event with
      // an error code, and we will update our state then.
      m_state := Started;
    end;
  Result := hr;
end;

//  Pause playback.
function TDCPlayer.Pause(): HResult;
var
 hr: HResult;

begin
  if (m_state <> Started) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

  if (m_Session = Nil) Or (m_Source = Nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if Assigned(PlayerExtension) then
    hr := PlayerExtension.Pause()
  else
    hr := m_Session.Pause();

  if SUCCEEDED(hr) then
    m_state := Paused;

  Result := hr;
end;

// Stop playback.
function TDCPlayer.Stop(): HResult;
var
 hr: HResult;

begin
  if (m_state <> Started) And (m_state <> Paused) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

  if (m_Session = Nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if Assigned(PlayerExtension) then
    hr := PlayerExtension.Stop()
  else
    hr := m_Session.Stop();

  if SUCCEEDED(hr) then
    m_state := Stopped;

  Result := hr;
end;

//  Repaint the video window. Call this method on WM_PAINT message handler of the caller.
function TDCPlayer.Repaint(): HResult;
begin
  if Assigned(m_VideoDisplay) then
    Result := m_VideoDisplay.RepaintVideo()
  else
    Result := S_OK;
end;

// Call this function if the state = Ready or on MESessionStarted event has been launched.
function TDCPlayer.HasVideo(): Boolean;
begin
  Result := (m_VideoDisplay <> Nil);
end;

//  Resize the video rectangle.
//
//  Call this method if the size of the video window changes.
function TDCPlayer.ResizeVideo(VideoWidth: WORD;
                               VideoHeight: WORD): HResult;
var
  rcDest: TRect;

  // If you resize the video window (OnResize),
  // update the destination rectangle on the EVR by calling the
  // IMFVideoDisplayControl.SetVideoPosition method:

begin
  if Assigned(m_VideoDisplay) then
    begin
      // Set the destination rectangle.
      // Leave the default source rectangle (0,0,1,1).
      rcDest.Left := 0;
      rcDest.Top := 0;
      rcDest.Width := VideoWidth;
      rcDest.height := VideoHeight;

      Result := m_VideoDisplay.SetVideoPosition(Nil,
                                                @rcDest);
    end
  else
    Result := S_OK;
end;


///////////////////

//  Close the media session.
function TDCPlayer.CloseSession(): HResult;
var
  hr: HResult;
  dwWaitResult: DWORD;

begin
  //  The IMFMediaSession.Close method is asynchronous, but the
  //  TDCPlayer.CloseSession method waits on the MESessionClosed event.
  //
  //  MESessionClosed is guaranteed to be the last event that the
  //  media session fires.

  hr := S_OK;
  m_VideoDisplay := Nil;

  // First close the media session.
  if Assigned(m_Session) then
    begin
      m_state := Closing;
      hr := m_Session.Close();
      // Wait for the close operation to complete
      if SUCCEEDED(hr) then
        begin
          dwWaitResult := WaitForSingleObject(m_hCloseEvent,
                                              5000);
          if (dwWaitResult = WAIT_TIMEOUT) then
            assert(False);

          // Now there will be no more events from this session.
        end;
    end;

  // Complete shutdown operations.
  // Important note:
  // Releasing IMFMediaSource and IMFMediaSession.
  //   First, shut down the MediaSource.
  //   To do so, call the IMFMediaSource.Shutdown method on the sequencer source.
  //   This call shuts down all of the underlying native media sources in the sequencer source.
  //   After shutting down the IMFMediaSource source, the application should close and shut down the
  //   Media Session by calling IMFMediaSession.Close AND IMFMediaSession.Shutdown, in that order.
  //   To avoid memory leaks, the application must release all pointers or references to (global) Media Foundation interfaces
  //   when they are no longer needed or when the aplication is closing down.
  if SUCCEEDED(hr) then
    begin
      // Shut down the media source. (Synchronous operation, no events.)
      if Assigned(m_Source) then
        {void} m_Source.Shutdown();

      // Shut down the media session. (Synchronous operation, no events.)
      if Assigned(m_Session) then
        {void} m_Session.Shutdown();
    end;
  // Release the interfaces
  m_Source := Nil;
  m_Session := Nil;
  PlayerExtension.Free;
  PlayerExtension := Nil;
  m_state := Closed;
  Result := hr;
end;

//  Release all resources held by this object.
function TDCPlayer.Shutdown(): HResult;
var
  hr: HResult;

begin
  // Close the session
  hr := CloseSession();

  // Shutdown the Media Foundation platform
  MFShutdown();

  if (m_hCloseEvent <> 0) then
    begin
        CloseHandle(m_hCloseEvent);
        m_hCloseEvent := 0;
    end;
  Result := hr;
end;

end.
