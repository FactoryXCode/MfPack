// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfpPlayerClass.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 3.1.4
// Description: This is the basic class of MfPlayer,
//              containing the necessary methodes to play a mediafile
//              For indepth information see the included examples (CPlayer)
//              and text files containing the complete information about
//              MfPlayer.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//          This sample shows how to implement the TInterfacedObject.
//
// Related objects: -
// Related projects: MfPackX314
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
// Copyright (c) Microsoft Corporation. All rights reserved
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

interface

uses
  {Winapi}
  Winapi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ComBaseApi,
  {System}
  System.Win.ComObj,
  System.SysUtils,
  System.Types,
  System.Classes,
  {VCL}
  VCL.Graphics,
  VCL.ExtCtrls,
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
  WinApi.MediaFoundationApi.Evr9;

  {$DEFINE AUTOREFCOUNT}

const
  WM_APP_PLAYER_EVENT = WM_APP + 1;


type

  TRedrawStatus = (rdStarted,
                   rdStopped);

  TCommand = (CmdNone = 0,
              CmdStop,
              CmdStart,          // start after Pause or stopped
              CmdStartPlayBack,  // start first (pending)
              CmdPause,
              CmdSeek,
              CmdClosing,
              CmdOpenPending,
              CmdClosed,
              CmdReady);


  TPlayerState = (Closed = 0,    // No session.
                  Ready,         // Session was created, ready to open a file.
                  OpenPending,   // Session is opening a file.
                  Starting,      // Session initializing Start
                  Stopping,      // Session initializing Stop
                  Started,       // Session is playing a file.
                  Paused,        // Session is paused.
                  Stopped,       // Session is stopped (ready to play).
                  Closing);      // Application has closed the session, but is waiting for MESessionClosed.


  TSeekState = record
    Command: TCommand;
    State: TPlayerState;
    fRate: FLOAT;       // Playback rate
    bThin: boolean;     // Thinned playback if supported
    hnsStart: MFTIME;   // Start position
  end;


type


  TMfAsyncCallback = interface(IMFAsyncCallback)

    function OpenURL(sURL: PWideChar): HRESULT;
    function Repaint: HRESULT;
    function SendPlayerCommand(cmd: TCommand): HRESULT;
    function ShutDown(): HRESULT;
    function GetState(): TPlayerState;
    function ResizeVideo(dRect: LPRECT): HRESULT;
    procedure SetRedraw();
    function HasVideo(): boolean;
    procedure TimerTimer(sender: TObject);
    procedure SetTimerInterval(val: cardinal);
    function TakeSnapShot(var bit: TBitMap): HRESULT;
    procedure SetVolume(Value: FLOAT);
    function GetVolume(): FLOAT;
    function GetInitialDuration(out dur: MFTIME): HRESULT;

    property Volume: FLOAT read GetVolume write SetVolume;
  end;


  TMfPlayer = class(TInterfacedPersistent, TMfAsyncCallback)
  private
  {private fields}

    // Internal objects
    stRedrawStatus:      TRedrawStatus;
    m_dWaitResult:       DWORD;
    m_iTimerInterval:    Cardinal;     // Timer interval
    m_bPending:          Boolean;      // Is a request pending?
    FFileName:           string;       // filename incl path
    m_dCaps:             DWORD;        // capabilites
    m_hnsDuration:       UInt64;       // Duration
    m_Request:           TSeekState;
    m_State:             TSeekState;
    FhCloseEvent:        THandle;
    FTimer:              TTimer;

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
    //--------------------------------------------------------------------------

    // Creates a media source from a URL.
    function CreateMediaSource(const sURL: WideString;
                               var ppSource: IMFMediaSource): HRESULT;

    // Create an activation object for a renderer, based on the stream media type
    function CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                     hVideoWnd: HWND;
                                     out ppActivate: IMFActivate): HRESULT;

    // Creates a playback topology from the media source.
    //
    // Pre-condition: The media source must be created already.
    // Call CreateMediaSource() before calling this method
    // Create a playback topology from a media source.
    function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                    pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                    hVideoWnd: HWND;                       // Video window.
                                    out ppTopology: IMFTopology): HRESULT; // Receives a pointer to the topology.

    // Add a source node to a topology.
    function AddSourceNode(pTopology: IMFTopology;                  // Topology.
                           pSource: IMFMediaSource;                 // Media source.
                           pPD: IMFPresentationDescriptor;          // Presentation descriptor.
                           pSD: IMFStreamDescriptor;                // Stream descriptor.
                           out ppNode: IMFTopologyNode): HRESULT;   // Receives the node pointer.

    // Add an output node to a topology.
    function AddOutputNode(pTopology: IMFTopology;                 // Topology.
                           pActivate: IMFActivate;                 // Media sink activation object.
                           dwId: DWORD;                            // Identifier of the stream sink.
                           out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.




    //  Adds a topology branch for one stream.
    //
    //  pTopology: Pointer to the topology object.
    //  pSourcePD: The source's presentation descriptor.
    //  iStream: Index of the stream to render.
    //
    //  Pre-conditions: The topology must be created already.
    //
    //  Notes: For each stream, we must do the following:
    //    1. Create a source node associated with the stream.
    //    2. Create an output node for the renderer.
    //    3. Connect the two nodes.
    //  The media session will resolve the topology, so we do not have
    //  to worry about decoders or other transforms.
    function AddBranchToPartialTopology(var pTopology: IMFTopology;
                                        var pSource: IMFMediaSource;
                                        var pPD: IMFPresentationDescriptor;
                                        iStream: DWord;
                                        hVideoWnd: HWND): HRESULT;


    //---------------------------------------------------------------
    // Handlers
    //
    // Handler for OnTopologyStatus event
    function OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
    // Handler for OnTopologySet event
    function OnTopologySet(pEvent: IMFMediaEvent):  HRESULT;
    // Handler for OnTopologyReady event
    function OnTopologyReady(pEvent: IMFMediaEvent): HRESULT;
    // Handler for MEEndOfPresentation event
    function OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
    // Override to handle additional session events.
    function OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
    // Handler for MENewPresentation event.
    //
    // This event is sent if the media source has a new presentation, which
    // requires a new topology.
    function OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;

    function OnStart(pEvent: IMFMediaEvent): HRESULT;  // Commando Start event
    function OnPause(pEvent: IMFMediaEvent): HRESULT;  // Commando Pause event
    function OnStop(pEvent: IMFMediaEvent): HRESULT;   // Commando Stop event

    //
    function HandleEvent(pEventPtr: UINT_PTR ): HRESULT;

    // create a new instance of the media session
    function CreateSession(): HRESULT;
    // close instance of the media session
    function CloseSession(): HRESULT;
    // Initial start playing media
    function StartPlayBack(): HRESULT;

    // Initialize the player
    function Initialize(): HRESULT;

    function SetPositionInternal(tPos: MFTIME): HRESULT;
    function UpdatePendingCommands(cmd: TCommand): HRESULT;

    procedure UpdateCaption();

    // play, pause, stop internal. Use SendPlayerCmd()
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    procedure Clear();

  protected
  {protected fields}

    // interfaces
    m_pSession:      IMFMediaSession;        // Media Session
    m_pSource:       IMFMediaSource;         // Media Source
    m_pVideoDisplay: IMFVideoDisplayControl; // Video control
    MfAsyncCallback: TMfAsyncCallback;

  {protected methods}

  public
  {public fields}
    m_hwndEvent:         HWND;
    m_hwndVideo:         HWND;
    m_fVolume:           FLOAT;        // Volumelevel

  {public methods}

    constructor Create(hwndVideo: HWND;
                       hwndEvent: HWND);
    destructor Destroy(); override;
    procedure BeforeDestruction(); override;

    // playback
    function OpenURL(sURL: PWideChar): HRESULT;
    function Repaint: HRESULT;
    function SendPlayerCommand(cmd: TCommand): HRESULT;

    function ShutDown(): HRESULT;

    // Playback control
    function GetState(): TPlayerState;

    // Resizes the video rectangle.
    // The application calls this method if the size of the video window changes;
    // e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(dRect: LPRECT): HRESULT;

    // Stop flickering of controls when resizing
    procedure SetRedraw();

    // Check if stream contains video
    function HasVideo(): boolean;

    // Callback timer
    procedure TimerTimer(sender: TObject);
    procedure SetTimerInterval(val: cardinal);

    // Frame capture
    function TakeSnapShot(var bit: TBitMap): HRESULT;

    // Volume
    procedure SetVolume(Value: FLOAT);
    function GetVolume(): FLOAT;

    // Get the duration if the mediafile
    function GetInitialDuration(out dur: MFTIME): HRESULT;

    // Properties
    property FileName: String read FFileName;
    property IntTimer: TTimer read FTimer write FTimer;
    property TimerInterval: cardinal read m_iTimerInterval write SetTimerInterval default 500;
    property Volume: FLOAT read GetVolume write SetVolume;
    property Duration: UInt64 read m_hnsDuration;
  end;


implementation

// TMfAsyncCallback ////////////////////////////////////////////////////////////

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

  if not Assigned(m_pSession) then
    begin
      Result := ERROR_NOT_ALL_ASSIGNED;
      Exit;
    end;

  // Get the event from the event queue.
  hr := m_pSession.EndGetEvent(pAsyncResult, pEvent);
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
      hr := m_pSession.BeginGetEvent(self as TMfAsyncCallback,
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

  if (m_state.Command <> CmdClosing) then
    begin
      // Send event message
      HandleEvent(UINT_PTR(pEvent));
    end;

done:
  SafeRelease(pAsyncResult);
  Result := hr;
end;


// external functions //////////////////////////////////////////////////////////

function GetEventObject(pEvent: IMFMediaEvent;
                        out ppObject): HRESULT;
var
  vVar: PROPVARIANT;
  hr: HRESULT;

begin
  PropVariantInit(vvar);

  hr := pEvent.GetValue(vvar);

  if (SUCCEEDED(hr)) then
    begin
      if (vvar.vt = VT_UNKNOWN) then
        hr := vvar.ppunkVal.QueryInterface(IID_IUnknown,
                                           ppObject)
      else
        hr := MF_E_INVALIDTYPE;

      PropVariantClear(vvar);
    end;

  Result := hr;
end;


function CreateSourceStreamNode(pSource: IMFMediaSource;
                                pSourcePD: IMFPresentationDescriptor;
                                pSourceSD: IMFStreamDescriptor;
                                out ppNode: IMFTopologyNode): HRESULT;
var
   pNode: IMFTopologyNode;
   hr: HRESULT;

label
  done;

begin

  if (not Assigned(pSource) or
      not Assigned(pSourcePD) or
      not Assigned(pSourceSD) or
      not Assigned(ppNode)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Create the source-stream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             pNode);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the media source.
  hr := pNode.SetUnknown(MF_TOPONODE_SOURCE,
                         pSource);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the presentation descriptor.
  hr := pNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the stream descriptor.
  hr := pNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                         pSourceSD);
  if (FAILED(hr)) then
    goto done;

  // Return the IMFTopologyNode pointer to the caller.
  ppNode := pNode;

done:
  Result := hr;
end;

//
function CreateOutputNode(pSourceSD: IMFStreamDescriptor;
                          hwndVideo: HWND;
                          out ppNode: IMFTopologyNode): HRESULT;
var
  pNode: IMFTopologyNode;
  pHandler: IMFMediaTypeHandler;
  pRendererActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HRESULT;

label
  done;

begin

  // Get the stream ID.
  //streamID := 0;

  // Just for debugging, ignore any failures.
  //hr := pSourceSD.GetStreamIdentifier(streamID);


  // Get the media type handler for the stream.
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if (FAILED(hr)) then
    goto done;

  // Get the major media type.
  hr := pHandler.GetMajorType(guidMajorType);
  if (FAILED(hr)) then
    goto done;

  // Create a downstream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             pNode);
  if (FAILED(hr)) then
    goto done;

  // Create an IMFActivate object for the renderer, based on the media type.
  if IsEqualGuid(MFMediaType_Audio,
                 guidMajorType) then
    begin
      // Create the audio renderer.
      hr := MFCreateAudioRendererActivate(pRendererActivate);
    end
  else
    if IsEqualGuid(MFMediaType_Video,
                   guidMajorType) then
      begin
        // Create the video renderer.
        hr := MFCreateVideoRendererActivate(hwndVideo,
                                            pRendererActivate);
      end
    else
      hr := E_FAIL;

  if (FAILED(hr)) then
    goto done;

  // Set the IActivate object on the output node.
  hr := pNode.SetObject(pRendererActivate);
  if (FAILED(hr)) then
    goto done;

  // Return the IMFTopologyNode pointer to the caller.
  ppNode := pNode;

done:
  Result := hr;
end;


////////////// TMFPlay /////////////////////////////////////////////////////////

function TMfPlayer.AddBranchToPartialTopology(var pTopology: IMFTopology;
                                              var pSource: IMFMediaSource;
                                              var pPD: IMFPresentationDescriptor;
                                              iStream: DWord;
                                              hVideoWnd: HWND): HRESULT;
var
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  fSelected: Bool;
  hr: HRESULT;

label
  Done;

begin
  assert(pTopology <> nil);

  // Get the stream descriptor for this stream.
  hr := pPD.GetStreamDescriptorByIndex(iStream,
                                       fSelected,
                                       pSD);
  if (FAILED(hr)) then
    goto done;

  // Create the topology branch only if the stream is selected.
  // Otherwise, do nothing.
  if (fSelected) then
    begin
      // create the media sink activation object
      hr := CreateMediaSinkActivate(pSD, hVideoWnd, pSinkActivate);
      if (FAILED(hr)) then
        goto done;

      // Create a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);
      if (FAILED(hr)) then
        goto done;

      // Create the output node for the renderer.
      hr := AddOutPutNode(pTopology, pSinkActivate, 0, pOutPutNode);
      if (FAILED(hr)) then
        goto done;

      // Connect the source node to the output node.
      hr := pSourceNode.ConnectOutput(0, pOutputNode, 0);
    end;

done:
  Result := hr;
end;


// Add a source node to a topology.
function TMfPlayer.AddSourceNode(pTopology: IMFTopology;                   // Topology.
                                 pSource: IMFMediaSource;                  // Media source.
                                 pPD: IMFPresentationDescriptor;           // Presentation descriptor.
                                 pSD: IMFStreamDescriptor;                 // Stream descriptor.
                                 out ppNode: IMFTopologyNode): HRESULT;    // Receives the node pointer.

var
  pNode: IMFTopologyNode;
  hr: HRESULT;

label
  Done;

begin
  pNode := nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE, pNode);
  if (FAILED(hr)) then
    goto done;

  // Set the attributes.
  hr := pNode.SetUnknown(MF_TOPONODE_SOURCE, pSource);
  if (FAILED(hr)) then
    goto done;

  hr := pNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR, pPD);
  if (FAILED(hr)) then
    goto done;

  hr := pNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR, pSD);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(pNode);
  if (FAILED(hr)) then
    goto done;

  // Return the pointer to the caller.
  ppNode := pNode;

done:
   Result := hr;
end;


procedure TMfPlayer.BeforeDestruction();
begin
  // Stop the session

  // The application must call Shutdown because the media session holds a
  // reference count on the MfPlayer object. (This happens when MfPlayer calls
  // IMediaEventGenerator.BeginGetEvent on the media session.) As a result,
  // there is a circular reference count between the MfPlayer object and the
  // media session. Calling Shutdown breaks the circular reference count.

  // If CreateInstance failed, the application will not call Shutdown. To
  // handle that case, we must call Shutdown() in the destructor. The
  // circular ref-count problem does not occcur if CreateInstance has failed.
  ShutDown();

  SafeRelease(m_pVideoDisplay);
  // Release the timer.
  FTimer.Enabled := False;
  FreeAndNil(FTimer);
  SafeRelease(MfAsyncCallback);

  CoUninitialize();
  MFShutdown();
  inherited BeforeDestruction();
end;


// Add an output node to a topology.
function TMfPlayer.AddOutputNode(pTopology: IMFTopology;      // Topology.
                                 pActivate: IMFActivate;      // Media sink activation object.
                                 dwId: DWORD;                 // Identifier of the stream sink.
                                 out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.
var
  pNode: IMFTopologyNode;
  hr: HRESULT;

label
  Done;

begin

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, pNode);
  if (FAILED(hr)) then
    goto done;

  // Set the object pointer.
  hr := pNode.SetObject(pActivate);
  if (FAILED(hr)) then
    goto done;

  // Set the stream sink ID attribute.
  hr := pNode.SetUINT32(MF_TOPONODE_STREAMID, dwId);
  if (FAILED(hr)) then
    goto done;

  hr := pNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE, 0);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(pNode);
  if (FAILED(hr)) then
    goto done;

  // Return the pointer to the caller.
  ppNode := pNode;

done:
  Result := hr;
end;


procedure TMfPlayer.Clear();
begin
  with m_Request do
    begin
      Command := CmdNone;
      fRate := 1.0;
      bThin := False;
      hnsStart := 0;
    end;

  with m_state do
    begin
      Command := CmdNone;
      State := Closed;
      fRate := 1.0;
      bThin := False;
      hnsStart := 0;
    end;

  // reset
  m_hnsDuration := 0;
  m_bPending := False;
  m_dCaps := 0;
  FFileName := '';
  SafeRelease(m_pSession);
  SafeRelease(m_pSource);
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;


function TMfPlayer.CloseSession(): HRESULT;
var
  hr: HRESULT;
  dwWaitResult: DWORD;

label
  Done;

begin

  hr := s_ok;

  if GetState in [Started, Paused] then
    SendPlayerCommand(CmdStop);

  // release the video display object
  m_pVideoDisplay := Nil;

  // First close the media session.
  if Assigned(m_pSession) then
    begin
      m_state.Command := CmdClosing;
      m_state.State := Closing;

      hr := m_pSession.Close;
      if (FAILED(hr)) then
        goto done;

      // Wait for the close operation to complete for 2 seconds
      dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent), 1000);

      if (dwWaitResult = WAIT_TIMEOUT) then
        m_dWaitResult := dwWaitResult;
    end;

  // Complete shutdown operations.
  // Shut down the media source. (Synchronous operation, no events.)
  if Assigned(m_pSource) and (m_state.State <> Closed) then
    hr := m_pSource.Shutdown();

  // Shut down the media session. (Synchronous operation, no events.)
  if Assigned(m_pSession) then
    begin
      hr := m_pSession.ClearTopologies();
      if Failed(hr) then
        raise Exception.Create('Error ' + SysErrorMessage(hr));
      hr := m_pSession.Shutdown();
    end;

  Clear();

  m_state.Command := CmdClosed;
  m_state.State := Closed;

done:
  Result := hr;
end;


procedure TMfPlayer.SetTimerInterval(val: cardinal);
begin
  if (val < 100) or (val > 10000) then
    val := 500; // reset to default
  FTimer.Interval := val;
end;



// The constructor
constructor TMfPlayer.Create(hwndVideo: HWND;
                             hwndEvent: HWND);
var
  hr: HRESULT;

begin
  inherited Create();

  m_hwndVideo := hwndVideo;
  m_hwndEvent := hwndEvent;

  CoInitializeEx(Nil,
                 COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' +
                       IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort;
    end;

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

  FTimer := TTimer.Create(Nil);
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerTimer;

end;

// The destructor
destructor TMfPlayer.Destroy();
begin

  inherited Destroy();
end;

////////////////////////////////////////////////////////////////////////////////


function TMfPlayer.CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                           hVideoWnd: HWND;
                                           out ppActivate: IMFActivate): HRESULT;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HRESULT;

label
  Done;

begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto Done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if FAILED(hr) then
    goto Done;

  // Create an IMFActivate object for the renderer, based on the media type
  if (MFMediaType_Audio = guidMajorType) then
    hr := MFCreateAudioRendererActivate(pActivate)
  else if (MFMediaType_Video = guidMajorType) then
    hr := MFCreateVideoRendererActivate(hVideoWnd, pActivate)
  else
    hr := E_FAIL;

  if FAILED(hr) then
    goto Done;

  // Return IMFactivate pointer to caller
  ppActivate := pActivate;

done:
  Result := hr;
end;


//  Create a media source from a URL.
function TMfPlayer.CreateMediaSource(const sURL: WideString;
                                     var ppSource: IMFMediaSource): HRESULT;
var
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  pSource: IUnknown;
  hr: HRESULT;

label
  Done;

begin

  ObjectType := MF_OBJECT_INVALID;

  SafeRelease(m_pSource);

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (FAILED(hr)) then
    goto done;

  // Use the source resolver to create the media source.

  // Note: For simplicity this sample uses the synchronous method on
  // IMFSourceResolver to create the media source. However, creating a media
  // source can take a noticeable amount of time, especially for a network
  // source. For a more responsive UI, use the asynchronous
  // pSourceResolver.BeginCreateObjectFromURL method.

  hr := pSourceResolver.CreateObjectFromURL(PWideChar(sURL),           // URL of the source.
                                            DWORD(MF_RESOLUTION_MEDIASOURCE), // Create a source object.
                                            Nil,                       // Optional property store.
                                            ObjectType,                // Receives the created object type.
                                            pSource);                  // Receives a pointer to the media source (IUnknown).

  if (FAILED(hr)) then
    goto done;

  // Get the IMFMediaSource interface from the media source.
  hr := pSource.QueryInterface(IID_IMFMediaSource,
                               ppSource);

Done:
  Result := hr;
end;


function TMfPlayer.CreateSession: HRESULT;
var
  hr: HRESULT;

label
  Done;

begin
  // Close the old session, if any.
  hr := CloseSession();
  if (FAILED(hr)) then
    goto done;

  // Debug issue
  assert(m_State.Command = CmdClosed);

  // Create the media session.
  hr := MFCreateMediaSession(Nil,
                             m_pSession);
  if (FAILED(hr)) then
    goto done;

  m_State.Command := CmdReady;
  m_State.State := Ready;

  // Start pulling events from the media session
  hr := m_pSession.BeginGetEvent(Self as TMfAsyncCallback,
                                 Nil);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


// Create a playback topology from a media source.
function TMfPlayer.CreatePlaybackTopology(pSource: IMFMediaSource;           // Media source.
                                          pPD: IMFPresentationDescriptor;    // Presentation descriptor.
                                          hVideoWnd: HWND;                   // Video window.
                                          out ppTopology: IMFTopology): HRESULT; // Receives a pointer to the topology.
var
  pTopology: IMFTopology;
  cSourceStreams: DWORD;
  hr: HResult;
  i: integer;

label
  done;

begin
  pTopology := Nil;
  cSourceStreams := 0;

  // Create a new topology.
  hr := MFCreateTopology(pTopology);
  if (FAILED(hr)) then
    goto done;

  // Get the number of streams in the media source.
  hr := pPD.GetStreamDescriptorCount(cSourceStreams);
  if (FAILED(hr)) then
    goto done;

  // For each stream, create the topology nodes and add them to the topology.
  for i := 0 to cSourceStreams - 1 do
    begin
      hr := AddBranchToPartialTopology(pTopology,
                                       pSource,
                                       pPD,
                                       i,
                                       hVideoWnd);
      if (FAILED(hr)) then
        goto done;
    end;

  // Return the IMFTopology pointer to the caller.
  ppTopology := pTopology;

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
  // use assertions in debug mode only.
  {$IFDEF DEBUG}
  Assert(bit <> Nil);
  {$IFEND}
  data := Nil;

  // Set the biSize member of the structure to sizeof(BITMAPINFOHEADER)
  ZeroMemory(@bmi, sizeof(BITMAPINFOHEADER));
  bmi.biSize := sizeof(BITMAPINFOHEADER);

  bufsize := $0000;
  hr := E_FAIL;

  if Assigned(m_pVideoDisplay) then
    begin
      hr := m_pVideoDisplay.GetCurrentImage(Bmi,
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
function TMfPlayer.GetInitialDuration(out dur: MFTIME): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  hr: HRESULT;
  mdur: MFTIME;

begin
  if Assigned(m_pSource) then
    begin
      dur := 0;
      hr := m_pSource.CreatePresentationDescriptor(pPD);
      if (SUCCEEDED(hr)) then
        begin
          hr := pPD.GetUINT64(MF_PD_DURATION, UINT64(mdur));
          dur := mdur;
          m_hnsDuration := dur;
        end;
    end
  else
    hr := E_POINTER;

  Result := hr;
end;


function TMfPlayer.GetState(): TPlayerState;
begin
  Result := m_State.State;
end;

{$HINTS OFF}
function TMfPlayer.GetVolume(): FLOAT;
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  hr: HRESULT;

begin
try
  // When returned value = -2147467262,  then No such interface supported. (0x80004002, $80004002)
  hr := (m_pSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                IID_IMFAudioStreamVolume,
                                                Pointer(pVol));
  if SUCCEEDED(hr) then
    begin
      hr := pVol.GetChannelCount(nChannels);

      // If balanced volume is needed, use an array f channels
      // to set each channel on desired volume.
      hr := pVol.GetChannelVolume(0,
                                  m_fVolume);

      // this function returns an array of channels with their respective volumes
      // hr := pVol.GetAllVolumes(nChannels,
      //                         fCurrVolume);
    end;

finally
  Result := m_fVolume;
end;
end;
{$HINTS ON}

function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult;
begin
  Result := E_NOTIMPL;
end;


function TMfPlayer.Initialize: HRESULT;
var
  h: THandle;

begin

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

  if FhCloseEvent = 0 then
    begin
      Result := GetLastError();
    end
  else
    Result := S_OK;

end;


function TMfPlayer.HasVideo: boolean;
begin
  Result := (m_pVideoDisplay <> nil);
end;


function TMfPlayer.OnTopologyStatus(pEvent: IMFMediaEvent): HRESULT;
var
  status: UINT32;
  hr: HRESULT;

begin
  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS, status);
  if SUCCEEDED(hr) then
    if (status = UINT32(MF_TOPOSTATUS_READY)) then
      begin
        // Call OnTopologyReady
        hr := OnTopologyReady(pEvent);  // Send msg we are ready
      end;
  Result := hr;
end;

// Handler for MESessionTopologySet event - This signals the topoly has been created.
function TMfPlayer.OnTopologySet(pEvent: IMFMediaEvent): HRESULT;
begin
  // Add your code here
  m_State.State := Ready;
  Result := S_OK;
end;

// Handler for MESessionTopologyReady event - starts video playback.
function TMfPlayer.OnTopologyReady(pEvent: IMFMediaEvent): HRESULT;
var
  hr: HRESULT;
  rc: TRect;
  rcd: LPRECT; //NRECT is a Normalised RECT (In C++ defined as RECT)

begin
  hr := S_OK;
  // release any previous instance of the m_pVideoDisplay interface
  SafeRelease(m_pVideoDisplay);

try
try
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

  if SUCCEEDED(hr) then
    begin
      // Adjust aspect ratio
      if Assigned(m_pVideoDisplay) then
        begin
          if Winapi.Windows.GetClientRect(m_hwndVideo, rc) then
            begin
              CopyTRectToLPRect(rc, rcD);
              hr := m_pVideoDisplay.SetAspectRatioMode(MFVideoARMode_PreservePicture);
              if FAILED(hr) then
                raise Exception.Create('SetAspectRatioMode failed!');
              hr := m_pVideoDisplay.SetVideoPosition(nil, rcd);
              if FAILED(hr) then
                raise Exception.Create('SetVideoPosition failed!');
            end;
        end;
   end;

  FTimer.Enabled := true;
  // since the topology is ready, start playback
  hr := Play();

except
  // All other exceptions
  Raise;
end;
finally
  rcd := Nil;
  Result := hr;
end;
end;


function TMfPlayer.OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
begin
  {implement something here}
  Result := S_OK;
end;


function TMfPlayer.OnPause(pEvent: IMFMediaEvent): HRESULT;
begin
  m_state.Command := CmdPause;
  UpdatePendingCommands(CmdPause);
  Result := S_OK;
end;


function TMfPlayer.OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
begin
  Stop;
  Result := S_OK;
end;


function TMfPlayer.OnSessionEvent(pEvent: IMFMediaEvent; meType: DWord): HRESULT;
begin
  //this more or less a dummy
  //but feel free to add your own sessionevent handlers here
  Result := S_OK;
end;


function TMfPlayer.OnStart(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(CmdStart);
  Result := S_OK;
end;


function TMfPlayer.OnStop(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(CmdStop);
  Result := S_OK;
end;

function TMfPlayer.HandleEvent(pEventPtr: UINT_PTR): HRESULT;
var
  hrStatus: HRESULT;
  TopoStatus: UINT32; // MF_TOPOSTATUS
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
    MESessionTopologySet   : hr := OnTopologySet(pEvent);
    MESessionTopologyStatus: begin
                               hr := (pEvent as IMFAttributes).GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                                                                         TopoStatus);
                               if SUCCEEDED(hr) then
                                  if TopoStatus = UINT32(MF_TOPOSTATUS_READY) then
                                    hr := OnTopologyReady(pEvent);
                               OnTopologyStatus(pEvent);
                             end;
    MEEndOfPresentation    : hr := OnPresentationEnded(pEvent);
    MENewPresentation      : hr := OnNewPresentation(pEvent);
    MESessionStopped       : hr := OnStop(pEvent);
    MESessionStarted       : hr := OnStart(pEvent);
    MESessionPaused        : hr := OnPause(pEvent)
  else
    hr := OnSessionEvent(pEvent, DWord(meType));
  end;

done:
  Result := hr;
end;


// The main routine to play a mediafile
function TMfPlayer.OpenURL(sURL: PWideChar): HRESULT;
var
  pTopology: IMFTopology;
  pSourcePD: IMFPresentationDescriptor;
  hr: HRESULT;
  pwInt: PWideChar;

label
  done;

begin

  // 1. Create a new media session.
  // 2. Create the media source.
  // 3. Create the topology.
  // 4. Queue the topology [asynchronous]
  // 5. Start playback [asynchronous - does not happen in this method.]

  hr := S_OK;

try

  FFilename := sURL;

  // Create the media session.
  hr := CreateSession();
  if (FAILED(hr)) then
    goto done;

  // Create the media source.
  hr := CreateMediaSource(sURL, m_pSource);
  if (FAILED(hr)) then
    goto done;

  // Create the presentation descriptor for the media source
  hr := m_pSource.CreatePresentationDescriptor(pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // Create a partial topology, com branches, source nodes and sink nodes.
  hr := CreatePlaybackTopology(m_pSource,
                               pSourcePD,
                               m_hwndVideo,
                               pTopology);
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
  hr := pSourcePD.GetUINT64(MF_PD_DURATION,
                            UINT64(m_hnsDuration));

  // obtain capabilities of the current session
  hr := m_pSession.GetSessionCapabilities(m_dCaps);
  if FAILED(hr) then
   begin
     m_dCaps := $0001;
     goto done;
   end;

  // Set our state to "open pending"
  m_state.Command := CmdOpenPending;
  m_state.State := OpenPending;

done:
  if (FAILED(hr)) then
    begin
      m_state.Command := CmdClosed;
      Clear();
      Abort; // throw silent exception
    end;

  Result := hr;

except
  pwInt := PWideChar(InttoStr(hr));
  Result := hr;
  MessageBox(0,
             PWideChar('An error has been returned. (hr: ' + pwInt + ')'),
             PWideChar(''),
             MB_ICONERROR);
end;
end;


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
    m_Request.Command := CmdPause
  else
    begin
      hr := m_pSession.Pause();
      m_state.command := CmdPause;
      m_State.State := Paused;
      m_bPending := true;
    end;
  Result := hr;
end;


// Start playback from the current position.
//==========================================
function TMfPlayer.StartPlayback(): HRESULT;
var
  hr: HRESULT;
  varStart: PROPVARIANT;

begin
  hr := S_OK;

try
  // Debug issue
  Assert(m_pSession <> Nil);

  if m_bPending then
    m_Request.command := CmdStart
  else
    begin
      PropVariantInit(varStart);
      hr := m_pSession.Start(GUID_NULL, varStart);

      m_bPending := True;
      PropVariantClear(varStart);
    end;

  if (SUCCEEDED(hr)) then
    begin
      // Note: Start is an asynchronous operation. However, we
      // can treat our state as being already started. If Start
      // fails later, we'll get an MESessionStarted event with
      // an error code, and we will update our state then.

      m_state.State := Started;
      m_State.Command := CmdStart;

      // Get initial volume
      GetVolume();
    end;

finally
  Result := hr;
end;
end;


function TMfPlayer.Play(): HRESULT;
var
  hr: HRESULT;

begin
  if (m_pSession = nil) or (m_pSource = nil) then
    hr := E_UNEXPECTED
  else
    hr := StartPlayback();

  Result := hr;
  // The Start method can also specify a starting position relative to the start
  // of the file; see the API reference topic for more information.
end;


// Repaint the video window.
// Call this method on WM_PAINT from the form where the video is playing on.
function TMfPlayer.Repaint(): HRESULT;
begin
  if Assigned(m_pVideoDisplay) then
    Result := m_pVideoDisplay.RepaintVideo()
  else
    Result := S_OK;
end;


function TMfPlayer.ResizeVideo(dRect: LPRECT): HRESULT;
var
  rcpdest: LPRECT;
  rc: TRect;
  hr: HResult;

begin
  Result := E_NOINTERFACE;

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

      //if (SUCCEEDED(hr)) then
      UpdateCaption();
      //else
      //  raise Exception.Create('Error ' + SysErrorMessage(hr));

      // Start repaint again
      SetRedraw();
      rcpdest := Nil;
      Result := hr;
    end;
end;


procedure TMfPlayer.SetRedraw();
begin

  //Stop flickering of controls and subtitle when resizing.
  if (stRedrawStatus = rdStarted) then
    begin
      SendMessage(m_hwndVideo,
                  WM_SETREDRAW,
                  WPARAM(False),
                  0);
      stRedrawStatus := rdStopped;
    end
  else
    begin
      SendMessage(m_hwndVideo,
                  WM_SETREDRAW,
                  WPARAM(True),
                  0);

      RedrawWindow(m_hwndVideo,
                   Nil,
                   0,
                   RDW_ERASE OR RDW_FRAME OR RDW_INVALIDATE OR RDW_ALLCHILDREN);

      stRedrawStatus := rdStarted;
    end;
end;


function TMfPlayer.SendPlayerCommand(cmd: TCommand): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;
  case cmd of
    CmdNone:         {do an implemention here};
    CmdStop:          hr := self.Stop();
    CmdStart:         hr := self.Play();
    CmdStartPlayBack: hr := self.StartPlayBack();
    CmdPause:         hr := self.Pause();
    CmdSeek:         {do an implemention here};
  else
    hr := E_NOTIMPL;
  end;
  Result := hr;
end;


function TMfPlayer.SetPositionInternal(tPos: MFTIME): HRESULT;
var
  varStart: PROPVARIANT;
  hr: HRESULT;

begin
  hr := S_OK;

try

  PropVariantInit(varStart);

  if (m_pSession = nil) then
    begin
      hr := MF_E_INVALIDREQUEST;
      Exit;
    end;

  varStart.vt := VT_I8;
  varStart.hVal.QuadPart := tPos;

  hr := m_pSession.Start(GUID_NULL,
                         varStart);

  if (SUCCEEDED(hr)) then
    begin
      // Store the pending state
      m_state.hnsStart := tPos;
      m_State.Command := CmdStart;
      m_bPending := True;
      UpdateCaption();
    end;

finally
  PropVariantClear(varStart);
  Result := hr;
end;
end;

{$HINTS OFF}
procedure TMfPlayer.SetVolume(Value: FLOAT);
var
  pVol: IMFAudioStreamVolume;
  nChannels: UINT32;
  fVolume: FLOAT;
  // faVolumes: array of Float;  // example, see comment below
  hr: HRESULT;   // for debugging
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

  if SUCCEEDED(hr) then
    begin
      for i := 0 to nChannels - 1 do
        hr := pVol.SetChannelVolume(i,
                                    fVolume);
      // Only 1 channel is selected; to do both you could use an array with channels
      // by using the GetAllVolumes function.
      if SUCCEEDED(hr) then
        hr := pVol.GetChannelVolume(0,
                                    m_fVolume);

      // or

      // Set the length of the array
      // SetLength(faVolumes, nChannels); // MUST DO!
      // hr := pVol.GetAllVolumes(nChannels,
      //                         @faVolumes[0]);

    end;
end;
{$HINTS ON}

function TMfPlayer.ShutDown(): HRESULT;
var
  hr: HRESULT;

begin

  // Close the session.
  hr := CloseSession();

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

end;


function TMfPlayer.Stop(): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;
  if Assigned(m_pSession) then
    begin
      if m_bPending then
        m_Request.Command := CmdStop
      else
        begin
          hr := m_pSession.Stop;
          m_State.command := CmdStop;
          m_State.State := Stopped;
          m_bPending := true;
        end;
    end
  else
    hr := E_POINTER;

  Result := hr;
end;


procedure TMfPlayer.TimerTimer(sender: TObject);
begin
  UpdateCaption();
end;

// Called after an operation completes.
// This method executes any cached requests.

procedure TMfPlayer.UpdateCaption();
var
  sfilename: string;

begin

  // convert to PWideChar (this is the best way to translate a string to PWideChar.
  // To prevent error $80070002 (-2147024894) you should use this function instead of using
  // something like PWideChar(stringvalue);
  sfilename := ExtractFileName(FFileName);

  case Self.GetState of
    OpenPending: SetWindowText(m_hwndVideo,
                               'Opening media... ' +
                               ExtractFileName(FFileName) + '"');

    Ready:       SetWindowText(m_hwndVideo,
                               'Session is ready.');

    Closing:     SetWindowText(m_hwndVideo,
                               'Closing session...');
    Started:     begin
                   SetWindowText(m_hwndVideo,
                                 ExtractFileName(FFileName) + '  ' +
                                 'Duration: ' + HnsTimeToStr(m_hnsDuration, false));
                 end;
    Paused:      SetWindowText(m_hwndVideo,
                               'Paused.');

    Stopped:     SetWindowText(m_hwndVideo,
                               'Stopped.');

    Closed:      SetWindowText(m_hwndVideo,
                               'Session closed.');

   end;
end;


function TMfPlayer.UpdatePendingCommands(cmd: TCommand): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

try
  if (m_bPending) and (m_State.Command = cmd) then
    begin
      m_bPending := False;
      // The current pending command has completed.
      // Now look for seek requests.
      if not m_bPending then
        case m_Request.command of
          CmdNone: ; // Nothing to do.
          CmdStart: Play();
          CmdPause: Pause();
          CmdStop:  Stop();
          CmdSeek:  SetPositionInternal(m_Request.hnsStart);
        end;
      m_Request.command := CmdNone;
    end;

finally
  Result := hr;
end;
end;

end.

