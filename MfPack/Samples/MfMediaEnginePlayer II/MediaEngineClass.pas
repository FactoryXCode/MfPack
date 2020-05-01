// FactoryX
//
// Copyright � FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MediaEngineClass.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 2.6.4
// Description: This class uses the IMFMediaEngine based on HTML 5 and
//              the TimedText interfaces for subtitles.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues, (Ciaran).
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 20H1)
// ----------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright � FactoryX, Netherlands/Australia
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit MediaEngineClass;

interface

uses
  Vcl.Forms,
  {WinApi}
  WinApi.Windows,
  Winapi.Messages,
  //WinApi.ActiveX {opt},
  {System}
  System.Classes,
  System.SysUtils,
  System.Win.ComObj,
  System.SyncObjs,
  {MfPack}
  MfPack.MfpUtils,
  MfPack.MfpTypes,
  MfPack.MfApi,
  MfPack.MfIdl,
  MfPack.MfObjects,
  MfPack.MfMediaEngine,
  MfPack.ObjBase,
  MfPack.ComBaseApi,
  MfPack.WinError,
  MfPack.MfpMetLib,  // replacement for MfMethods.pas
  {Application}
  TimedTextNotifyClass;

  {$TYPEINFO ON}

const
   WM_TIMERUPDATE = WM_USER + 1001;


type
  TRedrawStatus = (rdStarted, rdStopped);
  TRenderingState = (rsPlaying, rsPaused, rsStopped, rsFlushed);
  TRequestMsg = (rMsgSetVolume,
                 rMsgSetBalance,
                 rMsgUpdateVideoStream,
                 rMsgFlush,
                 rMsgNone);



  TcMediaEngine = class(TInterfacedPersistent, IMFMediaEngineNotify)
  protected
    {protected fields}
    pt_hwndVideo: HWND;       // Handle of the videosurface
    pt_hwndEvent: HWND;       // Handle of the caller that recieves the events
    pt_hwndCaller: HWND;      // Handle of the caller (mainform)
    pt_hwndSubTObj: HWND;     // Handle of the subtitle object that displays the subtitles
    pt_rcpdest: TRect;        // The window rectangle where the video is played on
    pt_RequestMsg: TRequestMsg;      // Request commands/messages
    pt_CritSec: TCriticalSection;    // Critical section handler
    pt_RedrawStatus: TRedrawStatus;  // Status of the redraw event

    /// IMPLEMENTATION OF IMFMediaEngineNotify /////////////////////////////////
    function EventNotify(event: DWORD;
                         param1: DWORD_PTR;
                         param2: DWORD): HResult; stdcall;

    // EVENT HANDLERS //////////////////////////////////////////////////////////
    procedure OnLoadStart(event: DWORD);
    procedure OnProgress(event: DWORD);
    procedure OnSuspend(event: DWORD);
    procedure OnAbort(event: DWORD);
    procedure OnError(event: DWORD; param1: DWORD_PTR; param2: DWORD);
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
    procedure OnFormatChanged(event: DWORD; param1: DWORD_PTR; param2: DWORD);
    // EXTENSIONS on the HTML5 specs
    procedure OnPurgeQueuedEvents(event: DWORD);
    procedure OnTimeLineMarker(event: DWORD);
    procedure OnBalanceChanged(event: DWORD);
    procedure OnDownloadComplete(event: DWORD);
    procedure OnBufferingStarted(event: DWORD);
    procedure OnBufferingEnded(event: DWORD);
    procedure OnFrameStepCompleted(event: DWORD);
    procedure OnNotifyStableState(event: DWORD; param1: DWORD_PTR; param2: DWORD);
    procedure OnFirstFrameReady(event: DWORD);
    procedure OnTracksChange(event: DWORD);
    procedure OnOpmInfo(event: DWORD);
    procedure OnResourceLost(event: DWORD);
    procedure OnDelayLoadEventChanged(event: DWORD);
    procedure OnStreamRenderingError(event: DWORD);
    procedure OnSupportedRatesChanged(event: DWORD);
    procedure OnAudioEndPointChanged(event: DWORD);

  private
    {private fields}
    pr_Volume: Double;
    pr_Balance: Double;

    // Enables an application to play audio or video files.
    pr_MediaEngine: IMFMEdiaEngineEx;

    // Creates an instance of the Media Engine.
    // Note: Before using this interface, CoInitializeEx and MFStartup should be initialized.
    // To get a pointer to this interface, call CoCreateInstance.
    // The class identifier is CLSID_MFMediaEngineClassFactory.
    //pr_MediaEngineClassFactory: IMFMediaEngineClassFactory;

    // TimedText interface for subtitling
    pr_TimedText: IMFTimedText;


  public
    {public fields}   //IMFMediaEngineClassFactoryEx

    pu_Duration: Double;        // Duration of the mediasource
    pu_CurrPosition: Double;    // The current play position
    pu_SourceURL: WideString;   // The URL of the source that is playing
    pu_bPlayAfterLoaded: Boolean; // When true, de mediasource wil be played immediately
    pu_RenderingState: TRenderingState;
    pu_aStreamCont: TStreamContentsArray;
    pu_VideoFrameAvailable: Boolean; // TRUE if a new videoframe is avaliable.

    // Callback interface
    pr_TimedTextNotify: TcTimedTextNotify;


    // Constructor
    constructor Create(hwndVideo: HWND;
                       hwndEvent: HWND;
                       hwndCaller: HWND;
                       out hres: HResult); overload;
    // Destructor
    destructor Destroy(); override;


    // Loads a mediasource
    function OpenURL(const pwURL: PWideChar;
                     SubTitleFileExt: string = ''): HResult;

    // Play the choosen mediasource
    function Play(): HResult;
    // Pause
    function Pause(): HResult;
    // Stop playing.
    function Stop(): HResult;
    // Stop and release all resources kept by the MediaEngine.
    procedure FlushPlayer();
    // Set audio volume
    procedure SetVolume(dVol: Double);
    // Set audio balance
    procedure SetBalance(dBal: Double);
    // Set mute
    function Mute(var bMute: BOOL): HResult;
    // Set a new play position
    function SetPosition(sTime: Double): HResult;
    //
    function FrameStep(goForward: BOOL): HResult;

    // Frame capture /////////////////////////////////////////////////////
    // It's currently not possible to capture a frame in rendering mode.
    // For this, the MediaEngine needs to be initialized as frame server.
    // However, we created a work around on this See: frmMfMediaEnginePlayer.
    //////////////////////////////////////////////////////////////////////

    procedure SetRedraw();
    function ResizeVideo(nr: TRect): HResult;
    // Queries the Media Engine to find out whether a new video frame is ready.
    function GetVideoStreamTick(): LONGLONG;
    function GetTimedTextInterface(url: string;
                                   fExt: string;
                                   lang: PWideChar): HResult;
  end;


  // Intitialize COM and MF
  function InitMF(): HResult;
  // Close COM and MF
  function CloseMF(): HResult;


implementation


// CONSTRUCTOR
constructor TcMediaEngine.Create(hwndVideo: HWND;
                                 hwndEvent: HWND;
                                 hwndCaller: HWND;
                                 out hres: HResult);
var
  hr: HResult;
  li_Attributes: IMFAttributes; // Attributes store
  li_MediaEngineClassFactory: IMFMediaEngineClassFactory;

begin
  inherited Create();

try
  // store the given handles from the caller.
  pt_hwndVideo := hwndVideo;
  pt_hwndEvent := hwndEvent;
  pt_hwndCaller := hwndCaller;

  // Initialize Media Foundation platform
  hr := InitMF();

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying to initialize Media Foundation.'),
                 LPCWSTR('Fatal Error!'),
                 MB_ICONEXCLAMATION);
      hres := hr;
      Exit();
    end;

  // IMFMediaEngineClassFactory
  // Creates an instance of the Media Engine.
  // Note: Before using this interface, CoInitializeEx and MFStartup should be initialized.
  // To get a pointer to this interface, call CoCreateInstance.
  // The class identifier is CLSID_MFMediaEngineClassFactory.
  //
  // hr := CoCreateInstance(CLSID_MFMediaEngineClassFactory,
  //                        Nil,
  //                        CLSCTX_INPROC_SERVER,
  //                        IID_IMFMediaEngineClassFactory,
  //                        li_MediaEngineClassFactory);
  // Or:
  //
  // In Delphi you could use CreateCOMObject function to do the same.
  // pr_MediaEngineClassFactory is a reference to the IMFMediaEngineClassFactory interface
  li_MediaEngineClassFactory := CreateCOMObject(CLSID_MFMediaEngineClassFactory) as IMFMediaEngineClassFactory;

  if Not Assigned(li_MediaEngineClassFactory) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying initialize the MediaEngineClassFactory interface.'),
                 LPCWSTR('Fatal Error!'),
                 MB_ICONEXCLAMATION);
      hres := E_POINTER;
      Exit();
    end;

  // Call MFCreateAttributes to create the attribute store.
  hres := MFCreateAttributes(li_Attributes,
                             2);

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying to get an attributes store.'),
                 LPCWSTR('Fatal Error!'),
                 MB_ICONEXCLAMATION);
      hres := hr;
      Exit();
     end;

  // Set the required attributes
  // See: https://docs.microsoft.com/windows/desktop/api/mfmediaengine/nf-mfmediaengine-imfmediaengineclassfactory-createinstance for
  // more info about this.

  // Set the callback pointer on the Media Engine
  hres := li_Attributes.SetUnknown(MF_MEDIA_ENGINE_CALLBACK,
                                   Self);
  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying to set the callback pointer on the Media Engine.'),
                 LPCWSTR('Fatal Error!'),
                 MB_ICONEXCLAMATION);
      hres := hr;
      Exit();
    end;

  // Sets a handle to a video playback window for the Media Engine.
  hr := li_Attributes.SetUINT64(MF_MEDIA_ENGINE_PLAYBACK_HWND,
                                pt_hwndVideo);
  if FAILED(hr) then
    begin
      hres := hr;
      Exit();
    end;

  // Create the mediaEngine
  hr := li_MediaEngineClassFactory.CreateInstance(0,
                                                  li_Attributes,
                                                  IUnknown(pr_MediaEngine));

  if FAILED(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('An error occured while trying to get the MediaEngine interface.'),
                 LPCWSTR('Fatal Error!'),
                 MB_ICONEXCLAMATION);
      hres := hr;
      Exit();
     end;


  pt_CritSec := TCriticalSection.Create();

  // Note: local interface declarations will be released if going out of scope.
  //       There is no need to release them manually.

except
  // Silent Exception
  hres := E_UNEXPECTED;
end;

end;


// DESTRUCTOR
destructor TcMediaEngine.Destroy();
begin
  // Release the global interfaces
  pr_MediaEngine := Nil;
  // Release CriticalSection
  FreeAndNil(pt_CritSec);
  // Close the media foundation platform and end COM
  {void} CloseMF();
  inherited Destroy();
end;


//
// EVENT HANDLERS  /////////////////////////////////////////////////////////////
//

// Queries the Media Engine to find out whether a new video frame is ready.


//
procedure TcMediaEngine.OnLoadStart(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnProgress(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnSuspend(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnAbort(event: DWORD);
begin
  pu_RenderingState := rsStopped;
end;


procedure TcMediaEngine.OnError(event: DWORD;
                                param1: DWORD_PTR;
                                param2: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnEmptied(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnStalled(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnPlay(event: DWORD);
begin
  pu_RenderingState := rsPlaying;
end;


procedure TcMediaEngine.OnPause(event: DWORD);
begin
  pu_RenderingState := rsPaused;
end;


procedure TcMediaEngine.OnLoadedMetaData(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnLoadedData(event: DWORD);
begin
  SetWindowText(pt_hwndEvent,
                'Mediafile ' +
                ExtractFileName(pu_SourceURL) +
                ' loaded. (duration ' +
                MfSecToStr(pu_Duration, false) +
                ')');
end;


procedure TcMediaEngine.OnWaiting(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnPlaying(event: DWORD);
begin
  pu_RenderingState := rsPlaying;
end;


procedure TcMediaEngine.OnCanPlay(event: DWORD);
begin
  // Immediately start playing when the source is loaded.
  if pu_bPlayAfterLoaded then
    {void} pr_MediaEngine.Play();

end;


procedure TcMediaEngine.OnCanPlayThrough(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnSeeking(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnSeeked(event: DWORD);
begin
  // Implement code here
end;

{$HINTS OFF}
procedure TcMediaEngine.OnTimeUpdate(event: DWORD);
var
  hr: HResult; // Debug

begin
  hr := S_OK;

  if Not Assigned(pr_MediaEngine) then
    Exit;

  // Request handlers

  if (pt_RequestMsg = rMsgSetVolume) then
    begin
      hr := pr_MediaEngine.SetVolume(pr_Volume);
      pt_RequestMsg := rMsgNone;
    end;

  if (pt_RequestMsg = rMsgSetBalance) then
    begin
      hr := pr_MediaEngine.SetBalance(pr_Balance);
      pt_RequestMsg := rMsgNone;
    end;

  if (pt_RequestMsg = rMsgUpdateVideoStream) then
    begin
      hr := pr_MediaEngine.UpdateVideoStream(Nil,
                                             @pt_rcpdest,
                                             Nil);
      pt_RequestMsg := rMsgNone;
    end;

  if (pt_RequestMsg = rMsgFlush) then
    begin
      hr := pr_MediaEngine.Shutdown();
      pt_RequestMsg := rMsgNone;
      Exit;
    end;

  if Failed(hr) then
    begin
      SetWindowText(pt_hwndEvent,
                    Format('An error (%u) occured in procedure OnTimeUpdate', [hr]));
      Exit;
    end;




  pu_CurrPosition := pr_MediaEngine.GetCurrentTime;
  // Send message to the caller, for instance to update a progressbar
  PostMessage(pt_hwndCaller,
              WM_TIMERUPDATE,
              0,
              0);

  SetWindowText(pt_hwndEvent,
                'Playing file ' + ExtractFileName(pu_SourceURL) +
                '  Duration: ' + MfSecToStr(pu_Duration - pu_CurrPosition, false) +
                ' / ' + MfSecToStr(pu_CurrPosition, true)
                );

end;
{$HINTS ON}

procedure TcMediaEngine.OnEnded(event: DWORD);
begin
  pu_RenderingState := rsStopped;
end;


procedure TcMediaEngine.OnRateChange(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnDurationChange(event: DWORD);
begin
  pu_Duration := pr_MediaEngine.GetDuration();
end;


procedure TcMediaEngine.OnVolumeChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnFormatChanged(event: DWORD;
                                        param1: DWORD_PTR;
                                        param2: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnPurgeQueuedEvents(event: DWORD);
begin
  // Implement code here
end;


procedure  TcMediaEngine.OnTimeLineMarker(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnBalanceChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnDownloadComplete(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnBufferingStarted(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnBufferingEnded(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnFrameStepCompleted(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnNotifyStableState(event: DWORD;
                                            param1: DWORD_PTR;
                                            param2: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnFirstFrameReady(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnTracksChange(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnOpmInfo(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnResourceLost(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnDelayLoadEventChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnStreamRenderingError(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnSupportedRatesChanged(event: DWORD);
begin
  // Implement code here
end;


procedure TcMediaEngine.OnAudioEndPointChanged(event: DWORD);
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
function TcMediaEngine.EventNotify(event: DWORD;
                                   param1: DWORD_PTR;
                                   param2: DWORD): HResult;
var
  hr: HResult;
  eEvent: MfMediaEngineEvent;

begin
  hr:= S_OK;
  eEvent:= MfMediaEngineEvent(event);

  case eEvent of
    // The Media Engine has started to load the source. See: IMFMediaEngine.Load.
    MF_MEDIA_ENGINE_EVENT_LOADSTART:              OnLoadStart(event);

    // The Media Engine is loading the source.
    MF_MEDIA_ENGINE_EVENT_PROGRESS:               OnProgress(event);

    // The Media Engine has suspended a load operation.
    MF_MEDIA_ENGINE_EVENT_SUSPEND:                OnSuspend(event);

    // The Media Engine cancelled a load operation that was in progress.
    MF_MEDIA_ENGINE_EVENT_ABORT:                  OnAbort(event);

    // An error occurred.
    // Event Parameter	   Description
    // ~~~~~~~~~~~~~~~     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // param1	             A member of the MF_MEDIA_ENGINE_ERR enumeration.
    // param2	             A HRESULT error code, or zero.
    //
    MF_MEDIA_ENGINE_EVENT_ERROR:                  OnError(event, param1, param2);

    // The Media Engine has switched to the MF_MEDIA_ENGINE_NETWORK_EMPTY state.
    // This can occur when the IMFMediaEngine.Load method is called,
    // or if an error occurs during the Load method. See: IMFMediaEngine.GetNetworkState.
    MF_MEDIA_ENGINE_EVENT_EMPTIED:                OnEmptied(event);

    // The Load algorithm is stalled, waiting for data.
    MF_MEDIA_ENGINE_EVENT_STALLED:                OnStalled(event);

    // The Media Engine is switching to the playing state. See: IMFMediaEngine.Play.
    MF_MEDIA_ENGINE_EVENT_PLAY:                   OnPlay(event);

    // The media engine has paused. See: IMFMediaEngine.Pause.
    MF_MEDIA_ENGINE_EVENT_PAUSE:                  OnPause(event);

    // The Media Engine has loaded enough source data to determine the duration and
    // dimensions of the source.
    // This is important, because you can't acces this data before this event.
    MF_MEDIA_ENGINE_EVENT_LOADEDMETADATA:         OnLoadedMetaData(event);

    // The Media Engine has loaded enough data to render some content (for example, a video frame).
    MF_MEDIA_ENGINE_EVENT_LOADEDDATA:             OnLoadedData(event);

    // Playback has stopped because the next frame is not available.
    MF_MEDIA_ENGINE_EVENT_WAITING:                OnWaiting(event);

    // Playback has started. See: IMFMediaEngine.Play.
    MF_MEDIA_ENGINE_EVENT_PLAYING:                OnPlaying(event);

    // Playback can start, but the Media Engine might need to stop to buffer more data.
    MF_MEDIA_ENGINE_EVENT_CANPLAY:                OnCanPlay(event);

    // The Media Engine can probably play through to the end of the resource,
    // without stopping to buffer data.
    MF_MEDIA_ENGINE_EVENT_CANPLAYTHROUGH:         OnCanPlayThrough(event);

    // The Media Engine has started seeking to a new playback position.
    // See: IMFMediaEngine.SetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_SEEKING:                OnSeeking(event);

    // The Media Engine has seeked to a new playback position. See: IMFMediaEngine.SetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_SEEKED:                 OnSeeked(event);

    // The playback position has changed. See: IMFMediaEngine.GetCurrentTime.
    MF_MEDIA_ENGINE_EVENT_TIMEUPDATE:             OnTimeUpdate(event);

    // Playback has reached the end of the source. This event is not sent if the GetLoop is TRUE.
    MF_MEDIA_ENGINE_EVENT_ENDED:                  OnEnded(event);

    // The playback rate has changed. See: IMFMediaEngine.SetPlaybackRate.
    MF_MEDIA_ENGINE_EVENT_RATECHANGE:             OnRateChange(event);

    // The duration of the media source has changed. See: IMFMediaEngine.GetDuration.
    MF_MEDIA_ENGINE_EVENT_DURATIONCHANGE:         OnDurationChange(event);

    // The audio volume changed. See: IMFMediaEngine.SetVolume.
    MF_MEDIA_ENGINE_EVENT_VOLUMECHANGE:           OnVolumeChanged(event);

    // The output format of the media source has changed.
    // Event Parameter	   Description
    // ~~~~~~~~~~~~~~~     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // param1	             Zero if the video format changed, 1 if the audio format changed.
    // param2	             Zero.
    //
    MF_MEDIA_ENGINE_EVENT_FORMATCHANGE:           OnFormatChanged(event, param1, param2);

    //
    // EXTENSIONS
    //

    // The Media Engine flushed any pending events from its queue.
    MF_MEDIA_ENGINE_EVENT_PURGEQUEUEDEVENTS:      OnPurgeQueuedEvents(event);

    // The playback position reached a timeline marker.
    // See: IMFMediaEngineEx.SetTimelineMarkerTimer.
    MF_MEDIA_ENGINE_EVENT_TIMELINE_MARKER:        OnTimeLineMarker(event);

    // The audio balance changed. See: IMFMediaEngineEx.SetBalance.
    MF_MEDIA_ENGINE_EVENT_BALANCECHANGE:          OnBalanceChanged(event);

    // The Media Engine has finished downloading the source data.
    MF_MEDIA_ENGINE_EVENT_DOWNLOADCOMPLETE:       OnDownloadComplete(event);

    // The media source has started to buffer data.
    MF_MEDIA_ENGINE_EVENT_BUFFERINGSTARTED:       OnBufferingStarted(event);

    // The media source has stopped buffering data.
    MF_MEDIA_ENGINE_EVENT_BUFFERINGENDED:         OnBufferingEnded(event);

    // The IMFMediaEngineEx.FrameStep method completed.
    MF_MEDIA_ENGINE_EVENT_FRAMESTEPCOMPLETED:     OnFrameStepCompleted(event);

    // The Media Engine's Load algorithm is waiting to start.
    // Event Parameter	   Description
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
    MF_MEDIA_ENGINE_EVENT_NOTIFYSTABLESTATE:      OnNotifyStableState(event, param1, param2);

    // The first frame of the media source is ready to render.
    MF_MEDIA_ENGINE_EVENT_FIRSTFRAMEREADY:        OnFirstFrameReady(event);

    // Raised when a new track is added or removed.
    // Supported in Windows 8.1 and later.
    MF_MEDIA_ENGINE_EVENT_TRACKSCHANGE:           OnTracksChange(event);

    // Raised when there is new information about the Output Protection Manager (OPM).
    // This event will be raised when an OPM failure occurs,
    // but ITA allows fallback without the OPM. In this case, constriction can be applied.
    // This event will not be raised when there is an OPM failure and the fallback also fails.
    // For example, if ITA blocks playback entirely when OPM cannot be established.
    // Supported in Windows 8.1 and later.
    MF_MEDIA_ENGINE_EVENT_OPMINFO:                OnOpmInfo(event);

    // The source URL is deleted or unreachable
    MF_MEDIA_ENGINE_EVENT_RESOURCELOST:           OnResourceLost(event);

    //
    MF_MEDIA_ENGINE_EVENT_DELAYLOADEVENT_CHANGED: OnDelayLoadEventChanged(event);

    // Raised when one of the component streams of a media stream fails.
    // This event is only raised if the media stream contains other component streams that did not fail.
    MF_MEDIA_ENGINE_EVENT_STREAMRENDERINGERROR:   OnStreamRenderingError(event);

    // One oer more of the supported rates changed.
    MF_MEDIA_ENGINE_EVENT_SUPPORTEDRATES_CHANGED: OnSupportedRatesChanged(event);

    // The Audio Endpoint changed. This event occures when user changed,
    // for instance, a headphone or speaker jack
    MF_MEDIA_ENGINE_EVENT_AUDIOENDPOINTCHANGE:    OnAudioEndPointChanged(event);

    else
      begin
        // Unknown Event

      end;
  end;
  Result:= hr;
end;


// Event handler
function TcMediaEngine.GetVideoStreamTick(): LONGLONG;
var
  llstrtick: LONGLONG;

begin
  if Succeeded(pr_MediaEngine.OnVideoStreamTick(llstrtick)) then
    Result := llstrtick
  else
    Result := -1;
end;


// TimedText implementation ////////////////////////////////////////////////////

// Get the TimedText interface if a timedtext file is present.
// call this funtion after the MediaEngine has loaded a mediafile.
function TcMediaEngine.GetTimedTextInterface(url: string;
                                             fExt: string;
                                             lang: PWideChar): HResult;
var
  hres: HResult;
  pwstxtUrl: PWideChar;
  strtxtUrl: String; // temp for conversion
  trackId: DWord;

label done;

begin
  trackId := 0;
  strtxtUrl := ChangeFileExt(url,
                             fExt);

  if not FileExists(strtxtUrl) then
    begin
      hres := ERROR_FILE_NOT_FOUND;
      goto done;
    end;

  pwstxtUrl := PWideChar(strtxtUrl);


  // Get the interface if not done so far.
  if Not Assigned(pr_TimedText) then
    begin

      // Call MFGetService to get the IMFTimedText interface.
      hres := MFGetService(pr_MediaEngine,
                           MF_MEDIA_ENGINE_TIMEDTEXT,
                           IID_IMFTimedText,
                           pr_TimedText);

     if Failed(hres) then
       goto done;

     // Create the TimedText callback interface
     pr_TimedTextNotify := TcTimedTextNotify.Create(pwstxtUrl,
                                                    Nil,
                                                    Nil,
                                                    pt_hwndCaller);

     if Not Assigned(pr_TimedTextNotify) then
       begin
         hres := E_POINTER;
         goto done;
       end;

    end;

  hres := pr_TimedText.AddDataSourceFromUrl(pwstxtUrl,
                                            nil,
                                            nil,
                                            MF_TIMED_TEXT_TRACK_KIND_SUBTITLES, // The kind of timed text track is subtitles.)
                                            True,
                                            trackId);

  if Failed(hres) then
    goto done;


  // Register TimedTextNotify for callbacks
  hres := pr_TimedText.RegisterNotifications(pr_TimedTextNotify);
  if Failed(hres) then
    goto done;

done:
  Result := hres;

end;


// IMPLEMENTED METHODS /////////////////////////////////////////////////////////

function TcMediaEngine.OpenURL(const pwURL: PWideChar;
                               SubTitleFileExt: string = ''): HResult;
var
  hr: HResult;

label done;

begin

  if Not Assigned(pr_MediaEngine) then
    begin
      hr := E_POINTER;
      goto done;
    end;

  if (pwURL = nil) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  // Sets the real time mode used for the next call to SetSource or Load.
  hr := pr_MediaEngine.SetRealTimeMode(True);
  if FAILED(hr) then
    goto done;

  hr := pr_MediaEngine.SetSource(pwURL);
  if FAILED(hr) then
    goto done;

  if SUCCEEDED(hr) then
    begin
      pu_SourceURL := pwURL;
    end;

  // Get the subtitles, if no file exists, continue.
  if (SubTitleFileExt <> '')  then
    begin
      hr := GetTimedTextInterface(pwURL,
                                  SubTitleFileExt,
                                  Nil);

      if FAILED(hr) then
        hr := S_FALSE;
    end;

done:
  Result:= hr;
end;


function TcMediaEngine.Play(): HResult;
var
  hr: HResult;

label
  done;

begin

 if pu_SourceURL <> '' then
   begin
     hr := pr_MediaEngine.EnableTimeUpdateTimer(True);
     if FAILED(hr) then
       goto done;

     hr := pr_MediaEngine.Play();
     if FAILED(hr) then
       goto done;
   end
 else
   hr := E_INVALIDARG;

done:
  Result := hr;
end;


function TcMediaEngine.Pause(): HResult;
begin
  Result := pr_MediaEngine.Pause();
  // Note that after you call Pause, the time returned by GetCurrentTime may not
  // be precisely accurate.
  // Apps that need a frame-accurate position value, such as media editors,
  // should call FrameStep immediately after calling Pause before calling GetCurrentTime.
  FrameStep(True);

  SetWindowText(pt_hwndEvent,
                'PAUSED');
end;


// This needs some explanation:
// There is a slight difference between stop or pause,
// At stop the mediafile will be reset to it's begin and starts all over again when playbutton is pressed.
function TcMediaEngine.Stop(): HResult;
begin
  if SUCCEEDED(pr_MediaEngine.Pause()) then
    begin
      Result := pr_MediaEngine.SetCurrentTime(0.0);
      SetWindowText(pt_hwndEvent,
                    'STOPPED');
    end
 else
   Result := E_FAIL;
end;


// This will free al resources kept by the MediaEngine.
// The caller has to release the player object and re-initialize,
// before playing a new mediasource.
procedure TcMediaEngine.FlushPlayer();
begin
  // Stop the player and release all it's resources
  // This is handled in the OnTime event
  pt_RequestMsg := rMsgFlush;
  while (pt_RequestMsg <> rMsgNone) do
    HandleMessages(GetCurrentThread());
end;


procedure TcMediaEngine.SetVolume(dVol: Double);
begin
  // Result := gi_MediaEngine.SetVolume(dVol); >> Don't use this directly,
  // but let it be done in the eventhandler, to prevent application is going into zombie state.
  pt_RequestMsg := rMsgSetVolume;
  pr_Volume := dVol;
  while (pt_RequestMsg <> rMsgNone) do
    HandleMessages(GetCurrentThread());
end;


procedure TcMediaEngine.SetBalance(dBal: Double);
begin
  // Result := gi_MediaEngine.SetBalance(dBal); >> Don't use this directly,
  // but let it be done in the eventhandler, to prevent application is going into zombie state.
  pt_RequestMsg := rMsgSetBalance;
  pr_Balance := dBal;
  while (pt_RequestMsg <> rMsgNone) do
    HandleMessages(GetCurrentThread());
end;


function TcMediaEngine.Mute(var bMute: BOOL): HResult;
begin
  Result := pr_MediaEngine.SetMuted(bMute);
  // Return the mute state
  bMute := pr_MediaEngine.GetMuted();
end;


function TcMediaEngine.SetPosition(sTime: Double): HResult;
begin
  Result := pr_MediaEngine.SetCurrentTime(sTime);
end;

// Use framestep after pause, to get an accurate playback position
// See: https://docs.microsoft.com/nl-nl/windows/win32/api/mfmediaengine/nf-mfmediaengine-imfmediaengine-getcurrenttime
function TcMediaEngine.FrameStep(goForward: BOOL): HResult;
begin
  Result := pr_MediaEngine.FrameStep(goForward);
end;


procedure TcMediaEngine.SetRedraw();
begin
  // Stop flickering of controls and subtitle when resizing.
  if (pt_RedrawStatus = rdStarted) then
    begin
      SendMessage(pt_hwndVideo,
                  WM_SETREDRAW,
                  WPARAM(False),
                  0);
      pt_RedrawStatus:= rdStopped;
    end
  else
    begin
      SendMessage(pt_hwndVideo,
                  WM_SETREDRAW,
                  WPARAM(True),
                  0);

      RedrawWindow(pt_hwndVideo,
                   Nil,
                   0,
                   RDW_ERASE OR
                   RDW_FRAME OR
                   RDW_INVALIDATE OR
                   RDW_ALLCHILDREN);

      pt_RedrawStatus := rdStarted;
    end;
end;


function TcMediaEngine.ResizeVideo(nr: TRect): HResult;
var
  hr: HResult;  //debug purpose

begin
  hr:= E_NOINTERFACE;
try

  pt_CritSec.Enter;

  if Assigned(pr_MediaEngine) then
    begin

      // Stop repaint
      SetRedraw();

      // We have to wait for a stable frame. If not, the function will "hang" and
      // freezes the application. For this the UpdateVideoStream function call is
      // executed in the OnTick handler

      pt_rcpdest := nr;
      pt_RequestMsg := rMsgUpdateVideoStream;
      // wait for any more events to be processed.
      WaitForSingleObject(pt_hwndVideo,
                          INFINITE);

      // Start repaint again
      SetRedraw();
    end;
finally
  pt_CritSec.Leave;
  Result:= hr;
end;
end;


function InitMF(): HResult;
var
  hr: HResult;

begin
  // Initialize the COM library.
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
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


end.
