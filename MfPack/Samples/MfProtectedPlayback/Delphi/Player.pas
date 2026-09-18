// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Player.pas
// Kind: Pascal Unit
// Release date: 19-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Media Foundation PMP playback helper, closely following the Microsoft CPlayer sample.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 19/09/2026 Tony                Delphi translation of ProtectedPlayback.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or higher.
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
// Source: Parts of Microsoft ProtectedPlayback and CPlayer examples
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

unit Player;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.ActiveX.PropIdl,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Evr,
  {Application}
  ContentEnabler;

const
  WM_APP_PLAYER_EVENT = WM_APP + 1;

type
  TPlayerState = (Closed = 0,
                  Ready,
                  OpenPending,
                  Started,
                  Paused,
                  Stopped,
                  Closing);

  IProtectedPlayer = interface(IMFAsyncCallback)
    ['{90EA1D2F-A881-491B-A997-6319D7EE60B1}']
    function OpenURL(const URL: PWideChar): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Shutdown(): HRESULT;
    function HandleEvent(EventPointer: UINT_PTR): HRESULT;
    function GetState(): TPlayerState;
    function Repaint(): HRESULT;

    function ResizeVideo(AWidth: Word;
                         AHeight: Word): HRESULT;

    function HasVideo(): Boolean;
    function GetContentProtectionManager(out Manager: IContentProtectionManagerApp): HRESULT;
  end;

  { Direct Delphi translation of the C++ CPlayer class. }
  TPlayer = class(TInterfacedObject, IMFAsyncCallback, IProtectedPlayer)
  private
    FSession: IMFMediaSession;
    FSource: IMFMediaSource;
    FVideoDisplay: IMFVideoDisplayControl;
    FVideoWindow: HWND;
    FEventWindow: HWND;
    FState: TPlayerState;
    FCloseEvent: THandle;
    FContentProtectionManager: IContentProtectionManagerApp;

    procedure Log(const Text: string);
    procedure LogResult(const Operation: string; HR: HRESULT);
    function Initialize(): HRESULT;
    function CreateSession(): HRESULT;
    function CloseSession(): HRESULT;
    function StartPlayback(): HRESULT;
    function CreateMediaSource(const URL: PWideChar): HRESULT;
    function CreateTopologyFromSource(out Topology: IMFTopology): HRESULT;

    function AddBranchToPartialTopology(Topology: IMFTopology;
                                        SourcePD: IMFPresentationDescriptor;
                                        StreamIndex: DWORD): HRESULT;

    function OnTopologyReady(MediaEvent: IMFMediaEvent): HRESULT;
    function OnPresentationEnded(MediaEvent: IMFMediaEvent): HRESULT;

  protected
    function GetParameters(out Flags: DWORD;
                           out Queue: DWORD): HRESULT; stdcall;
    function Invoke(AsyncResult: IMFAsyncResult): HRESULT; stdcall;

  public

    constructor Create(VideoWindow: HWND;
                       EventWindow: HWND);

    destructor Destroy(); override;

    class function CreateInstance(VideoWindow: HWND;
                                  EventWindow: HWND;
                                  out Player: IProtectedPlayer): HRESULT; static;

    function OpenURL(const URL: PWideChar): HRESULT;
    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Shutdown(): HRESULT;

    function HandleEvent(EventPointer: UINT_PTR): HRESULT;
    function GetState(): TPlayerState;
    function Repaint(): HRESULT;

    function ResizeVideo(AWidth: Word;
                         AHeight: Word): HRESULT;

    function HasVideo(): Boolean;
    function GetContentProtectionManager(out Manager: IContentProtectionManagerApp): HRESULT;
  end;

  function CreateSourceStreamNode(Source: IMFMediaSource;
                                  SourcePD: IMFPresentationDescriptor;
                                  SourceSD: IMFStreamDescriptor;
                                  out Node: IMFTopologyNode): HRESULT;

  function CreateOutputNode(SourceSD: IMFStreamDescriptor;
                            VideoWindow: HWND;
                            out Node: IMFTopologyNode): HRESULT;


implementation

uses

  {WinApi}
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Types,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {Application}
  SampleLog;

procedure TPlayer.Log(const Text: string);
begin

  PostLogMessage(FEventWindow,
                 Text);
end;


procedure TPlayer.LogResult(const Operation: string;
                            HR: HRESULT);
begin

  Log(Operation + ': ' + FormatHR(HR));
end;


constructor TPlayer.Create(VideoWindow, EventWindow: HWND);
begin

  inherited Create;

  FVideoWindow := VideoWindow;
  FEventWindow := EventWindow;
  FState := Ready;
  FCloseEvent := 0;
end;


destructor TPlayer.Destroy();
begin

  Shutdown();

  inherited;
end;


class function TPlayer.CreateInstance(VideoWindow: HWND;
                                      EventWindow: HWND;
                                      out Player: IProtectedPlayer): HRESULT;
var
  Instance: TPlayer;

begin

  Player := nil;
  if (VideoWindow = 0) or (EventWindow = 0) then
    Exit(E_INVALIDARG);

  Instance := TPlayer.Create(VideoWindow,
                             EventWindow);
  Result := Instance.Initialize;

  if Succeeded(Result) then
    Result := Instance.QueryInterface(IProtectedPlayer,
                                      Player);
  if Failed(Result) then
    Instance.Free;
end;


function TPlayer.Initialize(): HRESULT;
begin

  if (FCloseEvent <> 0) then
    Exit(MF_E_ALREADY_INITIALIZED);

  Result := MFStartup(MF_VERSION,
                      MFSTARTUP_FULL);

  LogResult('MFStartup',
            Result);
  if Failed(Result) then
    Exit;

  FCloseEvent := CreateEvent(nil,
                             False,
                             False,
                             nil);
  if (FCloseEvent = 0) then
    Result := HResultFromWin32(GetLastError);
end;


function TPlayer.GetParameters(out Flags: DWORD;
                               out Queue: DWORD): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TPlayer.Invoke(AsyncResult: IMFAsyncResult): HRESULT;
var
  MediaEvent: IMFMediaEvent;
  EventType: MediaEventType;
  EventPointer: Pointer;

begin

  Result := S_OK;
  if not Assigned(FSession) then
    Exit;

  Result := FSession.EndGetEvent(AsyncResult,
                                 MediaEvent);
  if Failed(Result) then
    Exit(S_OK);

  Result := MediaEvent.GetType(EventType);
  if Failed(Result) then
    Exit(S_OK);

  if (EventType = MESessionClosed) then
    SetEvent(FCloseEvent)
  else
    FSession.BeginGetEvent(Self as IMFAsyncCallback,
                           nil);

  if (FState <> Closing) then
    begin
      EventPointer := Pointer(MediaEvent);
      IUnknown(EventPointer)._AddRef;
      PostMessage(FEventWindow,
                  WM_APP_PLAYER_EVENT,
                  WPARAM(EventPointer),
                  0);
    end;

  Result := S_OK;
end;


function TPlayer.OpenURL(const URL: PWideChar): HRESULT;
var
  Topology: IMFTopology;

begin

  Log('Opening media: ' + string(URL));
  Result := CreateSession();

  LogResult('CreateSession',
            Result);
  if Succeeded(Result) then
    begin
      Result := CreateMediaSource(URL);
      LogResult('CreateMediaSource',
                Result);
    end;

  if Succeeded(Result) then
    begin
      Result := CreateTopologyFromSource(Topology);
      LogResult('CreateTopologyFromSource',
                Result);
    end;

  if Succeeded(Result) then
    begin
      Result := FSession.SetTopology(0,
                                     Topology);
      LogResult('IMFMediaSession.SetTopology',
                Result);
    end;

  if Succeeded(Result) then
    FState := OpenPending
  else
    FState := Closed;
end;


function TPlayer.Play(): HRESULT;
begin

  if not (FState in [Paused, Stopped]) then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FSession) or not Assigned(FSource) then
    Exit(E_UNEXPECTED);

  Result := StartPlayback;
end;


function TPlayer.Pause(): HRESULT;
begin

  if (FState <> Started) then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FSession) or not Assigned(FSource) then
    Exit(E_UNEXPECTED);

  Result := FSession.Pause;
  LogResult('IMFMediaSession.Pause',
            Result);

  if Succeeded(Result) then
    FState := Paused;
end;


function TPlayer.Stop(): HRESULT;
begin

  if not (FState in [Started, Paused]) then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FSession) or not Assigned(FSource) then
    Exit(E_UNEXPECTED);

  Result := FSession.Stop();
  LogResult('IMFMediaSession.Stop',
            Result);

  if Succeeded(Result) then
    FState := Stopped;
end;


function TPlayer.Repaint(): HRESULT;
begin

  Result := S_OK;
  if Assigned(FVideoDisplay) then
    Result := FVideoDisplay.RepaintVideo();
end;


function TPlayer.ResizeVideo(AWidth: Word;
                             AHeight: Word): HRESULT;
var
  Destination: TRect;

begin

  Result := S_OK;

  if Assigned(FVideoDisplay) then
    begin
      Destination := Rect(0,
                          0,
                          AWidth,
                          AHeight);

      Result := FVideoDisplay.SetVideoPosition(nil,
                                               @Destination);
    end;
end;


function TPlayer.HandleEvent(EventPointer: UINT_PTR): HRESULT;
var
  MediaEvent: IMFMediaEvent;
  EventStatus: HResult;
  EventType: MediaEventType;
  TopologyStatus: UINT32;

begin

  if (EventPointer = 0) then
    Exit(E_POINTER);

  Pointer(MediaEvent) := Pointer(EventPointer); // Consumes Invoke's AddRef

  try
    Result := MediaEvent.GetType(EventType);

    if Failed(Result) then
      Exit;

    Result := MediaEvent.GetStatus(EventStatus);
    if Failed(Result) then
      Exit;

    Log(Format('Media event: %s (%d), status %s',
               [MediaEventName(EventType), EventType, FormatHR(EventStatus)]));

    if Succeeded(EventStatus) then
      begin
        case EventType of
          MESessionTopologyStatus:
            begin
              Result := MediaEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                                             TopologyStatus);
              if Succeeded(Result) and (TopologyStatus = UINT32(MF_TOPOSTATUS_READY)) then
                Result := OnTopologyReady(MediaEvent);
            end;

          MEEndOfPresentation: Result := OnPresentationEnded(MediaEvent);
        else
          Result := S_OK;
        end;
      end
    else
      Result := EventStatus;

  finally
    MediaEvent := nil;
  end;
end;


function TPlayer.Shutdown(): HRESULT;
begin

  Log('Player shutdown requested.');
  Result := CloseSession();

  if (FCloseEvent <> 0) then
    begin
      MFShutdown();
      CloseHandle(FCloseEvent);
      FCloseEvent := 0;
    end;
end;


function TPlayer.GetContentProtectionManager(out Manager: IContentProtectionManagerApp): HRESULT;
begin

  Manager := FContentProtectionManager;

  if Assigned(Manager) then
    Result := S_OK
  else
    Result := E_FAIL;
end;


function TPlayer.GetState(): TPlayerState;
begin

  Result := FState;
end;


function TPlayer.HasVideo(): Boolean;
begin

  Result := Assigned(FVideoDisplay);
end;


function TPlayer.OnTopologyReady(MediaEvent: IMFMediaEvent): HRESULT;
var
  ServiceResult: HResult;

begin

  FVideoDisplay := nil;
  ServiceResult := MFGetService(FSession,
                                MR_VIDEO_RENDER_SERVICE,
                                IID_IMFVideoDisplayControl,
                                Pointer(FVideoDisplay));

  if Succeeded(ServiceResult) then
    Log('Topology ready; video renderer service is available.')
  else
    Log('Topology ready; no video renderer service (audio-only is valid).');

  FState := Stopped;
  Log('Media is loaded and ready. Press Play to start playback.');
  Result := S_OK;
end;


function TPlayer.OnPresentationEnded(MediaEvent: IMFMediaEvent): HRESULT;
begin

  FState := Stopped;
  Log('Presentation ended.');
  Result := S_OK;
end;


function TPlayer.CreateSession(): HRESULT;
var
  Attributes: IMFAttributes;
  EnablerActivate: IMFActivate;

begin

  Log('Creating Protected Media Path session.');

  Result := CloseSession();
  if Failed(Result) then
    Exit;

  Result := MFCreateAttributes(Attributes,
                               1);
  if Failed(Result) then
    Exit;

  Result := TContentProtectionManager.CreateInstance(FEventWindow,
                                                     FContentProtectionManager);
  if Failed(Result) then
    Exit;

  Result := Attributes.SetUnknown(MF_SESSION_CONTENT_PROTECTION_MANAGER,
                                  FContentProtectionManager as IMFContentProtectionManager);
  if Failed(Result) then
    Exit;

  Result := MFCreatePMPMediaSession(0,
                                    Attributes,
                                    FSession,
                                    EnablerActivate);

  LogResult('MFCreatePMPMediaSession',
            Result);
  if Failed(Result) then
    Exit;

  Result := FSession.BeginGetEvent(Self as IMFAsyncCallback,
                                   nil);
end;


function TPlayer.CloseSession(): HRESULT;
var
  WaitResult: DWord;

begin

  Result := S_OK;
  FVideoDisplay := nil;

  if Assigned(FSession) then
    begin
      Log('Closing media session.');
      FState := Closing;
      Result := FSession.Close;

      if Failed(Result) then
        Exit;

      WaitResult := WaitForSingleObject(FCloseEvent,
                                        5000);
      if (WaitResult = WAIT_TIMEOUT) then
        Log('Media-session close timed out after 5000 ms.');

      if (WaitResult = WAIT_FAILED) then
        Result := HResultFromWin32(GetLastError);
    end;

  if Assigned(FSource) then
    FSource.Shutdown;

  if Assigned(FSession) then
    FSession.Shutdown();

  FSource := nil;
  FSession := nil;
  FContentProtectionManager := nil;
  FState := Closed;
end;


function TPlayer.StartPlayback(): HRESULT;
var
  StartPosition: PROPVARIANT;

begin

  if not Assigned(FSession) then
    Exit(E_UNEXPECTED);

  PropVariantInit(StartPosition);

  try
    StartPosition.vt := VT_EMPTY;
    Result := FSession.Start(GUID_NULL,
                             StartPosition);
    LogResult('IMFMediaSession.Start',
              Result);

    if Succeeded(Result) then
      FState := Started;

  finally
    PropVariantClear(StartPosition);
  end;
end;


function TPlayer.CreateMediaSource(const URL: PWideChar): HRESULT;
var
  SourceResolver: IMFSourceResolver;
  SourceObject: IUnknown;
  ObjectType: MF_OBJECT_TYPE;

begin

  FSource := nil;
  Result := MFCreateSourceResolver(SourceResolver);
  if Failed(Result) then
    Exit;

  Result := SourceResolver.CreateObjectFromURL(URL,
                                               MF_RESOLUTION_MEDIASOURCE,
                                               nil,
                                               ObjectType,
                                               SourceObject);
  if Succeeded(Result) then
    Result := SourceObject.QueryInterface(IMFMediaSource,
                                          FSource);
end;


function TPlayer.CreateTopologyFromSource(out Topology: IMFTopology): HRESULT;
var
  SourcePD: IMFPresentationDescriptor;
  StreamCount: DWORD;
  StreamIndex: DWORD;

begin

  Topology := nil;
  Result := MFCreateTopology(Topology);

  if Succeeded(Result) then
    Result := FSource.CreatePresentationDescriptor(SourcePD);

  if Succeeded(Result) then
    Result := SourcePD.GetStreamDescriptorCount(StreamCount);

  if Failed(Result) then
    Exit;

  Log(Format('Presentation contains %d stream(s).',
             [StreamCount]));

  if (StreamCount > 0) then
    for StreamIndex := 0 to StreamCount - 1 do
    begin
      Result := AddBranchToPartialTopology(Topology,
                                           SourcePD,
                                           StreamIndex);
      if Failed(Result) then
        Exit;
    end;
end;


function TPlayer.AddBranchToPartialTopology(Topology: IMFTopology;
                                            SourcePD: IMFPresentationDescriptor;
                                            StreamIndex: DWORD): HRESULT;
var
  SourceSD: IMFStreamDescriptor;
  SourceNode: IMFTopologyNode;
  OutputNode: IMFTopologyNode;
  Selected: BOOL;
  SelectedText: string;

begin

  Result := SourcePD.GetStreamDescriptorByIndex(StreamIndex,
                                                Selected,
                                                SourceSD);
  if Failed(Result) then
    Exit;

  if Selected then
    SelectedText := 'True'
  else
    SelectedText := 'False';

  Log(Format('Stream %d selected: %s',
             [StreamIndex, SelectedText]));

  if Selected then
    begin
      Result := CreateSourceStreamNode(FSource,
                                       SourcePD,
                                       SourceSD,
                                       SourceNode);
    if Succeeded(Result) then
      Result := CreateOutputNode(SourceSD,
                                 FVideoWindow,
                                 OutputNode);

    if Succeeded(Result) then
      Result := Topology.AddNode(SourceNode);

    if Succeeded(Result) then
      Result := Topology.AddNode(OutputNode);

    if Succeeded(Result) then
      Result := SourceNode.ConnectOutput(0, OutputNode, 0);
    end;
end;


function CreateSourceStreamNode(Source: IMFMediaSource;
                                SourcePD: IMFPresentationDescriptor;
                                SourceSD: IMFStreamDescriptor;
                                out Node: IMFTopologyNode): HRESULT;
begin

  Node := nil;
  if not Assigned(Source) or not Assigned(SourcePD) or
    not Assigned(SourceSD) then
    Exit(E_POINTER);

  Result := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                                 Node);

  if Succeeded(Result) then
    Result := Node.SetUnknown(MF_TOPONODE_SOURCE,
                              Source);

  if Succeeded(Result) then
    Result := Node.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                              SourcePD);

  if Succeeded(Result) then
    Result := Node.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                              SourceSD);
end;

function CreateOutputNode(SourceSD: IMFStreamDescriptor;
                          VideoWindow: HWND;
                          out Node: IMFTopologyNode): HRESULT;
var
  Handler: IMFMediaTypeHandler;
  RendererActivate: IMFActivate;
  MajorType: TGUID;
  StreamID: DWORD;

begin

  Node := nil;
  StreamID := 0;
  SourceSD.GetStreamIdentifier(StreamID);

  Result := SourceSD.GetMediaTypeHandler(Handler);

  if Succeeded(Result) then
    Result := Handler.GetMajorType(MajorType);

  if Succeeded(Result) then
    Result := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                                   Node);

  if Succeeded(Result) then
    begin
      if IsEqualGUID(MajorType,
                     MFMediaType_Audio) then
        Result := MFCreateAudioRendererActivate(RendererActivate)
      else
        if IsEqualGUID(MajorType,
                       MFMediaType_Video) then
          Result := MFCreateVideoRendererActivate(VideoWindow,
                                                  RendererActivate)
        else
          Result := E_FAIL;
    end;

  if Succeeded(Result) then
    Result := Node.SetObject(RendererActivate);
end;

end.
