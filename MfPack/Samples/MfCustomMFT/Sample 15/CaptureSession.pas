// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: CaptureSession.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Combines camera and microphone in an aggregate source
//              for live video preview and delayed audio playback.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// Source: Microsoft Learn.
//
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
unit CaptureSession;

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
  WinApi.MediaFoundationApi.MfIdl;

const
  WM_CAPTURE_EVENT = WM_APP + 411;

type
  TCaptureEventKind = (cekReady,
                       cekStarted,
                       cekStopped,
                       cekEnded,
                       cekError);

  TCaptureNotice = class
  public
    SessionNumber: Cardinal;
    Kind: TCaptureEventKind;
    Text: string;

    constructor Create(const ASessionNumber: Cardinal;
                       const AKind: TCaptureEventKind;
                       const AText: string);
  end;

  ICaptureSession = interface
  ['{7E6F4AAE-3B90-4A32-9A88-C784A0507E71}']
    procedure Open(AVideoDevice, AAudioDevice: IMFActivate;
                   const APreviewWindow: HWND;
                   const ADelayMs, AWetPercent: Cardinal);
    procedure Start();
    procedure Stop();
    procedure UpdateEffect(const ADelayMs, AWetPercent: Cardinal);
    procedure Close(const ADetachWindow: Boolean);
  end;

function CreateCaptureSession(const ANotifyWindow: HWND;
                              const ASessionNumber: Cardinal): ICaptureSession;


implementation

uses
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfAudioDelayMFT;

type
  TCaptureSession = class(TInterfacedObject,
                               ICaptureSession,
                               IMFAsyncCallback)
  private
    FLock: TRTLCriticalSection;
    FNotifyWindow: HWND;
    FSessionNumber: Cardinal;
    FSession: IMFMediaSession;
    FVideoSource: IMFMediaSource;
    FAudioSource: IMFMediaSource;
    FAggregateSource: IMFMediaSource;
    FVideoActivate: IMFActivate;
    FAudioActivate: IMFActivate;
    FTransform: IMFTransform;
    FClosedEvent: THandle;

    procedure Check(const AOperation: string; const AHr: HResult);
    procedure PostNotice(const AKind: TCaptureEventKind;
                         const AText: string);
    function CreateCaptureTopology(const ASource: IMFMediaSource;
                                   const APreviewWindow: HWND;
                                   const ADelayMs, AWetPercent: Cardinal;
                                   out ATopology: IMFTopology;
                                   out ATransform: IMFTransform): HResult;
    function GetParameters(out pdwFlags, pdwQueue: DWORD): HResult; stdcall;
    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

  public

    constructor Create(const ANotifyWindow: HWND;
                       const ASessionNumber: Cardinal);
    destructor Destroy(); override;

    procedure Open(AVideoDevice, AAudioDevice: IMFActivate;
                   const APreviewWindow: HWND;
                   const ADelayMs, AWetPercent: Cardinal);
    procedure Start();
    procedure Stop();
    procedure UpdateEffect(const ADelayMs, AWetPercent: Cardinal);
    procedure Close(const ADetachWindow: Boolean);
  end;


constructor TCaptureNotice.Create(const ASessionNumber: Cardinal;
                                        const AKind: TCaptureEventKind;
                                        const AText: string);
begin
  inherited Create;
  SessionNumber := ASessionNumber;
  Kind := AKind;
  Text := AText;
end;


function CreateCaptureSession(const ANotifyWindow: HWND;
                                   const ASessionNumber: Cardinal): ICaptureSession;
begin

  Result := TCaptureSession.Create(ANotifyWindow,
                                        ASessionNumber);
end;


constructor TCaptureSession.Create(const ANotifyWindow: HWND;
                                        const ASessionNumber: Cardinal);
begin

  inherited Create;

  InitializeCriticalSection(FLock);
  FNotifyWindow := ANotifyWindow;
  FSessionNumber := ASessionNumber;

  FClosedEvent := CreateEvent(nil,
                              True,
                              False,
                              nil);

  if (FClosedEvent = 0) then
    begin
      DeleteCriticalSection(FLock);
      RaiseLastOSError();
    end;
end;


destructor TCaptureSession.Destroy();
begin

  Close(True);

  if (FClosedEvent <> 0) then
    CloseHandle(FClosedEvent);

  DeleteCriticalSection(FLock);

  inherited;
end;


procedure TCaptureSession.Check(const AOperation: string;
                                     const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: $%.8x (0x%.8x): %s',
                              [AOperation,
                               Cardinal(AHr),
                               Cardinal(AHr),
                               SysErrorMessage(Cardinal(AHr))]);
end;


function TCaptureSession.CreateCaptureTopology(
  const ASource: IMFMediaSource;
  const APreviewWindow: HWND;
  const ADelayMs, AWetPercent: Cardinal;
  out ATopology: IMFTopology;
  out ATransform: IMFTransform): HResult;
var
  Presentation: IMFPresentationDescriptor;
  Stream: IMFStreamDescriptor;
  VideoStream: IMFStreamDescriptor;
  AudioStream: IMFStreamDescriptor;
  Handler: IMFMediaTypeHandler;
  MajorType: TGUID;
  StreamCount: DWORD;
  I: DWORD;
  Selected: BOOL;
  Control: IMfAudioDelayControl;

  function AddBranch(const AStream: IMFStreamDescriptor;
                     const AKind: TGUID;
                     const AEffect: IMFTransform): HResult;
  var
    SourceNode: IMFTopologyNode;
    TransformNode: IMFTopologyNode;
    OutputNode: IMFTopologyNode;
    Renderer: IMFActivate;

  begin
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
                                      AStream);
    if SUCCEEDED(Result) then
      Result := ATopology.AddNode(SourceNode);
    if FAILED(Result) then
      Exit;

    if IsEqualGUID(AKind, MFMediaType_Video) then
      Result := MFCreateVideoRendererActivate(APreviewWindow,
                                               Renderer)
    else
      Result := MFCreateAudioRendererActivate(Renderer);
    if FAILED(Result) then
      Exit;

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
    if FAILED(Result) then
      Exit;

    if Assigned(AEffect) then
      begin
        Result := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                                       TransformNode);
        if SUCCEEDED(Result) then
          Result := TransformNode.SetObject(AEffect);
        if SUCCEEDED(Result) then
          Result := ATopology.AddNode(TransformNode);
        if SUCCEEDED(Result) then
          Result := SourceNode.ConnectOutput(0,
                                             TransformNode,
                                             0);
        if SUCCEEDED(Result) then
          Result := TransformNode.ConnectOutput(0,
                                                OutputNode,
                                                0);
      end
    else
      Result := SourceNode.ConnectOutput(0,
                                         OutputNode,
                                         0);
  end;

begin
  ATopology := nil;
  ATransform := nil;
  VideoStream := nil;
  AudioStream := nil;

  // The aggregate source exposes both device streams in one presentation.
  // Both source nodes must refer to this source and this descriptor.
  Result := ASource.CreatePresentationDescriptor(Presentation);
  if FAILED(Result) then
    Exit;

  Result := Presentation.GetStreamDescriptorCount(StreamCount);
  if FAILED(Result) then
    Exit;
  if StreamCount = 0 then
    Exit(MF_E_INVALIDMEDIATYPE);

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

      if not Assigned(VideoStream) and
         IsEqualGUID(MajorType, MFMediaType_Video) then
        begin
          VideoStream := Stream;
          Result := Presentation.SelectStream(I);
        end
      else if not Assigned(AudioStream) and
              IsEqualGUID(MajorType, MFMediaType_Audio) then
        begin
          AudioStream := Stream;
          Result := Presentation.SelectStream(I);
        end
      else
        Result := Presentation.DeselectStream(I);

      if FAILED(Result) then
        Exit;
    end;

  if not Assigned(VideoStream) or not Assigned(AudioStream) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := MFCreateTopology(ATopology);
  if FAILED(Result) then
    Exit;

  ATransform := TMfAudioDelayMFT.Create as IMFTransform;
  Control := ATransform as IMfAudioDelayControl;
  Result := Control.SetEffect(ADelayMs,
                              AWetPercent);
  if SUCCEEDED(Result) then
    Result := AddBranch(VideoStream,
                        MFMediaType_Video,
                        nil);
  if SUCCEEDED(Result) then
    Result := AddBranch(AudioStream,
                        MFMediaType_Audio,
                        ATransform);
end;

procedure TCaptureSession.Open(AVideoDevice, AAudioDevice: IMFActivate;
                               const APreviewWindow: HWND;
                               const ADelayMs, AWetPercent: Cardinal);
var
  VideoSource: IMFMediaSource;
  AudioSource: IMFMediaSource;
  AggregateSource: IMFMediaSource;
  SourceCollection: IMFCollection;
  Topology: IMFTopology;
  Transform: IMFTransform;
  Session: IMFMediaSession;
  Stored: Boolean;

begin

  Close(False);
  Stored := False;

  try
    Check('Activate camera',
          AVideoDevice.ActivateObject(IID_IMFMediaSource,
                                      Pointer(VideoSource)));

    Check('Activate microphone',
          AAudioDevice.ActivateObject(IID_IMFMediaSource,
                                      Pointer(AudioSource)));

    Check('MFCreateCollection',
          MFCreateCollection(SourceCollection));

    Check('Add camera to aggregate source',
          SourceCollection.AddElement(VideoSource));

    Check('Add microphone to aggregate source',
          SourceCollection.AddElement(AudioSource));

    // A Media Session needs one source presentation for synchronized live
    // camera and microphone capture. Two independent source objects in one
    // topology can fail at MESessionStarted with E_UNEXPECTED.
    Check('MFCreateAggregateSource',
          MFCreateAggregateSource(SourceCollection,
                                  AggregateSource));

    Check('Create live capture topology',
          CreateCaptureTopology(AggregateSource,
                                APreviewWindow,
                                ADelayMs,
                                AWetPercent,
                                Topology,
                                Transform));

    Check('MFCreateMediaSession',
          MFCreateMediaSession(nil,
                               Session));

    EnterCriticalSection(FLock);

    try
      FVideoSource := VideoSource;
      FAudioSource := AudioSource;
      FAggregateSource := AggregateSource;
      FVideoActivate := AVideoDevice;
      FAudioActivate := AAudioDevice;
      FTransform := Transform;
      FSession := Session;
      ResetEvent(FClosedEvent);
      Stored := True;

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
    if Stored then
      Close(False)
    else
      begin
        if Assigned(AggregateSource) then
          AggregateSource.Shutdown();
        if Assigned(VideoSource) then
          VideoSource.Shutdown();
        if Assigned(AudioSource) then
          AudioSource.Shutdown();
        AVideoDevice.ShutdownObject();
        AAudioDevice.ShutdownObject();
      end;
    raise;
  end;
end;


procedure TCaptureSession.PostNotice(const AKind: TCaptureEventKind;
                                          const AText: string);
var
  WindowHandle: HWND;
  Notice: TCaptureNotice;

begin

  EnterCriticalSection(FLock);

  try
    WindowHandle := FNotifyWindow;

  finally
    LeaveCriticalSection(FLock);
  end;

  if (WindowHandle = 0) then
    Exit;

  Notice := TCaptureNotice.Create(FSessionNumber,
                                        AKind,
                                        AText);

  if not PostMessage(WindowHandle,
                     WM_CAPTURE_EVENT,
                     0,
                     LPARAM(Notice)) then
    Notice.Free;
end;


function TCaptureSession.GetParameters(out pdwFlags,
                                            pdwQueue: DWORD): HResult;
begin

  pdwFlags := 0;
  pdwQueue := 0;
  Result := E_NOTIMPL;
end;


function TCaptureSession.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  Session: IMFMediaSession;
  MediaEvent: IMFMediaEvent;
  EventType: MediaEventType;
  EventStatus: HResult;
  TopologyStatus: UINT32;

  function EventName(const AType: MediaEventType): string;
  begin
    case AType of
      MESessionTopologyStatus: Result := 'topology';
      MESessionStarted:        Result := 'start';
      MESessionStopped:        Result := 'stop';
      MEEndOfPresentation:     Result := 'end of presentation';
    else
      Result := Format('event %d',
                       [Integer(AType)]);
    end;
  end;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;

  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Session) then
    Exit(S_OK);

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
      PostNotice(cekError,
                 Format('Media Session %s failed: $%.8x (0x%.8x): %s',
                        [EventName(EventType),
                         Cardinal(EventStatus),
                         Cardinal(EventStatus),
                         SysErrorMessage(Cardinal(EventStatus))]));
      Exit(S_OK);
    end;

  case EventType of
    MESessionTopologyStatus:
      begin
        TopologyStatus := 0;

        if SUCCEEDED((MediaEvent as IMFAttributes).GetUINT32(MF_EVENT_TOPOLOGY_STATUS, TopologyStatus)) and
                     (TopologyStatus = UINT32(MF_TOPOSTATUS_READY)) then
          PostNotice(cekReady,
                     'Camera and microphone topology is ready.');
      end;
    MESessionStarted:    PostNotice(cekStarted, 'Live preview and audio started.');
    MESessionStopped:    PostNotice(cekStopped, 'Live preview stopped.');
    MEEndOfPresentation: PostNotice(cekEnded, 'End of presentation.');
  end;

  Result := S_OK;
end;


procedure TCaptureSession.Start();
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


procedure TCaptureSession.Stop();
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
    Check('Stop Media Session',
          Session.Stop());
end;


procedure TCaptureSession.UpdateEffect(const ADelayMs,
                                             AWetPercent: Cardinal);
var
  Transform: IMFTransform;
  Control: IMfAudioDelayControl;

begin

  EnterCriticalSection(FLock);

  try
    Transform := FTransform;

  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Transform) then
    Exit;

  Control := Transform as IMfAudioDelayControl;
  Check('Update delay effect',
        Control.SetEffect(ADelayMs,
                          AWetPercent));
end;


procedure TCaptureSession.Close(const ADetachWindow: Boolean);
var
  Session: IMFMediaSession;
  AggregateSource: IMFMediaSource;
  VideoSource: IMFMediaSource;
  AudioSource: IMFMediaSource;
  VideoActivate: IMFActivate;
  AudioActivate: IMFActivate;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
    AggregateSource := FAggregateSource;
    VideoSource := FVideoSource;
    AudioSource := FAudioSource;
    VideoActivate := FVideoActivate;
    AudioActivate := FAudioActivate;

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
      Session.Shutdown();
    end;

  if Assigned(AggregateSource) then
    AggregateSource.Shutdown();
  if Assigned(VideoSource) then
    VideoSource.Shutdown();
  if Assigned(AudioSource) then
    AudioSource.Shutdown();
  if Assigned(VideoActivate) then
    VideoActivate.ShutdownObject();
  if Assigned(AudioActivate) then
    AudioActivate.ShutdownObject();

  EnterCriticalSection(FLock);
  try
    FSession := nil;
    FAggregateSource := nil;
    FVideoSource := nil;
    FAudioSource := nil;
    FVideoActivate := nil;
    FAudioActivate := nil;
    FTransform := nil;

  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
