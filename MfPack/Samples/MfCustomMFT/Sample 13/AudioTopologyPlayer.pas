// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: AudioTopologyPlayer.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Controls seekable audio playback through the registered
//              delay MFT, including pause, stop, and timeline events.
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
unit AudioTopologyPlayer;

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
  WM_AUDIO_TOPOLOGY_EVENT = WM_APP + 411;

type
  TAudioTopologyEventKind = (atekReady,
                             atekStarted,
                             atekPaused,
                             atekStopped,
                             atekEnded,
                             atekError);

  TAudioTopologyNotice = class
  public
    SessionNumber: Cardinal;
    Kind: TAudioTopologyEventKind;
    Text: string;

    constructor Create(const ASessionNumber: Cardinal;
                       const AKind: TAudioTopologyEventKind;
                       const AText: string);
  end;

  IAudioTopologyPlayer = interface
  ['{C49EE2B9-2359-420D-8E4D-B7CCB898B0B3}']
    procedure Open(const AFileName: string;
                   const ADelayMs, AWetPercent: Cardinal);
    procedure Start(const AFromBeginning: Boolean);
    procedure Pause();
    procedure Stop();
    procedure Seek(const APosition: Int64);
    function GetTimeline(out APosition, ADuration: Int64;
                         out ACanSeek: Boolean): Boolean;
    procedure UpdateEffect(const ADelayMs, AWetPercent: Cardinal);
    procedure Close(const ADetachWindow: Boolean);
  end;


  function CreateAudioTopologyPlayer(const ANotifyWindow: HWND;
                                     const ASessionNumber: Cardinal): IAudioTopologyPlayer;


implementation

uses
  {ActiveX}
  WinApi.ActiveX,
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  AudioDelayRegistration;

type
  TAudioTopologyPlayer = class(TInterfacedObject,
                               IAudioTopologyPlayer,
                               IMFAsyncCallback)
  private
    FLock: TRTLCriticalSection;
    FNotifyWindow: HWND;
    FSessionNumber: Cardinal;
    FSession: IMFMediaSession;
    FSource: IMFMediaSource;
    FTransform: IMFTransform;
    FClosedEvent: THandle;
    FDuration: Int64;
    FLastPosition: Int64;
    FCanSeek: Boolean;

    procedure Check(const AOperation: string; const AHr: HResult);
    procedure PostNotice(const AKind: TAudioTopologyEventKind;
                         const AText: string);
    function CreateMediaSource(const AFileName: string;
                               out ASource: IMFMediaSource): HResult;
    procedure ReadSourceTimeline(const ASource: IMFMediaSource;
                                 out ADuration: Int64;
                                 out ACanSeek: Boolean);
    function ActivateRegisteredTransform(out ATransform: IMFTransform): HResult;
    function CreatePlaybackTopology(const ASource: IMFMediaSource;
                                    const ADelayMs, AWetPercent: Cardinal;
                                    out ATopology: IMFTopology;
                                    out ATransform: IMFTransform): HResult;
    function GetParameters(out pdwFlags, pdwQueue: DWORD): HResult; stdcall;
    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

  public

    constructor Create(const ANotifyWindow: HWND;
                       const ASessionNumber: Cardinal);
    destructor Destroy(); override;

    procedure Open(const AFileName: string;
                   const ADelayMs, AWetPercent: Cardinal);
    procedure Start(const AFromBeginning: Boolean);
    procedure Pause();
    procedure Stop();
    procedure Seek(const APosition: Int64);
    function GetTimeline(out APosition, ADuration: Int64;
                         out ACanSeek: Boolean): Boolean;
    procedure UpdateEffect(const ADelayMs, AWetPercent: Cardinal);
    procedure Close(const ADetachWindow: Boolean);
  end;


constructor TAudioTopologyNotice.Create(const ASessionNumber: Cardinal;
                                        const AKind: TAudioTopologyEventKind;
                                        const AText: string);
begin

  inherited Create();

  SessionNumber := ASessionNumber;
  Kind := AKind;
  Text := AText;
end;


function CreateAudioTopologyPlayer(const ANotifyWindow: HWND;
                                   const ASessionNumber: Cardinal): IAudioTopologyPlayer;
begin

  Result := TAudioTopologyPlayer.Create(ANotifyWindow,
                                        ASessionNumber);
end;


constructor TAudioTopologyPlayer.Create(const ANotifyWindow: HWND;
                                        const ASessionNumber: Cardinal);
begin

  inherited Create();

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


destructor TAudioTopologyPlayer.Destroy();
begin

  Close(True);

  if (FClosedEvent <> 0) then
    CloseHandle(FClosedEvent);

  DeleteCriticalSection(FLock);

  inherited;
end;


procedure TAudioTopologyPlayer.Check(const AOperation: string;
                                     const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: HRESULT 0x%.8x',
                              [AOperation, Cardinal(AHr)]);
end;


function TAudioTopologyPlayer.CreateMediaSource(const AFileName: string;
                                                out ASource: IMFMediaSource): HResult;
var
  Resolver: IMFSourceResolver;
  SourceObject: IUnknown;
  ObjectType: MF_OBJECT_TYPE;

begin

  ASource := nil;
  ObjectType := MF_OBJECT_INVALID;
  Result := MFCreateSourceResolver(Resolver);

  if SUCCEEDED(Result) then
    Result := Resolver.CreateObjectFromURL(PWideChar(AFileName),
                                           DWORD(MF_RESOLUTION_MEDIASOURCE),
                                           nil,
                                           ObjectType,
                                           SourceObject);
  if SUCCEEDED(Result) then
    Result := SourceObject.QueryInterface(IID_IMFMediaSource,
                                          ASource);
end;


procedure TAudioTopologyPlayer.ReadSourceTimeline(const ASource: IMFMediaSource;
                                                  out ADuration: Int64;
                                                  out ACanSeek: Boolean);
var
  Characteristics: DWORD;
  Presentation: IMFPresentationDescriptor;
  DurationValue: UINT64;

begin

  ADuration := 0;
  ACanSeek := False;
  Characteristics := 0;

  if SUCCEEDED(ASource.GetCharacteristics(@Characteristics)) then
    ACanSeek := (Characteristics and DWORD(MFMEDIASOURCE_CAN_SEEK)) <> 0;

  // Duration is optional. Without it the fixed-range slider has no useful
  // mapping from its thumb position to presentation time.
  if SUCCEEDED(ASource.CreatePresentationDescriptor(Presentation)) and
     SUCCEEDED(Presentation.GetUINT64(MF_PD_DURATION,
                                      DurationValue)) and
     (DurationValue > 0) and (DurationValue <= UINT64(High(Int64))) then
    ADuration := Int64(DurationValue)
  else
    ACanSeek := False;
end;


function TAudioTopologyPlayer.ActivateRegisteredTransform(out ATransform: IMFTransform): HResult;
var
  AudioType: MFT_REGISTER_TYPE_INFO;
  Activates: PIMFActivateArray;
  Count: UINT32;
  I: Integer;
  CandidateClsid: TGUID;
  Found: Boolean;

begin

  ATransform := nil;
  // Filter to synchronous PCM audio effects, then select this sample's CLSID.
  // Enumeration alone does not load the DLL; ActivateObject does.
  AudioType.guidMajorType := MFMediaType_Audio;
  AudioType.guidSubtype := MFAudioFormat_PCM;
  Activates := nil;
  Count := 0;

  Result := MFTEnumEx(MFT_CATEGORY_AUDIO_EFFECT,
                      MFT_ENUM_FLAG_SYNCMFT,
                      @AudioType,
                      @AudioType,
                      Activates,
                      Count);
  if FAILED(Result) then
    Exit;

  Found := False;

  try
    for I := 0 to Integer(Count) - 1 do
      if SUCCEEDED(Activates^[I].GetGUID(MFT_TRANSFORM_CLSID_Attribute,
                                        CandidateClsid)) and
         IsEqualGUID(CandidateClsid,
                     CLSID_FactoryXAudioDelayMFT) then
        begin
          Found := True;
          Result := Activates^[I].ActivateObject(IID_IMFTransform,
                                                 Pointer(ATransform));
          Break;
        end;

    if not Found then
      Result := MF_E_TOPO_CODEC_NOT_FOUND;

  finally
    if Assigned(Activates) then
      begin
        // CoTaskMemFree owns the array only. Each IMFActivate has a separate
        // COM reference that must be released first.
        for I := 0 to Integer(Count) - 1 do
          Activates^[I] := nil;
        CoTaskMemFree(Activates);
      end;
  end;
end;


function TAudioTopologyPlayer.CreatePlaybackTopology(const ASource: IMFMediaSource;
                                                     const ADelayMs, AWetPercent: Cardinal;
                                                     out ATopology: IMFTopology;
                                                     out ATransform: IMFTransform): HResult;
var
  Presentation: IMFPresentationDescriptor;
  Stream: IMFStreamDescriptor;
  AudioStream: IMFStreamDescriptor;
  Handler: IMFMediaTypeHandler;
  SourceNode: IMFTopologyNode;
  TransformNode: IMFTopologyNode;
  OutputNode: IMFTopologyNode;
  Renderer: IMFActivate;
  Control: IMfAudioDelayControl;
  MajorType: TGUID;
  StreamCount: DWORD;
  I: DWORD;
  Selected: BOOL;

begin

  ATopology := nil;
  ATransform := nil;
  AudioStream := nil;

  Result := ASource.CreatePresentationDescriptor(Presentation);
  if FAILED(Result) then
    Exit;

  Result := Presentation.GetStreamDescriptorCount(StreamCount);
  if FAILED(Result) then
    Exit;

  if (StreamCount = 0) then
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

      if not Assigned(AudioStream) and IsEqualGUID(MajorType,
                                                   MFMediaType_Audio) then
        begin
          AudioStream := Stream;
          Result := Presentation.SelectStream(I);
        end
      else
        Result := Presentation.DeselectStream(I);
    if FAILED(Result) then
      Exit;
    end;

  if not Assigned(AudioStream) then Exit(MF_E_INVALIDMEDIATYPE);

  Result := MFCreateTopology(ATopology);
  if FAILED(Result) then
    Exit;

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
                                    AudioStream);
  if SUCCEEDED(Result) then
    Result := ATopology.AddNode(SourceNode);
  if FAILED(Result) then
    Exit;

  Result := ActivateRegisteredTransform(ATransform);
  if FAILED(Result) then
    Exit;

  // Set the initial effect before the Media Session starts pulling samples.
  // The form uses the same interface later when a slider moves.
  Control := ATransform as IMfAudioDelayControl;
  Result := Control.SetEffect(ADelayMs,
                              AWetPercent);

  if SUCCEEDED(Result) then
    Result := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                                   TransformNode);

  if SUCCEEDED(Result) then
    Result := TransformNode.SetObject(ATransform);

  if SUCCEEDED(Result) then
    Result := ATopology.AddNode(TransformNode);

  if FAILED(Result) then
    Exit;

  Result := MFCreateAudioRendererActivate(Renderer);

  if SUCCEEDED(Result) then
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

  if SUCCEEDED(Result) then
    Result := SourceNode.ConnectOutput(0,
                                       TransformNode,
                                       0);
  if SUCCEEDED(Result) then
    Result := TransformNode.ConnectOutput(0,
                                          OutputNode,
                                          0);
end;

procedure TAudioTopologyPlayer.Open(const AFileName: string;
                                    const ADelayMs, AWetPercent: Cardinal);
var
  hr: HResult;
  Source: IMFMediaSource;
  Topology: IMFTopology;
  Transform: IMFTransform;
  Session: IMFMediaSession;
  Duration: Int64;
  CanSeek: Boolean;

begin

  Close(False);

  try
    Check('Create media source',
          CreateMediaSource(AFileName,
                            Source));
    ReadSourceTimeline(Source,
                       Duration,
                       CanSeek);

    hr := CreatePlaybackTopology(Source,
                                 ADelayMs,
                                 AWetPercent,
                                 Topology,
                                 Transform);

    if (hr = MF_E_TOPO_CODEC_NOT_FOUND) then
      raise Exception.Create(FACTORYX_AUDIO_DELAY_MFT_NAME + ' is not registered. See Sample 12 README.');

    Check('Create playback topology',
          hr);

    Check('MFCreateMediaSession',
          MFCreateMediaSession(nil,
                               Session));

    EnterCriticalSection(FLock);

    try
      FSource := Source;
      FTransform := Transform;
      FSession := Session;
      FDuration := Duration;
      FLastPosition := 0;
      FCanSeek := CanSeek;
      ResetEvent(FClosedEvent);

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
    Close(False);
    raise;
  end;
end;


procedure TAudioTopologyPlayer.PostNotice(const AKind: TAudioTopologyEventKind;
                                          const AText: string);
var
  WindowHandle: HWND;
  Notice: TAudioTopologyNotice;

begin

  EnterCriticalSection(FLock);

  try
    WindowHandle := FNotifyWindow;

  finally
    LeaveCriticalSection(FLock);
  end;

  if (WindowHandle = 0) then
    Exit;

  Notice := TAudioTopologyNotice.Create(FSessionNumber,
                                        AKind,
                                        AText);

  if not PostMessage(WindowHandle,
                     WM_AUDIO_TOPOLOGY_EVENT,
                     0,
                     LPARAM(Notice)) then
    Notice.Free;
end;


function TAudioTopologyPlayer.GetParameters(out pdwFlags,
                                            pdwQueue: DWORD): HResult;
begin

  pdwFlags := 0;
  pdwQueue := 0;
  Result := E_NOTIMPL;
end;


function TAudioTopologyPlayer.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
  Session: IMFMediaSession;
  MediaEvent: IMFMediaEvent;
  EventType: MediaEventType;
  EventStatus: HResult;
  TopologyStatus: UINT32;

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
      PostNotice(atekError,
                 Format('Media Session event failed: HRESULT 0x%.8x',
                        [Cardinal(EventStatus)]));
      Exit(S_OK);
    end;

  case EventType of
    MESessionTopologyStatus:
      begin
        TopologyStatus := 0;

        if SUCCEEDED((MediaEvent as IMFAttributes).GetUINT32(MF_EVENT_TOPOLOGY_STATUS, TopologyStatus)) and
                     (TopologyStatus = UINT32(MF_TOPOSTATUS_READY)) then
          PostNotice(atekReady,
                     'Audio delay topology is ready.');
      end;
    MESessionStarted:    PostNotice(atekStarted, 'Playback started.');
    MESessionPaused:     PostNotice(atekPaused, 'Playback paused.');
    MESessionStopped:    PostNotice(atekStopped, 'Playback stopped.');
    MEEndOfPresentation: PostNotice(atekEnded, 'End of presentation.');
  end;

  Result := S_OK;
end;


procedure TAudioTopologyPlayer.Start(const AFromBeginning: Boolean);
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
    if AFromBeginning then
      begin
        // VT_EMPTY resumes at the current position. After Stop or the end of
        // a file that position may still be the end, so request time zero.
        Position.vt := VT_I8;
        Position.hVal.QuadPart := 0;
      end;

    Check('Start Media Session',
          Session.Start(GUID_NULL,
                        Position));
  finally
    PropVariantClear(Position);
  end;

  if AFromBeginning then
    begin
      EnterCriticalSection(FLock);
      try
        FLastPosition := 0;
      finally
        LeaveCriticalSection(FLock);
      end;
    end;
end;


procedure TAudioTopologyPlayer.Pause();
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
    Check('Pause Media Session',
           Session.Pause());
end;


procedure TAudioTopologyPlayer.Stop();
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


procedure TAudioTopologyPlayer.Seek(const APosition: Int64);
var
  Session: IMFMediaSession;
  Duration: Int64;
  CanSeek: Boolean;
  Position: PROPVARIANT;

begin

  EnterCriticalSection(FLock);
  try
    Session := FSession;
    Duration := FDuration;
    CanSeek := FCanSeek;
  finally
    LeaveCriticalSection(FLock);
  end;

  if not Assigned(Session) or not CanSeek or
     (APosition < 0) or (APosition > Duration) then
    raise Exception.Create('This audio source cannot seek to that position.');

  PropVariantInit(Position);
  try
    Position.vt := VT_I8;
    Position.hVal.QuadPart := APosition;

    // Seeking is a Media Session operation. Do not call ProcessMessage(FLUSH)
    // directly while the session may be processing a sample on another thread.
    Check('Seek Media Session',
          Session.Start(GUID_NULL,
                        Position));
  finally
    PropVariantClear(Position);
  end;

  EnterCriticalSection(FLock);
  try
    FLastPosition := APosition;
  finally
    LeaveCriticalSection(FLock);
  end;
end;


function TAudioTopologyPlayer.GetTimeline(out APosition,
                                          ADuration: Int64;
                                          out ACanSeek: Boolean): Boolean;
var
  Session: IMFMediaSession;
  Clock: IMFClock;
  PresentationClock: IMFPresentationClock;
  ClockTime: MFTIME;

begin

  EnterCriticalSection(FLock);
  try
    Session := FSession;
    ADuration := FDuration;
    ACanSeek := FCanSeek;
    APosition := FLastPosition;

  finally
    LeaveCriticalSection(FLock);
  end;

  Result := Assigned(Session) and (ADuration > 0);
  if not Result then
    Exit;

  if SUCCEEDED(Session.GetClock(Clock)) and
     SUCCEEDED(Clock.QueryInterface(IID_IMFPresentationClock,
                                    PresentationClock)) and
     SUCCEEDED(PresentationClock.GetTime(ClockTime)) then
    begin
      if (ClockTime < 0) then
        ClockTime := 0;

      if (ClockTime > ADuration) then
        ClockTime := ADuration;

      APosition := ClockTime;
      EnterCriticalSection(FLock);
      try
        FLastPosition := ClockTime;

      finally
        LeaveCriticalSection(FLock);
      end;
    end;
end;


procedure TAudioTopologyPlayer.UpdateEffect(const ADelayMs,
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


procedure TAudioTopologyPlayer.Close(const ADetachWindow: Boolean);
var
  Session: IMFMediaSession;
  Source: IMFMediaSource;

begin

  EnterCriticalSection(FLock);

  try
    Session := FSession;
    Source := FSource;

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

  if Assigned(Source) then
    Source.Shutdown;

  EnterCriticalSection(FLock);
  try
    FSession := nil;
    FSource := nil;
    FTransform := nil;
    FDuration := 0;
    FLastPosition := 0;
    FCanSeek := False;

  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
