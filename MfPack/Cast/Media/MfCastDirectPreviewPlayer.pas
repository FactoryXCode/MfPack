// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastDirectPreviewPlayer.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description: Media Session preview used for routes which do not supply decoded samples
//              to TMfCastWindowPreviewSink. The EVR owns video rendering and
//              Media Foundation's audio renderer supplies the local sound.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 10/08/2026 All                 Extracted reusable Windows UI support.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: MfCast.pas
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: -
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/MPL/2.0/
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
unit MfCastDirectPreviewPlayer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropVarUtil,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.Evr,
  {Cast}
  MfCastMediaInterfaces;

type
  TMfCastDirectPreviewPlayer = class;

  TMfCastDirectPreviewCallback = class(TInterfacedObject, IMFAsyncCallback)
  private
    FOwner: TMfCastDirectPreviewPlayer;

  public

    constructor Create(const AOwner: TMfCastDirectPreviewPlayer);

    procedure Detach();
    function GetParameters(out AFlags: DWORD;
                           out AQueue: DWORD): HRESULT; stdcall;
    function Invoke(AAsyncResult: IMFAsyncResult): HRESULT; stdcall;
  end;

  TMfCastDirectPreviewPlayer = class(TInterfacedObject,
                                     IMfCastDirectPreviewPlayer)
  private
    FSession: IMFMediaSession;
    FSource: IMFMediaSource;
    FVideoDisplay: IMFVideoDisplayControl;
    FAudioVolume: IMFSimpleAudioVolume;
    FCallback: IMFAsyncCallback;
    FCallbackObject: TMfCastDirectPreviewCallback;
    FVideoWindow: HWND;
    FLastError: HRESULT;
    FVolume: Single;
    FMuted: Boolean;
    FClosing: Boolean;
    FTopologyReady: Boolean;

    function SessionEvent(const AAsyncResult: IMFAsyncResult): HRESULT;
    function StartAt(const APosition100ns: Int64;
                     const ASeek: Boolean): HRESULT;
    procedure AcquireRendererServices();
    procedure ApplyAudioSettings();

  public

    constructor Create();
    destructor Destroy(); override;

    function SetWindow(const AWindow: HWND): HRESULT;
    function IsEnabled(): Boolean;
    function IsActive(): Boolean;

    function Open(const ASourceName: string;
                  const AVolume: Single;
                  const AMuted: Boolean): HRESULT;

    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function UpdateVideo(): HRESULT;
  end;


implementation


constructor TMfCastDirectPreviewCallback.Create(const AOwner: TMfCastDirectPreviewPlayer);
begin

  inherited Create();

  FOwner := AOwner;
end;


procedure TMfCastDirectPreviewCallback.Detach();
begin

  FOwner := nil;
end;


function TMfCastDirectPreviewCallback.GetParameters(out AFlags: DWORD;
                                                     out AQueue: DWORD): HRESULT;
begin
  AFlags := 0;
  AQueue := 0;
  Result := E_NOTIMPL;

end;

function TMfCastDirectPreviewCallback.Invoke(AAsyncResult: IMFAsyncResult): HRESULT;
begin

  if Assigned(FOwner) then
    Result := FOwner.SessionEvent(AAsyncResult)
  else
    Result := MF_E_SHUTDOWN;
end;


constructor TMfCastDirectPreviewPlayer.Create();
begin

  inherited Create();

  FVideoWindow := 0;
  FLastError := S_OK;
  FVolume := 1.0;
  FMuted := False;
  FClosing := False;
  FTopologyReady := False;
end;


destructor TMfCastDirectPreviewPlayer.Destroy();
begin

  Stop();

  inherited Destroy();
end;

function TMfCastDirectPreviewPlayer.SetWindow(const AWindow: HWND): HRESULT;
begin

  if (FVideoWindow = AWindow) then
    begin
      Result := UpdateVideo();
      Exit;
    end;

  if (AWindow = 0) then
    Stop();

  FVideoWindow := AWindow;
  if Assigned(FVideoDisplay) then
    begin
      Result := FVideoDisplay.SetVideoWindow(FVideoWindow);
      if SUCCEEDED(Result) then
        Result := UpdateVideo();
    end
  else
    Result := S_OK;
end;


function TMfCastDirectPreviewPlayer.IsEnabled(): Boolean;
begin

  Result := FVideoWindow <> 0;
end;


function TMfCastDirectPreviewPlayer.Open(const ASourceName: string;
                                         const AVolume: Single;
                                         const AMuted: Boolean): HRESULT;
var
  Presentation: IMFPresentationDescriptor;
  Topology: IMFTopology;

begin

  Stop();

  if (Trim(ASourceName) = '') or not IsEnabled() then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  FVolume := AVolume;
  FMuted := AMuted;
  FClosing := False;
  FTopologyReady := False;
  FLastError := S_OK;

  FCallbackObject := TMfCastDirectPreviewCallback.Create(Self);
  FCallback := FCallbackObject;

  Result := MFCreateMediaSession(nil,
                                 FSession);
  if FAILED(Result) then
    begin
      Stop();
      Exit;
    end;

  Result := FSession.BeginGetEvent(FCallback,
                                   nil);
  if FAILED(Result) then
    begin
      Stop();
      Exit;
    end;

  Result := CreateObjectFromUrl(WideString(ASourceName),
                                FSource);
  if FAILED(Result) then
    begin
      Stop();
      Exit;
    end;

  Result := FSource.CreatePresentationDescriptor(Presentation);
  if FAILED(Result) then
    begin
      Stop();
      Exit;
    end;

  Result := CreatePlaybackTopology(FSource,
                                   Presentation,
                                   FVideoWindow,
                                   Topology);
  if FAILED(Result) then
    begin
      Stop();
      Exit;
    end;

  Result := FSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                                 Topology);
  FLastError := Result;
  if FAILED(Result) then
    Stop();
end;


function TMfCastDirectPreviewPlayer.SessionEvent(const AAsyncResult: IMFAsyncResult): HRESULT;
var
  Event: IMFMediaEvent;
  EventType: MediaEventType;
  EventStatus: HRESULT;
  TopologyStatus: UINT32;
  Session: IMFMediaSession;

begin

  Session := FSession;

  if not Assigned(Session) then
    begin
      Result := MF_E_SHUTDOWN;
      Exit;
    end;

  Result := Session.EndGetEvent(AAsyncResult,
                                Event);
  if FAILED(Result) then
    Exit;

  EventStatus := S_OK;
  Event.GetStatus(EventStatus);
  if FAILED(EventStatus) then
    begin
      FLastError := EventStatus;
      OutputDebugString(PChar(Format(
        'MfCast direct Media Session event failed: HRESULT $%.8x',
        [DWORD(EventStatus)])));
    end;

  Result := Event.GetType(EventType);
  if FAILED(Result) then
    Exit;

  if (not FClosing) and Assigned(FSession) then
    FSession.BeginGetEvent(FCallback,
                           nil);

  if FAILED(EventStatus) then
    begin
      Result := EventStatus;
      Exit;
    end;

  if (EventType = MESessionTopologyStatus) then
    begin
      TopologyStatus := 0;

      Result := Event.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                                TopologyStatus);
      if SUCCEEDED(Result) and
         (TopologyStatus = UINT32(MF_TOPOSTATUS_READY)) then
        begin
          FTopologyReady := True;
          AcquireRendererServices();
          ApplyAudioSettings();

          Result := StartAt(0,
                            False);
          FLastError := Result;
        end;
    end
  else
    Result := S_OK;
end;


procedure TMfCastDirectPreviewPlayer.AcquireRendererServices();
begin

  FVideoDisplay := nil;

  if Assigned(FSession) then
    MFGetService(FSession,
                 MR_VIDEO_RENDER_SERVICE,
                 IID_IMFVideoDisplayControl,
                 Pointer(FVideoDisplay));

  FAudioVolume := nil;

  if Assigned(FSession) then
    MFGetService(FSession,
                 MR_POLICY_VOLUME_SERVICE,
                 IID_IMFSimpleAudioVolume,
                 Pointer(FAudioVolume));

  if Assigned(FVideoDisplay) then
    begin
      FVideoDisplay.SetAspectRatioMode(MFVideoARMode_PreservePicture);
      UpdateVideo();
    end;
end;


procedure TMfCastDirectPreviewPlayer.ApplyAudioSettings();
begin

  if Assigned(FAudioVolume) then
    begin
      FAudioVolume.SetMasterVolume(FVolume);
      FAudioVolume.SetMute(FMuted);
    end;
end;

function TMfCastDirectPreviewPlayer.StartAt(const APosition100ns: Int64;
                                            const ASeek: Boolean): HRESULT;
var
  Position: PROPVARIANT;

begin

  if not Assigned(FSession) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  FillChar(Position,
           SizeOf(Position),
           0);

  if ASeek then
    Result := InitPropVariantFromInt64(APosition100ns,
                                       Position)
  else
    Result := S_OK;

  if SUCCEEDED(Result) then
    try
      Result := FSession.Start(GUID_NULL,
                               Position);
    finally
      PropVariantClearSafe(Position);
    end;
end;


function TMfCastDirectPreviewPlayer.Play(): HRESULT;
begin

  Result := StartAt(0,
                    False);
end;


function TMfCastDirectPreviewPlayer.Pause(): HRESULT;
begin

  if Assigned(FSession) then
    Result := FSession.Pause()
  else
    Result := S_FALSE;
end;


function TMfCastDirectPreviewPlayer.Stop(): HRESULT;
begin

  Result := S_OK;
  FClosing := True;
  FTopologyReady := False;

  if Assigned(FCallbackObject) then
    FCallbackObject.Detach();

  FVideoDisplay := nil;
  FAudioVolume := nil;

  if Assigned(FSession) then
    begin
      FSession.Stop();
      FSession.ClearTopologies();
      Result := FSession.Shutdown();
      FSession := nil;
    end;

  if Assigned(FSource) then
    begin
      FSource.Shutdown();
      FSource := nil;
    end;

  FCallbackObject := nil;
  FCallback := nil;
end;


function TMfCastDirectPreviewPlayer.Seek(const APosition100ns: Int64): HRESULT;
var
  Position100ns: Int64;

begin

  Position100ns := APosition100ns;

  if (Position100ns < 0) then
    Position100ns := 0;
  Result := StartAt(Position100ns, True);
end;


function TMfCastDirectPreviewPlayer.SetVolume(const AVolume: Single): HRESULT;
begin

  FVolume := AVolume;

  if (FVolume < 0.0) then
    FVolume := 0.0
  else
    if (FVolume > 1.0) then
      FVolume := 1.0;

  if Assigned(FAudioVolume) then
    Result := FAudioVolume.SetMasterVolume(FVolume)
  else
    Result := S_FALSE;
end;


function TMfCastDirectPreviewPlayer.SetMuted(const AMuted: Boolean): HRESULT;
begin

  FMuted := AMuted;

  if Assigned(FAudioVolume) then
    Result := FAudioVolume.SetMute(FMuted)
  else
    Result := S_FALSE;
end;


function TMfCastDirectPreviewPlayer.UpdateVideo(): HRESULT;
var
  Destination: TRect;

begin

  if not Assigned(FVideoDisplay) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  if not GetClientRect(FVideoWindow,
                       Destination) then
    begin
      Result := HRESULT_FROM_WIN32(GetLastError());
      Exit;
    end;

  Result := FVideoDisplay.SetVideoPosition(nil,
                                           @Destination);
  if SUCCEEDED(Result) then
    Result := FVideoDisplay.RepaintVideo();
end;


function TMfCastDirectPreviewPlayer.IsActive(): Boolean;
begin

  Result := Assigned(FSession) and not FClosing;
end;

end.
