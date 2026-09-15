// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Casts
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastDirectPreviewPlayer.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.1
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
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: MfCast.pas
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
  System.Types,
  Vcl.Graphics,
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
  WinApi.MediaFoundationApi.Evr9,
  {Cast}
  MfCastMediaInterfaces,
  MfCastTypes,
  MfSubtitleCompositor,
  MfVobSubReader,
  TimedTextClass;

type
  TMfCastVideoAlphaBitmapParams = record
    dwFlags: DWORD;
    clrSrcKey: COLORREF;
    rcSrc: TRect;
    nrcDest: TRectF;
    fAlpha: Single;
    dwFilterMode: DWORD;
  end;

  TMfCastVideoAlphaBitmap = record
    GetBitmapFromDC: BOOL;
    Source: Pointer;
    params: TMfCastVideoAlphaBitmapParams;
  end;

  IMfCastVideoMixerBitmap = interface(IUnknown)
    ['{814C7B20-0FDB-4eec-AF8F-F957C8F69EDC}']
    function SetAlphaBitmap(var ABitmap: TMfCastVideoAlphaBitmap): HRESULT; stdcall;
    function ClearAlphaBitmap(): HRESULT; stdcall;
    function UpdateAlphaBitmapParameters(var AParams: TMfCastVideoAlphaBitmapParams): HRESULT; stdcall;
    function GetAlphaBitmapParameters(out AParams: TMfCastVideoAlphaBitmapParams): HRESULT; stdcall;
  end;

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
    FVideoMixerBitmap: IMfCastVideoMixerBitmap;
    FSubtitleCompositor: TMfSubtitleCompositor;
    FSubtitleBitmap: TBitmap;
    FSubtitleBitmapText: string;
    FSubtitleBitmapVisible: Boolean;
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
    function ConfigureAudioSelection(const APresentation: IMFPresentationDescriptor;
                                     const AAudioStreamIndex: DWORD;
                                     const AHasAudioStreamIndex: Boolean): HRESULT;
    procedure AcquireRendererServices();
    procedure ApplyAudioSettings();
    procedure ClearSubtitleBitmap();
    function RenderSubtitleBitmap(const AText: string): HRESULT;
    function RenderVobSubBitmap(const AFrame: TMfVobSubFrame;
                                const ACanvasWidth: Integer;
                                const ACanvasHeight: Integer): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;

    function SetWindow(const AWindow: HWND): HRESULT;
    function IsEnabled(): Boolean;
    function IsActive(): Boolean;

    function Open(const ASourceName: string;
                  const AVolume: Single;
                  const AMuted: Boolean;
                  const AAudioStreamIndex: DWORD;
                  const AHasAudioStreamIndex: Boolean): HRESULT;

    function Play(): HRESULT;
    function Pause(): HRESULT;
    function Stop(): HRESULT;
    function Seek(const APosition100ns: Int64): HRESULT;
    function SetVolume(const AVolume: Single): HRESULT;
    function SetMuted(const AMuted: Boolean): HRESULT;
    function UpdateVideo(): HRESULT;
    function ConfigureSubtitles(const ARequest: TMfCastTranscodeRequest): HRESULT;
    function UpdateSubtitles(): HRESULT;
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
  FSubtitleCompositor := TMfSubtitleCompositor.Create();
  FSubtitleBitmap := TBitmap.Create();
end;


destructor TMfCastDirectPreviewPlayer.Destroy();
begin

  Stop();
  FSubtitleBitmap.Free();
  FSubtitleCompositor.Free();

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


function TMfCastDirectPreviewPlayer.ConfigureAudioSelection(
  const APresentation: IMFPresentationDescriptor;
  const AAudioStreamIndex: DWORD;
  const AHasAudioStreamIndex: Boolean): HRESULT;
var
  Count: DWORD;
  I: DWORD;
  Selected: BOOL;
  StreamDescriptor: IMFStreamDescriptor;
  Handler: IMFMediaTypeHandler;
  MajorType: TGUID;
  Found: Boolean;
begin

  Result := S_OK;
  if not AHasAudioStreamIndex then
    Exit;
  if not Assigned(APresentation) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := APresentation.GetStreamDescriptorCount(Count);
  if FAILED(Result) then
    Exit;

  Found := False;
  for I := 0 to Count - 1 do
    begin
      StreamDescriptor := nil;
      Handler := nil;
      Selected := False;
      Result := APresentation.GetStreamDescriptorByIndex(I,
                                                         Selected,
                                                         StreamDescriptor);
      if FAILED(Result) then
        Exit;
      Result := StreamDescriptor.GetMediaTypeHandler(Handler);
      if FAILED(Result) then
        Exit;
      Result := Handler.GetMajorType(MajorType);
      if FAILED(Result) then
        Exit;

      if IsEqualGUID(MajorType, MFMediaType_Audio) then
        if I = AAudioStreamIndex then
          begin
            Result := APresentation.SelectStream(I);
            Found := SUCCEEDED(Result);
          end
        else
          Result := APresentation.DeselectStream(I);

      if FAILED(Result) then
        Exit;
    end;

  if not Found then
    Result := MF_E_INVALIDSTREAMNUMBER;
end;


function TMfCastDirectPreviewPlayer.Open(const ASourceName: string;
                                         const AVolume: Single;
                                         const AMuted: Boolean;
                                         const AAudioStreamIndex: DWORD;
                                         const AHasAudioStreamIndex: Boolean): HRESULT;
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

  Result := ConfigureAudioSelection(Presentation,
                                    AAudioStreamIndex,
                                    AHasAudioStreamIndex);
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

  // MfMetLib's general topology helper selects every source stream while it
  // walks the descriptor. Restore the requested audio selection so unused
  // tracks cannot accumulate data behind topology branches that do not exist.
  Result := ConfigureAudioSelection(Presentation,
                                    AAudioStreamIndex,
                                    AHasAudioStreamIndex);
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
  FVideoMixerBitmap := nil;

  if Assigned(FSession) then
    MFGetService(FSession,
                 MR_VIDEO_RENDER_SERVICE,
                 IID_IMFVideoDisplayControl,
                  Pointer(FVideoDisplay));

  if Assigned(FSession) then
    MFGetService(FSession,
                 MR_VIDEO_MIXER_SERVICE,
                 IMfCastVideoMixerBitmap,
                 Pointer(FVideoMixerBitmap));

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
  ClearSubtitleBitmap();
  FVideoMixerBitmap := nil;
  FSubtitleCompositor.Close();
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


function TMfCastDirectPreviewPlayer.ConfigureSubtitles(
  const ARequest: TMfCastTranscodeRequest): HRESULT;
var
  SourceName: WideString;
begin
  ClearSubtitleBitmap();
  FSubtitleCompositor.Close();
  Result := S_OK;

  if ARequest.SubtitleMode <> csmBurnIntoVideo then
    Exit;

  SourceName := ARequest.SubtitleSourceName;
  if SourceName = '' then
    SourceName := ARequest.SourceName;
  if SourceName = '' then
    Exit(E_INVALIDARG);

  Result := FSubtitleCompositor.OpenTimedTextFile(SourceName,
                                                   ARequest.SubtitleLanguage);
  if SUCCEEDED(Result) and ARequest.HasSubtitleStreamIndex then
    Result := FSubtitleCompositor.SelectEmbeddedSubtitleTrack(
                ARequest.SubtitleStreamIndex);
  if FAILED(Result) then
    FSubtitleCompositor.Close();
end;


procedure TMfCastDirectPreviewPlayer.ClearSubtitleBitmap();
begin
  if FSubtitleBitmapVisible and Assigned(FVideoMixerBitmap) then
    FVideoMixerBitmap.ClearAlphaBitmap();
  FSubtitleBitmapVisible := False;
  FSubtitleBitmapText := '';
end;


function TMfCastDirectPreviewPlayer.RenderSubtitleBitmap(
  const AText: string): HRESULT;
const
  TRANSPARENT_COLOR = TColor($010101);
var
  ClientRect: TRect;
  TextRect: TRect;
  DrawRect: TRect;
  BitmapParams: TMfCastVideoAlphaBitmap;
  FontSize: Integer;
  TextHeight: Integer;
  MarginX: Integer;
  MarginBottom: Integer;
  Dx: Integer;
  Dy: Integer;
  WideText: WideString;
  TextFlags: UINT;
begin
  if not Assigned(FVideoMixerBitmap) or (FVideoWindow = 0) then
    Exit(S_FALSE);
  if not GetClientRect(FVideoWindow, ClientRect) then
    Exit(HRESULT_FROM_WIN32(GetLastError()));
  if (ClientRect.Right <= 0) or (ClientRect.Bottom <= 0) then
    Exit(E_INVALIDARG);

  FSubtitleBitmap.PixelFormat := pf24bit;
  FSubtitleBitmap.SetSize(ClientRect.Right, ClientRect.Bottom);
  FSubtitleBitmap.Canvas.Brush.Style := bsSolid;
  FSubtitleBitmap.Canvas.Brush.Color := TRANSPARENT_COLOR;
  FSubtitleBitmap.Canvas.FillRect(ClientRect);

  FontSize := ClientRect.Bottom div 18;
  if FontSize < 16 then FontSize := 16;
  if FontSize > 34 then FontSize := 34;
  FSubtitleBitmap.Canvas.Font.Name := 'Segoe UI';
  FSubtitleBitmap.Canvas.Font.Size := FontSize;
  FSubtitleBitmap.Canvas.Font.Style := [fsBold];
  FSubtitleBitmap.Canvas.Font.Quality := fqAntialiased;
  FSubtitleBitmap.Canvas.Brush.Style := bsClear;
  SetBkMode(FSubtitleBitmap.Canvas.Handle, TRANSPARENT);

  MarginX := ClientRect.Right div 10;
  if MarginX < 24 then MarginX := 24;
  MarginBottom := ClientRect.Bottom div 20;
  TextRect := Rect(MarginX, 0, ClientRect.Right - MarginX,
                   ClientRect.Bottom);
  if TextRect.Left >= TextRect.Right then TextRect := ClientRect;
  DrawRect := TextRect;
  WideText := WideString(AText);
  TextFlags := DT_CENTER or DT_WORDBREAK or DT_NOPREFIX;
  DrawTextW(FSubtitleBitmap.Canvas.Handle, PWideChar(WideText),
            Length(WideText), DrawRect, TextFlags or DT_CALCRECT);
  TextHeight := DrawRect.Bottom - DrawRect.Top;
  DrawRect.Left := TextRect.Left;
  DrawRect.Right := TextRect.Right;
  DrawRect.Bottom := ClientRect.Bottom - MarginBottom;
  DrawRect.Top := DrawRect.Bottom - TextHeight;
  if DrawRect.Top < 0 then DrawRect.Top := 0;

  FSubtitleBitmap.Canvas.Font.Color := clBlack;
  for Dx := -2 to 2 do
    for Dy := -2 to 2 do
      if (Dx <> 0) or (Dy <> 0) then
        begin
          TextRect := DrawRect;
          OffsetRect(TextRect, Dx, Dy);
          DrawTextW(FSubtitleBitmap.Canvas.Handle, PWideChar(WideText),
                    Length(WideText), TextRect, TextFlags);
        end;
  FSubtitleBitmap.Canvas.Font.Color := clWhite;
  DrawTextW(FSubtitleBitmap.Canvas.Handle, PWideChar(WideText),
            Length(WideText), DrawRect, TextFlags);

  ZeroMemory(@BitmapParams, SizeOf(BitmapParams));
  BitmapParams.GetBitmapFromDC := True;
  BitmapParams.Source := Pointer(FSubtitleBitmap.Canvas.Handle);
  BitmapParams.params.dwFlags := MFVideoAlphaBitmap_SrcColorKey or
                                 MFVideoAlphaBitmap_SrcRect or
                                 MFVideoAlphaBitmap_DestRect or
                                 MFVideoAlphaBitmap_Alpha;
  BitmapParams.params.clrSrcKey := ColorToRGB(TRANSPARENT_COLOR);
  BitmapParams.params.rcSrc := ClientRect;
  BitmapParams.params.nrcDest := RectF(0, 0, 1, 1);
  BitmapParams.params.fAlpha := 1.0;
  Result := FVideoMixerBitmap.SetAlphaBitmap(BitmapParams);
  if SUCCEEDED(Result) then
    begin
      FSubtitleBitmapText := AText;
      FSubtitleBitmapVisible := True;
    end;
end;


function TMfCastDirectPreviewPlayer.RenderVobSubBitmap(
  const AFrame: TMfVobSubFrame;
  const ACanvasWidth: Integer;
  const ACanvasHeight: Integer): HRESULT;
const
  TRANSPARENT_COLOR = TColor($010101);
var
  ClientRect: TRect;
  BitmapParams: TMfCastVideoAlphaBitmap;
  TargetLeft: Integer;
  TargetTop: Integer;
  TargetWidth: Integer;
  TargetHeight: Integer;
  X: Integer;
  Y: Integer;
  SourceX: Integer;
  SourceY: Integer;
  SourceOffset: Integer;
  Alpha: Integer;
  Row: PByte;
  Pixel: PByte;
begin
  if not Assigned(FVideoMixerBitmap) or (FVideoWindow = 0) then
    Exit(S_FALSE);
  if not GetClientRect(FVideoWindow, ClientRect) then
    Exit(HRESULT_FROM_WIN32(GetLastError()));
  if (ClientRect.Right <= 0) or (ClientRect.Bottom <= 0) or
     (ACanvasWidth <= 0) or (ACanvasHeight <= 0) or
     (AFrame.Width <= 0) or (AFrame.Height <= 0) or
     (Length(AFrame.Pixels) < AFrame.Width * AFrame.Height * 4) then
    Exit(E_INVALIDARG);

  FSubtitleBitmap.PixelFormat := pf24bit;
  FSubtitleBitmap.SetSize(ClientRect.Right, ClientRect.Bottom);
  FSubtitleBitmap.Canvas.Brush.Color := TRANSPARENT_COLOR;
  FSubtitleBitmap.Canvas.FillRect(ClientRect);

  TargetLeft := Integer((Int64(AFrame.Left) * ClientRect.Right) div ACanvasWidth);
  TargetTop := Integer((Int64(AFrame.Top) * ClientRect.Bottom) div ACanvasHeight);
  TargetWidth := Integer((Int64(AFrame.Width) * ClientRect.Right + ACanvasWidth div 2) div ACanvasWidth);
  TargetHeight := Integer((Int64(AFrame.Height) * ClientRect.Bottom + ACanvasHeight div 2) div ACanvasHeight);
  if TargetWidth < 1 then TargetWidth := 1;
  if TargetHeight < 1 then TargetHeight := 1;
  if TargetLeft < 0 then TargetLeft := 0;
  if TargetTop < 0 then TargetTop := 0;
  if TargetLeft + TargetWidth > ClientRect.Right then
    TargetWidth := ClientRect.Right - TargetLeft;
  if TargetTop + TargetHeight > ClientRect.Bottom then
    TargetHeight := ClientRect.Bottom - TargetTop;
  if (TargetWidth <= 0) or (TargetHeight <= 0) then
    Exit(S_FALSE);

  for Y := 0 to TargetHeight - 1 do
    begin
      SourceY := Integer((Int64(Y) * AFrame.Height) div TargetHeight);
      Row := FSubtitleBitmap.ScanLine[TargetTop + Y];
      for X := 0 to TargetWidth - 1 do
        begin
          SourceX := Integer((Int64(X) * AFrame.Width) div TargetWidth);
          SourceOffset := (SourceY * AFrame.Width + SourceX) * 4;
          Alpha := AFrame.Pixels[SourceOffset + 3];
          if Alpha = 0 then
            Continue;
          Pixel := PByte(NativeInt(Row) + NativeInt(TargetLeft + X) * 3);
          Pixel^ := Byte(Integer(AFrame.Pixels[SourceOffset]) +
                         ((255 - Alpha + 127) div 255));
          PByte(NativeInt(Pixel) + 1)^ := Byte(Integer(AFrame.Pixels[SourceOffset + 1]) +
                                              ((255 - Alpha + 127) div 255));
          PByte(NativeInt(Pixel) + 2)^ := Byte(Integer(AFrame.Pixels[SourceOffset + 2]) +
                                              ((255 - Alpha + 127) div 255));
        end;
    end;

  ZeroMemory(@BitmapParams, SizeOf(BitmapParams));
  BitmapParams.GetBitmapFromDC := True;
  BitmapParams.Source := Pointer(FSubtitleBitmap.Canvas.Handle);
  BitmapParams.params.dwFlags := MFVideoAlphaBitmap_SrcColorKey or
                                 MFVideoAlphaBitmap_SrcRect or
                                 MFVideoAlphaBitmap_DestRect or
                                 MFVideoAlphaBitmap_Alpha;
  BitmapParams.params.clrSrcKey := ColorToRGB(TRANSPARENT_COLOR);
  BitmapParams.params.rcSrc := ClientRect;
  BitmapParams.params.nrcDest := RectF(0, 0, 1, 1);
  BitmapParams.params.fAlpha := 1.0;
  Result := FVideoMixerBitmap.SetAlphaBitmap(BitmapParams);
  if SUCCEEDED(Result) then
    begin
      FSubtitleBitmapText := '#VOBSUB:' + IntToStr(AFrame.CueIndex);
      FSubtitleBitmapVisible := True;
    end;
end;


function TMfCastDirectPreviewPlayer.UpdateSubtitles(): HRESULT;
var
  Clock: IMFClock;
  PresentationClock: IMFPresentationClock;
  MediaTime: MFTIME;
  TextValue: string;
  Track: TSubTitleTrack;
  VobFrame: TMfVobSubFrame;
  VobCanvasWidth: Integer;
  VobCanvasHeight: Integer;
begin
  Result := S_FALSE;
  if FClosing or not Assigned(FSession) or
     not Assigned(FVideoMixerBitmap) then
    Exit;
  if not FSubtitleCompositor.HasSubtitleSources() then
    begin
      ClearSubtitleBitmap();
      Exit;
    end;
  Result := FSession.GetClock(Clock);
  if FAILED(Result) then Exit;
  Result := Clock.QueryInterface(IID_IMFPresentationClock,
                                 PresentationClock);
  if FAILED(Result) then Exit;
  Result := PresentationClock.GetTime(MediaTime);
  if FAILED(Result) then Exit;

  if FSubtitleCompositor.TryGetVobSubFrameAtTime(MediaTime div 10000,
                                                 VobFrame,
                                                 VobCanvasWidth,
                                                 VobCanvasHeight) then
    begin
      TextValue := '#VOBSUB:' + IntToStr(VobFrame.CueIndex);
      if FSubtitleBitmapVisible and SameStr(FSubtitleBitmapText, TextValue) then
        Exit(S_OK);
      Result := RenderVobSubBitmap(VobFrame,
                                   VobCanvasWidth,
                                   VobCanvasHeight);
      Exit;
    end;

  if not FSubtitleCompositor.TryGetSubtitleTextAtTime(MediaTime div 10000,
                                                      TextValue, Track) then
    begin
      ClearSubtitleBitmap();
      Exit(S_OK);
    end;
  TextValue := Trim(TextValue);
  if TextValue = '' then
    begin
      ClearSubtitleBitmap();
      Exit(S_OK);
    end;
  if FSubtitleBitmapVisible and SameStr(FSubtitleBitmapText, TextValue) then
    Exit(S_OK);
  Result := RenderSubtitleBitmap(TextValue);
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
