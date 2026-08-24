// FactoryX
//
// Copyright Â© FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfSubtitleCompositor.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: MfPlayer X2 subtitle compositor. This unit owns timed-text lookup by media time.
//              The RGB32 blend method is the pipeline hook where X2 will draw into
//              decoded video frames before preview/stream output.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 02/08/2026 Carmen              Cache subtitle overlays to remove per-frame GDI allocation.
// 01/08/2026 Carmen              Added selectable MKV tracks and thread-safe timed-text swaps.
// 31/07/2026 Carmen              Added sidecar-first embedded subtitle fallback.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
unit MfSubtitleCompositor;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  TimedTextClass,
  MfEmbeddedSubtitleReader;

type

  TMfSubtitleCompositor = class(TObject)
  private
    FLock: TCriticalSection;
    FTimedText: TMfTimedText;
    FEmbeddedTracks: TMfEmbeddedSubtitleTrackInfoArray;
    FActiveEmbeddedStreamIndex: Integer;
    FActiveSubtitleIsEmbedded: Boolean;
    FMediaFileName: WideString;
    FPreferredLanguage: string;
    FTimedTextFileLoaded: Boolean;
    FEmbeddedWindowed: Boolean;
    FEmbeddedWindowLoading: Boolean;
    FEmbeddedWindowStartMs: Int64;
    FEmbeddedWindowEndMs: Int64;
    FEmbeddedWindowEndOfTrack: Boolean;
    FEmbeddedWindowGeneration: Integer;
    FSubtitleAspectRatio: Single;
    FSubtitleFontScale: Single;
    FOverlayText: string;
    FOverlayFrameWidth: Integer;
    FOverlayFrameHeight: Integer;
    FOverlayAspectRatio: Single;
    FOverlayLeft: Integer;
    FOverlayTop: Integer;
    FOverlayWidth: Integer;
    FOverlayHeight: Integer;
    FOverlayPixels: TBytes;
    FOverlayValid: Boolean;

    procedure ReleaseTimedText();
    procedure ResetOverlayCache();
    procedure ResetEmbeddedWindowState();
    function EnsureEmbeddedWindow(MediaTimeMs: Int64): HRESULT;
    function EnsureFullEmbeddedTrack(): HRESULT;
    function BuildSubtitleOverlay(const SubtitleText: string;
                                  Width: Integer;
                                  Height: Integer): HRESULT;
    function BlendCachedOverlay(VideoBuffer: Pointer;
                                Width: Integer;
                                Height: Integer;
                                Stride: Integer): HRESULT;
    function FindEmbeddedTrack(StreamIndex: DWORD;
                               out Track: TMfEmbeddedSubtitleTrackInfo): Boolean;
    function BuildPlainText(const Track: TSubTitleTrack): string;
    function GetSubtitleTargetRect(const ClientRect: TRect): TRect;
    procedure SetSubtitleAspectRatio(aValue: Single);
    procedure SetSubtitleFontScale(aValue: Single);

  public

    constructor Create();
    destructor Destroy(); override;

    function OpenTimedTextFile(const MediaFileName: WideString;
                               const PreferredLanguage: string;
                               const PresentationDescriptor: IMFPresentationDescriptor = nil;
                               const LoadEmbeddedTrack: Boolean = True): HRESULT;
    function RefreshEmbeddedSubtitleTracks(const PresentationDescriptor: IMFPresentationDescriptor = nil): HRESULT;
    function GetEmbeddedSubtitleTracks(out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
    function GetPreferredEmbeddedSubtitleStreamIndex(out StreamIndex: DWORD): HRESULT;
    function HasSubtitleSources(): Boolean;
    function SelectEmbeddedSubtitleTrack(StreamIndex: DWORD;
                                         const CancelEvent: THandle = 0): HRESULT;
    function SelectSidecarSubtitleLanguage(const PreferredLanguage: string): HRESULT;
    function ExportActiveWebVtt(out AData: TBytes;
                                out ALanguageTag: string;
                                out AFriendlyLanguageName: string): HRESULT;

    procedure Close();

    function TryGetSubtitleTextAtTime(MediaTimeMs: Int64;
                                      out SubtitleText: string;
                                      out Track: TSubTitleTrack): Boolean;

    function CompositeRgb32(VideoBuffer: Pointer;
                            BufferSize: UINT32;
                            Width: Integer;
                            Height: Integer;
                            Stride: Integer;
                            MediaTimeMs: Int64): HRESULT;

    property MediaFileName: WideString read FMediaFileName;
    property PreferredLanguage: string read FPreferredLanguage;
    property SubtitleAspectRatio: Single read FSubtitleAspectRatio write SetSubtitleAspectRatio;
    property SubtitleFontScale: Single read FSubtitleFontScale write SetSubtitleFontScale;
    property TimedTextFileLoaded: Boolean read FTimedTextFileLoaded;
    property ActiveSubtitleIsEmbedded: Boolean read FActiveSubtitleIsEmbedded;
    property ActiveEmbeddedStreamIndex: Integer read FActiveEmbeddedStreamIndex;
  end;


implementation

const
  MF_SUBTITLE_WINDOW_LOOK_BEHIND_MS = Int64(30000);
  MF_SUBTITLE_WINDOW_AHEAD_MS = Int64(5 * 60 * 1000);


constructor TMfSubtitleCompositor.Create();
begin

  inherited Create();

  FLock := TCriticalSection.Create();
  FTimedText := nil;
  SetLength(FEmbeddedTracks, 0);
  FActiveEmbeddedStreamIndex := -1;
  FActiveSubtitleIsEmbedded := False;
  FTimedTextFileLoaded := False;
  FEmbeddedWindowGeneration := 0;
  ResetEmbeddedWindowState();
  FSubtitleAspectRatio := 16.0 / 9.0;
  FSubtitleFontScale := 1.0;
  ResetOverlayCache();
end;


destructor TMfSubtitleCompositor.Destroy();
begin

  Close();
  ResetOverlayCache();
  FreeAndNil(FLock);

  inherited Destroy();
end;


procedure TMfSubtitleCompositor.ResetOverlayCache();
begin

  FOverlayText := '';
  FOverlayFrameWidth := 0;
  FOverlayFrameHeight := 0;
  FOverlayAspectRatio := 0.0;
  FOverlayLeft := 0;
  FOverlayTop := 0;
  FOverlayWidth := 0;
  FOverlayHeight := 0;
  SetLength(FOverlayPixels, 0);
  FOverlayValid := False;
end;


function TMfSubtitleCompositor.BuildSubtitleOverlay(const SubtitleText: string;
                                                    Width: Integer;
                                                    Height: Integer): HRESULT;
var
  rcClient: TRect;
  rcVideo: TRect;
  rcText: TRect;
  rcCalc: TRect;
  rcDraw: TRect;
  rcLocal: TRect;
  textValue: WideString;
  textFlags: UINT;
  fontSize: Integer;
  fontHeight: Integer;
  marginX: Integer;
  marginBottom: Integer;
  textHeight: Integer;
  videoWidth: Integer;
  videoHeight: Integer;
  dx: Integer;
  dy: Integer;
  outlinePadding: Integer;
  regionLeft: Integer;
  regionTop: Integer;
  regionRight: Integer;
  regionBottom: Integer;
  regionWidth: Integer;
  regionHeight: Integer;
  bitmapBytes: Integer;
  pixelOffset: Integer;
  pixelCount: Integer;
  pixelIndex: Integer;
  outlineAlpha: Integer;
  fillAlpha: Integer;
  totalAlpha: Integer;
  screenDC: HDC;
  memDC: HDC;
  oldBmp: HGDIOBJ;
  fontObj: HFONT;
  oldFont: HGDIOBJ;
  bmi: TBitmapInfo;
  dibBits: Pointer;
  dib: HBITMAP;

begin

  Result := S_OK;
  ResetOverlayCache();

  if (SubtitleText = '') or (Width <= 0) or (Height <= 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  rcClient := Rect(0, 0, Width, Height);
  rcVideo := GetSubtitleTargetRect(rcClient);
  videoWidth := rcVideo.Right - rcVideo.Left;
  videoHeight := rcVideo.Bottom - rcVideo.Top;


  if (videoWidth <= 0) or (videoHeight <= 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  fontSize := videoHeight div 29;

  if (fontSize < 10) then
    fontSize := 10
  else
    if (fontSize > 21) then
      fontSize := 21;

  fontSize := Round(fontSize * FSubtitleFontScale);
  if fontSize < 10 then
    fontSize := 10
  else
    if fontSize > 48 then
      fontSize := 48;

  marginX := videoWidth div 10;
  marginBottom := videoHeight div 20;

  if (marginX < 24) then
    marginX := 24;

  rcText := Rect(rcVideo.Left + marginX,
                 rcVideo.Top,
                 rcVideo.Right - marginX,
                 rcVideo.Bottom);

  if (rcText.Left >= rcText.Right) then
    rcText := rcVideo;

  textValue := SubtitleText;
  textFlags := DT_CENTER or DT_WORDBREAK or DT_NOPREFIX;
  outlinePadding := 2;
  memDC := 0;
  oldBmp := 0;
  fontObj := 0;
  oldFont := 0;
  dib := 0;
  dibBits := nil;

  screenDC := GetDC(0);
  if (screenDC = 0) then
    begin
      Result := HRESULT_FROM_WIN32(GetLastError());
      if SUCCEEDED(Result) then
        Result := E_FAIL;
      Exit;
    end;

  try
    fontHeight := -MulDiv(fontSize,
                          GetDeviceCaps(screenDC, LOGPIXELSY),
                          72);

    fontObj := CreateFontW(fontHeight,
                           0,
                           0,
                           0,
                           FW_BOLD,
                           0,
                           0,
                           0,
                           DEFAULT_CHARSET,
                           OUT_DEFAULT_PRECIS,
                           CLIP_DEFAULT_PRECIS,
                           ANTIALIASED_QUALITY,
                           DEFAULT_PITCH or FF_DONTCARE,
                           'Segoe UI');
    if (fontObj = 0) then
      begin

        Result := HRESULT_FROM_WIN32(GetLastError());

        if SUCCEEDED(Result) then
          Result := E_OUTOFMEMORY;
        Exit;
      end;

    memDC := CreateCompatibleDC(screenDC);
    if (memDC = 0) then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError());

        if SUCCEEDED(Result) then
          Result := E_OUTOFMEMORY;
        Exit;
      end;

    oldFont := SelectObject(memDC,
                            fontObj);
    SetBkMode(memDC,
              TRANSPARENT);
    SetTextColor(memDC,
                 RGB(255, 255, 255));

    rcCalc := Rect(0,
                   0,
                   rcText.Right - rcText.Left,
                   videoHeight);

    DrawTextW(memDC,
              PWideChar(textValue),
              Length(textValue),
              rcCalc,
              textFlags or DT_CALCRECT);

    textHeight := rcCalc.Bottom - rcCalc.Top;
    if (textHeight <= 0) then
      begin
        Result := S_FALSE;
        Exit;
      end;

    rcDraw.Left := rcText.Left;
    rcDraw.Right := rcText.Right;
    rcDraw.Bottom := rcVideo.Bottom - marginBottom;
    rcDraw.Top := rcDraw.Bottom - textHeight;

    if (rcDraw.Top < rcVideo.Top) then
      rcDraw.Top := rcVideo.Top;

    regionLeft := rcDraw.Left - outlinePadding;
    regionTop := rcDraw.Top - outlinePadding;
    regionRight := rcDraw.Right + outlinePadding;
    regionBottom := rcDraw.Bottom + outlinePadding;

    if (regionLeft < 0) then
      regionLeft := 0;

    if (regionTop < 0) then
      regionTop := 0;

    if (regionRight > Width) then
      regionRight := Width;

    if (regionBottom > Height) then
      regionBottom := Height;

    regionWidth := regionRight - regionLeft;
    regionHeight := regionBottom - regionTop;

    if (regionWidth <= 0) or (regionHeight <= 0) then
      begin

        Result := S_FALSE;
        Exit;
      end;

    FillChar(bmi, SizeOf(bmi), 0);
    bmi.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    bmi.bmiHeader.biWidth := regionWidth;
    bmi.bmiHeader.biHeight := -regionHeight;
    bmi.bmiHeader.biPlanes := 1;
    bmi.bmiHeader.biBitCount := 32;
    bmi.bmiHeader.biCompression := BI_RGB;

    dib := CreateDIBSection(memDC,
                            bmi,
                            DIB_RGB_COLORS,
                            dibBits,
                            0,
                            0);

    if (dib = 0) or (dibBits = nil) then
      begin

        Result := HRESULT_FROM_WIN32(GetLastError());

        if SUCCEEDED(Result) then
          Result := E_OUTOFMEMORY;
        Exit;
      end;

    oldBmp := SelectObject(memDC,
                           dib);
    bitmapBytes := regionWidth * regionHeight * 4;
    FillChar(dibBits^,
             bitmapBytes, 0);

    rcLocal := Rect(rcDraw.Left - regionLeft,
                    rcDraw.Top - regionTop,
                    rcDraw.Right - regionLeft,
                    rcDraw.Bottom - regionTop);

    // First render a white outline mask. The mask is converted to alpha below.
    for dx := -1 to 1 do
      for dy := -1 to 1 do
        if (dx <> 0) or (dy <> 0) then
          begin
            rcText := rcLocal;
            OffsetRect(rcText, dx, dy);
            DrawTextW(memDC,
                      PWideChar(textValue),
                      Length(textValue),
                      rcText,
                      textFlags);
          end;
    GdiFlush();

    SetLength(FOverlayPixels,
              bitmapBytes);

    FillChar(FOverlayPixels[0],
             bitmapBytes, 0);

    pixelCount := regionWidth * regionHeight;

    for pixelIndex := 0 to pixelCount - 1 do
      begin

        pixelOffset := pixelIndex * 4;
        outlineAlpha := PByte(NativeInt(dibBits) + pixelOffset)^;
        FOverlayPixels[pixelOffset + 3] := Byte(outlineAlpha);
      end;

    // Then render the fill mask. RGB stores premultiplied white; alpha combines
    // the white fill with the black outline mask.
    FillChar(dibBits^,
             bitmapBytes,
             0);

    DrawTextW(memDC,
              PWideChar(textValue),
              Length(textValue),
              rcLocal,
              textFlags);
    GdiFlush();

    for pixelIndex := 0 to pixelCount - 1 do
      begin
        pixelOffset := pixelIndex * 4;
        fillAlpha := PByte(NativeInt(dibBits) + pixelOffset)^;
        outlineAlpha := FOverlayPixels[pixelOffset + 3];
        totalAlpha := fillAlpha + (((255 - fillAlpha) * outlineAlpha + 127) div 255);
        FOverlayPixels[pixelOffset] := Byte(fillAlpha);
        FOverlayPixels[pixelOffset + 1] := Byte(fillAlpha);
        FOverlayPixels[pixelOffset + 2] := Byte(fillAlpha);
        FOverlayPixels[pixelOffset + 3] := Byte(totalAlpha);
      end;

    FOverlayText := SubtitleText;
    FOverlayFrameWidth := Width;
    FOverlayFrameHeight := Height;
    FOverlayAspectRatio := FSubtitleAspectRatio;
    FOverlayLeft := regionLeft;
    FOverlayTop := regionTop;
    FOverlayWidth := regionWidth;
    FOverlayHeight := regionHeight;
    FOverlayValid := True;

  finally

    if (oldBmp <> 0) then
      SelectObject(memDC,
                   oldBmp);

    if (oldFont <> 0) then
      SelectObject(memDC,
                   oldFont);

    if (dib <> 0) then
      DeleteObject(dib);

    if (fontObj <> 0) then
      DeleteObject(fontObj);

    if (memDC <> 0) then
      DeleteDC(memDC);

    if (screenDC <> 0) then
      ReleaseDC(0,
                screenDC);
  end;
end;


function TMfSubtitleCompositor.BlendCachedOverlay(VideoBuffer: Pointer;
                                                  Width: Integer;
                                                  Height: Integer;
                                                  Stride: Integer): HRESULT;
var
  X: Integer;
  Y: Integer;
  sourceOffset: Integer;
  destinationRow: PByte;
  destinationPixel: PByte;
  alphaValue: Integer;
  inverseAlpha: Integer;
  sourceValue: Integer;
  destinationValue: Integer;

begin

  Result := S_OK;

  if not FOverlayValid then
    Exit;

  if (VideoBuffer = nil) or
     (Width <> FOverlayFrameWidth) or
     (Height <> FOverlayFrameHeight) or
     (Stride = 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  for Y := 0 to FOverlayHeight - 1 do
    begin

      if (Stride > 0) then
        destinationRow := PByte(NativeInt(VideoBuffer) + NativeInt(FOverlayTop + Y) * NativeInt(Stride) +
                                NativeInt(FOverlayLeft) * 4)
      else
        destinationRow := PByte(NativeInt(VideoBuffer) + NativeInt(Height - 1 - (FOverlayTop + Y)) *
                                  NativeInt(Abs(Stride)) + NativeInt(FOverlayLeft) * 4);

      sourceOffset := Y * FOverlayWidth * 4;

      for X := 0 to FOverlayWidth - 1 do
        begin

          alphaValue := FOverlayPixels[sourceOffset + 3];

          if (alphaValue <> 0) then
            begin
              inverseAlpha := 255 - alphaValue;
              destinationPixel := PByte(NativeInt(destinationRow) + NativeInt(X) * 4);

              sourceValue := FOverlayPixels[sourceOffset];
              destinationValue := destinationPixel^;
              destinationPixel^ := Byte(sourceValue + ((destinationValue * inverseAlpha + 127) div 255));

              Inc(destinationPixel);
              sourceValue := FOverlayPixels[sourceOffset + 1];
              destinationValue := destinationPixel^;
              destinationPixel^ := Byte(sourceValue + ((destinationValue * inverseAlpha + 127) div 255));

              Inc(destinationPixel);
              sourceValue := FOverlayPixels[sourceOffset + 2];
              destinationValue := destinationPixel^;
              destinationPixel^ := Byte(sourceValue + ((destinationValue * inverseAlpha + 127) div 255));
            end;

          Inc(sourceOffset, 4);
        end;
    end;
end;


procedure TMfSubtitleCompositor.ReleaseTimedText();
begin

  if Assigned(FTimedText) then
    FreeAndNil(FTimedText);
end;


procedure TMfSubtitleCompositor.Close();
begin

  FLock.Acquire();

  try

    ReleaseTimedText();
    SetLength(FEmbeddedTracks,
              0);
    FActiveEmbeddedStreamIndex := -1;
    FActiveSubtitleIsEmbedded := False;
    FTimedTextFileLoaded := False;
    ResetEmbeddedWindowState();
    FMediaFileName := '';
  finally
    FLock.Release();
  end;
end;


function TMfSubtitleCompositor.OpenTimedTextFile(const MediaFileName: WideString;
                                                 const PreferredLanguage: string;
                                                 const PresentationDescriptor: IMFPresentationDescriptor;
                                                 const LoadEmbeddedTrack: Boolean): HRESULT;
var
  hr: HRESULT;
  hrTracks: HRESULT;
  NewTimedText: TMfTimedText;
  EmbeddedTrack: TMfEmbeddedSubtitleTrackInfo;

begin

  Close();

  FMediaFileName := MediaFileName;
  FPreferredLanguage := PreferredLanguage;

  // Keep the complete MKV track list for the language-selection dialog. This
  // operation reads metadata only. The player passes LoadEmbeddedTrack=False
  // and activates the selected embedded track asynchronously after submitting
  // its topology. Direct MKV cues are then read in rolling playback windows.
  // Export callers retain the complete synchronous path by using default True.
  hrTracks := RefreshEmbeddedSubtitleTracks(PresentationDescriptor);

  if FAILED(hrTracks) then
    OutputDebugString(PChar(Format('Embedded subtitle refresh failed hr=%s',
                                   [IntToHex(DWORD(hrTracks), 8)])));

  EmbeddedTrack.Reset();

  NewTimedText := TMfTimedText.Create(0,
                                      MediaFileName,
                                      PreferredLanguage);
  if not Assigned(NewTimedText) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  hr := NewTimedText.OpenTimedTextFile(MediaFileName);

  if (hr = S_FALSE) and LoadEmbeddedTrack then
    begin

      EmbeddedTrack.Reset();
      hr := TMfEmbeddedSubtitleReader.ImportBestTextTrackFromList(
              MediaFileName,
              PreferredLanguage,
              FEmbeddedTracks,
              NewTimedText,
              EmbeddedTrack);
    end;

  if (hr = S_OK) then
    begin
      FLock.Acquire();

      try
        ReleaseTimedText();
        FTimedText := NewTimedText;
        NewTimedText := nil;
        ResetEmbeddedWindowState();
        FPreferredLanguage := FTimedText.PreferredLanguage;
        FTimedTextFileLoaded := True;
        FActiveSubtitleIsEmbedded := EmbeddedTrack.Supported;
        if FActiveSubtitleIsEmbedded then
          FActiveEmbeddedStreamIndex := Integer(EmbeddedTrack.StreamIndex)
        else
          FActiveEmbeddedStreamIndex := -1;
      finally
        FLock.Release();
      end;
    end
  else
    begin

      FTimedTextFileLoaded := False;
      FPreferredLanguage := PreferredLanguage;
      FActiveSubtitleIsEmbedded := False;
      FActiveEmbeddedStreamIndex := -1;
    end;

  NewTimedText.Free();
  Result := hr;
end;

function TMfSubtitleCompositor.FindEmbeddedTrack(StreamIndex: DWORD;
                                                 out Track: TMfEmbeddedSubtitleTrackInfo): Boolean;
var
  I: Integer;

begin

  Track.Reset();
  Result := False;

  for I := Low(FEmbeddedTracks) to High(FEmbeddedTracks) do
    if (FEmbeddedTracks[I].StreamIndex = StreamIndex) then
      begin

        Track := FEmbeddedTracks[I];
        Result := True;
        Exit;
      end;
end;


function TMfSubtitleCompositor.RefreshEmbeddedSubtitleTracks(const PresentationDescriptor: IMFPresentationDescriptor): HRESULT;
var
  NewTracks: TMfEmbeddedSubtitleTrackInfoArray;
  I: Integer;
  HrFallback: HRESULT;
  IsMatroska: Boolean;

begin

  SetLength(NewTracks,
            0);

  if (FMediaFileName = '') then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  IsMatroska := SameText(ExtractFileExt(FMediaFileName),
                         '.mkv') or
                SameText(ExtractFileExt(FMediaFileName),
                         '.webm');

  // The direct EBML metadata pass is both faster and more complete for MKV.
  // In particular, it retains deselected subtitle tracks that some Media
  // Foundation presentation descriptors omit.
  if IsMatroska then
    Result := TMfEmbeddedSubtitleReader.EnumerateTracks(FMediaFileName,
                                                        NewTracks)
  else
    if Assigned(PresentationDescriptor) then
      Result := TMfEmbeddedSubtitleReader.EnumeratePresentationTracks(PresentationDescriptor,
                                                                      NewTracks)
    else
      Result := S_FALSE;

  if (Result = S_FALSE) or FAILED(Result) then
    begin

      if IsMatroska and Assigned(PresentationDescriptor) then
        HrFallback := TMfEmbeddedSubtitleReader.EnumeratePresentationTracks(PresentationDescriptor,
                                                                            NewTracks)
      else
        HrFallback := TMfEmbeddedSubtitleReader.EnumerateTracks(FMediaFileName,
                                                                NewTracks);

      if (HrFallback = S_OK) or ((Result = S_FALSE) and SUCCEEDED(HrFallback)) then
        Result := HrFallback;
    end;

  if SUCCEEDED(Result) then
    begin
      FLock.Acquire();

      try
        SetLength(FEmbeddedTracks,
                  Length(NewTracks));

        for I := Low(NewTracks) to High(NewTracks) do
          FEmbeddedTracks[I] := NewTracks[I];
      finally
        FLock.Release();
      end;
    end;

  OutputDebugString(PChar(Format('Embedded subtitle refresh hr=%s tracks=%d descriptor=%s',
                                 [IntToHex(DWORD(Result), 8), Length(NewTracks),
                                  BoolToStr(Assigned(PresentationDescriptor))])));
end;


function TMfSubtitleCompositor.GetEmbeddedSubtitleTracks(out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  I: Integer;

begin

  SetLength(Tracks,
            Length(FEmbeddedTracks));

  for I := Low(FEmbeddedTracks) to High(FEmbeddedTracks) do
    Tracks[I] := FEmbeddedTracks[I];

  if (Length(Tracks) = 0) then
    Result := S_FALSE
  else
    Result := S_OK;
end;


function TMfSubtitleCompositor.GetPreferredEmbeddedSubtitleStreamIndex(out StreamIndex: DWORD): HRESULT;
var
  Tracks: TMfEmbeddedSubtitleTrackInfoArray;
  SelectedTrack: TMfEmbeddedSubtitleTrackInfo;
  I: Integer;

begin

  StreamIndex := 0;
  SetLength(Tracks,
            0);
  SelectedTrack.Reset();

  FLock.Acquire();

  try
    SetLength(Tracks,
              Length(FEmbeddedTracks));

    for I := Low(FEmbeddedTracks) to High(FEmbeddedTracks) do
      Tracks[I] := FEmbeddedTracks[I];
  finally
    FLock.Release();
  end;

  Result := TMfEmbeddedSubtitleReader.FindBestTextTrackFromList(FPreferredLanguage,
                                                                Tracks,
                                                                SelectedTrack);
  if (Result = S_OK) then
    StreamIndex := SelectedTrack.StreamIndex;
end;


function TMfSubtitleCompositor.HasSubtitleSources(): Boolean;
begin

  FLock.Acquire();

  try
    // Keep the language-selection path available when the preferred embedded
    // track could not be imported. The enumerated tracks are still valid
    // choices and another text track may load successfully.
    Result := FTimedTextFileLoaded or (Length(FEmbeddedTracks) > 0);
  finally
    FLock.Release();
  end;
end;


function TMfSubtitleCompositor.SelectEmbeddedSubtitleTrack(StreamIndex: DWORD;
                                                           const CancelEvent: THandle): HRESULT;
var
  Track: TMfEmbeddedSubtitleTrackInfo;
  NewTimedText: TMfTimedText;
  OldTimedText: TMfTimedText;

begin

  if not FindEmbeddedTrack(StreamIndex,
                           Track) then
    begin
      Result := MF_E_INVALIDSTREAMNUMBER;
      Exit;
    end;

  if not Track.Supported then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  NewTimedText := TMfTimedText.Create(0,
                                      FMediaFileName,
                                      Track.Language);
  if not Assigned(NewTimedText) then
    begin

      Result := E_OUTOFMEMORY;
      Exit;
    end;

  if (Track.Source = essMatroska) then
    Result := S_OK
  else
    Result := TMfEmbeddedSubtitleReader.ImportTextTrack(FMediaFileName,
                                                        Track,
                                                        NewTimedText,
                                                        CancelEvent);

  if (Result <> S_OK) then
    begin
      NewTimedText.Free();
      Exit;
    end;

  FLock.Acquire();

  try
    OldTimedText := FTimedText;
    FTimedText := NewTimedText;
    ResetEmbeddedWindowState();
    FEmbeddedWindowed := Track.Source = essMatroska;
    FPreferredLanguage := FTimedText.PreferredLanguage;
    FTimedTextFileLoaded := True;
    FActiveSubtitleIsEmbedded := True;
    FActiveEmbeddedStreamIndex := Integer(StreamIndex);
  finally
    FLock.Release();
  end;
  OldTimedText.Free();
end;


function TMfSubtitleCompositor.SelectSidecarSubtitleLanguage(const PreferredLanguage: string): HRESULT;
var
  NewTimedText: TMfTimedText;
  OldTimedText: TMfTimedText;

begin

  NewTimedText := TMfTimedText.Create(0,
                                      FMediaFileName,
                                      PreferredLanguage);
  if not Assigned(NewTimedText) then
    begin

      Result := E_OUTOFMEMORY;
      Exit;
    end;

  Result := NewTimedText.OpenTimedTextFile(FMediaFileName);
  if (Result <> S_OK) then
    begin
      NewTimedText.Free();
      Exit;
    end;

  FLock.Acquire();

  try
    OldTimedText := FTimedText;
    FTimedText := NewTimedText;
    ResetEmbeddedWindowState();
    FPreferredLanguage := FTimedText.PreferredLanguage;
    FTimedTextFileLoaded := True;
    FActiveSubtitleIsEmbedded := False;
    FActiveEmbeddedStreamIndex := -1;
  finally
    FLock.Release();
  end;

  OldTimedText.Free();
end;


function TMfSubtitleCompositor.EnsureEmbeddedWindow(MediaTimeMs: Int64): HRESULT;
var
  Track: TMfEmbeddedSubtitleTrackInfo;
  Cues: TMfTimedTextCueArray;
  WindowStartMs: Int64;
  WindowEndMs: Int64;
  EndOfTrack: Boolean;
  Generation: Integer;
  MediaFileName: WideString;
  FriendlyName: string;
  ImportResult: HRESULT;

begin

  Result := S_OK;
  if (MediaTimeMs < 0) then
    MediaTimeMs := 0;

  Track.Reset();
  SetLength(Cues, 0);

  FLock.Acquire();
  try
    if (not FEmbeddedWindowed) or
       (not FActiveSubtitleIsEmbedded) or
       (not FTimedTextFileLoaded) or
       (not Assigned(FTimedText)) then
      Exit;

    if (FEmbeddedWindowStartMs >= 0) and
       (MediaTimeMs >= FEmbeddedWindowStartMs) and
       ((MediaTimeMs < FEmbeddedWindowEndMs) or FEmbeddedWindowEndOfTrack) then
      Exit;

    if FEmbeddedWindowLoading then
      begin
        Result := S_FALSE;
        Exit;
      end;

    if not FindEmbeddedTrack(DWORD(FActiveEmbeddedStreamIndex), Track) then
      begin
        Result := MF_E_INVALIDSTREAMNUMBER;
        Exit;
      end;

    FEmbeddedWindowLoading := True;
    Generation := FEmbeddedWindowGeneration;
    MediaFileName := FMediaFileName;
  finally
    FLock.Release();
  end;

  WindowStartMs := MediaTimeMs - MF_SUBTITLE_WINDOW_LOOK_BEHIND_MS;
  if (WindowStartMs < 0) then
    WindowStartMs := 0;
  WindowEndMs := MediaTimeMs + MF_SUBTITLE_WINDOW_AHEAD_MS;

  Result := TMfEmbeddedSubtitleReader.LoadMatroskaTextTrackWindow(
              MediaFileName,
              Track,
              WindowStartMs,
              WindowEndMs,
              Cues,
              EndOfTrack);

  FriendlyName := Track.Name;
  if (FriendlyName = '') then
    FriendlyName := Track.Language;
  if (FriendlyName = '') then
    FriendlyName := 'Embedded subtitles';

  FLock.Acquire();
  try
    if Generation <> FEmbeddedWindowGeneration then
      begin
        Result := E_ABORT;
        Exit;
      end;

    FEmbeddedWindowLoading := False;

    if FAILED(Result) then
      Exit;

    ImportResult := FTimedText.ImportCues(Cues,
                                          MediaFileName,
                                          Track.Language,
                                          FriendlyName);
    if FAILED(ImportResult) then
      begin
        Result := ImportResult;
        Exit;
      end;

    FEmbeddedWindowStartMs := WindowStartMs;
    FEmbeddedWindowEndMs := WindowEndMs;
    FEmbeddedWindowEndOfTrack := EndOfTrack;
    Result := S_OK;
  finally
    FLock.Release();
  end;
end;


function TMfSubtitleCompositor.EnsureFullEmbeddedTrack(): HRESULT;
var
  Track: TMfEmbeddedSubtitleTrackInfo;
  NewTimedText: TMfTimedText;
  OldTimedText: TMfTimedText;
  MediaFileName: WideString;
  Generation: Integer;
  Committed: Boolean;

begin

  Track.Reset();
  OldTimedText := nil;
  Committed := False;

  FLock.Acquire();
  try
    if not FEmbeddedWindowed then
      begin
        Result := S_OK;
        Exit;
      end;

    if not FindEmbeddedTrack(DWORD(FActiveEmbeddedStreamIndex), Track) then
      begin
        Result := MF_E_INVALIDSTREAMNUMBER;
        Exit;
      end;

    MediaFileName := FMediaFileName;
    Inc(FEmbeddedWindowGeneration);
    Generation := FEmbeddedWindowGeneration;
    FEmbeddedWindowLoading := False;
  finally
    FLock.Release();
  end;

  NewTimedText := TMfTimedText.Create(0,
                                      MediaFileName,
                                      Track.Language);
  if not Assigned(NewTimedText) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  Result := TMfEmbeddedSubtitleReader.ImportTextTrack(MediaFileName,
                                                      Track,
                                                      NewTimedText);
  if (Result <> S_OK) then
    begin
      NewTimedText.Free();
      Exit;
    end;

  FLock.Acquire();
  try
    if (Generation = FEmbeddedWindowGeneration) and
       FEmbeddedWindowed and
       (FActiveEmbeddedStreamIndex = Integer(Track.StreamIndex)) then
      begin
        OldTimedText := FTimedText;
        FTimedText := NewTimedText;
        NewTimedText := nil;
        ResetEmbeddedWindowState();
        FPreferredLanguage := FTimedText.PreferredLanguage;
        Committed := True;
      end;
  finally
    FLock.Release();
  end;

  OldTimedText.Free();
  NewTimedText.Free();

  if not Committed then
    Result := E_ABORT;
end;


function TMfSubtitleCompositor.ExportActiveWebVtt(out AData: TBytes;
                                                  out ALanguageTag: string;
                                                  out AFriendlyLanguageName: string): HRESULT;
begin

  SetLength(AData,
            0);
  ALanguageTag := '';
  AFriendlyLanguageName := '';

  Result := EnsureFullEmbeddedTrack();
  if FAILED(Result) then
    Exit;

  FLock.Acquire();
  try
    if (not FTimedTextFileLoaded) or (not Assigned(FTimedText)) then
      begin
        Result := S_FALSE;
        Exit;
      end;

    ALanguageTag := FTimedText.PreferredLanguage;
    AFriendlyLanguageName := FTimedText.FriendlyLanguage;
    Result := FTimedText.ExportWebVtt(AData);
  finally
    FLock.Release();
  end;
end;


procedure TMfSubtitleCompositor.ResetEmbeddedWindowState();
begin

  Inc(FEmbeddedWindowGeneration);
  FEmbeddedWindowed := False;
  FEmbeddedWindowLoading := False;
  FEmbeddedWindowStartMs := -1;
  FEmbeddedWindowEndMs := -1;
  FEmbeddedWindowEndOfTrack := False;
end;

function TMfSubtitleCompositor.BuildPlainText(const Track: TSubTitleTrack): string;
var
  I: Integer;

begin

  Result := '';

  for I := 0 to Length(Track.TrackText) - 1 do
    begin
      if (I > 0) then
        Result := Result + sLineBreak;

      Result := Result + Track.TrackText[I].TextLine;
    end;
end;


procedure TMfSubtitleCompositor.SetSubtitleAspectRatio(aValue: Single);
begin

  if (aValue <= 0.0) then
    aValue := 16.0 / 9.0;

  FSubtitleAspectRatio := aValue;
end;


procedure TMfSubtitleCompositor.SetSubtitleFontScale(aValue: Single);
begin

  if aValue <= 0.0 then
    aValue := 1.0;

  if Abs(FSubtitleFontScale - aValue) > 0.0001 then
    begin
      FSubtitleFontScale := aValue;
      ResetOverlayCache();
    end;
end;


function TMfSubtitleCompositor.GetSubtitleTargetRect(const ClientRect: TRect): TRect;
var
  clientWidth: Integer;
  clientHeight: Integer;
  targetWidth: Integer;
  targetHeight: Integer;
  targetLeft: Integer;
  targetTop: Integer;
  clientRatio: Single;

begin

  Result := ClientRect;
  clientWidth := ClientRect.Right - ClientRect.Left;
  clientHeight := ClientRect.Bottom - ClientRect.Top;

  if (clientWidth <= 0) or (clientHeight <= 0) or (FSubtitleAspectRatio <= 0.0) then
    Exit;

  clientRatio := clientWidth / clientHeight;

  if (clientRatio > FSubtitleAspectRatio) then
    begin
      targetHeight := clientHeight;
      targetWidth := Round(targetHeight * FSubtitleAspectRatio);
      targetLeft := ClientRect.Left + ((clientWidth - targetWidth) div 2);
      targetTop := ClientRect.Top;
    end
  else
    begin
      targetWidth := clientWidth;
      targetHeight := Round(targetWidth / FSubtitleAspectRatio);
      targetLeft := ClientRect.Left;
      targetTop := ClientRect.Top + ((clientHeight - targetHeight) div 2);
    end;

  Result := Rect(targetLeft,
                 targetTop,
                 targetLeft + targetWidth,
                 targetTop + targetHeight);
end;

function TMfSubtitleCompositor.TryGetSubtitleTextAtTime(MediaTimeMs: Int64;
                                                       out SubtitleText: string;
                                                       out Track: TSubTitleTrack): Boolean;
begin

  SubtitleText := '';
  Track.Start := 0;
  Track.Stop := 0;
  Track.Duration := 0;
  SetLength(Track.TrackText, 0);

  EnsureEmbeddedWindow(MediaTimeMs);

  // Keep the cue lookup and plain-text extraction under one lock. TrackText
  // owns formatted-text objects that belong to FTimedText, so a live language
  // switch must not free the old model between these two operations.
  FLock.Acquire();

  try
    Result := FTimedTextFileLoaded and
              Assigned(FTimedText) and
              FTimedText.TryGetTrackAtTime(MediaTimeMs,
                                           Track);
    if Result then
      SubtitleText := BuildPlainText(Track);
  finally
    FLock.Release();
  end;
end;


function TMfSubtitleCompositor.CompositeRgb32(VideoBuffer: Pointer;
                                             BufferSize: UINT32;
                                             Width: Integer;
                                             Height: Integer;
                                             Stride: Integer;
                                             MediaTimeMs: Int64): HRESULT;
var
  subtitleText: string;
  track: TSubTitleTrack;
  copyBytes: Integer;
  requiredBytes: UInt64;

begin

  Result := S_OK;

  if (VideoBuffer = nil) or
     (BufferSize = 0) or
     (Width <= 0) or
     (Height <= 0) or
     (Stride = 0) then
    begin

      Result := E_INVALIDARG;
      Exit;
    end;

  copyBytes := Width * 4;
  if (Abs(Stride) < copyBytes) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  requiredBytes := UInt64(Abs(Stride)) * UInt64(Height);
  if (requiredBytes > UInt64(BufferSize)) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if not TryGetSubtitleTextAtTime(MediaTimeMs,
                                  subtitleText,
                                  track) then
    Exit;

  subtitleText := Trim(subtitleText);
  if (subtitleText = '') then
    Exit;

  if (not FOverlayValid) or
     (not SameStr(FOverlayText,
                  subtitleText)) or
     (FOverlayFrameWidth <> Width) or
     (FOverlayFrameHeight <> Height) or
     (Abs(FOverlayAspectRatio - FSubtitleAspectRatio) > 0.0001) then
    begin

      Result := BuildSubtitleOverlay(subtitleText,
                                     Width,
                                     Height);
      if (Result <> S_OK) then
        Exit;
    end;

  Result := BlendCachedOverlay(VideoBuffer,
                               Width,
                               Height,
                               Stride);
end;

end.
