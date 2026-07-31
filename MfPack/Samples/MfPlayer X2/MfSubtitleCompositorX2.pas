// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfSubtitleCompositorX2.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
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
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
unit MfSubtitleCompositorX2;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  TimedTextClass;

type
  TMfSubtitleCompositor = class(TObject)
  private
    FTimedText: TMfTimedText;
    FMediaFileName: WideString;
    FPreferredLanguage: string;
    FTimedTextFileLoaded: Boolean;
    FSubtitleAspectRatio: Single;

    procedure ReleaseTimedText();
    function BuildPlainText(const Track: TSubTitleTrack): string;
    function GetSubtitleTargetRect(const ClientRect: TRect): TRect;
    procedure SetSubtitleAspectRatio(aValue: Single);

  public

    constructor Create();
    destructor Destroy(); override;

    function OpenTimedTextFile(const MediaFileName: WideString;
                               const PreferredLanguage: string): HRESULT;
    function ExportActiveWebVtt(out AData: TBytes;
                                out ALanguageTag: string;
                                out AFriendlyLanguageName: string): HRESULT;
    procedure Close();

    function TryGetSubtitleAtTime(MediaTimeMs: Int64;
                                  out Track: TSubTitleTrack): Boolean;
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
    property TimedTextFileLoaded: Boolean read FTimedTextFileLoaded;
  end;


implementation


constructor TMfSubtitleCompositor.Create();
begin

  inherited Create();

  FTimedText := nil;
  FTimedTextFileLoaded := False;
  FSubtitleAspectRatio := 16.0 / 9.0;
end;


destructor TMfSubtitleCompositor.Destroy();
begin

  Close();
  inherited Destroy();
end;


procedure TMfSubtitleCompositor.ReleaseTimedText();
begin

  if Assigned(FTimedText) then
    FreeAndNil(FTimedText);
end;


procedure TMfSubtitleCompositor.Close();
begin

  ReleaseTimedText();
  FTimedTextFileLoaded := False;
  FMediaFileName := '';
end;


function TMfSubtitleCompositor.OpenTimedTextFile(const MediaFileName: WideString;
                                                const PreferredLanguage: string): HRESULT;
var
  hr: HRESULT;

begin

  Close();

  FMediaFileName := MediaFileName;
  FPreferredLanguage := PreferredLanguage;

  FTimedText := TMfTimedText.Create(0,
                                    MediaFileName,
                                    PreferredLanguage);
  if not Assigned(FTimedText) then
    begin

      Result := E_OUTOFMEMORY;
      Exit;
    end;

  hr := FTimedText.OpenTimedTextFile(MediaFileName);

  FTimedTextFileLoaded := Succeeded(hr) and
                          (hr <> HRESULT(ERROR_FILE_NOT_FOUND)) and
                          (hr <> HRESULT(ERROR_INVALID_PARAMETER));

  Result := hr;
end;


function TMfSubtitleCompositor.ExportActiveWebVtt(
  out AData: TBytes;
  out ALanguageTag: string;
  out AFriendlyLanguageName: string): HRESULT;
begin
  SetLength(AData, 0);
  ALanguageTag := '';
  AFriendlyLanguageName := '';
  if (not FTimedTextFileLoaded) or (not Assigned(FTimedText)) then
    begin
      Result := S_FALSE;
      Exit;
    end;
  ALanguageTag := FTimedText.PreferredLanguage;
  AFriendlyLanguageName := FTimedText.FriendlyLanguage;
  Result := FTimedText.ExportWebVtt(AData);
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


function TMfSubtitleCompositor.TryGetSubtitleAtTime(MediaTimeMs: Int64;
                                                   out Track: TSubTitleTrack): Boolean;
begin

  Track.Start := 0;
  Track.Stop := 0;
  Track.Duration := 0;

  SetLength(Track.TrackText,
            0);
  Result := FTimedTextFileLoaded and
            Assigned(FTimedText) and
            FTimedText.TryGetTrackAtTime(MediaTimeMs,
                                         Track);
end;


function TMfSubtitleCompositor.TryGetSubtitleTextAtTime(MediaTimeMs: Int64;
                                                       out SubtitleText: string;
                                                       out Track: TSubTitleTrack): Boolean;
begin

  SubtitleText := '';
  Result := TryGetSubtitleAtTime(MediaTimeMs,
                                 Track);

  if Result then
    SubtitleText := BuildPlainText(Track);
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
  sourceRow: Pointer;
  targetRow: Pointer;
  requiredBytes: UInt64;
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

  rcClient := Rect(0,
                   0,
                   Width,
                   Height);

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

  textValue := subtitleText;
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
                          GetDeviceCaps(screenDC,
                                        LOGPIXELSY),
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
      Exit;

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
      Exit;

    FillChar(bmi,
             SizeOf(bmi),
             0);
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

    copyBytes := regionWidth * 4;
    sourceRow := Pointer(NativeInt(VideoBuffer) +
                         (NativeInt(regionTop) * NativeInt(Stride)) +
                         (NativeInt(regionLeft) * 4));
    targetRow := dibBits;
    Result := MFCopyImage(targetRow,
                          copyBytes,
                          sourceRow,
                          Stride,
                          copyBytes,
                          regionHeight);
    if FAILED(Result) then
      Exit;

    rcLocal := Rect(rcDraw.Left - regionLeft,
                    rcDraw.Top - regionTop,
                    rcDraw.Right - regionLeft,
                    rcDraw.Bottom - regionTop);

    SetTextColor(memDC,
                 RGB(0,
                     0,
                     0));
    for dx := -1 to 1 do
      for dy := -1 to 1 do
        if (dx <> 0) or (dy <> 0) then
          begin
            rcText := rcLocal;
            OffsetRect(rcText,
                       dx,
                       dy);
            DrawTextW(memDC,
                      PWideChar(textValue),
                      Length(textValue),
                      rcText,
                      textFlags);
          end;

    SetTextColor(memDC,
                 RGB(255,
                     255,
                     255));
    DrawTextW(memDC,
              PWideChar(textValue),
              Length(textValue),
              rcLocal,
              textFlags);

    GdiFlush();

    sourceRow := dibBits;
    targetRow := Pointer(NativeInt(VideoBuffer) +
                         (NativeInt(regionTop) * NativeInt(Stride)) +
                         (NativeInt(regionLeft) * 4));
    Result := MFCopyImage(targetRow,
                          Stride,
                          sourceRow,
                          copyBytes,
                          copyBytes,
                          regionHeight);
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

end.
