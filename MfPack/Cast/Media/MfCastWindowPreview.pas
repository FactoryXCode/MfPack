// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Module: MfCastWindowPreview.pas
// Description: Window-backed RGB32 preview sink for the Cast transcoder.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.

unit MfCastWindowPreview;

interface

uses
  WinApi.Windows,
  WinApi.MediaFoundationApi.MfObjects,
  System.SyncObjs,
  MfCastMediaInterfaces;

type
  TMfCastWindowPreviewSink = class(TInterfacedObject, IMfCastPreviewSink)
  private
    FLock: TCriticalSection;
    FWindow: HWND;
    FVideoWidth: UINT32;
    FVideoHeight: UINT32;
    procedure ClearWindow(const AWindow: HWND);
  public
    constructor Create();
    destructor Destroy(); override;
    function SetWindow(const AWindow: HWND): HRESULT;
    function IsEnabled(): Boolean;
    function ConfigureVideo(const AWidth: UINT32;
                            const AHeight: UINT32): HRESULT;
    function PresentSample(const ASample: IMFSample;
                           const ASampleTime100ns: Int64;
                           const ASampleDuration100ns: Int64): HRESULT;
    function Flush(): HRESULT;
  end;

implementation

uses
  WinApi.WinError,
  WinApi.MediaFoundationApi.MfError;

constructor TMfCastWindowPreviewSink.Create();
begin
  inherited Create();
  FLock := TCriticalSection.Create();
  FWindow := 0;
  FVideoWidth := 0;
  FVideoHeight := 0;
end;

destructor TMfCastWindowPreviewSink.Destroy();
begin
  SetWindow(0);
  FLock.Free();
  inherited Destroy();
end;

procedure TMfCastWindowPreviewSink.ClearWindow(const AWindow: HWND);
var
  DC: HDC;
  ClientRect: TRect;
begin
  if (AWindow = 0) or (not IsWindow(AWindow)) then
    Exit;
  DC := GetDC(AWindow);
  if DC = 0 then
    Exit;
  try
    if GetClientRect(AWindow, ClientRect) then
      FillRect(DC, ClientRect, HBRUSH(GetStockObject(BLACK_BRUSH)));
  finally
    ReleaseDC(AWindow, DC);
  end;
end;

function TMfCastWindowPreviewSink.SetWindow(const AWindow: HWND): HRESULT;
var
  PreviousWindow: HWND;
begin
  if (AWindow <> 0) and (not IsWindow(AWindow)) then
    begin
      Result := HRESULT_FROM_WIN32(ERROR_INVALID_WINDOW_HANDLE);
      Exit;
    end;
  FLock.Acquire();
  try
    PreviousWindow := FWindow;
    FWindow := AWindow;
  finally
    FLock.Release();
  end;
  if (PreviousWindow <> 0) and (PreviousWindow <> AWindow) then
    ClearWindow(PreviousWindow);
  if AWindow <> 0 then
    ClearWindow(AWindow);
  Result := S_OK;
end;

function TMfCastWindowPreviewSink.IsEnabled(): Boolean;
begin
  FLock.Acquire();
  try
    Result := FWindow <> 0;
  finally
    FLock.Release();
  end;
end;

function TMfCastWindowPreviewSink.ConfigureVideo(const AWidth: UINT32;
                                                  const AHeight: UINT32): HRESULT;
begin
  if (AWidth = 0) or (AHeight = 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;
  FLock.Acquire();
  try
    FVideoWidth := AWidth;
    FVideoHeight := AHeight;
  finally
    FLock.Release();
  end;
  Result := S_OK;
end;

function TMfCastWindowPreviewSink.PresentSample(
  const ASample: IMFSample;
  const ASampleTime100ns: Int64;
  const ASampleDuration100ns: Int64): HRESULT;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  Window: HWND;
  VideoWidth: UINT32;
  VideoHeight: UINT32;
  RequiredLength: UInt64;
  DC: HDC;
  MemoryDC: HDC;
  BackBitmap: HBITMAP;
  PreviousBitmap: HGDIOBJ;
  ClientRect: TRect;
  ClientWidth: Integer;
  ClientHeight: Integer;
  DestinationWidth: Integer;
  DestinationHeight: Integer;
  DestinationX: Integer;
  DestinationY: Integer;
  BitmapInfo: WinApi.Windows.TBitmapInfo;
begin
  Result := S_OK;
  if not Assigned(ASample) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  // Keep the target locked while GDI is using it. SetWindow(0) can then be
  // used as a lifetime barrier before the host destroys its preview control.
  FLock.Acquire();
  try
    Window := FWindow;
    VideoWidth := FVideoWidth;
    VideoHeight := FVideoHeight;
    if Window = 0 then
      Exit;
    if (not IsWindow(Window)) or (VideoWidth = 0) or (VideoHeight = 0) then
      begin
        Result := E_HANDLE;
        Exit;
      end;

    Buffer := nil;
    Result := ASample.ConvertToContiguousBuffer(@Buffer);
    if FAILED(Result) then
      Exit;
    Data := nil;
    MaxLength := 0;
    CurrentLength := 0;
    Result := Buffer.Lock(Data, @MaxLength, @CurrentLength);
    if FAILED(Result) then
      Exit;
    try
      RequiredLength := UInt64(VideoWidth) * UInt64(VideoHeight) * 4;
      if UInt64(CurrentLength) < RequiredLength then
        begin
          Result := MF_E_BUFFERTOOSMALL;
          Exit;
        end;
      DC := GetDC(Window);
      if DC = 0 then
        begin
          Result := HRESULT_FROM_WIN32(GetLastError());
          Exit;
        end;
      try
        if not GetClientRect(Window, ClientRect) then
          begin
            Result := HRESULT_FROM_WIN32(GetLastError());
            Exit;
          end;
        ClientWidth := ClientRect.Right - ClientRect.Left;
        ClientHeight := ClientRect.Bottom - ClientRect.Top;
        if (ClientWidth <= 0) or (ClientHeight <= 0) then
          Exit;
        DestinationWidth := ClientWidth;
        DestinationHeight := Integer((Int64(ClientWidth) * VideoHeight) div VideoWidth);
        if DestinationHeight > ClientHeight then
          begin
            DestinationHeight := ClientHeight;
            DestinationWidth := Integer((Int64(ClientHeight) * VideoWidth) div VideoHeight);
          end;
        DestinationX := (ClientWidth - DestinationWidth) div 2;
        DestinationY := (ClientHeight - DestinationHeight) div 2;
        // Compose the complete frame off-screen. Painting the black bars and
        // video separately onto the window exposes a black intermediate frame
        // and causes visible flicker at the source frame rate.
        MemoryDC := CreateCompatibleDC(DC);
        if MemoryDC = 0 then
          begin
            Result := HRESULT_FROM_WIN32(GetLastError());
            Exit;
          end;
        try
          BackBitmap := CreateCompatibleBitmap(DC,
                                               ClientWidth,
                                               ClientHeight);
          if BackBitmap = 0 then
            begin
              Result := HRESULT_FROM_WIN32(GetLastError());
              Exit;
            end;
          PreviousBitmap := SelectObject(MemoryDC,
                                         BackBitmap);
          try
            FillRect(MemoryDC,
                     ClientRect,
                     HBRUSH(GetStockObject(BLACK_BRUSH)));
            ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
            BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
            BitmapInfo.bmiHeader.biWidth := Integer(VideoWidth);
            // The frame pump supplies the conventional bottom-up RGB32 DIB
            // used by the Media Foundation sink writer.
            BitmapInfo.bmiHeader.biHeight := Integer(VideoHeight);
            BitmapInfo.bmiHeader.biPlanes := 1;
            BitmapInfo.bmiHeader.biBitCount := 32;
            BitmapInfo.bmiHeader.biCompression := BI_RGB;
            BitmapInfo.bmiHeader.biSizeImage := DWORD(RequiredLength);
            SetStretchBltMode(MemoryDC, HALFTONE);
            if StretchDIBits(MemoryDC,
                             DestinationX,
                             DestinationY,
                             DestinationWidth,
                             DestinationHeight,
                             0,
                             0,
                             Integer(VideoWidth),
                             Integer(VideoHeight),
                             Data,
                             BitmapInfo,
                             DIB_RGB_COLORS,
                             SRCCOPY) = -1 then
              Result := E_FAIL
            else
              if not BitBlt(DC,
                            0,
                            0,
                            ClientWidth,
                            ClientHeight,
                            MemoryDC,
                            0,
                            0,
                            SRCCOPY) then
                Result := HRESULT_FROM_WIN32(GetLastError());
          finally
            SelectObject(MemoryDC,
                         PreviousBitmap);
            DeleteObject(BackBitmap);
          end;
        finally
          DeleteDC(MemoryDC);
        end;
      finally
        ReleaseDC(Window, DC);
      end;
    finally
      Buffer.Unlock();
    end;
  finally
    FLock.Release();
  end;
end;

function TMfCastWindowPreviewSink.Flush(): HRESULT;
var
  Window: HWND;
begin
  FLock.Acquire();
  try
    Window := FWindow;
  finally
    FLock.Release();
  end;
  ClearWindow(Window);
  Result := S_OK;
end;

end.
