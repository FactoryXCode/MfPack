// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastWindowPreview.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Window-backed RGB32 preview sink for the Cast transcoder.
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
// Source: -
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
unit MfCastWindowPreview;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {System}
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMSysCom,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  {Cast}
  MfCastMediaInterfaces;

type
  TMfCastWindowPreviewSink = class(TInterfacedObject, IMfCastPreviewSink)
  private
    FLock: TCriticalSection;
    FWindow: HWND;
    FVideoWidth: UINT32;
    FVideoHeight: UINT32;
    FWaveOut: HWAVEOUT;
    FAudioChannels: UINT32;
    FAudioSamplesPerSecond: UINT32;
    FAudioBitsPerSample: UINT32;
    FQueuedAudio: TList<Pointer>;

    procedure ClearWindow(const AWindow: HWND);
    procedure ReleaseCompletedAudioLocked();
    procedure CloseAudioLocked();

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
    function ConfigureAudio(const AChannels: UINT32;
                            const ASamplesPerSecond: UINT32;
                            const ABitsPerSample: UINT32): HRESULT;
    function PresentAudioSample(const ASample: IMFSample;
                                const ASampleTime100ns: Int64;
                                const ASampleDuration100ns: Int64): HRESULT;
    function PauseAudio(): HRESULT;
    function ResumeAudio(): HRESULT;
    function Flush(): HRESULT;
  end;

implementation


type
  PMfCastWaveBuffer = ^TMfCastWaveBuffer;
  TMfCastWaveBuffer = record
    Header: WAVEHDR;
    Data: TBytes;
  end;

constructor TMfCastWindowPreviewSink.Create();
begin

  inherited Create();

  FLock := TCriticalSection.Create();
  FWindow := 0;
  FVideoWidth := 0;
  FVideoHeight := 0;
  FWaveOut := 0;
  FAudioChannels := 0;
  FAudioSamplesPerSecond := 0;
  FAudioBitsPerSample := 0;
  FQueuedAudio := TList<Pointer>.Create();
end;


destructor TMfCastWindowPreviewSink.Destroy();
begin

  SetWindow(0);
  FLock.Acquire();

  try
    CloseAudioLocked();
  finally
    FLock.Release();
  end;

  FQueuedAudio.Free();
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

  if (DC = 0) then
    Exit;

  try
    if GetClientRect(AWindow,
                     ClientRect) then
      FillRect(DC,
               ClientRect,
               HBRUSH(GetStockObject(BLACK_BRUSH)));
  finally
    ReleaseDC(AWindow,
              DC);
  end;
end;


procedure TMfCastWindowPreviewSink.ReleaseCompletedAudioLocked();
var
  I: Integer;
  WaveBuffer: PMfCastWaveBuffer;

begin

  for I := FQueuedAudio.Count - 1 downto 0 do
    begin
      WaveBuffer := PMfCastWaveBuffer(FQueuedAudio[I]);

      if ((WaveBuffer^.Header.dwFlags and WHDR_DONE) <> 0) then
        begin
          if (FWaveOut <> 0) then
            waveOutUnprepareHeader(FWaveOut,
                                   @WaveBuffer^.Header,
                                   SizeOf(WAVEHDR));
          Dispose(WaveBuffer);
          FQueuedAudio.Delete(I);
        end;
    end;
end;


procedure TMfCastWindowPreviewSink.CloseAudioLocked();
var
  I: Integer;
  WaveBuffer: PMfCastWaveBuffer;

begin

  if (FWaveOut <> 0) then
    waveOutReset(FWaveOut);

  for I := FQueuedAudio.Count - 1 downto 0 do
    begin
      WaveBuffer := PMfCastWaveBuffer(FQueuedAudio[I]);

      if (FWaveOut <> 0) then
        waveOutUnprepareHeader(FWaveOut,
                               @WaveBuffer^.Header,
                               SizeOf(WAVEHDR));
      Dispose(WaveBuffer);
    end;
  FQueuedAudio.Clear();

  if (FWaveOut <> 0) then
    waveOutClose(FWaveOut);

  FWaveOut := 0;
  FAudioChannels := 0;
  FAudioSamplesPerSecond := 0;
  FAudioBitsPerSample := 0;
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

    if (AWindow = 0) then
      CloseAudioLocked();
  finally
    FLock.Release();
  end;

  if (PreviousWindow <> 0) and (PreviousWindow <> AWindow) then
    ClearWindow(PreviousWindow);

  if (AWindow <> 0) then
    ClearWindow(AWindow);

  Result := S_OK;
end;


function TMfCastWindowPreviewSink.IsEnabled(): Boolean;
begin

  FLock.Acquire();

  try
    Result := (FWindow <> 0);
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


function TMfCastWindowPreviewSink.ConfigureAudio(const AChannels: UINT32;
                                                 const ASamplesPerSecond: UINT32;
                                                 const ABitsPerSample: UINT32): HRESULT;
var
  WaveFormat: WAVEFORMATEX;
  MmResult: UINT;

begin

  if (AChannels = 0) or
     (ASamplesPerSecond = 0) or
     (ABitsPerSample = 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  FLock.Acquire();

  try
    if (FWindow = 0) then
      begin
        Result := S_FALSE;
        Exit;
      end;

    if (FWaveOut <> 0) and
       (FAudioChannels = AChannels) and
       (FAudioSamplesPerSecond = ASamplesPerSecond) and
       (FAudioBitsPerSample = ABitsPerSample) then
      begin
        ReleaseCompletedAudioLocked();
        Result := S_OK;
        Exit;
      end;

    CloseAudioLocked();
    FillChar(WaveFormat,
             SizeOf(WaveFormat),
             0);

    WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
    WaveFormat.nChannels := AChannels;
    WaveFormat.nSamplesPerSec := ASamplesPerSecond;
    WaveFormat.wBitsPerSample := ABitsPerSample;
    WaveFormat.nBlockAlign := (AChannels * ABitsPerSample) div 8;
    WaveFormat.nAvgBytesPerSec := ASamplesPerSecond * WaveFormat.nBlockAlign;

    MmResult := waveOutOpen(@FWaveOut,
                            WAVE_MAPPER,
                            @WaveFormat,
                            0,
                            0,
                            CALLBACK_NULL);
    if (MmResult <> 0) then
      begin
        FWaveOut := 0;
        Result := E_FAIL;
        Exit;
      end;

    FAudioChannels := AChannels;
    FAudioSamplesPerSecond := ASamplesPerSecond;
    FAudioBitsPerSample := ABitsPerSample;
    Result := S_OK;
  finally
    FLock.Release();
  end;
end;


function TMfCastWindowPreviewSink.PresentAudioSample(const ASample: IMFSample;
                                                     const ASampleTime100ns: Int64;
                                                     const ASampleDuration100ns: Int64): HRESULT;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  WaveBuffer: PMfCastWaveBuffer;
  MmResult: UINT;

begin

  Result := S_OK;

  if not Assigned(ASample) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  FLock.Acquire();

  try
    if (FWindow = 0) or (FWaveOut = 0) then
      Exit;

    ReleaseCompletedAudioLocked();
    Buffer := nil;
    Result := ASample.ConvertToContiguousBuffer(@Buffer);
    if FAILED(Result) then
      Exit;

    Data := nil;
    MaxLength := 0;
    CurrentLength := 0;
    Result := Buffer.Lock(Data,
                          @MaxLength,
                          @CurrentLength);

    if FAILED(Result) then
      Exit;

    try
      if (CurrentLength = 0) then
        Exit;

      WaveBuffer := nil;

      try
        New(WaveBuffer);
        WaveBuffer^.Data := nil;

        FillChar(WaveBuffer^.Header,
                 SizeOf(WAVEHDR),
                 0);

        SetLength(WaveBuffer^.Data,
                  CurrentLength);

        Move(Data^,
             WaveBuffer^.Data[0],
             CurrentLength);

      except
        on E: EOutOfMemory do
          begin
            if Assigned(WaveBuffer) then
              Dispose(WaveBuffer);
            Result := E_OUTOFMEMORY;
            Exit;
          end;
      end;

      WaveBuffer^.Header.lpData := PAnsiChar(@WaveBuffer^.Data[0]);
      WaveBuffer^.Header.dwBufferLength := CurrentLength;

      MmResult := waveOutPrepareHeader(FWaveOut,
                                       @WaveBuffer^.Header,
                                       SizeOf(WAVEHDR));
      if (MmResult <> 0) then
        begin
          Dispose(WaveBuffer);
          Result := E_FAIL;
          Exit;
        end;

      MmResult := waveOutWrite(FWaveOut,
                               @WaveBuffer^.Header,
                               SizeOf(WAVEHDR));
      if (MmResult <> 0) then
        begin
          waveOutUnprepareHeader(FWaveOut,
                                 @WaveBuffer^.Header,
                                 SizeOf(WAVEHDR));
          Dispose(WaveBuffer);
          Result := E_FAIL;
          Exit;
        end;

      FQueuedAudio.Add(WaveBuffer);
      Result := S_OK;
    finally
      Buffer.Unlock();
    end;
  finally
    FLock.Release();
  end;
end;


function TMfCastWindowPreviewSink.PauseAudio(): HRESULT;
begin

  FLock.Acquire();

  try
    if (FWaveOut <> 0) and (waveOutPause(FWaveOut) <> 0) then
      Result := E_FAIL
    else
      Result := S_OK;
  finally
    FLock.Release();
  end;
end;


function TMfCastWindowPreviewSink.ResumeAudio(): HRESULT;
begin

  FLock.Acquire();

  try
    if (FWaveOut <> 0) and (waveOutRestart(FWaveOut) <> 0) then
      Result := E_FAIL
    else
      Result := S_OK;
  finally
    FLock.Release();
  end;
end;


function TMfCastWindowPreviewSink.PresentSample(const ASample: IMFSample;
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

    if (Window = 0) then
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
    Result := Buffer.Lock(Data,
                          @MaxLength,
                          @CurrentLength);

    if FAILED(Result) then
      Exit;

    try
      RequiredLength := UInt64(VideoWidth) * UInt64(VideoHeight) * 4;

      if (UInt64(CurrentLength) < RequiredLength) then
        begin
          Result := MF_E_BUFFERTOOSMALL;
          Exit;
        end;

      DC := GetDC(Window);

      if (DC = 0) then
        begin
          Result := HRESULT_FROM_WIN32(GetLastError());
          Exit;
        end;

      try
        if not GetClientRect(Window,
                             ClientRect) then
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

        if (DestinationHeight > ClientHeight) then
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

        if (MemoryDC = 0) then
          begin
            Result := HRESULT_FROM_WIN32(GetLastError());
            Exit;
          end;

        try
          BackBitmap := CreateCompatibleBitmap(DC,
                                               ClientWidth,
                                               ClientHeight);
          if (BackBitmap = 0) then
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

            ZeroMemory(@BitmapInfo,
                       SizeOf(BitmapInfo));

            BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
            BitmapInfo.bmiHeader.biWidth := Integer(VideoWidth);

            // The frame pump supplies the conventional bottom-up RGB32 DIB
            // used by the Media Foundation sink writer.
            BitmapInfo.bmiHeader.biHeight := Integer(VideoHeight);
            BitmapInfo.bmiHeader.biPlanes := 1;
            BitmapInfo.bmiHeader.biBitCount := 32;
            BitmapInfo.bmiHeader.biCompression := BI_RGB;
            BitmapInfo.bmiHeader.biSizeImage := DWORD(RequiredLength);

            SetStretchBltMode(MemoryDC,
                              HALFTONE);

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
        ReleaseDC(Window,
                  DC);
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
    CloseAudioLocked();
  finally
    FLock.Release();
  end;

  ClearWindow(Window);
  Result := S_OK;
end;

end.
