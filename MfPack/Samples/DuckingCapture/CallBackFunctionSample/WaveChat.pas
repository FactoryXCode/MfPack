// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WaveChat.pas
// Kind: Pascal / Delphi unit
// Release date: 04-10-2020
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Wave in renderer class.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Note that this sample requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
//==============================================================================
// Source: DuckingCaptureSample: wavechat.h, wavechat.ccp
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit WaveChat;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMeApi,
  {Appliccation}
  ChatTransport;

// const WAVE_MAPPED_DEFAULT_COMMUNICATION_DEVICE = $0010  declared in MMeApi

type

  CWaveChat = class(CChatTransport)
  private
    _waveHandle: HWAVEIN; // wavein handle, returned by function waveInOpen.
    _waveHeader1: WAVEHDR;
    _waveHeader2: WAVEHDR;
    _waveBuffer1: array of Word;
    _waveBuffer2: array of Word;

  public
    constructor Create(AppWindow: HWND); override;
    destructor Destroy(); override;
    function Initialize(const UseInputDevice: Boolean): Boolean; override;
    procedure Shutdown(); override;

    function StartChat(const HideFromVolumeMixer: Boolean): Boolean; override;
    procedure StopChat(); override;

    function CanStartChat(): Boolean;
    function TransportType(): ChatTransportType; override;

  end;


implementation


// NOTES: - The use of a systemproc can't access local (class) variables!
//          In this sample there is no reason to implement for instance a window handle intercept.
//        - Here we altered WaveInproc in MessageHandler.
//
procedure MessageHandler(hWaveIn: IntPtr;
                         uMsg: UINT;
                         dwInstance: DWord;
                         wPar: WPARAM;
                         lPar: LPARAM); stdcall;
var
  mmr: MMRESULT;
  hwaveHandle: IntPtr;
  waveHeader: LPWAVEHDR;

begin

  case uMsg of

    MM_WIM_OPEN:  begin
                    // ignore
                  end;

    MM_WIM_CLOSE: begin
                    // ignore
                  end;

    MM_WIM_DATA:  begin
                    //
                    // Process the capture data since we've received a buffers worth of data.
                    //
                    //  In real life, we'd copy the capture data out of the waveHeader that just completed and process it, but since
                    //  this is a sample, we discard the data and simply re-submit the buffer.
                    //
                    hwaveHandle := hWaveIn;
                    waveHeader := LPWAVEHDR(lPar);
                    if hWaveIn <> 0 then
                      begin
                        mmr := waveInAddBuffer(hwaveHandle,
                                               waveHeader,
                                               SizeOf(WAVEHDR));
                        if mmr <> MMSYSERR_NOERROR then
                          MessageBox(hWaveIn,
                                     'Failed to add buffer',
                                     'Error',
                                     MB_OK);
                      end;
                  end;
   end;
end;


// CWaveChat ///////////////////////////////////////////////////////////////////


constructor CWaveChat.Create(AppWindow: HWND);
begin
  _AppWindow := AppWindow;

  ZeroMemory(@_waveHeader1,
             SizeOf(_waveHeader1));
  ZeroMemory(@_waveHeader2,
             SizeOf(_waveHeader2));
end;

//
//  We can "Chat" if there's more than one wave input device.
//
function CWaveChat.Initialize(const UseInputDevice: Boolean): Boolean;
begin
  if UseInputDevice = False then
    begin
      MessageBox(_AppWindow,
                 'Wave Chat can only run on the input device',
                 'Failed to initialize chat',
                 MB_OK);
      Result := false;
      Exit;
    end;

  if waveInGetNumDevs() = 0 then
    begin
      MessageBox(0,
                 'No Capture Devices found',
                 'Failed to initialize chat',
                 MB_OK);
      Result := false;
      Exit;
    end;

  Result := true;
end;

//
//  Shut down the chat code and free all the resources.
//
procedure CWaveChat.Shutdown();
var
  mmr: MMRESULT;

begin
  if _waveHandle <> 0 then
    begin
      mmr := waveInStop(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to start',
                   'Error',
                   MB_OK);

      mmr := waveInReset(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to reset',
                   'Error',
                   MB_OK);

      mmr := waveInUnprepareHeader(_waveHandle,
                                   @_waveHeader1,
                                   SizeOf(_waveHeader1));

      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to unprepare wave header 1',
                   'Error',
                   MB_OK);

      Delete(_waveBuffer1, 0, Length(_waveBuffer1));

      mmr := waveInUnprepareHeader(_waveHandle,
                                   @_waveHeader2,
                                   sizeof(_waveHeader2));

      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to unprepare wave header 2',
                   'Error',
                   MB_OK);

      Delete(_waveBuffer2, 0, Length(_waveBuffer1));

      mmr := waveInClose(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to close wave handle',
                   'Error',
                   MB_OK);

      _waveHandle := 0;
    end;
end;

destructor CWaveChat.Destroy();
begin
  inherited Destroy;
end;

//
//  Start the "Chat" - open the wave in device, prepare two capture buffers for reading.
//
function CWaveChat.StartChat(const HideFromVolumeMixer: Boolean): Boolean;
var
  mmr: MMRESULT;
  waveFormat: WAVEFORMATEX;

begin

  waveFormat.cbSize := 0;
  waveFormat.nSamplesPerSec := 44100;
  waveFormat.nChannels := 2;
  waveFormat.wBitsPerSample := 16;
  waveFormat.nBlockAlign := Round((waveFormat.wBitsPerSample / 8) * waveFormat.nChannels);
  waveFormat.nAvgBytesPerSec := waveFormat.nSamplesPerSec * waveFormat.nBlockAlign;
  waveFormat.wFormatTag := WAVE_FORMAT_PCM;

  mmr := waveInOpen(@_waveHandle,
                    WAVE_MAPPER,
                    @waveFormat,
                    DWORD_PTR(@MessageHandler),
                    0,
                    CALLBACK_FUNCTION or WAVE_MAPPED_DEFAULT_COMMUNICATION_DEVICE); // we don't send messages to a window or dialog, but to this unit.

  if (mmr <> MMSYSERR_NOERROR) then
    begin
      MessageBox(_AppWindow,
                 'Failed to open wave in',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  ZeroMemory(@_waveHeader1,
             SizeOf(_waveHeader1));

  SetLength(_waveBuffer1,
            waveFormat.nAvgBytesPerSec);

  if Not Assigned(_waveBuffer1) then
    begin
      MessageBox(_AppWindow,
                 'Failed to allocate buffer for header 1',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  _waveHeader1.dwBufferLength := waveFormat.nAvgBytesPerSec;
  _waveHeader1.lpData := Pointer(_waveBuffer1);


  mmr := waveInPrepareHeader(_waveHandle,
                             @_waveHeader1,
                             SizeOf(WAVEHDR));
  if mmr <> MMSYSERR_NOERROR then
    begin
      MessageBox(_AppWindow,
                 'Failed to prepare header 1',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  mmr := waveInAddBuffer(_waveHandle,
                         @_waveHeader1,
                         SizeOf(WAVEHDR));
  if mmr <> MMSYSERR_NOERROR then
    begin
      MessageBox(_AppWindow,
                 'Failed to add buffer 1',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  ZeroMemory(@_waveHeader2,
             SizeOf(_waveHeader2));

  SetLength (_waveBuffer2,
             waveFormat.nAvgBytesPerSec);

  if Not Assigned(_waveBuffer2) then
    begin
      MessageBox(_AppWindow,
                 'Failed to allocate buffer for header 2',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  _waveHeader2.dwBufferLength := waveFormat.nAvgBytesPerSec;
  _waveHeader2.lpData := PAnsiChar(_waveBuffer2);

  mmr := waveInPrepareHeader(_waveHandle,
                             @_waveHeader2,
                             SizeOf(WAVEHDR));
  if mmr <> MMSYSERR_NOERROR then
    begin
      MessageBox(0,
                 'Failed to prepare header 2',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  mmr := waveInAddBuffer(_waveHandle,
                         @_waveHeader2,
                         SizeOf(WAVEHDR));
  if mmr <> MMSYSERR_NOERROR then
    begin
      MessageBox(_AppWindow,
                 'Failed to add buffer 2',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  mmr := waveInStart(_waveHandle);
  if mmr <> MMSYSERR_NOERROR then
    begin
      MessageBox(_AppWindow,
                 'Failed to start',
                 'Error',
                 MB_OK);
      Result := False;
      Exit;
    end;

  Result := True;
end;

//
//  Stop the "Chat" - Stop the wave in device, reset it (which removes the pending capture buffers), unprepare the headers, and free the buffers.
//
procedure CWaveChat.StopChat();
var
  mmr: MMRESULT;

begin
  if _waveHandle <> 0 then
    begin

     mmr := waveInStop(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to start',
                   'Error',
                   MB_OK);

      mmr := waveInReset(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to reset',
                   'Error',
                   MB_OK);

      mmr := waveInUnprepareHeader(_waveHandle,
                                   @_waveHeader1,
                                   SizeOf(_waveHeader1));
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to unprepare wave header 1',
                   'Error',
                   MB_OK);

      Delete(_waveBuffer1, 0, Length(_waveBuffer1));

      mmr := waveInUnprepareHeader(_waveHandle,
                                   @_waveHeader2,
                                   SizeOf(_waveHeader2));
      if mmr <> MMSYSERR_NOERROR then
          MessageBox(0,
                     'Failed to unprepare wave header 2',
                     'Error',
                     MB_OK);

      Delete(_waveBuffer2, 0, Length(_waveBuffer1));

      mmr := waveInClose(_waveHandle);
      if mmr <> MMSYSERR_NOERROR then
        MessageBox(0,
                   'Failed to close wave handle',
                   'Error',
                   MB_OK);

      _waveHandle := 0;
    end;
end;

function CWaveChat.CanStartChat(): Boolean;
begin
  Result := True;
end;

function CWaveChat.TransportType(): ChatTransportType;
begin
  Result := ChatTransportWave;
end;

end.
