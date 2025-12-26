// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  WasapiLoopbackCapture.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.8
// Description: Loopback capturer.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/11/2025 Tony                Ozzy Osbourne release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX318
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit WasapiLoopbackCapture;

interface

uses
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinError,
  WinApi.ksmedia,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.WinMM.MMeApi,
  Helpers;

type

  TWasapiDataEvent = procedure(Sender: TObject;
                               const Buffer: Pointer;
                               NumFrames: Cardinal;
                               const WaveFormat: PWAVEFORMATEX) of object;


  TWasapiLoopbackCapture = class
  private

    FOnData: TWasapiDataEvent;

    FThread: TThread;
    FStopEvent: TEvent;

    FDevice: IMMDevice;
    FAudioClient: IAudioClient;
    FCaptureClient: IAudioCaptureClient;
    FWaveFormat: PWAVEFORMATEX;
    FEventHandle: THandle;

    // Device
    FAudioDeviceId : string;

    procedure CaptureThreadProc;
    procedure InitClient;
    procedure FreeClient;

  public

    constructor Create(const aDeviceId: string = '');
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property AudioDeviceId: string read FAudioDeviceId write FAudioDeviceId;
    property OnData: TWasapiDataEvent read FOnData write FOnData;
  end;


implementation


uses
  Vcl.Dialogs;

constructor TWasapiLoopbackCapture.Create(const aDeviceId: string);
begin

  inherited Create;

  FAudioDeviceId := ADeviceId;
  FStopEvent := TEvent.Create(nil, True, False, '');
  FEventHandle := 0;
  FWaveFormat := nil;
end;


destructor TWasapiLoopbackCapture.Destroy;
begin

  Stop;
  FreeClient;
  FreeAndNil(FStopEvent);

  inherited;
end;


procedure TWasapiLoopbackCapture.InitClient;
var
  hr: HRESULT;
  enum: IMMDeviceEnumerator;
  //taskName: string;
  bufferDur: Int64;

begin

  // Default render device (loopback)
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         enum);
  CheckHR(hr, 'CoCreateInstance(IMMDeviceEnumerator)');

  // Select the audio device to capture from.
  if (FAudioDeviceId = '') then
    begin
      hr := enum.GetDefaultAudioEndpoint(eRender,
                                         eMultimedia,
                                         FDevice);
      CheckHR(hr, 'GetDefaultAudioEndpoint');
    end
  else
    begin
      hr := enum.GetDevice(PWideChar(FAudioDeviceId),
                           FDevice);
      CheckHR(hr, 'IMMDeviceEnumerator.GetDevice')
    end;

  hr := FDevice.Activate(IID_IAudioClient,
                         CLSCTX_INPROC_SERVER,
                         nil,
                         Pointer(FAudioClient));
  CheckHR(hr, 'IMMDevice.Activate(IAudioClient)');

  hr := FAudioClient.GetMixFormat(FWaveFormat);
  CheckHR(hr, 'IAudioClient.GetMixFormat');

  // 100ms buffer
  bufferDur := 1000000; // 100ms in 100ns units

  // Event-driven loopback
  FEventHandle := CreateEvent(nil, False, False, nil);
  if FEventHandle = 0 then
    RaiseLastOSError;

  hr := FAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                AUDCLNT_STREAMFLAGS_LOOPBACK or AUDCLNT_STREAMFLAGS_EVENTCALLBACK,
                                bufferDur,
                                0,
                                FWaveFormat,
                                nil);
  CheckHR(hr, 'IAudioClient.Initialize');

  hr := FAudioClient.SetEventHandle(FEventHandle);
  CheckHR(hr, 'IAudioClient.SetEventHandle');

  hr := FAudioClient.GetService(IID_IAudioCaptureClient,
                                FCaptureClient);
  CheckHR(hr, 'IAudioClient.GetService(IAudioCaptureClient)');
end;


procedure TWasapiLoopbackCapture.FreeClient;
var
  hr: HResult;

begin

  if Assigned(FAudioClient) then
    begin

      hr := FAudioClient.Stop();
      if FAILED(hr) then
        ShowMessage('Error: FAudioClient.Stop');
    end;

  FCaptureClient := nil;
  FAudioClient := nil;
  FDevice := nil;

  if Assigned(FWaveFormat) then
    begin
      CoTaskMemFree(FWaveFormat);
      FWaveFormat := nil;
    end;

  if (FEventHandle <> 0) then
    begin
      CloseHandle(FEventHandle);
      FEventHandle := 0;
    end;
end;


procedure TWasapiLoopbackCapture.Start;
begin
  if Assigned(FThread) then
    Exit;

  FStopEvent.ResetEvent();

  FThread := TThread.CreateAnonymousThread(procedure
                                             begin
                                               CaptureThreadProc;
                                             end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;


procedure TWasapiLoopbackCapture.Stop();
begin

  if not Assigned(FThread) then
    Exit;

  FStopEvent.SetEvent();
  FThread.WaitFor();
  FreeAndNil(FThread);
end;


procedure TWasapiLoopbackCapture.CaptureThreadProc();
var
  hr: HRESULT;
  packetFrames: UINT32;
  pData: PByte;
  numFrames: UINT32;
  flags: DWORD;
  waitRes: DWORD;
  silentBuf: TBytes;
  needBytes: Integer;

begin

  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);
  try

    InitClient();

    hr := FAudioClient.Start();
    CheckHR(hr, 'IAudioClient.Start');

    while (FStopEvent.WaitFor(0) = wrTimeout) do
      begin

        waitRes := WaitForSingleObject(FEventHandle,
                                       50);
        if (waitRes = WAIT_TIMEOUT) then
          Continue;

        // Drain all available packets
        hr := FCaptureClient.GetNextPacketSize(packetFrames);
        CheckHR(hr, 'IAudioCaptureClient.GetNextPacketSize');

        while (packetFrames > 0) do
          begin

            pData := nil;
            numFrames := 0;
            flags := 0;

            hr := FCaptureClient.GetBuffer(pData,
                                           numFrames,
                                           flags,
                                           nil,
                                           nil);
            CheckHR(hr, 'IAudioCaptureClient.GetBuffer');

            try
              if Assigned(FOnData) and (numFrames > 0) then
                begin
                  // Write silence.
                  if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                    begin

                      needBytes := Integer(numFrames) * FWaveFormat.nBlockAlign;
                      if (Length(silentBuf) <> needBytes) then
                        SetLength(silentBuf,
                                  needBytes);

                      FillChar(silentBuf[0],
                               needBytes,
                               0);

                      FOnData(Self,
                              @silentBuf[0],
                              numFrames,
                              FWaveFormat);
                    end
                  else
                    FOnData(Self,
                            pData,
                            numFrames,
                            FWaveFormat);
                end;
            finally

              hr := FCaptureClient.ReleaseBuffer(numFrames);
              CheckHR(hr, 'IAudioCaptureClient.ReleaseBuffer');
            end;

        hr := FCaptureClient.GetNextPacketSize(packetFrames);
        CheckHR(hr, 'IAudioCaptureClient.GetNextPacketSize');
      end;
    end;

    try

      FAudioClient.Stop();
    except
      // Do nothing.
    end;

    FreeClient();
  finally

    CoUninitialize();
  end;
end;

end.
