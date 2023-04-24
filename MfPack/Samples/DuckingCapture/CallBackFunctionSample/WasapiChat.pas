// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WasapiChat.pas
// Kind: Pascal / Delphi unit
// Release date: 04-10-2020
// Language: ENU
//
// Revision Version: 3.1.4
// Description: WasApi threaded capture class.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Note that this sample requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: DuckingCaptureSample: wasapichat.h, wasapichat.ccp
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
unit WasapiChat;

interface

uses
  {Winapi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMSysCom,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceAPI,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  ChatTransport;

type
  PWasapiChat = ^CWasapiChat;
  CWasapiChat = class(CChatTransport)
  private

    hwndWasApiChat: HWND;
    _ChatEndpoint: IMMDevice;
    _ChatThread: THandle;
    _ShutdownEvent: THandle;
    _AudioSamplesReadyEvent: THandle;

  public

    _Flow: EDataFlow;
    _AudioClient: IAudioClient;
    _RenderClient: IAudioRenderClient;
    _CaptureClient: IAudioCaptureClient;

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



// private thread function (see TThread) ///////////////////////////////////////
function WasapiChatThread({Context} Parameter: Pointer): Integer;
var
  hr: HResult;
  chat: PWasapiChat; // Note: We don't use the global (unit) scoped threadvar!
  waitArray: array of THandle;  // We don't use a static array
  waitResult: DWord;
  stillPlaying: Boolean;
  pData: PByte;
  framesAvailable: UINT32;
  flags: AUDCLNT_BUFFERFLAGS;
  flow: eDataFlow;
  iAClient: IAudioClient;
  iRClient: IAudioRenderClient;
  iCClient: IAudioCaptureClient;

begin

{$HINTS OFF}  // hr is only used for debugging, see comments below.

  // Set initial
  Result := 0;
  chat := Parameter;
  stillPlaying := True;
  // to keep the enigine run smoothly, we localise the global vars.
  flow := chat._Flow;
  iRClient := chat._RenderClient;
  iCClient := chat._CaptureClient;
  iAClient := chat._AudioClient;

  SetLength(waitArray, 2);
  waitArray[0] := chat._ShutdownEvent;
  waitArray[1] := chat._AudioSamplesReadyEvent;

  while stillPlaying do
    begin

      waitResult := WaitForMultipleObjects(2,
                                           Pointer(waitArray),   // Do NOT forget to clear the array after using it!
                                           False,
                                           INFINITE);

      case waitResult of
        WAIT_OBJECT_0 + 0: begin
                             stillPlaying := False;       // We're done, exit the loop.
                             // or use: Break;
                           end;

        WAIT_OBJECT_0 + 1: begin
                             //
                             //  Either stream silence to the audio client or ignore the audio samples.
                             //
                             //  Note that we don't check for errors here. This is because
                             //      (a) there's no way of reporting the failure
                             //      (b) once the streaming engine has started there's really no way for it to fail.
                             //
                             if flow = eRender then
                               begin
                                 hr := iAClient.GetCurrentPadding(framesAvailable);
                                 hr := iRClient.GetBuffer(framesAvailable,
                                                          pData);
                                 hr := iRClient.ReleaseBuffer(framesAvailable,
                                                              AUDCLNT_BUFFERFLAGS_SILENT);
                               end
                             else
                               begin
                                 hr := iAClient.GetCurrentPadding(framesAvailable);
                                 hr := iCClient.GetBuffer(pData,
                                                          framesAvailable,
                                                          flags,
                                                          0,
                                                          0);
                                 hr := iCClient.ReleaseBuffer(framesAvailable);
                               end;
                           end;
      end;
    end;

  SetLength(waitArray, 0); // Clear the array.
  EndThread(0); // Don't forget to end the thread!

end;


// CWasapiChat /////////////////////////////////////////////////////////////////

constructor CWasapiChat.Create(AppWindow: HWND);
begin
  _AppWindow := AppWindow;
  //hwndWasApiChat := AllocateHWnd(MessageHandler);  // Not thread safe, you could use a unit as published here:
                                                   // https://stackoverflow.com/questions/8820294/how-can-i-make-allocatehwnd-threadsafe
end;

function CWasapiChat.Initialize(const UseInputDevice: Boolean): Boolean;
var
  hr: HResult;
  deviceEnumerator: IMMDeviceEnumerator;

begin

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMMDeviceEnumerator,
                         deviceEnumerator);
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to instantiate device enumerator',
                 'WASAPI Transport Initialize Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  if UseInputDevice then
    _Flow := eCapture
  else
    _Flow := eRender;

  hr := deviceEnumerator.GetDefaultAudioEndpoint(_Flow,
                                                 eCommunications,
                                                 _ChatEndpoint);

  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to retrieve default endpoint',
                 'WASAPI Transport Initialize Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

    //
    //  Create our shutdown event - we want an auto reset event that starts in the not-signaled state.
    //
    {$IF CompilerVersion > 29}
    _ShutdownEvent := CreateEventEx(Nil,
                                    Nil,
                                    0,
                                    EVENT_MODIFY_STATE or SYNCHRONIZE);
    {$ELSE}
    _ShutdownEvent := CreateEvent(Nil,
                                  False,
                                  True,
                                  LPCWSTR('ShutdownEvent'));
    {$ENDIF}

    if _ShutdownEvent = 0 then
      begin
        MessageBox(_AppWindow,
                   'Unable to create shutdown event.',
                   'WASAPI Transport Initialize Failure',
                   MB_OK);
        Result := False;
        Exit;
      end;

    {$IF CompilerVersion > 29}
    _AudioSamplesReadyEvent := CreateEventEx(Nil,
                                             Nil,
                                             0,
                                             EVENT_MODIFY_STATE or SYNCHRONIZE);

    {$ELSE}
    _AudioSamplesReadyEvent := CreateEvent(Nil,
                                           False,
                                           True,
                                           LPCWSTR('AudioSamplesReadyEvent'));
    {$ENDIF}

    if _ShutdownEvent = 0 then
      begin
        MessageBox(_AppWindow,
                   'Unable to create samples ready event.',
                   'WASAPI Transport Initialize Failure',
                   MB_OK);
        Result := False;
        Exit;
      end;

  Result := True;
end;

//
//  Shut down the chat code and free all the resources.
//
procedure CWasapiChat.Shutdown();
begin
  if _ChatThread <> 0 then
    begin
      SetEvent(_ShutdownEvent);
      WaitForSingleObject(_ChatThread,
                          INFINITE);
      CloseHandle(_ChatThread);
      _ChatThread := 0;
    end;

  if _ShutdownEvent <> 0 then
    begin
      CloseHandle(_ShutdownEvent);
      _ShutdownEvent := 0;
    end;

  if _AudioSamplesReadyEvent <> 0 then
    begin
      CloseHandle(_AudioSamplesReadyEvent);
      _AudioSamplesReadyEvent := 0;
    end;

  SafeRelease(_ChatEndpoint);
  SafeRelease(_AudioClient);
  SafeRelease(_RenderClient);
  SafeRelease(_CaptureClient);
  DeAllocateHWnd(hwndWasApiChat);
end;

destructor CWasapiChat.Destroy();
begin
  inherited Destroy;
end;

//
//  Start the "Chat" - open the capture device, start capturing.
//
function CWasapiChat.StartChat(const HideFromVolumeMixer: Boolean): Boolean;
var
  hr: HResult;
  mixFormat: PWAVEFORMATEX;
  chatGuid: TGUID;
  pData: PByte;
  framesAvailable: UINT32;
  HideFromMixer: DWord;
  {We use some modifucations the Delphi way}
  hThreadId: TThreadID;

begin
  Result := True;
  hThreadId := 0;
  PData := Nil;

  hr := _ChatEndpoint.Activate(IID_IAudioClient,
                               CLSCTX_INPROC_SERVER,
                               Nil,
                               Pointer(_AudioClient));
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to activate audio client.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  hr := _AudioClient.GetMixFormat(mixFormat);
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to get mix format on audio client.',
                 'ASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  //
  //  Initialize the chat transport - Initialize WASAPI in event driven mode, associate the audio client with
  //  our samples ready event handle, retrieve a capture/render client for the transport, create the chat thread
  //  and start the audio engine.
  //
  hr := CoCreateGuid(chatGuid);
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to create GUID.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  if HideFromVolumeMixer then
    HideFromMixer := AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE or
                     AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                     AUDCLNT_STREAMFLAGS_NOPERSIST
  else
    HideFromMixer := AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                     AUDCLNT_STREAMFLAGS_NOPERSIST;

  hr := _AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                HideFromMixer,
                                500000,
                                0,
                                mixFormat,
                                chatGuid);

  CoTaskMemFree(@mixFormat);
  mixFormat := Nil;

  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to initialize audio client.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  hr := _AudioClient.SetEventHandle(_AudioSamplesReadyEvent);
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to set ready event.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  if (_Flow = eRender) then
    hr := _AudioClient.GetService(IID_IAudioRenderClient,
                                  _RenderClient)
  else
    hr := _AudioClient.GetService(IID_IAudioCaptureClient,
                                  _CaptureClient);

  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to get Capture/Render client.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  //
  //  Now create the thread which is going to drive the "Chat".
  //
  _ChatThread := BeginThread(Nil,
                             0,
                             Addr(WasapiChatThread),
                             Addr(Self),
                             0,
                             hThreadId);

  if _ChatThread = 0 then
    begin
      MessageBox(_AppWindow,
                 'Unable to create transport thread.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      Result := False;
      Exit;
    end;

  //
  //  For render, we want to pre-roll a frames worth of silence into the pipeline.  That way the audio engine won't glitch on startup.
  //
  if _Flow = eRender then
    begin
      hr := _AudioClient.GetBufferSize(framesAvailable);
      if FAILED(hr) then
        begin
          MessageBox(_AppWindow,
                     'Failed to get client buffer size.',
                     'WASAPI Transport Start Failure',
                     MB_OK);
          CloseHandle(_ChatThread); // close the thread.
          Result := False;
          Exit;
        end;

      hr := _RenderClient.GetBuffer(framesAvailable,
                                    pData);
      if FAILED(hr) then
        begin
          MessageBox(_AppWindow,
                     'Failed to get buffer.',
                     'WASAPI Transport Start Failure',
                     MB_OK);
          CloseHandle(_ChatThread); // close the thread.
          Result := False;
          Exit;
        end;

      hr := _RenderClient.ReleaseBuffer(framesAvailable,
                                        AUDCLNT_BUFFERFLAGS_SILENT);
      if FAILED(hr) then
        begin
          MessageBox(_AppWindow,
                     'Failed to release buffer.',
                     'WASAPI Transport Start Failure',
                     MB_OK);
          CloseHandle(_ChatThread); // close the thread.
          Result := False;
          Exit;
        end;
    end;

  //
  //  We're ready to go, start the chat!
  //
  hr := _AudioClient.Start();
  if FAILED(hr) then
    begin
      MessageBox(_AppWindow,
                 'Unable to start chat client.',
                 'WASAPI Transport Start Failure',
                 MB_OK);
      CloseHandle(_ChatThread); // close the thread.
      Result := False;
      Exit;
    end;

end;

//
//  Stop the "Chat" - Stop the capture thread and release the buffers.
//
procedure CWasapiChat.StopChat();
begin
  //
  //  Tell the chat thread to shut down, wait for the thread to complete then clean up all the stuff we
  //  allocated in StartChat().
  //
  if _ShutdownEvent > 0 then
    SetEvent(_ShutdownEvent);

  if _ChatThread > 0 then
    begin
      WaitForSingleObject(_ChatThread,
                          INFINITE);

      CloseHandle(_ChatThread);
      _ChatThread := 0;
    end;

  SafeRelease(_RenderClient);
  SafeRelease(_CaptureClient);
  SafeRelease(_AudioClient);
end;

function CWasapiChat.CanStartChat(): Boolean;
begin
  Result := True;
end;

function CWasapiChat.TransportType(): ChatTransportType;
begin
  Result := ChatTransportWasapi;
end;

end.
