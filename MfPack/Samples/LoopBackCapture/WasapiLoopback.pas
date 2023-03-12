// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WasapiLoopback.pas
// Kind: Pascal Unit
// Release date: 12-03-2023
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description:
//   This unit contains the WASAPI loopback class.
//
// Organisation: FactoryX
// Initiator(s): maXcomX
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 12/03/2023 Tony                PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX314/Samples/CaptureLoopBack
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Rita Han / FactoryX
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
unit WasapiLoopback;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMeApi,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  Utils;

const
  // REFERENCE_TIME time units per second and per millisecond
  REFTIMES_PER_SEC                    = 10000000;
  REFTIMES_PER_MILLISEC               = 10000;
  WM_BUSYNOTIFY                       = WM_USER + 1001;
  WM_PROGRESSNOTIFY                   = WM_USER + 1002;
  WM_STOPREQUEST                      = WM_USER + 1010;
  WM_CAPTURINGSTOPPED                 = WM_USER + 1011;
type

  TAudioSink = class(TObject)
  protected
    hThreadId: TThreadID;
    hmFile: HMMIO;
  private
    bStopRec: Boolean;
    bAppIsClosing: Boolean;
    pFileName: LPWSTR;
    hwOwner: HWND; // The handle of the caller.

    procedure WndProc(var Msg: TMessage);

    function CopyData(pData: PByte;
                      NumFrames: UINT32;
                      pwfx: PWAVEFORMATEX): HResult;

    function WriteWaveHeader(pwfx: PWAVEFORMATEX;
                             pckRIFF: MMCKINFO;
                             pckData: MMCKINFO): UINT;

    function FinishWaveFile(pckRIFF: MMCKINFO;
                            pckData: MMCKINFO): UINT;

    function OpenFile(): HResult;

  public
    hwHWND: HWND; // the handle of this object.

    constructor Create(hwEvents: HWND); reintroduce;
    destructor Destroy(); override;

    function RecordAudioStream(pAudioSink: TAudioSink;
                               ppfileName: LPWSTR): HResult;

  end;



implementation

// TAudioSink //////////////////////////////////////////////////////////////////

procedure TAudioSink.WndProc(var Msg: TMessage);
begin
  // prevent processing messages when app is shutting down.
  if bAppIsClosing then
    Exit;

  if (Msg.Msg = WM_STOPREQUEST) then // Check for timer messages
    try
      bStopRec := Boolean(Msg.WParam);
    except
      //Application.HandleException(Self);
      // Do nothing
    end
  // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
  // NOTE: The first parameter, hwHWND, is the handle of the window receiving this message.
  //       It is obtained from the call to AllocateHWnd in the Constructor.
  else
    msg.Result := DefWindowProc(hwHWND,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);
end;


constructor TAudioSink.Create(hwEvents: HWND);
begin
  inherited Create();
  hwOwner := hwEvents;
  bAppIsClosing := False;
  hwHWND := AllocateHWnd(WndProc);
end;


destructor TAudioSink.Destroy();
begin
  bAppIsClosing := True;
  DeallocateHWnd(hwHWND);
  inherited Destroy();
end;


function TAudioSink.CopyData(pData: PByte;
                             NumFrames: UINT32;
                             pwfx: PWAVEFORMATEX): HResult;
var
  hr: HResult;
  iBytesToWrite: Integer;
  iBytesWritten: Integer;

label
  done;

begin
  hr := S_OK;

  if (NumFrames = 0) then
    begin
      ErrMsg(Format('IAudioCaptureClient.GetBuffer returned %d frames.',[0]), GetLastError());
      hr := E_UNEXPECTED;
      goto done;
    end;

  iBytesToWrite := (NumFrames * pwfx.nBlockAlign);
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(pData),
                             iBytesToWrite);
  if (iBytesToWrite <> iBytesWritten) then
    begin
      ErrMsg(Format('mmioWrite wrote %d bytes : expected %d bytes',[iBytesWritten, iBytesToWrite]), GetLastError());
      hr := E_UNEXPECTED;
      goto done;
    end;

  // Send score
  SendMessage(hwOwner,
              WM_PROGRESSNOTIFY,
              NumFrames,
              0);

  HandleThreadMessages(GetCurrentThread(), 100);
done:
  Result := hr;
end;

// /////////////////////////////////////////////////////////////////////////////

function TAudioSink.WriteWaveHeader(pwfx: PWAVEFORMATEX;
                                    pckRIFF: MMCKINFO;
                                    pckData: MMCKINFO): UINT;
var
  mResult: MMRESULT;
  mChunk: MMCKINFO;
  iBytesInWfx: Integer;
  iBytesWritten: Integer;
  dwFrames: DWORD;

begin

  // make a RIFF/WAVE chunk

  pckRIFF.ckid := MAKEFOURCC('R', 'I', 'F', 'F');
  pckRIFF.fccType := MAKEFOURCC('W', 'A', 'V', 'E');

  mResult := mmioCreateChunk(hmFile,
                             @pckRIFF,
                             MMIO_CREATERIFF);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioCreateChunk("RIFF/WAVE") failed: MMRESULT = %',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // make a 'fmt ' chunk (within the RIFF/WAVE chunk)
  mChunk.ckid := MAKEFOURCC('f', 'm', 't', ' ');
  mResult := mmioCreateChunk(hmFile,
                             @mChunk,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioCreateChunk("fmt") failed: MMRESULT = %',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // write the WAVEFORMATEX data to it
  iBytesInWfx := SizeOf(WAVEFORMATEX) + pwfx.cbSize;
  iBytesWritten :=  mmioWrite(hmFile,
                              PAnsiChar( {LPWAVEFORMATEX} pwfx),
                              iBytesInWfx);

  if (iBytesWritten <> iBytesInWfx) then
    begin
      ErrMsg(Format('mmioWrite(fmt data) wrote %d bytes; expected %d bytes',[iBytesWritten, iBytesInWfx]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // ascend from the 'fmt ' chunk
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("fmt") failed: MMRESULT = %d',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // make a 'fact' chunk whose data is (DWORD)0
  mChunk.ckid := MAKEFOURCC('f', 'a', 'c', 't');
  mResult := mmioCreateChunk(hmFile,
                             @mChunk,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioCreateChunk("fmt") failed: MMRESULT = %d',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // Write DWORD(0) to it
  // This will be cleaned up later
  dwFrames := 0;
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(@dwFrames),
                             sizeof(dwFrames));
  if (iBytesWritten <> sizeof(dwFrames)) then
    begin
      ErrMsg(Format('mmioWrite(fact data) wrote %d bytes; expected %d bytes"',[iBytesWritten, SizeOf(dwFrames)]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // ascend from the 'fact' chunk
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);

  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("fact") failed: MMRESULT = %d',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;

  // make a 'data' chunk and leave the data pointer there
  pckData.ckid := MAKEFOURCC('d', 'a', 't', 'a');

  mResult := mmioCreateChunk(hmFile,
                             @pckData,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioCreateChunk("data") failed: MMRESULT = %d',[mResult]), GetLastError());
      Result := mResult;
      Exit;
    end;
  Result := 0;
end;


function TAudioSink.FinishWaveFile(pckRIFF: MMCKINFO;
                                   pckData: MMCKINFO): UINT;
var
  mResult: MMRESULT;

label
  done;

begin

  mResult := mmioAscend(hmFile,
                        @pckData,
                        0);
  if (mResult <> MMSYSERR_NOERROR) then
    begin
      ErrMsg(Format('mmioAscend("pckData (MMCKINFO)") failed: MMRESULT = %d',[mResult]), GetLastError());
      goto done;
    end;

  mResult := mmioAscend(hmFile,
                        @pckRIFF,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("pckRIFF (MMCKINFO") failed: MMRESULT = %d',[mResult]), GetLastError());
      goto done;
    end;

done:
  Result := mResult;
end;


//-----------------------------------------------------------
// Record an audio stream from the default audio capture
// device. The RecordAudioStream function allocates a shared
// buffer big enough to hold one second of PCM audio data.
// The function uses this buffer to stream data from the
// capture device. The main loop runs every 1/2 second.
//-----------------------------------------------------------
function TAudioSink.RecordAudioStream(pAudioSink: TAudioSink;
                                      ppfileName: LPWSTR): HResult;
var
  hr: HResult;
  mr: MMResult;
  hnsRequestedDuration: REFERENCE_TIME;
  hnsActualDuration: REFERENCE_TIME;
  bufferFrameCount: UINT32;
  numFramesAvailable: UINT32;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  pAudioClient: IAudioClient;
  pCaptureClient: IAudioCaptureClient;
  packetLength: UINT32;
  pData: PByte;
  flags: DWORD;
  ckRIFF: MMCKINFO;
  ckData: MMCKINFO;
  ppwfx: PWAVEFORMATEX;
  //
  cycle : Int64;

label
  done;

begin
  pFileName := ppFileName;
  // Create the initial audio file
  hr := OpenFile();
  if FAILED(hr) then
    goto done;

  hnsRequestedDuration := REFTIMES_PER_SEC;
  packetLength := 0;

  // Enumerate on capture and render devices
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);

  if FAILED(hr) then
    goto done;

  // Get the default endpoint. See MMDeviceApi line 278 for explanation.
  // Normally you wpuld check if a capture device is present, on most laptops there is none.
  hr := pEnumerator.GetDefaultAudioEndpoint(eRender,     //or eCapture,
                                            eMultimedia, //or eConsole,
                                            pDevice);
  if FAILED(hr) then
    goto done;

  hr := pDevice.Activate(IID_IAudioClient,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pAudioClient));
  if FAILED(hr) then
    goto done;

  hr := pAudioClient.GetMixFormat(ppwfx);
  if FAILED(hr) then
    goto done;

  hr := pAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                AUDCLNT_STREAMFLAGS_LOOPBACK,
                                hnsRequestedDuration,
                                0,
                                ppwfx,
                                GUID_NULL);
  { $88890003 = AUDCLNT_E_WRONG_ENDPOINT_TYPE }
  if FAILED(hr) then
    goto done;

  // Get the size of the allocated buffer.
  hr := pAudioClient.GetBufferSize(bufferFrameCount);
  if FAILED(hr) then
    goto done;

  hr := pAudioClient.GetService(IID_IAudioCaptureClient,
                                pCaptureClient);
  if FAILED(hr) then
    goto done;

  mr := WriteWaveHeader(ppwfx,
                        ckRIFF,
                        ckData);

  if (mr <> 0) then
    begin
      hr := E_FAIL;
      goto done;
    end;

  // Calculate the actual duration of the allocated buffer.
  hnsActualDuration := (REFTIMES_PER_SEC *
                        bufferFrameCount div ppwfx.nSamplesPerSec);

  hr := pAudioClient.Start();  // Start recording.
  if FAILED(hr) then
    goto done;

  cycle := 0;

  // Each loop fills about half of the shared buffer.
  while (bStopRec = FALSE) do
    begin
      // Sleep for half the buffer duration.
      Sleep(hnsActualDuration div REFTIMES_PER_MILLISEC div 2);

      hr := pCaptureClient.GetNextPacketSize(packetLength);
      if FAILED(hr) then
        goto done;

      while (packetLength <> 0) do
        begin
          // Get the available data in the shared buffer.
          hr := pCaptureClient.GetBuffer(pData,
                                         numFramesAvailable,
                                         flags,
                                         0,
                                         0);
          if FAILED(hr) then
            goto done;

          if (flags = AUDCLNT_BUFFERFLAGS_SILENT) then
            begin
              pData := nil;  // Tell CopyData to write silence.
            end;

          // Copy the available capture data to the audio sink.
          hr := pAudioSink.CopyData(pData,
                                    numFramesAvailable,
                                    ppwfx);
          if FAILED(hr) then
            goto done;

          hr := pCaptureClient.ReleaseBuffer(numFramesAvailable);
          if FAILED(hr) then
            goto done;

          hr := pCaptureClient.GetNextPacketSize(packetLength);
          if FAILED(hr) then
            goto done;

         // For safety on 32bit platforms we have to limit the wav size to < 4 gb or 16 hours.
         // That is (3600 * 16) seconds
         if cycle >= (3600 * 16) then
           bStopRec := True;

          inc(cycle, 1);
        end;
    end;

  hr := pAudioClient.Stop();  // Stop recording.
  if Failed(hr) then
    goto done;

  hr := FinishWaveFile(ckData,
                       ckRIFF);

done:
   mmioClose(hmFile,
                0);

  // Send capturing stopped.
  SendMessage(hwOwner,
              WM_CAPTURINGSTOPPED,
              hr,
              0);

  pData := nil;
  Result := hr;
end;


function TAudioSink.OpenFile(): HResult;
var
  hr: HResult;
  mi: PMMIOINFO;

begin
  hr := S_OK;
  // Must set PMMIOINFO to nil otherwise mmioOpen wil raise a pointer error.
  mi := nil;
  hmFile := mmioOpen(pFileName,     // some flags cause mmioOpen write to this buffer
                     mi,            // but not any that we're using
                     MMIO_WRITE or MMIO_CREATE);

  if (hmFile = 0) then
    begin
      hr := E_FAIL;
      ErrMsg(Format('CreateFile2(%s) failed. wErrorRet = %d',[WideCharToString(pFileName) , GetLastError()]), hr);
    end;

  Result := hr;
end;


end.
