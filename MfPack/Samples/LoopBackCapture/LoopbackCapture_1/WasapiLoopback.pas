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
// Revision Version: 3.1.5
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
// Related projects: MfPack/Samples/LoopbackCapture
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
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  Utils;

const
  // REFERENCE_TIME time units per second and per millisecond
  REFTIMES_PER_SEC                    = 10000000;
  REFTIMES_PER_MILLISEC               = 10000;   // 1 millisecond = 10,000 100-nanosecond units.
  WM_BUSYNOTIFY                       = WM_USER + 1001;
  WM_PROGRESSNOTIFY                   = WM_USER + 1002;
  WM_CAPTURINGSTOPPED                 = WM_USER + 1011;

type

  TAudioSink = class(TObject)
  protected
    hmFile: HMMIO;

  private
    bStopRec: Boolean;
    bAppIsClosing: Boolean;
    hwOwner: HWND; // The handle of the caller (usually the mainform).

    function CopyData(pData: PByte;
                      NumFrames: UINT32;
                      pwfx: PWAVEFORMATEX;
                      HnsLatency: REFERENCE_TIME): HResult;

    function WriteWaveHeader(ppwfx: PWAVEFORMATEX;
                             var pckRIFF: MMCKINFO;
                             var pckData: MMCKINFO): UINT;

    function FinishWaveFile(var pckRIFF: MMCKINFO;
                            var pckData: MMCKINFO): UINT;

    function OpenFile(ppfileName: LPWSTR): HResult;

  public

    constructor Create(hwEvents: HWND); reintroduce;
    destructor Destroy(); override;

    function RecordAudioStream(dataFlow: EDataFlow;  // eRender or eCapture
                               role: ERole;          // eConsole, eMultimedia or eCommunications
                               bufferDuration: REFERENCE_TIME;
                               ppfileName: LPWSTR): HResult;

    property StopRecording: Boolean read bStopRec write bStopRec;
  end;



implementation

// TAudioSink //////////////////////////////////////////////////////////////////

constructor TAudioSink.Create(hwEvents: HWND);
begin
  inherited Create();

  // Check if the current MF version match user's
  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 LPCWSTR('Your computer does not support this Media Foundation API version' +
                          IntToStr(MF_VERSION) + '.'),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;

  hwOwner := hwEvents;
  bAppIsClosing := False;

end;


destructor TAudioSink.Destroy();
begin
  bAppIsClosing := True;
  MFShutdown();
  inherited Destroy();
end;


function TAudioSink.CopyData(pData: PByte;
                             NumFrames: UINT32;
                             pwfx: PWAVEFORMATEX;
                             HnsLatency: REFERENCE_TIME): HResult;
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

  // Handle all other messages, like managing controls, moving window etc.
  HandleThreadMessages(GetCurrentThread());
  // Send score. Don't use PostMessage because it set priority above this thread.
  SendMessage(hwOwner,
              WM_PROGRESSNOTIFY,
              NumFrames,
              HnsLatency);

done:
  Result := hr;
end;

// /////////////////////////////////////////////////////////////////////////////

function TAudioSink.WriteWaveHeader(ppwfx: PWAVEFORMATEX;
                                    var pckRIFF: MMCKINFO;
                                    var pckData: MMCKINFO): UINT;
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
  iBytesInWfx := SizeOf(WAVEFORMATEX) + ppwfx.cbSize;
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(ppwfx),
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


function TAudioSink.FinishWaveFile(var pckRIFF: MMCKINFO;
                                   var pckData: MMCKINFO): UINT;
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
function TAudioSink.RecordAudioStream(dataFlow: EDataFlow;  // eRender or eCapture
                                      role: ERole;          // eConsole, eMultimedia or eCommunications
                                      bufferDuration: REFERENCE_TIME;
                                      ppfileName: LPWSTR): HResult;
var
  hr: HResult;
  mr: MMResult;
  hnsDefaultDevicePeriod: REFERENCE_TIME;
  hnsMinimumDevicePeriod: REFERENCE_TIME;
  pHnsLatency: REFERENCE_TIME;
  hnsActualDuration: REFERENCE_TIME;
  bufferFrameCount: UINT32;
  numFramesAvailable: UINT32;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  pAudioClient: IAudioClient3;
  pCaptureClient: IAudioCaptureClient;
  packetLength: UINT32;
  pData: PByte;
  flags: DWORD;
  ckRIFF: MMCKINFO;
  ckData: MMCKINFO;
  ppwfx: PWAVEFORMATEX;
  //
  cycle: Int64;
  pu64DevicePosition: UINT64;
  pu64QPCPosition: UINT64;
  //

label
  done;

begin
  bStopRec := False;
  pu64DevicePosition := 0;
  pu64QPCPosition := 0;
  ppwfx := nil;

  // Create the initial audio file
  hr := OpenFile(ppFileName);
  if FAILED(hr) then
    goto done;

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
  hr := pEnumerator.GetDefaultAudioEndpoint(dataFlow, // eRender or eCapture,
                                            role,     // eMultimedia, eConsole or eCommunications
                                            pDevice);
  if FAILED(hr) then
    goto done;

  hr := pDevice.Activate(IID_IAudioClient,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pAudioClient));
  if FAILED(hr) then
    goto done;

  // Get the mixformat from the WAS
  // See the comments on IAudioClient.GetMixFormat.
  //
  hr := pAudioClient.GetMixFormat(ppwfx);
  if FAILED(hr) then
    goto done;


  // The original sample creates a "gues what" bufferDuration,
  // that will cause sound disturbtion when capture sound from a streameservice like YouTube or other high latency services.
  // To prevent this, we use as a minimum the value of hnsDefaultDevicePeriod.
  hr := pAudioClient.GetDevicePeriod(@hnsDefaultDevicePeriod,
                                     @hnsMinimumDevicePeriod);
  if FAILED(hr) then
    goto done;

  // Don't go below this, because you will get a lot of hick-ups.
  if (bufferDuration < hnsDefaultDevicePeriod) then
    bufferDuration := hnsDefaultDevicePeriod;

  // Initialize low-latency client.
  hr := pAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                AUDCLNT_STREAMFLAGS_LOOPBACK,
                                bufferDuration,
                                0, // Must be zero when using shared mode!
                                ppwfx,
                                @GUID_NULL);
  if FAILED(hr) then
    goto done;

  // Get the size of the allocated buffer.
  // 480 frames per ms at 48000 samples/sec
  // 480 with 1 ms buffer
  // 960 with 2 ms buffer
  // 1440 with 3 ms buffer
  // 1920 with 4 ms buffer etc.
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

  // Get the stream latency (normally this should be 0)
  hr := pAudioClient.GetStreamLatency(pHnsLatency);
  if FAILED(hr) then
    goto done;

  // Each loop fills about half of the shared buffer.
  while (bStopRec = FALSE) do
    begin
      // Sleep for half the buffer duration.
     // Sleep(hnsActualDuration div REFTIMES_PER_MILLISEC div 2);
      HandleThreadMessages(GetCurrentThread(),
                           hnsActualDuration div REFTIMES_PER_MILLISEC div 2);

      hr := pCaptureClient.GetNextPacketSize(packetLength);
      if FAILED(hr) then
        Break;

      while (packetLength <> 0) do
        begin
          // Get the available data in the shared buffer.
          hr := pCaptureClient.GetBuffer(pData,
                                         numFramesAvailable,
                                         flags,
                                         @pu64DevicePosition,
                                         @pu64QPCPosition);
          if FAILED(hr) then
            Break;

          // Tell CopyData to write silence.
          // When a sound is detected, the app will act and process data.
          if (flags = AUDCLNT_BUFFERFLAGS_SILENT) then
            pData := nil;

          // Copy the available capture data to the audio sink.
          hr := CopyData(pData,
                         numFramesAvailable,
                         ppwfx,
                         pHnsLatency);
          if FAILED(hr) then
            Break;

          hr := pCaptureClient.ReleaseBuffer(numFramesAvailable);
          if FAILED(hr) then
            goto done;

          hr := pCaptureClient.GetNextPacketSize(packetLength);
          if FAILED(hr) then
            Break;

         // For safety on 32bit platforms we have to limit the wav size to < 4 gb or 16 hours.
         // That is (3600 * 16) seconds
         if cycle >= (3600 * 16) then
           bStopRec := True;

         Inc(cycle,
             1);
      end;
    end;

done:

  if SUCCEEDED(hr) then
    begin
      hr := pAudioClient.Stop();  // Stop recording.
        if SUCCEEDED(hr) then
      hr := FinishWaveFile(ckData,
                           ckRIFF);
      mmioClose(hmFile,
                0);
    end
  else
    begin
      mmioClose(hmFile,
                0);
      {void}DeleteFile(ppfileName);
    end;

  // Send capturing stopped.
  SendMessage(hwOwner,
              WM_CAPTURINGSTOPPED,
              hr,
              0);

  pData := nil;
  Result := hr;
end;


function TAudioSink.OpenFile(ppfileName: LPWSTR): HResult;
var
  hr: HResult;
  mi: PMMIOINFO;

begin
  hr := S_OK;

  // The mmioOpen() function is NOT deprecated as documentation says.

  // Must initialize PMMIOINFO = nil, otherwise mmioOpen wil raise a pointer error.
  mi := nil;
  hmFile := mmioOpen(ppFileName,    // some flags cause mmioOpen write to this buffer
                     mi,            // but not any that we're using
                     MMIO_WRITE or MMIO_CREATE);

  if (hmFile = 0) then
    begin
      hr := E_FAIL;
      ErrMsg(Format('mmioOpen(%s) failed. wErrorRet = %d',[WideCharToString(ppFileName) , GetLastError()]), hr);
    end;

  Result := hr;
end;


end.
