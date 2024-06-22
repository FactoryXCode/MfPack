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
// Revision Version: 3.1.7
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
// 19/06/2024 All                 Rammstein release  SDK 10.0.22621.0 (Windows 11)
// 25/04/2004 Tony                Updated to a more stable and crack free version.
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
// Source: Rita Han / Tony Kalf / FactoryX
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
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {WinMM}
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
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
  MAX_FILE_SIZE = 4294960000; // 4294967296 - 7296 keep some overhead.

type

  TRenderThread = class;

  TAudioSink = class(TObject)
  protected
    hmFile: HMMIO;
    pvRenderThread: TRenderThread;

  private

    pvAudioClient: IAudioClient;
    pvCaptureClient: IAudioCaptureClient;


    pvStopRec: Boolean;
    pvAppIsClosing: Boolean;
    pvErrorStatus: HResult;

    pvHnsLatency: REFERENCE_TIME;
    pvBufferHns: REFERENCE_TIME; // Buffer period in milliseconds.
    pvBytesWritten: Int64;
    pvBufferFrames: UInt32;
    pvWavFormatEx: WAVEFORMATEX;
    pvPwfx: PWAVEFORMATEX;


    pvCkRIFF: MMCKINFO;
    pvCkData: MMCKINFO;

    pvHnsActualDuration: REFERENCE_TIME;

    FOnCapturingStart: TNotifyEvent;
    FOnCapturingStopped: TNotifyEvent;
    pvRenderThreadClosedEvent: THandle;

    function CopyData(pData: PByte;
                      NumFrames: UINT32;
                      pwfx: PWAVEFORMATEX): HResult;

    function OpenFile(ppfileName: LPWSTR): HResult;

    function WriteWaveHeader(ppwfx: PWAVEFORMATEX;
                             var pckRIFF: MMCKINFO;
                             var pckData: MMCKINFO): UINT;

    function FinishWaveFile(var pckRIFF: MMCKINFO;
                            var pckData: MMCKINFO): UINT;

    // Here the rendering takes place in a thread.
    function RenderData(): HResult;

    //
    // Thread methods.
    //
    procedure CreatedRenderThread();
    procedure TerminateRenderThread();

  public

    constructor Create();
    destructor Destroy(); override;

    function RecordAudioStream(dataFlow: EDataFlow;  // eRender or eCapture
                               role: ERole;          // eConsole, eMultimedia or eCommunications
                               bufferDuration: REFERENCE_TIME;
                               ppfileName: LPWSTR): HResult;

    procedure Stop();

    // Used by OnProcessingData
    property Latency: REFERENCE_TIME read pvHnsLatency;
    property BufferDuration: REFERENCE_TIME read pvBufferHns;
    property BytesWritten: Int64 read pvBytesWritten;
    property CaptureBufferLength: UInt32 read pvBufferFrames;
    property CurrentWavFormat: WAVEFORMATEX read pvWavFormatEx;
    property ErrorStatus: HResult read pvErrorStatus;
    // Stops recording immediately.
    property StopRecording: Boolean read pvStopRec write pvStopRec;

    // Notify events.
    property OnStartCapturing: TNotifyEvent read FOnCapturingStart write FOnCapturingStart;
    property OnStoppedCapturing: TNotifyEvent read FOnCapturingStopped write FOnCapturingStopped;
  end;

  // This event type is used to pass back a HResult.
  TCallbackEvent = procedure(Sender: TObject;
                             const Hres: HRESULT) of object;

  // The thread we render after Start.
  TRenderThread = class(TThread)
  protected

    procedure Execute; override;
    procedure SetEvent;

  private

    FEngine: TAudioSink;
    FSuccess: HResult; // Used internally when synchronizing the HRESULT for handling.
    FOnEvent: TCallbackEvent;

  public

    constructor Create(AEngine: TAudioSink);
    destructor Destroy; override;

    property OnEvent: TCallbackEvent read FOnEvent write FOnEvent; // Triggered when a status changed.
  end;


implementation


// Thread ======================================================================
constructor TRenderThread.Create(AEngine: TAudioSink);
begin
  inherited Create(True);
  FEngine := AEngine;
  // Set termination when the thread is done.
  FreeOnTerminate := False;

end;


destructor TRenderThread.Destroy();
begin

  inherited;
end;


procedure TRenderThread.Execute;
begin
  // Initialize COM concurrency model multithreaded.
  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);
  // The function where we render the audio data and
  // (de)activate the MMCSS feature.
  // See: https://learn.microsoft.com/en-us/windows/win32/procthread/multimedia-class-scheduler-service
  // To get the best performance, it's recomended to set "Best Performance" in Windows energy settings.
  FSuccess := FEngine.RenderData();
  Synchronize(SetEvent);
  CoUnInitialize;
end;


procedure TRenderThread.SetEvent;
begin
  // Called from 'Synchronize'.
  // All code run from "Synchronize()"
  //   runs in the context of the Main VCL UI Thread, NOT from this thread.
  //   This simply triggers the event which was assigned by the calling thread
  //   to inform it that the download has completed.

  if Assigned(FOnEvent) then
    FOnEvent(Self,
             FSuccess);
end;

// =============================================================================
// Thread control methods ======================================================

procedure TAudioSink.CreatedRenderThread;
begin
  TerminateRenderThread();
  pvRenderThread := TRenderThread.Create(Self);
  pvRenderThreadClosedEvent := CreateEventEx(nil,
                                             nil,
                                             0,
                                             EVENT_MODIFY_STATE or SYNCHRONIZE);

  pvRenderThread.Start;
end;


procedure TAudioSink.TerminateRenderThread();
begin

  if Assigned(pvRenderThread) then
    begin
      pvRenderThread.Terminate;
      pvRenderThread.WaitFor;
      // Close threadhandle.
      if (pvRenderThreadClosedEvent <> 0) then
        begin
          SetEvent(pvRenderThreadClosedEvent);
          WaitForSingleObject(pvRenderThreadClosedEvent,
                              INFINITE);
          CloseHandle(pvRenderThreadClosedEvent);
          pvRenderThreadClosedEvent := 0;
        end;
      FreeAndNil(pvRenderThread);
    end;
end;

// TAudioSink //////////////////////////////////////////////////////////////////

constructor TAudioSink.Create();
begin
  inherited Create();
  pvAppIsClosing := False;
end;


destructor TAudioSink.Destroy();
begin
  pvAppIsClosing := True;
  pvStopRec := True;
  TerminateRenderThread();
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
      ErrMsg(Format('IAudioCaptureClient.GetBuffer returned %d frames.',
                    [0]),
             GetLastError());

      hr := E_UNEXPECTED;
      goto done;
    end;

  iBytesToWrite := (NumFrames * pwfx.nBlockAlign);
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(pData),
                             iBytesToWrite);

  if (iBytesToWrite <> iBytesWritten) then
    begin
      ErrMsg(Format('mmioWrite wrote %d bytes : expected %d bytes',
                    [iBytesWritten, iBytesToWrite]),
             GetLastError());
      hr := E_UNEXPECTED;
      goto done;
    end;

  // Keep score of written bytes to file.
  Inc(pvBytesWritten,
      iBytesWritten);

  // For safety on 32bit platforms we have to limit the wav size to < 4 gb.
  // So, we limit the size to 3.9 GB.
  if (pvBytesWritten > MAX_FILE_SIZE) then
    pvStopRec := True;  // Stop recording and close file.

done:
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
      ErrMsg(Format('mmioCreateChunk("RIFF/WAVE") failed: MMRESULT = %',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioCreateChunk("fmt") failed: MMRESULT = %',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioWrite(fmt data) wrote %d bytes; expected %d bytes',
                    [iBytesWritten, iBytesInWfx]),
             GetLastError());
      Result := mResult;
      Exit;
    end;

  // ascend from the 'fmt ' chunk
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("fmt") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioCreateChunk("fmt") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioWrite(fact data) wrote %d bytes; expected %d bytes"',
                    [iBytesWritten, SizeOf(dwFrames)]),
             GetLastError());
      Result := mResult;
      Exit;
    end;

  // ascend from the 'fact' chunk
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);

  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("fact") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioCreateChunk("data") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
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
      ErrMsg(Format('mmioAscend("pckData (MMCKINFO)") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
      goto done;
    end;

  mResult := mmioAscend(hmFile,
                        @pckRIFF,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    begin
      ErrMsg(Format('mmioAscend("pckRIFF (MMCKINFO") failed: MMRESULT = %d',
                    [mResult]),
             GetLastError());
      goto done;
    end;

done:
  Result := mResult;
end;


// Stop, destroy thread.
procedure TAudioSink.Stop();
begin
  // Signal the threadfuntion to stop.
  StopRecording := True;
  // Terminate the thread where the threadfunction is running in.
  TerminateRenderThread();
end;


//
// Render the audiodata in a parallel task.
//
function TAudioSink.RenderData(): HResult;
var
  hr: HResult;

  pData: PByte;
  flags: DWORD;
  numFramesAvailable: UINT32;
  packetLength: UINT32;

begin
  hr := S_OK;
  packetLength := 0;

try
  // Each loop fills about half of the shared buffer.
  while (pvStopRec = False) do
    begin

      // Sleep for half the buffer duration if the size > 0.
      if (bufferDuration > 0) then
        Sleep((pvHnsActualDuration div REFTIMES_PER_MILLISEC) div (bufferDuration div 2))
      else
        Sleep(0);

      hr := pvCaptureClient.GetNextPacketSize(packetLength);
      if FAILED(hr) then
        Break;

      HandleThreadMessages(GetCurrentThread());

      while (packetLength <> 0) do
        begin
          if pvStopRec then
            Break;

          // Get the available data in the shared buffer.
          hr := pvCaptureClient.GetBuffer(pData,
                                          numFramesAvailable,
                                          flags,
                                          nil,
                                          nil);
          if FAILED(hr) then
            Break;

          // Tell CopyData to write silence.
          // When a sound is detected, the app will act and process data.
          if (flags = AUDCLNT_BUFFERFLAGS_SILENT) then
            pData := nil;

         // Store the buffersize.
         if (pvBufferFrames = 0) and (numFramesAvailable > 0) then
           pvBufferFrames := numFramesAvailable;

          // Copy the available capture data to the audio sink.
          hr := CopyData(pData,
                         numFramesAvailable,
                         pvPwfx);
          if FAILED(hr) then
            Break;

          hr := pvCaptureClient.ReleaseBuffer(numFramesAvailable);
          if FAILED(hr) then
            Break;

          hr := pvCaptureClient.GetNextPacketSize(packetLength);
          if FAILED(hr) then
            Break;
      end;
    end;

    // Wait for last data in buffer to play before stopping.
    if (bufferDuration > 0) then
      Sleep((pvHnsActualDuration div REFTIMES_PER_MILLISEC) div (bufferDuration div 2))
    else
      Sleep(0);

finally

  if SUCCEEDED(hr) then
    begin
      hr := pvAudioClient.Stop();  // Stop recording.
        if SUCCEEDED(hr) then
      hr := FinishWaveFile(pvCkData,
                           pvCkRIFF);
      mmioClose(hmFile,
                0);
    end
  else
    begin
      mmioClose(hmFile,
                0);
      //{void}DeleteFile(ppfileName);
    end;

  // Send capturing stopped.
  FOnCapturingStopped(Self);
  pData := nil;
  Result := hr;
end;
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
  bufferFrameCount: UINT32;
  hnsActualDuration: REFERENCE_TIME;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

label
  done;

begin
  pvStopRec := False;
  pvPwfx := nil;

  // Create the initial audio file
  hr := OpenFile(ppFileName);
  if FAILED(hr) then
    goto done;

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
                         Pointer(pvAudioClient));
  if FAILED(hr) then
    goto done;

  // Get the mixformat from the WAS
  // See the comments on IAudioClient.GetMixFormat.
  //
  hr := pvAudioClient.GetMixFormat(pvPwfx);
  if FAILED(hr) then
    goto done;

  // Needed for timeformat.
  CopyMemory(@pvWavFormatEx,
             pvPwfx,
             SizeOf(WAVEFORMATEX));

  // The original sample creates a bufferDuration of 2 seconds,
  // that will cause sound disturbtion when capture sound from a streameservice like
  // YouTube or other high latency services.
  // To prevent this, we use as a minimum the value of hnsDefaultDevicePeriod.
  hr := pvAudioClient.GetDevicePeriod(hnsDefaultDevicePeriod,
                                      hnsMinimumDevicePeriod);
  if FAILED(hr) then
    goto done;

  // Pick the correct device period.
  if (bufferDuration > 0) then
    begin
      if (bufferDuration < hnsMinimumDevicePeriod) then
        bufferDuration := hnsMinimumDevicePeriod;
    end;

  // Initialize low-latency client.
  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_LOOPBACK,
                                 bufferDuration, // Note: When bufferDuration = 0, the audioclient will automaticly decise the bufferduration.
                                 0, // Must be zero when using shared mode!
                                 pvPwfx,
                                 @GUID_NULL);
  if FAILED(hr) then
    goto done;

  // Get the size of the allocated buffer.
  // 480 frames per ms at 48000 samples/sec
  // 480 with 1 ms buffer
  // 960 with 2 ms buffer
  // 1440 with 3 ms buffer
  // 1920 with 4 ms buffer etc.
  hr := pvAudioClient.GetBufferSize(bufferFrameCount);
  if FAILED(hr) then
    goto done;

  hr := pvAudioClient.GetService(IID_IAudioCaptureClient,
                                 pvCaptureClient);
  if FAILED(hr) then
    goto done;

  mr := WriteWaveHeader(pvPwfx,
                        pvCkRIFF,
                        pvCkData);

  if (mr <> 0) then
    begin
      hr := E_FAIL;
      goto done;
    end;

  // Calculate the actual duration of the allocated buffer.
  if (bufferFrameCount > 0) then
    hnsActualDuration := (REFTIMES_PER_SEC *
                          bufferFrameCount div pvPwfx.nSamplesPerSec)
  else
    hnsActualDuration := 0;

  // Store for user information.
  pvBufferHns := hnsActualDuration;

  // Get the stream latency (normally this should be inbetween 0 and 15 ms on Windows 11)
  hr := pvAudioClient.GetStreamLatency(pvHnsLatency);
  if FAILED(hr) then
    goto done;

  hr := pvAudioClient.Start();  // Start recording.
  if FAILED(hr) then
    goto done;

  // Signal the mainform caturing started.
  FOnCapturingStart(Self);

  // Render the audiodata.
  CreatedRenderThread;

done:
  pvErrorStatus := hr;
  Result := hr;
end;

end.
