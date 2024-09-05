// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WASAPIEngine.pas
// Kind: Pascal Unit
// Release date: 24-08-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: The WASAPI renderer class.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 05/09/2024 Tony                Enforged the release of all interfaces for older Delphi versions < 12.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
// =============================================================================
// Source:  https://learn.microsoft.com/en-us/windows/win32/coreaudio/rendering-a-stream
//          https://matthewvaneerde.wordpress.com/2008/12/10/sample-playing-silence-via-wasapi-event-driven-pull-mode
//          FactoryX
//
// Copyright (c) FactoryX All rights reserved.
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
unit WASAPIEngine;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  WinApi.Messages,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  {VCL}
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.MMDeviceApi,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  Tools;

const
  WM_DATA_PROCESSED_NOTIFY = WM_APP + 100;
  WM_DATA_READY_NOTIFY = WM_APP + 101;
  WM_DATA_ENDED_NOTIFY = WM_APP + 102;

  // Minimum and maximum volume values.
  MIN_VOLUME: Single = 0.0;
  MAX_VOLUME: Single = 1.0;

type

  TDeviceState = (dsUninitialized,
                  dsError,
                  // All states >= Initialized will allow some methods
                  // to be called successfully on the Audio Client.
                  dsInitialized, // < from this one..
                  dsPlay,
                  dsPause,
                  dsStop); // < ..until here

  TWasApiEngine = class(TObject)
  private

    pvAudioClient: IAudioClient;
    pvAudioStreamVolume: IAudioStreamVolume;
    pvRenderClient: IAudioRenderClient;
    pvAudioClock: IAudioClock;

    pvBytes: PByte;
    pvBytesLength: UINT32; // Length of pvBytes in bytes
    FOffset: UINT32;

    pvSourceWfx: PWAVEFORMATEX;
    pvwaveformatlength: Cardinal;
    pvSoundChannels: Word;

    pvErrStatus: HResult;
    pvDeviceState: TDeviceState;
    hwndCaller: HWND;
    pvBufferFrameCount: UINT32;

    pvFlags: DWORD;
    pvMmcssHandle: THandle;
    pvMmcssTaskIndex: DWord;
    //
    pvAudioSamplesReadyEvent: THandle;
    pvShutdownEvent: THandle;
    pvRenderThreadClosedEvent: THandle;

    function Initialize(): Boolean;
    procedure ResetAudioData(pFreeSourceStream: Boolean);
    function SetFormat(pwfx: PWAVEFORMATEX): HRESULT;
    function InitializeAudioEngine(): HRESULT;
    function LoadData(bufferFrameCount: UINT32;
                      pData: PByte;
                      var flags: DWORD): HRESULT;
    function PlayAudioStream(): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;
    procedure BeforeDestruction(); override;

    function LoadFile(hHwnd: HWND;
                      const audiofile: string;
                      fileDuration: LONGLONG): HResult;

    // Control.
    function Start(): HResult;
    function Stop(): HResult;
    function Pause(): HResult;
    // Volume
    function SetVolumes(pVolLeft: Single;
                        pVolRight: Single): HResult;

    property DeviceState: TDeviceState read pvDeviceState;
    property SoundChannels: Word read pvSoundChannels;
  end;


implementation

uses
  System.Threading,
  System.Services.Avrt;


constructor TWasApiEngine.Create();
begin

  inherited;
  Initialize();
end;


destructor TWasApiEngine.Destroy();
begin
  // ..
  inherited;
end;


procedure TWasApiEngine.BeforeDestruction();
begin

  // If the renderer is stil running, signal it to stop.
  if (pvRenderThreadClosedEvent <> 0) then
    begin

      SetEvent(pvShutdownEvent);
      WaitForSingleObject(pvRenderThreadClosedEvent,
                          INFINITE);
      CloseHandle(pvRenderThreadClosedEvent);
      pvRenderThreadClosedEvent := 0;
    end;

  HandleThreadMessages(GetCurrentThread());

  ResetAudioData(True);

  CoTaskMemFree(pvSourceWfx);

  // Release the interfaces.
  if Assigned(pvAudioClient) then
    SafeRelease(pvAudioClient);

  if Assigned(pvRenderClient) then
    SafeRelease(pvRenderClient);

  if Assigned(pvAudioStreamVolume) then
    SafeRelease(pvAudioStreamVolume);

  if Assigned(pvAudioClock) then
    SafeRelease(pvAudioClock);

  if (pvAudioSamplesReadyEvent > 0) then
    begin

      CloseHandle(pvAudioSamplesReadyEvent);
      pvAudioSamplesReadyEvent := 0;
    end;

  if (pvShutdownEvent > 0) then
    begin

      CloseHandle(pvShutdownEvent);
      pvShutdownEvent := 0;
    end;

  inherited;
end;


function TWasApiEngine.Initialize(): Boolean;
begin

  pvBytes := nil;
  pvSourceWfx := nil;
  pvBytesLength := 0;

  // This event is used by the audioclient interface.
  pvAudioSamplesReadyEvent := CreateEventEx(nil,
                                            nil,
                                            0,
                                            EVENT_MODIFY_STATE or SYNCHRONIZE);

  // Notifies the engine we are shutting down.
  pvShutdownEvent := CreateEventEx(nil,
                                   nil,
                                   0,
                                   EVENT_MODIFY_STATE or SYNCHRONIZE);

  // Sends event when the renderthread has been closed.
  pvRenderThreadClosedEvent := CreateEventEx(nil,
                                             nil,
                                             0,
                                             EVENT_MODIFY_STATE or SYNCHRONIZE);

  Result := True;
end;


procedure TWasApiEngine.ResetAudioData(pFreeSourceStream: Boolean);
begin

  if pFreeSourceStream then
    if (pvBytes <> nil) then
      begin
        FreeMem(pvBytes);
        pvBytes := nil;
        pvBytesLength := 0;
      end;

  if (pvMmcssHandle > 0) then
    AvRevertMmThreadCharacteristics(pvMmcssHandle);
end;


function TWasApiEngine.LoadFile(hHwnd: HWND;
                                const audiofile: string;
                                fileDuration: LONGLONG): HResult;
var
  hr: HResult;
  sourceReaderConfiguration: IMFAttributes;
  sourceReader: IMFSourceReader;
  nativeMediaType: IMFMediaType;
  uncompressedAudioType: IMFMediaType;
  partialType: IMFMediaType;
  majorType: TGUID;
  subType: TGUID;

label
  done;

begin

  hwndCaller := hHwnd;
  pvErrStatus := E_FAIL;

  hr := MFCreateAttributes(sourceReaderConfiguration,
                           1);
  if SUCCEEDED(hr) then
    hr := sourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                              UInt32(1));

  if SUCCEEDED(hr) then
    hr := MFCreateSourceReaderFromURL(PWideChar(audiofile),
                                      sourceReaderConfiguration,
                                      sourceReader);

  if SUCCEEDED(hr) then
    hr := sourceReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                          0,
                                          nativeMediaType);

  if SUCCEEDED(hr) then
    hr := nativeMediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                  majorType);

  if SUCCEEDED(hr) then
    if not IsEqualGUID(MFMediaType_Audio,
                       majorType) then
      begin
        raise Exception.CreateFmt('%s is not a valid audio file.',
                                  [audiofile]);
        hr := MF_E_INVALID_FILE_FORMAT;
        goto done;
      end;

  hr := nativeMediaType.GetGUID(MF_MT_SUBTYPE,
                                subType);

  if SUCCEEDED(hr) then
    if not (IsEqualGUID(MFAudioFormat_Float,
                        subType) or
            IsEqualGUID(MFAudioFormat_PCM,
                        subType)) then
      begin

        hr := MFCreateMediaType(partialType);

        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                    MFMediaType_Audio);

        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_SUBTYPE,
                                    MFAudioFormat_PCM {MFAudioFormat_Float});
        
        // Set this type on the source reader.
        // The source reader will load the necessary decoder.
        if SUCCEEDED(hr) then
          hr := sourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                                 0,
                                                 partialType);
      end;


  if SUCCEEDED(hr) then
    hr := sourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                           uncompressedAudioType);

  if SUCCEEDED(hr) then
    hr := MFCreateWaveFormatExFromMFMediaType(uncompressedAudioType,
                                              pvSourceWfx,
                                              pvwaveformatlength,
                                              MFWaveFormatExConvertFlag_ForceExtensible {MFWaveFormatExConvertFlag_Normal});

  if SUCCEEDED(hr) then
    begin

      ResetAudioData(True);
      pvBytesLength := 0;

      // Fill the buffer. We use a thread for this, to speedup things.
      TThread.Synchronize(nil,
                          procedure
                          var
                            hres: HResult;
                            audioData: PByte;
                            buffer: IMFMediaBuffer;
                            sample: IMFSample;
                            flags: DWORD;
                            audioDataLength: DWORD;

                            begin

                              hres := S_OK;
                              Flags := 0;

                              while (hres = S_OK) do
                                begin
                                  flags := 0;
                                  hres := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                                                  0,
                                                                  nil,
                                                                  @flags,
                                                                  nil,
                                                                  @sample);

                                  if (flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0 then
                                    Break;

                                  if (sample = nil) then
                                    Continue;


                                  hres := sample.ConvertToContiguousBuffer(@buffer);
                                  if FAILED(hres) then
                                    Break;

                                  hres := buffer.Lock(audioData,
                                                      nil,
                                                      @audioDataLength);
                                  if FAILED(hres) then
                                    Break;

                                  try

                                  ReallocMem(pvBytes,
                                             pvBytesLength + audioDataLength);

                                  Move(audioData^,
                                       (pvBytes + pvBytesLength)^,
                                       audioDataLength);

                                  Inc(pvBytesLength,
                                      audioDataLength);

                                  finally

                                    hres := buffer.Unlock();
                                  end;

                                  sample := nil;
                                end;
                              pvErrStatus := hres;
                            end);
    end;


  if SUCCEEDED(hr) and SUCCEEDED(pvErrStatus) then
    hr := InitializeAudioEngine();

done:

  Result := hr;
end;


function TWasApiEngine.InitializeAudioEngine(): HRESULT;
var
  hr: HRESULT;
  hnsRequestedDuration: REFERENCE_TIME;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;

  {$IFDEF DEBUG}
  // Use this to read out the last error in AvSetMmThreadCharacteristics.
  //nLastError: DWord;
  {$ENDIF}

begin
  ResetAudioData(False);

  hnsRequestedDuration := REFTIMES_PER_SEC;
  FOffset := 0;

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    Exit(hr);

  hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                            eMultimedia,
                                            pDevice);
  if FAILED(hr) then
    Exit(hr);

  hr := pDevice.Activate(IID_IAudioClient,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pvAudioClient));
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_EVENTCALLBACK or AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM,
                                 hnsRequestedDuration, // Note: When bufferDuration = 0, the audioclient will automaticly decise the bufferduration.
                                 0,
                                 pvSourceWfx,
                                 nil);
  if FAILED(hr) then
    Exit(hr);

  // Create the AudioStreamVolume interface.
  hr := pvAudioClient.GetService(IID_IAudioStreamVolume,
                                 pvAudioStreamVolume);
  if FAILED(hr) then
    Exit(hr);

  // Create the AudioClock interface.
  hr := pvAudioClient.GetService(IID_IAudioClock,
                                 pvAudioClock);
  if FAILED(hr) then
    Exit(hr);

  // Store some WAVEFORMATEX members, we need for calculations.
  hr := SetFormat(pvSourceWfx);
  if FAILED(hr) then
    Exit(hr);

  // Retrieve the maximum size of the shared buffer.
  hr := pvAudioClient.GetBufferSize(pvBufferFrameCount);
  if FAILED(hr) then
    Exit(hr);

  // Set the eventhandle for AudioSamplesReadyEvent.
  hr := pvAudioClient.SetEventHandle(pvAudioSamplesReadyEvent);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClient.GetService(IID_IAudioRenderClient,
                                 pvRenderClient);
  if FAILED(hr) then
    Exit(hr);

  // Register with MMCSS.
  if (pvMmcssHandle = 0) then
    begin

      pvMmcssHandle := 0;
      pvMmcssTaskIndex := 0; // Must be initialized with 0, otherwise we get "The specified task index is invalid." error.

      pvMmcssHandle := AvSetMmThreadCharacteristics('Playback',
                                                    @pvMmcssTaskIndex);
      if (pvMmcssHandle = 0) then
        begin
          {$IFDEF DEBUG}
          // nLastError := GetLastError();
          // See: https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes.
          {$ENDIF}
          Exit(E_UNEXPECTED);
        end;
    end;

  if SUCCEEDED(hr) then
    SendMessage(hwndCaller,
                WM_DATA_READY_NOTIFY,
                WPARAM(1),
                LPARAM(0));
  Result := hr;
end;


function TWasApiEngine.PlayAudioStream(): HRESULT;
var
  hr: HRESULT;
  waitArray: array[0..1] of THandle;
  waitResult: DWord;
  flags: DWord;
  numFramesPadding: UINT32;
  numFramesAvailable: UINT32;
  pBufferData: PByte;
  u64Position: UINT64;
  u64Frequency: UINT64;

begin

  pBufferData := nil;
  flags := 0;
  waitArray[0] := pvShutdownEvent;
  waitArray[1] := pvAudioSamplesReadyEvent;

  // Remember the offset to continue playing after pause.
  if not (pvDeviceState = dsPause) then
    FOffset := 0;

  // Create shared buffer.
  ReallocMem(pBufferData,
             pvBufferFrameCount * pvSourceWfx.nBlockAlign);

  hr := LoadData(pvBufferFrameCount,
                 pBufferData,
                 pvFlags);
  if FAILED(hr) then
    Exit(hr);

  // We defined the size of pData, the renderer will take of care of it, so, we can free the data pointer.
  // Note: Freeing it on the end of this method will result in a memoryleak,
  //       because the engine will internally use the data pointer until it stops and releasing it.

  FreeMem(pBufferData);

  hr := pvRenderClient.GetBuffer(pvBufferFrameCount,
                                 pBufferData);
  if FAILED(hr) then
    Exit(hr);

  hr := pvRenderClient.ReleaseBuffer(pvBufferFrameCount,
                                     pvFlags);
  if FAILED(hr) then
    Exit(hr);

  hr := pvAudioClock.GetFrequency(u64Frequency);
  if FAILED(hr) then
    Exit(hr);

  // Run.
  hr := pvAudioClient.Start;
  if FAILED(hr) then
    Exit(hr);

  pvDeviceState := dsPlay;

  while (pvDeviceState = dsPlay) and (pvAudioClient <> nil) do
    begin

      waitResult := WaitForMultipleObjects(2,
                                           @waitArray,
                                           False,
                                           INFINITE);

      case waitResult of

      // Shutdown Event
      // We're done.
      WAIT_OBJECT_0 + 0:
        begin
          Break;
        end;

      // Audio Samples Ready Event
      WAIT_OBJECT_0 + 1:
        begin

          // Get available bufferspace.
          hr := pvAudioClient.GetCurrentPadding(numFramesPadding);
          if FAILED(hr) then
            Break;

          numFramesAvailable := pvBufferFrameCount - numFramesPadding;

          // Get available space in shared buffer.
          hr := pvRenderClient.GetBuffer(numFramesAvailable,
                                         pBufferData);
          if FAILED(hr) then
            Break;

          hr := LoadData(numFramesAvailable,
                         pBufferData,
                         flags);
          if FAILED(hr) then
            Break;

          hr := pvRenderClient.ReleaseBuffer(numFramesAvailable,
                                             flags);
          if FAILED(hr) then
            Break;

          // If we reach the end of the audio, we will detect silence.
          if (flags and AUDCLNT_BUFFERFLAGS_SILENT = AUDCLNT_BUFFERFLAGS_SILENT) then
            begin
              pvDeviceState := dsStop;
              Break;
            end;

          hr :=  pvAudioClock.GetPosition(@u64Position,
                                          nil);
          if FAILED(hr) then
            Break;

          SendMessage(hwndCaller,
                      WM_DATA_PROCESSED_NOTIFY,
                      WPARAM((u64Position div u64Frequency) * REFTIMES_PER_SEC),
                      LPARAM(u64Position));

          // When using this code, we don't need the IAudioClock interface to get the amount of samples payed.
          // Global var.
          //   pvTotalFramesRendered: UINT64;

          //Inc(pvTotalFramesRendered,
          //    numFramesAvailable);
          //
          //SendMessage(hwndCaller,
          //            WM_DATA_PROCESSED_NOTIFY,
          //            WPARAM((pvTotalFramesRendered div pvSourceWfx.nSamplesPerSec) * REFTIMES_PER_SEC),
          //            LPARAM(pvTotalFramesRendered));
        end;
      end;
    end;

  if FAILED(hr) then
    pvDeviceState := dsError;

  hr := pvAudioClient.Stop();

  if pvDeviceState in [dsStop, dsError] then
    begin
      if SUCCEEDED(hr) then
        hr := pvAudioClient.Reset;
    end;
  
  SetEvent(pvRenderThreadClosedEvent);

  // Send message to caller (main form).
  SendMessage(hwndCaller,
              WM_DATA_ENDED_NOTIFY,
              WPARAM(1),
              LPARAM(0));

  Result := hr;
end;


function TWasApiEngine.LoadData(bufferFrameCount: UINT32;
                                pData: PByte;
                                var flags: DWORD): HRESULT;
var
  bytesToCopy: UINT32;

begin

  bytesToCopy := bufferFrameCount * pvSourceWfx.nBlockAlign;

  if (FOffset + bytesToCopy) > pvBytesLength then
    begin
      bytesToCopy := pvBytesLength - FOffset;
      flags := AUDCLNT_BUFFERFLAGS_SILENT;
    end;

  Move((pvBytes + FOffset)^,
       pData^,
       bytesToCopy);

  Inc(FOffset,
      bytesToCopy);

  Result := S_OK;
end;


function TWasApiEngine.SetFormat(pwfx: PWAVEFORMATEX): HRESULT;
begin

  if (pwfx.wFormatTag = WAVE_FORMAT_PCM) or (pwfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
    begin

      // We can do more checks here.
      pvSoundChannels := pwfx.nChannels;
      Result := S_OK;
    end
  else
    Result := E_FAIL;
end;


function TWasApiEngine.Start(): HResult;
{$IF COMPILERVERSION > 20.0}
var
  Task: ITask;
{$ENDIF}

begin

{$IF COMPILERVERSION < 28.0}
  TThread.CreateAnonymousThread(procedure
      var
        hr: HRESULT;

      begin
        hr := PlayAudioStream();
        TThread.Queue(nil,
                      procedure
                      begin
                        pvErrStatus := hr; // Update the status in the main thread if the function fails.
                      end);
      end).Start;
{$ELSE}
  // Run in a Task.
  Task := TTask.Create(procedure
                       var
                         hr: HRESULT;

                       begin
                         hr := PlayAudioStream();
                         TThread.Queue(nil,
                                       procedure
                                         begin
                                           // Update the status in the main thread.
                                           pvErrStatus := hr;
                                         end);
                       end);

  Task.Start;
{$ENDIF}
  Result := pvErrStatus;
end;


function TWasApiEngine.Stop(): HResult;
begin

  pvDeviceState := dsStop;
  Result := S_OK;
end;


function TWasApiEngine.Pause(): HResult;
begin
  pvDeviceState := dsPause;
  Result := S_OK;
end;


function TWasApiEngine.SetVolumes(pVolLeft: Single;
                                  pVolRight: Single): HResult;
var
  hr: HResult;

begin
  //
  if Assigned(pvAudioStreamVolume) then
    begin

      // Range check left.
      if (pVolLeft > MAX_VOLUME) then
        pVolLeft := MAX_VOLUME;
      if (pVolLeft < MIN_VOLUME) then
        pVolLeft := MIN_VOLUME;

      hr := pvAudioStreamVolume.SetChannelVolume(0,
                                                 pVolLeft);
      if SUCCEEDED(hr) then
        begin

          // Range check right.
          if (pVolRight > MAX_VOLUME) then
            pVolRight := MAX_VOLUME;
          if (pVolRight < MIN_VOLUME) then
            pVolRight := MIN_VOLUME;

          hr := pvAudioStreamVolume.SetChannelVolume(1,
                                                     pVolRight);
        end;
    end
  else
    hr := E_POINTER;

  Result := hr;
end;

end.

