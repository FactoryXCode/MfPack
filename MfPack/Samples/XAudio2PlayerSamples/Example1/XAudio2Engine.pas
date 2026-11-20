// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: XAudio2Engine.pas
// Kind: Pascal Unit
// Release date: 30-03-2024
// Language: ENU
//
// Revision Version: 4.1.0
// Description: The XAudio2 renderer class.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Carmen (carmenh), Tony Kalf (maXcomX))
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: https://www.gamedev.net/articles/programming/general-and-gameplay-programming/decoding-audio-for-xaudio2-with-microsoft-media-foundation-r4280/
//         https://learn.microsoft.com/en-us/windows/win32/xaudio2/how-to--load-audio-data-files-in-xaudio2
//
// Copyright © FacctoryX
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
unit XAudio2Engine;

interface

uses
  MfSampleWorker,
{WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  Messages,
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
  {XAudio2}
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPO,
  WinApi.DirectX.XAudio2.X3DAudio,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Application}
  Tools;

const
  WM_DATA_PROCESSED_NOTIFY = WM_APP + 100;
  WM_DATA_READY_NOTIFY = WM_APP + 101;
  WM_DATA_ENDED_NOTIFY = WM_APP + 102;

var
  pvCriticalSection: TRTLCriticalSection;

type

  TXaudio2Engine = class(TObject)

  private
    FMonitor: TSampleWorker;
    FMonitorStop: Boolean;
    procedure StartMonitor();
    procedure StopMonitor();
    procedure MonitorPlayback();
  private
    pvXAudio2: IXAudio2;
    pvMasteringVoice: PIXAudio2MasteringVoice;
    pvSourceVoice: PIXAudio2SourceVoice;
    pvXAudioBuffer: XAUDIO2_BUFFER;
    pvBytes: TBytes;
    pvFileName: TFileName;
    pvSourceFileDuration: LONGLONG;
    pvWaveformatex: PWAVEFORMATEX;
    pvwaveformatlength: Cardinal;
    hwndCaller: HWND;
    bPlaying: Boolean;
    nChannels: UInt32;            // Holds the number of volumechannels.
    nSamplesPerSecond: UInt32;    // Playtime calculations.

  public
    m_VolumeChannels: TFloatArray;  // Dynamic array that holds the volume per channel.

    constructor Create();
    destructor Destroy(); override;
    procedure BeforeDestruction(); override;

    function LoadFile(hHwnd: HWND;
                      const audiofile: TFileName;
                      fileDuration: LONGLONG): HResult;

    function InitializeXAudio2(replay: Boolean = False): HResult;  // Initialize XAudio2 engine. Call LoadFile first!

    function Start(): HResult;
    function Stop(): HResult;
    function Pause(): HResult;

    // Set all channels at once.
    function SetVolume(volume: Single): HResult;
    // Volume per channel.
    procedure SetVolumes(Value: TFloatArray);


    property SoundChannels: UINT32 read nChannels;
    property Volumes: TFloatArray read m_VolumeChannels write SetVolumes;
    property IsPlaying: Boolean read bPlaying;

  end;


implementation


procedure TXaudio2Engine.StartMonitor();
begin
  StopMonitor();
  FMonitorStop := False;
  FMonitor := TSampleWorker.Create(MonitorPlayback);
  FMonitor.Start();
end;

procedure TXaudio2Engine.StopMonitor();
var
  Msg: TMsg;
begin
  FMonitorStop := True;
  if FMonitor <> nil then
    begin
      FMonitor.WaitFor();
      FreeAndNil(FMonitor);
    end;
  if hwndCaller <> 0 then
    while PeekMessage(Msg,hwndCaller,WM_DATA_PROCESSED_NOTIFY,WM_DATA_ENDED_NOTIFY,PM_REMOVE) do ;
end;

procedure TXaudio2Engine.MonitorPlayback();
var
  State: XAUDIO2_VOICE_STATE;
  Time: Int64;
begin
  while not FMonitorStop do
    begin
      if pvSourceVoice = nil then Exit;
      pvSourceVoice.GetState(@State,0);
      if nSamplesPerSecond > 0 then Time := (Int64(State.SamplesPlayed)*10000000) div nSamplesPerSecond
      else Time := 0;
      PostMessage(hwndCaller,WM_DATA_PROCESSED_NOTIFY,WPARAM(Time),LPARAM(State.SamplesPlayed));
      if State.BuffersQueued = 0 then Break;
      Sleep(10);
    end;
  if not FMonitorStop then PostMessage(hwndCaller,WM_DATA_ENDED_NOTIFY,1,0);
end;

constructor TXaudio2Engine.Create();
begin
  inherited;

end;


destructor TXaudio2Engine.Destroy();
begin

  pvMasteringVoice := nil;
  pvSourceVoice := nil;
  SafeRelease(pvXAudio2);
  if pvWaveformatex <> nil then CoTaskMemFree(pvWaveformatex);
  pvWaveformatex := nil;
  inherited;
end;


procedure TXaudio2Engine.BeforeDestruction();
begin
  StopMonitor();
  if pvSourceVoice <> nil then
    begin
      pvSourceVoice.Stop();
      pvSourceVoice.FlushSourceBuffers();
      pvSourceVoice.DestroyVoice();
      pvSourceVoice := nil;
    end;
  if pvMasteringVoice <> nil then
    begin
      pvMasteringVoice.DestroyVoice();
      pvMasteringVoice := nil;
    end;
  pvXAudio2 := nil;
  inherited BeforeDestruction();
end;


function TXaudio2Engine.LoadFile(hHwnd: HWND;
                                 const audiofile: TFileName;
                                 fileDuration: LONGLONG): HResult;
var
  hr: HResult;
  sourceReaderConfiguration: IMFAttributes;
  sourceReader: IMFSourceReader;
  nativeMediaType: IMFMediaType;
  majorType: TGUID;
  subType: TGUID;
  partialType,
  uncompressedAudioType: IMFMediaType;

label
  done;

  procedure ProcessAudio1();
  var
    hres: HResult;
    sample: IMFSample;
    flags: DWord;
    buffer: IMFMediaBuffer;
    audioData: PByte;
    audioDataLength: DWord;

    begin
      hres := S_OK;
      while (hres = S_OK) do
        begin
          flags := 0;
          hres := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                          0,
                                          nil,
                                          @flags,
                                          nil,
                                          @sample);

          // Check for eof
          if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
            Break;

          // If the sample is nil, there is a gap in the data stream that can't be filled: No reason to quit..
          if (sample = nil) then
            Continue;

          // Convert data to contiguous buffer.
          hres := sample.ConvertToContiguousBuffer(@buffer);

          if SUCCEEDED(hres) then
            // Lock Buffer and copy to local memory
            hres := buffer.Lock(audioData,
                                nil,
                                @audioDataLength);

          if SUCCEEDED(hres) then
            try
              SetLength(pvBytes,
                        Length(pvBytes) + Integer(audioDataLength));

              Move(audioData^,
                   pvBytes[Length(pvBytes) - Integer(audioDataLength)],
                   audioDataLength);
            finally
              hres := buffer.Unlock();
            end;

          SafeRelease(sample);
          if FAILED(hres) then
            exit;
        end;
    end;

begin
  StopMonitor();
  if pvSourceVoice <> nil then
    begin
      pvSourceVoice.Stop();
      pvSourceVoice.FlushSourceBuffers();
      pvSourceVoice.DestroyVoice();
      pvSourceVoice := nil;
    end;
  if pvMasteringVoice <> nil then
    begin
      pvMasteringVoice.DestroyVoice();
      pvMasteringVoice := nil;
    end;
  pvXAudio2 := nil;
  if pvWaveformatex <> nil then
    begin
      CoTaskMemFree(pvWaveformatex);
      pvWaveformatex := nil;
    end;

  hwndCaller := hHwnd;
  pvFileName := audiofile;
  pvSourceFileDuration := fileDuration;

  // Create Attribute Store
  hr := MFCreateAttributes(sourceReaderConfiguration,
                           1);

  if SUCCEEDED(hr) then
    hr := sourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                              UInt32(1));


  // Create Source Reader
  if SUCCEEDED(hr) then
    hr := MFCreateSourceReaderFromURL(StrToPWideChar(audiofile),
                                      sourceReaderConfiguration,
                                      sourceReader);

  // Query information about the media file.
  if SUCCEEDED(hr) then
    hr := sourceReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                          0,
                                          @nativeMediaType);

  // Check if media file is indeed an audio file.
  if SUCCEEDED(hr) then
    hr := nativeMediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                  majorType);

  // Check if we are dealing with an audio format.
  if SUCCEEDED(hr) then
   if not IsEqualGUID(MFMediaType_Audio,
                      majorType) then
     begin
       raise Exception.CreateFmt('%s is not an audio file',
                                 [pvFileName]);
     end;

  // Check if media file is compressed or uncompressed.
  if SUCCEEDED(hr) then
    hr := nativeMediaType.GetGUID(MF_MT_SUBTYPE,
                                subType);

  if SUCCEEDED(hr) then
    if not (IsEqualGUID(MFAudioFormat_Float,
                        subType) or
            IsEqualGUID(MFAudioFormat_PCM,
                        subType)) then
      begin
        // Audio file is compressed.
        // Inform the SourceReader we want uncompressed data
        // This causes it to look for decoders to perform the request we are making
        hr := MFCreateMediaType(partialType);

        // We want Audio
        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                    MFMediaType_Audio);

        // We need uncompressed data
        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_SUBTYPE,
                                    MFAudioFormat_PCM);

        if SUCCEEDED(hr) then
          hr := sourceReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                                 0,
                                                 partialType);
      end;

  if SUCCEEDED(hr) then
    hr := sourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                           @uncompressedAudioType);
  if SUCCEEDED(hr) then
    hr := MFCreateWaveFormatExFromMFMediaType(uncompressedAudioType,
                                              pvWaveformatex,
                                              pvwaveformatlength);

  // Get the number of channels.
  if SUCCEEDED(hr) then
    nChannels := MFGetAttributeUINT32(uncompressedAudioType,
                                      MF_MT_AUDIO_NUM_CHANNELS,
                                      UINT32(2));
  // Set the channel array.
  SetLength(m_VolumeChannels,
            nChannels);

  // Get Samples Per Second for time calculations.
  if SUCCEEDED(hr) then
    nSamplesPerSecond := MFGetAttributeUINT32(uncompressedAudioType,
                                              MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                              UINT32(0));
  // Create sample.
  if SUCCEEDED(hr) then
    SetLength(pvBytes,
              0)
  else
    goto done;

  // Decode the source before creating the native XAudio2 voice.
  ProcessAudio1();

  // Create Xaudio2 and run audio.
  hr := InitializeXAudio2();

done:
  Result := hr;
end;


function TXaudio2Engine.InitializeXAudio2(replay: Boolean = False): HResult;
var
  hr: HResult;

label
  done;

begin
  StopMonitor();
  if pvSourceVoice <> nil then
    begin
      pvSourceVoice.Stop();
      pvSourceVoice.FlushSourceBuffers();
      pvSourceVoice.DestroyVoice();
      pvSourceVoice := nil;
    end;
  if pvMasteringVoice <> nil then
    begin
      pvMasteringVoice.DestroyVoice();
      pvMasteringVoice := nil;
    end;
  pvXAudio2 := nil;

  // Use the XAudio2Create function to create an instance of the XAudio2 engine.
  hr := XAudio2Create(@pvXAudio2,
                      0,
                      XAUDIO2_DEFAULT_PROCESSOR);
  if FAILED(hr) then
    goto done;

  // Use the CreateMasteringVoice method to create a mastering voice.
  //
  // The mastering voices encapsulates an audio device.
  // It is the ultimate destination for all audio that passes through an audio graph.
  hr := pvXAudio2.CreateMasteringVoice(@pvMasteringVoice);
  if FAILED(hr) then
    goto done;

  hr := pvXAudio2.CreateSourceVoice(@pvSourceVoice,
                                    pvWaveformatex,
                                    0);
  if FAILED(hr) then
    goto done;


  // Set up XAudio2 buffer.
  ZeroMemory(@pvXAudioBuffer,
             SizeOf(XAUDIO2_BUFFER));

  pvXAudioBuffer.AudioBytes := Length(pvBytes);
  pvXAudioBuffer.pAudioData := @pvBytes[0];
  pvXAudioBuffer.Flags := XAUDIO2_END_OF_STREAM;

  // Start source voice and submit buffer.
  hr := pvSourceVoice.SubmitSourceBuffer(@pvXAudioBuffer);
  if FAILED(hr) then
    goto done;

  // This flag should be set if we played this track before,
  // so we don't have to reinitialize the sourcereader.
  if replay then
    hr := pvSourceVoice.Start();

  if FAILED(hr) then
    goto done;

  bPlaying := True;

  // Signal we are ready to go.
  SendMessage(hwndCaller,
              WM_DATA_READY_NOTIFY,
              WPARAM(1),
              LPARAM(0));

  // While the audio is rendering, keep track of the position and samples played.

  StartMonitor();
done:
  Result := hr;
end;


// Control

function TXaudio2Engine.Start(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := pvSourceVoice.Start();
  bPlaying := True;
end;


function TXaudio2Engine.Stop(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := pvSourceVoice.Stop();

  if SUCCEEDED(Result) then
    begin
      bPlaying := False;
      // We must call this first, to stop the XAudio2 threads.
      pvXAudio2.StopEngine();
      Result := pvSourceVoice.FlushSourceBuffers();
      if SUCCEEDED(Result) then
        Result := pvSourceVoice.Discontinuity();
    end;
end;


function TXaudio2Engine.Pause(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := pvSourceVoice.Stop();
end;


function TXaudio2Engine.SetVolume(volume: Single): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := pvSourceVoice.SetVolume(volume,
                                    XAUDIO2_COMMIT_NOW);
end;


// Set the volumes for the channels.
procedure TXaudio2Engine.SetVolumes(Value: TFloatArray);
var
  aVolumes: TFloatArray;
  hr: HRESULT;  // used for debugging
  i: Integer;

begin

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  if Length(Value) <> Integer(nChannels) then
    raise EArgumentException.Create('Channel volume count must match the source');
  aVolumes := Copy(Value,0,Length(Value));

  // Set boundaries to prevent overflow or clipping
  for i := 0 to Length(aVolumes) -1 do
    begin
      if (aVolumes[i] > 224.0) then
        aVolumes[i] := 224.0;
      if (aVolumes[i] < -224.0) then
        aVolumes[i] := -224.0;
    end;

  // Set the volumes.
  hr := pvSourceVoice.SetChannelVolumes(2, // The number of channels.
                                        @aVolumes[0],
                                        XAUDIO2_COMMIT_NOW);
  if FAILED(hr) then
    ShowMessage('Setting channelvolumes failed.');
  m_VolumeChannels := aVolumes;
end;

end.
