// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: XAudio2Engine.pas
// Kind: Pascal Unit
// Release date: 30-03-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: The XAudio2 renderer class.
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
// 05/07/2024 Tony                Added threading.
// 06/08/2024                     Fixed threading issues.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
// Source: https://www.gamedev.net/articles/programming/general-and-gameplay-programming/decoding-audio-for-xaudio2-with-microsoft-media-foundation-r4280/
//         https://learn.microsoft.com/en-us/windows/win32/xaudio2/how-to--load-audio-data-files-in-xaudio2
//
// Copyright � FacctoryX
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
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {VCL}
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {XAudio2}
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPO,
  WinApi.DirectX.XAudio2.X3DAudio,
  {WinMM}
  WinApi.WinMM.MMeApi,
  Tools;

const
  WM_DATA_PROCESSED_NOTIFY = WM_APP + 100;
  WM_DATA_READY_NOTIFY = WM_APP + 101;
  WM_DATA_ENDED_NOTIFY = WM_APP + 102;

  // Minimum and maximum pitch values.
  MIN_PITCH: Single = 0.4;
  MAX_PITCH: Single = 2.0;
  // Minimum and maximum volume values.
  MIN_VOLUME: Single = 0.0;
  MAX_VOLUME: Single = 100.0;

type

  TRenderstatus =(rsStopped,
                  rsPlaying,
                  rsPauzed,
                  rsEndOfStream,
                  rsInitializing,
                  rsProcessingPassStart,
                  rsProcessingPassEnd,
                  rsDestroying,
                  rsError);

  TXaudio2Engine = class(IXAudio2VoiceCallback)

{$region 'IXAudio2VoiceCallback implementation'}

    /// <summary>Called during each processing pass for each voice,
    /// just before XAudio2 reads data from the voice's buffer queue.</summary>
    procedure OnVoiceProcessingPassStart(BytesRequired: UINT32); override; stdcall;
    /// <summary>Called just after the processing pass for the voice ends.</summary>
    procedure OnVoiceProcessingPassEnd(); override; stdcall; //
    /// <summary>Called when the voice has just finished playing a contiguous audio stream.</summary>
    procedure OnStreamEnd(); override; stdcall;
    /// <summary>Called when the voice is about to start processing a new audio buffer.</summary>
    procedure OnBufferStart(pBufferContext: Pointer); override; stdcall;
    /// <summary>Called when the voice finishes processing a buffer.</summary>
    procedure OnBufferEnd(pBufferContext: Pointer); override; stdcall;
    /// <summary>Called when the voice reaches the end position of a loop.</summary>
    procedure OnLoopEnd(pBufferContext: Pointer); override; stdcall;
    /// <summary>Called when a critical error occurs during voice processing.</summary>
    procedure OnVoiceError(pBufferContext: Pointer; Error: HRESULT); override; stdcall;

{$endregion}

  private

    pvXAudio2: IXAudio2;
    pvMasteringVoice: IXAudio2MasteringVoice;
    pvSourceVoice: IXAudio2SourceVoice;
    pvXAudioBuffer: XAUDIO2_BUFFER;
    pvBytes: TBytes;
    pvFileName: TFileName;
    pvSourceFileDuration: LONGLONG;
    pvWaveformatex: PWAVEFORMATEX;
    pvwaveformatlength: Cardinal;
    pvRenderStatus: TRenderstatus;

    hwndCaller: HWND;  // Usually the mainform.

    nChannels: UInt32;              // Holds the number of volumechannels.
    nSamplesPerSecond: UInt32;      // Playtime calculations.
    m_VolumeChannels: TFloatArray;  // Dynamic array that holds the volume per channel.

  public

    constructor Create();
    destructor Destroy(); override;
    procedure BeforeDestruction(); override;


    function LoadFile(hHwnd: HWND;
                      const audiofile: TFileName;
                      fileDuration: LONGLONG): HResult;

    function InitializeXAudio2(replay: Boolean = False): HResult;  // Initialize XAudio2 engine. Call LoadFile first!

    function Play(): HResult;
    function Stop(): HResult;
    function Pause(): HResult;

    // Set/get all channels at once.
    procedure SetVolume(aValue: Single);
    function GetVolume(): Single;

    // Volume per channel.
    procedure SetVolumes(Value: TFloatArray);
    function GetVolumes(): TFloatArray;
    procedure SetPitch(aValue: Single);

    property PlayStatus: TRenderStatus read pvRenderStatus write pvRenderStatus;
    property SoundChannels: UINT32 read nChannels;
    property VolumeChannels: TFloatArray read m_VolumeChannels write m_VolumeChannels;
  end;

implementation

uses
  System.Services.Avrt;  // may be optional?


constructor TXaudio2Engine.Create();
begin
  inherited;

end;


destructor TXaudio2Engine.Destroy();
begin

  pvMasteringVoice := nil;
  pvSourceVoice := nil;
  SafeRelease(pvXAudio2);
  inherited;
end;


procedure TXaudio2Engine.BeforeDestruction();
begin
  pvRenderStatus := rsDestroying;
  if Assigned(pvXAudio2) then
    begin
      pvXAudio2.StopEngine();
      pvSourceVoice.DestroyVoice();
      pvMasteringVoice.DestroyVoice();
    end;
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
  partialType: IMFMediaType;
  uncompressedAudioType: IMFMediaType;

  sample: IMFSample;
  flags: DWORD;
  buffer: IMFMediaBuffer;
  audioData: PByte;
  audioDataLength: DWORD;

label
  done;

begin

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
                                          nativeMediaType);

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
                                           uncompressedAudioType);
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

  // Fill the buffer. We use a thread for this, to speedup things.
  TThread.Synchronize(nil,
                      procedure
                      var
                        hres: HResult;
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

                              // To be on the safe side we check all flags for which
                              // further reading would not make any sense
                              // and set EndOfFile to True
                              if ((flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) or
                                 ((flags and MF_SOURCE_READERF_ERROR) <> 0) or
                                 ((flags and MF_SOURCE_READERF_NEWSTREAM) <> 0) or
                                 ((flags and MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) <> 0) or
                                 ((flags and MF_SOURCE_READERF_ALLEFFECTSREMOVED) <> 0) then
                              begin
                                Break;
                              end;

                              // If the sample is nil, there is a gap in the data stream that can't be filled: No reason to quit..
                              if (sample = nil) then
                                Continue;

                              if (flags = MF_SOURCE_READERF_STREAMTICK) then
                                Continue;

                              // Convert data to contiguous buffer.
                              hres := sample.ConvertToContiguousBuffer(@buffer);

                              // Lock Buffer and copy to local memory
                              hres := buffer.Lock(audioData,
                                                  nil,
                                                  @audioDataLength);

                              try
                                SetLength(pvBytes,
                                          Length(pvBytes) + Integer(audioDataLength));

                                Move(audioData^,
                                     pvBytes[Length(pvBytes) - Integer(audioDataLength)],
                                     audioDataLength);
                              finally
                                hres := buffer.Unlock();
                              end;
                              sample := nil;
                            end;
                        end);

  // Create Xaudio2 and run audio.
  if SUCCEEDED(hr) then
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
                                    0,
                                    XAUDIO2_DEFAULT_FREQ_RATIO,
                                    Self); // register Audio2VoiceCallback
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

  SendMessage(hwndCaller,
              WM_DATA_READY_NOTIFY,
              WPARAM(1),
              LPARAM(0));

  // This flag should be set if we played this track before,
  // so we don't have to reinitialize the sourcereader.
  if replay then
    hr := pvSourceVoice.Start();

  if FAILED(hr) then
    goto done;

  pvRenderStatus := rsPlaying;  // Will be set in the callback?

  // From here we could run the loop in a parallel thread or use the classic TThread.
  // NOTE: Parallel programming is introduced in Delphi XE7 and above, the latter is compatible with all versions.

  TThread.Synchronize(nil,
                      procedure
                      var
                        bReady: Boolean;
                        voiceState: XAUDIO2_VOICE_STATE;

                        begin
                          bReady := False;
                          while not bReady and ((pvRenderStatus = rsPlaying) or (pvRenderStatus = rsPauzed)) and (pvSourceVoice <> nil) do
                            begin
                              pvSourceVoice.GetState(voiceState,
                                                     0);

                              bReady := (voiceState.BuffersQueued = 0);

                              // Send score. Don't use PostMessage because it set priority above this thread.
                              SendMessage(hwndCaller,
                                          WM_DATA_PROCESSED_NOTIFY,
                                          WPARAM((voiceState.SamplesPlayed div nSamplesPerSecond) * 10000000),
                                          LPARAM(voiceState.SamplesPlayed));
                            end;
                        end);
done:
  if SUCCEEDED(hr) then
    // Signal we are done playing.
    SendMessage(hwndCaller,
                WM_DATA_ENDED_NOTIFY,
                WPARAM(1),
                LPARAM(0));

  Result := hr;
end;


// Control

function TXaudio2Engine.Play(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := pvSourceVoice.Start();

  if Succeeded(Result) then
    pvRenderStatus := rsPlaying
  else
    pvRenderStatus := rsError;
end;


function TXaudio2Engine.Stop(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    begin
      Exit(E_POINTER);
    end;

  Result := pvSourceVoice.Stop();

  if SUCCEEDED(Result) then
    begin
      // We must call this first, to stop the XAudio2 threads.
      pvXAudio2.StopEngine();
      Result := pvSourceVoice.FlushSourceBuffers();
      if SUCCEEDED(Result) then
        Result := pvSourceVoice.Discontinuity();
      if SUCCEEDED(Result) then
        pvRenderStatus := rsStopped
      else
        pvRenderStatus := rsError;
    end
  else
    pvRenderStatus := rsError;
end;


function TXaudio2Engine.Pause(): HResult;
begin
  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;
  Result := pvSourceVoice.Stop();

  if Succeeded(Result) then
    pvRenderStatus := rsPauzed
  else
    pvRenderStatus := rsError;
end;


procedure TXaudio2Engine.SetVolume(aValue: Single);
var
  hr: HResult;

begin

  if Assigned(pvSourceVoice) then
    begin
      hr := pvSourceVoice.SetVolume(aValue,
                                    XAUDIO2_COMMIT_NOW);
      if FAILED(hr) then
        raise Exception.CreateFmt('TXaudio2Engine.SetVolume failed with result %s.',
                                  [IntToHex(hr, 8) + #13,
                                  SysErrorMessage(hr)]);
    end;
end;


function TXaudio2Engine.GetVolume(): Single;
begin
  pvSourceVoice.GetVolume(Result);
end;


// Set the volumes for the channels.
procedure TXaudio2Engine.SetVolumes(Value: TFloatArray);
var
  aVolumes: TFloatArray;
  hr: HResult;
  i: Integer;

begin

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  aVolumes := Value;

  // Set boundaries to prevent overflow or clipping
  for i := 0 to Length(aVolumes) - 1 do
    begin
      if aVolumes[i] > XAUDIO2_MAX_VOLUME_LEVEL then
        aVolumes[i] := XAUDIO2_MAX_VOLUME_LEVEL;
      if aVolumes[i] < 0.0 then
        aVolumes[i] := 0.0;
    end;

  // Set the volumes.
  if Assigned(pvSourceVoice) then
    begin
      hr := pvSourceVoice.SetChannelVolumes(nChannels, // The number of channels.
                                            @aVolumes[0],
                                            XAUDIO2_COMMIT_NOW);
      if FAILED(hr) then
        raise Exception.CreateFmt('TXaudio2Engine.SetVolumes failed with result %s.',
                                  [IntToHex(hr, 8) + #13,
                                  SysErrorMessage(hr)]);
    end;
end;


// Get the volumes for the channels.
function TXaudio2Engine.GetVolumes(): TFloatArray;
begin

  pvSourceVoice.GetChannelVolumes(nChannels,
                                  @Result[0]);
end;


procedure TXaudio2Engine.SetPitch(aValue: Single);
var
  flPitch: Single;

begin

  flPitch := aValue;
  // To prevent extreme pitching causing buffer underrun.
  if (flPitch > MAX_PITCH) then
    flPitch := MAX_PITCH;
  if (flPitch < MIN_PITCH) then
    flPitch := MIN_PITCH;

  pvSourceVoice.SetFrequencyRatio(flPitch);
end;


// Callback implementation =================================================

procedure TXaudio2Engine.OnVoiceProcessingPassStart(BytesRequired: UINT32);
begin
  //pvRenderStatus := rsProcessingPassStart;
end;


procedure TXaudio2Engine.OnVoiceProcessingPassEnd();
begin
  //pvRenderStatus := rsProcessingPassEnd;
end;


procedure TXaudio2Engine.OnStreamEnd();
begin

  pvRenderStatus := rsEndOfStream;
end;


procedure TXaudio2Engine.OnBufferStart(pBufferContext: Pointer);
begin

  pvRenderStatus := rsPlaying;
end;


procedure TXaudio2Engine.OnBufferEnd(pBufferContext: Pointer);
begin

  pvRenderStatus := rsStopped;
end;


procedure TXaudio2Engine.OnLoopEnd(pBufferContext: Pointer);
begin
  // Stub.
end;


procedure TXaudio2Engine.OnVoiceError(pBufferContext: Pointer;
                                      Error: HRESULT);
begin
   pvRenderStatus := rsError;

end;

// =========================================================================

end.

