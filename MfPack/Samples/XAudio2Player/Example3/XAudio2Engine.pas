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
// Revision Version: 3.1.9
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
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {VCL}
  Vcl.Dialogs,
  Vcl.Forms,
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
  WinApi.DirectX.XAudio2.XAudio2Fx,
  WinApi.DirectX.XAudio2.XAPOFx,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Application}
  Tools,
  XAudio2_FXReverb,
  XAudio2_FXMasterLimiter;

const
  // Minimum and maximum pitch values.
  MIN_PITCH = 0.4;
  MAX_PITCH = 2.0;
  // Minimum and maximum volume values.
  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;

  STREAM_BUFFER_COUNT = 3;
  STREAM_BUFFER_MS = 100; // 100ms chunks.

type

  TRenderstatus =(rsStopped,
                  rsPlaying,
                  rsPauzed,
                  rsEndOfBuffer,
                  rsEndOfStream,
                  rsInitializing,
                  rsInitialized,
                  rsProcessingPassStart,
                  rsProcessingPassEnd,
                  rsDestroying,
                  rsLoopLeft,
                  rsError);

  PXaudio2EventData = ^TXaudio2EventData;
  TXaudio2EventData = record
    SamplesProcessed: LONGLONG;
    Position: LONGLONG;
    TimePlayed: MFTIME;

    procedure Reset();
  end;

  TStreamBuf = record
    Data: PByte;
    CapBytes: Cardinal;
    UsedBytes: Cardinal;
    XA: XAUDIO2_BUFFER;
  end;

  // Voice ID's
  TEffectsOnVoices = (afxMasteringVoice,
                      afxSourceVoice);

  TXaudio2Engine = class(IXAudio2VoiceCallback)

{$region 'IXAudio2VoiceCallback implementation'}

    /// <summary>Called during each processing pass for each voice,
    /// just before XAudio2 reads data from the voice's buffer queue.</summary>
    procedure OnVoiceProcessingPassStart(BytesRequired: UINT32); override; stdcall;
    /// <summary>Called just after the processing pass for the voice ends.</summary>
    procedure OnVoiceProcessingPassEnd(); override; stdcall;
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

    FLock: TCriticalSection;

    pvXAudio2: IXAudio2;
    pvMasteringVoice: IXAudio2MasteringVoice;
    pvSourceVoice: IXAudio2SourceVoice;

    // Effects
    pvFxReverbEffect: TFxReverb;
    pvFxMasterLimiter: TFxMasterLimiter;

    // Audio buffer.
    pvMemoryStream: TMemoryStream;

    // Media Foundation streaming decode
    FReader: IMFSourceReader;
    FReaderAttr: IMFAttributes;
    FReachedEof: Boolean;

    // Streaming buffer ring
    FStreamBuf: array[0..STREAM_BUFFER_COUNT -1] of TStreamBuf;
    FNextSubmit: Integer;

    pvFileName: TFileName;

    pvSourceFileDuration: MFTIME;
    pvBufferStart: UINT32;
    pvNewBufferPosition: LONGLONG; // See also: TXaudio2EventData.Position
    pvBufferPrevPlayed: UINT32;

    pvWaveformatex: PWAVEFORMATEX;

    pvwaveformatlength: Cardinal;
    pvRenderStatus: TRenderstatus;

    pvChannels: UInt32;              // Holds the number of volumechannels.
    pvSamplesPerSecond: UInt32;      // Playtime calculations.
    pvVolumeChannels: TFloatArray;   // Dynamic array that holds the volume per channel.
    pvReverbI3DL2ParamArray: TReverbI3DL2ParamArray;

    pvXaudio2EventData: TXaudio2EventData; // Event extra data structure.

    // buffer initialization members.
    pvAudioData: PByte;  // Audio buffer.

    // Playback bookkeeping
    pvSeekBaseSamples: Int64;   // samples offset at last seek (GotoNewPosition)

    pvXAudioBuffer: XAUDIO2_BUFFER;

    // Class events
    FOnProcessingData: TNotifyEvent;
    FOnAudioReadyEvent: TNotifyEvent;
    FOnAudioStoppedEvent: TNotifyEvent;
    FOnAudioPlayingEvent: TNotifyEvent;
    FOnAudioPauzedEvent: TNotifyEvent;
    FOnAudioDataLoopLeft: TNotifyEvent;

    // Events from callback.
    FOnVoiceProcessingPassStartEvent: TNotifyEvent;
    FOnVoiceProcessingPassEndEvent: TNotifyEvent;
    FOnStreamEndEvent: TNotifyEvent;
    FOnBufferStartEvent: TNotifyEvent;
    FOnBufferEndEvent: TNotifyEvent;

    // Reverb
    bReverbEffectOnSourceVoice: Boolean;
    bReverbEffectOnMasteringVoice: Boolean;

    // Initializes a new audiobuffer.
    function InitAudioBuffer(pPlayStart: UINT32 = 0;
                             pPlayEnd: UINT32 = 0): HResult;

  public

    FXaudio2EventData: TXaudio2EventData;

    constructor Create();
    destructor Destroy(); override;

    // Run this method to load the file and initialize XAudio2.
    function LoadAndInitialize(const audiofile: TFileName): HResult;

    // Use this method if you don't want to Initialize XAudio2.
    function LoadFile(const audiofile: TFileName): HResult;

    // Initialize XAudio2 engine. LoadFile must be called first!
    function InitializeXAudio2(replay: Boolean = False): HResult;

    function Play(): HResult;
    function Pause(): HResult;
    function Stop(): HResult;

    // Set/get all channels at once.
    procedure SetVolume(aValue: Single);
    function GetVolume(): Single;

    // Volume per channel.
    procedure SetVolumes(Value: TFloatArray);
    function GetVolumes(): TFloatArray;
    // Pitch.
    procedure SetPitch(aValue: Single);
    // Reverb.
    function SetReverb(Voice: TEffectsOnVoices;
                       pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                       pEnable: Boolean): HResult;
    // Masterlimiter
    function SetMasterLimiter(MasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS): HResult;

    // Sets the new startposition in the audiobuffer.
    // Note that position must contains the number of samples!
    function GotoNewPosition(position: MFTIME): HResult;

    property PlayStatus: TRenderStatus read pvRenderStatus;
    property SoundChannels: UINT32 read pvChannels;
    property VolumeChannels: TFloatArray read pvVolumeChannels;
    property Duration: MFTIME read pvSourceFileDuration;
    property SamplesPerSec: UINT32 read pvSamplesPerSecond;
    property ReverbParameters: TReverbI3DL2ParamArray read pvReverbI3DL2ParamArray;

    property AudioEventData: TXAudio2EventData read pvXaudio2EventData;

    // Reverb effect assignments.
    property ReverbEffectOnSourceVoice: Boolean read bReverbEffectOnSourceVoice;
    property ReverbEffectOnMasterVoice: Boolean read bReverbEffectOnMasteringVoice;

    // Class events.
    property OnProcessingData: TNotifyEvent read FOnProcessingData write FOnProcessingData;
    property OnAudioReadyEvent: TNotifyEvent read FOnAudioReadyEvent write FOnAudioReadyEvent;

    property OnAudioStoppedEvent: TNotifyEvent read FOnAudioStoppedEvent write FOnAudioStoppedEvent;
    property OnAudioPlayingEvent: TNotifyEvent read FOnAudioPlayingEvent write FOnAudioPlayingEvent;
    property OnAudioPauzedEvent: TNotifyEvent read FOnAudioPauzedEvent write FOnAudioPauzedEvent;
    property OnAudioDataLoopLeft: TNotifyEvent read FOnAudioDataLoopLeft write FOnAudioDataLoopLeft;

    // IXAudio2VoiceCallback events.
    property OnVoiceProcessingPassStartEvent: TNotifyEvent read FOnVoiceProcessingPassStartEvent write FOnVoiceProcessingPassStartEvent;
    property OnVoiceProcessingPassEndEvent: TNotifyEvent read FOnVoiceProcessingPassEndEvent write FOnVoiceProcessingPassEndEvent;
    property OnStreamEndEvent: TNotifyEvent read FOnStreamEndEvent write FOnStreamEndEvent;
    property OnBufferStartEvent: TNotifyEvent read FOnBufferStartEvent write FOnBufferStartEvent;
    property OnBufferEndEvent: TNotifyEvent read FOnBufferEndEvent write FOnBufferEndEvent;

  end;


implementation


constructor TXaudio2Engine.Create();
begin
  inherited;

  bReverbEffectOnSourceVoice := False;
  bReverbEffectOnMasteringVoice := False;
  pvReverbI3DL2ParamArray := GetReverbParams();

  pvSeekBaseSamples := 0;

  FLock := TCriticalSection.Create();

  // Create effects.
  pvFxReverbEffect := TFxReverb.Create();
  pvFxMasterLimiter := TFxMasterLimiter.Create();

  FXaudio2EventData.Reset();
end;


destructor TXaudio2Engine.Destroy();
begin

  pvRenderStatus := rsDestroying;

  try

    FLock.Acquire();
    try

      if Assigned(pvSourceVoice) then
        begin

          pvSourceVoice.Stop(0,
                             XAUDIO2_COMMIT_NOW);

          pvSourceVoice.FlushSourceBuffers();
          pvSourceVoice.DestroyVoice();
          pvSourceVoice := nil;
        end;

      if Assigned(pvMasteringVoice) then
        begin

          pvMasteringVoice.DestroyVoice();
          pvMasteringVoice := nil;
        end;

      if Assigned(pvXAudio2) then
        SafeRelease(pvXAudio2);

    finally

      FLock.Release();
    end;

  except
    // Do nothing.
  end;

  // Remove effects.
  if Assigned(pvFxReverbEffect) then
    FreeAndNil(pvFxReverbEffect);

  if Assigned(pvFxMasterLimiter) then
    FreeAndNil(pvFxMasterLimiter);

  ZeroMemory(@pvXAudioBuffer,
             SizeOf(XAUDIO2_BUFFER));

  if Assigned(pvWaveformatex) then
    CoTaskMemFree(pvWaveformatex);

  if Assigned(pvMemoryStream) then
    FreeAndNil(pvMemoryStream);

  if Assigned(pvAudioData) then
    FreeMem(pvAudioData);

  FreeAndNil(FLock);

  inherited;
end;


function TXaudio2Engine.LoadAndInitialize(const audiofile: TFileName): HResult;
begin

  Result := LoadFile(audiofile);

  if SUCCEEDED(Result) then
    Result := InitializeXAudio2();
end;


function TXaudio2Engine.LoadFile(const audiofile: TFileName): HResult;
var
  hr: HResult;
  sourceReaderConfiguration: IMFAttributes;
  sourceReader: IMFSourceReader;
  nativeMediaType: IMFMediaType;
  majorType: TGUID;
  subType: TGUID;
  partialType: IMFMediaType;
  uncompressedAudioType: IMFMediaType;

begin

  pvFileName := audiofile;
  if not FileExists(audiofile) then
    Exit(ERROR_FILE_NOT_FOUND);


  // Get the length of the audiofile.
  hr := GetFileDuration(StrToPWideChar(audiofile),
                        pvSourceFileDuration);

  if SUCCEEDED(hr) then
    // Create Attribute Store.
    hr := MFCreateAttributes(sourceReaderConfiguration,
                             1);

  if SUCCEEDED(hr) then
    hr := sourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                              UInt32(1));

  // Create Source Reader.
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
       Exit(E_UNEXPECTED);
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
        // Inform the SourceReader we want uncompressed data.
        // This causes it to look for decoders to perform the request we are making.
        hr := MFCreateMediaType(partialType);

        // We want Audio
        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_MAJOR_TYPE,
                                    MFMediaType_Audio);


        // We need uncompressed data.
        // The subtype must be set to MFAudioFormat_Float if you want to use XAPO effects.
        if SUCCEEDED(hr) then
          hr := partialType.SetGUID(MF_MT_SUBTYPE,
                                    MFAudioFormat_Float {MFAudioFormat_PCM});

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
                                              pvwaveformatlength,
                                              MFWaveFormatExConvertFlag_ForceExtensible);

  // Get the number of channels.
  if SUCCEEDED(hr) then
    pvChannels := MFGetAttributeUINT32(uncompressedAudioType,
                                       MF_MT_AUDIO_NUM_CHANNELS,
                                       UINT32(2));
  // Set the channel array.
  SetLength(pvVolumeChannels,
            pvChannels);

  // Get Samples Per Second for time calculations.
  if SUCCEEDED(hr) then
    pvSamplesPerSecond := MFGetAttributeUINT32(uncompressedAudioType,
                                               MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                               UINT32(0));

  // Create a memorystream.
  if not Assigned(pvMemoryStream) then
    pvMemoryStream := TMemoryStream.Create()
  else
    pvMemoryStream.Clear();

  // Fill the buffers.
  TThread.Synchronize(nil,
                      procedure
                      var
                        hres: HResult;
                        mfSample: IMFSample;
                        uFlags: UINT32;
                        mfMediaBuffer: IMFMediaBuffer;
                        audioData: PByte;
                        audioValidDataLength: DWord;

                        begin

                          while (hr = S_OK) do
                            begin

                              uFlags := 0;
                              hres := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                                              0,
                                                              nil,
                                                              @uFlags,
                                                              nil,
                                                              @mfSample);

                              if FAILED(hres) then
                                Break;

                              // Check for eof.
                              if ((uFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
                                Break;

                              // If the sample is nil, there is a gap in the data stream that can't be filled: No reason to quit.
                              if (mfSample = nil) then
                                Continue
                              else
                                begin
                                  // Get the media buffer from the sample.
                                  hres := mfSample.ConvertToContiguousBuffer(@mfMediaBuffer);

                                  if SUCCEEDED(hres) then
                                    // Get the audio data from the media buffer.
                                    // Lock Buffer & copy to memory stream
                                    hres := mfMediaBuffer.Lock(audioData,
                                                               nil,
                                                               @audioValidDataLength);

                                  if SUCCEEDED(hres) then
                                    try
                                      // Write data to stream.
                                      pvMemoryStream.Write(audioData^,
                                                           audioValidDataLength);

                                    finally
                                      // Unlock Buffer
                                      hres := mfMediaBuffer.Unlock;
                                    end;
                                end;
                              mfSample := nil;
                              if FAILED(hres) then
                                Exit;
                            end; // while
                        end);  // proc

  Result := hr;
end;


function TXaudio2Engine.InitAudioBuffer(pPlayStart: UINT32 = 0;
                                        pPlayEnd: UINT32 = 0): HResult;
var
  xaBuffer: XAUDIO2_BUFFER;
  bytes64: Int64;
  pData: Pointer;

begin

  if not Assigned(pvMemoryStream) then
    Exit(E_POINTER);

  pvMemoryStream.Position := 0;

  if Assigned(pvAudioData) then
    begin

      FreeMem(pvAudioData);
      pvAudioData := nil;
    end;

   bytes64 := pvMemoryStream.Size;
   if (bytes64 <= 0) then
    Exit(E_FAIL);

  // dcc32 is 32-bit: XAudio2_BUFFER.AudioBytes is UINT32 -> must fit
  if (bytes64 > High(Cardinal)) then
    Exit(E_OUTOFMEMORY); // Too large for a single XAudio2 buffer anyway.

  pData := pvMemoryStream.Memory;
  if (pData = nil) then
    Exit(E_FAIL);

  if not Assigned(pvSourceVoice) then
    Exit(E_POINTER);

  FillChar(xaBuffer,
           SizeOf(xaBuffer),
           0);

  xaBuffer.AudioBytes := Cardinal(bytes64);
  xaBuffer.pAudioData := PByte(pData);
  xaBuffer.Flags := XAUDIO2_END_OF_STREAM;

  pvXAudioBuffer.PlayBegin := pPlayStart; // pvBufferStart
  pvXAudioBuffer.PlayLength := pPlayEnd;  // Zero means to play until the end of the buffer.
  // Can be extended with loop parameters.

  Result := pvSourceVoice.SubmitSourceBuffer(@xaBuffer);
end;


function TXaudio2Engine.InitializeXAudio2(replay: Boolean = False): HResult;
var
  hr: HResult;

begin

  pvBufferPrevPlayed := 0;
  pvNewBufferPosition := 0;
  pvXaudio2EventData.Reset();

  // Use the XAudio2Create function to create an instance of the XAudio2 engine.
  hr := XAudio2Create(@pvXAudio2,
                      XAUDIO2_STOP_ENGINE_WHEN_IDLE, // Must do, because otherwise we have to implement XAudio2.Stop/Start with every voice stop/start. Otherwise we will have hard to find issues with the XAudio threads.
                      XAUDIO2_USE_DEFAULT_PROCESSOR); // See comments on using XAUDIO2_USE_DEFAULT_PROCESSOR.
  if FAILED(hr) then
    Exit(hr);

  // Use the CreateMasteringVoice method to create a mastering voice.
  //
  // The mastering voices encapsulates an audio device.
  // It is the ultimate destination for all audio that passes through an audio graph.
  hr := pvXAudio2.CreateMasteringVoice(@pvMasteringVoice);
  if FAILED(hr) then
    Exit(hr);

  // Add SourceVoice to the topology.
  hr := pvXAudio2.CreateSourceVoice(@pvSourceVoice,
                                    pvWaveformatex,
                                    0,
                                    XAUDIO2_DEFAULT_FREQ_RATIO,
                                    Self); // register Audio2VoiceCallback
  if FAILED(hr) then
    Exit(hr);

  // Set up XAudio2 buffer.
  hr := InitAudioBuffer();
  if FAILED(hr) then
    Exit(hr);


  // This flag should be set if we played this track before,
  // so we don't have to reinitialize the sourcereader.
  if replay then
    begin

      if Assigned(pvXAudio2) then
        hr := pvXAudio2.StartEngine();
      if FAILED(hr) then
        Exit(hr);

      hr := pvSourceVoice.Start();
      if FAILED(hr) then
        Exit(hr);
    end;

  // Setup is done, set status we are ready to go.
  pvRenderStatus := rsInitialized;
  pvBufferPrevPlayed := 0;

  // We are ready.
  OnAudioReadyEvent(Self);

  // Now the stream will be rendered in another thread.
  // So, we need to stay in this thread to keep control.
  //
  // Note that, when this audiostream is over,
  // the end of buffer will be signaled first, before endofstream.

  TThread.Synchronize(nil,
                      procedure
                      var
                        bBuffersQueued: Boolean;
                        voicestate: XAUDIO2_VOICE_STATE;

                        begin
                          bBuffersQueued := False;

                          while not bBuffersQueued and (pvSourceVoice <> nil) do
                            begin
                              if (pvRenderStatus = rsPlaying) then
                                begin
                                  // This consumes some cpu load, but saves using a timer.
                                  pvSourceVoice.GetState(voiceState,
                                                         0);
                                  bBuffersQueued := (voiceState.BuffersQueued = 0);
                                  // Calculate the samples processed (Starting after a stop event has taken place).
                                  pvXaudio2EventData.SamplesProcessed := voiceState.SamplesPlayed - pvBufferPrevPlayed;

                                  // Calculate the samples that have been processed, to milliseconds.
                                  // The caller can, for example, calculate the readable time in HH:MM:SS:MS.
                                  pvXaudio2EventData.TimePlayed := (((voiceState.SamplesPlayed + UINT64(pvNewBufferPosition)) - pvBufferPrevPlayed) div pvSamplesPerSecond) * 10000000;
                                  pvXaudio2EventData.Position := pvBufferStart;
                                end;

                              // Send score event.
                              OnProcessingData(Self);
                           end;
                        end);
  // Client have to handle this event, for instance to check if the XAusio engine has left it's loop.
  FOnAudioDataLoopLeft(Self);
  pvRenderStatus := rsLoopLeft;
  Result := hr;
end;


// Control

function TXaudio2Engine.Play(): HResult;
begin

  Result := E_FAIL;

  if not Assigned(pvSourceVoice) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if Assigned(pvXAudio2) then
    Result := pvXAudio2.StartEngine;
  if FAILED(Result) then
    Exit;

  Result := pvSourceVoice.Start();

  if SUCCEEDED(Result) then
    begin
      pvRenderStatus := rsPlaying;
      FOnAudioPlayingEvent(Self);
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
  // Do NOT stop the XAudio2 engine!

  if SUCCEEDED(Result) then
    begin

      pvRenderStatus := rsPauzed;
      FOnAudioPauzedEvent(Self);
    end;
end;


function TXaudio2Engine.Stop(): HResult;
begin

  if not Assigned(pvSourceVoice) then
    Exit(E_POINTER);

  Result := pvSourceVoice.Stop();

  if SUCCEEDED(Result) then
    begin

      Result := pvSourceVoice.FlushSourceBuffers();

      if SUCCEEDED(Result) then
        Result := pvSourceVoice.Discontinuity();

      if SUCCEEDED(Result) then
        begin
          pvRenderStatus := rsStopped;
          FOnAudioStoppedEvent(Self);
        end
      else
        pvRenderStatus := rsError;
    end
  else
    pvRenderStatus := rsError;
end;


procedure TXaudio2Engine.SetVolume(aValue: Single);
var
  hr: HResult;

begin

  FLock.Acquire();

  try
    if Assigned(pvSourceVoice) then
      begin

        hr := pvSourceVoice.SetVolume(aValue,
                                      XAUDIO2_COMMIT_NOW);
        if FAILED(hr) then
          raise Exception.CreateFmt('TXaudio2Engine.SetVolume failed with result %s.',
                                    [IntToHex(hr, 8) + #13,
                                    SysErrorMessage(hr)]);
    end;
  finally
    FLock.Release();
  end;
end;


function TXaudio2Engine.GetVolume(): Single;
begin

  FLock.Acquire();

  try
    if Assigned(pvSourceVoice) then
      pvSourceVoice.GetVolume(Result);
  finally
    FLock.Release();
  end;
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

  FLock.Acquire();

  try

    aVolumes := Value;

    // Set boundaries to prevent overflow or clipping.
    // Note: We don't use negative values that inverts the audio's phase.
    //       See: https://learn.microsoft.com/en-us/windows/win32/xaudio2/xaudio2-volume-and-pitch-control
    for i := 0 to Length(aVolumes) - 1 do
      begin
        if (aVolumes[i] > MAX_VOLUME) then
          aVolumes[i] := MAX_VOLUME;
        if (aVolumes[i] < MIN_VOLUME) then
          aVolumes[i] := MIN_VOLUME;
      end;

    // Set the volumes.
    if Assigned(pvSourceVoice) then
      begin
        hr := pvSourceVoice.SetChannelVolumes(pvChannels, // The number of channels.
                                              @aVolumes[0],
                                              XAUDIO2_COMMIT_ALL{XAUDIO2_COMMIT_NOW});
        if FAILED(hr) then
          raise Exception.CreateFmt('TXaudio2Engine.SetVolumes failed with result %s.',
                                    [IntToHex(hr, 8) + #13,
                                    SysErrorMessage(hr)]);
      end;
  finally
    FLock.Release();
  end;
end;


// Get the volumes for the channels.
function TXaudio2Engine.GetVolumes(): TFloatArray;
begin

  FLock.Acquire();
  try

    pvSourceVoice.GetChannelVolumes(pvChannels,
                                    @Result[0]);
  finally
    FLock.Release();
  end;
end;


procedure TXaudio2Engine.SetPitch(aValue: Single);
var
  flPitch: Single;

begin

  if not Assigned(pvSourceVoice) then
    Exit;

  flPitch := aValue;
  // To prevent extreme pitching causing buffer underrun.
  if (flPitch > MAX_PITCH) then
    flPitch := MAX_PITCH;
  if (flPitch < MIN_PITCH) then
    flPitch := MIN_PITCH;

  FLock.Acquire();

  try

    if Assigned(pvSourceVoice) then
      pvSourceVoice.SetFrequencyRatio(flPitch);
  finally
    FLock.Release();
  end;
end;


function TXaudio2Engine.SetReverb(Voice: TEffectsOnVoices;
                                  pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                                  pEnable: Boolean): HResult;
var
  ppVoice: PIXAudio2Voice;

begin
  Result := E_FAIL;

  if not Assigned(pvSourceVoice) or not Assigned(pvMasteringVoice) then
    Exit;

  // == Add reverb effect. ======================================
  if (Voice = afxSourceVoice) then
    ppVoice := @pvSourceVoice
  else
    ppVoice := @pvMasteringVoice;

  Result := pvFxReverbEffect.CreateNativeReverbEffect(ppVoice,
                                                      pReverbParams,
                                                      pvChannels,
                                                      pEnable);

  if SUCCEEDED(Result) then
    begin
      if pEnable then
        Result := ppVoice.EnableEffect(0,
                                       XAUDIO2_COMMIT_NOW)
      else
        Result := ppVoice.DisableEffect(0,
                                        XAUDIO2_COMMIT_NOW);
        pvFxReverbEffect.FxReverbEffectEnabled := SUCCEEDED(Result);
    end;

  if SUCCEEDED(Result) then
    begin
      if (Voice = afxSourceVoice) then
        bReverbEffectOnSourceVoice := pEnable
      else
        bReverbEffectOnMasteringVoice := pEnable;
    end;
end;


function TXaudio2Engine.SetMasterLimiter(MasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS): HResult;
begin

  if not Assigned(pvFxMasterLimiter) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := pvFxMasterLimiter.CreateMasterLimiter(MasterLimiterparams);
end;

{
function TXaudio2Engine.GotoNewPosition(position: MFTIME): HResult;
var
  hr: HResult;
  VoiceState: XAUDIO2_VOICE_STATE;

begin

  FLock.Acquire();

  try

    if (pvWaveformatex = nil) or
       (pvAudioData = nil) or
       (pvSamplesPerSecond = 0) then
      Exit(E_POINTER);

    hr := pvSourceVoice.Stop();

    if SUCCEEDED(hr) then
      hr := pvSourceVoice.FlushSourceBuffers();

    if SUCCEEDED(hr) then
      hr := pvSourceVoice.Discontinuity();

    if SUCCEEDED(hr) then
      begin

        pvBufferStart := position;

        pvSourceVoice.GetState(VoiceState);
        pvBufferPrevPlayed := VoiceState.SamplesPlayed;
        pvNewBufferPosition := pvBufferStart;
        pvXaudio2EventData.Position := pvBufferStart;

        // Set up new XAudio2 buffer.
        hr := InitAudioBuffer(pvBufferStart);

        if SUCCEEDED(hr) then
          hr := pvSourceVoice.Start();
      end;

  finally
    FLock.Release();
  end;

  Result := hr;
end;
}

function TXaudio2Engine.GotoNewPosition(position: MFTIME): HResult;
var
  hr: HResult;
  wasPlaying: Boolean;
  bytesTotal: UInt64;
  offsetSamples: UInt64;
  offsetBytes: UInt64;
  blockAlign: UInt32;
  xaBuffer: XAUDIO2_BUFFER;

begin

  FLock.Acquire();

  try
    if (pvWaveformatex = nil) or
       (pvSamplesPerSecond = 0) then
      Exit(E_POINTER);

    if (pvMemoryStream = nil) or
       (pvMemoryStream.Memory = nil) or
       (pvMemoryStream.Size <= 0) then
      Exit(E_POINTER);

    if (pvSourceVoice = nil) then
      Exit(E_POINTER);

    blockAlign := pvWaveformatex.nBlockAlign;
    if blockAlign = 0 then
      Exit(E_FAIL);

    bytesTotal := UInt64(pvMemoryStream.Size);

    // Convert MFTIME (100ns units) -> sample offset
    // samples = position * samplesPerSecond / 10,000,000
    if position <= 0 then
      offsetSamples := 0
    else
      offsetSamples := (UInt64(position) * UInt64(pvSamplesPerSecond)) div 10000000;

    offsetBytes := offsetSamples * UInt64(blockAlign);

    // Clamp to stream
    if (offsetBytes >= bytesTotal) then
      offsetBytes := bytesTotal;

    wasPlaying := (pvRenderStatus = rsPlaying);

    // Stop and clear current buffers
    hr := pvSourceVoice.Stop(0,
                             XAUDIO2_COMMIT_NOW);

    if SUCCEEDED(hr) then
      pvSourceVoice.FlushSourceBuffers();

    if SUCCEEDED(hr) then
      pvSourceVoice.Discontinuity();

    if FAILED(hr) then
      Exit(hr);

    // Submit a new buffer starting at offsetBytes
    FillChar(xaBuffer,
             SizeOf(xaBuffer),
             0);

    xaBuffer.pAudioData := PByte(NativeUInt(pvMemoryStream.Memory) + NativeUInt(offsetBytes));
    xaBuffer.AudioBytes := Cardinal(bytesTotal - offsetBytes);
    xaBuffer.Flags := XAUDIO2_END_OF_STREAM;

    hr := pvSourceVoice.SubmitSourceBuffer(@xaBuffer);
    if FAILED(hr) then
      Exit(hr);

    // Update base for UI/progress calculations:
    // - base samples = offsetSamples
    // - base time = position (already 100ns)
    pvSeekBaseSamples := Int64(offsetSamples);

    FXaudio2EventData.Position := pvSeekBaseSamples;
    FXaudio2EventData.SamplesProcessed := 0;
    FXaudio2EventData.TimePlayed := position;

    // Restart only if we were playing before seek
    if wasPlaying then
    begin

      hr := pvSourceVoice.Start(0,
                                XAUDIO2_COMMIT_NOW);
      if SUCCEEDED(hr) then
        pvRenderStatus := rsPlaying;
    end
    else
      pvRenderStatus := rsPauzed;

  finally

    FLock.Release();
  end;

  Result := hr;
end;


// Callback implementation =================================================

procedure TXaudio2Engine.OnVoiceProcessingPassStart(BytesRequired: UINT32);
begin
  // Not used.
  // pvRenderStatus := rsProcessingPassStart;
  FOnVoiceProcessingPassStartEvent(Self);
end;


procedure TXaudio2Engine.OnVoiceProcessingPassEnd();
begin
  // Not used.
  // pvRenderStatus := rsProcessingPassEnd;
  FOnVoiceProcessingPassEndEvent(Self);
end;

// OnStreanEnd will be triggered when OnBufferEnd signals and no further buffers are available.
// In short: If the last buffer has been rendered, this event will be triggered.
procedure TXaudio2Engine.OnStreamEnd();
begin

  // For internal use.
  pvRenderStatus := rsEndOfStream;
  FOnStreamEndEvent(Self);
end;


procedure TXaudio2Engine.OnBufferStart(pBufferContext: Pointer);
begin

  // For internal use.
  pvRenderStatus := rsPlaying;
  FOnBufferStartEvent(Self);
 end;


// OnBufferEnd is the first event triggered when a buffer has been finished.
procedure TXaudio2Engine.OnBufferEnd(pBufferContext: Pointer);
begin
  // Stub.
end;


procedure TXaudio2Engine.OnLoopEnd(pBufferContext: Pointer);
begin
  // Stub.
end;


procedure TXaudio2Engine.OnVoiceError(pBufferContext: Pointer;
                                      Error: HRESULT);
begin
  // Stub.
end;

// =============================================================================

procedure TXaudio2EventData.Reset();
begin
  SamplesProcessed := 0;
  Position := 0;
  TimePlayed := 0;
end;

end.


