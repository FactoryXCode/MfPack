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
// Revision Version: 3.1.7
// Description: The XAudio2 renderer class.
//              Uses a memory stream instead of bytearray.
//              Runs renderer in separate thread.
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
// 31/05/2025 Tony                Fixed memory leaks and thread handling.
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
                  rsDestroying);

  TXaudio2EventData = record
    SamplesProcessed: LONGLONG;
    Position: LONGLONG;
    TimePlayed: MFTIME;
  end;

  // Voice ID's
  TEffectsOnVoices = (afxMasteringVoice,
                      afxSourceVoice);

  TXAudio2RenderThread = class;

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

  protected
    pvRenderThread: TXAudio2RenderThread;

  private

    pvXAudio2: IXAudio2;
    pvMasteringVoice: IXAudio2MasteringVoice;
    pvSourceVoice: IXAudio2SourceVoice;
    pvXAudioBuffer: XAUDIO2_BUFFER;

    // Effects
    pvFxReverbEffect: TFxReverb;
    pvFxMasterLimiter: TFxMasterLimiter;

    // Audio buffer.
    pvAudioData: PByte;
    pvMemoryStream: TMemoryStream;

    pvFileName: TFileName;

    pvSourceFileDuration: MFTIME;
    pvBufferStart: UINT32;
    pvNewBufferPosition: LONGLONG; // See also: TXaudio2EventData.Position
    pvBufferSize: UINT32; // Length of the audiobuffer (pvBytes).
    pvBufferPrevPlayed: UINT32;

    pvWaveformatex: PWAVEFORMATEX;

    pvwaveformatlength: Cardinal;
    pvRenderStatus: TRenderstatus;

    pvChannels: UInt32;              // Holds the number of volumechannels.
    pvSamplesPerSecond: UInt32;      // Playtime calculations.
    pvVolumeChannels: TFloatArray;   // Dynamic array that holds the volume per channel.
    pvReverbI3DL2ParamArray: TReverbI3DL2ParamArray;

    FXaudio2EventData: TXaudio2EventData; // Event extra data structure.

    // This value will be set to True, when user stops rendering.
    // In that case the SourceVoice will be removed from the topology.
    // Replay wil create a new sourcevoice.
    pvNeedNewSourceVoice: Boolean;

    // Class events
    FOnProcessingData: TNotifyEvent;
    FOnAudioReadyEvent: TNotifyEvent;
    FOnAudioStoppedEvent: TNotifyEvent;
    FOnAudioPlayingEvent: TNotifyEvent;
    FOnAudioPauzedEvent: TNotifyEvent;

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
    function InitAudioBuffer(playStart: UINT32 = 0;
                             playEnd: UINT32 = 0): HResult;

    procedure InitializeXAudio2Thread();
    procedure TerminateRenderThread();

  public

    constructor Create();
    destructor Destroy(); override;

    // Run this method to load the file and initialize XAudio2,
    // after this the audio is ready to play.
    function LoadAndInitialize(const audiofile: TFileName): HResult;

    // Use this method if you don't want to Initialize XAudio2.
    function LoadFile(const audiofile: TFileName): HResult;

    // Initialize XAudio2 engine. LoadFile must be called first!
    function InitializeXAudio2(replay: Boolean = False): HResult;

    // Threaded renderer.
    procedure RenderAudio();

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

    property AudioEventData: TXAudio2EventData read FXaudio2EventData;
    property AddNewSourceVoice: Boolean read pvNeedNewSourceVoice;

    // Reverb effect assignments.
    property ReverbEffectOnSourceVoice: Boolean read bReverbEffectOnSourceVoice;
    property ReverbEffectOnMasterVoice: Boolean read bReverbEffectOnMasteringVoice;

    // Class events.
    property OnProcessingData: TNotifyEvent read FOnProcessingData write FOnProcessingData;
    property OnAudioReadyEvent: TNotifyEvent read FOnAudioReadyEvent write FOnAudioReadyEvent;

    property OnAudioStoppedEvent: TNotifyEvent read FOnAudioStoppedEvent write FOnAudioStoppedEvent;
    property OnAudioPlayingEvent: TNotifyEvent read FOnAudioPlayingEvent write FOnAudioPlayingEvent;
    property OnAudioPauzedEvent: TNotifyEvent read FOnAudioPauzedEvent write FOnAudioPauzedEvent;

    // IXAudio2VoiceCallback events.
    property OnVoiceProcessingPassStartEvent: TNotifyEvent read FOnVoiceProcessingPassStartEvent write FOnVoiceProcessingPassStartEvent;
    property OnVoiceProcessingPassEndEvent: TNotifyEvent read FOnVoiceProcessingPassEndEvent write FOnVoiceProcessingPassEndEvent;
    property OnStreamEndEvent: TNotifyEvent read FOnStreamEndEvent write FOnStreamEndEvent;
    property OnBufferStartEvent: TNotifyEvent read FOnBufferStartEvent write FOnBufferStartEvent;
    property OnBufferEndEvent: TNotifyEvent read FOnBufferEndEvent write FOnBufferEndEvent;

  end;

  // The thread we render after Start.
  TXAudio2RenderThread = class(TThread)
  private
    FEngine: TXaudio2Engine;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TXaudio2Engine);
  end;


implementation


constructor TXAudio2RenderThread.Create(AEngine: TXaudio2Engine);
begin
  inherited Create(True);
  FEngine := AEngine;
  FreeOnTerminate := False;
end;

procedure TXAudio2RenderThread.Execute;
begin
  while not Terminated do
    begin
      Synchronize(nil,
                  procedure
                    begin
                      FEngine.RenderAudio;
                    end);
      Sleep(100);
    end;
end;


procedure TXaudio2Engine.InitializeXAudio2Thread;
begin
  if not Assigned(pvRenderThread) then
    begin
      pvRenderThread := TXAudio2RenderThread.Create(Self);
      pvRenderThread.Start;
    end;
end;

procedure TXaudio2Engine.TerminateRenderThread();
begin
  if Assigned(pvRenderThread) then
    begin
      pvRenderThread.SetFreeOnTerminate(True);
      pvRenderThread.Terminate;
      // Give thread time to terminate itself.
      Sleep(200);
      pvRenderThread := nil;
    end;
end;



constructor TXaudio2Engine.Create();
begin
  inherited;

  pvNeedNewSourceVoice := False;
  bReverbEffectOnSourceVoice := False;
  bReverbEffectOnMasteringVoice := False;
  pvReverbI3DL2ParamArray := GetReverbParams();
end;


destructor TXaudio2Engine.Destroy();
begin

  pvRenderStatus := rsDestroying;

  if Assigned(pvXAudio2) then
    pvXAudio2.StopEngine();

  // Remove effects.
  if Assigned(pvFxReverbEffect) then
    FreeAndNil(pvFxReverbEffect);

  if Assigned(pvFxMasterLimiter) then
    FreeAndNil(pvFxMasterLimiter);

  if Assigned(pvMasteringVoice) then
    begin
      pvMasteringVoice.DestroyVoice();
      pvMasteringVoice := nil;
    end;

  if Assigned(pvSourceVoice) then
    begin
      pvSourceVoice.DestroyVoice();
      pvSourceVoice := nil;
    end;

  ZeroMemory(@pvXAudioBuffer,
             SizeOf(XAUDIO2_BUFFER));

  if Assigned(pvRenderThread) then
    TerminateRenderThread();

  if Assigned(pvWaveformatex) then
    CoTaskMemFree(pvWaveformatex);

  if Assigned(pvMemoryStream) then
    begin
      pvMemoryStream.Clear();
      FreeAndNil(pvMemoryStream);
    end;


  if Assigned(pvXAudio2) then
    SafeRelease(pvXAudio2);

  FreeMem(pvAudioData);

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

  mfSample: IMFSample;
  uFlags: UINT32;
  mfMediaBuffer: IMFMediaBuffer;
  audioData: PByte;
  audioValidDataLength: DWord;

label
  done;

begin

  pvFileName := audiofile;
  if not FileExists(audiofile) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto done;
    end;

  // Get the length of the audiofile.
  hr := GetFileDuration(StrToPWideChar(audiofile),
                        pvSourceFileDuration);

  if Assigned(pvWaveformatex) then
    CoTaskMemFree(pvWaveformatex);

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
                                    MFAudioFormat_Float);

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
  // Empty stream if used before or create a new one.
  if Assigned(pvMemoryStream) then
    pvMemoryStream.Clear()
  else
    pvMemoryStream := TMemoryStream.Create;

  // ===========================================================================

  // Fill the buffers.
  while (hr = S_OK) do
    begin
      uFlags := 0;
      hr := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                    0,
                                    nil,
                                    @uFlags,
                                    nil,
                                    @mfSample);

      // Check for eof.
      if ((uFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
        Break;

      // If the sample is nil, there is a gap in the data stream that can't be filled: No reason to quit.
      if (mfSample = nil) then
        begin
          mfSample := nil;
          Continue;
        end
      else
        begin
          // Get the media buffer from the sample and convert to contiguous buffer.
          hr := mfSample.ConvertToContiguousBuffer(@mfMediaBuffer);

          // Get the audio data from the media buffer.
          if SUCCEEDED(hr) then
            begin
              // Lock Buffer & copy to memory stream.
              hr := mfMediaBuffer.Lock(audioData,
                                       nil,
                                       @audioValidDataLength);
              if SUCCEEDED(hr) then
                try
                  pvMemoryStream.Write(audioData^,
                                       audioValidDataLength);
                finally
                  // Unlock Buffer.
                  hr := mfMediaBuffer.Unlock;
                end;
              end;
          mfSample := nil;
        end;
  end; // while

done:
  if Assigned(mfSample) then
    SafeRelease(mfSample);
  Result := hr;
end;


function TXaudio2Engine.InitAudioBuffer(playStart: UINT32 = 0;
                                        playEnd: UINT32 = 0): HResult;
begin

  // Prevent memory leaks when re-initialize the engine.
  if (pvAudioData <> nil) then
    FreeMem(pvAudioData);

  pvMemoryStream.Position := 0;
  // Allocate memory for pvAudioData
  GetMem(pvAudioData,
         pvMemoryStream.Size);

   // Copy the contents of pvMemoryStream to pvAudioData.
   pvMemoryStream.ReadBuffer(pvAudioData^,
                             pvMemoryStream.Size);

  // Set up a new XAudio2 buffer.
  ZeroMemory(@pvXAudioBuffer,
             SizeOf(XAUDIO2_BUFFER));

  pvBufferSize := pvMemoryStream.Size;

  pvXAudioBuffer.AudioBytes := pvBufferSize;
  pvXAudioBuffer.pAudioData := pvAudioData;
  pvXAudioBuffer.Flags := XAUDIO2_END_OF_STREAM;

  pvXAudioBuffer.PlayBegin := playStart; // pvBufferStart
  pvXAudioBuffer.PlayLength := playEnd;  // Zero means to play until the end of the buffer.

  //
  // Can be extended with loop arguments.
  //

  // Start source voice and submit buffer.
  Result := pvSourceVoice.SubmitSourceBuffer(@pvXAudioBuffer);
end;


// New code running the loop in a separate thread.
function TXaudio2Engine.InitializeXAudio2(replay: Boolean = False): HResult;
var
  hr: HResult;

label
  done;

begin
  pvBufferPrevPlayed := 0;
  pvBufferStart := 0;
  pvNewBufferPosition := 0;

  FXaudio2EventData.SamplesProcessed := 0;
  FXaudio2EventData.Position := 0;
  FXaudio2EventData.TimePlayed := 0;

  // Remove active effects.
  if Assigned(pvFxReverbEffect) then
    FreeAndNil(pvFxReverbEffect);

  if Assigned(pvFxMasterLimiter) then
    FreeAndNil(pvFxMasterLimiter);

  // Use the XAudio2Create function to create an instance of the XAudio2 engine.
  // Skip this if user pressed Stop and does Replay again.
  // In that case the SourceVoice is removed from the topology.
  // The SourceVoice will be added later.
  if (pvNeedNewSourceVoice = False) then
    begin
      pvXAudio2 := nil;
      pvMasteringVoice := nil;

      hr := XAudio2Create(@pvXAudio2,
                          0,
                          XAUDIO2_USE_DEFAULT_PROCESSOR); // See comments on using XAUDIO2_USE_DEFAULT_PROCESSOR.
      if FAILED(hr) then
        goto done;

      // Use the CreateMasteringVoice method to create a mastering voice.
      //
      // The mastering voices encapsulates an audio device.
      // It is the ultimate destination for all audio that passes through an audio graph.
      hr := pvXAudio2.CreateMasteringVoice(@pvMasteringVoice);
      if FAILED(hr) then
        goto done;
    end;

  pvNeedNewSourceVoice := False;
  pvSourceVoice := nil;

  // Add SourceVoice to the topology.
  hr := pvXAudio2.CreateSourceVoice(@pvSourceVoice,
                                    pvWaveformatex,
                                    0,
                                    XAUDIO2_DEFAULT_FREQ_RATIO,
                                    Self); // register Audio2VoiceCallback
  if FAILED(hr) then
    goto done;

  // Set up XAudio2 buffer.
  hr := InitAudioBuffer();
  if FAILED(hr) then
    goto done;

  // Create effects.
  pvFxReverbEffect := TFxReverb.Create();
  pvFxMasterLimiter := TFxMasterLimiter.Create();

  // This flag should be set if we played this track before,
  // so we don't have to reinitialize the sourcereader.
  if replay then
    hr := pvSourceVoice.Start();

  if FAILED(hr) then
    goto done;

  // Setup is done, set status we are ready to go.
  if Assigned(FOnAudioReadyEvent) then
  begin
    pvRenderStatus := rsInitialized;
    pvBufferPrevPlayed := 0;
    OnAudioReadyEvent(Self);
  end;

  // Now the stream will be rendered in another thread.
  // So, we need to create another thread to keep control.
  //
  // Note that, when this audiostream is over,
  // the end of buffer will be signaled first, before signal endofstream.

  if Assigned(pvRenderThread) then
    TerminateRenderThread()
  else
    begin
      // Start the rendering loop in the separate thread.
      InitializeXAudio2Thread();
    end;

done:

  Result := hr;
end;


procedure TXaudio2Engine.RenderAudio();
var
  voiceState: XAUDIO2_VOICE_STATE;

begin

  while  pvRenderStatus in [rsPlaying, rsPauzed] do
    begin
      // This consumes some CPU load, but saves using a timer.
      pvSourceVoice.GetState(voiceState,
                             0);
      // Calculate the samples processed (Starting after a stop event has taken place).
      FXaudio2EventData.SamplesProcessed := voiceState.SamplesPlayed - pvBufferPrevPlayed;
      // Calculate the samples that have been processed, to milliseconds.
      // The caller can, for example, calculate the readable time in HH:MM:SS:MS.
      FXaudio2EventData.TimePlayed := (((INT64(voiceState.SamplesPlayed) + pvNewBufferPosition) - pvBufferPrevPlayed) div pvSamplesPerSecond) * 10000000;
      // Notify the main thread about the processing data.
      OnProcessingData(Self);
  end;
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

  if SUCCEEDED(Result) then
    begin
      pvRenderStatus := rsPlaying;

      if Assigned(FOnAudioPlayingEvent) then
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

  if SUCCEEDED(Result) then
    begin
      pvRenderStatus := rsPauzed;
      FOnAudioPauzedEvent(Self);
    end;
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
      TerminateRenderThread();
      pvRenderStatus := rsStopped;
      FOnAudioStoppedEvent(Self);
      pvNeedNewSourceVoice := True;
    end;
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
      hr := pvSourceVoice.SetChannelVolumes(pvChannels, // The number of channels.
                                            @aVolumes[0],
                                            XAUDIO2_COMMIT_NOW);
      if FAILED(hr) then
        raise Exception.CreateFmt('TXaudio2Engine.SetVolumes failed with result %s.',
                                  [IntToHex(hr, 8) + #13,
                                  SysErrorMessage(hr)]);
    end;
  HandleThreadMessages(TThread.CurrentThread.Handle);
end;


// Get the volumes for the channels.
function TXaudio2Engine.GetVolumes(): TFloatArray;
begin

  pvSourceVoice.GetChannelVolumes(pvChannels,
                                  @Result[0]);
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

  pvSourceVoice.SetFrequencyRatio(flPitch);
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
    // If the effect has been created, skip initialization.
    //if (Result = MF_E_ALREADY_INITIALIZED) then
    //  Result := pvFxReverbEffect.EnableReverbEffect(ppVoice,
    //                                                pEnable);
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

  HandleThreadMessages(TThread.CurrentThread.Handle);
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


function TXaudio2Engine.GotoNewPosition(position: MFTIME): HResult;
var
  hr: HResult;
  VoiceState: XAUDIO2_VOICE_STATE;

begin

  pvBufferStart := position;

  hr := pvSourceVoice.Stop(0,
                           XAUDIO2_COMMIT_NOW);

  if SUCCEEDED(hr) then
    hr := pvSourceVoice.FlushSourceBuffers();

  if SUCCEEDED(hr) then
    hr := pvSourceVoice.Discontinuity();

  // We have to poll for the OnBufferEnd returns the rendersatus.
  while (pvRenderStatus <> rsEndOfBuffer) do
    HandleThreadMessages(TThread.CurrentThread.Handle);

  if SUCCEEDED(hr) then
    begin
      pvSourceVoice.GetState(VoiceState);
      pvBufferPrevPlayed := VoiceState.SamplesPlayed;
      pvNewBufferPosition := pvBufferStart;
      FXaudio2EventData.Position := pvBufferStart;

      // Set up new XAudio2 buffer.
      hr := InitAudioBuffer(pvBufferStart);
    end;

  if SUCCEEDED(hr) then
    pvSourceVoice.Start(0,
                        XAUDIO2_COMMIT_NOW);
  HandleThreadMessages(TThread.CurrentThread.Handle);
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

  // For internal use.
  pvRenderStatus := rsEndOfBuffer;
  FOnBufferEndEvent(Self);
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

// =========================================================================

end.

