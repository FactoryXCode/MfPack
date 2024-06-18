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
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
  WinApi.WinMM.MMReg,
  Tools,
  XAudio2_FXReverb,
  XAudio2_FXMasterLimiter;

const
  // Minimum and maximum pitch values.
  MIN_PITCH = 0.4;
  MAX_PITCH = 2.0;

  WM_DATA_PROCESSING_NOTIFY    = WM_APP + 1000;
  WM_DATA_READY_NOTIFY         = WM_APP + 1001;
  WM_DATA_PLAYING_NOTIFY       = WM_APP + 1002;
  WM_DATA_PAUZED_NOTIFY        = WM_APP + 1003;
  WM_DATA_END_OF_BUFFER_NOTIFY = WM_APP + 1004;
  WM_DATA_START_NOTIFY         = WM_APP + 1005;
  WM_DATA_STOPPED_NOTIFY       = WM_APP + 1006;
  WM_DATA_END_OF_STREAM_NOTIFY = WM_APP + 1007;
  WM_DATA_INITIALIZING_NOTIFY  = WM_APP + 1008;
  WM_DATA_INITIALIZED_NOTIFY   = WM_APP + 1009;
  WM_DATA_DESTROYING_NOTIFY    = WM_APP + 1010;


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

  PXaudio2EventData = ^TXaudio2EventData;
  TXaudio2EventData = record
    SamplesProcessed: LONGLONG;
    Position: LONGLONG;
    TimePlayed: MFTIME;
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
    pvHwndCaller: THandle;
    FCriticalSection: TCriticalSection;
    pvXAudio2: IXAudio2;
    pvMasteringVoice: IXAudio2MasteringVoice;
    pvSourceVoice: IXAudio2SourceVoice;


    // Effects
    pvFxReverbEffect: TFxReverb;
    pvFxMasterLimiter: TFxMasterLimiter;

    // Audio buffer.
    pvMemoryStream: TMemoryStream;

    pvFileName: TFileName;

    pvSourceFileDuration: MFTIME;
    pvBufferStart: UINT32;
    pvNewBufferPosition: UINT64; // See also: TXaudio2EventData.Position
    //pvBufferSize: UINT32; // Length of the audiobuffer (pvBytes).
    pvBufferPrevPlayed: UINT32;

    pvWaveformatex: PWAVEFORMATEX;

    pvwaveformatlength: Cardinal;
    pvRenderStatus: TRenderstatus;

    pvChannels: UInt32;              // Holds the number of volumechannels.
    pvSamplesPerSecond: UINT64; //UInt32;      // Playtime calculations.
    pvVolumeChannels: TFloatArray;   // Dynamic array that holds the volume per channel.
    pvReverbI3DL2ParamArray: TReverbI3DL2ParamArray;

    FXaudio2EventData: TXaudio2EventData; // Event extra data structure.

    // This value will be set to True, when user stops rendering.
    // In that case the SourceVoice will be removed from the topology.
    // Replay wil create a new sourcevoice.
    NeedNewSourceVoice: Boolean;

    // Reverb
    bReverbEffectOnSourceVoice: Boolean;
    bReverbEffectOnMasteringVoice: Boolean;

    // Initializes a new audiobuffer.
    function InitAudioBuffer(pPlayStart: UINT32 = 0;
                             pPlayEnd: UINT32 = 0): HResult;

  public

    constructor Create();
    destructor Destroy(); override;

    // Run this method to load the file, initialize XAudio2 and play..
    function LoadAndPlay(const pHwndCaller: THandle;
                         const pAudiofile: TFileName): HResult;

    // Use this method if you don't want to Initialize XAudio2.
    function LoadFile(const pHwndCaller: THandle;
                      const pAudiofile: TFileName): HResult;

    // Initialize XAudio2 engine. LoadFile must be called first!
    function InitializeXAudio2(pDoPlay: Boolean = False): HResult;

    function Play(): HResult;
    function Pause(): HResult;
    function Stop(): HResult;

    // Set/get all channels at once.
    procedure SetVolume(pVolume: Single);
    function GetVolume(): Single;

    // Volume per channel.
    procedure SetVolumes(pVolumes: TFloatArray);
    function GetVolumes(): TFloatArray;
    // Pitch.
    procedure SetPitch(pPitch: Single);
    // Reverb.
    function SetReverb(pVoiceEffects: TEffectsOnVoices;
                       pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                       pEnable: Boolean): HResult;
    // Masterlimiter
    function SetMasterLimiter(MasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS): HResult;

    // Sets the new startposition in the audiobuffer.
    // Note that position must contains the number of samples!
    function GotoNewPosition(pPosition: MFTIME): HResult;

    property PlayStatus: TRenderStatus read pvRenderStatus;
    property SoundChannels: UINT32 read pvChannels;
    property VolumeChannels: TFloatArray read pvVolumeChannels;
    property Duration: MFTIME read pvSourceFileDuration;
    property SamplesPerSec: UINT64 read pvSamplesPerSecond;
    property ReverbParameters: TReverbI3DL2ParamArray read pvReverbI3DL2ParamArray;

    property AudioEventData: TXAudio2EventData read FXaudio2EventData;
    property AddNewSourceVoice: Boolean read NeedNewSourceVoice;

    // Reverb effect assignments.
    property ReverbEffectOnSourceVoice: Boolean read bReverbEffectOnSourceVoice;
    property ReverbEffectOnMasterVoice: Boolean read bReverbEffectOnMasteringVoice;

  end;


implementation


constructor TXaudio2Engine.Create();
begin
  inherited;
  FCriticalSection := TCriticalSection.Create;
  NeedNewSourceVoice := False;
  bReverbEffectOnSourceVoice := False;
  bReverbEffectOnMasteringVoice := False;
  pvReverbI3DL2ParamArray := GetReverbParams();
end;


destructor TXaudio2Engine.Destroy();
begin
  pvRenderStatus := rsDestroying;
  if Assigned(pvXAudio2) then
    pvXAudio2.StopEngine();

      FreeAndNil(FCriticalSection);

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

      if Assigned(pvWaveformatex) then
        CoTaskMemFree(pvWaveformatex);

      if Assigned(pvMemoryStream) then
        FreeAndNil(pvMemoryStream);

      SafeRelease(pvXAudio2);

  inherited;
end;


function TXaudio2Engine.LoadAndPlay(const pHwndCaller: THandle;
                                    const pAudiofile: TFileName): HResult;
begin
  Result := LoadFile(pHwndCaller,
                     pAudiofile);

  if SUCCEEDED(Result) then
    Result := InitializeXAudio2(False);
end;


function TXaudio2Engine.LoadFile(const pHwndCaller: THandle;
                                 const pAudiofile: TFileName): HResult;
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
  flags: DWord;
  mfMediaBuffer: IMFMediaBuffer;
  audioData: PByte;

  audioValidDataLength: DWord;

  audiobaseTime: Int64;
  audioTimestamp: Int64;

label
  done;

begin
  pvHwndCaller := pHwndCaller;
  pvFileName := pAudiofile;
  if not FileExists(pAudiofile) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto done;
    end;

  // Get the length of the audiofile.
  hr := GetFileDuration(StrToPWideChar(pAudiofile),
                        pvSourceFileDuration);

  if SUCCEEDED(hr) then
    // Create Attribute Store.
    hr := MFCreateAttributes(sourceReaderConfiguration,
                             1);

  if SUCCEEDED(hr) then
    hr := sourceReaderConfiguration.SetUINT32(MF_LOW_LATENCY,
                                              UInt32(True));

  // Create Source Reader.
  if SUCCEEDED(hr) then
    hr := MFCreateSourceReaderFromURL(StrToPWideChar(pAudiofile),
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
                                    MFAudioFormat_Float {MFAudioFormat_PCM});

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



  audiobaseTime := 0;
  // Create reuse memorystream or create if not done yet.
  if Assigned(pvMemoryStream) then
    pvMemoryStream.Clear
  else
    pvMemoryStream := TMemoryStream.Create();

  // Fill the buffers.
  while (hr = S_OK) do
    begin
      flags := 0;
      hr := sourceReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                    0,
                                    nil,
                                    @flags,
                                    @AudioTimestamp,    // Get the timestamp for this sample.
                                    @mfSample);
      // Set base.
      if (audiobaseTime = 0) then
        audiobaseTime := AudioTimestamp;

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

     if (flags = MF_SOURCE_READERF_STREAMTICK) then
       Continue;

     if (mfSample <> nil) then
       begin
         // Rebase the timestamp.
         //audioTimeStamp := audioTimeStamp - audiobaseTime;

         //hr := mfSample.SetSampleTime(audioTimeStamp);

         if SUCCEEDED(hr) then
           begin
             // Convert data to contiguous buffer.
             hr := mfSample.ConvertToContiguousBuffer(mfMediaBuffer);
             // Get the media buffer from the sample.
             //hr := mfSample.GetBufferByIndex(0,
             //                                mfMediaBuffer);

             // Get the audio data from the media buffer.
             if SUCCEEDED(hr) then
               begin
                 // Lock Buffer and copy to memory stream
                 hr := mfMediaBuffer.Lock(audioData,
                                          nil,
                                          @audioValidDataLength);
                 if SUCCEEDED(hr) then
                   try
                     pvMemoryStream.Write(audioData^,
                                          audioValidDataLength);
                   finally
                     // Unlock Buffer
                     hr := mfMediaBuffer.Unlock();
                   end;
                end; // SUCCEEDED(hr)
             mfSample := nil;
           end; // SUCCEEDED(hr)
       end; // mfSample <> nil
    end;  // while

done:
  if Assigned(mfSample) then
    SafeRelease(mfSample);
  Result := hr;
end;


function TXaudio2Engine.InitAudioBuffer(pPlayStart: UINT32 = 0;
                                        pPlayEnd: UINT32 = 0): HResult;
var
  hr: HResult;
  pvAudioData: PByte;
  pvBufferSize: UINT32;
  pvXAudioBuffer: PXAUDIO2_BUFFER;

begin
  hr := E_FAIL;
  pvMemoryStream.Position := 0;
  pvBufferSize := pvMemoryStream.Size;

  // Allocate memory for pvAudioData
  GetMem(pvAudioData,
         pvBufferSize);
  ZeroMemory(pvXAudioBuffer,
               SizeOf(XAUDIO2_BUFFER));
  //GetMem(pvXAudioBuffer,
  //       SizeOf(XAUDIO2_BUFFER));

  try
    // Copy the contents of pvMemoryStream to pvAudioData.
    pvMemoryStream.ReadBuffer(pvAudioData^,
                              pvBufferSize);

    pvXAudioBuffer.AudioBytes := pvBufferSize;
    pvXAudioBuffer.pAudioData := pvAudioData;
    pvXAudioBuffer.Flags := XAUDIO2_END_OF_STREAM;

    pvXAudioBuffer.PlayBegin := pPlayStart; // pvBufferStart
    pvXAudioBuffer.PlayLength := pPlayEnd;  // Zero means to play until the end of the buffer.
    // Can be extended with loop arguments.

    // Start source voice and submit buffer.
    hr := pvSourceVoice.SubmitSourceBuffer(pvXAudioBuffer);

  finally
    FreeMem(pvAudioData);
    ZeroMemory(@pvXAudioBuffer,
               SizeOf(XAUDIO2_BUFFER));
    Result := hr;
  end;
end;


function TXaudio2Engine.InitializeXAudio2(pDoPlay: Boolean = False): HResult;
var
  hr: HResult;
  voicestate: XAUDIO2_VOICE_STATE;

label
  done;

begin
  pvBufferPrevPlayed := 0;
  pvNewBufferPosition := 0;

  //if Assigned(pvAudioData) then
  //  pvAudioData := nil;

  // Remove active effects.
  if Assigned(pvFxReverbEffect) then
    FreeAndNil(pvFxReverbEffect);

  if Assigned(pvFxMasterLimiter) then
    FreeAndNil(pvFxMasterLimiter);


  // Use the XAudio2Create function to create an instance of the XAudio2 engine.
  // Skip this if user pressed Stop and does Replay again.
  // In that case the SourceVoice is removed from the topology.
  // The SourceVoice will be added later.
  if (NeedNewSourceVoice = False) then
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

  NeedNewSourceVoice := False;

  // Add SourceVoice to the topology.
  hr := pvXAudio2.CreateSourceVoice(@pvSourceVoice,
                                    pvWaveformatex,
                                    0,
                                    XAUDIO2_DEFAULT_FREQ_RATIO,
                                    Self); // register Audio2VoiceCallback
  if FAILED(hr) then
    goto done;

  // Create effects.
  pvFxReverbEffect := TFxReverb.Create();
  pvFxMasterLimiter := TFxMasterLimiter.Create();

  // Set up XAudio2 buffer.
  hr := InitAudioBuffer();
  if FAILED(hr) then
    goto done;

  // Setup is done, set status we are ready to go.

  pvRenderStatus := rsInitialized;
  pvBufferPrevPlayed := 0;

  SendMessage(pvHwndCaller,
              WM_DATA_READY_NOTIFY,
              WPARAM(0),
              LPARAM(0));

  // This flag should be set if we played this track before,
  // so we don't have to reinitialize the sourcereader.
  if pDoPlay then
    begin
      hr := pvSourceVoice.Start();
      if SUCCEEDED(hr) then
        pvRenderStatus := rsPlaying;
    end;

  if FAILED(hr) then
    goto done;

  while (pvRenderStatus <> rsEndOfBuffer) or
        (pvRenderStatus <> rsEndOfStream) do
    begin

      if (pvRenderStatus = rsStopped) then
        begin
          if not Assigned(pvSourceVoice) then
            Break;

          pvSourceVoice.DestroyVoice();
          pvSourceVoice := nil;
          NeedNewSourceVoice := True;
          Break;
        end;

      if (pvRenderStatus = rsDestroying) then
        Break;

      // This consumes some cpu load, but saves using a timer.==================
      pvSourceVoice.GetState(voiceState,
                             0);
      // Here we store the results in struct FXaudio2EventData.
      // The caller can readout these values for example while using a timer, event or message handler.
      // Calculate the samples processed (Starting after a stop event has taken place).
      FXaudio2EventData.SamplesProcessed := voiceState.SamplesPlayed - pvBufferPrevPlayed;

      // Calculate the samples that have been processed, to milliseconds.
      // The caller can, for example, calculate the readable time in HH:MM:SS:MS.
      FXaudio2EventData.TimePlayed := (((voiceState.SamplesPlayed + pvNewBufferPosition) - pvBufferPrevPlayed) div pvSamplesPerSecond) * 10000000;
      FXaudio2EventData.Position := pvBufferStart;
      // Send score. Don't use PostMessage because it set priority above this thread.

      //SendMessage(pvHwndCaller,
      //            WM_DATA_PROCESSING_NOTIFY,
      //            WPARAM(@FXaudio2EventData),
      //            LPARAM(0));

      HandleThreadMessages(GetCurrentThread());
    end;

done:
  if SUCCEEDED(hr) then
    // Signal we are done playing.
    SendMessage(pvHwndCaller,
                WM_DATA_END_OF_STREAM_NOTIFY,
                WPARAM(0),
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

  if SUCCEEDED(Result) then
    pvRenderStatus := rsPlaying;

  SendMessage(pvHwndCaller,
              WM_DATA_PLAYING_NOTIFY,
              WPARAM(0),
              LPARAM(0));
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
    pvRenderStatus := rsPauzed;

  SendMessage(pvHwndCaller,
              WM_DATA_PAUZED_NOTIFY,
              WPARAM(0),
              LPARAM(0));
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
    pvRenderStatus := rsStopped;

  SendMessage(pvHwndCaller,
              WM_DATA_STOPPED_NOTIFY,
              WPARAM(0),
              LPARAM(0));
end;


procedure TXaudio2Engine.SetVolume(pVolume: Single);
var
  hr: HResult;

begin
  if Assigned(pvSourceVoice) then
    begin
      hr := pvSourceVoice.SetVolume(pVolume,
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
procedure TXaudio2Engine.SetVolumes(pVolumes: TFloatArray);
var
  aVolumes: TFloatArray;
  hr: HResult;
  i: Integer;

begin
  FCriticalSection.Enter;

  // Use the following formula to convert the volume level to the decibel (dB) scale:
  // Attenuation (dB) = 20 * log10(Level)
  // For example, a volume level of 0.50 represents 6.02 dB of attenuation.

  aVolumes := pVolumes;

  // Set boundaries to prevent overflow or clipping
  for i := 0 to Length(aVolumes) - 1 do
    begin
      if (aVolumes[i] > 1.0) then
        aVolumes[i] := 1.0;
      if (aVolumes[i] < 0.0) then
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
  FCriticalSection.Leave;
  HandleThreadMessages(TThread.CurrentThread.Handle);
end;


// Get the volumes for the channels.
function TXaudio2Engine.GetVolumes(): TFloatArray;
begin
  pvSourceVoice.GetChannelVolumes(pvChannels,
                                  @Result[0]);
end;


procedure TXaudio2Engine.SetPitch(pPitch: Single);
var
  flPitch: Single;
  
begin
  FCriticalSection.Enter;
  if not Assigned(pvSourceVoice) then
    Exit;

  flPitch := pPitch;
  // To prevent extreme pitching causing buffer underrun.
  if (flPitch > MAX_PITCH) then
    flPitch := MAX_PITCH;
  if (flPitch < MIN_PITCH) then
    flPitch := MIN_PITCH;

  pvSourceVoice.SetFrequencyRatio(flPitch);
  FCriticalSection.Leave;
end;


function TXaudio2Engine.SetReverb(pVoiceEffects: TEffectsOnVoices;
                                  pReverbParams: XAUDIO2FX_REVERB_PARAMETERS;
                                  pEnable: Boolean): HResult;
var
  ppVoice: PIXAudio2Voice;

begin
  FCriticalSection.Enter;
  Result := E_FAIL;

  if not Assigned(pvSourceVoice) or not Assigned(pvMasteringVoice) then
    Exit;

  // == Add reverb effect. ======================================
  if (pVoiceEffects = afxSourceVoice) then
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
      //Result := pvFxReverbEffect.EnableReverbEffect(ppVoice,
      //                                              pEnable);
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
      if (pVoiceEffects = afxSourceVoice) then
        bReverbEffectOnSourceVoice := pEnable
      else
        bReverbEffectOnMasteringVoice := pEnable;
    end;
  FCriticalSection.Leave;
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


function TXaudio2Engine.GotoNewPosition(pPosition: MFTIME): HResult;
var
  hr: HResult;
  VoiceState: XAUDIO2_VOICE_STATE;

begin
FCriticalSection.Enter;

  pvBufferStart := pPosition;

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

  FCriticalSection.Leave;

  HandleThreadMessages(TThread.CurrentThread.Handle);
  Result := hr;
end;


// Callback implementation =================================================

procedure TXaudio2Engine.OnVoiceProcessingPassStart(BytesRequired: UINT32);
begin
  // Stub. Not used.
  // pvRenderStatus := rsProcessingPassStart;
end;


procedure TXaudio2Engine.OnVoiceProcessingPassEnd();
begin
  // NStub. Not used.
  // pvRenderStatus := rsProcessingPassEnd;
end;

// OnStreanEnd will be triggered when OnBufferEnd signals and no further buffers are available.
// In short: If the last buffer has been rendered, this event will be triggered.
procedure TXaudio2Engine.OnStreamEnd();
begin
  // For internal use.
  pvRenderStatus := rsEndOfStream;
  SendMessage(pvHwndCaller,
              WM_DATA_END_OF_STREAM_NOTIFY,
              WPARAM(1),
              LPARAM(0));
end;


procedure TXaudio2Engine.OnBufferStart(pBufferContext: Pointer);
begin
  // For internal use.
  pvRenderStatus := rsPlaying;
  SendMessage(pvHwndCaller,
              WM_DATA_START_NOTIFY,
              WPARAM(1),
              LPARAM(0));
end;


// OnBufferEnd is the first event triggered when a buffer has been finished.
procedure TXaudio2Engine.OnBufferEnd(pBufferContext: Pointer);
begin
  // For internal use.
  pvRenderStatus := rsEndOfBuffer;
  SendMessage(pvHwndCaller,
              WM_DATA_END_OF_BUFFER_NOTIFY,
              WPARAM(1),
              LPARAM(0));
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

