// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: AudioDelayFileEngine.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Decodes an audio file, processes PCM through the delay MFT,
//              drains its tail, and writes a WAV file.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//
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
unit AudioDelayFileEngine;

interface

uses
  {System}
  System.SysUtils;

// Decode a file with the Source Reader, run Sample 9's MFT, and write PCM WAV.
procedure ProcessAudioFile(const AInputFile: string;
                           const AOutputFile: string;
                           const ADelayMs: Cardinal;
                           const AWetPercent: Cardinal;
                           out AInputFrames: UInt64;
                           out ATailFrames: UInt64;
                           out AFormatDescription: string);

implementation

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfAudioDelayMFT;

const
  AudioStream = MF_SOURCE_READER_FIRST_AUDIO_STREAM;


procedure CheckHr(const AOperation: string;
                  const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed (HRESULT %.8x).',
                              [AOperation, Cardinal(AHr)]);
end;


procedure WriteFourCC(const AStream: TStream;
                      const AText: AnsiString);
begin

  if (Length(AText) <> 4) then
    raise Exception.Create('A WAV chunk name must contain four bytes.');
  AStream.WriteBuffer(AText[1],
                      4);
end;


procedure WriteWord(const AStream: TStream;
                    const AValue: Word);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;


procedure WriteDWord(const AStream: TStream;
                     const AValue: Cardinal);
begin

  AStream.WriteBuffer(AValue,
                      SizeOf(AValue));
end;


procedure WriteWaveHeader(const AStream: TStream;
                          const AChannels,
                          ARate: Cardinal;
                          ABits: Cardinal;
                          ABlockAlign: Cardinal);
begin

  WriteFourCC(AStream,
              'RIFF');

  WriteDWord(AStream,
             0);                 // Filled after the final sample.

  WriteFourCC(AStream,
              'WAVE');

  WriteFourCC(AStream,
              'fmt ');

  WriteDWord(AStream,
             16);                // PCM format chunk size.

  WriteWord(AStream,
            1);                  // WAVE_FORMAT_PCM.

  WriteWord(AStream,
            Word(AChannels));

  WriteDWord(AStream,
             ARate);

  WriteDWord(AStream,
             ARate * ABlockAlign);

  WriteWord(AStream,
            Word(ABlockAlign));

  WriteWord(AStream,
            Word(ABits));

  WriteFourCC(AStream,
              'data');

  WriteDWord(AStream,
             0);                 // Filled after the final sample.
end;


procedure FinishWaveHeader(const AStream: TStream;
                           const ADataBytes: UInt64);
begin

  if (ADataBytes > High(Cardinal) - 36) then
    raise Exception.Create('The output is too large for a standard WAV file.');

  AStream.Position := 4;
  WriteDWord(AStream,
             Cardinal(ADataBytes + 36));
  AStream.Position := 40;
  WriteDWord(AStream,
             Cardinal(ADataBytes));
end;


function WriteSample(const ASample: IMFSample; const AStream: TStream;
                     const ABlockAlign: Cardinal): Cardinal;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  Capacity: DWORD;
  Bytes: DWORD;

begin

  if not Assigned(ASample) then
    raise Exception.Create('The MFT returned no output sample.');

  CheckHr('Convert output to one buffer',
          ASample.ConvertToContiguousBuffer(@Buffer));

  CheckHr('Lock output buffer',
          Buffer.Lock(Data,
                      @Capacity,
                      @Bytes));
  try
    if ((Bytes mod ABlockAlign) <> 0) then
      raise Exception.Create('The MFT returned an incomplete PCM frame.');

    if (Bytes <> 0) then
      AStream.WriteBuffer(Data^,
                          Bytes);

  finally
    Buffer.Unlock();
  end;

  Result := Bytes;
end;


procedure ProcessAudioFile(const AInputFile, AOutputFile: string;
                           const ADelayMs, AWetPercent: Cardinal;
                           out AInputFrames, ATailFrames: UInt64;
                           out AFormatDescription: string);
var
  hr: HResult;
  Reader: IMFSourceReader;
  RequestedType, ActualType: IMFMediaType;
  Transform: IMFTransform;
  Attributes: IMFAttributes;
  InputSample: IMFSample;
  Output: MFT_OUTPUT_DATA_BUFFER;
  OutputFile: TFileStream;
  Channels, Rate, Bits, BlockAlign, AvgBytes: Cardinal;
  Flags, Status, Bytes: DWORD;
  DataBytes: UInt64;
  TimeStamp: LONGLONG;
  Completed: Boolean;

begin

  AInputFrames := 0;
  ATailFrames := 0;
  AFormatDescription := '';

  if (ADelayMs = 0) or (ADelayMs > 10000) then
    raise Exception.Create('Delay must be from 1 to 10000 milliseconds.');

  if (AWetPercent > 100) then
    raise Exception.Create('Wet mix must be from 0 to 100 percent.');

  if SameText(ExpandFileName(AInputFile),
              ExpandFileName(AOutputFile)) then
    raise Exception.Create('Choose a different output file.');

  CheckHr('Open source file',
          MFCreateSourceReaderFromURL(PWideChar(AInputFile),
                                      nil,
                                      Reader));

  CheckHr('Deselect streams',
          Reader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                    False));

  CheckHr('Select audio stream',
          Reader.SetStreamSelection(AudioStream,
                                    True));

  CheckHr('Create requested type',
          MFCreateMediaType(RequestedType));

  CheckHr('Request audio',
          RequestedType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Audio));

  CheckHr('Request PCM',
          RequestedType.SetGUID(MF_MT_SUBTYPE,
          MFAudioFormat_PCM));

  CheckHr('Request 16-bit samples',
          RequestedType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
          16));

  CheckHr('Set decoded media type',
          Reader.SetCurrentMediaType(AudioStream,
                                     0,
                                     RequestedType));

  CheckHr('Get decoded media type',
          Reader.GetCurrentMediaType(AudioStream,
                                     @ActualType));

  CheckHr('Get channel count',
          ActualType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                               Channels));

  CheckHr('Get sample rate',
          ActualType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                               Rate));

  CheckHr('Get bit depth',
          ActualType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                               Bits));

  CheckHr('Get block alignment',
          ActualType.GetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                               BlockAlign));

  CheckHr('Get byte rate',
          ActualType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                               AvgBytes));

  if ((Channels <> 1) and (Channels <> 2)) or
     (Bits <> 16) or
     (Rate = 0) or
     (BlockAlign <> Channels * 2) or
     (AvgBytes <> Rate * BlockAlign) then
    raise Exception.Create('The decoded format must be mono or stereo, 16-bit PCM.');

  AFormatDescription := Format('%d Hz, %d channel(s), %d-bit PCM',
                               [Rate, Channels, Bits]);

  Transform := TMfAudioDelayMFT.Create as IMFTransform;

  CheckHr('Get delay attributes',
           Transform.GetAttributes(Attributes));

  CheckHr('Set delay length',
          Attributes.SetUINT32(MF_AUDIODELAY_DELAY_LENGTH,
                               ADelayMs));
  CheckHr('Set wet mix',
          Attributes.SetUINT32(MF_AUDIODELAY_WET_DRY_MIX,
                               AWetPercent));

  CheckHr('Set MFT input type',
           Transform.SetInputType(0,
                                  ActualType,
                                  0));

  CheckHr('Set MFT output type',
          Transform.SetOutputType(0,
                                  ActualType,
                                  0));

  CheckHr('Begin streaming',
          Transform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                   0));

  CheckHr('Start stream',
          Transform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                   0));

  Completed := False;
  OutputFile := TFileStream.Create(AOutputFile,
                                   fmCreate);

  try
    WriteWaveHeader(OutputFile,
                    Channels,
                    Rate,
                    Bits,
                    BlockAlign);

    DataBytes := 0;

    repeat
      InputSample := nil;
      Flags := 0;
      TimeStamp := 0;
      CheckHr('Read decoded sample',
              Reader.ReadSample(AudioStream,
                                0,
                                nil,
                                @Flags,
                                @TimeStamp,
                                @InputSample));

      if ((Flags and MF_SOURCE_READERF_ERROR) <> 0) then
        raise Exception.Create('The Source Reader reported a decoding error.');

      if ((Flags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0) then
        raise Exception.Create('The audio format changed during the file.');

      if Assigned(InputSample) then
        begin
          FillChar(Output,
                   SizeOf(Output),
                   0);

          CheckHr('Process input',
                  Transform.ProcessInput(0,
                                         InputSample,
                                         0));

          CheckHr('Process output',
                  Transform.ProcessOutput(0,
                                          1,
                                          @Output,
                                          Status));

          try
            Bytes := WriteSample(Output.pSample,
                                 OutputFile,
                                 BlockAlign);

          finally
            Output.pSample := nil;
          end;

          Inc(DataBytes,
              Bytes);

          Inc(AInputFrames,
              Bytes div BlockAlign);
        end;
      until ((Flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0);

    CheckHr('End of stream',
            Transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM,
                                     0));

    CheckHr('Drain delay tail',
            Transform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN,
                                     0));
    repeat
      FillChar(Output,
               SizeOf(Output),
               0);

      Status := 0;
      Bytes := 0;

      hr := Transform.ProcessOutput(0,
                                    1,
                                    @Output,
                                    Status);
      if (hr = MF_E_TRANSFORM_NEED_MORE_INPUT) then
        Break;

      CheckHr('Process delay tail',
              Hr);

      try
        if not Assigned(Output.pSample) then
          raise Exception.Create('The delay tail contained no sample.');

        Bytes := WriteSample(Output.pSample,
                             OutputFile,
                             BlockAlign);

      finally
        Output.pSample := nil;
      end;

      Inc(DataBytes,
          Bytes);

      Inc(ATailFrames,
          Bytes div BlockAlign);

    until False;

    FinishWaveHeader(OutputFile,
                     DataBytes);
    Completed := True;

  finally
    OutputFile.Free;
    Transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_STREAMING,
                             0);
    if not Completed then
      System.SysUtils.DeleteFile(AOutputFile);
  end;
end;

end.
