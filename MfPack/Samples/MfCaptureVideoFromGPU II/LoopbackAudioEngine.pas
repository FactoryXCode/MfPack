// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  LoopbackAudioEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Wav and Flac writer.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit LoopbackAudioEngine;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Rtti,
  System.TypInfo,
  {MediaFoundation (only used for FLAC path)}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfReadWrite,
  {Project}
  WasapiLoopbackCapture,
  Helpers;

type

  TAudioFileFormat = (aoWavPcm16,
                      aoFlac);

  // Open-array pointer types (Delphi-safe indexing; avoids "Array type required")
  TSingleArray = array[0..0] of Single;
  PSingleArray = ^TSingleArray;

  TSmallIntArray = array[0..0] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TLoopbackAudioOnlyRecorder = class(TComponent)
  private

    FCrit: TCriticalSection;
    FCapture: TWasapiLoopbackCapture;

    FFormat: TAudioFileFormat;
    FFileName: string;
    FChannelMask: Cardinal;

    // WAV
    FStream: TFileStream;
    FWavHeaderWritten: Boolean;
    FWavDataBytes: UInt64;

    // MF/FLAC
    FFlacWriter: IMFSinkWriter;
    FFlacAudioStreamIndex: DWORD;
    FFlacReady: Boolean;
    FAudioRt100ns: Int64;
    FTempPcm16: TBytes;

    FStopping: Boolean;

    FCaptureDeviceId: string;


    procedure ApplyDeviceIdToCapture();

    procedure CaptureOnData(Sender: TObject;
                            const Buffer: Pointer;
                            NumFrames: Cardinal;
                            const WaveFormat: PWAVEFORMATEX);

    function IsFloat32Format(const Wf: PWAVEFORMATEX): Boolean;
    function IsPcm16Format(const Wf: PWAVEFORMATEX): Boolean;

    function DefaultChannelMask(Channels: Word): Cardinal;

    procedure EnsureTempPcm16Bytes(NeededBytes: Integer);
    function ConvertToPcm16(const InBuf: Pointer;
                            NumFrames: Cardinal;
                            const Wf: PWAVEFORMATEX;
                            out OutPtr: Pointer;
                            out OutBytes: Integer): Boolean;

    // WAV helpers
    procedure EnsureWavHeader(const Wf: PWAVEFORMATEX);
    procedure WriteWavHeaderPcm16(const Channels: Word;
                                  const SampleRate: Cardinal;
                                  const DataSize: Cardinal);
    procedure PatchWavSizes;

    // FLAC via MF helpers

    procedure EnsureFlacWriter(const Wf: PWAVEFORMATEX);
    procedure WriteFlacSample(const PcmPtr: Pointer;
                              PcmBytes: Integer;
                              const Wf: PWAVEFORMATEX;
                              NumFrames: Cardinal);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    // ChannelMask: 0 = auto ($4 mono, $3 stereo, else 0)
    procedure StartToFile(const FileName: string;
                          const Format: TAudioFileFormat;
                          const ChannelMask: Cardinal = 0); overload;

    procedure StartToFile(const FileName: string;
                          const Format: TAudioFileFormat;
                          const ChannelMask: Cardinal;
                          const DeviceId: string); overload;
    procedure Stop();

    property OutputFileName: string read FFileName;
  end;


implementation


{ TLoopbackAudioOnlyRecorder }

constructor TLoopbackAudioOnlyRecorder.Create(AOwner: TComponent);
begin

  inherited;

  FCrit := TCriticalSection.Create();
  FCapture := TWasapiLoopbackCapture.Create;
  FCapture.OnData := CaptureOnData;

  FStream := nil;
  FWavHeaderWritten := False;
  FWavDataBytes := 0;

  FFlacWriter := nil;
  FFlacAudioStreamIndex := 0;
  FFlacReady := False;
  FAudioRt100ns := 0;

  SetLength(FTempPcm16, 0);
  FStopping := False;
end;


destructor TLoopbackAudioOnlyRecorder.Destroy();
begin

  Stop();
  FreeAndNil(FCapture);
  FreeAndNil(FCrit);
  inherited;
end;


procedure TLoopbackAudioOnlyRecorder.ApplyDeviceIdToCapture();
var
  Ctx: TRttiContext;
  T: TRttiType;
  P: TRttiProperty;
  Names: array[0..3] of string;
  i: Integer;

begin

  if (FCaptureDeviceId = '') or
     (FCapture = nil) then
  Exit;

  Names[0] := 'AudioDeviceID';
  Names[1] := 'AudioDeviceId';
  Names[2] := 'DeviceId';
  Names[3] := 'DeviceID';

  Ctx := TRttiContext.Create;
  T := Ctx.GetType(FCapture.ClassType);

  for i := Low(Names) to High(Names) do
    begin

      P := T.GetProperty(Names[i]);
      if (P <> nil) and
        P.IsWritable and
        (P.PropertyType.TypeKind = tkUString) then
        begin

          P.SetValue(FCapture,
                     FCaptureDeviceId);
          Exit;
        end;
    end;
end;


procedure TLoopbackAudioOnlyRecorder.StartToFile(const FileName: string;
                                                 const Format: TAudioFileFormat;
                                                 const ChannelMask: Cardinal);
begin

  StartToFile(FileName,
              Format,
              ChannelMask,
              '');
end;


procedure TLoopbackAudioOnlyRecorder.StartToFile(const FileName: string;
                                                 const Format: TAudioFileFormat;
                                                 const ChannelMask: Cardinal;
                                                 const DeviceId: string);
var
  dir: string;

begin

  Stop();

  FCrit.Enter;
  try
    FFileName := FileName;
    FFormat := Format;
    FChannelMask := ChannelMask;
    FCaptureDeviceId := DeviceId;

    FWavHeaderWritten := False;
    FWavDataBytes := 0;

    FreeAndNil(FStream);
    FFlacWriter := nil;
    FFlacReady := False;
    FFlacAudioStreamIndex := 0;
    FAudioRt100ns := 0;

    FStopping := False;

    dir := ExtractFileDir(FFileName);
    if dir <> '' then
      ForceDirectories(dir);

    if FFormat = aoWavPcm16 then
      FStream := TFileStream.Create(FFileName,
                                    fmCreate or fmShareDenyWrite);

    // CRITICAL: re-hook every time (some Stop implementations clear it)
    FCapture.OnData := CaptureOnData;

    // Best-effort: apply device id if the capture class supports it
    ApplyDeviceIdToCapture();

  finally

    FCrit.Leave;
  end;

  if Assigned(FCapture.OnData) then
    FCapture.Start;
end;


procedure TLoopbackAudioOnlyRecorder.Stop();
var
  hr: HResult;

begin

  FCrit.Enter;

  try

    if FStopping then
      Exit;
    FStopping := True;
  finally

    FCrit.Leave;
  end;

  if Assigned(FCapture) then
    FCapture.Stop;


  FCrit.Enter;
  try
    if Assigned(FStream) then
      begin

        if FWavHeaderWritten then
          PatchWavSizes;
        FreeAndNil(FStream);
      end;

    if Assigned(FFlacWriter) then
      begin

        hr := FFlacWriter.Finalize();
        CheckHR(hr, 'FLAC SinkWriter.Finalize');

        FFlacWriter := nil;
        FFlacReady := False;
      end;

    SetLength(FTempPcm16, 0);
  finally

    FCrit.Leave;
  end;
end;


procedure TLoopbackAudioOnlyRecorder.CaptureOnData(Sender: TObject;
                                                   const Buffer: Pointer;
                                                   NumFrames: Cardinal;
                                                   const WaveFormat: PWAVEFORMATEX);
var
  outPtr: Pointer;
  outBytes: Integer;

begin

  if FStopping then
    Exit;
  if (Buffer = nil) or
     (WaveFormat = nil) or
     (NumFrames = 0) then
    Exit;

  if not ConvertToPcm16(Buffer,
                        NumFrames,
                        WaveFormat,
                        outPtr,
                        outBytes) then
    Exit;

  FCrit.Enter;
  try

    if FStopping then
      Exit;

    case FFormat of
      aoWavPcm16:
        begin

          EnsureWavHeader(WaveFormat);

          if Assigned(FStream) then
            begin
              FStream.WriteBuffer(outPtr^, outBytes);

              // DEBUG only
              // OutputDebugString(PChar(Format('WAV pos=%d size=%d wrote=%d',
              //                                [FStream.Position, FStream.Size, outBytes])));

              //OutputDebugString(PChar(Format('WAV fmt: tag=%x ch=%d rate=%d bits=%d align=%d',
              //                 [WaveFormat.wFormatTag, WaveFormat.nChannels, WaveFormat.nSamplesPerSec,
              //                 WaveFormat.wBitsPerSample, WaveFormat.nBlockAlign])));

            Inc(FWavDataBytes,
                UInt64(outBytes));
          end;
        end;

      aoFlac:
        begin

          EnsureFlacWriter(WaveFormat);

          if FFlacReady then
            WriteFlacSample(outPtr,
                            outBytes,
                            WaveFormat,
                            NumFrames);
        end;
    end;
  finally
    FCrit.Leave;
  end;
end;


function TLoopbackAudioOnlyRecorder.IsFloat32Format(const Wf: PWAVEFORMATEX): Boolean;
var
  Ext: PWAVEFORMATEXTENSIBLE;

begin

  Result := False;

  if (Wf = nil) then
    Exit;

  // Plain old float
  if (Wf.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) and
     (Wf.wBitsPerSample = 32) then
    Exit(True);

  // EXTENSIBLE float (this is your case)
  if (Wf.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and
     (Wf.cbSize >= SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX)) then
    begin
      Ext := PWAVEFORMATEXTENSIBLE(Wf);
      Result := IsEqualGUID(Ext.SubFormat,
                            KSDATAFORMAT_SUBTYPE_IEEE_FLOAT) and
                            (Ext.Format.wBitsPerSample = 32);
    end;
end;


function TLoopbackAudioOnlyRecorder.IsPcm16Format(const Wf: PWAVEFORMATEX): Boolean;
var
  Ext: PWAVEFORMATEXTENSIBLE;

begin

  Result := False;

  if (Wf = nil) then
    Exit;

  if (Wf.wFormatTag = WAVE_FORMAT_PCM) and
     (Wf.wBitsPerSample = 16) then
    Exit(True);

  if (Wf.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and
     (Wf.cbSize >= SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX)) then
    begin
      Ext := PWAVEFORMATEXTENSIBLE(Wf);
      Result := IsEqualGUID(Ext.SubFormat,
                            KSDATAFORMAT_SUBTYPE_PCM) and
                            (Ext.Format.wBitsPerSample = 16);
    end;
end;


function TLoopbackAudioOnlyRecorder.DefaultChannelMask(Channels: Word): Cardinal;
begin

  if (Channels = 1) then
    Result := $4
  else
    if (Channels = 2) then
      Result := $3
  else
    Result := 0;
end;


procedure TLoopbackAudioOnlyRecorder.EnsureTempPcm16Bytes(NeededBytes: Integer);
begin

  if (NeededBytes <= 0) then
    Exit;
  if (Length(FTempPcm16) < NeededBytes) then
    SetLength(FTempPcm16,
              NeededBytes);
end;


function TLoopbackAudioOnlyRecorder.ConvertToPcm16(const InBuf: Pointer; NumFrames: Cardinal;
                                                   const Wf: PWAVEFORMATEX;
                                                   out OutPtr: Pointer;
                                                   out OutBytes: Integer): Boolean;
var
  nSamples: Integer;
  i: Integer;
  v: Single;
  s: Integer;
  inF: PSingle;
  outP: PSmallInt;

begin

  Result := False;
  OutPtr := nil;
  OutBytes := 0;

  if (InBuf = nil) or
     (Wf = nil) or
     (NumFrames = 0) or
     (Wf.nChannels = 0) then
    Exit;

  nSamples := Integer(NumFrames) * Integer(Wf.nChannels);
  if (nSamples <= 0) then
    Exit;

  // Pass-through PCM16
  if IsPcm16Format(Wf) then
    begin

      OutBytes := nSamples * SizeOf(SmallInt);
      OutPtr := Pointer(InBuf);
      Result := True;
      Exit;
    end;

  // Convert float32 -> PCM16 (works for WAVE_FORMAT_EXTENSIBLE float too)
  if IsFloat32Format(Wf) then
    begin
      OutBytes := nSamples * SizeOf(SmallInt);
      EnsureTempPcm16Bytes(OutBytes);

      inF  := PSingle(InBuf);
      outP := PSmallInt(@FTempPcm16[0]);

      for i := 0 to nSamples - 1 do
        begin
          v := inF^;
          Inc(inF);

          // clamp
          if (v > 1.0) then
            v := 1.0
          else
            if (v < -1.0 )then
              v := -1.0;

          // scale with proper handling of -1.0 -> -32768
          if (v >= 0) then
            s := Trunc(v * 32767.0 + 0.5)
          else
            s := Trunc(v * 32768.0 - 0.5);

          if (s > 32767) then
            s := 32767
          else
            if (s < -32768) then
              s := -32768;

          outP^ := SmallInt(s);
          Inc(outP);
        end;

      OutPtr := Pointer(@FTempPcm16[0]);
      Result := True;
      Exit;
    end;

  // Unsupported
  Result := False;
end;


procedure TLoopbackAudioOnlyRecorder.EnsureWavHeader(const Wf: PWAVEFORMATEX);
begin

  if FWavHeaderWritten then
    Exit;
  if not Assigned(FStream) then
    Exit;
  if (Wf = nil) or
     (Wf.nChannels = 0) or
     (Wf.nSamplesPerSec = 0) then
  Exit;

  WriteWavHeaderPcm16(Wf.nChannels,
                      Wf.nSamplesPerSec,
                      0);
  FWavHeaderWritten := True;
end;


procedure TLoopbackAudioOnlyRecorder.WriteWavHeaderPcm16(const Channels: Word;
                                                         const SampleRate: Cardinal;
                                                         const DataSize: Cardinal);
const
  RIFF_ID: array[0..3] of AnsiChar = ('R','I','F','F');
  WAVE_ID: array[0..3] of AnsiChar = ('W','A','V','E');
  FMT_ID : array[0..3] of AnsiChar = ('f','m','t',' ');
  DATA_ID: array[0..3] of AnsiChar = ('d','a','t','a');

var
  riffSize: Cardinal;
  fmtSize: Cardinal;
  audioFormat: Word;
  bitsPerSample: Word;
  blockAlign: Word;
  byteRate: Cardinal;

begin

  audioFormat := 1; // PCM
  bitsPerSample := 16;
  blockAlign := Channels * (bitsPerSample div 8);
  byteRate := SampleRate * Cardinal(blockAlign);

  fmtSize := 16;
  riffSize := 4 + (8 + fmtSize) + (8 + DataSize);

  FStream.Position := 0;
  FStream.WriteBuffer(RIFF_ID, 4);
  FStream.WriteBuffer(riffSize, 4);
  FStream.WriteBuffer(WAVE_ID, 4);

  FStream.WriteBuffer(FMT_ID, 4);
  FStream.WriteBuffer(fmtSize, 4);
  FStream.WriteBuffer(audioFormat, 2);
  FStream.WriteBuffer(Channels, 2);
  FStream.WriteBuffer(SampleRate, 4);
  FStream.WriteBuffer(byteRate, 4);
  FStream.WriteBuffer(blockAlign, 2);
  FStream.WriteBuffer(bitsPerSample, 2);

  FStream.WriteBuffer(DATA_ID, 4);
  FStream.WriteBuffer(DataSize, 4);
end;


procedure TLoopbackAudioOnlyRecorder.PatchWavSizes;
var
  riffSize: DWord;
  dataSize: DWord;

begin

  if (not Assigned(FStream)) or
     (not FWavHeaderWritten) then
    Exit;

  if (FWavDataBytes > High(DWord)) then
    dataSize := High(DWord)
  else
    dataSize := Cardinal(FWavDataBytes);

  riffSize := 36 + dataSize;

  FStream.Position := 4;
  FStream.WriteBuffer(riffSize, 4);
  FStream.Position := 40;
  FStream.WriteBuffer(dataSize, 4);
end;


procedure TLoopbackAudioOnlyRecorder.EnsureFlacWriter(const Wf: PWAVEFORMATEX);
var
  hr: HRESULT;
  outType,
  inType: IMFMediaType;
  chMask: Cardinal;

begin

  if FFlacReady then
    Exit;

  if (Wf = nil) or
     (Wf.nChannels = 0) or
     (Wf.nSamplesPerSec = 0) then
    Exit;

  hr := MFStartup(MF_VERSION,
                  MFSTARTUP_FULL);
  CheckHR(hr, 'MFStartup');

  hr := MFCreateSinkWriterFromURL(PWideChar(WideString(FFileName)),
                                  nil,
                                  nil,
                                  FFlacWriter);
  CheckHR(hr, 'MFCreateSinkWriterFromURL(.flac)');

  // Output type: FLAC (declare 24-bit; encoder will store correct bits-per-sample)
  hr := MFCreateMediaType(outType);
  CheckHR(hr, 'MFCreateMediaType(outType)');

  hr := outType.SetGUID(MF_MT_MAJOR_TYPE,
                        MFMediaType_Audio);
  CheckHR(hr, 'FLAC outType MAJOR');

  hr := outType.SetGUID(MF_MT_SUBTYPE,
                        MFAudioFormat_FLAC);
  CheckHR(hr, 'FLAC outType SUBTYPE');

  hr := outType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                          Wf.nChannels);
  CheckHR(hr, 'FLAC outType channels');

  hr := outType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                          Wf.nSamplesPerSec);
  CheckHR(hr, 'FLAC outType rate');

  hr := outType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                          16);
  CheckHR(hr, 'FLAC outType bps');

  hr := FFlacWriter.AddStream(outType,
                              FFlacAudioStreamIndex);
  CheckHR(hr, 'SinkWriter.AddStream(FLAC)');

  // Input type: PCM 16-bit (simple + reliable)
  hr := MFCreateMediaType(inType);
  CheckHR(hr, 'MFCreateMediaType(inType)');

  hr := inType.SetGUID(MF_MT_MAJOR_TYPE,
                       MFMediaType_Audio);
  CheckHR(hr, 'PCM inType MAJOR');

  hr := inType.SetGUID(MF_MT_SUBTYPE,
                       MFAudioFormat_PCM);
  CheckHR(hr, 'PCM inType SUBTYPE');

  hr := inType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                         Wf.nChannels);
  CheckHR(hr, 'PCM inType channels');

  hr := inType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                         Wf.nSamplesPerSec);
  CheckHR(hr, 'PCM inType rate');

  hr := inType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                         16);
  CheckHR(hr, 'PCM inType bps');

  hr := inType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                         Wf.nChannels * 2);
  CheckHR(hr, 'PCM inType blockalign');

  hr := inType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                         Wf.nSamplesPerSec * Wf.nChannels * 2);
  CheckHR(hr, 'PCM inType avgBps');


  chMask := FChannelMask;
  if (chMask = 0) then
    chMask := DefaultChannelMask(Wf.nChannels);

  if (chMask <> 0) then
    begin

      hr := inType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK,
                             chMask);
      CheckHR(hr, 'PCM inType channel mask');
    end;

  hr := FFlacWriter.SetInputMediaType(FFlacAudioStreamIndex,
                                      inType,
                                      nil);
  CheckHR(hr, 'SinkWriter.SetInputMediaType(PCM16)');

  hr := FFlacWriter.BeginWriting();
  CheckHR(hr,'SinkWriter.BeginWriting');

  FFlacReady := True;
  FAudioRt100ns := 0;
end;


procedure TLoopbackAudioOnlyRecorder.WriteFlacSample(const PcmPtr: Pointer;
                                                     PcmBytes: Integer;
                                                     const Wf: PWAVEFORMATEX;
                                                     NumFrames: Cardinal);
var
  hr: HRESULT;
  buf: IMFMediaBuffer;
  sample: IMFSample;
  p: PByte;
  maxLen: DWORD;
  dur100ns: Int64;

begin

  if (not FFlacReady) or
     (FFlacWriter = nil) then
    Exit;

  if (PcmPtr = nil) or
    (PcmBytes <= 0) then
    Exit;

  if (Wf = nil) or
     (Wf.nSamplesPerSec = 0) then
    Exit;

  dur100ns := (Int64(NumFrames) * 10000000) div Int64(Wf.nSamplesPerSec);

  hr := MFCreateMemoryBuffer(PcmBytes,
                             buf);
  CheckHR(hr, 'MFCreateMemoryBuffer');

  hr := buf.Lock(p,
                 @maxLen,
                 nil);
  CheckHR(hr, 'AudioBuffer.Lock');

  try
    Move(PcmPtr^,
         p^,
         PcmBytes);
  finally

    buf.Unlock();
  end;

  hr := buf.SetCurrentLength(PcmBytes);
  CheckHR(hr, 'AudioBuffer.SetCurrentLength');

  hr := MFCreateSample(sample);
  CheckHR(hr, 'MFCreateSample');

  hr := sample.AddBuffer(buf);
  CheckHR(hr, 'Sample.AddBuffer');

  hr := sample.SetSampleTime(FAudioRt100ns);
  CheckHR(hr, 'Sample.SetSampleTime');

  hr :=sample.SetSampleDuration(dur100ns);
  CheckHR(hr, 'Sample.SetSampleDuration');

  hr := FFlacWriter.WriteSample(FFlacAudioStreamIndex,
                                sample);
  CheckHR(hr, 'SinkWriter.WriteSample(FLAC)');

  Inc(FAudioRt100ns,
      dur100ns);
end;


end.

