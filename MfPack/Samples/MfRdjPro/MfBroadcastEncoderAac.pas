// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfBroadcastEncoderAac.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Split out from MfIcecastBroadcastEngine.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit MfBroadcastEncoderAac;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  RDJ.Setup,
  MfIcecastBroadcastEngine;

type

  TMfBroadcastEncoderAac = class(TMfBroadcastEncoderBase)
  private

    FTransform: IMFTransform;
    FInputType: IMFMediaType;
    FOutputType: IMFMediaType;

    FSampleRate: Integer;
    FChannels: Integer;
    FBitrateKbps: Integer;
    FBytesPerFrame: Integer;
    FNextSampleTime100ns: Int64;
    // TEMP:
    FTemp16: TBytes;

    function CreateMediaTypes(): HRESULT;
    function CreateInputSample(const pData: PSingle;
                               AFrames: Integer;
                               out ASample: IMFSample): HRESULT;
    function DrainOutput(out AOutBuf: TBytes): HRESULT;

  public

    constructor Create(); reintroduce;
    destructor Destroy(); override;

    function Initialize(const ASettings: TRDJBroadcastSetup): HRESULT; override;
    function EncodeInterleavedFloat32(const pData: PSingle;
                                      AFrames: Integer;
                                      out AOutBuf: TBytes): HRESULT; override;
    procedure Flush(out AOutBuf: TBytes); override;
  end;

implementation

const

  CLSID_CMSAACEncMFT: TGUID = '{93AF0C51-2275-45D2-A35B-F2BA21CAED00}';

{ TMfBroadcastEncoderAac }

// Helper
function _AppendBytes(const A,
                            B: TBytes): TBytes;
var
  LA: Integer;
  LB: Integer;

begin

  LA := Length(A);
  LB := Length(B);

  SetLength(Result,
            LA + LB);

  if (LA > 0) then
    Move(A[0],
         Result[0],
         LA);

  if (LB > 0) then
    Move(B[0],
         Result[LA],
         LB);
end;


constructor TMfBroadcastEncoderAac.Create();
begin

  inherited Create;

  FSampleRate := 44100;
  FChannels := 2;
  FBitrateKbps := 128;
  FBytesPerFrame := FChannels * SizeOf(SmallInt);
  FNextSampleTime100ns := 0;
end;


destructor TMfBroadcastEncoderAac.Destroy();
begin

  FOutputType := nil;
  FInputType := nil;
  FTransform := nil;
  SetLength(FTemp16,
            0);

  inherited;
end;


function TMfBroadcastEncoderAac.CreateMediaTypes(): HRESULT;
var
  hr: HResult;
  AvgBytesPerSec: Cardinal;

begin

  AvgBytesPerSec := Cardinal((FBitrateKbps * 1000) div 8);

  hr := MFCreateMediaType(FOutputType);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Audio);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetGUID(MF_MT_SUBTYPE,
                            MFAudioFormat_AAC);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                              FSampleRate);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                              FChannels);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                              AvgBytesPerSec);
  if Failed(hr) then
    Exit(hr);

  hr := FOutputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                              16);
  if Failed(hr) then
    Exit(hr);

  hr := FTransform.SetOutputType(0,
                                 FOutputType,
                                 0);
  if Failed(hr) then
    Exit(hr);

  hr := MFCreateMediaType(FInputType);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetGUID(MF_MT_MAJOR_TYPE,
                           MFMediaType_Audio);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetGUID(MF_MT_SUBTYPE,
                           MFAudioFormat_PCM);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                             FSampleRate);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                             FChannels);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                             16);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                             FChannels * 2);
  if Failed(hr) then
    Exit(hr);

  hr := FInputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                             FSampleRate * FChannels * 2);
  if Failed(hr) then
    Exit(hr);

  if FChannels = 1 then
    hr := FInputType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK, $4)
  else
    hr := FInputType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK, $3);
  if Failed(hr) then
    Exit(hr);

  Result := FTransform.SetInputType(0,
                                    FInputType,
                                    0);
end;


function TMfBroadcastEncoderAac.Initialize(const ASettings: TRDJBroadcastSetup): HRESULT;
var
  hr: HResult;

begin

  FSampleRate := ASettings.SampleRate;
  if (FSampleRate <= 0) then
    FSampleRate := 44100;

  FChannels := ASettings.Channels;
  if (FChannels <= 0) then
    FChannels := 2;

  FBitrateKbps := ASettings.BitrateKbps;
  if (FBitrateKbps <= 0) then
    FBitrateKbps := 128;

  FBytesPerFrame := FChannels * SizeOf(SmallInt);
  FNextSampleTime100ns := 0;

  hr := CoCreateInstance(CLSID_CMSAACEncMFT,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IMFTransform,
                         FTransform);
  if Failed(hr) then
    Exit(hr);

  hr := CreateMediaTypes();
  if Failed(hr) then
    Exit(hr);

  hr := FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                  0);
  if Failed(hr) then
    Exit(hr);

  hr := FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                  0);
  if Failed(hr) then
    Exit(hr);

  hr := FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                  0);
  if Failed(hr) then
    Exit(hr);

  Result := S_OK;
end;


function TMfBroadcastEncoderAac.CreateInputSample(const pData: PSingle;
                                                  AFrames: Integer;
                                                  out ASample: IMFSample): HRESULT;
var
  hr: HResult;
  Buffer: IMFMediaBuffer;
  pDst: PByte;
  MaxLen: DWORD;
  CurLen: DWORD;
  SampleBytes: Integer;
  I: Integer;
  Src: PSingle;
  Dst: PSmallInt;
  X: Single;
  Dur100ns: Int64;

begin

  ASample := nil;
  Result := E_INVALIDARG;

  if (pData = nil) or (AFrames <= 0) then
    Exit;

  SampleBytes := AFrames * FBytesPerFrame;
  if (SampleBytes <= 0) then
    Exit;

  if Length(FTemp16) < SampleBytes then
    SetLength(FTemp16,
              SampleBytes);

  Src := pData;
  Dst := PSmallInt(@FTemp16[0]);

  for I := 0 to (AFrames * FChannels) - 1 do
    begin

      X := Src^;

      if IsNan(X) or IsInfinite(X) then
        X := 0.0;

      if (X > 1.0) then
        X := 1.0
      else
        if (X < -1.0) then
          X := -1.0;

      Dst^ := Round(X * 32767.0);

      Inc(Src);
      Inc(Dst);
    end;

  hr := MFCreateMemoryBuffer(SampleBytes,
                             Buffer);
  if Failed(hr) then
    Exit(hr);

  hr := Buffer.Lock(pDst,
                    @MaxLen,
                    @CurLen);
  if Failed(hr) then
    Exit(hr);

  try

    Move(FTemp16[0],
         pDst^,
         SampleBytes);
  finally

    Buffer.Unlock;
  end;

  hr := Buffer.SetCurrentLength(SampleBytes);
  if Failed(hr) then
    Exit(hr);

  hr := MFCreateSample(ASample);
  if Failed(hr) then
    Exit(hr);

  hr := ASample.AddBuffer(Buffer);
  if Failed(hr) then
    Exit(hr);

  Dur100ns := (Int64(AFrames) * 10000000) div FSampleRate;

  hr := ASample.SetSampleTime(FNextSampleTime100ns);
  if Failed(hr) then
    Exit(hr);

  hr := ASample.SetSampleDuration(Dur100ns);
  if Failed(hr) then
    Exit(hr);

  Inc(FNextSampleTime100ns,
      Dur100ns);

  Result := S_OK;
end;


function TMfBroadcastEncoderAac.DrainOutput(out AOutBuf: TBytes): HRESULT;
var
  StreamInfo: MFT_OUTPUT_STREAM_INFO;
  OutputBuffer: MFT_OUTPUT_DATA_BUFFER;
  Status: DWORD;
  OutSample: IMFSample;
  OutBuffer: IMFMediaBuffer;
  ContigBuffer: IMFMediaBuffer;
  pData: PByte;
  MaxLen: DWORD;
  CurLen: DWORD;
  BufSize: Cardinal;

begin

  SetLength(AOutBuf,
            0);

  StreamInfo := Default(MFT_OUTPUT_STREAM_INFO);

  Result := FTransform.GetOutputStreamInfo(0,
                                           StreamInfo);
  if Failed(Result) then
    Exit;

  BufSize := StreamInfo.cbSize;
  if (BufSize = 0) then
    BufSize := 65536;

  OutSample := nil;
  OutBuffer := nil;
  ContigBuffer := nil;

  if ((StreamInfo.dwFlags and MFT_OUTPUT_STREAM_PROVIDES_SAMPLES) = 0) then
    begin

      Result := MFCreateSample(OutSample);
      if Failed(Result) then
        Exit;

      Result := MFCreateMemoryBuffer(BufSize,
                                     OutBuffer);
      if Failed(Result) then
        Exit;

      Result := OutSample.AddBuffer(OutBuffer);
      if Failed(Result) then
        Exit;
    end;

  //OutputBuffer := Default(MFT_OUTPUT_DATA_BUFFER);
  // OutputBuffer.dwStreamID := 0;
  // OutputBuffer.dwStatus := 0;
  // OutputBuffer.pEvents := nil;
  // or
  ZeroMemory(@OutputBuffer,
                 SizeOf(OutputBuffer));
  OutputBuffer.pSample := OutSample;


  Status := 0;

  try

    Result := FTransform.ProcessOutput(0,
                                       1,
                                       @OutputBuffer,
                                       Status);

    if (Result = MF_E_TRANSFORM_NEED_MORE_INPUT) then
      Exit(S_FALSE);

    if Failed(Result) then
      Exit;

    if (OutputBuffer.pSample = nil) then
      OutputBuffer.pSample := OutSample;

    if (OutputBuffer.pSample = nil) then
      Exit(S_FALSE);

    Result := OutputBuffer.pSample.ConvertToContiguousBuffer(@ContigBuffer);
    if Failed(Result) then
      Exit;

    Result := ContigBuffer.Lock(pData,
                                @MaxLen,
                                @CurLen);
    if Failed(Result) then
      Exit;

    try

      if (CurLen > 0) then
        begin

          SetLength(AOutBuf,
                    CurLen);
          Move(pData^,
               AOutBuf[0],
               CurLen);
        end;
    finally

      ContigBuffer.Unlock;
    end;

    Result := S_OK;
  finally

    OutputBuffer.pEvents := nil;
    OutputBuffer.pSample := nil;
    ContigBuffer := nil;
    OutBuffer := nil;
    OutSample := nil;
  end;
end;


function TMfBroadcastEncoderAac.EncodeInterleavedFloat32(const pData: PSingle;
                                                         AFrames: Integer;
                                                         out AOutBuf: TBytes): HRESULT;
var
  hr: HResult;
  Sample: IMFSample;
  Chunk: TBytes;
  Combined: TBytes;
  OldLen: Integer;
  ChunkLen: Integer;

  procedure AppendChunk(const AChunk: TBytes);
  var
    LOldLen: Integer;
    LChunkLen: Integer;

  begin

    LChunkLen := Length(AChunk);
    if (LChunkLen <= 0) then
      Exit;

    LOldLen := Length(Combined);
    SetLength(Combined,
              LOldLen + LChunkLen);
    Move(AChunk[0],
         Combined[LOldLen],
         LChunkLen);
  end;

begin

  SetLength(AOutBuf,
            0);
  SetLength(Combined,
            0);

  hr := CreateInputSample(pData,
                          AFrames,
                          Sample);
  if Failed(hr) then
    Exit(hr);

  hr := FTransform.ProcessInput(0,
                                Sample,
                                0);

  if (hr = MF_E_NOTACCEPTING) then
    begin

      while True do
        begin

          SetLength(Chunk,
                    0);

          hr := DrainOutput(Chunk);

          if (hr = S_FALSE) then
            Break;

          if Failed(hr) then
            Exit(hr);

          AppendChunk(Chunk);
          Chunk := nil;
        end;

      hr := FTransform.ProcessInput(0,
                                    Sample,
                                    0);
    end;

  if Failed(hr) then
    Exit(hr);

  while True do
    begin

      SetLength(Chunk,
                0);

      hr := DrainOutput(Chunk);

      if (hr = S_FALSE) then
        Break;

      if Failed(hr) then
        Exit(hr);

      ChunkLen := Length(Chunk);
      if (ChunkLen <= 0) then
        Break;

      OldLen := Length(Combined);
      SetLength(Combined,
                OldLen + ChunkLen);
      Move(Chunk[0],
           Combined[OldLen],
           ChunkLen);

      Chunk := nil;
    end;

  AOutBuf := Combined;
  Result := S_OK;
end;


procedure TMfBroadcastEncoderAac.Flush(out AOutBuf: TBytes);
var
  hr: HResult;
  Chunk: TBytes;
  Combined: TBytes;
  OldLen: Integer;
  ChunkLen: Integer;

begin

  SetLength(AOutBuf,
            0);
  SetLength(Combined,
            0);

  if Assigned(FTransform) then
    begin

      FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM,
                                0);

      FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN,
                                0);

      while True do
        begin

          SetLength(Chunk,
                    0);

          hr := DrainOutput(Chunk);

          if (hr = S_FALSE) then
            Break;

          if Failed(hr) then
            Break;

          ChunkLen := Length(Chunk);
          if (ChunkLen <= 0) then
            Break;

          OldLen := Length(Combined);
          SetLength(Combined,
                    OldLen + ChunkLen);
          Move(Chunk[0],
               Combined[OldLen],
               ChunkLen);

          Chunk := nil;
        end;
    end;

  AOutBuf := Combined;
end;


end.
