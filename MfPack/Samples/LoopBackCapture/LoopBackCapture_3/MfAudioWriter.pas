// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioWriter.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.2.0
// Description: WAV/FLAC Writer class using IMFSinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
//==============================================================================
// Source: Microsoft.
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
unit MfAudioWriter;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfReadWrite,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  Common;

type

  TAudioOutputKind = (aokWav, aokFlac);

  IAudioWriter = interface
    ['{B2D1F33F-8C7D-4E8D-9A6E-0E5C5C9C1A7B}']

    function Open(const FileName: string;
                  const Wf: WAVEFORMATEX): HResult;

    function WriteFrames(const Data: Pointer;
                         NumFrames: UINT32;
                         const Wf: WAVEFORMATEX;
                         var Time100ns: Int64;
                         out BytesWritten: UINT32): HResult;

    function Close(): HResult;
    function Kind(): TAudioOutputKind;
  end;

  TMfAudioWriter = class(TInterfacedObject, IAudioWriter)
  private

    FKind: TAudioOutputKind;
    FFileName: string;

    FSinkWriter: IMFSinkWriter;
    FAudioStreamIndex: DWORD;

    // Input properties we actually feed to MF (may differ from captured Wf if we convert)
    FInSampleRate: UINT32;
    FInChannels: UINT32;
    FInBitsPerSample: UINT32;
    FInBlockAlign: UINT32;
    FInAvgBytesPerSec: UINT32;

    // Source properties (captured Wf)
    FSrcBitsPerSample: UINT32;
    FSrcIsFloat: Boolean;

    // Conversion buffer (also used for silence)
    FConvBuf: TBytes;

    // MF init refcount
    class var FStartupCS: TCriticalSection;
    class var FStartupRef: Integer;
    class constructor ClassCreate();
    class destructor ClassDestroy();

    class function EnsureMFStarted(): HResult; static;
    class procedure EnsureMFStopped(); static;

    function GetChannelMask(const Wf: WAVEFORMATEX): UINT32;
    function IsFloatFormat(const Wf: WAVEFORMATEX): Boolean;
    function IsExtensible(const Wf: WAVEFORMATEX): Boolean;
    function GetExtensibleSubFormatGUID(const Wf: WAVEFORMATEX;
                                        out SubFmt: TGUID): Boolean;

    function MakeAudioType(const Major, SubType: TGUID;
                           SampleRate,
                           Channels,
                           BitsPerSample,
                           BlockAlign,
                           AvgBps,
                           ChannelMask: UINT32): IMFMediaType;

    function SetupTypes(const Wf: WAVEFORMATEX): HResult;

    procedure ConvertFloatToPCM24Packed(const Src: Pointer;
                                        NumFrames: UINT32;
                                        Channels: UINT32;
                                        out DstPtr: Pointer;
                                        out DstBytes: UINT32);

    procedure ConvertPCM32ToPCM24Packed(const Src: Pointer;
                                        NumFrames: UINT32;
                                        Channels: UINT32;
                                        out DstPtr: Pointer;
                                        out DstBytes: UINT32);

    function GetValidBitsPerSample(const Wf: WAVEFORMATEX): UINT32;

  public

    constructor Create(AKind: TAudioOutputKind);
    destructor Destroy; override;

    function Kind(): TAudioOutputKind;

    function Open(const FileName: string;
                  const Wf: WAVEFORMATEX): HResult;

    function WriteFrames(const Data: Pointer;
                         NumFrames: UINT32;
                         const Wf: WAVEFORMATEX;
                         var Time100ns: Int64;
                         out BytesWritten: UINT32): HResult;

    function Close(): HResult;
  end;


  function CreateAudioWriterFromFileName(const FileName: string): IAudioWriter;


implementation


uses
  WinApi.ActiveX; // CoTaskMemFree


{ TMfAudioWriter }

class constructor TMfAudioWriter.ClassCreate;
begin

  FStartupCS := TCriticalSection.Create();
  FStartupRef := 0;
end;


class destructor TMfAudioWriter.ClassDestroy();
begin

  FreeAndNil(FStartupCS);
end;


class function TMfAudioWriter.EnsureMFStarted: HResult;
begin

  FStartupCS.Enter;

  try
    if (FStartupRef = 0) then
      Result := MFStartup(MF_VERSION,
                          MFSTARTUP_FULL)
    else
      Result := S_OK;

    if SUCCEEDED(Result) then
      Inc(FStartupRef);
  finally

    FStartupCS.Leave;
  end;
end;


class procedure TMfAudioWriter.EnsureMFStopped;
begin

  FStartupCS.Enter;
  try
    if (FStartupRef > 0) then
      begin

        Dec(FStartupRef);
        if (FStartupRef = 0) then
          MFShutdown();
    end;
  finally

    FStartupCS.Leave;
  end;
end;


constructor TMfAudioWriter.Create(AKind: TAudioOutputKind);
begin

  inherited Create();

  FKind := AKind;
  FSinkWriter := nil;
  FAudioStreamIndex := 0;
  SetLength(FConvBuf, 0);
end;


destructor TMfAudioWriter.Destroy();
begin

  Close();

  inherited Destroy();
end;


function TMfAudioWriter.Kind(): TAudioOutputKind;
begin

  Result := FKind;
end;


function TMfAudioWriter.IsExtensible(const Wf: WAVEFORMATEX): Boolean;
begin

  Result := (Wf.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and
            (Wf.cbSize >= (SizeOf(WAVEFORMATEXTENSIBLE) - SizeOf(WAVEFORMATEX)));
end;


function TMfAudioWriter.GetExtensibleSubFormatGUID(const Wf: WAVEFORMATEX;
                                                   out SubFmt: TGUID): Boolean;
var
  pExt: PWAVEFORMATEXTENSIBLE;

begin

  Result := False;
  ZeroMemory(@SubFmt,
             SizeOf(SubFmt));

  if not IsExtensible(Wf) then
    Exit;

  pExt := PWAVEFORMATEXTENSIBLE(@Wf);
  SubFmt := pExt.SubFormat;
  Result := True;
end;


function TMfAudioWriter.IsFloatFormat(const Wf: WAVEFORMATEX): Boolean;
var
  sub: TGUID;

begin

  if Wf.wFormatTag = WAVE_FORMAT_IEEE_FLOAT then
    Exit(True);

  if GetExtensibleSubFormatGUID(Wf,
                                sub) then
    Exit(IsEqualGUID(sub,
         KSDATAFORMAT_SUBTYPE_IEEE_FLOAT));

  Result := False;
end;


function TMfAudioWriter.GetChannelMask(const Wf: WAVEFORMATEX): UINT32;
var
  pExt: PWAVEFORMATEXTENSIBLE;

begin

  if IsExtensible(Wf) then
    begin

      pExt := PWAVEFORMATEXTENSIBLE(@Wf);
      Result := pExt.dwChannelMask;
      if (Result <> 0) then
        Exit;
    end;

  // Sensible defaults if mask missing
  case Wf.nChannels of
    1: Result := $00000004; // SPEAKER_FRONT_CENTER
    2: Result := $00000003; // SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT
  else
    Result := 0; // unknown
  end;
end;


function TMfAudioWriter.MakeAudioType(const Major,
                                            SubType: TGUID;
                                      SampleRate,
                                      Channels,
                                      BitsPerSample,
                                      BlockAlign,
                                      AvgBps,
                                      ChannelMask: UINT32): IMFMediaType;
var
  hr: HResult;
  mt: IMFMediaType;

begin

  Result := nil;

  hr := MFCreateMediaType(mt);
  if FAILED(hr) then
    Exit;

  hr := mt.SetGUID(MF_MT_MAJOR_TYPE,
                   Major);
  if FAILED(hr) then
    Exit;

  hr := mt.SetGUID(MF_MT_SUBTYPE,
                   SubType);
  if FAILED(hr) then
    Exit;

  mt.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
               SampleRate);

  mt.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
               Channels);

  mt.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
               BitsPerSample);

  mt.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
               BlockAlign);

  mt.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
               AvgBps);

  if (ChannelMask <> 0) then
    mt.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK,
                 ChannelMask);

  Result := mt;
end;


function TMfAudioWriter.SetupTypes(const Wf: WAVEFORMATEX): HResult;
var
  hr: HResult;
  outType, inType: IMFMediaType;
  chMask: UINT32;

  outSub: TGUID;
  inSub: TGUID;

  bits: UINT32;

begin

  FInSampleRate := Wf.nSamplesPerSec;
  FInChannels := Wf.nChannels;

  FSrcBitsPerSample := GetValidBitsPerSample(Wf);
  FSrcIsFloat := IsFloatFormat(Wf);

  chMask := GetChannelMask(Wf);

  // Decide output/input subtypes & format
  if (FKind = aokWav) then
    begin

      // For WAV: keep native mix format (float stays float) to preserve "as-captured".
      if FSrcIsFloat then
        begin

          inSub := MFAudioFormat_Float;
          outSub := MFAudioFormat_Float;
          bits := 32;
        end
      else
        begin

          inSub := MFAudioFormat_PCM;
          outSub := MFAudioFormat_PCM;
          bits := Wf.wBitsPerSample;

          if (bits = 0) then
            bits := 16;
        end;
    end
  else
    begin

      // For FLAC: feed integer PCM, prefer 24-bit when source has enough precision
      outSub := MFAudioFormat_FLAC;
      inSub := MFAudioFormat_PCM;

      if FSrcIsFloat then
        bits := 24
      else
        begin

          bits := Wf.wBitsPerSample;

          if (bits < 16) then
            bits := 16;

          if (bits >= 24) then
            bits := 24
          else
            bits := 16;
        end;
    end;

  FInBitsPerSample := bits;
  FInBlockAlign := (FInChannels * (FInBitsPerSample div 8));
  FInAvgBytesPerSec := (FInSampleRate * FInBlockAlign);

  outType := MakeAudioType(MFMediaType_Audio,
                           outSub,
                           FInSampleRate,
                           FInChannels,
                           FInBitsPerSample,
                           FInBlockAlign,
                           FInAvgBytesPerSec,
                           chMask);

  if (outType = nil) then
    Exit(E_OUTOFMEMORY);

  hr := FSinkWriter.AddStream(outType,
                              FAudioStreamIndex);
  if FAILED(hr) then
    Exit(hr);

  inType := MakeAudioType(MFMediaType_Audio,
                          inSub,
                          FInSampleRate,
                          FInChannels,
                          FInBitsPerSample,
                          FInBlockAlign,
                          FInAvgBytesPerSec,
                          chMask);

  if (inType = nil) then
    Exit(E_OUTOFMEMORY);

  hr := FSinkWriter.SetInputMediaType(FAudioStreamIndex,
                                      inType,
                                      nil);
  if FAILED(hr) then
    Exit(hr);

  hr := FSinkWriter.BeginWriting();
  if FAILED(hr) then
    Exit(hr);

  Result := S_OK;
end;


function TMfAudioWriter.Open(const FileName: string;
                             const Wf: WAVEFORMATEX): HResult;
var
  hr: HResult;

begin

  if FileName = '' then
    Exit(E_INVALIDARG);

  hr := EnsureMFStarted();
  if FAILED(hr) then
    Exit(hr);

  FFileName := FileName;
  FSinkWriter := nil;
  FAudioStreamIndex := 0;

  hr := MFCreateSinkWriterFromURL(PWideChar(FFileName),
                                  nil,
                                  nil,
                                  FSinkWriter);
  if FAILED(hr) then
    begin

      EnsureMFStopped();
      Exit(hr);
    end;

  Result := SetupTypes(Wf);
  if FAILED(Result) then
    begin

      // Clean up and balance MFStartup refcount.
      FSinkWriter := nil;
      EnsureMFStopped();
    end;
end;


procedure TMfAudioWriter.ConvertFloatToPCM24Packed(const Src: Pointer;
                                                   NumFrames: UINT32;
                                                   Channels: UINT32;
                                                   out DstPtr: Pointer;
                                                   out DstBytes: UINT32);
var
  totalSamples: NativeUInt;
  needBytes: NativeUInt;
  i: NativeUInt;
  inF: PSingle;
  outB: PByte;
  v: Single;
  s: Integer;

begin

  // Src is interleaved float32 [-1..1]
  totalSamples := NativeUInt(NumFrames) * NativeUInt(Channels);
  needBytes := totalSamples * 3;

  if (Length(FConvBuf) < Integer(needBytes)) then
    SetLength(FConvBuf,
              Integer(needBytes));

  inF := PSingle(Src);
  outB := @FConvBuf[0];

  for i := 0 to totalSamples - 1 do
    begin

      v := inF^;
      if (v > 1.0) then
        v := 1.0
      else
        if (v < -1.0) then
          v := -1.0;

    // Signed 24-bit range
    s := Round(v * 8388607.0);

    if (s > 8388607) then
      s := 8388607
    else
      if (s < -8388608) then
        s := -8388608;

    // Pack little-endian 24-bit
    outB^ := Byte(s and $FF);
    Inc(outB);

    outB^ := Byte((s shr 8) and $FF);
    Inc(outB);

    outB^ := Byte((s shr 16) and $FF);
    Inc(outB);

    Inc(inF);
  end;

  DstPtr := @FConvBuf[0];
  DstBytes := UINT32(needBytes);
end;


function TMfAudioWriter.GetValidBitsPerSample(const Wf: WAVEFORMATEX): UINT32;
var
  pExt: PWAVEFORMATEXTENSIBLE;

begin

  Result := Wf.wBitsPerSample;

  if IsExtensible(Wf) then
    begin

      pExt := PWAVEFORMATEXTENSIBLE(@Wf);
      if (pExt.Samples.wValidBitsPerSample <> 0) then
        Result := pExt.Samples.wValidBitsPerSample;
    end;
end;


procedure TMfAudioWriter.ConvertPCM32ToPCM24Packed(const Src: Pointer; NumFrames: UINT32; Channels: UINT32;
  out DstPtr: Pointer; out DstBytes: UINT32);
var
  totalSamples: NativeUInt;
  needBytes: NativeUInt;
  i: NativeUInt;
  inS: PInteger;
  outB: PByte;
  s24: Integer;

begin

  // Treat input as signed 32-bit PCM, convert to signed 24-bit packed.
  totalSamples := NativeUInt(NumFrames) * NativeUInt(Channels);
  needBytes := totalSamples * 3;

  if Length(FConvBuf) < Integer(needBytes) then
    SetLength(FConvBuf, Integer(needBytes));

  inS := PInteger(Src);
  outB := @FConvBuf[0];

  for i := 0 to totalSamples - 1 do
    begin

      // Downshift 32->24 (drop least significant 8 bits)
      s24 := (inS^ div 256);

      if (s24 > 8388607) then
        s24 := 8388607
      else
        if (s24 < -8388608) then
          s24 := -8388608;

      outB^ := Byte(s24 and $FF);
      Inc(outB);

      outB^ := Byte((s24 shr 8) and $FF);
      Inc(outB);

      outB^ := Byte((s24 shr 16) and $FF);
      Inc(outB);

      Inc(inS);
    end;

  DstPtr := @FConvBuf[0];
  DstBytes := UINT32(needBytes);
end;


function TMfAudioWriter.WriteFrames(const Data: Pointer; NumFrames: UINT32; const Wf: WAVEFORMATEX;
  var Time100ns: Int64; out BytesWritten: UINT32): HResult;
var
  hr: HResult;
  sample: IMFSample;
  buf: IMFMediaBuffer;
  pDst: PByte;
  maxLen, curLen: DWORD;

  srcPtr: Pointer;
  bytesToWrite: UINT32;

  dur100ns: Int64;
begin

  BytesWritten := 0;

  if FSinkWriter = nil then Exit(MF_E_NOT_INITIALIZED);

  // Decide actual source pointer/size we feed to MF (maybe converted)
  if (FKind = aokFlac) then
  begin
    // FLAC input is integer PCM at FInBitsPerSample (16 or 24)
    if FInBitsPerSample = 24 then
    begin
      bytesToWrite := NumFrames * (FInChannels * 3);

      if Data = nil then
      begin
        if Length(FConvBuf) < Integer(bytesToWrite) then
          SetLength(FConvBuf, Integer(bytesToWrite));
        FillChar(FConvBuf[0], bytesToWrite, 0);
        srcPtr := @FConvBuf[0];
      end
      else
      begin
        if FSrcIsFloat then
          ConvertFloatToPCM24Packed(Data, NumFrames, FInChannels, srcPtr, bytesToWrite)
        else if (FSrcBitsPerSample = 32) then
          ConvertPCM32ToPCM24Packed(Data, NumFrames, FInChannels, srcPtr, bytesToWrite)
        else
        begin
          // Assume packed 24-bit PCM already
          srcPtr := Data;
        end;
      end;
    end
    else
    begin
      // 16-bit PCM
      bytesToWrite := NumFrames * (FInChannels * 2);

      if Data = nil then
      begin
        if Length(FConvBuf) < Integer(bytesToWrite) then
          SetLength(FConvBuf, Integer(bytesToWrite));
        FillChar(FConvBuf[0], bytesToWrite, 0);
        srcPtr := @FConvBuf[0];
      end
      else
        srcPtr := Data;
    end;
  end
  else
  begin
    // WAV path (float or PCM)
    bytesToWrite := NumFrames * FInBlockAlign;

    if Data = nil then
    begin
      if Length(FConvBuf) < Integer(bytesToWrite) then
        SetLength(FConvBuf, Integer(bytesToWrite));
      FillChar(FConvBuf[0], bytesToWrite, 0);
      srcPtr := @FConvBuf[0];
    end
    else
      srcPtr := Data;
  end;

  // Build MF sample
  hr := MFCreateSample(sample);
  if FAILED(hr) then Exit(hr);

  hr := MFCreateMemoryBuffer(bytesToWrite, buf);
  if FAILED(hr) then Exit(hr);

  hr := buf.Lock(pDst, @maxLen, @curLen);
  if FAILED(hr) then Exit(hr);

  try
    if bytesToWrite > 0 then
      Move(PByte(srcPtr)^, pDst^, bytesToWrite);
  finally
    buf.Unlock;
  end;

  hr := buf.SetCurrentLength(bytesToWrite);
  if FAILED(hr) then Exit(hr);

  hr := sample.AddBuffer(buf);
  if FAILED(hr) then Exit(hr);

  // Timestamping: sample-counter clock (stable)
  dur100ns := _MulDiv64(Int64(NumFrames), 10000000, Int64(FInSampleRate));

  hr := sample.SetSampleTime(Time100ns);
  if FAILED(hr) then Exit(hr);

  hr := sample.SetSampleDuration(dur100ns);
  if FAILED(hr) then Exit(hr);

  hr := FSinkWriter.WriteSample(FAudioStreamIndex, sample);
  if FAILED(hr) then Exit(hr);

  Inc(Time100ns, dur100ns);
  BytesWritten := bytesToWrite;
  Result := S_OK;
end;

function TMfAudioWriter.Close: HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  if FSinkWriter <> nil then
  begin
    // Finalize flushes headers/trailers etc.
    hr := FSinkWriter.Finalize;
    FSinkWriter := nil;
  end;

  EnsureMFStopped;
  Result := hr;
end;

function CreateAudioWriterFromFileName(const FileName: string): IAudioWriter;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if ext = '.flac' then
    Result := TMfAudioWriter.Create(aokFlac)
  else
    Result := TMfAudioWriter.Create(aokWav);
end;

end.


