// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioDelayMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Sample 14 extends the delay MFT with caller-allocated output samples.
//              TInterfacedObject supplies IUnknown; the class only implements its interfaces.
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
unit MfAudioDelayMFT;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Math,
  System.UITypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

const
  MF_AUDIODELAY_DELAY_LENGTH: TGUID = '{95915546-B07C-4234-A237-1AF27187DEEE}';
  MF_AUDIODELAY_WET_DRY_MIX: TGUID =  '{72127F43-5878-4EA8-8269-D1AF3BB11CB2}';


type

  // Optional control interface for changing the effect during playback.
  IMfAudioDelayControl = interface(IUnknown)
  ['{48222613-78FC-4A40-B95B-7E67E22443EE}']
    function SetEffect(const ADelayMs: Cardinal;
                       const AWetPercent: Cardinal): HResult; stdcall;
  end;

  TMfAudioDelayMFT = class(TInterfacedObject, IMFTransform, IMfAudioDelayControl)
  private
    FInputType: IMFMediaType;
    FOutputType: IMFMediaType;
    FAttributes: IMFAttributes;
    FInputSample: IMFSample;
    FDelay: TBytes;
    FDelayPosition: Cardinal;
    FBlockAlign: Cardinal;
    FRate: Cardinal;
    FBits: Cardinal;
    FChannels: Cardinal;
    FTailBytes: Cardinal;
    FDraining: Boolean;
    FValidTime: Boolean;
    FNextTime: Int64;

    procedure ResetStream(const AReleaseDelay: Boolean);

    function ValidateType(const AType: IMFMediaType;
                          out ABlockAlign: Cardinal;
                          out ARate: Cardinal;
                          out ABits: Cardinal;
                          out AChannels: Cardinal): HResult;

    function SameFormat(const AType: IMFMediaType): Boolean;
    function CloneType(const ASource: IMFMediaType;
                       out ACopy: IMFMediaType): HResult;

    function ProposedType(const AIndex: DWORD;
                          out AType: IMFMediaType): HResult;

    function StartDelay(): HResult;
    function ResizeDelay(const ANewBytes: Cardinal): HResult;

    procedure ProcessPcm(const AData: PByte;
                         const ABytes: Cardinal);

    function ProcessPending(var AOutput: MFT_OUTPUT_DATA_BUFFER): HResult;
    function ProcessTail(var AOutput: MFT_OUTPUT_DATA_BUFFER): HResult;

  public

    destructor Destroy(); override;

    function SetEffect(const ADelayMs,
                       AWetPercent: Cardinal): HResult; stdcall;

    function GetStreamLimits(out pdwInputMinimum: DWORD;
                             out pdwInputMaximum: DWORD;
                             out pdwOutputMinimum: DWORD;
                             out pdwOutputMaximum: DWORD): HResult; stdcall;

    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HResult; stdcall;

    function GetStreamIDs(dwInputIDArraySize: DWORD;
                          pdwInputIDs: WinApi.WinApiTypes.PDWORD;
                          dwOutputIDArraySize: DWORD;
                          pdwOutputIDs: WinApi.WinApiTypes.PDWORD): HResult; stdcall;

    function GetInputStreamInfo(const dwInputStreamID: DWORD;
                                out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult; stdcall;

    function GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                 out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult; stdcall;

    function GetAttributes(out pAttributes: IMFAttributes): HResult; stdcall;

    function GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                      out pAttributes: IMFAttributes): HResult; stdcall;

    function GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                       out pAttributes: IMFAttributes): HResult; stdcall;

    function DeleteInputStream(dwStreamID: DWORD): HResult; stdcall;
    function AddInputStreams(cStreams: DWORD;
                             adwStreamIDs: WinApi.WinApiTypes.PDWORD): HResult; stdcall;

    function GetInputAvailableType(const dwInputStreamID: DWORD;
                                   dwTypeIndex: DWORD;
                                   out pType: IMFMediaType): HResult; stdcall;

    function GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                    dwTypeIndex: DWORD;
                                    out pType: IMFMediaType): HResult; stdcall;

    function SetInputType(const dwInputStreamID: DWORD;
                          pType: IMFMediaType;
                          dwFlags: DWORD): HResult; stdcall;

    function SetOutputType(dwOutputStreamID: DWORD;
                           pType: IMFMediaType;
                           dwFlags: DWORD): HResult; stdcall;

    function GetInputCurrentType(const dwInputStreamID: DWORD;
                                 out pType: IMFMediaType): HResult; stdcall;

    function GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                  out pType: IMFMediaType): HResult; stdcall;

    function GetInputStatus(const dwInputStreamID: DWORD;
                            out pdwFlags: DWORD): HResult; stdcall;

    function GetOutputStatus(out pdwFlags: DWORD): HResult; stdcall;

    function SetOutputBounds(hnsLowerBound: LONGLONG;
                             hnsUpperBound: LONGLONG): HResult; stdcall;

    function ProcessEvent(const dwInputStreamID: DWORD;
                          pEvent: IMFMediaEvent): HResult; stdcall;

    function ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                            ulParam: ULONG_PTR): HResult; stdcall;

    function ProcessInput(const dwInputStreamID: DWORD;
                          const pSample: IMFSample;
                          dwFlags: DWORD = 0): HResult; stdcall;

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HResult; stdcall;
  end;


implementation

const
  DefaultDelayMs     = 2000;
  DefaultWetPercent  = 50;
  TimeUnitsPerSecond = 10000000;


destructor TMfAudioDelayMFT.Destroy();
begin

  ResetStream(True);

  inherited;
end;


procedure TMfAudioDelayMFT.ResetStream(const AReleaseDelay: Boolean);
begin

  FInputSample := nil;
  FTailBytes := 0;
  FDraining := False;
  FValidTime := False;
  FDelayPosition := 0;

  if AReleaseDelay then
    SetLength(FDelay,
              0)
  else
    if (Length(FDelay) > 0) then
      FillChar(FDelay[0],
               Length(FDelay),
               IfThen(FBits = 8, $80, 0));
end;


function TMfAudioDelayMFT.ValidateType(const AType: IMFMediaType;
                                       out ABlockAlign: Cardinal;
                                       out ARate: Cardinal;
                                       out ABits: Cardinal;
                                       out AChannels: Cardinal): HResult;
var
  Major: TGUID;
  Subtype: TGUID;
  Avg: Cardinal;

begin

  ABlockAlign := 0;
  ARate := 0;
  ABits := 0;
  AChannels := 0;

  if not Assigned(AType) then
    Exit(E_POINTER);

  if FAILED(AType.GetGUID(MF_MT_MAJOR_TYPE,
                          Major)) or
     FAILED(AType.GetGUID(MF_MT_SUBTYPE,
                          Subtype)) or not
     IsEqualGUID(Major,
                 MFMediaType_Audio) or not
     IsEqualGUID(Subtype,
                 MFAudioFormat_PCM) or
     FAILED(AType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                            AChannels)) or
     FAILED(AType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            ARate)) or
     FAILED(AType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                            ABits)) or
     FAILED(AType.GetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                            ABlockAlign)) or
     FAILED(AType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                            Avg)) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if ((AChannels <> 1) and (AChannels <> 2)) or
     ((ABits <> 8) and (ABits <> 16)) or
     (ARate = 0) or (ABlockAlign <> AChannels * (ABits div 8)) or
     (ARate > High(Cardinal) div ABlockAlign) or
     (Avg <> ARate * ABlockAlign) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := S_OK;
end;


function TMfAudioDelayMFT.SameFormat(const AType: IMFMediaType): Boolean;
var
  BlockAlign: Cardinal;
  Rate: Cardinal;
  Bits: Cardinal;
  Channels: Cardinal;

begin

  BlockAlign := 0;
  Rate := 0;
  Bits := 0;
  Channels := 0;

  Result := SUCCEEDED(ValidateType(AType,
                                   BlockAlign,
                                   Rate,
                                   Bits,
                                   Channels)) and
           (BlockAlign = FBlockAlign) and
           (Rate = FRate) and
           (Bits = FBits) and
           (Channels = FChannels);
end;


function TMfAudioDelayMFT.CloneType(const ASource: IMFMediaType;
                                    out ACopy: IMFMediaType): HResult;
begin

  ACopy := nil;
  Result := MFCreateMediaType(ACopy);

  if SUCCEEDED(Result) then
    Result := ASource.CopyAllItems(ACopy);
end;


function TMfAudioDelayMFT.ProposedType(const AIndex: DWORD;
                                       out AType: IMFMediaType): HResult;
begin

  AType := nil;

  if (AIndex > 1) then
    Exit(MF_E_NO_MORE_TYPES);

  Result := MFCreateMediaType(AType);

  if SUCCEEDED(Result) then
    Result := AType.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Audio);

  if SUCCEEDED(Result) then
    Result := AType.SetGUID(MF_MT_SUBTYPE,
                            MFAudioFormat_PCM);

  if SUCCEEDED(Result) and (AIndex = 1) then
    begin
      Result := AType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                48000);

      if SUCCEEDED(Result) then
        Result := AType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                  16);

      if SUCCEEDED(Result) then
        Result := AType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                  2);

      if SUCCEEDED(Result) then
        Result := AType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                  4);

      if SUCCEEDED(Result) then
        Result := AType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                  192000);

      if SUCCEEDED(Result) then
        Result := AType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                                  1);
    end;
end;


function TMfAudioDelayMFT.StartDelay(): HResult;
var
  Milliseconds: Cardinal;
  Bytes: UInt64;
  Attributes: IMFAttributes;

begin

  if not Assigned(FInputType) or not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  Result := GetAttributes(Attributes);
  if FAILED(Result) then
    Exit;

  Milliseconds := MFGetAttributeUINT32(Attributes,
                                       MF_AUDIODELAY_DELAY_LENGTH,
                                       DefaultDelayMs);

  if (Milliseconds = 0) then
    Milliseconds := DefaultDelayMs;

  Bytes := UInt64(Milliseconds) * FRate * FBlockAlign div 1000;
  Bytes := (Bytes div FBlockAlign) * FBlockAlign;

  if (Bytes < FBlockAlign) or (Bytes > High(Integer)) then
    Exit(E_INVALIDARG);

  Result := ResizeDelay(Cardinal(Bytes));
end;


function TMfAudioDelayMFT.ResizeDelay(const ANewBytes: Cardinal): HResult;
var
  NewDelay: TBytes;
  OldBytes: Cardinal;
  KeepBytes: Cardinal;
  I: Cardinal;
  SourceIndex: Cardinal;

begin

  OldBytes := Cardinal(Length(FDelay));
  if (ANewBytes = OldBytes) then
    Exit(S_OK);

  try
    SetLength(NewDelay, Integer(ANewBytes));
  except
    on EOutOfMemory do
      Exit(E_OUTOFMEMORY);
  end;

  FillChar(NewDelay[0],
           ANewBytes,
           IfThen(FBits = 8, $80, 0));

  // Both buffers represent input history from oldest to newest. Preserve
  // the newest frames when the requested delay changes.
  KeepBytes := OldBytes;
  if (KeepBytes > ANewBytes) then
    KeepBytes := ANewBytes;

  if (KeepBytes <> 0) then
    for I := 0 to KeepBytes - 1 do
      begin
        SourceIndex := (FDelayPosition + OldBytes - KeepBytes + I) mod OldBytes;
        NewDelay[ANewBytes - KeepBytes + I] := FDelay[SourceIndex];
      end;

  FDelay := NewDelay;
  FDelayPosition := 0;
  Result := S_OK;
end;


function TMfAudioDelayMFT.SetEffect(const ADelayMs,
                                    AWetPercent: Cardinal): HResult;
var
  Attributes: IMFAttributes;
  Bytes: UInt64;

begin

  if (ADelayMs = 0) or (ADelayMs > 10000) or (AWetPercent > 100) then
    Exit(E_INVALIDARG);

  TMonitor.Enter(Self);
  try
    Result := GetAttributes(Attributes);
    if FAILED(Result) then
      Exit;

    if (Length(FDelay) <> 0) and not FDraining then
      begin
        Bytes := UInt64(ADelayMs) * FRate * FBlockAlign div 1000;
        Bytes := (Bytes div FBlockAlign) * FBlockAlign;
        if (Bytes < FBlockAlign) or (Bytes > High(Integer)) then
          Exit(E_INVALIDARG);

        Result := ResizeDelay(Cardinal(Bytes));
        if FAILED(Result) then
          Exit;
      end;

    Result := Attributes.SetUINT32(MF_AUDIODELAY_DELAY_LENGTH,
                                   ADelayMs);
    if SUCCEEDED(Result) then
      Result := Attributes.SetUINT32(MF_AUDIODELAY_WET_DRY_MIX,
                                     AWetPercent);
  finally
    TMonitor.Exit(Self);
  end;
end;


procedure TMfAudioDelayMFT.ProcessPcm(const AData: PByte;
                                      const ABytes: Cardinal);
var
  I: Integer;
  Wet: Integer;
  Dry: Integer;
  InputValue: Integer;
  DelayValue: Integer;
  Mixed: Integer;
  InputWord: SmallInt;
  DelayWord: SmallInt;
  P: PByte;

begin

  Wet := MFGetAttributeUINT32(FAttributes,
                              MF_AUDIODELAY_WET_DRY_MIX,
                              DefaultWetPercent);

  if (Wet > 100) then
    Wet := 100;

  Dry := 100 - Wet;
  I := 0;

  while I < Integer(ABytes) do
    begin
      P := @FDelay[FDelayPosition];

      if (FBits = 8) then
        begin
          InputValue := Integer(AData[I]) - 128;
          DelayValue := Integer(P^) - 128;
          P^ := Byte(InputValue + 128);

          Mixed := (InputValue * Dry + DelayValue * Wet) div 100;
          AData[I] := Byte(Mixed + 128);
          Inc(I);
          Inc(FDelayPosition);
        end
      else
        begin
          Move(AData[I],
               InputWord,
               SizeOf(InputWord));

          Move(P^,
               DelayWord,
               SizeOf(DelayWord));

          Move(InputWord,
               P^,
               SizeOf(InputWord));

          Mixed := (Integer(InputWord) * Dry + Integer(DelayWord) * Wet) div 100;

          InputWord := SmallInt(Mixed);
          Move(InputWord,
               AData[I],
               SizeOf(InputWord));
          Inc(I,
              2);
          Inc(FDelayPosition,
              2);
        end;

      if (FDelayPosition >= Cardinal(Length(FDelay))) then
        FDelayPosition := 0;
    end;
end;


function TMfAudioDelayMFT.ProcessPending(var AOutput: MFT_OUTPUT_DATA_BUFFER): HResult;
var
  InputBuffer: IMFMediaBuffer;
  OutputBuffer: IMFMediaBuffer;
  InputData: PByte;
  OutputData: PByte;
  MaxLength, CurrentLength: DWORD;
  OutputCapacity: DWORD;
  SampleTime: Int64;
  SampleDuration: Int64;
  SampleFlags: DWORD;
  Sample: IMFSample;

begin

  Sample := FInputSample;
  Result := Sample.ConvertToContiguousBuffer(@InputBuffer);

  if FAILED(Result) then
    Exit;

  Result := InputBuffer.GetCurrentLength(CurrentLength);
  if FAILED(Result) then
    Exit;

  if ((CurrentLength mod FBlockAlign) <> 0) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if Assigned(AOutput.pSample) then
    begin
      // Capacity is checked before changing delay state so the caller can
      // retry a failed call with a larger buffer.
      Result := AOutput.pSample.ConvertToContiguousBuffer(@OutputBuffer);
      if FAILED(Result) then
        Exit;

      Result := OutputBuffer.GetMaxLength(OutputCapacity);
      if FAILED(Result) then
        Exit;

      if (OutputCapacity < CurrentLength) then
        Exit(MF_E_BUFFERTOOSMALL);

      Result := InputBuffer.Lock(InputData,
                                 @MaxLength,
                                 nil);
      if FAILED(Result) then
        Exit;
      try
        Result := OutputBuffer.Lock(OutputData,
                                    @MaxLength,
                                    nil);
        if FAILED(Result) then
          Exit;
        try
          Move(InputData^,
               OutputData^,
               CurrentLength);
          ProcessPcm(OutputData,
                     CurrentLength);
        finally
          OutputBuffer.Unlock();
        end;
      finally
        InputBuffer.Unlock();
      end;

      Result := OutputBuffer.SetCurrentLength(CurrentLength);
      if FAILED(Result) then
        Exit;

      // Sample time, duration and flags are not IMFAttributes items.
      Result := Sample.CopyAllItems(AOutput.pSample);
      if FAILED(Result) then
        Exit;

      if SUCCEEDED(Sample.GetSampleTime(@SampleTime)) then
        begin
          Result := AOutput.pSample.SetSampleTime(SampleTime);
          if FAILED(Result) then
            Exit;
        end;

      if SUCCEEDED(Sample.GetSampleDuration(@SampleDuration)) then
        begin
          Result := AOutput.pSample.SetSampleDuration(SampleDuration);
          if FAILED(Result) then
            Exit;
        end;

      Result := Sample.GetSampleFlags(@SampleFlags);
      if FAILED(Result) then
        Exit;

      Result := AOutput.pSample.SetSampleFlags(SampleFlags);
      if FAILED(Result) then
        Exit;
    end
  else
    begin
      Result := InputBuffer.Lock(InputData,
                                 @MaxLength,
                                 nil);
      if FAILED(Result) then
        Exit;
      try
        ProcessPcm(InputData,
                   CurrentLength);
      finally
        InputBuffer.Unlock();
      end;

      // Preserve the in-place path when the caller supplies no sample.
      AOutput.pSample := Sample;
    end;

  FInputSample := nil;
  Result := S_OK;
end;


function TMfAudioDelayMFT.ProcessTail(var AOutput: MFT_OUTPUT_DATA_BUFFER): HResult;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  Data: PByte;
  Capacity: DWORD;
  Count: DWORD;
  Duration: Int64;

begin

  Count := FTailBytes;

  if (Count > 4096) then
    Count := (4096 div FBlockAlign) * FBlockAlign;

  if (Count = 0) then
    Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

  Sample := AOutput.pSample;
  if Assigned(Sample) then
    begin
      Result := Sample.ConvertToContiguousBuffer(@Buffer);
      if FAILED(Result) then
        Exit;

      Result := Buffer.GetMaxLength(Capacity);
      if FAILED(Result) then
        Exit;

      if (Capacity < Count) then
        Exit(MF_E_BUFFERTOOSMALL);
    end
  else
    begin
      Result := MFCreateSample(Sample);
      if FAILED(Result) then
        Exit;

      Result := MFCreateMemoryBuffer(Count,
                                     Buffer);
      if FAILED(Result) then
        Exit;

      Result := Sample.AddBuffer(Buffer);
      if FAILED(Result) then
        Exit;
    end;

  Result := Buffer.Lock(Data,
                        @Capacity,
                        nil);
  if FAILED(Result) then
    Exit;

  try
    FillChar(Data^,
             Count,
             IfThen(FBits = 8, $80, 0));

    ProcessPcm(Data,
               Count);

  finally
    Buffer.Unlock;
  end;

  Result := Buffer.SetCurrentLength(Count);

  if FAILED(Result) then
    Exit;

  if FValidTime then
    begin
      Duration := Int64(Count) * TimeUnitsPerSecond div (FRate * FBlockAlign);

      Result := Sample.SetSampleTime(FNextTime);
      if FAILED(Result) then
        Exit;

      Result := Sample.SetSampleDuration(Duration);
      if FAILED(Result) then
        Exit;

      Inc(FNextTime,
          Duration);
    end;

  AOutput.pSample := Sample;
  Dec(FTailBytes,
      Count);

  if (FTailBytes <> 0) then
    AOutput.dwStatus := MFT_OUTPUT_DATA_BUFFER_INCOMPLETE
  else
    FDraining := False;

  Result := S_OK;
end;


function TMfAudioDelayMFT.GetStreamLimits(out pdwInputMinimum: DWORD;
                                          out pdwInputMaximum: DWORD;
                                          out pdwOutputMinimum: DWORD;
                                          out pdwOutputMaximum: DWORD): HResult;
begin

  pdwInputMinimum := 1;
  pdwInputMaximum := 1;
  pdwOutputMinimum := 1;
  pdwOutputMaximum := 1;
  Result := S_OK;
end;


function TMfAudioDelayMFT.GetStreamCount(out pcInputStreams,
                                         pcOutputStreams: DWORD): HResult;
begin

  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfAudioDelayMFT.GetStreamIDs(dwInputIDArraySize: DWORD;
                                       pdwInputIDs: WinApi.WinApiTypes.PDWORD;
                                       dwOutputIDArraySize: DWORD;
                                       pdwOutputIDs: WinApi.WinApiTypes.PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                             out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult;
begin
  TMonitor.Enter(Self);
  try
    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    ZeroMemory(@pStreamInfo,
               SizeOf(pStreamInfo));

    pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or
                           MFT_INPUT_STREAM_PROCESSES_IN_PLACE or
                           MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE;

    if Assigned(FInputType) then
      pStreamInfo.cbSize := FBlockAlign;

    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                              out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult;
begin
  TMonitor.Enter(Self);
  try
    if (dwOutputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    ZeroMemory(@pStreamInfo,
               SizeOf(pStreamInfo));

    // CAN_PROVIDE accepts either a caller sample or a sample supplied by us.
    // PROVIDES_SAMPLES would forbid a non-nil caller sample.
    pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or
                           MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES;

    if Assigned(FOutputType) then
      pStreamInfo.cbSize := FBlockAlign;
    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetAttributes(out pAttributes: IMFAttributes): HResult;
begin
  TMonitor.Enter(Self);
  try
    if not Assigned(FAttributes) then
      begin
        Result := MFCreateAttributes(FAttributes,
                                     2);
        if FAILED(Result) then
          Exit;

        Result := FAttributes.SetUINT32(MF_AUDIODELAY_DELAY_LENGTH,
                                        DefaultDelayMs);
        if FAILED(Result) then
          Exit;
        Result := FAttributes.SetUINT32(MF_AUDIODELAY_WET_DRY_MIX,
                                        DefaultWetPercent);
        if FAILED(Result) then
          Exit;
      end;

    pAttributes := FAttributes;
    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                   out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);
  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                    out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);
  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.DeleteInputStream(dwStreamID: DWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.AddInputStreams(cStreams: DWORD;
                                          adwStreamIDs: WinApi.WinApiTypes.PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.GetInputAvailableType(const dwInputStreamID: DWORD;
                                                dwTypeIndex: DWORD;
                                                out pType: IMFMediaType): HResult;
begin
  TMonitor.Enter(Self);
  try
    pType := nil;

    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if Assigned(FOutputType) then
      begin
        if (dwTypeIndex <> 0) then
          Exit(MF_E_NO_MORE_TYPES);

        Result := CloneType(FOutputType,
                            pType);
      end
    else
      Result := ProposedType(dwTypeIndex,
                             pType);
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                 dwTypeIndex: DWORD;
                                                 out pType: IMFMediaType): HResult;
begin
  TMonitor.Enter(Self);
  try
    pType := nil;

    if (dwOutputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if Assigned(FInputType) then
      begin

        if (dwTypeIndex <> 0) then
          Exit(MF_E_NO_MORE_TYPES);

        Result := CloneType(FInputType,
                            pType);
      end
    else
      Result := ProposedType(dwTypeIndex,
                             pType);
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.SetInputType(const dwInputStreamID: DWORD;
                                       pType: IMFMediaType;
                                       dwFlags: DWORD): HResult;
var
  Align: Cardinal;
  Rate: Cardinal;
  Bits: Cardinal;
  Channels: Cardinal;
  CopyType: IMFMediaType;

begin
  TMonitor.Enter(Self);
  try
    Align := 0;
    Rate := 0;
    Bits := 0;
    Channels := 0;

    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if ((dwFlags and not MFT_SET_TYPE_TEST_ONLY) <> 0) then
      Exit(E_INVALIDARG);

    if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) and not Assigned(pType) then
      Exit(E_INVALIDARG);

    if Assigned(FInputSample) or FDraining then
      Exit(MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING);

    if Assigned(pType) then
      begin
        Result := ValidateType(pType,
                               Align,
                               Rate,
                               Bits,
                               Channels);
        if FAILED(Result) then
          Exit;

        if Assigned(FOutputType) and not SameFormat(pType) then
          Exit(MF_E_INVALIDMEDIATYPE);
      end;

    if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
      Exit(S_OK);
    CopyType := nil;

    if Assigned(pType) then
      begin
        Result := CloneType(pType,
                            CopyType);
        if FAILED(Result) then
          Exit;

        FBlockAlign := Align;
        FRate := Rate;
        FBits := Bits;
        FChannels := Channels;
      end;

    FInputType := CopyType;
    ResetStream(True);
    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.SetOutputType(dwOutputStreamID: DWORD;
                                        pType: IMFMediaType;
                                        dwFlags: DWORD): HResult;
var
  Align: Cardinal;
  Rate: Cardinal;
  Bits: Cardinal;
  Channels: Cardinal;
  CopyType: IMFMediaType;

begin
  TMonitor.Enter(Self);
  try
    Align := 0;
    Rate := 0;
    Bits := 0;
    Channels := 0;

    if (dwOutputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if (dwFlags and not MFT_SET_TYPE_TEST_ONLY) <> 0 then
      Exit(E_INVALIDARG);

    if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) and not Assigned(pType) then
      Exit(E_INVALIDARG);

    if Assigned(FInputSample) or FDraining then
      Exit(MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING);

    if Assigned(pType) then
      begin
        Result := ValidateType(pType,
                               Align,
                               Rate,
                               Bits,
                               Channels);

        if FAILED(Result) then
          Exit;

        if Assigned(FInputType) and not SameFormat(pType) then
          Exit(MF_E_INVALIDMEDIATYPE);
      end;

    if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
      Exit(S_OK);

    CopyType := nil;

    if Assigned(pType) then
      begin
        Result := CloneType(pType,
                            CopyType);
        if FAILED(Result) then
          Exit;

        FBlockAlign := Align;
        FRate := Rate;
        FBits := Bits;
        FChannels := Channels;
      end;

    FOutputType := CopyType;
    ResetStream(True);
    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetInputCurrentType(const dwInputStreamID: DWORD;
                                              out pType: IMFMediaType): HResult;
begin
  TMonitor.Enter(Self);
  try
    pType := nil;

    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if not Assigned(FInputType) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    Result := CloneType(FInputType,
                        pType);
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                               out pType: IMFMediaType): HResult;
begin
  TMonitor.Enter(Self);
  try
    pType := nil;

    if (dwOutputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if not Assigned(FOutputType) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    Result := CloneType(FOutputType,
                        pType);
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetInputStatus(const dwInputStreamID: DWORD;
                                         out pdwFlags: DWORD): HResult;
begin
  TMonitor.Enter(Self);
  try
    pdwFlags := 0;

    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if not Assigned(FInputSample) and not FDraining then
      pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA;

    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.GetOutputStatus(out pdwFlags: DWORD): HResult;
begin
  TMonitor.Enter(Self);
  try
    pdwFlags := 0;

    if Assigned(FInputSample) or (FDraining and (FTailBytes <> 0)) then
      pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY;

    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.SetOutputBounds(hnsLowerBound: LONGLONG;
                                          hnsUpperBound: LONGLONG): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.ProcessEvent(const dwInputStreamID: DWORD;
                                       pEvent: IMFMediaEvent): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);
  Result := E_NOTIMPL;
end;


function TMfAudioDelayMFT.ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                                         ulParam: ULONG_PTR): HResult;
begin
  TMonitor.Enter(Self);
  try
    case eMessage of
      MFT_MESSAGE_COMMAND_FLUSH:          ResetStream(False);
      MFT_MESSAGE_NOTIFY_END_STREAMING:   ResetStream(True);
      MFT_MESSAGE_NOTIFY_BEGIN_STREAMING: Exit(StartDelay);
      MFT_MESSAGE_COMMAND_DRAIN:
        begin
          if (Length(FDelay) <> 0) then
          begin
            FTailBytes := Length(FDelay);
            FDraining := True;
          end;
        end;
      MFT_MESSAGE_SET_D3D_MANAGER: Exit(E_NOTIMPL);
    end;
    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.ProcessInput(const dwInputStreamID: DWORD;
                                       const pSample: IMFSample;
                                       dwFlags: DWORD): HResult;
var
  Buffer: IMFMediaBuffer;
  Bytes: DWORD;
  Time: Int64;
  HasTime: Boolean;
  Discontinuous: Boolean;

begin
  TMonitor.Enter(Self);
  try
    if (dwInputStreamID <> 0) then
      Exit(MF_E_INVALIDSTREAMNUMBER);

    if not Assigned(pSample) then
      Exit(E_POINTER);

    if (dwFlags <> 0) then
      Exit(E_INVALIDARG);

    if not Assigned(FInputType) or not
       Assigned(FOutputType) or
       Assigned(FInputSample) or
       FDraining then
      Exit(MF_E_NOTACCEPTING);

    Result := StartDelay;
    if FAILED(Result) then
     Exit;

    Result := pSample.ConvertToContiguousBuffer(@Buffer);
    if FAILED(Result) then
      Exit;

    Result := Buffer.GetCurrentLength(Bytes);
    if FAILED(Result) then
      Exit;

    if (Bytes = 0) or ((Bytes mod FBlockAlign) <> 0) then
      Exit(MF_E_INVALIDMEDIATYPE);

    HasTime := SUCCEEDED(pSample.GetSampleTime(@Time));
    Discontinuous := MFGetAttributeUINT32(pSample,
                                           MFSampleExtension_Discontinuity,
                                           0) <> 0;
    // Allow up to 5 ms for source timestamp rounding between PCM blocks.
    if HasTime and FValidTime and
       (Abs(Time - FNextTime) > TimeUnitsPerSecond div 200) then
      Discontinuous := True;

    // A seek can arrive as a pipeline flush, a discontinuity, or a timestamp
    // jump. Reset here under the MFT lock, before processing the new sample;
    // the UI must never send a flush into an active Media Session itself.
    if Discontinuous then
      ResetStream(False);

    FInputSample := pSample;
    FValidTime := HasTime;

    if FValidTime then
      FNextTime := Time + Int64(Bytes) * TimeUnitsPerSecond div (FRate * FBlockAlign);

    Result := S_OK;
  finally
    TMonitor.Exit(Self);
  end;
end;


function TMfAudioDelayMFT.ProcessOutput(dwFlags: DWORD;
                                        cOutputBufferCount: DWORD;
                                        pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                                        out pdwStatus: DWORD): HResult;
begin
  TMonitor.Enter(Self);
  try
    pdwStatus := 0;

    if (dwFlags <> 0) then
      Exit(E_INVALIDARG);

    if (cOutputBufferCount <> 1) or not Assigned(pOutputSamples) then
      Exit(E_INVALIDARG);

    if not Assigned(FInputSample) and not (FDraining and (FTailBytes <> 0)) then
      Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

    pOutputSamples^.dwStreamID := 0;
    pOutputSamples^.dwStatus := 0;
    pOutputSamples^.pEvents := nil;

    if Assigned(FInputSample) then
      Result := ProcessPending(pOutputSamples^)
    else
      Result := ProcessTail(pOutputSamples^);
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
