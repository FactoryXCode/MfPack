// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioBassTrebleMFT.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.9
// Description: MFT eq for bass and treble.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
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
unit MfAudioBassTrebleMFT;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {Application MFT}
  MfAudioBassTrebleTypes;

type

  TBiquad = record
    b0, b1, b2: Single;
    a1, a2: Single;
    z1, z2: Single;
  end;

  TSingleArray = array[0..$7FFFFFF] of Single;
  PSingleArray = ^TSingleArray;

  TMfBassTrebleMFT = class(TInterfacedObject, IMFTransform, IMfBassTrebleControl)

{$region 'IMFTransform implementation'}
    // IMFTransform implementation ////////////////////////////////////////////
    function GetStreamLimits(out pdwInputMinimum: DWORD;
                             out pdwInputMaximum: DWORD;
                             out pdwOutputMinimum: DWORD;
                             out pdwOutputMaximum: DWORD): HRESULT; stdcall;

    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HRESULT; stdcall;

    function GetStreamIDs(dwInputIDArraySize: DWORD;
                          {out} pdwInputIDs: PDWORD;
                          dwOutputIDArraySize: DWORD;
                          pdwOutputIDs: PDWORD): HResult; stdcall;

    function GetInputStreamInfo(const dwInputStreamID: DWORD;
                                out pStreamInfo: MFT_INPUT_STREAM_INFO): HRESULT; stdcall;

    function GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                 out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HRESULT; stdcall;

    function GetAttributes(out pAttributes: IMFAttributes): HRESULT; stdcall;

    function GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                      out pAttributes: IMFAttributes): HRESULT; stdcall;

    function GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                       out pAttributes: IMFAttributes): HRESULT; stdcall;

    function DeleteInputStream(dwStreamID: DWORD): HRESULT; stdcall;

    function AddInputStreams(cStreams: DWORD;
                             adwStreamIDs: PDWORD): HRESULT; stdcall;

    function GetInputAvailableType(const dwInputStreamID: DWORD;
                                   dwTypeIndex: DWORD;
                                   out ppType: IMFMediaType): HRESULT; stdcall;

    function GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                    dwTypeIndex: DWORD;
                                    out ppType: IMFMediaType): HRESULT; stdcall;

    function SetInputType(const dwInputStreamID: DWORD;
                          pType: IMFMediaType;
                          dwFlags: DWORD): HRESULT; stdcall;

    function SetOutputType(dwOutputStreamID: DWORD;
                           pType: IMFMediaType;
                           dwFlags: DWORD): HRESULT; stdcall;

    function GetInputCurrentType(const dwInputStreamID: DWORD;
                                 out ppType: IMFMediaType): HRESULT; stdcall;

    function GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                  out ppType: IMFMediaType): HRESULT; stdcall;

    function GetInputStatus(const dwInputStreamID: DWORD;
                            out pdwFlags: DWORD {MFT_INPUT_STATUS_ACCEPT_DATA}): HRESULT; stdcall;

    function GetOutputStatus(out pdwFlags: DWORD): HRESULT; stdcall;

    function SetOutputBounds(hnsLowerBound: LONGLONG;
                             hnsUpperBound: LONGLONG): HRESULT; stdcall;

    function ProcessEvent(const dwInputStreamID: DWORD;
                          pEvent: IMFMediaEvent): HRESULT; stdcall;

    function ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                            ulParam: ULONG_PTR): HRESULT; stdcall;

    function ProcessInput(const dwInputStreamID: DWORD;
                          const pSample: IMFSample;
                          dwFlags: DWORD = 0): HRESULT; stdcall;

    function ProcessOutput(dwFlags: MFT_PROCESS_OUTPUT_FLAGS;
                           cOutputBufferCount: DWORD;
                           pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HRESULT; stdcall;
 {$endregion}

  private

    FLock: TCriticalSection;

    // Media types
    FInType: IMFMediaType;
    FOutType: IMFMediaType;

    // Cached audio format
    FChannels: WORD;
    FSampleRate: WORD;
    FBitsPerSample: WORD;
    FBlockAlign: WORD;
    FIsFloat: Boolean;

    // One-sample buffering (synchronous MFT)
    FPendingSample: IMFSample;

    // Parameters
    FBassDb: Single;
    FTrebleDb: Single;
    FBassFreq: Single;
    FTrebleFreq: Single;
    FNeedsCoeffUpdate: Boolean;

    FBass: array of TBiquad;
    FTreble: array of TBiquad;

    FRampMode: TMfRampMode;
    FRampTimeMs: UINT32;

    FBassDbTarget: Single;
    FTrebleDbTarget: Single;
    FBassDbCurrent: Single;
    FTrebleDbCurrent: Single;

    function GetAudioTypeInfo(const pType: IMFMediaType;
                              out Channels: UINT32;
                              out SampleRate: UINT32;
                              out BitsPerSample: UINT32;
                              out BlockAlign: UINT32;
                              out IsFloat: Boolean): HRESULT;

    procedure EnsureStateSizeLocked();
    procedure ClearStateLocked();
    procedure UpdateCoeffsLocked();

    procedure ApplyRampingLocked(const Frames: Integer);

    procedure ComputeLowShelf(out BQ: TBiquad;
                              const Fs,
                              Fc,
                              GainDb: Single);

    procedure ComputeHighShelf(out BQ: TBiquad; const Fs, Fc, GainDb: Single);

    function ProcessSampleLocked(const InSample: IMFSample;
                                 out OutSample:  IMFSample): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;

    class function CreateInstance(out Mft: IMFTransform): HRESULT; static;

    // IMfBassTrebleControl
    function SetBassDb(const Db: Single): HRESULT; stdcall;
    function SetTrebleDb(const Db: Single): HRESULT; stdcall;
    function SetBassFreqHz(const Hz: Single): HRESULT; stdcall;
    function SetTrebleFreqHz(const Hz: Single): HRESULT; stdcall;
    function GetBassDb(out Db: Single): HRESULT; stdcall;
    function GetTrebleDb(out Db: Single): HRESULT; stdcall;

    function SetRampMode(const Mode: TMfRampMode): HRESULT; stdcall;
    function SetRampTimeMs(const Ms: UINT32): HRESULT; stdcall;
    function GetRampMode(out Mode: TMfRampMode): HRESULT; stdcall;
    function GetRampTimeMs(out Ms: UINT32): HRESULT; stdcall;
  end;

function CreateBassTrebleMFT(out Mft: IMFTransform): HRESULT;


implementation


const
  INPUT_STREAM_ID  = 0;
  OUTPUT_STREAM_ID = 0;


function ClampS(const v,
                lo,
                hi: Single): Single;
begin

  if (v < lo) then
    Result := lo
  else
    if (v > hi) then
      Result := hi
  else
    Result := v;
end;


function DbToA(const Db: Single): Double;
begin

  // RBJ shelf cookbook: A = 10^(dB/40)
  Result := Power(10.0,
                  Db / 40.0);
end;


{ TMfBassTrebleMFT }

constructor TMfBassTrebleMFT.Create();
begin
  inherited Create;

  FLock := TCriticalSection.Create();

  FInType := nil;
  FOutType := nil;
  FPendingSample := nil;

  FChannels := 0;
  FSampleRate := 0;
  FBitsPerSample := 0;
  FBlockAlign := 0;
  FIsFloat := False;

  FBassDb := 0.0;
  FTrebleDb := 0.0;
  FBassFreq := 100.0;
  FTrebleFreq := 8000.0;

  FRampMode := rmSmooth;
  FRampTimeMs := 30;

  FBassDbTarget := 0.0;
  FTrebleDbTarget := 0.0;
  FBassDbCurrent := 0.0;
  FTrebleDbCurrent := 0.0;

  FNeedsCoeffUpdate := True;
end;


destructor TMfBassTrebleMFT.Destroy;
begin

  FreeAndNil(FLock);

  inherited;
end;


class function TMfBassTrebleMFT.CreateInstance(out Mft: IMFTransform): HRESULT;
begin

  try

    Mft := TMfBassTrebleMFT.Create as IMFTransform;
    Result := S_OK;
  except

    Mft := nil;
    Result := E_OUTOFMEMORY;
  end;
end;


function CreateBassTrebleMFT(out Mft: IMFTransform): HRESULT;
begin

  Result := TMfBassTrebleMFT.CreateInstance(Mft);
end;


function TMfBassTrebleMFT.SetBassDb(const Db: Single): HRESULT;
begin

  FLock.Enter();

  try

    FBassDbTarget := ClampS(Db,
                            -24.0,
                            24.0);

    if (FRampMode = rmOff) then
      begin

        FBassDbCurrent := FBassDbTarget;
        FNeedsCoeffUpdate := True;
      end;
  finally

    FLock.Leave();
  end;

  Result := S_OK;

end;


function TMfBassTrebleMFT.SetTrebleDb(const Db: Single): HRESULT;
begin

  FLock.Enter();

  try

    FTrebleDbTarget := ClampS(Db,
                              -24.0,
                              24.0);

    if (FRampMode = rmOff) then
      begin

        FTrebleDbCurrent := FTrebleDbTarget;
        FNeedsCoeffUpdate := True;
      end;
  finally

    FLock.Leave();
  end;

  Result := S_OK;

end;


function TMfBassTrebleMFT.SetBassFreqHz(const Hz: Single): HRESULT;
begin

  FLock.Enter();

  try

    FBassFreq := ClampS(Hz,
                        20.0,
                        2000.0);
    FNeedsCoeffUpdate := True;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function TMfBassTrebleMFT.SetTrebleFreqHz(const Hz: Single): HRESULT;
begin

  FLock.Enter();

  try

    FTrebleFreq := ClampS(Hz,
                          200.0,
                          20000.0);
    FNeedsCoeffUpdate := True;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function TMfBassTrebleMFT.GetBassDb(out Db: Single): HRESULT;
begin

  FLock.Enter();

  try
    Db := FBassDbTarget;
  finally

    FLock.Leave();
  end;

  Result := S_OK;

end;


function TMfBassTrebleMFT.GetTrebleDb(out Db: Single): HRESULT;
begin

  FLock.Enter();

  try

    Db := FTrebleDbTarget;
  finally

    FLock.Leave();
  end;

  Result := S_OK;

end;


function TMfBassTrebleMFT.SetRampMode(const Mode: TMfRampMode): HRESULT; stdcall;
begin

  FLock.Enter();

  try

    FRampMode := Mode;

    if (FRampMode = rmOff) then
      begin

        FBassDbCurrent := FBassDbTarget;
        FTrebleDbCurrent := FTrebleDbTarget;
        FNeedsCoeffUpdate := True;
      end;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.SetRampTimeMs(const Ms: UINT32): HRESULT; stdcall;
var
  vv: UINT32;

begin

  FLock.Enter();

  try

   vv := Ms;
   if (vv < 0) then
     vv := 0
   else
     if (vv > 2000) then
       vv := 2000;
    FRampTimeMs := vv;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.GetRampMode(out Mode: TMfRampMode): HRESULT; stdcall;
begin

  FLock.Enter();

  try

    Mode := FRampMode;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.GetRampTimeMs(out Ms: UINT32): HRESULT; stdcall;
begin

  FLock.Enter();

  try

    Ms := FRampTimeMs;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


procedure TMfBassTrebleMFT.EnsureStateSizeLocked();
var
  i: Integer;

begin

  if (FChannels = 0) then
    Exit;

  if (Length(FBass) <> Integer(FChannels)) then
    begin

      SetLength(FBass,
                FChannels);

      for i := 0 to High(FBass) do
        FillChar(FBass[i],
                 SizeOf(FBass[i]),
                 0);
    end;

  if (Length(FTreble) <> Integer(FChannels)) then
    begin

      SetLength(FTreble,
                FChannels);
      for i := 0 to High(FTreble) do
        FillChar(FTreble[i],
                 SizeOf(FTreble[i]),
                 0);
    end;
end;


procedure TMfBassTrebleMFT.ClearStateLocked();
var
  i: Integer;

begin

  for i := 0 to High(FBass) do
    begin
      FBass[i].z1 := 0;
      FBass[i].z2 := 0;
    end;

  for i := 0 to High(FTreble) do
    begin
      FTreble[i].z1 := 0;
      FTreble[i].z2 := 0;
    end;
end;


procedure TMfBassTrebleMFT.ComputeLowShelf(out BQ: TBiquad;
                                           const Fs,
                                           Fc,
                                           GainDb: Single);
var
  A,
  w0,
  cosw0,
  sinw0,
  alpha,
  sqrtA: Double;

  b0,
  b1,
  b2,
  a0,
  a1,
  a2: Double;

begin

  FillChar(BQ,
           SizeOf(BQ),
           0);

  if (Fs <= 0) or (Fc <= 0) then
    Exit;

  A := DbToA(GainDb);
  w0 := 2.0 * Pi * (Fc / Fs);
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  // RBJ shelf cookbook, S=1
  sqrtA := Sqrt(A);
  alpha := sinw0 / 2.0 * Sqrt(2.0);

  b0 := A*( (A+1) - (A-1) * cosw0 + 2 * sqrtA*alpha );
  b1 :=  2*A*( (A-1) - (A+1) * cosw0 );
  b2 := A*( (A+1) - (A-1)*cosw0 - 2 * sqrtA*alpha );
  a0 := (A+1) + (A-1)*cosw0 + 2 * sqrtA*alpha;
  a1 := -2*( (A-1) + (A+1) * cosw0 );
  a2 := (A+1) + (A-1) * cosw0 - 2 * sqrtA*alpha;

  if (a0 = 0) then
    Exit;

  BQ.b0 := Single(b0 / a0);
  BQ.b1 := Single(b1 / a0);
  BQ.b2 := Single(b2 / a0);
  BQ.a1 := Single(a1 / a0);
  BQ.a2 := Single(a2 / a0);
end;


procedure TMfBassTrebleMFT.ComputeHighShelf(out BQ: TBiquad;
                                            const Fs,
                                            Fc,
                                            GainDb: Single);
var
  A, w0, cosw0, sinw0, alpha, sqrtA: Double;
  b0, b1, b2, a0, a1, a2: Double;

begin

  FillChar(BQ,
           SizeOf(BQ),
           0);

  if (Fs <= 0) or (Fc <= 0) then
    Exit;

  A := DbToA(GainDb);
  w0 := 2.0 * Pi * (Fc / Fs);
  cosw0 := Cos(w0);
  sinw0 := Sin(w0);

  // RBJ shelf cookbook, S=1
  sqrtA := Sqrt(A);
  alpha := sinw0 / 2.0 * Sqrt(2.0);

  b0 :=    A*( (A+1) + (A-1)*cosw0 + 2*sqrtA*alpha );
  b1 := -2*A*( (A-1) + (A+1)*cosw0 );
  b2 :=    A*( (A+1) + (A-1)*cosw0 - 2*sqrtA*alpha );
  a0 :=        (A+1) - (A-1)*cosw0 + 2*sqrtA*alpha;
  a1 :=    2*( (A-1) - (A+1)*cosw0 );
  a2 :=        (A+1) - (A-1)*cosw0 - 2*sqrtA*alpha;

  if a0 = 0 then Exit;

  BQ.b0 := Single(b0 / a0);
  BQ.b1 := Single(b1 / a0);
  BQ.b2 := Single(b2 / a0);
  BQ.a1 := Single(a1 / a0);
  BQ.a2 := Single(a2 / a0);
end;

procedure TMfBassTrebleMFT.UpdateCoeffsLocked();
var
  ch: Integer;
  bassBq, trebBq: TBiquad;
  fs, bf, tf, bd, td: Single;
begin
  if (FSampleRate = 0) or (FChannels = 0) then Exit;

  fs := FSampleRate;

  bd := ClampS(FBassDbCurrent, -24.0, 24.0);
  td := ClampS(FTrebleDbCurrent, -24.0, 24.0);

  bf := ClampS(FBassFreq, 20.0, fs * 0.45);
  tf := ClampS(FTrebleFreq, 200.0, fs * 0.45);

  ComputeLowShelf(bassBq, fs, bf, bd);
  ComputeHighShelf(trebBq, fs, tf, td);

  EnsureStateSizeLocked;

  for ch := 0 to Integer(FChannels) - 1 do
    begin
      FBass[ch].b0 := bassBq.b0;
      FBass[ch].b1 := bassBq.b1;
      FBass[ch].b2 := bassBq.b2;
      FBass[ch].a1 := bassBq.a1;
      FBass[ch].a2 := bassBq.a2;

      FTreble[ch].b0 := trebBq.b0;
      FTreble[ch].b1 := trebBq.b1;
      FTreble[ch].b2 := trebBq.b2;
      FTreble[ch].a1 := trebBq.a1;
      FTreble[ch].a2 := trebBq.a2;
    end;

  FNeedsCoeffUpdate := False;
end;

procedure TMfBassTrebleMFT.ApplyRampingLocked(const Frames: Integer);
var
  ms: Cardinal;
  rampFrames: Integer;
  k: Single;
begin
  case FRampMode of
    rmOff:    ms := 0;
    rmFast:   ms := 10;
    rmSmooth: ms := 30;
  else
    ms := FRampTimeMs;
  end;

  if (ms = 0) or (FSampleRate = 0) then
    begin
      FBassDbCurrent := FBassDbTarget;
      FTrebleDbCurrent := FTrebleDbTarget;
      Exit;
    end;

  rampFrames := Integer((Int64(ms) * Int64(FSampleRate)) div 1000);
  if rampFrames <= 0 then
    rampFrames := 1;

  if Frames <= 0 then
    k := 1.0
  else
    k := Frames / rampFrames;

  if k > 1.0 then
    k := 1.0
  else
    if k < 0.0 then
      k := 0.0;

  FBassDbCurrent := FBassDbCurrent + (FBassDbTarget - FBassDbCurrent) * k;
  FTrebleDbCurrent := FTrebleDbCurrent + (FTrebleDbTarget - FTrebleDbCurrent) * k;
end;


function TMfBassTrebleMFT.GetAudioTypeInfo(const pType: IMFMediaType;
                                           out Channels: UINT32;
                                           out SampleRate: UINT32;
                                           out BitsPerSample: UINT32;
                                           out BlockAlign: UINT32;
                                           out IsFloat: Boolean): HRESULT;
var
  hr: HResult;
  gdMajorType: TGUID;
  gdSubType: TGUID;

begin

  Channels := 0;
  SampleRate := 0;
  BitsPerSample := 0;
  BlockAlign := 0;
  IsFloat := False;

  if (pType = nil) then
    Exit(E_POINTER);

  hr := pType.GetGUID(MF_MT_MAJOR_TYPE,
                      gdMajorType);
  if FAILED(hr) then
    Exit(hr);

  if not IsEqualGUID(gdMajorType,
                     MFMediaType_Audio) then
    Exit(MF_E_INVALIDMEDIATYPE);

  hr := pType.GetGUID(MF_MT_SUBTYPE,
                      gdSubType);
  if FAILED(hr) then
    Exit(hr);

  if not (IsEqualGUID(gdSubType, MFAudioFormat_PCM) or IsEqualGUID(gdSubType, MFAudioFormat_Float)) then
    Exit(MF_E_INVALIDMEDIATYPE);

  IsFloat := IsEqualGUID(gdSubType,
                         MFAudioFormat_Float);

  hr := pType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        Channels);
  if FAILED(hr) then
    Exit(hr);

  hr := pType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        SampleRate);
  if FAILED(hr) then
    Exit(hr);

  hr := pType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        BitsPerSample);
  if FAILED(hr) then
    Exit(hr);

  if FAILED(pType.GetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                            BlockAlign)) then
    BlockAlign := (Channels * BitsPerSample) div 8;

  Result := hr;
end;


function TMfBassTrebleMFT.ProcessSampleLocked(const InSample: IMFSample;
                                              out OutSample: IMFSample): HRESULT;
var
  inBuf,
  outBuf: IMFMediaBuffer;
  pIn,
  pOut: PByte;
  cbMax,
  cbCur: DWORD;
  cbIn: DWORD;

  frames: Integer;
  bytesPerFrame: Integer;
  ch,
  i: Integer;

  s: Single;
  v: Integer;
  pF: PSingleArray;

  // Helpers -------------------------------------------------------------------

  function BiquadProcess(var BQ: TBiquad;
                         const x: Single): Single;
  var
    y: Single;

  begin
    // DF2T
    y := (BQ.b0 * x) + BQ.z1;
    BQ.z1 := (BQ.b1 * x) - (BQ.a1 * y) + BQ.z2;
    BQ.z2 := (BQ.b2 * x) - (BQ.a2 * y);
    Result := y;
  end;

  function ReadInt24(const p: PByte): Integer;
  begin
    Result := Integer(p^ or (PByte(NativeUInt(p) + 1)^ shl 8) or (PByte(NativeUInt(p) + 2)^ shl 16));
    if (Result and $00800000) <> 0 then
      Result := Result or Integer($FF000000);
  end;

  procedure WriteInt24(const p: PByte;
                       const Value: Integer);
  var
    vv: Integer;

  begin
    vv := Value;
    if vv > 8388607 then vv := 8388607
    else if vv < -8388608 then vv := -8388608;

    p^ := Byte(vv and $FF);
    PByte(NativeUInt(p) + 1)^ := Byte((vv shr 8) and $FF);
    PByte(NativeUInt(p) + 2)^ := Byte((vv shr 16) and $FF);
  end;

  // ---------------------------------------------------------------------------

var
  hr: HResult;
  timeStamp: Int64;  // TimeStamp

begin

  OutSample := nil;

  hr := InSample.ConvertToContiguousBuffer(@inBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := inBuf.Lock(pIn,
                   @cbMax,
                   @cbCur);
  if FAILED(hr) then
    Exit(hr);

  try

    cbIn := cbCur;

    hr := MFCreateSample(OutSample);
    if FAILED(hr) then
      Exit(hr);

    hr := MFCreateMemoryBuffer(cbIn,
                               outBuf);
    if FAILED(hr) then
      Exit(hr);

    hr := outBuf.Lock(pOut,
                      nil,
                      nil);
    if FAILED(hr) then
      Exit(hr);

    try

      Move(pIn^,
           pOut^,
           cbIn);

      bytesPerFrame := Integer(FBlockAlign);
      if (bytesPerFrame <= 0) or (FChannels = 0) then
        Exit(E_FAIL);

      frames := Integer(cbIn) div bytesPerFrame;

      if FIsFloat then
        begin

          pF := PSingleArray(pOut);

          for i := 0 to frames - 1 do
            for ch := 0 to Integer(FChannels) - 1 do
              begin

                s := pF[i * Integer(FChannels) + ch];

                // Use current ramped gains (or whatever your coeffs were computed from)
                if (FBassDbCurrent <> 0) then
                  s := BiquadProcess(FBass[ch], s);

                if (FTrebleDbCurrent <> 0) then
                  s := BiquadProcess(FTreble[ch], s);

                s := ClampS(s,
                            -1.5,
                            1.5);

                pF[i * Integer(FChannels) + ch] := s;
              end;
        end
      else
        begin
          case FBitsPerSample of

            16:
              begin

                for i := 0 to frames - 1 do
                  for ch := 0 to Integer(FChannels) - 1 do
                    begin

                      v := PSmallInt(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch*2))^;
                      s := v / 32768.0;

                      if (FBassDbCurrent <> 0) then
                        s := BiquadProcess(FBass[ch],
                                           s);

                      if (FTrebleDbCurrent <> 0) then
                        s := BiquadProcess(FTreble[ch],
                                           s);

                      s := ClampS(s,
                                  -1.0,
                                  1.0);

                      v := Round(s * 32767.0);
                      PSmallInt(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch * 2))^ := SmallInt(v);
                    end;
              end;

            24:
              begin

                for i := 0 to frames - 1 do
                  for ch := 0 to Integer(FChannels) - 1 do
                    begin

                      v := ReadInt24(PByte(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch * 3)));
                      s := v / 8388608.0;

                      if (FBassDbCurrent <> 0) then
                        s := BiquadProcess(FBass[ch], s);

                      if (FTrebleDbCurrent <> 0) then
                        s := BiquadProcess(FTreble[ch], s);

                      s := ClampS(s,
                                  -1.0,
                                  1.0);

                      v := Round(s * 8388607.0);
                      WriteInt24(PByte(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch * 3)), v);
                    end;
              end;

            32:
              begin

                for i := 0 to frames - 1 do
                  for ch := 0 to Integer(FChannels) - 1 do
                    begin

                      v := PInteger(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch * 4))^;
                      s := v / 2147483648.0;

                      if (FBassDbCurrent <> 0) then
                        s := BiquadProcess(FBass[ch], s);

                      if (FTrebleDbCurrent <> 0) then
                        s := BiquadProcess(FTreble[ch], s);

                      s := ClampS(s, -1.0, 1.0);
                      v := Round(s * 2147483647.0);
                      PInteger(NativeUInt(pOut) + NativeUInt(i*bytesPerFrame + ch * 4))^ := v;
                    end;
              end;

          else
            begin
              // pass-through
            end;
          end;
        end;

      hr := outBuf.SetCurrentLength(cbIn);
      if FAILED(hr) then
        Exit(hr);

      hr := OutSample.AddBuffer(outBuf);
      if FAILED(hr) then
        Exit(hr);

      // Timestamp / duration are OPTIONAL. Do not fail if not present.
      hr := InSample.GetSampleTime(@timeStamp);
      if SUCCEEDED(hr) then
        begin
          hr := OutSample.SetSampleTime(timeStamp);
          if FAILED(hr) then
            Exit(hr);
        end
      else
        if (hr <> MF_E_NO_SAMPLE_TIMESTAMP) then
          Exit(hr);

      hr := InSample.GetSampleDuration(@timeStamp);
      if SUCCEEDED(hr) then
        begin
          hr := OutSample.SetSampleDuration(timeStamp);
          if FAILED(hr) then
            Exit(hr);
        end
      else
        if (hr <> MF_E_NO_SAMPLE_DURATION) then
          Exit(hr);

      hr := S_OK;

    finally

      outBuf.Unlock();
    end;

  finally

    inBuf.Unlock();
  end;

  Result := hr;
end;



{ IMFTransform }

function TMfBassTrebleMFT.GetStreamLimits(out pdwInputMinimum: DWORD;
                                          out pdwInputMaximum: DWORD;
                                          out pdwOutputMinimum: DWORD;
                                          out pdwOutputMaximum: DWORD): HRESULT;
begin

  pdwInputMinimum := 1;
  pdwInputMaximum := 1;
  pdwOutputMinimum := 1;
  pdwOutputMaximum := 1;
  Result := S_OK;
end;


function TMfBassTrebleMFT.GetStreamCount(out pcInputStreams: DWORD;
                                         out pcOutputStreams: DWORD): HRESULT;
begin

  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfBassTrebleMFT.GetStreamIDs(dwInputIDArraySize: DWORD;
                                       {out} pdwInputIDs: PDWORD;
                                       dwOutputIDArraySize: DWORD;
                                       pdwOutputIDs: PDWORD): HResult;
begin

  // Fixed stream IDs = 0, caller can assume 0; return E_NOTIMPL per MF docs.
  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                             out pStreamInfo: MFT_INPUT_STREAM_INFO): HRESULT;
begin

  if (dwInputStreamID <> INPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
    SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or
                         MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE;
  Result := S_OK;
end;


function TMfBassTrebleMFT.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                              out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HRESULT;
begin

  if (dwOutputStreamID <> OUTPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));
  pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or
                         MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_OUTPUT_STREAM_FIXED_SAMPLE_SIZE;
  Result := S_OK;
end;


function TMfBassTrebleMFT.GetAttributes(out pAttributes: IMFAttributes): HRESULT;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                   out pAttributes: IMFAttributes): HRESULT;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                    out pAttributes: IMFAttributes): HRESULT;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.DeleteInputStream(dwStreamID: DWORD): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.AddInputStreams(cStreams: DWORD;
                                          adwStreamIDs: PDWORD): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.GetInputAvailableType(const dwInputStreamID: DWORD;
                                                dwTypeIndex: DWORD; // 0-based
                                                out ppType: IMFMediaType): HRESULT;
begin

  // We do not enumerate; caller proposes via SetInputType.
  ppType := nil;
  Result := MF_E_NO_MORE_TYPES;
end;


function TMfBassTrebleMFT.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                 dwTypeIndex: DWORD; // 0-based
                                                 out ppType: IMFMediaType): HRESULT;
begin

  ppType := nil;
  Result := MF_E_NO_MORE_TYPES;
end;


function TMfBassTrebleMFT.SetInputType(const dwInputStreamID: DWORD;
                                       pType: IMFMediaType;
                                       dwFlags: DWORD): HRESULT;
var
  ch,
  sr,
  bps,
  ba: UINT32;
  isf: Boolean;

begin

  if (dwInputStreamID <> INPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    begin

      if (pType = nil) then
        Exit(S_OK);

      Exit(GetAudioTypeInfo(pType,
                            ch,
                            sr,
                            bps,
                            ba,
                            isf));
    end;

  FLock.Enter();

  try

    FPendingSample := nil;

    if (pType = nil) then
      begin

        FInType := nil;
        FOutType := nil;
        FChannels := 0;
        FSampleRate := 0;
        FBitsPerSample := 0;
        FBlockAlign := 0;
        FIsFloat := False;
        SetLength(FBass, 0);
        SetLength(FTreble, 0);
        Exit(S_OK);
      end;

    Result := GetAudioTypeInfo(pType,
                               ch,
                               sr,
                               bps,
                               ba,
                               isf);
    if FAILED(Result) then
      Exit;

    FInType := pType;
    FOutType := pType; // output == input

    FChannels := ch;
    FSampleRate := sr;
    FBitsPerSample := bps;
    FBlockAlign := ba;
    FIsFloat := isf;

    EnsureStateSizeLocked();
    ClearStateLocked();
    FNeedsCoeffUpdate := True;

    Result := S_OK;
  finally

    FLock.Leave();
  end;
end;


function TMfBassTrebleMFT.SetOutputType(dwOutputStreamID: DWORD;
                                        pType: IMFMediaType;
                                        dwFlags: DWORD): HRESULT;
var
  ch,
  sr,
  bps,
  ba: UINT32;
  isf: Boolean;

begin

  if (dwOutputStreamID <> OUTPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    begin

      if (pType = nil) then
        Exit(S_OK);

      Result := GetAudioTypeInfo(pType,
                                 ch,
                                 sr,
                                 bps,
                                 ba,
                                 isf);
      if FAILED(Result) then
        Exit;

      FLock.Enter();

      try

        if (FInType = nil) then
          Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

        // Require exact match with current input format
        if (ch <> FChannels) or
           (sr <> FSampleRate) or
           (bps <> FBitsPerSample) or
           (isf <> FIsFloat) then
          Exit(MF_E_INVALIDMEDIATYPE);
      finally

        FLock.Leave();
      end;

      Exit(S_OK);
    end;

  FLock.Enter();

  try

    if (FInType = nil) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    FOutType := FInType; // enforce
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.GetInputCurrentType(const dwInputStreamID: DWORD;
                                              out ppType: IMFMediaType): HRESULT;
begin

  if (dwInputStreamID <> INPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  FLock.Enter();

  try

    ppType := FInType;
  finally

    FLock.Leave();
  end;

  if (ppType = nil) then
    Result := MF_E_TRANSFORM_TYPE_NOT_SET
  else
    Result := S_OK;
end;


function TMfBassTrebleMFT.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                               out ppType: IMFMediaType): HRESULT;
begin

  if (dwOutputStreamID <> OUTPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  FLock.Enter();

  try

    ppType := FOutType;
  finally

    FLock.Leave();
  end;

  if (ppType = nil) then
    Result := MF_E_TRANSFORM_TYPE_NOT_SET
  else
    Result := S_OK;
end;


function TMfBassTrebleMFT.GetInputStatus(const dwInputStreamID: DWORD;
                                         out pdwFlags: DWORD {MFT_INPUT_STATUS_ACCEPT_DATA}): HRESULT;
begin

  if (dwInputStreamID <> INPUT_STREAM_ID) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  FLock.Enter();

  try

    if (FInType = nil) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    if (FPendingSample = nil) then
      pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA
    else
      pdwFlags := 0;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.GetOutputStatus(out pdwFlags: DWORD): HRESULT;
begin

  FLock.Enter();

  try

    if (FOutType = nil) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    if (FPendingSample <> nil) then
      pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY
    else
      pdwFlags := 0;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.SetOutputBounds(hnsLowerBound: LONGLONG;
                                          hnsUpperBound: LONGLONG): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.ProcessEvent(const dwInputStreamID: DWORD;
                                       pEvent: IMFMediaEvent): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfBassTrebleMFT.ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                                         ulParam: ULONG_PTR): HRESULT;
begin

  case eMessage of

    MFT_MESSAGE_COMMAND_FLUSH:
      begin

        FLock.Enter();

        try

          FPendingSample := nil;
          ClearStateLocked;
          FBassDbCurrent := FBassDbTarget;
          FTrebleDbCurrent := FTrebleDbTarget;
          FNeedsCoeffUpdate := True;
        finally

          FLock.Leave();
        end;
        Exit(S_OK);
      end;

    MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
    MFT_MESSAGE_NOTIFY_START_OF_STREAM:
      begin

        FLock.Enter();
        try

          ClearStateLocked();
          FBassDbCurrent := FBassDbTarget;
          FTrebleDbCurrent := FTrebleDbTarget;
          FNeedsCoeffUpdate := True;
        finally

          FLock.Leave();
        end;
        Exit(S_OK);
      end;
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.ProcessInput(const dwInputStreamID: DWORD;
                                       const pSample: IMFSample;
                                       dwFlags: DWORD = 0): HRESULT;
begin

  if dwInputStreamID <> INPUT_STREAM_ID then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (pSample = nil) then
    Exit(E_POINTER);

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  FLock.Enter();

  try

    if (FInType = nil) or (FOutType = nil) then
      Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

    if (FPendingSample <> nil) then
      Exit(MF_E_NOTACCEPTING);

    FPendingSample := pSample;
  finally

    FLock.Leave();
  end;

  Result := S_OK;
end;


function TMfBassTrebleMFT.ProcessOutput(dwFlags: MFT_PROCESS_OUTPUT_FLAGS; // MFT_PROCESS_OUTPUT_FLAGS
                                        cOutputBufferCount: DWORD; // # returned by GetStreamCount()
                                        pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;  // one per stream
                                        out pdwStatus: DWORD): HRESULT; stdcall;
var
  hr: HResult;
  outSample: IMFSample;
  inBuf: IMFMediaBuffer;
  cbCur: DWORD;
  frames: Integer;

begin

  pdwStatus := 0;

  if (pOutputSamples = nil) then
    Exit(E_POINTER);

  if (cOutputBufferCount < 1) then
    Exit(E_INVALIDARG);

  FLock.Enter();

  try

    if (FOutType = nil) then
      begin
        Result := MF_E_TRANSFORM_TYPE_NOT_SET;
        Exit;
      end;

    if (FPendingSample = nil) then
      begin
        Result := MF_E_TRANSFORM_NEED_MORE_INPUT;
        Exit;
      end;

    if (FBlockAlign <> 0) and (FSampleRate <> 0) then
      begin

        // Determine how many frames this sample contains (no lock required on buffer)
        hr := FPendingSample.ConvertToContiguousBuffer(@inBuf);

        if SUCCEEDED(hr) then
          hr := inBuf.GetCurrentLength(cbCur);

        if SUCCEEDED(hr) and (cbCur > 0) then
          frames := Integer(cbCur div FBlockAlign)
        else
          frames := 0;

        ApplyRampingLocked(frames);
        FNeedsCoeffUpdate := True;
      end
    else
      begin
        // No valid format yet -> no ramp. Keep current=target.
        FBassDbCurrent := FBassDbTarget;
        FTrebleDbCurrent := FTrebleDbTarget;
        FNeedsCoeffUpdate := True;
      end;

    if FNeedsCoeffUpdate then
      UpdateCoeffsLocked;

    hr := ProcessSampleLocked(FPendingSample,
                              outSample);
    if FAILED(hr) then
      begin
        Result := hr;
        Exit;
      end;

    FPendingSample := nil;

  finally

    FLock.Leave();
  end;

  // IMPORTANT: clear the OUTPUT STRUCT.
  ZeroMemory(pOutputSamples,
             SizeOf(pOutputSamples^));

  pOutputSamples^.pSample := outSample;
  pOutputSamples^.dwStatus := 0;
  pOutputSamples^.pEvents := nil;

  Result := S_OK;
end;


end.

