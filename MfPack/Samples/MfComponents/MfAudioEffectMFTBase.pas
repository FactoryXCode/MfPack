// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEffectMFTBase.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Common PCM/Float plumbing + true-peak guard hook.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
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
unit MfAudioEffectMFTBase;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfError,
  {Application}
  PcmLib;

type
  TMfRampMode = (rmOff,
                 rmFast,
                 rmSmooth,
                 rmManual);

  // Shared biquad coeff view (a0 normalized to 1)
  TBiquadCoeffs = record
    b0,
    b1,
    b2: Double;
    a1,
    a2: Double;
  end;

  // Base class: 1-in/1-out audio MFT, internal float processing.
  // Output sample MUST be provided by the caller (matches your ProcessEQBuffer pattern).
  TMfAudioEffectMFTBase = class(TInterfacedPersistent, IMFTransform)

{$region 'IMFTransform implementation'}
// IMFTransform implementation /////////////////////////////////////////////////
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
                                   out pType: IMFMediaType): HRESULT; stdcall;

    function GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                    dwTypeIndex: DWORD;
                                    out pType: IMFMediaType): HRESULT; stdcall;

    function SetInputType(const dwInputStreamID: DWORD;
                          pType: IMFMediaType;
                          dwFlags: DWORD): HRESULT; stdcall;

    function SetOutputType(dwOutputStreamID: DWORD;
                           pType: IMFMediaType;
                           dwFlags: DWORD): HRESULT; stdcall;

    function GetInputCurrentType(const dwInputStreamID: DWORD;
                                 out pType: IMFMediaType): HRESULT; stdcall;

    function GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                  out pType: IMFMediaType): HRESULT; stdcall;

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

    FCS: TCriticalSection;

    FInType: IMFMediaType;
    FOutType: IMFMediaType;

    FChannels: Integer;
    FSampleRate: Integer;
    FBitsPerSample: Integer;   // for PCM
    FBytesPerSample: Integer;  // 2/3/4 for PCM, 4 for float
    FIsFloat: Boolean;

    FDynFloatBuf: PSingle;
    FDynFloatBufSamples: Integer;

    FHaveInput: Boolean;
    FInputSample: IMFSample;

    // True-peak guard
    FTPEnabled: Boolean;
    FTPCeilingLin: Single;     // linear ceiling (0..1)
    FTPOversample: Integer;    // 2/4/8 (we default to 4)
    FTPGain: Single;           // running gain (smoothed)
    FTPReleaseCoef: Single;    // per-block
    FTPAttackCoef: Single;     // per-block (fast)

    procedure ClearInput();
    function IsTypeSupported(const pType: IMFMediaType): Boolean;
    function ParseAudioType(const pType: IMFMediaType): HRESULT;

    procedure EnsureFloatScratch(NeededSamples: Integer);

    procedure ConvertBytesToFloat(const InBytes: PByte;
                                  OutF: PSingle;
                                  Samples: Integer);

    procedure ConvertFloatToBytes(const InF: PSingle;
                                  OutBytes: PByte;
                                  Samples: Integer);

    // True-peak estimate + gain smoothing
    function EstimateTruePeak4x(const x: PSingle;
                                Frames,
                                Channels: Integer): Single;

    procedure ApplyTruePeakGuard(const x: PSingle;
                                 Frames,
                                 Channels: Integer);

  protected

    // Override point: process interleaved float32 samples in-place.
    procedure ProcessAudioFloat32(pData: PSingle;
                                  Frames,
                                  Channels,
                                  SampleRate: Integer); virtual;

    // Optional post stage (default calls true-peak guard only)
    procedure PostProcessFloat32(pData: PSingle;
                                 Frames,
                                 Channels,
                                 SampleRate: Integer); virtual;
  public

    constructor Create();
    destructor Destroy(); override;

    // True-peak guard control (callable by derived/control interfaces)
    procedure SetTruePeakEnabled(const AEnabled: Boolean);
    procedure SetTruePeakCeilingDbTP(const CeilingDbTP: Single);  // e.g. -1.0
    procedure SetTruePeakOversample(const Factor: Integer);       // 2/4/8
    procedure SetTruePeakReleaseMs(const Ms: Integer);            // default 50
    procedure SetTruePeakAttackMs(const Ms: Integer);             // default 1
  end;


implementation


constructor TMfAudioEffectMFTBase.Create();
begin

  inherited Create();

  FCS := TCriticalSection.Create();

  FDynFloatBuf := nil;
  FDynFloatBufSamples := 0;

  ClearInput();

  // True-peak defaults (per your requirement)
  FTPEnabled := False;
  SetTruePeakCeilingDbTP(-1.0);
  SetTruePeakOversample(4);
  SetTruePeakAttackMs(1);
  SetTruePeakReleaseMs(50);
  FTPGain := 1.0;
end;


destructor TMfAudioEffectMFTBase.Destroy();
begin

  ClearInput();

  if (FDynFloatBuf <> nil) then
    begin

      FreeMem(FDynFloatBuf);
      FDynFloatBuf := nil;
      FDynFloatBufSamples := 0;
    end;

  FInType := nil;
  FOutType := nil;

  FCS.Free();

  inherited;
end;


procedure TMfAudioEffectMFTBase.ClearInput();
begin

  FInputSample := nil;
  FHaveInput := False;
end;


function TMfAudioEffectMFTBase.IsTypeSupported(const pType: IMFMediaType): Boolean;
var
  Major, Sub: TGUID;
  Bits: UINT32;

begin

  Result := False;

  if (pType = nil) then
    Exit;

  if FAILED(pType.GetGUID(MF_MT_MAJOR_TYPE,
                          Major)) then
    Exit;

  if not IsEqualGUID(Major,
         MFMediaType_Audio) then
    Exit;

  if FAILED(pType.GetGUID(MF_MT_SUBTYPE,
                          Sub)) then
    Exit;

  if IsEqualGUID(Sub,
                 MFAudioFormat_Float) then
    Exit(True);

  if IsEqualGUID(Sub,
                 MFAudioFormat_PCM) then
  begin

    if FAILED(pType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                              Bits)) then
      Exit;

    Result := Bits in [16,
                       24,
                       32];
  end;
end;


function TMfAudioEffectMFTBase.ParseAudioType(const pType: IMFMediaType): HRESULT;
var
  Sub: TGUID;
  v: UINT32;

begin

  Result := pType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                            v);
  if FAILED(Result) then
    Exit;

  FChannels := Integer(v);

  Result := pType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            v);
  if FAILED(Result) then
    Exit;

  FSampleRate := Integer(v);

  Result := pType.GetGUID(MF_MT_SUBTYPE,
                          Sub);
  if FAILED(Result) then
    Exit;

  FIsFloat := IsEqualGUID(Sub,
                          MFAudioFormat_Float);
  if FIsFloat then
    begin

      FBitsPerSample := 32;
      FBytesPerSample := 4;
    end
  else
    begin

      Result := pType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                v);
      if FAILED(Result) then
        Exit;

      FBitsPerSample := Integer(v);
      FBytesPerSample := FBitsPerSample div 8;
    end;

  Result := S_OK;
end;


procedure TMfAudioEffectMFTBase.EnsureFloatScratch(NeededSamples: Integer);
begin

  EnsureDynFloatBuf(FDynFloatBuf,
                    FDynFloatBufSamples,
                    NeededSamples);
end;


procedure TMfAudioEffectMFTBase.ConvertBytesToFloat(const InBytes: PByte;
                                                    OutF: PSingle;
                                                     Samples: Integer);
begin

  if FIsFloat then
    Move(InBytes^,
         OutF^,
         NativeUInt(Samples) * SizeOf(Single))
  else
    begin

      case FBitsPerSample of
        16: Int16ToFloat(InBytes,
                         OutF,
                         Samples);

        24: Int24ToFloat(InBytes,
                         OutF,
                         Samples);

        32: Int32ToFloat(InBytes,
                         OutF,
                         Samples);
      else

        // Should never happen due to type validation.
        FillChar(OutF^,
                 NativeUInt(Samples) * SizeOf(Single),
                 0);
      end;
    end;
end;


procedure TMfAudioEffectMFTBase.ConvertFloatToBytes(const InF: PSingle; OutBytes: PByte; Samples: Integer);
begin

  if FIsFloat then
    Move(InF^,
         OutBytes^,
         NativeUInt(Samples) * SizeOf(Single))
  else
    begin

      case FBitsPerSample of
        16: FloatToInt16(InF,
                         OutBytes,
                         Samples);

        24: FloatToInt24(InF,
                         OutBytes,
                         Samples);

        32: FloatToInt32(InF,
                         OutBytes,
                         Samples);
      end;
    end;
end;


procedure TMfAudioEffectMFTBase.ProcessAudioFloat32(pData: PSingle;
                                                    Frames,
                                                    Channels,
                                                    SampleRate: Integer);
begin

  // base no-op.
end;


procedure TMfAudioEffectMFTBase.PostProcessFloat32(pData: PSingle;
                                                   Frames,
                                                   Channels,
                                                   SampleRate: Integer);
begin

  if FTPEnabled then
    ApplyTruePeakGuard(pData,
                       Frames,
                       Channels);
end;


procedure TMfAudioEffectMFTBase.SetTruePeakEnabled(const AEnabled: Boolean);
begin

  FTPEnabled := AEnabled;
  if not FTPEnabled then
    FTPGain := 1.0;
end;


procedure TMfAudioEffectMFTBase.SetTruePeakCeilingDbTP(const CeilingDbTP: Single);
begin

  // dBTP ceiling in linear (0..1)
  FTPCeilingLin := Power(10.0,
                         CeilingDbTP / 20.0);
  if (FTPCeilingLin < 0.000001) then
    FTPCeilingLin := 0.000001;
end;


procedure TMfAudioEffectMFTBase.SetTruePeakOversample(const Factor: Integer);
begin

  if (Factor = 2) or
     (Factor = 4) or
     (Factor = 8) then
    FTPOversample := Factor
  else
    FTPOversample := 4;
end;


procedure TMfAudioEffectMFTBase.SetTruePeakReleaseMs(const Ms: Integer);
var
  tau: Double;

begin

  if (FSampleRate <= 0) then
    Exit;

  if (Ms <= 0) then
    FTPReleaseCoef := 0
  else
    begin

      tau := Ms / 1000.0;
      // per-sample coefficient; well convert to per-block inside ApplyTruePeakGuard.
      FTPReleaseCoef := (Exp(-1.0 / (tau * FSampleRate))) * 1.0;
    end;
end;


procedure TMfAudioEffectMFTBase.SetTruePeakAttackMs(const Ms: Integer);
var
  tau: Double;

begin

  if (FSampleRate <= 0) then
    Exit;
  if (Ms <= 0) then
    FTPAttackCoef := 0
  else
    begin

      tau := Ms / 1000.0;
      FTPAttackCoef := (Exp(-1.0 / (tau * FSampleRate))) * 1.0;
    end;
end;


function TMfAudioEffectMFTBase.EstimateTruePeak4x(const x: PSingle;
                                                  Frames,
                                                  Channels: Integer): Single;
var
  ch,
  n: Integer;
  y0,
  y1,
  y2,
  y3: Single;
  s,
  a: Single;
  idx0,
  idx1,
  idx2,
  idx3: Integer;

  function ReadSample(const Idx: Integer): Single;
  begin
    Result := PSingle(PByte(x) + (Idx * SizeOf(Single)))^;
  end;

begin
  Result := 0;

  if (x = nil) or (Frames <= 0) or (Channels <= 0) then
    Exit;

  for ch := 0 to Channels - 1 do
    begin
      for n := 0 to Frames - 2 do
        begin

          idx1 := (n * Channels) + ch;
          idx2 := ((n + 1) * Channels) + ch;
          idx0 := ((n - 1) * Channels) + ch;
          idx3 := ((n + 2) * Channels) + ch;

          if (n = 0) then
            idx0 := idx1;
          if (n >= Frames - 2) then
            idx3 := idx2;

          y0 := MfSanitizeSampleS(ReadSample(idx0));
          y1 := MfSanitizeSampleS(ReadSample(idx1));
          y2 := MfSanitizeSampleS(ReadSample(idx2));
          y3 := MfSanitizeSampleS(ReadSample(idx3));

          a := Abs(y1);
          if (a > Result) then
            Result := a;

          a := Abs(y2);
          if (a > Result) then
            Result := a;

          s := MfCatmullRomS(y0,
                             y1,
                             y2,
                             y3,
                             0.25);
          a := Abs(s);
          if (a > Result) then
            Result := a;

          s := MfCatmullRomS(y0,
                             y1,
                             y2,
                             y3,
                             0.50);
          a := Abs(s);
          if (a > Result) then
            Result := a;

          s := MfCatmullRomS(y0,
                             y1,
                             y2,
                             y3,
                             0.75);
          a := Abs(s);
          if (a > Result) then
            Result := a;
        end;
  end;
end;


procedure TMfAudioEffectMFTBase.ApplyTruePeakGuard(const x: PSingle;
                                                   Frames,
                                                   Channels: Integer);
var
  peak,
  targetGain: Single;
  // convert per-sample coeff to per-block coeff
  aAtk,
  aRel: Single;
  
  p: PSingle;
  i,
  total: Integer;

begin

  if (Frames <= 0) or (Channels <= 0) then
    Exit;

  // For now we implement 4x as requested; if you later select 2x/8x we can extend
  if (FTPOversample <> 4) then
    peak := EstimateTruePeak4x(x,
                               Frames,
                               Channels)
  else
    peak := EstimateTruePeak4x(x,
                               Frames,
                               Channels);

  if (peak <= 1e-12) then
    Exit;

  if (peak > FTPCeilingLin) then
    targetGain := FTPCeilingLin / peak
  else
    targetGain := 1.0;

  // per-block smoothing
  if (FTPAttackCoef <= 0) then
    aAtk := 0
  else
    aAtk := Power(FTPAttackCoef,
                  Frames);

  if (FTPReleaseCoef <= 0) then
    aRel := 0
  else
    aRel := Power(FTPReleaseCoef,
                  Frames);

  // If we need to reduce, attack fast; else release slow
  if (targetGain < FTPGain) then
    FTPGain := aAtk * FTPGain + (1 - aAtk) * targetGain
  else
    FTPGain := aRel * FTPGain + (1 - aRel) * targetGain;

  if (FTPGain >= 0.999999) then
    Exit;

  // Apply guard gain to all samples (common gain)
  // denormal-safe scale
  // (keep it simple + fast)

  total := Frames * Channels;
  p := x;
  for i := 0 to total - 1 do
    begin
      p^ := p^ * FTPGain;
      Inc(p);
    end;
end;


function TMfAudioEffectMFTBase.GetStreamLimits(out pdwInputMinimum: DWORD;
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


function TMfAudioEffectMFTBase.GetStreamCount(out pcInputStreams,
                                              pcOutputStreams: DWORD): HResult;
begin

  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetStreamIDs(dwInputIDArraySize: DWORD;
                                            pdwInputIDs: PDWORD;
                                            dwOutputIDArraySize: DWORD;
                                            pdwOutputIDs: PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                                  out pStreamInfo: MFT_INPUT_STREAM_INFO): HRESULT;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                                   out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HRESULT;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetAttributes(out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                        out pAttributes: IMFAttributes): HRESULT;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                         out pAttributes: IMFAttributes): HRESULT;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.DeleteInputStream(dwStreamID: DWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.AddInputStreams(cStreams: DWORD; adwStreamIDs: PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.GetInputAvailableType(const dwInputStreamID: DWORD;
                                                     dwTypeIndex: DWORD;
                                                     out pType: IMFMediaType): HRESULT;
begin

  pType := nil;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwTypeIndex <> 0) then
    Exit(MF_E_NO_MORE_TYPES);

  Result := MFCreateMediaType(pType);
  if FAILED(Result) then
    Exit;

  pType.SetGUID(MF_MT_MAJOR_TYPE,
                MFMediaType_Audio);

  pType.SetGUID(MF_MT_SUBTYPE,
                MFAudioFormat_Float);
end;


function TMfAudioEffectMFTBase.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                      dwTypeIndex: DWORD;
                                                      out pType: IMFMediaType): HRESULT;
begin

  Result := GetInputAvailableType(dwOutputStreamID,
                                  dwTypeIndex,
                                  pType);
end;


function TMfAudioEffectMFTBase.SetInputType(const dwInputStreamID: DWORD;
                                            pType: IMFMediaType;
                                            dwFlags: DWORD): HRESULT;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0 then
    begin

      if (pType <> nil) and (not IsTypeSupported(pType)) then
        Exit(MF_E_INVALIDMEDIATYPE);
      Exit(S_OK);
    end;

  if (pType <> nil) and (not IsTypeSupported(pType)) then
    Exit(MF_E_INVALIDMEDIATYPE);

  FInType := pType;
  Result := ParseAudioType(pType);

  // reset guard smoothing for new format
  FTPGain := 1.0;
end;


function TMfAudioEffectMFTBase.SetOutputType(dwOutputStreamID: DWORD;
                                             pType: IMFMediaType;
                                             dwFlags: DWORD): HResult;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    begin

      if (pType <> nil) and (not IsTypeSupported(pType)) then
        Exit(MF_E_INVALIDMEDIATYPE);
      Exit(S_OK);
    end;

  if (pType <> nil) and (not IsTypeSupported(pType)) then
    Exit(MF_E_INVALIDMEDIATYPE);

  FOutType := pType;
  Result := ParseAudioType(pType);

  FTPGain := 1.0;
end;


function TMfAudioEffectMFTBase.GetInputCurrentType(const dwInputStreamID: DWORD;
                                                   out pType: IMFMediaType): HRESULT;
begin

  pType := FInType;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                                    out pType: IMFMediaType): HRESULT;
begin

  pType := FOutType;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetInputStatus(const dwInputStreamID: DWORD;
                                              out pdwFlags: DWORD {MFT_INPUT_STATUS_ACCEPT_DATA}): HRESULT;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);
  if not FHaveInput then
    pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA
  else
    pdwFlags := 0;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.GetOutputStatus(out pdwFlags: DWORD): HResult;
begin

  if FHaveInput then
    pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY
  else
    pdwFlags := 0;
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.SetOutputBounds(hnsLowerBound, hnsUpperBound: Int64): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.ProcessEvent(const dwInputStreamID: DWORD;
                                            pEvent: IMFMediaEvent): HRESULT;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEffectMFTBase.ProcessMessage(eMessage: MFT_MESSAGE_TYPE; ulParam: ULONG_PTR): HResult;
begin

  if (eMessage = MFT_MESSAGE_COMMAND_FLUSH) then
    ClearInput();
  Result := S_OK;
end;


function TMfAudioEffectMFTBase.ProcessInput(const dwInputStreamID: DWORD;
                                            const pSample: IMFSample;
                                            dwFlags: DWORD = 0): HRESULT;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (pSample = nil) then
    Exit(E_POINTER);

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if (FInType = nil) or (FOutType = nil) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if FHaveInput then
    Exit(MF_E_NOTACCEPTING);

  FInputSample := pSample;
  FHaveInput := True;
  Result := S_OK;
end;

function TMfAudioEffectMFTBase.ProcessOutput(dwFlags: DWORD;
                                             cOutputBufferCount: DWORD;
                                             pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                                             out pdwStatus: DWORD): HResult;
var
  InBuf,
  OutBuf: IMFMediaBuffer;
  pIn,
  pOut: PByte;
  cbIn: DWORD;
  frames,
  samples: Integer;
  hnsTime,
  hnsDur: Int64;

begin

  pdwStatus := 0;

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if (cOutputBufferCount <> 1) or (pOutputSamples = nil) then
    Exit(E_INVALIDARG);

  // Caller must provide output sample (matches your engine code)
  if (pOutputSamples^.pSample = nil) then
    Exit(E_INVALIDARG);

  if not FHaveInput then
    Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

  Result := FInputSample.ConvertToContiguousBuffer(@InBuf);
  if FAILED(Result) then
    Exit;

  Result := InBuf.GetCurrentLength(cbIn);
  if FAILED(Result) then
    Exit;

  frames := Integer(cbIn) div (FChannels * FBytesPerSample);
  if (frames <= 0) then
    begin

      ClearInput();
      Exit(S_OK);
    end;

  samples := frames * FChannels;
  EnsureFloatScratch(samples);

  // Bytes -> float
  Result := InBuf.Lock(pIn,
                       nil,
                       nil);
  if FAILED(Result) then
    Exit;

  try

    ConvertBytesToFloat(pIn,
                        FDynFloatBuf,
                        samples);
  finally

    InBuf.Unlock();
  end;

  // DSP (derived) + post stage (true-peak guard)
  ProcessAudioFloat32(FDynFloatBuf,
                      frames,
                      FChannels,
                      FSampleRate);

  PostProcessFloat32(FDynFloatBuf,
                     frames,
                     FChannels,
                     FSampleRate);

  // float -> bytes
  Result := pOutputSamples^.pSample.ConvertToContiguousBuffer(@OutBuf);
  if FAILED(Result) then
    Exit;

  Result := OutBuf.Lock(pOut,
                        nil,
                        nil);
  if FAILED(Result) then
    Exit;

  try

    ConvertFloatToBytes(FDynFloatBuf,
                        pOut,
                        samples);
    OutBuf.SetCurrentLength(cbIn);
  finally

    OutBuf.Unlock();
  end;

  // propagate timestamps if present
  if Succeeded(FInputSample.GetSampleTime(@hnsTime)) then
    pOutputSamples^.pSample.SetSampleTime(hnsTime);

  if Succeeded(FInputSample.GetSampleDuration(@hnsDur)) then
    pOutputSamples^.pSample.SetSampleDuration(hnsDur);

  ClearInput();
  Result := S_OK;
end;

end.

