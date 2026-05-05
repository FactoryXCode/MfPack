// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEqBaseMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.2.0
//
// Description:
//   Minimal synchronous Audio MFT base class:
//     - 1 input stream, 1 output stream.
//     - Keeps one input sample, produces one output sample.
//     - Derived class processes raw PCM bytes (in-place on output buffer).
//
//   Supported formats (by default):
//     - MFMediaType_Audio + MFAudioFormat_PCM, 16-bit or 32-bit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX320/Samples/WasApiPlayer/Example3
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit MfAudioEqBaseMFT;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.WinApiTypes, // This will correct Delphi PDWord bug.
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils;


type

  TMfAudioEqBaseMFT = class(TInterfacedObject, IMFTransform)
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

  protected
    FLock: TCriticalSection;

    // current negotiated format (available to derived classes).
    FSampleRate: Cardinal;
    FChannels: Cardinal;
    FBitsPerSample: Cardinal;
    FBlockAlign: Cardinal;

    // derived DSP hooks (called under FLock).
    procedure ClearStateLocked; virtual;
    procedure OnFormatChangedLocked; virtual;
    function ProcessAudioLocked(pData: PByte; cbData: Cardinal): HRESULT; virtual; abstract;

    // Helpers.
    class function ClampS(const v,
                          vmin,
                          vmax: Single): Single; static;

    class function ClampI(const v,
                          vmin,
                          vmax: Integer): Integer; static;

  private

    FInputType: IMFMediaType;
    FOutputType: IMFMediaType;

    FHaveInputSample: Boolean;
    FInputSample: IMFSample;

    function IsTypeSupported(const pType: IMFMediaType): Boolean;
    function ReadAudioFormatFromType(const pType: IMFMediaType): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;
  end;


implementation


constructor TMfAudioEqBaseMFT.Create();
begin
  inherited Create();

  FLock := TCriticalSection.Create();
end;


destructor TMfAudioEqBaseMFT.Destroy();
begin

  FInputSample := nil;
  FInputType := nil;
  FOutputType := nil;
  FreeAndNil(FLock);

  inherited;
end;


procedure TMfAudioEqBaseMFT.ClearStateLocked();
begin

  // default: nothing
end;


procedure TMfAudioEqBaseMFT.OnFormatChangedLocked();
begin

  // default: nothing
end;


class function TMfAudioEqBaseMFT.ClampI(const v,
                                        vmin,
                                        vmax: Integer): Integer;
begin

  if (v < vmin) then
    Result := vmin
  else
    if (v > vmax) then
      Result := vmax
    else Result := v;
end;


class function TMfAudioEqBaseMFT.ClampS(const v, vmin, vmax: Single): Single;
begin

  if (v < vmin) then
    Result := vmin
  else
    if (v > vmax) then
      Result := vmax
    else Result := v;
end;


function TMfAudioEqBaseMFT.IsTypeSupported(const pType: IMFMediaType): Boolean;
var
  major,
  sub: TGUID;
  bits: UINT32;

begin

  Result := False;

  if (pType = nil) then
    Exit;

  if Failed(pType.GetGUID(MF_MT_MAJOR_TYPE,
                          major)) then
    Exit;

  if not IsEqualGUID(major,
                     MFMediaType_Audio) then
    Exit;

  if Failed(pType.GetGUID(MF_MT_SUBTYPE,
                          sub)) then
    Exit;

  if not IsEqualGUID(sub,
                     MFAudioFormat_PCM) then
    Exit;

  bits := 0;

  pType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                  bits);

  Result := (bits = 16) or (bits = 32);
end;


function TMfAudioEqBaseMFT.ReadAudioFormatFromType(const pType: IMFMediaType): HRESULT;
var
  sr,
  ch,
  bps,
  ba: UINT32;

begin

  Result := E_INVALIDARG;

  if (pType = nil) then
    Exit;

  sr := 0;
  ch := 0;
  bps := 0;
  ba := 0;

  if Failed(pType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            sr)) then
    Exit(E_FAIL);

  if Failed(pType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                            ch)) then
    Exit(E_FAIL);

  if Failed(pType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                            bps)) then
    Exit(E_FAIL);

  if Failed(pType.GetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                            ba)) then
    ba := (ch * (bps div 8));

  FSampleRate := sr;
  FChannels := ch;
  FBitsPerSample := bps;
  FBlockAlign := ba;

  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetStreamLimits(out pdwInputMinimum: DWORD;
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


function TMfAudioEqBaseMFT.GetStreamCount(out pcInputStreams,
                                          pcOutputStreams: DWORD): HResult;
begin

  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetStreamIDs(dwInputIDArraySize: DWORD;
                                        pdwInputIDs: PDWORD;
                                        dwOutputIDArraySize: DWORD;
                                        pdwOutputIDs: PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                              out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                               out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetAttributes(out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                    out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                     out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.DeleteInputStream(dwStreamID: DWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.AddInputStreams(cStreams: DWORD;
                                           adwStreamIDs: PDWORD): HResult;
begin
Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.GetInputAvailableType(const dwInputStreamID: DWORD;
                                                 dwTypeIndex: DWORD;
                                                 out pType: IMFMediaType): HResult;
begin

  pType := nil;
  Result := MF_E_NO_MORE_TYPES;
end;


function TMfAudioEqBaseMFT.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                  dwTypeIndex: DWORD;
                                                  out pType: IMFMediaType): HResult;
begin

  pType := nil;
  Result := MF_E_NO_MORE_TYPES;
end;


function TMfAudioEqBaseMFT.SetInputType(const dwInputStreamID: DWORD;
                                        pType: IMFMediaType;
                                        dwFlags: DWORD): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    begin

      if IsTypeSupported(pType) then
        Exit(S_OK)
      else Exit(MF_E_INVALIDMEDIATYPE);
    end;

  if not IsTypeSupported(pType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  FLock.Enter();

  try

    FInputType := pType;
    Result := ReadAudioFormatFromType(pType);

    if Succeeded(Result) then
      begin
        ClearStateLocked();
        OnFormatChangedLocked();
      end;
  finally

    FLock.Leave();
  end;
end;


function TMfAudioEqBaseMFT.SetOutputType(dwOutputStreamID: DWORD;
                                         pType: IMFMediaType;
                                         dwFlags: DWORD): HResult;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0 then
    begin

      if IsTypeSupported(pType) then
        Exit(S_OK)
      else Exit(MF_E_INVALIDMEDIATYPE);
    end;

  if not IsTypeSupported(pType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  FLock.Enter();

  try

    FOutputType := pType;
    Result := ReadAudioFormatFromType(pType);

    if Succeeded(Result) then
      begin

        ClearStateLocked();
        OnFormatChangedLocked();
      end;
  finally

    FLock.Leave();
  end;
end;


function TMfAudioEqBaseMFT.GetInputCurrentType(const dwInputStreamID: DWORD;
                                               out pType: IMFMediaType): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  pType := FInputType;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                                out pType: IMFMediaType): HResult;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  pType := FOutputType;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetInputStatus(const dwInputStreamID: DWORD;
                                          out pdwFlags: DWORD {MFT_INPUT_STATUS_ACCEPT_DATA}): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if FHaveInputSample then
    pdwFlags := 0
  else
    pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.GetOutputStatus(out pdwFlags: DWORD): HResult;
begin

  if FHaveInputSample then
    pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY
  else
    pdwFlags := 0;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.SetOutputBounds(hnsLowerBound, hnsUpperBound: LONGLONG): HResult;
begin
  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.ProcessEvent(const dwInputStreamID: DWORD;
                                        pEvent: IMFMediaEvent): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfAudioEqBaseMFT.ProcessMessage(eMessage: MFT_MESSAGE_TYPE; ulParam: ULONG_PTR): HResult;
begin

  FLock.Enter();

  try
    case eMessage of
      MFT_MESSAGE_COMMAND_FLUSH:
        begin
          FInputSample := nil;
          FHaveInputSample := False;
          ClearStateLocked;
        end;
    end;
  finally

    FLock.Leave();
  end;
  Result := S_OK;
end;


function TMfAudioEqBaseMFT.ProcessInput(const dwInputStreamID: DWORD;
                                        const pSample: IMFSample;
                                        dwFlags: DWORD = 0): HResult;
begin

  if dwInputStreamID <> 0 then Exit(MF_E_INVALIDSTREAMNUMBER);
  if pSample = nil then Exit(E_POINTER);
  if (FInputType = nil) or (FOutputType = nil) then Exit(MF_E_NOTACCEPTING);

  FLock.Enter();
  try
    if FHaveInputSample then Exit(MF_E_NOTACCEPTING);
    FInputSample := pSample;
    FHaveInputSample := True;
  finally
    FLock.Leave();
  end;

  Result := S_OK;
end;

function TMfAudioEqBaseMFT.ProcessOutput(dwFlags: DWORD; cOutputBufferCount: DWORD;
  pOutputSamples: PMFT_OUTPUT_DATA_BUFFER; out pdwStatus: DWORD): HResult;
var
  inBuf, outBuf: IMFMediaBuffer;
  pIn, pOut: PByte;
  cbIn, cbOut, cbMax: DWORD;
  hr: HRESULT;
  outSample: IMFSample;
  inSample: IMFSample;
  hnsTime, hnsDur: Int64;
begin
  pdwStatus := 0;

  // This base MFT is synchronous and supports exactly one output buffer.
  if cOutputBufferCount <> 1 then
    Exit(E_INVALIDARG);
  if pOutputSamples = nil then
    Exit(E_POINTER);
  if pOutputSamples.pSample = nil then
    Exit(E_POINTER);

  // Consume the pending input sample up-front to avoid "stuck" MF_E_NOTACCEPTING
  // if any later step fails. This keeps the MFT usable in manual push/pull pumps.
  FLock.Enter();
  try
    if not FHaveInputSample then
      Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

    inSample := FInputSample;
    FInputSample := nil;
    FHaveInputSample := False;
  finally
    FLock.Leave();
  end;

  outSample := pOutputSamples.pSample;

  // Get contiguous buffers for in/out
  hr := inSample.ConvertToContiguousBuffer(@inBuf);
  if Failed(hr) then
    Exit(hr);

  hr := outSample.ConvertToContiguousBuffer(@outBuf);
  if Failed(hr) then
    Exit(hr);

  hr := inBuf.Lock(pIn, @cbMax, @cbIn);
  if Failed(hr) then
    Exit(hr);

  try

    hr := outBuf.Lock(pOut, @cbMax, @cbOut); // cbOut = current length (may be 0)
    if Failed(hr) then
      Exit(hr);

    try

      if (cbMax < cbIn) then
        Exit(E_FAIL); // capacity too small

      // Copy input -> output, then let derived class process output in-place.
      Move(pIn^, pOut^, cbIn);
      outBuf.SetCurrentLength(cbIn);

      hr := ProcessAudioLocked(pOut, cbIn);

      // EQ failure must never break audio; treat as passthrough.
      if Failed(hr) then
        hr := S_OK;
    finally

      outBuf.Unlock;
    end;
  finally

    inBuf.Unlock;
  end;

  // Preserve sample timing if present on input.
  if Succeeded(inSample.GetSampleTime(@hnsTime)) then
    outSample.SetSampleTime(hnsTime);
  if Succeeded(inSample.GetSampleDuration(@hnsDur)) then
    outSample.SetSampleDuration(hnsDur);

  Result := hr;
end;
end.