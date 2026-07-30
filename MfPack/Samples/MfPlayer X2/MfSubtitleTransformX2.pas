// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfSubtitleTransformX2.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: MfPlayer X2 playback transform. Receives decoded RGB32 video samples, burns
//              subtitles into those samples in-place, and passes them on to the EVR.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Carmen (carmenh).
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
// Source: Parts of CPlayer Examples
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfSubtitleTransformX2;

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  {Project}
  MfSubtitleCompositorX2;

type
  IMfSubtitleVideoTransformControl = interface(IUnknown)
    ['{B22D997C-8AFD-4D35-9877-8FC82C5F6E09}']
    function SetEnabled(AEnabled: BOOL): HRESULT; stdcall;
    function GetEnabled(out AEnabled: BOOL): HRESULT; stdcall;
  end;

  TMfSubtitleVideoTransform = class(TInterfacedObject, IMFTransform, IMfSubtitleVideoTransformControl)
  private
    FCompositor: TMfSubtitleCompositor;
    FInputType: IMFMediaType;
    FOutputType: IMFMediaType;
    FInputSample: IMFSample;
    FHaveInput: Boolean;
    FWidth: UINT32;
    FHeight: UINT32;
    FStride: Integer;
    FEnabled: Boolean;

    procedure ClearInput();
    function CreateRgb32Type(out MediaType: IMFMediaType): HRESULT;
    function CloneMediaType(const SourceType: IMFMediaType;
                            out DestType: IMFMediaType): HRESULT;
    function IsTypeSupported(const MediaType: IMFMediaType): Boolean;
    function ParseVideoType(const MediaType: IMFMediaType): HRESULT;
    function CompositeSample(const Sample: IMFSample): HRESULT;

    function SetEnabled(AEnabled: BOOL): HRESULT; stdcall;
    function GetEnabled(out AEnabled: BOOL): HRESULT; stdcall;
  public
    constructor Create(Compositor: TMfSubtitleCompositor);
    destructor Destroy(); override;

    function GetStreamLimits(out pdwInputMinimum: DWORD;
                             out pdwInputMaximum: DWORD;
                             out pdwOutputMinimum: DWORD;
                             out pdwOutputMaximum: DWORD): HRESULT; stdcall;

    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HRESULT; stdcall;

    function GetStreamIDs(dwInputIDArraySize: DWORD;
                          pdwInputIDs: PDWORD;
                          dwOutputIDArraySize: DWORD;
                          pdwOutputIDs: PDWORD): HRESULT; stdcall;

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
                            out pdwFlags: DWORD): HRESULT; stdcall;

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

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HRESULT; stdcall;
  end;

implementation

constructor TMfSubtitleVideoTransform.Create(Compositor: TMfSubtitleCompositor);
begin
  inherited Create();
  FCompositor := Compositor;
  FStride := 0;
  FEnabled := True;
  ClearInput();
end;


destructor TMfSubtitleVideoTransform.Destroy();
begin
  ClearInput();
  FInputType := nil;
  FOutputType := nil;
  FCompositor := nil;
  inherited;
end;


procedure TMfSubtitleVideoTransform.ClearInput();
begin
  FInputSample := nil;
  FHaveInput := False;
end;


function TMfSubtitleVideoTransform.CreateRgb32Type(out MediaType: IMFMediaType): HRESULT;
begin
  MediaType := nil;

  Result := MFCreateMediaType(MediaType);
  if FAILED(Result) then
    Exit;

  Result := MediaType.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := MediaType.SetGUID(MF_MT_SUBTYPE,
                              MFVideoFormat_RGB32);
  if FAILED(Result) then
    Exit;

  if (FWidth > 0) and (FHeight > 0) then
    begin
      Result := MFSetAttributeSize(MediaType,
                                   MF_MT_FRAME_SIZE,
                                   FWidth,
                                   FHeight);
      if FAILED(Result) then
        Exit;

      Result := MediaType.SetUINT32(MF_MT_DEFAULT_STRIDE,
                                    UINT32(Integer(FWidth) * 4));
    end;
end;


function TMfSubtitleVideoTransform.CloneMediaType(const SourceType: IMFMediaType;
                                                  out DestType: IMFMediaType): HRESULT;
begin
  DestType := nil;
  if not Assigned(SourceType) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := MFCreateMediaType(DestType);
  if FAILED(Result) then
    Exit;

  Result := SourceType.CopyAllItems(DestType);
end;


function TMfSubtitleVideoTransform.IsTypeSupported(const MediaType: IMFMediaType): Boolean;
var
  majorType: TGUID;
  subType: TGUID;
begin
  Result := False;

  if not Assigned(MediaType) then
    Exit;

  if FAILED(MediaType.GetGUID(MF_MT_MAJOR_TYPE,
                              majorType)) then
    Exit;

  if not IsEqualGUID(majorType,
                     MFMediaType_Video) then
    Exit;

  if FAILED(MediaType.GetGUID(MF_MT_SUBTYPE,
                              subType)) then
    Exit;

  Result := IsEqualGUID(subType,
                        MFVideoFormat_RGB32);
end;


function TMfSubtitleVideoTransform.ParseVideoType(const MediaType: IMFMediaType): HRESULT;
var
  width: UINT32;
  height: UINT32;
  strideValue: UINT32;
  stride: Integer;
begin
  if not IsTypeSupported(MediaType) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  Result := MFGetAttributeSize(MediaType,
                               MF_MT_FRAME_SIZE,
                               width,
                               height);
  if FAILED(Result) then
    Exit;

  if (width = 0) or (height = 0) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  stride := Integer(width) * 4;
  if SUCCEEDED(MediaType.GetUINT32(MF_MT_DEFAULT_STRIDE,
                                   strideValue)) then
    begin
      stride := Integer(strideValue);
      if (stride < 0) then
        stride := -stride;
    end;

  if (stride < (Integer(width) * 4)) then
    stride := Integer(width) * 4;

  FWidth := width;
  FHeight := height;
  FStride := stride;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.CompositeSample(const Sample: IMFSample): HRESULT;
var
  buffer: IMFMediaBuffer;
  data: PByte;
  maxLength: DWORD;
  currentLength: DWORD;
  sampleTime: LONGLONG;
begin
  Result := S_OK;

  if (not FEnabled) or
     (not Assigned(FCompositor)) or
     (not FCompositor.TimedTextFileLoaded) or
     (FWidth = 0) or
     (FHeight = 0) or
     (FStride <= 0) then
    Exit;

  buffer := nil;
  data := nil;
  maxLength := 0;
  currentLength := 0;
  sampleTime := 0;

  Result := Sample.ConvertToContiguousBuffer(@buffer);
  if FAILED(Result) then
    Exit;

  Result := buffer.Lock(data,
                        @maxLength,
                        @currentLength);
  if FAILED(Result) then
    Exit;

  try
    if FAILED(Sample.GetSampleTime(@sampleTime)) then
      sampleTime := 0;

    Result := FCompositor.CompositeRgb32(data,
                                         currentLength,
                                         Integer(FWidth),
                                         Integer(FHeight),
                                         FStride,
                                         sampleTime div 10000);
  finally
    buffer.Unlock();
  end;
end;


function TMfSubtitleVideoTransform.SetEnabled(AEnabled: BOOL): HRESULT;
begin
  FEnabled := AEnabled;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetEnabled(out AEnabled: BOOL): HRESULT;
begin
  AEnabled := FEnabled;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetStreamLimits(out pdwInputMinimum: DWORD;
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


function TMfSubtitleVideoTransform.GetStreamCount(out pcInputStreams: DWORD;
                                                  out pcOutputStreams: DWORD): HRESULT;
begin
  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetStreamIDs(dwInputIDArraySize: DWORD;
                                                pdwInputIDs: PDWORD;
                                                dwOutputIDArraySize: DWORD;
                                                pdwOutputIDs: PDWORD): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                                       out pStreamInfo: MFT_INPUT_STREAM_INFO): HRESULT;
begin
  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));
  // This transform edits the input sample directly and returns that same
  // sample from ProcessOutput. Advertise the in-place processing model so the
  // Media Session can pair this input stream with the output stream correctly.
  pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or
                         MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_INPUT_STREAM_PROCESSES_IN_PLACE;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                                        out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HRESULT;
begin
  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));
  pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or
                         MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_OUTPUT_STREAM_PROVIDES_SAMPLES;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetAttributes(out pAttributes: IMFAttributes): HRESULT;
begin
  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                            out pAttributes: IMFAttributes): HRESULT;
begin
  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                             out pAttributes: IMFAttributes): HRESULT;
begin
  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.DeleteInputStream(dwStreamID: DWORD): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.AddInputStreams(cStreams: DWORD;
                                                   adwStreamIDs: PDWORD): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.GetInputAvailableType(const dwInputStreamID: DWORD;
                                                          dwTypeIndex: DWORD;
                                                          out pType: IMFMediaType): HRESULT;
begin
  pType := nil;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwTypeIndex <> 0) then
    Exit(MF_E_NO_MORE_TYPES);

  if Assigned(FOutputType) then
    Result := CloneMediaType(FOutputType,
                             pType)
  else
    Result := CreateRgb32Type(pType);
end;


function TMfSubtitleVideoTransform.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                           dwTypeIndex: DWORD;
                                                           out pType: IMFMediaType): HRESULT;
begin
  pType := nil;

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwTypeIndex <> 0) then
    Exit(MF_E_NO_MORE_TYPES);

  if Assigned(FInputType) then
    Result := CloneMediaType(FInputType,
                             pType)
  else
    Result := CreateRgb32Type(pType);
end;


function TMfSubtitleVideoTransform.SetInputType(const dwInputStreamID: DWORD;
                                                pType: IMFMediaType;
                                                dwFlags: DWORD): HRESULT;
begin
  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (pType = nil) then
    begin
      if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
        Exit(S_OK);

      FInputType := nil;
      ClearInput();
      Exit(S_OK);
    end;

  if not IsTypeSupported(pType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(S_OK);

  Result := ParseVideoType(pType);
  if FAILED(Result) then
    Exit;

  FInputType := pType;
  ClearInput();
end;


function TMfSubtitleVideoTransform.SetOutputType(dwOutputStreamID: DWORD;
                                                 pType: IMFMediaType;
                                                 dwFlags: DWORD): HRESULT;
begin
  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (pType = nil) then
    begin
      if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
        Exit(S_OK);

      FOutputType := nil;
      Exit(S_OK);
    end;

  if not IsTypeSupported(pType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(S_OK);

  Result := ParseVideoType(pType);
  if FAILED(Result) then
    Exit;

  FOutputType := pType;
end;


function TMfSubtitleVideoTransform.GetInputCurrentType(const dwInputStreamID: DWORD;
                                                        out pType: IMFMediaType): HRESULT;
begin
  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  pType := FInputType;
  if Assigned(pType) then
    Result := S_OK
  else
    Result := MF_E_TRANSFORM_TYPE_NOT_SET;
end;


function TMfSubtitleVideoTransform.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                                         out pType: IMFMediaType): HRESULT;
begin
  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  pType := FOutputType;
  if Assigned(pType) then
    Result := S_OK
  else
    Result := MF_E_TRANSFORM_TYPE_NOT_SET;
end;


function TMfSubtitleVideoTransform.GetInputStatus(const dwInputStreamID: DWORD;
                                                  out pdwFlags: DWORD): HRESULT;
begin
  pdwFlags := 0;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  // Do not advertise that input is accepted until both sides of the MFT have
  // negotiated a valid type. Returning ACCEPT_DATA too early can leave the
  // topology pump in an invalid state during resolution/start-up.
  if (not Assigned(FInputType)) or (not Assigned(FOutputType)) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if not FHaveInput then
    pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA;

  Result := S_OK;
end;


function TMfSubtitleVideoTransform.GetOutputStatus(out pdwFlags: DWORD): HRESULT;
begin
  pdwFlags := 0;

  if (not Assigned(FInputType)) or (not Assigned(FOutputType)) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if FHaveInput then
    pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY;

  Result := S_OK;
end;


function TMfSubtitleVideoTransform.SetOutputBounds(hnsLowerBound: LONGLONG;
                                                   hnsUpperBound: LONGLONG): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.ProcessEvent(const dwInputStreamID: DWORD;
                                                 pEvent: IMFMediaEvent): HRESULT;
begin
  Result := E_NOTIMPL;
end;


function TMfSubtitleVideoTransform.ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                                                  ulParam: ULONG_PTR): HRESULT;
begin
  case eMessage of
    MFT_MESSAGE_COMMAND_FLUSH,
    MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
    MFT_MESSAGE_NOTIFY_START_OF_STREAM,
    MFT_MESSAGE_NOTIFY_END_STREAMING:
      ClearInput();
  end;

  // This synchronous one-input/one-output transform stores at most one sample,
  // so COMMAND_DRAIN needs no special buffering implementation.
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.ProcessInput(const dwInputStreamID: DWORD;
                                                const pSample: IMFSample;
                                                dwFlags: DWORD): HRESULT;
begin
  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if not Assigned(pSample) then
    Exit(E_POINTER);

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if (not Assigned(FInputType)) or (not Assigned(FOutputType)) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if FHaveInput then
    Exit(MF_E_NOTACCEPTING);

  FInputSample := pSample;
  FHaveInput := True;
  Result := S_OK;
end;


function TMfSubtitleVideoTransform.ProcessOutput(dwFlags: DWORD;
                                                 cOutputBufferCount: DWORD;
                                                 pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                                                 out pdwStatus: DWORD): HRESULT;
var
  sample: IMFSample;
begin
  pdwStatus := 0;

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if (cOutputBufferCount <> 1) or (pOutputSamples = nil) then
    Exit(E_INVALIDARG);

  if (not Assigned(FInputType)) or (not Assigned(FOutputType)) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  // For a synchronous MFT it is normal for ProcessOutput to be called first.
  // NEED_MORE_INPUT tells the topology pump to obtain an upstream sample and
  // deliver it through ProcessInput.
  if not FHaveInput then
    Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

  sample := FInputSample;
  Result := CompositeSample(sample);
  if FAILED(Result) then
    begin
      ClearInput();
      Exit;
    end;

  pOutputSamples^.dwStreamID := 0;
  pOutputSamples^.dwStatus := 0;
  pOutputSamples^.pEvents := nil;
  pOutputSamples^.pSample := sample;

  ClearInput();
  Result := S_OK;
end;

end.
