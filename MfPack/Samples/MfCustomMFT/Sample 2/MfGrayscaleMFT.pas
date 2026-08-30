// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfGrayscaleMFT.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: A minimal synchronous Media Foundation Transform.
//              It accepts RGB32 video, converts every pixel to grayscale in place, and
//              returns the same IMFSample from ProcessOutput.
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
unit MfGrayscaleMFT;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

type

  TMfGrayscaleMFT = class(TInterfacedObject, IMFTransform)
  private
    FInputType: IMFMediaType;
    FOutputType: IMFMediaType;
    FInputSample: IMFSample;
    FWidth: UINT32;
    FHeight: UINT32;
    FStride: Integer;

    procedure ClearInput();
    function CloneMediaType(const ASource: IMFMediaType;
                            out ADestination: IMFMediaType): HResult;

    function CreateRgb32Type(out AMediaType: IMFMediaType): HResult;

    function GetVideoLayout(const AMediaType: IMFMediaType;
                            out AWidth: UINT32;
                            out AHeight: UINT32;
                            out AStride: Integer): HResult;

    function TypesMatch(const AFirst: IMFMediaType;
                        const ASecond: IMFMediaType): Boolean;

    function ConvertSampleToGrayscale(const ASample: IMFSample): HResult;

  public

    destructor Destroy(); override;

    // IMFTransform implementation
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
                          const pSample:
                          IMFSample; dwFlags:
                          DWORD = 0): HResult; stdcall;

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HResult; stdcall;
  end;


implementation


destructor TMfGrayscaleMFT.Destroy();
begin

  ClearInput();
  FInputType := nil;
  FOutputType := nil;

  inherited;
end;


procedure TMfGrayscaleMFT.ClearInput();
begin

  FInputSample := nil;
end;


function TMfGrayscaleMFT.CloneMediaType(const ASource: IMFMediaType;
                                        out ADestination: IMFMediaType): HResult;
begin

  ADestination := nil;

  if not Assigned(ASource) then
    Exit(E_POINTER);

  Result := MFCreateMediaType(ADestination);

  if SUCCEEDED(Result) then
    Result := ASource.CopyAllItems(ADestination);
end;


function TMfGrayscaleMFT.CreateRgb32Type(out AMediaType: IMFMediaType): HResult;
begin

  AMediaType := nil;
  Result := MFCreateMediaType(AMediaType);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Video);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetGUID(MF_MT_SUBTYPE,
                                 MFVideoFormat_RGB32);
end;


function TMfGrayscaleMFT.GetVideoLayout(const AMediaType: IMFMediaType;
                                        out AWidth: UINT32;
                                        out AHeight: UINT32;
                                        out AStride: Integer): HResult;
var
  MajorType: TGUID;
  Subtype: TGUID;
  StrideValue: UINT32;

begin
  AWidth := 0;
  AHeight := 0;
  AStride := 0;

  if not Assigned(AMediaType) then
    Exit(E_POINTER);

  if FAILED(AMediaType.GetGUID(MF_MT_MAJOR_TYPE,
                              MajorType)) or
     not IsEqualGUID(MajorType,
                     MFMediaType_Video) or
     FAILED(AMediaType.GetGUID(MF_MT_SUBTYPE,
                               Subtype)) or
     not IsEqualGUID(Subtype,
                     MFVideoFormat_RGB32) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := MFGetAttributeSize(AMediaType,
                               MF_MT_FRAME_SIZE,
                               AWidth,
                               AHeight);
  if FAILED(Result) then
    Exit;

  if (AWidth = 0) or (AHeight = 0) then
    Exit(MF_E_INVALIDMEDIATYPE);

  AStride := Integer(AWidth) * 4;

  if SUCCEEDED(AMediaType.GetUINT32(MF_MT_DEFAULT_STRIDE,
                                    StrideValue)) then
    begin
      AStride := Integer(StrideValue);
      if (AStride < 0) then
        AStride := -AStride;
    end;

  if (AStride < Integer(AWidth) * 4) then
    Exit(MF_E_INVALIDMEDIATYPE);
  Result := S_OK;
end;


function TMfGrayscaleMFT.TypesMatch(const AFirst: IMFMediaType;
                                    const ASecond: IMFMediaType): Boolean;
var
  Width1: UINT32;
  Height1: UINT32;
  Width2: UINT32;
  Height2: UINT32;
  Stride1: Integer;
  Stride2: Integer;

begin

  Result := SUCCEEDED(GetVideoLayout(AFirst,
                                     Width1,
                                     Height1,
                                     Stride1)) and

            SUCCEEDED(GetVideoLayout(ASecond,
                                     Width2,
                                     Height2,
                                     Stride2)) and
            (Width1 = Width2) and
            (Height1 = Height2) and
            (Stride1 = Stride2);
end;


function TMfGrayscaleMFT.ConvertSampleToGrayscale(const ASample: IMFSample): HResult;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  RequiredLength: UInt64;
  Row: PByte;
  Pixel: PByte;
  X: UINT32;
  Y: UINT32;
  Gray: Byte;

begin

  Buffer := nil;
  Data := nil;
  Result := ASample.ConvertToContiguousBuffer(@Buffer);

  if FAILED(Result) then
    Exit;

  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    RequiredLength := UInt64(FStride) * UInt64(FHeight);

    if (UInt64(CurrentLength) < RequiredLength) or
       (UInt64(MaxLength) < RequiredLength) then
      Exit(MF_E_BUFFERTOOSMALL);

    for Y := 0 to FHeight - 1 do
      begin
        Row := PByte(NativeUInt(Data) + NativeUInt(Y) * NativeUInt(FStride));

        for X := 0 to FWidth - 1 do
          begin
            Pixel := PByte(NativeUInt(Row) + NativeUInt(X) * 4);
            // RGB32 is stored in memory as B, G, R, unused/alpha.
            Gray := Byte((29 * Cardinal(Pixel[0]) +
                          150 * Cardinal(Pixel[1]) +
                          77 * Cardinal(Pixel[2]) + 128) shr 8);
            Pixel[0] := Gray;
            Pixel[1] := Gray;
            Pixel[2] := Gray;
          end;
      end;

    Result := S_OK;

  finally
    Buffer.Unlock();
  end;
end;


function TMfGrayscaleMFT.GetStreamLimits(out pdwInputMinimum: DWORD;
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


function TMfGrayscaleMFT.GetStreamCount(out pcInputStreams,
                                        pcOutputStreams: DWORD): HResult;
begin

  pcInputStreams := 1;
  pcOutputStreams := 1;
  Result := S_OK;
end;


function TMfGrayscaleMFT.GetStreamIDs(dwInputIDArraySize: DWORD;
                                      pdwInputIDs: WinApi.WinApiTypes.PDWORD;
                                      dwOutputIDArraySize: DWORD;
                                      pdwOutputIDs: WinApi.WinApiTypes.PDWORD): HResult;
begin

  // E_NOTIMPL means that both streams use the default identifier zero.
  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.GetInputStreamInfo(const dwInputStreamID: DWORD;
                                            out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_INPUT_STREAM_WHOLE_SAMPLES or
                         MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_INPUT_STREAM_PROCESSES_IN_PLACE;

  if (FStride > 0) and (FHeight > 0) then
    pStreamInfo.cbSize := DWORD(UInt64(FStride) * UInt64(FHeight));

  Result := S_OK;
end;


function TMfGrayscaleMFT.GetOutputStreamInfo(const dwOutputStreamID: DWORD;
                                             out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult;
begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  ZeroMemory(@pStreamInfo,
             SizeOf(pStreamInfo));

  pStreamInfo.dwFlags := MFT_OUTPUT_STREAM_WHOLE_SAMPLES or
                         MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER or
                         MFT_OUTPUT_STREAM_PROVIDES_SAMPLES;

  if (FStride > 0) and (FHeight > 0) then
    pStreamInfo.cbSize := DWORD(UInt64(FStride) * UInt64(FHeight));

  Result := S_OK;
end;


function TMfGrayscaleMFT.GetAttributes(out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.GetInputStreamAttributes(const dwInputStreamID: DWORD;
                                                  out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;
  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.GetOutputStreamAttributes(const dwOutputStreamID: DWORD;
                                                   out pAttributes: IMFAttributes): HResult;
begin

  pAttributes := nil;

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.DeleteInputStream(dwStreamID: DWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.AddInputStreams(cStreams: DWORD;
                                         adwStreamIDs: WinApi.WinApiTypes.PDWORD): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.GetInputAvailableType(const dwInputStreamID: DWORD;
                                               dwTypeIndex: DWORD;
                                               out pType: IMFMediaType): HResult;
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


function TMfGrayscaleMFT.GetOutputAvailableType(const dwOutputStreamID: DWORD;
                                                dwTypeIndex: DWORD;
                                                out pType: IMFMediaType): HResult;
begin

  pType := nil;

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwTypeIndex <> 0) then
    Exit(MF_E_NO_MORE_TYPES);

  if Assigned(FInputType) then
    Result := CloneMediaType(FInputType, pType)
  else
    Result := CreateRgb32Type(pType);
end;


function TMfGrayscaleMFT.SetInputType(const dwInputStreamID: DWORD;
                                      pType: IMFMediaType;
                                      dwFlags: DWORD): HResult;
var
  NewType: IMFMediaType;
  Width: UINT32;
  Height: UINT32;
  Stride: Integer;

begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if (dwFlags and not (MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(E_INVALIDARG);

  if not Assigned(pType) then
    begin
      if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) = 0) then
        begin
          FInputType := nil;
          ClearInput;
        end;
      Exit(S_OK);
    end;

  Result := GetVideoLayout(pType,
                           Width,
                           Height,
                           Stride);

  if FAILED(Result) then
    Exit;

  if Assigned(FOutputType) and not TypesMatch(pType,
                                              FOutputType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(S_OK);

  Result := CloneMediaType(pType,
                           NewType);
  if FAILED(Result) then
    Exit;

  FInputType := NewType;
  FWidth := Width;
  FHeight := Height;
  FStride := Stride;
  ClearInput;
  Result := S_OK;
end;


function TMfGrayscaleMFT.SetOutputType(dwOutputStreamID: DWORD;
                                       pType: IMFMediaType;
                                       dwFlags: DWORD): HResult;
var
  NewType: IMFMediaType;
  Width: UINT32;
  Height: UINT32;
  Stride: Integer;

begin

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if ((dwFlags and not MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(E_INVALIDARG);

  if not Assigned(pType) then
    begin
      if ((dwFlags and MFT_SET_TYPE_TEST_ONLY) = 0) then
        begin
          FOutputType := nil;
          ClearInput;
        end;
      Exit(S_OK);
    end;

  Result := GetVideoLayout(pType,
                           Width,
                           Height,
                           Stride);
  if FAILED(Result) then
    Exit;

  if Assigned(FInputType) and not TypesMatch(FInputType, pType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  if (dwFlags and (MFT_SET_TYPE_TEST_ONLY) <> 0) then
    Exit(S_OK);

  Result := CloneMediaType(pType,
                           NewType);
  if FAILED(Result) then
    Exit;

  FOutputType := NewType;
  FWidth := Width;
  FHeight := Height;
  FStride := Stride;
  ClearInput;
  Result := S_OK;
end;


function TMfGrayscaleMFT.GetInputCurrentType(const dwInputStreamID: DWORD;
                                             out pType: IMFMediaType): HResult;
begin

  pType := nil;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if not Assigned(FInputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  Result := CloneMediaType(FInputType,
                           pType);
end;


function TMfGrayscaleMFT.GetOutputCurrentType(const dwOutputStreamID: DWORD;
                                              out pType: IMFMediaType): HResult;
begin

  pType := nil;

  if (dwOutputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  Result := CloneMediaType(FOutputType,
                           pType);
end;


function TMfGrayscaleMFT.GetInputStatus(const dwInputStreamID: DWORD;
                                        out pdwFlags: DWORD): HResult;
begin

  pdwFlags := 0;

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if not Assigned(FInputType) or not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if not Assigned(FInputSample) then
    pdwFlags := MFT_INPUT_STATUS_ACCEPT_DATA;

  Result := S_OK;
end;


function TMfGrayscaleMFT.GetOutputStatus(out pdwFlags: DWORD): HResult;
begin

  pdwFlags := 0;

  if not Assigned(FInputType) or not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if Assigned(FInputSample) then
    pdwFlags := MFT_OUTPUT_STATUS_SAMPLE_READY;

  Result := S_OK;
end;


function TMfGrayscaleMFT.SetOutputBounds(hnsLowerBound,
                                         hnsUpperBound: LONGLONG): HResult;
begin

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.ProcessEvent(const dwInputStreamID: DWORD;
                                      pEvent: IMFMediaEvent): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  Result := E_NOTIMPL;
end;


function TMfGrayscaleMFT.ProcessMessage(eMessage: MFT_MESSAGE_TYPE;
                                        ulParam: ULONG_PTR): HResult;
begin

  case eMessage of
    MFT_MESSAGE_COMMAND_FLUSH,
    MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
    MFT_MESSAGE_NOTIFY_START_OF_STREAM,
    MFT_MESSAGE_NOTIFY_END_STREAMING: ClearInput();
  end;

  Result := S_OK;
end;


function TMfGrayscaleMFT.ProcessInput(const dwInputStreamID: DWORD;
                                      const pSample: IMFSample;
                                      dwFlags: DWORD): HResult;
begin

  if (dwInputStreamID <> 0) then
    Exit(MF_E_INVALIDSTREAMNUMBER);

  if not Assigned(pSample) then
    Exit(E_POINTER);

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if not Assigned(FInputType) or not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if Assigned(FInputSample) then
    Exit(MF_E_NOTACCEPTING);

  FInputSample := pSample;
  Result := S_OK;
end;


function TMfGrayscaleMFT.ProcessOutput(dwFlags,
                                       cOutputBufferCount: DWORD;
                                       pOutputSamples: PMFT_OUTPUT_DATA_BUFFER;
                                       out pdwStatus: DWORD): HResult;
var
  Sample: IMFSample;

begin

  pdwStatus := 0;

  if (dwFlags <> 0) then
    Exit(E_INVALIDARG);

  if (cOutputBufferCount <> 1) or not Assigned(pOutputSamples) then
    Exit(E_INVALIDARG);

  if not Assigned(FInputType) or not Assigned(FOutputType) then
    Exit(MF_E_TRANSFORM_TYPE_NOT_SET);

  if not Assigned(FInputSample) then
    Exit(MF_E_TRANSFORM_NEED_MORE_INPUT);

  Sample := FInputSample;
  Result := ConvertSampleToGrayscale(Sample);

  if FAILED(Result) then
    begin
      ClearInput();
      Exit;
    end;

  pOutputSamples^.dwStreamID := 0;
  pOutputSamples^.dwStatus := 0;
  pOutputSamples^.pEvents := nil;
  pOutputSamples^.pSample := Sample;
  ClearInput;
  Result := S_OK;
end;

end.
