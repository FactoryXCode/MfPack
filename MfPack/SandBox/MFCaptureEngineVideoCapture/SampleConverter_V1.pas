// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleConverter.pas
// Kind: Pascal Unit
// Release date: 29-03-2022
// Language: ENU
//
// Revision Version: 3.1.1
//
// Description:
//   This unit returns a BMP memory stream from an IMFSample.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
// 22/03/2022 Ciaran              Added support for sample conversion using color converter
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX311/Samples/MFFrameSample
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit SampleConverter_V1;
interface
uses
  {Winapi}
  WinApi.Windows,
  WinAPI.ActiveX,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.ComBaseApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.UuIds,
  {VCL}
  VCL.Graphics,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types;

type
  TVideoFormatInfo = record
  public
    iVideoWidth: UINT32;
    iVideoHeight: UINT32;
    iBufferWidth: UINT32;
    iBufferHeight: UINT32;
    iStride: UINT32;
    bTopDown: Boolean;

    // Major & Subtypes
    fSubType: TGuid;
    fMajorType: TGuid;

    // Supported framerates
    iFrameRate: UINT32;
    iFrameRateDenominator: UINT32;
    iMaxFrameRate: UINT32;
    iMaxFrameRateDenominator: UINT32;
    iMinFrameRate: UINT32;
    iMinFrameRateDenominator: UINT32;

    // AspectRatio
    AspectRatioSupported: Boolean;
    AspectRatioNumerator: UINT32;
    AspectRatioDenominator: UINT32;

    procedure Reset();
  end;

  TVideoFormatInfoArray = TArray<TVideoFormatInfo>;

type
  TSampleConverter = class(TPersistent)
  protected
    FOutputType: IMFMediaType;
    FTransform: IMFTransform;
    FSupportedInputs: TArray<TGUID>;
    FTopDownFormats: TArray<TGUID>;

  private

    function ConvertSampleToRGB(const AInputSample: IMFSample;
                                out AConvertedSample: IMFSample): HResult;

    function IndexOf(const AInput: TGUID;
                     const AValues: array of TGUID): Integer;

    function GetBMPFileHeader(): BITMAPFILEHEADER;
    function GetBMPFileInfo(const AVideoInfo: TVideoFormatInfo): BITMAPINFOHEADER;
    function CreateTransform(const AManager: IMFDXGIDeviceManager;
                             const AInputType: IMFMediaType): HResult;

    function IsTopDown(const ASubFormat: TGUID): Boolean;

    procedure FreeConverter();
    procedure NotifyBeginStreaming();
    procedure SetSupportedInputs();

  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateConverter(const AManager: IMFDXGIDeviceManager;
                             const AInputType: IMFMediaType): HResult;

    function DataFromSample(const ASample: IMFSample;
                            const AVideoInfo: TVideoFormatInfo;
                            out AMemoryStream: TMemoryStream): HResult;

    function IsInputSupported(const AInputFormat : TGUID) : Boolean;

  end;


implementation


constructor TSampleConverter.Create();
begin
  inherited;
  SetSupportedInputs;
end;


destructor TSampleConverter.Destroy();
begin
  FreeConverter;
  inherited;
end;


procedure TSampleConverter.SetSupportedInputs;
begin
  SetLength(FSupportedInputs, 20);
  FSupportedInputs[0] := MFVideoFormat_ARGB32;
  FSupportedInputs[1] := MFVideoFormat_RGB24;
  FSupportedInputs[2] := MFVideoFormat_RGB32;
  FSupportedInputs[3] := MFVideoFormat_RGB555;
  FSupportedInputs[4] := MFVideoFormat_RGB565;
  FSupportedInputs[5] := MFVideoFormat_RGB8;
  FSupportedInputs[6] := MFVideoFormat_AYUV;
  FSupportedInputs[7] := MFVideoFormat_I420;
  FSupportedInputs[8] := MFVideoFormat_IYUV;
  FSupportedInputs[9] := MFVideoFormat_NV11;
  FSupportedInputs[10] := MFVideoFormat_NV12;
  FSupportedInputs[11] := MFVideoFormat_UYVY;
  FSupportedInputs[12] := MFVideoFormat_V216;
  FSupportedInputs[13] := MFVideoFormat_V410;
  FSupportedInputs[14] := MFVideoFormat_Y41P;
  FSupportedInputs[15] := MFVideoFormat_Y41T;
  FSupportedInputs[16] := MFVideoFormat_Y42T;
  FSupportedInputs[17] := MFVideoFormat_YUY2;
  FSupportedInputs[18] := MFVideoFormat_YV12;
  FSupportedInputs[19] := MFVideoFormat_YVYU;

  SetLength(FTopDownFormats, 3);
  // Update list as needed for formats that return top-down data.
  FTopDownFormats[0] := MFVideoFormat_AYUV;
  FTopDownFormats[1] := MFVideoFormat_IYUV;
  FTopDownFormats[2] := MFVideoFormat_YUY2;
end;


function TSampleConverter.IsTopDown(const ASubFormat: TGUID): Boolean;
begin
  Result := IndexOf(ASubFormat, FTopDownFormats) > -1;
end;


function TSampleConverter.IsInputSupported(const AInputFormat: TGUID) : Boolean;
begin
  Result := IndexOf(AInputFormat, FSupportedInputs) > -1;
end;


function TSampleConverter.IndexOf(const AInput: TGUID; const AValues: array of TGUID): Integer;
begin
  Result := high(AValues);
  while (Result >= low(AValues)) and (AInput <> AValues[Result]) do
    Dec(Result);
end;


function TSampleConverter.DataFromSample(const ASample: IMFSample;
                                         const AVideoInfo: TVideoFormatInfo;
                                         out AMemoryStream: TMemoryStream): HResult;
var
  hr: HResult;
  pBuffer: IMFMediaBuffer;
  pConvertedSample: IMFSample;
  pBitmapData: PByte;
  cbBitmapData: DWord;
  iActualDataSize: Integer;
  iExpectedDataSize: Integer;
  oFileHeader: BITMAPFILEHEADER;
  oFileInfo: BITMAPINFOHEADER;

begin

  if (AVideoInfo.fSubType <> MFVideoFormat_RGB32) then
    begin
      hr := ConvertSampleToRGB(ASample,
                               pConvertedSample);
      if SUCCEEDED(hr) then
        // Converts a sample with multiple buffers into a sample with a single buffer.
        hr := pConvertedSample.ConvertToContiguousBuffer(pBuffer);
    end
  else
    hr := ConvertSampleToRGB(ASample,
                             pConvertedSample);

  if SUCCEEDED(hr) then
    begin

      hr := pBuffer.Lock(pBitmapData,
                         nil,
                         @cbBitmapData);

      try
      if SUCCEEDED(hr) then
        begin
          // For full frame capture, use the buffer dimensions for the data size check
          iExpectedDataSize := (AVideoInfo.iBufferWidth * 4) * AVideoInfo.iBufferHeight;
          iActualDataSize := Integer(cbBitmapData);

          if (iActualDataSize <> iExpectedDataSize) then
            begin
              {$IF DEBUG}
              OutputDebugString(PWideChar(Format('Sample size does not match expected size. Current: %d. Expected: %d',
                                                 [iActualDataSize, iExpectedDataSize])));
              {$ENDIF}
              hr := E_FAIL;
            end;
        AMemoryStream := TMemoryStream.Create;
        oFileHeader := GetBMPFileHeader;
        oFileInfo := GetBMPFileInfo(AVideoInfo);
        AMemoryStream.Write(oFileHeader, SizeOf(oFileHeader));
        AMemoryStream.Write(oFileInfo, SizeOf(oFileInfo));
        AMemoryStream.Write(pBitmapData[0], iActualDataSize);
      end;
    finally
      pBuffer.Unlock;
      SafeRelease(pBuffer);
    end;
  end;

  if Assigned(pConvertedSample) then
    SafeRelease(pConvertedSample);
end;


function TSampleConverter.GetBMPFileHeader: BITMAPFILEHEADER;
begin
  Result.bfType := Ord('B') or (Ord('M') shl 8); // Type is "BM" for BitMap
  Result.bfSize := sizeof(Result.bfOffBits) + sizeof(RGBTRIPLE);
  Result.bfReserved1 := 0;
  Result.bfReserved2 := 0;
  Result.bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
end;


function TSampleConverter.GetBMPFileInfo(const AVideoInfo: TVideoFormatInfo): BITMAPINFOHEADER;
begin
  // See: https://docs.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
  Result.biSize := sizeof(BITMAPINFOHEADER);
  Result.biWidth := AVideoInfo.iVideoWidth;

  if IsTopDown(AVideoInfo.fSubType) then
    Result.biHeight := -AVideoInfo.iVideoHeight
  else
    Result.biHeight := AVideoInfo.iVideoHeight;

  Result.biPlanes := 1;
  Result.biBitCount := 32;
  Result.biCompression := BI_RGB;
  Result.biClrImportant := 0;
  Result.biClrUsed := 0;
end;

procedure TSampleConverter.FreeConverter;
begin
  if Assigned(FTransform) then
    SafeRelease(FTransform);
  FOutputType := nil;
end;

function TSampleConverter.UpdateConverter(const AManager: IMFDXGIDeviceManager; const AInputType: IMFMediaType): HResult;
begin
  Result := CreateTransform(AManager, AInputType);

  NotifyBeginStreaming;
end;

function TSampleConverter.CreateTransform(const AManager: IMFDXGIDeviceManager; const AInputType: IMFMediaType): HResult;
var
  hr: HResult;

begin
  FreeConverter;

  // Create Video Processor MFT
  // https://docs.microsoft.com/en-us/windows/win32/medfound/video-processor-mft
  hr := CoCreateInstance(CLSID_VideoProcessorMFT,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMFTransform,
                         FTransform);

  if SUCCEEDED(hr) then
    begin
      // Set the input type for the transform
      if SUCCEEDED(hr) then
        hr := FTransform.SetInputType(0,
                                      AInputType,
                                      0);

      // Create the output type
      if SUCCEEDED(hr) then
        hr := MFCreateMediaType(FOutputType);

      // Copy all the properties from the input type to the output
      if SUCCEEDED(hr) then
        hr := AInputType.CopyAllItems(FOutputType);

      // Set sub type RGB32
      if SUCCEEDED(hr) then
        hr := FOutputType.SetGUID(MF_MT_SUBTYPE,
                                  MFVideoFormat_RGB32);

      // Assign the output type to the transform
      if SUCCEEDED(hr) then
        hr := FTransform.SetOutputType(0,
                                       FOutputType,
                                       0);

      // Is this needed?
      //if Assigned(AManager) then
      //  hr := FTransform.ProcessMessage(MFT_MESSAGE_SET_D3D_MANAGER,
      //                                  UInt64(AManager));
  end;

  Result := hr;
end;


procedure TSampleConverter.NotifyBeginStreaming();
begin
  // This should speed up the first frame request.
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/mft-message-notify-begin-streaming
  FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                            0);
end;


function TSampleConverter.ConvertSampleToRGB(const AInputSample: IMFSample;
                                             out AConvertedSample: IMFSample): HResult;
var
  hr: HResult;
  oStatus: DWord;
  oResult: HResult;
  pBufferOut: IMFMediaBuffer;
  oOutputDataBuffer: MFT_OUTPUT_DATA_BUFFER;
  oOutputStreamInfo: MFT_OUTPUT_STREAM_INFO;
  iConvertStart: int64;
  iConvertEnd: int64;

begin

  hr := FTransform.ProcessInput(0,
                                AInputSample,
                                0);

  if SUCCEEDED(hr) then
    begin
      hr := FTransform.GetOutputStreamInfo(0, oOutputStreamInfo);
      try
        if SUCCEEDED(hr) then
          hr := MFCreateMemoryBuffer(oOutputStreamInfo.cbSize, pBufferOut);
        if SUCCEEDED(hr) then
          hr := MFCreateSample(AConvertedSample);
        if SUCCEEDED(hr) then
          hr := AConvertedSample.AddBuffer(pBufferOut);
        if SUCCEEDED(hr)  then
          begin
            oOutputDataBuffer.dwStreamID := 0;
            oOutputDataBuffer.dwStatus := 0;
            oOutputDataBuffer.pSample := AConvertedSample;
            oOutputDataBuffer.pEvents := nil;
            hr := FTransform.ProcessOutput(0,
                                           1,
                                           @oOutputDataBuffer,
                                           oStatus);
            // If we don't flush we will get MF_E_NOTACCEPTING on next ProcessInput
            FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                      0);
          end;
      finally
        SafeRelease(pBufferOut);
      end;
  end;
  Result := hr;
end;

{ TVideoFormatInfo }
procedure TVideoFormatInfo.Reset();
begin
  iVideoWidth := 0;
  iVideoHeight := 0;
  iBufferWidth := 0;
  iBufferHeight := 0;
  iStride := 0;
  bTopDown := False;

  // Major & Subtypes
  fSubType := GUID_NULL;
  fMajorType := GUID_NULL;

  // Supported framerates
  iFrameRate := 0;
  iFrameRateDenominator := 0;
  iMaxFrameRate := 0;
  iMaxFrameRateDenominator := 0;
  iMinFrameRate := 0;
  iMinFrameRateDenominator := 0;

  // Aspectratio
  AspectRatioSupported := False;
  AspectRatioNumerator := 0;
  AspectRatioDenominator := 0;
end;

end.
