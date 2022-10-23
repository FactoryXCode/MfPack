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
// 28/06/2022 All                 Mercury release  SDK 10.0.22621.0 (Windows 11)
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
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  {VCL}
  VCL.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.UuIds,
  {Application}
  DeviceExplorer,
  Utils;

type
  TSampleConverter = class
  protected
    FOutputType: IMFMediaType;
    FTransform: IMFTransform;
    FSupportedInputs: array of TGUID;
    FTopDownFormats: array of TGUID;

  private

    function ConvertSampleToRGB(const pInputSample: IMFSample;
                                out pConvertedSample: IMFSample): HResult;

    function IndexOf(const pInput: TGUID;
                     const pValues: array of TGUID): Integer;

    function GetBMPFileHeader(): BITMAPFILEHEADER;
    function GetBMPFileInfo(const pVideoInfo: TVideoFormatInfo): BITMAPINFOHEADER;
    function CreateTransform(const pManager: IMFDXGIDeviceManager;
                             const pInputType: IMFMediaType): HResult;

    function IsTopDown(const pSubFormat: TGUID): Boolean;

    procedure FreeConverter();
    procedure NotifyBeginStreaming();
    procedure SetSupportedInputs();

  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateConverter(const pManager: IMFDXGIDeviceManager;
                             const pInputType: IMFMediaType): HResult;

    function DataFromSample(const pSample: IMFSample;
                            const pVideoInfo: TVideoFormatInfo;
                            out pStream: TMemoryStream): HResult;

    function IsInputSupported(const pInputFormat: TGUID) : Boolean;

  end;


implementation


constructor TSampleConverter.Create();
begin
  inherited Create();
  SetSupportedInputs();
end;


destructor TSampleConverter.Destroy();
begin
  FreeConverter();
  inherited;
end;


procedure TSampleConverter.SetSupportedInputs();
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


function TSampleConverter.IsTopDown(const pSubFormat: TGUID): Boolean;
begin
  Result := IndexOf(pSubFormat,
                    FTopDownFormats) > -1;
end;


function TSampleConverter.IsInputSupported(const pInputFormat: TGUID): Boolean;
begin
  Result := IsMfSupportedFormat(pInputFormat);
end;


function TSampleConverter.IndexOf(const pInput: TGUID;
                                  const pValues: array of TGUID): Integer;
begin
  Result := high(pValues);
  while (Result >= low(pValues)) and (pInput <> pValues[Result]) do
    Dec(Result);
end;

 // 1st step
function TSampleConverter.DataFromSample(const pSample: IMFSample;
                                         const pVideoInfo: TVideoFormatInfo;
                                         out pStream: TMemoryStream): HResult;
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

  if (pVideoInfo.fSubType <> MFVideoFormat_RGB32) then
    begin
      hr := ConvertSampleToRGB(pSample,
                               pConvertedSample);
      if (SUCCEEDED(hr)) then
        // Converts a sample with multiple buffers into a sample with a single buffer.
        hr := pConvertedSample.ConvertToContiguousBuffer(pBuffer);
    end
  else
    hr := ConvertSampleToRGB(pSample,
                             pConvertedSample);

  if (SUCCEEDED(hr)) then
    begin

      hr := pBuffer.Lock(pBitmapData,
                         nil,
                         @cbBitmapData);

      try
      if (SUCCEEDED(hr)) then
        begin
          // For full frame capture, use the buffer dimensions for the data size check
          iExpectedDataSize := (pVideoInfo.iBufferWidth * 4) * pVideoInfo.iBufferHeight;
          iActualDataSize := Integer(cbBitmapData);

          if (iActualDataSize <> iExpectedDataSize) then
            begin
              {$IF DEBUG}
              OutputDebugString(PWideChar(Format('Sample size does not match expected size. Current: %d. Expected: %d',
                                                 [iActualDataSize, iExpectedDataSize])));
              {$ENDIF}
              hr := E_FAIL;
            end;

        pStream := TMemoryStream.Create;
        oFileHeader := GetBMPFileHeader;
        oFileInfo := GetBMPFileInfo(pVideoInfo);
        pStream.Write(oFileHeader, SizeOf(oFileHeader));
        pStream.Write(oFileInfo, SizeOf(oFileInfo));
        pStream.Write(pBitmapData[0], iActualDataSize);

      end;
    finally
      pBuffer.Unlock();
      SafeRelease(pBuffer);
    end;
  end;

  if Assigned(pConvertedSample) then
    SafeRelease(pConvertedSample);
  Result := hr;
end;


function TSampleConverter.GetBMPFileHeader: BITMAPFILEHEADER;
begin
  Result.bfType := Ord('B') or (Ord('M') shl 8); // Type is "BM" for BitMap
  Result.bfSize := sizeof(Result.bfOffBits) + sizeof(RGBTRIPLE);
  Result.bfReserved1 := 0;
  Result.bfReserved2 := 0;
  Result.bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
end;


function TSampleConverter.GetBMPFileInfo(const pVideoInfo: TVideoFormatInfo): BITMAPINFOHEADER;
begin
  // See: https://docs.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
  Result.biSize := sizeof(BITMAPINFOHEADER);
  Result.biWidth := pVideoInfo.iVideoWidth;

  if IsTopDown(pVideoInfo.fSubType) then
    Result.biHeight := -pVideoInfo.iVideoHeight
  else
    Result.biHeight := pVideoInfo.iVideoHeight;

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


function TSampleConverter.UpdateConverter(const pManager: IMFDXGIDeviceManager;
                                          const pInputType: IMFMediaType): HResult;
begin
  Result := CreateTransform(pManager,
                            pInputType);

  NotifyBeginStreaming;
end;



function TSampleConverter.CreateTransform(const pManager: IMFDXGIDeviceManager;
                                          const pInputType: IMFMediaType): HResult;
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
                                      pInputType,
                                      0);

      // Create the output type
      if SUCCEEDED(hr) then
        hr := MFCreateMediaType(FOutputType);

      // Copy all the properties from the input type to the output
      if SUCCEEDED(hr) then
        hr := pInputType.CopyAllItems(FOutputType);

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


function TSampleConverter.ConvertSampleToRGB(const pInputSample: IMFSample;
                                             out pConvertedSample: IMFSample): HResult;
var
  hr: HResult;
  dwStatus: DWord;
  pBufferOut: IMFMediaBuffer;
  pOutputDataBuffer: MFT_OUTPUT_DATA_BUFFER;
  pOutputStreamInfo: MFT_OUTPUT_STREAM_INFO;

begin

  hr := FTransform.ProcessInput(0,
                                pInputSample,
                                0);

  if SUCCEEDED(hr) then
    begin
      hr := FTransform.GetOutputStreamInfo(0, pOutputStreamInfo);
      try
        if SUCCEEDED(hr) then
          hr := MFCreateMemoryBuffer(pOutputStreamInfo.cbSize, pBufferOut);
        if SUCCEEDED(hr) then
          hr := MFCreateSample(pConvertedSample);
        if SUCCEEDED(hr) then
          hr := pConvertedSample.AddBuffer(pBufferOut);
        if SUCCEEDED(hr)  then
          begin
            pOutputDataBuffer.dwStreamID := 0;
            pOutputDataBuffer.dwStatus := 0;
            pOutputDataBuffer.pSample := pConvertedSample;
            pOutputDataBuffer.pEvents := nil;
            hr := FTransform.ProcessOutput(0,
                                           1,
                                           @pOutputDataBuffer,
                                           dwStatus);
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

end.
