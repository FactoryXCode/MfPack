// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SampleConverter.pas
// Kind: Pascal Unit
// Release date: 18-04-2022
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
// 18/04/2022 Tony                Modified source for MFCaptureEngineVideoCapture
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
unit SampleConverter;

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

    procedure Reset;
  end;


type
  TSampleConverter = class(TPersistent)
  protected
    FOutputType: IMFMediaType;
    FTransform: IMFTransform;
    FSupportedInputs: TArray<TGUID>;

  private
    function ConvertSampleToRGB(const AInputSample: IMFSample;
                                out AConvertedSample: IMFSample): HResult;

    function IndexOf(const AInput: TGUID;
                     const AValues: array of TGUID): Integer;

    procedure GetBMPFileHeader(out pBmpFileHeader: BITMAPFILEHEADER);
    procedure GetBMPFileInfo(const AVideoInfo: TVideoFormatInfo;
                             out bmpFileHeader: BITMAPINFOHEADER);

    procedure FreeConverter();
    procedure NotifyBeginStreaming();
    procedure SetSupportedInputs();

  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateConverter(const AInputType: IMFMediaType): HResult;
    function DataFromSample(const ASample: IMFSample;
                            const AVideoInfo: TVideoFormatInfo;
                            out AMemoryStream: TMemoryStream): HResult;

    function IsInputSupported(const AInputFormat: TGUID): Integer;

  end;


implementation


constructor TSampleConverter.Create();
begin
  inherited;
  SetSupportedInputs;
end;


destructor TSampleConverter.Destroy();
begin
  FreeConverter();
  inherited;
end;


procedure TSampleConverter.SetSupportedInputs();
begin
  SetLength(FSupportedInputs, 20);
  FSupportedInputs[0] := MFVideoFormat_RGB24;
  FSupportedInputs[1] := MFVideoFormat_RGB32;
  FSupportedInputs[2] := MFVideoFormat_RGB555;
  FSupportedInputs[3] := MFVideoFormat_RGB565;
  FSupportedInputs[4] := MFVideoFormat_RGB8;
  FSupportedInputs[5] := MFVideoFormat_AYUV;
  FSupportedInputs[6] := MFVideoFormat_I420;
  FSupportedInputs[7] := MFVideoFormat_IYUV;
  FSupportedInputs[8] := MFVideoFormat_NV11;
  FSupportedInputs[9] := MFVideoFormat_NV12;
  FSupportedInputs[10] := MFVideoFormat_UYVY;
  FSupportedInputs[11] := MFVideoFormat_V216;
  FSupportedInputs[12] := MFVideoFormat_V410;
  FSupportedInputs[13] := MFVideoFormat_Y41P;
  FSupportedInputs[14] := MFVideoFormat_Y41T;
  FSupportedInputs[15] := MFVideoFormat_Y42T;
  FSupportedInputs[16] := MFVideoFormat_YUY2;
  FSupportedInputs[17] := MFVideoFormat_YV12;
  FSupportedInputs[18] := MFVideoFormat_YVU9;
  FSupportedInputs[19] := MFVideoFormat_YVYU;
end;


function TSampleConverter.IsInputSupported(const AInputFormat: TGUID): Integer;
begin
  Result := Integer(IndexOf(AInputFormat,
                    FSupportedInputs) > -1);
end;


function TSampleConverter.IndexOf(const AInput: TGUID;
                                  const AValues: array of TGUID): Integer;
begin
  Result := high(AValues);
  while (Result >= low(AValues)) and (AInput <> AValues[Result]) do
    Dec(Result);
end;


function TSampleConverter.DataFromSample(const ASample: IMFSample;
                                         const AVideoInfo: TVideoFormatInfo;
                                         out AMemoryStream: TMemoryStream): HResult;
var
  pBuffer: IMFMediaBuffer;
  pConvertedSample: IMFSample;
  pBitmapData: PByte;
  cbBitmapData: DWord;
  iActualDataSize: Integer;
  iExpectedDataSize: Integer;
  fBmpFileHeader: BITMAPFILEHEADER;
  fBmpFileInfo: BITMAPINFOHEADER;
  hr: HResult;

begin

  if AVideoInfo.fSubType <> MFVideoFormat_RGB32 then
    begin
      hr := ConvertSampleToRGB(ASample,
                                   pConvertedSample);

      if SUCCEEDED(hr) then
        // Converts a sample with multiple buffers into a sample with a single buffer.
        hr := pConvertedSample.ConvertToContiguousBuffer(pBuffer);
    end
  else
    // Converts a sample with multiple buffers into a sample with a single buffer.
    hr := ASample.ConvertToContiguousBuffer(pBuffer);

  if SUCCEEDED(hr) then
    begin
      hr := pBuffer.Lock(pBitmapData,
                         Nil,
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
                OutputDebugString(Format('Sample size does not match expected size. Current: %d. Expected: %d',
                                         [iActualDataSize, iExpectedDataSize]));
                {$ENDIF}

              end;

            AMemoryStream := TMemoryStream.Create;

           // fBmpFileHeader := GetBMPFileHeader;
          //  fBmpFileInfo := GetBMPFileInfo(AVideoInfo);

            AMemoryStream.Write(fBmpFileHeader,
                                SizeOf(fBmpFileHeader));
            AMemoryStream.Write(fBmpFileInfo,
                                SizeOf(fBmpFileInfo));
            AMemoryStream.Write(pBitmapData[0],
                                iActualDataSize);
          end;
      finally
        pBuffer.Unlock;
        SafeRelease(pBuffer);
      end;
  end;

  if Assigned(pConvertedSample) then
    SafeRelease(pConvertedSample);
end;


procedure TSampleConverter.GetBMPFileHeader(out pBmpFileHeader: BITMAPFILEHEADER);
begin
  pBmpFileHeader.bfType := Ord('B') or (Ord('M') shl 8); // Type is "BM" for BitMap
  pBmpFileHeader.bfSize := sizeof(pBmpFileHeader.bfOffBits) + sizeof(RGBTRIPLE);
  pBmpFileHeader.bfReserved1 := 0;
  pBmpFileHeader.bfReserved2 := 0;
  pBmpFileHeader.bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
end;


procedure TSampleConverter.GetBMPFileInfo(const AVideoInfo: TVideoFormatInfo;
                                          out bmpFileHeader: BITMAPINFOHEADER);
begin
  bmpFileHeader.biSize := sizeof(BITMAPINFOHEADER);
  bmpFileHeader.biWidth := AVideoInfo.iVideoWidth;
  // See: https://docs.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
  bmpFileHeader.biHeight       := -AVideoInfo.iVideoHeight;
  bmpFileHeader.biPlanes       := 1;
  bmpFileHeader.biBitCount     := 32;
  bmpFileHeader.biCompression  := BI_RGB;
  bmpFileHeader.biClrImportant := 0;
  bmpFileHeader.biClrUsed      := 0;
end;


procedure TSampleConverter.FreeConverter();
begin
  if Assigned(FTransform) then
    SafeRelease(FTransform);
  FOutputType := nil;
end;


function TSampleConverter.UpdateConverter(const AInputType: IMFMediaType): HResult;
var
  hr: HResult;

begin
  FreeConverter;

  // Create the color converter
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/colorconverter
  hr := CoCreateInstance(CLSID_CColorConvertDMO,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IID_IMFTransform,
                         FTransform);
  // or in Delphi style
  // FTransform := System.Win.ComObj.CreateCOMObject(CLSID_CColorConvertDMO) as IMFTransform;

  if SUCCEEDED(hr) then
    begin
      // Set the input type for the transform
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

      NotifyBeginStreaming();
  end;
end;


procedure TSampleConverter.NotifyBeginStreaming;
begin
  // This should speed up the first frame request.
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/mft-message-notify-begin-streaming
  FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                            0);
end;


function TSampleConverter.ConvertSampleToRGB(const AInputSample: IMFSample;
                                             out AConvertedSample: IMFSample): HResult;
var
  oStatus: DWord;
  oResult: HResult;
  pBufferOut: IMFMediaBuffer;
  oOutputDataBuffer: MFT_OUTPUT_DATA_BUFFER;
  oOutputStreamInfo: MFT_OUTPUT_STREAM_INFO;
  iConvertStart: int64;
  iConvertEnd: int64;
  hr: HResult;

begin

  hr := FTransform.ProcessInput(0,
                                AInputSample,
                                0);

  if SUCCEEDED(hr) then
    begin
      hr := FTransform.GetOutputStreamInfo(0,
                                           oOutputStreamInfo);
      if SUCCEEDED(hr) then
        begin
        try
          hr := MFCreateMemoryBuffer(oOutputStreamInfo.cbSize,
                                     pBufferOut);

          if SUCCEEDED(hr) then
            hr := MFCreateSample(AConvertedSample);

          if SUCCEEDED(hr) then
            hr := AConvertedSample.AddBuffer(pBufferOut);

          if SUCCEEDED(hr) then
            begin
              oOutputDataBuffer.dwStreamID := 0;
              oOutputDataBuffer.dwStatus := 0;
              oOutputDataBuffer.pSample := AConvertedSample;
              oOutputDataBuffer.pEvents := nil;

              hr := FTransform.ProcessOutput(MFT_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER,
                                             1,
                                             @oOutputDataBuffer,
                                             oStatus);
            end;
        finally
          SafeRelease(pBufferOut);
        end;
        end;
    end;
end;


{ TVideoFormatInfo }
procedure TVideoFormatInfo.Reset;
begin
  iVideoWidth := 0;
  iVideoHeight := 0;
  iBufferWidth := 0;
  iBufferHeight := 0;
end;

end.
