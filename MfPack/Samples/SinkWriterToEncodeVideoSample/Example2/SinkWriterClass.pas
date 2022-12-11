// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SinkWriterClass.pas
// Kind: Pascal / Delphi unit
// Release date: 25-11-2022
// Language: ENU
//
// Revision Version: 3.1.3
// Description: Contains an example of how to use the Sink Writer to encode video.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Explanatory memorandum:
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit SinkWriterClass;

interface

uses
  {Winapi}
  Winapi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {system}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Graphics,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.Mfobjects,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  Utils;


type

  TSampleSinkWriter = class
  private

    // Bitmap specs
    FBitmapPixelFormat: TPixelFormat;

    // Settings

    FVideoFrameDuration: UINT64;
    FVideoDuration: UINT64;
    FVideoEncodingFormat: TGUID;
    FVideoInputFormat: TGUID;
    FVideoPixels: UINT32;
    FVideoFrameCount: UINT32;
    FVideoHeigth: DWORD;
    FVideoWidth: DWORD;
    FVideoFps: UINT32;
    FVideoBitRate: UINT32;
    FVideoFrameBuffer: array of COLORREF;
    FSaveResizedBitmap: Boolean;
    FVideoFileName: string;

    // Setters
    procedure SetVideoFps(aValue: UINT32);
    procedure SetVideoBitRate(aValue: UINT32);
    procedure SetVideoEncodingFormat(aValue: TGUID);
    procedure SetVideoInputFormat(aValue: TGUID);
    procedure SetVideoPixels(aValue: UINT32);
    procedure SetVideoFrameCount(aValue: UINT32);
    procedure SetVideoFrameDuration(aValue: UINT64);
    procedure SetVideoDuration(aValue: UINT64);

    function InitializeSinkWriter(const sExt: string;
                                  const sEncFormat: string;
                                  out ppWriter: IMFSinkWriter;
                                  out pStreamIndex: DWORD): HResult;

    function WriteFrame(pWriter: IMFSinkWriter;
                        streamIndex: DWORD;
                        const rtStart: HNSTIME {Time stamp}): HResult;

    function SetBitmapToVideoFormat(aBmpFileName: string): HResult;

  public

    constructor Create();
    destructor Destroy(); override;

    function RunSinkWriter(sExt: string;
                           sEncFormat: string;
                           const gEncodingFormat: TGuid;
                           aBmpFileName: string;
                           aVideoLenght: UINT64 = 1;
                           aVideoWidth: DWord = 640;
                           aVideoHeigth: DWord = 480): HResult;


    property VideoHeight: UINT32 read FVideoHeigth;
    property VideoWidth: UINT32 read FVideoWidth;
    property VideoFps: UINT32 read FVideoFps write SetVideoFps;
    property VideoDuration: UINT64 read FVideoDuration write SetVideoDuration;
    property VideoBitRate: UINT32 read FVideoBitRate write SetVideoBitRate;
    property VideoEncodingFormat: TGUID read FVideoEncodingFormat write SetVideoEncodingFormat;
    property VideoInputFormat: TGUID read FVideoInputFormat write SetVideoInputFormat;
    property VideoDimensions: UINT32 read FVideoPixels write SetVideoPixels;
    property VideoFrameCount: UINT32 read FVideoFrameCount write SetVideoFrameCount;
    property VideoFrameDuration: UINT64 read FVideoFrameDuration write SetVideoFrameDuration;
    property SaveResizedBitmap: Boolean read FSaveResizedBitmap write FSaveResizedBitmap;
    property VideoFileName: string read FVideoFileName;

    // By default these properties are:
    // FVideoFrameDuration = {10 * 1000 * 1000} 10000000 div 30
    // FVideoFrameCount = 20 * 30
    // FVideoEncodingFormat = MFVideoFormat_H264
    // FVideoInputFormat = MFVideoFormat_RGB32
    // FVideoPixels = 640 * 480
    // FFileExtension = MP4

  end;

  var
    FSampleSinkWriter: TSampleSinkWriter;


implementation


// Helpers /////////////////////////////////////////////////////////////////////

procedure TSampleSinkWriter.SetVideoFps(aValue: UINT32);
begin
  FVideoFps := aValue;
end;

procedure TSampleSinkWriter.SetVideoBitRate(aValue: UINT32);
begin
  FVideoBitRate := aValue;
end;

procedure TSampleSinkWriter.SetVideoEncodingFormat(aValue: TGUID);
begin
  FVideoEncodingFormat := aValue;
end;

procedure TSampleSinkWriter.SetVideoInputFormat(aValue: TGUID);
begin
  FVideoInputFormat := aValue;
end;

procedure TSampleSinkWriter.SetVideoPixels(aValue: UINT32);
begin
  FVideoPixels := aValue;
end;

procedure TSampleSinkWriter.SetVideoFrameCount(aValue: UINT32);
begin
  FVideoFrameCount := aValue;
end;

procedure TSampleSinkWriter.SetVideoFrameDuration(aValue: UINT64);
begin
  FVideoFrameDuration := aValue;
end;

procedure TSampleSinkWriter.SetVideoDuration(aValue: UINT64);
begin
  FVideoDuration := aValue;
end;


////////////////////////////////////////////////////////////////////////////////


constructor TSampleSinkWriter.Create();
begin
  inherited Create();
  // Set some default values
  FVideoFps := 30;
  FVideoBitRate := 800000;
  FVideoHeigth := 480;
  FVideoWidth := 640;
end;


destructor TSampleSinkWriter.Destroy();
begin

  inherited Destroy();
end;


function TSampleSinkWriter.SetBitmapToVideoFormat(aBmpFileName: string): HResult;
var
  hr: HResult;
  d: Integer;
  x, y: NativeInt;
  bmSource: TBitmap;
  BytesPerPixel: NativeInt;
  ScanLine0: Pointer;
  BytesPerLine: NativeInt;
  FmtPf24Bit: TRgbTriple;
  PFmtPf24Bit: PRgbTriple;
  dwPxl: COLORREF;

label
  Done;

begin
  if not FileExists(aBmpFileName) then
    begin
      hr := ERROR_CANT_RESOLVE_FILENAME;
      goto Done;
    end;

  bmSource := TBitmap.Create();
  // Load the original bitmap from file
  bmSource.LoadFromFile(aBmpFileName);

  // Vheck for valid pizelformat
  if (bmSource.PixelFormat <> pf24Bit) then
    begin
      hr := ERROR_NOT_SUPPORTED;
      goto Done;
    end;

  // Resize the bitmap to the desired videoframe size.
  ResizeBitmap(bmSource,
               FVideoWidth,
               FVideoHeigth);

  // Optional save the bitmap to file
  if FSaveResizedBitmap then
    begin
      bmSource.SaveToFile(Format('%dx%d_%s',[FVideoWidth, FVideoHeigth, ExtractFileName(aBmpFileName)]));
    end;

  // Check if the bitmap is 24 bit (for example Paint3D can only store 24bit format bitmaps)
  FBitmapPixelFormat := bmSource.PixelFormat;

  BytesPerPixel := SizeOf(FmtPf24Bit);
  ScanLine0 := bmSource.ScanLine[0];
  BytesPerLine := NativeInt(bmSource.ScanLine[1]) - NativeInt(ScanLine0);
  FVideoPixels := (abs(BytesPerLine) * bmSource.Height);
  // Dimension the array to calculated range.
  SetLength(FVideoFrameBuffer,
            FVideoPixels);

  d := 0;
  // Scan and copy the rgb24bit records to COLORREF values (4 Bytes).
  for y := (FVideoHeigth - 1) downto 0 do
    begin
      for x := 0 to FVideoWidth - 1 do
        begin
          PFmtPf24Bit := SetPointer(ScanLine0,
                                   (BytesPerLine * y) + (x * BytesPerPixel));
          FmtPf24Bit := PFmtPf24Bit^;

          // Transform rgbtriple to COLORREF.
          CopyRgbTripleToClrRef(FmtPf24Bit,
                                dwPxl);

          FVideoFrameBuffer[d] := dwPxl;
          inc(d);
        end;
    end;

Done:
  SafeDelete(bmSource);
  Result := hr;
end;



// Inside this function, the following steps will be performed.
//
// 1 Call CoInitializeEx to initialize the COM library.
// 2 Call MFStartup to initialize Microsoft Media Foundation.
// 3 Create the sink writer.
// 4 Send video frames to the sink writer.
// 5 Call IMFSinkWriter.Finalize to finalize the output file.
// 6 You don't have to Release the pointer to the sink writer. The compiler is doing that automaticly.
// 7 Call MFShutdown.
// 8 Call CoUninitialize.
//
function TSampleSinkWriter.RunSinkWriter(sExt: string;
                                         sEncFormat: string;
                                         const gEncodingFormat: TGuid;
                                         aBmpFileName: string;
                                         aVideoLenght: UINT64 = 1;
                                         aVideoWidth: DWord = 640;
                                         aVideoHeigth: DWord = 480): HResult;
var
  hr: HResult;
  i: Integer;
  stream: DWORD;
  pSinkWriter: IMFSinkWriter;
  rtStart: HNSTIME;

label
  Done;

begin
  FVideoFrameDuration := 10 * 1000 * 1000 div FVideoFps;
  FVideoEncodingFormat := gEncodingFormat;
  FVideoInputFormat := MFVideoFormat_RGB32;
  // aVideoLenght is the given duration of the video in seconds.
  FVideoFrameCount := aVideoLenght * FVideoFps;
  FVideoWidth := aVideoWidth;
  FVideoHeigth := aVideoHeigth;

  hr := SetBitmapToVideoFormat(aBmpFileName);

  if FAILED(hr) then
    goto Done;

  rtStart := 0;

  hr := CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED);

  if SUCCEEDED(hr) then
    begin
      hr := MFStartup(MF_VERSION);
      if SUCCEEDED(hr) then
        begin
          hr := InitializeSinkWriter(sExt,
                                     sEncFormat,
                                     pSinkWriter,
                                     stream);
          if SUCCEEDED(hr) then
            begin
                // Send frames to the sink writer.
                for i := 0 to FVideoFrameCount -1 do
                  begin
                    hr := WriteFrame(pSinkWriter,
                                     stream,
                                     rtStart);
                    if FAILED(hr) then
                      Break;

                    inc(rtStart,
                        FVideoFrameDuration);
                  end;
            end;
          if SUCCEEDED(hr) then
            hr := pSinkWriter.Finalize();
        end;
      MFShutdown();
      CoUninitialize();
    end;

Done:
  Result := hr;
end;


// Initialize the Sink Writer
// To initialize the sink writer, perform the following steps.
//
// 1 Call MFCreateSinkWriterFromURL to create a new instance of the sink writer.
// 2 Create a media type that describes the encoded video.
// 3 Pass this media type to the IMFSinkWriter.AddStream method.
// 4 Create a second media type that describes the uncompressed input.
// 5  Pass the uncompressed media type to the IMFSinkWriter.SetInputMediaType method.
// 6 Call the IMFSinkWriter.BeginWriting method.
// 7 The sink writer is now ready to accept input samples.
// The following function shows these steps.
//
function TSampleSinkWriter.InitializeSinkWriter(const sExt: string;
                                                const sEncFormat: string;
                                                out ppWriter: IMFSinkWriter;
                                                out pStreamIndex: DWORD): HResult;
var
  hr: HResult;
  pSinkWriter: IMFSinkWriter;
  pMediaTypeOut: IMFMediaType;
  pMediaTypeIn: IMFMediaType;
  streamIndex: DWORD;

begin
  FVideoFileName := Format('output_%s.%s', [sEncFormat, sExt]);

  hr := MFCreateSinkWriterFromURL(PWideChar(FVideoFileName),
                                  nil,
                                  nil,
                                  pSinkWriter);

  // Set the output media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeOut);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetGUID(MF_MT_SUBTYPE,
                                FVideoEncodingFormat);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                                  FVideoBitRate);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                                  MFVideoInterlace_Progressive);
  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeOut,
                             MF_MT_FRAME_SIZE,
                             FVideoWidth,
                             FVideoHeigth);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_FRAME_RATE,
                              FVideoFps,
                              1);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.AddStream(pMediaTypeOut,
                                streamIndex);

  // Set the input media type.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaType(pMediaTypeIn);


  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetGUID(MF_MT_SUBTYPE,
                               FVideoInputFormat);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             FVideoWidth,
                             FVideoHeigth);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_FRAME_RATE,
                              FVideoFps,
                              1);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);

  if SUCCEEDED(hr) then
    hr := pSinkWriter.SetInputMediaType(streamIndex,
                                        pMediaTypeIn,
                                        nil);


  // Tell the sink writer to start accepting data.
  if SUCCEEDED(hr) then
    hr := pSinkWriter.BeginWriting();

  // Return the pointer to the caller.
  if SUCCEEDED(hr) then
    begin
       ppWriter := pSinkWriter;
       pStreamIndex := streamIndex;
    end;

  Result := hr;
end;


// This code performs the following steps.
//
// 1 Call MFCreateMemoryBuffer to create a media buffer object. This function allocates the memory for the buffer.
//
// 2 Call IMFMediaBuffer.Lock to lock the buffer and get a pointer to the memory.
//
// 3 Call MFCopyImage to copy the video frame into the buffer.
//
// Note
//
// In this particular example, using memcpy would work just as well.
// However, the MFCopyImage function correctly handles the case where the
// stride of the source image does not match the target buffer.
// For more information, see https://learn.microsoft.com/en-us/windows/win32/medfound/image-stride.
//
//
// 4 Call IMFMediaBuffer.Unlock to unlock the buffer.
//
// 5 Call IMFMediaBuffer.SetCurrentLength to update the length of the valid data in the buffer. (Otherwise, this value defaults to zero.)
//
// 6 Call MFCreateSample to create a media sample object.
//
// 7 Call IMFSample.AddBuffer to add the media buffer to the media sample.
//
// 8 Call IMFSample.SetSampleTime to set the time stamp for the video frame.
//
// 9 Call IMFSample.SetSampleDuration to set the duration of the video frame.
//
// 10 Call IMFSinkWriter.WriteSample to send the media sample to the sink writer.
//
function TSampleSinkWriter.WriteFrame(pWriter: IMFSinkWriter;
                                      streamIndex: DWORD;
                                      const rtStart: HNSTIME): HResult;
var
  hr: HResult;
  pSample: IMFSample;
  pBuffer: IMFMediaBuffer;
  pData: PByte;
  lLineWidth: DWORD;
  dwBuffer: DWORD;

label
  Done;

begin
  // Videolinewidth must correspondent with the size of RGBQuad (4 bytes) times the videowidth.
  lLineWidth := SizeOf(RGBQuad) * FVideoWidth;
  dwBuffer := (lLineWidth * FVideoHeigth);

  // Create a new memory buffer.
  hr := MFCreateMemoryBuffer(dwBuffer,
                             pBuffer);

  // Lock the buffer and copy the video frame to the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.Lock(pData,
                       nil,
                       nil);

  if SUCCEEDED(hr) then
    begin

      hr := MFCopyImage(pData {Destination buffer.},
                        lLineWidth {Destination stride.},
                        PByte(FVideoFrameBuffer) {First row in source image.},
                        lLineWidth {Source stride.},
                        lLineWidth {Image width in bytes.},
                        FVideoHeigth {Image height in pixels.})
     end;

  if Assigned(pBuffer) then
    pBuffer.Unlock();

  // Set the data length of the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.SetCurrentLength(dwBuffer);

  // Create a media sample and add the buffer to the sample.
  if SUCCEEDED(hr) then
    hr := MFCreateSample(pSample);

  if SUCCEEDED(hr) then
    hr := pSample.AddBuffer(pBuffer);

  // Set the time stamp and the duration.
  if SUCCEEDED(hr) then
    hr := pSample.SetSampleTime(rtStart);

  if SUCCEEDED(hr) then
    hr := pSample.SetSampleDuration(FVideoFrameDuration);

  // Send the sample to the Sink Writer.
  if SUCCEEDED(hr) then
    hr := pWriter.WriteSample(streamIndex,
                              pSample);

Done:
  Result := hr;
end;


end.

