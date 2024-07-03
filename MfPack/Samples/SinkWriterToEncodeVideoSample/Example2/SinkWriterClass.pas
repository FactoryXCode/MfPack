// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
// https://github.com/FactoryXCode/MfPack
// Module: SinkWriterClass.pas
// Kind: Pascal / Delphi unit
// Release date: 25-11-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Contains an example of how to use the Sink Writer to encode video.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Renate Schaaf.
//
// ------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 18/05/2023 Renate              Fixed runtime error on selecting multiple bitmaps.
//                                Speedup of bitmap resizing using D2D1_1
// ------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPack/Samples/SinkWriterToEncodeVideoSample
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
// ==============================================================================
// Source:
//   https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// ==============================================================================
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
// ==============================================================================
unit SinkWriterClass;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.ComBaseApi,
  Winapi.WinApiTypes,
  Winapi.WinError,
  Winapi.Messages,
  Dialogs,
  {system}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Graphics,
  {ActiveX}
  Winapi.ActiveX.ObjBase,
  {MediaFoundationApi}
  Winapi.MediaFoundationApi.MfApi,
  Winapi.MediaFoundationApi.MfReadWrite,
  Winapi.MediaFoundationApi.Mfobjects,
  Winapi.MediaFoundationApi.MfUtils,
  {Application}
  Utils;

const
  WM_BITMAP_PROCESSING_MSG = WM_USER + 11;
  WM_SINKWRITER_WRITES_BITMAP = WM_USER + 12;
  FRAME_TIME_UNITS = 10000; {1 ms = 10000 100-nanosecond units}

type

  TVideoFrameBuffer = array of COLORREF;

  TSinkWriterParams = record
    pwcVideoFileName: string;
    pwcVideoFileExtension: string;
    gdEncodingFormat: TGUID;
    sEncodingFormat: string; // GuidToString(gdEncodingFormat)
    gdInputFormat: TGUID;
    rtAverageTimePerFrame: HNSTIME; // Is same as REFERENCE_TIME;
    rtFrameDuration: HNSTIME;
    uiSizeInPixels: UINT32;
    uiFrameCount: INT64;
    dwHeigth: DWORD;
    dwWidth: DWORD;
    sResolutionDescription: string;
    dbFrameRate: Double; // FPS
    dwFrameTimeUnits: HNSTIME; // in 100 nasoseconds units.
    dwBitRate: DWORD;

    FrameBufferArray: TVideoFrameBuffer; //only one buffer needed now

    procedure Init();
  end;

  TSinkWriter = class(TObject)
  private
    hwndCaller: HWND; // Usually the UI.
    // Bitmap specs
    FBitmapPixelFormat: TPixelFormat;
    FSaveResizedBitmap: Boolean;
    FArraySize: Integer;

    function InitializeSinkWriter(out ppWriter: IMFSinkWriter;
                                  out pStreamIndex: DWORD): HResult;

    function WriteFrame(pWriter: IMFSinkWriter;
                        streamIndex: DWORD;
                        const rtStart: HNSTIME { Time stamp } ): HResult;

    function SetBitmapToVideoFormat(const aBmpFile: string): HResult;

  public

    // Record that holds the settings.
    SinkWriterParams: TSinkWriterParams;

    constructor Create(hCaller: HWND);
    destructor Destroy(); override;

    function RunSinkWriter(aBmpFileList: TStringList): HResult;

    property SaveResizedBitmap: Boolean read FSaveResizedBitmap
                                        write FSaveResizedBitmap;
    property BitmapPixelFormat: TPixelFormat read FBitmapPixelFormat;
    property Items: Integer read FArraySize;

  end;

var
  FSinkWriter: TSinkWriter;


implementation


constructor TSinkWriter.Create(hCaller: HWND);
begin
  inherited Create();
  hwndCaller := hCaller;
  // Set default values
  SinkWriterParams.Init();
  FArraySize := 0;
end;


destructor TSinkWriter.Destroy();
begin
  //
  inherited Destroy();
end;


// Transforms one bitmap at a time to the video-format. After transforming we write the frame immediately
function TSinkWriter.SetBitmapToVideoFormat(const aBmpFile: string): HResult;
var
  hr: HResult;
  d: Integer;
  x, y: NativeInt;
  bmSource: TBitmap;
  ScanLine0: Pointer;
  BytesPerLine: NativeInt;
  PFmtPf32Bit: PRgbQuad;
  PRow: PByte;
  dwPxl: COLORREF;

label
  done;

begin

  SetLength(SinkWriterParams.FrameBufferArray, 0);
  bmSource := TBitmap.Create();
  // from here we alter the bitmap format to the video format.

  if not FileExists(aBmpFile) then
  begin
    hr := ERROR_CANT_RESOLVE_FILENAME;
    goto done;
  end;

  if not bmSource.Empty then
    bmSource.FreeImage;

  // Load the original bitmap from file
  bmSource.LoadFromFile(aBmpFile);

  // Check for valid pizelformat
  if (bmSource.PixelFormat <> pf32Bit) then
    bmSource.PixelFormat := pf32Bit;

  // Resize the bitmap to the desired videoframe size.
  ResizeBitmap(bmSource,
               SinkWriterParams.dwWidth,
               SinkWriterParams.dwHeigth);

  // Optional save the bitmap to file
  if FSaveResizedBitmap then
    begin
      bmSource.SaveToFile(Format('%dx%d_%s', [SinkWriterParams.dwWidth,
                                              SinkWriterParams.dwHeigth,
                                              ExtractFileName(aBmpFile)]));
    end;

  FBitmapPixelFormat := bmSource.PixelFormat;

  ScanLine0 := bmSource.ScanLine[SinkWriterParams.dwHeigth - 1];
  BytesPerLine := 4 * bmSource.Width;
  SinkWriterParams.uiSizeInPixels := (abs(BytesPerLine) * bmSource.Height);

  hr := MFCalculateImageSize(MFVideoFormat_RGB32,
                             SinkWriterParams.dwWidth,
                             SinkWriterParams.dwHeigth,
                             SinkWriterParams.uiSizeInPixels);

  // Dimension the array to calculated range.
  SetLength(SinkWriterParams.FrameBufferArray,
            SinkWriterParams.uiSizeInPixels);

  d := 0;
  // Scan and copy the rgb32bit records to COLORREF values (4 Bytes).
  PRow := ScanLine0;
  for y := (SinkWriterParams.dwHeigth - 1) downto 0 do
    begin
      PFmtPf32Bit := PRgbQuad(PRow);
      for x := 0 to SinkWriterParams.dwWidth - 1 do
        begin
          // Transform rgbtriple to COLORREF so the sinkwriter can handle this value.
          // Note: A COLORREF (DWORD) value has a length of 4 bytes, equivalent to RGBQUAD.
          CopyRgbQuadToClrRef(PFmtPf32Bit^,
                              dwPxl);

          SinkWriterParams.FrameBufferArray[d] := dwPxl;
          inc(d);
          inc(PFmtPf32Bit);
        end;
      inc(PRow, BytesPerLine);
    end;

done:
  FreeAndNil(bmSource);
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
// Some notes:
//   FPS = sec/frames
//   Number of frames = duration (seconds) * frames per second
//   Frame duration = 1 second / frames per second
//
//
function TSinkWriter.RunSinkWriter(aBmpFileList: TStringList): HResult;
var
  hr: HResult;
  i, j: Integer;
  stream: DWORD;
  pSinkWriter: IMFSinkWriter;
  rtStart: HNSTIME;

label
  done;

begin
  rtStart := 0;

  hr := CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED);

  if SUCCEEDED(hr) then
    begin
      hr := MFStartup(MF_VERSION);
      if SUCCEEDED(hr) then
        begin
          hr := InitializeSinkWriter(pSinkWriter,
                                     stream);

          if SUCCEEDED(hr) then
            begin
              // Calculate the average time/frame
              SinkWriterParams.rtAverageTimePerFrame := (SinkWriterParams.dwFrameTimeUnits) div Round(SinkWriterParams.dbFrameRate);
              // Or use this method
              //hr := MFFrameRateToAverageTimePerFrame(SinkWriterParams.dwFrameTimeUnits,
              //                                       Round(SinkWriterParams.dbFrameRate),
              //                                       SinkWriterParams.rtAverageTimePerFrame);

              SinkWriterParams.gdInputFormat := MFVideoFormat_RGB32;
              // VideoLenght is the given duration of the video in seconds.
              SinkWriterParams.uiFrameCount := aBmpFileList.Count * Round(SinkWriterParams.dbFrameRate) * SinkWriterParams.rtAverageTimePerFrame;

              // Handle the number of bitmaps in the list.
              for i := 0 to aBmpFileList.Count - 1 do
                begin
                  // Resize and store to FrameBufferArray
                  hr := SetBitmapToVideoFormat(aBmpFileList.Strings[i]);
                  if FAILED(hr) then
                    goto done;

                  // Send message to UI.
                  SendMessage(hwndCaller,
                              WM_SINKWRITER_WRITES_BITMAP,
                              WParam(0),
                              LParam(i + 1));

                 // Send frames to the sink writer.
                 for j := 0 to SinkWriterParams.uiFrameCount - 1 do
                   begin
                     hr := WriteFrame(pSinkWriter,
                                      stream,
                                      rtStart);
                     if FAILED(hr) then
                       Break;

                     inc(rtStart, SinkWriterParams.rtAverageTimePerFrame);

                     MsgWaitForMultipleObjects(0,
                                               nil^,
                                               False, // do NOT set this to true!
                                               0,
                                               QS_ALLINPUT);
                   end;
                end;
            end;
          // You must call IMFSinkWriter.BeginWriting before calling this method.
          // Otherwise, the method returns MF_E_INVALIDREQUEST.
          if SUCCEEDED(hr) then
            begin
              hr := pSinkWriter.Finalize();
            end;
    end;
    MFShutdown();
    CoUninitialize();
  end;

done:
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
function TSinkWriter.InitializeSinkWriter(out ppWriter: IMFSinkWriter;
                                          out pStreamIndex: DWORD): HResult;
var
  hr: HResult;
  pSinkWriter: IMFSinkWriter;
  pMediaTypeOut: IMFMediaType;
  pMediaTypeIn: IMFMediaType;
  streamIndex: DWORD;

begin
  SinkWriterParams.pwcVideoFileName := Format('output_%s.%s', [SinkWriterParams.sEncodingFormat,
                                                               SinkWriterParams.pwcVideoFileExtension]);

  hr := MFCreateSinkWriterFromURL(PWideChar(SinkWriterParams.pwcVideoFileName),
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
                                SinkWriterParams.gdEncodingFormat);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                                  SinkWriterParams.dwBitRate);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                                  MFVideoInterlace_Progressive);
  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeOut,
                             MF_MT_FRAME_SIZE,
                             SinkWriterParams.dwWidth,
                             SinkWriterParams.dwHeigth);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_FRAME_RATE,
                              Round(SinkWriterParams.dbFrameRate),
                              1);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              1,
                              1);
  if SUCCEEDED(hr) then
    pMediaTypeOut.SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING,
                            UINT32(True));

  // make some faster

  if SUCCEEDED(hr) then
    pMediaTypeOut.SetUINT32(MF_MT_DEFAULT_STRIDE,
                            SinkWriterParams.dwWidth * 4); {4 is the size of DWord or rgbquad in bytes}

  if SUCCEEDED(hr) then
    pMediaTypeOut.SetUINT32(MF_MT_FIXED_SIZE_SAMPLES,
                            UINT32(1));

  if SUCCEEDED(hr) then
    pMediaTypeOut.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                            UINT32(1));

  if SUCCEEDED(hr) then
    pMediaTypeOut.SetUINT32(MF_MT_SAMPLE_SIZE, {4 is the size of DWord or rgbquad in bytes}
                            SinkWriterParams.dwWidth * SinkWriterParams.dwHeigth * 4);

  // end make some faster

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
                               SinkWriterParams.gdInputFormat);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             SinkWriterParams.dwWidth,
                             SinkWriterParams.dwHeigth);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_FRAME_RATE,
                              Round(SinkWriterParams.dbFrameRate),
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
function TSinkWriter.WriteFrame(pWriter: IMFSinkWriter;
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
  done;

begin

  // Videolinewidth must correspondent with the size of RGBQuad (4 bytes) times the videowidth.
  lLineWidth := SizeOf(RGBQuad) * SinkWriterParams.dwWidth;
  dwBuffer := (lLineWidth * SinkWriterParams.dwHeigth);

  // Create a new memory buffer.
  hr := MFCreateMemoryBuffer(dwBuffer,
                             pBuffer);

  // Lock the buffer and copy the video frame to the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.Lock(pData,
                       nil,
                       nil)
  else
    goto Done;

  if SUCCEEDED(hr) then
    begin
      hr := MFCopyImage(pData { Destination buffer. } ,
                        lLineWidth { Destination stride. } ,
                        PByte(SinkWriterParams.FrameBufferArray), { First row in source image. }
                        lLineWidth { Source stride. },
                        lLineWidth { Image width in bytes. } ,
                        SinkWriterParams.dwHeigth { Image height in pixels. } );

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
    hr := pSample.SetSampleDuration(SinkWriterParams.rtAverageTimePerFrame);

  // Send the sample to the Sink Writer.
  if SUCCEEDED(hr) then
    hr := pWriter.WriteSample(streamIndex,
                              pSample);

done:
  Result := hr;
end;


procedure TSinkWriterParams.Init();
begin

  pwcVideoFileName := '';
  pwcVideoFileExtension := 'mp4';
  gdEncodingFormat := MFVideoFormat_H264;
  gdInputFormat := MFVideoFormat_RGB32;
  // The FrameRate expressed in Frames Per Second (FPS) is the number of frames per second.
  dbFrameRate := 30.0; // FPS
  dwFrameTimeUnits := FRAME_TIME_UNITS;
  dwHeigth := 480;
  dwWidth := 640;
  // Duration per sample, also known as Frame Duration, in 100-nanosecond units .
  rtFrameDuration := Round((60 * FRAME_TIME_UNITS) / dbFrameRate);
  rtAverageTimePerFrame := Round(dwFrameTimeUnits / dbFrameRate);
  uiSizeInPixels := dwWidth * dwHeigth;
  uiFrameCount := 20 * Round(dbFrameRate);
  sResolutionDescription := 'SD    480p  (640 x 480)';
  dwBitRate := 800000;

  SetLength(FrameBufferArray, 0);
end;

end.
