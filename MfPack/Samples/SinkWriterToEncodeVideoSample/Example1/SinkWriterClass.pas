// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SinkWriterClass.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.1.7
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
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX317
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
unit SinkWriterClass;

interface

uses
  {Winapi}
  Winapi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  {system}
  System.Classes,
  System.SysUtils,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.Mfobjects;

const
  // Format constants
  VIDEO_WIDTH  = 640;
  VIDEO_HEIGHT = 480;
  VIDEO_FPS = 30;
  VIDEO_BIT_RATE = 800000;

type

  TSampleSinkWriter = class(TObject)
  private
    VIDEO_FRAME_DURATION: UINT64;
    VIDEO_ENCODING_FORMAT: TGUID;
    VIDEO_INPUT_FORMAT: TGUID;
    VIDEO_PELS: UINT32;
    VIDEO_FRAME_COUNT: UINT32;
    videoFrameBuffer: array of DWORD;


    function InitializeSinkWriter(const sExt: string;
                                  const sEncFormat: string;
                                  out ppWriter: IMFSinkWriter;
                                  out pStreamIndex: DWORD): HResult;

    function WriteFrame(pWriter: IMFSinkWriter;
                        streamIndex: DWORD;
                        const rtStart: HNSTIME {Time stamp}): HResult;

  public

    constructor Create();
    destructor Destroy(); override;

    function RunSinkWriter(sExt: string;
                           sEncFormat: string;
                           const gEncodingFormat: TGuid): HResult;

  end;

  var
    FSampleSinkWriter: TSampleSinkWriter;


implementation


constructor TSampleSinkWriter.Create();
begin
  inherited Create();

end;


destructor TSampleSinkWriter.Destroy();
begin

  inherited Destroy();
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
                                         const gEncodingFormat: TGuid): HResult;
var
  hr: HResult;
  i: DWord;
  stream: DWORD;
  pSinkWriter: IMFSinkWriter;
  rtStart: HNSTIME;

begin
  VIDEO_FRAME_DURATION := 10 * 1000 * 1000 div VIDEO_FPS;
  VIDEO_ENCODING_FORMAT := gEncodingFormat;
  VIDEO_INPUT_FORMAT := MFVideoFormat_RGB32;
  VIDEO_PELS := VIDEO_WIDTH * VIDEO_HEIGHT;
  VIDEO_FRAME_COUNT := 20 * VIDEO_FPS;

  // Buffer to hold the video frame data.
  SetLength(videoFrameBuffer,
            VIDEO_PELS);

  rtStart := 0;

  // Set all pixels to green
  for i := 0 to VIDEO_PELS -1 do
    videoFrameBuffer[i] := $0000FF00;

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
                for i := 0 to VIDEO_FRAME_COUNT -1 do
                  begin
                    hr := WriteFrame(pSinkWriter,
                                     stream,
                                     rtStart);
                    if FAILED(hr) then
                      Break;

                    inc(rtStart,
                        VIDEO_FRAME_DURATION);
                  end;
            end;
          if SUCCEEDED(hr) then
            hr := pSinkWriter.Finalize();
        end;
      MFShutdown();
      CoUninitialize();
    end;
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


  hr := MFCreateSinkWriterFromURL(PWideChar(Format('output_%s.%s', [sEncFormat, sExt])),
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
                                VIDEO_ENCODING_FORMAT);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_AVG_BITRATE,
                                  VIDEO_BIT_RATE);

  if SUCCEEDED(hr) then
    hr := pMediaTypeOut.SetUINT32(MF_MT_INTERLACE_MODE,
                                  MFVideoInterlace_Progressive);
  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeOut,
                             MF_MT_FRAME_SIZE,
                             VIDEO_WIDTH,
                             VIDEO_HEIGHT);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeOut,
                              MF_MT_FRAME_RATE,
                              VIDEO_FPS,
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
                               VIDEO_INPUT_FORMAT);

  if SUCCEEDED(hr) then
    hr := pMediaTypeIn.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pMediaTypeIn,
                             MF_MT_FRAME_SIZE,
                             VIDEO_WIDTH,
                             VIDEO_HEIGHT);

  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pMediaTypeIn,
                              MF_MT_FRAME_RATE,
                              VIDEO_FPS,
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
  cbWidth: LONG;
  cbBuffer: DWORD;

begin
  cbWidth := 4 * VIDEO_WIDTH;
  cbBuffer := DWord(cbWidth * VIDEO_HEIGHT);

  // Create a new memory buffer.
  hr := MFCreateMemoryBuffer(cbBuffer,
                             pBuffer);

  // Lock the buffer and copy the video frame to the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.Lock(pData,
                       nil,
                       nil);

  if SUCCEEDED(hr) then
    begin
        hr := MFCopyImage(pData,                      // Destination buffer.
                          cbWidth,                    // Destination stride.
                          PByte(videoFrameBuffer),    // First row in source image.
                          cbWidth,                    // Source stride.
                          cbWidth,                    // Image width in bytes.
                          VIDEO_HEIGHT                // Image height in pixels.
                         );
     end;

  if Assigned(pBuffer) then
    pBuffer.Unlock();


  // Set the data length of the buffer.
  if SUCCEEDED(hr) then
    hr := pBuffer.SetCurrentLength(cbBuffer);


  // Create a media sample and add the buffer to the sample.
  if SUCCEEDED(hr) then
    hr := MFCreateSample(pSample);

  if SUCCEEDED(hr) then
    hr := pSample.AddBuffer(pBuffer);

  // Set the time stamp and the duration.
  if SUCCEEDED(hr) then
    hr := pSample.SetSampleTime(rtStart);

  if SUCCEEDED(hr) then
    hr := pSample.SetSampleDuration(VIDEO_FRAME_DURATION);

  // Send the sample to the Sink Writer.
  if SUCCEEDED(hr) then
    hr := pWriter.WriteSample(streamIndex,
                              pSample);

  Result := hr;
end;

end.
