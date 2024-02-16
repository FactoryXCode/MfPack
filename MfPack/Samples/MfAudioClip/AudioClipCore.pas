// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
// Module:  AudioClipCore.pas
// Kind: Pascal Unit
// Release date: 21-11-2019
// Language: ENU
//
// Revision Version: 3.1.6
// Description:
//   This application demonstrates using the Media Foundation
//   source reader to extract decoded audio from an audio/video file.
//
//   The application reads audio data from an input file and writes
//   uncompressed PCM audio to a WAVE file.
//
//   The input file must be a media format supported by Media Foundation,
//   and must have  an audio stream. The audio stream can be an encoded
//   format, such as Windows Media Audio.
//   Note: The original application is a console app.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: AudioClip sample
//         https://docs.microsoft.com/en-us/windows/win32/medfound/tutorial--decoding-audio
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
unit AudioClipCore;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.WinMM.MMReg,
  {System}
  System.SysUtils,
  System.Math,
  {Vcl}
  Vcl.Forms,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfError,
  {ActiveX}
  WinApi.ActiveX.ObjBase;


const
  // The minimum OS support
  WINVER                              = _WIN32_WINNT_WIN7;
  // By default the maximum filesize of a wavefile can't be larger than 4294967295 bytes (approx. 4 GB)
  MAX_WAV_FILESIZE                    = MAXDWORD - (44 + SizeOf(WaveformatEx));

var
  bFlush:  Boolean;  // When user want's to abort, this flag will be true.
                     // The reason for this approach is this sample works in sync state,
                     // so the respond is tricky.

  function WriteWaveFile(pReader: IMFSourceReader;      // Pointer to the source reader.
                         const hFile: THandle;          // Handle to the output file.
                         msecAudioData: LONG): HResult;

  function ConfigureAudioStream(pReader: IMFSourceReader;    // Pointer to the source reader.
                                out ppPCMAudio: IMFMediaType): HResult;

  function WriteWaveHeader(const hFile: THandle;        // Output file.
                           pMediaType: IMFMediaType;    // PCM audio format.
                           out pcbWritten: DWORD): HResult;

  function CalculateMaxAudioDataSize(pAudioType: IMFMediaType;     // The PCM audio format.
                                     cbHeader: DWORD;              // The size of the WAVE file header.
                                     msecAudioData: DWORD): DWord;

  function WriteWaveData(const hFile: THandle;        // Output file.
                         pReader: IMFSourceReader;    // Source reader.
                         cbMaxAudioData: DWORD;       // Maximum amount of audio data (bytes).
                         out pcbDataWritten: DWORD): HResult;

  function FixUpChunkSizes(const hFile: THandle;          // Output file.
                           cbHeader: DWORD;               // Size of the 'fmt ' chuck.
                           cbAudioData: DWORD): HResult;

  function WriteToFile(const hFile: THandle;
                       const p;
                       cb: DWORD): HResult;

  procedure FlushSourceReader(doFlush: Boolean);

  function ExtractSound(const wHandle: HWnd;
                        const wszSourceFile: PWideChar;     // the sourcefile URL
                        const wszTargetFile: PWideChar;     // The target file URL
                        msDuration: Int64): HResult;        // The duration in ms to extract.


implementation


function ExtractSound(const wHandle: HWnd;
                      const wszSourceFile: PWideChar;     // the sourcefile URL
                      const wszTargetFile: PWideChar;     // The target file URL
                      msDuration: Int64): HResult;        // The duration in ms to extract.
var
  hr: HResult;
  pReader: IMFSourceReader;
  hFile: THandle;  // The file handle

begin

  if not FileExists(wszSourceFile) then
    begin
      Result := ERROR_FILE_NOT_FOUND;
      Exit;
    end;

  if (wszTargetFile = '') or (ExtractFileExt(WideCharToString(wszTargetFile)) <> '.wav') then
    begin
      Result := ERROR_FILE_NOT_FOUND;
      Exit;
    end;

  bFlush := False;
  hFile := INVALID_HANDLE_VALUE;

  // Initialize the COM library.
  hr := CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

  // Intialize the Media Foundation platform.
  if SUCCEEDED(hr) then
    hr := MFStartup(MF_VERSION);

  // Create the source reader to read the input file.
  if SUCCEEDED(hr) then
    begin
      hr := MFCreateSourceReaderFromURL(wszSourceFile,
                                        nil,
                                        pReader);

      if FAILED(hr) then
        begin
          Result := hr;
          //  printf("Error opening input file: %S\n", wszSourceFile, hr);
          Exit;
        end;
    end;

  // Open the output file for writing.
  if SUCCEEDED(hr) then
    begin
      hFile := CreateFile(wszTargetFile,
                          GENERIC_WRITE,
                          FILE_SHARE_READ,
                          nil,
                          CREATE_ALWAYS,
                          0,
                          0);

      if (hFile = INVALID_HANDLE_VALUE) then
        begin
          Result := HRESULT_FROM_WIN32(GetLastError());
          //  printf("Cannot create output file: %S\n", wszTargetFile, hr);
          Exit;
        end;
    end;

  // Write the WAVE file.
  if SUCCEEDED(hr) then
    hr := WriteWaveFile(pReader,
                        hFile,
                        msDuration);

  if FAILED(hr) then
    begin
      //  printf("Failed, hr = 0x%X\n", hr);
      Result := hr;
      Exit;
    end;

  // Clean up.
  if (hFile <> INVALID_HANDLE_VALUE) then
    CloseHandle(hFile);

  Result := hr;
end;



//-------------------------------------------------------------------
// WriteWaveFile
//
// Writes a WAVE file by getting audio data from the source reader.
//
//-------------------------------------------------------------------
function WriteWaveFile(pReader: IMFSourceReader;    // Pointer to the source reader.
                       const hFile: THandle;                // Handle to the output file.
                       msecAudioData: LONG): HResult;
var
  hr: HResult;
  cbHeader: DWORD;
  cbAudioData: DWORD;
  cbMaxAudioData: DWORD;
  pAudioType: IMFMediaType;  // Represents the PCM audio format.

begin

  cbHeader := 0;         // Size of the WAVE file header, in bytes.
  cbAudioData := 0;      // Total bytes of PCM audio data written to the file.

  // Configure the source reader to get uncompressed PCM audio from the source file.
  hr := ConfigureAudioStream(pReader,
                             pAudioType);

  // Write the WAVE file header.
  if SUCCEEDED(hr) then
    hr := WriteWaveHeader(hFile,
                          pAudioType,
                          cbHeader);


  // Calculate the maximum amount of audio to decode, in bytes.
  if SUCCEEDED(hr) then
    begin
      cbMaxAudioData := CalculateMaxAudioDataSize(pAudioType,
                                                  cbHeader,
                                                  msecAudioData);

      // Decode audio data to the file.
      hr := WriteWaveData(hFile,
                          pReader,
                          cbMaxAudioData,
                          cbAudioData);
    end;

  // Fix up the RIFF headers with the correct sizes.
  if SUCCEEDED(hr) then
    hr := FixUpChunkSizes(hFile,
                          cbHeader,
                          cbAudioData);

  // SafeRelease(pAudioType); releasing wil be automaticly done by compiler.
  Result := hr;
end;


//-------------------------------------------------------------------
// CalculateMaxAudioDataSize
//
// Calculates how much audio to write to the WAVE file, given the
// audio format and the maximum duration of the WAVE file.
// The return value is the size of the desired audio clip in bytes
//-------------------------------------------------------------------
function CalculateMaxAudioDataSize(pAudioType: IMFMediaType;     // The PCM audio format.
                                   cbHeader: DWORD;              // The size of the WAVE file header.
                                   msecAudioData: DWORD): DWord;
var
  cbBlockSize: UINT32;      // Audio frame size, in bytes.
  cbBytesPerSecond: UINT32; // Bytes per second.
  cbAudioClipSize: DWORD;
  cbMaxSize: DWORD;

begin

  // Get the audio block size and number of bytes/second from the audio format.

  cbBlockSize := MFGetAttributeUINT32(pAudioType,
                                      MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                      0);

  cbBytesPerSecond := MFGetAttributeUINT32(pAudioType,
                                           MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                           0);

  // Calculate the maximum amount of audio data to write.
  // This value equals (duration in seconds x bytes/second), but cannot
  // exceed the maximum size of the data chunk in the WAVE file.

  // Size of the desired audio clip in bytes:
  cbAudioClipSize := DWORD(MulDiv(cbBytesPerSecond,
                            msecAudioData,
                            1000));

  // Largest possible size of the data chunk:
  cbMaxSize := (MAXDWORD - cbHeader);

  // Maximum size altogether.
  cbAudioClipSize := min(cbAudioClipSize,
                         cbMaxSize);

  // Round to the audio block size, so that we do not write a partial audio frame.
  cbAudioClipSize := ((cbAudioClipSize div cbBlockSize) * cbBlockSize);

  Result := cbAudioClipSize;
end;


//-------------------------------------------------------------------
// ConfigureAudioStream
//
// Selects an audio stream from the source file, and configures the
// stream to deliver decoded PCM audio.
//-------------------------------------------------------------------
function ConfigureAudioStream(pReader: IMFSourceReader;    // Pointer to the source reader.
                              out ppPCMAudio: IMFMediaType): HResult;
var
  hr: HResult;
  pUncompressedAudioType: IMFMediaType;
  pPartialType: IMFMediaType;

begin

  // Create a partial media type that specifies uncompressed PCM audio.
  hr := MFCreateMediaType(pPartialType);

  if SUCCEEDED(hr) then
    hr := pPartialType.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Audio);

  if SUCCEEDED(hr) then
    hr := pPartialType.SetGUID(MF_MT_SUBTYPE,
                               MFAudioFormat_PCM);

  // Set this type on the source reader. The source reader will
  // load the necessary decoder.
  if SUCCEEDED(hr) then
    hr := pReader.SetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),
                                      0,
                                      pPartialType);

  // Get the complete uncompressed format.
  if SUCCEEDED(hr) then
    hr := pReader.GetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),
                                      pUncompressedAudioType);


  // a suggestion to prevent memory leak issues from msdn
  if SUCCEEDED(hr) then
    hr := pReader.SetStreamSelection(DWORD(MF_SOURCE_READER_ALL_STREAMS),
                                     False);

  // Ensure the stream is selected.
  if SUCCEEDED(hr) then
    hr := pReader.SetStreamSelection(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),
                                     True);


  // Return the PCM format to the caller.
  if SUCCEEDED(hr) then
    ppPCMAudio := pUncompressedAudioType;

  // In C/C++ we have to release the interfaces when going out of scope.
  // Delphi is automaticly taking care of that.
  // SafeRelease(pUncompressedAudioType);
  // SafeRelease(pPartialType);
  Result := hr;
end;


//-------------------------------------------------------------------
// WriteWaveHeader
//
// Write the WAVE file header.
//
// Note: This function writes placeholder values for the file size
// and data size, as these values will need to be filled in later.
//-------------------------------------------------------------------

function WriteWaveHeader(const hFile: THandle;             // Output file.
                         pMediaType: IMFMediaType;         // PCM audio format.
                         out pcbWritten: DWORD): HResult;  // bytes written
var
  hr: HResult;
  cbFormat: UINT32;
  header: array [0..4] of DWORD; // Each FOURCC (DWord) has a datalength of 4 bytes
  pWav: PWAVEFORMATEX;
  dataHeader: array [0..1] of DWORD;

begin
  cbFormat := 0;
  pcbWritten := 0;
  pWav := Nil;

  // Convert the PCM audio format into a WAVEFORMATEX structure.
  hr := MFCreateWaveFormatExFromMFMediaType(pMediaType, // Pointer to the IMFMediaType interface of the media type.
                                            pWav,   // Receives a pointer to the WAVEFORMATEX structure.
                                            cbFormat, // Receives the size of the WAVEFORMATEX structure.
                                            DWord(MFWaveFormatExConvertFlag_Normal)); // Contains a flag from the MFWaveFormatExConvertFlags enumeration.

  // Wave header explained: https://docs.microsoft.com/en-us/windows/win32/medfound/tutorial--decoding-audio


  // Define the 'RIFF' header and the start of the 'fmt ' chunk first
  if SUCCEEDED(hr) then
    begin

      // Here the header sections are stored in a fixed array.
      // A dynamic array could do as well, even as a record.
      // When using the Win api WriteToFile function, keep in mind that pointer data to be written, must be de-referenced!

      // RIFF header
      header[0] := FCC('RIFF'); // 4 bytes
      header[1] := 0;           // 4 bytes
      header[2] := FCC('WAVE'); // 4 bytes
      // Start of 'fmt ' chunk
      header[3] := FCC('fmt '); // 4 bytes
      header[4] := cbFormat;    // 4 bytes

      // The WaveFormatEx structure will be placed inbetween RIFF and Data header

      // data
      dataHeader[0] := FCC('data'); // 4 bytes
      dataHeader[1] := 0;           // 4 bytes

      // Write the RIFF header
      hr := WriteToFile(hFile,
                        header,   // can be written directly
                        SizeOf(header));

      // Write the WAVEFORMATEX structure.
      if SUCCEEDED(hr) then
        hr := WriteToFile(hFile,
                          pWav^,  // Dereference the pointer, to store the WAVEFORMATEX struct correctly
                          cbFormat);

      // Write the start of the 'data' chunk
      if SUCCEEDED(hr) then
        hr := WriteToFile(hFile,
                          dataHeader, // can be written directly
                          SizeOf(dataHeader));

      if SUCCEEDED(hr) then
        pcbWritten := SizeOf(header) + cbFormat + SizeOf(dataHeader);

    end;


  CoTaskMemFree(pWav);
  Result := hr;
end;


//-------------------------------------------------------------------
// WriteWaveData
//
// Decodes PCM audio data from the source file and writes it to
// the WAVE file.
//-------------------------------------------------------------------
function WriteWaveData(const hFile: THandle;        // Output file.
                       pReader: IMFSourceReader;    // Source reader.
                       cbMaxAudioData: DWORD;       // Maximum amount of audio data (bytes).
                       out pcbDataWritten: DWORD): HResult;
var
  hr: HResult;
  cbAudioData: DWORD;
  cbBuffer: DWORD;
  pAudioData: PByte;
  pSample: IMFSample;
  pBuffer: IMFMediaBuffer;
  dwFlags: DWORD;
  ActualStreamIndex: DWORD;
  Timestamp: LONGLONG;

begin
  hr := E_FAIL;
  cbAudioData := 0;
  cbBuffer := 0;
  pAudioData := nil;
  pBuffer := nil;

  // Get audio samples from the source reader.
  while (cbAudioData < cbMaxAudioData) do
    begin

      // Read the next sample.
      // The SourceReader is in Synchronous Mode
      hr := pReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                               0,
                               @ActualStreamIndex,
                               @dwFlags,
                               @Timestamp,
                               @pSample);
      if FAILED(hr) then
        Break;
      
      // Check whether the data is still valid
      if (MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED and dwFlags <> 0) then
			  Break;

      // Check for EOF
      if (MF_SOURCE_READERF_ENDOFSTREAM and dwFlags <> 0) then
			  Break;

      // If the sample is nil, there is a gap in the data stream that can't be filled; No reason to quit though..
      if (pSample = nil) then
        Continue;

      // Get a pointer to the audio data in the sample.
      hr := pSample.ConvertToContiguousBuffer(pBuffer);

      if FAILED(hr) then
        Break;

      hr := pBuffer.Lock(pAudioData,
                         nil,
                         @cbBuffer);

      if FAILED(hr) then
        Break;

      // Make sure not to exceed the specified maximum size.
      if ((cbMaxAudioData - cbAudioData) < cbBuffer) then
        cbBuffer := (cbMaxAudioData - cbAudioData);

      // Write this data to the output file.
      hr := WriteToFile(hFile,
                        pAudioData^,
                        cbBuffer);

      if FAILED(hr) then
        Break;

      // Unlock the buffer.
      hr := pBuffer.Unlock();
      pAudioData := nil;

      if FAILED(hr) then
        Break;

      // Update running total of audio data.
      Inc(cbAudioData, cbBuffer);

      // We reached the end of data required
      if (cbAudioData >= cbMaxAudioData) then
        Break;

      // User wants to abort
      if bFlush then
        begin
          hr := pReader.Flush(DWORD(MF_SOURCE_READER_ALL_STREAMS));
          Break;
        end;
    end;

  if SUCCEEDED(hr) then
    pcbDataWritten := cbAudioData;

  if (pAudioData <> Nil) then
    pBuffer.Unlock();

  // In Delphi this will be done automaticly.
  // SafeRelease(pBuffer);
  // SafeRelease(pSample);

  Result := hr;
end;

//-------------------------------------------------------------------
// FixUpChunkSizes
//
// Writes the file-size information into the WAVE file header.
//
// WAVE files use the RIFF file format. Each RIFF chunk has a data
// size, and the RIFF header has a total file size.
//-------------------------------------------------------------------

function FixUpChunkSizes(const hFile: THandle;     // Output file.
                         cbHeader: DWORD;          // Size of the 'fmt ' chuck.
                         cbAudioData: DWORD): HResult;
var
  hr: HResult;
  ll: Int64;
  cbRiffFileSize: DWORD;

begin
  hr := S_OK;

  ll := (cbHeader - SizeOf(DWORD));

  if (SetFilePointerEx(hFile,
                       ll,
                       Nil,
                       FILE_BEGIN) = False) then
    hr := HRESULT_FROM_WIN32(GetLastError());


  // Write the data size.
  if SUCCEEDED(hr) then
    hr := WriteToFile(hFile,
                      cbAudioData,
                      SizeOf(cbAudioData));

  if SUCCEEDED(hr) then
    begin
        // Write the file size.
        ll := sizeof(FOURCC);

        if (SetFilePointerEx(hFile,
                             ll,
                             Nil,
                             FILE_BEGIN) = False) then
          hr := HRESULT_FROM_WIN32(GetLastError());
    end;

  if SUCCEEDED(hr) then
    begin
      cbRiffFileSize := cbHeader + cbAudioData - 8;

      // NOTE: The "size" field in the RIFF header does not include
      // the first 8 bytes of the file. i.e., it is the size of the
      // data that appears _after_ the size field.

      hr := WriteToFile(hFile,
                        cbRiffFileSize,
                        SizeOf(cbRiffFileSize));
    end;

    Result := hr;
end;



//-------------------------------------------------------------------
//
// Writes a block of data to a file
//
// hFile: Handle to the file.
// p: Pointer to the buffer to write.
// cb: Size of the buffer, in bytes.
//
//-------------------------------------------------------------------

function WriteToFile(const hFile: THandle;
                     const p;
                     cb: DWORD): HResult;
var
  hr: HResult;
  cbWritten: DWORD;
  bResult: BOOL;

begin
  cbWritten := 0;
  hr := S_OK;

  bResult := WriteFile(hFile,
                       p,
                       cb,
                       cbWritten,
                       Nil);
  if not bResult then
    hr := HRESULT_FROM_WIN32(GetLastError());

  Application.ProcessMessages;

  Result := hr;
end;


procedure FlushSourceReader(doFlush: Boolean);
begin
  bFlush := doFlush;
end;


end.

