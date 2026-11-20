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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
  {WinMM}
  WinApi.WinMM.MMeApi,
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


function SampleSetFilePointerEx(hFile: THandle;
                                Distance: Int64;
                                NewPosition: Pointer;
                                Method: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'SetFilePointerEx';

function SampleGetFullPathNameW(Name: PWideChar;
                                Size: DWORD;
                                Buffer: PWideChar;
                                FilePart: Pointer): DWORD; stdcall;
  external 'kernel32.dll' name 'GetFullPathNameW';


function FullFileName(Name: PWideChar): UnicodeString;
var
  Size: DWORD;
  Written: DWORD;
begin
  Result := '';
  Size := SampleGetFullPathNameW(Name,0,nil,nil);
  if Size = 0 then
    Exit;
  SetLength(Result,Size);
  Written := SampleGetFullPathNameW(Name,Size,PWideChar(Result),nil);
  if (Written = 0) or (Written >= Size) then
    Result := ''
  else
    SetLength(Result,Written);
end;


function ExtractSound(const wHandle: HWnd;
                      const wszSourceFile: PWideChar;
                      const wszTargetFile: PWideChar;
                      msDuration: Int64): HResult;
var
  Reader: IMFSourceReader;
  FileHandle: THandle;
  SourcePath: UnicodeString;
  TargetPath: UnicodeString;
  Extension: UnicodeString;
  Attributes: DWORD;
begin
  Result := E_INVALIDARG;
  if (wszSourceFile = nil) or (wszTargetFile = nil) then
    Exit;
  if (wszSourceFile^ = #0) or (wszTargetFile^ = #0) or
     (msDuration <= 0) or (msDuration > MaxInt) then
    Exit;
  SourcePath := FullFileName(wszSourceFile);
  TargetPath := FullFileName(wszTargetFile);
  Extension := Copy(TargetPath,Length(TargetPath) - 3,4);
  if (SourcePath = '') or (TargetPath = '') or
     (lstrcmpiW(PWideChar(Extension),'.wav') <> 0) or
     (lstrcmpiW(PWideChar(SourcePath),PWideChar(TargetPath)) = 0) then
    Exit;
  Attributes := GetFileAttributesW(wszSourceFile);
  if (Attributes = INVALID_FILE_ATTRIBUTES) or
     ((Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));
  bFlush := False;
  Result := MFStartup(MF_VERSION,MFSTARTUP_FULL);
  if FAILED(Result) then
    Exit;
  try
    Result := MFCreateSourceReaderFromURL(wszSourceFile,nil,Reader);
    if FAILED(Result) then
      Exit;
    FileHandle := CreateFileW(wszTargetFile,GENERIC_WRITE,FILE_SHARE_READ,nil,
                              CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
    if FileHandle = INVALID_HANDLE_VALUE then
      Exit(HRESULT_FROM_WIN32(GetLastError));
    try
      Result := WriteWaveFile(Reader,FileHandle,LONG(msDuration));
    finally
      CloseHandle(FileHandle);
    end;
  finally
    Reader := nil;
    MFShutdown;
  end;
end;


function WriteWaveFile(pReader: IMFSourceReader;
                       const hFile: THandle;
                       msecAudioData: LONG): HResult;
var
  AudioType: IMFMediaType;
  HeaderSize: DWORD;
  AudioSize: DWORD;
  MaxSize: DWORD;
begin
  if (pReader = nil) or (msecAudioData <= 0) then
    Exit(E_INVALIDARG);
  Result := ConfigureAudioStream(pReader,AudioType);
  if FAILED(Result) then
    Exit;
  Result := WriteWaveHeader(hFile,AudioType,HeaderSize);
  if FAILED(Result) then
    Exit;
  MaxSize := CalculateMaxAudioDataSize(AudioType,HeaderSize,DWORD(msecAudioData));
  Result := WriteWaveData(hFile,pReader,MaxSize,AudioSize);
  if SUCCEEDED(Result) then
    Result := FixUpChunkSizes(hFile,HeaderSize,AudioSize);
end;


function ConfigureAudioStream(pReader: IMFSourceReader;
                              out ppPCMAudio: IMFMediaType): HResult;
var
  PartialType: IMFMediaType;
begin
  ppPCMAudio := nil;
  if pReader = nil then
    Exit(E_POINTER);
  Result := MFCreateMediaType(PartialType);
  if SUCCEEDED(Result) then
    Result := PartialType.SetGUID(MF_MT_MAJOR_TYPE,MFMediaType_Audio);
  if SUCCEEDED(Result) then
    Result := PartialType.SetGUID(MF_MT_SUBTYPE,MFAudioFormat_PCM);
  if SUCCEEDED(Result) then
    Result := pReader.SetStreamSelection(DWORD(MF_SOURCE_READER_ALL_STREAMS),False);
  if SUCCEEDED(Result) then
    Result := pReader.SetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),nil,PartialType);
  if SUCCEEDED(Result) then
    Result := pReader.GetCurrentMediaType(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),@ppPCMAudio);
  if SUCCEEDED(Result) then
    Result := pReader.SetStreamSelection(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),True);
end;


function CalculateMaxAudioDataSize(pAudioType: IMFMediaType;
                                   cbHeader: DWORD;
                                   msecAudioData: DWORD): DWORD;
var
  BlockSize: UINT32;
  BytesPerSecond: UINT32;
  Size: UInt64;
  Limit: UInt64;
begin
  Result := 0;
  if pAudioType = nil then
    Exit;
  BlockSize := MFGetAttributeUINT32(pAudioType,MF_MT_AUDIO_BLOCK_ALIGNMENT,0);
  BytesPerSecond := MFGetAttributeUINT32(pAudioType,MF_MT_AUDIO_AVG_BYTES_PER_SECOND,0);
  if (BlockSize = 0) or (BytesPerSecond = 0) then
    Exit;
  Size := UInt64(msecAudioData) * BytesPerSecond div 1000;
  Limit := UInt64(MAXDWORD) - cbHeader;
  if Size > Limit then
    Size := Limit;
  Result := DWORD(Size div BlockSize * BlockSize);
end;


function WriteWaveHeader(const hFile: THandle;
                         pMediaType: IMFMediaType;
                         out pcbWritten: DWORD): HResult;
var
  FormatSize: UINT32;
  Format: PWAVEFORMATEX;
  Header: array[0..4] of DWORD;
  DataHeader: array[0..1] of DWORD;
begin
  pcbWritten := 0;
  if pMediaType = nil then
    Exit(E_POINTER);
  Format := nil;
  FormatSize := 0;
  Result := MFCreateWaveFormatExFromMFMediaType(pMediaType,Format,FormatSize,
                                               DWORD(MFWaveFormatExConvertFlag_Normal));
  try
    if FAILED(Result) then
      Exit;
    if (Format = nil) or (FormatSize = 0) then
      Exit(E_UNEXPECTED);
    Header[0] := FCC('RIFF');
    Header[1] := 0;
    Header[2] := FCC('WAVE');
    Header[3] := FCC('fmt ');
    Header[4] := FormatSize;
    DataHeader[0] := FCC('data');
    DataHeader[1] := 0;
    Result := WriteToFile(hFile,Header,SizeOf(Header));
    if SUCCEEDED(Result) then
      Result := WriteToFile(hFile,Format^,FormatSize);
    if SUCCEEDED(Result) then
      Result := WriteToFile(hFile,DataHeader,SizeOf(DataHeader));
    if SUCCEEDED(Result) then
      pcbWritten := SizeOf(Header) + FormatSize + SizeOf(DataHeader);
  finally
    CoTaskMemFree(Format);
  end;
end;


function WriteWaveData(const hFile: THandle;
                       pReader: IMFSourceReader;
                       cbMaxAudioData: DWORD;
                       out pcbDataWritten: DWORD): HResult;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  AudioData: PByte;
  BufferSize: DWORD;
  Flags: DWORD;
  UnlockResult: HRESULT;
begin
  pcbDataWritten := 0;
  if pReader = nil then
    Exit(E_POINTER);
  Result := S_OK;
  while (pcbDataWritten < cbMaxAudioData) and not bFlush do
    begin
      // Native output slots overwrite pointers; release the previous interfaces first.
      Sample := nil;
      Buffer := nil;
      Flags := 0;
      Result := pReader.ReadSample(DWORD(MF_SOURCE_READER_FIRST_AUDIO_STREAM),0,
                                    nil,@Flags,nil,@Sample);
      if FAILED(Result) then
        Exit;
      if (Flags and MF_SOURCE_READERF_ERROR) <> 0 then
        Exit(E_FAIL);
      if (Flags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0 then
        Exit(MF_E_INVALIDMEDIATYPE);
      if Sample <> nil then
        begin
          Result := Sample.ConvertToContiguousBuffer(@Buffer);
          if FAILED(Result) then
            Exit;
          AudioData := nil;
          BufferSize := 0;
          Result := Buffer.Lock(AudioData,nil,@BufferSize);
          if FAILED(Result) then
            Exit;
          try
            if BufferSize > cbMaxAudioData - pcbDataWritten then
              BufferSize := cbMaxAudioData - pcbDataWritten;
            if BufferSize > 0 then
              Result := WriteToFile(hFile,AudioData^,BufferSize);
            if SUCCEEDED(Result) then
              Inc(pcbDataWritten,BufferSize);
          finally
            UnlockResult := Buffer.Unlock;
            if SUCCEEDED(Result) then
              Result := UnlockResult;
          end;
          if FAILED(Result) then
            Exit;
        end;
      if (Flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0 then
        Break;
    end;
  if bFlush then
    Result := pReader.Flush(DWORD(MF_SOURCE_READER_ALL_STREAMS));
end;


function FixUpChunkSizes(const hFile: THandle;
                         cbHeader: DWORD;
                         cbAudioData: DWORD): HResult;
var
  Position: Int64;
  RiffSize: DWORD;
begin
  if (cbHeader < 8) or (UInt64(cbHeader) + cbAudioData > MAXDWORD) then
    Exit(E_INVALIDARG);
  Position := cbHeader - SizeOf(DWORD);
  if not SampleSetFilePointerEx(hFile,Position,nil,FILE_BEGIN) then
    Exit(HRESULT_FROM_WIN32(GetLastError));
  Result := WriteToFile(hFile,cbAudioData,SizeOf(cbAudioData));
  if FAILED(Result) then
    Exit;
  Position := 4;
  if not SampleSetFilePointerEx(hFile,Position,nil,FILE_BEGIN) then
    Exit(HRESULT_FROM_WIN32(GetLastError));
  RiffSize := cbHeader + cbAudioData - 8;
  Result := WriteToFile(hFile,RiffSize,SizeOf(RiffSize));
end;


function WriteToFile(const hFile: THandle;
                     const p;
                     cb: DWORD): HResult;
var
  Written: DWORD;
begin
  Written := 0;
  if not WriteFile(hFile,p,cb,Written,nil) then
    Exit(HRESULT_FROM_WIN32(GetLastError));
  if Written <> cb then
    Exit(HRESULT_FROM_WIN32(ERROR_WRITE_FAULT));
  Result := S_OK;
  Application.ProcessMessages;
end;


procedure FlushSourceReader(doFlush: Boolean);
begin
  bFlush := doFlush;
end;

end.
