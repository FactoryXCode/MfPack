// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  AudioClipEngine.pas
// Kind: Pascal Unit
// Release date: 21-12-2019
// Language: ENU
//
// Revision Version: 3.1.4
//
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
//   Note: The original application was a console app. running in synchrone mode.
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
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: >= MfPackX300
// Known Issues: High use of CPU and power consumption due to IMFSourceReader.OnReadSample.
//               Please read the comments about this issue.
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Parts of the AudioClip sample
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
unit AudioClipEngine;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.ComBaseApi,
  WinApi.WinMM.MMReg,
  {Vcl}
  Vcl.Dialogs,
  {System}
  System.Classes,
  System.SysUtils,
  System.Math,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropVarUtil,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfError,
  {Project}
  Helpers;

const
  // The minimum OS support
  WINVER = _WIN32_WINNT_WIN7;
  // By default the maximum filesize of a wavefile can't be larger than 4294967295 bytes (approx. 4 GB)
  MAX_WAV_FILESIZE = MAXDWORD - (44 + SizeOf(WaveformatEx));
  //
  WM_CLIPENGINE_MSG = WM_APP + 111;
  WM_USER_ABORT     = WM_APP + 112;


type

  // Messages for caller response
  TCallBackState = (STATE_END_OF_STREAM,
                    STATE_END_OF_CLIP,
                    STATE_ABORT,
                    STATE_ERROR
                    );

  // Note: We don't use the TInterfacedObject, because TInterfacedPersistence does not
  // have a reference counting mechanism. It delegates it to it's owner and that is what we want.
  // You may read some more about this here:
  // https://www.codeproject.com/Articles/1252175/Fixing-Delphis-Interface-Limitations
  TAudioClipClass = class(TInterfacedPersistent, IMFSourceReaderCallback)
  private
  { private fields }
    FCritSec: TMFCritSec;
    hwCaller: HWnd;                 // The caller's handle
    hTargetFile: THandle;           // The targetfile handle
                                    // We set this handle to INVALID if the filehandle is closed. See: https://docs.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
    cbAudioData: DWORD;             // Total bytes of PCM audio data written to the file.
    cbHeader: DWORD;                // Size of the WAVE file header, in bytes.
    pReader: IMFSourceReader;       // The source reader.
    bSourceReaderFlushed : Boolean; // Flag to indicate we flushed the streams. To prevent looping.
    cbState: TCallBackState;        // Callbackstate

  {private methods}

    // IMFSourceReaderCallback implementation -----------------------------------

    // Called when the (pReader)IMFSourceReader.ReadSample method completes.
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HResult; stdcall;
    // Called when the IMFSourceReader.Flush method completes.
    // Note: The Flush method discards all queued samples and cancels all pending sample requests.
    //       This method can complete either synchronously or asynchronously.
    function OnFlush(dwStreamIndex: DWORD): HResult; stdcall;
    // Called when the source reader receives certain events from the media source.
    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HResult; stdcall;

    //  These functions will never been called in this example.
    function OnBufferingStarted(pEvent: IMFMediaEvent): HResult;
    function OnBufferingStopped(pEvent: IMFMediaEvent): HResult;
    function OnConnectStart(pEvent: IMFMediaEvent): HResult;
    function OnConnectEnd(pEvent: IMFMediaEvent): HResult;
    function OnExtendedType(pEvent: IMFMediaEvent): HResult;
    function OnSourceCharacteristicsChanged(pEvent: IMFMediaEvent): HResult;
    function OnSourceMetadataChanged(pEvent: IMFMediaEvent): HResult;
    function OnUnExpected(pEvent: IMFMediaEvent): HResult;


    // -------------------------------------------------------------------------

    function WriteWaveFile(): HResult; // Handle to the output file.

    function ConfigureAudioStream(out ppPCMAudio: IMFMediaType): HResult;

    function WriteWaveHeader(pMediaType: IMFMediaType;    // PCM audio format.
                             out pcbWritten: DWORD): HResult;

    function CalculateMaxAudioDataSize(pAudioType: IMFMediaType;     // The PCM audio format.
                                       cbHeader: DWORD;              // The size of the WAVE file header.
                                       msecAudioData: DWORD): DWord;

    function WriteWaveData(): HResult;

    function FixUpChunkSizes(): HResult;

    function WriteToFile(const p;
                         cb: DWORD): HResult;

    // Write header values and closes the targetfile, when audiodata has been processed by callback interface.
    procedure FinalizeClip(dwState: TCallBackState;
                           rs: HResult);

    // Catches all messages to this object.
    procedure WndProc(var Msg: TMessage);


  public
  {public fields}

    FHWnd: HWND;               // Handle to this class
    wcSourceFile: PWideChar;
    wcTargetFile: PWideChar;
    msecDuration: DWord;       // Duration of the sourcefile in milliseconds
    dwDataToExtract: DWord;    // Amount of data in Milliseconds to extract from the sourcefile.
    dwAudioDataWritten: DWord; // Total bytes of PCM audio data written to the file.
    SamplePriority: Cardinal;
    dwActualStreamIndex: DWord;
    dwMaxAudioData: DWord;

  {public methods}

    // Constructor
    constructor Create(hwndCaller: HWND;
                       wszSourceFile: PWideChar;
                       out hReturn: HResult); overload;
    procedure BeforeDestruction(); override;
    // Destructor
    destructor Destroy(); override;
    // Get the duration of the sourcefile
    function GetDuration(): DWord;
    // The main method
    function ExtractSoundClip(): HResult;    // The duration in milliseconds to extract.

    property Flushed: Boolean read bSourceReaderFlushed write bSourceReaderFlushed;
    property CallBackState: TCallBackState read cbState write cbState;
  end;


implementation


////////////////////////////////////////////////////////////////////////////////

// Constructor
constructor TAudioClipClass.Create(hwndCaller: HWND;
                                   wszSourceFile: PWideChar;
                                   out hReturn: HResult);
var
  hr: HResult;
  pAttributes: IMFAttributes;

begin
  inherited Create();
  FHWnd := AllocateHWnd(WndProc);
  hwCaller := hwndCaller;
  wcSourceFile := wszSourceFile;
  dwAudioDataWritten := 0;

  hr := InitMF();

  // Create the attributes store
  if Succeeded(hr) then
    hr := MFCreateAttributes(pAttributes,
                             2);

  // 1 Link the callback interface with the sourcereader
  if Succeeded(hr) then
    hr := pAttributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                                 Self as IMfSourceReaderCallback{(Self)});
  // 2 Enable hardware transforms
  if Succeeded(hr) then
    hr :=  pAttributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
                                 1 {True} );

  if Succeeded(hr) then
    // Create the source reader to read the input file and attribute store.
    hr := MFCreateSourceReaderFromURL(wcSourceFile,
                                      pAttributes,
                                      pReader);

  if Failed(hr) then
    begin
      //  printf("Error opening input file: %S\n", wszSourceFile, hr);
      MessageBox(0,
                 LPCWSTR('Error opening input file ' + #13 + wszSourceFile),
                 LPCWSTR('Error'),
                 MB_ICONSTOP);
    end;

  // Get sourcefile duration
  if Succeeded(hr) then
    msecDuration := GetDuration();

  // Create CriticalSection
  FCritSec := TMFCritSec.Create;

  hReturn := hr;

end;


procedure TAudioClipClass.BeforeDestruction();
begin
  DeallocateHWnd(FHWnd);
  if assigned(pReader) then
    begin
      pReader.Flush(MF_SOURCE_READER_ANY_STREAM);
      SafeRelease(pReader);
      //pReader := Nil;
    end;

  if Assigned(FCritSec) then
    FreeAndNil(FCritSec);

  wcSourceFile := Nil;
  inherited BeforeDestruction();
end;


// Destructor
destructor TAudioClipClass.Destroy();
begin
  CloseMF();
  inherited Destroy();
end;


function TAudioClipClass.OnReadSample(hrStatus: HRESULT;
                                      dwStreamIndex: DWORD;
                                      dwStreamFlags: DWORD;
                                      llTimestamp: LONGLONG;
                                      pSample: IMFSample): HResult;
var
  hr: HResult;
  cbBuffer: DWORD;
  pAudioData: PByte;
  pBuffer: IMFMediaBuffer;     // The media buffer

label
  done,
  nextsample;

begin
  hr := S_OK;
  // store the current stream index
  dwActualStreamIndex := dwStreamIndex;
  cbBuffer := 0;
  pAudioData := Nil;
  pBuffer := Nil;

  FCritSec.Lock();

  if Failed(hrStatus) then
    begin
      hr := hrStatus;
      goto done;
    end;

  // If the sample is Nil, there is a gap in the data stream that can't be filled; No reason to quit though..
  if (pSample = Nil) then
    goto nextsample;

  // Get a pointer to the audio data in the sample.
  hr := pSample.ConvertToContiguousBuffer(pBuffer);
  // or
  // hr := pSample.GetBufferByIndex(0, pBuffer);
  if FAILED(hr) then
    goto done;

  hr := pBuffer.Lock(pAudioData,
                     Nil,
                     @cbBuffer);
  if FAILED(hr) then
    goto done;

  // Make sure not to exceed the specified maximum size.
  if ((dwMaxAudioData - cbAudioData) < cbBuffer) then
    cbBuffer := (dwMaxAudioData - cbAudioData);

  // Write this data to the output file.
  hr := WriteToFile(pAudioData^,
                    cbBuffer);
  if FAILED(hr) then
    goto done;

  // Unlock the buffer.
  hr := pBuffer.Unlock();
  pAudioData := Nil;

  if FAILED(hr) then
    goto done;

  // Update running total of audio data.
  Inc(cbAudioData, cbBuffer);

  if (cbAudioData >= dwMaxAudioData) then
    begin
      FinalizeClip(STATE_END_OF_CLIP, hr);
      goto done;
    end;

  // Note: When a streamflag is set, the result (HResult) could be affected too.
  // Checks flags
  
  // Check whether the data is still valid
  if (MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED And dwStreamFlags <> 0) then
    begin
      FinalizeClip(STATE_ERROR, hr);
      goto done;
    end;

  // Check for EOF
  if (MF_SOURCE_READERF_ENDOFSTREAM And dwStreamFlags <> 0) then
    begin
      FinalizeClip(STATE_END_OF_STREAM, hr);
      goto done;
    end;

  // Important Note: On Windows 7 or Windows 10 an error $8007000E (0x8007000E)
  //                 with message 'Not enough storage is available to complete this operation.' might occure,
  //                 when pBuffer and/or pSample are not properly released.
  //                 The other issue is CPU and Power consumption
  //                 To fix this issue, we implement MsgWaitForMultipleObjects to give the
  //                 ReadSample method more time to organize it's sample pool.

  Sleep(SamplePriority);
  // or
  // MsgWaitForMultipleObjects(0,
  //                           Nil^,
  //                           False,  // do NOT set this to true!
  //                           SamplePriority,
  //                           QS_ALLINPUT);

nextsample:
  if Assigned(pReader) then
    // Read the next sample.
    hr := pReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                             0,
                             nil,
                             nil,
                             nil,
                             nil)
  else
    goto done;

  // Check result
  if FAILED(hr) then
    goto done;

done:

  if (pAudioData <> Nil) then
    begin
      pBuffer.Unlock();
      pAudioData := Nil;
    end;

  if FAILED(hr) then
    FinalizeClip(STATE_ERROR,
                 hr);

  // Just Nil will do too
  SafeRelease(pBuffer);
  SafeRelease(pSample);

  FCritSec.Unlock();

  Result := hr;
end;


function TAudioClipClass.OnFlush(dwStreamIndex: DWORD): HResult;
begin
  Result := S_OK;
  bSourceReaderFlushed := True;
  // This method will be triggered when SourceReader.flush is called.
  // However we handle this event in procedure FinalizeClip to prevent double handling.
  if CallBackState = STATE_ABORT then
    begin
      FinalizeClip(STATE_ABORT,
                   E_ABORT);
      Result := E_ABORT;
    end;

end;

// Note: The OnEvent method will probarly never been triggered in this case.
//       We implemented this for completeness.
function TAudioClipClass.OnEvent(dwStreamIndex: DWORD;
                                 pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
  EventType: MediaEventType;

begin
  hr := pEvent.GetType(EventType);
  if Succeeded(hr) then
    begin
      // Until the current implementation of this MF release,
      // the source reader uses these methods to forward the following events to the application:
      case EventType of
        MEBufferingStarted:             begin hr := OnBufferingStarted(pEvent); end;
        MEBufferingStopped:             begin hr := OnBufferingStopped(pEvent); end;
        MEConnectStart:                 begin hr := OnConnectStart(pEvent); end;
        MEConnectEnd:                   begin hr := OnConnectEnd(pEvent); end;
        MEExtendedType:                 begin hr := OnExtendedType(pEvent); end;
        MESourceCharacteristicsChanged: begin hr := OnSourceCharacteristicsChanged(pEvent); end;
        MESourceMetadataChanged:        begin hr := OnSourceMetadataChanged(pEvent); end;
      else  // This should not happen.
        begin hr := OnUnExpected(pEvent) end;
      end;
    end;
  Result := hr;
end;



function TAudioClipClass.OnBufferingStarted(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnBufferingStopped(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnConnectStart(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnConnectEnd(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnExtendedType(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnSourceCharacteristicsChanged(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnSourceMetadataChanged(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


function TAudioClipClass.OnUnExpected(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
begin
  hr := S_OK;

  Result := hr;
end;


// -----------------------------------------------------------------------------



function TAudioClipClass.ExtractSoundClip(): HResult;   // The duration in ms to extract.
var
  hr: HResult;

label
  done;

begin
  hr := S_OK;

  hTargetFile := INVALID_HANDLE_VALUE;
  bSourceReaderFlushed := False;

  if not FileExists(wcSourceFile) then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto done;
    end;

  if (wcTargetFile = '') or (ExtractFileExt(WideCharToString(wcTargetFile)) <> '.wav') then
    begin
      hr := ERROR_FILE_NOT_FOUND;
      goto done;
    end;

  // Open the output file for writing.
  if SUCCEEDED(hr) then
    begin
      hTargetFile := CreateFile(wcTargetFile,
                                GENERIC_WRITE,
                                FILE_SHARE_READ,
                                Nil,
                                CREATE_ALWAYS,
                                0,
                                0);

      if (hTargetFile = INVALID_HANDLE_VALUE) then
        begin
          hr := HRESULT_FROM_WIN32(GetLastError());
          goto done;
        end;
    end
  else
    goto done;

  // Write the WAVE file.
  if SUCCEEDED(hr) then
    hr := WriteWaveFile();

done:
  Result := hr;
end;


//-------------------------------------------------------------------
// WriteWaveFile
//
// Writes a WAVE file by getting audio data from the source reader.
//
//-------------------------------------------------------------------

function TAudioClipClass.WriteWaveFile(): HResult; // Handle to the output file.

var
  hr: HResult;
  pAudioType: IMFMediaType;  // Represents the PCM audio format.

begin

  cbHeader := 0;         // Reset size of the WAVE file header, in bytes.
  cbAudioData := 0;      // Reset total bytes of PCM audio data written to the file.

  // Configure the source reader to get uncompressed PCM audio from the source file.
  hr := ConfigureAudioStream(pAudioType);

  // Write the WAVE file header.
  if SUCCEEDED(hr) then
    hr := WriteWaveHeader(pAudioType,
                          cbHeader);

  // Calculate the maximum amount of audio to decode, in bytes.
  if SUCCEEDED(hr) then
    begin
      dwMaxAudioData := CalculateMaxAudioDataSize(pAudioType,
                                                  cbHeader,
                                                  dwDataToExtract);

      // Decode audio data to the file.
      hr := WriteWaveData();
    end;

  // SafeRelease(pAudioType); // releasing wil be automaticly done by compiler.
  Result := hr;
end;


//-------------------------------------------------------------------
// CalculateMaxAudioDataSize
//
// Calculates how much audio to write to the WAVE file, given the
// audio format and the maximum duration of the WAVE file.
// The return value is the size of the desired audio clip in bytes
//-------------------------------------------------------------------

function TAudioClipClass.CalculateMaxAudioDataSize(pAudioType: IMFMediaType;     // The PCM audio format.
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

function TAudioClipClass.ConfigureAudioStream(out ppPCMAudio: IMFMediaType): HResult;
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
    hr := pReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                      0,
                                      pPartialType);

  // Get the complete uncompressed format.
  if SUCCEEDED(hr) then
    hr := pReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                      pUncompressedAudioType);

  // IMPORTANT NOTE:
  // Before selecting a stream, make sure you deselected all streams first!
  // If the stream is selected, the media source might hold onto a queue of unread data,
  // and the queue might grow indefinitely, consuming memory what will result in Error $8007000E.
  //
  // First deselect all streams
  if SUCCEEDED(hr) then
    hr := pReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                     False);

  // Second, selected the desired stream
  if SUCCEEDED(hr) then
    hr := pReader.SetStreamSelection(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                     True);

  // Return the PCM format to the caller.
  if SUCCEEDED(hr) then
    ppPCMAudio := pUncompressedAudioType;

  // In C/C++ we have to release the interfaces when going out of scope.
  // Delphi is automaticly taking care of that.

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

function TAudioClipClass.WriteWaveHeader(pMediaType: IMFMediaType;         // PCM audio format.
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
      header[4] := cbFormat;    // 4 bytes = size of the waveformatex structure.

      // The WaveFormatEx structure will be placed inbetween RIFF and Data header

      // data
      dataHeader[0] := FCC('data'); // 4 bytes
      dataHeader[1] := 0;           // 4 bytes

      // Write the RIFF header
      hr := WriteToFile(header,   // can be written directly
                        SizeOf(header));

      // Write the WAVEFORMATEX structure.
      if SUCCEEDED(hr) then
        hr := WriteToFile(pWav^,  // Dereference the pointer, to store the WAVEFORMATEX struct correctly
                          cbFormat);

      // Write the start of the 'data' chunk
      if SUCCEEDED(hr) then
        hr := WriteToFile(dataHeader, // can be written directly
                          SizeOf(dataHeader));

      if SUCCEEDED(hr) then
        pcbWritten := SizeOf(header) + cbFormat + SizeOf(dataHeader);

    end;

  CoTaskMemFree(pWav);
  Result := hr;
end;

//-------------------------------------------------------------------
// WriteWaveData
// Initiate the callback interface to decode PCM audio data from
// the source file and writes it to the WAVE file.
//-------------------------------------------------------------------
function TAudioClipClass.WriteWaveData(): HResult;
var
  hr: HResult;

begin
  // Read the first sample. this will start the callback OnReadSample
  hr := pReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                           0,
                           nil,
                           nil,
                           nil,
                           nil);

  if Failed(hr) then
    FinalizeClip(STATE_ERROR,
                 hr);
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

function TAudioClipClass.FixUpChunkSizes(): HResult;
var
  hr: HResult;
  ll: Int64;
  cbRiffFileSize: DWORD;

begin
  hr := S_OK;
  ll := (cbHeader - SizeOf(DWORD));

  if (SetFilePointerEx(hTargetFile,
                       ll,
                       Nil,
                       FILE_BEGIN) = False) then
    hr := HRESULT_FROM_WIN32(GetLastError());

  // Write the data size.
  if SUCCEEDED(hr) then
    hr := WriteToFile(cbAudioData,
                      SizeOf(cbAudioData));

  if SUCCEEDED(hr) then
    begin
      // Write the file size.
      ll := sizeof(FOURCC);

      if (SetFilePointerEx(hTargetFile,
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

      hr := WriteToFile(cbRiffFileSize,
                        SizeOf(cbRiffFileSize));
    end;
  Result := hr;
end;



//-------------------------------------------------------------------
//
// Writes a block of data to a file
//
// hTargetFile: Handle to the file.
// p: Pointer to the buffer to write.
// cb: Size of the buffer, in bytes.
//
//-------------------------------------------------------------------

function TAudioClipClass.WriteToFile(const p;
                                     cb: DWORD): HResult;
var
  hr: HResult;
  cbWritten: DWORD;
  bResult: BOOL;

begin

  cbWritten := 0;
  hr := S_OK;

  bResult := WriteFile(hTargetFile,
                       p,
                       cb,
                       cbWritten,
                       Nil);

  if not bResult then
    begin
      hr := HRESULT_FROM_WIN32(GetLastError());
      dwAudioDataWritten := 0;
    end
  else
    Inc(dwAudioDataWritten,
        cbWritten);

  // Send message to the caller, to update the progressbar (if any)
  SendMessage(hwCaller,
              WM_CLIPENGINE_MSG,
              WPARAM(1),
              0);
  Result := hr;
end;


function TAudioClipClass.GetDuration(): DWord;
var
  hr: HResult;
  ms: LONGLONG;
  pvar: PROPVARIANT;

begin
  PropVariantInit(pvar);

  hr := pReader.GetPresentationAttribute(DWord(MF_SOURCE_READER_MEDIASOURCE),
                                         MF_PD_DURATION,
                                         pvar);
  if Succeeded(hr) then
    begin
      hr := PropVariantToInt64(pvar,
                               ms);
      if Succeeded(hr) then
        Result := DWord(HnsTimeToMsec(ms))
      else
        Result := 0;
      PropVariantClear(pvar);
    end
  else
    Result := 0;
end;


//-------------------------------------------------------------------
// FinalizeClip
// Writes the header data and closes the targetfile after the callback interface
// processed the audiodata.
//-------------------------------------------------------------------
procedure TAudioClipClass.FinalizeClip(dwState: TCallBackState;
                                       rs: HResult);
var
  hr: HResult;

  function CloseTfHandle(): HResult;
  begin
    Result := S_OK;
    if (hTargetFile <> INVALID_HANDLE_VALUE) then
      if CloseHandle(hTargetFile) then
        begin
          hTargetFile := INVALID_HANDLE_VALUE;
          Result := S_OK;
        end
      else
        Result := ERROR_INVALID_HANDLE_STATE;
  end;

begin
  hr := S_OK;

  // Flush the SourceReader's streams.
  // The Flush method discards all queued samples and cancels all pending sample requests.
  // This method can complete either synchronously or asynchronously.
  if not bSourceReaderFlushed then
    hr := pReader.Flush(MF_SOURCE_READER_ALL_STREAMS);

  // Note:
  //  Remember Flush will invoke the IMFSoureReaderCallBack.OnFlush method directly.
  //  So, when calling Flush, you should consider where to handel the message (here, or within the OnFlush method).
  //  It should be obvious, we do it here ;-)

  case dwState of
    STATE_END_OF_STREAM,
    STATE_END_OF_CLIP:   begin
                           // Fix up the RIFF headers with the correct sizes.
                           hr := FixUpChunkSizes();
                           if Succeeded(hr) then
                           // Close the targetfile
                           hr := CloseTfHandle();
                           dwAudioDataWritten := 0;
                         end;

    STATE_ERROR:         begin
                           // Clean up: Close file handle.
                           hr := CloseTfHandle();
                           // handle exception, if needed, like: Deleting the targetfile.
                         end;
    STATE_ABORT:         begin
                           // Clean up: Close file handle.
                           hr := CloseTfHandle();
                           // Delete the targetfile
                           if DeleteFile(string(wcTargetFile)) then
                             hr := S_OK;
                           dwAudioDataWritten := 0;
                         end;
  end;

  // Send a message to the caller we are done.
  if Succeeded(hr) then
    SendMessage(hwCaller,
                WM_CLIPENGINE_MSG,
                WPARAM(2),
                LPARAM(rs))
  else
    SendMessage(hwCaller,
                WM_CLIPENGINE_MSG,
                WPARAM(2),
                LPARAM(hr));

end;


// Handles internal messages
procedure TAudioClipClass.WndProc(var Msg: TMessage);
begin

try
  if (Msg.Msg = WM_USER_ABORT) then // User canceled operation
    begin
      // Handle the abort operation
      // This will ends the callback and calls FinalizeClip method
      CallBackState :=  STATE_ABORT;
      pReader.Flush(MF_SOURCE_READER_ALL_STREAMS);
    end
  else // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
       // NOTE: The first parameter, FHWnd, is the handle of the class's invisible window receiving this message.
       //       It is obtained from the call to AllocateHWnd in the Constructor.
    msg.Result := DefWindowProc(FHWnd,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);

except
  on E : Exception do
   ShowMessage(format('%s error raised, with message %s',
                      [E.ClassName, E.Message]));
end;
end;

end.

