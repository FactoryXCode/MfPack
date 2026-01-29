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
// Revision Version: 3.1.9
//
// Description:
//   This application demonstrates using the Media Foundation
//   source reader to extract decoded audio from an audio/video file.
//
//   The application reads audio data from an input file and writes
//   uncompressed PCM audio to a WAVE file.
//
//   The input file must be a media format supported by Media Foundation,
//   and must have an audio stream. The audio stream can be an encoded
//   format, such as Windows Media Audio.
//   Note: The original application was a console app. running in synchronous mode.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: >= MfPackX319
// Known Issues: -
//
// Compiler version: 28 up to 36
// SDK version: 10.0.26100.4654
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
  WinApi.ActiveX,
  WinApi.ActiveX.PropIdl,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  {Project}
  Helpers;

type
  TAudioClipCompleteEvent = procedure(Sender: TObject; Success: Boolean; HResultCode: HResult) of object;

  TAudioClipClass = class(TInterfacedPersistent, IMFSourceReaderCallback)
  private
    FSourceFile: string;
    FOutputFile: string;
    FReader: IMFSourceReader;
    FSinkWriter: IMFSinkWriter;
    FOutStreamIndex: DWORD;
    FDuration100ns: UInt64;
    FCancelHandle: THandle;
    FDoneEvent: TEvent;
    FOnComplete: TAudioClipCompleteEvent;
    FSamplePriorityMS: Integer;

    FProgressPercent: Integer;
    FProgressBytes: Int64;

    FCritSec: TMFCritSec;

    procedure SignalDone(const hr: HResult);
    function ReadDurationFromReader(): HRESULT;
    function WriteSampleToSink(pSample: IMFSample): HRESULT;
    procedure ReportProgressFromSample(pSample: IMFSample);

  protected
    // IMFSourceReaderCallback
    function OnReadSample(hrStatus: HRESULT;
                          dwStreamIndex: DWORD;
                          dwStreamFlags: DWORD;
                          llTimestamp: LONGLONG;
                          pSample: IMFSample): HResult; stdcall;

    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HResult; stdcall;

    function OnFlush(dwStreamIndex: DWORD): HResult; stdcall;

  public
    constructor Create();
    destructor Destroy; override;

    function ExtractSoundClip(CancelHandle: THandle;
                              OnComplete: TAudioClipCompleteEvent): HResult;

    property SourceFile: string read FSourceFile write FSourceFile;
    property OutputFile: string read FOutputFile write FOutputFile;
    property SamplingPriority: Integer read FSamplePriorityMS write FSamplePriorityMS;
    property Duration: UInt64 read FDuration100ns write FDuration100ns;
    property ProgressPercent: Integer read FProgressPercent;
    property ProgressBytes: Int64 read FProgressBytes;

  end;

implementation

uses
  System.Math;

{ TAudioClipClass }

constructor TAudioClipClass.Create();
begin
  inherited Create;

  FOutStreamIndex := DWORD($FFFFFFFF);
  FDuration100ns := 0;
  FCancelHandle := 0;
  FDoneEvent := TEvent.Create(nil,
                              True,
                              False,
                              '');
  // Initialize bytes counters
  FProgressBytes := 0;

  // Create CriticalSection
  FCritSec := TMFCritSec.Create;
end;


destructor TAudioClipClass.Destroy;
begin
  // Release interfaces
  SafeRelease(FReader);
  SafeRelease(FSinkWriter);

  if Assigned(FCritSec) then
    FreeAndnil(FCritSec);

  if Assigned(FDoneEvent) then
    FreeAndNil(FDoneEvent);

  FOnComplete := nil;

  FCritSec := nil;

  inherited;
end;


procedure TAudioClipClass.SignalDone(const hr: HResult);
begin
  // call completion callback (catch exceptions)
  if Assigned(FOnComplete) then
  begin
    try
      FOnComplete(Self,
                  Succeeded(hr),
                  hr);
    except
      // swallow exceptions
    end;
  end;

  if Assigned(FDoneEvent) then
    FDoneEvent.SetEvent;
end;


function TAudioClipClass.ReadDurationFromReader(): HRESULT;
begin
  // Uses helper from Helpers unit MfMetLib.pas (GetFileDuration)
  Result := GetFileDuration(FReader,
                            FDuration100ns);
end;

// Write sample to sink writer.
function TAudioClipClass.WriteSampleToSink(pSample: IMFSample): HRESULT;
var
  hr: HRESULT;
  sinkRef: IMFSinkWriter;

begin

  // Keep a local strong reference to avoid lifetime issues
  sinkRef := FSinkWriter;
  if not Assigned(sinkRef) then
    Exit(E_FAIL);

  // Write the sample (this gives the sink writer the sample)
  hr := sinkRef.WriteSample(FOutStreamIndex, pSample);
  if FAILED(hr) then
    Exit(hr);

  Result := S_OK;
end;


// Updates progress and written data (in bytes).
// Note: These properties should be called from the main thread to prevent a
// non responsive user interface.
procedure TAudioClipClass.ReportProgressFromSample(pSample: IMFSample);
var
  hr : HResult;
  llTime: LONGLONG;
  stats: MF_SINK_WRITER_STATISTICS;

begin

  if (FDuration100ns = 0) or (not Assigned(pSample)) then
    Exit;

  if Succeeded(pSample.GetSampleTime(@llTime)) then
    begin
      FProgressPercent := Round((llTime / FDuration100ns) * 100);
      if (FProgressPercent < 0) then
        FProgressPercent := 0;
      if (FProgressPercent > 100) then
        FProgressPercent := 100;
    end;

  if Assigned(FSinkWriter) then
    begin
      ZeroMemory(@stats,
                 SizeOf(stats));
      stats.cb := SizeOf(stats);
      hr := FSinkWriter.GetStatistics(FOutStreamIndex,
                                      stats);
      if Succeeded(hr) then
        FProgressBytes := stats.qwByteCountProcessed
      else
        FProgressBytes := 0;
    end;

end;


function TAudioClipClass.ExtractSoundClip(CancelHandle: THandle;
                                          OnComplete: TAudioClipCompleteEvent): HResult;
var
  hr: HResult;
  pAttr: IMFAttributes;
  pAudioNative: IMFMediaType;
  pTargetType: IMFMediaType;
  pReaderReqType: IMFMediaType;
  outIndex: DWORD;

begin

  // Set callbacks and cancel handle
  FOnComplete := OnComplete;
  FCancelHandle := CancelHandle;

  // Reset counters
  FProgressBytes := 0;

  // MFStartup: application may call globally; safe to call here if not started
  hr := InitMF();
  if Failed(hr) then
    Exit(hr);

  // Create attributes and set our callback object
  pAttr := nil;
  hr := MFCreateAttributes(pAttr,
                           2);
  if Failed(hr) then
    Exit(hr);

  // Link the callback interface with the source reader
  // 1 Link the callback interface with the sourcereader
  hr := pAttr.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,
                         Self as IMfSourceReaderCallback);
  if Failed(hr) then
    Exit(hr);

  // 2 Enable hardware transforms
  if Succeeded(hr) then
    hr := pAttr.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
                          1 {True} );
  if Failed(hr) then
    Exit(hr);

  // Create source reader with attributes (reader holds callback ref)
  FReader := nil;

  hr := MFCreateSourceReaderFromURL(PWideChar(WideString(FSourceFile)),
                                    pAttr,
                                    FReader);
  // Release local attr
  pAttr := nil;
  if Failed(hr) then
    Exit(hr);

  // Release previous sinkwriter and create a new one.
  SafeRelease(FSinkWriter);

  hr := MFCreateSinkWriterFromURL(PWideChar(WideString(FOutputFile)),
                                  nil,
                                  nil,
                                  FSinkWriter);
  if Failed(hr) then
    Exit(hr);

  // Create PCM target media type for sink
  pTargetType := nil;
  hr := MFCreateMediaType(pTargetType);
  if Failed(hr) then
    Exit(hr);

  pTargetType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  pTargetType.SetGUID(MF_MT_SUBTYPE,
                      MFAudioFormat_PCM);
  pTargetType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        2);
  pTargetType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        44100);
  pTargetType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        16);
  pTargetType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                        4);
  pTargetType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                        44100 * 4);

  outIndex := 0;
  hr := FSinkWriter.AddStream(pTargetType,
                              outIndex);
  if Failed(hr) then
    Exit(hr);
  FOutStreamIndex := outIndex;

  // Attempt to set reader to PCM request (best effort)
  pAudioNative := nil;
  if Succeeded(FReader.GetNativeMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                          0,
                                          @pAudioNative)) then
    begin
      pReaderReqType := nil;

      if Succeeded(MFCreateMediaType(pReaderReqType)) then
        begin
          pReaderReqType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Audio);
          pReaderReqType.SetGUID(MF_MT_SUBTYPE,
                                 MFAudioFormat_PCM);
          pReaderReqType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                   2);
          pReaderReqType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                   44100);
          pReaderReqType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                   16);

          // ignore failure of SetCurrentMediaType
          FReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                      0,
                                      pReaderReqType);
         // Skip video streams to avoid memory overrun.
         hr := SetSafeStream(FReader,
                             MF_SOURCE_READER_FIRST_AUDIO_STREAM);
         if Failed(hr) then
           Exit(hr);

          pReaderReqType := nil;
        end;
      pAudioNative := nil;
    end;

  // Set sink writer input media type
  hr := FSinkWriter.SetInputMediaType(FOutStreamIndex,
                                      pTargetType,
                                      nil);
  if Failed(hr) then
    Exit(hr);

  // Begin writing
  hr := FSinkWriter.BeginWriting;
  if Failed(hr) then
    Exit(hr);

  // Read duration (sets FDuration100ns)
  ReadDurationFromReader();

    // Kick off first async read
  hr := FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                           0,
                           nil,
                           nil,
                           nil,
                           nil);
  if Failed(hr) then
    begin
      SignalDone(hr);
      Exit(hr);
    end;

  // Wait for completion
  if Assigned(FDoneEvent) then
    FDoneEvent.WaitFor(INFINITE);

  // Finalize sink writer if not already done
  try
    if Assigned(FSinkWriter) then
      begin
        FSinkWriter.Finalize;
      end;
  except
    // ignore
  end;

  if Assigned(FReader) then
    FReader.Flush(MF_SOURCE_READER_ALL_STREAMS);

  Result := S_OK;
end;


{ IMFSourceReaderCallback }

function TAudioClipClass.OnReadSample(hrStatus: HRESULT;
                                      dwStreamIndex: DWORD;
                                      dwStreamFlags: DWORD;
                                      llTimestamp: LONGLONG;
                                      pSample: IMFSample): HResult;
var
  hr: HResult;

label
  done;

begin
  FCritSec.Lock();
  hr := hrStatus;

  // Throttle to use less CPU load.
  Sleep(FSamplePriorityMS);

  if Failed(hrStatus) then
    begin
      SignalDone(hrStatus);
      goto done
    end;

  if Assigned(pSample) then
    begin

      hr := WriteSampleToSink(pSample);

      if Failed(hr) then
        begin
          SignalDone(hr);
          goto done;
        end;

      // Progress
      ReportProgressFromSample(pSample);

    end;

  if (dwStreamFlags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0 then
    begin
      SignalDone(E_FAIL);
      hr := E_FAIL;
      goto done;
    end;

  if (dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0 then
    begin
      try
        if Assigned(FSinkWriter) then
          begin
            FSinkWriter.NotifyEndOfSegment(FOutStreamIndex);
            FSinkWriter.Finalize;
          end;
      except
      //
      end;

      SignalDone(S_OK);
      hr := S_OK;
      goto done;
    end;

  // If the sample is nil or we get MF_SOURCE_READERF_STREAMTICK, there is a gap in the data stream that can't be filled; No reason to quit though..
  if ((dwStreamFlags and MF_SOURCE_READERF_STREAMTICK) <> 0) or (pSample = nil) then
    begin
      if Assigned(FReader) then
        // Read the next sample.
        hr := FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                                 0,
                                 nil,
                                 nil,
                                 nil,
                                 nil);
      goto done;
    end;

  // Cancellation
  if (FCancelHandle <> 0) and (WaitForSingleObject(FCancelHandle, 0) = WAIT_OBJECT_0) then
  begin
    SignalDone(HRESULT($80004004)); // E_ABORT
    hr := HRESULT($80004004);
    goto done;
  end;


  // request next sample
  hr := FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,
                           0,
                           nil,
                           nil,
                           nil,
                           nil);

done:
  if Assigned(pSample) then
    pSample := nil;

  FCritSec.UnLock();
  Result := hr;
end;


function TAudioClipClass.OnEvent(dwStreamIndex: DWORD;
                                 pEvent: IMFMediaEvent): HResult;
begin
  // optional: inspect events
  Result := S_OK;
  pEvent := nil;
end;


function TAudioClipClass.OnFlush(dwStreamIndex: DWORD): HResult;
begin

  Result := S_OK;
end;


end.
