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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: >= MfPackX400
// Known Issues: -
//
// Compiler version: 28 up to 36
// SDK version: 10.0.28000.2705
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
    FStatus: HResult;
    FCompleted: Boolean;
    FClosing: Boolean;
    FFlushEvent: THandle;
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
  FFlushEvent:=CreateEvent(nil,True,False,nil);
  if FFlushEvent=0 then RaiseLastOSError;
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

  if FFlushEvent<>0 then CloseHandle(FFlushEvent);
  inherited;
end;


procedure TAudioClipClass.SignalDone(const hr: HResult);
begin
 if FCompleted then Exit;
 FCompleted:=True;FStatus:=hr;
 FDoneEvent.SetEvent;
end;


function TAudioClipClass.ReadDurationFromReader(): HRESULT;
var DurationValue: Int64;
begin
  // Uses helper from Helpers unit MfMetLib.pas (GetFileDuration)
  Result := GetFileDuration(FReader,DurationValue);
  if SUCCEEDED(Result) and (DurationValue>=0) then FDuration100ns:=UInt64(DurationValue);
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


function TAudioClipClass.ExtractSoundClip(CancelHandle: THandle;OnComplete: TAudioClipCompleteEvent): HResult;
var HR,FinalHR: HResult; Attributes: IMFAttributes; PCM,Actual: IMFMediaType;
    OutputCreated: Boolean;
begin
 Result:=E_INVALIDARG;
 if (FSourceFile='') or (FOutputFile='') then Exit;
 if SameText(ExpandFileName(FSourceFile),ExpandFileName(FOutputFile)) then Exit;
 HR:=MFStartup(MF_VERSION);if FAILED(HR) then Exit(HR);
 OutputCreated:=False;FClosing:=False;FCompleted:=False;FStatus:=E_PENDING;
 FCancelHandle:=CancelHandle;FProgressBytes:=0;FProgressPercent:=0;FDuration100ns:=0;
 FDoneEvent.ResetEvent;
 try
  HR:=MFCreateAttributes(Attributes,1);
  if SUCCEEDED(HR) then HR:=Attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK,Self);
  if SUCCEEDED(HR) then HR:=MFCreateSourceReaderFromURL(PWideChar(UnicodeString(FSourceFile)),Attributes,FReader);
  if SUCCEEDED(HR) then HR:=FReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,False);
  if SUCCEEDED(HR) then HR:=FReader.SetStreamSelection(MF_SOURCE_READER_FIRST_AUDIO_STREAM,True);
  if SUCCEEDED(HR) then HR:=MFCreateMediaType(PCM);
  if SUCCEEDED(HR) then HR:=PCM.SetGUID(MF_MT_MAJOR_TYPE,MFMediaType_Audio);
  if SUCCEEDED(HR) then HR:=PCM.SetGUID(MF_MT_SUBTYPE,MFAudioFormat_PCM);
  if SUCCEEDED(HR) then HR:=PCM.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,2);
  if SUCCEEDED(HR) then HR:=PCM.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,44100);
  if SUCCEEDED(HR) then HR:=PCM.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,16);
  if SUCCEEDED(HR) then HR:=PCM.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,4);
  if SUCCEEDED(HR) then HR:=PCM.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,44100*4);
  if SUCCEEDED(HR) then HR:=FReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,0,PCM);
  if SUCCEEDED(HR) then HR:=FReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_AUDIO_STREAM,@Actual);
  if SUCCEEDED(HR) then begin
   HR:=MFCreateSinkWriterFromURL(PWideChar(UnicodeString(FOutputFile)),nil,nil,FSinkWriter);
   OutputCreated:=SUCCEEDED(HR);
  end;
  if SUCCEEDED(HR) then HR:=FSinkWriter.AddStream(Actual,FOutStreamIndex);
  if SUCCEEDED(HR) then HR:=FSinkWriter.SetInputMediaType(FOutStreamIndex,Actual,nil);
  if SUCCEEDED(HR) then HR:=FSinkWriter.BeginWriting;
  if SUCCEEDED(HR) then begin
   ReadDurationFromReader;
   if (CancelHandle<>0) and (WaitForSingleObject(CancelHandle,0)=WAIT_OBJECT_0) then HR:=E_ABORT
   else HR:=FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,0,nil,nil,nil,nil);
  end;
  if SUCCEEDED(HR) then begin
   while FDoneEvent.WaitFor(50)<>wrSignaled do
    if (CancelHandle<>0) and (WaitForSingleObject(CancelHandle,0)=WAIT_OBJECT_0) then begin HR:=E_ABORT;Break;end;
   if SUCCEEDED(HR) then HR:=FStatus;
  end;
  FCritSec.Lock;
  try FClosing:=True;finally FCritSec.Unlock;end;
  if FReader<>nil then begin
   ResetEvent(FFlushEvent);
   if SUCCEEDED(FReader.Flush(MF_SOURCE_READER_ALL_STREAMS)) then WaitForSingleObject(FFlushEvent,5000);
  end;
  FReader:=nil;
  if FSinkWriter<>nil then begin
   FinalHR:=FSinkWriter.Finalize;
   if SUCCEEDED(HR) then HR:=FinalHR;
  end;
  FSinkWriter:=nil;
  if FAILED(HR) and OutputCreated then DeleteFile(PWideChar(UnicodeString(FOutputFile)));
  if SUCCEEDED(HR) then FProgressPercent:=100;
  Result:=HR;
 finally
  Attributes:=nil;PCM:=nil;Actual:=nil;FReader:=nil;FSinkWriter:=nil;
  MFShutdown;
 end;
 if Assigned(OnComplete) then OnComplete(Self,SUCCEEDED(Result),Result);
end;


{ IMFSourceReaderCallback }

function TAudioClipClass.OnReadSample(hrStatus: HRESULT;dwStreamIndex,dwStreamFlags: DWORD;llTimestamp: LONGLONG;pSample: IMFSample): HResult;
var HR: HResult;
begin
 Result:=S_OK;FCritSec.Lock;
 try
  if FClosing or FCompleted then Exit;
  HR:=hrStatus;
  if SUCCEEDED(HR) and Assigned(pSample) then begin
   HR:=WriteSampleToSink(pSample);
   if SUCCEEDED(HR) then ReportProgressFromSample(pSample);
  end;
  if FAILED(HR) then begin SignalDone(HR);Exit;end;
  if (dwStreamFlags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED)<>0 then begin SignalDone(E_FAIL);Exit;end;
  if (dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM)<>0 then begin SignalDone(S_OK);Exit;end;
  if FSamplePriorityMS>0 then Sleep(FSamplePriorityMS);
  if (FCancelHandle<>0) and (WaitForSingleObject(FCancelHandle,0)=WAIT_OBJECT_0) then begin SignalDone(E_ABORT);Exit;end;
  HR:=FReader.ReadSample(MF_SOURCE_READER_FIRST_AUDIO_STREAM,0,nil,nil,nil,nil);
  if FAILED(HR) then SignalDone(HR);
 finally FCritSec.Unlock;end;
end;


function TAudioClipClass.OnEvent(dwStreamIndex: DWORD;
                                 pEvent: IMFMediaEvent): HResult;
begin
  // optional: inspect events
  Result := S_OK;
  pEvent := nil;
end;


function TAudioClipClass.OnFlush(dwStreamIndex: DWORD): HResult;
begin SetEvent(FFlushEvent);Result:=S_OK;end;


end.
