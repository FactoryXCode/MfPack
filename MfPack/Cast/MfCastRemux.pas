// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastRemux.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Separate remuxing from full transcoding so compatible MKV codecs
//              can avoid video re-encoding.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: -
//
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
unit MfCastRemux;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  {Cast/Media}
  MfCastTypes,
  MfCastInterfaces,
  MfCastMediaInterfaces;

type
  TMfCastRemuxPipeline = class(TInterfacedObject, IMfCastRemuxPipeline)
  private
    FLogger: IMfCastLogger;
    FState: TMfCastState;
    FPublisher: IMfCastSegmentPublisher;
    FByteStream: IMFByteStream;
    FRequest: TMfCastRemuxRequest;
    FWorker: TThread;
    FWorkerResult: HRESULT;

    procedure Log(const ALevel: TMfCastLogLevel;
                  const AMessage: string);
    procedure SetState(const AState: TMfCastState);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure SetLogger(const ALogger: IMfCastLogger);

    function Start(const ARequest: TMfCastRemuxRequest;
                   const APublisher: IMfCastSegmentPublisher): HRESULT;

    function Pause(): HRESULT;
    function Resume(): HRESULT;
    function Stop(): HRESULT;
    function GetState(): TMfCastState;
  end;

implementation

uses

  {WinApi}
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib;

type
  TMfCastRemuxWorker = class(TThread)
  private
    FOwner: TMfCastRemuxPipeline;
    FPaused: Integer;

    function FindStreams(const AReader: IMFSourceReader;
                         out AVideoStream: DWORD;
                         out AVideoType: IMFMediaType;
                         out AHasAudio: Boolean;
                         out AAudioStream: DWORD;
                         out AAudioType: IMFMediaType): HRESULT;

    function SeekReader(const AReader: IMFSourceReader;
                        const APosition100ns: Int64): HRESULT;

    function CreateWriter(const AVideoType: IMFMediaType;
                          const AHasAudio: Boolean;
                          const AAudioType: IMFMediaType;
                          out AWriter: IMFSinkWriter;
                          out AVideoOutputStream: DWORD;
                          out AAudioOutputStream: DWORD): HRESULT;

    function RunRemux(): HRESULT;
    function WaitWhilePaused(): Boolean;
    procedure Pace(const ASampleTime100ns: Int64;
                   const AStartTick: Cardinal);
  protected
    procedure Execute(); override;

  public

    constructor Create(const AOwner: TMfCastRemuxPipeline);

    procedure PauseRemux();
    procedure ResumeRemux();
  end;


constructor TMfCastRemuxWorker.Create(const AOwner: TMfCastRemuxPipeline);
begin

  inherited Create(True);
  FreeOnTerminate := False;
  Priority := tpLower;
  FOwner := AOwner;
  FPaused := 0;
end;


procedure TMfCastRemuxWorker.PauseRemux();
begin

  InterlockedExchange(FPaused,
                      1);
end;


procedure TMfCastRemuxWorker.ResumeRemux();
begin

  InterlockedExchange(FPaused,
                      0);
end;


function TMfCastRemuxWorker.WaitWhilePaused(): Boolean;
begin

  while (InterlockedCompareExchange(FPaused,
                                    0,
                                    0) <> 0) and (not Terminated) do
    Sleep(20);
  Result := not Terminated;
end;


procedure TMfCastRemuxWorker.Pace(const ASampleTime100ns: Int64;
                                  const AStartTick: Cardinal);
var
  TargetMs: Int64;
  ElapsedMs: Cardinal;
  DelayMs: Int64;

begin

  TargetMs := ASampleTime100ns div 10000;

  if (TargetMs <= 0) then
    Exit;

  repeat
    ElapsedMs := GetTickCount() - AStartTick;
    DelayMs := TargetMs - Int64(ElapsedMs);

    if (DelayMs <= 100) then
      Exit;

    if (DelayMs > 50) then
      Sleep(50)
    else
      Sleep(DWORD(DelayMs));

  until Terminated;
end;



function TMfCastRemuxWorker.FindStreams(const AReader: IMFSourceReader;
                                        out AVideoStream: DWORD;
                                        out AVideoType: IMFMediaType;
                                        out AHasAudio: Boolean;
                                        out AAudioStream: DWORD;
                                        out AAudioType: IMFMediaType): HRESULT;
var
  StreamCount: DWORD;
  StreamIndex: DWORD;
  MediaType: IMFMediaType;
  MajorType: TGUID;
  Subtype: TGUID;
  AnyAudio: Boolean;

begin

  AVideoStream := 0;
  AVideoType := nil;
  AHasAudio := False;
  AAudioStream := 0;
  AAudioType := nil;
  AnyAudio := False;

  if not Assigned(AReader) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  StreamCount := CountSourceReaderStreams(AReader);
  if (StreamCount = 0) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  for StreamIndex := 0 to StreamCount - 1 do
    begin
      MediaType := nil;

      if FAILED(AReader.GetNativeMediaType(StreamIndex,
                                           0,
                                           @MediaType)) or
         (not Assigned(MediaType)) or
         FAILED(MediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                  MajorType)) or
         FAILED(MediaType.GetGUID(MF_MT_SUBTYPE,
                                  Subtype)) then
        Continue;

      if IsEqualGUID(MajorType,
                     MFMediaType_Video) and
         IsEqualGUID(Subtype,
                     MFVideoFormat_H264) and
         (not Assigned(AVideoType)) and
         ((not FOwner.FRequest.HasVideoStreamIndex) or
          (FOwner.FRequest.VideoStreamIndex = StreamIndex)) then
        begin
          AVideoStream := StreamIndex;
          AVideoType := MediaType;
        end
      else
        if IsEqualGUID(MajorType,
                       MFMediaType_Audio) then
          begin
            AnyAudio := True;

            if IsEqualGUID(Subtype,
                           MFAudioFormat_AAC) and
             (not Assigned(AAudioType)) and
             ((not FOwner.FRequest.HasAudioStreamIndex) or
              (FOwner.FRequest.AudioStreamIndex = StreamIndex)) then
            begin
              AAudioStream := StreamIndex;
              AAudioType := MediaType;
            end;
          end;
    end;

  if not Assigned(AVideoType) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  if FOwner.FRequest.HasAudioStreamIndex and (not Assigned(AAudioType)) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  if AnyAudio and (not Assigned(AAudioType)) then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  AHasAudio := Assigned(AAudioType);
  Result := S_OK;
end;


function TMfCastRemuxWorker.SeekReader(const AReader: IMFSourceReader;
                                       const APosition100ns: Int64): HRESULT;
var
  Position: PROPVARIANT;

begin

  if (APosition100ns <= 0) then
    begin
      Result := S_OK;
      Exit;
    end;

  PropVariantInit(Position);
  try
    Position.vt := VT_I8;
    Position.hVal.QuadPart := APosition100ns;
    Result := AReader.SetCurrentPosition(GUID_NULL,
                                         Position);
  finally
    PropVariantClear(Position);
  end;
end;


function TMfCastRemuxWorker.CreateWriter(const AVideoType: IMFMediaType;
                                         const AHasAudio: Boolean;
                                         const AAudioType: IMFMediaType;
                                         out AWriter: IMFSinkWriter;
                                         out AVideoOutputStream: DWORD;
                                         out AAudioOutputStream: DWORD): HRESULT;
var
  Attributes: IMFAttributes;

begin

  AWriter := nil;
  AVideoOutputStream := 0;
  AAudioOutputStream := 0;
  Attributes := nil;

  Result := MFCreateAttributes(Attributes,
                               3);
  if FAILED(Result) then
    Exit;

  Result := Attributes.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                               MFTranscodeContainerType_FMPEG4);
  if FAILED(Result) then
    Exit;

  Result := Attributes.SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING,
                                 UINT32(True));
  if FAILED(Result) then
    Exit;

  Result := Attributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
                                 UINT32(False));
  if FAILED(Result) then
    Exit;

  Result := MFCreateSinkWriterFromURL(PWideChar(WideString('mfcast-remux.mp4')),
                                      FOwner.FByteStream,
                                      Attributes,
                                      AWriter);
  if FAILED(Result) then
    Exit;

  Result := AWriter.AddStream(AVideoType,
                              AVideoOutputStream);
  if FAILED(Result) then
    Exit;

  Result := AWriter.SetInputMediaType(AVideoOutputStream,
                                      AVideoType,
                                      nil);
  if FAILED(Result) then
    Exit;

  if AHasAudio then
    begin
      Result := AWriter.AddStream(AAudioType,
                                  AAudioOutputStream);
      if FAILED(Result) then
        Exit;

      Result := AWriter.SetInputMediaType(AAudioOutputStream,
                                          AAudioType,
                                          nil);
      if FAILED(Result) then
        Exit;
    end;

  Result := AWriter.BeginWriting();
end;


function TMfCastRemuxWorker.RunRemux(): HRESULT;
var
  Reader: IMFSourceReader;
  Writer: IMFSinkWriter;
  VideoType: IMFMediaType;
  AudioType: IMFMediaType;
  Sample: IMFSample;
  VideoStream: DWORD;
  AudioStream: DWORD;
  VideoOutputStream: DWORD;
  AudioOutputStream: DWORD;
  ActualStream: DWORD;
  Flags: DWORD;
  Timestamp: LONGLONG;
  SampleTime: LONGLONG;
  OutputTime: LONGLONG;
  DecodeTime: UINT64;
  TimelineOrigin: LONGLONG;
  StartTick: Cardinal;
  HasAudio: Boolean;
  VideoDone: Boolean;
  AudioDone: Boolean;

begin

  Reader := nil;
  Writer := nil;
  VideoType := nil;
  AudioType := nil;
  Sample := nil;

  Result := MFCreateSourceReaderFromURL(PWideChar(WideString(FOwner.FRequest.SourceName)),
                                        nil,
                                        Reader);
  if FAILED(Result) then
    Exit;

  Result := FindStreams(Reader,
                        VideoStream,
                        VideoType,
                        HasAudio,
                        AudioStream,
                        AudioType);
  if FAILED(Result) then
    Exit;

  Result := SetSafeStream(Reader,
                          VideoStream);
  if FAILED(Result) then
    Exit;

  if HasAudio then
    begin
      Result := Reader.SetStreamSelection(AudioStream,
                                          True);
      if FAILED(Result) then
        Exit;
    end;

  Result := Reader.SetCurrentMediaType(VideoStream,
                                       0,
                                       VideoType);
  if FAILED(Result) then
    Exit;

  if HasAudio then
    begin
      Result := Reader.SetCurrentMediaType(AudioStream,
                                           0,
                                           AudioType);
      if FAILED(Result) then
        Exit;
    end;

  Result := SeekReader(Reader,
                       FOwner.FRequest.StartTime100ns);
  if FAILED(Result) then
    Exit;

  Result := CreateWriter(VideoType,
                         HasAudio,
                         AudioType,
                         Writer,
                         VideoOutputStream,
                         AudioOutputStream);
  if FAILED(Result) then
    Exit;

  TimelineOrigin := -1;
  StartTick := GetTickCount();
  VideoDone := False;
  AudioDone := not HasAudio;

  while (not Terminated) and (not (VideoDone and AudioDone)) do
    begin
      if not WaitWhilePaused() then
        Break;

      Sample := nil;
      ActualStream := 0;
      Flags := 0;
      Timestamp := 0;
      Result := Reader.ReadSample(MF_SOURCE_READER_ANY_STREAM,
                                  0,
                                  @ActualStream,
                                  @Flags,
                                  @Timestamp,
                                  @Sample);
      if FAILED(Result) then
        Exit;

      if ((Flags and DWORD(MF_SOURCE_READERF_ERROR)) <> 0) then
        begin
          Result := E_FAIL;
          Exit;
        end;

      if ((Flags and DWORD(MF_SOURCE_READERF_ENDOFSTREAM)) <> 0) then
        begin
          if (ActualStream = VideoStream) then
            VideoDone := True
          else
            if HasAudio and (ActualStream = AudioStream) then
              AudioDone := True;

          Continue;
        end;

      if ((Flags and (DWORD(MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED) or
                     DWORD(MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED))) <> 0) then
        begin
          Result := MF_E_INVALIDMEDIATYPE;
          Exit;
        end;

      if not Assigned(Sample) then
        begin
          if ((Flags and DWORD(MF_SOURCE_READERF_STREAMTICK)) <> 0) then
            begin
              if (TimelineOrigin < 0) then
                TimelineOrigin := Timestamp;
              OutputTime := Timestamp - TimelineOrigin;

              if (OutputTime < 0) then
                OutputTime := 0;

              if (ActualStream = VideoStream) then
                Result := Writer.SendStreamTick(VideoOutputStream,
                                                OutputTime)
              else
                if HasAudio and (ActualStream = AudioStream) then
                  Result := Writer.SendStreamTick(AudioOutputStream,
                                                  OutputTime);
              if FAILED(Result) then
                Exit;
            end;

          Continue;
        end;

      SampleTime := Timestamp;
      Sample.GetSampleTime(@SampleTime);

      if (TimelineOrigin < 0) then
        TimelineOrigin := SampleTime;

      OutputTime := SampleTime - TimelineOrigin;

      if (OutputTime < 0) then
        OutputTime := 0;

      Sample.SetSampleTime(OutputTime);

      DecodeTime := 0;
      if SUCCEEDED(Sample.GetUINT64(MFSampleExtension_DecodeTimestamp,
                                    DecodeTime)) then
        begin
          if (Int64(DecodeTime) > TimelineOrigin) then
            DecodeTime := UINT64(Int64(DecodeTime) - TimelineOrigin)
          else
            DecodeTime := 0;

          Sample.SetUINT64(MFSampleExtension_DecodeTimestamp,
                           DecodeTime);
        end;

      Pace(OutputTime, StartTick);
      if Terminated then
        Break;

      if (ActualStream = VideoStream) then
        Result := Writer.WriteSample(VideoOutputStream,
                                     Sample)
      else
        if HasAudio and (ActualStream = AudioStream) then
          Result := Writer.WriteSample(AudioOutputStream,
                                       Sample)
        else
          Result := S_OK;
      if FAILED(Result) then
        Exit;
    end;

  if Terminated then
    Result := E_ABORT
  else
    Result := Writer.Finalize();
end;


procedure TMfCastRemuxWorker.Execute();
var
  HrCom: HRESULT;
  ComInitialized: Boolean;
  Cancelled: Boolean;

begin

  if not Assigned(FOwner) then
    Exit;

  ComInitialized := False;
  HrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);
  if SUCCEEDED(HrCom) then
    ComInitialized := True
  else
    if (HrCom <> RPC_E_CHANGED_MODE) then
      begin
        FOwner.FWorkerResult := HrCom;
        FOwner.SetState(csError);
        Exit;
      end;

  try
    FOwner.Log(cllInfo,
               Format('Starting MKV remux without codec re-encoding: source="%s" start100ns=%d',
                      [FOwner.FRequest.SourceName, FOwner.FRequest.StartTime100ns]));
    try
      FOwner.FWorkerResult := RunRemux();
    except
      on E: Exception do
        begin
          FOwner.Log(cllError,
                     'Remux exception: ' + E.Message);
          FOwner.FWorkerResult := E_FAIL;
        end;
    end;

    Cancelled := Terminated and (FOwner.FWorkerResult = E_ABORT);

    if Assigned(FOwner.FPublisher) then
      begin
        if SUCCEEDED(FOwner.FWorkerResult) and (not Cancelled) then
          FOwner.FPublisher.CompletePresentation()
        else
          FOwner.FPublisher.AbortPresentation(FOwner.FWorkerResult);
      end;

    if Cancelled or SUCCEEDED(FOwner.FWorkerResult) then
      FOwner.SetState(csStopped)
    else
      FOwner.SetState(csError);

    FOwner.Log(cllInfo,
               Format('Remux worker finished: HRESULT $%.8x',
                      [DWORD(FOwner.FWorkerResult)]));
  finally
    if ComInitialized then
      CoUninitialize();
  end;
end;


constructor TMfCastRemuxPipeline.Create();
begin

  inherited Create();
  FState := csIdle;
  FWorker := nil;
  FWorkerResult := S_OK;
end;


destructor TMfCastRemuxPipeline.Destroy();
begin

  Stop();
  FByteStream := nil;
  FPublisher := nil;

  inherited Destroy();
end;


procedure TMfCastRemuxPipeline.Log(const ALevel: TMfCastLogLevel;
                                   const AMessage: string);
begin

  if Assigned(FLogger) then
    FLogger.Log(ALevel, 'Remux', AMessage);
end;


procedure TMfCastRemuxPipeline.SetState(const AState: TMfCastState);
begin

  FState := AState;
end;


procedure TMfCastRemuxPipeline.SetLogger(const ALogger: IMfCastLogger);
begin

  FLogger := ALogger;
end;


function TMfCastRemuxPipeline.Start(const ARequest: TMfCastRemuxRequest;
                                    const APublisher: IMfCastSegmentPublisher): HRESULT;
begin

  if not Assigned(APublisher) or (Trim(ARequest.SourceName) = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if not (FState in [csIdle, csStopped]) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

  if Assigned(FWorker) then
    begin
      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  FRequest := ARequest;
  FPublisher := APublisher;
  FByteStream := nil;
  Result := FPublisher.GetByteStream(FByteStream);
  if FAILED(Result) then
    Exit;

  FWorkerResult := S_OK;
  FWorker := TMfCastRemuxWorker.Create(Self);
  if not Assigned(FWorker) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

  SetState(csBuffering);
  FWorker.Start();
  Result := S_OK;
end;


function TMfCastRemuxPipeline.Pause(): HRESULT;
begin

  if Assigned(FWorker) then
    TMfCastRemuxWorker(FWorker).PauseRemux();
  SetState(csPaused);
  Result := S_OK;
end;


function TMfCastRemuxPipeline.Resume(): HRESULT;
begin

  if Assigned(FWorker) then
    TMfCastRemuxWorker(FWorker).ResumeRemux();
  SetState(csBuffering);
  Result := S_OK;
end;


function TMfCastRemuxPipeline.Stop(): HRESULT;
begin

  if Assigned(FWorker) then
    begin
      FWorker.Terminate();
      if Assigned(FPublisher) then
        FPublisher.AbortPresentation(E_ABORT);

      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;
  FByteStream := nil;
  FPublisher := nil;
  SetState(csStopped);
  Result := S_OK;
end;


function TMfCastRemuxPipeline.GetState(): TMfCastState;
begin

  Result := FState;
end;

end.
