// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.RdjPro.AudioQueue.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: RDJ Pro MP4 recorder.
//              IMFSinkWriter based MP4 path.
//              Camera/video samples come from the async IMFSourceReader callback.
//              Audio samples come from the RDJ pre-FX Float32 PCM tap.
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
//          This unit deliberately does not use IMFCaptureRecordSink or IMFCaptureEngine.
//          Please, read documentation carefully!
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
// Source: FactoryX.Code.
// =============================================================================
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
// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: RDJ Pro
// Module: RDJ.RdjPro.Mp4Recorder.pas
// Kind: Pascal Unit
// Language: ENU
//
// Description:
//   RDJ Pro MP4 recorder.
//   IMFSinkWriter based MP4 path.
//   Camera/video samples come from the async IMFSourceReader callback.
//   Audio samples come from the RDJ pre-FX Float32 PCM tap.
//
// Notes:
//   This unit deliberately does not use IMFCaptureRecordSink or IMFCaptureEngine.
//
//==============================================================================
unit RDJ.RdjPro.Mp4Recorder;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfReadWrite,
  {Application}
  RDJ_Common;

type
  TRdjProMp4RecorderState = (mrsStopped,
                             mrsStarting,
                             mrsRecording,
                             mrsStopping,
                             mrsFinalizing,
                             mrsError);

  TRdjProAudioBlock = record
    Data: TBytes;
    Frames: Integer;
    SamplesPerSec: DWORD;
    Channels: DWORD;
    BitsPerSample: DWORD;
    BlockAlign: DWORD;
    AvgBytesPerSec: DWORD;
  end;

  TRdjProMp4Recorder = class(TObject)
  private
    FCritSec: TCriticalSection;
    FQueueLock: TCriticalSection;
    FQueueEvent: TEvent;

    FSinkWriter: IMFSinkWriter;
    FVideoStreamIndex: DWORD;
    FAudioStreamIndex: DWORD;

    FVideoQueue: TQueue<IMFSample>;
    FAudioQueue: TQueue<TRdjProAudioBlock>;
    FWorker: TThread;

    FFileName: string;
    FState: TRdjProMp4RecorderState;
    FLastError: HRESULT;
    FAcceptSamples: Boolean;
    FStopRequested: Boolean;
    FVideoOnly: Boolean;

    FVideoMediaType: IMFMediaType;
    FRotationDegrees: DWORD;

    FAudioSamplesPerSec: DWORD;
    FAudioChannels: DWORD;
    FAudioBitsPerSample: DWORD;
    FAudioBlockAlign: DWORD;
    FAudioAvgBytesPerSec: DWORD;

    FBaseVideoTimeSet: Boolean;
    FBaseVideoTime100ns: LONGLONG;
    FNextAudioTime100ns: LONGLONG;

    function GetActive(): Boolean;
    procedure SetState(const AState: TRdjProMp4RecorderState;
                       const AError: HRESULT = S_OK);

    procedure ClearAudioFormat();
    procedure ResetTiming();
    procedure ClearQueues();

    function ConfigureVideoStream(const pInputType: IMFMediaType): HRESULT;
    function ConfigureAudioStream(): HRESULT;
    function StartWorker(): HRESULT;
    procedure WorkerExecute();

    function PopVideoSample(out ASample: IMFSample): Boolean;
    function PopAudioBlock(out ABlock: TRdjProAudioBlock): Boolean;

    function WriteVideoSample(var pSample: IMFSample): HRESULT;
    function WriteAudioBlock(const ABlock: TRdjProAudioBlock): HRESULT;

  public

    constructor Create(); reintroduce;
    destructor Destroy(); override;

    function StartRecording(const AFileName: string;
                            AVideoOnly: Boolean = False): HRESULT;
    function StopRecording(): HRESULT;

    function SetVideoPreviewMediaType(const pMediaType: IMFMediaType): HRESULT;
    function SetAudioWaveFormat(const pwfx: PWAVEFORMATEX): HRESULT;
    procedure SetRotationDegrees(const Degrees: DWORD);

    function QueueVideoSample(pSample: IMFSample): HRESULT;
    function PushPcmFloat32(const pData: PSingle;
                            const Frames: Integer;
                            const pwfx: PWAVEFORMATEX): HRESULT;

    procedure Reset();

    property Active: Boolean read GetActive;
    property FileName: string read FFileName;
    property State: TRdjProMp4RecorderState read FState;
    property LastError: HRESULT read FLastError;
    property VideoStreamIndex: DWORD read FVideoStreamIndex;
    property AudioStreamIndex: DWORD read FAudioStreamIndex;
    property RotationDegrees: DWORD read FRotationDegrees write SetRotationDegrees;
  end;


implementation

const
  RDJ_MP4_DEFAULT_VIDEO_BITRATE = 6000000;
  RDJ_MP4_DEFAULT_AUDIO_AVG_BYTES_PER_SEC = 20000;
  RDJ_100NS_PER_SECOND = 10000000;


function RDJMakeUINT64(const HighPart: DWORD;
                       const LowPart: DWORD): UINT64;
begin

  Result := (UINT64(HighPart) shl 32) or UINT64(LowPart);
end;


function RDJHighDWORD(const Value: UINT64): DWORD;
begin

  Result := DWORD(Value shr 32);
end;


function RDJLowDWORD(const Value: UINT64): DWORD;
begin

  Result := DWORD(Value and $FFFFFFFF);
end;


constructor TRdjProMp4Recorder.Create();
begin

  inherited Create();

  FCritSec := TCriticalSection.Create();
  FQueueLock := TCriticalSection.Create();
  FQueueEvent := TEvent.Create(nil,
                               True,
                               False,
                               '');

  FVideoQueue := TQueue<IMFSample>.Create();
  FAudioQueue := TQueue<TRdjProAudioBlock>.Create();

  FVideoStreamIndex := DWORD(-1);
  FAudioStreamIndex := DWORD(-1);
  FState := mrsStopped;
  FLastError := S_OK;
  FAcceptSamples := False;
  FStopRequested := False;
  FVideoOnly := False;

  ClearAudioFormat();
  ResetTiming();
end;


destructor TRdjProMp4Recorder.Destroy();
begin

  StopRecording();

  ClearQueues();

  FVideoMediaType := nil;
  FSinkWriter := nil;

  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FQueueLock);
  FreeAndNil(FCritSec);

  inherited Destroy();
end;


procedure TRdjProMp4Recorder.ClearAudioFormat();
begin

  FAudioSamplesPerSec := 0;
  FAudioChannels := 0;
  FAudioBitsPerSample := 0;
  FAudioBlockAlign := 0;
  FAudioAvgBytesPerSec := 0;
end;


procedure TRdjProMp4Recorder.ResetTiming();
begin

  FBaseVideoTimeSet := False;
  FBaseVideoTime100ns := 0;
  FNextAudioTime100ns := 0;
end;


procedure TRdjProMp4Recorder.ClearQueues();
var
  AudioBlock: TRdjProAudioBlock;
  VideoSample: IMFSample;

begin

  FQueueLock.Enter();

  try

    while FVideoQueue.Count > 0 do
      begin
        VideoSample := FVideoQueue.Dequeue();
        VideoSample := nil;
      end;

    while FAudioQueue.Count > 0 do
      begin

        AudioBlock := FAudioQueue.Dequeue();
        AudioBlock.Data := nil;
      end;

    FQueueEvent.ResetEvent();
  finally

    FQueueLock.Leave();
  end;
end;


function TRdjProMp4Recorder.GetActive(): Boolean;
begin

  Result := FState in [mrsStarting,
                       mrsRecording,
                       mrsStopping,
                       mrsFinalizing];
end;


procedure TRdjProMp4Recorder.SetState(const AState: TRdjProMp4RecorderState;
                                      const AError: HRESULT);
begin

  FState := AState;
  FLastError := AError;
end;


procedure TRdjProMp4Recorder.Reset();
begin

  StopRecording();

  FCritSec.Enter();

  try

    FSinkWriter := nil;
    FVideoMediaType := nil;

    FVideoStreamIndex := DWORD(-1);
    FAudioStreamIndex := DWORD(-1);

    FFileName := '';
    FRotationDegrees := 0;
    FVideoOnly := False;
    FAcceptSamples := False;

    ClearAudioFormat();
    ResetTiming();
    ClearQueues();

    SetState(mrsStopped,
             S_OK);
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProMp4Recorder.ConfigureVideoStream(const pInputType: IMFMediaType): HRESULT;
var
  OutType: IMFMediaType;
  FrameSize: UINT64;
  FrameRate: UINT64;
  Width: DWORD;
  Height: DWORD;
  Num: DWORD;
  Den: DWORD;
  SubType: TGUID;

begin

  if not Assigned(FSinkWriter) then
    Exit(MF_E_NOT_INITIALIZED);

  if not Assigned(pInputType) then
    Exit(E_POINTER);

  Width := 1280;
  Height := 720;
  Num := 30;
  Den := 1;

  if SUCCEEDED(pInputType.GetUINT64(MF_MT_FRAME_SIZE,
                                    FrameSize)) then
    begin

      Width := RDJHighDWORD(FrameSize);
      Height := RDJLowDWORD(FrameSize);
    end;

  if SUCCEEDED(pInputType.GetUINT64(MF_MT_FRAME_RATE,
                                    FrameRate)) then
    begin
      Num := RDJHighDWORD(FrameRate);
      Den := RDJLowDWORD(FrameRate);

      if (Den = 0) then
        Den := 1;

      if (Num = 0) then
        Num := 30;
    end;

  Result := MFCreateMediaType(OutType);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetGUID(MF_MT_SUBTYPE,
                            MFVideoFormat_H264);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_AVG_BITRATE,
                              RDJ_MP4_DEFAULT_VIDEO_BITRATE);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_INTERLACE_MODE,
                              MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT64(MF_MT_FRAME_SIZE,
                              RDJMakeUINT64(Width,
                                            Height));
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT64(MF_MT_FRAME_RATE,
                              RDJMakeUINT64(Num,
                                            Den));
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                              RDJMakeUINT64(1,
                                            1));
  if FAILED(Result) then
    Exit;

  Result := FSinkWriter.AddStream(OutType,
                                  FVideoStreamIndex);
  if FAILED(Result) then
    Exit;

  // The preview callback media type is the input media type.
  // Make sure it has the required major type.
  Result := pInputType.GetGUID(MF_MT_SUBTYPE,
                               SubType);
  if FAILED(Result) then
    Exit;

  Result := FSinkWriter.SetInputMediaType(FVideoStreamIndex,
                                          pInputType,
                                          nil);
end;


function TRdjProMp4Recorder.ConfigureAudioStream(): HRESULT;
var
  OutType: IMFMediaType;
  InType: IMFMediaType;

begin

  if not Assigned(FSinkWriter) then
    Exit(MF_E_NOT_INITIALIZED);

  if (FAudioSamplesPerSec = 0) or
     (FAudioChannels = 0) or
     (FAudioBlockAlign = 0) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := MFCreateMediaType(OutType);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetGUID(MF_MT_SUBTYPE,
                            MFAudioFormat_AAC);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                              16);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                              FAudioSamplesPerSec);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                              FAudioChannels);
  if FAILED(Result) then
    Exit;

  Result := OutType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                              RDJ_MP4_DEFAULT_AUDIO_AVG_BYTES_PER_SEC);
  if FAILED(Result) then
    Exit;

  Result := FSinkWriter.AddStream(OutType,
                                  FAudioStreamIndex);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMediaType(InType);
  if FAILED(Result) then
    Exit;

  Result := InType.SetGUID(MF_MT_MAJOR_TYPE,
                           MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := InType.SetGUID(MF_MT_SUBTYPE,
                           MFAudioFormat_Float);
  if FAILED(Result) then
    Exit;

  Result := InType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                             32);
  if FAILED(Result) then
    Exit;

  Result := InType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                             FAudioSamplesPerSec);
  if FAILED(Result) then
    Exit;

  Result := InType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                             FAudioChannels);
  if FAILED(Result) then
    Exit;

  Result := InType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                             FAudioChannels * SizeOf(Single));
  if FAILED(Result) then
    Exit;

  Result := InType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                             FAudioSamplesPerSec *
                             FAudioChannels *
                             SizeOf(Single));
  if FAILED(Result) then
    Exit;

  Result := FSinkWriter.SetInputMediaType(FAudioStreamIndex,
                                          InType,
                                          nil);
end;


function TRdjProMp4Recorder.StartWorker(): HRESULT;
begin

  Result := S_OK;

  if Assigned(FWorker) then
    Exit(MF_E_INVALIDREQUEST);

  FWorker := TThread.CreateAnonymousThread(procedure
                                           begin

                                             WorkerExecute();
                                           end);

  FWorker.FreeOnTerminate := False;
  FWorker.Start();
end;


function TRdjProMp4Recorder.StartRecording(const AFileName: string;
                                           AVideoOnly: Boolean): HRESULT;
var
  Ext: string;

begin

  FCritSec.Enter();

  try

    if Active then
      begin

        Result := MF_E_INVALIDREQUEST;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    if (Trim(AFileName) = '') then
      begin
        Result := E_INVALIDARG;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    Ext := LowerCase(ExtractFileExt(AFileName));
    if (Ext <> '.mp4') then
      begin

        Result := MF_E_INVALIDMEDIATYPE;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    if not Assigned(FVideoMediaType) then
      begin

        Result := MF_E_INVALIDMEDIATYPE;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    if (not AVideoOnly) and
       ((FAudioSamplesPerSec = 0) or
        (FAudioChannels = 0) or
        (FAudioBlockAlign = 0)) then
      begin

        Result := MF_E_INVALIDMEDIATYPE;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    SetState(mrsStarting,
             S_OK);

    FFileName := AFileName;
    FVideoOnly := AVideoOnly;
    FVideoStreamIndex := DWORD(-1);
    FAudioStreamIndex := DWORD(-1);
    FAcceptSamples := False;
    FStopRequested := False;

    ResetTiming();
    ClearQueues();

    Result := MFCreateSinkWriterFromURL(PWideChar(FFileName),
                                        nil,
                                        nil,
                                        FSinkWriter);
    if FAILED(Result) then
      begin

        SetState(mrsError,
                 Result);
        Exit;
      end;

    Result := ConfigureVideoStream(FVideoMediaType);
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    if not FVideoOnly then
      begin

        Result := ConfigureAudioStream();
        if FAILED(Result) then
          begin

            FSinkWriter := nil;
            SetState(mrsError,
                     Result);
            Exit;
          end;
      end;

    Result := FSinkWriter.BeginWriting();
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    Result := StartWorker();
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    FAcceptSamples := True;

    SetState(mrsRecording,
             S_OK);
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProMp4Recorder.StopRecording(): HRESULT;
var
  Worker: TThread;
  WaitRes: DWORD;

begin

  Result := S_OK;

  FCritSec.Enter();

  try

    if not Active then
      begin

        SetState(mrsStopped,
                 S_OK);
        Exit;
      end;

    SetState(mrsStopping,
             S_OK);

    // From this point on the audio tap and video callback must drop new data.
    // Do not let the worker drain a growing/large queue while the UI waits.
    FAcceptSamples := False;
    FStopRequested := True;

    // We prefer a clean, responsive stop over draining every last queued frame.
    ClearQueues();

    if Assigned(FQueueEvent) then
      FQueueEvent.SetEvent();

    Worker := FWorker;
  finally

    FCritSec.Leave();
  end;

  if Assigned(Worker) then
    begin

      WaitRes := WaitForSingleObject(Worker.Handle,
                                     5000);

      if WaitRes = WAIT_OBJECT_0 then
        begin

          FCritSec.Enter();

          try

            if FWorker = Worker then
              FWorker := nil;
          finally

            FCritSec.Leave();
          end;

          Worker.Free();
        end
      else
        begin

          // Do not freeze the UI forever. The recorder is left in error state
          // because the worker did not return in time.
          SetState(mrsError,
                   HRESULT_FROM_WIN32(WAIT_TIMEOUT));
          Exit(HRESULT_FROM_WIN32(WAIT_TIMEOUT));
        end;
    end;

  FCritSec.Enter();

  try
    if Assigned(FSinkWriter) then
      begin

        SetState(mrsFinalizing,
                 S_OK);

        Result := FSinkWriter.Finalize();

        FSinkWriter := nil;

        if FAILED(Result) then
          begin

            SetState(mrsError,
                     Result);
            Exit;
          end;
      end;

    ClearQueues();

    FVideoStreamIndex := DWORD(-1);
    FAudioStreamIndex := DWORD(-1);
    FAcceptSamples := False;
    FStopRequested := False;

    SetState(mrsStopped,
             S_OK);
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProMp4Recorder.SetVideoPreviewMediaType(const pMediaType: IMFMediaType): HRESULT;
begin

  Result := S_OK;

  FCritSec.Enter();

  try
    if not Assigned(pMediaType) then
      begin

        Result := E_POINTER;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    FVideoMediaType := pMediaType;
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProMp4Recorder.SetAudioWaveFormat(const pwfx: PWAVEFORMATEX): HRESULT;
begin

  Result := S_OK;

  FCritSec.Enter();

  try
    if not Assigned(pwfx) then
      begin

        Result := E_POINTER;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    if (pwfx.nSamplesPerSec = 0) or
       (pwfx.nChannels = 0) or
       (pwfx.nBlockAlign = 0) then
      begin

        Result := MF_E_INVALIDMEDIATYPE;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    FAudioSamplesPerSec := pwfx.nSamplesPerSec;
    FAudioChannels := pwfx.nChannels;
    FAudioBitsPerSample := 32;
    FAudioBlockAlign := FAudioChannels * SizeOf(Single);
    FAudioAvgBytesPerSec := FAudioSamplesPerSec *
                             FAudioBlockAlign;
  finally

    FCritSec.Leave();
  end;
end;


procedure TRdjProMp4Recorder.SetRotationDegrees(const Degrees: DWORD);
begin

  FCritSec.Enter();

  try
    case Degrees of
      0,
      90,
      180,
      270: FRotationDegrees := Degrees;
    else
      FRotationDegrees := 0;
    end;
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProMp4Recorder.QueueVideoSample(pSample: IMFSample): HRESULT;
var
  OwnedSample: IMFSample;

begin

  Result := S_OK;

  if not Assigned(pSample) then
    Exit(E_POINTER);

  FQueueLock.Enter();

  try
    if (FState <> mrsRecording) or
       (not FAcceptSamples) or
       FStopRequested then
      begin

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.QueueVideoSample rejected: State=%d Accept=%d Stop=%d',
                                       [Ord(FState), Ord(FAcceptSamples), Ord(FStopRequested)])));
        Exit(S_OK);
      end;

    OwnedSample := pSample;
    FVideoQueue.Enqueue(OwnedSample);
    FQueueEvent.SetEvent();
  finally

    FQueueLock.Leave();
  end;
end;


function TRdjProMp4Recorder.PushPcmFloat32(const pData: PSingle;
                                           const Frames: Integer;
                                           const pwfx: PWAVEFORMATEX): HRESULT;
var
  AudioBlock: TRdjProAudioBlock;
  ByteCount: Integer;

begin
  Result := S_OK;

  if not Assigned(pData) then
    Exit(E_POINTER);

  if not Assigned(pwfx) then
    Exit(E_POINTER);

  if Frames <= 0 then
    Exit(E_INVALIDARG);

  if FVideoOnly then
    Exit(S_OK);

  // Memory guard only: do the cheap state/queue check before allocating.
  // No trimming, no timestamp correction, no altered queue order.
  FQueueLock.Enter();
  try

    if (FState <> mrsRecording) or
       (not FAcceptSamples) or
       FStopRequested then
      begin

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.PushPcmFloat32 rejected: State=%d Accept=%d Stop=%d',
                                       [Ord(FState), Ord(FAcceptSamples), Ord(FStopRequested)])));
        Exit(S_OK);
      end;

    if (FAudioQueue.Count >= 8192) then
      begin

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.PushPcmFloat32 dropped newest: AQ=%d',
                                       [FAudioQueue.Count])));
        Exit(S_OK);
      end;
  finally

    FQueueLock.Leave();
  end;
  
  ByteCount := Frames *
               Integer(FAudioChannels) *
               SizeOf(Single);

  if ByteCount <= 0 then
    Exit(E_INVALIDARG);

  SetLength(AudioBlock.Data,
            ByteCount);

  Move(pData^,
       AudioBlock.Data[0],
       ByteCount);

  AudioBlock.Frames := Frames;
  AudioBlock.SamplesPerSec := pwfx.nSamplesPerSec;
  AudioBlock.Channels := pwfx.nChannels;
  AudioBlock.BitsPerSample := 32;
  AudioBlock.BlockAlign := pwfx.nChannels * SizeOf(Single);
  AudioBlock.AvgBytesPerSec := pwfx.nSamplesPerSec *
                               AudioBlock.BlockAlign;

  FQueueLock.Enter();

  try

    if (FState <> mrsRecording) or
       (not FAcceptSamples) or
       FStopRequested then
      Exit(S_OK);

    FAudioQueue.Enqueue(AudioBlock);
    FQueueEvent.SetEvent();
  finally

    FQueueLock.Leave();
  end;
end;


function TRdjProMp4Recorder.PopVideoSample(out ASample: IMFSample): Boolean;
begin

  Result := False;
  ASample := nil;

  FQueueLock.Enter();

  try

    if (FVideoQueue.Count > 0) then
      begin

        ASample := FVideoQueue.Dequeue();
        Result := Assigned(ASample);
      end;
  finally

    FQueueLock.Leave();
  end;
end;


function TRdjProMp4Recorder.PopAudioBlock(out ABlock: TRdjProAudioBlock): Boolean;
begin

  Result := False;
  ABlock.Data := nil;
  ABlock.Frames := 0;

  FQueueLock.Enter();

  try
    if FAudioQueue.Count > 0 then
      begin

        ABlock := FAudioQueue.Dequeue();
        Result := Length(ABlock.Data) > 0;
      end;
  finally

    FQueueLock.Leave();
  end;
end;


function TRdjProMp4Recorder.WriteVideoSample(var pSample: IMFSample): HRESULT;
var
  SampleTime: LONGLONG;
  SampleDuration: LONGLONG;

begin

  if not Assigned(FSinkWriter) then
    Exit(MF_E_NOT_INITIALIZED);

  if not Assigned(pSample) then
    Exit(E_POINTER);

  // Keep the camera/source-reader time base, but normalize it to MP4 start.
  // Do not use the RDJ audio clock here; the SinkWriter needs both streams
  // starting near zero, and the camera samples already carry their own timing.
  if SUCCEEDED(pSample.GetSampleTime(@SampleTime)) then
    begin
      if not FBaseVideoTimeSet then
        begin
          FBaseVideoTime100ns := SampleTime;
          FBaseVideoTimeSet := True;
        end;

      pSample.SetSampleTime(SampleTime - FBaseVideoTime100ns);
    end;

  // Some camera/source-reader paths do not attach duration consistently.
  // A missing duration can make MP4 playback look frozen or make players pick
  // a strange stream duration. Fall back to 30 fps for this first RDJ path.
  if FAILED(pSample.GetSampleDuration(@SampleDuration)) or
     (SampleDuration <= 0) then
    pSample.SetSampleDuration(RDJ_100NS_PER_SECOND div 30);

  Result := FSinkWriter.WriteSample(FVideoStreamIndex,
                                    pSample);

  pSample := nil;
end;


function TRdjProMp4Recorder.WriteAudioBlock(const ABlock: TRdjProAudioBlock): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  pDst: PByte;
  ByteCount: DWORD;
  MaxLen: DWORD;
  Duration100ns: LONGLONG;

begin

  if not Assigned(FSinkWriter) then
    Exit(MF_E_NOT_INITIALIZED);

  if Length(ABlock.Data) = 0 then
    Exit(S_OK);

  ByteCount := Length(ABlock.Data);

  Result := MFCreateSample(Sample);
  if FAILED(Result) then
    Exit;

  Result := MFCreateMemoryBuffer(ByteCount,
                                 Buffer);
  if FAILED(Result) then
    Exit;

  Result := Buffer.Lock(pDst,
                        @MaxLen,
                        nil);
  if FAILED(Result) then
    Exit;

  try

    Move(ABlock.Data[0],
         pDst^,
         ByteCount);
  finally

    Buffer.Unlock();
  end;

  Result := Buffer.SetCurrentLength(ByteCount);
  if FAILED(Result) then
    Exit;

  Result := Sample.AddBuffer(Buffer);
  if FAILED(Result) then
    Exit;

  Buffer := nil;

  Duration100ns := (LONGLONG(ABlock.Frames) *
                    RDJ_100NS_PER_SECOND) div
                    LONGLONG(ABlock.SamplesPerSec);

  Sample.SetSampleTime(FNextAudioTime100ns);
  Sample.SetSampleDuration(Duration100ns);

  Inc(FNextAudioTime100ns,
      Duration100ns);

  Result := FSinkWriter.WriteSample(FAudioStreamIndex,
                                    Sample);
end;


procedure TRdjProMp4Recorder.WorkerExecute();
var
  hr: HResult;
  VideoSample: IMFSample;
  AudioBlock: TRdjProAudioBlock;
  DidWork: Boolean;
  StopNow: Boolean;

begin

  while True do
    begin

      FCritSec.Enter();

      try
        StopNow := FStopRequested or
                   (FState <> mrsRecording);
      finally

        FCritSec.Leave();
      end;

      if StopNow then
        Break;

      DidWork := False;

      // Important: do not drain all video first and then all audio.
      // With live queues the video queue can stay non-empty for a long time,
      // starving audio and feeding the SinkWriter badly interleaved streams.
      // Write at most one sample from each stream per worker turn.
      if PopVideoSample(VideoSample) then
        begin

          FCritSec.Enter();

          try

            StopNow := FStopRequested;
          finally

            FCritSec.Leave();
          end;

          if StopNow then
            begin
              VideoSample := nil;
              Break;
            end;

          DidWork := True;

          hr := WriteVideoSample(VideoSample);
          if FAILED(hr) then
            begin

              // Keep worker alive for now. StopRecording/Finalize reports final state.
              OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WorkerExecute WriteVideoSample failed. hr=%.8x',
                                             [Hr])));
            end;

          VideoSample := nil;
        end;

      if StopNow then
        Break;

      if PopAudioBlock(AudioBlock) then
        begin

          FCritSec.Enter();

          try

            StopNow := FStopRequested;
          finally

            FCritSec.Leave();
          end;

          if StopNow then
            begin
              AudioBlock.Data := nil;
              Break;
            end;

          DidWork := True;

          hr := WriteAudioBlock(AudioBlock);

          if FAILED(hr) then
            begin
              // Keep worker alive for now.
              OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WorkerExecute WriteAudioBlock failed. hr=%.8x',
                                             [Hr])));
            end;

          AudioBlock.Data := nil;
        end;

      if not DidWork then
        FQueueEvent.WaitFor(10);

      FQueueLock.Enter();

      try

        if (FVideoQueue.Count = 0) and
           (FAudioQueue.Count = 0) then
          FQueueEvent.ResetEvent();
      finally

        FQueueLock.Leave();
      end;
    end;
end;

end.
