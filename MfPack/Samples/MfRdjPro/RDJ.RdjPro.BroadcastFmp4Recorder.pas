// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.RdjPro.BroadcastFmp4Recorder.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: RDJ Pro fragmented MP4 recorder. Experimental fMP4/MSE path.
//              IMFSinkWriter + fragmented MPEG-4 media sink based path.
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
unit RDJ.RdjPro.BroadcastFmp4Recorder;

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
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  RDJ_Common;

type
  TRdjProFmp4RecorderState = (mrsStopped,
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

  TRdjProFmp4LiveDiagnostics = record
    VideoQueueCount: Integer;
    AudioQueueCount: Integer;
    RawFragmentQueueCount: Integer;
    PatchedFragmentQueueCount: Integer;
    ParserBufferSize: Integer;
    PendingMoofBytes: Integer;
    TotalBytesWritten: UInt64;
    VideoQueued: UInt64;
    AudioQueued: UInt64;
    VideoWritten: UInt64;
    AudioWritten: UInt64;
    LastVideoWriteElapsedMs: UInt64;
    LastAudioWriteElapsedMs: UInt64;
    RecoveryCount: UInt64;
    SlowVideoWriteStreak: Integer;
  end;


  // Milestone 4: pass-through IMFByteStream proxy with passive fMP4 extraction.
  // It forwards every byte unchanged to the real MFCreateFile byte stream.
  // In parallel it extracts ftyp+moov as the init segment and moof+mdat pairs
  // into an in-memory fragment queue for the later MSE/HTTP layer.
  TRdjProFmp4CaptureByteStream = class(TObject, IInterface, IMFByteStream)
  private
    FRefCount: Integer;
    FInner: IMFByteStream;
    FNullOutput: Boolean;
    FNullPosition: UInt64;
    FNullLength: UInt64;
    FAsyncWriteSizes: TQueue<ULONG>;
    FCritSec: TCriticalSection;
    FTotalBytesWritten: UInt64;
    FNextLogAt: UInt64;
    FLoggedFirstWrite: Boolean;

    FParserBuffer: TBytes;
    FParserBufferSize: Integer;
    FInitSegment: TBytes;
    FPendingMoof: TBytes;
    FFragmentQueue: TQueue<TBytes>;
    FPatchedFragmentQueue: TQueue<TBytes>;
    FTrackDecodeTimes: TDictionary<DWORD, UInt64>;
    FBoxIndex: UInt64;
    FShuttingDown: Boolean;

    procedure ResetBoxObserver();
    procedure ObserveBytes(const pb: PByte; const cb: ULONG);
    procedure ProcessCompleteBox(const ABoxType: DWORD;
                                 const ABoxData: TBytes);
    function PatchFragmentForMse(const AFragment: TBytes;
                                 out APatchedFragment: TBytes): Boolean;
  public

    constructor Create(const AInner: IMFByteStream);
    destructor Destroy(); override;

    // IInterface.  Important: forward unknown interface requests to the real
    // MFCreateFile byte stream.  The MF file byte stream can expose optional
    // interfaces that the fMP4 sink may rely on.
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetCapabilities(out pdwCapabilities: DWORD): HRESULT; stdcall;
    function GetLength(out pqwLength: UInt64): HRESULT; stdcall;
    function SetLength(qwLength: UInt64): HRESULT; stdcall;
    function GetCurrentPosition(out pqwPosition: QWORD): HRESULT; stdcall;
    function SetCurrentPosition(const qwPosition: QWORD): HRESULT; stdcall;
    function IsEndOfStream(out pfEndOfStream: BOOL): HRESULT; stdcall;
    function Read(pb: PByte; cb: ULONG; out pcbRead: ULONG): HRESULT; stdcall;
    function BeginRead(pb: PByte; cb: ULONG; pCallback: IMFAsyncCallback; punkState: IUnknown): HRESULT; stdcall;
    function EndRead(pResult: IMFAsyncResult; out pcbRead: ULONG): HRESULT; stdcall;
    function Write(pb: PByte; cb: ULONG; out pcbWritten: ULONG): HRESULT; stdcall;
    function BeginWrite(pb: PByte; cb: ULONG; pCallback: IMFAsyncCallback; punkState: IUnknown): HRESULT; stdcall;
    function EndWrite(pResult: IMFAsyncResult; out pcbWritten: ULONG): HRESULT; stdcall;
    function Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                  llSeekOffset: LONGLONG;
                  dwSeekFlags: DWORD;
                  out pqwCurrentPosition: UInt64): HRESULT; stdcall;
    function Flush(): HRESULT; stdcall;
    function Close(): HRESULT; stdcall;

    function GetInitSegment(out ASegment: TBytes): Boolean;
    function TryPopFragment(out AFragment: TBytes): Boolean;
    function TryPopPatchedFragment(out AFragment: TBytes): Boolean;
    function TryPopFragmentPair(out ARawFragment: TBytes;
                                out APatchedFragment: TBytes): Boolean;
    function GetLiveQueueStatus(out ARawQueueCount: Integer;
                                out APatchedQueueCount: Integer;
                                out AParserBufferSize: Integer;
                                out APendingMoofBytes: Integer;
                                out ATotalBytesWritten: UInt64): Boolean;
    procedure BeginShutdown();
    procedure ClearLiveFragmentQueues();

    property TotalBytesWritten: UInt64 read FTotalBytesWritten;
  end;

  TRdjProFmp4Recorder = class(TObject)
  private
    FCritSec: TCriticalSection;
    FQueueLock: TCriticalSection;
    FQueueEvent: TEvent;

    FSinkWriter: IMFSinkWriter;
    FMediaSink: IMFMediaSink;
    FByteStream: IMFByteStream;
    FCaptureByteStream: TRdjProFmp4CaptureByteStream;
    FVideoStreamIndex: DWORD;
    FAudioStreamIndex: DWORD;

    FVideoQueue: TQueue<IMFSample>;
    FAudioQueue: TQueue<TRdjProAudioBlock>;
    FPendingAudioBlock: TRdjProAudioBlock;
    FWorker: TThread;

    FFileName: string;
    FPrivateSinkFileName: string;
    FState: TRdjProFmp4RecorderState;
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
    FSkippedAudioTime100ns: LONGLONG;

    FDbgVideoQueued: UInt64;
    FDbgAudioQueued: UInt64;
    FDbgVideoWritten: UInt64;
    FDbgAudioWritten: UInt64;
    FDbgLastLogTick: DWORD;

    FRecoveryActive: Integer;
    FRecoveryRequested: Integer;
    FRecoveryReason: string;
    FSlowVideoWriteStreak: Integer;
    FSlowAudioWriteStreak: Integer;
    FLastVideoWriteElapsedMs: UInt64;
    FLastAudioWriteElapsedMs: UInt64;
    FDbgRecoveries: UInt64;

    procedure DebugLogCounters(const AWhere: string);

    function GetActive(): Boolean;
    procedure SetState(const AState: TRdjProFmp4RecorderState;
                       const AError: HRESULT = S_OK);

    procedure ClearAudioFormat();
    procedure ResetTiming();
    procedure ClearQueues();

    function CreateVideoTargetMediaType(const pInputType: IMFMediaType;
                                        out ppTargetType: IMFMediaType): HRESULT;
    function CreateAudioTargetMediaType(out ppTargetType: IMFMediaType): HRESULT;
    function CreateOutputByteStream(out ppByteStream: IMFByteStream): HRESULT;
    function CreateFragmentedSinkWriter(): HRESULT;
    function StartWorker(): HRESULT;
    procedure WorkerExecute();

    function PopVideoSample(out ASample: IMFSample): Boolean;
    function PopAudioBlock(out ABlock: TRdjProAudioBlock): Boolean;
    function PopAudioBlockCoalesced(out ABlock: TRdjProAudioBlock): Boolean;

    function WriteVideoSample(var pSample: IMFSample): HRESULT;
    function WriteAudioBlock(const ABlock: TRdjProAudioBlock): HRESULT;
    procedure RequestLiveSinkWriterRecoveryInternal(const AReason: string);
    function ConsumeLiveSinkWriterRecoveryRequest(out AReason: string): Boolean;
    procedure ServiceLiveSinkWriterRecovery();

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

    procedure RequestLiveSinkWriterRecovery(const AReason: string);
    procedure DebugLogLiveState(const AWhere: string);
    function GetLiveDiagnostics(out ADiagnostics: TRdjProFmp4LiveDiagnostics): Boolean;

    function GetInitSegment(out ASegment: TBytes): Boolean;
    function TryPopFragment(out AFragment: TBytes): Boolean;
    function TryPopPatchedFragment(out AFragment: TBytes): Boolean;
    function TryPopFragmentPair(out ARawFragment: TBytes;
                                out APatchedFragment: TBytes): Boolean;

    procedure Reset();

    property Active: Boolean read GetActive;
    property FileName: string read FFileName;
    property State: TRdjProFmp4RecorderState read FState;
    property LastError: HRESULT read FLastError;
    property VideoStreamIndex: DWORD read FVideoStreamIndex;
    property AudioStreamIndex: DWORD read FAudioStreamIndex;
    property RotationDegrees: DWORD read FRotationDegrees write SetRotationDegrees;
  end;


implementation

const
  // CvdM archive profile: 2.5 Mbps H.264 video, 160 kbps AAC audio.
  RDJ_FMP4_DEFAULT_VIDEO_BITRATE = 2500000;
  RDJ_FMP4_DEFAULT_AUDIO_AVG_BYTES_PER_SEC = 20000;
  RDJ_100NS_PER_SECOND = 10000000;
  RDJ_FMP4_ASSUMED_VIDEO_STREAM_INDEX = 0;
  RDJ_FMP4_ASSUMED_AUDIO_STREAM_INDEX = 1;
  RDJ_FMP4_CAPTURE_LOG_STEP_BYTES = 16 * 1024 * 1024;
  RDJ_FMP4_SINK_FRAGMENT_DURATION_MS = 1000;
  RDJ_FMP4_SINK_MIN_FRAGMENT_DURATION_100NS =
    RDJ_FMP4_SINK_FRAGMENT_DURATION_MS * 10000;

  // Milestone 24 memory guard:
  // The browser only consumes patched MSE fragments. Keeping a second raw
  // moof+mdat queue doubles live heap pressure and, under the debugger, makes
  // the process look like it has a leak after a few minutes. Raw fragments are
  // therefore not queued in normal live mode. If we ever need raw diagnostics
  // again, turn this on deliberately.
  RDJ_FMP4_QUEUE_RAW_SOURCE_FRAGMENTS = False;
  RDJ_FMP4_LIVE_MAX_FRAGMENT_QUEUE = 24;

  // Per-box debug logging is useful while building the parser, but it also
  // creates a large debugger event stream. Keep the occasional high-level logs
  // and avoid printing every moof/mdat box in normal tests.
  RDJ_FMP4_VERBOSE_BOX_LOG = False;

  // Batch small 10 ms audio tap blocks into large live samples before
  // handing them to the fMP4 sink writer. The sink was eventually choking on
  // thousands of tiny AAC input samples and WriteAudioBlock started taking
  // about one second per call.
  RDJ_FMP4_AUDIO_COALESCE_TARGET_MS = 1000;
  RDJ_FMP4_MAX_AUDIO_QUEUE_BLOCKS = 8;

  // Live guard: after producer-side coalescing, one queue item is about one
  // second of audio. Do not keep minutes of stale audio if the SinkWriter falls
  // behind; for a live stream the newest audio is the only useful audio.
  RDJ_FMP4_AUDIO_QUEUE_TRIM_TO_BLOCKS = 2;

  // Live video queue guard. Queued IMFSamples are raw MF samples, and the
  // SourceReader/SinkWriter path has small internal sample pools. Keep this
  // below that pool size; old live frames are stale and must not pin MF samples.
  RDJ_FMP4_MAX_VIDEO_QUEUE_SAMPLES = 8;
  RDJ_FMP4_VIDEO_QUEUE_TRIM_TO_SAMPLES = 2;
  RDJ_FMP4_VIDEO_WRITES_PER_WORKER_TURN = 8;

  // Keep debug output useful without turning OutputDebugString itself into
  // part of the timing problem.  The old 5 ms threshold logged too many
  // normal encoder calls and made the log look like a thunderstorm.
  RDJ_FMP4_LOG_SLOW_VIDEO_MS = 250;
  RDJ_FMP4_LOG_SLOW_AUDIO_MS = 50;

  // Slow-write thresholds used for diagnostics and the outer recorder restart
  // watchdog. We no longer flush the live SinkWriter in place because that can
  // leave the public MSE segmenter without publishable fragments.
  RDJ_FMP4_RECOVERY_SLOW_VIDEO_MS = 1000;
  RDJ_FMP4_RECOVERY_VERY_SLOW_VIDEO_MS = 3000;
  RDJ_FMP4_RECOVERY_SLOW_VIDEO_STREAK = 2;
  RDJ_FMP4_RECOVERY_SLOW_AUDIO_MS = 500;
  RDJ_FMP4_RECOVERY_SLOW_AUDIO_STREAK = 3;

  // False = browser/MSE mode. The MF fMP4 sink writes to a private local
  // scratch file, while this unit extracts the live MSE fragments from the byte
  // stream. A pure null/seekable stream lets some MF builds retain too much
  // stream state over long runs.
  // True = old diagnostic mode. Writes the full growing public fMP4 file too.
  RDJ_FMP4_WRITE_PUBLIC_MP4_FILE = False;


// MfPack versions differ a little here. Keep the experimental fMP4 unit
// self-contained by declaring the two functions we need only when the project
// has not already provided them. If your MfPack already declares these, define
// RDJ_MF_FMP4_API_ALREADY_DECLARED before using this unit and remove/skip this
// block.
//{$IFNDEF RDJ_MF_FMP4_API_ALREADY_DECLARED}
//function MFCreateFMPEG4MediaSink(pIByteStream: IMFByteStream;
//                                 pVideoMediaType: IMFMediaType;
//                                 pAudioMediaType: IMFMediaType;
//                                 out ppIMediaSink: IMFMediaSink): HRESULT; stdcall; external 'Mfplat.dll';

//function MFCreateSinkWriterFromMediaSink(pMediaSink: IMFMediaSink;
//                                         pAttributes: IMFAttributes;
//                                         out ppSinkWriter: IMFSinkWriter): HRESULT; stdcall; external 'Mfreadwrite.dll';
//{$ENDIF}


function RDJReadU32BE(const p: PByte): DWORD; overload;
begin

  Result := (DWORD(p[0]) shl 24) or
            (DWORD(p[1]) shl 16) or
            (DWORD(p[2]) shl 8) or
             DWORD(p[3]);
end;


function RDJReadU64BE(const p: PByte): UInt64; overload;
begin

  Result := (UInt64(p[0]) shl 56) or
            (UInt64(p[1]) shl 48) or
            (UInt64(p[2]) shl 40) or
            (UInt64(p[3]) shl 32) or
            (UInt64(p[4]) shl 24) or
            (UInt64(p[5]) shl 16) or
            (UInt64(p[6]) shl 8) or
             UInt64(p[7]);
end;


function RDJFourCCToString(const AType: DWORD): string;
begin

  SetLength(Result,
            4);
  Result[1] := Char((AType shr 24) and $FF);
  Result[2] := Char((AType shr 16) and $FF);
  Result[3] := Char((AType shr 8) and $FF);
  Result[4] := Char(AType and $FF);
end;


{ TRdjProFmp4CaptureByteStream }

constructor TRdjProFmp4CaptureByteStream.Create(const AInner: IMFByteStream);
begin

  inherited Create();

  FInner := AInner;
  FNullOutput := not Assigned(AInner);
  FNullPosition := 0;
  FNullLength := 0;
  FAsyncWriteSizes := TQueue<ULONG>.Create();
  FCritSec := TCriticalSection.Create();
  FTotalBytesWritten := 0;
  FNextLogAt := RDJ_FMP4_CAPTURE_LOG_STEP_BYTES;
  FLoggedFirstWrite := False;
  FFragmentQueue := TQueue<TBytes>.Create();
  FPatchedFragmentQueue := TQueue<TBytes>.Create();
  FTrackDecodeTimes := TDictionary<DWORD, UInt64>.Create();
  FShuttingDown := False;
  ResetBoxObserver();
end;


destructor TRdjProFmp4CaptureByteStream.Destroy();
begin

  FInner := nil;
  FreeAndNil(FAsyncWriteSizes);
  FreeAndNil(FFragmentQueue);
  FreeAndNil(FPatchedFragmentQueue);
  FreeAndNil(FTrackDecodeTimes);
  FreeAndNil(FCritSec);

  inherited Destroy();
end;


procedure TRdjProFmp4CaptureByteStream.ResetBoxObserver();
var
  Fragment: TBytes;

begin

  FParserBuffer := nil;
  FParserBufferSize := 0;
  FInitSegment := nil;
  FPendingMoof := nil;
  FBoxIndex := 0;

  if Assigned(FFragmentQueue) then
    begin
      while FFragmentQueue.Count > 0 do
        begin
          Fragment := FFragmentQueue.Dequeue();
          Fragment := nil;
        end;
    end;

  if Assigned(FPatchedFragmentQueue) then
    begin
      while FPatchedFragmentQueue.Count > 0 do
        begin
          Fragment := FPatchedFragmentQueue.Dequeue();
          Fragment := nil;
        end;
    end;

  if Assigned(FTrackDecodeTimes) then
    FTrackDecodeTimes.Clear();
end;


procedure RDJAppendBytes(var ADest: TBytes;
                         const ASource: TBytes);
var
  OldLen: Integer;
  AddLen: Integer;

begin

  AddLen := Length(ASource);
  if AddLen <= 0 then
    Exit;

  OldLen := Length(ADest);
  System.SetLength(ADest,
                   OldLen + AddLen);

  Move(ASource[0],
       ADest[OldLen],
       AddLen);
end;



function RDJReadU32BE(const AData: TBytes;
                      const AOffset: Integer): DWORD; overload;
begin

  Result := (DWORD(AData[AOffset]) shl 24) or
            (DWORD(AData[AOffset + 1]) shl 16) or
            (DWORD(AData[AOffset + 2]) shl 8) or
             DWORD(AData[AOffset + 3]);
end;


function RDJReadU64BE(const AData: TBytes;
                      const AOffset: Integer): UInt64; overload;
begin

  Result := (UInt64(AData[AOffset]) shl 56) or
            (UInt64(AData[AOffset + 1]) shl 48) or
            (UInt64(AData[AOffset + 2]) shl 40) or
            (UInt64(AData[AOffset + 3]) shl 32) or
            (UInt64(AData[AOffset + 4]) shl 24) or
            (UInt64(AData[AOffset + 5]) shl 16) or
            (UInt64(AData[AOffset + 6]) shl 8) or
             UInt64(AData[AOffset + 7]);
end;


procedure RDJWriteU32BE(var AData: TBytes;
                        const AOffset: Integer;
                        const AValue: DWORD);
begin

  AData[AOffset] := Byte((AValue shr 24) and $FF);
  AData[AOffset + 1] := Byte((AValue shr 16) and $FF);
  AData[AOffset + 2] := Byte((AValue shr 8) and $FF);
  AData[AOffset + 3] := Byte(AValue and $FF);
end;


procedure RDJWriteU64BE(var AData: TBytes;
                        const AOffset: Integer;
                        const AValue: UInt64);
begin

  AData[AOffset] := Byte((AValue shr 56) and $FF);
  AData[AOffset + 1] := Byte((AValue shr 48) and $FF);
  AData[AOffset + 2] := Byte((AValue shr 40) and $FF);
  AData[AOffset + 3] := Byte((AValue shr 32) and $FF);
  AData[AOffset + 4] := Byte((AValue shr 24) and $FF);
  AData[AOffset + 5] := Byte((AValue shr 16) and $FF);
  AData[AOffset + 6] := Byte((AValue shr 8) and $FF);
  AData[AOffset + 7] := Byte(AValue and $FF);
end;


function RDJCopyBytes(const AData: TBytes;
                      const AOffset: Integer;
                      const ACount: Integer): TBytes;
begin

  System.SetLength(Result,
                   ACount);

  if ACount > 0 then
    Move(AData[AOffset],
         Result[0],
         ACount);
end;


procedure RDJAppendRaw(var ADest: TBytes;
                       const AData: TBytes;
                       const AOffset: Integer;
                       const ACount: Integer);
var
  OldLen: Integer;
begin

  if ACount <= 0 then
    Exit;

  OldLen := Length(ADest);
  System.SetLength(ADest,
                   OldLen + ACount);

  Move(AData[AOffset],
       ADest[OldLen],
       ACount);
end;


procedure RDJAppendU32BE(var ADest: TBytes;
                         const AValue: DWORD);
var
  OldLen: Integer;
begin

  OldLen := Length(ADest);
  System.SetLength(ADest,
                   OldLen + 4);
  RDJWriteU32BE(ADest,
                OldLen,
                AValue);
end;


procedure RDJAppendU64BE(var ADest: TBytes;
                         const AValue: UInt64);
var
  OldLen: Integer;
begin

  OldLen := Length(ADest);
  System.SetLength(ADest,
                   OldLen + 8);
  RDJWriteU64BE(ADest,
                OldLen,
                AValue);
end;


function RDJMakeTfdtBox(const ABaseDecodeTime: UInt64): TBytes;
begin

  System.SetLength(Result,
                   20);
  RDJWriteU32BE(Result,
                0,
                20);
  RDJWriteU32BE(Result,
                4,
                $74666474); // tfdt
  RDJWriteU32BE(Result,
                8,
                $01000000); // version 1, flags 0
  RDJWriteU64BE(Result,
                12,
                ABaseDecodeTime);
end;


function RDJPatchTfhdBox(const ABoxData: TBytes;
                         out ATrackId: DWORD;
                         out ABaseDataOffset: UInt64): TBytes;
var
  Flags: DWORD;
  NewFlags: DWORD;
  Cursor: Integer;
  Remaining: Integer;

begin

  Result := nil;
  ATrackId := 0;
  ABaseDataOffset := 0;

  if Length(ABoxData) < 16 then
    Exit;

  Flags := RDJReadU32BE(ABoxData,
                        8) and $00FFFFFF;
  ATrackId := RDJReadU32BE(ABoxData,
                           12);

  NewFlags := (Flags and not DWORD($000001)) or DWORD($020000);

  System.SetLength(Result,
                   16);
  RDJWriteU32BE(Result,
                4,
                $74666864); // tfhd
  RDJWriteU32BE(Result,
                8,
                NewFlags); // version 0 + flags
  RDJWriteU32BE(Result,
                12,
                ATrackId);

  Cursor := 16;
  if (Flags and $000001) <> 0 then
    begin
      if Length(ABoxData) >= Cursor + 8 then
        ABaseDataOffset := RDJReadU64BE(ABoxData,
                                        Cursor);
      Inc(Cursor,
          8);
    end;

  Remaining := Length(ABoxData) - Cursor;
  if Remaining > 0 then
    RDJAppendRaw(Result,
                 ABoxData,
                 Cursor,
                 Remaining);

  RDJWriteU32BE(Result,
                0,
                DWORD(Length(Result)));
end;


function RDJGetTrunDuration(const ABoxData: TBytes;
                            const ADefaultSampleDuration: DWORD): UInt64;
var
  Flags: DWORD;
  SampleCount: DWORD;
  Cursor: Integer;
  I: DWORD;
  SampleDuration: DWORD;

begin

  Result := 0;

  if Length(ABoxData) < 16 then
    Exit;
  SampleDuration := 0;
  Flags := RDJReadU32BE(ABoxData,
                        8) and $00FFFFFF;
  SampleCount := RDJReadU32BE(ABoxData,
                              12);
  Cursor := 16;

  if (Flags and $000001) <> 0 then
    Inc(Cursor,
        4);

  if (Flags and $000004) <> 0 then
    Inc(Cursor,
        4);

  for I := 0 to SampleCount - 1 do
    begin
      if (Flags and $000100) <> 0 then
        begin
          if Cursor + 4 > Length(ABoxData) then
            Break;
          SampleDuration := RDJReadU32BE(ABoxData,
                                          Cursor);
          Inc(Cursor,
              4);
        end
      else
        SampleDuration := ADefaultSampleDuration;

      Inc(Result,
          SampleDuration);

      if (Flags and $000200) <> 0 then
        Inc(Cursor,
            4);

      if (Flags and $000400) <> 0 then
        Inc(Cursor,
            4);

      if (Flags and $000800) <> 0 then
        Inc(Cursor,
            4);
    end;
end;


function RDJPatchTrunDataOffset(var ABoxData: TBytes;
                                const ANewDataOffset: DWORD): Boolean;
var
  Flags: DWORD;

begin

  Result := False;

  if Length(ABoxData) < 20 then
    Exit;

  Flags := RDJReadU32BE(ABoxData,
                        8) and $00FFFFFF;

  if (Flags and $000001) = 0 then
    Exit;

  RDJWriteU32BE(ABoxData,
                16,
                ANewDataOffset);
  Result := True;
end;


function TRdjProFmp4CaptureByteStream.PatchFragmentForMse(const AFragment: TBytes;
                                                          out APatchedFragment: TBytes): Boolean;
var
  MoofSize: DWORD;
  MdatSize: DWORD;
  MdatOffset: Integer;
  RawMoof: TBytes;
  PatchedMoof: TBytes;
  PatchedTrafList: TList<TBytes>;
  DataOffsetFixups: TList<Integer>;
  TrafDataOffsets: TList<DWORD>;
  TrafTrackIds: TList<DWORD>;
  TrafDurations: TList<UInt64>;
  TrafCursor: Integer;
  Cursor: Integer;
  BoxSize: DWORD;
  BoxType: DWORD;
  SubCursor: Integer;
  SubEnd: Integer;
  SubBoxSize: DWORD;
  SubBoxType: DWORD;
  NewTraf: TBytes;
  NewBox: TBytes;
  TrackId: DWORD;
  BaseDataOffset: UInt64;
  BaseDecodeTime: UInt64;
  DefaultDuration: DWORD;
  TrafDuration: UInt64;
  NewMoofSize: DWORD;
  I: Integer;
  FixOffset: Integer;
  OldDecodeTime: UInt64;
  OldTrunDataOffset: DWORD;

  procedure QueuePatchedTraf(const ATraf: TBytes;
                             const ATrackId: DWORD;
                             const ADuration: UInt64;
                             const ADataOffsetPatchPosition: Integer;
                             const AOldTrunDataOffset: DWORD);
  begin

    PatchedTrafList.Add(ATraf);
    TrafTrackIds.Add(ATrackId);
    TrafDurations.Add(ADuration);
    DataOffsetFixups.Add(ADataOffsetPatchPosition);
    TrafDataOffsets.Add(AOldTrunDataOffset);
  end;

begin

  Result := False;
  APatchedFragment := nil;

  if Length(AFragment) < 16 then
    Exit;

  MoofSize := RDJReadU32BE(AFragment,
                           0);

  if (MoofSize < 8) or
     (MoofSize > DWORD(Length(AFragment))) or
     (RDJReadU32BE(AFragment,
                   4) <> $6D6F6F66) then // moof
    Exit;

  MdatOffset := Integer(MoofSize);
  if MdatOffset + 8 > Length(AFragment) then
    Exit;

  MdatSize := RDJReadU32BE(AFragment,
                           MdatOffset);

  if (MdatSize < 8) or
     (MdatOffset + Integer(MdatSize) > Length(AFragment)) or
     (RDJReadU32BE(AFragment,
                   MdatOffset + 4) <> $6D646174) then // mdat
    Exit;

  RawMoof := RDJCopyBytes(AFragment,
                          0,
                          Integer(MoofSize));

  PatchedTrafList := TList<TBytes>.Create();
  DataOffsetFixups := TList<Integer>.Create();
  TrafDataOffsets := TList<DWORD>.Create();
  TrafTrackIds := TList<DWORD>.Create();
  TrafDurations := TList<UInt64>.Create();
  try
    PatchedMoof := nil;
    RDJAppendRaw(PatchedMoof,
                 RawMoof,
                 0,
                 8); // moof header, size fixed later

    Cursor := 8;
    while Cursor + 8 <= Integer(MoofSize) do
      begin
        BoxSize := RDJReadU32BE(RawMoof,
                                Cursor);
        BoxType := RDJReadU32BE(RawMoof,
                                Cursor + 4);

        if (BoxSize < 8) or
           (Cursor + Integer(BoxSize) > Integer(MoofSize)) then
          Exit;

        if BoxType <> $74726166 then // traf
          begin
            RDJAppendRaw(PatchedMoof,
                         RawMoof,
                         Cursor,
                         Integer(BoxSize));
            Inc(Cursor,
                Integer(BoxSize));
            Continue;
          end;

        NewTraf := nil;
        RDJAppendRaw(NewTraf,
                     RawMoof,
                     Cursor,
                     8); // traf header, size fixed later

        TrackId := 0;
        DefaultDuration := 0;
        TrafDuration := 0;
        FixOffset := -1;
        BaseDataOffset := 0;
        OldTrunDataOffset := 0;

        SubCursor := Cursor + 8;
        SubEnd := Cursor + Integer(BoxSize);
        while SubCursor + 8 <= SubEnd do
          begin
            SubBoxSize := RDJReadU32BE(RawMoof,
                                       SubCursor);
            SubBoxType := RDJReadU32BE(RawMoof,
                                       SubCursor + 4);

            if (SubBoxSize < 8) or
               (SubCursor + Integer(SubBoxSize) > SubEnd) then
              Exit;

            if SubBoxType = $74666864 then // tfhd
              begin
                NewBox := RDJPatchTfhdBox(RDJCopyBytes(RawMoof,
                                                       SubCursor,
                                                       Integer(SubBoxSize)),
                                          TrackId,
                                          BaseDataOffset);

                // Parse a possible default sample duration after the optional
                // base_data_offset was removed. It is easier to parse from the
                // original flags directly here.
                if (Length(NewBox) >= 20) and
                   ((RDJReadU32BE(NewBox, 8) and $000008) <> 0) then
                  DefaultDuration := RDJReadU32BE(NewBox,
                                                  16);

                RDJAppendBytes(NewTraf,
                               NewBox);

                if Assigned(FTrackDecodeTimes) and
                   FTrackDecodeTimes.TryGetValue(TrackId,
                                                 BaseDecodeTime) then
                  begin
                  end
                else
                  BaseDecodeTime := 0;

                RDJAppendBytes(NewTraf,
                               RDJMakeTfdtBox(BaseDecodeTime));
              end
            else if SubBoxType = $7472756E then // trun
              begin
                NewBox := RDJCopyBytes(RawMoof,
                                       SubCursor,
                                       Integer(SubBoxSize));

                Inc(TrafDuration,
                    RDJGetTrunDuration(NewBox,
                                       DefaultDuration));

                if (Length(NewBox) >= 20) and
                   ((RDJReadU32BE(NewBox, 8) and $000001) <> 0) then
                  OldTrunDataOffset := RDJReadU32BE(NewBox,
                                                    16);

                if RDJPatchTrunDataOffset(NewBox,
                                          0) then
                  begin
                    // Position of data_offset inside this traf. The final value
                    // depends on the final patched moof size, so patch it later.
                    FixOffset := Length(NewTraf) + 16;
                  end;

                RDJAppendBytes(NewTraf,
                               NewBox);
              end
            else
              RDJAppendRaw(NewTraf,
                           RawMoof,
                           SubCursor,
                           Integer(SubBoxSize));

            Inc(SubCursor,
                Integer(SubBoxSize));
          end;

        RDJWriteU32BE(NewTraf,
                      0,
                      DWORD(Length(NewTraf)));

        QueuePatchedTraf(NewTraf,
                         TrackId,
                         TrafDuration,
                         FixOffset,
                         OldTrunDataOffset);

        Inc(Cursor,
            Integer(BoxSize));
      end;

    for I := 0 to PatchedTrafList.Count - 1 do
      RDJAppendBytes(PatchedMoof,
                     PatchedTrafList[I]);

    NewMoofSize := DWORD(Length(PatchedMoof));
    RDJWriteU32BE(PatchedMoof,
                  0,
                  NewMoofSize);

    // Patch trun.data_offset now that the patched moof size is final.
    // MF uses base_data_offset = first byte of mdat payload, so old
    // trun.data_offset values were relative to that payload start. With
    // default-base-is-moof, use moof-relative payload offset.
    Cursor := 8;
    for I := 0 to PatchedTrafList.Count - 1 do
      begin
        // Find the I-th traf in the freshly assembled moof. This avoids keeping
        // stale offsets from before all traf boxes were appended.
        while (Cursor + 8 <= Integer(NewMoofSize)) and
              (RDJReadU32BE(PatchedMoof, Cursor + 4) <> $74726166) do
          Inc(Cursor,
              Integer(RDJReadU32BE(PatchedMoof, Cursor)));

        if Cursor + 8 > Integer(NewMoofSize) then
          Break;

        FixOffset := DataOffsetFixups[I];
        if FixOffset >= 0 then
          RDJWriteU32BE(PatchedMoof,
                        Cursor + FixOffset,
                        NewMoofSize + 8 + TrafDataOffsets[I]);

        Inc(Cursor,
            Integer(RDJReadU32BE(PatchedMoof, Cursor)));
      end;

    APatchedFragment := nil;
    System.SetLength(APatchedFragment,
                     Length(PatchedMoof) + Integer(MdatSize));

    if Length(PatchedMoof) > 0 then
      Move(PatchedMoof[0],
           APatchedFragment[0],
           Length(PatchedMoof));

    // Copy mdat directly from the original raw fragment. The previous code
    // created a separate RawMdat copy and then copied that again into the
    // final patched fragment. With live video this extra 200-300 KB allocation
    // happens several times per second and is exactly the kind of heap churn
    // that looks like a leak under the debugger.
    if MdatSize > 0 then
      Move(AFragment[MdatOffset],
           APatchedFragment[Length(PatchedMoof)],
           Integer(MdatSize));

    if Assigned(FTrackDecodeTimes) then
      begin
        for I := 0 to TrafTrackIds.Count - 1 do
          begin
            if FTrackDecodeTimes.TryGetValue(TrafTrackIds[I],
                                             OldDecodeTime) then
              FTrackDecodeTimes[TrafTrackIds[I]] := OldDecodeTime + TrafDurations[I]
            else
              FTrackDecodeTimes.Add(TrafTrackIds[I],
                                    TrafDurations[I]);
          end;
      end;

    if (FBoxIndex <= 20) or ((FBoxIndex mod 200) = 0) then
      OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.MSEPatch: raw=%d patched=%d moof=%d traf=%d',
                                     [Length(AFragment),
                                      Length(APatchedFragment),
                                      NewMoofSize,
                                      PatchedTrafList.Count])));

    Result := Length(APatchedFragment) > 0;
  finally
    RawMoof := nil;
    PatchedMoof := nil;
    NewTraf := nil;
    NewBox := nil;

    if Assigned(PatchedTrafList) then
      PatchedTrafList.Clear();

    PatchedTrafList.Free();
    DataOffsetFixups.Free();
    TrafDataOffsets.Free();
    TrafTrackIds.Free();
    TrafDurations.Free();
  end;
end;


procedure TRdjProFmp4CaptureByteStream.ProcessCompleteBox(const ABoxType: DWORD;
                                                          const ABoxData: TBytes);
var
  Fragment: TBytes;
  OldFragment: TBytes;
  FragmentSize: Integer;
  OldLen: Integer;
  QueueCount: Integer;

begin

  Inc(FBoxIndex);

  if RDJ_FMP4_VERBOSE_BOX_LOG then
    OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.Box[%d]: type=%s size=%d',
                                   [FBoxIndex,
                                    RDJFourCCToString(ABoxType),
                                    Length(ABoxData)])));

  // The init segment for MSE is ftyp + moov.  Keep pdin/uuid out of it for
  // now; if a browser later needs one of those boxes, we can explicitly add it.
  if (ABoxType = $66747970) or  // ftyp
     (ABoxType = $6D6F6F76) then // moov
    begin
      RDJAppendBytes(FInitSegment,
                     ABoxData);

      OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.InitSegment: bytes=%d',
                                     [Length(FInitSegment)])));
      Exit;
    end;

  if (ABoxType = $6D6F6F66) then // moof
    begin
      FPendingMoof := Copy(ABoxData,
                           0,
                           Length(ABoxData));
      Exit;
    end;

  if (ABoxType = $6D646174) then // mdat
    begin
      if Length(FPendingMoof) > 0 then
        begin
          System.SetLength(Fragment,
                           Length(FPendingMoof) + Length(ABoxData));

          if Length(FPendingMoof) > 0 then
            Move(FPendingMoof[0],
                 Fragment[0],
                 Length(FPendingMoof));

          if Length(ABoxData) > 0 then
            Move(ABoxData[0],
                 Fragment[Length(FPendingMoof)],
                 Length(ABoxData));

          FPendingMoof := nil;

          // Keep the size before queue housekeeping. The old code reused
          // Fragment for the dequeued/oldest item, so the debug line reported
          // bytes=0 once the queue reached the cap.
          FragmentSize := Length(Fragment);

          if Assigned(FFragmentQueue) and RDJ_FMP4_QUEUE_RAW_SOURCE_FRAGMENTS then
            begin
              // Diagnostic raw queue only. Normal live mode does not need a
              // second copy of every moof+mdat fragment.
              while FFragmentQueue.Count >= RDJ_FMP4_LIVE_MAX_FRAGMENT_QUEUE do
                begin
                  OldFragment := FFragmentQueue.Dequeue();
                  OldFragment := nil;
                end;

              FFragmentQueue.Enqueue(Fragment);
              QueueCount := FFragmentQueue.Count;
            end
          else
            QueueCount := 0;

          if Assigned(FPatchedFragmentQueue) then
            begin
              if PatchFragmentForMse(Fragment,
                                     OldFragment) then
                begin
                  while FPatchedFragmentQueue.Count >= RDJ_FMP4_LIVE_MAX_FRAGMENT_QUEUE do
                    begin
                      Fragment := FPatchedFragmentQueue.Dequeue();
                      Fragment := nil;
                    end;

                  FPatchedFragmentQueue.Enqueue(OldFragment);
                  QueueCount := FPatchedFragmentQueue.Count;
                end;

              OldFragment := nil;
            end;

          Fragment := nil;

          if (FBoxIndex <= 20) or ((FBoxIndex mod 200) = 0) then
            OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.FragmentReady: bytes=%d patchedQ=%d',
                                           [FragmentSize,
                                            QueueCount])));
        end;

      Exit;
    end;

  // Other top-level boxes are observed but not stored yet.
  OldLen := 0;
  if OldLen <> 0 then
    OutputDebugString(PChar(''));
end;


procedure TRdjProFmp4CaptureByteStream.ObserveBytes(const pb: PByte;
                                                    const cb: ULONG);
var
  OldSize: Integer;
  HeaderSize: Integer;
  BoxSize32: DWORD;
  BoxSize64: UInt64;
  BoxType: DWORD;
  BoxData: TBytes;
  Remaining: Integer;

begin

  if (pb = nil) or
     (cb = 0) then
    Exit;

  BoxSize64 := 0;

  FCritSec.Enter();
  try

    if FShuttingDown then
      Exit;

    OldSize := FParserBufferSize;
    FParserBufferSize := FParserBufferSize + Integer(cb);
    System.SetLength(FParserBuffer,
                     FParserBufferSize);

    Move(pb^,
         FParserBuffer[OldSize],
         cb);

    while FParserBufferSize >= 8 do
      begin

        BoxSize32 := RDJReadU32BE(@FParserBuffer[0]);
        BoxType := RDJReadU32BE(@FParserBuffer[4]);
        HeaderSize := 8;

        if BoxSize32 = 1 then
          begin
            if FParserBufferSize < 16 then
              Break;

            BoxSize64 := RDJReadU64BE(@FParserBuffer[8]);
            HeaderSize := 16;
          end
        else
          BoxSize64 := BoxSize32;

        if (BoxSize64 < UInt64(HeaderSize)) or
           (BoxSize64 = 0) or
           (BoxSize64 > UInt64(MaxInt)) then
          begin
            OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.BoxObserver invalid box type=%s size=%d buffer=%d',
                                           [RDJFourCCToString(BoxType),
                                            BoxSize64,
                                            FParserBufferSize])));
            ResetBoxObserver();
            Exit;
          end;

        if UInt64(FParserBufferSize) < BoxSize64 then
          Break;

        System.SetLength(BoxData,
                         Integer(BoxSize64));
        Move(FParserBuffer[0],
             BoxData[0],
             Integer(BoxSize64));

        ProcessCompleteBox(BoxType,
                           BoxData);

        Remaining := FParserBufferSize - Integer(BoxSize64);

        if Remaining > 0 then
          Move(FParserBuffer[Integer(BoxSize64)],
               FParserBuffer[0],
               Remaining);

        FParserBufferSize := Remaining;
        System.SetLength(FParserBuffer,
                         FParserBufferSize);
      end;
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.GetInitSegment(out ASegment: TBytes): Boolean;
begin

  ASegment := nil;

  FCritSec.Enter();
  try
    Result := Length(FInitSegment) > 0;

    if Result then
      ASegment := Copy(FInitSegment,
                       0,
                       Length(FInitSegment));
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.TryPopFragment(out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  FCritSec.Enter();
  try
    Result := Assigned(FFragmentQueue) and
              (FFragmentQueue.Count > 0);

    if Result then
      AFragment := FFragmentQueue.Dequeue();
  finally
    FCritSec.Leave();
  end;
end;



function TRdjProFmp4CaptureByteStream.TryPopPatchedFragment(out AFragment: TBytes): Boolean;
begin

  Result := False;
  AFragment := nil;

  FCritSec.Enter();
  try

    if Assigned(FPatchedFragmentQueue) and
       (FPatchedFragmentQueue.Count > 0) then
      begin
        AFragment := FPatchedFragmentQueue.Dequeue();
        Result := Length(AFragment) > 0;
      end;
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.TryPopFragmentPair(out ARawFragment: TBytes;
                                                               out APatchedFragment: TBytes): Boolean;
begin

  Result := False;
  ARawFragment := nil;
  APatchedFragment := nil;

  FCritSec.Enter();
  try
    if (not Assigned(FFragmentQueue)) or
       (not Assigned(FPatchedFragmentQueue)) then
      Exit;

    // Milestone 23:
    // Keep raw/patched queues paired. Popping raw first and discovering that
    // patched is not ready throws away one internal source fragment and lets
    // sourceSeq drift away from publicSeq. Only pop when both sides are ready.
    if (FFragmentQueue.Count <= 0) or
       (FPatchedFragmentQueue.Count <= 0) then
      Exit;

    ARawFragment := FFragmentQueue.Dequeue();
    APatchedFragment := FPatchedFragmentQueue.Dequeue();

    Result := (Length(ARawFragment) > 0) and
              (Length(APatchedFragment) > 0);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.GetLiveQueueStatus(out ARawQueueCount: Integer;
                                                         out APatchedQueueCount: Integer;
                                                         out AParserBufferSize: Integer;
                                                         out APendingMoofBytes: Integer;
                                                         out ATotalBytesWritten: UInt64): Boolean;
begin

  ARawQueueCount := 0;
  APatchedQueueCount := 0;
  AParserBufferSize := 0;
  APendingMoofBytes := 0;
  ATotalBytesWritten := 0;

  FCritSec.Enter();
  try
    if Assigned(FFragmentQueue) then
      ARawQueueCount := FFragmentQueue.Count;

    if Assigned(FPatchedFragmentQueue) then
      APatchedQueueCount := FPatchedFragmentQueue.Count;

    AParserBufferSize := FParserBufferSize;
    APendingMoofBytes := Length(FPendingMoof);
    ATotalBytesWritten := FTotalBytesWritten;

    Result := True;
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.QueryInterface(const IID: TGUID; out Obj): HResult;
begin

  // First answer for our own interfaces: IInterface and IMFByteStream.
  if GetInterface(IID, Obj) then
    Exit(S_OK);

  // Then delegate any optional MF/private interfaces to the real byte stream.
  // Without this, MFCreateFMPEG4MediaSink can see a poorer byte stream than
  // the original MFCreateFile stream and may stop flushing after the first
  // internal fragment/commit.
  Pointer(Obj) := nil;

  if Assigned(FInner) then
    begin
      Result := FInner.QueryInterface(IID, Obj);

      if Succeeded(Result) then
        OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.QueryInterface delegated IID=%s',
                                       [GUIDToString(IID)])));

      Exit;
    end;

  Result := E_NOINTERFACE;
end;


function TRdjProFmp4CaptureByteStream._AddRef: Integer;
begin

  Result := InterlockedIncrement(FRefCount);
end;


function TRdjProFmp4CaptureByteStream._Release: Integer;
begin

  Result := InterlockedDecrement(FRefCount);

  if Result = 0 then
    Destroy();
end;


function TRdjProFmp4CaptureByteStream.GetCapabilities(out pdwCapabilities: DWORD): HRESULT;
begin

  if FNullOutput then
    begin
      // Writable + seekable. The fMP4 sink wants a byte stream shape that
      // behaves like a normal file, but we deliberately discard the bytes
      // after observing/extracting MSE fragments.
      pdwCapabilities := $00000002 or $00000004; // WRITABLE | SEEKABLE
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetCapabilities(pdwCapabilities);
end;


function TRdjProFmp4CaptureByteStream.GetLength(out pqwLength: UInt64): HRESULT;
begin

  if FNullOutput then
    begin
      pqwLength := FNullLength;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetLength(pqwLength);
end;


function TRdjProFmp4CaptureByteStream.SetLength(qwLength: UInt64): HRESULT;
begin

  if FNullOutput then
    begin
      FNullLength := qwLength;
      if FNullPosition > FNullLength then
        FNullPosition := FNullLength;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.SetLength(qwLength);
end;


function TRdjProFmp4CaptureByteStream.GetCurrentPosition(out pqwPosition: UInt64): HRESULT;
begin

  if FNullOutput then
    begin
      pqwPosition := FNullPosition;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.GetCurrentPosition(pqwPosition);
end;


function TRdjProFmp4CaptureByteStream.SetCurrentPosition(const qwPosition: QWORD): HRESULT;
begin

  if FNullOutput then
    begin
      FNullPosition := qwPosition;
      if FNullPosition > FNullLength then
        FNullLength := FNullPosition;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.SetCurrentPosition(qwPosition);
end;


function TRdjProFmp4CaptureByteStream.IsEndOfStream(out pfEndOfStream: BOOL): HRESULT;
begin

  if FNullOutput then
    begin
      pfEndOfStream := BOOL(0);
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.IsEndOfStream(pfEndOfStream);
end;


function TRdjProFmp4CaptureByteStream.Read(pb: PByte;
                                           cb: ULONG;
                                           out pcbRead: ULONG): HRESULT;
begin

  pcbRead := 0;

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Read(pb,
                        cb,
                        pcbRead);
end;


function TRdjProFmp4CaptureByteStream.BeginRead(pb: PByte;
                                                cb: ULONG;
                                                pCallback: IMFAsyncCallback;
                                                punkState: IUnknown): HRESULT;
begin

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.BeginRead(pb,
                             cb,
                             pCallback,
                             punkState);
end;


function TRdjProFmp4CaptureByteStream.EndRead(pResult: IMFAsyncResult;
                                              out pcbRead: ULONG): HRESULT;
begin

  pcbRead := 0;

  if FNullOutput then
    Exit(MF_E_INVALIDREQUEST);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.EndRead(pResult,
                           pcbRead);
end;


function TRdjProFmp4CaptureByteStream.Write(pb: PByte;
                                            cb: ULONG;
                                            out pcbWritten: ULONG): HRESULT;
var
  ShouldLog: Boolean;

begin

  pcbWritten := 0;

  if FNullOutput then
    begin
      if (pb <> nil) and
         (cb > 0) then
        begin
          ObserveBytes(pb,
                       cb);

          ShouldLog := False;

          FCritSec.Enter();
          try
            Inc(FTotalBytesWritten,
                cb);
            Inc(FNullPosition,
                cb);
            if FNullPosition > FNullLength then
              FNullLength := FNullPosition;

            if not FLoggedFirstWrite then
              begin
                FLoggedFirstWrite := True;
                ShouldLog := True;
              end
            else if FTotalBytesWritten >= FNextLogAt then
              begin
                while FNextLogAt <= FTotalBytesWritten do
                  Inc(FNextLogAt,
                      RDJ_FMP4_CAPTURE_LOG_STEP_BYTES);

                ShouldLog := True;
              end;
          finally
            FCritSec.Leave();
          end;

          if ShouldLog then
            OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.NullWrite: total=%d last=%u',
                                           [FTotalBytesWritten,
                                            cb])));
        end;

      pcbWritten := cb;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  // Pure pass-through. Do not inspect or change pb.
  Result := FInner.Write(pb,
                         cb,
                         pcbWritten);

  if SUCCEEDED(Result) and
     (pcbWritten > 0) then
    begin

      ObserveBytes(pb,
                   pcbWritten);

      ShouldLog := False;

      FCritSec.Enter();
      try

        Inc(FTotalBytesWritten,
            pcbWritten);

        if not FLoggedFirstWrite then
          begin

            FLoggedFirstWrite := True;
            ShouldLog := True;
          end
        else if FTotalBytesWritten >= FNextLogAt then
          begin

            while FNextLogAt <= FTotalBytesWritten do
              Inc(FNextLogAt,
                  RDJ_FMP4_CAPTURE_LOG_STEP_BYTES);

            ShouldLog := True;
          end;
      finally

        FCritSec.Leave();
      end;

      if ShouldLog then
        OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.Write: total=%d last=%u',
                                       [FTotalBytesWritten,
                                        pcbWritten])));
    end;
end;


function TRdjProFmp4CaptureByteStream.BeginWrite(pb: PByte;
                                                 cb: ULONG;
                                                 pCallback: IMFAsyncCallback;
                                                 punkState: IUnknown): HRESULT;
var
  ShouldLog: Boolean;
  AsyncResult: IMFAsyncResult;

begin

  // MFCreateFMPEG4MediaSink usually writes through the async byte-stream
  // path. Observe the buffer here before optionally forwarding it to the real
  // MFCreateFile stream; the extracted MSE fragments are the browser product.
  if (pb <> nil) and
     (cb > 0) then
    begin
      ObserveBytes(pb,
                   cb);

      ShouldLog := False;

      FCritSec.Enter();
      try
        Inc(FTotalBytesWritten,
            cb);

        if FNullOutput then
          begin
            Inc(FNullPosition,
                cb);
            if FNullPosition > FNullLength then
              FNullLength := FNullPosition;

            if Assigned(FAsyncWriteSizes) then
              FAsyncWriteSizes.Enqueue(cb);
          end;

        if not FLoggedFirstWrite then
          begin
            FLoggedFirstWrite := True;
            ShouldLog := True;
          end
        else if FTotalBytesWritten >= FNextLogAt then
          begin
            while FNextLogAt <= FTotalBytesWritten do
              Inc(FNextLogAt,
                  RDJ_FMP4_CAPTURE_LOG_STEP_BYTES);

            ShouldLog := True;
          end;
      finally
        FCritSec.Leave();
      end;

      if ShouldLog then
        begin
          if FNullOutput then
            OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.NullBeginWrite: total=%d last=%u',
                                           [FTotalBytesWritten,
                                            cb])))
          else
            OutputDebugString(PChar(Format('TRdjProFmp4CaptureByteStream.BeginWrite: total=%d last=%u',
                                           [FTotalBytesWritten,
                                            cb])));
        end;
    end
  else if FNullOutput and Assigned(FAsyncWriteSizes) then
    begin
      FCritSec.Enter();
      try
        FAsyncWriteSizes.Enqueue(0);
      finally
        FCritSec.Leave();
      end;
    end;

  if FNullOutput then
    begin
      Result := S_OK;

      if Assigned(pCallback) then
        begin
          AsyncResult := nil;
          // MFCreateAsyncResult is normally declared by MfApi. It gives the
          // MF sink a proper async token; EndWrite below only reports byte
          // count from our own queue.
          Result := MFCreateAsyncResult(nil,
                                        pCallback,
                                        punkState,
                                        AsyncResult);
          if SUCCEEDED(Result) then
            Result := pCallback.Invoke(AsyncResult);
        end;

      Exit;
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.BeginWrite(pb,
                              cb,
                              pCallback,
                              punkState);
end;


function TRdjProFmp4CaptureByteStream.EndWrite(pResult: IMFAsyncResult;
                                               out pcbWritten: ULONG): HRESULT;
begin

  pcbWritten := 0;

  if FNullOutput then
    begin
      FCritSec.Enter();
      try
        if Assigned(FAsyncWriteSizes) and
           (FAsyncWriteSizes.Count > 0) then
          pcbWritten := FAsyncWriteSizes.Dequeue();
      finally
        FCritSec.Leave();
      end;

      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.EndWrite(pResult,
                            pcbWritten);
end;


function TRdjProFmp4CaptureByteStream.Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                                           llSeekOffset: LONGLONG;
                                           dwSeekFlags: DWORD;
                                           out pqwCurrentPosition: UInt64): HRESULT;
var
  NewPos: Int64;
begin

  if FNullOutput then
    begin
      if Integer(SeekOrigin) = 0 then
        NewPos := llSeekOffset
      else
        NewPos := Int64(FNullPosition) + llSeekOffset;

      if NewPos < 0 then
        NewPos := 0;

      FNullPosition := UInt64(NewPos);
      if FNullPosition > FNullLength then
        FNullLength := FNullPosition;

      pqwCurrentPosition := FNullPosition;
      Exit(S_OK);
    end;

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Seek(SeekOrigin,
                        llSeekOffset,
                        dwSeekFlags,
                        pqwCurrentPosition);
end;


function TRdjProFmp4CaptureByteStream.Flush(): HRESULT;
begin

  if FNullOutput then
    Exit(S_OK);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Flush();
end;


procedure TRdjProFmp4CaptureByteStream.BeginShutdown();
begin

  FCritSec.Enter();
  try
    if FShuttingDown then
      Exit;

    FShuttingDown := True;
  finally
    FCritSec.Leave();
  end;

  ClearLiveFragmentQueues();

  FCritSec.Enter();
  try
    // Media Foundation can keep the final byte-stream COM reference alive while
    // a worker is timing out in WriteSample. Release the live MSE storage here
    // so the queue/dictionary backing arrays are not reported as leaks.
    FParserBuffer := nil;
    FParserBufferSize := 0;
    FInitSegment := nil;
    FPendingMoof := nil;
    FreeAndNil(FAsyncWriteSizes);
    FreeAndNil(FFragmentQueue);
    FreeAndNil(FPatchedFragmentQueue);
    FreeAndNil(FTrackDecodeTimes);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4CaptureByteStream.Close(): HRESULT;
begin

  BeginShutdown();

  if FNullOutput then
    Exit(S_OK);

  if not Assigned(FInner) then
    Exit(MF_E_NOT_INITIALIZED);

  Result := FInner.Close();
end;


procedure TRdjProFmp4CaptureByteStream.ClearLiveFragmentQueues();
var
  Fragment: TBytes;

begin

  FCritSec.Enter();
  try
    if Assigned(FFragmentQueue) then
      begin
        while FFragmentQueue.Count > 0 do
          begin
            Fragment := FFragmentQueue.Dequeue();
            Fragment := nil;
          end;
      end;

    if Assigned(FPatchedFragmentQueue) then
      begin
        while FPatchedFragmentQueue.Count > 0 do
          begin
            Fragment := FPatchedFragmentQueue.Dequeue();
            Fragment := nil;
          end;
      end;

    // A flush can abandon a moof without its matching mdat. Do not allow a
    // later mdat to be stitched to an old moof after recovery.
    FPendingMoof := nil;

    OutputDebugString(PChar('TRdjProFmp4CaptureByteStream.ClearLiveFragmentQueues (stop/reset only)'));
  finally
    FCritSec.Leave();
  end;
end;


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


procedure TRdjProFmp4Recorder.DebugLogCounters(const AWhere: string);
var
  NowTick: DWORD;
  SearchRec: TSearchRec;
  FileSize: Int64;
begin
  NowTick := GetTickCount();

  if (NowTick - FDbgLastLogTick) < 10000 then
    Exit;

  FDbgLastLogTick := NowTick;
  FileSize := -1;

  if (FPrivateSinkFileName <> '') and
     (FindFirst(FPrivateSinkFileName,
                faAnyFile,
                SearchRec) = 0) then
    begin
      try
        FileSize := SearchRec.Size;
      finally
        FindClose(SearchRec);
      end;
    end;

  FQueueLock.Enter();
  try
    OutputDebugString(PChar(Format('TRdjProFmp4Recorder[%s]: Size=%d VQ=%d AQ=%d VQueued=%d AQueued=%d VWritten=%d AWritten=%d State=%d Accept=%d Stop=%d RecAct=%d RecReq=%d Rec=%d LastVms=%d LastAms=%d',
                                   [AWhere,
                                    FileSize,
                                    FVideoQueue.Count,
                                    FAudioQueue.Count,
                                    FDbgVideoQueued,
                                    FDbgAudioQueued,
                                    FDbgVideoWritten,
                                    FDbgAudioWritten,
                                    Ord(FState),
                                    Ord(FAcceptSamples),
                                    Ord(FStopRequested),
                                    FRecoveryActive,
                                    FRecoveryRequested,
                                    FDbgRecoveries,
                                    FLastVideoWriteElapsedMs,
                                    FLastAudioWriteElapsedMs])));
  finally
    FQueueLock.Leave();
  end;
end;


procedure TRdjProFmp4Recorder.DebugLogLiveState(const AWhere: string);
var
  RawQueueCount: Integer;
  PatchedQueueCount: Integer;
  ParserBufferSize: Integer;
  PendingMoofBytes: Integer;
  VideoQueueCount: Integer;
  AudioQueueCount: Integer;
  TotalBytesWritten: UInt64;

begin

  RawQueueCount := -1;
  PatchedQueueCount := -1;
  ParserBufferSize := -1;
  PendingMoofBytes := -1;
  TotalBytesWritten := 0;

  if Assigned(FCaptureByteStream) then
    FCaptureByteStream.GetLiveQueueStatus(RawQueueCount,
                                          PatchedQueueCount,
                                          ParserBufferSize,
                                          PendingMoofBytes,
                                          TotalBytesWritten);

  FQueueLock.Enter();
  try
    VideoQueueCount := FVideoQueue.Count;
    AudioQueueCount := FAudioQueue.Count;
  finally
    FQueueLock.Leave();
  end;

  OutputDebugString(PChar(Format('TRdjProFmp4Recorder.LiveState[%s]: VQ=%d AQ=%d RawQ=%d PatchedQ=%d ParserBytes=%d PendingMoofBytes=%d TotalBytes=%d VQueued=%d AQueued=%d VWritten=%d AWritten=%d LastVms=%d LastAms=%d Rec=%d',
                                 [AWhere,
                                  VideoQueueCount,
                                  AudioQueueCount,
                                  RawQueueCount,
                                  PatchedQueueCount,
                                  ParserBufferSize,
                                  PendingMoofBytes,
                                  TotalBytesWritten,
                                  FDbgVideoQueued,
                                  FDbgAudioQueued,
                                  FDbgVideoWritten,
                                  FDbgAudioWritten,
                                  FLastVideoWriteElapsedMs,
                                  FLastAudioWriteElapsedMs,
                                  FDbgRecoveries])));
end;


function TRdjProFmp4Recorder.GetLiveDiagnostics(out ADiagnostics: TRdjProFmp4LiveDiagnostics): Boolean;
var
  RawQueueCount: Integer;
  PatchedQueueCount: Integer;
  ParserBufferSize: Integer;
  PendingMoofBytes: Integer;
  TotalBytesWritten: UInt64;

begin

  FillChar(ADiagnostics,
           SizeOf(ADiagnostics),
           0);

  RawQueueCount := -1;
  PatchedQueueCount := -1;
  ParserBufferSize := -1;
  PendingMoofBytes := -1;
  TotalBytesWritten := 0;

  if Assigned(FCaptureByteStream) then
    FCaptureByteStream.GetLiveQueueStatus(RawQueueCount,
                                          PatchedQueueCount,
                                          ParserBufferSize,
                                          PendingMoofBytes,
                                          TotalBytesWritten);

  FQueueLock.Enter();
  try
    ADiagnostics.VideoQueueCount := FVideoQueue.Count;
    ADiagnostics.AudioQueueCount := FAudioQueue.Count;
    ADiagnostics.RawFragmentQueueCount := RawQueueCount;
    ADiagnostics.PatchedFragmentQueueCount := PatchedQueueCount;
    ADiagnostics.ParserBufferSize := ParserBufferSize;
    ADiagnostics.PendingMoofBytes := PendingMoofBytes;
    ADiagnostics.TotalBytesWritten := TotalBytesWritten;
    ADiagnostics.VideoQueued := FDbgVideoQueued;
    ADiagnostics.AudioQueued := FDbgAudioQueued;
    ADiagnostics.VideoWritten := FDbgVideoWritten;
    ADiagnostics.AudioWritten := FDbgAudioWritten;
    ADiagnostics.LastVideoWriteElapsedMs := FLastVideoWriteElapsedMs;
    ADiagnostics.LastAudioWriteElapsedMs := FLastAudioWriteElapsedMs;
    ADiagnostics.RecoveryCount := FDbgRecoveries;
    ADiagnostics.SlowVideoWriteStreak := FSlowVideoWriteStreak;
  finally
    FQueueLock.Leave();
  end;

  Result := True;
end;



constructor TRdjProFmp4Recorder.Create();
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

  FCaptureByteStream := nil;
  FVideoStreamIndex := DWORD(-1);
  FAudioStreamIndex := DWORD(-1);
  FState := mrsStopped;
  FLastError := S_OK;
  FAcceptSamples := False;
  FStopRequested := False;
  FVideoOnly := False;

  FDbgVideoQueued := 0;
  FDbgAudioQueued := 0;
  FDbgVideoWritten := 0;
  FDbgAudioWritten := 0;
  FDbgLastLogTick := GetTickCount();

  FRecoveryActive := 0;
  FRecoveryRequested := 0;
  FRecoveryReason := '';
  FSlowVideoWriteStreak := 0;
  FSlowAudioWriteStreak := 0;
  FLastVideoWriteElapsedMs := 0;
  FLastAudioWriteElapsedMs := 0;
  FDbgRecoveries := 0;

  ClearAudioFormat();
  ResetTiming();
end;


destructor TRdjProFmp4Recorder.Destroy();
var
  Worker: TThread;

begin
  if FAILED(StopRecording()) and Assigned(FWorker) then
    begin
      // A worker that did not stop may still be inside SinkWriter.WriteSample.
      // Destruction is not cancellable, so wait here rather than freeing
      // locks/queues underneath the still-running worker.
      OutputDebugString(PChar(
        'TRdjProFmp4Recorder.Destroy: waiting for worker after StopRecording timeout'));

      Worker := FWorker;
      WaitForSingleObject(Worker.Handle,
                          INFINITE);
      FWorker := nil;
      Worker.Free();
    end;

  ClearQueues();

  FVideoMediaType := nil;
  FSinkWriter := nil;
  FMediaSink := nil;
  FByteStream := nil;
  FCaptureByteStream := nil;

  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FQueueEvent);
  FreeAndNil(FQueueLock);
  FreeAndNil(FCritSec);

  inherited Destroy();
end;


procedure TRdjProFmp4Recorder.ClearAudioFormat();
begin

  FAudioSamplesPerSec := 0;
  FAudioChannels := 0;
  FAudioBitsPerSample := 0;
  FAudioBlockAlign := 0;
  FAudioAvgBytesPerSec := 0;
end;


procedure TRdjProFmp4Recorder.ResetTiming();
begin

  FBaseVideoTimeSet := False;
  FBaseVideoTime100ns := 0;
  FNextAudioTime100ns := 0;
  FSkippedAudioTime100ns := 0;
end;


procedure TRdjProFmp4Recorder.ClearQueues();
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

    FPendingAudioBlock.Data := nil;
    FPendingAudioBlock.Frames := 0;
    FPendingAudioBlock.SamplesPerSec := 0;
    FPendingAudioBlock.Channels := 0;
    FPendingAudioBlock.BitsPerSample := 0;
    FPendingAudioBlock.BlockAlign := 0;
    FPendingAudioBlock.AvgBytesPerSec := 0;
    FSkippedAudioTime100ns := 0;

    FQueueEvent.ResetEvent();
  finally
    FQueueLock.Leave();
  end;
end;


function TRdjProFmp4Recorder.GetActive(): Boolean;
begin
  Result := FState in [mrsStarting,
                       mrsRecording,
                       mrsStopping,
                       mrsFinalizing];
end;


procedure TRdjProFmp4Recorder.SetState(const AState: TRdjProFmp4RecorderState;
                                       const AError: HRESULT);
begin

  FState := AState;
  FLastError := AError;

  if FAILED(AError) then
    OutputDebugString(PChar(Format('SetState reports error: hr=%.8x in state=%d',
                                   [FLastError, Ord(FState)])));
end;



function TRdjProFmp4Recorder.GetInitSegment(out ASegment: TBytes): Boolean;
begin

  ASegment := nil;

  FCritSec.Enter();
  try
    Result := Assigned(FCaptureByteStream) and
              FCaptureByteStream.GetInitSegment(ASegment);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4Recorder.TryPopFragment(out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  FCritSec.Enter();
  try
    Result := Assigned(FCaptureByteStream) and
              FCaptureByteStream.TryPopFragment(AFragment);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4Recorder.TryPopPatchedFragment(out AFragment: TBytes): Boolean;
begin

  AFragment := nil;

  FCritSec.Enter();
  try
    Result := Assigned(FCaptureByteStream) and
              FCaptureByteStream.TryPopPatchedFragment(AFragment);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4Recorder.TryPopFragmentPair(out ARawFragment: TBytes;
                                                out APatchedFragment: TBytes): Boolean;
begin

  ARawFragment := nil;
  APatchedFragment := nil;

  FCritSec.Enter();
  try
    Result := Assigned(FCaptureByteStream) and
              FCaptureByteStream.TryPopFragmentPair(ARawFragment,
                                                   APatchedFragment);
  finally
    FCritSec.Leave();
  end;
end;


procedure TRdjProFmp4Recorder.Reset();
begin

  StopRecording();

  FCritSec.Enter();

  try

    FSinkWriter := nil;
    FMediaSink := nil;
    FByteStream := nil;
    FCaptureByteStream := nil;
    FVideoMediaType := nil;

    FVideoStreamIndex := DWORD(-1);
    FAudioStreamIndex := DWORD(-1);

    FFileName := '';
    FRotationDegrees := 0;
    FVideoOnly := False;
    FAcceptSamples := False;
    FRecoveryReason := '';
    TInterlocked.Exchange(FRecoveryActive, 0);
    TInterlocked.Exchange(FRecoveryRequested, 0);
    FSlowVideoWriteStreak := 0;
    FSlowAudioWriteStreak := 0;
    FLastVideoWriteElapsedMs := 0;
    FLastAudioWriteElapsedMs := 0;

    ClearAudioFormat();
    ResetTiming();
    ClearQueues();

    SetState(mrsStopped,
             S_OK);
  finally

    FCritSec.Leave();
  end;
end;


function TRdjProFmp4Recorder.CreateVideoTargetMediaType(const pInputType: IMFMediaType;
                                                        out ppTargetType: IMFMediaType): HRESULT;
var
  FrameSize: UINT64;
  FrameRate: UINT64;
  Width: DWORD;
  Height: DWORD;
  Num: DWORD;
  Den: DWORD;

begin

  ppTargetType := nil;

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

      if Den = 0 then
        Den := 1;

      if Num = 0 then
        Num := 30;
    end;

  Result := MFCreateMediaType(ppTargetType);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Video);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetGUID(MF_MT_SUBTYPE,
                                 MFVideoFormat_H264);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_AVG_BITRATE,
                                   RDJ_FMP4_DEFAULT_VIDEO_BITRATE);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_INTERLACE_MODE,
                                   MFVideoInterlace_Progressive);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT64(MF_MT_FRAME_SIZE,
                                   RDJMakeUINT64(Width,
                                                 Height));
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT64(MF_MT_FRAME_RATE,
                                   RDJMakeUINT64(Num,
                                                 Den));
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                   RDJMakeUINT64(1,
                                                 1));
end;


function TRdjProFmp4Recorder.CreateAudioTargetMediaType(out ppTargetType: IMFMediaType): HRESULT;
begin

  ppTargetType := nil;

  if (FAudioSamplesPerSec = 0) or
     (FAudioChannels = 0) or
     (FAudioBlockAlign = 0) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := MFCreateMediaType(ppTargetType);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Audio);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetGUID(MF_MT_SUBTYPE,
                                 MFAudioFormat_AAC);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                   16);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                   FAudioSamplesPerSec);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                   FAudioChannels);
  if FAILED(Result) then
    Exit;

  Result := ppTargetType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                   RDJ_FMP4_DEFAULT_AUDIO_AVG_BYTES_PER_SEC);
end;


function TRdjProFmp4Recorder.CreateOutputByteStream(out ppByteStream: IMFByteStream): HRESULT;
var
  FileStream: IMFByteStream;
  CaptureStream: TRdjProFmp4CaptureByteStream;

begin

  ppByteStream := nil;
  FileStream := nil;
  FPrivateSinkFileName := '';

  if Trim(FFileName) = '' then
    Exit(E_INVALIDARG);

  if RDJ_FMP4_WRITE_PUBLIC_MP4_FILE then
    begin
      // Old diagnostic mode: write the full growing fMP4 file as well.
      Result := MFCreateFile(MF_ACCESSMODE_WRITE,
                             MF_OPENMODE_DELETE_IF_EXIST,
                             MF_FILEFLAGS_NONE,
                             PWideChar(FFileName),
                             FileStream);
      if FAILED(Result) then
        Exit;

      CaptureStream := TRdjProFmp4CaptureByteStream.Create(FileStream);
    end
  else
    begin
      // Live MSE mode:
      //
      // Keep the public live.mp4 URL out of the browser path, but still give
      // Media Foundation a real MFCreateFile byte stream.  RDJ_LOG_41 showed
      // the null seekable stream publishing fragments correctly while private
      // bytes kept growing with TotalBytes, which points to MF retaining output
      // state internally.  A private scratch sink lets MF flush normally; the
      // .m4s fragments remain the actual browser output.
      FPrivateSinkFileName :=
        ChangeFileExt(FFileName,
                      Format('.sink_%x_%x.tmp',
                             [GetCurrentProcessId(),
                              GetTickCount()]));

      Result := MFCreateFile(MF_ACCESSMODE_WRITE,
                             MF_OPENMODE_DELETE_IF_EXIST,
                             MF_FILEFLAGS_NONE,
                             PWideChar(FPrivateSinkFileName),
                             FileStream);
      if FAILED(Result) then
        begin
          FPrivateSinkFileName := '';
          Exit;
        end;

      CaptureStream := TRdjProFmp4CaptureByteStream.Create(FileStream);
    end;

  FCaptureByteStream := CaptureStream;
  ppByteStream := CaptureStream as IMFByteStream;
  Result := S_OK;
end;

function TRdjProFmp4Recorder.CreateFragmentedSinkWriter(): HRESULT;
var
  VideoTargetType: IMFMediaType;
  AudioTargetType: IMFMediaType;
  AudioInputType: IMFMediaType;
  MediaSinkAttributes: IMFAttributes;
  SinkWriterAttributes: IMFAttributes;

begin

  VideoTargetType := nil;
  AudioTargetType := nil;
  AudioInputType := nil;
  MediaSinkAttributes := nil;
  SinkWriterAttributes := nil;

  if not Assigned(FVideoMediaType) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Result := CreateVideoTargetMediaType(FVideoMediaType,
                                       VideoTargetType);
  if FAILED(Result) then
    Exit;

  if not FVideoOnly then
    begin
      Result := CreateAudioTargetMediaType(AudioTargetType);
      if FAILED(Result) then
        Exit;
    end;

  // Create the byte stream through a small seam. At milestone 2 this returns a pure pass-through proxy over
  // the proven MFCreateFile byte stream.
  Result := CreateOutputByteStream(FByteStream);
  if FAILED(Result) then
    Exit;

  Result := MFCreateFMPEG4MediaSink(FByteStream,
                                    VideoTargetType,
                                    AudioTargetType,
                                    @FMediaSink);
  if FAILED(Result) then
    Exit;

  Result := FMediaSink.QueryInterface(IID_IMFAttributes,
                                      MediaSinkAttributes);
  if SUCCEEDED(Result) then
    begin
      // Without explicit fMP4 sink fragmentation, MF can buffer encoded media
      // internally and emit moov/moof/mdat only when Finalize is called. For
      // live MSE output that looks like a frozen public stream while memory
      // climbs until the restart/stop path flushes everything at once.
      //
      // Do not set MF_MPEG4SINK_MAX_CODED_SEQUENCES_PER_FRAGMENT to 1 here.
      // RDJ_LOG_42 showed that splits the stream into many tiny coded-sequence
      // fragments, which makes browser playback advance in short bursts.
      Result := MediaSinkAttributes.SetUINT32(MF_MPEG4SINK_MIN_FRAGMENT_DURATION,
                                              RDJ_FMP4_SINK_MIN_FRAGMENT_DURATION_100NS);
      if FAILED(Result) then
        Exit;
    end
  else
    begin
      OutputDebugString(PChar(
        'TRdjProFmp4Recorder.CreateFragmentedSinkWriter: fMP4 sink has no IMFAttributes; live fragments may be delayed until Finalize'));
    end;

  Result := MFCreateAttributes(SinkWriterAttributes,
                               2);
  if FAILED(Result) then
    Exit;

  // Live fMP4 output must not be paced by the sink writer. When MF throttles
  // here, WriteSample can block long enough to starve the MSE fragment stream.
  SinkWriterAttributes.SetUINT32(MF_LOW_LATENCY,
                                 1);
  SinkWriterAttributes.SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING,
                                 1);

  Result := MFCreateSinkWriterFromMediaSink(FMediaSink,
                                            SinkWriterAttributes,
                                            FSinkWriter);
  if FAILED(Result) then
    Exit;

  // The fragmented MPEG-4 media sink has fixed streams from creation time.
  // In practice the SinkWriter exposes them as stream index 0 = video and
  // 1 = audio when both streams are present. This is the first experimental
  // milestone; if a specific MfPack build reports different stream ids we can
  // replace these constants with stream-sink enumeration later.
  FVideoStreamIndex := RDJ_FMP4_ASSUMED_VIDEO_STREAM_INDEX;

  if FVideoOnly then
    FAudioStreamIndex := DWORD(-1)
  else
    FAudioStreamIndex := RDJ_FMP4_ASSUMED_AUDIO_STREAM_INDEX;

  Result := FSinkWriter.SetInputMediaType(FVideoStreamIndex,
                                          FVideoMediaType,
                                          nil);
  if FAILED(Result) then
    Exit;

  if not FVideoOnly then
    begin

      Result := MFCreateMediaType(AudioInputType);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetGUID(MF_MT_MAJOR_TYPE,
                                       MFMediaType_Audio);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetGUID(MF_MT_SUBTYPE,
                                       MFAudioFormat_Float);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                                         32);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                         FAudioSamplesPerSec);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                                         FAudioChannels);
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                                         FAudioChannels * SizeOf(Single));
      if FAILED(Result) then
        Exit;

      Result := AudioInputType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                                         FAudioSamplesPerSec *
                                         FAudioChannels *
                                         SizeOf(Single));
      if FAILED(Result) then
        Exit;

      Result := FSinkWriter.SetInputMediaType(FAudioStreamIndex,
                                              AudioInputType,
                                              nil);
    end;
end;


function TRdjProFmp4Recorder.StartWorker(): HRESULT;
begin

  Result := S_OK;

  if Assigned(FWorker) then
    Exit(MF_E_INVALIDREQUEST);

  FWorker := TThread.CreateAnonymousThread(
    procedure
    begin

      WorkerExecute();
    end);

  FWorker.FreeOnTerminate := False;
  FWorker.Start();
end;


function TRdjProFmp4Recorder.StartRecording(const AFileName: string;
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

    Result := CreateFragmentedSinkWriter();
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        FMediaSink := nil;
        FByteStream := nil;
        FCaptureByteStream := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    Result := FSinkWriter.BeginWriting();
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        FMediaSink := nil;
        FByteStream := nil;
        FCaptureByteStream := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    Result := StartWorker();
    if FAILED(Result) then
      begin

        FSinkWriter := nil;
        FMediaSink := nil;
        FByteStream := nil;
        FCaptureByteStream := nil;
        SetState(mrsError,
                 Result);
        Exit;
      end;

    TInterlocked.Exchange(FRecoveryActive, 0);
    TInterlocked.Exchange(FRecoveryRequested, 0);
    FRecoveryReason := '';
    FSlowVideoWriteStreak := 0;
    FSlowAudioWriteStreak := 0;
    FLastVideoWriteElapsedMs := 0;
    FLastAudioWriteElapsedMs := 0;

    FAcceptSamples := True;

    SetState(mrsRecording,
             S_OK);
  finally
    FCritSec.Leave();
  end;
end;


function TRdjProFmp4Recorder.StopRecording(): HRESULT;
var
  Worker: TThread;
  WaitRes: DWORD;
  CaptureByteStream: TRdjProFmp4CaptureByteStream;

begin

  Result := S_OK;

  FCritSec.Enter();
  try
    if (not Active) and (not Assigned(FWorker)) and
       (not Assigned(FSinkWriter)) and
       (not Assigned(FMediaSink)) and
       (not Assigned(FByteStream)) and
       (not Assigned(FCaptureByteStream)) then
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

    CaptureByteStream := FCaptureByteStream;
    Worker := FWorker;
  finally
    FCritSec.Leave();
  end;

  if Assigned(CaptureByteStream) then
    CaptureByteStream.BeginShutdown();

  if Assigned(Worker) then
    begin
      WaitRes := WaitForSingleObject(Worker.Handle,
                                     30000);

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
          OutputDebugString(PChar(
            'TRdjProFmp4Recorder.StopRecording timeout: worker still owns recorder resources'));

          FCritSec.Enter();
          try
            if Assigned(FMediaSink) then
              FMediaSink.Shutdown();

            FSinkWriter := nil;
            FMediaSink := nil;
            FByteStream := nil;
            FCaptureByteStream := nil;
          finally
            FCritSec.Leave();
          end;

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

        CaptureByteStream := FCaptureByteStream;

        if Assigned(CaptureByteStream) then
          CaptureByteStream.BeginShutdown();

        Result := FSinkWriter.Finalize();

        if Assigned(CaptureByteStream) then
          CaptureByteStream.Close();

        if Assigned(FMediaSink) then
          FMediaSink.Shutdown();

        FSinkWriter := nil;
        FMediaSink := nil;
        FByteStream := nil;
        FCaptureByteStream := nil;

        if (FPrivateSinkFileName <> '') and FileExists(FPrivateSinkFileName) then
          begin
            DeleteFile(PChar(FPrivateSinkFileName));
            FPrivateSinkFileName := '';
          end;

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

function TRdjProFmp4Recorder.SetVideoPreviewMediaType(const pMediaType: IMFMediaType): HRESULT;
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


function TRdjProFmp4Recorder.SetAudioWaveFormat(const pwfx: PWAVEFORMATEX): HRESULT;
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


procedure TRdjProFmp4Recorder.SetRotationDegrees(const Degrees: DWORD);
begin
  FCritSec.Enter();
  try
    case Degrees of
      0,
      90,
      180,
      270:
        FRotationDegrees := Degrees;
    else
      FRotationDegrees := 0;
    end;
  finally
    FCritSec.Leave();
  end;
end;


procedure TRdjProFmp4Recorder.RequestLiveSinkWriterRecoveryInternal(const AReason: string);
begin

  // Milestone 23:
  // Do not perform live SinkWriter.Flush/recovery while broadcasting.  The
  // previous milestone proved that clearing the sink/byte-stream queues during
  // a live session can starve the public fragment group forever: sourceSeq keeps
  // advancing, but publicSeq stops. Keep this hook as a diagnostic marker only.
  OutputDebugString(PChar('TRdjProFmp4Recorder.LiveRecovery disabled: ' + AReason));
end;


procedure TRdjProFmp4Recorder.RequestLiveSinkWriterRecovery(const AReason: string);
begin
  RequestLiveSinkWriterRecoveryInternal(AReason);
  DebugLogLiveState('Recovery request ignored');
end;


function TRdjProFmp4Recorder.ConsumeLiveSinkWriterRecoveryRequest(out AReason: string): Boolean;
begin
  Result := TInterlocked.CompareExchange(FRecoveryRequested, 0, 1) = 1;
  AReason := '';

  if not Result then
    Exit;

  FCritSec.Enter();
  try
    AReason := FRecoveryReason;
    FRecoveryReason := '';
  finally
    FCritSec.Leave();
  end;

  if Trim(AReason) = '' then
    AReason := 'requested';
end;


procedure TRdjProFmp4Recorder.ServiceLiveSinkWriterRecovery();
var
  Reason: string;

begin
  if not ConsumeLiveSinkWriterRecoveryRequest(Reason) then
    Exit;

  OutputDebugString(PChar('TRdjProFmp4Recorder.ServiceLiveSinkWriterRecovery ignored: ' + Reason));
  DebugLogLiveState('Recovery ignored');
end;


function TRdjProFmp4Recorder.QueueVideoSample(pSample: IMFSample): HRESULT;
var
  VideoSample: IMFSample;
  OwnedSample: IMFSample;

begin
  Result := S_OK;

  if not Assigned(pSample) then
    Exit(E_POINTER);

  FQueueLock.Enter();
  try
    if (FState <> mrsRecording) or
       (not FAcceptSamples) or
       FStopRequested or
       (FRecoveryActive <> 0) then
      begin
        if FRecoveryActive = 0 then
          OutputDebugString(PChar(Format('TRdjProFmp4Recorder.QueueVideoSample rejected: State=%d Accept=%d Stop=%d',
                                         [Ord(FState), Ord(FAcceptSamples), Ord(FStopRequested)])));
        Exit(S_OK);
      end;

    if FVideoQueue.Count >= RDJ_FMP4_MAX_VIDEO_QUEUE_SAMPLES then
      begin
        while FVideoQueue.Count > RDJ_FMP4_VIDEO_QUEUE_TRIM_TO_SAMPLES do
          begin
            VideoSample := FVideoQueue.Dequeue();
            VideoSample := nil;
          end;

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.QueueVideoSample memory guard trimmed old video backlog: VQ=%d',
                                       [FVideoQueue.Count])));
      end;

    OwnedSample := pSample;
    FVideoQueue.Enqueue(OwnedSample);
    Inc(FDbgVideoQueued);
    FQueueEvent.SetEvent();
  finally
    FQueueLock.Leave();
  end;
end;

function TRdjProFmp4Recorder.PushPcmFloat32(const pData: PSingle;
                                           const Frames: Integer;
                                           const pwfx: PWAVEFORMATEX): HRESULT;
var
  AudioBlock: TRdjProAudioBlock;
  QueueBlock: TRdjProAudioBlock;
  ByteCount: Integer;
  OldLen: Integer;
  TargetFrames: Integer;

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

  if (DWORD(pwfx.nSamplesPerSec) <> FAudioSamplesPerSec) or
     (DWORD(pwfx.nChannels) <> FAudioChannels) then
    Exit(MF_E_INVALIDMEDIATYPE);

  // Memory guard only: do the cheap state/queue check before allocating.
  // No trimming, no timestamp correction, no altered queue order.
  FQueueLock.Enter();
  try
    if (FState <> mrsRecording) or
       (not FAcceptSamples) or
       FStopRequested or
       (FRecoveryActive <> 0) then
      begin
        if FRecoveryActive = 0 then
          OutputDebugString(PChar(Format('TRdjProFmp4Recorder.PushPcmFloat32 rejected: State=%d Accept=%d Stop=%d',
                                         [Ord(FState), Ord(FAcceptSamples), Ord(FStopRequested)])));
        Exit(S_OK);
      end;

    if FAudioQueue.Count >= RDJ_FMP4_MAX_AUDIO_QUEUE_BLOCKS then
      begin
        // Live mode: discard old backlog and keep the newest tap block.
        // Dropping newest keeps the writer busy with stale audio and turns a
        // short overload into long latency, stutter and eventually a frozen
        // manifest. This trims only the queue payloads, not the recorder state.
        while FAudioQueue.Count > RDJ_FMP4_AUDIO_QUEUE_TRIM_TO_BLOCKS do
          begin
            AudioBlock := FAudioQueue.Dequeue();
            if (AudioBlock.Frames > 0) and
               (AudioBlock.SamplesPerSec > 0) then
              Inc(FSkippedAudioTime100ns,
                  (LONGLONG(AudioBlock.Frames) *
                   RDJ_100NS_PER_SECOND) div
                   LONGLONG(AudioBlock.SamplesPerSec));
            AudioBlock.Data := nil;
          end;

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.PushPcmFloat32 LIVE_GUARD_V2 trimmed old audio backlog: AQ=%d',
                                       [FAudioQueue.Count])));
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
       FStopRequested or
       (FRecoveryActive <> 0) then
      Exit(S_OK);

    // The queue can fill again while the PCM block is being copied.
    // Guard once more at the enqueue point so live streaming never falls
    // back to dropping newest audio.
    if FAudioQueue.Count >= RDJ_FMP4_MAX_AUDIO_QUEUE_BLOCKS then
      begin
        while FAudioQueue.Count > RDJ_FMP4_AUDIO_QUEUE_TRIM_TO_BLOCKS do
          begin
            QueueBlock := FAudioQueue.Dequeue();
            if (QueueBlock.Frames > 0) and
               (QueueBlock.SamplesPerSec > 0) then
              Inc(FSkippedAudioTime100ns,
                  (LONGLONG(QueueBlock.Frames) *
                   RDJ_100NS_PER_SECOND) div
                   LONGLONG(QueueBlock.SamplesPerSec));
            QueueBlock.Data := nil;
          end;

        OutputDebugString(PChar(Format('TRdjProFmp4Recorder.PushPcmFloat32 LIVE_GUARD_V2 trimmed old audio backlog before enqueue: AQ=%d',
                                       [FAudioQueue.Count])));
      end;

    // Coalesce on the producer side. Otherwise the worker can grab the first
    // 10 ms tap block during startup and feed the AAC sink many tiny samples,
    // which is exactly the pattern that produces one-second WriteSample stalls.
    if (FPendingAudioBlock.Frames <= 0) or
       (Length(FPendingAudioBlock.Data) <= 0) then
      begin
        FPendingAudioBlock.SamplesPerSec := AudioBlock.SamplesPerSec;
        FPendingAudioBlock.Channels := AudioBlock.Channels;
        FPendingAudioBlock.BitsPerSample := AudioBlock.BitsPerSample;
        FPendingAudioBlock.BlockAlign := AudioBlock.BlockAlign;
        FPendingAudioBlock.AvgBytesPerSec := AudioBlock.AvgBytesPerSec;
      end
    else if (FPendingAudioBlock.SamplesPerSec <> AudioBlock.SamplesPerSec) or
            (FPendingAudioBlock.Channels <> AudioBlock.Channels) or
            (FPendingAudioBlock.BlockAlign <> AudioBlock.BlockAlign) then
      begin
        FPendingAudioBlock.Data := nil;
        FPendingAudioBlock.Frames := 0;
        FPendingAudioBlock.SamplesPerSec := AudioBlock.SamplesPerSec;
        FPendingAudioBlock.Channels := AudioBlock.Channels;
        FPendingAudioBlock.BitsPerSample := AudioBlock.BitsPerSample;
        FPendingAudioBlock.BlockAlign := AudioBlock.BlockAlign;
        FPendingAudioBlock.AvgBytesPerSec := AudioBlock.AvgBytesPerSec;
      end;

    OldLen := Length(FPendingAudioBlock.Data);
    SetLength(FPendingAudioBlock.Data,
              OldLen + Length(AudioBlock.Data));

    Move(AudioBlock.Data[0],
         FPendingAudioBlock.Data[OldLen],
         Length(AudioBlock.Data));

    Inc(FPendingAudioBlock.Frames,
        AudioBlock.Frames);

    AudioBlock.Data := nil;

    TargetFrames := (Integer(FPendingAudioBlock.SamplesPerSec) *
                     RDJ_FMP4_AUDIO_COALESCE_TARGET_MS) div 1000;
    if TargetFrames <= 0 then
      TargetFrames := FPendingAudioBlock.Frames;

    if FPendingAudioBlock.Frames < TargetFrames then
      Exit(S_OK);

    QueueBlock := FPendingAudioBlock;
    FPendingAudioBlock.Data := nil;
    FPendingAudioBlock.Frames := 0;
    FPendingAudioBlock.SamplesPerSec := 0;
    FPendingAudioBlock.Channels := 0;
    FPendingAudioBlock.BitsPerSample := 0;
    FPendingAudioBlock.BlockAlign := 0;
    FPendingAudioBlock.AvgBytesPerSec := 0;

    FAudioQueue.Enqueue(QueueBlock);
    Inc(FDbgAudioQueued);
    FQueueEvent.SetEvent();
  finally
    FQueueLock.Leave();
  end;
end;


function TRdjProFmp4Recorder.PopVideoSample(out ASample: IMFSample): Boolean;
begin
  Result := False;
  ASample := nil;

  FQueueLock.Enter();
  try
    if FVideoQueue.Count > 0 then
      begin
        ASample := FVideoQueue.Dequeue();
        Result := Assigned(ASample);
      end;
  finally
    FQueueLock.Leave();
  end;
end;


function TRdjProFmp4Recorder.PopAudioBlock(out ABlock: TRdjProAudioBlock): Boolean;
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


function TRdjProFmp4Recorder.PopAudioBlockCoalesced(out ABlock: TRdjProAudioBlock): Boolean;
var
  NextBlock: TRdjProAudioBlock;
  OldLen: Integer;
  AddLen: Integer;
  TargetFrames: Integer;

begin
  Result := PopAudioBlock(ABlock);
  if not Result then
    Exit;

  if (ABlock.SamplesPerSec = 0) or
     (ABlock.Channels = 0) then
    Exit;

  TargetFrames := (Integer(ABlock.SamplesPerSec) *
                   RDJ_FMP4_AUDIO_COALESCE_TARGET_MS) div 1000;

  if TargetFrames <= ABlock.Frames then
    Exit;

  while ABlock.Frames < TargetFrames do
    begin
      NextBlock.Data := nil;
      NextBlock.Frames := 0;

      FQueueLock.Enter();
      try
        if FAudioQueue.Count <= 0 then
          Break;

        NextBlock := FAudioQueue.Dequeue();
      finally
        FQueueLock.Leave();
      end;

      if Length(NextBlock.Data) = 0 then
        Continue;

      if (NextBlock.SamplesPerSec <> ABlock.SamplesPerSec) or
         (NextBlock.Channels <> ABlock.Channels) or
         (NextBlock.BlockAlign <> ABlock.BlockAlign) then
        begin
          // Audio format changed while recording. This should not happen for
          // the broadcast tap. Do not mix incompatible blocks into one sample.
          NextBlock.Data := nil;
          Break;
        end;

      OldLen := Length(ABlock.Data);
      AddLen := Length(NextBlock.Data);

      SetLength(ABlock.Data,
                OldLen + AddLen);

      Move(NextBlock.Data[0],
           ABlock.Data[OldLen],
           AddLen);

      Inc(ABlock.Frames,
          NextBlock.Frames);

      NextBlock.Data := nil;
    end;
end;


function TRdjProFmp4Recorder.WriteVideoSample(var pSample: IMFSample): HRESULT;
var
  SampleTime: LONGLONG;
  SampleDuration: LONGLONG;
  T0: UInt64;
  T1: UInt64;
  ElapsedMs: UInt64;

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

  T0 := GetTickCount64();

  Result := FSinkWriter.WriteSample(FVideoStreamIndex,
                                    pSample);

  pSample := nil;

  T1 := GetTickCount64();
  ElapsedMs := T1 - T0;

  FLastVideoWriteElapsedMs := ElapsedMs;

  if ElapsedMs > RDJ_FMP4_LOG_SLOW_VIDEO_MS then
    OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WriteVideoSample took %d ms hr=0x%.8x VWritten=%d',
                                   [ElapsedMs,
                                    Cardinal(Result),
                                    FDbgVideoWritten])));

  if FAILED(Result) then
    begin
      FSlowVideoWriteStreak := 0;
      Exit;
    end;

  if ElapsedMs >= RDJ_FMP4_RECOVERY_SLOW_VIDEO_MS then
    Inc(FSlowVideoWriteStreak)
  else
    FSlowVideoWriteStreak := 0;

  if (ElapsedMs >= RDJ_FMP4_RECOVERY_VERY_SLOW_VIDEO_MS) or
     (FSlowVideoWriteStreak >= RDJ_FMP4_RECOVERY_SLOW_VIDEO_STREAK) then
    begin
      OutputDebugString(PChar(Format(
        'TRdjProFmp4Recorder.SlowVideoNoRecovery: elapsedMs=%d streak=%d',
        [ElapsedMs, FSlowVideoWriteStreak])));
      DebugLogLiveState('Slow video');
    end;
end;


function TRdjProFmp4Recorder.WriteAudioBlock(const ABlock: TRdjProAudioBlock): HRESULT;
var
  Sample: IMFSample;
  Buffer: IMFMediaBuffer;
  pDst: PByte;
  ByteCount: DWORD;
  MaxLen: DWORD;
  Duration100ns: LONGLONG;
  SkippedDuration100ns: LONGLONG;
  T0: UInt64;
  T1: UInt64;
  ElapsedMs: UInt64;

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

  // The sample now owns its buffer reference. Release our local buffer reference
  // before handing the sample to SinkWriter so MF's small internal pools do not
  // stay pinned by RDJ-side temporaries.
  Buffer := nil;

  Duration100ns := (LONGLONG(ABlock.Frames) *
                    RDJ_100NS_PER_SECOND) div
                    LONGLONG(ABlock.SamplesPerSec);

  FQueueLock.Enter();
  try
    SkippedDuration100ns := FSkippedAudioTime100ns;
    FSkippedAudioTime100ns := 0;
  finally
    FQueueLock.Leave();
  end;

  if SkippedDuration100ns > 0 then
    begin
      Inc(FNextAudioTime100ns,
          SkippedDuration100ns);
      OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WriteAudioBlock skipped stale audio duration: %d ms',
                                     [SkippedDuration100ns div 10000])));
    end;

  Sample.SetSampleTime(FNextAudioTime100ns);
  Sample.SetSampleDuration(Duration100ns);

  Inc(FNextAudioTime100ns,
      Duration100ns);

  T0 := GetTickCount64();

  Result := FSinkWriter.WriteSample(FAudioStreamIndex,
                                    Sample);

  Sample := nil;

  T1 := GetTickCount64();
  ElapsedMs := T1 - T0;

  FLastAudioWriteElapsedMs := ElapsedMs;

  if ElapsedMs > RDJ_FMP4_LOG_SLOW_AUDIO_MS then
    OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WriteAudioBlock took %d ms hr=0x%.8x AWritten=%d Frames=%d Bytes=%d',
                                   [ElapsedMs,
                                    Cardinal(Result),
                                    FDbgAudioWritten,
                                    ABlock.Frames,
                                    ByteCount])));

  if FAILED(Result) then
    begin
      FSlowAudioWriteStreak := 0;
      Exit;
    end;

  if ElapsedMs >= RDJ_FMP4_RECOVERY_SLOW_AUDIO_MS then
    Inc(FSlowAudioWriteStreak)
  else
    FSlowAudioWriteStreak := 0;

  if FSlowAudioWriteStreak >= RDJ_FMP4_RECOVERY_SLOW_AUDIO_STREAK then
    begin
      OutputDebugString(PChar(Format(
        'TRdjProFmp4Recorder.SlowAudioNoRecovery: elapsedMs=%d streak=%d',
        [ElapsedMs, FSlowAudioWriteStreak])));
      DebugLogLiveState('Slow audio');
    end;
end;


procedure TRdjProFmp4Recorder.WorkerExecute();
var
  hr: HResult;
  VideoSample: IMFSample;
  AudioBlock: TRdjProAudioBlock;
  DidWork: Boolean;
  StopNow: Boolean;
  VideoWritesThisTurn: Integer;

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

      ServiceLiveSinkWriterRecovery();

      DidWork := False;

      // Feed a bounded batch of video before one coalesced audio block. Audio
      // writes can take close to a second on the MF AAC/fMP4 path; writing only
      // one video frame before such a call makes the first live seconds thin.
      VideoWritesThisTurn := 0;
      while VideoWritesThisTurn < RDJ_FMP4_VIDEO_WRITES_PER_WORKER_TURN do
        begin
          if not PopVideoSample(VideoSample) then
            Break;

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
          Inc(VideoWritesThisTurn);

          hr := WriteVideoSample(VideoSample);
          if SUCCEEDED(hr) then
            Inc(FDbgVideoWritten);
          if FAILED(hr) then
            begin
              // Keep worker alive for now. StopRecording/Finalize reports final state.
              OutputDebugString(PChar(Format('TRdjProFmp4Recorder.WorkerExecute WriteVideoSample failed. hr=%.8x',
                                             [Hr])));
            end;

          VideoSample := nil;
        end;

      ServiceLiveSinkWriterRecovery();

      if StopNow then
        Break;

      if PopAudioBlockCoalesced(AudioBlock) then
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
          if SUCCEEDED(hr) then
            Inc(FDbgAudioWritten);
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

      DebugLogCounters('Worker');

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
