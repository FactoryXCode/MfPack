// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgMediaServer.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Media server dialog GUI - camera preview and MP4 recorder unit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
//==============================================================================
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
unit dlgMediaServer;

interface

uses

  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  System.DateUtils,
  System.Math,
  System.Diagnostics,
  System.TimeSpan,
  System.Services.Dbt,
  System.SyncObjs,
  {Vcl}
  Vcl.Graphics,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  MPxpButton,
  RDJ_Common,
  LWFileBrowserExDlg,
  RDJ.RdjPro.CaptureEngine,
  RDJ.RdjPro.Mp4Recorder,
  RDJ.RdjPro.BroadcastFmp4Recorder,
  RDJ.RdjPro.CastFmp4Rebaser,
  {Cast}
  MfCast,
  MfCastTypes,
  MfCastWindowsSupport, MfTrackBar;


const

  // Keep raw MSE fragments only as a very short-lived staging file.
  // False = write raw, write patched, then delete raw immediately.
  // True  = keep both raw and patched files for diagnostics.
  RDJ_KEEP_RAW_MSE_FRAGMENTS = False;

  // Milestone 15: publish fewer, larger public MSE fragments, with the
  // public fragment duration configurable from setup.  The Media Foundation
  // byte stream still emits small internal moof/mdat chunks, but the public
  // Caddy/MSE files concatenate several of those chunks into one appendable
  // fMP4 segment.
  RDJ_MSE_SOURCE_FRAGMENT_ESTIMATE_MS = 1000;
  RDJ_MSE_GROUP_SOURCE_FRAGMENTS_DEFAULT = 4;  // ~4s at 1s/source fragment

  RDJ_MSE_MANIFEST_POLL_MS = 750;
  // Publish the first new session as soon as one complete public fragment is
  // mirrored. The browser has its own startup buffer wait, and early session
  // publication lets it leave an old static/camera source immediately after a
  // source-mode restart.
  RDJ_MSE_STARTUP_MIN_PUBLIC_FRAGMENTS = 1;
  RDJ_MSE_GROUP_FORCE_FLUSH_MIN_MS = 2500;
  RDJ_MSE_GROUP_FORCE_FLUSH_FACTOR = 3;

  // Keep a short bounded public live window. The browser metadata is current,
  // so a large backlog makes restarted clients hear older audio while showing
  // the new artist/title.
  RDJ_MSE_PUBLIC_WINDOW_TARGET_MS = 45000;
  RDJ_MSE_KEEP_PATCHED_FRAGMENTS_DEFAULT = 24;
  RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN = 12;
  RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX = 48;

  // If the H.264/fMP4 writer starts taking about a second per video sample and
  // raw video frames are backing up, the process memory grows fast. Roll the
  // recorder session before the public stream stalls completely.
  RDJ_MSE_RECORDER_RESTART_SLOW_VIDEO_MS = 900;
  RDJ_MSE_RECORDER_RESTART_VIDEO_QUEUE = 12;

  // IMPORTANT for the UNC/Caddy mirror:
  // Keep this old caller-side delete path disabled.  The mirror thread now owns
  // server cleanup and runs it only after a safe live.json was written.
  RDJ_MSE_MIRROR_DELETE_OLD_FRAGMENTS = False;
  RDJ_MSE_MIRROR_ACCOUNTANT_ENABLED = True;
  RDJ_MSE_MIRROR_CLEANUP_SAFETY_FRAGMENTS = 2;
  RDJ_MSE_MIRROR_ORPHAN_SCAN_INTERVAL_MS = 60000;

  RDJ_BROADCAST_HANDOVER_LOCK_DIR = 'rdjpro_broadcast.lock';
  RDJ_BROADCAST_HANDOVER_STATUS_FILE = 'broadcast_status.json';
  RDJ_BROADCAST_HANDOVER_OWNER_FILE = 'owner.json';
  RDJ_BROADCAST_HANDOVER_HEARTBEAT_MS = 5000;
  RDJ_BROADCAST_HANDOVER_POLL_MS = 5000;
  RDJ_BROADCAST_HANDOVER_STALE_MS = 120000;
  RDJ_MS_PER_DAY = 86400000;

  RDJ_STATIC_VIDEO_WIDTH = 1280;
  RDJ_STATIC_VIDEO_HEIGHT = 720;
  RDJ_STATIC_VIDEO_FPS = 5;
  RDJ_STATIC_VIDEO_FRAME_DURATION_100NS = 10000000 div RDJ_STATIC_VIDEO_FPS;

  // RDJ Pro locked/unlocked colors.
  LOCKED_COLOR = clRed;
  UNLOCKED_COLOR = clLime;
  CAP_UNLOCKED = 'UNLOCKED';
  CAP_LOCKED = 'LOCKED';
  HEALTH_IDLE_COLOR = $00568000;
  HEALTH_OK_COLOR = clLime;
  HEALTH_WARNING_COLOR = clYellow;
  HEALTH_ERROR_COLOR = clRed;
  RDJ_BROADCAST_HEALTH_AUDIO_STALE_MS = 5000;
  RDJ_BROADCAST_HEALTH_VIDEO_STALE_MS = 5000;
  RDJ_BROADCAST_HEALTH_PUBLIC_STALE_MS = 12000;
  RDJ_BROADCAST_HEALTH_STARTUP_GRACE_MS = 15000;
  RDJ_BROADCAST_HEALTH_LOG_REPEAT_MS = 30000;

type

  TRdjProMseMirrorJobKind = (mjkWriteBytes,
                             mjkCopyFile,
                             mjkDeleteFile);

  TRdjProMseMirrorJob = class
  public

    Kind: TRdjProMseMirrorJobKind;
    FileName: string;       // destination file
    SourceFileName: string; // source file for mjkCopyFile
    Bytes: TBytes;          // small payloads only: init/live.json
  end;

  TRdjProMseMirrorThread = class(TThread)
  private

    FLock: TCriticalSection;
    FEvent: TEvent;
    FJobs: TList;
    FMaxJobs: Integer;
    FPendingLiveJsonFileName: string;
    FPendingLiveJsonBytes: TBytes;
    FLastMirroredPatchedSeq: Integer;
    FLastLiveJsonFlushTick: DWORD;
    FPendingLiveJsonGeneration: Cardinal;
    FLastPublishedLiveJsonSeq: Integer;
    FLastMirrorCleanupSeq: Integer;
    FLastMirrorOrphanScanTick: DWORD;

    function PopJob(): TRdjProMseMirrorJob;
    function IsLowPriorityJob(AJob: TRdjProMseMirrorJob): Boolean;

    function RemoveOneLowPriorityJob(): Boolean;
    function IsLiveJsonFile(const AFileName: string): Boolean;
    function ExtractPatchedFragmentSeq(const AFileName: string): Integer;

    function ExtractJsonInteger(const AJson: string;
                                const AName: string;
                                const ADefault: Integer): Integer;

    function ReplaceJsonInteger(const AJson: string;
                                const AName: string;
                                const AValue: Integer): string;

    function BuildSafeLiveJson(const AJson: string;
                               const ALastMirroredSeq: Integer;
                               out ASafeJson: string): Boolean;

    function Utf8BytesToString(const ABytes: TBytes): string;
    procedure StringToUtf8Bytes(const AText: string;
                                out ABytes: TBytes);

    procedure StorePendingLiveJson(const AFileName: string;
                                   const ABytes: TBytes);

    procedure TryFlushPendingLiveJson();
    procedure WriteBytesToFile(const AFileName: string;
                               const ABytes: TBytes);

    function WriteBytesToFileAtomicResult(const AFileName: string;
                                          const ABytes: TBytes): Boolean;
    procedure DeleteMirroredFragmentFiles(const ADir: string;
                                          const ASeq: Integer);

    procedure ScanOldMirroredFragments(const ADir: string;
                                       const ADeleteThroughSeq: Integer;
                                       const ASafeLast: Integer);

    procedure CleanupMirroredFragmentsAfterManifest(const ALiveJsonFileName: string;
                                                    const ASafeFirst: Integer;
                                                    const ASafeLast: Integer);

    procedure AdvanceMirroredPatchedSeq(const ASeq: Integer);

  protected

    procedure Execute(); override;

  public

    constructor Create(const AMaxJobs: Integer = 512); reintroduce;
    destructor Destroy(); override;

    procedure EnqueueWrite(const AFileName: string;
                           const ABytes: TBytes);

    procedure EnqueueCopy(const ASourceFileName: string;
                          const ADestFileName: string);

    procedure EnqueueDelete(const AFileName: string);
    procedure StopAndWait();
    function QueueCount(): Integer;
  end;

  TfrmMediaServer = class(TForm)
    pnlCaption: TPanel;
    shpOnAirCap: TShape;
    lblCaption: TLabel;
    shpOnAir: TShape;
    lblOnAir: TLabel;
    btnMinimize: TMPxpButton;
    tmrTime: TTimer;
    shpRecordingCap: TShape;
    shpRecording: TShape;
    lblRecording: TLabel;
    pnlRdjProControl: TPanel;
    pnlRdjProControls: TPanel;
    pnlRdjProPreviewHost: TPanel;
    imgRdjProStaticPreview: TImage;
    pnlBottom: TPanel;
    lblRecorderStatus: TLabel;
    lblIcecastServerStatus: TLabel;
    pnlServerCtrl: TPanel;
    pnlAudioEndPoints: TPanel;
    chkServerCtrl: TMPxpButton;
    chkRecorderCtrl: TMPxpButton;
    Bevel3: TBevel;
    Bevel2: TBevel;
    chkBroadcast: TMPxpButton;
    chkRdjProCamera: TMPxpButton;
    chkRdjProStaticImage: TMPxpButton;
    memLog: TMemo;
    pnlRecorderCtrl: TPanel;
    Bevel1: TBevel;
    Label3: TLabel;
    lblFileExt: TLabel;
    lblRecTime: TLabel;
    edRdjProRecFileName: TEdit;
    btnRdjProRecord: TMPxpButton;
    chkRecordVideoOnly: TMPxpButton;
    shpBcLocked: TShape;
    shpBcLockedCap: TShape;
    lblLockBC: TLabel;
    lblBroadcastHealth: TLabel;
    pnlCast: TPanel;
    lblCastDevice: TLabel;
    lblCastState: TLabel;
    cmbCastDevices: TComboBox;
    btnCastDiscover: TMPxpButton;
    btnCastLive: TMPxpButton;
    btnCastStop: TMPxpButton;
    trkCastVolume: TMfTrackBar;
    chkCastMuted: TMPxpButton;
    StaticText1: TStaticText;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkBroadcastClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnMinimizeClick(Sender: TObject);
    procedure chkRdjProCameraClick(Sender: TObject);
    procedure tmrTimeTimer(Sender: TObject);
    procedure btnRdjProRecordClick(Sender: TObject);
    procedure chkRecordVideoOnlyClick(Sender: TObject);
    procedure chkRdjProStaticImageClick(Sender: TObject);
    procedure pnlCaptionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chkServerCtrlClick(Sender: TObject);
    procedure chkRecorderCtrlClick(Sender: TObject);
    procedure btnCastDiscoverClick(Sender: TObject);
    procedure btnCastLiveClick(Sender: TObject);
    procedure btnCastStopClick(Sender: TObject);
    procedure chkCastMutedClick(Sender: TObject);
    procedure trkCastVolumeChange(Sender: TObject);
    procedure cmbCastDevicesChange(Sender: TObject);

  private
    { Private declarations }

    prStopWatch: TStopwatch;
    FTimerRunning: Boolean;
    FRecordingRdjPro: Boolean;
    FRecordVideoOnly: Boolean;
    FRdjProStaticImage: Boolean;
    FUpdatingRdjProVideoSourceButtons: Boolean;

    // Google Cast -------------------------------------------------------------
    FCast: TMfCast;
    FCastDevices: TMfCastDeviceArray;
    FCastWorker: TThread;
    FCastLiveByteStream: IMFByteStream;
    FCastLiveStreamLock: TCriticalSection;
    FCastLiveFragmentCount: Cardinal;
    FCastLiveRebaser: TRdjProCastFmp4Rebaser;
    FCastProducerRequested: Boolean;
    FPendingCastLive: Boolean;
    FPendingCastDevice: TMfCastDevice;
    FCastClosing: Boolean;
    FCastUpdatingVolume: Boolean;
    FCastMediaSessionReady: Boolean;

    // Camera / recorder -------------------------------------------------------
    FRdjProPreviewing: Boolean;
    FRdjProRecording: Boolean;
    FRdjProCaptureInitialized: Boolean;
    FPreviewFramePending: Boolean;
    FStaticImageFileName: string;
    FStaticVideoBitmap: TBitmap;
    FStaticVideoBuffer: IMFMediaBuffer;
    FStaticVideoBufferWidth: Integer;
    FStaticVideoBufferHeight: Integer;
    FStaticVideoMediaType: IMFMediaType;
    FStaticVideoFrameIndex: Int64;
    FStaticVideoStartTick: UInt64;
    FStaticVideoLastTick: UInt64;
    FStaticVideoFrameDuration100ns: LONGLONG;

    // Sample-2 Media Foundation capture engine. This replaces the old
    // SourceReader preview engine. Kept as a form-owned instance so the dialog
    // controls the preview lifetime explicitly.
    FRdjProCaptureManager: TRdjProCaptureManager;

    // Local regulatory recording recorder.
    FRdjProMp4Recorder: TRdjProMp4Recorder;

    // Internet AV broadcast recorder. Writes timestamped MP4 files to CaddyVideoPath.
    FRdjProBroadcastMp4Recorder: TRdjProFMp4Recorder;
    // The fMP4 producer is shared by the public browser broadcast and Cast.
    // FRdjProBroadcasting says the producer is running. This flag says its
    // browser/Caddy destination is enabled.
    FRdjProBroadcasting: Boolean;
    FBroadcastPublishing: Boolean;
    FBroadcastMseDumpDir: string;
    FBroadcastMseMirrorDir: string;
    FBroadcastMseMirrorThread: TRdjProMseMirrorThread;
    FBroadcastMseInitWritten: Boolean;
    FBroadcastMseInitSize: Integer;
    // Internal encoder fragment sequence. These are the small source chunks.
    FBroadcastMseFragmentSeq: Integer;
    // Public sequence published to live.json. Each public fragment contains
    // Configured number of internal fragments, normally about 2 seconds by default.
    FBroadcastMsePublicSeq: Integer;
    FBroadcastMseGroupPartCount: Integer;
    FBroadcastMseGroupBytes: TBytes;
    FBroadcastMseGroupStream: TMemoryStream;
    FBroadcastMseGroupFirstTick: UInt64;
    FBroadcastMseForceNextPublicGroup: Boolean;
    FBroadcastMsePublicTargetMs: Integer;
    FBroadcastMseGroupSourceFragments: Integer;
    FBroadcastMseKeepPatchedFragments: Integer;
    FBroadcastMseManifestWritten: Boolean;
    FBroadcastMseLastManifestSeq: Integer;
    FBroadcastMseLastCleanupSeq: Integer;
    FBroadcastMseManifestPublishSeq: Integer;
    FBroadcastMseSessionId: string;
    FBroadcastMseSessionStartedUtc: TDateTime;
    FLastBroadcastVideoSampleTick: UInt64;
    FLastBroadcastPublicSegmentTick: UInt64;
    FLastBroadcastAudioSampleTick: UInt64;
    FLastBroadcastVideoFlushTick: UInt64;
    FBroadcastVideoFlushQueued: Integer;
    FBroadcastMseLastStallTraceTick: UInt64;
    FBroadcastMseMemLastLogTick: UInt64;
    FBroadcastMseRecorderRestartQueued: Integer;
    FBroadcastMseRecorderRestartCount: Integer;
    FActiveBroadcastVideoMediaType: IMFMediaType;

    FBroadcastHandoverLockAcquired: Boolean;
    FBroadcastHandoverOwnerId: string;
    FLastBroadcastHandoverHeartbeatTick: DWORD;
    FLastBroadcastHandoverPollTick: DWORD;
    FLastBroadcastHandoverNotice: string;
    FLastBroadcastHealthCaption: string;
    FLastBroadcastHealthMessage: string;
    FLastBroadcastHealthLogTick: UInt64;

    FLastRdjProAudioWfx: WAVEFORMATEX;
    FLastRdjProAudioWfxValid: Boolean;
    FLastRdjProVideoSampleTime100ns: LONGLONG;
    FLastRdjProVideoSampleTick: UInt64;

    // MP4 start can be armed before the first RDJ audio tap has supplied
    // the actual mix format. The first valid tap completes the start on the
    // VCL thread.
    FPendingLocalRecording: Boolean;
    FPendingLocalFileName: string;
    FPendingLocalVideoOnly: Boolean;
    FPendingBroadcastRecording: Boolean;
    FPendingBroadcastFileName: string;
    FPendingBroadcastVideoOnly: Boolean;
    FCompletingPendingMp4Start: Boolean;
    // Reusable fields to prevent memory issues.
    FPendingMp4StartQueued: Integer;

    FPtrDevNotify: HDEVNOTIFY;

    procedure CastDeviceChanged(const ADevice: TMfCastDevice);
    procedure CastDeviceRemoved(const ADeviceId: string);
    procedure CastStateChanged(const AOldState: TMfCastState;
                               const ANewState: TMfCastState);

    procedure CastMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure CastError(const AError: TMfCastErrorInfo);

    procedure CastLog(Sender: TObject;
                      const ALevel: TMfCastLogLevel;
                      const ASource: string;
                      const AMessage: string);

    procedure PostCastText(const AWindowsMessage: Cardinal;
                           const AText: string);
    procedure ReloadCastDevices();
    procedure UpdateCastControls();
    procedure ClearCastLiveByteStream();
    procedure ForwardCastLiveFragment(const AFragment: TBytes);
    procedure TryStartPendingCastLive();
    procedure LogCastResult(const AOperation: string;
                            const AHResult: HRESULT);

    procedure WmCastDevices(var Message: TMessage); message WM_MFCAST_DEVICES;
    procedure WmCastState(var Message: TMessage); message WM_MFCAST_STATE;
    procedure WmCastStatus(var Message: TMessage); message WM_MFCAST_STATUS;
    procedure WmCastError(var Message: TMessage); message WM_MFCAST_ERROR;
    procedure WmCastFinished(var Message: TMessage); message WM_MFCAST_FINISHED;
    procedure WmCastLog(var Message: TMessage); message WM_MFCAST_LOG;

    procedure UpdateOnAirLamp(const AOnAir: Boolean);
    procedure UpdateRecorderLamp(const ARecording: Boolean);
    procedure UpdateTimeLabel();
    procedure UpdateRecordingUi();

    procedure SetBroadcastHealth(const ACaption: string;
                                 const AColor: TColor;
                                 const AMessage: string;
                                 const AForceLog: Boolean = False);

    procedure UpdateBroadcastHealth();

    // Camera / recorder
    function StartRdjProCamera(PreviewObject: HWnd): HRESULT;
    procedure StopRdjProCamera();
    procedure ResetRdjProCaptureManager();
    procedure SetRdjProVideoSourceButtons(const AStaticImage,
                                          ACamera: Boolean);

    procedure SetRdjProStaticImageMode(const AStaticImage: Boolean);
    function PickRdjProStaticImageFileName(): string;
    function ResolveRdjProStaticImageFileName(): string;

    function EnsureRdjProStaticVideoFrame(const AWidth: Integer = RDJ_STATIC_VIDEO_WIDTH;
                                          const AHeight: Integer = RDJ_STATIC_VIDEO_HEIGHT): Boolean;
    procedure ShowRdjProStaticPreview();
    procedure HideRdjProStaticPreview();
    procedure RefreshRdjProCameraPreview();
    procedure ScheduleRdjProCameraPreviewRefresh();
    function EnsureRdjProStaticVideoMediaType(): Boolean;

    function CreateRdjProStaticVideoSample(out ASample: IMFSample;
                                           const ASampleTime100ns: LONGLONG;
                                           const AWidth: Integer = RDJ_STATIC_VIDEO_WIDTH;
                                           const AHeight: Integer = RDJ_STATIC_VIDEO_HEIGHT): HRESULT;

    procedure QueueRdjProStaticVideoSample();
    function StartRdjProRecording(const AFileName: string;
                                  AVideoOnly: Boolean = False): Boolean;

    procedure StopRdjProRecording();
    function StartRdjProBroadcast(const APublishBroadcast: Boolean = True): Boolean;
    procedure StopRdjProBroadcast();
    function EnsureRdjProVideoSampleReader(const APreferredMediaType: IMFMediaType = nil): Boolean;
    procedure StopRdjProVideoSampleReaderIfIdle();
    function BuildTimestampedMp4FileName(const ADir: string;
                                         const APrefix: string): string;

    function ResolveLocalRecordingPath(): string;
    function ResolveCaddyLiveMp4Path(): string;
    function ArchiveExistingCaddyLiveMp4(const ALiveFileName: string): Boolean;
    function ResolveBroadcastMseDebugDir(): string;
    function ResolveBroadcastMseMirrorDir(): string;
    procedure ResetBroadcastMseDebugDump();
    procedure PrepareBroadcastMseVideoSourceSwitch();
    procedure DeleteBroadcastMseFilesByMask(const ADir: string;
                                            const AMask: string);

    procedure LaunchBroadcastMseCleanupBatch(const ADumpDir: string;
                                             const AMirrorDir: string);

    procedure CleanupBroadcastMseArtifactsOnStop();
    procedure ConfigureBroadcastMseFragmentGrouping();
    function BroadcastMsePublicTargetMs(): Integer;
    function BroadcastMseGroupSourceFragments(): Integer;
    function BroadcastMseKeepPatchedFragments(): Integer;
    procedure DumpBroadcastMseSegments();
    procedure QueueBroadcastMseRecorderRestart(const AReason: string);
    procedure CheckBroadcastMseVideoSourceFlush();
    procedure LogBroadcastMseMemoryHeartbeat(const AWhere: string);
    function CurrentBroadcastMseGroupBytes(): Int64;
    procedure WriteBytesToFile(const AFileName: string;
                               const ABytes: TBytes);

    procedure WriteBytesToFileAtomic(const AFileName: string;
                                     const ABytes: TBytes);

    procedure WriteUtf8TextToFileAtomic(const AFileName: string;
                                        const AText: string);

    procedure MirrorBytesToServerAtomic(const AFileName: string;
                                        const ABytes: TBytes);

    procedure MirrorFileToServer(const ASourceFileName: string;
                                 const ADestFileName: string);

    procedure MirrorUtf8TextToServerAtomic(const AFileName: string;
                                           const AText: string);

    procedure SetBroadcastHandoverLockIndicator(const ACaption: string;
                                                const AColor: TColor);
    function BroadcastHandoverBaseDir(): string;
    function BroadcastHandoverLockDir(): string;
    function BroadcastHandoverStatusFileName(): string;
    function BroadcastHandoverComputerName(): string;
    function BroadcastHandoverOwnerDisplay(): string;

    function BroadcastHandoverJsonEscape(const AValue: string): string;
    function ExtractBroadcastHandoverJsonString(const AJson: string;
                                                const AName: string): string;

    function ReadBroadcastHandoverTextFile(const AFileName: string): string;
    function IsBroadcastHandoverLockStale(const ALockDir: string): Boolean;
    procedure DeleteBroadcastHandoverLockDir(const ALockDir: string);
    function BuildBroadcastHandoverStatusJson(const AState: string;
                                              const AMessage: string): string;

    procedure PublishBroadcastHandoverStatus(const AState: string;
                                             const AMessage: string);

    function AcquireBroadcastHandoverLock(out AMessage: string): Boolean;
    procedure ReleaseBroadcastHandoverLock(const APublishHandoverReady: Boolean);
    procedure UpdateBroadcastHandoverHeartbeat(const AForce: Boolean = False);
    procedure PollBroadcastHandoverStatus();
    procedure MirrorDeleteServerFile(const AFileName: string);
    function EnsureBroadcastMseMirrorThread(): TRdjProMseMirrorThread;
    procedure StopBroadcastMseMirrorThread();
    function BroadcastMseFirstPublishedSeq(const ALastFragmentSeq: Integer): Integer;
    procedure CleanupOldBroadcastMseFragments(const ALastFragmentSeq: Integer);
    procedure WriteBroadcastMseLiveManifest(const ALastFragmentSeq: Integer);
    procedure WriteBroadcastHlsManifest(const AFirstFragmentSeq,
                                        ALastFragmentSeq: Integer);

    function BroadcastMseGroupForceFlushMs(): Integer;
    procedure ArmPendingLocalRecording(const AFileName: string;
                                       const AVideoOnly: Boolean);

    procedure ArmPendingBroadcastRecording(const AFileName: string;
                                           const AVideoOnly: Boolean);

    procedure TryCompletePendingMp4Starts();

    procedure OnRdjProVideoReaderSample(Sender: TObject;
                                        ASample: IMFSample;
                                        const SampleTime: LONGLONG);

  public
    { Public declarations }

    procedure RecordTapPreFx(const pData: PSingle;
                             const Frames: Integer;
                             const pwfx: PWAVEFORMATEX);

    procedure RecoverBroadcastAfterAudioGraphRestart(const AReason: string);
    function IsBroadcasting(): Boolean;
  end;

var
  FMediaServer: TfrmMediaServer;


implementation

{$R *.dfm}

uses

  {Application}
  RDJ.Setup,
  frmMainMDI;

type

  TRdjProcessMemoryCountersEx = packed record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: NativeUInt;
    WorkingSetSize: NativeUInt;
    QuotaPeakPagedPoolUsage: NativeUInt;
    QuotaPagedPoolUsage: NativeUInt;
    QuotaPeakNonPagedPoolUsage: NativeUInt;
    QuotaNonPagedPoolUsage: NativeUInt;
    PagefileUsage: NativeUInt;
    PeakPagefileUsage: NativeUInt;
    PrivateUsage: NativeUInt;
  end;

  TMfRdjCastStatusMessage = class
  public
    Status: TMfCastMediaStatus;
    constructor Create(const AStatus: TMfCastMediaStatus);
  end;

function RDJGetProcessMemoryInfo(Process: THandle;
                                 ppsmemCounters: Pointer;
                                 cb: DWORD): BOOL; stdcall; external 'psapi.dll' name 'GetProcessMemoryInfo';

function RDJBytesToMb(const ABytes: UInt64): UInt64;
begin

  Result := (ABytes + (1024 * 1024 div 2)) div (1024 * 1024);
end;


constructor TMfRdjCastStatusMessage.Create(const AStatus: TMfCastMediaStatus);
begin

  inherited Create();
  Status := AStatus;
end;


constructor TRdjProMseMirrorThread.Create(const AMaxJobs: Integer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FLock := TCriticalSection.Create();
  FEvent := TEvent.Create(nil,
                          False,
                          False,
                          '');
  FJobs := TList.Create();
  FMaxJobs := AMaxJobs;
  FLastMirroredPatchedSeq := 0;
  FLastLiveJsonFlushTick := 0;
  FPendingLiveJsonGeneration := 0;
  FLastPublishedLiveJsonSeq := 0;
  FLastMirrorCleanupSeq := 0;
  FLastMirrorOrphanScanTick := 0;
end;


destructor TRdjProMseMirrorThread.Destroy();
var
  Job: TRdjProMseMirrorJob;

begin

  if not Finished then
    StopAndWait();

  if Assigned(FJobs) then
    begin
      while (FJobs.Count > 0) do
        begin
          Job := TRdjProMseMirrorJob(FJobs[0]);
          FJobs.Delete(0);
          Job.Free();
        end;
    end;

  FreeAndNil(FJobs);
  FreeAndNil(FEvent);
  FreeAndNil(FLock);

  inherited Destroy();
end;


function TRdjProMseMirrorThread.QueueCount(): Integer;
begin

  FLock.Acquire();

  try

    Result := FJobs.Count;
  finally

    FLock.Release();
  end;
end;


function TRdjProMseMirrorThread.IsLiveJsonFile(const AFileName: string): Boolean;
begin

  Result := SameText(ExtractFileName(AFileName),
                     'live.json');
end;


function TRdjProMseMirrorThread.ExtractPatchedFragmentSeq(const AFileName: string): Integer;
var
  Name: string;
  Digits: string;

begin

  Result := 0;

  Name := ExtractFileName(AFileName);

  if not SameText(Copy(Name,
                       1,
                       Length('patched_frag_')),
                       'patched_frag_') then
    Exit;

  if not SameText(ExtractFileExt(Name),
                  '.m4s') then
    Exit;

  Digits := Copy(Name,
                 Length('patched_frag_') + 1,
                 6);

  Result := StrToIntDef(Digits,
                        0);
end;


function TRdjProMseMirrorThread.Utf8BytesToString(const ABytes: TBytes): string;
var
  Encoding: TEncoding;

begin

  Result := '';

  if (Length(ABytes) = 0) then
    Exit;

  Encoding := TEncoding.UTF8;
  Result := Encoding.GetString(ABytes);
end;


function TRdjProMseMirrorThread.ExtractJsonInteger(const AJson: string;
                                                   const AName: string;
                                                   const ADefault: Integer): Integer;
var
  Pattern: string;
  P: Integer;
  I: Integer;
  S: string;

begin

  Result := ADefault;

  Pattern := '"' + AName + '"';
  P := Pos(Pattern,
           AJson);

  if P <= 0 then
    Exit;

  P := P + Length(Pattern);

  while (P <= Length(AJson)) and
        (AJson[P] <> ':') do
    Inc(P);

  if P > Length(AJson) then
    Exit;

  Inc(P);

  while (P <= Length(AJson)) and
        (AJson[P] <= ' ') do
    Inc(P);

  I := P;

  while (I <= Length(AJson)) and
        {(AJson[I] in ['0'..'9'])}
        CharInSet(AJson[I],
                 ['0'..'9']) do
    Inc(I);

  S := Copy(AJson,
            P,
            I - P);

  if (S <> '') then
    Result := StrToIntDef(S,
                          ADefault);
end;


function TRdjProMseMirrorThread.ReplaceJsonInteger(const AJson: string;
                                                            const AName: string;
                                                            const AValue: Integer): string;
var
  Pattern: string;
  P: Integer;
  I: Integer;

begin

  Result := AJson;

  Pattern := '"' + AName + '"';
  P := Pos(Pattern,
           Result);

  if (P <= 0) then
    Exit;

  P := P + Length(Pattern);

  while (P <= Length(Result)) and
        (Result[P] <> ':') do
    Inc(P);

  if (P > Length(Result)) then
    Exit;

  Inc(P);

  while (P <= Length(Result)) and
        (Result[P] <= ' ') do
    Inc(P);

  I := P;

  while (I <= Length(Result)) and
        {(Result[I] in ['0'..'9'])}
        CharInSet(Result[I],
                 ['0'..'9']) do
    Inc(I);

  if I <= P then
    Exit;

  Result := Copy(Result,
                 1,
                 P - 1) + IntToStr(AValue) + Copy(Result,
                 I,
                 MaxInt);
end;


function TRdjProMseMirrorThread.BuildSafeLiveJson(const AJson: string;
                                                  const ALastMirroredSeq: Integer;
                                                  out ASafeJson: string): Boolean;
var
  ManifestFirst: Integer;
  ManifestLast: Integer;
  KeepBehind: Integer;
  SafeFirst: Integer;
  SafeLast: Integer;

begin

  Result := False;
  ASafeJson := '';

  ManifestFirst := ExtractJsonInteger(AJson,
                                      'first',
                                      0);
  ManifestLast := ExtractJsonInteger(AJson,
                                     'last',
                                     0);
  KeepBehind := ExtractJsonInteger(AJson,
                                   'keepBehind',
                                   RDJ_MSE_KEEP_PATCHED_FRAGMENTS_DEFAULT);

  if (KeepBehind < RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN) then
    KeepBehind := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN
  else
    if (KeepBehind > RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX) then
      KeepBehind := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX;

  if (ManifestFirst <= 0) or
     (ManifestLast <= 0) or
     (ManifestLast < ManifestFirst) or
     (ALastMirroredSeq <= 0) then
    Exit;

  // The public Caddy manifest must describe what the Caddy mirror has
  // actually copied, not what the latest local rolling manifest happened to
  // say when it was queued.  The local manifest can become stale while the
  // mirror thread is busy copying fragments.  If we clamp safeLast to the
  // stale local ManifestLast, Caddy can have patched_frag_000847.m4s on disk
  // while live.json still says last=250.  The browser then waits forever for
  // 251 even though the warehouse is full.
  //
  // FLastMirroredPatchedSeq is advanced only after a successful atomic copy of
  // patched_frag_XXXXXX.m4s, so it is the safest public last value.
  SafeLast := ALastMirroredSeq;

  // Server-side cleanup is deliberately disabled during this milestone.
  // That means Caddy may still have older patched fragments even after the
  // local rolling folder has advanced ManifestFirst.  Do NOT clamp the public
  // Caddy manifest to the local ManifestFirst, otherwise the server manifest
  // collapses to tiny windows like first=267 last=267 while older server
  // fragments are still perfectly fetchable.  Publish a normal rolling server
  // window ending at the highest fragment that the mirror has actually copied.
  SafeFirst := SafeLast - KeepBehind + 1;

  if (SafeFirst < 1) then
    SafeFirst := 1;

  ASafeJson := ReplaceJsonInteger(AJson,
                                  'first',
                                  SafeFirst);
  ASafeJson := ReplaceJsonInteger(ASafeJson,
                                  'last',
                                  SafeLast);

  Result := True;
end;


procedure TRdjProMseMirrorThread.StringToUtf8Bytes(const AText: string;
                                                   out ABytes: TBytes);
var
  Utf8: UTF8String;

begin

  Utf8 := UTF8String(AText);

  SetLength(ABytes,
            Length(Utf8));

  if Length(Utf8) > 0 then
    Move(PAnsiChar(Utf8)^,
         ABytes[0],
         Length(Utf8));
end;


procedure TRdjProMseMirrorThread.StorePendingLiveJson(const AFileName: string;
                                                      const ABytes: TBytes);
begin

  FLock.Acquire();

  try
    FPendingLiveJsonFileName := AFileName;
    Inc(FPendingLiveJsonGeneration);

    SetLength(FPendingLiveJsonBytes,
              Length(ABytes));

    if (Length(ABytes) > 0) then
      Move(ABytes[0],
           FPendingLiveJsonBytes[0],
           Length(ABytes));

    // Milestone 12:
    // OutputDebugString for every fragment/manifest tick can itself become a
    // small brake when running under the Delphi debugger.  Keep the useful
    // breadcrumb trail, but only print it occasionally.
    if (FPendingLiveJsonGeneration <= 5) or
       ((FPendingLiveJsonGeneration mod 25) = 0) then
      OutputDebugString(PChar(Format('TRdjProMseMirrorThread: queued pending live.json gen=%d mirroredLast=%d',
                                     [FPendingLiveJsonGeneration,
                                      FLastMirroredPatchedSeq])));
  finally
    FLock.Release();
  end;

  FEvent.SetEvent();
end;


procedure TRdjProMseMirrorThread.TryFlushPendingLiveJson();
const
  LIVE_JSON_FLUSH_INTERVAL_MS = 1500;

var
  FileName: string;
  Bytes: TBytes;
  Json: string;
  SafeJson: string;
  SafeBytes: TBytes;
  LastMirrored: Integer;
  JobsQueued: Integer;
  NowTick: DWORD;
  LastFlushTick: DWORD;
  FlushElapsed: DWORD;
  WriteOk: Boolean;
  PendingGeneration: Cardinal;
  PublishedLast: Integer;
  SafeFirst: Integer;
  SafeLast: Integer;

begin

  FileName := '';
  SetLength(Bytes,
            0);

  NowTick := GetTickCount();

  FLock.Acquire();

  try
    if (FPendingLiveJsonFileName = '') or
       (Length(FPendingLiveJsonBytes) = 0) then
      Exit;

    JobsQueued := FJobs.Count;
    LastFlushTick := FLastLiveJsonFlushTick;

    // live.json is public state, but writing it after every fragment hammers
    // the SMB/Caddy side and steals time from copying the actual media files.
    // While there are still fragment jobs waiting, do not publish live.json
    // too often.  In the Milestone-11 log the mirror queue reached ~110;
    // frequent atomic live.json writes plus debugger output were stealing time
    // from the actual media copies.  When the queue becomes empty, publish
    // immediately.
    if (JobsQueued > 0) and
       (LastFlushTick <> 0) then
      begin
        FlushElapsed := DWORD(NowTick - LastFlushTick);

        if FlushElapsed < LIVE_JSON_FLUSH_INTERVAL_MS then
          Exit;
      end;

    LastMirrored := FLastMirroredPatchedSeq;
    PendingGeneration := FPendingLiveJsonGeneration;
    PublishedLast := FLastPublishedLiveJsonSeq;
    FileName := FPendingLiveJsonFileName;

    SetLength(Bytes,
              Length(FPendingLiveJsonBytes));

    Move(FPendingLiveJsonBytes[0],
         Bytes[0],
         Length(FPendingLiveJsonBytes));

  finally
    FLock.Release();
  end;

  Json := Utf8BytesToString(Bytes);

  if not BuildSafeLiveJson(Json,
                           LastMirrored,
                           SafeJson) then
    Exit;

  SafeLast := ExtractJsonInteger(SafeJson,
                                 'last',
                                 0);
  SafeFirst := ExtractJsonInteger(SafeJson,
                                  'first',
                                  0);

  if (SafeLast > 0) and
     (SafeLast <= PublishedLast) then
    begin
      FLock.Acquire();
      try
        if (FPendingLiveJsonGeneration = PendingGeneration) and
           SameText(FPendingLiveJsonFileName,
                    FileName) then
          begin
            FPendingLiveJsonFileName := '';
            SetLength(FPendingLiveJsonBytes,
                      0);
          end;
      finally
        FLock.Release();
      end;
      Exit;
    end;

  StringToUtf8Bytes(SafeJson,
                    SafeBytes);

  WriteOk := WriteBytesToFileAtomicResult(FileName,
                                          SafeBytes);

  if not WriteOk then
    begin
      // Keep the pending manifest.  A transient Caddy/SMB lock should not make
      // us forget the newest scoreboard.
      OutputDebugString(PChar(Format('TRdjProMseMirrorThread: live.json write deferred safeLast=%d gen=%d queue=%d',
                                     [SafeLast,
                                      PendingGeneration,
                                      QueueCount()])));
      Exit;
    end;

  FLock.Acquire();

  try
    FLastLiveJsonFlushTick := GetTickCount();

    if (SafeLast > FLastPublishedLiveJsonSeq) then
      FLastPublishedLiveJsonSeq := SafeLast;

    // Clear only the exact pending generation we copied.  A newer live.json can
    // arrive while the SMB/Caddy atomic write is busy.  The old code cleared by
    // filename only, but every manifest is called live.json, so a fresh pending
    // manifest could be deleted silently.
    if (FPendingLiveJsonGeneration = PendingGeneration) and
       SameText(FPendingLiveJsonFileName,
                FileName) then
      begin
        FPendingLiveJsonFileName := '';
        SetLength(FPendingLiveJsonBytes,
                  0);
      end;
  finally
    FLock.Release();
  end;

  if (SafeLast <= 5) or
     ((SafeLast mod 25) = 0) then
    OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirrored safe live.json safeLast=%d mirroredLast=%d gen=%d queue=%d',
                                   [SafeLast,
                                    LastMirrored,
                                    PendingGeneration,
                                    QueueCount()])));

  CleanupMirroredFragmentsAfterManifest(FileName,
                                        SafeFirst,
                                        SafeLast);
end;


function TRdjProMseMirrorThread.IsLowPriorityJob(AJob: TRdjProMseMirrorJob): Boolean;
begin

  Result := False;

  if not Assigned(AJob) then
    Exit;

  if (AJob.Kind = mjkDeleteFile) then
    begin
      Result := True;
      Exit;
    end;

  if (AJob.Kind = mjkCopyFile) then
    Exit;

  if (AJob.Kind = mjkWriteBytes) and
     SameText(ExtractFileName(AJob.FileName),
              'live.json') then
    Result := True;
end;


function TRdjProMseMirrorThread.RemoveOneLowPriorityJob(): Boolean;
var
  I: Integer;
  Job: TRdjProMseMirrorJob;

begin

  Result := False;

  for I := 0 to FJobs.Count - 1 do
    begin
      Job := TRdjProMseMirrorJob(FJobs[I]);

      if IsLowPriorityJob(Job) then
        begin
          FJobs.Delete(I);
          Job.Free();
          Result := True;
          Exit;
        end;
    end;
end;


procedure TRdjProMseMirrorThread.EnqueueWrite(const AFileName: string;
                                              const ABytes: TBytes);
var
  Job: TRdjProMseMirrorJob;
  OldJob: TRdjProMseMirrorJob;

begin

  if Terminated or
     (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  // live.json is not a normal copy job anymore.  It is kept as the latest
  // pending scoreboard and is only written after the mirror thread has actually
  // copied the advertised patched fragments to the Caddy side.
  if IsLiveJsonFile(AFileName) then
    begin
      StorePendingLiveJson(AFileName,
                           ABytes);
      Exit;
    end;

  Job := TRdjProMseMirrorJob.Create();
  Job.Kind := mjkWriteBytes;
  Job.FileName := AFileName;

  SetLength(Job.Bytes,
            Length(ABytes));

  Move(ABytes[0],
       Job.Bytes[0],
       Length(ABytes));

  FLock.Acquire();
  try
    // Bound the network mirror queue, but try very hard not to drop patched
    // fragment write jobs. If the share hiccups, stale delete jobs are
    // disposable; missing media fragments cause browser 404 holes.
    while (FJobs.Count >= FMaxJobs) do
      begin
        if not RemoveOneLowPriorityJob() then
          begin
            OldJob := TRdjProMseMirrorJob(FJobs[0]);
            FJobs.Delete(0);
            OutputDebugString(PChar('TRdjProMseMirrorThread: mirror queue full, dropping oldest media job: ' +
                                    ExtractFileName(OldJob.FileName)));
            OldJob.Free();
          end;
      end;

    FJobs.Add(Job);
    Job := nil;
  finally
    FLock.Release();
    Job.Free();
  end;

  FEvent.SetEvent();
end;


procedure TRdjProMseMirrorThread.EnqueueCopy(const ASourceFileName: string;
                                                     const ADestFileName: string);
var
  Job: TRdjProMseMirrorJob;
  OldJob: TRdjProMseMirrorJob;

begin

  if Terminated or
     (ASourceFileName = '') or
     (ADestFileName = '') then
    Exit;

  Job := TRdjProMseMirrorJob.Create();
  Job.Kind := mjkCopyFile;
  Job.SourceFileName := ASourceFileName;
  Job.FileName := ADestFileName;

  FLock.Acquire();
  try
    // Copy jobs are tiny in memory now, but do not let an unreachable UNC
    // share build an unlimited backlog. If we ever reach the cap, drop old
    // low-priority jobs first. As a last resort drop the oldest copy job; the
    // safe live.json will only advance to files that really get copied.
    while (FJobs.Count >= FMaxJobs) do
      begin
        if not RemoveOneLowPriorityJob() then
          begin
            OldJob := TRdjProMseMirrorJob(FJobs[0]);
            FJobs.Delete(0);
            OutputDebugString(PChar('TRdjProMseMirrorThread: mirror queue full, dropping oldest job: ' +
                                    ExtractFileName(OldJob.FileName)));
            OldJob.Free();
          end;
      end;

    FJobs.Add(Job);
    Job := nil;
  finally
    FLock.Release();
    Job.Free();
  end;

  FEvent.SetEvent();
end;


procedure TRdjProMseMirrorThread.EnqueueDelete(const AFileName: string);
var
  Job: TRdjProMseMirrorJob;

begin

  if Terminated or
     (AFileName = '') then
    Exit;

  Job := TRdjProMseMirrorJob.Create();
  Job.Kind := mjkDeleteFile;
  Job.FileName := AFileName;

  FLock.Acquire();

  try
    FJobs.Add(Job);
    Job := nil;
  finally
    FLock.Release();
    Job.Free();
  end;

  FEvent.SetEvent();
end;


function TRdjProMseMirrorThread.PopJob(): TRdjProMseMirrorJob;
begin

  Result := nil;

  FLock.Acquire();

  try
    if (FJobs.Count > 0) then
      begin
        Result := TRdjProMseMirrorJob(FJobs[0]);
        FJobs.Delete(0);
      end;
  finally
    FLock.Release();
  end;
end;


procedure TRdjProMseMirrorThread.WriteBytesToFile(const AFileName: string;
                                                  const ABytes: TBytes);
var
  Stream: TFileStream;

begin

  if (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  ForceDirectories(ExtractFilePath(AFileName));

  Stream := TFileStream.Create(AFileName,
                               fmCreate or fmShareDenyNone);
  try
    Stream.WriteBuffer(ABytes[0],
                       Length(ABytes));
    // Milestone 9:
    // Do not FlushFileBuffers() for every mirrored media fragment.
    // On SMB/UNC shares this is very expensive and the log showed the
    // mirror queue climbing above 100 pending fragment jobs.  Atomic temp
    // write + rename is enough for Caddy/MSE visibility; durability after
    // a power loss is not important for live scratch fragments.
  finally
    Stream.Free();
  end;
end;


function TRdjProMseMirrorThread.WriteBytesToFileAtomicResult(const AFileName: string;
                                                              const ABytes: TBytes): Boolean;
var
  TmpFileName: string;
  I: Integer;

begin

  Result := False;

  if (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  ForceDirectories(ExtractFilePath(AFileName));

  TmpFileName := AFileName +
                 Format('.tmp_%x_%x_%x',
                        [GetCurrentProcessId(),
                         GetCurrentThreadId(),
                         GetTickCount()]);

  WriteBytesToFile(TmpFileName,
                   ABytes);

  for I := 0 to 20 do
    begin
      if MoveFileEx(PChar(TmpFileName),
                    PChar(AFileName),
                    MOVEFILE_REPLACE_EXISTING) then
        begin
          Result := True;
          Exit;
        end;

      Sleep(5);
    end;

  OutputDebugString(PChar(Format('TRdjProMseMirrorThread: atomic mirror write failed, keeping old file: %s err=%d',
                                 [ExtractFileName(AFileName),
                                  GetLastError()])));

  DeleteFile(TmpFileName);
end;


procedure TRdjProMseMirrorThread.DeleteMirroredFragmentFiles(const ADir: string;
                                                            const ASeq: Integer);
var
  Dir: string;
  PatchedFileName: string;
  RawFileName: string;

begin

  if (ASeq <= 0) or
     (Trim(ADir) = '') then
    Exit;

  Dir := IncludeTrailingPathDelimiter(Trim(ADir));

  PatchedFileName := Dir +
                     Format('patched_frag_%.6d.m4s',
                            [ASeq]);

  if FileExists(PatchedFileName) and (not DeleteFile(PatchedFileName)) then
    OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirror accountant delete failed: %s err=%d',
                                   [ExtractFileName(PatchedFileName),
                                    GetLastError()])));

  RawFileName := Dir +
                 Format('frag_%.6d.m4s',
                        [ASeq]);

  if FileExists(RawFileName) and (not DeleteFile(RawFileName)) then
    OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirror accountant raw delete failed: %s err=%d',
                                   [ExtractFileName(RawFileName),
                                    GetLastError()])));
end;


procedure TRdjProMseMirrorThread.ScanOldMirroredFragments(const ADir: string;
                                                         const ADeleteThroughSeq: Integer;
                                                         const ASafeLast: Integer);
var
  SearchRec: TSearchRec;
  Dir: string;
  Seq: Integer;
  Deleted: Integer;
  DeleteAboveSeq: Integer;

begin

  if ((ADeleteThroughSeq <= 0) and (ASafeLast <= 0)) or
     (Trim(ADir) = '') then
    Exit;

  Dir := IncludeTrailingPathDelimiter(Trim(ADir));

  if not DirectoryExists(Dir) then
    Exit;

  Deleted := 0;
  DeleteAboveSeq := ASafeLast + RDJ_MSE_MIRROR_CLEANUP_SAFETY_FRAGMENTS;

  if FindFirst(Dir + 'patched_frag_*.m4s',
               faAnyFile,
               SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = 0) then
          begin
            Seq := ExtractPatchedFragmentSeq(SearchRec.Name);

            if (Seq > 0) and
               ((Seq <= ADeleteThroughSeq) or ((ASafeLast > 0) and (Seq > DeleteAboveSeq))) then
              begin
                DeleteMirroredFragmentFiles(Dir,
                                            Seq);
                Inc(Deleted);
              end;
          end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;

  if Deleted > 0 then
    OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirror accountant orphan scan deleted=%d through=%d safeLast=%d',
                                   [Deleted,
                                    ADeleteThroughSeq,
                                    ASafeLast])));
end;


procedure TRdjProMseMirrorThread.CleanupMirroredFragmentsAfterManifest(const ALiveJsonFileName: string;
                                                                      const ASafeFirst: Integer;
                                                                      const ASafeLast: Integer);
var
  MirrorDir: string;
  DeleteThroughSeq: Integer;
  Seq: Integer;
  NowTick: DWORD;
  DoScan: Boolean;

begin

  if (not RDJ_MSE_MIRROR_ACCOUNTANT_ENABLED) or
     (ALiveJsonFileName = '') or
     (ASafeFirst <= 0) or
     (ASafeLast < ASafeFirst) then
    Exit;

  DeleteThroughSeq := ASafeFirst - 1 - RDJ_MSE_MIRROR_CLEANUP_SAFETY_FRAGMENTS;

  MirrorDir := IncludeTrailingPathDelimiter(ExtractFilePath(ALiveJsonFileName));

  if (MirrorDir = '') then
    Exit;

  if (DeleteThroughSeq > 0) and (DeleteThroughSeq > FLastMirrorCleanupSeq) then
    begin
      for Seq := FLastMirrorCleanupSeq + 1 to DeleteThroughSeq do
        DeleteMirroredFragmentFiles(MirrorDir,
                                    Seq);

      FLastMirrorCleanupSeq := DeleteThroughSeq;

      if (DeleteThroughSeq <= 5) or ((DeleteThroughSeq mod 25) = 0) then
        OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirror accountant cleanup safeFirst=%d safeLast=%d deletedThrough=%d',
                                       [ASafeFirst,
                                        ASafeLast,
                                        FLastMirrorCleanupSeq])));
    end;

  NowTick := GetTickCount();
  DoScan := (FLastMirrorOrphanScanTick = 0) or
            (DWORD(NowTick - FLastMirrorOrphanScanTick) >= RDJ_MSE_MIRROR_ORPHAN_SCAN_INTERVAL_MS);

  if DoScan then
    begin
      FLastMirrorOrphanScanTick := NowTick;
      ScanOldMirroredFragments(MirrorDir,
                               DeleteThroughSeq,
                               ASafeLast);
    end;
end;


procedure TRdjProMseMirrorThread.AdvanceMirroredPatchedSeq(const ASeq: Integer);
var
  Gap: Integer;

begin

  if (ASeq <= 0) then
    Exit;

  FLock.Acquire();

  try
    if (ASeq <= FLastMirroredPatchedSeq) then
      Exit;

    if (ASeq = (FLastMirroredPatchedSeq + 1)) then
      begin
        FLastMirroredPatchedSeq := ASeq;
        Exit;
      end;

    Gap := ASeq - FLastMirroredPatchedSeq - 1;

    // If the missing fragment is already far outside the public live window,
    // waiting for it will freeze the Caddy manifest forever. Resync to the
    // newest successfully mirrored fragment and let live.json publish a fresh
    // rolling window.
    if (Gap >= RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX) then
      begin
        OutputDebugString(PChar(Format('TRdjProMseMirrorThread: resync stale mirror gap oldSafeLast=%d got=%d gap=%d',
                                       [FLastMirroredPatchedSeq,
                                        ASeq,
                                        Gap])));
        FLastMirroredPatchedSeq := ASeq;
        Exit;
      end;

    // Short gaps can still be filled by queued jobs. Log sparsely; this path can
    // otherwise overflow the Delphi debugger after long broadcasts.
    if (ASeq <= 5) or ((ASeq mod 25) = 0) then
      OutputDebugString(PChar(Format('TRdjProMseMirrorThread: mirror sequence gap, keeping safeLast=%d got=%d gap=%d',
                                     [FLastMirroredPatchedSeq,
                                      ASeq,
                                      Gap])));
  finally
    FLock.Release();
  end;
end;


procedure TRdjProMseMirrorThread.Execute();
var
  Job: TRdjProMseMirrorJob;
  Seq: Integer;
  WriteOk: Boolean;

begin

  while not Terminated do
    begin
      Job := PopJob();

      if not Assigned(Job) then
        begin
          TryFlushPendingLiveJson();
          FEvent.WaitFor(100);
          Continue;
        end;

      try
        try
          case Job.Kind of
            mjkWriteBytes:
              begin
                Seq := ExtractPatchedFragmentSeq(Job.FileName);

                // Patched media fragment names are unique. They are not
                // referenced by public live.json until this write has returned
                // and FLastMirroredPatchedSeq is advanced below.  Therefore an
                // expensive temp-file + rename transaction is unnecessary for
                // fragments and was keeping the Caddy mirror about 50-60
                // fragments behind. Keep atomic publishing for init.mp4 and
                // live.json, but write patched_frag_NNNNNN.m4s directly.
                if (Seq > 0) then
                  begin
                    WriteBytesToFile(Job.FileName,
                                     Job.Bytes);

                    WriteOk := FileExists(Job.FileName);
                  end
                else
                  WriteOk := WriteBytesToFileAtomicResult(Job.FileName,
                                                          Job.Bytes);

                // Only advance FLastMirroredPatchedSeq after the file was
                // actually written on the public side.
                if WriteOk and (Seq > 0) then
                  AdvanceMirroredPatchedSeq(Seq);
              end;

            mjkCopyFile:
              begin
                Seq := ExtractPatchedFragmentSeq(Job.FileName);
                ForceDirectories(ExtractFilePath(Job.FileName));

                if FileExists(Job.SourceFileName) then
                  begin
                    WriteOk := CopyFile(PChar(Job.SourceFileName),
                                        PChar(Job.FileName),
                                        False);

                    if (not WriteOk) then
                      OutputDebugString(PChar(Format('TRdjProMseMirrorThread: CopyFile failed: %s err=%d',
                                                     [ExtractFileName(Job.FileName),
                                                      GetLastError()])));
                  end
                else
                  begin
                    WriteOk := False;
                    OutputDebugString(PChar('TRdjProMseMirrorThread: source fragment already gone: ' +
                                            ExtractFileName(Job.SourceFileName)));
                  end;

                if WriteOk and (Seq > 0) then
                  AdvanceMirroredPatchedSeq(Seq);
              end;

            mjkDeleteFile:
              if FileExists(Job.FileName) then
                DeleteFile(Job.FileName);
          end;
        except
          on E: Exception do
            OutputDebugString(PChar(Format('TRdjProMseMirrorThread: job failed: %s err=%s',
                                           [ExtractFileName(Job.FileName),
                                            E.Message])));
        end;
      finally
        Job.Free();
      end;

      TryFlushPendingLiveJson();
    end;
end;


procedure TRdjProMseMirrorThread.StopAndWait();
begin

  Terminate();

  if Assigned(FEvent) then
    FEvent.SetEvent();

  WaitFor();
end;


procedure TfrmMediaServer.btnMinimizeClick(Sender: TObject);
begin

  WindowState := wsMinimized;
  if Assigned(MainMDIFrm) then
    MainMDIFrm.SetMediaServerButtonChecked(False);
end;


function TfrmMediaServer.IsBroadcasting(): Boolean;
begin

  Result := FBroadcastPublishing;
end;


// FRecordingRdjPro
procedure TfrmMediaServer.btnRdjProRecordClick(Sender: TObject);
var
  FileName: string;
  bSuccess: Boolean;

begin

  if (btnRdjProRecord.Tag = 0) then
    begin

      FileName := ResolveLocalRecordingPath();

      bSuccess := StartRdjProRecording(FileName,
                                       FRecordVideoOnly);

      if not bSuccess then
        begin
          FRecordingRdjPro := False;
          btnRdjProRecord.Tag := 0;
          UpdateRecordingUi();
          Exit;
        end;

      FRecordingRdjPro := True;
      btnRdjProRecord.Tag := 1;

      prStopwatch := TStopwatch.StartNew();
      FTimerRunning := True;
      tmrTime.Enabled := True;

      UpdateRecordingUi();
      Exit;
    end;

  StopRdjProRecording();

  FRecordingRdjPro := False;
  btnRdjProRecord.Tag := 0;

  if FTimerRunning then
    begin

      prStopwatch.Stop;
      FTimerRunning := False;
      if not FRdjProBroadcasting then
        tmrTime.Enabled := True;
      UpdateTimeLabel();
    end;

  UpdateRecordingUi();
end;


procedure TfrmMediaServer.chkBroadcastClick(Sender: TObject);
begin

  if chkBroadcast.Checked then
    begin

      if not StartRdjProBroadcast() then
        begin

          chkBroadcast.Checked := False;
          chkBroadcast.Down := False;
          UpdateOnAirLamp(False);
          memLog.Lines.Append('Broadcasting stopped.');
        end;

      UpdateCastControls();
      Exit;
    end;

  StopRdjProBroadcast();
  UpdateCastControls();
end;


procedure TfrmMediaServer.chkRecordVideoOnlyClick(Sender: TObject);
begin

  FRecordVideoOnly := chkRecordVideoOnly.Checked;
end;


procedure TfrmMediaServer.chkRecorderCtrlClick(Sender: TObject);
begin

  pnlRecorderCtrl.BringToFront();
  chkServerCtrl.Checked := False;
  chkRecorderCtrl.Checked := True;
end;


procedure TfrmMediaServer.chkServerCtrlClick(Sender: TObject);
begin

  pnlServerCtrl.BringToFront;
  chkServerCtrl.Checked := True;
  chkRecorderCtrl.Checked := False;
end;


procedure TfrmMediaServer.chkRdjProCameraClick(Sender: TObject);
var
  ModeChanged: Boolean;

begin

  if FUpdatingRdjProVideoSourceButtons then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  ModeChanged := FRdjProStaticImage;

  if FRdjProStaticImage then
    begin

      if FRdjProBroadcasting and Assigned(FRdjProCaptureManager) then
        FRdjProCaptureManager.StopVideoSourceReader(150);

      StopRdjProCamera();
      FRdjProCaptureInitialized := False;
    end;

  SetRdjProStaticImageMode(False);
  FStaticVideoFrameIndex := 0;
  FStaticVideoStartTick := 0;
  FStaticVideoLastTick := 0;
  HideRdjProStaticPreview();

  if ModeChanged and FRdjProBroadcasting then
    PrepareBroadcastMseVideoSourceSwitch();

  if not FRdjProPreviewing then
    begin
      if FAILED(StartRdjProCamera(pnlRdjProPreviewHost.Handle)) then
        begin
          if ModeChanged then
            begin
              SetRdjProStaticImageMode(True);
              ShowRdjProStaticPreview();
            end
          else
            SetRdjProVideoSourceButtons(False,
                                        False);

          Exit;
        end;
    end;

  if ModeChanged and FRdjProBroadcasting then
    begin
      if not EnsureRdjProVideoSampleReader(FActiveBroadcastVideoMediaType) then
        begin
          StopRdjProCamera();
          SetRdjProStaticImageMode(True);
          ShowRdjProStaticPreview();
          Exit;
        end;
    end;

  if FRdjProPreviewing and not FRdjProStaticImage then
    begin
      RefreshRdjProCameraPreview();
      ScheduleRdjProCameraPreviewRefresh();
    end;

  if ModeChanged and FRdjProBroadcasting then
    OutputDebugString(PChar('RDJ Pro live source switched to camera without recorder restart.'));
end;


procedure TfrmMediaServer.chkRdjProStaticImageClick(Sender: TObject);
var
  ModeChanged: Boolean;
  StaticImageFileName: string;

begin

  if FUpdatingRdjProVideoSourceButtons then
    Exit;

  if FRdjProRecording then
    begin
      SetRdjProVideoSourceButtons(FRdjProStaticImage,
                                  not FRdjProStaticImage);
      Exit;
    end;

  StaticImageFileName := PickRdjProStaticImageFileName();
  if StaticImageFileName = '' then
    begin
      SetRdjProVideoSourceButtons(FRdjProStaticImage,
                                  not FRdjProStaticImage);
      Exit;
    end;

  FStaticImageFileName := StaticImageFileName;
  FStaticVideoBuffer := nil;
  FStaticVideoBufferWidth := 0;
  FStaticVideoBufferHeight := 0;

  ModeChanged := not FRdjProStaticImage;
  SetRdjProStaticImageMode(True);

  // Keep live MP4 timestamps monotonic when only replacing the static image.
  // Resetting the clock while the fMP4 SinkWriter is active can make the next
  // video samples jump back to zero and stall public .m4s publication.
  if ModeChanged or not (FRdjProRecording or FRdjProBroadcasting) then
    begin
      FStaticVideoFrameIndex := 0;
      FStaticVideoStartTick := 0;
      FStaticVideoLastTick := 0;
    end;

  FreeAndNil(FStaticVideoBitmap);
  EnsureRdjProStaticVideoFrame();

  chkRdjProCamera.Enabled := not FRecordingRdjPro;

  if FRdjProBroadcasting then
    PrepareBroadcastMseVideoSourceSwitch();

  QueueRdjProStaticVideoSample();

  if not FRdjProBroadcasting then
    begin
      if Assigned(FRdjProCaptureManager) then
        FRdjProCaptureManager.StopVideoSourceReader(50);
    end;

  StopRdjProCamera();
  ResetRdjProCaptureManager();
  ShowRdjProStaticPreview();

  if ModeChanged and FRdjProBroadcasting then
    OutputDebugString(PChar('RDJ Pro live source switched to static image without recorder restart.'));
end;


procedure TfrmMediaServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

begin

  CanClose := False;
  Hide();

  if Assigned(MainMDIFrm) then
    MainMDIFrm.RefreshMainButtonStates();
end;


procedure TfrmMediaServer.CastDeviceChanged(const ADevice: TMfCastDevice);
begin

  if not FCastClosing then
    PostMessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TfrmMediaServer.CastDeviceRemoved(const ADeviceId: string);
begin

  if not FCastClosing then
    PostMessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TfrmMediaServer.CastStateChanged(const AOldState,
                                                  ANewState: TMfCastState);
begin

  if not FCastClosing then
    PostMessage(Handle,
                WM_MFCAST_STATE,
                Ord(ANewState),
                0);
end;


procedure TfrmMediaServer.CastMediaStatus(const AStatus: TMfCastMediaStatus);
var
  StatusMessage: TMfRdjCastStatusMessage;

begin

  if FCastClosing then
    Exit;

  StatusMessage := TMfRdjCastStatusMessage.Create(AStatus);
  if not PostMessage(Handle,
                     WM_MFCAST_STATUS,
                     WPARAM(StatusMessage),
                     0) then
    StatusMessage.Free();
end;


procedure TfrmMediaServer.CastError(const AError: TMfCastErrorInfo);
var
  ErrorText: string;

begin

  ErrorText := Format('Cast error [%s]: %s (HRESULT $%.8x)',
                      [AError.Stage,
                       AError.MessageText,
                       DWORD(AError.HResult)]);

  if (AError.Detail <> '') then
    ErrorText := ErrorText + ' ' + AError.Detail;

  PostCastText(WM_MFCAST_ERROR,
               ErrorText);
end;


procedure TfrmMediaServer.CastLog(Sender: TObject;
                                  const ALevel: TMfCastLogLevel;
                                  const ASource,
                                        AMessage: string);
begin

  // Normal state is already visible in the compact Cast status label. Keep the
  // broadcast log useful by forwarding warnings and diagnostics only.
  if ALevel in [cllWarning, cllError] then
    PostCastText(WM_MFCAST_LOG,
                 Format('Cast [%s] %s',
                        [ASource,
                         AMessage]));
end;


procedure TfrmMediaServer.PostCastText(const AWindowsMessage: Cardinal;
                                       const AText: string);
var
  LogMessage: TMfCastUiLogMessage;

begin

  if FCastClosing then
    Exit;

  LogMessage := TMfCastUiLogMessage.Create(AText);
  if not PostMessage(Handle,
                     AWindowsMessage,
                     WPARAM(LogMessage),
                     0) then
    LogMessage.Free();
end;


procedure TfrmMediaServer.ReloadCastDevices();
var
  I: Integer;
  OldDeviceId: string;
  Hr: HRESULT;

begin

  OldDeviceId := '';
  if (cmbCastDevices.ItemIndex >= 0) and
     (cmbCastDevices.ItemIndex < Length(FCastDevices)) then
    OldDeviceId := FCastDevices[cmbCastDevices.ItemIndex].Id;

  if not Assigned(FCast) then
    Exit;

  Hr := FCast.GetDevices(FCastDevices);
  if FAILED(Hr) then
    begin
      LogCastResult('Read Cast devices',
                    Hr);
      Exit;
    end;

  cmbCastDevices.Items.BeginUpdate();
  try
    cmbCastDevices.Clear();
    for I := 0 to Length(FCastDevices) - 1 do
      begin
        cmbCastDevices.Items.Add(FCastDevices[I].FriendlyName);
        if (OldDeviceId <> '') and
           SameText(FCastDevices[I].Id,
                    OldDeviceId) then
          cmbCastDevices.ItemIndex := I;
      end;

    if (cmbCastDevices.ItemIndex < 0) and
       (cmbCastDevices.Items.Count > 0) then
      cmbCastDevices.ItemIndex := 0;
  finally
    cmbCastDevices.Items.EndUpdate();
  end;

  UpdateCastControls();
end;


procedure TfrmMediaServer.UpdateCastControls();
var
  CastState: TMfCastState;
  CanControl: Boolean;
  CanSetVolume: Boolean;

begin

  if Assigned(FCast) then
    CastState := FCast.State()
  else
    CastState := csError;

  btnCastDiscover.Enabled := Assigned(FCast) and (not FCastClosing);
  cmbCastDevices.Enabled := Assigned(FCast) and (not Assigned(FCastWorker)) and
                             (CastState in [csIdle,
                                            csDiscovering,
                                            csStopped,
                                            csError]);

  btnCastLive.Enabled := Assigned(FCast) and
                          (cmbCastDevices.ItemIndex >= 0) and
                          (cmbCastDevices.ItemIndex < Length(FCastDevices)) and
                          (not FPendingCastLive) and
                          (not Assigned(FCastWorker)) and
                          (CastState in [csIdle,
                                         csDiscovering,
                                         csStopped,
                                         csError]);

  CanControl := Assigned(FCast) and
                (CastState in [csConnected,
                               csLaunchingReceiver,
                               csPreparingMedia,
                               csBuffering,
                               csPlaying,
                               csPaused,
                               csStopping]);
  btnCastStop.Enabled := CanControl;
  CanSetVolume := CanControl and FCastMediaSessionReady;
  chkCastMuted.Enabled := CanSetVolume;
  trkCastVolume.Enabled := CanSetVolume;

  if not Assigned(FCast) then
    lblCastState.Caption := 'Cast unavailable'
  else
    lblCastState.Caption := MfCastStateToString(CastState);
end;


procedure TfrmMediaServer.ClearCastLiveByteStream();
begin

  if not Assigned(FCastLiveStreamLock) then
    begin
      FCastLiveByteStream := nil;
      FCastLiveFragmentCount := 0;
      Exit;
    end;

  FCastLiveStreamLock.Enter();
  try
    FCastLiveByteStream := nil;
    FCastLiveFragmentCount := 0;
    if Assigned(FCastLiveRebaser) then
      FCastLiveRebaser.Reset();
  finally
    FCastLiveStreamLock.Leave();
  end;
end;


procedure TfrmMediaServer.ForwardCastLiveFragment(const AFragment: TBytes);
var
  Stream: IMFByteStream;
  RebasedFragment: TBytes;
  BytesWritten: ULONG;
  Hr: HRESULT;
  FragmentNo: Cardinal;

begin

  if (Length(AFragment) <= 0) or
     (not Assigned(FCastLiveStreamLock)) then
    Exit;

  FCastLiveStreamLock.Enter();
  try
    Stream := FCastLiveByteStream;
    if Assigned(Stream) then
      begin
        if (not Assigned(FCastLiveRebaser)) or
           (not FCastLiveRebaser.Rebase(AFragment,
                                        RebasedFragment)) then
          begin
            OutputDebugString(PChar('TfrmMediaServer.Cast live fMP4 rebase failed; fragment was not published.'));
            Exit;
          end;
        Inc(FCastLiveFragmentCount);
        FragmentNo := FCastLiveFragmentCount;
      end
    else
      FragmentNo := 0;
  finally
    FCastLiveStreamLock.Leave();
  end;

  if not Assigned(Stream) then
    Exit;

  BytesWritten := 0;
  Hr := Stream.Write(@RebasedFragment[0],
                     Length(RebasedFragment),
                     BytesWritten);
  if FAILED(Hr) or (BytesWritten <> ULONG(Length(RebasedFragment))) then
    begin
      OutputDebugString(PChar(Format('TfrmMediaServer.Cast live fMP4 write failed: fragment=%d bytes=%d written=%d HRESULT=$%.8x',
                                     [FragmentNo,
                                      Length(RebasedFragment),
                                      BytesWritten,
                                      DWORD(Hr)])));
      ClearCastLiveByteStream();
      Exit;
    end;

  if (FragmentNo <= 5) or ((FragmentNo mod 25) = 0) then
    OutputDebugString(PChar(Format('TfrmMediaServer.Cast live fMP4 write: fragment=%d bytes=%d',
                                   [FragmentNo,
                                    BytesWritten])));
end;


procedure TfrmMediaServer.TryStartPendingCastLive();
var
  InitSegment: TBytes;
  DeviceName: string;

begin

  if (not FPendingCastLive) or
     (not FCastProducerRequested) or
     (not Assigned(FCast)) or
     Assigned(FCastWorker) then
    Exit;

  if (not Assigned(FRdjProBroadcastMp4Recorder)) or
     (not FRdjProBroadcastMp4Recorder.Active) or
     (not FRdjProBroadcastMp4Recorder.GetInitSegment(InitSegment)) or
     (Length(InitSegment) <= 24) then
    Exit;

  FPendingCastLive := False;
  ClearCastLiveByteStream();
  DeviceName := FPendingCastDevice.FriendlyName;
  FCastWorker := TMfCastLiveFmp4Worker.Create(FCast,
                                              FPendingCastDevice,
                                              InitSegment,
                                              Handle);
  FCastWorker.Start();
  lblCastState.Caption := 'Connecting...';
  memLog.Lines.Add('Casting the live fragmented MP4 stream to ' +
                   DeviceName + '.');
  UpdateCastControls();
end;


procedure TfrmMediaServer.LogCastResult(const AOperation: string;
                                        const AHResult: HRESULT);
begin

  if SUCCEEDED(AHResult) then
    memLog.Lines.Add(AOperation + ': OK')
  else
    memLog.Lines.Add(Format('%s failed (HRESULT $%.8x)',
                            [AOperation,
                             DWORD(AHResult)]));
end;


procedure TfrmMediaServer.WmCastDevices(var Message: TMessage);
begin

  ReloadCastDevices();
end;


procedure TfrmMediaServer.WmCastState(var Message: TMessage);
begin

  lblCastState.Caption := MfCastStateToString(TMfCastState(Message.WParam));
  UpdateCastControls();
end;


procedure TfrmMediaServer.WmCastStatus(var Message: TMessage);
var
  StatusMessage: TMfRdjCastStatusMessage;
  StatusText: string;

begin

  StatusMessage := TMfRdjCastStatusMessage(Message.WParam);
  try
    StatusText := StatusMessage.Status.PlayerState;
    if StatusText = '' then
      StatusText := 'Connected';

    lblCastState.Caption := StatusText;
    FCastMediaSessionReady := StatusMessage.Status.MediaSessionId <> 0;

    if FCastMediaSessionReady then
      begin
        FCastUpdatingVolume := True;
        try
          trkCastVolume.Position := EnsureRange(Round(StatusMessage.Status.Volume * 100.0),
                                                trkCastVolume.Minimum,
                                                trkCastVolume.Maximum);
          chkCastMuted.Checked := StatusMessage.Status.Muted;
        finally
          FCastUpdatingVolume := False;
        end;
      end;

    UpdateCastControls();
  finally
    StatusMessage.Free();
  end;
end;


procedure TfrmMediaServer.WmCastError(var Message: TMessage);
var
  ErrorMessage: TMfCastUiLogMessage;

begin

  ErrorMessage := TMfCastUiLogMessage(Message.WParam);
  try
    memLog.Lines.Add(ErrorMessage.Text);
    lblCastState.Caption := 'Cast error';
    UpdateCastControls();
  finally
    ErrorMessage.Free();
  end;
end;


procedure TfrmMediaServer.WmCastFinished(var Message: TMessage);
var
  LiveWorker: TMfCastLiveFmp4Worker;

begin

  if Assigned(FCastWorker) then
    begin
      FCastWorker.WaitFor();

      if (HRESULT(Message.WParam) = S_OK) and
         (FCastWorker is TMfCastLiveFmp4Worker) then
        begin
          LiveWorker := TMfCastLiveFmp4Worker(FCastWorker);
          FCastLiveStreamLock.Enter();
          try
            FCastLiveByteStream := LiveWorker.ByteStream;
            FCastLiveFragmentCount := 0;
            FCastLiveRebaser.Reset();
          finally
            FCastLiveStreamLock.Leave();
          end;
          OutputDebugString(PChar('TfrmMediaServer.Cast live fMP4 stream attached to the RDJ fragment producer.'));
        end;

      FreeAndNil(FCastWorker);
    end;

  if FAILED(HRESULT(Message.WParam)) then
    begin
      FCastProducerRequested := False;
      FPendingCastLive := False;
      ClearCastLiveByteStream();
      if not FBroadcastPublishing then
        StopRdjProBroadcast();
    end;

  LogCastResult('Cast live stream',
                HRESULT(Message.WParam));
  UpdateCastControls();
end;


procedure TfrmMediaServer.WmCastLog(var Message: TMessage);
var
  LogMessage: TMfCastUiLogMessage;

begin

  LogMessage := TMfCastUiLogMessage(Message.WParam);
  try
    memLog.Lines.Add(LogMessage.Text);
  finally
    LogMessage.Free();
  end;
end;


procedure TfrmMediaServer.btnCastDiscoverClick(Sender: TObject);
begin

  if not Assigned(FCast) then
    Exit;

  cmbCastDevices.Clear();
  SetLength(FCastDevices,
            0);

  lblCastState.Caption := 'Discovering...';
  LogCastResult('Cast discovery',
                FCast.Discover());
  UpdateCastControls();
end;


procedure TfrmMediaServer.btnCastLiveClick(Sender: TObject);
begin

  if (not Assigned(FCast)) or
     Assigned(FCastWorker) or
     (cmbCastDevices.ItemIndex < 0) or
     (cmbCastDevices.ItemIndex >= Length(FCastDevices)) then
    Exit;

  FCastProducerRequested := True;
  FPendingCastLive := True;
  FCastMediaSessionReady := False;
  FPendingCastDevice := FCastDevices[cmbCastDevices.ItemIndex];

  if (not FRdjProBroadcasting) and
     (not StartRdjProBroadcast(False)) then
    begin
      FCastProducerRequested := False;
      FPendingCastLive := False;
      FPendingCastDevice := Default(TMfCastDevice);
      Exit;
    end;

  lblCastState.Caption := 'Preparing live stream...';
  memLog.Lines.Add('Preparing the live fragmented MP4 stream for Cast.');
  TryStartPendingCastLive();
  UpdateCastControls();
end;


procedure TfrmMediaServer.btnCastStopClick(Sender: TObject);
var
  Hr: HRESULT;

begin

  if not Assigned(FCast) then
    Exit;

  FCastProducerRequested := False;
  FPendingCastLive := False;
  FCastMediaSessionReady := False;
  FPendingCastDevice := Default(TMfCastDevice);
  ClearCastLiveByteStream();
  Hr := FCast.Stop();

  if FAILED(Hr) then
    LogCastResult('Stop Cast playback',
                  Hr);

  Hr := FCast.Disconnect();
  LogCastResult('Disconnect Cast device',
                Hr);

  if not FBroadcastPublishing then
    StopRdjProBroadcast();

  UpdateCastControls();
end;


procedure TfrmMediaServer.chkCastMutedClick(Sender: TObject);
begin

  if FCastUpdatingVolume then
    Exit;

  if Assigned(FCast) and chkCastMuted.Enabled then
    LogCastResult('Set Cast mute',
                  FCast.SetMuted(chkCastMuted.Checked));
end;


procedure TfrmMediaServer.trkCastVolumeChange(Sender: TObject);
var
  Hr: HRESULT;

begin

  if FCastUpdatingVolume then
    Exit;

  if Assigned(FCast) and trkCastVolume.Enabled then
    begin
      Hr := FCast.SetVolume(trkCastVolume.Position / 100.0);
      if FAILED(Hr) then
        LogCastResult('Set Cast volume',
                      Hr);
    end;
end;


procedure TfrmMediaServer.cmbCastDevicesChange(Sender: TObject);
begin

  UpdateCastControls();
end;


procedure TfrmMediaServer.FormCreate(Sender: TObject);
begin

  chkBroadcast.Checked := False;
  chkBroadcast.Down := False;
  chkBroadcast.Enabled := True;

  btnRdjProRecord.Tag := 0;
  btnRdjProRecord.Enabled := True;
  FRecordVideoOnly := False;
  FUpdatingRdjProVideoSourceButtons := False;
  FRdjProStaticImage := False;
  SetRdjProVideoSourceButtons(False,
                              False);

  UpdateOnAirLamp(False);
  UpdateRecorderLamp(False);
  SetBroadcastHealth('HEALTH: IDLE',
                     HEALTH_IDLE_COLOR,
                     'Broadcast health idle.');

  // Form-owned Sample-2 capture engine instance.
  FRdjProCaptureManager := TRdjProCaptureManager.Create(Handle);
  FRdjProMp4Recorder := TRdjProMp4Recorder.Create();
  FRdjProBroadcastMp4Recorder := TRdjProFMp4Recorder.Create();

  FRdjProPreviewing := False;
  FRdjProRecording := False;
  FRdjProBroadcasting := False;
  FBroadcastPublishing := False;
  FRdjProCaptureInitialized := False;
  FPreviewFramePending := False;
  FLastRdjProAudioWfxValid := False;
  FStaticImageFileName := '';
  FPendingLocalRecording := False;
  FPendingLocalFileName := '';
  FPendingLocalVideoOnly := False;
  FPendingBroadcastRecording := False;
  FPendingBroadcastFileName := '';
  FPendingBroadcastVideoOnly := False;
  FCompletingPendingMp4Start := False;
  FBroadcastMseDumpDir := '';
  FBroadcastMseMirrorDir := '';
  FBroadcastMseInitWritten := False;
  FBroadcastMseInitSize := 0;
  FBroadcastMseFragmentSeq := 0;
  FBroadcastMsePublicSeq := 0;
  FBroadcastMseGroupPartCount := 0;
  SetLength(FBroadcastMseGroupBytes, 0);
  FBroadcastMseGroupStream := TMemoryStream.Create();
  FBroadcastMseGroupFirstTick := 0;
  FBroadcastMseForceNextPublicGroup := False;
  FBroadcastMsePublicTargetMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_DEFAULT_MS;
  FBroadcastMseGroupSourceFragments := RDJ_MSE_GROUP_SOURCE_FRAGMENTS_DEFAULT;
  FBroadcastMseKeepPatchedFragments := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_DEFAULT;
  FBroadcastMseManifestWritten := False;
  FBroadcastMseLastManifestSeq := 0;
  FBroadcastMseLastCleanupSeq := 0;
  FBroadcastMseManifestPublishSeq := 0;
  FBroadcastMseSessionId := FormatDateTime('yyyymmddhhnnsszzz', Now);
  FBroadcastMseSessionStartedUtc := TTimeZone.Local.ToUniversalTime(Now);
  FBroadcastMseMemLastLogTick := 0;
  FBroadcastHandoverLockAcquired := False;
  FBroadcastHandoverOwnerId := '';
  SetBroadcastHandoverLockIndicator(CAP_UNLOCKED,
                                    UNLOCKED_COLOR);
  FLastBroadcastHandoverHeartbeatTick := 0;
  FLastBroadcastHandoverPollTick := 0;
  FLastBroadcastHandoverNotice := '';
  FLastBroadcastHealthCaption := '';
  FLastBroadcastHealthMessage := '';
  FLastBroadcastHealthLogTick := 0;
  FBroadcastMseRecorderRestartQueued := 0;
  FBroadcastMseRecorderRestartCount := 0;
  FActiveBroadcastVideoMediaType := nil;
  FLastBroadcastAudioSampleTick := 0;
  FLastRdjProVideoSampleTime100ns := 0;
  FLastRdjProVideoSampleTick := 0;
  FStaticVideoBitmap := nil;
  FStaticVideoBuffer := nil;
  FStaticVideoBufferWidth := 0;
  FStaticVideoBufferHeight := 0;
  FStaticVideoMediaType := nil;
  FStaticVideoFrameIndex := 0;
  FStaticVideoStartTick := 0;
  FStaticVideoLastTick := 0;
  FStaticVideoFrameDuration100ns := RDJ_STATIC_VIDEO_FRAME_DURATION_100NS;

  FCast := nil;
  FCastWorker := nil;
  FCastLiveByteStream := nil;
  FCastLiveStreamLock := TCriticalSection.Create();
  FCastLiveFragmentCount := 0;
  FCastLiveRebaser := TRdjProCastFmp4Rebaser.Create();
  FCastProducerRequested := False;
  FPendingCastLive := False;
  FPendingCastDevice := Default(TMfCastDevice);
  FCastClosing := False;
  FCastUpdatingVolume := False;
  FCastMediaSessionReady := False;
  SetLength(FCastDevices, 0);
  trkCastVolume.Position := 75;
  chkCastMuted.Checked := False;

  try
    // Reuse the same built-in growing fMP4 publisher used by the proven
    // MfSimpleCastPlayer transcode path. RDJ supplies the encoded fragments.
    FCast := TMfCast.Create(True);
    FCast.OnDeviceAdded := CastDeviceChanged;
    FCast.OnDeviceUpdated := CastDeviceChanged;
    FCast.OnDeviceRemoved := CastDeviceRemoved;
    FCast.OnStateChanged := CastStateChanged;
    FCast.OnMediaStatus := CastMediaStatus;
    FCast.OnError := CastError;
    FCast.OnLog := CastLog;
  except
    on E: Exception do
      begin
        FreeAndNil(FCast);
        memLog.Lines.Add('Cast initialization failed: ' + E.Message);
      end;
  end;

  UpdateCastControls();

  RegisterForDeviceNotification(Handle,
                                FPtrDevNotify);
end;


procedure TfrmMediaServer.FormDestroy(Sender: TObject);
var
  CleanupDumpDir: string;
  CleanupMirrorDir: string;
  PendingMessage: TMsg;

begin

  FCastClosing := True;
  ClearCastLiveByteStream();

  if Assigned(FCast) then
    begin
      FCast.OnDeviceAdded := nil;
      FCast.OnDeviceUpdated := nil;
      FCast.OnDeviceRemoved := nil;
      FCast.OnStateChanged := nil;
      FCast.OnMediaStatus := nil;
      FCast.OnError := nil;
      FCast.OnLog := nil;
    end;

  if Assigned(FCastWorker) then
    begin
      FCastWorker.WaitFor();
      FreeAndNil(FCastWorker);
    end;

  if Assigned(FCast) then
    begin
      FCast.Stop();
      FCast.Disconnect();
      FreeAndNil(FCast);
    end;

  FreeAndNil(FCastLiveRebaser);
  FreeAndNil(FCastLiveStreamLock);

  while PeekMessage(PendingMessage,
                    Handle,
                    WM_MFCAST_STATUS,
                    WM_MFCAST_STATUS,
                    PM_REMOVE) do
    TObject(PendingMessage.WParam).Free();

  while PeekMessage(PendingMessage,
                    Handle,
                    WM_MFCAST_ERROR,
                    WM_MFCAST_ERROR,
                    PM_REMOVE) do
    TObject(PendingMessage.WParam).Free();

  while PeekMessage(PendingMessage,
                    Handle,
                    WM_MFCAST_LOG,
                    WM_MFCAST_LOG,
                    PM_REMOVE) do
    TObject(PendingMessage.WParam).Free();

  CleanupDumpDir := FBroadcastMseDumpDir;
  CleanupMirrorDir := FBroadcastMseMirrorDir;
  FRdjProBroadcasting := False;

  StopBroadcastMseMirrorThread();

  FreeAndNil(FBroadcastMseGroupStream);
  SetLength(FBroadcastMseGroupBytes, 0);

  UnRegisterForDeviceNotification(FPtrDevNotify);
  FPtrDevNotify := nil;

  if Assigned(FRdjProBroadcastMp4Recorder) then
    begin
      FRdjProBroadcastMp4Recorder.StopRecording();
      FreeAndNil(FRdjProBroadcastMp4Recorder);
    end;

  ReleaseBroadcastHandoverLock(True);

  LaunchBroadcastMseCleanupBatch(CleanupDumpDir,
                                 CleanupMirrorDir);

  if Assigned(FRdjProMp4Recorder) then
    begin
      FRdjProMp4Recorder.StopRecording();
      FreeAndNil(FRdjProMp4Recorder);
    end;

  if Assigned(FRdjProCaptureManager) then
    begin
      FRdjProCaptureManager.ShutDownEngine();
      FreeAndNil(FRdjProCaptureManager);
    end;

  FStaticVideoBuffer := nil;
  FStaticVideoMediaType := nil;
  FreeAndNil(FStaticVideoBitmap);
end;


procedure TfrmMediaServer.FormShow(Sender: TObject);
begin

  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle,
                              GWL_STYLE) and not WS_CAPTION or WS_BORDER);

  Width := Width - 10;
  Height := Height - 30;

  lblIcecastServerStatus.Caption := 'Server: handled by RDJ Pro service.';

  chkBroadcast.Enabled := True;
  chkBroadcast.Checked := FBroadcastPublishing;
  chkBroadcast.Down := chkBroadcast.Checked;

  UpdateOnAirLamp(chkBroadcast.Checked);
  UpdateRecordingUi();
  UpdateCastControls();
  tmrTime.Enabled := True;

  if Assigned(MainMDIFrm) then
    MainMDIFrm.RefreshMainButtonStates();
end;


procedure TfrmMediaServer.UpdateOnAirLamp(const AOnAir: Boolean);
begin

   if AOnAir then
     begin

       shpOnAirCap.Pen.Color := clRed;
       shpOnAir.Pen.Color := clRed;
       lblOnAir.Font.Color := clRed;
       lblOnAir.Caption := 'ON AIR';
     end
   else
     begin

       shpOnAirCap.Pen.Color := $00568000;
       shpOnAir.Pen.Color := $00568000;
       lblOnAir.Font.Color := $00568000;
       lblOnAir.Caption := 'OFF AIR';
     end;
end;


procedure TfrmMediaServer.UpdateRecorderLamp(const ARecording: Boolean);
begin

   if ARecording then
     begin

       shpRecordingCap.Pen.Color := clRed;
       shpRecording.Pen.Color := clRed;
       lblRecording.Font.Color := clRed;
       lblRecording.Caption := 'REC ON';
     end
   else
     begin

       shpRecordingCap.Pen.Color := $00568000;
       shpRecording.Pen.Color := $00568000;
       lblRecording.Font.Color := $00568000;
       lblRecording.Caption := 'REC OFF';
     end;
end;

procedure TfrmMediaServer.SetBroadcastHealth(const ACaption: string;
                                            const AColor: TColor;
                                            const AMessage: string;
                                            const AForceLog: Boolean);
var
  NowTick: UInt64;
  ShouldLog: Boolean;

begin

  if Assigned(lblBroadcastHealth) then
    begin
      lblBroadcastHealth.Caption := ACaption;
      lblBroadcastHealth.Font.Color := AColor;
    end;

  NowTick := GetTickCount64();
  ShouldLog := AForceLog or
               (FLastBroadcastHealthCaption <> ACaption) or
               (FLastBroadcastHealthMessage <> AMessage);

  if (not ShouldLog) and
     (ACaption <> 'HEALTH: IDLE') and
     (AMessage <> '') and
     ((FLastBroadcastHealthLogTick = 0) or
      ((NowTick - FLastBroadcastHealthLogTick) >= RDJ_BROADCAST_HEALTH_LOG_REPEAT_MS)) then
    ShouldLog := True;

  if ShouldLog and Assigned(memLog) and (AMessage <> '') then
    begin
      memLog.Lines.Append(FormatDateTime('hh:nn:ss  ',
                                         Now) + AMessage);
      while (memLog.Lines.Count > 250) do
        memLog.Lines.Delete(0);

      FLastBroadcastHealthLogTick := NowTick;
    end;

  FLastBroadcastHealthCaption := ACaption;
  FLastBroadcastHealthMessage := AMessage;
end;


procedure TfrmMediaServer.UpdateBroadcastHealth();
var
  NowTick: UInt64;
  AudioAgeMs: UInt64;
  VideoAgeMs: UInt64;
  PublicAgeMs: UInt64;
  NeedAudio: Boolean;
  RecorderActive: Boolean;
begin

  if not FBroadcastPublishing then
    begin
      SetBroadcastHealth('HEALTH: IDLE',
                         HEALTH_IDLE_COLOR,
                         'Broadcast health idle.');
      Exit;
    end;

  NowTick := GetTickCount64();

  if FPendingBroadcastRecording then
    begin
      SetBroadcastHealth('HEALTH: STARTING',
                         HEALTH_WARNING_COLOR,
                         'Broadcast waiting for the first valid audio format.');
      Exit;
    end;

  if not FBroadcastHandoverLockAcquired then
    begin
      SetBroadcastHealth('HEALTH: ERROR',
                         HEALTH_ERROR_COLOR,
                         'Broadcast health error: broadcast handover lock is not acquired.');
      Exit;
    end;

  RecorderActive := Assigned(FRdjProBroadcastMp4Recorder) and
                    FRdjProBroadcastMp4Recorder.Active;

  if not RecorderActive then
    begin
      SetBroadcastHealth('HEALTH: ERROR',
                         HEALTH_ERROR_COLOR,
                         'Broadcast health error: broadcast recorder is not active.');
      Exit;
    end;

  NeedAudio := not FRecordVideoOnly;
  if NeedAudio then
    begin
      if (FLastBroadcastAudioSampleTick = 0) then
        begin
          SetBroadcastHealth('HEALTH: STARTING',
                             HEALTH_WARNING_COLOR,
                             'Broadcast waiting for audio samples.');
          Exit;
        end;

      AudioAgeMs := NowTick - FLastBroadcastAudioSampleTick;

      if (AudioAgeMs >= RDJ_BROADCAST_HEALTH_AUDIO_STALE_MS) then
        begin
          SetBroadcastHealth('HEALTH: ERROR',
                             HEALTH_ERROR_COLOR,
                             'Broadcast health error: audio samples stopped.');
          Exit;
        end;
    end;

  if (FLastBroadcastVideoSampleTick = 0) then
    begin
      SetBroadcastHealth('HEALTH: STARTING',
                         HEALTH_WARNING_COLOR,
                         'Broadcast waiting for video samples.');
      Exit;
    end;

  VideoAgeMs := NowTick - FLastBroadcastVideoSampleTick;
  if (VideoAgeMs >= RDJ_BROADCAST_HEALTH_VIDEO_STALE_MS) then
    begin
      SetBroadcastHealth('HEALTH: ERROR',
                         HEALTH_ERROR_COLOR,
                         'Broadcast health error: video samples stopped.');
      Exit;
    end;

  if FLastBroadcastPublicSegmentTick = 0 then
    FLastBroadcastPublicSegmentTick := NowTick;

  PublicAgeMs := NowTick - FLastBroadcastPublicSegmentTick;
  if (FBroadcastMsePublicSeq <= 0) and
     (PublicAgeMs < RDJ_BROADCAST_HEALTH_STARTUP_GRACE_MS) then
    begin
      SetBroadcastHealth('HEALTH: STARTING',
                         HEALTH_WARNING_COLOR,
                         'Broadcast waiting for the first browser stream fragment.');
      Exit;
    end;

  if (PublicAgeMs >= RDJ_BROADCAST_HEALTH_PUBLIC_STALE_MS) then
    begin
      SetBroadcastHealth('HEALTH: WARNING',
                         HEALTH_WARNING_COLOR,
                         'Broadcast health warning: browser stream fragments are not advancing.');
      Exit;
    end;

  SetBroadcastHealth('HEALTH: OK',
                     HEALTH_OK_COLOR,
                     'Broadcast healthy: audio, video, recorder and browser stream active.');
end;


procedure TfrmMediaServer.tmrTimeTimer(Sender: TObject);
begin

  if FTimerRunning then
    UpdateTimeLabel();

  if FRdjProBroadcasting and FRdjProBroadcastMp4Recorder.Active then
    begin
      QueueRdjProStaticVideoSample();
      DumpBroadcastMseSegments();
      TryStartPendingCastLive();
      if FBroadcastPublishing then
        CheckBroadcastMseVideoSourceFlush();
      LogBroadcastMseMemoryHeartbeat('timer');
      if FBroadcastPublishing then
        UpdateBroadcastHandoverHeartbeat();
    end;

  if not FBroadcastPublishing then
    PollBroadcastHandoverStatus();

  if FRdjProRecording and not FRdjProBroadcasting then
    QueueRdjProStaticVideoSample();

  UpdateBroadcastHealth();
end;

// All Icecast end =============================================================

// RDJPro

procedure TfrmMediaServer.UpdateTimeLabel();
var
  TS: TTimeSpan;
  Hours: Integer;
  Minutes: Integer;
  Seconds: Integer;
  Hundredths: Integer;

begin

  TS := prStopwatch.Elapsed;
  Hours := Trunc(TS.TotalHours);
  Minutes := TS.Minutes;
  Seconds := TS.Seconds;
  Hundredths := TS.Milliseconds div 10;

  lblRecTime.Caption := Format('Recorded: %.2d:%.2d:%.2d.%.2d',
                               [Hours, Minutes, Seconds, Hundredths]);
end;


procedure TfrmMediaServer.UpdateRecordingUi();
begin

  if FRecordingRdjPro then
    begin

      btnRdjProRecord.Checked := False;
      btnRdjProRecord.Caption := 'Stop';
      chkRdjProCamera.Enabled := False;
      chkRecordVideoOnly.Enabled := False;
      chkRdjProStaticImage.Enabled := False;
      edRdjProRecFileName.Enabled := False;
      lblRecorderStatus.Caption := 'Recorder is running.';
      UpdateRecorderLamp(True);
    end
  else
    begin

      btnRdjProRecord.Checked := True;
      btnRdjProRecord.Caption := 'Start';
      chkRdjProCamera.Enabled := (not FRdjProBroadcasting) or
                                  FRdjProStaticImage;
      chkRecordVideoOnly.Enabled := not FRdjProBroadcasting;
      chkRdjProStaticImage.Enabled := True;
      edRdjProRecFileName.Enabled := True;
      lblRecorderStatus.Caption := 'Recorder is ready.';
      UpdateRecorderLamp(False);
    end;
end;


// Camera / recorder ===========================================================

function RdjMakeUINT64(const HighPart: DWORD;
                       const LowPart: DWORD): UINT64;
begin

  Result := (UINT64(HighPart) shl 32) or UINT64(LowPart);
end;


procedure TfrmMediaServer.SetRdjProVideoSourceButtons(const AStaticImage,
                                                      ACamera: Boolean);
begin

  FUpdatingRdjProVideoSourceButtons := True;

  try

    chkRdjProStaticImage.Checked := AStaticImage;
    chkRdjProCamera.Checked := ACamera;
  finally

    FUpdatingRdjProVideoSourceButtons := False;
  end;
end;


procedure TfrmMediaServer.SetRdjProStaticImageMode(const AStaticImage: Boolean);
begin

  FRdjProStaticImage := AStaticImage;
  SetRdjProVideoSourceButtons(AStaticImage,
                              not AStaticImage);
end;


function TfrmMediaServer.PickRdjProStaticImageFileName(): string;
begin

  Result := '';

  if not Assigned(DlgLWFileBrowserEx) then
    DlgLWFileBrowserEx := TLWFileBrowserExDlg.Create(Self);

  DlgLWFileBrowserEx.FileFilter := fbxGraphics;
  DlgLWFileBrowserEx.ShowModal();

  if (DlgLWFileBrowserEx.ModalResult = mrOk) then
    Result := DlgLWFileBrowserEx.FileURI;
end;


function TfrmMediaServer.ResolveRdjProStaticImageFileName(): string;
var
  BaseDir: string;
  Candidate: string;

begin

  Result := '';

  Candidate := Trim(FStaticImageFileName);
  if (Candidate <> '') and FileExists(Candidate) then
    Exit(Candidate);

  if Assigned(MainMDIFrm) then
    begin
      Candidate := Trim(MainMDIFrm.CoverJpg);
      if (Candidate <> '') and FileExists(Candidate) then
        Exit(Candidate);

      BaseDir := IncludeTrailingPathDelimiter(Trim(MainMDIFrm.Setup.CaddyArtworkPath));

      Candidate := BaseDir + 'cover.jpg';
      if FileExists(Candidate) then
        Exit(Candidate);

      Candidate := BaseDir + 'cover_default.jpg';
      if FileExists(Candidate) then
        Exit(Candidate);
    end;
end;


function TfrmMediaServer.EnsureRdjProStaticVideoFrame(const AWidth: Integer;
                                                      const AHeight: Integer): Boolean;
var
  Picture: TPicture;
  ImageFileName: string;
  SrcW: Integer;
  SrcH: Integer;
  DstW: Integer;
  DstH: Integer;
  DstX: Integer;
  DstY: Integer;
  Scale: Double;
  DstRect: TRect;
  TargetWidth: Integer;
  TargetHeight: Integer;

begin

  TargetWidth := AWidth;
  TargetHeight := AHeight;

  if (TargetWidth <= 0) then
    TargetWidth := RDJ_STATIC_VIDEO_WIDTH;

  if (TargetHeight <= 0) then
    TargetHeight := RDJ_STATIC_VIDEO_HEIGHT;

  if Assigned(FStaticVideoBitmap) and
     (FStaticVideoBitmap.Width = TargetWidth) and
     (FStaticVideoBitmap.Height = TargetHeight) then
    Exit(True);

  if not Assigned(FStaticVideoBitmap) then
    FStaticVideoBitmap := TBitmap.Create();

  FStaticVideoBitmap.PixelFormat := pf32bit;
  FStaticVideoBitmap.SetSize(TargetWidth,
                             TargetHeight);

  FStaticVideoBitmap.Canvas.Brush.Color := clBlack;

  FStaticVideoBitmap.Canvas.FillRect(Rect(0,
                                          0,
                                          FStaticVideoBitmap.Width,
                                          FStaticVideoBitmap.Height));

  ImageFileName := ResolveRdjProStaticImageFileName();
  if ImageFileName = '' then
    begin
      FStaticVideoBitmap.Canvas.Font.Color := clWhite;
      FStaticVideoBitmap.Canvas.Font.Size := 28;
      FStaticVideoBitmap.Canvas.TextOut(40,
                                        40,
                                        'RDJ Pro');
      Exit(True);
    end;

  Picture := TPicture.Create();
  try
    try
      Picture.LoadFromFile(ImageFileName);
    except
      on E: Exception do
        begin
          OutputDebugString(PChar('RDJ Pro static image load failed: ' + E.Message));
          Exit(True);
        end;
    end;

    if (not Assigned(Picture.Graphic)) or
       Picture.Graphic.Empty then
      Exit(True);

    SrcW := Picture.Width;
    SrcH := Picture.Height;
    if (SrcW <= 0) or (SrcH <= 0) then
      Exit(True);

    Scale := Min(TargetWidth / SrcW,
                 TargetHeight / SrcH);
    DstW := Round(SrcW * Scale);
    DstH := Round(SrcH * Scale);
    DstX := (TargetWidth - DstW) div 2;
    DstY := (TargetHeight - DstH) div 2;
    DstRect := Rect(DstX,
                    DstY,
                    DstX + DstW,
                    DstY + DstH);

    FStaticVideoBitmap.Canvas.StretchDraw(DstRect,
                                          Picture.Graphic);
  finally
    Picture.Free();
  end;

  Result := True;
end;


procedure TfrmMediaServer.ShowRdjProStaticPreview();
begin

  if not Assigned(imgRdjProStaticPreview) then
    Exit;

  if EnsureRdjProStaticVideoFrame() and
     Assigned(FStaticVideoBitmap) then
    imgRdjProStaticPreview.Picture.Assign(FStaticVideoBitmap)
  else
    imgRdjProStaticPreview.Picture.Graphic := nil;

  imgRdjProStaticPreview.Visible := True;
  imgRdjProStaticPreview.BringToFront();
end;


procedure TfrmMediaServer.HideRdjProStaticPreview();
begin

  if not Assigned(imgRdjProStaticPreview) then
    Exit;

  imgRdjProStaticPreview.Visible := False;
  imgRdjProStaticPreview.Picture.Graphic := nil;
end;


procedure TfrmMediaServer.RefreshRdjProCameraPreview();
begin

  if Assigned(imgRdjProStaticPreview) then
    begin

      imgRdjProStaticPreview.Visible := False;
      imgRdjProStaticPreview.Picture.Graphic := nil;
    end;

  if Assigned(pnlRdjProPreviewHost) then
    begin

      pnlRdjProPreviewHost.Invalidate();
      pnlRdjProPreviewHost.Update();
    end;

  if Assigned(FRdjProCaptureManager) and
     Assigned(pnlRdjProPreviewHost) then
    begin

      FRdjProCaptureManager.PreviewHandle := pnlRdjProPreviewHost.Handle;
      FRdjProCaptureManager.ResizeVideo(nil);
      FRdjProCaptureManager.UpdateVideo();
    end;
end;


procedure TfrmMediaServer.ScheduleRdjProCameraPreviewRefresh();
const
  RDJ_CAMERA_PREVIEW_REFRESH_DELAYS: array[0..3] of Integer = (100, 250, 500, 1000);

var
  RefreshThread: TThread;

begin

  if FPreviewFramePending then
    Exit;

  FPreviewFramePending := True;

  RefreshThread := TThread.CreateAnonymousThread(procedure
                                                 var
                                                   I: Integer;
                                                   RefreshProc: TThreadProcedure;
                                                   ClearProc: TThreadProcedure;

                                                 begin
                                                   for I := Low(RDJ_CAMERA_PREVIEW_REFRESH_DELAYS) to High(RDJ_CAMERA_PREVIEW_REFRESH_DELAYS) do
                                                     begin
                                                       Sleep(RDJ_CAMERA_PREVIEW_REFRESH_DELAYS[I]);

                                                       RefreshProc := procedure
                                                                      begin
                                                                        if FRdjProPreviewing and (not FRdjProStaticImage) then
                                                                          RefreshRdjProCameraPreview();
                                                                      end;
                                                       TThread.Queue(nil,
                                                                     RefreshProc);
                                                     end;

                                                   ClearProc := procedure
                                                                begin
                                                                  FPreviewFramePending := False;
                                                                end;

                                                   TThread.Queue(nil,
                                                                 ClearProc);
                                                 end);
  RefreshThread.Start();
end;


function TfrmMediaServer.EnsureRdjProStaticVideoMediaType(): Boolean;
var
  hr: HRESULT;

begin

  Result := Assigned(FStaticVideoMediaType);
  if Result then
    Exit;

  hr := MFCreateMediaType(FStaticVideoMediaType);
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                      MFMediaType_Video);
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetGUID(MF_MT_SUBTYPE,
                                      MFVideoFormat_RGB32);
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetUINT32(MF_MT_INTERLACE_MODE,
                                        MFVideoInterlace_Progressive);
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetUINT64(MF_MT_FRAME_SIZE,
                                        RdjMakeUINT64(RDJ_STATIC_VIDEO_WIDTH,
                                                     RDJ_STATIC_VIDEO_HEIGHT));
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetUINT32(MF_MT_DEFAULT_STRIDE,
                                        RDJ_STATIC_VIDEO_WIDTH * 4);
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetUINT64(MF_MT_FRAME_RATE,
                                        RdjMakeUINT64(RDJ_STATIC_VIDEO_FPS,
                                                     1));
  if FAILED(hr) then
    Exit;

  hr := FStaticVideoMediaType.SetUINT64(MF_MT_PIXEL_ASPECT_RATIO,
                                        RdjMakeUINT64(1,
                                                     1));
  if FAILED(hr) then
    Exit;

  Result := True;
end;


function TfrmMediaServer.CreateRdjProStaticVideoSample(out ASample: IMFSample;
                                                       const ASampleTime100ns: LONGLONG;
                                                       const AWidth: Integer;
                                                       const AHeight: Integer): HRESULT;
var
  NewBuffer: IMFMediaBuffer;
  Data: PByte;
  MaxLen: DWORD;
  CurLen: DWORD;
  BufferSize: DWORD;
  Stride: Integer;
  Row: Integer;
  TargetWidth: Integer;
  TargetHeight: Integer;
  Src: PByte;
  Dst: PByte;

begin

  ASample := nil;
  NewBuffer := nil;
  Data := nil;
  MaxLen := 0;
  CurLen := 0;

  TargetWidth := AWidth;
  TargetHeight := AHeight;

  if (TargetWidth <= 0) then
    TargetWidth := RDJ_STATIC_VIDEO_WIDTH;

  if (TargetHeight <= 0) then
    TargetHeight := RDJ_STATIC_VIDEO_HEIGHT;

  if not EnsureRdjProStaticVideoFrame(TargetWidth,
                                      TargetHeight) then
    Exit(MF_E_INVALIDMEDIATYPE);

  Stride := TargetWidth * 4;
  BufferSize := DWORD(Stride * TargetHeight);

  if (not Assigned(FStaticVideoBuffer)) or
     (FStaticVideoBufferWidth <> TargetWidth) or
     (FStaticVideoBufferHeight <> TargetHeight) then
    begin
      Result := MFCreateMemoryBuffer(BufferSize,
                                     NewBuffer);
      if FAILED(Result) then
        Exit;

      Result := NewBuffer.Lock(Data,
                               @MaxLen,
                               @CurLen);
      if FAILED(Result) then
        Exit;

      try
        for Row := 0 to TargetHeight - 1 do
          begin
            // Keep the bitmap's display row order and make that explicit with
            // MF_MT_DEFAULT_STRIDE on the media type.
            Src := FStaticVideoBitmap.ScanLine[Row];
            Dst := Data;
            Inc(Dst,
                Row * Stride);
            Move(Src^,
                 Dst^,
                 Stride);
          end;
      finally
        NewBuffer.Unlock();
      end;

      Result := NewBuffer.SetCurrentLength(BufferSize);
      if FAILED(Result) then
        Exit;

      FStaticVideoBuffer := NewBuffer;
      FStaticVideoBufferWidth := TargetWidth;
      FStaticVideoBufferHeight := TargetHeight;
    end;

  Result := MFCreateSample(ASample);
  if FAILED(Result) then
    Exit;

  Result := ASample.AddBuffer(FStaticVideoBuffer);
  if FAILED(Result) then
    begin
      ASample := nil;
      Exit;
    end;

  ASample.SetSampleTime(ASampleTime100ns);
  ASample.SetSampleDuration(FStaticVideoFrameDuration100ns);
end;


procedure TfrmMediaServer.QueueRdjProStaticVideoSample();
var
  NowTick: UInt64;
  TargetFrameIndex: Int64;
  SampleTime100ns: LONGLONG;
  FrameSize: UINT64;
  StaticWidth: Integer;
  StaticHeight: Integer;
  LocalSample: IMFSample;
  BroadcastSample: IMFSample;
  NeedLocal: Boolean;
  NeedBroadcast: Boolean;
  Queued: Boolean;
  hr: HRESULT;

begin

  if not FRdjProStaticImage then
    Exit;

  if not (FRdjProRecording or FRdjProBroadcasting) then
    Exit;

  NowTick := GetTickCount64();

  if FStaticVideoStartTick = 0 then
    begin
      FStaticVideoStartTick := NowTick;
      FStaticVideoFrameIndex := 0;
      FStaticVideoLastTick := 0;
    end;

  StaticWidth := RDJ_STATIC_VIDEO_WIDTH;
  StaticHeight := RDJ_STATIC_VIDEO_HEIGHT;

  if Assigned(FActiveBroadcastVideoMediaType) and
     SUCCEEDED(FActiveBroadcastVideoMediaType.GetUINT64(MF_MT_FRAME_SIZE,
                                                        FrameSize)) then
    begin
      StaticWidth := Integer(FrameSize shr 32);
      StaticHeight := Integer(FrameSize and $FFFFFFFF);
    end;

  TargetFrameIndex := Int64(((NowTick - FStaticVideoStartTick) *
                             UInt64(RDJ_STATIC_VIDEO_FPS)) div
                             1000);

  if (FStaticVideoLastTick <> 0) and
     (TargetFrameIndex < FStaticVideoFrameIndex) then
    Exit;

  if FLastRdjProVideoSampleTick <> 0 then
    SampleTime100ns := FLastRdjProVideoSampleTime100ns +
                       LONGLONG(NowTick - FLastRdjProVideoSampleTick) * 10000
  else
    SampleTime100ns := TargetFrameIndex * FStaticVideoFrameDuration100ns;

  LocalSample := nil;
  BroadcastSample := nil;
  Queued := False;

  NeedLocal := FRdjProRecording and
               Assigned(FRdjProMp4Recorder) and
               FRdjProMp4Recorder.Active;

  NeedBroadcast := FRdjProBroadcasting and
                   Assigned(FRdjProBroadcastMp4Recorder) and
                   FRdjProBroadcastMp4Recorder.Active;

  if not (NeedLocal or NeedBroadcast) then
    Exit;

  try
    if NeedLocal then
      begin
        hr := CreateRdjProStaticVideoSample(LocalSample,
                                            SampleTime100ns,
                                            StaticWidth,
                                            StaticHeight);
        if FAILED(hr) or (not Assigned(LocalSample)) then
          begin
            OutputDebugString(PChar(Format('RDJ Pro local static video sample failed hr=0x%.8x',
                                           [Cardinal(hr)])));
          end
        else
          begin
            hr := FRdjProMp4Recorder.QueueVideoSample(LocalSample);
            if FAILED(hr) then
              OutputDebugString(PChar('RDJ Pro local MP4 static QueueVideoSample failed: ' + IntToStr(hr)))
            else
              Queued := True;
          end;
      end;

    if NeedBroadcast then
      begin
        hr := CreateRdjProStaticVideoSample(BroadcastSample,
                                            SampleTime100ns,
                                            StaticWidth,
                                            StaticHeight);
        if FAILED(hr) or (not Assigned(BroadcastSample)) then
          begin
            OutputDebugString(PChar(Format('RDJ Pro broadcast static video sample failed hr=0x%.8x',
                                           [Cardinal(hr)])));
          end
        else
          begin
            FLastBroadcastVideoSampleTick := GetTickCount64();
            hr := FRdjProBroadcastMp4Recorder.QueueVideoSample(BroadcastSample);
            if FAILED(hr) then
              OutputDebugString(PChar('RDJ Pro broadcast MP4 static QueueVideoSample failed: ' + IntToStr(hr)))
            else
              Queued := True;
          end;
      end;

    if Queued then
      begin
        FLastRdjProVideoSampleTime100ns := SampleTime100ns;
        FLastRdjProVideoSampleTick := NowTick;
        FStaticVideoFrameIndex := TargetFrameIndex + 1;
        FStaticVideoLastTick := NowTick;
      end;
  finally
    LocalSample := nil;
    BroadcastSample := nil;
  end;
end;


function TfrmMediaServer.StartRdjProCamera(PreviewObject: HWnd): HRESULT;
var
  CameraActivate: IMFActivate;
  CameraSymbolicLink: string;
  Setup: TRDJSetup;

begin

  if not Assigned(FRdjProCaptureManager) then
    Exit(E_POINTER);

  if (PreviewObject = 0) then
    Exit(E_INVALIDARG);

  if not FRdjProStaticImage then
    HideRdjProStaticPreview();

  if not FRdjProCaptureInitialized then
    begin
      Setup := MainMDIFrm.Setup;
      CameraActivate := nil;
      CameraSymbolicLink := Trim(Setup.CameraSymbolicLink);

      Result := CaptureDeviceGetActivate(PWideChar(CameraSymbolicLink),
                                         CameraActivate);
      if FAILED(Result) then
        begin

          InfoMsg(optShowMsg,
                  'StartRdjProCamera: selected camera not found.',
                  Result);

          memLog.Lines.Append('Selected camera not found.');
          Exit;
        end;

      Result := FRdjProCaptureManager.InitializeCaptureManager(HWND(PreviewObject),
                                                               Handle,
                                                               CameraActivate as IUnknown,
                                                               False);
      if FAILED(Result) then
        begin
          InfoMsg(optShowMsg,
                  'StartRdjProCamera: InitializeCaptureManager failed.',
                  Result);

          memLog.Lines.Append('Initializing of Capture Manager failed.');
          Exit;
        end;

      FRdjProCaptureManager.OnVideoReaderSample := OnRdjProVideoReaderSample;
      FRdjProCaptureInitialized := True;
      // TODO: FRdjProPreviewHandle := HWND(PreviewObject);
    end;

  if FRdjProPreviewing then
    begin
      if not FRdjProStaticImage then
        begin
          RefreshRdjProCameraPreview();
          ScheduleRdjProCameraPreviewRefresh();
        end;
      Exit(S_OK);
    end;

  Result := FRdjProCaptureManager.StartCamera(nil);
  if (Result = E_VIDEOPROCESSOR_NOT_IMPLEMENTED) then
    Result := S_OK
  else
    if FAILED(Result) then
      begin
        InfoMsg(optShowMsg,
                'StartRdjProCamera: StartCamera failed.',
                Result);

        memLog.Lines.Append('Start camera failed.');
        Exit;
      end;

  FRdjProPreviewing := True;

  if not FRdjProStaticImage then
    begin
      RefreshRdjProCameraPreview();
      ScheduleRdjProCameraPreviewRefresh();
    end;
  // TODO: FRdjProPreviewMode := smReUse;
end;


procedure TfrmMediaServer.StopRdjProCamera();
begin

  if Assigned(FRdjProCaptureManager) then
    FRdjProCaptureManager.StopCamera();

  FRdjProPreviewing := False;
  FRdjProCaptureInitialized := False;
end;


procedure TfrmMediaServer.ResetRdjProCaptureManager();
begin

  if Assigned(FRdjProCaptureManager) then
    begin
      FRdjProCaptureManager.ShutDownEngine();
      FreeAndNil(FRdjProCaptureManager);
    end;

  FRdjProCaptureManager := TRdjProCaptureManager.Create(Handle);
  FRdjProPreviewing := False;
  FRdjProCaptureInitialized := False;
end;


procedure TfrmMediaServer.OnRdjProVideoReaderSample(Sender: TObject;
                                                          ASample: IMFSample;
                                                          const SampleTime: LONGLONG);
var
  hr: HRESULT;

begin

  if not Assigned(ASample) then
    Exit;

  FLastRdjProVideoSampleTime100ns := SampleTime;
  FLastRdjProVideoSampleTick := GetTickCount64();

  if FRdjProStaticImage then
    Exit;

  if FRdjProBroadcasting then
    FLastBroadcastVideoSampleTick := GetTickCount64();

  // Sample-2 EVR preview is independent. This callback is now fed by the
  // separate SourceReader used for MP4 recording/broadcast samples.
  if FRdjProRecording and
     Assigned(FRdjProMp4Recorder) and
     FRdjProMp4Recorder.Active then
    begin

      hr := FRdjProMp4Recorder.QueueVideoSample(ASample);
      if FAILED(hr) then
        OutputDebugString(PChar('RDJ Pro local MP4 QueueVideoSample failed: ' + IntToStr(hr)));
    end;

  if FRdjProBroadcasting and
     Assigned(FRdjProBroadcastMp4Recorder) and
     FRdjProBroadcastMp4Recorder.Active then
    begin

      hr := FRdjProBroadcastMp4Recorder.QueueVideoSample(ASample);
      if FAILED(hr) then
        OutputDebugString(PChar('RDJ Pro broadcast MP4 QueueVideoSample failed: ' + IntToStr(hr)));
    end;
end;


function MediaTypeGetFrameSize(const pType: IMFMediaType;
                               out AWidth: Integer;
                               out AHeight: Integer): HRESULT;
var
  FrameSize: UINT64;

begin
  AWidth := 0;
  AHeight := 0;

  if not Assigned(pType) then
    Exit(E_POINTER);

  Result := pType.GetUINT64(MF_MT_FRAME_SIZE,
                            FrameSize);
  if FAILED(Result) then
    Exit;

  AWidth := Integer(FrameSize shr 32);
  AHeight := Integer(FrameSize and $FFFFFFFF);
end;


procedure TfrmMediaServer.pnlCaptionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  // Drag window
  ReleaseCapture;
  SendMessage(Handle,
              WM_SYSCOMMAND,
              SC_MOVE + HTCAPTION,
              0);
end;


procedure TfrmMediaServer.RecordTapPreFx(const pData: PSingle;
                                         const Frames: Integer;
                                         const pwfx: PWAVEFORMATEX);
var
  hr: HRESULT;
  PendingStartProc: TThreadProcedure;
  StopLocalProc: TThreadProcedure;
  StopBroadcastProc: TThreadProcedure;

begin

  if (pwfx <> nil) and
     (pwfx.nSamplesPerSec <> 0) and
     (pwfx.nChannels <> 0) and
     (pwfx.nBlockAlign <> 0) then
    begin

      FLastRdjProAudioWfx := pwfx^;
      FLastRdjProAudioWfxValid := True;

      if (FPendingLocalRecording or FPendingBroadcastRecording) and
         (TInterlocked.CompareExchange(FPendingMp4StartQueued, 1, 0) = 0) then
        begin
          PendingStartProc := procedure
                              begin
                                try

                                  TryCompletePendingMp4Starts();
                                finally

                                  TInterlocked.Exchange(FPendingMp4StartQueued, 0);
                                end;
                              end;
          TThread.Queue(nil,
                        PendingStartProc);
        end;
    end;

  if (not FRdjProRecording) and
     (not FRdjProBroadcasting) then
    Exit;

  if (pData = nil) or
     (pwfx = nil) or
     (Frames <= 0) then
    Exit;

  if FRdjProRecording and
     Assigned(FRdjProMp4Recorder) and
     FRdjProMp4Recorder.Active then
    begin

      try
        hr := FRdjProMp4Recorder.PushPcmFloat32(pData,
                                                Frames,
                                                pwfx);
        if FAILED(hr) then
          OutputDebugString(PChar('RDJ Pro local MP4 PushPcmFloat32 failed: ' + IntToStr(hr)));
      except

        on E: Exception do
          begin

            OutputDebugString(PChar('RDJ Pro local MP4 PushPcmFloat32 exception: ' + E.ClassName + ': ' + E.Message));
            FRdjProRecording := False;
            StopLocalProc := procedure
                             begin
                               StopRdjProRecording();
                             end;
            TThread.Queue(nil,
                          StopLocalProc);
          end;
      end;
    end;

  if FRdjProBroadcasting and
     Assigned(FRdjProBroadcastMp4Recorder) and
     FRdjProBroadcastMp4Recorder.Active then
    begin

      try

        FLastBroadcastAudioSampleTick := GetTickCount64();
        hr := FRdjProBroadcastMp4Recorder.PushPcmFloat32(pData,
                                                         Frames,
                                                         pwfx);
        if FAILED(hr) then
          OutputDebugString(PChar('RDJ Pro broadcast MP4 PushPcmFloat32 failed: ' + IntToStr(hr)));
      except

        on E: Exception do
          begin
            OutputDebugString(PChar('RDJ Pro broadcast MP4 PushPcmFloat32 exception: ' + E.ClassName + ': ' + E.Message));
            FRdjProBroadcasting := False;

            StopBroadcastProc := procedure
                                 begin

                                   StopRdjProBroadcast();
                                 end;
            TThread.Queue(nil,
                          StopBroadcastProc);
          end;
      end;
    end;
end;


procedure TfrmMediaServer.ArmPendingLocalRecording(const AFileName: string;
                                                   const AVideoOnly: Boolean);
begin

  FPendingLocalFileName := AFileName;
  FPendingLocalVideoOnly := AVideoOnly;
  FPendingLocalRecording := True;
  FPendingMp4StartQueued := 0;
end;


procedure TfrmMediaServer.ArmPendingBroadcastRecording(const AFileName: string;
                                                       const AVideoOnly: Boolean);
begin

  FPendingBroadcastFileName := AFileName;
  FPendingBroadcastVideoOnly := AVideoOnly;
  FPendingBroadcastRecording := True;
  FPendingMp4StartQueued := 0;
end;


procedure TfrmMediaServer.TryCompletePendingMp4Starts();
var
  FileName: string;
  VideoOnly: Boolean;

begin

  if FCompletingPendingMp4Start then
    Exit;

  if not FLastRdjProAudioWfxValid then
    Exit;

  FCompletingPendingMp4Start := True;

  try

    if FPendingLocalRecording then
      begin

        FileName := FPendingLocalFileName;
        VideoOnly := FPendingLocalVideoOnly;
        FPendingLocalRecording := False;
        FPendingLocalFileName := '';

        if (FileName <> '') and
           Assigned(FRdjProMp4Recorder) and
           (not FRdjProMp4Recorder.Active) then
          begin
            if not StartRdjProRecording(FileName,
                                        VideoOnly) then
              begin
                FRdjProRecording := False;
                FRecordingRdjPro := False;
                btnRdjProRecord.Tag := 0;
                UpdateRecordingUi();
              end;
          end;
      end;

    if FPendingBroadcastRecording then
      begin

        FileName := FPendingBroadcastFileName;
        FPendingBroadcastRecording := False;
        FPendingBroadcastFileName := '';

        if (FileName <> '') and
           Assigned(FRdjProBroadcastMp4Recorder) and
           (not FRdjProBroadcastMp4Recorder.Active) then
          begin
            if not StartRdjProBroadcast(FBroadcastPublishing) then
              begin

                FRdjProBroadcasting := False;
                FBroadcastPublishing := False;
                FCastProducerRequested := False;
                FPendingCastLive := False;
                chkBroadcast.Checked := False;
                chkBroadcast.Down := False;
                UpdateOnAirLamp(False);
                ReleaseBroadcastHandoverLock(False);
              end;
          end;
      end;
  finally

    FCompletingPendingMp4Start := False;
  end;
end;


function TfrmMediaServer.StartRdjProRecording(const AFileName: string;
                                              AVideoOnly: Boolean = False): Boolean;
var
  hr: HRESULT;
  Dir: string;

begin

  Result := False;

  if not Assigned(FRdjProMp4Recorder) then
    Exit;

  if (Trim(AFileName) = '') then
    Exit;

  Dir := ExtractFilePath(AFileName);
  if (Dir <> '') then
    ForceDirectories(Dir);

  if FRdjProStaticImage then
    begin
      if not EnsureRdjProStaticVideoMediaType() then
        Exit;

      hr := FRdjProMp4Recorder.SetVideoPreviewMediaType(FStaticVideoMediaType);
    end
  else
    begin
      if not EnsureRdjProVideoSampleReader() then
        Exit;

      hr := FRdjProMp4Recorder.SetVideoPreviewMediaType(FRdjProCaptureManager.VideoSourceReaderMediaType);
    end;

  if FAILED(hr) then
    begin
      InfoMsg(optShowMsg,
              'StartRdjProRecording: SetVideoPreviewMediaType failed.',
              hr);

      memLog.Lines.Append('Set video preview mediaType failed');
      Exit;
    end;

  if (not AVideoOnly) then
    begin
      if not FLastRdjProAudioWfxValid then
        begin
          ArmPendingLocalRecording(AFileName,
                                   AVideoOnly);
          FRdjProRecording := True;
          Result := True;
          Exit;
        end;

      hr := FRdjProMp4Recorder.SetAudioWaveFormat(@FLastRdjProAudioWfx);
      if FAILED(hr) then
        begin
          InfoMsg(optShowMsg,
                  'StartRdjProRecording: SetAudioWaveFormat failed.',
                  hr);

          memLog.Lines.Append('Set audio wave format failed');
          Exit;
        end;
    end;

  hr := FRdjProMp4Recorder.StartRecording(AFileName,
                                          AVideoOnly);
  if FAILED(hr) then
    begin
      InfoMsg(optShowMsg,
              'StartRdjProRecording: StartRecording failed.',
              hr);

      memLog.Lines.Append('Start recording failed');
      Exit;
    end;

  FRdjProRecording := True;
  if FRdjProStaticImage and not FRdjProBroadcasting then
    begin
      FStaticVideoFrameIndex := 0;
      FStaticVideoStartTick := 0;
      FStaticVideoLastTick := 0;
    end;
  FPendingLocalRecording := False;
  FPendingLocalFileName := '';
  Result := True;
end;


function TfrmMediaServer.BuildTimestampedMp4FileName(const ADir: string;
                                                     const APrefix: string): string;
var
  Dir: string;
  Prefix: string;

begin

  Dir := IncludeTrailingPathDelimiter(Trim(ADir));
  Prefix := Trim(APrefix);

  if (Prefix = '') then
    Prefix := 'rdj';

  ForceDirectories(Dir);

  Result := Dir +
            Prefix + '_' +
            FormatDateTime('yyyymmdd_hhnnss_zzz',
                           Now) +
            '.mp4';
end;


function TfrmMediaServer.ResolveLocalRecordingPath(): string;
var
  Dir: string;
  BaseName: string;

begin

  Dir := Trim(MainMDIFrm.Setup.AudioRecordingsPath);

  if (Dir = '') then
    Dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
           Trim(MainMDIFrm.Setup.AudioRecordingsDir);

  if (ExtractFileDrive(Dir) <> '') then
    Dir := IncludeTrailingPathDelimiter(Dir)
  else
    Dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Dir;

  ForceDirectories(Dir);

  BaseName := Trim(edRdjProRecFileName.Text);
  if (BaseName = '') then
    Result := BuildTimestampedMp4FileName(Dir,
                                          'rdj_recording')
  else
    Result := IncludeTrailingPathDelimiter(Dir) +
              ChangeFileExt(ExtractFileName(BaseName),
                            '.mp4');
end;


function TfrmMediaServer.ResolveCaddyLiveMp4Path(): string;
var
  Dir: string;

begin

  //Dir := Trim(MainMDIFrm.Setup.CaddyVideoPath);  // Server
  Dir := Trim(MainMDIFrm.Setup.VideoRecordingsPath);   // local disk

  if (Dir = '') then
    Dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
           'Video';

  if (ExtractFileDrive(Dir) <> '') then
    Dir := IncludeTrailingPathDelimiter(Dir)
  else
    Dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Dir;

  ForceDirectories(Dir);

  Result := Dir + 'live.mp4';
end;



function TfrmMediaServer.ResolveBroadcastMseDebugDir(): string;
var
  BaseDir: string;

begin

  if (FBroadcastMseDumpDir <> '') then
    Exit(FBroadcastMseDumpDir);

  // Local, fast working directory.  RDJ writes all MSE artifacts here first.
  // The server share is only a mirror target and must never be the primary
  // Media Foundation / fMP4 working path.
  if Assigned(MainMDIFrm) then
    BaseDir := Trim(MainMDIFrm.Setup.VideoRecordingsPath)
  else
    BaseDir := '';

  if (BaseDir = '') then
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
               'VideoRecordings';

  if (ExtractFileDrive(BaseDir) <> '') then
    BaseDir := IncludeTrailingPathDelimiter(BaseDir)
  else
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
               IncludeTrailingPathDelimiter(BaseDir);

  // Result := IncludeTrailingPathDelimiter(BaseDir + 'mse_debug');
  Result := BaseDir;

  ForceDirectories(Result);

  FBroadcastMseDumpDir := Result;
end;


function TfrmMediaServer.ResolveBroadcastMseMirrorDir(): string;
var
  BaseDir: string;

begin

  if (FBroadcastMseMirrorDir <> '') then
    Exit(FBroadcastMseMirrorDir);

  Result := '';

  if not Assigned(MainMDIFrm) then
    Exit;

  // Caddy server/public mirror. Example:
  //   \\SERVER001\Caddy\stream
  // MainMDIFrm.Setup.CaddyVideoPath should point to the server's Video folder.
  BaseDir := Trim(MainMDIFrm.Setup.CaddyVideoPath);

  if (BaseDir = '') then
    Exit;

  if (ExtractFileDrive(BaseDir) <> '') or
     BaseDir.StartsWith('\\') then
    BaseDir := IncludeTrailingPathDelimiter(BaseDir)
  else
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
               IncludeTrailingPathDelimiter(BaseDir);

  // Result := IncludeTrailingPathDelimiter(BaseDir + 'mse_debug');
  Result := BaseDir;

  try
    ForceDirectories(Result);
    FBroadcastMseMirrorDir := Result;
  except
    on E: Exception do
      begin
        OutputDebugString(PChar('TfrmMediaServer.MSE mirror disabled: ' + E.Message));
        Result := '';
      end;
  end;
end;



procedure TfrmMediaServer.DeleteBroadcastMseFilesByMask(const ADir: string;
                                                        const AMask: string);
var
  SearchRec: TSearchRec;
  Dir: string;
  FileName: string;

begin

  Dir := IncludeTrailingPathDelimiter(Trim(ADir));

  if (Dir = '') or
     (AMask = '') then
    Exit;

  if not DirectoryExists(Dir) then
    Exit;

  if FindFirst(Dir + AMask,
               faAnyFile,
               SearchRec) = 0 then
    try

      repeat
        if (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = 0) then
          begin
            FileName := Dir + SearchRec.Name;
            try
              DeleteFile(FileName);
            except
              on E: Exception do
                OutputDebugString(PChar(Format('TfrmMediaServer.MSE cleanup delete failed: %s err=%s',
                                               [FileName,
                                                E.Message])));
            end;
          end;
      until FindNext(SearchRec) <> 0;
    finally

      FindClose(SearchRec);
    end;
end;


procedure TfrmMediaServer.LaunchBroadcastMseCleanupBatch(const ADumpDir: string;
                                                        const AMirrorDir: string);
var
  Batch: TStringList;
  TempPath: array[0..MAX_PATH] of Char;
  BatFileName: string;
  ComSpec: string;
  CmdLine: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;

  procedure AddCleanDir(const ADir: string);
  var
    Dir: string;

  begin
    Dir := IncludeTrailingPathDelimiter(Trim(ADir));

    if (Dir = '') then
      Exit;

    Batch.Add('call :clean "' + Dir + '"');
  end;

begin

  if (Trim(ADumpDir) = '') and
     (Trim(AMirrorDir) = '') then
    Exit;

  FillChar(TempPath,
           SizeOf(TempPath),
           0);

  if GetTempPath(MAX_PATH,
                 TempPath) = 0 then
    Exit;

  BatFileName := IncludeTrailingPathDelimiter(string(TempPath)) +
                 Format('rdj_mse_cleanup_%x_%x.bat',
                        [GetCurrentProcessId(),
                         GetTickCount()]);

  Batch := TStringList.Create();
  try

    Batch.Add('@echo off');
    Batch.Add('setlocal');
    Batch.Add('timeout /t 2 /nobreak >nul 2>nul');
    AddCleanDir(ADumpDir);
    AddCleanDir(AMirrorDir);
    Batch.Add('del /f /q "%~f0" >nul 2>nul');
    Batch.Add('exit /b');
    Batch.Add(':clean');
    Batch.Add('if "%~1"=="" exit /b');
    Batch.Add('if not exist "%~1\" exit /b');
    Batch.Add('del /f /q "%~1\live.json" >nul 2>nul');
    Batch.Add('del /f /q "%~1\live.m3u8" >nul 2>nul');
    Batch.Add('del /f /q "%~1\master.m3u8" >nul 2>nul');
    Batch.Add('del /f /q "%~1\init.mp4" >nul 2>nul');
    Batch.Add('del /f /q "%~1\frag_*.m4s" >nul 2>nul');
    Batch.Add('del /f /q "%~1\patched_frag_*.m4s" >nul 2>nul');
    Batch.Add('exit /b');

    try
      Batch.SaveToFile(BatFileName);
    except
      on E: Exception do
        begin
          OutputDebugString(PChar('TfrmMediaServer.MSE close cleanup batch write failed: ' + E.Message));
          Exit;
        end;
    end;
  finally
    Batch.Free();
  end;

  ComSpec := GetEnvironmentVariable('ComSpec');
  if (ComSpec = '') then
    ComSpec := IncludeTrailingPathDelimiter(GetEnvironmentVariable('SystemRoot')) +
               'System32\cmd.exe';

  if not FileExists(ComSpec) then
    begin
      DeleteFile(BatFileName);
      Exit;
    end;

  CmdLine := '"' + ComSpec + '" /D /C "' + BatFileName + '"';
  UniqueString(CmdLine);

  FillChar(StartupInfo,
           SizeOf(StartupInfo),
           0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  FillChar(ProcessInfo,
           SizeOf(ProcessInfo),
           0);

  if CreateProcess(nil,
                   PChar(CmdLine),
                   nil,
                   nil,
                   False,
                   CREATE_NO_WINDOW,
                   nil,
                   nil,
                   StartupInfo,
                   ProcessInfo) then
    begin
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);

      OutputDebugString(PChar('TfrmMediaServer.MSE close cleanup batch launched: ' + BatFileName));
    end
  else
    begin
      OutputDebugString(PChar(Format('TfrmMediaServer.MSE close cleanup batch launch failed: err=%d file=%s',
                                     [GetLastError(),
                                      BatFileName])));
      DeleteFile(BatFileName);
    end;
end;


procedure TfrmMediaServer.CleanupBroadcastMseArtifactsOnStop();
var
  DumpDir: string;
  MirrorDir: string;

begin

  // Stop-broadcast cleanup is intentionally separate from live-window cleanup.
  // During live playback RDJ_MSE_MIRROR_DELETE_OLD_FRAGMENTS must stay False,
  // because Caddy/browser may still hold an older live.json.  When broadcasting
  // stops, however, the live session is over and the scratch MSE artifacts may
  // be removed in a safe order.

  DumpDir := Trim(FBroadcastMseDumpDir);
  MirrorDir := Trim(FBroadcastMseMirrorDir);

  // First drain any pending mirror writes/live.json publishes.  After this point
  // no background mirror job should recreate a file that we are cleaning up.
  StopBroadcastMseMirrorThread();

  if (DumpDir <> '') then
    begin
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'live.json');
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'live.m3u8');
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'master.m3u8');
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'init.mp4');
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'frag_*.m4s');
      DeleteBroadcastMseFilesByMask(DumpDir,
                                    'patched_frag_*.m4s');

      OutputDebugString(PChar('TfrmMediaServer.MSE stop cleanup: local scratch files cleaned: ' + DumpDir));
    end;

  if MirrorDir <> '' then
    begin
      // Delete the public manifest first.  A browser refresh after stop should
      // not keep discovering an old live window.  The media files are scratch
      // live artifacts and may be removed after the manifest is gone.
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'live.json');
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'live.m3u8');
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'master.m3u8');
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'init.mp4');
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'frag_*.m4s');
      DeleteBroadcastMseFilesByMask(MirrorDir,
                                    'patched_frag_*.m4s');

      OutputDebugString(PChar('TfrmMediaServer.MSE stop cleanup: mirror scratch files cleaned: ' + MirrorDir));
    end;
end;


procedure TfrmMediaServer.ResetBroadcastMseDebugDump();
begin

  StopBroadcastMseMirrorThread();

  FBroadcastMseDumpDir := '';
  FBroadcastMseMirrorDir := '';
  FBroadcastMseInitWritten := False;
  FBroadcastMseInitSize := 0;
  FBroadcastMseFragmentSeq := 0;
  FBroadcastMsePublicSeq := 0;
  FBroadcastMseGroupPartCount := 0;
  SetLength(FBroadcastMseGroupBytes, 0);

  if Assigned(FBroadcastMseGroupStream) then
    FBroadcastMseGroupStream.Clear();

  FBroadcastMseGroupFirstTick := 0;
  FBroadcastMseForceNextPublicGroup := False;
  ConfigureBroadcastMseFragmentGrouping();
  FBroadcastMseManifestWritten := False;
  FBroadcastMseLastManifestSeq := 0;
  FBroadcastMseLastCleanupSeq := 0;
  FBroadcastMseManifestPublishSeq := 0;
  FBroadcastMseSessionId := FormatDateTime('yyyymmddhhnnsszzz', Now);
  FBroadcastMseSessionStartedUtc := TTimeZone.Local.ToUniversalTime(Now);
  FLastBroadcastVideoSampleTick := GetTickCount64();
  FLastBroadcastPublicSegmentTick := GetTickCount64();
  FLastBroadcastVideoFlushTick := 0;
  FBroadcastMseLastStallTraceTick := 0;
  FBroadcastMseMemLastLogTick := 0;
  TInterlocked.Exchange(FBroadcastVideoFlushQueued, 0);
end;


procedure TfrmMediaServer.PrepareBroadcastMseVideoSourceSwitch();
begin

  // Preserve already-grouped fMP4 fragments: they contain audio too.  Dropping
  // the partial group during a camera/static switch creates an audible gap in
  // the browser.  Force the next fragment to publish the current group early.
  FBroadcastMseForceNextPublicGroup := True;
end;


function TfrmMediaServer.BroadcastMsePublicTargetMs(): Integer;
begin

  Result := FBroadcastMsePublicTargetMs;

  if (Result < RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS) then
    Result := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS
  else
    if (Result > RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS) then
      Result := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS;
end;


function TfrmMediaServer.BroadcastMseGroupSourceFragments(): Integer;
begin

  Result := FBroadcastMseGroupSourceFragments;

  if (Result < 1) then
    Result := 1;
end;


function TfrmMediaServer.BroadcastMseKeepPatchedFragments(): Integer;
begin

  Result := FBroadcastMseKeepPatchedFragments;

  if (Result < RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN) then
    Result := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN
  else
    if (Result > RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX) then
      Result := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX;
end;


function TfrmMediaServer.BroadcastMseGroupForceFlushMs(): Integer;
begin

  Result := BroadcastMsePublicTargetMs() * RDJ_MSE_GROUP_FORCE_FLUSH_FACTOR;

  if (Result < RDJ_MSE_GROUP_FORCE_FLUSH_MIN_MS) then
    Result := RDJ_MSE_GROUP_FORCE_FLUSH_MIN_MS;
end;


procedure TfrmMediaServer.ConfigureBroadcastMseFragmentGrouping();
var
  TargetMs: Integer;
  GroupCount: Integer;
  KeepCount: Integer;

begin

  TargetMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_DEFAULT_MS;

  if Assigned(MainMDIFrm) then
    TargetMs := MainMDIFrm.Setup.MsePublicSegmentTargetMs;

  if TargetMs < RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS then
    TargetMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MIN_MS
  else
  if TargetMs > RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS then
    TargetMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_MAX_MS;

  GroupCount := (TargetMs + (RDJ_MSE_SOURCE_FRAGMENT_ESTIMATE_MS div 2)) div
                RDJ_MSE_SOURCE_FRAGMENT_ESTIMATE_MS;

  if GroupCount < 1 then
    GroupCount := 1;

  KeepCount := (RDJ_MSE_PUBLIC_WINDOW_TARGET_MS + TargetMs - 1) div
               TargetMs;

  if KeepCount < RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN then
    KeepCount := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MIN
  else
  if KeepCount > RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX then
    KeepCount := RDJ_MSE_KEEP_PATCHED_FRAGMENTS_MAX;

  FBroadcastMsePublicTargetMs := TargetMs;
  FBroadcastMseGroupSourceFragments := GroupCount;
  FBroadcastMseKeepPatchedFragments := KeepCount;

  OutputDebugString(PChar(Format('TfrmMediaServer.MSE grouping: targetMs=%d sourceFragments=%d keep=%d',
                                 [FBroadcastMsePublicTargetMs,
                                  FBroadcastMseGroupSourceFragments,
                                  FBroadcastMseKeepPatchedFragments])));
end;


procedure TfrmMediaServer.WriteBytesToFile(const AFileName: string;
                                           const ABytes: TBytes);
var
  Stream: TFileStream;

begin

  if (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  ForceDirectories(ExtractFilePath(AFileName));

  Stream := TFileStream.Create(AFileName,
                               fmCreate or fmShareDenyWrite);
  try

    Stream.WriteBuffer(ABytes[0],
                       Length(ABytes));
  finally

    Stream.Free();
  end;
end;


procedure TfrmMediaServer.WriteBytesToFileAtomic(const AFileName: string;
                                                 const ABytes: TBytes);
var
  TmpFileName: string;
  I: Integer;

begin

  if (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  ForceDirectories(ExtractFilePath(AFileName));

  TmpFileName := AFileName +
                 Format('.tmp_%x_%x_%x',
                        [GetCurrentProcessId(),
                         GetCurrentThreadId(),
                         GetTickCount()]);

  WriteBytesToFile(TmpFileName,
                   ABytes);

  // Make the browser see either the previous complete file or the new
  // complete file. Never expose a half-written fragment or live.json.
  //
  // Important for Caddy/Windows: while Caddy is serving live.json it may hold
  // the old file open without delete-sharing. In that case MoveFileEx can
  // fail transiently. Do NOT DeleteFile(AFileName) as a fallback, because that
  // creates short holes where browsers see HTTP 400/404. Just retry briefly
  // and keep the previous complete file if Windows still refuses the replace.
  for I := 0 to 20 do
    begin
      if MoveFileEx(PChar(TmpFileName),
                    PChar(AFileName),
                    MOVEFILE_REPLACE_EXISTING) then
        Exit;

      Sleep(5);
    end;

  OutputDebugString(PChar(Format('TfrmMediaServer.MSE atomic write failed, keeping old file: %s err=%d',
                                 [ExtractFileName(AFileName),
                                  GetLastError()])));

  DeleteFile(TmpFileName);
end;


procedure TfrmMediaServer.WriteUtf8TextToFileAtomic(const AFileName: string;
                                                    const AText: string);
var
  Bytes: TBytes;
  Utf8: UTF8String;

begin

  Utf8 := UTF8String(AText);

  if Length(Utf8) = 0 then
    Exit;

  SetLength(Bytes,
            Length(Utf8));

  Move(PAnsiChar(Utf8)^,
       Bytes[0],
       Length(Utf8));

  WriteBytesToFileAtomic(AFileName,
                         Bytes);
end;


procedure TfrmMediaServer.SetBroadcastHandoverLockIndicator(const ACaption: string;
                                                            const AColor: TColor);
begin

  lblLockBC.Font.Color := AColor;
  lblLockBC.Caption := ACaption;
  shpBcLockedCap.Pen.Color := AColor;
  shpBcLocked.Pen.Color := AColor;
end;


function TfrmMediaServer.BroadcastHandoverBaseDir(): string;
begin

  Result := '';

  if Assigned(MainMDIFrm) then
    begin
      Result := Trim(MainMDIFrm.Setup.CaddyDir);

      if (Result = '') then
        Result := ExtractFilePath(Trim(MainMDIFrm.Setup.CaddyNowPlayingJsonFile));

      if (Result = '') then
        Result := ExtractFilePath(Trim(MainMDIFrm.Setup.CaddyVideoPath));
    end;

  Result := Trim(Result);

  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result);
end;


function TfrmMediaServer.BroadcastHandoverLockDir(): string;
begin

  Result := BroadcastHandoverBaseDir();

  if (Result <> '') then
    Result := IncludeTrailingPathDelimiter(Result + RDJ_BROADCAST_HANDOVER_LOCK_DIR);
end;


function TfrmMediaServer.BroadcastHandoverStatusFileName(): string;
begin

  Result := BroadcastHandoverBaseDir();

  if (Result <> '') then
    Result := Result + RDJ_BROADCAST_HANDOVER_STATUS_FILE;
end;


function TfrmMediaServer.BroadcastHandoverComputerName(): string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  BufferLen: DWORD;

begin

  Result := '';
  BufferLen := Length(Buffer);

  if GetComputerName(Buffer,
                     BufferLen) then
    SetString(Result,
              Buffer,
              BufferLen);
end;


function TfrmMediaServer.BroadcastHandoverOwnerDisplay(): string;
var
  ComputerName: string;
  UserName: string;

begin

  Result := '';

  if Assigned(MainMDIFrm) then
    Result := Trim(MainMDIFrm.DjName);

  if (Result = '') then
    begin
      UserName := Trim(GetEnvironmentVariable('USERNAME'));
      if UserName <> '' then
        Result := UserName;
    end;

  ComputerName := BroadcastHandoverComputerName();
  if (ComputerName <> '') then
    begin
      if (Result = '') then
        Result := ComputerName
      else
        Result := Result + ' @ ' + ComputerName;
    end;

  if (Result = '') then
    Result := 'Unknown broadcaster';
end;


function TfrmMediaServer.BroadcastHandoverJsonEscape(const AValue: string): string;
var
  I: Integer;
  C: Char;

begin

  Result := '';

  for I := 1 to Length(AValue) do
    begin
      C := AValue[I];
      case C of
        '"': Result := Result + '\"';
        '\': Result := Result + '\\';
        #8: Result := Result + '\b';
        #9: Result := Result + '\t';
        #10: Result := Result + '\n';
        #12: Result := Result + '\f';
        #13: Result := Result + '\r';
      else
        if Ord(C) < 32 then
          Result := Result + Format('\u%.4x', [Ord(C)])
        else
          Result := Result + C;
      end;
    end;
end;


function TfrmMediaServer.ExtractBroadcastHandoverJsonString(const AJson: string;
                                                            const AName: string): string;
var
  P: Integer;
  Pattern: string;
  Escaped: Boolean;
  C: Char;

begin

  Result := '';
  Pattern := '"' + AName + '"';
  P := Pos(Pattern,
           AJson);

  if P <= 0 then
    Exit;

  Inc(P,
      Length(Pattern));

  while (P <= Length(AJson)) and (AJson[P] <> ':') do
    Inc(P);

  if (P > Length(AJson)) then
    Exit;

  Inc(P);

  while (P <= Length(AJson)) and (AJson[P] <= ' ') do
    Inc(P);

  if (P > Length(AJson)) or (AJson[P] <> '"') then
    Exit;

  Inc(P);
  Escaped := False;

  while (P <= Length(AJson)) do
    begin
      C := AJson[P];

      if Escaped then
        begin
          if C = 'n' then
            Result := Result + ' '
          else if C = 'r' then
            Result := Result + ' '
          else if C = 't' then
            Result := Result + ' '
          else
            Result := Result + C;

          Escaped := False;
        end
      else if C = '\' then
        Escaped := True
      else if C = '"' then
        Exit
      else
        Result := Result + C;

      Inc(P);
    end;
end;


function TfrmMediaServer.ReadBroadcastHandoverTextFile(const AFileName: string): string;
var
  Lines: TStringList;

begin

  Result := '';

  if (AFileName = '') or
     (not FileExists(AFileName)) then
    Exit;

  Lines := TStringList.Create();
  try
    Lines.LoadFromFile(AFileName,
                       TEncoding.UTF8);
    Result := Lines.Text;
  finally
    Lines.Free();
  end;
end;


function TfrmMediaServer.IsBroadcastHandoverLockStale(const ALockDir: string): Boolean;
var
  OwnerFileName: string;
  OwnerTime: TDateTime;

begin

  Result := False;
  OwnerFileName := IncludeTrailingPathDelimiter(ALockDir) + RDJ_BROADCAST_HANDOVER_OWNER_FILE;

  if not FileExists(OwnerFileName) then
    Exit;

  if not FileAge(OwnerFileName,
                 OwnerTime) then
    Exit;

  Result := ((Now - OwnerTime) * RDJ_MS_PER_DAY) > RDJ_BROADCAST_HANDOVER_STALE_MS;
end;


procedure TfrmMediaServer.DeleteBroadcastHandoverLockDir(const ALockDir: string);
var
  SearchRec: TSearchRec;
  LockDir: string;

begin

  LockDir := IncludeTrailingPathDelimiter(ALockDir);

  DeleteFile(LockDir + RDJ_BROADCAST_HANDOVER_OWNER_FILE);

  if FindFirst(LockDir + RDJ_BROADCAST_HANDOVER_OWNER_FILE + '.tmp_*',
               faAnyFile,
               SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          DeleteFile(LockDir + SearchRec.Name);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;

  RemoveDir(ALockDir);
end;


function TfrmMediaServer.BuildBroadcastHandoverStatusJson(const AState: string;
                                                          const AMessage: string): string;
var
  DjName: string;
  ShowName: string;
  ComputerName: string;
  OwnerDisplay: string;
  ActiveText: string;

begin

  DjName := '';
  ShowName := '';

  if Assigned(MainMDIFrm) then
    begin
      DjName := Trim(MainMDIFrm.DjName);
      ShowName := Trim(MainMDIFrm.ShowName);
    end;

  ComputerName := BroadcastHandoverComputerName();
  OwnerDisplay := BroadcastHandoverOwnerDisplay();

  if SameText(AState,
              'on_air') then
    ActiveText := 'true'
  else
    ActiveText := 'false';

  Result :=
    '{' + sLineBreak +
    '  "version": 1,' + sLineBreak +
    Format('  "active": %s,', [ActiveText]) + sLineBreak +
    Format('  "state": "%s",', [BroadcastHandoverJsonEscape(AState)]) + sLineBreak +
    Format('  "ownerId": "%s",', [BroadcastHandoverJsonEscape(FBroadcastHandoverOwnerId)]) + sLineBreak +
    Format('  "owner": "%s",', [BroadcastHandoverJsonEscape(OwnerDisplay)]) + sLineBreak +
    Format('  "djName": "%s",', [BroadcastHandoverJsonEscape(DjName)]) + sLineBreak +
    Format('  "showName": "%s",', [BroadcastHandoverJsonEscape(ShowName)]) + sLineBreak +
    Format('  "computerName": "%s",', [BroadcastHandoverJsonEscape(ComputerName)]) + sLineBreak +
    Format('  "processId": %d,', [GetCurrentProcessId()]) + sLineBreak +
    Format('  "updated": "%s",', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]) + sLineBreak +
    Format('  "heartbeatTick": %d,', [GetTickCount()]) + sLineBreak +
    Format('  "message": "%s"', [BroadcastHandoverJsonEscape(AMessage)]) + sLineBreak +
    '}';
end;


procedure TfrmMediaServer.PublishBroadcastHandoverStatus(const AState: string;
                                                         const AMessage: string);
var
  StatusFileName: string;
  Json: string;

begin

  StatusFileName := BroadcastHandoverStatusFileName();
  if (StatusFileName = '') then
    Exit;

  Json := BuildBroadcastHandoverStatusJson(AState,
                                           AMessage);

  WriteUtf8TextToFileAtomic(StatusFileName,
                            Json);
end;


function TfrmMediaServer.AcquireBroadcastHandoverLock(out AMessage: string): Boolean;
var
  BaseDir: string;
  LockDir: string;
  OwnerFileName: string;
  OwnerJson: string;
  OwnerName: string;
  OwnerUpdated: string;

begin

  Result := False;
  AMessage := '';

  if FBroadcastHandoverLockAcquired then
    begin
      UpdateBroadcastHandoverHeartbeat(True);
      Result := True;
      Exit;
    end;

  BaseDir := BroadcastHandoverBaseDir();
  LockDir := BroadcastHandoverLockDir();

  if (BaseDir = '') or (LockDir = '') then
    begin
      AMessage := 'Caddy directory is not configured, so RDJ Pro cannot protect the shared broadcast stream.';
      Exit;
    end;

  ForceDirectories(BaseDir);

  if DirectoryExists(LockDir) and
     IsBroadcastHandoverLockStale(LockDir) then
    begin
      memLog.Lines.Append('Removing stale broadcast handover lock.');
      DeleteBroadcastHandoverLockDir(LockDir);
    end;

  if not CreateDir(LockDir) then
    begin
      SetBroadcastHandoverLockIndicator(CAP_LOCKED,
                                        LOCKED_COLOR);

      OwnerFileName := IncludeTrailingPathDelimiter(LockDir) + RDJ_BROADCAST_HANDOVER_OWNER_FILE;

      OwnerJson := ReadBroadcastHandoverTextFile(OwnerFileName);
      OwnerName := ExtractBroadcastHandoverJsonString(OwnerJson,
                                                      'owner');
      OwnerUpdated := ExtractBroadcastHandoverJsonString(OwnerJson,
                                                         'updated');

      if (OwnerName = '') then
        OwnerName := 'another RDJ Pro computer';

      SetBroadcastHandoverLockIndicator(CAP_LOCKED,
                                        LOCKED_COLOR);

      AMessage := 'Broadcast is already active by ' + OwnerName;

      if (OwnerUpdated <> '') then
        AMessage := AMessage + ' since ' + OwnerUpdated;

      AMessage := AMessage + '. Wait for the handover message before taking over.';
      Exit;
    end;

  FBroadcastHandoverLockAcquired := True;
  FBroadcastHandoverOwnerId := Format('%s-%d-%d',
                                      [BroadcastHandoverComputerName(),
                                       GetCurrentProcessId(),
                                       GetTickCount()]);
  FLastBroadcastHandoverHeartbeatTick := 0;

  UpdateBroadcastHandoverHeartbeat(True);
  AMessage := 'Broadcast lock acquired by ' + BroadcastHandoverOwnerDisplay() + '.';
  Result := True;
end;


procedure TfrmMediaServer.ReleaseBroadcastHandoverLock(const APublishHandoverReady: Boolean);
var
  LockDir: string;
  MessageText: string;

begin

  if not FBroadcastHandoverLockAcquired then
    Exit;

  SetBroadcastHandoverLockIndicator(CAP_UNLOCKED,
                                    UNLOCKED_COLOR);

  LockDir := BroadcastHandoverLockDir();

  if APublishHandoverReady then
    begin
      MessageText := BroadcastHandoverOwnerDisplay() + ' has finished. The next DJ can take over now.';
      PublishBroadcastHandoverStatus('handover_ready',
                                     MessageText);
    end;

  if LockDir <> '' then
    DeleteBroadcastHandoverLockDir(LockDir);

  FBroadcastHandoverLockAcquired := False;
  FBroadcastHandoverOwnerId := '';
  FLastBroadcastHandoverHeartbeatTick := 0;
end;


procedure TfrmMediaServer.UpdateBroadcastHandoverHeartbeat(const AForce: Boolean = False);
var
  Tick: DWORD;
  LockDir: string;
  OwnerFileName: string;
  Json: string;

begin

  if not FBroadcastHandoverLockAcquired then
    Exit;

  Tick := GetTickCount();

  if (not AForce) and
     (FLastBroadcastHandoverHeartbeatTick <> 0) and
     ((Tick - FLastBroadcastHandoverHeartbeatTick) < RDJ_BROADCAST_HANDOVER_HEARTBEAT_MS) then
    Exit;

  LockDir := BroadcastHandoverLockDir();
  if LockDir = '' then
    Exit;

  OwnerFileName := IncludeTrailingPathDelimiter(LockDir) + RDJ_BROADCAST_HANDOVER_OWNER_FILE;
  Json := BuildBroadcastHandoverStatusJson('on_air',
                                           BroadcastHandoverOwnerDisplay() + ' is on air.');

  WriteUtf8TextToFileAtomic(OwnerFileName,
                            Json);
  PublishBroadcastHandoverStatus('on_air',
                                 BroadcastHandoverOwnerDisplay() + ' is on air.');

  FLastBroadcastHandoverHeartbeatTick := Tick;
end;


procedure TfrmMediaServer.PollBroadcastHandoverStatus();
var
  Tick: DWORD;
  StatusFileName: string;
  Json: string;
  State: string;
  MessageText: string;
  NoticeKey: string;

begin

  Tick := GetTickCount();

  if (FLastBroadcastHandoverPollTick <> 0) and
     ((Tick - FLastBroadcastHandoverPollTick) < RDJ_BROADCAST_HANDOVER_POLL_MS) then
    Exit;

  SetBroadcastHandoverLockIndicator(CAP_UNLOCKED,
                                    UNLOCKED_COLOR);

  FLastBroadcastHandoverPollTick := Tick;
  StatusFileName := BroadcastHandoverStatusFileName();

  if (StatusFileName = '') or
     (not FileExists(StatusFileName)) then
    Exit;

  Json := ReadBroadcastHandoverTextFile(StatusFileName);
  State := ExtractBroadcastHandoverJsonString(Json,
                                              'state');

  if not SameText(State,
                  'handover_ready') then
    Exit;

  MessageText := ExtractBroadcastHandoverJsonString(Json,
                                                    'message');
  if (MessageText = '') then
    MessageText := 'The previous DJ has finished. The next DJ can take over now.';

  NoticeKey := State + '|' + MessageText;
  if SameText(NoticeKey,
              FLastBroadcastHandoverNotice) then
    Exit;

  FLastBroadcastHandoverNotice := NoticeKey;
  memLog.Lines.Append('Handover ready: ' + MessageText);
  //

end;


function TfrmMediaServer.EnsureBroadcastMseMirrorThread(): TRdjProMseMirrorThread;
begin

  if not Assigned(FBroadcastMseMirrorThread) then
    FBroadcastMseMirrorThread := TRdjProMseMirrorThread.Create(512);

  Result := FBroadcastMseMirrorThread;
end;


procedure TfrmMediaServer.StopBroadcastMseMirrorThread();
begin

  if Assigned(FBroadcastMseMirrorThread) then
    FreeAndNil(FBroadcastMseMirrorThread);
end;


procedure TfrmMediaServer.MirrorBytesToServerAtomic(const AFileName: string;
                                                    const ABytes: TBytes);
var
  Worker: TRdjProMseMirrorThread;

begin

  if (AFileName = '') or
     (Length(ABytes) = 0) then
    Exit;

  Worker := EnsureBroadcastMseMirrorThread();
  Worker.EnqueueWrite(AFileName,
                      ABytes);
end;


procedure TfrmMediaServer.MirrorFileToServer(const ASourceFileName: string;
                                             const ADestFileName: string);
var
  Worker: TRdjProMseMirrorThread;

begin

  if (ASourceFileName = '') or (ADestFileName = '') then
    Exit;

  Worker := EnsureBroadcastMseMirrorThread();
  Worker.EnqueueCopy(ASourceFileName,
                     ADestFileName);
end;


procedure TfrmMediaServer.MirrorUtf8TextToServerAtomic(const AFileName: string;
                                                       const AText: string);
var
  Bytes: TBytes;
  Utf8: UTF8String;
  Worker: TRdjProMseMirrorThread;

begin

  if (AFileName = '') or (AText = '') then
    Exit;

  Utf8 := UTF8String(AText);

  SetLength(Bytes,
            Length(Utf8));

  Move(PAnsiChar(Utf8)^,
       Bytes[0],
       Length(Utf8));

  Worker := EnsureBroadcastMseMirrorThread();

  Worker.EnqueueWrite(AFileName,
                      Bytes);
end;


procedure TfrmMediaServer.MirrorDeleteServerFile(const AFileName: string);
var
  Worker: TRdjProMseMirrorThread;

begin

  if (AFileName = '') then
    Exit;

  Worker := EnsureBroadcastMseMirrorThread();
  Worker.EnqueueDelete(AFileName);
end;


function TfrmMediaServer.BroadcastMseFirstPublishedSeq(const ALastFragmentSeq: Integer): Integer;
begin

  Result := ALastFragmentSeq - BroadcastMseKeepPatchedFragments() + 1;

  if (Result < 1) then
    Result := 1;
end;


procedure TfrmMediaServer.CleanupOldBroadcastMseFragments(const ALastFragmentSeq: Integer);
var
  DumpDir: string;
  MirrorDir: string;
  FirstKeepSeq: Integer;
  Seq: Integer;
  FileName: string;
  RawFileName: string;

begin

  if (ALastFragmentSeq <= 0) then
    Exit;

  FirstKeepSeq := BroadcastMseFirstPublishedSeq(ALastFragmentSeq);

  // Nothing became newly deletable.
  if FBroadcastMseLastCleanupSeq >= (FirstKeepSeq - 1) then
    Exit;

  DumpDir := ResolveBroadcastMseDebugDir();
  MirrorDir := ResolveBroadcastMseMirrorDir();

  for Seq := FBroadcastMseLastCleanupSeq + 1 to FirstKeepSeq - 1 do
    begin

      FileName := DumpDir +
                  Format('patched_frag_%.6d.m4s',
                         [Seq]);

      if FileExists(FileName) then
        DeleteFile(FileName);

      if (MirrorDir <> '') and RDJ_MSE_MIRROR_DELETE_OLD_FRAGMENTS then
        begin
          FileName := MirrorDir + Format('patched_frag_%.6d.m4s',
                                         [Seq]);

          MirrorDeleteServerFile(FileName);
        end;

      // Defensive cleanup for old test runs or if raw-debug was enabled earlier.
      if not RDJ_KEEP_RAW_MSE_FRAGMENTS then
        begin
          RawFileName := DumpDir + Format('frag_%.6d.m4s',
                                          [Seq]);

          if FileExists(RawFileName) then
            DeleteFile(RawFileName);

          if (MirrorDir <> '') and RDJ_MSE_MIRROR_DELETE_OLD_FRAGMENTS then
            begin
              RawFileName := MirrorDir + Format('frag_%.6d.m4s',
                                                [Seq]);

              MirrorDeleteServerFile(RawFileName);
            end;
        end;
    end;

  FBroadcastMseLastCleanupSeq := FirstKeepSeq - 1;

  if (ALastFragmentSeq <= 5) or
     ((ALastFragmentSeq mod 25) = 0) then
    OutputDebugString(PChar(Format('TfrmMediaServer.MSE local cleanup: first=%d last=%d deletedThrough=%d',
                                   [FirstKeepSeq,
                                    ALastFragmentSeq,
                                    FBroadcastMseLastCleanupSeq])));
end;


procedure TfrmMediaServer.WriteBroadcastHlsManifest(const AFirstFragmentSeq,
                                                           ALastFragmentSeq: Integer);
var
  DumpDir: string;
  MirrorDir: string;
  Manifest: string;
  MasterManifest: string;
  SegmentDurationMs: Integer;
  TargetDurationSeconds: Integer;
  Seq: Integer;
  SegmentStartUtc: TDateTime;

begin

  if (AFirstFragmentSeq <= 0) or
     (ALastFragmentSeq < AFirstFragmentSeq) then
    Exit;

  SegmentDurationMs := BroadcastMsePublicTargetMs();
  if SegmentDurationMs < 1 then
    SegmentDurationMs := RDJ_MSE_PUBLIC_FRAGMENT_TARGET_DEFAULT_MS;

  TargetDurationSeconds := (SegmentDurationMs + 999) div 1000;
  if TargetDurationSeconds < 1 then
    TargetDurationSeconds := 1;

  Manifest := '#EXTM3U' + sLineBreak +
              '#EXT-X-VERSION:7' + sLineBreak +
              '#EXT-X-INDEPENDENT-SEGMENTS' + sLineBreak +
              Format('#EXT-X-TARGETDURATION:%d',
                     [TargetDurationSeconds]) + sLineBreak +
              Format('#EXT-X-MEDIA-SEQUENCE:%d',
                     [AFirstFragmentSeq]) + sLineBreak +
              Format('#EXT-X-MAP:URI="init.mp4?session=%s"',
                     [FBroadcastMseSessionId]) + sLineBreak;

  // Use integer formatting so the HLS decimal separator is always a dot,
  // regardless of the Windows locale used by the radio workstation.
  for Seq := AFirstFragmentSeq to ALastFragmentSeq do
    begin
      SegmentStartUtc := FBroadcastMseSessionStartedUtc +
                         (((Seq - 1) * Int64(SegmentDurationMs)) / MSecsPerDay);
      Manifest := Manifest +
                  '#EXT-X-PROGRAM-DATE-TIME:' +
                  FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"',
                                 SegmentStartUtc) + sLineBreak +
                  Format('#EXTINF:%d.%.3d,',
                         [SegmentDurationMs div 1000,
                          SegmentDurationMs mod 1000]) + sLineBreak +
                  Format('patched_frag_%.6d.m4s?session=%s',
                         [Seq,
                          FBroadcastMseSessionId]) + sLineBreak;
    end;

  DumpDir := ResolveBroadcastMseDebugDir();
  MirrorDir := ResolveBroadcastMseMirrorDir();

  WriteUtf8TextToFileAtomic(DumpDir + 'live.m3u8',
                            Manifest);

  MasterManifest := '#EXTM3U' + sLineBreak +
                    '#EXT-X-VERSION:7' + sLineBreak +
                    '#EXT-X-INDEPENDENT-SEGMENTS' + sLineBreak +
                    '#EXT-X-STREAM-INF:BANDWIDTH=3500000,' +
                    'AVERAGE-BANDWIDTH=2800000,' +
                    'CODECS="avc1.42C01F,mp4a.40.2"' + sLineBreak +
                    Format('live.m3u8?session=%s',
                           [FBroadcastMseSessionId]) + sLineBreak;

  WriteUtf8TextToFileAtomic(DumpDir + 'master.m3u8',
                            MasterManifest);
  if MirrorDir <> '' then
    begin
      MirrorUtf8TextToServerAtomic(MirrorDir + 'live.m3u8',
                                   Manifest);
      MirrorUtf8TextToServerAtomic(MirrorDir + 'master.m3u8',
                                   MasterManifest);
    end;
end;


procedure TfrmMediaServer.WriteBroadcastMseLiveManifest(const ALastFragmentSeq: Integer);
var
  DumpDir: string;
  MirrorDir: string;
  Json: string;
  FirstSeq: Integer;
  WasManifestWritten: Boolean;

begin

  WasManifestWritten := FBroadcastMseManifestWritten;

  if (ALastFragmentSeq <= 0) then
    Exit;

  if (not FBroadcastMseInitWritten) then
    Exit;

  if (not FBroadcastMseManifestWritten) and
     (ALastFragmentSeq < RDJ_MSE_STARTUP_MIN_PUBLIC_FRAGMENTS) then
    Exit;

  if FBroadcastMseManifestWritten and
     (ALastFragmentSeq = FBroadcastMseLastManifestSeq) then
    Exit;

  DumpDir := ResolveBroadcastMseDebugDir();
  MirrorDir := ResolveBroadcastMseMirrorDir();
  FirstSeq := BroadcastMseFirstPublishedSeq(ALastFragmentSeq);
  Inc(FBroadcastMseManifestPublishSeq);

  // Milestone 8: live.json is public state.  Never announce a fragment until
  // its final patched file exists in the local public folder.  The mirror
  // thread applies the same rule again for the Caddy/UNC side by publishing
  // only up to FLastMirroredPatchedSeq.
  if not FileExists(DumpDir + Format('patched_frag_%.6d.m4s',
                                     [ALastFragmentSeq])) then
    begin
      OutputDebugString(PChar(Format('TfrmMediaServer.MSE manifest deferred, missing local patched_frag_%.6d.m4s',
                                     [ALastFragmentSeq])));
      Exit;
    end;

  Json := '{' + sLineBreak +
          '  "version": 1,' + sLineBreak +
          '  "live": true,' + sLineBreak +
          '  "init": "init.mp4",' + sLineBreak +
          Format('  "first": %d,',
                 [FirstSeq]) + sLineBreak +
          Format('  "last": %d,',
                 [ALastFragmentSeq]) + sLineBreak +
          Format('  "publishSeq": %d,',
                 [FBroadcastMseManifestPublishSeq]) + sLineBreak +
          Format('  "sessionId": "%s",',
                 [FBroadcastMseSessionId]) + sLineBreak +
          Format('  "sourceSeq": %d,',
                 [FBroadcastMseFragmentSeq]) + sLineBreak +
          Format('  "pendingGroupParts": %d,',
                 [FBroadcastMseGroupPartCount]) + sLineBreak +
                  '  "prefix": "patched_frag_",' + sLineBreak +
                  '  "ext": ".m4s",' + sLineBreak +
                  '  "digits": 6,' + sLineBreak +
                  '  "codec": "video/mp4; codecs=\"avc1.42C01F, mp4a.40.2\"",' + sLineBreak +
          Format('  "pollMs": %d,',
                 [RDJ_MSE_MANIFEST_POLL_MS]) + sLineBreak +
          Format('  "fragmentTargetMs": %d,', [
                 BroadcastMsePublicTargetMs()]) + sLineBreak +
          Format('  "groupSourceFragments": %d,',
                 [BroadcastMseGroupSourceFragments()]) + sLineBreak +
          Format('  "keepBehind": %d',
                 [BroadcastMseKeepPatchedFragments()]) + sLineBreak + '}';

  WriteUtf8TextToFileAtomic(DumpDir + 'live.json',
                            Json);

  if MirrorDir <> '' then
    MirrorUtf8TextToServerAtomic(MirrorDir + 'live.json',
                                 Json);

  // Browsers continue to use live.json; Chromecast consumes the HLS view.
  WriteBroadcastHlsManifest(FirstSeq,
                            ALastFragmentSeq);

  FBroadcastMseManifestWritten := True;
  FBroadcastMseLastManifestSeq := ALastFragmentSeq;

  // Publication can run on a media callback thread. Marshal the first-ready
  // transition to the form before enabling Cast live.
  if not WasManifestWritten then
    PostMessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);

  CleanupOldBroadcastMseFragments(ALastFragmentSeq);

  if (ALastFragmentSeq <= 5) or
     ((ALastFragmentSeq mod 25) = 0) then
    OutputDebugString(PChar(Format('TfrmMediaServer.MSE manifest: live.json last=%d',
                                   [ALastFragmentSeq])));
end;


function TfrmMediaServer.CurrentBroadcastMseGroupBytes(): Int64;
begin

  if Assigned(FBroadcastMseGroupStream) then
    Result := FBroadcastMseGroupStream.Size
  else
    Result := Length(FBroadcastMseGroupBytes);
end;


procedure TfrmMediaServer.DumpBroadcastMseSegments();
const
  // Cast's FMP4 HLS mode expects CMAF media segments.  Media Foundation emits
  // the moof/mdat data, so add the segment type box when grouping fragments.
  BroadcastCmafSegmentTypeBox: array[0..23] of Byte =
    ($00, $00, $00, $18,              // box size
     $73, $74, $79, $70,              // styp
     $63, $6D, $66, $73,              // cmfs
     $00, $00, $00, $00,              // minor version
     $63, $6D, $66, $73,              // cmfs
     $69, $73, $6F, $36);             // iso6
var
  InitSegment: TBytes;
  PatchedFragment: TBytes;
  PublicFragment: TBytes;
  DumpDir: string;
  MirrorDir: string;
  PublicPatchedFileName: string;
  I: Integer;
  LastPublishedSeq: Integer;

  function CurrentPublicGroupBytes(): Int64;
  begin

    if Assigned(FBroadcastMseGroupStream) then
      Result := FBroadcastMseGroupStream.Size
    else
      Result := Length(FBroadcastMseGroupBytes);
  end;

  procedure ClearPublicGroup();
  begin

    SetLength(FBroadcastMseGroupBytes,
              0);

    if Assigned(FBroadcastMseGroupStream) then
      FBroadcastMseGroupStream.Clear();

    FBroadcastMseGroupPartCount := 0;
    FBroadcastMseGroupFirstTick := 0;
  end;

  procedure AppendToPublicGroup(const ABytes: TBytes);
  begin

    if (Length(ABytes) <= 0) then
      Exit;

    if not Assigned(FBroadcastMseGroupStream) then
      FBroadcastMseGroupStream := TMemoryStream.Create();

    if (FBroadcastMseGroupPartCount <= 0) then
      begin
        FBroadcastMseGroupFirstTick := GetTickCount64();
        FBroadcastMseGroupStream.WriteBuffer(BroadcastCmafSegmentTypeBox,
                                             SizeOf(BroadcastCmafSegmentTypeBox));
      end;

    FBroadcastMseGroupStream.WriteBuffer(ABytes[0],
                                         Length(ABytes));
    Inc(FBroadcastMseGroupPartCount);
  end;

  function PopPublicGroup(out ABytes: TBytes): Boolean;
  var
    GroupSize: Int64;

  begin

    Result := (FBroadcastMseGroupPartCount >= BroadcastMseGroupSourceFragments()) or
              (FBroadcastMseForceNextPublicGroup and
               (FBroadcastMseGroupPartCount > 0));

    if not Result then
      Exit;

    GroupSize := CurrentPublicGroupBytes();
    if (GroupSize <= 0) then
      Exit(False);

    SetLength(ABytes,
              Integer(GroupSize));

    FBroadcastMseGroupStream.Position := 0;
    FBroadcastMseGroupStream.ReadBuffer(ABytes[0],
                                        Integer(GroupSize));

    FBroadcastMseForceNextPublicGroup := False;
    ClearPublicGroup();
  end;


  function PopPublicGroupForced(out ABytes: TBytes): Boolean;
  var
    AgeMs: UInt64;
    GroupSize: Int64;

  begin

    Result := False;

    if (FBroadcastMseGroupPartCount <= 0) then
      Exit;

    GroupSize := CurrentPublicGroupBytes();
    if (GroupSize <= 0) then
      Exit;

    if (FBroadcastMseGroupFirstTick <= 0) then
      FBroadcastMseGroupFirstTick := GetTickCount64();

    AgeMs := GetTickCount64() - FBroadcastMseGroupFirstTick;

    if AgeMs < UInt64(BroadcastMseGroupForceFlushMs()) then
      Exit;

    SetLength(ABytes,
              Integer(GroupSize));

    FBroadcastMseGroupStream.Position := 0;
    FBroadcastMseGroupStream.ReadBuffer(ABytes[0],
                                        Integer(GroupSize));

    OutputDebugString(PChar(Format('TfrmMediaServer.MSE force public group flush: parts=%d targetParts=%d ageMs=%d bytes=%d',
                                   [FBroadcastMseGroupPartCount,
                                    BroadcastMseGroupSourceFragments(),
                                    AgeMs,
                                    Length(ABytes)])));

    ClearPublicGroup();
    Result := True;
  end;


  procedure PublishPublicFragment(const ABytes: TBytes);
  begin

    if (Length(ABytes) <= 0) then
      Exit;

    Inc(FBroadcastMsePublicSeq);

    PublicPatchedFileName := DumpDir + Format('patched_frag_%.6d.m4s',
                                              [FBroadcastMsePublicSeq]);

    WriteBytesToFileAtomic(PublicPatchedFileName,
                           ABytes);

    if (MirrorDir <> '') then
      begin
        // Milestone 24 memory guard:
        // Do not copy the 1-2 MB public media payload into a mirror-thread
        // TBytes job. The local public fragment already exists on disk, so the
        // mirror thread only needs source and destination filenames. This keeps
        // a slow UNC/Caddy share from turning the job queue into hundreds of MB
        // of retained heap.
        MirrorFileToServer(PublicPatchedFileName,
                           MirrorDir + ExtractFileName(PublicPatchedFileName));
      end;

    LastPublishedSeq := FBroadcastMsePublicSeq;
    FLastBroadcastPublicSegmentTick := GetTickCount64();
    FBroadcastMseLastStallTraceTick := 0;

    if (FBroadcastMsePublicSeq <= 5) or
       ((FBroadcastMsePublicSeq mod 10) = 0) then
      OutputDebugString(PChar(Format('TfrmMediaServer.MSE public dump: %s bytes=%d sourceParts=%d sourceSeq=%d',
                                     [ExtractFileName(PublicPatchedFileName),
                                      Length(ABytes),
                                      BroadcastMseGroupSourceFragments(),
                                      FBroadcastMseFragmentSeq])));
  end;


begin

  LastPublishedSeq := 0;

  if not Assigned(FRdjProBroadcastMp4Recorder) then
    Exit;

  if not FRdjProBroadcastMp4Recorder.Active then
    Exit;

  if FBroadcastPublishing then
    begin
      DumpDir := ResolveBroadcastMseDebugDir();
      MirrorDir := ResolveBroadcastMseMirrorDir();
    end
  else
    begin
      DumpDir := '';
      MirrorDir := '';
    end;

  // The recorder first sees only the ftyp box (24 bytes), then later the
  // complete init segment once moov arrives. Do not lock ourselves to the
  // early stub. Rewrite init.mp4 whenever the available init segment grows.
  if FBroadcastPublishing and
     FRdjProBroadcastMp4Recorder.GetInitSegment(InitSegment) and
     (Length(InitSegment) > FBroadcastMseInitSize) then
    begin

      WriteBytesToFileAtomic(DumpDir + 'init.mp4',
                             InitSegment);

      if MirrorDir <> '' then
        MirrorBytesToServerAtomic(MirrorDir + 'init.mp4',
                                  InitSegment);

      FBroadcastMseInitWritten := Length(InitSegment) > 24;
      FBroadcastMseInitSize := Length(InitSegment);

      OutputDebugString(PChar(Format('TfrmMediaServer.MSE dump: init.mp4 bytes=%d',
                                     [Length(InitSegment)])));
    end;

  // Milestone 15:
  // Keep Media Foundation's proven small internal fragment rhythm, but do not
  // publish every tiny chunk to the browser/Caddy side.  Concatenate several
  // complete fMP4 fragments into one public .m4s file.  MSE accepts a segment
  // containing multiple moof+mdat pairs, and the timestamps remain exactly the
  // timestamps produced by the recorder.  Result: about 6-7x fewer files,
  // mirror jobs, manifest opportunities, and browser fetches.
  for I := 0 to 3 do
    begin

      // Milestone 24:
      // The browser only needs patched fragments.  Milestone 23 proved that
      // source/public drift was useful to trace, but keeping raw+patched
      // fragment pairs doubles memory pressure.  Pop only the patched fragment
      // and count it as one internal source fragment.
      if not FRdjProBroadcastMp4Recorder.TryPopPatchedFragment(PatchedFragment) then
        Break;

      Inc(FBroadcastMseFragmentSeq);

      // The Chromecast path mirrors MfSimpleCastPlayer: one growing fMP4 HTTP
      // resource. Browser/Caddy publication below remains independent.
      ForwardCastLiveFragment(PatchedFragment);

      if FBroadcastPublishing then
        AppendToPublicGroup(PatchedFragment);

      PatchedFragment := nil;

      if FBroadcastPublishing and
         PopPublicGroup(PublicFragment) then
        begin
          PublishPublicFragment(PublicFragment);
          PublicFragment := nil;
        end;

      if (FBroadcastMseFragmentSeq <= 5) or ((FBroadcastMseFragmentSeq mod 25) = 0) then
        OutputDebugString(PChar(Format('TfrmMediaServer.MSE source patched: seq=%d groupParts=%d/%d publicSeq=%d groupBytes=%d',
                                       [FBroadcastMseFragmentSeq,
                                        FBroadcastMseGroupPartCount,
                                        BroadcastMseGroupSourceFragments(),
                                        FBroadcastMsePublicSeq,
                                        CurrentPublicGroupBytes()])));
    end;

  if FBroadcastPublishing and
     (LastPublishedSeq <= 0) and
     PopPublicGroupForced(PublicFragment) then
    PublishPublicFragment(PublicFragment);

  if (LastPublishedSeq > 0) then
    WriteBroadcastMseLiveManifest(LastPublishedSeq);
end;


procedure TfrmMediaServer.LogBroadcastMseMemoryHeartbeat(const AWhere: string);
var
  NowTick: UInt64;
  Mem: TRdjProcessMemoryCountersEx;
  WorkMb: UInt64;
  PeakWorkMb: UInt64;
  PagefileMb: UInt64;
  PrivateMb: UInt64;
  MirrorQ: Integer;
  Diag: TRdjProFmp4LiveDiagnostics;

begin

  if not FRdjProBroadcasting then
    Exit;

  NowTick := GetTickCount64();
  if (FBroadcastMseMemLastLogTick <> 0) and
     ((NowTick - FBroadcastMseMemLastLogTick) < 5000) then
    Exit;

  FBroadcastMseMemLastLogTick := NowTick;

  FillChar(Mem,
           SizeOf(Mem),
           0);

  Mem.cb := SizeOf(Mem);

  WorkMb := 0;
  PeakWorkMb := 0;
  PagefileMb := 0;
  PrivateMb := 0;

  if (RDJGetProcessMemoryInfo(GetCurrentProcess(),
                             @Mem,
                             SizeOf(Mem)) <> False) then
    begin
      WorkMb := RDJBytesToMb(UInt64(Mem.WorkingSetSize));
      PeakWorkMb := RDJBytesToMb(UInt64(Mem.PeakWorkingSetSize));
      PagefileMb := RDJBytesToMb(UInt64(Mem.PagefileUsage));
      PrivateMb := RDJBytesToMb(UInt64(Mem.PrivateUsage));
    end;

  FillChar(Diag,
           SizeOf(Diag),
           0);

  if Assigned(FRdjProBroadcastMp4Recorder) then
    FRdjProBroadcastMp4Recorder.GetLiveDiagnostics(Diag);

  MirrorQ := -1;

  if Assigned(FBroadcastMseMirrorThread) then
    MirrorQ := FBroadcastMseMirrorThread.QueueCount();

  OutputDebugString(PChar(Format('TfrmMediaServer.MSE MEM[%s]: WorkMB=%d PeakWorkMB=%d PagefileMB=%d PrivateMB=%d VQ=%d AQ=%d RawQ=%d PatchedQ=%d MirrorQ=%d GroupParts=%d/%d GroupBytes=%d PublicSeq=%d SourceSeq=%d LastVms=%d LastAms=%d ParserBytes=%d PendingMoof=%d TotalBytes=%d',
                                 [AWhere,
                                  WorkMb,
                                  PeakWorkMb,
                                  PagefileMb,
                                  PrivateMb,
                                  Diag.VideoQueueCount,
                                  Diag.AudioQueueCount,
                                  Diag.RawFragmentQueueCount,
                                  Diag.PatchedFragmentQueueCount,
                                  MirrorQ,
                                  FBroadcastMseGroupPartCount,
                                  BroadcastMseGroupSourceFragments(),
                                  CurrentBroadcastMseGroupBytes(),
                                  FBroadcastMsePublicSeq,
                                  FBroadcastMseFragmentSeq,
                                  Diag.LastVideoWriteElapsedMs,
                                  Diag.LastAudioWriteElapsedMs,
                                  Diag.ParserBufferSize,
                                  Diag.PendingMoofBytes,
                                  Diag.TotalBytesWritten])));

  if ((Diag.LastVideoWriteElapsedMs >= RDJ_MSE_RECORDER_RESTART_SLOW_VIDEO_MS) and
      (Diag.VideoQueueCount >= RDJ_MSE_RECORDER_RESTART_VIDEO_QUEUE)) or
     (Diag.SlowVideoWriteStreak >= 2) then
    begin

      QueueBroadcastMseRecorderRestart(Format('slow fMP4 video writer: lastVideoMs=%d streak=%d videoQueue=%d workMB=%d privateMB=%d publicSeq=%d sourceSeq=%d',
                                              [Diag.LastVideoWriteElapsedMs,
                                               Diag.SlowVideoWriteStreak,
                                               Diag.VideoQueueCount,
                                               WorkMb,
                                               PrivateMb,
                                               FBroadcastMsePublicSeq,
                                               FBroadcastMseFragmentSeq]));
      Exit;
    end;
end;


procedure TfrmMediaServer.QueueBroadcastMseRecorderRestart(const AReason: string);
var
  OldRecorder: TRdjProFMp4Recorder;
  RestartThread: TThread;
  Reason: string;
  RestartNo: Integer;

begin

  Reason := Trim(AReason);

  if (Reason = '') then
    Reason := 'public MSE output stalled';

  if (TInterlocked.CompareExchange(FBroadcastMseRecorderRestartQueued,
                                  1,
                                  0) <> 0) then
    Exit;

  Inc(FBroadcastMseRecorderRestartCount);
  RestartNo := FBroadcastMseRecorderRestartCount;

  OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart queued: count=%d reason=%s publicSeq=%d sourceSeq=%d',
                                 [RestartNo,
                                  Reason,
                                  FBroadcastMsePublicSeq,
                                  FBroadcastMseFragmentSeq])));

  // Stop feeding the stalled writer immediately. The UI remains ON AIR while
  // the internal fMP4 encoder session is rolled over.
  FRdjProBroadcasting := False;

  OldRecorder := FRdjProBroadcastMp4Recorder;
  FRdjProBroadcastMp4Recorder := nil;

  RestartThread := TThread.CreateAnonymousThread(
    procedure
    var
      StopHr: HRESULT;
      RestartUiProc: TThreadProcedure;

    begin

      StopHr := S_OK;

      if Assigned(OldRecorder) then
        begin
          StopHr := OldRecorder.StopRecording();
          if FAILED(StopHr) then
            OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart: StopRecording failed hr=0x%.8x; freeing recorder via destructor fallback',
                                            [Cardinal(StopHr)])));

          OldRecorder.Free();
        end;

      RestartUiProc := procedure
                       var
                         Hr: HRESULT;
                         FileName: string;
                         NewRecorder: TRdjProFMp4Recorder;
                         RestartVideoType: IMFMediaType;
                         RestartOk: Boolean;

                       begin

                         RestartOk := False;
                         NewRecorder := nil;
                         RestartVideoType := nil;

                         OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart stop completed: count=%d stopHr=0x%.8x reason=%s',
                                                        [RestartNo,
                                                         Cardinal(StopHr),
                                                         Reason])));

                         try

                           if FAILED(StopHr) then
                             Exit;

                           if not chkBroadcast.Down then
                             Exit;

                           CleanupBroadcastMseArtifactsOnStop();
                           ResetBroadcastMseDebugDump();

                           if FRdjProStaticImage then
                             begin
                               if not EnsureRdjProStaticVideoMediaType() then
                                 Exit;

                               RestartVideoType := FStaticVideoMediaType;
                             end
                           else
                             begin
                               if not Assigned(FRdjProCaptureManager) then
                                 Exit;

                               if not EnsureRdjProVideoSampleReader() then
                                 Exit;

                               if not Assigned(FRdjProCaptureManager.VideoSourceReaderMediaType) then
                                 Exit;

                               RestartVideoType := FRdjProCaptureManager.VideoSourceReaderMediaType;
                             end;

                           NewRecorder := TRdjProFMp4Recorder.Create();

                           Hr := NewRecorder.SetVideoPreviewMediaType(RestartVideoType);
                           if FAILED(Hr) then
                             begin
                               OutputDebugString(PChar(Format(
                                 'TfrmMediaServer.MSE recorder restart SetVideoPreviewMediaType failed: hr=0x%.8x',
                                 [Cardinal(Hr)])));
                               Exit;
                             end;

                           if not FRecordVideoOnly then
                             begin
                               if not FLastRdjProAudioWfxValid then
                                 begin
                                   OutputDebugString(PChar('TfrmMediaServer.MSE recorder restart failed: audio format not available'));
                                   Exit;
                                 end;

                               Hr := NewRecorder.SetAudioWaveFormat(@FLastRdjProAudioWfx);
                               if FAILED(Hr) then
                                 begin
                                   OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart SetAudioWaveFormat failed: hr=0x%.8x',
                                                                  [Cardinal(Hr)])));
                                   Exit;
                                 end;
                             end;

                           FileName := ResolveCaddyLiveMp4Path();

                           Hr := NewRecorder.StartRecording(FileName,
                                                            FRecordVideoOnly);
                           if FAILED(Hr) then
                             begin
                               OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart StartRecording failed: hr=0x%.8x',
                                                              [Cardinal(Hr)])));
                               Exit;
                             end;

                           FRdjProBroadcastMp4Recorder := NewRecorder;
                           NewRecorder := nil;
                           FActiveBroadcastVideoMediaType := RestartVideoType;

                           ResetBroadcastMseDebugDump();

                           FRdjProBroadcasting := True;

                           if FRdjProStaticImage and not FRdjProRecording then
                             begin
                               FStaticVideoFrameIndex := 0;
                               FStaticVideoStartTick := 0;
                               FStaticVideoLastTick := 0;
                             end;

                           tmrTime.Enabled := True;
                           chkBroadcast.Checked := FBroadcastPublishing;
                           chkBroadcast.Down := FBroadcastPublishing;
                           chkRdjProCamera.Enabled := FRdjProStaticImage and
                                                      (not FRecordingRdjPro);
                           chkRecordVideoOnly.Enabled := False;
                           chkRdjProStaticImage.Enabled := not FRecordingRdjPro;
                           UpdateOnAirLamp(FBroadcastPublishing);

                           if not FRdjProStaticImage then
                             begin
                               RefreshRdjProCameraPreview();
                               ScheduleRdjProCameraPreviewRefresh();
                             end;

                           RestartOk := True;

                           OutputDebugString(PChar(Format('TfrmMediaServer.MSE recorder restart completed: count=%d sessionId=%s reason=%s',
                                                          [RestartNo,
                                                           FBroadcastMseSessionId,
                                                           Reason])));
                         finally
                           if Assigned(NewRecorder) then
                             begin
                               NewRecorder.StopRecording();
                               NewRecorder.Free();
                             end;

                           if not RestartOk then
                             begin
                               FRdjProBroadcasting := False;
                               chkBroadcast.Checked := False;
                               chkBroadcast.Down := False;
                               chkRdjProCamera.Enabled := not FRecordingRdjPro;
                               chkRecordVideoOnly.Enabled := not FRecordingRdjPro;
                               chkRdjProStaticImage.Enabled := not FRecordingRdjPro;
                               UpdateOnAirLamp(False);
                               OutputDebugString(PChar(Format(
                                 'TfrmMediaServer.MSE recorder restart failed: count=%d reason=%s',
                                 [RestartNo,
                                  Reason])));
                             end;

                           TInterlocked.Exchange(FBroadcastMseRecorderRestartQueued, 0);
                         end;
                       end;
      TThread.Queue(nil,
                    RestartUiProc);
    end);

  RestartThread.FreeOnTerminate := True;
  RestartThread.Start();
end;


procedure TfrmMediaServer.RecoverBroadcastAfterAudioGraphRestart(const AReason: string);
var
  Reason: string;

begin

  if (not FRdjProBroadcasting) and
     (not chkBroadcast.Down) then
    Exit;

  Reason := Trim(AReason);
  if (Reason = '') then
    Reason := 'audio graph recovery';

  QueueBroadcastMseRecorderRestart('audio graph recovered: ' + Reason);
end;


procedure TfrmMediaServer.CheckBroadcastMseVideoSourceFlush();
var
  NowTick: UInt64;
  VideoAgeMs: UInt64;
  PublicAgeMs: UInt64;
  SinceLastFlushMs: UInt64;
  PublicStaleMs: UInt64;
  FlushProc: TThreadProcedure;

begin

  if not FRdjProBroadcasting then
    Exit;

  if not Assigned(FRdjProBroadcastMp4Recorder) then
    Exit;

  if not FRdjProBroadcastMp4Recorder.Active then
    Exit;

  if FRdjProStaticImage then
    Exit;

  NowTick := GetTickCount64();

  if (FLastBroadcastVideoSampleTick = 0) then
    FLastBroadcastVideoSampleTick := NowTick;

  if (FLastBroadcastPublicSegmentTick = 0) then
    FLastBroadcastPublicSegmentTick := NowTick;

  VideoAgeMs := NowTick - FLastBroadcastVideoSampleTick;
  PublicAgeMs := NowTick - FLastBroadcastPublicSegmentTick;
  PublicStaleMs := UInt64(BroadcastMseGroupForceFlushMs() + 3000);

  // Two different problems have different cures:
  //
  // 1) VideoAgeMs stale: camera/SourceReader has stopped delivering samples.
  //    Use the hard SourceReader stream-selection flush path.
  //
  // 2) VideoAgeMs fresh but PublicAgeMs stale: the camera is fine, but the
  //    public output did not advance.  Milestone 23 deliberately does NOT
  //    flush/clear live SinkWriter or public group state here.  We only trace
  //    the bridge from capture byte stream -> source pair -> public group.
  if (VideoAgeMs < 4000) then
    begin
      if (PublicAgeMs >= PublicStaleMs) then
        begin
          if (FBroadcastMseLastStallTraceTick = 0) or
             ((NowTick - FBroadcastMseLastStallTraceTick) >= 3000) then
            begin
              FBroadcastMseLastStallTraceTick := NowTick;

              OutputDebugString(PChar(Format('TfrmMediaServer.MSE live trace: video fresh, public stale: videoAgeMs=%d publicAgeMs=%d groupParts=%d/%d groupBytes=%d sourceSeq=%d publicSeq=%d',
                                             [VideoAgeMs,
                                              PublicAgeMs,
                                              FBroadcastMseGroupPartCount,
                                              BroadcastMseGroupSourceFragments(),
                                              CurrentBroadcastMseGroupBytes(),
                                              FBroadcastMseFragmentSeq,
                                              FBroadcastMsePublicSeq])));

              FRdjProBroadcastMp4Recorder.DebugLogLiveState('dlg public stale');

              QueueBroadcastMseRecorderRestart(Format('public MSE output stale: videoAgeMs=%d publicAgeMs=%d groupParts=%d sourceSeq=%d publicSeq=%d',
                                                      [VideoAgeMs,
                                                       PublicAgeMs,
                                                       FBroadcastMseGroupPartCount,
                                                       FBroadcastMseFragmentSeq,
                                                       FBroadcastMsePublicSeq]));
            end;
        end;

      Exit;
    end;

  if not Assigned(FRdjProCaptureManager) then
    Exit;

  SinceLastFlushMs := High(UInt64);

  if (FLastBroadcastVideoFlushTick <> 0) then
    SinceLastFlushMs := NowTick - FLastBroadcastVideoFlushTick;

  if (SinceLastFlushMs < 15000) then
    Exit;

  if (TInterlocked.CompareExchange(FBroadcastVideoFlushQueued,
                                   1,
                                   0) <> 0) then
    Exit;

  FLastBroadcastVideoFlushTick := NowTick;

  OutputDebugString(PChar(Format('TfrmMediaServer.MSE video SourceReader hard flush queued: videoAgeMs=%d publicAgeMs=%d groupParts=%d sourceSeq=%d publicSeq=%d',
                                 [VideoAgeMs,
                                  PublicAgeMs,
                                  FBroadcastMseGroupPartCount,
                                  FBroadcastMseFragmentSeq,
                                  FBroadcastMsePublicSeq])));

  FlushProc := procedure
               var
                 hr: HRESULT;

               begin
                 try
                   if FRdjProBroadcasting and Assigned(FRdjProCaptureManager) then
                     begin
                       hr := FRdjProCaptureManager.FlushVideoSourceReaderHard(1500);

                       OutputDebugString(PChar(Format('TfrmMediaServer.MSE video SourceReader hard flush completed hr=0x%.8x',
                                                      [Cardinal(hr)])));

                       if SUCCEEDED(hr) then
                         FLastBroadcastVideoSampleTick := GetTickCount64()
                       else
                         if (VideoAgeMs >= 15000) then
                           begin
                             // With no video, the SinkWriter keeps unmatched audio
                             // internally even though our own queues remain empty.
                             // Roll over the recorder before that hidden buffer can
                             // exhaust the 32-bit process address space.
                             QueueBroadcastMseRecorderRestart(Format('Camera SourceReader recovery failed: hr=0x%.8x videoAgeMs=%d publicAgeMs=%d',
                                                                     [Cardinal(hr), VideoAgeMs, PublicAgeMs]));
                         end;
                     end;
                 finally
                   TInterlocked.Exchange(FBroadcastVideoFlushQueued, 0);
                 end;
               end;
  TThread.Queue(nil,
                FlushProc);
end;


function TfrmMediaServer.ArchiveExistingCaddyLiveMp4(const ALiveFileName: string): Boolean;
var
  ArchiveFileName: string;
  Dir: string;
  I: Integer;

begin

  if (Trim(ALiveFileName) = '') then
    Exit(False);

  if not FileExists(ALiveFileName) then
    Exit(True);

  Dir := IncludeTrailingPathDelimiter(ExtractFilePath(ALiveFileName));

  ArchiveFileName := Dir + 'live_previous_' + FormatDateTime('yyyymmdd_hhnnss_zzz',
                                                             Now) +
    '.mp4';

  // First try to archive the old live file. This keeps the Caddy URL stable
  // while avoiding an in-place truncate of a file that a browser/VLC may still
  // have open.
  for I := 0 to 20 do
    begin
      if RenameFile(ALiveFileName,
                    ArchiveFileName) then
        Exit(True);

      Sleep(25);
    end;

  // If the old file is locked by a reader, do not start writing over it.
  // Starting must fail clearly instead of producing a half-open live.mp4.
  Result := False;
end;


function TfrmMediaServer.EnsureRdjProVideoSampleReader(const APreferredMediaType: IMFMediaType): Boolean;
var
  hr: HRESULT;

begin

  Result := False;

  if not Assigned(FRdjProCaptureManager) then
    Exit;

  if not FRdjProCaptureInitialized then
    begin
      hr := StartRdjProCamera(pnlRdjProPreviewHost.Handle);
      if FAILED(hr) then
        Exit;
    end;

  FRdjProCaptureManager.OnVideoReaderSample := OnRdjProVideoReaderSample;

  hr := FRdjProCaptureManager.StartVideoSourceReader(APreferredMediaType);
  if FAILED(hr) then
    begin
      InfoMsg(optShowMsg,
              'StartVideoSourceReader failed. Enable camera sharing in Windows camera settings.',
              hr);

      memLog.Lines.Append('Camera sharing failed.');
      memLog.Lines.Append('Enable camera sharing in Windows camera settings.');
      Exit;
    end;

  if not Assigned(FRdjProCaptureManager.VideoSourceReaderMediaType) then
    Exit;

  Result := True;
end;


procedure TfrmMediaServer.StopRdjProVideoSampleReaderIfIdle();
begin

  if FRdjProRecording or FRdjProBroadcasting then
    Exit;

  if Assigned(FRdjProCaptureManager) then
    FRdjProCaptureManager.StopVideoSourceReader(100);
end;


function TfrmMediaServer.StartRdjProBroadcast(const APublishBroadcast: Boolean): Boolean;
var
  FileName: string;
  hr: HRESULT;
  VideoType: IMFMediaType;
  HandoverMessage: string;

begin

  Result := False;
  VideoType := nil;

  if not Assigned(FRdjProBroadcastMp4Recorder) then
    Exit;

  if FRdjProBroadcasting and FRdjProBroadcastMp4Recorder.Active then
    begin
      if APublishBroadcast and (not FBroadcastPublishing) then
        begin
          if not AcquireBroadcastHandoverLock(HandoverMessage) then
            begin
              memLog.Lines.Append(HandoverMessage);
              ShowMessage(HandoverMessage);
              Exit;
            end;

          CleanupBroadcastMseArtifactsOnStop();
          ResetBroadcastMseDebugDump();
          FBroadcastPublishing := True;
          chkBroadcast.Checked := True;
          chkBroadcast.Down := True;
          UpdateOnAirLamp(True);
          memLog.Lines.Append('Broadcasting started; the active encoder is now feeding browser/Caddy and Cast.');
          UpdateBroadcastHealth();
        end;

      Result := True;
      Exit;
    end;

  FBroadcastPublishing := APublishBroadcast;

  if APublishBroadcast and
     (not FBroadcastHandoverLockAcquired) and
     (not AcquireBroadcastHandoverLock(HandoverMessage)) then
    begin
      FBroadcastPublishing := False;
      memLog.Lines.Append(HandoverMessage);
      ShowMessage(HandoverMessage);
      Exit;
    end;

  try

  if FRdjProStaticImage then
    begin
      if not EnsureRdjProStaticVideoMediaType() then
        Exit;

      VideoType := FStaticVideoMediaType;
    end
  else
    begin
      if not EnsureRdjProVideoSampleReader() then
        Exit;

      VideoType := FRdjProCaptureManager.VideoSourceReaderMediaType;
    end;

  FileName := ResolveCaddyLiveMp4Path();

  if not ArchiveExistingCaddyLiveMp4(FileName) then
    Exit;

  hr := FRdjProBroadcastMp4Recorder.SetVideoPreviewMediaType(VideoType);
  if FAILED(hr) then
    begin
      InfoMsg(optShowMsg,
              'StartRdjProBroadcast: SetVideoPreviewMediaType failed.',
              hr);
      memLog.Lines.Append('Set video preview mediaType failed.');
      Exit;
    end;

  if not FRecordVideoOnly then
    begin
      if not FLastRdjProAudioWfxValid then
        begin
          ArmPendingBroadcastRecording(FileName,
                                       FRecordVideoOnly);
          FActiveBroadcastVideoMediaType := VideoType;
          FLastBroadcastAudioSampleTick := 0;
          FLastBroadcastVideoSampleTick := GetTickCount64();
          FLastBroadcastPublicSegmentTick := GetTickCount64();
          FRdjProBroadcasting := True;
          chkBroadcast.Checked := FBroadcastPublishing;
          chkBroadcast.Down := FBroadcastPublishing;
          chkRdjProCamera.Enabled := FRdjProStaticImage and
                                     (not FRecordingRdjPro);
          chkRecordVideoOnly.Enabled := False;
          chkRdjProStaticImage.Enabled := not FRecordingRdjPro;
          UpdateOnAirLamp(FBroadcastPublishing);
          UpdateBroadcastHealth();

          if Assigned(MainMDIFrm) then
            begin
              if MainMDIFrm.HasActiveLoopbackDeck() then
                MainMDIFrm.ClearNowPlaying();
              MainMDIFrm.RefreshMainButtonStates();
            end;

          Result := True;
          Exit;
        end;

      hr := FRdjProBroadcastMp4Recorder.SetAudioWaveFormat(@FLastRdjProAudioWfx);
      if FAILED(hr) then
        begin
          InfoMsg(optShowMsg,
                  'StartRdjProBroadcast: SetAudioWaveFormat failed.',
                  hr);
          memLog.Lines.Append('Set audio wave format failed.');
          Exit;
        end;
    end;

  hr := FRdjProBroadcastMp4Recorder.StartRecording(FileName,
                                                   FRecordVideoOnly);
  if FAILED(hr) then
    begin
      InfoMsg(optShowMsg,
              'StartRdjProBroadcast: StartRecording failed.',
              hr);
      memLog.Lines.Append('Start recording failed.');
      Exit;
    end;

  ResetBroadcastMseDebugDump();

  FRdjProBroadcasting := True;
  FActiveBroadcastVideoMediaType := VideoType;
  FLastBroadcastAudioSampleTick := 0;
  FLastBroadcastVideoSampleTick := GetTickCount64();
  FLastBroadcastPublicSegmentTick := GetTickCount64();

  if FRdjProStaticImage and not FRdjProRecording then
    begin
      FStaticVideoFrameIndex := 0;
      FStaticVideoStartTick := 0;
      FStaticVideoLastTick := 0;
    end;

  tmrTime.Enabled := True;
  FPendingBroadcastRecording := False;
  FPendingBroadcastFileName := '';
  chkBroadcast.Checked := FBroadcastPublishing;
  chkBroadcast.Down := FBroadcastPublishing;
  chkRdjProCamera.Enabled := FRdjProStaticImage and
                             (not FRecordingRdjPro);
  chkRecordVideoOnly.Enabled := False;
  chkRdjProStaticImage.Enabled := not FRecordingRdjPro;
  UpdateOnAirLamp(FBroadcastPublishing);

  if Assigned(MainMDIFrm) then
    begin
      if MainMDIFrm.HasActiveLoopbackDeck() then
        MainMDIFrm.ClearNowPlaying();
      MainMDIFrm.RefreshMainButtonStates();
    end;

  if FBroadcastPublishing then
    memLog.Lines.Append('Broadcasting started.')
  else
    memLog.Lines.Append('Live encoder started for Cast only.');
  UpdateBroadcastHealth();
  Result := True;

finally
  if (not Result) and
     APublishBroadcast and
     (not FRdjProBroadcasting) and
     (not FPendingBroadcastRecording) then
    begin
      FBroadcastPublishing := False;
      ReleaseBroadcastHandoverLock(False);
    end;
end;
end;


procedure TfrmMediaServer.StopRdjProBroadcast();
var
  Recorder: TRdjProFMp4Recorder;
  StopThread: TThread;
  WasPublishing: Boolean;

begin

  WasPublishing := FBroadcastPublishing;
  FBroadcastPublishing := False;

  if not Assigned(FRdjProBroadcastMp4Recorder) then
    begin
      if WasPublishing then
        ReleaseBroadcastHandoverLock(True);

      if Assigned(MainMDIFrm) then
        MainMDIFrm.RefreshMainButtonStates();
      Exit;
    end;

  if not FTimerRunning then
    tmrTime.Enabled := True;

  if not FCastProducerRequested then
    begin
      FPendingBroadcastRecording := False;
      FPendingBroadcastFileName := '';
      FActiveBroadcastVideoMediaType := nil;
    end;
  chkBroadcast.Checked := False;
  chkBroadcast.Down := False;
  chkRdjProCamera.Enabled := (not FCastProducerRequested) and
                             (not FRecordingRdjPro);
  chkRecordVideoOnly.Enabled := (not FCastProducerRequested) and
                                (not FRecordingRdjPro);
  chkRdjProStaticImage.Enabled := not FRecordingRdjPro;
  UpdateOnAirLamp(False);
  UpdateBroadcastHealth();

  if Assigned(MainMDIFrm) then
    MainMDIFrm.RefreshMainButtonStates();

  if WasPublishing then
    begin
      CleanupBroadcastMseArtifactsOnStop();
      ResetBroadcastMseDebugDump();
      ReleaseBroadcastHandoverLock(True);
      memLog.Lines.Append('Public broadcasting stopped.');
    end;

  // Cast still consumes the same encoder. Turning the public destination off
  // must therefore leave the producer and Chromecast session untouched.
  if FCastProducerRequested then
    begin
      memLog.Lines.Append('Live encoder remains active for Cast only.');
      Exit;
    end;

  // Neither destination needs the producer. Stop accepting samples now; the
  // recorder shutdown itself stays off the VCL thread.
  FRdjProBroadcasting := False;

  if not FRdjProBroadcastMp4Recorder.Active then
    begin
      if not WasPublishing then
        begin
          CleanupBroadcastMseArtifactsOnStop();
          ResetBroadcastMseDebugDump();
        end;
      StopRdjProVideoSampleReaderIfIdle();
      Exit;
    end;

  Recorder := FRdjProBroadcastMp4Recorder;

  StopThread := TThread.CreateAnonymousThread(procedure
                                              var
                                                hr: HRESULT;
                                                StopUiProc: TThreadProcedure;

                                              begin

                                                hr := Recorder.StopRecording();

                                                StopUiProc := procedure
                                                              begin
                                                                if FAILED(hr) then
                                                                  OutputDebugString(PChar('RDJ Pro broadcast MP4 StopRecording failed: ' + IntToStr(hr)))
                                                                else
                                                                  OutputDebugString(PChar('Broadcast MP4 writer stopped.'));

                                                                if not WasPublishing then
                                                                  begin
                                                                    CleanupBroadcastMseArtifactsOnStop();
                                                                    ResetBroadcastMseDebugDump();
                                                                  end;
                                                                StopRdjProVideoSampleReaderIfIdle();
                                                              end;

                                                TThread.Queue(nil,
                                                              StopUiProc);
                                              end);

  StopThread.FreeOnTerminate := True;
  StopThread.Start();
end;


procedure TfrmMediaServer.StopRdjProRecording();
var
  Recorder: TRdjProMp4Recorder;
  StopThread: TThread;

begin

  if not Assigned(FRdjProMp4Recorder) then
    Exit;

  // Stop the RDJ tap immediately. The actual SinkWriter/worker shutdown may
  // take a moment, so never do that heavy work on the VCL thread.
  FRdjProRecording := False;
  FPendingLocalRecording := False;
  FPendingLocalFileName := '';

  if not FRdjProMp4Recorder.Active then
    begin
      lblRecorderStatus.Caption := 'Recorder is ready.';
      StopRdjProVideoSampleReaderIfIdle();
      Exit;
    end;

  Recorder := FRdjProMp4Recorder;

  StopThread := TThread.CreateAnonymousThread(procedure
                                               var
                                                 hr: HRESULT;
                                                 StopUiProc: TThreadProcedure;

                                               begin

                                                 hr := Recorder.StopRecording();

                                                 StopUiProc := procedure
                                                               begin

                                                                 if FAILED(hr) then
                                                                   begin

                                                                     lblRecorderStatus.Caption := 'Recorder stop failed: ' + IntToStr(hr);
                                                                    OutputDebugString(PChar('RDJ Pro MP4 StopRecording failed: ' + IntToStr(hr)));
                                                                  end
                                                                else
                                                                  begin

                                                                     lblRecorderStatus.Caption := 'Recorder is ready.';
                                                                     StopRdjProVideoSampleReaderIfIdle();
                                                                   end;
                                                               end;
                                                 TThread.Queue(nil,
                                                               StopUiProc);
                                               end);

  StopThread.FreeOnTerminate := True;
  StopThread.Start();
end;

end.
