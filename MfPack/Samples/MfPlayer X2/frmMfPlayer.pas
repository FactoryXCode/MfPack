// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMfPlayer.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: MfPlayer X: Requires Windows 10 or later.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues, Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Parts of CPlayer Examples
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
unit frmMfPlayer;

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.ActiveX,
  WinApi.WinError,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Types,
  System.Classes,
  System.UITypes,
  {RTTI}
  System.Rtti,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  MfPlayerClassX,
  TimedTextClass,
  LangTags,
  MfPCXConstants,
  dlgMfCastDevices,
  MfCast,
  MfCastTypes,
  MfCastInterfaces,
  QueueTimer;

type
  // By default the form is the videowindow.

  Tfrm_MfPlayer = class(TForm)
    dlgOpenUrl: TOpenDialog;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuExportSubtitled: TMenuItem;
    muSeparator1: TMenuItem;
    mnuExit: TMenuItem;
    mnuExtra: TMenuItem;
    mnuSetPosition: TMenuItem;
    pnlVideo: TPanel;
    pnlControls: TPanel;
    butStop: TButton;
    butPause: TButton;
    butPlay: TButton;
    trbVolumeL: TTrackBar;
    prbProgress: TProgressBar;
    mnuTakeScreenshot: TMenuItem;
    butFullScreen: TButton;
    N1: TMenuItem;
    Rate1: TMenuItem;
    Ratep2: TMenuItem;
    Ratem1: TMenuItem;
    mnuSetRate: TMenuItem;
    trbVolumeR: TTrackBar;
    cbLockVolumeSliders: TCheckBox;
    mnuEnableSubtitling: TMenuItem;
    N2: TMenuItem;
    mnuSelectStreams: TMenuItem;
    N3: TMenuItem;
    mnuMediaInfo: TMenuItem;
    mnuSubTitling: TMenuItem;
    mnuLanguage: TMenuItem;
    N4: TMenuItem;
    mnuAspectRatio: TMenuItem;
    mnuCinema: TMenuItem;
    mnuSixteenByNine: TMenuItem;
    mnuFourByThree: TMenuItem;
    QTimer1: TQTimer;
    dlgSaveSubtitled: TSaveDialog;
    mnuCast: TMenuItem;
    mnuStopCasting: TMenuItem;
    mnuCastTo: TMenuItem;
    mnuPauseCasting: TMenuItem;
    mnuResumeCasting: TMenuItem;
    stbCastStatus: TStatusBar;
    N6: TMenuItem;
    lblBarPositionInSTime: TLabel;

    procedure mnuOpenClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSetPositionClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure mnuTakeScreenshotClick(Sender: TObject);
    procedure butFullScreenClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Rate1Click(Sender: TObject);
    procedure Ratep2Click(Sender: TObject);
    procedure Ratem1Click(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure prbProgressMouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
    procedure mnuEnableSubtitlingClick(Sender: TObject);
    procedure mnuSelectStreamsClick(Sender: TObject);
    procedure mnuMediaInfoClick(Sender: TObject);
    procedure mnuLanguageClick(Sender: TObject);
    procedure mnuCinemaClick(Sender: TObject);
    procedure mnuSixteenByNineClick(Sender: TObject);
    procedure mnuFourByThreeClick(Sender: TObject);
    procedure mnuExportSubtitledClick(Sender: TObject);
    procedure mnuCastToClick(Sender: TObject);
    procedure mnuStopCastingClick(Sender: TObject);
    procedure mnuPauseCastingClick(Sender: TObject);
    procedure mnuResumeCastingClick(Sender: TObject);
    procedure prbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure prbProgressMouseLeave(Sender: TObject);
    procedure prbProgressMouseEnter(Sender: TObject);

  private
    { Private declarations }
    bAppIsClosing: Boolean;
    pb_IsFullScreen: Boolean;
    ps_AspectRatio: Single;
    sMediaFileName: WideString;
    FExportThread: TThread;
    FExportClosePending: Boolean;
    FExportStopPending: Boolean;
    FSubtitleLoadThread: TThread;
    FSubtitleLoadSerial: Integer;
    FSubtitleLoadPending: Boolean;
    FPendingSubtitleLoadEmbedded: Boolean;
    FPendingSubtitleLoadStreamIndex: DWORD;
    FPendingSubtitleLoadLanguageTag: string;
    FMfCast: TMfCast;
    FCastWorker: TThread;
    FCastStartPosition100ns: Int64;
    FCastSyncPending: Boolean;
    FCastLocalWasRunning: Boolean;
    FCastSessionActive: Boolean;
    FCastConnectivityTimer: TTimer;
    FCastBufferingStartedTick: Cardinal;
    FCastConnectivityWarningShown: Boolean;

    { Private methods }
    // Size and move handlers.
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    // Forces a resize to set aspect ratio.
    procedure ForceResize();
    // Progress handling.
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
    procedure WMSubtitleLoadEvent(var Msg: TMessage); message WM_SUBTITLELOADNOTIFY;

    procedure ExportThreadTerminated(Sender: TObject);
    procedure StartSubtitledExport(const OutputFileName: WideString);
    procedure StopSubtitleLoad();
    function StartEmbeddedSubtitleLoad(const StreamIndex: DWORD): HRESULT;
    function StartSidecarSubtitleLoad(const LanguageTag: string): HRESULT;
    procedure StartPendingSubtitleLoad();
    procedure StartPreferredEmbeddedSubtitleLoad();
    procedure StopPlaybackNow();
    procedure SetVolumeChannels(volchans: TFloatArray);
    function MfCastIsActive(): Boolean;
    procedure SetPlaybackButtonsEnabled(const AEnabled: Boolean);
    procedure RestoreLocalVideoForCasting();
    procedure StartLocalPlaybackForCasting();
    procedure CancelPendingCastSynchronization(const ResumeLocalPlayback: Boolean);
    procedure StopPlaybackAfterCastEnded();
    procedure SynchronizeLocalPlaybackToCast(const CastPosition100ns: MFTIME);
    procedure SetCastStatusText(const AText: string);
    procedure ClearCastStatusText();
    procedure UpdateCastControls();
    procedure CompletePlayback();
    procedure PauseCasting();
    procedure ResumeCasting();
    procedure MfCastStateChanged(const AOldState: TMfCastState;
                                 const ANewState: TMfCastState);
    procedure MfCastMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure MfCastError(const AError: TMfCastErrorInfo);
    procedure ApplyMfCastState(const ANewState: TMfCastState);
    procedure ApplyMfCastMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure ApplyMfCastError(const AError: TMfCastErrorInfo);
    procedure WmMfCastState(var Msg: TMessage); message WM_APP + 450;
    procedure WmMfCastStatus(var Msg: TMessage); message WM_APP + 451;
    procedure WmMfCastError(var Msg: TMessage); message WM_APP + 452;
    procedure WmMfCastFinished(var Msg: TMessage); message WM_APP + 453;
    procedure CastConnectivityTimer(Sender: TObject);
    procedure ResetCastConnectivityWatchdog();

  public
    { Public declarations }

    { Public methods }
    procedure SetWindowStyle(bFullScreen: Boolean);
    procedure ResetInterface();
    //procedure SetToParentRect();
    procedure RealignInterface();
    function GetFmPlayer(): HRESULT;
    function SelectEmbeddedSubtitleTrackAsync(const StreamIndex: DWORD): HRESULT;
    function SelectSidecarSubtitleLanguageAsync(const LanguageTag: string): HRESULT;
    function SubtitleLoadPending(): Boolean;
    procedure QuitMfPlayerSession();
  end;

var
  frm_MfPlayer: Tfrm_MfPlayer;


implementation

uses

  {Vcl}
  Vcl.ClipBrd,
  {Application}
  MfSubtitleCompositor,
  MfSubtitleFramePump,
  dlgStreamSelect,
  dlgSelectTimedTextLanguages,
  {Cast API}
  MfCastWindowsSupport;

{$R *.dfm}


function MfCastStartFailureText(const ADeviceName: string;
                                const AHResult: HRESULT): string;
begin

  case DWORD(AHResult) of
    $8007274C:
      Result := 'Could not start casting to ' + ADeviceName +
                '. The device did not answer. It may be offline or asleep. ' +
                'HRESULT $' + IntToHex(DWORD(AHResult), 8);
    $8007274D:
      Result := 'Could not start casting to ' + ADeviceName +
                '. The device refused the Cast connection. HRESULT $' +
                IntToHex(DWORD(AHResult), 8);
    $80072745,
    $80072746:
      Result := 'Could not start casting to ' + ADeviceName +
                '. The Cast connection was closed by the device. HRESULT $' +
                IntToHex(DWORD(AHResult), 8);
  else
    Result := 'Could not start casting to ' + ADeviceName +
              '. HRESULT $' + IntToHex(DWORD(AHResult), 8);
  end;
end;


type

  TMfPlayerCastStatusMessage = class
  public
    Status: TMfCastMediaStatus;
  end;

  TMfPlayerCastErrorMessage = class
  public
    ErrorInfo: TMfCastErrorInfo;
  end;

  TMfPlayerCastWorker = class(TThread)
  private
    FCast: TMfCast;
    FDevice: TMfCastDevice;
    FSource: string;
    FSubtitle: TMfCastSubtitleAsset;
    FMediaMode: TMfCastMediaMode;
    FSubtitleMode: TMfCastSubtitleMode;
    FStartSeconds: Double;
    FNotifyHandle: HWND;
  protected
    procedure Execute(); override;
  public
    constructor Create(const ACast: TMfCast;
                       const ADevice: TMfCastDevice;
                       const ASource: string;
                       const ASubtitle: TMfCastSubtitleAsset;
                       const AMediaMode: TMfCastMediaMode;
                       const ASubtitleMode: TMfCastSubtitleMode;
                       const AStartSeconds: Double;
                       const ANotifyHandle: HWND);
  end;

  TMfSubtitleLoadKind = (slkEmbedded,
                         slkSidecar);

  TMfSubtitleLoadThread = class(TThread)
  private
    FCompositor: TMfSubtitleCompositor;
    FKind: TMfSubtitleLoadKind;
    FStreamIndex: DWORD;
    FLanguageTag: string;
    FMediaFileName: WideString;
    FNotifyHandle: HWND;
    FSerial: Integer;
    FCancelEvent: THandle;
    FResult: HRESULT;

  protected
    procedure Execute(); override;

  public

    constructor Create(const Compositor: TMfSubtitleCompositor;
                       const Kind: TMfSubtitleLoadKind;
                       const StreamIndex: DWORD;
                       const LanguageTag: string;
                       const MediaFileName: WideString;
                       const NotifyHandle: HWND;
                       const Serial: Integer);
    destructor Destroy(); override;

    procedure CancelLoad();

    property Kind: TMfSubtitleLoadKind read FKind;
    property MediaFileName: WideString read FMediaFileName;
    property ResultCode: HRESULT read FResult;
    property Serial: Integer read FSerial;
  end;


  TMfSubtitleExportThread = class(TThread)
  private
    FInputFileName: WideString;
    FOutputFileName: WideString;
    FPreferredLanguage: string;
    FSubtitleAspectRatio: Single;
    FResult: HRESULT;
    FErrorMessage: string;
    FFramesWritten: Int64;
    FPump: TMfSubtitleFramePump;
    FNotifyHandle: HWND;
    FDuration100ns: MFTIME;
    FLastProgress: Integer;
    procedure PumpProgress(Sender: TObject;
                           FramesWritten: Int64;
                           SampleTime: MFTIME;
                           var Cancel: Boolean);
  protected
    procedure Execute(); override;

  public

    constructor Create(const InputFileName: WideString;
                       const OutputFileName: WideString;
                       const PreferredLanguage: string;
                       SubtitleAspectRatio: Single;
                       NotifyHandle: HWND;
                       Duration100ns: MFTIME);

    procedure CancelExport();

    property ErrorMessage: string read FErrorMessage;
    property FramesWritten: Int64 read FFramesWritten;
    property ResultCode: HRESULT read FResult;
  end;


constructor TMfPlayerCastWorker.Create(const ACast: TMfCast;
                                       const ADevice: TMfCastDevice;
                                       const ASource: string;
                                       const ASubtitle: TMfCastSubtitleAsset;
                                       const AMediaMode: TMfCastMediaMode;
                                       const ASubtitleMode: TMfCastSubtitleMode;
                                       const AStartSeconds: Double;
                                       const ANotifyHandle: HWND);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCast := ACast;
  FDevice := ADevice;
  FSource := ASource;
  FSubtitle := ASubtitle;
  FMediaMode := AMediaMode;
  FSubtitleMode := ASubtitleMode;
  FStartSeconds := AStartSeconds;
  FNotifyHandle := ANotifyHandle;
end;


procedure TMfPlayerCastWorker.Execute();
var
  hr: HRESULT;
  hrCom: HRESULT;
  comInitialized: Boolean;
begin
  hr := E_FAIL;
  comInitialized := False;
  hrCom := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if SUCCEEDED(hrCom) then
    comInitialized := True;
  try
    if SUCCEEDED(hrCom) or (hrCom = RPC_E_CHANGED_MODE) then
      hr := FCast.Cast(FDevice, FSource, FSubtitle, FMediaMode,
                       FSubtitleMode, FStartSeconds)
    else
      hr := hrCom;
  finally
    if comInitialized then
      CoUninitialize();
    PostMessage(FNotifyHandle, WM_APP + 453, WPARAM(hr), 0);
  end;
end;


constructor TMfSubtitleLoadThread.Create(const Compositor: TMfSubtitleCompositor;
                                         const Kind: TMfSubtitleLoadKind;
                                         const StreamIndex: DWORD;
                                         const LanguageTag: string;
                                         const MediaFileName: WideString;
                                         const NotifyHandle: HWND;
                                         const Serial: Integer);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  Priority := tpLower;
  FCompositor := Compositor;
  FKind := Kind;
  FStreamIndex := StreamIndex;
  FLanguageTag := LanguageTag;
  FMediaFileName := MediaFileName;
  FNotifyHandle := NotifyHandle;
  FSerial := Serial;
  FResult := E_FAIL;
  FCancelEvent := CreateEvent(nil,
                              True,
                              False,
                              nil);
end;


destructor TMfSubtitleLoadThread.Destroy();
begin

  if (FCancelEvent <> 0) then
    begin
      CloseHandle(FCancelEvent);
      FCancelEvent := 0;
    end;

  inherited Destroy();
end;


procedure TMfSubtitleLoadThread.CancelLoad();
begin

  Terminate();

  if (FCancelEvent <> 0) then
    SetEvent(FCancelEvent);
end;


procedure TMfSubtitleLoadThread.Execute();
var
  HrCom: HRESULT;
  ComInitialized: Boolean;

begin

  ComInitialized := False;
  HrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);

  if SUCCEEDED(HrCom) then
    ComInitialized := True
  else
    if (HrCom <> RPC_E_CHANGED_MODE) then
      begin
        FResult := HrCom;
        PostMessage(FNotifyHandle,
                    WM_SUBTITLELOADNOTIFY,
                    WPARAM(FSerial),
                    0);
        Exit;
      end;

  try
    if Terminated or
       ((FCancelEvent <> 0) and
        (WaitForSingleObject(FCancelEvent,
                             0) = WAIT_OBJECT_0)) then
      FResult := E_ABORT
    else
      if not Assigned(FCompositor) then
        FResult := E_POINTER
      else
        case FKind of
          slkEmbedded: FResult := FCompositor.SelectEmbeddedSubtitleTrack(FStreamIndex,
                                                                          FCancelEvent);
          slkSidecar:  FResult := FCompositor.SelectSidecarSubtitleLanguage(FLanguageTag);
        else
          FResult := E_INVALIDARG;
        end;
  finally
    if ComInitialized then
      CoUninitialize();
  end;

  PostMessage(FNotifyHandle,
              WM_SUBTITLELOADNOTIFY,
              WPARAM(FSerial),
              0);
end;


constructor TMfSubtitleExportThread.Create(const InputFileName: WideString;
                                            const OutputFileName: WideString;
                                            const PreferredLanguage: string;
                                            SubtitleAspectRatio: Single;
                                            NotifyHandle: HWND;
                                            Duration100ns: MFTIME);
begin

  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpLower;
  FInputFileName := InputFileName;
  FOutputFileName := OutputFileName;
  FPreferredLanguage := PreferredLanguage;
  FSubtitleAspectRatio := SubtitleAspectRatio;
  FNotifyHandle := NotifyHandle;
  FDuration100ns := Duration100ns;
  FLastProgress := -1;
  FResult := E_FAIL;
  FFramesWritten := 0;
  FPump := nil;
end;


procedure TMfSubtitleExportThread.CancelExport();
begin

  Terminate();

  if Assigned(FPump) then
    FPump.Cancel();
end;


procedure TMfSubtitleExportThread.PumpProgress(Sender: TObject;
                                              FramesWritten: Int64;
                                              SampleTime: MFTIME;
var Cancel: Boolean);
var
  progress: Integer;

begin

  Cancel := Terminated;
  if Cancel or (FNotifyHandle = 0) or (FDuration100ns <= 0) then
    Exit;

  progress := Integer((SampleTime * 100) div FDuration100ns);
  if progress < 0 then
    progress := 0
  else
    if progress > 100 then
      progress := 100;

  if progress <> FLastProgress then
    begin
      FLastProgress := progress;
      PostMessage(FNotifyHandle,
                  WM_PROGRESSNOTIFY,
                  WPARAM(3),
                  LPARAM(progress));
    end;
end;


procedure TMfSubtitleExportThread.Execute();
var
  hrCom: HRESULT;
  comInitialized: Boolean;
  compositor: TMfSubtitleCompositor;
  pump: TMfSubtitleFramePump;

begin

  comInitialized := False;
  compositor := nil;
  pump := nil;

  hrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);
  if SUCCEEDED(hrCom) then
    comInitialized := True
  else
    if (hrCom <> RPC_E_CHANGED_MODE) then
      begin

        FResult := hrCom;
        Exit;
     end;

  try
    try

      compositor := TMfSubtitleCompositor.Create();
      compositor.SubtitleAspectRatio := FSubtitleAspectRatio;
      if SameText(ExtractFileExt(FInputFileName), '.mp4') then
        compositor.SubtitleFontScale := 1.5;
      FResult := compositor.OpenTimedTextFile(FInputFileName,
                                              FPreferredLanguage);
      if FAILED(FResult) then
        Exit;

      pump := TMfSubtitleFramePump.Create(compositor);
      FPump := pump;
      pump.OnProgress := PumpProgress;
      FResult := pump.BurnSubtitlesToFile(FInputFileName,
                                          FOutputFileName);
      FFramesWritten := pump.FramesWritten;

    except
      on E: Exception do
        begin
          FResult := E_FAIL;
          FErrorMessage := E.Message;
        end;
    end;

  finally

    FPump := nil;
    FreeAndNil(pump);
    FreeAndNil(compositor);
    if comInitialized then
      CoUninitialize();
  end;
end;


procedure Tfrm_MfPlayer.StartSubtitledExport(const OutputFileName: WideString);
begin

  if Assigned(FExportThread) then
    Exit;

  if (not Assigned(MfPlayerX)) or (sMediaFileName = '') then
    Exit;

  FExportClosePending := False;
  FExportStopPending := False;
  FExportThread := TMfSubtitleExportThread.Create(sMediaFileName,
                                                  OutputFileName,
                                                  MfPlayerX.SubtitleLanguage,
                                                  MfPlayerX.SubtitleAspectRatio,
                                                  Handle,
                                                  MfPlayerX.Duration);

  FExportThread.OnTerminate := ExportThreadTerminated;
  mnuExportSubtitled.Enabled := False;
  mnuExportSubtitled.Caption := 'Exporting subtitled MP4...';
  prbProgress.Position := prbProgress.Min;
  butPlay.Enabled := False;
  butPause.Enabled := False;
  FExportThread.Start();
end;


procedure Tfrm_MfPlayer.mnuExportSubtitledClick(Sender: TObject);
var
  outputName: string;

begin

  if Assigned(FExportThread) then
    begin
      ShowMessage('A subtitled export is already running.');
      Exit;
    end;

  if (not Assigned(MfPlayerX)) or (sMediaFileName = '') then
    Exit;

  if not MfPlayerX.TimedTextFileLoaded then
    begin
      ShowMessage('No TimedText file is loaded.');
      Exit;
    end;

  if not (MfPlayerX.State in [Stopped, TopologyReady]) then
    begin
      ShowMessage('Stop playback before starting a subtitled MP4 export.');
      Exit;
    end;

  outputName := ChangeFileExt(ExtractFileName(sMediaFileName), '.subtitled.mp4');
  dlgSaveSubtitled.FileName := outputName;
  if not dlgSaveSubtitled.Execute() then
    Exit;

  StartSubtitledExport(dlgSaveSubtitled.FileName);
end;


procedure Tfrm_MfPlayer.ExportThreadTerminated(Sender: TObject);
var
  exportThread: TMfSubtitleExportThread;
  hr: HRESULT;
  msg: string;
  closePending: Boolean;
  stopPending: Boolean;

begin

  OutputDebugString(PChar('MfPlayer X2: export terminate handler begin'));
  exportThread := Sender as TMfSubtitleExportThread;
  hr := exportThread.ResultCode;
  closePending := FExportClosePending;
  stopPending := FExportStopPending;
  FExportClosePending := False;
  FExportStopPending := False;

  mnuExportSubtitled.Caption := 'Export subtitled MP4...';
  mnuExportSubtitled.Enabled := Assigned(MfPlayerX) and MfPlayerX.TimedTextFileLoaded;
  butPlay.Enabled := Assigned(MfPlayerX);
  butPause.Enabled := Assigned(MfPlayerX);
  butStop.Enabled := True;

  if FAILED(hr) then
    System.SysUtils.DeleteFile(string(exportThread.FOutputFileName));

  if (not closePending) and (not stopPending) then
    begin
      if (hr = E_ABORT) then
        ShowMessage('Subtitled video export stopped before frames were written.')
      else
        if FAILED(hr) then
          begin
            msg := Format('Could not export subtitled video. HRESULT: %.8x',
                          [DWORD(hr)]);
            if (exportThread.ErrorMessage <> '') then
              msg := msg + sLineBreak + exportThread.ErrorMessage;
            ShowMessage(msg);
          end
        else
          ShowMessage(Format('Subtitled video exported. Frames written: %d',
                             [exportThread.FramesWritten]));
    end;

  FExportThread := nil;

  if closePending then
    Close()
  else
    if stopPending then
      StopPlaybackNow();

  OutputDebugString(PChar('MfPlayer X2: export terminate handler end'));
end;


procedure Tfrm_MfPlayer.mnuExitClick(Sender: TObject);
begin

  Close();
end;


procedure Tfrm_MfPlayer.mnuTakeScreenshotClick(Sender: TObject);
var
  bm: TBitmap;

begin

  bm:= TBitmap.Create;
  MfPlayerX.TakeSnapShot(bm);
  Clipboard.Assign(bm);
  FreeAndNil(bm);
end;



function Tfrm_MfPlayer.MfCastIsActive(): Boolean;
begin

  Result := Assigned(FMfCast) and (FMfCast.State() in [csConnecting,
                                                                              csConnected,
                                                                              csLaunchingReceiver,
                                                                              csPreparingMedia,
                                                                              csBuffering,
                                                                              csPlaying,
                                                                              csPaused,
                                                                              csStopping]);
end;


procedure Tfrm_MfPlayer.SetPlaybackButtonsEnabled(const AEnabled: Boolean);
begin

  butPlay.Enabled := AEnabled;
  butPause.Enabled := AEnabled;
  butStop.Enabled := AEnabled;
end;


procedure Tfrm_MfPlayer.RestoreLocalVideoForCasting();
begin

  if not Assigned(MfPlayerX) then
    Exit;

  MfPlayerX.SetVideoSurface := pnlVideo.Handle;
  MfPlayerX.ResizeVideo(nil);
  MfPlayerX.Repaint();
  pnlVideo.Invalidate();
end;


procedure Tfrm_MfPlayer.StartLocalPlaybackForCasting();
begin

  if not Assigned(MfPlayerX) then
    Exit;

  if MfPlayerX.State in [Paused,
                         OpenPending,
                         Stopped,
                         TopologyReady] then
    begin
      MfPlayerX.SendPlayerRequest(reqStart);
      mnuTakeScreenshot.Enabled := True;
    end;

  RestoreLocalVideoForCasting();
end;


procedure Tfrm_MfPlayer.CancelPendingCastSynchronization(const ResumeLocalPlayback: Boolean);
begin

  if not FCastSyncPending then
    Exit;

  FCastSyncPending := False;

  if ResumeLocalPlayback and FCastLocalWasRunning then
    StartLocalPlaybackForCasting();

  FCastLocalWasRunning := False;
end;


procedure Tfrm_MfPlayer.StopPlaybackAfterCastEnded();
begin

  FCastSessionActive := False;
  CancelPendingCastSynchronization(False);
  StopPlaybackNow();
  ResetInterface();
  SetCastStatusText('ChromeCast: Stopped');
  UpdateCastControls();
  Caption := 'Casting stopped.';
end;


procedure Tfrm_MfPlayer.SynchronizeLocalPlaybackToCast(const CastPosition100ns: MFTIME);
const
  CAST_SYNC_TOLERANCE_100NS = 5000000; // 500 ms

var

  PositionHr: HRESULT;
  SeekHr: HRESULT;
  SyncPosition100ns: MFTIME;
  LocalPosition100ns: MFTIME;
  PositionDifference100ns: MFTIME;

begin

  if not FCastSyncPending then
    Exit;

  FCastSyncPending := False;

  if not FCastLocalWasRunning or
     not Assigned(MfPlayerX) then
    begin
      FCastLocalWasRunning := False;
      Exit;
    end;

  SyncPosition100ns := CastPosition100ns;
  if (SyncPosition100ns <= 0) then
    SyncPosition100ns := FCastStartPosition100ns;
  if (SyncPosition100ns < 0) then
    SyncPosition100ns := 0;

  LocalPosition100ns := 0;
  PositionHr := MfPlayerX.GetPosition(LocalPosition100ns);

  if SUCCEEDED(PositionHr) then
    begin
      if LocalPosition100ns >= SyncPosition100ns then
        PositionDifference100ns := LocalPosition100ns - SyncPosition100ns
      else
        PositionDifference100ns := SyncPosition100ns - LocalPosition100ns;

      if PositionDifference100ns <= CAST_SYNC_TOLERANCE_100NS then
        begin
          RestoreLocalVideoForCasting();
          mnuTakeScreenshot.Enabled := True;
          FCastLocalWasRunning := False;
          Exit;
        end;
    end;

  // SendPlayerRequest(reqSeek) changes the player state to Seeking before it
  // knows whether IMFMediaSession.Start accepted the new position. Use the
  // HRESULT-returning method so a rejected seek cannot leave the EVR black.
  SeekHr := MfPlayerX.SetPosition(SyncPosition100ns);
  if FAILED(SeekHr) then
    begin
      OutputDebugString(PChar(Format('ChromeCast local synchronization seek failed, hr=%.8x',
                                     [DWORD(SeekHr)])));
      StartLocalPlaybackForCasting();
    end
  else
    RestoreLocalVideoForCasting();

  mnuTakeScreenshot.Enabled := True;
  FCastLocalWasRunning := False;
end;


procedure Tfrm_MfPlayer.SetCastStatusText(const AText: string);
begin

  if Assigned(stbCastStatus) then
    begin

      stbCastStatus.SimpleText := AText;
      stbCastStatus.Visible := (Trim(AText) <> '');
    end;

end;


procedure Tfrm_MfPlayer.ClearCastStatusText();
begin

  if Assigned(stbCastStatus) then
    begin
      stbCastStatus.SimpleText := '';
      stbCastStatus.Visible := False;
    end;
end;


procedure Tfrm_MfPlayer.UpdateCastControls();
var
  CastState: TMfCastState;
  CastActive: Boolean;

begin

  if Assigned(FMfCast) then
    CastState := FMfCast.State()
  else
    CastState := csIdle;

  CastActive := CastState in [csConnecting,
                              csConnected,
                              csLaunchingReceiver,
                              csPreparingMedia,
                              csBuffering,
                              csPlaying,
                              csPaused,
                              csStopping];

  mnuCastTo.Enabled := Assigned(FMfCast) and
                       not CastActive;

  if CastActive then
    begin
      mnuStopCasting.Enabled := True;
      mnuPauseCasting.Enabled := CastState in [csBuffering,
                                               csPlaying];
      mnuResumeCasting.Enabled := CastState = csPaused;

      butStop.Enabled := False;
      butPause.Enabled := mnuPauseCasting.Enabled;
      butPlay.Enabled := mnuResumeCasting.Enabled;
      Exit;
    end;

  mnuStopCasting.Enabled := False;
  mnuPauseCasting.Enabled := False;
  mnuResumeCasting.Enabled := False;

  if Assigned(FExportThread) then
    begin
      butPlay.Enabled := False;
      butPause.Enabled := False;
      butStop.Enabled := True;
    end
  else
    SetPlaybackButtonsEnabled(Assigned(MfPlayerX));
end;


procedure Tfrm_MfPlayer.CompletePlayback();
begin

  FCastSessionActive := False;
  CancelPendingCastSynchronization(False);

  if MfCastIsActive() then
    FMfCast.Disconnect();

  if Assigned(MfPlayerX) and
     (not (MfPlayerX.State in [Closed, Stopped, Stopping])) then
    MfPlayerX.SendPlayerRequest(reqStop);

  ClearCastStatusText();
  UpdateCastControls();
  ResetInterface();
  Caption := 'Playback completed.';
end;


procedure Tfrm_MfPlayer.PauseCasting();
var
  hr: HRESULT;

begin

  if (not Assigned(FMfCast)) or
     (not (FMfCast.State() in [csBuffering, csPlaying])) then
    Exit;

  hr := FMfCast.Pause();
  if SUCCEEDED(hr) then
    begin
      if Assigned(MfPlayerX) and
         (MfPlayerX.State in [OpenPending, Started, TopologyReady]) then
        MfPlayerX.SendPlayerRequest(reqPause);

      SetCastStatusText('ChromeCast: Paused');
      UpdateCastControls();
    end
  else
    SetCastStatusText('Could not pause ChromeCast. HRESULT $' +
                      IntToHex(DWORD(hr), 8));
end;


procedure Tfrm_MfPlayer.ResumeCasting();
var
  hr: HRESULT;

begin

  if (not Assigned(FMfCast)) or
     (FMfCast.State() <> csPaused) then
    Exit;

  hr := FMfCast.Play();
  if SUCCEEDED(hr) then
    begin
      StartLocalPlaybackForCasting();
      SetCastStatusText('ChromeCast: Buffering');
      UpdateCastControls();
    end
  else
    SetCastStatusText('Could not resume ChromeCast. HRESULT $' +
                      IntToHex(DWORD(hr), 8));
end;


procedure Tfrm_MfPlayer.butPauseClick(Sender: TObject);
begin

  if Assigned(FExportThread) then
    Exit;

  if MfCastIsActive() then
    begin
      PauseCasting();
      Exit;
    end;

  if Assigned(MfPlayerX) then
    if MfPlayerX.State in [OpenPending, Started, TopologyReady] then
      MfPlayerX.SendPlayerRequest(reqPause);
end;


procedure Tfrm_MfPlayer.butPlayClick(Sender: TObject);
begin

  if Assigned(FExportThread) then
    Exit;

  if MfCastIsActive() then
    begin
      ResumeCasting();
      Exit;
    end;

  // Continue where we left when state is paused
  if Assigned(MfPlayerX) then
    begin

      if MfPlayerX.State in [Paused] then
        MfPlayerX.SendPlayerRequest(reqStart);

      // Start a new session
      if MfPlayerX.State in [OpenPending, Stopped, TopologyReady] then
        begin
          // Set initial volume
          //MfPlayerX.GetVolume();

          //if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
          //  trbVolumeR.Position := Trunc(MfPlayerX.m_VolumeChannels[0] * 30); // left  30%
          //if (Length(MfPlayerX.m_VolumeChannels) > 1) then
          //  trbVolumeL.Position := Trunc(MfPlayerX.m_VolumeChannels[1] * 30); // right 30%
          // or use the current position of the sliders
          SetVolumeChannels(MfPlayerX.m_VolumeChannels);

          // Set progressbar max
          prbProgress.Max := prbProgress.Width;

          // Start session
          MfPlayerX.SendPlayerRequest(reqStart);
          // Enable screenshot menu
          mnuTakeScreenshot.Enabled := True;

          // If we want to implement the MFPeakmeters..
          // Activate the peaklevel meters
          //MfPeakMeter1.Enabled:= True;
          //MfPeakMeter2.Esnabled:= True;
        end;
    end;
end;


procedure Tfrm_MfPlayer.StopPlaybackNow();
begin

  if Assigned(MfPlayerX) then
    begin

      if MfPlayerX.State in [Started, Starting, Pausing, Paused, Stopping, Seeking, SeekingReady] then
        begin
          OutputDebugString(PChar('MfPlayer X2: stopping playback'));
          MfPlayerX.SendPlayerRequest(reqStop);
        end;

      mnuTakeScreenshot.Enabled := False;
      prbProgress.Position := 0;
    end;
end;


procedure Tfrm_MfPlayer.butStopClick(Sender: TObject);
begin

  if MfCastIsActive() then
    Exit;

  if Assigned(FExportThread) then
    begin
      FExportStopPending := True;
      FExportClosePending := False;
      OutputDebugString(PChar('MfPlayer X2: stop requested while export is active'));
      (FExportThread as TMfSubtitleExportThread).CancelExport();
      butStop.Enabled := False;
      mnuExportSubtitled.Caption := 'Stopping subtitled MP4...';
      mnuExportSubtitled.Enabled := False;
      Exit;
    end;

  StopPlaybackNow();
end;


procedure Tfrm_MfPlayer.SetWindowStyle(bFullScreen: Boolean);
begin

  if (bFullScreen = True) then
    begin
      BorderStyle := bsNone;
      pnlControls.Visible := False;
      mnuFile.Visible := False;
      mnuExtra.Visible := False;
      WindowState := wsMaximized;
      pb_IsFullScreen := True;
    end
  else
    begin
      pb_IsFullScreen := False;
      pnlControls.Visible := True;
      mnuFile.Visible := True;
      mnuExtra.Visible := True;
      WindowState := wsNormal;
      BorderStyle := bsSizeable;
    end;
end;


procedure Tfrm_MfPlayer.ResetInterface();
begin

  prbProgress.Position := 0;
  mnuSetPosition.Enabled := False;
  pnlControls.Enabled := False;
  SetPlaybackButtonsEnabled(False);
  mnuTakeScreenshot.Enabled := False;
  mnuExportSubtitled.Enabled := False;
  mnuSubtitling.Enabled := False;
  mnuLanguage.Enabled := False;
  mnuSelectStreams.Enabled := False;
  mnuMediaInfo.Enabled := False;

  // Stop MFPeakmeters
  //MfPeakMeter1.Enabled := False;
  //MfPeakMeter2.Enabled := False;
end;


// show some audio info
procedure Tfrm_MfPlayer.mnuMediaInfoClick(Sender: TObject);
var
  i: Integer;
  lst: string;

begin


  for i := 0 to High(MfPlayerX.StreamContents) do
    begin

      if (MfPlayerX.StreamContents[i].idStreamMediaType = mtVideo) then
        begin
          lst := 'Video Info' + LFEED + LFEED;
          lst := lst + 'Pixel aspect ratio: ' + FloatToStrF(MfPlayerX.StreamContents[i].video_PixelAspectRatioNumerator / MfPlayerX.StreamContents[i].video_PixelAspectRatioDenominator, ffGeneral, 4, 2) + LFEED;
          lst := lst + 'Framerate per second (fps): ' + FloatToStrF(MfPlayerX.StreamContents[i].video_FrameRateNumerator / MfPlayerX.StreamContents[i].video_FrameRateDenominator, ffGeneral, 4, 2) + LFEED;
          lst := lst + 'Framesize (w x h): ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeWidth) + ' x ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeHeigth) + LFEED + LFEED;
        end
      else
        if (MfPlayerX.StreamContents[i].idStreamMediaType = mtAudio) then
          begin
            lst := lst + 'Audio Info' + LFEED + LFEED;
            lst := lst + 'Format        : ' + MfPlayerX.StreamContents[i].audio_wsAudioDescr + LFEED;
            lst := lst + 'Channels      : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iAudioChannels) + LFEED;
            lst := lst + 'FormatTag     : ' + IntToStr(MfPlayerX.StreamContents[i].audio_dwFormatTag) + LFEED;
            lst := lst + 'SamplesPerSec : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iSamplesPerSec) + LFEED;
            lst := lst + 'BitsPerSample : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iBitsPerSample) + LFEED + LFEED;
            lst := lst + 'Compressed    : ' + BoolToStr(MfPlayerX.StreamContents[i].bCompressed) + LFEED;
          end;
    end;
  ShowMessage(lst);
end;


// A different but effective way to play in full screen mode
procedure Tfrm_MfPlayer.butFullScreenClick(Sender: TObject);
begin

  if pb_IsFullScreen then
    SetWindowStyle(False)
  else
    SetWindowStyle(True);

  // Prior to SDK version RedStone5, for fullscreen modus you could use this function:
  // IMFVideoDisplayControl.GetFullscreen /  IMFVideoDisplayControl.SetFullscreen
  // However this API is deprecated and not functioning since SDK version RedStone5
  //===============================================================================

end;


function Tfrm_MfPlayer.SubtitleLoadPending(): Boolean;
begin

  Result := Assigned(FSubtitleLoadThread) or FSubtitleLoadPending;
end;


procedure Tfrm_MfPlayer.StopSubtitleLoad();
var
  LoadThread: TMfSubtitleLoadThread;

begin

  FSubtitleLoadPending := False;
  FPendingSubtitleLoadLanguageTag := '';

  if not Assigned(FSubtitleLoadThread) then
    Exit;

  LoadThread := TMfSubtitleLoadThread(FSubtitleLoadThread);
  FSubtitleLoadThread := nil;
  LoadThread.CancelLoad();

  LoadThread.WaitFor();
  LoadThread.Free();

  if Assigned(dlgTimedTextLanguages) then
    dlgTimedTextLanguages.SubtitleLoadCompleted(E_ABORT);
end;


function Tfrm_MfPlayer.StartEmbeddedSubtitleLoad(
  const StreamIndex: DWORD): HRESULT;
var
  LoadThread: TMfSubtitleLoadThread;

begin

  Result := E_POINTER;

  if not Assigned(MfPlayerX) or
     not Assigned(MfPlayerX.SubtitleCompositor) then
    Exit;

  Inc(FSubtitleLoadSerial);
  LoadThread := nil;

  try
    LoadThread := TMfSubtitleLoadThread.Create(MfPlayerX.SubtitleCompositor,
                                               slkEmbedded,
                                               StreamIndex,
                                               '',
                                               MfPlayerX.MediaFileName,
                                               Handle,
                                               FSubtitleLoadSerial);
    FSubtitleLoadThread := LoadThread;

    if Assigned(dlgTimedTextLanguages) then
      dlgTimedTextLanguages.SubtitleLoadStarted();
    UpdateCastControls();

    LoadThread.Start();
    Result := S_OK;
  except
    FSubtitleLoadThread := nil;
    LoadThread.Free();
    Result := E_OUTOFMEMORY;
  end;
end;


function Tfrm_MfPlayer.StartSidecarSubtitleLoad(
  const LanguageTag: string): HRESULT;
var
  LoadThread: TMfSubtitleLoadThread;

begin

  Result := E_POINTER;

  if not Assigned(MfPlayerX) or
     not Assigned(MfPlayerX.SubtitleCompositor) then
    Exit;

  Inc(FSubtitleLoadSerial);
  LoadThread := nil;

  try
    LoadThread := TMfSubtitleLoadThread.Create(MfPlayerX.SubtitleCompositor,
                                               slkSidecar,
                                               0,
                                               LanguageTag,
                                               MfPlayerX.MediaFileName,
                                               Handle,
                                               FSubtitleLoadSerial);
    FSubtitleLoadThread := LoadThread;

    if Assigned(dlgTimedTextLanguages) then
      dlgTimedTextLanguages.SubtitleLoadStarted();
    UpdateCastControls();

    LoadThread.Start();
    Result := S_OK;
  except
    FSubtitleLoadThread := nil;
    LoadThread.Free();
    Result := E_OUTOFMEMORY;
  end;
end;


procedure Tfrm_MfPlayer.StartPendingSubtitleLoad();
var
  PendingEmbedded: Boolean;
  PendingStreamIndex: DWORD;
  PendingLanguageTag: string;
  Hr: HRESULT;

begin

  if not FSubtitleLoadPending then
    Exit;

  PendingEmbedded := FPendingSubtitleLoadEmbedded;
  PendingStreamIndex := FPendingSubtitleLoadStreamIndex;
  PendingLanguageTag := FPendingSubtitleLoadLanguageTag;
  FSubtitleLoadPending := False;
  FPendingSubtitleLoadLanguageTag := '';

  if PendingEmbedded then
    Hr := StartEmbeddedSubtitleLoad(PendingStreamIndex)
  else
    Hr := StartSidecarSubtitleLoad(PendingLanguageTag);

  if (Hr <> S_OK) and Assigned(dlgTimedTextLanguages) then
    dlgTimedTextLanguages.SubtitleLoadCompleted(Hr);
end;


function Tfrm_MfPlayer.SelectEmbeddedSubtitleTrackAsync(
  const StreamIndex: DWORD): HRESULT;
begin

  Result := E_POINTER;

  if not Assigned(MfPlayerX) or
     not Assigned(MfPlayerX.SubtitleCompositor) then
    Exit;

  if Assigned(FSubtitleLoadThread) then
    begin

      // Do not wait for the previous selection on the VCL thread. Keep only
      // the newest request and start it when cancellation completes.
      FSubtitleLoadPending := True;
      FPendingSubtitleLoadEmbedded := True;
      FPendingSubtitleLoadStreamIndex := StreamIndex;
      FPendingSubtitleLoadLanguageTag := '';
      TMfSubtitleLoadThread(FSubtitleLoadThread).CancelLoad();

      if Assigned(dlgTimedTextLanguages) then
        dlgTimedTextLanguages.SubtitleLoadStarted();

      Result := S_OK;
      Exit;
    end;

  Result := StartEmbeddedSubtitleLoad(StreamIndex);
end;


function Tfrm_MfPlayer.SelectSidecarSubtitleLanguageAsync(const LanguageTag: string): HRESULT;
begin

  Result := E_POINTER;

  if not Assigned(MfPlayerX) or
     not Assigned(MfPlayerX.SubtitleCompositor) then
    Exit;

  if Assigned(FSubtitleLoadThread) then
    begin
      FSubtitleLoadPending := True;
      FPendingSubtitleLoadEmbedded := False;
      FPendingSubtitleLoadStreamIndex := 0;
      FPendingSubtitleLoadLanguageTag := LanguageTag;
      TMfSubtitleLoadThread(FSubtitleLoadThread).CancelLoad();

      if Assigned(dlgTimedTextLanguages) then
        dlgTimedTextLanguages.SubtitleLoadStarted();

      Result := S_OK;
      Exit;
    end;

  Result := StartSidecarSubtitleLoad(LanguageTag);
end;


procedure Tfrm_MfPlayer.StartPreferredEmbeddedSubtitleLoad();
var
  StreamIndex: DWORD;

begin

  if not Assigned(MfPlayerX) or
     MfPlayerX.TimedTextFileLoaded or
     (not MfPlayerX.SubtitleSourcesAvailable) then
    Exit;

  StreamIndex := 0;

  if (MfPlayerX.GetPreferredEmbeddedSubtitleStreamIndex(StreamIndex) = S_OK) then
    SelectEmbeddedSubtitleTrackAsync(StreamIndex);
end;


procedure Tfrm_MfPlayer.WMSubtitleLoadEvent(var Msg: TMessage);
var
  LoadThread: TMfSubtitleLoadThread;
  LoadResult: HRESULT;
  LoadedMediaFileName: WideString;

begin

  if not Assigned(FSubtitleLoadThread) then
    Exit;

  LoadThread := TMfSubtitleLoadThread(FSubtitleLoadThread);

  if (Integer(Msg.WParam) <> LoadThread.Serial) then
    Exit;

  FSubtitleLoadThread := nil;
  LoadThread.WaitFor();
  LoadResult := LoadThread.ResultCode;
  LoadedMediaFileName := LoadThread.MediaFileName;
  LoadThread.Free();

  if FSubtitleLoadPending then
    begin
      StartPendingSubtitleLoad();
      Exit;
    end;

  if (LoadResult = S_OK) and
     Assigned(MfPlayerX) and
     SameText(MfPlayerX.MediaFileName,
              LoadedMediaFileName) then
    begin
      MfPlayerX.CommitSubtitleSelection();
      mnuEnableSubtitling.Checked := MfPlayerX.TimedTextFileLoaded;
      MfPlayerX.SubtitlesEnabled := MfPlayerX.TimedTextFileLoaded;
      mnuExportSubtitled.Enabled := MfPlayerX.TimedTextFileLoaded;
    end;

  if Assigned(dlgTimedTextLanguages) then
    dlgTimedTextLanguages.SubtitleLoadCompleted(LoadResult);
  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.QuitMfPlayerSession();
var
  hr: HRESULT;

begin

  StopSubtitleLoad();
  ResetInterface();

  if not Assigned(MfPlayerX) then
    Exit;

  // ShutDown waits for MESessionClosed and breaks the Media Foundation
  // callback/session relationship. Do not wait for the public Stopped
  // state here because Stop is asynchronous.
  hr := MfPlayerX.ShutDown();

  if FAILED(hr) then
    OutputDebugString(PChar(Format('ShutDown failed, HRESULT=0x%.8x',
                                   [DWORD(hr)])));

  FreeAndNil(MfPlayerX);
end;


procedure Tfrm_MfPlayer.mnuOpenClick(Sender: TObject);
var
  hr: HRESULT;

begin

  // End a previous session
  QuitMfPlayerSession();

  hr := GetFmPlayer();
  if SUCCEEDED(hr) then
    begin

      if dlgOpenUrl.Execute then
        begin
          hr := MfPlayerX.OpenURL(PWideChar(dlgOpenUrl.Filename));

          if SUCCEEDED(hr) then
            begin
              mnuSetPosition.Enabled := True;
              pnlControls.Enabled := True;
              mnuTakeScreenshot.Enabled := True;
              mnuExportSubtitled.Enabled := MfPlayerX.TimedTextFileLoaded;
              mnuMediaInfo.Enabled := True;
              sMediaFileName := dlgOpenUrl.Filename;

              // Keep subtitle selection reachable for every opened media
              // file. The dialog rescans embedded tracks and sidecars on show;
              // a failed initial import must never disable its recovery path.
              mnuSubtitling.Enabled := True;
              mnuLanguage.Enabled := True;
              mnuEnableSubtitling.Checked := MfPlayerX.TimedTextFileLoaded;
              MfPlayerX.SubtitlesEnabled := MfPlayerX.TimedTextFileLoaded;

              RealignInterface();

              // FormCreate disables the individual playback buttons before
              // a player exists. Recalculate them now that OpenURL succeeded.
              UpdateCastControls();

              // Sidecar subtitles are already available. Embedded subtitle
              // cue data can require a complete MKV pass, so load the preferred
              // track on a worker after playback opening has completed.
              StartPreferredEmbeddedSubtitleLoad();
            end //SUCCEEDED
          else
            MessageBox(0,
                       lpcwstr('MfPlayer could not open ' + LFEED +
                       lpcwstr(dlgOpenUrl.Filename)),
                       lpcwstr('Initial Failure!'),
                       MB_ICONERROR);
        end // dlg execute
      else // User pressed cancel
        QuitMfPlayerSession();
    end
  else // could not init MfPlayer
    MessageBox(0,
               lpcwstr('Failed to initialize MfPlayer.'),
               lpcwstr('Initial Failure!'),
               MB_ICONERROR);
end;


procedure Tfrm_MfPlayer.mnuEnableSubtitlingClick(Sender: TObject);
begin

  if Assigned(MfPlayerX) then
    MfPlayerX.SubtitlesEnabled := mnuEnableSubtitling.Checked;
end;


procedure Tfrm_MfPlayer.Rate1Click(Sender: TObject);
begin

  // Set video rate back to normal
  MfPlayerX.SetRate(1.0);
end;


procedure Tfrm_MfPlayer.Ratep2Click(Sender: TObject);
begin

  // Set video rate to maximum speed
  MfPlayerX.SetRate(MfPlayerX.MaxPlayBackRate);
end;


procedure Tfrm_MfPlayer.Ratem1Click(Sender: TObject);
begin

  // Set video rate to minimum speed
  MfPlayerX.SetRate(MfPlayerX.MinPlayBackRate);
end;


procedure Tfrm_MfPlayer.RealignInterface();
var
  crD: TRECT;
  pcrD: LPRECT;

begin

  if bAppIsClosing then
    Exit;

  if (prbProgress <> nil) then
    prbProgress.Max:= prbProgress.Width;

  // Set video size
  if Assigned(MfPlayerX) then
    begin
      crD.left := 0;
      crD.top := 0;
      crD.right := pnlVideo.ClientWidth;
      crD.bottom := pnlVideo.ClientHeight;
      CopyTRectToLPRect(crD, pcrD);
      //Stop flickering of controls and subtitle when resizing.
      MfPlayerX.ResizeVideo(pcrD);
    end;
end;


procedure Tfrm_MfPlayer.pnlVideoResize(Sender: TObject);
begin

  RealignInterface();
end;


procedure Tfrm_MfPlayer.prbProgressMouseEnter(Sender: TObject);
begin

  lblBarPositionInSTime.Visible := True;
end;


procedure Tfrm_MfPlayer.prbProgressMouseLeave(Sender: TObject);
begin

  lblBarPositionInSTime.Visible := False;
end;


procedure Tfrm_MfPlayer.prbProgressMouseMove(Sender: TObject;
                                             Shift: TShiftState;
                                             X, Y: Integer);
var
  secPos: Float;
  hnsPos: Int64;

begin

  if (not Assigned(MfPlayerX)) or (prbProgress.Width <= 0) then
    Exit;

  // Show only when TopologyReady/playing/paused.
  if MfPlayerX.State in [TopologyReady, Started, Paused] then
    begin

      if (X <= 0) then
       secPos := 0.0
      else
       if (X >= prbProgress.Width) then
         secPos := MfPlayerX.Duration
       else
         secPos := ((X / prbProgress.Width) * MfPlayerX.Duration);

       hnsPos := Trunc(secPos);
       lblBarPositionInSTime.Caption := Format('Position: %s',
                                               [HnsTimeToStr(hnsPos,
                                                             False)]);
    end;
end;

// Seek
procedure Tfrm_MfPlayer.prbProgressMouseUp(Sender: TObject;
                                           Button: TMouseButton;
                                           Shift: TShiftState;
                                           X, Y: Integer);
var
  fPos: Float;
  hr: HRESULT;
  SeekPosition100ns: Int64;

begin

  if (not Assigned(MfPlayerX)) or (prbProgress.Width <= 0) then
    Exit;

  if (X <= 0) then
    fPos := 0.0
  else
    if (X >= prbProgress.Width) then
      fPos := MfPlayerX.Duration
    else
      fPos := ((X / prbProgress.Width) * MfPlayerX.Duration);

  SeekPosition100ns := Trunc(fPos);

  if MfCastIsActive() and Assigned(FMfCast) then
    begin

      hr := FMfCast.Seek(SeekPosition100ns / 10000000.0);
      if FAILED(hr) then
        SetCastStatusText('Could not seek ChromeCast. HRESULT $' +
                          IntToHex(DWORD(hr), 8));

      if SUCCEEDED(hr) then
        SetCastStatusText('ChromeCast: Seeking');
    end;

  MfPlayerX.SetNewPosition := SeekPosition100ns; // original media timeline
  MfPlayerX.SendPlayerRequest(reqSeek);

  if (X < prbProgress.Min) then
    prbProgress.Position := prbProgress.Min
  else
    if (X > prbProgress.Max) then
      prbProgress.Position := prbProgress.Max
    else
      prbProgress.Position := X;
end;


procedure Tfrm_MfPlayer.mnuSetPositionClick(Sender: TObject);
const
  iSec: integer = 1000000;  // 1000000 * 100 nanoseconds is 1 second

var
  sPos: string;
  sDur: string;
  iDef: int64;

begin

  // The calculated value is not accurate!
  sDur := IntToStr(int64(Trunc(MfPlayerX.Duration / (iSec * 60))));
  sPos := InputBox('Enter a position in seconds',
                   'Enter a value between 0 and ' + sDur + '.',
                   '1');

  iDef := StrToInt64Def(sPos, 0) * iSec;
  if (iDef >= MfPlayerX.Duration) then
    iDef := 0; // Set back to start position

  MfPlayerX.SetNewPosition:= iDef; // set new StartPosition
  MfPlayerX.SendPlayerRequest(reqSeek);
end;


// Show ChromeCast dialog.
procedure Tfrm_MfPlayer.mnuCastToClick(Sender: TObject);
var
  hr: HRESULT;
  PositionHr: HRESULT;
  CastStartPosition100ns: MFTIME;
  Device: TMfCastDevice;
  Subtitle: TMfCastSubtitleAsset;
  CastMediaMode: TMfCastMediaMode;
  CastSubtitleMode: TMfCastSubtitleMode;

begin

  if not Assigned(FMfCast) then
    begin
      Caption := 'ChromeCast support is not initialized.';
      Exit;
    end;

  if MfCastIsActive() then
    Exit;

  if Trim(sMediaFileName) = '' then
    begin
      Caption := 'Open a media file before casting.';
      Exit;
    end;

  if SubtitleLoadPending() then
    begin
      SetCastStatusText('ChromeCast: The selected subtitle track is still loading.');
      UpdateCastControls();
      Exit;
    end;

  if not CastDevicesDlg.Execute(FMfCast,
                                Device) then
    Exit;

  SetCastStatusText('ChromeCast: Connecting to ' + Device.FriendlyName);
  UpdateCastControls();

  Subtitle.Reset();

  if Assigned(MfPlayerX) and MfPlayerX.SubtitlesEnabled then
    begin
      hr := MfPlayerX.ExportActiveSubtitlesAsWebVtt(Subtitle.Data,
                                                     Subtitle.Language,
                                                     Subtitle.Name);
      if FAILED(hr) then
        begin
          SetCastStatusText('ChromeCast: Could not prepare the active subtitles.');
          UpdateCastControls();
          Exit;
        end;

      if (hr = S_OK) then
        begin
          Subtitle.Enabled := Length(Subtitle.Data) > 0;
          Subtitle.ContentType := 'text/vtt; charset=utf-8';
          Subtitle.AspectRatio := MfPlayerX.SubtitleAspectRatio;
          Subtitle.Language := StringReplace(Trim(Subtitle.Language),
                                             '_', '-', [rfReplaceAll]);
          if Subtitle.Language = '' then
            Subtitle.Language := 'und';
          if Trim(Subtitle.Name) = '' then
            Subtitle.Name := 'Subtitles';
        end;
    end;

  CastStartPosition100ns := 0;
  FCastLocalWasRunning := False;

  if Assigned(MfPlayerX) then
    begin
      FCastLocalWasRunning := MfPlayerX.State in [Starting, Started];
      PositionHr := MfPlayerX.GetPosition(CastStartPosition100ns);

      if FAILED(PositionHr) then
        CastStartPosition100ns := MfPlayerX.Position * ONE_HNS_MSEC;
      if (CastStartPosition100ns < 0) then
        CastStartPosition100ns := 0;
    end;

  FCastStartPosition100ns := CastStartPosition100ns;
  FCastSyncPending := True;
  FCastSessionActive := True;

  CastMediaMode := cmmAutomatic;
  CastSubtitleMode := csmAutomatic;
  if Subtitle.Enabled and SameText(ExtractFileExt(sMediaFileName), '.mp4') then
    begin
      CastMediaMode := cmmTranscodeBurnedSubtitles;
      CastSubtitleMode := csmBurnIntoVideo;
    end;

  FCastWorker := TMfPlayerCastWorker.Create(FMfCast,
                                            Device,
                                            sMediaFileName,
                                            Subtitle,
                                            CastMediaMode,
                                            CastSubtitleMode,
                                            CastStartPosition100ns / 10000000.0,
                                            Handle);
  FCastWorker.Start();

  // Keep local playback visible while the receiver connects and buffers.
  StartLocalPlaybackForCasting();
  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.MfCastStateChanged(const AOldState,
                                           ANewState: TMfCastState);
begin

  if not bAppIsClosing then
    PostMessage(Handle, WM_APP + 450, WPARAM(ANewState), 0);
end;


procedure Tfrm_MfPlayer.ApplyMfCastState(const ANewState: TMfCastState);
begin

  UpdateCastControls();

  case ANewState of
    csIdle,
    csStopped: begin
                 ResetCastConnectivityWatchdog();
                 if FCastSessionActive then
                   StopPlaybackAfterCastEnded()
                 else
                   begin
                     CancelPendingCastSynchronization(False);
                     ClearCastStatusText();
                   end;
               end;

    csError:   begin
                 ResetCastConnectivityWatchdog();
                 if FCastSessionActive then
                   StopPlaybackAfterCastEnded()
                 else
                   CancelPendingCastSynchronization(False); // MfCastError supplies the detailed error text.
               end;

    csBuffering:
      begin
        SetCastStatusText('ChromeCast: ' + MfCastStateToString(ANewState));
      end;

  else
    begin
      ResetCastConnectivityWatchdog();
      SetCastStatusText('ChromeCast: ' + MfCastStateToString(ANewState));
    end;
  end;
end;


procedure Tfrm_MfPlayer.ResetCastConnectivityWatchdog();
begin

  if Assigned(FCastConnectivityTimer) then
    FCastConnectivityTimer.Enabled := False;

  FCastBufferingStartedTick := 0;
  FCastConnectivityWarningShown := False;
end;


procedure Tfrm_MfPlayer.CastConnectivityTimer(Sender: TObject);
begin
  ResetCastConnectivityWatchdog();
end;


procedure Tfrm_MfPlayer.MfCastMediaStatus(const AStatus: TMfCastMediaStatus);
var
  StatusMessage: TMfPlayerCastStatusMessage;
begin
  if bAppIsClosing then
    Exit;
  StatusMessage := TMfPlayerCastStatusMessage.Create;
  StatusMessage.Status := AStatus;
  if not PostMessage(Handle, WM_APP + 451, WPARAM(StatusMessage), 0) then
    StatusMessage.Free;
end;


procedure Tfrm_MfPlayer.ApplyMfCastMediaStatus(const AStatus: TMfCastMediaStatus);
var
  StatusText: string;

begin

  StatusText := Trim(AStatus.PlayerState);
  if (StatusText = '') then
    Exit;

  if SameText(StatusText,
              'PLAYING') then
    SynchronizeLocalPlaybackToCast(AStatus.CurrentTime100ns);

  if SameText(StatusText,
              'IDLE') and
     SameText(Trim(AStatus.IdleReason),
              'FINISHED') then
    begin
      CompletePlayback();
      Exit;
    end;

  if SameText(StatusText,
              'LOADING') then
    StatusText := 'BUFFERING';

  if SameText(StatusText,
              'IDLE') and (Trim(AStatus.IdleReason) = '') then
    StatusText := 'IDLE (no idleReason)'
  else
    if (Trim(AStatus.IdleReason) <> '') then
      StatusText := StatusText + ' (' + AStatus.IdleReason + ')';

  SetCastStatusText('ChromeCast: ' + StatusText);
  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.MfCastError(const AError: TMfCastErrorInfo);
var
  ErrorMessage: TMfPlayerCastErrorMessage;
begin
  if bAppIsClosing then
    Exit;
  ErrorMessage := TMfPlayerCastErrorMessage.Create;
  ErrorMessage.ErrorInfo := AError;
  if not PostMessage(Handle, WM_APP + 452, WPARAM(ErrorMessage), 0) then
    ErrorMessage.Free;
end;


procedure Tfrm_MfPlayer.ApplyMfCastError(const AError: TMfCastErrorInfo);
var
  ErrorText: string;

begin

  if FCastSessionActive then
    StopPlaybackAfterCastEnded()
  else
    CancelPendingCastSynchronization(False);
  ErrorText := 'ChromeCast error';

  if (Trim(AError.Stage) <> '') then
    ErrorText := ErrorText + ' at ' + AError.Stage;

  if (Trim(AError.MessageText) <> '') then
    ErrorText := ErrorText + ': ' + AError.MessageText;

  ErrorText := ErrorText + '. HRESULT $' + IntToHex(DWORD(AError.HResult), 8);

  if (Trim(AError.Detail) <> '') then
    ErrorText := ErrorText + ' ' + AError.Detail;

  SetCastStatusText(ErrorText);
  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.WmMfCastState(var Msg: TMessage);
begin
  ApplyMfCastState(TMfCastState(Msg.WParam));
end;


procedure Tfrm_MfPlayer.WmMfCastStatus(var Msg: TMessage);
var
  StatusMessage: TMfPlayerCastStatusMessage;
begin
  StatusMessage := TMfPlayerCastStatusMessage(Msg.WParam);
  try
    ApplyMfCastMediaStatus(StatusMessage.Status);
  finally
    StatusMessage.Free;
  end;
end;


procedure Tfrm_MfPlayer.WmMfCastError(var Msg: TMessage);
var
  ErrorMessage: TMfPlayerCastErrorMessage;
begin
  ErrorMessage := TMfPlayerCastErrorMessage(Msg.WParam);
  try
    ApplyMfCastError(ErrorMessage.ErrorInfo);
  finally
    ErrorMessage.Free;
  end;
end;


procedure Tfrm_MfPlayer.WmMfCastFinished(var Msg: TMessage);
var
  hr: HRESULT;
begin
  hr := HRESULT(Msg.WParam);
  if Assigned(FCastWorker) then
    begin
      FCastWorker.WaitFor();
      FreeAndNil(FCastWorker);
    end;

  if FAILED(hr) and Assigned(FMfCast) and (FMfCast.State() <> csError) then
    begin
      SetCastStatusText('Could not start casting. HRESULT $' +
                        IntToHex(DWORD(hr), 8));
      CancelPendingCastSynchronization(False);
    end;
  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.mnuStopCastingClick(Sender: TObject);
var
  hr: HRESULT;

begin

  hr := S_OK;

  if Assigned(FMfCast) then
    hr := FMfCast.Disconnect();

  if SUCCEEDED(hr) then
    begin
      if Assigned(MfPlayerX) then
        MfPlayerX.SendPlayerRequest(reqStop);

      ResetInterface();
      SetCastStatusText('ChromeCast: Stopped');
      UpdateCastControls();
      Caption := 'Casting stopped.';
    end
  else
    Caption := 'Casting could not be stopped.';
end;


procedure Tfrm_MfPlayer.mnuPauseCastingClick(Sender: TObject);
begin

  PauseCasting();
end;


procedure Tfrm_MfPlayer.mnuResumeCastingClick(Sender: TObject);
begin

  ResumeCasting();
end;


procedure Tfrm_MfPlayer.mnuCinemaClick(Sender: TObject);
begin

  ps_AspectRatio := AR_235_1;
  mnuCinema.Checked := True;
  mnuSixteenByNine.Checked := False;
  mnuFourByThree.Checked := False;

  if Assigned(MfPlayerX) then
    MfPlayerX.SubtitleAspectRatio := ps_AspectRatio;
  ForceResize();
end;


procedure Tfrm_MfPlayer.mnuSixteenByNineClick(Sender: TObject);
begin

  ps_AspectRatio := AR_16_9; // default
  mnuCinema.Checked := False;
  mnuSixteenByNine.Checked := True;
  mnuFourByThree.Checked := False;

  if Assigned(MfPlayerX) then
    MfPlayerX.SubtitleAspectRatio := ps_AspectRatio;
  ForceResize();
end;


procedure Tfrm_MfPlayer.mnuFourByThreeClick(Sender: TObject);
begin

  ps_AspectRatio := AR_4_3;
  mnuCinema.Checked := False;
  mnuSixteenByNine.Checked := False;
  mnuFourByThree.Checked := True;

  if Assigned(MfPlayerX) then
    MfPlayerX.SubtitleAspectRatio := ps_AspectRatio;
  ForceResize();
end;


// VOLUMES ---------------------------------------------------------------------

procedure Tfrm_MfPlayer.SetVolumeChannels(volchans: TFloatArray);
var
  iSliderL, iSliderR: FLOAT;

begin

  if (not Assigned(MfPlayerX)) or
     (MfPlayerX.SoundChannels = 0) or
     (Length(MfPlayerX.m_VolumeChannels) < Integer(MfPlayerX.SoundChannels)) then
    Exit;

  iSliderL := (trbVolumeL.Position * 0.01);
  iSliderR := (trbVolumeR.Position * 0.01);

  // Mono
  // This is a very rare case, because mono is played on the leftchannel only or
  // on both channels without stereo effect.
  if (MfPlayerX.SoundChannels = 1) then
    MfPlayerX.m_VolumeChannels[0] := iSliderL;

  // Stereo
  // The first stereo channel (0) is always the LEFT one!
  if (MfPlayerX.SoundChannels = 2) then
    begin
      MfPlayerX.m_VolumeChannels[0] := iSliderL;
      MfPlayerX.m_VolumeChannels[1] := iSliderR;
    end;

  // DD 5.1 (AC3)
  // DD5.1 has 2 kind of formats:
  //       channels: 1  2  3  4    5   6
  // SMPTE standard: R, L, C, LFE, Rs, Ls  (most used because of R&L layout is the same as Stereo)
  // Film:           L, C, R, Ls,  Rs, LFE
  //
  // Frequencies: R, L, C, Rs and Ls: 20-20.000 Hz
  //              LFE: 20-120 Hz
  //
  // Wether or not we are dealing with Dolby Digital, use the class identifier CLSID_CMSDolbyDigitalEncMFT
  //
  // SMPTE to stereo
  // Here we combine the channels with both sliders (Right and Left)
  //
  // If (MfPlayer.m_aStreamCont[High(MfPlayer.m_aStreamCont)].dwFormatTag = MEDIASUBTYPE_DVM) then
  // You could assign more volumecontrols for the known sound decoder.
  if (MfPlayerX.SoundChannels = 6) then
    begin

      // Channel 1  R
      MfPlayerX.m_VolumeChannels[0] := iSliderR;
      // Channel 2  L
      MfPlayerX.m_VolumeChannels[1] := iSliderL;
      // Channel 3  C >> most of the time, this is the character's voice channel.
      MfPlayerX.m_VolumeChannels[2] := iSliderL;
      // Channel 4  LFE
      MfPlayerX.m_VolumeChannels[3] := (iSliderR + iSliderL) / 2;
      // Channel 5  Rs
      MfPlayerX.m_VolumeChannels[4] := iSliderR;
      // Channel 6  LS
      MfPlayerX.m_VolumeChannels[5] := iSliderL;
    end;
  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
end;


// Right volume channel
procedure Tfrm_MfPlayer.trbVolumeRChange(Sender: TObject);
begin

  if not Assigned(MfPlayerX) then
    Exit;

  if (MfPlayerX.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeL.Position := trbVolumeR.Position;
      SetVolumeChannels(MfPlayerX.m_VolumeChannels);
    end;
end;


// Left volume channel
procedure Tfrm_MfPlayer.trbVolumeLChange(Sender: TObject);
begin

  if not Assigned(MfPlayerX) then
    Exit;

  if (MfPlayerX.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeR.Position := trbVolumeL.Position;
      if (MfPlayerX.SoundChannels > 0) then
        SetVolumeChannels(MfPlayerX.m_VolumeChannels);
    end;
end;
//------------------------------------------------------------------------------

procedure Tfrm_MfPlayer.FormCloseQuery(Sender: TObject;
                                       var CanClose: Boolean);

begin

  if Assigned(FExportThread) then
    begin
      FExportClosePending := True;
      FExportStopPending := False;

      OutputDebugString(PChar('Close requested while export is active'));

     (FExportThread as TMfSubtitleExportThread).CancelExport();

      mnuExportSubtitled.Caption := 'Cancelling subtitled MP4...';

      mnuExportSubtitled.Enabled := False;

      CanClose := False;
      Exit;
    end;

  CanClose := False;
  bAppIsClosing := True;

  if Assigned(FMfCast) then
    begin
      FMfCast.OnStateChanged := nil;
      FMfCast.OnMediaStatus := nil;
      FMfCast.OnError := nil;
      FMfCast.Disconnect();
    end;

  if Assigned(FCastWorker) then
    begin
      FCastWorker.WaitFor();
      FreeAndNil(FCastWorker);
    end;

  FreeAndNil(FMfCast);

  QuitMfPlayerSession();

  CanClose := True;
end;


procedure Tfrm_MfPlayer.FormCreate(Sender: TObject);
begin

  prbProgress.Max := prbProgress.Width;
  bAppIsClosing := False;
  pb_IsFullScreen := False;
  FExportThread := nil;
  FExportClosePending := False;
  FExportStopPending := False;
  FSubtitleLoadThread := nil;
  FSubtitleLoadSerial := 0;
  FSubtitleLoadPending := False;
  FPendingSubtitleLoadEmbedded := False;
  FPendingSubtitleLoadStreamIndex := 0;
  FPendingSubtitleLoadLanguageTag := '';
  FMfCast := nil;
  FCastWorker := nil;
  FCastStartPosition100ns := 0;
  FCastSyncPending := False;
  FCastLocalWasRunning := False;
  FCastSessionActive := False;
  FCastBufferingStartedTick := 0;
  FCastConnectivityWarningShown := False;
  FCastConnectivityTimer := TTimer.Create(Self);
  FCastConnectivityTimer.Enabled := False;
  FCastConnectivityTimer.Interval := 1000;
  FCastConnectivityTimer.OnTimer := CastConnectivityTimer;
  ps_AspectRatio := AR_16_9;
  mnuStopCasting.Enabled := False;

  try
    FMfCast := TMfCast.Create(True);
    FMfCast.OnStateChanged := MfCastStateChanged;
    FMfCast.OnMediaStatus := MfCastMediaStatus;
    FMfCast.OnError := MfCastError;
  except
    on E: Exception do
      begin
        FreeAndNil(FMfCast);
        Caption := 'Cast API setup failed: ' + E.Message;
      end;
  end;

  UpdateCastControls();
end;


procedure Tfrm_MfPlayer.FormKeyUp(Sender: TObject;
                                  var Key: Word;
                                  Shift: TShiftState);
var
  bm: TBitmap;

begin

  case Key of
    VK_SPACE:   if Assigned(MfPlayerX) then
                  begin
                    case MfPlayerX.State of
                      Started: butPauseClick(Self);
                      OpenPending: butPlayClick(Self);
                      Stopped, Paused: butPlayClick(Self);
                    end;
                  end;

    VK_END:     if Assigned(MfPlayerX) then
                  begin
                    butStopClick(Self);
                  end;

    VK_F11:     begin  //Shut down
                  if Assigned(MfPlayerX) then
                    MfPlayerX.ShutDown();
                end;

    VK_F12:     mnuOpenClick(Nil);

    //take a snapshot and copy the bitmap to the clipboard
    VK_F8:      begin
                  bm:= TBitmap.Create;
                  MfPlayerX.TakeSnapShot(bm);
                  Clipboard.Assign(bm);
                  FreeAndNil(bm);
                end;

    // Use left and right arrows to adjust the volume.
    // since there is a trackbar, this one will have the focus.
    // We did not implement a balance method.
    VK_LEFT:    begin
                  if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
                    MfPlayerX.m_VolumeChannels[0]:= MfPlayerX.m_VolumeChannels[0] - 0.01;

                  if (Length(MfPlayerX.m_VolumeChannels) > 1) then
                    MfPlayerX.m_VolumeChannels[1]:= MfPlayerX.m_VolumeChannels[1] - 0.01;
                  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
                end;

    VK_RIGHT:   begin
                  if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
                    MfPlayerX.m_VolumeChannels[0]:= MfPlayerX.m_VolumeChannels[0] + 0.01;

                  if (Length(MfPlayerX.m_VolumeChannels) > 1) then
                    MfPlayerX.m_VolumeChannels[1]:= MfPlayerX.m_VolumeChannels[1] + 0.01;
                  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
                end;

    VK_ESCAPE:  begin
                  butFullScreenClick(Self);
                end;
  end;
end;


function Tfrm_MfPlayer.GetFmPlayer(): HRESULT;
begin

  Result:= E_FAIL;

  if not Assigned(MfPlayerX) then
    begin
      MfPlayerX := nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      MfPlayerX := TMfPlayerX.Create(pnlVideo.Handle,       // The clipping window / control
                                     0,                     // The window or control that receives the custom messages. (like Subtitles)
                                     frm_MfPlayer.Handle,   // The window or control that receives the (text) messages.
                                     frm_MfPlayer.Handle);  // Must be main form or parent window !!!

      // If you want to switch to a different clipping surface while the session is active:
      //MfPlayer.SetVideoSurface:= myFormOrControl.Handle;
      MfPlayerX.SubtitleAspectRatio := ps_AspectRatio;
      Result:= S_OK;
    end;
end;


procedure Tfrm_MfPlayer.mnuLanguageClick(Sender: TObject);
begin

  if Assigned(MfPlayerX) then
    begin
      // Show language dialog
      // No dialog initiated on startup, this should not happen.
      if not Assigned(dlgTimedTextLanguages) then
        begin
          ShowMessage('No TimedText Language selection dialog available.');
          Exit;
        end;
      DlgTimedTextLanguages.Show;
    end;
end;


//------------------------------------------------------------------------------
// Size and move handlers.
//------------------------------------------------------------------------------
procedure Tfrm_MfPlayer.WMSize(var Msg: TWMSize);
begin

  inherited;  // OnResize method will be handled first

  if bAppIsClosing then
    Exit;
  RealignInterface();
end;


procedure Tfrm_MfPlayer.WMMove(var Msg: TWMMove);
begin

  inherited;

  if bAppIsClosing then
    Exit;
  RealignInterface();
end;


procedure Tfrm_MfPlayer.ForceResize();
var
  pr: PRect;
  rc: TRect;

begin

  rc.Left := Left;
  rc.Top := Top;
  rc.Width := Width;
  rc.Height := Height;
  pr := @rc;

  // Force a resize to set aspectratio
  SendMessage(Handle,
              WM_SIZING,
              WParam(WMSZ_TOP),
              LParam(pr));
end;


procedure Tfrm_MfPlayer.WMProgressEvent(var Msg: TMessage);
var
  DurationMs: Int64;
  PositionMs: Int64;
  ProgressValue: Int64;

begin //Position

  if bAppIsClosing or (not Assigned(MfPlayerX)) then
    Exit;

  // WParam 3 is a worker-thread subtitled-export percentage update.
  if (Msg.WParam = 3) and Assigned(FExportThread) then
    begin
      ProgressValue := Msg.LParam;
      if ProgressValue < 0 then
        ProgressValue := 0
      else
        if ProgressValue > 100 then
          ProgressValue := 100;

      prbProgress.Position := Integer(
        (ProgressValue * prbProgress.Max) div 100);
      mnuExportSubtitled.Caption := Format('Exporting subtitled MP4... %d%%',
                                           [ProgressValue]);
      Exit;
    end;

  // The presentation clock no longer ticks after MESessionEnded, so the
  // player posts this explicit notification after the final sample drains.
  if (Msg.WParam = 2) then
    begin
      CompletePlayback();
      Exit;
    end;

  // WParam 1 is the Media Foundation presentation-clock update.
  if (Msg.WParam = 1) then
    begin
      if (MfPlayerX.State = Started) then
        begin
          DurationMs := MfPlayerX.Duration div ONE_HNS_MSEC;
          PositionMs := MfPlayerX.Position;

          // MF_PD_DURATION is optional and some MKV sources initially report
          // zero. Never divide by zero; keep playback and later timer updates
          // alive even when a duration is unavailable.
          if DurationMs > 0 then
            begin
              ProgressValue := (PositionMs * prbProgress.Max) div DurationMs;
              if (ProgressValue < prbProgress.Min) then
                ProgressValue := prbProgress.Min
              else
                if (ProgressValue > prbProgress.Max) then
                  ProgressValue := prbProgress.Max;

              prbProgress.Position := Integer(ProgressValue);
            end
          else
            prbProgress.Position := 0;
        end;

      if (MfPlayerX.State In [Closed, Stopped]) then
        ResetInterface();

      // Check if the topology is set.
      // The reason is that we have to deal with asynchronous operations.
      // Note: Topology will be set when a mediafile is loaded.
      if (MfPlayerX.State = TopologyReady) then
        begin
          // Check if forward rate is supported
          mnuSetRate.Enabled := MfPlayerX.CanSetRateForward;
          mnuSelectStreams.Enabled:= True;
        end;
    end;
end;


// Show a dialog to select audio, video or other streams
procedure Tfrm_MfPlayer.mnuSelectStreamsClick(Sender: TObject);
begin

  // No dialog initiated on startup, this should not happen.
  if not Assigned(dlgSelectStreams) then
    begin
      ShowMessage('No stream selection dialog available.');
      Exit;
    end;
  dlgSelectStreams.Show;
end;

end.
