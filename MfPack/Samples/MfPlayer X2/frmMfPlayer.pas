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
// Revision Version: 3.2.0
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
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
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
    N5: TMenuItem;
    stbCastStatus: TStatusBar;
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

  private
    { Private declarations }
    bAppIsClosing: Boolean;
    pb_IsFullScreen: Boolean;
    ps_AspectRatio: Single;
    sMediaFileName: WideString;
    FExportThread: TThread;
    FExportClosePending: Boolean;
    FExportStopPending: Boolean;
    FMfCastController: IMfCastController;

    { Private methods }
    // Size and move handlers.
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    // Forces a resize to set aspect ratio.
    procedure ForceResize();
    // Progress handling.
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;

    procedure ExportThreadTerminated(Sender: TObject);
    procedure StartSubtitledExport(const OutputFileName: WideString);
    procedure StopPlaybackNow();
    procedure SetVolumeChannels(volchans: TFloatArray);
    function MfCastIsActive(): Boolean;
    procedure SetPlaybackButtonsEnabled(const AEnabled: Boolean);
    procedure StartLocalPlaybackForCasting();
    procedure SetCastStatusText(const AText: string);
    procedure ClearCastStatusText();
    procedure UpdateCastControls();
    procedure PauseCasting();
    procedure ResumeCasting();
    procedure MfCastStateChanged(const AOldState: TMfCastState;
                                 const ANewState: TMfCastState);
    procedure MfCastMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure MfCastError(const AError: TMfCastErrorInfo);

  public
    { Public declarations }

    { Public methods }
    procedure SetWindowStyle(bFullScreen: Boolean);
    procedure ResetInterface();
    //procedure SetToParentRect();
    procedure RealignInterface();
    function GetFmPlayer(): HRESULT;
    procedure QuitMfPlayerSession();
  end;

var
  frm_MfPlayer: Tfrm_MfPlayer;


implementation

uses

  {Vcl}
  Vcl.ClipBrd,
  {Application}
  MfSubtitleCompositorX2,
  MfSubtitleFramePumpX2,
  dlgStreamSelect,
  dlgSelectTimedTextLanguages,
  {ChromeCast}
  MfCastController,
  MfCastChannel,
  MfCastDiscovery,
  MfCastHttpServer,
  MfCastMedia,
  MfCastTranscode,
  MfCastTransport;

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
                       SubtitleAspectRatio: Single);

    procedure CancelExport();

    property ErrorMessage: string read FErrorMessage;
    property FramesWritten: Int64 read FFramesWritten;
    property ResultCode: HRESULT read FResult;
  end;

constructor TMfSubtitleExportThread.Create(const InputFileName: WideString;
                                           const OutputFileName: WideString;
                                           const PreferredLanguage: string;
                                           SubtitleAspectRatio: Single);
begin

  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpLower;
  FInputFileName := InputFileName;
  FOutputFileName := OutputFileName;
  FPreferredLanguage := PreferredLanguage;
  FSubtitleAspectRatio := SubtitleAspectRatio;
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
begin

  Cancel := Terminated;
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
                                                 MfPlayerX.SubtitleAspectRatio);

  FExportThread.OnTerminate := ExportThreadTerminated;
  mnuExportSubtitled.Enabled := False;
  mnuExportSubtitled.Caption := 'Exporting subtitled MP4...';
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
  Result := Assigned(FMfCastController) and
            (FMfCastController.GetState() in [csConnecting,
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
end;


procedure Tfrm_MfPlayer.SetCastStatusText(const AText: string);
begin

  if Assigned(stbCastStatus) then
    begin

      stbCastStatus.SimpleText := AText;
      stbCastStatus.Visible := Trim(AText) <> '';
    end;

  if (Trim(AText) <> '') then
    Caption := AText;
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

  if Assigned(FMfCastController) then
    CastState := FMfCastController.GetState()
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

  mnuCastTo.Enabled := Assigned(FMfCastController) and
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


procedure Tfrm_MfPlayer.PauseCasting();
var
  hr: HRESULT;

begin

  if (not Assigned(FMfCastController)) or
     (not (FMfCastController.GetState() in [csBuffering, csPlaying])) then
    Exit;

  hr := FMfCastController.Pause();
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

  if (not Assigned(FMfCastController)) or
     (FMfCastController.GetState() <> csPaused) then
    Exit;

  hr := FMfCastController.Play();
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
      begin

        MfPlayerX.SendPlayerRequest(reqPause);
      end;
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
        begin

          MfPlayerX.SendPlayerRequest(reqStart);
        end;

      // Start a new session
      if MfPlayerX.State in [OpenPending, Stopped, TopologyReady] then
        begin

          // Set initial volume
          MfPlayerX.GetVolume();

          if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
            trbVolumeR.Position:= Trunc(MfPlayerX.m_VolumeChannels[0] * 30); // left  30%
          if (Length(MfPlayerX.m_VolumeChannels) > 1) then
            trbVolumeL.Position := Trunc(MfPlayerX.m_VolumeChannels[1] * 30); // right 30%

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
  mnuTakeScreenshot.Enabled := False;
  mnuExportSubtitled.Enabled := False;
  mnuSubtitling.Enabled := False;
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

          lst := 'Video Info' + #13 + #13;
          lst := lst + 'Pixel aspect ratio: ' + FloatToStrF(MfPlayerX.StreamContents[i].video_PixelAspectRatioNumerator / MfPlayerX.StreamContents[i].video_PixelAspectRatioDenominator, ffGeneral, 4, 2) + #13;
          lst := lst + 'Framerate per second (fps): ' + FloatToStrF(MfPlayerX.StreamContents[i].video_FrameRateNumerator / MfPlayerX.StreamContents[i].video_FrameRateDenominator, ffGeneral, 4, 2) + #13;
          lst := lst + 'Framesize (w x h): ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeWidth) + ' x ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeHeigth) + #13 + #13;
        end
      else
        if (MfPlayerX.StreamContents[i].idStreamMediaType = mtAudio) then
          begin

            lst := lst + 'Audio Info' + #13 + #13;
            lst := lst + 'Format        : ' + MfPlayerX.StreamContents[i].audio_wsAudioDescr + #13;
            lst := lst + 'Channels      : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iAudioChannels) + #13;
            lst := lst + 'FormatTag     : ' + IntToStr(MfPlayerX.StreamContents[i].audio_dwFormatTag) + #13;
            lst := lst + 'SamplesPerSec : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iSamplesPerSec) + #13;
            lst := lst + 'BitsPerSample : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iBitsPerSample) + #13 + #13;
            lst := lst + 'Compressed    : ' + BoolToStr(MfPlayerX.StreamContents[i].bCompressed) + #13;
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


procedure Tfrm_MfPlayer.QuitMfPlayerSession();
var
  hr: HRESULT;

begin

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
begin

  // End a previous session
  QuitMfPlayerSession();

  if SUCCEEDED(GetFmPlayer()) then
    begin

      if dlgOpenUrl.Execute then
        begin

          if SUCCEEDED(MfPlayerX.OpenURL(PWideChar(dlgOpenUrl.Filename))) then
            begin

              mnuSetPosition.Enabled := True;
              pnlControls.Enabled := True;
              mnuTakeScreenshot.Enabled := True;
              mnuExportSubtitled.Enabled := MfPlayerX.TimedTextFileLoaded;
              mnuMediaInfo.Enabled := True;
              sMediaFileName := dlgOpenUrl.Filename;

              // enable subtitling when X2 loaded a timed-text sidecar
              mnuSubtitling.Enabled := MfPlayerX.TimedTextFileLoaded;
              mnuEnableSubtitling.Checked := MfPlayerX.TimedTextFileLoaded;
              MfPlayerX.SubtitlesEnabled := MfPlayerX.TimedTextFileLoaded;

              RealignInterface();
            end //SUCCEEDED
          else
            MessageBox(0,
                       lpcwstr('MfPlayer could not open ' + #13 +
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

  if (prbProgress <> Nil) then
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


// Seek
procedure Tfrm_MfPlayer.prbProgressMouseUp(Sender: TObject;
                                           Button: TMouseButton;
                                           Shift: TShiftState;
                                           X, Y: Integer);
var
  fPos: Float;

begin

  if (X <= 0) then
    fPos := 0.0
  else
    fPos := ((X / prbProgress.Width) * MfPlayerX.Duration);

  MfPlayerX.SetNewPosition := Trunc(fPos); // set new StartPosition
  MfPlayerX.SendPlayerRequest(reqSeek);
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
  Device: TMfCastDevice;
  SubtitleSourceName: string;

begin

  if not Assigned(FMfCastController) then
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

  if not CastDevicesDlg.Execute(FMfCastController,
                                Device) then
    Exit;

  SetCastStatusText('ChromeCast: Connecting to ' +
                    Device.FriendlyName);
  UpdateCastControls();

  SubtitleSourceName := '';
  hr := FMfCastController.CastFile(Device,
                                   sMediaFileName,
                                   SubtitleSourceName,
                                   cmmAutomatic,
                                   csmAutomatic);

  if (hr = S_OK) and
     (FMfCastController.GetState() <> csError) then
    begin

      StartLocalPlaybackForCasting();
      UpdateCastControls();
    end
  else
    begin

      if FMfCastController.GetState() <> csError then
        SetCastStatusText(MfCastStartFailureText(Device.FriendlyName,
                                                 hr));

      UpdateCastControls();
    end;
end;


procedure Tfrm_MfPlayer.MfCastStateChanged(const AOldState,
                                           ANewState: TMfCastState);
begin

  UpdateCastControls();

  case ANewState of
    csIdle,
    csStopped:
      ClearCastStatusText();

    csError:
      ; // MfCastError supplies the detailed error text.

  else
    SetCastStatusText('ChromeCast: ' +
                      MfCastStateToString(ANewState));
  end;
end;


procedure Tfrm_MfPlayer.MfCastMediaStatus(const AStatus: TMfCastMediaStatus);
var
  StatusText: string;

begin

  StatusText := Trim(AStatus.PlayerState);
  if (StatusText = '') then
    Exit;

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
  ErrorText: string;
begin

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


procedure Tfrm_MfPlayer.mnuStopCastingClick(Sender: TObject);
begin

  if Assigned(FMfCastController) then
    FMfCastController.Disconnect();

  ClearCastStatusText();
  UpdateCastControls();
  Caption := 'Casting stopped.';
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
      SetVolumeChannels(MfPlayerX.m_VolumeChannels);
    end;
end;
//------------------------------------------------------------------------------

procedure Tfrm_MfPlayer.FormCloseQuery(
  Sender: TObject;
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

  if Assigned(FMfCastController) then
    begin

      FMfCastController.Disconnect();
      FMfCastController := nil;
    end;

  QuitMfPlayerSession();

  CanClose := True;
end;


procedure Tfrm_MfPlayer.FormCreate(Sender: TObject);
var
  Components: TMfCastComponents;
  Settings: TMfCastSettings;
  DefaultProfile: TMfCastDeviceProfile;
  CapabilityResolver: IMfCastCapabilityResolver;
  CastCallbacks: TMfCastControllerCallbacks;
  hr: HRESULT;

begin

  prbProgress.Max := prbProgress.Width;
  bAppIsClosing := False;
  pb_IsFullScreen := False;
  FExportThread := nil;
  FExportClosePending := False;
  FExportStopPending := False;
  ps_AspectRatio := AR_16_9;
  mnuStopCasting.Enabled := False;

  Components.Reset();
  Components.Discovery := TMfCastMdnsDiscovery.Create();
  Components.Channel := TMfCastChannel.Create(TMfCastTcpTransport.Create());
  Components.HttpServer := TMfCastHttpServer.Create();
  Components.MediaInspector := TMfCastMediaInspector.Create();

  DefaultProfile.Reset();
  DefaultProfile.Name := 'Default ChromeCast';
  SetLength(DefaultProfile.AllowedContentTypes, 5);
  DefaultProfile.AllowedContentTypes[0] := 'video/mp4';
  DefaultProfile.AllowedContentTypes[1] := 'audio/mp4';
  DefaultProfile.AllowedContentTypes[2] := 'video/webm';
  DefaultProfile.AllowedContentTypes[3] := 'audio/mpeg';
  DefaultProfile.AllowedContentTypes[4] := 'audio/aac';
  DefaultProfile.AllowUnknownFormats := True;
  CapabilityResolver := TMfCastCapabilityResolver.Create(DefaultProfile);
  Components.MediaPlanner := TMfCastMediaPlanner.Create(CapabilityResolver);
  Components.SegmentPublisher := TMfCastSegmentPublisher.Create(Components.HttpServer);
  Components.TranscodePipeline := TMfCastTranscodePipeline.Create();

  FMfCastController := TMfCastController.Create(Components);
  Settings := TMfCastSettings.CreateDefault();
  hr := FMfCastController.Configure(Settings);
  if SUCCEEDED(hr) then
    begin

      CastCallbacks.Reset();
      CastCallbacks.OnStateChanged := MfCastStateChanged;
      CastCallbacks.OnMediaStatus := MfCastMediaStatus;
      CastCallbacks.OnError := MfCastError;
      FMfCastController.SetCallbacks(CastCallbacks);
    end;

  if FAILED(hr) then
    begin

      FMfCastController := nil;
      Caption := 'ChromeCast setup failed. HRESULT $' + IntToHex(DWORD(hr), 8);
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

      // no TimedText files found; nothing to do here.
      if not MfPlayerX.TimedTextFileLoaded then
        Exit;
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
begin //Position

  if bAppIsClosing then
    Exit;

  // WParam 1 is a subtitle text event
  if (Msg.WParam = 1) then
    begin
      if (MfPlayerX.State = Started) then
        prbProgress.Position := Trunc((prbProgress.Width / (MfPlayerX.Duration / ONE_HNS_MSEC)) * (MfPlayerX.Position));

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