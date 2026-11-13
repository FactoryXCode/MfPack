// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmCastPlayer.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.2
// Description: CastPlayer GUI.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/09/2026 All                 Explain unavailable DTS decoding and keep the
//                                audio-stream choice with the user.
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: -
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit frmCastPlayer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.ActiveX,
  WinApi.messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Cast}
  MfCast,
  MfCastInterfaces,
  MfCastTypes,
  MfCastWindowsSupport,
  {Cast/Media}
  LangTags,
  MfEmbeddedSubtitleReader;

type

  TMfCastDesktopWorker = class(TThread)
  private
    FCast: TMfCast;
    FDevice: TMfCastDevice;
    FSettings: TMfCastCaptureSettings;
    FNotifyWindow: HWND;
    procedure PostLog(const AText: string);

  protected
    procedure Execute(); override;

  public

    constructor Create(const ACast: TMfCast;
                       const ADevice: TMfCastDevice;
                       const ASettings: TMfCastCaptureSettings;
                       const ANotifyWindow: HWND);
  end;

  TCastPlayerForm = class(TForm)
    lblState: TLabel;
    memLog: TMemo;
    pnlPreview: TPanel;
    pnlCtrl: TPanel;
    Bevel2: TBevel;
    lblDevices: TLabel;
    lblSource: TLabel;
    lblArtwork: TLabel;
    lblSeek: TLabel;
    lblVolume: TLabel;
    cbxDevices: TComboBox;
    edtSource: TEdit;
    edtArtwork: TEdit;
    cbxSubtitleLanguage: TComboBox;
    cbxCaptureCodec: TComboBox;
    Bevel1: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    Label2: TLabel;
    lblAudioStream: TLabel;
    cbxAudioStream: TComboBox;
    btnCastDiscover: TButton;
    btnRefresh: TButton;
    btnBrowse: TButton;
    cbxKeepOnTop: TCheckBox;
    cbxCaptureMode: TComboBox;
    chkEmbeddedSubtitles: TCheckBox;
    btnBrowseArtwork: TButton;
    MPxpButton6: TButton;
    btnCast: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnDisconnect: TButton;
    btnCastDesktop: TButton;
    btnSeek: TButton;
    chkMuted: TCheckBox;
    trkVolume: TTrackBar;
    trkSeek: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDiscoverClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnBrowseArtworkClick(Sender: TObject);
    procedure btnCastClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSeekClick(Sender: TObject);
    procedure trkSeekChange(Sender: TObject);
    procedure trkVolumeChange(Sender: TObject);
    procedure chkMutedClick(Sender: TObject);
    procedure SourceOrDeviceChanged(Sender: TObject);
    procedure SubtitleSelectionChanged(Sender: TObject);
    procedure AudioSelectionChanged(Sender: TObject);
    procedure btnCastDesktopClick(Sender: TObject);
    procedure CaptureModeChanged(Sender: TObject);
    procedure PreviewResize(Sender: TObject);
    procedure cbxKeepOnTopClick(Sender: TObject);

  private
    FCast: TMfCast;
    FPreviewSubtitleTimer: TTimer;
    FDevices: TMfCastDeviceArray;
    FWorker: TThread;
    FSubtitleWorker: TMfCastSubtitleWorker;
    FAudioTrackWorker: TMfCastAudioTrackWorker;
    FClosing: Boolean;
    FDesktopCaptureExcluded: Boolean;
    FPreviousDisplayAffinity: DWORD;
    FUpdatingSeekPosition: Boolean;
    FSeekPositionPending: Boolean;
    FSeekTargetSeconds: Integer;
    FSeekRequestTick: Cardinal;
    FSubtitleChoices: TMfCastSubtitleChoiceArray;
    FSubtitleSourceName: string;
    FAudioTracks: TMfCastTrackInfoArray;
    FAudioSourceName: string;
    FUpdatingAudioSelection: Boolean;
    FMediaBrowseDirectory: string;
    FArtworkBrowseDirectory: string;
    FActiveDesktopMode: Integer;

    function SelectLocalFile(const AArtwork: Boolean;
                              out AFileName: string): Boolean;

    procedure DeviceChanged(const ADevice: TMfCastDevice);
    procedure DeviceRemoved(const ADeviceId: string);

    procedure CastStateChanged(const AOldState: TMfCastState;
                               const ANewState: TMfCastState);

    procedure CastMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure PreviewSubtitleTimer(Sender: TObject);

    procedure CastError(const AError: TMfCastErrorInfo);

    procedure CastLog(Sender: TObject;
                      const ALevel: TMfCastLogLevel;
                      const ASource: string;
                      const Amessage: string);

    procedure PostLogmessage(const AWindowsmessage: Cardinal;
                             const AText: string);
    procedure ReloadDevices;
    procedure ClearSubtitleChoices();
    procedure RefreshSubtitleChoices(const ASourceName: string);
    procedure AddSubtitleChoice(const AChoice: TMfCastSubtitleChoice);
    procedure BuildSelectedSubtitle(out ASubtitle: TMfCastSubtitleAsset);
    procedure ApplySubtitleSelection();
    procedure ClearAudioTracks();
    procedure RefreshAudioTracks(const ASourceName: string);
    function EnsureSelectedAudioTrackSupported(): Boolean;
    function IsDtsAudioTrack(const ATrack: TMfCastTrackInfo): Boolean;
    procedure ApplyAudioSelection();
    procedure RegisterGlobalHotKeys();
    procedure UnregisterGlobalHotKeys();

    procedure LogResult(const AOperation: string;
                        const AHResult: HRESULT);

    function SelectCaptureMonitor(out AOutputIndex: Cardinal;
                                  out ADescription: string): Boolean;

    function SetDesktopCaptureExcluded(const AExcluded: Boolean): HRESULT;
    procedure UpdateControls();

    procedure WmCastDevices(var message: Tmessage); message WM_MFCAST_DEVICES;
    procedure WmCastState(var message: Tmessage); message WM_MFCAST_STATE;
    procedure WmCastStatus(var message: Tmessage); message WM_MFCAST_STATUS;
    procedure WmCastError(var message: Tmessage); message WM_MFCAST_ERROR;
    procedure WmCastFinished(var message: Tmessage); message WM_MFCAST_FINISHED;
    procedure WmDisconnectFinished(var message: Tmessage); message WM_MFCAST_DISCONNECT_FINISHED;
    procedure WmCastLog(var message: Tmessage); message WM_MFCAST_LOG;
    procedure WmCastSubtitleFinished(var message: Tmessage); message WM_MFCAST_SUBTITLE_FINISHED;
    procedure WmCastAudioTrackFinished(var message: Tmessage); message WM_MFCAST_AUDIO_TRACK_FINISHED;
    procedure WmHotKey(var message: Tmessage); message WM_HOTKEY;
  end;

var
  CastPlayerForm: TCastPlayerForm;


implementation

uses

  {Application}
  LWFileBrowserExDlg,
  dlgCaptureMonitor;

{$R *.dfm}

const
  MFCAST_MOD_NOREPEAT = $4000;
  MFCAST_HOTKEY_CAST = 1;
  MFCAST_HOTKEY_CAST_DESKTOP = 2;
  MFCAST_HOTKEY_PLAY = 3;
  MFCAST_HOTKEY_PAUSE = 4;
  MFCAST_HOTKEY_STOP = 5;
  MFCAST_HOTKEY_VOLUME_UP = 6;
  MFCAST_HOTKEY_VOLUME_DOWN = 7;
  MFCAST_HOTKEY_MUTE = 8;

procedure TCastPlayerForm.WmDisconnectFinished(var message: Tmessage);
begin

  if Assigned(FWorker) then
    begin
      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  LogResult('Disconnect',
            HRESULT(message.WParam));
  UpdateControls();
end;


constructor TMfCastDesktopWorker.Create(const ACast: TMfCast;
                                        const ADevice: TMfCastDevice;
                                        const ASettings: TMfCastCaptureSettings;
                                        const ANotifyWindow: HWND);

begin

  inherited Create(True);

  FreeOnTerminate := False;
  FCast := ACast;
  FDevice := ADevice;
  FSettings := ASettings;
  FNotifyWindow := ANotifyWindow;
end;


function TCastPlayerForm.SelectCaptureMonitor(out AOutputIndex: Cardinal;
                                              out ADescription: string): Boolean;
var
  Dialog: TCaptureMonitorDlg;
  Monitor: TMonitor;
  I: Integer;

begin

  Result := False;
  AOutputIndex := 0;
  ADescription := '';

  if Screen.MonitorCount = 0 then
    Exit;

  Dialog := TCaptureMonitorDlg.Create(Self);
  Dialog.PopupParent := Self;

  try
    for I := 0 to Screen.MonitorCount - 1 do
      begin
        Monitor := Screen.Monitors[I];
        Dialog.lstMonitors.Items.Add(CaptureMonitorName(Monitor));

        if Monitor.Primary then
          Dialog.lstMonitors.ItemIndex := I;
      end;

    if (Dialog.lstMonitors.ItemIndex < 0) then
      Dialog.lstMonitors.ItemIndex := 0;

    if (Dialog.ShowModal <> mrOk) then
      Exit;

    if (Dialog.lstMonitors.ItemIndex < 0) or (Dialog.lstMonitors.ItemIndex >= Screen.MonitorCount) then
      Exit;

    Monitor := Screen.Monitors[Dialog.lstMonitors.ItemIndex];
    AOutputIndex := Cardinal(Monitor.MonitorNum);
    ADescription := Dialog.lstMonitors.Items[Dialog.lstMonitors.ItemIndex];
    Result := True;

  finally
    Dialog.Free();
  end;
end;


procedure TMfCastDesktopWorker.PostLog(const AText: string);
var
  LogMessage: TMfCastUiLogMessage;

begin

  LogMessage := TMfCastUiLogMessage.Create(AText);

  if not PostMessage(FNotifyWindow,
                     WM_MFCAST_LOG,
                     WPARAM(LogMessage),
                     0) then
    LogMessage.Free();
end;


procedure TMfCastDesktopWorker.Execute();
var
  Hr: HRESULT;
  HrCom: HRESULT;
  Capabilities: TMfCastCaptureCapabilities;
  CodecName: string;

  function StartCapture(): HRESULT;
    begin
      PostLog('Checking capture capabilities in the worker thread...');
      Result := FCast.GetCaptureCapabilities(FDevice,
                                             FSettings,
                                             Capabilities);
    if FAILED(Result) then
      begin
        PostLog('Capture capability check failed: ' + Capabilities.SelectionReason);
        Exit;
      end;

    FSettings.AudioOnly := Capabilities.AudioOnly;
    case Capabilities.SelectedCodec of
      cvcH264: CodecName := 'H.264';
      cvcHEVC: CodecName := 'HEVC';
    else
      CodecName := 'none';
    end;

    PostLog(Format('Capture mode: audioOnly=%s, codec=%s, hardware H.264=%s, hardware HEVC=%s, receiver HEVC=%s, audio=%s.',
                   [BoolToStr(FSettings.AudioOnly, True), CodecName,
                    BoolToStr(Capabilities.HardwareH264EncoderAvailable, True),
                    BoolToStr(Capabilities.HardwareHEVCEncoderAvailable, True),
                    BoolToStr(Capabilities.ReceiverSupportsHEVC, True),
                    BoolToStr(FSettings.CaptureSystemAudio, True)]));

    PostLog(Capabilities.SelectionReason);

    // Keep a running cast until the replacement's capability check succeeds.
    // Stop can wait for network/encoder cleanup, so it also belongs off the UI.
    if FCast.State in [csBuffering, csPlaying, csPaused] then
      begin
        PostLog('Stopping previous capture before changing mode...');
        Result := FCast.Stop();
        if FAILED(Result) then
          Exit;
        PostLog('Previous capture stopped; applying the selected capture mode.');
      end;

    PostLog('Starting desktop cast after capture capability check...');
    Result := FCast.CastDesktop(FDevice,
                                FSettings,
                                Capabilities);
  end;

begin

  Hr := E_FAIL;
  HrCom := CoInitializeEx(nil,
                          COINIT_MULTITHREADED);

  try
    try
      if SUCCEEDED(HrCom) then
        Hr := StartCapture()
      else
        begin
          Hr := HrCom;
          PostLog(Format('Desktop worker COM initialization failed: HRESULT $%.8x.',
                         [DWORD(Hr)]));
        end;

    except
      on E: Exception do
        begin
          PostLog('Desktop cast worker exception: ' + E.Message);
          Hr := E_FAIL;
        end;
    end;

  finally
    if SUCCEEDED(HrCom) then
      CoUninitialize();

    // Release the UI's busy state even when preparation raises an exception.
    PostMessage(FNotifyWindow,
                WM_MFCAST_FINISHED,
                WPARAM(Hr),
                0);
  end;
end;


procedure TCastPlayerForm.FormCreate(Sender: TObject);
begin

  FClosing := False;
  FActiveDesktopMode := -1;
  FDesktopCaptureExcluded := False;
  FUpdatingSeekPosition := False;
  FSeekPositionPending := False;
  FSeekTargetSeconds := -1;
  FSeekRequestTick := 0;
  ClearSubtitleChoices();
  ClearAudioTracks();
  HandleNeeded();

  // Enable the optional conversion stack used by MfPlayer X2 so containers
  // such as Matroska can be converted to fragmented MP4 for Chromecast.
  FCast := TMfCast.Create(True);
  FPreviewSubtitleTimer := TTimer.Create(Self);
  FPreviewSubtitleTimer.Enabled := False;
  FPreviewSubtitleTimer.Interval := 100;
  FPreviewSubtitleTimer.OnTimer := PreviewSubtitleTimer;
  FPreviewSubtitleTimer.Enabled := True;
  cbxCaptureCodec.ItemIndex := 0;
  cbxCaptureMode.ItemIndex := 0;
  pnlPreview.HandleNeeded();
  LogResult('Attach preview window',
             FCast.SetPreviewWindow(pnlPreview.Handle));

  FCast.OnDeviceAdded := DeviceChanged;
  FCast.OnDeviceUpdated := DeviceChanged;
  FCast.OnDeviceRemoved := DeviceRemoved;
  FCast.OnStateChanged := CastStateChanged;
  FCast.OnMediaStatus := CastMediaStatus;
  FCast.OnError := CastError;
  FCast.OnLog := CastLog;

  trkSeek.Max := 7200;
  trkVolume.Max := 100;
  trkVolume.Position := 0;
  memLog.Lines.Add('Ready. Choose a local file/URL, or cast the desktop with DXGI and WASAPI.');
  RegisterGlobalHotKeys();
  UpdateControls;
end;


procedure TCastPlayerForm.PreviewSubtitleTimer(Sender: TObject);
begin

  if not FClosing and Assigned(FCast) then
    FCast.UpdatePreviewSubtitles();
end;


procedure TCastPlayerForm.FormDestroy(Sender: TObject);
var
  message: TMsg;

begin

  FClosing := True;
  FPreviewSubtitleTimer.Enabled := False;
  UnregisterGlobalHotKeys();

  if Assigned(FCast) then
    begin
      FCast.SetPreviewWindow(0);
      FCast.OnDeviceAdded := nil;
      FCast.OnDeviceUpdated := nil;
      FCast.OnDeviceRemoved := nil;
      FCast.OnStateChanged := nil;
      FCast.OnMediaStatus := nil;
      FCast.OnError := nil;
      FCast.OnLog := nil;
    end;

  if Assigned(FWorker) then
    begin
      FWorker.WaitFor;
      FreeAndNil(FWorker);
    end;

  if Assigned(FSubtitleWorker) then
    begin
      FSubtitleWorker.WaitFor;
      FreeAndNil(FSubtitleWorker);
    end;

  if Assigned(FAudioTrackWorker) then
    begin
      FAudioTrackWorker.WaitFor;
      FreeAndNil(FAudioTrackWorker);
    end;

  while Peekmessage(message,
                    Handle,
                    WM_MFCAST_ERROR,
                    WM_MFCAST_ERROR,
                    PM_REMOVE) do
    TObject(message.wParam).Free();

  while Peekmessage(message,
                    Handle,
                    WM_MFCAST_LOG,
                    WM_MFCAST_LOG,
                    PM_REMOVE) do
    TObject(message.wParam).Free();

  FreeAndNil(FCast);
end;


procedure TCastPlayerForm.RegisterGlobalHotKeys();
const
  NoRepeatModifiers = MOD_CONTROL or MOD_ALT or MFCAST_MOD_NOREPEAT;
  RepeatModifiers = MOD_CONTROL or MOD_ALT;

  procedure RegisterOne(const AId: Integer;
                        const AModifiers: Cardinal;
                        const AKey: Cardinal;
                        const ADescription: string);
  begin
    if not RegisterHotKey(Handle, AId, AModifiers, AKey) then
      memLog.Lines.Add(Format('Global shortcut unavailable: %s (Windows error %d).',
                              [ADescription, GetLastError()]));
  end;

begin

  RegisterOne(MFCAST_HOTKEY_CAST,
              NoRepeatModifiers,
              Ord('C'),
              'Ctrl+Alt+C');

  RegisterOne(MFCAST_HOTKEY_CAST_DESKTOP,
              NoRepeatModifiers,
              Ord('D'),
              'Ctrl+Alt+D');

  RegisterOne(MFCAST_HOTKEY_PLAY,
              NoRepeatModifiers,
              VK_F5,
              'Ctrl+Alt+F5');

  RegisterOne(MFCAST_HOTKEY_PAUSE,
              NoRepeatModifiers,
              VK_F6,
              'Ctrl+Alt+F6');

  RegisterOne(MFCAST_HOTKEY_STOP,
              NoRepeatModifiers,
              VK_F7,
              'Ctrl+Alt+F7');

  RegisterOne(MFCAST_HOTKEY_VOLUME_UP,
              RepeatModifiers,
              VK_UP,
              'Ctrl+Alt+Up');

  RegisterOne(MFCAST_HOTKEY_VOLUME_DOWN,
              RepeatModifiers,
              VK_DOWN,
              'Ctrl+Alt+Down');

  RegisterOne(MFCAST_HOTKEY_MUTE,
              NoRepeatModifiers,
              Ord('M'),
              'Ctrl+Alt+M');

  memLog.Lines.Add('Global shortcuts: Cast Ctrl+Alt+C, desktop Ctrl+Alt+D, play/pause/stop Ctrl+Alt+F5/F6/F7, volume Ctrl+Alt+Up/Down, mute Ctrl+Alt+M.');
end;


procedure TCastPlayerForm.UnregisterGlobalHotKeys();
var
  HotKeyId: Integer;

begin

  for HotKeyId := MFCAST_HOTKEY_CAST to MFCAST_HOTKEY_MUTE do
    UnregisterHotKey(Handle, HotKeyId);
end;


procedure TCastPlayerForm.WmHotKey(var message: Tmessage);
const
  VolumeStep = 5;

var
  NewPosition: Integer;

begin

  case message.WParam of
    MFCAST_HOTKEY_CAST: if btnCast.Enabled then
                          btnCast.Click();

    MFCAST_HOTKEY_CAST_DESKTOP: if btnCastDesktop.Enabled then
                                  btnCastDesktop.Click();

    MFCAST_HOTKEY_PLAY: if btnPlay.Enabled then
                          btnPlay.Click();

    MFCAST_HOTKEY_PAUSE: if btnPause.Enabled then
                           btnPause.Click();

    MFCAST_HOTKEY_STOP: if btnStop.Enabled then
                          btnStop.Click();

    MFCAST_HOTKEY_VOLUME_UP: if trkVolume.Enabled then
                               begin
                                 NewPosition := trkVolume.Position + VolumeStep;

                                 if (NewPosition > trkVolume.Max) then
                                   NewPosition := trkVolume.Max;

                                 trkVolume.Position := NewPosition;
                               end;

    MFCAST_HOTKEY_VOLUME_DOWN: if trkVolume.Enabled then
                                 begin
                                   NewPosition := trkVolume.Position - VolumeStep;

                                   if (NewPosition < trkVolume.Min) then
                                     NewPosition := trkVolume.Min;

                                   trkVolume.Position := NewPosition;
                                 end;

    MFCAST_HOTKEY_MUTE: if chkMuted.Enabled then
                          begin
                            chkMuted.Checked := not chkMuted.Checked;
                          end;
  end;
end;


procedure TCastPlayerForm.DeviceChanged(const ADevice: TMfCastDevice);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TCastPlayerForm.DeviceRemoved(const ADeviceId: string);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TCastPlayerForm.CastStateChanged(const AOldState: TMfCastState;
                                           const ANewState: TMfCastState);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_STATE,
                Ord(ANewState),
                0);
end;


procedure TCastPlayerForm.CastMediaStatus(const AStatus: TMfCastMediaStatus);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_STATUS,
                WPARAM(AStatus.CurrentTime100ns div 10000000),
                LPARAM(AStatus.Duration100ns div 10000000));
end;


procedure TCastPlayerForm.CastError(const AError: TMfCastErrorInfo);
var
  Text: string;

begin

  Text := Format('[ERROR][%s] %s (HRESULT $%.8x)',
                 [AError.Stage, AError.messageText, DWORD(AError.HResult)]);

  if (AError.Detail <> '') then
    Text := Text + ' Detail: ' + AError.Detail;

  PostLogmessage(WM_MFCAST_ERROR,
                 Text);
end;


procedure TCastPlayerForm.CastLog(Sender: TObject;
                                        const ALevel: TMfCastLogLevel;
                                        const ASource: string;
                                        const Amessage: string);
begin

  // Capture workers can report a failure only through the logger. Dropping
  // cllError here hides the original HRESULT before the live buffer closes,
  // leaving only the receiver's subsequent media-network error in the UI.
  PostLogmessage(WM_MFCAST_LOG,
                 Format('[%s][%s] %s',
                        [MfCastLogLevelToString(ALevel), ASource, Amessage]));
end;


procedure TCastPlayerForm.PostLogmessage(const AWindowsmessage: Cardinal;
                                               const AText: string);
var
  Logmessage: TMfCastUiLogMessage;

begin

  if FClosing then
    Exit;

  Logmessage := TMfCastUiLogMessage.Create(AText);
  if not Postmessage(Handle,
                     AWindowsmessage,
                     WPARAM(Logmessage),
                     0) then
    Logmessage.Free();
end;


procedure TCastPlayerForm.ReloadDevices;
var
  I: Integer;
  SelectedIndex: Integer;
  SelectedId: string;
  SelectedAddress: string;
  Hr: HRESULT;

begin

  SelectedId := '';
  SelectedAddress := '';

  if (cbxDevices.ItemIndex >= 0) and (cbxDevices.ItemIndex < Length(FDevices)) then
    begin
      SelectedId := FDevices[cbxDevices.ItemIndex].Id;
      SelectedAddress := FDevices[cbxDevices.ItemIndex].Address;
    end;

  Hr := FCast.GetDevices(FDevices);
  if FAILED(Hr) then
    begin
      LogResult('Read devices',
                Hr);
      Exit;
    end;

  cbxDevices.Items.BeginUpdate();

  try
    cbxDevices.Clear;

    SelectedIndex := -1;

    for I := 0 to Length(FDevices) - 1 do
      begin
        cbxDevices.Items.Add(Format('%s [%s]  (%s:%d)',
                                    [FDevices[I].FriendlyName,
                                     FDevices[I].ModelName,
                                     FDevices[I].Address,
                                     FDevices[I].Port]));

        // Discovery response order is nondeterministic. Preserve the actual
        // selected device, never merely its old list position.
        if (SelectedIndex < 0) and
           (((SelectedId <> '') and
             SameText(FDevices[I].Id, SelectedId)) or
            ((SelectedId = '') and
             (SelectedAddress <> '') and
             SameText(FDevices[I].Address,
                      SelectedAddress))) then
          SelectedIndex := I;
      end;

    if (SelectedIndex >= 0) then
      cbxDevices.ItemIndex := SelectedIndex
    else
      if (cbxDevices.Items.Count > 0) then
        cbxDevices.ItemIndex := 0;

  finally
    cbxDevices.Items.EndUpdate();
  end;

  UpdateControls();
end;


procedure TCastPlayerForm.ClearSubtitleChoices();
begin

  SetLength(FSubtitleChoices, 0);
  FSubtitleSourceName := '';
  cbxSubtitleLanguage.Items.Clear();
  cbxSubtitleLanguage.ItemIndex := -1;
end;


procedure TCastPlayerForm.AddSubtitleChoice(const AChoice: TMfCastSubtitleChoice);
var
  ChoiceLanguage: string;
  I: Integer;
  NewIndex: Integer;

begin

  ChoiceLanguage := TLanguageTags.NormalizeLanguageTag(AChoice.Language);

  if (ChoiceLanguage <> '') then
    for I := Low(FSubtitleChoices) to High(FSubtitleChoices) do
      if SameText(TLanguageTags.NormalizeLanguageTag(FSubtitleChoices[I].Language),
                  ChoiceLanguage) then
        Exit;

  NewIndex := Length(FSubtitleChoices);

  SetLength(FSubtitleChoices,
            NewIndex + 1);

  FSubtitleChoices[NewIndex] := AChoice;
  cbxSubtitleLanguage.Items.Add(AChoice.DisplayName);
end;


procedure TCastPlayerForm.RefreshSubtitleChoices(const ASourceName: string);
const
  SUBTITLE_EXTENSIONS: array[0..3] of string = ('.srt',
                                                 '.vtt',
                                                 '.sub',
                                                 '.idx');
var
  Choice: TMfCastSubtitleChoice;
  EmbeddedTracks: TMfEmbeddedSubtitleTrackInfoArray;
  EmbeddedTrackNumber: Integer;
  ExtensionIndex: Integer;
  FriendlyName: string;
  I: Integer;
  LanguageTags: TLanguageTags;
  Sidecars: TTxtPropArray;
  SourceName: string;

begin

  SourceName := Trim(ASourceName);
  cbxSubtitleLanguage.Items.BeginUpdate();

  try
    ClearSubtitleChoices();
    FSubtitleSourceName := SourceName;

    if (SourceName = '') or (not FileExists(SourceName)) then
      Exit;

    memLog.Lines.Add(Format('[DEBUG][Subtitles] Scanning source="%s".',
                            [SourceName]));

    // Sidecars are deliberately inserted first. AddSubtitleChoice suppresses
    // later duplicates by language, so an embedded track cannot replace one.
    LanguageTags := TLanguageTags.Create();
    try
      for ExtensionIndex := Low(SUBTITLE_EXTENSIONS) to High(SUBTITLE_EXTENSIONS) do
        begin
          Sidecars := LanguageTags.ReadFileTags(SourceName,
                                                '',
                                                0,
                                                SUBTITLE_EXTENSIONS[ExtensionIndex]);

          for I := Low(Sidecars) to High(Sidecars) do
            if Sidecars[I].sTTxtType <> UNKNOWN then
              begin
                Choice.Source := cscsSidecar;
                Choice.Language := Sidecars[I].sLanguageTag;
                Choice.SourceName := Sidecars[I].sFile;

                Choice.StreamIndex := MfCastStablePathIndex(Choice.SourceName);
                Choice.HasStreamIndex := False;

                Choice.TrackId := MfCastMakeTrackId(ctkSubtitle,
                                                    ctsSidecar,
                                                    Choice.StreamIndex);

                FriendlyName := Trim(Sidecars[I].sFriendlyLanguageName);

                if FriendlyName = '' then
                  FriendlyName := Trim(Choice.Language);

                if FriendlyName = '' then
                  FriendlyName := ExtractFileName(Choice.SourceName);

                Choice.DisplayName := FriendlyName + ' (sidecar)';
                AddSubtitleChoice(Choice);
              end
            else
              memLog.Lines.Add(Format('[INFO][Subtitles] Unsupported bitmap or unknown subtitle sidecar: "%s".',
                                      [ExtractFileName(Sidecars[I].sFile)]));

          SetLength(Sidecars, 0);
        end;

      SetLength(EmbeddedTracks, 0);
      EmbeddedTrackNumber := 0;

      if SUCCEEDED(TMfEmbeddedSubtitleReader.EnumerateTracks(SourceName,
                                                             EmbeddedTracks)) then
        for I := Low(EmbeddedTracks) to High(EmbeddedTracks) do
          begin
            memLog.Lines.Add(Format('[DEBUG][Subtitles] Candidate stream=%d language="%s" name="%s" codec="%s" supported=%s.',
                                    [EmbeddedTracks[I].StreamIndex,
                                     EmbeddedTracks[I].Language,
                                     EmbeddedTracks[I].Name,
                                     EmbeddedTracks[I].CodecId,
                                     BoolToStr(EmbeddedTracks[I].Supported, True)]));

            if EmbeddedTracks[I].Supported then
              begin
                Inc(EmbeddedTrackNumber);
                Choice.Source := cscsEmbedded;
                Choice.Language := EmbeddedTracks[I].Language;
                Choice.SourceName := SourceName;
                Choice.StreamIndex := EmbeddedTracks[I].StreamIndex;
                Choice.HasStreamIndex := True;

                if EmbeddedTracks[I].Source = essMatroska then
                  Choice.TrackId := MfCastMakeTrackId(ctkSubtitle,
                                                       ctsMatroska,
                                                       Choice.StreamIndex)
                else
                  Choice.TrackId := MfCastMakeTrackId(ctkSubtitle,
                                                      ctsMediaFoundation,
                                                      Choice.StreamIndex);

                Choice.Language := TLanguageTags.NormalizeLanguageTag(Choice.Language);

                memLog.Lines.Add(Format('[DEBUG][Subtitles] Embedded track %d: stream=%d language="%s" name="%s".',
                                        [EmbeddedTrackNumber,
                                         EmbeddedTracks[I].StreamIndex,
                                         Choice.Language,
                                         EmbeddedTracks[I].Name]));

                FriendlyName := '';

                if (Choice.Language <> '') then
                  FriendlyName := LanguageTags.GetLangOrCountryFromTag(Choice.Language);

                if (FriendlyName = '') then
                  FriendlyName := Trim(EmbeddedTracks[I].Name);

                if (FriendlyName = '') then
                  FriendlyName := Trim(Choice.Language);

                if (FriendlyName = '') then
                  FriendlyName := 'Unknown language';

                Choice.DisplayName := Format('Track %d %s (embedded)',
                                             [EmbeddedTrackNumber, FriendlyName]);
                AddSubtitleChoice(Choice);
              end;
          end;

    finally
      LanguageTags.Free();
      SetLength(Sidecars, 0);
      SetLength(EmbeddedTracks, 0);
    end;

    if (cbxSubtitleLanguage.Items.Count > 0) then
      cbxSubtitleLanguage.ItemIndex := 0;

  finally
    cbxSubtitleLanguage.Items.EndUpdate();
  end;

  memLog.Lines.Add(Format('Subtitle languages found: %d.',
                          [cbxSubtitleLanguage.Items.Count]));
  UpdateControls();
end;


procedure TCastPlayerForm.BuildSelectedSubtitle(out ASubtitle: TMfCastSubtitleAsset);
var
  Choice: TMfCastSubtitleChoice;

begin

  ASubtitle.Reset();

  if chkEmbeddedSubtitles.Checked and (cbxSubtitleLanguage.ItemIndex >= 0) and (cbxSubtitleLanguage.ItemIndex < Length(FSubtitleChoices)) then
    begin
      Choice := FSubtitleChoices[cbxSubtitleLanguage.ItemIndex];
      ASubtitle.Enabled := True;
      ASubtitle.Embedded := Choice.Source = cscsEmbedded;
      ASubtitle.Name := Choice.DisplayName;
      ASubtitle.Language := Choice.Language;
      ASubtitle.SourceName := Choice.SourceName;
      ASubtitle.TrackId := Choice.TrackId;
      ASubtitle.StreamIndex := Choice.StreamIndex;
      ASubtitle.HasStreamIndex := Choice.HasStreamIndex;
    end;
end;


procedure TCastPlayerForm.ClearAudioTracks();
begin

  SetLength(FAudioTracks, 0);
  FAudioSourceName := '';
  cbxAudioStream.Items.Clear();
  cbxAudioStream.ItemIndex := -1;
end;


procedure TCastPlayerForm.RefreshAudioTracks(const ASourceName: string);
var
  AllTracks: TMfCastTrackInfoArray;
  DisplayName: string;
  Hr: HRESULT;
  I: Integer;
  NewIndex: Integer;
  SelectedIndex: Integer;
  SourceName: string;

begin

  SourceName := Trim(ASourceName);
  cbxAudioStream.Items.BeginUpdate();
  try
    ClearAudioTracks();
    FAudioSourceName := SourceName;
    if (SourceName = '') or (not FileExists(SourceName)) then
      Exit;

    Hr := FCast.GetMediaTracks(SourceName,
                               AllTracks);
    if FAILED(Hr) then
      begin
        memLog.Lines.Add(Format('Audio track scan failed: HRESULT $%.8x',
                                [DWORD(Hr)]));
        Exit;
      end;

    SelectedIndex := -1;
    for I := Low(AllTracks) to High(AllTracks) do
      if AllTracks[I].Kind = ctkAudio then
        begin
          NewIndex := Length(FAudioTracks);
          SetLength(FAudioTracks, NewIndex + 1);
          FAudioTracks[NewIndex] := AllTracks[I];

          DisplayName := Trim(AllTracks[I].Name);
          if DisplayName = '' then
            DisplayName := Format('Audio track %d', [NewIndex + 1]);

          if Trim(AllTracks[I].Language) <> '' then
            DisplayName := DisplayName + ' [' + AllTracks[I].Language + ']';

          if not AllTracks[I].Supported then
            DisplayName := DisplayName + ' (decoder required)';

          cbxAudioStream.Items.Add(DisplayName);

          if AllTracks[I].Selected and (SelectedIndex < 0) then
            SelectedIndex := NewIndex;
        end;

    if (SelectedIndex < 0) and (cbxAudioStream.Items.Count > 0) then
      SelectedIndex := 0;

    cbxAudioStream.ItemIndex := SelectedIndex;

  finally
    cbxAudioStream.Items.EndUpdate();
    SetLength(AllTracks, 0);
  end;

  memLog.Lines.Add(Format('Audio tracks found: %d.',
                          [cbxAudioStream.Items.Count]));
end;


function TCastPlayerForm.IsDtsAudioTrack(const ATrack: TMfCastTrackInfo): Boolean;
begin

  Result := (Pos('DTS',
                 UpperCase(ATrack.Name)) > 0) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_RAW)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_HD)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_XLL)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_LBR)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_UHD)) or
            SameText(ATrack.SubType,
                     GUIDToString(MFAudioFormat_DTS_UHDY));
end;


function TCastPlayerForm.EnsureSelectedAudioTrackSupported(): Boolean;
var
  I: Integer;
  Track: TMfCastTrackInfo;

begin

  Result := True;

  if (cbxAudioStream.ItemIndex < 0) or
     (cbxAudioStream.ItemIndex >= Length(FAudioTracks)) then
    Exit;

  Track := FAudioTracks[cbxAudioStream.ItemIndex];
  if Track.Supported then
    Exit;

  Result := False;

  if IsDtsAudioTrack(Track) then
    MessageDlg('The selected DTS audio stream cannot be decoded.' + sLineBreak +
               sLineBreak +
               'Windows does not include a native DTS decoder. Install a ' +
               'compatible third-party Media Foundation decoder and reload ' +
               'the file, or select another supported audio stream.',
               mtWarning,
               [mbOK],
               0)
  else
    MessageDlg('The selected audio stream cannot be decoded by Media ' +
               'Foundation. Install a compatible third-party decoder and ' +
               'reload the file, or select another supported audio stream.',
               mtWarning,
               [mbOK],
               0);

  FUpdatingAudioSelection := True;

  try
    cbxAudioStream.ItemIndex := -1;
    for I := Low(FAudioTracks) to High(FAudioTracks) do
      if FAudioTracks[I].Selected and FAudioTracks[I].Supported then
        begin
          cbxAudioStream.ItemIndex := I;
          Break;
        end;

  finally
    FUpdatingAudioSelection := False;
  end;

  if cbxAudioStream.CanFocus then
    cbxAudioStream.SetFocus();
  cbxAudioStream.DroppedDown := True;
end;


procedure TCastPlayerForm.ApplyAudioSelection();
begin

  if not Assigned(FCast) or Assigned(FWorker) or
     Assigned(FSubtitleWorker) or Assigned(FAudioTrackWorker) or
     (cbxAudioStream.ItemIndex < 0) or
     (cbxAudioStream.ItemIndex >= Length(FAudioTracks)) or
     (not (FCast.State in [csBuffering, csPlaying, csPaused])) then
    Exit;

  if not EnsureSelectedAudioTrackSupported() then
    Exit;

  FAudioTrackWorker := TMfCastAudioTrackWorker.Create(FCast,
                                                      FAudioTracks[cbxAudioStream.ItemIndex].TrackId,
                                                      Handle);

  FAudioTrackWorker.Start();
  memLog.Lines.Add('Switching audio track in a worker thread...');
  UpdateControls();
end;


procedure TCastPlayerForm.ApplySubtitleSelection();
var
  Subtitle: TMfCastSubtitleAsset;

begin

  if (not Assigned(FCast)) or Assigned(FWorker) or
     Assigned(FSubtitleWorker) or
     (not (FCast.State in [csBuffering, csPlaying, csPaused])) then
    Exit;

  BuildSelectedSubtitle(Subtitle);
  FSubtitleWorker := TMfCastSubtitleWorker.Create(FCast,
                                                  Subtitle,
                                                  Subtitle.Enabled,
                                                  Handle);
  FSubtitleWorker.Start();
  if Subtitle.Enabled then
    memLog.Lines.Add('Switching subtitles in a worker thread...')
  else
    memLog.Lines.Add('Disabling subtitles in a worker thread...');

  UpdateControls();
end;


procedure TCastPlayerForm.LogResult(const AOperation: string;
                                    const AHResult: HRESULT);
begin

  if FAILED(AHResult) then
    memLog.Lines.Add(Format('%s failed: HRESULT $%.8x',
                            [AOperation, DWORD(AHResult)]))
  else
    memLog.Lines.Add(AOperation + ' requested.');
end;


function TCastPlayerForm.SetDesktopCaptureExcluded(const AExcluded: Boolean): HRESULT;
const
  // The Delphi's Windows headers predate Windows 10 version 2004.
  WDA_EXCLUDEFROMCAPTURE = $00000011;

var
  Affinity: DWORD;

begin

  Result := S_OK;

  if (FDesktopCaptureExcluded = AExcluded) then
    Exit;

  if AExcluded then
    begin
      if not GetWindowDisplayAffinity(Handle,
                                      FPreviousDisplayAffinity) then
        begin
          Result := WinApi.WinError.HRESULT_FROM_WIN32(GetLastError());

          if SUCCEEDED(Result) then
            Result := E_FAIL;
          Exit;
        end;

      Affinity := WDA_EXCLUDEFROMCAPTURE;
    end
  else
    Affinity := FPreviousDisplayAffinity;

  // Keep the preview attached to its panel. Exclude the top-level player
  // window from desktop capture so its live preview cannot capture itself.
  if not SetWindowDisplayAffinity(Handle,
                                  Affinity) then
    begin
      Result := WinApi.WinError.HRESULT_FROM_WIN32(GetLastError());

      if SUCCEEDED(Result) then
        Result := E_FAIL;
      Exit;
    end;

  FDesktopCaptureExcluded := AExcluded;
end;


procedure TCastPlayerForm.UpdateControls();
var
  Busy: Boolean;
  CanControlReceiver: Boolean;
  State: TMfCastState;

begin

  Busy := Assigned(FWorker) or Assigned(FSubtitleWorker) or
          Assigned(FAudioTrackWorker);

  if Assigned(FCast) then
    State := FCast.State
  else
    State := csIdle;

  if FDesktopCaptureExcluded and Assigned(FCast) and (not Busy) and (State in [csIdle, csStopped, csError]) then
    LogResult('Restore player capture visibility', SetDesktopCaptureExcluded(False));

  CanControlReceiver := Assigned(FCast) and (not Busy) and (State in [csConnected, csLaunchingReceiver, csBuffering, csPlaying, csPaused, csStopped]);

  btnCast.Enabled := (cbxDevices.ItemIndex >= 0) and
                     (Trim(edtSource.Text) <> '') and
                     (not Busy) and
                      Assigned(FCast) and (State in [csIdle, csDiscovering, csStopped, csError]);

  btnCastDesktop.Enabled := (cbxDevices.ItemIndex >= 0) and
                             (not Busy) and Assigned(FCast) and
                             (State in [csIdle, csDiscovering, csStopped, csError]);

  // STOP ends the receiver's media session, so PLAY cannot restart it.  A
  // stopped source must be cast again.  PLAY is only a resume operation for
  // an existing paused session.
  btnPlay.Enabled := CanControlReceiver and (State = csPaused);
  btnPause.Enabled := CanControlReceiver and (State = csPlaying);
  btnStop.Enabled := CanControlReceiver and (State in [csBuffering, csPlaying, csPaused]);

  btnDisconnect.Enabled := CanControlReceiver;

  btnSeek.Enabled := CanControlReceiver and (State in [csPlaying, csPaused]);
  trkSeek.Enabled := btnSeek.Enabled;
  trkVolume.Enabled := CanControlReceiver;
  chkMuted.Enabled := CanControlReceiver;
  chkEmbeddedSubtitles.Enabled := not Busy;

  cbxSubtitleLanguage.Enabled := chkEmbeddedSubtitles.Checked and
                                 (cbxSubtitleLanguage.Items.Count > 0) and
                                 (not Assigned(FWorker)) and
                                 (not Assigned(FSubtitleWorker));

  cbxAudioStream.Enabled := (cbxAudioStream.Items.Count > 0) and (not Busy);

  cbxCaptureCodec.Enabled := (not Busy) and (cbxCaptureMode.ItemIndex <> 1);
  cbxCaptureMode.Enabled := not Busy;
end;


procedure TCastPlayerForm.WmCastDevices(var message: Tmessage);
begin

  ReloadDevices();
end;


procedure TCastPlayerForm.WmCastState(var message: Tmessage);
var
  State: TMfCastState;

begin

  State := TMfCastState(message.WParam);
  lblState.Caption := 'State: ' + MfCastStateToString(State);

  if State in [csIdle, csStopped, csError] then
    begin
      FSeekPositionPending := False;
      FSeekTargetSeconds := -1;
      FSeekRequestTick := 0;
    end;

  UpdateControls;
end;


procedure TCastPlayerForm.WmCastStatus(var message: Tmessage);
var
  PositionSeconds: Integer;
  DurationSeconds: Integer;

begin

  PositionSeconds := Integer(message.WParam);
  DurationSeconds := Integer(message.LParam);
  FUpdatingSeekPosition := True;

  try
    if (DurationSeconds > trkSeek.Max) then
      trkSeek.Max := DurationSeconds;

  finally
    FUpdatingSeekPosition := False;
  end;

  if FSeekPositionPending then
    begin
      if (FSeekTargetSeconds >= 0) and
         (Abs(PositionSeconds - FSeekTargetSeconds) <= 2) then
        begin
          FSeekPositionPending := False;
          FSeekTargetSeconds := -1;
          FSeekRequestTick := 0;
        end
      else
        if (FSeekRequestTick = 0) or
           ((GetTickCount() - FSeekRequestTick) < 20000) then
          Exit
        else
          begin
            FSeekPositionPending := False;
            FSeekTargetSeconds := -1;
            FSeekRequestTick := 0;
          end;
    end;

  if (PositionSeconds > trkSeek.Max) then
    PositionSeconds := trkSeek.Max;

  if (PositionSeconds >= trkSeek.Min) then
    begin
      FUpdatingSeekPosition := True;

      try
        trkSeek.Position := PositionSeconds;

      finally
        FUpdatingSeekPosition := False;
      end;
    end;
end;


procedure TCastPlayerForm.WmCastError(var message: Tmessage);
var
  Logmessage: TMfCastUiLogMessage;

begin

  Logmessage := TMfCastUiLogMessage(message.WParam);

  try
    memLog.Lines.Add(Logmessage.Text);
    UpdateControls();

  finally
    Logmessage.Free();
  end;
end;


procedure TCastPlayerForm.WmCastFinished(var message: Tmessage);
var
  Hr: HRESULT;

begin

  if Assigned(FWorker) then
    begin
      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;

  Hr := HRESULT(message.WParam);
  if FAILED(Hr) then
    FActiveDesktopMode := -1;

  if FAILED(Hr) and FDesktopCaptureExcluded then
    LogResult('Restore player capture visibility',
              SetDesktopCaptureExcluded(False));

  LogResult('Cast media',
            Hr);

  if SUCCEEDED(Hr) then
    begin

      // Apply the UI settings to a receiver that may have retained a muted
      // state from an earlier session.
      FCast.SetVolume(trkVolume.Position / 100.0);
      FCast.SetMuted(chkMuted.Checked);

      // Also pick up an artwork selection made while the cast worker connected.
      if FileExists(Trim(edtArtwork.Text)) then
        LogResult('Apply artwork',
                  FCast.SetAudioArtwork(Trim(edtArtwork.Text)));
    end;

  UpdateControls;
end;


procedure TCastPlayerForm.WmCastLog(var message: Tmessage);
var
  Logmessage: TMfCastUiLogMessage;

begin

  Logmessage := TMfCastUiLogMessage(message.WParam);

  try
    memLog.Lines.Add(Logmessage.Text);

  finally
    Logmessage.Free();
  end;
end;


procedure TCastPlayerForm.WmCastSubtitleFinished(var message: Tmessage);
begin

  if Assigned(FSubtitleWorker) then
    begin
      FSubtitleWorker.WaitFor();
      FreeAndNil(FSubtitleWorker);
    end;

  if Boolean(message.LParam) then
    LogResult('Subtitle switch',
              HRESULT(message.WParam))
  else
    LogResult('Disable subtitles',
              HRESULT(message.WParam));
  UpdateControls();
end;


procedure TCastPlayerForm.WmCastAudioTrackFinished(var message: Tmessage);
var
  I: Integer;

begin

  if Assigned(FAudioTrackWorker) then
    begin
      FAudioTrackWorker.WaitFor();
      FreeAndNil(FAudioTrackWorker);
    end;

  LogResult('Audio track switch',
            HRESULT(message.WParam));

  if SUCCEEDED(HRESULT(message.WParam)) then
    for I := Low(FAudioTracks) to High(FAudioTracks) do
      FAudioTracks[I].Selected := I = cbxAudioStream.ItemIndex;

  UpdateControls();
end;


procedure TCastPlayerForm.btnDiscoverClick(Sender: TObject);
begin

  LogResult('Discovery',
            FCast.Discover);
end;


procedure TCastPlayerForm.btnRefreshClick(Sender: TObject);
begin

  LogResult('Discovery refresh',
            FCast.RefreshDiscovery);
end;


function TCastPlayerForm.SelectLocalFile(const AArtwork: Boolean;
                                         out AFileName: string): Boolean;
const
  MediaFilter = 'Cast media|*.mp4;*.m4v;*.m4a;*.webm;*.mp3;*.aac;*.flac;*.wav;*.ogg;*.oga;*.opus;*.mkv;*.avi|All files|*.*';
  ArtworkFilter = 'Picture files|*.jpg;*.jpeg;*.png;*.bmp;*.gif|All files|*.*';

var
  Browser: TLWFileBrowserExDlg;
  CurrentFile: string;
  InitialDirectory: string;

begin

  AFileName := '';

  if AArtwork then
    CurrentFile := Trim(edtArtwork.Text)
  else
    CurrentFile := Trim(edtSource.Text);

  InitialDirectory := '';

  if (CurrentFile <> '') and (Pos('://', CurrentFile) = 0) then
    InitialDirectory := ExtractFilePath(CurrentFile);

  if not DirectoryExists(InitialDirectory) then
    if AArtwork then
      InitialDirectory := FArtworkBrowseDirectory
    else
      InitialDirectory := FMediaBrowseDirectory;

  Browser := TLWFileBrowserExDlg.Create(Self);

  try
    if AArtwork then
      begin
        Browser.FileFilter := fbxGraphics;
        Browser.Caption := 'Select artwork';
        Browser.cbxFileFilter.Filter := ArtworkFilter;
      end
    else
      begin
        Browser.FileFilter := fbxAudio;
        Browser.Caption := 'Select a media file';
        Browser.cbxFileFilter.Filter := MediaFilter;
      end;

    Browser.SetInitialDirectory(InitialDirectory);
    Result := Browser.ShowModal() = mrOk;

    if Result then
      AFileName := Browser.FileURI;

    if AArtwork then
      FArtworkBrowseDirectory := Browser.flbFiles.Directory
    else
      FMediaBrowseDirectory := Browser.flbFiles.Directory;

  finally
    Browser.Free();
  end;
end;


procedure TCastPlayerForm.btnBrowseClick(Sender: TObject);
var
  FileName: string;

begin

  if SelectLocalFile(False,
                     FileName) then
    begin
      edtSource.Text := FileName;
      RefreshSubtitleChoices(edtSource.Text);
      RefreshAudioTracks(edtSource.Text);
    end;

  UpdateControls();
end;


procedure TCastPlayerForm.btnBrowseArtworkClick(Sender: TObject);
var
  FileName: string;

begin

  if SelectLocalFile(True,
                     FileName) then
    edtArtwork.Text := FileName;
end;


procedure TCastPlayerForm.btnCastClick(Sender: TObject);
var
  Subtitle: TMfCastSubtitleAsset;
  Hr: HRESULT;

begin

  if (cbxDevices.ItemIndex < 0) or
     (cbxDevices.ItemIndex >= Length(FDevices)) or
     (Trim(edtSource.Text) = '') then
    Exit;

  if not SameText(FSubtitleSourceName,
                  Trim(edtSource.Text)) then
    RefreshSubtitleChoices(edtSource.Text);

  if not SameText(FAudioSourceName,
                  Trim(edtSource.Text)) then
    RefreshAudioTracks(edtSource.Text);

  BuildSelectedSubtitle(Subtitle);

  if (cbxAudioStream.ItemIndex >= 0) and (cbxAudioStream.ItemIndex < Length(FAudioTracks)) then
    begin
      if not EnsureSelectedAudioTrackSupported() then
        Exit;

      Hr := FCast.SelectAudioTrack(FAudioTracks[cbxAudioStream.ItemIndex].TrackId);
      if FAILED(Hr) then
        begin
          LogResult('Queue audio track', Hr);
          Exit;
        end;
    end;

  Hr := FCast.SetAudioArtwork(Trim(edtArtwork.Text));
  if FAILED(Hr) then
    begin
      LogResult('Set audio artwork',
                Hr);
      Exit;
    end;

  FActiveDesktopMode := -1;
  FWorker := TMfCastFileWorker.Create(FCast,
                                      FDevices[cbxDevices.ItemIndex],
                                      Trim(edtSource.Text),
                                      Subtitle,
                                      Handle);
  FWorker.Start();
  memLog.Lines.Add('Preparing and connecting in a worker thread...');
  UpdateControls;
end;


procedure TCastPlayerForm.CaptureModeChanged(Sender: TObject);
var
  PreviousMode: Integer;

begin

  if Assigned(FCast) and (not Assigned(FWorker)) and
     (not Assigned(FSubtitleWorker)) and
     (not Assigned(FAudioTrackWorker)) and
     (FActiveDesktopMode >= 0) and
     (cbxCaptureMode.ItemIndex <> FActiveDesktopMode) and
     (FCast.State in [csBuffering, csPlaying, csPaused]) then
    begin
      PreviousMode := FActiveDesktopMode;
      // Reuse the normal screen/artwork selection and capability checks.
      // They finish before the existing stream is stopped.
      btnCastDesktopClick(Sender);

      if not Assigned(FWorker) then
        cbxCaptureMode.ItemIndex := PreviousMode;
    end;

  UpdateControls();
end;


procedure TCastPlayerForm.btnCastDesktopClick(Sender: TObject);
var
  Settings: TMfCastCaptureSettings;
  MonitorDescription: string;
  ArtworkFileName: string;
  Hr: HRESULT;

begin
  if (cbxDevices.ItemIndex < 0) or (cbxDevices.ItemIndex >= Length(FDevices)) or Assigned(FWorker) then
    Exit;

  Settings.Reset();
  Settings.AudioOnly := cbxCaptureMode.ItemIndex = 1;
  Settings.CaptureSystemAudio := (cbxCaptureMode.ItemIndex <> 2);

  if (cbxCaptureMode.ItemIndex = 3) then
    begin
      if (Trim(edtArtwork.Text) = '') then
        begin
          if not SelectLocalFile(True,
                                 ArtworkFileName) then
            Exit;
          edtArtwork.Text := ArtworkFileName;
        end;

      Settings.ArtworkSourceName := Trim(edtArtwork.Text);

      if not FileExists(Settings.ArtworkSourceName) then
        begin
          MessageDlg('The artwork file does not exist. Please choose an artwork file.',
                     mtError,
                     [mbOK],
                     0);
          Exit;
        end;

      Settings.Width := 1280;
      Settings.Height := 720;
      Settings.FrameRateNumerator := 10;
      Settings.FrameRateDenominator := 1;
      Settings.VideoBitrate := 1000000;
    end;

  if (not Settings.CaptureSystemAudio) and MfCastDeviceIsAudioOnly(FDevices[cbxDevices.ItemIndex]) then
    begin
      memLog.Lines.Add(
        'Video-only casting requires a video-capable receiver. Select another receiver or capture mode.');
      Exit;
    end;

  memLog.Lines.Add(Format('Desktop target: name="%s", model="%s", address=%s:%d, capabilities=%d.',
                          [FDevices[cbxDevices.ItemIndex].FriendlyName,
                           FDevices[cbxDevices.ItemIndex].ModelName,
                           FDevices[cbxDevices.ItemIndex].Address,
                           FDevices[cbxDevices.ItemIndex].Port,
                           FDevices[cbxDevices.ItemIndex].RawCapabilities]));

  case cbxCaptureCodec.ItemIndex of
    1: Settings.CodecPreference := cvcpH264;
    2: Settings.CodecPreference := cvcpHEVC;
  else
    Settings.CodecPreference := cvcpAutomatic;
  end;

  // Ask for the desktop source before probing encoders or starting capture.
  if (not Settings.AudioOnly) and (Settings.ArtworkSourceName = '') and (not MfCastDeviceIsAudioOnly(FDevices[cbxDevices.ItemIndex])) then
    begin
      if not SelectCaptureMonitor(Settings.OutputIndex,
                                  MonitorDescription) then
        begin
          memLog.Lines.Add('Desktop casting cancelled: no screen was selected.');
          Exit;
        end;
    end;

  if Settings.AudioOnly then
    memLog.Lines.Add('Audio source: default Windows playback device (WASAPI loopback); DXGI is not used.')
  else
    if (Settings.ArtworkSourceName <> '') then
      memLog.Lines.Add('Artwork source: ' + Settings.ArtworkSourceName + '; system audio uses WASAPI loopback.')
    else
      memLog.Lines.Add(Format('Desktop source: %s (DXGI output %d).',
                              [MonitorDescription, Settings.OutputIndex]));

  Hr := SetDesktopCaptureExcluded((not Settings.AudioOnly) and (Settings.ArtworkSourceName = ''));
  if FAILED(Hr) then
    begin
      LogResult('Exclude player from desktop capture',
                Hr);
      Exit;
    end;

  if (not Settings.AudioOnly) and (Settings.ArtworkSourceName = '') then
    memLog.Lines.Add('Local preview enabled; player window excluded from desktop capture to prevent feedback.');

  // Every new desktop cast starts silent; raise the slider after connecting.
  trkVolume.Position := 0;

  FActiveDesktopMode := cbxCaptureMode.ItemIndex;
  FWorker := TMfCastDesktopWorker.Create(FCast,
                                         FDevices[cbxDevices.ItemIndex],
                                         Settings,
                                         Handle);

  FWorker.Start();

  if Settings.AudioOnly then
    memLog.Lines.Add('Starting WASAPI audio-only casting in a worker thread...')
  else
    if Settings.ArtworkSourceName <> '' then
      memLog.Lines.Add('Starting system audio with artwork in a worker thread...')
    else
      if not Settings.CaptureSystemAudio then
        memLog.Lines.Add('Starting DXGI video-only desktop casting in a worker thread...')
      else
        memLog.Lines.Add('Starting DXGI/WASAPI desktop casting in a worker thread...');

  UpdateControls();
end;


procedure TCastPlayerForm.btnPlayClick(Sender: TObject);
var
  Hr: HRESULT;

begin

  Hr := FCast.Play;
  LogResult('Play',
            Hr);
end;


procedure TCastPlayerForm.btnPauseClick(Sender: TObject);
begin

  LogResult('Pause',
            FCast.Pause);
end;


procedure TCastPlayerForm.btnStopClick(Sender: TObject);
begin

  LogResult('Stop',
            FCast.Stop);
end;


procedure TCastPlayerForm.btnDisconnectClick(Sender: TObject);
begin

  if not Assigned(FCast) or Assigned(FWorker) or Assigned(FSubtitleWorker) or Assigned(FAudioTrackWorker) then
    Exit;

  FWorker := TMfCastDisconnectWorker.Create(FCast,
                                            Handle);
  FWorker.Start();
  memLog.Lines.Add('Disconnecting and requesting standby for the paired TV...');

  UpdateControls();
end;


procedure TCastPlayerForm.btnSeekClick(Sender: TObject);
var
  Hr: HRESULT;

begin

  FSeekPositionPending := True;
  FSeekTargetSeconds := trkSeek.Position;
  FSeekRequestTick := GetTickCount();

  Hr := FCast.Seek(FSeekTargetSeconds);
  LogResult('Seek',
            Hr);

  if FAILED(Hr) then
    begin
      FSeekPositionPending := False;
      FSeekTargetSeconds := -1;
      FSeekRequestTick := 0;
    end;
end;


procedure TCastPlayerForm.trkSeekChange(Sender: TObject);
begin

  if FUpdatingSeekPosition then
    Exit;

  FSeekPositionPending := True;
  FSeekTargetSeconds := -1;
  FSeekRequestTick := 0;
end;


procedure TCastPlayerForm.trkVolumeChange(Sender: TObject);
begin

  if trkVolume.Enabled then
    begin
      FCast.SetVolume(trkVolume.Position / 100.0);
      lblVolume.Caption := Format('Volume %d%%',
                                  [trkVolume.Position]);
    end;
end;


procedure TCastPlayerForm.cbxKeepOnTopClick(Sender: TObject);
begin

  // Keep on top.
  if cbxKeepOnTop.Checked then

    SetWindowPos(Handle,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize)
  else
    SetWindowPos(Handle,
                 HWND_NOTOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize);
end;


procedure TCastPlayerForm.chkMutedClick(Sender: TObject);
begin

  if chkMuted.Enabled then
    FCast.SetMuted(chkMuted.Checked);
end;


procedure TCastPlayerForm.PreviewResize(Sender: TObject);
begin

  if Assigned(FCast) then
    FCast.UpdatePreviewWindow();
end;


procedure TCastPlayerForm.SourceOrDeviceChanged(Sender: TObject);
var
  Hr: HRESULT;

begin

  if (Sender = edtArtwork) and Assigned(FCast) and (not Assigned(FWorker)) and FileExists(Trim(edtArtwork.Text)) then
    begin
      Hr := FCast.SetAudioArtwork(Trim(edtArtwork.Text));
      if FAILED(Hr) then
        LogResult('Update artwork (previous image retained)',
                  Hr);
    end;

  if (Sender = edtSource) and
     (not SameText(FSubtitleSourceName,
                   Trim(edtSource.Text))) then
    begin
      ClearSubtitleChoices();
      ClearAudioTracks();
    end;

  UpdateControls();
end;


procedure TCastPlayerForm.SubtitleSelectionChanged(Sender: TObject);
begin

  UpdateControls();
  ApplySubtitleSelection();
end;


procedure TCastPlayerForm.AudioSelectionChanged(Sender: TObject);
begin

  if FUpdatingAudioSelection then
    Exit;

  UpdateControls();
  ApplyAudioSelection();
end;

end.
