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
  WinApi.messages,
  WinApi.MediaFoundationApi.MfApi,
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
    btnDiscover: TButton;
    btnRefresh: TButton;
    edtSource: TEdit;
    btnBrowse: TButton;
    edtArtwork: TEdit;
    btnBrowseArtwork: TButton;
    chkEmbeddedSubtitles: TCheckBox;
    trkSeek: TTrackBar;
    btnSeek: TButton;
    trkVolume: TTrackBar;
    chkMuted: TCheckBox;
    cbxSubtitleLanguage: TComboBox;
    OpenDialog: TOpenDialog;
    ArtworkDialog: TOpenDialog;
    chkCaptureAudio: TCheckBox;
    cbxCaptureCodec: TComboBox;
    Bevel1: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    btnCast: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnDisconnect: TButton;
    btnCastDesktop: TButton;
    Label2: TLabel;
    cbxKeepOnTop: TCheckBox;
    lblAudioStream: TLabel;
    cbxAudioStream: TComboBox;

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
    procedure PreviewResize(Sender: TObject);
    procedure cbxKeepOnTopClick(Sender: TObject);

  private
    FCast: TMfCast;
    FDevices: TMfCastDeviceArray;
    FWorker: TThread;
    FSubtitleWorker: TMfCastSubtitleWorker;
    FAudioTrackWorker: TMfCastAudioTrackWorker;
    FClosing: Boolean;
    FUpdatingSeekPosition: Boolean;
    FSeekPositionPending: Boolean;
    FSeekTargetSeconds: Integer;
    FSeekRequestTick: Cardinal;
    FSubtitleChoices: TMfCastSubtitleChoiceArray;
    FSubtitleSourceName: string;
    FAudioTracks: TMfCastTrackInfoArray;
    FAudioSourceName: string;
    FUpdatingAudioSelection: Boolean;

    procedure DeviceChanged(const ADevice: TMfCastDevice);
    procedure DeviceRemoved(const ADeviceId: string);

    procedure CastStateChanged(const AOldState: TMfCastState;
                               const ANewState: TMfCastState);
    procedure CastMediaStatus(const AStatus: TMfCastMediaStatus);

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

    procedure UpdateControls();

    procedure WmCastDevices(var message: Tmessage); message WM_MFCAST_DEVICES;
    procedure WmCastState(var message: Tmessage); message WM_MFCAST_STATE;
    procedure WmCastStatus(var message: Tmessage); message WM_MFCAST_STATUS;
    procedure WmCastError(var message: Tmessage); message WM_MFCAST_ERROR;
    procedure WmCastFinished(var message: Tmessage); message WM_MFCAST_FINISHED;
    procedure WmCastLog(var message: Tmessage); message WM_MFCAST_LOG;
    procedure WmCastSubtitleFinished(var message: Tmessage); message WM_MFCAST_SUBTITLE_FINISHED;
    procedure WmCastAudioTrackFinished(var message: Tmessage); message WM_MFCAST_AUDIO_TRACK_FINISHED;
    procedure WmHotKey(var message: Tmessage); message WM_HOTKEY;
  end;

var
  CastPlayerForm: TCastPlayerForm;


implementation

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
  Dialog: TForm;
  PromptLabel: TLabel;
  MonitorList: TListBox;
  OkButton: TButton;
  CancelButton: TButton;
  Monitor: TMonitor;
  I: Integer;
  PrimaryText: string;

begin
  Result := False;
  AOutputIndex := 0;
  ADescription := '';

  if Screen.MonitorCount = 0 then
    Exit;

  Dialog := TForm.Create(Self);
  try
    Dialog.Caption := 'Select screen to cast';
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poOwnerFormCenter;
    Dialog.ClientWidth := 500;
    Dialog.ClientHeight := 238;

    PromptLabel := TLabel.Create(Dialog);
    PromptLabel.Parent := Dialog;
    PromptLabel.Left := 16;
    PromptLabel.Top := 16;
    PromptLabel.Caption := 'Choose the screen that should be captured:';

    MonitorList := TListBox.Create(Dialog);
    MonitorList.Parent := Dialog;
    MonitorList.SetBounds(16, 40, 468, 144);

    for I := 0 to Screen.MonitorCount - 1 do
      begin
        Monitor := Screen.Monitors[I];
        if Monitor.Primary then
          PrimaryText := ' (primary)'
        else
          PrimaryText := '';

        MonitorList.Items.Add(Format('Screen %d: %dx%d at (%d, %d)%s',
          [I + 1,
           Monitor.Width,
           Monitor.Height,
           Monitor.Left,
           Monitor.Top,
           PrimaryText]));

        if Monitor.Primary then
          MonitorList.ItemIndex := I;
      end;

    if MonitorList.ItemIndex < 0 then
      MonitorList.ItemIndex := 0;

    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.SetBounds(316, 198, 80, 27);
    OkButton.Caption := 'Cast';
    OkButton.Default := True;
    OkButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.SetBounds(404, 198, 80, 27);
    CancelButton.Caption := 'Cancel';
    CancelButton.Cancel := True;
    CancelButton.ModalResult := mrCancel;

    if Dialog.ShowModal <> mrOk then
      Exit;

    if (MonitorList.ItemIndex < 0) or
       (MonitorList.ItemIndex >= Screen.MonitorCount) then
      Exit;

    Monitor := Screen.Monitors[MonitorList.ItemIndex];
    AOutputIndex := Cardinal(Monitor.MonitorNum);
    ADescription := MonitorList.Items[MonitorList.ItemIndex];
    Result := True;
  finally
    Dialog.Free();
  end;
end;


procedure TMfCastDesktopWorker.Execute();
var
  Hr: HRESULT;
  Capabilities: TMfCastCaptureCapabilities;

begin

  Hr := FCast.CastDesktop(FDevice,
                          FSettings,
                          Capabilities);

  PostMessage(FNotifyWindow,
              WM_MFCAST_FINISHED,
              WPARAM(Hr),
              0);
end;


procedure TCastPlayerForm.FormCreate(Sender: TObject);
begin

  FClosing := False;
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
  cbxCaptureCodec.ItemIndex := 0;
  chkCaptureAudio.Checked := True;
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
  trkSeek.Frequency := 300;
  trkVolume.Max := 100;
  trkVolume.Position := 75;
  memLog.Lines.Add('Ready. Choose a local file/URL, or cast the desktop with DXGI and WASAPI.');
  RegisterGlobalHotKeys();
  UpdateControls;
end;


procedure TCastPlayerForm.FormDestroy(Sender: TObject);
var
  message: TMsg;

begin

  FClosing := True;
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
  RegisterOne(MFCAST_HOTKEY_CAST, NoRepeatModifiers, Ord('C'), 'Ctrl+Alt+C');
  RegisterOne(MFCAST_HOTKEY_CAST_DESKTOP, NoRepeatModifiers, Ord('D'), 'Ctrl+Alt+D');
  RegisterOne(MFCAST_HOTKEY_PLAY, NoRepeatModifiers, VK_F5, 'Ctrl+Alt+F5');
  RegisterOne(MFCAST_HOTKEY_PAUSE, NoRepeatModifiers, VK_F6, 'Ctrl+Alt+F6');
  RegisterOne(MFCAST_HOTKEY_STOP, NoRepeatModifiers, VK_F7, 'Ctrl+Alt+F7');
  RegisterOne(MFCAST_HOTKEY_VOLUME_UP, RepeatModifiers, VK_UP, 'Ctrl+Alt+Up');
  RegisterOne(MFCAST_HOTKEY_VOLUME_DOWN, RepeatModifiers, VK_DOWN, 'Ctrl+Alt+Down');
  RegisterOne(MFCAST_HOTKEY_MUTE, NoRepeatModifiers, Ord('M'), 'Ctrl+Alt+M');
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
    MFCAST_HOTKEY_CAST:
      if btnCast.Enabled then
        btnCast.Click();

    MFCAST_HOTKEY_CAST_DESKTOP:
      if btnCastDesktop.Enabled then
        btnCastDesktop.Click();

    MFCAST_HOTKEY_PLAY:
      if btnPlay.Enabled then
        btnPlay.Click();

    MFCAST_HOTKEY_PAUSE:
      if btnPause.Enabled then
        btnPause.Click();

    MFCAST_HOTKEY_STOP:
      if btnStop.Enabled then
        btnStop.Click();

    MFCAST_HOTKEY_VOLUME_UP:
      if trkVolume.Enabled then
        begin
          NewPosition := trkVolume.Position + VolumeStep;
          if NewPosition > trkVolume.Max then
            NewPosition := trkVolume.Max;
          trkVolume.Position := NewPosition;
          trkVolumeChange(trkVolume);
        end;

    MFCAST_HOTKEY_VOLUME_DOWN:
      if trkVolume.Enabled then
        begin
          NewPosition := trkVolume.Position - VolumeStep;
          if NewPosition < trkVolume.Min then
            NewPosition := trkVolume.Min;
          trkVolume.Position := NewPosition;
          trkVolumeChange(trkVolume);
        end;

    MFCAST_HOTKEY_MUTE:
      if chkMuted.Enabled then
        begin
          chkMuted.Checked := not chkMuted.Checked;
          chkMutedClick(chkMuted);
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

  // Error details arrive through OnError, which includes the structured stage
  // and detail fields. Avoid writing the same failure twice in the memo.
  if (ALevel <> cllError) then
    PostLogmessage(WM_MFCAST_LOG,
                   Format('[%s][%s] %s',
                          [MfCastLogLevelToString(ALevel),
                           ASource,
                           Amessage]));
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

  if (cbxDevices.ItemIndex >= 0) and
     (cbxDevices.ItemIndex < Length(FDevices)) then
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
           (((SelectedId <> '') and SameText(FDevices[I].Id, SelectedId)) or
            ((SelectedId = '') and (SelectedAddress <> '') and
             SameText(FDevices[I].Address, SelectedAddress))) then
          SelectedIndex := I;
      end;

    if SelectedIndex >= 0 then
      cbxDevices.ItemIndex := SelectedIndex
    else if cbxDevices.Items.Count > 0 then
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
      for ExtensionIndex := Low(SUBTITLE_EXTENSIONS) to
                            High(SUBTITLE_EXTENSIONS) do
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

    if cbxSubtitleLanguage.Items.Count > 0 then
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
  if chkEmbeddedSubtitles.Checked and
     (cbxSubtitleLanguage.ItemIndex >= 0) and
     (cbxSubtitleLanguage.ItemIndex < Length(FSubtitleChoices)) then
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

    Hr := FCast.GetMediaTracks(SourceName, AllTracks);
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


function TCastPlayerForm.IsDtsAudioTrack(
  const ATrack: TMfCastTrackInfo): Boolean;
begin

  Result := (Pos('DTS', UpperCase(ATrack.Name)) > 0) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_RAW)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_HD)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_XLL)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_LBR)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_UHD)) or
            SameText(ATrack.SubType, GUIDToString(MFAudioFormat_DTS_UHDY));
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

  FAudioTrackWorker := TMfCastAudioTrackWorker.Create(
    FCast,
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

  CanControlReceiver := Assigned(FCast) and (not Busy) and
                        (State in [csConnected, csLaunchingReceiver,
                                   csBuffering, csPlaying, csPaused,
                                   csStopped]);

  btnCast.Enabled := (cbxDevices.ItemIndex >= 0) and
                     (Trim(edtSource.Text) <> '') and
                     (not Busy) and
                      Assigned(FCast) and
                       (State in [csIdle, csDiscovering, csStopped, csError]);
  btnCastDesktop.Enabled := (cbxDevices.ItemIndex >= 0) and
                             (not Busy) and Assigned(FCast) and
                             (State in [csIdle, csDiscovering,
                                        csStopped, csError]);

  // STOP ends the receiver's media session, so PLAY cannot restart it.  A
  // stopped source must be cast again.  PLAY is only a resume operation for
  // an existing paused session.
  btnPlay.Enabled := CanControlReceiver and (State = csPaused);
  btnPause.Enabled := CanControlReceiver and (State = csPlaying);
  btnStop.Enabled := CanControlReceiver and
                     (State in [csBuffering, csPlaying, csPaused]);
  btnDisconnect.Enabled := CanControlReceiver;
  btnSeek.Enabled := CanControlReceiver and
                     (State in [csPlaying, csPaused]);
  trkSeek.Enabled := btnSeek.Enabled;
  trkVolume.Enabled := CanControlReceiver;
  chkMuted.Enabled := CanControlReceiver;
  chkEmbeddedSubtitles.Enabled := not Busy;
  cbxSubtitleLanguage.Enabled := chkEmbeddedSubtitles.Checked and
                                 (cbxSubtitleLanguage.Items.Count > 0) and
                                 (not Assigned(FWorker)) and
                                 (not Assigned(FSubtitleWorker));
  cbxAudioStream.Enabled := (cbxAudioStream.Items.Count > 0) and
                            (not Busy);
  cbxCaptureCodec.Enabled := not Busy;
  chkCaptureAudio.Enabled := not Busy;
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

  LogResult('Cast media',
            Hr);

  if SUCCEEDED(Hr) then
    begin
      // Apply the UI settings to a receiver that may have retained a muted
      // state from an earlier session.
      FCast.SetVolume(trkVolume.Position / 100.0);
      FCast.SetMuted(chkMuted.Checked);
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

  LogResult('Audio track switch', HRESULT(message.WParam));
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


procedure TCastPlayerForm.btnBrowseClick(Sender: TObject);
begin

  if OpenDialog.Execute then
    begin
      edtSource.Text := OpenDialog.FileName;
      RefreshSubtitleChoices(edtSource.Text);
      RefreshAudioTracks(edtSource.Text);
    end;
  UpdateControls();
end;


procedure TCastPlayerForm.btnBrowseArtworkClick(Sender: TObject);
begin

  if ArtworkDialog.Execute then
    edtArtwork.Text := ArtworkDialog.FileName;
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

  if not SameText(FSubtitleSourceName, Trim(edtSource.Text)) then
    RefreshSubtitleChoices(edtSource.Text);
  if not SameText(FAudioSourceName, Trim(edtSource.Text)) then
    RefreshAudioTracks(edtSource.Text);
  BuildSelectedSubtitle(Subtitle);

  if (cbxAudioStream.ItemIndex >= 0) and
     (cbxAudioStream.ItemIndex < Length(FAudioTracks)) then
    begin
      if not EnsureSelectedAudioTrackSupported() then
        Exit;

      Hr := FCast.SelectAudioTrack(
        FAudioTracks[cbxAudioStream.ItemIndex].TrackId);
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

  FWorker := TMfCastFileWorker.Create(FCast,
                                      FDevices[cbxDevices.ItemIndex],
                                      Trim(edtSource.Text),
                                      Subtitle,
                                      Handle);
  FWorker.Start();
  memLog.Lines.Add('Preparing and connecting in a worker thread...');
  UpdateControls;
end;


procedure TCastPlayerForm.btnCastDesktopClick(Sender: TObject);
var
  Settings: TMfCastCaptureSettings;
  Capabilities: TMfCastCaptureCapabilities;
  MonitorDescription: string;
  CodecName: string;
  Hr: HRESULT;

begin
  if (cbxDevices.ItemIndex < 0) or
     (cbxDevices.ItemIndex >= Length(FDevices)) or
     Assigned(FWorker) then
    Exit;

  Settings.Reset();
  Settings.CaptureSystemAudio := chkCaptureAudio.Checked;

  if not SelectCaptureMonitor(Settings.OutputIndex,
                              MonitorDescription) then
    begin
      memLog.Lines.Add('Desktop casting cancelled: no screen was selected.');
      Exit;
    end;

  memLog.Lines.Add(Format('Desktop source: %s (DXGI output %d).',
                          [MonitorDescription,
                           Settings.OutputIndex]));

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

  Hr := FCast.GetCaptureCapabilities(FDevices[cbxDevices.ItemIndex],
                                     Settings.CodecPreference,
                                     Capabilities);
  if FAILED(Hr) then
    begin
      memLog.Lines.Add('Capture capability check: ' +
                       Capabilities.SelectionReason);
      LogResult('Check capture capabilities', Hr);
      Exit;
    end;

  case Capabilities.SelectedCodec of
    cvcH264: CodecName := 'H.264';
    cvcHEVC: CodecName := 'HEVC';
  else
    CodecName := 'none';
  end;

  memLog.Lines.Add(Format('Desktop capture: codec=%s, hardware H.264=%s, hardware HEVC=%s, receiver HEVC=%s, audio=%s.',
                          [CodecName,
                           BoolToStr(Capabilities.HardwareH264EncoderAvailable, True),
                           BoolToStr(Capabilities.HardwareHEVCEncoderAvailable, True),
                           BoolToStr(Capabilities.ReceiverSupportsHEVC, True),
                           BoolToStr(Settings.CaptureSystemAudio, True)]));

  memLog.Lines.Add(Capabilities.SelectionReason);

  FWorker := TMfCastDesktopWorker.Create(FCast,
                                         FDevices[cbxDevices.ItemIndex],
                                         Settings,
                                         Handle);

  FWorker.Start();

  memLog.Lines.Add('Starting DXGI/WASAPI desktop casting in a worker thread...');
  UpdateControls();
end;


procedure TCastPlayerForm.btnPlayClick(Sender: TObject);
var
  Hr: HRESULT;

begin

  Hr := FCast.Play;
  LogResult('Play', Hr);
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

  LogResult('Disconnect',
            FCast.Disconnect);
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
  LogResult('Seek', Hr);

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
    FCast.SetVolume(trkVolume.Position / 100.0);
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
begin

  if (Sender = edtSource) and
     (not SameText(FSubtitleSourceName, Trim(edtSource.Text))) then
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
