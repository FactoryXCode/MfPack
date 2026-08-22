// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmCastPlayer.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
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
  {System}
  System.SysUtils,
  System.Classes,
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
  MfEmbeddedSubtitleReader,
  {Sample resolvers}
  MfYouTubeSourceResolver;

type

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
    lblYouTubeMode: TLabel;
    cbxYouTubeMode: TComboBox;
    Bevel1: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    btnCast: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnDisconnect: TButton;

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
    procedure YouTubeModeChanged(Sender: TObject);
    procedure PreviewResize(Sender: TObject);

  private
    FCast: TMfCast;
    FSourceResolver: IMfCastSourceResolver;
    FYouTubeSourceResolver: TMfYouTubeSourceResolver;
    FDevices: TMfCastDeviceArray;
    FWorker: TMfCastFileWorker;
    FSubtitleWorker: TMfCastSubtitleWorker;
    FClosing: Boolean;
    FUpdatingSeekPosition: Boolean;
    FSeekPositionPending: Boolean;
    FSeekTargetSeconds: Integer;
    FSeekRequestTick: Cardinal;
    FSubtitleChoices: TMfCastSubtitleChoiceArray;
    FSubtitleSourceName: string;

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
    procedure LogResult(const AOperation: string; const AHResult: HRESULT);
    procedure UpdateControls();

    procedure WmCastDevices(var message: Tmessage); message WM_MFCAST_DEVICES;
    procedure WmCastState(var message: Tmessage); message WM_MFCAST_STATE;
    procedure WmCastStatus(var message: Tmessage); message WM_MFCAST_STATUS;
    procedure WmCastError(var message: Tmessage); message WM_MFCAST_ERROR;
    procedure WmCastFinished(var message: Tmessage); message WM_MFCAST_FINISHED;
    procedure WmCastLog(var message: Tmessage); message WM_MFCAST_LOG;
    procedure WmCastSubtitleFinished(var message: Tmessage); message WM_MFCAST_SUBTITLE_FINISHED;
  end;

var
  CastPlayerForm: TCastPlayerForm;


implementation

{$R *.dfm}


procedure TCastPlayerForm.FormCreate(Sender: TObject);
begin

  FClosing := False;
  FUpdatingSeekPosition := False;
  FSeekPositionPending := False;
  FSeekTargetSeconds := -1;
  FSeekRequestTick := 0;
  ClearSubtitleChoices();
  HandleNeeded();

  // Enable the optional conversion stack used by MfPlayer X2 so containers
  // such as Matroska can be converted to fragmented MP4 for Chromecast.
  FCast := TMfCast.Create(True);
  FYouTubeSourceResolver := TMfYouTubeSourceResolver.Create();
  FSourceResolver := FYouTubeSourceResolver;
  cbxYouTubeMode.ItemIndex := 0;
  YouTubeModeChanged(cbxYouTubeMode);
  LogResult('Attach source resolver',
            FCast.SetSourceResolver(FSourceResolver));
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
  memLog.Lines.Add('Ready. Choose a local file, direct media URL, or YouTube URL.');
  UpdateControls;
end;


procedure TCastPlayerForm.YouTubeModeChanged(Sender: TObject);
begin
  if not Assigned(FYouTubeSourceResolver) then
    Exit;

  if cbxYouTubeMode.ItemIndex = 1 then
    FYouTubeSourceResolver.Mode := yrmBestQualityDownload
  else
    FYouTubeSourceResolver.Mode := yrmFastCombinedStream;
end;


procedure TCastPlayerForm.FormDestroy(Sender: TObject);
var
  message: TMsg;

begin

  FClosing := True;

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
  OldIndex: Integer;
  Hr: HRESULT;

begin

  OldIndex := cbxDevices.ItemIndex;

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

    for I := 0 to Length(FDevices) - 1 do
      cbxDevices.Items.Add(Format('%s  (%s:%d)',
                                  [FDevices[I].FriendlyName, FDevices[I].Address, FDevices[I].Port]));

    if (OldIndex >= 0) and (OldIndex < cbxDevices.Items.Count) then
      cbxDevices.ItemIndex := OldIndex
    else
      if (cbxDevices.Items.Count > 0) then
        cbxDevices.ItemIndex := 0;
  finally
    cbxDevices.Items.EndUpdate();
  end;
  UpdateControls;
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
  SUBTITLE_EXTENSIONS: array[0..2] of string = ('.srt',
                                                '.vtt',
                                                '.sub');
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
              end;

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
  Active: Boolean;
  Busy: Boolean;

begin

  Busy := Assigned(FWorker) or Assigned(FSubtitleWorker);
  Active := Assigned(FCast) and (not Busy) and
            (FCast.State in [csConnected, csLaunchingReceiver, csBuffering, csPlaying, csPaused, csStopped]);

  btnCast.Enabled := (cbxDevices.ItemIndex >= 0) and
                     (Trim(edtSource.Text) <> '') and
                     (not Busy) and
                     Assigned(FCast) and
                     (FCast.State in [csIdle, csDiscovering, csStopped, csError]);

  btnPlay.Enabled := Active;
  btnPause.Enabled := Active;
  btnStop.Enabled := Active;
  btnDisconnect.Enabled := Active;
  btnSeek.Enabled := Active;
  trkSeek.Enabled := Active;
  trkVolume.Enabled := Active;
  chkMuted.Enabled := Active;
  chkEmbeddedSubtitles.Enabled := not Busy;
  cbxSubtitleLanguage.Enabled := chkEmbeddedSubtitles.Checked and
                                 (cbxSubtitleLanguage.Items.Count > 0) and
                                 (not Assigned(FWorker)) and
                                 (not Assigned(FSubtitleWorker));
  cbxYouTubeMode.Enabled := not Busy;
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
  LogResult('Cast media', Hr);
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
  IsIndirectSource: Boolean;

begin

  if (cbxDevices.ItemIndex < 0) or
     (cbxDevices.ItemIndex >= Length(FDevices)) or
     (Trim(edtSource.Text) = '') then
    Exit;

  IsIndirectSource := Assigned(FSourceResolver) and
                      FSourceResolver.CanResolve(Trim(edtSource.Text));
  if IsIndirectSource then
    begin
      ClearSubtitleChoices();
      Subtitle.Reset();
      if FYouTubeSourceResolver.Mode = yrmFastCombinedStream then
        memLog.Lines.Add('Resolving the fast YouTube stream in a worker thread...')
      else
        memLog.Lines.Add('Resolving and downloading the best-quality YouTube media in a worker thread...');
    end
  else
    begin
      if not SameText(FSubtitleSourceName, Trim(edtSource.Text)) then
        RefreshSubtitleChoices(edtSource.Text);
      BuildSelectedSubtitle(Subtitle);
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
    ClearSubtitleChoices();

  UpdateControls();
end;


procedure TCastPlayerForm.SubtitleSelectionChanged(Sender: TObject);
begin

  UpdateControls();
  ApplySubtitleSelection();
end;

end.
