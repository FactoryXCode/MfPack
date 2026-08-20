unit frmSimpleCastPlayer;

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
  MfCastTypes,
  MfCastWindowsSupport,
  {Cast/Media}
  LangTags,
  MfEmbeddedSubtitleReader;

type

  TSimpleCastPlayerForm = class(TForm)
    lblState: TLabel;
    memLog: TMemo;
    pnlPreview: TPanel;
    pnlCtrl: TPanel;
    Bevel2: TBevel;
    lblDevices: TLabel;
    lblSource: TLabel;
    lblSeek: TLabel;
    lblVolume: TLabel;
    Bevel3: TBevel;
    lstDevices: TListBox;
    btnDiscover: TButton;
    btnRefresh: TButton;
    edtSource: TEdit;
    btnBrowse: TButton;
    btnCast: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnDisconnect: TButton;
    chkEmbeddedSubtitles: TCheckBox;
    trkSeek: TTrackBar;
    btnSeek: TButton;
    trkVolume: TTrackBar;
    chkMuted: TCheckBox;
    cbxSubtitleLanguage: TComboBox;
    OpenDialog: TOpenDialog;
    Bevel1: TBevel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDiscoverClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
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

  private
    FCast: TMfCast;
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
  SimpleCastPlayerForm: TSimpleCastPlayerForm;

implementation

{$R *.dfm}


procedure TSimpleCastPlayerForm.FormCreate(Sender: TObject);
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
  memLog.Lines.Add('Ready. Discover a device, then choose a local file or enter an HTTP(S) URL.');
  UpdateControls;
end;


procedure TSimpleCastPlayerForm.FormDestroy(Sender: TObject);
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


procedure TSimpleCastPlayerForm.DeviceChanged(const ADevice: TMfCastDevice);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TSimpleCastPlayerForm.DeviceRemoved(const ADeviceId: string);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_DEVICES,
                0,
                0);
end;


procedure TSimpleCastPlayerForm.CastStateChanged(const AOldState: TMfCastState;
                                                 const ANewState: TMfCastState);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_STATE,
                Ord(ANewState),
                0);
end;


procedure TSimpleCastPlayerForm.CastMediaStatus(const AStatus: TMfCastMediaStatus);
begin

  if not FClosing then
    Postmessage(Handle,
                WM_MFCAST_STATUS,
                WPARAM(AStatus.CurrentTime100ns div 10000000),
                LPARAM(AStatus.Duration100ns div 10000000));
end;


procedure TSimpleCastPlayerForm.CastError(const AError: TMfCastErrorInfo);
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


procedure TSimpleCastPlayerForm.CastLog(Sender: TObject;
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


procedure TSimpleCastPlayerForm.PostLogmessage(const AWindowsmessage: Cardinal;
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


procedure TSimpleCastPlayerForm.ReloadDevices;
var
  I: Integer;
  OldIndex: Integer;
  Hr: HRESULT;

begin

  OldIndex := lstDevices.ItemIndex;

  Hr := FCast.GetDevices(FDevices);
  if FAILED(Hr) then
    begin
      LogResult('Read devices',
                Hr);
      Exit;
    end;

  lstDevices.Items.BeginUpdate();

  try
    lstDevices.Clear;

    for I := 0 to Length(FDevices) - 1 do
      lstDevices.Items.Add(Format('%s  (%s:%d)',
                                  [FDevices[I].FriendlyName, FDevices[I].Address, FDevices[I].Port]));

    if (OldIndex >= 0) and (OldIndex < lstDevices.Count) then
      lstDevices.ItemIndex := OldIndex
    else
      if (lstDevices.Count > 0) then
        lstDevices.ItemIndex := 0;
  finally
    lstDevices.Items.EndUpdate();
  end;
  UpdateControls;
end;


procedure TSimpleCastPlayerForm.ClearSubtitleChoices();
begin

  SetLength(FSubtitleChoices, 0);
  FSubtitleSourceName := '';
  cbxSubtitleLanguage.Items.Clear();
  cbxSubtitleLanguage.ItemIndex := -1;
end;


procedure TSimpleCastPlayerForm.AddSubtitleChoice(const AChoice: TMfCastSubtitleChoice);
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


procedure TSimpleCastPlayerForm.RefreshSubtitleChoices(const ASourceName: string);
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


procedure TSimpleCastPlayerForm.BuildSelectedSubtitle(out ASubtitle: TMfCastSubtitleAsset);
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
    end
  else
    if chkEmbeddedSubtitles.Checked then
      begin
        ASubtitle.Enabled := True;
        ASubtitle.Embedded := True;
        ASubtitle.Name := 'Embedded subtitles';
        ASubtitle.SourceName := Trim(edtSource.Text);
      end;
end;


procedure TSimpleCastPlayerForm.ApplySubtitleSelection();
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


procedure TSimpleCastPlayerForm.LogResult(const AOperation: string;
                                          const AHResult: HRESULT);
begin

  if FAILED(AHResult) then
    memLog.Lines.Add(Format('%s failed: HRESULT $%.8x',
                            [AOperation, DWORD(AHResult)]))
  else
    memLog.Lines.Add(AOperation + ' requested.');
end;


procedure TSimpleCastPlayerForm.UpdateControls();
var
  Active: Boolean;
  Busy: Boolean;

begin

  Busy := Assigned(FWorker) or Assigned(FSubtitleWorker);
  Active := Assigned(FCast) and (not Busy) and
            (FCast.State in [csConnected, csLaunchingReceiver, csBuffering, csPlaying, csPaused, csStopped]);

  btnCast.Enabled := (lstDevices.ItemIndex >= 0) and
                     (Trim(edtSource.Text) <> '') and
                     (not Busy);

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
end;


procedure TSimpleCastPlayerForm.WmCastDevices(var message: Tmessage);
begin

  ReloadDevices();
end;


procedure TSimpleCastPlayerForm.WmCastState(var message: Tmessage);
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


procedure TSimpleCastPlayerForm.WmCastStatus(var message: Tmessage);
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


procedure TSimpleCastPlayerForm.WmCastError(var message: Tmessage);
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


procedure TSimpleCastPlayerForm.WmCastFinished(var message: Tmessage);
begin

  if Assigned(FWorker) then
    begin
      FWorker.WaitFor();
      FreeAndNil(FWorker);
    end;
  LogResult('Cast media',
            HRESULT(message.WParam));
  UpdateControls;
end;


procedure TSimpleCastPlayerForm.WmCastLog(var message: Tmessage);
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


procedure TSimpleCastPlayerForm.WmCastSubtitleFinished(var message: Tmessage);
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


procedure TSimpleCastPlayerForm.btnDiscoverClick(Sender: TObject);
begin

  LogResult('Discovery',
            FCast.Discover);
end;


procedure TSimpleCastPlayerForm.btnRefreshClick(Sender: TObject);
begin

  LogResult('Discovery refresh',
            FCast.RefreshDiscovery);
end;


procedure TSimpleCastPlayerForm.btnBrowseClick(Sender: TObject);
begin

  if OpenDialog.Execute then
    begin
      edtSource.Text := OpenDialog.FileName;
      RefreshSubtitleChoices(edtSource.Text);
    end;
  UpdateControls();
end;


procedure TSimpleCastPlayerForm.btnCastClick(Sender: TObject);
var
  Subtitle: TMfCastSubtitleAsset;

begin

  if (lstDevices.ItemIndex < 0) or
     (lstDevices.ItemIndex >= Length(FDevices)) or
     (Trim(edtSource.Text) = '') then
    Exit;

  if not SameText(FSubtitleSourceName, Trim(edtSource.Text)) then
    RefreshSubtitleChoices(edtSource.Text);

  BuildSelectedSubtitle(Subtitle);

  FWorker := TMfCastFileWorker.Create(FCast,
                                FDevices[lstDevices.ItemIndex],
                                Trim(edtSource.Text),
                                Subtitle,
                                Handle);
  FWorker.Start();
  memLog.Lines.Add('Preparing and connecting in a worker thread...');
  UpdateControls;
end;


procedure TSimpleCastPlayerForm.btnPlayClick(Sender: TObject);
begin

  LogResult('Play',
            FCast.Play);
end;


procedure TSimpleCastPlayerForm.btnPauseClick(Sender: TObject);
begin

  LogResult('Pause',
            FCast.Pause);
end;


procedure TSimpleCastPlayerForm.btnStopClick(Sender: TObject);
begin

  LogResult('Stop',
            FCast.Stop);
end;


procedure TSimpleCastPlayerForm.btnDisconnectClick(Sender: TObject);
begin

  LogResult('Disconnect',
            FCast.Disconnect);
  UpdateControls();
end;


procedure TSimpleCastPlayerForm.btnSeekClick(Sender: TObject);
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


procedure TSimpleCastPlayerForm.trkSeekChange(Sender: TObject);
begin

  if FUpdatingSeekPosition then
    Exit;

  FSeekPositionPending := True;
  FSeekTargetSeconds := -1;
  FSeekRequestTick := 0;
end;


procedure TSimpleCastPlayerForm.trkVolumeChange(Sender: TObject);
begin

  if trkVolume.Enabled then
    FCast.SetVolume(trkVolume.Position / 100.0);
end;


procedure TSimpleCastPlayerForm.chkMutedClick(Sender: TObject);
begin

  if chkMuted.Enabled then
    FCast.SetMuted(chkMuted.Checked);
end;


procedure TSimpleCastPlayerForm.SourceOrDeviceChanged(Sender: TObject);
begin

  if (Sender = edtSource) and
     (not SameText(FSubtitleSourceName, Trim(edtSource.Text))) then
    ClearSubtitleChoices();

  UpdateControls();
end;


procedure TSimpleCastPlayerForm.SubtitleSelectionChanged(Sender: TObject);
begin

  UpdateControls();
  ApplySubtitleSelection();
end;

end.
