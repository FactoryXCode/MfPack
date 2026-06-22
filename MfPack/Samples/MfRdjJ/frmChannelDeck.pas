// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmChannelDeck.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Channeldeck MDI child form.
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
unit frmChannelDeck;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  Winapi.CommDlg,
  Winapi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Winapi.UxTheme,
  {System}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Types,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Application}
  RDJ.Setup,
  RDJ_Common,
  MfPeakMeterMmcs,
  MfChannelDeckEngine,
  MPxpButton,
  MfTrackBar,
  RDJ.InternalMixer,
  MfWasApiEffectsRack,
  MfParametricEqComponent,
  // Icecast
  MfIcecastBroadcastEngine,
  // Playlist
  RDJ.PlaylistTypes,
  RDJ.PlaylistDb,
  RDJ.TrackLibrary,
  RDJ.PlaylistManager,
  RDJ.FilenameParser,
  LWFileBrowserExDlg, // Select single file.
  MfBeatLed,
  MfLevelProgressBar;

const

  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;

  MIN_FREQ = 10.0;
  MAX_FREQ = 22000.0;

type

  TfrmChannelDeck = class(TLockedMdiChildForm)
    pnlMid: TPanel;
    pnlTop: TPanel;
    pmLeft: TMfPeakMeterMmcs;
    pmRight: TMfPeakMeterMmcs;
    lblVolumePerc: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lblPitch: TLabel;
    lblP: TLabel;
    lblVol: TLabel;
    tbVolume: TMfTrackBar;
    tbPitch: TMfTrackBar;
    tbBalance: TMfTrackBar;
    lblBarPositionTime: TLabel;
    chkAutoCue: TMPxpButton;
    btnPFL: TMPxpButton;
    chkPlayList: TMPxpButton;
    chkMute: TMPxpButton;
    chkCrossFade: TMPxpButton;
    btnLoad: TMPxpButton;
    btnPlayPause: TMPxpButton;
    btnStop: TMPxpButton;
    tbEqQ: TMfTrackBar;
    tbEqCenterFreqHz: TMfTrackBar;
    tbEqGainDb: TMfTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnEqEnable: TMPxpButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    chkRepeatPlay: TMPxpButton;
    chkPlayListShuffle: TMPxpButton;
    btnPlayListPrev: TMPxpButton;
    btnPlayListNext: TMPxpButton;
    lblBpm: TLabel;
    bldBeat: TMfBeatLed;
    tbInputGain: TMfTrackBar;
    lblGain: TLabel;
    lblPeq: TLabel;
    lblInputGainValue: TLabel;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    pnlBottom: TPanel;
    Bevel8: TBevel;
    lblDuration: TLabel;
    lblPlayed: TLabel;
    lblStatus: TLabel;
    pbProgress: TMfLevelProgressBar;
    Bevel5: TBevel;
    Bevel6: TBevel;

    procedure FormCreate(Sender: TObject);
    procedure AfterConstruction(); override;
    procedure tbVolumeChange(Sender: TObject);
    procedure chkCrossFadeClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tbPitchChange(Sender: TObject);
    procedure chkMuteClick(Sender: TObject);
    procedure jgsProgressChange(Sender: TObject;
      const NewPosition: Integer);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPlayPauseClick(Sender: TObject);

    // Callbacks
    procedure DeckEngineEnded(Sender: TObject);
    procedure DeckEngineError(Sender: TObject;
                              const Hr: HRESULT;
                              const Msg: string);
    procedure DeckEngineReady(Sender: TObject);

    procedure DeckEngineProcessed(Sender: TObject;
                                  const Position100ns: Int64;
                                  const RawPosition: UInt64);
    procedure DeckEngineStateChanged(Sender: TObject;
                                     const NewState: TDeviceState);

    procedure DeckTick(Sender: TObject;
                       const Position100ns: Int64;
                       const CurrentBpm: Double;
                       const BeatPhase: Double);

    procedure DeckBeat(Sender: TObject;
                       const Position100ns: Int64;
                       const BeatNumber: Int64;
                       const CurrentBpm: Double);

    // -----------------------------------------------------------------
    procedure BeforeDestruction; override;
    procedure FormDestroy(Sender: TObject);
    procedure btnPFLClick(Sender: TObject);
    procedure tbBalanceChange(Sender: TObject);
    procedure btnEqEnableClick(Sender: TObject);
    procedure tbEqGainDbChange(Sender: TObject);
    procedure tbEqCenterFreqHzChange(Sender: TObject);
    procedure tbEqQChange(Sender: TObject);
    procedure tbVolumeDblClick(Sender: TObject);
    procedure chkPlayListClick(Sender: TObject);
    procedure chkRepeatPlayClick(Sender: TObject);
    procedure chkPlayListShuffleClick(Sender: TObject);
    procedure btnPlayListNextClick(Sender: TObject);
    procedure btnPlayListPrevClick(Sender: TObject);
    procedure tbBalanceDblClick(Sender: TObject);
    procedure btnSetDownbeatClick(Sender: TObject);
    procedure tbInputGainChange(Sender: TObject);
    procedure tbInputGainDblClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure tbPitchDblClick(Sender: TObject);
    procedure tbEqCenterFreqHzDblClick(Sender: TObject);
    procedure tbEqQDblClick(Sender: TObject);
    procedure tbEqGainDbDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    FChannelIndex: Integer;
    FFileName: TFileName;
    FAudioDuration: LONGLONG;
    FAudioPosition100ns: LONGLONG;
    FCueEnabled: Boolean;
    FVolPosLast: Integer;
    FApplyingXFade: Boolean;
    FMixerChannel: TRDJMixerChannel;
    FDeckEngine: TMfChannelDeckEngine;
    FAudioRack: TMfWasApiEffectsRack;
    FUpdatingAudioRackGui: Boolean;
    // bpm
    FIgnoreBpmEditChange: Boolean;
    FBeatPulseActive: Boolean;
    // Playlist
    FPlaylistDb: TRDJPlaylistDb;
    FTrackLibrary: TRDJTrackLibrary;
    FPlaylistMgr: TRDJPlaylistManager;
    FCurrentPlaylist: TRDJPlaylist;
    FCurrentPlaylistIndex: Integer;
    FPlayingPlaylist: Boolean;
    FPlaylistPopup: TPopupMenu;
    FRepeatPlay: Boolean;
    FPlaylistShuffle: Boolean;
    FNextPlaylistPath: string;  // When a track starts, resolve the next track path and store it.
    FShuffleHistory: TList<Integer>;
    FShuffleHistoryPos: Integer;
    FSuppressPlaylistAutoAdvanceOnce: Boolean;
    // Icecast
    FLastSentNowPlaying: string;
    FFileNameParser: TFileNameParser;

    procedure DetachDeckEngineEvents();
    procedure BindAudioRack();
    procedure LoadAudioRackToGui();
    function GetEqEffect(): TMfParametricEqEffect;
    function GetEqSlot(): TMfWasApiFxSlot;

    // Udate GUI when state changes.
    procedure UpdateTransportUi(const AState: TDeviceState);
    procedure UpdateTransportUiFromEngine();

    // bpm
    procedure UpdateBpmGuiFromEngine();
    procedure ResetBeatLamp();

    // Helper
    procedure RegisterToMixer();
    procedure UnregisterFromMixer();
    procedure UpdateMixerChannelState();

    procedure EngineOutputPcmToVU(Sender: TObject;
                                  pData: PByte;
                                  const ByteCount: DWORD;
                                  Wfx: PWAVEFORMATEX);

    procedure GetMixerVolumes(out AVolL,
                              AVolR: Single);
    procedure SetVolumeChannels(AInvert: Boolean);
    procedure ApplyInputGainToEngine();

    procedure ApplyTwoDeckXFade(const NewPos: Integer);

    // NOTE: When using the TTrackBar, parameter tb = TTrackBar, AInvert (inversion) should be set to True.
    //       When using the TMfTrackBar, you can set it's behaviour like TTrackBar AInvert (inversion) = True.
    function InvertTrackbarPos(const tb: TMfTrackBar;
                               AInvert: Boolean): Integer;

    // Playlist
    procedure EnsurePlaylistInfra();
    procedure FreePlaylistInfra();

    procedure BuildPlaylistPopup();
    procedure PlaylistPopupItemClick(Sender: TObject);

    function PlayPlaylistEntry(const AIndex: Integer;
                               const AAutoStart: Boolean = True): HRESULT;
    function PlayNextPlaylistEntry(): HRESULT;

    function PlayPrevPlaylistEntry(): HRESULT;
    function ResolveNextPlaylistIndex(): Integer;
    function ResolvePrevPlaylistIndex(): Integer;
    procedure UpdateNextPlaylistPath();
    procedure ResetShuffleHistory();
    procedure AddCurrentToShuffleHistory(const AIndex: Integer);

    // Icecast
    function BuildDisplaySongText(const AFileName: string): string;
    procedure SetStatusSongText(const APrefix: string;
                                const AFileName: string);
    procedure NotifyBroadcastNowPlaying();

  public

    // Playlist
    function PlayPlaylistByID(const APlaylistID: Integer): HRESULT;
    function LoadPlaylistByID(const APlaylistID: Integer): HRESULT;
    //function PlaySingleTrack(const ATrackFileName: string): HRESULT;
    function LoadSingleTrack(const ATrackFileName: string;
                             const APlayAfterLoad: Boolean = False): HRESULT;

    procedure StopPlaylistMode();

    procedure SyncToDeck(AMasterDeck: TfrmChannelDeck);
    procedure ApplyExternalCrossFadeDelta(const ADelta: Integer);

    property ChannelIndex: Integer read FChannelIndex write FChannelIndex;
    property CueEnabled: Boolean read FCueEnabled write FCueEnabled;
  end;


implementation

{$R *.dfm}

uses
  System.Math,
  frmMainMDI,
  frmMasterDeck;


procedure TfrmChannelDeck.btnEqEnableClick(Sender: TObject);
var
  Slot: TMfWasApiFxSlot;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Slot := GetEqSlot();
  if not Assigned(Slot) then
    Exit;

  Slot.Enabled := btnEqEnable.Checked;
end;


procedure TfrmChannelDeck.btnPlayPauseClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(FDeckEngine) then
    Exit;

  case FDeckEngine.DeviceState of

    dsPlay:
      begin

        hr := FDeckEngine.Pause();
      end;

    dsReady,
    dsStop:
      begin

        pbProgress.Position := 0;

        // Keep volume on previous volume.
        SetVolumeChannels(False);
        RegisterToMixer();
        UpdateMixerChannelState();

        NotifyBroadcastNowPlaying();
        hr := FDeckEngine.Start();
      end;

    dsPause:
      begin

        hr := FDeckEngine.Start();
      end;

  else
    Exit;
  end;

  if FAILED(hr) then
    begin

      lblStatus.Caption := Format('Play/Pause failed for file: %s with Error: %d',
                                  [ExtractFileName(FFileName), hr]);
      lblStatus.Hint := lblStatus.Caption;
    end;

end;


procedure TfrmChannelDeck.btnSetDownbeatClick(Sender: TObject);
begin

  if Assigned(FDeckEngine) then
    FDeckEngine.BeatOffset100ns := FDeckEngine.Position100ns;
end;


procedure TfrmChannelDeck.btnStopClick(Sender: TObject);
var
  hr: HResult;

begin

  // Do NOT stop playlist mode here.
  // Stop should stop transport only, while keeping the playlist armed.
  hr := FDeckEngine.Stop();

  if FAILED(hr) then
    begin

      lblStatus.Caption := Format('Stop failed for file: %s with Error: %d',
                                [ExtractFileName(FFileName), hr]);
      lblStatus.Hint := lblStatus.Caption;
    end;
end;


procedure TfrmChannelDeck.chkCrossFadeClick(Sender: TObject);
var
  i: Integer;
  Count: Integer;
  Other: TfrmChannelDeck;

begin

  if not chkCrossFade.Checked then
    Exit;

  // Count how many are checked now
  Count := 0;
  for i := 0 to MainMDIFrm.Setup.ChannelCount - 1 do
    if (MainMDIFrm.FChannelDecks[i] <> nil) and MainMDIFrm.FChannelDecks[i].chkCrossFade.Checked then
      Inc(Count);

  // If > 2, uncheck one other deck (first found that's not Self)
  if (Count > 2) then
    begin

      for i := 0 to MainMDIFrm.Setup.ChannelCount - 1 do
        begin

          Other := MainMDIFrm.FChannelDecks[i];
          if (Other <> nil) and (Other <> Self) and Other.chkCrossFade.Checked then
            begin

              Other.chkCrossFade.Checked := False;
              Break;
            end;
        end;
    end;
end;


procedure TfrmChannelDeck.FormCreate(Sender: TObject);
begin

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);

  SetWindowTheme(pbProgress.Handle,
                 '',
                 '');
  pbProgress.BackgroundColor := $00353801;

  // DO NOT HARDCODE component settings here: We use Object inspector for that, to prevent MDI errors.
  FDeckEngine := TMfChannelDeckEngine.Create(Self);
  BindAudioRack();
  LoadAudioRackToGui();

  FDeckEngine.OnStateChanged := DeckEngineStateChanged;
  FDeckEngine.OnError := DeckEngineError;
  FDeckEngine.OnProcessedPCM := EngineOutputPcmToVU;
  FDeckEngine.OnProcessed := DeckEngineProcessed;
  //FDeckEngine.OnProcessPcm := DeckEngineProcessPcm;   // optional
  FDeckEngine.OnReady := DeckEngineReady;
  FDeckEngine.OnEnded := DeckEngineEnded;
  FDeckEngine.OnDeckTick := DeckTick;
  FDeckEngine.OnBeat := DeckBeat;

  FPlaylistDb := nil;
  FTrackLibrary := nil;
  FPlaylistMgr := nil;
  FCurrentPlaylist := nil;
  FCurrentPlaylistIndex := -1;
  FPlayingPlaylist := False;
  FPlaylistPopup := TPopupMenu.Create(Self);
  FRepeatPlay := False;
  FPlaylistShuffle := False;
  FNextPlaylistPath := '';
  FShuffleHistory := TList<Integer>.Create;
  FShuffleHistoryPos := -1;
  FSuppressPlaylistAutoAdvanceOnce := False;
  Randomize;

  // Register once when possible. RegisterToMixer is idempotent and will do
  // nothing if the main mixer is not ready yet.
  RegisterToMixer();

  FIgnoreBpmEditChange := False;
  FBeatPulseActive := False;

  lblBpm.Caption := '--.-- BPM';
  ResetBeatLamp();

  FFileNameParser := TFileNameParser.Create;
end;


procedure TfrmChannelDeck.FormDestroy(Sender: TObject);
begin

  // 1) Unregister when the deck is going to be destroyed.
  UnregisterFromMixer();

  // 2) Detach callbacks FIRST (prevents any further calls into this form)
  if Assigned(FDeckEngine) then
    begin

      FDeckEngine.OnStateChanged := nil;
      FDeckEngine.OnError := nil;
      FDeckEngine.OnProcessedPCM := nil;
      FDeckEngine.OnProcessed := nil;
      FDeckEngine.OnReady := nil;
      FDeckEngine.OnDeckTick := nil;
      FDeckEngine.OnBeat := nil;
      FDeckEngine.OnBpmAnalyzed := nil;

      FreeAndNil(FDeckEngine);
    end;

  FreePlaylistInfra();
  FreeAndNil(FPlaylistPopup);
  FreeAndNil(FShuffleHistory);
  FreeAndNil(FMixerChannel);
  FreeAndNil(FFileNameParser);
end;


procedure TfrmChannelDeck.FormShow(Sender: TObject);
begin


  //Height := 1538;
  //Width := 466;
end;


procedure TfrmChannelDeck.AfterConstruction();
var
  setup: TRDJSetup;

begin

  inherited AfterConstruction;

  // Once during setup.
  pmLeft.InputSource := isWasapiEngine;
  pmRight.InputSource := isWasapiEngine;

  if Assigned(FDeckEngine) then
    begin

      FDeckEngine.OnProcessedPCM := EngineOutputPcmToVU;
      // optional PCM tap / rack hook (pre/post FX depending where we call it in the engine)
      //FDeckEngine.Engine.OnProcessPcm := EngineAProcessPcm;

      BindAudioRack();
      LoadAudioRackToGui();
      ApplyInputGainToEngine();
    end;

  // Apply frozen global setup.
  setup := GetGlobalSetup();

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);
end;


procedure TfrmChannelDeck.BeforeDestruction();
begin

  DetachDeckEngineEvents();

  inherited;
end;


procedure TfrmChannelDeck.DetachDeckEngineEvents();
begin

  if not Assigned(FDeckEngine) then
    Exit;

  FDeckEngine.OnStateChanged := nil;
  FDeckEngine.OnDeckTick := nil;
  FDeckEngine.OnBeat := nil;
  FDeckEngine.OnBpmAnalyzed := nil;
  FDeckEngine.OnProcessed := nil;
  FDeckEngine.OnOutputPcm := nil;
  FDeckEngine.OnError := nil;
end;

// Audio rack ------------------------------------------------------------------
procedure TfrmChannelDeck.BindAudioRack();
begin

  FAudioRack := nil;

  if Assigned(FDeckEngine) then
    FAudioRack := FDeckEngine.AudioRack;
end;


procedure TfrmChannelDeck.LoadAudioRackToGui();
var
  Eq: TMfParametricEqEffect;

begin

  if not Assigned(FAudioRack) then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  FUpdatingAudioRackGui := True;

  try

    btnEqEnable.Checked := Assigned(GetEqSlot()) and GetEqSlot().Enabled;

    tbEqGainDb.Position := Round(Eq.GainDb);
    tbEqCenterFreqHz.Position := Round(Eq.CenterFreqHz);
    tbEqQ.Position := Round(Eq.Q * 10.0);
  finally

    FUpdatingAudioRackGui := False;
  end;

  // Placeholder for future channel FX GUI wiring.
  // The deck EQ section is intended to be fixed and always visible;
  // this method will load the current engine-owned EQ state into the controls.
end;


function TfrmChannelDeck.GetEqEffect(): TMfParametricEqEffect;
var
  i: Integer;
  Fx: TObject;

begin

  Result := nil;

  if not Assigned(FAudioRack) then
    Exit;

  for i := 0 to FAudioRack.Slots.Count - 1 do
    begin

      Fx := TMfWasApiFxSlot(FAudioRack.Slots[i]).Effect;

      if Assigned(Fx) and
        (Fx is TMfParametricEqEffect) then
        begin

          Result := TMfParametricEqEffect(Fx);
          Exit;
        end;
    end;
end;


function TfrmChannelDeck.GetEqSlot(): TMfWasApiFxSlot;
begin

  Result := nil;

  if not Assigned(FAudioRack) then
    Exit;

  Result := FAudioRack.FindFirstSlotByEffectClass(TMfParametricEqEffect);
end;
// Audio rack end --------------------------------------------------------------

procedure TfrmChannelDeck.UpdateTransportUi(const AState: TDeviceState);
begin

  case AState of
    dsPlay:
      begin

        btnPlayPause.Caption := 'Pause';
        btnPlayPause.Enabled := True;
        btnStop.Enabled := True;

        SetStatusSongText('Playing',
                          FFileName);

        // Activate the peakmeters.
        pmLeft.Enabled := True;
        pmRight.Enabled := True;
        // bpm
        UpdateBpmGuiFromEngine();
      end;

    dsPause:
      begin

        btnPlayPause.Caption := 'Play';
        btnPlayPause.Enabled := True;
        btnStop.Enabled := True;
        SetStatusSongText('Pauzed',
                          FFileName);
        ResetBeatLamp();
        UpdateBpmGuiFromEngine();

        pmLeft.AudioEnded();
        pmRight.AudioEnded();
      end;

    dsReady:
      begin

        btnPlayPause.Caption := 'Play';
        btnPlayPause.Enabled := True;
        btnStop.Enabled := False;

        lblPlayed.Caption := 'Played: 00:00:00';
        //lblProcessed.Caption := 'Samples: 0';
      end;

    dsStop:
      begin

        btnPlayPause.Caption := 'Play';
        btnPlayPause.Enabled := True;
        btnStop.Enabled := False;

        //FCtlUpdateTrottle := 0;
        pbProgress.Position := 0;
        lblPlayed.Caption := 'Played: 00:00:00';
        //lblProcessed.Caption := 'Samples: 0';
        lblBarPositionTime.Caption := '00:00:00';

        // Deactivate the peakmeters.
        pmLeft.AudioEnded();
        pmRight.AudioEnded();
        pmLeft.Enabled := False;
        pmRight.Enabled := False;
        SetStatusSongText('Stopped',
                          FFileName);
        ResetBeatLamp();
        UpdateBpmGuiFromEngine();
      end;

    dsLoading:
      begin

        btnPlayPause.Caption := 'Play';
        btnPlayPause.Enabled := False;
        btnStop.Enabled := False;
      end;
  else
      begin

        btnPlayPause.Caption := 'Play';
        btnPlayPause.Enabled := False;
        btnStop.Enabled := False;
      end;
  end;
end;


procedure TfrmChannelDeck.UpdateTransportUiFromEngine();
begin

  if Assigned(FDeckEngine) then
    UpdateTransportUi(FDeckEngine.DeviceState)
  else
    UpdateTransportUi(dsStop);
end;

// bpm -------------------------------------------------------------------------
procedure TfrmChannelDeck.UpdateBpmGuiFromEngine();
begin

  if not Assigned(FDeckEngine) then
    Exit;

  FIgnoreBpmEditChange := True;

  if (FDeckEngine.GetCurrentBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [FDeckEngine.GetCurrentBpm])
  else
    if (FDeckEngine.TrackBpm > 0.0) then
      lblBpm.Caption := Format('%.2f BPM',
                               [FDeckEngine.TrackBpm])
    else
      lblBpm.Caption := '--.-- BPM';
end;


procedure TfrmChannelDeck.ResetBeatLamp();
begin

  FBeatPulseActive := False;
  bldBeat.LedOffColor := $004A3809;
end;
// bpm end ---------------------------------------------------------------------


// Register the deck in the mixer.
procedure TfrmChannelDeck.RegisterToMixer();
begin

  if Assigned(FMixerChannel) then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  if not Assigned(MainMDIFrm.InternalMixer) then
    Exit;

  if not Assigned(FDeckEngine) then
    Exit;

  FMixerChannel := MainMDIFrm.InternalMixer.FindChannelByDeckEngine(FDeckEngine);
  if not Assigned(FMixerChannel) then
    FMixerChannel := MainMDIFrm.InternalMixer.AddChannel();

  FMixerChannel.DeckEngineObj := FDeckEngine;
  FMixerChannel.OnReadOutputPcmFloat32 := FDeckEngine.ReadOutputPcmFloat32;

  BindAudioRack();
  LoadAudioRackToGui();
  UpdateMixerChannelState();
end;


// Unregister when the deck is destroyed.
procedure TfrmChannelDeck.UnregisterFromMixer();
begin

  if Assigned(FMixerChannel) and
     Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.InternalMixer) then
    MainMDIFrm.InternalMixer.RemoveChannel(FMixerChannel);

  FMixerChannel := nil;
end;


procedure TfrmChannelDeck.UpdateMixerChannelState();
var
  VolL: Single;
  VolR: Single;

begin

  if not Assigned(FMixerChannel) then
    begin

      RegisterToMixer();

      if not Assigned(FMixerChannel) then
        Exit;
    end;

  FMixerChannel.Enabled := True;
  FMixerChannel.Muted := chkMute.Checked;
  FMixerChannel.CueEnabled := btnPFL.Checked;

  GetMixerVolumes(VolL,
                  VolR);

  FMixerChannel.VolL := VolL;
  FMixerChannel.VolR := VolR;

  // PFL is pre-fader.
  FMixerChannel.CueVolL := 1.0;
  FMixerChannel.CueVolR := 1.0;
end;


procedure TfrmChannelDeck.EngineOutputPcmToVU(Sender: TObject;
                                              pData: PByte;
                                              const ByteCount: DWORD;
                                              Wfx: PWAVEFORMATEX);
begin

  // Reset both when the engine signals silence / stop
  if (pData = nil) or (ByteCount = 0) then
    begin

      pmLeft.AudioEnded();
      pmRight.AudioEnded();
      Exit;
    end;

  // Same stereo buffer goes to both meters; each meter picks its own channel.
  pmLeft.PushPcm(pData,
                 ByteCount,
                 Wfx);

  pmRight.PushPcm(pData,
                  ByteCount,
                  Wfx);
end;


procedure TfrmChannelDeck.GetMixerVolumes(out AVolL,
                                          AVolR: Single);

  function Clamp01(const x: Single): Single; inline;
  begin

    if (x < 0) then
      Exit(0);
    if (x > 1) then
      Exit(1);
    Result := x;
  end;

  procedure CalcStereoVolumes_EqualPower(const VolPos,
                                         BalPos: Integer;
                                         out L,
                                         R: Single);
  var
    baseVol: Single;
    pan: Single;
    angle: Double;

  begin

    // Volume slider -> 0..1
    baseVol := Clamp01(MapRange(VolPos,
                                tbVolume.Minimum,
                                tbVolume.Maximum,
                                0.0,
                                1.0));

    // Balance slider expected -100..+100
    pan := Clamp01((BalPos + 100) / 200);

    angle := pan * (Pi / 2.0);

    L := baseVol * Cos(angle);
    R := baseVol * Sin(angle);
  end;

begin

  CalcStereoVolumes_EqualPower(tbVolume.Position,
                               tbBalance.Position,
                               AVolL,
                               AVolR);
end;


// Calculates volume gains left/right/balance.
procedure TfrmChannelDeck.SetVolumeChannels(AInvert: Boolean);
var
  hr: HResult;
  volL: Single;
  volR: Single;

begin

  if not Assigned(FDeckEngine) then
    Exit;

  hr := E_FAIL;

  if (FDeckEngine.SoundChannels = 2) then
    begin

      GetMixerVolumes(volL,
                      volR);

      // Don't use this one, it will side effects the COM/WASAPI behaviour (calling from GUI is wrong, it needs to run on the engine thread!)
      // (unsafe from GUI thread)
      //hr := FDeckEngine.SetVolumes(vL,
      //                                      vR);
      // So, we use this safe call, (safe from GUI thread)
      //hr := FDeckEngine.SetVolumesAsync(volL,
      //                                           volR);

      //if SUCCEEDED(hr) then
      //  FDeckEngine.SetMeterFaderGains(volL,
      //                                          volR);
    end;

  if FAILED(hr) then
    begin

      lblStatus.Caption := Format('Adjusting volumes failed with error: %d.',
                                  [hr]);
      lblStatus.Hint := lblStatus.Caption;
    end;
end;


procedure TfrmChannelDeck.ApplyInputGainToEngine();
var
  GainDb: Single;

begin

  GainDb := tbInputGain.Position / 10.0;

  if (GainDb > 0) then
    lblInputGainValue.Caption := '+' + FormatFloat('0.0',
                                                   GainDb) + ' dB'
  else
    lblInputGainValue.Caption := FormatFloat('0.0',
                                             GainDb) + ' dB';

  if Assigned(FDeckEngine) then
    FDeckEngine.InputGainDb := GainDb;
end;


procedure TfrmChannelDeck.ApplyTwoDeckXFade(const NewPos: Integer);
var
  Delta: Integer;
  i: Integer;
  Other: TfrmChannelDeck;

  function ClampI(const x,
                  a,
                  b: Integer): Integer; inline;
  begin

    if (x < a) then
      Exit(a);
    if (x > b) then
      Exit(b);
    Result := x;
  end;

begin

  if FApplyingXFade then
    Exit;

  if not chkCrossFade.Checked then
    Exit;

  Delta := NewPos - FVolPosLast;
  if (Delta = 0) then
    Exit;

  Other := nil;
  for i := 0 to MainMDIFrm.Setup.ChannelCount - 1 do
    if (MainMDIFrm.FChannelDecks[i] <> nil) and
       (MainMDIFrm.FChannelDecks[i] <> Self) and
       MainMDIFrm.FChannelDecks[i].chkCrossFade.Checked then
    begin

      Other := MainMDIFrm.FChannelDecks[i];
      Break;
    end;

  if (Other = nil) then
    Exit;

  FApplyingXFade := True;
  try

    Other.FApplyingXFade := True;

    try

      Other.tbVolume.Position := ClampI(Other.tbVolume.Position - Delta,
                                        Other.tbVolume.Minimum,
                                        Other.tbVolume.Maximum);

      Other.FVolPosLast := Other.tbVolume.Position;
      Other.UpdateMixerChannelState();
    finally

      Other.FApplyingXFade := False;
    end;
  finally

    FApplyingXFade := False;
  end;
end;


procedure TfrmChannelDeck.ApplyExternalCrossFadeDelta(const ADelta: Integer);

  function ClampI(const x, a, b: Integer): Integer; inline;
  begin
    if (x < a) then
      Exit(a);
    if (x > b) then
      Exit(b);
    Result := x;
  end;

begin

  if (ADelta = 0) then
    Exit;

  FApplyingXFade := True;
  try

    tbVolume.Position := ClampI(tbVolume.Position - ADelta,
                                tbVolume.Minimum,
                                tbVolume.Maximum);
    FVolPosLast := tbVolume.Position;
    UpdateMixerChannelState();
  finally

    FApplyingXFade := False;
  end;
end;


// When using the TTrackBar, parameter tb = TTrackBar, AInvert (inversion) should be set to True.
function TfrmChannelDeck.InvertTrackbarPos(const tb: TMfTrackBar;
                                           AInvert: Boolean): Integer;
begin

  if (AInvert = True) then
    Result := tb.Maximum - tb.Position + tb.Minimum  // Flips negative -> positive.
  else
    Result := tb.Position;
end;


procedure TfrmChannelDeck.jgsProgressChange(Sender: TObject;
  const NewPosition: Integer);
begin
  //

end;


// Playlist --------------------------------------------------------------------

procedure TfrmChannelDeck.EnsurePlaylistInfra();
var
  DbFileName: string;

begin

  if Assigned(FPlaylistMgr) then
    Exit;

  FPlaylistDb := TRDJPlaylistDb.Create();

  DbFileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                'Data\RDJLibrary.db';

  FPlaylistDb.Open(DbFileName);

  FTrackLibrary := TRDJTrackLibrary.Create(FPlaylistDb);
  FPlaylistMgr := TRDJPlaylistManager.Create(FPlaylistDb,
                                             FTrackLibrary);
end;


procedure TfrmChannelDeck.FreePlaylistInfra();
begin

  FreeAndNil(FCurrentPlaylist);
  FreeAndNil(FPlaylistMgr);
  FreeAndNil(FTrackLibrary);

  if Assigned(FPlaylistDb) then
    begin

      FPlaylistDb.Close();
      FreeAndNil(FPlaylistDb);
    end;
end;


procedure TfrmChannelDeck.StopPlaylistMode();
begin

  FPlayingPlaylist := False;
  FCurrentPlaylistIndex := -1;
  FNextPlaylistPath := '';
  FSuppressPlaylistAutoAdvanceOnce := False;
  ResetShuffleHistory();

  if Assigned(chkPlayList) then
    chkPlayList.Checked := False;
end;


procedure TfrmChannelDeck.BuildPlaylistPopup();
var
  L: TList<TRDJPlaylistInfo>;
  i: Integer;
  Info: TRDJPlaylistInfo;
  MI: TMenuItem;

begin

  EnsurePlaylistInfra();

  FPlaylistPopup.Items.Clear;

  L := FPlaylistMgr.GetAllPlaylists();
  try

    if (L.Count = 0) then
      begin

        MI := TMenuItem.Create(FPlaylistPopup);
        MI.Caption := '(No playlists found)';
        MI.Enabled := False;
        FPlaylistPopup.Items.Add(MI);
        Exit;
      end;

    for i := 0 to L.Count - 1 do
      begin

        Info := L[i];

        MI := TMenuItem.Create(FPlaylistPopup);
        MI.Caption := Info.Name;
        MI.Tag := Info.PlaylistID;
        MI.OnClick := PlaylistPopupItemClick;
        FPlaylistPopup.Items.Add(MI);
      end;
  finally

    L.Free;
  end;
end;


procedure TfrmChannelDeck.PlaylistPopupItemClick(Sender: TObject);
var
  PlaylistID: Integer;

begin

  if not (Sender is TMenuItem) then
    Exit;

  PlaylistID := TMenuItem(Sender).Tag;
  if (PlaylistID <= 0) then
    Exit;

  if FAILED(PlayPlaylistByID(PlaylistID)) then
    begin

      chkPlayList.Checked := False;
      lblStatus.Caption := 'Failed to play selected playlist.';
      lblStatus.Hint := lblStatus.Caption;
    end;
end;


function TfrmChannelDeck.PlayPlaylistByID(const APlaylistID: Integer): HRESULT;
begin

  Result := E_FAIL;

  if (APlaylistID <= 0) then
    Exit;

  EnsurePlaylistInfra();

  FreeAndNil(FCurrentPlaylist);
  FCurrentPlaylist := FPlaylistMgr.LoadPlaylist(APlaylistID);

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if (FCurrentPlaylist.Count <= 0) then
    begin

      lblStatus.Caption := 'Selected playlist is empty.';
      lblStatus.Hint := lblStatus.Caption;
      Exit;
    end;

  FRepeatPlay := chkRepeatPlay.Checked;
  FPlayingPlaylist := True;
  FCurrentPlaylistIndex := 0;
  chkPlayList.Checked := True;
  ResetShuffleHistory();

  Result := PlayPlaylistEntry(FCurrentPlaylistIndex,
                              True);
end;


function TfrmChannelDeck.LoadPlaylistByID(const APlaylistID: Integer): HRESULT;
begin

  Result := E_FAIL;

  if (APlaylistID <= 0) then
    Exit;

  EnsurePlaylistInfra();

  FreeAndNil(FCurrentPlaylist);
  FCurrentPlaylist := FPlaylistMgr.LoadPlaylist(APlaylistID);

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if (FCurrentPlaylist.Count <= 0) then
    begin

      lblStatus.Caption := 'Selected playlist is empty.';
      lblStatus.Hint := lblStatus.Caption;
      Exit;
    end;

  FRepeatPlay := chkRepeatPlay.Checked;
  FPlayingPlaylist := True;
  FCurrentPlaylistIndex := 0;
  chkPlayList.Checked := True;
  ResetShuffleHistory();

  Result := PlayPlaylistEntry(FCurrentPlaylistIndex,
                              False);
end;


{function TfrmChannelDeck.PlaySingleTrack(const ATrackFileName: string): HRESULT;
var
  hr: HRESULT;

begin

  if (Trim(ATrackFileName) = '') then
    begin

      Result := E_POINTER;
      Exit;
    end;

  if not FileExists(ATrackFileName) then
    begin

      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  StopPlaylistMode();

  FFileName := ATrackFileName;

  hr := FDeckEngine.OpenFile(FFileName);
  if FAILED(hr) then
    begin

      Result := hr;
      Exit;
    end;

  FAudioDuration := FDeckEngine.Duration100ns;
  FAudioPosition100ns := FDeckEngine.Position100ns;

  pbProgress.Max := Integer(FAudioDuration div HNS_PER_100MS);
  pbProgress.Position := 0;

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(FAudioDuration,
                                              False)]);

  lblStatus.Caption := Format('Loaded: %s',
                              [ExtractFileName(FFileName)]);

  UpdateBpmGuiFromEngine();

  btnPlayPauseClick(Self);

  Result := S_OK;
end;}


function TfrmChannelDeck.LoadSingleTrack(const ATrackFileName: string;
                                         const APlayAfterLoad: Boolean = False): HRESULT;
var
  hr: HRESULT;

begin

  if (Trim(ATrackFileName) = '') then
    begin

      Result := E_POINTER;
      Exit;
    end;

  if not FileExists(ATrackFileName) then
    begin

      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  StopPlaylistMode();

  FFileName := ATrackFileName;
  FLastSentNowPlaying := '';

  hr := FDeckEngine.OpenFile(FFileName);
  if FAILED(hr) then
    begin

      Result := hr;
      Exit;
    end;

  FAudioDuration := FDeckEngine.Duration100ns;
  FAudioPosition100ns := FDeckEngine.Position100ns;

  pbProgress.Maximum := Integer(FAudioDuration div HNS_PER_100MS);
  pbProgress.Position := 0;

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(FAudioDuration,
                                              False)]);

  SetStatusSongText('Loaded',
                    FFileName);

  UpdateBpmGuiFromEngine();

  if APlayAfterLoad then
    btnPlayPauseClick(Self);

  Result := S_OK;
end;


procedure TfrmChannelDeck.btnLoadClick(Sender: TObject);
var
  SelectedFile: TFileName;
  hr: HRESULT;

begin

  StopPlaylistMode();

  DlgLWFileBrowserEx.ShowModal;

  if (DlgLWFileBrowserEx.ModalResult = mrOk) then
    SelectedFile := DlgLWFileBrowserEx.FileURI
  else
    Exit;

  btnLoad.Enabled := False;

  try

    lblStatus.Caption := Format('Loading file: %s',
                                [ExtractFileName(SelectedFile)]);
    lblStatus.Hint := lblStatus.Caption;

    hr := LoadSingleTrack(SelectedFile,
                          False);

    if FAILED(hr) then
      begin

        lblStatus.Caption := Format('Failed to load file: %s',
                                    [ExtractFileName(SelectedFile)]);
        lblStatus.Hint := lblStatus.Caption;

        ShowMessage(Format('Failed to load file: %s' + sLineBreak +
                           'HRESULT: $%.8x',
                           [ExtractFileName(SelectedFile),
                            Cardinal(hr)]));
        Exit;
      end;
  finally

    btnLoad.Enabled := True;
  end;


end;


function TfrmChannelDeck.PlayPlaylistEntry(const AIndex: Integer;
                                           const AAutoStart: Boolean = True): HRESULT;
var
  hr: HRESULT;
  Entry: TRDJPlaylistEntry;
  TrackPath: string;

begin

  Result := E_FAIL;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if (AIndex < 0) or (AIndex >= FCurrentPlaylist.Count) then
    Exit;

  Entry := FCurrentPlaylist[AIndex];
  TrackPath := Trim(Entry.Track.FullPath);

  if (TrackPath = '') then
    begin

      Result := ERROR_PATH_NOT_FOUND;
      lblStatus.Caption := Format('Playlist item %d has no file path.',
                                  [AIndex + 1]);
      lblStatus.Hint := lblStatus.Caption;
      Exit;
    end;

  if not FileExists(TrackPath) then
    begin

      Result := ERROR_FILE_NOT_FOUND;
      lblStatus.Caption := Format('Missing file in playlist: %s',
                                  [ExtractFileName(TrackPath)]);
      lblStatus.Hint := lblStatus.Caption;
      Exit;
    end;

  FCurrentPlaylistIndex := AIndex;
  FFileName := TrackPath;

  hr := FDeckEngine.OpenFile(FFileName);
  if FAILED(hr) then
    begin

      Result := hr;
      lblStatus.Caption := Format('Failed to load playlist item: %s',
                                  [ExtractFileName(FFileName)]);
      lblStatus.Hint := lblStatus.Caption;
      Exit;
    end;

  FRepeatPlay := chkRepeatPlay.Checked;

  if FPlaylistShuffle then
    begin

      if (FShuffleHistoryPos < 0) or
         (FShuffleHistory[FShuffleHistoryPos] <> AIndex) then
        AddCurrentToShuffleHistory(AIndex);
    end;

  FAudioDuration := FDeckEngine.Duration100ns;
  FAudioPosition100ns := FDeckEngine.Position100ns;

  pbProgress.Maximum := Integer(FAudioDuration div HNS_PER_100MS);

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(FAudioDuration,
                                              False)]);

  lblStatus.Caption := Format('Playlist %s (%d/%d): %s',
                              [FCurrentPlaylist.Info.Name,
                               FCurrentPlaylistIndex + 1,
                               FCurrentPlaylist.Count,
                               BuildDisplaySongText(FFileName)]);
  lblStatus.Hint := lblStatus.Caption;

  UpdateNextPlaylistPath();

  UpdateBpmGuiFromEngine();

  Result := S_OK;

  if AAutoStart then
    btnPlayPauseClick(Self);
end;


function TfrmChannelDeck.PlayNextPlaylistEntry(): HRESULT;
var
  NextIndex: Integer;

begin

  Result := E_FAIL;

  if not FPlayingPlaylist then
    Exit;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  NextIndex := ResolveNextPlaylistIndex();
  if (NextIndex < 0) then
    begin

      lblStatus.Caption := Format('Playlist finished: %s',
                                  [FCurrentPlaylist.Info.Name]);
      lblStatus.Hint := lblStatus.Caption;
      StopPlaylistMode();
      Exit;
    end;

  if FPlaylistShuffle and (FShuffleHistoryPos < FShuffleHistory.Count - 1) and
     (FShuffleHistory[FShuffleHistoryPos + 1] = NextIndex) then
    Inc(FShuffleHistoryPos);

  Result := PlayPlaylistEntry(NextIndex,
                              True);
end;


function TfrmChannelDeck.ResolveNextPlaylistIndex(): Integer;
begin

  Result := -1;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if (FCurrentPlaylist.Count = 0) then
    Exit;

  FRepeatPlay := chkRepeatPlay.Checked;

  if FPlaylistShuffle then
    begin

      // If there is already forward history, use it first.
      if (FShuffleHistoryPos < FShuffleHistory.Count - 1) then
        begin

          Result := FShuffleHistory[FShuffleHistoryPos + 1];
          Exit;
        end;

      // One track playlist.
      if (FCurrentPlaylist.Count = 1) then
        begin

          if FRepeatPlay then
            Result := 0
          else
            Result := -1;

          Exit;
        end;

      // End of current shuffle cycle.
      if (FShuffleHistory.Count >= FCurrentPlaylist.Count) and
         (FShuffleHistoryPos >= FShuffleHistory.Count - 1) then
        begin

          if not FRepeatPlay then
            begin

              Result := -1;
              Exit;
            end;

          ResetShuffleHistory();
        end;

      repeat

        Result := Random(FCurrentPlaylist.Count);
      until (Result <> FCurrentPlaylistIndex) and
            (FShuffleHistory.IndexOf(Result) < 0);

      Exit;
    end;

  Result := FCurrentPlaylistIndex + 1;

  if (Result >= FCurrentPlaylist.Count) then
    begin

      if FRepeatPlay then
        Result := 0
      else
        Result := -1;
    end;
end;


function TfrmChannelDeck.ResolvePrevPlaylistIndex(): Integer;
begin

  Result := -1;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if (FCurrentPlaylist.Count = 0) then
    Exit;

  FRepeatPlay := chkRepeatPlay.Checked;

  if FPlaylistShuffle then
    begin

      if FShuffleHistoryPos > 0 then
        Result := FShuffleHistory[FShuffleHistoryPos - 1];

      Exit;
    end;

  Result := FCurrentPlaylistIndex - 1;

  if (Result < 0) then
    begin

      if FRepeatPlay then
        Result := FCurrentPlaylist.Count - 1
      else
        Result := -1;
    end;
end;


function TfrmChannelDeck.PlayPrevPlaylistEntry(): HRESULT;
var
  PrevIndex: Integer;

begin

  Result := E_FAIL;

  if not FPlayingPlaylist then
    Exit;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  PrevIndex := ResolvePrevPlaylistIndex();
  if (PrevIndex < 0) then
    Exit;

  if FPlaylistShuffle and (FShuffleHistoryPos > 0) and
     (FShuffleHistory[FShuffleHistoryPos - 1] = PrevIndex) then
    Dec(FShuffleHistoryPos);

  Result := PlayPlaylistEntry(PrevIndex,
                              True);
end;


procedure TfrmChannelDeck.UpdateNextPlaylistPath();
var
  NextIndex: Integer;
  Entry: TRDJPlaylistEntry;

begin

  FNextPlaylistPath := '';

  if not FPlayingPlaylist then
    Exit;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  NextIndex := ResolveNextPlaylistIndex();
  if (NextIndex < 0) then
    Exit;

  Entry := FCurrentPlaylist[NextIndex];
  FNextPlaylistPath := Trim(Entry.Track.FullPath);
end;


procedure TfrmChannelDeck.ResetShuffleHistory();
begin

  FShuffleHistory.Clear;
  FShuffleHistoryPos := -1;
end;


procedure TfrmChannelDeck.AddCurrentToShuffleHistory(const AIndex: Integer);
begin

  if not FPlaylistShuffle then
    Exit;

  if AIndex < 0 then
    Exit;

  // If user went backwards in history and then moves forward again,
  // discard everything ahead of the current history position.
  while (FShuffleHistory.Count - 1 > FShuffleHistoryPos) do
    FShuffleHistory.Delete(FShuffleHistory.Count - 1);

  FShuffleHistory.Add(AIndex);
  FShuffleHistoryPos := FShuffleHistory.Count - 1;
end;

// Playlist end ----------------------------------------------------------------

// Icecast

function TfrmChannelDeck.BuildDisplaySongText(const AFileName: string): string;
var
  Artist: string;
  Title: string;

begin

  Result := '';

  if (Trim(AFileName) = '') then
    Exit;

  FFileNameParser.ParseArtistTitleFromFileName(AFileName,
                                               Artist,
                                               Title);

  Result := FFileNameParser.BuildIceCastSongText(Artist,
                                                 Title);

  if (Result = '') then
    Result := ChangeFileExt(ExtractFileName(AFileName),
                            '');
end;


procedure TfrmChannelDeck.SetStatusSongText(const APrefix: string;
                                            const AFileName: string);
var
  SongText: string;

begin

  SongText := BuildDisplaySongText(AFileName);

  if SongText <> '' then
    lblStatus.Caption := Format('%s: %s',
                                [APrefix,
                                 SongText])
  else
    lblStatus.Caption := APrefix;

  lblStatus.Hint := lblStatus.Caption;
end;


procedure TfrmChannelDeck.NotifyBroadcastNowPlaying();
var
  Artist: string;
  Title: string;
  SongText: string;

begin

  if (Trim(FFileName) = '') then
    Exit;

  FFileNameParser.ParseArtistTitleFromFileName(FFileName,
                                               Artist,
                                               Title);

  SongText := FFileNameParser.BuildIceCastSongText(Artist,
                                                  Title);

  if (SongText = '') then
    Exit;

  if SameText(SongText,
              FLastSentNowPlaying) then
    Exit;

  if Assigned(MainMDIFrm) and
     Assigned(MainMDIFrm.IcecastEngine) then
    begin

      MainMDIFrm.IcecastEngine.UpdateNowPlaying(Artist,
                                                Title);
      FLastSentNowPlaying := SongText;
    end;
end;


// WasApi event handlers =======================================================

procedure TfrmChannelDeck.DeckEngineEnded(Sender: TObject);
var
  hr: HResult;

begin

  FRepeatPlay := chkRepeatPlay.Checked;

  UpdateTransportUiFromEngine();

  if FSuppressPlaylistAutoAdvanceOnce then
    begin

      FSuppressPlaylistAutoAdvanceOnce := False;
      Exit;
    end;

  if FPlayingPlaylist then
    begin

      PlayNextPlaylistEntry();
      Exit;
    end;

  if FRepeatPlay then
    begin

      hr := FDeckEngine.Stop();
      if SUCCEEDED(hr) then
        hr := FDeckEngine.Start();

      if SUCCEEDED(hr) then
        Exit;
    end;

  SetStatusSongText('Stopped',
                    FFileName);
end;


procedure TfrmChannelDeck.DeckEngineError(Sender: TObject;
                                          const Hr: HRESULT;
                                          const Msg: string);
begin

  lblStatus.Caption := Format('%s (error 0x%.8x)',
                              [Msg, Cardinal(Hr)]);
  lblStatus.Hint := lblStatus.Caption;
end;


procedure TfrmChannelDeck.DeckEngineProcessed(Sender: TObject;
                                                         const Position100ns: Int64;
                                                         const RawPosition: UInt64);
var
  iProgress: LONGLONG;
  //iSamples: LONGLONG;
  tstr: string;
  secPos: Integer;

begin

  if not Assigned(FDeckEngine) then
    Exit;

  if (FDeckEngine.DeviceState <> dsPlay) then
    Exit;

  if (pbProgress.Maximum <= 0) then
    Exit;

  iProgress := Position100ns;
  //iSamples := RawPosition;

  secPos := Integer(Position100ns div HNS_PER_100MS);

  if (secPos < 0) then
    secPos := 0;

  if (secPos > pbProgress.Maximum) then
    secPos := pbProgress.Maximum;

  pbProgress.Position := secPos;

  tstr := HnsTimeToStr(iProgress,
                       False);


  lblPlayed.Caption := Format('Played: %s',
                              [tstr]);

  // This will stop the flickering effect on the control.
  //inc(FCtlUpdateTrottle);
  //if (FCtlUpdateTrottle >= 60) and (iSamples > 1024) then
  //  begin

  //    lblProcessed.Caption := Format('Samples: %d kb',
  //                                   [iSamples div 1024]);
  //    FCtlUpdateTrottle := 0;
  //  end;
end;


procedure TfrmChannelDeck.DeckEngineReady(Sender: TObject);
begin

  UpdateTransportUiFromEngine();
end;


procedure TfrmChannelDeck.DeckEngineStateChanged(Sender: TObject;
                                                 const NewState: TDeviceState);
begin

  UpdateTransportUi(NewState);
end;


procedure TfrmChannelDeck.DeckTick(Sender: TObject;
                                   const Position100ns: Int64;
                                   const CurrentBpm: Double;
                                   const BeatPhase: Double);
begin

  bldBeat.UpdatePulse(Position100ns);

  if (CurrentBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [CurrentBpm])
  else
    lblBpm.Caption := '--.-- BPM';
end;


procedure TfrmChannelDeck.DeckBeat(Sender: TObject;
                                   const Position100ns: Int64;
                                   const BeatNumber: Int64;
                                   const CurrentBpm: Double);
begin

  bldBeat.TriggerPulse(Position100ns);

  if (CurrentBpm > 0.0) then
    lblBpm.Caption := Format('%.2f BPM',
                             [CurrentBpm]);
end;


// =============================================================================

procedure TfrmChannelDeck.btnPFLClick(Sender: TObject);
begin

  UpdateMixerChannelState();
end;


procedure TfrmChannelDeck.tbBalanceChange(Sender: TObject);
begin

  UpdateMixerChannelState();
end;


procedure TfrmChannelDeck.tbBalanceDblClick(Sender: TObject);
begin

  tbBalance.AnimateTrackBarToPosition(0,
                                      2);
end;


procedure TfrmChannelDeck.tbEqCenterFreqHzChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  Eq.CenterFreqHz := tbEqCenterFreqHz.Position;
end;


procedure TfrmChannelDeck.tbEqCenterFreqHzDblClick(Sender: TObject);
begin

  tbEqCenterFreqHz.AnimateTrackBarToPosition(1500,
                                             2)
end;


procedure TfrmChannelDeck.tbEqGainDbChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  Eq.GainDb := tbEqGainDb.Position;
end;


procedure TfrmChannelDeck.tbEqGainDbDblClick(Sender: TObject);
begin

  tbEqCenterFreqHz.AnimateTrackBarToPosition(0,
                                             2)
end;


procedure TfrmChannelDeck.tbEqQChange(Sender: TObject);
var
  Eq: TMfParametricEqEffect;
  NewQ: Single;

begin

  if FUpdatingAudioRackGui then
    Exit;

  Eq := GetEqEffect();
  if not Assigned(Eq) then
    Exit;

  NewQ := tbEqQ.Position / 10.0;
  if (NewQ <= 0) then
    NewQ := 0.1;

  Eq.Q := NewQ;
end;


procedure TfrmChannelDeck.tbEqQDblClick(Sender: TObject);
begin

  tbEqQ.AnimateTrackBarToPosition(10,
                                  2)
end;


procedure TfrmChannelDeck.tbInputGainChange(Sender: TObject);
begin

  ApplyInputGainToEngine();
end;


procedure TfrmChannelDeck.tbInputGainDblClick(Sender: TObject);
begin

  tbInputGain.AnimateTrackBarToPosition(0,
                                        2);
  ApplyInputGainToEngine();
end;


procedure TfrmChannelDeck.tbPitchChange(Sender: TObject);
var
  pct: Double;
  snapped: Boolean;

begin

  // Show value based on slider pos.
  pct := (tbPitch.Position / 100.0) * 16.0;

  // Snap GUI to 0 if close (matches engine behavior).
  snapped := Abs(pct) <= 0.30; // Must match FPitchAutoZeroPct (or read from engine).

  if snapped and (tbPitch.Position <> 0) then
    begin

      tbPitch.OnChange := nil;
      tbPitch.Position := 0;
      tbPitch.OnChange := tbPitchChange;
      pct := 0.0;
    end;

  if Assigned(FDeckEngine) then
    FDeckEngine.SetTempo(InvertTrackbarPos(tbPitch,
                                           False));

  if Assigned(FDeckEngine) then
    lblBpm.Caption := Format('%.2f BPM',
                             [FDeckEngine.GetCurrentBpm]);

  lblPitch.Caption := Format('%.2f%%',
                             [pct]);
end;


procedure TfrmChannelDeck.tbPitchDblClick(Sender: TObject);
begin

 tbPitch.AnimateTrackBarToPosition(0,
                                   2);
end;


// BPM
procedure TfrmChannelDeck.SyncToDeck(AMasterDeck: TfrmChannelDeck);
var
  hr: HRESULT;

begin

  if (AMasterDeck = nil) or
     (AMasterDeck.FDeckEngine = nil) or
     (FDeckEngine = nil) then
    Exit;

  if (AMasterDeck.FDeckEngine.TrackBpm <= 0.0) or
     (FDeckEngine.TrackBpm <= 0.0) then
    Exit;

  hr := FDeckEngine.SyncTempoTo(AMasterDeck.FDeckEngine.TrackBpm);
  if FAILED(hr) then
    Exit;

  hr := FDeckEngine.SyncPhaseTo(AMasterDeck.FDeckEngine.GetBeatPhase);
  if FAILED(hr) then
    Exit;
end;


procedure TfrmChannelDeck.tbVolumeChange(Sender: TObject);
var
  volPosFixed: Integer;

begin

  if not FApplyingXFade then
    ApplyTwoDeckXFade(tbVolume.Position);

  FVolPosLast := tbVolume.Position;

  UpdateMixerChannelState();

  volPosFixed := tbVolume.Position;
  lblVolumePerc.Caption := IntToStr(volPosFixed) + '%';

  // Auto start.
  if FDeckEngine.DeviceState in [dsReady, dsPause, dsStop] then
   if chkAutoCue.Checked then
     btnPlayPauseClick(Self);
end;


procedure TfrmChannelDeck.tbVolumeDblClick(Sender: TObject);
var
  TargetPos: Integer;

begin

  if (tbVolume.Position <> 0) then
    TargetPos := 0
  else
    TargetPos := tbVolume.Maximum;

  tbVolume.AnimateTrackBarToPosition(TargetPos,
                                     2);
end;


procedure TfrmChannelDeck.pbProgressMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  secPos: Integer;
  hnsPos: Int64;

begin

  if (pbProgress.Maximum <= 0) or (FAudioDuration <= 0) then
    Exit;

  // Show only when playing/pause.
  if FDeckEngine.DeviceState in [dsReady, dsPlay, dsPause] then
    begin

      secPos := (abs(Y - pbProgress.Height) * pbProgress.Maximum) div pbProgress.Height;

      if (secPos < 0) then
        secPos := 0
      else
        if (secPos > pbProgress.Maximum) then
          secPos := pbProgress.Maximum;

      pbProgress.ShowHint := True;
      pbProgress.Hint := Format('Position: %d s',
                                [secPos]);
      hnsPos := Int64(secPos) * HNS_PER_100MS;
      lblBarPositionTime.Caption := Format('%s',
                                           [HnsTimeToStr(hnsPos,
                                                         True)]);
    end;
end;


procedure TfrmChannelDeck.pbProgressMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hr: HRESULT;
  tickPos: Int64;  // 100ms ticks
  posHns: Int64;   // 100ns units
  Yc: Integer;
  denom: Integer;

begin

  if (FDeckEngine = nil) then
    Exit;

  denom := pbProgress.Height;
  if (denom <= 0) then
    Exit;

  if (pbProgress.Maximum <= 0) then
    Exit;

  // Clamp Y into [0..Height]
  Yc := Y;
  if (Yc < 0) then
    Yc := 0;
  if (Yc > denom) then
    Yc := denom;

  // top = 0, bottom = Max
  // tickPos := Trunc((Yc / denom) * pbProgress.Max);
  // top = 100, bottom = Min
  tickPos := Trunc(((denom - Yc) / denom) * pbProgress.Maximum);

  // Avoid exact end tick (can map to EOF).
  if (tickPos >= pbProgress.Maximum) and (pbProgress.Maximum > 0) then
    tickPos := pbProgress.Maximum - 1;

  // 100ms tick -> 100ns
  posHns := tickPos * HNS_PER_100MS;

  // Clamp to duration
  if (FAudioDuration > 0) and (posHns >= FAudioDuration) then
    begin

      posHns := FAudioDuration - HNS_PER_100MS;
      if (posHns < 0) then
        posHns := 0;
      tickPos := posHns div HNS_PER_100MS;
    end;

  hr := FDeckEngine.SeekTo(posHns);

  if SUCCEEDED(hr) then
    pbProgress.Position := Integer(tickPos)
  else
    begin

      lblStatus.Caption := Format('SeekTo failed. (hr=%d)',
                                  [hr]);
      lblStatus.Hint := lblStatus.Caption;
    end;
end;


procedure TfrmChannelDeck.chkMuteClick(Sender: TObject);
begin

  UpdateMixerChannelState();
end;


procedure TfrmChannelDeck.chkPlayListClick(Sender: TObject);
var
  P: TPoint;

begin

  if not chkPlayList.Checked then
    begin

      StopPlaylistMode();
      Exit;
    end;

  BuildPlaylistPopup();

  P := chkPlayList.ClientToScreen(Point(0,
                                        chkPlayList.Height));

  FPlaylistPopup.Popup(P.X,
                       P.Y);
end;


procedure TfrmChannelDeck.chkRepeatPlayClick(Sender: TObject);
begin

  FRepeatPlay := chkRepeatPlay.Checked;

  if chkPlayList.Checked then
    UpdateNextPlaylistPath();
end;


procedure TfrmChannelDeck.chkPlayListShuffleClick(Sender: TObject);
begin

  FPlaylistShuffle := chkPlayList.Checked and chkPlayListShuffle.Checked;

  if FPlayingPlaylist then
    begin

      ResetShuffleHistory();

      if (FCurrentPlaylistIndex >= 0) then
        AddCurrentToShuffleHistory(FCurrentPlaylistIndex);
    end;

  UpdateNextPlaylistPath();
end;


procedure TfrmChannelDeck.btnPlayListNextClick(Sender: TObject);
begin

  if FPlayingPlaylist then
    begin

      FSuppressPlaylistAutoAdvanceOnce := True;
      PlayNextPlaylistEntry();
    end;
end;


procedure TfrmChannelDeck.btnPlayListPrevClick(Sender: TObject);
begin

  if FPlayingPlaylist then
    begin

      FSuppressPlaylistAutoAdvanceOnce := True;
      PlayPrevPlaylistEntry();
    end;
end;

end.

