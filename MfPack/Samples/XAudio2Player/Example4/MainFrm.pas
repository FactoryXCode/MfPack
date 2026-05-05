 // FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 28-03-2024
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Main window.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Note: Closing the application can take some time,
//                because any running thread will be terminated first.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// =============================================================================
// Source: https://www.gamedev.net/articles/programming/general-and-gameplay-programming/decoding-audio-for-xaudio2-with-microsoft-media-foundation-r4280/
//         https://learn.microsoft.com/en-us/windows/win32/xaudio2/how-to--load-audio-data-files-in-xaudio2
//
//==============================================================================
//
// LICENSE (MPL 2.0)
//==============================================================================

unit MainFrm;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {ActiveX}
  Winapi.ActiveX,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.SyncObjs,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPOFx,
  {Application}
  Tools,
  XAudio2Engine,
  XAudio2_FXReverb,
  MfPeakMeter;  // Don't forget to add the Mfpeakmeter location in your project settings searchpath.

const
  AUDIO_FILE_FILTER  = 'Waveform Audio File Format|*.wav|' +                // 1
                       'MPEG Audio Layer III|*.mp3|' +                      // 2
                       'Dolby AC-3 audio|*.ac3|' +                          // 3
                       'Free Lossless Audio Codec|*.flac|' +                // 4
                       'Advanced Audio Coding (AAC)|*.aac|' +               // 5
                       'MPEG-4 Audio|*.m4a|' +                              // 6
                       'Windows Media Audio|*.wma|' +                       // 7
                       'All Files|*.*|';                                    // 8

type
  TfrmMain = class(TForm)
    butPlayPause: TButton;
    butStop: TButton;
    mnuMain: TMainMenu;
    OpenAudioFile1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    dlgOpen: TOpenDialog;
    lblDuration: TLabel;
    lblProcessed: TLabel;
    lblPlayed: TLabel;
    butReplay: TButton;
    pbProgress: TProgressBar;
    pnlControls: TPanel;
    Bevel4: TBevel;
    Bevel3: TBevel;
    Bevel2: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    lblPitch: TLabel;
    Label4: TLabel;
    Bevel7: TBevel;
    trbVolumeR: TTrackBar;
    trbVolumeL: TTrackBar;
    cbLockVolumeSliders: TCheckBox;
    trbPitch: TTrackBar;
    ckbReverbMain: TComboBox;
    ckbReverbSource: TComboBox;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    spedLimiterThreshold: TSpinEdit;
    spedLimiterReleaseTime: TSpinEdit;
    lblBarPositionInSamples: TLabel;
    lblBarPositionInSTime: TLabel;
    stxtStatus: TStaticText;
    Label9: TLabel;
    Label5: TLabel;
    Bevel1: TBevel;
    Bevel5: TBevel;
    sTxtStatusBar: TStaticText;
    pmLeft: TMfPeakMeter;
    pmRight: TMfPeakMeter;

    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure trbPitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ckbReverbMainCloseUp(Sender: TObject);
    procedure ckbReverbSourceCloseUp(Sender: TObject);
    procedure butReplayClick(Sender: TObject);

  private

    fXaudio2Engine: TXaudio2Engine;
    fAudioFileFullPath: TFileName;
    fAudioFileName: TFileName;
    mftAudioDuration: MFTIME;

    function GetAudioFile(): string;
    procedure SetVolumeChannels();
    function GetStatus(): string;

    // Xaudio2Engine events
    procedure HandleOnAudioReadyEvent(Sender: TObject);
    procedure HandleOnAudioStoppedEvent(Sender: TObject);
    procedure HandleOnAudioPlayingEvent(Sender: TObject);
    procedure HandleOnAudioPauzedEvent(Sender: TObject);

    // XAudio2VoiceCallback events
    procedure HandleOnVoiceProcessingPassStartEvent(Sender: TObject);
    procedure HandleOnVoiceProcessingPassEndEvent(Sender: TObject);
    procedure HandleOnStreamEndEvent(Sender: TObject);
    procedure HandleOnBufferStartEvent(Sender: TObject);
    procedure HandleOnBufferEndEvent(Sender: TObject);

  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;

begin

  fXaudio2Engine := TXaudio2Engine.Create();

  // Engine events (safe: engine uses TThread.Queue internally)
  fXaudio2Engine.OnStreamEndEvent := HandleOnStreamEndEvent;
  fXaudio2Engine.OnAudioReadyEvent := HandleOnAudioReadyEvent;
  fXaudio2Engine.OnAudioStoppedEvent := HandleOnAudioStoppedEvent;
  fXaudio2Engine.OnAudioPlayingEvent := HandleOnAudioPlayingEvent;
  fXaudio2Engine.OnAudioPauzedEvent := HandleOnAudioPauzedEvent;

  fXaudio2Engine.OnVoiceProcessingPassStartEvent := HandleOnVoiceProcessingPassStartEvent;
  fXaudio2Engine.OnVoiceProcessingPassEndEvent := HandleOnVoiceProcessingPassEndEvent;
  fXaudio2Engine.OnStreamEndEvent := HandleOnStreamEndEvent;
  fXaudio2Engine.OnBufferStartEvent := HandleOnBufferStartEvent;
  fXaudio2Engine.OnBufferEndEvent := HandleOnBufferEndEvent;

  dlgOpen.Filter := AUDIO_FILE_FILTER;
  dlgOpen.FilterIndex := 1;

  // Reverb UI not supported by the current XAudio2Engine interface (no ReverbParameters list exposed).
  ckbReverbMain.Clear;
  ckbReverbSource.Clear;
  ckbReverbMain.Items.Add('None');
  ckbReverbSource.Items.Add('None');
  ckbReverbMain.ItemIndex := 0;
  ckbReverbSource.ItemIndex := 0;
  ckbReverbMain.Enabled := False;
  ckbReverbSource.Enabled := False;

  // Disable controls until something is loaded
  pnlControls.Enabled := False;
  pbProgress.Enabled := False;
  butPlayPause.Enabled := False;
  butStop.Enabled := False;
  butReplay.Enabled := False;

  pmLeft.Enabled := False;
  pmRight.Enabled := False;

  sTxtStatusBar.Caption := 'No file loaded.';
  stxtStatus.Caption := 'Idle';

  // Reverb
  // Load reverb parameters in the list.
  ckbReverbMain.Clear();
  ckbReverbSource.Clear();

  // First in the list.
  ckbReverbMain.Items.Append('None');
  ckbReverbSource.Items.Append('None');

  for i := 0 to Length(fXaudio2Engine.ReverbParameters) - 1 do
    begin
       ckbReverbMain.Items.Append(fXaudio2Engine.ReverbParameters[i].i3dl2Name);
       ckbReverbSource.Items.Append(fXaudio2Engine.ReverbParameters[i].i3dl2Name);
    end;

  ckbReverbMain.ItemIndex := 0;
  ckbReverbSource.ItemIndex := 0;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;

  try
    if Assigned(fXaudio2Engine) then
      begin

        // Detach handlers FIRST
        fXaudio2Engine.OnAudioReadyEvent := nil;
        fXaudio2Engine.OnAudioStoppedEvent := nil;
        fXaudio2Engine.OnAudioPlayingEvent := nil;
        fXaudio2Engine.OnAudioPauzedEvent := nil;

        fXaudio2Engine.OnVoiceProcessingPassStartEvent := nil;
        fXaudio2Engine.OnVoiceProcessingPassEndEvent := nil;
        fXaudio2Engine.OnStreamEndEvent := nil;
        fXaudio2Engine.OnBufferStartEvent := nil;
        fXaudio2Engine.OnBufferEndEvent := nil;

        // Now shut down
        fXaudio2Engine.Stop();
        FreeAndNil(fXaudio2Engine);
      end;
  except
    // never block close!
  end;

  CanClose := True;
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


function TfrmMain.GetAudioFile(): string;
begin

  Result := '';
  dlgOpen.FileName := '';

  if not dlgOpen.Execute(Handle) then
    Exit;
  Result := dlgOpen.FileName;
end;


procedure TfrmMain.Open1Click(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  fAudioFileFullPath := GetAudioFile();
  if (fAudioFileFullPath = '') then
    Exit;

  fAudioFileName := ExtractFileName(fAudioFileFullPath);

  // Pre-compute duration for UI (engine no longer exposes Duration property)
  hr := GetFileDuration(StrToPWideChar(fAudioFileFullPath),
                        mftAudioDuration);
  if FAILED(hr) then
    mftAudioDuration := 0;

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(mftAudioDuration, False)]);

  // Progress bar now uses seconds (avoid missing SamplesPerSec)
  pbProgress.Min := 0;
  pbProgress.Max := Integer(mftAudioDuration div 10000000); // seconds
  pbProgress.Position := 0;

  // Load & decode to PCM/WAV and initialize XAudio2 (do NOT start playback)
  sTxtStatusBar.Caption := Format('Loaded file (ready): %s ...',
                                  [fAudioFileName]);
  stxtStatus.Caption := 'Loading...';

  fXaudio2Engine.LoadAndInitializeAsync(fAudioFileFullPath,
                                        False);

  // Reset play button state
  butPlayPause.Caption := 'Play';
  butPlayPause.Tag := 0;
  butStop.Enabled := False;
  butReplay.Enabled := False;

  SetVolumeChannels();

  ckbReverbMain.Enabled := True;
  ckbReverbSource.Enabled := True;
end;


procedure TfrmMain.butPlayPauseClick(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  if (butPlayPause.Tag = 0) then
    begin

      fXaudio2Engine.Play();

      pmLeft.Enabled := true;

      // Set choosen effects.
      if (ckbReverbMain.ItemIndex > 0) then
        ckbReverbMainCloseUp(Self);
      if (ckbReverbSource.ItemIndex > 0) then
        ckbReverbSourceCloseUp(Self);

      SetVolumeChannels();
      butPlayPause.Tag := 1;
    end
  else
    begin

      if SUCCEEDED(fXaudio2Engine.Pause) then
        stxtStatus.Caption := Format('Paused file: %s.',
                                     [fAudioFileName]);
      butPlayPause.Tag := 0;
    end;
end;


procedure TfrmMain.butReplayClick(Sender: TObject);
var
  hr: HRESULT;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  // Quick replay: rewind to start without reloading or decoding .
  hr := fXaudio2Engine.GotoNewPosition(0);

  if FAILED(hr) then
  begin
    stxtStatus.Caption := Format('Replay failed (GotoNewPosition). hr=%d', [hr]);
    Exit;
  end;

  pbProgress.Position := 0;
  butPlayPause.Tag := 0;
  butPlayPause.Caption := 'Play';

  // Start playback
  fXaudio2Engine.Play();
  SetVolumeChannels();

  // Set GUI controls in play mode.
  HandleOnAudioReadyEvent(Self);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if Assigned(fXaudio2Engine) then
    if SUCCEEDED(fXaudio2Engine.Stop()) then
      butStop.Enabled := False;
end;


procedure TfrmMain.CheckBox1Click(Sender: TObject);
var
  params: FXMASTERINGLIMITER_PARAMETERS;

begin

  if Assigned(fXaudio2Engine) then
    begin

      params.Release := spedLimiterReleaseTime.Value;
      params.Loudness := spedLimiterThreshold.Value;
      fXaudio2Engine.SetMasterLimiter(params);
    end;
end;


procedure TfrmMain.ckbReverbMainCloseUp(Sender: TObject);
var
  index: Integer;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  if (ckbReverbMain.ItemIndex > 0) then
    index := ckbReverbMain.ItemIndex - 1
  else
    index := ckbReverbMain.ItemIndex;

  fXaudio2Engine.SetReverb(afxMasteringVoice,
                           fXaudio2Engine.ReverbParameters[index].nativeParam,
                           (ckbReverbMain.ItemIndex > 0));
end;


procedure TfrmMain.ckbReverbSourceCloseUp(Sender: TObject);
var
  index: Integer;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  if (ckbReverbSource.ItemIndex > 0) then
    index := ckbReverbSource.ItemIndex - 1
  else
    index := ckbReverbSource.ItemIndex;

  fXaudio2Engine.SetReverb(afxSourceVoice,
                           fXaudio2Engine.ReverbParameters[index].nativeParam,
                           (ckbReverbSource.ItemIndex > 0));
end;


procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  iPos: Integer;

begin

  if (Shift = [ssShift]) and (Key = VK_ESCAPE) then
  begin

    trbVolumeL.Position := 0;
    trbVolumeR.Position := 0;
    Key := 0;
    Exit;
  end;

  if (Shift = [ssShift]) and (Key = VK_SPACE) then
  begin

    trbPitch.Position := 1000;
    Key := 0;
    Exit;
  end;

  case Key of
    VK_SPACE: butPlayPauseClick(nil);
    VK_END:   butStopClick(nil);
    VK_F12:   Open1Click(nil);
    VK_F8:
      begin

        iPos := trbVolumeL.Position + trbVolumeR.Position div 2;
        trbVolumeL.Position := iPos;
        trbVolumeR.Position := iPos;
      end;
  end;
end;


procedure TfrmMain.SetVolumeChannels();
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  // Mono
  if (fXaudio2Engine.SoundChannels = 1) then
    fXaudio2Engine.VolumeChannels[0] := (Abs(trbVolumeR.Position) * 0.01); // swapped

  // Stereo
  // Channel 0 = LEFT, Channel 1 = RIGHT
  if (fXaudio2Engine.SoundChannels = 2) then
  begin
    fXaudio2Engine.VolumeChannels[0] := (Abs(trbVolumeR.Position) * 0.01); // LEFT comes from R slider (swapped)
    fXaudio2Engine.VolumeChannels[1] := (Abs(trbVolumeL.Position) * 0.01); // RIGHT comes from L slider (swapped)
  end;

  fXaudio2Engine.SetVolumes(fXaudio2Engine.VolumeChannels);
end;



procedure TfrmMain.trbVolumeLChange(Sender: TObject);
var
  vol: Single;

begin

  if cbLockVolumeSliders.Checked then
    trbVolumeR.Position := trbVolumeL.Position;

  SetVolumeChannels();

  vol := MapRange(trbVolumeL.Position,
                  trbVolumeL.Max,
                  trbVolumeL.Min,
                  MIN_VOLUME,
                  MAX_VOLUME);

  lblLeftVolume.Caption := Format('%d', [Trunc(vol)]) + '%';
end;


procedure TfrmMain.trbVolumeRChange(Sender: TObject);
var
  vol: Single;

begin

  if cbLockVolumeSliders.Checked then
    trbVolumeL.Position := trbVolumeR.Position;

  SetVolumeChannels();

  vol := MapRange(trbVolumeR.Position,
                  trbVolumeR.Max,
                  trbVolumeR.Min,
                  MIN_VOLUME,
                  MAX_VOLUME);

  lblRightVolume.Caption := Format('%d', [Trunc(vol)]) + '%';
end;


procedure TfrmMain.trbPitchChange(Sender: TObject);
var
  freq: Single;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  freq := MapRange((trbPitch.Position + 10) * 0.1,
                    trbPitch.Max * 0.1,
                    trbPitch.Min * 0.1,
                    MIN_PITCH,
                    MAX_PITCH);

  fXaudio2Engine.SetPitch(freq);

  if (trbPitch.Position > 0) then
    lblPitch.Caption := Format('-%d', [Abs(trbPitch.Position)])
  else
    lblPitch.Caption := Format('%d', [Abs(trbPitch.Position)]);
end;


procedure TfrmMain.StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

  if FileExists(fAudioFileFullPath) then
    begin

      sTxtStatusBar.ShowHint := True;
      sTxtStatusBar.Hint := fAudioFileName;
    end;
end;


procedure TfrmMain.pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  secPos: Integer;
  hnsPos: Int64;

begin

  if (pbProgress.Max <= 0) or (mftAudioDuration <= 0) then
    Exit;

  if fXaudio2Engine.RenderStatus in [rsPlaying, rsPauzed] then
    begin

      secPos := Trunc((X / pbProgress.Width) * pbProgress.Max);
      pbProgress.ShowHint := True;
      pbProgress.Hint := Format('Position: %d s', [secPos]);

      lblBarPositionInSamples.Caption := Format('Position: %d s', [secPos]);

      hnsPos := Int64(secPos) * 10000000;
      lblBarPositionInSTime.Caption := Format('Position: %s', [HnsTimeToStr(hnsPos, False)]);
    end;
end;


procedure TfrmMain.pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  hr: HResult;
  samplePos: Int64;
  posHns: LONGLONG;

begin

  if (fXaudio2Engine = nil) then
    Exit;

  if (pbProgress.Width <= 0) then
    Exit;

  if (pbProgress.Max <= 0) then
    Exit;

  if (X <= 0) then
    samplePos := 0
  else
    if (X >= pbProgress.Width) then
      samplePos := pbProgress.Max
  else
    samplePos := Trunc((X / pbProgress.Width) * pbProgress.Max); // samples

  // samples -> 100ns
  if (fXaudio2Engine.SamplesPerSec > 0) then
    posHns := (samplePos * 10000000) div fXaudio2Engine.SamplesPerSec
  else
    posHns := 0;

  hr := fXaudio2Engine.GotoNewPosition(posHns);

  if SUCCEEDED(hr) then
    pbProgress.Position := Integer(samplePos)
  else
    stxtStatus.Caption := Format('GotoNewPosition failed. (hr=%d)', [hr]);
end;


function TfrmMain.GetStatus(): string;
begin
  if not Assigned(fXaudio2Engine) then
    Exit('No engine');

  case fXaudio2Engine.RenderStatus of
    rsStopped: Result := 'Stopped';
    rsPlaying: Result := 'Playing';
    rsPauzed: Result := 'Paused';
    rsEndOfBuffer: Result := 'EndOfBuffer';
    rsEndOfStream: Result := 'EndOfStream';
    rsInitializing: Result := 'Initializing';
    rsInitialized: Result := 'Initialized';
    rsDestroying: Result := 'Destroying';
  else
    Result := 'Unknown render status';
  end;
end;


procedure TfrmMain.HandleOnStreamEndEvent(Sender: TObject);
begin

  pnlControls.Enabled := False;
  butPlayPause.Enabled := False;
  butPlayPause.Caption := 'Play';
  butPlayPause.Tag := 0;

  pmLeft.Enabled := False;
  pmRight.Enabled := False;

  lblPlayed.Caption := 'Played: 00:00:00';
  lblProcessed.Caption := 'Samples: 0';
  pbProgress.Position := 0;

  butStop.Enabled := False;
  butReplay.Enabled := True;

  sTxtStatusBar.Caption := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus;
end;


procedure TfrmMain.HandleOnAudioReadyEvent(Sender: TObject);
begin

  pnlControls.Enabled := True;
  pbProgress.Enabled := (pbProgress.Max > 0);

  butPlayPause.Enabled := True;
  butStop.Enabled := False;
  butReplay.Enabled := False;

  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  sTxtStatusBar.Caption := Format('Loaded file: %s', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();

  pbProgress.Max := (mftAudioDuration div 10000000) * fXaudio2Engine.SamplesPerSec;
  pbProgress.Position := 0;
end;


procedure TfrmMain.HandleOnAudioStoppedEvent(Sender: TObject);
begin

  pnlControls.Enabled := False;
  butPlayPause.Enabled := False;
  butPlayPause.Caption := 'Play';
  butPlayPause.Tag := 0;

  butStop.Enabled := False;
  butReplay.Enabled := True;

  sTxtStatusBar.Caption := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus;

  lblPlayed.Caption := 'Played: 00:00:00';
  lblProcessed.Caption := 'Samples: 0';
  pbProgress.Position := 0;

  pmLeft.Enabled := False;
  pmRight.Enabled := False;
end;


procedure TfrmMain.HandleOnAudioPlayingEvent(Sender: TObject);
var
  d: TXaudio2EventData;
  absSamples: Int64;
  sPlayed: string;

begin

  if (fXaudio2Engine = nil) then
    Exit;

  if (fXaudio2Engine.RenderStatus = rsDestroying) then
    Exit;

  // If engine is not playing anymore, ignore stale queued ticks.
  if (fXaudio2Engine.RenderStatus <> rsPlaying) then
    Exit;

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Pause';
  butPlayPause.Tag := 1;

  butStop.Enabled := True;
  butReplay.Enabled := True;

  sTxtStatusBar.Caption := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus;

  // Ensure volume is applied at start
  SetVolumeChannels();

  d := fXaudio2Engine.AudioEventData;

  absSamples := d.Position + d.SamplesProcessed;

  // Samples label
  lblProcessed.Caption := Format('Samples: %d', [absSamples]);

  // Played time
  sPlayed := HnsTimeToStr(d.TimePlayed, False);
  lblPlayed.Caption := Format('Played: %s', [sPlayed]);

  // Progress bar expects samples (your original design)
  if (pbProgress.Max > 0) then
    pbProgress.Position := Integer(absSamples);
end;


procedure TfrmMain.HandleOnAudioPauzedEvent(Sender: TObject);
begin

  butPlayPause.Caption := 'Play';
  butPlayPause.Tag := 0;
  sTxtStatusBar.Caption := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus;
end;


procedure TfrmMain.HandleOnVoiceProcessingPassStartEvent(Sender: TObject);
begin
  // Stub.
end;


procedure TfrmMain.HandleOnVoiceProcessingPassEndEvent(Sender: TObject);
begin
  // Stub.
end;


procedure TfrmMain.HandleOnBufferStartEvent(Sender: TObject);
begin

  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Pause';
  butPlayPause.Tag := 1;

  butStop.Enabled := True;
  butReplay.Enabled := True;

  sTxtStatusBar.Caption := Format('Loaded file: %s', [fAudioFileName]);
  stxtStatus.Caption := GetStatus;
end;


procedure TfrmMain.HandleOnBufferEndEvent(Sender: TObject);
begin

  sTxtStatusBar.Caption := 'End of playbuffer reached.';
  stxtStatus.Caption := GetStatus;
end;


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_LITE)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version ' +
                         IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
    end;

finalization

  MFShutdown();

end.

