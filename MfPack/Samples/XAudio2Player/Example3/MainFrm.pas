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
// Revision Version: 3.1.6
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: https://www.gamedev.net/articles/programming/general-and-gameplay-programming/decoding-audio-for-xaudio2-with-microsoft-media-foundation-r4280/
//         https://learn.microsoft.com/en-us/windows/win32/xaudio2/how-to--load-audio-data-files-in-xaudio2
//
// Copyright © FacctoryX
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
                       'Windows Media Audio|*.wma' +                        // 7
                       'All Files|*.*';                                     // 8
type

  TfrmMain = class(TForm)
    butPlayPause: TButton;
    butStop: TButton;
    mnuMain: TMainMenu;
    OpenAudioFile1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar: TStatusBar;
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
    pmRight: TMfPeakMeter;
    pmLeft: TMfPeakMeter;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    lblPitch: TLabel;
    Label4: TLabel;
    Bevel7: TBevel;
    Label5: TLabel;
    Label6: TLabel;
    trbVolumeR: TTrackBar;
    trbVolumeL: TTrackBar;
    cbLockVolumeSliders: TCheckBox;
    trbPitch: TTrackBar;
    cbxReverbMain: TCheckBox;
    cbxReverbSource: TCheckBox;
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
    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butReplayClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure trbPitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxReverbMainClick(Sender: TObject);
    procedure cbxReverbSourceClick(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  private
    { Private declarations }
    fXaudio2Engine: TXaudio2Engine;
    fAudioFileFullPath: TFileName;
    fAudioFileName: TFileName;
    mftAudioDuration: MFTIME;

    iSliderL: FLOAT;
    iSliderR: FLOAT;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();

    // Get the status of the XAudio2 engine.
    function GetStatus(): string;
    // Helper for Reverb calls
    function GetReverbParams(index: Integer): TReverbParams;


    // Xaudi2Engine events
    procedure HandleOnProcessingData(Sender: TObject);
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
    { Public declarations }

  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}


procedure TfrmMain.butPlayPauseClick(Sender: TObject);
begin
  if not Assigned(fXaudio2Engine) then
    Exit;

  if (butPlayPause.Tag = 0) then
    begin
      // Play.
      fXaudio2Engine.Play();
      // Keep volume on previous volume.
      SetVolumeChannels();
      butPlayPause.Tag := 1;
    end
  else // Pause.
    begin
      fXaudio2Engine.Pause();
      butPlayPause.Tag := 0;
    end;
end;


procedure TfrmMain.butReplayClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  // When Stopped a previous audiostream, the SourceVoice is removed from the topology.
  // I that case fXaudio2Engine.InitializeXAudio2 will build a new one.
  if not fXaudio2Engine.AddNewSourceVoice then
    butStopClick(nil);

  // You don't need to command Play, this will be done internally.
  hr := fXaudio2Engine.InitializeXAudio2(True);

  // Set choosen effects.
  if cbxReverbMain.Checked then
    fXaudio2Engine.SetReverb(afxMasteringVoice,
                             GetReverbParams(ckbReverbMain.ItemIndex),
                             cbxReverbMain.Checked);

  if cbxReverbSource.Checked then
    fXaudio2Engine.SetReverb(afxSourceVoice,
                             GetReverbParams(ckbReverbMain.ItemIndex),
                             cbxReverbSource.Checked);

  // Add more effects here =====================================================

  // ===========================================================================

  // Keep volume on previous volume.
  SetVolumeChannels();

  if FAILED(hr) then
    StatusBar.SimpleText := Format('Could not initialize XAudio2 Error: %d.', [hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  fXaudio2Engine.Stop();
end;

// Helper.
function TfrmMain.GetReverbParams(index: Integer): TReverbParams;
begin
  case index of
    0: Result := rpDefault;
    1: Result := rpMinimum;
    2: Result := rpMaximum;
    3: Result := rpManual;
    else
      Result := rpDefault;
  end;
end;


procedure TfrmMain.cbxReverbMainClick(Sender: TObject);
begin
  if Assigned(fXaudio2Engine) then
    fXaudio2Engine.SetReverb(afxMasteringVoice,
                             GetReverbParams(ckbReverbMain.ItemIndex),
                             cbxReverbMain.Checked);
end;


procedure TfrmMain.cbxReverbSourceClick(Sender: TObject);
begin
  if Assigned(fXaudio2Engine) then
    fXaudio2Engine.SetReverb(afxSourceVoice,
                             GetReverbParams(ckbReverbSource.ItemIndex),
                             cbxReverbSource.Checked);
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


procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  if Assigned(fXaudio2Engine) then
    begin
      fXaudio2Engine.Stop;
      FreeAndNil(fXaudio2Engine);
    end;
  CanClose := True;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Create the engine class.
  fXaudio2Engine := TXaudio2Engine.Create();
  //
  fXaudio2Engine.OnProcessingData := HandleOnProcessingData;
  fXaudio2Engine.OnAudioReadyEvent := HandleOnAudioReadyEvent;

  fXaudio2Engine.OnAudioStoppedEvent := HandleOnAudioStoppedEvent;
  fXaudio2Engine.OnAudioPlayingEvent := HandleOnAudioPlayingEvent;
  fXaudio2Engine.OnAudioPauzedEvent := HandleOnAudioPauzedEvent;

  // Set XAudio2VoiceCallback handlers.
  fXaudio2Engine.OnVoiceProcessingPassStartEvent := HandleOnVoiceProcessingPassStartEvent;
  fXaudio2Engine.OnVoiceProcessingPassEndEvent := HandleOnVoiceProcessingPassEndEvent;
  fXaudio2Engine.OnStreamEndEvent := HandleOnStreamEndEvent;
  fXaudio2Engine.OnBufferStartEvent := HandleOnBufferStartEvent;
  fXaudio2Engine.OnBufferEndEvent := HandleOnBufferEndEvent;

  dlgOpen.Filter := AUDIO_FILE_FILTER;
  dlgOpen.FilterIndex := 1;
end;


procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  iPos: Integer;

begin

  // Set volume slider positions to 0.
  if (Shift = [ssShift]) and (Key = VK_ESCAPE) then
    begin
      trbVolumeL.Position := 0;
      trbVolumeR.Position := 0;
      Key := 0;
      Exit;
    end;

  // Set pitch position to 100 (normal tempo)
  if (Shift = [ssShift]) and (Key = VK_SPACE) then
    begin
      trbPitch.Position := 1000;
      Key := 0;
      Exit;
    end;

  case Key of
    VK_SPACE:   if Assigned(fXaudio2Engine) then
                  begin
                    butPlayPauseClick(nil);
                  end;

    VK_END:     if Assigned(fXaudio2Engine) then
                  begin
                    butStopClick(nil);
                  end;

    VK_F11:     begin
                  if Assigned(fXaudio2Engine) then
                    begin
                      butReplayClick(nil);
                    end;
                end;

    VK_F12:     begin
                  Open1Click(nil);
                end;

    VK_F8:      begin
                  iPos := trbVolumeL.Position + trbVolumeR.Position div 2;
                  trbVolumeL.Position := iPos;
                  trbVolumeR.Position := iPos;
                end;
  end;
end;


function TfrmMain.GetAudioFile(): string;
begin
  Result := 'No audiofile selected.';
  dlgOpen.FileName := '';
  if not dlgOpen.Execute(Handle) then
    Exit;

  Result := dlgOpen.FileName;
end;


procedure TfrmMain.SetVolumeChannels();
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  // trHorizontal or trVertical use
  if (trbVolumeL.Orientation = trVertical) then
    iSliderL := ((trbVolumeL.Max - trbVolumeL.Position) + trbVolumeL.Min) * 0.01
  else
    iSliderL := (trbVolumeL.Position * 0.01);

  if (trbVolumeR.Orientation = trVertical) then
    iSliderR := ((trbVolumeR.Max - trbVolumeR.Position) + trbVolumeR.Min) * 0.01
  else
    iSliderR := (trbVolumeR.Position * 0.01);

  // Mono
  // This is a very rare case, because mono is played on the leftchannel only or
  // on both channels without stereo effect.
  if (fXaudio2Engine.SoundChannels = 1) then
    fXaudio2Engine.VolumeChannels[0] := iSliderL;

  // Stereo
  // The first stereo channel (0) is always the LEFT one!
  if (fXaudio2Engine.SoundChannels = 2) then
    begin
      fXaudio2Engine.VolumeChannels[0] := iSliderL;
      fXaudio2Engine.VolumeChannels[1] := iSliderR;
    end;
  fXaudio2Engine.SetVolumes(fXaudio2Engine.VolumeChannels);
end;


procedure TfrmMain.StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FileExists(fAudioFileFullPath) then
    begin
      StatusBar.ShowHint := True;
      StatusBar.Hint := fAudioFileName;
    end;
end;


procedure TfrmMain.trbPitchChange(Sender: TObject);
var
  freq: Single;

begin
  if not Assigned(fXaudio2Engine) then
    Exit;

  freq := MapRange((trbPitch.Position + 10) * 0.1,
                   trbPitch.Max * 0.1,
                   trbPitch.Min* 0.1,
                   MIN_PITCH,
                   MAX_PITCH);

  fXaudio2Engine.SetPitch(freq);

  if (trbPitch.Position > 0) then
    lblPitch.Caption :=  Format('-%d',
                                [Abs(trbPitch.Position)])
  else
    lblPitch.Caption :=  Format('%d',
                                [Abs(trbPitch.Position)]);
end;


procedure TfrmMain.trbVolumeLChange(Sender: TObject);
begin
  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeR.Position := trbVolumeL.Position;

  SetVolumeChannels();

  lblRightVolume.Caption :=  Format('%d', [((trbVolumeL.Max - trbVolumeL.Position) + trbVolumeL.Min)]);
end;


procedure TfrmMain.trbVolumeRChange(Sender: TObject);
begin
  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeL.Position := trbVolumeR.Position;

  SetVolumeChannels();

  lblLeftVolume.Caption := Format('%d', [((trbVolumeR.Max - trbVolumeR.Position) + trbVolumeR.Min)]);
end;


procedure TfrmMain.Open1Click(Sender: TObject);
var
  hr: HResult;

begin
try
  // Select an audiofile.
  fAudioFileFullPath := GetAudioFile();

  if (fAudioFileName = 'No audiofile selected.') then
    Exit;

  fAudioFileName := ExtractFileName(fAudioFileFullPath);

  // Load and play the audiofile.
  hr := fXaudio2Engine.LoadAndPlay(fAudioFileFullPath);

  if SUCCEEDED(hr) then
    SetVolumeChannels();

finally
  StatusBar.SimpleText := 'Open an audio file';
end;
end;


procedure TfrmMain.pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  fSamplePos: LONGLONG;
  fTimePos: string;

begin
  fSamplePos := Trunc((X / pbProgress.Width) * pbProgress.Max);
  pbProgress.ShowHint := True;
  pbProgress.Hint := Format('Position: %d', [fSamplePos]);

  lblBarPositionInSamples.Caption := Format('Position: %d',
                                            [fSamplePos]);

  fTimePos := HnsTimeToStr((fSamplePos div (fXaudio2Engine.SamplesPerSec)) * 10000000,
                           False);

  lblBarPositionInSTime.Caption := Format('Position: %s',
                                          [fTimePos]);
end;


procedure TfrmMain.pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  hr: HResult;
  fPos: LONGLONG;

begin
  if (X <= 0) then
    fPos := 0
  else
    fPos := Trunc((X / pbProgress.Width) * pbProgress.Max);

  hr := fXaudio2Engine.GotoNewPosition(fPos);

  if SUCCEEDED(hr) then
    pbProgress.Position := fPos
  else
    stxtStatus.Caption := Format('GotoNewPosition failed. (Result: %d)', [hr]);
end;

// IXAudio2VoiceCallback handlers ==============================================

function TfrmMain.GetStatus(): string;
begin
  case fXaudio2Engine.PlayStatus of
    rsStopped: Result := 'Stopped';
    rsPlaying: Result := 'Playing';
    rsPauzed: Result := 'Paused';
    rsEndOfBuffer: Result := 'EndOfBuffer';
    rsEndOfStream: Result := 'EndOfStream';
    rsInitializing: Result := 'Initializing';
    rsInitialized: Result := 'Initialized';

    // These are stubs.
    //rsProcessingPassStart: sStatus := 'ProcessingPassStart';
    //rsProcessingPassEnd: sStatus := 'ProcessingPassEnd';

    rsDestroying: Result := 'Destroying';
    else
      Result := 'Unknown render status';
    end;

end;


procedure TfrmMain.HandleOnProcessingData(Sender: TObject);
var
  Xaudio2EventData: TXaudio2EventData;
  sPlayed: string;

begin

  Xaudio2EventData := fXaudio2Engine.AudioEventData;

  TThread.Synchronize(nil,
                      procedure
                        begin
                          sPlayed := HnsTimeToStr(Xaudio2EventData.TimePlayed,
                                                  False);

                          lblProcessed.Caption := Format('Samples: %d',
                                                         [Xaudio2EventData.Position + Xaudio2EventData.SamplesProcessed]);

                          lblPlayed.Caption := Format('Played: %s',
                                                      [sPlayed]);

                          pbProgress.Position := Xaudio2EventData.Position + Xaudio2EventData.SamplesProcessed;
                        {$IFDEF DEBUG}
                          OutputDebugString(StrToPWideChar(GetStatus()));
                        {$ENDIF}

                        end);

  Application.ProcessMessages;
end;

// Audio is ready to play.
procedure TfrmMain.HandleOnAudioReadyEvent(Sender: TObject);
begin
  pnlControls.Enabled := True;
  pbProgress.Enabled := True;
  butPlayPause.Enabled := True;
  butStop.Enabled := False;
  butReplay.Enabled := False;
  StatusBar.SimpleText := Format('Loaded file: %s', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();

  // Enable peakmeters.
  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  mftAudioDuration := fXaudio2Engine.Duration;
  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(mftAudioDuration,
                                              False)]);

  StatusBar.SimpleText := Format('Loaded file: %s.',
                                 [fAudioFileName]);
  // Set progressbar values.
  pbProgress.Max := (mftAudioDuration div 10000000) * fXaudio2Engine.SamplesPerSec;
  pbProgress.Min := 0;
  pbProgress.Position := 0;
end;

// Called after user hits Stop.
procedure TfrmMain.HandleOnAudioStoppedEvent(Sender: TObject);
begin
  pnlControls.Enabled := False;
  butPlayPause.Enabled := False;
  butPlayPause.Caption := 'Play';
  lblPlayed.Caption := 'Played: 00:00:00';
  pbProgress.Position := 0;
  butStop.Enabled := False;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();
  pbProgress.Position := 0;
end;

// Called when playing starts.
procedure TfrmMain.HandleOnAudioPlayingEvent(Sender: TObject);
begin
  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Pause';
  butStop.Enabled := True;
  StatusBar.SimpleText := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();
end;

// Called when paused.
procedure TfrmMain.HandleOnAudioPauzedEvent(Sender: TObject);
begin
  butPlayPause.Caption := 'Play';
  butPlayPause.Tag := 0;
  StatusBar.SimpleText := Format('Loaded file: %s.', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnVoiceProcessingPassStartEvent(Sender: TObject);
begin
  // Stub.
end;


procedure TfrmMain.HandleOnVoiceProcessingPassEndEvent(Sender: TObject);
begin
  // Stub.
end;


// Called when all buffers have been played.
procedure TfrmMain.HandleOnStreamEndEvent(Sender: TObject);
begin
  pnlControls.Enabled := False;
  pbProgress.Enabled := False;
  butPlayPause.Enabled := False;
  butPlayPause.Caption := 'Play';
  pbProgress.Position := 0;
  butPlayPause.Tag := 0;
  butStop.Enabled := False;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Loaded file: %s', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();
  pbProgress.Position := 0;
end;

// Start playing engine buffer.
procedure TfrmMain.HandleOnBufferStartEvent(Sender: TObject);
begin
  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Pause';
  butPlayPause.Tag := 1;
  butStop.Enabled := True;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Loaded file: %s', [fAudioFileName]);
  stxtStatus.Caption := GetStatus();
end;

// Called when playing an audio buffer reached the end.
procedure TfrmMain.HandleOnBufferEndEvent(Sender: TObject);
begin
  StatusBar.SimpleText := 'End of playbuffer reached.';
  stxtStatus.Caption := GetStatus();
end;

end.
