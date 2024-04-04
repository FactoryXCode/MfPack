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
// Remarks: Requires Windows 7 or higher.
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
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.DirectX.XAudio2.XAudio2,
  {}
  Tools,
  XAudio2Engine,
  MfPeakMeter;  // Don't forget to add the Mfpeakmeter location in your project settings.


type

  TfrmMain = class(TForm)
    butPlay: TButton;
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
    pmRight: TMfPeakMeter;
    pmLeft: TMfPeakMeter;
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    trbVolumeR: TTrackBar;
    trbVolumeL: TTrackBar;
    cbLockVolumeSliders: TCheckBox;
    butReplay: TButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
    butPause: TButton;
    trbPitch: TTrackBar;
    Label3: TLabel;
    Bevel4: TBevel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    lblPitch: TLabel;
    lblStatus: TLabel;
    Bevel5: TBevel;
    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butReplayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure trbPitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    fXaudio2Engine: TXaudio2Engine;
    fAudioFileName: TFileName;
    llAudioDuration: LONGLONG;
    iSliderL: FLOAT;
    iSliderR: FLOAT;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();

    // Get the status of the XAudio2 engine.
    function GetStatus(): string;

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


procedure TfrmMain.butPauseClick(Sender: TObject);
begin
  if not Assigned(fXaudio2Engine) then
    Exit;
  fXaudio2Engine.Pause();
end;


procedure TfrmMain.butPlayClick(Sender: TObject);
begin
  if not Assigned(fXaudio2Engine) then
    Exit;

  // Activate the peakmeters.
  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  // Keep volume on previous volume.
  SetVolumeChannels();
  // Play.
  fXaudio2Engine.Play();
end;


procedure TfrmMain.butReplayClick(Sender: TObject);
var
  hr: HResult;

begin
  hr := S_OK;

  if not Assigned(fXaudio2Engine) then
    Exit;

  // When Stopped a previous audiostream, the SourceVoice is removed from the topology.
  // I that case fXaudio2Engine.InitializeXAudio2 will build a new one.
  if not fXaudio2Engine.NeedNewSourceVoice then
    butStopClick(nil);

  // Keep volume on previous volume.
  SetVolumeChannels();

  if SUCCEEDED(hr) then
    hr := fXaudio2Engine.InitializeXAudio2(True);

  if FAILED(hr) then
    StatusBar.SimpleText := Format('Could not initialize XAudio2 Error: %d.', [hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  fXaudio2Engine.Stop();
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
                    butPlayClick(nil);
                  end;

    VK_END:     if Assigned(fXaudio2Engine) then
                  begin
                    butStopClick(nil);
                  end;

    VK_F10:     if Assigned(fXaudio2Engine) then
                  begin
                    butPauseClick(nil);
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
  fAudioFileName := GetAudioFile();
  if (fAudioFileName = 'No audiofile selected.') then
    Exit;

  // Get the length of the audiofile.
 hr := GetFileDuration(StrToPWideChar(fAudioFileName),
                       llAudioDuration);
 if FAILED(hr) then
    begin
      ShowMessage('Could not retrieve the duration of the audio file.');
      llAudioDuration := 0;
    end;

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(llAudioDuration, False)]);

  if not Assigned(fXaudio2Engine) then
    Exit;

  StatusBar.SimpleText := Format('Selected file: %s.',
                                 [fAudioFileName]);

  // Initialize the engine.
  hr := fXaudio2Engine.LoadFile(Handle,
                                fAudioFileName,
                                llAudioDuration);
  if SUCCEEDED(hr) then
    SetVolumeChannels();

finally
  StatusBar.SimpleText := 'Open an audio file';
end;
end;


// IXAudio2VoiceCallback handlers ==============================================

function TfrmMain.GetStatus(): string;
begin
  case fXaudio2Engine.PlayStatus of
    rsStopped: Result := 'Stopped';
    rsPlaying: Result := 'Playing';
    rsPauzed: Result := 'Pauzed';
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

  Xaudio2EventData := fXaudio2Engine.FXaudio2EventData;


  TThread.Synchronize(nil,
                      procedure
                        begin
                          sPlayed := HnsTimeToStr(Xaudio2EventData.TimePlayed,
                                               False);

                          lblProcessed.Caption := Format('Samples: %d',
                                                         [Xaudio2EventData.SamplesProcessed]);
                          lblPlayed.Caption := Format('Played: %s',
                                                      [sPlayed]);

                        {$IFDEF DEBUG}
                          OutputDebugString(StrToPWideChar(GetStatus()));
                        {$ENDIF}

                        end);

  Application.ProcessMessages;
end;


procedure TfrmMain.HandleOnAudioReadyEvent(Sender: TObject);
begin
  butPlay.Enabled := True;
  butPause.Enabled := True;
  butStop.Enabled := True;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Ready to play: %s', [fAudioFileName]);
  lblStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnAudioStoppedEvent(Sender: TObject);
begin
  butPlay.Enabled := False;
  butPause.Enabled := False;
  butStop.Enabled := False;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Stopped playing file: %s.', [fAudioFileName]);
  lblStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnAudioPlayingEvent(Sender: TObject);
begin
  butPlay.Enabled := True;
  butPause.Enabled := True;
  butStop.Enabled := True;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Playing file: %s.', [fAudioFileName]);
  lblStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnAudioPauzedEvent(Sender: TObject);
begin
  lblStatus.Caption := GetStatus();
  StatusBar.SimpleText := Format('Paused file: %s.', [fAudioFileName]);
end;


procedure TfrmMain.HandleOnVoiceProcessingPassStartEvent(Sender: TObject);
begin
  // Stub.
end;


procedure TfrmMain.HandleOnVoiceProcessingPassEndEvent(Sender: TObject);
begin
  // Stub.
end;


procedure TfrmMain.HandleOnStreamEndEvent(Sender: TObject);
begin
  StatusBar.SimpleText := Format('Ended playing: %s', [fAudioFileName]);
  butPlay.Enabled := True;
  butPause.Enabled := True;
  butStop.Enabled := True;
  butReplay.Enabled := True;
  lblStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnBufferStartEvent(Sender: TObject);
begin
  butPlay.Enabled := True;
  butPause.Enabled := True;
  butStop.Enabled := True;
  butReplay.Enabled := True;
  StatusBar.SimpleText := Format('Ready to play: %s', [fAudioFileName]);
  lblStatus.Caption := GetStatus();
end;


procedure TfrmMain.HandleOnBufferEndEvent(Sender: TObject);
begin
  StatusBar.SimpleText := 'End of playbuffer reached.';
  lblStatus.Caption := GetStatus();
end;


// initialization and finalization =============================================


initialization
  // A gui app should always use COINIT_APARTMENTTHREADED in stead of COINIT_MULTITHREADED
  CoInitializeEx(nil,
                COINIT_APARTMENTTHREADED);

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_FULL)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();
  CoUnInitialize();

end.
