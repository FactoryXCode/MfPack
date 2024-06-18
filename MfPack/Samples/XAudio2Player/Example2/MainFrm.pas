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
// Revision Version: 3.1.7
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
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
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

  private
    { Private declarations }
    fXaudio2Engine: TXaudio2Engine;
    fAudioFileUrl: TFileName;
    fFileName: string; // Filename without path.
    llAudioDuration: LONGLONG;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();
    /// <summary>Keep track of data been played.</summary>
    procedure OnAudioDataProcessedEvent(var AMessage: TMessage); message WM_DATA_PROCESSED_NOTIFY;
    /// <summary>Signals the audio is ready to play.</summary>
    procedure OnAudioReadyEvent(var AMessage: TMessage); message WM_DATA_READY_NOTIFY;
    /// <summary>Signals the audio reached end.</summary>
    procedure OnAudioEndedEvent(var AMessage: TMessage); message WM_DATA_ENDED_NOTIFY;

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
  if SUCCEEDED(fXaudio2Engine.Pause) then
    StatusBar.SimpleText := Format('Paused file: %s.', [fFileName]);
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

  if SUCCEEDED(fXaudio2Engine.Play()) then
    StatusBar.SimpleText := Format('Playing file: %s.', [fFileName]);
end;


procedure TfrmMain.butReplayClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  hr := fXaudio2Engine.Stop();

  if SUCCEEDED(hr) then
    hr := fXaudio2Engine.InitializeXAudio2(True);

  if FAILED(hr) then
    StatusBar.SimpleText := Format('Could not initialize XAudio2 Error: %d.', [hr]);

end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;
  if SUCCEEDED(fXaudio2Engine.Stop()) then
    if SUCCEEDED(fXaudio2Engine.InitializeXAudio2(False)) then
       StatusBar.SimpleText := Format('Stopped playing file: %s.', [fFileName]);
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;
  // Deactivate the peakmeters.
  pmLeft.Enabled := False;
  pmRight.Enabled := False;
  if Assigned(fXaudio2Engine) then
    begin
      fXaudio2Engine.Stop;
      FreeAndNil(fXaudio2Engine);
    end;
  CanClose := True;
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

  // Mono
  // This is a very rare case, because mono is played on the leftchannel only or
  // on both channels without stereo effect.
  if (fXaudio2Engine.SoundChannels = 1) then
    fXaudio2Engine.VolumeChannels[0] := (Abs(trbVolumeL.Position) * 0.01);

  // Stereo
  // The first stereo channel (0) is always the LEFT one!
  if (fXaudio2Engine.SoundChannels = 2) then
    begin
      fXaudio2Engine.VolumeChannels[0] := (Abs(trbVolumeL.Position) * 0.01);
      fXaudio2Engine.VolumeChannels[1] := (Abs(trbVolumeR.Position) * 0.01);
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
                   trbPitch.Min * 0.1,
                   MIN_PITCH,
                   MAX_PITCH);

  fXaudio2Engine.SetPitch(freq);

  if (trbPitch.Position > 0) then
    lblPitch.Caption :=  Format('%d',
                                [Abs(trbPitch.Position)])
  else
    lblPitch.Caption :=  Format('%d',
                                [Abs(trbPitch.Position)]);
end;


procedure TfrmMain.trbVolumeLChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeR.Position := trbVolumeL.Position;

  SetVolumeChannels();

  vol := MapRange((trbVolumeL.Position),
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

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeL.Position := trbVolumeR.Position;

  SetVolumeChannels();

  vol := MapRange((trbVolumeR.Position),
                   trbVolumeR.Max,
                   trbVolumeR.Min,
                   MIN_VOLUME,
                   MAX_VOLUME);

  lblRightVolume.Caption := Format('%d', [Trunc(vol)]) + '%';
end;


procedure TfrmMain.Open1Click(Sender: TObject);
var
  hr: HResult;

begin

try
  // Select an audiofile.
  fAudioFileUrl := GetAudioFile();
  if (fAudioFileUrl = 'No audiofile selected.') then
    Exit;

  fFileName := ExtractFileName(fAudioFileUrl);

  // Get the length of the audiofile.
 hr := GetFileDuration(StrToPWideChar(fAudioFileUrl),
                       llAudioDuration);
 if FAILED(hr) then
    begin
      ShowMessage('Could not retrieve the duration of the audio file.');
      llAudioDuration := 0;
    end;

  lblDuration.Caption := Format('Duration: %s',
                                [HnsTimeToStr(llAudioDuration, False)]);

  // Create the engine
  if SUCCEEDED(hr) then
    fXaudio2Engine := TXaudio2Engine.Create();

  if not Assigned(fXaudio2Engine) then
    Exit;

  StatusBar.SimpleText := Format('Selected file: %s.',
                                 [fFileName]);

  // Initialize the engine.
  hr := fXaudio2Engine.LoadFile(Handle,
                                fAudioFileUrl,
                                llAudioDuration);
  if SUCCEEDED(hr) then
    SetVolumeChannels();

finally
  StatusBar.SimpleText := 'Open an audio file';
end;
end;


// Event handlers ==============================================================

procedure TfrmMain.OnAudioDataProcessedEvent(var AMessage: TMessage);
var
  iProgress: LONGLONG;
  iSamples: LONGLONG;
  tstr: string;

begin

  iProgress := AMessage.WParam;
  iSamples := AMessage.LParam;
  tstr := HnsTimeToStr(iProgress, False);

  lblProcessed.Caption := Format('Samples: %d',
                                 [iSamples]);
  lblPlayed.Caption := Format('Played: %s',
                              [tstr]);
  Application.ProcessMessages;
end;


procedure TfrmMain.OnAudioReadyEvent(var AMessage: TMessage);
begin

  if (AMessage.WParam = 1) then
    begin
      StatusBar.SimpleText := Format('Ready to play: %s', [fFileName]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
      butReplay.Enabled := True;
    end;
end;


procedure TfrmMain.OnAudioEndedEvent(var AMessage: TMessage);
begin

  if (AMessage.WParam = 1) then
    begin
      StatusBar.SimpleText := Format('Ended playing: %s', [fFileName]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
      butReplay.Enabled := True;
    end;
end;


// initialization and finalization =============================================


initialization

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

end.
