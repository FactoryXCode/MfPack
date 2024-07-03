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
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 30/05/2024 Tony                Corrected volume calculation.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butReplayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);

  private
    { Private declarations }
    fXaudio2Engine: TXaudio2Engine;
    fAudioFileName: TFileName;
    llAudioDuration: LONGLONG;

    function GetAudioFile(): string;

    // Set Left and/or Right volume.
    procedure SetVolumeChannels();

    // Keep track of data been played
    procedure OnAudioDataProcessedEvent(var AMessage: TMessage); message WM_DATA_PROCESSED_NOTIFY;
    // Signals the audio is ready to play.
    procedure OnAudioReadyEvent(var AMessage: TMessage); message WM_DATA_READY_NOTIFY;
    // Signals the audio reached end.
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

  if SUCCEEDED(fXaudio2Engine.Pause) then
    StatusBar.SimpleText := Format('Paused file: %s.', [fAudioFileName]);
end;


procedure TfrmMain.butPlayClick(Sender: TObject);
begin

  // Activate the peakmeters.
  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  if SUCCEEDED(fXaudio2Engine.Start()) then
    StatusBar.SimpleText := Format('Playing file: %s.', [fAudioFileName]);
end;


procedure TfrmMain.butReplayClick(Sender: TObject);
var
  hr: HResult;

begin

  hr := fXaudio2Engine.Stop();

  if SUCCEEDED(hr) then
    hr := fXaudio2Engine.InitializeXAudio2(True);

  if FAILED(hr) then
    StatusBar.SimpleText := Format('Could not initialize XAudio2 Error: %d.', [hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if SUCCEEDED(fXaudio2Engine.Stop()) then
    StatusBar.SimpleText := Format('Stopped playing file: %s.', [fAudioFileName]);
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;
  if Assigned(fXaudio2Engine) then
    FreeAndNil(fXaudio2Engine);
  CanClose := True;
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
    fXaudio2Engine.m_VolumeChannels[0] := (trbVolumeL.Position * 0.01);

  // Stereo
  // The first stereo channel (0) is always the LEFT one!
  if (fXaudio2Engine.SoundChannels = 2) then
    begin
      fXaudio2Engine.m_VolumeChannels[0] := (trbVolumeL.Position * 0.01);
      fXaudio2Engine.m_VolumeChannels[1] := (trbVolumeR.Position * 0.01);
    end;
  fXaudio2Engine.SetVolumes(fXaudio2Engine.m_VolumeChannels);
end;


procedure TfrmMain.trbVolumeLChange(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  if (fXaudio2Engine.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeR.Position := trbVolumeL.Position ;
      SetVolumeChannels();
    end;
end;


procedure TfrmMain.trbVolumeRChange(Sender: TObject);
begin

  if not Assigned(fXaudio2Engine) then
    Exit;

  if (fXaudio2Engine.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeL.Position := trbVolumeR.Position;
      SetVolumeChannels();
    end;
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

  // Create the engine
  if SUCCEEDED(hr) then
    fXaudio2Engine := TXaudio2Engine.Create();

  if not Assigned(fXaudio2Engine) then
    Exit;

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
      StatusBar.SimpleText := Format('Ready to play: %s', [ExtractFileName(fAudioFileName)]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
    end;
end;


procedure TfrmMain.OnAudioEndedEvent(var AMessage: TMessage);
begin

  if (AMessage.WParam = 1) then
    begin
      StatusBar.SimpleText := Format('Ended playing: %s', [ExtractFileName(fAudioFileName)]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
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
