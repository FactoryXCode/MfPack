// FactoryX
//
// Copyright: ® FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 24-08-2024
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
// Source:  https://learn.microsoft.com/en-us/windows/win32/coreaudio/rendering-a-stream
//          https://matthewvaneerde.wordpress.com/2008/12/10/sample-playing-silence-via-wasapi-event-driven-pull-mode/
//
// Copyright (c) FactoryX All rights reserved.
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
  System.Diagnostics,
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
  WASAPIEngine,
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
    Bevel2: TBevel;
    Bevel3: TBevel;
    butPause: TButton;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    lblStatus: TLabel;
    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    fWasApiEngine: TWasApiEngine;
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

  if not Assigned(fWasApiEngine) then
    Exit;
  if SUCCEEDED(fWasApiEngine.Pause) then
    lblStatus.Caption := Format('Paused file: %s.',
                                [fFileName]);
end;


procedure TfrmMain.butPlayClick(Sender: TObject);
begin

  if not Assigned(fWasApiEngine) then
    Exit;

  // Activate the peakmeters.
  pmLeft.Enabled := True;
  pmRight.Enabled := True;

  // Keep volume on previous volume.
  SetVolumeChannels();

  if SUCCEEDED(fWasApiEngine.Start()) then
    lblStatus.Caption := Format('Playing file: %s.',
                                [fFileName]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
begin

  if not Assigned(fWasApiEngine) then
    Exit;
  if SUCCEEDED(fWasApiEngine.Stop()) then
    lblStatus.Caption := Format('Stopped playing file: %s.',
                                [fFileName]);
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin

  Close();
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;

  FreeAndNil(pmLeft);
  FreeAndNil(pmRight);

  if Assigned(fWasApiEngine) then
    FreeAndNil(fWasApiEngine);

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

  case Key of
    VK_SPACE:   if Assigned(fWasApiEngine) then
                  begin
                    butPlayClick(nil);
                  end;

    VK_END:     if Assigned(fWasApiEngine) then
                  begin
                    butStopClick(nil);
                  end;

    VK_F10:     if Assigned(fWasApiEngine) then
                  begin
                    butPauseClick(nil);
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
var
  hr: HResult;

begin

  hr := E_FAIL;

  if not Assigned(fWasApiEngine) then
    Exit;

  // Stereo
  // The first stereo channel (0) is always the LEFT one! SetVolumes
  if (fWasApiEngine.SoundChannels = 2) then
    begin
      hr := fWasApiEngine.SetVolumes(Abs(trbVolumeL.Position) * 0.01,
                                     Abs(trbVolumeR.Position) * 0.01);
    end;

  if FAILED(hr) then
    lblStatus.Caption := 'Adjusting volumes failed.';
end;


procedure TfrmMain.trbVolumeLChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeR.Position := trbVolumeL.Position;

  SetVolumeChannels();

  vol := (MapRange((trbVolumeL.Position),
                    trbVolumeL.Max,
                    trbVolumeL.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblLeftVolume.Caption := Format('%d',
                                  [Trunc(vol)]) + '%';
end;


procedure TfrmMain.trbVolumeRChange(Sender: TObject);
var
  vol: Single;

begin

  if (cbLockVolumeSliders.Checked = True) then
    trbVolumeL.Position := trbVolumeR.Position;

  SetVolumeChannels();

  vol := (MapRange((trbVolumeR.Position),
                    trbVolumeR.Max,
                    trbVolumeR.Min,
                    MIN_VOLUME,
                    MAX_VOLUME) / (MAX_VOLUME / 100));

  lblRightVolume.Caption := Format('%d',
                                   [Trunc(vol)]) + '%';
end;


procedure TfrmMain.Open1Click(Sender: TObject);
var
  hr: HResult;

begin

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
    fWasApiEngine := TWasApiEngine.Create();

  if not Assigned(fWasApiEngine) then
    Exit;

  lblStatus.Caption := Format('Selected file: %s.',
                              [fFileName]);

  // Initialize the engine.
  hr := fWasApiEngine.LoadFile(Handle,
                               fAudioFileUrl,
                               llAudioDuration);
  if SUCCEEDED(hr) then
    SetVolumeChannels();
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

  tstr := HnsTimeToStr(iProgress,
                       False);

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

      lblStatus.Caption := Format('Ready to play: %s',
                                  [fFileName]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
    end;
end;


procedure TfrmMain.OnAudioEndedEvent(var AMessage: TMessage);
begin

  if (AMessage.WParam = 1) then
    begin

      lblStatus.Caption := Format('Stopped playing: %s',
                                  [fFileName]);
      butPlay.Enabled := True;
      butPause.Enabled := True;
      butStop.Enabled := True;
    end;
end;


// initialization and finalization =============================================


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
