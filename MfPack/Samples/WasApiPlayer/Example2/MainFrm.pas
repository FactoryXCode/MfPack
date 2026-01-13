// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 24-08-2024
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Main window that only is the frontend, no calculations or whatever, that
//              could interference with the threading model of this application.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
  WinApi.Windows,
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
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  WASAPIEngine,
  // MFT
  MfAudioBassTrebleTypes,
  // MfPack component
  MfPeakMeter;  // Don't forget to add the Mfpeakmeter location in your project settings.

type

  TfrmMain = class(TForm)
    butPlayPause: TButton;
    butStop: TButton;
    Panel1: TPanel;
    Bevel3: TBevel;
    Bevel2: TBevel;
    lblDuration: TLabel;
    lblProcessed: TLabel;
    lblPlayed: TLabel;
    pmRight: TMfPeakMeter;
    pmLeft: TMfPeakMeter;
    Label1: TLabel;
    Label2: TLabel;
    lblLeftVolume: TLabel;
    lblRightVolume: TLabel;
    cbLockVolumeSliders: TCheckBox;
    trbVolumeL: TTrackBar;
    trbVolumeR: TTrackBar;
    mnuMain: TMainMenu;
    OpenAudioFile1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    dlgOpen: TOpenDialog;
    Panel2: TPanel;
    pbProgress: TProgressBar;
    lblBarPositionInSTime: TLabel;
    lblBarPositionInSamples: TLabel;
    tbBass: TTrackBar;
    tbTreble: TTrackBar;
    lblBass: TLabel;
    Label3: TLabel;
    cbxSetRamp: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    chkResetEQOnNewFile: TCheckBox;
    Bevel1: TBevel;
    stxtStatus: TStaticText;
    edtRampMs: TEdit;
    cbEnableEq: TCheckBox;

    procedure Open1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butPlayPauseClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tbBassChange(Sender: TObject);
    procedure tbTrebleChange(Sender: TObject);
    procedure cbxSetRampChange(Sender: TObject);

  private
    { Private declarations }
    fWasApiEngine: TWasApiEngine;
    fAudioFileUrl: TFileName;
    fFileName: string; // Filename without path.
    llAudioDuration: LONGLONG;

    function GetAudioFile(): string;

    /// <summary>Set Left and/or Right volume.</summary>
    procedure SetVolumeChannels();

    procedure ResetEQ();

    /// <summary>Calculate the linear values of the bass and treble sliders to dB.</summary>
    function SliderToDb(Slider: Integer;
                        MaxDb: Single = 24.0): Single;


    /// <summary>Keep track of data been played.</summary>
    procedure OnAudioDataProcessed(Sender: TObject;
                                   const Position100ns: Int64;
                                   const RawPosition: UInt64);

    /// <summary>Signals the audio is ready to play.</summary>
    procedure OnAudioReady(Sender: TObject);

    /// <summary>Signals the audio reached end.</summary>
    procedure OnAudioEnded(Sender: TObject);
    /// <summary>Signals an engine error.</summary>
    procedure OnEngineError(Sender: TObject;
                            const Msg: string;
                            const Hr: HRESULT);
    /// <summary>Signals the rendering engine state.</summary>
    procedure OnEngineState(Sender: TObject;
                            const NewState: TDeviceState);
public
    { Public declarations }

  end;

var
  frmMain: TfrmMain;



implementation

{$R *.dfm}


uses
  System.Math;


procedure TfrmMain.butPauseClick(Sender: TObject);
begin

  // Play/Pause is handled by the Play button.
  butPlayPauseClick(Sender);
end;

procedure TfrmMain.butPlayPauseClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fWasApiEngine) then
    Exit;

  case fWasApiEngine.DeviceState of

    dsPlay:
      begin
        hr := fWasApiEngine.Pause();
      end;

    dsReady,
    dsStop,
    dsPause:
      begin

        // Activate the peakmeters.
        pmLeft.Enabled := True;
        pmRight.Enabled := True;

        // Keep volume on previous volume.
        SetVolumeChannels();

        hr := fWasApiEngine.Start();
      end;

  else
    Exit;
  end;

  if FAILED(hr) then
    stxtStatus.Caption := Format('Play/Pause failed for file: %s with Error: %d',
                                [fFileName, hr]);
end;


procedure TfrmMain.butStopClick(Sender: TObject);
var
  hr: HResult;

begin

  if not Assigned(fWasApiEngine) then
    Exit;

  hr := fWasApiEngine.Stop();
  if SUCCEEDED(hr) then
    stxtStatus.Caption := Format('Stopped: %s.',
                                [fFileName])
  else
    stxtStatus.Caption := Format('Stopped: %s failed with Error: %s',
                                [fFileName, hr])

end;


procedure TfrmMain.cbxSetRampChange(Sender: TObject);
var
  ms: Integer;

begin

  if not Assigned(fWasApiEngine) then
    Exit;

  case cbxSetRamp.ItemIndex of
    0: fWasApiEngine.SetRampMode(rmOff);
    1: fWasApiEngine.SetRampMode(rmFast);
    2: fWasApiEngine.SetRampMode(rmSmooth);
    3: begin

         fWasApiEngine.SetRampMode(rmCustom);

         // show edit
         edtRampMs.Enabled := True;

         ms := StrToIntDef(Trim(edtRampMs.Text),
                           30);
         if (ms < 0) then
           ms := 0;
         if (ms > 2000) then
           ms := 2000; // sensible clamp

         fWasApiEngine.SetRampTimeMs(ms);
       end;
  end;

  // Disable edit when not manual.
  if (cbxSetRamp.ItemIndex <> 3) then
    edtRampMs.Enabled := False;
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
                    butPlayPauseClick(nil);
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
    stxtStatus.Caption := Format('Adjusting volumes failed with error: %d.',
                                [hr]);
end;


procedure TfrmMain.tbBassChange(Sender: TObject);
begin

  // TrackBar.Position is integer -24..+24
  if Assigned(fWasApiEngine) then
    // fWasApiEngine.SetBassDb(tbBass.Position); Note: dB's are not linear but logarithmic values.
    fWasApiEngine.SetBassDb(Round(SliderToDb(tbBass.Position)));

  // Optional UI label
  // lblBass.Caption := Format('%d dB', [tbBass.Position]);
end;


procedure TfrmMain.tbTrebleChange(Sender: TObject);
begin

  // TrackBar.Position is integer -24..+24
  if Assigned(fWasApiEngine) then
    // fWasApiEngine.SetTrebleDb(tbTreble.Position); Note: dB's are not linear but logarithmic values.
    fWasApiEngine.SetTrebleDb(Round(SliderToDb(tbTreble.Position)));

  // Optional UI label
  // lblTreble.Caption := Format('%d dB', [tbTreble.Position]);
end;


procedure TfrmMain.ResetEQ();
begin

  tbBass.Position := 0;
  tbTreble.Position := 0;
  cbxSetRamp.ItemIndex := 2; // Smooth default, if you want.
  edtRampMs.Text := '30';    // Only if Manual is choosen.

  if Assigned(FWasApiEngine) then
    begin
      FWasApiEngine.SetBassDb(0);
      FWasApiEngine.SetTrebleDb(0);
      fWasApiEngine.SetRampMode(rmSmooth);
    end;
end;


function TfrmMain.SliderToDb(Slider: Integer;
                             MaxDb: Single = 24.0): Single;
const
  Gamma = 2.2; // perceptual curve.

var
  x: Single;

begin

  x := Slider / MaxDb;          // normalize to -1..+1
  Result := Sign(x) * MaxDb * Power(Abs(x), Gamma);
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

  // Set progressbar max
  pbProgress.Max := llAudioDuration div 1000000;

  // Create the engine
  if SUCCEEDED(hr) then
    fWasApiEngine := TWasApiEngine.Create();

  // Wire engine events (Sample 4 principle: callbacks/events only)
  fWasApiEngine.OnReady := OnAudioReady;
  fWasApiEngine.OnProcessed := OnAudioDataProcessed;
  fWasApiEngine.OnEnded := OnAudioEnded;
  fWasApiEngine.OnError := OnEngineError;
  fWasApiEngine.OnStateChanged := OnEngineState;

  if not Assigned(fWasApiEngine) then
    Exit;

  stxtStatus.Caption := Format('Selected file: %s.',
                               [fFileName]);

  // Initialize the engine.
  hr := fWasApiEngine.OpenFile(fAudioFileUrl,
                               llAudioDuration);
  if FAILED(hr) then
    stxtStatus.Caption := Format('Selected file: %s open file failed with error: %d.',
                                [fFileName, hr]);
end;


procedure TfrmMain.pbProgressMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  secPos: Integer;
  hnsPos: Int64;

begin

  if (pbProgress.Max <= 0) or (llAudioDuration <= 0) then
    Exit;

  // Show only when playing/pause
  if fWasApiEngine.DeviceState in [dsPlay, dsPause] then
    begin

      secPos := Trunc((X / pbProgress.Width) * pbProgress.Max);

      if (secPos < 0) then
        secPos := 0
      else
        if (secPos > pbProgress.Max) then
          secPos := pbProgress.Max;

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
  secPos: Int64;
  posHns: Int64;

begin

  if (fWasApiEngine = nil) then
    Exit;

  if (pbProgress.Width <= 0) then
    Exit;

  if (pbProgress.Max <= 0) then
    Exit;

  if (X <= 0) then
    secPos := 0
  else
    if (X >= pbProgress.Width) then
      secPos := pbProgress.Max
  else
    secPos := Trunc((X / pbProgress.Width) * pbProgress.Max); // seconds

  // Seconds -> 100ns
  posHns := secPos * 10000000;

  // clamp to duration (optional safety)
  if (llAudioDuration > 0) and (posHns > llAudioDuration) then
    posHns := llAudioDuration;

  hr := fWasApiEngine.SeekTo(posHns);

  if SUCCEEDED(hr) then
    pbProgress.Position := Integer(secPos)
  else
    stxtStatus.Caption := Format('SeekTo failed. (hr=%d)',
                                 [hr]);
end;


// Event handlers ==============================================================

procedure TfrmMain.OnAudioDataProcessed(Sender: TObject;
                                        const Position100ns: Int64;
                                        const RawPosition: UInt64);
var
  iProgress: LONGLONG;
  iSamples: LONGLONG;
  tstr: string;
  secPos: Integer;

begin


  iProgress := Position100ns;
  iSamples := RawPosition;

  if (pbProgress.Max <= 0) then
    Exit;

  secPos := Integer(Position100ns div 10000000);

  if (secPos < 0) then
    secPos := 0;

  if (secPos > pbProgress.Max) then
    secPos := pbProgress.Max;

  pbProgress.Position := secPos;

  tstr := HnsTimeToStr(iProgress,
                       False);

  lblProcessed.Caption := Format('Samples: %d',
                                 [iSamples]);
  lblPlayed.Caption := Format('Played: %s',
                              [tstr]);
end;


procedure TfrmMain.OnAudioReady(Sender: TObject);
var
  durSec: Int64;

begin

  if (fWasApiEngine = nil) then
    Exit;

  if (llAudioDuration > 0) then
    durSec := llAudioDuration div 10000000
  else
    durSec := 0;

  if (durSec > High(Integer)) then
    pbProgress.Max := High(Integer)
  else
    pbProgress.Max := Integer(durSec);

  pbProgress.Position := 0;
  pbProgress.Enabled := (pbProgress.Max > 0);
  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
  SetVolumeChannels();

  // EQ Bass/treble ============================================================

  if cbEnableEq.Checked then
    begin

      if chkResetEQOnNewFile.Checked then
        begin
          ResetEQ();
        end
      else
        begin

          // re-apply current UI values to engine (important after plugging a new MFT or new file)
          fWasApiEngine.SetBassDb(tbBass.Position);
          fWasApiEngine.SetTrebleDb(tbTreble.Position);
          cbxSetRampChange(nil);
        end;
    end;
  // ===========================================================================
end;


procedure TfrmMain.OnAudioEnded(Sender: TObject);
begin

  stxtStatus.Caption := Format('Stopped: %s',
                              [fFileName]);

  butPlayPause.Enabled := True;
  butPlayPause.Caption := 'Play';
  butStop.Enabled := False;
end;


procedure TfrmMain.OnEngineError(Sender: TObject; const Msg: string; const Hr: HRESULT);
begin

  stxtStatus.Caption := Format('%s (error 0x%.8x)',
                              [Msg, Cardinal(Hr)]);
end;


procedure TfrmMain.OnEngineState(Sender: TObject;
                            const NewState: TDeviceState);
begin

  case NewState of

    dsReady,
    dsStop:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := False;

        pbProgress.Position := 0;
        lblPlayed.Caption := 'Played: 00:00:00';
        lblProcessed.Caption := 'Samples: 0';
      end;

    dsPlay:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Pause';
        butStop.Enabled := True;
        stxtStatus.Caption := Format('Playing: %s',
                                     [fFileName]);
      end;

    dsPause:
      begin

        butPlayPause.Enabled := True;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := True;
        stxtStatus.Caption := Format('Pauzed: %s',
                                     [fFileName]);
      end;

    dsUninitialized,
    dsInitialized:
      begin

        butPlayPause.Enabled := False;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := False;
      end;

    dsError:
      begin

        butPlayPause.Enabled := False;
        butPlayPause.Caption := 'Play';
        butStop.Enabled := False;
        stxtStatus.Caption := Format('Yoo! we have an error: %d', [GetLastError()]);
      end;

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
