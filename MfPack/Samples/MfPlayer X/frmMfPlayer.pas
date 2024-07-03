// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMfPlayer.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 3.1.6
// Description: MfPlayer X: Requires Windows 7 or later.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
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
// Source: Parts of CPlayer Examples
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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

unit frmMfPlayer;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Types,
  System.Classes,
  System.UITypes,
  {RTTI}
  System.Rtti,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.WmCodecDsp,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  MfPlayerClassX,
  FloatingFrm,
  TimedTextClass,
  LangTags,
  MfPCXConstants,
  QueueTimer;

type
  // By default the form is the videowindow.

  Tfrm_MfPlayer = class(TForm)
    dlgOpenUrl: TOpenDialog;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    muSeparator1: TMenuItem;
    mnuExit: TMenuItem;
    mnuExtra: TMenuItem;
    mnuSetPosition: TMenuItem;
    pnlVideo: TPanel;
    pnlControls: TPanel;
    butStop: TButton;
    butPause: TButton;
    butPlay: TButton;
    trbVolumeL: TTrackBar;
    prbProgress: TProgressBar;
    mnuTakeScreenshot: TMenuItem;
    butFullScreen: TButton;
    N1: TMenuItem;
    Rate1: TMenuItem;
    Ratep2: TMenuItem;
    Ratem1: TMenuItem;
    mnuSetRate: TMenuItem;
    trbVolumeR: TTrackBar;
    cbLockVolumeSliders: TCheckBox;
    mnuEnableSubtitling: TMenuItem;
    N2: TMenuItem;
    mnuSelectStreams: TMenuItem;
    N3: TMenuItem;
    mnuMediaInfo: TMenuItem;
    mnuSubTitling: TMenuItem;
    mnuLanguage: TMenuItem;
    N4: TMenuItem;
    mnuAspectRatio: TMenuItem;
    mnuCinema: TMenuItem;
    mnuSixteenByNine: TMenuItem;
    mnuFourByThree: TMenuItem;
    QTimer1: TQTimer;
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSetPositionClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure trbVolumeLChange(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure mnuTakeScreenshotClick(Sender: TObject);
    procedure butFullScreenClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Rate1Click(Sender: TObject);
    procedure Ratep2Click(Sender: TObject);
    procedure Ratem1Click(Sender: TObject);
    procedure trbVolumeRChange(Sender: TObject);
    procedure prbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuEnableSubtitlingClick(Sender: TObject);
    procedure mnuSelectStreamsClick(Sender: TObject);
    procedure mnuMediaInfoClick(Sender: TObject);
    procedure mnuLanguageClick(Sender: TObject);
    procedure mnuCinemaClick(Sender: TObject);
    procedure mnuSixteenByNineClick(Sender: TObject);
    procedure mnuFourByThreeClick(Sender: TObject);

  private
    { Private declarations }
    bAppIsClosing: Boolean;
    pb_IsFullScreen: Boolean;
    ps_AspectRatio: Single;
    ph_FloatingForm: HWnd;
    sMediaFileName: WideString;

    { Private methods }
    // Size and move methods, needed by FloatingForm to adjust position and size.
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    // Forces a resize to set aspect ratio
    procedure ForceResize();
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
    procedure SetVolumeChannels(volchans: TFloatArray);

  public
    { Public declarations }

    { Public methods }
    procedure SetWindowStyle(bFullScreen: Boolean);
    procedure ResetInterface();
    procedure SetToParentRect();
    procedure RealignInterface();
    function GetFmPlayer(): HRESULT;
    procedure QuitMfPlayerSession();
  end;

var
  frm_MfPlayer: Tfrm_MfPlayer;


implementation

{$R *.dfm}

uses
  Vcl.ClipBrd,
  dlgStreamSelect,
  dlgTimedTextLanguages;


procedure Tfrm_MfPlayer.mnuExitClick(Sender: TObject);
begin
  Close();
end;


procedure Tfrm_MfPlayer.mnuTakeScreenshotClick(Sender: TObject);
var
  bm: TBitmap;

begin
  bm:= TBitmap.Create;
  MfPlayerX.TakeSnapShot(bm);
  Clipboard.Assign(bm);
  FreeAndNil(bm);
end;


procedure Tfrm_MfPlayer.butPauseClick(Sender: TObject);
begin
  if Assigned(MfPlayerX) then
    if MfPlayerX.State in [OpenPending, Started, TopologyReady] then
      begin
        MfPlayerX.SendPlayerRequest(reqPause);
      end;
end;


procedure Tfrm_MfPlayer.butPlayClick(Sender: TObject);
begin

  // Continue where we left when state is paused
  if Assigned(MfPlayerX) then
    begin
      if MfPlayerX.State in [Paused] then
        begin
          MfPlayerX.SendPlayerRequest(reqStart);
        end;

      // Start a new session
      if MfPlayerX.State in [OpenPending, Stopped, TopologyReady] then
        begin

          // Set initial volume
          MfPlayerX.GetVolume();

          if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
            trbVolumeR.Position:= Trunc(MfPlayerX.m_VolumeChannels[0] * 30); // left  30%
          if (Length(MfPlayerX.m_VolumeChannels) > 1) then
            trbVolumeL.Position := Trunc(MfPlayerX.m_VolumeChannels[1] * 30); // right 30%

          // Set progressbar max
          prbProgress.Max := prbProgress.Width;

          // Use the layer to show a custom message
          if Assigned(FloatingForm) then
            begin
              FloatingForm.SubtitleText := 'Playing ' + ExtractFileName(MfPlayerX.MediaFileName);
              Sleep(1000);  // 1 sec
              FloatingForm.SubtitleText := '';
            end;

          // Start session
          MfPlayerX.SendPlayerRequest(reqStart);
          // Enable screenshot menu
          mnuTakeScreenshot.Enabled := True;

          // If we want to implement the MFPeakmeters..
          // Activate the peaklevel meters
          //MfPeakMeter1.Enabled:= True;
          //MfPeakMeter2.Esnabled:= True;
        end;
    end;
end;


procedure Tfrm_MfPlayer.butStopClick(Sender: TObject);
begin
  if Assigned(MfPlayerX) then
    begin
      MfPlayerX.SendPlayerRequest(reqStop);
      mnuTakeScreenshot.Enabled:= False;
      prbProgress.Position:= 0;

      // Stop MFPeakmeters
      //MfPeakMeter1.Enabled:= False;
      //MfPeakMeter2.Enabled:= False;

    end;
end;


procedure Tfrm_MfPlayer.SetWindowStyle(bFullScreen: Boolean);
begin
  if (bFullScreen = True) then
    begin
      BorderStyle := bsNone;
      pnlControls.Visible := False;
      mnuFile.Visible := False;
      mnuExtra.Visible := False;
      WindowState := wsMaximized;
      pb_IsFullScreen := True;
    end
  else
    begin
      pb_IsFullScreen := False;
      pnlControls.Visible := True;
      mnuFile.Visible := True;
      mnuExtra.Visible := True;
      WindowState := wsNormal;
      BorderStyle := bsSizeable;
    end;
end;


procedure Tfrm_MfPlayer.ResetInterface();
begin
  prbProgress.Position := 0;
  mnuSetPosition.Enabled := False;
  pnlControls.Enabled := False;
  mnuTakeScreenshot.Enabled := False;
  mnuSubtitling.Enabled := False;
  mnuSelectStreams.Enabled := False;
  mnuMediaInfo.Enabled := False;

  // Stop MFPeakmeters
  //MfPeakMeter1.Enabled := False;
  //MfPeakMeter2.Enabled := False;

end;


// show some audio info
procedure Tfrm_MfPlayer.mnuMediaInfoClick(Sender: TObject);
var
  i: Integer;
  lst: string;

begin


  for i := 0 to High(MfPlayerX.StreamContents) do
    begin
      if (MfPlayerX.StreamContents[i].idStreamMediaType = mtVideo) then
        begin
          lst := 'Video Info' + #13 + #13;
          lst := lst + 'Pixel aspect ratio: ' + FloatToStrF(MfPlayerX.StreamContents[i].video_PixelAspectRatioNumerator / MfPlayerX.StreamContents[i].video_PixelAspectRatioDenominator, ffGeneral, 4, 2) + #13;
          lst := lst + 'Framerate per second (fps): ' + FloatToStrF(MfPlayerX.StreamContents[i].video_FrameRateNumerator / MfPlayerX.StreamContents[i].video_FrameRateDenominator, ffGeneral, 4, 2) + #13;
          lst := lst + 'Framesize (w x h): ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeWidth) + ' x ' + IntToStr(MfPlayerX.StreamContents[i].video_FrameSizeHeigth) + #13 + #13;
        end
      else
      if (MfPlayerX.StreamContents[i].idStreamMediaType = mtAudio) then
        begin
          lst := lst + 'Audio Info' + #13 + #13;
          lst := lst + 'Format        : ' + MfPlayerX.StreamContents[i].audio_wsAudioDescr + #13;
          lst := lst + 'Channels      : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iAudioChannels) + #13;
          lst := lst + 'FormatTag     : ' + IntToStr(MfPlayerX.StreamContents[i].audio_dwFormatTag) + #13;
          lst := lst + 'SamplesPerSec : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iSamplesPerSec) + #13;
          lst := lst + 'BitsPerSample : ' + IntToStr(MfPlayerX.StreamContents[i].audio_iBitsPerSample) + #13 + #13;
          lst := lst + 'Compressed    : ' + BoolToStr(MfPlayerX.StreamContents[i].bCompressed) + #13;
        end;
    end;
  ShowMessage(lst);
end;


// A different but effective way to play in full screen mode
procedure Tfrm_MfPlayer.butFullScreenClick(Sender: TObject);
begin

  if pb_IsFullScreen then
    SetWindowStyle(False)
  else
    SetWindowStyle(True);

  // Prior to SDK version RedStone5, for fullscreen modus you could use this function:
  // IMFVideoDisplayControl.GetFullscreen /  IMFVideoDisplayControl.SetFullscreen
  // However this API is deprecated and not functioning since SDK version RedStone5
  //===============================================================================

end;


procedure Tfrm_MfPlayer.QuitMfPlayerSession();
begin
  ResetInterface();
  if Assigned(MfPlayerX) then
    begin
      // because we work in A-sync mode, we have to wait until the player
      // reached the Stopped state.
      if (MfPlayerX.State <> Stopped) and not (MfPlayerX.State = closed) then
        MfPlayerX.Stop();
      if (MfPlayerX.State in [Stopped, closed]) then
        begin
          MfPlayerX.ShutDown();

          MfPlayerX.Free;
          MfPlayerX := nil;
        end;
    end;

end;


procedure Tfrm_MfPlayer.mnuOpenClick(Sender: TObject);
begin

  // End a previous session
  QuitMfPlayerSession();

  if SUCCEEDED(GetFmPlayer()) then
    begin
      if dlgOpenUrl.Execute then
        begin
          if SUCCEEDED(MfPlayerX.OpenURL(PWideChar(dlgOpenUrl.Filename))) then
            begin
              mnuSetPosition.Enabled := True;
              pnlControls.Enabled := True;
              mnuTakeScreenshot.Enabled := True;
              mnuMediaInfo.Enabled := True;
              sMediaFileName := dlgOpenUrl.Filename;

              // enable subtitling
              if (MfPlayerX.m_hwndFloatingForm > 0) then
                begin
                  mnuSubtitling.Enabled := True;
                  mnuEnableSubtitling.Checked := True;
                  // Get the handle from the floatingForm, this form is initialized
                  // when a valid subtitlefile is loaded.
                  ph_FloatingForm := MfPlayerX.m_hwndFloatingForm;
                  SetToParentRect(); // Adjust floatingform position and dimensions
                end
              else
                begin
                  mnuSubtitling.Enabled := False;
                  mnuEnableSubtitling.Checked := False;
                end;

              RealignInterface();
            end //SUCCEEDED
          else
            MessageBox(0,
                       lpcwstr('MfPlayer could not open ' + #13 +
                       lpcwstr(dlgOpenUrl.Filename)),
                       lpcwstr('Initial Failure!'),
                       MB_ICONERROR);
        end // dlg execute
      else // User pressed cancel
        QuitMfPlayerSession();
    end
  else // could not init MfPlayer
    MessageBox(0,
               lpcwstr('Failed to initialize MfPlayer.'),
               lpcwstr('Initial Failure!'),
               MB_ICONERROR);
end;


procedure Tfrm_MfPlayer.mnuEnableSubtitlingClick(Sender: TObject);
begin
  if Assigned(FloatingForm) then
    begin
      if (mnuEnableSubtitling.Checked = True) then
        begin
          FloatingForm.Show;
          SetToParentRect();
        end
      else
        FloatingForm.Hide;
    end;
end;


procedure Tfrm_MfPlayer.Rate1Click(Sender: TObject);
begin
  // Set video rate back to normal
  MfPlayerX.SetRate(1.0);
end;


procedure Tfrm_MfPlayer.Ratep2Click(Sender: TObject);
begin
  // Set video rate to maximum speed
  MfPlayerX.SetRate(MfPlayerX.MaxPlayBackRate);
end;


procedure Tfrm_MfPlayer.Ratem1Click(Sender: TObject);
begin
  // Set video rate to minimum speed
  MfPlayerX.SetRate(MfPlayerX.MinPlayBackRate);
end;


// Implementation for FloatingForm  (subtitlelayer)
procedure Tfrm_MfPlayer.SetToParentRect();
begin
  if (MfPlayerX.m_hwndFloatingForm > 0) then
    SendMessage(ph_FloatingForm,
                WM_PARENTPOSCHANGED,
                WPARAM(0),
                LPARAM(0));
end;


procedure Tfrm_MfPlayer.RealignInterface();
var
  crD: TRECT;
  pcrD: LPRECT;

begin
  if bAppIsClosing then
    Exit;

  if (prbProgress <> Nil) then
    prbProgress.Max:= prbProgress.Width;

  // Set video size
  if Assigned(MfPlayerX) then
    begin
      crD.left := 0;
      crD.top := 0;
      crD.right := pnlVideo.ClientWidth;
      crD.bottom := pnlVideo.ClientHeight;
      CopyTRectToLPRect(crD, pcrD);
      //Stop flickering of controls and subtitle when resizing.
      MfPlayerX.ResizeVideo(pcrD);
    end;
end;


procedure Tfrm_MfPlayer.pnlVideoResize(Sender: TObject);
begin
  RealignInterface();
end;


// Seek
procedure Tfrm_MfPlayer.prbProgressMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  fPos: Float;

begin
  // Clear any subtitle
  if Assigned(FloatingForm) then
    FloatingForm.SubtitleText := '';

  if (X <= 0) then
    fPos := 0.0
  else
    fPos := ((X / prbProgress.Width) * MfPlayerX.Duration);

  MfPlayerX.SetNewPosition := trunc(fPos); // set new StartPosition
  MfPlayerX.SendPlayerRequest(reqSeek);
  prbProgress.Position := X;
end;


procedure Tfrm_MfPlayer.mnuSetPositionClick(Sender: TObject);
const
  iSec: integer = 1000000;  // 1000000 * 100 nanoseconds is 1 second

var
  sPos: string;
  sDur: string;
  iDef: int64;

begin
  // The calculated value is not accurate!
  sDur := IntToStr(int64(Trunc(MfPlayerX.Duration / (iSec * 60))));
  sPos := InputBox('Enter a position in seconds',
                   'Enter a value between 0 and ' + sDur + '.',
                   '1');

  iDef := StrToInt64Def(sPos, 0) * iSec;
  if (iDef >= MfPlayerX.Duration) then
    iDef := 0; // Set back to start position

  MfPlayerX.SetNewPosition:= iDef; // set new StartPosition
  MfPlayerX.SendPlayerRequest(reqSeek);
end;


procedure Tfrm_MfPlayer.mnuCinemaClick(Sender: TObject);
begin
  ps_AspectRatio := AR_235_1;
  ForceResize();
end;


procedure Tfrm_MfPlayer.mnuSixteenByNineClick(Sender: TObject);
begin
  ps_AspectRatio := AR_16_9; // default
  ForceResize();
end;


procedure Tfrm_MfPlayer.mnuFourByThreeClick(Sender: TObject);
begin
  ps_AspectRatio := AR_4_3;
  ForceResize();
end;


// VOLUMES ---------------------------------------------------------------------

procedure Tfrm_MfPlayer.SetVolumeChannels(volchans: TFloatArray);
var
  iSliderL, iSliderR: FLOAT;

begin
  iSliderL := (trbVolumeL.Position * 0.01);
  iSliderR := (trbVolumeR.Position * 0.01);

  // Mono
  // This is a very rare case, because mono is played on the leftchannel only or
  // on both channels without stereo effect.
  if (MfPlayerX.SoundChannels = 1) then
    MfPlayerX.m_VolumeChannels[0] := iSliderL;

  // Stereo
  // The first stereo channel (0) is always the LEFT one!
  if (MfPlayerX.SoundChannels = 2) then
    begin
      MfPlayerX.m_VolumeChannels[0] := iSliderL;
      MfPlayerX.m_VolumeChannels[1] := iSliderR;
    end;

  // DD 5.1 (AC3)
  // DD5.1 has 2 kind of formats:
  //       channels: 1  2  3  4    5   6
  // SMPTE standard: R, L, C, LFE, Rs, Ls  (most used because of R&L layout is the same as Stereo)
  // Film:           L, C, R, Ls,  Rs, LFE
  //
  // Frequencies: R, L, C, Rs and Ls: 20-20.000 Hz
  //              LFE: 20-120 Hz
  //
  // Wether or not we are dealing with Dolby Digital, use the class identifier CLSID_CMSDolbyDigitalEncMFT
  //
  // SMPTE to stereo
  // Here we combine the channels with both sliders (Right and Left)
  //
  // If (MfPlayer.m_aStreamCont[High(MfPlayer.m_aStreamCont)].dwFormatTag = MEDIASUBTYPE_DVM) then
  // You could assign more volumecontrols for the known sound decoder.
  if (MfPlayerX.SoundChannels = 6) then
    begin
      // Channel 1  R
      MfPlayerX.m_VolumeChannels[0] := iSliderR;
      // Channel 2  L
      MfPlayerX.m_VolumeChannels[1] := iSliderL;
      // Channel 3  C >> most of the time, this is the character's voice channel.
      MfPlayerX.m_VolumeChannels[2] := iSliderL;
      // Channel 4  LFE
      MfPlayerX.m_VolumeChannels[3] := (iSliderR + iSliderL) / 2;
      // Channel 5  Rs
      MfPlayerX.m_VolumeChannels[4] := iSliderR;
      // Channel 6  LS
      MfPlayerX.m_VolumeChannels[5] := iSliderL;
    end;
  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
end;

// Right volume channel
procedure Tfrm_MfPlayer.trbVolumeRChange(Sender: TObject);
begin
  if not Assigned(MfPlayerX) then
    Exit;

  if (MfPlayerX.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeL.Position := trbVolumeR.Position;
      SetVolumeChannels(MfPlayerX.m_VolumeChannels);
    end;
end;

// Left volume channel
procedure Tfrm_MfPlayer.trbVolumeLChange(Sender: TObject);
begin
  if not Assigned(MfPlayerX) then
    Exit;

  if (MfPlayerX.SoundChannels > 0) then
    begin
      if (cbLockVolumeSliders.Checked = True) then
        trbVolumeR.Position := trbVolumeL.Position;
      SetVolumeChannels(MfPlayerX.m_VolumeChannels);
    end;
end;
//------------------------------------------------------------------------------

procedure Tfrm_MfPlayer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  QuitMfPlayerSession();
  CanClose := True;
  bAppIsClosing := CanClose;
end;


procedure Tfrm_MfPlayer.FormCreate(Sender: TObject);
begin
  prbProgress.Max := prbProgress.Width;
  bAppIsClosing := False;
  pb_IsFullScreen := False;

end;


procedure Tfrm_MfPlayer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  bm: TBitmap;

begin
  case Key of
    VK_SPACE:   if Assigned(MfPlayerX) then
                  begin
                    case MfPlayerX.State of
                      Started: butPauseClick(Self);
                      OpenPending: butPlayClick(Self);
                      Stopped, Paused: butPlayClick(Self);
                    end;
                  end;

    VK_END:     if Assigned(MfPlayerX) then
                  begin
                    butStopClick(Self);
                  end;

    VK_F11:     begin  //Shut down
                  if Assigned(MfPlayerX) then
                    MfPlayerX.ShutDown();
                end;

    VK_F12:     mnuOpenClick(Nil);

    //take a snapshot and copy the bitmap to the clipboard
    VK_F8:      begin
                  bm:= TBitmap.Create;
                  MfPlayerX.TakeSnapShot(bm);
                  Clipboard.Assign(bm);
                  FreeAndNil(bm);
                end;

    // Use left and right arrows to adjust the volume.
    // since there is a trackbar, this one will have the focus.
    // We did not implement a balance method.
    VK_LEFT:    begin
                  if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
                    MfPlayerX.m_VolumeChannels[0]:= MfPlayerX.m_VolumeChannels[0] - 0.01;
                  if (Length(MfPlayerX.m_VolumeChannels) > 1) then
                    MfPlayerX.m_VolumeChannels[1]:= MfPlayerX.m_VolumeChannels[1] - 0.01;
                  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
                end;

    VK_RIGHT:   begin
                  if (Length(MfPlayerX.m_VolumeChannels) >= 1) then
                    MfPlayerX.m_VolumeChannels[0]:= MfPlayerX.m_VolumeChannels[0] + 0.01;
                  if (Length(MfPlayerX.m_VolumeChannels) > 1) then
                    MfPlayerX.m_VolumeChannels[1]:= MfPlayerX.m_VolumeChannels[1] + 0.01;
                  MfPlayerX.SetVolume(MfPlayerX.m_VolumeChannels);
                end;

    VK_ESCAPE:  begin
                  butFullScreenClick(Self);
                end;
  end;
end;


function Tfrm_MfPlayer.GetFmPlayer(): HRESULT;
begin
  Result:= E_FAIL;
  if not Assigned(MfPlayerX) then
    begin
      MfPlayerX := nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      MfPlayerX := TMfPlayerX.Create(pnlVideo.Handle,       // The clipping window / control
                                     0,                     // The window or control that receives the custom messages. (like Subtitles)
                                     frm_MfPlayer.Handle,   // The window or control that receives the (text) messages.
                                     frm_MfPlayer.Handle);  // Must be main form or parent window !!!

      // If you want to switch to a different clipping surface while the session is active:
      //MfPlayer.SetVideoSurface:= myFormOrControl.Handle;
      Result:= S_OK;
    end;
end;


procedure Tfrm_MfPlayer.mnuLanguageClick(Sender: TObject);
begin

  if Assigned(MfPlayerX) then
    begin
      // no TimedText files found; nothing to do here.
      if MfPlayerX.m_hwndFloatingForm = 0 then
        Exit;
      // Show language dialog
      // No dialog initiated on startup, this should not happen.
      if not Assigned(dlgTimedTextLang) then
        begin
          ShowMessage('No TimedText Language selection dialog available.');
          Exit;
        end;
      DlgTimedTextLang.Show;
    end;
end;


//------------------------------------------------------------------------------
// Size and move methods, needed by FloatingForm to adjust position and size.
//------------------------------------------------------------------------------
procedure Tfrm_MfPlayer.WMSize(var Msg: TWMSize);
begin
  inherited;  // OnResize method will be handled first
  if bAppIsClosing then
    Exit;
  if Assigned(FloatingForm) then
    SendMessage(ph_FloatingForm,
                WM_PARENTSIZECHANGED,
                0,
                0);
  RealignInterface();
end;


procedure Tfrm_MfPlayer.WMMove(var Msg: TWMMove);
begin
  inherited;
  if bAppIsClosing then
    Exit;

  if Assigned(FloatingForm) then
    SendMessage(ph_FloatingForm,
                WM_PARENTPOSCHANGED,
                0,
                0);
  RealignInterface();
end;


procedure Tfrm_MfPlayer.ForceResize();
var
  pr: PRect;
  rc: TRect;

begin

  rc.Left := Left;
  rc.Top := Top;
  rc.Width := Width;
  rc.Height := Height;
  pr := @rc;

  // Force a resize to set aspectratio
  SendMessage(Handle,
              WM_SIZING,
              WParam(WMSZ_TOP),
              LParam(pr));
end;


procedure Tfrm_MfPlayer.WMProgressEvent(var Msg: TMessage);
begin //Position

  if bAppIsClosing then
    Exit;

  // WParam 1 is a subtitle text event
  if (Msg.WParam = 1) then
    begin
      if (MfPlayerX.State = Started) then
        prbProgress.Position := Trunc((prbProgress.Width / (MfPlayerX.Duration / ONE_HNS_MSEC)) * (MfPlayerX.Position));

      if (MfPlayerX.State In [Closed, Stopped]) then
        ResetInterface();

      // Check if the topology is set.
      // The reason is that we have to deal with asynchronous operations.
      // Note: Topology will be set when a mediafile is loaded.
      if (MfPlayerX.State = TopologyReady) then
        begin
          // Check if forward rate is supported
          mnuSetRate.Enabled := MfPlayerX.CanSetRateForward;
          mnuSelectStreams.Enabled:= True;
        end;
    end;
end;


// Show a dialog to select audio, video or other streams
procedure Tfrm_MfPlayer.mnuSelectStreamsClick(Sender: TObject);
begin
  // No dialog initiated on startup, this should not happen.
  if not Assigned(dlgSelectStreams) then
    begin
      ShowMessage('No stream selection dialog available.');
      Exit;
    end;
  dlgSelectStreams.Show;
end;

end.
