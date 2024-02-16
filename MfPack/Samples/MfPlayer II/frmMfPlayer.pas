// FactoryX
//
// Copyright © FactoryX. Allrights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmMfPlayer.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 3.1.6
// Description: This is the basic class of MfPlayer,
//              containing the necessary methodes to play a mediafile
//              For indepth information see the included examples (CPlayer)
//              and text files containing the complete information about
//              MfPlayer.
//
// Organisation: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
// Rudy Velthuis 1960 ~ 2019.
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
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
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
  {Application}
  MfPlayerClass,
  frmFullScreenLayer;

type
  // By default the form is the videowindow.

  Tfrm_MfPlayer = class(TForm)
    dlgOpenUrl: TOpenDialog;
    MainMenu1: TMainMenu;
    muFile: TMenuItem;
    muOpen: TMenuItem;
    muSeparator1: TMenuItem;
    muExit: TMenuItem;
    Extra1: TMenuItem;
    mnuSetPosition: TMenuItem;
    pnlControls: TPanel;
    butStop: TButton;
    butPause: TButton;
    butPlay: TButton;
    trbVolume: TTrackBar;
    prbProgress: TProgressBar;
    mnuTakeScreenshot: TMenuItem;
    butFullScreen: TButton;
    pnlVideo: TPanel;
    stxtSubs: TStaticText;
    procedure muOpenClick(Sender: TObject);
    procedure muExitClick(Sender: TObject);
    procedure mnuSetPositionClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure trbVolumeChange(Sender: TObject);
    procedure mnuTakeScreenshotClick(Sender: TObject);
    procedure butFullScreenClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private declarations }
    bFullScreenMode: Boolean;
    FullScreenLayer: TFullScreenLayer;

    function GetFmPlayer(): HRESULT;
    procedure DestroyMfPlayer();

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;

  public
    { Public declarations }
    procedure RealignInterface();
    procedure SetProgressbarPosition(iPos: int64);

  end;


var
  frm_MfPlayer: Tfrm_MfPlayer;


implementation

{$R *.dfm}

uses
  Vcl.ClipBrd;


procedure Tfrm_MfPlayer.muExitClick(Sender: TObject);
begin
  Close();
end;


procedure Tfrm_MfPlayer.mnuTakeScreenshotClick(Sender: TObject);
var
  bm: TBitmap;

begin
  bm := TBitmap.Create;
  MfPlayer.TakeSnapShot(bm);
  // copy the bitmap to the Clipboard
  Clipboard.Assign(bm);
  FreeAndNil(bm);
end;


procedure Tfrm_MfPlayer.butPauseClick(Sender: TObject);
begin
  if Assigned(MfPlayer) then
    if MfPlayer.State in [OpenPending, Started] then
      MfPlayer.SendPlayerRequest(reqPause);
end;


procedure Tfrm_MfPlayer.butPlayClick(Sender: TObject);
begin
  if Assigned(MfPlayer) then
    if MfPlayer.State in [OpenPending, Stopped, Paused] then
      begin
        prbProgress.Max := 1000000;  // = 1 sec
        prbProgress.Enabled := True;
        if MfPlayer.State in [Started, Ready, OpenPending] then
          trbVolume.Position := Trunc(MfPlayer.Volume) * 10;

        pnlVideo.Caption := '';
        MfPlayer.SendPlayerRequest(reqStart);
        mnuTakeScreenshot.Enabled := True;
      end;
end;


procedure Tfrm_MfPlayer.butStopClick(Sender: TObject);
begin
  if Assigned(MfPlayer) then
    begin
      SetProgressbarPosition(0);
      MfPlayer.SendPlayerRequest(reqStop);
      mnuTakeScreenshot.Enabled := False;
    end;
end;


// A different but effective way to play in full screen mode
procedure Tfrm_MfPlayer.butFullScreenClick(Sender: TObject);
var
  rc: TRect;
  prc: LPRECT;

begin
   prc := Nil;

   if (bFullScreenMode = False) then
     begin
        //Create the form TFullScreenLayer
        if Not Assigned(FullScreenLayer) then
          FullScreenLayer := TFullScreenLayer.Create(Nil);

        with FullScreenLayer do
          begin
            Visible := true;
            Top := 0;
            Left := 0;
            Width := Screen.Width;
            Height := Screen.Height;
          end;

        rc.Top := 0;
        rc.Left := 0;
        rc.right := Screen.Width;
        rc.bottom := Screen.Height;

        // Let MfPlayer know we changed the clipping window.
        MfPlayer.SetVideoSurface := FullScreenLayer.Handle;
        // copy TRECT to LPRECT
        CopyTRectToLPRect(rc, prc);
        MfPlayer.ResizeVideo(prc);
        bFullScreenMode := True;
     end
   else  // if FullScreenMode = False, get to the original window state
    begin
      bFullScreenMode := False;

      rc.Top := pnlVideo.Top;
      rc.Left := pnlVideo.Left;
      rc.right := pnlVideo.Width;
      rc.bottom := pnlVideo.Height;

      MfPlayer.SetVideoSurface := pnlVideo.Handle;

      // copy NRECT to LPRECT
      CopyTRectToLPRect(rc, prc);
      MfPlayer.ResizeVideo(prc);

      //close the form TFullScreenLayer
      if Assigned(FullScreenLayer) then
        begin
          FullScreenLayer.Close();
        end;
    end;

  // Prior to SDK version RedStone5, for fullscreen modus you could use this function:
  // IMFVideoDisplayControl.GetFullscreen /  IMFVideoDisplayControl.SetFullscreen
  // However this API is deprecated and not functioning since SDK version RedStone5
  //===============================================================================

end;


procedure Tfrm_MfPlayer.DestroyMfPlayer();
begin
  if Assigned(MfPlayer) then
    begin
      //close the form TFullScreenLayer
      if Assigned(FullScreenLayer) then
        FreeAndNil(FullScreenLayer);
      if SUCCEEDED(MfPlayer.ShutDown()) then
        begin
          prbProgress.Enabled := False;
          mnuSetPosition.Enabled := False;
          pnlControls.Enabled := false;
          mnuTakeScreenshot.Enabled := False;
          FreeAndNil(MfPlayer);
        end;
    end;
end;


procedure Tfrm_MfPlayer.muOpenClick(Sender: TObject);
begin
  // End the previous session
  DestroyMfPlayer();

  if SUCCEEDED(GetFmPlayer()) then
    begin
      if dlgOpenUrl.Execute then
        MfPlayer.OpenURL(PWideChar(dlgOpenUrl.Filename));
        mnuSetPosition.Enabled := True;
        mnuTakeScreenshot.Enabled := True;
        pnlControls.Enabled := True;
    end
  else
    MessageBox(0,
               lpcwstr('Failed to initialize MfPlayer.'),
               lpcwstr('Initial Failure!'),
               MB_ICONERROR);
end;


procedure Tfrm_MfPlayer.WMSize(var Msg: TWMSize);
begin
  Inherited;  // OnResize method will be handled first
  RealignInterface();
end;


procedure Tfrm_MfPlayer.WMProgressEvent(var Msg: TMessage);
begin
  // WParam 1 is a subtitle text event
  if (Msg.WParam = 1) then
    if (MfPlayer.State = Started) then
      SetProgressbarPosition(MfPlayer.Position);
end;


procedure Tfrm_MfPlayer.SetProgressbarPosition(iPos: int64);
begin
  if prbProgress.Enabled then
    begin
      prbProgress.Position := Trunc((prbProgress.Width / MfPlayer.Duration) * iPos);
    end;
end;


procedure Tfrm_MfPlayer.RealignInterface();
var
  crD: TRECT;
  pcrD: LPRECT;

begin
  if Assigned(prbProgress) then
    prbProgress.Max:= prbProgress.Width;

  // Set video size
  if Assigned(MfPlayer) then
    begin
      crD.left:= 0;
      crD.top:= 0;
      crD.right:= pnlVideo.ClientWidth;
      crD.bottom:= pnlVideo.ClientHeight;
      pcrD:= @crD;
      MfPlayer.ResizeVideo(pcrD);
    end;
end;


procedure Tfrm_MfPlayer.mnuSetPositionClick(Sender: TObject);
const
  iSec: integer = 1000000;  // 1000000 nanoseconds is 1 second
var
  sPos: string;
  sDur: string;
  iDef: int64;

begin
  // The calculated value is not accurate!
  sDur := IntToStr(int64(Trunc(MfPlayer.Duration / (iSec))));

  sPos := InputBox('Enter a position in seconds',
                   'Enter a value between 0 and ' + sDur + '.',
                   '1');

  iDef := StrToInt64Def(sPos, 0) * iSec;
  if (iDef >= MfPlayer.Duration) then
    iDef := 0; // Set back to start position

  MfPlayer.SetNewPosition := iDef; // set new StartPosition
  MfPlayer.SendPlayerRequest(reqSeek);
end;



procedure Tfrm_MfPlayer.trbVolumeChange(Sender: TObject);
begin
  MfPlayer.SetVolume(trbVolume.Position * 0.1);
end;


procedure Tfrm_MfPlayer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  DestroyMfPlayer();
  CanClose := True;
end;


procedure Tfrm_MfPlayer.FormCreate(Sender: TObject);
begin
  bFullScreenMode := False;
end;


procedure Tfrm_MfPlayer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  bm: TBitmap;

begin
  case Key of
    VK_SPACE:   if Assigned(MfPlayer) then
                  begin
                    case MfPlayer.State of
                      Started: MfPlayer.SendPlayerRequest(reqPause);
                      OpenPending: MfPlayer.SendPlayerRequest(reqStart);
                      Stopped, Paused: MfPlayer.SendPlayerRequest(reqStart);
                    end;
                  end;

    VK_END:     if Assigned(MfPlayer) then
                  begin
                    MfPlayer.SendPlayerRequest(reqStop);
                  end;

    VK_F11:     begin
                  if Assigned(MfPlayer) then
                  MfPlayer.ShutDown(); //DestroyMfPlayer;   //Kick ass...
                end;

    VK_F12:     muOpenClick(nil);

    //take a snapshot and copy the bitmap to the clipboard
    VK_F8:      begin
                  bm := TBitmap.Create;
                  MfPlayer.TakeSnapShot(bm);
                  Clipboard.Assign(bm);
                  FreeAndNil(bm);
                end;

    // Use left and right arrows to adjust the volume.
    // since there is a trackbar, this one will have the focus.
    VK_LEFT:    begin
                  MfPlayer.SetVolume(MfPlayer.Volume - 0.1);
                end;

    VK_RIGHT:   begin
                  MfPlayer.SetVolume(MfPlayer.Volume + 0.1);
                end;

    VK_ESCAPE:  begin
                  ButFullScreenClick(Self);
                end;
  end;
end;


function Tfrm_MfPlayer.GetFmPlayer(): HRESULT;
begin
  Result := E_FAIL;
  if not Assigned(MfPlayer) then
    begin
      MfPlayer := Nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      MfPlayer := TMfPlayer.Create(pnlVideo.Handle,
                                   stxtSubs.Handle,       // The clipping window / control
                                   frm_MfPlayer.Handle,   // The window or control that receives the (text) messages.
                                   frm_MfPlayer.Handle);  // Must be main form!
      // If you want a different clipping surface, for instance a TPanel
      //MfPlayer.SetVideoSurface := pnlVideo.Handle;
      Result := S_OK;
    end;
end;

end.
