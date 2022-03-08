// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: frmSimplePlayer.pas
// Kind: Pascal Unit
// Release date: 05-07-2020
// Language: ENU
//
// Revision Version: 3.1.1
// Description: Demonstrates audio/video playback using the IMFPMediaPlayer API.
//              Note: This API is deprecated!
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
//          The MFPlayer API is deprecated: See: https://docs.microsoft.com/en-us/windows/win32/api/mfplay/
//
//          To play a file, select **Open File** from the **File** menu.
//          To pause, press the **Spacebar**. To resume playback, press the **Spacebar** again.
//          To Stop, press Esc.
//          To change volume, press left arrow (decrease) or right arrow (increase).
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: It's not recommended to use this api.
//               As MS said this api is deprecated and support for most media types is not (fully) supported.
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
// =============================================================================
// Source: SimplePlay sample project
//
// Copyright (c) Microsoft Corporation. All rights reserved
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit frmSimplePlayer;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Win.ComObj,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  {DirectX}
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfPlay,
  WinApi.MediaFoundationApi.MfError;

type

////////////////////////////////////////////////////////////////////////////////
  TMediaPlayerCallback = class(TInterfacedPersistent, IMFPMediaPlayerCallback)
  private
    // IMFPMediaPlayerCallback methods
    procedure OnMediaPlayerEvent(var pEventHeader: MFP_EVENT_HEADER); stdcall;

  public
    constructor Create(); virtual;
    destructor Destroy(); override;

  end;

////////////////////////////////////////////////////////////////////////////////

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    Exit1: TMenuItem;
    dlgOpenFile: TOpenDialog;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenFile1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { Private declarations }
    AppHandle: HWND;

    function PlayMediaFile(const hApp: HWND; const sURL: LPCWSTR): HResult;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;

  public
    { Public declarations }

  end;


  procedure OnMediaItemCreated(pEvent: PMFP_MEDIAITEM_CREATED_EVENT);
  procedure OnMediaItemSet(pEvent: PMFP_MEDIAITEM_SET_EVENT);
  procedure ShowErrorMessage(fmt: string; hrErr: HResult);

var
  Form1: TForm1;

  // Global variables
  g_pPlayer: IMFPMediaPlayer;        // The MFPlay player object.
  g_pPlayerCB: TMediaPlayerCallback; // Application callback object.
  g_bHasVideo: BOOL;


implementation

{$R *.dfm}

// TMediaPlayerCallback class //////////////////////////////////////////////////

constructor TMediaPlayerCallback.Create();
begin
  inherited Create();
end;

destructor TMediaPlayerCallback.Destroy();
begin
  inherited Destroy();
end;

//-------------------------------------------------------------------
// OnMediaPlayerEvent
//
// Implements IMFPMediaPlayerCallback.OnMediaPlayerEvent.
// This callback method handles events from the MFPlay object.
//-------------------------------------------------------------------
procedure TMediaPlayerCallback.OnMediaPlayerEvent(var pEventHeader: MFP_EVENT_HEADER);
begin
  if Failed(pEventHeader.hrEvent) then
    begin
      ShowErrorMessage('Playback error', pEventHeader.hrEvent);
      Exit;
    end;

  case (pEventHeader.eEventType) of
    MFP_EVENT_TYPE_MEDIAITEM_CREATED:
      begin
        OnMediaItemCreated(MFP_GET_MEDIAITEM_CREATED_EVENT(@pEventHeader));
      end;

    MFP_EVENT_TYPE_MEDIAITEM_SET:
      begin
        OnMediaItemSet(MFP_GET_MEDIAITEM_SET_EVENT(@pEventHeader));
      end;
  end;
end;


// Form class //////////////////////////////////////////////////////////////////

procedure TForm1.WMSize(var Msg: TMessage);
var
  whdc: HDC;
  ps: PAINTSTRUCT;

begin
  Inherited;  // OnResize method will be handled first
  if (Msg.wParam = SIZE_RESTORED) then
    if Assigned(g_pPlayer) then
      begin
        whdc := BeginPaint(AppHandle, ps);
        // Resize the video.
        g_pPlayer.UpdateVideo();
        {void} EndPaint(whdc, ps);
      end;
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;

  if Assigned(g_pPlayer) then
    begin
      g_pPlayer.Stop();
      g_pPlayer.Shutdown();
      g_pPlayer := Nil;
    end;

   if Assigned(g_pPlayerCB) then
    begin
      FreeAndNil(g_pPlayerCB);
    end;
  
  CanClose := True;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  AppHandle := Handle;
  g_bHasVideo := False;
end;


procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  hr: HResult;
  state: MFP_MEDIAPLAYER_STATE;
  CurVolume: FLOAT;

begin
  hr := S_OK;
  state := MFP_MEDIAPLAYER_STATE_EMPTY;

  case Key of

    VK_SPACE: // Toggle between playback and paused/stopped.
      begin
        if Assigned(g_pPlayer) then
          begin
            hr := g_pPlayer.GetState(state);
            if Succeeded(hr) then
              begin
                if (state = MFP_MEDIAPLAYER_STATE_PAUSED) or (state = MFP_MEDIAPLAYER_STATE_STOPPED) then
                  hr := g_pPlayer.Play()
                else if (state = MFP_MEDIAPLAYER_STATE_PLAYING) then
                  hr := g_pPlayer.Pause();
              end;
          end;
      end;

    VK_ESCAPE: // Stop and shutdown
      begin
        if Assigned(g_pPlayer) then
          begin
            hr := g_pPlayer.GetState(state);
            if Succeeded(hr) then
              if (state <> MFP_MEDIAPLAYER_STATE_EMPTY) or (state <> MFP_MEDIAPLAYER_STATE_SHUTDOWN) then
                begin
                  hr := g_pPlayer.Stop();
                  // Repaint the screen
                  SendMessage(AppHandle, WM_SIZE, 0, 0);
                end;
          end;
      end;

    VK_RIGHT:
      begin
        if Assigned(g_pPlayer) then
          begin
            hr := g_pPlayer.GetState(state);
            if Succeeded(hr) then
              begin
                if (state <> MFP_MEDIAPLAYER_STATE_EMPTY) or (state <> MFP_MEDIAPLAYER_STATE_SHUTDOWN) then
                  if Succeeded(g_pPlayer.GetVolume(CurVolume)) then
                    begin
                      CurVolume := CurVolume + 0.1;
                      if CurVolume > 1 then
                        CurVolume := 1;
                      hr := g_pPlayer.SetVolume(CurVolume);
                    end;
              end;
          end;
      end;

    VK_LEFT:
      begin
        if Assigned(g_pPlayer) then
          begin
            hr := g_pPlayer.GetState(state);
            if Succeeded(hr) then
              begin
                if (state <> MFP_MEDIAPLAYER_STATE_EMPTY) or (state <> MFP_MEDIAPLAYER_STATE_SHUTDOWN) then
                  if Succeeded(g_pPlayer.GetVolume(CurVolume)) then
                    begin
                      CurVolume := CurVolume - 0.1;
                      if CurVolume < 0 then
                        CurVolume := 0;
                      hr := g_pPlayer.SetVolume(CurVolume);
                    end;
              end;
          end;
      end;
  end;

  if Failed(hr) then
    ShowErrorMessage('Playback Error', hr);

end;


procedure TForm1.FormPaint(Sender: TObject);
var
  ps: PAINTSTRUCT;
  whdc: HDC;

begin

  whdc := BeginPaint(AppHandle, ps);
  if (Assigned(g_pPlayer) and g_bHasVideo) then
    begin
      // Playback has started and there is video.
      // Do not draw the window background, because the video
      // frame fills the entire client area.
      g_pPlayer.UpdateVideo();
    end
  else
    begin
      // There is no video stream, or playback has not started.
      // Paint the entire client area.
      FillRect(whdc,
               ps.rcPaint,
               HBRUSH(COLOR_WINDOW + 1));
    end;

    EndPaint(whdc, ps);
end;


procedure TForm1.OpenFile1Click(Sender: TObject);
var
  hr: HResult;
  pwszFilePath: PWideChar;

begin
  hr := S_OK;
  if dlgOpenFile.Execute then
    begin
      pwszFilePath := PWideChar(dlgOpenFile.FileName);
      // Open the media file.
      hr := PlayMediaFile(AppHandle, pwszFilePath);
    end;

  if Failed(hr) then
    ShowErrorMessage('Could not open file.', hr);
end;


//-------------------------------------------------------------------
// PlayMediaFile
//
// Plays a media file, using the IMFPMediaPlayer interface.
//-------------------------------------------------------------------

function TForm1.PlayMediaFile(const hApp: HWND; const sURL: LPCWSTR): HResult;
var
  hr: HResult;

label
  done;

begin

  // Create the MFPlayer object.
  if not Assigned(g_pPlayer) then
    begin
      g_pPlayerCB := TMediaPlayerCallback.Create();

      if not Assigned(g_pPlayerCB) then
        begin
          hr := E_OUTOFMEMORY;
          goto done;
        end;

      hr := MFPCreateMediaPlayer(Nil,            // Mediafile path
                                 False,          // Start playback automatically?
                                 0,              // Flags
                                 g_pPlayerCB,    // Callback pointer
                                 hApp,           // Video window
                                 g_pPlayer       // The player
                                 );

      if Failed(hr) then
        goto done;
    end;

  // Create a new media item for this URL.
 hr := g_pPlayer.CreateMediaItemFromURL(sURL,
                                        False,
                                        0,
                                        Nil);

  // The CreateMediaItemFromURL method completes asynchronously.
  // The application will receive an MFP_EVENT_TYPE_MEDIAITEM_CREATED
  // event. See MediaPlayerCallback.OnMediaPlayerEvent().

done:

  Result := hr;

end;

//-------------------------------------------------------------------
// OnMediaItemCreated
//
// Called when the IMFPMediaPlayer.CreateMediaItemFromURL method
// completes.
//-------------------------------------------------------------------
procedure OnMediaItemCreated(pEvent: PMFP_MEDIAITEM_CREATED_EVENT);
var
  hr: HResult;
  bHasVideo,
  bIsSelected: BOOL;

label
  done;

begin
  hr := S_OK;
  // The media item was created successfully.

  if Assigned(g_pPlayer) then
    begin
      bHasVideo := False;
      bIsSelected := False;

      // Check if the media item contains video.
      hr := pEvent.pMediaItem.HasVideo(bHasVideo, bIsSelected);

      if Failed(hr) then goto done;

      g_bHasVideo := bHasVideo and bIsSelected;

      // Set the media item on the player. This method completes asynchronously.
      hr := g_pPlayer.SetMediaItem(pEvent.pMediaItem);
    end;

done:
  if Failed(hr) then
    ShowErrorMessage('Error playing this file.', hr);

end;


//-------------------------------------------------------------------
// OnMediaItemSet
//
// Called when the IMFPMediaPlayer.SetMediaItem method completes.
//-------------------------------------------------------------------
procedure OnMediaItemSet(pEvent: PMFP_MEDIAITEM_SET_EVENT);
var
  hr: HResult;

begin
  hr := g_pPlayer.Play();

  if Failed(hr) then
    ShowErrorMessage('IMFPMediaPlayer.Play failed.', hr);
end;


procedure ShowErrorMessage(fmt: string; hrErr: HResult);
var
  msg: string;

begin
  msg := Format('%s Resultcode: (%d)', [fmt, hrErr]);

  MessageBox(0,
             LPCWSTR(msg),
             LPCWSTR('Error'),
             MB_ICONERROR);
end;


// initialization and finalization /////////////////////////////////////////////

initialization
begin
  // Initialize Media Foundation platform
  if Succeeded(MFStartup(MF_VERSION)) then
    CoInitializeEx(Nil,
                   COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE)
  else
    Abort();
end;


finalization
begin
  // Shutdown MF
  MFShutdown();
  CoUninitialize();
end;

end.
