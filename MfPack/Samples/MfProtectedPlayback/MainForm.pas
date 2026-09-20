
// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MainForm.pas
// Kind: Pascal Unit
// Release date: 19-09-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: VCL main window for protected Media Foundation playback, video display, and diagnostic logging.
//
// Company: FactoryX
// Intiator(s): Christian Hackbart (TetrisSQC), Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
// 19/09/2026 Tony                Delphi translation of ProtectedPlayback.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Parts of Microsoft ProtectedPlayback and CPlayer examples
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

unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  Player,
  ContentEnabler,
  SampleLog;

type
  TProtectedPlaybackForm = class(TForm)
    pnlVideo: TPanel;
    FLogMemo: TMemo;
    FOpenDialog: TOpenDialog;
    FOpenFileItem: TMenuItem;
    FOpenURLItem: TMenuItem;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    MenuSeparator: TMenuItem;
    ExitMenuItem: TMenuItem;
    pnlControls: TPanel;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure OpenURLClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure VideoResize(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure PauseClick(Sender: TObject);
    procedure StopClick(Sender: TObject);

  private
    FPlayer: IProtectedPlayer;
    FRepaintClient: Boolean;
    FOriginalVideoPanelWindowProc: TWndMethod;

    procedure PlayerEvent(var Message: TMessage); message WM_APP_PLAYER_EVENT;
    procedure ContentEnablerMessage(var Message: TMessage); message WM_APP_CONTENT_ENABLER;
    procedure BrowserDoneMessage(var Message: TMessage); message WM_APP_BROWSER_DONE;
    procedure LogMessage(var Message: TMessage); message WM_APP_LOG;
    procedure OnContentEnablerMessage();
    procedure OnWebBrowserClosed();
    procedure VideoPanelWindowProc(var Message: TMessage);
    procedure AddLog(const Text: string);
    procedure DiscardPendingLogMessages;
    procedure NotifyError(const Text: string;
                          ErrorCode: HRESULT);
    procedure UpdateUI(State: TPlayerState);
  end;

var
  ProtectedPlaybackForm: TProtectedPlaybackForm;


implementation

{$R *.dfm}

uses

  {System}
  System.SysUtils,
  System.UITypes;

procedure TProtectedPlaybackForm.FormCreate(Sender: TObject);
var
  hr: HResult;

begin

  FRepaintClient := True;
  FOriginalVideoPanelWindowProc := pnlVideo.WindowProc;
  pnlVideo.WindowProc := VideoPanelWindowProc;

  HandleNeeded();
  pnlVideo.HandleNeeded;
  AddLog('Starting MF Protected Playback.');

  hr := TPlayer.CreateInstance(pnlVideo.Handle,
                               Handle,
                               FPlayer);
  if Failed(hr) then
    raise Exception.CreateFmt('Could not initialize the player ($%.8x).',
      [Cardinal(hr)]);

  UpdateUI(Ready);
  AddLog('Player initialized. Open a media file to begin.');
end;


procedure TProtectedPlaybackForm.FormDestroy(Sender: TObject);
begin

  AddLog('Shutting down.');

  if Assigned(FPlayer) then
    FPlayer.Shutdown;

  FPlayer := nil;

  if Assigned(FOriginalVideoPanelWindowProc) then
    pnlVideo.WindowProc := FOriginalVideoPanelWindowProc;
  DiscardPendingLogMessages;
end;

procedure TProtectedPlaybackForm.OpenFileClick(Sender: TObject);
var
  hr: HResult;

begin

  if not FOpenDialog.Execute then
    Exit;

  AddLog('Open file: ' + FOpenDialog.FileName);

  hr := FPlayer.OpenURL(PWideChar(FOpenDialog.FileName));
  if Succeeded(hr) then
    UpdateUI(OpenPending)
  else
    begin
      NotifyError('Could not open the file.', HR);
      UpdateUI(Ready);
    end;
end;


procedure TProtectedPlaybackForm.OpenURLClick(Sender: TObject);
var
  hr: HResult;
  URL: string;

begin

  URL := '';
  if not InputQuery('Open URL',
                    'Enter the URL to open:',
                    URL) then
    Exit;

  AddLog('Open URL: ' + URL);

  hr := FPlayer.OpenURL(PWideChar(URL));
  if Succeeded(hr) then
    UpdateUI(OpenPending)
  else
    begin
      NotifyError('Could not open this URL.', HR);
      UpdateUI(Ready);
    end;
end;


procedure TProtectedPlaybackForm.ExitClick(Sender: TObject);
begin

  Close();
end;


procedure TProtectedPlaybackForm.VideoPanelWindowProc(var Message: TMessage);
begin

  FOriginalVideoPanelWindowProc(Message);

  if (Message.Msg = WM_PAINT) and not FRepaintClient and Assigned(FPlayer) then
    FPlayer.Repaint;
end;


procedure TProtectedPlaybackForm.VideoResize(Sender: TObject);
begin

  if Assigned(FPlayer) and pnlVideo.HandleAllocated then
    FPlayer.ResizeVideo(pnlVideo.ClientWidth,
                        pnlVideo.ClientHeight);
end;


procedure TProtectedPlaybackForm.PlayClick(Sender: TObject);
var
  hr: HRESULT;

begin

  AddLog('Play requested.');
  hr := FPlayer.Play();
  if Failed(hr) then
    NotifyError('Could not start playback.',
                hr);
  UpdateUI(FPlayer.GetState());
end;


procedure TProtectedPlaybackForm.PauseClick(Sender: TObject);
var
  hr: HRESULT;

begin

  AddLog('Pause requested.');
  hr := FPlayer.Pause();
  if Failed(hr) then
    NotifyError('Could not pause playback.',
                hr);
  UpdateUI(FPlayer.GetState());
end;


procedure TProtectedPlaybackForm.StopClick(Sender: TObject);
var
  hr: HRESULT;

begin

  AddLog('Stop requested.');
  hr := FPlayer.Stop();
  if Failed(hr) then
    NotifyError('Could not stop playback.',
                hr);
  UpdateUI(FPlayer.GetState());
end;


procedure TProtectedPlaybackForm.FormKeyPress(Sender: TObject;
                                              var Key: Char);
begin

  if (Key = ' ') and Assigned(FPlayer) then
    begin
      if (FPlayer.GetState = Started) then
        PauseClick(Sender)
      else if (FPlayer.GetState in [Paused, Stopped]) then
        PlayClick(Sender);

      Key := #0;
    end;
end;


procedure TProtectedPlaybackForm.PlayerEvent(var Message: TMessage);
var
  hr: HResult;

begin

  hr := FPlayer.HandleEvent(UINT_PTR(Message.WParam));
  if Failed(HR) then
    NotifyError('An error occurred.',
                hr);
  UpdateUI(FPlayer.GetState);
end;


procedure TProtectedPlaybackForm.ContentEnablerMessage(var Message: TMessage);
begin

  AddLog('Content-enabler notification received.');
  OnContentEnablerMessage();
end;


procedure TProtectedPlaybackForm.BrowserDoneMessage(var Message: TMessage);
begin

  AddLog('Browser closed notification received.');
  OnWebBrowserClosed();
end;


procedure TProtectedPlaybackForm.LogMessage(var Message: TMessage);
var
  MessageText: PString;

begin

  MessageText := PString(Message.LParam);
  if (MessageText = nil) then
    Exit;
  try
    AddLog(MessageText^);

  finally
    Dispose(MessageText);
  end;
end;


procedure TProtectedPlaybackForm.AddLog(const Text: string);
begin

  if not Assigned(FLogMemo) then
    Exit;

  FLogMemo.Lines.Add(FormatDateTime('hh:nn:ss.zzz',
                                    Now) + '  ' + Text);
  FLogMemo.SelStart := Length(FLogMemo.Text);
  FLogMemo.Perform(EM_SCROLLCARET,
                   0,
                   0);
end;


procedure TProtectedPlaybackForm.DiscardPendingLogMessages;
var
  PendingMessage: TMsg;
  MessageText: PString;

begin

  if not HandleAllocated then
    Exit;

  while PeekMessage(PendingMessage,
                    Handle,
                    WM_APP_LOG,
                    WM_APP_LOG,
                    PM_REMOVE) do
  begin
    MessageText := PString(PendingMessage.lParam);

    if (MessageText <> nil) then
      Dispose(MessageText);
  end;
end;


procedure TProtectedPlaybackForm.OnContentEnablerMessage;
var
  hr: HResult;
  Manager: IContentProtectionManagerApp;

begin

  hr := FPlayer.GetContentProtectionManager(Manager);
  if Failed(hr) then
    begin
      AddLog('Content protection manager unavailable: ' + FormatHR(HR));
      Exit;
    end;

  case Manager.GetState of
    Enabler_Ready:
      begin
        AddLog('Content enabler ready; starting enable operation.');
        HR := Manager.DoEnable;
      end;

    Enabler_SilentInProgress:
      begin
        if (Manager.GetStatus = NS_E_DRM_LICENSE_NOTACQUIRED) then
          begin
            AddLog('Silent acquisition did not obtain a license; trying non-silent.');
            hr := Manager.DoEnable(ForceNonSilent)
          end
        else
          begin
            AddLog('Silent enable completed: ' + FormatHR(Manager.GetStatus));
            hr := Manager.CompleteEnable;
          end;
      end;

    Enabler_NonSilentInProgress:
      begin
        AddLog('Non-silent enable completed: ' + FormatHR(Manager.GetStatus));
        hr := Manager.CompleteEnable;
      end;
    Enabler_Complete:
      hr := S_OK;
  else
    hr := E_UNEXPECTED;
  end;

  if Failed(hr) then
    begin
      AddLog('Content-enabler action failed: ' + FormatHR(hr));
      Manager.CompleteEnable();
    end;
end;


procedure TProtectedPlaybackForm.OnWebBrowserClosed;
var
  Manager: IContentProtectionManagerApp;

begin

  if Succeeded(FPlayer.GetContentProtectionManager(Manager)) and (Manager.GetState <> Enabler_Complete) then
    begin
      AddLog('Cancelling content enable after browser close.');
      Manager.CancelEnable();
    end;
end;


procedure TProtectedPlaybackForm.UpdateUI(State: TPlayerState);
var
  Waiting: Boolean;
  Playback: Boolean;

begin

  Waiting := (State = OpenPending);
  Playback := State in [Started, Paused];
  FOpenFileItem.Enabled := not Waiting;
  FOpenURLItem.Enabled := not Waiting;
  btnPlay.Enabled := State in [Paused, Stopped];
  btnPause.Enabled := State = Started;
  btnStop.Enabled := State in [Started, Paused];

  if Waiting then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;

  FRepaintClient := not (Playback and FPlayer.HasVideo);
  pnlVideo.Invalidate;
end;


procedure TProtectedPlaybackForm.NotifyError(const Text: string;
  ErrorCode: HRESULT);

begin

  AddLog(Text + ' ' + FormatHR(ErrorCode));
  MessageDlg(Format('%s (HRESULT = $%.8x)',
                    [Text, Cardinal(ErrorCode)]),
             mtError,
             [mbOK],
             0);
end;

end.
