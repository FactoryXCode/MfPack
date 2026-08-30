// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.Main.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Real-time, timestamp-paced playback through the grayscale MFT.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// Source: Microsoft Learn.
//
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
unit Form.Main;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  {Appliccation}
  AsyncVideoEngine;

type
  TfrmMain = class(TForm)
    pnlCommands: TPanel;
    btnOpen: TButton;
    btnPlayPause: TButton;
    btnStop: TButton;
    pbPosition: TProgressBar;
    lblTime: TLabel;
    pnlSource: TPanel;
    lblSource: TLabel;
    imgSource: TImage;
    pnlOutput: TPanel;
    lblOutput: TLabel;
    imgOutput: TImage;
    memLog: TMemo;
    dlgOpenVideo: TOpenDialog;
    tmrFrame: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrFrameTimer(Sender: TObject);

  private

    FEngine: IAsyncVideoEngine;
    FSession: Cardinal;
    FDuration: LONGLONG;
    FPlaying: Boolean;
    FStarted: Boolean;
    FFirstSampleTime: LONGLONG;
    FLastDisplayedTime: LONGLONG;
    FPlaybackStartTick: UInt64;
    FPendingPacket: TVideoFramePacket;
    FSourceBitmap: TBitmap;
    FOutputBitmap: TBitmap;

    procedure WMFrameReady(var Message: TMessage); message WM_ASYNC_FRAME_READY;
    procedure WMEndOfStream(var Message: TMessage); message WM_ASYNC_END_OF_STREAM;
    procedure WMPlaybackError(var Message: TMessage); message WM_ASYNC_PLAYBACK_ERROR;

    procedure CopyPackedToBitmap(const APixels: TBytes;
                                 const AWidth: Integer;
                                 const AHeight: Integer;
                                 const ABitmap: TBitmap);

    procedure DisplayPendingFrame();
    procedure FreePendingFrame();
    procedure OpenVideo(const AFileName: string);
    procedure SchedulePendingFrame();
    procedure SetPlaying(const AValue: Boolean);
    procedure StopVideo();
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

const
  BYTES_PER_PIXEL = 4;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  // TImage is a graphic control, so its parent paints it. Without buffering,
  //  the parent panel can erase its background between consecutive video
  //  frames and briefly expose clBtnFace.
  DoubleBuffered := True;
  pnlSource.DoubleBuffered := True;
  pnlOutput.DoubleBuffered := True;

  FSourceBitmap := TBitmap.Create;
  FOutputBitmap := TBitmap.Create;
  FSourceBitmap.PixelFormat := pf32bit;
  FOutputBitmap.PixelFormat := pf32bit;
  FFirstSampleTime := -1;
  FLastDisplayedTime := -1;
  pbPosition.Min := 0;
  pbPosition.Max := 1000;
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;

  dlgOpenVideo.Filter := 'Video files|*.mp4;*.m4v;*.mov;*.wmv;*.avi|All files|*.*';
  memLog.Lines.Add('Open a video, then click Play.');

  FormResize(nil);
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin

  //pnlOutput is alClient; keeping pnlSource at half the client width gives
  //  both videos the same display area at every window size.
  pnlSource.Width := ClientWidth div 2;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Msg: TMsg;

begin

  tmrFrame.Enabled := False;
  FreePendingFrame();

  if Assigned(FEngine) then
    FEngine.Close(True);

  FEngine := nil;

  // Close(True) detaches the callback. Remove packets already posted before
  // the detach so their ownership cannot be lost while the form is destroyed.
  while PeekMessage(Msg, Handle, WM_ASYNC_FRAME_READY,
                    WM_ASYNC_PLAYBACK_ERROR, PM_REMOVE) do
    case Msg.message of
      WM_ASYNC_FRAME_READY:
        TVideoFramePacket(Pointer(Msg.lParam)).Free;
      WM_ASYNC_PLAYBACK_ERROR:
        TPlaybackNotice(Pointer(Msg.lParam)).Free;
    end;

  FOutputBitmap.Free;
  FSourceBitmap.Free;
end;


procedure TfrmMain.FreePendingFrame;
begin

  FreeAndNil(FPendingPacket);
end;


procedure TfrmMain.CopyPackedToBitmap(const APixels: TBytes;
                                      const AWidth: Integer;
                                      const AHeight: Integer;
                                      const ABitmap: TBitmap);
var
  RowBytes: Integer;
  Y: Integer;

begin

  RowBytes := AWidth * BYTES_PER_PIXEL;

  if (Length(APixels) <> RowBytes * AHeight) then
    raise Exception.Create('Packed frame size does not match its dimensions.');

  ABitmap.SetSize(AWidth,
                  AHeight);

  for Y := 0 to AHeight - 1 do
    Move(APixels[Y * RowBytes],
         ABitmap.ScanLine[AHeight - 1 - Y]^,
         RowBytes);
end;


procedure TfrmMain.OpenVideo(const AFileName: string);
begin

  StopVideo();

  FEngine := CreateAsyncVideoEngine(Handle, FSession);
  FDuration := FEngine.Open(AFileName);

  if (FDuration <= 0) then
    raise Exception.Create('The video duration is zero.');

  FFirstSampleTime := -1;
  FLastDisplayedTime := -1;
  FStarted := False;
  pbPosition.Position := 0;
  lblTime.Caption := Format('0.000 / %.3f s',
                            [FDuration / 10000000.0]);

  btnPlayPause.Enabled := True;
  btnStop.Enabled := True;

  memLog.Clear();
  memLog.Lines.Add(ExtractFileName(AFileName));
  memLog.Lines.Add(Format('Duration: %.3f seconds.',
                          [FDuration / 10000000.0]));
  memLog.Lines.Add('Ready. Exactly one asynchronous read is issued at a time.');
end;


procedure TfrmMain.StopVideo();
begin

  tmrFrame.Enabled := False;
  FPlaying := False;
  btnPlayPause.Caption := 'Play';
  FreePendingFrame();

  if Assigned(FEngine) then
    FEngine.Close(False);

  FEngine := nil;

  // Any callback that was already being delivered belongs to the old session.
  Inc(FSession);
  FStarted := False;
  btnPlayPause.Enabled := False;
  btnStop.Enabled := False;
end;


procedure TfrmMain.SetPlaying(const AValue: Boolean);
begin

  if not Assigned(FEngine) then
    Exit;

  FPlaying := AValue;

  if FPlaying then
    begin
      btnPlayPause.Caption := 'Pause';

      if Assigned(FPendingPacket) then
        FFirstSampleTime := FPendingPacket.TimeStamp
      else
        FFirstSampleTime := FLastDisplayedTime;

      FPlaybackStartTick := GetTickCount;

      if Assigned(FPendingPacket) then
        DisplayPendingFrame
      else
        if not FStarted then
          begin
            FStarted := True;
            FEngine.Start;
          end
        else
          FEngine.RequestNext;
    end
  else
    begin
      btnPlayPause.Caption := 'Play';
      tmrFrame.Enabled := False;
      // No new read is requested after the outstanding callback arrives.
      // Its packet remains pending until playback resumes.
    end;
end;


procedure TfrmMain.SchedulePendingFrame();
var
  DueMs: Int64;
  ElapsedMs: UInt64;
  DelayMs: Int64;

begin

  if not FPlaying or not Assigned(FPendingPacket) then
    Exit;

  if (FFirstSampleTime < 0) then
    begin
      FFirstSampleTime := FPendingPacket.TimeStamp;
      FPlaybackStartTick := GetTickCount;
    end;

  DueMs := (FPendingPacket.TimeStamp - FFirstSampleTime) div 10000;
  ElapsedMs := Cardinal(GetTickCount - Cardinal(FPlaybackStartTick));
  DelayMs := DueMs - Int64(ElapsedMs);

  if (DelayMs <= 1) then
    DisplayPendingFrame()
  else
    begin
      if (DelayMs > 60000) then
        DelayMs := 60000;
      tmrFrame.Interval := Integer(DelayMs);
      tmrFrame.Enabled := True;
    end;
end;


procedure TfrmMain.DisplayPendingFrame;
var
  Packet: TVideoFramePacket;

begin

  if not Assigned(FPendingPacket) then
    Exit;

  tmrFrame.Enabled := False;
  Packet := FPendingPacket;
  FPendingPacket := nil;

  try
    CopyPackedToBitmap(Packet.ColorPixels,
                       Packet.Width,
                       Packet.Height,
                       FSourceBitmap);

    CopyPackedToBitmap(Packet.GrayPixels,
                       Packet.Width,
                       Packet.Height,
                       FOutputBitmap);

    imgSource.Picture.Assign(FSourceBitmap);
    imgOutput.Picture.Assign(FOutputBitmap);
    FLastDisplayedTime := Packet.TimeStamp;

    if (FDuration > 0) then
      pbPosition.Position := Integer((Packet.TimeStamp * pbPosition.Max) div FDuration);

    lblTime.Caption := Format('%.3f / %.3f s',
                              [Packet.TimeStamp / 10000000.0, FDuration / 10000000.0]);

  finally
    Packet.Free;
  end;

  if FPlaying and Assigned(FEngine) then
    FEngine.RequestNext;
end;


procedure TfrmMain.WMFrameReady(var message: TMessage);
var
  Packet: TVideoFramePacket;

begin

  Packet := TVideoFramePacket(Pointer(message.LParam));

  if Cardinal(Message.WParam) <> FSession then
    begin
      Packet.Free;
      Exit;
    end;

  if not Assigned(Packet) then
    begin
      if FPlaying and Assigned(FEngine) then
        FEngine.RequestNext;
      Exit;
    end;

  FreePendingFrame;
  FPendingPacket := Packet;
  SchedulePendingFrame;
end;


procedure TfrmMain.WMEndOfStream(var Message: TMessage);
begin

  if (Cardinal(Message.WParam) <> FSession) then
    Exit;

  SetPlaying(False);
  memLog.Lines.Add('End of stream.');
  lblTime.Caption := 'End of video.';
end;


procedure TfrmMain.WMPlaybackError(var Message: TMessage);
var
  Notice: TPlaybackNotice;

begin

  Notice := TPlaybackNotice(Pointer(Message.LParam));

  try
    if Assigned(Notice) and (Notice.Session = FSession) then
      begin
        SetPlaying(False);
        memLog.Lines.Add(Notice.Text);
      end;

  finally
    Notice.Free;
  end;
end;


procedure TfrmMain.btnOpenClick(Sender: TObject);
begin

  if not dlgOpenVideo.Execute then
    Exit;

  try
    OpenVideo(dlgOpenVideo.FileName);
  except
    on E: Exception do
      begin
        StopVideo;
        memLog.Lines.Add(E.Message);
      end;
  end;
end;


procedure TfrmMain.btnPlayPauseClick(Sender: TObject);
begin

  SetPlaying(not FPlaying);
end;


procedure TfrmMain.btnStopClick(Sender: TObject);
begin

  StopVideo();
  memLog.Lines.Add('Stopped. Open a video to start again.');
end;

procedure TfrmMain.tmrFrameTimer(Sender: TObject);
begin

  DisplayPendingFrame();
end;

end.
