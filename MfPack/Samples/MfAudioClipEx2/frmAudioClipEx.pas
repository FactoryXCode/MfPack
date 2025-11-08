// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  frmAudioClipEx.pas
// Kind: Pascal Unit
// Release date: 21-12-2019
// Language: ENU
//
// Revision Version: 3.1.8
//
// Description:
//   This application demonstrates using the Media Foundation
//   source reader to extract decoded audio from an audio/video file.
//
//   The application reads audio data from an input file and writes
//   uncompressed PCM audio to a WAVE file.
//
//   The input file must be a media format supported by Media Foundation,
//   and must have  an audio stream. The audio stream can be an encoded
//   format, such as Windows Media Audio.
//   Note: The original application was a console app. running in synchrone mode.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 06/11/2025 All                 Ozzy Osbourne release  SDK 10.0.26100.4654 (Windows 11)
// 07/11/2025 Tony                Addded support for WMA and FLAC.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: >= MfPackX318
// Known Issues: -
//
// Compiler version: 28 up to 36
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: Parts of the AudioClip sample
//         https://docs.microsoft.com/en-us/windows/win32/medfound/tutorial--decoding-audio
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
unit frmAudioClipEx;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Project}
  AudioClipEngine;

type
  TAudioClipExfrm = class(TForm)
    butStart: TButton;
    butCancel: TButton;
    pbProgress: TProgressBar;
    lblStatus: TLabel;
    lblGetSourceFile: TLabel;
    lblSourceFile: TLabel;
    lblTargetFile: TLabel;
    lblSetTartgetFile: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lblTime: TLabel;
    tbPriority: TTrackBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    
    procedure butStartClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure lblGetSourceFileClick(Sender: TObject);
    procedure lblSetTartgetFileClick(Sender: TObject);
    procedure tbPriorityChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);

  private
    FEngine: TAudioClipClass;
    FCancelEvent: TEvent;
    FThrottle: Integer;
    FDuration: LONGLONG;

    procedure UpdateUIBusy(ABusy: Boolean);

    procedure WorkerComplete(Sender: TObject;
                             Success: Boolean;
                             HResultCode: HResult);
    procedure Reset();

  end;

var
  AudioClipExfrm: TAudioClipExfrm;


implementation

{$R *.dfm}

uses
  System.Threading;

procedure TAudioClipExfrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;

  if Assigned(FEngine) then
    FreeAndNil(FEngine);

  if Assigned(FCancelEvent) then
    FreeAndNil(FCancelEvent);

  CanClose := True;
end;


procedure TAudioClipExfrm.FormCreate(Sender: TObject);
begin

  FCancelEvent := TEvent.Create(nil,
                                True,
                                False,
                                '');
  Reset();
end;


procedure TAudioClipExfrm.FormDestroy(Sender: TObject);
begin
  FCancelEvent.Free;
  FEngine := nil;
end;


procedure TAudioClipExfrm.lblGetSourceFileClick(Sender: TObject);
var
  dlg: TOpenDialog;

begin

  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Video/Audio files|*.mp4;*.avi;*.mkv;*.mov;*.mp3;*.wav;*.wma;*.flac;|All files|*.*';
    if dlg.Execute then
      lblSourceFile.Caption := dlg.FileName;
  finally
    dlg.Free;
  end;
end;


procedure TAudioClipExfrm.lblSetTartgetFileClick(Sender: TObject);
var
  dlg: TSaveDialog;

begin

  dlg := TSaveDialog.Create(Self);
  try
    dlg.Filter := 'WAV files|*.wav';
    if dlg.Execute then
      lblTargetFile.Caption := dlg.FileName;

    if (dlg.FileName = '') then
      lblTargetFile.Caption := ChangeFileExt(lblSourceFile.Caption, '.wav');

  finally
    dlg.Free;
  end;
end;


procedure TAudioClipExfrm.tbPriorityChange(Sender: TObject);
begin
  FThrottle := tbPriority.Position;
  if Assigned(FEngine) then
    FEngine.SamplingPriority := FThrottle;
end;


procedure TAudioClipExfrm.Timer1Timer(Sender: TObject);
begin
  if Assigned(FEngine) then
    begin
      pbProgress.Position := FEngine.ProgressPercent;
      lblStatus.Caption := Format('Processing... %d%% (%d KB)', [FEngine.ProgressPercent, FEngine.ProgressBytes div 1024]);
    end;
end;


procedure TAudioClipExfrm.UpdateUIBusy(ABusy: Boolean);
begin
  butStart.Enabled := not ABusy;
  butCancel.Enabled := ABusy;
  lblGetSourceFile.Enabled := not ABusy;
  lblSetTartgetFile.Enabled := not ABusy;
end;


procedure TAudioClipExfrm.butStartClick(Sender: TObject);
var
  hr: HResult;

begin
  if (Trim(lblSourceFile.Caption) = '') or (Trim(lblTargetFile.Caption) = '') then
    begin
      MessageDlg('Please select a valid source and target file.', mtWarning, [mbOK], 0);
      Exit;
    end;

  pbProgress.Position := 0;
  lblStatus.Caption := 'Starting extraction...';
  UpdateUIBusy(True);

  FCancelEvent.ResetEvent;

  if Assigned(FEngine) then
    FEngine := nil;

  FEngine := TAudioClipClass.Create();

  FEngine.SourceFile := lblSourceFile.Caption;
  FEngine.OutputFile := lblTargetFile.Caption;

  // Run in background thread to avoid blocking UI
  TTask.Run(procedure
            var
              hr: HResult;

            begin
              FEngine.SamplingPriority := FThrottle;
              hr := FEngine.ExtractSoundClip(FCancelEvent.Handle,
                                             WorkerComplete);


              if Failed(hr) then
                TThread.Queue(nil,
                              TThreadProcedure(procedure
                                               begin
                                                 lblStatus.Caption := Format('Failed to start extraction (0x%x)', [hr]);
                                                 UpdateUIBusy(False);
                                               end));

    end);

    // Get the size of the sourcefile.
  hr := GetFileDuration(PCWSTR(FEngine.SourceFile),
                        FDuration);
  if SUCCEEDED(hr) then
    lblTime.Caption := MSecToStr(FDuration div 10000,
                                 False)
  else
    lblTime.Caption := '00:00:00';
end;


procedure TAudioClipExfrm.butCancelClick(Sender: TObject);
begin
  if Assigned(FCancelEvent) then
    begin
      FCancelEvent.SetEvent;
      lblStatus.Caption := 'Cancelling...';
    end;
end;


procedure TAudioClipExfrm.WorkerComplete(Sender: TObject;
                                         Success: Boolean;
                                         HResultCode: HResult);
begin
  TThread.Queue(nil,
                TThreadProcedure(procedure
                                 begin
                                   if Success then
                                     lblStatus.Caption := 'Extraction complete!'
                                   else
                                     lblStatus.Caption := Format('Extraction aborted (0x%x)', [HResultCode]);
                                   UpdateUIBusy(False);
                                   FreeAndNil(FEngine);
                                 end));
end;


procedure TAudioClipExfrm.Reset();
begin
  lblTime.Caption := '00:00:00';
  lblStatus.Caption := 'Ready.';
  pbProgress.Position := 0;
  UpdateUIBusy(False);
end;

end.
