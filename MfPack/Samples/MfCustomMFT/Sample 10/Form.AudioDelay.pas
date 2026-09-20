// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.AudioDelay.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: VCL form for choosing an audio file and processing it
//              with delay and wet-mix controls.
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
unit Form.AudioDelay;

interface

uses
  {System}
  System.Classes,
  System.SysUtils,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.StdCtrls;

type
  TfrmAudioDelay = class(TForm)
    lblInput: TLabel;
    edtInput: TEdit;
    btnBrowse: TButton;
    lblDelay: TLabel;
    edtDelay: TEdit;
    lblMix: TLabel;
    edtMix: TEdit;
    btnProcess: TButton;
    btnPlay: TButton;
    memLog: TMemo;
    dlgOpenAudio: TOpenDialog;
    dlgSaveWave: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
  private

    FOutputFile: string;
  end;

var
  frmAudioDelay: TfrmAudioDelay;


implementation

uses
  {WinApi}
  WinApi.WinMM.PlaySoundApi,
  {Application}
  AudioDelayFileEngine;

{$R *.dfm}


procedure TfrmAudioDelay.FormCreate(Sender: TObject);
begin

  dlgOpenAudio.Filter := 'Audio files|*.wav;*.mp3;*.wma;*.m4a;*.aac;*.flac|All files|*.*';
  dlgSaveWave.Filter := 'PCM WAV file|*.wav';
  dlgSaveWave.DefaultExt := 'wav';
  edtDelay.Text := '250';
  edtMix.Text := '50';
  btnPlay.Enabled := False;
  memLog.Lines.Add('Choose an audio file, then create a delayed WAV file.');
end;


procedure TfrmAudioDelay.btnBrowseClick(Sender: TObject);
begin

  if dlgOpenAudio.Execute then
    edtInput.Text := dlgOpenAudio.FileName;
end;


procedure TfrmAudioDelay.btnProcessClick(Sender: TObject);
var
  DelayMs, WetPercent: Integer;
  InputFrames: UInt64;
  TailFrames: UInt64;
  FormatDescription: string;

begin

  if not FileExists(edtInput.Text) then
    begin
      MessageDlg('Choose an existing audio file first.',
                 mtInformation,
                 [mbOK],
                 0);
      Exit;
    end;

  if not TryStrToInt(edtDelay.Text,
                     DelayMs) or (DelayMs < 1) or (DelayMs > 10000) then
    begin
      MessageDlg('Delay must be from 1 to 10000 milliseconds.',
                 mtInformation,
                 [mbOK],
                 0);
      Exit;
    end;

  if not TryStrToInt(edtMix.Text,
                     WetPercent) or (WetPercent < 0) or (WetPercent > 100) then
    begin
      MessageDlg('Wet mix must be from 0 to 100 percent.',
                 mtInformation,
                 [mbOK],
                 0);
      Exit;
    end;

  dlgSaveWave.FileName := ChangeFileExt(edtInput.Text,
                                        '_delayed.wav');

  if not dlgSaveWave.Execute then
    Exit;

  btnBrowse.Enabled := False;
  btnProcess.Enabled := False;
  btnPlay.Enabled := False;
  Screen.Cursor := crHourGlass;

  memLog.Lines.Add('Decoding and processing: ' + edtInput.Text);

  try
    try
      ProcessAudioFile(edtInput.Text,
                       dlgSaveWave.FileName,
                       Cardinal(DelayMs),
                       Cardinal(WetPercent),
                       InputFrames,
                       TailFrames,
                       FormatDescription);

      FOutputFile := dlgSaveWave.FileName;
      memLog.Lines.Add('Decoded format: ' + FormatDescription);

      memLog.Lines.Add(Format('Input frames: %d; tail frames: %d.',
                              [Int64(InputFrames), Int64(TailFrames)]));

      memLog.Lines.Add('Saved: ' + FOutputFile);
      btnPlay.Enabled := True;

    except
      on E: Exception do
        begin
          memLog.Lines.Add('Error: ' + E.Message);
          MessageDlg(E.Message,
                     mtError,
                     [mbOK],
                     0);
        end;
    end;
  finally
    Screen.Cursor := crDefault;
    btnBrowse.Enabled := True;
    btnProcess.Enabled := True;
  end;
end;


procedure TfrmAudioDelay.btnPlayClick(Sender: TObject);
begin

  if not PlaySound(PWideChar(FOutputFile),
                   0,
                   SND_FILENAME or SND_ASYNC or SND_NODEFAULT) then
    MessageDlg('Windows could not play the WAV file.',
               mtError,
               [mbOK],
               0);
end;

end.
