// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfTranscode.pas
// Kind: Pascal Unit
// Release date: 24-01-2020
// Language: ENU
//
// Revision Version: 3.1.4
// Description: This is a modified translation of the Transcoder sample,
//              The original is a commandline app.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Transcoding Example
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit frmTranscode;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.DateUtils,
  System.Variants,
  System.Classes,
  System.Diagnostics,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {Project}
  Transcoder;

type
  TfrmTranscoder = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuOpenSourceFile: TMenuItem;
    mnuExit: TMenuItem;
    lblSourceFile: TLabel;
    Label2: TLabel;
    edSourceFile: TEdit;
    edTargetFile: TEdit;
    Panel1: TPanel;
    butExecute: TButton;
    butStop: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    mnuTargetfile: TMenuItem;
    ProgressBar: TProgressBar;
    lblProgress: TLabel;
    lblEstFinish: TLabel;
    N1: TMenuItem;
    procedure mnuOpenSourceFileClick(Sender: TObject);
    procedure mnuTargetfileClick(Sender: TObject);
    procedure butExecuteClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    sInputFile: PWideChar;   // Audio source file name
    sOutputFile: PWideChar;  // Output file name
    transcoder: TTranscoder; // transcoder object
    stopwatch: TStopWatch;
    ElapsedSet: Boolean;

    procedure Start();
    procedure Reset(const hr: HResult);
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
  public
    { Public declarations }

  end;

var
  frmTranscoder: TfrmTranscoder;

implementation

{$R *.dfm}

procedure TfrmTranscoder.mnuTargetfileClick(Sender: TObject);
var
  tmp: string;

begin
  tmp := ExtractFileName(string(sInputFile));
  Savedialog1.FileName := ChangeFileExt(tmp, '.wmv');

  if Savedialog1.execute then
    begin
      // If choosen the source as target, change it to wav
      if sInputFile = Savedialog1.FileName then
        Savedialog1.FileName := ChangeFileExt(sInputFile, '.wmv');
      // delete existing target
      if FileExists(Savedialog1.FileName) then
        DeleteFile(Savedialog1.FileName);

      edTargetFile.Text := Savedialog1.FileName;
      sOutputFile := PWideChar(Savedialog1.FileName);
      butExecute.Enabled := True;
    end;
end;

// The "main" method
procedure TfrmTranscoder.butExecuteClick(Sender: TObject);
begin
  Start();
end;


procedure TfrmTranscoder.Start();
var
  hr: HResult;

begin
  // Create the transcoder object
  transcoder := TTranscoder.Create(Self.Handle);
  // Create a stopwatch to calculate the estimated rendering time.
  stopwatch := TStopWatch.Create();
  ElapsedSet := False;

  if Not Assigned(transcoder) then
    Exit;

  // Create a media source for the input file.
  hr := transcoder.OpenFile(sInputFile);
  if SUCCEEDED(hr) then
    begin
      Caption := Format('Opened file: %s.', [sInputFile]);
      // Configure the profile and build a topology.
      hr := transcoder.ConfigureAudioOutput();
    end;

  if SUCCEEDED(hr) then
    hr := transcoder.ConfigureVideoOutput();

  if SUCCEEDED(hr) then
    hr := transcoder.ConfigureContainer();

  butStop.Enabled := True;
  butExecute.Enabled := False;

  // Start the stopwatch
  stopwatch.Start;
  lblEstFinish.Caption := 'Calculating estimated time, one moment please...';

  // Transcode and generate the output file.
  if SUCCEEDED(hr) then
    hr := transcoder.EncodeToFile(sOutputFile);

  if SUCCEEDED(hr) then
    Caption := Format('Output file created: %s', [sOutputFile]);

  if FAILED(hr) then
    if hr = E_ABORT then
      Caption := 'Rendering aborted by user.'
    else
      Caption := Format('Could not create the output file (Error %d).',[hr]);

  stopwatch.Stop;
  Sleep(2000);

  Reset(hr);

end;


procedure TfrmTranscoder.Reset(const hr: HResult);
begin
  // When finished, release the engine.
  if Assigned(transcoder) then
    begin
      transcoder.Free;
      transcoder := Nil;
    end;

  butExecute.Enabled := False;
  butStop.Enabled := False;

  // When the process encountered an error or abortion, delete the targetfile.
  if hr <> S_OK then
    DeleteFile(sOutputFile);

  edTargetFile.Text := 'None';
  edSourceFile.Text := 'None';
  lblProgress.Caption := 'Progress: -';
  lblEstFinish.Caption := 'Estimated finish at: -';
  ProgressBar.Position := 0;
end;


procedure TfrmTranscoder.butStopClick(Sender: TObject);
begin
  {void} transcoder.Stop();
end;


procedure TfrmTranscoder.mnuExitClick(Sender: TObject);
begin
  Close();
end;


procedure TfrmTranscoder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  // Terminate a running Transcode process.
  if Assigned(transcoder) then
      transcoder.Stop();
  CanClose := True;
end;


procedure TfrmTranscoder.mnuOpenSourceFileClick(Sender: TObject);
begin
  // Pick a file to extract the audio from
  butExecute.Enabled := False;
  butStop.Enabled := False;
  if Opendialog1.execute then
    begin
      if FileExists(Opendialog1.Filename) then
        begin
          sInputFile := PWideChar(Opendialog1.Filename);
          edSourceFile.Text := Opendialog1.Filename;
        end;
    end;
end;


procedure TfrmTranscoder.WMProgressEvent(var Msg: TMessage);
var
  CurrPosition: Integer;

begin //Position

  // WParam 1 is a progress msg event
  if (Msg.WParam = 1) then
    begin
      CurrPosition := Round((100 * transcoder.m_Position) / transcoder.m_Duration);
      // We wait until we reached 1st 1% to calculate the estimated rendering end time
      if (CurrPosition = 1) and (ElapsedSet = False) then  // calculate 1st 1%
        begin
          ElapsedSet := true;
          stopwatch.Stop;
          lblEstFinish.Caption := Format('Estimated finish at: %s', [MSecToStr((stopwatch.ElapsedMilliseconds * 100) + MillisecondOfTheDay(Now), False)]);
        end;

      // Progress percentage calculation: (100 * position) / totallength; Where progressbar.Max = 100 and Min = 0
      ProgressBar.Position := CurrPosition;
      lblProgress.Caption := Format('Processed: %d%%', [CurrPosition]);
    end;
end;


end.
