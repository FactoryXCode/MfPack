// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgAudioFileBrowser.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: A lightweight browser to pick audiofiles. This prevents audio disturbs when playing or capturing.
//              The native Windows opendialog is way to heavy and causes glitches in a running audiostream.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit dlgAudioFileBrowser;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.FileCtrl,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  MPxpButton;

const

  CAudioFilter = 'Audio Files (*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus)|*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus|' +
                 'MP3 Files (*.mp3)|*.mp3|' +
                 'WAV Files (*.wav)|*.wav|' +
                 'FLAC Files (*.flac)|*.flac|' +
                 'OGG Files (*.ogg)|*.ogg|' +
                 'M4A Files (*.m4a)|*.m4a|' +
                 'AAC Files (*.aac)|*.aac|' +
                 'WMA Files (*.wma)|*.wma|' +
                 'Opus Files (*.opus)|*.opus|' +
                 'All Files (*.*)|*.*';

type

  TAudioFileBrowserDlg = class(TForm)
    flbFiles: TFileListBox;
    dlbDirectory: TDirectoryListBox;
    pnlBottom: TPanel;
    lblDuration: TLabel;
    Panel1: TPanel;
    cbxDrives: TDriveComboBox;
    Splitter1: TSplitter;
    cbxAudioFileFilter: TFilterComboBox;
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    lblSelectedFile: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure flbFilesChange(Sender: TObject);

  private
    { Private declarations }

    FSelectedFile: TFileName;
    FSelectedFilePath: string;
    FFileDuration: string;

    procedure UpdateOkState();

  public
    { Public declarations }

  published

    property FileName: TFileName read FSelectedFile;
    property FileURI: string read FSelectedFilePath;
    property AudioDuration: string read FFileDuration;
  end;

var
  AudioFileBrowserDlg: TAudioFileBrowserDlg;


implementation

{$R *.dfm}

procedure TAudioFileBrowserDlg.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TAudioFileBrowserDlg.btnOkClick(Sender: TObject);
begin

  ModalResult := mrOk;
end;


procedure TAudioFileBrowserDlg.flbFilesChange(Sender: TObject);
var
  duration: LongLong;

begin

  if (flbFiles.ItemIndex >= 0 ) then
    begin

      FSelectedFile := flbFiles.Items[flbFiles.ItemIndex];
      FSelectedFilePath := IncludeTrailingPathDelimiter(dlbDirectory.Directory) + FSelectedFile;

      lblSelectedFile.Caption := FSelectedFile;
      lblSelectedFile.Hint := FSelectedFilePath;

      GetFileDuration(PWideChar(WideString(FSelectedFile)),
                      duration);

      FFileDuration := HnsTimeToStr(duration,
                                    False);
      lblDuration.Caption := Format('Duration: %s',
                                    [FFileDuration]);
    end;
  UpdateOkState();
end;


procedure TAudioFileBrowserDlg.FormCreate(Sender: TObject);
begin

  cbxAudioFileFilter.Filter := CAudioFilter;
end;


procedure TAudioFileBrowserDlg.UpdateOkState();
begin

  btnOk.Enabled := FileExists(FSelectedFile);
end;


end.
