// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  frmAudioClip.pas
// Kind: Pascal Unit
// Release date: 21-11-2019
// Language: ENU
//
// Revision Version: 3.1.4
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
//   Note: The original application is a console app.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
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
//==============================================================================
// Source: AudioClip sample
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
unit frmAudioClip;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  //WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  {Application}
  AudioClipCore;


type
  TfrmAudioClip = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Extractto1: TMenuItem;
    N1: TMenuItem;
    lblSourceFile: TLabel;
    lblTargetFile: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    butCancel: TButton;
    butExtract: TButton;
    Bevel1: TBevel;
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Extractto1Click(Sender: TObject);
    procedure butExtractClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
  private
    { Private declarations }

    wSourceFile: PWideChar;     // the sourcefile URL
    wTargetFile: PWideChar;     // The target file URL


  public
    { Public declarations }
  end;

var
  AudioClipFrm: TfrmAudioClip;

implementation

{$R *.dfm}

procedure TfrmAudioClip.butExtractClick(Sender: TObject);
var
  iClipLen: integer;

begin
  if TryStrToInt(Edit1.Text, iClipLen) then
    begin

      butExtract.Enabled := False;
      butCancel.Enabled := True;

      if SUCCEEDED(ExtractSound(Self.Handle,
                                wSourceFile,
                                wTargetFile,
                                iClipLen * 1000)) then
        begin
          if (bFlush = false) then
            ShowMessage('Clip extracted!')
          else
            ShowMessage('Clip extraction aborted!');
        end
      else
        ShowMessage('Clip extraction failed!');

      butExtract.Enabled := True;
      butCancel.Enabled := False;
    end
  else
    begin
      ShowMessage('Invalid clip length!');
      Edit1.Text := '5';
    end;
end;


procedure TfrmAudioClip.butCancelClick(Sender: TObject);
begin
  FlushSourceReader(True);
end;


procedure TfrmAudioClip.Exit1Click(Sender: TObject);
begin
  FlushSourceReader(True);
  Close();
end;


procedure TfrmAudioClip.Extractto1Click(Sender: TObject);
var
  tmp: string;

begin
  tmp := ExtractFileName(string(wSourceFile));
  Savedialog1.FileName := ChangeFileExt(tmp, '.wav');

  if Savedialog1.execute then
    begin
      if FileExists(Savedialog1.FileName) then
        DeleteFile(Savedialog1.FileName);

      lblTargetFile.Caption := Savedialog1.FileName;
      wTargetFile := PWideChar(Savedialog1.FileName);
      ButExtract.Enabled := True;
    end;
end;


procedure TfrmAudioClip.Open1Click(Sender: TObject);
begin
  // Pick a file to extract the audio from
  ButExtract.Enabled := False;

  if Opendialog1.execute then
    begin
      if FileExists(Opendialog1.Filename) then
        begin
          wSourceFile := PWideChar(Opendialog1.Filename);
          lblSourceFile.Caption := Opendialog1.Filename;
        end;
    end;
end;

end.
