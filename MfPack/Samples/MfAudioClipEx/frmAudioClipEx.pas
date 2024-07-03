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
// Revision Version: 3.1.7
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
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: The IMFSourceReader.ReadSample method eats a lot of CPU cycles and
//               power on low latency file reading.
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  {MfPack}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfReadWrite,
  {Application}
  AudioClipEngine,
  Helpers;


type
  TAudioClipExFrm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    lblGetSourceFile: TLabel;
    lblSetTartgetFile: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Extractto1: TMenuItem;
    N1: TMenuItem;
    lblSourceFile: TLabel;
    lblTargetFile: TLabel;
    Label1: TLabel;
    edClipDuration: TEdit;
    lblTime: TLabel;
    Label2: TLabel;
    tbPriority: TTrackBar;
    lblProgress: TLabel;
    Label3: TLabel;
    prbProgress: TProgressBar;
    Bevel1: TBevel;
    butExtract: TButton;
    butCancel: TButton;
    butClose: TButton;
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Extractto1Click(Sender: TObject);
    procedure butExtractClick(Sender: TObject);
    procedure lblGetSourceFileMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lblSetTartgetFileMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure edClipDurationKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure butCancelClick(Sender: TObject);
    procedure tbPriorityChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure butCloseClick(Sender: TObject);
  private
    { Private declarations }

    wSourceFile: PWideChar;     // The sourcefile URL
    wTargetFile: PWideChar;     // The target file URL
    MfAudioClip: TAudioClipClass;

    procedure Reset();
    procedure OnClipEngineMsg(var Msg: TMessage); message WM_CLIPENGINE_MSG;

  public
    { Public declarations }
  end;

var
  AudioClipExFrm: TAudioClipExFrm;

implementation

{$R *.dfm}

procedure TAudioClipExFrm.Reset();
begin
  tbPriority.Position := 10;
  butExtract.Enabled := False;
  butCancel.Enabled := False;
  edClipDuration.Text := '0';
  prbProgress.Position := 0;
  lblTime.Caption := '00:00:00';
  lblSourceFile.Caption := '-';
  lblTargetFile.Caption := '-';
  lblProgress.Caption := '';
  wSourceFile := Nil;
  wTargetFile := Nil;
  if Assigned(MfAudioClip) then
    FreeAndNil(MfAudioClip);
end;


procedure TAudioClipExFrm.tbPriorityChange(Sender: TObject);
begin
  if Assigned(MfAudioClip) then
    MfAudioClip.SamplePriority := tbPriority.Position;
end;


procedure TAudioClipExFrm.butExtractClick(Sender: TObject);
var
  iClipLen: Integer;
  hr: HResult;

begin
  hr := S_OK;

  if (wSourceFile = Nil) or (wTargetFile = Nil) then
    exit;

  // Delete existing target
  if FileExists(wTargetFile) then
    DeleteFile(wTargetFile);

  if not Assigned(MfAudioClip) then
    MfAudioClip := TAudioClipClass.Create(Self.Handle,
                                          wSourceFile,
                                          hr);

  if Failed(hr) then
     Exit;

  if TryStrToInt(edClipDuration.Text, iClipLen) then
    begin
      butExtract.Enabled := False;
      butClose.Enabled := False;
      MfAudioClip.Flushed := False;
      MfAudioClip.wcSourceFile := wSourceFile;
      MfAudioClip.wcTargetFile := wTargetFile;
      MfAudioClip.dwDataToExtract := (iClipLen * 1000);
      MfAudioClip.SamplePriority := tbPriority.Position;
      if Succeeded(MfAudioClip.ExtractSoundClip()) then
        butCancel.Enabled := True
      else
        begin
          ShowMessage('Function ExtractSoundClip failed!');
          Reset();
        end;
    end
  else
    begin
      ShowMessage('Invalid clip length!');
      Reset();
    end;
end;


procedure TAudioClipExFrm.butCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TAudioClipExFrm.butCancelClick(Sender: TObject);
begin
  if Assigned(MfAudioClip) then
    begin
      SendMessage(MfAudioClip.FHWnd,
                  WM_USER_ABORT,
                  0,
                  0);

      while Not MfAudioClip.Flushed do
        HandleMessages(0);
    end;
end;


procedure TAudioClipExFrm.edClipDurationKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  iClipLen: Integer;

begin
  if Length(edClipDuration.Text) = 0 then
    begin
      lblTime.Caption := '00:00:00';
      Exit;
    end;

  if TryStrToInt(edClipDuration.Text, iClipLen) then
    lblTime.Caption := MSecToStr(iClipLen * ONE_MSEC_SECOND, False)
  else
    lblTime.Caption := '00:00:00';
end;


procedure TAudioClipExFrm.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TAudioClipExFrm.Extractto1Click(Sender: TObject);
var
  tmp: string;

begin
  tmp := ExtractFileName(string(wSourceFile));
  Savedialog1.FileName := ChangeFileExt(tmp, '.wav');

  if Savedialog1.execute then
    begin
      // If choosen the source as target, change it to wav
      if wSourceFile = Savedialog1.FileName then
        Savedialog1.FileName := ChangeFileExt(wSourceFile, '.wav');
      // Delete existing target
      if FileExists(Savedialog1.FileName) then
        DeleteFile(Savedialog1.FileName);

      lblTargetFile.Caption := Savedialog1.FileName;
      wTargetFile := PWideChar(Savedialog1.FileName);
      butExtract.Enabled := True;
      butCancel.Enabled := True;
      butClose.Enabled := False;
    end;
end;


procedure TAudioClipExFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  butCancelClick(Self); // when sampling is going on, we need to send a message to quit.

  MfAudioClip.Free();
  CoUninitialize();
  CanClose := True;
end;


procedure TAudioClipExFrm.FormCreate(Sender: TObject);
var
  hr: HResult;
begin
  prbProgress.Max := prbProgress.Width;

  hr := CoInitializeEx(nil,
                       COINIT_MULTITHREADED);
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Initialize Com Failure.' + 'The application will closed.'),
                 lpcwstr('Com Failure'),
                 MB_ICONSTOP);
      Close();
    end;
end;


procedure TAudioClipExFrm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblGetSourceFile.Font.Color := clHighLight;
  lblGetSourceFile.Font.Style := lblGetSourceFile.Font.Style - [fsBold];
  lblSetTartgetFile.Font.Color := clHighLight;
  lblSetTartgetFile.Font.Style := lblSetTartgetFile.Font.Style - [fsBold];
end;


procedure TAudioClipExFrm.lblGetSourceFileMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblGetSourceFile.Font.Color := clHotLight;
  lblGetSourceFile.Font.Style := lblGetSourceFile.Font.Style + [fsBold];
end;


procedure TAudioClipExFrm.lblSetTartgetFileMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblSetTartgetFile.Font.Color := clHotLight;
  lblSetTartgetFile.Font.Style := lblSetTartgetFile.Font.Style + [fsBold];
end;


procedure TAudioClipExFrm.Open1Click(Sender: TObject);
var
  hr: HResult;

begin
  // Pick a file to extract the audio from

  // Clear all
  Reset();

  if Opendialog1.execute then
    begin
      if FileExists(Opendialog1.Filename) then
        begin
          wSourceFile := PWideChar(Opendialog1.Filename);
          lblSourceFile.Caption := Opendialog1.Filename;

          // Now we have to check some properties of the source file.
          MfAudioClip := TAudioClipClass.Create(Self.Handle,
                                                wSourceFile,
                                                hr);

          if Failed(hr) then
            Reset()
          else // set duration
            begin
              edClipDuration.Text := IntToStr(MfAudioClip.msecDuration div ONE_MSEC_SECOND);
              lblTime.Caption := MSecToStr(MfAudioClip.msecDuration, False);
            end;
        end;
    end;
end;


procedure TAudioClipExFrm.OnClipEngineMsg(var Msg: TMessage);
begin
  if (Not Assigned(MfAudioClip)) or (MfAudioClip.dwMaxAudioData = 0) then
    Exit;

  // Update progress msg
  if Msg.WParam = 1 then
    begin
      prbProgress.Position := Round((prbProgress.Width / MfAudioClip.dwMaxAudioData) *
                                    MfAudioClip.dwAudioDataWritten);
      lblProgress.Caption := Format('Processed %u Kb from %u Kb.',
                                    [MfAudioClip.dwAudioDataWritten div 1024,
                                     MfAudioClip.dwMaxAudioData div 1024]);
    end
  else
    // Finnished processing audiodata
    if Msg.WParam = 2 then
      begin
        // Check HResult
        if Succeeded(HResult(Msg.lParam)) then
          begin
            lblProgress.Caption := 'Clip succesfully extracted.';
          end
        else
          if HResult(Msg.lParam) = E_ABORT then  //$80004004
            lblProgress.Caption := 'Clip extraction aborted.'
        else
          lblProgress.Caption := 'Clip extraction failed!';

        butExtract.Enabled := False;
        butCancel.Enabled := False;
        butClose.Enabled := True;
      end;
end;

end.
