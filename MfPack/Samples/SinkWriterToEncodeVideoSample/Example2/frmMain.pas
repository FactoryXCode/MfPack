// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal / Delphi unit
// Release date: 25-11-2022
// Language: ENU
//
// Revision Version: 3.1.3
// Description: UI for example of how to use the SinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Explanatory memorandum:
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit frmMain;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
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
  Vcl.Menus,
  Vcl.ExtDlgs,
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  SinkWriterClass;


type
  TMainForm = class(TForm)
    lblInfo: TLabel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Settings1: TMenuItem;
    Videooutput1: TMenuItem;
    dlgOpenPicture: TOpenPictureDialog;
    mnuCreateFile: TMenuItem;
    imgBitmap: TImage;
    mnuPlayVideoFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Open1Click(Sender: TObject);
    procedure mnuCreateFileClick(Sender: TObject);
    procedure Videooutput1Click(Sender: TObject);
    procedure mnuPlayVideoFileClick(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }

    tgEncodingFormat: TGuid;
    sEncodingFormat: string;
    sBitmapFile: string;
    sExtension: string;
    dwSelectedVideoHeight: DWord;
    dwSelectedVideoWidth: DWord;
    iVideoLength: UINT32;
    bSaveResizedBitmap: Boolean;

  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}

uses
  dlgVideoOutput;


procedure TMainForm.mnuCreateFileClick(Sender: TObject);
var
  hr: HResult;

begin
  if not FileExists(sBitmapFile) then
    begin
      ShowMessage('You did not select a bitmap file');
      Exit;
    end;

  FSampleSinkWriter := TSampleSinkWriter.Create();

  FSampleSinkWriter.SaveResizedBitmap := bSaveResizedBitmap;

  // We could set the params to it's class properties, but that's up to you.
  hr := FSampleSinkWriter.RunSinkWriter(sExtension,
                                        sEncodingFormat,
                                        tgEncodingFormat,
                                        sBitmapFile,
                                        iVideoLength,
                                        dwSelectedVideoWidth,
                                        dwSelectedVideoHeight);

  if SUCCEEDED(hr) then
    begin
      lblInfo.Font.Color := clGreen;
      lblInfo.Caption := 'File has been succesfully created!';
      mnuPlayVideoFile.Enabled := True;
    end
  else
    begin
      lblInfo.Font.Color := clRed;
      lblInfo.Caption := format('Oops, something went wrong :-(   HResult code = %d', [hr]);
      mnuPlayVideoFile.Enabled := False;
    end;
end;


procedure TMainForm.mnuPlayVideoFileClick(Sender: TObject);
begin
  ShellExecute(Handle,
               'open',
               PWideChar(FSampleSinkWriter.VideoFileName),
               nil,
               nil,
               SW_SHOWNORMAL) ;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  SafeDelete(FSampleSinkWriter);
  CanClose := True;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  sExtension := 'mp4';
  tgEncodingFormat := MFVideoFormat_H264;
  sEncodingFormat  := 'MFVideoFormat_H264';
  dwSelectedVideoHeight := 480;
  dwSelectedVideoWidth  := 640;
  iVideoLength := 20;
  bSaveResizedBitmap := False;
end;


procedure TMainForm.Open1Click(Sender: TObject);
begin
try

  dlgOpenPicture := TOpenPictureDialog.Create(Self);
  dlgOpenPicture.InitialDir := GetCurrentDir();
  if dlgOpenPicture.Execute then
    begin
      sBitmapFile := dlgOpenPicture.FileName;
      imgBitmap.Picture.LoadFromFile(sBitmapFile);
      lblInfo.Caption := 'Select ''Render Video File'' or select ''Video output'' to configure the video.';
      mnuCreateFile.Enabled := True;
    end
  else
    begin
      lblInfo.Caption := 'Select ''Open Bitmap''';
      mnuCreateFile.Enabled := False;
    end;

finally
  SafeDelete(dlgOpenPicture);
end;
end;


procedure TMainForm.Videooutput1Click(Sender: TObject);
begin
  // Create the dialog if it's not allready done.
  if not Assigned(dlgVideoSetttings) then
    begin
      Application.CreateForm(TdlgVideoSetttings,
                             dlgVideoSetttings);
      dlgVideoSetttings.Visible := False;
    end;

  // Ask the user to select one.
  if dlgVideoSetttings.ShowModal = mrOk then
    begin
      lblInfo.Caption := 'Select ''Create Video File''';
    end
  else
    begin
      // User canceled.
    end;
end;

end.
