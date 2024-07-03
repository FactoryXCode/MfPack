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
// Revision Version: 3.1.7
// Description: UI for example of how to use the SinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Renate Schaaf.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 18/05/2023 Renate              Fixed runtime error on selecting multiple bitmaps.
//                                Speedup of bitmap resizing using D2D1_1
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPack/Samples/SinkWriterToEncodeVideoSample
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source:
//   https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
//
// Copyright (c) FactoryX. All rights reserved.
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
    mnuSelectOneBitmap: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    mmnuSettings: TMenuItem;
    mnuVideoOutput: TMenuItem;
    dlgOpenPicture: TOpenPictureDialog;
    mnuRender: TMenuItem;
    imgBitmap: TImage;
    mnuPlayVideoFile: TMenuItem;
    mnuSelectMultipleBitmaps: TMenuItem;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuSelectOneBitmapClick(Sender: TObject);
    procedure mnuRenderClick(Sender: TObject);
    procedure mnuVideoOutputClick(Sender: TObject);
    procedure mnuPlayVideoFileClick(Sender: TObject);
    procedure mnuSelectMultipleBitmapsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);

  private
    { Private declarations }

    // Recieve messages from the sinkwriter.
    procedure OnProcesBmp(var Msg: TMessage); message WM_BITMAP_PROCESSING_MSG;
    procedure OnSinkWriterMsg(var Msg: TMessage); message WM_SINKWRITER_WRITES_BITMAP;

  public
    { Public declarations }
    FBitmapFiles: TStringList;

  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}

uses
  dlgVideoOutput;


procedure TMainForm.mnuRenderClick(Sender: TObject);
var
  hr: HResult;

begin

  MainMenu.Items.Enabled := False;

  if (FBitmapFiles.Count = 0) then
    begin
      ShowMessage('You did not select a bitmap file');
      Exit;
    end;

  lblInfo.Caption := 'Rendering the bitmaps. This can take a while..';

  // Let the sinkwriter process the bitmaps.
  hr := FSinkWriter.RunSinkWriter(FBitmapFiles);

  MainMenu.Items.Enabled := True;

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
               PWideChar(FSinkWriter.SinkWriterParams.pwcVideoFileName),
               nil,
               nil,
               SW_SHOWNORMAL) ;
end;


procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(FSinkWriter);
  FreeAndNil(FBitmapFiles);
  CanClose := True;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin

  FSinkWriter := TSinkWriter.Create(Handle);

  // Default settings
  FSinkWriter.SinkWriterParams.Init();
  FBitmapFiles := TStringList.Create();
end;


procedure TMainForm.mnuSelectOneBitmapClick(Sender: TObject);
var
  bm: TBitmap;

begin
try
  bm := TBitmap.Create();
  FBitmapFiles.Clear();
  dlgOpenPicture := TOpenPictureDialog.Create(Self);
  dlgOpenPicture.InitialDir := GetCurrentDir();

  if dlgOpenPicture.Execute then
    begin
      FBitmapFiles.Append(dlgOpenPicture.FileName);
      bm.LoadFromFile(FBitmapFiles.Strings[0]);
      imgBitmap.Picture.Assign(bm);
      lblInfo.Caption := 'Select ''Render Video File'' or select ''Video output'' to configure the video.';
      mnuRender.Enabled := True;
    end
  else
    begin
      lblInfo.Caption := 'Select ''Select Single Bitmap'' or ''Select Multiple Bitmaps''';
      mnuRender.Enabled := False;
    end;

finally
  FreeAndNil(bm);
  FreeAndNil(dlgOpenPicture);
end;
end;


procedure TMainForm.mnuSelectMultipleBitmapsClick(Sender: TObject);
Var
  Path: string;
  SearchRec: TSearchRec;
  DirList: TStringList;

begin

  if dlgOpenPicture.Execute then
    begin
      mnuRender.Enabled := False;
      // Create a stringlist to add the filenames found.
      DirList := TStringList.Create;
      DirList.Sorted := True;

      try
        FBitmapFiles.Clear;
        //Get the path of the selected file
        Path := ExtractFileDir(dlgOpenPicture.FileName);

        lblInfo.Caption := 'Moment, processing the bitmaps...';

        {$WARN SYMBOL_PLATFORM OFF}
        if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.bmp',
                     faArchive,
                     SearchRec) = 0 then
        {$WARN SYMBOL_PLATFORM ON}
          begin
            repeat
              DirList.Add(IncludeTrailingPathDelimiter(Path) + SearchRec.Name); //Fill the list
            until FindNext(SearchRec) <> 0;
            FindClose(SearchRec);
          end;

        // process the bitmaps found
        FBitmapFiles.AddStrings(DirList);

        if (FBitmapFiles.Count = 0) then
          begin
            lblInfo.Caption := 'No bitmaps found.';
            Exit;
          end;

      finally

        DirList.Free;
        lblInfo.Font.Color := clWindowText;
        lblInfo.Caption := 'Select ''Render Video File'' or select ''Video output'' to configure the video.';
        mnuRender.Enabled := True;
        imgBitmap.Picture.LoadFromFile(FBitmapFiles[0]);

      end;
    end;
end;


procedure TMainForm.mnuVideoOutputClick(Sender: TObject);
begin
  // Create the dialog if it's not allready done.
  if not Assigned(dlgVideoSetttings) then
    begin
      Application.CreateForm(TdlgVideoSetttings,
                             dlgVideoSetttings);
      dlgVideoSetttings.Visible := False;
    end;

  // Ask the user to select one.
  if (dlgVideoSetttings.ShowModal = mrOk) then
    begin
      lblInfo.Caption := 'Select ''Select Single Bitmap'', ''Select Multiple Bitmaps'' or ''Render Video File''';
    end
  else
    begin
      // User canceled.
    end;
end;


procedure TMainForm.OnProcesBmp(var Msg: TMessage);
begin
  // We did recieve a message from SinkWriter.SetBitmapToVideoFormat
  lblInfo.Caption := Format('Processing Bitmap %d of %d.',
                             [Msg.LParam,
                              FBitmapFiles.Count]);
  lblInfo.Update();
end;


procedure TMainForm.OnSinkWriterMsg(var Msg: TMessage);
begin
  // We did recieve a message from SinkWriter.RunSinkWriter
  lblInfo.Caption := Format('Writing Frame %d of %d to file: %s.',
                            [Msg.LParam,
                             FBitmapFiles.Count,
                             FSinkWriter.SinkWriterParams.pwcVideoFileName]);
  lblInfo.Update();
end;


initialization

  ReportMemoryLeaksOnShutDown := True;


end.
