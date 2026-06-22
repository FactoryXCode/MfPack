// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioFileBrowserDlg.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Lightwheigt file open dialog (See comments below this header).
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
unit MfAudioFileBrowserDlg;

{
  Lightweight reusable audio-file browser dialog for VCL.

  Goals:
  - Avoid the heavyweight Windows common Open dialog
  - Reuse a persistent dialog instance
  - Keep browsing simple and fast
  - No shell icon extraction / preview handlers / metadata scanning

  Usage:

    uses
      MfAudioFileBrowserDlg;

    procedure TfrmPlaylistEditor.btnOpenFileClick(Sender: TObject);
    var
      SelectedFile: TFileName;

    begin

      PreloadAudioFileBrowser(Self);

      if not BrowseAudioFileLightweight(Self,
                                        SelectedFile,
                                        ExtractFilePath(edFileName.Hint)) then
        Exit;

      edFileName.Text := ExtractFileName(SelectedFile);
      edFileName.Hint := SelectedFile;
    end;

  Notes:
  - This unit creates the form completely in code. No DFM is needed.
  - The dialog instance is cached and reused until finalization.
  - Still runs on the UI thread, but avoids the shell-heavy common dialog.
}

interface

uses

  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,

  System.Classes,

  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Vcl.Graphics;

type

  TfrmAudioFileBrowser = class(TForm)
  private
    FAccepted: Boolean;
    FSelectedFile: TFileName;

    pnlTop: TPanel;
    pnlClient: TPanel;
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlBottomButtons: TPanel;
    pnlBottomFile: TPanel;
    pnlBottomFilter: TPanel;

    lblFolder: TLabel;
    lblFiles: TLabel;
    lblFileName: TLabel;
    lblFilter: TLabel;

    drvDrives: TDriveComboBox;
    dirFolders: TDirectoryListBox;
    flbFiles: TFileListBox;
    fcbFilter: TFilterComboBox;
    edtFileName: TEdit;
    btnOpen: TButton;
    btnCancel: TButton;

    procedure BuildUi;
    procedure UpdateOkState;
    procedure UpdateSelectedFileFromControls;
    procedure ApplyInitialPath(const AInitialPath: string);

    procedure DriveChanged(Sender: TObject);
    procedure FolderChanged(Sender: TObject);
    procedure FileSelectionChanged(Sender: TObject);
    procedure FileDoubleClick(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FileNameChange(Sender: TObject);
    procedure FileNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  protected

    procedure CreateParams(var Params: TCreateParams); override;

  public

    constructor Create(AOwner: TComponent); override;
    function Execute(out AFileName: TFileName;
                     const AInitialPath: string = ''): Boolean;
    property SelectedFile: TFileName read FSelectedFile;
  end;

  procedure PreloadAudioFileBrowser(AOwner: TComponent);
  function BrowseAudioFileLightweight(const AOwner: TComponent;
                                      out AFileName: TFileName;
                                      const AInitialPath: string = ''): Boolean;
  procedure ReleaseAudioFileBrowser();


implementation

var
  GAudioFileBrowser: TfrmAudioFileBrowser = nil;

const
  CAudioFilter =
    'Audio Files (*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus)|*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus|' +
    'MP3 Files (*.mp3)|*.mp3|' +
    'WAV Files (*.wav)|*.wav|' +
    'FLAC Files (*.flac)|*.flac|' +
    'OGG Files (*.ogg)|*.ogg|' +
    'M4A Files (*.m4a)|*.m4a|' +
    'AAC Files (*.aac)|*.aac|' +
    'WMA Files (*.wma)|*.wma|' +
    'Opus Files (*.opus)|*.opus|' +
    'All Files (*.*)|*.*';


function NormalizeExistingPath(const APath: string): string;
begin

  Result := Trim(APath);

  if (Result = '') then
    Exit('');

  if FileExists(Result) then
    Result := ExtractFilePath(Result)
  else
    if not System.SysUtils.DirectoryExists(Result) then
      Result := '';
end;


{ TfrmAudioFileBrowser }

constructor TfrmAudioFileBrowser.Create(AOwner: TComponent);
begin

  inherited CreateNew(AOwner);

  BorderStyle := bsDialog;
  BorderIcons := [];
  Caption := 'Open audio file';
  Position := poScreenCenter;
  Width := 860;
  Height := 560;
  Color := clBtnFace;
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  KeyPreview := True;
  OnCloseQuery := FormCloseQuery;

  BuildUi();

  fcbFilter.Filter := CAudioFilter;
  fcbFilter.FileList := flbFiles;
  fcbFilter.ItemIndex := 1;

  drvDrives.DirList := dirFolders;
  dirFolders.FileList := flbFiles;

  UpdateOkState();
end;


procedure TfrmAudioFileBrowser.CreateParams(var Params: TCreateParams);
begin

  inherited CreateParams(Params);

  Params.Style := Params.Style or
                  WS_CAPTION or
                  WS_SYSMENU or
                  WS_CLIPCHILDREN;

  Params.ExStyle := Params.ExStyle or WS_EX_CONTROLPARENT;
end;


procedure TfrmAudioFileBrowser.BuildUi;
begin

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.BevelOuter := bvNone;
  pnlTop.Height := 44;
  pnlTop.Padding.Left := 8;
  pnlTop.Padding.Top := 8;
  pnlTop.Padding.Right := 8;
  pnlTop.Padding.Bottom := 4;

  drvDrives := TDriveComboBox.Create(Self);
  drvDrives.Parent := pnlTop;
  drvDrives.Align := alClient;
  drvDrives.OnChange := DriveChanged;

  pnlClient := TPanel.Create(Self);
  pnlClient.Parent := Self;
  pnlClient.Align := alClient;
  pnlClient.BevelOuter := bvNone;
  pnlClient.Padding.Left := 8;
  pnlClient.Padding.Top := 4;
  pnlClient.Padding.Right := 8;
  pnlClient.Padding.Bottom := 4;

  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := pnlClient;
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 300;
  pnlLeft.BevelOuter := bvNone;
  pnlLeft.Padding.Right := 4;

  lblFolder := TLabel.Create(Self);
  lblFolder.Parent := pnlLeft;
  lblFolder.Align := alTop;
  lblFolder.Caption := 'Folders';

  dirFolders := TDirectoryListBox.Create(Self);
  dirFolders.Parent := pnlLeft;
  dirFolders.Align := alClient;
  dirFolders.OnChange := FolderChanged;
  dirFolders.TabOrder := 0;

  pnlRight := TPanel.Create(Self);
  pnlRight.Parent := pnlClient;
  pnlRight.Align := alClient;
  pnlRight.BevelOuter := bvNone;
  pnlRight.Padding.Left := 4;

  lblFiles := TLabel.Create(Self);
  lblFiles.Parent := pnlRight;
  lblFiles.Align := alTop;
  lblFiles.Caption := 'Files';

  flbFiles := TFileListBox.Create(Self);
  flbFiles.Parent := pnlRight;
  flbFiles.Align := alClient;
  flbFiles.Mask := '*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus';
  flbFiles.FileType := [ftReadOnly, ftHidden, ftArchive, ftNormal];
  flbFiles.OnChange := FileSelectionChanged;
  flbFiles.OnDblClick := FileDoubleClick;
  flbFiles.TabOrder := 0;

  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self;
  pnlBottom.Align := alBottom;
  pnlBottom.BevelOuter := bvNone;
  pnlBottom.Height := 96;
  pnlBottom.Padding.Left := 8;
  pnlBottom.Padding.Top := 4;
  pnlBottom.Padding.Right := 8;
  pnlBottom.Padding.Bottom := 8;

  pnlBottomFile := TPanel.Create(Self);
  pnlBottomFile.Parent := pnlBottom;
  pnlBottomFile.Align := alTop;
  pnlBottomFile.BevelOuter := bvNone;
  pnlBottomFile.Height := 28;

  lblFileName := TLabel.Create(Self);
  lblFileName.Parent := pnlBottomFile;
  lblFileName.Align := alLeft;
  lblFileName.Caption := 'File name:';
  lblFileName.Layout := tlCenter;
  lblFileName.Width := 80;

  edtFileName := TEdit.Create(Self);
  edtFileName.Parent := pnlBottomFile;
  edtFileName.Align := alClient;
  edtFileName.OnChange := FileNameChange;
  edtFileName.OnKeyPress := FileNameKeyPress;
  edtFileName.TabOrder := 0;

  pnlBottomFilter := TPanel.Create(Self);
  pnlBottomFilter.Parent := pnlBottom;
  pnlBottomFilter.Align := alTop;
  pnlBottomFilter.BevelOuter := bvNone;
  pnlBottomFilter.Height := 28;
  pnlBottomFilter.Padding.Top := 4;

  lblFilter := TLabel.Create(Self);
  lblFilter.Parent := pnlBottomFilter;
  lblFilter.Align := alLeft;
  lblFilter.Caption := 'Filter:';
  lblFilter.Layout := tlCenter;
  lblFilter.Width := 80;

  fcbFilter := TFilterComboBox.Create(Self);
  fcbFilter.Parent := pnlBottomFilter;
  fcbFilter.Align := alClient;
  fcbFilter.OnChange := FilterChanged;
  fcbFilter.TabOrder := 0;

  pnlBottomButtons := TPanel.Create(Self);
  pnlBottomButtons.Parent := pnlBottom;
  pnlBottomButtons.Align := alBottom;
  pnlBottomButtons.BevelOuter := bvNone;
  pnlBottomButtons.Height := 34;

  btnCancel := TButton.Create(Self);
  btnCancel.Parent := pnlBottomButtons;
  btnCancel.Align := alRight;
  btnCancel.Width := 96;
  btnCancel.Caption := 'Cancel';
  btnCancel.ModalResult := mrCancel;
  btnCancel.OnClick := CancelClick;
  btnCancel.TabOrder := 1;

  btnOpen := TButton.Create(Self);
  btnOpen.Parent := pnlBottomButtons;
  btnOpen.Align := alRight;
  btnOpen.Width := 96;
  btnOpen.Caption := 'Open';
  btnOpen.Default := True;
  btnOpen.ModalResult := mrNone;
  btnOpen.OnClick := OpenClick;
  btnOpen.TabOrder := 0;
end;


procedure TfrmAudioFileBrowser.UpdateOkState;
begin

  btnOpen.Enabled := FileExists(FSelectedFile);
end;


procedure TfrmAudioFileBrowser.UpdateSelectedFileFromControls;
var
  Candidate: string;

begin

  Candidate := Trim(edtFileName.Text);

  if (Candidate = '') then
    begin

      FSelectedFile := '';
      UpdateOkState;
      Exit;
    end;

  if ExtractFilePath(Candidate) = '' then
    Candidate := IncludeTrailingPathDelimiter(dirFolders.Directory) + Candidate;

  FSelectedFile := ExpandFileName(Candidate);
  UpdateOkState();
end;


procedure TfrmAudioFileBrowser.ApplyInitialPath(const AInitialPath: string);
var
  PathToOpen: string;

begin

  PathToOpen := NormalizeExistingPath(AInitialPath);
  if (PathToOpen = '') then
    PathToOpen := GetCurrentDir;

  if not System.SysUtils.DirectoryExists(PathToOpen) then
    Exit;

  try

    dirFolders.Directory := PathToOpen;
  except

    // Ignore invalid drive/path transitions and leave current selection as-is.
  end;

  edtFileName.Clear;
  FSelectedFile := '';
  UpdateOkState;
end;

procedure TfrmAudioFileBrowser.DriveChanged(Sender: TObject);
begin

  UpdateSelectedFileFromControls();
end;


procedure TfrmAudioFileBrowser.FolderChanged(Sender: TObject);
begin

  UpdateSelectedFileFromControls();
end;


procedure TfrmAudioFileBrowser.FileSelectionChanged(Sender: TObject);
begin

  if (flbFiles.ItemIndex >= 0) then
    edtFileName.Text := flbFiles.Items[flbFiles.ItemIndex]
  else
    UpdateSelectedFileFromControls;
end;


procedure TfrmAudioFileBrowser.FileDoubleClick(Sender: TObject);
begin

  FileSelectionChanged(Sender);
  if btnOpen.Enabled then
    OpenClick(btnOpen);
end;


procedure TfrmAudioFileBrowser.FilterChanged(Sender: TObject);
begin

  UpdateSelectedFileFromControls();
end;


procedure TfrmAudioFileBrowser.OpenClick(Sender: TObject);
begin

  UpdateSelectedFileFromControls;

  if not FileExists(FSelectedFile) then
    Exit;

  FAccepted := True;
  ModalResult := mrOk;
end;


procedure TfrmAudioFileBrowser.CancelClick(Sender: TObject);
begin

  FAccepted := False;
  ModalResult := mrCancel;
end;


procedure TfrmAudioFileBrowser.FileNameChange(Sender: TObject);
begin

  UpdateSelectedFileFromControls;
end;


procedure TfrmAudioFileBrowser.FileNameKeyPress(Sender: TObject; var Key: Char);
begin

  if (Key = #13) then
  begin

    Key := #0;
    OpenClick(btnOpen);
  end;
end;


procedure TfrmAudioFileBrowser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := True;
end;


function TfrmAudioFileBrowser.Execute(out AFileName: TFileName;
  const AInitialPath: string): Boolean;

begin

  FAccepted := False;
  FSelectedFile := '';
  AFileName := '';

  ApplyInitialPath(AInitialPath);

  if ShowModal = mrOk then
    Result := FAccepted and FileExists(FSelectedFile)
  else
    Result := False;

  if Result then
    AFileName := FSelectedFile;
end;


procedure PreloadAudioFileBrowser(AOwner: TComponent);
begin

  if Assigned(GAudioFileBrowser) then
    Exit;

  GAudioFileBrowser := TfrmAudioFileBrowser.Create(AOwner);
  GAudioFileBrowser.Visible := False;
end;


function BrowseAudioFileLightweight(const AOwner: TComponent;
  out AFileName: TFileName; const AInitialPath: string): Boolean;

begin

  PreloadAudioFileBrowser(TComponent(AOwner));
  Result := GAudioFileBrowser.Execute(AFileName, AInitialPath);
end;


procedure ReleaseAudioFileBrowser;
begin
  FreeAndNil(GAudioFileBrowser);
end;


initialization

finalization
  ReleaseAudioFileBrowser();

end.
