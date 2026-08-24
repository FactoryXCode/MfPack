// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmPlaylistEditor.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Playlist editor form.
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
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
unit frmPlaylistEditor;

interface

  {$WARN SYMBOL_PLATFORM OFF}

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  Winapi.ShellAPI,
  WinApi.WinError,
  Winapi.MMSystem,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.Types,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.Menus,
  {FireDAC}
  FireDAC.Stan.Factory,
  {Application}
  RDJ_Common,
  RDJ.PlaylistTypes,
  RDJ.PlaylistDb,
  RDJ.TrackLibrary,
  RDJ.TrackQuality,
  RDJ.PlaylistManager,
  RDJ.LibraryScanner,
  RDJ.LibraryScanThread,
  LWFileBrowserExDlg,
  MPxpButton,
  frmTagEditor;

type

  TfrmPlaylistEditor = class(TForm)
    pnlBottom: TPanel;
    lblStatus: TLabel;
    pnlClient: TPanel;
    splLeft: TSplitter;
    pnlLibrary: TPanel;
    grdLibrary: TStringGrid;
    pnlActions: TPanel;
    btnAddToPlaylist: TMPxpButton;
    btnRemoveFromPlaylist: TMPxpButton;
    btnMoveUp: TMPxpButton;
    btnMoveDown: TMPxpButton;
    MPxpButton1: TMPxpButton;
    btnScanFolder: TMPxpButton;
    btnCancelScan: TMPxpButton;
    btnClearLibrary: TMPxpButton;
    btnClearMissingTracks: TMPxpButton;
    grdPlaylist: TStringGrid;
    pnlTop: TPanel;
    Bevel2: TBevel;
    lblSearch: TLabel;
    edtSearch: TEdit;
    btnSearch: TMPxpButton;
    btnClearSearch: TMPxpButton;
    pnlPlaylist: TPanel;
    lblPlaylist: TLabel;
    cbPlaylists: TComboBox;
    btnDeletePlaylist: TMPxpButton;
    btnSavePlaylist: TMPxpButton;
    btnNewPlaylist: TMPxpButton;
    btnOpenFile: TMPxpButton;
    edFileName: TEdit;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    lblPlayListDuration: TLabel;
    btnMinimize: TMPxpButton;
    btnMaxNormal: TMPxpButton;
    btnExit: TMPxpButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnNewPlaylistClick(Sender: TObject);
    procedure btnSavePlaylistClick(Sender: TObject);
    procedure btnDeletePlaylistClick(Sender: TObject);
    procedure cbPlaylistsChange(Sender: TObject);
    procedure btnAddToPlaylistClick(Sender: TObject);
    procedure btnRemoveFromPlaylistClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure grdLibraryDblClick(Sender: TObject);
    procedure grdLibrarySelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure grdLibrarySetEditText(Sender: TObject; ACol, ARow: LongInt;
      const Value: string);
    procedure cbPlaylistsDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnTagEditorClick(Sender: TObject);
    procedure grdLibraryMouseDown(Sender: TObject;
                                  Button: TMouseButton;
                                  Shift: TShiftState;
                                  X, Y: Integer);
    procedure mnuScanFolderClick(Sender: TObject);
    procedure LibraryPopupPrelistenClick(Sender: TObject); // Pre listen from source, before adding it to a playlist.
    procedure LibraryPopupLoadToChannelClick(Sender: TObject);
    procedure LibraryPopupAddToPlaylistClick(Sender: TObject);
    procedure LibraryPopupEditTagsClick(Sender: TObject);
    procedure LibraryPopupOpenFileLocationClick(Sender: TObject);
    procedure LibraryPopupRemoveFromLibraryClick(Sender: TObject);
    procedure grdPlaylistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOpenFileClick(Sender: TObject);
    procedure edFileNameMouseDown(Sender: TObject;
                                  Button: TMouseButton;
                                  Shift: TShiftState;
                                  X, Y: Integer);
    procedure LoadedFilePopupClick(Sender: TObject);
    procedure btnClearLibraryClick(Sender: TObject);
    procedure btnClearMissingTracksClick(Sender: TObject);
    procedure btnCancelScanClick(Sender: TObject);
    procedure btnScanFolderClick(Sender: TObject);
    procedure pnlCaptionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnExitClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnMaxNormalClick(Sender: TObject);

  private

    FDb: TRDJPlaylistDb;
    FLibrary: TRDJTrackLibrary;
    FPlaylistMgr: TRDJPlaylistManager;
    FScanner: TRDJLibraryScanner;
    FScanThread: TRDJLibraryScanThread;

    FCurrentPlaylist: TRDJPlaylist;
    FLibraryRows: TList<Integer>;    // row -> TrackID
    FPlaylistRows: TList<Integer>;   // row -> PlaylistItemID
    FCurrentPlaylistDurationMs: Int64; // Gets the total duration of a playlist.

    FSendToChannelPopup: TPopupMenu;
    FLibraryPopup: TPopupMenu;
    FLoadFilePopUp: TPopUpMenu;

    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;

    procedure BuildLoadedFilePopup();
    procedure BuildLibraryPopup();
    function PrelistenSelectedLibraryTrackOnChannel(const AChannelIndex: Integer): HRESULT;
    function RemoveSelectedLibraryTrack(): Boolean;

    // Handlers ----------------------------------------------------------------
    procedure ScanThreadProgress(Sender: TObject;
                                 const AFileName: string;
                                 ACurrentIndex: Integer;
                                 ATotalCount: Integer);

    procedure ScanThreadFinished(Sender: TObject;
                                 AProcessedCount: Integer;
                                 ACancelled: Boolean;
                                 const AErrorMsg: string);
    // -------------------------------------------------------------------------

    procedure InitLibraryGrid();
    procedure InitPlaylistGrid();

    procedure RefreshLibraryGrid(const ASearchText: string);
    procedure RefreshPlaylistGrid();
    procedure RefreshPlaylistsCombo();

    function GetSelectedLibraryTrackID(): Integer;
    function LoadSelectedLibraryTrackOnChannel(const AChannelIndex: Integer): HRESULT;
    function GetSelectedPlaylistIndex(): Integer;
    function GetCurrentPlaylistIDFromCombo(): Integer;

    procedure LoadPlaylistByID(const APlaylistID: Integer);
    function CalculateCurrentPlaylistDurationMs(): Int64;
    function FormatDurationMs(const AMs: Int64;
                              const ALongFormat: Boolean = False): string;
    procedure UpdatePlaylistDurationStatus();
    procedure SetStatus(const S: string);

    procedure ScannerProgress(Sender: TObject;
                              const AFileName: string;
                              ACurrentIndex: Integer;
                              ATotalCount: Integer);

    function SelectFolderDialog(var AFolder: string): Boolean;

    procedure BuildSendToChannelPopup();
    function PlaySelectedPlaylistRowOnChannel(const AChannelIndex: Integer): HRESULT;
    function LoadSelectedPlaylistRowOnChannel(const AChannelIndex: Integer): HRESULT;
    procedure SendPlaylistToChannelClick(Sender: TObject);
    function PlayCurrentPlaylistOnChannel(const AChannelIndex: Integer): HRESULT;
    function LoadCurrentPlaylistOnChannel(const AChannelIndex: Integer): HRESULT;

    function EditSelectedLibraryTrackTags(): Boolean;
    function GetLoadedFileNameFromEdit(out AFileName: string): Boolean;
    function FindLibraryTrackByFullPath(const AFileName: string;
                                        out ATrack: TRDJTrack): Boolean;
    function ReadAudioDurationMs(const AFileName: string): Int64;
    function AddLoadedFileToCurrentPlaylist(): Boolean;
    function LoadLoadedFileOnChannel(const AChannelIndex: Integer;
                                     const AStartPlayback: Boolean): HRESULT;

  public


  end;

var
  PlaylistEditorfrm: TfrmPlaylistEditor;


implementation

{$R *.dfm}

uses
  frmMainMDI,
  frmChannelDeck,
  MfAudioFileBrowserDlg,
  RDJ.FilenameParser;


procedure TfrmPlaylistEditor.FormCreate(Sender: TObject);
var
  DbFileName: string;

begin

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);

  FLibraryRows := TList<Integer>.Create;
  FPlaylistRows := TList<Integer>.Create;
  FCurrentPlaylist := nil;
  FCurrentPlaylistDurationMs := 0;

  FSendToChannelPopup := TPopupMenu.Create(Self);
  FLibraryPopup := TPopupMenu.Create(Self);

  FLoadFilePopUp := TPopupMenu.Create(Self);
  edFileName.OnMouseDown := edFileNameMouseDown;
  edFileName.PopupMenu := FLoadFilePopUp;

  FDb := TRDJPlaylistDb.Create;
  DbFileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                'Data\RDJLibrary.db';
  FDb.Open(DbFileName);

  FLibrary := TRDJTrackLibrary.Create(FDb);
  FPlaylistMgr := TRDJPlaylistManager.Create(FDb,
                                             FLibrary);
  FScanner := TRDJLibraryScanner.Create(FLibrary);
  FScanner.OnProgress := ScannerProgress;

  InitLibraryGrid();
  InitPlaylistGrid();

  RefreshPlaylistsCombo();
  RefreshLibraryGrid('');

  SetStatus('Playlist editor ready.');
end;


procedure TfrmPlaylistEditor.FormDestroy(Sender: TObject);
begin

  if Assigned(FScanThread) then
    begin

      FScanThread.Terminate;
      FScanThread := nil;
    end;

  FreeAndNil(FCurrentPlaylist);
  FreeAndNil(FScanner);
  FreeAndNil(FPlaylistMgr);
  FreeAndNil(FLibrary);
  FreeAndNil(FDb);

  FreeAndNil(FPlaylistRows);
  FreeAndNil(FLibraryRows);

  FreeAndNil(FSendToChannelPopup);
  FreeAndNil(FLibraryPopup);
end;


procedure TfrmPlaylistEditor.FormShow(Sender: TObject);
begin

  ApplyDarkWindowFrame(Handle);

  if (cbPlaylists.Items.Count > 0) then
    cbPlaylistsChange(Self);
end;

// Handlers --------------------------------------------------------------------

procedure TfrmPlaylistEditor.ScanThreadProgress(Sender: TObject;
                                                const AFileName: string;
                                                ACurrentIndex: Integer;
                                                ATotalCount: Integer);
begin

  SetStatus(Format('Scanning %d/%d: %s',
                   [ACurrentIndex,
                    ATotalCount,
                    ExtractFileName(AFileName)]));
end;


procedure TfrmPlaylistEditor.ScanThreadFinished(Sender: TObject;
                                                AProcessedCount: Integer;
                                                ACancelled: Boolean;
                                                const AErrorMsg: string);
begin

  FScanThread := nil;

  RefreshLibraryGrid(edtSearch.Text);

  if (AErrorMsg <> '') then
    begin

      SetStatus('Scan failed: ' + AErrorMsg);
      Exit;
    end;

  if ACancelled then
    SetStatus(Format('Scan cancelled. %d file(s) processed.',
                     [AProcessedCount]))
  else
    SetStatus(Format('Scan complete. %d file(s) processed.',
                     [AProcessedCount]));
end;

// -----------------------------------------------------------------------------


procedure TfrmPlaylistEditor.InitLibraryGrid();
begin

  grdLibrary.ColCount := 5;  //7 if we want "genre" and "album"
  grdLibrary.RowCount := 2;
  grdLibrary.FixedRows := 1;
  grdLibrary.FixedCols := 0;
  grdLibrary.Options := grdLibrary.Options + [goRowSelect,
                                              goColSizing,
                                              goEditing];

  grdLibrary.Cells[0, 0] := 'Artist';
  grdLibrary.Cells[1, 0] := 'Title';
  //grdLibrary.Cells[2, 0] := 'Album';
  //grdLibrary.Cells[3, 0] := 'Genre';
  grdLibrary.Cells[2, 0] := 'Duration';
  grdLibrary.Cells[3, 0] := 'Quality';
  grdLibrary.Cells[4, 0] := 'Path';

  grdLibrary.ColWidths[0] := 200;
  grdLibrary.ColWidths[1] := 200;
  //grdLibrary.ColWidths[2] := 200;
  //grdLibrary.ColWidths[3] := 120;
  grdLibrary.ColWidths[2] := 100;
  grdLibrary.ColWidths[3] := 170;
  grdLibrary.ColWidths[4] := 250;
end;


procedure TfrmPlaylistEditor.InitPlaylistGrid;
begin

  grdPlaylist.ColCount := 5;
  grdPlaylist.RowCount := 2;
  grdPlaylist.FixedRows := 1;
  grdPlaylist.FixedCols := 0;
  grdPlaylist.Options := grdPlaylist.Options + [goRowSelect];
  grdPlaylist.Options := grdPlaylist.Options - [goEditing];

  grdPlaylist.Cells[0, 0] := '#';
  grdPlaylist.Cells[1, 0] := 'Artist';
  grdPlaylist.Cells[2, 0] := 'Title';
  grdPlaylist.Cells[3, 0] := 'Duration';
  grdPlaylist.Cells[4, 0] := 'Note';

  grdPlaylist.ColWidths[0] := 60;
  grdPlaylist.ColWidths[1] := 200;
  grdPlaylist.ColWidths[2] := 200;
  grdPlaylist.ColWidths[3] := 90;
  grdPlaylist.ColWidths[4] := 240;
end;


function TfrmPlaylistEditor.FormatDurationMs(const AMs: Int64;
                                             const ALongFormat: Boolean): string;
var
  TotalSecs: Int64;
  Hours: Int64;
  Mins: Int64;
  Secs: Int64;

begin

  if (AMs <= 0) then
    begin

      if ALongFormat then
        Exit('00:00:00')
      else
        Exit('');
    end;

  TotalSecs := AMs div 1000;
  Hours := TotalSecs div 3600;
  Mins := (TotalSecs mod 3600) div 60;
  Secs := TotalSecs mod 60;

  if ALongFormat or (Hours > 0) then
    Result := Format('%.2d:%.2d:%.2d',
                     [Hours,
                      Mins,
                      Secs])
  else
    Result := Format('%.2d:%.2d',
                     [Mins,
                      Secs]);
end;


function TfrmPlaylistEditor.CalculateCurrentPlaylistDurationMs(): Int64;
var
  i: Integer;
  E: TRDJPlaylistEntry;

begin

  Result := 0;

  if (FCurrentPlaylist = nil) then
    Exit;

  for i := 0 to FCurrentPlaylist.Count - 1 do
    begin

      E := FCurrentPlaylist[i];

      if (E.Track.DurationMs > 0) then
        Inc(Result,
            E.Track.DurationMs);
    end;
end;


procedure TfrmPlaylistEditor.UpdatePlaylistDurationStatus();
begin

  FCurrentPlaylistDurationMs := CalculateCurrentPlaylistDurationMs();

  lblPlayListDuration.Caption := Format('Playlist loaded: %s - %d track(s), total duration %s.',
                                        [FCurrentPlaylist.Info.Name,
                                        FCurrentPlaylist.Count,
                                        FormatDurationMs(FCurrentPlaylistDurationMs,
                                        True)]);
end;


procedure TfrmPlaylistEditor.SetStatus(const S: string);
begin

  lblStatus.Caption := S;
end;


procedure TfrmPlaylistEditor.ScannerProgress(Sender: TObject;
                                             const AFileName: string;
                                             ACurrentIndex: Integer;
                                             ATotalCount: Integer);
begin

  SetStatus(Format('Scanning %d/%d: %s',
                   [ACurrentIndex,
                    ATotalCount,
                    ExtractFileName(AFileName)]));

  Application.ProcessMessages;
end;


procedure TfrmPlaylistEditor.RefreshLibraryGrid(const ASearchText: string);
var
  L: TList<TRDJTrack>;
  i: Integer;
  T: TRDJTrack;

  function FormatDuration(const AMs: Int64): string;
  var
    Secs: Int64;
    Mins: Int64;
    RemSecs: Int64;

  begin

    if (AMs <= 0) then
      Exit('');

    Secs := AMs div 1000;
    Mins := Secs div 60;
    RemSecs := Secs mod 60;

    Result := Format('%.2d:%.2d',
                     [Mins,
                      RemSecs]);
  end;

begin

  FLibraryRows.Clear();

  grdLibrary.RowCount := 2;
  grdLibrary.Rows[1].Clear;

  if (Trim(ASearchText) = '') then
    begin

      SetStatus('Type to search your library.');
      Exit;
    end;

  L := FLibrary.SearchTrackSummaries(ASearchText);
  try

    if (L.Count = 0) then
      begin

        SetStatus('No tracks found.');
        Exit;
      end;

    grdLibrary.RowCount := L.Count + 1;

    for i := 0 to L.Count - 1 do
      begin

        T := L[i];

        FLibraryRows.Add(T.TrackID);

        grdLibrary.Cells[0, i + 1] := T.Artist;
        grdLibrary.Cells[1, i + 1] := T.Title;
        //grdLibrary.Cells[2, i + 1] := T.Album;
        //grdLibrary.Cells[3, i + 1] := T.Genre;
        grdLibrary.Cells[2, i + 1] := FormatDuration(T.DurationMs);
        grdLibrary.Cells[3, i + 1] := T.QualityLabel;
        grdLibrary.Cells[4, i + 1] := T.FullPath;
      end;

    SetStatus(Format('%d track(s) found.',
                     [L.Count]));
    finally

      L.Free;
    end;
end;


procedure TfrmPlaylistEditor.RefreshPlaylistGrid();
var
  i: Integer;
  E: TRDJPlaylistEntry;

begin

  FPlaylistRows.Clear;

  grdPlaylist.RowCount := 2;
  grdPlaylist.Rows[1].Clear;

  if (FCurrentPlaylist = nil) then
    begin
      FCurrentPlaylistDurationMs := 0;
      Exit;
    end;

  if (FCurrentPlaylist.Count = 0) then
    begin

      grdPlaylist.RowCount := 2;
      FCurrentPlaylistDurationMs := 0;
      Exit;
    end;

  grdPlaylist.RowCount := FCurrentPlaylist.Count + 1;

  for i := 0 to FCurrentPlaylist.Count - 1 do
    begin

      E := FCurrentPlaylist[i];
      FPlaylistRows.Add(E.PlaylistItemID);

      grdPlaylist.Cells[0, i + 1] := IntToStr(E.SortOrder);
      grdPlaylist.Cells[1, i + 1] := E.Track.Artist;
      grdPlaylist.Cells[2, i + 1] := E.Track.Title;
      grdPlaylist.Cells[3, i + 1] := FormatDurationMs(E.Track.DurationMs);
      grdPlaylist.Cells[4, i + 1] := E.UserNote;
    end;

  FCurrentPlaylistDurationMs := CalculateCurrentPlaylistDurationMs();
end;


procedure TfrmPlaylistEditor.RefreshPlaylistsCombo();
var
  L: TList<TRDJPlaylistInfo>;
  i: Integer;
  Info: TRDJPlaylistInfo;
  CurID: Integer;

begin

  CurID := GetCurrentPlaylistIDFromCombo();

  cbPlaylists.Items.BeginUpdate;

  try

    cbPlaylists.Clear;

    L := FPlaylistMgr.GetAllPlaylists();
    try

      for i := 0 to L.Count - 1 do
        begin

          Info := L[i];
          cbPlaylists.Items.AddObject(Info.Name,
                                      TObject(Info.PlaylistID));
        end;
    finally

      L.Free;
    end;

    if (cbPlaylists.Items.Count > 0) then
      begin

        if (CurID <> 0) then
          begin

            for i := 0 to cbPlaylists.Items.Count - 1 do
              if Integer(cbPlaylists.Items.Objects[i]) = CurID then
                begin

                  cbPlaylists.ItemIndex := i;
                  Break;
                end;
          end;

        if (cbPlaylists.ItemIndex < 0) then
          begin

            cbPlaylists.ItemIndex := 0;
          end;
      end;
  finally

    cbPlaylists.Items.EndUpdate;
  end;
end;


function TfrmPlaylistEditor.GetSelectedLibraryTrackID: Integer;
var
  Row: Integer;

begin

  Result := 0;

  Row := grdLibrary.Row;

  if (Row <= 0) then
    Exit;

  if (Row - 1 < 0) or (Row - 1 >= FLibraryRows.Count) then
    Exit;

  Result := FLibraryRows[Row - 1];
end;


function TfrmPlaylistEditor.GetSelectedPlaylistIndex: Integer;
begin

  Result := grdPlaylist.Row - 1;

  if (Result < 0) then
    Result := -1;

  if (FCurrentPlaylist = nil) or (Result >= FCurrentPlaylist.Count) then
    Result := -1;
end;


function TfrmPlaylistEditor.GetCurrentPlaylistIDFromCombo: Integer;
begin

  Result := 0;

  if (cbPlaylists.ItemIndex < 0) then
    Exit;

  Result := Integer(cbPlaylists.Items.Objects[cbPlaylists.ItemIndex]);
end;


procedure TfrmPlaylistEditor.LoadPlaylistByID(const APlaylistID: Integer);
begin

  FreeAndNil(FCurrentPlaylist);
  FCurrentPlaylist := FPlaylistMgr.LoadPlaylist(APlaylistID);
  RefreshPlaylistGrid;

  if (FCurrentPlaylist <> nil) then
    UpdatePlaylistDurationStatus();
end;


procedure TfrmPlaylistEditor.mnuScanFolderClick(Sender: TObject);
var
  directory: string;

begin

  if Assigned(FScanThread) then
    begin

      ShowMessage('A scan is already running.');
      Exit;
    end;

  directory := '';

  if not SelectFolderDialog(directory) then
    Exit;

  FScanThread := TRDJLibraryScanThread.Create(FDb.DbFileName,
                                              directory,
                                              True);

  FScanThread.OnProgress := ScanThreadProgress;
  FScanThread.OnFinished := ScanThreadFinished;
  FScanThread.Start;

  SetStatus('Background scan started...');
end;


procedure TfrmPlaylistEditor.btnSearchClick(Sender: TObject);
begin

  RefreshLibraryGrid(edtSearch.Text);
end;


procedure TfrmPlaylistEditor.btnTagEditorClick(Sender: TObject);
begin

  EditSelectedLibraryTrackTags();
end;


procedure TfrmPlaylistEditor.btnCancelScanClick(Sender: TObject);
begin

  if Assigned(FScanThread) then
    FScanThread.Terminate;
end;


procedure TfrmPlaylistEditor.btnClearLibraryClick(Sender: TObject);
begin

  if MessageDlg('Clear the entire library database?' + sLineBreak +
                'This removes all tracks from the library and playlist items that reference them.',
                mtConfirmation,
                [mbYes, mbNo],
                0) <> mrYes then
    Exit;

  FLibrary.ClearLibrary(True);

  FreeAndNil(FCurrentPlaylist);

  RefreshLibraryGrid('');
  RefreshPlaylistsCombo();
  RefreshPlaylistGrid();

  SetStatus('Library cleared.');
end;


procedure TfrmPlaylistEditor.btnClearMissingTracksClick(Sender: TObject);
begin

  FLibrary.RemoveMissingTracks();

  RefreshLibraryGrid(edtSearch.Text);
  RefreshPlaylistsCombo();
  RefreshPlaylistGrid();

  SetStatus('Missing tracks removed from library.');
end;


procedure TfrmPlaylistEditor.btnClearSearchClick(Sender: TObject);
begin

  edtSearch.Clear();
  RefreshLibraryGrid('');
  edFileName.Clear();
  edFileName.Hint := '';
end;


procedure TfrmPlaylistEditor.btnNewPlaylistClick(Sender: TObject);
var
  S: string;
  ID: Integer;

begin

  S := Trim(InputBox('New Playlist',
                     'Playlist name:',
                     ''));

  if (S = '') then
    Exit;

  ID := FPlaylistMgr.CreateNewPlaylist(S);
  RefreshPlaylistsCombo();

  if (ID <> 0) then
    begin

      LoadPlaylistByID(ID);

      if (cbPlaylists.Items.Count > 0) then
        cbPlaylists.ItemIndex := cbPlaylists.Items.IndexOfObject(TObject(ID));
    end;
end;


procedure TfrmPlaylistEditor.btnOpenFileClick(Sender: TObject);
begin

  DlgLWFileBrowserEx.FileFilter := fbxAudio;
  DlgLWFileBrowserEx.ShowModal;

  if (DlgLWFileBrowserEx.ModalResult = mrOk) then
    begin

      edFileName.Text := DlgLWFileBrowserEx.FileName;
      edFileName.Hint := DlgLWFileBrowserEx.FileURI;
    end
  else
    begin

      edFileName.Text := '';
      edFileName.Hint := '';
    end;
end;


procedure TfrmPlaylistEditor.btnSavePlaylistClick(Sender: TObject);
begin

  if (FCurrentPlaylist = nil) then
    Exit;

  if FPlaylistMgr.SavePlaylist(FCurrentPlaylist) then
    begin

      RefreshPlaylistsCombo();
      SetStatus('Playlist saved.');
    end;
end;


procedure TfrmPlaylistEditor.btnScanFolderClick(Sender: TObject);
var
  directory: string;

begin

  if Assigned(FScanThread) then
    begin

      ShowMessage('A scan is already running.');
      Exit;
    end;

  directory := '';

  if not SelectFolderDialog(directory) then
    Exit;

  FScanThread := TRDJLibraryScanThread.Create(FDb.DbFileName,
                                              directory,
                                              True);

  FScanThread.OnProgress := ScanThreadProgress;
  FScanThread.OnFinished := ScanThreadFinished;
  FScanThread.Start;

  SetStatus('Background scan started...');
end;


procedure TfrmPlaylistEditor.btnDeletePlaylistClick(Sender: TObject);
var
  ID: Integer;

begin

  ID := GetCurrentPlaylistIDFromCombo();
  if (ID = 0) then
    Exit;

  if MessageDlg('Delete selected playlist?',
                mtConfirmation,
                [mbYes, mbNo],
                0) <> mrYes then
    Exit;

  FPlaylistMgr.DeletePlaylist(ID);
  FreeAndNil(FCurrentPlaylist);
  RefreshPlaylistsCombo();
  RefreshPlaylistGrid();
  SetStatus('Playlist deleted.');
end;


procedure TfrmPlaylistEditor.btnExitClick(Sender: TObject);
begin

  Close();
end;


procedure TfrmPlaylistEditor.cbPlaylistsChange(Sender: TObject);
var
  ID: Integer;

begin

  ID := GetCurrentPlaylistIDFromCombo();
  if (ID <> 0) then
    LoadPlaylistByID(ID);
end;


procedure TfrmPlaylistEditor.cbPlaylistsDblClick(Sender: TObject);
var
  P: TPoint;

begin

  if not Assigned(FCurrentPlaylist) then
    Exit;

  BuildSendToChannelPopup();

  P := Mouse.CursorPos;
  FSendToChannelPopup.Popup(P.X,
                            P.Y);
end;


procedure TfrmPlaylistEditor.btnAddToPlaylistClick(Sender: TObject);
var
  TrackID: Integer;

begin

  if (FCurrentPlaylist = nil) then
    Exit;

  TrackID := GetSelectedLibraryTrackID();
  if (TrackID = 0) then
    Exit;

  FPlaylistMgr.AddTrackToPlaylist(FCurrentPlaylist,
                                  TrackID);
  RefreshPlaylistGrid;
  lblPlayListDuration.Caption := Format('Total duration: %s.',
                                        [FormatDurationMs(FCurrentPlaylistDurationMs,
                                                          True)]);
end;


procedure TfrmPlaylistEditor.btnRemoveFromPlaylistClick(Sender: TObject);
var
  Idx: Integer;

begin

  if (FCurrentPlaylist = nil )then
    Exit;

  Idx := GetSelectedPlaylistIndex();
  if Idx < 0 then
    Exit;

  FPlaylistMgr.RemovePlaylistItem(FCurrentPlaylist,
                                  Idx);
  RefreshPlaylistGrid;
  lblPlayListDuration.Caption := Format('Total duration: %s.',
                                        [FormatDurationMs(FCurrentPlaylistDurationMs,
                                         True)]);
end;


//  Which one we should use?
//
//  Use these as two different tools:
//
//    - ClearLibrary(False)
//      Removes all tracks and playlist items, keeps library folders.
//
//    - ClearLibrary(True)
//      Removes all tracks, playlist items, and library folders.
//
//    - RemoveMissingTracks()
//      Removes only entries whose files no longer exist.

procedure TfrmPlaylistEditor.btnMoveUpClick(Sender: TObject);
var
  Idx: Integer;

begin

  if (FCurrentPlaylist = nil) then
    Exit;

  Idx := GetSelectedPlaylistIndex();
  if Idx <= 0 then
    Exit;

  FPlaylistMgr.MovePlaylistItem(FCurrentPlaylist,
                                Idx,
                                Idx - 1);
  RefreshPlaylistGrid();
  grdPlaylist.Row := Idx;
  lblPlayListDuration.Caption := Format('Total duration: %s.',
                                        [FormatDurationMs(FCurrentPlaylistDurationMs,
                                         True)]);
end;


procedure TfrmPlaylistEditor.btnMaxNormalClick(Sender: TObject);
begin

  if (WindowState = wsMaximized) then
    WindowState := wsNormal
  else
    if (WindowState = wsNormal) then
      WindowState := wsMaximized;
  Invalidate;
end;


procedure TfrmPlaylistEditor.btnMinimizeClick(Sender: TObject);
begin

   WindowState := wsMinimized;
end;


procedure TfrmPlaylistEditor.btnMoveDownClick(Sender: TObject);
var
  Idx: Integer;

begin

  if (FCurrentPlaylist = nil) then
    Exit;

  Idx := GetSelectedPlaylistIndex();
  if (Idx < 0) or (Idx >= FCurrentPlaylist.Count - 1) then
    Exit;

  FPlaylistMgr.MovePlaylistItem(FCurrentPlaylist,
                                Idx,
                                Idx + 1);
  RefreshPlaylistGrid();
  grdPlaylist.Row := Idx + 2;
  lblPlayListDuration.Caption := Format('Total duration: %s.',
                                        [FormatDurationMs(FCurrentPlaylistDurationMs,
                                         True)]);
end;


procedure TfrmPlaylistEditor.grdLibraryDblClick(Sender: TObject);
begin

  btnAddToPlaylistClick(Sender);
end;


procedure TfrmPlaylistEditor.grdLibraryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  GC: TGridCoord;
  P: TPoint;

begin

  if (Button <> mbRight) then
    Exit;

  GC := grdLibrary.MouseCoord(X,
                              Y);

  if (GC.Y > 0) and (GC.Y < grdLibrary.RowCount) then
    grdLibrary.Row := GC.Y;

  BuildLibraryPopup();

  P := grdLibrary.ClientToScreen(Point(X,
                                      Y));

  FLibraryPopup.Popup(P.X,
                      P.Y);
end;


procedure TfrmPlaylistEditor.grdLibrarySelectCell(Sender: TObject;
                                                  ACol, ARow: Integer;
                                                  var CanSelect: Boolean);
begin

  CanSelect := True;

  // Header row never editable.
  if (ARow = 0) then
    begin

      grdLibrary.Options := grdLibrary.Options - [goEditing];
      Exit;
    end;

  // Editable columns: Artist, Title, etc.
  if ACol in [0, 1{, 2, 3}] then
    grdLibrary.Options := grdLibrary.Options + [goEditing]
  else
    grdLibrary.Options := grdLibrary.Options - [goEditing];
end;


procedure TfrmPlaylistEditor.grdLibrarySetEditText(Sender: TObject;
                                                   ACol, ARow: Integer;
                                                   const Value: string);
var
  TrackID: Integer;
  Track: TRDJTrack;
  Changed: Boolean;

begin

  if (ARow <= 0) then
    Exit;

  if (ARow - 1 < 0) or (ARow - 1 >= FLibraryRows.Count) then
    Exit;

  TrackID := FLibraryRows[ARow - 1];
  if TrackID = 0 then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  Changed := False;

  case ACol of
    0:
      if (Track.Artist <> Value) then
        begin

          Track.Artist := Value;
          Changed := True;
        end;
    1:
      if (Track.Title <> Value) then
        begin

          Track.Title := Value;
          Changed := True;
        end;
    {2:
      if (Track.Album <> Value) then
        begin
          Track.Album := Value;
          Changed := True;
        end;
    3:
      if (Track.Genre <> Value) then
        begin
          Track.Genre := Value;
          Changed := True;
        end;}
  end;

  if Changed then
    begin

      RDJUpdateTrackQuality(Track);
      FLibrary.AddOrUpdateTrack(Track);
      SetStatus('Track metadata updated.');
    end;
end;



procedure TfrmPlaylistEditor.grdPlaylistMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;

begin

  if (Button <> mbRight) then
    Exit;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  BuildSendToChannelPopup();

  P := Mouse.CursorPos;
  FSendToChannelPopup.Popup(P.X,
                            P.Y);
end;


function TfrmPlaylistEditor.SelectFolderDialog(var AFolder: string): Boolean;
begin

  Result := False;

  with TFileOpenDialog.Create(nil) do
    try

      Title := 'Select Directory';
      Options := [fdoPickFolders,
                  fdoPathMustExist,
                  fdoForceFileSystem];

      OkButtonLabel := 'Select';

      if (Trim(AFolder) <> '') then
        begin

          DefaultFolder := AFolder;
          FileName := AFolder;
        end;

      if Execute then
        begin

          AFolder := FileName;
          Result := True;
        end;
    finally

      Free;
    end;
end;


procedure TfrmPlaylistEditor.BuildSendToChannelPopup();
var
  i: Integer;
  MI: TMenuItem;
  Sep: TMenuItem;

begin

  FSendToChannelPopup.Items.Clear;

  if not Assigned(MainMDIFrm) then
    Exit;

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FSendToChannelPopup);
      MI.Caption := Format('Play complete playlist on channel %d',
                           [i + 1]);
      MI.Tag := i;
      MI.OnClick := SendPlaylistToChannelClick;
      FSendToChannelPopup.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FSendToChannelPopup);
  Sep.Caption := '-';
  FSendToChannelPopup.Items.Add(Sep);

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FSendToChannelPopup);
      MI.Caption := Format('Load complete playlist on channel %d',
                           [i + 1]);
      MI.Tag := 2000 + i;
      MI.OnClick := SendPlaylistToChannelClick;
      FSendToChannelPopup.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FSendToChannelPopup);
  Sep.Caption := '-';
  FSendToChannelPopup.Items.Add(Sep);

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FSendToChannelPopup);
      MI.Caption := Format('Play selected row on channel %d',
                           [i + 1]);
      MI.Tag := 1000 + i;
      MI.OnClick := SendPlaylistToChannelClick;
      FSendToChannelPopup.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FSendToChannelPopup);
  Sep.Caption := '-';
  FSendToChannelPopup.Items.Add(Sep);

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FSendToChannelPopup);
      MI.Caption := Format('Load selected row on channel %d',
                           [i + 1]);
      MI.Tag := 3000 + i;
      MI.OnClick := SendPlaylistToChannelClick;
      FSendToChannelPopup.Items.Add(MI);
    end;
end;


function TfrmPlaylistEditor.PlaySelectedPlaylistRowOnChannel(const AChannelIndex: Integer): HRESULT;
var
  Row: Integer;
  ItemIdx: Integer;
  Deck: TfrmChannelDeck;
  Entry: TRDJPlaylistEntry;

begin

  Result := E_FAIL;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  Row := grdPlaylist.Row;
  if (Row <= 0) then
    Exit;

  ItemIdx := Row - 1;
  if (ItemIdx < 0) or (ItemIdx >= FCurrentPlaylist.Count) then
    Exit;

  Entry := FCurrentPlaylist[ItemIdx];

  //  Result := Deck.PlaySingleTrack(Entry.Track.FullPath);
  Result := Deck.LoadSingleTrack(Entry.Track.FullPath,
                                 True);

  if SUCCEEDED(Result) then
    SetStatus(Format('Selected row sent to channel %d.',
                     [AChannelIndex + 1]))
  else
    if (Result = HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)) then
      SetStatus(Format('File not found %s.',
                       [Entry.Track.FullPath]))
    else
      SetStatus(Format('Failed to send selected row to channel %d (Result: %d).',
                       [AChannelIndex + 1, Result]));
end;


procedure TfrmPlaylistEditor.pnlCaptionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;

begin

  if (Button = mbLeft) then
    begin

      ReleaseCapture();
      Perform(WM_SYSCOMMAND,
              SC_DRAGMOVE,
              0);
    end;
end;


function TfrmPlaylistEditor.LoadSelectedPlaylistRowOnChannel(const AChannelIndex: Integer): HRESULT;
var
  Row: Integer;
  ItemIdx: Integer;
  Deck: TfrmChannelDeck;
  Entry: TRDJPlaylistEntry;

begin

  Result := E_FAIL;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  Row := grdPlaylist.Row;
  if (Row <= 0) then
    Exit;

  ItemIdx := Row - 1;
  if (ItemIdx < 0) or (ItemIdx >= FCurrentPlaylist.Count) then
    Exit;

  Entry := FCurrentPlaylist[ItemIdx];

  Result := Deck.LoadSingleTrack(Entry.Track.FullPath,
                                 False);

  if SUCCEEDED(Result) then
    SetStatus(Format('Selected row loaded on channel %d.',
                     [AChannelIndex + 1]))
  else
    if (Result = HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)) then
      SetStatus(Format('File not found %s.',
                       [Entry.Track.FullPath]))
    else
      SetStatus(Format('Failed to load selected row on channel %d (Result: %d).',
                       [AChannelIndex + 1, Result]));
end;


function TfrmPlaylistEditor.PlayCurrentPlaylistOnChannel(const AChannelIndex: Integer): HRESULT;
var
  PlaylistID: Integer;
  Deck: TfrmChannelDeck;

label
  done;

begin

  Result := E_FAIL;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  PlaylistID := FCurrentPlaylist.Info.PlaylistID;
  if (PlaylistID <= 0) then
    Exit;

  Result := Deck.PlayPlaylistByID(PlaylistID);

done:

  if SUCCEEDED(Result) then
    SetStatus(Format('Playlist "%s" sent to channel %d.',
                     [FCurrentPlaylist.Info.Name,
                      AChannelIndex + 1]))
  else
    if (Result = ERROR_FILE_NOT_FOUND) then
      SetStatus(Format('Failed to send playlist "%s" to channel %d :File not found.',
                       [FCurrentPlaylist.Info.Name,
                        AChannelIndex + 1]))
    else
      SetStatus(Format('Failed to send playlist "%s" to channel %d.',
                       [FCurrentPlaylist.Info.Name,
                        AChannelIndex + 1]));

end;


function TfrmPlaylistEditor.LoadCurrentPlaylistOnChannel(const AChannelIndex: Integer): HRESULT;
var
  PlaylistID: Integer;
  Deck: TfrmChannelDeck;

begin

  Result := E_FAIL;

  if not Assigned(FCurrentPlaylist) then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  PlaylistID := FCurrentPlaylist.Info.PlaylistID;
  if (PlaylistID <= 0) then
    Exit;

  Result := Deck.LoadPlaylistByID(PlaylistID);

  if SUCCEEDED(Result) then
    SetStatus(Format('Playlist "%s" loaded on channel %d.',
                     [FCurrentPlaylist.Info.Name,
                      AChannelIndex + 1]))
  else
    if (Result = HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)) then
      SetStatus(Format('Failed to load playlist "%s" on channel %d: file not found.',
                       [FCurrentPlaylist.Info.Name,
                        AChannelIndex + 1]))
    else
      SetStatus(Format('Failed to load playlist "%s" on channel %d.',
                       [FCurrentPlaylist.Info.Name,
                        AChannelIndex + 1]));

end;


function TfrmPlaylistEditor.EditSelectedLibraryTrackTags(): Boolean;
var
  TrackID: Integer;
  Track: TRDJTrack;

begin

  Result := False;

  TrackID := GetSelectedLibraryTrackID();
  if (TrackID = 0) then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  if not TfrmTagEditor.Execute(Track) then
    Exit;

  RDJUpdateTrackQuality(Track);
  FLibrary.AddOrUpdateTrack(Track);

  RefreshLibraryGrid(edtSearch.Text);
  SetStatus('Track tags updated.');

  Result := True;

  // TODO:
  // RDJ.TagWriter.WriteTags(Track);

end;


procedure TfrmPlaylistEditor.SendPlaylistToChannelClick(Sender: TObject);
var
  ChIdx: Integer;
  TagValue: Integer;

begin

  if not (Sender is TMenuItem) then
    Exit;

  TagValue := TMenuItem(Sender).Tag;

  if (TagValue >= 3000) then
    begin

      ChIdx := TagValue - 3000;
      LoadSelectedPlaylistRowOnChannel(ChIdx);
      Exit;
    end;

  if (TagValue >= 2000) then
    begin

      ChIdx := TagValue - 2000;
      LoadCurrentPlaylistOnChannel(ChIdx);
      Exit;
    end;

  if (TagValue >= 1000) then
    begin

      ChIdx := TagValue - 1000;
      PlaySelectedPlaylistRowOnChannel(ChIdx);
    end
  else
    begin

      ChIdx := TagValue;
      PlayCurrentPlaylistOnChannel(ChIdx);
    end;
end;


procedure TfrmPlaylistEditor.CMDialogKey(var Message: TCMDialogKey);
begin

  if (ActiveControl = edtSearch) and (Message.CharCode = VK_RETURN) then
    begin

      btnSearch.Click;
      Message.Result := 1;
      Exit;
    end;

  inherited;
end;


procedure TfrmPlaylistEditor.BuildLoadedFilePopup();
var
  i: Integer;
  MI: TMenuItem;
  Sep: TMenuItem;

begin

  FLoadFilePopUp.Items.Clear;

  MI := TMenuItem.Create(FLoadFilePopUp);
  MI.Caption := 'Add file to current playlist';
  MI.Tag := 10;
  MI.OnClick := LoadedFilePopupClick;
  FLoadFilePopUp.Items.Add(MI);

  Sep := TMenuItem.Create(FLoadFilePopUp);
  Sep.Caption := '-';
  FLoadFilePopUp.Items.Add(Sep);

  if not Assigned(MainMDIFrm) then
    Exit;

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FLoadFilePopUp);
      MI.Caption := Format('Load file on channel %d',
                           [i + 1]);
      MI.Tag := 1000 + i;
      MI.OnClick := LoadedFilePopupClick;
      FLoadFilePopUp.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FLoadFilePopUp);
  Sep.Caption := '-';
  FLoadFilePopUp.Items.Add(Sep);

  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin
      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FLoadFilePopUp);
      MI.Caption := Format('Play file on channel %d',
                           [i + 1]);
      MI.Tag := 2000 + i;
      MI.OnClick := LoadedFilePopupClick;
      FLoadFilePopUp.Items.Add(MI);
    end;
end;


procedure TfrmPlaylistEditor.edFileNameMouseDown(Sender: TObject;
                                                 Button: TMouseButton;
                                                 Shift: TShiftState;
                                                 X, Y: Integer);
var
  P: TPoint;
  FileName: string;

begin

  if (Button <> mbRight) then
    Exit;

  if not GetLoadedFileNameFromEdit(FileName) then
    Exit;

  BuildLoadedFilePopup();

  P := edFileName.ClientToScreen(Point(X,
                                       Y));
  FLoadFilePopUp.Popup(P.X,
                       P.Y);
end;


procedure TfrmPlaylistEditor.LoadedFilePopupClick(Sender: TObject);
var
  ChIdx: Integer;
  TagValue: Integer;

begin

  if not (Sender is TMenuItem) then
    Exit;

  TagValue := TMenuItem(Sender).Tag;

  if (TagValue = 10) then
    begin

      AddLoadedFileToCurrentPlaylist();
      Exit;
    end;

  if (TagValue >= 2000) then
    begin

      ChIdx := TagValue - 2000;
      LoadLoadedFileOnChannel(ChIdx,
                              True);
    end
  else
    if (TagValue >= 1000) then
      begin

        ChIdx := TagValue - 1000;
        LoadLoadedFileOnChannel(ChIdx,
                                False);
      end;
end;


function TfrmPlaylistEditor.GetLoadedFileNameFromEdit(out AFileName: string): Boolean;
begin

  AFileName := Trim(edFileName.Hint);

  if (AFileName = '') or not FileExists(AFileName) then
    AFileName := Trim(edFileName.Text);

  Result := (AFileName <> '') and FileExists(AFileName);

  if not Result then
    SetStatus('No valid file selected.');
end;


function TfrmPlaylistEditor.FindLibraryTrackByFullPath(const AFileName: string;
                                                       out ATrack: TRDJTrack): Boolean;
var
  Artist: string;
  Title: string;

  function TrySearch(const AText: string): Boolean;
  var
    L: TList<TRDJTrack>;
    I: Integer;
  begin

    Result := False;

    if Trim(AText) = '' then
      Exit;

    L := FLibrary.SearchTrackSummaries(AText);
    try

      for I := 0 to L.Count - 1 do
        begin

          if SameText(ExpandFileName(L[I].FullPath),
                      ExpandFileName(AFileName)) then
            begin

              ATrack := L[I];
              Exit(True);
            end;
        end;
    finally

      L.Free;
    end;
  end;

begin

  ATrack := Default(TRDJTrack);

  Result := TrySearch(AFileName);
  if Result then
    Exit;

  Result := TrySearch(ExtractFileName(AFileName));
  if Result then
    Exit;

  TFileNameParser.ResolveArtistTitle(AFileName,
                                     '',
                                     '',
                                     Artist,
                                     Title);

  Result := TrySearch(Artist + ' ' + Title);
end;



function TfrmPlaylistEditor.ReadAudioDurationMs(const AFileName: string): Int64;
var
  AliasName: string;
  Cmd: string;
  Buffer: array[0..63] of Char;
  Err: MCIERROR;

begin

  Result := 0;

  if (Trim(AFileName) = '') or not FileExists(AFileName) then
    Exit;

  AliasName := 'RDJDuration' + IntToHex(GetTickCount(), 8);

  Cmd := Format('open "%s" alias %s',
                [AFileName,
                 AliasName]);

  Err := mciSendString(PChar(Cmd),
                       nil,
                       0,
                       0);
  if (Err <> 0) then
    Exit;

  try

    mciSendString(PChar('set ' + AliasName + ' time format milliseconds'),
                  nil,
                  0,
                  0);

    FillChar(Buffer,
             SizeOf(Buffer),
             0);

    Cmd := 'status ' + AliasName + ' length';

    Err := mciSendString(PChar(Cmd),
                         Buffer,
                         Length(Buffer),
                         0);
    if (Err = 0) then
      Result := StrToInt64Def(Trim(string(Buffer)),
                              0);
  finally

    mciSendString(PChar('close ' + AliasName),
                  nil,
                  0,
                  0);
  end;
end;


function TfrmPlaylistEditor.AddLoadedFileToCurrentPlaylist(): Boolean;
var
  FileName: string;
  Track: TRDJTrack;
  Artist: string;
  Title: string;
  OldCount: Integer;
  NewIndex: Integer;

begin

  Result := False;

  if (FCurrentPlaylist = nil) then
    begin

      SetStatus('No playlist selected.');
      Exit;
    end;

  if not GetLoadedFileNameFromEdit(FileName) then
    Exit;

  if not FindLibraryTrackByFullPath(FileName,
                                    Track) then
    begin

      TFileNameParser.ResolveArtistTitle(FileName,
                                         '',
                                         '',
                                         Artist,
                                         Title);

      Track := Default(TRDJTrack);
      Track.FullPath := FileName;
      Track.Artist := Trim(Artist);
      Track.Title := Trim(Title);
      Track.Album := '';
      Track.Genre := '';
      Track.DurationMs := ReadAudioDurationMs(FileName);

      if (Track.Artist = '') then
        Track.Artist := 'Unknown Artist';

      if (Track.Title = '') then
        Track.Title := ChangeFileExt(ExtractFileName(FileName),
                                     '');

      RDJUpdateTrackQuality(Track);
      FLibrary.AddOrUpdateTrack(Track);

      // AddOrUpdateTrack may not update the TrackID in the local record, so
      // read the track back from the library before adding it to the playlist.
      FindLibraryTrackByFullPath(FileName,
                                 Track);
    end;

  if (Track.TrackID = 0) then
    begin

      SetStatus('Could not add selected file to the library.');
      Exit;
    end;

  // Existing library entries can still have DurationMs = 0. Fill it once, then
  // update the library before the playlist entry is created.
  if (Track.DurationMs <= 0) then
    begin

      Track.DurationMs := ReadAudioDurationMs(FileName);
      if (Track.DurationMs > 0) then
        FLibrary.AddOrUpdateTrack(Track);
    end;

  OldCount := FCurrentPlaylist.Count;

  FPlaylistMgr.AddTrackToPlaylist(FCurrentPlaylist,
                                  Track.TrackID);

  // Important: do NOT reload the playlist here. AddTrackToPlaylist updates the
  // in-memory playlist; reloading can drop an unsaved appended item and makes it
  // look as if nothing was added.
  if (FCurrentPlaylist.Count <= OldCount) then
    begin

      SetStatus('Could not append selected file to the playlist.');
      Exit;
    end;

  NewIndex := FCurrentPlaylist.Count - 1;

  RefreshPlaylistGrid();
  UpdatePlaylistDurationStatus();

  // The grid is refreshed from the playlist entry. Correct only the new appended
  // visible row, never the currently selected row.
  if (grdPlaylist.RowCount > NewIndex + 1) then
    begin

      grdPlaylist.Cells[1, NewIndex + 1] := Track.Artist;
      grdPlaylist.Cells[2, NewIndex + 1] := Track.Title;
      grdPlaylist.Cells[3, NewIndex + 1] := FormatDurationMs(Track.DurationMs);
    end;

  SetStatus(Format('Added "%s - %s" to playlist "%s".',
                   [Track.Artist,
                    Track.Title,
                    FCurrentPlaylist.Info.Name]));

  Result := True;
end;

function TfrmPlaylistEditor.LoadLoadedFileOnChannel(const AChannelIndex: Integer;
  const AStartPlayback: Boolean): HRESULT;
var
  Deck: TfrmChannelDeck;
  FileName: string;
  DisplayName: string;

begin

  Result := E_FAIL;

  if not Assigned(MainMDIFrm) then
    Exit;

  if not GetLoadedFileNameFromEdit(FileName) then
    begin

      Result := HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND);
      Exit;
    end;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  DisplayName := Trim(edFileName.Text);
  if (DisplayName = '') then
    DisplayName := ExtractFileName(FileName);

  Result := Deck.LoadSingleTrack(FileName,
                                 AStartPlayback);

  if SUCCEEDED(Result) then
    begin

      if AStartPlayback then
        SetStatus(Format('Playing "%s" on channel %d.',
                         [DisplayName, AChannelIndex + 1]))
      else
        SetStatus(Format('Loaded "%s" on channel %d.',
                         [DisplayName, AChannelIndex + 1]));
    end
  else
    if (Result = HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)) then
      SetStatus(Format('Failed to open "%s" on channel %d: file not found.',
                       [DisplayName, AChannelIndex + 1]))
    else
      SetStatus(Format('Failed to open "%s" on channel %d.',
                       [DisplayName, AChannelIndex + 1]));
end;


procedure TfrmPlaylistEditor.BuildLibraryPopup();
var
  i: Integer;
  MI: TMenuItem;
  Sep: TMenuItem;

begin

  FLibraryPopup.Items.Clear;

  if not Assigned(MainMDIFrm) then
    Exit;

  // Prelisten targets.
  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FLibraryPopup);
      MI.Caption := Format('Prelisten on channel %d',
                           [i + 1]);
      MI.Tag := i;
      MI.OnClick := LibraryPopupPrelistenClick;
      FLibraryPopup.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FLibraryPopup);
  Sep.Caption := '-';
  FLibraryPopup.Items.Add(Sep);

  // Load-only targets.
  for i := 0 to MainMDIFrm.GetChannelDeckCount() - 1 do
    begin

      if not Assigned(MainMDIFrm.GetChannelDeck(i)) then
        Continue;

      MI := TMenuItem.Create(FLibraryPopup);
      MI.Caption := Format('Load on channel %d',
                           [i + 1]);
      MI.Tag := 1000 + i;
      MI.OnClick := LibraryPopupLoadToChannelClick;
      FLibraryPopup.Items.Add(MI);
    end;

  Sep := TMenuItem.Create(FLibraryPopup);
  Sep.Caption := '-';
  FLibraryPopup.Items.Add(Sep);

  MI := TMenuItem.Create(FLibraryPopup);
  MI.Caption := 'Add to current playlist';
  MI.OnClick := LibraryPopupAddToPlaylistClick;
  FLibraryPopup.Items.Add(MI);

  MI := TMenuItem.Create(FLibraryPopup);
  MI.Caption := 'Edit tags';
  MI.OnClick := LibraryPopupEditTagsClick;
  FLibraryPopup.Items.Add(MI);

  MI := TMenuItem.Create(FLibraryPopup);
  MI.Caption := 'Open file location';
  MI.OnClick := LibraryPopupOpenFileLocationClick;
  FLibraryPopup.Items.Add(MI);

  Sep := TMenuItem.Create(FLibraryPopup);
  Sep.Caption := '-';
  FLibraryPopup.Items.Add(Sep);

  MI := TMenuItem.Create(FLibraryPopup);
  MI.Caption := 'Remove from library';
  MI.OnClick := LibraryPopupRemoveFromLibraryClick;
  FLibraryPopup.Items.Add(MI);
end;


procedure TfrmPlaylistEditor.LibraryPopupPrelistenClick(Sender: TObject);
var
  ChIdx: Integer;

begin

  if not (Sender is TMenuItem) then
    Exit;

  ChIdx := TMenuItem(Sender).Tag;
  PrelistenSelectedLibraryTrackOnChannel(ChIdx);
end;


procedure TfrmPlaylistEditor.LibraryPopupLoadToChannelClick(Sender: TObject);
var
  ChIdx: Integer;

begin

  if not (Sender is TMenuItem) then
    Exit;

  ChIdx := TMenuItem(Sender).Tag - 1000;
  LoadSelectedLibraryTrackOnChannel(ChIdx);
end;


procedure TfrmPlaylistEditor.LibraryPopupAddToPlaylistClick(Sender: TObject);
begin

  btnAddToPlaylistClick(Sender);
end;


procedure TfrmPlaylistEditor.LibraryPopupEditTagsClick(Sender: TObject);
begin

  EditSelectedLibraryTrackTags();
end;


procedure TfrmPlaylistEditor.LibraryPopupOpenFileLocationClick(Sender: TObject);
var
  TrackID: Integer;
  Track: TRDJTrack;
  Param: string;

begin

  TrackID := GetSelectedLibraryTrackID();
  if TrackID = 0 then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  if not FileExists(Track.FullPath) then
    begin

      SetStatus('File does not exist anymore.');
      Exit;
    end;

  Param := '/select,"' + Track.FullPath + '"';

  ShellExecute(Handle,
               'open',
               'explorer.exe',
               PChar(Param),
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmPlaylistEditor.LibraryPopupRemoveFromLibraryClick(Sender: TObject);
begin

  RemoveSelectedLibraryTrack();
end;


function TfrmPlaylistEditor.RemoveSelectedLibraryTrack(): Boolean;
var
  TrackID: Integer;
  Track: TRDJTrack;

begin

  Result := False;

  TrackID := GetSelectedLibraryTrackID();
  if (TrackID = 0) then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  if MessageDlg(Format('Remove "%s" from the library?',
                       [Track.Title]),
                mtConfirmation,
                [mbYes, mbNo],
                0) <> mrYes then
    Exit;

  FLibrary.DeleteTrackByID(TrackID);

  RefreshLibraryGrid(edtSearch.Text);
  RefreshPlaylistGrid();
  RefreshPlaylistsCombo();

  SetStatus('Track removed from library.');
  Result := True;
end;


function TfrmPlaylistEditor.LoadSelectedLibraryTrackOnChannel(const AChannelIndex: Integer): HRESULT;
var
  TrackID: Integer;
  Track: TRDJTrack;
  Deck: TfrmChannelDeck;
  DisplayName: string;

begin

  Result := E_FAIL;

  if not Assigned(MainMDIFrm) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  TrackID := GetSelectedLibraryTrackID();
  if (TrackID = 0) then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  Result := Deck.LoadSingleTrack(Track.FullPath);

  if (Track.Title <> '') then
    DisplayName := Track.Title
  else
    DisplayName := ExtractFileName(Track.FullPath);

  if SUCCEEDED(Result) then
    SetStatus(Format('Loaded "%s" on channel %d.',
                     [DisplayName,
                      AChannelIndex + 1]))
  else
    if (Result = ERROR_FILE_NOT_FOUND) then
      SetStatus(Format('Failed to load selected track on channel %d: file not found.',
                       [AChannelIndex + 1]))
    else
      SetStatus(Format('Failed to load selected track on channel %d.',
                       [AChannelIndex + 1]));
end;


function TfrmPlaylistEditor.PrelistenSelectedLibraryTrackOnChannel(const AChannelIndex: Integer): HRESULT;
var
  TrackID: Integer;
  Track: TRDJTrack;
  Deck: TfrmChannelDeck;
  DisplayName: string;

begin

  Result := E_FAIL;

  if not Assigned(MainMDIFrm) then
    Exit;

  Deck := MainMDIFrm.GetChannelDeck(AChannelIndex);
  if not Assigned(Deck) then
    Exit;

  TrackID := GetSelectedLibraryTrackID();
  if (TrackID = 0) then
    Exit;

  if not FLibrary.FindTrackByID(TrackID,
                                Track) then
    Exit;

  //Result := Deck.PlaySingleTrack(Track.FullPath);
  Result := Deck.LoadSingleTrack(Track.FullPath,
                                 True);

  if (Track.Title <> '') then
    DisplayName := Track.Title
  else
    DisplayName := ExtractFileName(Track.FullPath);

  if SUCCEEDED(Result) then
    SetStatus(Format('Prelistening "%s" on channel %d.',
                     [DisplayName,
                      AChannelIndex + 1]))
  else
    if (Result = ERROR_FILE_NOT_FOUND) then
      SetStatus(Format('Failed to prelisten selected track on channel %d.',
                       [ExtractFileName(Track.FullPath)]))
    else
      SetStatus(Format('Failed to prelisten selected track on channel %d.',
                       [AChannelIndex + 1]));
end;

end.