// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMfPlaylist.pas
// Kind: Pascal / Delphi unit
// Release date: 26-11-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: This sample is based on Microsoft's "Sequencer Source Example",
//              translated to a VCL interface for the sequencer-source playlist walkthrough.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX), Carmen (Carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/sequencer-source-example-code
//
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
unit frmMfPlaylist;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Menus,
  {Application}
  RDJ.FilenameParser,
  MfArtworkCaption,
  MfPlaylistFile,
  MfPlaylistPlayer;

type

  TMfPlaylistForm = class(TForm)
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblIntroduction: TLabel;
    pnlTransport: TPanel;
    btnBuild: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnPlaySelected: TButton;
    btnRelease: TButton;
    lblState: TLabel;
    pnlTime: TPanel;
    lblCurrent: TLabel;
    lblTime: TLabel;
    prbSegment: TProgressBar;
    pnlLog: TPanel;
    lblLog: TLabel;
    memLog: TMemo;
    pnlContent: TPanel;
    pnlPlaylist: TPanel;
    lblPlaylist: TLabel;
    lstPlaylist: TListBox;
    pnlEdit: TPanel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnClear: TButton;
    btnUp: TButton;
    btnDown: TButton;
    Splitter: TSplitter;
    pnlPreview: TPanel;
    lblPreview: TLabel;
    pnlVideo: TPanel;
    pnlArtwork: TPanel;
    imgArtwork: TImage;
    pmProgress: TPopupMenu;
    mnuPlayFromHere: TMenuItem;
    OpenDialog: TOpenDialog;
    PositionTimer: TTimer;
    PlaylistMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuNewPlaylist: TMenuItem;
    mnuOpenPlaylist: TMenuItem;
    mnuSavePlaylist: TMenuItem;
    mnuSavePlaylistAs: TMenuItem;
    OpenPlaylistDialog: TOpenDialog;
    SavePlaylistDialog: TSaveDialog;
    procedure NewPlaylistClick(Sender: TObject);
    procedure OpenPlaylistClick(Sender: TObject);
    procedure SavePlaylistClick(Sender: TObject);
    procedure SavePlaylistAsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject;
                             var CanClose: Boolean);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    procedure BuildClick(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure PauseClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure PlaySelectedClick(Sender: TObject);
    procedure ReleaseClick(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure PositionTimerTimer(Sender: TObject);
    procedure PlayFromHereClick(Sender: TObject);
    procedure ProgressMouseDown(Sender: TObject;
                                Button: TMouseButton;
                                Shift: TShiftState;
                                X: Integer;
                                Y: Integer);
    procedure ProgressContextPopup(Sender: TObject;
                                   MousePos: TPoint;
                                   var Handled: Boolean);

  private
    FPlayer: TMfPlaylistPlayer;
    FFiles: TStringList;
    FPlaylistFileName: string;
    FPlaylistModified: Boolean;
    FClosing: Boolean;
    FNotifyWindow: HWND;
    FVideoWindowProc: TWndMethod;
    FPreviewIndex: Integer;
    FArtworkFileName: string;
    FUsingDefaultArtwork: Boolean;
    FArtworkArtist: string;
    FArtworkTitle: string;
    FArtworkCaptionRendered: Boolean;
    FSeekIndex: Integer;
    FSeekOffset: Int64;

    procedure PlaylistChanged(Sender: TObject);
    procedure UpdatePlaylistTitle();
    function ConfirmPlaylistChanges(): Boolean;
    function SaveCurrentPlaylist(const ASaveAs: Boolean): Boolean;
    procedure PlaylistWindowProc(var Message: TMessage);
    procedure VideoWindowProc(var Message: TMessage);
    function PrepareProgressPosition(const APoint: TPoint): Boolean;
    procedure UpdatePreview();
    procedure LoadDefaultArtwork();
    procedure LoadArtwork(const AMediaFile: string);
    procedure Log(const AText: string);

    procedure Report(const AOperation: string;
                     const AStatus: HRESULT);

    procedure UpdateControls();
    procedure RefreshPlaylist(const ASelection: Integer);
    procedure UpdateTime();
    procedure MoveItem(const AOffset: Integer);
    procedure DrainEvents();

  public

    procedure AddFiles(const AFiles: TStrings);
    procedure LoadPlaylistFile(const AFileName: string);
    procedure SavePlaylistFile(const AFileName: string);
    property PlaylistFileName: string read FPlaylistFileName;
    property PlaylistModified: Boolean read FPlaylistModified;

    property Player: TMfPlaylistPlayer read FPlayer;
    property ArtworkArtist: string read FArtworkArtist;
    property ArtworkTitle: string read FArtworkTitle;
    property ArtworkCaptionRendered: Boolean read FArtworkCaptionRendered;
    property UsingDefaultArtwork: Boolean read FUsingDefaultArtwork;
    property ArtworkFileName: string read FArtworkFileName;
  end;

var
  MfPlaylistForm: TMfPlaylistForm;


implementation

uses

  {Vcl}
  Vcl.Graphics,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage;


{$R *.dfm}

procedure TMfPlaylistForm.UpdatePlaylistTitle();
var
  Name: string;

begin

  if FPlaylistFileName = '' then
    Name := 'Untitled'
  else
    Name := ChangeFileExt(ExtractFileName(FPlaylistFileName),
                          '');
  if FPlaylistModified then
    Name := Name + ' *';
  Caption := 'MfPlayList2 - ' + Name;
end;


procedure TMfPlaylistForm.PlaylistChanged(Sender: TObject);

begin

  FPlaylistModified := True;
  UpdatePlaylistTitle();
end;


procedure TMfPlaylistForm.LoadPlaylistFile(const AFileName: string);
var
  Entries: TStringList;
  I, Missing: Integer;

begin

  if FPlayer.HasPlaylist then
    raise Exception.Create('Release the current playback sequence before opening another playlist.');
  Entries := TStringList.Create();
  try
    LoadM3u8(AFileName,
             Entries);
    FFiles.Assign(Entries);
    FPlaylistFileName := ExpandFileName(AFileName);
    FPlaylistModified := False;
    RefreshPlaylist(0);
    UpdatePlaylistTitle();
    UpdateControls();
    Missing := 0;
    for I := 0 to FFiles.Count - 1 do
      if not FileExists(FFiles[I]) then
        begin

          Inc(Missing);
          Log('Missing media: ' + FFiles[I]);
        end;
    Log(Format('Opened %s (%d entries; %d missing). Click Build playlist to prepare playback.',
               [FPlaylistFileName, FFiles.Count, Missing]));
  finally
    Entries.Free();
  end;
end;


procedure TMfPlaylistForm.SavePlaylistFile(const AFileName: string);

begin

  SaveM3u8(AFileName,
           FFiles);
  FPlaylistFileName := ExpandFileName(AFileName);
  FPlaylistModified := False;
  UpdatePlaylistTitle();
  Log('Saved playlist: ' + FPlaylistFileName);
end;


function TMfPlaylistForm.SaveCurrentPlaylist(const ASaveAs: Boolean): Boolean;
var
  FileName: string;

begin

  Result := False;
  FileName := FPlaylistFileName;
  if ASaveAs or (FileName = '') then
    begin

      SavePlaylistDialog.FileName := FileName;
      if not SavePlaylistDialog.Execute() then
        Exit;
      FileName := SavePlaylistDialog.FileName;
    end;
  try
    SavePlaylistFile(FileName);
    Result := True;
  except
    on E: Exception do
      MessageDlg('Could not save the playlist: ' + E.Message,
                 mtError,
                 [mbOK],
                 0);
  end;
end;


function TMfPlaylistForm.ConfirmPlaylistChanges(): Boolean;

begin

  Result := True;
  if not FPlaylistModified then
    Exit;
  case MessageDlg('Save changes to this playlist before continuing?',
                  mtConfirmation,
                  [mbYes, mbNo, mbCancel],
                  0) of
    mrYes: Result := SaveCurrentPlaylist(False);
    mrNo: Result := True;
    else
      Result := False;
  end;
end;


procedure TMfPlaylistForm.NewPlaylistClick(Sender: TObject);

begin

  if FPlayer.HasPlaylist or not ConfirmPlaylistChanges() then
    Exit;
  FFiles.Clear();
  FPlaylistFileName := '';
  FPlaylistModified := False;
  RefreshPlaylist(-1);
  UpdatePlaylistTitle();
  UpdateControls();
end;


procedure TMfPlaylistForm.OpenPlaylistClick(Sender: TObject);

begin

  if FPlayer.HasPlaylist then
    Exit;
  if not OpenPlaylistDialog.Execute() then
    Exit;
  if not ConfirmPlaylistChanges() then
    Exit;
  try
    LoadPlaylistFile(OpenPlaylistDialog.FileName);
  except
    on E: Exception do
      MessageDlg('Could not open the playlist: ' + E.Message,
                 mtError,
                 [mbOK],
                 0);
  end;
end;


procedure TMfPlaylistForm.SavePlaylistClick(Sender: TObject);

begin

  SaveCurrentPlaylist(False);
end;


procedure TMfPlaylistForm.SavePlaylistAsClick(Sender: TObject);

begin

  SaveCurrentPlaylist(True);
end;


procedure TMfPlaylistForm.FormCloseQuery(Sender: TObject;
                                         var CanClose: Boolean);

begin

  CanClose := ConfirmPlaylistChanges();
end;


function TimeText(const ATime100ns: Int64): string;
var
  Seconds: Int64;

begin

  Seconds := ATime100ns div 10000000;
  if (Seconds < 0) then
    Seconds := 0;

  Result := Format('%.2d:%.2d:%.2d',
                   [Seconds div 3600, (Seconds div 60) mod 60, Seconds mod 60]);
end;


procedure TMfPlaylistForm.FormCreate(Sender: TObject);

begin

  FFiles := TStringList.Create();
  FFiles.OnChange := PlaylistChanged;
  UpdatePlaylistTitle();
  FPreviewIndex := -2;
  FSeekIndex := -1;
  HandleNeeded();
  pnlVideo.HandleNeeded();
  FVideoWindowProc := pnlVideo.WindowProc;
  pnlVideo.WindowProc := VideoWindowProc;
  // A dedicated window survives any recreation of the main form's HWND.
  FNotifyWindow := AllocateHWnd(PlaylistWindowProc);
  FPlayer := TMfPlaylistPlayer.Create(FNotifyWindow,
                                      pnlVideo.Handle);
  FPlayer.OnLog := Log;
  Log('Use File > Open playlist to restore an M3U8, or add files and save a named playlist.');
  Log('1. Add files and arrange their order.');
  Log('2. Build playlist: create and append partial topologies to the sequencer.');
  Log('3. Play: MENewPresentation queues following segments before they are needed.');
  Log('Release the playlist before editing its file list.');
  PositionTimer.Enabled := True;
  UpdateControls();
end;


procedure TMfPlaylistForm.DrainEvents();
var
  Message: TMsg;

begin

  while PeekMessage(Message,
      FNotifyWindow,
      WM_MFPLAYLIST_EVENT,
      WM_MFPLAYLIST_EVENT,
      PM_REMOVE) do
    TMfPlaylistEventPacket(Message.LParam).Free();
end;


procedure TMfPlaylistForm.FormDestroy(Sender: TObject);

begin

  FClosing := True;
  PositionTimer.Enabled := False;
  if Assigned(FVideoWindowProc) then
    pnlVideo.WindowProc := FVideoWindowProc;

  if Assigned(FPlayer) then
    FPlayer.OnLog := nil;
  FreeAndNil(FPlayer);
  DrainEvents();
  if (FNotifyWindow <> 0) then
    DeallocateHWnd(FNotifyWindow);
  FNotifyWindow := 0;
  FreeAndNil(FFiles);
end;


procedure TMfPlaylistForm.VideoWindowProc(var Message: TMessage);

begin

  FVideoWindowProc(Message);

  if Assigned(FPlayer) and (not FClosing) then
    case Message.Msg of
      WM_PAINT: FPlayer.RepaintVideo();
      WM_SIZE: FPlayer.ResizeVideo();
    end;
end;


procedure TMfPlaylistForm.Log(const AText: string);

begin

  if FClosing then
    Exit;

  memLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz',
                                  Now()) + '  ' + AText);

  while (memLog.Lines.Count > 500) do
    memLog.Lines.Delete(0);
end;


procedure TMfPlaylistForm.Report(const AOperation: string;
                                 const AStatus: HRESULT);

begin

  if FAILED(AStatus) then
    Log(Format('%s failed (HRESULT $%.8x). %s',
               [AOperation, DWORD(AStatus), FPlayer.LastError]));

  UpdateControls();
end;


procedure TMfPlaylistForm.AddFiles(const AFiles: TStrings);
var
  I: Integer;

begin

  if FPlayer.HasPlaylist or (AFiles = nil) then
    Exit;

  for I := 0 to AFiles.Count - 1 do
    FFiles.Add(ExpandFileName(AFiles[I]));

  RefreshPlaylist(FFiles.Count - 1);

  UpdateControls();
end;


procedure TMfPlaylistForm.RefreshPlaylist(const ASelection: Integer);
var
  I: Integer;
  Text: string;

begin

  lstPlaylist.Items.BeginUpdate();
  try
    lstPlaylist.Clear();

    for I := 0 to FFiles.Count - 1 do
      begin

        Text := Format('%d. %s',
                       [I + 1, ExtractFileName(FFiles[I])]);
        if FPlayer.HasPlaylist and (I < FPlayer.Count) then
          Text := Text + Format('  [segment %d]',
                                [FPlayer.SegmentId(I)]);
        lstPlaylist.Items.Add(Text);
      end;

    if (ASelection >= 0) and (ASelection < lstPlaylist.Count) then
      lstPlaylist.ItemIndex := ASelection;

  finally
    lstPlaylist.Items.EndUpdate();
  end;
end;


procedure TMfPlaylistForm.UpdateControls();
var
  Editable: Boolean;
  Built: Boolean;
  Index: Integer;
  State: TMfPlaylistState;

begin

  if not Assigned(FPlayer) then
    Exit;

  Built := FPlayer.HasPlaylist;
  Editable := not Built;
  mnuNewPlaylist.Enabled := Editable;
  mnuOpenPlaylist.Enabled := Editable;
  Index := lstPlaylist.ItemIndex;
  State := FPlayer.State;
  btnAdd.Enabled := Editable;
  btnRemove.Enabled := Editable and (Index >= 0);
  btnClear.Enabled := Editable and (FFiles.Count > 0);
  btnUp.Enabled := Editable and (Index > 0);
  btnDown.Enabled := Editable and (Index >= 0) and (Index < FFiles.Count - 1);
  btnBuild.Enabled := Editable and (FFiles.Count > 0);
  btnRelease.Enabled := Built;
  btnPlay.Enabled := Built and (State in [plsReady, plsPaused, plsStopped, plsEnded]);

  if (State = plsPaused) then
    btnPlay.Caption := 'Resume'
  else
    btnPlay.Caption := 'Play';

  btnPause.Enabled := Built and (State = plsPlaying);
  btnStop.Enabled := Built and (State in [plsStarting, plsPlaying, plsPaused]);
  btnPlaySelected.Enabled := Built and (Index >= 0) and
    (State in [plsReady, plsPlaying, plsPaused, plsStopped, plsEnded]);
  lblState.Caption := 'State: ' + MfPlaylistStateText(State);

  if (Index >= 0) and (Index < FFiles.Count) then
    lstPlaylist.Hint := FFiles[Index]
  else
    lstPlaylist.Hint := '';

  UpdateTime();
  UpdatePreview();
end;


procedure TMfPlaylistForm.UpdateTime();
var
  Presentation: Int64;
  Position: Int64;
  Duration: Int64;
  Index: Integer;

begin

  if not Assigned(FPlayer) then
    Exit;

  Index := FPlayer.CurrentIndex;

  if (Index >= 0) and (Index < FPlayer.Count) then
    lblCurrent.Caption := Format('Item %d of %d: %s',
                                 [Index + 1, FPlayer.Count,
      ExtractFileName(FPlayer.SegmentFileName(Index))])
  else
    lblCurrent.Caption := 'No segment is playing';

  if (FPlayer.GetPlaybackTime(Presentation,
    Position,
    Duration) = S_OK) then
    begin

      lblTime.Caption := Format('Segment %s / %s     Presentation clock %s',
                                [TimeText(Position), TimeText(Duration), TimeText(Presentation)]);
      if (Duration > 0) then
        begin

          if (Position > Duration) then
            Position := Duration;

          prbSegment.Position := Round((Position / Duration) * prbSegment.Max);
        end
      else
        prbSegment.Position := 0;
    end
  else
    begin

      lblTime.Caption := 'Segment 00:00:00 / 00:00:00     Presentation clock 00:00:00';
      prbSegment.Position := 0;
    end;
end;


procedure TMfPlaylistForm.LoadDefaultArtwork();
var
  FileName: string;

begin

  FileName := ExtractFilePath(ParamStr(0)) + 'DefaultArtwork.png';
  if not FileExists(FileName) then
    begin

      Log('Default artwork file not found: ' + FileName);
      Exit;
    end;
  try
    imgArtwork.Picture.LoadFromFile(FileName);
    if Assigned(imgArtwork.Picture.Graphic) and (not imgArtwork.Picture.Graphic.Empty) then
      begin

        FArtworkFileName := FileName;
        FUsingDefaultArtwork := True;
        Log('Audio artwork: default (' + FileName + ')');
      end;
  except
    on E: Exception do
      begin

        imgArtwork.Picture.Assign(nil);
        Log('Cannot load default artwork: ' + E.Message);
      end;
  end;
end;


procedure TMfPlaylistForm.LoadArtwork(const AMediaFile: string);
const
  Extensions: array[0..3] of string = ('.jpg',
                                       '.png',
                                       '.bmp',
                                       '.jpeg');
  Names: array[0..4] of string = ('Art',
                                  'Cover',
                                  'Folder',
                                  'Front',
                                  'Album');
var
  Candidates: TStringList;
  Others: TStringList;
  Search: TSearchRec;
  Folder: string;
  Candidate: string;
  Extension: string;
  I: Integer;
  J: Integer;

begin

  FArtworkFileName := '';
  FUsingDefaultArtwork := False;
  imgArtwork.Picture.Assign(nil);
  Folder := ExtractFilePath(AMediaFile);
  Candidates := TStringList.Create();
  Others := TStringList.Create();

  try
    // Prefer a track-specific image, then conventional album-art names.
    for I := Low(Extensions) to High(Extensions) do
      Candidates.Add(ChangeFileExt(AMediaFile,
                                   Extensions[I]));
    for I := Low(Names) to High(Names) do
      for J := Low(Extensions) to High(Extensions) do
        Candidates.Add(Folder + Names[I] + Extensions[J]);

    if (FindFirst(Folder + '*', faAnyFile, Search) = 0) then
    try
      repeat
        Extension := LowerCase(ExtractFileExt(Search.Name));
        if ((Search.Attr and faDirectory) = 0) and
          ((Extension = '.jpg') or (Extension = '.jpeg') or
          (Extension = '.png') or (Extension = '.bmp')) then
          Others.Add(Folder + Search.Name);
      until (FindNext(Search) <> 0);

    finally
      FindClose(Search);
    end;

    Others.Sort();

    for I := 0 to Others.Count - 1 do
      if (Candidates.IndexOf(Others[I]) < 0) then
        Candidates.Add(Others[I]);

    for I := 0 to Candidates.Count - 1 do
      begin

        Candidate := Candidates[I];
        // The application default is a fallback, even when media is beside the EXE.
        if SameText(ExpandFileName(Candidate),
          ExpandFileName(ExtractFilePath(ParamStr(0)) + 'DefaultArtwork.png')) then
          Continue;

        if not FileExists(Candidate) then
          Continue;

        try
          imgArtwork.Picture.LoadFromFile(Candidate);
          if Assigned(imgArtwork.Picture.Graphic) and
            (not imgArtwork.Picture.Graphic.Empty) then
            begin

              FArtworkFileName := Candidate;
              Log('Audio artwork: ' + ExtractFileName(Candidate));
              Break;
            end;

        except
          on E: Exception do
            begin

              imgArtwork.Picture.Assign(nil);
              Log('Cannot load artwork ' + ExtractFileName(Candidate) + ': ' + E.Message);
            end;
        end;
      end;
  finally
    Others.Free();
    Candidates.Free();
  end;
  if FArtworkFileName = '' then
    LoadDefaultArtwork();
end;


procedure TMfPlaylistForm.UpdatePreview();
var
  Index: Integer;

begin

  Index := FPlayer.CurrentIndex;

  if (FPlayer.State = plsError) then
    Index := -1;
  if (Index = FPreviewIndex) then
    Exit;

  FPreviewIndex := Index;
  FArtworkArtist := '';
  FArtworkTitle := '';
  FArtworkCaptionRendered := False;
  FArtworkFileName := '';
  FUsingDefaultArtwork := False;
  imgArtwork.Picture.Assign(nil);

  if (Index >= 0) and (not FPlayer.SegmentHasVideo(Index)) then
    begin

      pnlVideo.Visible := False;
      pnlArtwork.Visible := True;
      TFileNameParser.ResolveArtistTitle(FPlayer.SegmentFileName(Index),
                                         '',
                                         '',
                                         FArtworkArtist,
                                         FArtworkTitle);
      if Trim(FArtworkArtist) = '' then
        FArtworkArtist := 'Unknown Artist';
      if Trim(FArtworkTitle) = '' then
        FArtworkTitle := ChangeFileExt(ExtractFileName(FPlayer.SegmentFileName(Index)),
                                       '');
      LoadArtwork(FPlayer.SegmentFileName(Index));
      if FArtworkFileName <> '' then
      try
        RenderArtworkCaption(FArtworkFileName,
                             FArtworkArtist,
                             FArtworkTitle,
                             imgArtwork.Picture);
        FArtworkCaptionRendered := True;
      except
        on E: Exception do
          Log('Cannot render artwork caption: ' + E.Message);
      end;

      if FUsingDefaultArtwork then
        lblPreview.Caption := 'Audio artwork: default'
      else if (FArtworkFileName <> '') then
        lblPreview.Caption := 'Audio artwork: ' + ExtractFileName(FArtworkFileName)
      else
        lblPreview.Caption := 'Audio artwork (no readable JPG, BMP or PNG found)';
    end
  else
    begin

      pnlArtwork.Visible := False;
      pnlVideo.Visible := True;
      lblPreview.Caption := 'Video preview / audio artwork';
      pnlVideo.Invalidate();
      FPlayer.ResizeVideo();
    end;
end;


function TMfPlaylistForm.PrepareProgressPosition(const APoint: TPoint): Boolean;
var
  Presentation: Int64;
  Position: Int64;
  Duration: Int64;
  Width: Integer;

begin

  Result := False;
  FSeekIndex := -1;

  if not PtInRect(prbSegment.ClientRect, APoint) then
    Exit;

  if not Assigned(FPlayer) or
    (not (FPlayer.State in [plsPlaying, plsPaused, plsStopped, plsEnded])) then
    Exit;

  if (FPlayer.CurrentIndex < 0) or
    (FPlayer.GetPlaybackTime(Presentation,
                             Position,
                             Duration) <> S_OK) or
    (Duration <= 0) then
    Exit;

  Width := prbSegment.ClientWidth - 1;

  if (Width <= 0) then
    Exit;

  FSeekOffset := Round((APoint.X / Width) * Duration);

  // MF segment offsets must be strictly before the segment end.
  if (FSeekOffset >= Duration) then
    FSeekOffset := Duration - 1;
  FSeekIndex := FPlayer.CurrentIndex;
  mnuPlayFromHere.Caption := 'Play from this position (' + TimeText(FSeekOffset) + ')';
  Result := True;
end;


procedure TMfPlaylistForm.ProgressMouseDown(Sender: TObject;
                                            Button: TMouseButton;
                                            Shift: TShiftState;
                                            X: Integer;
                                            Y: Integer);

begin

  if (Sender = prbSegment) and (Button = mbLeft) and (not FClosing) and
    PrepareProgressPosition(Point(X,
                                  Y)) then
    PlayFromHereClick(nil);
end;


procedure TMfPlaylistForm.ProgressContextPopup(Sender: TObject;
                                               MousePos: TPoint;
                                               var Handled: Boolean);
var
  ScreenPoint: TPoint;

begin

  Handled := True;

  if (Sender = prbSegment) and (not FClosing) and PrepareProgressPosition(MousePos) then
    begin

      ScreenPoint := prbSegment.ClientToScreen(MousePos);
      pmProgress.Popup(ScreenPoint.X,
                       ScreenPoint.Y);
    end;
end;


procedure TMfPlaylistForm.PlayFromHereClick(Sender: TObject);

begin

  if (FSeekIndex >= 0) and (FSeekIndex = FPlayer.CurrentIndex) then
    Report('Play from this position',
           FPlayer.PlaySegment(FSeekIndex,
                               FSeekOffset));
end;


procedure TMfPlaylistForm.AddClick(Sender: TObject);

begin

  if OpenDialog.Execute() then
    AddFiles(OpenDialog.Files);
end;


procedure TMfPlaylistForm.RemoveClick(Sender: TObject);
var
  Index: Integer;

begin

  Index := lstPlaylist.ItemIndex;

  if FPlayer.HasPlaylist or (Index < 0) then
    Exit;

  FFiles.Delete(Index);

  if (Index >= FFiles.Count) then
    Index := FFiles.Count - 1;

  RefreshPlaylist(Index);
  UpdateControls();
end;


procedure TMfPlaylistForm.ClearClick(Sender: TObject);

begin

  if FPlayer.HasPlaylist then
    Exit;

  FFiles.Clear();
  RefreshPlaylist(-1);
  UpdateControls();
end;


procedure TMfPlaylistForm.MoveItem(const AOffset: Integer);
var
  Index: Integer;
  Destination: Integer;

begin

  Index := lstPlaylist.ItemIndex;
  Destination := Index + AOffset;

  if FPlayer.HasPlaylist or (Index < 0) or (Destination < 0) or (Destination >= FFiles.Count) then
    Exit;

  FFiles.Exchange(Index,
                  Destination);
  RefreshPlaylist(Destination);
  UpdateControls();
end;


procedure TMfPlaylistForm.UpClick(Sender: TObject);

begin

  MoveItem(-1);
end;


procedure TMfPlaylistForm.DownClick(Sender: TObject);

begin

  MoveItem(1);
end;


procedure TMfPlaylistForm.BuildClick(Sender: TObject);
var
  Status: HRESULT;

begin

  if FPlayer.HasPlaylist or (FFiles.Count = 0) then
    Exit;

  Screen.Cursor := crHourGlass;

  try
    FPlayer.VideoWindow := pnlVideo.Handle;
    Status := FPlayer.BuildPlaylist(FFiles);
  finally
    Screen.Cursor := crDefault;
  end;

  RefreshPlaylist(lstPlaylist.ItemIndex);
  Report('Build playlist',
         Status);
end;


procedure TMfPlaylistForm.PlayClick(Sender: TObject);

begin

  Report('Play',
         FPlayer.Play());
end;


procedure TMfPlaylistForm.PauseClick(Sender: TObject);

begin

  Report('Pause',
         FPlayer.Pause());
end;


procedure TMfPlaylistForm.StopClick(Sender: TObject);

begin

  Report('Stop',
         FPlayer.Stop());
end;


procedure TMfPlaylistForm.PlaySelectedClick(Sender: TObject);

begin

  if btnPlaySelected.Enabled then
    Report('Play selected segment',
           FPlayer.PlaySegment(lstPlaylist.ItemIndex));
end;


procedure TMfPlaylistForm.ReleaseClick(Sender: TObject);

begin

  FPlayer.ClosePlaylist();
  DrainEvents();
  pnlVideo.Invalidate();
  RefreshPlaylist(lstPlaylist.ItemIndex);
  Log('Sequence and session released. The file list can now be edited.');
  UpdateControls();
end;


procedure TMfPlaylistForm.SelectionChanged(Sender: TObject);

begin

  UpdateControls();
end;


procedure TMfPlaylistForm.PositionTimerTimer(Sender: TObject);

begin

  UpdateTime();
end;


procedure TMfPlaylistForm.PlaylistWindowProc(var Message: TMessage);
var
  Packet: TMfPlaylistEventPacket;

begin

  if (Message.Msg <> WM_MFPLAYLIST_EVENT) then
    begin

      Message.Result := DefWindowProc(FNotifyWindow,
                                      Message.Msg,
                                      Message.WParam,
                                      Message.LParam);
      Exit;
    end;

  Packet := TMfPlaylistEventPacket(Message.LParam);

  try
    if Assigned(FPlayer) and (not FClosing) then
      begin

        FPlayer.ProcessEvent(Packet);
        UpdateControls();
      end;

  finally
    Packet.Free();
  end;

  Message.Result := 0;
end;

end.
