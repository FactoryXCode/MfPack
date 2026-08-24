// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgTimedTextLanguages.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Dialog example to select sidecar and embedded subtitle tracks.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues, Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/08/2026 All                 Added exact MKV embedded subtitle track selection.
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: Bitmap PGS/VobSub tracks are displayed as unsupported and cannot
//               be selected by the current text compositor.
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
//==============================================================================
unit dlgSelectTimedTextLanguages;

interface

uses

  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {Project}
  MfPlayerClassX,
  MfEmbeddedSubtitleReader,
  TimedTextClass,
  LangTags,
  MfPCXConstants;

type
  TSubtitleListSource = (slsSidecar,
                         slsEmbedded);

  TSubtitleListEntry = record
    Source: TSubtitleListSource;
    SourceIndex: Integer;
  end;

  TSubtitleListEntryArray = array of TSubtitleListEntry;

  TDlgTimedTextLanguages = class(TForm)
    butOk: TButton;
    butCancel: TButton;
    lvTTxtLang: TListView;
    procedure FormShow(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure lvTTxtLangMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  protected
    procedure WMLButtonDown(var Msg: TWMLBUTTONDOWN); message WM_LBUTTONDOWN;

  private
    FEmbeddedTracks: TMfEmbeddedSubtitleTrackInfoArray;
    FEntries: TSubtitleListEntryArray;

    procedure AddEntry(const Source: TSubtitleListSource;
                       const SourceIndex: Integer;
                       const Checked: Boolean;
                       const LanguageTag: string;
                       const FriendlyName: string;
                       const SourceName: string;
                       const Enabled: Boolean);
    procedure UncheckOtherItems(const KeepIndex: Integer);
    procedure UpdateCheckedItems();

  public

    procedure SubtitleLoadStarted();
    procedure SubtitleLoadCompleted(const LoadResult: HRESULT);
  end;

var
  dlgTimedTextLanguages: TDlgTimedTextLanguages;


implementation

{$R *.dfm}

uses frmMfPlayer;


procedure TDlgTimedTextLanguages.WMLButtonDown(var Msg: TWMLBUTTONDOWN);
begin

  if (lvTTxtLang.GetItemAt(Msg.XPos, Msg.YPos) <> nil) then
    inherited;
end;


procedure TDlgTimedTextLanguages.butOkClick(Sender: TObject);
begin

  Close();
end;


procedure TDlgTimedTextLanguages.butCancelClick(Sender: TObject);
begin

  Close();
end;


procedure TDlgTimedTextLanguages.AddEntry(const Source: TSubtitleListSource;
                                          const SourceIndex: Integer;
                                          const Checked: Boolean;
                                          const LanguageTag: string;
                                          const FriendlyName: string;
                                          const SourceName: string;
                                          const Enabled: Boolean);
var
  EntryIndex: Integer;
  Item: TListItem;
  DisplayLanguage: string;

begin

  EntryIndex := Length(FEntries);
  SetLength(FEntries,
            EntryIndex + 1);
  FEntries[EntryIndex].Source := Source;
  FEntries[EntryIndex].SourceIndex := SourceIndex;

  DisplayLanguage := Trim(LanguageTag);
  if (DisplayLanguage = '') then
    DisplayLanguage := 'und';

  Item := lvTTxtLang.Items.Add();
  Item.Checked := Checked;
  Item.Caption := DisplayLanguage;
  Item.SubItems.Add(FriendlyName);
  Item.SubItems.Add(SourceName);

  if not Enabled then
    Item.Cut := True;
end;


procedure TDlgTimedTextLanguages.UncheckOtherItems(const KeepIndex: Integer);
var
  I: Integer;

begin

  for I := 0 to lvTTxtLang.Items.Count - 1 do
    if (I <> KeepIndex) then
      lvTTxtLang.Items[I].Checked := False;
end;


procedure TDlgTimedTextLanguages.UpdateCheckedItems();
var
  I: Integer;
  IsActive: Boolean;
  Entry: TSubtitleListEntry;

begin

  if not Assigned(MfPlayerX) then
    Exit;

  lvTTxtLang.Items.BeginUpdate();

  try
    for I := Low(FEntries) to High(FEntries) do
      begin
        if (I < 0) or (I >= lvTTxtLang.Items.Count) then
          Continue;

        Entry := FEntries[I];
        IsActive := False;

        case Entry.Source of
          slsSidecar:  if (Entry.SourceIndex >= Low(pc_LanguageTags.TimedTxtPropsArray)) and
                          (Entry.SourceIndex <= High(pc_LanguageTags.TimedTxtPropsArray)) then
                         IsActive := (not MfPlayerX.GetActiveSubtitleIsEmbedded()) and
                                     SameText(pc_LanguageTags.TimedTxtPropsArray[Entry.SourceIndex].sLanguageTag,
                                              MfPlayerX.SubtitleLanguage);

          slsEmbedded: if (Entry.SourceIndex >= Low(FEmbeddedTracks)) and
                          (Entry.SourceIndex <= High(FEmbeddedTracks)) then
                         IsActive := MfPlayerX.GetActiveSubtitleIsEmbedded() and
                                     (MfPlayerX.GetActiveEmbeddedSubtitleStreamIndex() =
                                      Integer(FEmbeddedTracks[Entry.SourceIndex].StreamIndex));
        end;

        lvTTxtLang.Items[I].Checked := IsActive;
      end;
  finally
    lvTTxtLang.Items.EndUpdate();
  end;
end;


procedure TDlgTimedTextLanguages.SubtitleLoadStarted();
begin

  Caption := 'Select preferred subtitle language - loading...';
  // Keep the list interactive. A later click cancels and supersedes the
  // current background load without waiting on the VCL thread.
  lvTTxtLang.Enabled := True;
  butOk.Enabled := True;
  butCancel.Enabled := True;
end;


procedure TDlgTimedTextLanguages.SubtitleLoadCompleted(
  const LoadResult: HRESULT);
begin

  Caption := 'Select preferred subtitle language';
  lvTTxtLang.Enabled := True;
  butOk.Enabled := True;
  butCancel.Enabled := True;

  if not Visible then
    Exit;

  UpdateCheckedItems();

  if (LoadResult <> S_OK) and
     (LoadResult <> E_ABORT) then
    ShowMessage(Format('The subtitle track could not be selected. HRESULT 0x%s',
                       [IntToHex(DWORD(LoadResult), 8)]));
end;


procedure TDlgTimedTextLanguages.FormShow(Sender: TObject);
var
  I: Integer;
  IsActive: Boolean;
  FriendlyName: string;
  SourceName: string;

begin

  lvTTxtLang.Items.BeginUpdate();

  try
    lvTTxtLang.Items.Clear();
    SetLength(FEntries,
              0);
    SetLength(FEmbeddedTracks,
              0);

    if not Assigned(MfPlayerX) then
      Exit;

    // Re-scan sidecars because files may have been added or removed while the
    // player is open.
    SetLength(pc_LanguageTags.TimedTxtPropsArray,
              0);
    pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(MfPlayerX.MediaFileName,
                                                                       MfPlayerX.SubtitleLanguage,
                                                                       0,
                                                                       EXTSUBRIP);

    for I := Low(pc_LanguageTags.TimedTxtPropsArray) to
             High(pc_LanguageTags.TimedTxtPropsArray) do
      begin
        IsActive := (not MfPlayerX.GetActiveSubtitleIsEmbedded()) and
                    pc_LanguageTags.TimedTxtPropsArray[I].bActiveFile;

        AddEntry(slsSidecar,
                 I,
                 IsActive,
                 pc_LanguageTags.TimedTxtPropsArray[I].sLanguageTag,
                 pc_LanguageTags.TimedTxtPropsArray[I].sFriendlyLanguageName,
                 'Sidecar SRT',
                 True);
      end;

    // The embedded-track list was already populated when the media opened.
    // Direct Matroska enumeration performs file I/O, so only rescan when the
    // player currently has no cached track list.
    if (MfPlayerX.GetEmbeddedSubtitleTracks(FEmbeddedTracks) <> S_OK) then
      begin
        MfPlayerX.RefreshEmbeddedSubtitleTracks();
        MfPlayerX.GetEmbeddedSubtitleTracks(FEmbeddedTracks);
      end;

    if (Length(FEmbeddedTracks) > 0) then
      for I := Low(FEmbeddedTracks) to High(FEmbeddedTracks) do
        begin
          FriendlyName := Trim(FEmbeddedTracks[I].Name);
          if (FriendlyName = '') then
            FriendlyName := pc_LanguageTags.GetLangOrCountryFromTag(
                              FEmbeddedTracks[I].Language);
          if (FriendlyName = '') then
            FriendlyName := Trim(FEmbeddedTracks[I].Language);

          if (FriendlyName = '') then
            FriendlyName := 'Embedded subtitle track';

          case FEmbeddedTracks[I].Format of
            esfSrt:         SourceName := 'MKV SRT/UTF-8';
            esfSsaAss:      SourceName := 'MKV SSA/ASS';
            esfGenericText: SourceName := 'MKV text subtitles';
            esfVobSub:      SourceName := 'MKV VobSub (unsupported)';
            esfPgs:         SourceName := 'MKV PGS (unsupported)';
          else
            SourceName := 'MKV subtitle (unsupported)';
          end;

          IsActive := MfPlayerX.GetActiveSubtitleIsEmbedded() and
                      (MfPlayerX.GetActiveEmbeddedSubtitleStreamIndex() = Integer(FEmbeddedTracks[I].StreamIndex));

          AddEntry(slsEmbedded,
                   I,
                   IsActive,
                   FEmbeddedTracks[I].Language,
                   FriendlyName,
                   SourceName,
                   FEmbeddedTracks[I].Supported);
        end;

    if (lvTTxtLang.Items.Count = 0) then
      begin

        with lvTTxtLang.Items.Add do
          begin
            Caption := 'No subtitle streams reported';
            SubItems.Add('');
            SubItems.Add('No subtitle track found by Media Foundation or the direct MKV parser');
            Checked := False;
          end;
      end;
  finally
    lvTTxtLang.Items.EndUpdate();
  end;

  if Assigned(frm_MfPlayer) and
     frm_MfPlayer.SubtitleLoadPending() then
    SubtitleLoadStarted()
  else
    begin

      Caption := 'Select preferred subtitle language';
      lvTTxtLang.Enabled := True;
      butOk.Enabled := True;
      butCancel.Enabled := True;
    end;
end;


procedure TDlgTimedTextLanguages.lvTTxtLangMouseUp(Sender: TObject;
                                                   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTests: THitTests;
  Item: TListItem;
  Entry: TSubtitleListEntry;
  Hr: HRESULT;

begin

  HitTests := (Sender as TCustomListView).GetHitTestInfoAt(X,
                                                           Y);
  if not (htOnStateIcon in HitTests) then
    Exit;

  Item := (Sender as TCustomListView).GetItemAt(X, Y);
  if not Assigned(Item) then
    Exit;

  if (Item.Index < Low(FEntries)) or (Item.Index > High(FEntries)) then
    Exit;

  // Clicking the active check box must not leave the player with no selected
  // source. Restore it if the VCL just toggled it off.
  if not Item.Checked then
    begin
      Item.Checked := True;
      Exit;
    end;

  Entry := FEntries[Item.Index];
  Hr := E_FAIL;

  case Entry.Source of
    slsSidecar:  begin

                   if (Entry.SourceIndex >= Low(pc_LanguageTags.TimedTxtPropsArray)) and
                      (Entry.SourceIndex <= High(pc_LanguageTags.TimedTxtPropsArray)) then
                     Hr := frm_MfPlayer.SelectSidecarSubtitleLanguageAsync(
                             pc_LanguageTags.TimedTxtPropsArray[Entry.SourceIndex].sLanguageTag);
                 end;

    slsEmbedded: begin

                   if (Entry.SourceIndex >= Low(FEmbeddedTracks)) and
                      (Entry.SourceIndex <= High(FEmbeddedTracks)) then
                     begin

                       if not FEmbeddedTracks[Entry.SourceIndex].Supported then
                         Hr := MF_E_INVALIDMEDIATYPE
                       else
                         Hr := frm_MfPlayer.SelectEmbeddedSubtitleTrackAsync(
                                 FEmbeddedTracks[Entry.SourceIndex].StreamIndex);
                    end;
                 end;
  end;

  if (Hr = S_OK) then
    UncheckOtherItems(Item.Index)
  else
    begin
      Item.Checked := False;
      ShowMessage(Format('The subtitle track could not be selected. HRESULT 0x%s',
                         [IntToHex(DWORD(Hr), 8)]));
      UpdateCheckedItems();
    end;
end;

end.
