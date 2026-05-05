// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmProcessPicker.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: frmProcessPicker form.
//              Process selection for Loopback deck.
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
unit frmProcessPicker;

interface

uses

  {WiAapi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.StrUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  {Application}
  RDJ_Common,
  MPxpButton,
  ProcessAudioPickerUtils;

type

  TdlgProcessPicker = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblDuration: TLabel;
    edtSearchProcess: TEdit;
    chkActiveOnly: TMPxpButton;
    chkAudioOnly: TMPxpButton;
    chkShowSysProcesses: TMPxpButton;
    btnRefresh: TMPxpButton;
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    Label1: TLabel;
    chkWholeAppTree: TMPxpButton;
    chkSelectedOnly: TMPxpButton;
    lblMode: TLabel;
    pnlAppsHost: TPanel;
    lblAppsHeader: TLabel;
    lvApps: TListView;
    splAppsChildren: TSplitter;
    pnlChildrenHost: TPanel;
    lblChildrenHeader: TLabel;
    lvChildren: TListView;

    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chkActiveOnlyClick(Sender: TObject);
    procedure chkAudioOnlyClick(Sender: TObject);
    procedure chkSelectedOnlyClick(Sender: TObject);
    procedure chkShowSysProcessesClick(Sender: TObject);
    procedure chkWholeAppTreeClick(Sender: TObject);
    procedure edtSearchProcessChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvAppsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvChildrenSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvAppsDblClick(Sender: TObject);
    procedure lvChildrenDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    FSelectedPID: DWORD;
    FSelectedProcName: string;
    FGroups: TObjectList<TProcessGroupItem>;
    FFilteredGroups: TList<TProcessGroupItem>;
    FSelectedGroup: TProcessGroupItem;
    FSelectedChild: TProcessChildItem;
    procedure ConfigureListViews;
    procedure BuildColumns;
    procedure LoadLiveData;
    procedure ApplyFilters;
    procedure PopulateApps;
    procedure PopulateChildren;
    procedure UpdateSelectedOutput;
    procedure UpdateInfoLabel;
    procedure SetSelectionMode(const AWholeTree: Boolean);
    function GroupMatchesSearch(const AGroup: TProcessGroupItem; const AText: string): Boolean;
    function GroupPassesFilters(const AGroup: TProcessGroupItem): Boolean;
    function ChildPassesFilters(const AChild: TProcessChildItem): Boolean;
    function AudioStateToText(const AState: TProcessAudioState): string;
    function PeakToText(const APeak: Single): string;
    function GroupAllowsWholeTree(const AGroup: TProcessGroupItem): Boolean;
    function GroupTreeStateText(const AGroup: TProcessGroupItem): string;
    function FormatGroupCaption(const AGroup: TProcessGroupItem): string;
    function FormatChildCaption(const AChild: TProcessChildItem): string;
    function ChildAudioStateText(const AChild: TProcessChildItem): string;
    procedure RestoreSelection(const APrevPID: DWORD; const APrevWholeTree: Boolean);

  published

    property SelectedPID: DWORD read FSelectedPID write FSelectedPID;
    property SelectedProcName: string read FSelectedProcName write FSelectedProcName;
  end;

var
  dlgProcessPicker: TdlgProcessPicker;


implementation

{$R *.dfm}

procedure TdlgProcessPicker.FormCreate(Sender: TObject);
begin

  FGroups := TObjectList<TProcessGroupItem>.Create(True);
  FFilteredGroups := TList<TProcessGroupItem>.Create;

  ConfigureListViews();
  BuildColumns();

  if (edtSearchProcess.Text = '-') then
    edtSearchProcess.Text := '';
  edtSearchProcess.ReadOnly := False;

  SetSelectionMode(True);
  LoadLiveData();
  ApplyFilters();
end;


procedure TdlgProcessPicker.FormDestroy(Sender: TObject);
begin

  FFilteredGroups.Free;
  FGroups.Free;
end;


procedure TdlgProcessPicker.FormShow(Sender: TObject);
begin

  ApplyDarkWindowFrame(Handle);
end;


procedure TdlgProcessPicker.ConfigureListViews;
begin

  lvApps.ViewStyle := vsReport;
  lvApps.ReadOnly := True;
  lvApps.RowSelect := True;
  lvApps.HideSelection := False;
  lvApps.MultiSelect := False;
  lvApps.ColumnClick := False;
  lvApps.GridLines := False;
  lvApps.OwnerData := False;

  lvChildren.ViewStyle := vsReport;
  lvChildren.ReadOnly := True;
  lvChildren.RowSelect := True;
  lvChildren.HideSelection := False;
  lvChildren.MultiSelect := False;
  lvChildren.ColumnClick := False;
  lvChildren.GridLines := False;
  lvChildren.OwnerData := False;
end;


procedure TdlgProcessPicker.BuildColumns;
begin

  lvApps.Columns.BeginUpdate;

  try

    lvApps.Columns.Clear;
    with lvApps.Columns.Add do
      begin
        Caption := 'Application';
        Width := 250;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'PID';
        Width := 90;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'Audio children';
        Width := 100;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'Tree';
        Width := 100;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'Audio';
        Width := 90;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'Peak';
        Width := 70;
      end;
    with lvApps.Columns.Add do
      begin
        Caption := 'Path';
        Width := 360;
      end;
  finally
    lvApps.Columns.EndUpdate;
  end;

  lvChildren.Columns.BeginUpdate;

  try

    lvChildren.Columns.Clear;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Process / Session';
        Width := 290;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'PID';
        Width := 90;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Parent PID';
        Width := 90;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Role';
        Width := 190;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Audio';
        Width := 140;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Peak';
        Width := 70;
      end;
    with lvChildren.Columns.Add do
      begin
        Caption := 'Path';
        Width := 300;
      end;
  finally
    lvChildren.Columns.EndUpdate;
  end;
end;


procedure TdlgProcessPicker.LoadLiveData;
var
  PrevPID: DWORD;
  PrevWholeTree: Boolean;

begin

  PrevPID := FSelectedPID;
  PrevWholeTree := chkWholeAppTree.Checked;

  FGroups.Clear();
  BuildLiveProcessGroups(FGroups);
  RestoreSelection(PrevPID, PrevWholeTree);
end;


procedure TdlgProcessPicker.RestoreSelection(const APrevPID: DWORD; const APrevWholeTree: Boolean);
var
  I,
  J: Integer;

begin

  FSelectedGroup := nil;
  FSelectedChild := nil;

  if (APrevPID = 0) then
    Exit;

  if APrevWholeTree then
    begin

      for I := 0 to FGroups.Count - 1 do
        if (FGroups[I].RootProcessId = APrevPID) then
          begin

            FSelectedGroup := FGroups[I];
            Exit;
          end;
    end
  else
    begin

      for I := 0 to FGroups.Count - 1 do
        begin

          if (FGroups[I].RootProcessId = APrevPID) then
            begin

              FSelectedGroup := FGroups[I];
              Exit;
            end;

          for J := 0 to FGroups[I].Children.Count - 1 do
            if FGroups[I].Children[J].ProcessId = APrevPID then
              begin

                FSelectedGroup := FGroups[I];
                FSelectedChild := FGroups[I].Children[J];
                Exit;
              end;
        end;
    end;
end;


function TdlgProcessPicker.AudioStateToText(const AState: TProcessAudioState): string;
begin

  case AState of
    pasActive: Result := 'Active';
    pasSilent: Result := 'Silent';
  else
    Result := 'Unknown';
  end;
end;


function TdlgProcessPicker.PeakToText(const APeak: Single): string;
var
  Pct: Integer;

begin

  Pct := Round(EnsureRange(APeak,
                           0.0,
                           1.0) * 100.0);
  Result := IntToStr(Pct) + '%';
end;

function TdlgProcessPicker.GroupAllowsWholeTree(const AGroup: TProcessGroupItem): Boolean;
begin

  Result := (AGroup = nil) or (AGroup.AudioChildCount <= 1);
end;


function TdlgProcessPicker.GroupTreeStateText(const AGroup: TProcessGroupItem): string;
begin
  if AGroup = nil then
    Exit('');

  if GroupAllowsWholeTree(AGroup) then
    Result := 'safe'
  else
    Result := 'unsafe';
end;

function TdlgProcessPicker.FormatGroupCaption(const AGroup: TProcessGroupItem): string;
begin
  Result := AGroup.DisplayName;

  if AGroup.IsBrowserLike and (AGroup.BrowserFamily <> '') then
    Result := Result + ' [' + AGroup.BrowserFamily + ']';

  if AGroup.AudioChildCount > 1 then
    Result := Result + Format(' (%d audio children)', [AGroup.AudioChildCount]);
end;

function TdlgProcessPicker.FormatChildCaption(const AChild: TProcessChildItem): string;
begin
  Result := AChild.DisplayName;

  if AChild.IsBrowserLike and (AChild.BrowserFamily <> '') then
    Result := Result + ' [' + AChild.BrowserFamily + ']';

  if AChild.SessionCount > 0 then
    Result := Result + Format(' (%d session%s)', [AChild.SessionCount, IfThen(AChild.SessionCount = 1, '', 's')]);

  if Trim(AChild.RoleHint) <> '' then
    Result := Result + ' - ' + AChild.RoleHint;
end;

function TdlgProcessPicker.ChildAudioStateText(const AChild: TProcessChildItem): string;
begin
  if AChild = nil then
    Exit('');

  if AChild.SessionCount > 0 then
    Result := Format('%s (%d session%s)',
                     [AudioStateToText(AChild.AudioState),
                      AChild.SessionCount,
                      IfThen(AChild.SessionCount = 1, '', 's')])
  else
    Result := 'No audio session';
end;


function TdlgProcessPicker.GroupMatchesSearch(const AGroup: TProcessGroupItem;
  const AText: string): Boolean;
var
  SearchText: string;
  I: Integer;

begin

  SearchText := LowerCase(Trim(AText));
  if (SearchText = '') then
    Exit(True);

  Result := (Pos(SearchText, LowerCase(AGroup.DisplayName)) > 0) or
            (Pos(SearchText, LowerCase(AGroup.ExeName)) > 0) or
            (Pos(SearchText, LowerCase(AGroup.ImagePath)) > 0) or
            (Pos(SearchText, LowerCase(IntToStr(AGroup.RootProcessId))) > 0);

  if Result then
    Exit;

  for I := 0 to AGroup.Children.Count - 1 do
    begin
      Result := (Pos(SearchText, LowerCase(AGroup.Children[I].DisplayName)) > 0) or
                (Pos(SearchText, LowerCase(AGroup.Children[I].ExeName)) > 0) or
                (Pos(SearchText, LowerCase(AGroup.Children[I].ImagePath)) > 0) or
                (Pos(SearchText, LowerCase(IntToStr(AGroup.Children[I].ProcessId))) > 0);
      if Result then
        Exit;
    end;
end;


function TdlgProcessPicker.GroupPassesFilters(const AGroup: TProcessGroupItem): Boolean;
var
  I: Integer;
  HasActiveChild: Boolean;
  HasAudioInfo: Boolean;

begin

  Result := GroupMatchesSearch(AGroup,
                               edtSearchProcess.Text);
  if not Result then
    Exit;

  if chkActiveOnly.Checked and (AGroup.AudioState <> pasActive) then
    begin

      HasActiveChild := False;
      for I := 0 to AGroup.Children.Count - 1 do
        if (AGroup.Children[I].AudioState = pasActive) then
          begin

            HasActiveChild := True;
            Break;
          end;
      if not HasActiveChild then
        Exit(False);
    end;

  if chkAudioOnly.Checked then
    begin

      HasAudioInfo := (AGroup.AudioState <> pasUnknown);
      if not HasAudioInfo then
        for I := 0 to AGroup.Children.Count - 1 do
          if AGroup.Children[I].AudioState <> pasUnknown then
            begin

              HasAudioInfo := True;
              Break;
            end;
       if not HasAudioInfo then
         Exit(False);
    end;

  if (not chkShowSysProcesses.Checked) and
     ((AGroup.RootProcessId <= 4) or SameText(AGroup.ExeName, 'System')) then
    Exit(False);

  Result := True;
end;


function TdlgProcessPicker.ChildPassesFilters(const AChild: TProcessChildItem): Boolean;
begin

  Result := Assigned(AChild);
  if not Result then
    Exit;

  if chkActiveOnly.Checked and (AChild.AudioState <> pasActive) then
    Exit(False);

  if chkAudioOnly.Checked and (AChild.SessionCount <= 0) and
     (AChild.AudioState = pasUnknown) then
    Exit(False);

  if (not chkShowSysProcesses.Checked) and
     ((AChild.ProcessId <= 4) or SameText(AChild.ExeName, 'System')) then
    Exit(False);

  Result := True;
end;


procedure TdlgProcessPicker.ApplyFilters();
var
  I: Integer;

begin

  FFilteredGroups.Clear;

  for I := 0 to FGroups.Count - 1 do
    if GroupPassesFilters(FGroups[I]) then
      FFilteredGroups.Add(FGroups[I]);

  if Assigned(FSelectedGroup) and (FFilteredGroups.IndexOf(FSelectedGroup) < 0) then
    begin

      FSelectedGroup := nil;
      FSelectedChild := nil;
    end;

  PopulateApps();
  PopulateChildren();
  UpdateSelectedOutput();
  UpdateInfoLabel();
end;


procedure TdlgProcessPicker.PopulateApps;
var
  I: Integer;
  Item: TListItem;
  Group: TProcessGroupItem;

begin

  lvApps.Items.BeginUpdate;

  try

    lvApps.Items.Clear;

    for I := 0 to FFilteredGroups.Count - 1 do
      begin

        Group := FFilteredGroups[I];
        Item := lvApps.Items.Add;
        Item.Caption := FormatGroupCaption(Group);
        Item.SubItems.Add(IntToStr(Group.RootProcessId));
        Item.SubItems.Add(IntToStr(Group.AudioChildCount));
        Item.SubItems.Add(GroupTreeStateText(Group));
        Item.SubItems.Add(AudioStateToText(Group.AudioState));
        Item.SubItems.Add(PeakToText(Group.Peak));
        Item.SubItems.Add(Group.ImagePath);
        Item.Data := Group;

        if (Group = FSelectedGroup) then
          Item.Selected := True;
      end;
  finally

    lvApps.Items.EndUpdate;
  end;
end;


procedure TdlgProcessPicker.PopulateChildren;
var
  I: Integer;
  Item: TListItem;
  Child: TProcessChildItem;
  KeepSelectedChild: Boolean;

begin

  lvChildren.Items.BeginUpdate;

  try

    lvChildren.Items.Clear;
    if not Assigned(FSelectedGroup) then
      Exit;

    KeepSelectedChild := False;

    for I := 0 to FSelectedGroup.Children.Count - 1 do
      begin

        Child := FSelectedGroup.Children[I];
        if not ChildPassesFilters(Child) then
          Continue;

        Item := lvChildren.Items.Add;
        Item.Caption := FormatChildCaption(Child);
        Item.SubItems.Add(IntToStr(Child.ProcessId));
        Item.SubItems.Add(IntToStr(Child.ParentProcessId));
        Item.SubItems.Add(Child.RoleHint);
        Item.SubItems.Add(ChildAudioStateText(Child));
        Item.SubItems.Add(PeakToText(Child.Peak));
        Item.SubItems.Add(Child.ImagePath);
        Item.Data := Child;

        if Child = FSelectedChild then
          begin
            Item.Selected := True;
            KeepSelectedChild := True;
          end;
      end;

    if not KeepSelectedChild then
      FSelectedChild := nil;
  finally

    lvChildren.Items.EndUpdate;
  end;
end;


procedure TdlgProcessPicker.UpdateSelectedOutput;
begin

  if chkWholeAppTree.Checked then
    begin

      if Assigned(FSelectedGroup) then
        begin
          if not GroupAllowsWholeTree(FSelectedGroup) then
            begin
              FSelectedPID := 0;
              FSelectedProcName := '';
              Exit;
            end;

          FSelectedPID := FSelectedGroup.RootProcessId;
          if (Trim(FSelectedGroup.DisplayName) <> '') then
            FSelectedProcName := FSelectedGroup.DisplayName
          else
            FSelectedProcName := FSelectedGroup.ExeName;
        end
      else
        begin

          FSelectedPID := 0;
          FSelectedProcName := '';
        end;
      Exit;
    end;

  if Assigned(FSelectedChild) then
    begin

      FSelectedPID := FSelectedChild.ProcessId;
      if (Trim(FSelectedChild.DisplayName) <> '') then
        FSelectedProcName := FSelectedChild.DisplayName
      else
        FSelectedProcName := FSelectedChild.ExeName;
    end
  else if Assigned(FSelectedGroup) and (FSelectedGroup.Children.Count = 1) then
    begin
      FSelectedPID := FSelectedGroup.Children[0].ProcessId;
      if (Trim(FSelectedGroup.Children[0].DisplayName) <> '') then
        FSelectedProcName := FSelectedGroup.Children[0].DisplayName
      else
        FSelectedProcName := FSelectedGroup.Children[0].ExeName;
    end
  else
    begin

      FSelectedPID := 0;
      FSelectedProcName := '';
    end;
end;


procedure TdlgProcessPicker.UpdateInfoLabel;
var
  ModeText: string;

begin

  if chkWholeAppTree.Checked then
    ModeText := 'whole app tree'
  else
    ModeText := 'selected only';

  if chkWholeAppTree.Checked and Assigned(FSelectedGroup) and
     (not GroupAllowsWholeTree(FSelectedGroup)) then
    begin
      Label1.Caption := Format('Selected: %s has %d audio child processes. Use selected only.',
                               [FSelectedGroup.DisplayName, FSelectedGroup.AudioChildCount]);
      Exit;
    end;

  if FSelectedPID = 0 then
    Label1.Caption := 'Selected: nothing'
  else if Assigned(FSelectedGroup) then
    Label1.Caption := Format('Selected: %s (PID %d, %s, tree %s)',
                             [FSelectedProcName,
                              FSelectedPID,
                              ModeText,
                              GroupTreeStateText(FSelectedGroup)])
  else
    Label1.Caption := Format('Selected: %s (PID %d, %s)',
                             [FSelectedProcName, FSelectedPID, ModeText]);
end;


procedure TdlgProcessPicker.SetSelectionMode(const AWholeTree: Boolean);
begin

  if AWholeTree and Assigned(FSelectedGroup) and
     (not GroupAllowsWholeTree(FSelectedGroup)) then
    begin
      chkWholeAppTree.Checked := False;
      chkSelectedOnly.Checked := True;
      UpdateSelectedOutput();
      UpdateInfoLabel();
      MessageDlg('Whole app tree is not safe for this application because it has multiple audio child processes. Select an individual child/session instead.',
                 mtWarning,
                 [mbOK],
                 0);
      Exit;
    end;

  chkWholeAppTree.Checked := AWholeTree;
  chkSelectedOnly.Checked := not AWholeTree;
  UpdateSelectedOutput();
  UpdateInfoLabel();
end;


procedure TdlgProcessPicker.lvAppsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin

  if not Selected then
    Exit;

  FSelectedGroup := TProcessGroupItem(Item.Data);
  if chkWholeAppTree.Checked or (FSelectedGroup.Children.Count <> 1) then
    FSelectedChild := nil
  else
    FSelectedChild := FSelectedGroup.Children[0];

  PopulateChildren();
  UpdateSelectedOutput();
  UpdateInfoLabel();
end;


procedure TdlgProcessPicker.lvChildrenSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin

  if not Selected then
    Exit;

  FSelectedChild := TProcessChildItem(Item.Data);
  if chkSelectedOnly.Checked then
    UpdateSelectedOutput();
  UpdateInfoLabel();
end;


procedure TdlgProcessPicker.lvAppsDblClick(Sender: TObject);
begin

  if (lvApps.Selected <> nil) then
    begin
      UpdateSelectedOutput();
      if (FSelectedPID <> 0) then
        ModalResult := mrOk;
    end;
end;


procedure TdlgProcessPicker.lvChildrenDblClick(Sender: TObject);
begin

  if (lvChildren.Selected <> nil) then
  begin

    SetSelectionMode(False);
    ModalResult := mrOk;
  end;
end;


procedure TdlgProcessPicker.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TdlgProcessPicker.btnOkClick(Sender: TObject);
begin

  UpdateSelectedOutput;
  if (FSelectedPID = 0) then
    begin

      MessageDlg('Please select an application or process first.',
                 mtWarning,
                 [mbOK],
                 0);
      Exit;
    end;
  ModalResult := mrOk;
end;


procedure TdlgProcessPicker.btnRefreshClick(Sender: TObject);
begin

  LoadLiveData();
  ApplyFilters();
end;


procedure TdlgProcessPicker.chkActiveOnlyClick(Sender: TObject);
begin

  ApplyFilters();
end;


procedure TdlgProcessPicker.chkAudioOnlyClick(Sender: TObject);
begin

  ApplyFilters();
end;


procedure TdlgProcessPicker.chkSelectedOnlyClick(Sender: TObject);
begin

  SetSelectionMode(False);
end;


procedure TdlgProcessPicker.chkShowSysProcessesClick(Sender: TObject);
begin

  ApplyFilters();
end;


procedure TdlgProcessPicker.chkWholeAppTreeClick(Sender: TObject);
begin

  SetSelectionMode(True);
end;


procedure TdlgProcessPicker.edtSearchProcessChange(Sender: TObject);
begin

  ApplyFilters();
end;

end.
