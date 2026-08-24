// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: LWFileBrowserExDlg.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 4.0.0
//
// Description: A very lightweight filebrowser dialog with network support.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX400/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit LWFileBrowserExDlg;

interface

uses

  {WinApi}
  Winapi.Windows,
  Winapi.WinSock,
  Winapi.Messages,
  Winapi.ShellAPI,
  Winapi.CommCtrl,
  Winapi.WinNetWk, // << do not remove!
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  {MediaFoundationApi}
  Winapi.MediaFoundationApi.MfMetLib,
  Winapi.MediaFoundationApi.MfUtils,
  {Application}
  RDJ_NetWorkStationsScanner,
  MPxpButton;

const

  CLWAudioFilter = 'Audio Files (*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus)|*.mp3;*.wav;*.flac;*.ogg;*.m4a;*.aac;*.wma;*.opus|' +
                   'MP3 Files (*.mp3)|*.mp3|' +
                   'WAV Files (*.wav)|*.wav|' +
                   'FLAC Files (*.flac)|*.flac|' +
                   'OGG Files (*.ogg)|*.ogg|' +
                   'M4A Files (*.m4a)|*.m4a|' +
                   'AAC Files (*.aac)|*.aac|' +
                   'WMA Files (*.wma)|*.wma|' +
                   'Opus Files (*.opus)|*.opus|' +
                   'All Files (*.*)|*.*';

  CLWGraphicsFilter = 'Graphic Files (*.png;*.bmp;*.jpg;*.jpeg)|*.png;*.bmp;*.jpg;*.jpeg|' +
                      'PNG Files (*.png)|*.png|' +
                      'BMP Files (*.bmp)|*.bmp|' +
                      'JPG Files (*.jpg;*.jpeg)|*.jpg;*.jpeg|' +
                      'All Files (*.*)|*.*';

  LW_NERR_SUCCESS         = 0;
  LW_MAX_PREFERRED_LENGTH = DWORD(-1);
  LW_ERROR_MORE_DATA      = 234;

  LW_SV_TYPE_WORKSTATION  = $00000001;
  LW_SV_TYPE_SERVER       = $00000002;

  LW_STYPE_DISKTREE       = $00000000;
  LW_STYPE_MASK           = $000000FF;

type

  PLWServerInfo100 = ^TLWServerInfo100;
  TLWServerInfo100 = record
    sv100_platform_id: DWORD;
    sv100_name: PWideChar;
  end;

  PLWShareInfo1 = ^TLWShareInfo1;
  TLWShareInfo1 = record
    shi1_netname: PWideChar;
    shi1_type: DWORD;
    shi1_remark: PWideChar;
  end;

function NetServerEnum(servername: PWideChar;
                       level: DWORD;
                       var bufptr: Pointer;
                       prefmaxlen: DWORD;
                       var entriesread: DWORD;
                       var totalentries: DWORD;
                       servertype: DWORD;
                       domain: PWideChar;
                       var resume_handle: DWORD): DWORD; stdcall;
                       external 'Netapi32.dll' name 'NetServerEnum';

function NetShareEnum(servername: PWideChar;
                      level: DWORD;
                      var bufptr: Pointer;
                      prefmaxlen: DWORD;
                      var entriesread: DWORD;
                      var totalentries: DWORD;
                      var resume_handle: DWORD): DWORD; stdcall;
                      external 'Netapi32.dll' name 'NetShareEnum';

function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall;
                      external 'Netapi32.dll' name 'NetApiBufferFree';

type

  TLWFileBrowserExFilter = (fbxAudio,
                            fbxGraphics);

  TLWFileBrowserExDlg = class(TForm)
    pnlTop: TPanel;
    lblLocation: TLabel;
    cbxLocations: TComboBox;
    lblIPv4Address: TLabel;
    edtIPv4Address: TEdit;
    edtPath: TEdit;
    btnGo: TMPxpButton;
    cbxFileFilter: TFilterComboBox;
    pnlBottom: TPanel;
    lblSelectedFile: TLabel;
    lblDuration: TLabel;
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    pnlLeft: TPanel;
    lbFolders: TListBox;
    Splitter1: TSplitter;
    flbFiles: TFileListBox;
    pnlPreview: TPanel;
    lblPreview: TLabel;
    imgPreview: TImage;
    SplitterPreview: TSplitter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure cbxLocationsSelect(Sender: TObject);
    procedure cbxLocationsDrawItem(Control: TWinControl;
                                   Index: Integer;
                                   Rect: TRect;
                                   State: TOwnerDrawState);
    procedure edtPathKeyDown(Sender: TObject;
                             var Key: Word;
                             Shift: TShiftState);
    procedure lbFoldersDrawItem(Control: TWinControl;
                                Index: Integer;
                                Rect: TRect;
                                State: TOwnerDrawState);
    procedure flbFilesChange(Sender: TObject);
    procedure flbFilesDblClick(Sender: TObject);
    procedure btnScanNetworkClick(Sender: TObject);
    procedure btnNetStationsSearchClick(Sender: TObject);
    procedure lbFoldersDblClick(Sender: TObject);

  private

    FSelectedFilter: TLWFileBrowserExFilter;
    FSelectedFile: TFileName;
    FSelectedFilePath: string;
    FFileDuration: string;
    FCurrentDirectory: string;
    FRootPath: string;
    FSmallSysImages: HIMAGELIST;
    FUpdatingUi: Boolean;
    FNetworkScanRunning: Boolean;
    FNetworkFinder: TNetworkDiscoveryThread;

    // Leave code here for network scan example .
    // procedure NetworkDiscoveryFinished(Sender: TObject);
    procedure StartNetworkStationDiscovery();

    procedure InitSystemImageList();
    procedure FillLocationCombo();
    procedure CollectNetworkResourcesToStrings(const AResource: PNetResource;
                                               const ADepth: Integer;
                                               const AItems: TStrings);

    procedure FillNetworkServerFolderList(const AServerName: string);
    function IsNetworkServerPath(const ARemoteName: string): Boolean;
    function IsSelectableNetworkShare(const ARemoteName: string): Boolean;
    function GetRemoteHostName(const APath: string): string;
    function GetLocalHostName(): string;
    function TryResolveIPv4Address(const AHostName: string;
                                   out AIPv4Address: string): Boolean;
    procedure UpdateIPv4Address(const APath: string);
    procedure NetworkStationFound(const AStation: string);

    procedure AddLocationItem(const APath: string);
    procedure AddCurrentLocationIfNeeded(const APath: string);
    procedure FillFolderList();
    procedure ClearSelection();
    procedure UpdateOkState();
    procedure UpdatePreview();
    procedure UpdateDuration();
    procedure SetSelectedFilter(AValue: TLWFileBrowserExFilter);
    function GetSelectedFilter(): TLWFileBrowserExFilter;
    function IsGraphicFile(const AFileName: string): Boolean;
    function IsAudioFile(const AFileName: string): Boolean;
    function IsUncPath(const APath: string): Boolean;
    function NormalizeDirectory(const ADirectory: string): string;
    function TrySetBrowserDirectory(const ADirectory: string;
                                    const AShowError: Boolean): Boolean;
    function GetDisplayPathFromLocationItem(const AText: string): string;
    function GetShellIconIndex(const APath: string;
                               const AAttributes: DWORD;
                               const AUseAttributes: Boolean): Integer;
    procedure DrawShellItem(ACanvas: TCanvas;
                            const ARect: TRect;
                            const AText: string;
                            const AIconIndex: Integer;
                            const ASelected: Boolean);


  public

    procedure SetInitialDirectory(const ADirectory: string);

  published

    property FileFilter: TLWFileBrowserExFilter read GetSelectedFilter write SetSelectedFilter default fbxAudio;
    property FileName: TFileName read FSelectedFile;
    property FileURI: string read FSelectedFilePath;
    property AudioDuration: string read FFileDuration;
  end;

function BrowseLWFileEx(const AOwner: TComponent;
                        const AFilter: TLWFileBrowserExFilter;
                        out AFileName: TFileName;
                        out AFileURI: string;
                        out AAudioDuration: string;
                        const AInitialDirectory: string = ''): Boolean;

var
  DlgLWFileBrowserEx: TLWFileBrowserExDlg;


implementation

{$R *.dfm}


function BrowseLWFileEx(const AOwner: TComponent;
                        const AFilter: TLWFileBrowserExFilter;
                        out AFileName: TFileName;
                        out AFileURI: string;
                        out AAudioDuration: string;
                        const AInitialDirectory: string = ''): Boolean;
var
  Dlg: TLWFileBrowserExDlg;

begin

  AFileName := '';
  AFileURI := '';
  AAudioDuration := '';

  Dlg := TLWFileBrowserExDlg.Create(AOwner);

  try

    Dlg.FileFilter := AFilter;
    Dlg.SetInitialDirectory(AInitialDirectory);

    Result := Dlg.ShowModal() = mrOk;

    if Result then
      begin

        AFileName := Dlg.FileName;
        AFileURI := Dlg.FileURI;
        AAudioDuration := Dlg.AudioDuration;
      end;
  finally

    Dlg.Free();
  end;
end;


procedure TLWFileBrowserExDlg.FormCreate(Sender: TObject);
begin

  FUpdatingUi := False;
  FNetworkScanRunning := False;
  FSelectedFilter := fbxAudio;
  FSelectedFile := '';
  FSelectedFilePath := '';
  FFileDuration := '';
  FCurrentDirectory := '';

  InitSystemImageList();
  FillLocationCombo();
  SetSelectedFilter(fbxAudio);
  StartNetworkStationDiscovery();

  if not TrySetBrowserDirectory(ExtractFilePath(ParamStr(0)),
                                False) then
    TrySetBrowserDirectory('C:\',
                           False);

  // Set index to the first drive.
  cbxLocations.ItemIndex := 0;
  // Refresh
  TrySetBrowserDirectory(FRootPath,
                         True);
  cbxLocationsSelect(Self);
  lbFoldersDblClick(nil);
  UpdateOkState();
end;


procedure TLWFileBrowserExDlg.FormDestroy(Sender: TObject);
begin

  if Assigned(FNetworkFinder) then
    begin

      //FNetworkFinder.OnStationFound := nil;
      //FNetworkFinder.OnFinished := nil;
      //FNetworkFinder.Terminate();
      //FNetworkFinder.WaitFor();
      //FreeAndNil(FNetworkFinder);
    end;

  // NOTE: FSmallSysImages is the shared system image list. Do not destroy it.
end;


procedure TLWFileBrowserExDlg.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TLWFileBrowserExDlg.btnOkClick(Sender: TObject);
begin

  if FileExists(FSelectedFilePath) then
    ModalResult := mrOk;
end;


procedure TLWFileBrowserExDlg.btnGoClick(Sender: TObject);
var
  PathText: string;

begin

  PathText := NormalizeDirectory(edtPath.Text);

  if IsNetworkServerPath(PathText) then
    begin

      FillNetworkServerFolderList(PathText);
      Exit;
    end;

  TrySetBrowserDirectory(PathText,
                         True);
end;


procedure TLWFileBrowserExDlg.cbxLocationsSelect(Sender: TObject);
begin

  if FUpdatingUi then
    Exit;

  FRootPath := GetDisplayPathFromLocationItem(cbxLocations.Text);
  UpdateIPv4Address(FRootPath);

  if IsNetworkServerPath(FRootPath) then
    begin
      FillNetworkServerFolderList(FRootPath);
      Exit;
    end;

  TrySetBrowserDirectory(FRootPath,
                         True);
end;


function TLWFileBrowserExDlg.GetRemoteHostName(const APath: string): string;
const
  CRemotePathBufferSize = 32768;

var
  PathText: string;
  RemotePath: string;
  LocalDevice: string;
  BufferSize: DWORD;
  P: Integer;

begin

  Result := '';
  PathText := GetDisplayPathFromLocationItem(APath);

  { Convert a mapped network drive, such as Z:, to its UNC path. }
  if (Length(PathText) >= 2) and
     (PathText[2] = ':') and
     (GetDriveType(PChar(Copy(PathText, 1, 3))) = DRIVE_REMOTE) then
    begin

      LocalDevice := Copy(PathText,
                          1,
                          2);
      BufferSize := CRemotePathBufferSize;
      SetLength(RemotePath,
                BufferSize);

      if (WNetGetConnection(PChar(LocalDevice),
                            PChar(RemotePath),
                            BufferSize) <> NO_ERROR) then
        Exit;

      SetLength(RemotePath,
                StrLen(PChar(RemotePath)));
      PathText := RemotePath;
    end;

  if not IsUncPath(PathText) then
    Exit;

  Delete(PathText,
         1,
         2);
  P := Pos('\',
           PathText);

  if (P > 0) then
    Result := Copy(PathText,
                   1,
                   P - 1)
  else
    Result := PathText;

  Result := Trim(Result);
end;


function TLWFileBrowserExDlg.GetLocalHostName(): string;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  ComputerNameLength: DWORD;

begin

  Result := '';
  ComputerNameLength := Length(ComputerName);

  if GetComputerName(ComputerName,
                     ComputerNameLength) then
    SetString(Result,
              ComputerName,
              ComputerNameLength);
end;


function TLWFileBrowserExDlg.TryResolveIPv4Address(const AHostName: string;
                                                   out AIPv4Address: string): Boolean;
var
  WsaData: TWSAData;
  HostName: AnsiString;
  HostEntry: PHostEnt;
  Address: PInAddr;

begin

  Result := False;
  AIPv4Address := '';

  if (Trim(AHostName) = '') then
    Exit;

  if (WSAStartup($0202,
                 WsaData) <> 0) then
    Exit;

  try

    HostName := AnsiString(AHostName);
    HostEntry := gethostbyname(PAnsiChar(HostName));

    if not Assigned(HostEntry) or
       (HostEntry^.h_addrtype <> AF_INET) or
       not Assigned(HostEntry^.h_addr_list) or
       not Assigned(HostEntry^.h_addr_list^) then
      Exit;

    Address := PInAddr(HostEntry^.h_addr_list^);
    AIPv4Address := string(AnsiString(inet_ntoa(Address^)));
    Result := AIPv4Address <> '';
  finally

    WSACleanup();
  end;
end;


procedure TLWFileBrowserExDlg.UpdateIPv4Address(const APath: string);
var
  HostName: string;
  IPv4Address: string;
  PathText: string;

begin

  edtIPv4Address.Clear();
  HostName := GetRemoteHostName(APath);

  { A drive path without a remote host belongs to this computer. }
  if (HostName = '') then
    begin

      PathText := GetDisplayPathFromLocationItem(APath);

      if (Length(PathText) >= 2) and
         (PathText[2] = ':') then
        HostName := GetLocalHostName();
    end;

  if (HostName = '') then
    Exit;

  if TryResolveIPv4Address(HostName,
                           IPv4Address) then
    edtIPv4Address.Text := IPv4Address
  else
    edtIPv4Address.Text := 'Not found';
end;


procedure TLWFileBrowserExDlg.edtPathKeyDown(Sender: TObject;
                                            var Key: Word;
                                            Shift: TShiftState);
var
  PathText: string;

begin

  if (Key = VK_RETURN) then
    begin
      Key := 0;

      PathText := NormalizeDirectory(edtPath.Text);

      if IsNetworkServerPath(PathText) then
        begin
          FillNetworkServerFolderList(PathText);
          Exit;
        end;

      TrySetBrowserDirectory(PathText,
                             True);
    end;
end;


procedure TLWFileBrowserExDlg.flbFilesChange(Sender: TObject);
begin

  FSelectedFile := '';
  FSelectedFilePath := '';
  FFileDuration := '';

  if (flbFiles.ItemIndex >= 0) then
    begin
      FSelectedFile := flbFiles.Items[flbFiles.ItemIndex];
      FSelectedFilePath := IncludeTrailingPathDelimiter(FCurrentDirectory) + FSelectedFile;

      lblSelectedFile.Caption := FSelectedFile;
      lblSelectedFile.Hint := FSelectedFilePath;

      UpdateDuration();
    end
  else
    begin
      lblSelectedFile.Caption := 'Selected file:';
      lblSelectedFile.Hint := '';
      lblDuration.Caption := 'Duration: 00:00:00';
    end;

  UpdatePreview();
  UpdateOkState();
end;


procedure TLWFileBrowserExDlg.flbFilesDblClick(Sender: TObject);
begin

  if btnOk.Enabled then
    btnOkClick(Sender);
end;


// Scan network for network stations (>= Win 10)
procedure TLWFileBrowserExDlg.StartNetworkStationDiscovery();
begin

  if FNetworkScanRunning then
    Exit;

  if Assigned(FNetworkFinder) then
    Exit;

  FNetworkScanRunning := True;
  FNetworkFinder := TNetworkDiscoveryThread.Create();
  FNetworkFinder.FreeOnTerminate := True;
  FNetworkFinder.OnStationFound := NetworkStationFound;
  FNetworkFinder.Start();
end;


procedure TLWFileBrowserExDlg.InitSystemImageList();
var
  SFI: TSHFileInfo;

begin

  ZeroMemory(@SFI,
             SizeOf(SFI));

  FSmallSysImages := SHGetFileInfo('C:\',
                                   FILE_ATTRIBUTE_DIRECTORY,
                                   SFI,
                                   SizeOf(SFI),
                                   SHGFI_SYSICONINDEX or
                                   SHGFI_SMALLICON or
                                   SHGFI_USEFILEATTRIBUTES);
end;


procedure TLWFileBrowserExDlg.FillLocationCombo();
var
  DriveBits: DWORD;
  I: Integer;
  Root: string;
  DriveType: UINT;
  DisplayText: string;

begin

  cbxLocations.Items.BeginUpdate();

  try
    cbxLocations.Items.Clear();

    DriveBits := GetLogicalDrives();

    for I := 0 to 25 do
      begin
        if (DriveBits and (DWORD(1) shl I)) = 0 then
          Continue;

        Root := Char(Ord('A') + I) + ':\';
        DriveType := GetDriveType(PChar(Root));

        case DriveType of
          DRIVE_FIXED:
            DisplayText := Root + '  Local disk';
          DRIVE_REMOVABLE:
            DisplayText := Root + '  Removable disk';
          DRIVE_CDROM:
            DisplayText := Root + '  CD/DVD';
          DRIVE_REMOTE:
            DisplayText := Root + '  Network drive';
          DRIVE_RAMDISK:
            DisplayText := Root + '  RAM disk';
        else
          DisplayText := Root + '  Drive';
        end;

        cbxLocations.Items.Add(DisplayText);
      end;

  finally

    cbxLocations.Items.EndUpdate();
  end;
end;


function TLWFileBrowserExDlg.IsNetworkServerPath(const ARemoteName: string): Boolean;
var
  S: string;
  SlashCount: Integer;
  I: Integer;

begin

  Result := False;
  S := ExcludeTrailingPathDelimiter(Trim(ARemoteName));

  if not IsUncPath(S) then
    Exit;

  SlashCount := 0;

  for I := 1 to Length(S) do
    begin
      if (S[I] = '\') then
        Inc(SlashCount);
    end;

  { \SERVER has exactly two slash characters. }
  Result := SlashCount = 2;
end;


function TLWFileBrowserExDlg.IsSelectableNetworkShare(const ARemoteName: string): Boolean;
var
  S: string;
  SlashCount: Integer;
  I: Integer;

begin

  Result := False;
  S := ExcludeTrailingPathDelimiter(Trim(ARemoteName));

  if not IsUncPath(S) then
    Exit;

  SlashCount := 0;

  for I := 1 to Length(S) do
    begin
      if S[I] = '\' then
        Inc(SlashCount);
    end;

  { \SERVER\Share has at least three slash characters. }
  Result := SlashCount >= 3;
end;


procedure TLWFileBrowserExDlg.NetworkStationFound(const AStation: string);
begin

  AddLocationItem(AStation);
end;


procedure TLWFileBrowserExDlg.btnScanNetworkClick(Sender: TObject);
begin

  StartNetworkStationDiscovery();
end;


procedure TLWFileBrowserExDlg.CollectNetworkResourcesToStrings(const AResource: PNetResource;
                                                               const ADepth: Integer;
                                                               const AItems: TStrings);
const
  CBufferSize = 64 * 1024;
  CMaxDepth = 5;

var
  EnumHandle: THandle;
  EnumResult: DWORD;
  Count: DWORD;
  BufferSize: DWORD;
  Buffer: Pointer;
  NetRes: PNetResource;
  I: DWORD;
  RemoteName: string;
  IsContainer: Boolean;

begin

  if (AItems = nil) then
    Exit;

  if (ADepth > CMaxDepth) then
    Exit;

  EnumHandle := 0;

  EnumResult := WNetOpenEnum(RESOURCE_GLOBALNET,
                             RESOURCETYPE_ANY,
                             0,
                             AResource,
                             EnumHandle);

  if (EnumResult <> NO_ERROR) then
    Exit;

  try

    GetMem(Buffer,
           CBufferSize);
    try
      repeat

        Count := DWORD(-1);
        BufferSize := CBufferSize;

        ZeroMemory(Buffer,
                   BufferSize);

        EnumResult := WNetEnumResource(EnumHandle,
                                       Count,
                                       Buffer,
                                       BufferSize);

        if (EnumResult = NO_ERROR) then
          begin
            NetRes := PNetResource(Buffer);

            for I := 0 to Count - 1 do
              begin
                RemoteName := '';

                if (NetRes^.lpRemoteName <> nil) then
                  RemoteName := Trim(NetRes^.lpRemoteName);

                if (IsNetworkServerPath(RemoteName) or
                    IsSelectableNetworkShare(RemoteName)) and
                   (AItems.IndexOf(RemoteName) < 0) then
                  AItems.Add(RemoteName);

                IsContainer := (NetRes^.dwUsage and RESOURCEUSAGE_CONTAINER) <> 0;

                if IsContainer then
                  CollectNetworkResourcesToStrings(NetRes,
                                                   ADepth + 1,
                                                   AItems);

                Inc(NetRes);
              end;
          end;

      until EnumResult = ERROR_NO_MORE_ITEMS;
    finally

      FreeMem(Buffer);
    end;

  finally

    WNetCloseEnum(EnumHandle);
  end;
end;


procedure TLWFileBrowserExDlg.FillNetworkServerFolderList(const AServerName: string);
var
  ServerPath: string;
  Buffer: Pointer;
  EntriesRead: DWORD;
  TotalEntries: DWORD;
  ResumeHandle: DWORD;
  Status: DWORD;
  ShareInfo: PLWShareInfo1;
  I: DWORD;
  ShareName: string;
  SharePath: string;

begin

  ServerPath := ExcludeTrailingPathDelimiter(Trim(AServerName));

  if not IsNetworkServerPath(ServerPath) then
    Exit;

  FUpdatingUi := True;

  try

    FCurrentDirectory := ServerPath;
    edtPath.Text := ServerPath;
    flbFiles.Clear();
    ClearSelection();

    lbFolders.Items.BeginUpdate();
    cbxLocations.Items.BeginUpdate();

    try

      lbFolders.Items.Clear();
      AddLocationItem(ServerPath);

      ResumeHandle := 0;

      repeat

        Buffer := nil;
        EntriesRead := 0;
        TotalEntries := 0;

        Status := NetShareEnum(PWideChar(WideString(ServerPath)),
                               1,
                               Buffer,
                               LW_MAX_PREFERRED_LENGTH,
                               EntriesRead,
                               TotalEntries,
                               ResumeHandle);

        if (Status <> LW_NERR_SUCCESS) and
           (Status <> LW_ERROR_MORE_DATA) then
          Break;

        try
          ShareInfo := PLWShareInfo1(Buffer);

          for I := 0 to EntriesRead - 1 do
            begin

              if (ShareInfo^.shi1_netname <> nil) then
                begin
                  if ((ShareInfo^.shi1_type and LW_STYPE_MASK) = LW_STYPE_DISKTREE) then
                    begin

                      ShareName := string(ShareInfo^.shi1_netname);

                      if (ShareName <> '') and
                         (ShareName[Length(ShareName)] <> '$') then
                        begin
                          SharePath := ServerPath + '\' + ShareName;

                          if lbFolders.Items.IndexOf(ShareName) < 0 then
                            lbFolders.Items.Add(ShareName);

                          AddLocationItem(SharePath);
                        end;
                    end;
                end;

              Inc(ShareInfo);
            end;
        finally

          if (Buffer <> nil) then
            NetApiBufferFree(Buffer);
        end;

      until (Status <> LW_ERROR_MORE_DATA);
    finally

      cbxLocations.Items.EndUpdate();
      lbFolders.Items.EndUpdate();
    end;

  finally

    FUpdatingUi := False;
  end;
end;


procedure TLWFileBrowserExDlg.AddLocationItem(const APath: string);
begin

  if (Trim(APath) = '') then
    Exit;

  if (cbxLocations.Items.IndexOf(APath) < 0) then
    cbxLocations.Items.Add(APath);
end;


procedure TLWFileBrowserExDlg.AddCurrentLocationIfNeeded(const APath: string);
var
  PathText: string;

begin

  PathText := NormalizeDirectory(APath);

  if IsUncPath(PathText) then
    AddLocationItem(PathText);
end;


procedure TLWFileBrowserExDlg.FillFolderList();
var
  SR: TSearchRec;
  SearchPath: string;
  ParentDir: string;

begin

  lbFolders.Items.BeginUpdate();

  try
    lbFolders.Items.Clear();

    ParentDir := ExtractFileDir(ExcludeTrailingPathDelimiter(FCurrentDirectory));

    if (ParentDir <> '') and
       (ParentDir <> ExcludeTrailingPathDelimiter(FCurrentDirectory)) then
      lbFolders.Items.Add('..');

    SearchPath := IncludeTrailingPathDelimiter(FCurrentDirectory) + '*.*';

    if FindFirst(SearchPath,
                 faDirectory,
                 SR) = 0 then
      try

        repeat
          if ((SR.Attr and faDirectory) <> 0) and
             (SR.Name <> '.') and
             (SR.Name <> '..') then
            lbFolders.Items.Add(SR.Name);
        until (FindNext(SR) <> 0);
      finally

        FindClose(SR);
      end;

  finally

    lbFolders.Items.EndUpdate();
  end;
end;


procedure TLWFileBrowserExDlg.ClearSelection();
begin

  FSelectedFile := '';
  FSelectedFilePath := '';
  FFileDuration := '';

  lblSelectedFile.Caption := 'Selected file:';
  lblSelectedFile.Hint := '';

  if (FSelectedFilter = fbxAudio) then
    lblDuration.Caption := 'Duration: 00:00:00'
  else
    lblDuration.Caption := 'Image preview';

  if Assigned(imgPreview.Picture) then
    imgPreview.Picture.Assign(nil);

  UpdateOkState();
end;


procedure TLWFileBrowserExDlg.UpdateOkState();
begin

  btnOk.Enabled := FileExists(FSelectedFilePath);
end;


procedure TLWFileBrowserExDlg.UpdatePreview();
begin

  if not Assigned(imgPreview) then
    Exit;

  imgPreview.Picture.Assign(nil);

  if (FSelectedFilePath = '') then
    Exit;

  if not FileExists(FSelectedFilePath) then
    Exit;

  if not IsGraphicFile(FSelectedFilePath) then
    Exit;

  try

    imgPreview.Picture.LoadFromFile(FSelectedFilePath);
  except

    imgPreview.Picture.Assign(nil);
  end;
end;


procedure TLWFileBrowserExDlg.UpdateDuration();
var
  Duration: Int64;

begin

  FFileDuration := '';

  if (FSelectedFilter <> fbxAudio) then
    begin
      lblDuration.Caption := 'Image preview';
      Exit;
    end;

  if not IsAudioFile(FSelectedFilePath) then
    begin
      lblDuration.Caption := 'Duration: 00:00:00';
      Exit;
    end;

  Duration := 0;

  try

    GetFileDuration(PWideChar(WideString(FSelectedFilePath)),
                    Duration);

    FFileDuration := HnsTimeToStr(Duration,
                                  False);

    lblDuration.Caption := Format('Duration: %s',
                                  [FFileDuration]);
  except

    FFileDuration := '';
    lblDuration.Caption := 'Duration: 00:00:00';
  end;
end;


procedure TLWFileBrowserExDlg.SetSelectedFilter(AValue: TLWFileBrowserExFilter);
begin

  FSelectedFilter := AValue;

  case FSelectedFilter of
    fbxAudio: begin
                Caption := 'Select an audio file';
                cbxFileFilter.Filter := CLWAudioFilter;
                pnlPreview.Visible := False;
                SplitterPreview.Visible := False;
                lblDuration.Caption := 'Duration: 00:00:00';
              end;

    fbxGraphics: begin
                   Caption := 'Select an image file';
                   cbxFileFilter.Filter := CLWGraphicsFilter;
                   pnlPreview.Visible := True;
                   SplitterPreview.Visible := True;
                   lblDuration.Caption := 'Image preview';
                 end;
  end;

  ClearSelection();
end;


function TLWFileBrowserExDlg.GetSelectedFilter(): TLWFileBrowserExFilter;
begin

  Result := FSelectedFilter;
end;


function TLWFileBrowserExDlg.IsGraphicFile(const AFileName: string): Boolean;
var
  Ext: string;

begin

  Ext := LowerCase(ExtractFileExt(AFileName));

  Result := (Ext = '.jpg') or
            (Ext = '.jpeg') or
            (Ext = '.png') or
            (Ext = '.bmp');
end;


function TLWFileBrowserExDlg.IsAudioFile(const AFileName: string): Boolean;
var
  Ext: string;

begin

  Ext := LowerCase(ExtractFileExt(AFileName));

  Result := (Ext = '.mp3') or
            (Ext = '.wav') or
            (Ext = '.flac') or
            (Ext = '.ogg') or
            (Ext = '.m4a') or
            (Ext = '.aac') or
            (Ext = '.wma') or
            (Ext = '.opus');
end;


function TLWFileBrowserExDlg.IsUncPath(const APath: string): Boolean;
begin

  Result := Copy(Trim(APath),
                 1,
                 2) = '\\';
end;


function TLWFileBrowserExDlg.NormalizeDirectory(const ADirectory: string): string;
begin

  Result := Trim(ADirectory);

  if (Result = '') then
    Exit;

  if (Length(Result) = 2) and
     (Result[2] = ':') then
    Result := Result + '\';

  if (Length(Result) >= 3) and
     (Result[2] = ':') and
     (Result[3] <> '\') then
    Result := Copy(Result, 1, 2) + '\';
end;


function TLWFileBrowserExDlg.TrySetBrowserDirectory(const ADirectory: string;
                                                    const AShowError: Boolean): Boolean;
var
  Dir: string;

begin

  Result := False;
  Dir := NormalizeDirectory(ADirectory);

  if (Dir = '') then
    Exit;

  if IsNetworkServerPath(Dir) then
    begin
      FillNetworkServerFolderList(Dir);
      Exit(True);
    end;

  if not System.SysUtils.DirectoryExists(Dir) then
    begin
      if AShowError then
        MessageDlg('Directory not found:'#13#10 + Dir,
                   mtWarning,
                   [mbOk],
                   0);
      Exit;
    end;

  FUpdatingUi := True;

  try

    FCurrentDirectory := IncludeTrailingPathDelimiter(Dir);
    edtPath.Text := FCurrentDirectory;

    AddCurrentLocationIfNeeded(FCurrentDirectory);

    flbFiles.Directory := FCurrentDirectory;
    FillFolderList();
    ClearSelection();
  finally

    FUpdatingUi := False;
  end;

  Result := True;
end;


function TLWFileBrowserExDlg.GetDisplayPathFromLocationItem(const AText: string): string;
var
  P: Integer;

begin

  Result := Trim(AText);

  if (Result = '') then
    Exit;

  if IsUncPath(Result) then
    Exit;

  P := Pos('  ',
           Result);
  if (P > 0) then
    Result := Copy(Result,
                   1,
                   P - 1);
end;


function TLWFileBrowserExDlg.GetShellIconIndex(const APath: string;
                                               const AAttributes: DWORD;
                                               const AUseAttributes: Boolean): Integer;
var
  SFI: TSHFileInfo;
  Flags: UINT;

begin

  Result := 0;

  ZeroMemory(@SFI,
             SizeOf(SFI));

  Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON;

  if AUseAttributes then
    Flags := Flags or SHGFI_USEFILEATTRIBUTES;

  if (SHGetFileInfo(PChar(APath),
                   AAttributes,
                   SFI,
                   SizeOf(SFI),
                   Flags) <> 0) then
    Result := SFI.iIcon;
end;


procedure TLWFileBrowserExDlg.DrawShellItem(ACanvas: TCanvas;
                                            const ARect: TRect;
                                            const AText: string;
                                            const AIconIndex: Integer;
                                            const ASelected: Boolean);
var
  TextRect: TRect;
  IconY: Integer;
  Flags: UINT;

begin

  ACanvas.FillRect(ARect);

  IconY := ARect.Top + (((ARect.Bottom - ARect.Top) - GetSystemMetrics(SM_CYSMICON)) div 2);

  if (FSmallSysImages <> 0) then
    ImageList_Draw(FSmallSysImages,
                   AIconIndex,
                   ACanvas.Handle,
                   ARect.Left + 4,
                   IconY,
                   ILD_TRANSPARENT);

  TextRect := ARect;
  TextRect.Left := TextRect.Left + 28;
  TextRect.Right := TextRect.Right - 4;

  Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;

  DrawText(ACanvas.Handle,
           PChar(AText),
           Length(AText),
           TextRect,
           Flags);
end;


procedure TLWFileBrowserExDlg.cbxLocationsDrawItem(Control: TWinControl;
                                                   Index: Integer;
                                                   Rect: TRect;
                                                   State: TOwnerDrawState);
var
  TextValue: string;
  PathValue: string;
  IconIndex: Integer;

begin

  if (Index < 0) then
    Exit;

  TextValue := cbxLocations.Items[Index];
  PathValue := GetDisplayPathFromLocationItem(TextValue);

  if IsUncPath(PathValue) then
    IconIndex := GetShellIconIndex(PathValue,
                                   FILE_ATTRIBUTE_DIRECTORY,
                                   True)
  else
    IconIndex := GetShellIconIndex(PathValue,
                                   FILE_ATTRIBUTE_DIRECTORY,
                                   False);

  DrawShellItem(cbxLocations.Canvas,
                Rect,
                TextValue,
                IconIndex,
                odSelected in State);
end;


procedure TLWFileBrowserExDlg.lbFoldersDblClick(Sender: TObject);
var
  ItemText: string;
  NewDir: string;

begin

  if (lbFolders.ItemIndex < 0) then
    Exit;

  ItemText := lbFolders.Items[lbFolders.ItemIndex];

  if (ItemText = '..') then
    begin
      NewDir := ExtractFileDir(ExcludeTrailingPathDelimiter(FCurrentDirectory));

      if (NewDir <> '') and (NewDir <> FCurrentDirectory) then
        TrySetBrowserDirectory(NewDir,
                               False);
    end
  else
    begin
      NewDir := IncludeTrailingPathDelimiter(FCurrentDirectory) + ItemText;
      TrySetBrowserDirectory(NewDir,
                             True);
    end;
end;


procedure TLWFileBrowserExDlg.lbFoldersDrawItem(Control: TWinControl;
                                                Index: Integer;
                                                Rect: TRect;
                                                State: TOwnerDrawState);
var
  TextValue: string;
  PathValue: string;
  IconIndex: Integer;

begin

  if (Index < 0) then
    Exit;

  TextValue := lbFolders.Items[Index];

  if (TextValue = '..') then
    PathValue := ExtractFileDir(ExcludeTrailingPathDelimiter(FCurrentDirectory))
  else
    PathValue := IncludeTrailingPathDelimiter(FCurrentDirectory) + TextValue;

  IconIndex := GetShellIconIndex(PathValue,
                                 FILE_ATTRIBUTE_DIRECTORY,
                                 True);

  DrawShellItem(lbFolders.Canvas,
                Rect,
                TextValue,
                IconIndex,
                odSelected in State);
end;


procedure TLWFileBrowserExDlg.btnNetStationsSearchClick(Sender: TObject);
begin

  StartNetworkStationDiscovery();
end;


procedure TLWFileBrowserExDlg.SetInitialDirectory(const ADirectory: string);
begin

  if (Trim(ADirectory) <> '') then
    TrySetBrowserDirectory(ADirectory,
                           False);
end;

end.
