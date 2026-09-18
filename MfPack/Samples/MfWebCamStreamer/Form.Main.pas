// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.Main.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Webcam/microphone capture published through FxServe.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
unit Form.Main;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ShellAPI,
  WinApi.WinSock,
  WinApi.WinNetWk,
  {System}
  System.Classes,
  System.IniFiles,
  System.SysUtils,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Vcl.ExtCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  {Application}
  SimpleAvCapture,
  FxServePublisher;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    pnlPreview: TPanel;
    lblCamera: TLabel;
    lblMicrophone: TLabel;
    lblPublishFolder: TLabel;
    lblPublicUrl: TLabel;
    cbCamera: TComboBox;
    cbMicrophone: TComboBox;
    edPublishFolder: TEdit;
    btnBrowsePublishFolder: TButton;
    edPublicUrl: TEdit;
    btnStart: TButton;
    btnStop: TButton;
    btnOpenBrowser: TButton;
    chkMonitorAudio: TCheckBox;
    memStatus: TMemo;
    tmrStatus: TTimer;
    btnSelectBrowserUrl: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnBrowsePublishFolderClick(Sender: TObject);
    procedure btnOpenBrowserClick(Sender: TObject);
    procedure tmrStatusTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSelectBrowserUrlClick(Sender: TObject);

  private

    FVideoDevices: TSimpleDeviceList;
    FAudioDevices: TSimpleDeviceList;
    FCapture: TSimpleAvCapture;
    FPublisher: TFxServePublisher;
    FLastFragmentCount: UInt64;
    FLastPublisherError: string;
    FLastPreviewError: HRESULT;
    FPreviewWarningShown: Boolean;
    FOldPreviewWindowProc: TWndMethod;

    procedure FillDeviceLists();
    procedure AddStatus(const S: string);
    procedure UpdateControls();
    procedure StopStreaming();
    function IniFileName(): string;
    procedure LoadSettings();
    procedure SaveSettings();
    function DefaultPublishFolder(): string;
    function FindPlayerSource(): string;
    function GetHostNameFromPath(const APath: string;
                                 out AHostName: string): Boolean;
    function ResolveIPv4Address(const AHostName: string;
                                out AAddress: string): Boolean;
    function ReadFxServeLanUrl(const AWebCamFolder: string;
                               out ALanUrl: string): Boolean;
    function BuildBrowserUrl(const ALanUrl: string;
                             const AIPv4Address: string): string;
    procedure PreviewWindowProc(var Message: TMessage);
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

const
  WEBCAM_STREAM_PAGE = 'webcam_stream.html';
  WEBCAM_STREAM_URL_PATH = '/WebCam/webcam_stream.html';
  SETTINGS_SECTION = 'FxServe';
  SETTINGS_FOLDER = 'Folder';
  SETTINGS_BROWSER_URL = 'BrowserUrl';


procedure TfrmMain.AddStatus(const S: string);
var
  OldSelStart: Integer;
  OldSelLength: Integer;

begin

  OldSelStart := memStatus.SelStart;
  OldSelLength := memStatus.SelLength;
  memStatus.Lines.Add(FormatDateTime('hh:nn:ss',
                                     Now) + '  ' + S);

  if (OldSelLength > 0) then
    begin
      memStatus.SelStart := OldSelStart;
      memStatus.SelLength := OldSelLength;
    end
  else
    begin
      memStatus.SelStart := Length(memStatus.Text);
      memStatus.Perform(EM_SCROLLCARET,
                        0,
                        0);
    end;
end;


function TfrmMain.IniFileName(): string;
begin

  Result := ChangeFileExt(Application.ExeName,
                          '.ini');
end;


procedure TfrmMain.LoadSettings();
var
  Ini: TIniFile;

begin

  Ini := TIniFile.Create(IniFileName);
  try
    edPublishFolder.Text := Ini.ReadString(SETTINGS_SECTION,
                                           SETTINGS_FOLDER,
                                           DefaultPublishFolder);
    edPublicUrl.Text := Ini.ReadString(SETTINGS_SECTION,
                                       SETTINGS_BROWSER_URL,
                                       'http://127.0.0.1:8080' +
                                       WEBCAM_STREAM_URL_PATH);
  finally
    Ini.Free;
  end;
end;


procedure TfrmMain.SaveSettings();
var
  Ini: TIniFile;

begin

  Ini := TIniFile.Create(IniFileName);
  try
    Ini.WriteString(SETTINGS_SECTION,
                    SETTINGS_FOLDER,
                    Trim(edPublishFolder.Text));
    Ini.WriteString(SETTINGS_SECTION,
                    SETTINGS_BROWSER_URL,
                    Trim(edPublicUrl.Text));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


function TfrmMain.DefaultPublishFolder(): string;
begin

  // FxServe's self-contained installer uses this location. Do not derive the
  // publication folder from the MfWebCamStreamer executable: Debug/Release
  // output directories are not web roots.
  Result := 'C:\FxServe\www\WebCam';
end;


function TfrmMain.FindPlayerSource(): string;
var
  Candidate: string;

begin

  // Prefer the location explicitly selected by the user. This also ensures
  // Start uses the same page that btnSelectBrowserUrl validated.
  Candidate := IncludeTrailingPathDelimiter(Trim(edPublishFolder.Text)) + WEBCAM_STREAM_PAGE;
  if FileExists(Candidate) then
    Exit(Candidate);

  Candidate := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'www\' + WEBCAM_STREAM_PAGE;
  if FileExists(Candidate) then
    Exit(Candidate);

  Candidate := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + WEBCAM_STREAM_PAGE;
  if FileExists(Candidate) then
    Exit(Candidate);

  Candidate := ExpandFileName(
                 IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                 '..\..\www\' + WEBCAM_STREAM_PAGE);
  if FileExists(Candidate) then
    Exit(Candidate);

  Result := '';
end;


function TfrmMain.GetHostNameFromPath(const APath: string;
                                      out AHostName: string): Boolean;
const
  REMOTE_PATH_BUFFER_SIZE = 32768;

var
  PathText: string;
  RemotePath: string;
  LocalDevice: string;
  BufferSize: DWORD;
  SeparatorPosition: Integer;
  ComputerNameBuffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  ComputerNameLength: DWORD;

begin

  Result := False;
  AHostName := '';
  PathText := Trim(APath);

  // Convert a mapped network drive to its UNC share before extracting the
  // server. This makes X:\FxServe\www\WebCam work like a direct UNC path.
  if (Length(PathText) >= 3) and
     (PathText[2] = ':') and
     (GetDriveType(PChar(Copy(PathText, 1, 3))) = DRIVE_REMOTE) then
    begin
      LocalDevice := Copy(PathText, 1, 2);
      BufferSize := REMOTE_PATH_BUFFER_SIZE;
      SetLength(RemotePath, BufferSize);

      if WNetGetConnection(PChar(LocalDevice),
                           PChar(RemotePath),
                           BufferSize) <> NO_ERROR then
        Exit;

      SetLength(RemotePath, StrLen(PChar(RemotePath)));
      PathText := ExcludeTrailingPathDelimiter(RemotePath) +
                  Copy(PathText, 3, MaxInt);
    end;

  if Copy(PathText, 1, 2) = '\\' then
    begin
      Delete(PathText, 1, 2);
      SeparatorPosition := Pos('\', PathText);

      if SeparatorPosition > 0 then
        AHostName := Copy(PathText, 1, SeparatorPosition - 1)
      else
        AHostName := PathText;
    end
  else
    begin
      // A local FxServe folder still needs the machine's LAN address rather
      // than 127.0.0.1 when the URL is opened on another device.
      ComputerNameLength := Length(ComputerNameBuffer);
      if GetComputerName(ComputerNameBuffer, ComputerNameLength) then
        SetString(AHostName,
                  ComputerNameBuffer,
                  ComputerNameLength);
    end;

  AHostName := Trim(AHostName);
  Result := AHostName <> '';
end;


function TfrmMain.ResolveIPv4Address(const AHostName: string;
                                     out AAddress: string): Boolean;
var
  WsaData: TWSAData;
  HostName: AnsiString;
  HostEntry: PHostEnt;
  Address: PInAddr;

begin

  Result := False;
  AAddress := '';

  if Trim(AHostName) = '' then
    Exit;

  if WSAStartup($0202, WsaData) <> 0 then
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
    AAddress := string(AnsiString(inet_ntoa(Address^)));
    Result := AAddress <> '';
  finally
    WSACleanup();
  end;
end;


function TfrmMain.ReadFxServeLanUrl(const AWebCamFolder: string;
                                    out ALanUrl: string): Boolean;
var
  ConfigFile: string;
  Config: TStringList;
  Text: string;
  Position: Integer;
  EndPosition: Integer;

begin

  Result := False;
  ALanUrl := '';
  ConfigFile := IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(AWebCamFolder))) + 'fxserve-config.json';

  if not FileExists(ConfigFile) then
    Exit;

  Config := TStringList.Create;

  try
    Config.LoadFromFile(ConfigFile);
    Text := Config.Text;
    Position := Pos('"lanUrl"', Text);

    if Position = 0 then
      Exit;

    Text := Copy(Text,
                 Position + Length('"lanUrl"'),
                 MaxInt);
    Position := Pos(':', Text);

    if (Position = 0) then
      Exit;

    Text := Copy(Text,
                 Position + 1,
                 MaxInt);
    Position := Pos('"', Text);

    if (Position = 0) then
      Exit;

    Text := Copy(Text,
                 Position + 1,
                 MaxInt);
    EndPosition := Pos('"', Text);

    if (EndPosition = 0) then
      Exit;

    ALanUrl := Trim(Copy(Text,
                         1,
                         EndPosition - 1));

    Result := (ALanUrl <> '');

  finally
    Config.Free;
  end;
end;


function TfrmMain.BuildBrowserUrl(const ALanUrl: string;
                                  const AIPv4Address: string): string;
var
  BaseUrl: string;
  Scheme: string;
  Authority: string;
  PortSuffix: string;
  Position: Integer;
  SlashPosition: Integer;
  ColonPosition: Integer;

begin

  BaseUrl := Trim(ALanUrl);
  Scheme := 'http://';
  PortSuffix := ':8080';
  Position := Pos('://',
                  BaseUrl);

  if (Position > 0) then
    begin
      Scheme := Copy(BaseUrl,
                     1,
                     Position + 2);

      Authority := Copy(BaseUrl,
                        Position + 3,
                        MaxInt);

      SlashPosition := Pos('/',
                           Authority);

      if (SlashPosition > 0) then
        Authority := Copy(Authority,
                          1,
                          SlashPosition - 1);

      ColonPosition := LastDelimiter(':',
                                     Authority);
      if (ColonPosition > 0) then
        PortSuffix := Copy(Authority,
                           ColonPosition,
                           MaxInt)
      else
        PortSuffix := '';
    end;

  Result := Scheme + AIPv4Address + PortSuffix + WEBCAM_STREAM_URL_PATH;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FVideoDevices := TSimpleDeviceList.Create(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  FAudioDevices := TSimpleDeviceList.Create(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID);

  FCapture := TSimpleAvCapture.Create();
  FPublisher := TFxServePublisher.Create();
  FOldPreviewWindowProc := pnlPreview.WindowProc;
  pnlPreview.WindowProc := PreviewWindowProc;

  FLastFragmentCount := 0;
  FLastPublisherError := '';
  FLastPreviewError := S_OK;
  FPreviewWarningShown := False;

  LoadSettings;

  FillDeviceLists();
  UpdateControls();
  AddStatus('Select the FxServe WebCam folder, then click Start.');
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  tmrStatus.Enabled := False;
  SaveSettings;
  if Assigned(FOldPreviewWindowProc) then
    pnlPreview.WindowProc := FOldPreviewWindowProc;
  StopStreaming();

  FreeAndNil(FPublisher);
  FreeAndNil(FCapture);
  FreeAndNil(FAudioDevices);
  FreeAndNil(FVideoDevices);
end;


procedure TfrmMain.FillDeviceLists();
var
  hr: HResult;
  I: UINT32;
  DeviceName: string;

begin

  cbCamera.Items.BeginUpdate;
  cbMicrophone.Items.BeginUpdate;

  try
    cbCamera.Clear();
    cbMicrophone.Clear();

    hr := FVideoDevices.Enumerate;

    if SUCCEEDED(hr) and (FVideoDevices.Count > 0) then
      begin
        for I := 0 to FVideoDevices.Count - 1 do
          if SUCCEEDED(FVideoDevices.GetFriendlyName(I,
                                                     DeviceName)) then
            cbCamera.Items.Add(DeviceName);
      end;

    hr := FAudioDevices.Enumerate;
    if SUCCEEDED(hr) and (FAudioDevices.Count > 0) then
      begin
        for I := 0 to FAudioDevices.Count - 1 do
          if SUCCEEDED(FAudioDevices.GetFriendlyName(I, DeviceName)) then
            cbMicrophone.Items.Add(DeviceName);
      end;

    if (cbCamera.Items.Count > 0) then
      cbCamera.ItemIndex := 0;

    if (cbMicrophone.Items.Count > 0) then
      cbMicrophone.ItemIndex := 0;

    AddStatus(Format('Found %d camera(s), %d microphone(s).',
                     [cbCamera.Items.Count, cbMicrophone.Items.Count]));
  finally
    cbCamera.Items.EndUpdate;
    cbMicrophone.Items.EndUpdate;
  end;
end;


procedure TfrmMain.UpdateControls();
var
  Capturing: Boolean;

begin

  Capturing := Assigned(FCapture) and (FCapture.State = csCapturing);
  btnStart.Enabled := not Capturing and
                      (cbCamera.ItemIndex >= 0) and
                      (cbMicrophone.ItemIndex >= 0);

  btnStop.Enabled := Capturing;
  btnOpenBrowser.Enabled := (Trim(edPublicUrl.Text) <> '');

  cbCamera.Enabled := not Capturing;
  cbMicrophone.Enabled := not Capturing;
  edPublishFolder.Enabled := not Capturing;
  btnBrowsePublishFolder.Enabled := not Capturing;
  btnSelectBrowserUrl.Enabled := not Capturing;
  chkMonitorAudio.Enabled := not Capturing;
end;


procedure TfrmMain.btnBrowsePublishFolderClick(Sender: TObject);
var
  Folder: string;

begin

  Folder := Trim(edPublishFolder.Text);

  if not System.SysUtils.DirectoryExists(Folder) then
    Folder := ExtractFileDir(Folder);

  if not System.SysUtils.DirectoryExists(Folder) then
    Folder := ExtractFilePath(Application.ExeName);

  if SelectDirectory('Select or create FxServe www\WebCam folder',
                     '',
                     Folder) then
    edPublishFolder.Text := ExcludeTrailingPathDelimiter(Folder);
end;


procedure TfrmMain.btnSelectBrowserUrlClick(Sender: TObject);
var
  SelectedFolder: string;
  WebCamFolder: string;
  PlayerFile: string;
  HostName: string;
  IPv4Address: string;
  LanUrl: string;

begin

  SelectedFolder := Trim(edPublishFolder.Text);

  if not System.SysUtils.DirectoryExists(SelectedFolder) then
    SelectedFolder := ExtractFilePath(Application.ExeName);

  if not SelectDirectory('Select the FxServe WebCam folder',
                         '',
                         SelectedFolder) then
    Exit;

  SelectedFolder := ExcludeTrailingPathDelimiter(SelectedFolder);

  if SameText(ExtractFileName(SelectedFolder), 'WebCam') then
    WebCamFolder := SelectedFolder
  else if System.SysUtils.DirectoryExists(
            IncludeTrailingPathDelimiter(SelectedFolder) + 'WebCam') then
    WebCamFolder := IncludeTrailingPathDelimiter(SelectedFolder) +
                    'WebCam'
  else
    begin
      MessageDlg('The selected location does not contain a WebCam folder.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  PlayerFile := IncludeTrailingPathDelimiter(WebCamFolder) + WEBCAM_STREAM_PAGE;

  if not FileExists(PlayerFile) then
    begin
      MessageDlg(WEBCAM_STREAM_PAGE + ' was not found in:'#13#10 +
                 WebCamFolder,
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  if not GetHostNameFromPath(WebCamFolder, HostName) then
    begin
      MessageDlg('Unable to determine the server for the selected folder.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  if not ResolveIPv4Address(HostName, IPv4Address) then
    begin
      MessageDlg('Unable to resolve the IPv4 address of server "' +
                 HostName + '".',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  // FxServeAdmin writes this file in the www root. It supplies the configured
  // LAN scheme and port. Older installations fall back to HTTP port 8080.
  if not ReadFxServeLanUrl(WebCamFolder, LanUrl) then
    LanUrl := 'http://' + HostName + ':8080/';

  edPublishFolder.Text := WebCamFolder;
  edPublicUrl.Text := BuildBrowserUrl(LanUrl,
                                      IPv4Address);
  AddStatus('FxServe server: ' + HostName + ' (' + IPv4Address + ').');
  AddStatus('Browser URL selected: ' + edPublicUrl.Text);
  UpdateControls;
end;


procedure TfrmMain.btnStartClick(Sender: TObject);
var
  hr: HResult;
  VideoActivate: IMFActivate;
  AudioActivate: IMFActivate;
  PlayerSource: string;
  PublishFolder: string;

begin

  PublishFolder := ExcludeTrailingPathDelimiter(Trim(edPublishFolder.Text));
  if (PublishFolder = '') then
    begin
      MessageDlg('Select an FxServe publication folder.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  PlayerSource := FindPlayerSource;
  if (PlayerSource = '') then
    begin
      MessageDlg(WEBCAM_STREAM_PAGE + ' was not found next to the executable or in the sample folder.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  if not FPublisher.Start(PublishFolder,
                          PlayerSource) then
    begin
      MessageDlg('Unable to prepare FxServe publication.'#13#10 + FPublisher.LastError,
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  hr := FVideoDevices.GetActivate(cbCamera.ItemIndex,
                                  VideoActivate);
  if SUCCEEDED(hr) then
    hr := FAudioDevices.GetActivate(cbMicrophone.ItemIndex,
                                    AudioActivate);

  if SUCCEEDED(hr) then
    hr := FCapture.Start(VideoActivate,
                         AudioActivate,
                         '',
                         pnlPreview.Handle,
                         chkMonitorAudio.Checked);

  if FAILED(hr) then
    begin
      FPublisher.Stop();

      AddStatus(Format('Start failed: 0x%.8x',
                       [Cardinal(HR)]));
      MessageDlg(Format('Unable to start capture.'#13#10'HRESULT = 0x%.8x',
                        [Cardinal(HR)]),
                 mtError,
                 [mbOK],
                 0);
    end
  else
    begin
      FLastFragmentCount := 0;
      FLastPublisherError := '';
      FLastPreviewError := S_OK;
      FPreviewWarningShown := False;
      pnlPreview.Caption := '';
      pnlPreview.Invalidate;
      AddStatus('Capture started: H.264 video + AAC audio.');

      if chkMonitorAudio.Checked then
        AddStatus('Local microphone monitoring is enabled.')
      else
        AddStatus('Local microphone monitoring is disabled.');

      AddStatus('FxServe folder: ' + PublishFolder);
      AddStatus('Session: ' + FPublisher.SessionId);
      AddStatus('Browser: ' + Trim(edPublicUrl.Text));
      FormResize(nil);
    end;

  UpdateControls();
end;


procedure TfrmMain.StopStreaming();
var
  hr: HResult;
  PublishedCount: Integer;

begin

  if Assigned(FCapture) and (FCapture.State = csCapturing) then
    begin
      if Assigned(FPublisher) and FPublisher.Active then
        FPublisher.Service(FCapture,
                           PublishedCount);

      if Assigned(FPublisher) then
        FPublisher.Stop;

      hr := FCapture.Stop;
      if FAILED(hr) then
        AddStatus(Format('Stop/finalize failed: 0x%.8x',
                         [Cardinal(hr)]))
      else
        AddStatus('Capture stopped; live.json now reports live=false.');

      pnlPreview.Caption := 'Camera preview appears when streaming';
      pnlPreview.Invalidate;
    end
  else if Assigned(FPublisher) then
    FPublisher.Stop;
end;


procedure TfrmMain.btnStopClick(Sender: TObject);
begin

  StopStreaming();
  UpdateControls();
end;


procedure TfrmMain.btnOpenBrowserClick(Sender: TObject);
var
  Url: string;

begin

  Url := Trim(edPublicUrl.Text);
  if (Pos('http://',
          LowerCase(Url)) <> 1) and
     (Pos('https://',
          LowerCase(Url)) <> 1) then
    begin
      MessageDlg('Browser URL must be an HTTP or HTTPS address served by FxServe.'#13#10#13#10 +
                 'Example:'#13#10 +
                 'http://192.168.50.101:8080' + WEBCAM_STREAM_URL_PATH,
                 mtError,
                 [mbOK],
                 0);

      edPublicUrl.SetFocus;
      Exit;
    end;

  ShellExecute(Handle,
               'open',
               PChar(Url),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmMain.tmrStatusTimer(Sender: TObject);
var
  InitBytes: Integer;
  FragmentBytes: Integer;
  FragmentCount: UInt64;
  TotalBytes: UInt64;
  PublishedCount: Integer;

begin

  if Assigned(FCapture) and (FCapture.State = csCapturing) then
    begin
      if not FPublisher.Service(FCapture,
                                PublishedCount) then
        begin
          if (FPublisher.LastError <> FLastPublisherError) then
            begin
              FLastPublisherError := FPublisher.LastError;
              AddStatus('Publish error: ' + FLastPublisherError);
            end;
        end
      else
        if (PublishedCount > 0) then
          AddStatus(Format('Published through FxServe: fragments %d-%d.',
                           [FPublisher.FirstPublishedSequence, FPublisher.LastPublishedSequence]));

      Caption := Format('MfWebCamStreamer - Video %d / Audio %d / Preview %d',
                        [FCapture.VideoSamples,
                         FCapture.AudioSamples,
                         FCapture.PreviewVideoSamples]);

      if (FCapture.PreviewLastError <> S_OK) and
         (FCapture.PreviewLastError <> FLastPreviewError) then
        begin
          FLastPreviewError := FCapture.PreviewLastError;
          AddStatus(Format('EVR preview error: 0x%.8x',
                           [Cardinal(FLastPreviewError)]));
        end
      else if not FPreviewWarningShown and
              (FCapture.VideoSamples >= 30) and
              (FCapture.PreviewVideoSamples = 0) then
        begin
          FPreviewWarningShown := True;
          AddStatus(Format('EVR preview has no frames: requests=%d, preroll=%s, clock=%s.',
                           [FCapture.PreviewVideoRequests,
                            BoolToStr(FCapture.PreviewVideoPrerolled, True),
                            BoolToStr(FCapture.PreviewClockStarted, True)]));
        end;

      if FCapture.GetFmp4Diagnostics(InitBytes,
                                     FragmentBytes,
                                     FragmentCount,
                                     TotalBytes) and
         (FragmentCount <> FLastFragmentCount) then
        FLastFragmentCount := FragmentCount;
    end
  else
    Caption := 'MfWebCamStreamer';
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin

  if Assigned(FCapture) and Assigned(pnlPreview) then
    FCapture.ResizePreview(pnlPreview.ClientWidth,
                           pnlPreview.ClientHeight);
end;


procedure TfrmMain.PreviewWindowProc(var Message: TMessage);
begin

  if (Message.Msg = WM_ERASEBKGND) and
     Assigned(FCapture) and
     (FCapture.State = csCapturing) and
     (FCapture.PreviewVideoSamples > 0) then
    begin
      Message.Result := 1;
      Exit;
    end;

  FOldPreviewWindowProc(Message);

  if (Message.Msg = WM_PAINT) and
     Assigned(FCapture) and
     (FCapture.State = csCapturing) then
    FCapture.RepaintPreview();
end;

end.
