// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgMediaServer.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Media server dialog GUI -handles IceCast and Caddy- unit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
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
// Source: -
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
unit dlgMediaServer;

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WinSock,
  WinApi.WinError,
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
  Vcl.ExtCtrls,
  {Application}
  MPxpButton,
  RDJ_Common,
  MfIcecastServerManager;

type

  TfrmMediaServer = class(TForm)
    pnlIceCast: TPanel;
    pnlCaption: TPanel;
    shpOnAirCap: TShape;
    lblCaption: TLabel;
    shpOnAir: TShape;
    lblOnAir: TLabel;
    btnMinimize: TMPxpButton;
    Panel1: TPanel;
    lblIcecastServerStatus: TLabel;
    memIcecastLog: TMemo;
    Panel2: TPanel;
    Bevel3: TBevel;
    Label4: TLabel;
    chkBroadcast: TMPxpButton;
    chkStartStopServer: TMPxpButton;
    chkAutoRestart: TMPxpButton;

    procedure chkStartStopServerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkBroadcastClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkAutoRestartClick(Sender: TObject);
    procedure pnlCaptionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnMinimizeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    { Private declarations }

    // Icecast
    FUpdatingBroadcastUi: Boolean;
    // IceCast server
    FIcecastMgr: TMfIcecastServerManager;
    // Caddy output log when starting Caddy. The same as when starting Caddy in a DOS box.
    //IcecastCaddyLogFile: string;
    // Caddy handle and process ID to end it.
    FCaddyProcessHandle: THandle;
    FCaddyProcessId: DWORD;
    FCaddyStartedByRdj: Boolean;
    FUpdatingServerUi: Boolean;

    procedure IcecastLog(Sender: TObject;
                         const AText: string);
    procedure IcecastStateChanged(Sender: TObject;
                                  AState: TMfIcecastServerState);
    procedure UpdateIcecastUi();
    procedure UpdateOnAirLamp(const AOnAir: Boolean);
    procedure SyncBroadcastUiFromEngine();

    procedure StartStopPublicRadioServices();
    function StartCaddy(): HRESULT;
    procedure StopCaddy();

    function IsCaddyRunning(): Boolean;
    function TcpPortOpen(const AHost: string;
                         const APort: Word): Boolean;

    function IsUncPath(const APath: string): Boolean;
    function IsRemoteServerMode(): Boolean;
    function IsIcecastReachable(): Boolean;
    function IsCaddyShareReady(): Boolean;
    function ConfirmRemoteServerReady(): Boolean;

  public
    { Public declarations }

    // Icecast (called by frmMainMdi)
    procedure SetBroadcastUiState(const AChecked,
                                  AOnAir: Boolean);
  end;

var
  FMediaServer: TfrmMediaServer;


implementation

{$R *.dfm}

uses
  {Application}
  RDJ.Setup,
  MfIcecastBroadcastEngine,
  frmMainMDI;


procedure TfrmMediaServer.btnMinimizeClick(Sender: TObject);
begin

  WindowState := wsMinimized;
end;


procedure TfrmMediaServer.chkAutoRestartClick(Sender: TObject);
begin

  if Assigned(FIcecastMgr) then
    FIcecastMgr.AutoRestart := chkAutoRestart.Checked;
end;


procedure TfrmMediaServer.chkBroadcastClick(Sender: TObject);
begin

  if FUpdatingBroadcastUi then
    Exit;

  if not Assigned(MainMDIFrm) then
    begin
      SetBroadcastUiState(False,
                          False);
      Exit;
    end;

  if chkBroadcast.Checked then
    begin
      if IsRemoteServerMode() and
         (not chkStartStopServer.Checked) then
        begin
          IcecastLog(Self,
                     'Broadcast disabled: remote server is not confirmed.');
          SetBroadcastUiState(False,
                              False);
          Exit;
        end;

      // Keep the user's request visible while the broadcast engine connects.
      // The engine state callback will update the On Air indication.
      SetBroadcastUiState(True,
                          False);

      try
        MainMDIFrm.SetBroadcastEnabled(True);
        SyncBroadcastUiFromEngine();
      except
        SetBroadcastUiState(False,
                            False);
        raise;
      end;
    end
  else
    begin
      MainMDIFrm.SetBroadcastEnabled(False);
      SetBroadcastUiState(False,
                          False);
    end;
end;


procedure TfrmMediaServer.chkStartStopServerClick(Sender: TObject);
begin

  if FUpdatingServerUi then
    Exit;

  StartStopPublicRadioServices();
end;


procedure TfrmMediaServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

begin

  CanClose := False;
  Hide;
end;


procedure TfrmMediaServer.FormCreate(Sender: TObject);
begin

  FUpdatingBroadcastUi := False;
  FUpdatingServerUi := False;

  FCaddyProcessHandle := 0;
  FCaddyProcessId := 0;
  FCaddyStartedByRdj := False;

  chkStartStopServer.Checked := False;

  chkBroadcast.Checked := False;
  chkBroadcast.Enabled := False;

  chkAutoRestart.Checked := True;

  UpdateOnAirLamp(False);
end;


procedure TfrmMediaServer.FormDestroy(Sender: TObject);
begin

  if Assigned(MainMDIFrm) then
    begin

      MainMDIFrm.SetBroadcastEnabled(False);
      MainMDIFrm.RemoveBroadcastEngine();
    end;

  if Assigned(FIcecastMgr) then
    begin

      FIcecastMgr.AutoRestart := False;
      FIcecastMgr.Stop(True);
      FIcecastMgr.OnLog := nil;
      FIcecastMgr.OnStateChanged := nil;
      Sleep(2000);
      FreeAndNil(FIcecastMgr);
    end;
end;


procedure TfrmMediaServer.FormResize(Sender: TObject);
begin

  MainMdifrm.chkMediaServer.Checked := (chkBroadcast.Tag > 0);
end;


procedure TfrmMediaServer.FormShow(Sender: TObject);
var
  setup: TRDJSetup;

begin

  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);

  Width := Width - 10;
  Height := Height - 30;

  if not Assigned(MainMDIFrm) then
    Exit;

  setup := MainMDIFrm.Setup;

  if not Assigned(FIcecastMgr) then
    begin
      FIcecastMgr := TMfIcecastServerManager.Create({Self});
      FIcecastMgr.OnLog := IcecastLog;
      FIcecastMgr.OnStateChanged := IcecastStateChanged;
    end;

  FIcecastMgr.ExePath := setup.IcecastExePath;
  FIcecastMgr.ConfigPath := setup.IcecastConfigPath;
  FIcecastMgr.WorkingDir := setup.IcecastWorkingDir;
  FIcecastMgr.Host := setup.IcecastHost;
  FIcecastMgr.Port := setup.IcecastPort;
  FIcecastMgr.HttpPath := setup.IcecastHttpPath;

  if (setup.IcecastAutoRestart = False) then
    setup.IcecastAutoRestart := True;

  FIcecastMgr.AutoRestart := setup.IcecastAutoRestart;
  chkAutoRestart.Checked := FIcecastMgr.AutoRestart;

  FIcecastMgr.RestartDelayMs := setup.IcecastRestartDelayMs;

  FCaddyProcessHandle := 0;
  FCaddyProcessId := 0;
  FCaddyStartedByRdj := False;

  if IsRemoteServerMode() then
    begin

      chkStartStopServer.Enabled := True;

      if Assigned(MainMDIFrm.IceCastEngine) and
         (MainMDIFrm.IceCastEngine.State in [bsConnecting,
                                             bsLive,
                                             bsReconnecting]) then
        begin
          lblIcecastServerStatus.Caption := 'Server: Remote server ready';
          chkStartStopServer.Checked := True;
          chkStartStopServer.Caption := 'Stop';
          SyncBroadcastUiFromEngine();
        end
      else
        begin
          lblIcecastServerStatus.Caption := 'Server: Remote mode - not confirmed';
          chkStartStopServer.Checked := False;
          chkStartStopServer.Caption := 'Start';
          SetBroadcastUiState(False,
                              False);
        end;
      Exit;
    end;

  UpdateIcecastUi();
end;


procedure TfrmMediaServer.IcecastLog(Sender: TObject;
                                     const AText: string);
begin

  if not Assigned(memIcecastLog) then
    Exit;

  memIcecastLog.Lines.BeginUpdate;

  try

    memIcecastLog.Lines.Add(FormatDateTime('hh:nn:ss',
                                           Now) + '  ' + TrimRight(AText));
    memIcecastLog.SelStart := Length(memIcecastLog.Text);
    memIcecastLog.Perform(EM_SCROLLCARET,
                          0,
                          0);
  finally

    memIcecastLog.Lines.EndUpdate;
  end;
end;


procedure TfrmMediaServer.IcecastStateChanged(Sender: TObject;
                                             AState: TMfIcecastServerState);
begin

  UpdateIcecastUi();
end;


procedure TfrmMediaServer.UpdateIcecastUi();
begin

  if IsRemoteServerMode() then
    begin

      if chkStartStopServer.Checked then
        begin

          lblIcecastServerStatus.Caption := 'Server: Remote server ready';
          chkStartStopServer.Caption := 'Stop';
          chkBroadcast.Enabled := True;
        end
      else
        begin

          lblIcecastServerStatus.Caption := 'Server: Remote server disconnected';
          chkStartStopServer.Caption := 'Start';
          chkBroadcast.Enabled := False;
          chkBroadcast.Checked := False;
          UpdateOnAirLamp(False);
        end;

      chkStartStopServer.Enabled := True;

      if Assigned(MainMDIFrm) then
        MainMDIFrm.chkMediaServer.Checked := chkStartStopServer.Checked;

      Exit;
    end;

  if not Assigned(FIcecastMgr) then
    Exit;

  case FIcecastMgr.State of
    issStopped:
      begin

        chkStartStopServer.Checked := False;
        chkStartStopServer.Caption := 'Start';
        chkBroadcast.Enabled := False;
        chkBroadcast.Checked := False;

        MainMDIFrm.SetBroadcastEnabled(False);
        MainMDIFrm.chkMediaServer.Checked := False;

        UpdateOnAirLamp(False);
      end;

    issStarting:
      begin

        lblIcecastServerStatus.Caption := 'Server: Starting';
        chkStartStopServer.Caption := 'Starting...';
        chkStartStopServer.Enabled := False;
        chkBroadcast.Enabled := False;
        chkBroadcast.Checked := False;
        MainMdifrm.chkMediaServer.Checked := False;
      end;

    issRunningNotReady:
      begin

        chkStartStopServer.Checked := True;
        chkStartStopServer.Caption := 'Stop';
        chkBroadcast.Enabled := False;
        chkBroadcast.Checked := False;
        UpdateOnAirLamp(False);
      end;

    issReady:
      begin

        chkStartStopServer.Checked := True;
        chkStartStopServer.Caption := 'Stop';
        chkBroadcast.Enabled := True;
        MainMDIFrm.chkMediaServer.Checked := True;
      end;

    issStopping:
      begin

        lblIcecastServerStatus.Caption := 'Server: Stopping';
        chkStartStopServer.Caption := 'Stopping...';
        chkStartStopServer.Enabled := False;
        chkBroadcast.Enabled := False;
        chkBroadcast.Checked := False;
        MainMdifrm.chkMediaServer.Checked := False;
      end;
  end;

  chkAutoRestart.Checked := FIcecastMgr.AutoRestart;
end;


procedure TfrmMediaServer.UpdateOnAirLamp(const AOnAir: Boolean);
const
  ON_COLOR = clRed;
  OFF_COLOR = $00568000;

begin

  if AOnAir then
    begin

      shpOnAir.Brush.Color := ON_COLOR;
      shpOnAir.Pen.Color := ON_COLOR;
      shpOnAirCap.Pen.Color := ON_COLOR;
      lblOnAir.Font.Color := ON_COLOR;
    end
  else
    begin

      shpOnAir.Brush.Color := OFF_COLOR;
      shpOnAir.Pen.Color := OFF_COLOR;
      shpOnAirCap.Pen.Color := OFF_COLOR;
      lblOnAir.Font.Color := OFF_COLOR;
    end;
end;


procedure TfrmMediaServer.SyncBroadcastUiFromEngine();
begin

  if not Assigned(MainMDIFrm) or
     not Assigned(MainMDIFrm.IceCastEngine) then
    begin
      SetBroadcastUiState(False,
                          False);
      Exit;
    end;

  case MainMDIFrm.IceCastEngine.State of
    bsConnecting,
    bsReconnecting: SetBroadcastUiState(True,
                                        False);

    bsLive: SetBroadcastUiState(True,
                                True);
  else
    SetBroadcastUiState(False,
                        False);
  end;
end;


procedure TfrmMediaServer.SetBroadcastUiState(const AChecked,
                                               AOnAir: Boolean);
begin

  FUpdatingBroadcastUi := True;

  try

    if IsRemoteServerMode() then
      chkBroadcast.Enabled := chkStartStopServer.Checked
    else
      chkBroadcast.Enabled := chkBroadcast.Enabled or AChecked or AOnAir;

    chkBroadcast.Checked := AChecked;

    UpdateOnAirLamp(AOnAir);
  finally

    FUpdatingBroadcastUi := False;
  end;
end;


procedure TfrmMediaServer.StartStopPublicRadioServices();
var
  hr: HResult;

begin

  if IsRemoteServerMode() then
    begin

      { TMPxpButton toggles Checked before OnClick.  Checked=True therefore
        means Start was requested; Checked=False means Stop/disconnect. }
      if chkStartStopServer.Checked then
        ConfirmRemoteServerReady()
      else
        begin
          if Assigned(MainMDIFrm) then
            begin
              MainMDIFrm.SetBroadcastEnabled(False);
              MainMDIFrm.chkMediaServer.Checked := False;
            end;

          SetBroadcastUiState(False,
                              False);

          chkStartStopServer.Checked := False;
          chkStartStopServer.Caption := 'Start';
          chkStartStopServer.Enabled := True;
          lblIcecastServerStatus.Caption := 'Server: Remote server disconnected';

          IcecastLog(Self,
                     'Remote server mode: disconnected. Remote Icecast and Caddy services remain running.');
        end;
      Exit;
    end;

  if not Assigned(FIcecastMgr) then
    Exit;

  case FIcecastMgr.State of
    issStopped: begin

                  hr := StartCaddy();
                  if Failed(hr) then
                    raise Exception.Create('Could not start Caddy.');

                  FIcecastMgr.AutoRestart := chkAutoRestart.Checked;

                  hr := FIcecastMgr.Start();
                  if Failed(hr) then
                    raise Exception.Create('Could not start Icecast.');

                  chkStartStopServer.Caption := 'Stop';
                end;

    issRunningNotReady,
    issReady: begin

                MainMDIFrm.SetBroadcastEnabled(False);
                SetBroadcastUiState(False,
                                    False);

                FIcecastMgr.Stop();
                StopCaddy();

                chkStartStopServer.Caption := 'Start';
              end;

    issStarting,
    issStopping: UpdateIcecastUi();
  end;
end;


function TfrmMediaServer.StartCaddy(): HRESULT;
var
  hr: HResult;
  SI: TStartupInfo;
  PI: TProcessInformation;
  SA: TSecurityAttributes;
  LogHandle: THandle;
  CmdLine: string;
  WorkDir: string;
  CaddyExe: string;
  CaddyConfigFile: string;
  CaddyLogFile: string;
  setup: TRDJSetup;

  function QuoteCmd(const S: string): string;
  begin

    Result := '"' + S + '"';
  end;

begin

  if not Assigned(MainMDIFrm) then
    Exit(E_FAIL);

  // Check if Caddy is allready running, if so, exit.
  if IsCaddyRunning() then
    begin

      FCaddyStartedByRdj := False;
      Exit(S_OK);
    end;

  Result := S_OK;
  setup := MainMDIFrm.Setup;

  WorkDir := Trim(setup.IcecastCaddyDir);
  CaddyConfigFile := Trim(setup.IcecastCaddyConfigFile);
  CaddyLogFile :=  IncludeTrailingPathDelimiter(WorkDir) + Trim(setup.IcecastCaddyLogFile);

  if (WorkDir = '') then
    Exit(S_OK);

  CaddyExe := IncludeTrailingPathDelimiter(WorkDir) + 'caddy.exe';

  if not FileExists(CaddyExe) then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));

  if (CaddyConfigFile <> '') and
     (not FileExists(CaddyConfigFile)) then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));

  if (CaddyLogFile = '') then
    begin
      CaddyLogFile := IncludeTrailingPathDelimiter(WorkDir) + 'caddy.log';
      ForceDirectories(ExtractFilePath(CaddyLogFile));
    end;

  if FileExists(CaddyLogFile) then
    DeleteFile(PChar(CaddyLogFile));

  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;

  LogHandle := CreateFile(PChar(CaddyLogFile),
                          GENERIC_WRITE,
                          FILE_SHARE_READ,
                          @SA,
                          CREATE_ALWAYS,
                          FILE_ATTRIBUTE_NORMAL,
                          0);

  if LogHandle = INVALID_HANDLE_VALUE then
    Exit(HRESULT_FROM_WIN32(GetLastError));

  try

    if (CaddyConfigFile <> '') then
      CmdLine := Format('%s run --config %s --adapter caddyfile',
                        [QuoteCmd(CaddyExe),
                         QuoteCmd(CaddyConfigFile)])
    else
      CmdLine := Format('%s run',
                        [QuoteCmd(CaddyExe)]);

    ZeroMemory(@SI,
               SizeOf(SI));

    ZeroMemory(@PI,
               SizeOf(PI));

    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdOutput := LogHandle;
    SI.hStdError := LogHandle;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

    if not CreateProcess(nil,
                         PChar(CmdLine),
                         nil,
                         nil,
                         True,
                         CREATE_NO_WINDOW,
                         nil,
                         PChar(WorkDir),
                         SI,
                         PI) then
      hr := HRESULT_FROM_WIN32(GetLastError)
    else
      hr := S_OK;

    if FAILED(hr) then
      Exit(hr)
    else
      begin

        FCaddyProcessHandle := PI.hProcess;
        FCaddyProcessId := PI.dwProcessId;
        FCaddyStartedByRdj := True;

        CloseHandle(PI.hThread);
        // Do not close Caddy handle, we need it to stop Caddy.
        //CloseHandle(PI.hProcess);
      end;

  finally

    if (LogHandle <> INVALID_HANDLE_VALUE) then
      CloseHandle(LogHandle);
  end;
end;


// Stop Caddy as a service.
procedure TfrmMediaServer.StopCaddy();
var
  ExitCode: DWORD;

begin

  if (FCaddyProcessHandle = 0) then
    Exit;

  if GetExitCodeProcess(FCaddyProcessHandle,
                        ExitCode) then
    begin

      if (ExitCode = STILL_ACTIVE) then
        begin

          TerminateProcess(FCaddyProcessHandle,
                           0);
          WaitForSingleObject(FCaddyProcessHandle,
                              3000);
        end;

    end;

  CloseHandle(FCaddyProcessHandle);

  FCaddyProcessHandle := 0;
  FCaddyProcessId := 0;
end;


function TfrmMediaServer.IsCaddyRunning(): Boolean;
begin

  Result := TcpPortOpen('127.0.0.1',
                        443) or
            TcpPortOpen('127.0.0.1',
                        80);
end;


procedure TfrmMediaServer.pnlCaptionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  // Drag window
  ReleaseCapture;
  SendMessage(Handle,
              WM_SYSCOMMAND,
              SC_MOVE + HTCAPTION,
              0);
end;


function TfrmMediaServer.IsUncPath(const APath: string): Boolean;
var
  S: string;

begin
  S := Trim(APath);
  Result := (Length(S) >= 2) and
            (S[1] = '\') and
            (S[2] = '\');
end;


function TfrmMediaServer.IsRemoteServerMode(): Boolean;
var
  Setup: TRDJSetup;
  Host: string;

begin
  Result := False;

  if not Assigned(MainMDIFrm) then
    Exit;

  Setup := MainMDIFrm.Setup;
  Host := Trim(Setup.IcecastHost);

  Result := IsUncPath(Setup.IcecastExePath) or
            IsUncPath(Setup.IcecastConfigPath) or
            IsUncPath(Setup.IcecastWorkingDir) or
            IsUncPath(Setup.IcecastCaddyDir) or
            IsUncPath(Setup.IcecastCaddyConfigFile) or
            IsUncPath(Setup.IcecastNowPlayingJsonFile) or
            ((Host <> '') and
             (not SameText(Host, '127.0.0.1')) and
             (not SameText(Host, 'localhost')));
end;


function TfrmMediaServer.IsIcecastReachable(): Boolean;
var
  Host: string;
  Port: Word;

begin
  Result := False;

  if not Assigned(MainMDIFrm) then
    Exit;

  Host := Trim(MainMDIFrm.Setup.IcecastHost);
  Port := MainMDIFrm.Setup.IcecastPort;

  if Host = '' then
    Host := Trim(MainMDIFrm.Setup.Broadcast.Host);

  if Port = 0 then
    Port := MainMDIFrm.Setup.Broadcast.Port;

  if (Host = '') or
     (Port = 0) then
    Exit;

  Result := TcpPortOpen(Host,
                        Port);
end;


function TfrmMediaServer.IsCaddyShareReady(): Boolean;
var
  Dir: string;
  JsonFile: string;

begin
  Result := True;

  if not Assigned(MainMDIFrm) then
    Exit(False);

  Dir := Trim(MainMDIFrm.Setup.IcecastCaddyDir);
  JsonFile := Trim(MainMDIFrm.Setup.IcecastNowPlayingJsonFile);

  if Dir <> '' then
    Result := DirectoryExists(Dir);

  if Result and
     (JsonFile <> '') then
    Result := DirectoryExists(ExtractFilePath(JsonFile));
end;


function TfrmMediaServer.ConfirmRemoteServerReady(): Boolean;
var
  IcecastOk: Boolean;
  CaddyShareOk: Boolean;

begin
  IcecastOk := IsIcecastReachable();
  CaddyShareOk := IsCaddyShareReady();

  if IcecastOk then
    IcecastLog(Self,
               'Remote server mode: Icecast is reachable.')
  else
    IcecastLog(Self,
               'Remote server mode: Icecast is NOT reachable.');

  if CaddyShareOk then
    IcecastLog(Self,
               'Remote server mode: Caddy web folder is reachable.')
  else
    IcecastLog(Self,
               'Remote server mode: Caddy web folder is NOT reachable.');

  Result := IcecastOk and CaddyShareOk;

  chkStartStopServer.Enabled := True;
  chkStartStopServer.Checked := Result;

  if Result then
    chkStartStopServer.Caption := 'Stop'
  else
    chkStartStopServer.Caption := 'Start';

  if Result then
    begin
      lblIcecastServerStatus.Caption := 'Server: Remote server ready';
      SyncBroadcastUiFromEngine();
    end
  else
    begin
      lblIcecastServerStatus.Caption := 'Server: Remote server not ready';
      SetBroadcastUiState(False,
                          False);
    end;

  if Assigned(MainMDIFrm) then
    MainMDIFrm.chkMediaServer.Checked := Result;
end;


function TfrmMediaServer.TcpPortOpen(const AHost: string;
                                    const APort: Word): Boolean;
var
  WSAData: TWSAData;
  Sock: TSocket;
  Addr: TSockAddrIn;

begin

  Result := False;

  if (WSAStartup($0202,
                WSAData) <> 0) then
    Exit;

  try

    Sock := socket(AF_INET,
                   SOCK_STREAM,
                   IPPROTO_TCP);
    if (Sock = INVALID_SOCKET) then
      Exit;

    try

      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(APort);
      Addr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(AHost)));

      Result := connect(Sock,
                        Addr,
                        SizeOf(Addr)) = 0;

    finally

      closesocket(Sock);
    end;

  finally

    WSACleanup();
  end;
end;

// All Icecast end =============================================================


end.
